;;; mevedel-shared-editing.el --- Durable shared editor operations -*- lexical-binding: t; -*-

;;; Commentary:

;; One serialized editing queue per session.  A private Node helper computes
;; candidates; Emacs validates authority and commits them through existing
;; session storage before notifying browsers.  The helper owns no user files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mevedel-structs)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)
(require 'mevedel-transport)
(require 'mevedel-utilities)

(defcustom mevedel-shared-editing-node-program "node"
  "Node 22.4 or newer used for the packaged collaboration engine."
  :type 'string :group 'mevedel)

(defconst mevedel-shared-editing--directory
  (file-name-concat
   (file-name-directory (or load-file-name buffer-file-name)) "shared-editing")
  "Directory containing the packaged editing helper and renderer resources.")

(defvar mevedel-shared-editing-change-hook nil
  "Functions called with SESSION, STATE, RESULT after an editing commit.
Observers cannot change whether the preceding commit succeeded.")

(defvar-local mevedel-shared-editing--runtime nil
  "Session-local helper process, operation queue, and active operation.")

(defun mevedel-shared-editing--logical (id)
  "Return the session logical path for validated editor ID."
  (unless (and (stringp id)
               (string-match-p "\\`[a-zA-Z0-9_-]\\{1,80\\}\\'" id))
    (error "Invalid shared item identity"))
  (file-name-concat "artifacts" "shared-editing" (concat id ".json")))

(defun mevedel-shared-editing--json (value)
  "Encode VALUE with the editor's JSON null and false conventions."
  (decode-coding-string
   (json-serialize value :null-object nil :false-object :json-false)
   'utf-8-unix))

(defun mevedel-shared-editing--parse (text)
  "Parse bounded editor JSON TEXT."
  (when (> (string-bytes text) (* 64 1024 1024))
    (error "Shared editing response is too large"))
  ;; Empty objects occur in document marks and must survive exact patch reads.
  ;; Decode null separately, then lower it while retaining empty object hashes.
  (cl-labels ((normalize (value)
                (cond
                 ((eq value :null) nil)
                 ((null value) (make-hash-table))
                 ((vectorp value) (vconcat (mapcar #'normalize value)))
                 ((listp value)
                  (let (result)
                    (while value
                      (setq result (plist-put result (pop value)
                                              (normalize (pop value)))))
                    result))
                 (t value))))
    (normalize
     (json-parse-string text :object-type 'plist :array-type 'array
                        :null-object :null :false-object :json-false))))

(defun mevedel-shared-editing--read (session id)
  "Read committed shared item ID in SESSION."
  (mevedel-shared-editing--parse
   (decode-coding-string
    (mevedel-session-artifacts-read-artifact
     session (mevedel-shared-editing--logical id) t)
    'utf-8-unix)))

(defun mevedel-shared-editing-list (session)
  "Return SESSION's committed shared item catalog."
  (when-let* ((root (mevedel-session-save-path session)))
    (let* ((prefix "artifacts/shared-editing/")
           (names
            (if (mevedel-session-codec-portable-authority-p session)
                (mapcar #'car (plist-get
                               (or (mevedel-session-publication session)
                                   (mevedel-session-publication-read root))
                               :artifacts))
              (let ((directory (file-name-concat root prefix)))
                (when (file-directory-p directory)
                  (mapcar (lambda (name) (concat prefix name))
                          (directory-files directory nil "\\.json\\'"))))))
           result)
      (dolist (name names (nreverse result))
        (when (and (string-prefix-p prefix name)
                   (string-match-p "\\.json\\'" name))
          (let* ((id (file-name-base name))
                 (state (mevedel-shared-editing--read session id)))
            (push (list :id id :kind (plist-get state :kind)
                        :title (plist-get state :title)
                        :revision (plist-get state :revision)) result)))))))

(defun mevedel-shared-editing--commit (session state)
  "Durably commit candidate STATE while SESSION still owns authority."
  (mevedel-session-artifacts-assert-new-mutation-authority session)
  (when buffer-read-only (user-error "Session is read-only"))
  (let* ((logical (mevedel-shared-editing--logical (plist-get state :id)))
         (path (file-name-concat (mevedel-session-save-path session) logical))
         (content (mevedel-shared-editing--json state)))
    (if (mevedel-session-codec-portable-authority-p session)
        (mevedel-session-publication-publish
         session
         (list
          (list :path path :content content :coding 'utf-8-unix)
          (list :path (mevedel-session-artifacts-sidecar-path
                       (mevedel-session-save-path session))
                :content (mevedel-session-artifacts-printed-value
                          (mevedel-session-artifacts-build-sidecar
                           session (current-buffer)))
                :commit-marker t)) t)
      (let ((write-region-inhibit-fsync nil))
        (mevedel--write-file-atomically path content 'utf-8-unix #o600)))))

(cl-defun mevedel-shared-editing-stop (&optional runtime)
  "Stop RUNTIME or this buffer's helper and settle outstanding callbacks."
  (when-let* ((runtime (or runtime mevedel-shared-editing--runtime)))
    (when (plist-get runtime :committing)
      (plist-put runtime :stop-requested t)
      (cl-return-from mevedel-shared-editing-stop))
    (when (eq runtime mevedel-shared-editing--runtime)
      (setq mevedel-shared-editing--runtime nil))
    (dolist (key '(:timeout :timer))
      (when-let* ((timer (plist-get runtime key))) (cancel-timer timer)))
    (when-let* ((process (plist-get runtime :process)))
      (set-process-sentinel process #'ignore)
      (delete-process process))
    (dolist (job (cons (plist-get runtime :active) (plist-get runtime :queue)))
      (when job
        (condition-case nil
            (funcall (plist-get job :callback)
                     '(:error "Shared editing stopped; pending edits were not saved"))
          (error nil))))))

(defun mevedel-shared-editing--finish (buffer job reply)
  "Settle JOB with REPLY and continue BUFFER's editing queue."
  (let ((runtime (plist-get job :runtime)))
    (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
      (when (eq job (plist-get runtime :active))
        (when-let* ((timer (plist-get runtime :timeout)))
          (cancel-timer timer))
        (plist-put runtime :timeout nil)
        (plist-put runtime :active nil)
        (unwind-protect (funcall (plist-get job :callback) reply)
          (when (and (buffer-live-p buffer)
                     (eq runtime mevedel-shared-editing--runtime))
            (plist-put runtime :timer
                       (run-at-time 0 nil #'mevedel-shared-editing--drain buffer))))))))

(defun mevedel-shared-editing--accept (buffer job reply)
  "Check JOB's continuing authority and commit its computed REPLY in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq job (plist-get mevedel-shared-editing--runtime :active))
        (let ((session (plist-get job :session)))
          (if (mevedel-transport-busy-p (mevedel-session-save-path session))
              (plist-put mevedel-shared-editing--runtime :timer
                         (run-at-time 0.05 nil #'mevedel-shared-editing--accept
                                      buffer job reply))
            (condition-case err
                (progn
                  (when (plist-get job :cancelled) (error "Editing operation cancelled"))
                  (when-let* ((authorize (plist-get job :authorize)))
                    (unless (funcall authorize) (error "Editing authority ended")))
                  (funcall
                   (or (plist-get job :commit) #'funcall)
                   (lambda ()
                     (let ((runtime mevedel-shared-editing--runtime))
                       (plist-put runtime :committing t)
                       (unwind-protect
                           (progn
                             (when-let* ((state (plist-get reply :state)))
                               (mevedel-shared-editing--commit session state)
                               (let ((result (plist-get reply :result)))
                                 (dolist (observer mevedel-shared-editing-change-hook)
                                   (condition-case nil (funcall observer session state result)
                                     (error nil)))))
                             (mevedel-shared-editing--finish buffer job reply))
                         (plist-put runtime :committing nil)
                         (when (plist-get runtime :stop-requested)
                           (mevedel-shared-editing-stop runtime)))))))
              (error (mevedel-shared-editing--finish
                      buffer job (list :error (error-message-string err)))))))))))

(defun mevedel-shared-editing--process (buffer)
  "Return BUFFER's live private helper process, starting it if needed."
  (or (let ((process (plist-get mevedel-shared-editing--runtime :process)))
        (and (process-live-p process) process))
      (let* ((default-directory temporary-file-directory)
             (program (or (executable-find mevedel-shared-editing-node-program)
                          (user-error "Shared editing requires Node 22.4 or newer")))
             (process
              (make-process
               :name "mevedel-shared-editing" :buffer nil
               :command (list program "--no-experimental-webstorage"
                              (file-name-concat mevedel-shared-editing--directory
                                                "host.bundle.mjs"))
               :connection-type 'pipe :coding 'utf-8-unix :noquery t
               :sentinel (lambda (child _event)
                           (unless (process-live-p child)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (mevedel-shared-editing-stop)))))
               :filter
               (lambda (child chunk)
                 (let ((text (concat (process-get child :partial) chunk)))
                   (if (> (string-bytes text) (* 64 1024 1024))
                       (delete-process child)
                     (while (string-match "\n" text)
                       (let ((line (substring text 0 (match-beginning 0))))
                         (setq text (substring text (match-end 0)))
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (let ((job (plist-get mevedel-shared-editing--runtime :active)))
                               (when job
                                 (condition-case err
                                     (let ((reply (mevedel-shared-editing--parse line)))
                                       (when (equal (plist-get reply :requestId)
                                                    (plist-get job :requestId))
                                         (plist-put mevedel-shared-editing--runtime :timer
                                                    (run-at-time
                                                     0 nil #'mevedel-shared-editing--accept
                                                     buffer job reply))))
                                   (error
                                    (mevedel-shared-editing--finish
                                     buffer job (list :error (error-message-string err)))))))))))
                     (process-put child :partial text)))))))
        (plist-put mevedel-shared-editing--runtime :process process)
        process)))

(defun mevedel-shared-editing--drain (buffer)
  "Start the next serialized editing operation in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and mevedel-shared-editing--runtime
                 (not (plist-get mevedel-shared-editing--runtime :active))
                 (plist-get mevedel-shared-editing--runtime :queue))
        (let* ((job (car (plist-get mevedel-shared-editing--runtime :queue)))
               (session (plist-get job :session)))
          (if (mevedel-transport-busy-p (mevedel-session-save-path session))
              (plist-put mevedel-shared-editing--runtime :timer
                         (run-at-time 0.05 nil #'mevedel-shared-editing--drain buffer))
            (plist-put mevedel-shared-editing--runtime :queue
                       (cdr (plist-get mevedel-shared-editing--runtime :queue)))
            (plist-put mevedel-shared-editing--runtime :active job)
            (condition-case err
                (let* ((args (copy-sequence (plist-get job :args)))
                       (action (plist-get args :action))
                       (mutation (not (member action '("read" "export" "list")))))
                  (when (plist-get job :cancelled) (error "Editing operation cancelled"))
                  (when-let* ((authorize (plist-get job :authorize)))
                    (unless (funcall authorize) (error "Editing authority ended")))
                  (when mutation
                    (mevedel-session-artifacts-assert-new-mutation-authority session)
                    (when buffer-read-only (error "Session is read-only"))
                    (unless (mevedel-session-save-path session)
                      (mevedel-session-artifacts-save session buffer)))
                  (if (equal action "list")
                      (mevedel-shared-editing--finish
                       buffer job (list :result (vconcat (mevedel-shared-editing-list session))))
                    (unless (member action '("create" "import"))
                      (setq args (plist-put args :state
                                            (mevedel-shared-editing--read
                                             session (plist-get args :id)))))
                    (when (member action '("create" "import"))
                      (let ((logical (mevedel-shared-editing--logical (plist-get args :id))))
                        (when (mevedel-session-artifacts-artifact-present-p session logical)
                          (let ((existing (mevedel-shared-editing--read session (plist-get args :id))))
                            (unless (and (stringp (plist-get args :opId))
                                         (plist-member (plist-get existing :receipts)
                                                       (intern (concat ":" (plist-get args :opId)))))
                              (error "Shared item already exists; reopen it instead"))
                            (setq args (plist-put args :state existing)
                                  args (plist-put args :action "update"))))))
                    (setq args (plist-put args :requestId (plist-get job :requestId)))
                    (process-send-string (mevedel-shared-editing--process buffer)
                                         (concat (mevedel-shared-editing--json args) "\n"))
                    (plist-put mevedel-shared-editing--runtime :timeout
                               (run-at-time
                                30 nil (lambda ()
                                         (when (buffer-live-p buffer)
                                           (with-current-buffer buffer
                                             (mevedel-shared-editing-stop))))))))
              (error (mevedel-shared-editing--finish
                      buffer job (list :error (error-message-string err)))))))))))

;;;###autoload
(defun mevedel-shared-editing-call (session args callback &optional authorize commit)
  "Run editor ARGS for SESSION; deliver one response to CALLBACK.
AUTHORIZE, when non-nil, must still permit the operation at admission and
commit.  Return a cancellation function; cancellation before commit keeps
accepted content unchanged.  COMMIT optionally runs the final commit and
callback with the caller's cancellation settlement deferred until its outcome.
All calls for one session are serialized."
  (let ((buffer (mevedel-session-persistence-root-buffer-for-session
                 session (current-buffer))))
    (unless (buffer-live-p buffer) (error "Shared editing needs a live session"))
    (with-current-buffer buffer
      (unless mevedel-shared-editing--runtime
        (setq mevedel-shared-editing--runtime (list :queue nil :sequence 0))
        (add-hook 'kill-buffer-hook #'mevedel-shared-editing-stop nil t))
      (when (>= (length (plist-get mevedel-shared-editing--runtime :queue)) 64)
        (error "Shared editing queue is full"))
      (let* ((bytes (string-bytes (mevedel-shared-editing--json args)))
             (queued (cl-loop for job in (cons (plist-get mevedel-shared-editing--runtime :active)
                                               (plist-get mevedel-shared-editing--runtime :queue))
                              sum (or (plist-get job :bytes) 0)))
             (sequence (1+ (plist-get mevedel-shared-editing--runtime :sequence)))
             (job (list :session session :args args :callback callback
                        :runtime mevedel-shared-editing--runtime
                        :authorize authorize :commit commit :requestId sequence :bytes bytes)))
        (when (> (+ bytes queued) (* 32 1024 1024))
          (error "Shared editing queue is full; wait for pending saves"))
        (plist-put mevedel-shared-editing--runtime :sequence sequence)
        (plist-put mevedel-shared-editing--runtime :queue
                   (append (plist-get mevedel-shared-editing--runtime :queue) (list job)))
        (unless (plist-get mevedel-shared-editing--runtime :active)
          (when-let* ((timer (plist-get mevedel-shared-editing--runtime :timer)))
            (cancel-timer timer))
          (plist-put mevedel-shared-editing--runtime :timer
                     (run-at-time 0 nil #'mevedel-shared-editing--drain buffer)))
        (lambda () (plist-put job :cancelled t))))))

(provide 'mevedel-shared-editing)
;;; mevedel-shared-editing.el ends here
