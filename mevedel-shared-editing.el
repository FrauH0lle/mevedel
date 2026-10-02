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
A deleted item arrives as a STATE with `:deleted' and `:actor' and a nil
RESULT.  Observers cannot change whether the preceding commit succeeded.")

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
  (unless (mevedel-session-artifacts-artifact-present-p
           session (mevedel-shared-editing--logical id) t)
    (error "This item no longer exists"))
  (mevedel-shared-editing--parse
   (decode-coding-string
    (mevedel-session-artifacts-read-artifact
     session (mevedel-shared-editing--logical id) t)
    'utf-8-unix)))

(defun mevedel-shared-editing--delete (session args)
  "Delete SESSION's shared item named by ARGS and return the reply.
The item's whole state goes with it: content, images, comments and
history.  Observers learn of it as a state with `:deleted' and the
deleting `:actor'."
  (let* ((id (plist-get args :id))
         (logical (mevedel-shared-editing--logical id)))
    (unless (mevedel-session-artifacts-artifact-present-p session logical t)
      (error "This item no longer exists"))
    (mevedel-session-artifacts-delete-files
     session (list (file-name-concat (mevedel-session-save-path session) logical)))
    (let ((state (list :id id :deleted t :actor (plist-get args :actor))))
      (dolist (observer mevedel-shared-editing-change-hook)
        (condition-case nil (funcall observer session state nil)
          (error nil))))
    (list :result (list :id id :deleted t))))

(defun mevedel-shared-editing-ids (session)
  "Return the ids of SESSION's committed shared items, without reading them."
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
        ;; Items are the folder's direct entries; nested folders hold
        ;; annotations such as artifact comments, not items.
        (when (and (string-prefix-p prefix name)
                   (not (string-search "/" (substring name (length prefix))))
                   (string-match-p "\\.json\\'" name))
          (push (file-name-base name) result))))))

(defun mevedel-shared-editing-list (session)
  "Return SESSION's committed shared item catalog."
  (mapcar (lambda (id)
            (let ((state (mevedel-shared-editing--read session id)))
              (list :id id :kind (plist-get state :kind)
                    :title (plist-get state :title)
                    :revision (plist-get state :revision))))
          (mevedel-shared-editing-ids session)))

(defun mevedel-shared-editing--commit (session state)
  "Durably commit candidate STATE while SESSION still owns authority."
  (mevedel-shared-editing-commit-file
   session (mevedel-shared-editing--logical (plist-get state :id))
   (mevedel-shared-editing--json state)))

(defun mevedel-shared-editing-commit-file (session logical content)
  "Durably write CONTENT to session-relative LOGICAL in SESSION.
Run in SESSION's data buffer.  Portable sessions publish it with a fresh
sidecar; PID-lock sessions write it atomically.  Collaboration annotations
that live beside shared items use this so Resume, Save As and Fork carry
them the same way."
  (mevedel-session-artifacts-assert-new-mutation-authority session)
  (when buffer-read-only (user-error "Session is read-only"))
  (let ((path (file-name-concat (mevedel-session-save-path session) logical)))
    (if (mevedel-session-codec-portable-authority-p session)
        (progn
          ;; The authority gate only shallow-materializes a fresh portable
          ;; session.  Its first durable write needs the full root snapshot,
          ;; or Resume finds no published segment.
          (unless (mevedel-session-artifacts-artifact-present-p
                   session "session.meta.el" t)
            (mevedel-session-artifacts-save session (current-buffer)))
          (mevedel-session-publication-publish
           session
           (list
            (list :path path :content content :coding 'utf-8-unix)
            (list :path (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session))
                  :content (mevedel-session-artifacts-printed-value
                            (mevedel-session-artifacts-build-sidecar
                             session (current-buffer)))
                  :commit-marker t)) t))
      (let ((write-region-inhibit-fsync nil))
        (mevedel--write-file-atomically path content 'utf-8-unix #o600)))))

(cl-defun mevedel-shared-editing-stop (&optional runtime reason)
  "Stop RUNTIME or this buffer's helper and settle callbacks with REASON."
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
                     (list :error (or reason "Shared editing stopped; pending edits were not saved")))
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

(defun mevedel-shared-editing--process (buffer &optional restart)
  "Return BUFFER's private helper, starting it if needed or RESTART is non-nil."
  ;; Configuration changes take effect between jobs, without cancelling the
  ;; queue or an operation already sent to the previous helper.
  (let ((process (plist-get mevedel-shared-editing--runtime :process)))
    (when (and (process-live-p process)
               (or restart
                   (not (equal (process-get process :configuration)
                               (list mevedel-shared-editing-node-program
                                     mevedel-shared-editing--directory)))))
      (set-process-sentinel process #'ignore)
      (delete-process process)))
  (or (let ((process (plist-get mevedel-shared-editing--runtime :process)))
        (and (process-live-p process) process))
      (let* ((default-directory temporary-file-directory)
             (program (or (executable-find mevedel-shared-editing-node-program)
                          (user-error "Install Node 22.4+ on the Emacs host or set mevedel-shared-editing-node-program, then recheck")))
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
                                 (mevedel-shared-editing-stop
                                  nil "Shared editing helper exited. Check Node 22.4+ and the installed shared-editing resources on the Emacs host, then recheck")))))
               :filter
               (lambda (child chunk)
                 ;; Scan each arriving chunk once. Repeatedly concatenating
                 ;; a growing image-bearing reply makes framing quadratic.
                 (let ((start 0))
                   (while (and (< start (length chunk)) (process-live-p child))
                     (let* ((end (string-match "\n" chunk start))
                            (part (substring chunk start end))
                            (size (+ (or (process-get child :partial-bytes) 0)
                                     (string-bytes part))))
                       (setq start (if end (1+ end) (length chunk)))
                       (if (> size (* 64 1024 1024))
                           (delete-process child)
                         (process-put child :partial-bytes size)
                         (process-put child :partial
                                      (cons part (process-get child :partial)))
                         (when end
                           (let ((line (mapconcat #'identity
                                                  (nreverse (process-get child :partial)) "")))
                             (process-put child :partial nil)
                             (process-put child :partial-bytes 0)
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
                                         buffer job
                                         (list :error
                                               (if (equal (plist-get (plist-get job :args) :action) "status")
                                                   "Shared editing helper could not start. Check Node 22.4+ and the installed helper resources on the Emacs host, then recheck"
                                                 (error-message-string err))))))))))))))))))))
        (plist-put mevedel-shared-editing--runtime :process process)
        (process-put process :configuration
                     (list mevedel-shared-editing-node-program
                           mevedel-shared-editing--directory))
        process)))

(defun mevedel-shared-editing--send (process args)
  "Send one bounded JSON request ARGS to helper PROCESS.
Small writes avoid Emacs's large pipe-write backpressure pauses.  Chunk by
characters so UTF-8 encoding never splits a character between writes."
  (let* ((text (concat (mevedel-shared-editing--json args) "\n"))
         (length (length text))
         (offset 0))
    (while (< offset length)
      (let ((end (min length (+ offset 1024))))
        (process-send-string process (substring text offset end))
        (setq offset end)))))

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
                       (mutation (not (member action '("read" "export" "list" "status" "library-sheet")))))
                  (when (plist-get job :cancelled) (error "Editing operation cancelled"))
                  (when-let* ((authorize (plist-get job :authorize)))
                    (unless (funcall authorize) (error "Editing authority ended")))
                  (when mutation
                    (mevedel-session-artifacts-assert-new-mutation-authority session)
                    (when buffer-read-only (error "Session is read-only"))
                    (unless (mevedel-session-save-path session)
                      (mevedel-session-artifacts-save session buffer)))
                  (cond
                   ((equal action "list")
                    (mevedel-shared-editing--finish
                     buffer job (list :result (vconcat (mevedel-shared-editing-list session)))))
                   ;; Deleting needs no helper; queued, it cannot overtake a save.
                   ((equal action "delete")
                    (mevedel-shared-editing--finish
                     buffer job (mevedel-shared-editing--delete session args)))
                   (t
                    (unless (member action '("create" "import" "status" "library-sheet"))
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
                    ;; A check starts fresh so repaired resources and runtime
                    ;; changes are verified, without discarding queued work.
                    (mevedel-shared-editing--send
                     (mevedel-shared-editing--process buffer (equal action "status")) args)
                    (plist-put mevedel-shared-editing--runtime :timeout
                               (run-at-time
                                30 nil (lambda ()
                                         (when (buffer-live-p buffer)
                                           (with-current-buffer buffer
                                             (mevedel-shared-editing-stop)))))))))
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
