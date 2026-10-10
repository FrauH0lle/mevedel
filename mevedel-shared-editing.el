;;; mevedel-shared-editing.el --- Durable shared editor operations -*- lexical-binding: t; -*-

;;; Commentary:

;; Whiteboards and documents live in the workspace artifact store as
;; `.state/ID/state.json'.  One serialized editing queue per workspace in this Emacs
;; serves every room and session that edits them.  A private Node helper
;; computes candidates; Emacs checks authority, proves its item lease and
;; commits them before notifying browsers.  The helper owns no user files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mevedel-artifact-lease)
(require 'mevedel-artifact-store)
(require 'mevedel-structs)
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
  "Functions called with WORKSPACE, STATE, RESULT after an editing commit.
A deleted item arrives as a STATE with `:deleted' and `:actor' and a nil
RESULT.  Observers cannot change whether the preceding commit succeeded.")

(defvar mevedel-shared-editing--runtimes (make-hash-table :test #'equal)
  "Helper process, operation queue and active operation per workspace root.")

(defun mevedel-shared-editing--valid-id (id)
  "Return ID when it is a valid shared item identity, else signal."
  (unless (and (stringp id)
               (string-match-p "\\`[a-zA-Z0-9_-]\\{1,80\\}\\'" id))
    (error "Invalid shared item identity"))
  id)

(defun mevedel-shared-editing--state-path (workspace id)
  "Return the target-proved state path of WORKSPACE's shared item ID.
Callers access this lexical path only through pinned control operations."
  (file-name-concat (mevedel-artifact-store-directory workspace) ".state"
                    (mevedel-shared-editing--valid-id id) "state.json"))

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

(defun mevedel-shared-editing--present-p (workspace id)
  "Return non-nil when WORKSPACE's store holds shared item ID."
  (and (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
       (file-exists-p (mevedel-shared-editing--state-path workspace id))))

(defun mevedel-shared-editing--read (workspace id)
  "Read committed shared item ID of WORKSPACE in one target-pinned batch."
  (let* ((state-path (mevedel-shared-editing--state-path workspace id))
         (results (mevedel-session-control-fs-run-program
                   (list (list :op 'read :path (file-name-concat
                                               (file-name-directory state-path) "meta.el"))
                         (list :op 'read :path state-path)))))
    (when (cl-some (lambda (result) (eq 'absent (plist-get result :status))) results)
      (error "This item no longer exists"))
    (unless (mevedel-artifact-store-item-p
             (mevedel-artifact-store--validate-meta
              (read (mevedel-session-control-fs-program-value (car results))) state-path))
      (error "This item no longer exists"))
    (mevedel-shared-editing--parse
     (mevedel-session-control-fs-program-value (cadr results)))))

(defun mevedel-shared-editing--notify (workspace state result)
  "Tell `mevedel-shared-editing-change-hook' about WORKSPACE's STATE and RESULT."
  (dolist (observer mevedel-shared-editing-change-hook)
    (condition-case nil (funcall observer workspace state result)
      (error nil))))

(defun mevedel-shared-editing--delete (workspace args)
  "Delete WORKSPACE's shared item named by ARGS and return the reply.
The item's whole artifact goes with it: content, images, comments,
history, versions and its dedicated session.  Observers learn of it as a
state with `:deleted' and the deleting `:actor'."
  (let ((id (plist-get args :id)))
    (unless (mevedel-shared-editing--present-p workspace id)
      (error "This item no longer exists"))
    (mevedel-artifact-store-delete workspace id)
    (mevedel-artifact-lease-forget-item workspace id)
    (mevedel-shared-editing--notify
     workspace (list :id id :deleted t :actor (plist-get args :actor)) nil)
    (list :result (list :id id :deleted t))))

(defun mevedel-shared-editing-ids (workspace)
  "Return the ids of WORKSPACE's shared items, reading only their metadata."
  (cl-remove-if-not
   (lambda (id)
     (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id)))
   (mevedel-artifact-store-ids workspace)))

(defun mevedel-shared-editing-list (workspace)
  "Return WORKSPACE's shared item catalog from small, target-proved metadata."
  (let* ((ids (mevedel-artifact-store-ids workspace))
         (store (mevedel-artifact-store-directory workspace))
         (operations
          (mapcar (lambda (id)
                    (unless (mevedel-artifact-store-id-p id)
                      (error "Invalid artifact id: %S" id))
                    (list :op 'read :optional t
                          :path (file-name-concat store ".state" id "meta.el"))) ids))
         batches results catalog)
    (while operations
      (push (mevedel-session-control-fs-run-program
             (cl-loop repeat 32 while operations collect (pop operations))) batches))
    (setq results (apply #'append (nreverse batches)))
    (dolist (id ids (nreverse catalog))
      (let ((result (pop results)))
        (condition-case nil
            (unless (eq (plist-get result :status) 'absent)
              (let ((meta (mevedel-artifact-store--validate-meta
                           (read (mevedel-session-control-fs-program-value result)) id)))
                (when (and (mevedel-artifact-store-item-p meta)
                           (natnump (plist-get meta :revision)))
                  (push (list :id id :kind (symbol-name (plist-get meta :kind))
                              :title (plist-get meta :title)
                              :revision (plist-get meta :revision)) catalog))))
          (error nil))))))

(defun mevedel-shared-editing--commit (workspace state)
  "Durably commit candidate STATE of WORKSPACE under this Emacs's item lease.
Metadata records the committed kind, title and revision."
  (let* ((id (plist-get state :id))
         (directory (file-name-directory (mevedel-shared-editing--state-path workspace id)))
         (authored (file-name-concat (mevedel-artifact-store-directory workspace) id))
         (meta-path (file-name-concat directory "meta.el"))
         (metadata (car (mevedel-session-control-fs-run-program
                         (list (list :op 'read :path meta-path :coding 'no-conversion)))))
         (bytes (unless (eq 'absent (plist-get metadata :status))
                  (mevedel-session-control-fs-program-value metadata)))
         (meta (and bytes (car (read-from-string (decode-coding-string bytes 'utf-8-unix)))))
         (kind (intern (plist-get state :kind)))
         (changed (or (not (equal (plist-get meta :title) (plist-get state :title)))
                      (not (eq (plist-get meta :kind) kind))
                      (not (equal (plist-get meta :revision) (plist-get state :revision))))))
    (unless bytes (mevedel-session-control-fs-make-directory directory t))
    (when changed
      (setq meta (plist-put
                  (or meta (list :kind kind
                                 :title (plist-get state :title) :file "state.json"
                                 :created (format-time-string "%FT%T%z")))
                  :title (plist-get state :title)))
      (setq meta (plist-put meta :kind kind)
            meta (plist-put meta :revision (plist-get state :revision))))
    (let* ((metadata-write
            (when changed
              (list :op 'write :path meta-path
                    :content (let ((print-length nil) (print-level nil))
                               (prin1-to-string meta)))))
           (results
            (mevedel-artifact-lease-run
             workspace id
             (append
              (list (list :op 'make-directory :path authored :optional t)
                    (list :op 'directory-p :path authored))
              (when changed
                (list (if bytes (list :op 'verify :path meta-path :content bytes)
                        (list :op 'absent :path meta-path))))
              ;; Metadata first only on creation, so an interrupted create retries.
              (when (and changed (not bytes)) (list metadata-write))
              (list (list :op 'write :path (mevedel-shared-editing--state-path workspace id)
                          :content (encode-coding-string (mevedel-shared-editing--json state)
                                                         'utf-8-unix)))
              (when (and changed bytes) (list metadata-write))))))
      (unless (memq (plist-get (car results) :status) '(ok conflict))
        (mevedel-session-control-fs-program-value (car results)))
      (dolist (result (cdr results))
        (mevedel-session-control-fs-program-value result)))
    (mevedel-artifact-store--changed workspace)))

(defun mevedel-shared-editing--version-content (state)
  "Return STATE as a version keeps it: without receipts and history."
  (let ((state (copy-sequence state)))
    (cl-remf state :receipts)
    (cl-remf state :transactions)
    (mevedel-shared-editing--json state)))

(defun mevedel-shared-editing-save-version (workspace id &optional session-id)
  "Record WORKSPACE's shared item ID as a new version and return its number.
SESSION-ID names the session that edited it, if any."
  (mevedel-artifact-store-record-version
   workspace id session-id
   (mevedel-shared-editing--version-content
    (mevedel-shared-editing--read workspace id))))

(defun mevedel-shared-editing--op-id ()
  "Return a fresh operation identity."
  (secure-hash 'sha256 (format "%s%s" (current-time) (random t))))

(defun mevedel-shared-editing-restore (workspace id n actor callback)
  "Restore version N of WORKSPACE's shared item ID as one edit by ACTOR.
The version's content becomes the item's through the editing queue, so
lineage, comments and history stay and the restore can be reverted.
CALLBACK receives the edit's reply."
  (let ((version (mevedel-shared-editing--parse
                  (with-temp-buffer
                    (insert-file-contents
                     (mevedel-artifact-store-version-path workspace id n))
                    (buffer-string)))))
    (mevedel-shared-editing-call
     workspace (list :action "restore" :id id :target (plist-get version :crdt)
                     :actor actor :opId (mevedel-shared-editing--op-id))
     callback)))

(defun mevedel-shared-editing-duplicate (workspace id new-id)
  "Copy WORKSPACE's shared item ID into the new, independent item NEW-ID.
The copy starts with the source's content and one version."
  (mevedel-shared-editing--valid-id new-id)
  (when (or (file-exists-p (mevedel-artifact-store-artifact-directory workspace new-id))
            (file-exists-p (mevedel-artifact-store-bookkeeping-directory workspace new-id)))
    (error "Artifact %s already exists" new-id))
  (let ((state (plist-put (copy-sequence (mevedel-shared-editing--read workspace id))
                          :id new-id)))
    (mevedel-artifact-lease-acquire workspace new-id)
    (make-directory (mevedel-artifact-store-bookkeeping-directory workspace new-id))
    (mevedel-shared-editing--commit workspace state)
    (mevedel-shared-editing-save-version workspace new-id)
    new-id))

;; Callbacks run in the buffer current when the operation was queued, so a
;; tool or room handler sees its own buffer, wherever the helper replies.

(defmacro mevedel-shared-editing--in-job (job &rest body)
  "Run BODY in the buffer JOB was queued from, or the current one."
  (declare (indent 1) (debug t))
  `(let ((buffer (plist-get ,job :buffer)))
     (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
       ,@body)))

(defun mevedel-shared-editing--runtime (workspace)
  "Return WORKSPACE's editing runtime, creating it on first use."
  (let ((key (mevedel-workspace-root workspace)))
    (or (gethash key mevedel-shared-editing--runtimes)
        (puthash key (list :workspace workspace :queue nil :sequence 0)
                 mevedel-shared-editing--runtimes))))

(defun mevedel-shared-editing--live-p (runtime)
  "Return non-nil while RUNTIME is still its workspace's runtime."
  (when-let* ((workspace (plist-get runtime :workspace)))
    (eq runtime (gethash (mevedel-workspace-root workspace)
                         mevedel-shared-editing--runtimes))))

(defun mevedel-shared-editing--busy-item-p (workspace id)
  "Return non-nil while WORKSPACE's runtime has work for item ID."
  (when-let* ((runtime (gethash (mevedel-workspace-root workspace)
                                mevedel-shared-editing--runtimes)))
    (cl-some (lambda (job) (and job (equal id (plist-get (plist-get job :args) :id))))
             (cons (plist-get runtime :active) (plist-get runtime :queue)))))

(setq mevedel-artifact-lease-busy-function #'mevedel-shared-editing--busy-item-p)

(cl-defun mevedel-shared-editing-stop (&optional runtime reason)
  "Stop RUNTIME, or every runtime, and settle its callbacks with REASON."
  (unless runtime
    (let (all)
      (maphash (lambda (_root value) (push value all)) mevedel-shared-editing--runtimes)
      (dolist (value all) (mevedel-shared-editing-stop value reason)))
    (cl-return-from mevedel-shared-editing-stop))
  (when (plist-get runtime :committing)
    (plist-put runtime :stop-requested t)
    (cl-return-from mevedel-shared-editing-stop))
  (when (mevedel-shared-editing--live-p runtime)
    (remhash (mevedel-workspace-root (plist-get runtime :workspace))
             mevedel-shared-editing--runtimes))
  (dolist (key '(:timeout :timer))
    (when-let* ((timer (plist-get runtime key))) (cancel-timer timer)))
  (when-let* ((process (plist-get runtime :process)))
    (set-process-sentinel process #'ignore)
    (delete-process process))
  (let ((jobs (cons (plist-get runtime :active) (plist-get runtime :queue))))
    ;; Retire jobs before callbacks: they can reenter stop, and a delayed
    ;; helper reply must never commit an edit already reported as unsaved.
    (plist-put runtime :active nil)
    (plist-put runtime :queue nil)
    (dolist (job jobs)
      (when job
        (condition-case nil
            (mevedel-shared-editing--in-job job
              (funcall (plist-get job :callback)
                       (list :error (or reason "Shared editing stopped; pending edits were not saved"))))
          (error nil))))))

(defun mevedel-shared-editing--finish (runtime job reply)
  "Settle JOB with REPLY and continue RUNTIME's editing queue."
  (when (eq job (plist-get runtime :active))
    (when-let* ((timer (plist-get runtime :timeout)))
      (cancel-timer timer))
    (plist-put runtime :timeout nil)
    (plist-put runtime :active nil)
    (unwind-protect (mevedel-shared-editing--in-job job
                      (funcall (plist-get job :callback) reply))
      (when (mevedel-shared-editing--live-p runtime)
        (plist-put runtime :timer
                   (mevedel-transport-run-at-time 0 #'mevedel-shared-editing--drain runtime))))))

(defun mevedel-shared-editing--store-busy-p (runtime)
  "Return non-nil when RUNTIME's store must not be touched now."
  (mevedel-transport-busy-p
   (mevedel-artifact-store-directory (plist-get runtime :workspace))))

(defun mevedel-shared-editing--accept (runtime job reply)
  "Check JOB's continuing authority and commit its computed REPLY for RUNTIME."
  (when (eq job (plist-get runtime :active))
    (if (mevedel-shared-editing--store-busy-p runtime)
        (plist-put runtime :timer
                   (mevedel-transport-run-at-time 0.05 #'mevedel-shared-editing--accept
                                runtime job reply))
      (condition-case err
          (progn
            (when (plist-get job :cancelled) (error "Editing operation cancelled"))
            (when-let* ((authorize (plist-get job :authorize)))
              (unless (mevedel-shared-editing--in-job job (funcall authorize))
                (error "Editing authority ended")))
            (mevedel-shared-editing--in-job job
              (funcall
               (or (plist-get job :commit) #'funcall)
               (lambda ()
                 (plist-put runtime :committing t)
                 (unwind-protect
                     (progn
                       (when-let* ((state (plist-get reply :state)))
                         (let ((workspace (plist-get runtime :workspace)))
                           (mevedel-shared-editing--commit workspace state)
                           (mevedel-shared-editing--notify
                            workspace state (plist-get reply :result))))
                       (mevedel-shared-editing--finish runtime job reply))
                   (plist-put runtime :committing nil)
                   (when (plist-get runtime :stop-requested)
                     (mevedel-shared-editing-stop runtime)))))))
        (error (mevedel-shared-editing--finish
                runtime job (list :error (error-message-string err))))))))

(defun mevedel-shared-editing--process (runtime &optional restart)
  "Return RUNTIME's private helper, starting it if needed or RESTART is non-nil."
  ;; Configuration changes take effect between jobs, without cancelling the
  ;; queue or an operation already sent to the previous helper.
  (let ((process (plist-get runtime :process)))
    (when (and (process-live-p process)
               (or restart
                   (not (equal (process-get process :configuration)
                               (list mevedel-shared-editing-node-program
                                     mevedel-shared-editing--directory)))))
      (set-process-sentinel process #'ignore)
      (delete-process process)))
  (or (let ((process (plist-get runtime :process)))
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
                             (when (mevedel-shared-editing--live-p runtime)
                               (mevedel-shared-editing-stop
                                runtime "Shared editing helper exited. Check Node 22.4+ and the installed shared-editing resources on the Emacs host, then recheck"))))
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
                             (when-let* ((job (plist-get runtime :active)))
                               (condition-case err
                                   (let ((reply (mevedel-shared-editing--parse line)))
                                     (when (equal (plist-get reply :requestId)
                                                  (plist-get job :requestId))
                                       (plist-put runtime :timer
                                                  (mevedel-transport-run-at-time
                                                   0 #'mevedel-shared-editing--accept
                                                   runtime job reply))))
                                 (error
                                  (mevedel-shared-editing--finish
                                   runtime job
                                   (list :error
                                         (if (equal (plist-get (plist-get job :args) :action) "status")
                                             "Shared editing helper could not start. Check Node 22.4+ and the installed helper resources on the Emacs host, then recheck"
                                           (error-message-string err)))))))))))))))))
        (plist-put runtime :process process)
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

;; ponytail: one queue and helper per workspace serializes every item there;
;; split per item only if unrelated boards measurably wait on each other.
(defun mevedel-shared-editing--drain (runtime)
  "Start RUNTIME's next serialized editing operation."
  (when (and (mevedel-shared-editing--live-p runtime)
             (not (plist-get runtime :active))
             (plist-get runtime :queue))
    (let ((job (car (plist-get runtime :queue)))
          (workspace (plist-get runtime :workspace)))
      (if (mevedel-shared-editing--store-busy-p runtime)
          (plist-put runtime :timer
                     (mevedel-transport-run-at-time 0.05 #'mevedel-shared-editing--drain runtime))
        (plist-put runtime :queue (cdr (plist-get runtime :queue)))
        (plist-put runtime :active job)
        (condition-case err
            (let* ((args (copy-sequence (plist-get job :args)))
                   (action (plist-get args :action))
                   (id (plist-get args :id))
                   (mutation (not (member action '("read" "view" "export" "list" "status" "library-view")))))
              (when (plist-get job :cancelled) (error "Editing operation cancelled"))
              (when-let* ((authorize (plist-get job :authorize)))
                (unless (mevedel-shared-editing--in-job job (funcall authorize))
                  (error "Editing authority ended")))
              ;; Another Emacs editing the item leaves it read-only here.  A
              ;; takeover asks only when the caller could have asked.
              (when mutation
                (condition-case nil
                    (let ((inhibit-interaction (plist-get job :inhibit-interaction)))
                      (mevedel-artifact-lease-ensure
                       workspace (mevedel-shared-editing--valid-id id)))
                  (inhibited-interaction
                   (error "This needs a decision in Emacs on the host first"))))
              (cond
               ((equal action "list")
                (mevedel-shared-editing--finish
                 runtime job (list :result (vconcat (mevedel-shared-editing-list workspace)))))
               ;; Deleting needs no helper; queued, it cannot overtake a save.
               ((equal action "delete")
                (mevedel-shared-editing--finish
                 runtime job (mevedel-shared-editing--delete workspace args)))
               (t
                (unless (member action '("create" "import" "status" "library-view"))
                  (setq args
                        (plist-put
                         args :state
                         (if (and (equal action "export") (plist-member args :version))
                             (progn
                               (unless (and (integerp (plist-get args :version))
                                            (> (plist-get args :version) 0))
                                 (error "Invalid artifact version"))
                               (mevedel-shared-editing--parse
                                (with-temp-buffer
                                  (insert-file-contents
                                   (mevedel-artifact-store-version-path
                                    workspace id (plist-get args :version)))
                                  (buffer-string))))
                           (mevedel-shared-editing--read workspace id)))))
                (when (member action '("create" "import"))
                  ;; Metadata naming the item without its state is a create
                  ;; that was interrupted; any other directory is taken.
                  (when (and (or (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
                                 (file-exists-p (mevedel-artifact-store-bookkeeping-directory workspace id)))
                             (not (mevedel-artifact-store-item-p
                                   (mevedel-artifact-store-meta workspace id))))
                    (error "Artifact %s already exists" id))
                  (when (mevedel-shared-editing--present-p workspace id)
                    (let ((existing (mevedel-shared-editing--read workspace id)))
                      (unless (and (stringp (plist-get args :opId))
                                   (plist-member (plist-get existing :receipts)
                                                 (intern (concat ":" (plist-get args :opId)))))
                        (error "Shared item already exists; reopen it instead"))
                      (setq args (plist-put args :state existing)
                            args (plist-put args :action "update")))))
                (setq args (plist-put args :requestId (plist-get job :requestId)))
                ;; A check starts fresh so repaired resources and runtime
                ;; changes are verified, without discarding queued work.
                (mevedel-shared-editing--send
                 (mevedel-shared-editing--process runtime (equal action "status")) args)
                (plist-put runtime :timeout
                           (run-at-time
                            30 nil (lambda () (mevedel-shared-editing-stop runtime)))))))
          ;; A quit at a takeover question must settle the job too, or the
          ;; workspace queue would wait for it forever.
          ((error quit)
           (mevedel-shared-editing--finish
            runtime job (list :error (if (eq (car err) 'quit) "Editing operation cancelled"
                                       (error-message-string err))))))))))

;;;###autoload
(defun mevedel-shared-editing-call (workspace args callback &optional authorize commit)
  "Run editor ARGS for WORKSPACE's shared items; deliver one reply to CALLBACK.
AUTHORIZE, when non-nil, must still permit the operation at admission and
commit.  Return a cancellation function; cancellation before commit keeps
accepted content unchanged.  COMMIT optionally runs the final commit and
callback with the caller's cancellation settlement deferred until its outcome.
All calls for one workspace in this Emacs are serialized, whichever room or
session makes them; CALLBACK runs in the buffer current now."
  (let ((runtime (mevedel-shared-editing--runtime workspace)))
    (when (>= (length (plist-get runtime :queue)) 64)
      (error "Shared editing queue is full"))
    (let* ((bytes (string-bytes (mevedel-shared-editing--json args)))
           (queued (cl-loop for job in (cons (plist-get runtime :active)
                                             (plist-get runtime :queue))
                            sum (or (plist-get job :bytes) 0)))
           (sequence (1+ (plist-get runtime :sequence)))
           (job (list :args args :callback callback :buffer (current-buffer)
                      :authorize authorize :commit commit :requestId sequence :bytes bytes
                      ;; The queue runs from timers, after the caller's
                      ;; binding is gone.
                      :inhibit-interaction inhibit-interaction)))
      (when (> (+ bytes queued) (* 32 1024 1024))
        (error "Shared editing queue is full; wait for pending saves"))
      (plist-put runtime :sequence sequence)
      (plist-put runtime :queue (append (plist-get runtime :queue) (list job)))
      (unless (plist-get runtime :active)
        (when-let* ((timer (plist-get runtime :timer)))
          (cancel-timer timer))
        (plist-put runtime :timer
                   (mevedel-transport-run-at-time 0 #'mevedel-shared-editing--drain runtime)))
      (lambda () (plist-put job :cancelled t)))))

(provide 'mevedel-shared-editing)
;;; mevedel-shared-editing.el ends here
