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
  (unless (mevedel-artifact-store-id-p id)
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

(defun mevedel-shared-editing-present-p (workspace id)
  "Return non-nil when WORKSPACE's store holds shared item ID."
  (and (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
       (file-exists-p (mevedel-shared-editing--state-path workspace id))))

(defun mevedel-shared-editing--read-item (workspace id)
  "Read committed shared item ID of WORKSPACE in one target-pinned batch.
Return (META-BYTES . STATE): the metadata's bytes, which a commit verifies
before replacing them, and the parsed state."
  (let* ((state-path (mevedel-shared-editing--state-path workspace id))
         (results (mevedel-session-control-fs-run-program
                   (list (list :op 'read :coding 'no-conversion
                               :path (file-name-concat
                                      (file-name-directory state-path) "meta.el"))
                         (list :op 'read :path state-path))))
         (bytes (progn
                  (when (cl-some (lambda (result) (eq 'absent (plist-get result :status)))
                                 results)
                    (error "This item no longer exists"))
                  (mevedel-session-control-fs-program-value (car results)))))
    (unless (mevedel-artifact-store-item-p
             (mevedel-artifact-store--validate-meta
              (read (decode-coding-string bytes 'utf-8-unix)) state-path))
      (error "This item no longer exists"))
    (cons bytes (mevedel-shared-editing--parse
                 (mevedel-session-control-fs-program-value (cadr results))))))

(defun mevedel-shared-editing--read (workspace id)
  "Read committed shared item ID of WORKSPACE."
  (cdr (mevedel-shared-editing--read-item workspace id)))

(defun mevedel-shared-editing--committed (runtime id)
  "Return RUNTIME's last commit of item ID as (META-BYTES . STATE), or nil.
It is the item's state while this Emacs still holds the item as it did
at that commit, since the lease fences every state write.
Other metadata writers are not fenced, so a commit verifies META-BYTES.
A stale entry is dropped."
  (let ((table (plist-get runtime :committed)))
    (when-let* ((entry (gethash id table)))
      (if (and (car entry)
               (eq (car entry)
                   (mevedel-artifact-lease-held (plist-get runtime :workspace) id)))
          (cdr entry)
        (remhash id table)
        nil))))

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
    (unless (mevedel-shared-editing-present-p workspace id)
      (error "This item no longer exists"))
    (mevedel-artifact-store--delete workspace id)
    (mevedel-artifact-lease-forget-item workspace id)
    (mevedel-shared-editing--notify
     workspace (list :id id :deleted t :actor (plist-get args :actor)) nil)
    (list :result (list :id id :deleted t))))

(defun mevedel-shared-editing-ids (workspace)
  "Return the ids of WORKSPACE's shared items, reading only their metadata."
  (mapcar (lambda (item) (plist-get item :id))
          (mevedel-shared-editing-list workspace)))

(defun mevedel-shared-editing-list (workspace)
  "Return WORKSPACE's shared item catalog from small, target-proved metadata.
An item's state can run to megabytes, and the prompt's resource roster
lists items on every request; metadata that cannot be read leaves its
item out rather than failing every request."
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

(defun mevedel-shared-editing--commit (workspace state &optional meta-bytes)
  "Durably commit candidate STATE of WORKSPACE under this Emacs's item lease.
Metadata records the committed kind, title and revision.  META-BYTES, the
metadata as last read, saves reading it again; when it is no longer
current the commit reads it and retries.  Return the metadata's bytes."
  (let* ((id (plist-get state :id))
         (directory (file-name-directory (mevedel-shared-editing--state-path workspace id)))
         (authored (file-name-concat (mevedel-artifact-store-directory workspace) id))
         (meta-path (file-name-concat directory "meta.el"))
         (bytes (or meta-bytes
                    (let ((metadata (car (mevedel-session-control-fs-run-program
                                          (list (list :op 'read :path meta-path
                                                      :coding 'no-conversion))))))
                      (unless (eq 'absent (plist-get metadata :status))
                        (mevedel-session-control-fs-program-value metadata)))))
         (meta (and bytes (car (read-from-string (decode-coding-string bytes 'utf-8-unix)))))
         (kind (intern (plist-get state :kind)))
         ;; What listings show beyond size and time: they follow content
         ;; changes with a delay.
         (listed (or (not bytes)
                     (not (equal (plist-get meta :title) (plist-get state :title)))
                     (not (eq (plist-get meta :kind) kind))))
         (changed (or listed
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
    (let* ((written (when changed
                      (encode-coding-string
                       (let ((print-length nil) (print-level nil)) (prin1-to-string meta))
                       'utf-8-unix)))
           (metadata-write (when changed (list :op 'write :path meta-path :content written)))
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
      (if (and meta-bytes changed (eq 'mismatch (plist-get (nth 2 results) :status)))
          ;; Metadata changed since it was read; nothing was written.
          (mevedel-shared-editing--commit workspace state)
        (unless (memq (plist-get (car results) :status) '(ok conflict))
          (mevedel-session-control-fs-program-value (car results)))
        (dolist (result (cdr results))
          (mevedel-session-control-fs-program-value result))
        (if listed
            (mevedel-artifact-store--changed workspace)
          (mevedel-artifact-store--content-changed workspace))
        (or written bytes)))))

(defun mevedel-shared-editing--version-state (state)
  "Return STATE as a version keeps it: without receipts and history."
  (let ((state (copy-sequence state)))
    (cl-remf state :receipts)
    (cl-remf state :transactions)
    state))

(defun mevedel-shared-editing-save-version (workspace id &optional session-id)
  "Record WORKSPACE's shared item ID as a new version and return its number.
SESSION-ID names the session that edited it, if any.  The state this
Emacs last committed serves while it still holds the item as it did then,
so a large item's history is not read back.
Once the lease was released, another Emacs may have edited the item."
  (let ((runtime (gethash (mevedel-workspace-root workspace)
                         mevedel-shared-editing--runtimes)))
    (mevedel-artifact-store-record-version
     workspace id session-id
     (mevedel-shared-editing--json
      (mevedel-shared-editing--version-state
       (or (cdr (and runtime (mevedel-shared-editing--committed runtime id)))
           (mevedel-shared-editing--read workspace id)))))))

(defun mevedel-shared-editing-save-version-later (workspace id &optional session-id)
  "Record WORKSPACE's shared item ID as a version through its editing queue.
It follows any save in progress and waits while the store's connection is
busy.  SESSION-ID names the session that edited it; a failure is a message."
  (mevedel-shared-editing-call
   workspace (list :action "version" :id id :session session-id)
   (lambda (reply)
     (when-let* ((failure (plist-get reply :error)))
       (message "mevedel: no version of %s was saved: %s" id failure)))))

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
        (puthash key (list :workspace workspace :queue nil :sequence 0
                           ;; Item id -> (LEASE-HOLDING META-BYTES . STATE) of
                           ;; its last commit.
                           :committed (make-hash-table :test #'equal))
                 mevedel-shared-editing--runtimes))))

(defun mevedel-shared-editing--live-p (runtime)
  "Return non-nil while RUNTIME is still its workspace's runtime."
  (when-let* ((workspace (plist-get runtime :workspace)))
    (eq runtime (gethash (mevedel-workspace-root workspace)
                         mevedel-shared-editing--runtimes))))

(defun mevedel-shared-editing-item-busy-p (workspace id)
  "Return non-nil while WORKSPACE's runtime has work for item ID.
A held item lease is neither released nor handed over while it does."
  (when-let* ((runtime (gethash (mevedel-workspace-root workspace)
                                mevedel-shared-editing--runtimes)))
    (cl-some (lambda (job) (and job (equal id (plist-get (plist-get job :args) :id))))
             (cons (plist-get runtime :active) (plist-get runtime :queue)))))

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
  (let ((jobs (append (mapcar #'car (plist-get runtime :batch))
                      (cons (plist-get runtime :active) (plist-get runtime :queue)))))
    ;; Retire jobs before callbacks: they can reenter stop, and a delayed
    ;; helper reply must never commit an edit already reported as unsaved.
    (plist-put runtime :active nil)
    (plist-put runtime :queue nil)
    (plist-put runtime :batch nil)
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
    (mevedel-shared-editing--flush-batch runtime)
    ;; A stop requested during the flush settled JOB already.
    (when (eq job (plist-get runtime :active))
      (plist-put runtime :active nil)
      (unwind-protect (mevedel-shared-editing--in-job job
                        (funcall (plist-get job :callback) reply))
        (when (mevedel-shared-editing--live-p runtime)
          (plist-put runtime :timer
                     (mevedel-transport-run-at-time 0 #'mevedel-shared-editing--drain runtime)))))))

(defun mevedel-shared-editing--store-busy-p (runtime)
  "Return non-nil when RUNTIME's store must not be touched now."
  (mevedel-transport-busy-p
   (mevedel-artifact-store-directory (plist-get runtime :workspace))))

(defconst mevedel-shared-editing--batch-limit 16
  "Most consecutive edits of one item committed together.")

(defun mevedel-shared-editing--store (runtime state meta-bytes)
  "Commit STATE in RUNTIME's workspace and remember it as the last commit.
META-BYTES is as `mevedel-shared-editing--commit' takes it.  Entries of
items no longer held as they were are dropped with their states."
  (let* ((workspace (plist-get runtime :workspace))
         (table (plist-get runtime :committed))
         (id (plist-get state :id))
         (bytes (mevedel-shared-editing--commit workspace state meta-bytes)))
    (puthash id (cons (mevedel-artifact-lease-held workspace id) (cons bytes state)) table)
    (maphash (lambda (key _entry) (mevedel-shared-editing--committed runtime key)) table)))

;; Under load a commit, one fenced target program, outlasts computing an
;; edit, so writers queue behind each other's commits.  Consecutive edits
;; of one item therefore commit once: each is computed from the state the
;; previous one left, uncommitted, and all are acknowledged and announced,
;; in order, after the commit that includes them.  The batch is
;; RUNTIME's `:batch', a list of (JOB . REPLY).

(defun mevedel-shared-editing--batchable-p (job)
  "Return non-nil when JOB is an edit that may commit with others."
  (and (member (plist-get (plist-get job :args) :action)
               '("update" "patch" "insert" "rename" "background"))
       (not (plist-get job :commit))
       (not (plist-get job :cancelled))))

(defun mevedel-shared-editing--defer-p (runtime job reply)
  "Return non-nil when JOB's REPLY may wait for the next queued edit's commit."
  (let ((next (car (plist-get runtime :queue)))
        (id (plist-get (plist-get job :args) :id)))
    (and next
         (equal id (plist-get (plist-get reply :state) :id))
         (equal id (plist-get (plist-get next :args) :id))
         (mevedel-shared-editing--batchable-p job)
         (mevedel-shared-editing--batchable-p next)
         (< (1+ (length (plist-get runtime :batch)))
            mevedel-shared-editing--batch-limit))))

(defun mevedel-shared-editing--defer (runtime job reply)
  "Keep JOB's REPLY uncommitted in RUNTIME's batch and start the next edit.
The next edit starts from REPLY's state."
  (plist-put runtime :batch (append (plist-get runtime :batch) (list (cons job reply))))
  (when-let* ((timer (plist-get runtime :timeout)))
    (cancel-timer timer))
  (plist-put runtime :timeout nil)
  (plist-put runtime :active nil)
  (plist-put runtime :timer
             (mevedel-transport-run-at-time 0 #'mevedel-shared-editing--drain runtime)))

(defun mevedel-shared-editing--batch-tip (runtime)
  "Return (META-BYTES . STATE) that RUNTIME's batched edits leave.
Signal unless they may still commit: none was cancelled or lost its
authority, and this Emacs holds the item as it did when they were
computed, so nobody else can have replaced the state they start from.
META-BYTES are those the batch's first edit started from."
  (let ((last (car (last (plist-get runtime :batch)))))
    (pcase-dolist (`(,job . ,_) (plist-get runtime :batch))
      (when (plist-get job :cancelled) (error "Editing operation cancelled"))
      (when-let* ((authorize (plist-get job :authorize)))
        (unless (mevedel-shared-editing--in-job job (funcall authorize))
          (error "Editing authority ended"))))
    (unless (and (plist-get (car last) :holding)
                 (eq (plist-get (car last) :holding)
                     (mevedel-artifact-lease-held
                      (plist-get runtime :workspace)
                      (plist-get (plist-get (cdr last) :state) :id))))
      (error "This Emacs lost the item while saving; edits were not saved"))
    (cons (car (plist-get (car last) :committed)) (plist-get (cdr last) :state))))

(defun mevedel-shared-editing--settle-batch (runtime &optional error)
  "Settle RUNTIME's batched edits, announcing them unless ERROR says why not."
  (let ((batch (plist-get runtime :batch)))
    (plist-put runtime :batch nil)
    (pcase-dolist (`(,job . ,reply) batch)
      (condition-case nil
          (mevedel-shared-editing--in-job job
            (unless error
              (mevedel-shared-editing--notify
               (plist-get runtime :workspace)
               (plist-get reply :state) (plist-get reply :result)))
            (funcall (plist-get job :callback) (if error (list :error error) reply)))
        (error nil)))))

(defun mevedel-shared-editing--flush-batch (runtime)
  "Commit RUNTIME's batched edits on their own, then settle them.
The edit that was to continue them did not commit."
  (when (plist-get runtime :batch)
    (plist-put runtime :committing t)
    (unwind-protect
        (condition-case err
            (let ((tip (mevedel-shared-editing--batch-tip runtime)))
              (mevedel-shared-editing--store runtime (cdr tip) (car tip))
              (mevedel-shared-editing--settle-batch runtime))
          (error (mevedel-shared-editing--settle-batch
                  runtime (error-message-string err))))
      (plist-put runtime :committing nil)
      (when (plist-get runtime :stop-requested)
        (mevedel-shared-editing-stop runtime)))))

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
            (if (mevedel-shared-editing--defer-p runtime job reply)
                (mevedel-shared-editing--defer runtime job reply)
              (mevedel-shared-editing--in-job job
                (funcall
                 (or (plist-get job :commit) #'funcall)
                 (lambda ()
                   (plist-put runtime :committing t)
                   (unwind-protect
                       (progn
                         (when-let* ((state (plist-get reply :state)))
                           (when (plist-get runtime :batch)
                             (mevedel-shared-editing--batch-tip runtime))
                           (mevedel-shared-editing--store
                            runtime state
                            (and (equal (plist-get state :id)
                                        (plist-get (plist-get job :args) :id))
                                 (car (plist-get job :committed))))
                           ;; The batch this edit continued is in that commit.
                           (mevedel-shared-editing--settle-batch runtime)
                           (mevedel-shared-editing--notify
                            (plist-get runtime :workspace) state (plist-get reply :result)))
                         (mevedel-shared-editing--finish runtime job reply))
                     (plist-put runtime :committing nil)
                     (when (plist-get runtime :stop-requested)
                       (mevedel-shared-editing-stop runtime))))))))
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
                                 ;; Finishing can commit a batch; never from
                                 ;; inside a filter that may run in a remote
                                 ;; operation.
                                 (error
                                  (mevedel-transport-run-at-time
                                   0 #'mevedel-shared-editing--finish
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
            (catch 'stopped
              (let* ((args (copy-sequence (plist-get job :args)))
                     (action (plist-get args :action))
                     (id (plist-get args :id))
                     (mutation (not (member action '("read" "view" "export" "list" "status" "library-view"
                                                    "version"))))
                     ;; Target I/O runs timers and sentinels; one that stopped
                     ;; the runtime has settled this job already.
                     (current (lambda ()
                                (unless (and (mevedel-shared-editing--live-p runtime)
                                             (eq job (plist-get runtime :active)))
                                  (throw 'stopped nil)))))
                (when (plist-get job :cancelled) (error "Editing operation cancelled"))
                (when-let* ((authorize (plist-get job :authorize)))
                  (unless (mevedel-shared-editing--in-job job (funcall authorize))
                    (error "Editing authority ended")))
                ;; A stale browser editing a deleted item must not lease it
                ;; back into being.  A held lease means it still exists: a
                ;; deletion needs the lease too.
                (when (and mutation (not (member action '("create" "import")))
                           (not (mevedel-artifact-lease-held
                                 workspace (mevedel-shared-editing--valid-id id)))
                           (not (mevedel-shared-editing-present-p workspace id)))
                  (error "This item no longer exists"))
                ;; Another Emacs editing the item leaves it read-only here.
                (when mutation
                  (mevedel-artifact-lease-ensure
                   workspace (mevedel-shared-editing--valid-id id))
                  (plist-put job :holding (mevedel-artifact-lease-held workspace id))
                  ;; Until this job commits, the disk is the only truth; the
                  ;; commit it starts from travels with the job.  A batch
                  ;; pending is of this item, which the job continues.
                  (unless (member action '("create" "import"))
                    (plist-put job :committed
                               (if (plist-get runtime :batch)
                                   (mevedel-shared-editing--batch-tip runtime)
                                 (mevedel-shared-editing--committed runtime id))))
                  (remhash id (plist-get runtime :committed)))
                (funcall current)
                (cond
                 ((equal action "list")
                  (mevedel-shared-editing--finish
                   runtime job (list :result (vconcat (mevedel-shared-editing-list workspace)))))
                 ((equal action "version")
                  (mevedel-shared-editing--finish
                   runtime job (list :result (mevedel-shared-editing-save-version
                                              workspace id (plist-get args :session)))))
                 ;; Deleting needs no helper; queued, it cannot overtake a save.
                 ((equal action "delete")
                  (mevedel-shared-editing--finish
                   runtime job (mevedel-shared-editing--delete workspace args)))
                 (t
                  (unless (member action '("create" "import" "status" "library-view"))
                    (setq args
                          (plist-put
                           args :state
                           ;; A historical preview exports a saved version
                           ;; without touching the live item.
                           (if (and (equal action "export") (plist-member args :version))
                               (progn
                                 (unless (and (integerp (plist-get args :version))
                                              (> (plist-get args :version) 0))
                                   (error "Invalid artifact version"))
                                 (mevedel-shared-editing--parse
                                  (mevedel-session-control-fs-read-file
                                   (mevedel-artifact-store-version-path
                                    workspace id (plist-get args :version)))))
                             (let ((item (or (plist-get job :committed)
                                             (mevedel-shared-editing--committed runtime id)
                                             (mevedel-shared-editing--read-item workspace id))))
                               (plist-put job :committed item)
                               (cdr item))))))
                  (when (member action '("create" "import"))
                    ;; Metadata naming the item without its state is a create
                    ;; that was interrupted; any other directory is taken.
                    (when (and (or (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
                                   (file-exists-p (mevedel-artifact-store-bookkeeping-directory
                                                   workspace id)))
                               (not (mevedel-artifact-store-item-p
                                     (mevedel-artifact-store-meta workspace id))))
                      (error "Artifact %s already exists" id))
                    (when (mevedel-shared-editing-present-p workspace id)
                      (let ((existing (mevedel-shared-editing--read workspace id)))
                        (unless (and (stringp (plist-get args :opId))
                                     (plist-member (plist-get existing :receipts)
                                                   (intern (concat ":" (plist-get args :opId)))))
                          (error "Shared item already exists; reopen it instead"))
                        (setq args (plist-put args :state existing)
                              args (plist-put args :action "update")))))
                  (setq args (plist-put args :requestId (plist-get job :requestId)))
                  (funcall current)
                  ;; A check starts fresh so repaired resources and runtime
                  ;; changes are verified, without discarding queued work.
                  (mevedel-shared-editing--send
                   (mevedel-shared-editing--process runtime (equal action "status")) args)
                  (plist-put runtime :timeout
                             (run-at-time
                              30 nil (lambda () (mevedel-shared-editing-stop runtime))))))))
          ;; A quit during target I/O must settle the job too, or the
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
                      :authorize authorize :commit commit :requestId sequence :bytes bytes)))
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
