;;; niwa-session-save-as.el --- Portable Save As transaction -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the portable session Save As transaction.  The interface is
;; `niwa-session-save-as-run': it validates the committed parent, creates
;; and publishes a fresh child, moves it into discoverability, adopts the
;; committed child into the live session, and returns one committed result.
;; Pre-commit failures remove hidden staging and re-signal the original error;
;; post-commit failures retain the child and report the finalization error.

;;; Code:

(require 'niwa-engine)

;; `setf' on a slot of a struct defined elsewhere needs that
;; `cl-defstruct' at compile time: `declare-function' supplies the
;; accessor but not the setter, and without the expander the form
;; compiles to a call to a function that does not exist.
(eval-when-compile (require 'niwa-structs))

(eval-when-compile (require 'niwa-utilities))
(require 'niwa-session-artifacts)
(require 'niwa-session-codec)
(require 'niwa-session-durability)
(require 'niwa-session-fork)
(require 'niwa-session-persistence)
(require 'niwa-session-rewind)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `niwa-schedule'
(declare-function niwa-schedule-hand-back "niwa-schedule" (session))
(declare-function niwa-schedule-hand-over "niwa-schedule" (session))
(declare-function niwa-schedule-restore-session "niwa-schedule" (session buffer))
(autoload 'niwa-schedule-hand-back "niwa-schedule")
(autoload 'niwa-schedule-hand-over "niwa-schedule")
(autoload 'niwa-schedule-restore-session "niwa-schedule")

;; `niwa-session-artifacts'
(declare-function niwa-execution-session-live-spools "niwa-execution" (session))
(autoload 'niwa-execution-session-live-spools "niwa-execution")
(declare-function niwa-session-artifacts--live-sidecar
                  "niwa-session-artifacts" (session &optional data-buffer))
(declare-function niwa-session-artifacts-printed-value
                  "niwa-session-artifacts" (value))
(declare-function niwa-session-artifacts-sidecar-path
                  "niwa-session-artifacts" (save-path))

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p
                  "niwa-session-codec" (session))
(declare-function niwa-session-codec-write "niwa-session-codec"
                  (path plist))

;; `niwa-session-durability'
(declare-function niwa-session-durability-adopt-owned-lease
                  "niwa-session-durability" (session source))
(declare-function niwa-session-durability-call-with-reserved-lease
                  "niwa-session-durability" (session function))
(declare-function niwa-session-durability-forget-removed-session
                  "niwa-session-durability" (session))

;; `niwa-session-fork'
(declare-function niwa-session-fork-clone-session
                  "niwa-session-fork"
                  (session policy &rest keys))

;; `niwa-session-naming'
(declare-function niwa-session-naming-adopt-explicit "niwa-session-naming" (session name))
(autoload 'niwa-session-naming-adopt-explicit "niwa-session-naming")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-lock-acquire
                  "niwa-session-persistence"
                  (session-dir buffer-name &optional session))
(declare-function niwa-session-persistence-lock-release
                  "niwa-session-persistence"
                  (session-dir &optional session))

;; `niwa-session-publication'
(declare-function niwa-session-publication-publish
                  "niwa-session-publication"
                  (session artifacts &optional require-commit))
(declare-function niwa-session-publication-read
                  "niwa-session-publication"
                  (session-dir &optional head names))

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-materialize-publication
                  "niwa-session-rewind"
                  (session publication staging-path))
(declare-function niwa-session-rewind-rewind-publication-artifacts
                  "niwa-session-rewind"
                  (session buffer staging-path &optional state))

;; `niwa-structs'
(declare-function niwa-session-buffer-name
                  "niwa-structs" (session-name workspace))
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-forked-from-session-id
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-forked-from-turn
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-lease
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-name
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-updated-at
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace
                  "niwa-structs" (cl-x) t)

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-warning "niwa-utilities"
                  (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

(defvar niwa--view-buffer)


;;
;;; Transaction state

(defconst niwa-session-save-as--committed-stages
  '(published moved adopted)
  "Transaction stages after the child publication marker is committed.")

(defun niwa-session-save-as--committed-p (transaction)
  "Return non-nil when TRANSACTION has a retained child publication."
  (memq (plist-get transaction :stage)
        niwa-session-save-as--committed-stages))

(defun niwa-session-save-as--record-error (transaction error)
  "Record the first ERROR in TRANSACTION and return the transaction."
  (if (plist-get transaction :error)
      transaction
    (plist-put transaction :error error)))

(defun niwa-session-save-as--read-publication (path)
  "Read PATH's publication, returning nil when it is not readable yet."
  (condition-case nil
      (niwa-session-publication-read path)
    (error nil)))

(defun niwa-session-save-as--validate
    (session buffer new-name new-id new-save-path)
  "Return a validated Save As transaction for SESSION and BUFFER.

NEW-NAME, NEW-ID, and NEW-SAVE-PATH identify the child.  The source path and
id are read from SESSION so the transaction cannot silently target a stale
parent identity."
  (niwa-engine-assert-local-history session "Save As")
  (unless (niwa-session-save-path session)
    (error "Portable Save As requires a materialized parent session"))
  (unless (niwa-session-execution-target session)
    (error "Portable Save As requires an execution target"))
  (unless (niwa-session-codec-portable-authority-p session)
    (error "Portable Save As requires portable authority"))
  (unless (buffer-live-p buffer)
    (error "Portable Save As requires a live data buffer"))
  (let* ((old-save-path (niwa-session-save-path session))
         (old-id (niwa-session-session-id session))
         (publication
          (or (niwa-session-publication session)
              (niwa-session-save-as--read-publication old-save-path)
              (error "Remote Save As requires a committed publication")))
         (head (plist-get publication :head)))
    (unless (and (stringp new-name)
                 (stringp new-id)
                 (stringp new-save-path)
                 (not (string-empty-p new-name))
                 (not (string-empty-p new-id)))
      (error "Portable Save As requires a complete child identity"))
    (unless head
      (error "Remote Save As requires a committed publication"))
    (list :session session
          :buffer buffer
          :old-save-path old-save-path
          :old-id old-id
          :new-name new-name
          :new-id new-id
          :new-save-path new-save-path
          :old-buffer-file (buffer-local-value 'buffer-file-name buffer)
          :parent-publication publication
          :parent-head head
          :stage 'validated)))


;;
;;; Stages

(defun niwa-session-save-as--hand-over (transaction)
  "Durably clear the parent's wake-ups before any child can hold them.
A wake-up then lives in at most one store: a crash before the child commits
loses it rather than firing it twice.  The commit moves the parent head,
so TRANSACTION captures the new one."
  (let ((session (plist-get transaction :session)))
    (when (condition-case err
              (niwa-schedule-hand-over session)
            (error
             (user-error "Save As stopped: the original's wake-ups could not be cleared (%s)"
                         (error-message-string err))))
      (let ((publication
             (or (niwa-session-save-as--read-publication
                  (plist-get transaction :old-save-path))
                 (error "Remote Save As requires a committed publication"))))
        (plist-put transaction :handed-over t)
        (plist-put transaction :parent-publication publication)
        (plist-put transaction :parent-head (plist-get publication :head))))
    transaction))

(defun niwa-session-save-as--capture-parent (transaction)
  "Recheck TRANSACTION's parent head while its lease is reserved."
  (let* ((path (plist-get transaction :old-save-path))
         (current (niwa-session-save-as--read-publication path)))
    (unless (and current
                 (equal (plist-get transaction :parent-head)
                        (plist-get current :head)))
      (user-error "Session state changed before portable Save As; retry"))
    (plist-put transaction :parent-publication current)))

(defun niwa-session-save-as--stage-child (transaction)
  "Create TRANSACTION's hidden child and its explicit logical clone."
  (let* ((old-save-path (plist-get transaction :old-save-path))
         (parent-directory
          (file-name-directory (directory-file-name old-save-path)))
         (staging-path
          (file-name-as-directory
           (make-temp-file
           (expand-file-name ".niwa-save-as-" parent-directory) t)))
         ;; Cleanup owns the directory even if clone construction exits.
         (_ (plist-put transaction :staging-path staging-path))
         (now (format-time-string "%FT%H-%M-%S"))
         (child
          (niwa-session-fork-clone-session
           (plist-get transaction :session) 'save-as
           :save-path staging-path
           :session-id (plist-get transaction :new-id)
           :name (plist-get transaction :new-name)
           :created-at now
           :updated-at now
           :forked-from-session-id (plist-get transaction :old-id))))
    (plist-put transaction :child child)
    (plist-put transaction :stage 'staged)))

(defun niwa-session-save-as--claim-child (transaction)
  "Claim TRANSACTION's child lease before writing staged bytes."
  (unless
      (niwa-session-persistence-lock-acquire
       (plist-get transaction :staging-path)
       (buffer-name (plist-get transaction :buffer))
       (plist-get transaction :child))
    (error "Could not acquire Save As child lease"))
  (plist-put transaction :stage 'claimed))

(defun niwa-session-save-as--materialize-child (transaction)
  "Materialize and sidecar-write TRANSACTION's child before publication."
  (let ((child (plist-get transaction :child))
        (buffer (plist-get transaction :buffer))
        (staging-path (plist-get transaction :staging-path)))
    (niwa-session-rewind-materialize-publication
     (plist-get transaction :session)
     (plist-get transaction :parent-publication)
     staging-path)
    ;; Retained execution output is plain storage outside the publication.
    ;; Pending (hidden) and still-written spools stay with the parent.
    (let ((spool (file-name-concat (plist-get transaction :old-save-path)
                                   "tool-results" "executions"))
          (target (file-name-concat staging-path "tool-results" "executions"))
          (live (niwa-execution-session-live-spools
                 (plist-get transaction :session))))
      (when (file-directory-p spool)
        (dolist (file (directory-files spool t "\\`[^.]"))
          (when (and (file-regular-p file)
                     (not (member (expand-file-name file) live)))
            (make-directory target t)
            (copy-file file (file-name-concat target (file-name-nondirectory file))
                       nil t)))))
    (niwa-session-codec-write
     (niwa-session-artifacts-sidecar-path staging-path)
     (niwa-session-artifacts--live-sidecar child buffer))
    (plist-put transaction :stage 'materialized)))

(defun niwa-session-save-as--publish-child (transaction)
  "Publish TRANSACTION's complete child and classify publication errors."
  (let ((child (plist-get transaction :child))
        (buffer (plist-get transaction :buffer))
        (staging-path (plist-get transaction :staging-path)))
    (condition-case error
        (unwind-protect
            (progn
              (niwa-session-publication-publish
               child
               (niwa-session-rewind-rewind-publication-artifacts
                child buffer staging-path child))
              (plist-put transaction :stage 'published))
          ;; A throw or quit after publication must not make outer cleanup
          ;; mistake the committed child for disposable staging.
          (unless (niwa-session-save-as--committed-p transaction)
            (let ((publication
                   (niwa-session-save-as--read-publication staging-path))
                  (committed-head
                   (plist-get (niwa-session-lease child) :publication-head)))
              (when (or publication committed-head)
                (plist-put transaction :publication publication)
                (plist-put transaction :stage 'published)))))
      (error
       (if (niwa-session-save-as--committed-p transaction)
           (niwa-session-save-as--record-error transaction error)
         (signal (car error) (cdr error)))))))

(defun niwa-session-save-as--move-child (transaction)
  "Move TRANSACTION's published child into its discoverable path."
  (condition-case error
      (progn
        (niwa-session-durability-call-with-reserved-lease
         (plist-get transaction :child)
         (lambda ()
           (rename-file
            (directory-file-name (plist-get transaction :staging-path))
            (directory-file-name (plist-get transaction :new-save-path)))
           (setf (plist-get transaction :stage) 'moved
                 (niwa-session-save-path (plist-get transaction :child))
                 (plist-get transaction :new-save-path)))))
    (error
     (niwa-session-save-as--record-error transaction error)))
  transaction)

(defun niwa-session-save-as--refresh-publication (transaction)
  "Refresh TRANSACTION's child publication after move or publication error."
  (when (niwa-session-save-as--committed-p transaction)
    (let ((publication
           (or (niwa-session-save-as--read-publication
                (if (eq (plist-get transaction :stage) 'moved)
                    (plist-get transaction :new-save-path)
                  (plist-get transaction :staging-path)))
               (plist-get transaction :publication))))
      (if publication
          (plist-put transaction :publication publication)
        (niwa-session-save-as--record-error
         transaction
         (list 'error "Committed Save As child publication is unavailable"))))))

(defun niwa-session-save-as--run-stages (transaction)
  "Run TRANSACTION's target stages under the reserved parent lease."
  (let ((session (plist-get transaction :session)))
    (niwa-session-durability-call-with-reserved-lease
     session
     (lambda ()
       (setq transaction
             (niwa-session-save-as--capture-parent transaction))
       (setq transaction
             (niwa-session-save-as--stage-child transaction))
       (setq transaction
             (niwa-session-save-as--claim-child transaction))
       (setq transaction
             (niwa-session-save-as--materialize-child transaction))
       (setq transaction
             (niwa-session-save-as--publish-child transaction))
       (when (niwa-session-save-as--committed-p transaction)
         (setq transaction
               (niwa-session-save-as--move-child transaction))
         (setq transaction
               (niwa-session-save-as--refresh-publication transaction)))
       transaction))))


;;
;;; Adoption and cleanup

(defun niwa-session-save-as--adopt-child (transaction)
  "Adopt TRANSACTION's committed child into its live session."
  (when (and (niwa-session-save-as--committed-p transaction)
             (plist-get transaction :child)
             (plist-get transaction :publication))
    (let* ((session (plist-get transaction :session))
           (child (plist-get transaction :child))
           (buffer (plist-get transaction :buffer))
           (committed-path
            (if (eq (plist-get transaction :stage) 'moved)
                (plist-get transaction :new-save-path)
              (plist-get transaction :staging-path))))
      (condition-case error
          (progn
            (niwa-session-durability-adopt-owned-lease session child)
            (setf (niwa-session-session-id session)
                  (plist-get transaction :new-id)
                  ;; The saved copy is the user's own chat.
                  (niwa-session-dedicated-artifact session) nil
                  (niwa-session-forked-from-session-id session)
                  (plist-get transaction :old-id)
                  (niwa-session-forked-from-turn session)
                  (niwa-session-turn-count child)
                  (niwa-session-updated-at session)
                  (niwa-session-updated-at child)
                  (niwa-session-publication session)
                  (plist-get transaction :publication)
                  (niwa-session-checkpoint-state session)
                  (niwa-session-checkpoint-state child))
            (niwa-session-naming-adopt-explicit session (plist-get transaction :new-name))
            (with-current-buffer buffer
              (setq buffer-file-name
                    (and (plist-get transaction :old-buffer-file)
                         (file-name-concat
                          committed-path
                          (file-name-nondirectory
                           (plist-get transaction :old-buffer-file)))))
              (setq buffer-file-truename nil))
            (condition-case release-error
                (niwa-session-persistence-lock-release
                 (plist-get transaction :old-save-path))
              (error
               (setq transaction
                     (niwa-session-save-as--record-error
                      transaction release-error))))
            (niwa-session-save-as--rename-live-session-buffers
             session buffer)
            (niwa-schedule-restore-session session buffer)
            (plist-put transaction :stage 'adopted))
        (error
         (niwa-session-save-as--record-error transaction error))))
    transaction))

(defun niwa-session-save-as--cleanup (transaction)
  "Release and remove TRANSACTION's uncommitted staging state."
  (unless (niwa-session-save-as--committed-p transaction)
    (let ((child (plist-get transaction :child))
          (staging-path (plist-get transaction :staging-path)))
      (when (and child (memq (plist-get transaction :stage)
                             '(claimed materialized)))
        (condition-case error
            (niwa-session-persistence-lock-release staging-path child)
          (error
           (setq transaction
                 (niwa-session-save-as--record-error
                  transaction error)))))
      (when (and staging-path (file-directory-p staging-path))
        (condition-case error
            (delete-directory staging-path t)
          (error
           (setq transaction
                 (niwa-session-save-as--record-error
                  transaction error)))))
      (when (and child staging-path
                 (not (file-directory-p staging-path)))
        (condition-case error
            (niwa-session-durability-forget-removed-session child)
          (error
           (setq transaction
                 (niwa-session-save-as--record-error
                  transaction error)))))
      ;; No child holds the wake-ups, so the original takes them back.
      (when (plist-get transaction :handed-over)
        (condition-case error
            (niwa-schedule-hand-back (plist-get transaction :session))
          (error
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope (plist-get transaction :session)) 'niwa
            (format "Save As failed and the original's wake-ups could not be restored: %s"
                    (error-message-string error))
            :warning))))))
  transaction)

(defun niwa-session-save-as--finish (transaction)
  "Return TRANSACTION's result or signal its classified error."
  (let ((error (plist-get transaction :error)))
    (when error
      (if (niwa-session-save-as--committed-p transaction)
          (error "Remote Save As committed a child, but finalization failed: %s"
                 (error-message-string error))
        (signal (car error) (cdr error))))
    (list :status 'committed
          :session (plist-get transaction :session)
          :save-path (niwa-session-save-path
                      (plist-get transaction :session))
          :publication (plist-get transaction :publication))))

(defun niwa-session-save-as-run
    (session buffer new-name new-id new-save-path)
  "Run portable Save As for SESSION and live root BUFFER.

NEW-NAME, NEW-ID, and NEW-SAVE-PATH identify the child.  Return a result plist
with `:status' `committed', the adopted live `:session', its `:save-path', and
the committed `:publication'.  Pre-commit errors are re-signaled after
staging cleanup.  Once the child marker commits, errors retain the child and
are signaled as finalization errors.  Throws and quits propagate after cleanup
or committed-child adoption.

The caller must already have checked live mutation authority.  This module
rechecks the portable parent head while holding the reserved lease and uses
the existing lease/publication gates for every child write."
  (niwa--with-gc-busy
    (let ((transaction
           (niwa-session-save-as--validate
            session buffer new-name new-id new-save-path)))
      (unwind-protect
          (condition-case error
              (progn
                (setq transaction
                      (niwa-session-save-as--hand-over transaction))
                (setq transaction
                      (niwa-session-save-as--run-stages transaction)))
            (error
             (setq transaction
                   (niwa-session-save-as--record-error transaction error))))
        (if (niwa-session-save-as--committed-p transaction)
            (setq transaction
                  (niwa-session-save-as--adopt-child transaction))
          (setq transaction
                (niwa-session-save-as--cleanup transaction))))
      (niwa-session-save-as--finish transaction))))

(defun niwa-session-save-as--rename-live-session-buffers
    (session data-buffer)
  "Rename DATA-BUFFER and its view for SESSION's current name."
  (let* ((new-data-name
          (niwa-session-buffer-name
           (niwa-session-name session)
           (niwa-session-workspace session)))
         (view-buffer
          (buffer-local-value 'niwa--view-buffer data-buffer)))
    (with-current-buffer data-buffer
      (rename-buffer new-data-name t))
    (when (buffer-live-p view-buffer)
      (with-current-buffer view-buffer
        (rename-buffer
         (if (string-match "\\*$" new-data-name)
             (replace-match ":view*" t t new-data-name)
           (concat new-data-name ":view"))
         t)))))

(provide 'niwa-session-save-as)

;;; niwa-session-save-as.el ends here
