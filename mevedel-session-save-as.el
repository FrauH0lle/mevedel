;;; mevedel-session-save-as.el --- Portable Save As transaction -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the portable project Save As transaction.  The interface is
;; `mevedel-session-save-as-run': it validates the committed parent, creates
;; and publishes a fresh child, moves it into discoverability, adopts the
;; committed child into the live session, and returns one committed result.
;; Pre-commit failures remove hidden staging and re-signal the original error;
;; post-commit failures retain the child and report the finalization error.

;;; Code:

(require 'mevedel-session-artifacts)
(require 'mevedel-session-codec)
(require 'mevedel-session-durability)
(require 'mevedel-session-fork)
(require 'mevedel-session-persistence)
(require 'mevedel-session-rewind)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-build-sidecar
                  "mevedel-session-artifacts" (session buffer))
(declare-function mevedel-session-artifacts-printed-value
                  "mevedel-session-artifacts" (value))
(declare-function mevedel-session-artifacts-sidecar-path
                  "mevedel-session-artifacts" (save-path))

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-portable-authority-p
                  "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-write "mevedel-session-codec"
                  (path plist))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-adopt-owned-lease
                  "mevedel-session-durability" (session source))
(declare-function mevedel-session-durability-call-with-reserved-lease
                  "mevedel-session-durability" (session function))
(declare-function mevedel-session-durability-forget-removed-session
                  "mevedel-session-durability" (session))

;; `mevedel-session-fork'
(declare-function mevedel-session-fork-clone-session
                  "mevedel-session-fork"
                  (session policy &rest keys))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-lock-acquire
                  "mevedel-session-persistence"
                  (session-dir buffer-name &optional session))
(declare-function mevedel-session-persistence-lock-release
                  "mevedel-session-persistence"
                  (session-dir &optional session))

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-publish
                  "mevedel-session-publication"
                  (session artifacts &optional require-commit))
(declare-function mevedel-session-publication-read
                  "mevedel-session-publication" (session-dir))

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-materialize-publication
                  "mevedel-session-rewind"
                  (session publication staging-path))
(declare-function mevedel-session-rewind-rewind-publication-artifacts
                  "mevedel-session-rewind"
                  (session buffer staging-path &optional state))

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-session-id
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-turn
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-updated-at
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-buffer-name
                  "mevedel-structs" (session-name workspace))

(defvar mevedel--view-buffer)


;;
;;; Transaction state

(defconst mevedel-session-save-as--committed-stages
  '(published moved adopted)
  "Transaction stages after the child publication marker is committed.")

(defun mevedel-session-save-as--committed-p (transaction)
  "Return non-nil when TRANSACTION has a retained child publication."
  (memq (plist-get transaction :stage)
        mevedel-session-save-as--committed-stages))

(defun mevedel-session-save-as--record-error (transaction error)
  "Record the first ERROR in TRANSACTION and return the transaction."
  (if (plist-get transaction :error)
      transaction
    (plist-put transaction :error error)))

(defun mevedel-session-save-as--read-publication (path)
  "Read PATH's publication, returning nil when it is not readable yet."
  (condition-case nil
      (mevedel-session-publication-read path)
    (error nil)))

(defun mevedel-session-save-as--validate
    (session buffer new-name new-id new-save-path)
  "Return a validated Save As transaction for SESSION and BUFFER.

NEW-NAME, NEW-ID, and NEW-SAVE-PATH identify the child.  The source path and
id are read from SESSION so the transaction cannot silently target a stale
parent identity."
  (unless (mevedel-session-save-path session)
    (error "Portable Save As requires a materialized parent session"))
  (unless (mevedel-session-execution-target session)
    (error "Portable Save As requires an execution target"))
  (unless (mevedel-session-codec-portable-authority-p session)
    (error "Portable Save As requires portable authority"))
  (unless (buffer-live-p buffer)
    (error "Portable Save As requires a live data buffer"))
  (let* ((old-save-path (mevedel-session-save-path session))
         (old-id (mevedel-session-session-id session))
         (publication
          (or (mevedel-session-publication session)
              (mevedel-session-save-as--read-publication old-save-path)
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

(defun mevedel-session-save-as--capture-parent (transaction)
  "Recheck TRANSACTION's parent head while its lease is reserved."
  (let* ((path (plist-get transaction :old-save-path))
         (current (mevedel-session-save-as--read-publication path)))
    (unless (and current
                 (equal (plist-get transaction :parent-head)
                        (plist-get current :head)))
      (user-error "Session state changed before portable Save As; retry"))
    (plist-put transaction :parent-publication current)))

(defun mevedel-session-save-as--stage-child (transaction)
  "Create TRANSACTION's hidden child and its explicit logical clone."
  (let* ((old-save-path (plist-get transaction :old-save-path))
         (parent-directory
          (file-name-directory (directory-file-name old-save-path)))
         (staging-path
          (file-name-as-directory
           (make-temp-file
            (expand-file-name ".mevedel-save-as-" parent-directory) t)))
         (now (format-time-string "%FT%H-%M-%S"))
         (child
          (mevedel-session-fork-clone-session
           (plist-get transaction :session) 'save-as
           :save-path staging-path
           :session-id (plist-get transaction :new-id)
           :name (plist-get transaction :new-name)
           :created-at now
           :updated-at now
           :forked-from-session-id (plist-get transaction :old-id))))
    (plist-put transaction :staging-path staging-path)
    (plist-put transaction :child child)
    (plist-put transaction :stage 'staged)))

(defun mevedel-session-save-as--claim-child (transaction)
  "Claim TRANSACTION's child lease before writing staged bytes."
  (unless
      (mevedel-session-persistence-lock-acquire
       (plist-get transaction :staging-path)
       (buffer-name (plist-get transaction :buffer))
       (plist-get transaction :child))
    (error "Could not acquire Save As child lease"))
  (plist-put transaction :stage 'claimed))

(defun mevedel-session-save-as--materialize-child (transaction)
  "Materialize and sidecar-write TRANSACTION's child before publication."
  (let ((child (plist-get transaction :child))
        (buffer (plist-get transaction :buffer))
        (staging-path (plist-get transaction :staging-path)))
    (mevedel-session-rewind-materialize-publication
     (plist-get transaction :session)
     (plist-get transaction :parent-publication)
     staging-path)
    (mevedel-session-codec-write
     (mevedel-session-artifacts-sidecar-path staging-path)
     (mevedel-session-artifacts-build-sidecar child buffer))
    (plist-put transaction :stage 'materialized)))

(defun mevedel-session-save-as--publish-child (transaction)
  "Publish TRANSACTION's complete child and classify publication errors."
  (let ((child (plist-get transaction :child))
        (buffer (plist-get transaction :buffer))
        (staging-path (plist-get transaction :staging-path)))
    (condition-case error
        (progn
          (mevedel-session-publication-publish
           child
           (mevedel-session-rewind-rewind-publication-artifacts
            child buffer staging-path child))
          (plist-put transaction :stage 'published))
      (error
       (let ((publication
              (mevedel-session-save-as--read-publication staging-path))
             (committed-head
              (plist-get (mevedel-session-lease child) :publication-head)))
         (if (or publication committed-head)
             (progn
               (plist-put transaction :publication publication)
               (plist-put transaction :stage 'published)
               (mevedel-session-save-as--record-error transaction error))
           (signal (car error) (cdr error))))))))

(defun mevedel-session-save-as--move-child (transaction)
  "Move TRANSACTION's published child into its discoverable path."
  (condition-case error
      (progn
        (mevedel-session-durability-call-with-reserved-lease
         (plist-get transaction :child)
         (lambda ()
           (rename-file
            (directory-file-name (plist-get transaction :staging-path))
            (directory-file-name (plist-get transaction :new-save-path)))
           (setf (plist-get transaction :stage) 'moved
                 (mevedel-session-save-path (plist-get transaction :child))
                 (plist-get transaction :new-save-path)))))
    (error
     (mevedel-session-save-as--record-error transaction error)))
  transaction)

(defun mevedel-session-save-as--refresh-publication (transaction)
  "Refresh TRANSACTION's child publication after move or publication error."
  (when (mevedel-session-save-as--committed-p transaction)
    (let ((publication
           (or (mevedel-session-save-as--read-publication
                (if (eq (plist-get transaction :stage) 'moved)
                    (plist-get transaction :new-save-path)
                  (plist-get transaction :staging-path)))
               (plist-get transaction :publication))))
      (if publication
          (plist-put transaction :publication publication)
        (mevedel-session-save-as--record-error
         transaction
         (list 'error "Committed Save As child publication is unavailable"))))))

(defun mevedel-session-save-as--run-stages (transaction)
  "Run TRANSACTION's target stages under the reserved parent lease."
  (let ((session (plist-get transaction :session)))
    (mevedel-session-durability-call-with-reserved-lease
     session
     (lambda ()
       (setq transaction
             (mevedel-session-save-as--capture-parent transaction))
       (setq transaction
             (mevedel-session-save-as--stage-child transaction))
       (setq transaction
             (mevedel-session-save-as--claim-child transaction))
       (setq transaction
             (mevedel-session-save-as--materialize-child transaction))
       (setq transaction
             (mevedel-session-save-as--publish-child transaction))
       (when (mevedel-session-save-as--committed-p transaction)
         (setq transaction
               (mevedel-session-save-as--move-child transaction))
         (setq transaction
               (mevedel-session-save-as--refresh-publication transaction)))
       transaction))))


;;
;;; Adoption and cleanup

(defun mevedel-session-save-as--adopt-child (transaction)
  "Adopt TRANSACTION's committed child into its live session."
  (when (and (mevedel-session-save-as--committed-p transaction)
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
            (mevedel-session-durability-adopt-owned-lease session child)
            (setf (mevedel-session-session-id session)
                  (plist-get transaction :new-id)
                  (mevedel-session-name session)
                  (plist-get transaction :new-name)
                  (mevedel-session-forked-from-session-id session)
                  (plist-get transaction :old-id)
                  (mevedel-session-forked-from-turn session)
                  (mevedel-session-turn-count child)
                  (mevedel-session-updated-at session)
                  (mevedel-session-updated-at child)
                  (mevedel-session-publication session)
                  (plist-get transaction :publication))
            (with-current-buffer buffer
              (setq buffer-file-name
                    (and (plist-get transaction :old-buffer-file)
                         (file-name-concat
                          committed-path
                          (file-name-nondirectory
                           (plist-get transaction :old-buffer-file)))))
              (setq buffer-file-truename nil))
            (condition-case release-error
                (mevedel-session-persistence-lock-release
                 (plist-get transaction :old-save-path))
              (error
               (setq transaction
                     (mevedel-session-save-as--record-error
                      transaction release-error))))
            (mevedel-session-save-as--rename-live-session-buffers
             session buffer)
            (plist-put transaction :stage 'adopted))
        (error
         (mevedel-session-save-as--record-error transaction error))))
    transaction))

(defun mevedel-session-save-as--cleanup (transaction)
  "Release and remove TRANSACTION's uncommitted staging state."
  (unless (mevedel-session-save-as--committed-p transaction)
    (let ((child (plist-get transaction :child))
          (staging-path (plist-get transaction :staging-path)))
      (when (and child (memq (plist-get transaction :stage)
                             '(claimed materialized)))
        (condition-case error
            (mevedel-session-persistence-lock-release staging-path child)
          (error
           (setq transaction
                 (mevedel-session-save-as--record-error
                  transaction error)))))
      (when (and staging-path (file-directory-p staging-path))
        (condition-case error
            (delete-directory staging-path t)
          (error
           (setq transaction
                 (mevedel-session-save-as--record-error
                  transaction error)))))
      (when (and child staging-path
                 (not (file-directory-p staging-path)))
        (condition-case error
            (mevedel-session-durability-forget-removed-session child)
          (error
           (setq transaction
                 (mevedel-session-save-as--record-error
                  transaction error)))))))
  transaction)

(defun mevedel-session-save-as--finish (transaction)
  "Return TRANSACTION's result or signal its classified error."
  (let ((error (plist-get transaction :error)))
    (when error
      (if (mevedel-session-save-as--committed-p transaction)
          (error "Remote Save As committed a child, but finalization failed: %s"
                 (error-message-string error))
        (signal (car error) (cdr error))))
    (list :status 'committed
          :session (plist-get transaction :session)
          :save-path (mevedel-session-save-path
                      (plist-get transaction :session))
          :publication (plist-get transaction :publication))))

(defun mevedel-session-save-as-run
    (session buffer new-name new-id new-save-path)
  "Run portable Save As for SESSION and live root BUFFER.

NEW-NAME, NEW-ID, and NEW-SAVE-PATH identify the child.  Return a result plist
with `:status' `committed', the adopted live `:session', its `:save-path', and
the committed `:publication'.  Pre-commit errors are re-signaled after
staging cleanup.  Once the child marker commits, failures retain the child and
are signaled as finalization errors.

The caller must already have checked live mutation authority.  This module
rechecks the portable parent head while holding the reserved lease and uses
the existing lease/publication gates for every child write."
  (let ((transaction
         (mevedel-session-save-as--validate
          session buffer new-name new-id new-save-path)))
    (condition-case error
        (setq transaction
              (mevedel-session-save-as--run-stages transaction))
      (error
       (setq transaction
             (mevedel-session-save-as--record-error transaction error))))
    (if (mevedel-session-save-as--committed-p transaction)
        (setq transaction
              (mevedel-session-save-as--adopt-child transaction))
      (setq transaction
            (mevedel-session-save-as--cleanup transaction)))
    (mevedel-session-save-as--finish transaction)))

(defun mevedel-session-save-as--rename-live-session-buffers
    (session data-buffer)
  "Rename DATA-BUFFER and its view for SESSION's current name."
  (let* ((new-data-name
          (mevedel-session-buffer-name
           (mevedel-session-name session)
           (mevedel-session-workspace session)))
         (view-buffer
          (buffer-local-value 'mevedel--view-buffer data-buffer)))
    (with-current-buffer data-buffer
      (rename-buffer new-data-name t))
    (when (buffer-live-p view-buffer)
      (with-current-buffer view-buffer
        (rename-buffer
         (if (string-match "\\*$" new-data-name)
             (replace-match ":view*" t t new-data-name)
           (concat new-data-name ":view"))
         t)))))

(provide 'mevedel-session-save-as)

;;; mevedel-session-save-as.el ends here
