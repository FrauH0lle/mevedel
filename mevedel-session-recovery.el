;;; mevedel-session-recovery.el -- Portable specialized recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Owns target-side recovery markers and the manual-recovery state that blocks a portable session after an incomplete specialized transaction.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; Every control operation in this file runs through the session control
;; filesystem, so its feature is a hard load-time dependency rather than a
;; lazily reachable one.
(require 'mevedel-session-control-fs)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer-notify
                  "mevedel-session-control-transfer"
                  (session event &rest args))
(declare-function mevedel-session-control-transfer-root-buffer
                  "mevedel-session-control-transfer" (session))

;; `mevedel-session-durability'
(declare-function mevedel-session-control-fs-create-file
                  "mevedel-session-durability"
                  (path content &optional coding-system))
(declare-function mevedel-session-control-fs-delete-directory
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-control-fs-delete-file
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-control-fs-directory-p
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-control-fs-list-directory
                  "mevedel-session-durability" (directory regexp))
(declare-function mevedel-session-control-fs-make-directory
                  "mevedel-session-durability" (path &optional parents))
(declare-function mevedel-session-control-fs-path-exists-p
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-control-fs-run-program
                  "mevedel-session-durability" (operations))
(declare-function mevedel-session-control-fs-write-file
                  "mevedel-session-durability"
                  (path content &optional coding-system))
(declare-function mevedel-session-durability--assert-no-pid-lock
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability--portable-session-p
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability--read-plist
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-durability--valid-relative-path-p
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-durability--write-plist
                  "mevedel-session-durability" (path plist))
(declare-function mevedel-session-durability-call-with-reserved-lease
                  "mevedel-session-durability" (session function))
(declare-function mevedel-session-durability-lease-acquire
                  "mevedel-session-durability"
                  (session-dir buffer-name &optional session))
(declare-function mevedel-session-durability-lease-owned-p
                  "mevedel-session-durability" (session))
(defvar mevedel-session-durability--client-id)
(defvar mevedel-session-recovery--mutation-cache nil
  "Cons cell collecting sessions whose marker was read this mutation, or nil.

One durable save consults the target recovery marker at the mutation gate and
again when publication begins.  Nothing between those two points can install a
marker, so the second read is a target round trip for an answer already held.
A caller that spans one mutation binds this to a fresh `(list nil)'; outside
such a binding every call reads the target.")

;; `mevedel-structs'
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)


(defun mevedel-session-recovery-refresh-session-buffers (session)
  "Refresh SESSION's registered root and notify visible projections.

The coordinator owns the root registration and semantic observer boundary;
recovery does not discover session buffers by scanning Emacs global state."
  (when-let ((root
             (mevedel-session-control-transfer-root-buffer session)))
    (with-current-buffer root
      (when (and buffer-file-name (file-exists-p buffer-file-name))
        (setq buffer-file-truename (file-truename buffer-file-name))
        (set-visited-file-modtime))
      (when (boundp 'mevedel-session--save-failed)
        (setq mevedel-session--save-failed nil))
      (force-mode-line-update)))
  (mevedel-session-control-transfer-notify session 'refresh-status)
  nil)


(defun mevedel-session-recovery--root (session-dir)
  "Return SESSION-DIR's target-side specialized recovery root."
  (mevedel-session-control-fs-physical-path session-dir)
  (let ((root
         (file-name-as-directory
          (file-name-concat (file-name-as-directory session-dir)
                            ".recovery"))))
    (setq root (mevedel-session-control-fs-physical-path root))
    root))

(defun mevedel-session-recovery--valid-directory-p (path)
  "Return non-nil when PATH is a safe session-relative recovery directory."
  (and (mevedel-session-durability--valid-relative-path-p path)
       (string-prefix-p ".recovery/" path)
       (not (string-suffix-p "/" path))))

(defun mevedel-session-recovery--marker-name-p (path)
  "Return non-nil when PATH is shaped like a recovery marker file name."
  (string-match-p "\\`recovery-[0-9a-f]+\\.el\\'"
                  (file-name-nondirectory path)))

(defun mevedel-session-recovery--read-marker
    (session-dir marker-path)
  "Read and validate target recovery MARKER-PATH for SESSION-DIR."
  (setq marker-path
        (mevedel-session-control-fs-physical-path marker-path))
  ;; The name is validated before anything is read: a junk entry must
  ;; not cost a target round trip, and the listing that produced the
  ;; name already proved the entry exists and is no symlink.
  (unless (mevedel-session-recovery--marker-name-p marker-path)
    (error "Invalid specialized recovery marker: %s" marker-path))
  (let* ((root (mevedel-session-recovery--root session-dir))
         (marker (mevedel-session-durability--read-plist marker-path))
         (relative (and marker (plist-get marker :directory)))
         (directory (and relative
                         (expand-file-name relative session-dir))))
    (unless (and (proper-list-p marker)
                 (equal (plist-get marker :version) 1)
                 (eq (plist-get marker :kind) 'rewind)
                 (stringp (plist-get marker :reason))
                 (mevedel-session-recovery--valid-directory-p
                  relative)
                 (mevedel-session-control-fs-directory-p directory)
                 (file-in-directory-p directory root))
      (error "Invalid specialized recovery marker: %s" marker-path))
    (list :marker marker-path
          :directory directory
          :relative-directory relative
          :reason (plist-get marker :reason)
          :kind (plist-get marker :kind)
          :created-at (plist-get marker :created-at))))

(defun mevedel-session-recovery--markers (session-dir)
  "Return validated specialized recovery markers below SESSION-DIR.

One directory operation answers existence and kind together, so the
common no-recovery case costs one target process instead of two."
  (let* ((root (mevedel-session-control-fs-physical-path
                (mevedel-session-recovery--root session-dir)))
         (result (car (mevedel-session-control-fs-run-program
                       (list (list :op 'directory-p :path root
                                   :optional t))))))
    (pcase (plist-get result :status)
      ('absent nil)
      ('ok
       (mapcar
        (lambda (path)
          (mevedel-session-recovery--read-marker
           session-dir path))
        (mevedel-session-control-fs-list-directory
         root "\\`recovery-[0-9a-f]+\\.el\\'")))
      (_ (error "Invalid specialized recovery root: %s" root)))))

(defun mevedel-session-recovery-read (session-dir)
  "Return the first target-side specialized recovery for SESSION-DIR.

The result is a plist containing `:marker', `:directory', `:reason', and
`:created-at', or nil when no durable recovery marker is installed."
  (let ((remote-file-name-inhibit-cache t))
    (mevedel-session-durability--assert-no-pid-lock session-dir)
    (car (mevedel-session-recovery--markers session-dir))))

(defun mevedel-session-recovery--id (session reason)
  "Return a fresh target-side recovery id for SESSION and REASON."
  (substring
   (secure-hash
    'sha256
    (format "%S"
            (list (current-time) (random most-positive-fixnum)
                  mevedel-session-durability--client-id session reason)))
   0 32))

(defun mevedel-session-recovery--authority-p (session)
  "Ensure SESSION owns its portable lease for target recovery.

Return non-nil only when the live client owns the lease after acquisition.
Recovery installation is deliberately refused when another client still owns
the target, leaving the local recovery directory available for a later retry."
  (or (mevedel-session-durability-lease-owned-p session)
      (when-let ((session-dir (mevedel-session-save-path session)))
        (and
         (mevedel-session-durability-lease-acquire
          session-dir (buffer-name (current-buffer)) session)
         (mevedel-session-durability-lease-owned-p session)))))

(defun mevedel-session-recovery--copy-local-directory
    (source destination)
  "Copy local recovery SOURCE into target DESTINATION through control FS."
  (when (file-symlink-p source)
    (error "Specialized recovery source contains a symbolic link: %s" source))
  (unless (mevedel-session-control-fs-make-directory destination)
    (error "Specialized recovery target already exists: %s" destination))
  (dolist (entry (directory-files source t nil t))
    (unless (member (file-name-nondirectory entry) '("." ".."))
      (when (file-symlink-p entry)
        (error "Specialized recovery source contains a symbolic link: %s"
               entry))
      (let ((target (file-name-concat destination
                                      (file-name-nondirectory entry))))
        (if (file-directory-p entry)
            (mevedel-session-recovery--copy-local-directory entry target)
          (with-temp-buffer
            (insert-file-contents-literally entry)
            (unless (mevedel-session-control-fs-create-file
                     target (buffer-string) 'no-conversion)
              (error "Specialized recovery target already exists: %s"
                     target))))))))

(defun mevedel-session-recovery--install
    (session reason recovery-path)
  "Copy RECOVERY-PATH to SESSION's target and install its marker last.

Return the validated recovery descriptor.  RECOVERY-PATH remains untouched
until the marker has been written successfully; callers may remove it only
after this function returns."
  (let* ((session-dir (mevedel-session-save-path session))
         (root (and session-dir
                    (mevedel-session-recovery--root session-dir)))
         (id (and root
                  (mevedel-session-recovery--id session reason)))
         (relative (and id (file-name-concat ".recovery" id)))
         (directory (and root (file-name-concat root id)))
         (marker (and root
                      (file-name-concat root (concat "recovery-" id ".el"))))
         (created-at (format-time-string "%FT%T%z")))
    (unless (and session-dir
                 (file-directory-p recovery-path)
                 (file-in-directory-p recovery-path temporary-file-directory))
      (error "Specialized recovery source is not a local temporary directory: %s"
             recovery-path))
    (mevedel-session-control-fs-physical-path root)
    (mevedel-session-control-fs-physical-path directory)
    (mevedel-session-control-fs-physical-path marker)
    (mevedel-session-control-fs-make-directory root t)
    (mevedel-session-control-fs-physical-path root)
    (condition-case err
        (progn
          (mevedel-session-recovery--copy-local-directory
           recovery-path directory)
          (mevedel-session-control-fs-physical-path directory)
          (unless (mevedel-session-control-fs-directory-p directory)
            (error "Target specialized recovery directory was not created: %s"
                   directory))
          (mevedel-session-durability--write-plist
           marker
           (list :version 1
                 :kind 'rewind
                 :directory relative
                 :reason reason
                 :created-at created-at))
          (mevedel-session-recovery--read-marker
           session-dir marker))
      (error
       ;; Remove both halves after any copy, write, or readback failure so a
       ;; later client cannot discover a false recovery record.
       (condition-case nil
           (when (mevedel-session-control-fs-path-exists-p marker)
             (mevedel-session-control-fs-delete-file marker))
         (error nil))
       (condition-case nil
           (when (mevedel-session-control-fs-directory-p directory)
             (mevedel-session-control-fs-delete-directory directory))
         (error nil))
       (signal (car err) (cdr err))))))

(defun mevedel-session-recovery--delete-local (path)
  "Delete generated local recovery PATH when it is safely temporary."
  (when (and (stringp path)
             (file-directory-p path)
             (file-in-directory-p path temporary-file-directory)
             (not (equal (file-name-as-directory (expand-file-name path))
                         (file-name-as-directory
                          (expand-file-name temporary-file-directory)))))
    (delete-directory path t)))

(defun mevedel-session-recovery-refresh (session)
  "Refresh SESSION's pending state from its target recovery marker.

The marker is authoritative across client loss.  Existing local recovery
state is preserved while the target marker is attached to the pending record,
so a later explicit abandonment can remove both sources safely."
  (when (and (mevedel-session-durability--portable-session-p session)
             (mevedel-session-save-path session)
             (not (memq session
                        (car mevedel-session-recovery--mutation-cache))))
    (when mevedel-session-recovery--mutation-cache
      (push session (car mevedel-session-recovery--mutation-cache)))
    (when-let ((recovery
                (mevedel-session-recovery-read
                 (mevedel-session-save-path session))))
      (let ((pending
             (copy-sequence
              (or (mevedel-session-pending-publication session)
                  '(:batches nil)))))
        (setf (plist-get pending :failed-at)
              (or (plist-get recovery :created-at)
                  (format-time-string "%FT%T%z"))
              (plist-get pending :reason) (plist-get recovery :reason)
              (plist-get pending :manual-recovery)
              (plist-get recovery :directory)
              (plist-get pending :manual-recovery-marker)
              (plist-get recovery :marker)
              (plist-get pending :recovery-portable) t
              (plist-get pending :recovery-kind)
              (plist-get recovery :kind))
        (setf (mevedel-session-pending-publication session) pending))))
  (mevedel-session-pending-publication session))

(defun mevedel-session-recovery--abandon (session)
  "Delete SESSION's target recovery bytes and markers under its lease."
  (unless (mevedel-session-durability--portable-session-p session)
    (error "Target specialized recovery requires a portable project session"))
  (unless (mevedel-session-durability-lease-owned-p session)
    (user-error "Portable specialized recovery requires its live session lease"))
  (mevedel-session-durability-call-with-reserved-lease
   session
   (lambda ()
     (let ((session-dir (mevedel-session-save-path session)))
       (dolist (recovery
                (mevedel-session-recovery--markers session-dir))
         (mevedel-session-control-fs-delete-directory
          (plist-get recovery :directory))
         (when (mevedel-session-control-fs-directory-p
                (plist-get recovery :directory))
           (error "Could not remove specialized recovery bytes: %s"
                  (plist-get recovery :directory)))
         (mevedel-session-control-fs-delete-file
          (plist-get recovery :marker)))
       (let ((root (mevedel-session-recovery--root session-dir)))
         (when (and (mevedel-session-control-fs-directory-p root)
                    (null (mevedel-session-control-fs-list-directory
                           root "\\`[^.]")))
           (mevedel-session-control-fs-delete-directory root)))
       t))))

(defun mevedel-session-recovery-record-failure
    (session reason recovery-path)
  "Block SESSION after a specialized transaction failed incompletely.

REASON describes the inconsistent state.  RECOVERY-PATH names the local
transaction directory.  Portable project sessions first copy it to target-side
recovery storage and install a marker; the local directory is retained when
that installation fails."
  (let* ((portable-p
          (mevedel-session-durability--portable-session-p session))
         target-recovery target-error)
    (when portable-p
      (setq target-recovery
            (condition-case err
                (and (mevedel-session-save-path session)
                     (if (mevedel-session-recovery--authority-p
                          session)
                         (mevedel-session-durability-call-with-reserved-lease
                          session
                          (lambda ()
                            (mevedel-session-recovery--install
                             session reason recovery-path)))
                       (error "Portable session lease is unavailable for target recovery")))
              (error
               (setq target-error (error-message-string err))
               nil))))
    (if target-recovery
        (progn
          (condition-case err
              (mevedel-session-recovery--delete-local
               recovery-path)
            (error
             (display-warning
              'mevedel
              (format "Target recovery installed, but local cleanup failed: %s"
                      (error-message-string err))
              :warning)))
          (setf (mevedel-session-pending-publication session)
                (list :batches nil
                      :failed-at (or (plist-get target-recovery :created-at)
                                     (format-time-string "%FT%T%z"))
                      :reason reason
                      :manual-recovery
                      (plist-get target-recovery :directory)
                      :manual-recovery-marker
                      (plist-get target-recovery :marker)
                      :recovery-portable t
                      :recovery-kind (plist-get target-recovery :kind))))
      (let ((failure-reason
             (if target-error
                 (format
                  "%s; target-side recovery install failed: %s; local recovery retained at %s"
                  reason target-error recovery-path)
               reason)))
        (setf (mevedel-session-pending-publication session)
              (list :batches nil
                    :failed-at (format-time-string "%FT%T%z")
                    :reason failure-reason
                    :manual-recovery recovery-path
                    :manual-recovery-local
                    (and (stringp recovery-path)
                         (file-directory-p recovery-path)
                         (file-in-directory-p
                          recovery-path temporary-file-directory)
                         (not (equal
                               (file-name-as-directory
                                (expand-file-name recovery-path))
                               (file-name-as-directory
                                (expand-file-name temporary-file-directory))))
                         recovery-path)
                    :recovery-portable nil
                    :recovery-kind 'rewind))))
    (mevedel-session-recovery-refresh-session-buffers session)
    (display-warning
     'mevedel
     (cond
      (target-recovery
       (format
        (concat "%s. Automatic retry is unavailable; inspect target recovery "
                "data at %s, then explicitly abandon the block when repaired.")
        reason (plist-get target-recovery :directory)))
      (target-error
       (format
        (concat "%s. Automatic retry is unavailable; target recovery could not "
                "be installed. Inspect local recovery data at %s, then "
                "explicitly abandon the block when repaired.")
        (plist-get (mevedel-session-pending-publication session) :reason)
        recovery-path))
      (t
       (format
        (concat "%s. Automatic retry is unavailable; inspect local recovery "
                "data at %s, then explicitly abandon the block when repaired.")
        reason recovery-path)))
     :error)))

(provide 'mevedel-session-recovery)

;;; mevedel-session-recovery.el ends here
