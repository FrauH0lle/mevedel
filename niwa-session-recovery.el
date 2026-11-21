;;; niwa-session-recovery.el -- Portable specialized recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Owns target-side recovery markers and the manual-recovery state that blocks a portable session after an incomplete specialized transaction.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-structs))

;; Recovery always operates through durability's control filesystem and
;; refreshes the registered session root after mutation.
(require 'niwa-session-control-fs)
(require 'niwa-session-control-transfer)
(require 'niwa-session-durability)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-create-file
                  "niwa-session-control-fs"
                  (path content &optional coding-system))
(declare-function niwa-session-control-fs-delete-directory
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-delete-file
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-directory-p
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-list-directory
                  "niwa-session-control-fs" (directory regexp))
(declare-function niwa-session-control-fs-make-directory
                  "niwa-session-control-fs" (path &optional parents))
(declare-function niwa-session-control-fs-path-exists-p
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-physical-path
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-run-program
                  "niwa-session-control-fs" (operations))
(declare-function niwa-session-control-fs-write-file
                  "niwa-session-control-fs"
                  (path content &optional coding-system))

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer-notify
                  "niwa-session-control-transfer"
                  (session event &rest args))
(declare-function niwa-session-control-transfer-root-buffer
                  "niwa-session-control-transfer" (session))

;; `niwa-session-durability'
(declare-function niwa-session-durability--clock-operation
                  "niwa-session-durability" (directory))
(declare-function niwa-session-durability--lease-path
                  "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability--pid-lock-operation
                  "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability--portable-session-p
                  "niwa-session-durability" (session))
(declare-function niwa-session-durability--read-plist
                  "niwa-session-durability" (path))
(declare-function niwa-session-durability--settle-clock
                  "niwa-session-durability" (directory result))
(declare-function niwa-session-durability--settle-pid-lock
                  "niwa-session-durability" (session-dir result))
(declare-function niwa-session-durability--valid-relative-path-p
                  "niwa-session-durability" (path))
(declare-function niwa-session-durability--write-plist
                  "niwa-session-durability" (path plist))
(declare-function niwa-session-durability-call-with-reserved-lease
                  "niwa-session-durability" (session function))
(declare-function niwa-session-durability-lease-acquire
                  "niwa-session-durability"
                  (session-dir buffer-name &optional session))
(declare-function niwa-session-durability-lease-owned-p
                  "niwa-session-durability" (session))
(defvar niwa-session-durability--client-id)
(defvar niwa-session-recovery--mutation-cache nil
  "Cons cell collecting sessions whose marker was read this mutation, or nil.

One durable save consults the target recovery marker at the mutation gate and
again when publication begins.  Nothing between those two points can install a
marker, so the second read is a target round trip for an answer already held.
A caller that spans one mutation binds this to a fresh `(list nil)'; outside
such a binding every call reads the target.")

;; `niwa-structs'
(declare-function niwa-session-pending-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)


;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

(defun niwa-session-recovery-refresh-session-buffers (session)
  "Refresh SESSION's registered root and notify visible projections.

The coordinator owns the root registration and semantic observer boundary;
recovery does not discover session buffers by scanning Emacs global state."
  (when-let* ((root
             (niwa-session-control-transfer-root-buffer session)))
    (with-current-buffer root
      (when (and buffer-file-name (file-exists-p buffer-file-name))
        (setq buffer-file-truename (file-truename buffer-file-name))
        (set-visited-file-modtime))
      (when (boundp 'niwa-session--save-failed)
        (setq niwa-session--save-failed nil))
      (force-mode-line-update)))
  (niwa-session-control-transfer-notify session 'refresh-status)
  nil)


(defun niwa-session-recovery--root (session-dir)
  "Return SESSION-DIR's target-side specialized recovery root."
  (niwa-session-control-fs-physical-path session-dir)
  (let ((root
         (file-name-as-directory
          (file-name-concat (file-name-as-directory session-dir)
                            ".recovery"))))
    (setq root (niwa-session-control-fs-physical-path root))
    root))

(defun niwa-session-recovery--valid-directory-p (path)
  "Return non-nil when PATH is a safe session-relative recovery directory."
  (and (niwa-session-durability--valid-relative-path-p path)
       (string-prefix-p ".recovery/" path)
       (not (string-suffix-p "/" path))))

(defun niwa-session-recovery--marker-name-p (path)
  "Return non-nil when PATH is shaped like a recovery marker file name."
  (string-match-p "\\`recovery-[0-9a-f]+\\.el\\'"
                  (file-name-nondirectory path)))

(defun niwa-session-recovery--read-marker
    (session-dir marker-path)
  "Read and validate target recovery MARKER-PATH for SESSION-DIR.
An abandonment marker may outlive its explicitly discarded directory."
  (setq marker-path
        (niwa-session-control-fs-physical-path marker-path))
  ;; The name is validated before anything is read: a junk entry must
  ;; not cost a target round trip, and the listing that produced the
  ;; name already proved the entry exists and is no symlink.
  (unless (niwa-session-recovery--marker-name-p marker-path)
    (error "Invalid specialized recovery marker: %s" marker-path))
  (let* ((root (niwa-session-recovery--root session-dir))
         (marker (niwa-session-durability--read-plist marker-path))
         (abandoning (plist-get marker :abandoning))
         (relative (and marker (plist-get marker :directory)))
         (directory (and relative
                         (expand-file-name relative session-dir))))
    (unless (and (proper-list-p marker)
                 (equal (plist-get marker :version) 1)
                 (eq (plist-get marker :kind) 'rewind)
                 (memq abandoning '(nil t))
                 (stringp (plist-get marker :reason))
                 (niwa-session-recovery--valid-directory-p
                  relative)
                 (or (niwa-session-control-fs-directory-p directory)
                     (and abandoning
                          (not (niwa-session-control-fs-path-exists-p directory))))
                 (file-in-directory-p directory root))
      (error "Invalid specialized recovery marker: %s" marker-path))
    (list :marker marker-path
          :directory directory
          :relative-directory relative
          :abandoning abandoning
          :reason (if abandoning
                      (format "Recovery abandonment is incomplete: %s"
                              (plist-get marker :reason))
                    (plist-get marker :reason))
          :kind (plist-get marker :kind)
          :created-at (plist-get marker :created-at))))

(defun niwa-session-recovery--markers
    (session-dir &optional preamble root-observation)
  "Return validated specialized recovery markers below SESSION-DIR.

One directory operation answers existence and kind together, so the
common no-recovery case costs one target process instead of two.  With
PREAMBLE, the same process also proves SESSION-DIR has no PID lock and,
inside a durable transaction, reads the target clock the lease checks that
follow will need.  ROOT-OBSERVATION is an optional directory probe result
from the caller's admission program, which already carries that preamble."
  (let* ((root (niwa-session-control-fs-physical-path
                (niwa-session-recovery--root session-dir)))
         (lease (and preamble
                     (niwa-session-durability--lease-path session-dir)))
         (pid-lock (and preamble
                        (niwa-session-durability--pid-lock-operation
                         session-dir)))
         (clock (and preamble
                     (niwa-session-durability--clock-operation lease)))
         (results (unless root-observation
                    (niwa-session-control-fs-run-program
                     (append (and pid-lock (list pid-lock))
                             (list (list :op 'directory-p :path root
                                         :optional t))
                             (and clock (list clock))))))
         (result (or root-observation (nth (if pid-lock 1 0) results))))
    (when (and pid-lock (not root-observation))
      (niwa-session-durability--settle-pid-lock session-dir (car results)))
    (when (and clock (not root-observation))
      (niwa-session-durability--settle-clock lease (car (last results))))
    (pcase (plist-get result :status)
      ('absent nil)
      ('ok
       (mapcar
        (lambda (path)
          (niwa-session-recovery--read-marker
           session-dir path))
        (niwa-session-control-fs-list-directory
         root "\\`recovery-[0-9a-f]+\\.el\\'")))
      (_ (error "Invalid specialized recovery root: %s" root)))))

(defun niwa-session-recovery-read (session-dir &optional root-observation)
  "Return the first target-side specialized recovery for SESSION-DIR.

The result is a plist containing `:marker', `:directory', `:reason', and
`:created-at', or nil when no durable recovery marker is installed.
ROOT-OBSERVATION is the recovery-root probe from an admission program."
  (let ((remote-file-name-inhibit-cache t))
    (car (niwa-session-recovery--markers session-dir t root-observation))))

(defun niwa-session-recovery--id (session reason)
  "Return a fresh target-side recovery id for SESSION and REASON."
  (substring
   (secure-hash
    'sha256
    (format "%S"
            (list (current-time) (random most-positive-fixnum)
                  niwa-session-durability--client-id session reason)))
   0 32))

(defun niwa-session-recovery--authority-p (session)
  "Ensure SESSION owns its portable lease for target recovery.

Return non-nil only when the live client owns the lease after acquisition.
Recovery installation is deliberately refused when another client still owns
the target, leaving the local recovery directory available for a later retry."
  (or (niwa-session-durability-lease-owned-p session)
      (when-let* ((session-dir (niwa-session-save-path session)))
        (and
         (niwa-session-durability-lease-acquire
          session-dir (buffer-name (current-buffer)) session)
         (niwa-session-durability-lease-owned-p session)))))

(defun niwa-session-recovery--local-temporary-p (path)
  "Return non-nil when PATH resolves to a directory strictly inside the temp dir.
Resolved names are compared as strings: `file-in-directory-p' compares two
stats of the shared temporary directory, which concurrent activity there
makes differ."
  (when (and (stringp path) (file-directory-p path))
    (let ((temporary (file-name-as-directory
                      (file-truename temporary-file-directory)))
          (true (file-name-as-directory (file-truename path))))
      (and (string-prefix-p temporary true)
           (not (equal temporary true))))))

(defun niwa-session-recovery--copy-local-directory
    (source destination)
  "Copy local recovery SOURCE into target DESTINATION through control FS."
  (when (file-symlink-p source)
    (error "Specialized recovery source contains a symbolic link: %s" source))
  (unless (niwa-session-control-fs-make-directory destination)
    (error "Specialized recovery target already exists: %s" destination))
  (dolist (entry (directory-files source t nil t))
    (unless (member (file-name-nondirectory entry) '("." ".."))
      (when (file-symlink-p entry)
        (error "Specialized recovery source contains a symbolic link: %s"
               entry))
      (let ((target (file-name-concat destination
                                      (file-name-nondirectory entry))))
        (if (file-directory-p entry)
            (niwa-session-recovery--copy-local-directory entry target)
          (with-temp-buffer
            (insert-file-contents-literally entry)
            (unless (niwa-session-control-fs-create-file
                     target (buffer-string) 'no-conversion)
              (error "Specialized recovery target already exists: %s"
                     target))))))))

(defun niwa-session-recovery--install
    (session reason recovery-path)
  "Copy RECOVERY-PATH to SESSION's target and install its marker last.

Return the validated recovery descriptor.  RECOVERY-PATH remains untouched
until the marker has been written successfully; callers may remove it only
after this function returns."
  (let* ((session-dir (niwa-session-save-path session))
         (root (and session-dir
                    (niwa-session-recovery--root session-dir)))
         (id (and root
                  (niwa-session-recovery--id session reason)))
         (relative (and id (file-name-concat ".recovery" id)))
         (directory (and root (file-name-concat root id)))
         (marker (and root
                      (file-name-concat root (concat "recovery-" id ".el"))))
         (created-at (format-time-string "%FT%T%z")))
    (unless (and session-dir
                 (niwa-session-recovery--local-temporary-p recovery-path))
      (error "Specialized recovery source is not a local temporary directory: %s"
             recovery-path))
    (niwa-session-control-fs-physical-path root)
    (niwa-session-control-fs-physical-path directory)
    (niwa-session-control-fs-physical-path marker)
    (niwa-session-control-fs-make-directory root t)
    (niwa-session-control-fs-physical-path root)
    (condition-case err
        (progn
          (niwa-session-recovery--copy-local-directory
           recovery-path directory)
          (niwa-session-control-fs-physical-path directory)
          (unless (niwa-session-control-fs-directory-p directory)
            (error "Target specialized recovery directory was not created: %s"
                   directory))
          (niwa-session-durability--write-plist
           marker
           (list :version 1
                 :kind 'rewind
                 :directory relative
                 :reason reason
                 :created-at created-at))
          (niwa-session-recovery--read-marker
           session-dir marker))
      (error
       ;; Remove both halves after any copy, write, or readback failure so a
       ;; later client cannot discover a false recovery record.
       (condition-case nil
           (when (niwa-session-control-fs-path-exists-p marker)
             (niwa-session-control-fs-delete-file marker))
         (error nil))
       (condition-case nil
           (when (niwa-session-control-fs-directory-p directory)
             (niwa-session-control-fs-delete-directory directory))
         (error nil))
       (signal (car err) (cdr err))))))

(defun niwa-session-recovery--delete-local (path)
  "Delete generated local recovery PATH when it is safely temporary."
  (when (niwa-session-recovery--local-temporary-p path)
    (delete-directory path t)))

(defun niwa-session-recovery-refresh (session &optional root-observation)
  "Refresh SESSION's pending state from its target recovery marker.

The marker is authoritative across client loss.  Existing local recovery
state is preserved while the target marker is attached to the pending record,
so a later explicit abandonment can remove both sources safely.
ROOT-OBSERVATION is the recovery-root probe from this admission's program."
  (when (and (niwa-session-durability--portable-session-p session)
             (niwa-session-save-path session)
             (not (memq session
                        (car niwa-session-recovery--mutation-cache))))
    (when niwa-session-recovery--mutation-cache
      (push session (car niwa-session-recovery--mutation-cache)))
    (when-let* ((recovery
                (niwa-session-recovery-read
                 (niwa-session-save-path session) root-observation)))
      (let ((pending
             (copy-sequence
              (or (niwa-session-pending-publication session)
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
        (setf (niwa-session-pending-publication session) pending))))
  (niwa-session-pending-publication session))

(defun niwa-session-recovery--abandon (session)
  "Delete SESSION's target recovery bytes and markers under its lease."
  (unless (niwa-session-durability--portable-session-p session)
    (error "Target specialized recovery requires a portable project session"))
  (unless (niwa-session-durability-lease-owned-p session)
    (user-error "Portable specialized recovery requires its live session lease"))
  (niwa-session-durability-call-with-reserved-lease
   session
   (lambda ()
     (let ((session-dir (niwa-session-save-path session)))
       (dolist (recovery
                (niwa-session-recovery--markers session-dir))
         ;; Persist the approved deletion before touching bytes.  A surviving
         ;; marker remains actionable even if its directory is already gone.
         (unless (plist-get recovery :abandoning)
           (niwa-session-durability--write-plist
            (plist-get recovery :marker)
            (list :version 1 :kind (plist-get recovery :kind)
                  :directory (plist-get recovery :relative-directory)
                  :reason (plist-get recovery :reason)
                  :created-at (plist-get recovery :created-at)
                  :abandoning t)))
         (niwa-session-control-fs-delete-directory
          (plist-get recovery :directory))
         (when (niwa-session-control-fs-directory-p
                (plist-get recovery :directory))
           (error "Could not remove specialized recovery bytes: %s"
                  (plist-get recovery :directory)))
         (niwa-session-control-fs-delete-file
          (plist-get recovery :marker)))
       (let ((root (niwa-session-recovery--root session-dir)))
         (when (niwa-session-control-fs-directory-p root)
           (niwa-session-control-fs-program-value
            (car (niwa-session-control-fs-run-program
                  (list (list :op 'delete-empty-directory :path root)))))))
       t))))

(defun niwa-session-recovery-record-failure
    (session reason recovery-path)
  "Block SESSION after a specialized transaction failed incompletely.

REASON describes the inconsistent state.  RECOVERY-PATH names the local
transaction directory.  Portable project sessions first copy it to target-side
recovery storage and install a marker; the local directory is retained when
that installation fails."
  (let* ((portable-p
          (niwa-session-durability--portable-session-p session))
         target-recovery target-error)
    (when portable-p
      (setq target-recovery
            (condition-case err
                (and (niwa-session-save-path session)
                     (if (niwa-session-recovery--authority-p
                          session)
                         (niwa-session-durability-call-with-reserved-lease
                          session
                          (lambda ()
                            (niwa-session-recovery--install
                             session reason recovery-path)))
                       (error "Portable session lease is unavailable for target recovery")))
              (error
               (setq target-error (error-message-string err))
               nil))))
    (if target-recovery
        (progn
          (condition-case err
              (niwa-session-recovery--delete-local
               recovery-path)
            (error
             (niwa-diagnostic-warning (niwa-diagnostic-scope session)
              'niwa
              (format "Target recovery installed, but local cleanup failed: %s"
                      (error-message-string err))
              :warning)))
          (setf (niwa-session-pending-publication session)
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
        (setf (niwa-session-pending-publication session)
              (list :batches nil
                    :failed-at (format-time-string "%FT%T%z")
                    :reason failure-reason
                    :manual-recovery recovery-path
                    :manual-recovery-local
                    (and (niwa-session-recovery--local-temporary-p
                          recovery-path)
                         recovery-path)
                    :recovery-portable nil
                    :recovery-kind 'rewind))))
    (niwa-session-recovery-refresh-session-buffers session)
    (niwa-diagnostic-warning (niwa-diagnostic-scope session)
     'niwa
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
        (plist-get (niwa-session-pending-publication session) :reason)
        recovery-path))
      (t
       (format
        (concat "%s. Automatic retry is unavailable; inspect local recovery "
                "data at %s, then explicitly abandon the block when repaired.")
        reason recovery-path)))
     :error)))

(provide 'niwa-session-recovery)

;;; niwa-session-recovery.el ends here
