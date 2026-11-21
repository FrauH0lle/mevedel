;;; niwa-workspace-identity.el -- Durable workspace identity -*- lexical-binding: t -*-

;;; Commentary:

;; Stores one opaque identity with the workspace so persisted sessions can be
;; rebound independently of a client's local or TRAMP path spelling.

;;; Code:

(defun niwa-workspace-identity-client ()
  "Return a restart-stable opaque identity for this client's host and user."
  (secure-hash 'sha256 (format "%s\0%s" (system-name) (user-login-name))))

(defconst niwa-workspace-identity--regexp
  "\\`[0-9a-f]\\{64\\}\n\\'"
  "Exact on-disk format of a workspace identity.")

(defun niwa-workspace-identity--path (root)
  "Return the durable workspace identity path below ROOT."
  (unless (and (stringp root) (file-name-absolute-p root))
    (error "Workspace root must be an absolute path: %S" root))
  (file-name-concat (file-name-as-directory (expand-file-name root))
                    ".niwa" "workspace-id"))

(defvar niwa-workspace-identity--cache (make-hash-table :test #'equal)
  "Durable identities keyed by identity file path.

An identity never changes once created -- the first atomic creator
wins and nothing rewrites it -- so one successful read is
authoritative for the rest of the process.  On a remote workspace the
uncached read is a full TRAMP round trip on every save.")

(defun niwa-workspace-identity-read (root)
  "Return ROOT's durable workspace identity, or nil when none exists.

Signal an error when the identity file exists but is malformed.  A
successful read is cached for the process; an absent identity is not,
so a later creator is still observed."
  (let ((path (niwa-workspace-identity--path root)))
    (or (gethash path niwa-workspace-identity--cache)
        (when (file-exists-p path)
          (let ((contents
                 (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string))))
            (unless (string-match-p niwa-workspace-identity--regexp
                                    contents)
              (error "Invalid workspace identity at %s" path))
            (puthash path (substring contents 0 -1)
                     niwa-workspace-identity--cache))))))

(defun niwa-workspace-identity-ensure (root)
  "Return ROOT's durable workspace identity, creating it when absent.

The first atomic creator wins when multiple clients initialize ROOT at once."
  (or (niwa-workspace-identity-read root)
      (let* ((path (niwa-workspace-identity--path root))
             (directory (file-name-directory path))
             (identity
              (secure-hash
               'sha256
               (format "%S"
                       (list (current-time) (random most-positive-fixnum)
                             (emacs-pid) (system-name))))))
        (make-directory directory t)
        (or (niwa-workspace-identity-read root)
            (let ((temporary
                   (let ((default-directory directory))
                     (make-nearby-temp-file
                      (expand-file-name
                       ".niwa-workspace-id-" directory)))))
              (unwind-protect
                  (progn
                    (with-temp-file temporary
                      (insert identity "\n"))
                    (condition-case nil
                        (progn
                          (add-name-to-file temporary path nil)
                          identity)
                      (file-already-exists
                       (niwa-workspace-identity-read root))))
                (when (file-exists-p temporary)
                  (delete-file temporary))))))))

(provide 'niwa-workspace-identity)

;;; niwa-workspace-identity.el ends here
