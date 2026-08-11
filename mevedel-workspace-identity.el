;;; mevedel-workspace-identity.el -- Durable workspace identity -*- lexical-binding: t -*-

;;; Commentary:

;; Stores one opaque identity with the workspace so persisted sessions can be
;; rebound independently of a client's local or TRAMP path spelling.

;;; Code:

(defconst mevedel-workspace-identity--regexp
  "\\`[0-9a-f]\\{64\\}\n\\'"
  "Exact on-disk format of a workspace identity.")

(defun mevedel-workspace-identity--path (root)
  "Return the durable workspace identity path below ROOT."
  (unless (and (stringp root) (file-name-absolute-p root))
    (error "Workspace root must be an absolute path: %S" root))
  (file-name-concat (file-name-as-directory (expand-file-name root))
                    ".mevedel" "workspace-id"))

(defun mevedel-workspace-identity-read (root)
  "Return ROOT's durable workspace identity, or nil when none exists.

Signal an error when the identity file exists but is malformed."
  (let ((path (mevedel-workspace-identity--path root)))
    (when (file-exists-p path)
      (let ((contents
             (with-temp-buffer
               (insert-file-contents path)
               (buffer-string))))
        (unless (string-match-p mevedel-workspace-identity--regexp contents)
          (error "Invalid workspace identity at %s" path))
        (substring contents 0 -1)))))

(defun mevedel-workspace-identity-ensure (root)
  "Return ROOT's durable workspace identity, creating it when absent.

The first atomic creator wins when multiple clients initialize ROOT at once."
  (or (mevedel-workspace-identity-read root)
      (let* ((path (mevedel-workspace-identity--path root))
             (directory (file-name-directory path))
             (identity
              (secure-hash
               'sha256
               (format "%S"
                       (list (current-time) (random most-positive-fixnum)
                             (emacs-pid) (system-name))))))
        (make-directory directory t)
        (or (mevedel-workspace-identity-read root)
            (let ((temporary
                   (let ((default-directory directory))
                     (make-nearby-temp-file
                      (expand-file-name
                       ".mevedel-workspace-id-" directory)))))
              (unwind-protect
                  (progn
                    (with-temp-file temporary
                      (insert identity "\n"))
                    (condition-case nil
                        (progn
                          (add-name-to-file temporary path nil)
                          identity)
                      (file-already-exists
                       (mevedel-workspace-identity-read root))))
                (when (file-exists-p temporary)
                  (delete-file temporary))))))))

(provide 'mevedel-workspace-identity)

;;; mevedel-workspace-identity.el ends here
