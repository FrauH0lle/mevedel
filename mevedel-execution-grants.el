;;; mevedel-execution-grants.el --- Protected child grant coverage -*- lexical-binding: t -*-

;;; Commentary:

;; Execution mounts retain separately protected descendants of approved trees.
;; Coverage here reflects that boundary without changing native resource grants.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution-target)
(require 'mevedel-permission-rules)
(require 'subr-x)

(defun mevedel-execution-grants--boundaries (path access &optional target restrictions)
  "Return protection boundaries containing PATH for ACCESS on TARGET.
Include authored and canonical ancestors, literal policy aliases and concrete
launch RESTRICTIONS.  Match each policy's root, not its descendant suffix, so
an approved protected tree still covers ordinary descendants within it."
  (let* ((paths (delete-dups (list (directory-file-name (expand-file-name path))
                                 (directory-file-name (file-truename path)))))
         boundaries)
    (dolist (entry (mevedel-permission-protected-path-policy))
      (unless (and (eq access 'read) (eq (cdr entry) 'read-only))
        (let ((pattern (string-remove-suffix "/**" (car entry))))
          (dolist (candidate paths)
            (while candidate
              (when (mevedel-permission-rules-match-path-p candidate pattern target)
                (push (file-truename candidate) boundaries))
              (setq candidate (file-name-parent-directory candidate))
              (when candidate (setq candidate (directory-file-name candidate)))))
          ;; A literal protected alias can mask the canonical target even when
          ;; neither the requested spelling nor its ancestors match the policy.
          (when (and (not (string-match-p "[*?\\[]" pattern))
                     (or (file-name-absolute-p pattern) (string-prefix-p "~" pattern)))
            (when-let* ((literal (if (and target (string-prefix-p "~" pattern))
                                    (mevedel-execution-target-expand-path target pattern)
                                  (expand-file-name pattern)))
                        ((file-exists-p literal)))
              (let ((canonical (file-truename literal)))
                (when (mevedel-permission-rules-path-in-allowed-roots-p
                       (car (last paths)) (list canonical))
                  (push canonical boundaries))))))))
    (dolist (restriction restrictions)
      (when (or (eq access 'write) (eq (plist-get restriction :mode) 'inaccessible))
        (let ((boundary (file-truename (plist-get restriction :path))))
          (when (cl-some (lambda (candidate)
                           (mevedel-permission-rules-path-in-allowed-roots-p
                            candidate (list boundary))) paths)
            (push boundary boundaries)))))
    (delete-dups (mapcar #'directory-file-name boundaries))))

(defun mevedel-execution-grants-covering-grant
    (path access grants &optional recursive target restrictions)
  "Return a GRANTS entry that can enforce ACCESS to PATH in a child.
RECURSIVE requires a tree.  TARGET supplies the policy path domain;
RESTRICTIONS supplies concrete launch masks.  A broad ancestor never crosses
a separate protection boundary.  Exact directory writes or masked reads
require already approved recursive authority, not an exact directory grant."
  (let* ((canonical (directory-file-name (file-truename path)))
         (boundaries (mevedel-execution-grants--boundaries path access target restrictions))
         (tree (or recursive
                   (and (file-directory-p path)
                        (or (eq access 'write) boundaries)))))
    (cl-find-if
     (lambda (grant)
       (and (mevedel-permission-rules-resource-granted-p path access (list grant) tree)
            (let ((root (directory-file-name (file-truename (plist-get grant :path))))
                  (source (directory-file-name (expand-file-name (plist-get grant :path)))))
              (and (mevedel-permission-rules-path-in-allowed-roots-p canonical (list root))
                   (cl-every
                    (lambda (boundary)
                      (or (mevedel-permission-rules-path-in-allowed-roots-p root (list boundary))
                          (mevedel-permission-rules-path-in-allowed-roots-p source (list boundary))))
                    boundaries)))))
     grants)))

(provide 'mevedel-execution-grants)

;;; mevedel-execution-grants.el ends here
