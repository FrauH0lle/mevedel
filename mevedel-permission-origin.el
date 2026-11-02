;;; mevedel-permission-origin.el -- Workspace grant bounds -*- lexical-binding: t -*-

;;; Commentary:
;; Workspace-relative grants retain their root while runtime paths are absolute.
;; These private records are never durable session or global authority.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun mevedel-permission-origin-relative (path)
  "Return canonical workspace-relative PATH syntax, or nil.
Reject parent components, home syntax, and resource or target addresses."
  (when (and (stringp path) (not (string-empty-p path))
             (not (file-name-absolute-p path))
             (not (string-prefix-p "~" path))
             (not (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" path)))
    (let ((components (split-string path "/" t)))
      (unless (member ".." components)
        (concat "./" (string-join (delete "." components) "/"))))))

(defun mevedel-permission-origin-contained-p (path root)
  "Return non-nil if PATH stays below canonical ROOT without symlinks.
Observe remote links without the TRAMP attribute cache.  Missing tails are
allowed; this is a fresh pathname check, not atomic native filesystem I/O."
  (let* ((remote-file-name-inhibit-cache t)
         (root (file-name-as-directory (expand-file-name root)))
         (path (expand-file-name path))
         (relative (file-relative-name path root))
         (cursor (directory-file-name root)))
    (and (not (file-name-absolute-p relative))
         (not (member ".." (split-string relative "/" t)))
         (equal (directory-file-name root)
                (directory-file-name (file-truename root)))
         (not (file-symlink-p cursor))
         (cl-every
          (lambda (component)
            (setq cursor (file-name-concat cursor component))
            (not (file-symlink-p cursor)))
          (delete "." (split-string relative "/" t))))))

(defun mevedel-permission-origin-grant (grant root)
  "Resolve an authored relative GRANT against ROOT, retaining its origin."
  (when-let* ((relative (mevedel-permission-origin-relative
                        (plist-get grant :path))))
    (let* ((remote-file-name-inhibit-cache t)
           (root (file-name-as-directory (file-truename root)))
           (path (expand-file-name relative root)))
      (append (list :path path :access (plist-get grant :access))
              (and (plist-get grant :recursive) '(:recursive t))
              (list :workspace-origin (list :root root :relative relative))))))

(defun mevedel-permission-origin-valid-p (grant &optional path)
  "Return non-nil when GRANT's retained workspace bound is usable.
PATH, when supplied, is the actual consumer path and must also stay within
that root.  Ordinary absolute grants have no additional bound."
  (if-let* ((origin (plist-get grant :workspace-origin)))
      (condition-case nil
          (let* ((root (plist-get origin :root))
                 (relative (plist-get origin :relative))
                 (source (plist-get grant :path)))
            (and (stringp root) (file-name-absolute-p root)
                 (equal relative (mevedel-permission-origin-relative relative))
                 (equal (directory-file-name source)
                        (directory-file-name (expand-file-name relative root)))
                 (mevedel-permission-origin-contained-p source root)
                 (or (null path) (mevedel-permission-origin-contained-p path root))))
        (error nil))
    t))

(defun mevedel-permission-origin-usable-grants (grants &optional path)
  "Return the currently usable GRANTS for consumer PATH."
  (cl-remove-if-not (lambda (grant) (mevedel-permission-origin-valid-p grant path))
                    grants))

(defun mevedel-permission-origin-usable-rules (rules)
  "Return RULES with currently unusable bounded profile grants removed.
Operation authority is independent of these filesystem additions."
  (mapcar
   (lambda (rule)
     (if (plist-member (cdr rule) :file-system)
         (cons (car rule)
               (plist-put (copy-sequence (cdr rule)) :file-system
                          (mevedel-permission-origin-usable-grants
                           (plist-get (cdr rule) :file-system))))
       rule))
   rules))

(defun mevedel-permission-origin-derive (grant path access recursive)
  "Derive PATH ACCESS RECURSIVE authority without losing GRANT's bound."
  (append (list :path path :access access)
          (and recursive '(:recursive t))
          (when-let* ((origin (plist-get grant :workspace-origin))
                      (root (plist-get origin :root)))
            (list :workspace-origin
                  (list :root root :relative
                        (mevedel-permission-origin-relative
                         (file-relative-name path root)))))))

(provide 'mevedel-permission-origin)
;;; mevedel-permission-origin.el ends here
