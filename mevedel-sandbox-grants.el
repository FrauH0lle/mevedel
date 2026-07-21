;;; mevedel-sandbox-grants.el --- Exact filesystem grant mounts -*- lexical-binding: t -*-

;;; Commentary:

;; Compiles authorized exact filesystem grants into FD-backed Bubblewrap
;; mounts.  Symlink grants retain their host source path, every link hop, and
;; the canonical mount target so protected parent masks can be rebuilt without
;; broadening the granted resource.

;;; Code:

(eval-when-compile
  (require 'cl-lib))


;;
;;; Grant resolution

(defun mevedel-sandbox--symlink-chain (path)
  "Return the ordered symbolic links traversed while resolving PATH."
  (let ((pending (split-string (expand-file-name path) "/" t))
        (current "/")
        (remaining 40)
        links)
    (while pending
      (setq current (file-name-concat current (pop pending)))
      (when-let ((target (file-symlink-p current)))
        (when (< (cl-decf remaining) 0)
          (signal 'mevedel-sandbox-policy-error
                  (list (format "Filesystem symlink chain is too deep: %s"
                                path))))
        (push (cons current target) links)
        (setq pending
              (append
               (split-string
                (expand-file-name target (file-name-directory current))
                "/" t)
               pending)
              current "/")))
    (nreverse links)))

(defun mevedel-sandbox--resolve-filesystem-permissions (permissions)
  "Return PERMISSIONS with canonical paths and original source paths."
  (mapcar
   (lambda (grant)
     (let ((path (plist-get grant :path))
           (access (plist-get grant :access)))
       (unless (and (stringp path)
                    (file-name-absolute-p path)
                    (file-exists-p path))
         (signal 'mevedel-sandbox-policy-error
                 (list (format "Additional filesystem path is unavailable: %S"
                               path))))
       (unless (memq access '(read write))
         (signal 'mevedel-sandbox-policy-error
                 (list (format "Invalid additional filesystem access: %S"
                               access))))
       (let ((resolved (copy-sequence grant)))
         (setq resolved (plist-put resolved :source-path path))
         (setq resolved
               (plist-put resolved :symlinks
                          (mevedel-sandbox--symlink-chain path)))
         (plist-put resolved :path (file-truename path)))))
   permissions))

(defun mevedel-sandbox--grant-paths (grant)
  "Return every original, intermediate, and canonical path in GRANT."
  (delete-dups
   (delq nil
         (append (list (plist-get grant :source-path))
                 (mapcar #'car (plist-get grant :symlinks))
                 (list (plist-get grant :path))))))


;;
;;; Bubblewrap arguments

(defun mevedel-sandbox--additional-filesystem-mounts
    (permissions &optional first-fd)
  "Return FD-backed exact mounts for normalized filesystem PERMISSIONS.
FIRST-FD defaults to 10."
  (let (arguments paths)
    (dolist (grant (plist-get permissions :file-system))
      (let ((path (plist-get grant :path))
            (source (or (plist-get grant :source-path)
                        (plist-get grant :path)))
            (access (plist-get grant :access))
            (fd (+ (or first-fd 10) (length paths))))
        (setq arguments
              (append arguments
                      (list (if (eq access 'write)
                                "--bind-fd"
                              "--ro-bind-fd")
                            (number-to-string fd) path))
              paths (append paths (list source)))))
    (list :arguments arguments :paths paths)))

(defun mevedel-sandbox--fd-backed-command (command paths)
  "Return COMMAND wrapped to preserve exact host PATHS on file descriptors."
  (require 'subr-x)
  (if (not paths)
      command
    (let ((bash (executable-find "bash")))
      (unless bash
        (signal 'mevedel-sandbox-policy-error
                '("Additive filesystem confinement requires 'bash'")))
      (let* ((count (length paths))
             (open-forms
              (cl-loop for index from 1 to count
                       for fd from 10
                       collect (format "exec %d<\"$%d\"" fd index)))
             (script
              (string-join
               (append open-forms
                       (list (format "shift %d" count) "exec \"$@\""))
               "; ")))
        (append (list bash "-p" "-c" script "mevedel-sandbox-fds")
                paths command)))))

(defun mevedel-sandbox--open-granted-paths (arguments permissions)
  "Reopen exact granted paths in protected Bubblewrap ARGUMENTS."
  (let* ((grants (plist-get permissions :file-system))
         (granted-paths
          (cl-mapcan #'mevedel-sandbox--grant-paths grants))
         updated)
    (while arguments
      (if (and (equal (car arguments) "--ro-bind")
               (equal (nth 1 arguments) "/dev/null")
               (member (nth 2 arguments) granted-paths)
               (equal (nth 3 arguments) "--chmod")
               (equal (nth 4 arguments) "000")
               (equal (nth 5 arguments) (nth 2 arguments)))
          (setq arguments (nthcdr 6 arguments))
        (push (pop arguments) updated)))
    (setq updated (nreverse updated))
    (cl-loop for tail on updated
             when (and (equal (car tail) "--perms")
                       (equal (nth 1 tail) "000")
                       (equal (nth 2 tail) "--tmpfs")
                       (stringp (nth 3 tail))
                       (cl-some
                        (lambda (grant)
                          (let ((parent (file-name-as-directory
                                         (expand-file-name (nth 3 tail)))))
                            (cl-some
                             (lambda (path)
                               (string-prefix-p parent path))
                             (mevedel-sandbox--grant-paths grant))))
                        grants))
             do (setcar (cdr tail) "0111"))
    updated))

(defun mevedel-sandbox--granted-path-mounts (arguments permissions)
  "Return empty parent and symlink mounts needed below masked ARGUMENTS."
  (let (directories symlinks)
    (cl-loop for tail on arguments
             when (and (equal (car tail) "--perms")
                       (equal (nth 2 tail) "--tmpfs"))
             do (let ((root (file-name-as-directory (nth 3 tail))))
                  (dolist (grant (plist-get permissions :file-system))
                    (dolist (path (mevedel-sandbox--grant-paths grant))
                      (let ((parent
                             (directory-file-name
                              (file-name-directory path))))
                        (while (and (not (string-equal parent
                                                       (directory-file-name root)))
                                    (string-prefix-p root parent))
                          (push parent directories)
                          (setq parent
                                (directory-file-name
                                 (file-name-directory parent))))))
                    (dolist (link (plist-get grant :symlinks))
                      (when (string-prefix-p root (car link))
                        (push link symlinks))))))
    (append
     (cl-mapcan (lambda (path) (list "--dir" path))
                (sort (delete-dups directories)
                      (lambda (left right) (< (length left) (length right)))))
     (cl-mapcan (lambda (link) (list "--symlink" (cdr link) (car link)))
                (delete-dups (nreverse symlinks))))))

(defun mevedel-sandbox--protected-remounts (arguments permissions)
  "Return protected remount ARGUMENTS not superseded by exact PERMISSIONS."
  (cl-loop for (option path) on arguments by #'cddr
           unless (cl-some
                   (lambda (grant)
                     (and (eq (plist-get grant :access) 'write)
                          (member
                           (directory-file-name (expand-file-name path))
                           (mapcar #'directory-file-name
                                   (mevedel-sandbox--grant-paths grant)))))
                   (plist-get permissions :file-system))
           append (list option path)))

(provide 'mevedel-sandbox-grants)

;;; mevedel-sandbox-grants.el ends here
