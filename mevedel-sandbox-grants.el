;;; mevedel-sandbox-grants.el --- Filesystem grant mounts -*- lexical-binding: t -*-

;;; Commentary:

;; Compiles authorized file and directory-tree grants into FD-backed Bubblewrap
;; mounts.  Symlink grants retain their host source path, every link hop, and
;; the canonical mount target so protected parent masks can be rebuilt without
;; broadening the granted resource.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-path-in-allowed-roots-p
                  "mevedel-permission-rules" (path roots))
(declare-function mevedel-permission-rules-resource-granted-p
                  "mevedel-permission-rules" (path access grants &optional recursive))
(autoload 'mevedel-permission-rules-path-in-allowed-roots-p "mevedel-permission-rules")
(autoload 'mevedel-permission-rules-resource-granted-p "mevedel-permission-rules")


;;
;;; Grant resolution

(defun mevedel-sandbox--symlink-chain (path)
  "Return the ordered symbolic links traversed while resolving PATH."
  (let* ((path (expand-file-name path))
         (target-prefix (file-remote-p path))
         (pending
          (split-string
           (or (file-remote-p path 'localname 'never) path) "/" t))
         (current (if target-prefix (concat target-prefix "/") "/"))
         (remaining 40)
         links)
    (while pending
      (setq current (file-name-concat current (pop pending)))
      (when-let* ((target (file-symlink-p current)))
        (when (< (cl-decf remaining) 0)
          (signal 'mevedel-sandbox-policy-error
                  (list (format "Filesystem symlink chain is too deep: %s"
                                path))))
        (push (cons current target) links)
        (let ((expanded
               (if (and target-prefix (file-name-absolute-p target))
                   (concat target-prefix target)
                 (expand-file-name target (file-name-directory current)))))
          (setq pending
                (append
                 (split-string
                  (or (file-remote-p expanded 'localname 'never) expanded)
                  "/" t)
                 pending)
                current
                (if target-prefix (concat target-prefix "/") "/")))))
    (nreverse links)))

(defun mevedel-sandbox--target-identity (path)
  "Return PATH's device and inode as the target's own `stat' reports them.

The launcher compares this pair with `stat' output taken from an already
open descriptor on the target, so the recorded pair must come from the same
producer.  Client-side attributes cannot be used: TRAMP reports a synthetic
device number for remote files, which never matches the target."
  (let* ((expanded (expand-file-name path))
         (default-directory (file-name-directory expanded))
         (stat (executable-find "stat" (file-remote-p expanded))))
    (unless stat
      (signal 'mevedel-sandbox-policy-error
              '("Exact filesystem identity requires 'stat'")))
    (with-temp-buffer
      (let ((status (process-file stat nil t nil "-Lc" "%d:%i"
                                  (file-local-name expanded)))
            identity)
        (setq identity (string-trim (buffer-string)))
        (unless (and (integerp status) (zerop status)
                     (string-match-p "\\`[0-9]+:[0-9]+\\'" identity))
          (signal 'mevedel-sandbox-policy-error
                  (list
                   (format "Cannot establish exact filesystem identity: %s"
                           path))))
        identity))))

(defun mevedel-sandbox--resolve-filesystem-permissions (permissions &optional inaccessible-paths)
  "Return PERMISSIONS with canonical paths and original source paths.
Directory bind mounts expose descendants.  Refuse an exact directory rather
than silently widening it; an explicitly approved tree subsumes exact grants.
Exact directory reads outside INACCESSIBLE-PATHS need no additional mount:
the base read-only filesystem already allows them."
  (delq nil (mapcar
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
                 (if (and (file-directory-p path) (not (plist-get grant :recursive)))
                     (when (or (eq access 'write)
                               (mevedel-permission-rules-path-in-allowed-roots-p
                                (file-truename path) inaccessible-paths))
                       (signal 'mevedel-sandbox-policy-error
                               (list (format
                                      "Exact directory access cannot be confined: %s; select a recursive directory scope in the permission prompt"
                                      path))))
                   (let* ((resolved (copy-sequence grant))
                          (canonical (file-truename path))
                          (identity (mevedel-sandbox--target-identity canonical)))
                     (setq resolved (plist-put resolved :source-path path))
                     (setq resolved
                           (plist-put resolved :symlinks
                                      (mevedel-sandbox--symlink-chain path)))
                     (setq resolved (plist-put resolved :identity identity))
                     (plist-put resolved :path canonical)))))
             (cl-remove-if
              (lambda (grant)
                (and (not (plist-get grant :recursive))
                     (mevedel-permission-rules-resource-granted-p
                      (plist-get grant :path) (plist-get grant :access) permissions t)))
              permissions))))

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
  "Return FD-backed mounts for normalized filesystem PERMISSIONS.
FIRST-FD defaults to 10."
  (let (arguments grants)
    (dolist (grant (plist-get permissions :file-system))
      (let ((path (plist-get grant :path))
            (access (plist-get grant :access))
            (fd (+ (or first-fd 10) (length grants))))
        (setq arguments
              (append arguments
                      (list (if (eq access 'write)
                                "--bind-fd"
                              "--ro-bind-fd")
                            (number-to-string fd) path))
              grants (append grants (list grant)))))
    (list :arguments arguments :grants grants)))

(defun mevedel-sandbox--grant-identity (grant)
  "Return the target-native device/inode identity recorded in GRANT."
  (let ((identity (plist-get grant :identity)))
    (unless (and (stringp identity)
                 (string-match-p "\\`[0-9]+:[0-9]+\\'" identity))
      (signal 'mevedel-sandbox-policy-error
              (list "Filesystem grant lacks an exact identity")))
    identity))

(defun mevedel-sandbox--fd-backed-command (command grants &optional workdir)
  "Wrap COMMAND to preserve exact target GRANTS on file descriptors.
Also open empty input descriptors for compiled protected-file masks.
When WORKDIR is remote, discover and launch the wrapper on that target."
  (let ((empty-fds
         (cl-loop for tail on command
                  until (equal (car tail) "--")
                  when (equal (car tail) "--ro-bind-data")
                  collect (string-to-number (cadr tail)))))
    (if (not (or grants empty-fds))
        command
      (let* ((remote (and workdir (file-remote-p workdir)))
             (bash
              (if remote
                  (with-temp-buffer
                    (setq default-directory workdir)
                    (executable-find "bash" remote))
                (executable-find "bash"))))
        (unless bash
          (signal 'mevedel-sandbox-policy-error
                  '("Additive filesystem confinement requires 'bash'")))
        (let ((stat
               (if remote
                   (with-temp-buffer
                     (setq default-directory workdir)
                     (executable-find "stat" remote))
                 (executable-find "stat"))))
          (when (and grants (not stat))
            (signal 'mevedel-sandbox-policy-error
                    '("Additive filesystem confinement requires 'stat'")))
          (let* ((count (length grants))
                 (open-forms
                  (cl-loop for fd from 10 repeat count
                           collect
                           (format
                            (concat
                             "if ! exec %d<\"$1\"; then "
                             "export MEVEDEL_SANDBOX_GRANT_FAILURE=1; "
                             "exec %d<&0; "
                             "fi; "
                             "actual=$(%s -Lc '%%d:%%i' /proc/self/fd/%d "
                             "2>/dev/null) || "
                             "export MEVEDEL_SANDBOX_GRANT_FAILURE=1; "
                             "if [ \"$actual\" != \"$2\" ]; then "
                             "export MEVEDEL_SANDBOX_GRANT_FAILURE=1; "
                             "fi; shift 2")
                            fd fd (shell-quote-argument stat) fd)))
                 (script
                  (string-join
                   (append
                    ;; A target exporting POSIXLY_CORRECT puts `bash -p -c' in
                    ;; posix mode, where a failed `exec' redirection kills the
                    ;; shell instead of setting the failure flag.  Keep the
                    ;; controlled grant-refusal diagnostic in that case too.
                    (list "set +o posix"
                          "unset MEVEDEL_SANDBOX_GRANT_FAILURE")
                    open-forms
                    (mapcar (lambda (fd) (format "exec %d</dev/null || exit 125" fd))
                            empty-fds)
                    (list "exec \"$@\""))
                   "; ")))
            (append
             (list bash "-p" "-c" script "mevedel-sandbox-fds")
             (cl-mapcan
              (lambda (grant)
                (list (plist-get grant :source-path)
                      (mevedel-sandbox--grant-identity grant)))
              grants)
             command)))))))

(defun mevedel-sandbox--open-granted-paths (arguments permissions)
  "Reopen exact granted paths in protected Bubblewrap ARGUMENTS."
  (let* ((grants (plist-get permissions :file-system))
         (granted-paths
          (cl-mapcan #'mevedel-sandbox--grant-paths grants))
         (write-paths
          (cl-mapcan #'mevedel-sandbox--grant-paths
                     (cl-remove-if-not
                      (lambda (grant) (eq (plist-get grant :access) 'write))
                      grants)))
         updated)
    (while arguments
      (cond
       ((and (equal (car arguments) "--ro-bind-try")
             (member (nth 1 arguments) write-paths)
             (equal (nth 1 arguments) (nth 2 arguments)))
        ;; A granted ancestor is mounted before its protected children.  Its
        ;; own readonly restriction must not undo that explicit write grant.
        (setq arguments (nthcdr 3 arguments)))
       ((and (equal (car arguments) "--perms")
             (equal (nth 1 arguments) "000")
             (equal (nth 2 arguments) "--tmpfs")
             (member (nth 3 arguments) granted-paths))
        ;; Retain child restrictions, but not the granted directory's mask.
        (setq arguments (nthcdr 4 arguments)))
       ((and (equal (car arguments) "--perms")
             (equal (nth 1 arguments) "000")
             (equal (nth 2 arguments) "--ro-bind-data")
             (member (nth 4 arguments) granted-paths))
        (setq arguments (nthcdr 5 arguments)))
       (t (push (pop arguments) updated))))
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
