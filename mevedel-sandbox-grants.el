;;; mevedel-sandbox-grants.el --- Filesystem grant mounts -*- lexical-binding: t -*-

;;; Commentary:

;; Compiles authorized grants into identity-checked Bubblewrap mounts, and lays
;; them out against protected-path restrictions in one mount plan.  Symlink
;; grants retain their host source path, every link hop,
;; and the canonical mount target so protected parent masks can be rebuilt
;; without broadening the granted resource.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)
(require 'mevedel-execution-grants)

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-path-in-allowed-roots-p
                  "mevedel-permission-rules" (path roots))
(autoload 'mevedel-permission-rules-path-in-allowed-roots-p "mevedel-permission-rules")


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

(defun mevedel-sandbox--target-stat (path)
  "Return PATH's identity and device-node flag from the target's `stat'.

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
      (let ((status (process-file stat nil t nil "-Lc" "%d:%i:%f"
                                  (file-local-name expanded)))
            output)
        (setq output (string-trim (buffer-string)))
        (unless (and (integerp status) (zerop status)
                     (string-match "\\`\\([0-9]+:[0-9]+\\):\\([0-9a-fA-F]+\\)\\'" output))
          (signal 'mevedel-sandbox-policy-error
                  (list
                   (format "Cannot establish exact filesystem identity: %s"
                           path))))
        (list :identity (match-string 1 output)
              :device-p (memq (logand (string-to-number (match-string 2 output) 16)
                                     #o170000)
                              '(#o020000 #o060000)))))))

(defun mevedel-sandbox--resolve-filesystem-permissions
    (permissions &optional inaccessible-paths restrictions)
  "Return PERMISSIONS with canonical paths and original source paths.
Directory bind mounts expose descendants.  Refuse an exact directory rather
than silently widening it; an approved tree subsumes ordinary exact grants.
RESTRICTIONS are the concrete launch protection boundaries.
Exact directory reads outside INACCESSIBLE-PATHS need no additional mount:
the base read-only filesystem already allows them."
  (delq nil (mapcar
             (lambda (grant)
               ;; One spelling for every prefix comparison and mount:
               ;; `//run/user' is `/run/user' to the kernel.
               (let ((path (mevedel-collapse-leading-slashes (plist-get grant :path)))
                     (access (plist-get grant :access)))
                 (unless (mevedel-permission-origin-valid-p grant)
                   (signal 'mevedel-sandbox-policy-error
                           (list (format "Workspace-relative filesystem grant is no longer contained: %S"
                                         path))))
                 ;; A quoted name escapes spelling-based protection and
                 ;; would otherwise be dropped silently as an exact read.
                 (when (and (stringp path) (file-name-quoted-p path))
                   (signal 'mevedel-sandbox-policy-error
                           (list (format "Quoted filesystem grant paths are not supported: %S"
                                         path))))
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
                                (mevedel-file-truename path) inaccessible-paths))
                       (signal 'mevedel-sandbox-policy-error
                               (list (format
                                      "Exact directory access cannot be confined: %s; select a recursive directory scope in the permission prompt"
                                      path))))
                   (let* ((resolved (copy-sequence grant))
                          (canonical (mevedel-file-truename path))
                          (attributes (mevedel-sandbox--target-stat canonical))
                          (identity (plist-get attributes :identity))
                          (device-p (plist-get attributes :device-p)))
                     ;; A read-only mount does not prevent writes or ioctls on
                     ;; a device.  Never turn a read grant into device access.
                     (when device-p
                       (unless (eq access 'write)
                         (signal 'mevedel-sandbox-policy-error
                                 (list (format "Read-only device access cannot be confined: %s; request explicit write access"
                                               path))))
                       (setq resolved (plist-put resolved :device-p t)))
                     (setq resolved (plist-put resolved :source-path path))
                     (setq resolved
                           (plist-put resolved :symlinks
                                      (mevedel-sandbox--symlink-chain path)))
                     (setq resolved (plist-put resolved :identity identity))
                     ;; Resolution may yield to another writer.  Do not accept
                     ;; a new outside snapshot captured after the first check.
                     (unless (mevedel-permission-origin-valid-p grant canonical)
                       (signal 'mevedel-sandbox-policy-error
                               (list (format "Workspace-relative filesystem grant changed during resolution: %S"
                                             path))))
                     (plist-put resolved :path canonical)))))
             (cl-remove-if
              (lambda (grant)
                (and (not (plist-get grant :recursive))
                     (mevedel-execution-grants-covering-grant
                      (plist-get grant :path) (plist-get grant :access)
                      permissions t nil restrictions)
                     ;; A directory bind keeps nodev and cannot supply a
                     ;; device grant, even when its path authority covers it.
                     (or (not (file-exists-p (plist-get grant :path)))
                         (not (plist-get
                               (mevedel-sandbox--target-stat (plist-get grant :path))
                               :device-p)))))
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
    (grants &optional first-fd)
  "Return mount arguments for normalized filesystem GRANTS.
FIRST-FD defaults to 10."
  (cl-loop for grant in grants
           for fd from (or first-fd 10)
           append (if (plist-get grant :device-p)
                      ;; Ordinary binds add nodev.  Bubblewrap resolves this
                      ;; descriptor to a path; verify the mounted node too.
                      (list "--dev-bind" (format "/proc/self/fd/%d" fd)
                            (plist-get grant :path))
                    (list (if (eq (plist-get grant :access) 'write)
                              "--bind-fd"
                            "--ro-bind-fd")
                          (number-to-string fd) (plist-get grant :path)))))

(defun mevedel-sandbox--grant-identity (grant)
  "Return the target-native device/inode identity recorded in GRANT."
  (let ((identity (plist-get grant :identity)))
    (unless (and (stringp identity)
                 (string-match-p "\\`[0-9]+:[0-9]+\\'" identity))
      (signal 'mevedel-sandbox-policy-error
              (list "Filesystem grant lacks an exact identity")))
    identity))

(defun mevedel-sandbox--device-grant-command (command grants workdir)
  "Verify mounted device GRANTS and close their source FDs before COMMAND.
WORKDIR selects the target's Bash and stat.  Bubblewrap's path-based
device bind lacks its FD-based mount's identity check and descriptor cleanup."
  (if (not (cl-some (lambda (grant) (plist-get grant :device-p)) grants))
      command
    (let* ((default-directory workdir)
           (remote (file-remote-p workdir))
           (bash (executable-find "bash" remote))
           (stat (executable-find "stat" remote)))
      (unless (and bash stat)
        (signal 'mevedel-sandbox-policy-error
                '("Device confinement requires 'bash' and 'stat'")))
      (let ((checks
             (cl-loop for grant in grants for fd from 10
                      when (plist-get grant :device-p)
                      collect
                      (format
                       (concat "actual=$(%s -Lc '%%d:%%i' -- %s 2>&1) || "
                               "export MEVEDEL_SANDBOX_GRANT_FAILURE=1; "
                               "if [ \"$actual\" != %s ]; then "
                               "export MEVEDEL_SANDBOX_GRANT_FAILURE=1; fi; "
                               "exec %d<&-")
                       (shell-quote-argument stat)
                       (shell-quote-argument (file-local-name (plist-get grant :path)))
                       (shell-quote-argument (mevedel-sandbox--grant-identity grant)) fd))))
        (append (list bash "-p" "-c"
                      (string-join (append checks '("exec \"$@\"")) "; ")
                      "mevedel-sandbox-devices")
                command)))))

(defun mevedel-sandbox--fd-backed-command
    (command grants &optional mask-fds workdir)
  "Wrap COMMAND to preserve exact target GRANTS on file descriptors.
MASK-FDS are the empty input descriptors backing protected-file masks.
When WORKDIR is remote, discover and launch the wrapper on that target."
  (if (not (or grants mask-fds))
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
                          mask-fds)
                  (list "exec \"$@\""))
                 "; ")))
          (append
           (list bash "-p" "-c" script "mevedel-sandbox-fds")
           (cl-mapcan
            (lambda (grant)
              (list (plist-get grant :source-path)
                    (mevedel-sandbox--grant-identity grant)))
            grants)
           command))))))

(defun mevedel-sandbox--mount-plan (restrictions grants)
  "Return the Bubblewrap mounts for protected RESTRICTIONS and GRANTS.

RESTRICTIONS are `(:path :mode :directory-p)' plists ordered shallow to deep;
`:required' means a missing read-only mount source must refuse launch.
GRANTS are normalized filesystem grants.  A grant containing a protected path
is bound before the protections so nested masks still apply inside it; every
other grant is bound after them.  A protected path that is itself granted
keeps no mask, and an explicit write grant also lifts its read-only
restriction and remount.  A masked directory above a grant stays traversable,
with only the parents and link hops the grant needs recreated beneath it.
Return `:arguments', the `:grants' in descriptor order, and the `:mask-fds'
backing private empty file masks."
  (let* ((grant-paths
          (lambda (grant)
            (mapcar #'directory-file-name (mevedel-sandbox--grant-paths grant))))
         (granted-paths (mapcan grant-paths grants))
         (write-paths
          (mapcan grant-paths
                  (cl-remove-if-not
                   (lambda (grant) (eq (plist-get grant :access) 'write))
                   grants)))
         (protected-paths
          (mapcar (lambda (restriction) (plist-get restriction :path))
                  restrictions))
         (under-p (lambda (root path)
                    (string-prefix-p (file-name-as-directory root) path)))
         (ancestors
          (cl-remove-if-not
           (lambda (grant)
             (cl-some (lambda (grant-path)
                        (cl-some (lambda (protected)
                                   (funcall under-p grant-path protected))
                                 protected-paths))
                      (funcall grant-paths grant)))
           grants))
         (later (cl-set-difference grants ancestors :test #'eq))
         ;; Parent directory mounts must not cover a later exact device bind.
         (ordered (append ancestors
                          (cl-remove-if (lambda (grant) (plist-get grant :device-p)) later)
                          (cl-remove-if-not (lambda (grant) (plist-get grant :device-p)) later)))
         (ancestor-mounts
          (mevedel-sandbox--additional-filesystem-mounts ancestors))
         (later-mounts
          (mevedel-sandbox--additional-filesystem-mounts
           (nthcdr (length ancestors) ordered)
           (+ 10 (length ancestors))))
         (next-fd (+ 10 (length grants)))
         protections remounts directories symlinks mask-fds)
    (dolist (restriction restrictions)
      (let* ((path (plist-get restriction :path))
             (granted-p (member path granted-paths))
             (writable-p (member path write-paths)))
        (pcase (plist-get restriction :mode)
          ('read-only
           ;; State roots protect future files too: their disappearance must
           ;; refuse launch, not silently discard the boundary.
           (unless writable-p
             (setq protections
                   (append protections
                           (list (if (plist-get restriction :required)
                                     "--ro-bind" "--ro-bind-try")
                                 path path)))))
          ('inaccessible
           (cond
            ((not (plist-get restriction :directory-p))
             (unless granted-p
               ;; Set permissions on a private empty file before the readonly
               ;; mount, never on a bound host inode.
               (push next-fd mask-fds)
               (setq protections
                     (append protections
                             (list "--perms" "000" "--ro-bind-data"
                                   (number-to-string next-fd) path)))
               (cl-incf next-fd)))
            (t
             (unless writable-p
               (setq remounts (append remounts (list "--remount-ro" path))))
             (unless granted-p
               (let ((traversed
                      (cl-some (lambda (grant-path) (funcall under-p path grant-path))
                               granted-paths)))
                 (dolist (grant grants)
                   (dolist (grant-path (funcall grant-paths grant))
                     (let ((parent (file-name-parent-directory grant-path)))
                       (while (and parent
                                   (funcall under-p path (directory-file-name parent)))
                         (push (directory-file-name parent) directories)
                         (setq parent (file-name-parent-directory parent)))))
                   (dolist (link (plist-get grant :symlinks))
                     (when (funcall under-p path (car link))
                       (push link symlinks))))
                 (setq protections
                       (append protections
                               (list "--perms" (if traversed "0111" "000")
                                     "--tmpfs" path)))))))))))
    (list :arguments
          (append
           ancestor-mounts
           protections
           (mapcan (lambda (directory) (list "--dir" directory))
                   (sort (delete-dups directories)
                         (lambda (left right) (< (length left) (length right)))))
           (mapcan (lambda (link) (list "--symlink" (cdr link) (car link)))
                   (delete-dups (nreverse symlinks)))
           later-mounts
           remounts)
          :grants ordered
          :mask-fds (nreverse mask-fds))))

(provide 'mevedel-sandbox-grants)

;;; mevedel-sandbox-grants.el ends here
