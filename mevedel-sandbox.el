;;; mevedel-sandbox.el --- Optional child-process confinement -*- lexical-binding: t -*-

;;; Commentary:

;; Builds and probes the Linux Bubblewrap boundary used by Bash and batch Eval.
;; The host is read-only, approved roots are rebound writable, and process and
;; network namespaces cover the requested process and all descendants.  This
;; module prepares execution facts; the tool executor owns asynchronous launch
;; and the narrowly permitted pre-exec fallback.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-permissions'
(declare-function mevedel-permission--match-path-pattern
                  "mevedel-permissions" (path pattern))
(declare-function mevedel-permission-protected-path-policy
                  "mevedel-permissions" ())
(defvar mevedel-protected-paths)


;;
;;; Configuration and state

(defcustom mevedel-sandbox-mode 'auto
  "Child-process confinement policy for Bash and batch Eval.

`auto' uses Bubblewrap when its cached probe succeeds and otherwise executes
directly with unrestricted-filesystem and unrestricted-network disclosure.
`required' refuses execution when Bubblewrap is unavailable.  `off' executes
directly and visibly disables confinement."
  :type '(choice
          (const :tag "Auto -- prefer Bubblewrap, disclose fallback" auto)
          (const :tag "Required -- refuse without Bubblewrap" required)
          (const :tag "Off -- execute directly with disclosure" off))
  :group 'mevedel)

(defvar mevedel-sandbox--probe-cache nil
  "Cached Bubblewrap availability plist, or nil before the first probe.")

(defvar mevedel-sandbox--last-facts nil
  "Most recently prepared child-confinement facts.")

(defconst mevedel-sandbox--marker-script
  "printf '%s\\n' \"$1\"; shift; exec \"$@\""
  "Shell wrapper that records entry into the requested process boundary.")

(define-error 'mevedel-sandbox-policy-error
  "Invalid child-confinement authority")


;;
;;; Bubblewrap profile

(defun mevedel-sandbox--probe-command (executable)
  "Return a harmless confinement probe command for EXECUTABLE."
  (list executable
        "--new-session"
        "--die-with-parent"
        "--ro-bind" "/" "/"
        "--dev" "/dev"
        "--unshare-user"
        "--unshare-pid"
        "--unshare-net"
        "--proc" "/proc"
        "--" (or (executable-find "true") "/bin/true")))

(defun mevedel-sandbox-probe ()
  "Return and cache the availability facts for Linux Bubblewrap.

The probe creates the same core namespaces and mounts as real execution but
runs only `true'.  A failed probe means the backend is unavailable even when a
`bwrap' executable is installed."
  (or mevedel-sandbox--probe-cache
      (setq
       mevedel-sandbox--probe-cache
       (if (not (eq system-type 'gnu/linux))
           (list :available nil
                 :reason "Bubblewrap confinement is supported only on Linux")
         (let ((executable (executable-find "bwrap")))
           (if (not executable)
               (list :available nil :reason "'bwrap' is not installed")
             (with-temp-buffer
               (condition-case err
                   (let ((status
                          (apply #'call-process executable nil t nil
                                 (cdr (mevedel-sandbox--probe-command
                                       executable)))))
                     (if (and (integerp status) (zerop status))
                         (list :available t :executable executable)
                       (list :available nil
                             :executable executable
                             :reason
                             (format "Bubblewrap probe failed%s"
                                     (let ((detail
                                            (string-trim (buffer-string))))
                                       (if (string-empty-p detail)
                                           (format " with exit code %s" status)
                                         (format ": %s" detail)))))))
                 (error
                  (list :available nil
                        :executable executable
                        :reason
                        (format "Bubblewrap probe failed: %s"
                                (error-message-string err))))))))))))

(defun mevedel-sandbox--canonical-directories (roots)
  "Return existing canonical directories from ROOTS without duplicates."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (root)
            (when (and (stringp root)
                       (file-directory-p (expand-file-name root)))
              (file-name-as-directory
               (file-truename (expand-file-name root)))))
          roots))))

(defun mevedel-sandbox--first-missing-path (path)
  "Return the first nonexistent component of absolute PATH, or nil."
  (let ((current "/") missing)
    (dolist (component (split-string (expand-file-name path) "/" t))
      (unless missing
        (setq current (file-name-concat current component))
        (unless (file-exists-p current)
          (setq missing current))))
    missing))

(defun mevedel-sandbox--writable-symlink-component (path writable-roots)
  "Return the first symlink crossing in PATH under WRITABLE-ROOTS."
  (require 'cl-lib)
  (let ((current "/") found)
    (dolist (component (split-string (expand-file-name path) "/" t))
      (unless found
        (setq current (file-name-concat current component))
        (when (and (file-symlink-p current)
                   (cl-some
                    (lambda (root)
                      (let ((root (file-name-as-directory
                                   (expand-file-name root))))
                        (or (string-equal (directory-file-name root) current)
                            (string-prefix-p root current))))
                    writable-roots))
          (setq found current))))
    found))

(defun mevedel-sandbox--git-pointer-target (path)
  "Return PATH's expanded Git directory pointer target, or nil."
  (when (and (string-equal (file-name-nondirectory path) ".git")
             (file-regular-p path))
    (with-temp-buffer
      (insert-file-contents path nil 0 4096)
      (goto-char (point-min))
      (when (looking-at "gitdir:[[:space:]]*\\(.+\\)$")
        (expand-file-name (string-trim (match-string 1))
                          (file-name-directory path))))))

(defun mevedel-sandbox--protected-candidates (workdir writable-roots)
  "Return concrete protected-path candidates for WORKDIR and WRITABLE-ROOTS."
  (require 'cl-lib)
  (require 'mevedel-permissions)
  (let (candidates)
    (cl-labels
        ((add-candidate
          (path mode directory-p)
          (let* ((path (directory-file-name (expand-file-name path)))
                 (existing
                  (cl-find path candidates
                           :key (lambda (item) (plist-get item :path))
                           :test #'string-equal)))
            (if existing
                (progn
                  (when (eq mode 'inaccessible)
                    (plist-put existing :mode mode))
                  (when directory-p
                    (plist-put existing :directory-p t)))
              (push (list :path path :mode mode :directory-p directory-p)
                    candidates))))
         (glob-p (pattern)
           (cl-some (lambda (char) (memq char '(?* ?? ?\[)))
                    (string-to-list pattern)))
         (search-tree
          (root pattern mode directory-p)
          (when (file-directory-p root)
            (if (not (file-readable-p root))
                (add-candidate root 'inaccessible t)
              (dolist
                  (path
                   (cons
                    root
                    (directory-files-recursively
                     root "." t
                     (lambda (directory)
                       (if (file-readable-p directory)
                           t
                         (add-candidate directory 'inaccessible t)
                         nil))
                     nil)))
                (when (mevedel-permission--match-path-pattern path pattern)
                  (add-candidate path mode directory-p)))))))
      (dolist (entry (mevedel-permission-protected-path-policy))
        (let* ((pattern (car entry))
               (mode (cdr entry))
               (directory-p (string-suffix-p "/**" pattern))
               (root-pattern (if directory-p (substring pattern 0 -3) pattern))
               (absolute-pattern
                (and (or (string-prefix-p "~" root-pattern)
                         (file-name-absolute-p root-pattern))
                     (expand-file-name root-pattern))))
          (cond
           ((not (glob-p root-pattern))
            (add-candidate (or absolute-pattern
                               (expand-file-name root-pattern workdir))
                           mode directory-p))
           (t
            (let ((search-roots (copy-sequence writable-roots)))
              (when absolute-pattern
                (let* ((index
                        (cl-position-if
                         (lambda (char) (memq char '(?* ?? ?\[)))
                         absolute-pattern))
                       (prefix (substring absolute-pattern 0 index))
                       (root (if (string-suffix-p "/" prefix)
                                 (directory-file-name prefix)
                               (file-name-directory prefix))))
                  (when (and root (not (string-equal root "/")))
                    (push root search-roots))))
              (dolist (root (delete-dups search-roots))
                (search-tree root root-pattern mode directory-p))
              (when (and directory-p
                         (string-prefix-p "**/" root-pattern)
                         (not (glob-p (substring root-pattern 3))))
                (let ((suffix (substring root-pattern 3)))
                  (dolist (root search-roots)
                    (add-candidate (file-name-concat root suffix)
                                   mode t)))))))))
      (nreverse candidates))))

(defun mevedel-sandbox-cleanup (preparation)
  "Remove unchanged synthetic mount targets owned by PREPARATION."
  (dolist (target (reverse (plist-get preparation :cleanup-paths)))
    (let* ((path (plist-get target :path))
           (attributes (and (file-exists-p path)
                            (file-attributes path 'string)))
           (inode (and attributes
                       (file-attribute-inode-number attributes))))
      (when (and (equal inode (plist-get target :inode))
                 (file-directory-p path))
        (condition-case err
            (progn
              (set-file-modes path #o700)
              (when (null (directory-files
                           path nil directory-files-no-dot-files-regexp))
                (delete-directory path)))
          (error
           (display-warning
            'mevedel
            (format "Could not remove sandbox mount target %s: %s"
                    path (error-message-string err))
            :warning)))))))

(defun mevedel-sandbox--protected-restrictions (workdir writable-roots)
  "Compile protected restrictions for WORKDIR and WRITABLE-ROOTS."
  (require 'cl-lib)
  (let (restrictions cleanup-paths)
    (condition-case err
        (progn
          (dolist (candidate
                   (mevedel-sandbox--protected-candidates
                    workdir writable-roots))
            (let* ((path (plist-get candidate :path))
                   (mode (plist-get candidate :mode))
                   (directory-p (plist-get candidate :directory-p))
                   (symlink
                    (mevedel-sandbox--writable-symlink-component
                     path writable-roots)))
              (when symlink
                (signal
                 'mevedel-sandbox-policy-error
                 (list
                  (format
                   "Cannot enforce protected path %s across writable symlink %s"
                   path symlink))))
              (unless (file-exists-p path)
                (when (and directory-p
                           (cl-some
                            (lambda (root)
                              (let* ((expanded-root (expand-file-name root))
                                     (root-directory
                                      (file-name-as-directory expanded-root)))
                                (or (string-equal
                                     (directory-file-name expanded-root)
                                     path)
                                    (string-prefix-p root-directory path))))
                            writable-roots))
                  (setq path (or (mevedel-sandbox--first-missing-path path)
                                 path))
                  (make-directory path)
                  (set-file-modes path #o700)
                  (let ((attributes (file-attributes path 'string)))
                    (push (list :path path
                                :inode
                                (file-attribute-inode-number attributes))
                          cleanup-paths))))
              (when (file-exists-p path)
                (push (list :path path
                            :mode mode
                            :directory-p (file-directory-p path))
                      restrictions)
                (let ((target (mevedel-sandbox--git-pointer-target path)))
                  (when (and target (file-exists-p target))
                    (push (list :path target
                                :mode mode
                                :directory-p (file-directory-p target))
                          restrictions)))
                (let ((canonical (file-truename path)))
                  (unless (string-equal canonical path)
                    (push (list :path canonical
                                :mode mode
                                :directory-p (file-directory-p canonical))
                          restrictions))))))
          (let (arguments post-arguments resolved)
            (dolist (restriction restrictions)
              (let* ((path (plist-get restriction :path))
                     (existing (assoc path resolved)))
                (when-let ((symlink
                            (mevedel-sandbox--writable-symlink-component
                             path writable-roots)))
                  (signal
                   'mevedel-sandbox-policy-error
                   (list
                    (format
                     "Cannot enforce protected path %s across writable symlink %s"
                     path symlink))))
                (if existing
                    (when (eq (plist-get restriction :mode) 'inaccessible)
                      (setcdr existing restriction))
                  (push (cons path restriction) resolved))))
            (setq resolved
                  (sort resolved
                        (lambda (left right)
                          (< (length (split-string (car left) "/" t))
                             (length (split-string (car right) "/" t))))))
            (dolist (entry resolved)
              (let* ((restriction (cdr entry))
                     (path (plist-get restriction :path))
                     (mode (plist-get restriction :mode))
                     (directory-p (plist-get restriction :directory-p)))
                (setq arguments
                      (append
                       arguments
                       (pcase mode
                         ('read-only
                          (list "--ro-bind" path path))
                         ('inaccessible
                          (if directory-p
                              (progn
                                (setq post-arguments
                                      (append post-arguments
                                              (list "--remount-ro" path)))
                                (list "--perms" "000" "--tmpfs" path))
                            (list "--ro-bind" "/dev/null" path
                                  "--chmod" "000" path))))))))
            (list :arguments arguments
                  :post-arguments post-arguments
                  :cleanup-paths (nreverse cleanup-paths)
                  :count (length resolved))))
      (error
       (mevedel-sandbox-cleanup (list :cleanup-paths cleanup-paths))
       (if (eq (car err) 'mevedel-sandbox-policy-error)
           (signal (car err) (cdr err))
         (signal 'mevedel-sandbox-policy-error
                 (list (format "Could not compile protected paths: %s"
                               (error-message-string err)))))))))

(defun mevedel-sandbox--unrestricted-facts (sandbox reason)
  "Return direct-execution facts for SANDBOX and REASON."
  (list :sandbox sandbox
        :filesystem 'unrestricted
        :network 'unrestricted
        :reason reason))

(defun mevedel-sandbox--direct-preparation (command sandbox reason)
  "Return direct preparation for COMMAND with SANDBOX and REASON facts."
  (let ((facts (mevedel-sandbox--unrestricted-facts sandbox reason)))
    (setq mevedel-sandbox--last-facts facts)
    (list :state 'unrestricted :command command :facts facts)))

(defun mevedel-sandbox--additional-filesystem-mounts (permissions)
  "Return FD-backed exact mounts for normalized filesystem PERMISSIONS."
  (let (arguments paths)
    (dolist (grant (plist-get permissions :file-system))
      (let ((path (plist-get grant :path))
            (access (plist-get grant :access))
            (fd (+ 10 (length paths))))
        (unless (and (stringp path)
                     (file-name-absolute-p path)
                     (file-exists-p path))
          (signal
           'mevedel-sandbox-policy-error
           (list (format "Additional filesystem path is unavailable: %S"
                         path))))
        (unless (memq access '(read write))
          (signal
           'mevedel-sandbox-policy-error
           (list (format "Invalid additional filesystem access: %S"
                         access))))
        (setq arguments
              (append arguments
                      (list (if (eq access 'write)
                                "--bind-fd"
                              "--ro-bind-fd")
                            (number-to-string fd) path))
              paths (append paths (list path)))))
    (list :arguments arguments :paths paths)))

(defun mevedel-sandbox--fd-backed-command (command paths)
  "Return COMMAND wrapped to preserve exact host PATHS on file descriptors."
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
        (append (list bash "-c" script "mevedel-sandbox-fds")
                paths command)))))

(defun mevedel-sandbox--open-granted-parent-traversal
    (arguments permissions)
  "Allow traversal through masked parents in ARGUMENTS for exact PERMISSIONS."
  (let ((updated (copy-sequence arguments))
        (grants (plist-get permissions :file-system)))
    (cl-loop for tail on updated
             when (and (equal (car tail) "--perms")
                       (equal (nth 1 tail) "000")
                       (equal (nth 2 tail) "--tmpfs")
                       (stringp (nth 3 tail))
                       (cl-some
                        (lambda (grant)
                          (let ((parent (file-name-as-directory
                                         (expand-file-name (nth 3 tail))))
                                (path (expand-file-name
                                       (plist-get grant :path))))
                            (string-prefix-p parent path)))
                        grants))
             do (setcar (cdr tail) "0111"))
    updated))

(defun mevedel-sandbox--confined-preparation
    (command workdir writable-roots executable additional-permissions)
  "Prepare COMMAND in WORKDIR with WRITABLE-ROOTS using EXECUTABLE.
ADDITIONAL-PERMISSIONS is the validated additive execution profile."
  (require 'cl-lib)
  (let* ((canonical-workdir
          (file-name-as-directory (file-truename workdir)))
         (roots (mevedel-sandbox--canonical-directories writable-roots)))
    (unless roots
      (signal 'mevedel-sandbox-policy-error
              '("Sandbox confinement requires a writable root")))
    (unless (cl-some
             (lambda (root)
               (or (string-equal root canonical-workdir)
                   (file-in-directory-p canonical-workdir root)))
             roots)
      (signal 'mevedel-sandbox-policy-error
              (list
               (format "Sandbox working directory is outside writable roots: %s"
                       canonical-workdir))))
    (let* ((marker (make-temp-name "MEVEDEL_SANDBOX_STARTED_"))
           (protected
            (mevedel-sandbox--protected-restrictions
             canonical-workdir roots))
           (network-access-p
            (eq t (plist-get additional-permissions :network)))
           (filesystem-permissions
            (plist-get additional-permissions :file-system))
           (filesystem-mounts
            (mevedel-sandbox--additional-filesystem-mounts
             additional-permissions))
           (facts (list :sandbox 'bubblewrap
                        :filesystem 'workspace-write
                        :network (if network-access-p
                                     'unrestricted
                                   'isolated)
                        :writable-roots roots
                        :protected-paths (plist-get protected :count)
                        :additional-filesystem
                        (length filesystem-permissions)))
           (arguments
            (append
             (list "--new-session"
                   "--die-with-parent"
                   "--ro-bind" "/" "/"
                   "--dev" "/dev")
             (cl-mapcan
              (lambda (root) (list "--bind" root root))
              roots)
             (mevedel-sandbox--open-granted-parent-traversal
              (plist-get protected :arguments)
              additional-permissions)
             (plist-get filesystem-mounts :arguments)
             (plist-get protected :post-arguments)
             (list "--unshare-user"
                   "--unshare-pid")
             (unless network-access-p
               (list "--unshare-net"))
             (list
                   "--proc" "/proc"
                   "--chdir" canonical-workdir
                   "--"
                   "sh" "-c" mevedel-sandbox--marker-script
                   "mevedel-sandbox" marker)
             command)))
      (setq mevedel-sandbox--last-facts facts)
      (list :state 'confined
            :command
            (mevedel-sandbox--fd-backed-command
             (cons executable arguments)
             (plist-get filesystem-mounts :paths))
            :original-command command
            :marker marker
            :cleanup-paths (plist-get protected :cleanup-paths)
            :facts facts))))


;;
;;; Public execution interface

(defun mevedel-sandbox-prepare
    (command workdir writable-roots &optional additional-permissions)
  "Prepare child COMMAND for WORKDIR and WRITABLE-ROOTS.

Return a plist with :state, :command, and :facts.  Confined preparations also
carry :marker and :original-command.  A required but unavailable backend
returns :state `refused' and :error without a command.
ADDITIONAL-PERMISSIONS is a validated additive execution profile."
  (pcase mevedel-sandbox-mode
    ('off
     (mevedel-sandbox--direct-preparation
      command 'off "Confinement disabled by mevedel-sandbox-mode"))
    ((or 'auto 'required)
     (let ((availability (mevedel-sandbox-probe)))
       (if (plist-get availability :available)
           (condition-case err
               (let ((preparation
                      (mevedel-sandbox--confined-preparation
                       command workdir writable-roots
                       (plist-get availability :executable)
                       additional-permissions)))
                 (plist-put preparation :fallback-p
                            (eq mevedel-sandbox-mode 'auto)))
             (mevedel-sandbox-policy-error
              (let* ((reason (error-message-string err))
                     (facts
                      (mevedel-sandbox--unrestricted-facts
                       'refused reason)))
                (setq mevedel-sandbox--last-facts facts)
                (list :state 'refused :error reason :facts facts)))
             (error
              (let ((reason (error-message-string err)))
                (if (eq mevedel-sandbox-mode 'required)
                    (let ((facts
                           (mevedel-sandbox--unrestricted-facts
                            'unavailable reason)))
                      (setq mevedel-sandbox--last-facts facts)
                      (list :state 'refused :error reason :facts facts))
                  (mevedel-sandbox--direct-preparation
                   command 'unavailable reason)))))
         (let ((reason (plist-get availability :reason)))
           (if (eq mevedel-sandbox-mode 'required)
               (let ((facts
                      (mevedel-sandbox--unrestricted-facts
                       'unavailable reason)))
                 (setq mevedel-sandbox--last-facts facts)
                 (list :state 'refused :error reason :facts facts))
             (mevedel-sandbox--direct-preparation
              command 'unavailable reason))))))
    (_ (error "Unknown sandbox mode: %s" mevedel-sandbox-mode))))

(defun mevedel-sandbox-launch-failed-p (preparation child-result)
  "Return non-nil when PREPARATION failed before CHILD-RESULT ran the command."
  (and (eq (plist-get preparation :state) 'confined)
       (not (plist-get child-result :timed-out-p))
       (not (member (plist-get preparation :marker)
                    (split-string (or (plist-get child-result :output) "")
                                  "\n" nil)))
       (or (plist-get child-result :error)
           (not (zerop (or (plist-get child-result :exit-code) -1))))))

(defun mevedel-sandbox-strip-marker (preparation child-result)
  "Return CHILD-RESULT without PREPARATION's private start marker line."
  (let* ((marker (plist-get preparation :marker))
         (output (or (plist-get child-result :output) ""))
         (lines (split-string output "\n" nil))
         (cleaned (mapconcat #'identity (delete marker lines) "\n")))
    (plist-put (copy-sequence child-result) :output cleaned)))

(defun mevedel-sandbox-status-text (facts)
  "Return one persistent human-readable status line for FACTS."
  (concat
   (format "sandbox: %s; filesystem: %s; network: %s"
           (plist-get facts :sandbox)
           (plist-get facts :filesystem)
           (plist-get facts :network))
   (let ((reason (plist-get facts :reason)))
     (when reason
       (format "; reason: %s" reason)))))

(defun mevedel-sandbox--record-launch-failure (child-result)
  "Record CHILD-RESULT as an unavailable backend launch."
  (let* ((output (string-trim (or (plist-get child-result :output) "")))
         (reason
          (if (string-empty-p output)
              "Bubblewrap failed before the requested process started"
            (format "Bubblewrap failed before process start: %s" output))))
    (setq mevedel-sandbox--probe-cache
          (list :available nil :reason reason))
    (setq mevedel-sandbox--last-facts
          (mevedel-sandbox--unrestricted-facts 'unavailable reason))))

(provide 'mevedel-sandbox)

;;; mevedel-sandbox.el ends here
