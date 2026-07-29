;;; mevedel-sandbox.el --- Optional child-process confinement -*- lexical-binding: t -*-

;;; Commentary:

;; Builds and probes the Linux Bubblewrap boundary used by model-triggered
;; child processes.  The host is read-only, approved roots are rebound
;; writable, and process and network namespaces cover the requested process
;; and all descendants.  This module prepares execution facts;
;; `mevedel-execution' owns launch, teardown, and the narrowly permitted
;; pre-exec fallback.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-permissions'
(declare-function mevedel-permission--get-session-scoped
                  "mevedel-permissions" (sym slot-getter))
(declare-function mevedel-permission--match-path-pattern
                  "mevedel-permissions" (path pattern))
(declare-function mevedel-permission--set-session-scoped
                  "mevedel-permissions" (sym val slot-setter))
(declare-function mevedel-permission-protected-path-policy
                  "mevedel-permissions" ())
(defvar mevedel-protected-paths)

;; `mevedel-sandbox-grants'
(declare-function mevedel-sandbox--additional-filesystem-mounts
                  "mevedel-sandbox-grants" (permissions &optional first-fd))
(declare-function mevedel-sandbox--fd-backed-command
                  "mevedel-sandbox-grants" (command paths))
(declare-function mevedel-sandbox--grant-paths
                  "mevedel-sandbox-grants" (grant))
(declare-function mevedel-sandbox--granted-path-mounts
                  "mevedel-sandbox-grants" (arguments permissions))
(declare-function mevedel-sandbox--open-granted-paths
                  "mevedel-sandbox-grants" (arguments permissions))
(declare-function mevedel-sandbox--protected-remounts
                  "mevedel-sandbox-grants" (arguments permissions))
(declare-function mevedel-sandbox--resolve-filesystem-permissions
                  "mevedel-sandbox-grants" (permissions))

;; `mevedel-structs'
(declare-function mevedel-session--set-sandbox-mode
                  "mevedel-structs" (session mode))
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x) t)


;;
;;; Configuration and state

(defun mevedel-sandbox-mode-normalize (mode)
  "Return canonical sandbox MODE or signal a user error."
  (let ((mode (cond
               ((symbolp mode) mode)
               ((stringp mode) (intern (string-trim mode)))
               (t mode))))
    (if (memq mode '(best-effort required off))
        mode
      (user-error "Unknown sandbox mode: %s" mode))))

(defun mevedel-sandbox-mode--set (sym val)
  "Set session-scoped sandbox SYM to canonical VAL."
  (require 'mevedel-permissions)
  (mevedel-permission--set-session-scoped
   sym (mevedel-sandbox-mode-normalize val)
   #'mevedel-session--set-sandbox-mode))

(defun mevedel-sandbox-mode--get (sym)
  "Return sandbox SYM from the current session or its global default."
  (require 'mevedel-permissions)
  (mevedel-permission--get-session-scoped
   sym #'mevedel-session-sandbox-mode))

(defcustom mevedel-sandbox-mode 'best-effort
  "Confinement policy for model-triggered child processes.

`best-effort' uses Bubblewrap when its cached probe succeeds and otherwise
executes directly with unrestricted-filesystem and unrestricted-network
disclosure.  `required' refuses execution when Bubblewrap is unavailable.
`off' deliberately executes directly.

The global value is copied into new sessions.  Changing it with `setopt'
inside a session updates only that session."
  :type '(choice
          (const :tag "Best effort -- prefer Bubblewrap, disclose fallback"
                 best-effort)
          (const :tag "Required -- refuse without Bubblewrap" required)
          (const :tag "Off -- execute directly with disclosure" off))
  :set #'mevedel-sandbox-mode--set
  :get #'mevedel-sandbox-mode--get
  :group 'mevedel)

(defun mevedel-sandbox-mode-effective (&optional session)
  "Return canonical sandbox mode for SESSION or the global default."
  (mevedel-sandbox-mode-normalize
   (or (and session (mevedel-session-sandbox-mode session))
       (default-toplevel-value 'mevedel-sandbox-mode)
       'best-effort)))

(defcustom mevedel-sandbox-probe-timeout 0.5
  "Maximum seconds to wait for a Bubblewrap capability probe."
  :type 'number
  :group 'mevedel)

(defvar mevedel-sandbox--probe-cache nil
  "Cached Bubblewrap availability plist, or nil before the first probe.
A runtime failure carries :retry-on-execution until the next child launch.")

(defvar mevedel-sandbox--last-facts nil
  "Most recently prepared child-confinement facts.")

(defconst mevedel-sandbox--marker-script
  "printf '%s\\n' \"$1\"; shift; exec \"$@\""
  "Shell wrapper that records entry into the requested process boundary.")

(defconst mevedel-sandbox--probe-output-limit (* 64 1024)
  "Maximum bytes of combined Bubblewrap probe output to retain.")

(define-error 'mevedel-sandbox-policy-error
  "Invalid child-confinement authority")

(defconst mevedel-sandbox-intrinsic-paths '("/dev/null")
  "Paths supplied by the private child environment without host authority.")


;;
;;; Bubblewrap profile

(defun mevedel-sandbox--probe-command (executable &optional omit-proc-p)
  "Return a harmless confinement probe command for EXECUTABLE.
OMIT-PROC-P leaves the host proc filesystem visible through the root bind."
  (append
   (list executable
         "--new-session"
         "--die-with-parent"
         "--ro-bind" "/" "/"
         "--dev" "/dev"
         "--unshare-user"
         "--unshare-pid"
         "--unshare-net")
   (unless omit-proc-p (list "--proc" "/proc"))
   (list "--" (or (executable-find "true") "/bin/true"))))

(defun mevedel-sandbox--run-probe (command)
  "Run Bubblewrap probe COMMAND within the configured time bound."
  (let ((output-buffer (generate-new-buffer " *mevedel-bwrap-probe*"))
        (deadline (+ (float-time) mevedel-sandbox-probe-timeout))
        process stderr-process timed-out result)
    (unwind-protect
        (condition-case err
            (progn
              (with-current-buffer output-buffer
                (set-buffer-multibyte nil))
              (let ((filter
                     (lambda (_process output)
                       (when (buffer-live-p output-buffer)
                         (with-current-buffer output-buffer
                           (let ((remaining
                                  (- mevedel-sandbox--probe-output-limit
                                     (buffer-size))))
                             (when (> remaining 0)
                               (insert
                                (substring output 0
                                           (min remaining
                                                (length output)))))))))))
                (setq stderr-process
                      (make-pipe-process
                       :name "mevedel-bwrap-probe-stderr"
                       :buffer output-buffer
                       :coding 'no-conversion
                       :filter filter
                       :noquery t
                       :sentinel #'ignore))
              (setq process
                    (make-process
                     :name "mevedel-bwrap-probe"
                     :buffer output-buffer
                     :stderr stderr-process
                     :command command
                     :coding 'no-conversion
                     :connection-type 'pipe
                     :filter filter
                     :noquery t
                     :sentinel #'ignore)))
              (while (and (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output
                 process (max 0 (- deadline (float-time)))))
              (when (process-live-p process)
                (setq timed-out t)
                (delete-process process))
              (accept-process-output process 0.01)
              (setq result
                    (list :status (process-exit-status process)
                          :output (with-current-buffer output-buffer
                                    (buffer-string))
                          :timed-out-p timed-out)))
          (error
           (setq result (list :error (error-message-string err)))))
      (when (process-live-p process)
        (delete-process process))
      (when (process-live-p stderr-process)
        (delete-process stderr-process))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))
    result))

(defun mevedel-sandbox--probe-succeeded-p (probe)
  "Return non-nil when PROBE completed successfully."
  (let ((status (plist-get probe :status)))
    (and (integerp status) (zerop status))))

(defun mevedel-sandbox--probe-failure-reason (probe)
  "Return a concise diagnostic for failed PROBE facts."
  (let ((status (plist-get probe :status))
        (detail (string-trim (or (plist-get probe :output) ""))))
    (cond
     ((plist-get probe :timed-out-p)
      "Bubblewrap probe timed out")
     ((plist-get probe :error)
      (format "Bubblewrap probe failed: %s" (plist-get probe :error)))
     ((string-empty-p detail)
      (format "Bubblewrap probe failed with exit code %s" status))
     (t
      (format "Bubblewrap probe failed: %s" detail)))))

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
             (let ((probe
                    (mevedel-sandbox--run-probe
                     (mevedel-sandbox--probe-command executable))))
               (cond
                ((mevedel-sandbox--probe-succeeded-p probe)
                 (list :available t :executable executable :mount-proc t))
                ((or (plist-get probe :timed-out-p)
                     (plist-get probe :error))
                 (list :available nil
                       :executable executable
                       :reason
                       (mevedel-sandbox--probe-failure-reason probe)))
                (t
                 (let ((without-proc
                        (mevedel-sandbox--run-probe
                         (mevedel-sandbox--probe-command executable t))))
                   (if (mevedel-sandbox--probe-succeeded-p without-proc)
                       (list :available t
                             :executable executable
                             :mount-proc nil)
                     (list :available nil
                           :executable executable
                           :reason
                           (mevedel-sandbox--probe-failure-reason
                            without-proc)))))))))))))

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
  (let ((current (directory-file-name (expand-file-name path)))
        missing)
    (while (and current (not (file-exists-p current)))
      (setq missing current)
      (let* ((directory (file-name-directory current))
             (parent (and directory (directory-file-name directory))))
        (setq current (unless (equal parent current) parent))))
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
                  (add-candidate path mode directory-p))))))
         (search-literal-directory
          (root name mode)
          (when (file-directory-p root)
            (if (not (file-readable-p root))
                (add-candidate root 'inaccessible t)
              (when (string-equal
                     (file-name-nondirectory (directory-file-name root))
                     name)
                (add-candidate root mode t))
              (dolist
                  (path
                   (directory-files-recursively
                    root
                    (concat "/" (regexp-quote name) "\\'")
                    t
                    (lambda (directory)
                      (cond
                       ((not (file-readable-p directory))
                        (add-candidate directory 'inaccessible t)
                        nil)
                       ((string-equal
                         (file-name-nondirectory
                          (directory-file-name directory))
                         name)
                        (add-candidate directory mode t)
                        nil)
                       (t t)))
                    nil))
                (add-candidate path mode t))))))
      (dolist (entry (mevedel-permission-protected-path-policy))
        (let* ((pattern (car entry))
               (mode (cdr entry))
               (directory-p (string-suffix-p "/**" pattern))
               (root-pattern (if directory-p (substring pattern 0 -3) pattern))
               (absolute-pattern
                (and (or (string-prefix-p "~" root-pattern)
                         (file-name-absolute-p root-pattern))
                     (expand-file-name root-pattern)))
               (literal-directory
                (and directory-p
                     (not absolute-pattern)
                     (string-prefix-p "**/" root-pattern)
                     (let ((suffix (substring root-pattern 3)))
                       (and (not (string-empty-p suffix))
                            (not (string-match-p "/" suffix))
                            (not (glob-p suffix))
                            suffix)))))
          (cond
           ((not (glob-p root-pattern))
            (add-candidate (or absolute-pattern
                               (expand-file-name root-pattern workdir))
                           mode directory-p))
           (literal-directory
            (dolist (root (delete-dups (copy-sequence writable-roots)))
              (search-literal-directory root literal-directory mode)
              (add-candidate
               (file-name-concat root literal-directory) mode t)))
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
                  :paths (mapcar #'car resolved)
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

(defun mevedel-sandbox--refused-preparation (sandbox reason)
  "Return a refused preparation with SANDBOX facts and REASON."
  (let ((facts
         (plist-put
          (mevedel-sandbox--unrestricted-facts sandbox reason)
          :refused t)))
    (setq mevedel-sandbox--last-facts facts)
    (list :state 'refused :error reason :facts facts)))

(defun mevedel-sandbox-pending-facts
    (&optional additional-permissions sandbox-permissions mode)
  "Return the selected boundary facts for a pending child request.

ADDITIONAL-PERMISSIONS is the validated additive profile.
SANDBOX-PERMISSIONS may be `require-escalated'.  This preview may probe the
configured backend, but it does not prepare a command or mutate launch facts.
A `best-effort' launch can still fall back if Bubblewrap later fails before
the requested process starts.  MODE defaults to the global sandbox mode."
  (let ((mode (mevedel-sandbox-mode-normalize
               (or mode mevedel-sandbox-mode))))
    (cond
     ((or (eq mode 'off)
          (eq sandbox-permissions 'require-escalated))
      (mevedel-sandbox--unrestricted-facts
       (if (eq mode 'off) 'off 'escalated)
       (if (eq mode 'off)
           "Confinement disabled by mevedel-sandbox-mode"
         "Full execution escalation requested")))
     ((memq mode '(best-effort required))
      (let ((availability (mevedel-sandbox-probe)))
        (cond
         ((plist-get availability :available)
          (list :sandbox 'bubblewrap
                :filesystem 'workspace-write
                :proc (if (plist-get availability :mount-proc)
                          'fresh
                        'host)
                :network
                (if (eq t (plist-get additional-permissions :network))
                    'unrestricted
                  'isolated)))
         ((eq mode 'required)
          (list :sandbox 'refused
                :filesystem 'unavailable
                :network 'unavailable
                :reason (plist-get availability :reason)))
         (t
          (mevedel-sandbox--unrestricted-facts
           'unavailable (plist-get availability :reason)))))))))

(defun mevedel-sandbox--direct-preparation (command sandbox reason)
  "Return direct preparation for COMMAND with SANDBOX and REASON facts."
  (let ((facts (mevedel-sandbox--unrestricted-facts sandbox reason)))
    (setq mevedel-sandbox--last-facts facts)
    (list :state 'unrestricted :command command :facts facts)))

(defun mevedel-sandbox--confined-preparation
    (command workdir writable-roots executable mount-proc-p
             additional-permissions)
  "Prepare COMMAND in WORKDIR with WRITABLE-ROOTS using EXECUTABLE.
MOUNT-PROC-P requests a fresh proc filesystem for the PID namespace.
ADDITIONAL-PERMISSIONS is the validated additive execution profile."
  (require 'cl-lib)
  (require 'mevedel-sandbox-grants)
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
           (filesystem-permissions
            (mevedel-sandbox--resolve-filesystem-permissions
             (cl-remove-if
              (lambda (grant)
                (member (expand-file-name (plist-get grant :path))
                        mevedel-sandbox-intrinsic-paths))
              (plist-get additional-permissions :file-system))))
           (effective-permissions
            (plist-put (copy-sequence additional-permissions)
                       :file-system filesystem-permissions))
           (network-access-p
            (eq t (plist-get effective-permissions :network)))
           (filesystem-read-count
            (cl-count 'read filesystem-permissions
                      :key (lambda (grant) (plist-get grant :access))))
           (filesystem-write-count
            (cl-count 'write filesystem-permissions
                      :key (lambda (grant) (plist-get grant :access))))
           (protected-paths (plist-get protected :paths))
           (ancestor-permissions
            (cl-remove-if-not
             (lambda (grant)
               (cl-some
                (lambda (grant-path)
                  (let ((grant-path (directory-file-name grant-path)))
                    (cl-some
                     (lambda (protected-path)
                       (and (not (string-equal grant-path protected-path))
                            (string-prefix-p
                             (file-name-as-directory grant-path)
                             protected-path)))
                     protected-paths)))
                (mevedel-sandbox--grant-paths grant)))
             filesystem-permissions))
           (post-protection-permissions
            (cl-set-difference filesystem-permissions ancestor-permissions
                               :test #'eq))
           (ancestor-mounts
            (mevedel-sandbox--additional-filesystem-mounts
             (list :file-system ancestor-permissions)))
           (post-protection-mounts
            (mevedel-sandbox--additional-filesystem-mounts
             (list :file-system post-protection-permissions)
             (+ 10 (length ancestor-permissions))))
           (facts (list :sandbox 'bubblewrap
                        :filesystem 'workspace-write
                        :proc (if mount-proc-p 'fresh 'host)
                        :network (if network-access-p
                                     'unrestricted
                                   'isolated)
                        :writable-roots roots
                        :protected-paths (plist-get protected :count)
                        :additional-filesystem
                        (length filesystem-permissions)
                        :additional-filesystem-read filesystem-read-count
                        :additional-filesystem-write filesystem-write-count))
           (arguments
            (append
             (list "--new-session"
                   "--die-with-parent"
                   "--ro-bind" "/" "/"
                   "--dev" "/dev")
             (cl-mapcan
              (lambda (root) (list "--bind" root root))
              roots)
             (plist-get ancestor-mounts :arguments)
             (mevedel-sandbox--open-granted-paths
              (plist-get protected :arguments)
              effective-permissions)
             (mevedel-sandbox--granted-path-mounts
              (plist-get protected :arguments)
              effective-permissions)
             (plist-get post-protection-mounts :arguments)
             (mevedel-sandbox--protected-remounts
              (plist-get protected :post-arguments)
              effective-permissions)
             (list "--unshare-user"
                   "--unshare-pid")
             (unless network-access-p
               (list "--unshare-net"))
             (when mount-proc-p
               (list "--proc" "/proc"))
             (list "--chdir" canonical-workdir
                   "--"
                   "sh" "-c" mevedel-sandbox--marker-script
                   "mevedel-sandbox" marker)
             command)))
      (setq mevedel-sandbox--last-facts facts)
      (list :state 'confined
            :command
            (mevedel-sandbox--fd-backed-command
             (cons executable arguments)
             (append (plist-get ancestor-mounts :paths)
                     (plist-get post-protection-mounts :paths)))
            :original-command command
            :marker marker
            :cleanup-paths (plist-get protected :cleanup-paths)
            :facts facts))))


;;
;;; Public execution interface

(defun mevedel-sandbox-prepare
    (command workdir writable-roots
             &optional additional-permissions sandbox-permissions mode)
  "Prepare child COMMAND for WORKDIR and WRITABLE-ROOTS.

Return a plist with :state, :command, and :facts.  Confined preparations also
carry :marker and :original-command.  A required but unavailable backend
returns :state `refused' and :error without a command.
ADDITIONAL-PERMISSIONS is a validated additive execution profile.
SANDBOX-PERMISSIONS may be `require-escalated' after explicit approval.
MODE defaults to the global sandbox mode."
  (setq mode (mevedel-sandbox-mode-normalize
              (or mode mevedel-sandbox-mode)))
  (if (and (eq sandbox-permissions 'require-escalated)
           (not (eq mode 'off)))
      (progn
        (when additional-permissions
          (signal 'mevedel-sandbox-policy-error
                  '("Full escalation cannot include additive permissions")))
        (mevedel-sandbox--direct-preparation
         command 'escalated "Full execution escalation approved"))
    (pcase mode
      ('off
       (mevedel-sandbox--direct-preparation
        command 'off "Confinement disabled by mevedel-sandbox-mode"))
      ((or 'best-effort 'required)
       (when (plist-get mevedel-sandbox--probe-cache :retry-on-execution)
         (setq mevedel-sandbox--probe-cache nil))
       (let ((availability (mevedel-sandbox-probe)))
         (if (plist-get availability :available)
             (condition-case err
                 (let ((preparation
                        (mevedel-sandbox--confined-preparation
                         command workdir writable-roots
                         (plist-get availability :executable)
                         (plist-get availability :mount-proc)
                         additional-permissions)))
                   (plist-put preparation :fallback-p
                              (eq mode 'best-effort)))
               (mevedel-sandbox-policy-error
                (mevedel-sandbox--refused-preparation
                 'refused (error-message-string err)))
               (error
                (let ((reason (error-message-string err)))
                  (if (eq mode 'required)
                      (mevedel-sandbox--refused-preparation
                       'unavailable reason)
                    (mevedel-sandbox--direct-preparation
                     command 'unavailable reason)))))
           (let ((reason (plist-get availability :reason)))
             (if (eq mode 'required)
                 (mevedel-sandbox--refused-preparation
                  'unavailable reason)
               (mevedel-sandbox--direct-preparation
                command 'unavailable reason))))))
      (_ (error "Unknown sandbox mode: %s" mode)))))

(defun mevedel-sandbox-launch-failed-p (preparation child-result)
  "Return non-nil when PREPARATION failed before CHILD-RESULT ran the command."
  (and (eq (plist-get preparation :state) 'confined)
       (not (plist-get child-result :timed-out-p))
       (not (eq (plist-get child-result :termination) 'signaled))
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
   (let ((read-count (or (plist-get facts :additional-filesystem-read) 0))
         (write-count (or (plist-get facts :additional-filesystem-write) 0)))
     (when (> (+ read-count write-count) 0)
       (format "; additional filesystem: %d read, %d write"
               read-count write-count)))
   (when (eq (plist-get facts :proc) 'host)
     "; proc: host")
   (let ((reason (plist-get facts :reason)))
     (when reason
       (format "; reason: %s" reason)))))

(defun mevedel-sandbox--record-launch-failure (child-result)
  "Record CHILD-RESULT as a retryable unavailable backend launch."
  (let* ((output (string-trim (or (plist-get child-result :output) "")))
         (reason
          (cond
           ((not (string-empty-p output))
            (format "Bubblewrap failed before process start: %s" output))
           ((plist-get child-result :error)
            (format "Sandbox launcher failed before process start: %s"
                    (error-message-string
                     (plist-get child-result :error))))
           (t
            (format "Bubblewrap exited before process start with exit code %s"
                    (or (plist-get child-result :exit-code) "unknown"))))))
    (setq mevedel-sandbox--probe-cache
          (list :available nil :reason reason :retry-on-execution t))
    (setq mevedel-sandbox--last-facts
          (mevedel-sandbox--unrestricted-facts 'unavailable reason))))

(provide 'mevedel-sandbox)

;;; mevedel-sandbox.el ends here
