;;; mevedel-execution-test-helpers.el --- Shared execution test helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Shared process/session helpers for the execution test files.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-execution-process)
(require 'mevedel-execution-target)
(require 'mevedel-session-durability)
(require 'mevedel-structs)
(require 'subr-x)

(defun test-mevedel-execution--workspace (root)
  "Return an execution workspace rooted at ROOT.

TRAMP roots model project workspaces so remote sessions use portable authority;
local test roots model file workspaces so they retain PID-lock authority."
  (mevedel-workspace--create
   :type (if (file-remote-p root) 'project 'file)
   :id root :root root :name "execution"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defun test-mevedel-execution--process-gone-p (pid)
  "Return non-nil when PID no longer names a live process."
  (let ((attributes (process-attributes pid)))
    (or (null attributes)
        (equal "Z" (alist-get 'state attributes)))))

(defun test-mevedel-execution--attach-child (record spool-path)
  "Attach an unlaunched process child using SPOOL-PATH to RECORD."
  (setf (mevedel-execution--record-child record)
        (mevedel-execution-process-create
         :workdir temporary-file-directory :spool-path spool-path))
  record)

(defun test-mevedel-execution--read-pid (path)
  "Return the process id stored at PATH."
  (string-to-number
   (string-trim
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))))

(defun test-mevedel-execution--session (root)
  "Return a materialized test session rooted below ROOT."
  (let* ((workspace (test-mevedel-execution--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (save-path (file-name-as-directory
                     (file-name-concat root "session"))))
    (make-directory save-path t)
    (setf (mevedel-session-save-path session) save-path
          (mevedel-session-sandbox-mode session) 'off)
    (when (file-remote-p root)
      (puthash
       (mevedel-execution-target-identity
        (mevedel-session-execution-target session))
       t mevedel-session-durability--disclosed-targets))
    session))

(defun test-mevedel-execution--wait (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, bounded by TIMEOUT seconds."
  (with-timeout ((or timeout 5) (error "Timed out"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.02))))

(defun test-mevedel-execution--stop-all (session owner ids)
  "Stop IDS owned by OWNER in SESSION and wait for every settlement."
  (let ((remaining (length ids)))
    (dolist (id ids)
      (mevedel-execution-stop
       session owner id (lambda (_value) (setq remaining (1- remaining)))))
    (test-mevedel-execution--wait (lambda () (zerop remaining)))))

(cl-defun test-mevedel-execution--start-managed
    (session root command &key (owner "main") owner-context data-buffer
             outcome-function tty tool-args tool-use-id
             (yield-time-ms 10) artifact-directory)
  "Start managed COMMAND for SESSION at ROOT and return its first observation."
  (let (observation)
    (mevedel-execution-start-bash
     (lambda (value) (setq observation value))
     :session session :data-buffer data-buffer
     :owner owner :owner-context owner-context :command command
     :workdir root :writable-roots (list root)
     :artifact-directory (or artifact-directory
                             (unless (file-remote-p root)
                               (file-name-concat root "artifacts")))
     :outcome-function outcome-function
     :tool-args tool-args :tool-use-id tool-use-id
     :tty tty :yield-time-ms yield-time-ms)
    (test-mevedel-execution--wait (lambda () observation))
    observation))

(cl-defun test-mevedel-execution--observe
    (session execution-id &key chars (wait-ms 4000) (owner "main"))
  "Observe EXECUTION-ID in SESSION and return the delivered observation."
  (let (observation)
    (mevedel-execution-observe
     session owner execution-id
     (lambda (value) (setq observation value))
     :chars chars :wait-ms wait-ms)
    (test-mevedel-execution--wait (lambda () observation))
    observation))

(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-connection-share)

(defun test-mevedel-execution-remote--real-root (variable method)
  "Return the opt-in real TRAMP root from VARIABLE for METHOD.

The root must already exist, be writable, and be reachable through normal
  TRAMP authentication.  The tests never provision, start, or stop a target."
  (let ((value (getenv variable)))
    (unless value
      (ert-skip (format "%s is not set" variable)))
    (when-let* ((config (and (eq method 'ssh)
                            (getenv "MEVEDEL_TEST_SSH_CONFIG"))))
      (setq tramp-use-connection-share t
            tramp-ssh-controlmaster-options
            (format "-F %s" (shell-quote-argument config))))
    (when (string-empty-p value)
      (ert-fail (format "%s is set but empty" variable)))
    (let ((root (file-name-as-directory value)))
      (unless (file-remote-p root)
        (ert-fail (format "%s must be a TRAMP directory" variable)))
      (unless (equal (symbol-name method)
                     (file-remote-p root 'method 'never))
        (ert-fail
         (format "%s must use the %s TRAMP method" variable method)))
      (when (file-remote-p root 'hop 'never)
        (ert-fail (format "%s must name one target, without hops" variable)))
      (unless (file-remote-p root 'host 'never)
        (ert-fail (format "%s must name a target host" variable)))
      (condition-case err
          (progn
            (unless (file-directory-p root)
              (ert-fail (format "%s is not a directory" variable)))
            (unless (file-writable-p root)
              (ert-fail (format "%s is not writable" variable))))
        (file-error
         (ert-fail
          (format "Could not authenticate or open %s: %s"
                  variable (error-message-string err)))))
      root)))

(defun test-mevedel-execution-remote--real-temp-directory
    (root stem &optional persistent)
  "Create and return a fresh target-side directory named STEM.

The directory is created in the target's temporary directory, which is
where a disposable journey belongs.  PERSISTENT places it inside ROOT
instead: ROOT is the project volume that outlives the target, and a journey
that replaces the target must find its durable state again afterwards."
  (let ((default-directory root))
    (if persistent
        (let ((directory
               (file-name-as-directory
                (file-name-concat root (make-temp-name stem)))))
          (make-directory directory t)
          directory)
      (file-name-as-directory (make-nearby-temp-file stem t)))))

(defun test-mevedel-execution-remote--accept-storage (session)
  "Accept SESSION's target-side durable storage for an opt-in test."
  (puthash
   (mevedel-execution-target-identity
    (mevedel-session-execution-target session))
   t mevedel-session-durability--disclosed-targets))

(provide 'mevedel-execution-test-helpers)
;;; mevedel-execution-test-helpers.el ends here
