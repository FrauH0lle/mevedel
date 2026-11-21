;;; niwa-execution-test-helpers.el --- Shared execution test helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Shared process/session helpers for the execution test files.

;;; Code:

(require 'cl-lib)
(require 'niwa-execution)
(require 'niwa-execution-process)
(require 'niwa-execution-target)
(require 'niwa-session-durability)
(require 'niwa-structs)
(require 'niwa-transport)
(require 'subr-x)

;; `helpers'
(defvar niwa-test--write-roots)

(defun test-niwa-execution--workspace (root)
  "Return an execution workspace rooted at ROOT.

TRAMP roots model project workspaces; local test roots model file workspaces.
Both use portable lease authority."
  (niwa-workspace--create
   :type (if (file-remote-p root) 'project 'file)
   :id root :root root :name "execution"
   :file-cache (niwa-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defun test-niwa-execution--process-gone-p (pid)
  "Return non-nil when PID no longer names a live process."
  (let ((attributes (process-attributes pid)))
    (or (null attributes)
        (equal "Z" (alist-get 'state attributes)))))

(defun test-niwa-execution--attach-child (record spool-path)
  "Attach an unlaunched process child using SPOOL-PATH to RECORD."
  (setf (niwa-execution--record-child record)
        (niwa-execution-process-create
         :workdir temporary-file-directory :spool-path spool-path))
  record)

(defun test-niwa-execution--read-pid (path)
  "Return the process id stored at PATH."
  (string-to-number
   (string-trim
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))))

(defun test-niwa-execution--session (root)
  "Return a materialized test session rooted below ROOT."
  (let* ((workspace (test-niwa-execution--workspace root))
         (session (niwa-session-create "main" workspace root))
         (save-path (file-name-as-directory
                     (file-name-concat root "session"))))
    (make-directory save-path t)
    (setf (niwa-session-save-path session) save-path
          (niwa-session-sandbox-mode session) 'off)
    (when (file-remote-p root)
      (puthash
       (niwa-execution-target-identity
        (niwa-session-execution-target session))
       t niwa-session-durability--disclosed-targets))
    session))

(defun test-niwa-execution--wait (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, bounded by TIMEOUT seconds."
  (let* ((timeout (or timeout 5))
         (deadline (+ (float-time) timeout)))
    (with-timeout (timeout (error "Timed out"))
      (while (not (funcall predicate))
        ;; Direct TRAMP operations can discard the suspended timeout timer.
        (when (>= (float-time) deadline)
          (error "Timed out"))
        (accept-process-output nil 0.02)))))

(defun test-niwa-execution--timeout-after-ready (path thunk)
  "Call THUNK, arming child timeouts only after PATH contains a ready marker.
Keep the requested timeout duration: these tests exercise termination, not
whether the host can start a shell within that duration."
  (let ((schedule (symbol-function 'niwa-transport-run-at-time)))
    (cl-letf (((symbol-function 'niwa-transport-run-at-time)
               (lambda (seconds function &rest args)
                 (when (eq function #'niwa-execution-process--time-out)
                   (test-niwa-execution--wait
                    (lambda ()
                      (when-let* ((attributes (file-attributes path)))
                        (> (file-attribute-size attributes) 0)))))
                 (apply schedule seconds function args))))
      (funcall thunk))))

(defun test-niwa-execution--stop-all (session owner ids)
  "Stop IDS owned by OWNER in SESSION and wait for every settlement."
  (let ((remaining (length ids)))
    (dolist (id ids)
      (niwa-execution-stop
       session owner id (lambda (_value) (setq remaining (1- remaining)))))
    (test-niwa-execution--wait (lambda () (zerop remaining)))))

(cl-defun test-niwa-execution--start-managed
    (session root command &key (owner "main") owner-context data-buffer
             outcome-function tty tool-args tool-use-id
             (yield-time-ms 10) artifact-directory)
  "Start managed COMMAND for SESSION at ROOT and return its first observation."
  (let (observation)
    (niwa-execution-start-bash
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
    (test-niwa-execution--wait (lambda () observation))
    observation))

(cl-defun test-niwa-execution--observe
    (session execution-id &key chars (wait-ms 4000) (owner "main"))
  "Observe EXECUTION-ID in SESSION and return the delivered observation."
  (let (observation)
    (niwa-execution-observe
     session owner execution-id
     (lambda (value) (setq observation value))
     :chars chars :wait-ms wait-ms)
    (test-niwa-execution--wait (lambda () observation))
    observation))

(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-connection-share)

(defun test-niwa-execution-remote--real-root (variable method)
  "Return the opt-in real TRAMP root from VARIABLE for METHOD.

The root must already exist, be writable, and be reachable through normal
  TRAMP authentication.  The tests never provision, start, or stop a target."
  (let ((value (getenv variable)))
    (unless value
      (ert-skip (format "%s is not set" variable)))
    (when-let* ((config (and (eq method 'ssh)
                            (getenv "NIWA_TEST_SSH_CONFIG"))))
      (setq tramp-use-connection-share t
            tramp-ssh-controlmaster-options
            (format "-F %s" (shell-quote-argument config))))
    (when (string-empty-p value)
      (ert-fail (format "%s is set but empty" variable)))
    ;; Normalization can discard the hop spelling, so inspect the raw root.
    (when (file-remote-p value 'hop 'never)
      (ert-fail (format "%s must name one target, without hops" variable)))
    (let ((root (file-name-as-directory value)))
      (unless (file-remote-p root)
        (ert-fail (format "%s must be a TRAMP directory" variable)))
      (unless (equal (symbol-name method)
                     (file-remote-p root 'method 'never))
        (ert-fail
         (format "%s must use the %s TRAMP method" variable method)))
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

(defun test-niwa-execution-remote--real-temp-directory
    (root stem &optional persistent)
  "Create and return a fresh target-side directory named STEM.

The directory is created in the target's temporary directory, which is
where a disposable journey belongs.  PERSISTENT places it inside ROOT
instead: ROOT is the project volume that outlives the target, and a journey
that replaces the target must find its durable state again afterwards."
  (let* ((default-directory root)
         (directory
          (let ((niwa-test--write-roots
                 (cons (if persistent root (temporary-file-directory))
                       niwa-test--write-roots)))
            ;; Only this allocator can write in the shared parent.  The rest
            ;; of the journey owns the exact returned directory, not /tmp.
            (file-name-as-directory
             (make-nearby-temp-file
              (if persistent (file-name-concat root stem) stem) t)))))
    (push directory niwa-test--write-roots)
    directory))

(defmacro test-niwa-execution-remote--with-temporary-directory
    (directory &rest body)
  "Run BODY with target temporary files contained in owned DIRECTORY.
Other connections keep their normal temporary directories."
  (declare (indent 1) (debug (form body)))
  `(let* ((owned-directory ,directory)
          (owned-remote (file-remote-p owned-directory))
          (original-temporary-directory
           (symbol-function 'temporary-file-directory))
          (original-remote-tmpdir (symbol-function 'tramp-get-remote-tmpdir)))
     (cl-letf (((symbol-function 'temporary-file-directory)
                (lambda ()
                  (if (equal owned-remote (file-remote-p default-directory))
                      owned-directory
                    (funcall original-temporary-directory))))
               ((symbol-function 'tramp-get-remote-tmpdir)
                (lambda (vec)
                  (if (equal owned-remote
                             (file-remote-p (tramp-make-tramp-file-name vec "/")))
                      owned-directory
                    (funcall original-remote-tmpdir vec)))))
       ,@body)))

(defun test-niwa-execution-remote--accept-storage (session)
  "Accept SESSION's target-side durable storage for an opt-in test."
  (puthash
   (niwa-execution-target-identity
    (niwa-session-execution-target session))
   t niwa-session-durability--disclosed-targets))

(provide 'niwa-execution-test-helpers)
;;; niwa-execution-test-helpers.el ends here
