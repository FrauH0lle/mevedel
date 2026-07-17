;;; mevedel-execution-test-helpers.el --- Shared execution test helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Shared process/session helpers for the execution test files.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-structs)
(require 'subr-x)

(defun test-mevedel-execution--workspace (root)
  "Return a test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'test :id root :root root :name "execution"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defun test-mevedel-execution--process-gone-p (pid)
  "Return non-nil when PID no longer names a live process."
  (null (process-attributes pid)))

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
    (setf (mevedel-session-save-path session) save-path)
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
             outcome-function tty timeout tool-args tool-use-id
             (yield-time-ms 10))
  "Start managed COMMAND for SESSION at ROOT and return its first observation."
  (let (observation)
    (mevedel-execution-start-bash
     (lambda (value) (setq observation value))
     :session session :data-buffer data-buffer
     :owner owner :owner-context owner-context :command command
     :workdir root :writable-roots (list root)
     :artifact-directory (file-name-concat root "artifacts")
     :outcome-function outcome-function
     :tool-args tool-args :tool-use-id tool-use-id
     :timeout timeout :tty tty :yield-time-ms yield-time-ms)
    (test-mevedel-execution--wait (lambda () observation))
    observation))

(cl-defun test-mevedel-execution--observe
    (session execution-id &key chars (wait-ms 5000))
  "Observe EXECUTION-ID in SESSION and return the delivered observation."
  (let (observation)
    (mevedel-execution-observe
     session "main" execution-id
     (lambda (value) (setq observation value))
     :chars chars :wait-ms wait-ms)
    (test-mevedel-execution--wait (lambda () observation))
    observation))

(provide 'mevedel-execution-test-helpers)
;;; mevedel-execution-test-helpers.el ends here
