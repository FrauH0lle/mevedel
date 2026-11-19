;;; terminal-check.el --- Isolated terminal floating-composer checks -*- lexical-binding: t -*-

;;; Commentary:

;; Entry point for Eask's terminal-check script.  The batch parent starts a
;; separate `emacs -Q -nw' on a pseudo-terminal of fixed size, never this
;; editor, and owns the deadline and temporary HOME.  The child runs the
;; native floating-composer cases that hold on a terminal and exits nonzero
;; on any unexpected result or skip.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defvar warning-minimum-level)

(defconst mevedel-terminal-check--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Checkout whose sources and tests the runner uses.")

(defconst mevedel-terminal-check--selector
  '(or (tag mevedel-floating-composer/hand-over)
       (tag mevedel-floating-composer/host-geometry)
       (tag mevedel-floating-composer/keys))
  "ERT selector for the cases the terminal child runs.")

(defun mevedel-terminal-check-run ()
  "Run the terminal cases in a disposable terminal Emacs, never this editor."
  (unless noninteractive (error "Use the Eask terminal-check script, not the live editor"))
  (let* ((temporary (make-temp-file "mevedel-terminal-" t))
         (home (file-name-concat temporary "home"))
         (bootstrap (file-name-concat temporary "bootstrap.el"))
         (log (file-name-concat temporary "ert.log"))
         (deadline (+ (float-time) 300))
         (process nil))
    (make-directory home)
    (add-hook 'kill-emacs-hook
              (lambda ()
                (when (and process (process-live-p process)) (delete-process process))
                (when (file-exists-p temporary) (delete-directory temporary t))))
    (with-temp-file bootstrap
      (prin1 `(progn
                (setq load-path ',load-path native-comp-jit-compilation nil)
                (load ,(file-name-concat mevedel-terminal-check--root
                                         "test" "terminal-check.el")
                      nil t)
                (mevedel-terminal-check--child ,log))
             (current-buffer)))
    (let ((process-environment
           (append (list (concat "HOME=" home) "TERM=xterm-256color"
                         "DBUS_SESSION_BUS_ADDRESS")
                   (mapcar (lambda (name)
                             (let ((directory (file-name-concat home name)))
                               (make-directory directory)
                               (format "%s=%s" name directory)))
                           '("XDG_CACHE_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME"
                             "XDG_STATE_HOME" "XDG_RUNTIME_DIR"))
                   process-environment))
          (default-directory mevedel-terminal-check--root))
      (setq process
            (make-process
             :name "mevedel-terminal-check" :connection-type 'pty :noquery t
             ;; The child sizes its frame from the terminal when it starts.
             :command (list "sh" "-c" "stty rows 40 cols 110 && exec \"$0\" \"$@\""
                            (expand-file-name invocation-name invocation-directory)
                            "-Q" "-nw" "--load" bootstrap)
             ;; Drain the screen output, which would otherwise block the child.
             :filter #'ignore))
      ;; A real terminal focus-in report, once the child has enabled them.
      (run-at-time 1 nil (lambda () (when (process-live-p process)
                                      (process-send-string process "\e[I"))))
      (while (and (process-live-p process) (< (float-time) deadline))
        (accept-process-output process 0.1)))
    (when (file-exists-p log)
      (with-temp-buffer (insert-file-contents log) (princ (buffer-string))))
    (let ((status (if (process-live-p process) 'timeout (process-exit-status process))))
      (delete-process process)
      (delete-directory temporary t)
      (unless (eq status 0)
        (princ (format "Terminal check FAILED (%s)\n" status))
        (kill-emacs 1))
      (princ "Terminal check passed\n"))))

(defun mevedel-terminal-check--child (log &optional deadline)
  "Run the terminal cases, write the ERT report to LOG, and always exit.
Until DEADLINE, first wait for the parent's focus-in report: only the
command loop decodes it, so wait from a timer, not a loop."
  (let ((deadline (or deadline (+ (float-time) 10))))
    (if (and (not (eq t (frame-focus-state))) (< (float-time) deadline))
        (run-at-time 0.1 nil #'mevedel-terminal-check--child log deadline)
      (mevedel-terminal-check--run-tests log))))

(defun mevedel-terminal-check--run-tests (log)
  "Run the terminal cases, write the ERT report to LOG, and exit."
  (let ((status 1))
    (condition-case err
        (progn
          (when (display-graphic-p) (error "A terminal frame is required"))
          ;; ERT's multi-line messages, or a warning window such as the one
          ;; for missing Markdown grammars, would resize the host mid-case.
          (setq resize-mini-windows nil
                warning-minimum-level :emergency)
          (load (file-name-concat mevedel-terminal-check--root
                                  "test" "test-mevedel-floating-composer-graphical.el")
                nil t)
          (let ((stats (ert-run-tests-batch mevedel-terminal-check--selector)))
            (cl-loop for test across (ert--stats-tests stats)
                     for result = (ert-test-most-recent-result test)
                     when (ert-test-skipped-p result)
                     do (message "Skipped %s: %s" (ert-test-name test)
                                 (ert-reason-for-test-result result)))
            ;; A skip is missing coverage, not a pass.
            (when (and (> (ert-stats-total stats) 0)
                       (zerop (ert-stats-completed-unexpected stats))
                       (zerop (ert-stats-skipped stats)))
              (setq status 0))))
      (error (message "Terminal check error: %s" (error-message-string err))))
    (with-current-buffer (messages-buffer)
      (write-region (point-min) (point-max) log nil 'silent))
    (kill-emacs status)))

(provide 'terminal-check)
;;; terminal-check.el ends here
