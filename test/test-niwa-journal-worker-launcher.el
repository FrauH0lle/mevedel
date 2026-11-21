;;; test-niwa-journal-worker-launcher.el --- Closed launcher contract -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the public launcher with real children.  Faults alter the process
;; boundary, not owner implementation functions or the operation table.

;;; Code:
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-journal-worker)
(require 'niwa-journal-process)
(require 'niwa-journal-claim)
(require 'niwa-workspace-identity)
(require 'niwa-transport)
(require 'niwa-tool-exec)
(require 'niwa-execution)
(require 'niwa-utilities)

(defun niwa-test-launcher--wait (predicate)
  "Wait boundedly for PREDICATE, allowing process and transport delivery."
  (let ((until (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) until))
      (accept-process-output nil .005))
    (should (funcall predicate))))

(defun niwa-test-launcher--with-workspace (function)
  "Call FUNCTION with an isolated workspace and local worker lifetime."
  (let* ((root (make-temp-file "niwa-launcher-" t))
         (workspace (niwa-workspace--create :root root :id root :type 'project))
         (kill-emacs-hook kill-emacs-hook)
         (niwa-journal-worker--processes nil)
         (niwa-transport--enabled-p t)
         (niwa-transport--pending (make-hash-table :test #'equal)))
    (unwind-protect (funcall function workspace)
      (niwa-journal-worker-stop-all)
      (niwa-transport-cancel-pending)
      (accept-process-output nil .01)
      (delete-directory root t))))

(niwa-deftest niwa-journal-worker-start/launcher ()
  ,test
  (test)
  :doc "completion is idle, once-only, outside the sentinel, and reentrant"
  (niwa-test-launcher--with-workspace
   (lambda (workspace)
     (let ((spawn (symbol-function 'make-process)) inside returned child (calls 0))
       (cl-letf (((symbol-function 'make-process)
                  (lambda (&rest args)
                    (let ((sentinel (plist-get args :sentinel)))
                      (apply spawn
                             (plist-put args :sentinel
                                        (lambda (process event)
                                          (setq inside t)
                                          (unwind-protect
                                              (progn (funcall sentinel process event)
                                                     (funcall sentinel process event))
                                            (setq inside nil)))))))))
         (let ((niwa-transport--depth 1))
           (setq child
                 (niwa-journal-worker-start
                  workspace 'recovery nil
                  (lambda (result)
                    (should returned)
                    (should-not inside)
                    (should (plist-get result :ok))
                    (cl-incf calls)
                    (niwa-journal-worker-start
                     workspace 'generation-observations
                     (list :directory (niwa-workspace-root workspace) :generations nil)
                     (lambda (reply) (should (equal reply '(:ok t :observations nil))) (cl-incf calls))))))
           (should (zerop calls))
           (setq returned t)
           (niwa-test-launcher--wait (lambda () (not (process-live-p child))))
           (should (zerop calls))))
       (niwa-test-launcher--wait (lambda () (= calls 2)))
       (accept-process-output nil .01)
       (should (= calls 2))
       (should-not niwa-journal-worker--processes))))
  :doc "unknown operations and launch failure signal without a callback"
  (niwa-test-launcher--with-workspace
   (lambda (workspace)
     (let ((temporary-file-directory (niwa-workspace-root workspace)) called)
       (should-error (niwa-journal-worker-start workspace 'eval nil (lambda (_) (setq called t))))
       (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "Injected launch failure"))))
         (should-error (niwa-journal-worker-start workspace 'recovery nil (lambda (_) (setq called t)))))
       (accept-process-output nil .01)
       (should-not called)
       (should-not niwa-journal-worker--processes)
       (should-not (directory-files temporary-file-directory nil "^[^.]")))))
  :doc "transport cancellation and disabled transport still deliver the stored result"
  (dolist (enabled '(t nil))
    (niwa-test-launcher--with-workspace
     (lambda (workspace)
       (let ((niwa-transport--enabled-p enabled) child result (calls 0))
         (niwa-transport-call-as-remote-operation
          (lambda ()
           (setq child (niwa-journal-worker-start
                        workspace 'recovery nil
                        (lambda (reply) (setq result reply) (cl-incf calls))))
           (when enabled
             (niwa-test-launcher--wait (lambda () (gethash child niwa-transport--pending)))
             (niwa-transport-cancel-pending child)
             (should-not result))))
         (niwa-test-launcher--wait (lambda () result))
         (should (plist-get result :ok))
         (should (= calls 1))))))
  :doc "cancelling a live child or queued completion drops late delivery"
  (dolist (queued '(nil t))
    (dolist (all '(nil t))
      (niwa-test-launcher--with-workspace
       (lambda (workspace)
         (let (child directory called)
           (niwa-transport-call-as-remote-operation
            (lambda ()
              (setq child (niwa-journal-worker-start workspace 'recovery nil (lambda (_) (setq called t)))
                    directory (process-get child 'niwa-journal-worker-directory))
              (if queued
                  (niwa-test-launcher--wait (lambda () (gethash child niwa-transport--pending)))
                (should (process-live-p child)))
              (if all (niwa-journal-worker-stop-all) (niwa-journal-worker-cancel child))
              (should-not (file-exists-p directory))
              (should-not niwa-journal-worker--processes)))
           (niwa-test-launcher--wait (lambda () (zerop (hash-table-count niwa-transport--pending))))
           (accept-process-output nil .02)
           (should-not called))))))
  :doc "closed child requests reject extra fields and non-table callables"
  (dolist (fault '(:function :load-path :operation))
    (niwa-test-launcher--with-workspace
     (lambda (workspace)
       (let ((spawn (symbol-function 'make-process)) result injected
             (marker (file-name-concat (niwa-workspace-root workspace) "executed")))
         (cl-letf (((symbol-function 'make-process)
                    (lambda (&rest args)
                      (let* ((entry (read (car (last (plist-get args :command)))))
                             (request (nth 1 entry))
                             (options (with-temp-buffer (insert-file-contents request) (read (current-buffer)))))
                        (setq injected t)
                        (setq options
                              (plist-put options fault
                                         (if (eq fault :operation) 'write-region
                                           `(lambda (&rest _) (write-region "executed" nil ,marker)))))
                        (with-temp-file request (prin1 options (current-buffer))))
                      (apply spawn args))))
           (niwa-journal-worker-start workspace 'recovery nil (lambda (reply) (setq result reply))))
         (niwa-test-launcher--wait (lambda () result))
         (should injected)
         (should (string-match-p (if (eq fault :operation) "Unknown journal worker operation" "Unsupported journal worker request field")
                                 (plist-get result :error)))
         (should-not (file-exists-p marker))))))
  :doc "oversized replies, nonzero exits and unsolicited SIGTERM report deferred errors once"
  (dolist (fault '(oversize exit signal))
    (niwa-test-launcher--with-workspace
     (lambda (workspace)
       (let ((spawn (symbol-function 'make-process)) result injected inside returned child (calls 0))
         (cl-letf (((symbol-function 'make-process)
                    (lambda (&rest args)
                      (let* ((command (plist-get args :command))
                             (reply (nth 2 (read (car (last command)))))
                             (sentinel (plist-get args :sentinel))
                             (program (pcase fault
                                        ('exit '(kill-emacs 17))
                                        (_ `(with-temp-file ,reply (insert (make-string 4097 ?x)))))))
                        (setq injected t)
                        (setq args (plist-put args :sentinel
                                              (lambda (process event)
                                                (setq inside t)
                                                (unwind-protect
                                                    (progn (funcall sentinel process event)
                                                           (funcall sentinel process event))
                                                  (setq inside nil)))))
                        (apply spawn (plist-put args :command
                                                (if (eq fault 'signal) '("sh" "-c" "kill -TERM $$")
                                                  (list (car command) "--batch" "-Q" "--eval" (prin1-to-string program)))))))))
           (niwa-transport-call-as-remote-operation
            (lambda ()
              (setq child (niwa-journal-worker-start
                           workspace 'recovery nil
                           (lambda (reply)
                             (should returned)
                             (should-not inside)
                             (setq result reply)
                             (cl-incf calls)))
                    returned t)
              (niwa-test-launcher--wait (lambda () (not (process-live-p child))))
              (should (eq (process-status child) (if (eq fault 'signal) 'signal 'exit)))
              (should (zerop calls)))))
         (niwa-test-launcher--wait (lambda () result))
         (should injected)
         (should (string-match-p "Journal worker exited with status" (plist-get result :error)))
         (should-not (file-exists-p (process-get child 'niwa-journal-worker-directory)))
         (should-not niwa-journal-worker--processes)
         (accept-process-output nil .02)
         (should (= calls 1))))))
  :doc "display scanning and cancellation do not settle a mutation claim"
  (niwa-test-launcher--with-workspace
   (lambda (workspace)
     (let* ((root (niwa-workspace-root workspace))
            (claim (niwa-journal-claim-acquire (file-name-concat root "mutation") 30))
            (payload (list :client "launcher-test" :roots nil)) result child)
       (niwa-workspace-identity-ensure root)
       (unwind-protect
           (progn
             (setq child (niwa-journal-worker-start workspace 'memory-display payload (lambda (reply) (setq result reply))))
             (niwa-test-launcher--wait (lambda () result))
             (should (plist-member result :rows))
             (should-not (plist-get result :error))
             (should (niwa-journal-claim-owned-p claim))
             (setq child (niwa-journal-worker-start workspace 'memory-display payload #'ignore))
             (niwa-journal-worker-cancel child)
             (niwa-test-launcher--wait (lambda () (not niwa-journal-worker--processes)))
             (should (niwa-journal-claim-owned-p claim)))
         (niwa-journal-claim-settle claim 'cancelled ""))))))

(niwa-deftest niwa-journal-process-schedule/launcher-retirement ()
  ,test
  (test)
  :doc "failed or explicitly cancelled children release scheduling for a new opportunity"
  (dolist (ending '(signal exit cancel-live cancel-queued))
    (niwa-test-launcher--with-workspace
     (lambda (workspace)
       (let ((niwa-journal-enabled t)
             (niwa-journal-process--inhibit-scheduling nil)
             (niwa-journal-process--workers (make-hash-table :test #'equal))
             (niwa-journal-process--running (make-hash-table :test #'equal))
             (niwa-journal-process--pending (make-hash-table :test #'equal))
             (niwa-journal-process--recover-requested (make-hash-table :test #'equal))
             (spawn (symbol-function 'make-process)) (launches 0) child directory warning)
         (unwind-protect
             (progn
               (niwa-test--with-captured-diagnostics warning
                 (cl-letf (((symbol-function 'make-process)
                            (lambda (&rest args)
                              (cl-incf launches)
                              (when (= launches 1)
                                (setq args
                                      (plist-put args :command
                                                 (if (eq ending 'signal) '("sh" "-c" "kill -TERM $$")
                                                   (list (car (plist-get args :command)) "--batch" "-Q" "--eval"
                                                       (prin1-to-string
                                                        (pcase ending
                                                          ('cancel-live '(sleep-for 20))
                                                          (_ '(kill-emacs 17)))))))))
                              (let ((process (apply spawn args)))
                                (when (= launches 1) (setq child process))
                                process))))
                   (niwa-journal-process-schedule workspace)
                   (niwa-test-launcher--wait (lambda () (= launches 1)))
                   (setq directory (process-get child 'niwa-journal-worker-directory))
                   (when (memq ending '(cancel-live cancel-queued))
                     (niwa-transport-call-as-remote-operation
                      (lambda ()
                        (if (eq ending 'cancel-queued)
                            (niwa-test-launcher--wait (lambda () (gethash child niwa-transport--pending)))
                          (should (process-live-p child)))
                        (niwa-journal-process-stop-all)
                        (should-not (file-exists-p directory)))))
                   (niwa-test-launcher--wait
                    (lambda () (and (zerop (hash-table-count niwa-journal-process--workers))
                                    (not niwa-journal-worker--processes))))
                   (should-not (file-exists-p directory))
                   (when (eq ending 'signal) (should (eq (process-status child) 'signal)))
                   (niwa-journal-process-schedule workspace)
                   (niwa-test-launcher--wait
                    (lambda () (and (= launches 2)
                                    (zerop (hash-table-count niwa-journal-process--workers))
                                    (not niwa-journal-worker--processes))))))
               (if (memq ending '(signal exit))
                   (should (equal warning
                                  (format "niwa [%s]: Journal worker exited with status %s\n"
                                          (niwa-workspace-root workspace)
                                          (if (eq ending 'signal) 15 17))))
                 (should (string-empty-p warning))))
           (niwa-journal-process-stop-all)))))))

(niwa-deftest niwa--emacs-executable ()
  ,test
  (test)
  :doc "the running executable bypasses a launcher, with a portable fallback"
  (let* ((default-directory "/ssh:unreachable:/")
         (native-comp-enable-subr-trampolines nil)
         (invocation-directory temporary-file-directory)
         (invocation-name "emacs-launcher")
         (symlink (symbol-function 'file-symlink-p)))
    (when (eq system-type 'gnu/linux)
      (should (equal (niwa--emacs-executable)
                     (file-symlink-p (format "/proc/%s/exe" (emacs-pid))))))
    (dolist (fault '(missing inaccessible stale))
      (cl-letf (((symbol-function 'file-symlink-p)
                 (lambda (path)
                   (if (string-prefix-p "/proc/" path)
                       (pcase fault
                         ('inaccessible (signal 'file-error '("Denied")))
                         ('stale "/missing-emacs-binary")
                         (_ nil))
                     (funcall symlink path)))))
        (should (equal (niwa--emacs-executable)
                       (expand-file-name invocation-name invocation-directory)))))))

(niwa-deftest niwa-journal-worker-cancel/non-exec-launcher ()
  ,test
  (test)
  :doc "journal cancellation and Fresh Eval stop quiesce descendants behind the same waiting launcher"
  (niwa-test-launcher--with-workspace
   (lambda (workspace)
     (let* ((default-directory (file-name-as-directory (niwa-workspace-root workspace)))
            (temporary-file-directory default-directory)
            (emacs (expand-file-name invocation-name invocation-directory))
            (launcher (file-name-concat default-directory "emacs-launcher"))
            (launched (file-name-concat default-directory "launched"))
            ;; A native trampoline build can launch Emacs via the invocation
            ;; variables independently of the worker or Fresh Eval under test.
            (native-comp-enable-subr-trampolines nil)
            (invocation-directory default-directory)
            (invocation-name "emacs-launcher")
            (niwa--session (niwa-session--create
                              :name "launcher" :working-directory default-directory
                              :workspace workspace))
            (niwa-sandbox-mode 'off)
            (niwa-execution-mailbox-delivery-function nil)
            (spawn (symbol-function 'make-process))
            control)
       (with-temp-file launcher
         (insert (format "#!/bin/sh\nprintf launched >%s\n%s \"$@\"\nstatus=$?\nexit \"$status\"\n"
                         (shell-quote-argument launched) (shell-quote-argument emacs))))
       (set-file-modes launcher #o700)
       (unwind-protect
           (progn
             ;; The control proves this is a waiting shell, not an exec wrapper.
             (setq control (make-process :name "launcher-control" :noquery t
                                         :command (list launcher "-Q" "--batch" "--eval" "(sleep-for 30)")))
             (niwa-test-launcher--wait (lambda () (file-exists-p launched)))
             (niwa-test-launcher--wait
              (lambda () (cl-some (lambda (pid)
                                   (eql (process-id control) (alist-get 'ppid (process-attributes pid))))
                                 (list-system-processes))))
             (signal-process (- (process-id control)) 'SIGKILL)
             (delete-process control)
             (delete-file launched)
             (should-not (file-exists-p launched))
             (dolist (kind '(journal fresh))
               (let* ((marker (file-name-concat default-directory (symbol-name kind)))
                      (late (concat marker "-late"))
                      (shell-command (format "echo $$ >%s; sleep 30; printf late >%s"
                                             (shell-quote-argument marker) (shell-quote-argument late)))
                      (form `(process-file "sh" nil nil nil "-c" ,shell-command))
                      child shell result stopped)
                 (unwind-protect
                     (progn
                       (if (eq kind 'journal)
                           (cl-letf (((symbol-function 'make-process)
                                      (lambda (&rest args)
                                        (let* ((command (copy-sequence (plist-get args :command)))
                                               (run (read (car (last command)))))
                                          (setcar (last command) (prin1-to-string `(progn ,form ,run)))
                                          (apply spawn (plist-put args :command command))))))
                             (setq child (niwa-journal-worker-start
                                          workspace 'recovery nil (lambda (reply) (setq result reply)))))
                         (niwa-tool-exec--eval
                          (lambda (reply) (setq result reply))
                          (list :expression (prin1-to-string form) :mode "fresh" :yield_time_ms 250)))
                       (niwa-test-launcher--wait (lambda () (file-exists-p marker)))
                       (setq shell (with-temp-buffer (insert-file-contents marker)
                                                     (string-to-number (buffer-string))))
                       (should (> shell 0))
                       (ert-info ((format "%s used invocation launcher" kind))
                         (should-not (file-exists-p launched)))
                       (if child
                           (should (niwa-journal-worker-cancel child))
                         (niwa-test-launcher--wait (lambda () result))
                         (niwa-tool-exec--stop-execution
                          (lambda (reply) (setq stopped reply))
                          (list :execution_id (plist-get (plist-get result :render-data) :execution-id)))
                         (niwa-test-launcher--wait (lambda () stopped))
                         (should (eq 'stopped (plist-get (plist-get stopped :render-data) :termination))))
                       (should-not (cl-some
                                    (lambda (pid)
                                      (let ((attrs (process-attributes pid)))
                                        (and (eql shell (alist-get 'pgrp attrs))
                                             (not (equal "Z" (alist-get 'state attrs))))))
                                    (list-system-processes)))
                       (should-not (file-exists-p late))
                       (when child (should-not result)))
                   (when shell (ignore-errors (signal-process (- shell) 'SIGKILL)))
                   (when child
                     (ignore-errors (signal-process (- (process-id child)) 'SIGKILL))
                     (niwa-journal-worker-cancel child))))))
         (when (and control (process-live-p control))
           (signal-process (- (process-id control)) 'SIGKILL)
           (delete-process control))
         (niwa-execution-teardown-session niwa--session))))))

(provide 'test-niwa-journal-worker-launcher)
;;; test-niwa-journal-worker-launcher.el ends here
