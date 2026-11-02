;;; test-mevedel-journal-worker-launcher.el --- Closed launcher contract -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the public launcher with real children.  Faults alter the process
;; boundary, not owner implementation functions or the operation table.

;;; Code:
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-worker)
(require 'mevedel-journal-process)
(require 'mevedel-journal-claim)
(require 'mevedel-workspace-identity)
(require 'mevedel-transport)

(defun mevedel-test-launcher--wait (predicate)
  "Wait boundedly for PREDICATE, allowing process and transport delivery."
  (let ((until (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) until))
      (accept-process-output nil .005))
    (should (funcall predicate))))

(defun mevedel-test-launcher--with-workspace (function)
  "Call FUNCTION with an isolated workspace and local worker lifetime."
  (let* ((root (make-temp-file "mevedel-launcher-" t))
         (workspace (mevedel-workspace--create :root root :id root :type 'project))
         (kill-emacs-hook kill-emacs-hook)
         (mevedel-journal-worker--processes nil)
         (mevedel-transport--enabled-p t)
         (mevedel-transport--pending (make-hash-table :test #'equal)))
    (unwind-protect (funcall function workspace)
      (mevedel-journal-worker-stop-all)
      (mevedel-transport-cancel-pending)
      (accept-process-output nil .01)
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-worker-start/launcher ()
  ,test
  (test)
  :doc "completion is idle, once-only, outside the sentinel, and reentrant"
  (mevedel-test-launcher--with-workspace
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
         (let ((mevedel-transport--depth 1))
           (setq child
                 (mevedel-journal-worker-start
                  workspace 'recovery nil
                  (lambda (result)
                    (should returned)
                    (should-not inside)
                    (should (plist-get result :ok))
                    (cl-incf calls)
                    (mevedel-journal-worker-start
                     workspace 'generation-observations
                     (list :directory (mevedel-workspace-root workspace) :generations nil)
                     (lambda (reply) (should (equal reply '(:ok t :observations nil))) (cl-incf calls))))))
           (should (zerop calls))
           (setq returned t)
           (mevedel-test-launcher--wait (lambda () (not (process-live-p child))))
           (should (zerop calls))))
       (mevedel-test-launcher--wait (lambda () (= calls 2)))
       (accept-process-output nil .01)
       (should (= calls 2))
       (should-not mevedel-journal-worker--processes))))
  :doc "unknown operations and launch failure signal without a callback"
  (mevedel-test-launcher--with-workspace
   (lambda (workspace)
     (let ((temporary-file-directory (mevedel-workspace-root workspace)) called)
       (should-error (mevedel-journal-worker-start workspace 'eval nil (lambda (_) (setq called t))))
       (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "Injected launch failure"))))
         (should-error (mevedel-journal-worker-start workspace 'recovery nil (lambda (_) (setq called t)))))
       (accept-process-output nil .01)
       (should-not called)
       (should-not mevedel-journal-worker--processes)
       (should-not (directory-files temporary-file-directory nil "^[^.]")))))
  :doc "transport cancellation and disabled transport still deliver the stored result"
  (dolist (enabled '(t nil))
    (mevedel-test-launcher--with-workspace
     (lambda (workspace)
       (let ((mevedel-transport--enabled-p enabled) child result (calls 0))
         (mevedel-transport-call-as-remote-operation
          (lambda ()
           (setq child (mevedel-journal-worker-start
                        workspace 'recovery nil
                        (lambda (reply) (setq result reply) (cl-incf calls))))
           (when enabled
             (mevedel-test-launcher--wait (lambda () (gethash child mevedel-transport--pending)))
             (mevedel-transport-cancel-pending child)
             (should-not result))))
         (mevedel-test-launcher--wait (lambda () result))
         (should (plist-get result :ok))
         (should (= calls 1))))))
  :doc "cancelling a live child or queued completion drops late delivery"
  (dolist (queued '(nil t))
    (dolist (all '(nil t))
      (mevedel-test-launcher--with-workspace
       (lambda (workspace)
         (let (child directory called)
           (mevedel-transport-call-as-remote-operation
            (lambda ()
              (setq child (mevedel-journal-worker-start workspace 'recovery nil (lambda (_) (setq called t)))
                    directory (process-get child 'mevedel-journal-worker-directory))
              (if queued
                  (mevedel-test-launcher--wait (lambda () (gethash child mevedel-transport--pending)))
                (should (process-live-p child)))
              (if all (mevedel-journal-worker-stop-all) (mevedel-journal-worker-cancel child))
              (should-not (file-exists-p directory))
              (should-not mevedel-journal-worker--processes)))
           (mevedel-test-launcher--wait (lambda () (zerop (hash-table-count mevedel-transport--pending))))
           (accept-process-output nil .02)
           (should-not called))))))
  :doc "closed child requests reject extra fields and non-table callables"
  (dolist (fault '(:function :load-path :operation))
    (mevedel-test-launcher--with-workspace
     (lambda (workspace)
       (let ((spawn (symbol-function 'make-process)) result injected
             (marker (file-name-concat (mevedel-workspace-root workspace) "executed")))
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
           (mevedel-journal-worker-start workspace 'recovery nil (lambda (reply) (setq result reply))))
         (mevedel-test-launcher--wait (lambda () result))
         (should injected)
         (should (string-match-p (if (eq fault :operation) "Unknown journal worker operation" "Unsupported journal worker request field")
                                 (plist-get result :error)))
         (should-not (file-exists-p marker))))))
  :doc "oversized replies, nonzero exits and unsolicited SIGTERM report deferred errors once"
  (dolist (fault '(oversize exit signal))
    (mevedel-test-launcher--with-workspace
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
           (mevedel-transport-call-as-remote-operation
            (lambda ()
              (setq child (mevedel-journal-worker-start
                           workspace 'recovery nil
                           (lambda (reply)
                             (should returned)
                             (should-not inside)
                             (setq result reply)
                             (cl-incf calls)))
                    returned t)
              (mevedel-test-launcher--wait (lambda () (not (process-live-p child))))
              (should (eq (process-status child) (if (eq fault 'signal) 'signal 'exit)))
              (should (zerop calls)))))
         (mevedel-test-launcher--wait (lambda () result))
         (should injected)
         (should (string-match-p "Journal worker exited with status" (plist-get result :error)))
         (should-not (file-exists-p (process-get child 'mevedel-journal-worker-directory)))
         (should-not mevedel-journal-worker--processes)
         (accept-process-output nil .02)
         (should (= calls 1))))))
  :doc "display scanning and cancellation do not settle a mutation claim"
  (mevedel-test-launcher--with-workspace
   (lambda (workspace)
     (let* ((root (mevedel-workspace-root workspace))
            (claim (mevedel-journal-claim-acquire (file-name-concat root "mutation") 30))
            (payload (list :client "launcher-test" :roots nil)) result child)
       (mevedel-workspace-identity-ensure root)
       (unwind-protect
           (progn
             (setq child (mevedel-journal-worker-start workspace 'memory-display payload (lambda (reply) (setq result reply))))
             (mevedel-test-launcher--wait (lambda () result))
             (should (plist-member result :rows))
             (should-not (plist-get result :error))
             (should (mevedel-journal-claim-owned-p claim))
             (setq child (mevedel-journal-worker-start workspace 'memory-display payload #'ignore))
             (mevedel-journal-worker-cancel child)
             (mevedel-test-launcher--wait (lambda () (not mevedel-journal-worker--processes)))
             (should (mevedel-journal-claim-owned-p claim)))
         (mevedel-journal-claim-settle claim 'cancelled ""))))))

(mevedel-deftest mevedel-journal-process-schedule/launcher-retirement ()
  ,test
  (test)
  :doc "failed or explicitly cancelled children release scheduling for a new opportunity"
  (dolist (ending '(signal exit cancel-live cancel-queued))
    (mevedel-test-launcher--with-workspace
     (lambda (workspace)
       (let ((mevedel-journal-enabled t)
             (mevedel-journal-process--inhibit-scheduling nil)
             (mevedel-journal-process--workers (make-hash-table :test #'equal))
             (mevedel-journal-process--running (make-hash-table :test #'equal))
             (mevedel-journal-process--pending (make-hash-table :test #'equal))
             (mevedel-journal-process--recover-requested (make-hash-table :test #'equal))
             (spawn (symbol-function 'make-process)) (launches 0) child directory warning)
         (unwind-protect
             (progn
               (mevedel-test--with-captured-diagnostics warning
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
                   (mevedel-journal-process-schedule workspace)
                   (mevedel-test-launcher--wait (lambda () (= launches 1)))
                   (setq directory (process-get child 'mevedel-journal-worker-directory))
                   (when (memq ending '(cancel-live cancel-queued))
                     (mevedel-transport-call-as-remote-operation
                      (lambda ()
                        (if (eq ending 'cancel-queued)
                            (mevedel-test-launcher--wait (lambda () (gethash child mevedel-transport--pending)))
                          (should (process-live-p child)))
                        (mevedel-journal-process-stop-all)
                        (should-not (file-exists-p directory)))))
                   (mevedel-test-launcher--wait
                    (lambda () (and (zerop (hash-table-count mevedel-journal-process--workers))
                                    (not mevedel-journal-worker--processes))))
                   (should-not (file-exists-p directory))
                   (when (eq ending 'signal) (should (eq (process-status child) 'signal)))
                   (mevedel-journal-process-schedule workspace)
                   (mevedel-test-launcher--wait
                    (lambda () (and (= launches 2)
                                    (zerop (hash-table-count mevedel-journal-process--workers))
                                    (not mevedel-journal-worker--processes))))))
               (if (memq ending '(signal exit))
                   (should (equal warning
                                  (format "Journal worker exited with status %s\n" (if (eq ending 'signal) 15 17))))
                 (should (string-empty-p warning))))
           (mevedel-journal-process-stop-all)))))))

(provide 'test-mevedel-journal-worker-launcher)
;;; test-mevedel-journal-worker-launcher.el ends here
