;;; test-mevedel-journal-capture-worker.el --- Prepared checkpoints -*- lexical-binding: t -*-

;;; Commentary:
;; Real child preparation must not acquire authority or publish stale snapshots.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))

(mevedel-deftest mevedel-journal-capture-checkpoint-start ()
  ,test
  (test)
  :doc "prepares saved evidence in a child and commits only in the owner"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "Question" "Frozen answer"))
     (let (result completed cancel timer tick)
       (unwind-protect
           (progn
             (setq cancel (mevedel-journal-capture-checkpoint-start
                           session buffer (lambda (value) (setq result value completed t)))
                   timer (run-at-time 0 nil (lambda () (setq tick t))))
             (should-not completed)
             (let ((until (+ (float-time) 15)))
               (while (and (not completed) (< (float-time) until))
                 (accept-process-output nil .01)))
             (should completed)
             (should tick)
             (should-not (plist-get result :error))
             (should (plist-get result :id))
             (should (string-match-p "Frozen answer" (plist-get result :evidence)))
             (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
             (should (equal (plist-get result :id)
                            (plist-get (mevedel-journal-capture-checkpoint session buffer) :id))))
         (when timer (cancel-timer timer))
         (when cancel (funcall cancel))))))

  :doc "a changed saved publication rejects the prepared checkpoint without a pin"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "Question" "Answer"))
     (let (result completed cancel)
       (unwind-protect
           (progn
             (setq cancel (mevedel-journal-capture-checkpoint-start
                           session buffer (lambda (value) (setq result value completed t))))
             (setf (mevedel-session-name session) "Changed title")
             (mevedel-session-artifacts-save session buffer t)
             (let ((until (+ (float-time) 15)))
               (while (and (not completed) (< (float-time) until))
                 (accept-process-output nil .01)))
             (should completed)
             (should (plist-get result :error))
             (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
             (should-not (mevedel-journal-capture-list (mevedel-session-workspace session))))
         (when cancel (funcall cancel))))))

  :doc "a replacement request rejects otherwise current prepared evidence"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "Question" "Answer"))
     (let (result completed cancel)
       (unwind-protect
           (progn
             (setq cancel (mevedel-journal-capture-checkpoint-start
                           session buffer (lambda (value) (setq result value completed t))))
             (with-current-buffer buffer
               (setq-local mevedel--current-request (mevedel-request--create :id "replacement")))
             (let ((until (+ (float-time) 15)))
               (while (and (not completed) (< (float-time) until)) (accept-process-output nil .01)))
             (should completed)
             (should (plist-get result :error))
             (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
         (when cancel (funcall cancel))
         (with-current-buffer buffer (setq-local mevedel--current-request nil))))))

  :doc "cancelled preparation cannot publish through a late child sentinel"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "Question" "Answer"))
     (let ((processes mevedel-journal-worker--processes) callback)
       (funcall (mevedel-journal-capture-checkpoint-start
                 session buffer (lambda (_) (setq callback t))))
       (accept-process-output nil .05)
       (should-not callback)
       (should (equal processes mevedel-journal-worker--processes))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
       (should-not (mevedel-journal-capture-list (mevedel-session-workspace session)))))))

;;; test-mevedel-journal-capture-worker.el ends here
