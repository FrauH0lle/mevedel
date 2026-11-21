;;; test-niwa-journal-capture-worker.el --- Prepared checkpoints -*- lexical-binding: t -*-

;;; Commentary:
;; Real child preparation must not acquire authority or publish stale snapshots.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))

(niwa-deftest niwa-journal-capture-checkpoint-start ()
  ,test
  (test)
  :doc "prepares saved evidence in a child and commits only in the owner"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Question" "Frozen answer"))
     (let (result completed cancel timer tick)
       (unwind-protect
           (progn
             (setq cancel (niwa-journal-capture-checkpoint-start
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
             (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
             (should (equal (plist-get result :id)
                            (plist-get (niwa-journal-capture-checkpoint session buffer) :id))))
         (when timer (cancel-timer timer))
         (when cancel (funcall cancel))))))

  :doc "a newer publication restarts preparation against the current head"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Question" "Answer"))
     (let (result completed cancel)
       (unwind-protect
           (progn
             (setq cancel (niwa-journal-capture-checkpoint-start
                           session buffer (lambda (value) (setq result value completed t))))
             (setf (niwa-session-name session) "Changed title")
             (niwa-session-artifacts-save session buffer t)
             (let ((until (+ (float-time) 15)))
               (while (and (not completed) (< (float-time) until))
                 (accept-process-output nil .01)))
             (should completed)
             (should-not (plist-get result :error))
             (should (equal "Changed title"
                            (plist-get (plist-get result :metadata) :session-name)))
             (should (equal (plist-get (niwa-session-publication session) :head)
                            (plist-get result :head)))
             (should (niwa-journal-pins-present-p (niwa-session-save-path session))))
         (when cancel (funcall cancel))))))

  :doc "repeated supersession yields nil without an error, capture or pin"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Question" "Answer"))
     (let (result completed cancel (saves 0))
       (unwind-protect
           (progn
             (setq cancel (niwa-journal-capture-checkpoint-start
                           session buffer (lambda (value) (setq result value completed t))))
             (let ((until (+ (float-time) 30)))
               ;; Keep publishing so both preparation attempts are superseded.
               (while (and (not completed) (< (float-time) until))
                 (setf (niwa-session-name session) (format "Title %d" (cl-incf saves)))
                 (niwa-session-artifacts-save session buffer t)
                 (accept-process-output nil .05)))
             (should completed)
             (should-not result)
             (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
             (should-not (niwa-journal-capture-list (niwa-session-workspace session))))
         (when cancel (funcall cancel))))))

  :doc "a replacement request rejects otherwise current prepared evidence"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Question" "Answer"))
     (let (result completed cancel)
       (unwind-protect
           (progn
             (setq cancel (niwa-journal-capture-checkpoint-start
                           session buffer (lambda (value) (setq result value completed t))))
             (with-current-buffer buffer
               (setq-local niwa--current-request (niwa-request--create :id "replacement")))
             (let ((until (+ (float-time) 15)))
               (while (and (not completed) (< (float-time) until)) (accept-process-output nil .01)))
             (should completed)
             (should (plist-get result :error))
             (should-not (niwa-journal-pins-present-p (niwa-session-save-path session))))
         (when cancel (funcall cancel))
         (with-current-buffer buffer (setq-local niwa--current-request nil))))))

  :doc "cancelled preparation cannot publish through a late child sentinel"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Question" "Answer"))
     (let ((processes niwa-journal-worker--processes) callback)
       (funcall (niwa-journal-capture-checkpoint-start
                 session buffer (lambda (_) (setq callback t))))
       (accept-process-output nil .05)
       (should-not callback)
       (should (equal processes niwa-journal-worker--processes))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
       (should-not (niwa-journal-capture-list (niwa-session-workspace session)))))))

;;; test-niwa-journal-capture-worker.el ends here
