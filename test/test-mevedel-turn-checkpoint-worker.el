;;; test-mevedel-turn-checkpoint-worker.el --- Suspended settlement -*- lexical-binding: t -*-

;;; Commentary:
;; A pending storage step keeps admission held without polling or running hooks.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-turn)

(mevedel-deftest mevedel--defer-turn-steps
  (:vars ((mevedel-transport--enabled-p t)))
  ,test
  (test)
  :doc "a pending step resumes once before later settlement steps"
  (let* ((buffer (generate-new-buffer " *pending-settlement*"))
         (fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-request-id "pending")))
         ready ended resume (calls 0))
    (unwind-protect
        (progn
          (mevedel--defer-turn-steps
           fsm (list (lambda (_)
                       (cl-incf calls)
                       (unless ready
                         (setq resume mevedel--turn-resume)
                         'mevedel-turn-pending))
                     (lambda (_) (setq ended t))))
          (let ((until (+ (float-time) 2)))
            (while (and (not resume) (< (float-time) until)) (accept-process-output nil .01)))
          (should (functionp resume))
          (should-not ended)
          (should (= 1 (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
          (accept-process-output nil .03)
          (should (= calls 1))
          (setq ready t)
          (funcall resume)
          (let ((until (+ (float-time) 2)))
            (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01)))
          (should ended)
          (should (= 2 calls))
          (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
          (funcall resume)
          (accept-process-output nil .02)
          (should (= 2 calls)))
      (when (buffer-live-p buffer) (kill-buffer buffer))))

  :doc "source death cancels pending checkpoint work and rejects a late resume"
  (let* ((buffer (generate-new-buffer " *cancel-pending-settlement*"))
         (fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-request-id "cancel")))
         resume cancelled ended)
    (unwind-protect
        (progn
          (mevedel--defer-turn-steps
           fsm (list (lambda (_)
                       (setq resume mevedel--turn-resume)
                       (setf (gptel-fsm-info fsm)
                             (plist-put (gptel-fsm-info fsm) :mevedel-checkpoint-cancel
                                        (lambda () (setq cancelled t))))
                       'mevedel-turn-pending)
                     (lambda (_) (setq ended t))))
          (let ((until (+ (float-time) 2)))
            (while (and (not resume) (< (float-time) until)) (accept-process-output nil .01)))
          (should resume)
          (kill-buffer buffer)
          (should cancelled)
          (funcall resume)
          (accept-process-output nil .03)
          (should-not ended)
          (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(mevedel-deftest mevedel--turn-checkpoint
  (:vars ((mevedel-transport--enabled-p t)))
  ,test
  (test)
  :doc "real checkpoint preparation holds admission until evidence is pinned"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((mevedel-journal-enabled nil))
       (mevedel-test-journal-capture--turn session buffer "Question" "Completed answer"))
     (let* ((fsm (gptel-make-fsm :info (list :buffer buffer :mevedel-turn-saved t
                                           :mevedel-request-id "checkpoint")))
            ended pinned)
       (mevedel--defer-turn-steps
        fsm (list #'mevedel--turn-checkpoint
                  (lambda (_)
                    (setq pinned (mevedel-journal-pins-present-p (mevedel-session-save-path session))
                          ended t))))
       (let ((until (+ (float-time) 15)))
         (while (and (not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-state))
                     (< (float-time) until))
           (accept-process-output nil .005)))
       (should (eq 'waiting (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-state)))
       (should-not ended)
       (should (= 1 (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))
       (let ((until (+ (float-time) 15)))
         (while (and (not ended) (< (float-time) until)) (accept-process-output nil .01)))
       (should ended)
       (should pinned)
       (should-not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-error))
       (should-not (plist-get (gptel-fsm-info fsm) :mevedel-checkpoint-cancel))
       (should (zerop (plist-get (gptel-fsm-info fsm) :mevedel-settlement-holds)))))))

;;; test-mevedel-turn-checkpoint-worker.el ends here
