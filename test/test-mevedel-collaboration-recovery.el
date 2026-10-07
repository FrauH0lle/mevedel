;;; test-mevedel-collaboration-recovery.el --- Recovery authority -*- lexical-binding: t -*-
;;; Commentary:
;; Login and repair frames have the same explicit owner boundary.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration-recovery)
(require 'mevedel-collaboration)
(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "mevedel-engine-test-support"))

(mevedel-deftest mevedel-collaboration-recovery-handle (:quiet t)
  ,test
  (test)
  :doc "ordinary guests cannot change recovery state"
  (let* ((guests (make-hash-table :test #'eql))
	 (room (list :guests guests)))
    (puthash 1 '(:writable t :owner nil) guests)
    (dolist (action '("model" "preset" "history" "retry" "login" "login-code" "cancel-login" "update" "input-discard"))
      (should-error (mevedel-collaboration-recovery-handle room 1 (list :action action)) :type 'user-error)))
  :doc "owner requeue discards failed live preparation and keeps input provenance"
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((submission (mevedel-prompt-submission-create :input "fresh" :session session :state 'reserved))
           (entry (list :id 1 :category 'steering :state 'failed-turn :input "fresh"
                        :guest-name "Owner" :request-id "old" :submission submission
                        :request-context '(:old t)))
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer buffer :guests guests)))
      (puthash 1 '(:owner t :writable t) guests)
      (setf (mevedel-session-pending-steering session) (list entry)
            (mevedel-session-pending-input-next-id session) 1
            (mevedel-session-pending-input-failure-paused session) t)
      (cl-letf (((symbol-function 'mevedel-collaboration--publish-status) #'ignore)
                ((symbol-function 'mevedel-collaboration--publish-queue) #'ignore)
                ((symbol-function 'mevedel-collaboration-recovery-send) #'ignore))
        (mevedel-collaboration-recovery-handle room 1 '(:action "input-requeue" :value 1)))
      (let ((queued (car (mevedel-session-pending-follow-ups session))))
        (should (equal "fresh" (plist-get queued :input)))
        (should (equal "Owner" (plist-get queued :guest-name)))
        (should-not (plist-get queued :submission))
        (should-not (plist-get queued :request-context))
        (should-not (plist-get queued :request-id))
        (should-not (mevedel-session-pending-steering session))
        (should (mevedel-session-pending-input-failure-paused session))))))

(mevedel-deftest mevedel-collaboration-recovery-send ()
  (let ((room (list :guests (make-hash-table :test #'eql))) sent)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
	       (lambda (&rest _) (setq sent t))))
      (mevedel-collaboration-recovery-send room 1)
      (should-not sent))))

(provide 'test-mevedel-collaboration-recovery)
;;; test-mevedel-collaboration-recovery.el ends here
