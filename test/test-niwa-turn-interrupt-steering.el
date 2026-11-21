;;; test-niwa-turn-interrupt-steering.el --- Interrupted steering tests -*- lexical-binding: t -*-

;;; Commentary:

;; Distinguish deliberately interrupted steering from failed in-flight input.

;;; Code:

(require 'gptel-request)
(require 'niwa-goal)
(require 'niwa-pending-inputs)
(require 'niwa-session-codec)
(require 'niwa-turn)
(require 'niwa-transport)
(require 'niwa-view-interaction)
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))

(niwa-deftest niwa--turn-fail-pending-input/interrupt-steering
  ()
  ,test
  (test)
  :doc "abort detaches matching steering without changing content or failure state"
  (dolist (external '(nil t))
    (with-temp-buffer
      (let* ((submission (list :prepared t))
             (entry (list :id 1 :input "keep me" :request-id "old" :delivering t
                          :submission submission :guest-name "Guest"
                          :dropped-file-grants '("/tmp/image.png")))
             (other '(:id 2 :input "other" :request-id "different"))
             (follow-ups '((:id 3 :input "later")))
             (session (niwa-session--create
                       :pending-steering (list entry other)
                       :pending-follow-ups follow-ups))
             (request (niwa-request--create :id "old" :session session))
             (info (list :buffer (current-buffer) :niwa-request-id "old"
                         :niwa-terminal-status 'aborted))
             (owner (if external request (gptel-make-fsm :info info))))
        (when external (setf (niwa-engine-info owner) info))
        (setq-local niwa--session session niwa--current-request request)
        (niwa--turn-fail-pending-input owner 'aborted)
        (let ((retained (car (niwa-session-pending-steering session))))
          (should-not (plist-get retained :request-id))
          (should-not (plist-get retained :state))
          (should-not (plist-get retained :delivering))
          (should (equal "keep me" (plist-get retained :input)))
          (should (eq submission (plist-get retained :submission)))
          (should (equal "Guest" (plist-get retained :guest-name)))
          (should (equal '("/tmp/image.png") (plist-get retained :dropped-file-grants))))
        (should (eq other (cadr (niwa-session-pending-steering session))))
        (should (eq follow-ups (niwa-session-pending-follow-ups session)))
        (should (niwa-session-pending-input-paused session))
        (should-not (niwa-session-pending-input-failure-paused session)))))
  :doc "abort pauses follow-ups even without steering, preserving an older failure hold"
  (with-temp-buffer
    (let ((session (niwa-session--create
                    :pending-follow-ups '((:id 1 :input "later"))
                    :pending-input-failure-paused t)))
      (setq-local niwa--session session)
      (niwa--turn-fail-pending-input
       (gptel-make-fsm :info (list :buffer (current-buffer)
                                 :niwa-request-id "old"
                                 :niwa-terminal-status 'aborted)) 'aborted)
      (should (niwa-session-pending-input-paused session))
      (should (niwa-session-pending-input-failure-paused session))))
  :doc "a real failure retains request ownership and requires review"
  (with-temp-buffer
    (let ((session (niwa-session--create
                    :pending-steering '((:id 1 :input "review" :request-id "old")))))
      (setq-local niwa--session session)
      (niwa--turn-fail-pending-input
       (gptel-make-fsm :info (list :buffer (current-buffer)
                                 :niwa-request-id "old"
                                 :niwa-terminal-status 'aborted)) 'error)
      (let ((entry (car (niwa-session-pending-steering session))))
        (should (equal "old" (plist-get entry :request-id)))
        (should (eq 'failed-turn (plist-get entry :state))))
      (should (niwa-session-pending-input-failure-paused session)))))

(niwa-deftest niwa--fail-turn/interrupt-steering
  ()
  ,test
  (test)
  :doc "cancelled transport settlement still preserves intentional abort input"
  (with-temp-buffer
    (let* ((session (niwa-session--create
                     :pending-steering '((:id 1 :input "keep" :request-id "old"))))
           (request (niwa-request--create :id "old" :session session))
           (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                          :niwa-request-id "old"))))
      (setq-local niwa--session session niwa--current-request request)
      (cl-letf (((symbol-function 'niwa--turn-commit) #'ignore)
                ((symbol-function 'niwa--turn-end-request) #'ignore)
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (&rest _) nil)))
        (niwa--fail-turn fsm 'aborted))
      (should-not (plist-get (car (niwa-session-pending-steering session)) :request-id))
      (should-not (niwa-session-pending-input-failure-paused session))
      (should (niwa-session-pending-input-paused session)))))

(niwa-deftest niwa-session-codec-deserialize/interrupt-steering
  ()
  ,test
  (test)
  :doc "restart preserves normal detached input but reviews crashed in-flight steering"
  (dolist (in-flight '(nil t))
    (let ((root (make-temp-file "niwa-interrupt-codec-" t)))
      (unwind-protect
          (let* ((source (test-niwa-session-persistence--make-session root))
                 (entry (append '(:id 1 :category steering :input "keep")
                                (and in-flight '(:request-id "old")))))
            (setf (niwa-session-pending-steering source) (list entry)
                  (niwa-session-pending-input-paused source) (not in-flight))
            (let* ((raw (niwa-session-codec-serialize source))
                   (restored (plist-get
                              (niwa-session-codec-deserialize
                               raw (niwa-session-workspace source)) :session))
                   (retained (car (niwa-session-pending-steering restored))))
              (should (eq in-flight (eq 'failed-turn (plist-get retained :state))))
              (should (eq in-flight (niwa-session-pending-input-failure-paused restored)))
              (should (eq (not in-flight) (niwa-session-pending-input-paused restored)))))
        (delete-directory root t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-goal-continue-if-idle/interrupt-steering
  ()
  ,test
  (test)
  :doc "pending steering and an interrupted queue pause each block Goal continuation"
  (dolist (hold '(steering paused))
    (with-temp-buffer
      (let* ((goal (niwa-goal--create :id "goal" :status 'active))
             (session (niwa-session--create
                       :goal goal
                       :pending-steering (and (eq hold 'steering) '((:input "keep")))
                       :pending-input-paused (eq hold 'paused))))
        (setq-local niwa--session session)
        (should (eq 'pending-input
                    (niwa-goal-continue-if-idle session (current-buffer))))))))

(niwa-deftest niwa--turn-resume-pending-input
  ()
  ,test
  (test)
  :doc "only the actual root send rebinds normal input and releases an interruption hold"
  (dolist (scenario '(root queued-only agent foreign-root foreign-session
                          owning-cockpit other-cockpit))
    (with-temp-buffer
      (let* ((first '(:id 1 :input "first" :state pending))
             (failed '(:id 2 :input "review" :state failed-turn))
             (second '(:id 3 :input "second"))
             (live '(:id 4 :input "other request" :request-id "other"))
             (session (niwa-session--create
                       :root-buffer (unless (eq scenario 'foreign-root) (current-buffer))
                       :pending-input-paused t
                       :pending-follow-ups '((:id 5 :input "later"))
                       :pending-steering (unless (eq scenario 'queued-only)
                                           (list first failed second live))))
             (request (niwa-request--create :id "new" :session session))
             (allowed (memq scenario '(root queued-only other-cockpit)))
             (cockpit (and (memq scenario '(owning-cockpit other-cockpit))
                           (generate-new-buffer niwa-pending-inputs-buffer-name))))
        (unwind-protect
            (progn
              (setq-local niwa--session (if (eq scenario 'foreign-session)
                                               (niwa-session--create) session)
                          niwa--agent-invocation (and (eq scenario 'agent) t))
              (when cockpit
                (with-current-buffer cockpit
                  (setq-local niwa-cockpit--context
                              (list :session (if (eq scenario 'owning-cockpit)
                                                 session (niwa-session--create))))))
              (niwa--turn-resume-pending-input request)
              (should (eq (not allowed) (niwa-session-pending-input-paused session)))
              (unless (eq scenario 'queued-only)
                (let ((entries (niwa-session-pending-steering session)))
                  (should (equal '(1 2 3 4) (mapcar (lambda (entry) (plist-get entry :id)) entries)))
                  (should (equal (and allowed "new") (plist-get (car entries) :request-id)))
                  (should (eq failed (cadr entries)))
                  (should (equal (and allowed "new") (plist-get (nth 2 entries) :request-id)))
                  (should (eq live (nth 3 entries))))))
          (when cockpit (kill-buffer cockpit)))))))

(provide 'test-niwa-turn-interrupt-steering)
;;; test-niwa-turn-interrupt-steering.el ends here
