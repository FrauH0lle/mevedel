;;; test-mevedel-turn-interrupt-steering.el --- Interrupted steering tests -*- lexical-binding: t -*-

;;; Commentary:

;; Distinguish deliberately interrupted steering from failed in-flight input.

;;; Code:

(require 'gptel-request)
(require 'mevedel-goal)
(require 'mevedel-pending-inputs)
(require 'mevedel-session-codec)
(require 'mevedel-turn)
(require 'mevedel-transport)
(require 'mevedel-view-interaction)
(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))

(mevedel-deftest mevedel--turn-fail-pending-input/interrupt-steering
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
             (session (mevedel-session--create
                       :pending-steering (list entry other)
                       :pending-follow-ups follow-ups))
             (request (mevedel-request--create :id "old" :session session))
             (info (list :buffer (current-buffer) :mevedel-request-id "old"
                         :mevedel-terminal-status 'aborted))
             (owner (if external request (gptel-make-fsm :info info))))
        (when external (setf (mevedel-engine-info owner) info))
        (setq-local mevedel--session session mevedel--current-request request)
        (mevedel--turn-fail-pending-input owner 'aborted)
        (let ((retained (car (mevedel-session-pending-steering session))))
          (should-not (plist-get retained :request-id))
          (should-not (plist-get retained :state))
          (should-not (plist-get retained :delivering))
          (should (equal "keep me" (plist-get retained :input)))
          (should (eq submission (plist-get retained :submission)))
          (should (equal "Guest" (plist-get retained :guest-name)))
          (should (equal '("/tmp/image.png") (plist-get retained :dropped-file-grants))))
        (should (eq other (cadr (mevedel-session-pending-steering session))))
        (should (eq follow-ups (mevedel-session-pending-follow-ups session)))
        (should (mevedel-session-pending-input-paused session))
        (should-not (mevedel-session-pending-input-failure-paused session)))))
  :doc "abort pauses follow-ups even without steering, preserving an older failure hold"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :pending-follow-ups '((:id 1 :input "later"))
                    :pending-input-failure-paused t)))
      (setq-local mevedel--session session)
      (mevedel--turn-fail-pending-input
       (gptel-make-fsm :info (list :buffer (current-buffer)
                                 :mevedel-request-id "old"
                                 :mevedel-terminal-status 'aborted)) 'aborted)
      (should (mevedel-session-pending-input-paused session))
      (should (mevedel-session-pending-input-failure-paused session))))
  :doc "a real failure retains request ownership and requires review"
  (with-temp-buffer
    (let ((session (mevedel-session--create
                    :pending-steering '((:id 1 :input "review" :request-id "old")))))
      (setq-local mevedel--session session)
      (mevedel--turn-fail-pending-input
       (gptel-make-fsm :info (list :buffer (current-buffer)
                                 :mevedel-request-id "old"
                                 :mevedel-terminal-status 'aborted)) 'error)
      (let ((entry (car (mevedel-session-pending-steering session))))
        (should (equal "old" (plist-get entry :request-id)))
        (should (eq 'failed-turn (plist-get entry :state))))
      (should (mevedel-session-pending-input-failure-paused session)))))

(mevedel-deftest mevedel--fail-turn/interrupt-steering
  ()
  ,test
  (test)
  :doc "cancelled transport settlement still preserves intentional abort input"
  (with-temp-buffer
    (let* ((session (mevedel-session--create
                     :pending-steering '((:id 1 :input "keep" :request-id "old"))))
           (request (mevedel-request--create :id "old" :session session))
           (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                          :mevedel-request-id "old"))))
      (setq-local mevedel--session session mevedel--current-request request)
      (cl-letf (((symbol-function 'mevedel--turn-commit) #'ignore)
                ((symbol-function 'mevedel--turn-end-request) #'ignore)
                ((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (&rest _) nil)))
        (mevedel--fail-turn fsm 'aborted))
      (should-not (plist-get (car (mevedel-session-pending-steering session)) :request-id))
      (should-not (mevedel-session-pending-input-failure-paused session))
      (should (mevedel-session-pending-input-paused session)))))

(mevedel-deftest mevedel-session-codec-deserialize/interrupt-steering
  ()
  ,test
  (test)
  :doc "restart preserves normal detached input but reviews crashed in-flight steering"
  (dolist (in-flight '(nil t))
    (let ((root (make-temp-file "mevedel-interrupt-codec-" t)))
      (unwind-protect
          (let* ((source (test-mevedel-session-persistence--make-session root))
                 (entry (append '(:id 1 :category steering :input "keep")
                                (and in-flight '(:request-id "old")))))
            (setf (mevedel-session-pending-steering source) (list entry)
                  (mevedel-session-pending-input-paused source) (not in-flight))
            (let* ((raw (mevedel-session-codec-serialize source))
                   (restored (plist-get
                              (mevedel-session-codec-deserialize
                               raw (mevedel-session-workspace source)) :session))
                   (retained (car (mevedel-session-pending-steering restored))))
              (should (eq in-flight (eq 'failed-turn (plist-get retained :state))))
              (should (eq in-flight (mevedel-session-pending-input-failure-paused restored)))
              (should (eq (not in-flight) (mevedel-session-pending-input-paused restored)))))
        (delete-directory root t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-goal-continue-if-idle/interrupt-steering
  ()
  ,test
  (test)
  :doc "pending steering and an interrupted queue pause each block Goal continuation"
  (dolist (hold '(steering paused))
    (with-temp-buffer
      (let* ((goal (mevedel-goal--create :id "goal" :status 'active))
             (session (mevedel-session--create
                       :goal goal
                       :pending-steering (and (eq hold 'steering) '((:input "keep")))
                       :pending-input-paused (eq hold 'paused))))
        (setq-local mevedel--session session)
        (should (eq 'pending-input
                    (mevedel-goal-continue-if-idle session (current-buffer))))))))

(mevedel-deftest mevedel--turn-resume-pending-input
  ()
  ,test
  (test)
  :doc "only the actual root send rebinds normal input and releases an interruption hold"
  (dolist (scenario '(root queued-only agent directive foreign-root foreign-session
                          owning-cockpit other-cockpit))
    (with-temp-buffer
      (let* ((first '(:id 1 :input "first" :state pending))
             (failed '(:id 2 :input "review" :state failed-turn))
             (second '(:id 3 :input "second"))
             (live '(:id 4 :input "other request" :request-id "other"))
             (session (mevedel-session--create
                       :root-buffer (unless (eq scenario 'foreign-root) (current-buffer))
                       :pending-input-paused t
                       :pending-follow-ups '((:id 5 :input "later"))
                       :pending-steering (unless (eq scenario 'queued-only)
                                           (list first failed second live))))
             (request (mevedel-request--create
                       :id "new" :session session
                       :directive-uuid (and (eq scenario 'directive) "directive")))
             (allowed (memq scenario '(root queued-only other-cockpit)))
             (cockpit (and (memq scenario '(owning-cockpit other-cockpit))
                           (generate-new-buffer mevedel-pending-inputs-buffer-name))))
        (unwind-protect
            (progn
              (setq-local mevedel--session (if (eq scenario 'foreign-session)
                                               (mevedel-session--create) session)
                          mevedel--agent-invocation (and (eq scenario 'agent) t))
              (when cockpit
                (with-current-buffer cockpit
                  (setq-local mevedel-cockpit--context
                              (list :session (if (eq scenario 'owning-cockpit)
                                                 session (mevedel-session--create))))))
              (mevedel--turn-resume-pending-input request)
              (should (eq (not allowed) (mevedel-session-pending-input-paused session)))
              (unless (eq scenario 'queued-only)
                (let ((entries (mevedel-session-pending-steering session)))
                  (should (equal '(1 2 3 4) (mapcar (lambda (entry) (plist-get entry :id)) entries)))
                  (should (equal (and allowed "new") (plist-get (car entries) :request-id)))
                  (should (eq failed (cadr entries)))
                  (should (equal (and allowed "new") (plist-get (nth 2 entries) :request-id)))
                  (should (eq live (nth 3 entries))))))
          (when cockpit (kill-buffer cockpit)))))))

(provide 'test-mevedel-turn-interrupt-steering)
;;; test-mevedel-turn-interrupt-steering.el ends here
