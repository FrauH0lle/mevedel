;;; test-niwa-recovery.el --- Recovery behavior -*- lexical-binding: t -*-
;;; Commentary:
;; Issues and retained inputs contain no live process state.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-recovery)
(require 'niwa-utilities)
(require 'niwa-models)
(require 'niwa-pending-inputs)
(require 'gptel-openai)

(niwa-deftest niwa-recovery-report ()
  (let ((session (niwa-session--create)))
    (should (niwa-recovery-report session "model" 'model "Select a model" t))
    (should-error (niwa-recovery-assert-ready session) :type 'user-error)
    (should-not (niwa-recovery-report session "model" 'model "Select a model" t))
    (should (niwa-recovery-report session "model" 'model "Using fallback" nil))
    (should (= 1 (length (niwa-session-recovery-issues session))))
    (should-not (niwa-recovery-blocker session))
    (should (niwa-recovery-clear session "model"))
    (should-not (niwa-recovery-clear session "model"))
    (should-not (niwa-session-recovery-issues session))))

(niwa-deftest niwa-recovery-assert-ready ()
  (let* ((workspace (niwa-workspace--create :root "/project A/same/"))
         (issue (list :id "model" :category "model" :message "Select a model" :blocking t))
         (session (niwa-session--create :workspace workspace :recovery-issues (list issue))))
    (with-temp-buffer
      (setq-local niwa--workspace (niwa-workspace--create :root "/project B/same/"))
      (should (equal (list 'user-error "Select a model")
                     (should-error (niwa-recovery-assert-ready session) :type 'user-error)))
      (should (equal (list 'user-error "niwa [/project A/same/]: Select a model")
                     (should-error
                      (niwa-with-diagnostic-errors (niwa-diagnostic-scope session)
                        (niwa-recovery-assert-ready session))
                      :type 'user-error))))
    (should (equal "Select a model" (plist-get issue :message)))
    (should (equal "Select a model" (niwa-recovery-public-message issue)))))

(niwa-deftest niwa-recovery-clear-informational ()
  (let ((session (niwa-session--create)))
    (niwa-recovery-report session "preset" 'configuration "Choose a preset" t)
    (niwa-recovery-report session "agent:a" 'request "Agent a failed" nil)
    (niwa-recovery-report session "session-naming" 'request "Naming failed" nil)
    (should (niwa-recovery-clear-informational session))
    (should (equal '("preset") (mapcar (lambda (issue) (plist-get issue :id))
                                       (niwa-session-recovery-issues session))))))

(niwa-deftest niwa-recovery-dismiss ()
  ,test
  (test)
  :doc "dismissal removes only the notice and schedules a save"
  (let* ((issue '(:id "agent:a" :message "Failed" :blocking nil))
         (blocker '(:id "authentication" :message "Sign in" :blocking t))
         (session (niwa-session--create
                   :recovery-issues (list issue blocker)))
         saved)
    (cl-letf (((symbol-function 'niwa-recovery-save)
               (lambda (session) (setq saved session))))
      (should (niwa-recovery-dismiss session issue))
      (should (eq session saved))
      (should (equal (list blocker) (niwa-session-recovery-issues session)))
      (should-not (niwa-recovery-dismiss session issue))))

  :doc "blockers cannot be dismissed"
  (let* ((issue '(:id "authentication" :message "Sign in" :blocking t))
         (session (niwa-session--create :recovery-issues (list issue))))
    (should-not (niwa-recovery-dismiss session issue))
    (should (eq issue (niwa-recovery-blocker session))))

  :doc "a stale action cannot remove a replacement with the same id"
  (let* ((old (list :id "agent:a" :message "Failed" :blocking nil))
         (new (copy-sequence old))
         (session (niwa-session--create :recovery-issues (list new))))
    (should-not (niwa-recovery-dismiss session old))
    (should (eq new (car (niwa-session-recovery-issues session)))))

  :doc "a replacement during authority admission is not dismissed"
  (let* ((issue (list :id "agent:a" :message "Failed" :blocking nil))
         (session (niwa-session--create :recovery-issues (list issue))))
    (cl-letf (((symbol-function 'niwa-session-artifacts-assert-new-mutation-authority)
               (lambda (session)
                 (niwa-recovery-report session "agent:a" 'request "Failed again" nil))))
      (should-not (niwa-recovery-dismiss session issue))
      (should (equal "Failed again"
                     (plist-get (car (niwa-session-recovery-issues session)) :message)))))

  :doc "quiescing sessions reject dismissal without changing the notice"
  (let* ((issue '(:id "agent:a" :message "Failed" :blocking nil))
         (session (niwa-session--create
                   :recovery-issues (list issue)
                   :control-transfer '(:state quiescing))))
    (should-error (niwa-recovery-dismiss session issue) :type 'user-error)
    (should (eq issue (car (niwa-session-recovery-issues session))))))

(niwa-deftest niwa-recovery-category ()
  (progn
    (should (eq 'model (niwa-recovery-category "Unavailable" "model_not_found")))
    (should (eq 'model (niwa-recovery-category "model \"llama3\" not found" nil "404")))
    (should (eq 'authentication (niwa-recovery-category "Denied" nil "401")))
    (dolist (row '(("Login expired" authentication)
                   ("No auth credentials found" authentication)
                   ("Unauthorized" authentication)
                   ("The author of this request is unknown" request)
                   ;; Free text never identifies a missing model: overload
                   ;; messages name the model too.
                   ("Model x unavailable" request)
                   ("Please upgrade to a paid plan" request)
                   ("Install Claude Code" dependency)
                   ("Connection timed out" request)))
      (should (eq (cadr row) (niwa-recovery-category (car row)))))))

(niwa-deftest niwa-recovery-public-message ()
  (progn
    (should (equal "Provider sign-in is required"
                   (niwa-recovery-public-message
                    '(:id "authentication" :category "authentication"
                      :message "Searching for program: /home/me/bin/claude"))))
    (should (equal "Agent a failed"
                   (niwa-recovery-public-message
                    '(:id "agent:a" :category "request" :message "Agent a failed"))))))

(niwa-deftest niwa-recovery-persist-inputs ()
  (let* ((entry (list :input "hello" :delivering t :guest-paths '("/tmp/input")))
	 (session (niwa-session--create :pending-follow-ups (list entry)))
	 (saved (niwa-recovery-persist-inputs session)))
    (should (plist-get entry :delivering))
    (should-not (plist-get (car saved) :delivering))
    (should (equal '("/tmp/input") (plist-get (car saved) :guest-paths)))))

(niwa-deftest niwa-model-recover-provider ()
  (let* ((gptel--known-backends nil)
	 (backend (gptel-make-openai "Recovery" :key "test" :models '(available)))
	 (niwa-model-fallback-provider "Recovery:available")
	 (session (niwa-session--create :model-provider "Gone:old" :reasoning-effort 'high)))
    (with-temp-buffer
      (niwa-model-apply-session-policy session)
      (should (eq backend gptel-backend))
      (should (eq 'available gptel-model))
      (should-not gptel-reasoning-effort)
      (should (equal "Recovery:available" (niwa-session-model-provider session)))
      (should (niwa-session-recovery-issues session))
      (should-not (niwa-recovery-blocker session)))
    (setf (niwa-session-recovery-issues session) nil)
    ;; A live rejection of the fallback itself finds nothing and blocks nothing.
    (should-not (niwa-model-recover-provider "Recovery:available" session t))
    (should-not (niwa-recovery-blocker session))
    (setq niwa-model-fallback-provider "Gone:also-gone")
    (setf (niwa-session-model-provider session) "Gone:old")
    (with-temp-buffer
      ;; A saved model with no fallback blocks until a model is selected.
      (niwa-model-apply-session-policy session)
      (should (niwa-recovery-blocker session))
      (niwa-model-set-session-provider
       session (niwa-model-resolve-provider "Recovery:available"))
      (should-not (niwa-recovery-blocker session)))
    (should-error (niwa-model-resolve-provider "Gone:old") :type 'user-error)))

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "niwa-engine-test-support"))

(defun test-niwa-recovery--saved-follow-ups (session)
  "Return SESSION's follow-ups as its committed sidecar holds them."
  (plist-get (car (read-from-string
                   (niwa-session-artifacts-read-artifact session "session.meta.el")))
             :pending-follow-ups))

(niwa-deftest niwa-recovery-save (:quiet t)
  ,test
  (test)
  :doc "queue changes reach the sidecar through one coalesced save"
  (niwa-engine-test--with-session
   ;; Sidecar-only saves overlay the first full snapshot.
   (should (niwa-session-artifacts-save session buffer))
   (niwa-session-enqueue-pending-input session 'follow-up (list :input "hello"))
   (niwa-pending-inputs-follow-up-changed session)
   (niwa-pending-inputs-follow-up-changed session)
   (let ((timer (gethash session niwa-session-persistence--deferred-agent-saves)))
     (should timer)
     (cancel-timer timer)
     (remhash session niwa-session-persistence--deferred-agent-saves))
   (should-not (test-niwa-recovery--saved-follow-ups session))
   (niwa-session-persistence--deferred-agent-save session)
   (niwa-test--await 5 "Deferred save did not run"
     (test-niwa-recovery--saved-follow-ups session))
   (should (equal "hello" (plist-get (car (test-niwa-recovery--saved-follow-ups session))
                                     :input))))
  :doc "an unchanged queue replacement schedules no save"
  (niwa-engine-test--with-session
   (niwa-pending-inputs--set-queues
    session 'follow-up (niwa-session-pending-follow-ups session))
   (should-not (gethash session niwa-session-persistence--deferred-agent-saves))))

(niwa-deftest niwa-view--drain-follow-up/delivery-intent (:quiet t)
  (niwa-engine-test--with-session
   (niwa-request-end)
   (let ((view (generate-new-buffer " *recovery-view*")))
     (unwind-protect
	 (progn
	   (setq-local niwa--view-buffer view)
	   (niwa-view--setup view buffer)
	   (should (niwa-session-artifacts-save session buffer))
	   (niwa-session-enqueue-pending-input session 'follow-up '(:input "commit once"))
	   (cl-letf (((symbol-function 'niwa-view--dispatch-follow-up-entry)
		      (lambda (_kind _entry input data _before after _release)
			(with-current-buffer data (insert input))
			(funcall after))))
	     (niwa-view--drain-follow-up buffer))
	   (should-not (niwa-session-pending-follow-ups session))
	   (should-not (niwa-session-pending-input-failure-paused session))
	   ;; The intent was durable before dispatch; delivery itself writes
	   ;; nothing, and a restart reviews the uncertain prompt.
	   (let* ((saved (car (read-from-string
			       (niwa-session-artifacts-read-artifact session "session.meta.el"))))
		  (restored (plist-get (niwa-session-codec-deserialize saved workspace) :session)))
	     (should (eq 'dispatching (plist-get (car (plist-get saved :pending-follow-ups)) :state)))
	     (should (eq 'failed-turn (plist-get (car (niwa-session-pending-follow-ups restored)) :state)))
	     (should (niwa-session-pending-input-failure-paused restored))))
       (when (buffer-live-p view) (kill-buffer view))))))

(provide 'test-niwa-recovery)
;;; test-niwa-recovery.el ends here
