;;; test-mevedel-recovery.el --- Recovery behavior -*- lexical-binding: t -*-
;;; Commentary:
;; Issues and retained inputs contain no live process state.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-recovery)
(require 'mevedel-models)
(require 'mevedel-pending-inputs)
(require 'gptel-openai)

(mevedel-deftest mevedel-recovery-report ()
  (let ((session (mevedel-session--create)))
    (should (mevedel-recovery-report session "model" 'model "Select a model" t))
    (should-error (mevedel-recovery-assert-ready session) :type 'user-error)
    (should-not (mevedel-recovery-report session "model" 'model "Select a model" t))
    (should (mevedel-recovery-report session "model" 'model "Using fallback" nil))
    (should (= 1 (length (mevedel-session-recovery-issues session))))
    (should-not (mevedel-recovery-blocker session))
    (should (mevedel-recovery-clear session "model"))
    (should-not (mevedel-recovery-clear session "model"))
    (should-not (mevedel-session-recovery-issues session))))

(mevedel-deftest mevedel-recovery-clear-informational ()
  (let ((session (mevedel-session--create)))
    (mevedel-recovery-report session "preset" 'configuration "Choose a preset" t)
    (mevedel-recovery-report session "agent:a" 'request "Agent a failed" nil)
    (mevedel-recovery-report session "session-naming" 'request "Naming failed" nil)
    (should (mevedel-recovery-clear-informational session))
    (should (equal '("preset") (mapcar (lambda (issue) (plist-get issue :id))
                                       (mevedel-session-recovery-issues session))))))

(mevedel-deftest mevedel-recovery-category ()
  (progn
    (should (eq 'model (mevedel-recovery-category "Unavailable" "model_not_found")))
    (should (eq 'model (mevedel-recovery-category "model \"llama3\" not found" nil "404")))
    (should (eq 'authentication (mevedel-recovery-category "Denied" nil "401")))
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
      (should (eq (cadr row) (mevedel-recovery-category (car row)))))))

(mevedel-deftest mevedel-recovery-public-message ()
  (progn
    (should (equal "Provider sign-in is required"
                   (mevedel-recovery-public-message
                    '(:id "authentication" :category "authentication"
                      :message "Searching for program: /home/me/bin/claude"))))
    (should (equal "Agent a failed"
                   (mevedel-recovery-public-message
                    '(:id "agent:a" :category "request" :message "Agent a failed"))))))

(mevedel-deftest mevedel-recovery-persist-inputs ()
  (let* ((entry (list :input "hello" :delivering t :guest-paths '("/tmp/input")))
	 (session (mevedel-session--create :pending-follow-ups (list entry)))
	 (saved (mevedel-recovery-persist-inputs session)))
    (should (plist-get entry :delivering))
    (should-not (plist-get (car saved) :delivering))
    (should (equal '("/tmp/input") (plist-get (car saved) :guest-paths)))))

(mevedel-deftest mevedel-model-recover-provider ()
  (let* ((gptel--known-backends nil)
	 (backend (gptel-make-openai "Recovery" :key "test" :models '(available)))
	 (mevedel-model-fallback-provider "Recovery:available")
	 (session (mevedel-session--create :model-provider "Gone:old" :reasoning-effort 'high)))
    (with-temp-buffer
      (mevedel-model-apply-session-policy session)
      (should (eq backend gptel-backend))
      (should (eq 'available gptel-model))
      (should-not gptel-reasoning-effort)
      (should (equal "Recovery:available" (mevedel-session-model-provider session)))
      (should (mevedel-session-recovery-issues session))
      (should-not (mevedel-recovery-blocker session)))
    (setf (mevedel-session-recovery-issues session) nil)
    ;; A live rejection of the fallback itself finds nothing and blocks nothing.
    (should-not (mevedel-model-recover-provider "Recovery:available" session t))
    (should-not (mevedel-recovery-blocker session))
    (setq mevedel-model-fallback-provider "Gone:also-gone")
    (setf (mevedel-session-model-provider session) "Gone:old")
    (with-temp-buffer
      ;; A saved model with no fallback blocks until a model is selected.
      (mevedel-model-apply-session-policy session)
      (should (mevedel-recovery-blocker session))
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Recovery:available"))
      (should-not (mevedel-recovery-blocker session)))
    (should-error (mevedel-model-resolve-provider "Gone:old") :type 'user-error)))

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "mevedel-engine-test-support"))

(defun test-mevedel-recovery--saved-follow-ups (session)
  "Return SESSION's follow-ups as its committed sidecar holds them."
  (plist-get (car (read-from-string
                   (mevedel-session-artifacts-read-artifact session "session.meta.el")))
             :pending-follow-ups))

(mevedel-deftest mevedel-recovery-save (:quiet t)
  ,test
  (test)
  :doc "queue changes reach the sidecar through one coalesced save"
  (mevedel-engine-test--with-session
   ;; Sidecar-only saves overlay the first full snapshot.
   (should (mevedel-session-artifacts-save session buffer))
   (mevedel-session-enqueue-pending-input session 'follow-up (list :input "hello"))
   (mevedel-pending-inputs-follow-up-changed session)
   (mevedel-pending-inputs-follow-up-changed session)
   (let ((timer (gethash session mevedel-session-persistence--deferred-agent-saves)))
     (should timer)
     (cancel-timer timer)
     (remhash session mevedel-session-persistence--deferred-agent-saves))
   (should-not (test-mevedel-recovery--saved-follow-ups session))
   (mevedel-session-persistence--deferred-agent-save session)
   (mevedel-test--await 5 "Deferred save did not run"
     (test-mevedel-recovery--saved-follow-ups session))
   (should (equal "hello" (plist-get (car (test-mevedel-recovery--saved-follow-ups session))
                                     :input))))
  :doc "an unchanged queue replacement schedules no save"
  (mevedel-engine-test--with-session
   (mevedel-pending-inputs--set-queues
    session 'follow-up (mevedel-session-pending-follow-ups session))
   (should-not (gethash session mevedel-session-persistence--deferred-agent-saves))))

(mevedel-deftest mevedel-view--drain-follow-up/delivery-intent (:quiet t)
  (mevedel-engine-test--with-session
   (mevedel-request-end)
   (let ((view (generate-new-buffer " *recovery-view*")))
     (unwind-protect
	 (progn
	   (setq-local mevedel--view-buffer view)
	   (mevedel-view--setup view buffer)
	   (should (mevedel-session-artifacts-save session buffer))
	   (mevedel-session-enqueue-pending-input session 'follow-up '(:input "commit once"))
	   (cl-letf (((symbol-function 'mevedel-view--dispatch-follow-up-entry)
		      (lambda (_kind _entry input _session data _before after _release)
			(with-current-buffer data (insert input))
			(funcall after))))
	     (mevedel-view--drain-follow-up buffer))
	   (should-not (mevedel-session-pending-follow-ups session))
	   (should-not (mevedel-session-pending-input-failure-paused session))
	   ;; The intent was durable before dispatch; delivery itself writes
	   ;; nothing, and a restart reviews the uncertain prompt.
	   (let* ((saved (car (read-from-string
			       (mevedel-session-artifacts-read-artifact session "session.meta.el"))))
		  (restored (plist-get (mevedel-session-codec-deserialize saved workspace) :session)))
	     (should (eq 'dispatching (plist-get (car (plist-get saved :pending-follow-ups)) :state)))
	     (should (eq 'failed-turn (plist-get (car (mevedel-session-pending-follow-ups restored)) :state)))
	     (should (mevedel-session-pending-input-failure-paused restored))))
       (when (buffer-live-p view) (kill-buffer view))))))

(provide 'test-mevedel-recovery)
;;; test-mevedel-recovery.el ends here
