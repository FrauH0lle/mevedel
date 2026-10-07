;;; test-mevedel-recovery.el --- Recovery behavior -*- lexical-binding: t -*-
;;; Commentary:
;; Issues and retained inputs contain no live process state.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-recovery)
(require 'mevedel-models)
(require 'gptel-openai)

(mevedel-deftest mevedel-recovery-report ()
  (let ((session (mevedel-session--create)))
    (mevedel-recovery-report session "model" 'model "Select a model" t)
    (should-error (mevedel-recovery-assert-ready session) :type 'user-error)
    (mevedel-recovery-report session "model" 'model "Using fallback" nil)
    (should (= 1 (length (mevedel-session-recovery-issues session))))
    (should-not (mevedel-recovery-blocker session))
    (mevedel-recovery-clear session "model")
    (should-not (mevedel-session-recovery-issues session))))

(mevedel-deftest mevedel-recovery-category ()
  (progn
    (should (eq 'model (mevedel-recovery-category "Unavailable" "model_not_found")))
    (dolist (row '(("Login expired" authentication) ("Model x unavailable" model)
		   ("Install Claude Code" dependency) ("Connection timed out" request)))
      (should (eq (cadr row) (mevedel-recovery-category (car row)))))))

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
      (should-not (mevedel-recovery-blocker session)))
    (should-not (mevedel-model-recover-provider "Recovery:available" session t))
    (should (mevedel-recovery-blocker session))
    (setq mevedel-model-fallback-provider "Gone:also-gone")
    (should-not (mevedel-model-recover-provider "Gone:old" session))
    (should (mevedel-recovery-blocker session))
    (should-error (mevedel-model-resolve-provider "Gone:old") :type 'user-error)))
(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "mevedel-engine-test-support"))

(mevedel-deftest mevedel-recovery-enqueue (:quiet t)
  ,test
  (test)
  :doc "accepted input is durable before enqueue returns and removal is durable"
  (mevedel-engine-test--with-session
   (let ((attachment (file-name-concat root "attachment.txt")))
     (with-temp-file attachment (insert "retained"))
     (mevedel-recovery-enqueue session 'follow-up
			       (list :input "hello" :guest-paths (list attachment)))
     (let ((saved (car (read-from-string
			(mevedel-session-artifacts-read-artifact session "session.meta.el")))))
       (should (equal "hello" (plist-get (car (plist-get saved :pending-follow-ups)) :input)))
       (should (file-exists-p attachment)))
     (mevedel-pending-inputs--set-queues session 'follow-up nil)
     (should-not (plist-get (car (read-from-string
				  (mevedel-session-artifacts-read-artifact session "session.meta.el")))
			    :pending-follow-ups))))
  :doc "failed publication rolls acceptance back without taking attachment ownership"
  (let ((session (mevedel-session--create)))
    (cl-letf (((symbol-function 'mevedel-recovery-save) (lambda (_) (error "Disk failure"))))
      (should-error (mevedel-recovery-enqueue session 'follow-up '(:input "retained draft"))))
    (should-not (mevedel-session-pending-follow-ups session))
    (should-not (mevedel-session-pending-input-next-id session))))

(mevedel-deftest mevedel-view--drain-follow-up/publication-failure (:quiet t)
  (mevedel-engine-test--with-session
   (mevedel-request-end)
   (let ((view (generate-new-buffer " *recovery-view*"))
	 (save (symbol-function 'mevedel-recovery-save))
	 (calls 0))
     (unwind-protect
	 (progn
	   (setq-local mevedel--view-buffer view)
	   (mevedel-view--setup view buffer)
	   (mevedel-recovery-enqueue session 'follow-up '(:input "commit once"))
	   (cl-letf (((symbol-function 'mevedel-view--dispatch-follow-up-entry)
		      (lambda (_kind _entry input _session data _before after _release)
			(with-current-buffer data (insert input))
			(funcall after)))
		     ((symbol-function 'mevedel-recovery-save)
		      (lambda (owner)
			(cl-incf calls)
			(if (= calls 1) (funcall save owner) (error "Publication failed")))))
	     (should-error (mevedel-view--drain-follow-up buffer)))
	   (should-not (mevedel-session-pending-follow-ups session))
	   (should (mevedel-session-pending-input-failure-paused session))
	   (let* ((saved (car (read-from-string
			       (mevedel-session-artifacts-read-artifact session "session.meta.el"))))
		  (restored (plist-get (mevedel-session-codec-deserialize saved workspace) :session)))
	     (should (eq 'dispatching (plist-get (car (plist-get saved :pending-follow-ups)) :state)))
	     (should (eq 'failed-turn (plist-get (car (mevedel-session-pending-follow-ups restored)) :state)))
	     (should (mevedel-session-pending-input-failure-paused restored))))
       (when (buffer-live-p view) (kill-buffer view))))))

(provide 'test-mevedel-recovery)
;;; test-mevedel-recovery.el ends here
