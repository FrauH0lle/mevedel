;;; test-mevedel-readiness.el --- Shared readiness and replay boundaries -*- lexical-binding: t -*-
;;; Commentary:
;; Recovery fans out through credential identity but never clears failure pause.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-readiness)

(mevedel-deftest mevedel-readiness-changed ()
  (let* ((gptel--known-backends nil)
	 (first (gptel-make-openai-oauth "First"))
	 (second (gptel-make-openai-oauth "Second")))
    (with-temp-buffer
      (let ((session (mevedel-session--create :root-buffer (current-buffer)
					      :pending-input-failure-paused t)))
	(setq-local mevedel--session session gptel-backend second)
	(mevedel-recovery-report session "authentication" 'authentication "Expired" t)
	(mevedel-recovery-report session "input" 'authentication "Retained" t)
	(mevedel-readiness-changed first '(:status "ready"))
	(should-not (mevedel-recovery-blocker session))
	(should (mevedel-session-pending-input-failure-paused session))
        (setq-local gptel-backend (gptel-make-openai "Ordinary" :key "fixture" :models '(current))
                    mevedel-auth--waiting-backend second)
        (mevedel-recovery-report session "authentication" 'authentication "Effective provider expired" t)
        (mevedel-readiness-changed first '(:status "ready"))
        (should-not (mevedel-recovery-blocker session))
        (should-not mevedel-auth--waiting-backend)))))

(mevedel-deftest mevedel-readiness-assert ()
  (let* ((gptel--known-backends nil)
	 (backend (gptel-make-openai "Available" :key "fixture" :models '(current)))
	 (mevedel-model-fallback-provider "Available:current"))
    (with-temp-buffer
      (let ((session (mevedel-session--create :model-provider "Removed:old")))
	(setq-local gptel-backend backend gptel-model 'current)
	(mevedel-readiness-assert session)
	(should (equal "Available:current" (mevedel-session-model-provider session)))
	(mevedel-recovery-report session "preset" 'configuration "Choose a preset" t)
	(should-error (mevedel-readiness-assert session) :type 'user-error)))))

(mevedel-deftest mevedel-readiness-stop ()
  (with-temp-buffer
    (let (cancelled)
      (setq mevedel-readiness--claude (list :cancel (lambda () (should-not mevedel-readiness--claude) (setq cancelled t))))
      (mevedel-readiness-stop)
      (should cancelled)
      (should-not mevedel-readiness--claude))))

(mevedel-deftest mevedel-readiness-assert/native-precommit (:quiet t)
  (let ((gptel--known-backends nil)
	(peer (expand-file-name "test/fixtures/acp-agent.py" default-directory)))
    (with-temp-buffer
      (let* ((session (mevedel-session--create :root-buffer (current-buffer)))
	     (backend (mevedel-claude-code-register))
	     (buffer (current-buffer)))
	(setq-local mevedel--session session gptel-backend backend
		    gptel-model (car (gptel-backend-models backend)))
	(cl-letf (((symbol-function 'mevedel-claude-code-launch)
		   (lambda (&rest _) (list :command (executable-find "python3")
					   :args (list peer) :cwd temporary-file-directory))))
	  (should-error (mevedel-readiness-assert session) :type 'user-error)
	  (should (string-empty-p (buffer-string)))
	  (with-timeout (5 (ert-fail "Native readiness did not settle"))
	    (while (eq 'checking (plist-get mevedel-readiness--claude :state))
	      (accept-process-output nil 0.01)))
	  (ert-info ((prin1-to-string (mevedel-session-recovery-issues session)))
            (should (eq 'ready (plist-get mevedel-readiness--claude :state))))
	  (mevedel-readiness-assert session)
	  (should (buffer-live-p buffer))
	  (should (string-empty-p (buffer-string))))))))

(mevedel-deftest mevedel-readiness-runtime-changed ()
  (let ((gptel--known-backends nil))
    (with-temp-buffer
      (let ((session (mevedel-session--create :root-buffer (current-buffer))))
	(setq-local mevedel--session session gptel-backend (mevedel-claude-code-register))
	(mevedel-readiness-runtime-changed '(:status "failed" :message "Update failed"))
	(should (equal "Update failed" (plist-get (car (mevedel-session-recovery-issues session)) :message)))
	(should-not (mevedel-recovery-blocker session))
	(mevedel-readiness-runtime-changed '(:status "ready"))
	(should-not (mevedel-session-recovery-issues session))))))

(mevedel-deftest mevedel-readiness-changed/scoped-native ()
  (let* ((gptel--known-backends nil)
         (backend (mevedel-claude-code-register)))
    (with-temp-buffer
      (let* ((first (current-buffer))
             (session (mevedel-session--create :root-buffer first)))
        (setq-local mevedel--session session gptel-backend backend)
        (mevedel-recovery-report session "authentication" 'model "Unavailable model" t)
        (with-temp-buffer
          (let ((other (mevedel-session--create :root-buffer (current-buffer))))
            (setq-local mevedel--session other gptel-backend backend)
            (mevedel-recovery-report other "authentication" 'model "Other model unavailable" t)
            (mevedel-readiness-changed backend (list :status "ready" :checked first))
            (should-not (mevedel-recovery-blocker session))
            (should (mevedel-recovery-blocker other))))))))

(provide 'test-mevedel-readiness)
;;; test-mevedel-readiness.el ends here
