;;; test-mevedel-readiness.el --- Shared readiness and replay boundaries -*- lexical-binding: t -*-
;;; Commentary:
;; Readiness never refuses a Claude send, clears only the issues it owns,
;; and never re-imposes saved model policy.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-readiness)

(defmacro test-mevedel-readiness--with-claude (&rest body)
  "Run BODY in a root buffer whose session uses the Claude backend."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (with-temp-buffer
       (let* ((session (mevedel-session--create :root-buffer (current-buffer)))
              (backend (mevedel-claude-code-register))
              (buffer (current-buffer)))
         (ignore buffer)
         (setq-local mevedel--session session gptel-backend backend
                     gptel-model (car (gptel-backend-models backend)))
         (cl-letf (((symbol-function 'mevedel-recovery-save) #'ignore))
           ,@body)))))

(mevedel-deftest mevedel-readiness-changed ()
  (let* ((gptel--known-backends nil)
	 (first (gptel-make-openai-oauth "First"))
	 (second (gptel-make-openai-oauth "Second")))
    (with-temp-buffer
      (let ((session (mevedel-session--create :root-buffer (current-buffer)
					      :pending-input-failure-paused t)))
	(setq-local mevedel--session session gptel-backend second)
	(mevedel-recovery-report session "authentication" 'authentication "Expired" t)
	;; Gates owned elsewhere survive an unrelated provider becoming ready.
	(mevedel-recovery-report session "preset" 'configuration "Choose a preset" t)
	(mevedel-readiness-changed first '(:status "ready"))
	(should (equal "preset" (plist-get (mevedel-recovery-blocker session) :id)))
	(should (mevedel-session-pending-input-failure-paused session))
        (mevedel-recovery-clear session "preset")
        (setq-local gptel-backend (gptel-make-openai "Ordinary" :key "fixture" :models '(current))
                    mevedel-auth--waiting-backend second)
        (mevedel-recovery-report session "authentication" 'authentication "Effective provider expired" t)
        (mevedel-readiness-changed first '(:status "ready"))
        (should-not (mevedel-recovery-blocker session))
        (should-not mevedel-auth--waiting-backend)))))

(mevedel-deftest mevedel-readiness-assert ()
  ,test
  (test)
  :doc "keeps the buffer's model and refuses only for owned blocking issues"
  (let* ((gptel--known-backends nil)
	 (backend (gptel-make-openai "Available" :key "fixture" :models '(current other))))
    (with-temp-buffer
      (let ((session (mevedel-session--create :model-provider "Available:current")))
	(setq-local gptel-backend backend gptel-model 'other gptel-reasoning-effort 'high)
	(mevedel-recovery-report session "authentication" 'authentication "Expired" t)
	(mevedel-readiness-assert session)
	;; A model chosen outside the model menu is not reverted per send.
	(should (eq 'other gptel-model))
	(should (eq 'high gptel-reasoning-effort))
	(should-not (mevedel-recovery-blocker session))
	(mevedel-recovery-report session "preset" 'configuration "Choose a preset" t)
	(should-error (mevedel-readiness-assert session) :type 'user-error))))
  :doc "unknown Claude readiness never refuses or starts a check"
  (test-mevedel-readiness--with-claude
    (cl-letf (((symbol-function 'mevedel-claude-code-launch)
               (lambda (&rest _) (ert-fail "No check expected"))))
      (mevedel-readiness-assert session)
      (should-not mevedel-readiness--claude)))
  :doc "a known failure is informational and a successful check resumes"
  (test-mevedel-readiness--with-claude
    (let ((peer (expand-file-name "test/fixtures/acp-agent.py" default-directory))
          messages)
      (mevedel-readiness-record nil "Install Claude Code and run `claude auth login'" t)
      (should-not (mevedel-recovery-blocker session))
      (should (equal "Install Claude Code and run `claude auth login'"
                     (plist-get (car (mevedel-session-recovery-issues session)) :message)))
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
		 (lambda (&rest _) (list :command (executable-find "python3")
					 :args (list peer) :cwd temporary-file-directory)))
                ((symbol-function 'message)
                 (lambda (format &rest args) (when format (push (apply #'format format args) messages)))))
        ;; The next send is the retry; only an explicit check probes.
        (mevedel-readiness-assert session)
        (should (eq 'failed (plist-get mevedel-readiness--claude :state)))
        (mevedel-readiness-check)
        (should (eq 'checking (plist-get mevedel-readiness--claude :state)))
	(with-timeout (5 (ert-fail "Native readiness did not settle"))
	  (while (eq 'checking (plist-get mevedel-readiness--claude :state))
	    (accept-process-output nil 0.01)))
	(should (eq 'ready (plist-get mevedel-readiness--claude :state)))
        (should-not (mevedel-recovery-blocker session))
        (should (cl-find-if (lambda (text) (string-search "Claude is ready" text))
                            messages))
	(mevedel-readiness-assert session)
	(should (string-empty-p (buffer-string))))))
  :doc "a configuration change forgets a failure"
  (test-mevedel-readiness--with-claude
    (mevedel-readiness-record nil "Old executable is missing")
    (mevedel-recovery-clear session "authentication")
    (let ((mevedel-claude-code-executable "/elsewhere/claude"))
      (mevedel-readiness-assert session)
      (should-not mevedel-readiness--claude))))

(mevedel-deftest mevedel-readiness-record-turn ()
  (test-mevedel-readiness--with-claude
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (when format (setq echoed (apply #'format format args))))))
        ;; A startup that never reached its prompt shows the real cause.
        (mevedel-readiness-record-turn (list :backend backend) 'error "Claude Code 2.1.290 is required")
        (should (eq 'failed (plist-get mevedel-readiness--claude :state)))
        (should (string-search "2.1.290 is required" echoed))
        ;; The failed turn's own request issue reports it; no second issue.
        (should-not (mevedel-session-recovery-issues session))
        ;; Reaching the prompt proves readiness, whatever the outcome.
        (mevedel-readiness-record-turn (list :backend backend :mevedel-acp-prompted t) 'error "Later")
        (should (eq 'ready (plist-get mevedel-readiness--claude :state)))
        (should-not (mevedel-recovery-blocker session))
        ;; Aborted startups and other providers teach nothing.
        (mevedel-readiness-stop)
        (mevedel-readiness-record-turn (list :backend backend) 'aborted nil)
        (mevedel-readiness-record-turn (list :backend 'other) 'error "Other")
        (should-not mevedel-readiness--claude)))))

(mevedel-deftest mevedel-readiness-stop ()
  (with-temp-buffer
    (let (cancelled)
      (setq mevedel-readiness--claude (list :cancel (lambda () (should-not mevedel-readiness--claude) (setq cancelled t))))
      (mevedel-readiness-stop)
      (should cancelled)
      (should-not mevedel-readiness--claude))))

(mevedel-deftest mevedel-readiness-runtime-changed ()
  (let ((gptel--known-backends nil)
        (saves 0))
    (cl-letf (((symbol-function 'mevedel-recovery-save) (lambda (_) (cl-incf saves))))
      (with-temp-buffer
        (let ((session (mevedel-session--create :root-buffer (current-buffer))))
	  (setq-local mevedel--session session gptel-backend (mevedel-claude-code-register))
          (with-temp-buffer
            (let ((other (mevedel-session--create :root-buffer (current-buffer))))
	      (setq-local mevedel--session other gptel-backend (mevedel-claude-code-register))
	      (mevedel-readiness-runtime-changed '(:status "failed" :message "Update failed"))
	      (should (equal "Update failed" (plist-get (car (mevedel-session-recovery-issues session)) :message)))
	      (should-not (mevedel-recovery-blocker session))
              ;; One save per changed session; an unchanged event saves nothing.
              (should (= 2 saves))
	      (mevedel-readiness-runtime-changed '(:status "failed" :message "Update failed"))
              (should (= 2 saves))
	      (mevedel-readiness-runtime-changed '(:status "ready"))
              (should (= 4 saves))
	      (should-not (mevedel-session-recovery-issues session))
	      (should-not (mevedel-session-recovery-issues other)))))))))

(provide 'test-mevedel-readiness)
;;; test-mevedel-readiness.el ends here
