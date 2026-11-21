;;; test-niwa-readiness.el --- Shared readiness and replay boundaries -*- lexical-binding: t -*-
;;; Commentary:
;; Readiness never refuses a Claude send, clears only the issues it owns,
;; and never re-imposes saved model policy.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-readiness)
(require 'niwa-auth)
(require 'niwa-claude-code)
(require 'niwa-utilities)
(require 'niwa-view)

(defmacro test-niwa-readiness--with-claude (&rest body)
  "Run BODY in a root buffer whose session uses the Claude backend."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (with-temp-buffer
       (let* ((session (niwa-session--create :root-buffer (current-buffer)))
              (backend (niwa-claude-code-register))
              (buffer (current-buffer)))
         (ignore buffer)
         (setq-local niwa--session session gptel-backend backend
                     gptel-model (car (gptel-backend-models backend)))
         ;; Cases that assert echo text capture `message' themselves.
         (cl-letf (((symbol-function 'niwa-recovery-save) #'ignore)
                   ((symbol-function 'message) #'ignore))
           ,@body)))))

(niwa-deftest niwa-readiness-changed ()
  (let* ((gptel--known-backends nil)
	 (first (gptel-make-openai-oauth "First"))
	 (second (gptel-make-openai-oauth "Second")))
    (with-temp-buffer
      (let ((session (niwa-session--create :root-buffer (current-buffer)
					      :pending-input-failure-paused t)))
	(setq-local niwa--session session gptel-backend second)
	(niwa-recovery-report session "authentication" 'authentication "Expired" t)
	;; Gates owned elsewhere survive an unrelated provider becoming ready.
	(niwa-recovery-report session "preset" 'configuration "Choose a preset" t)
	(niwa-readiness-changed first '(:status "ready"))
	(should (equal "preset" (plist-get (niwa-recovery-blocker session) :id)))
	(should (niwa-session-pending-input-failure-paused session))
        (niwa-recovery-clear session "preset")
        (setq-local gptel-backend (gptel-make-openai "Ordinary" :key "fixture" :models '(current))
                    niwa-auth--waiting-backend second)
        (niwa-recovery-report session "authentication" 'authentication "Effective provider expired" t)
        (niwa-readiness-changed first '(:status "ready"))
        (should-not (niwa-recovery-blocker session))
        (should-not niwa-auth--waiting-backend)))))

(niwa-deftest niwa-readiness-assert ()
  ,test
  (test)
  :doc "keeps the buffer's model and refuses only for owned blocking issues"
  (let* ((gptel--known-backends nil)
	 (backend (gptel-make-openai "Available" :key "fixture" :models '(current other))))
    (with-temp-buffer
      (let ((session (niwa-session--create :model-provider "Available:current")))
	(setq-local gptel-backend backend gptel-model 'other gptel-reasoning-effort 'high)
	(niwa-recovery-report session "authentication" 'authentication "Expired" t)
	(niwa-readiness-assert session)
	;; A model chosen outside the model menu is not reverted per send.
	(should (eq 'other gptel-model))
	(should (eq 'high gptel-reasoning-effort))
	(should-not (niwa-recovery-blocker session))
	(niwa-recovery-report session "preset" 'configuration "Choose a preset" t)
	(should-error (niwa-readiness-assert session) :type 'user-error))))
  :doc "shared auth failures stay raw while local projection preserves condition fields"
  (let* ((workspace (niwa-workspace--create :root "/project A/same/"))
         (session (niwa-session--create :workspace workspace))
         (failure (list 'user-error "Token expired" :provider "Codex")))
    (with-temp-buffer
      (setq-local niwa--workspace (niwa-workspace--create :root "/project B/same/"))
      (cl-letf (((symbol-function 'niwa-auth-assert-ready)
                 (lambda (_backend) (signal (car failure) (cdr failure)))))
        (should (equal failure
                       (should-error (niwa-readiness-assert session 'test-provider)
                                     :type 'user-error)))
        (should
         (equal (list 'user-error "niwa [/project A/same/]: Token expired" :provider "Codex")
                (should-error
                 (niwa-with-diagnostic-errors (niwa-diagnostic-scope session)
                   (niwa-readiness-assert session 'test-provider))
                 :type 'user-error)))))
    (let ((issue (niwa-recovery-blocker session)))
      (should (equal "Codex login needs renewal" (plist-get issue :message)))
      (should (equal "Provider sign-in is required" (niwa-recovery-public-message issue)))))
  :doc "unknown Claude readiness never refuses or starts a check"
  (test-niwa-readiness--with-claude
    (cl-letf (((symbol-function 'niwa-claude-code-launch)
               (lambda (&rest _) (ert-fail "No check expected"))))
      (niwa-readiness-assert session)
      (should-not niwa-readiness--claude)))
  :doc "a known failure is informational and a successful check resumes"
  (test-niwa-readiness--with-claude
    (let (messages)
      (niwa-readiness-record nil "Install Claude Code and run `claude auth login'" t)
      (should-not (niwa-recovery-blocker session))
      (should (equal "Install Claude Code and run `claude auth login'"
                     (plist-get (car (niwa-session-recovery-issues session)) :message)))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
		 (lambda (&rest _) (list :command (executable-find "python3")
					 :args (list niwa-test--acp-peer) :cwd temporary-file-directory)))
                ((symbol-function 'message)
                 (lambda (format &rest args) (when format (push (apply #'format format args) messages)))))
        ;; The next send is the retry; only an explicit check probes.
        (niwa-readiness-assert session)
        (should (eq 'failed (plist-get niwa-readiness--claude :state)))
        (niwa-readiness-check)
        (should (eq 'checking (plist-get niwa-readiness--claude :state)))
	(niwa-test--await 5 "Native readiness did not settle"
          (not (eq 'checking (plist-get niwa-readiness--claude :state))))
	(should (eq 'ready (plist-get niwa-readiness--claude :state)))
        (should-not (niwa-recovery-blocker session))
        (should (cl-find-if (lambda (text) (string-search "Claude is ready" text))
                            messages))
	(niwa-readiness-assert session)
	(should (string-empty-p (buffer-string))))))
  :doc "a configuration change forgets a failure"
  (test-niwa-readiness--with-claude
    (niwa-readiness-record nil "Old executable is missing")
    (niwa-recovery-clear session "authentication")
    (let ((niwa-claude-code-executable "/elsewhere/claude"))
      (niwa-readiness-assert session)
      (should-not niwa-readiness--claude))))

(niwa-deftest niwa-readiness-record-turn ()
  (test-niwa-readiness--with-claude
    (let (echoed)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (when format (setq echoed (apply #'format format args))))))
        ;; A startup that never reached its prompt shows the real cause.
        (niwa-readiness-record-turn (list :backend backend) 'error "Claude Code 2.1.290 is required")
        (should (eq 'failed (plist-get niwa-readiness--claude :state)))
        (should (string-search "2.1.290 is required" echoed))
        ;; The failed turn's own request issue reports it; no second issue.
        (should-not (niwa-session-recovery-issues session))
        ;; Reaching the prompt proves readiness, whatever the outcome.
        (niwa-readiness-record-turn (list :backend backend :niwa-acp-prompted t) 'error "Later")
        (should (eq 'ready (plist-get niwa-readiness--claude :state)))
        (should-not (niwa-recovery-blocker session))
        ;; Aborted startups and other providers teach nothing.
        (niwa-readiness-stop)
        (niwa-readiness-record-turn (list :backend backend) 'aborted nil)
        (niwa-readiness-record-turn (list :backend 'other) 'error "Other")
        (should-not niwa-readiness--claude)))))

(niwa-deftest niwa-readiness-stop ()
  (with-temp-buffer
    (let (cancelled)
      (setq niwa-readiness--claude (list :cancel (lambda () (should-not niwa-readiness--claude) (setq cancelled t))))
      (niwa-readiness-stop)
      (should cancelled)
      (should-not niwa-readiness--claude))))

(niwa-deftest niwa-readiness-runtime-changed ()
  (let ((gptel--known-backends nil)
        (saves 0))
    (cl-letf (((symbol-function 'niwa-recovery-save) (lambda (_) (cl-incf saves))))
      (with-temp-buffer
        (let ((session (niwa-session--create :root-buffer (current-buffer))))
	  (setq-local niwa--session session gptel-backend (niwa-claude-code-register))
          (with-temp-buffer
            (let ((other (niwa-session--create :root-buffer (current-buffer))))
	      (setq-local niwa--session other gptel-backend (niwa-claude-code-register))
	      (niwa-readiness-runtime-changed '(:status "failed" :message "Update failed"))
	      (should (equal "Update failed" (plist-get (car (niwa-session-recovery-issues session)) :message)))
	      (should-not (niwa-recovery-blocker session))
              ;; One save per changed session; an unchanged event saves nothing.
              (should (= 2 saves))
	      (niwa-readiness-runtime-changed '(:status "failed" :message "Update failed"))
              (should (= 2 saves))
	      (niwa-readiness-runtime-changed '(:status "ready"))
              (should (= 4 saves))
	      (should-not (niwa-session-recovery-issues session))
	      (should-not (niwa-session-recovery-issues other)))))))))

(niwa-deftest niwa-readiness-runtime-changed/fanout ()
  (let* ((gptel--known-backends nil)
	 (backend (niwa-claude-code-register))
	 buffers sessions saves notifications)
    (unwind-protect
	(progn
	  (dotimes (_ 3)
	    (let ((buffer (generate-new-buffer " *readiness-root*")))
	      (push buffer buffers)
	      (with-current-buffer buffer
		(let ((session (niwa-session--create :root-buffer buffer)))
		  (push session sessions)
		  (setq-local niwa--session session gptel-backend backend
			      niwa-readiness--claude '(:state ready :time 0))
		  (niwa-recovery-report session "runtime" 'dependency "Install failed" nil)
		  (niwa-recovery-report session "authentication" 'authentication "Login required" t)))))
	  (cl-letf (((symbol-function 'niwa-recovery-save)
		     (lambda (session) (push session saves)))
		    ((symbol-function 'niwa-collaboration-notify-request-changed)
		     (lambda (buffer) (push buffer notifications))))
	    (niwa-readiness-runtime-changed '(:status "ready")))
	  (should (= 3 (length saves)))
	  (should (= 3 (length notifications)))
	  (dolist (session sessions)
	    (should (= 1 (cl-count session saves)))
	    (should (equal "authentication" (plist-get (niwa-recovery-blocker session) :id)))
	    (should (= 1 (length (niwa-session-recovery-issues session))))
	    (should-not (buffer-local-value 'niwa-readiness--claude
					    (niwa-session-root-buffer session)))))
      (mapc #'kill-buffer buffers))))

(niwa-deftest niwa-readiness-runtime-changed/draft ()
  (niwa-view-test--with-buffers
    (let* ((gptel--known-backends nil)
           (session (niwa-session--create :root-buffer data-buf)))
      (with-current-buffer data-buf
        (setq-local niwa--session session gptel-backend (niwa-claude-code-register)))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> retained draft\nsecond line" 4))
      (cl-letf (((symbol-function 'niwa-pending-inputs-schedule-held-input-delivery) #'ignore))
        (niwa-readiness-runtime-changed '(:status "failed" :message "Update failed"))
        (niwa-readiness-runtime-changed '(:status "ready")))
      (with-current-buffer view-buf
        (should (equal "> retained draft\nsecond line" (niwa-view-composer-draft-text)))))))

(provide 'test-niwa-readiness)
;;; test-niwa-readiness.el ends here
