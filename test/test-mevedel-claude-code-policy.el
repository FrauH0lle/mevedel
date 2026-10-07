;;; test-mevedel-claude-code-policy.el --- Native request policy selection -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise request-only model selection through the ordinary composer.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(defconst mevedel-claude-code-policy-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-view-send/claude-request-policy (:quiet t)
  (pcase-dolist (`(,skill-p ,selected)
		 '((nil "Claude Code:haiku") (t "Claude Code:haiku")
		   (nil "API fixture:api-model")))
    (mevedel-engine-test--with-session
     (mevedel-request-end)
     (let* ((view (generate-new-buffer " *claude-policy-composer*"))
	    (gptel--known-backends nil)
	    (mevedel-claude-code-directory (file-name-concat root "claude"))
	    (mevedel-model-workloads '((planning :provider "Claude Code:sonnet" :effort high)))
	    (mevedel-skills-include-bundled nil)
	    (mevedel-skills-check-for-modifications nil)
	    (skill-root (file-name-concat root "skills"))
	    (mevedel-skill-dirs (list skill-root))
	    launch)
       (unwind-protect
	   (progn
	     (make-directory (file-name-concat skill-root "inspect") t)
	     (write-region "---\nname: inspect\ndescription: Inspect evidence\ncontext: inline\nmodel: \"Claude Code:opus\"\neffort: xhigh\n---\nSKILL POLICY EVIDENCE\n"
			   nil (file-name-concat skill-root "inspect" "SKILL.md") nil 'silent)
	     (mevedel-claude-code-register)
	     (gptel-make-openai "API fixture" :host "127.0.0.1:9" :key "fixture" :models '(api-model))
	     (mevedel-model-set-session-provider
	      session (mevedel-model-resolve-provider selected) buffer)
	     (setq-local gptel-system-prompt
			 (lambda ()
			   (let ((policy (mevedel-skills-request-model-policy)))
			     (format "Policy fixture %s/%s" (plist-get policy :model)
				     (plist-get policy :effort))))
			 gptel-tools nil)
	     (setf (mevedel-session-plan-mode session) t)
	     (mevedel-skills-install session buffer)
	     (mevedel-chat-install-request-hooks)
	     (mevedel-view--setup view buffer)
	     (cl-letf (((symbol-function 'mevedel-claude-code-launch)
			(mevedel-engine-test--claude-launch
                         (lambda (system mcp model effort &optional id _hook)
			   (should (string-search (if skill-p "Policy fixture opus/xhigh"
						    "Policy fixture sonnet/high") system))
			   (setq launch (list model effort))
			   nil))))
	       (with-current-buffer view
		 (goto-char (mevedel-view--input-start))
		 (insert (if skill-p "$inspect this change" "Plan this change"))
		 (mevedel-view-send))
	       (with-timeout (5 (ert-fail (format "Policy turn did not settle: %s" mevedel-test--captured)))
		 (while (or (zerop (mevedel-session-turn-count session))
			    (mevedel-turn-busy-p buffer))
		   (accept-process-output nil 0.01))))
	     (should (equal (if skill-p '("opus" xhigh) '("sonnet" high)) launch))
	     (should (equal (if (string-prefix-p "API fixture:" selected) "api-model" "haiku")
                            (gptel--model-name gptel-model)))
	     (should-not gptel-reasoning-effort)
	     (should (equal selected (mevedel-session-model-provider session)))
	     (when skill-p (should (string-search "SKILL POLICY EVIDENCE" (buffer-string))))
	     (when (string-prefix-p "API fixture:" selected)
	       (setf (mevedel-session-plan-mode session) nil)
               (let ((real-request (symbol-function 'gptel-request)) sent)
                 (cl-letf (((symbol-function 'gptel-request)
                            (lambda (prompt &rest options)
                              (setq sent (apply real-request prompt
                                                (append options '(:dry-run t)))))))
                   (with-current-buffer view
                     (goto-char (mevedel-view--input-start))
                     (insert "Continue through the saved provider")
                     (mevedel-view-send)))
                 (should (equal "API fixture"
                                (gptel-backend-name (plist-get (gptel-fsm-info sent) :backend))))
                 (should-not (assoc "root" (mevedel-session-external-conversations session))))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel-view-send/claude-unavailable-policy (:quiet t)
  (dolist (route '(session preset workload directive))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (let* ((gptel--known-backends nil)
             (gptel--known-presets (copy-tree gptel--known-presets))
             (mevedel-preset--registry (copy-tree mevedel-preset--registry))
             (mevedel-model-workloads nil)
             (mevedel-show-chat-buffer nil)
             (inhibit-interaction t)
             (mevedel-claude-code-directory (file-name-concat root "claude-state"))
             (mevedel-claude-code-executable (file-name-concat root "claude"))
             (mevedel-claude-code-adapter-executable mevedel-claude-code-executable)
             (exec-path (cons root exec-path))
             (launch (symbol-function 'mevedel-claude-code-launch))
             (backend (mevedel-claude-code-register))
             (missing (mevedel-model-resolve-provider "Claude Code:claude-unavailable-fixture"))
             view source owner selected prompt-delivered directive-error)
        (unwind-protect
            (progn
              (copy-file (file-name-concat
                          (file-name-directory mevedel-claude-code-policy-test--peer)
                          "claude-status.py") mevedel-claude-code-executable)
              (set-file-modes mevedel-claude-code-executable #o700)
              (copy-file mevedel-claude-code-executable (file-name-concat root "node"))
              (mevedel-tools-register)
              (mevedel-model-set-session-provider
               session (if (eq route 'session) missing
                         (mevedel-model-resolve-provider "Claude Code:sonnet")) buffer)
              (pcase route
                ('preset
                 (mevedel-define-preset claude-unavailable-fixture
                   :backend backend :model (plist-get missing :model) :tools nil)
                 (mevedel-preset-apply 'claude-unavailable-fixture))
                ('workload
                 (setq-local mevedel-model-workloads
                             '((planning :provider "Claude Code:claude-unavailable-fixture")))
                 (setf (mevedel-session-plan-mode session) t)))
              (setq-local gptel-system-prompt "Unavailable model fixture" gptel-tools nil)
              (setq view (mevedel-view--ensure buffer))
              (cl-letf (((symbol-function 'gptel-request)
                         (lambda (&rest _) (ert-fail "Unavailable model fell back to API")))
                        ((symbol-function 'mevedel-claude-code-launch)
                         (lambda (system mcp model effort &optional id hook)
                           (setq selected model owner mevedel--current-request)
                           (let* ((config (funcall launch system mcp model effort id hook))
                                  (observe (plist-get config :observe)))
                             (plist-put config :command (executable-find "python3"))
                             (plist-put config :args (list mevedel-claude-code-policy-test--peer))
                             (plist-put config :observe
                                        (lambda (request notification)
                                          (when (equal "user"
                                                       (alist-get 'type
                                                                  (alist-get 'message
                                                                             (alist-get 'params notification))))
                                            (setq prompt-delivered t))
                                          (funcall observe request notification)))
                             (plist-put config :meta
                                        '((sessionInfo .
                                           ((configOptions .
                                             [((category . "model") (type . "select")
                                               (currentValue . "sonnet")
                                               (options . [((value . "sonnet") (name . "Sonnet"))]))])))
                                          (responseText . "FORBIDDEN MODEL PROMPT")))
                             config))))
                (if (eq route 'directive)
                    (progn
                      (mevedel-tools-register)
                      (mevedel--define-presets)
                      (setq source (find-file-noselect (file-name-concat root "subject.txt")))
                      (with-current-buffer source
                        (setq-local mevedel--workspace workspace)
                        (insert "subject\n") (save-buffer)
                        (let* ((directive (mevedel--create-directive-in
                                           source (point-min) (1- (point-max)) nil "Inspect subject"))
                               (record (mevedel--directive-record directive)))
                          (setf (mevedel-directive-session-id record) (mevedel-session-session-id session))
                          (overlay-put directive 'mevedel-directive-model-provider
                                       "Claude Code:claude-unavailable-fixture")
                          (mevedel--start-directive-discussion
                           directive (lambda (error _owner) (setq directive-error error))))))
                  (with-current-buffer view
                    (goto-char (mevedel-view--input-start))
                    (insert "This must never be sampled")
                    (mevedel-view-send)))
                (with-timeout (5 (ert-fail "Unavailable-model request did not settle"))
                  (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
              (ert-info ((format "Unavailable route: %s" route))
                (should (equal "claude-unavailable-fixture" selected))
                (should-not prompt-delivered)
                (should-not (string-search "FORBIDDEN MODEL PROMPT" (buffer-string)))
                (let ((error (plist-get (mevedel-engine-info owner) :error)))
                  (should (string-search "unavailable" error))
                  (should (string-search "select a listed model and retry" error))
                  (when (eq route 'directive) (should (equal error directive-error))))
                (should-not mevedel--current-request)
                ;; Selection-time lookups accept the ID without adding it to
                ;; the discovered catalog; each dispatch rejects it as above.
                (should-not (cl-find "claude-unavailable-fixture" (gptel-backend-models backend)
                                     :key #'gptel--model-name :test #'equal))))
          (when (buffer-live-p view) (kill-buffer view))
          (when (buffer-live-p source) (kill-buffer source)))))))

(provide 'test-mevedel-claude-code-policy)
;;; test-mevedel-claude-code-policy.el ends here
