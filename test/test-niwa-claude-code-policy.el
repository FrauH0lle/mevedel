;;; test-niwa-claude-code-policy.el --- Native request policy selection -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise request-only model selection through the ordinary composer.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)

(niwa-deftest niwa-view-send/claude-request-policy (:quiet t)
  (pcase-dolist (`(,skill-p ,selected)
		 '((nil "Claude Code:haiku") (t "Claude Code:haiku")
		   (nil "API fixture:api-model")))
    (niwa-engine-test--with-session
     :file-workspace (or skill-p (string-prefix-p "API fixture:" selected))
     (niwa-request-end)
     (let* ((view (generate-new-buffer " *claude-policy-composer*"))
	    (gptel--known-backends nil)
	    (niwa-claude-code-directory (file-name-concat root "claude"))
	    (niwa-model-workloads '((planning :provider "Claude Code:sonnet" :effort high)))
	    (niwa-skills-include-bundled nil)
	    (niwa-skills-check-for-modifications nil)
	    (skill-root (file-name-concat root "skills"))
	    (niwa-skill-dirs (list skill-root))
	    launch)
       (unwind-protect
	   (progn
	     (make-directory (file-name-concat skill-root "inspect") t)
	     (write-region "---\nname: inspect\ndescription: Inspect evidence\ncontext: inline\nmodel: \"Claude Code:opus\"\neffort: xhigh\n---\nSKILL POLICY EVIDENCE\n"
			   nil (file-name-concat skill-root "inspect" "SKILL.md") nil 'silent)
	     (niwa-claude-code-register)
	     (gptel-make-openai "API fixture" :host "127.0.0.1:9" :key "fixture" :models '(api-model))
	     (niwa-model-set-session-provider
	      session (niwa-model-resolve-provider selected) buffer)
	     (setq-local gptel-system-prompt
			 (lambda ()
			   (let ((policy (niwa-skills-request-model-policy)))
			     (format "Policy fixture %s/%s" (plist-get policy :model)
				     (plist-get policy :effort))))
			 gptel-tools nil)
	     (setf (niwa-session-plan-mode session) t)
	     (niwa-skills-install session buffer)
	     (niwa-chat-install-request-hooks)
	     (niwa-view--setup view buffer)
	     (cl-letf (((symbol-function 'niwa-claude-code-launch)
			(niwa-engine-test--claude-launch
                         (lambda (system mcp model effort &optional id _hook)
			   (should (string-search (if skill-p "Policy fixture opus/xhigh"
						    "Policy fixture sonnet/high") system))
			   (setq launch (list model effort))
			   nil))))
	       (with-current-buffer view
		 (niwa-view-composer-insert (if skill-p "$inspect this change" "Plan this change"))
		 (with-current-buffer (niwa-view-composer-buffer)
		   (niwa-view-send)))
	       (niwa-test--await 5 (format "Policy turn did not settle: %s" niwa-test--captured)
                 (not (or (zerop (niwa-session-turn-count session))
                          (niwa-turn-busy-p buffer)))))
	     (should (equal (if skill-p '("opus" xhigh) '("sonnet" high)) launch))
	     (should (equal (if (string-prefix-p "API fixture:" selected) "api-model" "haiku")
                            (gptel--model-name gptel-model)))
	     (should-not gptel-reasoning-effort)
	     (should (equal selected (niwa-session-model-provider session)))
	     (when skill-p (should (string-search "SKILL POLICY EVIDENCE" (buffer-string))))
	     (when (string-prefix-p "API fixture:" selected)
	       (setf (niwa-session-plan-mode session) nil)
               (let ((real-request (symbol-function 'gptel-request)) sent)
                 (cl-letf (((symbol-function 'gptel-request)
                            (lambda (prompt &rest options)
                              (setq sent (apply real-request prompt
                                                (append options '(:dry-run t)))))))
                   (with-current-buffer view
                     (niwa-view-composer-insert "Continue through the saved provider")
                     (with-current-buffer (niwa-view-composer-buffer)
                       (niwa-view-send))))
                 (should (equal "API fixture"
                                (gptel-backend-name (plist-get (gptel-fsm-info sent) :backend))))
                 (should-not (plist-get (cdr (assoc "root" (niwa-session-external-conversations session))) :id)))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa-view-send/claude-unavailable-policy (:quiet t)
  (dolist (route '(session preset workload))
    (niwa-engine-test--with-session
      :file-workspace (not (eq route 'session))
      (niwa-request-end)
		    (setq-local niwa-model-workloads nil)
      (let* ((gptel--known-backends nil)
             (gptel--known-presets (copy-tree gptel--known-presets))
             (niwa-preset--registry (copy-tree niwa-preset--registry))
             (niwa-model-workloads nil)
             (inhibit-interaction t)
             (niwa-claude-code-directory (file-name-concat root "claude-state"))
             (niwa-claude-code-executable (file-name-concat root "claude"))
             (niwa-claude-code-adapter-executable niwa-claude-code-executable)
             (exec-path (cons root exec-path))
             (launch (symbol-function 'niwa-claude-code-launch))
             (backend (niwa-claude-code-register))
             (missing (niwa-model-resolve-provider "Claude Code:claude-unavailable-fixture"))
             view owner selected prompt-delivered)
        (unwind-protect
            (progn
              (copy-file (file-name-concat
                          (file-name-directory niwa-test--acp-peer)
                          "claude-status.py") niwa-claude-code-executable)
              (set-file-modes niwa-claude-code-executable #o700)
              (copy-file niwa-claude-code-executable (file-name-concat root "node"))
              (niwa-tools-register)
              (niwa-model-set-session-provider
               session (if (eq route 'session) missing
                         (niwa-model-resolve-provider "Claude Code:sonnet")) buffer)
              (pcase route
                ('preset
                 (niwa-define-preset claude-unavailable-fixture
                   :backend backend :model (plist-get missing :model) :tools nil)
                 (niwa-preset-apply 'claude-unavailable-fixture))
                ('workload
                 (setq-local niwa-model-workloads
                             '((planning :provider "Claude Code:claude-unavailable-fixture")))
                 (setf (niwa-session-plan-mode session) t)))
              (setq-local gptel-system-prompt "Unavailable model fixture" gptel-tools nil)
              (setq view (niwa-view--ensure buffer))
              (cl-letf (((symbol-function 'gptel-request)
                         (lambda (&rest _) (ert-fail "Unavailable model fell back to API")))
                        ((symbol-function 'niwa-claude-code-launch)
                         (lambda (system mcp model effort &optional id hook)
                           (setq selected model owner niwa--current-request)
                           (let* ((config (funcall launch system mcp model effort id hook))
                                  (observe (plist-get config :observe)))
                             (plist-put config :command (executable-find "python3"))
                             (plist-put config :args (list niwa-test--acp-peer))
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
                (with-current-buffer view
                  (niwa-view-composer-insert "This must never be sampled")
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send)))
                (niwa-test--await 5 "Unavailable-model request did not settle"
                  (not (niwa-turn-busy-p buffer))))
              (ert-info ((format "Unavailable route: %s" route))
                (should (equal "claude-unavailable-fixture" selected))
                (should-not prompt-delivered)
                (should-not (string-search "FORBIDDEN MODEL PROMPT" (buffer-string)))
                (let ((error (plist-get (niwa-engine-info owner) :error)))
                  (should (string-search "unavailable" error))
					(should (string-search "select an available provider before continuing" error)))
                (should-not niwa--current-request)
                ;; Selection-time lookups accept the ID without adding it to
                ;; the discovered catalog; each dispatch rejects it as above.
                (should-not (cl-find "claude-unavailable-fixture" (gptel-backend-models backend)
                                     :key #'gptel--model-name :test #'equal))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(provide 'test-niwa-claude-code-policy)
;;; test-niwa-claude-code-policy.el ends here
