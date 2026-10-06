;;; test-mevedel-claude-code.el --- Claude subscription launch contracts -*- lexical-binding: t -*-

;;; Commentary:
;; Authentication checks use the supported CLI status surface, never credentials.

;;; Code:

(require 'mevedel-claude-code)
(require 'mevedel-acp)
(require 'mevedel-models)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defconst mevedel-claude-code-test--cli
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "claude-status.py"))

(defmacro mevedel-claude-code-test--with-cli (&rest body)
  "Run BODY with temporary executables and isolated subscription state."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "mevedel-claude-setup-" t))
          (gptel--known-backends (copy-tree gptel--known-backends))
          (mevedel-claude-code-directory directory)
          (mevedel-claude-code-executable (file-name-concat directory "claude"))
          (mevedel-claude-code-adapter-executable mevedel-claude-code-executable)
          (exec-path (cons directory exec-path))
          (process-environment (copy-sequence process-environment)))
     (unwind-protect
         (progn
           (copy-file mevedel-claude-code-test--cli mevedel-claude-code-executable)
           (set-file-modes mevedel-claude-code-executable #o700)
           (copy-file mevedel-claude-code-executable (file-name-concat directory "node"))
           ,@body)
       (delete-directory directory t))))

(mevedel-deftest mevedel-claude-code-launch ()
  ,test
  (test)
  :doc "supported login launches isolated tools without inherited API billing routes"
  (mevedel-claude-code-test--with-cli
    (setenv "ANTHROPIC_API_KEY" "fixture-not-a-real-key")
    (setenv "CLAUDE_CODE_USE_BEDROCK" "1")
    (setenv "ANTHROPIC_MODEL" "opus")
    (setenv "CLAUDE_CODE_EFFORT_LEVEL" "low")
    (let* ((launch (mevedel-claude-code-launch "Mevedel system" [] "sonnet" 'low nil "native-hook"))
           (meta (plist-get launch :meta))
           (options (alist-get 'options (alist-get 'claudeCode meta)))
           (process-environment (plist-get launch :environment)))
      (should-not (getenv "ANTHROPIC_API_KEY"))
      (should-not (getenv "CLAUDE_CODE_USE_BEDROCK"))
      (should (equal mevedel-claude-code-executable (getenv "CLAUDE_CODE_EXECUTABLE")))
      (should (equal "sonnet" (getenv "ANTHROPIC_MODEL")))
      (should-not (getenv "CLAUDE_CODE_EFFORT_LEVEL"))
      (should (equal '((type . "custom") (prompt . "Mevedel system") (snapshot . :false))
                     (alist-get 'systemPrompt options)))
      (should (equal [] (alist-get 'tools options)))
      (should (equal [] (alist-get 'settingSources options)))
      (should (eq t (alist-get 'strictMcpConfig options)))
      (should (eq t (alist-get 'includeHookEvents options)))
      (should (functionp (plist-get launch :observe)))
      (should (functionp (plist-get launch :check-context)))
      (let ((entry (aref (alist-get 'PreToolUse (alist-get 'hooks (alist-get 'settings options))) 0)))
        (should (equal "native-hook" (alist-get 'command (aref (alist-get 'hooks entry) 0)))))
      (let ((entry (aref (alist-get 'SessionStart (alist-get 'hooks (alist-get 'settings options))) 0)))
        (should (equal "compact" (alist-get 'matcher entry)))
        (should (equal "native-hook" (alist-get 'command (aref (alist-get 'hooks entry) 0)))))
      (should (equal "sonnet" (alist-get 'model options)))
      (should-not (assq 'effort options))
      (should (functionp (plist-get launch :prepare-session)))
      (should (equal "native-hook"
                     (alist-get 'command
                                (aref (alist-get 'hooks
                                                 (aref (alist-get 'PostToolBatch
                                                                  (alist-get 'hooks (alist-get 'settings options))) 0)) 0))))
      (should (file-directory-p (plist-get launch :cwd)))
      (should-not (equal default-directory (plist-get launch :cwd)))
      (should-not (assq 'bare (alist-get 'extraArgs options)))))

  :doc "preflight drains output when status commands exit before the first read"
  (mevedel-claude-code-test--with-cli
    (let ((make (symbol-function 'make-process)))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (let ((process (apply make args))
                         (deadline (+ (float-time) 5)))
                     ;; Let the real child exit without servicing its output,
                     ;; as can happen while the editor is busy between reads.
                     (while (and (process-live-p process)
                                 (< (float-time) deadline)))
                     (when (process-live-p process)
                       (delete-process process)
                       (error "Fixture status command failed to exit"))
                     process))))
        (should (plist-get
                 (mevedel-claude-code-launch "system" [] "sonnet" 'high)
                 :command)))))

  :doc "preflight drains output after a read wakes without output as the child exits"
  (mevedel-claude-code-test--with-cli
    (let ((accept (symbol-function 'accept-process-output)))
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (process &rest args)
                   (if (process-get process 'fixture-woke)
                       (apply accept process args)
                     (process-put process 'fixture-woke t)
                     (let ((deadline (+ (float-time) 5)))
                       (while (and (process-live-p process)
                                   (< (float-time) deadline)))
                       (when (process-live-p process)
                         (error "Fixture status command failed to exit")))
                     nil))))
        (should (plist-get
                 (mevedel-claude-code-launch "system" [] "sonnet" 'high)
                 :command)))))

  :doc "API authentication cannot silently substitute for a subscription"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_AUTH_METHOD" "api-key")
    (should-error (mevedel-claude-code-launch "system" [] "sonnet" 'low)
                  :type 'user-error))

  :doc "rejects an unsupported Node runtime before adapter startup"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_NODE_VERSION" "v20.0.0")
    (should-error (mevedel-claude-code-launch "system" [] "sonnet" 'low)
                  :type 'user-error))

  :doc "rejects an outdated Emacs ACP client before adapter startup"
  (mevedel-claude-code-test--with-cli
    (let ((acp-package-version "0.14.0"))
      (should-error (mevedel-claude-code-launch "system" [] "sonnet" 'low)
                    :type 'user-error)))

  :doc "an incomplete package cannot report readiness without its bridge"
  (mevedel-claude-code-test--with-cli
    (let ((mevedel-mcp--source-directory directory))
      (should-error (mevedel-claude-code-launch "system" [] "sonnet" 'low)
                    :type 'user-error)))

  :doc "a failing prerequisite identifies the actual check without raw diagnostics"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_NODE_FAILURE" "1")
    (let ((failure (should-error (mevedel-claude-code-launch "system" [] "sonnet" 'low)
                                 :type 'user-error)))
      (should (string-search "node --version" (error-message-string failure)))
      (should-not (string-search "PRIVATE-DIAGNOSTIC" (error-message-string failure))))))

(mevedel-deftest mevedel-claude-code-setup (:quiet t)
  ,test
  (test)
  :doc "checks a supported installation and shows normal provider selection"
  (mevedel-claude-code-test--with-cli
    (let* ((gptel--known-backends nil)
           (backend (mevedel-claude-code-register)) buffer)
      (unwind-protect
          (progn
            (setq buffer (mevedel-claude-code-setup))
            (with-current-buffer buffer
              (should (string-search "Ready for Claude Code subscription sessions" (buffer-string)))
              (should (string-search "Claude Code:sonnet" (buffer-string))))
            (should (mevedel-claude-code-backend-p (cdr (assoc "Claude Code" gptel--known-backends))))
            (should (eq backend (cdr (assoc "Claude Code" gptel--known-backends)))))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  :doc "an unsupported login stays actionable without starting a model"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_AUTH_METHOD" "api-key")
    (let (buffer)
      (unwind-protect
          (progn
            (setq buffer (mevedel-claude-code-setup))
            (with-current-buffer buffer
              (should (string-search "Claude subscription login required" (buffer-string)))
              (should (string-search "claude auth login" (buffer-string)))
              (should-not (string-search "Ready for Claude Code subscription sessions" (buffer-string)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(mevedel-deftest mevedel-claude-code-install-adapter (:quiet t)
  (dolist (answer '(yes no failure))
    (mevedel-claude-code-test--with-cli
      (let ((mevedel-claude-code-adapter-executable nil)
            (mevedel-claude-code-directory (file-name-concat directory "adapter with spaces"))
            setup log process prompt)
        (copy-file mevedel-claude-code-executable (file-name-concat directory "npm"))
        (when (eq answer 'failure) (setenv "MEVEDEL_TEST_INSTALL_FAIL" "1"))
        (unwind-protect
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (question) (setq prompt question) (not (eq answer 'no)))))
              (setq setup (mevedel-claude-code-setup))
              (with-current-buffer setup
                (goto-char (point-min))
                (should (search-forward "Install/update adapter" nil t))
                (setq process (button-activate (button-at (1- (point))))))
              (should (string-search "@agentclientprotocol/claude-agent-acp@0.86.0" prompt))
              (should (string-search mevedel-claude-code-directory prompt))
              (if (eq answer 'no)
                  (should-not (file-exists-p mevedel-claude-code-directory))
                (should (process-live-p process))
                (setq log (process-buffer process))
                (should-error (mevedel-claude-code-install-adapter) :type 'user-error)
                (with-timeout (5 (ert-fail "Adapter installation did not finish"))
                  (while (process-live-p process) (accept-process-output process 0.01)))
                (should (= (if (eq answer 'failure) 1 0) (process-exit-status process)))
                (with-temp-buffer
                  (insert-file-contents (file-name-concat mevedel-claude-code-directory "install-invocation.json"))
                  (let ((args (json-parse-buffer :array-type 'list)))
                    (should (member "@agentclientprotocol/claude-agent-acp@0.86.0" args))
                    (should (equal mevedel-claude-code-directory (cadr (member "--prefix" args))))))
                (with-current-buffer log
                  (should (string-search (if (eq answer 'failure) "Installation failed" "Installation complete")
                                         (buffer-string))))
                (setq setup (mevedel-claude-code-setup))
                (with-current-buffer setup
                  (should (eq (eq answer 'yes)
                              (not (null (string-search "Ready for Claude Code subscription sessions" (buffer-string)))))))))
          (when (and process (process-live-p process)) (delete-process process))
          (when (buffer-live-p setup) (kill-buffer setup))
          (when (buffer-live-p log) (kill-buffer log)))))))

(mevedel-deftest mevedel-model-supported-efforts/claude-aliases (:quiet t)
  (let ((gptel--known-backends nil))
    (mevedel-claude-code-register)
    (dolist (name '("sonnet" "opus" "fable" "haiku"))
      (should (assoc (concat "Claude Code:" name) (mevedel-model-candidates)))
      (let ((model (plist-get (mevedel-model-resolve-provider (concat "Claude Code:" name)) :model)))
        (should (equal (unless (equal name "haiku") '(low medium high xhigh max))
                       (mevedel-model-supported-efforts model)))))))

(mevedel-deftest mevedel-model-candidates/claude-discovery (:quiet t)
  (mevedel-claude-code-test--with-cli
    (let* ((gptel--known-backends nil)
           (backend (mevedel-claude-code-register))
           (peer (expand-file-name "test/fixtures/acp-agent.py" default-directory))
           (launch (mevedel-claude-code-launch "Catalog fixture" [] "sonnet" 'max))
           ready failure connection)
      (plist-put launch :command (executable-find "python3"))
      (plist-put launch :args (list peer))
      (plist-put launch :meta
                 '((sessionInfo .
                    ((configOptions .
                      [((id . "model") (category . "model") (type . "select")
                        (currentValue . "sonnet")
                        (options . [((value . "sonnet") (name . "Sonnet latest"))
                                    ((value . "claude-fixture-5") (name . "Fixture 5"))]))
                       ((id . "effort") (category . "thought_level") (type . "select")
                        (currentValue . "max")
                        (options . [((value . "default") (name . "Default"))
                                    ((value . "low") (name . "Low"))
                                    ((value . "max") (name . "Maximum"))]))])))))
      (unwind-protect
          (progn
            (setq connection
                  (mevedel-acp-open launch (lambda (_) (setq ready t))
                                    (lambda (message) (setq failure message))))
            (with-timeout (5 (ert-fail "Model discovery did not settle"))
              (while (not (or ready failure)) (accept-process-output nil 0.01)))
            (should-not failure)
            ;; Session initialization discovers choices without any prompt.
            (dolist (name '("Claude Code:sonnet" "Claude Code:opus" "Claude Code:haiku"
                            "Claude Code:claude-fixture-5"))
              (should (assoc name (mevedel-model-candidates))))
            (let ((model (plist-get (mevedel-model-resolve-provider "Claude Code:sonnet") :model)))
              (should (equal '(low max) (mevedel-model-supported-efforts model)))
              (should-error (mevedel-model-validate-effort model 'high) :type 'user-error))
            (should (eq backend (plist-get (mevedel-model-resolve-provider "Claude Code:claude-fixture-5") :backend)))
            (should-error (mevedel-model-resolve-provider "Claude Code:missing") :type 'user-error))
        (when connection (mevedel-acp-close connection))))))

(mevedel-deftest mevedel-acp-text-request/claude-model-validation (:quiet t)
  (pcase-dolist (`(,model ,effort ,reported ,levels ,diagnostic)
                '(("missing" nil "sonnet" ["low"] "unavailable")
                  ("sonnet" nil "opus" ["low"] "instead of")))
    (mevedel-claude-code-test--with-cli
      (let* ((gptel--known-backends nil)
             (backend (mevedel-claude-code-register))
             (provisional (mevedel-model-resolve-provider (concat "Claude Code:" model)))
             (launch (mevedel-claude-code-launch "Validation fixture" [] model effort))
             (peer (expand-file-name "test/fixtures/acp-agent.py" default-directory))
             done response info cancel)
        (should (eq backend (plist-get provisional :backend)))
        (plist-put launch :command (executable-find "python3"))
        (plist-put launch :args (list peer))
        (plist-put launch :meta
                   `((sessionInfo .
                      ((configOptions .
                        [((category . "model") (type . "select")
                          (currentValue . ,reported)
                          (options . [((value . "sonnet") (name . "Sonnet"))
                                      ((value . "opus") (name . "Opus"))]))
                         ((category . "thought_level") (type . "select")
                          (currentValue . "high")
                          (options . ,(vconcat (mapcar (lambda (value) `((value . ,value))) levels))))])))))
        (unwind-protect
            (progn
              (setq cancel (mevedel-acp-text-request
                            launch "This prompt must never reach the model"
                            (lambda (result metadata)
                              (setq done t response result info metadata))))
              (with-timeout (5 (ert-fail "Model validation did not settle"))
                (while (not done) (accept-process-output nil 0.01)))
              (should-not response)
              (should (string-search diagnostic (plist-get info :error))))
          (when cancel (funcall cancel)))))))

(mevedel-deftest mevedel-claude-code--prepare-session (:quiet t)
  (pcase-dolist (`(,effort ,levels ,behavior ,expected)
                '((high ["default" "low" "high"] nil "high")
                  (high ["default" "low"] nil "default")
                  (nil ["default" "low" "high"] nil "default")
                  (high nil nil "unset")
                  (high ["default" "high"] "mismatch" "acknowledge")
                  (high ["default" "high"] "error" "rejected")
                  (high ["default" "high"] "wait" "closed")))
    (mevedel-claude-code-test--with-cli
      (with-temp-buffer
        (let* ((gptel--known-backends nil)
               (backend (mevedel-claude-code-register))
               (model (plist-get (mevedel-model-resolve-provider "Claude Code:opus") :model))
               (launch (mevedel-claude-code-launch "Effort fixture" [] "opus" effort))
               (peer (expand-file-name "test/fixtures/acp-agent.py" default-directory))
               done response info cancel)
          (setq-local gptel-backend backend gptel-model model)
          (setq-local mevedel--session
                      (mevedel-session--create :root-buffer (current-buffer)))
          ;; This is the same pre-connection selection used by the model menu.
          (mevedel-model-set-session-effort mevedel--session effort)
          (plist-put launch :command (executable-find "python3"))
          (plist-put launch :args (list peer))
          (plist-put launch :meta
                     `((configBehavior . ,behavior)
                       (sessionInfo .
                        ((configOptions .
                          ,(vconcat
                            [((id . "model") (category . "model") (type . "select")
                              (currentValue . "opus") (options . [((value . "opus"))]))]
                            (when levels
                              (vector `((id . "effort") (category . "thought_level") (type . "select")
                                        (currentValue . "low")
                                        (options . ,(vconcat (mapcar (lambda (value) `((value . ,value))) levels))))))))))))
          (unwind-protect
              (progn
                (setq cancel (mevedel-acp-text-request
                              launch "report-effort"
                              (lambda (result metadata)
                                (setq done t response result info metadata))))
                (when (equal behavior "wait")
                  ;; Interrupt startup, including a pending configuration request.
                  (accept-process-output nil 0.2)
                  (funcall cancel))
                (with-timeout (5 (ert-fail "Effort configuration did not settle"))
                  (while (not done) (accept-process-output nil 0.01)))
                (cond
                 ((equal behavior "wait")
                  (should (eq response 'abort)))
                 (behavior
                  (should-not response)
                  (should (string-search expected (plist-get info :error))))
                 (t
                  ;; The peer reports only values applied by a real config request.
                  (should (equal expected response))
                  (should-not (plist-get info :error))
                  (should (eq (mevedel-session-reasoning-effort mevedel--session)
                              gptel-reasoning-effort))
                  (should (eq gptel-reasoning-effort
                              (and (equal expected "high") 'high))))))
            (when cancel (funcall cancel))))))))

(provide 'test-mevedel-claude-code)
;;; test-mevedel-claude-code.el ends here
