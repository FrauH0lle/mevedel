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
          (mevedel-claude-code--version-cache (make-hash-table :test #'equal))
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

(mevedel-deftest mevedel-claude-code-usage-launch ()
  (mevedel-claude-code-test--with-cli
    (setenv "ANTHROPIC_API_KEY" "not-a-real-key")
    (let* ((launch (mevedel-claude-code-usage-launch "sonnet"))
           (options (alist-get 'options (alist-get 'claudeCode (plist-get launch :meta))))
           (process-environment (plist-get launch :environment)))
      (should-not (getenv "ANTHROPIC_API_KEY"))
      (should (equal [] (plist-get launch :mcp)))
      (should-not (plist-get launch :session-id))
      (should-not (plist-get launch :prepare-session))
      (should (equal "usage" (plist-get launch :required-command)))
      (should (= 30 (plist-get launch :response-timeout)))
      (should (equal [] (alist-get 'tools options)))
      (should (equal [] (alist-get 'settingSources options)))
      (should-not (assq 'disable-slash-commands (alist-get 'extraArgs options)))
      (should (assq 'disable-slash-commands
                    (alist-get 'extraArgs
                               (alist-get 'options
                                          (alist-get 'claudeCode
                                                     (plist-get (mevedel-claude-code-launch "Coding" [] "sonnet" nil) :meta)))))))))

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

  :doc "asynchronous readiness receives complete output from fast status commands"
  (mevedel-claude-code-test--with-cli
    (should-not
     (mevedel-claude-code--wait
      (plist-get (mevedel-claude-code-launch "system" [] "sonnet" 'high) :prepare-launch))))

  :doc "API authentication cannot silently substitute for a subscription"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_AUTH_METHOD" "api-key")
    (should-error (mevedel-claude-code--wait
                    (plist-get (mevedel-claude-code-launch "system" [] "sonnet" 'low) :prepare-launch))
                  :type 'user-error))

  :doc "rejects an unsupported Node runtime before adapter startup"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_NODE_VERSION" "v20.0.0")
    (should-error (mevedel-claude-code--wait
                    (plist-get (mevedel-claude-code-launch "system" [] "sonnet" 'low) :prepare-launch))
                  :type 'user-error))

  :doc "rejects an outdated Emacs ACP client before adapter startup"
  (mevedel-claude-code-test--with-cli
    (let ((acp-package-version "0.14.0"))
      (should-error (mevedel-claude-code--wait
                    (plist-get (mevedel-claude-code-launch "system" [] "sonnet" 'low) :prepare-launch))
                    :type 'user-error)))

  :doc "a logged-out status exits nonzero yet still names the login action"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_LOGGED_OUT" "1")
    (should (string-search
             "Claude subscription login required"
             (error-message-string
              (should-error (mevedel-claude-code--wait
                             (plist-get (mevedel-claude-code-launch "system" [] "sonnet" 'low) :prepare-launch))
                            :type 'user-error)))))

  :doc "a failing prerequisite identifies the actual check without raw diagnostics"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_NODE_FAILURE" "1")
    (let ((failure (should-error (mevedel-claude-code--wait
                    (plist-get (mevedel-claude-code-launch "system" [] "sonnet" 'low) :prepare-launch))
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
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

  :doc "an incomplete package cannot report readiness without its bridge"
  (mevedel-claude-code-test--with-cli
    (let ((mevedel-mcp--source-directory directory) buffer)
      (unwind-protect
          (with-current-buffer (setq buffer (mevedel-claude-code-setup))
            (should (string-search "mevedel-mcp-stdio.py" (buffer-string)))
            (should-not (string-search "Ready for Claude Code subscription sessions" (buffer-string))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(mevedel-deftest mevedel-claude-code-install-adapter (:quiet t)
		 (let (forced)
		   (cl-letf (((symbol-function 'mevedel-claude-code-maintenance-check)
			      (lambda (force) (setq forced force))))
		     (mevedel-claude-code-install-adapter)
		     (should forced))))

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
            ;; Validation-only lookups neither grow the catalog nor depend on
            ;; discovery order; dispatch rejects an unavailable model.
            (let ((models (copy-sequence (gptel-backend-models backend))))
              (should (equal "missing" (gptel--model-name
                                        (plist-get (mevedel-model-resolve-provider "Claude Code:missing") :model))))
              (should (equal models (gptel-backend-models backend)))))
        (when connection (mevedel-acp-close connection))))))

(mevedel-deftest mevedel-acp-text-request/claude-model-validation (:quiet t)
  (pcase-dolist (`(,model ,effort ,reported ,levels ,diagnostic)
                '(("missing" nil "sonnet" ["low"] "unavailable")
                  ("sonnet" nil "opus" ["low"] "instead of")))
    (mevedel-claude-code-test--with-cli
      (let* ((gptel--known-backends nil)
             (backend (mevedel-claude-code-register))
             (models (copy-sequence (gptel-backend-models backend)))
             (provisional (mevedel-model-resolve-provider (concat "Claude Code:" model)))
             (launch (mevedel-claude-code-launch "Validation fixture" [] model effort))
             (peer (expand-file-name "test/fixtures/acp-agent.py" default-directory))
             done response info cancel)
        (should (eq backend (plist-get provisional :backend)))
        (should (equal models (gptel-backend-models backend)))
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
  (pcase-dolist (`(,effort ,levels ,behavior ,expected ,mode)
                '((high ["default" "low" "high"] nil "high mode:default" "plan")
                  (high ["default" "low" "high"] nil "high")
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
                            ;; User Claude settings can choose the initial mode.
                            (when mode
                              (vector `((id . "mode") (category . "mode") (type . "select")
                                        (currentValue . ,mode)
                                        (options . [((value . "default")) ((value . "plan"))]))))
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
                              (and (string-prefix-p "high" expected) 'high))))))
            (when cancel (funcall cancel))))))))

(mevedel-deftest mevedel-claude-code--command-output-async ()
  ,test
  (test)
  :doc "slow commands return immediately and permit editor timers before completion"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_STATUS_DELAY" "0.2")
    (let* ((start (float-time)) output failure tick
           (cancel (mevedel-claude-code--command-output-async
                    mevedel-claude-code-executable '("auth" "status" "--json")
                    (lambda (text) (setq output text)) (lambda (text) (setq failure text))))
           (timer (run-at-time 0.02 nil (lambda () (setq tick t)))))
      (unwind-protect
          (progn
            (should (< (- (float-time) start) 0.15))
            (with-timeout (2 (ert-fail "Editor timer did not run during readiness"))
              (while (not tick) (accept-process-output nil 0.01)))
            (should-not output)
            (should-not failure)
            (with-timeout (2 (ert-fail "Status command did not complete"))
              (while (not (or output failure)) (accept-process-output nil 0.01)))
            (should-not failure)
            (should (eq t (plist-get (json-parse-string output :object-type 'plist) :loggedIn))))
        (cancel-timer timer)
        (funcall cancel))))

  :doc "cancellation kills the subprocess and delivers no late callback"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_STATUS_DELAY" "0.2")
    (let* ((before (buffer-list)) (calls 0)
           (cancel (mevedel-claude-code--command-output-async
                    mevedel-claude-code-executable '("auth" "status" "--json")
                    (lambda (_) (cl-incf calls)) (lambda (_) (cl-incf calls)))))
      (funcall cancel)
      (accept-process-output nil 0.3)
      (should (zerop calls))
      (should-not (seq-some (lambda (buffer)
                             (and (not (memq buffer before))
                                  (string-prefix-p " *claude-status" (buffer-name buffer))))
                           (buffer-list)))
      (should-not (seq-some (lambda (process)
                             (and (process-live-p process)
                                  (string-prefix-p "mevedel-claude-status" (process-name process))))
                           (process-list)))))

  :doc "timeout cancels the command and reports one safe error"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_STATUS_DELAY" "0.2")
    (let (cancel output failure)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mevedel-transport-run-at-time)
                       (lambda (_seconds function &rest args)
                         (apply #'run-at-time 0.03 nil function args))))
              (setq cancel (mevedel-claude-code--command-output-async
                            mevedel-claude-code-executable '("auth" "status" "--json")
                            (lambda (text) (setq output text)) (lambda (text) (setq failure text)))))
            (with-timeout (2 (ert-fail "Readiness timeout did not run"))
              (while (not failure) (accept-process-output nil 0.01)))
            (should-not output)
            (should (string-search "Setup check timed out: claude auth status" failure)))
        (when cancel (funcall cancel))))))

(mevedel-deftest mevedel-claude-code--version-key ()
  ,test
  (test)
  :doc "executable replacement and npm package metadata invalidate the cache identity"
  (mevedel-claude-code-test--with-cli
    (let* ((manifest (file-name-concat directory "package.json"))
           (initial (mevedel-claude-code--version-key mevedel-claude-code-executable)))
      (write-region "{\"version\":\"1\"}" nil manifest nil 'silent)
      (let ((packaged (mevedel-claude-code--version-key mevedel-claude-code-executable)))
        (should-not (equal initial packaged))
        (write-region "{\"version\":\"2\",\"changed\":true}" nil manifest nil 'silent)
        (should-not (equal packaged (mevedel-claude-code--version-key mevedel-claude-code-executable))))
      (let ((before (mevedel-claude-code--version-key mevedel-claude-code-executable)))
        (write-region "\n# changed\n" nil mevedel-claude-code-executable t 'silent)
        (should-not (equal before (mevedel-claude-code--version-key mevedel-claude-code-executable))))))

  :doc "changing a symlink target invalidates an unchanged command path"
  (mevedel-claude-code-test--with-cli
    (let ((first (file-name-concat directory "first"))
          (second (file-name-concat directory "second"))
          (link (file-name-concat directory "launcher")))
      (copy-file mevedel-claude-code-executable first)
      (copy-file mevedel-claude-code-executable second)
      (make-symbolic-link first link)
      (let ((before (mevedel-claude-code--version-key link)))
        (delete-file link)
        (make-symbolic-link second link)
        (should-not (equal before (mevedel-claude-code--version-key link)))))))

(mevedel-deftest mevedel-claude-code--prepare-launch ()
  ,test
  (test)
  :doc "unchanged versions are cached while every dispatch rechecks subscription login"
  (mevedel-claude-code-test--with-cli
    (let ((log (file-name-concat directory "checks.log")))
      (setenv "MEVEDEL_TEST_STATUS_LOG" log)
      (mevedel-claude-code--wait (plist-get (mevedel-claude-code-launch "system" [] "sonnet" nil) :prepare-launch))
      (mevedel-claude-code--wait (plist-get (mevedel-claude-code-launch "system" [] "sonnet" nil) :prepare-launch))
      (with-temp-buffer
        (insert-file-contents log)
        (should (= 1 (how-many "^node --version$")))
        (should (= 1 (how-many "^claude --version$")))
        (should (= 2 (how-many "^claude auth status --json$"))))
      (setenv "MEVEDEL_TEST_AUTH_PROVIDER" "bedrock")
      (should-error (mevedel-claude-code--wait
                     (plist-get (mevedel-claude-code-launch "system" [] "sonnet" nil) :prepare-launch))
                    :type 'user-error)
      (with-temp-buffer
        (insert-file-contents log)
        (should (= 1 (how-many "^node --version$")))
        (should (= 3 (how-many "^claude auth status --json$"))))))

  :doc "a changed executable is version-checked again before login or model startup"
  (mevedel-claude-code-test--with-cli
    (mevedel-claude-code--wait (plist-get (mevedel-claude-code-launch "system" [] "sonnet" nil) :prepare-launch))
    (setenv "MEVEDEL_TEST_NODE_VERSION" "v20.0.0")
    (write-region "\n# changed runtime\n" nil (file-name-concat directory "node") t 'silent)
    (let ((error (should-error
                  (mevedel-claude-code--wait
                   (plist-get (mevedel-claude-code-launch "system" [] "sonnet" nil) :prepare-launch)) :type 'user-error)))
      (should (string-search "Node.js 22.0.0 or newer" (error-message-string error)))))

  :doc "fresh authentication failure prevents the adapter from starting without a fallback"
  (mevedel-claude-code-test--with-cli
    (mevedel-claude-code--wait (plist-get (mevedel-claude-code-launch "system" [] "sonnet" nil) :prepare-launch))
    (setenv "MEVEDEL_TEST_AUTH_METHOD" "api-key")
    (let* ((launch (mevedel-claude-code-launch "system" [] "sonnet" nil))
           (send (symbol-function 'acp-send-request)) connection failure started ready)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'acp-send-request)
                       (lambda (&rest args) (setq started t) (apply send args))))
              (setq connection (mevedel-acp-open launch (lambda (_) (setq ready t))
                                                  (lambda (text) (setq failure text))))
              (with-timeout (2 (ert-fail "Authentication failure did not settle"))
                (while (not failure) (accept-process-output nil 0.01))))
            (should-not ready)
            (should-not started)
            (should (eq 'closed (mevedel-acp-state connection)))
            (should (string-search "Claude subscription login required" failure)))
        (when connection (mevedel-acp-close connection)))))

  :doc "interrupting readiness cancels its subprocess before any adapter request"
  (mevedel-claude-code-test--with-cli
    (setenv "MEVEDEL_TEST_STATUS_DELAY" "0.2")
    (let* ((launch (mevedel-claude-code-launch "system" [] "sonnet" nil))
           (send (symbol-function 'acp-send-request)) connection started (failures 0))
      (unwind-protect
          (cl-letf (((symbol-function 'acp-send-request)
                     (lambda (&rest args) (setq started t) (apply send args))))
            (setq connection (mevedel-acp-open launch (lambda (_) (ert-fail "Cancelled readiness started"))
                                                (lambda (_) (cl-incf failures))))
            (mevedel-acp-close connection)
            (accept-process-output nil 0.3)
            (should (eq 'closed (mevedel-acp-state connection)))
            (should (= failures 1))
            (should-not started)
            (should-not (seq-some (lambda (process)
                                   (and (process-live-p process)
                                        (string-prefix-p "mevedel-claude-status" (process-name process))))
                                 (process-list))))
        (when connection (mevedel-acp-close connection))))))

(mevedel-deftest mevedel-claude-code--workload-control ()
  (progn
    (should (equal '(:continue t)
                   (mevedel-claude-code--workload-control
                    (lambda () (ert-fail "Only tool batches run the boundary"))
                    '(:hook_event_name "PreToolUse"))))
    (should (equal '(:hookSpecificOutput (:hookEventName "PostToolBatch" :additionalContext "Reminder"))
                   (mevedel-claude-code--workload-control
                    (lambda () "Reminder") '(:hook_event_name "PostToolBatch"))))
    (should (equal '(:continue t)
                   (mevedel-claude-code--workload-control #'ignore '(:hook_event_name "PostToolBatch"))))
    (should-error (mevedel-claude-code--workload-control
                   (lambda () (make-string 10001 ?x)) '(:hook_event_name "PostToolBatch")))
    (should-error (mevedel-claude-code--workload-control
                   #'ignore '(:hook_event_name "SessionStart" :source "startup")))))

(mevedel-deftest mevedel-claude-code--version ()
  (progn
    (should (equal "22.4.0" (mevedel-claude-code--version "v22.4.0" "22.0.0")))
    (should (equal "2.1.290" (mevedel-claude-code--version "2.1.290 (Claude Code)" "2.1.290")))
    (should-not (mevedel-claude-code--version "v20.0.0" "22.0.0"))
    (should-not (mevedel-claude-code--version "unknown" "22.0.0"))))

(provide 'test-mevedel-claude-code)
;;; test-mevedel-claude-code.el ends here
