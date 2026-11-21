;;; test-niwa-agents.el --- Tests for niwa-agents.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for built-in agent definitions and agent registry helpers.

;;; Code:

(require 'niwa-agents)
(require 'gptel-openai)
(require 'niwa-context-delivery)
(require 'niwa-agent-control)
(require 'niwa-execution-target)
(require 'niwa-hooks)
(require 'niwa-reminders)
(require 'niwa-skills-core)
(require 'niwa-system)
(require 'niwa-tool-ui)
(require 'niwa-tools)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel-reasoning-effort)

(defmacro test-niwa-agents--with-tiers (&rest body)
  "Run BODY with an isolated backend and two configured model tiers."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil)
         (niwa-model-fallback-provider nil))
     (gptel-make-openai "Team" :key "test"
                        :models '(small-model big-model other-model))
     (let ((niwa-model-tiers
            '((fast :provider "Team:small-model" :effort nil)
              (strong :provider "Team:big-model" :effort high)))
           (gptel-backend (gptel-get-backend "Team"))
           (gptel-model 'other-model)
           (gptel-reasoning-effort 'medium))
       ,@body)))


;;
;;; Agent definitions

(defun test-niwa-agents--resolved-tool-names (agent-name)
  "Return every available tool name for AGENT-NAME, across both routes."
  (let ((resolved (niwa-tool-resolve
                   (niwa-agent--effective-specs (niwa-agent-get agent-name)))))
    (mapcar #'niwa-tool-name
            (append (plist-get resolved :active) (plist-get resolved :discoverable)))))

(defun test-niwa-agents--restore-builtins ()
  "Restore bundled agent definitions after tests that clear the registry."
  (unless (niwa-agent-get "explorer")
    (load-file (locate-library "niwa-agents")))
  (niwa-tools-register))

(niwa-deftest niwa-plan-read-only-p ()
  ,test
  (test)
  :doc "uses an explicit root session"
  (let ((session (niwa-session--create :name "main" :plan-mode t)))
    (should (niwa-plan-read-only-p session)))

  :doc "keeps a captured Plan request read-only without sticky session Plan"
  (let ((session (niwa-session--create :name "main"))
        (niwa--current-request
         (niwa-request--create :plan-read-only t)))
    (should (niwa-plan-read-only-p session)))

  :doc "uses a retained agent's parent session"
  (let ((session (niwa-session--create :name "main" :plan-mode t)))
    (with-temp-buffer
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create
                   :parent-session session))
      (should (niwa-plan-read-only-p))))

  :doc "uses immutable Plan authority on a retained agent"
  (let ((session (niwa-session--create :name "main")))
    (with-temp-buffer
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create
                   :parent-session session :plan-read-only t))
      (should (niwa-plan-read-only-p))))

  :doc "is nil without a stamp or a sticky Plan session"
  (let ((session (niwa-session--create :name "main"))
        (niwa--current-request (niwa-request--create)))
    (should-not (niwa-plan-read-only-p session))
    (with-temp-buffer
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create :parent-session session))
      (should-not (niwa-plan-read-only-p)))))

(niwa-deftest niwa-agent-invocation-require-path
  ()
  ,test
  (test)
  :doc "returns canonical invocation paths and rejects missing or malformed paths"
  (let ((invocation
         (niwa-agent-invocation--create
          :agent-id "default--opaque"
          :path "/root/worker")))
    (should (equal "/root/worker"
                   (niwa-agent-invocation-require-path invocation)))
    (setf (niwa-agent-invocation-path invocation) nil)
    (should-error (niwa-agent-invocation-require-path invocation))
    (setf (niwa-agent-invocation-path invocation) "/root/Upper")
    (should-error (niwa-agent-invocation-require-path invocation))))

(niwa-deftest niwa-agent-request-locals-p
  ()
  ,test
  (test)
  :doc "accepts unique closed-schema subsets and complete configurations"
  (let ((complete
         (mapcar (lambda (symbol) (cons symbol nil))
                 niwa-agent-request-local-symbols)))
    (should (niwa-agent-request-locals-p
             (list (car complete))))
    (should (niwa-agent-request-locals-p complete t)))
  :doc "rejects unknown, duplicate, and incomplete configuration keys"
  (let ((complete
         (mapcar (lambda (symbol) (cons symbol nil))
                 niwa-agent-request-local-symbols)))
    (should-not
     (niwa-agent-request-locals-p '((kill-buffer-hook ignore))))
    (should-not
     (niwa-agent-request-locals-p
      (cons (car complete) complete)))
    (should-not
     (niwa-agent-request-locals-p (cdr complete) t))))

(niwa-deftest niwa-agent-configuration-model-metadata ()
  ,test
  (test)
  :doc "reports the frozen backend, model and effort as strings"
  (let ((backend (gptel--make-backend :name "Metadata Provider")))
    (should (equal '(:backend "Metadata Provider" :model "metadata-model"
                    :effort "high")
                   (niwa-agent-configuration-model-metadata
                    (niwa-agent-configuration--create
                     :request-locals `((gptel-backend . ,backend)
                                       (gptel-model . metadata-model)
                                       (gptel-reasoning-effort . high))))))
    ;; No selected effort means the provider default, which is unknown here.
    (should (equal '(:backend "Metadata Provider" :model "metadata-model"
                    :effort nil)
                   (niwa-agent-configuration-model-metadata
                    (niwa-agent-configuration--create
                     :request-locals `((gptel-backend . ,backend)
                                       (gptel-model . "metadata-model")))))))
  :doc "reports nothing without a frozen backend and model"
  (let ((backend (gptel--make-backend :name "Metadata Provider")))
    (should-not (niwa-agent-configuration-model-metadata nil))
    (should-not (niwa-agent-configuration-model-metadata
                 (niwa-agent-configuration--create
                  :request-locals '((gptel-model . metadata-model)))))
    (should-not (niwa-agent-configuration-model-metadata
                 (niwa-agent-configuration--create
                  :request-locals `((gptel-backend . ,backend)))))))

(niwa-deftest niwa-agent-model-metadata-p ()
  ,test
  (test)
  :doc "accepts complete metadata with or without a known effort"
  (progn
    (should (niwa-agent-model-metadata-p
             '(:backend "B" :model "m" :effort "low")))
    (should (niwa-agent-model-metadata-p
             '(:backend "B" :model "m" :effort nil))))
  :doc "rejects partial, extra and non-string fields"
  (dolist (value '(nil (:backend "B" :model "m")
                   (:backend "B" :model m :effort nil)
                   (:backend "B" :model "m" :effort low)
                   (:backend "B" :model "m" :effort nil :extra t)
                   (:backend "B" :model "m" . "x")))
    (should-not (niwa-agent-model-metadata-p value))))

(niwa-deftest niwa-agent--effective-specs/test
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "delegating roles receive skill tools while review and verify do not"
  (dolist (name '("worker" "explorer"))
    (let ((tools (test-niwa-agents--resolved-tool-names name)))
      (should (member "Skill" tools))
      (should (member "ListSkills" tools))))
  (dolist (name '("verifier" "reviewer"))
    (let ((tools (test-niwa-agents--resolved-tool-names name)))
      (should-not (member "Skill" tools))
      (should-not (member "ListSkills" tools)))))

(niwa-deftest niwa-agent-default/plan-inheritance
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "frozen default roles retain transiently hidden specialists for ordinary followups"
  (with-temp-buffer
    (let ((session (niwa-session--create
                    :plan-mode t
                    :tool-catalog '((("niwa" "Eval") . "eval")
                                    (("niwa" "SharedEdit") . "edit")
                                    (("niwa" "Imenu") . "outline")))))
      (setq-local niwa--session session
                  niwa--current-request
                  (niwa-request--create :plan-read-only t
                                           :ptc-primitives '("Eval" "SharedEdit")))
      (let ((frozen (niwa-agent-freeze (niwa-agent-default))))
        (dolist (plan '(t nil))
          (setf (niwa-session-plan-mode session) plan)
          (setq-local niwa--current-request nil)
          ;; Each follow-up creates a new invocation from the frozen role.
          (setq-local niwa--agent-invocation (niwa-agent-invocation-create frozen))
          (setf (niwa-agent-invocation-parent-session niwa--agent-invocation) session
                (niwa-agent-invocation-plan-read-only niwa--agent-invocation) plan)
          (should (equal '("Eval" "SharedEdit")
                         (mapcar (lambda (entry) (cadr (car entry)))
                                 (niwa-agent-invocation-tool-catalog niwa--agent-invocation))))
          (should (equal (unless plan '("Eval" "SharedEdit"))
                         (niwa-tool-ptc--roster))))))))

(niwa-deftest niwa-agent--specs-contain-tool-p/test
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "checks resolved active tools rather than raw spec spelling"
  (should (niwa-agent--specs-contain-tool-p
           '((:tool "Agent")) "Agent"))
  (should-not (niwa-agent--specs-contain-tool-p
               '((:tool "Read")) "Agent")))

(niwa-deftest niwa-agent-invocation-create
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "advertises extra discoverable capabilities to retained agents"
  (let* ((niwa-agent-extra-tool-specs
          '((explorer (:discoverable (:tool "Eval")))))
         (invocation
          (niwa-agent-invocation-create
           (niwa-agent-get "explorer"))))
    (should
     (cl-find "Eval" (niwa-agent-invocation-tool-catalog invocation)
              :key (lambda (entry) (cadr (car entry))) :test #'equal))))

(niwa-deftest niwa-agent-to-gptel-spec
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "installs discovery and ToolCall once for every built-in role"
  (dolist (name '("worker" "explorer" "verifier" "reviewer"))
    (let ((spec (niwa-agent-to-gptel-spec (niwa-agent-get name)))
          tools)
      (gptel--apply-preset
       (cdr spec)
       (lambda (symbol value)
         (when (eq symbol 'gptel-tools) (setq tools value))))
      (dolist (name '("ToolCall" "ToolSearch"))
        (should (= 1 (cl-count name tools :key #'gptel-tool-name
                               :test #'equal)))))))

(niwa-deftest niwa-agent--declared-specs/test
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "appends role-specific user extras before authority augmentation"
  (let* ((niwa-agent-extra-tool-specs
          '((explorer (:tool "Eval"))))
         (specs (niwa-agent--declared-specs
                 (niwa-agent-get "explorer"))))
    (should (member '(:discoverable (:tool "Agent")) specs))
    (should (member '(:tool "Eval") specs))
    (should-not (member '(:tool "SendMessage") specs))))

(niwa-deftest niwa-agent-default-native-tools
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "makes a permitted SendMessage native once, keeping inherited tools first"
  (let* ((read (niwa-tool-gptel-tool (niwa-tool-get "Read" "niwa")))
         (catalog '((("niwa" "SendMessage") . "s") (("niwa" "ListAgents") . "l")))
         (tools (niwa-agent-default-native-tools catalog (list read))))
    (should (equal '("Read" "SendMessage") (mapcar #'gptel-tool-name tools)))
    (should (equal tools (niwa-agent-default-native-tools catalog tools))))
  :doc "adds nothing the inherited catalog does not permit"
  (let ((read (niwa-tool-gptel-tool (niwa-tool-get "Read" "niwa"))))
    (should (equal (list read)
                   (niwa-agent-default-native-tools
                    '((("niwa" "Imenu") . "i")) (list read))))))

(niwa-deftest niwa-agent--effective-context-components
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "named and frozen roles keep their own selection"
  (let ((worker (niwa-agent-get "worker")))
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create :name "ctx"))
      (should (equal (niwa-agent-context-components worker)
                     (niwa-agent--effective-context-components worker)))
      (let ((frozen (copy-niwa-agent (niwa-agent-default))))
        (setf (niwa-agent-frozen-p frozen) t
              (niwa-agent-context-components frozen) '(environment))
        (should (equal '(environment)
                       (niwa-agent--effective-context-components frozen))))))

  :doc "a default child of the root inherits root observations except root-only ones"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "ctx"))
    (let ((components (niwa-agent--effective-context-components
                       (niwa-agent-default))))
      (dolist (name '(workspace-config memory environment tools skills resources))
        (should (memq name components)))
      (dolist (name niwa-agent--root-only-components)
        (should-not (memq name components)))))

  :doc "a default child of an agent inherits that agent's selection"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "ctx")
                niwa--agent-invocation
                (niwa-agent-invocation--create
                 :agent (niwa-agent--create
                         :name "delegator" :frozen-p t
                         :context-components '(workspace-config environment tools))))
    (should (equal '(workspace-config environment tools)
                   (niwa-agent--effective-context-components
                    (niwa-agent-default)))))

  :doc "freezing the default role records the inherited selection"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "ctx"))
    (should (memq 'workspace-config
                  (niwa-agent-context-components
                   (niwa-agent-freeze (niwa-agent-default)))))
    ;; The shared role definition itself is never changed.
    (should-not (niwa-agent-context-components (niwa-agent-default)))))

(niwa-deftest niwa-agent-resolve-role/test
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "omitted roles select default while named roles resolve visibly"
  (should (eq (niwa-agent-default)
              (niwa-agent-resolve-role nil)))
  (should (equal "worker"
                 (niwa-agent-name
                  (niwa-agent-resolve-role "worker"))))
  (dolist (role '("" "missing" worker))
    (should-error (niwa-agent-resolve-role role) :type 'user-error)))

(niwa-deftest niwa-agent-freeze/test
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "materializes dynamic instructions and effective tools once"
  (let* ((prompt "Initial instructions.")
         (niwa-agent-extra-tool-specs
          '((freeze_test (:tool "Read"))))
         (agent
          (niwa-agent--create
           :name "freeze_test"
           :description "Frozen role"
           :tools '((:tool "Agent"))
           :system-prompt (lambda () prompt)
           :reminders (list (niwa-reminders-make-pending-events))))
         (frozen (niwa-agent-freeze agent)))
    (setq prompt "Mutated instructions.")
    (setf (niwa-agent-tools agent) '((:tool "Eval")))
    (setq niwa-agent-extra-tool-specs
          '((freeze_test (:tool "Write"))))
    (should (niwa-agent-frozen-p frozen))
    (should (equal "Initial instructions."
                   (niwa-agent-system-prompt frozen)))
    (should (member '(:tool "Agent")
                    (niwa-agent--effective-specs frozen)))
    (should (member '(:tool "Read")
                    (niwa-agent--effective-specs frozen)))
    (should-not (member '(:tool "Write")
                        (niwa-agent--effective-specs frozen)))
    (should-not (member '(:tool "Eval")
                        (niwa-agent--effective-specs frozen)))
    (should
     (equal '((pending-events))
            (mapcar #'niwa-reminder-recipe
                    (niwa-agent-reminders frozen)))))

  :doc "rejects a closure-only reminder before a role can be published"
  (let ((agent
         (niwa-agent--create
          :name "ephemeral"
          :description "Non-durable role"
          :reminders
          (list
           (niwa-reminder-create
            :type 'runtime-only
            :trigger (lambda (_) t)
            :content (lambda (_) "runtime"))))))
    (should-error (niwa-agent-freeze agent))))

(niwa-deftest niwa-agent-role-tools/test
  (:before-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "worker and explorer orchestrate while reviewer and verifier are leaves"
  (let ((control '("Agent" "WaitAgent" "InterruptAgent"))
        (observation '("SendMessage" "ListAgents")))
    (dolist (name '("worker" "explorer" "reviewer" "verifier"))
      (let ((tools (test-niwa-agents--resolved-tool-names name)))
        (dolist (tool observation)
          (should (member tool tools)))
        (if (member name '("worker" "explorer"))
            (dolist (tool control)
              (should (member tool tools)))
          (dolist (tool control)
            (should-not (member tool tools)))))))

  :doc "worker has independent broad implementation capabilities"
  (let ((tools (test-niwa-agents--resolved-tool-names "worker")))
    (dolist (tool '("Read" "ApplyPatch" "Bash" "Eval"
                    "XrefDefinitions" "Skill" "TaskWrite"))
      (should (member tool tools))))

  :doc "every named role reports natively; worker and verifier also evaluate natively"
  (dolist (name '("worker" "explorer" "reviewer" "verifier"))
    (let* ((resolved (niwa-tool-resolve
                      (niwa-agent--effective-specs (niwa-agent-get name))))
           (active (mapcar #'niwa-tool-name (plist-get resolved :active)))
           (catalog (mapcar #'niwa-tool-name (plist-get resolved :discoverable))))
      (should (member "SendMessage" active))
      (should-not (member "SendMessage" catalog))
      (should (member "ListAgents" catalog))
      (if (member name '("worker" "verifier"))
          (should (member "Eval" active))
        (should-not (member "Eval" active)))
      (should-not (member "Eval" catalog))
      (if (member "Bash" active)
          (should (member "WriteStdin" active))
        (should-not (member "WriteStdin" active)))
      (should-not (member "WriteStdin" catalog))))

  :doc "shared editing stays discoverable to the worker; every role reads through Read"
  (dolist (name '("worker" "explorer" "reviewer" "verifier"))
    (let* ((resolved (niwa-tool-resolve
                      (niwa-agent--effective-specs (niwa-agent-get name))))
           (active (mapcar #'niwa-tool-name (plist-get resolved :active)))
           (catalog (mapcar #'niwa-tool-name (plist-get resolved :discoverable))))
      (should (member "Read" active))
      (dolist (tool '("SharedCreate" "SharedEdit"))
        (should-not (member tool active)))
      (dolist (tool '("SharedCreate" "SharedEdit"))
        (if (equal name "worker")
            (should (member tool catalog))
          (should-not (member tool catalog))))))

  :doc "explorer remains directly read-only despite delegation authority"
  (dolist (tool (plist-get
                 (niwa-tool-resolve
                  (niwa-agent--effective-specs
                   (niwa-agent-get "explorer")))
                 :active))
    (unless (member (niwa-tool-name tool)
                    '("Agent" "WaitAgent"
                      "InterruptAgent" "SendMessage" "ListAgents"))
      (should (niwa-tool-read-only-p tool))))

  :doc "custom roles with Agent receive the complete control bundle"
  (unwind-protect
      (progn
        (niwa-define-agent delegator-test
          :description "Capability bundle test."
          :tools ((:tool "Agent"))
          :system-components
          '((role :text "Delegate.")
            workspace-config
            environment))
        (let ((tools
               (test-niwa-agents--resolved-tool-names
                "delegator-test")))
          (dolist (tool '("Agent" "WaitAgent"
                          "InterruptAgent" "SendMessage" "ListAgents"))
            (should (member tool tools)))))
    (setq niwa-agent--registry
          (assoc-delete-all "delegator-test" niwa-agent--registry))))

(niwa-deftest niwa-define-agent/system-components/test
  (:before-each
   (test-niwa-agents--restore-builtins)
   :after-each
   (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "rejects removed and unknown definition keys"
  (should-error
   (macroexpand
    '(niwa-define-agent stale-profile-agent
       :system-prompt "Removed API")))

  :doc "built-in roles keep stable policy and deliver only their selected observations"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-role-context-" t)))
         (workspace (niwa-workspace--create :root root :id root :type 'project :name "roles"))
         (session (niwa-session-create "roles" workspace))
         (niwa-memory-dirs '(".niwa/memory/"))
         (niwa-skill-dirs nil)
         (niwa-skills-include-bundled nil))
    (unwind-protect
        (progn
          (write-region "Documented project command." nil (file-name-concat root "AGENTS.md") nil 'silent)
          (make-directory (file-name-concat root ".niwa/memory") t)
          (write-region "Private remembered fact." nil
                        (file-name-concat root ".niwa/memory/MEMORY.md") nil 'silent)
          (dolist (name '("worker" "explorer" "verifier" "reviewer"))
            (with-temp-buffer
              (setq-local niwa--session session)
              (let* ((agent (niwa-agent-freeze (niwa-agent-get name)))
                     (prompt (niwa-agent-system-prompt agent))
                     (fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
                (setq-local niwa--agent-invocation
                            (niwa-agent-invocation--create :agent agent))
                (should (string-search "## Task boundaries" prompt))
                (should-not (string-search "Documented project command" prompt))
                (should-not (string-search "## Environment" prompt))
                (niwa-context-delivery-stage fsm)
                (let ((context (prin1-to-string
                                (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries))))
                  (should (string-search "Documented project command" context))
                  (should (string-search "## Environment" context))
                  (should (string-search "work://shared/" context))
                  (should-not (string-search "context-journal" context))
                  (if (equal name "worker")
                      (should (string-search "Private remembered fact" context))
                    (should-not (string-search "Private remembered fact" context))))))))
      (delete-directory root t)))

  :doc "custom agents retain inline context and select named live context"
  (unwind-protect
      (progn
        (niwa-define-agent custom-profile-agent
          :description "custom" :tools nil
          :system-components '((role :text "Custom role") workspace-config environment
                               (memory :text "Custom inline memory")))
        (let* ((agent (niwa-agent-get "custom-profile-agent"))
               (prompt (funcall (niwa-agent-system-prompt agent))))
          (should (string-search "Custom role" prompt))
          (should (string-search "Custom inline memory" prompt))
          (should (memq 'workspace-config (niwa-agent-context-components agent)))
          (should (memq 'environment (niwa-agent-context-components agent)))
          (should-not (memq 'memory (niwa-agent-context-components agent)))))
    (setq niwa-agent--registry
          (assoc-delete-all "custom-profile-agent" niwa-agent--registry))))

(niwa-deftest niwa-define-agent/command-hook-source/test
  (:before-each (test-niwa-agents--restore-builtins)
   :after-each (test-niwa-agents--restore-builtins))
  ,test
  (test)
  :doc "runs user-loaded agent command hooks from their stable local origin"
  (let* ((name "command-hook-source-agent")
         (root (file-name-as-directory
                (make-temp-file "niwa-agent-command-hook-" t)))
         (remote-root (format "/niwamock:agent-hook:%s" root)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("agent-hook")
          (niwa-define-agent command-hook-source-agent
            :description "command hook source test"
            :hooks ((PreToolUse
                     ((:matcher "Bash"
                       :hooks ((:type command :command "true")))))))
          (let* ((rules
                  (niwa-agent-hook-rules
                   (niwa-agent-get name)))
                 (group (cadr (assq 'PreToolUse rules)))
                 (handler (car (plist-get group :hooks)))
                 (target (niwa-execution-target-create remote-root))
                 (session
                  (niwa-session--create
                   :name "main" :execution-target target))
                 (directory
                  (niwa-hooks--command-default-directory
                   handler
                   (list :cwd remote-root :workspace-root remote-root)
                   session)))
            (should (eq 'user (plist-get handler :source)))
            (should (equal user-emacs-directory
                           (plist-get handler :source-root)))
            (should (equal (file-name-as-directory user-emacs-directory)
                           directory))
            (should-not (file-remote-p directory))))
      (setq niwa-agent--registry
            (assoc-delete-all name niwa-agent--registry))
      (delete-directory root t))))


(niwa-deftest niwa-agents-install-agent-schema ()
  ,test
  (test)
  :doc "replaces the direct Agent with one roster copy, keeping other tools"
  (progn
    (niwa-tool-ui--register)
    (let* ((registered (niwa-tool-gptel-tool (niwa-tool-get "Agent")))
           (list-agents (niwa-tool-gptel-tool (niwa-tool-get "ListAgents")))
           (roles (lambda (tool)
                    (plist-get (cl-find "role" (gptel-tool-args tool)
                                        :key (lambda (arg) (plist-get arg :name))
                                        :test #'equal)
                               :enum)))
           (agents (lambda ()
                     (cl-remove-if-not (lambda (tool)
                                         (equal "Agent" (gptel-tool-name tool)))
                                       gptel-tools))))
      (with-temp-buffer
        (setq-local gptel-tools (list list-agents registered))
        (niwa-agents-set-specs '(("alpha")))
        (niwa-agents-install-agent-schema)
        (let ((first (car (funcall agents))))
          (should (= 1 (length (funcall agents))))
          (should (eq list-agents (car gptel-tools)))
          (should (eq first (cadr gptel-tools)))
          (should-not (eq registered first))
          (should (eq (gptel-tool-function registered) (gptel-tool-function first)))
          (should (equal ["alpha"] (funcall roles first)))
          ;; A later roster replaces the earlier copy instead of adding one.
          (niwa-agents-set-specs '(("beta") ("gamma")))
          (niwa-agents-install-agent-schema)
          (should (= 2 (length gptel-tools)))
          (should (= 1 (length (funcall agents))))
          (should-not (eq first (car (funcall agents))))
          (should (equal ["beta" "gamma"] (funcall roles (car (funcall agents)))))))
      ;; The registered tool every buffer shares never changes.
      (should (equal [] (funcall roles registered)))))

  :doc "describes this buffer's tiers on the copied model argument"
  (progn
    (niwa-tool-ui--register)
    (test-niwa-agents--with-tiers
      (with-temp-buffer
        (setq-local gptel-tools
                    (list (niwa-tool-gptel-tool (niwa-tool-get "Agent"))))
        (niwa-agents-set-specs '(("alpha")))
        (niwa-agents-install-agent-schema)
        (let ((model (cl-find "model" (gptel-tool-args (car gptel-tools))
                              :key (lambda (arg) (plist-get arg :name))
                              :test #'equal)))
          (should (equal ["fast" "strong"] (plist-get model :enum)))
          (should (string-search
                   "strong = Team:big-model (high), for hard design"
                   (plist-get model :description)))))))

  :doc "leaves a foreign same-named tool and a discoverable Agent alone"
  (progn
    (niwa-tool-ui--register)
    (let ((foreign (gptel--copy-tool
                    (niwa-tool-gptel-tool (niwa-tool-get "Agent")))))
      (setf (gptel-tool-function foreign) #'ignore)
      (with-temp-buffer
        (niwa-agents-set-specs '(("alpha")))
        (let ((tools (list foreign)))
          (setq-local gptel-tools tools)
          (niwa-agents-install-agent-schema)
          (should (eq tools gptel-tools))
          (should (eq foreign (car gptel-tools))))
        (setq-local gptel-tools nil)
        (niwa-agents-install-agent-schema)
        (should-not gptel-tools)))))

(niwa-deftest niwa-agents-agent-contract ()
  ,test
  (test)
  :doc "resolves the roster and tier guide for the current buffer"
  (progn
    (niwa-tool-ui--register)
    (test-niwa-agents--with-tiers
      (with-temp-buffer
        (niwa-agents-set-specs '(("alpha") ("beta")))
        (let* ((tool (niwa-tool-get "Agent"))
               (resolved (niwa-tool-contextual-contract tool))
               (extras (lambda (name)
                         (nthcdr 4 (assq name (car resolved))))))
          (should (equal ["alpha" "beta"]
                         (plist-get (funcall extras 'role) :enum)))
          (should (equal ["fast" "strong"]
                         (plist-get (funcall extras 'model) :enum)))
          (should (string-search
                   "fast = Team:small-model (default effort), for search"
                   (nth 3 (assq 'model (car resolved)))))
          (should (string-prefix-p "Start a retained" (cdr resolved)))
          (should (string-search "Model tiers: fast=small-model, strong=big-model"
                                 (cdr resolved)))
          ;; The registered specification stays buffer independent.
          (should (equal [] (plist-get (nthcdr 4 (assq 'role (niwa-tool-args tool)))
                                       :enum)))
          (should-not (string-search "Model tiers" (niwa-tool-summary tool)))
          ;; The specialist listing line carries the same tier models.
          (should (string-search
                   "Model tiers: fast=small-model, strong=big-model"
                   (niwa-tool-ptc--tool-declaration "Agent")))))))

  :doc "a buffer keeps the tier mapping it was configured with"
  (progn
    (niwa-tool-ui--register)
    (test-niwa-agents--with-tiers
      (with-temp-buffer
        (setq-local niwa-model-tiers
                    (copy-tree '((fast :provider "Team:big-model"))))
        (let ((resolved (niwa-tool-contextual-contract
                         (niwa-tool-get "Agent"))))
          (should (string-search "fast = Team:big-model"
                                 (nth 3 (assq 'model (car resolved)))))
          (should-not (string-search "small-model" (cdr resolved))))))))

(provide 'test-niwa-agents)

;;; test-niwa-agents.el ends here
