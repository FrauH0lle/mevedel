;;; test-mevedel-agents.el --- Tests for mevedel-agents.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for built-in agent definitions and agent registry helpers.

;;; Code:

(require 'mevedel-agents)
(require 'mevedel-context-delivery)
(require 'mevedel-agent-control)
(require 'mevedel-execution-target)
(require 'mevedel-hooks)
(require 'mevedel-reminders)
(require 'mevedel-skills-core)
(require 'mevedel-system)
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Agent definitions

(defun test-mevedel-agents--resolved-tool-names (agent-name)
  "Return every available tool name for AGENT-NAME, across both routes."
  (let ((resolved (mevedel-tool-resolve
                   (mevedel-agent--effective-specs (mevedel-agent-get agent-name)))))
    (mapcar #'mevedel-tool-name
            (append (plist-get resolved :active) (plist-get resolved :discoverable)))))

(defun test-mevedel-agents--restore-builtins ()
  "Restore bundled agent definitions after tests that clear the registry."
  (unless (mevedel-agent-get "explorer")
    (load-file (locate-library "mevedel-agents")))
  (mevedel-tools-register))

(mevedel-deftest mevedel-plan-directive-p
  ()
  ,test
  (test)
  :doc "recognizes session and request directive planning authority"
  (let ((session (mevedel-session--create
                  :name "directive"
                  :directive-planning '(:directive-id "d1" :phase approval)))
        (request (mevedel-request--create :directive-uuid "d1")))
    (should (mevedel-plan-directive-p session))
    (should (mevedel-plan-directive-p nil request)))
  :doc "recognizes ambient directive planning and rejects ordinary requests"
  (let ((mevedel--current-request
         (mevedel-request--create :directive-uuid "d1")))
    (should (mevedel-plan-directive-p)))
  (let ((session (mevedel-session--create :name "ordinary"))
        (request (mevedel-request--create :plan-read-only t)))
    (should-not (mevedel-plan-directive-p session request))))

(mevedel-deftest mevedel-plan-read-only-p ()
  ,test
  (test)
  :doc "uses an explicit root session"
  (let ((session (mevedel-session--create :name "main" :plan-mode t)))
    (should (mevedel-plan-read-only-p session)))

  :doc "keeps a directive planning request read-only after its phase advances"
  (let ((session (mevedel-session--create :name "main"))
        (mevedel--current-request
         (mevedel-request--create :plan-read-only t)))
    (should (mevedel-plan-read-only-p session))
    (setf (mevedel-session-directive-planning session)
          '(:directive-id "d1" :phase implementation))
    (should (mevedel-plan-read-only-p session)))

  :doc "uses a retained agent's parent session"
  (let ((session (mevedel-session--create :name "main" :plan-mode t)))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create
                   :parent-session session))
      (should (mevedel-plan-read-only-p))))

  :doc "uses immutable Plan authority on a retained agent"
  (let ((session (mevedel-session--create :name "main")))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create
                   :parent-session session :plan-read-only t))
      (should (mevedel-plan-read-only-p))))

  :doc "is nil without a stamp or a sticky Plan session"
  (let ((session (mevedel-session--create :name "main"))
        (mevedel--current-request (mevedel-request--create)))
    (should-not (mevedel-plan-read-only-p session))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create :parent-session session))
      (should-not (mevedel-plan-read-only-p)))))

(mevedel-deftest mevedel-agent-invocation-require-path
  ()
  ,test
  (test)
  :doc "returns canonical invocation paths and rejects missing or malformed paths"
  (let ((invocation
         (mevedel-agent-invocation--create
          :agent-id "default--opaque"
          :path "/root/worker")))
    (should (equal "/root/worker"
                   (mevedel-agent-invocation-require-path invocation)))
    (setf (mevedel-agent-invocation-path invocation) nil)
    (should-error (mevedel-agent-invocation-require-path invocation))
    (setf (mevedel-agent-invocation-path invocation) "/root/Upper")
    (should-error (mevedel-agent-invocation-require-path invocation))))

(mevedel-deftest mevedel-agent-request-locals-p
  ()
  ,test
  (test)
  :doc "accepts unique closed-schema subsets and complete configurations"
  (let ((complete
         (mapcar (lambda (symbol) (cons symbol nil))
                 mevedel-agent-request-local-symbols)))
    (should (mevedel-agent-request-locals-p
             (list (car complete))))
    (should (mevedel-agent-request-locals-p complete t)))
  :doc "rejects unknown, duplicate, and incomplete configuration keys"
  (let ((complete
         (mapcar (lambda (symbol) (cons symbol nil))
                 mevedel-agent-request-local-symbols)))
    (should-not
     (mevedel-agent-request-locals-p '((kill-buffer-hook ignore))))
    (should-not
     (mevedel-agent-request-locals-p
      (cons (car complete) complete)))
    (should-not
     (mevedel-agent-request-locals-p (cdr complete) t))))

(mevedel-deftest mevedel-agent--effective-specs/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "delegating roles receive skill tools while review and verify do not"
  (dolist (name '("worker" "explorer"))
    (let ((tools (test-mevedel-agents--resolved-tool-names name)))
      (should (member "Skill" tools))
      (should (member "ListSkills" tools))))
  (dolist (name '("verifier" "reviewer"))
    (let ((tools (test-mevedel-agents--resolved-tool-names name)))
      (should-not (member "Skill" tools))
      (should-not (member "ListSkills" tools)))))

(mevedel-deftest mevedel-agent--specs-contain-tool-p/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "checks resolved active tools rather than raw spec spelling"
  (should (mevedel-agent--specs-contain-tool-p
           '((:tool "Agent")) "Agent"))
  (should-not (mevedel-agent--specs-contain-tool-p
               '((:tool "Read")) "Agent")))

(mevedel-deftest mevedel-agent-invocation-create
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "advertises extra discoverable capabilities to retained agents"
  (let* ((mevedel-agent-extra-tool-specs
          '((explorer (:discoverable (:tool "Eval")))))
         (invocation
          (mevedel-agent-invocation-create
           (mevedel-agent-get "explorer"))))
    (should
     (cl-find "Eval" (mevedel-agent-invocation-tool-catalog invocation)
              :key (lambda (entry) (cadr (car entry))) :test #'equal))))

(mevedel-deftest mevedel-agent-to-gptel-spec
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "installs discovery and ToolCall once for every built-in role"
  (dolist (name '("worker" "explorer" "verifier" "reviewer"))
    (let ((spec (mevedel-agent-to-gptel-spec (mevedel-agent-get name)))
          tools)
      (gptel--apply-preset
       (cdr spec)
       (lambda (symbol value)
         (when (eq symbol 'gptel-tools) (setq tools value))))
      (dolist (name '("ToolCall" "ToolSearch"))
        (should (= 1 (cl-count name tools :key #'gptel-tool-name
                               :test #'equal)))))))

(mevedel-deftest mevedel-agent--declared-specs/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "appends role-specific user extras before authority augmentation"
  (let* ((mevedel-agent-extra-tool-specs
          '((explorer (:tool "Eval"))))
         (specs (mevedel-agent--declared-specs
                 (mevedel-agent-get "explorer"))))
    (should (member '(:discoverable (:tool "Agent")) specs))
    (should (member '(:tool "Eval") specs))
    (should-not (member '(:tool "SendMessage") specs))))

(mevedel-deftest mevedel-agent-resolve-role/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "omitted roles select default while named roles resolve visibly"
  (should (eq (mevedel-agent-default)
              (mevedel-agent-resolve-role nil)))
  (should (equal "worker"
                 (mevedel-agent-name
                  (mevedel-agent-resolve-role "worker"))))
  (dolist (role '("" "missing" worker))
    (should-error (mevedel-agent-resolve-role role) :type 'user-error)))

(mevedel-deftest mevedel-agent-freeze/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "materializes dynamic instructions and effective tools once"
  (let* ((prompt "Initial instructions.")
         (mevedel-agent-extra-tool-specs
          '((freeze_test (:tool "Read"))))
         (agent
          (mevedel-agent--create
           :name "freeze_test"
           :description "Frozen role"
           :tools '((:tool "Agent"))
           :system-prompt (lambda () prompt)
           :reminders (list (mevedel-reminders-make-pending-events))))
         (frozen (mevedel-agent-freeze agent)))
    (setq prompt "Mutated instructions.")
    (setf (mevedel-agent-tools agent) '((:tool "Eval")))
    (setq mevedel-agent-extra-tool-specs
          '((freeze_test (:tool "Write"))))
    (should (mevedel-agent-frozen-p frozen))
    (should (equal "Initial instructions."
                   (mevedel-agent-system-prompt frozen)))
    (should (member '(:tool "Agent")
                    (mevedel-agent--effective-specs frozen)))
    (should (member '(:tool "Read")
                    (mevedel-agent--effective-specs frozen)))
    (should-not (member '(:tool "Write")
                        (mevedel-agent--effective-specs frozen)))
    (should-not (member '(:tool "Eval")
                        (mevedel-agent--effective-specs frozen)))
    (should
     (equal '((pending-events))
            (mapcar #'mevedel-reminder-recipe
                    (mevedel-agent-reminders frozen)))))

  :doc "rejects a closure-only reminder before a role can be published"
  (let ((agent
         (mevedel-agent--create
          :name "ephemeral"
          :description "Non-durable role"
          :reminders
          (list
           (mevedel-reminder-create
            :type 'runtime-only
            :trigger (lambda (_) t)
            :content (lambda (_) "runtime"))))))
    (should-error (mevedel-agent-freeze agent))))

(mevedel-deftest mevedel-agent-role-tools/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "worker and explorer orchestrate while reviewer and verifier are leaves"
  (let ((control '("Agent" "FollowupAgent" "WaitAgent" "InterruptAgent"))
        (observation '("SendMessage" "ListAgents")))
    (dolist (name '("worker" "explorer" "reviewer" "verifier"))
      (let ((tools (test-mevedel-agents--resolved-tool-names name)))
        (dolist (tool observation)
          (should (member tool tools)))
        (if (member name '("worker" "explorer"))
            (dolist (tool control)
              (should (member tool tools)))
          (dolist (tool control)
            (should-not (member tool tools)))))))

  :doc "worker has independent broad implementation capabilities"
  (let ((tools (test-mevedel-agents--resolved-tool-names "worker")))
    (dolist (tool '("Read" "ApplyPatch" "Bash" "Eval"
                    "XrefDefinitions" "Skill" "TaskCreate"))
      (should (member tool tools))))

  :doc "shared editing stays discoverable to the worker; every role reads through Read"
  (dolist (name '("worker" "explorer" "reviewer" "verifier"))
    (let* ((resolved (mevedel-tool-resolve
                      (mevedel-agent--effective-specs (mevedel-agent-get name))))
           (active (mapcar #'mevedel-tool-name (plist-get resolved :active)))
           (catalog (mapcar #'mevedel-tool-name (plist-get resolved :discoverable))))
      (should (member "Read" active))
      (dolist (tool '("SharedCreate" "SharedEdit"))
        (should-not (member tool active)))
      (dolist (tool '("SharedCreate" "SharedEdit"))
        (if (equal name "worker")
            (should (member tool catalog))
          (should-not (member tool catalog))))))

  :doc "explorer remains directly read-only despite delegation authority"
  (dolist (tool (plist-get
                 (mevedel-tool-resolve
                  (mevedel-agent--effective-specs
                   (mevedel-agent-get "explorer")))
                 :active))
    (unless (member (mevedel-tool-name tool)
                    '("Agent" "FollowupAgent" "WaitAgent"
                      "InterruptAgent" "SendMessage" "ListAgents"))
      (should (mevedel-tool-read-only-p tool))))

  :doc "custom roles with Agent receive the complete control bundle"
  (unwind-protect
      (progn
        (mevedel-define-agent delegator-test
          :description "Capability bundle test."
          :tools ((:tool "Agent"))
          :system-components
          '((role :text "Delegate.")
            workspace-config
            environment))
        (let ((tools
               (test-mevedel-agents--resolved-tool-names
                "delegator-test")))
          (dolist (tool '("Agent" "FollowupAgent" "WaitAgent"
                          "InterruptAgent" "SendMessage" "ListAgents"))
            (should (member tool tools)))))
    (setq mevedel-agent--registry
          (assoc-delete-all "delegator-test" mevedel-agent--registry))))

(mevedel-deftest mevedel-define-agent/system-components/test
  (:before-each
   (test-mevedel-agents--restore-builtins)
   :after-each
   (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "rejects removed and unknown definition keys"
  (should-error
   (macroexpand
    '(mevedel-define-agent stale-profile-agent
       :system-prompt "Removed API")))

  :doc "built-in roles keep stable policy and deliver only their selected observations"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-role-context-" t)))
         (workspace (mevedel-workspace--create :root root :id root :type 'project :name "roles"))
         (session (mevedel-session-create "roles" workspace))
         (mevedel-memory-dirs '(".mevedel/memory/"))
         (mevedel-skill-dirs nil)
         (mevedel-skills-include-bundled nil))
    (unwind-protect
        (progn
          (write-region "Documented project command." nil (file-name-concat root "AGENTS.md") nil 'silent)
          (make-directory (file-name-concat root ".mevedel/memory") t)
          (write-region "Private remembered fact." nil
                        (file-name-concat root ".mevedel/memory/MEMORY.md") nil 'silent)
          (dolist (name '("worker" "explorer" "verifier" "reviewer"))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (let* ((agent (mevedel-agent-freeze (mevedel-agent-get name)))
                     (prompt (mevedel-agent-system-prompt agent))
                     (fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
                (setq-local mevedel--agent-invocation
                            (mevedel-agent-invocation--create :agent agent))
                (should (string-search "## Task boundaries" prompt))
                (should-not (string-search "Documented project command" prompt))
                (should-not (string-search "## Environment" prompt))
                (mevedel-context-delivery-stage fsm)
                (let ((context (prin1-to-string
                                (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
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
        (mevedel-define-agent custom-profile-agent
          :description "custom" :tools nil
          :system-components '((role :text "Custom role") workspace-config environment
                               (memory :text "Custom inline memory")))
        (let* ((agent (mevedel-agent-get "custom-profile-agent"))
               (prompt (funcall (mevedel-agent-system-prompt agent))))
          (should (string-search "Custom role" prompt))
          (should (string-search "Custom inline memory" prompt))
          (should (memq 'workspace-config (mevedel-agent-context-components agent)))
          (should (memq 'environment (mevedel-agent-context-components agent)))
          (should-not (memq 'memory (mevedel-agent-context-components agent)))))
    (setq mevedel-agent--registry
          (assoc-delete-all "custom-profile-agent" mevedel-agent--registry))))

(mevedel-deftest mevedel-define-agent/command-hook-source/test
  (:before-each (test-mevedel-agents--restore-builtins)
   :after-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "runs user-loaded agent command hooks from their stable local origin"
  (let* ((name "command-hook-source-agent")
         (root (file-name-as-directory
                (make-temp-file "mevedel-agent-command-hook-" t)))
         (remote-root (format "/mevedelmock:agent-hook:%s" root)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("agent-hook")
          (mevedel-define-agent command-hook-source-agent
            :description "command hook source test"
            :hooks ((PreToolUse
                     ((:matcher "Bash"
                       :hooks ((:type command :command "true")))))))
          (let* ((rules
                  (mevedel-agent-hook-rules
                   (mevedel-agent-get name)))
                 (group (cadr (assq 'PreToolUse rules)))
                 (handler (car (plist-get group :hooks)))
                 (target (mevedel-execution-target-create remote-root))
                 (session
                  (mevedel-session--create
                   :name "main" :execution-target target))
                 (directory
                  (mevedel-hooks--command-default-directory
                   handler
                   (list :cwd remote-root :workspace-root remote-root)
                   session)))
            (should (eq 'user (plist-get handler :source)))
            (should (equal user-emacs-directory
                           (plist-get handler :source-root)))
            (should (equal (file-name-as-directory user-emacs-directory)
                           directory))
            (should-not (file-remote-p directory))))
      (setq mevedel-agent--registry
            (assoc-delete-all name mevedel-agent--registry))
      (delete-directory root t))))


(provide 'test-mevedel-agents)

;;; test-mevedel-agents.el ends here
