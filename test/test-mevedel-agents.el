;;; test-mevedel-agents.el --- Tests for mevedel-agents.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for built-in agent definitions and agent registry helpers.

;;; Code:

(require 'mevedel-agents)
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
  "Return resolved active tool names for AGENT-NAME."
  (mapcar #'mevedel-tool-name
          (plist-get
           (mevedel-tool-resolve
            (mevedel-agent--effective-specs
             (mevedel-agent-get agent-name)))
           :active)))

(defun test-mevedel-agents--restore-builtins ()
  "Restore bundled agent definitions after tests that clear the registry."
  (unless (mevedel-agent-get "explorer")
    (load-file (locate-library "mevedel-agents")))
  (mevedel-tools-register))

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

(mevedel-deftest mevedel-agent--declared-specs/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "appends role-specific user extras before authority augmentation"
  (let* ((mevedel-agent-extra-tool-specs
          '((explorer (:tool "Eval"))))
         (specs (mevedel-agent--declared-specs
                 (mevedel-agent-get "explorer"))))
    (should (member '(:tool "Agent") specs))
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
           :reminders (list (mevedel-reminders-make-verifier-read-only))))
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
     (equal '((verifier-read-only))
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

  :doc "built-in roles freeze their explicit context and tone matrix"
  (let* ((root-dir (file-name-as-directory
                    (make-temp-file "mevedel-agent-profile-" t)))
         (agents-md (file-name-concat root-dir "AGENTS.md"))
         (memory-dir (file-name-concat root-dir ".mevedel" "memory"))
         (memory-file (file-name-concat memory-dir "MEMORY.md"))
         (mevedel-memory-dirs '(".mevedel/memory/"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "agent-profiles"))
         (session (mevedel-session-create "main" ws))
         prompts)
    (unwind-protect
        (progn
          (make-directory memory-dir t)
          (write-region "Documented project command." nil agents-md)
          (write-region "Private remembered fact." nil memory-file)
          (setf (mevedel-session-skills session)
                (list (mevedel-skill--create
                       :name "agent-helper"
                       :description "helps profile agents"
                       :model-invocable-p t
                       :active-p t)))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (dolist (name '("worker" "explorer" "verifier" "reviewer"))
              (setf (alist-get name prompts nil nil #'equal)
                    (mevedel-agent-system-prompt
                     (mevedel-agent-freeze (mevedel-agent-get name))))))
          (dolist (prompt (mapcar #'cdr prompts))
            (should (string-match-p "Documented project command" prompt))
            (should (string-match-p "## Environment" prompt))
            (should (string-match-p "Tool orchestration" prompt))
            (dolist (scheme '("local://" "artifact://" "skill://" "agent://"
                              "history://" "memory://" "mcp://"))
              (should (string-match-p (regexp-quote scheme) prompt)))
            (should (string-match-p "shared durable space" prompt))
            (should (string-match-p "parent and retained agents" prompt))
            (should (string-match-p "SendMessage" prompt))
            (should (string-match-p
                     "not an attachment, invocation, or delegation"
                     prompt)))
          (dolist (name '("worker" "explorer" "verifier"))
            (should (string-match-p
                     "Reporting style"
                     (alist-get name prompts nil nil #'equal))))
          (should-not
           (string-match-p
            "Reporting style"
            (alist-get "reviewer" prompts nil nil #'equal)))
          (should
           (string-match-p
            "Private remembered fact"
            (alist-get "worker" prompts nil nil #'equal)))
          (dolist (name '("explorer" "verifier" "reviewer"))
            (should-not
             (string-match-p
              "Private remembered fact"
              (alist-get name prompts nil nil #'equal))))
          (dolist (name '("worker" "explorer"))
            (should
             (string-match-p
              "agent-helper"
              (alist-get name prompts nil nil #'equal))))
          (dolist (name '("verifier" "reviewer"))
            (should-not
             (string-match-p
              "agent-helper"
              (alist-get name prompts nil nil #'equal)))))
      (mevedel-workspace-clear-registry)
      (delete-directory root-dir t)))

  :doc "custom agents declare the same ordered components directly"
  (let* ((root-dir (file-name-as-directory
                    (make-temp-file "mevedel-custom-profile-" t)))
         (agents-md (file-name-concat root-dir "AGENTS.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "custom-profile"))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (progn
          (write-region "Custom workspace context." nil agents-md)
          (mevedel-define-agent custom-profile-agent
            :description "custom"
            :tools nil
            :system-components
            '((role :text "Custom role")
              workspace-config
              environment))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (let ((prompt
                   (funcall
                    (mevedel-agent-system-prompt
                     (mevedel-agent-get "custom-profile-agent")))))
              (should (string-match-p "Custom role" prompt))
              (should (string-match-p "Custom workspace context" prompt))
              (should (string-match-p "## Environment" prompt)))))
      (setq mevedel-agent--registry
            (assoc-delete-all "custom-profile-agent"
                              mevedel-agent--registry))
      (mevedel-workspace-clear-registry)
      (delete-directory root-dir t))))


(provide 'test-mevedel-agents)

;;; test-mevedel-agents.el ends here
