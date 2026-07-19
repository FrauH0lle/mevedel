;;; test-mevedel-agents.el --- Tests for mevedel-agents.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for built-in agent definitions and agent registry helpers.

;;; Code:

(require 'mevedel-agents)
(require 'mevedel-skills-core)
(require 'mevedel-tools)
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
  :doc "explorer receives skill tools while review/verify/coordinator do not"
  (let ((explorer (test-mevedel-agents--resolved-tool-names "explorer")))
    (should (member "Skill" explorer))
    (should (member "ListSkills" explorer)))
  (dolist (name '("coordinator" "verifier" "reviewer"))
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
    (dolist (tool '("Read" "Write" "Edit" "Bash" "Eval"
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
          :system-prompt "Delegate.")
        (let ((tools
               (test-mevedel-agents--resolved-tool-names
                "delegator-test")))
          (dolist (tool '("Agent" "FollowupAgent" "WaitAgent"
                          "InterruptAgent" "SendMessage" "ListAgents"))
            (should (member tool tools)))))
    (setq mevedel-agent--registry
          (assoc-delete-all "delegator-test" mevedel-agent--registry))))

(mevedel-deftest mevedel-agent-skill-tool-capable-p/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "skill roster follows resolved Skill/ListSkills availability"
  (should (mevedel-agent-skill-tool-capable-p
           (mevedel-agent-get "explorer")))
  (dolist (name '("coordinator" "verifier" "reviewer"))
    (should-not (mevedel-agent-skill-tool-capable-p
                 (mevedel-agent-get name))))

  :doc "does not mutate a partial registry while resolving agent tools"
  (unwind-protect
      (progn
        (mevedel-tool-clear-registry)
        (mevedel-tool-register
         (mevedel-tool--create
          :name "Read" :category "mevedel" :groups '(read)))
        (should-not (mevedel-agent-skill-tool-capable-p
                     (mevedel-agent-get "explorer")))
        (should-not (mevedel-tool-get "Skill")))
    (mevedel-tool-clear-registry)
    (test-mevedel-agents--restore-builtins)))

(mevedel-deftest mevedel-define-agent/explicit-skill-prompt/test
  (:before-each
   (test-mevedel-agents--restore-builtins)
   (setq mevedel-agent--registry nil)
   :after-each
   (setq mevedel-agent--registry nil)
   (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "explicit system-prompt agents with Skill tools receive skills section"
  (mevedel-define-agent explicit-skill-agent
    :description "explicit"
    :tools ((:tool "Skill"))
    :system-prompt (lambda () "Agent explicit base")
    :include-workspace-config nil
    :include-memory nil
    :include-environment nil)
  (let* ((ws (mevedel-workspace--create
              :type 'test :id "agent-skills" :root "/tmp/agent-skills"
              :name "agent-skills"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (agent (mevedel-agent-get "explicit-skill-agent"))
         (prompt nil))
    (setf (mevedel-session-skills session)
          (list (mevedel-skill--create
                 :name "agent-helper"
                 :description "helps explicit prompt agents"
                 :model-invocable-p t
                 :active-p t)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq prompt (funcall (mevedel-agent-system-prompt agent))))
    (should (string-match-p "Agent explicit base" prompt))
    (should (string-match-p "## Skills" prompt))
    (should (string-match-p "agent-helper" prompt)))

  :doc "explicit system-prompt skill roster follows extra resolved tools"
  (let ((mevedel-agent-extra-tool-specs
         '((explicit-extra-agent (:tool "Skill")))))
    (mevedel-define-agent explicit-extra-agent
      :description "explicit extra"
      :tools nil
      :system-prompt (lambda () "Agent extra base")
      :include-workspace-config nil
      :include-memory nil
      :include-environment nil)
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "agent-extra-skills"
                :root "/tmp/agent-extra-skills"
                :name "agent-extra-skills"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (agent (mevedel-agent-get "explicit-extra-agent"))
           prompt)
      (setf (mevedel-session-skills session)
            (list (mevedel-skill--create
                   :name "extra-helper"
                   :description "helps extra prompt agents"
                   :model-invocable-p t
                   :active-p t)))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (setq prompt (funcall (mevedel-agent-system-prompt agent))))
      (should (string-match-p "Agent extra base" prompt))
      (should (string-match-p "## Skills" prompt))
      (should (string-match-p "extra-helper" prompt)))))


(provide 'test-mevedel-agents)

;;; test-mevedel-agents.el ends here
