;;; test-mevedel-agents.el --- Tests for mevedel-agents.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for built-in agent definitions and agent registry helpers.

;;; Code:

(require 'mevedel-agents)
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

(defun test-mevedel-agents--tool-names (agent-name)
  "Return explicit tool names from AGENT-NAME's effective specs."
  (delq nil
        (mapcar (lambda (spec)
                  (and (consp spec)
                       (eq :tool (car spec))
                       (cadr spec)))
                (mevedel-agent--effective-specs
                 (mevedel-agent-get agent-name)))))

(defun test-mevedel-agents--restore-builtins ()
  "Restore bundled agent definitions after tests that clear the registry."
  (unless (mevedel-agent-get "explorer")
    (load-file (locate-library "mevedel-agents")))
  (mevedel-tools-register))

(mevedel-deftest mevedel-agent--effective-specs/test
  (:before-each (test-mevedel-agents--restore-builtins))
  ,test
  (test)
  :doc "explorer receives skill tools while review/verify/coordinator do not"
  (let ((explorer (test-mevedel-agents--tool-names "explorer")))
    (should (member "Skill" explorer))
    (should (member "ListSkills" explorer)))
  (dolist (name '("coordinator" "verifier" "reviewer"))
    (let ((tools (test-mevedel-agents--tool-names name)))
      (should-not (member "Skill" tools))
      (should-not (member "ListSkills" tools)))))

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
