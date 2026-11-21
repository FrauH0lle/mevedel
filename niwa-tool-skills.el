;;; niwa-tool-skills.el --- Skill tool schemas -*- lexical-binding: t -*-

;;; Commentary:

;; Registers the model-facing Skill and ListSkills tool schemas.  Invocation,
;; lookup, and rendering behavior remains owned by `niwa-skills-invoke'.

;;; Code:

(require 'niwa-tool-registry)

(autoload 'niwa-skills--invoke-handler "niwa-skills-invoke")
(autoload 'niwa-skills--list-handler "niwa-skills-invoke")
(autoload 'niwa-skills--render-skill-tool "niwa-skills-invoke")

;; `niwa-skills-invoke'
(declare-function niwa-skills--invoke-handler
                  "niwa-skills-invoke" (callback args))
(declare-function niwa-skills--list-handler
                  "niwa-skills-invoke" (callback args))
(declare-function niwa-skills--render-skill-tool
                  "niwa-skills-invoke" (name args result render-data))


;;
;;; Registration

;;;###autoload
(defun niwa-tool-skills--register ()
  "Register the Skill and ListSkills tools."
  (niwa-define-tool
    :name "Skill"
    :description "Invoke a reusable prompt recipe (skill) by name."
    :prompt-file "prompts/tools/skill.md"
    :handler #'niwa-skills--invoke-handler
    :args ((name string :required
                 "The skill name (as shown in the skills listing).")
           (arguments string :optional
                      "Optional argument string passed to the skill."))
    :async-p t
    :read-only-p t
    :get-name (lambda (args) (plist-get args :name))
    :groups (util)
    :renderer '((success . niwa-skills--render-skill-tool)))
  (niwa-define-tool
    :name "ListSkills"
    :description "List active model-invocable skills, optionally filtered by query."
    :prompt-file "prompts/tools/listskills.md"
    :handler #'niwa-skills--list-handler
    :args ((query string :optional
                  "Optional case-insensitive search over skill name and description."))
    :async-p t
    :read-only-p t
    :groups (util)))

(provide 'niwa-tool-skills)
;;; niwa-tool-skills.el ends here
