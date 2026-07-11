;;; mevedel-tool-skills.el --- Skill tool schemas -*- lexical-binding: t -*-

;;; Commentary:

;; Registers the model-facing Skill and ListSkills tool schemas.  Invocation,
;; lookup, and rendering behavior remains owned by `mevedel-skills'.

;;; Code:

(require 'mevedel-tool-registry)

;; `mevedel-skills'
(declare-function mevedel-skills--invoke-handler
                  "mevedel-skills" (callback args))
(declare-function mevedel-skills--list-handler
                  "mevedel-skills" (callback args))
(declare-function mevedel-skills--render-skill-tool
                  "mevedel-skills" (name args result render-data))


;;
;;; Registration

;;;###autoload
(defun mevedel-tool-skills--register ()
  "Register the Skill and ListSkills tools."
  (mevedel-define-tool
    :name "Skill"
    :description "Invoke a reusable prompt recipe (skill) by name."
    :handler #'mevedel-skills--invoke-handler
    :args ((name string :required
                 "The skill name (as shown in the skills listing).")
           (arguments string :optional
                      "Optional argument string passed to the skill."))
    :async-p t
    :read-only-p t
    :get-name (lambda (args) (plist-get args :name))
    :groups (util)
    :renderer #'mevedel-skills--render-skill-tool)
  (mevedel-define-tool
    :name "ListSkills"
    :description "List active model-invocable skills, optionally filtered by query."
    :handler #'mevedel-skills--list-handler
    :args ((query string :optional
                  "Optional case-insensitive search over skill name and description."))
    :async-p t
    :read-only-p t
    :groups (util)))

(provide 'mevedel-tool-skills)
;;; mevedel-tool-skills.el ends here
