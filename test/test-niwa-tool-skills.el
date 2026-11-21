;;; test-niwa-tool-skills.el --- Skill tool schema tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests registration of the model-facing Skill and ListSkills schemas.

;;; Code:

(require 'niwa-tool-registry)
(require 'niwa-tool-skills)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-tool-skills--register
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "registers exactly Skill and ListSkills with skill-name extraction"
  (progn
    (niwa-tool-skills--register)
    (should (equal '("ListSkills" "Skill")
                   (sort (mapcar #'niwa-tool-name
                                 (niwa-tool-all))
                         #'string<)))
    (let ((skill (niwa-tool-get "Skill" "niwa")))
      (should skill)
      (should (equal "example"
                     (funcall (niwa-tool-get-name skill)
                              '(:name "example")))))))

(provide 'test-niwa-tool-skills)
;;; test-niwa-tool-skills.el ends here
