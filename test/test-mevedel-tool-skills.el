;;; test-mevedel-tool-skills.el --- Skill tool schema tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests registration of the model-facing Skill and ListSkills schemas.

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-tool-skills)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-skills--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers exactly Skill and ListSkills with skill-name extraction"
  (progn
    (mevedel-tool-skills--register)
    (should (equal '("ListSkills" "Skill")
                   (sort (mapcar #'mevedel-tool-name
                                 (mevedel-tool-all))
                         #'string<)))
    (let ((skill (mevedel-tool-get "Skill" "mevedel")))
      (should skill)
      (should (equal "example"
                     (funcall (mevedel-tool-get-name skill)
                              '(:name "example")))))))

(provide 'test-mevedel-tool-skills)
;;; test-mevedel-tool-skills.el ends here
