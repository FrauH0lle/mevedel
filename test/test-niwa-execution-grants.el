;;; test-niwa-execution-grants.el --- Child coverage tests -*- lexical-binding: t -*-

;;; Commentary:
;; Native and child resource authority have deliberately different coverage.

;;; Code:

(require 'niwa-execution-grants)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-execution-grants--boundaries
  (:doc "policy roots, canonical aliases and concrete masks remain separate")
  (let* ((root (make-temp-file "niwa-boundaries-" t))
         (hidden (file-name-concat root "hidden"))
         (link (file-name-concat root "alias"))
         (file (file-name-concat hidden "value"))
         (niwa-protected-paths `((,link . inaccessible))))
    (unwind-protect
        (progn
          (make-directory hidden)
          (with-temp-file file (insert "value"))
          (make-symbolic-link hidden link)
          (should (equal (list hidden)
                         (niwa-execution-grants--boundaries file 'read)))
          (let ((niwa-protected-paths nil))
            (should (equal (list hidden)
                           (niwa-execution-grants--boundaries
                            file 'write nil `((:path ,hidden :mode read-only)))))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-grants-covering-grant
  (:doc "parent trees cannot reopen protected children; child trees preserve deeper and sibling boundaries")
  (let* ((root (make-temp-file "niwa-child-coverage-" t))
         (git (file-name-concat root ".git"))
         (file (file-name-concat git "value"))
         (deep (file-name-concat git "secret"))
         (sibling (file-name-concat root "other" ".git"))
         (parent `(:path ,root :access write :recursive t))
         (child `(:path ,git :access write :recursive t))
         (niwa-protected-paths `(("**/.git/**" . read-only)
                                    (,deep . inaccessible))))
    (unwind-protect
        (progn
          (make-directory git)
          (make-directory sibling t)
          (with-temp-file file)
          (with-temp-file deep)
          (should (niwa-permission-rules-resource-granted-p git 'write (list parent)))
          (should-not (niwa-execution-grants-covering-grant git 'write (list parent)))
          (should-not (niwa-execution-grants-covering-grant
                       git 'write (list `(:path ,git :access write))))
          (should (eq child (niwa-execution-grants-covering-grant git 'write (list parent child))))
          (should (eq child (niwa-execution-grants-covering-grant file 'write (list parent child))))
          (should-not (niwa-execution-grants-covering-grant deep 'write (list parent child)))
          (should-not (niwa-execution-grants-covering-grant sibling 'write (list parent child)))
          (should (niwa-execution-grants-covering-grant file 'read (list parent)))
          (should-not (niwa-execution-grants-covering-grant file 'write (list parent)))
          (should (niwa-execution-grants-covering-grant
                   file 'write (list parent `(:path ,file :access write))))
          (should-not (niwa-execution-grants-covering-grant file 'write (list parent) t))
          (let* ((outside (file-name-concat root "outside"))
                 (link (file-name-concat git "link"))
                 (alias (file-name-concat root "alias"))
                 (alias-file (file-name-concat alias "value")))
            (with-temp-file outside)
            (make-symbolic-link outside link)
            (make-symbolic-link git alias)
            (should-not (niwa-execution-grants-covering-grant alias-file 'write (list parent)))
            (should (niwa-execution-grants-covering-grant
                     alias-file 'write (list `(:path ,alias :access write :recursive t))))
            (should (niwa-execution-grants-covering-grant
                     link 'write (list `(:path ,link :access write))))))
      (delete-directory root t))))

(provide 'test-niwa-execution-grants)
;;; test-niwa-execution-grants.el ends here
