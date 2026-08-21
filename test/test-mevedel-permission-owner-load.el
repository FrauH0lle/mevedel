;;; test-mevedel-permission-owner-load.el -- Cold permission owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the split permission owners and representative consumers without
;; loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-permission-owners/cold-load ()
  ,test
  (test)
  :doc "loads compiled owners and consumers through their real seams"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-permission-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("mevedel-bash-policy.el"
                   "mevedel-permission-mode.el"
                   "mevedel-permission-persistence.el"
                   "mevedel-permission-rules.el"
                   "mevedel-permissions.el"
                   "mevedel-sandbox.el"))
         (cases
          '((mode
             (progn
               (require 'mevedel-permission-mode)
               (unless (eq 'edits
                           (mevedel-permission-mode-normalize "edits"))
                 (error "Mode owner did not normalize mode"))))
            (rules
             (progn
               (require 'mevedel-permission-rules)
               (unless (equal
                        (list (list :path (expand-file-name default-directory)
                                    :access 'write))
                        (mevedel-permission-rules-merge-resource-grant
                         (list
                          (mevedel-permission-rules-resource-grant
                           default-directory 'read))
                         default-directory 'write))
                 (error "Rules owner did not merge resource authority"))))
            (persistence
             (progn
               (require 'mevedel-permission-persistence)
               (unless
                   (equal '(:rules (("Read" :action allow))
                                   :resource-grants nil)
                          (mevedel-permission-deserialize-authority
                           '(("Read" :action allow)) nil nil))
                 (error "Persistence owner did not decode authority"))))
            (mode-consumer
             (progn
               (require 'mevedel-bash-policy)
               (unless
                   (eq 'ask
                       (mevedel-bash-policy-effective-permission-mode))
                 (error "Mode consumer did not load its owner"))))
            (rules-consumer
             (progn
               (require 'mevedel-sandbox)
               (mevedel-sandbox--protected-candidates
                default-directory nil)
               (unless (featurep 'mevedel-permission-rules)
                 (error "Rules consumer did not load its owner"))))
            (facade-consumer
             (progn
               (require 'mevedel-permissions)
               (require 'mevedel-structs)
               (let* ((workspace
                       (mevedel-workspace--create
                        :type 'file :id "cold" :root default-directory))
                      (rule '("Read" :action allow))
                      (session
                       (mevedel-session--create
                        :workspace workspace :permission-rules (list rule))))
                 (mevedel-permission--invocation-context
                  :tool-name "ColdMissingTool" :workspace workspace
                  :workspace-root default-directory)
                 (mevedel-permission-remove-session-rule session rule)
                 (when (mevedel-session-permission-rules session)
                   (error "Facade owner did not revoke session authority")))
               (dolist (feature '(mevedel-permission-mode
                                  mevedel-permission-persistence
                                  mevedel-permission-rules))
                 (unless (featurep feature)
                   (error "Facade consumer did not load %s" feature))))))))
    (unwind-protect
        (progn
          (dolist (owner owners)
            (copy-file (file-name-concat root owner)
                       (file-name-concat compiled-root owner))
            (let ((byte-compile-verbose nil))
              (byte-compile-file (file-name-concat compiled-root owner))))
          (dolist (case cases)
            (with-temp-buffer
              (ert-info ((format "cold permission owner: %s" (car case)))
                (should
                 (= 0
                    (call-process
                     emacs nil t nil
                     "--batch" "-Q" "-L" compiled-root "-L" root
                     "--eval"
                     (prin1-to-string
                      `(progn
                         ,(cadr case)
                         (when (featurep 'mevedel)
                           (error "Permission owner loaded the umbrella")))))))
                (should
                 (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-permission-owner-load)
;;; test-mevedel-permission-owner-load.el ends here
