;;; test-niwa-permission-owner-load.el -- Cold permission owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the split permission owners and representative consumers without
;; loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-permission-owners/cold-load ()
  ,test
  (test)
  :doc "loads compiled owners and consumers through their real seams"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-permission-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("niwa-bash-policy.el"
                   "niwa-permission-mode.el"
                   "niwa-permission-prompt.el"
                   "niwa-permission-queue.el"
                   "niwa-permission-persistence.el"
                   "niwa-permission-rules.el"
                   "niwa-permissions.el"
                   "niwa-sandbox.el"))
         (cases
          '((mode
             (progn
               (require 'niwa-permission-mode)
               (unless (eq 'edits
                           (niwa-permission-mode-normalize "edits"))
                 (error "Mode owner did not normalize mode"))))
            (rules
             (progn
               (require 'niwa-permission-rules)
               (unless (equal
                        (list (list :path (expand-file-name default-directory)
                                    :access 'write))
                        (niwa-permission-rules-merge-resource-grant
                         (list
                          (niwa-permission-rules-resource-grant
                           default-directory 'read))
                         default-directory 'write))
                 (error "Rules owner did not merge resource authority"))))
            (persistence
             (progn
               (require 'niwa-permission-persistence)
               (unless
                   (equal '(:rules (("Read" :action allow))
                                   :resource-grants nil)
                          (niwa-permission-deserialize-authority
                           '(("Read" :action allow)) nil nil))
                 (error "Persistence owner did not decode authority"))))
            (mode-consumer
             (progn
               (require 'niwa-bash-policy)
               (unless
                   (eq 'ask
                       (niwa-bash-policy-effective-permission-mode))
                 (error "Mode consumer did not load its owner"))))
            (rules-consumer
             (progn
               (require 'niwa-sandbox)
               (niwa-sandbox--protected-candidates
                default-directory nil)
               (unless (featurep 'niwa-permission-rules)
                 (error "Rules consumer did not load its owner"))))
            (prompt-consumer
             (progn
               (require 'niwa-permission-queue)
               (let* ((session (niwa-session--create :name "cold-prompt"))
                      (entry (list :kind 'eval :expression "(+ 1 2)"
                                   :session session :mode "fresh"))
                      body)
                 (setf (niwa-session-permission-queue session) (list entry))
                 (cl-letf
                     (((symbol-function 'niwa-view--interaction-target-buffer)
                       (lambda (&optional _) (current-buffer)))
                      ((symbol-function 'niwa-view--interaction-register)
                       (lambda (descriptor)
                         (setq body (plist-get descriptor :body))
                         (make-overlay (point-min) (point-min)))))
                   (niwa-permission-queue--render-entry entry))
                 (unless (and (string-match-p "Mode: fresh" body)
                              (string-search "(+ 1 2)" body)
                              (featurep 'niwa-permission-prompt)
                              (not (featurep 'niwa-tool-exec-permission)))
                   (error "Eval prompt did not load through its UI owner")))))
            (facade-consumer
             (progn
               (require 'niwa-permissions)
               (require 'niwa-structs)
               (let* ((workspace
                       (niwa-workspace--create
                        :type 'file :id "cold" :root default-directory))
                      (rule '("Read" :action allow))
                      (session
                       (niwa-session--create
                        :workspace workspace :permission-rules (list rule))))
                 (niwa-permission--invocation-context
                  :tool-name "ColdMissingTool" :workspace workspace
                  :workspace-root default-directory)
                 (niwa-permission-remove-session-rule session rule)
                 (when (niwa-session-permission-rules session)
                   (error "Facade owner did not revoke session authority")))
               (dolist (feature '(niwa-permission-mode
                                  niwa-permission-persistence
                                  niwa-permission-rules))
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
                (let ((status
                       (call-process
                        emacs nil t nil
                        "--batch" "-Q" "-L" compiled-root "-L" root
                        "--eval"
                        (prin1-to-string
                         `(progn
                            ,(cadr case)
                            (dolist (owner ',owners)
                              (when (featurep (intern (file-name-base owner)))
                                (unless (assoc (concat (file-name-concat
                                                        ,compiled-root owner) "c")
                                               load-history)
                                  (error "Owner did not load compiled code: %s"
                                         owner))))
                            (when (featurep 'niwa)
                              (error "Permission owner loaded the umbrella")))))))
                  (ert-info ((buffer-string))
                    (should (= 0 status))))
                (should
                 (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-permission-owner-load)
;;; test-niwa-permission-owner-load.el ends here
