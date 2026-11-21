;;; test-niwa-plan-mode-remote.el --- Browser Plan approval -*- lexical-binding: t; -*-

;;; Commentary:

;; Owner Worktree approval uses the rendered descriptor and ordinary dispatcher.

;;; Code:

(require 'niwa)
(require 'niwa-plan-mode)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-plan-mode--render-approval/remote
  (:doc "owner accepts Worktree with a validated branch; full/view cannot accept"
   :quiet t)
  (let ((root (make-temp-file "niwa-plan-remote-" t)))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil "init" "--quiet" root)))
          (dolist (tier '(view full owner))
            (niwa-view-test--with-buffers
              (let* ((session (niwa-session--create
                               :name "Plan" :working-directory root))
                     (selection (list :location 'worktree :context 'current
                                      :execution 'direct :mode 'edits
                                      :model-provider "OpenAI:gpt-5"))
                     (entry (niwa-plan-mode--approval-entry
                             "# Implement" data-buf session selection))
                     (guests (make-hash-table :test #'equal))
                     (room (list :guests guests :ui-requests (make-hash-table)))
                     (niwa-collaboration-remote-interactions t)
                     overlay frame accepted)
                (puthash "peer" (list :name "Reviewer" :writable (not (eq tier 'view))
                                      :owner (eq tier 'owner)) guests)
                (with-current-buffer data-buf (setq-local niwa--session session))
                (cl-letf (((symbol-function 'niwa--prompt-announce)
                           (lambda (value)
                             (setq overlay value
                                   frame (niwa-collaboration--ui-request-frame 1 value))
                             (puthash 1 value (plist-get room :ui-requests))))
                          ((symbol-function 'niwa-plan-mode--worktree-warning) #'ignore)
                          ((symbol-function 'niwa-plan-handoff-assert-worktree-ready) #'ignore)
                          ((symbol-function 'niwa-model-resolve-provider) (lambda (&rest _) t))
                          ((symbol-function 'read-string)
                           (lambda (&rest _) (ert-fail "Browser approval prompted in Emacs")))
                          ((symbol-function 'niwa--prompt--settle)
                           (lambda (_ value) (setq accepted value))))
                  (niwa-plan-mode--render-approval entry)
                  (should (overlayp overlay))
                  (let ((remote (overlay-get overlay 'niwa--remote)))
                    (should (plist-get remote :options-owner-only))
                    (should (equal "worktree/accepted-plan"
                                   (plist-get (plist-get remote :option-input) :value)))
                    (should (plist-get remote :feedback)))
                  (should (eq t (plist-get frame :optionsOwnerOnly)))
                  (should (equal "Worktree branch"
                                 (plist-get (plist-get frame :optionInput) :label)))
                  (if (eq tier 'owner)
                      (progn
                        (dolist (value (list "" "invalid..branch" 42 '("injected")
                                             (make-string
                                              (1+ niwa-collaboration--max-prompt-bytes)
                                              ?x)))
                          (should-error
                           (niwa-collaboration--handle-ui-response
                            room "peer" (list :reqId 1 :option 0 :value value))
                           :type 'user-error)
                          (should-not accepted))
                        (niwa-collaboration--handle-ui-response
                         room "peer" '(:reqId 1 :option 0 :value "feature/browser-plan"))
                        (should (plist-get accepted :accept))
                        (should (equal "feature/browser-plan"
                                       (plist-get (plist-get accepted :selection) :branch))))
                    (niwa-collaboration--handle-ui-response
                     room "peer" '(:reqId 1 :option 0 :value "forged"))
                    (should-not accepted)))))))
      (delete-directory root t))))

(niwa-deftest niwa-plan-mode--render-approval/remote-here
  (:doc "Here acceptance remains available to full and owner, not view" :quiet t)
  (dolist (tier '(view full owner))
    (niwa-view-test--with-buffers
      (let* ((session (niwa-session--create :name "Plan"))
             (selection (list :location 'here :context 'current
                              :execution 'direct :mode 'edits
                              :model-provider "OpenAI:gpt-5"))
             (entry (niwa-plan-mode--approval-entry
                     "# Implement here" data-buf session selection))
             (guests (make-hash-table :test #'equal))
             (room (list :guests guests :ui-requests (make-hash-table)))
             (niwa-collaboration-remote-interactions t)
             accepted)
        (puthash "peer" (list :name "Reviewer" :writable (not (eq tier 'view))
                              :owner (eq tier 'owner)) guests)
        (with-current-buffer data-buf (setq-local niwa--session session))
        (cl-letf (((symbol-function 'niwa--prompt-announce)
                   (lambda (overlay)
                     (should-not (plist-get (overlay-get overlay 'niwa--remote)
                                            :options-owner-only))
                     (puthash 1 overlay (plist-get room :ui-requests))))
                  ((symbol-function 'niwa-model-resolve-provider) (lambda (&rest _) t))
                  ((symbol-function 'niwa--prompt--settle)
                   (lambda (_ value) (setq accepted value))))
          (niwa-plan-mode--render-approval entry)
          (niwa-collaboration--handle-ui-response room "peer" '(:reqId 1 :option 0))
          (should (eq (not (eq tier 'view)) (plist-get accepted :accept)))
          (when accepted
            (should (equal selection (plist-get accepted :selection)))))))))

(niwa-deftest niwa-plan-mode--render-approval/remote-oversized
  (:doc "oversized full disclosure retains feedback but requires native approval")
  (dolist (location '(here worktree))
    (niwa-view-test--with-buffers
      (let* ((session (niwa-session--create :name "Plan"))
             (instructions (make-string 20000 ?漢))
             (entry (niwa-plan-mode--approval-entry
                     "# Complete plan" data-buf session
                     (list :location location :context 'current
                           :execution 'direct :mode 'edits
                           :model-provider "OpenAI:gpt-5"
                           :instructions instructions)))
             remote)
        (cl-letf (((symbol-function 'niwa--prompt-announce)
                   (lambda (overlay) (setq remote (overlay-get overlay 'niwa--remote))))
                  ((symbol-function 'niwa-plan-mode--worktree-warning) #'ignore))
          (niwa-plan-mode--render-approval entry)
          (should-not (and (plist-get remote :options) t))
          (should-not (plist-get remote :option-input))
          (should (functionp (plist-get remote :feedback)))
          (should (string-search "Full review in Emacs" (plist-get remote :body)))
          (should (<= (string-bytes (json-serialize (vector (plist-get remote :body))))
                      57000))
          (should (equal instructions (plist-get (plist-get entry :selection) :instructions))))))))

(provide 'test-niwa-plan-mode-remote)
;;; test-niwa-plan-mode-remote.el ends here
