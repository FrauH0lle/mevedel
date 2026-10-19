;;; test-mevedel-plan-mode-remote.el --- Browser Plan approval -*- lexical-binding: t; -*-

;;; Commentary:

;; Owner Worktree approval uses the rendered descriptor and ordinary dispatcher.

;;; Code:

(require 'mevedel)
(require 'mevedel-plan-mode)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-plan-mode--remote-body
  (:doc "directive rendering discloses instructions without unavailable axes")
  (let* ((instructions "Keep α intact.\nVerify rollback and retry.")
         (body (mevedel-plan-mode--remote-body
                (list :body "# Directive proposal" :directive t
                      :selection (list :location 'here :context 'current
                                       :execution 'direct :mode 'edits
                                       :model-provider "Provider:model"
                                       :instructions instructions))
                nil)))
    (should (string-search instructions body))
    (should (string-search "Directive implementation" body))
    (should (string-search "**Model:** Provider:model" body))
    (should-not (string-search "**Location:**" body))
    (should-not (string-search "**Context:**" body))
    (should-not (string-search "**Execution:**" body))
    (should-not (string-search "**Budget:**" body))
    (should-not (string-search "RET" body))))

(mevedel-deftest mevedel-plan-mode--render-approval/remote
  (:doc "owner accepts Worktree with a validated branch; full/view cannot accept"
   :quiet t)
  (let ((root (make-temp-file "mevedel-plan-remote-" t)))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil "init" "--quiet" root)))
          (dolist (tier '(view full owner))
            (mevedel-view-test--with-buffers
              (let* ((session (mevedel-session--create
                               :name "Plan" :working-directory root))
                     (selection (list :location 'worktree :context 'current
                                      :execution 'direct :mode 'edits
                                      :model-provider "OpenAI:gpt-5"))
                     (entry (mevedel-plan-mode--approval-entry
                             "# Implement" data-buf session selection))
                     (guests (make-hash-table :test #'equal))
                     (room (list :guests guests :ui-requests (make-hash-table)))
                     (mevedel-collaboration-remote-interactions t)
                     overlay frame accepted)
                (puthash "peer" (list :name "Reviewer" :writable (not (eq tier 'view))
                                      :owner (eq tier 'owner)) guests)
                (with-current-buffer data-buf (setq-local mevedel--session session))
                (cl-letf (((symbol-function 'mevedel--prompt-announce)
                           (lambda (value)
                             (setq overlay value
                                   frame (mevedel-collaboration--ui-request-frame 1 value))
                             (puthash 1 value (plist-get room :ui-requests))))
                          ((symbol-function 'mevedel-plan-mode--worktree-warning) #'ignore)
                          ((symbol-function 'mevedel-plan-handoff-assert-worktree-ready) #'ignore)
                          ((symbol-function 'mevedel-model-resolve-provider) (lambda (&rest _) t))
                          ((symbol-function 'read-string)
                           (lambda (&rest _) (ert-fail "Browser approval prompted in Emacs")))
                          ((symbol-function 'mevedel--prompt--settle)
                           (lambda (_ value) (setq accepted value))))
                  (mevedel-plan-mode--render-approval entry)
                  (should (overlayp overlay))
                  (let ((remote (overlay-get overlay 'mevedel--remote)))
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
                                              (1+ mevedel-collaboration--max-prompt-bytes)
                                              ?x)))
                          (should-error
                           (mevedel-collaboration--handle-ui-response
                            room "peer" (list :reqId 1 :option 0 :value value))
                           :type 'user-error)
                          (should-not accepted))
                        (mevedel-collaboration--handle-ui-response
                         room "peer" '(:reqId 1 :option 0 :value "feature/browser-plan"))
                        (should (plist-get accepted :accept))
                        (should (equal "feature/browser-plan"
                                       (plist-get (plist-get accepted :selection) :branch))))
                    (mevedel-collaboration--handle-ui-response
                     room "peer" '(:reqId 1 :option 0 :value "forged"))
                    (should-not accepted)))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-plan-mode--render-approval/remote-here
  (:doc "Here acceptance remains available to full and owner, not view" :quiet t)
  (dolist (tier '(view full owner))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create :name "Plan"))
             (selection (list :location 'here :context 'current
                              :execution 'direct :mode 'edits
                              :model-provider "OpenAI:gpt-5"))
             (entry (mevedel-plan-mode--approval-entry
                     "# Implement here" data-buf session selection))
             (guests (make-hash-table :test #'equal))
             (room (list :guests guests :ui-requests (make-hash-table)))
             (mevedel-collaboration-remote-interactions t)
             accepted)
        (puthash "peer" (list :name "Reviewer" :writable (not (eq tier 'view))
                              :owner (eq tier 'owner)) guests)
        (with-current-buffer data-buf (setq-local mevedel--session session))
        (cl-letf (((symbol-function 'mevedel--prompt-announce)
                   (lambda (overlay)
                     (should-not (plist-get (overlay-get overlay 'mevedel--remote)
                                            :options-owner-only))
                     (puthash 1 overlay (plist-get room :ui-requests))))
                  ((symbol-function 'mevedel-model-resolve-provider) (lambda (&rest _) t))
                  ((symbol-function 'mevedel--prompt--settle)
                   (lambda (_ value) (setq accepted value))))
          (mevedel-plan-mode--render-approval entry)
          (mevedel-collaboration--handle-ui-response room "peer" '(:reqId 1 :option 0))
          (should (eq (not (eq tier 'view)) (plist-get accepted :accept)))
          (when accepted
            (should (equal selection (plist-get accepted :selection)))))))))

(mevedel-deftest mevedel-plan-mode--render-approval/remote-oversized
  (:doc "oversized full disclosure retains feedback but requires native approval")
  (dolist (location '(here worktree))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create :name "Plan"))
             (instructions (make-string 20000 ?漢))
             (entry (mevedel-plan-mode--approval-entry
                     "# Complete plan" data-buf session
                     (list :location location :context 'current
                           :execution 'direct :mode 'edits
                           :model-provider "OpenAI:gpt-5"
                           :instructions instructions)))
             remote)
        (cl-letf (((symbol-function 'mevedel--prompt-announce)
                   (lambda (overlay) (setq remote (overlay-get overlay 'mevedel--remote))))
                  ((symbol-function 'mevedel-plan-mode--worktree-warning) #'ignore))
          (mevedel-plan-mode--render-approval entry)
          (should-not (and (plist-get remote :options) t))
          (should-not (plist-get remote :option-input))
          (should (functionp (plist-get remote :feedback)))
          (should (string-search "Full review in Emacs" (plist-get remote :body)))
          (should (<= (string-bytes (json-serialize (vector (plist-get remote :body))))
                      57000))
          (should (equal instructions (plist-get (plist-get entry :selection) :instructions))))))))

(provide 'test-mevedel-plan-mode-remote)
;;; test-mevedel-plan-mode-remote.el ends here
