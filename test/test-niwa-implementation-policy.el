;;; test-niwa-implementation-policy.el -- Implementation policy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise policy validation and the production request-realization seam.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'gptel-openai)
(require 'gptel-request)
(require 'niwa-implementation-policy)
(require 'niwa-skills-invoke)
(require 'niwa-system)
(require 'niwa-view)

(defmacro niwa-implementation-test--with-models (&rest body)
  "Run BODY with isolated model metadata and registered providers."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil)
         (baseline-properties (copy-tree (symbol-plist 'scope-baseline)))
         (implementation-properties
          (copy-tree (symbol-plist 'scope-implementation))))
     (unwind-protect
         (progn
           (gptel-make-openai "Baseline" :key "test" :models '(scope-baseline))
           (gptel-make-openai "Implementation" :key "test"
                              :models '(scope-implementation))
           (put 'scope-baseline :reasoning-effort '(member low high))
           (put 'scope-implementation :reasoning-effort '(member low high))
           ,@body)
       (setplist 'scope-baseline baseline-properties)
       (setplist 'scope-implementation implementation-properties))))

(niwa-deftest niwa-implementation-policy-validate ()
  ,test
  (test)
  :doc "accepts nil and exact serializable choices without provider lookup"
  (should-not (niwa-implementation-policy-validate nil))
  (dolist (label '("Unavailable:model" "Claude Subscription:model"
                   "Local:namespace:model"))
    (let ((policy (list :model-provider label :reasoning-effort nil
                        :permission-mode 'edits)))
      (should (eq policy (niwa-implementation-policy-validate policy)))))

  :doc "rejects malformed, duplicate, missing and unknown fields"
  (dolist (policy
           '(t (:model-provider "A:b")
             (:model-provider "A:b" :permission-mode edits :permission-mode ask)
             (:model-provider "A:b" :reasoning-effort nil :unknown edits)
             (:model-provider " " :reasoning-effort nil :permission-mode edits)
             (:model-provider "A: " :reasoning-effort nil :permission-mode edits)
             (:model-provider " :b" :reasoning-effort nil :permission-mode edits)
             (:model-provider "A:b" :reasoning-effort "high" :permission-mode edits)
             (:model-provider "A:b" :reasoning-effort :high :permission-mode edits)
             (:model-provider "A:b" :reasoning-effort high :permission-mode plan)))
    (should-error (niwa-implementation-policy-validate policy))))

(niwa-deftest niwa-implementation-policy-from-selection ()
  ,test
  (test)
  :doc "projects accepted choices without context, skills or Goal budget"
  (should
   (equal '(:model-provider "A:b" :reasoning-effort nil :permission-mode edits)
          (niwa-implementation-policy-from-selection
           '(:location here :context current :execution goal :mode edits
             :model-provider "A:b" :goal-token-budget 123 :skills (skill))))))

(niwa-deftest niwa-implementation-policy-model ()
  ,test
  (test)
  :doc "resolves the exact accepted provider and explicit default effort"
  (niwa-implementation-test--with-models
    (let ((gptel-reasoning-effort 'high))
      (dolist (effort '(nil low high))
        (let ((model
               (niwa-implementation-policy-model
                (list :model-provider "Implementation:scope-implementation"
                      :reasoning-effort effort :permission-mode 'edits))))
          (should (eq 'scope-implementation (plist-get model :model)))
          (should (eq (gptel-get-backend "Implementation")
                      (plist-get model :backend)))
          (should (eq effort (plist-get model :effort)))))
      (should (eq 'high gptel-reasoning-effort))))

  :doc "rejects unavailable providers and unsupported effort without fallback"
  (niwa-implementation-test--with-models
    (let ((niwa-model-fallback-provider "Baseline:scope-baseline"))
      (dolist (policy
               '((:model-provider "Missing:model" :reasoning-effort nil
                  :permission-mode edits)
                 (:model-provider "Implementation:scope-implementation"
                  :reasoning-effort impossible :permission-mode edits)))
        (should-error (niwa-implementation-policy-model policy))))))

(niwa-deftest niwa-implementation-policy-current ()
  ,test
  (test)
  :doc "projects only active root Goals and honors captured request ownership"
  (let* ((policy '(:model-provider "A:b" :reasoning-effort low
                  :permission-mode edits))
         (goal (niwa-goal--create :status 'active :implementation-policy policy))
         (session (niwa-session--create :goal goal)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (niwa-session-set-root-buffer session (current-buffer))
      (should (equal policy (niwa-implementation-policy-current)))
      (dolist (status '(paused blocked budget-limited complete))
        (setf (niwa-goal-status goal) status)
        (should-not (niwa-implementation-policy-current)))
      (setf (niwa-goal-status goal) 'active)
      (let ((niwa--agent-invocation t))
        (should-not (niwa-implementation-policy-current)))
      (setf (niwa-session-plan-mode session) t)
      (should-not (niwa-implementation-policy-current))
      (setf (niwa-session-plan-mode session) nil)
      (setq-local niwa--current-request
                  (niwa-request--create :session session))
      (should-not (niwa-implementation-policy-current))
      (setf (niwa-request-implementation-policy niwa--current-request) policy
            (niwa-goal-status goal) 'paused)
      (should (equal policy (niwa-implementation-policy-current)))
      (should-not
       (niwa-implementation-policy-current (niwa-session--create))))))

(niwa-deftest niwa-skills-request-model-policy ()
  ,test
  (test)
  :doc "realizes Direct policy in gptel while preserving baseline locals and slots"
  (niwa-implementation-test--with-models
    (with-temp-buffer
      (let ((session (niwa-session--create
                      :model-provider "Baseline:scope-baseline"
                      :reasoning-effort 'high :permission-mode 'ask)))
        (setq-local niwa--session session
                    gptel-backend (gptel-get-backend "Baseline")
                    gptel-model 'scope-baseline
                    gptel-reasoning-effort 'high
                    niwa-model-workloads nil
                    niwa--pending-implementation-policy
                    '(:model-provider "Implementation:scope-implementation"
                      :reasoning-effort low :permission-mode edits))
        (let* ((gptel-use-tools nil)
               (fsm (gptel-request
                     "Implement" :system nil :dry-run t
                     :transforms '(niwa-skills--transform-apply-request-model-policy)))
               (info (gptel-fsm-info fsm)))
          (should (eq 'scope-implementation (plist-get info :model)))
          (should (eq (gptel-get-backend "Implementation") (plist-get info :backend)))
          (should (eq 'low (plist-get info :reasoning-effort)))
          (should (eq niwa--pending-implementation-policy
                      (plist-get info :niwa-pending-implementation-policy)))
          (should (plist-member info :niwa-pending-permission-mode))
          (let ((original niwa--pending-implementation-policy)
                (replacement (copy-tree niwa--pending-implementation-policy)))
            (setq-local niwa--pending-implementation-policy replacement)
            (niwa--turn-clear-pending-policy fsm)
            (should (eq replacement niwa--pending-implementation-policy))
            (setq-local niwa--pending-implementation-policy original)
            (niwa--turn-clear-pending-policy fsm)
            (should-not niwa--pending-implementation-policy)))
        (should (eq 'scope-baseline gptel-model))
        (should (eq 'high gptel-reasoning-effort))
        (should (equal "Baseline:scope-baseline" (niwa-session-model-provider session)))
        (should (eq 'ask (niwa-session-permission-mode session)))
        (should (eq 'high (niwa-session-reasoning-effort session))))))

  :doc "Goal turns use stored choices and inactive chat uses the latest baseline"
  (niwa-implementation-test--with-models
    (with-temp-buffer
      (let* ((goal (niwa-goal--create
                    :status 'active
                    :implementation-policy
                    '(:model-provider "Implementation:scope-implementation"
                      :reasoning-effort nil :permission-mode edits)))
             (session (niwa-session--create :goal goal)))
        (niwa-session-set-root-buffer session (current-buffer))
        (setq-local niwa--session session
                    gptel-backend (gptel-get-backend "Baseline")
                    gptel-model 'scope-baseline
                    gptel-reasoning-effort 'high
                    niwa-model-workloads nil)
        (should (eq 'scope-implementation
                    (plist-get (niwa-skills-request-model-policy) :model)))
        (should-not (plist-get (niwa-skills-request-model-policy) :effort))
        (dolist (status '(paused blocked budget-limited complete))
          (setf (niwa-goal-status goal) status)
          (should (eq 'scope-baseline
                      (plist-get (niwa-skills-request-model-policy) :model)))
          (should (eq 'high (plist-get (niwa-skills-request-model-policy) :effort))))
        (setf (niwa-goal-status goal) 'active)
        (should (eq 'scope-implementation
                    (plist-get (niwa-skills-request-model-policy) :model))))))

  :doc "owning skill fields keep model precedence without changing accepted choices"
  (niwa-implementation-test--with-models
    (with-temp-buffer
      (setq-local niwa--session (niwa-session--create :permission-mode 'ask)
                  gptel-backend (gptel-get-backend "Baseline")
                  gptel-model 'scope-baseline
                  gptel-reasoning-effort 'low
                  niwa-model-workloads nil
                  niwa--pending-implementation-policy
                  '(:model-provider "Implementation:scope-implementation"
                    :reasoning-effort nil :permission-mode edits)
                  niwa-skills--pending-request-context
                  (list :model (niwa-model-resolve-provider "Baseline:scope-baseline")
                        :effort 'high))
      (let ((policy (niwa-skills-request-model-policy)))
        (should (eq 'scope-baseline (plist-get policy :model)))
        (should (eq 'high (plist-get policy :effort))))
      (should-not (plist-get niwa--pending-implementation-policy :reasoning-effort))
      (should (equal "Implementation:scope-implementation"
                     (plist-get niwa--pending-implementation-policy :model-provider)))
      (should (eq 'edits (niwa-permission-mode-effective niwa--session)))
      (should (eq 'low gptel-reasoning-effort)))))

(niwa-deftest niwa-view--status-strip/implementation-model
  (:doc "shows implementation then baseline models without changing a composer draft")
  (let* ((data (generate-new-buffer " *implementation-status-data*"))
         (view (generate-new-buffer " *implementation-status-view*"))
         (policy '(:model-provider "Implementation:scope-implementation"
                   :reasoning-effort nil :permission-mode edits))
         (goal (niwa-goal--create :status 'active :turns-run 0
                                     :implementation-policy policy))
         (session (niwa-session--create :name "scope" :permission-mode 'ask
                                          :goal goal)))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local niwa--session session gptel-model 'scope-baseline)
            (niwa-session-set-root-buffer session data))
          (niwa-view--setup view data)
          (with-current-buffer view
            (niwa-view-composer-insert "Keep this draft\nwith its second line")
            (let ((draft (niwa-view-composer-draft-text))
                  (position (point)))
              (should (string-search "scope-implementation" (niwa-view--status-strip)))
              (setf (niwa-goal-status goal) 'paused)
              (should (string-search "scope-baseline" (niwa-view--status-strip)))
              (with-current-buffer data
                (setq-local niwa--current-request
                            (niwa-request--create
                             :session session :implementation-policy policy
                             :model-policy '(:model scope-implementation)
                             :permission-mode-override 'edits)))
              (should (string-search "scope-implementation" (niwa-view--status-strip)))
              (with-current-buffer data (setq-local niwa--current-request nil))
              (should (string-search "scope-baseline" (niwa-view--status-strip)))
              (should (equal draft (niwa-view-composer-draft-text)))
              (should (= position (point))))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data)))))

(niwa-deftest niwa-system--effective-prompt-report/implementation-policy
  (:doc "reports actual implementation model, effort and mode without changing defaults")
  (niwa-implementation-test--with-models
    (with-temp-buffer
      (let ((session (niwa-session--create :permission-mode 'ask
                                             :reasoning-effort 'high)))
        (setq-local niwa--session session
                    gptel-backend (gptel-get-backend "Baseline")
                    gptel-model 'scope-baseline
                    gptel-reasoning-effort 'high
                    gptel-system-prompt "Report fixture"
                    gptel-tools nil
                    niwa-model-workloads nil
                    niwa--pending-implementation-policy
                    '(:model-provider "Implementation:scope-implementation"
                      :reasoning-effort nil :permission-mode edits))
        (let* ((report (niwa-system--effective-prompt-report (current-buffer)))
               (context (plist-get (car (plist-get report :sections)) :body)))
          (should (string-search "Backend: Implementation" context))
          (should (string-search "Model: scope-implementation" context))
          (should (string-search "Reasoning effort: default" context))
          (should (string-search "Permission mode: edits" context)))
        (should (eq 'scope-baseline gptel-model))
        (should (eq 'high gptel-reasoning-effort))
        (should (eq 'high (niwa-session-reasoning-effort session)))
        (should (eq 'ask (niwa-session-permission-mode session)))))))

(provide 'test-niwa-implementation-policy)
;;; test-niwa-implementation-policy.el ends here
