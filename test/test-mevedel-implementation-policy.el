;;; test-mevedel-implementation-policy.el -- Implementation policy tests -*- lexical-binding: t -*-

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
(require 'mevedel-implementation-policy)
(require 'mevedel-skills-invoke)
(require 'mevedel-system)
(require 'mevedel-view)

(defmacro mevedel-implementation-test--with-models (&rest body)
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

(mevedel-deftest mevedel-implementation-policy-validate ()
  ,test
  (test)
  :doc "accepts nil and exact serializable choices without provider lookup"
  (should-not (mevedel-implementation-policy-validate nil))
  (dolist (label '("Unavailable:model" "Claude Subscription:model"
                   "Local:namespace:model"))
    (let ((policy (list :model-provider label :reasoning-effort nil
                        :permission-mode 'edits)))
      (should (eq policy (mevedel-implementation-policy-validate policy)))))

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
    (should-error (mevedel-implementation-policy-validate policy))))

(mevedel-deftest mevedel-implementation-policy-from-selection ()
  ,test
  (test)
  :doc "projects accepted choices without context, skills or Goal budget"
  (should
   (equal '(:model-provider "A:b" :reasoning-effort nil :permission-mode edits)
          (mevedel-implementation-policy-from-selection
           '(:location here :context current :execution goal :mode edits
             :model-provider "A:b" :goal-token-budget 123 :skills (skill))))))

(mevedel-deftest mevedel-implementation-policy-model ()
  ,test
  (test)
  :doc "resolves the exact accepted provider and explicit default effort"
  (mevedel-implementation-test--with-models
    (let ((gptel-reasoning-effort 'high))
      (dolist (effort '(nil low high))
        (let ((model
               (mevedel-implementation-policy-model
                (list :model-provider "Implementation:scope-implementation"
                      :reasoning-effort effort :permission-mode 'edits))))
          (should (eq 'scope-implementation (plist-get model :model)))
          (should (eq (gptel-get-backend "Implementation")
                      (plist-get model :backend)))
          (should (eq effort (plist-get model :effort)))))
      (should (eq 'high gptel-reasoning-effort))))

  :doc "rejects unavailable providers and unsupported effort without fallback"
  (mevedel-implementation-test--with-models
    (let ((mevedel-model-fallback-provider "Baseline:scope-baseline"))
      (dolist (policy
               '((:model-provider "Missing:model" :reasoning-effort nil
                  :permission-mode edits)
                 (:model-provider "Implementation:scope-implementation"
                  :reasoning-effort impossible :permission-mode edits)))
        (should-error (mevedel-implementation-policy-model policy))))))

(mevedel-deftest mevedel-implementation-policy-current ()
  ,test
  (test)
  :doc "projects only active root Goals and honors captured request ownership"
  (let* ((policy '(:model-provider "A:b" :reasoning-effort low
                  :permission-mode edits))
         (goal (mevedel-goal--create :status 'active :implementation-policy policy))
         (session (mevedel-session--create :goal goal)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (mevedel-session-set-root-buffer session (current-buffer))
      (should (equal policy (mevedel-implementation-policy-current)))
      (dolist (status '(paused blocked budget-limited complete))
        (setf (mevedel-goal-status goal) status)
        (should-not (mevedel-implementation-policy-current)))
      (setf (mevedel-goal-status goal) 'active)
      (let ((mevedel--agent-invocation t))
        (should-not (mevedel-implementation-policy-current)))
      (let ((mevedel--current-directive-uuid "directive"))
        (should-not (mevedel-implementation-policy-current)))
      (setf (mevedel-session-plan-mode session) t)
      (should-not (mevedel-implementation-policy-current))
      (setf (mevedel-session-plan-mode session) nil)
      (setq-local mevedel--current-request
                  (mevedel-request--create :session session))
      (should-not (mevedel-implementation-policy-current))
      (setf (mevedel-request-implementation-policy mevedel--current-request) policy
            (mevedel-goal-status goal) 'paused)
      (should (equal policy (mevedel-implementation-policy-current)))
      (should-not
       (mevedel-implementation-policy-current (mevedel-session--create))))))

(mevedel-deftest mevedel-skills-request-model-policy ()
  ,test
  (test)
  :doc "realizes Direct policy in gptel while preserving baseline locals and slots"
  (mevedel-implementation-test--with-models
    (with-temp-buffer
      (let ((session (mevedel-session--create
                      :model-provider "Baseline:scope-baseline"
                      :reasoning-effort 'high :permission-mode 'ask)))
        (setq-local mevedel--session session
                    gptel-backend (gptel-get-backend "Baseline")
                    gptel-model 'scope-baseline
                    gptel-reasoning-effort 'high
                    mevedel-model-workloads nil
                    mevedel--pending-implementation-policy
                    '(:model-provider "Implementation:scope-implementation"
                      :reasoning-effort low :permission-mode edits))
        (let* ((gptel-use-tools nil)
               (fsm (gptel-request
                     "Implement" :system nil :dry-run t
                     :transforms '(mevedel-skills--transform-apply-request-model-policy)))
               (info (gptel-fsm-info fsm)))
          (should (eq 'scope-implementation (plist-get info :model)))
          (should (eq (gptel-get-backend "Implementation") (plist-get info :backend)))
          (should (eq 'low (plist-get info :reasoning-effort)))
          (should (eq mevedel--pending-implementation-policy
                      (plist-get info :mevedel-pending-implementation-policy)))
          (should (plist-member info :mevedel-pending-permission-mode))
          (let ((original mevedel--pending-implementation-policy)
                (replacement (copy-tree mevedel--pending-implementation-policy)))
            (setq-local mevedel--pending-implementation-policy replacement)
            (mevedel--turn-clear-pending-policy fsm)
            (should (eq replacement mevedel--pending-implementation-policy))
            (setq-local mevedel--pending-implementation-policy original)
            (mevedel--turn-clear-pending-policy fsm)
            (should-not mevedel--pending-implementation-policy)))
        (should (eq 'scope-baseline gptel-model))
        (should (eq 'high gptel-reasoning-effort))
        (should (equal "Baseline:scope-baseline" (mevedel-session-model-provider session)))
        (should (eq 'ask (mevedel-session-permission-mode session)))
        (should (eq 'high (mevedel-session-reasoning-effort session))))))

  :doc "Goal turns use stored choices and inactive chat uses the latest baseline"
  (mevedel-implementation-test--with-models
    (with-temp-buffer
      (let* ((goal (mevedel-goal--create
                    :status 'active
                    :implementation-policy
                    '(:model-provider "Implementation:scope-implementation"
                      :reasoning-effort nil :permission-mode edits)))
             (session (mevedel-session--create :goal goal)))
        (mevedel-session-set-root-buffer session (current-buffer))
        (setq-local mevedel--session session
                    gptel-backend (gptel-get-backend "Baseline")
                    gptel-model 'scope-baseline
                    gptel-reasoning-effort 'high
                    mevedel-model-workloads nil)
        (should (eq 'scope-implementation
                    (plist-get (mevedel-skills-request-model-policy) :model)))
        (should-not (plist-get (mevedel-skills-request-model-policy) :effort))
        (dolist (status '(paused blocked budget-limited complete))
          (setf (mevedel-goal-status goal) status)
          (should (eq 'scope-baseline
                      (plist-get (mevedel-skills-request-model-policy) :model)))
          (should (eq 'high (plist-get (mevedel-skills-request-model-policy) :effort))))
        (setf (mevedel-goal-status goal) 'active)
        (should (eq 'scope-implementation
                    (plist-get (mevedel-skills-request-model-policy) :model))))))

  :doc "owning skill fields keep model precedence without changing accepted choices"
  (mevedel-implementation-test--with-models
    (with-temp-buffer
      (setq-local mevedel--session (mevedel-session--create :permission-mode 'ask)
                  gptel-backend (gptel-get-backend "Baseline")
                  gptel-model 'scope-baseline
                  gptel-reasoning-effort 'low
                  mevedel-model-workloads nil
                  mevedel--pending-implementation-policy
                  '(:model-provider "Implementation:scope-implementation"
                    :reasoning-effort nil :permission-mode edits)
                  mevedel-skills--pending-request-context
                  (list :model (mevedel-model-resolve-provider "Baseline:scope-baseline")
                        :effort 'high))
      (let ((policy (mevedel-skills-request-model-policy)))
        (should (eq 'scope-baseline (plist-get policy :model)))
        (should (eq 'high (plist-get policy :effort))))
      (should-not (plist-get mevedel--pending-implementation-policy :reasoning-effort))
      (should (equal "Implementation:scope-implementation"
                     (plist-get mevedel--pending-implementation-policy :model-provider)))
      (should (eq 'edits (mevedel-permission-mode-effective mevedel--session)))
      (should (eq 'low gptel-reasoning-effort)))))

(mevedel-deftest mevedel-view--status-strip/implementation-model
  (:doc "shows implementation then baseline models without changing a composer draft")
  (let* ((data (generate-new-buffer " *implementation-status-data*"))
         (view (generate-new-buffer " *implementation-status-view*"))
         (policy '(:model-provider "Implementation:scope-implementation"
                   :reasoning-effort nil :permission-mode edits))
         (goal (mevedel-goal--create :status 'active :turns-run 0
                                     :implementation-policy policy))
         (session (mevedel-session--create :name "scope" :permission-mode 'ask
                                          :goal goal)))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel--session session gptel-model 'scope-baseline)
            (mevedel-session-set-root-buffer session data))
          (mevedel-view--setup view data)
          (with-current-buffer view
            (goto-char (mevedel-view--input-start))
            (insert "Keep this draft\nwith its second line")
            (let ((draft (mevedel-view--input-text))
                  (position (point)))
              (should (string-search "scope-implementation" (mevedel-view--status-strip)))
              (setf (mevedel-goal-status goal) 'paused)
              (should (string-search "scope-baseline" (mevedel-view--status-strip)))
              (with-current-buffer data
                (setq-local mevedel--current-request
                            (mevedel-request--create
                             :session session :implementation-policy policy
                             :model-policy '(:model scope-implementation)
                             :permission-mode-override 'edits)))
              (should (string-search "scope-implementation" (mevedel-view--status-strip)))
              (with-current-buffer data (setq-local mevedel--current-request nil))
              (should (string-search "scope-baseline" (mevedel-view--status-strip)))
              (should (equal draft (mevedel-view--input-text)))
              (should (= position (point))))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data)))))

(mevedel-deftest mevedel-system--effective-prompt-report/implementation-policy
  (:doc "reports actual implementation model, effort and mode without changing defaults")
  (mevedel-implementation-test--with-models
    (with-temp-buffer
      (let ((session (mevedel-session--create :permission-mode 'ask
                                             :reasoning-effort 'high)))
        (setq-local mevedel--session session
                    gptel-backend (gptel-get-backend "Baseline")
                    gptel-model 'scope-baseline
                    gptel-reasoning-effort 'high
                    gptel-system-prompt "Report fixture"
                    gptel-tools nil
                    mevedel-model-workloads nil
                    mevedel--pending-implementation-policy
                    '(:model-provider "Implementation:scope-implementation"
                      :reasoning-effort nil :permission-mode edits))
        (let* ((report (mevedel-system--effective-prompt-report (current-buffer)))
               (context (plist-get (car (plist-get report :sections)) :body)))
          (should (string-search "Backend: Implementation" context))
          (should (string-search "Model: scope-implementation" context))
          (should (string-search "Reasoning effort: default" context))
          (should (string-search "Permission mode: edits" context)))
        (should (eq 'scope-baseline gptel-model))
        (should (eq 'high gptel-reasoning-effort))
        (should (eq 'high (mevedel-session-reasoning-effort session)))
        (should (eq 'ask (mevedel-session-permission-mode session)))))))

(provide 'test-mevedel-implementation-policy)
;;; test-mevedel-implementation-policy.el ends here
