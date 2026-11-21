;;; test-niwa-implementation-startup.el -- Implementation startup regressions -*- lexical-binding: t -*-

;;; Commentary:

;; Production composer admission and durable Goal failure handling with real
;; published sessions.  Only the provider send/authentication boundary is faked.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-implementation-policy)
(require 'niwa-goal)
(require 'niwa-plan)
(require 'niwa-view-composer)

(defmacro niwa-implementation-startup-test--with-goal (policy &rest body)
  "Run BODY with POLICY on a real accepted-plan Goal and composer view."
  (declare (indent 1) (debug (form body)))
  `(niwa-engine-test--with-session
     (niwa-request-end)
     (niwa-skills-test--with-model-backends
       (let* ((view (generate-new-buffer " *implementation-startup-view*"))
              (policy (copy-tree ,policy))
              (global-budget (default-value 'niwa-goal-token-budget))
              goal)
         (unwind-protect
             (progn
               (niwa-model-set-session-provider
                session (niwa-model-resolve-provider "Fast:fast-model") buffer)
               (niwa-model-set-session-effort session 'high buffer)
               (niwa-permission-mode-set-raw 'ask)
               (setq-local niwa-goal-token-budget 4321
                           niwa-model-workloads nil)
               (let* ((draft (niwa-plan-write-current
                              "# Accepted outcome\nRead evidence and verify the outcome."
                              session buffer))
                      (accepted (niwa-plan-archive-accepted draft session)))
                 (setf (niwa-session-plan-metadata session)
                       (list :status 'accepted
                             :accepted-path (plist-get accepted :path)
                             :accepted-hash (plist-get accepted :hash)))
                 (setq goal (niwa-goal-create
                             "Implement the accepted outcome"
                             :session session :plan-reference (plist-get accepted :path)
                             :implementation-policy policy :token-budget 200)))
               (niwa-view--setup view buffer)
               ,@body
               (should (equal "Fast:fast-model"
                              (niwa-session-model-provider session)))
               (should (eq 'high (niwa-session-reasoning-effort session)))
               (should (eq 'ask (niwa-session-permission-mode session)))
               (should (eq 'fast-model (buffer-local-value 'gptel-model buffer)))
               (should (eq (gptel-get-backend "Fast")
                           (buffer-local-value 'gptel-backend buffer)))
               (should (eq 'high (buffer-local-value 'gptel-reasoning-effort buffer)))
               (should (= 4321 (buffer-local-value 'niwa-goal-token-budget buffer)))
               (should (equal global-budget (default-value 'niwa-goal-token-budget)))
               (should (equal policy (niwa-goal-implementation-policy goal))))
           (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa-view--dispatch-prepared-outcome/implementation-startup
  (:quiet t)
  ,test
  (test)
  :doc "unavailable provider and unsupported effort pause before composer admission"
  (dolist (case '(("Unavailable:model" nil "Unavailable")
                  ("Balanced:balanced-model" impossible "unsupported")))
    (niwa-implementation-startup-test--with-goal
        (list :model-provider (nth 0 case) :reasoning-effort (nth 1 case)
              :permission-mode 'edits)
      (let* ((input "Read the implementation evidence")
             (submission (niwa-prompt-submission-create
                          :input input :display-text input :session session
                          :outcome (list :model-input input :transcript-input input)))
             (before (buffer-string))
             (sent 0)
             diagnostics)
        (with-current-buffer view
          (niwa-view-composer-insert input))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _) (cl-incf sent)))
                  ((symbol-function 'niwa-claude-code-send)
                   (lambda (&rest _) (cl-incf sent))))
          (niwa-test--with-captured-diagnostics diagnostics
            (with-current-buffer view
              (should-not
               (niwa-view--dispatch-prepared-outcome
                submission buffer :submitted-draft input)))))
        (should (= 0 sent))
        (should-not niwa--current-request)
        (should (equal before (buffer-string)))
        (should (eq 'pending (niwa-prompt-submission-state submission)))
        (should (equal input (with-current-buffer view (niwa-view-composer-draft-text))))
        (should (eq 'paused (niwa-goal-status goal)))
        (should (string-match-p (nth 2 case) (niwa-goal-reason goal)))
        (should (string-match-p (regexp-quote (niwa-goal-reason goal)) diagnostics))
        (let* ((saved (read (decode-coding-string
                            (niwa-session-artifacts-read-artifact
                             session "session.meta.el" t)
                            'utf-8-unix)))
               (saved-goal (plist-get saved :goal)))
          (should (eq 'paused (plist-get saved-goal :status)))
          (should (equal (niwa-goal-reason goal) (plist-get saved-goal :reason)))
          (should (equal policy (plist-get saved-goal :implementation-policy)))
          (should (equal "Fast:fast-model" (plist-get saved :model-provider)))
          (should (eq 'high (plist-get saved :reasoning-effort)))
          (should (eq 'ask (plist-get saved :permission-mode)))))))

  :doc "authentication failure after preflight pauses without invoking the provider send"
  (niwa-implementation-startup-test--with-goal
      '(:model-provider "Balanced:balanced-model" :reasoning-effort nil
        :permission-mode edits)
    (let* ((input "Continue implementation")
           (submission (niwa-prompt-submission-create
                        :input input :display-text input :session session
                        :outcome (list :model-input input :transcript-input input)))
           (sent 0))
      (cl-letf (((symbol-function 'niwa-auth-assert-ready)
                 (lambda (_backend) (user-error "Implementation login expired")))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sent))))
        (with-current-buffer view
          (should-not
           (niwa-view--dispatch-prepared-outcome submission buffer))))
      (should (= 0 sent))
      (should-not niwa--current-request)
      (should (eq 'paused (niwa-goal-status goal)))
      (should (equal "Implementation login expired" (niwa-goal-reason goal)))
      (let ((saved (read (decode-coding-string
                         (niwa-session-artifacts-read-artifact
                          session "session.meta.el" t)
                         'utf-8-unix))))
        (should (equal "Implementation login expired"
                       (plist-get (plist-get saved :goal) :reason))))))

  :doc "inactive ordinary composer turns realize baseline model and effort"
  (dolist (status '(paused blocked budget-limited complete))
    (niwa-implementation-startup-test--with-goal
        '(:model-provider "Unavailable:model" :reasoning-effort nil
          :permission-mode edits)
      (setf (niwa-goal-status goal) status
            (niwa-goal-reason goal) (unless (eq status 'complete) "Held"))
      (niwa-session-artifacts-save session buffer)
      (let* ((input "Discuss unrelated work")
             (submission (niwa-prompt-submission-create
                          :input input :display-text input :session session
                          :outcome (list :model-input input :transcript-input input)))
             info)
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _)
                     (let ((gptel-use-tools nil))
                       (setq info
                             (gptel-fsm-info
                              (gptel-request
                               input :system nil :dry-run t
                               :transforms
                               '(niwa-skills--transform-apply-request-model-policy))))))))
          (with-current-buffer view
            (niwa-view--dispatch-prepared-outcome submission buffer)))
        (should info)
        (should (eq 'fast-model (plist-get info :model)))
        (should (eq (gptel-get-backend "Fast") (plist-get info :backend)))
        (should (eq 'high (plist-get info :reasoning-effort)))
        (should (eq status (niwa-goal-status goal)))
        (should (eq 'committed (niwa-prompt-submission-state submission)))
        (should-not niwa--pending-implementation-policy)))))

(niwa-deftest niwa-implementation-policy-pause-failure
  (:quiet t)
  ,test
  (test)
  :doc "concrete root failure durably pauses accepted policy without changing baseline"
  (niwa-implementation-startup-test--with-goal
      '(:model-provider "Balanced:balanced-model" :reasoning-effort nil
        :permission-mode edits)
    (should (eq goal (niwa-implementation-policy-pause-failure
                      buffer '(error "Implementation startup refused"))))
    (should (eq 'paused (niwa-goal-status goal)))
    (should (equal "Implementation startup refused" (niwa-goal-reason goal)))
    (let ((saved (read (decode-coding-string
                       (niwa-session-artifacts-read-artifact
                        session "session.meta.el" t)
                       'utf-8-unix))))
      (should (eq 'paused (plist-get (plist-get saved :goal) :status)))
      (should (equal policy (plist-get (plist-get saved :goal) :implementation-policy)))))

  :doc "ordinary Goals, agent work, inactive Goals and non-owner buffers do nothing"
  (niwa-implementation-startup-test--with-goal
      '(:model-provider "Balanced:balanced-model" :reasoning-effort nil
        :permission-mode edits)
    (let ((publication (niwa-session-publication session)))
      (setf (niwa-goal-implementation-policy goal) nil)
      (should-not (niwa-implementation-policy-pause-failure buffer '(error "Ordinary")))
      (should (eq 'active (niwa-goal-status goal)))
      (setf (niwa-goal-implementation-policy goal) policy)
      (let ((niwa--agent-invocation t))
        (should-not (niwa-implementation-policy-pause-failure buffer '(error "Child"))))
      (with-temp-buffer
        (setq-local niwa--session session)
        (should-not (niwa-implementation-policy-pause-failure
                     (current-buffer) '(error "Non-owner"))))
      (should (eq 'active (niwa-goal-status goal)))
      (dolist (status '(paused blocked budget-limited complete))
        (setf (niwa-goal-status goal) status
              (niwa-goal-reason goal) (unless (eq status 'complete) "Held"))
        (should-not (niwa-implementation-policy-pause-failure buffer '(error "Inactive")))
        (should (eq status (niwa-goal-status goal)))
        (should (equal (unless (eq status 'complete) "Held") (niwa-goal-reason goal))))
      (should (eq publication (niwa-session-publication session))))))

(provide 'test-niwa-implementation-startup)
;;; test-niwa-implementation-startup.el ends here
