;;; test-mevedel-implementation-startup.el -- Implementation startup regressions -*- lexical-binding: t -*-

;;; Commentary:

;; Production composer admission and durable Goal failure handling with real
;; published sessions.  Only the provider send/authentication boundary is faked.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-implementation-policy)
(require 'mevedel-goal)
(require 'mevedel-plan)
(require 'mevedel-view-composer)

(defmacro mevedel-implementation-startup-test--with-goal (policy &rest body)
  "Run BODY with POLICY on a real accepted-plan Goal and composer view."
  (declare (indent 1) (debug (form body)))
  `(mevedel-engine-test--with-session
     (mevedel-request-end)
     (mevedel-skills-test--with-model-backends
       (let* ((view (generate-new-buffer " *implementation-startup-view*"))
              (policy (copy-tree ,policy))
              (global-budget (default-value 'mevedel-goal-token-budget))
              goal)
         (unwind-protect
             (progn
               (mevedel-model-set-session-provider
                session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
               (mevedel-model-set-session-effort session 'high buffer)
               (mevedel-permission-mode-set-raw 'ask)
               (setq-local mevedel-goal-token-budget 4321
                           mevedel-model-workloads nil)
               (let* ((draft (mevedel-plan-write-current
                              "# Accepted outcome\nRead evidence and verify the outcome."
                              session buffer))
                      (accepted (mevedel-plan-archive-accepted draft session)))
                 (setf (mevedel-session-plan-metadata session)
                       (list :status 'accepted
                             :accepted-path (plist-get accepted :path)
                             :accepted-hash (plist-get accepted :hash)))
                 (setq goal (mevedel-goal-create
                             "Implement the accepted outcome"
                             :session session :plan-reference (plist-get accepted :path)
                             :implementation-policy policy :token-budget 200)))
               (mevedel-view--setup view buffer)
               ,@body
               (should (equal "Fast:fast-model"
                              (mevedel-session-model-provider session)))
               (should (eq 'high (mevedel-session-reasoning-effort session)))
               (should (eq 'ask (mevedel-session-permission-mode session)))
               (should (eq 'fast-model (buffer-local-value 'gptel-model buffer)))
               (should (eq (gptel-get-backend "Fast")
                           (buffer-local-value 'gptel-backend buffer)))
               (should (eq 'high (buffer-local-value 'gptel-reasoning-effort buffer)))
               (should (= 4321 (buffer-local-value 'mevedel-goal-token-budget buffer)))
               (should (equal global-budget (default-value 'mevedel-goal-token-budget)))
               (should (equal policy (mevedel-goal-implementation-policy goal))))
           (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel-view--dispatch-prepared-outcome/implementation-startup
  (:quiet t)
  ,test
  (test)
  :doc "unavailable provider and unsupported effort pause before composer admission"
  (dolist (case '(("Unavailable:model" nil "Unavailable")
                  ("Balanced:balanced-model" impossible "unsupported")))
    (mevedel-implementation-startup-test--with-goal
        (list :model-provider (nth 0 case) :reasoning-effort (nth 1 case)
              :permission-mode 'edits)
      (let* ((input "Read the implementation evidence")
             (submission (mevedel-prompt-submission-create
                          :input input :display-text input :session session
                          :outcome (list :model-input input :transcript-input input)))
             (before (buffer-string))
             (sent 0)
             diagnostics)
        (with-current-buffer view
          (goto-char (mevedel-view--input-start))
          (insert input))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _) (cl-incf sent)))
                  ((symbol-function 'mevedel-claude-code-send)
                   (lambda (&rest _) (cl-incf sent))))
          (mevedel-test--with-captured-diagnostics diagnostics
            (with-current-buffer view
              (should-not
               (mevedel-view--dispatch-prepared-outcome
                submission buffer :submitted-draft input)))))
        (should (= 0 sent))
        (should-not mevedel--current-request)
        (should (equal before (buffer-string)))
        (should (eq 'pending (mevedel-prompt-submission-state submission)))
        (should (equal input (with-current-buffer view (mevedel-view--input-text))))
        (should (eq 'paused (mevedel-goal-status goal)))
        (should (string-match-p (nth 2 case) (mevedel-goal-reason goal)))
        (should (string-match-p (regexp-quote (mevedel-goal-reason goal)) diagnostics))
        (let* ((saved (read (decode-coding-string
                            (mevedel-session-artifacts-read-artifact
                             session "session.meta.el" t)
                            'utf-8-unix)))
               (saved-goal (plist-get saved :goal)))
          (should (eq 'paused (plist-get saved-goal :status)))
          (should (equal (mevedel-goal-reason goal) (plist-get saved-goal :reason)))
          (should (equal policy (plist-get saved-goal :implementation-policy)))
          (should (equal "Fast:fast-model" (plist-get saved :model-provider)))
          (should (eq 'high (plist-get saved :reasoning-effort)))
          (should (eq 'ask (plist-get saved :permission-mode)))))))

  :doc "authentication failure after preflight pauses without invoking the provider send"
  (mevedel-implementation-startup-test--with-goal
      '(:model-provider "Balanced:balanced-model" :reasoning-effort nil
        :permission-mode edits)
    (let* ((input "Continue implementation")
           (submission (mevedel-prompt-submission-create
                        :input input :display-text input :session session
                        :outcome (list :model-input input :transcript-input input)))
           (sent 0))
      (cl-letf (((symbol-function 'mevedel-auth-assert-ready)
                 (lambda (_backend) (user-error "Implementation login expired")))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sent))))
        (with-current-buffer view
          (should-not
           (mevedel-view--dispatch-prepared-outcome submission buffer))))
      (should (= 0 sent))
      (should-not mevedel--current-request)
      (should (eq 'paused (mevedel-goal-status goal)))
      (should (equal "Implementation login expired" (mevedel-goal-reason goal)))
      (let ((saved (read (decode-coding-string
                         (mevedel-session-artifacts-read-artifact
                          session "session.meta.el" t)
                         'utf-8-unix))))
        (should (equal "Implementation login expired"
                       (plist-get (plist-get saved :goal) :reason))))))

  :doc "inactive ordinary composer turns realize baseline model and effort"
  (dolist (status '(paused blocked budget-limited complete))
    (mevedel-implementation-startup-test--with-goal
        '(:model-provider "Unavailable:model" :reasoning-effort nil
          :permission-mode edits)
      (setf (mevedel-goal-status goal) status
            (mevedel-goal-reason goal) (unless (eq status 'complete) "Held"))
      (mevedel-session-artifacts-save session buffer)
      (let* ((input "Discuss unrelated work")
             (submission (mevedel-prompt-submission-create
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
                               '(mevedel-skills--transform-apply-request-model-policy))))))))
          (with-current-buffer view
            (mevedel-view--dispatch-prepared-outcome submission buffer)))
        (should info)
        (should (eq 'fast-model (plist-get info :model)))
        (should (eq (gptel-get-backend "Fast") (plist-get info :backend)))
        (should (eq 'high (plist-get info :reasoning-effort)))
        (should (eq status (mevedel-goal-status goal)))
        (should (eq 'committed (mevedel-prompt-submission-state submission)))
        (should-not mevedel--pending-implementation-policy)))))

(mevedel-deftest mevedel-implementation-policy-pause-failure
  (:quiet t)
  ,test
  (test)
  :doc "concrete root failure durably pauses accepted policy without changing baseline"
  (mevedel-implementation-startup-test--with-goal
      '(:model-provider "Balanced:balanced-model" :reasoning-effort nil
        :permission-mode edits)
    (should (eq goal (mevedel-implementation-policy-pause-failure
                      buffer '(error "Implementation startup refused"))))
    (should (eq 'paused (mevedel-goal-status goal)))
    (should (equal "Implementation startup refused" (mevedel-goal-reason goal)))
    (let ((saved (read (decode-coding-string
                       (mevedel-session-artifacts-read-artifact
                        session "session.meta.el" t)
                       'utf-8-unix))))
      (should (eq 'paused (plist-get (plist-get saved :goal) :status)))
      (should (equal policy (plist-get (plist-get saved :goal) :implementation-policy)))))

  :doc "ordinary Goals, agent/directive work, inactive Goals and non-owner buffers do nothing"
  (mevedel-implementation-startup-test--with-goal
      '(:model-provider "Balanced:balanced-model" :reasoning-effort nil
        :permission-mode edits)
    (let ((publication (mevedel-session-publication session)))
      (setf (mevedel-goal-implementation-policy goal) nil)
      (should-not (mevedel-implementation-policy-pause-failure buffer '(error "Ordinary")))
      (should (eq 'active (mevedel-goal-status goal)))
      (setf (mevedel-goal-implementation-policy goal) policy)
      (let ((mevedel--agent-invocation t))
        (should-not (mevedel-implementation-policy-pause-failure buffer '(error "Child"))))
      (let ((mevedel--current-directive-uuid "directive"))
        (should-not (mevedel-implementation-policy-pause-failure buffer '(error "Directive"))))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (should-not (mevedel-implementation-policy-pause-failure
                     (current-buffer) '(error "Non-owner"))))
      (should (eq 'active (mevedel-goal-status goal)))
      (dolist (status '(paused blocked budget-limited complete))
        (setf (mevedel-goal-status goal) status
              (mevedel-goal-reason goal) (unless (eq status 'complete) "Held"))
        (should-not (mevedel-implementation-policy-pause-failure buffer '(error "Inactive")))
        (should (eq status (mevedel-goal-status goal)))
        (should (equal (unless (eq status 'complete) "Held") (mevedel-goal-reason goal))))
      (should (eq publication (mevedel-session-publication session))))))

(provide 'test-mevedel-implementation-startup)
;;; test-mevedel-implementation-startup.el ends here
