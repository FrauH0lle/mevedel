;;; test-mevedel-directive-plan.el --- Directive planning tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the Plan-before-implementation workflow owned by directives.

;;; Code:

(require 'mevedel)
(require 'mevedel-session-persistence)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-directive-plan--planning-prompt ()
  ,test
  (test)

  :doc "carries the exact implementation request into initial planning"
  (let ((prompt
         (mevedel-directive-plan--planning-prompt "Implement exact request")))
    (should (string-search "do not modify the project" prompt))
    (should (string-search "Implement exact request" prompt)))

  :doc "requests a complete replacement from feedback and prior proposal"
  (let ((prompt
         (mevedel-directive-plan--planning-prompt
          "Implement exact request" "Cover failure recovery" "# Old plan")))
    (should (string-search "Cover failure recovery" prompt))
    (should (string-search "replace it completely" prompt))
    (should (string-search "# Old plan" prompt))))

(mevedel-deftest mevedel-directive-plan--planning-model-policy ()
  ,test
  (test)

  :doc "prefers the directive model when explicitly configured"
  (cl-letf (((symbol-function 'mevedel--directive-model-policy)
             (lambda (_) '(:model explicit)))
            ((symbol-function 'mevedel-model-resolve-workload)
             (lambda (_) (ert-fail "planning workload should not resolve"))))
    (should (equal '(:model explicit)
                   (mevedel-directive-plan--planning-model-policy 'directive))))

  :doc "uses the planning workload without a directive model"
  (cl-letf (((symbol-function 'mevedel--directive-model-policy) #'ignore)
            ((symbol-function 'mevedel-model-resolve-workload)
             (lambda (workload)
               (should (eq 'planning workload))
               '(:model planner))))
    (should (equal '(:model planner)
                   (mevedel-directive-plan--planning-model-policy 'directive)))))

(mevedel-deftest mevedel-directive-plan--selection ()
  ,test
  (test)
  :doc "seeds a fresh default selection with directive-selected skills"
  (let ((record (mevedel-directive--create
                 :id "directive" :request "Request"
                 :skills '((:name "alpha"
                            :source-file "/tmp/alpha/SKILL.md"))))
        (chat-buffer (generate-new-buffer " *plan-selection*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-plan-mode--default-selection)
                   (lambda (_)
                     (list :location 'here :skills
                           '((:name "alpha"
                              :source-file "/tmp/alpha/SKILL.md"))))))
          (let ((plan (list :chat-buffer chat-buffer)))
            ;; Seeding deduplicates against the default by source file.
            (should
             (equal '((:name "alpha" :source-file "/tmp/alpha/SKILL.md"))
                    (plist-get
                     (mevedel-directive-plan--selection nil plan record)
                     :skills)))))
      (kill-buffer chat-buffer)))

  :doc "a retained card selection stays authoritative over record skills"
  (let ((record (mevedel-directive--create
                 :id "directive" :request "Request"
                 :skills '((:name "alpha"
                            :source-file "/tmp/alpha/SKILL.md"))))
        (plan (list :chat-buffer nil
                    :selection '(:location here :skills nil))))
    (should-not
     (plist-get (mevedel-directive-plan--selection nil plan record)
                :skills))))

(mevedel-deftest mevedel-directive-plan--approval-outcome ()
  ,test
  (test)
  :doc "settles the caller with an error when the proposal is invalidated"
  (let* ((attempt (gensym "attempt-"))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :plan (list :status 'proposed :action 'implement
                              :attempt attempt)))
         (session (mevedel-session--create
                   :name "main"
                   :directive-planning
                   (list :directive-id "directive" :attempt attempt)))
         (directive (make-overlay (point-min) (point-min)))
         settled)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-directive-plan--persist)
                   #'ignore)
                  ((symbol-function 'mevedel-directive-plan--refresh)
                   #'ignore)
                  ((symbol-function
                    'mevedel-directive-plan--restore-chat-scope)
                   #'ignore))
          (mevedel-directive-plan--approval-outcome
           directive record session
           (lambda (err fsm) (setq settled (list :err err :fsm fsm)))
           attempt
           'invalidated)
          (should (stringp (plist-get settled :err))))
      (delete-overlay directive)))

  :doc "rejects a captured Accept after a replacement attempt starts"
  (let* ((old-attempt (gensym "old-attempt-"))
         (new-attempt (gensym "new-attempt-"))
         (new-owner (list :directive-id "directive" :phase 'approval
                          :attempt new-attempt))
         (record (mevedel-directive--create
                  :id "directive" :request "Edited"
                  :plan (list :status 'proposed :action 'implement
                              :attempt new-attempt)))
         (session (mevedel-session--create
                   :name "main" :directive-planning new-owner))
         (directive (make-overlay (point-min) (point-min)))
         settled)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-directive-plan--implement)
                   (lambda (&rest _)
                     (ert-fail "Invalidated plan reached implementation")))
                  ((symbol-function 'mevedel-directive-plan--persist) #'ignore)
                  ((symbol-function 'mevedel-directive-plan--refresh) #'ignore)
                  ((symbol-function
                    'mevedel-directive-plan--restore-chat-scope)
                   #'ignore))
          (mevedel-directive-plan--approval-outcome
           directive record session
           (lambda (err fsm) (setq settled (list :err err :fsm fsm)))
           old-attempt
           '(:accept t :selection (:mode edits)))
          (should-not settled)
          (should (eq new-owner
                      (mevedel-session-directive-planning session))))
      (delete-overlay directive))))

(mevedel-deftest mevedel-directive-plan--settle-planning
  (:quiet t)
  ,test
  (test)

  :doc "does not revive a proposal invalidated by an owner or child edit"
  (dolist (edit '(owner child))
    (mevedel-view-test--with-buffers
      (let* ((draft ">first editable character\nsecond line")
             (proposal "<proposed_plan>\n# Stale\n</proposed_plan>")
             (attempt (gensym "attempt-"))
             (session (mevedel-session--create :name "main"))
             (record
              (mevedel-directive--create
               :id "directive" :request "Original"
               :plan (list :status 'planning :action 'implement
                           :attempt attempt)
               :planning (list (list :result proposal))))
             (directive (make-overlay (point-min) (point-min)))
             (fsm (gptel-make-fsm :info (list :buffer data-buf)))
             presented implemented settled (drains 0))
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local mevedel--session session))
              (with-current-buffer view-buf
                (setq-local mevedel--session session)
                (goto-char (mevedel-view--input-start))
                (insert draft)
                (mevedel-view--switch-composer-scope
                 (list :directive-id "directive" :action 'plan
                       :record record)))
              (setf (mevedel-session-directive-planning session)
                    (list :directive-id "directive" :phase 'planning
                          :attempt attempt))
              (if (eq edit 'owner)
                  (mevedel-directive-set-request record "Edited")
                (mevedel-directive-add-subdirective
                 record (mevedel-subdirective--create
                         :id "child" :request "New detail")))
              (cl-letf
                  (((symbol-function 'mevedel-directive-plan--refresh) #'ignore)
                   ((symbol-function 'mevedel-plan-approval-present)
                    (lambda (&rest _) (setq presented t)))
                   ((symbol-function 'mevedel-directive-plan--implement)
                    (lambda (&rest _) (setq implemented t)))
                   ((symbol-function
                     'mevedel-view--schedule-late-follow-up-drain)
                    (lambda () (cl-incf drains))))
                (mevedel-directive-plan--settle-planning
                 directive record
                 (lambda (err terminal-fsm)
                   (setq settled (list err terminal-fsm)))
                 nil fsm attempt session))
              (should-not presented)
              (should-not implemented)
              (should (string-match-p "invalidated" (car settled)))
              (should (eq fsm (cadr settled)))
              (should-not (mevedel-session-directive-planning session))
              (should-not (mevedel-session-pending-plan-approval session))
              (should (= 1 drains))
              (with-current-buffer view-buf
                (should-not mevedel-view--composer-scope)
                (should
                 (equal draft
                        (buffer-substring-no-properties
                         (mevedel-view--input-start) (point-max))))))
          (delete-overlay directive)))))

  :doc "releases failed, aborted, missing, and dead planning terminals"
  (dolist (terminal '(error abort missing dead-buffer))
    (mevedel-view-test--with-buffers
      (let* ((draft ">first editable character\nsecond line")
             (proposal "<proposed_plan>\n# Plan\n</proposed_plan>")
             (attempt (gensym "attempt-"))
             (record
              (mevedel-directive--create
               :id "directive" :session-id "session" :request "Original"
               :plan (list :status 'planning :action 'implement
                           :attempt attempt)
               :planning
               (list (list :result (if (eq terminal 'missing)
                                       "No proposal"
                                     proposal)))))
             (workspace
              (mevedel-workspace--create
               :type 'file :id "terminal" :root "/tmp" :name "terminal"
               :directives (list record)))
             (session (mevedel-session--create
                       :name "main" :session-id "session"
                       :workspace workspace))
             (directive (make-overlay (point-min) (point-min)))
             (fsm (gptel-make-fsm :info (list :buffer data-buf)))
             settled (drains 0))
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local mevedel--session session))
              (with-current-buffer view-buf
                (setq-local mevedel--session session)
                (goto-char (mevedel-view--input-start))
                (insert draft)
                (mevedel-view--switch-composer-scope
                 (list :directive-id "directive" :action 'plan
                       :record record)))
              (setf (mevedel-session-directive-planning session)
                    (list :directive-id "directive" :phase 'planning
                          :attempt attempt))
              (when (eq terminal 'dead-buffer)
                (kill-buffer data-buf))
              (cl-letf
                  (((symbol-function 'mevedel-directive-plan--refresh) #'ignore)
                   ((symbol-function 'mevedel-plan-approval-present)
                    (lambda (&rest _)
                      (ert-fail "Non-presentable terminal reached approval")))
                   ((symbol-function
                     'mevedel-view--schedule-late-follow-up-drain)
                    (lambda () (cl-incf drains))))
                (mevedel-directive-plan--settle-planning
                 directive record
                 (lambda (err terminal-fsm)
                   (setq settled (list err terminal-fsm)))
                 (pcase terminal
                   ('error "Provider failed")
                   ('abort 'abort))
                 fsm attempt session))
              (should (car settled))
              (should (eq fsm (cadr settled)))
              (should-not (mevedel-session-directive-planning session))
              (should-not (mevedel-session-pending-plan-approval session))
              (should (plist-get (mevedel-directive-plan record) :cancelled))
              (should-not
               (mevedel-directive-plan-restore-pending
                session (if (buffer-live-p data-buf)
                            data-buf
                          (current-buffer))))
              (should-not (mevedel-session-directive-planning session))
              (should (= (if (eq terminal 'dead-buffer) 0 1) drains))
              (when (buffer-live-p view-buf)
                (with-current-buffer view-buf
                  (should-not mevedel-view--composer-scope)
                  (should
                   (equal draft
                          (buffer-substring-no-properties
                           (mevedel-view--input-start) (point-max)))))))
          (delete-overlay directive)))))

  :doc "leaves a replacement attempt owned when an old callback settles"
  (mevedel-view-test--with-buffers
    (let* ((draft ">first editable character\nsecond line")
           (old-attempt (gensym "old-attempt-"))
           (new-attempt (gensym "new-attempt-"))
           (proposal "<proposed_plan>\n# Old\n</proposed_plan>")
           (new-plan (list :status 'planning :action 'implement
                           :attempt new-attempt))
           (new-owner (list :directive-id "directive" :phase 'planning
                            :attempt new-attempt))
           (session (mevedel-session--create
                     :name "main" :directive-planning new-owner))
           (turn (list :result proposal))
           (record (mevedel-directive--create
                    :id "directive" :request "Replacement"
                    :plan new-plan :planning (list turn)))
           (directive (make-overlay (point-min) (point-min)))
           (fsm (gptel-make-fsm :info (list :buffer data-buf)))
           settled (drains 0))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (goto-char (mevedel-view--input-start))
              (insert draft)
              (mevedel-view--switch-composer-scope
               (list :directive-id "directive" :action 'plan
                     :record record)))
            (cl-letf
                (((symbol-function 'mevedel-directive-plan--refresh) #'ignore)
                 ((symbol-function 'mevedel-plan-approval-present)
                  (lambda (&rest _)
                    (ert-fail "Old planning attempt reached approval")))
                 ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
                  (lambda () (cl-incf drains))))
              (mevedel-directive-plan--settle-planning
               directive record
               (lambda (err terminal-fsm)
                 (setq settled (list err terminal-fsm)))
               nil fsm old-attempt session))
            (should (string-match-p "no longer current" (car settled)))
            (should (eq fsm (cadr settled)))
            (should (eq new-plan (mevedel-directive-plan record)))
            (should-not (plist-member turn :proposal))
            (should (eq new-owner
                        (mevedel-session-directive-planning session)))
            (should (= 0 drains))
            (with-current-buffer view-buf
              (should mevedel-view--composer-scope)
              (mevedel-view-back-to-chat)
              (should
               (equal draft
                      (buffer-substring-no-properties
                       (mevedel-view--input-start) (point-max))))))
        (delete-overlay directive)))))

(mevedel-deftest mevedel-directive-plan-continue ()
  ,test
  (test)

  :doc "rejects cancelled or invalidated retained drafts"
  (let ((record
         (mevedel-directive--create
          :plan '(:status draft :action implement :invalidated t))))
    (cl-letf (((symbol-function 'mevedel--directive-record)
               (lambda (_) record)))
      (should-error
       (mevedel-directive-plan-continue 'directive "Continue")
       :type 'user-error))
    (setf (mevedel-directive-plan record)
          '(:status draft :action implement :cancelled t))
    (cl-letf (((symbol-function 'mevedel--directive-record)
               (lambda (_) record)))
      (should-error
       (mevedel-directive-plan-continue 'directive "Continue")
       :type 'user-error))))

(mevedel-deftest mevedel-directive-plan-restore-pending ()
  ,test
  (test)

  :doc "does not reactivate cancelled or invalidated drafts on resume"
  (let* ((cancelled
          (mevedel-directive--create
           :id "cancelled" :session-id "session"
           :plan '(:status draft :action implement :cancelled t)))
         (invalidated
          (mevedel-directive--create
           :id "invalidated" :session-id "session"
           :plan '(:status draft :action implement :invalidated t)))
         (workspace
          (mevedel-workspace--create
           :type 'file :id "restore" :root "/tmp" :name "restore"
           :directives (list cancelled invalidated)))
         (session
          (mevedel-session--create
           :session-id "session" :workspace workspace)))
    (should-not
     (mevedel-directive-plan-restore-pending session (current-buffer)))
    (should-not (mevedel-session-directive-planning session))))

(mevedel-deftest mevedel-directive-plan-start
  (:vars ((mevedel-action-preset-alist
           '((discuss . (:system "plan"))
             (implement . (:system "implement"))))))
  ,test
  (test)

  :doc "moves one opted-in directive from planning proposal to implementation"
  (let* ((chat-buffer (generate-new-buffer " *directive-plan-chat*"))
         (session (mevedel-session--create
                   :name "main" :permission-mode 'ask))
         (record (mevedel-directive--create
                  :id "directive-1" :request "Change it"
                  :planning-enabled t))
         (directive (make-overlay (point-min) (point-min)))
         (mevedel-directive-plan-test-observations
          (vector 0 nil nil nil nil))
         final-callback-ran)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session))
          (cl-letf
              (((symbol-function 'mevedel--directive-record)
                (lambda (_) record))
               ((symbol-function 'mevedel--update-instruction-overlay)
                #'ignore)
               ((symbol-function 'mevedel-plan-mode--default-selection)
                (lambda (_)
                  '(:location here :context current :execution direct
                    :mode ask :model-provider "Test:model"
                    :reasoning-effort high :goal-token-budget nil
                    :skills nil :instructions nil)))
               ((symbol-function 'mevedel-model-resolve-provider)
                (lambda (&rest _) '(:backend backend :model model)))
               ((symbol-function 'mevedel-model-validate-effort)
                (lambda (_model effort) effort))
               ((symbol-function 'mevedel-skills-input-prepare-user-input)
                (lambda (input _) input))
               ((symbol-function
                 'mevedel-plan-handoff-validate-skill-bindings)
                #'ignore)
               ((symbol-function 'mevedel-directive-plan--persist)
                #'ignore)
               ((symbol-function 'mevedel-view-enter-directive-scope)
                #'ignore)
               ((symbol-function 'mevedel-plan-approval-present)
                (lambda (entry owner)
                  (should (eq owner session))
                  (aset mevedel-directive-plan-test-observations 1 entry)))
               ((symbol-function 'mevedel--process-directive)
                (lambda (_directive preset prompt-fn callback &optional options)
                  (aset mevedel-directive-plan-test-observations 0
                        (1+ (aref mevedel-directive-plan-test-observations 0)))
                  (if (<= (aref mevedel-directive-plan-test-observations 0) 2)
                      (let* ((planning-prompt (funcall prompt-fn "Source context"))
                             (proposal
                              (concat
                               "<proposed_plan>\n# Plan\n\n"
                               "## Summary\n- Do it.\n\n"
                               "## Key Changes\n- Change it.\n\n"
                               "## Regression Coverage\n- Test it.\n\n"
                               "## Validation\n- Run tests.\n\n"
                               "## Assumptions\n- None.\n"
                               "</proposed_plan>"))
                             (fsm (gptel-make-fsm)))
                        (should (equal '(:system "plan") preset))
                        (should (string-search "Source context" planning-prompt))
                        (setf (mevedel-directive-planning record)
                              (append
                               (mevedel-directive-planning record)
                               (list
                                (list
                                 :sequence
                                 (aref mevedel-directive-plan-test-observations
                                       0)
                                 :action 'implement
                                 :directive-request "Change it"
                                 :request planning-prompt
                                 :result proposal :outcome 'success
                                 :checkpoint
                                 (list
                                  :session-id "session"
                                  :turn
                                  (aref
                                   mevedel-directive-plan-test-observations
                                   0))))))
                        (setf (gptel-fsm-info fsm)
                              (list :buffer chat-buffer))
                        (funcall callback nil fsm)
                        fsm)
                    (aset mevedel-directive-plan-test-observations 2
                          (funcall prompt-fn "ignored"))
                    (aset mevedel-directive-plan-test-observations 3 options)
                    (aset mevedel-directive-plan-test-observations 4 callback)
                    (should (stringp
                             (aref mevedel-directive-plan-test-observations 2)))
                    (should
                     (string-search
                      "Exact request: Source context"
                      (aref mevedel-directive-plan-test-observations 2)))
                    (should
                     (string-search
                      "### ACCEPTED DIRECTIVE PLAN"
                      (aref mevedel-directive-plan-test-observations 2)))
                    (should (eq 'ask (plist-get options :permission-mode)))
                    (should
                     (eq 'high
                         (plist-get (plist-get options :model-policy)
                                    :effort)))
                    (should
                     (equal
                      "# Plan\n\n## Summary\n- Do it.\n\n## Key Changes\n- Change it.\n\n## Regression Coverage\n- Test it.\n\n## Validation\n- Run tests.\n\n## Assumptions\n- None."
                      (plist-get options :plan)))
                    (should (equal '(:system "implement") preset))
                    (gptel-make-fsm)))))
            (mevedel-directive-plan-start
             directive 'implement
             (lambda (content) (concat "Exact request: " content))
             (lambda (&rest _) (setq final-callback-ran t)))
            (should (eq 'proposed
                        (plist-get (mevedel-directive-plan record) :status)))
            (should (plist-get
                     (aref mevedel-directive-plan-test-observations 1)
                     :directive))
            (funcall
             (plist-get (aref mevedel-directive-plan-test-observations 1)
                        :callback)
             'feedback-draft)
            (mevedel-directive-plan-continue directive "Revise it")
            (funcall
             (plist-get (aref mevedel-directive-plan-test-observations 1)
                        :callback)
             (list :accept t
                   :selection
                   (plist-get
                    (aref mevedel-directive-plan-test-observations 1)
                    :selection)))
            (should (= (aref mevedel-directive-plan-test-observations 0) 3))
            (should (eq 'implementation
                        (plist-get
                         (mevedel-session-directive-planning session) :phase)))
            (let ((fsm (gptel-make-fsm)))
              (setf (gptel-fsm-info fsm) (list :buffer chat-buffer))
              (funcall
               (aref mevedel-directive-plan-test-observations 4) nil fsm))
            (should final-callback-ran)))
      (delete-overlay directive)
      (kill-buffer chat-buffer)))

  :doc "releases the captured owner after the planning chat buffer dies"
  (let* ((chat-buffer (generate-new-buffer " *directive-plan-dead-chat*"))
         (session (mevedel-session--create :name "main"))
         (record (mevedel-directive--create
                  :id "directive" :request "Change it"))
         (directive (make-overlay (point-min) (point-min)))
         callback fsm settled)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session))
          (cl-letf
              (((symbol-function 'mevedel--directive-record)
                (lambda (_) record))
               ((symbol-function 'mevedel-directive-plan--planning-model-policy)
                #'ignore)
               ((symbol-function 'mevedel-directive-plan--refresh) #'ignore)
               ((symbol-function 'mevedel-plan-approval-present)
                (lambda (&rest _)
                  (ert-fail "Dead planning chat reached approval")))
               ((symbol-function 'mevedel--process-directive)
                (lambda (_directive _preset prompt-fn terminal
                         &optional _options)
                  (funcall prompt-fn "Source context")
                  (setf (mevedel-directive-planning record)
                        (list
                         (list :result
                               "<proposed_plan>\n# Plan\n</proposed_plan>"))
                        (mevedel-session-directive-planning session)
                        '(:directive-id "directive" :phase planning))
                  (setq callback terminal
                        fsm (gptel-make-fsm
                             :info (list :buffer chat-buffer)))
                  fsm)))
            (mevedel-directive-plan-start
             directive 'implement #'identity
             (lambda (err terminal-fsm)
               (setq settled (list err terminal-fsm))))
            (kill-buffer chat-buffer)
            (funcall callback nil fsm))
          (should (car settled))
          (should (eq fsm (cadr settled)))
          (should-not (mevedel-session-directive-planning session)))
      (delete-overlay directive)
      (when (buffer-live-p chat-buffer) (kill-buffer chat-buffer)))))

(provide 'test-mevedel-directive-plan)
;;; test-mevedel-directive-plan.el ends here
