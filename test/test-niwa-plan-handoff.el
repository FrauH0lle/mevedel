;;; test-niwa-plan-handoff.el -- Tests for niwa-plan-handoff.el -*- lexical-binding: t -*-

;;; Commentary:

;; Durable accepted-Plan handoff tests.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))

(require 'gptel-request)
(require 'niwa-compact-evidence)
(require 'niwa-compact-run)
(require 'niwa-compact-target)
(require 'niwa-context-summary)
(require 'niwa-execution-target)
(require 'niwa-plan)
(require 'niwa-plan-handoff)
(require 'niwa-plan-mode)
(require 'niwa-goal)
(require 'niwa-interaction-prompt)
(require 'niwa-mention-bindings)
(require 'niwa-permissions)
(require 'niwa-presets)
(require 'niwa-prompt-submission)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-skills-invoke)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)
(require 'niwa-worktree)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-compact)
(require 'niwa-session-control-transfer)

(defconst test-niwa-plan-handoff--summary
  "## Scope
- background
## Constraints & Preferences
- constraint
## Work & Evidence
- evidence
## Key Decisions
- decision
## Open Questions & Risks
- risk
## Critical Context
- context
## Relevant Files
- file.el
## Skills Invoked
- (none)"
  "Valid handoff summary used by Plan handoff tests.")

(defun niwa-plan-handoff-test--publish (session path body)
  "Commit fixture BODY at PATH with SESSION's genuine sidecar."
  (niwa-session-publication-publish
   session
   (list (list :path path :content body)
         (list :path (file-name-concat (niwa-session-save-path session) "session.meta.el")
               :content (niwa-session-artifacts-printed-value
                         (niwa-session-codec-serialize session))
               :commit-marker t))))

(niwa-deftest niwa-plan-handoff-reserved-goal-id
  (:doc "reserves only Here handoffs; a Worktree source hands its Goal to the fork")
  ,test
  (test)
  (let ((here
         (niwa-session--create
          :name "here"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "here-goal"
             :selection (:location here :execution goal)))))
        (source
         (niwa-session--create
          :name "source"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "fork-goal"
             :selection (:location worktree :execution goal))))))
    (should (equal "here-goal"
                   (niwa-plan-handoff-reserved-goal-id here)))
    (should-not (niwa-plan-handoff-reserved-goal-id source))))

(niwa-deftest niwa-plan-handoff--implementation-prompt
  (:doc "derives the logical artifact path and includes the plan body")
  ,test
  (test)
  (let ((prompt
         (niwa-plan-handoff--implementation-prompt
          '(:path "local/plans/accepted-20260813-120000.md"
            :absolute-path "/tmp/accepted.md") "# Accepted")))
    (should (string-match-p "work://plans/accepted-20260813-120000.md"
                            prompt))
    (should-not (string-match-p "/tmp/accepted.md" prompt))
    (should (string-match-p "# Accepted" prompt))
    (should (string-match-p "Implement the accepted plan" prompt)))

  :doc "appends bound selected skills and preserves bound instruction mentions"
  (let ((instructions "Also use $beta."))
    (niwa-mention-bindings-set
     9 14
     '(:kind skill :token "$beta"
       :source-file "/tmp/beta/SKILL.md")
     instructions)
    (let* ((prompt
            (niwa-plan-handoff--implementation-prompt
             '(:path "local/plans/accepted-20260813-120000.md"
                  :absolute-path "/tmp/accepted.md") "# Accepted"
             (list
              :skills
              '((:name "alpha"
                 :source-file "/tmp/alpha/SKILL.md"))
              :instructions instructions)))
           (bindings
            (mapcar
             (lambda (range)
               (plist-get (plist-get range :binding) :source-file))
             (niwa-mention-bindings-ranges prompt))))
      (should (string-match-p "Use \\$alpha during implementation" prompt))
      (should (string-match-p "Also use \\$beta" prompt))
      (should (equal '("/tmp/alpha/SKILL.md"
                       "/tmp/beta/SKILL.md")
                     bindings)))))

(niwa-deftest niwa-plan-handoff--goal-kickoff-prompt
  (:doc "includes the artifact and plan before the compact kickoff")
  ,test
  (test)
  (let* ((artifact '(:path "local/plans/accepted-20260813-120000.md"
                     :absolute-path "/tmp/accepted.md"))
         (body "Free-form plan")
         (prompt (niwa-plan-handoff--goal-kickoff-prompt artifact body)))
    (should (< (string-search "work://plans/accepted-20260813-120000.md"
                              prompt)
               (string-search body prompt)
               (string-search "Begin the active Goal" prompt)))
    (should-not
     (string-match-p "/tmp/accepted.md"
                     prompt))
    (should-not
     (string-match-p "/tmp/accepted.md"
                     niwa-plan-handoff--accepted-goal-objective))))

(niwa-deftest niwa-plan-handoff--implementation-record
  (:doc "chooses the first unfinished preparation step")
  ,test
  (test)
  (let ((accepted '(:path "accepted.md" :hash "h"
                    :absolute-path "/tmp/must-not-persist.md")))
    (should (eq 'submit
                (plist-get
                 (niwa-plan-handoff--implementation-record
                  '(:location here :context current) accepted)
                 :step)))
    (should (eq 'prepare-context
                (plist-get
                 (niwa-plan-handoff--implementation-record
                  '(:location here :context fresh) accepted)
                 :step)))
    (should (eq 'prepare-worktree
                (plist-get
                 (niwa-plan-handoff--implementation-record
                  '(:location worktree :context fresh) accepted)
                 :step)))
    ;; A Worktree implementation always forks first; its context runs there.
    (dolist (context '(current summary))
      (should (eq 'prepare-worktree
                  (plist-get
                   (niwa-plan-handoff--implementation-record
                    (list :location 'worktree :context context) accepted)
                   :step))))
    (should (eq 'prepare-summary
                (plist-get
                 (niwa-plan-handoff--implementation-record
                  '(:location here :context summary) accepted)
                 :step)))
    (should-not
     (plist-member
      (plist-get
       (niwa-plan-handoff--implementation-record
        '(:location here :context current) accepted)
       :accepted)
      :absolute-path)))

  :doc "reserves an identity only for Goal execution"
  (let* ((accepted '(:path "accepted.md" :hash "h"))
         (goal-record
          (niwa-plan-handoff--implementation-record
           '(:location here :context current :execution goal) accepted))
         (direct-record
          (niwa-plan-handoff--implementation-record
           '(:location here :context current :execution direct) accepted)))
    (should (stringp (plist-get goal-record :goal-id)))
    (should-not (plist-member direct-record :goal-id))))

(niwa-deftest niwa-plan-handoff--summary-focus
  (:doc "keeps the exact accepted plan and implementation-only instructions")
  ,test
  (test)
  (let ((focus
         (niwa-plan-handoff--summary-focus
          "# Exact plan\nDo it." '(:instructions "Use $tdd exactly."))))
    (should (string-match-p (regexp-quote "# Exact plan\nDo it.") focus))
    (should (string-match-p (regexp-quote "Use $tdd exactly.") focus))))

(niwa-deftest niwa-plan-handoff--summary-source
  (:doc "turns prior retained state into evidence and omits the accepted plan")
  ,test
  (test)
  (let ((source
         (niwa-plan-handoff--summary-source
          "Current evidence.\n# Exact plan"
          "Prior retained state with # Exact plan."
          "# Exact plan")))
    (should (string-match-p "Prior retained state" source))
    (should (string-match-p "accepted-plan omission" source))
    (should-not (string-match-p "# Exact plan" source))))

(niwa-deftest niwa-plan-handoff--implementation-failed
  (:quiet t :doc "records a retryable failure and clears preparation progress")
  ,test
  (test)
  (let* ((session
          (niwa-session--create
           :name "main" :plan-metadata
           '(:implementation-retry (:step submit))))
         (chat-buffer (generate-new-buffer " *niwa-plan-failed-data*"))
         (view-buffer (generate-new-buffer " *niwa-plan-failed-view*"))
         stopped)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-view--interaction-target-buffer)
                   (lambda (_) view-buffer))
                  ((symbol-function 'niwa-view--stop-request-progress)
                   (lambda () (setq stopped t)))
                  ((symbol-function 'niwa-session-artifacts-save) #'ignore))
          (niwa-plan-handoff--implementation-failed
           session chat-buffer "Transport failed")
          (should stopped)
          (should (equal "Transport failed"
                         (plist-get
                          (plist-get (niwa-session-plan-metadata session)
                                     :implementation-retry)
                          :failure)))
          (niwa-plan-handoff--implementation-failed
           session chat-buffer
           (list 'error (make-string 100000 ?x)))
          (should (<= (length
                       (plist-get
                        (plist-get (niwa-session-plan-metadata session)
                                   :implementation-retry)
                        :failure))
                      niwa-plan-handoff--failure-reason-limit)))
      (kill-buffer view-buffer)
      (kill-buffer chat-buffer))))

(niwa-deftest niwa-plan-handoff--implementation-request-started
  (:doc "attaches recovery to the request without clearing retry state")
  ,test
  (test)
  (let* ((retry '(:step submit))
         (session
          (niwa-session--create
           :name "main" :plan-metadata
           (list :status 'accepted :implementation-retry retry)))
         (chat-buffer (generate-new-buffer " *niwa-plan-request-source*"))
         (fsm (gptel-make-fsm :info nil)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local niwa--session session))
          (should
           (eq fsm
               (niwa-plan-handoff--implementation-request-started
                fsm chat-buffer)))
          (should
           (eq chat-buffer
               (plist-get
                (gptel-fsm-info fsm)
                :niwa-plan-handoff-source-buffer)))
          (should
           (eq retry
               (plist-get (niwa-session-plan-metadata session)
                          :implementation-retry))))
      (kill-buffer chat-buffer)))

  :doc "attaches recovery to an external engine's admitted request"
  (let* ((session (niwa-session--create :name "main"))
         (chat-buffer (generate-new-buffer " *niwa-plan-request-source*"))
         (request (niwa-request--create
                   :session session :context '(:external-history t))))
    (unwind-protect
        (progn
          (should
           (eq request
               (niwa-plan-handoff--implementation-request-started
                request chat-buffer)))
          (should
           (eq chat-buffer
               (plist-get (niwa-engine-info request)
                          :niwa-plan-handoff-source-buffer)))
          (should (plist-get (niwa-engine-info request)
                             :external-history)))
      (kill-buffer chat-buffer))))

(niwa-deftest niwa-plan-handoff--failure-reason
  (:doc "keeps short strings and describes error values")
  ,test
  (test)
  (should (equal "Transport failed"
                 (niwa-plan-handoff--failure-reason "Transport failed")))
  (should (equal "Boom"
                 (niwa-plan-handoff--failure-reason '(error "Boom"))))
  (should (equal ":skip" (niwa-plan-handoff--failure-reason :skip)))

  :doc "bounds error data that carries a whole request"
  (let* ((session (niwa-session--create
                   :name (make-string 100000 ?x)
                   :plan-metadata '(:implementation-retry (:step submit))))
         (request (niwa-request--create :session session))
         (reason (niwa-plan-handoff--failure-reason
                  (list 'wrong-type-argument 'gptel-fsm request))))
    (should (string-prefix-p "Wrong type argument: gptel-fsm, #s(" reason))
    (should (<= (length reason)
                niwa-plan-handoff--failure-reason-limit))))

(niwa-deftest niwa-plan-handoff-settle-request
  (:quiet t :doc "terminal success clears the attached direct implementation retry")
  ,test
  (test)
  (let* ((retry '(:step submit))
         (session
          (niwa-session--create
           :name "main" :plan-metadata
           (list :status 'accepted :implementation-retry retry)))
         (chat-buffer (generate-new-buffer " *niwa-plan-success-source*"))
         (fsm (gptel-make-fsm :info nil))
         persisted)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local niwa--session session))
          (niwa-plan-handoff--implementation-request-started
           fsm chat-buffer)
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (saved-session saved-buffer)
                       (setq persisted (list saved-session saved-buffer)))))
            (niwa-plan-handoff-settle-request fsm 'success))
          (should (equal (list session chat-buffer) persisted))
          (should-not
           (plist-member (niwa-session-plan-metadata session)
                         :implementation-retry))
          (should-not
           (plist-member (gptel-fsm-info fsm)
                         :niwa-plan-handoff-source-buffer)))
      (kill-buffer chat-buffer)))

  :doc "provider failure and abort retain the exact prepared handoff"
  (dolist (case '((error "Provider overloaded")
                  (aborted "Request aborted")))
    (let* ((status (car case))
           (reason (cadr case))
           (retry '(:step submit :selection (:location here)))
           (session
            (niwa-session--create
             :name "main" :plan-metadata
             (list :status 'accepted :implementation-retry retry)))
           (chat-buffer
            (generate-new-buffer " *niwa-plan-failure-source*"))
           (fsm (gptel-make-fsm :info nil)))
      (unwind-protect
          (progn
            (with-current-buffer chat-buffer
              (setq-local niwa--session session))
            (niwa-plan-handoff--implementation-request-started
             fsm chat-buffer)
            (cl-letf (((symbol-function
                        'niwa-view--interaction-target-buffer)
                       (lambda (_) nil))
                      ((symbol-function 'niwa-session-artifacts-save)
                       #'ignore))
              (niwa-plan-handoff-settle-request fsm status reason))
            (let ((saved
                   (plist-get (niwa-session-plan-metadata session)
                              :implementation-retry)))
              (should (equal reason (plist-get saved :failure)))
              (should (equal (plist-get retry :selection)
                             (plist-get saved :selection))))
            (should-not
             (plist-member (gptel-fsm-info fsm)
                           :niwa-plan-handoff-source-buffer)))
        (kill-buffer chat-buffer))))

  :doc "a dead source buffer leaves the durable retry untouched"
  (let* ((retry '(:step submit))
         (session
          (niwa-session--create
           :name "main"
           :workspace (niwa-workspace--create :root "/tmp/origin/worktree")
           :plan-metadata
           (list :implementation-retry retry)))
         (chat-buffer (generate-new-buffer " *niwa-plan-dead-source*"))
         (fsm (gptel-make-fsm :info nil)))
    (with-current-buffer chat-buffer
      (setq-local niwa--session session))
    (niwa-plan-handoff--implementation-request-started fsm chat-buffer)
    (kill-buffer chat-buffer)
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest warning) (push warning warnings))))
        (niwa-plan-handoff-settle-request fsm 'success))
      (should (string-prefix-p "niwa [/tmp/origin/worktree]: "
                               (cadar warnings))))
    (should
     (eq retry
         (plist-get (niwa-session-plan-metadata session)
                    :implementation-retry)))))

(niwa-deftest niwa-plan-handoff--goal-handoff-complete
  (:doc "restores retry state when its durable cleanup fails")
  ,test
  (test)
  (let* ((retry '(:step submit :goal-id "reserved"))
         (metadata (list :status 'accepted :implementation-retry retry))
         (session (niwa-session--create
                   :name "main" :plan-metadata metadata)))
    (cl-letf (((symbol-function 'niwa-session-artifacts-save)
               (lambda (&rest _) (error "Disk unavailable"))))
      (should-error
       (niwa-plan-handoff--goal-handoff-complete session (current-buffer))))
    (should (eq metadata (niwa-session-plan-metadata session)))
    (should (eq retry
                (plist-get (niwa-session-plan-metadata session)
                           :implementation-retry)))))

(niwa-deftest niwa-plan-handoff--prepare-summary ()
  ,test
  (test)
  :doc "Here uses handoff semantics through aggressive compaction"
  (let* ((save-dir (make-temp-file "niwa-plan-summary-save-" t))
         (path (file-name-concat save-dir "local" "plans" "accepted.md"))
         (session (test-niwa-session-persistence--leased-session save-dir
                   :name "source" ))
         (buffer (generate-new-buffer " *niwa-plan-summary-here*"))
         captured-source captured-focus applied dispatched)
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (niwa-plan-handoff-test--publish session path "# Accepted")
          (cl-letf (((symbol-function 'niwa-compact-target-main-target)
                     (lambda ()
                       (list :previous-summary "Prior state"
                             :begin-context-epoch t
                             :apply
                             (lambda (_target summary &rest _)
                               (setq applied summary)))))
                    ((symbol-function 'niwa-compact-run-start)
                     (lambda (&rest args)
                       (should (plist-get args :aggressive))
                       (should (eq 'handoff (plist-get args :purpose)))
                       (setq captured-focus (plist-get args :focus))
                       (setq captured-source
                             (funcall (plist-get args :source-transform)
                                      "Current evidence.\n# Accepted"))
                       (let* ((summary
                               (funcall (plist-get args :summary-ready)
                                        test-niwa-plan-handoff--summary))
                              (target (plist-get args :target)))
                         (funcall (plist-get target :apply) target summary)
                         (funcall (plist-get args :callback) nil))))
                    ((symbol-function 'niwa-session-artifacts-save) #'ignore)
                    ((symbol-function 'niwa-plan-handoff--dispatch-accepted)
                     (lambda (&rest _) (setq dispatched t))))
            (niwa-plan-handoff--prepare-summary
             session buffer
             (list :selection
                   '(:location here :context summary
                   :execution direct :mode ask
                   :instructions "Use $tdd exactly.")
                   :accepted
                   (list :path "local/plans/accepted.md"
                         :absolute-path path
                         :hash (niwa-plan-hash "# Accepted")))))
          (should dispatched)
          (should (equal test-niwa-plan-handoff--summary applied))
          (should (string-match-p "Prior state" captured-source))
          (should (string-match-p "accepted-plan omission" captured-source))
          (should-not (string-match-p "# Accepted" captured-source))
          (should (string-match-p "# Accepted" captured-focus))
          (should (string-match-p "Use \\$tdd exactly" captured-focus)))
      (kill-buffer buffer)
      (niwa-session-persistence-lock-release save-dir session)
      (niwa-workspace-clear-registry)
      (delete-directory save-dir t))))

(niwa-deftest niwa-retry-plan-implementation
  (:doc "redispatches only when an accepted retry record exists")
  ,test
  (test)
  (let ((session
         (niwa-session--create
          :name "main" :plan-metadata
          '(:implementation-retry (:step submit))))
        dispatched)
    (cl-letf (((symbol-function 'niwa-plan-handoff--dispatch-accepted)
               (lambda (retry-session retry-buffer)
                 (setq dispatched (list retry-session retry-buffer)))))
      (niwa-retry-plan-implementation session (current-buffer)))
    (should (equal (list session (current-buffer)) dispatched))
    (setf (niwa-session-plan-metadata session) nil)
    (setf (niwa-session-workspace session)
          (niwa-workspace--create :root "/tmp/explicit/worktree"))
    (let ((failure
           (should-error
            (niwa-retry-plan-implementation session (current-buffer))
            :type 'user-error)))
      (should (equal (cadr failure)
                     "niwa [/tmp/explicit/worktree]: No accepted plan implementation to retry")))))

(niwa-deftest niwa-plan-handoff-selection-valid-p
  (:doc "accepts exactly the supported location/context/mode matrix")
  ,test
  (test)
  (dolist (selection
           '((:location here :context current :execution direct :mode ask)
             (:location here :context fresh :execution direct :mode edits)
             (:location worktree :context summary :execution direct
                        :mode full-auto
                        :skills ((:name "alpha"
                                  :source-file "/tmp/alpha/SKILL.md"))
                        :instructions "Run tests"
                        :model-provider "OpenAI:gpt-5"
                        :reasoning-effort high)
             (:location here :context current :execution goal :mode ask
                        :goal-token-budget 1000)
             (:location here :context fresh :execution goal :mode ask)
             (:location here :context summary :execution goal :mode ask)
             (:location worktree :context fresh :execution goal :mode ask)
             (:location worktree :context summary :execution goal :mode ask)
             (:location worktree :context current :execution direct :mode ask)))
    (should (niwa-plan-handoff-selection-valid-p selection)))
  (dolist (selection
           '((:location elsewhere :context current :execution direct :mode ask)
             (:location here :context current :execution other :mode ask)
             (:location here :context current :execution direct :mode plan)
             (:location here :context current :execution goal :mode ask
                        :goal-token-budget 0)
             (:location here :context current :execution goal :mode ask
                        :goal-token-budget "1000")))
    (should-not (niwa-plan-handoff-selection-valid-p selection)))

  :doc "rejects malformed implementation extras"
  (dolist
      (selection
       '((:location here :context current :execution direct :mode ask
                    :skills ((:name "alpha" :source-file "relative")))
         (:location here :context current :execution direct :mode ask
                    :instructions 42)
         (:location here :context current :execution direct :mode ask
                    :model-provider openai)
         (:location here :context current :execution direct :mode ask
                    :reasoning-effort "high")))
    (should-not (niwa-plan-handoff-selection-valid-p selection))))

(niwa-deftest niwa-plan-handoff--validate-record
  (:doc "accepts only relative durable plan artifacts with hashes")
  ,test
  (test)
  (let ((record
         '(:step submit
           :selection (:location here :context current :execution direct
                       :mode ask :model-provider "OpenAI:gpt-5")
           :accepted (:path "plans/accepted.md" :hash "h"))))
    (should (equal (plist-get record :selection)
                   (niwa-plan-handoff--validate-record record)))
    (setf (plist-get (plist-get record :accepted) :absolute-path)
          "/tmp/old-shape.md")
    (should-error (niwa-plan-handoff--validate-record record)))
  (should-error
   (niwa-plan-handoff--validate-record
    '(:step submit
      :selection (:location here :context current :execution direct
                  :mode ask :model-provider "OpenAI:gpt-5")
      :accepted (:path "../escape.md" :hash "h")))))

(niwa-deftest niwa-plan-handoff--dispatch-submission
  (:doc "releases preparation progress for Direct/Goal without losing drafts")
  ,test
  (test)
  (dolist (execution '(direct goal))
    (progn
      (dolist (owner '(plan-preparation another-request))
        (let* ((data (generate-new-buffer " *handoff-progress-data*"))
               (view (generate-new-buffer " *handoff-progress-view*"))
               (session (niwa-session--create :name "handoff"))
               (goal (and (eq execution 'goal)
                          (niwa-goal--create
                           :id "reserved" :status 'active
                           :implementation-policy
                           '(:model-provider "Test:model" :reasoning-effort nil
                             :permission-mode edits))))
               (fsm (gptel-make-fsm :info nil))
               (draft "> quoted\nsecond line")
               dispatched)
          (unwind-protect
              (save-window-excursion
                (with-current-buffer data
                  (setq-local niwa--session nil)
                  (setq-local niwa--current-request nil))
                (niwa-view--setup view data)
                (set-window-buffer (selected-window) view)
                (should (niwa-view-composer-display (selected-window)))
                (redisplay t)
                (with-current-buffer view
                  (niwa-view-test--insert-composer-draft draft 3)
                  (niwa-view--update-spinner "Preparing implementation..." owner)
                  (should (timerp niwa-view--spinner-timer)))
                (setf (niwa-session-goal session) goal)
                (cl-letf (((symbol-function 'niwa-plan-handoff--start-goal)
                           (lambda (&rest _) goal))
                          ((symbol-function 'niwa-session-artifacts-save) #'ignore)
                          ((symbol-function 'niwa--implement-plan)
                           (lambda (action)
                             (should (eq (current-buffer) data))
                             (should (eq goal (plist-get action :implementation-goal)))
                             (should (equal (and goal "reserved")
                                            (plist-get action :implementation-goal-id)))
                             (with-current-buffer view
                               (should (eq niwa-view--spinner-owner
                                           (unless (eq owner 'plan-preparation)
                                             owner)))
                               (when (eq owner 'plan-preparation)
                                 (should-not niwa-view--spinner-timer))
                               (should (equal draft (niwa-view-composer-draft-text)))
                               (should (= 3 (niwa-view-test--composer-point-offset))))
                             (setq dispatched t)
                             fsm)))
                  (niwa-plan-handoff--dispatch-submission
                   session data '(:goal-id "reserved") (list :execution execution :mode 'edits
                                          :model-provider "Test:model")
                   nil "Implement"
                   (niwa-prompt-submission-create
                    :input "Implement" :outcome '(:model-input "Implement"))))
                (should dispatched)
                (when (eq execution 'direct)
                  (should (eq data (plist-get
                                    (gptel-fsm-info fsm)
                                    :niwa-plan-handoff-source-buffer)))))
            (with-current-buffer view (niwa-view-stream-stop))
            (kill-buffer view)
            (kill-buffer data)))))))

(niwa-deftest niwa-plan-handoff--submit
  (:doc "routes the generated handoff through planned skill submission")
  ,test
  (test)
  (let* ((session
          (niwa-session--create
           :name "main" :save-path "/tmp/plan-submit-session/"))
         (data-buffer (generate-new-buffer " *plan-submit-data*"))
         (view-buffer (generate-new-buffer " *plan-submit-view*"))
         (selection
          '(:location here :context current :execution direct :mode edits
            :model-provider "OpenAI:gpt-5" :reasoning-effort high
            :instructions "Use $alpha"))
         (record
          (list :selection selection
                :accepted '(:path "local/plans/accepted-20260813-120000.md"
                            :absolute-path "/tmp/accepted.md" :hash "h")))
         planned dispatched)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf
              (((symbol-function 'niwa-plan-read-artifact)
                (lambda (_session _artifact) "# Accepted"))
               ((symbol-function 'niwa-view--interaction-target-buffer)
                (lambda (_) view-buffer))
               ((symbol-function 'niwa-skills-input-prepare-user-input)
                (lambda (input _) input))
               ((symbol-function
                 'niwa-plan-handoff-validate-skill-bindings)
                #'ignore)
               ((symbol-function 'niwa-view--submit-planned-input)
                (lambda (input _before _blocked dispatch &optional _after)
                  (setq planned input)
                  (funcall
                   dispatch
                   (niwa-prompt-submission-create
                    :input input :outcome '(:model-input "expanded")))))
               ((symbol-function
                 'niwa-plan-handoff--dispatch-submission)
                (lambda (&rest args) (setq dispatched args))))
            (niwa-plan-handoff--submit
             session data-buffer record))
          (should (string-match-p "# Accepted" planned))
          (should (string-match-p "Use \\$alpha" planned))
          (should dispatched))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer))))

(niwa-deftest niwa-plan-handoff-validate-skill-bindings
  (:doc "accepts available canonical skills and rejects missing targets")
  ,test
  (test)
  (let* ((source "/tmp/alpha/SKILL.md")
         (skill
          (niwa-skill--create
           :name "alpha" :source-file source
           :user-invocable-p t :active-p t))
         (session
          (niwa-session--create :name "main" :skills (list skill)))
         (prompt "Use $alpha"))
    (niwa-mention-bindings-set
     4 10
     (list :kind 'skill :token "$alpha" :source-file source)
     prompt)
    (should-not
     (niwa-plan-handoff-validate-skill-bindings prompt session))
    (setf (niwa-session-skills session) nil)
    (should-error
     (niwa-plan-handoff-validate-skill-bindings prompt session))))

(provide 'test-niwa-plan-handoff)
;;; test-niwa-plan-handoff.el ends here
