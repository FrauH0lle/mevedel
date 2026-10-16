;;; test-mevedel-plan-handoff.el -- Tests for mevedel-plan-handoff.el -*- lexical-binding: t -*-

;;; Commentary:

;; Durable accepted-Plan handoff tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-run)
(require 'mevedel-compact-target)
(require 'mevedel-context-summary)
(require 'mevedel-execution-target)
(require 'mevedel-plan)
(require 'mevedel-plan-handoff)
(require 'mevedel-plan-mode)
(require 'mevedel-goal)
(require 'mevedel-interaction-prompt)
(require 'mevedel-mention-bindings)
(require 'mevedel-permissions)
(require 'mevedel-presets)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-invoke)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-worktree)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-compact)
(require 'mevedel-session-control-transfer)

(defconst test-mevedel-plan-handoff--summary
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

(mevedel-deftest mevedel-plan-handoff-reserved-goal-id
  (:doc "reserves only Here handoffs; a Worktree source hands its Goal to the fork")
  ,test
  (test)
  (let ((here
         (mevedel-session--create
          :name "here"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "here-goal"
             :selection (:location here :execution goal)))))
        (source
         (mevedel-session--create
          :name "source"
          :plan-metadata
          '(:implementation-retry
            (:goal-id "fork-goal"
             :selection (:location worktree :execution goal))))))
    (should (equal "here-goal"
                   (mevedel-plan-handoff-reserved-goal-id here)))
    (should-not (mevedel-plan-handoff-reserved-goal-id source))))

(mevedel-deftest mevedel-plan-handoff--implementation-prompt
  (:doc "derives the logical artifact path and includes the plan body")
  ,test
  (test)
  (let ((prompt
         (mevedel-plan-handoff--implementation-prompt
          '(:path "local/plans/accepted-20260813-120000.md"
            :absolute-path "/tmp/accepted.md") "# Accepted")))
    (should (string-match-p "work://plans/accepted-20260813-120000.md"
                            prompt))
    (should-not (string-match-p "/tmp/accepted.md" prompt))
    (should (string-match-p "# Accepted" prompt))
    (should (string-match-p "Implement the accepted plan" prompt)))

  :doc "appends bound selected skills and preserves bound instruction mentions"
  (let ((instructions "Also use $beta."))
    (mevedel-mention-bindings-set
     9 14
     '(:kind skill :token "$beta"
       :source-file "/tmp/beta/SKILL.md")
     instructions)
    (let* ((prompt
            (mevedel-plan-handoff--implementation-prompt
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
             (mevedel-mention-bindings-ranges prompt))))
      (should (string-match-p "Use \\$alpha during implementation" prompt))
      (should (string-match-p "Also use \\$beta" prompt))
      (should (equal '("/tmp/alpha/SKILL.md"
                       "/tmp/beta/SKILL.md")
                     bindings)))))

(mevedel-deftest mevedel-plan-handoff--goal-kickoff-prompt
  (:doc "includes the artifact and plan before the compact kickoff")
  ,test
  (test)
  (let* ((artifact '(:path "local/plans/accepted-20260813-120000.md"
                     :absolute-path "/tmp/accepted.md"))
         (body "Free-form plan")
         (prompt (mevedel-plan-handoff--goal-kickoff-prompt artifact body)))
    (should (< (string-search "work://plans/accepted-20260813-120000.md"
                              prompt)
               (string-search body prompt)
               (string-search "Begin the active Goal" prompt)))
    (should-not
     (string-match-p "/tmp/accepted.md"
                     prompt))
    (should-not
     (string-match-p "/tmp/accepted.md"
                     mevedel-plan-handoff--accepted-goal-objective))))

(mevedel-deftest mevedel-plan-handoff--implementation-record
  (:doc "chooses the first unfinished preparation step")
  ,test
  (test)
  (let ((accepted '(:path "accepted.md" :hash "h"
                    :absolute-path "/tmp/must-not-persist.md")))
    (should (eq 'submit
                (plist-get
                 (mevedel-plan-handoff--implementation-record
                  '(:location here :context current) accepted)
                 :step)))
    (should (eq 'prepare-context
                (plist-get
                 (mevedel-plan-handoff--implementation-record
                  '(:location here :context fresh) accepted)
                 :step)))
    (should (eq 'prepare-worktree
                (plist-get
                 (mevedel-plan-handoff--implementation-record
                  '(:location worktree :context fresh) accepted)
                 :step)))
    ;; A Worktree implementation always forks first; its context runs there.
    (dolist (context '(current summary))
      (should (eq 'prepare-worktree
                  (plist-get
                   (mevedel-plan-handoff--implementation-record
                    (list :location 'worktree :context context) accepted)
                   :step))))
    (should (eq 'prepare-summary
                (plist-get
                 (mevedel-plan-handoff--implementation-record
                  '(:location here :context summary) accepted)
                 :step)))
    (should-not
     (plist-member
      (plist-get
       (mevedel-plan-handoff--implementation-record
        '(:location here :context current) accepted)
       :accepted)
      :absolute-path)))

  :doc "reserves an identity only for Goal execution"
  (let* ((accepted '(:path "accepted.md" :hash "h"))
         (goal-record
          (mevedel-plan-handoff--implementation-record
           '(:location here :context current :execution goal) accepted))
         (direct-record
          (mevedel-plan-handoff--implementation-record
           '(:location here :context current :execution direct) accepted)))
    (should (stringp (plist-get goal-record :goal-id)))
    (should-not (plist-member direct-record :goal-id))))

(mevedel-deftest mevedel-plan-handoff--summary-focus
  (:doc "keeps the exact accepted plan and implementation-only instructions")
  ,test
  (test)
  (let ((focus
         (mevedel-plan-handoff--summary-focus
          "# Exact plan\nDo it." '(:instructions "Use $tdd exactly."))))
    (should (string-match-p (regexp-quote "# Exact plan\nDo it.") focus))
    (should (string-match-p (regexp-quote "Use $tdd exactly.") focus))))

(mevedel-deftest mevedel-plan-handoff--summary-source
  (:doc "turns prior retained state into evidence and omits the accepted plan")
  ,test
  (test)
  (let ((source
         (mevedel-plan-handoff--summary-source
          "Current evidence.\n# Exact plan"
          "Prior retained state with # Exact plan."
          "# Exact plan")))
    (should (string-match-p "Prior retained state" source))
    (should (string-match-p "accepted-plan omission" source))
    (should-not (string-match-p "# Exact plan" source))))

(mevedel-deftest mevedel-plan-handoff--implementation-failed
  (:quiet t :doc "records a retryable failure and clears preparation progress")
  ,test
  (test)
  (let* ((session
          (mevedel-session--create
           :name "main" :plan-metadata
           '(:implementation-retry (:step submit))))
         (chat-buffer (generate-new-buffer " *mevedel-plan-failed-data*"))
         (view-buffer (generate-new-buffer " *mevedel-plan-failed-view*"))
         stopped)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_) view-buffer))
                  ((symbol-function 'mevedel-view--stop-request-progress)
                   (lambda () (setq stopped t)))
                  ((symbol-function 'mevedel-session-artifacts-save) #'ignore))
          (mevedel-plan-handoff--implementation-failed
           session chat-buffer "Transport failed")
          (should stopped)
          (should (equal "Transport failed"
                         (plist-get
                          (plist-get (mevedel-session-plan-metadata session)
                                     :implementation-retry)
                          :failure)))
          (mevedel-plan-handoff--implementation-failed
           session chat-buffer
           (list 'error (make-string 100000 ?x)))
          (should (<= (length
                       (plist-get
                        (plist-get (mevedel-session-plan-metadata session)
                                   :implementation-retry)
                        :failure))
                      mevedel-plan-handoff--failure-reason-limit)))
      (kill-buffer view-buffer)
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-plan-handoff--implementation-request-started
  (:doc "attaches recovery to the request without clearing retry state")
  ,test
  (test)
  (let* ((retry '(:step submit))
         (session
          (mevedel-session--create
           :name "main" :plan-metadata
           (list :status 'accepted :implementation-retry retry)))
         (chat-buffer (generate-new-buffer " *mevedel-plan-request-source*"))
         (fsm (gptel-make-fsm :info nil)))
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session))
          (should
           (eq fsm
               (mevedel-plan-handoff--implementation-request-started
                fsm chat-buffer)))
          (should
           (eq chat-buffer
               (plist-get
                (gptel-fsm-info fsm)
                :mevedel-plan-handoff-source-buffer)))
          (should
           (eq retry
               (plist-get (mevedel-session-plan-metadata session)
                          :implementation-retry))))
      (kill-buffer chat-buffer)))

  :doc "attaches recovery to an external engine's admitted request"
  (let* ((session (mevedel-session--create :name "main"))
         (chat-buffer (generate-new-buffer " *mevedel-plan-request-source*"))
         (request (mevedel-request--create
                   :session session :context '(:external-history t))))
    (unwind-protect
        (progn
          (should
           (eq request
               (mevedel-plan-handoff--implementation-request-started
                request chat-buffer)))
          (should
           (eq chat-buffer
               (plist-get (mevedel-engine-info request)
                          :mevedel-plan-handoff-source-buffer)))
          (should (plist-get (mevedel-engine-info request)
                             :external-history)))
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-plan-handoff--failure-reason
  (:doc "keeps short strings and describes error values")
  ,test
  (test)
  (should (equal "Transport failed"
                 (mevedel-plan-handoff--failure-reason "Transport failed")))
  (should (equal "Boom"
                 (mevedel-plan-handoff--failure-reason '(error "Boom"))))
  (should (equal ":skip" (mevedel-plan-handoff--failure-reason :skip)))

  :doc "bounds error data that carries a whole request"
  (let* ((session (mevedel-session--create
                   :name (make-string 100000 ?x)
                   :plan-metadata '(:implementation-retry (:step submit))))
         (request (mevedel-request--create :session session))
         (reason (mevedel-plan-handoff--failure-reason
                  (list 'wrong-type-argument 'gptel-fsm request))))
    (should (string-prefix-p "Wrong type argument: gptel-fsm, #s(" reason))
    (should (<= (length reason)
                mevedel-plan-handoff--failure-reason-limit))))

(mevedel-deftest mevedel-plan-handoff-settle-request
  (:quiet t :doc "terminal success clears the attached direct implementation retry")
  ,test
  (test)
  (let* ((retry '(:step submit))
         (session
          (mevedel-session--create
           :name "main" :plan-metadata
           (list :status 'accepted :implementation-retry retry)))
         (chat-buffer (generate-new-buffer " *mevedel-plan-success-source*"))
         (fsm (gptel-make-fsm :info nil))
         persisted)
    (unwind-protect
        (progn
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session))
          (mevedel-plan-handoff--implementation-request-started
           fsm chat-buffer)
          (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                     (lambda (saved-session saved-buffer)
                       (setq persisted (list saved-session saved-buffer)))))
            (mevedel-plan-handoff-settle-request fsm 'success))
          (should (equal (list session chat-buffer) persisted))
          (should-not
           (plist-member (mevedel-session-plan-metadata session)
                         :implementation-retry))
          (should-not
           (plist-member (gptel-fsm-info fsm)
                         :mevedel-plan-handoff-source-buffer)))
      (kill-buffer chat-buffer)))

  :doc "provider failure and abort retain the exact prepared handoff"
  (dolist (case '((error "Provider overloaded")
                  (aborted "Request aborted")))
    (let* ((status (car case))
           (reason (cadr case))
           (retry '(:step submit :selection (:location here)))
           (session
            (mevedel-session--create
             :name "main" :plan-metadata
             (list :status 'accepted :implementation-retry retry)))
           (chat-buffer
            (generate-new-buffer " *mevedel-plan-failure-source*"))
           (fsm (gptel-make-fsm :info nil)))
      (unwind-protect
          (progn
            (with-current-buffer chat-buffer
              (setq-local mevedel--session session))
            (mevedel-plan-handoff--implementation-request-started
             fsm chat-buffer)
            (cl-letf (((symbol-function
                        'mevedel-view--interaction-target-buffer)
                       (lambda (_) nil))
                      ((symbol-function 'mevedel-session-artifacts-save)
                       #'ignore))
              (mevedel-plan-handoff-settle-request fsm status reason))
            (let ((saved
                   (plist-get (mevedel-session-plan-metadata session)
                              :implementation-retry)))
              (should (equal reason (plist-get saved :failure)))
              (should (equal (plist-get retry :selection)
                             (plist-get saved :selection))))
            (should-not
             (plist-member (gptel-fsm-info fsm)
                           :mevedel-plan-handoff-source-buffer)))
        (kill-buffer chat-buffer))))

  :doc "a dead source buffer leaves the durable retry untouched"
  (let* ((retry '(:step submit))
         (session
          (mevedel-session--create
           :name "main" :plan-metadata
           (list :implementation-retry retry)))
         (chat-buffer (generate-new-buffer " *mevedel-plan-dead-source*"))
         (fsm (gptel-make-fsm :info nil)))
    (with-current-buffer chat-buffer
      (setq-local mevedel--session session))
    (mevedel-plan-handoff--implementation-request-started fsm chat-buffer)
    (kill-buffer chat-buffer)
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest warning) (push warning warnings))))
        (mevedel-plan-handoff-settle-request fsm 'success))
      (should warnings))
    (should
     (eq retry
         (plist-get (mevedel-session-plan-metadata session)
                    :implementation-retry)))))

(mevedel-deftest mevedel-plan-handoff--goal-handoff-complete
  (:doc "restores retry state when its durable cleanup fails")
  ,test
  (test)
  (let* ((retry '(:step submit :goal-id "reserved"))
         (metadata (list :status 'accepted :implementation-retry retry))
         (session (mevedel-session--create
                   :name "main" :plan-metadata metadata)))
    (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
               (lambda (&rest _) (error "Disk unavailable"))))
      (should-error
       (mevedel-plan-handoff--goal-handoff-complete session (current-buffer))))
    (should (eq metadata (mevedel-session-plan-metadata session)))
    (should (eq retry
                (plist-get (mevedel-session-plan-metadata session)
                           :implementation-retry)))))

(mevedel-deftest mevedel-plan-handoff--prepare-summary ()
  ,test
  (test)
  :doc "Here uses handoff semantics through aggressive compaction"
  (let* ((save-dir (make-temp-file "mevedel-plan-summary-save-" t))
         (path (file-name-concat save-dir "local" "plans" "accepted.md"))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "source" :save-path save-dir))
         (buffer (generate-new-buffer " *mevedel-plan-summary-here*"))
         captured-source captured-focus applied dispatched)
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Accepted" nil path nil 'silent)
          (cl-letf (((symbol-function 'mevedel-compact-target-main-target)
                     (lambda ()
                       (list :previous-summary "Prior state"
                             :begin-context-epoch t
                             :apply
                             (lambda (_target summary &rest _)
                               (setq applied summary)))))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args)
                       (should (plist-get args :aggressive))
                       (should (eq 'handoff (plist-get args :purpose)))
                       (setq captured-focus (plist-get args :focus))
                       (setq captured-source
                             (funcall (plist-get args :source-transform)
                                      "Current evidence.\n# Accepted"))
                       (let* ((summary
                               (funcall (plist-get args :summary-ready)
                                        test-mevedel-plan-handoff--summary))
                              (target (plist-get args :target)))
                         (funcall (plist-get target :apply) target summary)
                         (funcall (plist-get args :callback) nil))))
                    ((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                    ((symbol-function 'mevedel-plan-handoff--dispatch-accepted)
                     (lambda (&rest _) (setq dispatched t))))
            (mevedel-plan-handoff--prepare-summary
             session buffer
             (list :selection
                   '(:location here :context summary
                   :execution direct :mode ask
                   :instructions "Use $tdd exactly.")
                   :accepted
                   (list :path "local/plans/accepted.md"
                         :absolute-path path
                         :hash (mevedel-plan-hash "# Accepted")))))
          (should dispatched)
          (should (equal test-mevedel-plan-handoff--summary applied))
          (should (string-match-p "Prior state" captured-source))
          (should (string-match-p "accepted-plan omission" captured-source))
          (should-not (string-match-p "# Accepted" captured-source))
          (should (string-match-p "# Accepted" captured-focus))
          (should (string-match-p "Use \\$tdd exactly" captured-focus)))
      (kill-buffer buffer)
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-retry-plan-implementation
  (:doc "redispatches only when an accepted retry record exists")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :name "main" :plan-metadata
          '(:implementation-retry (:step submit))))
        dispatched)
    (cl-letf (((symbol-function 'mevedel-plan-handoff--dispatch-accepted)
               (lambda (retry-session retry-buffer)
                 (setq dispatched (list retry-session retry-buffer)))))
      (mevedel-retry-plan-implementation session (current-buffer)))
    (should (equal (list session (current-buffer)) dispatched))
    (setf (mevedel-session-plan-metadata session) nil)
    (should-error
     (mevedel-retry-plan-implementation session (current-buffer))
     :type 'user-error)))

(mevedel-deftest mevedel-plan-handoff-selection-valid-p
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
    (should (mevedel-plan-handoff-selection-valid-p selection)))
  (dolist (selection
           '((:location elsewhere :context current :execution direct :mode ask)
             (:location here :context current :execution other :mode ask)
             (:location here :context current :execution direct :mode plan)
             (:location here :context current :execution goal :mode ask
                        :goal-token-budget 0)
             (:location here :context current :execution goal :mode ask
                        :goal-token-budget "1000")))
    (should-not (mevedel-plan-handoff-selection-valid-p selection)))

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
    (should-not (mevedel-plan-handoff-selection-valid-p selection))))

(mevedel-deftest mevedel-plan-handoff--validate-record
  (:doc "accepts only relative durable plan artifacts with hashes")
  ,test
  (test)
  (let ((record
         '(:step submit
           :selection (:location here :context current :execution direct
                       :mode ask :model-provider "OpenAI:gpt-5")
           :accepted (:path "plans/accepted.md" :hash "h"))))
    (should (equal (plist-get record :selection)
                   (mevedel-plan-handoff--validate-record record)))
    (setf (plist-get (plist-get record :accepted) :absolute-path)
          "/tmp/old-shape.md")
    (should-error (mevedel-plan-handoff--validate-record record)))
  (should-error
   (mevedel-plan-handoff--validate-record
    '(:step submit
      :selection (:location here :context current :execution direct
                  :mode ask :model-provider "OpenAI:gpt-5")
      :accepted (:path "../escape.md" :hash "h")))))

(mevedel-deftest mevedel-plan-handoff--dispatch-submission
  (:doc "releases preparation progress for Direct/Goal without losing drafts")
  ,test
  (test)
  (dolist (execution '(direct goal))
    (progn
      (dolist (owner '(plan-preparation another-request))
        (let* ((data (generate-new-buffer " *handoff-progress-data*"))
               (view (generate-new-buffer " *handoff-progress-view*"))
               (session (mevedel-session--create :name "handoff"))
               (goal (and (eq execution 'goal)
                          (mevedel-goal--create
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
                  (setq-local mevedel--session nil)
                  (setq-local mevedel--current-request nil))
                (mevedel-view--setup view data)
                (set-window-buffer (selected-window) view)
                (with-current-buffer view
                  (goto-char (mevedel-view--input-start))
                  (insert draft)
                  (goto-char (+ (mevedel-view--input-start) 3))
                  (mevedel-view--update-spinner "Preparing implementation..." owner)
                  (should (timerp mevedel-view--spinner-timer)))
                (setf (mevedel-session-goal session) goal)
                (cl-letf (((symbol-function 'mevedel-plan-handoff--start-goal)
                           (lambda (&rest _) goal))
                          ((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                          ((symbol-function 'mevedel--implement-plan)
                           (lambda (action)
                             (should (eq (current-buffer) data))
                             (should (eq goal (plist-get action :implementation-goal)))
                             (should (equal (and goal "reserved")
                                            (plist-get action :implementation-goal-id)))
                             (with-current-buffer view
                               (should (eq mevedel-view--spinner-owner
                                           (unless (eq owner 'plan-preparation)
                                             owner)))
                               (when (eq owner 'plan-preparation)
                                 (should-not mevedel-view--spinner-timer))
                               (should (equal draft (mevedel-view--input-text)))
                               (should (= (point) (+ (mevedel-view--input-start) 3))))
                             (setq dispatched t)
                             fsm)))
                  (mevedel-plan-handoff--dispatch-submission
                   session data '(:goal-id "reserved") (list :execution execution :mode 'edits
                                          :model-provider "Test:model")
                   nil "Implement"
                   (mevedel-prompt-submission-create
                    :input "Implement" :outcome '(:model-input "Implement"))))
                (should dispatched)
                (when (eq execution 'direct)
                  (should (eq data (plist-get
                                    (gptel-fsm-info fsm)
                                    :mevedel-plan-handoff-source-buffer)))))
            (with-current-buffer view (mevedel-view-stream-stop))
            (kill-buffer view)
            (kill-buffer data)))))))

(mevedel-deftest mevedel-plan-handoff--submit
  (:doc "routes the generated handoff through planned skill submission")
  ,test
  (test)
  (let* ((session
          (mevedel-session--create
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
            (setq-local mevedel--session session))
          (cl-letf
              (((symbol-function 'mevedel-plan-read-artifact)
                (lambda (_session _artifact) "# Accepted"))
               ((symbol-function 'mevedel-view--interaction-target-buffer)
                (lambda (_) view-buffer))
               ((symbol-function 'mevedel-skills-input-prepare-user-input)
                (lambda (input _) input))
               ((symbol-function
                 'mevedel-plan-handoff-validate-skill-bindings)
                #'ignore)
               ((symbol-function 'mevedel-view--submit-planned-input)
                (lambda (input _before _blocked dispatch &optional _after)
                  (setq planned input)
                  (funcall
                   dispatch
                   (mevedel-prompt-submission-create
                    :input input :outcome '(:model-input "expanded")))))
               ((symbol-function
                 'mevedel-plan-handoff--dispatch-submission)
                (lambda (&rest args) (setq dispatched args))))
            (mevedel-plan-handoff--submit
             session data-buffer record))
          (should (string-match-p "# Accepted" planned))
          (should (string-match-p "Use \\$alpha" planned))
          (should dispatched))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer))))

(mevedel-deftest mevedel-plan-handoff-validate-skill-bindings
  (:doc "accepts available canonical skills and rejects missing targets")
  ,test
  (test)
  (let* ((source "/tmp/alpha/SKILL.md")
         (skill
          (mevedel-skill--create
           :name "alpha" :source-file source
           :user-invocable-p t :active-p t))
         (session
          (mevedel-session--create :name "main" :skills (list skill)))
         (prompt "Use $alpha"))
    (mevedel-mention-bindings-set
     4 10
     (list :kind 'skill :token "$alpha" :source-file source)
     prompt)
    (should-not
     (mevedel-plan-handoff-validate-skill-bindings prompt session))
    (setf (mevedel-session-skills session) nil)
    (should-error
     (mevedel-plan-handoff-validate-skill-bindings prompt session))))

(provide 'test-mevedel-plan-handoff)
;;; test-mevedel-plan-handoff.el ends here
