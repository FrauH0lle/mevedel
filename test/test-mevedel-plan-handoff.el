;;; test-mevedel-plan-handoff.el -- Tests for mevedel-plan-handoff.el -*- lexical-binding: t -*-

;;; Commentary:

;; Durable accepted-Plan handoff tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel-context-summary)
(require 'mevedel-plan)
(require 'mevedel-plan-handoff)
(require 'mevedel-plan-mode)
(require 'mevedel-goal)
(require 'mevedel-interaction-prompt)
(require 'mevedel-mention-bindings)
(require 'mevedel-permissions)
(require 'mevedel-presets)
(require 'mevedel-prompt-submission)
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
  (:doc "reserves Here sources and Worktree targets without blocking sources")
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
            (:goal-id "target-goal"
             :selection (:location worktree :execution goal)))))
        (target
         (mevedel-session--create
          :name "target"
          :plan-metadata '(:implementation-goal-id "target-goal"))))
    (should (equal "here-goal"
                   (mevedel-plan-handoff-reserved-goal-id here)))
    (should-not (mevedel-plan-handoff-reserved-goal-id source))
    (should (equal "target-goal"
                   (mevedel-plan-handoff-reserved-goal-id target)))))

(mevedel-deftest mevedel-plan-handoff--implementation-prompt
  (:doc "includes the immutable artifact, plan body, and implementation order")
  ,test
  (test)
  (let ((prompt
         (mevedel-plan-handoff--implementation-prompt
          '(:absolute-path "/tmp/accepted.md") "# Accepted")))
    (should (string-match-p "/tmp/accepted.md" prompt))
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
             '(:absolute-path "/tmp/accepted.md") "# Accepted"
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
  (let* ((artifact '(:absolute-path "/tmp/accepted.md"))
         (body "Free-form plan")
         (prompt (mevedel-plan-handoff--goal-kickoff-prompt artifact body)))
    (should (< (string-search "/tmp/accepted.md" prompt)
               (string-search body prompt)
               (string-search "Begin the active Goal" prompt)))
    (should-not
     (string-match-p "/tmp/accepted.md"
                     mevedel-plan-handoff--accepted-goal-objective))))

(mevedel-deftest mevedel-plan-handoff--implementation-record
  (:doc "chooses the first unfinished preparation step")
  ,test
  (test)
  (let ((accepted '(:path "accepted.md" :absolute-path "/tmp/accepted.md"
                    :hash "h")))
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
    (should (eq 'prepare-summary
                (plist-get
                 (mevedel-plan-handoff--implementation-record
                  '(:location worktree :context summary) accepted)
                 :step))))

  :doc "reserves an identity only for Goal execution"
  (let* ((accepted '(:path "accepted.md" :absolute-path "/tmp/accepted.md"
                     :hash "h"))
         (goal-record
          (mevedel-plan-handoff--implementation-record
           '(:location here :context current :execution goal) accepted))
         (direct-record
          (mevedel-plan-handoff--implementation-record
           '(:location here :context current :execution direct) accepted)))
    (should (stringp (plist-get goal-record :goal-id)))
    (should-not (plist-member direct-record :goal-id))))

(mevedel-deftest mevedel-plan-handoff--accepted-body
  (:doc "reads a matching immutable artifact and rejects a bad hash")
  ,test
  (test)
  (let ((path (make-temp-file "mevedel-plan-accepted-")))
    (unwind-protect
        (progn
          (write-region "# Accepted" nil path nil 'silent)
          (should (equal "# Accepted"
                         (mevedel-plan-handoff--accepted-body
                          (list :absolute-path path
                                :hash
                                (mevedel-plan-hash "# Accepted")))))
          (should-error
           (mevedel-plan-handoff--accepted-body
            (list :absolute-path path :hash "wrong"))))
      (delete-file path))))

  :doc "resolves the accepted artifact from its relative path after a rename"
  (let* ((old-dir (make-temp-file "mevedel-plan-old-save-" t))
         (new-dir (make-temp-file "mevedel-plan-new-save-" t))
         (relative "local/plans/accepted-1.md")
         (old-path (file-name-concat old-dir relative))
         (new-path (file-name-concat new-dir relative))
         (body "# Accepted")
         (session (mevedel-session--create :name "renamed"
                                           :save-path new-dir))
         (artifact (list :path relative :absolute-path old-path
                         :hash (mevedel-plan-hash body))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory old-path) t)
          (make-directory (file-name-directory new-path) t)
          (write-region body nil old-path nil 'silent)
          (copy-file old-path new-path)
          (should (equal body
                         (mevedel-plan-handoff--accepted-body
                          artifact session))))
      (delete-directory old-dir t)
      (delete-directory new-dir t)))

  :doc "rejects a relative artifact path outside managed plan storage"
  (let* ((save-dir (make-temp-file "mevedel-plan-unsafe-save-" t))
         (outside (make-temp-file "mevedel-plan-unsafe-artifact-"))
         (body "# Outside")
         (session (mevedel-session--create :name "unsafe"
                                           :save-path save-dir))
         (artifact (list :path "../outside.md"
                         :absolute-path outside
                         :hash (mevedel-plan-hash body))))
    (unwind-protect
        (progn
          (write-region body nil outside nil 'silent)
          (should-error
           (mevedel-plan-handoff--accepted-body artifact session))
          (should-error
           (mevedel-plan-handoff--accepted-body
            (list :absolute-path outside
                  :hash (mevedel-plan-hash body))
            session)))
      (delete-directory save-dir t)
      (delete-file outside)))

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
  (:doc "records a retryable failure and clears preparation progress")
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
                  ((symbol-function 'mevedel-plan-handoff--persist) #'ignore))
          (mevedel-plan-handoff--implementation-failed
           session chat-buffer "Transport failed")
          (should stopped)
          (should (equal "Transport failed"
                         (plist-get
                          (plist-get (mevedel-session-plan-metadata session)
                                     :implementation-retry)
                          :failure))))
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
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-plan-handoff-settle-request
  (:doc "terminal success clears the attached direct implementation retry")
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
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist)
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
      (kill-buffer chat-buffer))))

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
                      ((symbol-function 'mevedel-plan-handoff--persist)
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
                    :implementation-retry))))

(mevedel-deftest mevedel-plan-handoff--goal-handoff-complete
  (:doc "restores retry state when its durable cleanup fails")
  ,test
  (test)
  (let* ((retry '(:step submit :goal-id "reserved"))
         (metadata (list :status 'accepted :implementation-retry retry))
         (session (mevedel-session--create
                   :name "main" :plan-metadata metadata)))
    (cl-letf (((symbol-function 'mevedel-plan-handoff--persist)
               (lambda (&rest _) (error "Disk unavailable"))))
      (should-error
       (mevedel-plan-handoff--goal-handoff-complete session (current-buffer))))
    (should (eq metadata (mevedel-session-plan-metadata session)))
    (should (eq retry
                (plist-get (mevedel-session-plan-metadata session)
                           :implementation-retry)))))

(mevedel-deftest mevedel-plan-handoff--persist
  (:doc "delegates session saving to the canonical persistence writer")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "main")) seen)
    (cl-letf (((symbol-function 'mevedel-session-persistence-save)
               (lambda (saved-session buffer)
                 (setq seen (list saved-session buffer)))))
      (mevedel-plan-handoff--persist session (current-buffer)))
    (should (equal (list session (current-buffer)) seen))))

(mevedel-deftest mevedel-plan-handoff--worktree-target-buffer
  (:doc "restores only the recorded Worktree session and directory")
  ,test
  (test)
  (let* ((directory (make-temp-file "mevedel-plan-target-dir-" t))
         (save-path (make-temp-file "mevedel-plan-target-save-" t))
         (buffer (generate-new-buffer " *mevedel-plan-target*"))
         (session
          (mevedel-session--create
           :name "target" :session-id "target-id"
           :working-directory directory)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (_) buffer)))
            (should
             (eq buffer
                 (mevedel-plan-handoff--worktree-target-buffer
                  (list :target-save-path save-path
                        :target-session-id "target-id"
                        :target-directory directory))))))
      (kill-buffer buffer)
      (delete-directory save-path t)
      (delete-directory directory t))))

(mevedel-deftest mevedel-plan-handoff--prepare-worktree
  (:doc "creates one clean target and persists its retry identity")
  ,test
  (test)
  (let* ((source-session (mevedel-session--create :name "source"))
         (target-session
          (mevedel-session--create :name "target" :session-id "target-id"))
         (source-buffer (generate-new-buffer " *mevedel-plan-worktree-source*"))
         (target-buffer (generate-new-buffer " *mevedel-plan-worktree-target*"))
         (record
          '(:selection (:location worktree :context summary
                        :execution direct :mode edits :branch "plan/test")))
         clean)
    (unwind-protect
        (progn
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session))
          (cl-letf (((symbol-function 'mevedel-worktree-create-session)
                     (lambda (branch _purpose clean-arg)
                       (setq clean clean-arg)
                       (list :branch branch :directory "/tmp/target"
                             :buffer target-buffer)))
                    ((symbol-function
                      'mevedel-session-persistence-ensure-files)
                     (lambda (&rest _) "/tmp/target-save"))
                    ((symbol-function 'mevedel-plan-handoff--persist) #'ignore))
            (let ((prepared
                   (mevedel-plan-handoff--prepare-worktree
                    source-session source-buffer record)))
              (should clean)
              (should (eq 'prepare-target (plist-get prepared :step)))
              (should (equal "target-id"
                             (plist-get prepared :target-session-id)))
              (should (equal prepared
                             (plist-get
                              (mevedel-session-plan-metadata source-session)
                              :implementation-retry))))))
      (kill-buffer target-buffer)
      (kill-buffer source-buffer))))

(mevedel-deftest mevedel-plan-handoff--prepare-worktree-target
  (:doc "copies the accepted artifact and selected settings into the target")
  ,test
  (test)
  (let* ((source-save (make-temp-file "mevedel-plan-worktree-source-" t))
         (path (file-name-concat source-save "local" "plans" "source.md"))
         (source-session
          (mevedel-session--create
           :name "source" :save-path source-save
           :preset-name 'source-preset
           :preset-settings '(:model test)))
         (target-session (mevedel-session--create
                          :name "target"
                          :save-path (make-temp-file "mevedel-plan-target-" t)))
         (source-buffer (generate-new-buffer " *mevedel-plan-target-source*"))
         (target-buffer (generate-new-buffer " *mevedel-plan-target-data*"))
         mode archive-relative-path)
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Accepted" nil path nil 'silent)
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session))
          (cl-letf (((symbol-function
                      'mevedel-plan-handoff--worktree-target-buffer)
                     (lambda (_) target-buffer))
                    ((symbol-function 'mevedel-plan-archive-accepted)
                     (lambda (_artifact _session relative-path)
                       (setq archive-relative-path relative-path)
                       '(:path "local/plans/accepted.md"
                         :absolute-path "/tmp/target-accepted.md"
                         :hash "target-hash")))
                    ((symbol-function 'mevedel-preset-restore-session)
                     #'ignore)
                    ((symbol-function 'mevedel-permission-mode-transition)
                     (lambda (selected) (setq mode selected)))
                    ((symbol-function 'mevedel-plan-handoff--persist) #'ignore))
            (let* ((record
                    (list :selection
                          '(:location worktree :context fresh
                            :execution goal :mode full-auto
                            :goal-token-budget 4321)
                          :goal-id "reserved-goal"
                          :accepted
                          (list :path "local/plans/source.md"
                                :absolute-path path
                                :hash (mevedel-plan-hash "# Accepted"))))
                   (prepared
                    (mevedel-plan-handoff--prepare-worktree-target
                     source-session source-buffer record)))
              (should (eq 'submit (plist-get prepared :step)))
              (should (eq 'full-auto mode))
              (should (equal "local/plans/accepted.md"
                             archive-relative-path))
              (should (= 4321
                         (buffer-local-value
                          'mevedel-goal-token-budget target-buffer)))
              (should (eq 'source-preset
                          (mevedel-session-preset-name target-session)))
              (should (equal
                       "reserved-goal"
                       (plist-get
                        (mevedel-session-plan-metadata target-session)
                        :implementation-goal-id)))
              (should (equal "/tmp/target-accepted.md"
                             (plist-get
                              (plist-get prepared :target-accepted)
                              :absolute-path))))))
      (kill-buffer target-buffer)
      (kill-buffer source-buffer)
      (delete-directory source-save t)
      (delete-directory (mevedel-session-save-path target-session) t)))

  :doc "reuses durable target preparation when source step persistence fails"
  (let* ((source-save (make-temp-file "mevedel-plan-target-source-" t))
         (target-save (make-temp-file "mevedel-plan-target-" t))
         (source-path (file-name-concat source-save "local" "plans" "source.md"))
         (target-path (file-name-concat target-save "local" "plans" "accepted.md"))
         (body "# Accepted")
         (source-session (mevedel-session--create
                          :name "source" :save-path source-save))
         (target-session (mevedel-session--create
                          :name "target" :save-path target-save))
         (source-buffer (generate-new-buffer " *mevedel-plan-target-retry-source*"))
         (target-buffer (generate-new-buffer " *mevedel-plan-target-retry-data*"))
         (record
          (list :step 'prepare-target
                :selection '(:location worktree :context fresh
                              :execution goal :mode full-auto
                              :goal-token-budget 4321)
                :goal-id "reserved"
                :accepted
                (list :path "local/plans/source.md"
                      :absolute-path source-path
                      :hash (mevedel-plan-hash body))))
         (archives 0)
         (settings 0)
         (source-saves 0))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source-path) t)
          (make-directory (file-name-directory target-path) t)
          (write-region body nil source-path nil 'silent)
          (write-region body nil target-path nil 'silent)
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session))
          (setf (mevedel-session-plan-metadata source-session)
                (list :implementation-retry record))
          (cl-letf (((symbol-function
                      'mevedel-plan-handoff--worktree-target-buffer)
                     (lambda (_) target-buffer))
                    ((symbol-function 'mevedel-plan-archive-accepted)
                     (lambda (&rest _)
                       (cl-incf archives)
                       (list :path "local/plans/accepted.md"
                             :absolute-path target-path
                             :hash (mevedel-plan-hash body))))
                    ((symbol-function 'mevedel-preset-restore-session)
                     (lambda (&rest _) (cl-incf settings)))
                    ((symbol-function 'mevedel-permission-mode-transition)
                     #'ignore)
                    ((symbol-function 'mevedel-plan-handoff--persist)
                     (lambda (saved-session _buffer)
                       (when (eq saved-session source-session)
                         (cl-incf source-saves)
                         (when (= source-saves 1)
                           (error "Source sidecar unavailable"))))))
            (should-error
             (mevedel-plan-handoff--prepare-worktree-target
              source-session source-buffer record))
            ;; Simulate restoring the source sidecar that still points at the
            ;; last durable step while retaining the durable target session.
            (setf (mevedel-session-plan-metadata source-session)
                  (list :implementation-retry record))
            (let ((prepared
                   (mevedel-plan-handoff--prepare-worktree-target
                    source-session source-buffer record)))
              (should (eq 'submit (plist-get prepared :step)))))
          (should (= 1 archives))
          (should (= 1 settings)))
      (kill-buffer target-buffer)
      (kill-buffer source-buffer)
      (delete-directory target-save t)
      (delete-directory source-save t))))

(mevedel-deftest mevedel-plan-handoff--prepare-summary
  (:doc "Worktree generates one plan-free portable handoff without compaction")
  ,test
  (test)
  (let* ((root (make-temp-file "mevedel-plan-summary-root-" t))
         (save-dir (make-temp-file "mevedel-plan-summary-save-" t))
         (path (file-name-concat save-dir "local" "plans" "accepted.md"))
         (session
          (mevedel-session--create
           :name "source" :save-path save-dir :working-directory root))
         (buffer (generate-new-buffer " *mevedel-plan-summary-data*"))
         (source-before "Current evidence.\n# Accepted")
         (calls 0) captured dispatched)
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Accepted" nil path nil 'silent)
          (with-current-buffer buffer (insert source-before))
          (cl-letf (((symbol-function 'mevedel--compact-main-target)
                     (lambda ()
                       (list :eligible-p t :previous-summary "Prior state")))
                    ((symbol-function 'mevedel--compact-find-boundary)
                     (lambda () (point-max)))
                    ((symbol-function 'mevedel--compact-evidence-selection)
                     (lambda (&rest _)
                       (list :content source-before)))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest _)
                       (ert-fail "Worktree summary ran compaction")))
                    ((symbol-function 'mevedel-context-summary-generate)
                     (lambda (source purpose callback &rest args)
                       (cl-incf calls)
                       (setq captured (list source purpose args))
                       (funcall callback
                                (list :outcome 'success
                                      :summary
                                      (concat root "/file.el\n"
                                              test-mevedel-plan-handoff--summary)))
                       #'ignore))
                    ((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                    ((symbol-function 'mevedel-plan-handoff--dispatch-accepted)
                     (lambda (&rest _) (setq dispatched t))))
            (mevedel-plan-handoff--prepare-summary
             session buffer
             (list :selection
                   '(:location worktree :context summary
                   :execution direct :mode ask
                   :instructions "Use $tdd exactly.")
                   :accepted
                   (list :path "local/plans/accepted.md"
                         :absolute-path path
                         :hash (mevedel-plan-hash "# Accepted")))))
          (let* ((retry
                  (plist-get (mevedel-session-plan-metadata session)
                             :implementation-retry))
                 (summary (plist-get retry :summary)))
            (should dispatched)
            (should (= calls 1))
            (should (eq (cadr captured) 'handoff))
            (should (string-match-p "Prior state" (car captured)))
            (should (string-match-p "accepted-plan omission" (car captured)))
            (should-not (string-match-p "# Accepted" (car captured)))
            (should (string-match-p "# Accepted"
                                    (plist-get (caddr captured) :focus)))
            (should (string-match-p "Use \\$tdd exactly"
                                    (plist-get (caddr captured) :focus)))
            (should (eq 'prepare-worktree (plist-get retry :step)))
            (should-not (string-match-p (regexp-quote root) summary))
            (should-not (string-match-p "# Accepted" summary))
            (cl-letf (((symbol-function 'mevedel--compact-main-target)
                       (lambda () '(:previous-summary "Prior state")))
                      ((symbol-function 'mevedel-context-summary-generate)
                       (lambda (&rest _)
                         (ert-fail "Cached summary sent another request")))
                      ((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                      ((symbol-function 'mevedel-plan-handoff--dispatch-accepted)
                       #'ignore))
              (mevedel-plan-handoff--prepare-summary
               session buffer
               (plist-put (copy-tree retry) :step 'prepare-summary)))
            (should (= calls 1))
            (with-current-buffer buffer
              (should (equal source-before (buffer-string))))))
      (kill-buffer buffer)
      (delete-file path)
      (delete-directory root t)
      (delete-directory save-dir t)))

  :doc "Here uses handoff semantics through aggressive compaction"
  (let* ((save-dir (make-temp-file "mevedel-plan-summary-save-" t))
         (path (file-name-concat save-dir "local" "plans" "accepted.md"))
         (session (mevedel-session--create :name "source" :save-path save-dir))
         (buffer (generate-new-buffer " *mevedel-plan-summary-here*"))
         captured-source captured-focus applied dispatched)
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Accepted" nil path nil 'silent)
          (cl-letf (((symbol-function 'mevedel--compact-main-target)
                     (lambda ()
                       (list :previous-summary "Prior state"
                             :begin-context-epoch t
                             :apply
                             (lambda (_target summary &rest _)
                               (setq applied summary)))))
                    ((symbol-function 'mevedel--compact-run)
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
                    ((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
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
             (:location worktree :context summary :execution goal :mode ask)))
    (should (mevedel-plan-handoff-selection-valid-p selection)))
  (dolist (selection
           '((:location worktree :context current :execution direct :mode ask)
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

(mevedel-deftest mevedel-plan-handoff--submit
  (:doc "routes the generated handoff through planned skill submission")
  ,test
  (test)
  (let* ((session (mevedel-session--create :name "main"))
         (data-buffer (generate-new-buffer " *plan-submit-data*"))
         (view-buffer (generate-new-buffer " *plan-submit-view*"))
         (selection
          '(:location here :context current :execution direct :mode edits
            :model-provider "OpenAI:gpt-5" :reasoning-effort high
            :instructions "Use $alpha"))
         (record
          (list :selection selection
                :accepted '(:absolute-path "/tmp/accepted.md" :hash "h")))
         planned dispatched)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf
              (((symbol-function 'mevedel-plan-handoff--accepted-body)
                (lambda (&rest _) "# Accepted"))
               ((symbol-function 'mevedel-view--interaction-target-buffer)
                (lambda (_) view-buffer))
               ((symbol-function 'mevedel-plan-handoff--apply-model-policy)
                #'ignore)
               ((symbol-function 'mevedel-skills-prepare-user-input)
                (lambda (input _) input))
               ((symbol-function
                 'mevedel-plan-handoff--validate-skill-bindings)
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

(mevedel-deftest mevedel-plan-handoff--validate-skill-bindings
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
     (mevedel-plan-handoff--validate-skill-bindings prompt session))
    (setf (mevedel-session-skills session) nil)
    (should-error
     (mevedel-plan-handoff--validate-skill-bindings prompt session))))

(provide 'test-mevedel-plan-handoff)
;;; test-mevedel-plan-handoff.el ends here
