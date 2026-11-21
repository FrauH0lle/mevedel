;;; test-niwa-session-fork.el --- Conversation and Worktree Fork tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `niwa-session-fork'.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-tool-exec-permission)
(require 'niwa-agent-control)
(require 'niwa-execution)


(defun test-niwa-fork--publish (session buffer &rest entries)
  "Publish and commit ENTRIES, (LOGICAL . BYTES), in SESSION via BUFFER."
  (niwa-session-publication-publish
   session
   (mapcar (lambda (entry)
             (list :path (expand-file-name (car entry)
                                           (niwa-session-save-path session))
                   :content (cdr entry)))
           entries)
   t)
  (niwa-session-artifacts-save session buffer nil t))

(niwa-deftest niwa-session-fork-clone-session ()
  ,test
  (test)
  :doc "Save As copies per-agent cwd while forks discard retained identities"
  (dolist (directory '(nil "/tmp/agent-copy/" "/ssh:example.invalid:/srv/agent/"))
    (let* ((record (niwa-agent-record--create
                    :id "cwd" :path "/root/cwd" :parent-path "/root"
                    :working-directory directory))
           (source (niwa-session--create
                    :agent-registry (list (cons "/root/cwd" record))))
           (saved (niwa-session-fork-clone-session source 'save-as))
           (forked (niwa-session-fork-clone-session source 'fork))
           (copied (cdr (assoc "/root/cwd" (niwa-session-agent-registry saved)))))
      (should (niwa-agent-record-p copied))
      (should-not (eq record copied))
      (should (equal directory (niwa-agent-record-working-directory copied)))
      (setf (niwa-agent-record-working-directory copied) "/tmp/changed-copy/")
      (should (equal directory (niwa-agent-record-working-directory record)))
      (should-not (niwa-session-agent-registry forked))))

  :doc "covers every session slot and isolates both clone policies"
  (progn
    (should (= 106
               (length
                (cdr (cl-struct-slot-info 'niwa-session)))))
    (should (niwa-session-fork--assert-clone-slot-completeness))
    (let* ((workspace
          (niwa-workspace--create
           :type 'project :id "clone-project" :root "/tmp/clone-project/"
           :name "clone-project"))
         (target (niwa-execution-target-create "/tmp/clone-project/"))
         (task (niwa-task--create
                :id 1 :subject "task" :status 'pending
                :metadata '(:nested (:value source))))
         (reminder
          (niwa-reminder--create
           :type 'clone :trigger #'ignore :content #'ignore :interval 1))
         (invoked
          (niwa-skill-invocation-record--create
           :name "clone-skill" :role 'instruction :origin 'user :turn 1))
         (goal
          (niwa-goal--create
           :id "clone-goal" :objective "Clone" :status 'paused
           :reason "test" :tokens-used 1 :time-used-seconds 2
           :turns-run 3 :created-at "created" :updated-at "updated"))
         (source
          (niwa-session--create
           :name "source"
           :workspace workspace
           :execution-target target
           :authority-mode 'portable
           :working-directory "/tmp/clone-project/"
           :tasks (list task)
           :task-status-notes '((nil :note "note"))
           :last-task-write-turn 1
           :permission-rules '(("Read" :path "/tmp" :action allow))
           :resource-grants '((:path "/tmp/exact" :access read))
           :permission-mode 'edits
           :sandbox-mode 'off
           :plan-mode t
           :preset-name 'clone-preset
           :model-provider "clone:model"
           :reasoning-effort 'high
           :turn-count 1
           :reminders (list reminder)
           :last-observed-date "2026-08-13"
           :agent-types-snapshot '(("worker" . "Worker"))
           :tool-catalog '(((:tool "Read") . "Read"))
           :messages '((:type RESULT :payload "message"))
           :agent-registry '(("/root/worker" :state (:nested source)))
           :agent-reservations '((reserved))
           :agent-root-activity 'running
           :agent-root-waiter 'waiter
           :agent-turn-capacity 7
           :pending-steering '((:input "steer"))
           :pending-follow-ups '((:input "follow-up"))
           :pending-input-next-id 2
           :pending-input-paused t
           :pending-input-failure-paused t
           :dropped-file-grants '("drop")
           :active-dropped-file-grants '("active")
           :skills '((skill :name "skill"))
           :hook-rules '((:event SessionStart))
           :hook-log '((:event SessionStart))
           :hook-log-pending '((pending))
           :repair-log '((repair))
           :repair-log-pending '((repair-pending))
           :permission-log-pending '((permission-pending))
           :telemetry-pending '((telemetry-pending))
           :hook-context-pending '((:event UserPromptSubmit))
           :execution-state 'execution
           :audit-session 'audit
           :pending-publication 'pending-publication
           :publication 'publication
           :publication-queue '(publication-queue)
           :publication-uncommitted-batches '(uncommitted)
           :publication-active-p t
           :publication-job 'publication-job
           :publication-preparations '(preparation)
           :publication-preparation-token 'preparation-token
           :publication-finishing-p t
           :publication-stopping-p t
           :mail-batch '(("/root" nil monitor ignore))
           :mail-notifications '((notification . ignore))
           :mail-notification-timer 'notification-timer
           :mail-notification-delay 0.5
           :checkpoint-state 'source-checkpoint
           :control-transfer 'control-transfer
           :control-transfer-drains '(drain)
           :lease 'lease
           :ownership-epoch 7
           :lease-renewal-timer 'timer
           :lease-renewal-job 'heartbeat
           :current-segment 1
           :prompt-index '((1 . ((:turn 1 :cum-turn 1))))
           :chapters '((:segment 1 :title "Earlier chat" :closed-at "closed"))
           :file-snapshots '((1 . (("/tmp/file" . (:version 1)))))
           :agent-transcripts '(("worker--1" :parent-turn 1 :status completed))
           :invoked-skills (list invoked)
           :pending-plan-approval '(:proposal pending)
           :plan-metadata '(:status accepted :nested source)
           :goal goal
           :attached-artifacts (list "board")
           :dedicated-artifact "board"
           :note-conversation t
           :schedules (list (list :id "w-000001" :note "Check CI" :status 'pending))))
         (fork
          (niwa-session-fork-clone-session
           source 'fork
           :save-path "/tmp/fork/"
           :session-id "fork"
           :created-at "fork-created"
           :updated-at "fork-updated"
           :current-segment 1
           :forked-from-session-id "source"
           :forked-from-turn 1))
         (save-as
          (niwa-session-fork-clone-session
           source 'save-as
           :name "copy"
           :save-path "/tmp/copy/"
           :session-id "copy"
           :created-at "copy-created"
           :updated-at "copy-updated"
           :forked-from-session-id "source")))
    (should-not (niwa-session-note-conversation fork))
    (should (niwa-session-note-conversation save-as))
    ;; One wake-up must never fire in two conversations.
    (should-not (niwa-session-schedules fork))
    (should (equal (niwa-session-schedules source) (niwa-session-schedules save-as)))
    (should-not (eq (car (niwa-session-schedules source))
                    (car (niwa-session-schedules save-as))))
    (should (= 7 (niwa-session-ownership-epoch source)))
    (dolist (candidate (list fork save-as))
      (should (= 0 (niwa-session-ownership-epoch candidate)))
      (should-not (niwa-session-checkpoint-state candidate))
      (should (eq workspace (niwa-session-workspace candidate)))
      (should (eq target (niwa-session-execution-target candidate)))
      (should (= 0 (hash-table-count
                    (niwa-session-touched-files candidate))))
      (should (= 0 (hash-table-count
                    (niwa-session-mentions-shown candidate))))
      (dolist (value
               (list (niwa-session-pending-reminders candidate)
                     (niwa-session-agent-reservations candidate)
                     (niwa-session-agent-root-waiter candidate)
                     (niwa-session-pending-steering candidate)
                     (niwa-session-pending-follow-ups candidate)
                     (niwa-session-pending-input-next-id candidate)
                     (niwa-session-pending-input-paused candidate)
                     (niwa-session-pending-input-failure-paused candidate)
                     (niwa-session-dropped-file-grants candidate)
                     (niwa-session-active-dropped-file-grants candidate)
                     (niwa-session-hook-log candidate)
                     (niwa-session-hook-log-pending candidate)
                     (niwa-session-repair-log candidate)
                     (niwa-session-repair-log-pending candidate)
                     (niwa-session-permission-log-pending candidate)
                     (niwa-session-telemetry-pending candidate)
                     (niwa-session-hook-context-pending candidate)
                     (niwa-session-execution-state candidate)
                     (niwa-session-audit-session candidate)
                     (niwa-session-pending-publication candidate)
                     (niwa-session-publication candidate)
                     (niwa-session-publication-queue candidate)
                     (niwa-session-publication-uncommitted-batches candidate)
                     (niwa-session-publication-active-p candidate)
                     (niwa-session-publication-job candidate)
                     (niwa-session-publication-preparations candidate)
                     (niwa-session-publication-preparation-token candidate)
                     (niwa-session-publication-finishing-p candidate)
                     (niwa-session-publication-stopping-p candidate)
                     (niwa-session-mail-batch candidate)
                     (niwa-session-mail-notifications candidate)
                     (niwa-session-mail-notification-timer candidate)
                     (niwa-session-mail-notification-delay candidate)
                     (niwa-session-control-transfer candidate)
                     (niwa-session-control-transfer-drains candidate)
                     (niwa-session-lease candidate)
                     (niwa-session-lease-renewal-timer candidate)
                     (niwa-session-lease-renewal-job candidate)
                     (niwa-session-permission-queue candidate)
                     (niwa-session-permission-absence candidate)
                     (niwa-session-pending-plan-approval candidate)))
        (should-not value)))
    (dolist (candidate (list fork save-as))
      (should (eq 'idle
                  (niwa-session-agent-root-activity candidate))))
    ;; Save As preserves logical state but owns every mutable container.
    (should (equal (niwa-session-tasks source)
                   (niwa-session-tasks save-as)))
    (should-not (eq (car (niwa-session-tasks source))
                    (car (niwa-session-tasks save-as))))
    (should-not (eq (niwa-task-metadata (car (niwa-session-tasks source)))
                    (niwa-task-metadata (car (niwa-session-tasks save-as)))))
    (dolist (getter '(niwa-session-task-status-notes
                      niwa-session-permission-rules
                      niwa-session-resource-grants
                      niwa-session-agent-types-snapshot
                      niwa-session-tool-catalog
                      niwa-session-messages
                      niwa-session-agent-registry
                      niwa-session-skills
                      niwa-session-prompt-index
                      niwa-session-chapters
                      niwa-session-file-snapshots
                      niwa-session-agent-transcripts
                      niwa-session-invoked-skills
                      niwa-session-plan-metadata))
      (should-not (eq (funcall getter source)
                      (funcall getter save-as))))
    (should-not (eq (niwa-session-reminders source)
                    (niwa-session-reminders save-as)))
    (should-not (eq (car (niwa-session-reminders source))
                    (car (niwa-session-reminders save-as))))
    (should-not (eq (niwa-session-goal source)
                    (niwa-session-goal save-as)))
    (should (equal (niwa-session-turn-count source)
                   (niwa-session-turn-count save-as)))
    (should-not (niwa-session-hook-rules save-as))
    ;; Fork retains policy configuration but starts a new conversation state.
    (should-not (niwa-session-tasks fork))
    (should-not (niwa-session-task-status-notes fork))
    (should (equal (niwa-session-hook-rules source)
                   (niwa-session-hook-rules fork)))
    (should-not (eq (niwa-session-hook-rules source)
                    (niwa-session-hook-rules fork)))
    (should (= 1 (niwa-session-turn-count fork)))
    (should (equal '((1 . ((:turn 1 :cum-turn 1))))
                   (niwa-session-prompt-index fork)))
    ;; Save As keeps every chat; a Fork only those closed before its segment.
    (should (equal (niwa-session-chapters source)
                   (niwa-session-chapters save-as)))
    (should-not (niwa-session-chapters fork))
    (should (equal '((1 . (("/tmp/file" . (:version 1)))))
                   (niwa-session-file-snapshots fork)))
    (should-not (eq (niwa-session-permission-rules source)
                    (niwa-session-permission-rules fork)))
    (should-not (eq (niwa-session-reminders source)
                    (niwa-session-reminders fork)))
    (should-not (niwa-session-goal fork))
    ;; Fork and Save As carry artifact references, each in its own list.
    (dolist (child (list fork save-as))
      (should (equal '("board") (niwa-session-attached-artifacts child)))
      ;; A copy of an artifact's conversation is an ordinary chat.
      (should-not (niwa-session-dedicated-artifact child))
      (should-not (eq (niwa-session-attached-artifacts source)
                      (niwa-session-attached-artifacts child))))
    (should (eq 'portable (niwa-session-authority-mode fork)))
    (should (equal "source"
                   (niwa-session-forked-from-session-id fork)))
    (should (equal "source"
                   (niwa-session-forked-from-session-id save-as))))))


(niwa-deftest niwa-session-fork-clone-session/fork ()
  ,test
  (test)
  :doc "copies and reduces fork state without mutating the parent"
  (let ((fixture (test-niwa-session-persistence--make-fork-ready)))
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-reminder
                (niwa-reminder--create
                 :type 'fork-isolation
                 :trigger (lambda (_ctx) t)
                 :content (lambda (_ctx) "Fork isolation")
                 :interval 1
                 :last-fired 1))
               (source-skill
                (niwa-skill--create
                 :name "fork-isolation"
                 :display-name "Fork isolation"
                 :path-patterns '("*.el")))
               (source-invoked-skill
                (niwa-skill-invocation-record--create
                 :name "source-skill"
                 :role 'instruction
                 :origin 'user
                 :turn 1))
               (future-invoked-skill
                (niwa-skill-invocation-record--create
                 :name "future-skill"
                 :role 'instruction
                 :origin 'user
                 :turn 3))
               (_ (setf (niwa-session-permission-mode session) 'full-auto
                        (niwa-session-permission-rules session)
                        '(("Bash" :pattern "npx test*"
                           :network t
                           :file-system
                           ((:path "/tmp/source-only" :access read))
                           :action allow))
                        (niwa-session-preset-name session) 'test-preset
                        (niwa-session-sandbox-mode session) 'required
                        (niwa-session-model-provider session)
                        "test-backend:test-model"
                        (niwa-session-reasoning-effort session) 'high
                        (niwa-session-resource-grants session)
                        '((:path "/tmp/independent-only" :access read))
                        (niwa-session-reminders session)
                        (list source-reminder)
                        (niwa-session-pending-reminders session)
                        '("pending once")
                        (niwa-session-skills session)
                        (list source-skill)
                        (niwa-session-invoked-skills session)
                        (list source-invoked-skill future-invoked-skill)
                        (niwa-session-dropped-file-grants session)
                        '("/tmp/pending.txt")
                        (niwa-session-active-dropped-file-grants session)
                        '("/tmp/active.txt")
                        (niwa-session-hook-context-pending session)
                        "pending hook context"
                        (niwa-session-pending-plan-approval session)
                        '(:proposal source)
                        (niwa-session-plan-metadata session)
                        '(:status proposed :path "local/plans/current.md")
                        (niwa-session-task-status-notes session)
                        '((nil :note "Source task note" :updated-turn 1))
                        (niwa-session-last-task-write-turn session) 1
                        (niwa-session-goal session)
                        (niwa-goal--create
                         :id "parent-goal" :objective "Ship"
                         :status 'active :tokens-used 0
                         :time-used-seconds 0 :turns-run 0
                         :created-at "created" :updated-at "updated")
                        (niwa-session-execution-state session)
                        'source-execution-state))
               (before (niwa-session-codec-serialize session))
               (before-text (prin1-to-string before))
               (staged (file-name-as-directory
                        (make-temp-file
                         (expand-file-name ".stage-clone-"
                                           (plist-get fixture :sessions-dir))
                         t)))
               (child
                (niwa-session-fork-clone-session
                 session 'fork
                 :save-path staged
                 :session-id "child-id"
                 :created-at "now"
                 :updated-at "now"
                 :current-segment 2
                 :forked-from-session-id (plist-get fixture :parent-id)
                 :forked-from-turn 2)))
          (should-not (eq child session))
          (should (eq (niwa-session-execution-target session)
                      (niwa-session-execution-target child)))
          (should (equal before
                         (niwa-session-codec-serialize session)))
          (should (equal staged (niwa-session-save-path child)))
          (should (= 2 (niwa-session-turn-count child)))
          (should (equal "test-backend:test-model"
                         (niwa-session-model-provider child)))
          (should (eq 'high (niwa-session-reasoning-effort child)))
          (should (eq 'full-auto
                      (niwa-session-permission-mode child)))
          (should (eq 'required (niwa-session-sandbox-mode child)))
          (should (equal
                   '(("Bash" :pattern "npx test*"
                      :network t
                      :file-system
                      ((:path "/tmp/source-only" :access read))
                      :action allow))
                   (niwa-session-permission-rules child)))
          (should
           (equal
            '(:level additive :additional-permissions
              (:network t :file-system ((:path "/tmp/source-only" :access read))))
            (niwa-tool-exec-permission-effective-sandbox-request
             '(:command "npx test") "Bash" "npx test" nil `(:session ,child))))
          (should-not (eq (niwa-session-permission-rules session)
                          (niwa-session-permission-rules child)))
          (should-not (eq (niwa-session-resource-grants session)
                          (niwa-session-resource-grants child)))
          (should-not (assoc 3 (niwa-session-prompt-index child)))
          (should-not (assoc 3 (niwa-session-file-snapshots child)))
          (should-not (assoc "future--2"
                             (niwa-session-agent-transcripts child)))
          (should (equal '("source-skill")
                         (mapcar #'niwa-skill-invocation-record-name
                                 (niwa-session-invoked-skills child))))
          (should-not (niwa-session-agent-registry child))
          (should-not (niwa-session-messages child))
          (should-not (niwa-session-execution-state child))
          (should-not (niwa-session-pending-reminders child))
          (should-not (niwa-session-dropped-file-grants child))
          (should-not (niwa-session-active-dropped-file-grants child))
          (should-not (niwa-session-hook-context-pending child))
          (should-not (niwa-session-pending-plan-approval child))
          (should-not (niwa-session-plan-metadata child))
          (should (eq 'source-execution-state
                      (niwa-session-execution-state session)))
          (should (= 7 (niwa-session-agent-turn-capacity child)))
          (should-not (niwa-session-goal child))
          (should (niwa-session-goal session))
          (should-not (niwa-session-task-status-notes child))
          (should-not (niwa-session-last-task-write-turn child))
          (puthash "child.el" t (niwa-session-touched-files child))
          (puthash '(file . "child.el") '(1 . "hash")
                   (niwa-session-mentions-shown child))
          (should-not (gethash "child.el"
                               (niwa-session-touched-files session)))
          (should-not (gethash '(file . "child.el")
                               (niwa-session-mentions-shown session)))
          (should (eq 'test-preset
                      (niwa-session-preset-name child)))
          (niwa-permission-add-session-resource-grant
           child "/tmp/child-only" 'read)
          (setf (niwa-session-permission-mode child) 'ask
                (niwa-session-sandbox-mode child) 'off
                (niwa-session-permission-rules child)
                '(("Bash" :pattern "child-only" :action allow)))
          (should (eq 'full-auto
                      (niwa-session-permission-mode session)))
          (should (eq 'required (niwa-session-sandbox-mode session)))
          (should (equal
                   '(("Bash" :pattern "npx test*"
                      :network t
                      :file-system
                      ((:path "/tmp/source-only" :access read))
                      :action allow))
                   (niwa-session-permission-rules session)))
          (should-not
           (member '(:path "/tmp/child-only" :access read)
                   (niwa-session-resource-grants session)))
          (let ((child-reminder (car (niwa-session-reminders child)))
                (child-skill (car (niwa-session-skills child)))
                (child-invoked-skill
                 (car (niwa-session-invoked-skills child))))
            (should-not (eq source-reminder child-reminder))
            (should-not (eq source-skill child-skill))
            (should-not (eq source-invoked-skill child-invoked-skill))
            (mapc #'funcall
                  (plist-get (niwa-reminders--collect-from
                              (niwa-session-reminders child) 2 child)
                             :commits))
            (niwa-skills-maybe-activate child "/tmp/child.el")
            (setf (niwa-skill-invocation-record-name child-invoked-skill)
                  "child-skill")
            (should (= 1 (niwa-reminder-last-fired source-reminder)))
            (should (= 2 (niwa-reminder-last-fired child-reminder)))
            (should-not (niwa-skill-active-p source-skill))
            (should (niwa-skill-active-p child-skill))
            (should (equal "source-skill"
                           (niwa-skill-invocation-record-name
                            source-invoked-skill))))
          (should (equal before
                         (niwa-session-codec-serialize session)))
          (should (equal before-text
                         (prin1-to-string
                          (niwa-session-codec-serialize session)))))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture))))


(niwa-deftest niwa-session-fork--materialize-fork-artifact ()
  ,test
  (test)
  :doc "copies literal committed bytes and distinguishes optional artifacts"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "niwa-fork-artifact-" t)))
         (workspace
          (test-niwa-session-persistence--make-file-workspace root))
         (source (niwa-session-create "source" workspace))
         (source-path
          (file-name-as-directory (file-name-concat root "source")))
         (staging-path
          (file-name-as-directory (file-name-concat root "staging")))
         (logical "plans/accepted.md"))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path source) source-path
                (niwa-session-session-id source) "source")
          (niwa-workspace-identity-ensure root)
          (make-directory source-path t)
          (should (niwa-session-durability-lease-acquire
                   source-path "*fork-artifact*" source))
          (should (niwa-session-publication-publish
                   source
                   (list (list :path (expand-file-name logical source-path)
                               :content "accepted bytes")
                         (list :path (niwa-session-artifacts-sidecar-path
                                      source-path)
                               :content
                               (niwa-session-artifacts-printed-value
                                (with-temp-buffer
                                  (niwa-session-artifacts--live-sidecar
                                   source (current-buffer))))
                               :commit-marker t :replace t))))
          (should
           (niwa-session-fork--materialize-fork-artifact
            source logical staging-path t))
          (should
           (equal "accepted bytes"
                  (niwa-session-artifacts--file-text
                   (expand-file-name logical staging-path))))
          (should-not
           (niwa-session-fork--materialize-fork-artifact
            source "plans/optional.md" staging-path))
          (should-error
           (niwa-session-fork--materialize-fork-artifact
            source "plans/required.md" staging-path t)))
      (ignore-errors
        (niwa-session-durability-lease-release source-path source))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-fork--stage-fork (:quiet t)
  ,test
  (test)
  :doc "bare staging repairs active and closed segments without changing Source"
  (let* ((fixture (test-niwa-session-persistence--make-fork-ready))
         (session (plist-get fixture :session))
         (staging-path (file-name-as-directory
                        (make-temp-file
                         (expand-file-name ".stage-bounds-"
                                           (plist-get fixture :sessions-dir)) t)))
         (child (niwa-session-fork-clone-session
                 session 'fork :save-path staging-path :session-id "bounds-child"
                 :created-at "now" :updated-at "now" :current-segment 2
                 :forked-from-session-id (plist-get fixture :parent-id)
                 :forked-from-turn 2))
         (staging (generate-new-buffer " *fork-bounds-stage*")))
    (unwind-protect
        (progn
          (test-niwa-fork--publish
           session (plist-get fixture :buffer)
           (cons "segment-0001.chat.org"
                 (with-temp-buffer
                   (org-mode)
                   (insert (test-niwa-session-persistence--execution-tool-block
                            "closed-bash"
                            '(:kind bash :state running :execution-id "closed-exec"
                                    :live-execution-p t :execution-output "closed output")))
                   (niwa-session-artifacts-stabilize-gptel-bounds)
                   (buffer-string))))
          (with-current-buffer staging
            (org-mode)
            (should-not (bound-and-true-p gptel-mode))
            (insert "Prompt\n")
            (insert (propertize
                     (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"true\"))\n\noutput\n"
                             (niwa-tool-render-data-format
                              '(:kind bash :state running :execution-id "bounds-exec"
                                      :live-execution-p t :execution-output "output")
                              "bounds-bash")
                             "#+end_tool\n")
                     'gptel '(tool . "bounds-bash")))
            (insert (niwa--format-hook-audit-record
                     '(:type tool-result-rewrite :event "PostToolUse"
                             :tool-name "Bash" :reason "retained audit evidence")))
            (insert (propertize
                     "#+begin_tool\n(:name \"ToolSearch\" :args (:query \"agents\"))\n\nfound\n#+end_tool\n"
                     'gptel '(tool . "bounds-search")))
            (insert (propertize "Following response.\n" 'gptel 'response))
            (niwa-session-artifacts-stabilize-gptel-bounds))
          (niwa-session-fork--stage-fork
           child (plist-get fixture :buffer) staging (plist-get fixture :parent-path)
           staging-path 2 2)
          (dolist (where '(child source))
            (with-temp-buffer
              (if (eq where 'child)
                  (insert-file-contents
                   (niwa-session-artifacts-segment-path staging-path 1))
                (insert (decode-coding-string
                         (niwa-session-artifacts-read-artifact
                          session "segment-0001.chat.org" t) 'utf-8-unix)))
              (org-mode)
              (niwa-transcript-restore-properties)
              (should (eq (if (eq where 'child) 'lost 'running)
                          (plist-get
                           (niwa-tool-render-data-for-tool
                            (current-buffer) "closed-bash") :state)))))
          (with-current-buffer staging
            (should (equal (alist-get 'tool
                                      (read (org-entry-get (point-min) "GPTEL_BOUNDS")))
                           (alist-get 'tool (gptel--get-buffer-bounds)))))
          (with-temp-buffer
            (insert-file-contents
             (niwa-session-artifacts-segment-path staging-path 2))
            (org-mode)
            (niwa-transcript-restore-properties)
            (should (eq 'lost (plist-get
                              (niwa-tool-render-data-for-tool
                               (current-buffer) "bounds-bash") :state)))
            (let ((ranges (alist-get 'tool
                                    (read (org-entry-get (point-min) "GPTEL_BOUNDS")))))
              (dolist (range ranges)
                (goto-char (car range))
                (should (looking-at-p "(:name "))))
            (should (= 1 (length (niwa-transcript-audit-records (buffer-string)))))
            (let ((gptel--known-backends nil)
                  (gptel-model 'gpt-4o))
              (goto-char (point-max))
              (should (gptel--parse-buffer
                       (gptel-make-openai "fork-bounds" :key "unused") nil)))))
      (when (buffer-live-p staging)
        (with-current-buffer staging
          (set-buffer-modified-p nil)
          (setq-local kill-buffer-hook nil))
        (kill-buffer staging))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture)))

  :doc "materializes and validates a complete child in staging"
  (let* ((fixture (test-niwa-session-persistence--make-fork-ready))
         (session (plist-get fixture :session))
         (buf (plist-get fixture :buffer))
         (staging-path
          (file-name-as-directory
           (make-temp-file
            (expand-file-name ".stage-test-"
                              (plist-get fixture :sessions-dir))
            t)))
         (child
          (niwa-session-fork-clone-session
           session 'fork
           :save-path staging-path
           :session-id "child-id"
           :created-at "now"
           :updated-at "now"
           :current-segment 2
           :forked-from-session-id (plist-get fixture :parent-id)
           :forked-from-turn 2))
         (staging-buffer
          (with-current-buffer buf
            (clone-buffer " *niwa-stage-test*" nil))))
    (unwind-protect
        (progn
          (let ((plans-dir
                 (file-name-concat
                  (plist-get fixture :parent-path) "local" "plans")))
            (setf (niwa-session-plan-metadata session)
                  (list :status 'accepted
                        :accepted-turn 2
                        :accepted-path "local/plans/accepted.md"
                        :accepted-hash (niwa-plan-hash
                                        "accepted evidence"))))
          (with-current-buffer staging-buffer
            (setq-local kill-buffer-hook nil))
          ;; Artifacts live in the workspace store; a fork copies none.
          (test-niwa-fork--publish
           session buf
           (cons "local/plans/current.md" "mutable draft")
           (cons "local/plans/accepted.md" "accepted evidence")
           (cons "local/plans/unrelated.md" "unrelated plan evidence")
           (cons "local/notes.md" "parent local\n")
           (cons "artifacts/nested/mockup.html" "local artifact"))
          (niwa-session-fork--stage-fork
           child buf staging-buffer (plist-get fixture :parent-path)
           staging-path 2 2)
          (let ((child-local
                 (file-name-concat staging-path "local" "notes.md")))
            (should (equal "parent local\n"
                           (with-temp-buffer
                             (insert-file-contents child-local)
                             (buffer-string))))
            (write-region "child local\n" nil child-local nil 'silent)
            (should (equal "parent local\n"
                           (niwa-session-artifacts-read-artifact
                            session "local/notes.md" t))))
          (should (file-exists-p
                   (niwa-session-artifacts-segment-path
                    staging-path 1)))
          (should (file-exists-p
                   (niwa-session-artifacts-segment-path
                    staging-path 2)))
          (should (file-exists-p
                   (niwa-session-artifacts-sidecar-path staging-path)))
          ;; The workspace store owns notes: a fork copies no snapshot.
          (should-not (file-exists-p
                       (file-name-concat staging-path "instructions")))
          (should-not (file-exists-p (file-name-concat staging-path "artifacts")))
          ;; Mutation authority belongs to `--publish-fork' and must already
          ;; exist before this staging helper starts target writes.
          (should-not
           (file-exists-p (file-name-concat staging-path ".lease")))
          (should
           (file-exists-p
            (file-name-concat staging-path "local" "plans" "accepted.md")))
          (should-not
           (file-exists-p
            (file-name-concat staging-path "local" "plans" "unrelated.md")))
          (should-not
           (file-exists-p
            (file-name-concat staging-path "local" "plans" "current.md")))
          (test-niwa-fork--publish
           session buf (cons "local/plans/accepted.md" "tampered evidence"))
          (should-error
           (niwa-session-fork--stage-fork
            child buf staging-buffer (plist-get fixture :parent-path)
            staging-path 2 2)))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (when (file-directory-p staging-path)
        (delete-directory staging-path t))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture)))
  :doc "sources every retained remote artifact from the committed publication"
  (let* ((host "localhost")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-stage-fork-remote-" t)))
         (niwa-session-durability--client-id (make-string 64 ?a))
         source-buffer
         staging-buffer
         child
         staging-path)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,workspace ,source ,parent-path ,segment-one)
                        (test-niwa-session-persistence--make-remote-restore-fixture
                         host local-root "fixed segment"))
                       (plan-relative "plans/accepted.md")
                       (plan-path (expand-file-name plan-relative parent-path))
                       (backup-name "tracked@v1")
                       (backup-relative
                        (file-name-concat "file-history" backup-name))
                       (backup-path
                        (expand-file-name backup-relative parent-path))
                       (agent-relative "agents/explorer.chat.org")
                       (agent-path
                        (expand-file-name agent-relative parent-path))
                       (artifact-relative "artifacts/nested/mockup.html")
                       (artifact-path
                        (expand-file-name artifact-relative parent-path))
                       (sidecar
                        (niwa-session-artifacts-sidecar-path parent-path))
                       (sessions-dir
                        (niwa-session-artifacts-sessions-dir workspace))
                       (niwa-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (niwa-execution-target-identity
              (niwa-session-execution-target source))
             t niwa-session-durability--disclosed-targets)
            (setq source-buffer
                  (generate-new-buffer " *remote-stage-fork-source*"))
            (with-current-buffer source-buffer
              (org-mode)
              (setq-local niwa--session source)
              (insert "selected current segment\n"))
            (setf
             (niwa-session-current-segment source) 2
             (niwa-session-turn-count source) 2
             (niwa-session-prompt-index source)
             '((1 . ((:turn 1 :file-turn 1 :cum-turn 1 :pos 1
                       :preview "one" :fork-point-id "one")))
               (2 . ((:turn 1 :file-turn 1 :cum-turn 2 :pos 1
                       :preview "two" :fork-point-id "two"))))
             (niwa-session-file-snapshots source)
             `((1 . ((,(file-name-concat local-root "tracked.el")
                      :backup-name ,backup-name :version 1))))
             (niwa-session-agent-transcripts source)
             `(("explorer--1" :path ,agent-relative :parent-turn 1
                :status completed))
             (niwa-session-plan-metadata source)
             `(:status accepted :accepted-turn 2
               :accepted-path ,plan-relative
               :accepted-hash ,(niwa-plan-hash "published accepted plan")))
            (should
             (niwa-session-durability-lease-acquire
              parent-path "*remote-stage-parent*" source))
            (setf (niwa-session-publication source)
                  (niwa-session-publication-read parent-path))
            (niwa-session-publication-publish
             source
             (list
              (list :path segment-one :content "published segment one")
              (list :path plan-path :content "published accepted plan")
              (list :path backup-path :content "published backup bytes")
              (list :path agent-path :content "published agent transcript")
              (list :path artifact-path :content "published artifact")
              (list
               :path sidecar
               :content
               (niwa-session-artifacts-printed-value
                (niwa-session-artifacts-build-sidecar
                 source source-buffer))
               :commit-marker t)))
            (niwa-session-durability-lease-release parent-path source)
            ;; Poison or remove every fixed cache after the immutable commit.
            (write-region "poison segment" nil segment-one nil 'silent)
            (when (file-exists-p plan-path)
              (delete-file plan-path))
            (when (file-exists-p backup-path)
              (delete-file backup-path))
            (make-directory (file-name-directory agent-path) t)
            (write-region "poison agent" nil agent-path nil 'silent)
            (make-directory (file-name-directory artifact-path) t)
            (write-region "poison artifact" nil artifact-path nil 'silent)
            (setq staging-path
                  (file-name-as-directory
                   (file-name-concat sessions-dir ".fork-artifacts"))
                  child
                  (niwa-session-fork-clone-session
                   source 'fork
                   :save-path staging-path
                   :session-id "fork-artifacts"
                   :created-at "2026-08-11T20-00-00"
                   :updated-at "2026-08-11T20-00-00"
                   :current-segment 2
                   :forked-from-session-id
                   (niwa-session-session-id source)
                   :forked-from-turn 2)
                  staging-buffer
                  (generate-new-buffer " *remote-stage-fork-child*"))
            (with-current-buffer staging-buffer
              (org-mode)
              (insert "selected current segment\n"))
            (make-directory staging-path t)
            (should
             (niwa-session-persistence-lock-acquire
              staging-path "*remote-stage-child*" child))
            (niwa-session-fork--stage-fork
             child source-buffer staging-buffer parent-path staging-path 2 2)
            (should
             (equal "published segment one"
                    (niwa-session-artifacts--file-text
                     (niwa-session-artifacts-segment-path
                      staging-path 1))))
            (should
             (equal "published accepted plan"
                    (niwa-session-artifacts--file-text
                     (expand-file-name plan-relative staging-path))))
            (should
             (equal "published backup bytes"
                    (niwa-session-artifacts--file-text
                     (expand-file-name backup-relative staging-path))))
            (should
             (equal "published agent transcript"
                    (niwa-session-artifacts--file-text
                     (expand-file-name agent-relative staging-path))))
            ;; An old session's published artifact entry stays with it.
            (should-not (file-exists-p
                         (expand-file-name artifact-relative staging-path)))
            (niwa-session-persistence-lock-release staging-path child)
            (setq child nil)))
      (when child
        (ignore-errors
          (niwa-session-persistence-lock-release staging-path child)))
      (dolist (buffer (list staging-buffer source-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-fork--publish-fork
  (:after-each (tramp-cleanup-all-connections))
  ,test
  (test)
  :doc "moves one owned remote lease from staging into the restored child"
  (let* ((host "localhost")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-publish-fork-" t)))
         child-buffer
         source-buffer
         staging-buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace parent parent-path _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Parent transcript\n")
            (let* ((sessions-dir
                    (niwa-session-artifacts-sessions-dir workspace))
                   (staging-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir ".fork-staging")))
                   (new-save-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir "fork-child")))
                   (child
                    (niwa-session-fork-clone-session
                     parent 'fork
                     :save-path staging-path
                     :session-id "fork-child"
                     :created-at "2026-08-11T18-00-00"
                     :updated-at "2026-08-11T18-00-00"
                     :current-segment 1
                     :forked-from-session-id
                     (niwa-session-session-id parent)
                     :forked-from-turn 0))
                   (niwa-session-durability--client-id
                    (make-string 64 ?a))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'niwa-session-persistence-lock-acquire))
                   acquisitions
                   staged-generation)
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target child))
               t niwa-session-durability--disclosed-targets)
              (make-directory staging-path t)
              (setq source-buffer
                    (generate-new-buffer " *publish-fork-source*")
                    staging-buffer
                    (generate-new-buffer " *publish-fork-staging*"))
              (with-current-buffer source-buffer
                (org-mode)
                (setq-local niwa--session parent))
              (with-current-buffer staging-buffer
                (org-mode)
                (insert "Child transcript\n"))
              (cl-letf
                  (((symbol-function
                     'niwa-session-persistence-lock-acquire)
                    (lambda (&rest arguments)
                      (push (car arguments) acquisitions)
                      (apply acquire-function arguments)))
                   ((symbol-function 'niwa-session-fork--stage-fork)
                    (lambda (actual-child _buffer actual-staging-buffer
                              _parent-save-path actual-staging-path
                              _picked-segment _picked-cum-turn
                              &optional _additional-roots)
                      (setq staged-generation
                            (plist-get (niwa-session-lease actual-child)
                                       :generation))
                      (with-current-buffer actual-staging-buffer
                        (setq-local niwa--session actual-child)
                        (write-region
                         (point-min) (point-max)
                         (niwa-session-artifacts-segment-path
                          actual-staging-path 1)
                         nil 'silent)
                        (niwa-session-codec-write
                         (niwa-session-artifacts-sidecar-path
                          actual-staging-path)
                         (niwa-session-artifacts-build-sidecar
                          actual-child actual-staging-buffer)))))
                   ((symbol-function 'niwa--probe-session-target) #'ignore)
                   ((symbol-function 'niwa--chat-buffer-init-common)
                    #'ignore)
                   ((symbol-function
                     'niwa-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'niwa-session-artifacts-load-instructions)
                    #'ignore))
                (setq child-buffer
                      (niwa-session-fork--publish-fork
                       child source-buffer staging-buffer parent-path
                       staging-path new-save-path 1 0 nil)))
              (should (buffer-live-p child-buffer))
              (should (= 1 (length acquisitions)))
              (should
               (= staged-generation
                  (plist-get (niwa-session-lease child) :generation)))
              (should
               (equal new-save-path (niwa-session-save-path child)))
              (should-not (file-directory-p staging-path))
              (should
               (file-directory-p
                (file-name-concat new-save-path ".lease")))
              (should (niwa-session-durability-lease-owned-p child))
              (let ((publication (niwa-session-publication child)))
                (should publication)
                (should
                 (equal (plist-get publication :head)
                        (plist-get (niwa-session-lease child)
                                   :publication-head)))
                (should
                 (assoc "segment-0001.chat.org"
                        (plist-get publication :artifacts)))))))
      (when (buffer-live-p child-buffer)
        (let ((child-session
               (buffer-local-value 'niwa--session child-buffer)))
          (when (niwa-session-save-path child-session)
            (niwa-session-persistence-lock-release
             (niwa-session-save-path child-session) child-session))
          (with-current-buffer child-buffer
            (set-buffer-modified-p nil))
          (kill-buffer child-buffer)))
      (dolist (buffer (list source-buffer staging-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "removes a moved remote child when its publication cannot be read"
  (let* ((host "publish-fork-read-failure-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-publish-fork-failure-" t)))
         source-buffer
         staging-buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace parent parent-path _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Parent transcript\n")
            (let* ((sessions-dir
                    (niwa-session-artifacts-sessions-dir workspace))
                   (staging-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir ".fork-failed-staging")))
                   (new-save-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir "fork-failed-child")))
                   (child
                    (niwa-session-fork-clone-session
                     parent 'fork
                     :save-path staging-path
                     :session-id "fork-failed-child"
                     :created-at "2026-08-11T18-01-00"
                     :updated-at "2026-08-11T18-01-00"
                     :current-segment 1
                     :forked-from-session-id
                     (niwa-session-session-id parent)
                     :forked-from-turn 0))
                   (child-id (niwa-session-session-id child))
                   (read-publication
                    (symbol-function
                     'niwa-session-publication-read))
                   (release-lock
                    (symbol-function
                     'niwa-session-persistence-lock-release))
                   (niwa-session-durability--client-id
                    (make-string 64 ?b))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   released-before-delete)
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target child))
               t niwa-session-durability--disclosed-targets)
              (make-directory staging-path t)
              (setq source-buffer
                    (generate-new-buffer " *publish-fork-failure-source*")
                    staging-buffer
                    (generate-new-buffer " *publish-fork-failure-staging*"))
              (with-current-buffer staging-buffer
                (org-mode)
                (insert "Child transcript\n"))
              (cl-letf
                  (((symbol-function 'niwa-session-fork--stage-fork)
                    (lambda (actual-child _buffer actual-staging-buffer
                              _parent-save-path actual-staging-path
                              _picked-segment _picked-cum-turn
                              &optional _additional-roots)
                      (with-current-buffer actual-staging-buffer
                        (setq-local niwa--session actual-child)
                        (write-region
                         (point-min) (point-max)
                         (niwa-session-artifacts-segment-path
                          actual-staging-path 1)
                         nil 'silent)
                        (niwa-session-codec-write
                         (niwa-session-artifacts-sidecar-path
                          actual-staging-path)
                         (niwa-session-artifacts-build-sidecar
                          actual-child actual-staging-buffer)))))
                   ((symbol-function
                     'niwa-session-publication-read)
                    (lambda (path)
                      (if (equal (file-name-as-directory path)
                                 new-save-path)
                          (error "Injected publication read failure")
                        (funcall read-publication path))))
                   ((symbol-function
                     'niwa-session-persistence-lock-release)
                    (lambda (path &optional actual-child)
                      (when (and (equal path new-save-path)
                                 (file-directory-p new-save-path))
                        (setq released-before-delete t))
                      (funcall release-lock path actual-child))))
                (should-error
                 (niwa-session-fork--publish-fork
                  child source-buffer staging-buffer parent-path
                  staging-path new-save-path 1 0 nil)))
              (should-not released-before-delete)
              (should-not (file-directory-p staging-path))
              (should-not (file-directory-p new-save-path))
              (should-not (niwa-session-lease child))
              (should-not (niwa-session-lease-renewal-timer child))
              (should-not
               (cl-find-if
                (lambda (entry)
                  (equal child-id
                         (plist-get (plist-get entry :summary) :session-id)))
                (niwa-session-persistence-list-sessions workspace))))))
      (dolist (buffer (list source-buffer staging-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-fork-create/conversation ()
  ,test
  (test)
  :doc "publishes an independent child without changing Source files or state"
  (let ((fixture (test-niwa-session-persistence--make-fork-ready))
        child-buffer
        lifecycle-sources
        stopped
        source-state)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-file
                (file-name-concat
                 (niwa-session-working-directory session)
                 "current.txt"))
               (source-text
                (with-current-buffer (plist-get fixture :buffer)
                  (buffer-string)))
               (source-head
                (plist-get (niwa-session-publication session) :head)))
          (write-region "current checkout\n" nil source-file nil 'silent)
          (setf (niwa-session-model-provider session)
                "test-backend:test-model"
                (niwa-session-naming-state session) 'attempted
                (niwa-session-naming-input session) "Source naming input"
                (niwa-session-permission-mode session)
                'full-auto
                (niwa-session-sandbox-mode session)
                'required
                (niwa-session-permission-rules session)
                '(("Bash" :pattern "npx test*" :action allow))
                (niwa-session-resource-grants session)
                '((:path "/tmp/external-input" :access read))
                (niwa-session-hook-rules session)
                '((:event UserPromptSubmit :command "true")))
          (setq source-state
                (niwa-session-codec-serialize session))
          (cl-letf
              (((symbol-function 'niwa-execution-stop-services)
                (lambda (owner)
                  (should (eq owner session))
                  (should (equal '("fork") lifecycle-sources))
                  (setq stopped t)))
               ((symbol-function 'niwa--run-session-start-hooks)
                (lambda (source)
                  (push source lifecycle-sources)))
               ((symbol-function 'niwa-model-apply-session-policy)
                #'ignore))
            (setq child-buffer
                  (niwa-session-fork-create
                   (plist-get fixture :buffer)
                   '(:fork-point-id "fixture-fork") (quote conversation))))
          (should stopped)
          (should (buffer-live-p child-buffer))
          (should (equal '("fork") lifecycle-sources))
          (should (buffer-local-value 'niwa--session child-buffer))
          (let* ((child
                  (buffer-local-value 'niwa--session child-buffer))
                 (child-path (niwa-session-save-path child))
                 (child-sidecar
                  (car (read-from-string
                        (niwa-session-artifacts-read-artifact
                         child "session.meta.el" t)))))
            (should-not (equal (niwa-session-session-id session)
                               (niwa-session-session-id child)))
            (should (string= (niwa-session-session-id child)
                             (niwa-session-name child)))
            (should (eq 'pending (niwa-session-naming-state child)))
            (should-not (niwa-session-naming-input child))
            (should (equal (niwa-session-working-directory session)
                           (niwa-session-working-directory child)))
            (should (eq 'conversation
                        (niwa-session-fork-type child)))
            (should (equal "test-backend:test-model"
                           (niwa-session-model-provider child)))
            (should (eq 'full-auto
                        (niwa-session-permission-mode child)))
            (should (eq 'required
                        (niwa-session-sandbox-mode child)))
            (should (equal
                     '(("Bash" :pattern "npx test*" :action allow))
                     (niwa-session-permission-rules child)))
            (should (equal
                     '((:path "/tmp/external-input" :access read))
                     (niwa-session-resource-grants child)))
            (should-not (eq (niwa-session-permission-rules session)
                            (niwa-session-permission-rules child)))
            (should-not (eq (niwa-session-resource-grants session)
                            (niwa-session-resource-grants child)))
            (should-not (eq (niwa-session-hook-rules session)
                            (niwa-session-hook-rules child)))
            (should (equal (niwa-session-hook-rules session)
                           (niwa-session-hook-rules child)))
            (should (equal (niwa-session-session-id session)
                           (niwa-session-forked-from-session-id child)))
            (should (= 2 (niwa-session-forked-from-turn child)))
            (should (equal "fixture-fork"
                           (niwa-session-forked-from-fork-point-id child)))
            (should (eq 'conversation
                        (plist-get child-sidecar :fork-type)))
            ;; Provenance is no longer durable transcript text: the
            ;; sparse fork-provenance reminder regenerates it from the
            ;; child's slots.
            (with-current-buffer child-buffer
              (should-not (string-match-p "Conversation Fork"
                                          (buffer-string)))
              (should-not (string-match-p "Future segment prompt"
                                          (buffer-string))))
            (should (string-match-p "Conversation Fork"
                                    (niwa-session-fork-provenance-body
                                     child)))
            (should (equal "current checkout\n"
                           (niwa-session-artifacts--file-text
                            source-file)))
            (with-current-buffer (plist-get fixture :buffer)
              (should (equal source-text (buffer-string)))
              (should (eq session niwa--session)))
            (should (equal (plist-get fixture :parent-sidecar-text)
                           (decode-coding-string
                            (niwa-session-artifacts-read-artifact
                             session "session.meta.el" t)
                            'utf-8-unix)))
            ;; The Source keeps its lease and its published head.
            (should (niwa-session-durability-lease-owned-p session))
            (should (equal source-head
                           (plist-get (niwa-session-publication session)
                                      :head)))
            (should (equal source-state
                           (niwa-session-codec-serialize session)))))
      (when (buffer-live-p child-buffer)
        (let ((view (buffer-local-value 'niwa--view-buffer child-buffer)))
          (with-current-buffer child-buffer
            (set-buffer-modified-p nil))
          (when (buffer-live-p view)
            (kill-buffer view)))
        (when (buffer-live-p child-buffer)
          (kill-buffer child-buffer)))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture))))


(niwa-deftest niwa-session-fork-create/cleanup
  (:doc "cleans staging and preserves the source on errors and nonlocal exits")
  (dolist (escape '(error throw))
    (let ((fixture (test-niwa-session-persistence--make-fork-ready))
          staging-buffer)
      (unwind-protect
          (let* ((session (plist-get fixture :session))
                 (source-state (niwa-session-codec-serialize session))
                 (sessions-dir (niwa-session-artifacts-sessions-dir
                                (niwa-session-workspace session)))
                 (before (directory-files sessions-dir)))
            (cl-letf (((symbol-function 'niwa-session-rewind-load-rewind-target)
                       (lambda (_source buffer _target)
                         (setq staging-buffer buffer)
                         (if (eq escape 'error)
                             (error "Injected fork load failure")
                           (throw 'fork-cancel 'cancelled)))))
              (if (eq escape 'error)
                  (should-error
                   (niwa-session-fork-create
                    (plist-get fixture :buffer)
                    '(:fork-point-id "fixture-fork") (quote conversation)))
                (should (eq 'cancelled
                            (catch 'fork-cancel
                              (niwa-session-fork-create
                               (plist-get fixture :buffer)
                               '(:fork-point-id "fixture-fork") (quote conversation)))))))
            (should staging-buffer)
            (should-not (buffer-live-p staging-buffer))
            (should (equal before (directory-files sessions-dir)))
            (should (equal source-state (niwa-session-codec-serialize session)))
            (should (equal (plist-get fixture :parent-sidecar-text)
                           (decode-coding-string
                            (niwa-session-artifacts-read-artifact
                             session "session.meta.el" t)
                            'utf-8-unix))))
        (test-niwa-session-persistence--cleanup-fork-fixture fixture)))))


(niwa-deftest niwa-session-fork--retarget-worktree-state ()
  ,test
  (test)
  :doc "retargets valid local paths and drops malformed copied path state"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-retarget-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-retarget-child-" t)))
         (session
          (niwa-session--create
           :working-directory source-root
           :worktree-source-root source-root
           :worktree-directory worktree-root
           :file-snapshots
           `((1 . ((,(file-name-concat source-root "local.el")
                     . (:backup-name "local@v1")))))
           :resource-grants
           `((:path ,(file-name-concat source-root "grant.el")
              :access read)
             (:path "/external/grant.el" :access read)
             (:path "relative-grant.el" :access read))
           :permission-rules
           `(("Read" :path ,(file-name-concat source-root "rule.el")
              :action allow)
             ("Read" :path "/external/rule.el" :action allow)
             ("Bash" :pattern "git status" :action allow)
             ("Read" :path 42 :action allow)))))
    (unwind-protect
        (let ((dropped
               (niwa-session-fork--retarget-worktree-state
                session)))
          (should (= 2 (length dropped)))
          (should
           (equal
            (file-name-concat worktree-root "local.el")
            (caar (cdar (niwa-session-file-snapshots session)))))
          (should
           (equal
            (list (file-name-concat worktree-root "grant.el")
                  "/external/grant.el")
            (mapcar
             (lambda (grant) (plist-get grant :path))
             (niwa-session-resource-grants session))))
          (should
           (equal
            (list (file-name-concat worktree-root "rule.el")
                  "/external/rule.el"
                  nil)
            (mapcar
             (lambda (rule) (plist-get (cdr rule) :path))
             (niwa-session-permission-rules session)))))
      (delete-directory source-root t)
      (delete-directory worktree-root t))))


(niwa-deftest niwa-session-fork--retarget-worktree-roots ()
  ,test
  (test)
  :doc "retargets repository roots, preserves external roots, and drops malformed roots"
  (let* ((source
          (file-name-as-directory
           (make-temp-file "niwa-roots-source-" t)))
         (worktree
          (file-name-as-directory
           (make-temp-file "niwa-roots-worktree-" t)))
         (session
          (niwa-session--create
           :worktree-source-root source
           :worktree-directory worktree))
         (result
          (niwa-session-fork--retarget-worktree-roots
           session
           `((,source
              ,(file-name-concat source "src")
              "/tmp/external"
              "src")))))
    (unwind-protect
        (progn
          (should
           (equal
            `((,source
               ,(file-name-concat worktree "src")
               "/tmp/external"))
            (plist-get result :roots)))
          (should (= 1 (length (plist-get result :dropped)))))
      (delete-directory source t)
      (delete-directory worktree t))))


(niwa-deftest niwa-session-fork--assert-worktree-target ()
  ,test
  (test)
  :doc "accepts local targets and rejects escape through a symlink"
  (let ((worktree
         (file-name-as-directory
          (make-temp-file "niwa-worktree-safe-" t)))
        (external
         (file-name-as-directory
          (make-temp-file "niwa-worktree-escape-" t))))
    (unwind-protect
        (progn
          (should-not
           (niwa-session-fork--assert-worktree-target
            worktree (file-name-concat worktree "safe.el")))
          (make-symbolic-link external
                              (file-name-concat worktree "escape"))
          (should-error
           (niwa-session-fork--assert-worktree-target
            worktree
            (file-name-concat worktree "escape" "unsafe.el"))))
      (delete-directory worktree t)
      (delete-directory external t))))


(niwa-deftest niwa-session-fork--restore-worktree-files ()
  ,test
  (test)
  :doc "continues after individual backup failures without touching Source"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-restore-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-restore-child-" t)))
         (save-path
          (file-name-as-directory
           (make-temp-file "niwa-worktree-restore-session-" t)))
         (external-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-restore-external-" t)))
         (good-source (file-name-concat source-root "good.el"))
         (bad-source (file-name-concat source-root "bad.el"))
         (external-file (file-name-concat external-root "shared.el"))
         (good-target (file-name-concat worktree-root "good.el"))
         (bad-target (file-name-concat worktree-root "bad.el"))
         (source
          (niwa-session--create
           :save-path save-path
           :file-snapshots
           `((1 . ((,good-source . (:backup-name "good@v1"))
                   (,bad-source . (:backup-name "missing@v1"))
                   (,external-file . (:backup-name "external@v1")))))))
         (child
          (niwa-session--create
           :worktree-source-root source-root
           :worktree-directory worktree-root)))
    (unwind-protect
        (progn
          (test-niwa-session-persistence--lease-fixture
           source
           (list (list :path (niwa-session-artifacts-backup-path
                              save-path "good@v1")
                       :content "captured good\n")
                 (list :path (niwa-session-artifacts-sidecar-path save-path)
                       ;; This archive test reads backups, never sidecar metadata.
                       :content "sidecar"
                       :commit-marker t)))
          (write-region "source good\n" nil good-source nil 'silent)
          (write-region "source bad\n" nil bad-source nil 'silent)
          (write-region "shared external\n" nil external-file nil 'silent)
          (write-region "HEAD bad\n" nil bad-target nil 'silent)
          (let ((report
                 (niwa-session-fork--restore-worktree-files
                  source child 1)))
            (should (= 1 (plist-get report :restored)))
            (should (= 1 (length (plist-get report :unrestored))))
            (should (equal (list external-file)
                           (plist-get report :external)))
            (should (equal bad-target
                           (plist-get
                            (car (plist-get report :unrestored))
                            :path)))
            (should (equal "captured good\n"
                           (niwa-session-artifacts--file-text
                            good-target)))
            (should (equal "HEAD bad\n"
                           (niwa-session-artifacts--file-text
                            bad-target)))
            (should (equal "source good\n"
                           (niwa-session-artifacts--file-text
                            good-source)))
            (should (equal "source bad\n"
                           (niwa-session-artifacts--file-text
                            bad-source)))
            (should (equal "shared external\n"
                           (niwa-session-artifacts--file-text
                            external-file)))))
      (niwa-session-durability-lease-release save-path source)
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (delete-directory save-path t)
      (delete-directory external-root t)))
  :doc "validates the whole plan before writing any target"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-invalid-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-invalid-child-" t)))
         (save-path
          (file-name-as-directory
           (make-temp-file "niwa-worktree-invalid-session-" t)))
         (valid-source (file-name-concat source-root "valid.el"))
         (valid-target (file-name-concat worktree-root "valid.el"))
         (source
          (niwa-session--create
           :save-path save-path
           :file-snapshots
           `((1 . ((,valid-source . (:backup-name "valid@v1"))
                   ("relative.el" . (:backup-name "bad@v1")))))))
         (child
          (niwa-session--create
           :worktree-source-root source-root
           :worktree-directory worktree-root)))
    (unwind-protect
        (progn
          (write-region "HEAD\n" nil valid-target nil 'silent)
          (let ((err (should-error
                      (niwa-session-fork--restore-worktree-files
                       source child 1))))
            (should (string-match-p "Invalid captured Worktree Fork path"
                                    (error-message-string err))))
          (should (equal "HEAD\n"
                         (niwa-session-artifacts--file-text
                          valid-target))))
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (delete-directory save-path t)))
  :doc "portable restoration uses committed backups without a fixed cache"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-published-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "niwa-worktree-published-child-" t)))
         (workspace
          (niwa-workspace-get-or-create
           'project "worktree-published" source-root "worktree-published"))
         (source (niwa-session-create "source" workspace source-root))
         (session-dir
          (file-name-as-directory
           (file-name-concat source-root ".niwa" "sessions" "source")))
         (source-file (file-name-concat source-root "tracked.el"))
         (target-file (file-name-concat worktree-root "tracked.el"))
         (backup-name "tracked@v1")
         (backup-path
          (file-name-concat session-dir "file-history" backup-name))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (child
          (niwa-session--create
           :worktree-source-root source-root
           :worktree-directory worktree-root))
         (niwa-session-durability--client-id (make-string 64 ?b)))
    (setf (niwa-session-session-id source) "source"
          (niwa-session-save-path source) session-dir
          (niwa-session-file-snapshots source)
          `((1 . ((,source-file . (:backup-name ,backup-name
                                  :version 1))))))
    (make-directory session-dir t)
    (unwind-protect
        (progn
          (should
           (niwa-session-durability-lease-acquire
            session-dir "*worktree-published*" source))
          (niwa-session-publication-publish
           source
           (list (list :path backup-path :content "published backup")
                 (list :path sidecar :content "sidecar" :commit-marker t)))
          (niwa-session-durability-lease-release session-dir source)
          (make-directory (file-name-directory backup-path) t)
          (write-region "poison" nil backup-path nil 'silent)
          (let ((report
                 (niwa-session-fork--restore-worktree-files
                  source child 1)))
            (should (= 1 (plist-get report :restored)))
            (should-not (plist-get report :unrestored)))
          (should (equal "published backup"
                         (niwa-session-artifacts-read-file-raw target-file)))
          (delete-directory (file-name-directory backup-path) t)
          (write-region "reset" nil target-file nil 'silent)
          (let ((report
                 (niwa-session-fork--restore-worktree-files
                  source child 1)))
            (should (= 1 (plist-get report :restored)))
            (should-not (plist-get report :unrestored)))
          (should (equal "published backup"
                         (niwa-session-artifacts-read-file-raw target-file))))
      (when (niwa-session-durability-lease-owned-p source)
        (niwa-session-durability-lease-release session-dir source))
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-fork--worktree-restore-report-body ()
  ,test
  (test)
  :doc "names every partial, malformed, and external restoration gap"
  (let ((session
         (niwa-session--create
          :forked-from-session-id "source-id"
          :worktree-directory "/repo/.worktrees/main-fork-1/"
          :worktree-branch "worktree/main-fork-1"
          :worktree-base-commit "0123456789abcdef")))
    (let ((text
           (niwa-session-fork--worktree-restore-report-body
            session
            '(:restored 1
              :unrestored ((:path "/child/missing.el"
                            :reason "backup unavailable"))
              :external ("/external/shared.el")
              :dropped ("resource grant (:path relative.el)")))))
      (should (string-match-p "partial restoration" text))
      (should (string-match-p "not an exact historical checkout" text))
      (should (string-match-p "/child/missing.el: backup unavailable"
                              text))
      (should (string-match-p "/external/shared.el" text))
      (should (string-match-p "resource grant (:path relative.el)"
                              text))))
  :doc "strips client-specific remote prefixes from every disclosed path"
  (let* ((target (niwa-execution-target-create
                  "/ssh:alias:/srv/repo/"))
         (session
          (niwa-session--create
           :execution-target target
           :forked-from-session-id "source-id"
           :worktree-directory "/ssh:alias:/srv/repo/.worktrees/fork/"
           :worktree-branch "worktree/fork"
           :worktree-base-commit "0123456789abcdef"))
         (text
          (niwa-session-fork--worktree-restore-report-body
           session
           '(:restored 0
             :unrestored ((:path "/ssh:alias:/srv/repo/missing.el"
                           :reason "backup unavailable"))
             :external ("/ssh:alias:/srv/shared.el"
                        "/ssh:foreign:/srv/private.el")))))
    (should-not (string-match-p "/ssh:alias:" text))
    (should-not (string-match-p "/ssh:foreign:" text))
    (should (string-match-p "/srv/repo/missing.el" text))
    (should (string-match-p "/srv/shared.el" text))
    (should (string-match-p "<path outside session target>" text))))

(niwa-deftest niwa-session-fork-provenance-body ()
  ,test
  (test)
  :doc "returns nil for a session that is not a fork"
  (should-not (niwa-session-fork-provenance-body
               (niwa-session--create :name "main")))
  :doc "describes a conversation fork's shared working directory"
  (let ((body (niwa-session-fork-provenance-body
               (niwa-session--create
                :forked-from-session-id "source-id"
                :fork-type 'conversation
                :working-directory "/repo/"))))
    (should (string-prefix-p "Conversation Fork" body))
    (should (string-match-p "Source session: source-id" body))
    (should (string-match-p "Working directory: /repo/" body))
    (should (string-match-p "changes are shared" body)))
  :doc "describes a worktree fork with stripped remote prefixes"
  (let* ((target (niwa-execution-target-create "/ssh:alias:/srv/repo/"))
         (body (niwa-session-fork-provenance-body
                (niwa-session--create
                 :execution-target target
                 :forked-from-session-id "source-id"
                 :fork-type 'worktree
                 :worktree-directory "/ssh:alias:/srv/repo/.worktrees/fork/"
                 :worktree-branch "worktree/fork"
                 :worktree-base-commit "0123456789abcdef"))))
    (should (string-prefix-p "Worktree Fork" body))
    (should-not (string-match-p "/ssh:alias:" body))
    (should (string-match-p "/srv/repo/.worktrees/fork/" body))
    (should (string-match-p "Branch: worktree/fork" body))
    (should (string-match-p "Base commit: 0123456789abcdef" body))
    (should (string-match-p "Uncaptured files retain" body))))


(niwa-deftest niwa-session-fork-create/worktree ()
  ,test
  (test)
  :doc "publishes captured repository state in an isolated linked worktree"
  (let ((fixture (test-niwa-session-persistence--make-fork-ready))
        child-buffer
        lifecycle-sources)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-root
                (file-name-as-directory
                 (niwa-session-working-directory session)))
               (source-file (file-name-concat source-root "current.txt"))
               (untracked-file
                (file-name-concat source-root "source-only.txt"))
               (backup-name "current@v1"))
          (test-niwa-session-persistence--git source-root "init")
          (test-niwa-session-persistence--git
           source-root "config" "user.email" "niwa@example.invalid")
          (test-niwa-session-persistence--git
           source-root "config" "user.name" "Niwa Test")
          (write-region "HEAD state\n" nil source-file nil 'silent)
          (test-niwa-session-persistence--git
           source-root "add" "current.txt")
          (test-niwa-session-persistence--git
           source-root "commit" "-m" "base")
          (let ((base-commit
                 (test-niwa-session-persistence--git
                  source-root "rev-parse" "HEAD")))
            (setf
             (niwa-session-file-snapshots session)
             `((1 . ((,source-file
                      . (:backup-name ,backup-name :version 1)))))
             (niwa-session-resource-grants session)
             `((:path ,source-file :access write))
            (niwa-session-permission-rules session)
             `(("Write" :path ,source-file :action allow))
             (niwa-session-permission-mode session)
             'full-auto
             (niwa-session-sandbox-mode session)
             'required
             (niwa-session-model-provider session)
             "test-backend:test-model")
            (with-current-buffer (plist-get fixture :buffer)
              (setq-local
               niwa-workspace-additional-roots
               `((,source-root
                  ,(file-name-concat source-root "nested")
                  "/tmp/shared"))))
            (test-niwa-fork--publish
             session (plist-get fixture :buffer)
             (cons (file-name-concat "file-history" backup-name)
                   "captured state\n"))
            (let ((source-state
                   (niwa-session-codec-serialize session))
                  (source-sidecar
                   (niwa-session-artifacts-read-artifact
                    session "session.meta.el" t))
                  (source-head
                   (plist-get (niwa-session-publication session) :head)))
              (write-region "dirty Source state\n" nil
                            source-file nil 'silent)
              (write-region "untracked Source file\n" nil
                            untracked-file nil 'silent)
              (cl-letf
                  (((symbol-function 'niwa--run-session-start-hooks)
                    (lambda (source)
                      (push source lifecycle-sources)))
                   ((symbol-function 'niwa-model-apply-session-policy)
                    #'ignore))
                (setq child-buffer
                      (niwa-session-fork-create
                       (plist-get fixture :buffer)
                       '(:fork-point-id "fixture-fork") (quote worktree))))
              (should (buffer-live-p child-buffer))
              (should (equal '("fork") lifecycle-sources))
              (let* ((child
                      (buffer-local-value 'niwa--session child-buffer))
                     (worktree
                      (niwa-session-worktree-directory child))
                     (child-file
                      (file-name-concat worktree "current.txt")))
                (should (string= (niwa-session-session-id child)
                                 (niwa-session-name child)))
                (should (eq 'pending (niwa-session-naming-state child)))
                (should-not (niwa-session-naming-input child))
                (should (eq 'worktree
                            (niwa-session-fork-type child)))
                (should (equal source-root
                               (niwa-session-worktree-source-root child)))
                (should (eq 'full-auto
                            (niwa-session-permission-mode child)))
                (should (eq 'required
                            (niwa-session-sandbox-mode child)))
                (should (equal worktree
                               (niwa-session-working-directory child)))
                (should (equal "worktree/main-fork-1"
                               (niwa-session-worktree-branch child)))
                (should (equal base-commit
                               (niwa-session-worktree-base-commit child)))
                (should (equal base-commit
                               (test-niwa-session-persistence--git
                                worktree "rev-parse" "HEAD")))
                (should (equal "captured state\n"
                               (niwa-session-artifacts--file-text
                                child-file)))
                (should-not
                 (file-exists-p
                  (file-name-concat worktree "source-only.txt")))
                (should
                 (equal child-file
                        (caar
                         (cdar
                          (niwa-session-file-snapshots child)))))
                (should
                 (equal child-file
                        (plist-get
                         (car (niwa-session-resource-grants child))
                         :path)))
                (with-current-buffer child-buffer
                  (should
                   (equal
                    `((,source-root
                       ,(file-name-concat worktree "nested")
                       "/tmp/shared"))
                    niwa-workspace-additional-roots))
                  ;; The disclosure no longer lives in the transcript:
                  ;; provenance regenerates from slots, and the restore
                  ;; report waits in the pending reminder FIFO.
                  (should-not (string-match-p "Worktree Fork"
                                              (buffer-string))))
                (let ((provenance
                       (niwa-session-fork-provenance-body child)))
                  (should (string-match-p "Worktree Fork" provenance))
                  (should (string-match-p (regexp-quote base-commit)
                                          provenance)))
                (should (equal 1 (length
                                  (niwa-session-pending-reminders
                                   child))))
                (should (string-match-p
                         "Captured repository files restored: 1"
                         (car (niwa-session-pending-reminders
                               child))))
                (should (equal "dirty Source state\n"
                               (niwa-session-artifacts--file-text
                                source-file)))
                (should (equal "untracked Source file\n"
                               (niwa-session-artifacts--file-text
                                untracked-file)))
                (should (equal source-state
                               (niwa-session-codec-serialize
                                session)))
                (should
                 (equal source-sidecar
                        (niwa-session-artifacts-read-artifact
                         session "session.meta.el" t)))
                ;; The Source keeps its lease and its published head.
                (should (niwa-session-durability-lease-owned-p session))
                (should
                 (equal source-head
                        (plist-get (niwa-session-publication session)
                                   :head)))))))
      (when (buffer-live-p child-buffer)
        (let ((view (buffer-local-value 'niwa--view-buffer child-buffer)))
          (with-current-buffer child-buffer
            (set-buffer-modified-p nil))
          (when (buffer-live-p view)
            (kill-buffer view)))
        (when (buffer-live-p child-buffer)
          (kill-buffer child-buffer)))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture)))
  :doc "retains one reserved Git target after staging failure and blocks retry"
  (let ((fixture (test-niwa-session-persistence--make-fork-ready)))
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-root
                (file-name-as-directory
                 (niwa-session-working-directory session))))
          (test-niwa-session-persistence--git source-root "init")
          (test-niwa-session-persistence--git
           source-root "config" "user.email" "niwa@example.invalid")
          (test-niwa-session-persistence--git
           source-root "config" "user.name" "Niwa Test")
          (write-region "base\n" nil
                        (file-name-concat source-root "file.txt")
                        nil 'silent)
          (test-niwa-session-persistence--git source-root "add" "file.txt")
          (test-niwa-session-persistence--git
           source-root "commit" "-m" "base")
          (let* ((reservation
                  (niwa-worktree-fork-reservation session))
                 (target
                  (list :fork-point-id "fixture-fork"
                        :worktree-reservation reservation))
                 (directory (plist-get reservation :directory))
                 (branch (plist-get reservation :branch))
                 (source-state
                  (niwa-session-codec-serialize session))
                 (first-error
                  (cl-letf
                      (((symbol-function
                         'niwa-session-fork--stage-fork)
                        (lambda (&rest _)
                          (error "Injected staging failure"))))
                    (should-error
                     (niwa-session-fork-create
                      (plist-get fixture :buffer) target (quote worktree))))))
            (should (file-directory-p directory))
            (should
             (string-match-p
              (regexp-quote branch)
              (test-niwa-session-persistence--git
               source-root "branch" "--list" branch)))
            (should (string-match-p
                     (regexp-quote directory)
                     (error-message-string first-error)))
            (should (string-match-p
                     "git -C.*worktree remove --force"
                     (error-message-string first-error)))
            (should (equal source-state
                           (niwa-session-codec-serialize session)))
            (cl-letf
                (((symbol-function 'niwa-worktree-fork-reservation)
                  (lambda (&rest _)
                    (ert-fail "Retry allocated another reservation"))))
              (let ((retry-error
                     (should-error
                      (niwa-session-fork-create
                       (plist-get fixture :buffer) target (quote worktree))
                      :type 'user-error)))
                (should (string-match-p
                         (regexp-quote branch)
                         (error-message-string retry-error)))))
            (should-not
             (file-exists-p
              (file-name-concat source-root ".worktrees" "main-fork-2")))))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture))))


(niwa-deftest niwa-session-fork--worktree-fork-retained-error ()
  ,test
  (test)
  :doc "reports retained remote worktree paths in the target-native domain"
  (let* ((target (niwa-execution-target-create
                  "/ssh:alias:/srv/repo/"))
         (session (niwa-session--create :execution-target target))
         (text
          (niwa-session-fork--worktree-fork-retained-error
           session '(error "publish failed")
           '(:branch "worktree/fork"
             :directory "/ssh:alias:/srv/repo/.worktrees/fork/"
             :cleanup-command "git worktree remove"))))
    (should-not (string-match-p "/ssh:alias:" text))
    (should (string-match-p "/srv/repo/.worktrees/fork/" text))))


;;
;;; Pure helper: --agent-files-for-segments

(niwa-deftest niwa-session-fork--agent-files-for-segments
  (:doc "filters agent-transcripts entries by copied segment turn ranges")
  ,test
  (test)

  :doc "returns entries whose :parent-turn falls in copied segment ranges"
  (let ((index
         '((1 . ((:cum-turn 1) (:cum-turn 2)))
           (2 . ((:cum-turn 10) (:cum-turn 11)))))
        (entries
         '(("a--1" :parent-turn 1 :path "agents/a--1.chat.org")
           ("b--2" :parent-turn 2 :path "agents/b--2.chat.org")
           ("gap--5" :parent-turn 5 :path "agents/gap.chat.org")
           ("c--10" :parent-turn 10 :path "agents/c--10.chat.org")
           ("d--12" :parent-turn 12 :path "agents/d--12.chat.org"))))
    (let ((result
           (niwa-session-fork--agent-files-for-segments
            index entries 2 10)))
      (should (= 3 (length result)))
      (should (assoc "a--1" result))
      (should (assoc "b--2" result))
      (should (assoc "c--10" result))
      (should-not (assoc "gap--5" result))
      (should-not (assoc "d--12" result))))

  :doc "excludes entries with non-integer :parent-turn"
  (let ((index '((1 . ((:cum-turn 1) (:cum-turn 2)))))
        (entries
         '(("good--1" :parent-turn 1 :path "agents/good.chat.org")
           ("bad--2"  :parent-turn nil :path "agents/bad.chat.org")
           ("ugly--3" :parent-turn "string" :path "agents/ugly.chat.org"))))
    (let ((result
           (niwa-session-fork--agent-files-for-segments
            index entries 1 2)))
      (should (= 1 (length result)))
      (should (equal "good--1" (caar result)))))

  :doc "empty input returns empty"
  (should (null
           (niwa-session-fork--agent-files-for-segments
            nil nil 1 1)))

  :doc "picked-cum-turn before all prompts excludes everything"
  (let ((index '((1 . ((:cum-turn 1)))))
        (entries
         '(("a--1" :parent-turn 1 :path "agents/a.chat.org"))))
    (should (null
             (niwa-session-fork--agent-files-for-segments
              index entries 1 0)))))

(provide 'test-niwa-session-fork)
;;; test-niwa-session-fork.el ends here
