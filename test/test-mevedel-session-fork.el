;;; test-mevedel-session-fork.el --- Conversation and Worktree Fork tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-fork'.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))


(mevedel-deftest mevedel-session-fork-clone-session
  (:doc "covers every session slot and isolates both clone policies")
  (progn
    (should (= 89
             (length
              (cdr (cl-struct-slot-info 'mevedel-session)))))
    (should (mevedel-session-fork--assert-clone-slot-completeness))
    (let* ((workspace
          (mevedel-workspace--create
           :type 'project :id "clone-project" :root "/tmp/clone-project/"
           :name "clone-project"))
         (target (mevedel-execution-target-create "/tmp/clone-project/"))
         (task (mevedel-task--create
                :id 1 :subject "task" :status 'pending
                :metadata '(:nested (:value source))))
         (reminder
          (mevedel-reminder--create
           :type 'clone :trigger #'ignore :content #'ignore :interval 1))
         (invoked
          (mevedel-skill-invocation-record--create
           :name "clone-skill" :role 'instruction :origin 'user :turn 1))
         (goal
          (mevedel-goal--create
           :id "clone-goal" :objective "Clone" :status 'paused
           :reason "test" :tokens-used 1 :time-used-seconds 2
           :turns-run 3 :created-at "created" :updated-at "updated"))
         (source
          (mevedel-session--create
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
           :skills-snapshot '(("skill" . "Skill"))
           :specialist-nudge-state '(:count 1)
           :deferred-set '(((:tool "Read") . "Read"))
           :deferred-pending '(pending)
           :deferred-injected '(("Read" . 1))
           :deferred-used '("Read")
           :deferred-expired '("Old")
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
           :control-transfer 'control-transfer
           :control-transfer-drains '(drain)
           :lease 'lease
           :lease-renewal-timer 'timer
           :current-segment 1
           :prompt-index '((1 . ((:turn 1 :cum-turn 1))))
           :file-snapshots '((1 . (("/tmp/file" . (:version 1)))))
           :agent-transcripts '(("worker--1" :parent-turn 1 :status completed))
           :invoked-skills (list invoked)
           :pending-plan-approval '(:proposal pending)
           :plan-metadata '(:status accepted :nested source)
           :goal goal))
         (fork
          (mevedel-session-fork-clone-session
           source 'fork
           :save-path "/tmp/fork/"
           :session-id "fork"
           :created-at "fork-created"
           :updated-at "fork-updated"
           :current-segment 1
           :forked-from-session-id "source"
           :forked-from-turn 1))
         (save-as
          (mevedel-session-fork-clone-session
           source 'save-as
           :name "copy"
           :save-path "/tmp/copy/"
           :session-id "copy"
           :created-at "copy-created"
           :updated-at "copy-updated"
           :forked-from-session-id "source")))
    (dolist (candidate (list fork save-as))
      (should (eq workspace (mevedel-session-workspace candidate)))
      (should (eq target (mevedel-session-execution-target candidate)))
      (should (= 0 (hash-table-count
                    (mevedel-session-touched-files candidate))))
      (should (= 0 (hash-table-count
                    (mevedel-session-mentions-shown candidate))))
      (dolist (value
               (list (mevedel-session-directive-planning candidate)
                     (mevedel-session-pending-reminders candidate)
                     (mevedel-session-specialist-nudge-state candidate)
                     (mevedel-session-deferred-pending candidate)
                     (mevedel-session-deferred-injected candidate)
                     (mevedel-session-deferred-used candidate)
                     (mevedel-session-deferred-expired candidate)
                     (mevedel-session-agent-reservations candidate)
                     (mevedel-session-agent-root-waiter candidate)
                     (mevedel-session-pending-steering candidate)
                     (mevedel-session-pending-follow-ups candidate)
                     (mevedel-session-pending-input-next-id candidate)
                     (mevedel-session-pending-input-paused candidate)
                     (mevedel-session-pending-input-failure-paused candidate)
                     (mevedel-session-dropped-file-grants candidate)
                     (mevedel-session-active-dropped-file-grants candidate)
                     (mevedel-session-hook-log candidate)
                     (mevedel-session-hook-log-pending candidate)
                     (mevedel-session-repair-log candidate)
                     (mevedel-session-repair-log-pending candidate)
                     (mevedel-session-permission-log-pending candidate)
                     (mevedel-session-telemetry-pending candidate)
                     (mevedel-session-hook-context-pending candidate)
                     (mevedel-session-execution-state candidate)
                     (mevedel-session-audit-session candidate)
                     (mevedel-session-pending-publication candidate)
                     (mevedel-session-publication candidate)
                     (mevedel-session-publication-queue candidate)
                     (mevedel-session-publication-uncommitted-batches candidate)
                     (mevedel-session-publication-active-p candidate)
                     (mevedel-session-control-transfer candidate)
                     (mevedel-session-control-transfer-drains candidate)
                     (mevedel-session-lease candidate)
                     (mevedel-session-lease-renewal-timer candidate)
                     (mevedel-session-permission-queue candidate)
                     (mevedel-session-pending-plan-approval candidate)))
        (should-not value)))
    (dolist (candidate (list fork save-as))
      (should (eq 'idle
                  (mevedel-session-agent-root-activity candidate))))
    ;; Save As preserves logical state but owns every mutable container.
    (should (equal (mevedel-session-tasks source)
                   (mevedel-session-tasks save-as)))
    (should-not (eq (car (mevedel-session-tasks source))
                    (car (mevedel-session-tasks save-as))))
    (should-not (eq (mevedel-task-metadata (car (mevedel-session-tasks source)))
                    (mevedel-task-metadata (car (mevedel-session-tasks save-as)))))
    (dolist (getter '(mevedel-session-task-status-notes
                      mevedel-session-permission-rules
                      mevedel-session-resource-grants
                      mevedel-session-agent-types-snapshot
                      mevedel-session-skills-snapshot
                      mevedel-session-deferred-set
                      mevedel-session-messages
                      mevedel-session-agent-registry
                      mevedel-session-skills
                      mevedel-session-prompt-index
                      mevedel-session-file-snapshots
                      mevedel-session-agent-transcripts
                      mevedel-session-invoked-skills
                      mevedel-session-plan-metadata))
      (should-not (eq (funcall getter source)
                      (funcall getter save-as))))
    (should-not (eq (mevedel-session-reminders source)
                    (mevedel-session-reminders save-as)))
    (should-not (eq (car (mevedel-session-reminders source))
                    (car (mevedel-session-reminders save-as))))
    (should-not (eq (mevedel-session-goal source)
                    (mevedel-session-goal save-as)))
    (should (equal (mevedel-session-turn-count source)
                   (mevedel-session-turn-count save-as)))
    (should-not (mevedel-session-hook-rules save-as))
    ;; Fork retains policy configuration but starts a new conversation state.
    (should-not (mevedel-session-tasks fork))
    (should-not (mevedel-session-task-status-notes fork))
    (should (equal (mevedel-session-hook-rules source)
                   (mevedel-session-hook-rules fork)))
    (should-not (eq (mevedel-session-hook-rules source)
                    (mevedel-session-hook-rules fork)))
    (should (= 1 (mevedel-session-turn-count fork)))
    (should (equal '((1 . ((:turn 1 :cum-turn 1))))
                   (mevedel-session-prompt-index fork)))
    (should (equal '((1 . (("/tmp/file" . (:version 1)))))
                   (mevedel-session-file-snapshots fork)))
    (should-not (eq (mevedel-session-permission-rules source)
                    (mevedel-session-permission-rules fork)))
    (should-not (eq (mevedel-session-reminders source)
                    (mevedel-session-reminders fork)))
    (should-not (mevedel-session-goal fork))
    (should (eq 'portable (mevedel-session-authority-mode fork)))
    (should (equal "source"
                   (mevedel-session-forked-from-session-id fork)))
    (should (equal "source"
                   (mevedel-session-forked-from-session-id save-as))))))


(mevedel-deftest mevedel-session-fork-clone-session/fork ()
  ,test
  (test)
  :doc "copies and reduces fork state without mutating the parent"
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready)))
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-reminder
                (mevedel-reminder--create
                 :type 'fork-isolation
                 :trigger (lambda (_ctx) t)
                 :content (lambda (_ctx) "Fork isolation")
                 :interval 1
                 :last-fired 1))
               (source-skill
                (mevedel-skill--create
                 :name "fork-isolation"
                 :display-name "Fork isolation"
                 :path-patterns '("*.el")))
               (source-invoked-skill
                (mevedel-skill-invocation-record--create
                 :name "source-skill"
                 :role 'instruction
                 :origin 'user
                 :turn 1))
               (future-invoked-skill
                (mevedel-skill-invocation-record--create
                 :name "future-skill"
                 :role 'instruction
                 :origin 'user
                 :turn 3))
               (_ (setf (mevedel-session-permission-mode session) 'full-auto
                        (mevedel-session-permission-rules session)
                        '(("Bash" :pattern "npx test*"
                           :network t
                           :file-system
                           ((:path "/tmp/source-only" :access read))
                           :action allow))
                        (mevedel-session-preset-name session) 'test-preset
                        (mevedel-session-sandbox-mode session) 'required
                        (mevedel-session-model-provider session)
                        "test-backend:test-model"
                        (mevedel-session-reasoning-effort session) 'high
                        (mevedel-session-resource-grants session)
                        '((:path "/tmp/source-only" :access read))
                        (mevedel-session-reminders session)
                        (list source-reminder)
                        (mevedel-session-pending-reminders session)
                        '("pending once")
                        (mevedel-session-deferred-pending session)
                        '(deferred-tool)
                        (mevedel-session-deferred-injected session)
                        '(("Deferred" . 2))
                        (mevedel-session-deferred-used session)
                        '("Deferred")
                        (mevedel-session-deferred-expired session)
                        '("Expired")
                        (mevedel-session-skills session)
                        (list source-skill)
                        (mevedel-session-invoked-skills session)
                        (list source-invoked-skill future-invoked-skill)
                        (mevedel-session-dropped-file-grants session)
                        '("/tmp/pending.txt")
                        (mevedel-session-active-dropped-file-grants session)
                        '("/tmp/active.txt")
                        (mevedel-session-hook-context-pending session)
                        "pending hook context"
                        (mevedel-session-pending-plan-approval session)
                        '(:proposal source)
                        (mevedel-session-plan-metadata session)
                        '(:status proposed :path "local/plans/current.md")
                        (mevedel-session-task-status-notes session)
                        '((nil :note "Source task note" :updated-turn 1))
                        (mevedel-session-last-task-write-turn session) 1
                        (mevedel-session-goal session)
                        (mevedel-goal--create
                         :id "parent-goal" :objective "Ship"
                         :status 'active :tokens-used 0
                         :time-used-seconds 0 :turns-run 0
                         :created-at "created" :updated-at "updated")
                        (mevedel-session-execution-state session)
                        'source-execution-state))
               (before (mevedel-session-codec-serialize session))
               (before-text (prin1-to-string before))
               (child
                (mevedel-session-fork-clone-session
                 session 'fork
                 :save-path "/tmp/staged-fork/"
                 :session-id "child-id"
                 :created-at "now"
                 :updated-at "now"
                 :current-segment 2
                 :forked-from-session-id (plist-get fixture :parent-id)
                 :forked-from-turn 2)))
          (should-not (eq child session))
          (should (eq (mevedel-session-execution-target session)
                      (mevedel-session-execution-target child)))
          (should (equal before
                         (mevedel-session-codec-serialize session)))
          (should (equal "/tmp/staged-fork/"
                         (mevedel-session-save-path child)))
          (should (= 2 (mevedel-session-turn-count child)))
          (should (equal "test-backend:test-model"
                         (mevedel-session-model-provider child)))
          (should (eq 'high (mevedel-session-reasoning-effort child)))
          (should (eq 'full-auto
                      (mevedel-session-permission-mode child)))
          (should (eq 'required (mevedel-session-sandbox-mode child)))
          (should (equal
                   '(("Bash" :pattern "npx test*"
                      :network t
                      :file-system
                      ((:path "/tmp/source-only" :access read))
                      :action allow))
                   (mevedel-session-permission-rules child)))
          (should-not (eq (mevedel-session-permission-rules session)
                          (mevedel-session-permission-rules child)))
          (should-not (eq (mevedel-session-resource-grants session)
                          (mevedel-session-resource-grants child)))
          (should-not (assoc 3 (mevedel-session-prompt-index child)))
          (should-not (assoc 3 (mevedel-session-file-snapshots child)))
          (should-not (assoc "future--2"
                             (mevedel-session-agent-transcripts child)))
          (should (equal '("source-skill")
                         (mapcar #'mevedel-skill-invocation-record-name
                                 (mevedel-session-invoked-skills child))))
          (should-not (mevedel-session-agent-registry child))
          (should-not (mevedel-session-messages child))
          (should-not (mevedel-session-execution-state child))
          (should-not (mevedel-session-pending-reminders child))
          (should-not (mevedel-session-deferred-pending child))
          (should-not (mevedel-session-deferred-injected child))
          (should-not (mevedel-session-deferred-used child))
          (should-not (mevedel-session-deferred-expired child))
          (should-not (mevedel-session-dropped-file-grants child))
          (should-not (mevedel-session-active-dropped-file-grants child))
          (should-not (mevedel-session-hook-context-pending child))
          (should-not (mevedel-session-pending-plan-approval child))
          (should-not (mevedel-session-plan-metadata child))
          (should (eq 'source-execution-state
                      (mevedel-session-execution-state session)))
          (should (= 7 (mevedel-session-agent-turn-capacity child)))
          (should-not (mevedel-session-goal child))
          (should (mevedel-session-goal session))
          (should-not (mevedel-session-task-status-notes child))
          (should-not (mevedel-session-last-task-write-turn child))
          (puthash "child.el" t (mevedel-session-touched-files child))
          (puthash '(file . "child.el") '(1 . "hash")
                   (mevedel-session-mentions-shown child))
          (should-not (gethash "child.el"
                               (mevedel-session-touched-files session)))
          (should-not (gethash '(file . "child.el")
                               (mevedel-session-mentions-shown session)))
          (should (eq 'test-preset
                      (mevedel-session-preset-name child)))
          (mevedel-permission-add-session-resource-grant
           child "/tmp/child-only" 'read)
          (setf (mevedel-session-permission-mode child) 'ask
                (mevedel-session-sandbox-mode child) 'off
                (mevedel-session-permission-rules child)
                '(("Bash" :pattern "child-only" :action allow)))
          (should (eq 'full-auto
                      (mevedel-session-permission-mode session)))
          (should (eq 'required (mevedel-session-sandbox-mode session)))
          (should (equal
                   '(("Bash" :pattern "npx test*"
                      :network t
                      :file-system
                      ((:path "/tmp/source-only" :access read))
                      :action allow))
                   (mevedel-session-permission-rules session)))
          (should-not
           (member '(:path "/tmp/child-only" :access read)
                   (mevedel-session-resource-grants session)))
          (let ((child-reminder (car (mevedel-session-reminders child)))
                (child-skill (car (mevedel-session-skills child)))
                (child-invoked-skill
                 (car (mevedel-session-invoked-skills child))))
            (should-not (eq source-reminder child-reminder))
            (should-not (eq source-skill child-skill))
            (should-not (eq source-invoked-skill child-invoked-skill))
            (mapc #'funcall
                  (plist-get (mevedel-reminders--collect-from
                              (mevedel-session-reminders child) 2 child)
                             :commits))
            (mevedel-skills-maybe-activate child "/tmp/child.el")
            (setf (mevedel-skill-invocation-record-name child-invoked-skill)
                  "child-skill")
            (should (= 1 (mevedel-reminder-last-fired source-reminder)))
            (should (= 2 (mevedel-reminder-last-fired child-reminder)))
            (should-not (mevedel-skill-active-p source-skill))
            (should (mevedel-skill-active-p child-skill))
            (should (equal "source-skill"
                           (mevedel-skill-invocation-record-name
                            source-invoked-skill))))
          (should (equal before
                         (mevedel-session-codec-serialize session)))
          (should (equal before-text
                         (prin1-to-string
                          (mevedel-session-codec-serialize session)))))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))


(mevedel-deftest mevedel-session-fork--materialize-fork-artifact ()
  ,test
  (test)
  :doc "copies literal committed bytes and distinguishes optional artifacts"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-fork-artifact-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (source (mevedel-session-create "source" workspace))
         (source-path
          (file-name-as-directory (file-name-concat root "source")))
         (staging-path
          (file-name-as-directory (file-name-concat root "staging")))
         (logical "plans/accepted.md"))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path source) source-path)
          (make-directory (file-name-concat source-path "plans") t)
          (write-region "accepted bytes" nil
                        (expand-file-name logical source-path) nil 'silent)
          (should
           (mevedel-session-fork--materialize-fork-artifact
            source logical staging-path t))
          (should
           (equal "accepted bytes"
                  (mevedel-session-artifacts--file-text
                   (expand-file-name logical staging-path))))
          (should-not
           (mevedel-session-fork--materialize-fork-artifact
            source "plans/optional.md" staging-path))
          (should-error
           (mevedel-session-fork--materialize-fork-artifact
            source "plans/required.md" staging-path t)))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-fork--stage-fork ()
  ,test
  (test)
  :doc "materializes and validates a complete child in staging"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (session (plist-get fixture :session))
         (buf (plist-get fixture :buffer))
         (staging-path
          (file-name-as-directory
           (make-temp-file
            (expand-file-name ".stage-test-"
                              (plist-get fixture :sessions-dir))
            t)))
         (child
          (mevedel-session-fork-clone-session
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
            (clone-buffer " *mevedel-stage-test*" nil))))
    (unwind-protect
        (progn
          (let ((plans-dir
                 (file-name-concat
                  (plist-get fixture :parent-path) "local" "plans")))
            (make-directory plans-dir t)
            (write-region "mutable draft" nil
                          (file-name-concat plans-dir "current.md")
                          nil 'silent)
            (write-region "accepted evidence" nil
                          (file-name-concat plans-dir "accepted.md")
                          nil 'silent)
            (write-region "unrelated plan evidence" nil
                          (file-name-concat plans-dir "unrelated.md")
                          nil 'silent)
            (setf (mevedel-session-plan-metadata session)
                  (list :status 'accepted
                        :accepted-turn 2
                        :accepted-path "local/plans/accepted.md"
                        :accepted-hash (mevedel-plan-hash
                                        "accepted evidence"))))
          (with-current-buffer staging-buffer
            (setq-local kill-buffer-hook nil))
          (let ((parent-local
                 (file-name-concat
                  (plist-get fixture :parent-path) "local" "notes.md")))
            (make-directory (file-name-directory parent-local) t)
            (write-region "parent local\n" nil parent-local nil 'silent))
          (mevedel-session-fork--stage-fork
           child buf staging-buffer (plist-get fixture :parent-path)
           staging-path 2 2)
          (let ((parent-local
                 (file-name-concat
                  (plist-get fixture :parent-path) "local" "notes.md"))
                (child-local
                 (file-name-concat staging-path "local" "notes.md")))
            (should (equal "parent local\n"
                           (with-temp-buffer
                             (insert-file-contents child-local)
                             (buffer-string))))
            (write-region "child local\n" nil child-local nil 'silent)
            (should (equal "parent local\n"
                           (with-temp-buffer
                             (insert-file-contents parent-local)
                             (buffer-string)))))
          (should (file-exists-p
                   (mevedel-session-artifacts-segment-path
                    staging-path 1)))
          (should (file-exists-p
                   (mevedel-session-artifacts-segment-path
                    staging-path 2)))
          (should (file-exists-p
                   (mevedel-session-artifacts-sidecar-path staging-path)))
          (should (file-exists-p
                   (mevedel-session-artifacts-instructions-current-path
                    staging-path)))
          ;; Mutation authority belongs to `--publish-fork' and must already
          ;; exist before this staging helper starts target writes.
          (should-not
           (file-exists-p
            (mevedel-session-persistence--lock-path staging-path)))
          (should
           (file-exists-p
            (file-name-concat staging-path "local" "plans" "accepted.md")))
          (should-not
           (file-exists-p
            (file-name-concat staging-path "local" "plans" "unrelated.md")))
          (should-not
           (file-exists-p
            (file-name-concat staging-path "local" "plans" "current.md")))
          (write-region "tampered evidence" nil
                        (file-name-concat
                         (plist-get fixture :parent-path)
                         "local" "plans" "accepted.md")
                        nil 'silent)
          (should-error
           (mevedel-session-fork--stage-fork
            child buf staging-buffer (plist-get fixture :parent-path)
            staging-path 2 2)))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (when (file-directory-p staging-path)
        (delete-directory staging-path t))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "sources every retained remote artifact from the committed publication"
  (let* ((host "localhost")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-stage-fork-remote-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         source-buffer
         staging-buffer
         child
         staging-path)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,workspace ,source ,parent-path ,segment-one)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
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
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path parent-path))
                       (sessions-dir
                        (mevedel-session-artifacts-sessions-dir workspace))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target source))
             t mevedel-session-durability--disclosed-targets)
            (setq source-buffer
                  (generate-new-buffer " *remote-stage-fork-source*"))
            (with-current-buffer source-buffer
              (org-mode)
              (setq-local mevedel--session source)
              (insert "selected current segment\n"))
            (setf
             (mevedel-session-current-segment source) 2
             (mevedel-session-turn-count source) 2
             (mevedel-session-prompt-index source)
             '((1 . ((:turn 1 :file-turn 1 :cum-turn 1 :pos 1
                       :preview "one" :fork-point-id "one")))
               (2 . ((:turn 1 :file-turn 1 :cum-turn 2 :pos 1
                       :preview "two" :fork-point-id "two"))))
             (mevedel-session-file-snapshots source)
             `((1 . ((,(file-name-concat local-root "tracked.el")
                      :backup-name ,backup-name :version 1))))
             (mevedel-session-agent-transcripts source)
             `(("explorer--1" :path ,agent-relative :parent-turn 1
                :status completed))
             (mevedel-session-plan-metadata source)
             `(:status accepted :accepted-turn 2
               :accepted-path ,plan-relative
               :accepted-hash ,(mevedel-plan-hash "published accepted plan")))
            (should
             (mevedel-session-durability-lease-acquire
              parent-path "*remote-stage-parent*" source))
            (setf (mevedel-session-publication source)
                  (mevedel-session-publication-read parent-path))
            (mevedel-session-publication-publish
             source
             (list
              (list :path segment-one :content "published segment one")
              (list :path plan-path :content "published accepted plan")
              (list :path backup-path :content "published backup bytes")
              (list :path agent-path :content "published agent transcript")
              (list
               :path sidecar
               :content
               (mevedel-session-artifacts-printed-value
                (mevedel-session-artifacts-build-sidecar
                 source source-buffer))
               :commit-marker t)))
            (mevedel-session-durability-lease-release parent-path source)
            ;; Poison or remove every fixed cache after the immutable commit.
            (write-region "poison segment" nil segment-one nil 'silent)
            (when (file-exists-p plan-path)
              (delete-file plan-path))
            (when (file-exists-p backup-path)
              (delete-file backup-path))
            (make-directory (file-name-directory agent-path) t)
            (write-region "poison agent" nil agent-path nil 'silent)
            (setq staging-path
                  (file-name-as-directory
                   (file-name-concat sessions-dir ".fork-artifacts"))
                  child
                  (mevedel-session-fork-clone-session
                   source 'fork
                   :save-path staging-path
                   :session-id "fork-artifacts"
                   :created-at "2026-08-11T20-00-00"
                   :updated-at "2026-08-11T20-00-00"
                   :current-segment 2
                   :forked-from-session-id
                   (mevedel-session-session-id source)
                   :forked-from-turn 2)
                  staging-buffer
                  (generate-new-buffer " *remote-stage-fork-child*"))
            (with-current-buffer staging-buffer
              (org-mode)
              (insert "selected current segment\n"))
            (make-directory staging-path t)
            (should
             (mevedel-session-persistence-lock-acquire
              staging-path "*remote-stage-child*" child))
            (mevedel-session-fork--stage-fork
             child source-buffer staging-buffer parent-path staging-path 2 2)
            (should
             (equal "published segment one"
                    (mevedel-session-artifacts--file-text
                     (mevedel-session-artifacts-segment-path
                      staging-path 1))))
            (should
             (equal "published accepted plan"
                    (mevedel-session-artifacts--file-text
                     (expand-file-name plan-relative staging-path))))
            (should
             (equal "published backup bytes"
                    (mevedel-session-artifacts--file-text
                     (expand-file-name backup-relative staging-path))))
            (should
             (equal "published agent transcript"
                    (mevedel-session-artifacts--file-text
                     (expand-file-name agent-relative staging-path))))
            (mevedel-session-persistence-lock-release staging-path child)
            (setq child nil)))
      (when child
        (ignore-errors
          (mevedel-session-persistence-lock-release staging-path child)))
      (dolist (buffer (list staging-buffer source-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-fork--publish-fork ()
  ,test
  (test)
  :doc "moves one owned remote lease from staging into the restored child"
  (let* ((host "localhost")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-publish-fork-" t)))
         child-buffer
         source-buffer
         staging-buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace parent parent-path _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Parent transcript\n")
            (let* ((sessions-dir
                    (mevedel-session-artifacts-sessions-dir workspace))
                   (staging-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir ".fork-staging")))
                   (new-save-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir "fork-child")))
                   (child
                    (mevedel-session-fork-clone-session
                     parent 'fork
                     :save-path staging-path
                     :session-id "fork-child"
                     :created-at "2026-08-11T18-00-00"
                     :updated-at "2026-08-11T18-00-00"
                     :current-segment 1
                     :forked-from-session-id
                     (mevedel-session-session-id parent)
                     :forked-from-turn 0))
                   (mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'mevedel-session-persistence-lock-acquire))
                   acquisitions
                   staged-generation)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target child))
               t mevedel-session-durability--disclosed-targets)
              (make-directory staging-path t)
              (setq source-buffer
                    (generate-new-buffer " *publish-fork-source*")
                    staging-buffer
                    (generate-new-buffer " *publish-fork-staging*"))
              (with-current-buffer source-buffer
                (org-mode)
                (setq-local mevedel--session parent))
              (with-current-buffer staging-buffer
                (org-mode)
                (insert "Child transcript\n"))
              (cl-letf
                  (((symbol-function
                     'mevedel-session-persistence-lock-acquire)
                    (lambda (&rest arguments)
                      (push (car arguments) acquisitions)
                      (apply acquire-function arguments)))
                   ((symbol-function 'mevedel-session-fork--stage-fork)
                    (lambda (actual-child _buffer actual-staging-buffer
                              _parent-save-path actual-staging-path
                              _picked-segment _picked-cum-turn
                              &optional _additional-roots)
                      (setq staged-generation
                            (plist-get (mevedel-session-lease actual-child)
                                       :generation))
                      (with-current-buffer actual-staging-buffer
                        (setq-local mevedel--session actual-child)
                        (write-region
                         (point-min) (point-max)
                         (mevedel-session-artifacts-segment-path
                          actual-staging-path 1)
                         nil 'silent)
                        (mevedel-session-codec-write
                         (mevedel-session-artifacts-sidecar-path
                          actual-staging-path)
                         (mevedel-session-artifacts-build-sidecar
                          actual-child actual-staging-buffer)))))
                   ((symbol-function 'mevedel--probe-session-target) #'ignore)
                   ((symbol-function 'mevedel--chat-buffer-init-common)
                    #'ignore)
                   ((symbol-function
                     'mevedel-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'mevedel-session-artifacts-load-instructions)
                    #'ignore))
                (setq child-buffer
                      (mevedel-session-fork--publish-fork
                       child source-buffer staging-buffer parent-path
                       staging-path new-save-path 1 0 nil)))
              (should (buffer-live-p child-buffer))
              (should (= 1 (length acquisitions)))
              (should
               (= staged-generation
                  (plist-get (mevedel-session-lease child) :generation)))
              (should
               (equal new-save-path (mevedel-session-save-path child)))
              (should-not (file-directory-p staging-path))
              (should
               (file-directory-p
                (file-name-concat new-save-path ".lease")))
              (should (mevedel-session-durability-lease-owned-p child))
              (let ((publication (mevedel-session-publication child)))
                (should publication)
                (should
                 (equal (plist-get publication :head)
                        (plist-get (mevedel-session-lease child)
                                   :publication-head)))
                (should
                 (assoc "segment-0001.chat.org"
                        (plist-get publication :artifacts)))))))
      (when (buffer-live-p child-buffer)
        (let ((child-session
               (buffer-local-value 'mevedel--session child-buffer)))
          (when (mevedel-session-save-path child-session)
            (mevedel-session-persistence-lock-release
             (mevedel-session-save-path child-session) child-session))
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
      (mevedel-workspace-clear-registry)))
  :doc "removes a moved remote child when its publication cannot be read"
  (let* ((host "publish-fork-read-failure-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-publish-fork-failure-" t)))
         source-buffer
         staging-buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace parent parent-path _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Parent transcript\n")
            (let* ((sessions-dir
                    (mevedel-session-artifacts-sessions-dir workspace))
                   (staging-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir ".fork-failed-staging")))
                   (new-save-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir "fork-failed-child")))
                   (child
                    (mevedel-session-fork-clone-session
                     parent 'fork
                     :save-path staging-path
                     :session-id "fork-failed-child"
                     :created-at "2026-08-11T18-01-00"
                     :updated-at "2026-08-11T18-01-00"
                     :current-segment 1
                     :forked-from-session-id
                     (mevedel-session-session-id parent)
                     :forked-from-turn 0))
                   (child-id (mevedel-session-session-id child))
                   (read-publication
                    (symbol-function
                     'mevedel-session-publication-read))
                   (release-lock
                    (symbol-function
                     'mevedel-session-persistence-lock-release))
                   (mevedel-session-durability--client-id
                    (make-string 64 ?b))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   released-before-delete)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target child))
               t mevedel-session-durability--disclosed-targets)
              (make-directory staging-path t)
              (setq source-buffer
                    (generate-new-buffer " *publish-fork-failure-source*")
                    staging-buffer
                    (generate-new-buffer " *publish-fork-failure-staging*"))
              (with-current-buffer staging-buffer
                (org-mode)
                (insert "Child transcript\n"))
              (cl-letf
                  (((symbol-function 'mevedel-session-fork--stage-fork)
                    (lambda (actual-child _buffer actual-staging-buffer
                              _parent-save-path actual-staging-path
                              _picked-segment _picked-cum-turn
                              &optional _additional-roots)
                      (with-current-buffer actual-staging-buffer
                        (setq-local mevedel--session actual-child)
                        (write-region
                         (point-min) (point-max)
                         (mevedel-session-artifacts-segment-path
                          actual-staging-path 1)
                         nil 'silent)
                        (mevedel-session-codec-write
                         (mevedel-session-artifacts-sidecar-path
                          actual-staging-path)
                         (mevedel-session-artifacts-build-sidecar
                          actual-child actual-staging-buffer)))))
                   ((symbol-function
                     'mevedel-session-publication-read)
                    (lambda (path)
                      (if (equal (file-name-as-directory path)
                                 new-save-path)
                          (error "Injected publication read failure")
                        (funcall read-publication path))))
                   ((symbol-function
                     'mevedel-session-persistence-lock-release)
                    (lambda (path &optional actual-child)
                      (when (and (equal path new-save-path)
                                 (file-directory-p new-save-path))
                        (setq released-before-delete t))
                      (funcall release-lock path actual-child))))
                (should-error
                 (mevedel-session-fork--publish-fork
                  child source-buffer staging-buffer parent-path
                  staging-path new-save-path 1 0 nil)))
              (should-not released-before-delete)
              (should-not (file-directory-p staging-path))
              (should-not (file-directory-p new-save-path))
              (should-not (mevedel-session-lease child))
              (should-not (mevedel-session-lease-renewal-timer child))
              (should-not
               (cl-find-if
                (lambda (entry)
                  (equal child-id
                         (plist-get (plist-get entry :summary) :session-id)))
                (mevedel-session-persistence-list-sessions workspace))))))
      (dolist (buffer (list source-buffer staging-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-fork-conversation-fork ()
  ,test
  (test)
  :doc "publishes an independent child without changing Source files or state"
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready))
        child-buffer
        lifecycle-sources
        source-state)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-file
                (file-name-concat
                 (mevedel-session-working-directory session)
                 "current.txt"))
               (source-text
                (with-current-buffer (plist-get fixture :buffer)
                  (buffer-string)))
               (source-lock
                (mevedel-session-artifacts--file-text
                 (plist-get fixture :parent-lock))))
          (write-region "current checkout\n" nil source-file nil 'silent)
          (setf (mevedel-session-model-provider session)
                "test-backend:test-model"
                (mevedel-session-permission-mode session)
                'full-auto
                (mevedel-session-sandbox-mode session)
                'required
                (mevedel-session-permission-rules session)
                '(("Bash" :pattern "npx test*" :action allow))
                (mevedel-session-resource-grants session)
                '((:path "/tmp/external-input" :access read))
                (mevedel-session-hook-rules session)
                '((:event UserPromptSubmit :command "true")))
          (setq source-state
                (mevedel-session-codec-serialize session))
          (cl-letf
              (((symbol-function 'mevedel--run-session-start-hooks)
                (lambda (source)
                  (push source lifecycle-sources)))
               ((symbol-function 'mevedel-model-apply-session-policy)
                #'ignore))
            (setq child-buffer
                  (mevedel-session-fork-conversation-fork
                   (plist-get fixture :buffer)
                   '(:fork-point-id "fixture-fork"))))
          (should (buffer-live-p child-buffer))
          (should (equal '("fork") lifecycle-sources))
          (should (buffer-local-value 'mevedel--session child-buffer))
          (let* ((child
                  (buffer-local-value 'mevedel--session child-buffer))
                 (child-path (mevedel-session-save-path child))
                 (child-sidecar
                  (mevedel-session-codec-read
                   (mevedel-session-artifacts-sidecar-path child-path))))
            (should-not (equal (mevedel-session-session-id session)
                               (mevedel-session-session-id child)))
            (should (string= "main · conversation 1"
                             (mevedel-session-name child)))
            (should (equal (mevedel-session-working-directory session)
                           (mevedel-session-working-directory child)))
            (should (eq 'conversation
                        (mevedel-session-fork-type child)))
            (should (equal "test-backend:test-model"
                           (mevedel-session-model-provider child)))
            (should (eq 'full-auto
                        (mevedel-session-permission-mode child)))
            (should (eq 'required
                        (mevedel-session-sandbox-mode child)))
            (should (equal
                     '(("Bash" :pattern "npx test*" :action allow))
                     (mevedel-session-permission-rules child)))
            (should (equal
                     '((:path "/tmp/external-input" :access read))
                     (mevedel-session-resource-grants child)))
            (should-not (eq (mevedel-session-permission-rules session)
                            (mevedel-session-permission-rules child)))
            (should-not (eq (mevedel-session-resource-grants session)
                            (mevedel-session-resource-grants child)))
            (should-not (eq (mevedel-session-hook-rules session)
                            (mevedel-session-hook-rules child)))
            (should (equal (mevedel-session-hook-rules session)
                           (mevedel-session-hook-rules child)))
            (should (equal (mevedel-session-session-id session)
                           (mevedel-session-forked-from-session-id child)))
            (should (= 2 (mevedel-session-forked-from-turn child)))
            (should (equal "fixture-fork"
                           (mevedel-session-forked-from-fork-point-id child)))
            (should (eq 'conversation
                        (plist-get child-sidecar :fork-type)))
            (with-current-buffer child-buffer
              (should (string-match-p "Conversation Fork"
                                      (buffer-string)))
              (should (string-match-p "Files were not restored"
                                      (buffer-string)))
              (should-not (string-match-p "Future segment prompt"
                                          (buffer-string))))
            (should (equal "current checkout\n"
                           (mevedel-session-artifacts--file-text
                            source-file)))
            (with-current-buffer (plist-get fixture :buffer)
              (should (equal source-text (buffer-string)))
              (should (eq session mevedel--session)))
            (should (equal (plist-get fixture :parent-sidecar-text)
                           (mevedel-session-artifacts--file-text
                            (mevedel-session-artifacts-sidecar-path
                             (plist-get fixture :parent-path)))))
            (should (equal source-lock
                           (mevedel-session-artifacts--file-text
                            (plist-get fixture :parent-lock))))
            (should (equal source-state
                           (mevedel-session-codec-serialize session)))))
      (when (buffer-live-p child-buffer)
        (let ((view (buffer-local-value 'mevedel--view-buffer child-buffer)))
          (with-current-buffer child-buffer
            (set-buffer-modified-p nil))
          (when (buffer-live-p view)
            (kill-buffer view)))
        (when (buffer-live-p child-buffer)
          (kill-buffer child-buffer)))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))


(mevedel-deftest mevedel-session-fork--retarget-worktree-state ()
  ,test
  (test)
  :doc "retargets valid local paths and drops malformed copied path state"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-retarget-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-retarget-child-" t)))
         (session
          (mevedel-session--create
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
               (mevedel-session-fork--retarget-worktree-state
                session)))
          (should (= 2 (length dropped)))
          (should
           (equal
            (file-name-concat worktree-root "local.el")
            (caar (cdar (mevedel-session-file-snapshots session)))))
          (should
           (equal
            (list (file-name-concat worktree-root "grant.el")
                  "/external/grant.el")
            (mapcar
             (lambda (grant) (plist-get grant :path))
             (mevedel-session-resource-grants session))))
          (should
           (equal
            (list (file-name-concat worktree-root "rule.el")
                  "/external/rule.el"
                  nil)
            (mapcar
             (lambda (rule) (plist-get (cdr rule) :path))
             (mevedel-session-permission-rules session)))))
      (delete-directory source-root t)
      (delete-directory worktree-root t))))


(mevedel-deftest mevedel-session-fork--retarget-worktree-roots ()
  ,test
  (test)
  :doc "retargets repository roots, preserves external roots, and drops malformed roots"
  (let* ((source
          (file-name-as-directory
           (make-temp-file "mevedel-roots-source-" t)))
         (worktree
          (file-name-as-directory
           (make-temp-file "mevedel-roots-worktree-" t)))
         (session
          (mevedel-session--create
           :worktree-source-root source
           :worktree-directory worktree))
         (result
          (mevedel-session-fork--retarget-worktree-roots
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


(mevedel-deftest mevedel-session-fork--assert-worktree-target ()
  ,test
  (test)
  :doc "accepts local targets and rejects escape through a symlink"
  (let ((worktree
         (file-name-as-directory
          (make-temp-file "mevedel-worktree-safe-" t)))
        (external
         (file-name-as-directory
          (make-temp-file "mevedel-worktree-escape-" t))))
    (unwind-protect
        (progn
          (should-not
           (mevedel-session-fork--assert-worktree-target
            worktree (file-name-concat worktree "safe.el")))
          (make-symbolic-link external
                              (file-name-concat worktree "escape"))
          (should-error
           (mevedel-session-fork--assert-worktree-target
            worktree
            (file-name-concat worktree "escape" "unsafe.el"))))
      (delete-directory worktree t)
      (delete-directory external t))))


(mevedel-deftest mevedel-session-fork--restore-worktree-files ()
  ,test
  (test)
  :doc "continues after individual backup failures without touching Source"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-restore-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-restore-child-" t)))
         (save-path
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-restore-session-" t)))
         (external-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-restore-external-" t)))
         (good-source (file-name-concat source-root "good.el"))
         (bad-source (file-name-concat source-root "bad.el"))
         (external-file (file-name-concat external-root "shared.el"))
         (good-target (file-name-concat worktree-root "good.el"))
         (bad-target (file-name-concat worktree-root "bad.el"))
         (source
          (mevedel-session--create
           :authority-mode 'pid-lock
           :save-path save-path
           :file-snapshots
           `((1 . ((,good-source . (:backup-name "good@v1"))
                   (,bad-source . (:backup-name "missing@v1"))
                   (,external-file . (:backup-name "external@v1")))))))
         (child
          (mevedel-session--create
           :worktree-source-root source-root
           :worktree-directory worktree-root)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "file-history") t)
          (write-region "captured good\n" nil
                        (mevedel-session-artifacts-backup-path
                         save-path "good@v1")
                        nil 'silent)
          (write-region "source good\n" nil good-source nil 'silent)
          (write-region "source bad\n" nil bad-source nil 'silent)
          (write-region "shared external\n" nil external-file nil 'silent)
          (write-region "HEAD bad\n" nil bad-target nil 'silent)
          (let ((report
                 (mevedel-session-fork--restore-worktree-files
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
                           (mevedel-session-artifacts--file-text
                            good-target)))
            (should (equal "HEAD bad\n"
                           (mevedel-session-artifacts--file-text
                            bad-target)))
            (should (equal "source good\n"
                           (mevedel-session-artifacts--file-text
                            good-source)))
            (should (equal "source bad\n"
                           (mevedel-session-artifacts--file-text
                            bad-source)))
            (should (equal "shared external\n"
                           (mevedel-session-artifacts--file-text
                            external-file)))))
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (delete-directory save-path t)
      (delete-directory external-root t)))
  :doc "validates the whole plan before writing any target"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-invalid-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-invalid-child-" t)))
         (save-path
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-invalid-session-" t)))
         (valid-source (file-name-concat source-root "valid.el"))
         (valid-target (file-name-concat worktree-root "valid.el"))
         (source
          (mevedel-session--create
           :authority-mode 'pid-lock
           :save-path save-path
           :file-snapshots
           `((1 . ((,valid-source . (:backup-name "valid@v1"))
                   ("relative.el" . (:backup-name "bad@v1")))))))
         (child
          (mevedel-session--create
           :worktree-source-root source-root
           :worktree-directory worktree-root)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "file-history") t)
          (write-region "captured\n" nil
                        (mevedel-session-artifacts-backup-path
                         save-path "valid@v1")
                        nil 'silent)
          (write-region "HEAD\n" nil valid-target nil 'silent)
          (should-error
           (mevedel-session-fork--restore-worktree-files
            source child 1))
          (should (equal "HEAD\n"
                         (mevedel-session-artifacts--file-text
                          valid-target))))
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (delete-directory save-path t)))
  :doc "rejects an unavailable required history store"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-store-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-store-child-" t)))
         (save-path
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-store-session-" t)))
         (source
          (mevedel-session--create :authority-mode 'pid-lock :save-path save-path))
         (child
          (mevedel-session--create
           :worktree-source-root source-root
           :worktree-directory worktree-root)))
    (unwind-protect
        (should-error
         (mevedel-session-fork--restore-worktree-files
          source child 1))
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (delete-directory save-path t)))
  :doc "portable restoration uses committed backups without a fixed cache"
  (let* ((source-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-published-source-" t)))
         (worktree-root
          (file-name-as-directory
           (make-temp-file "mevedel-worktree-published-child-" t)))
         (workspace
          (mevedel-workspace-get-or-create
           'project "worktree-published" source-root "worktree-published"))
         (source (mevedel-session-create "source" workspace source-root))
         (session-dir
          (file-name-as-directory
           (file-name-concat source-root ".mevedel" "sessions" "source")))
         (source-file (file-name-concat source-root "tracked.el"))
         (target-file (file-name-concat worktree-root "tracked.el"))
         (backup-name "tracked@v1")
         (backup-path
          (file-name-concat session-dir "file-history" backup-name))
         (sidecar (file-name-concat session-dir "session.meta.el"))
         (child
          (mevedel-session--create
           :worktree-source-root source-root
           :worktree-directory worktree-root))
         (mevedel-session-durability--client-id (make-string 64 ?b)))
    (setf (mevedel-session-session-id source) "source"
          (mevedel-session-save-path source) session-dir
          (mevedel-session-file-snapshots source)
          `((1 . ((,source-file . (:backup-name ,backup-name
                                  :version 1))))))
    (make-directory session-dir t)
    (unwind-protect
        (progn
          (should
           (mevedel-session-durability-lease-acquire
            session-dir "*worktree-published*" source))
          (mevedel-session-publication-publish
           source
           (list (list :path backup-path :content "published backup")
                 (list :path sidecar :content "sidecar" :commit-marker t)))
          (mevedel-session-durability-lease-release session-dir source)
          (write-region "poison" nil backup-path nil 'silent)
          (let ((report
                 (mevedel-session-fork--restore-worktree-files
                  source child 1)))
            (should (= 1 (plist-get report :restored)))
            (should-not (plist-get report :unrestored)))
          (should (equal "published backup"
                         (mevedel-session-artifacts-read-file-raw target-file)))
          (delete-directory (file-name-directory backup-path) t)
          (write-region "reset" nil target-file nil 'silent)
          (let ((report
                 (mevedel-session-fork--restore-worktree-files
                  source child 1)))
            (should (= 1 (plist-get report :restored)))
            (should-not (plist-get report :unrestored)))
          (should (equal "published backup"
                         (mevedel-session-artifacts-read-file-raw target-file))))
      (when (mevedel-session-durability-lease-owned-p source)
        (mevedel-session-durability-lease-release session-dir source))
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-fork--worktree-fork-disclosure ()
  ,test
  (test)
  :doc "names every partial, malformed, and external restoration gap"
  (let ((session
         (mevedel-session--create
          :forked-from-session-id "source-id"
          :worktree-directory "/repo/.worktrees/main-fork-1/"
          :worktree-branch "worktree/main-fork-1"
          :worktree-base-commit "0123456789abcdef")))
    (let ((text
           (mevedel-session-fork--worktree-fork-disclosure
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
  (let* ((target (mevedel-execution-target-create
                  "/ssh:alias:/srv/repo/"))
         (session
          (mevedel-session--create
           :execution-target target
           :forked-from-session-id "source-id"
           :worktree-directory "/ssh:alias:/srv/repo/.worktrees/fork/"
           :worktree-branch "worktree/fork"
           :worktree-base-commit "0123456789abcdef"))
         (text
          (mevedel-session-fork--worktree-fork-disclosure
           session
           '(:restored 0
             :unrestored ((:path "/ssh:alias:/srv/repo/missing.el"
                           :reason "backup unavailable"))
             :external ("/ssh:alias:/srv/shared.el"
                        "/ssh:foreign:/srv/private.el")))))
    (should-not (string-match-p "/ssh:alias:" text))
    (should-not (string-match-p "/ssh:foreign:" text))
    (should (string-match-p "/srv/repo/.worktrees/fork/" text))
    (should (string-match-p "/srv/repo/missing.el" text))
    (should (string-match-p "/srv/shared.el" text))
    (should (string-match-p "<path outside session target>" text))))


(mevedel-deftest mevedel-session-fork-worktree-fork ()
  ,test
  (test)
  :doc "publishes captured repository state in an isolated linked worktree"
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready))
        child-buffer
        lifecycle-sources)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-root
                (file-name-as-directory
                 (mevedel-session-working-directory session)))
               (source-file (file-name-concat source-root "current.txt"))
               (untracked-file
                (file-name-concat source-root "source-only.txt"))
               (backup-name "current@v1"))
          (test-mevedel-session-persistence--git source-root "init")
          (test-mevedel-session-persistence--git
           source-root "config" "user.email" "mevedel@example.invalid")
          (test-mevedel-session-persistence--git
           source-root "config" "user.name" "Mevedel Test")
          (write-region "HEAD state\n" nil source-file nil 'silent)
          (test-mevedel-session-persistence--git
           source-root "add" "current.txt")
          (test-mevedel-session-persistence--git
           source-root "commit" "-m" "base")
          (let ((base-commit
                 (test-mevedel-session-persistence--git
                  source-root "rev-parse" "HEAD")))
            (write-region "captured state\n" nil
                          (mevedel-session-artifacts-backup-path
                           (plist-get fixture :parent-path) backup-name)
                          nil 'silent)
            (setf
             (mevedel-session-file-snapshots session)
             `((1 . ((,source-file
                      . (:backup-name ,backup-name :version 1)))))
             (mevedel-session-resource-grants session)
             `((:path ,source-file :access write))
            (mevedel-session-permission-rules session)
             `(("Write" :path ,source-file :action allow))
             (mevedel-session-permission-mode session)
             'full-auto
             (mevedel-session-sandbox-mode session)
             'required
             (mevedel-session-model-provider session)
             "test-backend:test-model")
            (with-current-buffer (plist-get fixture :buffer)
              (setq-local
               mevedel-workspace-additional-roots
               `((,source-root
                  ,(file-name-concat source-root "nested")
                  "/tmp/shared"))))
            (mevedel-session-codec-write
             (mevedel-session-artifacts-sidecar-path
              (plist-get fixture :parent-path))
             (mevedel-session-artifacts-build-sidecar
              session (plist-get fixture :buffer)))
            (let ((source-state
                   (mevedel-session-codec-serialize session))
                  (source-sidecar
                   (mevedel-session-artifacts--file-text
                    (mevedel-session-artifacts-sidecar-path
                     (plist-get fixture :parent-path))))
                  (source-lock
                   (mevedel-session-artifacts--file-text
                    (plist-get fixture :parent-lock))))
              (write-region "dirty Source state\n" nil
                            source-file nil 'silent)
              (write-region "untracked Source file\n" nil
                            untracked-file nil 'silent)
              (cl-letf
                  (((symbol-function 'mevedel--run-session-start-hooks)
                    (lambda (source)
                      (push source lifecycle-sources)))
                   ((symbol-function 'mevedel-model-apply-session-policy)
                    #'ignore))
                (setq child-buffer
                      (mevedel-session-fork-worktree-fork
                       (plist-get fixture :buffer)
                       '(:fork-point-id "fixture-fork"))))
              (should (buffer-live-p child-buffer))
              (should (equal '("fork") lifecycle-sources))
              (let* ((child
                      (buffer-local-value 'mevedel--session child-buffer))
                     (worktree
                      (mevedel-session-worktree-directory child))
                     (child-file
                      (file-name-concat worktree "current.txt")))
                (should (string= "main · worktree 1"
                                 (mevedel-session-name child)))
                (should (eq 'worktree
                            (mevedel-session-fork-type child)))
                (should (equal source-root
                               (mevedel-session-worktree-source-root child)))
                (should (eq 'full-auto
                            (mevedel-session-permission-mode child)))
                (should (eq 'required
                            (mevedel-session-sandbox-mode child)))
                (should (equal worktree
                               (mevedel-session-working-directory child)))
                (should (equal "worktree/main-fork-1"
                               (mevedel-session-worktree-branch child)))
                (should (equal base-commit
                               (mevedel-session-worktree-base-commit child)))
                (should (equal base-commit
                               (test-mevedel-session-persistence--git
                                worktree "rev-parse" "HEAD")))
                (should (equal "captured state\n"
                               (mevedel-session-artifacts--file-text
                                child-file)))
                (should-not
                 (file-exists-p
                  (file-name-concat worktree "source-only.txt")))
                (should
                 (equal child-file
                        (caar
                         (cdar
                          (mevedel-session-file-snapshots child)))))
                (should
                 (equal child-file
                        (plist-get
                         (car (mevedel-session-resource-grants child))
                         :path)))
                (with-current-buffer child-buffer
                  (should
                   (equal
                    `((,source-root
                       ,(file-name-concat worktree "nested")
                       "/tmp/shared"))
                    mevedel-workspace-additional-roots))
                  (should (string-match-p "Worktree Fork"
                                          (buffer-string)))
                  (should (string-match-p
                           (regexp-quote worktree)
                           (buffer-string)))
                  (should (string-match-p
                           (regexp-quote base-commit)
                           (buffer-string)))
                  (should (string-match-p
                           "Captured repository files restored: 1"
                           (buffer-string))))
                (should (equal "dirty Source state\n"
                               (mevedel-session-artifacts--file-text
                                source-file)))
                (should (equal "untracked Source file\n"
                               (mevedel-session-artifacts--file-text
                                untracked-file)))
                (should (equal source-state
                               (mevedel-session-codec-serialize
                                session)))
                (should
                 (equal source-sidecar
                        (mevedel-session-artifacts--file-text
                         (mevedel-session-artifacts-sidecar-path
                          (plist-get fixture :parent-path)))))
                (should
                 (equal source-lock
                        (mevedel-session-artifacts--file-text
                         (plist-get fixture :parent-lock))))))))
      (when (buffer-live-p child-buffer)
        (let ((view (buffer-local-value 'mevedel--view-buffer child-buffer)))
          (with-current-buffer child-buffer
            (set-buffer-modified-p nil))
          (when (buffer-live-p view)
            (kill-buffer view)))
        (when (buffer-live-p child-buffer)
          (kill-buffer child-buffer)))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "retains one reserved Git target after staging failure and blocks retry"
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready)))
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source-root
                (file-name-as-directory
                 (mevedel-session-working-directory session))))
          (test-mevedel-session-persistence--git source-root "init")
          (test-mevedel-session-persistence--git
           source-root "config" "user.email" "mevedel@example.invalid")
          (test-mevedel-session-persistence--git
           source-root "config" "user.name" "Mevedel Test")
          (write-region "base\n" nil
                        (file-name-concat source-root "file.txt")
                        nil 'silent)
          (test-mevedel-session-persistence--git source-root "add" "file.txt")
          (test-mevedel-session-persistence--git
           source-root "commit" "-m" "base")
          (let* ((reservation
                  (mevedel-worktree-fork-reservation session))
                 (target
                  (list :fork-point-id "fixture-fork"
                        :worktree-reservation reservation))
                 (directory (plist-get reservation :directory))
                 (branch (plist-get reservation :branch))
                 (source-state
                  (mevedel-session-codec-serialize session))
                 (first-error
                  (cl-letf
                      (((symbol-function
                         'mevedel-session-fork--stage-fork)
                        (lambda (&rest _)
                          (error "Injected staging failure"))))
                    (should-error
                     (mevedel-session-fork-worktree-fork
                      (plist-get fixture :buffer) target)))))
            (should (file-directory-p directory))
            (should
             (string-match-p
              (regexp-quote branch)
              (test-mevedel-session-persistence--git
               source-root "branch" "--list" branch)))
            (should (string-match-p
                     (regexp-quote directory)
                     (error-message-string first-error)))
            (should (string-match-p
                     "git -C.*worktree remove --force"
                     (error-message-string first-error)))
            (should (equal source-state
                           (mevedel-session-codec-serialize session)))
            (cl-letf
                (((symbol-function 'mevedel-worktree-fork-reservation)
                  (lambda (&rest _)
                    (ert-fail "Retry allocated another reservation"))))
              (let ((retry-error
                     (should-error
                      (mevedel-session-fork-worktree-fork
                       (plist-get fixture :buffer) target)
                      :type 'user-error)))
                (should (string-match-p
                         (regexp-quote branch)
                         (error-message-string retry-error)))))
            (should-not
             (file-exists-p
              (file-name-concat source-root ".worktrees" "main-fork-2")))))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))


(mevedel-deftest mevedel-session-fork--worktree-fork-retained-error ()
  ,test
  (test)
  :doc "reports retained remote worktree paths in the target-native domain"
  (let* ((target (mevedel-execution-target-create
                  "/ssh:alias:/srv/repo/"))
         (session (mevedel-session--create :execution-target target))
         (text
          (mevedel-session-fork--worktree-fork-retained-error
           session '(error "publish failed")
           '(:branch "worktree/fork"
             :directory "/ssh:alias:/srv/repo/.worktrees/fork/"
             :cleanup-command "git worktree remove"))))
    (should-not (string-match-p "/ssh:alias:" text))
    (should (string-match-p "/srv/repo/.worktrees/fork/" text))))


(mevedel-deftest mevedel-session-fork--commit-remote-rename (:quiet t)
  ,test
  (test)
  :doc "moves the owned lease and commits renamed metadata through one head"
  (let* ((host "rename-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rename-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?e))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host local-root "rename transcript\n"))
                       (parent
                        (file-name-directory
                         (directory-file-name session-dir)))
                       (new-id "renamed-remote-restore")
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat parent new-id)))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target session))
             t mevedel-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*remote-rename*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer " *remote-rename-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "rename transcript\n"))
                  (let ((generation
                         (plist-get (mevedel-session-lease session)
                                    :generation))
                        (head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (should-not
                     (mevedel-session-fork--commit-remote-rename
                      session buffer "renamed" new-id new-save-path))
                    (should-not (file-directory-p session-dir))
                    (should (file-directory-p new-save-path))
                    (should
                     (= generation
                        (plist-get (mevedel-session-lease session)
                                   :generation)))
                    (should-not
                     (equal head-before
                            (plist-get (mevedel-session-publication session)
                                       :head)))
                    (should
                     (equal "rename transcript\n"
                            (mevedel-session-artifacts-read-artifact
                             session "segment-0001.chat.org" t)))
                    (let ((sidecar
                           (with-temp-buffer
                             (insert
                              (mevedel-session-artifacts-read-artifact
                               session "session.meta.el" t))
                             (goto-char (point-min))
                             (read (current-buffer)))))
                      (should (equal "renamed"
                                     (plist-get sidecar :session-name)))
                      (should (equal new-id
                                     (plist-get sidecar :session-id))))))
              (when session
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path session) session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "rolls the move and in-memory paths back after a pre-CAS failure"
  (let* ((host "rename-rollback")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rename-rollback-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?f))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host local-root "rollback transcript\n"))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat
                          (file-name-directory
                           (directory-file-name session-dir))
                          "renamed-rollback")))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target session))
             t mevedel-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*remote-rename-rollback*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer
                         " *remote-rename-rollback-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "rollback transcript\n"))
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head))
                        (publish-artifact
                         (symbol-function
                          'mevedel-session-publication--publish-artifact)))
                    (cl-letf
                        (((symbol-function
                           'mevedel-session-publication--publish-artifact)
                          (lambda (artifact)
                            (ignore artifact publish-artifact)
                            (signal 'file-error
                                    '("Injected Rename publication failure")))))
                      (should-error
                       (mevedel-session-fork--commit-remote-rename
                        session buffer "renamed" "renamed-rollback"
                        new-save-path)
                       :type 'file-error))
                    (should (file-directory-p session-dir))
                    (should-not (file-directory-p new-save-path))
                    (should (equal session-dir
                                   (mevedel-session-save-path session)))
                    (should (equal "main" (mevedel-session-name session)))
                    (should (equal segment
                                   (buffer-local-value
                                    'buffer-file-name buffer)))
                    (should (equal head-before
                                   (plist-get
                                    (mevedel-session-publication session)
                                    :head)))
                    (should-not
                     (mevedel-session-pending-publication session))
                    (should
                     (mevedel-session-durability-lease-owned-p session))))
              (when session
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path session) session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-rename-session (:quiet t)
  ,test
  (test)
  :doc "renames the session-name field and the buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "Hi\n")
                (mevedel-session-artifacts-save session buf)
                (let* ((old-save-path (mevedel-session-save-path session))
                       (artifact-directory
                        (file-name-concat old-save-path "tool-results"))
                       (mevedel-sandbox-mode 'off)
                       initial terminal execution-id)
                  (mevedel-execution-start-bash
                   (lambda (value) (setq initial value))
                   :session session :data-buffer buf :owner "agent-a"
                   :owner-context session
                   :command
                   '("sh" "-c" "printf before; sleep 1; printf after")
                   :workdir tempdir :writable-roots (list tempdir)
                   :artifact-directory artifact-directory
                   :yield-time-ms 10)
                  (with-timeout (2 (error "Execution did not yield"))
                    (while (null initial)
                      (accept-process-output nil 0.02)))
                  (setq execution-id
                        (plist-get (plist-get initial :facts) :execution-id))
                  (mevedel-rename-session "alt-permissions")
                  (should (equal "alt-permissions"
                                 (mevedel-session-name session)))
                  ;; Old directory gone, new directory exists.
                  (should-not (file-directory-p old-save-path))
                  (should (file-directory-p
                           (mevedel-session-save-path session)))
                  ;; New directory name reflects the new session-name.
                  (should (string-prefix-p
                           "alt-permissions-"
                           (file-name-nondirectory
                            (directory-file-name
                             (mevedel-session-save-path session)))))
                  ;; Buffer renamed per convention.
                  (should (string-match-p
                           "\\`\\*mevedel:alt-permissions@"
                           (buffer-name buf)))
                  (mevedel-execution-observe
                   session "agent-a" execution-id
                   (lambda (value) (setq terminal value))
                   :wait-ms 5000)
                  (with-timeout (6 (error "Renamed execution did not finish"))
                    (while (null terminal)
                      (accept-process-output nil 0.02)))
                  (should (= 0
                             (plist-get (plist-get terminal :facts)
                                        :exit-code)))
                  (let ((artifact
                         (plist-get (plist-get terminal :facts) :output-path)))
                    (should (string-prefix-p "artifact://" artifact))
                    (should
                     (equal "beforeafter"
                            (mevedel-resource-execute
                             (mevedel-resource-prepare
                              'read artifact (list :session session))
                             (lambda (path _address)
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (buffer-string)))))))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "publishes the renamed sidecar through the critical seam"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *rename-publication*"))
           (publish-function
            (symbol-function 'mevedel-session-artifacts-publish-text))
           published)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "transcript\n")
            (mevedel-session-artifacts-save session buffer)
            (cl-letf
                (((symbol-function
                   'mevedel-session-artifacts-publish-text)
                  (lambda (actual-session path content &optional coding)
                    (push path published)
                    (funcall publish-function
                             actual-session path content coding))))
              (mevedel-rename-session "renamed"))
            (should
             (equal
              (list
               (mevedel-session-artifacts-sidecar-path
                (mevedel-session-save-path session)))
              published)))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry)))))


;;
;;; Pure helper: --agent-files-for-segments

(mevedel-deftest mevedel-session-fork--agent-files-for-segments
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
           (mevedel-session-fork--agent-files-for-segments
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
           (mevedel-session-fork--agent-files-for-segments
            index entries 1 2)))
      (should (= 1 (length result)))
      (should (equal "good--1" (caar result)))))

  :doc "empty input returns empty"
  (should (null
           (mevedel-session-fork--agent-files-for-segments
            nil nil 1 1)))

  :doc "picked-cum-turn before all prompts excludes everything"
  (let ((index '((1 . ((:cum-turn 1)))))
        (entries
         '(("a--1" :parent-turn 1 :path "agents/a.chat.org"))))
    (should (null
             (mevedel-session-fork--agent-files-for-segments
              index entries 1 0)))))

(provide 'test-mevedel-session-fork)
;;; test-mevedel-session-fork.el ends here
