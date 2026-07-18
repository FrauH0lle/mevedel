;;; test-mevedel-session-persistence.el --- Tests for session persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-persistence' (Phase 1: serialization).

;;; Code:

(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-presets)
(require 'mevedel-skills-ui)
(require 'mevedel-reminders)
(require 'mevedel-view)
(require 'mevedel-view-history)
(require 'mevedel-chat)
(require 'mevedel-hooks)
(require 'mevedel-permission-log)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-repair)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel--preset)
(defvar gptel-system-prompt)
(defvar so-long-predicate)
(declare-function org-entry-delete "org" (pom property))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (pom property value))


;;
;;; Helpers

(defun test-mevedel-session-persistence--make-workspace (root)
  "Build a workspace struct registered in the global registry.
ROOT is a temporary directory owned and cleaned up by the caller."
  (mevedel-workspace-clear-registry)
  (make-directory (file-name-concat root "packages" "api") t)
  (mevedel-workspace-get-or-create
   'project "test-id" root (file-name-nondirectory
                            (directory-file-name root))))

(defun test-mevedel-session-persistence--make-session (root)
  "Build a populated session for ROOT in round-trip cases."
  (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
         (root (mevedel-workspace-root workspace))
         (session   (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-working-directory session)
          (file-name-as-directory
           (file-name-concat root "packages" "api")))
    (setf (mevedel-session-permission-mode session) 'ask)
    (setf (mevedel-session-permission-rules session)
          '(("Read"  :path "/tmp/foo/**" :action allow)
            ("Bash"  :pattern "git log*" :action allow)
            ("Write" :path "/tmp/bar"    :action deny)))
    (setf (mevedel-session-resource-grants session)
          '((:path "/tmp/exact-read" :access read)
            (:path "/tmp/exact-write" :access write)))
    (setf (mevedel-session-turn-count session) 5)
    (setf (mevedel-session-last-task-write-turn session) 4)
    (setf (mevedel-session-task-status-notes session)
          '((nil :note "Main status" :updated-turn 4
                 :updated-at "2026-04-23T18:20:00+0200")
            ("main" :note "Agent status" :updated-turn 4
             :updated-at "2026-04-23T18:21:00+0200")))
    (setf (mevedel-session-skills-snapshot session)
          '(("alpha" . "Alpha helper")))
    (setf (mevedel-session-session-id session) "main-2026-04-23T14-30-a9f2")
    (setf (mevedel-session-save-path session)
          (file-name-as-directory
           (file-name-concat
            root ".mevedel" "sessions" "main-2026-04-23T14-30-a9f2")))
    (setf (mevedel-session-created-at session) "2026-04-23T14-30-00")
    (setf (mevedel-session-updated-at session) "2026-04-23T18-22-11")
    (setf (mevedel-session-current-segment session) 2)
    (setf (mevedel-session-prompt-index session)
          '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                   :pos 142 :preview "Refactor X" :timestamp "...")))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/tmp/foo.el"
                   . (:backup-name "abc@v1" :version 1
                      :backup-time "..." :file-mtime "..."))))))
    (setf (mevedel-session-tasks session)
          (list (mevedel-task--create
                 :id 1 :subject "Plan refactor" :status 'completed
                 :completed-turn 3
                 :owner nil :blocks nil :blocked-by nil :metadata nil)
                (mevedel-task--create
                 :id 2 :subject "Implement permission chain"
                 :description "Replace the deprecated specifier handling"
                 :status 'in-progress
                 :owner "main" :blocks '(1) :blocked-by nil
                 :metadata '(:priority high))))
    session))

(defun test-mevedel-session-persistence--complete-sidecar (plist)
  "Return a current complete sidecar with PLIST values overriding defaults."
  (let ((sidecar
         (list :version (mevedel-version)
               :session-id "test-session"
               :session-name "x"
               :workspace nil
               :working-directory nil
               :created-at "created"
               :updated-at "updated"
               :current-segment 1
               :total-turn-count 0
               :last-task-write-turn nil
               :task-status-notes nil
               :first-user-message nil
               :latest-user-message nil
               :forked-from-session-id nil
               :forked-from-turn nil
               :permission-mode 'ask
               :permission-rules nil
               :resource-grants nil
               :preset-name nil
               :preset-settings nil
               :last-observed-date "2026-01-01"
               :agent-types-snapshot :uninitialized
               :skills-snapshot :uninitialized
               :additional-roots nil
               :tasks nil
               :prompt-index nil
               :file-snapshots nil
               :agent-transcripts nil
               :plan-metadata nil
               :goal nil
               :goal-handoff nil
               :messages nil)))
    (while plist
      (setq sidecar (plist-put sidecar (pop plist) (pop plist))))
    sidecar))


;;
;;; Workspace round-trip

(mevedel-deftest mevedel-session-persistence--workspace-to-plist ()
  ,test
  (test)
  :doc "captures all four identity fields"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
               (plist (mevedel-session-persistence--workspace-to-plist workspace)))
          (should (eq 'project    (plist-get plist :type)))
          (should (equal "test-id" (plist-get plist :id)))
          (should (equal root (plist-get plist :root)))
          (should (equal (file-name-nondirectory
                          (directory-file-name root))
                         (plist-get plist :name))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "returns nil for a nil workspace"
  (should (null (mevedel-session-persistence--workspace-to-plist nil))))

(mevedel-deftest mevedel-session-persistence--workspace-from-plist ()
  ,test
  (test)
  :doc "round-trips through the registry"
  (mevedel-workspace-clear-registry)
  (let* ((source     (mevedel-workspace-get-or-create
                      'project "abc" "/tmp/p" "p"))
         (plist      (mevedel-session-persistence--workspace-to-plist source))
         (recovered  (mevedel-session-persistence--workspace-from-plist plist)))
    (should (eq source recovered)))
  :doc "registers from saved tuple when registry is empty"
  (mevedel-workspace-clear-registry)
  (let* ((plist (list :type 'project :id "xyz"
                      :root "/tmp/q" :name "q"))
         (recovered (mevedel-session-persistence--workspace-from-plist plist)))
    (should (eq 'project (mevedel-workspace-type recovered)))
    (should (equal "xyz" (mevedel-workspace-id recovered)))
    (should (equal (expand-file-name "/tmp/q")
                   (mevedel-workspace-root recovered))))
  :doc "expands tilde project paths"
  (mevedel-workspace-clear-registry)
  (let* ((root "~/mevedel-session-root/")
         (expected (expand-file-name root))
         (plist (list :type 'project :id root :root root :name "home-root"))
         (recovered (mevedel-session-persistence--workspace-from-plist plist)))
    (should (equal expected (mevedel-workspace-id recovered)))
    (should (equal expected (mevedel-workspace-root recovered))))
  :doc "returns nil for nil plist"
  (should (null (mevedel-session-persistence--workspace-from-plist nil))))


;;
;;; Permission rule hygiene

(mevedel-deftest mevedel-session-persistence--filter-permission-rules ()
  ,test
  (test)
  :doc "keeps allow / deny / ask rules"
  (let ((rules '(("Read" :path "/x" :action allow)
                 ("Bash" :pattern "rm" :action deny)
                 ("Write" :path "/y" :action ask))))
    (should (equal rules
                   (mevedel-session-persistence--filter-permission-rules rules))))
  :doc "drops rules with unknown actions"
  (let* ((rules '(("Read"  :path "/x" :action allow)
                  ("Write" :path "/y" :action future-action)
                  ("Bash"  :pattern "ls" :action allow)))
         (filtered (mevedel-session-persistence--filter-permission-rules rules)))
    (should (= 2 (length filtered)))
    (should (equal "Read" (caar filtered)))
    (should (equal "Bash" (caadr filtered))))
  :doc "drops malformed entries"
  (let ((rules '(("Read" :path "/x" :action allow)
                 nil
                 "not a rule"
                 ("Bash" :pattern "echo" :action allow))))
    (should (= 2 (length
                  (mevedel-session-persistence--filter-permission-rules rules))))))

(mevedel-deftest mevedel-session-persistence--filter-resource-grants ()
  ,test
  (test)
  :doc "keeps exact read and write grants"
  (let ((grants '((:path "/tmp/read" :access read)
                  (:path "/tmp/write" :access write))))
    (should (equal grants
                   (mevedel-session-persistence--filter-resource-grants
                    grants))))
  :doc "drops malformed grants and unknown access levels"
  (should
   (equal '((:path "/tmp/read" :access read))
          (mevedel-session-persistence--filter-resource-grants
           '((:path "/tmp/read" :access read)
             (:path "/tmp/future" :access execute)
             (:access write)
             "not a grant")))))


;;
;;; Goal round-trip

(mevedel-deftest mevedel-session-persistence--goal-to-plist ()
  ,test
  (test)
  :doc "captures lifecycle position, cycle metadata, and blocked reason"
  (let* ((goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'blocked
                :phase 'reviewing :approval-policy 'supervised
                :owner-session "main" :cycle 2
                :execution-home '(:kind current :directory "/tmp/"
                                  :session-id "main" :locked t)
                :implementation-context 'full
                :cycles '((:cycle 1) (:cycle 2))
                :token-budget 1000 :token-usage 345
                :continuation-key "key"
                :checkpoint
                '(:phase reviewing :cycle 2 :input "Exact review"
                  :workload review :provider "openai:model" :effort high
                  :plan-reference nil
                  :attempt 1 :attempt-id "g1/2/reviewing/1"
                  :retry-count 0 :dispatch-state started :request-started t
                  :last-settled-boundary nil :prepared-at "2026-01-01T00:00:00Z")
                :reason "Need an API credential."))
         (plist (mevedel-session-persistence--goal-to-plist goal)))
    (should (equal "g1" (plist-get plist :id)))
    (should (eq 'blocked (plist-get plist :status)))
    (should (= 2 (plist-get plist :cycle)))
    (should (= 1000 (plist-get plist :token-budget)))
    (should (= 345 (plist-get plist :token-usage)))
    (should (eq 'current
                (plist-get (plist-get plist :execution-home) :kind)))
    (should (eq t (plist-get (plist-get plist :execution-home) :locked)))
    (should (eq 'full (plist-get plist :implementation-context)))
    (should (equal "Exact review"
                   (plist-get (plist-get plist :checkpoint) :input)))
    (should (equal "Need an API credential." (plist-get plist :reason)))))

(mevedel-deftest mevedel-session-persistence--goal-from-plist ()
  ,test
  (test)
  :doc "rebuilds a Goal without sharing mutable recovery records"
  (let* ((cycles '((:cycle 1 :plan "cycle-001-plan.md")))
         (checkpoint
          '(:phase planning :cycle 1 :input "Exact planning"
            :workload planning :provider "p:m" :effort nil
            :plan-reference nil
            :attempt 1 :attempt-id "g1/1/planning/1"
            :retry-count 0 :dispatch-state settled :request-started t
            :last-settled-boundary nil :prepared-at "2026-01-01T00:00:00Z"))
         (goal (mevedel-session-persistence--goal-from-plist
                (list :id "g1" :objective "Ship" :status 'active
                      :phase 'planning :approval-policy 'supervised
                      :owner-session "main"
                      :execution-home '(:kind current :directory "/tmp/"
                                        :session-id "main" :locked t)
                      :implementation-context 'full
                      :cycle 1 :cycles cycles :checkpoint checkpoint
                      :token-budget 1000 :token-usage 25
                      :continuation-key "key"))))
    (should (mevedel-goal-p goal))
    (should (equal "Ship" (mevedel-goal-objective goal)))
    (should (equal cycles (mevedel-goal-cycles goal)))
    (should-not (eq cycles (mevedel-goal-cycles goal)))
    (should (equal checkpoint (mevedel-goal-checkpoint goal)))
    (should-not (eq checkpoint (mevedel-goal-checkpoint goal)))
    (should (= 1000 (mevedel-goal-token-budget goal)))
    (should (= 25 (mevedel-goal-token-usage goal)))
    (should (eq 'current
                (plist-get (mevedel-goal-execution-home goal) :kind)))
    (should (eq t
                (plist-get (mevedel-goal-execution-home goal) :locked)))
    (should (eq 'full (mevedel-goal-implementation-context goal)))
    (should (equal "key" (mevedel-goal-continuation-key goal))))
  :doc "keeps sessions without a Goal empty"
  (should-not (mevedel-session-persistence--goal-from-plist nil))
  :doc "rejects unsafe IDs and malformed lifecycle state"
  (let ((valid '(:id "g1" :objective "Ship" :status active
                 :phase planning :approval-policy supervised
                 :owner-session "main"
                 :execution-home (:kind current :directory "/tmp/"
                                  :session-id "main" :locked t)
                 :implementation-context full
                 :cycle 1 :cycles ((:cycle 1))
                 :token-budget nil :token-usage 0
                 :continuation-key nil)))
    (dolist (change '((:id "../escape")
                      (:status unknown)
                      (:phase editing)
                      (:cycle 0)
                      (:cycles ((:cycle "one")))
                      (:execution-home
                       (:kind current :directory "/tmp/" :session-id "main"))
                      (:execution-home
                       (:kind current :directory "/tmp/" :session-id "main"
                        :locked yes))
                      (:checkpoint (:phase planning))
                      (:reason 42)))
      (let ((plist (copy-tree valid)))
        (setq plist (plist-put plist (car change) (cadr change)))
        (should-error
         (mevedel-session-persistence--goal-from-plist plist)
         :type 'error)))))


;;
;;; Task round-trip

(mevedel-deftest mevedel-session-persistence--task-to-plist ()
  ,test
  (test)
  :doc "captures all task fields"
  (let* ((task (mevedel-task--create
                :id 7 :subject "S" :description "D"
                :status 'pending :owner "explorer"
                :blocks '(8) :blocked-by '(5 6)
                :completed-turn 12
                :metadata '(:priority low :tag "x")))
         (plist (mevedel-session-persistence--task-to-plist task)))
    (should (= 7 (plist-get plist :id)))
    (should (equal "S" (plist-get plist :subject)))
    (should (equal "D" (plist-get plist :description)))
    (should (eq 'pending (plist-get plist :status)))
    (should (equal "explorer" (plist-get plist :owner)))
    (should (equal '(8) (plist-get plist :blocks)))
    (should (equal '(5 6) (plist-get plist :blocked-by)))
    (should (= 12 (plist-get plist :completed-turn)))
    (should (equal '(:priority low :tag "x")
                   (plist-get plist :metadata)))))

(mevedel-deftest mevedel-session-persistence--task-from-plist ()
  ,test
  (test)
  :doc "rebuilds a task struct from plist"
  (let* ((plist (list :id 3 :subject "X" :description nil
                      :status 'completed :owner nil
                      :blocks nil :blocked-by nil
                      :completed-turn 9 :metadata nil))
         (task (mevedel-session-persistence--task-from-plist plist)))
    (should (mevedel-task-p task))
    (should (= 3 (mevedel-task-id task)))
    (should (equal "X" (mevedel-task-subject task)))
    (should (eq 'completed (mevedel-task-status task)))
    (should (= 9 (mevedel-task-completed-turn task))))

  :doc "normalizes empty owner to nil"
  (let* ((plist (list :id 4 :subject "Y" :description nil
                      :status 'pending :owner ""
                      :blocks nil :blocked-by nil :metadata nil))
         (task (mevedel-session-persistence--task-from-plist plist)))
    (should (mevedel-task-p task))
    (should (null (mevedel-task-owner task)))))


;;
;;; Top-level round-trip

(mevedel-deftest mevedel-session-persistence-serialize ()
  ,test
  (test)
  :doc "serializes a fully populated session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((session (test-mevedel-session-persistence--make-session root))
               (_ (setf (mevedel-session-preset-name session) 'test-preset
                        (mevedel-session-preset-settings session)
                        '((mevedel-model-tiers
                           (strong :provider "Test:test-model" :effort high))
                          (mevedel-model-workloads
                           (planning :tier strong)))
                        (mevedel-session-goal-handoff session)
                        '(:goal-id "g1" :target-session-id "target"
                          :target-directory "/tmp/target/" :state complete)))
               (plist (mevedel-session-persistence-serialize
                       session
                       :first-user-message "Refactor X"
                       :latest-user-message "Ship Y"
                       :additional-roots '(("alt" . "/tmp/alt")))))
          (should (equal "v0.5.0" (plist-get plist :version)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (plist-get plist :session-id)))
          (should (equal "main" (plist-get plist :session-name)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (plist-get plist :working-directory)))
          (should (equal 'ask (plist-get plist :permission-mode)))
          (should (eq 'test-preset (plist-get plist :preset-name)))
          (should (equal '((mevedel-model-tiers
                            (strong :provider "Test:test-model" :effort high))
                           (mevedel-model-workloads
                            (planning :tier strong)))
                         (plist-get plist :preset-settings)))
          (should (= 2 (plist-get plist :current-segment)))
          (should (= 5 (plist-get plist :total-turn-count)))
          (should (= 4 (plist-get plist :last-task-write-turn)))
          (should (equal '((nil :note "Main status" :updated-turn 4
                                :updated-at "2026-04-23T18:20:00+0200")
                           ("main" :note "Agent status" :updated-turn 4
                            :updated-at "2026-04-23T18:21:00+0200"))
                         (plist-get plist :task-status-notes)))
          (should (equal "Refactor X" (plist-get plist :first-user-message)))
          (should (equal "Ship Y" (plist-get plist :latest-user-message)))
          (should (equal '(("alt" . "/tmp/alt"))
                         (plist-get plist :additional-roots)))
          (should (equal '(("alpha" . "Alpha helper"))
                         (plist-get plist :skills-snapshot)))
          (should (= 3 (length (plist-get plist :permission-rules))))
          (should (= 2 (length (plist-get plist :resource-grants))))
          (should (= 2 (length (plist-get plist :tasks))))
          (should (plist-get plist :workspace))
          (should (plist-get plist :prompt-index))
          (should (plist-get plist :file-snapshots))
          (should (equal "target"
                         (plist-get (plist-get plist :goal-handoff)
                                    :target-session-id))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "fork fields default nil for a non-fork session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((session (test-mevedel-session-persistence--make-session root))
               (plist (mevedel-session-persistence-serialize session)))
          (should (null (plist-get plist :forked-from-session-id)))
          (should (null (plist-get plist :forked-from-turn))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "materializes the canonical global mode when the session inherits it"
  (let ((root (make-temp-file "mevedel-test-proj-" t))
        (saved-mode (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-permission-mode session) nil)
          (set-default-toplevel-value 'mevedel-permission-mode 'auto)
          (should (eq 'auto
                      (plist-get
                       (mevedel-session-persistence-serialize session)
                       :permission-mode))))
      (set-default-toplevel-value 'mevedel-permission-mode saved-mode)
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "refuses to persist retired permission modes"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-permission-mode session) 'default)
          (should-error (mevedel-session-persistence-serialize session)
                        :type 'error))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-persistence--validate-current-sidecar ()
  ,test
  (test)
  :doc "accepts a complete current sidecar"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (should (eq plist
                (mevedel-session-persistence--validate-current-sidecar
                 plist))))
  :doc "rejects a current-version sidecar with a missing required key"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (cl-remf plist :working-directory)
    (should-error
     (mevedel-session-persistence--validate-current-sidecar plist)
     :type 'error))
  :doc "requires resource grants even when none are stored"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (cl-remf plist :resource-grants)
    (should-error
     (mevedel-session-persistence--validate-current-sidecar plist)
     :type 'error))
  :doc "accepts only canonical persisted permission modes"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (dolist (mode '(ask auto full-auto))
      (should (eq plist
                  (mevedel-session-persistence--validate-current-sidecar
                   (plist-put plist :permission-mode mode)))))
    (dolist (mode '(default accept-edits trust-all edit))
      (should-error
       (mevedel-session-persistence--validate-current-sidecar
        (plist-put plist :permission-mode mode))
       :type 'error)))
  :doc "rejects prompt entries without current turn coordinates"
  (let ((plist
         (test-mevedel-session-persistence--complete-sidecar
          '(:prompt-index ((1 . ((:turn 1 :cum-turn 1))))))))
    (should-error
     (mevedel-session-persistence--validate-current-sidecar plist)
     :type 'error)))

(mevedel-deftest mevedel-session-persistence-deserialize ()
  ,test
  (test)
  :doc "round-trips a populated session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((source (test-mevedel-session-persistence--make-session root))
               (_ (setf (mevedel-session-preset-name source) 'test-preset
                        (mevedel-session-preset-settings source)
                        '((mevedel-model-tiers
                           (strong :provider "Test:test-model" :effort high))
                          (mevedel-model-workloads
                           (review :tier strong)))))
               (plist (mevedel-session-persistence-serialize
                       source
                       :first-user-message "Hi"
                       :latest-user-message "Later"))
               (result (mevedel-session-persistence-deserialize plist))
               (session (plist-get result :session)))
          (should (mevedel-session-p session))
          (should (equal "main" (mevedel-session-name session)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (mevedel-session-working-directory session)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (mevedel-session-session-id session)))
          (should (eq 'ask (mevedel-session-permission-mode session)))
          (should (eq 'test-preset (mevedel-session-preset-name session)))
          (should (equal '((mevedel-model-tiers
                            (strong :provider "Test:test-model" :effort high))
                           (mevedel-model-workloads
                            (review :tier strong)))
                         (mevedel-session-preset-settings session)))
          (should (= 5 (mevedel-session-turn-count session)))
          (should (= 4 (mevedel-session-last-task-write-turn session)))
          (should (equal '(("alpha" . "Alpha helper"))
                         (mevedel-session-skills-snapshot session)))
          (should (equal '((nil :note "Main status" :updated-turn 4
                                :updated-at "2026-04-23T18:20:00+0200")
                           ("main" :note "Agent status" :updated-turn 4
                            :updated-at "2026-04-23T18:21:00+0200"))
                         (mevedel-session-task-status-notes session)))
          (should (= 2 (mevedel-session-current-segment session)))
          (should (= 2 (length (mevedel-session-tasks session))))
          (should (= 3 (mevedel-task-completed-turn
                        (car (mevedel-session-tasks session)))))
          (should (= 3 (length (mevedel-session-permission-rules session))))
          (should (equal '((:path "/tmp/exact-read" :access read)
                           (:path "/tmp/exact-write" :access write))
                         (mevedel-session-resource-grants session)))
          (should (equal "Hi" (plist-get result :first-user-message)))
          (should (equal "Later" (plist-get result :latest-user-message)))
          ;; touched-files / mentions-shown reset to empty hash tables
          (should (hash-table-p (mevedel-session-touched-files session)))
          (should (zerop (hash-table-count (mevedel-session-touched-files session))))
          (should (hash-table-p (mevedel-session-mentions-shown session)))
          (should (zerop (hash-table-count (mevedel-session-mentions-shown session))))
          ;; workspace identity recovered
          (let ((workspace (mevedel-session-workspace session)))
            (should (eq 'project (mevedel-workspace-type workspace)))
            (should (equal "test-id" (mevedel-workspace-id workspace)))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "preserves automatic revision metadata and compact cycle audit evidence"
  (let* ((input-hash (secure-hash 'sha256 "# Original"))
         (replacement-hash (secure-hash 'sha256 "# Revised"))
         (plan-metadata
          `(:path "goals/g1/current-plan.md"
            :hash ,replacement-hash
            :status presented
            :revision-count 1
            :revision-pending nil
            :guardian-pending t))
         (goal
          `(:id "g1" :objective "Ship" :status active
            :phase awaiting-approval :approval-policy automatic
            :owner-session "test-session"
            :execution-home (:kind current :directory "/tmp/"
                             :session-id "test-session" :locked t)
            :implementation-context full
            :cycle 1
            :cycles
            ((:cycle 1
              :plan-revisions
              ((:revision 1
                :input-plan-hash ,input-hash
                :replacement-plan-hash ,replacement-hash
                :verdict revise
                :reason "Add recovery coverage"
                :feedback ("Cover restart")
                :guardian-provider "guardian"
                :guardian-effort high
                :planner-provider "planner"
                :planner-effort medium
                :settlement-state settled
                :started-at "2026-01-01T00:00:00Z"
                :settled-at "2026-01-01T00:01:00Z"))))
            :token-budget nil :token-usage 0
            :continuation-key nil))
         (result
          (mevedel-session-persistence-deserialize
           (test-mevedel-session-persistence--complete-sidecar
            (list :plan-metadata plan-metadata :goal goal))))
         (session (plist-get result :session))
         (restored-goal (mevedel-session-goal session)))
    (should (equal plan-metadata
                   (mevedel-session-plan-metadata session)))
    (should
     (equal (plist-get (car (mevedel-goal-cycles restored-goal))
                       :plan-revisions)
            (plist-get (car (plist-get goal :cycles))
                       :plan-revisions))))
  :doc "round-tripped revision state does not share mutable records"
  (let* ((records
          '((:cycle 1 :plan-revisions
             ((:revision 1 :settlement-state started)))))
         (goal
          (mevedel-session-persistence--goal-from-plist
           `(:id "g1" :objective "Ship" :status active
             :phase awaiting-approval :approval-policy automatic
             :owner-session "test-session"
             :execution-home (:kind current :directory "/tmp/"
                              :session-id "test-session" :locked t)
             :implementation-context full
             :cycle 1 :cycles ,records
             :token-budget nil :token-usage 0
             :continuation-key nil))))
    (setf (plist-get
           (car (plist-get (car (mevedel-goal-cycles goal))
                           :plan-revisions))
           :settlement-state)
          'failed)
    (should
     (eq 'started
         (plist-get (car (plist-get (car records) :plan-revisions))
                    :settlement-state))))
  :doc "persists and restores a blocked Goal reason through an on-disk sidecar"
  (let ((root (make-temp-file "mevedel-goal-sidecar-" t))
        (path (make-temp-file "mevedel-goal-sidecar-" nil ".el")))
    (unwind-protect
        (let* ((source (test-mevedel-session-persistence--make-session root))
               (_ (setf (mevedel-session-goal source)
                        (mevedel-goal--create
                         :id "blocked-goal" :objective "Ship"
                         :status 'blocked :phase 'reviewing
                         :approval-policy 'supervised
                         :owner-session "main-2026-04-23T14-30-a9f2" :cycle 1
                         :execution-home
                         '(:kind current :directory "/tmp/"
                           :session-id "main-2026-04-23T14-30-a9f2" :locked t)
                         :implementation-context 'full
                         :cycles '((:cycle 1))
                         :reason "Need an API credential.")))
               (_ (mevedel-session-persistence-write
                   path (mevedel-session-persistence-serialize source)))
               (result
                (mevedel-session-persistence-deserialize
                 (mevedel-session-persistence-read path)))
               (goal (mevedel-session-goal (plist-get result :session))))
          (should (eq 'paused (mevedel-goal-status goal)))
          (should (eq 'planning (mevedel-goal-phase goal)))
          (should (equal "Need an API credential."
                         (mevedel-goal-review-findings goal)))
          (should (equal
                   "Session restored paused: Need an API credential."
                   (mevedel-goal-reason goal))))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-exists-p path)
        (delete-file path))))
  :doc "restoration reason remains stable across repeated reopen cycles"
  (let* ((goal '(:id "g1" :objective "Ship" :status active
                 :phase planning :approval-policy supervised
                 :owner-session "test-session"
                 :execution-home (:kind current :directory "/tmp/"
                                  :session-id "test-session" :locked t)
                 :implementation-context full
                 :cycle 1 :cycles ((:cycle 1))
                 :token-budget nil :token-usage 0
                 :continuation-key nil))
         (first (mevedel-session-persistence-deserialize
                 (test-mevedel-session-persistence--complete-sidecar
                  (list :goal goal))))
         (first-session (plist-get first :session))
         (reason (mevedel-goal-reason
                  (mevedel-session-goal first-session)))
         (second
          (mevedel-session-persistence-deserialize
           (test-mevedel-session-persistence--complete-sidecar
            (list :goal
                  (mevedel-session-persistence--goal-to-plist
                   (mevedel-session-goal first-session)))))))
    (should (equal reason
                   (mevedel-goal-reason
                    (mevedel-session-goal (plist-get second :session))))))
  :doc "restores every unfinished status paused while leaving complete terminal"
  (dolist (case '((active . paused) (paused . paused)
                  (blocked . paused) (complete . complete)))
    (let* ((goal (list :id "g1" :objective "Ship" :status (car case)
                       :phase 'planning :approval-policy 'supervised
                       :owner-session "test-session"
                       :execution-home
                       '(:kind current :directory "/tmp/"
                         :session-id "test-session" :locked t)
                       :implementation-context 'full
                       :cycle 1 :cycles '((:cycle 1))
                       :token-budget nil :token-usage 0
                       :continuation-key nil))
           (result
            (mevedel-session-persistence-deserialize
             (test-mevedel-session-persistence--complete-sidecar
              (list :goal goal))))
           (restored (mevedel-session-goal (plist-get result :session))))
      (should (eq (cdr case) (mevedel-goal-status restored)))))
  :doc "drops permission rules with unknown actions"
  (let* ((plist (list :version (mevedel-version)
                      :session-name "x"
                      :permission-rules
                      '(("Read"  :path "/x" :action allow)
                        ("Write" :path "/y" :action future-action))
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil))
         (session (plist-get
                   (mevedel-session-persistence-deserialize
                    (test-mevedel-session-persistence--complete-sidecar plist))
                   :session)))
    (should (= 1 (length (mevedel-session-permission-rules session)))))
  :doc "preserves relocated working directories under the new workspace root"
  (let* ((old-root (make-temp-file "mevedel-old-root-" t))
         (new-root (make-temp-file "mevedel-new-root-" t))
         (workspace-id (format "relocated-id-%s" (gensym)))
         (old-cwd (file-name-concat old-root "packages/api"))
         (new-cwd (file-name-concat new-root "packages/api"))
         (plist (list :version (mevedel-version)
                      :session-name "x"
                      :workspace (list :type 'project
                                       :id workspace-id
                                       :root old-root
                                       :name "relocated-proj")
                      :working-directory old-cwd
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil)))
    (unwind-protect
        (progn
          (make-directory new-cwd t)
          (mevedel-workspace-get-or-create
           'project workspace-id new-root "relocated-proj")
          (let ((session (plist-get
                          (mevedel-session-persistence-deserialize
                           (test-mevedel-session-persistence--complete-sidecar
                            plist))
                          :session)))
            (should (equal (file-name-as-directory new-cwd)
                           (mevedel-session-working-directory session)))))
      (mevedel-workspace-clear-registry)
      (when (file-directory-p old-root)
        (delete-directory old-root t))
      (when (file-directory-p new-root)
        (delete-directory new-root t))))

  :doc "preserves saved working directories already under a nested current root"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "mevedel-old-root-" t)))
         (new-root (file-name-as-directory
                    (file-name-concat old-root "packages" "api")))
         (workspace-id (format "nested-relocated-id-%s" (gensym)))
         (saved-cwd new-root)
         (plist (list :version (mevedel-version)
                      :session-name "x"
                      :workspace (list :type 'project
                                       :id workspace-id
                                       :root old-root
                                       :name "nested-proj")
                      :working-directory saved-cwd
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil)))
    (unwind-protect
        (progn
          (make-directory new-root t)
          (mevedel-workspace-get-or-create
           'project workspace-id new-root "nested-proj")
          (let ((session (plist-get
                          (mevedel-session-persistence-deserialize
                           (test-mevedel-session-persistence--complete-sidecar
                            plist))
                          :session)))
            (should (equal new-root
                           (mevedel-session-working-directory session)))))
      (mevedel-workspace-clear-registry)
      (when (file-directory-p old-root)
        (delete-directory old-root t))))

  :doc "rejects restored working directories outside the workspace"
  (let ((plist (list :version (mevedel-version)
                     :session-name "x"
                     :workspace '(:type project
                                  :id "restore-id"
                                  :root "/tmp/restore-proj/"
                                  :name "restore-proj")
                     :working-directory "/tmp/restore-proj-sibling/"
                     :tasks nil
                     :prompt-index nil
                     :file-snapshots nil)))
    (should-error
     (mevedel-session-persistence-deserialize
      (test-mevedel-session-persistence--complete-sidecar plist))
     :type 'user-error)))

  :doc "rejects restored symlink working directories outside the workspace"
  (let* ((root (make-temp-file "mevedel-restore-root-" t))
         (outside (make-temp-file "mevedel-restore-outside-" t))
         (link (file-name-concat root "linked-cwd"))
         (workspace-id (format "restore-symlink-%s" (gensym)))
         (plist (list :version (mevedel-version)
                      :session-name "x"
                      :workspace (list :type 'project
                                       :id workspace-id
                                       :root root
                                       :name "restore-proj")
                      :working-directory link
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil)))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (should-error
           (mevedel-session-persistence-deserialize
            (test-mevedel-session-persistence--complete-sidecar plist))
           :type 'user-error))
      (when (file-symlink-p link)
        (delete-file link))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p outside)
        (delete-directory outside t))))


;;
;;; Sidecar IO

(mevedel-deftest mevedel-session-persistence-write ()
  ,test
  (test)
  :doc "atomic write produces a readable plist"
  (let ((tmp (make-temp-file "mevedel-session-meta-test-" nil ".el")))
    (unwind-protect
        (let* ((plist `(:version ,(mevedel-version)
                                 :session-name "main"
                                 :tasks nil
                                 :permission-rules nil)))
          (mevedel-session-persistence-write tmp plist)
          (should (file-exists-p tmp))
          (let ((readback (mevedel-session-persistence-read tmp)))
            (should (equal "main" (plist-get readback :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(mevedel-deftest mevedel-session-persistence--write-current-buffer-atomically ()
  ,test
  (test)
  :doc "publishes current buffer contents through a same-directory rename"
  (let ((path (make-temp-file "mevedel-transcript-atomic-")))
    (unwind-protect
        (with-temp-buffer
          (insert "replacement")
          (mevedel-session-persistence--write-current-buffer-atomically path)
          (should
           (equal "replacement"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (when (file-exists-p path) (delete-file path))))
  :doc "preserves the original when publication fails"
  (let ((path (make-temp-file "mevedel-transcript-atomic-")))
    (unwind-protect
        (progn
          (write-region "original" nil path nil 'silent)
          (with-temp-buffer
            (insert "replacement")
            (cl-letf (((symbol-function 'rename-file)
                       (lambda (&rest _) (error "Publication failed"))))
              (should-error
               (mevedel-session-persistence--write-current-buffer-atomically
                path))))
          (should
           (equal "original"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (when (file-exists-p path) (delete-file path)))))

;;
;;; Phase 2: ID generation, paths, lazy materialization

(mevedel-deftest mevedel-session-persistence--sanitize ()
  ,test
  (test)
  :doc "leaves alphanumerics, underscores, dashes alone"
  (should (equal "main" (mevedel-session-persistence--sanitize "main")))
  (should (equal "alt-1_2"
                 (mevedel-session-persistence--sanitize "alt-1_2")))
  :doc "replaces spaces and slashes with underscores"
  (should (equal "alt_branch"
                 (mevedel-session-persistence--sanitize "alt branch")))
  (should (equal "a_b_c"
                 (mevedel-session-persistence--sanitize "a/b/c")))
  :doc "handles nil input"
  (should (equal "" (mevedel-session-persistence--sanitize nil))))

(mevedel-deftest mevedel-session-persistence--short-uuid ()
  ,test
  (test)
  :doc "returns four hex characters"
  (let ((u (mevedel-session-persistence--short-uuid)))
    (should (= 4 (length u)))
    (should (string-match-p "\\`[0-9a-f]+\\'" u)))
  :doc "produces different values across calls (probabilistically)"
  (let ((seen (make-hash-table :test #'equal)))
    (dotimes (_ 32)
      (puthash (mevedel-session-persistence--short-uuid) t seen))
    ;; With 4 hex chars (65536 possible values) and only 32 samples,
    ;; collisions are vanishingly rare.  Accept any number > 1.
    (should (> (hash-table-count seen) 1))))

(mevedel-deftest mevedel-session-persistence--compute-id ()
  ,test
  (test)
  :doc "generates id matching <name>-<timestamp>-<short-uuid>"
  (let ((id (mevedel-session-persistence--compute-id "main")))
    (should (string-match-p
             "\\`main-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9a-f]\\{4\\}\\'"
             id)))
  :doc "sanitizes the name component"
  (let ((id (mevedel-session-persistence--compute-id "my session")))
    (should (string-prefix-p "my_session-" id))))

(mevedel-deftest mevedel-session-persistence--allocate-session-id ()
  ,test
  (test)
  :doc "retries until the generated id has no session directory"
  (let ((sessions-dir (make-temp-file "mevedel-id-allocation-" t))
        (calls 0))
    (unwind-protect
        (progn
          (write-region "occupied\n" nil
                        (file-name-concat sessions-dir "taken") nil 'silent)
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--compute-id)
                     (lambda (_name)
                       (if (= (cl-incf calls) 1) "taken" "fresh"))))
            (should (equal
                     "fresh"
                     (mevedel-session-persistence--allocate-session-id
                      "main" sessions-dir)))
            (should (= calls 2))))
      (delete-directory sessions-dir t))))

(mevedel-deftest mevedel-session-persistence--segment-path ()
  ,test
  (test)
  :doc "zero-pads segment number to four digits"
  (should (equal "/x/segment-0001.chat.org"
                 (mevedel-session-persistence--segment-path "/x" 1)))
  (should (equal "/x/segment-0042.chat.org"
                 (mevedel-session-persistence--segment-path "/x" 42)))
  (should (equal "/x/segment-1000.chat.org"
                 (mevedel-session-persistence--segment-path "/x" 1000))))

(mevedel-deftest mevedel-session-persistence--first-user-message ()
  ,test
  (test)
  :doc "extracts first non-blank line of first user region"
  (with-temp-buffer
    (insert "Refactor the permission chain\n\nMore details follow.")
    (should (equal "Refactor the permission chain"
                   (mevedel-session-persistence--first-user-message
                    (current-buffer)))))
  :doc "skips assistant response regions"
  (with-temp-buffer
    (insert (propertize "Sure, I'll do that.\n" 'gptel 'response))
    (insert "What about edge cases?\n")
    (should (equal "What about edge cases?"
                   (mevedel-session-persistence--first-user-message
                    (current-buffer)))))
  :doc "returns nil for buffers with no user content"
  (with-temp-buffer
    (insert (propertize "All response.\n" 'gptel 'response))
    (should (null (mevedel-session-persistence--first-user-message
                   (current-buffer)))))
  :doc "truncates long lines"
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (let ((preview (mevedel-session-persistence--first-user-message
                    (current-buffer))))
      (should (= 120 (length preview)))
      (should (string-suffix-p "..." preview)))))


;;
;;; Phase 2: write path

(defun test-mevedel-session-persistence--make-tempdir-workspace ()
  "Build a workspace rooted in a fresh tempdir.
Returns (cons WORKSPACE TEMPDIR).  The workspace's NAME is derived
from the tempdir basename so that different tests never collide on the
chat-buffer name (`*mevedel:NAME@WORKSPACE*'); buffer leakage across
tests would otherwise mask correctness bugs in the live-buffer path of
`mevedel-session-persistence-restore'.  Caller must
`delete-directory' the tempdir on cleanup."
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-test-ws-" t)))
         (basename (file-name-nondirectory (directory-file-name tempdir)))
         (_       (mevedel-workspace-clear-registry))
         (ws      (mevedel-workspace-get-or-create
                   'project basename tempdir basename)))
    (cons ws tempdir)))

(defun test-mevedel-session-persistence--release-and-kill (buf session)
  "Release SESSION's lock and kill BUF if alive.
Mirrors the production kill-buffer-hook's lock release for tests
that don't go through `mevedel--chat-buffer-init-common' (which
installs the real hook)."
  (when (and session (mevedel-session-save-path session))
    (mevedel-session-persistence-lock-release
     (mevedel-session-save-path session)))
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf)))

(defun test-mevedel-session-persistence--make-missing-cwd-session ()
  "Return a saved session whose working directory has been deleted.
The result is (WORKSPACE TEMPDIR MISSING-DIR REPLACEMENT-DIR SESSION-DIR)."
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((missing-dir (file-name-as-directory
                         (file-name-concat tempdir "deleted-worktree")))
           (replacement-dir (file-name-as-directory
                             (file-name-concat tempdir "replacement")))
           (session (mevedel-session-create "main" workspace missing-dir))
           (buf (generate-new-buffer "*test-data-buf*"))
           session-dir)
      (unwind-protect
          (progn
            (make-directory missing-dir t)
            (make-directory replacement-dir t)
            (with-current-buffer buf
              (org-mode)
              (insert "Missing working directory\n")
              (mevedel-session-persistence-save session buf))
            (setq session-dir (mevedel-session-save-path session))
            (test-mevedel-session-persistence--release-and-kill buf session)
            (setq buf nil)
            (delete-directory missing-dir t)
            (list workspace tempdir missing-dir replacement-dir session-dir))
        (test-mevedel-session-persistence--release-and-kill buf session)))))

(defun test-mevedel-session-persistence--reset-instructions ()
  "Reset global and workspace-scoped instruction state for persistence cases."
  (setf (mevedel--instruction-alist) nil)
  (setf (mevedel--instruction-id-counter) 0)
  (setf (mevedel--instruction-id-usage-map) (make-hash-table))
  (setf (mevedel--instruction-retired-ids) nil)
  (setq mevedel--instruction-states (make-hash-table :test #'equal))
  (setq mevedel--instruction-current-state-key :global))

(mevedel-deftest mevedel-session-persistence--authoritative-buffer ()
  ,test
  (test)
  :doc "returns ordinary data buffers unchanged"
  (let ((buf (generate-new-buffer " *test-data*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (eq buf (mevedel-session-persistence--authoritative-buffer
                           buf))))
      (when (buffer-live-p buf) (kill-buffer buf))))
  :doc "routes interactive view buffers to their data buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (should (eq data-buf
                              (mevedel-session-persistence--authoritative-buffer
                               view-buf)))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "does not treat transcript inspection views as session segment buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-agent-data*"))
               (view-buf (generate-new-buffer " *test-agent-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace))
                (mevedel-view--setup view-buf data-buf
                                     (list :agent-transcript-p t))
                (with-current-buffer view-buf
                  (should-not
                   (mevedel-session-persistence--authoritative-buffer
                    view-buf))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel--instruction-workspace-state ()
  ,test
  (test)
  :doc "keeps instruction alists isolated by workspace"
  (let* ((root-a (file-name-as-directory
                  (make-temp-file "mevedel-test-ws-a-" t)))
         (root-b (file-name-as-directory
                  (make-temp-file "mevedel-test-ws-b-" t)))
         (file-a (file-name-concat root-a "a.el"))
         (file-b (file-name-concat root-b "b.el"))
         (buf-a nil)
         (buf-b nil))
    (unwind-protect
        (progn
          (test-mevedel-session-persistence--reset-instructions)
          (mevedel-workspace-clear-registry)
          (write-region "(message \"a\")\n" nil file-a nil 'silent)
          (write-region "(message \"b\")\n" nil file-b nil 'silent)
          (let ((ws-a (mevedel-workspace-get-or-create
                       'project "a" root-a "a"))
                (ws-b (mevedel-workspace-get-or-create
                       'project "b" root-b "b")))
            (setq buf-a (find-file-noselect file-a))
            (setq buf-b (find-file-noselect file-b))
            (with-current-buffer buf-a
              (setq-local mevedel--workspace ws-a)
              (mevedel--create-reference-in buf-a (point-min) (point-max)))
            (with-current-buffer buf-b
              (setq-local mevedel--workspace ws-b)
              (mevedel--create-reference-in buf-b (point-min) (point-max)))
            (mevedel--instruction-activate-workspace ws-a)
            (should (= 1 (length (alist-get buf-a (mevedel--instruction-alist)))))
            (should-not (assoc buf-b (mevedel--instruction-alist)))
            (mevedel--instruction-activate-workspace ws-b)
            (should (= 1 (length (alist-get buf-b (mevedel--instruction-alist)))))
            (should-not (assoc buf-a (mevedel--instruction-alist)))))
      (when (buffer-live-p buf-a)
        (with-current-buffer buf-a (set-buffer-modified-p nil))
        (kill-buffer buf-a))
      (when (buffer-live-p buf-b)
        (with-current-buffer buf-b (set-buffer-modified-p nil))
        (kill-buffer buf-b))
      (delete-directory root-a t)
      (delete-directory root-b t)
      (test-mevedel-session-persistence--reset-instructions)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel--instruction-operation-state-key ()
  ,test
  (test)
  :doc "prefers a dynamic workspace override over the buffer workspace"
  (let ((workspace (mevedel-workspace--create
                    :type 'project :id "buffer" :root "/tmp/buffer/")))
    (with-temp-buffer
      (setq-local mevedel--workspace workspace)
      (let ((mevedel--instruction-state-key-override
             '(project . "explicit")))
        (should (equal '(project . "explicit")
                       (mevedel--instruction-operation-state-key)))))))

(mevedel-deftest mevedel-session-persistence-ensure-files ()
  ,test
  (test)
  :doc "lazily materializes the session directory tree"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hello LLM\n")
                (let ((path (mevedel-session-persistence-ensure-files
                             session buf)))
                  (should path)
                  (should (file-directory-p path))
                  (should (file-directory-p (file-name-concat path "agents")))
                  (should (file-directory-p
                           (file-name-concat path "file-history")))
                  ;; `ensure-files' leaves sidecar writing to `save'
                  ;; (one write instead of two on first materialization).
                  (should (file-exists-p
                           (file-name-concat path "segment-0001.chat.org")))
                  ;; Struct fields populated
                  (should (mevedel-session-session-id session))
                  (should (mevedel-session-created-at session))
                  (should (= 1 (mevedel-session-current-segment session)))
                  ;; Buffer wired to segment file
                  (should (equal (file-name-concat path "segment-0001.chat.org")
                                 buffer-file-name))
                  ;; Idempotent: second call returns same path, no churn
                  (should (equal path
                                 (mevedel-session-persistence-ensure-files
                                  session buf)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "backfills diagnostics recorded before first materialization"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               (repair-event
                '(:time "now" :origin "main" :backend backend
                  :model model :tool "Read" :outcome valid
                  :repair-enabled t :rules nil :paths nil
                  :issue-kinds nil :execution executed :result success)))
          (unwind-protect
              (progn
                (mevedel-hooks--log
                 session '(:event UserPromptSubmit :status ok))
                (mevedel-tool-repair-log-event session repair-event)
                (mevedel-permission-log
                 session 'permission-decision :tool-name "Read")
                (with-current-buffer buf
                  (org-mode)
                  (mevedel-session-persistence-ensure-files session buf))
                (dolist (file '("hook-log.el" "repair-log.el"
                                "permission-log.el"))
                  (should
                   (file-readable-p
                    (file-name-concat
                     (mevedel-session-save-path session) file))))
                (should-not
                 (mevedel-session-permission-log-pending session)))
            (when (buffer-live-p buf)
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "repairs shallowly materialized sessions before saving data buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (wrong-buf (generate-new-buffer "*test-wrong-buf*"))
               (data-buf (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (let ((path (with-current-buffer wrong-buf
                            (org-mode)
                            (mevedel-session-persistence--shallow-ensure-files
                             session wrong-buf))))
                (should path)
                (should-not
                 (file-exists-p
                  (file-name-concat path "segment-0001.chat.org")))
                (with-current-buffer data-buf
                  (org-mode)
                  (insert "Hello after shallow materialization\n")
                  (should-not buffer-file-name)
                  (should (equal path
                                 (mevedel-session-persistence-ensure-files
                                  session data-buf)))
                  (should (equal (file-name-concat
                                  path "segment-0001.chat.org")
                                 buffer-file-name))
                  (should
                   (file-exists-p
                    (file-name-concat path "segment-0001.chat.org")))
                  (let ((segment-file buffer-file-name))
                    (should (string-match-p
                             "Hello after shallow materialization"
                             (with-temp-buffer
                               (insert-file-contents segment-file)
                               (buffer-string)))))))
            (when (buffer-live-p wrong-buf) (kill-buffer wrong-buf))
            (when (buffer-live-p data-buf) (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-save ()
  ,test
  (test)
  :doc "advances updated-at across saves"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (mevedel-session-persistence-save session buf)
                (let ((first-updated (mevedel-session-updated-at session)))
                  (should first-updated)
                  ;; Force a second-tick gap so the timestamp can advance.
                  (sleep-for 1.1)
                  (insert "Second prompt\n")
                  (mevedel-session-persistence-save session buf)
                  (should-not (equal first-updated
                                     (mevedel-session-updated-at session)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "rewritten sidecar reflects current session state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Refactor the permission chain\n")
                (mevedel-session-persistence-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-persistence-read sidecar-path)))
                  (should (equal "main" (plist-get plist :session-name)))
                  (should (equal "Refactor the permission chain"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Refactor the permission chain"
                                 (plist-get plist :latest-user-message)))
                  (should (= 1 (plist-get plist :current-segment)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "latest sidecar preview follows the newest prompt"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (insert (propertize "Assistant response\n" 'gptel 'response))
                (insert "Second prompt\n")
                (mevedel-session-persistence-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-persistence-read sidecar-path)))
                  (should (equal "First prompt"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Second prompt"
                                 (plist-get plist :latest-user-message)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "first sidecar preview stays stable across later saves"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Original prompt\n")
                (mevedel-session-persistence-save session buf)
                (erase-buffer)
                (insert "Later prompt\n")
                (mevedel-session-persistence-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-persistence-read sidecar-path)))
                  (should (equal "Original prompt"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Later prompt"
                                 (plist-get plist :latest-user-message)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "view buffers save through their data buffer without becoming files"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace)
                  (insert "Persist data prompt\n"))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (goto-char mevedel-view--input-marker)
                    (insert "Working view chrome\n"))
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'read-file-name)
                           (lambda (&rest _)
                             (error "View buffer requested a save filename"))))
                  (mevedel-session-persistence-save session view-buf))
                (with-current-buffer view-buf
                  (should-not buffer-file-name)
                  (should-not buffer-file-truename))
                (let ((segment-path
                       (mevedel-session-persistence--segment-path
                        (mevedel-session-save-path session) 1)))
                  (should (file-exists-p segment-path))
                  (with-temp-buffer
                    (insert-file-contents segment-path)
                    (should (string-match-p "Persist data prompt"
                                            (buffer-string)))
                    (should-not (string-match-p "Working view chrome"
                                                (buffer-string))))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--kill-emacs-hook ()
  ,test
  (test)
  :doc "force-tears down executions before exit persistence"
  (let (torn-down)
    (cl-letf (((symbol-function 'mevedel-execution-teardown-all)
               (lambda () (setq torn-down t))))
      (mevedel-session-persistence--kill-emacs-hook))
    (should torn-down))
  :doc "modified view buffers are persisted through data buffers on exit"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace)
                  (insert "Exit hook data prompt\n")
                  (set-buffer-modified-p nil))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (goto-char mevedel-view--input-marker)
                    (insert "Exit hook view chrome\n"))
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional _frame)
                             (list view-buf data-buf)))
                          ((symbol-function 'read-file-name)
                           (lambda (&rest _)
                             (error "View buffer requested a save filename"))))
                  (mevedel-session-persistence--kill-emacs-hook))
                (with-current-buffer view-buf
                  (should-not buffer-file-name)
                  (should-not buffer-file-truename))
                (let ((segment-path
                       (mevedel-session-persistence--segment-path
                        (mevedel-session-save-path session) 1)))
                  (should (file-exists-p segment-path))
                  (with-temp-buffer
                    (insert-file-contents segment-path)
                    (should (string-match-p "Exit hook data prompt"
                                            (buffer-string)))
                    (should-not (string-match-p "Exit hook view chrome"
                                                (buffer-string))))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--instruction-snapshots ()
  ,test
  (test)
  :doc "saves current and per-turn instruction snapshots"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun alpha () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-reference-in source-buf (point-min) (point-max)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain alpha\n")
              (mevedel-session-persistence-save session data-buf))
            (let ((current-path
                   (mevedel-session-persistence--instructions-current-path
                    (mevedel-session-save-path session)))
                  (turn-path
                   (mevedel-session-persistence--instructions-turn-path
                    (mevedel-session-save-path session) 1)))
              (should (file-exists-p current-path))
              (should (file-exists-p turn-path))
              (let* ((current-save (with-temp-buffer
                                     (insert-file-contents current-path)
                                     (read (current-buffer))))
                     (turn-save (with-temp-buffer
                                  (insert-file-contents turn-path)
                                  (read (current-buffer))))
                     (current-file-plist
                      (cdr (assoc "source.el"
                                  (plist-get current-save :files))))
                     (turn-file-plist
                      (cdr (assoc "source.el"
                                  (plist-get turn-save :files))))
                     (instruction
                      (car (plist-get current-file-plist :instructions)))
                     (turn-instruction
                      (car (plist-get turn-file-plist :instructions)))
                     (properties
                      (plist-get instruction :properties))
                     (anchor (plist-get turn-instruction :anchor)))
                (should (plist-member current-file-plist :original-content))
                (should-not (plist-member turn-file-plist
                                          :original-content))
                (should (= 1 (plist-get turn-file-plist :anchor-schema)))
                (should (plist-get turn-file-plist :content-hash))
                (should (= 1 (plist-get anchor :schema)))
                (should (plist-get anchor :uuid))
                (should (plist-member anchor :bodyless))
                (should (plist-get anchor :text-hash))
                (should (memq 'mevedel-instruction properties))
                (should-not (memq 'before-string properties))
                (should-not (memq 'face properties))
                (should-not (memq 'keymap properties))
                (should-not (memq 'mevedel-bg-color properties)))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "restores instruction overlays after clearing live state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun beta () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-reference-in source-buf (point-min) (point-max)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain beta\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (should-not (mevedel--all-instructions))
              (mevedel-session-persistence--load-instructions session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (should (= 1 (length (alist-get source-buf (mevedel--instruction-alist))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "strips transient text properties from persisted instruction strings"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el"))
                 (directive-text (copy-sequence "Fix beta")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun beta () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (add-text-properties 0 (length directive-text)
                                 `(tabulated-list-id ,source-buf)
                                 directive-text)
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-directive-in
               source-buf (point-min) (point-max) nil directive-text))
            (setq session (mevedel-session-create "main" workspace))
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain beta\n")
              (mevedel-session-persistence-save session data-buf))
            (let* ((current-path
                    (mevedel-session-persistence--instructions-current-path
                     (mevedel-session-save-path session)))
                   (save-file (with-temp-buffer
                                (insert-file-contents current-path)
                                (read (current-buffer))))
                   (file-plist (cdr (assoc "source.el"
                                           (plist-get save-file :files))))
                   (instruction
                    (car (plist-get file-plist :instructions)))
                   (properties (plist-get instruction :properties))
                   (directive (plist-get properties 'mevedel-directive)))
              (should (equal "Fix beta" directive))
              (should-not (text-properties-at 0 directive)))
            (with-current-buffer data-buf
              (mevedel--clear-instruction-state workspace)
              (mevedel-session-persistence--load-instructions session data-buf))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ov (car (alist-get source-buf (mevedel--instruction-alist))))
                   (directive (overlay-get ov 'mevedel-directive)))
              (should (equal "Fix beta" directive))
              (should-not (text-properties-at 0 directive))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "ignores unreadable instruction snapshots during session restore"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((data-buf nil)
          (session nil))
      (unwind-protect
          (progn
            (test-mevedel-session-persistence--reset-instructions)
            (setq session (mevedel-session-create "main" workspace))
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain gamma\n")
              (mevedel-session-persistence-save session data-buf))
            (let ((path (mevedel-session-persistence--instructions-current-path
                         (mevedel-session-save-path session))))
              (make-directory (file-name-directory path) t)
              (write-region "(:files ((\"source.el\" . #<marker>)))"
                            nil path nil 'silent)
              (should-not
               (mevedel-session-persistence--load-instructions
                session data-buf))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-session-persistence--instruction-anchor-restore ()
  ,test
  (test)
  :doc "reanchors an instruction after text is inserted before it"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "aaa\nTARGET\nbbb\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "TARGET\n")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let ((ov (car (alist-get source-buf (mevedel--instruction-alist)))))
              (should ov)
              (with-current-buffer source-buf
                (should (equal "TARGET\n"
                               (buffer-substring-no-properties
                                (overlay-start ov) (overlay-end ov)))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "uses parent containment to resolve duplicate child text"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "PARENT\nchild\nEND\noutside child\n"
                          nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (let ((parent-start (point)))
                (search-forward "END\n")
                (mevedel--create-reference-in
                 source-buf parent-start (point)))
              (goto-char (point-min))
              (search-forward "child")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain nested target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ovs (alist-get source-buf (mevedel--instruction-alist)))
                   (child (cl-find-if
                           (lambda (ov)
                             (with-current-buffer source-buf
                               (equal "child"
                                      (buffer-substring-no-properties
                                       (overlay-start ov)
                                       (overlay-end ov)))))
                           ovs)))
              (should (= 2 (length ovs)))
              (should child)
              (with-current-buffer source-buf
                (save-excursion
                  (goto-char (overlay-start child))
                  (should (search-backward "PARENT" nil t))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "leaves ambiguous anchors unresolved instead of restoring stale bounds"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil)
          (old-context mevedel-instruction-anchor-context-chars))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (setq mevedel-instruction-anchor-context-chars 0)
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "x\ndup\ny\nx\ndup\ny\n"
                          nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "dup\n")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain ambiguous target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (with-current-buffer source-buf
              (should-not (mevedel--instructions-in
                           (point-min) (point-max)))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (setq mevedel-instruction-anchor-context-chars old-context)
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "reanchors a bodyless directive by surrounding context"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "before TARGET after\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "TARGET")
              (mevedel--create-directive-in
               source-buf (match-beginning 0) (match-beginning 0)
               t "Do it"))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain bodyless target\n")
              (mevedel-session-persistence-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-persistence--load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let ((ov (car (alist-get source-buf (mevedel--instruction-alist)))))
              (should ov)
              (should (= (overlay-start ov) (overlay-end ov)))
              (with-current-buffer source-buf
                (goto-char (overlay-start ov))
                (should (looking-at-p "TARGET")))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-session-persistence--restore-gptel-state ()
  ,test
  (test)
  :doc "does not dirty resumed buffers while repairing bounds and properties"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BOUNDS: ((response (2 999)))\n"
            ":END:\n"
            "#+begin_tool\n"
            "(:name \"Bash\" :args (:command \"true\"))\n"
            "ok\n"
            "#+end_tool\n"
            "Focused tests passed\n")
    (setq-local gptel-mode nil)
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'gptel-mode)
               (lambda (&optional _arg)
                 (setq-local gptel-mode t)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "#+begin_tool")
                   (let ((tool-start (match-beginning 0)))
                     (search-forward "sed tests")
                     (add-text-properties
                      tool-start (match-beginning 0)
                      '(gptel (tool . "stale"))))
                   (goto-char (point-min))
                   (search-forward "sed tests")
                   (add-text-properties
                    (match-beginning 0) (point-max)
                    '(gptel response))))))
      (mevedel-session-persistence--restore-gptel-state))
    (should-not (buffer-modified-p))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Focused tests")
      (should (eq (get-text-property (match-beginning 0) 'gptel)
                  'response)))))

(mevedel-deftest mevedel-session-persistence--dynamic-system-preset-p ()
  ,test
  (test)
  :doc "detects function-valued system presets"
  (let ((gptel--preset 'mevedel-test-dynamic))
    (cl-letf (((symbol-function 'gptel-get-preset)
               (lambda (preset)
                 (when (eq preset 'mevedel-test-dynamic)
                   `(:system ,(lambda () "Dynamic prompt"))))))
      (should (mevedel-session-persistence--dynamic-system-preset-p))))
  :doc "detects dynamic-spec system presets"
  (let ((gptel--preset 'mevedel-test-dynamic-spec))
    (cl-letf (((symbol-function 'gptel-get-preset)
               (lambda (preset)
                 (when (eq preset 'mevedel-test-dynamic-spec)
                   '(:system (:eval (mevedel-system-build-prompt)))))))
      (should (mevedel-session-persistence--dynamic-system-preset-p))))
  :doc "ignores static string system presets"
  (let ((gptel--preset 'mevedel-test-static))
    (cl-letf (((symbol-function 'gptel-get-preset)
               (lambda (preset)
                 (when (eq preset 'mevedel-test-static)
                   '(:system "Static prompt")))))
      (should-not (mevedel-session-persistence--dynamic-system-preset-p)))))

(mevedel-deftest mevedel-session-persistence--save-gptel-state-around ()
  ,test
  (test)
  :doc "removes frozen GPTEL_SYSTEM before delegated save for dynamic presets"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Frozen prompt")
                delegated-system
                system-present-at-delegate
                orig-fun)
            (setq orig-fun
                  (lambda ()
                    (setq delegated-system gptel-system-prompt)
                    (setq system-present-at-delegate
                          (org-entry-get (point-min) "GPTEL_SYSTEM"))
                    (org-entry-put (point-min) "GPTEL_BOUNDS"
                                   "((response (42 55)))")))
            (org-entry-put (point-min) "GPTEL_SYSTEM" "Frozen prompt")
            (cl-letf (((symbol-function
                        'mevedel-session-persistence--dynamic-system-preset-p)
                       (lambda () t)))
              (mevedel-session-persistence--save-gptel-state-around orig-fun))
            (should-not delegated-system)
            (should-not system-present-at-delegate)
	    (should-not (org-entry-get (point-min) "GPTEL_SYSTEM"))
	    (should-not (org-entry-get (point-min) "GPTEL_BOUNDS"))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "removes accumulated and case-variant GPTEL_SYSTEM properties"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (insert ":PROPERTIES:\n:gptel_system: Lowercase prompt\n:GPTEL_SYSTEM+: Extra prompt\n:OTHER: Keep\n:END:\n")
	  (setq-local mevedel--session
		      (mevedel-session-create
		       "main"
		       (test-mevedel-session-persistence--make-workspace root)))
	  (let ((gptel-system-prompt "Frozen prompt")
		seen-system)
	    (cl-letf (((symbol-function
			  'mevedel-session-persistence--dynamic-system-preset-p)
			 (lambda () t))
			((symbol-function 'gptel--get-buffer-bounds)
			 (lambda () nil)))
	      (mevedel-session-persistence--save-gptel-state-around
	       (lambda ()
		 (setq seen-system (org-entry-get (point-min) "GPTEL_SYSTEM")))))
	    (should-not seen-system)
	    (let ((text (buffer-substring-no-properties
			 (point-min) (point-max))))
	      (should-not (string-match-p "GPTEL_SYSTEM" text))
	      (should-not (string-match-p "gptel_system" text))
	      (should (string-match-p ":OTHER: Keep" text)))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "removes multiline GPTEL_SYSTEM values"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (org-entry-put (point-min) "GPTEL_SYSTEM"
			 "Frozen first\nFrozen second")
	  (goto-char (point-max))
	  (insert "Body\n")
	  (setq-local mevedel--session
		      (mevedel-session-create
		       "main"
		       (test-mevedel-session-persistence--make-workspace root)))
	  (let ((gptel-system-prompt "Frozen prompt"))
	    (cl-letf (((symbol-function
			  'mevedel-session-persistence--dynamic-system-preset-p)
			 (lambda () t))
			((symbol-function 'gptel--get-buffer-bounds)
			 (lambda () nil)))
	      (mevedel-session-persistence--save-gptel-state-around
	       (lambda () nil))))
	  (let ((text (buffer-substring-no-properties
		       (point-min) (point-max))))
	    (should-not (string-match-p "GPTEL_SYSTEM" text))
	    (should-not (string-match-p "Frozen first" text))
	    (should-not (string-match-p "Frozen second" text))
	    (should (string-match-p "Body" text))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "routes top-level property writes around Org entry helpers"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (insert ":PROPERTIES:\n:GPTEL_SYSTEM: Frozen prompt\n:END:\n")
	  (let (start end)
	    (setq start (point))
	    (insert "Assistant body\n")
	    (setq end (point))
	    (add-text-properties start end '(gptel response))
	    (setq-local mevedel--session
			(mevedel-session-create
			 "main"
			 (test-mevedel-session-persistence--make-workspace root)))
	    (let ((orig-fun
		   (lambda ()
		     (org-entry-put (point-min) "GPTEL_MODEL" "fake-model")
		     (org-entry-delete (point-min) "GPTEL_SYSTEM"))))
	      (cl-letf (((symbol-function
			  'mevedel-session-persistence--dynamic-system-preset-p)
			 (lambda () nil))
			((symbol-function 'gptel--get-buffer-bounds)
			 (lambda () `((response (,start ,end)))))
			((symbol-function 'org-entry-put)
			 (lambda (&rest _)
			   (error "Slow org-entry-put should not run")))
			((symbol-function 'org-entry-delete)
			 (lambda (&rest _)
			   (error "Slow org-entry-delete should not run"))))
		(mevedel-session-persistence--save-gptel-state-around
		 orig-fun)))
	    (let ((text (buffer-substring-no-properties
			 (point-min) (point-max))))
	      (should (string-match-p ":GPTEL_MODEL: fake-model" text))
	      (should (string-match-p ":GPTEL_BOUNDS: " text))
	      (should-not (string-match-p ":GPTEL_SYSTEM:" text)))))
      (when (file-directory-p root)
	(delete-directory root t))))
  :doc "stabilizes GPTEL_BOUNDS after delegated save resizes the property drawer"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* main\n")
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((start (point-marker))
                end
                orig-fun)
            (insert "Assistant body\n")
            (setq end (point-marker))
            (add-text-properties start end '(gptel response))
            (setq orig-fun
                  (lambda ()
                    (org-entry-put (point-min) "GPTEL_BOUNDS"
                                   "((response (1 2)))")))
            (cl-letf (((symbol-function
                        'mevedel-session-persistence--dynamic-system-preset-p)
                       (lambda () nil)))
              (mevedel-session-persistence--save-gptel-state-around orig-fun))
            (pcase-let ((`((response (,beg ,stored-end)))
                         (read (org-entry-get (point-min) "GPTEL_BOUNDS"))))
              (should (= beg (marker-position start)))
              (should (= stored-end (marker-position end))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "delegates unchanged for non-dynamic presets"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Custom prompt")
                delegated-system
                system-present-at-delegate
                orig-fun)
            (setq orig-fun
                  (lambda ()
                    (setq delegated-system gptel-system-prompt)
                    (setq system-present-at-delegate
                          (org-entry-get (point-min) "GPTEL_SYSTEM"))))
            (org-entry-put (point-min) "GPTEL_SYSTEM" "Frozen prompt")
            (cl-letf (((symbol-function
                        'mevedel-session-persistence--dynamic-system-preset-p)
                       (lambda () nil)))
              (mevedel-session-persistence--save-gptel-state-around orig-fun))
            (should (equal "Custom prompt" delegated-system))
            (should (equal "Frozen prompt" system-present-at-delegate))
            (should (equal "Frozen prompt"
                           (org-entry-get (point-min) "GPTEL_SYSTEM")))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "rerenders a live view when gptel metadata shifts transcript positions"
  (let ((view (generate-new-buffer " *mevedel-save-state-view*"))
        (rerenders 0))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session t)
          (setq-local mevedel--view-buffer view)
          (let ((data (current-buffer)))
            (with-current-buffer view
              (setq-local mevedel--data-buffer data))
            (insert "Transcript body\n")
            (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                       (lambda () nil))
                      ((symbol-function 'mevedel-view--full-rerender)
                       (lambda () (cl-incf rerenders))))
              (mevedel-session-persistence--save-gptel-state-around
               (lambda ()
                 (org-entry-put (point-min) "GPTEL_MODEL" "fake-model"))))
            (should (= rerenders 1))))
      (when (buffer-live-p view)
        (kill-buffer view)))))

(mevedel-deftest mevedel-session-persistence--refresh-restored-buffers ()
  ,test
  (test)
  :doc "reverts unmodified visiting buffers after file restore"
  (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (write-region "new\n" nil file nil 'silent)
          (mevedel-session-persistence--refresh-restored-buffers
           (list (list :action 'restore :path file))
           (list :succeeded 1))
          (with-current-buffer buf
            (should (equal "new\n"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "reverts modified visiting buffers after confirmed file restore"
  (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (write-region "new\n" nil file nil 'silent)
          (mevedel-session-persistence--refresh-restored-buffers
           (list (list :action 'restore :path file))
           (list :succeeded 1))
          (with-current-buffer buf
            (should-not (buffer-modified-p))
            (should (equal "new\n"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "kills visiting buffers for deleted restored files"
  (let* ((tempdir (make-temp-file "mevedel-refresh-" t))
         (file (file-name-concat tempdir "source.el"))
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (delete-file file)
          (mevedel-session-persistence--refresh-restored-buffers
           (list (list :action 'delete :path file))
           (list :succeeded 1))
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence--prepare-buffers-for-restore ()
  ,test
  (test)
  :doc "discard marks affected modified buffers unmodified before restore"
  (let* ((tempdir (make-temp-file "mevedel-prepare-" t))
         (file (file-name-concat tempdir "source.el"))
         (plan nil)
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (setq plan (list (list :action 'restore :path file)))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?d)))
            (should (equal plan
                           (mevedel-session-persistence--prepare-buffers-for-restore
                            nil 1 plan))))
          (with-current-buffer buf
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t)))
  :doc "abort returns abort sentinel when affected buffers are modified"
  (let* ((tempdir (make-temp-file "mevedel-prepare-" t))
         (file (file-name-concat tempdir "source.el"))
         (plan nil)
         (buf nil))
    (unwind-protect
        (progn
          (write-region "old\n" nil file nil 'silent)
          (setq buf (find-file-noselect file))
          (setq plan (list (list :action 'restore :path file)))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "local\n"))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?a)))
            (should (eq :abort
                        (mevedel-session-persistence--prepare-buffers-for-restore
                         nil 1 plan))))
          (with-current-buffer buf
            (should (buffer-modified-p))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory tempdir t))))


;;
;;; Phase 3: file-history store

(defun test-mevedel-session-persistence--make-materialized-session ()
  "Create a session, materialize it, return (cons SESSION TEMPDIR).
The session's data buffer is `*test-data-buf*' and is left alive — the
caller must `kill-buffer' it during cleanup.  TEMPDIR holds the entire
workspace tree."
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buf     (generate-new-buffer "*test-data-buf*")))
      (with-current-buffer buf
        (org-mode)
        (insert "Initial prompt\n")
        (mevedel-session-persistence-ensure-files session buf))
      (cons session tempdir))))

(defun test-mevedel-session-persistence--cleanup (tempdir)
  "Tear down a test session: kill data buffer and remove TEMPDIR."
  (when-let ((buf (get-buffer "*test-data-buf*")))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf))
  (when (file-directory-p tempdir)
    (delete-directory tempdir t))
  (mevedel-workspace-clear-registry))

(mevedel-deftest mevedel-file-history--path-hash ()
  ,test
  (test)
  :doc "returns 16 hex chars"
  (let ((h (mevedel-file-history--path-hash "/tmp/foo.el")))
    (should (= 16 (length h)))
    (should (string-match-p "\\`[0-9a-f]+\\'" h)))
  :doc "is deterministic for a given path"
  (should (equal (mevedel-file-history--path-hash "/tmp/foo.el")
                 (mevedel-file-history--path-hash "/tmp/foo.el")))
  :doc "differs across paths"
  (should-not (equal (mevedel-file-history--path-hash "/tmp/foo.el")
                     (mevedel-file-history--path-hash "/tmp/bar.el"))))

(mevedel-deftest mevedel-file-history--backup-name ()
  ,test
  (test)
  :doc "appends @v<N>"
  (let ((n (mevedel-file-history--backup-name "/tmp/x.el" 3)))
    (should (string-match "@v3\\'" n))
    (should (= 19 (length n)))))   ; 16 hex + "@v" + "3" = 19

(mevedel-deftest mevedel-file-history--latest-version ()
  ,test
  (test)
  :doc "returns 0 for unknown path"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (should (= 0 (mevedel-file-history--latest-version
                  session "/tmp/foo")))
    (mevedel-workspace-clear-registry))
  :doc "finds max across multiple turn entries"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id2" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/tmp/foo" . (:backup-name "abc@v1" :version 1))))
            (3 . (("/tmp/foo" . (:backup-name "abc@v3" :version 3))
                  ("/tmp/bar" . (:backup-name "def@v1" :version 1))))
            (2 . (("/tmp/foo" . (:backup-name "abc@v2" :version 2))))))
    (should (= 3 (mevedel-file-history--latest-version session "/tmp/foo")))
    (should (= 1 (mevedel-file-history--latest-version session "/tmp/bar")))
    (should (= 0 (mevedel-file-history--latest-version session "/tmp/baz")))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-file-history-snapshot-modified ()
  ,test
  (test)
  :doc "writes a backup for a modified file"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "foo.el"))
               (pre          (make-hash-table :test #'equal)))
          (write-region "old content" nil tracked-file nil 'silent)
          (puthash tracked-file "old content" pre)
          (write-region "new content" nil tracked-file nil 'silent)
          (let ((written (mevedel-file-history-snapshot-modified
                          session 1 pre)))
            (should (= 1 (length written)))
            (let* ((entry (assoc tracked-file
                                 (cdr (assoc 1 (mevedel-session-file-snapshots
                                                session)))))
                   (backup-name (plist-get (cdr entry) :backup-name))
                   (backup-path (mevedel-file-history--backup-path
                                 (mevedel-session-save-path session)
                                 backup-name)))
              (should backup-name)
              (should (file-exists-p backup-path))
              (with-temp-buffer
                (insert-file-contents-literally backup-path)
                (should (equal "new content" (buffer-string)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "skips unchanged files"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "foo.el"))
               (pre          (make-hash-table :test #'equal)))
          (write-region "same content" nil tracked-file nil 'silent)
          (puthash tracked-file "same content" pre)
          (let ((written (mevedel-file-history-snapshot-modified
                          session 1 pre)))
            (should (null written))
            (should-not (mevedel-session-file-snapshots session))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records absent marker when file deleted during turn"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "gone.el"))
               (pre          (make-hash-table :test #'equal)))
          (puthash tracked-file "had content" pre)
          (mevedel-file-history-snapshot-modified session 2 pre)
          (let* ((entry (assoc tracked-file
                               (cdr (assoc 2 (mevedel-session-file-snapshots
                                              session))))))
            (should entry)
            (should (null (plist-get (cdr entry) :backup-name)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "snapshots a created file (pre-content nil, current exists)"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "new.el"))
               (pre          (make-hash-table :test #'equal)))
          (puthash tracked-file nil pre)
          (write-region "fresh" nil tracked-file nil 'silent)
          (mevedel-file-history-snapshot-modified session 3 pre)
          (let* ((entry (assoc tracked-file
                               (cdr (assoc 3 (mevedel-session-file-snapshots
                                              session))))))
            (should entry)
            (should (plist-get (cdr entry) :backup-name))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "skips files exceeding the size cap"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "huge.el"))
               (pre          (make-hash-table :test #'equal))
               (mevedel-file-history-max-snapshot-bytes 10))
          (write-region (make-string 100 ?x) nil tracked-file nil 'silent)
          (puthash tracked-file nil pre)
          (let ((written (mevedel-file-history-snapshot-modified
                          session 1 pre)))
            (should (null written))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-file-history-evict ()
  ,test
  (test)
  :doc "drops oldest entries beyond the cap"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((mevedel-file-history-max-snapshots 2))
          ;; Pre-populate with 3 turns; create dummy backup files.
          (dotimes (i 3)
            (let* ((turn (1+ i))
                   (path (format "/tmp/file-%d.el" turn))
                   (backup-name (format "%s@v1"
                                        (mevedel-file-history--path-hash path))))
              (mevedel-file-history--write-backup
               (mevedel-session-save-path session) backup-name "x")
              (push (cons turn (list (cons path
                                            (list :backup-name backup-name
                                                  :version 1))))
                    (mevedel-session-file-snapshots session))))
          (mevedel-file-history-evict session)
          (let ((kept (mapcar #'car (mevedel-session-file-snapshots session))))
            (should (= 2 (length kept)))
            ;; Oldest dropped: only highest two turn numbers retained.
            (should (memq 2 kept))
            (should (memq 3 kept))
            (should-not (memq 1 kept)))
          ;; GC should have removed the orphaned v1 file for turn 1's path.
          (let ((dir (file-name-concat
                      (mevedel-session-save-path session) "file-history")))
            (should-not
             (member
              (format "%s@v1"
                      (mevedel-file-history--path-hash "/tmp/file-1.el"))
              (directory-files dir nil "[^.]")))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when count is below cap"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((mevedel-file-history-max-snapshots 100))
          (setf (mevedel-session-file-snapshots session)
                '((1 . (("/x" . (:backup-name "a@v1" :version 1))))))
          (mevedel-file-history-evict session)
          (should (= 1 (length (mevedel-session-file-snapshots session)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when cap is nil"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((mevedel-file-history-max-snapshots nil))
          (dotimes (i 5)
            (push (cons (1+ i) nil) (mevedel-session-file-snapshots session)))
          (mevedel-file-history-evict session)
          (should (= 5 (length (mevedel-session-file-snapshots session)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; Phase 4: split-on-compact

(mevedel-deftest mevedel-session-persistence-rotate-segment ()
  ,test
  (test)
  :doc "creates a new segment file and bumps the segment counter"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (_orig-segment buffer-file-name)
               (new-path (mevedel-session-persistence-rotate-segment
                          session buf "Summary of the prior conversation.")))
          (with-current-buffer buf
            (should new-path)
            (should (= 2 (mevedel-session-current-segment session)))
            (should (file-exists-p new-path))
            ;; Old segment file still exists.
            (let ((seg1 (mevedel-session-persistence--segment-path
                         (mevedel-session-save-path session) 1)))
              (should (file-exists-p seg1))
              ;; Old segment got finalized property
              (with-temp-buffer
                (insert-file-contents seg1)
                (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                        (buffer-string)))))
            ;; New buffer points at the new segment file.
            (should (equal new-path buffer-file-name))
            ;; Buffer body contains the summary.
            (should (string-match-p "Summary of the prior conversation."
                                    (buffer-string)))
            ;; Buffer also contains the segment-2 number property.
            (should (string-match-p "MEVEDEL_SEGMENT_NUMBER:[ \t]*2"
                                    (buffer-string)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before editing"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
          (should (mevedel-session-persistence-rotate-segment
                   session buf "Summary after stale modtime."))
          (with-current-buffer buf
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before deleting pending text"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-visited-file-modtime)
            (set-buffer-modified-p nil)
            (goto-char (point-max))
            (insert "\nPending prompt")
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
	          (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
	                     (lambda (&rest _args)
	                       (error "Supersession prompt"))))
            (should (mevedel-session-persistence-rotate-segment
                     session buf "Summary after stale pending text."
                     :pending-text "\nPending prompt")))
          (with-current-buffer buf
            (should (string-suffix-p "Pending prompt\n" (buffer-string)))
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "signals a controlled error when current segment differs on disk"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (write-region "external edit\n" nil buffer-file-name nil 'silent)
            (should-not (verify-visited-file-modtime buf)))
          (should-error
           (mevedel-session-persistence-rotate-segment
            session buf "Summary should not be written.")
           :type 'error))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "signals a controlled error when current segment was deleted on disk"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (delete-file buffer-file-name)
            (should-not (file-exists-p buffer-file-name)))
          (should-error
           (mevedel-session-persistence-rotate-segment
            session buf "Summary should not be written.")
           :type 'error)
          (should (= 1 (mevedel-session-current-segment session))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "sidecar reflects bumped current-segment after rotation"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-rotate-segment
           session buf "First summary.")
          (let ((plist (mevedel-session-persistence-read
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))))
            (should (= 2 (plist-get plist :current-segment)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "two consecutive rotations produce three segment files"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-rotate-segment session buf "Summary A.")
          (mevedel-session-persistence-rotate-segment session buf "Summary B.")
          (let ((dir (mevedel-session-save-path session)))
            (should (file-exists-p
                     (mevedel-session-persistence--segment-path dir 1)))
            (should (file-exists-p
                     (mevedel-session-persistence--segment-path dir 2)))
            (should (file-exists-p
                     (mevedel-session-persistence--segment-path dir 3)))
            (should (= 3 (mevedel-session-current-segment session)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when session is not materialized"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "no-mat" "/tmp/x" "x"))
         (session (mevedel-session-create "main" workspace))
         (buf     (generate-new-buffer "*test-rotate-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (null (mevedel-session-persistence-rotate-segment
                         session buf "Won't happen."))))
      (kill-buffer buf)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-start-fresh-segment ()
  ,test
  (test)
  :doc "creates an empty new segment without a compaction summary"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (new-path (mevedel-session-persistence-start-fresh-segment
                          session buf :initial-text "### ")))
          (with-current-buffer buf
            (should new-path)
            (should (= 2 (mevedel-session-current-segment session)))
            (should (file-exists-p new-path))
            (should (equal new-path buffer-file-name))
            (should (string-match-p "MEVEDEL_SEGMENT_NUMBER:[ \t]*2"
                                    (buffer-string)))
            (should (string-suffix-p "### " (buffer-string)))
            (should-not (string-match-p "#\\+begin_summary"
                                        (buffer-string)))
            (with-temp-buffer
              (insert-file-contents new-path)
              (should-not (string-match-p "### " (buffer-string))))
            (let ((seg1 (mevedel-session-persistence--segment-path
                         (mevedel-session-save-path session) 1)))
              (should (file-exists-p seg1))
              (with-temp-buffer
                (insert-file-contents seg1)
                (should (string-match-p "Initial prompt"
                                        (buffer-string)))
                (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                        (buffer-string)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before fresh segment edit"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
          (should (mevedel-session-persistence-start-fresh-segment
                   session buf :initial-text "### "))
          (with-current-buffer buf
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "sidecar and prompt index point at the new empty segment"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-start-fresh-segment
           session buf :initial-text "### ")
          (let ((plist (mevedel-session-persistence-read
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))))
            (should (= 2 (plist-get plist :current-segment)))
            (should-not (assoc 2 (plist-get plist :prompt-index)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes finalized segment prompt index before bumping segment"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (setf (mevedel-session-prompt-index session)
                '((1 . ((:turn 1 :pos 999 :preview "stale")))))
          (with-current-buffer buf
            (goto-char (point-max))
            (let ((response-start (point)))
              (insert "\nAssistant response\n")
              (put-text-property response-start (point) 'gptel 'response))
            (insert "\nFresh unsaved prompt\n"))
          (mevedel-session-persistence-start-fresh-segment
           session buf :initial-text "### ")
          (let* ((plist (mevedel-session-persistence-read
                         (mevedel-session-persistence--sidecar-path
                          (mevedel-session-save-path session))))
                 (seg1 (cdr (assoc 1 (plist-get plist :prompt-index)))))
            (should seg1)
            (should (equal "Initial prompt" (plist-get (car seg1) :preview)))
            (should (equal "Fresh unsaved prompt"
                           (plist-get (cadr seg1) :preview)))
            (should-not (equal 999 (plist-get (car seg1) :pos)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when session is not materialized"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "fresh-no-mat" "/tmp/x" "x"))
         (session (mevedel-session-create "main" workspace))
         (buf     (generate-new-buffer "*test-fresh-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (null (mevedel-session-persistence-start-fresh-segment
                         session buf :initial-text "### "))))
      (kill-buffer buf)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-rollback ()
  ,test
  (test)
  :doc "rolls live buffer and segment counter back on sidecar write failure"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (old-segment (with-current-buffer buf buffer-file-name))
               (old-text (with-current-buffer buf
                           (buffer-substring (point-min) (point-max)))))
	          (cl-letf (((symbol-function 'mevedel-session-persistence-write)
	                     (lambda (&rest _)
	                       (error "Sidecar write failed"))))
            (should-error
             (mevedel-session-persistence-rotate-segment
              session buf "Summary that will not commit.")))
          (with-current-buffer buf
            (should (= 1 (mevedel-session-current-segment session)))
            (should (equal old-segment buffer-file-name))
            (should (equal old-text
                           (buffer-substring (point-min) (point-max)))))
          (should-not
           (file-exists-p
            (mevedel-session-persistence--segment-path
             (mevedel-session-save-path session) 2))))
      (test-mevedel-session-persistence--cleanup tempdir)))

  :doc "restores sidecar when failure happens after sidecar publish"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (sidecar (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session))))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--save-instructions)
	                     (lambda (&rest _)
	                       (error "Instruction save failed"))))
            (should-error
             (mevedel-session-persistence-rotate-segment
              session buf "Summary that will not commit.")))
          (let ((plist (mevedel-session-persistence-read sidecar)))
            (should (= 1 (mevedel-session-current-segment session)))
            (should (= 1 (plist-get plist :current-segment))))
          (with-current-buffer buf
            (should (equal
                     (mevedel-session-persistence--segment-path
                      (mevedel-session-save-path session) 1)
                     buffer-file-name)))
          (should-not
           (file-exists-p
            (mevedel-session-persistence--segment-path
             (mevedel-session-save-path session) 2))))
      (test-mevedel-session-persistence--cleanup tempdir)))

  :doc "restores pending prompt when predecessor save fails"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (old-segment (with-current-buffer buf buffer-file-name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "Pending prompt\n"))
            (set-buffer-modified-p t))
	          (cl-letf (((symbol-function 'save-buffer)
	                     (lambda (&rest _)
	                       (error "Save failed"))))
            (should-error
             (mevedel-session-persistence-rotate-segment
              session buf "Summary."
              :pending-text "Pending prompt\n")))
          (with-current-buffer buf
            (should (= 1 (mevedel-session-current-segment session)))
            (should (equal old-segment buffer-file-name))
            (should (string-match-p "Pending prompt" (buffer-string)))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence--summary-block ()
  ,test
  (test)
  :doc "wraps summary in #+begin_summary block"
  (let ((wrapped (mevedel-session-persistence--summary-block "hello")))
    (should (string-match-p "#\\+begin_summary" wrapped))
    (should (string-match-p "#\\+end_summary" wrapped))
    (should (string-match-p "Another language model started" wrapped))
    (should (string-match-p "hello" wrapped)))
  :doc "marker lines carry gptel ignore property"
  (let ((wrapped (mevedel-session-persistence--summary-block "x")))
    ;; The first character is in the begin_summary marker.
    (should (eq 'ignore (get-text-property 0 'gptel wrapped)))))

(mevedel-deftest mevedel-session-persistence--strip-summary-handoff-prefix ()
  ,test
  (test)
  :doc "removes the model-facing handoff prefix before summary reuse"
  (let* ((summary "## Goal\n- continue")
         (prefixed (concat mevedel-session-persistence--summary-handoff-prefix
                           summary)))
    (should (equal summary
                   (mevedel-session-persistence--strip-summary-handoff-prefix
                    prefixed)))
    (should (equal summary
                   (mevedel-session-persistence--strip-summary-handoff-prefix
                    summary)))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-tail ()
  ,test
  (test)
  :doc "rotates into summary followed by preserved tail and pending prompt"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-persistence-rotate-segment
           session buf "Summary."
           :tail-text "Tail turn.\n"
           :pending-text "Pending prompt.\n")
          (with-current-buffer buf
            (let ((text (buffer-string)))
              (should (string-match-p "#\\+begin_summary mevedel-role=compaction-summary" text))
              (should (string-match-p "Summary\\." text))
              (should (string-match-p "Tail turn\\." text))
              (should (string-match-p "Pending prompt\\." text)))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-pending-save ()
  ,test
  (test)
  :doc "pending prompts are not saved before request completion"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Old prompt\n")
              (insert (propertize "Old response\n" 'gptel 'response))
              (insert "Pending prompt\n"))
            (set-buffer-modified-p t))
          (mevedel-session-persistence-rotate-segment
           session buf "Summary."
           :pending-text "Pending prompt\n")
          (let ((seg1 (mevedel-session-persistence--segment-path
                       (mevedel-session-save-path session) 1))
                (seg2 (mevedel-session-persistence--segment-path
                       (mevedel-session-save-path session) 2)))
            (with-temp-buffer
              (insert-file-contents seg1)
              (should-not (string-match-p "Pending prompt" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents seg2)
              (should-not (string-match-p "Pending prompt" (buffer-string))))
            (with-current-buffer buf
              (should (string-match-p "Pending prompt" (buffer-string)))
              (should-not (buffer-modified-p)))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence-rotate-segment-tail-index ()
  ,test
  (test)
  :doc "copied tail prompts do not consume new cumulative turn ids"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*"))
              (tail-text
               (concat
                "Tail prompt 1\n"
                (propertize "Tail response 1\n" 'gptel 'response)
                "Tail prompt 2\n"
                (propertize "Tail response 2\n" 'gptel 'response))))
          (setf (mevedel-session-turn-count session) 10)
          (setf (mevedel-session-prompt-index session)
                (list
                 (cons 1
                       (cl-loop for turn from 1 to 10
                                collect
                                (list :turn turn
                                      :cum-turn turn
                                      :pos turn
                                      :preview (format "Prompt %d" turn))))))
          (mevedel-session-persistence-rotate-segment
           session buf "Summary."
           :tail-text tail-text
           :pending-text "Next real prompt\n")
          (mevedel-session-persistence--update-prompt-index session buf)
          (let ((seg2 (cdr (assoc 2 (mevedel-session-prompt-index session)))))
            (should (= 1 (length seg2)))
            (should (= 1 (plist-get (car seg2) :turn)))
            (should (= 3 (plist-get (car seg2) :file-turn)))
            (should (= 11 (plist-get (car seg2) :cum-turn)))
            (should (equal "Next real prompt"
                           (plist-get (car seg2) :preview)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; Phase 5: read path

(mevedel-deftest mevedel-session-persistence-load-sidecar ()
  ,test
  (test)
  :doc "reads a current-version sidecar"
  (let ((tmp (make-temp-file "mevedel-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write
           tmp (test-mevedel-session-persistence--complete-sidecar
                '(:session-name "x")))
          (let ((plist (mevedel-session-persistence-load-sidecar tmp)))
            (should (equal (mevedel-version) (plist-get plist :version)))
            (should (equal "x" (plist-get plist :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "rejects an unsupported sidecar version"
  (let ((tmp (make-temp-file "mevedel-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write
           tmp '(:version "v0.0.0" :session-name "x"))
          (should-error
           (mevedel-session-persistence-load-sidecar tmp)
           :type 'error))
      (when (file-exists-p tmp) (delete-file tmp)))))

(mevedel-deftest mevedel-session-persistence-restore ()
  ,test
  (test)
  :doc "restores stale rows as lost but supersedes rows with newer facts"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "First user prompt\n")
                  (insert
                   (mevedel-pipeline--format-render-data-block
                    '(:execution-id "exec-stale" :state running
                      :status success :live-execution-p t)))
                  (insert
                   (mevedel-pipeline--format-render-data-block
                    '(:execution-id "exec-tail" :state running
                      :status success :live-execution-p t)))
                  (mevedel-session-persistence-save session buf)
                  (mevedel-session-persistence-rotate-segment
                   session buf "Earlier conversation")
                  (insert "Second user prompt\n")
                  (insert
                   (mevedel-pipeline--format-render-data-block
                    '(:execution-id "exec-current" :state running
                      :status success :live-execution-p t)))
                  (insert
                   (mevedel-pipeline--format-render-data-block
                    '(:execution-id "exec-tail" :state completed
                      :status success :live-execution-p nil)))
                  (insert
                   (mevedel--format-hook-audit-record
                    '(:type execution-completion
                      :tool-use-id "archived-call"
                      :render-data (:execution-id "exec-stale"
                                    :state completed
                                    :live-execution-p nil))))
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                ;; Release the lock + kill the buffer (the test buffer didn't
                ;; go through chat-buffer-init-common so the kill-hook isn't
                ;; installed; we mirror its work manually).
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (should (file-exists-p session-dir))
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (derived-mode-p 'org-mode))
                  (should (bound-and-true-p gptel-mode))
                  (should mevedel--session)
                  (should (equal "main"
                                 (mevedel-session-name mevedel--session)))
                  (should (= 2 (mevedel-session-current-segment
                                mevedel--session)))
                  (should-not (mevedel-session-execution-state
                               mevedel--session))
                  (should (string-match-p "Second user prompt"
                                          (buffer-string)))
                  (should (string-match-p ":state lost"
                                          (buffer-string))))
                (with-temp-buffer
                  (insert-file-contents
                   (mevedel-session-persistence--segment-path
                    session-dir 1))
                  (goto-char (point-min))
                  (should (= 2 (how-many ":state archived"))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))
  :doc "retargets and persists a missing working directory"
  (cl-destructuring-bind
      (_workspace tempdir _missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (prompt dir default mustmatch &rest _)
                         (should (string-match-p
                                  "deleted-worktree.*missing" prompt))
                         (should (equal tempdir
                                        (file-name-as-directory dir)))
                         (should (equal tempdir
                                        (file-name-as-directory default)))
                         (should mustmatch)
                         replacement-dir)))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (with-current-buffer restored
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-persistence--sidecar-path session-dir))))
              (should (equal replacement-dir
                             (plist-get sidecar :working-directory)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "does not persist a retargeted directory in read-only mode"
  (cl-destructuring-bind
      (_workspace tempdir missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) replacement-dir))
                      ((symbol-function
                        'mevedel-session-persistence-lock-acquire)
                       (lambda (&rest _) nil)))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (with-current-buffer restored
              (should mevedel-session--read-only-mode)
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-persistence--sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "rejects an invalid replacement before opening the session"
  (cl-destructuring-bind
      (workspace tempdir missing-dir _replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let ((outside (make-temp-file "mevedel-cwd-outside-" t))
          (buf-name (mevedel-session-buffer-name "main" workspace)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) outside)))
              (should-error
               (mevedel-session-persistence-restore session-dir)
               :type 'user-error))
            (should-not
             (file-exists-p
              (mevedel-session-persistence--lock-path session-dir)))
            (should-not (get-buffer buf-name))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-persistence--sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (when (file-directory-p outside)
          (delete-directory outside t))
        (mevedel-workspace-clear-registry))))
  :doc "round-trips a multi-segment (compacted) session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Original prompt\n")
                  (mevedel-session-persistence-save session buf)
                  (mevedel-session-persistence-rotate-segment
                   session buf "Summary of segment 1.")
                  (insert "After-compact prompt\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (with-current-buffer restored
                  (should (= 2 (mevedel-session-current-segment
                                mevedel--session)))
                  (should (string-match-p "Summary of segment 1\\."
                                          (buffer-string)))
                  (should (string-match-p "After-compact prompt"
                                          (buffer-string)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "preserves permission rules across resume"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (setf (mevedel-session-permission-rules session)
                      '(("Read" :path "/tmp/foo/**" :action allow)))
                (with-current-buffer buf
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (with-current-buffer restored
                  (should (equal '(("Read" :path "/tmp/foo/**" :action allow))
                                 (mevedel-session-permission-rules
                                  mevedel--session)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "does not double-rewrite nested relocated working directories"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "mevedel-old-root-" t)))
         (workspace-id (format "nested-restore-id-%s" (gensym)))
         (new-root (file-name-as-directory
                    (file-name-concat old-root "packages" "api")))
         (old-cwd (file-name-as-directory
                   (file-name-concat old-root "src")))
         (expected-cwd (file-name-as-directory
                        (file-name-concat new-root "src")))
         buf session session-dir restored)
    (unwind-protect
        (progn
          (make-directory old-cwd t)
          (make-directory expected-cwd t)
          (mevedel-workspace-clear-registry)
          (let ((workspace (mevedel-workspace-get-or-create
                            'project workspace-id old-root "nested-proj")))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-working-directory session) old-cwd))
          (setq buf (generate-new-buffer "*test-data-buf*"))
          (with-current-buffer buf
            (org-mode)
            (insert "Nested relocation\n")
            (mevedel-session-persistence-save session buf))
          (setq session-dir (mevedel-session-save-path session))
          (test-mevedel-session-persistence--release-and-kill
           buf session)
          (setq buf nil)
          (mevedel-workspace-clear-registry)
          (mevedel-workspace-get-or-create
           'project workspace-id new-root "nested-proj")
          (setq restored (mevedel-session-persistence-restore
                          session-dir))
          (with-current-buffer restored
            (should (equal expected-cwd
                           (mevedel-session-working-directory
                            mevedel--session)))))
      (test-mevedel-session-persistence--release-and-kill
       buf session)
      (test-mevedel-session-persistence--release-and-kill
       restored
       (and restored (buffer-local-value 'mevedel--session restored)))
      (when (file-directory-p old-root)
        (delete-directory old-root t))
      (mevedel-workspace-clear-registry)))
  :doc "switches to a live buffer instead of re-loading"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf-name (mevedel-session-buffer-name "main" workspace))
               (buf      (get-buffer-create buf-name))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Live buffer\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                ;; Restore should return the existing live buffer.
                (should (eq buf restored)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 6: locking

(mevedel-deftest mevedel-session-persistence--same-host-lock-active-p ()
  ,test
  (test)
  :doc "returns nil for dead PIDs"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) nil)))
    (should-not
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345
            :emacs-invocation-time "2026-04-23T14-30-15"))))
  :doc "keeps live PIDs active when process start predates lock time"
  (let* ((lock-time (current-time))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (time-subtract lock-time 10))))
      (should
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "keeps live PIDs active within timestamp tolerance"
  (let* ((lock-time (current-time))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (time-add lock-time 1))))
      (should
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "treats live PIDs as stale when process start proves PID reuse"
  (let* ((lock-time (time-subtract (current-time) (* 30 24 60 60)))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (current-time))))
      (should-not
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "keeps live PIDs active when process start is unavailable"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) t))
            ((symbol-function
              'mevedel-session-persistence--pid-start-time)
             (lambda (&rest _) nil)))
    (should
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345
            :emacs-invocation-time "2026-04-23T14-30-15"))))
  :doc "keeps live PIDs active when lock time is malformed"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) t))
            ((symbol-function
              'mevedel-session-persistence--pid-start-time)
             (lambda (&rest _) (current-time))))
    (should
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345 :emacs-invocation-time "old")))))

(mevedel-deftest mevedel-session-persistence--active-lock-p ()
  ,test
  (test)
  :doc "treats cross-host locks as active without local PID checks"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "old"
                         :buffer "*remote*")
                   (current-buffer)))
          (should (mevedel-session-persistence--active-lock-p tempdir)))
      (delete-directory tempdir t)))
  :doc "treats same-host reused-PID locks as inactive"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir))
               (lock-time (time-subtract (current-time) (* 30 24 60 60))))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*reused*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (should-not
             (mevedel-session-persistence--active-lock-p tempdir))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence-lock-acquire ()
  ,test
  (test)
  :doc "writes a fresh lock when none exists"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (progn
          (should (mevedel-session-persistence-lock-acquire
                   tempdir "*test-buf*"))
          (let ((lock-path
                 (mevedel-session-persistence--lock-path tempdir)))
            (should (file-exists-p lock-path))
            (let ((plist (mevedel-session-persistence--read-lock lock-path)))
              (should (= (emacs-pid) (plist-get plist :pid)))
              (should (equal "*test-buf*" (plist-get plist :buffer))))))
      (delete-directory tempdir t)))
  :doc "unreadable raced lock signals instead of recursing"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-session-persistence--read-lock)
                   (lambda (&rest _) nil))
                  ((symbol-function
                    'mevedel-session-persistence--write-lock-atomic)
                   (lambda (&rest _) nil)))
          (should-error
           (mevedel-session-persistence-lock-acquire tempdir "*test-buf*")
           :type 'user-error))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [b]reak overwrites the lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          ;; Plant a lock with a live PID on this host.
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?b)))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*test-buf*")))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*test-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [r]ead-only returns nil and preserves lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?r)))
            (should (null (mevedel-session-persistence-lock-acquire
                           tempdir "*test-buf*"))))
          ;; Original lock untouched.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (equal "*other-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [a]bort signals user-error"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?a)))
            (should-error
             (mevedel-session-persistence-lock-acquire
              tempdir "*test-buf*")
             :type 'user-error)))
      (delete-directory tempdir t)))
  :doc "same-host reused PID follows the stale-lock confirmation path"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir))
               (lock-time (time-subtract (current-time) (* 30 24 60 60))))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _)
                       (error "Unexpected live-lock prompt")))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*new-buf*")))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*new-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "breaks a stale lock when user confirms"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          ;; Plant a lock with a hostname-mismatching PID-alive predicate
          ;; stubbed nil so the stale-lock branch fires deterministically.
          (with-temp-file lock-path
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*new-buf*")))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*new-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "leaves a stale lock alone when user declines"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-persistence-lock-acquire
              tempdir "*new-buf*")
             :type 'user-error))
          ;; Original lock remains untouched.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= 999999 (plist-get plist :pid)))))
      (delete-directory tempdir t)))
  :doc "cross-host: read-only response returns nil"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "..."
                         :buffer "*remote-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?r)))
            (should (null (mevedel-session-persistence-lock-acquire
                           tempdir "*test-buf*"))))
          ;; The remote lock is still in place.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (equal "other-host" (plist-get plist :hostname)))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence-lock-release ()
  ,test
  (test)
  :doc "deletes our own lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (mevedel-session-persistence-lock-acquire tempdir "*x*")
          (should (file-exists-p lock-path))
          (mevedel-session-persistence-lock-release tempdir)
          (should-not (file-exists-p lock-path)))
      (delete-directory tempdir t)))
  :doc "leaves alien locks alone"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :buffer "*x*")
                   (current-buffer)))
          (mevedel-session-persistence-lock-release tempdir)
          ;; Lock still present.
          (should (file-exists-p lock-path)))
      (delete-directory tempdir t)))
  :doc "is a no-op when no lock exists"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (progn
          ;; Should not error.
          (mevedel-session-persistence-lock-release tempdir)
          (should-not (file-exists-p
                       (mevedel-session-persistence--lock-path tempdir))))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence--sweep-stale-locks ()
  ,test
  (test)
  :doc "removes same-host dead-PID lock files silently"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (stale-dir    (file-name-as-directory
                              (file-name-concat sessions-dir "stale-sess")))
               (stale-lock   (file-name-concat stale-dir ".lock")))
          (make-directory stale-dir t)
          (with-temp-file stale-lock
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*gone*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (mevedel-session-persistence--sweep-stale-locks workspace))
          (should-not (file-exists-p stale-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "removes same-host reused-PID lock files silently"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (stale-dir    (file-name-as-directory
                              (file-name-concat sessions-dir "reused-sess")))
               (stale-lock   (file-name-concat stale-dir ".lock"))
               (lock-time    (time-subtract (current-time) (* 30 24 60 60))))
          (make-directory stale-dir t)
          (with-temp-file stale-lock
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*reused*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (mevedel-session-persistence--sweep-stale-locks workspace))
          (should-not (file-exists-p stale-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "leaves same-host live-PID locks alone"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (live-dir     (file-name-as-directory
                              (file-name-concat sessions-dir "live-sess")))
               (live-lock    (file-name-concat live-dir ".lock")))
          (make-directory live-dir t)
          (with-temp-file live-lock
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "new"
                         :buffer "*live*")
                   (current-buffer)))
          (mevedel-session-persistence--sweep-stale-locks workspace)
          (should (file-exists-p live-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "leaves cross-host locks alone"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
                              workspace))
               (remote-dir   (file-name-as-directory
                              (file-name-concat sessions-dir "remote-sess")))
               (remote-lock  (file-name-concat remote-dir ".lock")))
          (make-directory remote-dir t)
          (with-temp-file remote-lock
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "..."
                         :buffer "*remote*")
                   (current-buffer)))
          (mevedel-session-persistence--sweep-stale-locks workspace)
          (should (file-exists-p remote-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-ensure-files-acquires-lock ()
  ,test
  (test)
  :doc "lazy materialization writes the .lock file"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hi\n")
                (let ((path (mevedel-session-persistence-ensure-files
                             session buf)))
                  (should (file-exists-p
                           (mevedel-session-persistence--lock-path path)))))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 7: rewind picker

(mevedel-deftest mevedel-session-persistence--collect-prompts ()
  ,test
  (test)
  :doc "extracts user prompt regions in document order"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Sure, I'll do that.\n" 'gptel 'response))
    (insert "Second prompt\n")
    (insert (propertize "Okay.\n" 'gptel 'response))
    (insert "Third prompt\n")
    (let ((prompts (mevedel-session-persistence--collect-prompts
                    (current-buffer))))
      (should (= 3 (length prompts)))
      (should (= 1 (plist-get (nth 0 prompts) :turn)))
      (should (= 2 (plist-get (nth 1 prompts) :turn)))
      (should (= 3 (plist-get (nth 2 prompts) :turn)))
      (should (string-match-p "First prompt"
                              (plist-get (nth 0 prompts) :preview)))
      (should (string-match-p "Third prompt"
                              (plist-get (nth 2 prompts) :preview)))))
  :doc "skips blank-only regions"
  (with-temp-buffer
    (insert "   \n\n  \t\n")
    (insert (propertize "response" 'gptel 'response))
    (insert "Real prompt\n")
    (let ((prompts (mevedel-session-persistence--collect-prompts
                    (current-buffer))))
      (should (= 1 (length prompts)))
      (should (string-match-p "Real prompt"
                              (plist-get (car prompts) :preview)))))
  :doc "skips indented leading property drawer"
  (with-temp-buffer
    (insert "  :PROPERTIES:\n")
    (insert "  :MEVEDEL_SESSION: metadata\n")
    (insert "  :END:\n")
    (let ((prompt-start (point)))
      (insert "Real prompt after metadata\n")
      (insert (propertize "response" 'gptel 'response))
      (let ((prompts (mevedel-session-persistence--collect-prompts
                      (current-buffer))))
        (should (= 1 (length prompts)))
        (should (= prompt-start (plist-get (car prompts) :pos)))
        (should (equal "Real prompt after metadata"
                       (plist-get (car prompts) :preview))))))
  :doc "skips unpropertized gptel org tool and reasoning scaffolding"
  (with-temp-buffer
    (insert "Fetch a page\n")
    (insert (propertize "Initial answer text.\n" 'gptel 'response))
    (insert "#+begin_reasoning\nThinking text.\n")
    (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
    (insert (propertize
             "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\nbody\n"
             'gptel '(tool . "call_1")))
    (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
    (insert "Search for docs\n")
    (insert (propertize "Second answer.\n" 'gptel 'response))
    (let ((prompts (mevedel-session-persistence--collect-prompts
                    (current-buffer))))
      (should (= 2 (length prompts)))
      (should (equal "Fetch a page"
                     (plist-get (nth 0 prompts) :preview)))
      (should (equal "Search for docs"
                     (plist-get (nth 1 prompts) :preview)))))
  :doc "keeps user-authored org block marker as prompt start"
  (with-temp-buffer
    (let ((prompt-start (point)))
      (insert "#+begin_src emacs-lisp\n")
      (insert "(message \"hello\")\n")
      (insert "#+end_src\n")
      (insert (propertize "Response.\n" 'gptel 'response))
      (let ((prompts (mevedel-session-persistence--collect-prompts
                      (current-buffer))))
        (should (= 1 (length prompts)))
        (should (= prompt-start (plist-get (car prompts) :pos)))
        (should (equal "#+begin_src emacs-lisp"
                       (plist-get (car prompts) :preview)))))))

(mevedel-deftest mevedel-session-persistence--update-prompt-index ()
  ,test
  (test)
  :doc "updates only the live segment's entry"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                ;; Pre-seed with a finalized segment 1 entry.
                (setf (mevedel-session-prompt-index session)
                      '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                              :pos 1 :preview "old prompt")))))
                (setf (mevedel-session-current-segment session) 2)
                (insert "New live prompt\n")
                (mevedel-session-persistence--update-prompt-index
                 session buf)
                (let ((index (mevedel-session-prompt-index session)))
                  ;; Segment 1 untouched.
                  (should (= 1 (length (cdr (assoc 1 index)))))
                  ;; Segment 2 has the new prompt.
                  (should (assoc 2 index))
                  (should (= 1 (length (cdr (assoc 2 index)))))
                  (should
                   (string-match-p
                    "New live prompt"
                    (plist-get (car (cdr (assoc 2 index))) :preview)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--latest-user-message-from-index ()
  ,test
  (test)
  :doc "returns newest prompt by cumulative turn"
  (should
   (equal "third"
          (mevedel-session-persistence--latest-user-message-from-index
           '((2 . ((:turn 1 :cum-turn 3 :preview "third")))
             (1 . ((:turn 1 :cum-turn 1 :preview "first")
                   (:turn 2 :cum-turn 2 :preview "second")))))))
  :doc "ignores blank previews"
  (should
   (null (mevedel-session-persistence--latest-user-message-from-index
          '((1 . ((:turn 1 :preview "   "))))))))

(mevedel-deftest mevedel-session-persistence--prompt-candidates ()
  ,test
  (test)
  :doc "returns flat alist with unique display strings across segments"
  (let ((session (mevedel-session-create
                  "main" (mevedel-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (mevedel-session-prompt-index session)
          '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                   :pos 0 :preview "alpha")
                  (:turn 2 :file-turn 2 :cum-turn 2
                   :pos 100 :preview "beta")))
            (2 . ((:turn 1 :file-turn 1 :cum-turn 3
                   :pos 0 :preview "alpha")
                  (:turn 2 :file-turn 2 :cum-turn 4
                   :pos 50 :preview "gamma")))))
    (let ((candidates
           (mevedel-session-persistence--prompt-candidates session)))
      (should (= 4 (length candidates)))
      ;; All display strings unique (segment + turn folded in).
      (should (= 4 (length (cl-delete-duplicates
                            (mapcar #'car candidates) :test #'equal))))
      ;; Newest prompt in the newest segment first.
      (let* ((first (car candidates))
             (plist (cdr first)))
        (should (= 2 (plist-get plist :segment)))
        (should (= 2 (plist-get plist :turn)))))
    (mevedel-workspace-clear-registry))
  :doc "preserves raw file turn for compacted segments with copied tail"
  (let ((session (mevedel-session-create
                  "main" (mevedel-workspace-get-or-create
                          'project "x" "/tmp" "x"))))
    (setf (mevedel-session-prompt-index session)
          '((2 . ((:turn 1 :file-turn 3 :cum-turn 11
                   :pos 100 :preview "after tail")))))
    (let* ((candidate
            (car (mevedel-session-persistence--prompt-candidates session)))
           (plist (cdr candidate)))
      (should (= 1 (plist-get plist :turn)))
      (should (= 3 (plist-get plist :file-turn))))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-session-persistence--find-turn-cutoff ()
  ,test
  (test)
  :doc "returns position of next user prompt"
  (with-temp-buffer
    (insert "First prompt\n")                               ; pos 1, turn 1
    (let ((next-prompt-pos
           (progn
             (insert (propertize "Response 1.\n" 'gptel 'response))
             (point))))
      (insert "Second prompt\n")                            ; turn 2 starts here
      (insert (propertize "Response 2.\n" 'gptel 'response))
      ;; Cutoff for turn 1 is the start of turn 2's prompt.
      (should (= next-prompt-pos
                 (mevedel-session-persistence--find-turn-cutoff 1)))))
  :doc "returns point-max when turn-n is the last"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Response.\n" 'gptel 'response))
    (insert "Last prompt\n")
    (should (= (point-max)
               (mevedel-session-persistence--find-turn-cutoff 2))))
  :doc "skips unpropertized gptel org tool and reasoning scaffolding"
  (with-temp-buffer
    (insert "Fetch a page\n")
    (insert (propertize "Initial answer text.\n" 'gptel 'response))
    (insert "#+begin_reasoning\nThinking text.\n")
    (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
    (insert (propertize
             "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\nbody\n"
             'gptel '(tool . "call_1")))
    (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
    (let ((next-prompt-pos (point)))
      (insert "Search for docs\n")
      (insert (propertize "Second answer.\n" 'gptel 'response))
      (should (= next-prompt-pos
                 (mevedel-session-persistence--find-turn-cutoff 1)))))
  :doc "stays consistent with transcript-repaired assistant fragments"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Initial answer.\n" 'gptel 'response))
    (insert (propertize "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n"
                        'gptel '(tool . "call_1")))
    (insert "Conti")
    (insert (propertize "nuing the answer.\n" 'gptel 'response))
    (let ((next-prompt-pos (point)))
      (insert "Second prompt\n")
      (insert (propertize "Second answer.\n" 'gptel 'response))
      (let ((prompts (mevedel-session-persistence--collect-prompts
                      (current-buffer))))
        (should (= 2 (length prompts)))
        (should (equal "Second prompt"
                       (plist-get (nth 1 prompts) :preview)))
        (should (= next-prompt-pos
                   (mevedel-session-persistence--find-turn-cutoff 1)))))))

(mevedel-deftest mevedel-rewind ()
  ,test
  (test)
  :doc "errors when no current session"
  (with-temp-buffer
    (let ((mevedel--session nil))
      (should-error (mevedel-rewind) :type 'user-error)))
  :doc "refuses before the picker while executions remain live"
  (let ((buffer (generate-new-buffer " *execution-rewind*"))
        (session (mevedel-session--create :name "rewind")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-execution-session-live-p)
                     (lambda (_session) t)))
            (let ((err (should-error (mevedel-rewind) :type 'user-error)))
              (should (string-match-p
                       "/ps or /stop" (error-message-string err))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))))
  :doc "errors when request in flight"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local mevedel--session session)
                (let ((mevedel--current-request 'placeholder))
                  (should-error (mevedel-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "errors when no recorded prompts"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (setq-local mevedel--session session)
                (let ((mevedel--current-request nil))
                  (should-error (mevedel-rewind) :type 'user-error)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "from view buffer rewinds the data buffer and rerenders the view"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer "*test-data-buf*"))
               (view-buf (generate-new-buffer "*test-view-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local mevedel--session session)
                  (insert "First prompt\n")
                  (insert (propertize "First reply.\n" 'gptel 'response))
                  (insert "Second prompt\n")
                  (insert (propertize "Second reply.\n" 'gptel 'response))
                  (mevedel-session-persistence-save session data-buf))
                (mevedel-view--setup view-buf data-buf)
                (let ((choice
                       (caar (last (mevedel-session-persistence--prompt-candidates
                                    session))))
                      loaded-buffer loaded-segment loaded-turn)
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (&rest _args) choice))
                            ((symbol-function
                              'mevedel-session-persistence--load-truncated)
                             (lambda (_session buffer segment turn
                                               &optional _cum-turn
                                               _logical-turn)
                               (setq loaded-buffer buffer)
                               (setq loaded-segment segment)
                               (setq loaded-turn turn)
                               (with-current-buffer buffer
                                 (let ((inhibit-read-only t))
                                   (erase-buffer)
                                   (insert "First prompt\n")
                                   (insert
                                    (mevedel--format-hook-audit-record
                                     '(:type prompt-rewrite
                                       :event "UserPromptSubmit"
                                       :original "first original"
                                       :submitted "First prompt")))
                                   (insert (propertize
                                            "First reply.\n"
                                            'gptel 'response)))
                                 (setq buffer-file-name nil)
                                 (setq-local
                                  mevedel-session--fork-pending t)
                                 (when-let* ((vb (buffer-local-value
                                                  'mevedel--view-buffer
                                                  buffer))
                                             ((buffer-live-p vb)))
                                   (with-current-buffer vb
                                     (mevedel-view--full-rerender)))))))
                    (with-current-buffer view-buf
                      (mevedel-rewind)))
                  (should (eq loaded-buffer data-buf))
                  (should (= loaded-segment 1))
                  (should (= loaded-turn 1)))
                (with-current-buffer data-buf
                  (should (string-match-p "First prompt" (buffer-string)))
                  (should (string-match-p "First reply" (buffer-string)))
                  (should-not (string-match-p "Second prompt" (buffer-string)))
                  (should mevedel-session--fork-pending))
                (with-current-buffer view-buf
                  (should (derived-mode-p 'mevedel-view-mode))
                  (let ((rendered (buffer-substring-no-properties
                                   (point-min) (point-max))))
                    (should (string-match-p "You" rendered))
                    (should (string-match-p "First prompt" rendered))
                    (should (string-match-p "hook changed prompt" rendered))
                    (should-not (string-match-p "first original" rendered))
                    (should (string-match-p "Assistant" rendered))
                    (should-not (string-match-p ":PROPERTIES:" rendered))
                    (should-not (string-match-p "Second prompt" rendered)))))
            (when (buffer-live-p view-buf) (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--load-truncated ()
  ,test
  (test)
  :doc "disconnects file and sets fork-pending flag"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Lone prompt\n")
                (mevedel-session-persistence-save session buf)
                (mevedel-session-persistence--load-truncated
                 session buf 1 1)
                ;; Content is reloaded from the segment file.
                (should (string-match-p "Lone prompt" (buffer-string)))
                ;; File-name disconnected, fork-pending set, modified flag clear.
                (should (null buffer-file-name))
                (should mevedel-session--fork-pending)
                (should-not (buffer-modified-p)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "errors when target segment file is missing"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hi\n")
                (mevedel-session-persistence-save session buf)
                ;; Ask to load segment 99, which doesn't exist.
                (should-error
                 (mevedel-session-persistence--load-truncated
                  session buf 99 1)
                 :type 'user-error))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "file turn selects prompts after copied compaction tail"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Initial prompt\n")
                (mevedel-session-persistence-save session buf)
                (let ((segment-2
                       (mevedel-session-persistence--segment-path
                        (mevedel-session-save-path session) 2)))
                  (make-directory (file-name-directory segment-2) t)
                  (with-temp-file segment-2
                    (insert ":PROPERTIES:\n")
                    (insert ":MEVEDEL_SEGMENT_TAIL_PROMPTS: 2\n")
                    (insert ":END:\n\n")
                    (insert "#+begin_summary\nSummary\n#+end_summary\n")
                    (insert "Tail prompt 1\nTail response 1\n")
                    (insert "Tail prompt 2\nTail response 2\n")
                    (insert "Actual prompt 1\nActual response 1\n")
                    (insert "Actual prompt 2\nActual response 2\n"))
                  (cl-letf (((symbol-function 'gptel-org--restore-state)
                             (lambda ()
                               (save-excursion
                                 (goto-char (point-min))
                                 (while (re-search-forward
                                         "^.*response [0-9]+$" nil t)
                                   (put-text-property
                                    (line-beginning-position)
                                    (line-end-position)
                                    'gptel 'response))))))
                    (mevedel-session-persistence--load-truncated
                     session buf 2 3))
                  (should (string-match-p "Actual prompt 1"
                                          (buffer-string)))
                  (should-not (string-match-p "Actual prompt 2"
                                              (buffer-string)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 8: file restore plan

(mevedel-deftest mevedel-session-persistence--state-at-turn ()
  ,test
  (test)
  :doc "picks the latest snapshot whose turn is <= cum-turn"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "fooA" :version 1))))
            (3 . (("/abs/foo" . (:backup-name "fooC" :version 3))
                  ("/abs/bar" . (:backup-name "barB" :version 2))))
            (5 . (("/abs/foo" . (:backup-name "fooE" :version 5))))))
    ;; State at turn 4: foo=fooC (turn 3), bar=barB (turn 3).
    (let ((state (mevedel-session-persistence--state-at-turn session 4)))
      (should (= 2 (length state)))
      (should (equal "fooC"
                     (plist-get (cdr (assoc "/abs/foo" state)) :backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state)) :backup-name))))
    ;; State at turn 1: just foo=fooA.
    (let ((state (mevedel-session-persistence--state-at-turn session 1)))
      (should (= 1 (length state)))
      (should (equal "fooA"
                     (plist-get (cdr (assoc "/abs/foo" state)) :backup-name))))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-session-persistence--latest-snapshot-entry ()
  ,test
  (test)
  :doc "returns highest-version entry for the path"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id2" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "v1" :version 1))))
            (5 . (("/abs/foo" . (:backup-name "v3" :version 3))))
            (3 . (("/abs/foo" . (:backup-name "v2" :version 2))))))
    (let ((latest (mevedel-session-persistence--latest-snapshot-entry
                   session "/abs/foo")))
      (should (equal "v3" (plist-get latest :backup-name))))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-session-persistence-restore-plan ()
  ,test
  (test)
  :doc "noop when current content matches target snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (mevedel-file-history--backup-name path 1)))
          (write-region "v1" nil path nil 'silent)
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name "v1")
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (null plan))))   ; noop entries filtered
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "create when target has content but file currently absent"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (backup-name (mevedel-file-history--backup-name path 1)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name "content")
          ;; File doesn't currently exist.
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,backup-name :version 1
                                  :backup-time "..." :file-mtime "..."))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'create (plist-get (car plan) :action)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "delete when target is absent but file exists"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "stale.el")))
          (write-region "stale content" nil path nil 'silent)
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name nil :version 1
                                  :backup-time "..." :file-mtime nil))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'delete (plist-get (car plan) :action)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "overwrite when current content diverges from latest snapshot"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "foo.el"))
               (b1   (mevedel-file-history--backup-name path 1))
               (b2   (mevedel-file-history--backup-name path 2)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) b1 "v1")
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) b2 "v2")
          ;; Current file content is something the snapshots have never seen.
          (write-region "external edits" nil path nil 'silent)
          (setf (mevedel-session-file-snapshots session)
                `((1 . ((,path . (:backup-name ,b1 :version 1
                                  :backup-time "..." :file-mtime "..."))))
                  (2 . ((,path . (:backup-name ,b2 :version 2
                                  :backup-time "..." :file-mtime "..."))))))
          (let ((plan (mevedel-session-persistence-restore-plan session 1)))
            (should (= 1 (length plan)))
            (should (eq 'overwrite (plist-get (car plan) :action)))
            (should (plist-get (car plan) :diverged))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(mevedel-deftest mevedel-session-persistence-execute-restore ()
  ,test
  (test)
  :doc "applies create / delete / restore actions correctly"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((create-path (file-name-concat tempdir "new.el"))
               (delete-path (file-name-concat tempdir "old.el"))
               (restore-path (file-name-concat tempdir "modified.el"))
               (backup-name-create
                (mevedel-file-history--backup-name create-path 1))
               (backup-name-restore
                (mevedel-file-history--backup-name restore-path 1)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name-create "newly created")
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) backup-name-restore "original")
          ;; Set up current state: delete-path exists, restore-path has different content
          (write-region "to be deleted" nil delete-path nil 'silent)
          (write-region "diverged" nil restore-path nil 'silent)
          (let* ((plan
                  (list (list :action 'create  :path create-path
                              :backup-name backup-name-create)
                        (list :action 'delete  :path delete-path)
                        (list :action 'overwrite :path restore-path
                              :backup-name backup-name-restore
                              :diverged t)))
                 (result (mevedel-session-persistence-execute-restore
                          session plan)))
            (should (= 3 (plist-get result :succeeded)))
            (should (null (plist-get result :failed)))
            (should (file-exists-p create-path))
            (should-not (file-exists-p delete-path))
            (with-temp-buffer
              (insert-file-contents create-path)
              (should (equal "newly created" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents restore-path)
              (should (equal "original" (buffer-string))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "stops on first failure"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((path (file-name-concat tempdir "fine.el"))
               (bn   (mevedel-file-history--backup-name path 1)))
          (mevedel-file-history--write-backup
           (mevedel-session-save-path session) bn "ok")
          (let* ((plan
                  (list (list :action 'create :path path :backup-name bn)
                        ;; Bogus backup name — read of backup will fail.
                        (list :action 'create
                              :path (file-name-concat tempdir "two.el")
                              :backup-name "nonexistent@v1")
                        ;; Should not be reached.
                        (list :action 'create
                              :path (file-name-concat tempdir "three.el")
                              :backup-name bn)))
                 (result (mevedel-session-persistence-execute-restore
                          session plan)))
            (should (= 1 (plist-get result :succeeded)))
            (should (plist-get result :failed))
            (should-not (file-exists-p
                         (file-name-concat tempdir "three.el")))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; Phase 9: fork-on-send + rename-session

(defun test-mevedel-session-persistence--make-fork-ready ()
  "Return a real saved session rewound and ready to fork.
The result is a plist whose :tempdir owns every created file."
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buf (generate-new-buffer "*test-data-buf*"))
           (eligible-transcript
            (concat "* Agent Task: inspect\n\n"
                    "#+begin_summary\n"
                    "## Goal\n- Continue.\n"
                    "#+end_summary\n"
                    "Recent agent turn.\n"))
           parent-path
           parent-id)
      (with-current-buffer buf
        (org-mode)
        (setq-local mevedel--session session)
        (add-hook 'kill-buffer-hook
                  #'mevedel-session-persistence--release-on-kill nil t)
        (insert "Segment one prompt\n")
        (mevedel-session-persistence-save session buf)
        (mevedel-session-persistence-rotate-segment
         session buf "Summary 1.")
        (insert "Segment two prompt\n")
        (mevedel-session-persistence-save session buf)
        (mevedel-session-persistence-rotate-segment
         session buf "Summary 2.")
        (insert "Future segment prompt\n")
        (mevedel-session-persistence-save session buf)
        (setq parent-path (mevedel-session-save-path session)
              parent-id (mevedel-session-session-id session))
        (make-directory (file-name-concat parent-path "agents") t)
        (make-directory (file-name-concat parent-path "plans") t)
        (write-region "# Parent plan\n" nil
                      (file-name-concat parent-path "plans/current.md")
                      nil 'silent)
        (write-region eligible-transcript nil
                      (file-name-concat parent-path
                                        "agents/eligible.chat.org")
                      nil 'silent)
        (write-region "eligible recovery archive\n" nil
                      (file-name-concat
                       parent-path
                       "agents/eligible.compact-0001.chat.org")
                      nil 'silent)
        (write-region "future transcript\n" nil
                      (file-name-concat parent-path
                                        "agents/future.chat.org")
                      nil 'silent)
        (write-region "kept backup\n" nil
                      (mevedel-file-history--backup-path
                       parent-path "keep@v1") nil 'silent)
        (write-region "future backup\n" nil
                      (mevedel-file-history--backup-path
                       parent-path "future@v2") nil 'silent)
        (setf (mevedel-session-plan-metadata session)
              '(:path "plans/current.md" :status presented))
        (setf (mevedel-session-prompt-index session)
              '((1 . ((:turn 1 :file-turn 1 :cum-turn 1)))
                (2 . ((:turn 1 :file-turn 1 :cum-turn 2)))
                (3 . ((:turn 1 :file-turn 1 :cum-turn 3)))))
        (setf (mevedel-session-file-snapshots session)
              '((1 . (("/tmp/kept.el"
                       . (:backup-name "keep@v1" :version 1))))
                (3 . (("/tmp/future.el"
                       . (:backup-name "future@v2" :version 2))))))
        (setf (mevedel-session-agent-transcripts session)
              '(("eligible--1" :parent-turn 2
                 :path "agents/eligible.chat.org")
                ("future--2" :parent-turn 3
                 :path "agents/future.chat.org")
                ("poison--3" :parent-turn 2
                 :path "../poison.chat.org")))
        (setf (mevedel-session-turn-count session) 3)
        (mevedel-session-persistence-write
         (mevedel-session-persistence--sidecar-path parent-path)
         (mevedel-session-persistence--build-sidecar session buf))
        (mevedel-session-persistence--load-truncated
         session buf 2 1 2 1))
      (let* ((sessions-dir
              (mevedel-session-persistence--sessions-dir workspace))
             (parent-lock
              (mevedel-session-persistence--lock-path parent-path)))
        (list
         :workspace workspace
         :tempdir tempdir
         :session session
         :buffer buf
         :sessions-dir sessions-dir
         :parent-id parent-id
         :parent-path parent-path
         :parent-lock parent-lock
         :parent-lock-state
         (mevedel-session-persistence--read-lock parent-lock)
         :eligible-transcript eligible-transcript
         :parent-sidecar-text
         (mevedel-session-persistence--file-text
          (mevedel-session-persistence--sidecar-path parent-path))
         :parent-segment-1-text
         (mevedel-session-persistence--file-text
          (mevedel-session-persistence--segment-path parent-path 1))
         :parent-segment-2-text
         (mevedel-session-persistence--file-text
          (mevedel-session-persistence--segment-path parent-path 2))
         :session-state
         (copy-tree (mevedel-session-persistence-serialize session))
         :session-save-path (mevedel-session-save-path session)
         :buffer-text
         (with-current-buffer buf
           (buffer-substring (point-min) (point-max)))
         :buffer-point (with-current-buffer buf (point))
         :buffer-modified-p (with-current-buffer buf (buffer-modified-p))
         :buffer-file-name (with-current-buffer buf buffer-file-name)
         :rewind-context
         (with-current-buffer buf (copy-tree mevedel-session--rewind-context)))))))

(defun test-mevedel-session-persistence--assert-fork-rolled-back (fixture)
  "Assert failed fork restored every parent invariant in FIXTURE."
  (let ((session (plist-get fixture :session))
        (buf (plist-get fixture :buffer))
        (parent-path (plist-get fixture :parent-path))
        (parent-lock (plist-get fixture :parent-lock))
        (sessions-dir (plist-get fixture :sessions-dir)))
    (should (equal (plist-get fixture :session-state)
                   (mevedel-session-persistence-serialize session)))
    (should (equal (plist-get fixture :session-save-path)
                   (mevedel-session-save-path session)))
    (should (equal (plist-get fixture :parent-lock-state)
                   (mevedel-session-persistence--read-lock parent-lock)))
    (should (equal (plist-get fixture :parent-sidecar-text)
                   (mevedel-session-persistence--file-text
                    (mevedel-session-persistence--sidecar-path parent-path))))
    (with-current-buffer buf
      (should (eq session mevedel--session))
      (should (equal (plist-get fixture :buffer-text)
                     (buffer-substring (point-min) (point-max))))
      (should (= (plist-get fixture :buffer-point) (point)))
      (should (eq (plist-get fixture :buffer-modified-p)
                  (buffer-modified-p)))
      (should (equal (plist-get fixture :buffer-file-name) buffer-file-name))
      (should mevedel-session--fork-pending)
      (should (equal (plist-get fixture :rewind-context)
                     mevedel-session--rewind-context)))
    (should
     (equal (list (file-name-nondirectory
                   (directory-file-name parent-path)))
            (sort
             (directory-files sessions-dir nil
                              directory-files-no-dot-files-regexp)
             #'string<)))))

(defun test-mevedel-session-persistence--cleanup-fork-fixture (fixture)
  "Delete the real files and buffer owned by FIXTURE."
  (when-let ((buf (plist-get fixture :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)))
  (when-let ((tempdir (plist-get fixture :tempdir)))
    (when (file-directory-p tempdir)
      (delete-directory tempdir t)))
  (mevedel-workspace-clear-registry))

(mevedel-deftest mevedel-session-persistence--fork-candidate ()
  ,test
  (test)
  :doc "copies and reduces fork state without mutating the parent"
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready)))
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (_ (setf (mevedel-session-preset-name session) 'test-preset
                        (mevedel-session-preset-settings session)
                        '((mevedel-test-setting base))
                        (mevedel-session-goal session)
                        (mevedel-goal--create
                         :id "parent-goal" :objective "Ship"
                         :status 'active :phase 'planning
                         :approval-policy 'supervised :cycle 1
                         :execution-home
                         '(:kind current :directory "/tmp/"
                           :session-id "parent" :locked t)
                         :implementation-context 'full
                         :cycles '((:cycle 1)))))
               (before (mevedel-session-persistence-serialize session))
               (child
                (mevedel-session-persistence--fork-candidate
                 session "/tmp/staged-fork/" "child-id"
                 (plist-get fixture :parent-id) 2 2 "now")))
          (should-not (eq child session))
          (should (equal before
                         (mevedel-session-persistence-serialize session)))
          (should (equal "/tmp/staged-fork/"
                         (mevedel-session-save-path child)))
          (should (= 2 (mevedel-session-turn-count child)))
          (should-not (assoc 3 (mevedel-session-prompt-index child)))
          (should-not (assoc 3 (mevedel-session-file-snapshots child)))
          (should-not (assoc "future--2"
                             (mevedel-session-agent-transcripts child)))
          (should-not (mevedel-session-goal child))
          (should (mevedel-session-goal session))
          (setcar (cdr (assq 'mevedel-test-setting
                             (mevedel-session-preset-settings child)))
                  'child)
          (should (equal '((mevedel-test-setting base))
                         (mevedel-session-preset-settings session)))
          (should (equal '((mevedel-test-setting child))
                         (mevedel-session-preset-settings child))))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(mevedel-deftest mevedel-session-persistence--stage-fork ()
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
          (mevedel-session-persistence--fork-candidate
           session staging-path "child-id"
           (plist-get fixture :parent-id) 2 2 "now"))
         (staging-buffer
          (with-current-buffer buf
            (clone-buffer " *mevedel-stage-test*" nil))))
    (unwind-protect
        (progn
          (with-current-buffer staging-buffer
            (setq-local kill-buffer-hook nil))
          (mevedel-session-persistence--stage-fork
           child buf staging-buffer (plist-get fixture :parent-path)
           staging-path 2 2)
          (should (file-exists-p
                   (mevedel-session-persistence--segment-path
                    staging-path 1)))
          (should (file-exists-p
                   (mevedel-session-persistence--segment-path
                    staging-path 2)))
          (should (file-exists-p
                   (mevedel-session-persistence--sidecar-path staging-path)))
          (should (file-exists-p
                   (mevedel-session-persistence--instructions-current-path
                    staging-path)))
          (should (file-exists-p
                   (mevedel-session-persistence--lock-path staging-path))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (when (file-directory-p staging-path)
        (delete-directory staging-path t))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(mevedel-deftest mevedel-session-persistence-fork-now ()
  ,test
  (test)
  :doc "success preserves segments, history, plans, agents, metadata, and locks"
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready)))
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (buf (plist-get fixture :buffer))
               (parent-path (plist-get fixture :parent-path))
               (stale-block
                (mevedel-pipeline--format-render-data-block
                 '(:execution-id "exec-stale" :state running
                   :live-execution-p t)))
               (parent-segment-1
                (mevedel-session-persistence--segment-path parent-path 1))
               (_archived-stale
                (with-temp-buffer
                  (insert-file-contents parent-segment-1)
                  (goto-char (point-max))
                  (insert stale-block)
                  (write-region (point-min) (point-max)
                                parent-segment-1 nil 'silent)))
               (parent-segment-1-text
                (mevedel-session-persistence--file-text parent-segment-1))
               (_current-stale
                (with-current-buffer buf
                  (goto-char (point-max))
                  (insert stale-block)))
               (_state (mevedel-execution--state-for-session session))
               (new-path (mevedel-session-persistence-fork-now buf))
               (sidecar
                (mevedel-session-persistence-read
                 (mevedel-session-persistence--sidecar-path new-path))))
          (should (string-match-p
                   ":state archived"
                   (mevedel-session-persistence--file-text
                    (mevedel-session-persistence--segment-path new-path 1))))
          (should (string-match-p
                   "Segment two prompt"
                   (mevedel-session-persistence--file-text
                    (mevedel-session-persistence--segment-path new-path 2))))
          (should (string-match-p
                   ":state lost"
                   (mevedel-session-persistence--file-text
                    (mevedel-session-persistence--segment-path new-path 2))))
          (should (equal "kept backup\n"
                         (mevedel-session-persistence--file-text
                          (mevedel-file-history--backup-path
                           new-path "keep@v1"))))
          (should-not (file-exists-p
                       (mevedel-file-history--backup-path
                        new-path "future@v2")))
          (should (equal "# Parent plan\n"
                         (mevedel-session-persistence--file-text
                          (file-name-concat new-path "plans/current.md"))))
          (should
           (equal
            (plist-get fixture :eligible-transcript)
            (mevedel-session-persistence--file-text
             (file-name-concat new-path "agents/eligible.chat.org"))))
          (should-not
           (file-exists-p
            (file-name-concat
             new-path "agents/eligible.compact-0001.chat.org")))
          (should-not (file-exists-p
                       (file-name-concat new-path "agents/future.chat.org")))
          (should-not (file-exists-p
                       (expand-file-name "../poison.chat.org" new-path)))
          (should (equal (plist-get fixture :parent-id)
                         (plist-get sidecar :forked-from-session-id)))
          (should (= 2 (plist-get sidecar :forked-from-turn)))
          (should (equal '(:path "plans/current.md" :status presented)
                         (plist-get sidecar :plan-metadata)))
          (should (assoc "eligible--1"
                         (plist-get sidecar :agent-transcripts)))
          (should-not (assoc "future--2"
                             (plist-get sidecar :agent-transcripts)))
          (should (assoc 1 (plist-get sidecar :file-snapshots)))
          (should-not (assoc 3 (plist-get sidecar :file-snapshots)))
          (should (file-exists-p
                   (mevedel-session-persistence--lock-path new-path)))
          (should-not (file-exists-p (plist-get fixture :parent-lock)))
          (with-current-buffer buf
            (should-not mevedel-session--fork-pending)
            (should-not mevedel-session--rewind-context)
            (should (string-prefix-p new-path buffer-file-name)))
          (should-not (mevedel-session-execution-state session))
          (should (equal (plist-get fixture :parent-sidecar-text)
                         (mevedel-session-persistence--file-text
                          (mevedel-session-persistence--sidecar-path
                           parent-path))))
          (should (equal parent-segment-1-text
                         (mevedel-session-persistence--file-text
                          (mevedel-session-persistence--segment-path
                           parent-path 1))))
          (should (equal (plist-get fixture :parent-segment-2-text)
                         (mevedel-session-persistence--file-text
                          (mevedel-session-persistence--segment-path
                           parent-path 2))))
          (should (= 2 (length
                        (directory-files
                         (plist-get fixture :sessions-dir) nil
                         directory-files-no-dot-files-regexp))))
          (should (equal new-path (mevedel-session-save-path session))))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "retries a colliding session id before staging publication"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer))
         (parent-id (plist-get fixture :parent-id))
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-session-persistence--compute-id)
                   (lambda (_name)
                     (if (= (cl-incf calls) 1)
                         parent-id
                       "fork-child"))))
          (let ((new-path (mevedel-session-persistence-fork-now buf)))
            (should (= calls 2))
            (should (equal "fork-child"
                           (file-name-nondirectory
                            (directory-file-name new-path))))
            (should (file-exists-p
                     (mevedel-session-persistence--lock-path new-path)))
            (should-not
             (directory-files new-path nil "\\`\\.mevedel-fork-"))))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "publish race preserves the competing session directory"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer))
         (sessions-dir (plist-get fixture :sessions-dir))
         (final-path (file-name-concat sessions-dir "raced-child"))
         (sentinel (file-name-concat final-path "sentinel"))
         (real-rename (symbol-function 'rename-file))
         raced)
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-persistence--compute-id)
              (lambda (_name) "raced-child"))
             ((symbol-function 'rename-file)
              (lambda (src dst &rest args)
                (when (and (not raced)
                           (file-directory-p src)
                           (equal (expand-file-name dst)
                                  (expand-file-name final-path)))
                  (setq raced t)
                  (make-directory final-path)
                  (write-region "owned by competitor\n" nil sentinel
                                nil 'silent))
                (apply real-rename src dst args))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (should (equal '("sentinel")
                         (directory-files
                          final-path nil
                          directory-files-no-dot-files-regexp)))
          (delete-directory final-path t)
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  ;; These cases inject deterministic failures only at explicit transaction
  ;; phase seams.  Session state, buffers, directories, files, and locks
  ;; remain real so every rollback assertion observes actual filesystem state.
  :doc "candidate failure removes staging before artifact assembly"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer)))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-session-persistence--reduce-prompt-index)
                   (lambda (&rest _) (error "Injected candidate failure"))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "copy failure restores the parent and removes all fork artifacts"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer))
         (source (file-name-concat (plist-get fixture :parent-path)
                                   "agents/eligible.chat.org"))
         (real-copy (symbol-function 'copy-file)))
    (unwind-protect
        (cl-letf (((symbol-function 'copy-file)
                   (lambda (src dst &rest args)
                     (if (equal (expand-file-name src)
                                (expand-file-name source))
                         (error "Injected fork copy failure")
                       (apply real-copy src dst args)))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "sidecar failure restores the parent and removes staging"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-persistence-write)
                   (lambda (&rest _) (error "Injected sidecar failure"))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "instruction persistence failure rolls a complete candidate back"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer)))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-session-persistence--save-instructions)
                   (lambda (&rest _) (error "Injected instruction failure"))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "child lock failure preserves the parent lock and rewind state"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer))
         (parent-path (plist-get fixture :parent-path))
         (real-acquire
          (symbol-function 'mevedel-session-persistence-lock-acquire)))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-persistence-lock-acquire)
              (lambda (path buffer-name)
                (if (equal (file-truename path)
                           (file-truename parent-path))
                    (funcall real-acquire path buffer-name)
                  (error "Injected child lock failure")))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "parent release failure after deletion reacquires the parent lock"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer))
         (parent-path (plist-get fixture :parent-path))
         (real-release
          (symbol-function 'mevedel-session-persistence-lock-release))
         injected)
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-persistence-lock-release)
              (lambda (path)
                (funcall real-release path)
                (when (and (not injected)
                           (equal (file-truename path)
                                  (file-truename parent-path)))
                  (setq injected t)
                  (error "Injected parent release failure")))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "publish rename failure removes staging and preserves the parent"
  (let* ((fixture (test-mevedel-session-persistence--make-fork-ready))
         (buf (plist-get fixture :buffer))
         (sessions-dir (plist-get fixture :sessions-dir))
         (real-rename (symbol-function 'rename-file)))
    (unwind-protect
        (cl-letf
            (((symbol-function 'rename-file)
              (lambda (src dst &rest args)
                (if (and (file-directory-p src)
                         (file-equal-p (file-name-directory dst)
                                       sessions-dir))
                    (error "Injected fork publish failure")
                  (apply real-rename src dst args)))))
          (should-error (mevedel-session-persistence-fork-now buf))
          (test-mevedel-session-persistence--assert-fork-rolled-back fixture))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture)))
  :doc "errors when buffer is not in rewind preview state"
  (with-temp-buffer
    (let ((mevedel-session--fork-pending nil))
      (should-error (mevedel-session-persistence-fork-now (current-buffer))
                    :type 'user-error))))

(mevedel-deftest mevedel-rename-session ()
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
                (mevedel-session-persistence-save session buf)
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
                    (should (string-prefix-p
                             (mevedel-session-save-path session) artifact))
                    (should (file-exists-p artifact))
                    (should
                     (equal "beforeafter"
                            (with-temp-buffer
                              (insert-file-contents artifact)
                              (buffer-string)))))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 10: resume / list / save commands

(mevedel-deftest mevedel-session-persistence-list-sessions ()
  ,test
  (test)
  :doc "lists materialized sessions, sorted newest-first"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (mevedel-session-create "alpha" workspace))
               (b1 (generate-new-buffer "*test-session-alpha*"))
               (s2 (mevedel-session-create "beta" workspace))
               (b2 (generate-new-buffer "*test-session-beta*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "Hello\n")
                  (mevedel-session-persistence-save s1 b1))
                (sleep-for 1.1)   ; ensure :updated-at differs
                (with-current-buffer b2
                  (org-mode)
                  (insert "World\n")
                  (mevedel-session-persistence-save s2 b2))
                (let ((listed (mevedel-session-persistence-list-sessions
                               workspace)))
                  (should (= 2 (length listed)))
                  ;; b2 (beta) was saved last → first in list.
                  (should (equal "beta"
                                 (plist-get
                                  (plist-get (car listed) :summary)
                                  :session-name)))
                  (should (equal "alpha"
                                 (plist-get
                                  (plist-get (cadr listed) :summary)
                                  :session-name)))))
            (test-mevedel-session-persistence--release-and-kill b1 s1)
            (test-mevedel-session-persistence--release-and-kill b2 s2)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "returns nil for a workspace with no sessions"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (should (null (mevedel-session-persistence-list-sessions workspace)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "resume completion preserves newest-first session order"
  (let* ((displays '("2h ago       new" "yesterday    old"))
         (collection
          (mevedel-session-persistence--ordered-display-collection
           displays 'mevedel-session))
         (metadata (funcall collection "" nil 'metadata)))
    (should (eq 'identity
                (cdr (assq 'display-sort-function (cdr metadata)))))
    (should (eq 'identity
                (cdr (assq 'cycle-sort-function (cdr metadata)))))))

(mevedel-deftest mevedel-session-persistence--read-summary ()
  ,test
  (test)
  :doc "extracts only picker-relevant fields"
  (let ((tmp (make-temp-file "mevedel-summary-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write
           tmp
           (test-mevedel-session-persistence--complete-sidecar
            `(:session-name "demo"
              :session-id "demo-1234"
              :updated-at "2026-04-23T12-00-00"
              :first-user-message "Hello"
              :latest-user-message "Latest")))
          (let ((s (mevedel-session-persistence--read-summary tmp)))
            (should (equal "demo" (plist-get s :session-name)))
            (should (equal "demo-1234" (plist-get s :session-id)))
            (should (equal "Hello" (plist-get s :first-user-message)))
            (should (equal "Latest" (plist-get s :latest-user-message)))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "returns nil on unreadable file"
  (should (null (mevedel-session-persistence--read-summary
                 "/nonexistent/path"))))

(mevedel-deftest mevedel-session-persistence--format-session-candidate ()
  ,test
  (test)
  :doc "prefers latest preview over first preview"
  (let ((display
         (mevedel-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 2
                      :total-turn-count 4
                      :first-user-message "Original request"
                      :latest-user-message "Newest request")))))
    (should (string-match-p "Newest request" display))
    (should-not (string-match-p "Original request" display)))
  :doc "falls back to first preview for old summaries"
  (let ((display
         (mevedel-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 1
                      :total-turn-count 1
                      :first-user-message "Original request")))))
    (should (string-match-p "Original request" display))))


;;
;;; Phase 11: relocation, self-heal, save-failure flag

(mevedel-deftest mevedel-session-persistence--reconcile-relocation ()
  ,test
  (test)
  :doc "rewrites permission rules whose :path is under the saved root"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id" "/new/root/" "ws"))
         (session   (mevedel-session-create "x" workspace)))
    (setf (mevedel-session-permission-rules session)
          '(("Read"  :path "/old/root/foo/**" :action allow)
            ("Read"  :path "/old/root/bar/baz" :action allow)
            ("Bash"  :pattern "git log*"      :action allow)
            ("Read"  :path "/elsewhere/baz"   :action deny)))
    (mevedel-session-persistence--reconcile-relocation
     session '(:type project :id "id" :root "/old/root/" :name "ws"))
    (let ((rules (mevedel-session-permission-rules session)))
      (should (equal (file-name-concat (expand-file-name "/new/root/")
                                       "foo/**")
                     (plist-get (cdr (nth 0 rules)) :path)))
      (should (equal (file-name-concat (expand-file-name "/new/root/")
                                       "bar/baz")
                     (plist-get (cdr (nth 1 rules)) :path)))
      ;; Bash rule untouched (no :path).
      (should (equal "git log*" (plist-get (cdr (nth 2 rules)) :pattern)))
      ;; Out-of-tree path untouched.
      (should (equal "/elsewhere/baz"
                     (plist-get (cdr (nth 3 rules)) :path))))
    (mevedel-workspace-clear-registry))
  :doc "no-op when saved root matches current"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id2" "/same/root/" "ws"))
         (session   (mevedel-session-create "x" workspace))
         (orig-rules '(("Read" :path "/same/root/foo" :action allow))))
    (setf (mevedel-session-permission-rules session) orig-rules)
    (mevedel-session-persistence--reconcile-relocation
     session '(:type project :id "id2" :root "/same/root/" :name "ws"))
    (should (equal orig-rules
                   (mevedel-session-permission-rules session)))
    (mevedel-workspace-clear-registry))
  :doc "does not rewrite permission paths already under nested current root"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id3" "/old/root/packages/api/" "ws"))
         (session   (mevedel-session-create "x" workspace))
         (orig-rules '(("Read" :path "/old/root/packages/api/foo" :action allow)
                       ("Read" :path "/old/root/other" :action allow))))
    (setf (mevedel-session-permission-rules session) orig-rules)
    (mevedel-session-persistence--reconcile-relocation
     session '(:type project :id "id3" :root "/old/root/" :name "ws"))
    (let ((rules (mevedel-session-permission-rules session)))
      (should (equal "/old/root/packages/api/foo"
                     (plist-get (cdr (nth 0 rules)) :path)))
      (should (equal (file-name-concat
                      (expand-file-name "/old/root/packages/api/")
                      "other")
                     (plist-get (cdr (nth 1 rules)) :path))))
    (mevedel-workspace-clear-registry)))

(mevedel-deftest mevedel-session-persistence--detect-highest-segment ()
  ,test
  (test)
  :doc "returns the maximum segment number on disk"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-segdetect-" t))))
    (unwind-protect
        (progn
          (write-region "" nil
                        (file-name-concat tempdir "segment-0001.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0003.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          ;; Decoy file shouldn't count.
          (write-region "" nil
                        (file-name-concat tempdir "session.meta.el")
                        nil 'silent)
          (should (= 3 (mevedel-session-persistence--detect-highest-segment
                        tempdir))))
      (delete-directory tempdir t)))
  :doc "returns 0 when no segment files exist"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-segdetect-" t))))
    (unwind-protect
        (should (= 0 (mevedel-session-persistence--detect-highest-segment
                      tempdir)))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence--self-heal-segment-counter ()
  ,test
  (test)
  :doc "trusts filesystem when sidecar disagrees"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-selfheal-" t))))
    (unwind-protect
        (let ((session (mevedel-session-create
                        "x"
                        (mevedel-workspace-get-or-create
                         'project "id" "/" "x"))))
          (setf (mevedel-session-current-segment session) 1)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0001.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          ;; Suppress display-warning popup during the test.
          (cl-letf (((symbol-function 'display-warning) #'ignore))
            (mevedel-session-persistence--self-heal-segment-counter
             session tempdir))
	          (should (= 2 (mevedel-session-current-segment session))))
	      (delete-directory tempdir t)
	      (mevedel-workspace-clear-registry)))
  :doc "finalizes predecessor when healing upward"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-selfheal-" t))))
    (unwind-protect
        (let ((session (mevedel-session-create
                        "x"
                        (mevedel-workspace-get-or-create
                         'project "id" "/" "x")))
              (seg1 (file-name-concat tempdir "segment-0001.chat.org")))
          (setf (mevedel-session-current-segment session) 1)
          (write-region "* Chat\n" nil seg1 nil 'silent)
          (write-region "* Chat\n" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          (cl-letf (((symbol-function 'display-warning) #'ignore))
            (mevedel-session-persistence--self-heal-segment-counter
             session tempdir))
          (with-temp-buffer
            (insert-file-contents seg1)
            (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                    (buffer-string)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 12: auto-cleanup

(mevedel-deftest mevedel-session-persistence--parse-iso-time ()
  ,test
  (test)
  :doc "parses our ISO-with-dashes format"
  (let ((time (mevedel-session-persistence--parse-iso-time
               "2026-04-23T14-30-15")))
    (should time)
    (should (equal "2026-04-23T14-30-15"
                   (format-time-string "%FT%H-%M-%S" time))))
  :doc "returns nil for malformed input"
  (should (null (mevedel-session-persistence--parse-iso-time "not a date")))
  (should (null (mevedel-session-persistence--parse-iso-time nil))))

(mevedel-deftest mevedel-session-persistence-cleanup-expired ()
  ,test
  (test)
  :doc "deletes sessions older than the cap"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               ;; Reset the throttle so tests don't leak.
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s1 (mevedel-session-create "old" workspace))
               (b1 (generate-new-buffer "*test-old-buf*"))
               (s2 (mevedel-session-create "new" workspace))
               (b2 (generate-new-buffer "*test-new-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "Old\n")
                  (mevedel-session-persistence-save s1 b1))
                (with-current-buffer b2
                  (org-mode)
                  (insert "New\n")
                  (mevedel-session-persistence-save s2 b2))
                (let ((archive
                       (file-name-concat
                        (mevedel-session-save-path s1)
                        "agents/old.compact-0001.chat.org")))
                  (make-directory (file-name-directory archive) t)
                  (write-region "recovery archive\n" nil archive nil 'silent)
                  (should (file-exists-p archive)))
                ;; Forge :updated-at on the old session to be 14 days ago.
                (let* ((old-path (mevedel-session-save-path s1))
                       (sidecar  (mevedel-session-persistence--sidecar-path
                                  old-path))
                       (plist    (mevedel-session-persistence-read sidecar))
                       (forged   (format-time-string
                                  "%FT%H-%M-%S"
                                  (time-subtract (current-time)
                                                 (* 14 24 60 60)))))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist))
                ;; Release locks so cleanup can delete the dirs.
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path s1))
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path s2))
                (let ((deleted
                       (mevedel-session-persistence-cleanup-expired
                        workspace t)))
                  (should (= 1 deleted))
                  (should-not (file-directory-p
                               (mevedel-session-save-path s1)))
                  (should (file-directory-p
                           (mevedel-session-save-path s2)))))
            (when (buffer-live-p b1)
              (with-current-buffer b1 (set-buffer-modified-p nil))
              (kill-buffer b1))
            (when (buffer-live-p b2)
              (with-current-buffer b2 (set-buffer-modified-p nil))
              (kill-buffer b2))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "skips locked sessions even when expired"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s (mevedel-session-create "stuck" workspace))
               (b (generate-new-buffer "*test-stuck-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-persistence-save s b))
                ;; Forge old :updated-at.
                (let* ((path (mevedel-session-save-path s))
                       (sidecar (mevedel-session-persistence--sidecar-path
                                 path))
                       (plist   (mevedel-session-persistence-read sidecar))
                       (forged  (format-time-string
                                 "%FT%H-%M-%S"
                                 (time-subtract (current-time)
                                                (* 30 24 60 60)))))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist))
                ;; The lock from save still exists with our PID — live.
                (let ((deleted
                       (mevedel-session-persistence-cleanup-expired
                        workspace t)))
                  (should (= 0 deleted))
                  (should (file-directory-p
                           (mevedel-session-save-path s)))))
            (when (buffer-live-p b)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "deletes expired sessions whose same-host lock has a reused PID"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s (mevedel-session-create "reused" workspace))
               (b (generate-new-buffer "*test-reused-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-persistence-save s b))
                (let* ((path      (mevedel-session-save-path s))
                       (sidecar   (mevedel-session-persistence--sidecar-path
                                   path))
                       (plist     (mevedel-session-persistence-read sidecar))
                       (old-time  (time-subtract (current-time)
                                                 (* 30 24 60 60)))
                       (forged    (format-time-string "%FT%H-%M-%S"
                                                       old-time))
                       (lock-path (mevedel-session-persistence--lock-path
                                   path)))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist)
                  (with-temp-file lock-path
                    (prin1 (list :pid 12345
                                 :hostname (system-name)
                                 :emacs-invocation-time forged
                                 :buffer "*old-buf*")
                           (current-buffer))))
                (cl-letf (((symbol-function
                            'mevedel-session-persistence--pid-alive-p)
                           (lambda (&rest _) t))
                          ((symbol-function
                            'mevedel-session-persistence--pid-start-time)
                           (lambda (&rest _) (current-time))))
                  (let ((deleted
                         (mevedel-session-persistence-cleanup-expired
                          workspace t)))
                    (should (= 1 deleted))
                    (should-not (file-directory-p
                                 (mevedel-session-save-path s))))))
            (when (buffer-live-p b)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "no-op when cap is nil"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let ((mevedel-session-max-age-days nil))
          (should (null (mevedel-session-persistence-cleanup-expired
                         workspace t))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "throttled to at most one run per workspace per Emacs"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal)))
          ;; First call returns 0 (no sessions); second call (no force) returns nil.
          (should (= 0 (mevedel-session-persistence-cleanup-expired
                        workspace)))
          (should (null (mevedel-session-persistence-cleanup-expired
                         workspace))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Integration: pipeline snapshot -> request struct -> session save

(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)

(mevedel-deftest mevedel-session-persistence/file-history-roundtrip ()
  ,test
  (test)
  :doc "a modifying tool routed through the pipeline lands a backup in file-history"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (setf (mevedel-session-permission-mode session) 'auto)
    (unwind-protect
        (let* ((data-buf (get-buffer "*test-data-buf*"))
               (tracked  (file-name-concat tempdir "tracked.el"))
               ;; Plant pre-edit content so the snapshot has a
               ;; non-nil "original" to compare against at save time.
               (_ (let ((coding-system-for-write 'utf-8-unix))
                    (write-region "ORIGINAL\n" nil tracked nil 'silent)))
               ;; Mock tool with `get-path' so the pipeline's
               ;; snapshot step fires for it.  Handler mutates the
               ;; file to simulate what a real Edit / Write would do.
               (tool (mevedel-tool--create
                      :name "WriteMock"
                      :groups '(edit)
                      :handler (lambda (args)
                                 (let ((p (plist-get args :path))
                                       (c (plist-get args :content)))
                                   (let ((coding-system-for-write 'utf-8-unix))
                                     (write-region c nil p nil 'silent))
                                   '(:result "ok")))
                      :args '((path string :required "Path")
                              (content string :required "Content"))
                      :get-path (lambda (args) (plist-get args :path))
                      :read-only-p nil
                      :async-p nil))
               result)
          ;; Plant the session buffer-locally so
          ;; `mevedel-pipeline-run-tool' captures it as the context.
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace
                        (mevedel-session-workspace session))
            ;; Begin a request so tool-fs records the original content.
            (mevedel-request-begin session)
            (unwind-protect
                (progn
                  (mevedel-pipeline-run-tool
                   tool (lambda (r) (setq result r))
                   (list :path tracked :content "MODIFIED\n"))
                  (should (equal "ok" result))
                  ;; Snapshot step captured the pre-edit content.
                  (let ((ht (mevedel-request-file-snapshots
                             mevedel--current-request)))
                    (should (hash-table-p ht))
                    (should (equal "ORIGINAL\n" (gethash tracked ht))))
                  ;; Drive a save (what the DONE terminal handler
                  ;; would do in production) and verify a backup file
                  ;; landed under file-history/.
                  (mevedel-session-persistence-save session data-buf)
                  (let* ((snaps (mevedel-session-file-snapshots session))
                         (turn-entry (cdar snaps))
                         (file-entry (assoc tracked turn-entry))
                         (backup-name (plist-get (cdr file-entry)
                                                 :backup-name))
                         (backup-path (mevedel-file-history--backup-path
                                       (mevedel-session-save-path session)
                                       backup-name)))
                    (should snaps)
                    (should backup-name)
                    (should (file-exists-p backup-path))
                    ;; Backup stores the post-edit content (the state
                    ;; `snapshot-modified' observes at save time).
                    (with-temp-buffer
                      (insert-file-contents-literally backup-path)
                      (should (equal "MODIFIED\n" (buffer-string))))))
              (mevedel-request-end))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; View rerender on resume / rewind

(mevedel-deftest mevedel-session-persistence--find-file-noselect ()
  ,test
  (test)
  :doc "disables so-long predicate while opening persisted files"
  (let ((observed :unset)
        (opened (generate-new-buffer " *mevedel-so-long-open*")))
    (unwind-protect
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (_file &rest _args)
                     (setq observed (funcall so-long-predicate))
                     opened)))
          (should (eq opened
                      (mevedel-session-persistence--find-file-noselect
                       "/tmp/session.chat.org")))
          (should (eq observed nil)))
      (when (buffer-live-p opened)
        (kill-buffer opened)))))

(mevedel-deftest mevedel-session-persistence/view-rerender ()
  ,test
  (test)
  :doc "save path calls mevedel-view--full-rerender after buffer save"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (vb      (generate-new-buffer "*test-view-buf*"))
               (rerender-count 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--view-buffer vb)
                (with-current-buffer vb
                  (setq-local mevedel--data-buffer buf))
                (insert "prompt before save\n")
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (mevedel-session-persistence-save session buf))
                (should (= rerender-count 1)))
            (when (buffer-live-p vb) (kill-buffer vb))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume path calls mevedel-view--full-rerender"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (rerender-count 0)
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from resume test\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (setq restored
                        (mevedel-session-persistence-restore session-dir)))
                (should (buffer-live-p restored))
                ;; The rerender may fire via init-common's view-ensure
                ;; flow (which touches the view buffer).  We only care
                ;; that it fires at least once.
                (should (>= rerender-count 1)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
	  (delete-directory tempdir t)
	  (mevedel-workspace-clear-registry)))

  :doc "resume path renders persisted hook audit records"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist
                              '((org-mode . "*** ")))
                  (insert "\n\n*** rewritten prompt")
                  (insert
                   (mevedel--format-hook-audit-record
                    '(:type prompt-rewrite
                      :event "UserPromptSubmit"
                      :original "original prompt"
                      :submitted "rewritten prompt")))
                  (insert "\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (mevedel-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'mevedel--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (mevedel-view--full-rerender)
                  (let ((text (buffer-substring-no-properties
                               (point-min) mevedel-view--input-marker)))
                    (should (string-match-p "hook changed prompt" text))
                    (should (string-match-p "rewritten prompt" text))
                    (should-not (string-match-p "original prompt" text)))
                  (goto-char (point-min))
                  (search-forward "hook changed prompt")
                  (mevedel-view-toggle-section)
                  (let ((expanded (buffer-substring-no-properties
                                   (point-min) mevedel-view--input-marker)))
                    (should (string-match-p "original prompt" expanded)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume path restores view input history"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from history resume test\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (let ((history-path
                       (file-name-concat tempdir ".mevedel/input-history.el")))
                  (make-directory (file-name-directory history-path) t)
                  (mevedel-session-persistence-write
                   history-path
                   '(:version 2 :entries ("second" "first"))))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (mevedel-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'mevedel--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (should (equal '("second" "first")
                                 (mevedel-view-history--entries)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume command displays the companion view buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               restored displayed)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from resume display test\n")
                  (mevedel-session-persistence-save session buf))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (let ((default-directory tempdir))
                  (cl-letf (((symbol-function 'mevedel-workspace)
                             (lambda (&optional _arg) workspace))
                            ((symbol-function 'completing-read)
                             (lambda (_prompt _collection &optional
                                               _predicate _require-match
                                               _initial-input _hist def
                                               _inherit-input-method)
                               def))
                            ((symbol-function 'display-buffer)
                             (lambda (buffer &optional _action _frame)
                               (setq displayed buffer)
                               buffer)))
                    (setq restored (mevedel-resume))))
                (should (buffer-live-p restored))
                (should (eq displayed
                            (buffer-local-value 'mevedel--view-buffer
                                                restored))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "rewind path calls mevedel-view--full-rerender"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (vb      (generate-new-buffer "*test-view-buf*"))
               (rerender-count 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--view-buffer vb)
                (insert "Original prompt\n")
                (mevedel-session-persistence-save session buf)
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (mevedel-session-persistence--load-truncated
                   session buf 1 1))
                (should (>= rerender-count 1)))
            (when (buffer-live-p vb) (kill-buffer vb))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; WAIT-handler fork: data-buffer send after rewind

(mevedel-deftest mevedel-session-persistence/wait-handler-fork ()
  ,test
  (test)
  :doc "WAIT handler materializes fork before request-begin when fork-pending"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (fork-calls 0)
               (begin-calls 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (setq-local mevedel-session--fork-pending t)
                (cl-letf
                    (((symbol-function 'mevedel-session-persistence-fork-now)
                      (lambda (_b) (cl-incf fork-calls)))
                     ((symbol-function 'mevedel-request-begin)
                      (lambda (_s &optional _d) (cl-incf begin-calls))))
                  (let* ((handlers
                          (mevedel-preset--build-handlers
                           '((WAIT) (TYPE) (DONE) (ERRS))))
                         (wait-handler (car (cdr (assq 'WAIT handlers))))
                         (info (list :buffer buf))
                         (fsm (gptel-make-fsm :info info)))
                    (funcall wait-handler fsm)
                    (should (= 1 fork-calls))
                    (should (= 1 begin-calls)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "WAIT handler skips fork when not in rewind preview"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (fork-calls 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (cl-letf
                    (((symbol-function 'mevedel-session-persistence-fork-now)
                      (lambda (_b) (cl-incf fork-calls)))
                     ((symbol-function 'mevedel-request-begin)
                      (lambda (_s &optional _d) nil)))
                  (let* ((handlers
                          (mevedel-preset--build-handlers
                           '((WAIT) (TYPE) (DONE) (ERRS))))
                         (wait-handler (car (cdr (assq 'WAIT handlers))))
                         (fsm (gptel-make-fsm :info (list :buffer buf))))
                    (funcall wait-handler fsm)
                    (should (zerop fork-calls)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; View-send fork gating (empty input / local slash / unknown slash)

(mevedel-deftest mevedel-session-persistence/view-send-fork-gating ()
  ,test
  (test)
  :doc "empty input after rewind does not materialize the fork"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*"))
        (fork-calls 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel-session--fork-pending t))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                     (lambda (_b) (cl-incf fork-calls))))
            (with-current-buffer view-buf
              ;; Empty input region.
              (should-error (mevedel-view-send) :type 'user-error)))
          (should (zerop fork-calls)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))
  :doc "local slash command after rewind does not materialize the fork"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*"))
        (fork-calls 0)
        (dispatch-calls 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel-session--fork-pending t))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-slash-commands
                 `(("local" . ,(lambda (&rest _) (cl-incf dispatch-calls))))))
            (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                       (lambda (_b) (cl-incf fork-calls))))
              (with-current-buffer view-buf
                (goto-char (point-max))
                (insert "/local")
                (mevedel-view-send)))
            (should (= 1 dispatch-calls))
            (should (zerop fork-calls))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))
  :doc "compaction in flight blocks view send"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--compaction-in-flight t))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (insert "hello")
            (should-error (mevedel-view-send) :type 'user-error)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))
  :doc "unknown slash command after rewind does not materialize the fork"
  (let ((data-buf (generate-new-buffer " *test-data*"))
        (view-buf (generate-new-buffer " *test-view*"))
        (fork-calls 0))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel-session--fork-pending t))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-slash-commands nil))
            (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                       (lambda (_b) (cl-incf fork-calls))))
              (with-current-buffer view-buf
                (goto-char (point-max))
                (insert "/no-such-command")
                (mevedel-view-send))))
          (should (zerop fork-calls)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))


;;
;;; Fork releases parent lock

(mevedel-deftest mevedel-session-persistence/fork-releases-parent-lock ()
  ,test
  (test)
  :doc "fork-now deletes the parent session's .lock"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "Original prompt\n")
                (mevedel-session-persistence-save session buf)
                (let* ((parent-path (mevedel-session-save-path session))
                       (parent-lock
                        (mevedel-session-persistence--lock-path parent-path)))
                  (should (file-exists-p parent-lock))
                  (mevedel-session-persistence--load-truncated
                   session buf 1 1 1)
                  (mevedel-session-persistence-fork-now buf)
                  (should-not (file-exists-p parent-lock))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Sidecar missing / unreadable fallback on restore

(mevedel-deftest mevedel-session-persistence/sidecar-missing-on-restore ()
  ,test
  (test)
  :doc "deleted sidecar causes restore to synthesize a fresh session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (delete-file
                 (mevedel-session-persistence--sidecar-path session-dir))
                (cl-letf (((symbol-function 'display-warning) #'ignore))
                  (setq restored
                        (mevedel-session-persistence-restore session-dir)))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should mevedel--session)
                  (should (mevedel-session-session-id mevedel--session))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "corrupt sidecar also causes restore to synthesize a fresh session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (mevedel-session-persistence-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (write-region "this is not a plist" nil
                              (mevedel-session-persistence--sidecar-path
                               session-dir)
                              nil 'silent)
                (cl-letf (((symbol-function 'display-warning) #'ignore))
                  (setq restored
                        (mevedel-session-persistence-restore session-dir)))
                (should (buffer-live-p restored)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Cross-host cleanup behavior

(mevedel-deftest mevedel-session-persistence/cleanup-cross-host-lock ()
  ,test
  (test)
  :doc "cross-host lock prevents cleanup from deleting an expired session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 1)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello\n")
                  (mevedel-session-persistence-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (lock-path
                        (mevedel-session-persistence--lock-path save-path))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path save-path))
                       (plist (mevedel-session-persistence-read sidecar))
                       (forged (format-time-string
                                "%FT%H-%M-%S"
                                (time-subtract (current-time)
                                               (* 7 24 60 60)))))
                  ;; Forge an expired :updated-at.
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist)
                  ;; Overwrite our lock with a cross-host lock (still
                  ;; active from cleanup's perspective).
                  (with-temp-file lock-path
                    (prin1 (list :pid 99999
                                 :hostname "other-host.example"
                                 :emacs-invocation-time "..."
                                 :buffer "*remote*")
                           (current-buffer)))
                  ;; Run cleanup.
                  (let ((deleted (mevedel-session-persistence-cleanup-expired
                                  workspace t)))
                    (should (= 0 deleted))
                    (should (file-directory-p save-path)))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Same-name sessions in one workspace

(mevedel-deftest mevedel-session-persistence/same-name-sessions ()
  ,test
  (test)
  :doc "restore resolves the right session-id when two sessions share a name"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (mevedel-session-create "main" workspace))
               (b1 (generate-new-buffer "*test-data-1*"))
               (s2 (mevedel-session-create "main" workspace))
               (b2 (generate-new-buffer "*test-data-2*"))
               restored)
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (setq-local mevedel--session s1)
                  (insert "session one\n")
                  (mevedel-session-persistence-save s1 b1))
                ;; Force a visible clock gap so session ids differ.
                (sleep-for 1.1)
                (with-current-buffer b2
                  (org-mode)
                  (setq-local mevedel--session s2)
                  (insert "session two\n")
                  (mevedel-session-persistence-save s2 b2))
                (should-not (equal (mevedel-session-session-id s1)
                                   (mevedel-session-session-id s2)))
                ;; Both buffers share the default
                ;; `*mevedel:main@...*' buffer name (identical session
                ;; name + workspace).  Restore must match session-id,
                ;; not just the buffer name, and return b1 when asked
                ;; to resume s1's dir.
                (setq restored
                      (mevedel-session-persistence-restore
                       (mevedel-session-save-path s1)))
                (should (buffer-live-p restored))
                (should (eq restored b1))
                (with-current-buffer restored
                  (should (equal (mevedel-session-session-id s1)
                                 (mevedel-session-session-id mevedel--session)))))
            (test-mevedel-session-persistence--release-and-kill b1 s1)
            (test-mevedel-session-persistence--release-and-kill b2 s2)
            (when (and restored (buffer-live-p restored))
              (test-mevedel-session-persistence--release-and-kill
               restored
               (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Session-id collision retry loop

(mevedel-deftest mevedel-session-persistence/id-collision-retry ()
  ,test
  (test)
  :doc "ensure-files retries id generation when the target dir already exists"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir
                (mevedel-session-persistence--sessions-dir workspace))
               ;; Pre-create a directory that a naive `compute-id'
               ;; would collide with.
               (colliding "main-collision-0001")
               (remaining '("main-collision-0002" "main-collision-0003")))
          (make-directory (file-name-concat sessions-dir colliding) t)
          (let ((session (mevedel-session-create "main" workspace))
                (buf     (generate-new-buffer "*test-data-buf*")))
            (unwind-protect
                (cl-letf*
                    ;; First call returns the colliding id, subsequent
                    ;; calls return fresh ids from `remaining'.
                    ((first-call-p t)
                     ((symbol-function
                       'mevedel-session-persistence--compute-id)
                      (lambda (_name)
                        (cond
                         (first-call-p
                          (setq first-call-p nil)
                          colliding)
                         (t (pop remaining))))))
                  (with-current-buffer buf
                    (org-mode)
                    (insert "hi\n")
                    (mevedel-session-persistence-ensure-files session buf)
                    ;; Picked a non-colliding id.
                    (should-not (equal colliding
                                       (mevedel-session-session-id session)))
                    ;; Original colliding dir was not touched.
                    (should (file-directory-p
                             (file-name-concat sessions-dir colliding)))))
              (test-mevedel-session-persistence--release-and-kill
               buf session))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(provide 'test-mevedel-session-persistence)

;;; test-mevedel-session-persistence.el ends here
