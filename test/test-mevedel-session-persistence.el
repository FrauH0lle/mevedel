;;; test-mevedel-session-persistence.el --- Tests for session persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-persistence' (Phase 1: serialization).

;;; Code:

(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-workspace-identity)
(require 'mevedel-presets)
(require 'mevedel-plan)
(require 'mevedel-skills-ui)
(require 'mevedel-reminders)
(require 'mevedel-resource)
(require 'mevedel-view)
(require 'mevedel-view-history)
(require 'mevedel-chat)
(require 'mevedel-execution-target)
(require 'mevedel-hooks)
(require 'mevedel-permission-log)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-durability)
(require 'mevedel-session-recovery)
(require 'mevedel-session-transfer)
(require 'mevedel-session-publication)
(require 'mevedel-session-save-as)
(require 'mevedel-session-persistence)
(require 'mevedel-session-control-transfer)
(require 'mevedel-tool-repair)
(require 'mevedel-tools)
(require 'mevedel-worktree)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `gptel'
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--preset)
(defvar gptel-system-prompt)

;; `gptel-request'
(declare-function gptel--make-backend "ext:gptel-request" (&rest slots))
(declare-function gptel-get-backend "ext:gptel-request" (name))
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-make-fsm "ext:gptel-request" (&rest slots))

;; `mevedel-permissions'
(declare-function mevedel-permission-add-session-resource-grant
                  "mevedel-permissions" (session path access))

;; `mevedel-session-persistence'
(defvar mevedel-session-persistence--summary-cache)

;; `mevedel-skills-core'
(declare-function mevedel-skills--maybe-activate
                  "mevedel-skills-core" (session path))

;; `mevedel-tools'
(declare-function mevedel-tools--handle-message-inject "mevedel-tools" (fsm))

;; `org'
(declare-function org-entry-delete "org" (pom property))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (pom property value))

;; `so-long'
(defvar so-long-predicate)

(mevedel-tools-register)


;;
;;; Helpers

(defun test-mevedel-session-persistence--git (directory &rest args)
  "Run Git ARGS in DIRECTORY and return its trimmed output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eq 0 (apply #'process-file "git" nil t nil args))
        (error "Git failed: %s" (buffer-string)))
      (string-trim (buffer-string)))))

(defun test-mevedel-session-persistence--agent-backend ()
  "Return the registered backend used by retained-agent fixtures."
  (let ((name "Session Persistence Agent Test"))
    (condition-case nil
        (gptel-get-backend name)
      (user-error
       (let ((backend (gptel--make-backend :name name)))
         (setf (gptel-get-backend name) backend)
         backend)))))

(defun test-mevedel-session-persistence--make-workspace (root)
  "Build a workspace struct registered in the global registry.
ROOT is a temporary directory owned and cleaned up by the caller."
  (mevedel-workspace-clear-registry)
  (make-directory (file-name-concat root "packages" "api") t)
  (let ((workspace
         (mevedel-workspace-get-or-create
          'project "test-id" root
          (file-name-nondirectory (directory-file-name root)))))
    (mevedel-workspace-identity-ensure root)
    workspace))

(defun test-mevedel-session-persistence--make-file-workspace (root)
  "Build a file-workspace authority rooted at ROOT.
Use this for tests that exercise direct local files rather than project
publication."
  (mevedel-workspace-clear-registry)
  (mevedel-workspace-get-or-create
   'file
   (file-name-nondirectory (directory-file-name root))
   root
   (file-name-nondirectory (directory-file-name root))))

(defun test-mevedel-session-persistence--pid-lock-context ()
  "Return an explicit file-session authority profile for lock tests."
  (mevedel-session--create :authority-mode 'pid-lock))

(defun test-mevedel-session-persistence--make-session (root)
  "Build a populated session for ROOT in round-trip cases."
  (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
         (root (mevedel-workspace-root workspace))
         (session   (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-working-directory session)
          (file-name-as-directory
           (file-name-concat root "packages" "api")))
    (setf (mevedel-session-permission-mode session) 'ask)
    (setf (mevedel-session-sandbox-mode session) 'required)
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
    (setf (mevedel-session-workspace-instruction-hashes session)
          (list (cons (list "/root" (file-name-concat root "AGENTS.md"))
                      (make-string 64 ?a))))
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

(defun test-mevedel-session-persistence--make-remote-restore-fixture
    (host local-root transcript)
  "Create a mock-TRAMP session for HOST at LOCAL-ROOT with TRANSCRIPT."
  (let* ((remote-root
          (format "/mevedelmock:%s:%s"
                  host (file-name-as-directory local-root)))
         (workspace
          (mevedel-workspace-get-or-create
           'project remote-root remote-root "remote-restore"))
         (identity (mevedel-workspace-identity-ensure remote-root))
         (session (mevedel-session-create "main" workspace remote-root))
         (target (mevedel-session-execution-target session))
         (target-incarnation
          (progn
            (mevedel-execution-target-probe target t 'off)
            (mevedel-execution-target-incarnation target)))
         (session-id "main-remote-restore")
         (session-dir
          (file-name-as-directory
           (file-name-concat remote-root ".mevedel" "sessions" session-id)))
         (segment
          (mevedel-session-persistence--segment-path session-dir 1))
         (sidecar
          (test-mevedel-session-persistence--complete-sidecar
           (list
            :session-id session-id
            :session-name "main"
            :workspace
            (list :type 'project
                  :workspace-id identity
                  :target-native-root (file-name-as-directory local-root)
                  :name "remote-restore")
            :working-directory (file-name-as-directory local-root)
            :target-incarnation target-incarnation))))
    (setf (mevedel-session-session-id session) session-id
          (mevedel-session-save-path session) session-dir)
    (make-directory session-dir t)
    (write-region transcript nil segment nil 'silent)
    (with-temp-file
        (mevedel-session-persistence--sidecar-path session-dir)
      (let ((print-length nil)
            (print-level nil))
        (prin1 sidecar (current-buffer))))
    ;; Remote readers discover only immutable, sidecar-committed state.  Leave
    ;; the fixed files in place as non-authoritative caches for poison/race
    ;; cases, but seed a real publication head for the fixture.
    (let ((mevedel-session-durability--client-id
           (secure-hash 'sha256 (format "fixture-%s-%s" host local-root))))
      (unwind-protect
          (progn
            (unless
                (mevedel-session-durability-lease-acquire
                 session-dir "*fixture-publisher*" session)
              (error "Could not acquire fixture publication lease"))
            (mevedel-session-publication-publish
             session
             (list
              (list :path segment :content transcript)
              (list
               :path (mevedel-session-persistence--sidecar-path session-dir)
               :content
               (mevedel-session-persistence--printed-value sidecar)
               :commit-marker t))))
        (mevedel-session-durability-lease-release session-dir session)))
    (list workspace session session-dir segment)))

(defun test-mevedel-session-persistence--agent-configuration ()
  "Return a compact frozen configuration for persistence tests."
  (mevedel-agent-configuration--create
   :agent
   (mevedel-agent--create
    :name "default"
    :description "Persisted default agent"
    :tools '((:tool "Read"))
    :system-prompt "Frozen persistence instructions"
    :max-turns 10
    :hook-rules nil
    :frozen-p t)
   :request-locals
   (list (cons 'gptel-backend
               (or gptel-backend
                   (test-mevedel-session-persistence--agent-backend)))
         (cons 'gptel-model 'test-model)
         (cons 'gptel-tools
               (list (gptel-get-tool '("mevedel" "Read"))))
         (cons 'gptel-context '(("/tmp/persisted-context.el"))))))

(defun test-mevedel-session-persistence--complete-sidecar (plist)
  "Return a current complete sidecar with PLIST values overriding defaults."
  (let ((sidecar
         (list :version mevedel-session-persistence-format-version
               :session-id "test-session"
               :session-name "x"
               :workspace '(:type project
                            :workspace-id
                            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                            :target-native-root "/tmp/"
                            :name "test")
               :working-directory "/tmp/"
               :authority-mode 'portable
               :target-incarnation "test-incarnation"
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
               :fork-type nil
               :forked-from-fork-point-id nil
               :worktree-source-root nil
               :worktree-directory nil
               :worktree-branch nil
               :worktree-base-commit nil
               :permission-mode 'ask
               :sandbox-mode 'best-effort
               :plan-mode nil
               :permission-rules nil
               :resource-grants nil
               :preset-name nil
               :preset-settings nil
               :model-provider nil
               :reasoning-effort nil
               :last-observed-date "2026-01-01"
               :agent-types-snapshot :uninitialized
               :skills-snapshot :uninitialized
               :additional-roots nil
               :tasks nil
               :prompt-index nil
               :file-snapshots nil
               :workspace-instruction-hashes nil
               :agent-transcripts nil
               :agent-registry nil
               :agent-turn-capacity 3
               :plan-metadata nil
               :goal nil
               :messages nil)))
    (let ((overrides plist))
      (while plist
        (setq sidecar (plist-put sidecar (pop plist) (pop plist))))
      (when (and (not (plist-member overrides :authority-mode))
                 (eq (plist-get (plist-get sidecar :workspace) :type) 'file))
        (setq sidecar (plist-put sidecar :authority-mode 'pid-lock))))
    sidecar))

(defun test-mevedel-session-persistence--deserialize-sidecar (overrides)
  "Deserialize a complete sidecar with OVERRIDES in a real workspace."
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-sidecar-workspace-" t))))
    (unwind-protect
        (let* ((workspace
                (test-mevedel-session-persistence--make-workspace root))
               (sidecar
                (test-mevedel-session-persistence--complete-sidecar
                 (append
                  (list :workspace
                        (mevedel-session-persistence--workspace-to-plist
                         workspace)
                        :working-directory root)
                  overrides))))
          (mevedel-session-persistence-deserialize sidecar workspace))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


;;
;;; Workspace round-trip

(mevedel-deftest mevedel-session-persistence--workspace-to-plist ()
  ,test
  (test)
  :doc "captures durable identity and target-native root"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
               (plist (mevedel-session-persistence--workspace-to-plist workspace)))
          (should (eq 'project    (plist-get plist :type)))
          (should (equal (mevedel-workspace-identity-read root)
                         (plist-get plist :workspace-id)))
          (should (equal (file-name-as-directory root)
                         (plist-get plist :target-native-root)))
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
  :doc "accepts the currently opened matching workspace"
  (let ((root (make-temp-file "mevedel-workspace-match-" t)))
    (unwind-protect
        (let* ((workspace
                (test-mevedel-session-persistence--make-workspace root))
               (plist
                (mevedel-session-persistence--workspace-to-plist workspace)))
          (should
           (equal (cons workspace nil)
                  (mevedel-session-persistence--workspace-from-plist
                   plist workspace))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "requires confirmation for a different project-owned identity"
  (let ((saved-root (make-temp-file "mevedel-workspace-saved-" t))
        (opened-root (make-temp-file "mevedel-workspace-opened-" t)))
    (unwind-protect
        (let* ((saved
                (test-mevedel-session-persistence--make-workspace saved-root))
               (plist
                (mevedel-session-persistence--workspace-to-plist saved))
               (opened
                (progn
                  (mevedel-workspace-clear-registry)
                  (test-mevedel-session-persistence--make-workspace
                   opened-root))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-persistence--workspace-from-plist plist opened)
             :type 'user-error))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (should
             (equal (cons opened t)
                    (mevedel-session-persistence--workspace-from-plist
                     plist opened)))))
      (delete-directory saved-root t)
      (delete-directory opened-root t)
      (mevedel-workspace-clear-registry))))


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
  :doc "captures the strict phase-free Goal schema"
  (let* ((goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'blocked
                :reason "Need an API credential."
                :token-budget 1000 :tokens-used 345
                :time-used-seconds 12 :turns-run 4
                :plan-reference "local/plans/accepted.md"
                :created-at "created" :updated-at "updated"))
         (plist (mevedel-session-persistence--goal-to-plist goal)))
    (should (equal "g1" (plist-get plist :id)))
    (should (eq 'blocked (plist-get plist :status)))
    (should (= 1000 (plist-get plist :token-budget)))
    (should (= 345 (plist-get plist :tokens-used)))
    (should (= 12 (plist-get plist :time-used-seconds)))
    (should (= 4 (plist-get plist :turns-run)))
    (should (equal "local/plans/accepted.md" (plist-get plist :plan-reference)))
    (should (equal "Need an API credential." (plist-get plist :reason)))))

(mevedel-deftest mevedel-session-persistence--goal-from-plist ()
  ,test
  (test)
  :doc "rebuilds the current strict Goal schema"
  (let ((goal (mevedel-session-persistence--goal-from-plist
               '(:id "g1" :objective "Ship" :status active :reason nil
                 :token-budget 1000 :tokens-used 25
                 :time-used-seconds 7 :turns-run 2
                 :plan-reference "local/plans/accepted.md"
                 :created-at "created" :updated-at "updated"))))
    (should (mevedel-goal-p goal))
    (should (equal "Ship" (mevedel-goal-objective goal)))
    (should (= 1000 (mevedel-goal-token-budget goal)))
    (should (= 25 (mevedel-goal-tokens-used goal)))
    (should (= 7 (mevedel-goal-time-used-seconds goal)))
    (should (= 2 (mevedel-goal-turns-run goal))))
  :doc "round-trips a budget-limited Goal with its exact usage and reason"
  (let ((goal (mevedel-session-persistence--goal-from-plist
               '(:id "g2" :objective "Ship" :status budget-limited
                 :reason "Token budget reached: 110/100 tokens used"
                 :token-budget 100 :tokens-used 110
                 :time-used-seconds 9 :turns-run 3
                 :plan-reference nil
                 :created-at "created" :updated-at "updated"))))
    (should (eq 'budget-limited (mevedel-goal-status goal)))
    (should (= 100 (mevedel-goal-token-budget goal)))
    (should (= 110 (mevedel-goal-tokens-used goal)))
    (should (equal "Token budget reached: 110/100 tokens used"
                   (mevedel-goal-reason goal))))
  :doc "keeps sessions without a Goal empty"
  (should-not (mevedel-session-persistence--goal-from-plist nil))
  :doc "rejects old, incomplete, and unsafe Goal records"
  (let ((valid '(:id "g1" :objective "Ship" :status active
                 :reason nil :token-budget nil :tokens-used 0
                 :time-used-seconds 0 :turns-run 0 :plan-reference nil
                 :created-at "created" :updated-at "updated")))
    (dolist (change '((:id "../escape")
                      (:status unknown)
                      (:tokens-used -1)
                      (:plan-reference "../escape.md")
                      (:reason 42)))
      (let ((plist (copy-tree valid)))
        (setq plist (plist-put plist (car change) (cadr change)))
        (should-error
         (mevedel-session-persistence--goal-from-plist plist)
         :type 'error)))
    (should-error
     (mevedel-session-persistence--goal-from-plist
      '(:id "old" :objective "Ship" :status active :phase planning)))))


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
               (_ (setf (mevedel-session-plan-mode session) t
                        (mevedel-session-preset-name session) 'test-preset
                        (mevedel-session-model-provider session)
                        "Test:test-model"
                        (mevedel-session-reasoning-effort session) 'high
                        (mevedel-session-preset-settings session)
                        '((mevedel-model-tiers
                           (strong :provider "Test:test-model" :effort high))
                          (mevedel-model-workloads
                           (planning :tier strong)))))
               (plist (mevedel-session-persistence-serialize
                       session
                       :first-user-message "Refactor X"
                       :latest-user-message "Ship Y"
                       :additional-roots '(("alt" . "/tmp/alt")))))
          (should (equal mevedel-session-persistence-format-version
                         (plist-get plist :version)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (plist-get plist :session-id)))
          (should (equal "main" (plist-get plist :session-name)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (plist-get plist :working-directory)))
          (should (plist-member plist :target-incarnation))
          (should (stringp (plist-get plist :target-incarnation)))
          (should (equal 'ask (plist-get plist :permission-mode)))
          (should (equal 'required (plist-get plist :sandbox-mode)))
          (should (eq t (plist-get plist :plan-mode)))
          (should (eq 'test-preset (plist-get plist :preset-name)))
          (should (equal "Test:test-model"
                         (plist-get plist :model-provider)))
          (should (eq 'high (plist-get plist :reasoning-effort)))
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
          (should (equal (mevedel-session-workspace-instruction-hashes session)
                         (plist-get plist :workspace-instruction-hashes)))
          (should (= 3 (length (plist-get plist :permission-rules))))
          (should (= 2 (length (plist-get plist :resource-grants))))
          (should (= 2 (length (plist-get plist :tasks))))
          (should (plist-get plist :workspace))
          (should (plist-get plist :prompt-index))
          (should (plist-get plist :file-snapshots)))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "stores project identity and target-native workspace paths"
  (let* ((identity (make-string 64 ?a))
         (workspace
          (mevedel-workspace--create
           :type 'project
           :id "/ssh:first:/srv/project/"
           :root "/ssh:first:/srv/project/"
           :name "project"))
         (session (mevedel-session-create "main" workspace)))
    (mevedel-execution-target-seed-incarnation
     (mevedel-session-execution-target session) "remote-host-a")
    (cl-letf (((symbol-function 'mevedel-workspace-identity-read)
               (lambda (_root) identity)))
      (let* ((sidecar (mevedel-session-persistence-serialize session))
             (saved-workspace (plist-get sidecar :workspace)))
        (should (equal identity
                       (plist-get saved-workspace :workspace-id)))
        (should (equal "/srv/project/"
                       (plist-get saved-workspace :target-native-root)))
        (should (equal "/srv/project/"
                       (plist-get sidecar :working-directory)))
        (should (equal "remote-host-a"
                       (plist-get sidecar :target-incarnation)))
        (should-not (file-remote-p
                     (plist-get saved-workspace :target-native-root))))))
  :doc "stores remote session authority without a client TRAMP prefix"
  (let* ((identity (make-string 64 ?a))
         (workspace
          (mevedel-workspace--create
           :type 'project
           :id "/ssh:first:/srv/project/"
           :root "/ssh:first:/srv/project/"
           :name "project"))
         (session (mevedel-session-create "main" workspace)))
    (mevedel-execution-target-seed-incarnation
     (mevedel-session-execution-target session) "remote-host-a")
    (setf (mevedel-session-permission-rules session)
          '(("Read" :path "/ssh:first:/srv/project/src/**" :action allow)
            ("Bash" :pattern "git status"
             :file-system
             ((:path "/ssh:first:/srv/shared/input" :access read))
             :action allow))
          (mevedel-session-resource-grants session)
          '((:path "/ssh:first:/srv/project/out" :access write)))
    (cl-letf (((symbol-function 'mevedel-workspace-identity-read)
               (lambda (_root) identity)))
      (let ((sidecar (mevedel-session-persistence-serialize session)))
        (should
         (equal
          '(("Read" :path "/srv/project/src/**" :action allow)
            ("Bash" :pattern "git status"
             :file-system ((:path "/srv/shared/input" :access read))
             :action allow))
          (plist-get sidecar :permission-rules)))
        (should
         (equal '((:path "/srv/project/out" :access write))
                (plist-get sidecar :resource-grants)))
        (should-not
         (string-match-p "/ssh:first:"
                         (prin1-to-string sidecar))))))
  :doc "fork fields default nil for a non-fork session"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let* ((session (test-mevedel-session-persistence--make-session root))
               (plist (mevedel-session-persistence-serialize session)))
          (should (null (plist-get plist :forked-from-session-id)))
          (should (null (plist-get plist :forked-from-turn)))
          (should (null (plist-get plist :fork-type)))
          (should (null (plist-get plist :forked-from-fork-point-id)))
          (should (null (plist-get plist :worktree-directory))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "serializes durable fork type and stable fork-point lineage"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-forked-from-session-id session) "source"
                (mevedel-session-forked-from-turn session) 3
                (mevedel-session-fork-type session) 'worktree
                (mevedel-session-forked-from-fork-point-id session)
                "stable-point"
                (mevedel-session-worktree-source-root session) "/repo/"
                (mevedel-session-worktree-directory session)
                "/repo/.worktrees/main-fork-1/"
                (mevedel-session-worktree-branch session)
                "worktree/main-fork-1"
                (mevedel-session-worktree-base-commit session) "abc123")
          (let ((plist (mevedel-session-persistence-serialize session)))
            (should (eq 'worktree (plist-get plist :fork-type)))
            (should (equal "stable-point"
                           (plist-get plist
                                      :forked-from-fork-point-id)))
            (should (equal "/repo/.worktrees/main-fork-1/"
                           (plist-get plist :worktree-directory)))
            (should (equal "worktree/main-fork-1"
                           (plist-get plist :worktree-branch)))
            (should (equal "abc123"
                           (plist-get plist :worktree-base-commit)))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "materializes the canonical global mode when the session inherits it"
  (let ((root (make-temp-file "mevedel-test-proj-" t))
        (saved-mode (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-permission-mode session) nil)
          (set-default-toplevel-value 'mevedel-permission-mode 'edits)
          (should (eq 'edits
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
        (delete-directory root t))))
  :doc "materializes and validates the canonical global sandbox mode"
  (let ((root (make-temp-file "mevedel-test-proj-" t))
        (saved-mode (default-toplevel-value 'mevedel-sandbox-mode)))
    (unwind-protect
        (let ((session
               (test-mevedel-session-persistence--make-session root)))
          (setf (mevedel-session-sandbox-mode session) nil)
          (set-default-toplevel-value 'mevedel-sandbox-mode 'off)
          (should (eq 'off
                      (plist-get
                       (mevedel-session-persistence-serialize session)
                       :sandbox-mode)))
          (setf (mevedel-session-sandbox-mode session) 'auto)
          (should-error (mevedel-session-persistence-serialize session)
                        :type 'error))
      (set-default-toplevel-value 'mevedel-sandbox-mode saved-mode)
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
  :doc "requires a nonblank target incarnation field"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (cl-remf plist :target-incarnation)
    (should-error
     (mevedel-session-persistence--validate-current-sidecar plist)
     :type 'error)
    (dolist (incarnation '(42 "" "   "))
      (should-error
       (mevedel-session-persistence--validate-current-sidecar
        (plist-put plist :target-incarnation incarnation))
       :type 'error))
    (should
     (mevedel-session-persistence--validate-current-sidecar
      (plist-put plist :target-incarnation "remote-host-a"))))
  :doc "accepts only canonical persisted permission modes"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (dolist (mode '(ask edits full-auto))
      (should (eq plist
                  (mevedel-session-persistence--validate-current-sidecar
                   (plist-put plist :permission-mode mode)))))
    (dolist (mode '(default accept-edits trust-all edit))
      (should-error
       (mevedel-session-persistence--validate-current-sidecar
       (plist-put plist :permission-mode mode))
       :type 'error)))
  :doc "accepts only canonical persisted sandbox modes"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (dolist (mode '(best-effort required off))
      (should (eq plist
                  (mevedel-session-persistence--validate-current-sidecar
                   (plist-put plist :sandbox-mode mode)))))
    (should-error
     (mevedel-session-persistence--validate-current-sidecar
      (plist-put plist :sandbox-mode 'auto))
     :type 'error))
  :doc "accepts only boolean persisted Plan mode"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (dolist (mode '(nil t))
      (should (eq plist
                  (mevedel-session-persistence--validate-current-sidecar
                   (plist-put plist :plan-mode mode)))))
    (should-error
     (mevedel-session-persistence--validate-current-sidecar
     (plist-put plist :plan-mode 'plan))
     :type 'error))
  :doc "accepts only exact model labels and symbolic effort"
  (let ((plist (test-mevedel-session-persistence--complete-sidecar nil)))
    (should
     (mevedel-session-persistence--validate-current-sidecar
      (plist-put
       (plist-put plist :model-provider "OpenAI:gpt-5")
       :reasoning-effort 'high)))
    (dolist (provider '("gpt-5" openai 42))
      (should-error
       (mevedel-session-persistence--validate-current-sidecar
        (plist-put plist :model-provider provider))))
    (should-error
     (mevedel-session-persistence--validate-current-sidecar
      (plist-put
       (plist-put plist :model-provider nil)
       :reasoning-effort "high"))))
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
               (_ (setf (mevedel-session-plan-mode source) t
                        (mevedel-session-preset-name source) 'test-preset
                        (mevedel-session-model-provider source)
                        "Test:test-model"
                        (mevedel-session-reasoning-effort source) 'high
                        (mevedel-session-preset-settings source)
                        '((mevedel-model-tiers
                           (strong :provider "Test:test-model" :effort high))
                          (mevedel-model-workloads
                           (review :tier strong)))
                        (mevedel-session-pending-steering source)
                        '((:id 1 :input "steer"))
                        (mevedel-session-pending-follow-ups source)
                        '((:id 2 :input "later"))
                        (mevedel-session-pending-input-next-id source) 3
                        (mevedel-session-pending-input-paused source) t
                        (mevedel-session-pending-input-failure-paused source)
                        t))
               (plist (mevedel-session-persistence-serialize
                       source
                       :first-user-message "Hi"
                       :latest-user-message "Later"))
               (result
                (mevedel-session-persistence-deserialize
                 plist (mevedel-session-workspace source)))
               (session (plist-get result :session)))
          (should (mevedel-session-p session))
          (should (equal "main" (mevedel-session-name session)))
          (should (equal (file-name-as-directory
                          (file-name-concat root "packages" "api"))
                         (mevedel-session-working-directory session)))
          (should (equal "main-2026-04-23T14-30-a9f2"
                         (mevedel-session-session-id session)))
          (should (eq 'ask (mevedel-session-permission-mode session)))
          (should (eq 'required (mevedel-session-sandbox-mode session)))
          (should (mevedel-session-plan-mode session))
          (should (eq 'test-preset (mevedel-session-preset-name session)))
          (should (equal "Test:test-model"
                         (mevedel-session-model-provider session)))
          (should (eq 'high
                      (mevedel-session-reasoning-effort session)))
          (should (equal '((mevedel-model-tiers
                            (strong :provider "Test:test-model" :effort high))
                           (mevedel-model-workloads
                            (review :tier strong)))
                         (mevedel-session-preset-settings session)))
          (should (= 5 (mevedel-session-turn-count session)))
          (should (= 4 (mevedel-session-last-task-write-turn session)))
          (should (equal '(("alpha" . "Alpha helper"))
                         (mevedel-session-skills-snapshot session)))
          (should (equal
                   (mevedel-session-workspace-instruction-hashes source)
                   (mevedel-session-workspace-instruction-hashes session)))
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
          (dolist (key '(:pending-steering :pending-follow-ups
                         :pending-input-next-id :pending-input-paused
                         :pending-input-failure-paused))
            (should-not (plist-member plist key)))
          (should-not (mevedel-session-pending-input-p session))
          (should-not (mevedel-session-pending-input-next-id session))
          (should-not (mevedel-session-pending-input-paused session))
          (should-not
           (mevedel-session-pending-input-failure-paused session))
          ;; touched-files / mentions-shown reset to empty hash tables
          (should (hash-table-p (mevedel-session-touched-files session)))
          (should (zerop (hash-table-count (mevedel-session-touched-files session))))
          (should (hash-table-p (mevedel-session-mentions-shown session)))
          (should (zerop (hash-table-count (mevedel-session-mentions-shown session))))
          ;; workspace identity recovered
          (let ((workspace (mevedel-session-workspace session)))
            (should (eq 'project (mevedel-workspace-type workspace)))
            (should (equal "test-id" (mevedel-workspace-id workspace))))
          (should
           (equal
            (mevedel-execution-target-incarnation
             (mevedel-session-execution-target session))
            (plist-get plist :target-incarnation)))
          (should (mevedel-session-execution-target session)))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "rejects the superseded session format without a compatibility reader"
  (let ((sidecar
         (test-mevedel-session-persistence--complete-sidecar
          '(:version "v0.5.0"))))
    (should-error
     (mevedel-session-persistence-deserialize
      sidecar
      (mevedel-workspace--create
       :type 'project :id "/tmp/old-format/"
       :root "/tmp/old-format/" :name "old-format"))
     :type 'error))

  :doc "identity rebinding drops copied authority and additional roots"
  (let ((saved-root (make-temp-file "mevedel-rebind-saved-" t))
        (opened-root (make-temp-file "mevedel-rebind-opened-" t)))
    (unwind-protect
        (let* ((saved
                (test-mevedel-session-persistence--make-workspace saved-root))
               (saved-plist
                (mevedel-session-persistence--workspace-to-plist saved))
               (opened
                (progn
                  (mevedel-workspace-clear-registry)
                  (test-mevedel-session-persistence--make-workspace
                   opened-root)))
               (opened-identity
                (mevedel-workspace-identity-read opened-root))
               (sidecar
                (test-mevedel-session-persistence--complete-sidecar
                 (list
                  :workspace saved-plist
                  :working-directory saved-root
                  :permission-rules
                  `(("Read" :path ,(file-name-concat saved-root "src/**")
                            :action allow))
                  :resource-grants
                  `((:path ,(file-name-concat saved-root "out")
                            :access write))
                  :additional-roots '(("shared" . "/srv/shared/"))))))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-persistence-deserialize sidecar opened)
             :type 'user-error))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (let* ((result
                    (mevedel-session-persistence-deserialize sidecar opened))
                   (session (plist-get result :session))
                   (saved-again
                    (mevedel-session-persistence-serialize session)))
              (should-not (mevedel-session-permission-rules session))
              (should-not (mevedel-session-resource-grants session))
              (should-not (plist-get result :additional-roots))
              (should
               (equal opened-identity
                      (plist-get (plist-get saved-again :workspace)
                                 :workspace-id))))))
      (delete-directory saved-root t)
      (delete-directory opened-root t)
      (mevedel-workspace-clear-registry)))

  :doc "requalifies target-native authority through the opened remote alias"
  (let* ((identity (make-string 64 ?a))
         (workspace
          (mevedel-workspace--create
           :type 'project
           :id "/ssh:second:/srv/project/"
           :root "/ssh:second:/srv/project/"
           :name "project"))
         (sidecar
          (test-mevedel-session-persistence--complete-sidecar
           (list
            :workspace
            (list :type 'project
                  :workspace-id identity
                  :target-native-root "/srv/project/"
                  :name "project")
            :working-directory "/srv/project/"
            :target-incarnation "remote-host-a"
            :permission-rules
            '(("Read" :path "/srv/project/src/**" :action allow)
              ("Bash" :pattern "git status"
               :file-system ((:path "/srv/shared/input" :access read))
               :action allow))
            :resource-grants
            '((:path "/srv/project/out" :access write))))))
    (cl-letf (((symbol-function 'mevedel-workspace-identity-read)
               (lambda (_root) identity))
              ((symbol-function 'file-in-directory-p)
               (lambda (_file _directory) t)))
      (let ((session
             (plist-get
              (mevedel-session-persistence-deserialize sidecar workspace)
              :session)))
        (should
         (equal "remote-host-a"
                (mevedel-execution-target-incarnation
                 (mevedel-session-execution-target session))))
        (should
         (equal
          '(("Read" :path "/ssh:second:/srv/project/src/**" :action allow)
            ("Bash" :pattern "git status"
             :file-system
             ((:path "/ssh:second:/srv/shared/input" :access read))
             :action allow))
          (mevedel-session-permission-rules session)))
        (should
         (equal
          '((:path "/ssh:second:/srv/project/out" :access write))
          (mevedel-session-resource-grants session))))))

  :doc "drops persisted opaque and unknown agent task owners"
  (let ((root (make-temp-file "mevedel-task-owner-roundtrip-" t)))
    (unwind-protect
        (let* ((source
                (test-mevedel-session-persistence--make-session root))
               (configuration
                (test-mevedel-session-persistence--agent-configuration))
               (worker-id
                "worker--0123456789abcdef0123456789abcdef")
               (reviewer-id
                "reviewer--fedcba9876543210fedcba9876543210")
               (valid-owner "/root/worker/reviewer"))
          (setf
           (mevedel-session-agent-registry source)
           (list
            (cons
             "/root/worker"
             (mevedel-agent-record--create
              :id worker-id
              :path "/root/worker"
              :parent-path "/root"
              :role "default"
              :configuration configuration
              :activity 'idle
              :conversation-location "agents/worker.chat.org"))
            (cons
             valid-owner
             (mevedel-agent-record--create
              :id reviewer-id
              :path valid-owner
              :parent-path "/root/worker"
              :role "default"
              :configuration configuration
              :activity 'idle
              :conversation-location "agents/reviewer.chat.org")))
           (mevedel-session-tasks source)
           (list
            (mevedel-task--create
             :id 1 :subject "valid nested" :status 'pending
             :owner valid-owner :blocks '(2) :blocked-by '(3))
            (mevedel-task--create
             :id 2 :subject "opaque" :status 'pending
             :owner worker-id)
            (mevedel-task--create
             :id 3 :subject "unknown" :status 'pending
             :owner "/root/ghost"))
           (mevedel-session-task-status-notes source)
           (list
            (list valid-owner :note "valid")
            (list worker-id :note "opaque")
            (list "/root/ghost" :note "unknown")))
          (let* ((sidecar (mevedel-session-persistence-serialize source))
                 (restored
                  (plist-get
                   (mevedel-session-persistence-deserialize
                    sidecar (mevedel-session-workspace source))
                   :session)))
            (should
             (equal (list valid-owner)
                    (mapcar #'mevedel-task-owner
                            (mevedel-session-tasks restored))))
            (should-not
             (mevedel-task-blocks (car (mevedel-session-tasks restored))))
            (should-not
             (mevedel-task-blocked-by
              (car (mevedel-session-tasks restored))))
            (should
             (equal (list valid-owner)
                    (mapcar #'car
                            (mevedel-session-task-status-notes restored))))
            (should (= 2 (length
                          (mevedel-session-agent-registry restored))))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))

  :doc "preserves an accepted standalone Plan implementation retry"
  (let* ((retry
          '(:step submit
            :selection (:location here :context summary
                        :execution direct :mode edits)
            :accepted (:path "local/plans/accepted.md"
                       :absolute-path "/tmp/session/local/plans/accepted.md"
                       :hash "abc")
            :summary "# Handoff"
            :failure "Transport refused"))
         (metadata (list :status 'accepted :implementation-retry retry))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :plan-metadata metadata)))
         (session (plist-get result :session)))
    (should (equal metadata (mevedel-session-plan-metadata session))))

  :doc "preserves a prepared standalone Plan Worktree target"
  (let* ((retry
          '(:step submit
            :selection (:location worktree :context fresh
                        :execution direct :mode full-auto
                        :branch "plan/topic")
            :accepted (:path "local/plans/source.md"
                       :absolute-path "/tmp/source/local/plans/source.md"
                       :hash "abc")
            :target-directory "/tmp/repo/.worktrees/topic/"
            :target-save-path "/tmp/repo/.mevedel/target/"
            :target-session-id "target-id"
            :target-accepted
            (:path "local/plans/accepted.md"
             :absolute-path "/tmp/repo/.mevedel/target/local/plans/accepted.md"
             :hash "abc")))
         (metadata (list :status 'accepted :implementation-retry retry))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :plan-metadata metadata)))
         (session (plist-get result :session)))
    (should (equal metadata (mevedel-session-plan-metadata session))))

  :doc "demotes a persisted active Goal to paused on session resume"
  (let* ((goal '(:id "g1" :objective "Ship" :status active :reason nil
                 :token-budget nil :tokens-used 9 :time-used-seconds 4
                 :turns-run 2 :plan-reference nil
                 :created-at "created" :updated-at "updated"))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :goal goal)))
         (restored (mevedel-session-goal (plist-get result :session))))
    (should (eq 'paused (mevedel-goal-status restored)))
    (should (equal "session resumed" (mevedel-goal-reason restored)))
    (should (= 9 (mevedel-goal-tokens-used restored))))

  :doc "loads an old Goal schema as no Goal"
  (let* ((old '(:id "g1" :objective "Ship" :status active
                :phase planning :approval-policy supervised))
         (result
          (test-mevedel-session-persistence--deserialize-sidecar
           (list :goal old))))
    (should-not (mevedel-session-goal (plist-get result :session))))
  :doc "drops permission rules with unknown actions"
  (let* ((plist (list :version mevedel-session-persistence-format-version
                      :session-name "x"
                      :permission-rules
                      '(("Read"  :path "/x" :action allow)
                        ("Write" :path "/y" :action future-action))
                      :tasks nil
                      :prompt-index nil
                      :file-snapshots nil))
         (session
          (plist-get
           (test-mevedel-session-persistence--deserialize-sidecar plist)
           :session)))
    (should (= 1 (length (mevedel-session-permission-rules session)))))
  :doc "preserves relocated working directories under the new workspace root"
  (let* ((old-root (make-temp-file "mevedel-old-root-" t))
         (new-root (make-temp-file "mevedel-new-root-" t))
         (old-cwd (file-name-concat old-root "packages/api"))
         (new-cwd (file-name-concat new-root "packages/api")))
    (unwind-protect
        (progn
          (make-directory old-cwd t)
          (make-directory new-cwd t)
          (let* ((saved-workspace
                  (test-mevedel-session-persistence--make-workspace old-root))
                 (saved-plist
                  (mevedel-session-persistence--workspace-to-plist
                   saved-workspace)))
            (make-directory (file-name-concat new-root ".mevedel") t)
            (copy-file
             (file-name-concat old-root ".mevedel" "workspace-id")
             (file-name-concat new-root ".mevedel" "workspace-id"))
            (mevedel-workspace-clear-registry)
            (let* ((opened-workspace
                    (mevedel-workspace-get-or-create
                     'project new-root new-root "relocated-proj"))
                   (sidecar
                    (test-mevedel-session-persistence--complete-sidecar
                     (list :workspace saved-plist
                           :working-directory old-cwd)))
                   (session
                    (plist-get
                     (mevedel-session-persistence-deserialize
                      sidecar opened-workspace)
                     :session)))
            (should (equal (file-name-as-directory new-cwd)
                           (mevedel-session-working-directory session))))))
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
         (saved-cwd new-root))
    (unwind-protect
        (progn
          (make-directory new-root t)
          (let* ((saved-workspace
                  (test-mevedel-session-persistence--make-workspace old-root))
                 (saved-plist
                  (mevedel-session-persistence--workspace-to-plist
                   saved-workspace)))
            (make-directory (file-name-concat new-root ".mevedel") t)
            (copy-file
             (file-name-concat old-root ".mevedel" "workspace-id")
             (file-name-concat new-root ".mevedel" "workspace-id"))
            (mevedel-workspace-clear-registry)
            (let* ((opened-workspace
                    (mevedel-workspace-get-or-create
                     'project new-root new-root "nested-proj"))
                   (sidecar
                    (test-mevedel-session-persistence--complete-sidecar
                     (list :workspace saved-plist
                           :working-directory saved-cwd)))
                   (session
                    (plist-get
                     (mevedel-session-persistence-deserialize
                      sidecar opened-workspace)
                     :session)))
              (should (equal new-root
                             (mevedel-session-working-directory session))))))
      (mevedel-workspace-clear-registry)
      (when (file-directory-p old-root)
        (delete-directory old-root t))))

  :doc "rejects restored working directories outside the workspace"
  (let ((root (make-temp-file "mevedel-restore-root-" t))
        (outside (make-temp-file "mevedel-restore-outside-" t)))
    (unwind-protect
        (let* ((workspace
                (test-mevedel-session-persistence--make-workspace root))
               (sidecar
                (test-mevedel-session-persistence--complete-sidecar
                 (list
                  :workspace
                  (mevedel-session-persistence--workspace-to-plist workspace)
                  :working-directory outside))))
          (should-error
           (mevedel-session-persistence-deserialize sidecar workspace)
           :type 'user-error))
      (delete-directory root t)
      (delete-directory outside t)
      (mevedel-workspace-clear-registry)))

  :doc "rejects restored symlink working directories outside the workspace"
  (let* ((root (make-temp-file "mevedel-restore-root-" t))
         (outside (make-temp-file "mevedel-restore-outside-" t))
         (link (file-name-concat root "linked-cwd")))
    (unwind-protect
        (let* ((workspace
                (test-mevedel-session-persistence--make-workspace root))
               (sidecar
                (test-mevedel-session-persistence--complete-sidecar
                 (list
                  :workspace
                  (mevedel-session-persistence--workspace-to-plist workspace)
                  :working-directory link))))
          (make-symbolic-link outside link)
          (should-error
           (mevedel-session-persistence-deserialize sidecar workspace)
           :type 'user-error))
      (when (file-symlink-p link)
        (delete-file link))
      (when (file-directory-p root)
        (delete-directory root t))
      (when (file-directory-p outside)
        (delete-directory outside t))
      (mevedel-workspace-clear-registry))))


;;
;;; Sidecar IO

(mevedel-deftest mevedel-session-persistence-write ()
  ,test
  (test)
  :doc "atomic write produces a readable plist"
  (let ((tmp (make-temp-file "mevedel-session-meta-test-" nil ".el")))
    (unwind-protect
        (let* ((plist `(:version ,mevedel-session-persistence-format-version
                                 :session-name "main"
                                 :tasks nil
                                 :permission-rules nil)))
          (mevedel-session-persistence-write tmp plist)
          (should (file-exists-p tmp))
          (let ((readback (mevedel-session-persistence-read tmp)))
            (should (equal "main" (plist-get readback :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "atomic write preserves shared objects as readable circle syntax"
  (let* ((tmp (make-temp-file "mevedel-session-meta-test-" nil ".el"))
         (shared (propertize "prompt" 'mevedel-mention-binding
                             (list :token "prompt")))
         (plist (list :history (list shared shared))))
    (unwind-protect
        (progn
          (mevedel-session-persistence-write tmp plist)
          (let* ((readback (mevedel-session-persistence-read tmp))
                 (history (plist-get readback :history)))
            (should (eq (car history) (cadr history)))))
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
  (let* ((root (make-temp-file "mevedel-transcript-atomic-" t))
         (path (file-name-concat root "segment.org")))
    (unwind-protect
        (progn
          (write-region "original" nil path nil 'silent)
          ;; A read-only parent stops the control filesystem before its
          ;; rename, which is the only way the published bytes can appear.
          (set-file-modes root #o500)
          (with-temp-buffer
            (insert "replacement")
            (should-error
             (mevedel-session-persistence--write-current-buffer-atomically
              path)))
          (should
           (equal "original"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (set-file-modes root #o700)
      (delete-directory root t))))

(mevedel-deftest mevedel-session-persistence--set-visited-segment-file ()
  ,test
  (test)
  :doc "sets canonical visited-file identity without renaming the live buffer"
  (let* ((directory (make-temp-file "mevedel-segment-identity-" t))
         (old-path (file-name-concat directory "segment-0001.chat.org"))
         (new-path (file-name-concat directory "segment-0002.chat.org"))
         (buffer (generate-new-buffer "*test-segment-identity*")))
    (unwind-protect
        (progn
          (write-region "old" nil old-path nil 'silent)
          (write-region "new" nil new-path nil 'silent)
          (with-current-buffer buffer
            (set-visited-file-name old-path t)
            (insert "new")
            (let ((name (buffer-name)))
              (mevedel-session-persistence--set-visited-segment-file new-path)
              (should (equal name (buffer-name))))
            (should (file-equal-p new-path buffer-file-name))
            (should (equal (file-truename new-path) buffer-file-truename))
            (should (verify-visited-file-modtime buffer))
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p directory) (delete-directory directory t)))))

(mevedel-deftest mevedel-session-persistence--publish-segment-text ()
  ,test
  (test)
  :doc "atomically publishes text and installs matching live-buffer state"
  (let* ((directory (make-temp-file "mevedel-segment-publish-" t))
         (old-path (file-name-concat directory "segment-0001.chat.org"))
         (new-path (file-name-concat directory "segment-0002.chat.org"))
         (buffer (generate-new-buffer "*test-segment-publish*")))
    (unwind-protect
        (progn
          (write-region "old" nil old-path nil 'silent)
          (with-current-buffer buffer
            (set-visited-file-name old-path t)
            (insert "old")
            (set-buffer-modified-p nil)
            (mevedel-session-persistence--publish-segment-text
             new-path "replacement")
            (should (equal "replacement" (buffer-string)))
            (should (file-equal-p new-path buffer-file-name))
            (should (verify-visited-file-modtime buffer)))
          (should
           (equal "replacement"
                  (with-temp-buffer
                    (insert-file-contents new-path)
                    (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p directory) (delete-directory directory t)))))

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

(mevedel-deftest mevedel-session-persistence-segments ()
  ,test
  (test)
  :doc "lists the canonical range without hiding broken archived segments"
  (let* ((directory (make-temp-file "mevedel-segment-list-" t))
         (session
          (mevedel-session--create
           :name "segments"
           :save-path (file-name-as-directory directory)
           :authority-mode 'pid-lock
           :current-segment 4
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "first prompt")))
             (2 . ((:cum-turn 2 :preview "missing prompt")))
             (3 . ((:cum-turn 3 :preview "unreadable prompt")))
             (4 . ((:cum-turn 4 :preview "live prompt"))))))
         (live-buffer (generate-new-buffer " *segment-list-live*")))
    (unwind-protect
        (progn
          (write-region "segment one\n" nil
                        (mevedel-session-persistence--segment-path
                         directory 1)
                        nil 'silent)
          (make-directory
           (mevedel-session-persistence--segment-path directory 3))
          (let ((segments
                 (mevedel-session-persistence-segments
                  session live-buffer)))
            (should (equal '(1 2 3 4)
                           (mapcar
                            (lambda (entry) (plist-get entry :number))
                            segments)))
            (should (equal '(readable missing unreadable readable)
                           (mapcar
                            (lambda (entry) (plist-get entry :status))
                            segments)))
            (should (equal '(nil nil nil t)
                           (mapcar
                            (lambda (entry) (plist-get entry :current-p))
                            segments)))
            (should (equal
                     '("first prompt" "missing prompt"
                       "unreadable prompt" "live prompt")
                     (mapcar
                      (lambda (entry) (plist-get entry :preview))
                      segments)))))
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (delete-directory directory t))))

(mevedel-deftest mevedel-session-persistence-read-segment ()
  ,test
  (test)
  :doc "loads restored transcript properties into a non-authoritative buffer"
  (let* ((directory (make-temp-file "mevedel-segment-read-" t))
         (path (mevedel-session-persistence--segment-path directory 1))
         (session
          (mevedel-session--create
           :name "segments"
           :save-path (file-name-as-directory directory)
           :authority-mode 'pid-lock
           :current-segment 2))
         inspection)
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
                    "Prompt\n"
                    "Archived answer.\n")
            (dotimes (_ 3)
              (goto-char (point-min))
              (search-forward "Archived answer.")
              (org-entry-put
               (point-min) "GPTEL_BOUNDS"
               (prin1-to-string
                `((response (,(match-beginning 0) ,(match-end 0)))))))
            (write-region (point-min) (point-max) path nil 'silent))
          (setq inspection
                (mevedel-session-persistence-read-segment session 1))
          (with-current-buffer inspection
            (should (derived-mode-p 'org-mode))
            (should buffer-read-only)
            (should (bound-and-true-p
                     mevedel-session--inspection-buffer-p))
            (should-not (bound-and-true-p mevedel--session))
            (should-not buffer-file-name)
            (goto-char (point-min))
            (search-forward "Archived answer.")
            (should (eq 'response
                        (get-text-property (match-beginning 0) 'gptel)))
            (should-not
             (mevedel-session-persistence--authoritative-buffer
              inspection))))
      (when (buffer-live-p inspection)
        (kill-buffer inspection))
      (delete-directory directory t)))

  :doc "reports the exact missing path"
  (let* ((directory (make-temp-file "mevedel-segment-missing-" t))
         (session
          (mevedel-session--create
           :name "segments"
           :save-path (file-name-as-directory directory)
           :authority-mode 'pid-lock
           :current-segment 2))
         (path (mevedel-session-persistence--segment-path directory 1)))
    (unwind-protect
        (let ((error
               (should-error
                (mevedel-session-persistence-read-segment session 1)
                :type 'user-error)))
          (should (string-search path (error-message-string error))))
      (delete-directory directory t))))

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
  "Build a file workspace rooted in a fresh tempdir.
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
                   'file basename tempdir basename)))
    (mevedel-workspace-identity-ensure tempdir)
    (cons ws tempdir)))

(defun test-mevedel-session-persistence--release-and-kill (buf session)
  "Release SESSION's lock and kill BUF if alive.
Mirrors the production kill-buffer-hook's lock release for tests
that don't go through `mevedel--chat-buffer-init-common' (which
installs the real hook)."
  (when (and session (mevedel-session-save-path session))
    (mevedel-session-persistence-lock-release
     (mevedel-session-save-path session)
     session))
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf)))

(defun test-mevedel-session-persistence--expire-session (session)
  "Set SESSION's persisted update time to fourteen days ago."
  (let* ((save-path (mevedel-session-save-path session))
         (sidecar (mevedel-session-persistence--sidecar-path save-path))
         (plist (mevedel-session-persistence-read sidecar)))
    (plist-put
     plist :updated-at
     (format-time-string
      "%FT%H-%M-%S"
      (time-subtract (current-time) (* 14 24 60 60))))
    (mevedel-session-persistence-write sidecar plist)))

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

(mevedel-deftest mevedel-session-persistence--root-data-buffer-p ()
  ,test
  (test)
  :doc "recognizes only root session data buffers"
  (let ((session (mevedel-session--create :name "root-role"))
        (root (generate-new-buffer " *test-root-role*"))
        (view (generate-new-buffer " *test-view-role*"))
        (agent (generate-new-buffer " *test-agent-role*")))
    (unwind-protect
        (progn
          (with-current-buffer root
            (setq-local mevedel--session session))
          (with-current-buffer view
            (setq-local mevedel--session session))
          (with-current-buffer view
            (setq-local mevedel--data-buffer root))
          (with-current-buffer agent
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation t))
          (mevedel-session-set-root-buffer session root)
          (should
           (mevedel-session-persistence--root-data-buffer-p root))
          (should-not
           (mevedel-session-persistence--root-data-buffer-p view))
          (should-not
           (mevedel-session-persistence--root-data-buffer-p agent)))
      (dolist (buffer (list root view agent))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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
      (mevedel-workspace-clear-registry)))
  :doc "rejects agent conversation buffers"
  (let ((buf (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--agent-invocation t)
          (should-not
           (mevedel-session-persistence--authoritative-buffer buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun test-mevedel-session-persistence--cold-agent-tree-round-trip ()
  "Exercise one durable agent-tree cold resume and its recovery boundary."
  (mevedel-tools-register)
  (require 'mevedel-agent-runtime)
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (root (generate-new-buffer "*test-agent-tree-root*"))
           (configuration
            (test-mevedel-session-persistence--agent-configuration))
           session-dir restored restored-session)
      (unwind-protect
          (progn
            (with-current-buffer root
              (org-mode)
              (insert "Root conversation\n")
              (mevedel-session-persistence-save session root))
            (setq session-dir (mevedel-session-save-path session))
            (let* ((agents-dir (file-name-concat session-dir "agents"))
                   (idle-relative "agents/idle.chat.org")
                   (active-relative "agents/active.chat.org")
                   (idle-file (expand-file-name idle-relative session-dir))
                   (active-file (expand-file-name active-relative session-dir))
                   (idle
                    (mevedel-agent-record--create
                     :id "opaque-idle" :path "/root/idle"
                     :parent-path "/root" :role "default"
                     :configuration configuration :activity 'idle
                     :conversation-location idle-relative
                     :mailbox
                     (list
                      (list :type 'MAIL :sender "/root"
                            :recipient "/root/idle" :payload "child mail"
                            :timestamp '(1 0 0 0)))))
                   (active
                    (let ((invocation
                           (mevedel-agent-invocation--create
                            :path "/root/active" :parent-session session)))
                      (mevedel-agent-record--create
                       :id "opaque-active" :path "/root/active"
                       :parent-path "/root" :role "default"
                       :configuration configuration
                       :activity 'running :invocation invocation
                       :conversation-location active-relative)))
                   (bad
                    (mevedel-agent-record--create
                     :id "opaque-bad" :path "/root/bad"
                     :parent-path "/root" :role "default"
                     :configuration configuration :activity 'idle
                     :conversation-location "agents/bad.chat.org")))
              (make-directory agents-dir t)
              (write-region
               "* Conversation Summary\nIndependent compacted history.\n\n* Agent Task: idle\nOld answer.\n"
               nil idle-file nil 'silent)
              (with-temp-buffer
                (org-mode)
                (insert "* Agent Task: active\n")
                (insert (propertize "Partial abandoned response.\n"
                                    'gptel 'response))
                (mevedel-session-persistence--stabilize-gptel-bounds)
                (write-region (point-min) (point-max)
                              active-file nil 'silent))
              (setf (mevedel-session-agent-turn-capacity session) 7
                    (mevedel-session-agent-transcripts session)
                    `(("opaque-idle" :agent-path "/root/idle"
                       :status completed :path ,idle-relative)
                      ("opaque-active" :agent-path "/root/active"
                       :status running :path ,active-relative))
                    (mevedel-session-agent-registry session)
                    (list (cons "/root/idle" idle)
                          (cons "/root/active" active)
                          (cons "/root/bad" bad))
                    (mevedel-session-messages session)
                    (list
                     (list :type 'MAIL :sender "/root/idle"
                           :recipient "/root" :payload "root mail"
                           :timestamp '(1 0 0 0))))
              (should
               (mevedel-agent-control-block-turn
                session "/root/active" 'permission-blocked))
              (with-current-buffer root
                (mevedel-session-persistence-save session root))
              (let* ((sidecar (file-name-concat session-dir
                                                 "session.meta.el"))
                     (persisted (mevedel-session-persistence-read sidecar))
                     (bad-entry
                      (cl-find-if
                       (lambda (entry)
                         (equal "/root/bad" (plist-get entry :path)))
                       (plist-get persisted :agent-registry))))
                (setf (plist-get bad-entry :conversation-location)
                      "../escape.chat.org")
                (mevedel-session-persistence-write sidecar persisted)))
            (test-mevedel-session-persistence--release-and-kill root session)
            (setq root nil)
            (let ((dispatches 0))
              (cl-letf (((symbol-function 'mevedel-agent-runtime-dispatch)
                         (lambda (&rest _)
                           (cl-incf dispatches)
                           (error "Restore replayed an abandoned request"))))
                (setq restored
                      (mevedel-session-persistence-restore session-dir)))
              (should (zerop dispatches)))
            (setq restored-session
                  (buffer-local-value 'mevedel--session restored))
            (should (= 7 (mevedel-session-agent-turn-capacity
                          restored-session)))
            (should
             (equal '("/root" "/root/active" "/root/idle")
                    (mapcar
                     (lambda (item) (plist-get item :path))
                     (mevedel-agent-control-list-agents restored-session))))
            (let* ((idle
                    (cdr (assoc "/root/idle"
                                (mevedel-session-agent-registry
                                 restored-session))))
                   (active
                    (cdr (assoc "/root/active"
                                (mevedel-session-agent-registry
                                 restored-session))))
                   (idle-buffer
                    (mevedel-agent-record-conversation-buffer idle))
                   (active-buffer
                    (mevedel-agent-record-conversation-buffer active)))
              (should (equal "opaque-idle" (mevedel-agent-record-id idle)))
              (should (eq 'idle (mevedel-agent-record-activity active)))
              (should (buffer-live-p idle-buffer))
              (should (buffer-live-p active-buffer))
              (should
               (eq 'aborted
                   (plist-get
                    (cdr (assoc
                          "opaque-active"
                          (mevedel-session-agent-transcripts
                           restored-session)))
                    :status)))
              (should
               (eq 'aborted
                   (mevedel-agent-invocation-transcript-status
                    (buffer-local-value 'mevedel--agent-invocation
                                        active-buffer))))
              (with-current-buffer idle-buffer
                (should (string-match-p "Independent compacted history"
                                        (buffer-string))))
              (should (equal "child mail"
                             (plist-get
                              (car (mevedel-agent-record-mailbox idle))
                              :payload))))
            (should (= 2 (length (mevedel-session-messages
                                  restored-session))))
            (should (= 1
                       (cl-count-if
                        (lambda (message)
                          (eq 'interrupted (plist-get message :outcome)))
                        (mevedel-session-messages restored-session))))
            (let* ((idle
                    (cdr (assoc "/root/idle"
                                (mevedel-session-agent-registry
                                 restored-session))))
                   (idle-buffer
                    (mevedel-agent-record-conversation-buffer idle))
                   (identity
                    (buffer-local-value 'mevedel--agent-invocation
                                        idle-buffer))
                   (root-data (list :messages (vector)))
                   (root-fsm
                    (gptel-make-fsm
                     :info (list :buffer restored :backend nil
                                 :data root-data)))
                   (idle-data (list :messages (vector)))
                   (idle-fsm
                    (gptel-make-fsm
                     :info (list :buffer idle-buffer :backend nil
                                 :data idle-data
                                 :mevedel-agent-invocation identity))))
              (require 'mevedel-tools)
              (mevedel-tools--handle-message-inject root-fsm)
              (mevedel-tools--handle-message-inject idle-fsm)
              (should-not (mevedel-session-messages restored-session))
              (should-not (mevedel-agent-record-mailbox idle))
              (should (= 2 (length (plist-get root-data :messages))))
              (should (= 1 (length (plist-get idle-data :messages))))
              (should
               (cl-find-if
                (lambda (message)
                  (string-match-p
                   "Partial abandoned response\\."
                   (plist-get message :content)))
                (append (plist-get root-data :messages) nil)))
              (should
               (string-match-p
                "child mail"
                (plist-get (aref (plist-get idle-data :messages) 0)
                           :content)))
              (mevedel-tools--handle-message-inject root-fsm)
              (mevedel-tools--handle-message-inject idle-fsm)
              (should (= 2 (length (plist-get root-data :messages))))
              (should (= 1 (length (plist-get idle-data :messages))))
              (with-current-buffer restored
                (should (= 1 (how-many "root mail" (point-min) (point-max))))
                (should (= 1 (how-many
                              "Agent turn was interrupted by session recovery"
                              (point-min) (point-max)))))
              (with-current-buffer idle-buffer
                (should (= 1 (how-many "child mail"
                                       (point-min) (point-max)))))
              (with-current-buffer restored
                (mevedel-session-persistence-save
                 restored-session restored)))
            ;; A second cold resume keeps the injected transcript history but
            ;; neither restores consumed mail nor enqueues another recovery
            ;; RESULT for the already-idle turn.
            (test-mevedel-session-persistence--release-and-kill
             restored restored-session)
            (setq restored nil restored-session nil)
            (setq restored
                  (mevedel-session-persistence-restore session-dir)
                  restored-session
                  (buffer-local-value 'mevedel--session restored))
            (should-not (mevedel-session-messages restored-session))
            (should
             (eq 'aborted
                 (plist-get
                  (cdr (assoc
                        "opaque-active"
                        (mevedel-session-agent-transcripts restored-session)))
                  :status)))
            (let* ((idle
                    (cdr (assoc "/root/idle"
                                (mevedel-session-agent-registry
                                 restored-session))))
                   (idle-buffer
                    (mevedel-agent-record-conversation-buffer idle))
                   captured-buffer)
              (should-not (mevedel-agent-record-mailbox idle))
              (with-current-buffer restored
                (should (= 1 (how-many "root mail" (point-min) (point-max))))
                (should (= 1 (how-many
                              "Agent turn was interrupted by session recovery"
                              (point-min) (point-max)))))
              (with-current-buffer idle-buffer
                (should (= 1 (how-many "child mail"
                                       (point-min) (point-max))))
                (should (string-match-p "Independent compacted history"
                                        (buffer-string))))
              (with-current-buffer restored
                (cl-letf
                    (((symbol-function 'mevedel-agent-runtime-dispatch)
                      (lambda (_agent _description _message &rest keys)
                        (setq captured-buffer
                              (plist-get keys :retained-buffer))
                        t)))
                  (mevedel-agent-control-followup
                   restored-session "/root/idle" "Continue after resume.")))
              (should (eq idle-buffer captured-buffer))
              (should (eq 'running (mevedel-agent-record-activity idle)))
              (setf (mevedel-agent-record-activity idle) 'idle
                    (mevedel-agent-record-invocation idle) nil)
              (let* ((saved
                      (mevedel-session-persistence-load-sidecar
                       (mevedel-session-persistence--sidecar-path
                        session-dir)))
                     (saved-idle
                      (cl-find "/root/idle"
                               (plist-get saved :agent-registry)
                               :key (lambda (entry)
                                      (plist-get entry :path))
                               :test #'equal)))
                (should-not (plist-get saved :messages))
                (should-not (plist-get saved-idle :mailbox)))))
        (test-mevedel-session-persistence--release-and-kill
         root session)
        (test-mevedel-session-persistence--release-and-kill
         restored restored-session)
        (when (file-directory-p tempdir) (delete-directory tempdir t))
        (mevedel-workspace-clear-registry)))))

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
                  (let ((identity
                         (mevedel-workspace-identity-read
                          (mevedel-workspace-root workspace))))
                    (should (string-match-p
                             "\\`[0-9a-f]\\{64\\}\\'" identity))
                    (let ((saved-workspace
                           (plist-get
                            (mevedel-session-persistence-serialize session)
                            :workspace)))
                      (should (equal identity
                                     (plist-get saved-workspace
                                                :workspace-id)))))
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
                (mevedel-telemetry-record
                 session 'test-lifecycle :outcome 'buffered)
                (with-current-buffer buf
                  (org-mode)
                  (mevedel-session-persistence-ensure-files session buf))
                (dolist (file '("hook-log.el" "repair-log.el"
                                "permission-log.el" "telemetry-log.el"))
                  (should
                   (file-readable-p
                    (file-name-concat
                     (mevedel-session-save-path session) file))))
                (should-not
                 (mevedel-session-permission-log-pending session))
                (should-not
                 (mevedel-session-telemetry-pending session))
                (should-not
                 (mevedel-session-hook-log-pending session))
                (should-not
                 (mevedel-session-repair-log-pending session)))
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
  :doc "assigns stable fork-point identity only to settled responses"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-fork-point-save*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (insert (propertize "First response\n" 'gptel 'response))
                (mevedel-session-persistence-save session buf)
                (should-not
                 (plist-get
                  (car (cdr (assoc 1
                                   (mevedel-session-prompt-index session))))
                  :fork-point-id))
                (mevedel-session-persistence-save session buf t)
                (let* ((entry
                        (car (cdr (assoc
                                   1
                                   (mevedel-session-prompt-index session)))))
                       (fork-point-id (plist-get entry :fork-point-id)))
                  (should (stringp fork-point-id))
                  (should-not (string-empty-p fork-point-id))
                  (should
                   (mevedel-transcript-audit-records
                    (buffer-string) 'fork-point))
                  (mevedel-session-persistence-save session buf t)
                  (should
                   (equal
                    fork-point-id
                    (plist-get
                     (car (cdr (assoc
                                1
                                (mevedel-session-prompt-index session))))
                     :fork-point-id)))
                  (let* ((sidecar
                          (mevedel-session-persistence-read
                           (mevedel-session-persistence--sidecar-path
                            (mevedel-session-save-path session))))
                         (persisted
                          (car (cdr (assoc
                                     1
                                     (plist-get sidecar :prompt-index))))))
                    (should
                     (equal fork-point-id
                            (plist-get persisted :fork-point-id))))
                  (erase-buffer)
                  (insert "First prompt\n")
                  (insert (propertize "First response\n" 'gptel 'response))
                  (mevedel-session-persistence-save session buf t)
                  (should-not
                   (equal
                    fork-point-id
                    (plist-get
                     (car (cdr (assoc
                                1
                                (mevedel-session-prompt-index session))))
                     :fork-point-id)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "leaves no Emacs backup or lock beside the segment"
  ;; A backup is one whole-segment copy over the connection and a lock is a
  ;; symlink per modify-and-save cycle, both answering questions the
  ;; publication and the lease already answer -- and both would sit in a
  ;; directory another client resumes from.
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-save-machinery*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (mevedel-session-persistence-save session buf)
                ;; The backup lands on the second save, once the segment it
                ;; would copy exists.
                (insert "Second prompt\n")
                (mevedel-session-persistence-save session buf)
                (should-not make-backup-files)
                (should-not create-lockfiles)
                (let ((entries (directory-files
                                (mevedel-session-save-path session))))
                  (should (member "segment-0001.chat.org" entries))
                  (should-not
                   (seq-find (lambda (entry)
                               (or (string-suffix-p "~" entry)
                                   (string-prefix-p ".#" entry)))
                             entries))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "advances updated-at across saves"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (mevedel-test--with-shifted-clock
                  (insert "First prompt\n")
                  (mevedel-session-persistence-save session buf)
                  (let ((first-updated (mevedel-session-updated-at session)))
                    (should first-updated)
                    ;; Advance the clock the stamps see instead of sleeping
                    ;; past a second boundary.
                    (setq mevedel-test--timestamp-offset 2)
                    (insert "Second prompt\n")
                    (mevedel-session-persistence-save session buf)
                    (should-not (equal first-updated
                                       (mevedel-session-updated-at session))))))
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
                  (mevedel-session-persistence-save session data-buf))
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
      (mevedel-workspace-clear-registry)))
  :doc "retries queued diagnostics after the next successful save"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer " *test-diagnostic-retry*"))
               (blocked (file-name-concat tempdir "blocked"))
               save-path)
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Persist diagnostics\n")
                (mevedel-session-persistence-save session buf)
                (setq save-path (mevedel-session-save-path session))
                (write-region "not a directory" nil blocked nil 'silent)
                (setf (mevedel-session-save-path session) blocked)
                (mevedel-hooks--log
                 session '(:event Stop :status completed))
                (mevedel-tool-repair-log-event
                 session
                 '(:time "now" :origin "/root" :backend backend
                         :model model :tool "Read" :outcome repaired
                         :repair-enabled t :rules (array-to-list)
                         :paths ((names)) :issue-kinds (wrong-shape)
                         :execution executed :result success))
                (should (mevedel-session-hook-log-pending session))
                (should (mevedel-session-repair-log-pending session))
                (setf (mevedel-session-save-path session) save-path)
                (mevedel-session-persistence-save session buf)
                (should-not (mevedel-session-hook-log-pending session))
                (should-not (mevedel-session-repair-log-pending session))
                (should
                 (file-readable-p (file-name-concat save-path "hook-log.el")))
                (should
                 (file-readable-p
                  (file-name-concat save-path "repair-log.el"))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--allow-emacs-exit-p ()
  ,test
  (test)
  :doc "pending publication vetoes exit until retry or explicit abandonment"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/tmp/pending-exit/"
                     :root "/tmp/pending-exit/" :name "pending-exit"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-pending-exit*"))
         (kill-emacs-query-functions
          '(mevedel-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (setf (mevedel-session-pending-publication session)
                '(:reason "target unavailable"))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (should-not
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))
            (setf (mevedel-session-pending-publication session) nil)
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "unsettled remote mutation vetoes exit until acknowledgement"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/tmp/unsettled-exit/"
                     :root "/tmp/unsettled-exit/"
                     :name "unsettled-exit"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-unsettled-exit*"))
         (kill-emacs-query-functions
          '(mevedel-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (setf (mevedel-session-lease session)
                '(:state owned :unsettled-mutation t))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer)))
                    ((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should-not
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions)))
          (setf (mevedel-session-lease session)
                '(:state owned :unsettled-mutation nil))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "a foreign read-only inspector can exit with a durable mutation latch"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/tmp/foreign-exit/"
                     :root "/tmp/foreign-exit/"
                     :name "foreign-exit"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-foreign-exit*"))
         (kill-emacs-query-functions
          '(mevedel-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session)
            (setq-local mevedel-session--read-only-mode t)
            (setq buffer-read-only t))
          (setf (mevedel-session-lease session)
                '(:state foreign :unsettled-mutation t))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer)))
                    ((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))
            (should
             (plist-get (mevedel-session-lease session)
                        :unsettled-mutation))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq buffer-read-only nil))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-session-persistence--kill-emacs-hook ()
  ,test
  (test)
  :doc "force-tears down executions before exit persistence"
  (let ((mevedel-workspace--registry nil)
        torn-down)
    (cl-letf (((symbol-function 'mevedel-execution-teardown-all)
               (lambda () (setq torn-down t)))
              ((symbol-function 'buffer-list)
               (lambda (&optional _frame) nil)))
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
      (mevedel-workspace-clear-registry)))
  :doc "cleans inactive expired sessions before releasing live locks"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (live-session (mevedel-session-create "live" workspace))
               (expired-session (mevedel-session-create "expired" workspace))
               (live-buf (generate-new-buffer " *test-live*"))
               (expired-buf (generate-new-buffer " *test-expired*")))
          (unwind-protect
              (progn
                (dolist (pair `((,live-session . ,live-buf)
                                (,expired-session . ,expired-buf)))
                  (with-current-buffer (cdr pair)
                    (org-mode)
                    (setq-local mevedel--session (car pair))
                    (setq-local mevedel--workspace workspace)
                    (insert "Old session\n")
                    (mevedel-session-persistence-save
                     (car pair) (cdr pair))
                    (set-buffer-modified-p nil)))
                (dolist (session (list live-session expired-session))
                  (test-mevedel-session-persistence--expire-session session))
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path expired-session)
                 expired-session)
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional _frame) (list live-buf))))
                  (mevedel-session-persistence--kill-emacs-hook))
                (should-not
                 (file-directory-p
                  (mevedel-session-save-path expired-session)))
                (should
                 (file-directory-p (mevedel-session-save-path live-session)))
                (should-not
                 (file-exists-p
                  (mevedel-session-persistence--lock-path
                   (mevedel-session-save-path live-session)))))
            (test-mevedel-session-persistence--release-and-kill
             live-buf live-session)
            (test-mevedel-session-persistence--release-and-kill
             expired-buf expired-session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "honors a cleanup run already throttled for the workspace"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "expired" workspace))
               (buf (generate-new-buffer " *test-expired*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old session\n")
                  (mevedel-session-persistence-save session buf))
                (let ((save-path (mevedel-session-save-path session)))
                  (test-mevedel-session-persistence--expire-session session)
                  (mevedel-session-persistence-lock-release save-path session)
                  (puthash
                   (cons (mevedel-workspace-type workspace)
                         (mevedel-workspace-id workspace))
                   t mevedel-session-persistence--cleanup-throttle)
                  (cl-letf (((symbol-function 'buffer-list)
                             (lambda (&optional _frame) nil)))
                    (mevedel-session-persistence--kill-emacs-hook))
                  (should (file-directory-p save-path))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "failed exit saves remain protected until cleanup finishes"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "live" workspace))
               (buf (generate-new-buffer " *test-live*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local mevedel--session session)
                  (insert "Unsaved exit change\n")
                  (mevedel-session-persistence-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path save-path)))
                  (test-mevedel-session-persistence--expire-session session)
                  (delete-file sidecar)
                  (make-directory sidecar)
                  (set-file-times
                   sidecar
                   (time-subtract (current-time) (* 14 24 60 60)))
                  (with-current-buffer buf
                    (set-buffer-modified-p t))
                  (cl-letf (((symbol-function 'buffer-list)
                             (lambda (&optional _frame) (list buf))))
                    (mevedel-session-persistence--kill-emacs-hook))
                  (should (file-directory-p save-path))
                  (should (file-directory-p sidecar))
                  (should-not
                   (file-exists-p
                    (mevedel-session-persistence--lock-path save-path)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "cleanup errors do not block sibling workspaces or lock release"
  (let* ((mevedel-session-max-age-days 7)
         (mevedel-session-persistence--cleanup-throttle
          (make-hash-table :test #'equal))
         (mevedel-workspace--registry (make-hash-table :test #'equal))
         (bad-root (file-name-as-directory
                    (make-temp-file "mevedel-test-bad-ws-" t)))
         (good-root (file-name-as-directory
                     (make-temp-file "mevedel-test-good-ws-" t)))
         (bad-workspace
          (mevedel-workspace-get-or-create
           'file "bad" bad-root "bad"))
         (good-workspace
          (mevedel-workspace-get-or-create
           'file "good" good-root "good"))
         (bad-sessions
          (mevedel-session-persistence--sessions-dir bad-workspace))
         (live-session (mevedel-session-create "live" good-workspace))
         (expired-session (mevedel-session-create "expired" good-workspace))
         (live-buf (generate-new-buffer " *test-live*"))
         (expired-buf (generate-new-buffer " *test-expired*")))
    (unwind-protect
        (progn
          (dolist (pair `((,live-session . ,live-buf)
                          (,expired-session . ,expired-buf)))
            (with-current-buffer (cdr pair)
              (org-mode)
              (setq-local mevedel--session (car pair))
              (insert "Session\n")
              (mevedel-session-persistence-save (car pair) (cdr pair))
              (set-buffer-modified-p nil)))
          (let ((save-path (mevedel-session-save-path expired-session)))
            (test-mevedel-session-persistence--expire-session expired-session)
            (mevedel-session-persistence-lock-release save-path expired-session))
          (make-directory bad-sessions t)
          (set-file-modes bad-sessions 0)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list live-buf))))
            (mevedel-session-persistence--kill-emacs-hook))
          (should-not
           (file-directory-p (mevedel-session-save-path expired-session)))
          (should-not
           (file-exists-p
            (mevedel-session-persistence--lock-path
             (mevedel-session-save-path live-session)))))
      (when (file-exists-p bad-sessions)
        (set-file-modes bad-sessions #o700))
      (test-mevedel-session-persistence--release-and-kill
       live-buf live-session)
      (test-mevedel-session-persistence--release-and-kill
       expired-buf expired-session)
      (delete-directory bad-root t)
      (delete-directory good-root t))))

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
                   (directive
                    (car (plist-get save-file :directives)))
                   (request (plist-get directive :request)))
              (should (equal "Fix beta" request))
              (should-not (text-properties-at 0 request)))
            (with-current-buffer data-buf
              (mevedel--clear-instruction-state workspace)
              (mevedel-session-persistence--load-instructions session data-buf))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ov (car (alist-get source-buf (mevedel--instruction-alist))))
                   (request (mevedel--directive-text ov)))
              (should (equal "Fix beta" request))
              (should-not (text-properties-at 0 request))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "loads historical presentations without replacing current directive records"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "EARLY\nLATER\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "EARLY")
              (mevedel--create-directive-in
               source-buf (match-beginning 0) (match-end 0) nil "Early"))
            (setq session (mevedel-session-create "main" workspace)
                  data-buf (generate-new-buffer "*test-data-buf*"))
            (setf (mevedel-session-turn-count session) 1)
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "First turn\n")
              (mevedel-session-persistence-save session data-buf))
            (let ((early (car (mevedel-workspace-directives workspace))))
              (setf (mevedel-directive-attempts early)
                    (list
                     (mevedel-directive-attempt--create
                      :directive-request "Early" :request "Early prompt"
                      :result "Done" :outcome 'success :patch ""
                      :capture 'complete :captured-at "2026-08-02T03:00:00+0200"
                      :checkpoint '(:session-id "session" :turn 1))))
              (mevedel-directive-set-request early "Current early edit")
              (with-current-buffer source-buf
                (goto-char (point-min))
                (search-forward "LATER")
                (mevedel--create-directive-in
                 source-buf (match-beginning 0) (match-end 0) nil "Later"))
              (let* ((later (car (mevedel-workspace-directives workspace)))
                     (records (copy-sequence
                               (mevedel-workspace-directives workspace))))
                (setf (mevedel-directive-attempts later)
                      (list
                       (mevedel-directive-attempt--create
                        :directive-request "Later" :outcome 'success
                        :checkpoint '(:session-id "session" :turn 2))))
                (with-current-buffer data-buf
                  (mevedel-session-persistence--load-instructions
                   session data-buf 1 records))
                (should (equal records
                               (mevedel-workspace-directives workspace)))
                (should (equal "Current early edit"
                               (mevedel-directive-request early)))
                (should (= 1 (length (mevedel-directive-attempts early))))
                (should (= 1 (length (mevedel-directive-attempts later))))
                (dolist (record records)
                  (should
                   (eq record
                       (mevedel--directive-record
                        (mevedel--instruction-with-uuid
                         (mevedel-directive-id record) workspace))))))))
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
        (mevedel-workspace-clear-registry))))
  :doc "loads remote instruction bytes from the immutable publication"
  (let* ((host "instruction-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-instruction-publication-" t)))
         data-buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((client-id (make-string 64 ?a))
                   (mevedel-session-durability--client-id client-id)
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (instruction-path
                    (mevedel-session-persistence--instructions-current-path
                     session-dir))
                   (published "(:published instruction snapshot)\n")
                   observed-source
                   observed-content
                   observed-base)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (unwind-protect
                  (progn
                    (should
                     (mevedel-session-durability-lease-acquire
                      session-dir "*instruction-publisher*" session))
                    (let* ((publication
                            (mevedel-session-publication-read
                             session-dir))
                           (sidecar
                            (mevedel-session-persistence-load-sidecar
                             (plist-get publication :sidecar))))
                      (mevedel-session-publication-publish
                       session
                       (list
                        (list :path instruction-path :content published)
                        (list
                         :path
                         (mevedel-session-persistence--sidecar-path session-dir)
                         :content
                         (mevedel-session-persistence--printed-value sidecar)
                         :commit-marker t))))
                    (mevedel-session-durability-lease-release
                     session-dir session)
                    (make-directory (file-name-directory instruction-path) t)
                    (write-region "poisoned fixed cache\n" nil
                                  instruction-path nil 'silent)
                    (setq data-buffer
                          (generate-new-buffer " *remote instructions*"))
                    (cl-letf
                        (((symbol-function 'mevedel--load-instructions-file)
                          (lambda (source base &rest _)
                            (setq observed-source source
                                  observed-base base
                                  observed-content
                                  (mevedel-file-history--read-file-raw source))
                            t)))
                      (should
                       (mevedel-session-persistence--load-instructions
                        session data-buffer)))
                    (should-not (file-remote-p observed-source))
                    (should-not (equal observed-source instruction-path))
                    (should (equal published observed-content))
                    (should
                     (equal (mevedel-workspace-root workspace)
                            observed-base)))
                (when (mevedel-session-durability-lease-owned-p session)
                  (mevedel-session-durability-lease-release
                   session-dir session))))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

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
  :doc "rebases a live view when gptel metadata shifts transcript positions"
  (let ((view (generate-new-buffer " *mevedel-save-state-view*"))
        deltas)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session t)
          (setq-local mevedel--view-buffer view)
          (let ((data (current-buffer)))
            (with-current-buffer view
              (setq-local mevedel--data-buffer data))
            (require 'mevedel-session-control-transfer)
            (let ((observer
                   (lambda (event &rest args)
                     (when (eq event 'rebase-data-sources)
                       (apply #'mevedel-view--rebase-data-sources args)))))
              (mevedel-session-control-transfer-register-observer
               t observer)
              (unwind-protect
                  (progn
                    (insert "Transcript body\n")
                    (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                               (lambda () nil))
                              ((symbol-function
                                'mevedel-view--rebase-data-sources)
                               (lambda (delta) (push delta deltas))))
                      (mevedel-session-persistence--save-gptel-state-around
                       (lambda ()
                         (org-entry-put (point-min) "GPTEL_MODEL"
                                        "fake-model"))))
                    (should (= 1 (length deltas)))
                    (should-not (= 0 (car deltas))))
                (mevedel-session-control-transfer-unregister-observer
                 t observer)))))
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
  :doc "discard reverts affected modified buffers before restore"
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
            (should-not (buffer-modified-p))
            (should (equal "old\n" (buffer-string)))))
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
  :doc "writes a pre-turn checkpoint for a modified file"
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
            (should (= 2 (length written)))
            (let* ((entry (assoc tracked-file
                                 (cdr (assoc 1 (mevedel-session-file-snapshots
                                                session)))))
                   (backup-name (plist-get (cdr entry) :pre-backup-name))
                   (backup-path (mevedel-file-history--backup-path
                                 (mevedel-session-save-path session)
                                 backup-name)))
              (should backup-name)
              (should (file-exists-p backup-path))
              (with-temp-buffer
                (insert-file-contents-literally backup-path)
                (should (equal "old content" (buffer-string)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records an empty checkpoint when tracked files are unchanged"
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
            (should (assoc 1 (mevedel-session-file-snapshots session)))
            (should-not
             (cdr (assoc 1 (mevedel-session-file-snapshots session))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records prior content when a file is deleted during the turn"
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
            (should (plist-get (cdr entry) :pre-backup-name))
            (should-not (plist-get (cdr entry) :backup-name))
            (should
             (equal
              "had content"
              (mevedel-session-persistence--file-text
               (mevedel-file-history--backup-path
                (mevedel-session-save-path session)
                (plist-get (cdr entry) :pre-backup-name)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records an absent checkpoint for a file created during the turn"
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
            (should (plist-get (cdr entry) :backup-name))
            (should-not (plist-get (cdr entry) :pre-backup-name))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records files exceeding the size cap as checkpoint gaps"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "huge.el"))
               (pre          (make-hash-table :test #'equal))
               (mevedel-file-history-max-snapshot-bytes 10))
          (puthash tracked-file (make-string 100 ?x) pre)
          (write-region "changed" nil tracked-file nil 'silent)
          (let ((written (mevedel-file-history-snapshot-modified
                          session 1 pre)))
            (should (null written))
            (should
             (string-match-p
              "exceeds"
              (plist-get
               (cdr
                (assoc tracked-file
                       (cdr (assoc
                             1
                             (mevedel-session-file-snapshots session)))))
               :gap)))))
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
            (should (file-equal-p new-path buffer-file-name))
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
  :doc "noninteractive publication:
rotation never saves through a rebound temporary visited filename or prompts"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (original-save-buffer (symbol-function 'save-buffer))
               new-path)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "unsaved transcript\n"))
          (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
                     (lambda (&rest _) (error "Supersession prompt")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (error "yes-or-no prompt")))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "y-or-n prompt")))
                    ((symbol-function 'save-buffer)
                     (lambda (&rest args)
                       (when (and buffer-file-name
                                  (string-suffix-p ".tmp" buffer-file-name))
                         (error "Temporary visited-file save"))
                       (apply original-save-buffer args))))
            (setq new-path
                  (mevedel-session-persistence-rotate-segment
                   session buf "Noninteractive summary.")))
          (with-current-buffer buf
            (should (file-equal-p new-path buffer-file-name))
            (should (equal (file-truename new-path) buffer-file-truename))
            (should (verify-visited-file-modtime buf))
            (should-not (buffer-modified-p))))
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
            (should (file-equal-p new-path buffer-file-name))
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
            (should (file-equal-p old-segment buffer-file-name))
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
            (should
             (file-equal-p
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
            (should (file-equal-p old-segment buffer-file-name))
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
            (should (equal mevedel-session-persistence-format-version
                           (plist-get plist :version)))
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
  :doc "read-only inspection trusts published state and skips lifecycle commands"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let (restored)
      (unwind-protect
          (let* ((session (mevedel-session-create "main" workspace))
                 (buffer (generate-new-buffer "*test-published-segment*"))
                 session-dir segment-1 segment-2 probed started)
            (with-current-buffer buffer
              (org-mode)
              (insert "Published transcript\n")
              (mevedel-session-persistence-save session buffer))
            (setq session-dir (mevedel-session-save-path session)
                  segment-1 (mevedel-session-persistence--segment-path
                             session-dir 1)
                  segment-2 (mevedel-session-persistence--segment-path
                             session-dir 2))
            (test-mevedel-session-persistence--release-and-kill
             buffer session)
            (write-region "Unpublished transcript\n" nil segment-2 nil 'silent)
            (cl-letf (((symbol-function
                        'mevedel-session-persistence-lock-acquire)
                       (lambda (&rest _) nil))
                      ((symbol-function 'mevedel--probe-session-target)
                       (lambda (&rest _) (setq probed t)))
                      ((symbol-function 'mevedel--run-session-start-hooks)
                       (lambda (&rest _) (setq started t))))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (with-current-buffer restored
              (should mevedel-session--read-only-mode)
              (should (= 1 (mevedel-session-current-segment
                            mevedel--session)))
              (should (string-match-p "Published transcript"
                                      (buffer-string)))
              (should-not (memq #'mevedel--run-session-end-hooks
                                kill-buffer-hook)))
            (should probed)
            (should-not started)
            (with-temp-buffer
              (insert-file-contents segment-1)
              (should-not
               (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                               (buffer-string))))
            (with-temp-buffer
              (insert-file-contents segment-2)
              (should (equal "Unpublished transcript\n" (buffer-string)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "an actual foreign lease permits inspection without hooks or repair writes"
  (let* ((host "restore-foreign-owner-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-foreign-restore-" t)))
         (owner-id (make-string 64 ?a))
         (inspector-id (make-string 64 ?b))
         (running
          (mevedel-pipeline--format-render-data-block
           '(:execution-id "foreign-stale" :state running
             :status success :live-execution-p t)))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir segment-1)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root (concat "* Chat\n" running))
            (let* ((segment-2
                    (mevedel-session-persistence--segment-path
                     session-dir 2))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   probed
                   started)
              (write-region
               "Unpublished transcript\n" nil segment-2 nil 'silent)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target owner))
               t mevedel-session-durability--disclosed-targets)
              (let ((mevedel-session-durability--client-id owner-id))
                (should
                 (mevedel-session-durability-lease-acquire
                  session-dir "*owner*" owner))
                (when-let ((timer
                            (mevedel-session-lease-renewal-timer owner)))
                  (cancel-timer timer)
                  (setf (mevedel-session-lease-renewal-timer owner) nil)))
              (cl-labels
                  ((snapshot ()
                     (mapcar
                      (lambda (path)
                        (cons
                         (file-relative-name path local-root)
                         (if (file-directory-p path)
                             :directory
                           (with-temp-buffer
                             (insert-file-contents-literally path)
                             (buffer-string)))))
                      (sort
                       (directory-files-recursively local-root ".*" t)
                       #'string<))))
                (let ((before (snapshot))
                      (mevedel-session-durability--client-id inspector-id))
                  (unwind-protect
                      (progn
                        (cl-letf
                            (((symbol-function 'mevedel--probe-session-target)
                              (lambda (&rest _) (setq probed t)))
                             ((symbol-function
                               'mevedel--run-session-start-hooks)
                              (lambda (&rest _) (setq started t)))
                             ((symbol-function
                               'mevedel-session-persistence--self-heal-segment-counter)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore self-healed")))
                             ((symbol-function
                               'mevedel-session-persistence--reconcile-lost-execution-segments)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore repaired")))
                             ((symbol-function
                               'mevedel-session-persistence--maybe-prune-orphan)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore pruned")))
                             ((symbol-function
                               'mevedel-session-publication-publish)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore published"))))
                          (setq restored
                                (mevedel-session-persistence-restore
                                 session-dir nil nil workspace)))
                        (with-current-buffer restored
                          (should mevedel-session--read-only-mode)
                          (should
                           (eq 'foreign
                               (plist-get
                                (mevedel-session-lease mevedel--session)
                                :state)))
                          (should (= 1 (mevedel-session-current-segment
                                        mevedel--session)))
                          (should
                           (string-match-p ":state running"
                                           (buffer-string)))
                          (should-not
                           (memq #'mevedel--run-session-end-hooks
                                 kill-buffer-hook)))
                        (should probed)
                        (should-not started)
                        (should (equal before (snapshot)))
                        (with-temp-buffer
                          (insert-file-contents segment-1)
                          (should
                           (string-match-p ":state running"
                                           (buffer-string)))
                          (should-not
                           (string-match-p ":state lost"
                                           (buffer-string))))
                        (with-temp-buffer
                          (insert-file-contents segment-2)
                          (should
                           (equal "Unpublished transcript\n"
                                  (buffer-string)))))
                    (test-mevedel-session-persistence--release-and-kill
                     restored
                     (and restored
                          (buffer-local-value 'mevedel--session restored)))
                    (setq restored nil))))
              (let ((mevedel-session-durability--client-id owner-id))
                (mevedel-session-durability-lease-release
                 session-dir owner)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "foreign inspection ignores a missing fixed segment cache"
  (let* ((host "restore-foreign-missing-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-foreign-missing-" t)))
         (owner-id (make-string 64 ?a))
         (inspector-id (make-string 64 ?b)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (delete-file segment)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target owner))
               t mevedel-session-durability--disclosed-targets)
              (let ((mevedel-session-durability--client-id owner-id))
                (should
                 (mevedel-session-durability-lease-acquire
                  session-dir "*owner*" owner))
                (when-let ((timer
                            (mevedel-session-lease-renewal-timer owner)))
                  (cancel-timer timer)
                  (setf (mevedel-session-lease-renewal-timer owner) nil)))
              (let ((before (directory-files-recursively local-root ".*" t))
                    (mevedel-session-durability--client-id inspector-id)
                    restored)
                (unwind-protect
                    (progn
                      (cl-letf
                          (((symbol-function 'yes-or-no-p)
                            (lambda (&rest _)
                              (ert-fail "Foreign restore prompted to prune")))
                           ((symbol-function
                             'mevedel-session-persistence--maybe-prune-orphan)
                            (lambda (&rest _)
                              (ert-fail "Foreign restore pruned"))))
                        (setq restored
                              (mevedel-session-persistence-restore
                               session-dir nil nil workspace)))
                      (with-current-buffer restored
                        (should mevedel-session--read-only-mode)
                        (should (string-match-p
                                 "Published transcript" (buffer-string))))
                      (should (file-directory-p session-dir))
                      (should
                       (equal before
                              (directory-files-recursively
                               local-root ".*" t))))
                  (test-mevedel-session-persistence--release-and-kill
                   restored
                   (and restored
                        (buffer-local-value 'mevedel--session restored)))))
              (let ((mevedel-session-durability--client-id owner-id))
                (mevedel-session-durability-lease-release
                 session-dir owner)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "retries cleanly when the publication head changes during lease acquisition"
  (let* ((host "restore-sidecar-race-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-sidecar-race-" t)))
         restored
         acquired-session)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'mevedel-session-persistence-lock-acquire))
                   changed)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target fixture-session))
               t mevedel-session-durability--disclosed-targets)
              (cl-letf
                  (((symbol-function
                     'mevedel-session-persistence-lock-acquire)
                    (lambda (&rest arguments)
                      ;; Another owner commits a new immutable head after our
                      ;; initial capture but before this lease claim.
                      (unless changed
                        (setq changed t)
                        (let ((mevedel-session-durability--client-id
                               (make-string 64 ?d)))
                          (unwind-protect
                              (progn
                                (should
                                 (mevedel-session-durability-lease-acquire
                                  session-dir "*intervening-owner*"
                                  fixture-session))
                                (let* ((publication
                                        (mevedel-session-publication-read
                                         session-dir))
                                       (new-sidecar
                                        (mevedel-session-persistence-read
                                         (plist-get publication :sidecar))))
                                  (plist-put
                                   new-sidecar :total-turn-count 17)
                                  (mevedel-session-publication-publish
                                   fixture-session
                                   (list
                                    (list
                                     :path
                                     (mevedel-session-persistence--sidecar-path
                                      session-dir)
                                     :content
                                     (mevedel-session-persistence--printed-value
                                      new-sidecar)
                                     :commit-marker t)))))
                            (mevedel-session-durability-lease-release
                             session-dir fixture-session))))
                      (setq acquired-session (nth 2 arguments))
                      (apply acquire-function arguments)))
                   ((symbol-function 'mevedel--probe-session-target)
                    #'ignore)
                   ((symbol-function 'mevedel--chat-buffer-init-common)
                    #'ignore)
                   ((symbol-function
                     'mevedel-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'mevedel-session-persistence--load-instructions)
                    #'ignore))
                (let ((err
                       (should-error
                        (setq restored
                              (mevedel-session-persistence-restore
                               session-dir nil nil workspace))
                        :type 'user-error)))
                  (should
                   (string-match-p
                    "changed while acquiring"
                    (error-message-string err)))))
              (should acquired-session)
              (should-not (mevedel-session-lease acquired-session))
              (should-not
               (mevedel-session-lease-renewal-timer acquired-session))
              (should
               (eq 'released
                   (plist-get
                    (mevedel-session-durability--lease-head
                     (mevedel-session-durability--lease-path session-dir))
                    :status)))
              (should
               (= 17
                  (plist-get
                   (mevedel-session-persistence-read
                    (plist-get
                     (mevedel-session-publication-read session-dir)
                     :sidecar))
                   :total-turn-count))))))
      (mevedel-test--with-local-shell-tramp (list host)
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "loads immutable sidecar and transcript bytes over poisoned fixed caches"
  (let* ((host "restore-sidecar-cache-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-sidecar-cache-" t)))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target fixture-session))
               t mevedel-session-durability--disclosed-targets)
              (write-region "(:poisoned fixed sidecar)" nil
                            (mevedel-session-persistence--sidecar-path
                             session-dir)
                            nil 'silent)
              (write-region "Poisoned fixed transcript\n" nil segment nil 'silent)
              (cl-letf
                  (((symbol-function 'mevedel--probe-session-target) #'ignore)
                   ((symbol-function 'mevedel--chat-buffer-init-common) #'ignore)
                   ((symbol-function 'mevedel-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'mevedel-session-persistence--load-instructions)
                    #'ignore))
                (setq restored
                      (mevedel-session-persistence-restore
                       session-dir nil nil workspace)))
              (with-current-buffer restored
                (should (equal "main" (mevedel-session-name mevedel--session)))
                (should (string-match-p
                         "Published transcript" (buffer-string)))
                (should-not
                 (string-match-p "Poisoned fixed" (buffer-string)))
                (should (mevedel-session-publication mevedel--session))))))
      (mevedel-test--with-local-shell-tramp (list host)
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "fails closed when an immutable transcript digest no longer matches"
  (let* ((host "restore-digest-corruption")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-restore-digest-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (publication
                    (mevedel-session-publication-read session-dir))
                   (entry
                    (cdr (assoc "segment-0001.chat.org"
                                (plist-get publication :artifacts)))))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target fixture-session))
               t mevedel-session-durability--disclosed-targets)
              (write-region "corrupt" nil (plist-get entry :published)
                            nil 'silent)
              (let ((err
                     (should-error
                      (mevedel-session-persistence-restore
                       session-dir nil nil workspace))))
                (should
                 (string-match-p "failed verification"
                                 (error-message-string err))))
              (should-not
               (mevedel-session-persistence--find-live-buffer
                "main-remote-restore" "*unused*"))
              (should
               (eq 'released
                   (plist-get
                    (mevedel-session-durability--lease-head
                     (mevedel-session-durability--lease-path session-dir))
                    :status))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "releases its lease when restore fails before opening a buffer"
  (let* ((host "restore-pre-buffer-failure-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-pre-buffer-failure-" t)))
         acquired-session
         session-dir)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session fixture-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (setq session-dir fixture-dir)
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'mevedel-session-persistence-lock-acquire))
                   artifact-open-attempted-p)
              (unwind-protect
                  (progn
                    (puthash
                     (mevedel-execution-target-identity
                      (mevedel-session-execution-target fixture-session))
                     t mevedel-session-durability--disclosed-targets)
                    (cl-letf
                        (((symbol-function
                           'mevedel-session-persistence-lock-acquire)
                          (lambda (&rest arguments)
                            (setq acquired-session (nth 2 arguments))
                            (apply acquire-function arguments)))
                         ((symbol-function
                           'mevedel-session-persistence-find-artifact-noselect)
                          (lambda (&rest _)
                            (setq artifact-open-attempted-p t)
                            (error "Injected pre-buffer restore failure"))))
                      (should-error
                       (mevedel-session-persistence-restore
                        session-dir nil nil workspace)
                       :type 'error))
                    (should acquired-session)
                    (should artifact-open-attempted-p)
                    (should-not
                     (mevedel-session-persistence--find-live-buffer
                      "main-remote-restore" "*unused*"))
                    (should-not (mevedel-session-lease acquired-session))
                    (should-not
                     (mevedel-session-lease-renewal-timer acquired-session))
                    (should
                     (eq 'released
                         (plist-get
                          (mevedel-session-durability--lease-head
                           (mevedel-session-durability--lease-path session-dir))
                          :status))))
                (when acquired-session
                  (mevedel-session-durability-lease-release
                   session-dir acquired-session))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "remote repair publishes transcript and sidecar in one ordered batch"
  (let* ((host "restore-publication-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-restore-" t)))
         (running
          (mevedel-pipeline--format-render-data-block
           '(:execution-id "remote-stale" :state running
             :status success :live-execution-p t)))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root (concat "* Chat\n" running))
            (let ((mevedel-session-durability--client-id
                   (make-string 64 ?a))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal))
                  publications)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (unwind-protect
                  (progn
                    (cl-letf
                        (((symbol-function 'mevedel--probe-session-target)
                          #'ignore)
                         ((symbol-function 'mevedel--chat-buffer-init-common)
                          #'ignore)
                         ((symbol-function
                           'mevedel-agent-persistence-restore-tree)
                          (lambda (&rest _) 1))
                         ((symbol-function
                           'mevedel-session-persistence--load-instructions)
                          #'ignore)
                         ((symbol-function
                           'mevedel-session-publication-publish)
                          (lambda (_session artifacts)
                            (push artifacts publications)
                            t)))
                      (setq restored
                            (mevedel-session-persistence-restore
                             session-dir nil nil workspace)))
                    (should (= 1 (length publications)))
                    (let* ((artifacts (car publications))
                           (transcript (car artifacts))
                           (commit (car (last artifacts))))
                      (should (equal segment (plist-get transcript :path)))
                      (should (string-match-p
                               ":state lost" (plist-get transcript :content)))
                      (should-not
                       (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                       (plist-get transcript :content)))
                      (should
                       (equal
                        (mevedel-session-persistence--sidecar-path session-dir)
                        (plist-get commit :path)))
                      (should
                       (string-match-p
                        ":current-segment 1"
                        (plist-get commit :content)))
                      (should (eq t (plist-get commit :commit-marker))))
                    ;; The publisher was replaced with a spy, so a direct
                    ;; repair or finalization write would change target state.
                    (with-temp-buffer
                      (insert-file-contents segment)
                      (should
                       (string-match-p ":state running" (buffer-string)))
                      (should-not
                       (string-match-p ":state lost" (buffer-string)))
                      (should-not
                       (string-match-p
                        "MEVEDEL_SEGMENT_FINALIZED_AT"
                        (buffer-string)))))
                (when restored
                  (test-mevedel-session-persistence--release-and-kill
                   restored
                   (buffer-local-value 'mevedel--session restored)))
                (setq restored nil)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
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
  :doc "keeps a removed Worktree Fork discoverable and preserves its origin"
  (cl-destructuring-bind
      (workspace tempdir missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (let* ((sidecar-path
                    (mevedel-session-persistence--sidecar-path session-dir))
                   (sidecar
                    (mevedel-session-persistence-read sidecar-path)))
              (plist-put sidecar :fork-type 'worktree)
              (plist-put sidecar :forked-from-session-id "source-id")
              (plist-put sidecar :worktree-source-root tempdir)
              (plist-put sidecar :worktree-directory missing-dir)
              (plist-put sidecar :worktree-branch "worktree/main-fork-1")
              (plist-put sidecar :worktree-base-commit "abc123")
              (mevedel-session-persistence-write sidecar-path sidecar))
            (let* ((entry
                    (car
                     (mevedel-session-persistence-list-sessions workspace)))
                   (display
                    (mevedel-session-persistence--format-session-candidate
                     entry)))
              (should entry)
              (should (string-match-p "Worktree Fork" display))
              (should (string-match-p "missing" display)))
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) replacement-dir)))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (should-not (file-exists-p missing-dir))
            (with-current-buffer restored
              (should (eq 'worktree
                          (mevedel-session-fork-type mevedel--session)))
              (should (equal missing-dir
                             (mevedel-session-worktree-directory
                              mevedel--session)))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let* ((summary
                    (mevedel-session-persistence--read-summary
                     (mevedel-session-persistence--sidecar-path
                      session-dir)))
                   (display
                    (mevedel-session-persistence--format-session-candidate
                     (list :summary summary))))
              (should (equal missing-dir
                             (plist-get summary :worktree-directory)))
              (should (equal replacement-dir
                             (plist-get summary :working-directory)))
              (should (string-match-p "retargeted" display))
              (should (string-match-p
                       (regexp-quote replacement-dir) display))))
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
  :doc "preserves operation, network, and resource authority across resume"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (setf (mevedel-session-permission-rules session)
                      '(("Read" :path "/tmp/foo/**" :action allow)
                        ("Bash" :pattern "npx test*"
                         :network t
                         :file-system
                         ((:path "/tmp/external-input" :access read))
                         :action allow)))
                (setf (mevedel-session-resource-grants session)
                      '((:path "/tmp/external-input" :access read)))
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
                  (should
                   (equal
                    '(("Read" :path "/tmp/foo/**" :action allow)
                      ("Bash" :pattern "npx test*"
                       :network t
                       :file-system
                       ((:path "/tmp/external-input" :access read))
                       :action allow))
                    (mevedel-session-permission-rules
                     mevedel--session)))
                  (should
                   (equal
                    '((:path "/tmp/external-input" :access read))
                    (mevedel-session-resource-grants
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
         (new-root (file-name-as-directory
                    (file-name-concat old-root "packages" "api")))
         (old-cwd (file-name-as-directory
                   (file-name-concat old-root "src")))
         (expected-cwd (file-name-as-directory
                        (file-name-concat new-root "src")))
         buf session session-dir restored opened-workspace)
    (unwind-protect
        (progn
          (make-directory old-cwd t)
          (make-directory expected-cwd t)
          (mevedel-workspace-clear-registry)
          (let ((workspace (mevedel-workspace-get-or-create
                            'project old-root old-root "nested-proj")))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-working-directory session) old-cwd))
          (setq buf (generate-new-buffer "*test-data-buf*"))
          (with-current-buffer buf
            (org-mode)
            (setq-local mevedel--session session)
            (insert "Nested relocation\n")
            (mevedel-session-persistence-save session buf))
          (setq session-dir (mevedel-session-save-path session))
          (test-mevedel-session-persistence--release-and-kill
           buf session)
          (setq buf nil)
          (make-directory (file-name-concat new-root ".mevedel") t)
          (copy-file
           (file-name-concat old-root ".mevedel" "workspace-id")
           (file-name-concat new-root ".mevedel" "workspace-id"))
          (mevedel-workspace-clear-registry)
          (setq opened-workspace
                (mevedel-workspace-get-or-create
                 'project new-root new-root "nested-proj"))
          (setq restored (mevedel-session-persistence-restore
                          session-dir nil nil opened-workspace))
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
      (mevedel-workspace-clear-registry)))
  :doc "cold-restores the durable tree, mailboxes, recovery, and follow-up"
  (test-mevedel-session-persistence--cold-agent-tree-round-trip))

(mevedel-deftest mevedel-session-persistence-restore/unsettled-mutation ()
  ,test
  (test)
  :doc "restore inherits an unsettled mutation and acknowledgement clears it"
  (let* ((host "restore-unsettled-mutation")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-restore-unsettled-" t)))
         (owner-id (make-string 64 ?a))
         (successor-id (make-string 64 ?b))
         (observer-id (make-string 64 ?c))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target owner))
               t mevedel-session-durability--disclosed-targets)
              (let ((mevedel-session-durability--client-id owner-id))
                (should
                 (mevedel-session-durability-lease-acquire
                  session-dir "*previous-owner*" owner))
                (should
                 (mevedel-session-durability-set-unsettled-mutation
                  owner t))
                (mevedel-session-durability-lease-release
                 session-dir owner))
              (let ((mevedel-session-durability--client-id successor-id))
                (cl-letf
                    (((symbol-function 'mevedel--probe-session-target)
                      #'ignore)
                     ((symbol-function 'mevedel--run-session-start-hooks)
                      #'ignore))
                  (setq restored
                        (mevedel-session-persistence-restore
                         session-dir nil nil workspace)))
                (with-current-buffer restored
                  (require 'mevedel-execution)
                  (should
                   (mevedel-execution-mutation-blocked-p mevedel--session))
                  (mevedel-execution-acknowledge-unknown mevedel--session)
                  (should-not
                   (mevedel-execution-mutation-blocked-p mevedel--session))
                  (should-not
                   (mevedel-session-durability-unsettled-mutation-p
                    mevedel--session))
                  (mevedel-session-durability-lease-release
                   session-dir mevedel--session)
                  (let ((mevedel-session-durability--client-id observer-id))
                    (should
                     (mevedel-session-durability-lease-acquire
                      session-dir "*observer*" mevedel--session))
                    (should-not
                     (mevedel-session-durability-unsettled-mutation-p
                      mevedel--session))
                    (mevedel-session-durability-lease-release
                     session-dir mevedel--session)))))))
      (mevedel-test--with-local-shell-tramp (list host)
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
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
                   tempdir "*test-buf*"
                   (test-mevedel-session-persistence--pid-lock-context)))
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
           (mevedel-session-persistence-lock-acquire
            tempdir "*test-buf*"
            (test-mevedel-session-persistence--pid-lock-context))
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
                     tempdir "*test-buf*"
                     (test-mevedel-session-persistence--pid-lock-context))))
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
                           tempdir "*test-buf*"
                           (test-mevedel-session-persistence--pid-lock-context)))))
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
              tempdir "*test-buf*"
              (test-mevedel-session-persistence--pid-lock-context))
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
                     tempdir "*new-buf*"
                     (test-mevedel-session-persistence--pid-lock-context))))
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
                     tempdir "*new-buf*"
                     (test-mevedel-session-persistence--pid-lock-context))))
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
              tempdir "*new-buf*"
              (test-mevedel-session-persistence--pid-lock-context))
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
                           tempdir "*test-buf*"
                           (test-mevedel-session-persistence--pid-lock-context)))))
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
          (mevedel-session-persistence-lock-acquire
           tempdir "*x*"
           (test-mevedel-session-persistence--pid-lock-context))
          (should (file-exists-p lock-path))
          (mevedel-session-persistence-lock-release
           tempdir (test-mevedel-session-persistence--pid-lock-context))
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
          (mevedel-session-persistence-lock-release
           tempdir (test-mevedel-session-persistence--pid-lock-context))
          ;; Lock still present.
          (should (file-exists-p lock-path)))
      (delete-directory tempdir t)))
  :doc "is a no-op when no lock exists"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (progn
          ;; Should not error.
          (mevedel-session-persistence-lock-release
           tempdir (test-mevedel-session-persistence--pid-lock-context))
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

(mevedel-deftest mevedel-session-persistence-project-materialization ()
  ,test
  (test)
  :doc "materializes local project authority as lease and publication without a lock"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-local-project-authority-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (should
           (mevedel-session-persistence-assert-mutation-authority
            session (current-buffer)))
          (let ((save-path (mevedel-session-save-path session)))
            (should save-path)
            (should (file-directory-p
                     (file-name-concat save-path ".lease")))
            (should-not (file-exists-p
                         (file-name-concat save-path ".lock")))
            (should
             (mevedel-session-publication-publish
              session
              (list
               (list :path (file-name-concat save-path "session.meta.el")
                     :content "(:version \"v0.5.2\")"
                     :commit-marker t))))
            (should (mevedel-session-publication-read save-path))
            (let ((other-client (make-string 64 ?b))
                  (other (mevedel-session-create "main" workspace)))
              (setf (mevedel-session-save-path other) save-path)
              (let ((mevedel-session-durability--client-id other-client))
                (with-temp-buffer
                  (setq-local mevedel--session other)
                  (should-error
                   (mevedel-session-persistence-assert-mutation-authority
                    other (current-buffer))
                   :type 'user-error))))
            (mevedel-session-persistence-lock-release save-path session)))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-assert-mutation-authority ()
  ,test
  (test)
  :doc "fences remote file-session grants after target replacement"
  (let* ((host "file-incarnation")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-file-incarnation-" t)))
         (remote-root (format "/mevedelmock:%s:%s" host local-root))
         session buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (mevedel-workspace-identity-ensure remote-root)
            (let* ((workspace
                    (mevedel-workspace-get-or-create
                     'file "remote-file" remote-root "remote-file"))
                 (_ (setq session (mevedel-session-create "main" workspace)))
                 (target (mevedel-session-execution-target session))
                 (grant (file-name-concat remote-root "granted.txt")))
            (setq buffer (generate-new-buffer " *remote-file-incarnation*"))
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--workspace workspace
                          mevedel--session session)
              (setq default-directory remote-root)
              (insert "Remote file session\n")
              (should
               (mevedel-session-persistence-save session buffer t))
              (set-buffer-modified-p nil))
            (write-region "grant\n" nil grant nil 'silent)
            (setf (mevedel-session-resource-grants session)
                  (list (list :path grant :access 'read)))
            (with-current-buffer buffer
              (mevedel-session-persistence-save session buffer t)
              (set-buffer-modified-p nil))
            (mevedel-execution-target--record-incarnation
             target "replacement-incarnation")
            (with-current-buffer buffer
              (should
               (mevedel-session-persistence-assert-mutation-authority
                session buffer)))
            (should-not (mevedel-session-resource-grants session))
            (should-not
             (mevedel-execution-target-incarnation-changed-p target))
            (let ((sidecar
                   (mevedel-session-persistence-read
                    (mevedel-session-persistence--sidecar-path
                     (mevedel-session-save-path session)))))
              (should
               (equal "replacement-incarnation"
                      (plist-get sidecar :target-incarnation)))
              (should-not (plist-get sidecar :resource-grants))))))
      (when (and session (mevedel-session-save-path session))
        (ignore-errors
          (mevedel-session-persistence-lock-release
           (mevedel-session-save-path session) session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-mixed-authority-controls ()
  ,test
  (test)
  :doc "rejects mixed PID-lock and portable controls during discovery, restore, and admission"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-mixed-authority-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session-dir
          (file-name-as-directory
           (file-name-concat root ".mevedel" "sessions" "mixed")))
         (session (mevedel-session-create "mixed" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat session-dir ".lease") t)
          (with-temp-file (file-name-concat session-dir ".lock")
            (insert "mixed controls"))
          (setf (mevedel-session-save-path session) session-dir)
          (should-error
           (mevedel-session-persistence--authority-mode-for-path session-dir)
           :type 'error)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (should-error
             (mevedel-session-persistence-assert-mutation-authority
              session (current-buffer))
             :type 'error))
          (should-error
           (mevedel-session-persistence-list-sessions workspace)
           :type 'error)
          (should-error
           (mevedel-session-persistence-restore
            session-dir nil nil workspace)
           :type 'error))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-transfer-admission ()
  ,test
  (test)
  :doc "blocks new mutation admission while preserving a quiet drain"
  (let ((session (mevedel-session--create :name "transfer-admission"))
        (ordinary-called nil))
    (setf (mevedel-session-control-transfer session)
          '(:state quiescing))
    (cl-letf (((symbol-function
                'mevedel-session-persistence-assert-mutation-authority)
               (lambda (&rest _) (setq ordinary-called t))))
      (should-error
       (mevedel-session-persistence-assert-new-mutation-authority session)
       :type 'user-error)
      (should-not ordinary-called))))

(mevedel-deftest mevedel-session-persistence-control-transfer-poll ()
  ,test
  (test)
  :doc "keeps a drained transfer quiescing when no root buffer can be saved"
  (let* ((session (mevedel-session--create :name "transfer-poll"))
         (transfer '(:state quiescing))
         saved released)
    (setf (mevedel-session-control-transfer session) transfer)
    (cl-letf (((symbol-function
                'mevedel-session-transfer-poll)
               (lambda (_) transfer))
              ((symbol-function
                'mevedel-session-persistence--root-buffer-for-session)
               (lambda (&rest _) nil))
              ((symbol-function 'mevedel-session-persistence-save)
               (lambda (&rest _) (setq saved t)))
              ((symbol-function
                'mevedel-session-transfer-release)
               (lambda (_) (setq released t))))
      (should
       (eq transfer
           (mevedel-session-control-transfer-poll session nil nil)))
      (should-not saved)
      (should-not released)))
  :doc "refreshes committed transcript and sidecar state before enabling writes"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-transfer-refresh-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "requester" workspace))
         (refreshed (mevedel-session-create "requester" workspace))
         (buffer (generate-new-buffer " *mevedel-transfer-refresh*"))
         (save-path (file-name-as-directory
                     (file-name-concat root "session")))
         (segment-path (file-name-concat save-path "segment-0002.chat.org"))
         instructions-loaded incarnation-checked released)
    (make-directory save-path t)
    (write-region "initial transcript" nil segment-path nil 'silent)
    (setf (mevedel-session-save-path session) save-path
          (mevedel-session-control-transfer session)
          '(:state requested :request (:request-id "request"))
          (mevedel-session-turn-count refreshed) 7
          (mevedel-session-current-segment refreshed) 2)
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-file-name segment-path)
          (insert "stale transcript")
          (set-buffer-modified-p nil)
          (set-visited-file-modtime)
          (setq buffer-read-only t
                mevedel-session--read-only-mode t)
          (write-region "owner committed transcript" nil segment-path
                        nil 'silent)
          (should-not (verify-visited-file-modtime buffer))
          (cl-letf
              (((symbol-function 'mevedel-session-durability-lease-acquire)
                (lambda (_path _name actual)
                  (setf (mevedel-session-lease actual) '(:state owned))
                  t))
               ((symbol-function
                 'mevedel-session-publication-read)
                (lambda (_) '(:sidecar "/committed/session.meta.el")))
               ((symbol-function
                 'mevedel-session-persistence-load-sidecar)
                (lambda (_) '(:committed t)))
               ((symbol-function 'mevedel-session-persistence-deserialize)
                (lambda (&rest _) (list :session refreshed)))
               ((symbol-function 'mevedel-session-persistence-read-artifact)
                (lambda (&rest _) "fresh transcript"))
               ((symbol-function
                 'mevedel-session-persistence--check-target-incarnation)
                (lambda (_session checked-buffer)
                  (should (eq checked-buffer buffer))
                  (should buffer-read-only)
                  (setq incarnation-checked t)))
               ((symbol-function 'mevedel-transcript-restore-gptel-state)
                #'ignore)
               ((symbol-function
                 'mevedel-session-persistence--load-instructions)
                (lambda (&rest _) (setq instructions-loaded t)))
               ((symbol-function
                 'mevedel-session-durability-lease-release)
                (lambda (&rest _) (setq released t)))
               ((symbol-function 'ask-user-about-supersession-threat)
                (lambda (&rest _) (error "Supersession prompt"))))
            (should
             (mevedel-session-control-transfer-poll session buffer t))
            (should (= 7 (mevedel-session-turn-count session)))
            (should (equal "fresh transcript" (buffer-string)))
            (should-not buffer-read-only)
            (should-not mevedel-session--read-only-mode)
            (should instructions-loaded)
            (should incarnation-checked)
            (should-not released)
            (should
             (eq 'acquired
                 (plist-get (mevedel-session-control-transfer session)
                            :state)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "releases a newly acquired lease when committed refresh fails"
  (let* ((session (mevedel-session--create
                   :name "failed-refresh" :save-path "/session/"))
         (buffer (generate-new-buffer " *mevedel-transfer-failure*"))
         released)
    (setf (mevedel-session-control-transfer session)
          '(:state requested :request (:request-id "request")))
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-read-only t
                mevedel-session--read-only-mode t)
          (cl-letf
              (((symbol-function 'mevedel-session-durability-lease-acquire)
                (lambda (&rest _) t))
               ((symbol-function
                 'mevedel-session-publication-read)
                (lambda (_) (error "Injected refresh failure")))
               ((symbol-function
                 'mevedel-session-durability-lease-release)
                (lambda (&rest _) (setq released t))))
            (should-error
             (mevedel-session-control-transfer-poll session buffer t))
            (should released)
            (should buffer-read-only)
            (should mevedel-session--read-only-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-session-persistence-read-artifact ()
  ,test
  (test)
  :doc "reads local artifacts from their fixed logical paths"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-local-artifact-resolver-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "resolver" workspace))
         (session-dir
          (file-name-as-directory (file-name-concat root "session")))
         (segment (file-name-concat session-dir "segment-0001.chat.org")))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir
          (mevedel-session-publication session)
          '(:artifacts
            (("segment-0001.chat.org"
              :published "/must/not/be/read"
              :sha256
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
    (unwind-protect
        (progn
          (write-region "fixed" nil segment nil 'silent)
          (should
           (equal "fixed"
                  (mevedel-session-persistence-read-artifact
                   session "segment-0001.chat.org")))
          (should-error
           (mevedel-session-persistence-read-artifact session "../escape")))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))
  :doc "resolves remote staged owner bytes or verified committed bytes"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-artifact-resolver-" t)))
         (host "artifact-resolver")
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "fixed cache"))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path session-dir)))
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*resolver*" session))
                  (should
                   (mevedel-session-publication-publish
                    session
                    (list (list :path segment :content "committed")
                          (list :path sidecar :content "sidecar"
                                :commit-marker t))))
                  (write-region "stale fixed cache" nil segment nil 'silent)
                  (should
                   (equal "committed"
                          (mevedel-session-persistence-read-artifact
                           session "segment-0001.chat.org" t)))
                  (should
                   (mevedel-session-publication-publish
                    session (list (list :path segment :content "staged"))))
                  (should
                   (equal "staged"
                          (mevedel-session-persistence-read-artifact
                           session "segment-0001.chat.org")))
                  (should
                   (equal "committed"
                          (mevedel-session-persistence-read-artifact
                           session "segment-0001.chat.org" t)))
                  (let* ((entry
                          (cdr
                           (assoc
                            "segment-0001.chat.org"
                            (plist-get (mevedel-session-publication session)
                                       :artifacts))))
                         (published (plist-get entry :published)))
                    (write-region "corrupt" nil published nil 'silent)
                    (should-error
                     (mevedel-session-persistence-read-artifact
                      session "segment-0001.chat.org" t))))
              (mevedel-session-durability-lease-release session-dir session)
              (ignore workspace))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-artifact-present-p ()
  ,test
  (test)
  :doc "uses fixed logical-path existence for local sessions"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-local-artifact-present-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "resolver" workspace))
         (session-dir
          (file-name-as-directory (file-name-concat root "session")))
         (logical "plans/current.md")
         (path (file-name-concat session-dir logical)))
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should-not
           (mevedel-session-persistence-artifact-present-p session logical))
          (make-directory (file-name-directory path) t)
          (write-region "plan" nil path nil 'silent)
          (should
           (mevedel-session-persistence-artifact-present-p session logical))
          (should-error
           (mevedel-session-persistence-artifact-present-p
            session ".publications/escape")))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))
  :doc "uses remote staged or captured membership without fixed-cache fallback"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-artifact-present-" t)))
         (host "artifact-present")
         (mevedel-session-durability--client-id (make-string 64 ?b)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "fixed cache"))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path session-dir)))
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*present*" session))
                  (should
                   (mevedel-session-publication-publish
                    session
                    (list (list :path segment :content "committed")
                          (list :path sidecar :content "sidecar"
                                :commit-marker t))))
                  (delete-file segment)
                  (should
                   (mevedel-session-persistence-artifact-present-p
                    session "segment-0001.chat.org"))
                  (should-not
                   (mevedel-session-persistence-artifact-present-p
                    session "plans/missing.md"))
                  (let ((plan (file-name-concat session-dir "plans/current.md")))
                    (should
                     (mevedel-session-publication-publish
                      session (list (list :path plan :content "staged"))))
                    (should
                     (mevedel-session-persistence-artifact-present-p
                      session "plans/current.md"))))
              (mevedel-session-durability-lease-release session-dir session))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-find-artifact-noselect ()
  ,test
  (test)
  :doc "visits the logical path with verified remote bytes for inspection"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-find-artifact-" t)))
         (host "find-artifact")
         (mevedel-session-durability--client-id (make-string 64 ?c))
         buffer
         authoritative)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "fixed cache"))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path session-dir)))
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*find-artifact*" session))
                  (should
                   (mevedel-session-publication-publish
                    session
                    (list (list :path segment :content "committed transcript")
                          (list :path sidecar :content "sidecar"
                                :commit-marker t))))
                  (write-region "stale fixed cache" nil segment nil 'silent)
                  (setq buffer
                        (mevedel-session-persistence-find-artifact-noselect
                         session "segment-0001.chat.org" t))
                  (with-current-buffer buffer
                    (should (equal buffer-file-name segment))
                    (should (equal (buffer-string) "committed transcript"))
                    (should buffer-read-only)
                    (should mevedel-session--inspection-buffer-p)
                    (should-not (buffer-modified-p))
                    (should (verify-visited-file-modtime buffer)))
                  (cl-letf
                      (((symbol-function
                         'mevedel-session-persistence--find-file-noselect)
                        (lambda (&rest _)
                          (ert-fail "Remote adapter read the fixed cache"))))
                    (setq
                     authoritative
                     (mevedel-session-persistence-find-artifact-noselect
                      session "segment-0001.chat.org")))
                  (should-not (eq buffer authoritative))
                  (with-current-buffer authoritative
                    (should (equal buffer-file-name segment))
                    (should (equal (buffer-string) "committed transcript"))
                    (should-not buffer-read-only)
                    (should-not mevedel-session--inspection-buffer-p)
                    (should-not (buffer-modified-p))))
              (mevedel-session-durability-lease-release session-dir session))))
      (dolist (candidate (list buffer authoritative))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (setq buffer-read-only nil)
            (set-buffer-modified-p nil))
          (kill-buffer candidate)))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-publish-text ()
  ,test
  (test)
  :doc "writes local bytes atomically and returns their path"
  (let* ((root (make-temp-file "mevedel-publish-text-" t))
         (path (file-name-concat root "state.el"))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "publish" workspace)))
    (unwind-protect
        (progn
          (should
           (equal path
                  (mevedel-session-persistence-publish-text
                   session path "local state\n" 'utf-8-unix)))
          (should
           (equal "local state\n"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (delete-directory root t)))
  :doc "returns the remote queue outcome without claiming publication"
  (let* ((target
          (mevedel-execution-target-create "/ssh:user@host:/srv/project/"))
         (session (mevedel-session--create
                   :execution-target target
                   :authority-mode 'portable))
         (path "/ssh:user@host:/srv/project/state.el")
         published)
    (cl-letf
        (((symbol-function
           'mevedel-session-persistence-assert-mutation-authority)
          (lambda (_) t))
         ((symbol-function 'mevedel-session-publication-publish)
          (lambda (_session artifacts)
            (setq published artifacts)
            'queued)))
      (should
       (eq 'queued
           (mevedel-session-persistence-publish-text
            session path "remote state" 'utf-8-unix)))
      (should
       (equal
        (list (list :path path :content "remote state" :coding 'utf-8-unix))
        published)))))

(mevedel-deftest mevedel-session-persistence-publish-transcript-state ()
  ,test
  (test)
  :doc "commits a remote transcript and authoritative sidecar in one head"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-transcript-state-" t)))
         (host "transcript-state")
         (mevedel-session-durability--client-id (make-string 64 ?d))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "old transcript"))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path
                         session-dir))
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
                    session-dir "*transcript-state*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer " *transcript-state-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "old transcript"))
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    ;; Fixed caches are not publication authority.
                    (delete-file segment)
                    (delete-file sidecar)
                    (should
                     (mevedel-session-persistence-publish-transcript-state
                      session buffer segment "archived terminal\n"))
                    (should-not
                     (equal head-before
                            (plist-get
                             (mevedel-session-publication session) :head)))
                    (should
                     (equal
                      "archived terminal\n"
                      (mevedel-session-persistence-read-artifact
                       session "segment-0001.chat.org" t)))
                    (should
                     (plist-get
                      (with-temp-buffer
                        (insert
                         (mevedel-session-persistence-read-artifact
                          session "session.meta.el" t))
                        (goto-char (point-min))
                        (read (current-buffer)))
                      :session-id))))
              (when session
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-publish-sidecar-state ()
  ,test
  (test)
  :doc "commits only the remote sidecar and propagates publication failure"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-sidecar-state-" t)))
         (host "sidecar-state")
         (mevedel-session-durability--client-id (make-string 64 ?e))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "published transcript"))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path
                         session-dir))
                       (publications
                        (file-name-concat
                         root ".mevedel" "sessions"
                         (mevedel-session-session-id session)
                         ".publications"))
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
                    session-dir "*sidecar-state*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer " *sidecar-state-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "published transcript"))
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (setf (mevedel-session-name session) "sidecar-only")
                    (delete-file sidecar)
                    (should
                     (mevedel-session-persistence-publish-sidecar-state
                      session buffer))
                    (should-not
                     (equal head-before
                            (plist-get
                             (mevedel-session-publication session) :head)))
                    (should
                     (equal
                      "published transcript"
                      (mevedel-session-persistence-read-artifact
                       session "segment-0001.chat.org" t)))
                    (should
                     (equal
                      "sidecar-only"
                      (plist-get
                       (with-temp-buffer
                         (insert
                          (mevedel-session-persistence-read-artifact
                           session "session.meta.el" t))
                         (goto-char (point-min))
                         (read (current-buffer)))
                       :session-name))))
                  (let ((directories
                         (cons
                          publications
                          (cl-remove-if-not
                           #'file-directory-p
                           (directory-files-recursively
                            publications ".*" t)))))
                    (unwind-protect
                        (progn
                          (mapc (lambda (path) (set-file-modes path #o500))
                                directories)
                          (setf (mevedel-session-name session) "blocked")
                          (should-error
                           (mevedel-session-persistence-publish-sidecar-state
                            session buffer)
                           :type 'file-error)
                          (should
                           (mevedel-session-pending-publication session)))
                      (mapc (lambda (path) (set-file-modes path #o700))
                            directories))
                    (should
                     (mevedel-session-publication-retry session))))
              (when session
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))

  :doc "rejects reentrant queueing as an uncommitted strict marker"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "queued")))
      (cl-letf
          (((symbol-function
             'mevedel-session-persistence--sidecar-publication-artifact)
            (lambda (_session _root-buffer)
              '(:path "/target/session.meta.el"
                :content "sidecar"
                :commit-marker t)))
           ((symbol-function 'mevedel-session-publication-publish)
            (lambda (_session artifacts)
              (should (plist-get (car artifacts) :commit-marker))
              'queued)))
        (should-error
         (mevedel-session-persistence-publish-sidecar-state
          session (current-buffer))
         :type 'user-error)))))


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
                       (plist-get (car prompts) :preview))))))
  :doc "indexes a directive prompt at its paired boundary"
  (with-temp-buffer
    (insert "ordinary\n" (propertize "answer\n" 'gptel 'response))
    (let ((boundary-start (point)))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge start
                 :directive-id "directive-123" :action discuss :turn 2)))
      (insert "directive prompt\n")
      (insert (propertize "directive answer\n" 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge end
                 :directive-id "directive-123" :action discuss :turn 2
                 :outcome success :sequence 1)))
      (let ((prompt (nth 1 (mevedel-session-persistence--collect-prompts
                            (current-buffer)))))
        (should (= boundary-start (plist-get prompt :pos)))
        (should (eq 'directive (plist-get prompt :kind)))
        (should (equal "directive-123" (plist-get prompt :directive-id)))
        (should (= 2 (plist-get prompt :reserved-turn))))))
  :doc "keeps mixed chat and directive follow-ups in one chronology"
  (with-temp-buffer
    (insert "ordinary one\n" (propertize "answer one\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge start
               :directive-id "directive-123" :action discuss :turn 2)))
    (insert "directive one\n"
            (propertize "directive answer one\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge end
               :directive-id "directive-123" :action discuss :turn 2
               :outcome success :sequence 1)))
    (insert "ordinary two\n" (propertize "answer two\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge start
               :directive-id "directive-123" :action discuss :turn 4)))
    (insert "directive follow-up\n"
            (propertize "directive answer two\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge end
               :directive-id "directive-123" :action discuss :turn 4
               :outcome success :sequence 2)))
    (let ((prompts (mevedel-session-persistence--collect-prompts
                    (current-buffer))))
      (should (equal '(1 2 3 4) (mapcar (lambda (entry)
                                         (plist-get entry :turn))
                                       prompts)))
      (should (equal '(nil directive nil directive)
                     (mapcar (lambda (entry) (plist-get entry :kind))
                             prompts)))
      (should (equal '(2 4)
                     (delq nil
                           (mapcar (lambda (entry)
                                     (plist-get entry :reserved-turn))
                                   prompts)))))))

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
      (mevedel-workspace-clear-registry)))
  :doc "uses the directive boundary as the canonical cumulative turn"
  (let ((session (mevedel-session--create :current-segment 1)))
    (with-temp-buffer
      (insert "ordinary\n" (propertize "answer\n" 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge start
                 :directive-id "directive-123" :action discuss :turn 2)))
      (insert "directive prompt\n")
      (insert (propertize "directive answer\n" 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge end
                 :directive-id "directive-123" :action discuss :turn 2
                 :outcome success :sequence 1)))
      (mevedel-session-persistence--update-prompt-index
       session (current-buffer))
      (let ((prompt (nth 1 (cdr (assoc 1
                                       (mevedel-session-prompt-index
                                        session))))))
        (should (= 2 (plist-get prompt :cum-turn)))
        (should (= 2 (plist-get prompt :reserved-turn)))))))

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
                   :pos 0 :preview "alpha" :fork-point-id "a")
                  (:turn 2 :file-turn 2 :cum-turn 2
                   :pos 100 :preview "beta" :fork-point-id "b")))
            (2 . ((:turn 1 :file-turn 1 :cum-turn 3
                   :pos 0 :preview "alpha" :fork-point-id "c")
                  (:turn 2 :file-turn 2 :cum-turn 4
                   :pos 50 :preview "gamma" :fork-point-id "d")))))
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
                   :pos 100 :preview "after tail"
                   :fork-point-id "after-tail")))))
    (let* ((candidate
            (car (mevedel-session-persistence--prompt-candidates session)))
           (plist (cdr candidate)))
      (should (= 1 (plist-get plist :turn)))
      (should (= 3 (plist-get plist :file-turn))))
    (mevedel-workspace-clear-registry))
  :doc "labels directive turns by identity and action"
  (let ((session (mevedel-session--create
                  :prompt-index
                  '((1 . ((:turn 2 :file-turn 2 :cum-turn 2
                           :kind directive :directive-id "abcdef123456"
                           :action request-changes
                           :fork-point-id "directive")))))))
    (should (string-match-p
             "◆ abcdef12 · Request changes"
             (caar (mevedel-session-persistence--prompt-candidates
                    session))))))

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

(mevedel-deftest mevedel-session-persistence--staged-file-p ()
  ,test
  (test)
  :doc "checks the remote Git index with target-native command arguments"
  (let* ((host "staged-file-host")
         (root (file-name-as-directory
                (make-temp-file "mevedel-staged-file-" t)))
         (local-file (file-name-concat root "staged.el"))
         (remote-file (format "/mevedelmock:%s:%s" host local-file)))
    (unwind-protect
        (progn
          (test-mevedel-session-persistence--git root "init")
          (write-region "staged\n" nil local-file nil 'silent)
          (test-mevedel-session-persistence--git root "add" "staged.el")
          (mevedel-test--with-local-shell-tramp (list host)
            (should
             (mevedel-session-persistence--staged-file-p remote-file)))
          (test-mevedel-session-persistence--git
           root "reset" "--" "staged.el")
          (mevedel-test--with-local-shell-tramp (list host)
            (should-not
             (mevedel-session-persistence--staged-file-p remote-file))))
      (delete-directory root t)))

  :doc "does not expose client environment variables to target Git"
  (let* ((host "staged-file-environment-host")
         (root (file-name-as-directory
                (make-temp-file "mevedel-staged-environment-" t)))
         (local-file (file-name-concat root "staged.el"))
         (remote-file (format "/mevedelmock:%s:%s" host local-file))
         (process-environment
          (cons "MEVEDEL_CLIENT_SECRET=do-not-forward" process-environment)))
    (write-region "staged\n" nil local-file nil 'silent)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (&rest _)
                       (should-not (getenv "MEVEDEL_CLIENT_SECRET"))
                       "/usr/bin/git"))
                    ((symbol-function 'process-file)
                     (lambda (&rest _)
                       (should-not (getenv "MEVEDEL_CLIENT_SECRET"))
                       1)))
            (should
             (mevedel-session-persistence--staged-file-p remote-file))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-persistence--rewind-impact
  (:doc "lists the complete discarded prompt suffix in chronological order")
  (let* ((session
          (mevedel-session--create
           :name "rewind" :turn-count 3
           :prompt-index
           '((1 . ((:turn 1 :cum-turn 1 :preview "Directive one"
                    :fork-point-id "one")
                   (:turn 2 :cum-turn 2 :preview "Ordinary chat"
                    :fork-point-id "two")
                   (:turn 3 :cum-turn 3 :preview "Directive two"
                    :kind directive :directive-id "directive-two"
                    :action discuss :fork-point-id "three"))))))
         (target '(:segment 1 :turn 2 :cum-turn 2 :fork-point-id "two"))
         impact)
    (cl-letf (((symbol-function
                'mevedel-session-persistence--detached-child-count)
               (lambda (&rest _) 0))
              ((symbol-function 'display-buffer) #'ignore))
      (setq impact
            (mevedel-session-persistence--rewind-impact session target nil))
      (should
       (equal '("Ordinary chat" "Directive two")
              (mapcar (lambda (entry) (plist-get entry :preview))
                      (plist-get impact :discarded-prompts))))
      (mevedel-session-persistence--render-rewind-impact session impact)
      (with-current-buffer "*mevedel-rewind-impact*"
        (let ((text (buffer-string)))
          (should (string-match-p "Discarded session events" text))
          (should (< (string-match "Ordinary chat" text)
                     (string-match "◆ directiv · Discuss" text))))))))

(mevedel-deftest mevedel-session-persistence-rewind-checkpoint
  (:doc "resumes a cold session without replacing workspace directive records")
  (let* ((record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached)))
         (workspace (mevedel-workspace--create
                     :type 'file :id "checkpoint" :root "/tmp"
                     :name "checkpoint" :directives (list record)))
         (session (mevedel-session--create :session-id "cold-session"))
         (buffer (generate-new-buffer " *checkpoint-rewind*"))
         reset-records resumed restored rewound)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (cl-letf
              (((symbol-function
                 'mevedel--reset-instructions-preserving-directives)
                (lambda (_ records) (push records reset-records)))
               ((symbol-function 'mevedel--restore-preserved-directives)
                (lambda (_) (setq restored t)))
               ((symbol-function 'mevedel-session-persistence-resume-id)
                (lambda (owner session-id)
                  (setq resumed (list owner session-id))
                  buffer))
               ((symbol-function
                 'mevedel-session-persistence--prompt-candidates)
                (lambda (_)
                  '(("S1 T4" . (:segment 1 :turn 4 :cum-turn 4
                                  :fork-point-id "point")))))
               ((symbol-function 'mevedel-session-persistence-rewind)
                (lambda (selected target)
                  (setq rewound (list selected target))
                  t)))
            (should
             (mevedel-session-persistence-rewind-checkpoint
              workspace '(:session-id "cold-session" :turn 4))))
          (should (equal (list workspace "cold-session") resumed))
          (should restored)
          (should (= 2 (length reset-records)))
          (should (cl-every (lambda (records) (equal (list record) records))
                            reset-records))
          (should (eq buffer (car rewound)))
          (should (= 4 (plist-get (cadr rewound) :cum-turn))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-rewind ()
  ,test
  (test)
  :doc "errors when no current session"
  (with-temp-buffer
    (let ((mevedel--session nil))
      (should-error (mevedel-rewind) :type 'user-error)))
  :doc "refuses both pending-input categories before the picker"
  (dolist (category '(steering follow-up))
    (with-temp-buffer
      (let ((session (mevedel-session--create :name "rewind"))
            picked)
        (setq-local mevedel--session session)
        (mevedel-session-enqueue-pending-input
         session category '(:input "keep me"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq picked t))))
          (let ((err (should-error (mevedel-rewind) :type 'user-error)))
            (should (string-match-p "Pending Inputs"
                                    (error-message-string err)))
            (should (string-match-p "C-c C-q"
                                    (error-message-string err)))
            (should-not picked))))))
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
  :doc "refuses without changing a tree that has an active agent turn"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "rewind"))
           (record
            (mevedel-agent-record--create :activity 'running)))
      (setq-local mevedel--session session)
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (let ((err (should-error (mevedel-rewind) :type 'user-error)))
        (should (string-match-p
                 "Interrupt active agent turns"
                 (error-message-string err))))
      (should (eq record
                  (cdr (assoc
                        "/root/worker"
                        (mevedel-session-agent-registry session)))))))
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
  :doc "selecting the latest turn still confirms discarding that turn"
  (let* ((session
          (mevedel-session--create
           :name "rewind" :turn-count 1
           :prompt-index
           '((1 . ((:turn 1 :cum-turn 1 :fork-point-id "point"))))))
         (buffer (generate-new-buffer " *empty-rewind*"))
         (target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         confirmed committed started)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence--assert-stable-source)
                #'ignore)
               ((symbol-function
                 'mevedel-session-persistence-restore-plan)
                (lambda (&rest _) nil))
               ((symbol-function
                 'mevedel-session-persistence--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _) (setq confirmed t)))
               ((symbol-function
                 'mevedel-session-persistence--commit-rewind)
                (lambda (&rest _) (setq committed t)))
               ((symbol-function 'mevedel--run-session-start-hooks)
                (lambda (&rest _) (setq started t))))
            (mevedel-session-persistence-rewind buffer target))
          (should confirmed)
          (should committed)
          (should started))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "rechecks remote mutation authority after impact confirmation"
  (let* ((target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         (session
          (mevedel-session--create
           :name "rewind" :turn-count 1
           :prompt-index `((1 . (,target)))))
         (buffer (generate-new-buffer " *remote-authority-rewind*"))
         (authority-checks 0)
         confirmed)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence--assert-stable-source)
                #'ignore)
               ((symbol-function
                 'mevedel-session-persistence--prompt-candidates)
                (lambda (_) (list (cons "target" target))))
               ((symbol-function 'completing-read)
                (lambda (&rest _) "target"))
               ((symbol-function
                 'mevedel-session-persistence-assert-mutation-authority)
                (lambda (&rest _)
                  (cl-incf authority-checks)
                  (when confirmed
                    (user-error "Injected lost remote lease"))))
               ((symbol-function
                 'mevedel-session-persistence-restore-plan)
                (lambda (&rest _) nil))
               ((symbol-function
                 'mevedel-session-persistence--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (setq confirmed t)))
               ((symbol-function
                 'mevedel-session-persistence--commit-rewind)
                (lambda (&rest _)
                  (ert-fail "Rewind committed without mutation authority"))))
            (should-error (mevedel-rewind) :type 'user-error))
          (should (= 2 authority-checks)))
      (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
        (kill-buffer impact))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "rewinds first-turn modifications, creations, and deletions to pre-turn state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buffer (generate-new-buffer "*test-first-turn-rewind*"))
               (modified (file-name-concat tempdir "modified.el"))
               (created (file-name-concat tempdir "created.el"))
               (deleted (file-name-concat tempdir "deleted.el")))
          (unwind-protect
              (with-current-buffer buffer
                (org-mode)
                (setq-local mevedel--session session)
                (write-region "before-modify" nil modified nil 'silent)
                (write-region "before-delete" nil deleted nil 'silent)
                (mevedel-request-begin session)
                (let ((checkpoint
                       (mevedel-request-file-snapshots
                        mevedel--current-request)))
                  (puthash modified "before-modify" checkpoint)
                  (puthash created nil checkpoint)
                  (puthash deleted "before-delete" checkpoint))
                (write-region "after-modify" nil modified nil 'silent)
                (write-region "after-create" nil created nil 'silent)
                (delete-file deleted)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-persistence-save session buffer t)
                (let ((target
                       (copy-sequence
                        (cdar
                         (mevedel-session-persistence--prompt-candidates
                          session)))))
                  (mevedel-request-end)
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) t))
                            ((symbol-function
                              'mevedel--run-session-start-hooks)
                             #'ignore))
                    (should
                     (mevedel-session-persistence-rewind buffer target))))
                (should (= 0 (mevedel-session-turn-count session)))
                (should-not (string-match-p "First prompt" (buffer-string)))
                (should
                 (equal "before-modify"
                        (mevedel-session-persistence--file-text modified)))
                (should-not (file-exists-p created))
                (should
                 (equal "before-delete"
                        (mevedel-session-persistence--file-text deleted))))
            (test-mevedel-session-persistence--release-and-kill
             buffer session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "rewinds before a saved first gptel turn without exposing metadata"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (data-buf (generate-new-buffer " *test-gptel-rewind-data*"))
           (view-buf (generate-new-buffer " *test-gptel-rewind-view*"))
           (draft "> keep this draft\nsecond line"))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local mevedel--workspace workspace)
              (setq-local gptel-backend
                          (test-mevedel-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (mevedel-session-persistence-save session data-buf)
              (goto-char (point-max))
              (insert "Implement hello world\n")
              (dotimes (index 40)
                (let ((start (point)))
                  (insert (format "model span %02d\n" index))
                  (put-text-property
                   start (point) 'gptel
                   (if (= (% index 2) 0) 'response 'ignore))))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-persistence-save session data-buf t))
            (mevedel-view--setup view-buf data-buf)
            (with-current-buffer view-buf
              (mevedel-view--full-rerender)
              (mevedel-view-test--insert-composer-draft draft 4))
            (let ((target
                   (copy-sequence
                    (cdar
                     (mevedel-session-persistence--prompt-candidates
                      session)))))
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'mevedel--run-session-start-hooks)
                         #'ignore))
                (should
                 (mevedel-session-persistence-rewind data-buf target))))
            (should (= 0 (mevedel-session-turn-count session)))
            (should-not
             (cdr (assoc 1 (mevedel-session-prompt-index session))))
            (with-current-buffer data-buf
              (should
               (mevedel-session-persistence--property-drawer-region)))
            (let ((sidecar
                   (mevedel-session-persistence-read
                    (mevedel-session-persistence--sidecar-path
                     (mevedel-session-save-path session)))))
              (should-not (cdr (assoc 1 (plist-get sidecar :prompt-index)))))
            (with-current-buffer view-buf
              (should (equal draft (mevedel-view--input-text)))
              (should-not (string-match-p ":PROPERTIES:" (buffer-string)))
              (should-not (string-match-p "^You$" (buffer-string))))
            (should-not (get-buffer "*mevedel-rewind-impact*")))
        (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
          (kill-buffer impact))
        (when (buffer-live-p view-buf)
          (kill-buffer view-buf))
        (test-mevedel-session-persistence--release-and-kill
         data-buf session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "rewinds a later turn while preserving the preceding turn"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buffer (generate-new-buffer "*test-later-turn-rewind*"))
               (path (file-name-concat tempdir "serial.el")))
          (unwind-protect
              (with-current-buffer buffer
                (org-mode)
                (setq-local mevedel--session session)
                (write-region "zero" nil path nil 'silent)
                (mevedel-request-begin session)
                (puthash path "zero"
                         (mevedel-request-file-snapshots
                          mevedel--current-request))
                (write-region "one" nil path nil 'silent)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-persistence-save session buffer t)
                (mevedel-request-end)
                (mevedel-request-begin session)
                (puthash path "one"
                         (mevedel-request-file-snapshots
                          mevedel--current-request))
                (write-region "two" nil path nil 'silent)
                (insert "Second prompt\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 2)
                (mevedel-session-persistence-save session buffer t)
                (let* ((candidates
                        (mevedel-session-persistence--prompt-candidates
                         session))
                       (target (copy-sequence (cdar (last candidates)))))
                  (mevedel-request-end)
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) t))
                            ((symbol-function
                              'mevedel--run-session-start-hooks)
                             #'ignore))
                    (should
                     (mevedel-session-persistence-rewind buffer target))))
                (should (= 1 (mevedel-session-turn-count session)))
                (should (string-match-p "First reply" (buffer-string)))
                (should-not (string-match-p "Second prompt"
                                            (buffer-string)))
                (should
                 (equal "one"
                        (mevedel-session-persistence--file-text path))))
            (test-mevedel-session-persistence--release-and-kill
             buffer session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "keeps Rewind reachable and discloses known checkpoint gaps"
  (let* ((session
          (mevedel-session--create
           :name "rewind" :turn-count 1
           :prompt-index
           '((1 . ((:turn 1 :cum-turn 1 :fork-point-id "point"))))
           :file-snapshots
           '((1 . (("/unreadable" . (:gap "capture failed"
                                      :version 1)))))))
         (buffer (generate-new-buffer " *gap-rewind*"))
         (target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         confirmed impact-text)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence--assert-stable-source)
                #'ignore)
               ((symbol-function
                 'mevedel-session-persistence--detached-child-count)
                (lambda (&rest _) 0))
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (setq confirmed t
                        impact-text
                        (with-current-buffer "*mevedel-rewind-impact*"
                          (buffer-string)))
                  nil)))
            (mevedel-session-persistence-rewind buffer target))
          (should confirmed)
          (should (string-match-p "Checkpoint coverage: incomplete"
                                  impact-text))
          (should (string-match-p "/unreadable" impact-text))
          (should (string-match-p "capture failed" impact-text))
          (should-not (get-buffer "*mevedel-rewind-impact*")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "retains the impact buffer when the commit fails"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *failed-rewind*"))
           (missing (file-name-concat tempdir "missing.el")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "First prompt\n")
            (insert (propertize "First reply.\n" 'gptel 'response))
            (setf (mevedel-session-turn-count session) 1)
            (mevedel-session-persistence-save session buffer t)
            (let ((target
                   (copy-sequence
                    (cdar
                     (mevedel-session-persistence--prompt-candidates
                      session)))))
              (setf (mevedel-session-file-snapshots session)
                    `((1 . ((,missing . (:backup-name "missing"
                                         :pre-backup-name "missing"
                                         :version 1))))))
              (cl-letf (((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) t)))
                (should-error
                 (mevedel-session-persistence-rewind buffer target)))
              (should (get-buffer "*mevedel-rewind-impact*"))
              (should (= 1 (mevedel-session-turn-count session)))
              (should (string-match-p "First prompt" (buffer-string)))))
        (when-let* ((impact (get-buffer "*mevedel-rewind-impact*")))
          (kill-buffer impact))
        (test-mevedel-session-persistence--release-and-kill
         buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "preserves serial authored directives and reattaches a restored deleted source"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((session nil)
          (buffer nil)
          (source-buffer nil)
          consumed-child-id
          current-child-id
          (source-file (file-name-concat tempdir "source.el")))
      (unwind-protect
          (progn
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "EARLY\nLATER\n" nil source-file nil 'silent)
            (setq source-buffer (find-file-noselect source-file))
            (with-current-buffer source-buffer
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "EARLY")
              (mevedel--create-directive-in
               source-buffer (match-beginning 0) (match-end 0) nil "Early"))
            (setq session (mevedel-session-create "main" workspace)
                  buffer (generate-new-buffer "*test-directive-rewind*"))
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (insert "First directive\n")
              (insert (propertize "First reply.\n" 'gptel 'response))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-persistence-save session buffer t))
            (let ((early (car (mevedel-workspace-directives workspace))))
              (setf (mevedel-directive-attempts early)
                    (list
                     (mevedel-directive-attempt--create
                      :directive-request "Early" :request "Early prompt"
                      :result "Done" :outcome 'success :patch ""
                      :capture 'complete
                      :captured-at "2026-08-02T03:00:00+0200"
                      :checkpoint
                      (list :session-id (mevedel-session-session-id session)
                            :turn 1)))
                    (mevedel-directive-state early) 'implemented)
              (mevedel-directive-set-request early "Current early edit")
              (with-current-buffer source-buffer
                (goto-char (point-min))
                (search-forward "LATER")
                (let* ((start (match-beginning 0))
                       (later-overlay
                        (mevedel--create-directive-in
                         source-buffer start (match-end 0) nil "Later"))
                       (consumed-child
                        (mevedel--create-directive-in
                         source-buffer (1+ start) (+ start 3)
                         nil "Consumed detail")))
                  (setq consumed-child-id
                        (overlay-get consumed-child 'mevedel-uuid))
                  (overlay-put later-overlay
                               'mevedel-test-consumed-child
                               consumed-child)))
              (let* ((later (car (mevedel-workspace-directives workspace)))
                     (later-overlay
                      (mevedel--instruction-with-uuid
                       (mevedel-directive-id later) workspace))
                     (consumed-child
                      (overlay-get later-overlay
                                   'mevedel-test-consumed-child))
                     (consumed-snapshot
                      (mevedel-subdirective-copy
                       (mevedel--subdirective-record consumed-child)))
                     (records (copy-sequence
                               (mevedel-workspace-directives workspace)))
                     target)
                (setf (mevedel-directive-attempts later)
                      (list
                       (mevedel-directive-attempt--create
                        :directive-request "Later" :request "Later prompt"
                        :result "Deleted" :outcome 'success
                        :patch "deleted source.el" :capture 'complete
                        :captured-at "2026-08-02T03:01:00+0200"
                        :covered-files (list source-file)
                        :consumed-subdirectives (list consumed-snapshot)
                        :checkpoint
                        (list :session-id
                              (mevedel-session-session-id session)
                              :turn 2)))
                      (mevedel-directive-state later) 'implemented)
                (with-current-buffer source-buffer
                  (mevedel--delete-instruction consumed-child)
                  (setq current-child-id
                        (overlay-get
                         (mevedel--create-directive-in
                          source-buffer
                          (+ 2 (overlay-start later-overlay))
                          (1- (overlay-end later-overlay))
                          nil "Current detail")
                         'mevedel-uuid)))
                (with-current-buffer buffer
                  (mevedel-request-begin session)
                  (puthash source-file "EARLY\nLATER\n"
                           (mevedel-request-file-snapshots
                            mevedel--current-request))
                  (delete-file source-file)
                  (goto-char (point-max))
                  (insert "Second directive\n")
                  (insert (propertize "Second reply.\n" 'gptel 'response))
                  (setf (mevedel-session-turn-count session) 2)
                  (mevedel-session-persistence-save session buffer t)
                  (setq target
                        (copy-sequence
                         (cdr
                          (cl-find-if
                           (lambda (entry)
                             (= 2 (plist-get (cdr entry) :cum-turn)))
                           (mevedel-session-persistence--prompt-candidates
                            session)))))
                  (mevedel-request-end)
                  (goto-char (point-max))
                  (insert "Ordinary later chat\n")
                  (insert (propertize "Later reply.\n" 'gptel 'response))
                  (setf (mevedel-session-turn-count session) 3)
                  (mevedel-session-persistence-save session buffer t))
                (when (buffer-live-p source-buffer)
                  (kill-buffer source-buffer)
                  (setq source-buffer nil))
                (cl-letf (((symbol-function 'display-buffer) #'ignore)
                          ((symbol-function 'yes-or-no-p)
                           (lambda (&rest _) t))
                          ((symbol-function 'mevedel--run-session-start-hooks)
                           #'ignore))
                  (should
                   (mevedel-session-persistence-rewind buffer target)))
                (should (= 1 (mevedel-session-turn-count session)))
                (with-current-buffer buffer
                  (should (string-match-p "First reply" (buffer-string)))
                  (should-not (string-match-p "Second directive"
                                              (buffer-string)))
                  (should-not (string-match-p "Ordinary later chat"
                                              (buffer-string))))
                (should (file-exists-p source-file))
                (should (equal records
                               (mevedel-workspace-directives workspace)))
                (should (= 1 (length (mevedel-directive-attempts early))))
                (should-not (mevedel-directive-state early))
                (should-not (mevedel-directive-attempts later))
                (should-not (mevedel-directive-state later))
                (should
                 (equal
                  (list consumed-child-id current-child-id)
                  (mapcar #'mevedel-subdirective-id
                          (mevedel-directive-subdirectives later))))
                (should
                 (eq 'attached
                     (plist-get (mevedel-directive-anchor later) :state)))
                (let ((overlay
                       (mevedel--instruction-with-uuid
                        (mevedel-directive-id later) workspace)))
                  (should overlay)
                  (should (eq later (mevedel--directive-record overlay)))
                  (let ((consumed-restored
                         (mevedel--instruction-with-uuid
                          consumed-child-id workspace))
                        (current-restored
                         (mevedel--instruction-with-uuid
                          current-child-id workspace)))
                    (should (overlayp consumed-restored))
                    (should (overlayp current-restored))
                    (should (eq overlay
                                (mevedel--topmost-instruction
                                 consumed-restored 'directive)))
                    (should (eq overlay
                                (mevedel--topmost-instruction
                                 current-restored 'directive))))
                  (should
                   (equal "LATER"
                          (with-current-buffer (overlay-buffer overlay)
                            (buffer-substring-no-properties
                             (overlay-start overlay) (overlay-end overlay)))))))))
        (when (and buffer (buffer-live-p buffer))
          (test-mevedel-session-persistence--release-and-kill buffer session))
        (setq source-buffer (or source-buffer
                                (find-buffer-visiting source-file)))
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer (set-buffer-modified-p nil))
          (kill-buffer source-buffer))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "cancelling the impact confirmation changes no transcript or file state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-rewind-cancel*"))
               (path (file-name-concat tempdir "tracked.el"))
               target)
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-persistence-save session buf t)
                (setq target
                      (copy-sequence
                       (cdar
                         (mevedel-session-persistence--prompt-candidates
                         session))))
                (let ((backup
                       (mevedel-file-history--backup-name path 1)))
                  (mevedel-file-history--write-backup
                   (mevedel-session-save-path session) backup "first")
                  (setf (mevedel-session-file-snapshots session)
                        `((1 . ((,path . (:backup-name ,backup
                                        :pre-backup-name ,backup
                                        :version 1)))))))
                (goto-char (point-max))
                (insert "Second prompt\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 2)
                (mevedel-session-persistence-save session buf t)
                (let ((backup
                       (mevedel-file-history--backup-name path 2)))
                  (mevedel-file-history--write-backup
                   (mevedel-session-save-path session) backup "second")
                  (setf (mevedel-session-file-snapshots session)
                        (append
                         (mevedel-session-file-snapshots session)
                         `((2 . ((,path . (:backup-name ,backup
                                         :pre-backup-name ,backup
                                         :version 2))))))))
                (write-region "second" nil path nil 'silent)
                (mevedel-session-persistence-write
                 (mevedel-session-persistence--sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-persistence--build-sidecar session buf))
                (let* ((before-buffer (buffer-string))
                       (session-id (mevedel-session-session-id session))
                       (save-path (mevedel-session-save-path session))
                       starts ended
                       (before-sidecar
                        (with-temp-buffer
                          (insert-file-contents
                           (mevedel-session-persistence--sidecar-path
                            (mevedel-session-save-path session)))
                          (buffer-string))))
                  (cl-letf (((symbol-function 'display-buffer) #'ignore)
                            ((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) nil))
                            ((symbol-function
                              'mevedel--run-session-start-hooks)
                             (lambda (source)
                               (push source starts))))
                    (mevedel-session-persistence-rewind buf target))
                  (should-not starts)
                  (should-not (get-buffer "*mevedel-rewind-impact*"))
                  (should (equal before-buffer (buffer-string)))
                  (should
                   (equal
                    before-sidecar
                    (with-temp-buffer
                      (insert-file-contents
                       (mevedel-session-persistence--sidecar-path
                        (mevedel-session-save-path session)))
                      (buffer-string))))
                  (should
                   (equal "second"
                          (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string))))
                  (should (= 2 (mevedel-session-turn-count session)))
                  (should (equal buffer-file-name
                                 (mevedel-session-persistence--segment-path
                                  (mevedel-session-save-path session) 1)))
                  (let (confirmation)
                    (cl-letf (((symbol-function 'display-buffer) #'ignore)
                              ((symbol-function 'yes-or-no-p)
                               (lambda (prompt)
                                 (setq confirmation prompt)
                                 t))
                              ((symbol-function
                                'mevedel--run-session-start-hooks)
                               (lambda (source)
                                 (push source starts)
                                 (setf
                                  (mevedel-session-hook-context-pending session)
                                  `((:event SessionStart
                                     :body ,source)))))
                              ((symbol-function
                                'mevedel--run-session-end-hooks)
                               (lambda ()
                                 (setq ended t))))
                      (mevedel-session-persistence-rewind buf target))
                    (should (string-match-p "no redo" confirmation)))
                  (should (equal '("rewind") starts))
                  (should-not ended)
                  (let* ((context
                          (mevedel-session-hook-context-pending session))
                         (submission
                          (mevedel-prompt-submission-create
                           :session session :context-entries context)))
                    (mevedel-prompt-submission-commit submission)
                    (should-not
                     (mevedel-session-hook-context-pending session)))
                  (should (equal session-id
                                 (mevedel-session-session-id session)))
                  (should (equal save-path
                                 (mevedel-session-save-path session)))
                  (should (= 0 (mevedel-session-turn-count session)))
                  (should-not (string-match-p "First prompt"
                                              (buffer-string)))
                  (should-not (string-match-p "Second prompt"
                                              (buffer-string)))
                  (should
                   (equal "first"
                          (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string))))
                  (let ((sidecar
                         (mevedel-session-persistence-read
                          (mevedel-session-persistence--sidecar-path
                           save-path))))
                    (should (= 0 (plist-get sidecar :total-turn-count)))
                    (should-not (plist-get sidecar
                                           :forked-from-session-id)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--commit-rewind ()
  ,test
  (test)
  :doc "post-publication failure rolls back files, transcript, sidecar, and session state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-rewind-rollback*"))
               (path-a (file-name-concat tempdir "a.el"))
               (path-b (file-name-concat tempdir "b.el"))
               target record)
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--session session)
                (insert "First prompt\n")
                (insert (propertize "First reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 1)
                (mevedel-session-persistence-save session buf t)
                (setq target
                      (copy-sequence
                       (cdar
                        (mevedel-session-persistence--prompt-candidates
                         session))))
                (let ((backup-a
                       (mevedel-file-history--backup-name path-a 1))
                      (backup-b
                       (mevedel-file-history--backup-name path-b 1)))
                  (mevedel-file-history--write-backup
                   (mevedel-session-save-path session) backup-a "old-a")
                  (mevedel-file-history--write-backup
                   (mevedel-session-save-path session) backup-b "old-b")
                  (setf (mevedel-session-file-snapshots session)
                        `((1 . ((,path-a . (:backup-name ,backup-a
                                          :version 1))
                                (,path-b . (:backup-name ,backup-b
                                          :version 1)))))))
                (goto-char (point-max))
                (insert "Second prompt\n")
                (insert (propertize "Second reply.\n" 'gptel 'response))
                (setf (mevedel-session-turn-count session) 2)
                (mevedel-session-persistence-save session buf t)
                (write-region "new-a" nil path-a nil 'silent)
                (write-region "new-b" nil path-b nil 'silent)
                (mevedel-session-persistence-write
                 (mevedel-session-persistence--sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-persistence--build-sidecar session buf))
                (setq record
                      (mevedel-directive--create
                       :id "rollback-directive" :request "Keep me"
                       :anchor '(:state source-missing) :state 'failed
                       :subdirectives
                       (list
                        (mevedel-subdirective--create
                         :id "current-child" :request "Current"
                         :anchor '(:state attached)))
                       :attempts
                       (list
                        (mevedel-directive-attempt--create
                         :directive-request "Keep me" :outcome 'success
                         :checkpoint
                         (list :session-id
                               (mevedel-session-session-id session)
                               :turn 1))
                        (mevedel-directive-attempt--create
                         :directive-request "Keep me" :outcome 'error
                         :consumed-subdirectives
                         (list
                          (mevedel-subdirective--create
                           :id "consumed-child" :request "Consumed"
                           :anchor '(:state attached)))
                         :checkpoint
                         (list :session-id
                               (mevedel-session-session-id session)
                               :turn 2)))))
                (mevedel-workspace-add-directive workspace record)
                (let* ((plan
                        (mevedel-session-persistence-restore-plan session 1))
                       (before-buffer (buffer-string))
                       (before-attempts
                        (mevedel-directive-attempts record))
                       (before-subdirectives
                        (mevedel-directive-subdirectives record))
                       (before-state
                        (mevedel-session-persistence-serialize session))
                       (sidecar-path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session)))
                       (before-sidecar
                        (mevedel-session-persistence--file-text sidecar-path)))
                  (cl-letf
                      (((symbol-function
                         'mevedel-session-persistence--save-instructions)
                        (lambda (&rest _)
                          (error "Injected publication failure"))))
                    (should-error
                     (mevedel-session-persistence--commit-rewind
                      session buf target plan)))
                  (should (equal before-buffer (buffer-string)))
                  (should (equal before-state
                                 (mevedel-session-persistence-serialize
                                  session)))
                  (should (equal before-sidecar
                                 (mevedel-session-persistence--file-text
                                  sidecar-path)))
                  (should
                   (equal "new-a"
                          (mevedel-session-persistence--file-text path-a)))
                  (should
                   (equal "new-b"
                          (mevedel-session-persistence--file-text path-b)))
                  (should (equal (list record)
                                 (mevedel-workspace-directives workspace)))
                  (should (eq before-attempts
                              (mevedel-directive-attempts record)))
                  (should (eq before-subdirectives
                              (mevedel-directive-subdirectives record)))
                  (should (eq 'failed (mevedel-directive-state record)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "pre-restore failure does not overwrite a concurrent file edit"
  (let* ((tempdir
          (file-name-as-directory
           (make-temp-file "mevedel-rewind-pre-restore-" t)))
         (save-path (file-name-as-directory
                     (file-name-concat tempdir "session")))
         (path (file-name-concat tempdir "tracked.el"))
         (workspace
          (mevedel-workspace--create
           :type 'file :id "rewind" :root tempdir :name "rewind"))
         (session
          (mevedel-session--create
           :name "main"
           :workspace workspace
           :save-path save-path
           :current-segment 1
           :turn-count 1))
         (buffer (generate-new-buffer " *rewind-pre-restore*"))
         (target '(:segment 1 :turn 1 :cum-turn 1
                   :fork-point-id "point"))
         (plan (list (list :path path :action 'overwrite))))
    (unwind-protect
        (progn
          (make-directory save-path t)
          (write-region "before" nil path nil 'silent)
          (with-current-buffer buffer
            (insert "transcript"))
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence--rewind-candidate)
                (lambda (&rest _) (copy-sequence session)))
               ((symbol-function
                 'mevedel-session-persistence--stage-rewind)
                (lambda (&rest _)
                  (write-region "concurrent" nil path nil 'silent)
                  (error "Injected staging failure"))))
            (should-error
             (mevedel-session-persistence--commit-rewind
              session buffer target plan)))
          (should
           (equal "concurrent"
                  (mevedel-session-persistence--file-text path))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory tempdir t))))

(mevedel-deftest mevedel-session-persistence--commit-remote-rewind ()
  ,test
  (test)
  :doc "commits one replacement head without moving remote control state"
  (let* ((host "remote-rewind-commit")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rewind-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?b))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point")))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*remote-rewind*" session))
              (setf (mevedel-session-publication session)
                    (mevedel-session-publication-read session-dir)
                    (mevedel-session-turn-count session) 1
                    (mevedel-session-prompt-index session)
                    `((1 . (,target))))
              (setq buffer (generate-new-buffer " *remote-rewind-live*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local mevedel--session session)
                (setq buffer-file-name segment))
              (let ((generation
                     (plist-get (mevedel-session-lease session) :generation))
                    (old-head
                     (plist-get (mevedel-session-publication session) :head)))
                (cl-letf
                    (((symbol-function
                       'mevedel-session-persistence--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (mevedel-session-persistence--segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'mevedel-session-persistence-restore-plan)
                      (lambda (&rest _) nil))
                     ((symbol-function
                       'mevedel-session-persistence--load-instructions)
                      (lambda (&rest _) t))
                     ((symbol-function 'mevedel-workspace-rewind-directives)
                      #'ignore)
                     ((symbol-function 'mevedel--restore-preserved-directives)
                      #'ignore)
                     ((symbol-function
                       'mevedel-session-persistence--refresh-restored-buffers)
                      #'ignore))
                  (should
                   (mevedel-session-persistence--commit-remote-rewind
                    session buffer target nil)))
                (should
                 (equal generation
                        (plist-get (mevedel-session-lease session)
                                   :generation)))
                (should-not
                 (equal old-head
                        (plist-get (mevedel-session-publication session)
                                   :head)))
                (should
                 (file-exists-p (file-name-concat session-dir old-head)))
                (should
                 (file-directory-p
                  (file-name-concat session-dir ".publications")))
                (should
                 (equal "Rewound transcript\n"
                        (mevedel-session-persistence-read-artifact
                         session "segment-0001.chat.org" t)))
                (should (= 0 (mevedel-session-turn-count session)))
                (with-current-buffer buffer
                  (should (equal "Rewound transcript\n" (buffer-string))))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'mevedel--session buffer)))
          (when (and session
                     (mevedel-session-durability-lease-owned-p session))
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "rolls project files back under a fresh authority reservation"
  (let* ((host "remote-rewind-rollback")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-rewind-rollback-" t)))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (_workspace session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Original transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?c))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (tracked (file-name-concat
                             (file-name-directory session-dir) "tracked.el"))
                   (target '(:segment 1 :turn 1 :cum-turn 1
                             :fork-point-id "remote-point"))
                   (plan (list (list :path tracked :action 'overwrite)))
                   (reserve-function
                    (symbol-function
                     'mevedel-session-durability-call-with-reserved-lease))
                   reservations)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (should
               (mevedel-session-durability-lease-acquire
                session-dir "*remote-rewind-rollback*" session))
              (setf (mevedel-session-publication session)
                    (mevedel-session-publication-read session-dir)
                    (mevedel-session-turn-count session) 1)
              (write-region "current project bytes\n" nil tracked nil 'silent)
              (setq buffer (generate-new-buffer " *remote-rewind-rollback*"))
              (with-current-buffer buffer
                (org-mode)
                (insert "Original transcript\n")
                (setq-local mevedel--session session)
                (setq buffer-file-name segment))
              (let ((old-head
                     (plist-get (mevedel-session-publication session) :head)))
                (cl-letf
                    (((symbol-function
                       'mevedel-session-persistence--stage-rewind)
                      (lambda (_session candidate actual-target staging-path
                               staging-buffer &rest _)
                        (make-directory staging-path t)
                        (with-current-buffer staging-buffer
                          (erase-buffer)
                          (insert "Rewound transcript\n")
                          (setq buffer-file-name
                                (mevedel-session-persistence--segment-path
                                 staging-path
                                 (plist-get actual-target :segment)))
                          (write-region (point-min) (point-max)
                                        buffer-file-name nil 'silent))
                        candidate))
                     ((symbol-function
                       'mevedel-session-persistence-restore-plan)
                      (lambda (&rest _) plan))
                     ((symbol-function
                       'mevedel-session-persistence-execute-restore)
                      (lambda (_session _plan)
                        (write-region "rewound project bytes\n" nil
                                      tracked nil 'silent)
                        '(:succeeded 1 :failed nil :total 1)))
                     ((symbol-function 'mevedel-session-publication-publish)
                      (lambda (actual-session _artifacts)
                        (setf (mevedel-session-pending-publication
                               actual-session)
                              '(:batches nil))
                        (error "Injected pre-CAS failure")))
                     ((symbol-function
                       'mevedel-session-durability-call-with-reserved-lease)
                      (lambda (actual-session function)
                        (push (mevedel-session-save-path actual-session)
                              reservations)
                        (funcall reserve-function actual-session function))))
                  (should-error
                   (mevedel-session-persistence--commit-remote-rewind
                    session buffer target plan)))
                (should (= 2 (length reservations)))
                (should-not (mevedel-session-pending-publication session))
                (should
                 (equal old-head
                        (plist-get (mevedel-session-publication session)
                                   :head)))
                (should
                 (equal "current project bytes\n"
                        (mevedel-session-persistence--file-text tracked)))
                (with-current-buffer buffer
                  (should (equal "Original transcript\n"
                                 (buffer-string))))))))
      (when (buffer-live-p buffer)
        (let ((session (buffer-local-value 'mevedel--session buffer)))
          (when (and session
                     (mevedel-session-durability-lease-owned-p session))
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--rollback-restore-files ()
  ,test
  (test)
  :doc "reports every file whose rollback fails"
  (let* ((path-a "/tmp/rewind-a")
         (path-b "/tmp/rewind-b")
         (backups
          `((:path ,path-a :existed t :backup "/tmp/backup-a")
            (:path ,path-b :existed t :backup "/tmp/backup-b")))
         (failures
          (cl-letf (((symbol-function 'make-directory) #'ignore)
                    ((symbol-function 'copy-file)
                     (lambda (_source target &rest _)
                       (error "Cannot restore %s" target))))
            (mevedel-session-persistence--rollback-restore-files
             backups))))
    (should (= 2 (length failures)))
    (should (string-match-p (regexp-quote path-a) (nth 0 failures)))
    (should (string-match-p (regexp-quote path-b) (nth 1 failures)))))


;;
;;; Phase 8: file restore plan

(mevedel-deftest mevedel-session-persistence--state-at-turn ()
  ,test
  (test)
  :doc "picks each path's earliest pre-turn checkpoint in the discarded suffix"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/abs/foo" . (:backup-name "fooA-post"
                                  :pre-backup-name "fooA" :version 1))))
            (3 . (("/abs/foo" . (:backup-name "fooC-post"
                                  :pre-backup-name "fooC" :version 3))
                  ("/abs/bar" . (:backup-name "barB-post"
                                  :pre-backup-name "barB" :version 2))))
            (5 . (("/abs/foo" . (:backup-name "fooE-post"
                                  :pre-backup-name "fooE" :version 5))))))
    ;; Rewind before turn 2: foo and bar first change at turn 3.
    (let ((state (mevedel-session-persistence--state-at-turn session 2 t)))
      (should (= 2 (length state)))
      (should (equal "fooC"
                     (plist-get (cdr (assoc "/abs/foo" state))
                                :pre-backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state))
                                :pre-backup-name))))
    ;; Rewind before turn 1 selects foo's turn-1 checkpoint and bar's turn-3
    ;; checkpoint because bar was not changed before then.
    (let ((state (mevedel-session-persistence--state-at-turn session 1 t)))
      (should (= 2 (length state)))
      (should (equal "fooA"
                     (plist-get (cdr (assoc "/abs/foo" state))
                                :pre-backup-name)))
      (should (equal "barB"
                     (plist-get (cdr (assoc "/abs/bar" state))
                                :pre-backup-name))))
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

(mevedel-deftest mevedel-save-session ()
  ,test
  (test)
  :doc "refuses save-as without changing a tree that has an active turn"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "save-as"))
           (record
            (mevedel-agent-record--create :activity 'running)))
      (setq-local mevedel--session session)
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (let ((err (should-error (mevedel-save-session t)
                               :type 'user-error)))
        (should (string-match-p
                 "Interrupt active agent turns"
                 (error-message-string err))))
      (should (eq record
                  (cdr (assoc
                        "/root/worker"
                        (mevedel-session-agent-registry session)))))))
  :doc "save-as canonically publishes the parent and cloned sidecar"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *save-as-publication*"))
           (save-function
            (symbol-function 'mevedel-session-persistence-save))
           (publish-function
            (symbol-function 'mevedel-session-persistence-publish-text))
           (canonical-saves 0)
           published)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "original\n")
            (mevedel-session-persistence-save session buffer)
            (goto-char (point-max))
            (insert "pending\n")
            (cl-letf
                (((symbol-function 'read-string)
                  (lambda (&rest _) "clone"))
                 ((symbol-function 'mevedel-session-persistence-save)
                  (lambda (&rest arguments)
                    (cl-incf canonical-saves)
                    (apply save-function arguments)))
                 ((symbol-function
                   'mevedel-session-persistence-publish-text)
                  (lambda (actual-session path content &optional coding)
                    (push path published)
                    (funcall publish-function
                             actual-session path content coding))))
              (mevedel-save-session t))
            (should (= 1 canonical-saves))
            (should
             (member
              (mevedel-session-persistence--sidecar-path
               (mevedel-session-save-path session))
              published))
            (should-not (buffer-modified-p)))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "remote save-as fences the parent until a fresh child lease is held"
  (let* ((host "save-as-lease-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-save-as-remote-" t)))
         (owner-id (make-string 64 ?a))
         (competitor-id (make-string 64 ?b))
         buffer
         session
         old-id
         old-save-path
         new-save-path)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (unwind-protect
              (cl-destructuring-bind
                  (workspace fixture-session session-dir segment)
                  (test-mevedel-session-persistence--make-remote-restore-fixture
                   host local-root "Parent transcript\n")
                (setq session fixture-session
                      old-id (mevedel-session-session-id fixture-session)
                      old-save-path session-dir
                      buffer (generate-new-buffer " *save-as-lease*"))
                (let* ((mevedel-session-durability--client-id owner-id)
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal))
                       (materialize-function
                        (symbol-function
                         'mevedel-session-persistence--materialize-publication))
                       (publish-function
                        (symbol-function
                         'mevedel-session-publication-publish))
                       parent-generation
                       competitor-blocked
                       fresh-child
                       materialized
                       copy-called
                       child-rewrite-authorized
                       child-session
                       parent-artifacts
                       staging-path
                       target-probed)
                  (puthash
                   (mevedel-execution-target-identity
                    (mevedel-session-execution-target session))
                   t mevedel-session-durability--disclosed-targets)
                  ;; Start the parent above generation one so copied lease
                  ;; state cannot look like a freshly acquired child.
                  (should
                   (mevedel-session-durability-lease-acquire
                    old-save-path "*save-as-parent*" session))
                  (mevedel-session-durability-lease-release
                   old-save-path session)
                  (should
                   (mevedel-session-durability-lease-acquire
                    old-save-path "*save-as-parent*" session))
                  (setq parent-generation
                        (plist-get
                         (mevedel-session-lease session) :generation))
                  (should (> parent-generation 1))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session
                                buffer-file-name segment)
                    (insert "Parent transcript\n"))
                  (cl-letf
                      (((symbol-function 'read-string)
                        (lambda (&rest _) "clone"))
                       ((symbol-function 'mevedel-execution-target-probe)
                        (lambda (&rest _)
                          (setq target-probed t)
                          '(:status ready)))
                       ((symbol-function 'copy-directory)
                        (lambda (&rest _)
                          (setq copy-called t)
                          (error "Remote Save As must not copy a session directory")))
                       ((symbol-function
                         'mevedel-session-persistence--materialize-publication)
                        (lambda (actual-session publication destination)
                          (let ((mevedel-session-durability--client-id
                                 competitor-id))
                            (should-not
                             (mevedel-session-durability-lease-acquire
                              old-save-path "*save-as-competitor*")))
                          (setq competitor-blocked t
                                staging-path destination)
                          (setq parent-artifacts
                                (sort
                                 (mapcar #'car
                                         (plist-get publication :artifacts))
                                 #'string-lessp))
                          (setq materialized t)
                          (funcall materialize-function
                                   actual-session publication destination)
                          (should (file-exists-p
                                   (file-name-concat destination
                                                     "segment-0001.chat.org")))
                          (should-not
                           (file-exists-p
                            (file-name-concat destination ".publications")))
                          (should-not
                           (file-exists-p
                            (file-name-concat destination ".recovery")))
                          (should-not
                           (file-exists-p
                           (file-name-concat destination ".lock")))))
                        ((symbol-function
                        'mevedel-session-publication-publish)
                        (lambda (actual-session artifacts)
                          (let ((sidecar-artifact
                                 (cl-find-if
                                  (lambda (artifact)
                                    (equal
                                     (plist-get artifact :path)
                                     (mevedel-session-persistence--sidecar-path
                                      (mevedel-session-save-path actual-session))))
                                  artifacts)))
                            (when (and staging-path sidecar-artifact)
                            (should-not (eq actual-session session))
                            (setq fresh-child t)
                            (setq child-session actual-session)
                            (should
                             (equal staging-path
                                    (mevedel-session-save-path actual-session)))
                            (should (plist-get sidecar-artifact
                                               :commit-marker))
                            (should
                             (mevedel-session-durability-lease-owned-p
                              actual-session))
                            (should
                             (eq
                              'publishing
                              (plist-get
                               (mevedel-session-durability--lease-head
                                (mevedel-session-durability--lease-path
                                 old-save-path))
                               :status)))
                            (setq child-rewrite-authorized t)))
                          (funcall publish-function
                                   actual-session artifacts))))
                    (should
                     (eq 'portable
                         (mevedel-session-persistence--authority-mode
                          session)))
                    (with-current-buffer buffer
                      (mevedel-save-session t))
                  (should target-probed)
                  (should competitor-blocked)
                  (should materialized)
                  (should-not copy-called)
                  (should fresh-child)
                  (should child-rewrite-authorized)
                  (should child-session)
                  (should
                   (mevedel-session-durability-lease-owned-p child-session))
                  (should
                   (= 1
                      (plist-get (mevedel-session-lease session)
                                 :generation)))
                  (should
                   (eq
                    'released
                    (plist-get
                     (mevedel-session-durability--lease-head
                      (mevedel-session-durability--lease-path old-save-path))
                     :status)))
                  (setq new-save-path (mevedel-session-save-path session))
                  (let ((publication
                         (mevedel-session-publication-read
                          new-save-path)))
                    (should publication)
                    (should
                     (equal parent-artifacts
                            (sort
                             (mapcar #'car
                                     (plist-get publication :artifacts))
                             #'string-lessp)))
                    (should
                     (= 1
                        (length
                         (directory-files
                          (file-name-concat new-save-path ".publications")
                          nil "\\`generation-"))))
                    (should
                     (> (length
                         (directory-files
                          (file-name-concat old-save-path ".publications")
                          nil "\\`generation-"))
                        1))
                    (let ((transcript
                           (condition-case err
                               (mevedel-session-persistence-read-artifact
                                session "segment-0001.chat.org" t)
                             (error
                              (ert-fail
                               (format
                                "Save As child transcript read failed: %S; publication=%S"
                                err publication))))))
                      (should (equal "Parent transcript\n" transcript)))
                    (let ((sidecar
                           (with-temp-buffer
                             (insert
                              (mevedel-session-persistence-read-artifact
                               session "session.meta.el" t))
                             (goto-char (point-min))
                             (read (current-buffer)))))
                      (should (equal "clone"
                                     (plist-get sidecar :session-name)))
                      (should
                       (equal old-id
                              (plist-get sidecar
                                         :forked-from-session-id)))))))
            (when (and session (mevedel-session-save-path session))
              (ignore-errors
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path session) session)))
            (when old-save-path
              (ignore-errors
                (mevedel-session-persistence-lock-release old-save-path)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-session-save-as-run ()
  ,test
  (test)
  :doc "cleans pre-commit recovery and keeps every committed child discoverable"
  (dolist (failure-point
           '(pre-commit publish-pre-commit post-commit move-post-commit))
    (let* ((host (format "save-as-%s" failure-point))
           (local-root
            (file-name-as-directory
             (make-temp-file "mevedel-save-as-failure-" t)))
           (owner-id (make-string 64 ?a))
           buffer
           view-buffer
           session
           old-save-path
           new-save-path
           staging-path)
      (unwind-protect
          (mevedel-test--with-local-shell-tramp (list host)
            (cl-destructuring-bind
                (_workspace fixture-session session-dir segment)
                (test-mevedel-session-persistence--make-remote-restore-fixture
                 host local-root "Parent transcript\n")
              (setq session fixture-session
                    old-save-path session-dir
                    new-save-path
                    (file-name-as-directory
                     (file-name-concat
                      (file-name-directory (directory-file-name session-dir))
                      (format "clone-%s" failure-point)))
                    buffer (generate-new-buffer " *save-as-failure*")
                    view-buffer (generate-new-buffer " *save-as-failure:view*"))
              (let* ((mevedel-session-durability--client-id owner-id)
                     (mevedel-session-durability--disclosed-targets
                      (make-hash-table :test #'equal))
                     (materialize-function
                      (symbol-function
                       'mevedel-session-persistence--materialize-publication))
                     (publish-function
                      (symbol-function 'mevedel-session-publication-publish))
                     (rename-function (symbol-function 'rename-file))
                     recovery)
                (puthash
                 (mevedel-execution-target-identity
                  (mevedel-session-execution-target session))
                 t mevedel-session-durability--disclosed-targets)
                (should
                 (mevedel-session-durability-lease-acquire
                  old-save-path "save-as-owner" session))
                (with-current-buffer buffer
                  (org-mode)
                  (setq-local mevedel--session session
                              mevedel--view-buffer view-buffer
                              buffer-file-name segment)
                  (insert "Parent transcript\n"))
                (cl-letf
                    (((symbol-function
                       'mevedel-session-persistence--materialize-publication)
                      (lambda (actual-session publication destination)
                        (setq staging-path destination)
                        (if (eq failure-point 'pre-commit)
                            (error "Injected pre-commit Save As failure")
                          (funcall materialize-function
                                   actual-session publication destination))))
                     ((symbol-function 'mevedel-session-publication-publish)
                      (lambda (actual-session artifacts)
                        (if (eq failure-point 'publish-pre-commit)
                            (let* ((directory
                                    (make-temp-file
                                     "mevedel-save-as-recovery-" t))
                                   (source
                                    (file-name-concat directory "artifact")))
                              (write-region "recovery" nil source nil 'silent)
                              (setq recovery directory)
                              (setf
                               (mevedel-session-pending-publication
                                actual-session)
                               (list
                                :batches
                                (list
                                 (list :directory directory
                                       :artifacts
                                       (list (list :source source))))))
                              (error "Injected publication failure"))
                          (prog1
                              (funcall publish-function
                                       actual-session artifacts)
                            (when (eq failure-point 'post-commit)
                              (error "Injected post-commit Save As failure"))))))
                     ((symbol-function 'rename-file)
                      (lambda (file newname &optional ok-if-exists)
                        (if (and (eq failure-point 'move-post-commit)
                                 staging-path
                                 (equal file
                                        (directory-file-name staging-path)))
                            (error "Injected Save As discovery move failure")
                          (funcall rename-function
                                   file newname ok-if-exists)))))
                  (let ((error
                         (should-error
                          (mevedel-session-save-as-run
                           session buffer "clone"
                           (file-name-nondirectory
                            (directory-file-name new-save-path))
                           new-save-path))))
                    (if (memq failure-point
                              '(pre-commit publish-pre-commit))
                        (progn
                          (should-not (file-directory-p staging-path))
                          (should-not (file-directory-p new-save-path))
                          (should
                           (equal old-save-path
                                  (mevedel-session-save-path session)))
                          (should
                           (mevedel-session-durability-lease-owned-p session)))
                      (should
                       (string-match-p
                        "committed a child"
                        (error-message-string error)))
                      (if (eq failure-point 'move-post-commit)
                          (progn
                            (should-not (file-directory-p new-save-path))
                            (should (file-directory-p staging-path))
                            (should
                             (equal staging-path
                                    (mevedel-session-save-path session)))
                            (should
                             (cl-find staging-path
                                      (mevedel-session-persistence-list-sessions
                                       (mevedel-session-workspace session))
                                      :key (lambda (entry)
                                             (plist-get entry :save-path))
                                      :test #'equal)))
                        (should (file-directory-p new-save-path)))
                      (should
                       (mevedel-session-publication-read
                        (mevedel-session-save-path session)))
                      (should
                       (string-match-p "clone" (buffer-name buffer)))
                      (should
                       (string-match-p "clone" (buffer-name view-buffer)))
                      (should
                       (mevedel-session-durability-lease-owned-p session))))
                  (when recovery
                    (should-not (file-exists-p recovery)))))))
        (mevedel-test--with-local-shell-tramp (list host)
          (when (and session (mevedel-session-save-path session))
            (ignore-errors
              (mevedel-session-persistence-lock-release
               (mevedel-session-save-path session) session)))
          (when old-save-path
            (ignore-errors
              (mevedel-session-persistence-lock-release old-save-path)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil)
              (set-visited-file-name nil t)
              (setq-local kill-buffer-hook nil))
            (kill-buffer buffer)))
        (when (buffer-live-p view-buffer)
          (kill-buffer view-buffer))
        (when (file-directory-p local-root)
          (delete-directory local-root t))
        (mevedel-workspace-clear-registry)))))

(defun test-mevedel-session-persistence--make-fork-ready ()
  "Return a real saved session rewound and ready to fork.
The result is a plist whose :tempdir owns every created file."
  (mevedel-tools-register)
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buf (generate-new-buffer "*test-data-buf*"))
           (source-agent-buffer
            (generate-new-buffer " *fork-source-agent*"))
           (root-view-buffer
            (generate-new-buffer " *fork-root-view*"))
           (source-invocation
            (mevedel-agent-invocation--create
             :agent-id "eligible--1"
             :path "/root/eligible"
             :parent-session session
             :buffer source-agent-buffer))
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
        ;; Every path below is derived from this one: without it the writes
        ;; below are relative and land in the working tree.
        (should parent-path)
        (make-directory (file-name-concat parent-path "agents") t)
        (make-directory (file-name-concat parent-path "local" "plans") t)
        (write-region "# Parent plan\n" nil
                      (file-name-concat parent-path "local" "plans/current.md")
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
              '(:path "local/plans/current.md" :status presented))
        (setf (mevedel-session-prompt-index session)
              '((1 . ((:turn 1 :file-turn 1 :cum-turn 1)))
                (2 . ((:turn 1 :file-turn 1 :cum-turn 2
                       :fork-point-id "fixture-fork")))
                (3 . ((:turn 1 :file-turn 1 :cum-turn 3)))))
        (setf (mevedel-session-file-snapshots session)
              '((1 . (("/tmp/kept.el"
                       . (:backup-name "keep@v1" :version 1))))
                (3 . (("/tmp/future.el"
                       . (:backup-name "future@v2" :version 2))))))
        (setf (mevedel-session-agent-transcripts session)
              '(("eligible--1" :agent-path "/root/eligible"
                 :parent-turn 2 :type "default"
                 :description "Historical eligible agent"
                 :status completed :path "agents/eligible.chat.org")
                ("future--2" :agent-path "/root/future" :parent-turn 3
                 :path "agents/future.chat.org")
                ("poison--3" :agent-path "/root/poison" :parent-turn 2
                 :path "../poison.chat.org")))
        (setf (mevedel-session-agent-registry session)
              (list
               (cons
                "/root/eligible"
                (mevedel-agent-record--create
                 :id "eligible--1"
                 :path "/root/eligible"
                 :parent-path "/root"
                 :role "default"
                 :configuration
                 (test-mevedel-session-persistence--agent-configuration)
                 :activity 'idle
                 :conversation-location "agents/eligible.chat.org"
                 :conversation-buffer source-agent-buffer
                 :invocation source-invocation
                 :mailbox
                 (list
                  (list :type 'MAIL :sender "/root"
                        :recipient "/root/eligible"
                        :payload "Source-only mail"
                        :timestamp (current-time)))))))
        (setf (mevedel-session-agent-turn-capacity session) 7)
        (setf (mevedel-session-messages session)
              (list
               (list :type 'RESULT :sender "/root/eligible"
                     :recipient "/root" :outcome 'completed
                     :payload "Source-only result"
                     :timestamp (current-time))))
        (setf (mevedel-session-turn-count session) 3)
        (with-current-buffer source-agent-buffer
          (setq-local mevedel--session session)
          (setq-local mevedel--agent-invocation source-invocation))
        (mevedel-session-persistence-write
         (mevedel-session-persistence--sidecar-path parent-path)
         (mevedel-session-persistence--build-sidecar session buf))
        (with-temp-buffer
          (insert-file-contents
           (mevedel-session-persistence--segment-path parent-path 2))
          (goto-char (point-max))
          (insert
           (mevedel--format-hook-audit-record
            '(:type fork-point :fork-point-id "fixture-fork"
              :segment 2 :turn 1 :file-turn 1 :cum-turn 2
              :captured-file-turn 2)))
          (write-region nil nil
                        (mevedel-session-persistence--segment-path
                         parent-path 2)
                        nil 'silent))
        (mevedel-session-persistence--load-rewind-target
         session buf
         '(:segment 2 :fork-point-id "fixture-fork"
           :turn 1 :file-turn 1 :cum-turn 2))
        (setq-local mevedel--view-buffer root-view-buffer))
      (with-current-buffer root-view-buffer
        (setq-local mevedel--session session)
        (setq-local mevedel--data-buffer buf))
      (let* ((sessions-dir
              (mevedel-session-persistence--sessions-dir workspace))
             (parent-lock
              (mevedel-session-persistence--lock-path parent-path)))
        (list
         :workspace workspace
         :tempdir tempdir
         :session session
         :buffer buf
         :root-view-buffer root-view-buffer
         :sessions-dir sessions-dir
         :parent-id parent-id
         :parent-path parent-path
         :parent-lock parent-lock
         :parent-lock-state
         (mevedel-session-persistence--read-lock parent-lock)
         :eligible-transcript eligible-transcript
         :source-agent-record
         (cdr (assoc "/root/eligible"
                     (mevedel-session-agent-registry session)))
         :source-agent-buffer source-agent-buffer
         :source-invocation source-invocation
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
         :buffer-file-name (with-current-buffer buf buffer-file-name))))))

(defun test-mevedel-session-persistence--cleanup-fork-fixture (fixture)
  "Delete the real files and buffer owned by FIXTURE."
  (when-let ((buf (plist-get fixture :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)))
  (when-let ((agent-buffer (plist-get fixture :source-agent-buffer)))
    (when (buffer-live-p agent-buffer)
      (with-current-buffer agent-buffer
        (set-buffer-modified-p nil))
      (kill-buffer agent-buffer)))
  (when-let ((view-buffer (plist-get fixture :root-view-buffer)))
    (when (buffer-live-p view-buffer)
      (kill-buffer view-buffer)))
  (when-let ((tempdir (plist-get fixture :tempdir)))
    (when (file-directory-p tempdir)
      (delete-directory tempdir t)))
  (mevedel-workspace-clear-registry))

(mevedel-deftest mevedel-session-persistence--clone-session
  (:doc "covers every session slot and isolates both clone policies")
  (progn
    (should (= 90
             (length
              (cdr (cl-struct-slot-info 'mevedel-session)))))
    (should (mevedel-session-persistence--assert-clone-slot-completeness))
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
           :preset-settings '((clone-setting (:nested source)))
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
          (mevedel-session-persistence--clone-session
           source 'fork
           :save-path "/tmp/fork/"
           :session-id "fork"
           :created-at "fork-created"
           :updated-at "fork-updated"
           :current-segment 1
           :forked-from-session-id "source"
           :forked-from-turn 1))
         (save-as
          (mevedel-session-persistence--clone-session
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
                      mevedel-session-preset-settings
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

(mevedel-deftest mevedel-session-persistence--clone-session/fork ()
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
                        (mevedel-session-preset-settings session)
                        '((mevedel-test-setting base))
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
               (before (mevedel-session-persistence-serialize session))
               (before-text (prin1-to-string before))
               (child
                (mevedel-session-persistence--clone-session
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
                         (mevedel-session-persistence-serialize session)))
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
          (setcar (cdr (assq 'mevedel-test-setting
                             (mevedel-session-preset-settings child)))
                  'child)
          (should (equal '((mevedel-test-setting base))
                         (mevedel-session-preset-settings session)))
          (should (equal '((mevedel-test-setting child))
                         (mevedel-session-preset-settings child)))
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
            (mevedel-reminders--collect-from
             (mevedel-session-reminders child) 2 child)
            (mevedel-skills--maybe-activate child "/tmp/child.el")
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
                         (mevedel-session-persistence-serialize session)))
          (should (equal before-text
                         (prin1-to-string
                          (mevedel-session-persistence-serialize session)))))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(mevedel-deftest mevedel-session-persistence--fork-point-spans ()
  ,test
  (test)
  :doc "caches parsed fork points until transcript text changes"
  (with-temp-buffer
    (let ((calls 0))
      (cl-letf
          (((symbol-function 'mevedel-transcript-audit-spans)
            (lambda (&rest _)
              (cl-incf calls)
              nil)))
        (mevedel-session-persistence--fork-point-spans (current-buffer))
        (mevedel-session-persistence--fork-point-spans (current-buffer))
        (should (= 1 calls))
        (insert "changed")
        (mevedel-session-persistence--fork-point-spans (current-buffer))
        (should (= 2 calls))))))

(mevedel-deftest mevedel-session-persistence--assert-stable-source ()
  ,test
  (test)
  :doc "blocks every live owner and permits stable Goal and Plan states"
  (let ((session (mevedel-session--create :name "source"))
        (buffer (generate-new-buffer " *stable-source*"))
        execution-live
        agent-live)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--current-request nil))
          (cl-letf
              (((symbol-function 'mevedel-execution-session-live-p)
                (lambda (_) execution-live))
               ((symbol-function 'mevedel-agent-control-active-turn-p)
                (lambda (_) agent-live)))
            (setf (mevedel-session-pending-follow-ups session)
                  '((:input "later")))
            (should-error
             (mevedel-session-persistence--assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setf (mevedel-session-pending-follow-ups session) nil)
            (with-current-buffer buffer
              (setq-local mevedel--current-request t))
            (should-error
             (mevedel-session-persistence--assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (with-current-buffer buffer
              (setq-local mevedel--current-request nil))
            (setq execution-live t)
            (should-error
             (mevedel-session-persistence--assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setq execution-live nil
                  agent-live t)
            (should-error
             (mevedel-session-persistence--assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setq agent-live nil)
            (setf (mevedel-session-goal session)
                  (mevedel-goal--create :status 'active))
            (should-error
             (mevedel-session-persistence--assert-stable-source
              session buffer "forking")
             :type 'user-error)
            (setf (mevedel-goal-status (mevedel-session-goal session))
                  'paused
                  (mevedel-session-pending-plan-approval session)
                  '(:proposal stable))
            (should-not
             (mevedel-session-persistence--assert-stable-source
              session buffer "forking"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-session-persistence--materialize-fork-artifact ()
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
           (mevedel-session-persistence--materialize-fork-artifact
            source logical staging-path t))
          (should
           (equal "accepted bytes"
                  (mevedel-session-persistence--file-text
                   (expand-file-name logical staging-path))))
          (should-not
           (mevedel-session-persistence--materialize-fork-artifact
            source "plans/optional.md" staging-path))
          (should-error
           (mevedel-session-persistence--materialize-fork-artifact
            source "plans/required.md" staging-path t)))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

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
          (mevedel-session-persistence--clone-session
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
          (mevedel-session-persistence--stage-fork
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
           (mevedel-session-persistence--stage-fork
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
                        (mevedel-session-persistence--sidecar-path parent-path))
                       (sessions-dir
                        (mevedel-session-persistence--sessions-dir workspace))
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
               (mevedel-session-persistence--printed-value
                (mevedel-session-persistence--build-sidecar
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
                  (mevedel-session-persistence--clone-session
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
            (mevedel-session-persistence--stage-fork
             child source-buffer staging-buffer parent-path staging-path 2 2)
            (should
             (equal "published segment one"
                    (mevedel-session-persistence--file-text
                     (mevedel-session-persistence--segment-path
                      staging-path 1))))
            (should
             (equal "published accepted plan"
                    (mevedel-session-persistence--file-text
                     (expand-file-name plan-relative staging-path))))
            (should
             (equal "published backup bytes"
                    (mevedel-session-persistence--file-text
                     (expand-file-name backup-relative staging-path))))
            (should
             (equal "published agent transcript"
                    (mevedel-session-persistence--file-text
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

(mevedel-deftest mevedel-session-persistence--publish-fork ()
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
                    (mevedel-session-persistence--sessions-dir workspace))
                   (staging-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir ".fork-staging")))
                   (new-save-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir "fork-child")))
                   (child
                    (mevedel-session-persistence--clone-session
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
                   ((symbol-function 'mevedel-session-persistence--stage-fork)
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
                         (mevedel-session-persistence--segment-path
                          actual-staging-path 1)
                         nil 'silent)
                        (mevedel-session-persistence-write
                         (mevedel-session-persistence--sidecar-path
                          actual-staging-path)
                         (mevedel-session-persistence--build-sidecar
                          actual-child actual-staging-buffer)))))
                   ((symbol-function 'mevedel--probe-session-target) #'ignore)
                   ((symbol-function 'mevedel--chat-buffer-init-common)
                    #'ignore)
                   ((symbol-function
                     'mevedel-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'mevedel-session-persistence--load-instructions)
                    #'ignore))
                (setq child-buffer
                      (mevedel-session-persistence--publish-fork
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
                    (mevedel-session-persistence--sessions-dir workspace))
                   (staging-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir ".fork-failed-staging")))
                   (new-save-path
                    (file-name-as-directory
                     (file-name-concat sessions-dir "fork-failed-child")))
                   (child
                    (mevedel-session-persistence--clone-session
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
                  (((symbol-function 'mevedel-session-persistence--stage-fork)
                    (lambda (actual-child _buffer actual-staging-buffer
                              _parent-save-path actual-staging-path
                              _picked-segment _picked-cum-turn
                              &optional _additional-roots)
                      (with-current-buffer actual-staging-buffer
                        (setq-local mevedel--session actual-child)
                        (write-region
                         (point-min) (point-max)
                         (mevedel-session-persistence--segment-path
                          actual-staging-path 1)
                         nil 'silent)
                        (mevedel-session-persistence-write
                         (mevedel-session-persistence--sidecar-path
                          actual-staging-path)
                         (mevedel-session-persistence--build-sidecar
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
                 (mevedel-session-persistence--publish-fork
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

(mevedel-deftest mevedel-session-persistence-conversation-fork ()
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
                (mevedel-session-persistence--file-text
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
                (mevedel-session-persistence-serialize session))
          (cl-letf
              (((symbol-function 'mevedel--run-session-start-hooks)
                (lambda (source)
                  (push source lifecycle-sources)))
               ((symbol-function 'mevedel-model-apply-session-policy)
                #'ignore))
            (setq child-buffer
                  (mevedel-session-persistence-conversation-fork
                   (plist-get fixture :buffer)
                   '(:fork-point-id "fixture-fork"))))
          (should (buffer-live-p child-buffer))
          (should (equal '("fork") lifecycle-sources))
          (should (buffer-local-value 'mevedel--session child-buffer))
          (let* ((child
                  (buffer-local-value 'mevedel--session child-buffer))
                 (child-path (mevedel-session-save-path child))
                 (child-sidecar
                  (mevedel-session-persistence-read
                   (mevedel-session-persistence--sidecar-path child-path))))
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
                           (mevedel-session-persistence--file-text
                            source-file)))
            (with-current-buffer (plist-get fixture :buffer)
              (should (equal source-text (buffer-string)))
              (should (eq session mevedel--session)))
            (should (equal (plist-get fixture :parent-sidecar-text)
                           (mevedel-session-persistence--file-text
                            (mevedel-session-persistence--sidecar-path
                             (plist-get fixture :parent-path)))))
            (should (equal source-lock
                           (mevedel-session-persistence--file-text
                            (plist-get fixture :parent-lock))))
            (should (equal source-state
                           (mevedel-session-persistence-serialize session)))))
      (when (buffer-live-p child-buffer)
        (let ((view (buffer-local-value 'mevedel--view-buffer child-buffer)))
          (with-current-buffer child-buffer
            (set-buffer-modified-p nil))
          (when (buffer-live-p view)
            (kill-buffer view)))
        (when (buffer-live-p child-buffer)
          (kill-buffer child-buffer)))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(mevedel-deftest mevedel-session-persistence--retarget-worktree-state ()
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
               (mevedel-session-persistence--retarget-worktree-state
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

(mevedel-deftest mevedel-session-persistence--retarget-worktree-roots ()
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
          (mevedel-session-persistence--retarget-worktree-roots
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

(mevedel-deftest mevedel-session-persistence--assert-worktree-target ()
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
           (mevedel-session-persistence--assert-worktree-target
            worktree (file-name-concat worktree "safe.el")))
          (make-symbolic-link external
                              (file-name-concat worktree "escape"))
          (should-error
           (mevedel-session-persistence--assert-worktree-target
            worktree
            (file-name-concat worktree "escape" "unsafe.el"))))
      (delete-directory worktree t)
      (delete-directory external t))))

(mevedel-deftest mevedel-session-persistence--restore-worktree-files ()
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
                        (mevedel-file-history--backup-path
                         save-path "good@v1")
                        nil 'silent)
          (write-region "source good\n" nil good-source nil 'silent)
          (write-region "source bad\n" nil bad-source nil 'silent)
          (write-region "shared external\n" nil external-file nil 'silent)
          (write-region "HEAD bad\n" nil bad-target nil 'silent)
          (let ((report
                 (mevedel-session-persistence--restore-worktree-files
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
                           (mevedel-session-persistence--file-text
                            good-target)))
            (should (equal "HEAD bad\n"
                           (mevedel-session-persistence--file-text
                            bad-target)))
            (should (equal "source good\n"
                           (mevedel-session-persistence--file-text
                            good-source)))
            (should (equal "source bad\n"
                           (mevedel-session-persistence--file-text
                            bad-source)))
            (should (equal "shared external\n"
                           (mevedel-session-persistence--file-text
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
                        (mevedel-file-history--backup-path
                         save-path "valid@v1")
                        nil 'silent)
          (write-region "HEAD\n" nil valid-target nil 'silent)
          (should-error
           (mevedel-session-persistence--restore-worktree-files
            source child 1))
          (should (equal "HEAD\n"
                         (mevedel-session-persistence--file-text
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
         (mevedel-session-persistence--restore-worktree-files
          source child 1))
      (delete-directory source-root t)
      (delete-directory worktree-root t)
      (delete-directory save-path t))))

(mevedel-deftest mevedel-session-persistence--worktree-fork-disclosure ()
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
           (mevedel-session-persistence--worktree-fork-disclosure
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
          (mevedel-session-persistence--worktree-fork-disclosure
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

(mevedel-deftest mevedel-session-persistence-worktree-fork ()
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
                          (mevedel-file-history--backup-path
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
            (mevedel-session-persistence-write
             (mevedel-session-persistence--sidecar-path
              (plist-get fixture :parent-path))
             (mevedel-session-persistence--build-sidecar
              session (plist-get fixture :buffer)))
            (let ((source-state
                   (mevedel-session-persistence-serialize session))
                  (source-sidecar
                   (mevedel-session-persistence--file-text
                    (mevedel-session-persistence--sidecar-path
                     (plist-get fixture :parent-path))))
                  (source-lock
                   (mevedel-session-persistence--file-text
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
                      (mevedel-session-persistence-worktree-fork
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
                               (mevedel-session-persistence--file-text
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
                               (mevedel-session-persistence--file-text
                                source-file)))
                (should (equal "untracked Source file\n"
                               (mevedel-session-persistence--file-text
                                untracked-file)))
                (should (equal source-state
                               (mevedel-session-persistence-serialize
                                session)))
                (should
                 (equal source-sidecar
                        (mevedel-session-persistence--file-text
                         (mevedel-session-persistence--sidecar-path
                          (plist-get fixture :parent-path)))))
                (should
                 (equal source-lock
                        (mevedel-session-persistence--file-text
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
                  (mevedel-session-persistence-serialize session))
                 (first-error
                  (cl-letf
                      (((symbol-function
                         'mevedel-session-persistence--stage-fork)
                        (lambda (&rest _)
                          (error "Injected staging failure"))))
                    (should-error
                     (mevedel-session-persistence-worktree-fork
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
                           (mevedel-session-persistence-serialize session)))
            (cl-letf
                (((symbol-function 'mevedel-worktree-fork-reservation)
                  (lambda (&rest _)
                    (ert-fail "Retry allocated another reservation"))))
              (let ((retry-error
                     (should-error
                      (mevedel-session-persistence-worktree-fork
                       (plist-get fixture :buffer) target)
                      :type 'user-error)))
                (should (string-match-p
                         (regexp-quote branch)
                         (error-message-string retry-error)))))
            (should-not
             (file-exists-p
              (file-name-concat source-root ".worktrees" "main-fork-2")))))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(mevedel-deftest mevedel-session-persistence--worktree-fork-retained-error ()
  ,test
  (test)
  :doc "reports retained remote worktree paths in the target-native domain"
  (let* ((target (mevedel-execution-target-create
                  "/ssh:alias:/srv/repo/"))
         (session (mevedel-session--create :execution-target target))
         (text
          (mevedel-session-persistence--worktree-fork-retained-error
           session '(error "publish failed")
           '(:branch "worktree/fork"
             :directory "/ssh:alias:/srv/repo/.worktrees/fork/"
             :cleanup-command "git worktree remove"))))
    (should-not (string-match-p "/ssh:alias:" text))
    (should (string-match-p "/srv/repo/.worktrees/fork/" text))))

(mevedel-deftest mevedel-session-persistence--commit-remote-rename ()
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
                     (mevedel-session-persistence--commit-remote-rename
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
                            (mevedel-session-persistence-read-artifact
                             session "segment-0001.chat.org" t)))
                    (let ((sidecar
                           (with-temp-buffer
                             (insert
                              (mevedel-session-persistence-read-artifact
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
                       (mevedel-session-persistence--commit-remote-rename
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
            (symbol-function 'mevedel-session-persistence-publish-text))
           published)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "transcript\n")
            (mevedel-session-persistence-save session buffer)
            (cl-letf
                (((symbol-function
                   'mevedel-session-persistence-publish-text)
                  (lambda (actual-session path content &optional coding)
                    (push path published)
                    (funcall publish-function
                             actual-session path content coding))))
              (mevedel-rename-session "renamed"))
            (should
             (equal
              (list
               (mevedel-session-persistence--sidecar-path
                (mevedel-session-save-path session)))
              published)))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry)))))


;;
;;; Phase 10: resume / list / save commands

(mevedel-deftest mevedel-session-persistence--entry-action ()
  ,test
  (test)
  :doc "labels foreign and expired portable sessions without changing them"
  (let* ((root (make-temp-file "mevedel-entry-action-" t))
         (workspace
          (test-mevedel-session-persistence--make-workspace root))
         (entry '(:save-path "/session/")))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-durability-lease-state)
              (lambda (&rest _) 'foreign)))
          (should
           (equal "Join read-only"
                  (mevedel-session-persistence--entry-action
                   workspace entry)))
          (cl-letf
              (((symbol-function 'mevedel-session-durability-lease-state)
                (lambda (&rest _) 'expired)))
            (should
             (equal "Take over"
                    (mevedel-session-persistence--entry-action
                     workspace entry)))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-choose-entry ()
  ,test
  (test)
  :doc "offers safe new-session disclosure and restores the chosen session"
  (let* ((root (make-temp-file "mevedel-entry-choice-" t))
         (workspace
          (test-mevedel-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-name "main")))
         (choice "Start new session")
         (warned nil)
         (restored (generate-new-buffer " *mevedel-entry-restored*")))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-persistence-list-sessions)
              (lambda (&rest _) (list entry)))
             ((symbol-function 'mevedel-session-persistence--entry-action)
              (lambda (&rest _) "Join read-only"))
             ((symbol-function 'mevedel-session-durability-lease-state)
              (lambda (&rest _) 'owned))
             ((symbol-function
               'mevedel-session-persistence--format-session-candidate)
              (lambda (&rest _) "main"))
             ((symbol-function
               'mevedel-session-persistence--ordered-display-collection)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (&rest _) choice))
             ((symbol-function 'yes-or-no-p)
              (lambda (&rest _) (setq warned t)))
             ((symbol-function 'mevedel-session-persistence-restore)
              (lambda (path &rest _)
                (should (equal "/session/" path))
                restored)))
          (should
           (eq 'new
               (mevedel-session-persistence-choose-entry workspace)))
          (should warned)
          (setq choice "Join read-only — main")
          (should
           (eq restored
               (mevedel-session-persistence-choose-entry workspace))))
      (when (buffer-live-p restored)
        (kill-buffer restored))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))

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
              (mevedel-test--with-shifted-clock
                (with-current-buffer b1
                  (org-mode)
                  (insert "Hello\n")
                  (mevedel-session-persistence-save s1 b1))
                ;; Advance the stamps so `:updated-at' differs.
                (setq mevedel-test--timestamp-offset 2)
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
  :doc "remote listing omits nil or corrupt heads without reading transcript bytes"
  (let* ((host "list-publications")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-list-publications-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace _session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((sessions-dir
                    (mevedel-session-persistence--sessions-dir workspace))
                   (nil-head
                    (file-name-as-directory
                     (file-name-concat sessions-dir "nil-head")))
                   (corrupt-head
                    (file-name-as-directory
                     (file-name-concat sessions-dir "corrupt-head")))
                   (publication
                    (mevedel-session-publication-read session-dir))
                   (segment-entry
                    (cdr (assoc "segment-0001.chat.org"
                                (plist-get publication :artifacts))))
                   (manifest
                    (file-name-concat session-dir
                                      (plist-get publication :head))))
              ;; Listing eagerly verifies only the immutable sidecar, so a
              ;; corrupted transcript is diagnosed when consumed, not here.
              (write-region "corrupt transcript" nil
                            (plist-get segment-entry :published) nil 'silent)
              (make-directory nil-head t)
              (mevedel-session-persistence-write
               (mevedel-session-persistence--sidecar-path nil-head)
               (test-mevedel-session-persistence--complete-sidecar
                '(:session-id "nil-head" :session-name "nil-head")))
              (copy-directory session-dir corrupt-head nil t t)
              (let* ((corrupt-manifest
                      (file-name-concat corrupt-head
                                        (file-relative-name manifest session-dir)))
                     (value
                      (mevedel-session-persistence-read corrupt-manifest))
                     (entry (car (plist-get value :artifacts))))
                (setf (plist-get (cdr entry) :published) "../escape")
                (mevedel-session-persistence-write corrupt-manifest value))
              (let ((listed
                     (mevedel-session-persistence-list-sessions workspace)))
                (should (= 1 (length listed)))
                (should
                 (equal session-dir (plist-get (car listed) :save-path)))
                (should (plist-get (car listed) :publication))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "discovers a Save As child through alias B and resumes its full state"
  (let* ((alias-a "portable-resume-a")
         (alias-b "portable-resume-b")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-portable-resume-" t)))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list alias-a alias-b)
          (cl-destructuring-bind (_workspace-a session-a session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               alias-a local-root "Portable transcript\n")
            (let* ((root-a
                    (mevedel-workspace-root
                     (mevedel-session-workspace session-a)))
                   (identity
                    (mevedel-workspace-identity-read root-a))
                   (root-b
                    (format "/mevedelmock:%s:%s"
                            alias-b (file-name-as-directory local-root)))
                   session-id
                   (mevedel-session-durability--client-id
                    (make-string 64 ?b))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session-a))
               t mevedel-session-durability--disclosed-targets)
              (let ((buffer (generate-new-buffer " *save-as-alias*")))
                (unwind-protect
                    (progn
                      (should
                       (mevedel-session-durability-lease-acquire
                        session-dir "save-as-alias" session-a))
                      (with-current-buffer buffer
                        (org-mode)
                        (setq-local mevedel--session session-a
                                    buffer-file-name segment)
                        (insert "Portable transcript\n")
                        (cl-letf (((symbol-function 'read-string)
                                   (lambda (&rest _) "alias-clone")))
                          (mevedel-save-session t)))
                      (setq session-id
                            (mevedel-session-session-id session-a))
                      (mevedel-session-persistence-lock-release
                       (mevedel-session-save-path session-a) session-a))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (set-buffer-modified-p nil)
                      (set-visited-file-name nil t)
                      (setq-local kill-buffer-hook nil))
                    (kill-buffer buffer))))
              (mevedel-workspace-clear-registry)
              (let* ((workspace-b
                      (mevedel-workspace-get-or-create
                       'project root-b root-b "portable-resume"))
                     (target-b (mevedel-execution-target-create root-b))
                     (listed
                      (mevedel-session-persistence-list-sessions workspace-b))
                     (child
                      (cl-find
                       session-id listed
                       :key (lambda (entry)
                              (plist-get (plist-get entry :summary)
                                         :session-id))
                       :test #'equal)))
                (should
                 (equal identity
                        (mevedel-workspace-identity-read root-b)))
                (should (= 2 (length listed)))
                (should child)
                (should
                 (string-prefix-p root-b
                                  (plist-get child :save-path)))
                (puthash
                 (mevedel-execution-target-identity target-b)
                 t mevedel-session-durability--disclosed-targets)
                (cl-letf
                    (((symbol-function 'mevedel--chat-buffer-init-common)
                      #'ignore)
                     ((symbol-function
                       'mevedel-agent-persistence-restore-tree)
                      (lambda (&rest _) 0))
                     ((symbol-function
                       'mevedel-session-persistence--load-instructions)
                      #'ignore))
                  (setq restored
                        (mevedel-session-persistence-resume-id
                         workspace-b session-id)))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (string-match-p
                           "Portable transcript" (buffer-string)))
                  (should
                   (eq workspace-b
                       (mevedel-session-workspace mevedel--session)))
                  (should
                   (equal root-b
                          (mevedel-workspace-root
                           (mevedel-session-workspace mevedel--session))))
                  (should
                   (equal root-b
                          (mevedel-session-working-directory
                           mevedel--session))))))))
      (when (buffer-live-p restored)
        (mevedel-test--with-local-shell-tramp (list alias-a alias-b)
          (let ((session (buffer-local-value 'mevedel--session restored)))
            (when (and session (mevedel-session-save-path session))
              (mevedel-session-persistence-lock-release
               (mevedel-session-save-path session) session))
            (with-current-buffer restored
              (set-buffer-modified-p nil))
            (kill-buffer restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
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

(mevedel-deftest mevedel-session-persistence-conversation-variants ()
  ,test
  (test)
  :doc "finds the persisted Source and one direct Child from either variant"
  (let* ((root (make-temp-file "mevedel-variants-" t))
         (workspace
          (test-mevedel-session-persistence--make-workspace root))
         (source (mevedel-session-create "source" workspace))
         (child (mevedel-session-create "child" workspace))
         (source-entry
          (copy-tree
           '(:save-path "/sessions/source/"
             :summary (:session-id "source-id"
                       :session-name "source"
                       :created-at "2026-07-01T10:00:00+0200"
                       :fork-point-ids ("fork-point-1")
                       :working-directory "/repo/"))))
         (child-entry
          '(:save-path "/sessions/child/"
            :summary (:session-id "child-id"
                      :session-name "child"
                      :created-at "2026-07-01T10:01:00+0200"
                      :fork-point-ids ("fork-point-1" "later-point")
                      :working-directory "/repo/"
                      :forked-from-session-id "source-id"
                      :forked-from-fork-point-id "fork-point-1"
                      :fork-type conversation)))
         (worktree-entry
          '(:save-path "/sessions/worktree/"
            :summary (:session-id "worktree-id"
                      :session-name "worktree"
                      :created-at "2026-07-01T10:02:00+0200"
                      :fork-point-ids ("fork-point-1")
                      :working-directory "/repo/.worktrees/fork/"
                      :forked-from-session-id "source-id"
                      :forked-from-fork-point-id "fork-point-1"
                      :fork-type worktree)))
         (grandchild-entry
          '(:save-path "/sessions/grandchild/"
            :summary (:session-id "grandchild-id"
                      :session-name "grandchild"
                      :created-at "2026-07-01T10:03:00+0200"
                      :fork-point-ids ("later-point")
                      :working-directory "/repo/"
                      :forked-from-session-id "child-id"
                      :forked-from-fork-point-id "later-point"
                      :fork-type conversation)))
         (entries
          (list grandchild-entry worktree-entry child-entry source-entry)))
    (unwind-protect
        (progn
          (setf (mevedel-session-session-id source) "source-id"
                (mevedel-session-save-path source) "/sessions/source/"
                (mevedel-session-session-id child) "child-id"
                (mevedel-session-save-path child) "/sessions/child/"
                (mevedel-session-forked-from-session-id child) "source-id"
                (mevedel-session-forked-from-fork-point-id child)
                "fork-point-1"
                (mevedel-session-fork-type child) 'conversation)
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-list-sessions)
                (lambda (_workspace) entries)))
            (dolist (session (list source child))
              (let ((variants
                     (mevedel-session-persistence-conversation-variants
                      session "fork-point-1")))
                (should (equal '("source-id" "child-id" "worktree-id")
                               (mapcar
                                (lambda (entry)
                                  (plist-get
                                   (plist-get entry :summary)
                                   :session-id))
                                variants)))
                (should (equal '(source conversation worktree)
                               (mapcar
                                (lambda (entry)
                                  (plist-get entry :variant-origin))
                                variants)))))
            ;; Forking a later Child response makes that Child the Source of
            ;; an independent group; inherited lineage is not flattened.
            (let ((later
                   (mevedel-session-persistence-conversation-variants
                    child "later-point")))
              (should
               (equal '("child-id" "grandchild-id")
                      (mapcar
                       (lambda (entry)
                         (plist-get (plist-get entry :summary) :session-id))
                       later)))
              (should
               (equal '(source conversation)
                      (mapcar
                       (lambda (entry)
                         (plist-get entry :variant-origin))
                       later))))
            ;; Rewind detaches Source by removing the stable point, but the
            ;; surviving direct Children remain a sibling group.
            (plist-put (plist-get source-entry :summary)
                       :fork-point-ids '("different-point"))
            (should
             (equal '("child-id" "worktree-id")
                    (mapcar
                     (lambda (entry)
                       (plist-get (plist-get entry :summary) :session-id))
                     (mevedel-session-persistence-conversation-variants
                      child "fork-point-1"))))
            ;; Removing a sibling removes only that entry and the affordance
            ;; naturally disappears when the current session is alone.
            (setq entries (list child-entry))
            (should
             (equal '("child-id")
                    (mapcar
                     (lambda (entry)
                       (plist-get (plist-get entry :summary) :session-id))
                     (mevedel-session-persistence-conversation-variants
                      child "fork-point-1"))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-persistence-choose-conversation-variant ()
  ,test
  (test)
  :doc "shows stable rich entries and marks the current variant without moving it"
  (let* ((source
          '(:save-path "/sessions/source/"
            :variant-origin source
            :summary (:session-id "source-id"
                      :session-name "source"
                      :working-directory "/repo/"
                      :latest-user-message "original prompt")))
         (conversation
          '(:save-path "/sessions/conversation/"
            :variant-origin conversation
            :summary (:session-id "conversation-id"
                      :session-name "conversation"
                      :working-directory "/repo/"
                      :latest-user-message "shared prompt")))
         (worktree
          '(:save-path "/sessions/worktree/"
            :variant-origin worktree
            :summary (:session-id "worktree-id"
                      :session-name "worktree"
                      :working-directory "/replacement/"
                      :latest-user-message "isolated prompt"
                      :worktree-directory "/missing-worktree/"
                      :worktree-branch "worktree/source-fork-1")))
         displays)
    (cl-letf
        (((symbol-function 'completing-read)
          (lambda (_prompt collection &rest _)
            (setq displays (all-completions "" collection))
            (car (last displays)))))
      (should
       (eq worktree
           (mevedel-session-persistence-choose-conversation-variant
            (list source conversation worktree) "worktree-id"))))
    (should (string-prefix-p "  Source" (nth 0 displays)))
    (should (string-match-p "/repo/" (nth 0 displays)))
    (should (string-prefix-p "  Conversation" (nth 1 displays)))
    (should (string-match-p "shared files" (nth 1 displays)))
    (should (string-match-p "shared prompt" (nth 1 displays)))
    (should (string-prefix-p "* Worktree" (nth 2 displays)))
    (should (string-match-p "worktree/source-fork-1" (nth 2 displays)))
    (should (string-match-p "retargeted; original missing"
                            (nth 2 displays)))
    (should (string-match-p "isolated prompt" (nth 2 displays)))
    (plist-put (plist-get conversation :summary)
               :working-directory "/other/")
    (cl-letf
        (((symbol-function 'completing-read)
          (lambda (_prompt collection &rest _)
            (setq displays (all-completions "" collection))
            (car displays))))
      (mevedel-session-persistence-choose-conversation-variant
       (list source conversation worktree) "source-id"))
    (should (string-match-p "independent directory" (nth 1 displays)))))

(mevedel-deftest mevedel-session-persistence--read-summary ()
  ,test
  (test)
  :doc "reuses unchanged sidecars and refreshes atomically replaced files"
  (let ((tmp (make-temp-file "mevedel-summary-cache-" nil ".el"))
        (mevedel-session-persistence--summary-cache
         (make-hash-table :test #'equal))
        (read-function
         (symbol-function 'mevedel-session-persistence-read))
        (read-count 0))
    (unwind-protect
        (cl-labels
            ((write-sidecar
              (name)
              (mevedel-session-persistence-write
               tmp
               (test-mevedel-session-persistence--complete-sidecar
                `(:session-name ,name :session-id "cache-test")))))
          (cl-letf
              (((symbol-function 'mevedel-session-persistence-read)
                (lambda (path)
                  (cl-incf read-count)
                  (funcall read-function path))))
            (write-sidecar "first")
            (should
             (equal "first"
                    (plist-get
                     (mevedel-session-persistence--read-summary tmp)
                     :session-name)))
            (mevedel-session-persistence--read-summary tmp)
            (should (= 1 read-count))
            (write-sidecar "second")
            (should
             (equal "second"
                    (plist-get
                     (mevedel-session-persistence--read-summary tmp)
                     :session-name)))
            (should (= 2 read-count))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "reuses a cached failure for an unchanged invalid sidecar"
  (let ((tmp (make-temp-file "mevedel-summary-cache-invalid-" nil ".el"))
        (mevedel-session-persistence--summary-cache
         (make-hash-table :test #'equal))
        (read-function
         (symbol-function 'mevedel-session-persistence-read))
        (read-count 0))
    (unwind-protect
        (progn
          (write-region "invalid" nil tmp nil 'silent)
          (cl-letf
              (((symbol-function 'mevedel-session-persistence-read)
                (lambda (path)
                  (cl-incf read-count)
                  (funcall read-function path))))
            (should-not
             (mevedel-session-persistence--read-summary tmp))
            (should-not
             (mevedel-session-persistence--read-summary tmp))
            (should (= 1 read-count))))
      (when (file-exists-p tmp) (delete-file tmp))))
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
              :latest-user-message "Latest"
              :prompt-index
              ((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                      :fork-point-id "fork-point-1")
                     (:turn 2 :file-turn 2 :cum-turn 2)
                     (:turn 3 :file-turn 3 :cum-turn 3
                      :fork-point-id "fork-point-2")))))))
          (let ((s (mevedel-session-persistence--read-summary tmp)))
            (should (equal "demo" (plist-get s :session-name)))
            (should (equal "demo-1234" (plist-get s :session-id)))
            (should (equal "Hello" (plist-get s :first-user-message)))
            (should (equal "Latest" (plist-get s :latest-user-message)))
            (should (equal '("fork-point-1" "fork-point-2")
                           (plist-get s :fork-point-ids)))))
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
     session '(:type project :workspace-id "id"
               :target-native-root "/old/root/" :name "ws"))
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
     session '(:type project :workspace-id "id2"
               :target-native-root "/same/root/" :name "ws"))
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
     session '(:type project :workspace-id "id3"
               :target-native-root "/old/root/" :name "ws"))
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
      (mevedel-workspace-clear-registry)))
  :doc "returns an unmodified predecessor when finalization is deferred"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-selfheal-deferred-" t))))
    (unwind-protect
        (let* ((session
                (mevedel-session-create
                 "x"
                 (mevedel-workspace-get-or-create
                  'project "deferred-id" "/" "x")))
               (segment-1
                (file-name-concat tempdir "segment-0001.chat.org")))
          (setf (mevedel-session-current-segment session) 1)
          (write-region "* Chat\n" nil segment-1 nil 'silent)
          (write-region
           "* Current\n" nil
           (file-name-concat tempdir "segment-0002.chat.org") nil 'silent)
          (cl-letf (((symbol-function 'display-warning) #'ignore))
            (should
             (equal
              segment-1
              (mevedel-session-persistence--self-heal-segment-counter
               session tempdir t))))
          (with-temp-buffer
            (insert-file-contents segment-1)
            (should-not
             (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
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
                 (mevedel-session-save-path s1) s1)
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path s2) s2)
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
  :doc "deletes expired sessions with obsolete sidecars"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "obsolete" workspace))
               (buf (generate-new-buffer "*test-obsolete-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old\n")
                  (mevedel-session-persistence-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path save-path))
                       (plist (mevedel-session-persistence-read sidecar))
                       (forged
                        (format-time-string
                         "%FT%H-%M-%S"
                         (time-subtract (current-time) (* 14 24 60 60)))))
                  (cl-remf plist :plan-mode)
                  (plist-put plist :updated-at forged)
                  (mevedel-session-persistence-write sidecar plist)
                  (mevedel-session-persistence-lock-release save-path session)
                  (should (= 1
                             (mevedel-session-persistence-cleanup-expired
                              workspace t)))
                  (should-not (file-directory-p save-path))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "deletes expired sessions without sidecars"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "missing" workspace))
               (buf (generate-new-buffer "*test-missing-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old\n")
                  (mevedel-session-persistence-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path save-path))
                       (old-time
                        (time-subtract (current-time) (* 14 24 60 60))))
                  (mevedel-session-persistence-lock-release save-path session)
                  (delete-file sidecar)
                  (set-file-times save-path old-time)
                  (should (= 1
                             (mevedel-session-persistence-cleanup-expired
                              workspace t)))
                  (should-not (file-directory-p save-path))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
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
  :doc "never deletes expired remote sessions with an active lease"
  (let* ((host "cleanup-remote-lease-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-cleanup-remote-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((mevedel-session-max-age-days 7)
                  (mevedel-session-persistence--cleanup-throttle
                   (make-hash-table :test #'equal))
                  (mevedel-session-durability--client-id
                   (make-string 64 ?a))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (let* ((sidecar-path
                      (mevedel-session-persistence--sidecar-path session-dir))
                     (sidecar
                      (mevedel-session-persistence-read sidecar-path)))
                (plist-put
                 sidecar :updated-at
                 (format-time-string
                  "%FT%H-%M-%S"
                  (time-subtract (current-time) (* 30 24 60 60))))
                (mevedel-session-persistence-write sidecar-path sidecar))
              (unwind-protect
                  (progn
                    (should
                     (mevedel-session-durability-lease-acquire
                      session-dir "*cleanup-owner*" session))
                    (should-not
                     (mevedel-session-persistence-cleanup-expired
                      workspace t))
                    (should (file-directory-p session-dir))
                    (should
                     (mevedel-session-durability-lease-owned-p session)))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
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
    (setf (mevedel-session-permission-mode session) 'edits)
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
                      :snapshot-p t
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
                         (pre-backup-name
                          (plist-get (cdr file-entry) :pre-backup-name))
                         (backup-path (mevedel-file-history--backup-path
                                       (mevedel-session-save-path session)
                                       backup-name))
                         (pre-backup-path
                          (mevedel-file-history--backup-path
                           (mevedel-session-save-path session)
                           pre-backup-name)))
                    (should snaps)
                    (should backup-name)
                    (should pre-backup-name)
                    (should (file-exists-p backup-path))
                    (should (file-exists-p pre-backup-path))
                    (with-temp-buffer
                      (insert-file-contents-literally backup-path)
                      (should (equal "MODIFIED\n" (buffer-string))))
                    (with-temp-buffer
                      (insert-file-contents-literally pre-backup-path)
                      (should (equal "ORIGINAL\n" (buffer-string))))))
              (mevedel-request-end))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; View rerender on resume / rewind

(mevedel-deftest mevedel-session-persistence--hydrate-restored-buffer ()
  ,test
  (test)
  :doc "plants session state after Org setup and before transcript restoration"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-restore-hydrate-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace root))
         (buffer (generate-new-buffer " *mevedel-restore-hydrate*"))
         (segment-path (file-name-concat root "segment-0001.chat.org"))
         events)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-transcript-restore-gptel-state)
                   (lambda ()
                     (should (derived-mode-p 'org-mode))
                     (should (eq mevedel--session session))
                     (should (eq mevedel--workspace workspace))
                     (push 'transcript events)))
                  ((symbol-function
                    'mevedel-pipeline-reconcile-lost-executions)
                   (lambda (_buffer) 0))
                  ((symbol-function
                    'mevedel-session-persistence--check-target-incarnation)
                   (lambda (checked-session checked-buffer)
                     (should (eq checked-session session))
                     (should (eq checked-buffer buffer))
                     (push 'incarnation events)))
                  ((symbol-function 'mevedel--chat-buffer-init-common)
                   (lambda (_buffer _workspace source &optional inspection-p)
                     (should (equal source "resume"))
                     (should-not inspection-p)
                     (push 'chat events)))
                  ((symbol-function
                    'mevedel-agent-persistence-restore-tree)
                   (lambda (_session _buffer read-only-p)
                     (should-not read-only-p)
                     (push 'agents events)
                     2))
                  ((symbol-function
                    'mevedel-session-persistence--load-instructions)
                   (lambda (_session _buffer)
                     (push 'instructions events))))
          (should
           (= 2
              (mevedel-session-persistence--hydrate-restored-buffer
               buffer session workspace segment-path t nil nil)))
          (should (equal '(transcript incarnation chat agents instructions)
                         (nreverse events)))
          (with-current-buffer buffer
            (should (equal root default-directory))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--finish-restored-buffer ()
  ,test
  (test)
  :doc "persists repairs and loads history before rendering a fresh buffer"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-restore-finish-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "main" workspace root))
         (buffer (generate-new-buffer " *mevedel-restore-finish-data*"))
         observer
         events)
    (setf (mevedel-session-save-path session) root)
    (unwind-protect
        (progn
          (setq observer
                (mevedel-session-control-transfer-register-observer
                 session
                 (lambda (event &rest _args)
                   (pcase event
                     ('load-history (push 'history events))
                     ('rerender (push 'render events))))))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--build-sidecar)
                     (lambda (_session _buffer) '(:sidecar t)))
                    ((symbol-function 'mevedel-session-persistence-write)
                     (lambda (_path _sidecar) (push 'write events))))
            (should
             (eq buffer
                 (mevedel-session-persistence--finish-restored-buffer
                  buffer session nil t))))
          (should (equal '(write history render) (nreverse events))))
      (mevedel-session-control-transfer-unregister-observer session observer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence--find-live-buffer ()
  ,test
  (test)
  :doc "finds only root data buffers, never agent or view projections"
  (let* ((session (mevedel-session--create
                   :name "source" :session-id "source-id"))
         (root-buffer (generate-new-buffer " *source-root*"))
         (view-buffer (generate-new-buffer " *source-view*"))
         (agent-buffer (generate-new-buffer " *source-agent*")))
    (unwind-protect
        (progn
          (with-current-buffer root-buffer
            (setq-local mevedel--session session))
          (with-current-buffer view-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--data-buffer root-buffer))
          (with-current-buffer agent-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation t))
          (mevedel-session-set-root-buffer session root-buffer)
          (should (eq root-buffer
                      (mevedel-session-persistence--find-live-buffer
                       "source-id" " *source-root*")))
          (kill-buffer root-buffer)
          (should-not
           (mevedel-session-persistence--find-live-buffer
            "source-id" " *source-root*")))
      (dolist (buffer (list agent-buffer view-buffer root-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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
  :doc "save path does not rebuild the visible transcript"
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
                (should (= rerender-count 0)))
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
      (mevedel-workspace-clear-registry))))

;;
;;; Sidecar missing / unreadable fallback on restore

(mevedel-deftest mevedel-session-persistence/sidecar-missing-on-restore ()
  ,test
  (test)
  :doc "deleted sidecar fails closed without a committed authority profile"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               session-dir)
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
                (should-error
                 (mevedel-session-persistence-restore session-dir)
                 :type 'error))
            (ignore-errors
              (test-mevedel-session-persistence--release-and-kill
               buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))
  :doc "corrupt sidecar fails closed instead of synthesizing authority"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               session-dir)
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
                (should-error
                 (mevedel-session-persistence-restore session-dir)
                 :type 'error))
            (ignore-errors
              (test-mevedel-session-persistence--release-and-kill
               buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))))


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
              (mevedel-test--with-shifted-clock
                (with-current-buffer b1
                  (org-mode)
                  (setq-local mevedel--session s1)
                  (insert "session one\n")
                  (mevedel-session-persistence-save s1 b1))
                ;; Advance the stamps so the derived session ids differ.
                (setq mevedel-test--timestamp-offset 120)
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

(mevedel-deftest mevedel-session-persistence--write-sidecar-now ()
  ,test
  (test)
  :doc "commits a remote sidecar when its fixed cache is missing"
  (let* ((host "write-sidecar-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-write-sidecar-remote-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host local-root "sidecar transcript\n"))
                       (sidecar
                        (mevedel-session-persistence--sidecar-path session-dir))
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
                    session-dir "*write-sidecar*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir)
                        (mevedel-session-name session) "updated")
                  (setq buffer
                        (generate-new-buffer " *write-sidecar-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "sidecar transcript\n"))
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (delete-file sidecar)
                    (should
                     (mevedel-session-persistence--write-sidecar-now
                      session buffer))
                    (should-not
                     (equal head-before
                            (plist-get
                             (mevedel-session-publication session) :head)))
                    (should
                     (equal
                      "updated"
                      (plist-get
                       (with-temp-buffer
                         (insert
                          (mevedel-session-persistence-read-artifact
                           session "session.meta.el" t))
                         (goto-char (point-min))
                         (read (current-buffer)))
                       :session-name)))))
              (when session
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-persistence-save-agent-state ()
  ,test
  (test)
  :doc "writes through the exact root segment buffer and ignores agent buffers"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-agent-state-" t)))
         (workspace
          (mevedel-workspace--create
           :type 'project :id "agent-state" :root tempdir
           :name "agent-state"))
         (session (mevedel-session-create "main" workspace))
         (root (generate-new-buffer " *agent-state-root*"))
         (agent (generate-new-buffer " *agent-state-child*"))
         calls)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (with-current-buffer root
            (setq-local mevedel--session session)
            (setq-local buffer-file-name
                        (file-name-concat tempdir
                                          "segment-0001.chat.org")))
          (with-current-buffer agent
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create)))
          (mevedel-session-control-transfer-register-root-buffer
           session root)
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (seen-session seen-buffer)
                       (setq calls (list seen-session seen-buffer))
                       t)))
            (should
             (mevedel-session-persistence-save-agent-state session)))
          (should (eq session (car calls)))
          (should (eq root (cadr calls))))
      (when (buffer-live-p root) (kill-buffer root))
      (when (buffer-live-p agent) (kill-buffer agent))
      (delete-directory tempdir t))))


(mevedel-deftest mevedel-session-persistence-resume-id
  (:doc "resumes an exact persisted session id and reports unavailable ids")
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-resume-id-" t)))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "resume-id"))
         (session-id "main-2026-08-02T12-00-abcd")
         (session-dir
          (file-name-concat root ".mevedel" "sessions" session-id))
         restored)
    (unwind-protect
        (progn
          (make-directory session-dir t)
          (cl-letf (((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (path &rest _)
                       (setq restored path)
                       'restored-buffer)))
            (should (eq 'restored-buffer
                        (mevedel-session-persistence-resume-id
                         workspace session-id)))
            (should (equal (file-name-as-directory session-dir)
                           restored))
            (should-not
             (mevedel-session-persistence-resume-id workspace "missing"))
            (should-error
             (mevedel-session-persistence-resume-id workspace "../escape"))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-persistence--directive-capture-gaps
  (:doc "reports untracked effects belonging to discarded directive attempts")
  (let* ((workspace
          (mevedel-workspace--create
           :type 'file :id "capture-gaps" :root "/tmp" :name "capture-gaps"))
         (session (mevedel-session-create "main" workspace))
         (attempt
          (mevedel-directive-attempt--create
           :checkpoint '(:session-id "session" :turn 3)
           :untracked-effects '(("Bash" . "untracked command effects"))))
         (directive
          (mevedel-directive--create :id "directive" :attempts (list attempt))))
    (setf (mevedel-session-session-id session) "session")
    (mevedel-workspace-set-directives workspace (list directive))
    (should
     (equal '((:path "Directive directive via Bash"
               :reason "untracked command effects"))
            (mevedel-session-persistence--directive-capture-gaps session 3)))
    (should-not
     (mevedel-session-persistence--directive-capture-gaps session 4))))

(provide 'test-mevedel-session-persistence)

;;; test-mevedel-session-persistence.el ends here
