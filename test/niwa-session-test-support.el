;;; niwa-session-test-support.el --- Session test support -*- lexical-binding: t -*-

;;; Commentary:

;; Shared setup and real-file fixtures for session owner tests.

;;; Code:

(require 'niwa)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'niwa-presets)
(require 'niwa-plan)
(require 'niwa-skills-ui)
(require 'niwa-reminders)
(require 'niwa-resource)
(require 'niwa-view)
(require 'niwa-view-history)
(require 'niwa-chat)
(require 'niwa-execution-target)
(require 'niwa-hooks)
(require 'niwa-permission-log)
(require 'niwa-prompt-submission)
(require 'niwa-session-durability)
(require 'niwa-session-recovery)
(require 'niwa-session-transfer)
(require 'niwa-session-publication)
(require 'niwa-session-save-as)
(require 'niwa-session-persistence)
(require 'niwa-session-control-transfer)
(require 'niwa-tool-repair)
(require 'niwa-tool-render-data)
(require 'niwa-tools)
(require 'niwa-worktree)
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

;; `niwa-permissions'
(declare-function niwa-permission-add-session-resource-grant
                  "niwa-permissions"
                  (session path access &optional recursive))

;; `niwa-session-persistence'
(defvar niwa-session-persistence--summary-cache)

;; `niwa-skills-core'
(declare-function niwa-skills-maybe-activate
                  "niwa-skills-core" (session path))

;; `niwa-tools'
(declare-function niwa-tools--handle-message-inject "niwa-tools" (fsm))

;; `org'
(declare-function org-entry-delete "org" (pom property))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (pom property value))

;; `so-long'
(defvar so-long-predicate)

;;
;;; Helpers

(defun test-niwa-session-persistence--git (directory &rest args)
  "Run Git ARGS in DIRECTORY and return its trimmed output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (eq 0 (apply #'process-file "git" nil t nil args))
        (error "Git failed: %s" (buffer-string)))
      (string-trim (buffer-string)))))


(defun test-niwa-session-persistence--agent-backend ()
  "Return the registered backend used by retained-agent fixtures."
  (let ((name "Session Persistence Agent Test"))
    (condition-case nil
        (gptel-get-backend name)
      (user-error
       (let ((backend (gptel--make-backend :name name)))
         (setf (gptel-get-backend name) backend)
         backend)))))


(defun test-niwa-session-persistence--make-workspace (root)
  "Build a workspace struct registered in the global registry.
ROOT is a temporary directory owned and cleaned up by the caller."
  (niwa-workspace-clear-registry)
  (make-directory (file-name-concat root "packages" "api") t)
  (let ((workspace
         (niwa-workspace-get-or-create
          'project "test-id" root
          (file-name-nondirectory (directory-file-name root)))))
    (niwa-workspace-identity-ensure root)
    workspace))


(defun test-niwa-session-persistence--make-file-workspace (root)
  "Build a portable file-workspace authority rooted at ROOT."
  (niwa-workspace-clear-registry)
  (niwa-workspace-get-or-create
   'file
   (file-name-nondirectory (directory-file-name root))
   root
   (file-name-nondirectory (directory-file-name root))))


(defun test-niwa-session-persistence--lease-fixture (session &optional artifacts)
  "Lease SESSION's existing save path and publish optional ARTIFACTS with a marker.
Return SESSION.  The caller releases its lease during cleanup."
  (when (and artifacts
             (not (cl-some (lambda (item) (plist-get item :commit-marker)) artifacts)))
    (error "Fixture artifacts require a sidecar commit marker"))
  (setf (niwa-session-authority-mode session) 'portable)
  (unless (niwa-session-durability-lease-acquire
           (niwa-session-save-path session) "*lease-fixture*" session)
    (error "Could not acquire fixture lease"))
  (when artifacts
    (niwa-session-publication-publish session artifacts t))
  session)


(defun test-niwa-session-persistence--leased-session (directory &rest slots)
  "Build a real file-workspace session at DIRECTORY with caller SLOTS.
The caller releases the lease, removes DIRECTORY and clears the workspace registry."
  (let* ((root (file-name-as-directory directory))
         (workspace (test-niwa-session-persistence--make-file-workspace root)))
    (niwa-workspace-identity-ensure root)
    (test-niwa-session-persistence--lease-fixture
     (apply #'niwa-session--create
            (append slots
                    (list :workspace workspace :authority-mode 'portable
                          :execution-target (niwa-execution-target-create root)
                          :working-directory root :save-path root
                          :name "fixture" :naming-state 'explicit
                          :session-id (niwa-session-generate-id)))))))


(defun test-niwa-session-persistence--execution-tool-block
    (tool-use-id render-data)
  "Return a tool block owned by TOOL-USE-ID with RENDER-DATA."
  (propertize
   (concat "#+begin_tool (Bash :command \"true\")\n"
           "(:name \"Bash\" :args (:command \"true\"))\n\n"
           (niwa-tool-render-data-format
            render-data tool-use-id)
           "#+end_tool\n")
   'gptel (cons 'tool tool-use-id)))


(defun test-niwa-session-persistence--make-session (root)
  "Build a populated session for ROOT in round-trip cases."
  (let* ((workspace (test-niwa-session-persistence--make-workspace root))
         (root (niwa-workspace-root workspace))
         (session   (niwa-session-create "main" workspace)))
    (setf (niwa-session-working-directory session)
          (file-name-as-directory
           (file-name-concat root "packages" "api")))
    (setf (niwa-session-permission-mode session) 'ask)
    (setf (niwa-session-sandbox-mode session) 'required)
    (setf (niwa-session-permission-rules session)
          '(("Read"  :path "/tmp/foo/**" :action allow)
            ("Bash"  :pattern "git log*" :action allow)
            ("Write" :path "/tmp/bar"    :action deny)))
    (setf (niwa-session-resource-grants session)
          '((:path "/tmp/exact-read" :access read)
            (:path "/tmp/exact-write" :access write)))
    (setf (niwa-session-turn-count session) 5)
    (setf (niwa-session-last-task-write-turn session) 4)
    (setf (niwa-session-task-status-notes session)
          '((nil :note "Main status" :updated-turn 4
                 :updated-at "2026-04-23T18:20:00+0200")
            ("main" :note "Agent status" :updated-turn 4
             :updated-at "2026-04-23T18:21:00+0200")))
    (setf (niwa-session-workspace-instruction-hashes session)
          (list (cons (list "/root" (file-name-concat root "AGENTS.md"))
                      (make-string 64 ?a))
                (cons (list "/root" (file-name-concat root "nested" "AGENTS.md")) nil)))
    (setf (niwa-session-session-id session) "main-2026-04-23T14-30-a9f2")
    (setf (niwa-session-save-path session)
          (file-name-as-directory
           (file-name-concat
            root ".niwa" "sessions" "main-2026-04-23T14-30-a9f2")))
    (setf (niwa-session-created-at session) "2026-04-23T14-30-00")
    (setf (niwa-session-updated-at session) "2026-04-23T18-22-11")
    (setf (niwa-session-current-segment session) 2)
    (setf (niwa-session-prompt-index session)
          '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                   :pos 142 :preview "Refactor X" :timestamp "...")))))
    (setf (niwa-session-file-snapshots session)
          '((1 . (("/tmp/foo.el"
                   . (:backup-name "abc@v1" :version 1
                      :backup-time "..." :file-mtime "..."))))))
    (setf (niwa-session-tasks session)
          (list (niwa-task--create
                 :id 1 :subject "Plan refactor" :status 'completed
                 :completed-turn 3
                 :owner nil :blocked-by nil :metadata nil)
                (niwa-task--create
                 :id 2 :subject "Implement permission chain"
                 :description "Replace the deprecated specifier handling"
                 :status 'in-progress
                 :owner "main" :blocked-by nil
                 :metadata '(:priority high))))
    session))


(defun test-niwa-session-persistence--publish-generation
    (session session-dir segment transcript turns)
  "Publish TRANSCRIPT for SESSION at SEGMENT with TURNS settled turns."
  (setf (niwa-session-turn-count session) turns)
  (niwa-session-publication-publish
   session
   (list
    (list :path segment :content transcript)
    (list :path (niwa-session-artifacts-sidecar-path session-dir)
          :content
          (niwa-session-artifacts-printed-value
           (niwa-session-artifacts--live-sidecar
            session (current-buffer)))
          :commit-marker t
          :replace t)))
  (plist-get (niwa-session-publication session) :head))

(defun test-niwa-session-persistence--make-remote-restore-fixture
    (host local-root transcript)
  "Create a mock-TRAMP session for HOST at LOCAL-ROOT with TRANSCRIPT."
  (let* ((remote-root
          (format "/niwamock:%s:%s"
                  host (file-name-as-directory local-root)))
         (workspace
          (niwa-workspace-get-or-create
           'project remote-root remote-root "remote-restore"))
         (identity (niwa-workspace-identity-ensure remote-root))
         (session (niwa-session-create "main" workspace remote-root))
         (target (niwa-session-execution-target session))
         (target-incarnation
          (progn
            (niwa-execution-target-probe target t 'off)
            (niwa-execution-target-incarnation target)))
         (session-id "main-remote-restore")
         (session-dir
          (file-name-as-directory
           (file-name-concat remote-root ".niwa" "sessions" session-id)))
         (segment
          (niwa-session-artifacts-segment-path session-dir 1))
         (sidecar
          (test-niwa-session-persistence--complete-sidecar
           (list
            :session-id session-id
            :session-name "main"
            :naming-state 'explicit
            :workspace
            (list :type 'project
                  :workspace-id identity
                  :target-native-root (file-name-as-directory local-root)
                  :name "remote-restore")
            :working-directory (file-name-as-directory local-root)
            :target-incarnation target-incarnation))))
    (setf (niwa-session-session-id session) session-id
          (niwa-session-save-path session) session-dir)
    (make-directory session-dir t)
    (write-region transcript nil segment nil 'silent)
    (with-temp-file
        (niwa-session-artifacts-sidecar-path session-dir)
      (let ((print-length nil)
            (print-level nil))
        (prin1 sidecar (current-buffer))))
    ;; Remote readers discover only immutable, sidecar-committed state.  Leave
    ;; the fixed files in place as non-authoritative caches for poison/race
    ;; cases, but seed a real publication head for the fixture.
    (let ((niwa-session-durability--client-id
           (secure-hash 'sha256 (format "fixture-%s-%s" host local-root))))
      (unwind-protect
          (progn
            (unless
                (niwa-session-durability-lease-acquire
                 session-dir "*fixture-publisher*" session)
              (error "Could not acquire fixture publication lease"))
            (niwa-session-publication-publish
             session
             (list
              (list :path segment :content transcript)
              (list
               :path (niwa-session-artifacts-sidecar-path session-dir)
               :content
               (niwa-session-artifacts-printed-value sidecar)
               :commit-marker t))))
        (niwa-session-durability-lease-release session-dir session)))
    (list workspace session session-dir segment)))


(defun test-niwa-session-persistence--agent-configuration ()
  "Return a compact frozen configuration for persistence tests."
  (niwa-agent-configuration--create
   :agent
   (niwa-agent--create
    :name "default"
    :description "Persisted default agent"
    :tools '((:tool "Read"))
    :system-prompt "Frozen persistence instructions"
    :max-turns 10
    :hook-rules nil
    :frozen-p t)
   :request-locals
   (mapcar
    (lambda (symbol)
      (cons symbol
            (pcase symbol
              ('gptel-backend
               (or gptel-backend
                   (test-niwa-session-persistence--agent-backend)))
              ('gptel-model 'test-model)
              ('gptel-tools
               (list (gptel-get-tool '("niwa" "Read"))))
              ('gptel-context '(("/tmp/persisted-context.el"))))))
    niwa-agent-request-local-symbols)))


(defun test-niwa-session-persistence--complete-sidecar (plist)
  "Return a current complete sidecar with PLIST values overriding defaults."
  (let ((sidecar
         (list :version niwa-session-codec-format-version
               :session-id "test-session"
               :session-name "x"
               :naming-state 'explicit
               :naming-input nil
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
               :recovery-issues nil :pending-follow-ups nil :pending-steering nil
               :pending-input-next-id 0 :pending-input-paused nil :pending-input-failure-paused nil
               :model-provider nil
               :external-conversations nil
               :reasoning-effort nil
               :last-observed-date "2026-01-01"
               :agent-types-snapshot :uninitialized
               :additional-roots nil
               :tasks nil
               :prompt-index nil
               :chapters nil
               :file-snapshots nil
               :ptc-checkpoints nil
               :workspace-instruction-hashes nil
               :agent-transcripts nil
               :agent-registry nil
               :agent-turn-capacity 3
               :plan-metadata nil
               :goal nil
               :attached-artifacts nil
               :dedicated-artifact nil
               :messages nil
               :schedules nil)))
    (while plist
      (setq sidecar (plist-put sidecar (pop plist) (pop plist))))
    sidecar))


(defun test-niwa-session-persistence--deserialize-sidecar (overrides)
  "Deserialize a complete sidecar with OVERRIDES in a real workspace."
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-sidecar-workspace-" t))))
    (unwind-protect
        (let* ((workspace
                (test-niwa-session-persistence--make-workspace root))
               (sidecar
                (test-niwa-session-persistence--complete-sidecar
                 (append
                  (list :workspace
                        (niwa-session-codec--workspace-to-plist
                         workspace)
                        :working-directory root)
                  overrides))))
          (niwa-session-codec-deserialize sidecar workspace))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))


;;
;;; Phase 2: write path

(defun test-niwa-session-persistence--make-tempdir-workspace ()
  "Build a file workspace rooted in a fresh tempdir.
Returns (cons WORKSPACE TEMPDIR).  The workspace's NAME is derived
from the tempdir basename so that different tests never collide on the
chat-buffer name (`*niwa:NAME@WORKSPACE*'); buffer leakage across
tests would otherwise mask correctness bugs in the live-buffer path of
`niwa-session-persistence-restore'.  Caller must
`delete-directory' the tempdir on cleanup."
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "niwa-test-ws-" t)))
         (basename (file-name-nondirectory (directory-file-name tempdir)))
         (_       (niwa-workspace-clear-registry))
         (ws      (niwa-workspace-get-or-create
                   'file basename tempdir basename)))
    (niwa-workspace-identity-ensure tempdir)
    (cons ws tempdir)))


(defun test-niwa-session-persistence--release-and-kill (buf session)
  "Release SESSION's lock and kill BUF if alive.
Mirrors the production kill-buffer-hook's lock release for tests
that don't go through `niwa--chat-buffer-init-common' (which
installs the real hook)."
  (when (and session (niwa-session-save-path session))
    (niwa-session-persistence-lock-release
     (niwa-session-save-path session)
     session))
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf)))


(defun test-niwa-session-persistence--edit-published-sidecar
    (workspace session-dir function)
  "Republish closed SESSION-DIR's sidecar after FUNCTION edits its plist.
The lease publication is the sidecar's authority, not the directory cache.
WORKSPACE owns SESSION-DIR."
  (let* ((sidecar (niwa-session-codec-read
                   (plist-get (niwa-session-publication-read session-dir)
                              :sidecar)))
         (owner (niwa-session-create
                 (plist-get sidecar :session-name) workspace)))
    (setf (niwa-session-session-id owner) (plist-get sidecar :session-id)
          (niwa-session-save-path owner) session-dir)
    (unless (niwa-session-persistence-lock-acquire
             session-dir "*edit-sidecar*" owner)
      (error "Fixture could not lease %s" session-dir))
    (unwind-protect
        (niwa-session-publication-publish
         owner
         (list (list :path (niwa-session-artifacts-sidecar-path session-dir)
                     :content (niwa-session-artifacts-printed-value
                               (funcall function sidecar))
                     :commit-marker t)))
      (niwa-session-persistence-lock-release session-dir owner))))

(defun test-niwa-session-persistence--call-later (function)
  "Call FUNCTION with the target clock thirty days ahead.
Lease cleanup also requires the last renewal to be older than the cap."
  (let ((later (+ (floor (float-time)) (* 30 24 60 60))))
    (cl-letf (((symbol-function 'niwa-session-durability--target-time)
               (lambda (&optional _) later)))
      (funcall function))))

(defun test-niwa-session-persistence--cleanup-later (workspace)
  "Force WORKSPACE's expiry sweep with the target clock thirty days ahead."
  (test-niwa-session-persistence--call-later
   (lambda () (niwa-session-persistence-cleanup-expired workspace t))))

(defun test-niwa-session-persistence--lease-released-p (save-path)
  "Return non-nil when SAVE-PATH's lease head is released."
  (eq 'released
      (plist-get (niwa-session-durability--lease-head
                  (niwa-session-durability--lease-path save-path))
                 :status)))

(defun test-niwa-session-persistence--edit-sidecar (session function)
  "Publish SESSION's metadata after FUNCTION edits it under its held lease."
  (let* ((directory (niwa-session-save-path session))
         (sidecar (niwa-session-codec-read
                   (plist-get (niwa-session-publication-read directory)
                              :sidecar))))
    (niwa-session-publication-publish
     session
     (list (list :path (niwa-session-artifacts-sidecar-path directory)
                 :content (niwa-session-artifacts-printed-value
                           (funcall function sidecar))
                 :commit-marker t)) t)))


(defun test-niwa-session-persistence--expire-session (session &optional days)
  "Publish SESSION's update time as DAYS ago, defaulting to fourteen."
  (test-niwa-session-persistence--edit-sidecar
   session
   (lambda (sidecar)
     (plist-put sidecar :updated-at
                (format-time-string
                 "%FT%H-%M-%S"
                 (time-subtract (current-time) (* (or days 14) 24 60 60)))))))


(defun test-niwa-session-persistence--make-missing-cwd-session ()
  "Return a saved session whose working directory has been deleted.
The result is (WORKSPACE TEMPDIR MISSING-DIR REPLACEMENT-DIR SESSION-DIR)."
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((missing-dir (file-name-as-directory
                         (file-name-concat tempdir "deleted-worktree")))
           (replacement-dir (file-name-as-directory
                             (file-name-concat tempdir "replacement")))
           (session (niwa-session-create "main" workspace missing-dir))
           (buf (generate-new-buffer "*test-data-buf*"))
           session-dir)
      (unwind-protect
          (progn
            (make-directory missing-dir t)
            (make-directory replacement-dir t)
            (with-current-buffer buf
              (org-mode)
              (insert "Missing working directory\n")
              (niwa-session-artifacts-save session buf))
            (setq session-dir (niwa-session-save-path session))
            (test-niwa-session-persistence--release-and-kill buf session)
            (setq buf nil)
            (delete-directory missing-dir t)
            (list workspace tempdir missing-dir replacement-dir session-dir))
        (test-niwa-session-persistence--release-and-kill buf session)))))


(defun test-niwa-session-persistence--reset-instructions ()
  "Reset global and workspace-scoped instruction state for persistence cases."
  (setf (niwa--instruction-alist) nil)
  (setf (niwa--instruction-id-counter) 0)
  (setf (niwa--instruction-id-usage-map) (make-hash-table))
  (setf (niwa--instruction-retired-ids) nil)
  (setq niwa--instruction-states (make-hash-table :test #'equal))
  (setq niwa--instruction-current-state-key :global))


(defun test-niwa-session-persistence--cold-agent-tree-round-trip ()
  "Exercise one durable agent-tree cold resume and its recovery boundary."
  (niwa-tools-register)
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (root (generate-new-buffer "*test-agent-tree-root*"))
           (configuration
            (test-niwa-session-persistence--agent-configuration))
           session-dir restored restored-session)
      (unwind-protect
          (progn
            (with-current-buffer root
              (org-mode)
              (insert "Root conversation\n")
              (niwa-session-artifacts-save session root))
            (setq session-dir (niwa-session-save-path session))
            (let* ((agents-dir (file-name-concat session-dir "agents"))
                   (idle-relative "agents/idle.chat.org")
                   (active-relative "agents/active.chat.org")
                   (idle-file (expand-file-name idle-relative session-dir))
                   (active-file (expand-file-name active-relative session-dir))
                   (idle
                    (niwa-agent-record--create
                     :id "opaque-idle" :path "/root/idle"
                     :parent-path "/root" :role "default"
                     :configuration configuration :activity 'idle
                     :conversation-location idle-relative
                     :mailbox
                     (list
                      (list :id "child-mail" :type 'MAIL :sender "/root"
                            :recipient "/root/idle" :payload "child mail"
                            :timestamp '(1 0 0 0)))))
                   (active
                    (let ((invocation
                           (niwa-agent-invocation--create
                            :path "/root/active" :parent-session session)))
                      (niwa-agent-record--create
                       :id "opaque-active" :path "/root/active"
                       :parent-path "/root" :role "default"
                       :configuration configuration
                       :activity 'running :invocation invocation
                       :conversation-location active-relative)))
                   (bad
                    (niwa-agent-record--create
                     :id "opaque-bad" :path "/root/bad"
                     :parent-path "/root" :role "default"
                     :configuration configuration :activity 'idle
                     :conversation-location "agents/bad.chat.org")))
              (ignore agents-dir)
              ;; Agent transcripts are lease publications, not loose files.
              (niwa-session-publication-publish
               session
               (list
                (list :path idle-file
                      :content "* Conversation Summary\nIndependent compacted history.\n\n* Agent Task: idle\nOld answer.\n")
                (list :path active-file
                      :content
                      (with-temp-buffer
                        (org-mode)
                        (insert "* Agent Task: active\n")
                        (insert (propertize "Partial abandoned response.\n"
                                            'gptel 'response))
                        (niwa-session-artifacts-stabilize-gptel-bounds)
                        (buffer-substring-no-properties
                         (point-min) (point-max))))))
              (setf (niwa-session-agent-turn-capacity session) 7
                    (niwa-session-agent-transcripts session)
                    `(("opaque-idle" :agent-path "/root/idle"
                       :status completed :path ,idle-relative)
                      ("opaque-active" :agent-path "/root/active"
                       :status running :path ,active-relative))
                    (niwa-session-agent-registry session)
                    (list (cons "/root/idle" idle)
                          (cons "/root/active" active)
                          (cons "/root/bad" bad))
                    (niwa-session-messages session)
                    (list
                     (list :id "root-mail" :type 'MAIL :sender "/root/idle"
                           :recipient "/root" :payload "root mail"
                           :timestamp '(1 0 0 0))))
              (should
               (niwa-agent-control-block-turn
                session "/root/active" 'permission-blocked))
              (with-current-buffer root
                (niwa-session-artifacts-save session root)))
            (test-niwa-session-persistence--release-and-kill root session)
            (setq root nil)
            (test-niwa-session-persistence--edit-published-sidecar
             workspace session-dir
             (lambda (persisted)
               (setf (plist-get
                      (cl-find-if
                       (lambda (entry)
                         (equal "/root/bad" (plist-get entry :path)))
                       (plist-get persisted :agent-registry))
                      :conversation-location)
                     "../escape.chat.org")
               persisted))
            (let ((dispatches 0))
              (cl-letf (((symbol-function 'niwa-agent-runtime-dispatch)
                         (lambda (&rest _)
                           (cl-incf dispatches)
                           (error "Restore replayed an abandoned request"))))
                (setq restored
                      (niwa-session-persistence-restore session-dir)))
              (should (zerop dispatches)))
            (setq restored-session
                  (buffer-local-value 'niwa--session restored))
            (should (= 7 (niwa-session-agent-turn-capacity
                          restored-session)))
            (should
             (equal '("/root" "/root/active" "/root/idle")
                    (mapcar
                     (lambda (item) (plist-get item :path))
                     (niwa-agent-control-list-agents restored-session))))
            (let* ((idle
                    (cdr (assoc "/root/idle"
                                (niwa-session-agent-registry
                                 restored-session))))
                   (active
                    (cdr (assoc "/root/active"
                                (niwa-session-agent-registry
                                 restored-session))))
                   (idle-buffer
                    (niwa-agent-record-conversation-buffer idle))
                   (active-buffer
                    (niwa-agent-record-conversation-buffer active)))
              (should (equal "opaque-idle" (niwa-agent-record-id idle)))
              (should (eq 'idle (niwa-agent-record-activity active)))
              (should-not idle-buffer)
              (setq idle-buffer (niwa-agent-persistence-ensure-conversation
                                 restored-session idle restored))
              (should (buffer-live-p idle-buffer))
              (should (buffer-live-p active-buffer))
              (should
               (eq 'aborted
                   (plist-get
                    (cdr (assoc
                          "opaque-active"
                          (niwa-session-agent-transcripts
                           restored-session)))
                    :status)))
              (should
               (eq 'aborted
                   (niwa-agent-invocation-transcript-status
                    (buffer-local-value 'niwa--agent-invocation
                                        active-buffer))))
              (with-current-buffer idle-buffer
                (should (string-match-p "Independent compacted history"
                                        (buffer-string))))
              (should (equal "child mail"
                             (plist-get
                              (car (niwa-agent-record-mailbox idle))
                              :payload))))
            (should (= 2 (length (niwa-session-messages
                                  restored-session))))
            (should (= 1
                       (cl-count-if
                        (lambda (message)
                          (eq 'interrupted (plist-get message :outcome)))
                        (niwa-session-messages restored-session))))
            (let* ((idle
                    (cdr (assoc "/root/idle"
                                (niwa-session-agent-registry
                                 restored-session))))
                   (idle-buffer
                    (niwa-agent-record-conversation-buffer idle))
                   (identity
                    (buffer-local-value 'niwa--agent-invocation
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
                                 :niwa-agent-invocation identity))))
              (niwa-tools--handle-message-inject root-fsm)
              (niwa-tools--handle-message-inject idle-fsm)
              (should-not (niwa-session-messages restored-session))
              (should-not (niwa-agent-record-mailbox idle))
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
              (niwa-tools--handle-message-inject root-fsm)
              (niwa-tools--handle-message-inject idle-fsm)
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
                (niwa-session-artifacts-save
                 restored-session restored)))
            ;; A second cold resume keeps the injected transcript history but
            ;; neither restores consumed mail nor enqueues another recovery
            ;; RESULT for the already-idle turn.
            (test-niwa-session-persistence--release-and-kill
             restored restored-session)
            (setq restored nil restored-session nil)
            (setq restored
                  (niwa-session-persistence-restore session-dir)
                  restored-session
                  (buffer-local-value 'niwa--session restored))
            (should-not (niwa-session-messages restored-session))
            (should
             (eq 'aborted
                 (plist-get
                  (cdr (assoc
                        "opaque-active"
                        (niwa-session-agent-transcripts restored-session)))
                  :status)))
            (let* ((idle
                    (cdr (assoc "/root/idle"
                                (niwa-session-agent-registry
                                 restored-session))))
                   (idle-buffer
                    (niwa-agent-record-conversation-buffer idle))
                   captured-buffer)
              (should-not (niwa-agent-record-mailbox idle))
              (with-current-buffer restored
                (should (= 1 (how-many "root mail" (point-min) (point-max))))
                (should (= 1 (how-many
                              "Agent turn was interrupted by session recovery"
                              (point-min) (point-max)))))
              (should-not idle-buffer)
              (with-current-buffer restored
                (cl-letf
                    (((symbol-function 'niwa-agent-runtime-dispatch)
                      (lambda (_agent _description _message &rest keys)
                        (setq captured-buffer
                              (plist-get keys :retained-buffer))
                        (funcall
                         (plist-get keys :on-invocation)
                         (buffer-local-value 'niwa--agent-invocation
                                             captured-buffer))
                        t)))
                  (niwa-agent-control-send-message
                   restored-session "/root/idle" "Continue after resume.")))
              (setq idle-buffer (niwa-agent-record-conversation-buffer idle))
              (should (buffer-live-p idle-buffer))
              (should (eq idle-buffer captured-buffer))
              (with-current-buffer idle-buffer
                (should (= 1 (how-many "child mail" (point-min) (point-max))))
                (should (string-match-p "Independent compacted history" (buffer-string))))
              (should (eq 'running (niwa-agent-record-activity idle)))
              (setf (niwa-agent-record-activity idle) 'idle
                    (niwa-agent-record-invocation idle) nil)
              (let* ((saved
                      (niwa-session-persistence-load-sidecar
                       (niwa-session-artifacts-sidecar-path
                        session-dir)))
                     (saved-idle
                      (cl-find "/root/idle"
                               (plist-get saved :agent-registry)
                               :key (lambda (entry)
                                      (plist-get entry :path))
                               :test #'equal)))
                (should-not (plist-get saved :messages))
                (should-not (plist-get saved-idle :mailbox)))))
        (test-niwa-session-persistence--release-and-kill
         root session)
        (test-niwa-session-persistence--release-and-kill
         restored restored-session)
        (when (file-directory-p tempdir) (delete-directory tempdir t))
        (niwa-workspace-clear-registry)))))


;;
;;; Phase 3: file-history store

(defun test-niwa-session-persistence--make-materialized-session ()
  "Create a session, materialize it, return (cons SESSION TEMPDIR).
The session's data buffer is `*test-data-buf*' and is left alive; the
caller must `kill-buffer' it during cleanup.  TEMPDIR holds the entire
workspace tree."
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buf     (generate-new-buffer "*test-data-buf*")))
      (with-current-buffer buf
        (org-mode)
        (insert "Initial prompt\n")
        (niwa-session-artifacts-ensure-files session buf))
      (cons session tempdir))))


(defun test-niwa-session-persistence--cleanup (tempdir)
  "Tear down a test session: kill data buffer and remove TEMPDIR."
  (when-let* ((buf (get-buffer "*test-data-buf*")))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf))
  (when (file-directory-p tempdir)
    (delete-directory tempdir t))
  (niwa-workspace-clear-registry))


(defun test-niwa-session-persistence--make-fork-ready ()
  "Return a real saved session rewound and ready to fork.
The result is a plist whose :tempdir owns every created file."
  (niwa-tools-register)
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buf (generate-new-buffer "*test-data-buf*"))
           (source-agent-buffer
            (generate-new-buffer " *fork-source-agent*"))
           (root-view-buffer
            (generate-new-buffer " *fork-root-view*"))
           (source-invocation
            (niwa-agent-invocation--create
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
        (setq-local niwa--session session)
        (add-hook 'kill-buffer-hook
                  #'niwa-session-persistence-release-on-kill nil t)
        (insert "Segment one prompt\n")
        (niwa-session-artifacts-save session buf)
        (niwa-session-artifacts-rotate-segment
         session buf "Summary 1.")
        (insert "Segment two prompt\n")
        (niwa-session-artifacts-save session buf)
        (niwa-session-artifacts-rotate-segment
         session buf "Summary 2.")
        (insert "Future segment prompt\n")
        (niwa-session-artifacts-save session buf)
        (setq parent-path (niwa-session-save-path session)
              parent-id (niwa-session-session-id session))
        ;; Every path below is derived from this one: without it the writes
        ;; below are relative and land in the working tree.
        (should parent-path)
        ;; Stage the parent's plan, agent transcripts and backups for the
        ;; committing save below; the publication tree is the only reader.
        (niwa-session-publication-publish
         session
         (list
          (list :path (file-name-concat parent-path "local" "plans/current.md")
                :content "# Parent plan\n")
          (list :path (file-name-concat parent-path "agents/eligible.chat.org")
                :content eligible-transcript)
          (list :path (file-name-concat
                       parent-path "agents/eligible.compact-0001.chat.org")
                :content "eligible recovery archive\n")
          (list :path (file-name-concat parent-path "agents/future.chat.org")
                :content "future transcript\n")
          (list :path (niwa-session-artifacts-backup-path
                       parent-path "keep@v1")
                :content "kept backup\n")
          (list :path (niwa-session-artifacts-backup-path
                       parent-path "future@v2")
                :content "future backup\n"))
         t)
        (setf (niwa-session-plan-metadata session)
              '(:path "local/plans/current.md" :status presented))
        (setf (niwa-session-prompt-index session)
              '((1 . ((:turn 1 :file-turn 1 :cum-turn 1)))
                (2 . ((:turn 1 :file-turn 1 :cum-turn 2
                       :fork-point-id "fixture-fork")))
                (3 . ((:turn 1 :file-turn 1 :cum-turn 3)))))
        (setf (niwa-session-file-snapshots session)
              `((1 . ((,(file-name-concat tempdir "kept.el")
                       . (:backup-name "keep@v1" :version 1))))
                (3 . ((,(file-name-concat tempdir "future.el")
                       . (:backup-name "future@v2" :version 2))))))
        (setf (niwa-session-agent-transcripts session)
              '(("eligible--1" :agent-path "/root/eligible"
                 :parent-turn 2 :type "default"
                 :description "Historical eligible agent"
                 :status completed :path "agents/eligible.chat.org")
                ("future--2" :agent-path "/root/future" :parent-turn 3
                 :path "agents/future.chat.org")
                ("poison--3" :agent-path "/root/poison" :parent-turn 2
                 :path "../poison.chat.org")))
        (setf (niwa-session-agent-registry session)
              (list
               (cons
                "/root/eligible"
                (niwa-agent-record--create
                 :id "eligible--1"
                 :path "/root/eligible"
                 :parent-path "/root"
                 :role "default"
                 :configuration
                 (test-niwa-session-persistence--agent-configuration)
                 :activity 'idle
                 :conversation-location "agents/eligible.chat.org"
                 :conversation-buffer source-agent-buffer
                 :invocation source-invocation
                 :mailbox
                 (list
                  (list :id "source-mail" :type 'MAIL :sender "/root"
                        :recipient "/root/eligible"
                        :payload "Source-only mail"
                        :timestamp (current-time)))))))
        (setf (niwa-session-agent-turn-capacity session) 7)
        (setf (niwa-session-messages session)
              (list
               (list :id "source-result" :type 'RESULT :sender "/root/eligible"
                     :recipient "/root" :outcome 'completed
                     :payload "Source-only result"
                     :timestamp (current-time))))
        (setf (niwa-session-turn-count session) 3)
        (with-current-buffer source-agent-buffer
          (setq-local niwa--session session)
          (setq-local niwa--agent-invocation source-invocation))
        (niwa-session-publication-publish
         session
         (list
          (list :path (niwa-session-artifacts-segment-path parent-path 2)
                :coding 'utf-8-unix
                :content
                (with-temp-buffer
                  (insert (decode-coding-string
                           (niwa-session-artifacts-read-artifact
                            session "segment-0002.chat.org" t)
                           'utf-8-unix))
                  (org-mode)
                  (niwa-transcript-restore-properties)
                  (goto-char (point-max))
                  (insert
                   (niwa--format-hook-audit-record
                    '(:type fork-point :fork-point-id "fixture-fork"
                      :segment 2 :turn 1 :file-turn 1 :cum-turn 2
                      :captured-file-turn 2)))
                  (niwa-session-artifacts-stabilize-gptel-bounds)
                  (buffer-string))))
         t)
        (niwa-session-artifacts-save session buf nil t)
        (niwa-session-rewind-load-rewind-target
         session buf
         '(:segment 2 :fork-point-id "fixture-fork"
           :turn 1 :file-turn 1 :cum-turn 2))
        (setq-local niwa--view-buffer root-view-buffer))
      (with-current-buffer root-view-buffer
        (setq-local niwa--session session)
        (setq-local niwa--data-buffer buf))
      (let* ((sessions-dir
              (niwa-session-artifacts-sessions-dir workspace))
             (committed
              (lambda (logical)
                (decode-coding-string
                 (niwa-session-artifacts-read-artifact session logical t)
                 'utf-8-unix))))
        (list
         :workspace workspace
         :tempdir tempdir
         :session session
         :buffer buf
         :root-view-buffer root-view-buffer
         :sessions-dir sessions-dir
         :parent-id parent-id
         :parent-path parent-path
         :eligible-transcript eligible-transcript
         :source-agent-record
         (cdr (assoc "/root/eligible"
                     (niwa-session-agent-registry session)))
         :source-agent-buffer source-agent-buffer
         :source-invocation source-invocation
         :parent-sidecar-text (funcall committed "session.meta.el")
         :parent-segment-1-text (funcall committed "segment-0001.chat.org")
         :parent-segment-2-text (funcall committed "segment-0002.chat.org")
         :session-state
         (copy-tree (niwa-session-codec-serialize session))
         :session-save-path (niwa-session-save-path session)
         :buffer-text
         (with-current-buffer buf
           (buffer-substring (point-min) (point-max)))
         :buffer-point (with-current-buffer buf (point))
         :buffer-modified-p (with-current-buffer buf (buffer-modified-p))
         :buffer-file-name (with-current-buffer buf buffer-file-name))))))


(defun test-niwa-session-persistence--cleanup-fork-fixture (fixture)
  "Delete the real files and buffer owned by FIXTURE."
  (when-let* ((buf (plist-get fixture :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)))
  (when-let* ((agent-buffer (plist-get fixture :source-agent-buffer)))
    (when (buffer-live-p agent-buffer)
      (with-current-buffer agent-buffer
        (set-buffer-modified-p nil))
      (kill-buffer agent-buffer)))
  (when-let* ((view-buffer (plist-get fixture :root-view-buffer)))
    (when (buffer-live-p view-buffer)
      (kill-buffer view-buffer)))
  (when-let* ((tempdir (plist-get fixture :tempdir)))
    (when (file-directory-p tempdir)
      (delete-directory tempdir t)))
  (niwa-workspace-clear-registry))

(provide 'niwa-session-test-support)
;;; niwa-session-test-support.el ends here
