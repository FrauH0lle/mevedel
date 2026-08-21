;;; mevedel-session-test-support.el --- Session test support -*- lexical-binding: t -*-

;;; Commentary:

;; Shared setup and real-file fixtures for session owner tests.

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
(require 'mevedel-tool-render-data)
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
(declare-function mevedel-skills-maybe-activate
                  "mevedel-skills-core" (session path))

;; `mevedel-tools'
(declare-function mevedel-tools--handle-message-inject "mevedel-tools" (fsm))

;; `org'
(declare-function org-entry-delete "org" (pom property))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (pom property value))

;; `so-long'
(defvar so-long-predicate)

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


(defun test-mevedel-session-persistence--execution-tool-block
    (tool-use-id render-data)
  "Return a tool block owned by TOOL-USE-ID with RENDER-DATA."
  (propertize
   (concat "#+begin_tool (Bash :command \"true\")\n"
           "(:name \"Bash\" :args (:command \"true\"))\n\n"
           (mevedel-tool-render-data-format
            render-data tool-use-id)
           "#+end_tool\n")
   'gptel (cons 'tool tool-use-id)))


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
                 :owner nil :blocked-by nil :metadata nil)
                (mevedel-task--create
                 :id 2 :subject "Implement permission chain"
                 :description "Replace the deprecated specifier handling"
                 :status 'in-progress
                 :owner "main" :blocked-by nil
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
          (mevedel-session-artifacts-segment-path session-dir 1))
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
        (mevedel-session-artifacts-sidecar-path session-dir)
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
               :path (mevedel-session-artifacts-sidecar-path session-dir)
               :content
               (mevedel-session-artifacts-printed-value sidecar)
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
   (mapcar
    (lambda (symbol)
      (cons symbol
            (pcase symbol
              ('gptel-backend
               (or gptel-backend
                   (test-mevedel-session-persistence--agent-backend)))
              ('gptel-model 'test-model)
              ('gptel-tools
               (list (gptel-get-tool '("mevedel" "Read"))))
              ('gptel-context '(("/tmp/persisted-context.el"))))))
    mevedel-agent-request-local-symbols)))


(defun test-mevedel-session-persistence--complete-sidecar (plist)
  "Return a current complete sidecar with PLIST values overriding defaults."
  (let ((sidecar
         (list :version mevedel-session-codec-format-version
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
                        (mevedel-session-codec--workspace-to-plist
                         workspace)
                        :working-directory root)
                  overrides))))
          (mevedel-session-codec-deserialize sidecar workspace))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


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
         (sidecar (mevedel-session-artifacts-sidecar-path save-path))
         (plist (mevedel-session-codec-read sidecar)))
    (plist-put
     plist :updated-at
     (format-time-string
      "%FT%H-%M-%S"
      (time-subtract (current-time) (* 14 24 60 60))))
    (mevedel-session-codec-write sidecar plist)))


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
              (mevedel-session-artifacts-save session buf))
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
              (mevedel-session-artifacts-save session root))
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
                (mevedel-session-artifacts-stabilize-gptel-bounds)
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
                (mevedel-session-artifacts-save session root))
              (let* ((sidecar (file-name-concat session-dir
                                                 "session.meta.el"))
                     (persisted (mevedel-session-codec-read sidecar))
                     (bad-entry
                      (cl-find-if
                       (lambda (entry)
                         (equal "/root/bad" (plist-get entry :path)))
                       (plist-get persisted :agent-registry))))
                (setf (plist-get bad-entry :conversation-location)
                      "../escape.chat.org")
                (mevedel-session-codec-write sidecar persisted)))
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
                (mevedel-session-artifacts-save
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
                        (funcall
                         (plist-get keys :on-invocation)
                         (buffer-local-value 'mevedel--agent-invocation
                                             captured-buffer))
                        t)))
                  (mevedel-agent-control-followup
                   restored-session "/root/idle" "Continue after resume.")))
              (should (eq idle-buffer captured-buffer))
              (should (eq 'running (mevedel-agent-record-activity idle)))
              (setf (mevedel-agent-record-activity idle) 'idle
                    (mevedel-agent-record-invocation idle) nil)
              (let* ((saved
                      (mevedel-session-persistence-load-sidecar
                       (mevedel-session-artifacts-sidecar-path
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


;;
;;; Phase 3: file-history store

(defun test-mevedel-session-persistence--make-materialized-session ()
  "Create a session, materialize it, return (cons SESSION TEMPDIR).
The session's data buffer is `*test-data-buf*' and is left alive; the
caller must `kill-buffer' it during cleanup.  TEMPDIR holds the entire
workspace tree."
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buf     (generate-new-buffer "*test-data-buf*")))
      (with-current-buffer buf
        (org-mode)
        (insert "Initial prompt\n")
        (mevedel-session-artifacts-ensure-files session buf))
      (cons session tempdir))))


(defun test-mevedel-session-persistence--cleanup (tempdir)
  "Tear down a test session: kill data buffer and remove TEMPDIR."
  (when-let ((buf (get-buffer "*test-data-buf*")))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf))
  (when (file-directory-p tempdir)
    (delete-directory tempdir t))
  (mevedel-workspace-clear-registry))


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
                  #'mevedel-session-persistence-release-on-kill nil t)
        (insert "Segment one prompt\n")
        (mevedel-session-artifacts-save session buf)
        (mevedel-session-artifacts-rotate-segment
         session buf "Summary 1.")
        (insert "Segment two prompt\n")
        (mevedel-session-artifacts-save session buf)
        (mevedel-session-artifacts-rotate-segment
         session buf "Summary 2.")
        (insert "Future segment prompt\n")
        (mevedel-session-artifacts-save session buf)
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
                      (mevedel-session-artifacts-backup-path
                       parent-path "keep@v1") nil 'silent)
        (write-region "future backup\n" nil
                      (mevedel-session-artifacts-backup-path
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
        (mevedel-session-codec-write
         (mevedel-session-artifacts-sidecar-path parent-path)
         (mevedel-session-artifacts-build-sidecar session buf))
        (with-temp-buffer
          (insert-file-contents
           (mevedel-session-artifacts-segment-path parent-path 2))
          (org-mode)
          (require 'mevedel-transcript-restore)
          (mevedel-transcript-restore-properties)
          (goto-char (point-max))
          (insert
           (mevedel--format-hook-audit-record
            '(:type fork-point :fork-point-id "fixture-fork"
              :segment 2 :turn 1 :file-turn 1 :cum-turn 2
              :captured-file-turn 2)))
          (mevedel-session-artifacts-stabilize-gptel-bounds)
          (write-region nil nil
                        (mevedel-session-artifacts-segment-path
                         parent-path 2)
                        nil 'silent))
        (mevedel-session-rewind-load-rewind-target
         session buf
         '(:segment 2 :fork-point-id "fixture-fork"
           :turn 1 :file-turn 1 :cum-turn 2))
        (setq-local mevedel--view-buffer root-view-buffer))
      (with-current-buffer root-view-buffer
        (setq-local mevedel--session session)
        (setq-local mevedel--data-buffer buf))
      (let* ((sessions-dir
              (mevedel-session-artifacts-sessions-dir workspace))
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
         (mevedel-session-artifacts--file-text
          (mevedel-session-artifacts-sidecar-path parent-path))
         :parent-segment-1-text
         (mevedel-session-artifacts--file-text
          (mevedel-session-artifacts-segment-path parent-path 1))
         :parent-segment-2-text
         (mevedel-session-artifacts--file-text
          (mevedel-session-artifacts-segment-path parent-path 2))
         :session-state
         (copy-tree (mevedel-session-codec-serialize session))
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

(provide 'mevedel-session-test-support)
;;; mevedel-session-test-support.el ends here
