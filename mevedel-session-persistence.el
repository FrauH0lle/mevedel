;;; mevedel-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Persistence layer for mevedel chat sessions.
;;
;; Handles sidecar serialization, lazy materialization, per-turn
;; autosave, per-session file-history snapshots, split-on-compact
;; segment rotation, explicit session authority (portable leases for project
;; sessions and PID locks for file sessions),
;; resume, transactional rewind, fork projection, session rename, workspace
;; relocation reconciliation, and auto-cleanup of stale sessions.
;;
;; Sidecar plist shape:
;;
;;   (:version "v0.5.2"
;;    :session-id "main-2026-04-23T14-30-a9f2"
;;    :session-name "main"
;;    :workspace (:type project :workspace-id ID
;;                :target-native-root ROOT :name NAME)
;;    :authority-mode portable
;;    :target-incarnation STRING
;;    :created-at "..." :updated-at "..."
;;    :current-segment 3 :total-turn-count 47
;;    :first-user-message "..."
;;    :latest-user-message "..."
;;    :task-status-notes ((nil :note "..." :updated-turn 12) ...)
;;    :forked-from-session-id nil :forked-from-turn nil
;;    :fork-type nil :forked-from-fork-point-id nil
;;    :permission-mode ask
;;    :sandbox-mode best-effort
;;    :plan-mode nil
;;    :permission-rules ((TOOL-NAME ...) ...)
;;    :resource-grants ((:path "/abs/path" :access read) ...)
;;    :workspace-instruction-hashes (((OWNER PATH) . SHA256) ...)
;;    :additional-roots (("name" . "/abs/path") ...)
;;    :prompt-index ((SEGMENT-N . ((:turn N :pos POS :preview STR :timestamp STR) ...)) ...)
;;    :file-snapshots ((TURN-N . ((PATH . (:backup-name STR-OR-NIL
;;                                          :pre-backup-name STR-OR-NIL
;;                                          :version INT :gap STR-OR-NIL)) ...)) ...))
;;
;; Hash-table-valued slots on the session struct (`touched-files',
;; `mentions-shown') are NOT persisted.  Workspace instruction hashes
;; are persisted as an owner/path alist.  The hash tables reset to empty
;; on load; the consequence is that an LLM coming back from a
;; resume may re-Read files that were already read pre-resume (over-
;; dedup is worse than re-expansion).  Tasks are serialized as plists
;; in `:tasks' and deserialized on load.  Owner-scoped task status
;; notes are serialized in `:task-status-notes'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents)
  (require 'mevedel-structs))

(require 'mevedel-session-control-fs)
(require 'mevedel-transcript)
(require 'mevedel-transport)

;; `diff'
(declare-function diff "diff" (old new &optional switches no-async))

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `gptel'
(declare-function gptel--get-buffer-bounds "ext:gptel" nil)
(declare-function gptel--save-state "ext:gptel" nil)
(declare-function gptel-get-preset "ext:gptel" (name))
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--preset)
(defvar gptel-display-buffer-action)
(defvar gptel-mode)
(defvar gptel-system-prompt)

;; `gptel-org'
(declare-function gptel-org--restore-state "ext:gptel-org" nil)

;; `mevedel'
(declare-function mevedel-version "mevedel" (&optional here message))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p
		  "mevedel-agent-control" (session))

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-deserialize-registry
		  "mevedel-agent-persistence" (raw))
(declare-function mevedel-agent-persistence-restore-tree
		  "mevedel-agent-persistence"
		  (session root-buffer readonly-p))
(declare-function mevedel-agent-persistence-sanitize-mailbox
		  "mevedel-agent-persistence" (raw recipient))
(declare-function mevedel-agent-persistence-serialize-registry
		  "mevedel-agent-persistence" (session))
(declare-function mevedel-agent-persistence-transcript-path-p
		  "mevedel-agent-persistence" (path save-path))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--chat-buffer-disable-org-element-cache
		  "mevedel-chat" nil)
(declare-function mevedel--chat-buffer-init-common "mevedel-chat"
		  (buf workspace source &optional inspection-p))
(declare-function mevedel--normalize-session-directory "mevedel-chat"
		  (directory workspace))
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))

;; `mevedel-directive'
(declare-function mevedel-workspace-rewind-directives
                  "mevedel-directive" (workspace session-id target-turn))

;; `mevedel-execution'
(declare-function mevedel-execution-relocate-artifacts
		  "mevedel-execution" (session old-root new-root))
(declare-function mevedel-execution-session-live-p "mevedel-execution"
		  (session))
(declare-function mevedel-execution-teardown-all "mevedel-execution"
		  nil)
(declare-function mevedel-execution-unsettled-mutation-p
		  "mevedel-execution" (session))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-acknowledge-incarnation
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-incarnation
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-incarnation-changed-p
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-native-root
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-observe-incarnation
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-prepare-incarnation-acknowledgement
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-probe
                  "mevedel-execution-target"
                  (target &optional refresh sandbox-mode))
(declare-function mevedel-execution-target-readiness
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-refresh-incarnation
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-restore-incarnation
                  "mevedel-execution-target" (target incarnation))

;; `mevedel-hooks'
(declare-function mevedel-hooks-flush-log "mevedel-hooks" (session))

;; `mevedel-permission-log'
(declare-function mevedel-permission-log-flush
		  "mevedel-permission-log" (session))

;; `mevedel-permissions'
(declare-function mevedel-permission-deserialize-authority
                  "mevedel-permissions" (rules grants target))
(declare-function mevedel-permission-invalidate-target-grants
                  "mevedel-permissions" (session))
(declare-function mevedel-permission-serialize-authority
                  "mevedel-permissions" (rules grants target))
(defvar mevedel-permission-mode)

;; `mevedel-sandbox'
(defvar mevedel-sandbox-mode)

;; `mevedel-persistence'
(declare-function mevedel--load-instructions-file
		  "mevedel-persistence"
		  (path &optional base-directory confirm quiet
			workspace directive-records preserve-directives-p))
(declare-function mevedel--reset-instructions-preserving-directives
		  "mevedel-persistence" (workspace directives))
(declare-function mevedel--restore-preserved-directives
		  "mevedel-persistence" (workspace))
(declare-function mevedel--serialize-instructions
                  "mevedel-persistence"
                  (&optional base-directory include-original-content))
(declare-function mevedel--write-instructions-file
		  "mevedel-persistence"
		  (path &optional base-directory write-empty quiet
			include-original-content))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-extract-render-data
		  "mevedel-pipeline" (result))
(declare-function mevedel-pipeline-reconcile-lost-executions
		  "mevedel-pipeline"
		  (buffer &optional successor-execution-ids))
(defvar mevedel-pipeline--render-data-close)
(defvar mevedel-pipeline--render-data-open)

;; `mevedel-plan'

;; `mevedel-reminders'
(declare-function mevedel-reminders-clone-list "mevedel-reminders"
		  (reminders))

;; `mevedel-session-durability'
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-path-exists-p
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-read-file
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-write-file
                  "mevedel-session-control-fs" (path content))
(declare-function mevedel-session-durability-call-with-reserved-lease
                  "mevedel-session-durability" (session function))
(defvar mevedel-session-durability--asserted-directories)
(defvar mevedel-session-durability--transaction-clock)
(declare-function mevedel-session-publication-discard-rolled-back
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-durability-disclose
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-forget-removed-session
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-acquire
                  "mevedel-session-durability"
                  (session-dir buffer-name &optional session))
(declare-function mevedel-session-durability-lease-owned-p
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-release
                  "mevedel-session-durability" (session-dir &optional session))
(declare-function mevedel-session-durability-lease-state
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability-lease-status
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-transfer-poll
                  "mevedel-session-transfer" (session))
(declare-function mevedel-session-transfer-decide
                  "mevedel-session-transfer" (session decision))
(declare-function mevedel-session-transfer-release
                  "mevedel-session-transfer" (session))
(declare-function mevedel-session-transfer-request
                  "mevedel-session-transfer" (session &optional label))
(declare-function mevedel-session-publication-committed-p
                  "mevedel-session-publication" (session artifacts))
(declare-function mevedel-session-publication-logical-path-p
                  "mevedel-session-publication" (path))
(declare-function mevedel-session-publication-prune-committed
                  "mevedel-session-publication" (session artifacts))
(declare-function mevedel-session-publication-publish
                  "mevedel-session-publication" (session artifacts))
(declare-function mevedel-session-recovery-refresh
                  "mevedel-session-recovery" (session))
(defvar mevedel-session-recovery--mutation-cache)
(declare-function mevedel-session-publication-read
                  "mevedel-session-publication" (session-dir))
(declare-function mevedel-session-recovery-record-failure
                  "mevedel-session-recovery"
                  (session reason recovery-path))
(declare-function mevedel-session-publication-uncommitted-artifact
                  "mevedel-session-publication" (session logical))

;; `mevedel-session-save-as'
(declare-function mevedel-session-save-as--rename-live-session-buffers
                  "mevedel-session-save-as" (session data-buffer))
(declare-function mevedel-session-save-as-run
                  "mevedel-session-save-as"
                  (session buffer new-name new-id new-save-path))

;; `mevedel-structs'
(declare-function mevedel-directive-attempt-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-untracked-effects
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal--create "mevedel-structs" (&rest slots))
(declare-function mevedel-goal-created-at "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-plan-reference "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-time-used-seconds "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-budget "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-tokens-used "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-turns-run "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-updated-at "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-file-snapshots "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session--create "mevedel-structs"
		  (&rest slots))
(declare-function mevedel-session-agent-turn-capacity
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-authority-mode-for-session
                  "mevedel-structs" (session))
(declare-function mevedel-session-authority-mode-for-workspace
                  "mevedel-structs" (workspace))
(declare-function mevedel-session-buffer-name "mevedel-structs"
		  (session-name workspace))
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-control-transfer "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-execution-target "mevedel-structs"
                  (cl-x) t)
(declare-function mevedel-session-file-snapshots "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-forked-from-fork-point-id
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-session-id
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-turn "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-fork-type "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-p
		  "mevedel-structs" (session))
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-active-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-root-buffer
                  "mevedel-structs" (session buffer))
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-log-pending
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-preset-settings "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-prompt-index "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-resource-grants "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-task-status-notes "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-tasks "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-working-directory "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-worktree-base-commit "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-worktree-branch "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-worktree-directory "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-worktree-source-root "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-task--create "mevedel-structs" (&rest slots))
(declare-function mevedel-task-blocked-by "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-blocks "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-completed-turn "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-task-description "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-normalize-owner "mevedel-structs"
		  (owner agent-registry))
(declare-function mevedel-task-owner "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-prune-dangling-dependencies
		  "mevedel-structs" (tasks))
(declare-function mevedel-task-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-task-subject "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-get-or-create "mevedel-structs"
                  (type id root name))
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-set-directives
                  "mevedel-structs" (workspace directives))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar mevedel-workspace--registry)
(defvar mevedel-session--read-only-mode)

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer-observe
                  "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-notify
                  "mevedel-session-control-transfer"
                  (session event &rest args))
(declare-function mevedel-session-control-transfer-register-root-buffer
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-root-buffer-for-id
                  "mevedel-session-control-transfer" (session-id))
(declare-function mevedel-session-control-transfer-presentation-buffer
                  "mevedel-session-control-transfer" (session))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry"
		  (span &rest props))
(declare-function mevedel-telemetry-record "mevedel-telemetry"
		  (session event &rest props))
(declare-function mevedel-telemetry-start "mevedel-telemetry"
		  (session event &rest props))

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-flush-log
		  "mevedel-tool-repair-diagnostics" (session))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
		  "mevedel-transcript-audit" (record))
(declare-function mevedel-transcript-audit-records
		  "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-audit-spans
		  "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-buffer-directive-ranges
                  "mevedel-transcript-audit" (&optional allow-open))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state
		  "mevedel-transcript-restore" nil)
(declare-function mevedel-transcript-restore-properties
		  "mevedel-transcript-restore"
		  (&optional only-if-missing))
(declare-function mevedel-transcript-restore-sanitize-bounds
		  "mevedel-transcript-restore" nil)

;; `mevedel-utilities'
(declare-function mevedel--forget-place "mevedel-utilities" nil)
(declare-function mevedel--normalize-message-text
                  "mevedel-utilities" (text))

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" nil)
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-agent'
(declare-function mevedel-view-reset-agent-ephemeral-state
		  "mevedel-view-agent" (&optional view-buffer))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-history'
(declare-function mevedel-view-history-load "mevedel-view-history"
		  (&optional session))
(declare-function mevedel-view-history-save "mevedel-view-history"
		  (&optional view-buffer))

;; `mevedel-view-render'
(declare-function mevedel-view--rebase-data-sources
		  "mevedel-view-render" (delta))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace"
		  (&optional buffer))
(declare-function mevedel-workspace-ensure-generated-state-ignored
		  "mevedel-workspace" (workspace))
(defvar mevedel-workspace-additional-roots)

;; `mevedel-workspace-identity'
(declare-function mevedel-workspace-identity-ensure
                  "mevedel-workspace-identity" (root))
(declare-function mevedel-workspace-identity-read
                  "mevedel-workspace-identity" (root))

;; `mevedel-worktree'
(declare-function mevedel-worktree-fork-create
		  "mevedel-worktree" (reservation))
(declare-function mevedel-worktree-fork-preflight
		  "mevedel-worktree" (session))
(declare-function mevedel-worktree-fork-reservation
		  "mevedel-worktree" (session &optional preflight))
(declare-function mevedel-worktree-fork-validate-reservation
		  "mevedel-worktree" (session reservation))

;; `nadvice'
(declare-function advice-add "nadvice"
		  (symbol where function &optional props))
(declare-function advice-member-p "nadvice" (advice symbol))

;; `org'
(declare-function org-entry-delete "ext:org" (pom property))
(declare-function org-entry-get "ext:org"
		  (pom property &optional inherit literal-nil))
(declare-function org-entry-put "ext:org" (epom property value))
(defvar org-agenda-file-menu-enabled)

;; `saveplace'
(defvar save-place-mode)

;; `so-long'
(defvar so-long-predicate)

;;
;;; Customization

(defcustom mevedel-sessions-directory (file-name-concat ".mevedel" "sessions")
  "Directory where session persistence files live.

Relative paths resolve against the workspace root at save time;
absolute paths are used as-is."
  :type 'directory
  :group 'mevedel)

(defcustom mevedel-file-history-max-snapshot-bytes (* 1024 1024)
  "Soft size cap (bytes) for individual pre-turn file checkpoints.

Larger content is persisted as an explicit checkpoint gap with a warning.
Defaults to 1 MB."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-max-age-days 30
  "Auto-cleanup threshold for old sessions, in days.

Sessions older than this are eligible for deletion when the `mevedel'
session chooser runs or Emacs exits (throttled per workspace per Emacs
invocation).  Age comes from `:updated-at', or the sidecar or session
directory modification time when metadata cannot provide it.  Sessions with
active locks are always
skipped: any cross-host lock, or a same-host lock whose PID is live and not
known to have been reused.  A nil value disables auto-cleanup entirely."
  :type '(choice (integer :tag "Days")
          (const :tag "Disabled" nil))
  :group 'mevedel)


;;
;;; Constants

(defconst mevedel-session-persistence-format-version "v0.5.2"
  "Current on-disk session sidecar format.

The authority profile is part of this format.  Readers accept exactly this
value and intentionally do not migrate older sidecars.")

(defconst mevedel-session-persistence--allowed-permission-actions
  '(allow deny ask)
  "Permission rule actions recognised by this version.
Rules with other actions are dropped on load (a future version may
add more, and we don't want to act on actions we don't understand).")

(defconst mevedel-session-persistence--required-sidecar-keys
  '(:version :session-id :session-name :workspace :working-directory
    :authority-mode :target-incarnation
    :created-at :updated-at :current-segment :total-turn-count
    :last-task-write-turn :task-status-notes :first-user-message
    :latest-user-message :forked-from-session-id :forked-from-turn
    :fork-type :forked-from-fork-point-id
    :worktree-source-root :worktree-directory :worktree-branch
    :worktree-base-commit
    :permission-mode :sandbox-mode :plan-mode :permission-rules :resource-grants
    :preset-name :preset-settings :model-provider :reasoning-effort
    :last-observed-date
    :agent-types-snapshot :skills-snapshot :workspace-instruction-hashes
    :additional-roots :tasks
    :prompt-index :file-snapshots :agent-transcripts :agent-registry
    :agent-turn-capacity :plan-metadata :goal :messages)
  "Keys required in every current-version session sidecar.")

(defun mevedel-session-persistence--workspace-authority-mode (workspace)
  "Return the authority mode required by WORKSPACE's category."
  (mevedel-session-authority-mode-for-workspace workspace))

(defun mevedel-session-persistence--authority-mode (session)
  "Return SESSION's explicit authority mode.

Normalize a fresh session's missing mode from its workspace category.  An
explicit mode that contradicts the workspace is rejected."
  (mevedel-session-authority-mode-for-session session))

(defun mevedel-session-persistence--portable-authority-p (session)
  "Return non-nil when SESSION uses the portable lease authority."
  (eq (mevedel-session-persistence--authority-mode session) 'portable))

(defun mevedel-session-persistence--validate-authority-mode
    (mode workspace-plist)
  "Validate MODE against the persisted WORKSPACE-PLIST category."
  (unless (memq mode '(portable pid-lock))
    (error "Invalid session authority mode: %S" mode))
  (let ((type (plist-get workspace-plist :type)))
    (when (and (eq type 'project) (not (eq mode 'portable)))
      (error "Project sessions require portable authority"))
    (when (and (eq type 'file) (not (eq mode 'pid-lock)))
      (error "File sessions require PID-lock authority")))
  mode)

(defun mevedel-session-persistence--authority-mode-for-path
    (session-dir &optional session explicit-mode)
  "Return the authority mode for SESSION-DIR.

SESSION or EXPLICIT-MODE is required for a fresh path with no control
artifact.  Existing control artifacts are only a narrow cold-start fallback
for callers that intentionally operate before hydration; contradictory
artifacts fail closed.  A fresh path without a session or explicit profile is
an error rather than an implicit PID-lock fallback."
  (unless (and (stringp session-dir)
               (file-name-absolute-p session-dir))
    (error "Session path is unavailable: %S" session-dir))
  (let* ((lock (file-name-concat session-dir ".lock"))
         (lease (file-name-concat session-dir ".lease"))
         ;; A decided profile needs no inference probes, and every control
         ;; probe is one target process.
         (decided (or explicit-mode
                      (and session
                           (mevedel-session-persistence--authority-mode
                            session))))
         (mode
          (or decided
              (let ((lock-exists
                     (mevedel-session-control-fs-path-exists-p lock))
                    (lease-exists
                     (mevedel-session-control-fs-path-exists-p lease)))
                (cond
                 ((and lock-exists lease-exists)
                  (error "Session has both PID lock and portable lease: %s"
                         session-dir))
                 (lease-exists 'portable)
                 (lock-exists 'pid-lock)
                 (t
                  (let ((sidecar
                         (file-name-concat session-dir "session.meta.el")))
                    (unless (mevedel-session-control-fs-path-exists-p sidecar)
                      (error "Session authority profile is unavailable: %s"
                             session-dir))
                    (let* ((plist (mevedel-session-persistence-read sidecar))
                           (mode (plist-get plist :authority-mode)))
                      (mevedel-session-persistence--validate-authority-mode
                       mode (plist-get plist :workspace))))))))))
    ;; Only a decided profile can disagree with the directory: an inferred
    ;; one was derived from these very artifacts and already failed closed on
    ;; a mixed directory.  Probe just the artifact that would contradict it.
    (when decided
      (pcase mode
        ('portable
         (when (mevedel-session-control-fs-path-exists-p lock)
           (error "Portable session has a PID lock: %s" session-dir)))
        ('pid-lock
         (when (mevedel-session-control-fs-path-exists-p lease)
           (error "PID-lock session has a portable lease: %s" session-dir)))))
    mode))


;;
;;; Workspace serialization

(defun mevedel-session-persistence--sanitize-workspace-instruction-hashes
    (value)
  "Return valid persisted workspace instruction hashes from VALUE."
  (when (proper-list-p value)
    (cl-loop for entry in value
             for key = (car-safe entry)
             for hash = (cdr-safe entry)
             when (and (proper-list-p key)
                       (= (length key) 2)
                       (stringp (car key))
                       (or (equal (car key) "/root")
                           (mevedel-agent-path-p (car key)))
                       (stringp (cadr key))
                       (file-name-absolute-p (cadr key))
                       (stringp hash)
                       (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" hash))
             collect (cons (copy-sequence key) hash))))

(defun mevedel-session-persistence--workspace-to-plist (workspace)
  "Convert WORKSPACE to a plist for sidecar storage.

The project-owned identity and target-native root are portable across
client-specific TRAMP spellings.  The process-local workspace id and file
cache are not persisted."
  (when workspace
    (require 'mevedel-execution-target)
    (require 'mevedel-workspace-identity)
    (let* ((root (mevedel-workspace-root workspace))
           (identity (mevedel-workspace-identity-read root))
           (target (mevedel-execution-target-create root)))
      (unless identity
        (error "Workspace identity is not materialized: %s" root))
      (list :type (mevedel-workspace-type workspace)
            :workspace-id identity
            :target-native-root
            (mevedel-execution-target-native-root target)
            :name (mevedel-workspace-name workspace)))))

(defun mevedel-session-persistence--workspace-from-plist (plist workspace)
  "Bind PLIST to the currently opened WORKSPACE.

The sidecar never supplies a live filesystem authority.  Resume must bind it
to the workspace through which the user opened the session.  Return
`(WORKSPACE . IDENTITY-CHANGED-P)'.  Rebinding a different project-owned
identity requires confirmation so callers can discard copied authority."
  (unless (and (proper-list-p plist) workspace)
    (error "Invalid persisted workspace"))
  (require 'mevedel-workspace-identity)
  (let ((saved-identity (plist-get plist :workspace-id))
        (saved-root (plist-get plist :target-native-root))
        (current-identity
         (mevedel-workspace-identity-read
          (mevedel-workspace-root workspace))))
    (unless (and (stringp saved-identity)
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" saved-identity))
      (error "Invalid persisted workspace identity"))
    (unless (and (stringp saved-root)
                 (file-name-absolute-p saved-root)
                 (not (file-remote-p saved-root)))
      (error "Invalid target-native workspace root: %S" saved-root))
    (unless current-identity
      (error "Workspace identity is missing: %s"
             (mevedel-workspace-root workspace)))
    (let ((changed-p (not (equal saved-identity current-identity))))
      (when (and changed-p
                 (not
                  (yes-or-no-p
                   (concat
                    "This session belongs to a different workspace identity. "
                    "Resume against the opened project and discard copied "
                    "session permissions, resource grants, and additional "
                    "roots? "))))
        (user-error "Workspace identity change was not accepted"))
      (cons workspace changed-p))))


;;
;;; Permission rule hygiene

(defun mevedel-session-persistence--filter-permission-rules (rules)
  "Drop RULES whose `:action' is unrecognised.
A rule is `(TOOL-NAME &rest PLIST)' with `:action SYMBOL'."
  (cl-remove-if-not
   (lambda (rule)
     (and (consp rule)
          (memq (plist-get (cdr rule)
                           :action)
                mevedel-session-persistence--allowed-permission-actions)))
   rules))

(defun mevedel-session-persistence--filter-resource-grants (grants)
  "Keep well-formed exact resource GRANTS.
A grant is `(:path ABSOLUTE-PATH :access read-or-write)'."
  (cl-remove-if-not
   (lambda (grant)
     (and (proper-list-p grant)
          (stringp (plist-get grant :path))
          (file-name-absolute-p (plist-get grant :path))
          (memq (plist-get grant :access) '(read write))))
   grants))


;;
;;; Working directory restore

(defun mevedel-session-persistence--working-directory-from-plist
    (plist workspace target)
  "Return PLIST's restored working directory for WORKSPACE.

TARGET qualifies the persisted target-native path through the current
client's live filesystem authority.  Paths below a prior target-native root
are mapped below the opened workspace root before containment is checked."
  (let* ((raw (plist-get plist :working-directory))
         (saved-workspace (plist-get plist :workspace))
         (saved-root
          (file-name-as-directory
           (expand-file-name
            (plist-get saved-workspace :target-native-root))))
         (current-native-root
          (mevedel-execution-target-native-root target)))
    (unless (and (stringp raw)
                 (file-name-absolute-p raw)
                 (not (file-remote-p raw)))
      (error "Invalid target-native working directory: %S" raw))
    (let* ((native-directory (file-name-as-directory (expand-file-name raw)))
           (mapped
            (cond
             ((string-prefix-p current-native-root native-directory)
              native-directory)
             ((string-prefix-p saved-root native-directory)
              (concat current-native-root
                      (substring native-directory (length saved-root))))
             (t native-directory)))
           (dir
            (file-name-as-directory
             (mevedel-execution-target-expand-path target mapped)))
           (root (mevedel-workspace-root workspace)))
      (unless (file-in-directory-p dir root)
        (user-error "Working directory must be inside workspace root %s" root))
      dir)))


;;
;;; Goal serialization

(defun mevedel-session-persistence--goal-to-plist (goal)
  "Serialize GOAL to a sidecar plist."
  (list :id (mevedel-goal-id goal)
        :objective (mevedel-goal-objective goal)
        :status (mevedel-goal-status goal)
        :reason (mevedel-goal-reason goal)
        :token-budget (mevedel-goal-token-budget goal)
        :tokens-used (mevedel-goal-tokens-used goal)
        :time-used-seconds (mevedel-goal-time-used-seconds goal)
        :turns-run (mevedel-goal-turns-run goal)
        :plan-reference (mevedel-goal-plan-reference goal)
        :created-at (mevedel-goal-created-at goal)
        :updated-at (mevedel-goal-updated-at goal)))

(defun mevedel-session-persistence--goal-from-plist (plist)
  "Reconstruct a `mevedel-goal' from PLIST, or nil."
  (when plist
    (let ((keys '(:id :objective :status :reason :token-budget :tokens-used
                  :time-used-seconds :turns-run :plan-reference
                  :created-at :updated-at)))
      (unless
          (and (proper-list-p plist)
               (cl-evenp (length plist))
               (= (length plist) (* 2 (length keys)))
               (cl-every (lambda (key) (plist-member plist key)) keys)
               (cl-loop for (key _) on plist by #'cddr
                        always (memq key keys))
               (stringp (plist-get plist :id))
               (string-match-p "\\`[[:alnum:]_.-]+\\'"
                               (plist-get plist :id))
               (not (member (plist-get plist :id) '("." "..")))
               (stringp (plist-get plist :objective))
               (not (string-blank-p (plist-get plist :objective)))
               (memq (plist-get plist :status)
                     '(active paused blocked budget-limited complete))
               (let ((reason (plist-get plist :reason))
                     (status (plist-get plist :status)))
                 (if (memq status '(paused blocked budget-limited))
                     (and (stringp reason) (not (string-blank-p reason)))
                   (null reason)))
               (or (null (plist-get plist :token-budget))
                   (and (integerp (plist-get plist :token-budget))
                        (> (plist-get plist :token-budget) 0)))
               (natnump (plist-get plist :tokens-used))
               (natnump (plist-get plist :time-used-seconds))
               (natnump (plist-get plist :turns-run))
               (or (null (plist-get plist :plan-reference))
                   (let ((reference (plist-get plist :plan-reference)))
                     (and (stringp reference)
                          (not (string-empty-p reference))
                          (not (file-name-absolute-p reference))
                          (equal reference
                                 (file-relative-name
                                  (expand-file-name reference "/") "/"))
                          (not (string-prefix-p "../" reference))
                          (not (equal reference "..")))))
               (stringp (plist-get plist :created-at))
               (stringp (plist-get plist :updated-at)))
        (error "Invalid Goal sidecar")))
    (mevedel-goal--create
     :id (plist-get plist :id)
     :objective (plist-get plist :objective)
     :status (plist-get plist :status)
     :reason (plist-get plist :reason)
     :token-budget (plist-get plist :token-budget)
     :tokens-used (plist-get plist :tokens-used)
     :time-used-seconds (plist-get plist :time-used-seconds)
     :turns-run (plist-get plist :turns-run)
     :plan-reference (plist-get plist :plan-reference)
     :created-at (plist-get plist :created-at)
     :updated-at (plist-get plist :updated-at))))


;;
;;; Task serialization

(defun mevedel-session-persistence--task-to-plist (task)
  "Serialize TASK struct to a plist."
  (list :id          (mevedel-task-id task)
        :subject     (mevedel-task-subject task)
        :description (mevedel-task-description task)
        :status      (mevedel-task-status task)
        :owner       (mevedel-task-owner task)
        :blocks      (mevedel-task-blocks task)
        :blocked-by  (mevedel-task-blocked-by task)
        :completed-turn (mevedel-task-completed-turn task)
        :metadata    (mevedel-task-metadata task)))

(defun mevedel-session-persistence--task-from-plist
    (plist &optional agent-registry)
  "Reconstruct a `mevedel-task' from PLIST."
  (mevedel-task--create
   :id          (plist-get plist :id)
   :subject     (plist-get plist :subject)
   :description (plist-get plist :description)
   :status      (plist-get plist :status)
   :owner       (mevedel-task-normalize-owner
                 (plist-get plist :owner) agent-registry)
   :blocks      (plist-get plist :blocks)
   :blocked-by  (plist-get plist :blocked-by)
   :completed-turn (plist-get plist :completed-turn)
   :metadata    (plist-get plist :metadata)))


;;
;;; Top-level serialize / deserialize

(cl-defun mevedel-session-persistence-serialize (session
                                                 &key
                                                 first-user-message
                                                 latest-user-message
                                                 additional-roots)
  "Serialize SESSION to a sidecar plist.

FIRST-USER-MESSAGE is the cached original-request preview.
LATEST-USER-MESSAGE is the cached resume picker preview.
ADDITIONAL-ROOTS is the buffer-local value of
`mevedel-workspace-additional-roots' for this session.

The resulting plist is round-trippable via
`mevedel-session-persistence-deserialize'."
  (require 'mevedel-agent-persistence)
  (require 'mevedel-execution-target)
  (require 'mevedel-permissions)
  (let* ((execution-target (mevedel-session-execution-target session))
         (authority-mode
          (mevedel-session-persistence--authority-mode session))
         (target-incarnation
          (mevedel-execution-target-incarnation execution-target))
         (permission-mode
         (or (mevedel-session-permission-mode session)
             (and (boundp 'mevedel-permission-mode)
                  (default-toplevel-value 'mevedel-permission-mode))
             'ask))
        (sandbox-mode
         (or (mevedel-session-sandbox-mode session)
             (and (boundp 'mevedel-sandbox-mode)
                  (default-toplevel-value 'mevedel-sandbox-mode))
             'best-effort))
        (authority
         (or
          (mevedel-permission-serialize-authority
           (mevedel-session-permission-rules session)
           (mevedel-session-resource-grants session)
           execution-target)
          (error "Session permission authority is not portable"))))
    (mevedel-session-persistence--validate-authority-mode
     authority-mode
     (mevedel-session-persistence--workspace-to-plist
      (mevedel-session-workspace session)))
    (unless (and (stringp target-incarnation)
                 (string-match-p "\\S-" target-incarnation))
      (error "Target incarnation is not available"))
    (unless (memq permission-mode '(ask edits full-auto))
      (error "Invalid persisted permission mode: %S" permission-mode))
    (unless (memq sandbox-mode '(best-effort required off))
      (error "Invalid persisted sandbox mode: %S" sandbox-mode))
    (list
   :version                mevedel-session-persistence-format-version
   :session-id             (mevedel-session-session-id session)
   :session-name           (mevedel-session-name session)
   :workspace              (mevedel-session-persistence--workspace-to-plist
                            (mevedel-session-workspace session))
   :authority-mode         authority-mode
   :working-directory
   (mevedel-execution-target-native-path
    execution-target
    (or (mevedel-session-working-directory session)
        (mevedel-workspace-root (mevedel-session-workspace session))))
   :target-incarnation     target-incarnation
   :created-at             (mevedel-session-created-at session)
   :updated-at             (mevedel-session-updated-at session)
   :current-segment        (or (mevedel-session-current-segment session) 1)
   :total-turn-count       (or (mevedel-session-turn-count session) 0)
   :last-task-write-turn   (mevedel-session-last-task-write-turn session)
   :task-status-notes      (mevedel-session-task-status-notes session)
   :first-user-message     first-user-message
   :latest-user-message    latest-user-message
   :forked-from-session-id (mevedel-session-forked-from-session-id session)
   :forked-from-turn       (mevedel-session-forked-from-turn session)
   :fork-type              (mevedel-session-fork-type session)
   :forked-from-fork-point-id
   (mevedel-session-forked-from-fork-point-id session)
   :worktree-source-root (mevedel-session-worktree-source-root session)
   :worktree-directory   (mevedel-session-worktree-directory session)
   :worktree-branch      (mevedel-session-worktree-branch session)
   :worktree-base-commit (mevedel-session-worktree-base-commit session)
   :permission-mode        permission-mode
   :sandbox-mode           sandbox-mode
   :plan-mode              (and (mevedel-session-plan-mode session) t)
   :permission-rules       (plist-get authority :rules)
   :resource-grants        (plist-get authority :resource-grants)
   :preset-name            (mevedel-session-preset-name session)
   :preset-settings        (mevedel-session-preset-settings session)
   :model-provider         (mevedel-session-model-provider session)
   :reasoning-effort       (mevedel-session-reasoning-effort session)
   :last-observed-date     (mevedel-session-last-observed-date session)
   :agent-types-snapshot   (mevedel-session-agent-types-snapshot session)
   :skills-snapshot        (mevedel-session-skills-snapshot session)
   :workspace-instruction-hashes
   (copy-tree (mevedel-session-workspace-instruction-hashes session) t)
   :additional-roots       additional-roots
   :tasks                  (mapcar #'mevedel-session-persistence--task-to-plist
                                   (mevedel-session-tasks session))
   :prompt-index           (mevedel-session-prompt-index session)
   :file-snapshots         (mevedel-session-file-snapshots session)
   :agent-transcripts      (mevedel-session-agent-transcripts session)
   :agent-registry         (mevedel-agent-persistence-serialize-registry session)
   :agent-turn-capacity    (mevedel-session-agent-turn-capacity session)
   :plan-metadata          (mevedel-session-plan-metadata session)
   :goal                   (when-let* ((goal (mevedel-session-goal session)))
                             (mevedel-session-persistence--goal-to-plist goal))
   ;; Root's reverse-order unread queue.  Child queues live on their explicit
   ;; registry records and all queues become FIFO only at delivery time.
   :messages
   (mevedel-agent-persistence-sanitize-mailbox
    (mevedel-session-messages session) "/root"))))

(defun mevedel-session-persistence--validate-current-sidecar (plist)
  "Return PLIST when it contains every current-version sidecar key."
  (unless (proper-list-p plist)
    (error "Invalid session sidecar"))
  (dolist (key mevedel-session-persistence--required-sidecar-keys)
    (unless (plist-member plist key)
      (error "Missing session sidecar key: %s" key)))
  (mevedel-session-persistence--validate-authority-mode
   (plist-get plist :authority-mode)
   (plist-get plist :workspace))
  (let ((incarnation (plist-get plist :target-incarnation)))
    (unless (and (stringp incarnation)
                 (string-match-p "\\S-" incarnation))
      (error "Invalid persisted target incarnation: %S" incarnation))
    incarnation)
  (unless (memq (plist-get plist :permission-mode) '(ask edits full-auto))
    (error "Invalid persisted permission mode: %S"
           (plist-get plist :permission-mode)))
  (unless (memq (plist-get plist :sandbox-mode)
                '(best-effort required off))
    (error "Invalid persisted sandbox mode: %S"
           (plist-get plist :sandbox-mode)))
  (unless (booleanp (plist-get plist :plan-mode))
    (error "Invalid persisted Plan mode: %S" (plist-get plist :plan-mode)))
  (unless (or (null (plist-get plist :model-provider))
              (and (stringp (plist-get plist :model-provider))
                   (string-match-p
                    "\\`[^:]+:.+\\'"
                    (plist-get plist :model-provider))))
    (error "Invalid persisted model provider: %S"
           (plist-get plist :model-provider)))
  (unless (or (null (plist-get plist :reasoning-effort))
              (symbolp (plist-get plist :reasoning-effort)))
    (error "Invalid persisted reasoning effort: %S"
           (plist-get plist :reasoning-effort)))
  (unless (and (integerp (plist-get plist :agent-turn-capacity))
               (> (plist-get plist :agent-turn-capacity) 0))
    (error "Invalid persisted agent turn capacity: %S"
           (plist-get plist :agent-turn-capacity)))
  (dolist (segment (plist-get plist :prompt-index))
    (unless (and (consp segment) (integerp (car segment)))
      (error "Invalid session prompt-index segment: %S" segment))
    (dolist (prompt (cdr segment))
      (unless (and (proper-list-p prompt)
                   (cl-every (lambda (key) (plist-member prompt key))
                             '(:turn :file-turn :cum-turn)))
        (error "Invalid session prompt entry: %S" prompt))))
  plist)

(defun mevedel-session-persistence-deserialize (plist workspace)
  "Reconstruct a session from sidecar PLIST for opened WORKSPACE.

Returns a plist:
  (:session SESSION
   :first-user-message STR-OR-NIL
   :latest-user-message STR-OR-NIL
   :additional-roots ALIST)

Where SESSION is a freshly-created `mevedel-session' struct populated
from PLIST.  The auxiliary fields (first-user-message,
latest-user-message, additional-roots) are returned alongside because
they are not on the session struct.

Only the current sidecar version is accepted.  Permission rules with
unknown actions and task state with invalid agent owners are dropped via
their hygiene filters."
  (unless (equal (plist-get plist :version)
                mevedel-session-persistence-format-version)
    (error "Unsupported session version: %s"
           (or (plist-get plist :version) "missing")))
  (mevedel-session-persistence--validate-current-sidecar plist)
  (require 'mevedel-agent-persistence)
  (require 'mevedel-execution-target)
  (require 'mevedel-permissions)
  (let* ((workspace-binding
          (mevedel-session-persistence--workspace-from-plist
           (plist-get plist :workspace) workspace))
         (workspace (car workspace-binding))
         (workspace-identity-changed-p (cdr workspace-binding))
         (execution-target
          (let* ((target
                  (mevedel-execution-target-create
                   (mevedel-workspace-root workspace)))
                 (incarnation (plist-get plist :target-incarnation)))
            (mevedel-execution-target-restore-incarnation
             target incarnation)
            target))
         (working-directory
          (mevedel-session-persistence--working-directory-from-plist
           plist workspace execution-target))
         (persisted-rules
          (unless workspace-identity-changed-p
            (mevedel-session-persistence--filter-permission-rules
             (plist-get plist :permission-rules))))
         (persisted-resource-grants
          (unless workspace-identity-changed-p
            (mevedel-session-persistence--filter-resource-grants
             (plist-get plist :resource-grants))))
         (authority
          (mevedel-permission-deserialize-authority
           persisted-rules
           persisted-resource-grants
           execution-target))
         (rules (and authority (plist-get authority :rules)))
         (resource-grants
          (and authority (plist-get authority :resource-grants)))
         (prompt-index (plist-get plist :prompt-index))
         (latest-user-message (plist-get plist :latest-user-message))
         (raw-agent-registry (plist-get plist :agent-registry))
         (agent-registry
          (mevedel-agent-persistence-deserialize-registry raw-agent-registry))
         (tasks
          (mevedel-task-prune-dangling-dependencies
           (delq
            nil
            (mapcar
             (lambda (task-plist)
               (condition-case nil
                   (mevedel-session-persistence--task-from-plist
                    task-plist agent-registry)
                 (error nil)))
             (plist-get plist :tasks)))))
         (task-status-notes
          (cl-loop
           for entry in (and (proper-list-p
                              (plist-get plist :task-status-notes))
                             (plist-get plist :task-status-notes))
           when (consp entry)
           for normalized
           = (condition-case nil
                 (cons t
                       (mevedel-task-normalize-owner
                        (car entry) agent-registry))
               (error nil))
           when normalized
           collect (cons (cdr normalized) (copy-tree (cdr entry)))))
         (session   (mevedel-session--create
                     :name             (plist-get plist :session-name)
                     :workspace        workspace
                     :execution-target execution-target
                     :authority-mode  (plist-get plist :authority-mode)
                     :working-directory working-directory
                     :touched-files    (make-hash-table :test #'equal)
                     :mentions-shown   (make-hash-table :test #'equal)
                     :tasks            tasks
                     :permission-rules rules
                     :resource-grants  resource-grants
                     :permission-mode  (plist-get plist :permission-mode)
                     :sandbox-mode     (plist-get plist :sandbox-mode)
                     :plan-mode        (plist-get plist :plan-mode)
                     :preset-name      (plist-get plist :preset-name)
                     :preset-settings  (copy-tree
                                        (plist-get plist :preset-settings))
                     :model-provider   (plist-get plist :model-provider)
                     :reasoning-effort (plist-get plist :reasoning-effort)
                     :turn-count       (plist-get plist :total-turn-count)
                     :last-observed-date (plist-get plist :last-observed-date)
                     :agent-types-snapshot
                     (plist-get plist :agent-types-snapshot)
                     :skills-snapshot (plist-get plist :skills-snapshot)
                     :workspace-instruction-hashes
                     (mevedel-session-persistence--sanitize-workspace-instruction-hashes
                      (plist-get plist :workspace-instruction-hashes))
                     :last-task-write-turn
                     (plist-get plist :last-task-write-turn)
                     :task-status-notes task-status-notes
                     :session-id       (plist-get plist :session-id)
                     :created-at       (plist-get plist :created-at)
                     :updated-at       (plist-get plist :updated-at)
                     :current-segment  (plist-get plist :current-segment)
                     :forked-from-session-id
                     (plist-get plist :forked-from-session-id)
                     :forked-from-turn (plist-get plist :forked-from-turn)
                     :fork-type (plist-get plist :fork-type)
                     :forked-from-fork-point-id
                     (plist-get plist :forked-from-fork-point-id)
                     :worktree-source-root
                     (plist-get plist :worktree-source-root)
                     :worktree-directory
                     (plist-get plist :worktree-directory)
                     :worktree-branch
                     (plist-get plist :worktree-branch)
                     :worktree-base-commit
                     (plist-get plist :worktree-base-commit)
                     :prompt-index     prompt-index
                     :file-snapshots   (plist-get plist :file-snapshots)
                     :plan-metadata    (plist-get plist :plan-metadata)
                     :goal
                     (condition-case nil
                         (mevedel-session-persistence--goal-from-plist
                          (plist-get plist :goal))
                       (error nil))
                     :agent-transcripts
                     (mevedel-session-persistence--sanitize-agent-transcripts
                      (plist-get plist :agent-transcripts))
                     :agent-registry agent-registry
                     :agent-turn-capacity
                     (plist-get plist :agent-turn-capacity)
                     :messages
                     (mevedel-agent-persistence-sanitize-mailbox
                      (plist-get plist :messages) "/root"))))
    (when-let* ((goal (mevedel-session-goal session))
                ((eq (mevedel-goal-status goal) 'active)))
        (setf (mevedel-goal-status goal) 'paused
              (mevedel-goal-reason goal) "session resumed"
              (mevedel-goal-updated-at goal)
              (format-time-string "%FT%T%z")))
    (list :session             session
          :first-user-message  (plist-get plist :first-user-message)
          :latest-user-message latest-user-message
          :additional-roots    (unless workspace-identity-changed-p
                                 (plist-get plist :additional-roots))
          :agent-registry-repaired-p
          (not (= (length agent-registry)
                  (length (and (proper-list-p raw-agent-registry)
                               raw-agent-registry)))))))


;;
;;; Sidecar IO

(defun mevedel-session-persistence-write (path plist)
  "Write sidecar PLIST to PATH atomically.
Uses a temp file created in PATH's own directory so the final
  `rename-file' stays within the same filesystem and is POSIX-atomic
  even on setups where the workspace lives on a different mount from
  the system temp directory."
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil)
          (print-circle t)
          (print-quoted t))
      (prin1 plist (current-buffer))
      (mevedel-session-control-fs-write-file
       (mevedel-session-control-fs-physical-path path)
       (buffer-string)))))

(defun mevedel-session-persistence-read (path)
  "Read sidecar plist from PATH.
Returns the raw plist.  Caller is responsible for passing it through
`mevedel-session-persistence-deserialize' for validation and hygiene."
  (with-temp-buffer
    (insert (mevedel-session-control-fs-read-file
             (mevedel-session-control-fs-physical-path path)))
    (goto-char (point-min))
    (read (current-buffer))))

(defun mevedel-session-persistence--write-current-buffer-atomically (path)
  "Write the current buffer to PATH through a same-directory rename."
  (mevedel-session-control-fs-write-file
   (mevedel-session-control-fs-physical-path path)
   (buffer-substring-no-properties (point-min) (point-max))))

(defun mevedel-session-persistence--execution-successor-ids (path)
  "Return structured execution ids present in transcript PATH."
  (when (file-regular-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (require 'mevedel-pipeline)
      (require 'mevedel-transcript-audit)
      (let (ids)
        (dolist (record
                 (mevedel-transcript-audit-records
                  (buffer-string)))
          (when (memq (plist-get record :type)
                      '(execution-archive execution-completion))
            (when-let* ((id (plist-get (plist-get record :render-data)
                                       :execution-id)))
              (cl-pushnew id ids :test #'equal))))
        (goto-char (point-min))
        (while (search-forward mevedel-pipeline--render-data-open nil t)
          (let ((begin (match-beginning 0)))
            (when (search-forward mevedel-pipeline--render-data-close nil t)
              (when-let* ((parsed
                           (mevedel-pipeline-extract-render-data
                            (buffer-substring-no-properties
                             begin (match-end 0))))
                          (id (plist-get (cdr parsed) :execution-id)))
                (cl-pushnew id ids :test #'equal)))))
        ids))))

(defun mevedel-session-persistence--reconcile-lost-execution-file
    (path &optional successor-execution-ids artifact-callback)
  "Mark stale running execution rows in transcript PATH as lost.

When ARTIFACT-CALLBACK is non-nil, pass it the repaired replacement instead
of writing PATH directly."
  (when (file-regular-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (require 'mevedel-pipeline)
      (let ((count
              (mevedel-pipeline-reconcile-lost-executions
              (current-buffer) successor-execution-ids)))
        (when (> count 0)
          (if artifact-callback
              (funcall
               artifact-callback
               (list :path path
                     :content (buffer-string)
                     :coding (or buffer-file-coding-system 'utf-8-unix)))
            (mevedel-session-persistence--write-current-buffer-atomically
             path)))
        count))))

(defun mevedel-session-persistence--reconcile-lost-execution-segments
    (session &optional exclude-path artifact-callback)
  "Repair stale execution rows in SESSION segments except EXCLUDE-PATH.

ARTIFACT-CALLBACK has the meaning described by
`mevedel-session-persistence--reconcile-lost-execution-file'."
  (let ((save-path (mevedel-session-save-path session))
        (successor-ids
         (and exclude-path
              (mevedel-session-persistence--execution-successor-ids
               exclude-path)))
        (count 0))
    (cl-loop for segment downfrom (mevedel-session-current-segment session) to 1
             for path = (mevedel-session-persistence--segment-path
                         save-path segment)
             unless (and exclude-path
                         (string= (expand-file-name path)
                                  (expand-file-name exclude-path)))
             when (file-exists-p path)
             do (cl-incf
                 count
                 (or (mevedel-session-persistence--reconcile-lost-execution-file
                      path successor-ids artifact-callback)
                     0))
             and do
             (dolist (id
                      (mevedel-session-persistence--execution-successor-ids
                       path))
               (cl-pushnew id successor-ids :test #'equal)))
    count))

(defun mevedel-session-persistence--sanitize-agent-transcripts (raw)
  "Sanitize the `:agent-transcripts' alist RAW read from a sidecar.

Drops entries whose paths fail validation.  Coerces unknown status
values to `incomplete'.  Deduplicates duplicate agent-ids by keeping
the entry with the newest `:updated-at'.  Preserves unknown plist
keys for forward compatibility -- they round-trip but are ignored at
render time."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (entry (and (listp raw) raw))
      (when (and (consp entry)
                 (stringp (car entry))
                 (listp (cdr entry)))
        (let* ((id    (car entry))
               (plist (copy-sequence (cdr entry)))
               (status (plist-get plist :status))
               (existing (gethash id seen)))
          (unless (memq status '(running completed error aborted incomplete))
            (setq plist (plist-put plist :status 'incomplete)))
          (cond
           ((null existing)
            (puthash id plist seen)
            (push (cons id plist) out))
           ((let ((a (plist-get plist :updated-at))
                  (b (plist-get existing :updated-at)))
              (and (stringp a) (stringp b) (string> a b)))
            (puthash id plist seen)
            (setf (alist-get id out nil nil #'equal) plist))))))
    (nreverse out)))

(defun mevedel-session-persistence--prune-agent-transcripts-after-fork
    (session fork-turn)
  "Drop SESSION transcript entries whose `:parent-turn' exceeds FORK-TURN."
  (let ((entries (mevedel-session-agent-transcripts session)))
    (setf (mevedel-session-agent-transcripts session)
          (cl-remove-if (lambda (entry)
                          (let ((pt (plist-get (cdr entry) :parent-turn)))
                            (and (integerp pt) (> pt fork-turn))))
                        entries))))

(defun mevedel-session-persistence--flush-diagnostic-logs-now (session)
  "Retry SESSION diagnostics queued before or after materialization."
  (when (fboundp 'mevedel-telemetry-flush)
    (mevedel-telemetry-flush session))
  (when (fboundp 'mevedel-hooks-flush-log)
    (mevedel-hooks-flush-log session))
  (when (fboundp 'mevedel-tool-repair-flush-log)
    (mevedel-tool-repair-flush-log session))
  (when (fboundp 'mevedel-permission-log-flush)
    (mevedel-permission-log-flush session)))

(defun mevedel-session-persistence--flush-diagnostic-logs (session)
  "Flush SESSION diagnostics, keeping target I/O off the caller's path.

A remote diagnostic flush is a whole publication transaction carrying
data nothing reads live, so it waits for an idle transport instead of
extending the save or settlement that queued it.  A local flush is one
append and runs inline.  Emacs exit flushes inline either way."
  (let ((save-path (mevedel-session-save-path session)))
    (if (and save-path (file-remote-p save-path))
        ;; The zero timer lets the save or settlement that queued the
        ;; diagnostics return first; transport idleness alone would run
        ;; the flush inline, because the transport is usually idle by
        ;; the time a save reaches its cleanup.
        (run-at-time
         0 nil
         (lambda ()
           (require 'mevedel-transport)
           (mevedel-transport-run-when-idle
            (list 'diagnostic-flush session) save-path
            (lambda ()
              (mevedel-session-persistence--flush-diagnostic-logs-now
               session)))))
      (mevedel-session-persistence--flush-diagnostic-logs-now session))))

(defun mevedel-session-persistence--allocate-session-id (name sessions-dir)
  "Return a fresh session id for NAME below SESSIONS-DIR."
  (cl-loop repeat 33
           for candidate = (mevedel-session-persistence--compute-id name)
           for path = (file-name-concat sessions-dir candidate)
           unless (or (file-exists-p path) (file-symlink-p path))
           return candidate
           finally (error "Could not allocate a unique session id after 33 attempts")))

(defun mevedel-session-persistence--shallow-ensure-files (session buffer)
  "Materialize SESSION and BUFFER paths without writing the sidecar.

Used by sub-agent allocation: a sub-agent can spawn during
the parent's first turn (before any DONE handler has run), so we
need the session directory and `agents/' subdirectory but must not
write `session.meta.el' yet.  On-disk session state reflects a
completed turn boundary.  The parent's first DONE autosave will
write the sidecar later, picking up any
sub-agent transcript entries that accumulated in the in-memory
slot.

Returns SESSION's `save-path' on success, or nil on failure.  Idempotent."
  (or (mevedel-session-save-path session)
      (condition-case err
          (let* ((_disclosure
                  (when (mevedel-execution-target-remote-p
                         (mevedel-session-execution-target session))
                    (require 'mevedel-session-durability)
                    (mevedel-session-durability-disclose session)))
                 (_workspace-identity
                  (progn
                    (require 'mevedel-workspace-identity)
                    (mevedel-workspace-identity-ensure
                     (mevedel-workspace-root
                      (mevedel-session-workspace session)))))
                 (sessions-dir (mevedel-session-persistence--sessions-dir
                                (mevedel-session-workspace session)))
                 (session-id
                  (mevedel-session-persistence--allocate-session-id
                   (mevedel-session-name session) sessions-dir))
                 (save-path (file-name-as-directory
                             (file-name-concat sessions-dir session-id)))
                 (segment-path (mevedel-session-persistence--segment-path
                                save-path 1))
                 (now (format-time-string "%FT%H-%M-%S")))
            (make-directory save-path t)
            (make-directory (file-name-concat save-path "agents") t)
            (make-directory (file-name-concat save-path "file-history") t)
            (mevedel-session-persistence-lock-acquire
             save-path (buffer-name buffer) session)
            (setf (mevedel-session-session-id session)      session-id)
            (setf (mevedel-session-save-path session)       save-path)
            (setf (mevedel-session-created-at session)      now)
            (setf (mevedel-session-updated-at session)      now)
            (setf (mevedel-session-current-segment session) 1)
            (mevedel-session-persistence--flush-diagnostic-logs session)
            (require 'mevedel-workspace)
            (mevedel-workspace-ensure-generated-state-ignored
             (mevedel-session-workspace session))
            (with-current-buffer buffer
              (unless buffer-file-name
                (setq buffer-file-name segment-path))
              (mevedel-session-persistence--disown-save-machinery))
            save-path)
        (error
         (message "mevedel: shallow session materialization failed: %S" err)
         nil))))

(defun mevedel-session-persistence--record-running-transcript
    (session entry)
  "Insert ENTRY into SESSION's agent-transcripts.  ENTRY is (ID . PLIST)."
  (when (and session (consp entry))
    (setf (alist-get (car entry)
                     (mevedel-session-agent-transcripts session)
                     nil nil #'equal)
          (cdr entry))))

(defun mevedel-session-persistence--update-transcript-entry
    (session agent-id updates)
  "Return nil after merging transcript data into SESSION entry for AGENT-ID.
The argument UPDATES is the change plist."
  (when (and session agent-id)
    (let ((existing (alist-get agent-id
                               (mevedel-session-agent-transcripts session)
                               nil nil #'equal)))
      (when existing
        (let ((merged (copy-sequence existing)))
          (cl-loop for (k v) on updates by #'cddr do
                   (setq merged (plist-put merged k v)))
          (setf (alist-get agent-id
                           (mevedel-session-agent-transcripts session)
                           nil nil #'equal)
                merged))))))

(defun mevedel-session-persistence--write-sidecar-now (session buffer)
  "Best-effort sidecar rewrite for SESSION and BUFFER.

Only writes when the sidecar file already exists on disk -- i.e.
the parent's first DONE has fired and a full materialization has
written `session.meta.el'.  Before that, the session is in shallow
materialization mode (directory + lock + agents/ but no sidecar)
and writing now would violate the completed-turn boundary contract.
In that case the write is deferred to the parent's DONE autosave;
the in-memory `agent-transcripts' slot still reflects current state
and will be picked up by that autosave."
  (when (and session (mevedel-session-save-path session))
    (let* ((sidecar (mevedel-session-persistence--sidecar-path
                     (mevedel-session-save-path session)))
           (portable-p
            (mevedel-session-persistence--portable-authority-p session))
           (present-p
            (if portable-p
                (mevedel-session-persistence-artifact-present-p
                 session "session.meta.el")
              (file-exists-p sidecar))))
      (when present-p
        (condition-case err
            (if portable-p
                (progn
                  (mevedel-session-persistence-assert-mutation-authority
                   session buffer)
                  (require 'mevedel-session-durability)
                  (require 'mevedel-session-publication)
                  (require 'mevedel-session-recovery)
                  (mevedel-session-publication-publish
                   session
                   (list
                    (list
                     :path sidecar
                     :content
                     (mevedel-session-persistence--printed-value
                      (mevedel-session-persistence--build-sidecar
                       session buffer))
                     :commit-marker t))))
              (mevedel-session-persistence-write
               sidecar
               (mevedel-session-persistence--build-sidecar session buffer)))
          (error
           (message "mevedel: sidecar rewrite failed: %S" err)
           nil)
          (:success t))))))

(defun mevedel-session-persistence-save-agent-state (session)
  "Best-effort persist SESSION's agent state through its root data buffer."
  (let* ((save-path (mevedel-session-save-path session))
         (segment-path
          (and save-path
               (mevedel-session-persistence--segment-path
                save-path
                (or (mevedel-session-current-segment session) 1))))
         (buffer (mevedel-session-root-buffer session)))
    (when (and segment-path
               (buffer-live-p buffer)
               (buffer-file-name buffer)
               (equal (expand-file-name (buffer-file-name buffer))
                      (expand-file-name segment-path)))
      (mevedel-session-persistence--write-sidecar-now session buffer))))


;;
;;; Session id and paths

(defun mevedel-session-persistence--sanitize (name)
  "Return NAME with everything outside `[A-Za-z0-9_-]' replaced with `_'."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (or name "")))

(defun mevedel-session-persistence--short-uuid ()
  "Return 4 hex chars derived from random + monotonic clock entropy."
  (substring
   (secure-hash 'sha256
                (format "%s-%s-%s"
                        (random most-positive-fixnum)
                        (float-time)
                        (emacs-pid)))
   0 4))

(defun mevedel-session-persistence--compute-id (name)
  "Compute a fresh session id from NAME.

Format: `<sanitized-name>-<ISO-timestamp>-<short-uuid>'.  ISO timestamp
uses dashes throughout (no colons) so it works on every filesystem and
sorts lexicographically."
  (format "%s-%s-%s"
          (mevedel-session-persistence--sanitize name)
          (format-time-string "%FT%H-%M")
          (mevedel-session-persistence--short-uuid)))

(defun mevedel-session-persistence--sessions-dir (workspace)
  "Return the absolute sessions directory for WORKSPACE.

Resolves `mevedel-sessions-directory' against WORKSPACE's root if the
defcustom is relative; otherwise uses it as-is."
  (let ((dir mevedel-sessions-directory))
    (if (file-name-absolute-p dir)
        (expand-file-name dir)
      (expand-file-name dir (mevedel-workspace-root workspace)))))

(defun mevedel-session-persistence--segment-path (save-path n)
  "Return the absolute path to segment number N under SAVE-PATH.

Segments are zero-padded to four digits (`segment-0001.chat.org')."
  (file-name-concat save-path
                    (format "segment-%04d.chat.org" n)))

(defvar-local mevedel-session--inspection-buffer-p nil
  "Non-nil in a read-only archived-segment inspection buffer.")

(defun mevedel-session-persistence-read-artifact
    (session logical &optional committed-only)
  "Return SESSION artifact LOGICAL as literal bytes.

LOGICAL is a normalized session-relative path.  PID-lock sessions read their
fixed logical file directly.  A portable live owner sees its newest locally
staged write unless COMMITTED-ONLY is non-nil.  Otherwise portable sessions
resolve only through SESSION's captured immutable publication and verify its
recorded hash; fixed portable caches are never an authority fallback."
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (unless (mevedel-session-publication-logical-path-p logical)
    (error "Invalid session artifact path: %S" logical))
  (let ((save-path (or (mevedel-session-save-path session)
                       (error "Session has no persistence path"))))
    (if (not (mevedel-session-persistence--portable-authority-p session))
        (mevedel-file-history--read-file-raw
         (expand-file-name logical save-path))
      (let ((source
             (and (not committed-only)
                  (mevedel-session-durability-lease-owned-p session)
                  (mevedel-session-publication-uncommitted-artifact
                   session logical)))
            expected)
        (unless source
          (let* ((publication
                  (or (mevedel-session-publication session)
                      (setf
                       (mevedel-session-publication session)
                       (mevedel-session-publication-read
                        save-path))))
                 (entry (and publication
                             (cdr (assoc
                                   logical
                                   (plist-get publication :artifacts))))))
            (unless entry
              (error "Session artifact is not published: %s" logical))
            (setq source (plist-get entry :published)
                  expected (plist-get entry :sha256))))
        (let ((content (mevedel-file-history--read-file-raw source)))
          (when (and expected
                     (not (equal expected (secure-hash 'sha256 content))))
            (error
             "Published session artifact failed verification: %s" logical))
          content)))))

(defun mevedel-session-persistence-artifact-present-p
    (session logical &optional committed-only)
  "Return non-nil when SESSION has artifact LOGICAL.

PID-lock sessions consult the fixed logical path.  Portable sessions consult
a live owner's newest staged write or captured immutable publication only.
When COMMITTED-ONLY is non-nil, ignore staged portable writes."
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (unless (mevedel-session-publication-logical-path-p logical)
    (error "Invalid session artifact path: %S" logical))
  (let ((save-path (or (mevedel-session-save-path session)
                       (error "Session has no persistence path"))))
    (if (not (mevedel-session-persistence--portable-authority-p session))
        (file-exists-p (expand-file-name logical save-path))
      (or
       (and (not committed-only)
            (mevedel-session-durability-lease-owned-p session)
            (mevedel-session-publication-uncommitted-artifact session logical))
       (let ((publication
              (or (mevedel-session-publication session)
                  (setf
                   (mevedel-session-publication session)
                   (mevedel-session-publication-read save-path)))))
         (and (assoc logical (plist-get publication :artifacts)) t))))))

(defun mevedel-session-persistence-find-artifact-noselect
    (session logical &optional inspection)
  "Visit SESSION artifact LOGICAL without exposing immutable storage.

Insert resolver-verified bytes into a buffer visiting the qualified logical
path.  When INSPECTION is non-nil, make the buffer read-only and ineligible
for saving."
  (let* ((save-path (or (mevedel-session-save-path session)
                        (error "Session has no persistence path")))
         (path (expand-file-name logical save-path))
         ;; Read first so the fixed remote cache can never select the bytes.
         (content (mevedel-session-persistence-read-artifact session logical))
         (existing
          (and
           (not inspection)
           (cl-find-if
            (lambda (candidate)
              (with-current-buffer candidate
                (and buffer-file-name
                     (equal (expand-file-name buffer-file-name) path)
                     (not (bound-and-true-p
                           mevedel-session--inspection-buffer-p)))))
            (buffer-list))))
         (buffer
          (or existing
              (generate-new-buffer
               (format " *mevedel artifact %s*"
                       (file-name-nondirectory logical))))))
    (with-current-buffer buffer
      (when (and (not inspection) (buffer-modified-p))
        (error "Session artifact buffer has unsaved changes: %s" path))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (decode-coding-string
          content (or buffer-file-coding-system 'utf-8-unix))))
      (unless existing
        (setq buffer-file-name path
              buffer-file-truename path)
        (delay-mode-hooks (set-auto-mode)))
      (require 'mevedel-utilities)
      (mevedel--forget-place)
      (setq-local mevedel-session--inspection-buffer-p inspection)
      (setq-local buffer-offer-save (not inspection))
      (setq buffer-read-only inspection)
      (set-buffer-modified-p nil)
      (set-visited-file-modtime))
    buffer))

(defun mevedel-session-persistence-segments (session live-buffer)
  "Return ordered segment descriptors for SESSION.

LIVE-BUFFER supplies the current segment even when it has not reached disk.
Each descriptor contains `:number', `:path', `:status', `:current-p', and
`:preview'.  Missing and unreadable archived segments remain in the result."
  (let* ((save-path (mevedel-session-save-path session))
         (current (or (mevedel-session-current-segment session) 1))
         (index (mevedel-session-prompt-index session)))
    (cl-loop
     for number from 1 to current
     for path = (and save-path
                     (mevedel-session-persistence--segment-path
                      save-path number))
     for logical = (format "segment-%04d.chat.org" number)
     for current-p = (= number current)
     for prompts = (cdr (assoc number index))
     collect
     (list
      :number number
      :path path
      :status
      (cond
       ((and current-p (buffer-live-p live-buffer)) 'readable)
       ((not path) 'missing)
       ((mevedel-session-persistence--portable-authority-p session)
        (if (mevedel-session-persistence-artifact-present-p session logical)
            'readable
          'missing))
       ((not (file-exists-p path)) 'missing)
       ((and (file-regular-p path) (file-readable-p path)) 'readable)
       (t 'unreadable))
      :current-p current-p
      :preview
      (cl-loop for prompt in (reverse prompts)
               for preview = (plist-get prompt :preview)
               when (and (stringp preview)
                         (not (string-empty-p (string-trim preview))))
               return preview)))))

(defun mevedel-session-persistence-read-segment (session number)
  "Load SESSION segment NUMBER into a read-only inspection buffer.

The returned buffer restores transcript properties but is never authoritative
session state.  Signal a `user-error' naming the exact path when the segment
cannot be read."
  (let* ((save-path (mevedel-session-save-path session))
         (current (or (mevedel-session-current-segment session) 1))
         (logical (format "segment-%04d.chat.org" number))
         (path (and save-path
                    (mevedel-session-persistence--segment-path
                     save-path number))))
    (unless (and (integerp number) (<= 1 number) (<= number current))
      (user-error "Unknown session segment: %s" number))
    (unless (and path
                 (if (mevedel-session-persistence--portable-authority-p session)
                     (mevedel-session-persistence-artifact-present-p
                      session logical)
                   (file-exists-p path)))
      (user-error "Segment file is missing: %s" (or path "(unsaved session)")))
    (unless (or (mevedel-session-persistence--portable-authority-p session)
                (and (file-regular-p path) (file-readable-p path)))
      (user-error "Segment file is unreadable: %s" path))
    (let ((buffer
           (if (mevedel-session-persistence--portable-authority-p session)
               (mevedel-session-persistence-find-artifact-noselect
                session logical t)
             (generate-new-buffer
              (format " *mevedel segment %d*" number)))))
      (condition-case err
          (with-current-buffer buffer
            (setq-local default-directory
                        (or (mevedel-session-working-directory session)
                            save-path))
            (unless (mevedel-session-persistence--portable-authority-p session)
              (insert-file-contents path)
              (delay-mode-hooks (org-mode)))
            (setq-local mevedel-session--inspection-buffer-p t)
            (require 'mevedel-transcript-restore)
            (mevedel-transcript-restore-properties)
            (unless (mevedel-session-persistence--portable-authority-p session)
              (setq buffer-file-name nil))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            buffer)
        (error
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (user-error "Could not read segment %s: %s"
                     path (error-message-string err)))))))

(defun mevedel-session-persistence--sidecar-path (save-path)
  "Return the absolute path to the session sidecar under SAVE-PATH."
  (file-name-concat save-path "session.meta.el"))


;;
;;; Prompt index (used by the rewind picker)

(defun mevedel-session-persistence--content-start (buffer)
  "Return the first buffer position past BUFFER's leading metadata.
Skips an initial org property drawer (:PROPERTIES: ... :END:) so the
prompt walker does not treat the drawer as a user prompt.  Returns
`point-min' when no drawer is present."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                (progn (forward-line 1) (point))
              (point-min))
          (point-min))))))

(defun mevedel-session-persistence--collect-prompts (buffer)
  "Return indexed user prompt plists for BUFFER.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Turns are numbered 1, 2, ... in document order.  Used at
save time to refresh the live segment's entry in
`mevedel-session-prompt-index'.

Skips the initial org property drawer (via
`mevedel-session-persistence--content-start') and any content inside
`#+begin_summary' / `#+end_summary' blocks (the compaction summary
has its body stripped of the `gptel' property but is not a user
prompt).  Also skips unpropertized gptel org tool/reasoning block glue."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (require 'mevedel-transcript-audit)
          (let* ((content-start
                  (mevedel-session-persistence--content-start buffer))
                (directive-ranges
                 (mevedel-transcript-buffer-directive-ranges))
                (turn 0)
                (results nil))
            (dolist (seg (mevedel-transcript-segments
                          content-start
                          (point-max)))
              (pcase-let ((`(,type ,seg-start ,seg-end) seg))
                (when (eq type 'user)
                  (when-let* ((prompt-start
                               (mevedel-transcript--user-prompt-start
                                (max seg-start content-start) seg-end nil)))
                    (let* ((text (buffer-substring-no-properties
                                  prompt-start seg-end))
                           (directive
                            (cl-find-if
                             (lambda (range)
                               (and (>= prompt-start
                                        (plist-get range :body-start))
                                    (< prompt-start
                                       (plist-get range :body-end))))
                             directive-ranges)))
                      (when (string-match "[^[:space:]].*$" text)
                        (cl-incf turn)
                        (push (append
                               (list :turn turn
                                     :pos (if directive
                                              (plist-get directive :start)
                                            prompt-start)
                                     :preview
                                     (truncate-string-to-width
                                      (match-string 0 text)
                                      80 nil nil "..."))
                               (and directive
                                    (list
                                     :kind 'directive
                                     :directive-id
                                     (plist-get directive :directive-id)
                                     :action
                                     (plist-get directive :action)
                                     :outcome
                                     (plist-get directive :outcome)
                                     :activity-kind
                                     (plist-get directive :activity-kind)
                                     :sequence
                                     (plist-get directive :sequence)
                                     :reserved-turn
                                     (plist-get directive :turn))))
                              results)))))))
            (nreverse results)))))))

(defun mevedel-session-persistence--prompt-count-in-text (text)
  "Return the number of user prompt markers detected in TEXT."
  (if (string-empty-p (or text ""))
    0
    (with-temp-buffer
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (insert text)
      (length (mevedel-session-persistence--collect-prompts
               (current-buffer))))))

(defun mevedel-session-persistence--segment-tail-prompt-count ()
  "Return the copied-tail prompt count recorded on the current segment."
  (if (derived-mode-p 'org-mode)
      (max 0 (string-to-number
              (or (org-entry-get (point-min)
                                 "MEVEDEL_SEGMENT_TAIL_PROMPTS")
                  "0")))
    0))

(defun mevedel-session-persistence--new-fork-point-id (session)
  "Return a fresh stable fork-point identity for SESSION."
  (substring
   (secure-hash
    'sha256
    (format "%S"
            (list (mevedel-session-session-id session)
                  (current-time)
                  (random most-positive-fixnum)
                  (emacs-pid))))
   0 32))

(defvar-local mevedel-session-persistence--fork-point-spans-cache nil
  "Cached transcript fork-point spans keyed by modification tick.")

(defun mevedel-session-persistence--fork-point-spans (buffer)
  "Return durable fork-point records and source spans from BUFFER."
  (with-current-buffer buffer
    (let ((tick (buffer-chars-modified-tick)))
      (if (eq tick
              (car-safe
               mevedel-session-persistence--fork-point-spans-cache))
          (cdr mevedel-session-persistence--fork-point-spans-cache)
        (require 'mevedel-transcript-audit)
        (let ((spans
               (mapcar
                (lambda (span)
                  (append
                   (copy-sequence (plist-get span :record))
                   (list
                    :record-start
                    (+ (point-min) (plist-get span :start))
                    :transcript-cutoff
                    (+ (point-min) (plist-get span :end)))))
                (mevedel-transcript-audit-spans
                 (buffer-string) 'fork-point))))
          (setq mevedel-session-persistence--fork-point-spans-cache
                (cons tick spans))
          spans)))))

(defun mevedel-session-persistence-fork-point-at-source
    (buffer source-start source-end)
  "Return the stable fork point inside BUFFER source bounds.

SOURCE-START and SOURCE-END must bound one rendered assistant turn."
  (cl-find-if
   (lambda (fork-point)
     (and (<= source-start (plist-get fork-point :record-start))
          (<= (plist-get fork-point :transcript-cutoff) source-end)))
   (mevedel-session-persistence--fork-point-spans buffer)))

(defun mevedel-session-persistence--prompt-fork-point
    (prompt next-position fork-points)
  "Return PROMPT's fork point before NEXT-POSITION from FORK-POINTS."
  (cl-find-if
   (lambda (fork-point)
     (let ((start (plist-get fork-point :record-start)))
       (and (> start (plist-get prompt :pos))
            (or (null next-position) (< start next-position)))))
   fork-points))

(defun mevedel-session-persistence--update-prompt-index (session buffer)
  "Refresh the live segment's prompt list in SESSION from BUFFER's contents.

Operates only on the current segment; previous (finalized) segments
keep their pre-recorded entries.  Idempotent -- safe to call on every
save.

Each prompt plist gets a `:cum-turn' field equal to the prompt's
sequence number across the entire session.  Prompts copied forward as
compaction tail are skipped because they are already indexed in the
predecessor segment.  The cumulative
number is what `:file-snapshots' is keyed by, so the restore plan
can map a picker selection to the checkpoint from immediately before
that prompt's model turn."
  (let* ((current-seg (or (mevedel-session-current-segment session) 1))
         (index       (mevedel-session-prompt-index session))
         (cell        (assoc current-seg index))
         (offset
          (cl-loop for (seg . prompts) in index
                   when (< seg current-seg)
                   sum (length prompts)))
         (raw-all     (mevedel-session-persistence--collect-prompts buffer))
         (tail-count  (with-current-buffer buffer
                        (mevedel-session-persistence--segment-tail-prompt-count)))
         (skip-count  (min tail-count (length raw-all)))
         (raw         (nthcdr skip-count raw-all))
         (fork-points
          (mevedel-session-persistence--fork-point-spans buffer))
         (with-cum    (cl-loop for remaining on raw
                               for p = (car remaining)
                               for next-position =
                               (plist-get (cadr remaining) :pos)
                               for turn from 1
                               collect
                               (let ((copy (copy-sequence p)))
                                 (plist-put copy :turn turn)
                                 (plist-put copy :file-turn
                                            (+ skip-count turn))
                                 (plist-put copy :cum-turn (+ offset turn))
                                 (when (and (eq (plist-get copy :kind)
                                                'directive)
                                            (/= (plist-get copy
                                                           :reserved-turn)
                                                (plist-get copy :cum-turn)))
                                   (error
                                    "Directive turn %d disagrees with prompt index turn %d"
                                    (plist-get copy :reserved-turn)
                                    (plist-get copy :cum-turn)))
                                 (when-let* ((fork-point
                                              (mevedel-session-persistence--prompt-fork-point
                                               p next-position fork-points)))
                                   (dolist (key '(:fork-point-id
                                                  :transcript-cutoff))
                                     (plist-put copy key
                                                (plist-get fork-point key))))
                                 copy))))
    (if cell
        (setcdr cell with-cum)
      (setf (mevedel-session-prompt-index session)
            (cons (cons current-seg with-cum) index)))))

(defun mevedel-session-persistence--ensure-latest-fork-point
    (session buffer)
  "Attach a durable fork-point marker to SESSION's latest response in BUFFER."
  (let* ((segment (or (mevedel-session-current-segment session) 1))
         (prompt (car (last (cdr (assoc
                                  segment
                                  (mevedel-session-prompt-index session)))))))
    (when (and prompt (not (plist-get prompt :fork-point-id)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (plist-get prompt :pos))
          (when (text-property-search-forward 'gptel 'response t)
            (require 'mevedel-transcript-audit)
            (goto-char (point-max))
            (insert
             (mevedel--format-hook-audit-record
              (list :type 'fork-point
                    :fork-point-id
                    (mevedel-session-persistence--new-fork-point-id session)
                    :segment segment
                    :turn (plist-get prompt :turn)
                    :file-turn (plist-get prompt :file-turn)
                    :cum-turn (plist-get prompt :cum-turn)
                    :captured-file-turn (plist-get prompt :cum-turn))))
            t))))))

(defun mevedel-session-persistence--newer-prompt-p (candidate incumbent)
  "Return non-nil when CANDIDATE is newer than INCUMBENT."
  (or (null incumbent)
      (> (plist-get candidate :cum-turn)
         (plist-get incumbent :cum-turn))))

(defun mevedel-session-persistence--latest-user-message-from-index (index)
  "Return the newest non-empty prompt preview from prompt INDEX, or nil."
  (let (best)
    (dolist (entry index)
      (let ((segment (car entry)))
        (dolist (prompt (cdr entry))
          (when (consp prompt)
            (let ((preview (plist-get prompt :preview)))
              (when (and (stringp preview)
                         (not (string-empty-p (string-trim preview))))
                (let ((candidate (copy-sequence prompt)))
                  (plist-put candidate :segment segment)
                  (when (mevedel-session-persistence--newer-prompt-p
                         candidate best)
                    (setq best candidate)))))))))
    (plist-get best :preview)))


;;
;;; First user message extraction

(defun mevedel-session-persistence--first-user-message (buffer)
  "Return a one-line preview of the first user prompt in BUFFER, or nil.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Skips the initial org property drawer and
`#+begin_summary' / `#+end_summary' block bodies, so the picker preview
reflects an actual user prompt rather than metadata, a compaction
summary, or tool/reasoning block glue.  The preview is the first
non-empty line, truncated to 120 characters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (catch 'found
            (let ((pos (mevedel-session-persistence--content-start buffer)))
              (while (< pos (point-max))
                (let* ((next (next-single-property-change
                              pos 'gptel nil (point-max)))
                       (prop (get-text-property pos 'gptel)))
                  (when-let* ((prompt-start
                               (mevedel-transcript--user-prompt-start
                                pos next prop)))
                    (let ((text (buffer-substring-no-properties
                                 prompt-start next)))
                      (when (string-match "[^[:space:]].*$" text)
                        (let ((line (match-string 0 text)))
                          (throw 'found
                                 (truncate-string-to-width
                                  line 120 nil nil "..."))))))
                  (setq pos next)))
              nil)))))))


;;
;;; Buffer selection

(defun mevedel-session-persistence--notify-session-event
    (session event &rest args)
  "Send semantic lifecycle EVENT to SESSION's registered observers."
  (require 'mevedel-session-control-transfer)
  (apply #'mevedel-session-control-transfer-notify session event args))

(defun mevedel-session-persistence--root-data-buffer-p (buffer)
  "Return non-nil when BUFFER is an explicitly registered session root.

Unassociated ordinary buffers remain eligible for low-level inspection, but
session buffers must cross the session-owned root registration seam."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((session (and (boundp 'mevedel--session)
                             mevedel--session)))
           (and (not (bound-and-true-p mevedel--agent-invocation))
                (or (and session
                         (eq buffer (mevedel-session-root-buffer session)))
                    (and (null session)
                         (not (bound-and-true-p
                               mevedel-session--inspection-buffer-p)))))))))

(defun mevedel-session-persistence--authoritative-buffer (buffer)
  "Return the authoritative session root for BUFFER, or nil.

The root registration is the only projection boundary persistence needs to
know.  View buffers never become roots because an initialized view has
already registered the session-owned data buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((session (and (boundp 'mevedel--session) mevedel--session)))
        (cond
         ((bound-and-true-p mevedel--agent-invocation) nil)
         ((and session (buffer-live-p (mevedel-session-root-buffer session)))
          (mevedel-session-root-buffer session))
         ((mevedel-session-persistence--root-data-buffer-p buffer)
          buffer))))))

(defun mevedel-session-persistence--root-buffer-for-session
  (session &optional buffer)
  "Return SESSION's live root data buffer, preferring BUFFER."
  (or (let ((registered (mevedel-session-root-buffer session)))
        (and (buffer-live-p registered) registered))
      (let ((candidate
             (mevedel-session-persistence--authoritative-buffer buffer)))
        (and candidate
             (eq session
                 (buffer-local-value 'mevedel--session candidate))
             candidate))))


;;
;;; Lazy materialization

(defun mevedel-session-persistence-ensure-files (session buffer)
  "Lazily materialize SESSION's on-disk artifacts.

If SESSION has no `save-path' yet, allocate a fresh session id,
create the session directory tree (session dir + `agents/' +
`file-history/'), acquire its explicit mutation authority (a portable
lease for project sessions or a `.lock' for file-workspace sessions), set
BUFFER's variable `buffer-file-name' to the first segment file, and save the
buffer.

Does NOT write the sidecar -- the caller (always
`mevedel-session-persistence-save') is expected to do that once it
has updated the prompt-index and snapshot maps.  Keeping the write
in one place avoids double-writing the sidecar on first save.

Idempotent -- if SESSION already has a `save-path', repairs BUFFER's
variable `buffer-file-name' so it visits the current segment before any save.
Returns SESSION's `save-path' (allocated or existing)."
  (let* ((existing-save-path (mevedel-session-save-path session))
           (save-path
            (or existing-save-path
                (let* ((_disclosure
                        (when (mevedel-execution-target-remote-p
                               (mevedel-session-execution-target session))
                          (require 'mevedel-session-durability)
                          (mevedel-session-durability-disclose session)))
                       (_workspace-identity
                        (progn
                          (require 'mevedel-workspace-identity)
                          (mevedel-workspace-identity-ensure
                           (mevedel-workspace-root
                            (mevedel-session-workspace session)))))
                       (sessions-dir
                        (mevedel-session-persistence--sessions-dir
                         (mevedel-session-workspace session)))
                       (session-id
                        (mevedel-session-persistence--allocate-session-id
                         (mevedel-session-name session) sessions-dir))
                       (new-save-path
                        (file-name-as-directory
                         (file-name-concat sessions-dir session-id)))
                       (now (format-time-string "%FT%H-%M-%S")))
                  (make-directory new-save-path t)
                  (make-directory (file-name-concat new-save-path "agents") t)
                  (make-directory (file-name-concat new-save-path "file-history") t)
                  (mevedel-session-persistence-lock-acquire
                   new-save-path (buffer-name buffer) session)
                  (setf (mevedel-session-session-id session)      session-id)
                  (setf (mevedel-session-save-path session)       new-save-path)
                  (setf (mevedel-session-created-at session)      now)
                  (setf (mevedel-session-updated-at session)      now)
                  (setf (mevedel-session-current-segment session) 1)
                  (mevedel-session-persistence--flush-diagnostic-logs session)
                  (require 'mevedel-workspace)
                  (mevedel-workspace-ensure-generated-state-ignored
                   (mevedel-session-workspace session))
                  (setf (mevedel-session-durable-tree-ensured session) t)
                  new-save-path)))
           (segment-number (or (mevedel-session-current-segment session) 1))
           (segment-path (mevedel-session-persistence--segment-path
                          save-path segment-number)))
      ;; The tree and the workspace ignore entries are established once per
      ;; process.  Re-establishing them on every save is several target round
      ;; trips plus a re-parse of the ignore file for a settled answer.
      (unless (mevedel-session-durable-tree-ensured session)
        (make-directory save-path t)
        (make-directory (file-name-concat save-path "agents") t)
        (make-directory (file-name-concat save-path "file-history") t)
        (require 'mevedel-workspace)
        (mevedel-workspace-ensure-generated-state-ignored
         (mevedel-session-workspace session))
        (setf (mevedel-session-durable-tree-ensured session) t))
      (require 'mevedel-session-control-transfer)
      (when (and (buffer-live-p buffer)
                 (eq buffer (mevedel-session-root-buffer session)))
        (mevedel-session-control-transfer-register-root-buffer session buffer))
      (with-current-buffer buffer
        (unless (and buffer-file-name
                     (equal (expand-file-name buffer-file-name)
                            (expand-file-name segment-path)))
          (setq buffer-file-name segment-path))
        (mevedel-session-persistence--disown-save-machinery)
        (unless (file-exists-p segment-path)
          (set-buffer-modified-p t)
          (unless (mevedel-session-persistence--portable-authority-p session)
            (save-buffer))))
      save-path))

(defvar mevedel-session-persistence--checking-incarnation nil
  "Non-nil while publishing a replacement target incarnation.")

(defun mevedel-session-persistence--observe-target-incarnation
    (target sandbox-mode)
  "Refresh TARGET's incarnation observation and return its readiness.

Admission needs the replacement fingerprint, not the whole readiness suite:
environment, capabilities, and sandbox facts are fixed for the life of a
connection, while the fingerprint is one target command.  A first probe or a
reconnect still runs the full probe, because a new connection may be a new
target.  A failed observation falls back to the full probe, which settles a
blocked readiness the caller reports."
  (require 'mevedel-execution-target)
  (let ((readiness (mevedel-execution-target-probe target nil sandbox-mode)))
    (if (not (eq 'ready (plist-get readiness :status)))
        readiness
      (condition-case nil
          (progn
            (mevedel-execution-target-observe-incarnation target)
            readiness)
        (error (mevedel-execution-target-probe target t sandbox-mode))))))

(defun mevedel-session-persistence--check-target-incarnation
    (session buffer)
  "Fence and publish a replacement execution target for SESSION.

BUFFER identifies SESSION's live root data buffer.  The fresh probe runs at
every durable mutation boundary; unchanged targets take no durability I/O."
  (unless mevedel-session-persistence--checking-incarnation
    (let ((buffer
           (or buffer
               (and (boundp 'mevedel--session)
                    (eq mevedel--session session)
                    (current-buffer)))))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (boundp 'mevedel--session)
                        (eq mevedel--session session)
                        (not (bound-and-true-p mevedel--agent-invocation)))))
        (when (or (null (mevedel-session-root-buffer session))
                  (eq buffer (mevedel-session-root-buffer session)))
          (require 'mevedel-session-control-transfer)
          (mevedel-session-control-transfer-register-root-buffer
           session buffer)))
      (when-let ((target (mevedel-session-execution-target session)))
        (require 'mevedel-execution-target)
        (unless (mevedel-execution-target-incarnation-changed-p target)
          (if (mevedel-execution-target-remote-p target)
              (let ((readiness
                     (or (mevedel-session-persistence--observe-target-incarnation
                          target (mevedel-session-sandbox-mode session))
                         (mevedel-execution-target-readiness target))))
                (unless (eq 'ready (plist-get readiness :status))
                  (user-error "Execution target is not ready: %s"
                              (or (plist-get readiness :error)
                                  (plist-get readiness :reason)
                                  "probe failed"))))
            (mevedel-execution-target-refresh-incarnation target)))
        (when (mevedel-execution-target-incarnation-changed-p target)
          (let ((root-buffer
                 (mevedel-session-persistence--root-buffer-for-session
                  session buffer)))
            (unless (buffer-live-p root-buffer)
              (error "Target replacement requires the live root session buffer"))
            (let ((mevedel-session-persistence--checking-incarnation t))
              (require 'mevedel-permissions)
              (mevedel-permission-invalidate-target-grants session)
              (mevedel-execution-target-prepare-incarnation-acknowledgement
               target)
              (when (mevedel-session-save-path session)
                (if (mevedel-session-persistence--portable-authority-p session)
                    (when (mevedel-session-persistence-artifact-present-p
                           session "session.meta.el" t)
                      (mevedel-session-persistence-publish-sidecar-state
                       session root-buffer))
                  (let ((sidecar
                         (mevedel-session-persistence--sidecar-path
                          (mevedel-session-save-path session))))
                    (when (file-exists-p sidecar)
                      (mevedel-session-persistence-write
                       sidecar
                       (mevedel-session-persistence--build-sidecar
                        session root-buffer))))))
              (mevedel-execution-target-acknowledge-incarnation target))))))))

(defun mevedel-session-persistence-assert-mutation-authority
    (session &optional buffer)
  "Return non-nil when SESSION may start durable mutation from BUFFER.

Pending critical publication blocks every session.  A portable project session
also materializes its directory when necessary and must own its live lease.
Every execution target is incarnation-fenced before mutation.  Shallow
materialization always uses SESSION's root buffer."
  (let ((buffer
         (or buffer
             (and (boundp 'mevedel--session)
                  (eq mevedel--session session)
                  (current-buffer)))))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (boundp 'mevedel--session)
                      (eq mevedel--session session)
                      (not (bound-and-true-p mevedel--agent-invocation))
                      (not (buffer-live-p
                            (mevedel-session-root-buffer session))))))
      (require 'mevedel-session-control-transfer)
      (mevedel-session-control-transfer-register-root-buffer
       session buffer))
    (let ((target (mevedel-session-execution-target session)))
      (when (and target
                 (mevedel-session-persistence--portable-authority-p session))
        (require 'mevedel-session-durability)
        (require 'mevedel-session-recovery)
        (mevedel-session-recovery-refresh session)))
    (when (mevedel-session-pending-publication session)
      (user-error "Session has pending publication; retry or abandon it first"))
    (let* ((target (mevedel-session-execution-target session))
           (portable-p
            (and target
                 (mevedel-session-persistence--portable-authority-p session))))
      (when (and target portable-p)
        (let* ((root-buffer
                (mevedel-session-persistence--root-buffer-for-session
                 session buffer)))
          (require 'mevedel-session-durability)
          (mevedel-session-durability-disclose session)
          ;; Every portable mutation must have a materialized session
          ;; directory before it is admitted.
          (unless (or (mevedel-session-save-path session)
                      (and root-buffer
                           (mevedel-session-persistence--shallow-ensure-files
                            session root-buffer)))
            (user-error "Could not materialize portable session state"))
          (unless (or (mevedel-session-durability-lease-owned-p session)
                      (mevedel-session-persistence-lock-acquire
                       (mevedel-session-save-path session)
                       (buffer-name (or root-buffer buffer))
                       session))
            (user-error "Portable session is leased by another client"))
          (mevedel-session-persistence--check-target-incarnation
           session buffer)))
      (when (and target (not portable-p))
        (mevedel-session-persistence--check-target-incarnation
         session (or buffer (current-buffer)))))
    t))

(defun mevedel-session-persistence-assert-new-mutation-authority (session)
  "Reject a new mutation while SESSION is quiescing for control transfer.

Existing requests and durability settlement continue through the ordinary
authority gate; callers use this narrower guard at admission boundaries."
  ;; Hydrate the durable transfer state before admitting a restarted owner.
  ;; Do not run the owner-side drain/release path here: admission must only
  ;; observe state, and settlement remains responsible for releasing it.
  (let ((state (plist-get (mevedel-session-control-transfer session) :state)))
    (when (memq state '(quiescing released))
      (user-error "Session is quiescing for cooperative control transfer")))
  (when (and (mevedel-session-persistence--portable-authority-p session)
             (mevedel-session-save-path session)
             (mevedel-session-session-id session))
    (require 'mevedel-session-control-transfer)
    (let ((state
           (plist-get
            (mevedel-session-control-transfer-observe session) :state)))
      (when (memq state '(quiescing released))
        (user-error "Session is quiescing for cooperative control transfer"))))
  (mevedel-session-persistence-assert-mutation-authority session))


;;
;;; Sidecar build helper

(defun mevedel-session-persistence--persisted-first-user-message (session)
  "Return SESSION's already persisted first user preview, or nil.

The committed field never changes once a session has a first turn, so the
first successful read is cached on the session rather than re-read and
re-verified on every save."
  (or (mevedel-session-persisted-first-user-message session)
      (when-let* ((save-path (mevedel-session-save-path session)))
        (condition-case nil
            (let ((sidecar
                   (if (mevedel-session-persistence--portable-authority-p
                        session)
                       (with-temp-buffer
                         (insert
                          (decode-coding-string
                           (mevedel-session-persistence-read-artifact
                            session "session.meta.el")
                           'utf-8-unix))
                         (read (current-buffer)))
                     (let ((path
                            (mevedel-session-persistence--sidecar-path
                             save-path)))
                       (and (file-exists-p path)
                            (mevedel-session-persistence-read path))))))
              (setf (mevedel-session-persisted-first-user-message session)
                    (plist-get sidecar :first-user-message)))
          (error nil)))))

(defun mevedel-session-persistence--build-sidecar (session buffer)
  "Build the sidecar plist for SESSION using BUFFER for ancillary fields."
  (let* ((first-preview
          (or (mevedel-session-persistence--persisted-first-user-message
               session)
              ;; The buffer's first message is exactly what this sidecar
              ;; is about to persist, and the committed field never
              ;; changes afterwards -- so it is authoritative now.
              ;; Without this, an owner whose committed sidecar predates
              ;; its first turn re-reads the whole artifact over TRAMP
              ;; on every save, twice.
              (when-let* ((preview
                           (mevedel-session-persistence--first-user-message
                            buffer)))
                (setf (mevedel-session-persisted-first-user-message
                       session)
                      preview))))
         (latest-preview
          (or (mevedel-session-persistence--latest-user-message-from-index
               (mevedel-session-prompt-index session))
              first-preview))
         (roots (when (buffer-live-p buffer)
                  (buffer-local-value 'mevedel-workspace-additional-roots
                                      buffer))))
    (mevedel-session-persistence-serialize
     session
     :first-user-message  first-preview
     :latest-user-message latest-preview
     :additional-roots   roots)))


;;
;;; Fast Org property writes

(defun mevedel-session-persistence--top-level-pom-p (pom)
  "Return non-nil when POM points at the top-level property drawer."
  (cond
   ((integerp pom) (= pom (point-min)))
   ((markerp pom) (= (marker-position pom) (point-min)))
   (t nil)))

(defun mevedel-session-persistence--property-drawer-region ()
  "Return (START . END) for the initial Org property drawer, or nil.
END is the position just after the `:END:' line."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
        (let ((start (line-beginning-position)))
          (forward-line 1)
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (forward-line 1)
            (cons start (point))))))))

(defun mevedel-session-persistence--ensure-property-drawer ()
  "Return the initial Org property drawer region, creating it if needed."
  (or (mevedel-session-persistence--property-drawer-region)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert ":PROPERTIES:\n:END:\n"))
          (mevedel-session-persistence--property-drawer-region)))))

(defun mevedel-session-persistence--property-delete-direct (property)
  "Delete PROPERTY from the initial Org property drawer without Org parsing."
  (when-let* ((region (mevedel-session-persistence--property-drawer-region)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t)
              (regexp (format "^[ \t]*:%s\\+?:[ \t]*.*$"
                              (regexp-quote property)))
              (end-marker
               (copy-marker
                (save-excursion
                  (goto-char (cdr region))
                  (forward-line -1)
                  (line-beginning-position))
                t))
              (drawer-end-marker (copy-marker (cdr region) t))
              (inhibit-read-only t)
              removed)
          (unwind-protect
              (progn
                (goto-char (car region))
                (forward-line 1)
                (while (re-search-forward regexp
                                          (marker-position end-marker) t)
                  (setq removed t)
                  (delete-region
                   (line-beginning-position)
                   (save-excursion
                     (forward-line 1)
                     (while (and (< (point) (marker-position end-marker))
                                 (not (looking-at-p
                                       "[ \t]*:[^: \t\n][^:\n]*:[ \t]*.*$")))
                       (forward-line 1))
                     (point))))
                (when (and removed
                           (= (save-excursion
                                (goto-char (car region))
                                (forward-line 1)
                                (point))
                              (marker-position end-marker)))
                  (delete-region (car region)
                                 (marker-position drawer-end-marker)))
                removed)
            (set-marker end-marker nil)
            (set-marker drawer-end-marker nil)))))))

(defun mevedel-session-persistence--property-put-direct (property value)
  "Set PROPERTY to VALUE in the initial Org property drawer without Org parsing."
  (mevedel-session-persistence--property-delete-direct property)
  (when-let* ((region (mevedel-session-persistence--ensure-property-drawer)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t))
          (goto-char (car region))
          (forward-line 1)
          (insert (format ":%s: %s\n" property value)))))))

(defun mevedel-session-persistence--with-fast-property-writes (fn)
  "Call FN while routing top-level Org property writes through text helpers."
  (let ((orig-put (symbol-function 'org-entry-put))
        (orig-delete (symbol-function 'org-entry-delete)))
    (cl-letf (((symbol-function 'org-entry-put)
               (lambda (pom property value)
                 (if (mevedel-session-persistence--top-level-pom-p pom)
                     (mevedel-session-persistence--property-put-direct
                      property value)
                   (funcall orig-put pom property value))))
              ((symbol-function 'org-entry-delete)
               (lambda (pom property)
                 (if (mevedel-session-persistence--top-level-pom-p pom)
                     (mevedel-session-persistence--property-delete-direct
                      property)
                   (funcall orig-delete pom property)))))
      (funcall fn))))


(defun mevedel-session-persistence--dynamic-system-preset-p ()
  "Return non-nil if the current gptel preset can recreate the system prompt.

gptel can use function-valued or dynamic-spec `:system' entries at
runtime, but its Org persistence evaluates them into a frozen
`GPTEL_SYSTEM' string.  Mevedel session files should keep the preset
reference and drop that frozen override only when the preset can
recreate the system prompt on restore."
  (when (and (boundp 'gptel--preset)
             gptel--preset
             (fboundp 'gptel-get-preset))
    (when-let* ((preset-spec (gptel-get-preset gptel--preset))
                ((plist-member preset-spec :system)))
      (let ((system (plist-get preset-spec :system)))
        (or (functionp system)
            (and (consp system)
                 (keywordp (car system))))))))

(defun mevedel-session-persistence--stabilize-gptel-bounds ()
  "Rewrite `GPTEL_BOUNDS' until Org property drawer offsets settle.

`gptel-org--save-state' persists absolute buffer positions.  Updating
the Org property drawer can itself move every marked transcript region,
so a single write can save positions that were correct for the previous
drawer size.  Recompute after each write and stop once the serialized
bounds no longer change."
  (when (and (derived-mode-p 'org-mode)
             (require 'org nil t)
             (fboundp 'gptel--get-buffer-bounds))
    (save-excursion
      (save-restriction
        (widen)
        (let ((last nil)
              (done nil)
              (attempts 0))
          (while (and (not done) (< attempts 8))
            (setq attempts (1+ attempts))
            (mevedel-transcript-normalize-properties)
            (let ((serialized
                   (when-let* ((bounds (gptel--get-buffer-bounds)))
                     (prin1-to-string bounds))))
              (cond
               ((null serialized)
                (mevedel-session-persistence--property-delete-direct
                 "GPTEL_BOUNDS")
                (setq done t))
               ((equal serialized last)
                (setq done t))
               (t
                (setq last serialized)
                (mevedel-session-persistence--property-put-direct
                 "GPTEL_BOUNDS" serialized))))))))))

(defun mevedel-session-persistence--save-gptel-state-around (orig-fun &rest args)
  "Call ORIG-FUN with ARGS without freezing dynamic system prompt values.

This is an around-advice for `gptel--save-state'.  For non-mevedel
buffers and static prompts it delegates unchanged.  For retained agents
and mevedel chat buffers using presets with dynamic `:system' values, it
removes any existing `GPTEL_SYSTEM' first and dynamically binds
`gptel-system-prompt' to nil while gptel writes its Org metadata.
After delegation, it rewrites `GPTEL_BOUNDS' until the saved absolute
positions match the post-drawer-update buffer.  If the metadata changed
the buffer size, it shifts the bound view's source coordinates by the same
amount so disclosures continue to address the intended transcript segments."
  (let ((size-before (buffer-size))
        (mevedel-org-buffer-p
         (and (bound-and-true-p mevedel--session)
              (derived-mode-p 'org-mode))))
    (prog1
        (if mevedel-org-buffer-p
            (mevedel-session-persistence--with-fast-property-writes
             (lambda ()
               (if (and (or (bound-and-true-p mevedel--agent-invocation)
                            (mevedel-session-persistence--dynamic-system-preset-p))
                        (require 'org nil t))
                   (save-excursion
                     (save-restriction
                       (widen)
                       (mevedel-session-persistence--property-delete-direct
                        "GPTEL_SYSTEM")
                       (let ((gptel-system-prompt nil))
                         (apply orig-fun args))))
                 (apply orig-fun args))))
          (apply orig-fun args))
      (when mevedel-org-buffer-p
        (mevedel-session-persistence--stabilize-gptel-bounds)
        (let ((delta (- (buffer-size) size-before)))
          (when (/= delta 0)
            (mevedel-session-persistence--notify-session-event
             mevedel--session 'rebase-data-sources delta)))))))

(defun mevedel-session-persistence--install-gptel-save-state-advice ()
  "Install mevedel's dynamic-system preservation advice for gptel save operations."
  (unless (advice-member-p
           #'mevedel-session-persistence--save-gptel-state-around
           'gptel--save-state)
    (advice-add 'gptel--save-state :around
                #'mevedel-session-persistence--save-gptel-state-around)))


;;
;;; Instruction snapshots

(defun mevedel-session-persistence--instructions-dir (save-path)
  "Return the instruction snapshot directory under SAVE-PATH."
  (file-name-concat save-path "instructions"))

(defun mevedel-session-persistence--instructions-current-path (save-path)
  "Return the current instruction snapshot path under SAVE-PATH."
  (file-name-concat
   (mevedel-session-persistence--instructions-dir save-path)
   "current.el"))

(defun mevedel-session-persistence--instructions-turn-path (save-path turn)
  "Return the instruction snapshot path for TURN under SAVE-PATH."
  (file-name-concat
   (mevedel-session-persistence--instructions-dir save-path)
   (format "turn-%06d.el" turn)))

(defun mevedel-session-persistence--save-instructions
    (session buffer &optional current-only)
  "Persist current instruction state for SESSION and BUFFER.

Writes `instructions/current.el' and, unless CURRENT-ONLY is non-nil, a
turn-specific snapshot used by rewind/fork when the session has a turn count."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (require 'mevedel-persistence)
    (let ((dir (mevedel-session-persistence--instructions-dir save-path))
          (workspace-root (mevedel-workspace-root
                           (mevedel-session-workspace session)))
          (turn (mevedel-session-turn-count session)))
      (make-directory dir t)
      (with-current-buffer buffer
        (mevedel--write-instructions-file
         (mevedel-session-persistence--instructions-current-path save-path)
         workspace-root t t t)
        (when (and (not current-only) (integerp turn))
          (mevedel--write-instructions-file
           (mevedel-session-persistence--instructions-turn-path save-path turn)
           workspace-root t t nil))))))

(defun mevedel-session-persistence--load-instructions
    (session buffer &optional turn directive-records preserve-directives-p)
  "Restore SESSION instruction snapshot into BUFFER's workspace.

When TURN is non-nil, restore the turn-specific snapshot; otherwise restore
`instructions/current.el'.  DIRECTIVE-RECORDS preserves current authored
directive identity and activity while replacing historical presentations.
PRESERVE-DIRECTIVES-P applies that behavior even when the record list is empty.
Missing snapshots clear presentations only in preservation mode."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (let* ((path (if turn
                     (mevedel-session-persistence--instructions-turn-path
                      save-path turn)
                   (mevedel-session-persistence--instructions-current-path
                    save-path)))
           (logical (file-relative-name path save-path))
           (portable-p
            (mevedel-session-persistence--portable-authority-p session))
           (present-p
            (if portable-p
                (mevedel-session-persistence-artifact-present-p
                 session logical)
              (file-exists-p path))))
      (require 'mevedel-persistence)
      (if present-p
        (condition-case err
            (let ((source path)
                  inspection
                  temporary)
              (unwind-protect
                  (progn
                    (when portable-p
                      (setq
                       inspection
                       (mevedel-session-persistence-find-artifact-noselect
                        session logical t)
                       temporary
                       (make-temp-file "mevedel-instructions-" nil ".el")
                       source temporary)
                      (with-current-buffer inspection
                        (let ((coding-system-for-write
                               (or buffer-file-coding-system 'utf-8-unix)))
                          (write-region
                           (point-min) (point-max) temporary nil 'silent))))
                    (with-current-buffer buffer
                      (mevedel--load-instructions-file
                       source
                       (mevedel-workspace-root
                        (mevedel-session-workspace session))
                       nil t
                       (mevedel-session-workspace session)
                       directive-records preserve-directives-p)))
                (when (buffer-live-p inspection)
                  (kill-buffer inspection))
                (when (and temporary (file-exists-p temporary))
                  (delete-file temporary))))
          (error
           (display-warning
            'mevedel
            (format "Could not restore instruction snapshot %s: %s"
                    path (error-message-string err))
            :warning)
           nil))
        (when (or preserve-directives-p directive-records)
          (mevedel--reset-instructions-preserving-directives
           (mevedel-session-workspace session) directive-records)
          (mevedel--restore-preserved-directives
           (mevedel-session-workspace session))
          (list :restored 0 :lost 0 :total 0))))))


;;
;;; Per-turn save

(defvar mevedel-session-persistence--critical-artifacts nil
  "Dynamically collected target artifacts for one portable publication.")

(defvar mevedel-session-persistence--collecting-critical-artifacts-p nil
  "Non-nil while portable save captures durability-critical file writes.")

(defun mevedel-session-persistence--printed-value (value)
  "Return VALUE in the package's durable Lisp representation."
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil)
          (print-circle t)
          (print-quoted t))
      (prin1 value (current-buffer)))
    (buffer-string)))

(defun mevedel-session-persistence-publish-text
    (session path content &optional coding)
  "Publish SESSION's durability-critical CONTENT atomically at PATH.

Portable project writes enter the session publication queue.  File-workspace
writes retain the existing same-filesystem temporary-file and rename
behavior.  Return the publication outcome, or PATH after a direct write."
  (if (mevedel-session-persistence--portable-authority-p session)
      (progn
        (mevedel-session-persistence-assert-mutation-authority session)
        (require 'mevedel-session-durability)
        (require 'mevedel-session-publication)
        (mevedel-session-publication-publish
         session (list (list :path path :content content :coding coding))))
    (progn
      (make-directory (file-name-directory (expand-file-name path)) t)
      (with-temp-buffer
        (setq buffer-file-coding-system (or coding 'utf-8-unix))
        (insert content)
        (mevedel-session-persistence--write-current-buffer-atomically path))
      path)))

(defun mevedel-session-persistence--sidecar-publication-artifact
    (session root-buffer)
  "Return SESSION's freshly built sidecar marker artifact.

SESSION must use portable authority and be materialized.  ROOT-BUFFER must be
its live root data buffer, and the current immutable publication must contain
the sidecar.  Mutation authority is checked before the artifact is built."
  ;; An explicit root argument is the registration seam for callers that
  ;; materialize a session without going through view initialization.
  (when (and (buffer-live-p root-buffer)
             (with-current-buffer root-buffer
               (and (boundp 'mevedel--session)
                    (eq mevedel--session session)
                    (not (bound-and-true-p mevedel--agent-invocation)))))
    (require 'mevedel-session-control-transfer)
    (mevedel-session-control-transfer-register-root-buffer
     session root-buffer))
  (unless (and (buffer-live-p root-buffer)
               (mevedel-session-persistence--root-data-buffer-p root-buffer)
               (eq session
                   (buffer-local-value 'mevedel--session root-buffer)))
    (error "Session state publication requires the live root session buffer"))
  (unless (and (mevedel-session-save-path session)
               (mevedel-session-execution-target session)
               (mevedel-session-persistence--portable-authority-p session))
    (error "Session state publication requires a portable materialized session"))
  (require 'mevedel-session-durability)
  (unless (mevedel-session-persistence-artifact-present-p
           session "session.meta.el" t)
    (error "Portable session sidecar is not published"))
  (mevedel-session-persistence-assert-mutation-authority
   session root-buffer)
  (list
   :path
   (mevedel-session-persistence--sidecar-path
    (mevedel-session-save-path session))
   :content
   (mevedel-session-persistence--printed-value
    (mevedel-session-persistence--build-sidecar session root-buffer))
   :commit-marker t))

(defun mevedel-session-persistence-publish-sidecar-state
    (session root-buffer)
  "Publish SESSION's freshly built sidecar as one strict commit.

ROOT-BUFFER must be SESSION's live root data buffer.  Publication and
authority failures are propagated to the caller."
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (let ((result
         (mevedel-session-publication-publish
          session
          (list
           (mevedel-session-persistence--sidecar-publication-artifact
            session root-buffer)))))
    (when (eq result 'queued)
      (user-error "Session state publication was queued before its commit"))
    result))

(defun mevedel-session-persistence-publish-transcript-state
    (session root-buffer transcript-path content &optional coding)
  "Publish portable SESSION transcript CONTENT and its sidecar atomically.

ROOT-BUFFER must be SESSION's live root data buffer.  TRANSCRIPT-PATH must be
a logical artifact below SESSION's save path.  The current authoritative
sidecar is rebuilt from SESSION and ROOT-BUFFER and commits the two-artifact
batch."
  (unless (stringp content)
    (error "Transcript publication requires string content"))
  (let* ((sidecar-artifact
          (mevedel-session-persistence--sidecar-publication-artifact
           session root-buffer))
         (save-path (file-name-as-directory
                     (mevedel-session-save-path session)))
         (path (expand-file-name transcript-path))
         (logical (file-relative-name path save-path)))
    (unless (and (file-in-directory-p path save-path)
                 (mevedel-session-publication-logical-path-p logical))
      (error "Invalid session transcript path: %s" transcript-path))
    (mevedel-session-publication-publish
     session
     (list
      (list :path path :content content :coding coding)
      sidecar-artifact))))

(defun mevedel-session-persistence-publish-agent-terminal-state (invocation)
  "Publish INVOCATION's portable transcript and final session sidecar together.

The authoritative sidecar must already exist.  Shallow first-turn agent state
continues to wait for the root turn's completed-turn publication boundary."
  (require 'mevedel-agent-persistence)
  (unless (mevedel-agent-invocation-p invocation)
    (error "Invalid agent invocation"))
  (let* ((session (mevedel-agent-invocation-parent-session invocation))
         (parent (mevedel-agent-invocation-parent-data-buffer invocation))
         (buffer (mevedel-agent-invocation-buffer invocation))
         (save-path (and session (mevedel-session-save-path session)))
         (relative
          (mevedel-agent-invocation-transcript-relative-path invocation))
         (target (and session (mevedel-session-execution-target session)))
         (transcript (and save-path relative
                          (expand-file-name relative save-path)))
         (sidecar (and save-path
                       (mevedel-session-persistence--sidecar-path save-path))))
    (unless (and session target save-path relative transcript sidecar
                 (mevedel-session-persistence--portable-authority-p session)
                 (buffer-live-p parent)
                 (buffer-live-p buffer)
                 (mevedel-agent-persistence-transcript-path-p
                  relative save-path)
                 (mevedel-session-persistence-artifact-present-p
                  session "session.meta.el"))
      (error "Portable terminal agent state is not materialized"))
    (with-current-buffer buffer
      (unless (and buffer-file-name
                   (equal (expand-file-name buffer-file-name) transcript))
        (error "Agent transcript path does not match its invocation"))
      (mevedel-session-persistence-assert-mutation-authority session)
      (let ((modified-p (buffer-modified-p)))
        (when modified-p
          (when (bound-and-true-p gptel-mode)
            (gptel--save-state))
          (let ((before-save-hook
                 (remq 'gptel--save-state before-save-hook)))
            (run-hooks 'before-save-hook)))
        (mevedel-session-persistence--update-transcript-entry
         session (mevedel-agent-invocation-agent-id invocation)
         (list :updated-at (format-time-string "%FT%H-%M-%S")))
        (require 'mevedel-session-durability)
        (require 'mevedel-session-publication)
        (mevedel-session-publication-publish
         session
         (list
          (list :path transcript
                :content (buffer-substring-no-properties
                          (point-min) (point-max))
                :coding (or buffer-file-coding-system 'utf-8-unix))
          ;; The sidecar commits transcript metadata and final registry state.
          (list :path sidecar
                :content
                (mevedel-session-persistence--printed-value
                 (mevedel-session-persistence--build-sidecar
                  session parent))
                :commit-marker t)))
        (when modified-p
          (set-visited-file-modtime)
          (set-buffer-modified-p nil)
          (run-hooks 'after-save-hook))
        (setf (mevedel-agent-invocation-sidecar-dirty invocation) nil)
        t))))

(defun mevedel-session-persistence--instruction-artifacts (session buffer)
  "Return SESSION instruction snapshot artifacts derived from BUFFER."
  (require 'mevedel-persistence)
  (let* ((save-path (mevedel-session-save-path session))
         (workspace-root
          (mevedel-workspace-root (mevedel-session-workspace session)))
         (turn (mevedel-session-turn-count session)))
    (with-current-buffer buffer
      (append
       (list
        (list :path
              (mevedel-session-persistence--instructions-current-path
               save-path)
              :content
              (mevedel-session-persistence--printed-value
               (mevedel--serialize-instructions workspace-root t))))
       (when (integerp turn)
         (list
          (list :path
                (mevedel-session-persistence--instructions-turn-path
                 save-path turn)
                :content
                (mevedel-session-persistence--printed-value
                 (mevedel--serialize-instructions workspace-root nil)))))))))

(defun mevedel-session-persistence--sidecar-artifact (session buffer)
  "Return SESSION's sidecar publication artifact derived from BUFFER.

The sidecar is the commit marker, so callers place it last in a batch."
  (list :path
        (mevedel-session-persistence--sidecar-path
         (mevedel-session-save-path session))
        :content
        (mevedel-session-persistence--printed-value
         (mevedel-session-persistence--build-sidecar session buffer))
        :commit-marker t))

(defun mevedel-session-persistence--remote-save
    (session buffer settled &optional force)
  "Publish portable project SESSION's durability-critical state from BUFFER.
When SETTLED is non-nil, update the prompt index and latest fork point.
When FORCE is non-nil, publish even when the state is already committed.
The public save entry point has already completed the mutation gate before
calling this serializer."
  (mevedel-session-persistence-ensure-files session buffer)
  (when settled
    (mevedel-session-persistence--update-prompt-index session buffer)
    (mevedel-session-persistence--ensure-latest-fork-point session buffer))
  (let ((segment-artifact nil)
        (mevedel-session-persistence--critical-artifacts nil)
        (mevedel-session-persistence--collecting-critical-artifacts-p t))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (run-hooks 'before-save-hook)
        (setq segment-artifact
              (list
               :path buffer-file-name
               :content (buffer-substring-no-properties
                         (point-min) (point-max))
               :coding buffer-file-coding-system))))
    (mevedel-session-persistence--update-prompt-index session buffer)
    (when (and (boundp 'mevedel--current-request)
               mevedel--current-request)
      (mevedel-file-history-snapshot-modified
       session
       (or (mevedel-session-turn-count session) 0)
       (mevedel-request-file-snapshots mevedel--current-request)))
    (require 'mevedel-session-durability)
    (require 'mevedel-session-publication)
    ;; `updated-at' is stamped only once this save is known to carry a
    ;; change, because stamping it first would make the sidecar differ from
    ;; the committed one on every call and defeat the comparison below.
    (let* ((leading
            (append
             (and segment-artifact (list segment-artifact))
             (nreverse mevedel-session-persistence--critical-artifacts)
             (mevedel-session-persistence--instruction-artifacts
              session buffer)))
           (artifacts
            (append leading
                    (list (mevedel-session-persistence--sidecar-artifact
                           session buffer))))
           (unchanged
            (and (not force)
                 (mevedel-session-publication-committed-p
                  session artifacts))))
      (if unchanged
          ;; Nothing durable differs from the committed snapshot, so this
          ;; save owes the target no transaction.  A segment artifact that
          ;; compared equal proves the fixed cache already holds these
          ;; bytes, so the buffer is no longer dirty against the target.
          (when segment-artifact
            (with-current-buffer buffer
              (set-visited-file-modtime)
              (set-buffer-modified-p nil)))
        (setf (mevedel-session-updated-at session)
              (format-time-string "%FT%H-%M-%S"))
        (mevedel-session-publication-publish
         session
         (mevedel-session-publication-prune-committed
          session
          (append leading
                  (list (mevedel-session-persistence--sidecar-artifact
                         session buffer)))))
        (when segment-artifact
          (with-current-buffer buffer
            (set-visited-file-modtime)
            (set-buffer-modified-p nil)
            (run-hooks 'after-save-hook)))))
    ;; Input history is diagnostic UI state: warn/retry independently and
    ;; never turn it into critical publication.
    (mevedel-session-persistence--notify-session-event
     session 'save-history)
    (mevedel-session-save-path session)))

(defun mevedel-session-persistence-save
    (session buffer &optional settled force)
  "Save SESSION's on-disk state from BUFFER's contents.

Materializes lazily on first call.  Subsequent calls update the
`updated-at' timestamp, save the data buffer, checkpoint any tool-modified
files from before this turn, and rewrite the sidecar.  When SETTLED is non-nil,
mark the latest assistant response
  as a stable fork point.

A portable session whose durable state is byte for byte the committed one
performs no target transaction.  FORCE publishes anyway, for a caller that
must materialize a snapshot rather than record a change."
  ;; A direct data-buffer caller establishes the session-owned root before
  ;; the projection boundary is consulted.  View callers must pass their
  ;; registered data buffer, never the view projection itself.
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (boundp 'mevedel--session)
                    (eq mevedel--session session)
                    (not (bound-and-true-p mevedel--agent-invocation)))))
    (when (or (null (mevedel-session-root-buffer session))
              (eq buffer (mevedel-session-root-buffer session)))
      (require 'mevedel-session-control-transfer)
      (mevedel-session-control-transfer-register-root-buffer
       session buffer)))
  (when-let ((buffer (mevedel-session-persistence--authoritative-buffer
                      buffer)))
    (when (or (null (mevedel-session-root-buffer session))
              (eq buffer (mevedel-session-root-buffer session)))
      (mevedel-session-control-transfer-register-root-buffer session buffer))
    ;; The mutation gate and the publication entry both consult the target
    ;; recovery marker, and nothing in between can install one.
    (require 'mevedel-session-recovery)
    (let ((mevedel-session-recovery--mutation-cache
           (or (bound-and-true-p mevedel-session-recovery--mutation-cache)
               (list nil)))
          ;; One save is one durable transaction, so its lease operations
          ;; share the target clock reading rather than each paying for one.
          ;; The durability module loads lazily, so the outer values are
          ;; read tolerantly: a save may run before anything durable ever
          ;; loaded it.
          (mevedel-session-durability--transaction-clock
           (or (bound-and-true-p mevedel-session-durability--transaction-clock)
               (list nil)))
          (mevedel-session-durability--asserted-directories
           (or (bound-and-true-p
                mevedel-session-durability--asserted-directories)
               (list nil))))
      ;; The whole save is one transaction on one connection, and every
      ;; segment write, sidecar rewrite, modtime stat, and file-history read
      ;; below is a command on it.  A foreign idle timer that sends its own
      ;; command in between consumes the reply we are waiting for, so the
      ;; transaction holds the connection rather than merely refusing to nest
      ;; inside somebody else's.
      (mevedel-transport-with-exclusive-connection
        ;; A save is a real durable mutation even for file-workspace sessions.
        ;; Probe the target before any branch constructs serialized bytes.
        (mevedel-session-persistence-assert-mutation-authority session buffer)
        (prog1
            (if (mevedel-session-persistence--portable-authority-p session)
                (mevedel-session-persistence--remote-save
                 session buffer settled force)
              (mevedel-session-persistence-ensure-files session buffer)
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (when settled
                (mevedel-session-persistence--update-prompt-index
                 session buffer)
                (mevedel-session-persistence--ensure-latest-fork-point
                 session buffer))
              (with-current-buffer buffer
                (when (buffer-modified-p)
                  (save-buffer)))
              (mevedel-session-persistence--update-prompt-index session buffer)
              ;; Snapshot files modified during the just-completed turn.
              (when (and (boundp 'mevedel--current-request)
                         mevedel--current-request)
                (let ((pre-snapshots
                       (mevedel-request-file-snapshots
                        mevedel--current-request)))
                  (mevedel-file-history-snapshot-modified
                   session
                   (or (mevedel-session-turn-count session) 0)
                   pre-snapshots)))
              (mevedel-session-persistence-write
               (mevedel-session-persistence--sidecar-path
                (mevedel-session-save-path session))
               (mevedel-session-persistence--build-sidecar session buffer))
              (mevedel-session-persistence--save-instructions session buffer)
              (mevedel-session-persistence--notify-session-event
               session 'save-history)
              (mevedel-session-save-path session))
          (mevedel-session-persistence--flush-diagnostic-logs session))))))


;;
;;; File-history store
;;
;; Per-session on-disk backup store at <save-path>/file-history/.
;; Filename scheme: `<sha256(absolute-filepath)[:16]>@v<N>' where <N>
;; is the sequential per-file version.  Mapping from (turn, path) to
;; backup filename lives in `mevedel-session-file-snapshots' (alist
;; keyed by turn number; inner alist keyed by absolute path).

(defun mevedel-file-history--path-hash (path)
  "Return the first 16 hex chars of SHA-256 of PATH (expanded)."
  (substring (secure-hash 'sha256 (expand-file-name path)) 0 16))

(defun mevedel-file-history--backup-name (path version)
  "Return the backup filename for PATH at VERSION."
  (format "%s@v%d" (mevedel-file-history--path-hash path) version))

(defun mevedel-file-history--backup-path (save-path backup-name)
  "Return the absolute path to BACKUP-NAME under SAVE-PATH's file-history/."
  (file-name-concat save-path "file-history" backup-name))

(defun mevedel-file-history--latest-version (session path)
  "Return the highest version recorded for PATH in SESSION, or 0 if none."
  (let ((best 0))
    (dolist (turn-entry (mevedel-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best) (setq best v))))))

(defun mevedel-file-history--read-file-raw (path)
  "Return PATH's contents as a unibyte string (no encoding conversion)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-string)))

(defun mevedel-file-history--write-backup (save-path backup-name content)
  "Atomically write CONTENT under `<SAVE-PATH>/file-history/BACKUP-NAME'."
  (let* ((dir  (file-name-concat save-path "file-history"))
         (dest (file-name-concat dir backup-name)))
    (if mevedel-session-persistence--collecting-critical-artifacts-p
        (push (list :path dest :content content)
              mevedel-session-persistence--critical-artifacts)
      (unless (file-directory-p dir)
        (make-directory dir t))
      ;; Create the temp file in the destination's own directory so
      ;; `rename-file' stays on one filesystem and remains atomic.
      (let ((tmp (make-temp-file (expand-file-name ".mevedel-fh-" dir))))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'no-conversion))
                (write-region content nil tmp nil 'silent))
              (rename-file tmp dest t))
          (when (file-exists-p tmp) (delete-file tmp)))))))

(defun mevedel-file-history--maybe-snapshot (session path pre-content)
  "Return SESSION's pre-turn checkpoint entry for changed PATH.

PATH is an absolute filesystem path that was touched by the just-completed
turn's tools.  PRE-CONTENT is PATH's content at the start of the turn,
or nil if it did not exist.

Returns nil when PATH did not change.  PRE-CONTENT is a string, nil for a
previously absent path, or `(:gap REASON)' when pre-turn capture failed."
  (let* ((current-exists (file-exists-p path))
         (version (1+ (mevedel-file-history--latest-version session path)))
         (base (list :version version
                     :backup-time (format-time-string "%FT%H-%M-%S")))
         (gap
          (lambda (reason)
            (display-warning
             'mevedel (format "Checkpoint gap for %s: %s" path reason)
             :warning)
            (cons path (append base (list :gap reason))))))
    (cond
     ((and (listp pre-content) (plist-get pre-content :gap))
      (funcall gap (plist-get pre-content :gap)))
     ((and (stringp pre-content)
           (> (string-bytes pre-content)
              mevedel-file-history-max-snapshot-bytes))
      (funcall gap
               (format "pre-turn content exceeds %d bytes"
                       mevedel-file-history-max-snapshot-bytes)))
     ((and current-exists (not (file-regular-p path)))
      (funcall gap "post-turn path is not a regular file"))
     (t
      (condition-case err
          (let ((current-content
                 (and current-exists
                      (mevedel-file-history--read-file-raw path))))
            (unless (if current-exists
                        (and (stringp pre-content)
                             (string-equal pre-content current-content))
                      (null pre-content))
              (let* ((backup-name
                      (and current-exists
                           (mevedel-file-history--backup-name path version)))
                     (pre-backup-name
                      (and (stringp pre-content)
                           (concat
                            (mevedel-file-history--backup-name path version)
                            ".pre"))))
                (when backup-name
                  (mevedel-file-history--write-backup
                   (mevedel-session-save-path session)
                   backup-name current-content))
                (when pre-backup-name
                  (mevedel-file-history--write-backup
                   (mevedel-session-save-path session)
                   pre-backup-name pre-content))
                (cons path
                      (append base
                              (list :backup-name backup-name
                                    :pre-backup-name pre-backup-name))))))
        (error (funcall gap (error-message-string err))))))))

(defun mevedel-file-history-snapshot-modified (session turn-n pre-snapshots)
  "Snapshot files modified during TURN-N for SESSION.

PRE-SNAPSHOTS is a hash-table mapping absolute path to the file's content
at turn start (nil for paths that did not yet exist).  Typically the
`file-snapshots' slot of the just-completed request.

For each changed path, persist its pre- and post-turn content or absence
under TURN-N.  Known capture failures remain explicit gap entries.  TURN-N
is recorded even when no tracked file changed.  Returns the list of backup
names written."
  (when (and (mevedel-session-save-path session)
             (hash-table-p pre-snapshots))
    (let (entries written)
      (maphash
       (lambda (path pre-content)
         (when-let ((entry (mevedel-file-history--maybe-snapshot
                            session path pre-content)))
           (push entry entries)
           (when-let ((name (plist-get (cdr entry) :backup-name)))
             (push name written))
           (when-let ((name (plist-get (cdr entry) :pre-backup-name)))
             (push name written))))
       pre-snapshots)
      ;; Sort entries by path so two saves with identical state
      ;; produce byte-identical sidecars (hash-table iteration is
      ;; otherwise non-deterministic).
      (setq entries (sort entries
                          (lambda (a b) (string< (car a) (car b)))))
      (let ((cell (assoc turn-n (mevedel-session-file-snapshots session))))
        (if cell
            (setcdr cell entries)
          (setf (mevedel-session-file-snapshots session)
                (cons (cons turn-n entries)
                      (mevedel-session-file-snapshots session)))))
      (sort written #'string<))))

;;
;;; Segment rotation (split-on-compact)

(defvar gptel--markdown-block-map)
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(declare-function org-cycle "ext:org" (&optional arg))

(defun mevedel-session-persistence--file-text (file)
  "Return FILE contents as a string using normal text decoding."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mevedel-session-persistence--refresh-visited-file-modtime-or-error
    (&optional expected-texts)
  "Refresh stale visited-file metadata when disk text is expected.

EXPECTED-TEXTS is a string or list of strings that may also match the
visited file.  This covers automatic edits that first remove transient
unsaved text from the live buffer.  If the visited file changed externally
to different text or was deleted, signal a controlled error instead of
letting `save-buffer' ask an interactive supersession question during
automatic segment rotation."
  (when buffer-file-name
    (cond
     ((not (file-exists-p buffer-file-name))
      (when buffer-file-number
        (error "Session segment changed on disk: %s" buffer-file-name)))
     ((not (verify-visited-file-modtime (current-buffer)))
      (let ((file-text (mevedel-session-persistence--file-text buffer-file-name))
            (accepted (cons (buffer-substring-no-properties (point-min) (point-max))
                            (if (listp expected-texts)
                                expected-texts
                              (list expected-texts)))))
        (if (member file-text accepted)
            (set-visited-file-modtime)
          (error "Session segment changed on disk: %s" buffer-file-name)))))))

(defun mevedel-session-persistence--disown-save-machinery ()
  "Keep Emacs backups and file locks off the current buffer's segment.

Both are answers to problems this session already answers better.  A backup is
the immutable publication and the file-history checkpoint; a lock is the
durable lease, which a second client observes before its buffer becomes
writable, where Emacs's lock is only advisory between Emacsen.

On a target they are not free.  The backup is one whole-segment copy over the
connection, and the lock is a symlink created and removed around every
modify-and-save cycle.  They also leave `segment-NNNN.chat.org~' and
`.#segment-NNNN.chat.org' inside a portable session directory that another
client resumes from, where neither means anything."
  (setq-local make-backup-files nil)
  (setq-local create-lockfiles nil))

(defun mevedel-session-persistence--set-visited-segment-file (file)
  "Make the current buffer visit segment FILE without changing its name."
  (let ((name (buffer-name)))
    (set-visited-file-name file t)
    (rename-buffer name t))
  (setq buffer-file-truename (file-truename file))
  (mevedel-session-persistence--disown-save-machinery)
  (set-visited-file-modtime)
  (set-buffer-modified-p nil))

(defun mevedel-session-persistence--publish-segment-text (file text)
  "Atomically write TEXT to FILE and make the current buffer visit it."
  (let ((coding-system buffer-file-coding-system))
    (with-temp-buffer
      (setq buffer-file-coding-system coding-system)
      (insert text)
      (mevedel-session-persistence--write-current-buffer-atomically file)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert text))
  (mevedel-session-persistence--set-visited-segment-file file))

(defun mevedel-session-persistence--insert-segment-header (session)
  "Insert per-segment org properties at point in current buffer.

Sets `MEVEDEL_SESSION_ID', `MEVEDEL_SEGMENT_NUMBER',
`MEVEDEL_SEGMENT_CREATED_AT', and `MEVEDEL_VERSION'.  Caller is
responsible for ensuring the buffer is in `org-mode' (mevedel data
buffers are locked to `org-mode' by `mevedel--chat-buffer-setup')."
  (when (derived-mode-p 'org-mode)
    (require 'org)
    (org-entry-put (point-min) "MEVEDEL_VERSION" (mevedel-version))
    (org-entry-put (point-min) "MEVEDEL_SESSION_ID"
                   (or (mevedel-session-session-id session) ""))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_NUMBER"
                   (number-to-string
                    (or (mevedel-session-current-segment session) 1)))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_CREATED_AT"
                   (format-time-string "%FT%H-%M-%S"))))

(defun mevedel-session-persistence--segment-summary-bounds ()
  "Return bounds for the leading segment compaction summary, or nil.

The returned plist contains `:begin', `:body-begin', `:body-end',
and `:end'.  A summary is accepted only when it is the first top-level
content after the optional org property drawer and whitespace."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+begin_summary\\b.*$" nil t)
      (let ((begin (match-beginning 0))
            (body-begin (match-end 0)))
        (when (and (save-excursion
                     (goto-char begin)
                     (let ((prefix (buffer-substring-no-properties
                                    (point-min) begin)))
                       (string-match-p
                        "\\`[[:space:]\n]*\\(:PROPERTIES:\n\\(.\\|\n\\)*?:END:\n\\)?[[:space:]\n]*\\'"
                        prefix)))
                   (re-search-forward "^#\\+end_summary\\b.*$" nil t))
          (list :begin begin
                :body-begin (1+ body-begin)
                :body-end (match-beginning 0)
                :end (match-end 0)))))))

(defconst mevedel-session-persistence--summary-handoff-prefix
  "Another language model started to solve this problem and produced a summary of its work. Use this to build on the work that has already been done and avoid duplicating work. Here is the summary:\n\n"
  "Model-facing preface inserted before a compacted segment summary.")

(defun mevedel-session-persistence--strip-summary-handoff-prefix (summary)
  "Return SUMMARY without the model-facing handoff prefix."
  (if (and (stringp summary)
           (string-prefix-p
            mevedel-session-persistence--summary-handoff-prefix summary))
      (substring summary
                 (length mevedel-session-persistence--summary-handoff-prefix))
    summary))

(defun mevedel-session-persistence--summary-block (summary)
  "Return SUMMARY wrapped in an org `#+begin_summary' block.

The block markers are propertized with `gptel \\='ignore' so the LLM sees
only the handoff preface plus SUMMARY -- not the wrapper lines.  The
user\\='s view, by contrast, sees a foldable block."
  (concat (propertize "#+begin_summary mevedel-role=compaction-summary\n"
                      'gptel 'ignore)
          mevedel-session-persistence--summary-handoff-prefix
          summary
          (propertize "\n#+end_summary\n" 'gptel 'ignore)))

(defun mevedel-session-persistence--finalize-segment-file (file)
  "Mark segment FILE finalized on disk."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (org-entry-put (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"
                     (format-time-string "%FT%H-%M-%S"))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun mevedel-session-persistence--finalized-segment-text (text coding)
  "Return segment TEXT with finalized metadata, encoded using CODING."
  (with-temp-buffer
    (setq buffer-file-coding-system coding)
    (insert text)
    (let ((org-agenda-file-menu-enabled nil))
      (org-mode))
    (org-entry-put (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"
                   (format-time-string "%FT%H-%M-%S"))
    (buffer-string)))

(defun mevedel-session-persistence--publish-remote-segment-transition
    (session buffer old-segment old-text new-segment new-text)
  "Publish one portable project SESSION segment transition derived from BUFFER.

OLD-SEGMENT receives finalized OLD-TEXT.  NEW-SEGMENT receives NEW-TEXT.
Instruction snapshots and the sidecar join the same batch, with the sidecar
last as its commit marker."
  (with-current-buffer buffer
    (let ((coding buffer-file-coding-system))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert new-text))
      ;; Keep the live transition installed if publication becomes pending.
      ;; Retry can then publish the exact state represented in memory.
      (setq buffer-file-name new-segment
            buffer-file-truename nil)
      (require 'mevedel-session-durability)
      (require 'mevedel-session-publication)
      (mevedel-session-publication-publish
       session
       (append
        (list
         (list :path old-segment
               :content
               (mevedel-session-persistence--finalized-segment-text
                old-text coding)
               :coding coding)
         (list :path new-segment :content new-text :coding coding))
        (mevedel-session-persistence--instruction-artifacts session buffer)
        (list
         (list
          :path
          (mevedel-session-persistence--sidecar-path
           (mevedel-session-save-path session))
          :content
          (mevedel-session-persistence--printed-value
           (mevedel-session-persistence--build-sidecar session buffer))
          :commit-marker t))))
      (mevedel-session-persistence--set-visited-segment-file new-segment))))

(defun mevedel-session-persistence--delete-trailing-text (text)
  "Delete trailing TEXT from the current buffer when it is an exact suffix."
  (when (and text
             (not (string-empty-p text))
             (string-suffix-p
              (substring-no-properties text)
              (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (- (point-max) (length text)) (point-max))
    t))

(cl-defun mevedel-session-persistence-rotate-segment
    (session buffer summary &key tail-text pending-text archive-text
             truncated-tail-p)
  "Finalize SESSION's current segment and start a new one with SUMMARY.

Performs the split-on-compact rotation:
  1. Saves the current segment file before replacing the live buffer.
  2. Advances `mevedel-session-current-segment' on SESSION.
  3. Builds the new segment in a temporary buffer.
  4. Atomically publishes it and repoints BUFFER's visited-file state.
  5. Restores PENDING-TEXT in the live buffer without marking it saved.
  6. Rewrites the sidecar.
  7. Sets `MEVEDEL_SEGMENT_FINALIZED_AT' on the predecessor segment.

TAIL-TEXT is preserved recent transcript text, including text
properties.  ARCHIVE-TEXT contains durable hidden records replacing
execution rows removed by compaction.  PENDING-TEXT is an
inserted-but-unsent prompt region.
TRUNCATED-TAIL-P is recorded as segment metadata when non-nil.

Requires SESSION to have a `save-path' (i.e., to have been lazily
materialized).  Returns the new segment's absolute path on success,
nil if SESSION is not yet materialized."
  (when (mevedel-session-save-path session)
    (mevedel-session-persistence-assert-mutation-authority session buffer)
    (with-current-buffer buffer
      (require 'org)
      ;; 1. Save the current segment before replacing the buffer body.
      (let ((portable-p
             (mevedel-session-persistence--portable-authority-p session))
            (old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            (tail-prompt-count
             (mevedel-session-persistence--prompt-count-in-text tail-text))
            old-publish-text
            new-segment
            new-text
            pending-position)
        (let ((telemetry-span
               (and (fboundp 'mevedel-telemetry-start)
                    (mevedel-telemetry-start
                     session 'segment-rotation
                     :old-segment old-current-segment
                     :new-segment (1+ old-current-segment)))))
        (condition-case err
            (progn
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'old-save-start
                 :old-segment old-current-segment))
              (mevedel-session-persistence--refresh-visited-file-modtime-or-error
               (when (and pending-text
                          (string-suffix-p
                           (substring-no-properties pending-text)
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                 (buffer-substring-no-properties
                  (point-min) (- (point-max) (length pending-text)))))
              (when pending-text
                (let ((inhibit-read-only t))
                  (mevedel-session-persistence--delete-trailing-text
                   pending-text)))
              (if portable-p
                  (setq old-publish-text
                        (buffer-substring (point-min) (point-max)))
                (when (buffer-modified-p) (save-buffer)))
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'old-saved
                 :old-segment old-current-segment))
              ;; 2. Advance segment counter.
              (cl-incf (mevedel-session-current-segment session))
              ;; 3. Build the new segment without changing visited-file state.
              (setq new-segment
                    (mevedel-session-persistence--segment-path
                     (mevedel-session-save-path session)
                     (mevedel-session-current-segment session)))
              (let ((coding-system buffer-file-coding-system))
                (setq new-text
                      (with-temp-buffer
                        (setq buffer-file-coding-system coding-system)
                        (org-mode)
                        ;; 4. Build the persisted body in isolation.
                        (mevedel-session-persistence--insert-segment-header
                         session)
                        (when truncated-tail-p
                          (org-entry-put
                           (point-min) "MEVEDEL_SEGMENT_TRUNCATED_TAIL" "t"))
                        (when (> tail-prompt-count 0)
                          (org-entry-put
                           (point-min) "MEVEDEL_SEGMENT_TAIL_PROMPTS"
                           (number-to-string tail-prompt-count)))
                        (goto-char (point-max))
                        (unless (bolp) (insert "\n"))
                        (insert "\n")
                        (insert
                         (mevedel-session-persistence--summary-block summary))
                        (when tail-text
                          (unless (bolp) (insert "\n"))
                          (insert tail-text))
                        (when archive-text
                          (insert archive-text))
                        (when pending-text
                          (unless (bolp) (insert "\n"))
                          (setq pending-position (point)))
                        (insert "\n")
                        (buffer-string))))
              ;; 5. Publish, then repoint the canonical live buffer.
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'new-publish-start
                 :new-segment (mevedel-session-current-segment session)))
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (if portable-p
                  (mevedel-session-persistence--publish-remote-segment-transition
                   session buffer old-segment old-publish-text
                   new-segment new-text)
                (mevedel-session-persistence--publish-segment-text
                 new-segment new-text)
                ;; 6. Rewrite the sidecar with the bumped current-segment.
                (mevedel-session-persistence-write
                 (mevedel-session-persistence--sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-persistence--build-sidecar session buffer))
                (mevedel-session-persistence--save-instructions session buffer)
                (mevedel-session-persistence--finalize-segment-file
                 old-segment))
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'new-published
                 :new-segment (mevedel-session-current-segment session)))
              (when telemetry-span
                (mevedel-telemetry-finish
                 telemetry-span :outcome 'success))
              (when pending-position
                (goto-char pending-position)
                (insert pending-text)
                ;; The pending prompt belongs to the in-flight request.
                ;; The DONE autosave will commit it together with the
                ;; assistant response; failure/abort paths must not.
                (set-buffer-modified-p nil))
              new-segment)
          (error
           (when telemetry-span
             (mevedel-telemetry-finish
              telemetry-span :outcome 'error :error-class (car-safe err)))
           (if (and portable-p
                    (mevedel-session-pending-publication session))
               (progn
                 (when pending-position
                   (goto-char pending-position)
                   (insert pending-text))
                 (set-buffer-modified-p nil))
             (setf (mevedel-session-current-segment session)
                   old-current-segment)
             (setf (mevedel-session-updated-at session) old-updated-at)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert old-text))
             (mevedel-session-persistence--set-visited-segment-file
              old-segment)
             (goto-char (min old-point (point-max)))
             (set-buffer-modified-p old-modified-p)
             (ignore-errors
               (mevedel-session-persistence-write
                (mevedel-session-persistence--sidecar-path
                 (mevedel-session-save-path session))
                (mevedel-session-persistence--build-sidecar session buffer)))
             (when (and new-segment (file-exists-p new-segment))
               (delete-file new-segment)))
           (signal (car err) (cdr err)))))))))

(cl-defun mevedel-session-persistence-start-fresh-segment
    (session buffer &key initial-text)
  "Finalize SESSION's current segment and start a blank live segment in BUFFER.

INITIAL-TEXT, when non-nil, is inserted after the new segment's org
metadata.  This is used by `/clear' to leave a fresh prompt prefix in
the data buffer without carrying over any conversation summary.

Requires SESSION to have a `save-path'.  Returns the new segment's
absolute path on success, nil if SESSION is not yet materialized."
  (when (mevedel-session-save-path session)
    (mevedel-session-persistence-assert-mutation-authority session buffer)
    (with-current-buffer buffer
      (require 'org)
      (let ((portable-p
             (mevedel-session-persistence--portable-authority-p session))
            (old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            old-publish-text
            new-segment
            new-text
            initial-position)
        (unless old-segment
          (error "No current segment file"))
        (condition-case err
            (progn
              (mevedel-session-persistence--refresh-visited-file-modtime-or-error)
              (if portable-p
                  (setq old-publish-text
                        (buffer-substring (point-min) (point-max)))
                (when (buffer-modified-p) (save-buffer)))
              (mevedel-session-persistence--update-prompt-index
               session buffer)
              (cl-incf (mevedel-session-current-segment session))
              (setq new-segment
                    (mevedel-session-persistence--segment-path
                     (mevedel-session-save-path session)
                     (mevedel-session-current-segment session)))
              (let ((coding-system buffer-file-coding-system))
                (setq new-text
                      (with-temp-buffer
                        (setq buffer-file-coding-system coding-system)
                        (org-mode)
                        (mevedel-session-persistence--insert-segment-header
                         session)
                        (goto-char (point-max))
                        (when (and initial-text
                                   (not (string-empty-p initial-text)))
                          (unless (bolp) (insert "\n"))
                          (setq initial-position (point)))
                        (buffer-string))))
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (if portable-p
                  (mevedel-session-persistence--publish-remote-segment-transition
                   session buffer old-segment old-publish-text
                   new-segment new-text)
                (mevedel-session-persistence--publish-segment-text
                 new-segment new-text)
                (mevedel-session-persistence-write
                 (mevedel-session-persistence--sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-persistence--build-sidecar session buffer))
                (mevedel-session-persistence--save-instructions session buffer)
                (mevedel-session-persistence--finalize-segment-file
                 old-segment))
              (when initial-position
                (goto-char initial-position)
                (insert initial-text)
                (set-buffer-modified-p nil))
              (goto-char (point-max))
              (mevedel-session-persistence--notify-session-event
               session 'rerender)
              new-segment)
          (error
           (if (and portable-p
                    (mevedel-session-pending-publication session))
               (progn
                 (when initial-position
                   (goto-char initial-position)
                   (insert initial-text))
                 (set-buffer-modified-p nil))
             (setf (mevedel-session-current-segment session)
                   old-current-segment)
             (setf (mevedel-session-updated-at session) old-updated-at)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert old-text))
             (mevedel-session-persistence--set-visited-segment-file
              old-segment)
             (goto-char (min old-point (point-max)))
             (set-buffer-modified-p old-modified-p)
             (ignore-errors
               (mevedel-session-persistence-write
                (mevedel-session-persistence--sidecar-path
                 (mevedel-session-save-path session))
                (mevedel-session-persistence--build-sidecar session buffer)))
             (when (and new-segment (file-exists-p new-segment))
               (delete-file new-segment)))
           (signal (car err) (cdr err))))))))


;;
;;; Locking

(defvar mevedel-session-persistence--emacs-invocation-time
  (format-time-string "%FT%H-%M-%S")
  "Wall-clock time when this Emacs process started.
Stamped into `.lock' files as a forensic / tiebreaker field.")

(defun mevedel-session-persistence--lock-path (session-dir)
  "Return the absolute path to SESSION-DIR's `.lock' file."
  (file-name-concat session-dir ".lock"))

(defun mevedel-session-persistence--write-lock (lock-path buffer-name)
  "Write or overwrite a lock file at LOCK-PATH naming BUFFER-NAME as holder.
Prefer `mevedel-session-persistence--write-lock-atomic' when
acquiring a fresh lock; this function is safe only when the caller
already owns (or is replacing) an existing lock."
  (let ((plist (list :pid (emacs-pid)
                     :hostname (system-name)
                     :emacs-invocation-time
                     mevedel-session-persistence--emacs-invocation-time
                     :buffer buffer-name)))
    (with-temp-file lock-path
      (let ((print-length nil) (print-level nil))
        (prin1 plist (current-buffer))))))

(defun mevedel-session-persistence--write-lock-atomic (lock-path buffer-name)
  "Atomically create LOCK-PATH for BUFFER-NAME with this Emacs as holder.
Returns t on success, nil when LOCK-PATH already exists (race lost).

Uses `add-name-to-file' which is POSIX link(2) and thus atomic: if
the target already exists it signals `file-already-exists'.  Writes
the payload into a unique temp file in the same directory first so
partial payloads can never appear at LOCK-PATH."
  (let* ((dir (file-name-directory (expand-file-name lock-path)))
         (tmp (make-temp-file (expand-file-name ".mevedel-lock-" dir)))
         (plist (list :pid (emacs-pid)
                      :hostname (system-name)
                      :emacs-invocation-time
                      mevedel-session-persistence--emacs-invocation-time
                      :buffer buffer-name)))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((print-length nil) (print-level nil))
              (prin1 plist (current-buffer))))
          (condition-case _
              (progn
                (add-name-to-file tmp lock-path nil)
                t)
            (file-already-exists nil)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun mevedel-session-persistence--read-lock (lock-path)
  "Return the plist read from LOCK-PATH, or nil if absent or unparseable."
  (when (file-exists-p lock-path)
    (condition-case _
        (with-temp-buffer
          (insert-file-contents lock-path)
          (goto-char (point-min))
          (read (current-buffer)))
      (error nil))))

(defun mevedel-session-persistence--pid-alive-p (pid)
  "Return non-nil if PID is a live process on the current host."
  (and pid (numberp pid)
       (condition-case _
           (signal-process pid 0)
         (error nil))))

(defconst mevedel-session-persistence--lock-start-time-tolerance 2
  "Seconds of tolerance when comparing a lock timestamp to process start.

The lock timestamp is formatted to whole seconds, while process start
attributes can carry sub-second precision and can be rounded differently
by the host OS.")

(defun mevedel-session-persistence--pid-start-time (pid)
  "Return PID's process start time, or nil if unavailable."
  (when (and pid (numberp pid))
    (condition-case _
        (cdr (assq 'start (process-attributes pid)))
      (error nil))))

(defun mevedel-session-persistence--same-host-lock-active-p (lock-info)
  "Return non-nil if same-host LOCK-INFO describes an active holder.

Dead PIDs are stale.  Live PIDs are considered active unless the live
process start time proves PID reuse: a process that started clearly
after the lock holder's recorded Emacs invocation cannot be the process
that wrote the lock.  When the recorded invocation time or live process
start time is unavailable, keep the lock active rather than risking data
loss."
  (when (mevedel-session-persistence--pid-alive-p
         (plist-get lock-info :pid))
    (let ((holder-start (mevedel-session-persistence--parse-iso-time
                         (plist-get lock-info :emacs-invocation-time)))
          (pid-start    (mevedel-session-persistence--pid-start-time
                         (plist-get lock-info :pid))))
      (or (not holder-start)
          (not pid-start)
          (<= (- (float-time pid-start) (float-time holder-start))
              mevedel-session-persistence--lock-start-time-tolerance)))))

(defun mevedel-session-persistence-lock-acquire
    (session-dir buffer-name &optional session)
  "Acquire SESSION-DIR's mutation authority for BUFFER-NAME.

Portable sessions use a renewable lease.  File-workspace sessions use `.lock'.
SESSION supplies the live lease owner when SESSION-DIR is portable.

Returns:
  t   - lock acquired (or broken from a previous holder).
  nil - user chose read-only access; caller should set variable
        `buffer-read-only'.

Signals `user-error' when the user declines to break a stale lock or
aborts any of the 3-way conflict prompts.

Portable project sessions acquire their renewable lease and never inspect the
PID-lock table.  File-workspace sessions use the following lock table:
- No existing lock: write a new lock, return t.
- Lock from same host, dead PID: prompt to break (`y-or-n-p').
- Lock from same host, live PID: 3-way prompt -- break / read-only / abort.
- Lock from different host: 3-way prompt -- break / read-only / abort."
  (if (eq (mevedel-session-persistence--authority-mode-for-path
           session-dir session)
          'portable)
      (progn
        (require 'mevedel-session-durability)
        (when session
          (mevedel-session-durability-disclose session))
        (mevedel-session-durability-lease-acquire
         session-dir buffer-name session))
    (let* ((lock-path (mevedel-session-persistence--lock-path session-dir))
         (existing  (mevedel-session-persistence--read-lock lock-path)))
      (cond
     ((null existing)
      ;; Race-free create via `add-name-to-file'.  If another process
      ;; beat us to it between the read above and this write, fall
      ;; through to the existing-lock branches.
      (cond
       ((mevedel-session-persistence--write-lock-atomic lock-path buffer-name)
        t)
       ((mevedel-session-persistence--read-lock lock-path)
        (mevedel-session-persistence-lock-acquire
         session-dir buffer-name session))
       (t
        (user-error "Session lock exists but could not be read: %s"
                    lock-path))))
     ((equal (plist-get existing :hostname) (system-name))
      (cond
       ((mevedel-session-persistence--same-host-lock-active-p existing)
        (let ((response
               (read-char-choice
                (format
                 (concat "Mevedel session locked by a live process on this host:\n"
                         "  PID:    %s\n"
                         "  Since:  %s\n"
                         "  Buffer: %s\n"
                         "[b]reak, [r]ead-only, [a]bort? ")
                 (plist-get existing :pid)
                 (plist-get existing :emacs-invocation-time)
                 (plist-get existing :buffer))
                '(?b ?r ?a))))
          (pcase response
            (?b (mevedel-session-persistence--write-lock lock-path buffer-name)
                t)
            (?r nil)
            (?a (user-error "Session resume aborted")))))
       (t
        (if (y-or-n-p
             (format "Stale mevedel lock (PID %d, buffer %s).  Break and proceed? "
                     (plist-get existing :pid)
                     (plist-get existing :buffer)))
            (progn
              (mevedel-session-persistence--write-lock lock-path buffer-name)
              t)
          (user-error "Lock not broken")))))
     (t
      (let ((response
             (read-char-choice
              (format
               (concat "Mevedel session locked by:\n"
                       "  PID:    %s\n"
                       "  Host:   %s\n"
                       "  Since:  %s\n"
                       "  Buffer: %s\n"
                       "[b]reak, [r]ead-only, [a]bort? ")
               (plist-get existing :pid)
               (plist-get existing :hostname)
               (plist-get existing :emacs-invocation-time)
               (plist-get existing :buffer))
              '(?b ?r ?a))))
        (pcase response
          (?b (mevedel-session-persistence--write-lock lock-path buffer-name)
              t)
          (?r nil)
          (?a (user-error "Session resume aborted")))))))))

(defun mevedel-session-persistence-lock-release (session-dir &optional session)
  "Release this client's mutation authority for SESSION-DIR.
SESSION supplies the live lease owner when SESSION-DIR is portable."
  (if (eq (mevedel-session-persistence--authority-mode-for-path
           session-dir session)
          'portable)
      (progn
        (require 'mevedel-session-durability)
        (mevedel-session-durability-lease-release session-dir session))
    (let* ((lock-path (mevedel-session-persistence--lock-path session-dir))
           (existing  (mevedel-session-persistence--read-lock lock-path)))
      (when (and existing
                 (eq (plist-get existing :pid) (emacs-pid))
                 (equal (plist-get existing :hostname) (system-name)))
        (delete-file lock-path)))))

(defun mevedel-session-persistence--sweep-stale-locks (workspace)
  "Silently remove stale `.lock' files in WORKSPACE.

A lock is stale only when its hostname matches this host and the holder
is not active: either its PID is dead, or the PID is live but the live
process start time proves PID reuse.  Cross-host locks are left alone.
Best-effort; any I/O failure is swallowed.

Called opportunistically from the `mevedel' session chooser."
  (let ((sessions-dir (mevedel-session-persistence--sessions-dir workspace)))
    ;; Portable leases expire and are taken over explicitly.  A project
    ;; sweep has no authority over them.
    (when (and (eq (mevedel-workspace-type workspace) 'file)
               (file-directory-p sessions-dir))
      (dolist (entry (directory-files sessions-dir t "\\`[^.]"))
        (when (file-directory-p entry)
          (let* ((lock-path (mevedel-session-persistence--lock-path entry))
                 (info      (mevedel-session-persistence--read-lock
                             lock-path)))
            (when (and info
                       (equal (plist-get info :hostname) (system-name))
                       (not (mevedel-session-persistence--same-host-lock-active-p
                             info)))
              (condition-case _
                  (delete-file lock-path)
                (error nil)))))))))

(defun mevedel-session-persistence--release-on-kill ()
  "Buffer-local `kill-buffer-hook' that releases session mutation authority."
  (when (and (boundp 'mevedel--session)
             mevedel--session)
    (when-let ((dir (mevedel-session-save-path mevedel--session)))
      (condition-case _
          (mevedel-session-persistence-lock-release dir mevedel--session)
        (error nil)))))


;;
;;; Workspace relocation reconciliation

(defun mevedel-session-persistence--reconcile-relocation
    (session saved-workspace-plist)
  "Reconcile SESSION's path-bearing fields against workspace relocation.

If SAVED-WORKSPACE-PLIST's `:target-native-root' differs from SESSION's current
workspace root, rewrite permission rules whose `:path' starts with
the saved root and is not already under the current root, and prune
touched-files entries pointing at vanished paths.  Logs the rewrite
count to `*Messages*'.

A no-op when the saved root is missing or matches current."
  (let* ((saved-root
          (plist-get saved-workspace-plist :target-native-root))
         (current-root
          (mevedel-execution-target-native-root
           (mevedel-session-execution-target session)))
         (saved-root-expanded (and saved-root (expand-file-name saved-root)))
         (current-root-expanded (and current-root
                                     (expand-file-name current-root))))
    (when (and saved-root current-root
               (not (equal saved-root-expanded current-root-expanded)))
      ;; Rewrite path-bearing permission rules.
      (let ((rewrites 0)
            (saved-prefix (file-name-as-directory saved-root-expanded))
            (current-prefix (file-name-as-directory current-root-expanded)))
        (setf (mevedel-session-permission-rules session)
              (mapcar
               (lambda (rule)
                 (let* ((path (plist-get (cdr rule) :path))
                        (expanded-path (and path (expand-file-name path))))
                   (cond
                    ((and expanded-path
                          (string-prefix-p saved-prefix expanded-path)
                          (not (string-prefix-p current-prefix expanded-path)))
                     (cl-incf rewrites)
                     (let ((new-rule (copy-tree rule)))
                       (plist-put
                        (cdr new-rule) :path
                        (concat current-prefix
                                (substring expanded-path
                                           (length saved-prefix))))
                       new-rule))
                    (t rule))))
               (mevedel-session-permission-rules session)))
        ;; Touched-files resets to an empty hash on load, so there is
        ;; nothing to prune here today; kept as the conceptual place
        ;; if we ever start carrying touched-files across resume.
        (when (> rewrites 0)
          (message "Reconciled %d permission paths from %s to %s"
                   rewrites saved-root current-root))))))


;;
;;; Segment-counter self-heal

(defun mevedel-session-persistence--detect-highest-segment (save-path)
  "Return the highest segment number found on disk under SAVE-PATH, or 0."
  (let ((max-n 0))
    (when (file-directory-p save-path)
      (dolist (f (directory-files save-path nil
                                  "\\`segment-[0-9]+\\.chat\\.org\\'"))
        (when (string-match "segment-\\([0-9]+\\)\\.chat\\.org" f)
          (let ((n (string-to-number (match-string 1 f))))
            (when (> n max-n) (setq max-n n))))))
    max-n))

(defun mevedel-session-persistence--self-heal-segment-counter
    (session save-path &optional defer-finalization-p)
  "Reconcile SESSION's `:current-segment' with the filesystem under SAVE-PATH.

If the highest-numbered segment file on disk differs from the sidecar's
recorded `:current-segment', trust the filesystem (the sidecar may be
stale from a crash mid-rotation).  Logs a warning.  When healing upward
after a crash that published a new segment before finalizing its
predecessor, mark the predecessor finalized now.  When
DEFER-FINALIZATION-P is non-nil, return that predecessor without writing it
so the caller can include finalization in a larger publication batch."
  (let* ((sidecar-n    (or (mevedel-session-current-segment session) 1))
         (filesystem-n (mevedel-session-persistence--detect-highest-segment
                        save-path))
         predecessor)
    (when (and (> filesystem-n 0)
               (not (= sidecar-n filesystem-n)))
      (display-warning
       'mevedel
       (format "Sidecar :current-segment %d differs from filesystem (%d); using %d"
               sidecar-n filesystem-n filesystem-n)
       :warning)
      (when (> filesystem-n sidecar-n)
        (setq predecessor
              (mevedel-session-persistence--segment-path
               save-path (1- filesystem-n)))
        (unless defer-finalization-p
          (mevedel-session-persistence--finalize-segment-file predecessor)))
      (setf (mevedel-session-current-segment session) filesystem-n))
    predecessor))


;;
;;; Save-failure indicator

(defvar-local mevedel-session--save-failed nil
  "Non-nil when the most recent auto-save failed in this buffer.
Set by the DONE-terminal autosave handler on any save error and
cleared on the next successful save.  Surfaced by
`mevedel-session-persistence-header-segment' so the user has a
visible cue in addition to the `display-warning'.")

(defun mevedel-session-persistence-header-segment ()
  "Return a header-line fragment summarising persistence state.
Empty when the session has no unusual state; highlights save
failures and read-only mode when active."
  (let ((parts nil))
    (when (and (bound-and-true-p mevedel--session)
               (mevedel-session-pending-publication mevedel--session))
      (push (propertize " [Publication pending] " 'face 'error) parts))
    (when (bound-and-true-p mevedel--session)
      (when-let* ((lease-state
                   (plist-get (mevedel-session-lease mevedel--session)
                              :state))
                  ((memq lease-state '(foreign expired lost))))
        (push (propertize (format " [Lease: %s] " lease-state)
                          'face 'warning)
              parts)))
    (when (bound-and-true-p mevedel-session--save-failed)
      (push (propertize " [Save failed] " 'face 'error) parts))
    (when (bound-and-true-p mevedel-session--read-only-mode)
      (push (propertize " [Read-only] " 'face 'warning) parts))
    (if parts (apply #'concat parts) "")))


;;
;;; Read path (resume)

(defun mevedel-session-persistence--find-file-noselect (file)
  "Return a buffer visiting persisted mevedel FILE without `so-long'.

Persisted chat and agent transcript files may contain very long org
property lines, especially GPTEL_SYSTEM.  Those lines are expected
data, and letting `so-long' replace `org-mode' breaks gptel/org state
restoration and reveal timers."
  (let* ((so-long-predicate (lambda () nil))
         ;; Bound around the visit so a stale entry cannot move point either.
         (save-place-mode nil)
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (require 'mevedel-utilities)
      (mevedel--forget-place))
    buffer))

(defun mevedel-session-persistence-load-sidecar (path)
  "Read a current-version sidecar plist from PATH.
Return nil when the sidecar is missing or unreadable.  Signal when a
readable sidecar has an unsupported version or obsolete shape."
  (cond
   ((not (file-exists-p path))
    (display-warning 'mevedel
                     (format "Sidecar missing at %s; treating as fresh session"
                             path)
                     :warning)
    nil)
   (t
    (let ((plist
           (condition-case err
               (mevedel-session-persistence-read path)
             (error
              (display-warning
               'mevedel
               (format "Sidecar unreadable at %s: %s; treating as fresh session"
                       path (error-message-string err))
               :warning)
              nil))))
      (cond
       ((null plist) nil)
       ((not (proper-list-p plist))
        (display-warning
         'mevedel
         (format "Sidecar unreadable at %s; treating as fresh session" path)
         :warning)
        nil)
       (t
        (unless (equal (plist-get plist :version)
                       mevedel-session-persistence-format-version)
          (error "Unsupported session version: %s"
                 (or (plist-get plist :version) "missing")))
        (mevedel-session-persistence--validate-current-sidecar plist)))))))

(defvar-local mevedel-session--read-only-mode nil
  "Non-nil when this chat buffer is in read-only session mode.
Set by the restore path when a cross-host lock cannot be safely
broken.  While set: autosave is inhibited (the terminal DONE handler
early-outs), the view buffer refuses to insert into the data buffer,
and the data buffer itself is marked `buffer-read-only'.")

(defun mevedel-session-persistence--apply-read-only-mode (buf &optional reason)
  "Put BUF into read-only session mode.

REASON replaces the default explanation.  Handing control away deliberately
is not the same event as finding someone else already holding the lock, and
saying the wrong one is how a user learns to ignore the notice.

The notice is a message, not a warning: the buffer states its own authority
in the interaction zone and the cockpit header for as long as it holds, so a
popped-up window would repeat durably visible state and steal the frame at
the moment the user is watching the handoff.  A lease that fails or is lost
is a different matter and still warns.
See `mevedel-session--read-only-mode' for semantics."
  (with-current-buffer buf
    (setq buffer-read-only t)
    (setq-local mevedel-session--read-only-mode t)
    (message "mevedel: %s"
             (or reason
                 "session opened read-only; another client holds the lease"))))

(defun mevedel-session-persistence--synthesize-session (session-dir workspace)
  "Build a minimal `mevedel-session' when the sidecar is absent.
Used when the sidecar file for SESSION-DIR is missing or unparseable.
WORKSPACE is the current workspace (resolved by the caller)."
  (require 'mevedel-execution-target)
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name session-dir)))
         (name (if (string-match
                    "\\`\\(.*?\\)-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9a-f]+\\'"
                    dir-name)
                   (match-string 1 dir-name)
                 dir-name))
         (highest (mevedel-session-persistence--detect-highest-segment
                   session-dir))
         (now (format-time-string "%FT%H-%M-%S")))
    (mevedel-session--create
     :name            name
     :workspace       workspace
     :execution-target
     (mevedel-execution-target-create (mevedel-workspace-root workspace))
     :authority-mode
     (mevedel-session-persistence--workspace-authority-mode workspace)
     :working-directory (mevedel-workspace-root workspace)
     :touched-files   (make-hash-table :test #'equal)
     :mentions-shown  (make-hash-table :test #'equal)
     :session-id      dir-name
     :last-observed-date (format-time-string "%F")
     :agent-types-snapshot :uninitialized
     :skills-snapshot :uninitialized
     :save-path       (file-name-as-directory session-dir)
     :current-segment (if (> highest 0) highest 1)
     :created-at      now
     :updated-at      now
     :turn-count      0
     :permission-mode 'ask
     :sandbox-mode 'best-effort)))

(defun mevedel-session-persistence--find-live-buffer (session-id buf-name)
  "Return the named live root buffer for SESSION-ID, or nil.

The canonical session buffer name is the durable locator.  Persistence does
not search unrelated buffers or infer roles from view-local variables."
  (or (progn
        (require 'mevedel-session-control-transfer)
        (mevedel-session-control-transfer-root-buffer-for-id session-id))
      (let ((candidate (get-buffer buf-name)))
        (when (and (mevedel-session-persistence--root-data-buffer-p candidate)
                   (with-current-buffer candidate
                     (or (not (bound-and-true-p mevedel--session))
                         (equal (mevedel-session-session-id mevedel--session)
                                session-id))))
          candidate))))

(defun mevedel-session-persistence--maybe-prune-orphan (session-dir segment-path)
  "Offer to delete SESSION-DIR when SEGMENT-PATH is missing.
Called from the restore path when the live segment file cannot be
found.  Returns nil; signals `user-error' to abort the restore."
  (cond
   ((and (file-directory-p session-dir)
         (yes-or-no-p
          (format "Live segment %s is missing.  Delete orphan session directory %s? "
                  segment-path session-dir)))
    (delete-directory session-dir t)
    (user-error "Orphan session directory deleted"))
   (t
    (user-error "Session segment file %s missing" segment-path))))

(defun mevedel-session-persistence--hydrate-restored-buffer
    (buf session workspace segment-path acquired additional-roots
         lifecycle-source &optional artifact-callback)
  "Hydrate fresh restore buffer BUF and return its agent repair count.
SESSION and WORKSPACE are planted before gptel restores persisted state.
SEGMENT-PATH is reconciled when ACQUIRED owns the session lock.  Additional
workspace roots and LIFECYCLE-SOURCE restore the saved session environment.
ARTIFACT-CALLBACK collects remote transcript replacements for one batch."
  (with-current-buffer buf
    (unless (derived-mode-p 'org-mode)
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode)))
    (setq-local mevedel--session session)
    (setq-local mevedel--workspace workspace)
    (setq-local default-directory
                (mevedel-session-working-directory session))
    (when additional-roots
      (setq-local mevedel-workspace-additional-roots additional-roots))
    (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
      (mevedel--chat-buffer-disable-org-element-cache))
    (require 'mevedel-transcript-restore)
    (mevedel-transcript-restore-gptel-state)
    (when acquired
      (mevedel-session-persistence--check-target-incarnation session buf)
      (require 'mevedel-pipeline)
      (when (> (mevedel-pipeline-reconcile-lost-executions buf) 0)
        (if artifact-callback
            (funcall
             artifact-callback
             (list :path segment-path
                   :content (buffer-string)
                   :coding (or buffer-file-coding-system 'utf-8-unix)))
          (mevedel-session-persistence--write-current-buffer-atomically
           segment-path)
          (set-visited-file-modtime))
        (set-buffer-modified-p nil)))
    (unless acquired
      (mevedel-session-persistence--apply-read-only-mode buf))
    (mevedel--chat-buffer-init-common
     buf workspace (or lifecycle-source "resume") (not acquired))
    (require 'mevedel-agent-persistence)
    (prog1
        (mevedel-agent-persistence-restore-tree
         session buf (bound-and-true-p mevedel-session--read-only-mode))
      (mevedel-session-persistence--load-instructions session buf))))

(defun mevedel-session-persistence--finish-restored-buffer
    (buf session live persist-repairs-p &optional repair-artifacts)
  "Finish restoring BUF for SESSION and return BUF.
LIVE means BUF was already initialized.  When PERSIST-REPAIRS-P is non-nil,
write repaired sidecar state before rendering the companion view.
REPAIR-ARTIFACTS are portable transcript replacements published before the
sidecar commit marker."
  (with-current-buffer buf
    (when persist-repairs-p
      (let ((portable-p
             (mevedel-session-persistence--portable-authority-p session))
            (published-p nil))
        (condition-case err
            (progn
              (if portable-p
                  (progn
                    (require 'mevedel-session-durability)
                    (require 'mevedel-session-publication)
                    (mevedel-session-publication-publish
                     session
                     (append
                      repair-artifacts
                      (list
                       (list
                        :path
                        (mevedel-session-persistence--sidecar-path
                         (mevedel-session-save-path session))
                        :content
                        (mevedel-session-persistence--printed-value
                         (mevedel-session-persistence--build-sidecar
                          session buf))
                        :commit-marker t)))))
                (mevedel-session-persistence-write
                 (mevedel-session-persistence--sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-persistence--build-sidecar session buf)))
              (setq published-p t))
          (error
           (unless (or (not portable-p)
                       (mevedel-session-pending-publication session))
             (signal (car err) (cdr err)))))
        (when (and published-p
                   (cl-find buffer-file-name repair-artifacts
                            :key (lambda (artifact)
                                   (plist-get artifact :path))
                            :test #'equal))
          (set-visited-file-modtime))))
    (unless live
      (mevedel-session-persistence--notify-session-event
       session 'load-history session))
    (mevedel-session-persistence--notify-session-event
     session 'rerender))
  buf)

(defun mevedel-session-persistence-resume-id (workspace session-id)
  "Resume WORKSPACE session SESSION-ID, or return nil when unavailable."
  (unless (and (stringp session-id)
               (not (string-empty-p session-id))
               (equal session-id (file-name-nondirectory session-id))
               (not (member session-id '("." ".."))))
    (error "Invalid session id: %S" session-id))
  (let ((session-dir
         (file-name-as-directory
          (file-name-concat
           (mevedel-session-persistence--sessions-dir workspace)
           session-id))))
    (when (file-directory-p session-dir)
      (mevedel-session-persistence-restore session-dir nil nil workspace))))

(defun mevedel-session-persistence--cold-workspace (session-dir sidecar)
  "Return the workspace SIDECAR names, as reachable from SESSION-DIR.

The persisted root is target-native, so it is only a complete path for a
client whose filesystem authority is the target itself.  SESSION-DIR is a
live path below that same root, so its TRAMP prefix -- empty on the target,
the client's spelling of the connection elsewhere -- qualifies the persisted
root for the client doing the restore."
  (let* ((saved (plist-get sidecar :workspace))
         (root (concat (file-remote-p session-dir)
                       (plist-get saved :target-native-root))))
    (mevedel-workspace-get-or-create
     (plist-get saved :type) root root (plist-get saved :name))))

(defun mevedel-session-persistence-restore
    (session-dir &optional lifecycle-source session-override workspace)
  "Restore the chat buffer for the session at SESSION-DIR.

Loads the sidecar (or, locally, synthesizes a minimal session when the sidecar
is missing/unreadable), opens the live segment file in a buffer named
per `mevedel-session-buffer-name', enables `org-mode' and
`gptel-mode' (the latter triggers gptel's restore of text-property
bounds and config), hydrates the session struct on the buffer, then
runs `mevedel--chat-buffer-init-common'.

Returns the chat buffer.  If a buffer for this session is already
alive (matched by session-id), switches to it instead of re-loading.

LIFECYCLE-SOURCE defaults to \"resume\".  SESSION-OVERRIDE, when non-nil,
is the already-projected in-memory session used by a newly published fork.
WORKSPACE is the currently opened workspace authority.

Tasks are deserialized from the sidecar.  Touched-files and
mentions-shown reset to empty hash tables on load."
  (let* ((direct-sidecar
          (mevedel-session-persistence--sidecar-path session-dir))
         ;; A supplied workspace is the authority profile for cold discovery.
         ;; Without one, read the fixed sidecar only to learn which committed
         ;; profile must be verified below.
         (cold-sidecar
          (and (not workspace)
               (not session-override)
               (mevedel-session-control-fs-path-exists-p direct-sidecar)
               (mevedel-session-persistence-read direct-sidecar)))
         (authority-mode
          (or (and session-override
                   (mevedel-session-persistence--authority-mode
                    session-override))
              (and workspace
                   (mevedel-session-persistence--workspace-authority-mode
                    workspace))
              (and cold-sidecar
                   (mevedel-session-persistence--validate-authority-mode
                    (plist-get cold-sidecar :authority-mode)
                    (plist-get cold-sidecar :workspace)))
              (and (mevedel-session-control-fs-path-exists-p
                    (file-name-concat session-dir ".lease"))
                   'portable)
              (and (mevedel-session-control-fs-path-exists-p
                    (file-name-concat session-dir ".lock"))
                   'pid-lock)
              (error "Session authority profile is unavailable: %s"
                     session-dir)))
         (_authority-check
          (mevedel-session-persistence--authority-mode-for-path
           session-dir session-override authority-mode))
         (portable-p (eq authority-mode 'portable))
         (publication
         (when portable-p
            (require 'mevedel-session-durability)
            (require 'mevedel-session-publication)
            (or (mevedel-session-publication-read session-dir)
                (user-error
                 "Portable session has no committed publication: %s"
                 session-dir))))
         (sidecar-path
          (if portable-p
              (plist-get publication :sidecar)
            direct-sidecar))
         (sidecar
          (mevedel-session-persistence-load-sidecar sidecar-path))
         (had-sidecar-p    (not (null sidecar)))
         (opened-workspace
          (or workspace
              (and session-override
                   (mevedel-session-workspace session-override))
              (and sidecar
                   (mevedel-session-persistence--cold-workspace
                    session-dir sidecar))
              (mevedel-workspace)))
         (result           (when sidecar
                             (mevedel-session-persistence-deserialize
                              sidecar opened-workspace)))
         (session          (or session-override
                               (plist-get result :session)
                               (if portable-p
                                   (user-error
                                    "Portable session has no valid published sidecar: %s"
                                    session-dir)
                                 (mevedel-session-persistence--synthesize-session
                                  session-dir opened-workspace))))
         (agent-registry-repaired-p
          (plist-get result :agent-registry-repaired-p))
         (additional-roots (plist-get result :additional-roots))
         (workspace        (mevedel-session-workspace session))
         (sidecar-current-n (and had-sidecar-p
                                 (mevedel-session-current-segment session))))
    ;; `save-path' is intentionally not serialized in the sidecar: the
    ;; session directory itself is the source of truth at restore time.
    (setf (mevedel-session-save-path session)
          (file-name-as-directory session-dir)
          (mevedel-session-publication session) publication)
    ;; Workspace relocation: rewrite path-bearing fields if the saved
    ;; root no longer matches the current workspace's root.
    (when had-sidecar-p
      (mevedel-session-persistence--reconcile-relocation
       session (plist-get sidecar :workspace)))
    (let* ((buf-name     (mevedel-session-buffer-name
                          (mevedel-session-name session)
                          workspace))
           (session-id   (mevedel-session-session-id session))
           ;; Prefer session-id-based lookup so two saved sessions
           ;; named `main' in one workspace don't collide.
           (live         (mevedel-session-persistence--find-live-buffer
                          session-id buf-name))
           (cwd-retargeted-p
            (when (and (not live)
                       (not (file-directory-p
                             (mevedel-session-working-directory session))))
              (setf (mevedel-session-working-directory session)
                    (mevedel--normalize-session-directory
                     (read-directory-name
                      (format "Session directory %s is missing; resume in: "
                              (mevedel-session-working-directory session))
                      (mevedel-workspace-root workspace)
                      (mevedel-workspace-root workspace)
                      t)
                     workspace))))
           ;; Acquire the lock BEFORE opening the segment file.  If the
           ;; user aborts the conflict prompt (`user-error') we unwind
           ;; before any buffer is materialized, so no stray half-
           ;; initialized chat buffer is left behind.  Read-only
           ;; acquisitions return nil; we still open the buffer but
           ;; flip it to read-only below.  `live' skips acquisition
           ;; because the buffer is already owned by this Emacs.
           (acquired
           (unless live
              (if (and session-override
                       portable-p
                       (progn
                         (require 'mevedel-session-durability)
                         (require 'mevedel-session-publication)
                         (mevedel-session-durability-lease-owned-p session)))
                  t
                (mevedel-session-persistence-lock-acquire
                 session-dir buf-name session)))))
      (let ((repair-artifacts nil)
            repair-callback
            self-healed-predecessor
            buf
            (setup-done nil))
        (unwind-protect
            (progn
              (setq repair-callback
                    (and acquired
                         (not live)
                         portable-p
                         (lambda (artifact)
                           (push artifact repair-artifacts))))
              ;; Once fenced, confirm the same published state still owns the
              ;; portable lease head.  File-workspace sessions retain their sidecar-last
              ;; comparison because they have no immutable publication head.
              (when (and acquired (not live))
                (if portable-p
                    (let ((current
                           (mevedel-session-publication-read
                            session-dir)))
                      (unless (and current
                                   (equal (plist-get publication :head)
                                          (plist-get current :head)))
                        (user-error
                         "Session state changed while acquiring its lease; retry restore"))
                      (setf (mevedel-session-publication session) current))
                  (unless
                      (equal
                       sidecar
                       (mevedel-session-persistence-load-sidecar sidecar-path))
                    (user-error
                     "Session state changed while acquiring its lock; retry restore"))))
              ;; Files newer than the sidecar may be an incomplete publication.
              ;; Only the mutation owner may reconcile them; inspection trusts
              ;; the sidecar.
              (when (and acquired (not live) (not portable-p))
                (setq self-healed-predecessor
                      (mevedel-session-persistence--self-heal-segment-counter
                       session session-dir (not (null repair-callback)))))
              (let* ((segment-n (mevedel-session-current-segment session))
                     (segment-path
                      (mevedel-session-persistence--segment-path
                       session-dir segment-n))
                     (agent-repairs 0))
                (setq
                 buf
                 (or
                  live
                  (if portable-p
                      (mevedel-session-persistence-find-artifact-noselect
                       session (file-name-nondirectory segment-path)
                       (not acquired))
                    (and
                     (file-exists-p segment-path)
                     (mevedel-session-persistence--find-file-noselect
                      segment-path)))))
                (unless (and buf (buffer-live-p buf)
                             (or portable-p (file-exists-p segment-path)))
                  (if (and acquired (not portable-p))
                      (mevedel-session-persistence--maybe-prune-orphan
                       session-dir segment-path)
                    (user-error
                     "%s session segment is unavailable: %s"
                     (if portable-p "Published" "Session") segment-path)))
                (when (and acquired (not live))
                  (mevedel-session-persistence--reconcile-lost-execution-segments
                   session segment-path repair-callback))
                (when (and repair-callback
                           self-healed-predecessor
                           (file-regular-p self-healed-predecessor))
                  (if-let ((artifact
                            (cl-find
                             self-healed-predecessor repair-artifacts
                             :key (lambda (candidate)
                                    (plist-get candidate :path))
                             :test #'equal)))
                      (let ((coding (or (plist-get artifact :coding)
                                        'utf-8-unix)))
                        (setf
                         (plist-get artifact :content)
                         (mevedel-session-persistence--finalized-segment-text
                          (plist-get artifact :content) coding)))
                    (with-temp-buffer
                      (insert-file-contents self-healed-predecessor)
                      (let ((coding (or buffer-file-coding-system
                                        'utf-8-unix)))
                        (funcall
                         repair-callback
                         (list
                          :path self-healed-predecessor
                          :content
                          (mevedel-session-persistence--finalized-segment-text
                           (buffer-string) coding)
                          :coding coding))))))
                (with-current-buffer buf
                  (mevedel-session-set-root-buffer session buf)
                  (unless (equal (buffer-name) buf-name)
                    (rename-buffer buf-name t))
                  (unless (and buffer-file-name
                               (equal (expand-file-name buffer-file-name)
                                      (expand-file-name segment-path)))
                    (setq buffer-file-name segment-path))
                  (when
                      (and
                       (derived-mode-p 'org-mode)
                       (fboundp
                        'mevedel--chat-buffer-disable-org-element-cache))
                    (mevedel--chat-buffer-disable-org-element-cache)))
                (unless live
                  (setq agent-repairs
                        (mevedel-session-persistence--hydrate-restored-buffer
                         buf session workspace segment-path acquired
                         additional-roots lifecycle-source repair-callback)))
                (mevedel-session-persistence--finish-restored-buffer
                 buf session live
                 (and acquired
                      (or repair-artifacts
                          (and
                           had-sidecar-p
                           (or cwd-retargeted-p
                               agent-registry-repaired-p
                               (> agent-repairs 0)
                               (and sidecar-current-n
                                    (not (= sidecar-current-n segment-n)))))))
                 (nreverse repair-artifacts))
                (setq setup-done t)
                buf))
          ;; Any failure after acquisition releases the session lease and
          ;; removes a freshly opened buffer.
          (unless setup-done
            (when (and acquired (not live))
              (condition-case _
                  (mevedel-session-persistence-lock-release
                   session-dir session)
                (error nil)))
            (when (and (not live) buf (buffer-live-p buf))
              (condition-case _
                  (kill-buffer buf)
                (error nil)))))))))


;;
;;; File restore plan

(defun mevedel-session-persistence--latest-snapshot-entry (session path)
  "Return the highest-version snapshot plist for PATH in SESSION, or nil."
  (let ((best nil) (best-version 0))
    (dolist (turn-entry (mevedel-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  ((not (plist-get (cdr entry) :gap)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best-version)
          (setq best-version v
                best          (cdr entry)))))))

(defun mevedel-session-persistence--read-backup (session backup-name)
  "Return SESSION's BACKUP-NAME as literal bytes.

Portable project Rewind resolves only the committed immutable publication.  File
sessions retain their existing direct file-history read."
         (if (mevedel-session-persistence--portable-authority-p session)
      (mevedel-session-persistence-read-artifact
       session (file-name-concat "file-history" backup-name) t)
    (mevedel-file-history--read-file-raw
     (mevedel-file-history--backup-path
      (mevedel-session-save-path session) backup-name))))

(defun mevedel-session-persistence--state-at-turn
    (session cum-turn &optional before-turn)
  "Return SESSION tracked-file state at CUM-TURN.

For each path that ever appeared in SESSION's `:file-snapshots',
picks the latest checkpoint through CUM-TURN.  When BEFORE-TURN is non-nil,
picks its earliest checkpoint in the discarded suffix instead."
  (let ((state (make-hash-table :test #'equal)))
    (dolist
        (turn-entry
         (sort (copy-sequence (mevedel-session-file-snapshots session))
               (if before-turn
                   (lambda (a b) (< (car a) (car b)))
                 (lambda (a b) (> (car a) (car b))))))
      (let ((turn (car turn-entry)))
        (when (if before-turn
                  (>= turn cum-turn)
                (<= turn cum-turn))
          (dolist (file-entry (cdr turn-entry))
            (unless (gethash (car file-entry) state)
              (puthash (car file-entry) (cdr file-entry) state))))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) state)
      result)))

(defun mevedel-session-persistence--plan-action
    (session path target-plist &optional before-turn)
  "Return SESSION restore action plist for PATH.

TARGET-PLIST is the snapshot entry recorded for PATH at the picked turn
or earlier.  Possible `:action' values are:

  noop       File already matches target state.
  delete     Target state is absent; file currently exists.
  create     Target has content; file currently absent.
  restore    Target has content; file differs but matches its own
             latest snapshot (i.e., no detected external changes).
  overwrite  Target has content; file differs from target AND from
             latest snapshot (external edits will be overwritten)."
  (let* ((target-backup-name
          (plist-get target-plist
                     (if before-turn :pre-backup-name :backup-name)))
         (currently-exists   (file-exists-p path)))
    (cond
     ;; Target says "absent" at the picked turn.
     ((null target-backup-name)
      (if currently-exists
          (list :action 'delete :path path)
        (list :action 'noop :path path)))
     ;; Target has content; file currently absent.
     ((not currently-exists)
      (list :action 'create
            :path path
            :backup-name target-backup-name))
     ;; Target has content; file currently present -- compare.
     (t
      (let* ((target-content
              (mevedel-session-persistence--read-backup
               session target-backup-name))
             (current-content
              (mevedel-file-history--read-file-raw path)))
        (if (string-equal current-content target-content)
            (list :action 'noop :path path)
          ;; Differs from target.  Check vs LATEST snapshot to detect
          ;; external changes since.
          (let* ((latest (mevedel-session-persistence--latest-snapshot-entry
                          session path))
                 (latest-name (and latest (plist-get latest :backup-name)))
                 (latest-content
                  (and latest-name
                       (mevedel-session-persistence--read-backup
                        session latest-name)))
                 (diverged (not (and latest-content
                                     (string-equal current-content
                                                   latest-content)))))
            (list :action (if diverged 'overwrite 'restore)
                  :path path
                  :backup-name target-backup-name
                  :diverged diverged))))))))

(defun mevedel-session-persistence-restore-plan
    (session cum-turn &optional before-turn)
  "Compute SESSION's captured file-restore plan at CUM-TURN.

Returns a list of plan-entry plists (see
`mevedel-session-persistence--plan-action').  An empty list means
nothing to do.  When BEFORE-TURN is non-nil, target the pre-turn checkpoint."
  (let ((target-state
         (mevedel-session-persistence--state-at-turn
          session cum-turn before-turn))
        (plan nil))
    (dolist (entry target-state)
      (unless (plist-get (cdr entry) :gap)
        (push (mevedel-session-persistence--plan-action
               session (car entry) (cdr entry) before-turn)
              plan)))
    (cl-remove-if
     (lambda (e) (eq 'noop (plist-get e :action)))
     (nreverse plan))))

(defun mevedel-session-persistence--checkpoint-gaps (session cum-turn)
  "Return known SESSION checkpoint gaps before CUM-TURN."
  (cl-loop for (path . checkpoint)
           in (mevedel-session-persistence--state-at-turn
               session cum-turn t)
           when (plist-get checkpoint :gap)
           collect (list :path path :reason (plist-get checkpoint :gap))))

(defun mevedel-session-persistence--directive-capture-gaps
    (session target-turn)
  "Return untracked directive effects discarded from TARGET-TURN in SESSION."
  (when-let* ((workspace (mevedel-session-workspace session)))
    (let ((session-id (mevedel-session-session-id session)))
      (cl-loop
       for directive in
       (mevedel-workspace-directives workspace)
       append
       (cl-loop
	for attempt in (mevedel-directive-attempts directive)
	for checkpoint = (mevedel-directive-attempt-checkpoint attempt)
	when (and (equal session-id (plist-get checkpoint :session-id))
                  (>= (or (plist-get checkpoint :turn) 0) target-turn))
	append
	(mapcar
	 (lambda (effect)
           (list :path
		 (format "Directive %s via %s"
			 (mevedel-directive-id directive) (car effect))
		 :reason (cdr effect)))
	 (mevedel-directive-attempt-untracked-effects attempt)))))))

(defun mevedel-session-persistence--summarize-plan (plan)
  "Return a human-readable one-line summary of restore PLAN."
  (let ((counts (make-hash-table)))
    (dolist (entry plan)
      (cl-incf (gethash (plist-get entry :action) counts 0)))
    (format "%d create, %d restore, %d overwrite, %d delete"
            (gethash 'create counts 0)
            (gethash 'restore counts 0)
            (gethash 'overwrite counts 0)
            (gethash 'delete counts 0))))

(defvar-local mevedel-session-persistence--plan-buffer-session nil
  "Buffer-local session for the `*mevedel-restore-plan*' buffer.
Consumed by `mevedel-session-persistence--plan-row-diff' so `d' on a
plan row can resolve the backup file.")

(defvar mevedel-session-persistence--plan-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "d")
      #'mevedel-session-persistence--plan-row-diff)
    map)
  "Keymap for the `*mevedel-restore-plan*' buffer.
Adds `d' -- show diff between current file and target snapshot.")

(defun mevedel-session-persistence--plan-row-diff ()
  "Show a diff between the current file and the restore target backup.
Invoked from `*mevedel-restore-plan*' via the `d' binding.  The row
at point identifies the path; its `mevedel-plan-entry' text property
carries the plan-entry plist."
  (interactive)
  (let* ((entry
          (get-text-property (line-beginning-position) 'mevedel-plan-entry))
         (session mevedel-session-persistence--plan-buffer-session))
    (unless entry
      (user-error "No restore plan row at point"))
    (unless session
      (user-error "Plan buffer has no associated session"))
    (let* ((path        (plist-get entry :path))
           (action      (plist-get entry :action))
           (backup-name (plist-get entry :backup-name))
           (backup-path (and backup-name
                             (mevedel-file-history--backup-path
                              (mevedel-session-save-path session)
                              backup-name))))
      (pcase action
        ('delete
         (user-error "Row is a delete action; nothing to diff against"))
        ('noop
         (user-error "Row is a noop; nothing to diff"))
        (_
         (unless (and backup-path (file-exists-p backup-path))
           (user-error "Backup file missing: %s" backup-path))
         (diff (or (and (file-exists-p path) path) "/dev/null")
               backup-path nil 'no-async))))))

(defun mevedel-session-persistence--render-plan-buffer (plan &optional session)
  "Render PLAN into `*mevedel-restore-plan*' for user inspection.
When SESSION is non-nil, attach it so the `d' binding can resolve
backup paths for per-row diffs."
  (with-current-buffer (get-buffer-create "*mevedel-restore-plan*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Restore plan: %s\n"
                      (mevedel-session-persistence--summarize-plan plan)))
      (insert "Press `d' on a row to diff current vs. snapshot.\n")
      (insert "================================\n\n")
      (dolist (entry plan)
        (let ((row-start (point)))
          (insert
           (format "  %-9s  %s%s\n"
                   (symbol-name (plist-get entry :action))
                   (plist-get entry :path)
                   (if (plist-get entry :diverged)
                       "  (current != latest snapshot -- external edits)"
                     "")))
          (put-text-property row-start (point)
                             'mevedel-plan-entry entry))))
    (special-mode)
    (use-local-map mevedel-session-persistence--plan-buffer-map)
    (setq-local mevedel-session-persistence--plan-buffer-session session)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun mevedel-session-persistence--apply-restore-action (session entry)
  "Apply one restore ENTRY (plan-entry plist) for SESSION."
  (let* ((path        (plist-get entry :path))
         (action      (plist-get entry :action))
         (backup-name (plist-get entry :backup-name)))
    (pcase action
      ('noop nil)
      ('delete
       (when (file-exists-p path)
         (delete-file path)))
      ((or 'create 'restore 'overwrite)
       (let ((content
              (mevedel-session-persistence--read-backup
               session backup-name)))
         (let ((dir (file-name-directory path)))
           (when (and dir (not (file-directory-p dir)))
             (make-directory dir t)))
         (let ((coding-system-for-write 'no-conversion))
           (write-region content nil path nil 'silent)))))))

(defun mevedel-session-persistence-execute-restore (session plan)
  "Execute restore PLAN for SESSION, stopping on the first failure.

Returns a plist describing the outcome:
  (:succeeded N :failed PATH-OR-NIL :error STR-OR-NIL :total N)

Successful actions are applied in order; on error, remaining plan
entries are not attempted.  The user-visible report goes to
`*mevedel-restore-results*'."
  (let ((succeeded 0) (failed nil) (err-str nil)
        (total (length plan)))
    (catch 'failed
      (dolist (entry plan)
        (condition-case e
            (progn
              (mevedel-session-persistence--apply-restore-action session entry)
              (cl-incf succeeded))
          (error
           (setq failed   (plist-get entry :path)
                 err-str  (error-message-string e))
           (throw 'failed nil)))))
    (with-current-buffer (get-buffer-create "*mevedel-restore-results*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Restore results: %d/%d files done\n"
                        succeeded total))
        (when failed
          (insert (format "Failed on %s: %s\n" failed err-str)))))
    (list :succeeded succeeded :failed failed
          :error err-str :total total)))

(defun mevedel-session-persistence--modified-buffers-for-plan (plan)
  "Return modified buffers visiting files affected by restore PLAN."
  (let (buffers)
    (dolist (entry plan)
      (when-let* ((path (plist-get entry :path))
                  (buf (find-buffer-visiting path))
                  ((buffer-live-p buf)))
        (with-current-buffer buf
          (when (buffer-modified-p)
            (push buf buffers)))))
    (nreverse (cl-remove-duplicates buffers))))

(defun mevedel-session-persistence--prepare-buffers-for-restore
    (session cum-turn plan)
  "Prepare visiting buffers before restoring PLAN for SESSION at CUM-TURN.

If modified buffers visit affected files, prompt the user to save,
discard, or abort.  Returns the current restore plan, recomputing it
after saves.  Returns nil when the restore should be aborted."
  (let ((current-plan plan)
        done)
    (while (not done)
      (let ((buffers
             (mevedel-session-persistence--modified-buffers-for-plan
              current-plan)))
        (if (null buffers)
            (setq done t)
          (pcase (read-char-choice
                  (format
                   "Rewind affects %d modified buffer%s (%s): [s]ave, [d]iscard, [a]bort? "
                   (length buffers)
                   (if (= 1 (length buffers)) "" "s")
                   (mapconcat #'buffer-name buffers ", "))
                  '(?s ?d ?a))
            (?s
             (save-some-buffers
              nil
              (lambda ()
                (memq (current-buffer) buffers)))
             (setq current-plan
                   (mevedel-session-persistence-restore-plan
                    session cum-turn t)))
            (?d
             (dolist (buf buffers)
               (with-current-buffer buf
                 (revert-buffer t t t)))
             (setq done t))
            (?a
             (setq current-plan :abort
                   done t))))))
    current-plan))

(defun mevedel-session-persistence--refresh-restored-buffers (plan result)
  "Refresh visiting buffers for files restored by PLAN.

RESULT is the plist returned by `mevedel-session-persistence-execute-restore'."
  (let ((remaining (plist-get result :succeeded)))
    (dolist (entry plan)
      (when (> remaining 0)
        (cl-decf remaining)
        (let ((path (plist-get entry :path))
              (action (plist-get entry :action)))
          (pcase action
            ((or 'create 'restore 'overwrite)
             (when-let* (((file-exists-p path))
                         (buf (find-buffer-visiting path)))
               (with-current-buffer buf
                 (revert-buffer t t t))))
            ('delete
             (when-let* ((buf (find-buffer-visiting path)))
               (with-current-buffer buf
                 (set-buffer-modified-p nil))
               (kill-buffer buf)))))))))


;;
;;; Rewind picker

(defun mevedel-session-persistence--prompt-label (prompt)
  "Return PROMPT's concise picker and impact label."
  (if (eq (plist-get prompt :kind) 'directive)
      (let ((id (or (plist-get prompt :directive-id) "directive")))
        (format "◆ %s · %s"
                (substring id 0 (min 8 (length id)))
                (capitalize
                 (replace-regexp-in-string
                  "-" " "
                  (symbol-name (or (plist-get prompt :action) 'directive))))))
    (or (plist-get prompt :preview) "(empty prompt)")))

(defun mevedel-session-persistence--prompt-candidates (session)
  "Return alist entries of DISPLAY to PLIST for SESSION prompt history.

PLIST has `:segment', `:turn', `:file-turn', `:cum-turn', `:pos',
`:preview'.
DISPLAY is unique across the whole session -- segment and turn
numbers are folded into the display string so duplicate previews
do not collide.

Segments are listed newest-first (the live segment at the top of
the picker); within each segment, prompts are listed newest-first so
recent turns appear before older turns."
  (let ((all nil))
    (dolist (segment-entry
             (sort (copy-sequence (mevedel-session-prompt-index session))
                   ;; Newest segment first.
                   (lambda (a b) (> (car a) (car b)))))
      (let ((segment-n (car segment-entry)))
        (dolist (prompt (reverse (cdr segment-entry)))
          (when (plist-get prompt :fork-point-id)
            (let* ((turn (plist-get prompt :turn))
                   (display
                    (format "S%d T%d  %s" segment-n turn
                            (mevedel-session-persistence--prompt-label
                             prompt)))
                   (target (copy-sequence prompt)))
              (plist-put target :segment segment-n)
              (push (cons display target) all))))))
    (nreverse all)))

(defvar mevedel-session-persistence--prompt-history nil
  "History list for `mevedel-rewind' picks.")

(defun mevedel-session-persistence--prompt-collection-fn (candidates lookup)
  "Return a completion table for the rewind picker.

CANDIDATES is the list returned by
`mevedel-session-persistence--prompt-candidates' (each element is
`(DISPLAY . PLIST)').  LOOKUP is a hash-table mapping DISPLAY to
PLIST so the metadata helpers resolve in O(1).

The returned function answers `(metadata)' with:
  - category          = `mevedel-prompt' (consult/marginalia hook);
  - annotation-function inserts a right-aligned S<segment> T<turn>
    marker so picker rows carry context beyond the preview string;
  - group-function groups rows by segment with headings like
    `Segment N'.

Any other action delegates to `complete-with-action' over the raw
DISPLAY strings in CANDIDATES order -- newest segment first, newest
turn first within each segment."
  (let ((displays (mapcar #'car candidates)))
    (lambda (string pred action)
      (cond
       ((eq action 'metadata)
        `(metadata
          (category . mevedel-prompt)
          (display-sort-function . identity)
          (cycle-sort-function . identity)
          (annotation-function
           . ,(lambda (s)
                (when-let* ((p (gethash s lookup)))
                  (format "  S%d T%d"
                          (or (plist-get p :segment) 0)
                          (or (plist-get p :turn) 0)))))
          (group-function
           . ,(lambda (s transform)
                (if transform
                    s
                  (when-let* ((p (gethash s lookup)))
                    (format "Segment %d"
                            (or (plist-get p :segment) 0))))))))
       (t
        (complete-with-action action displays string pred))))))

(defun mevedel-session-persistence--format-relative-time (iso)
  "Format ISO (a `YYYY-MM-DDTHH-MM-SS' string) as a relative age.
Returns strings like `2h ago' / `yesterday' / `Apr 22'.  Returns a
placeholder when ISO cannot be parsed."
  (let ((t2 (mevedel-session-persistence--parse-iso-time iso)))
    (if (not t2)
        "?"
      (let* ((secs (- (float-time) (float-time t2)))
             (abs  (abs secs)))
        (cond
         ((< abs 60) "just now")
         ((< abs 3600) (format "%dm ago" (/ abs 60)))
         ((< abs (* 24 3600)) (format "%dh ago" (/ abs 3600)))
         ((< abs (* 48 3600)) "yesterday")
         ((< abs (* 7 24 3600)) (format "%dd ago" (/ abs (* 24 3600))))
         (t (format-time-string "%b %d" t2)))))))

(defun mevedel-session-persistence--find-turn-cutoff (turn-n)
  "Return the position right before the (TURN-N + 1)th user prompt.
Returns `point-max' when TURN-N is the final user prompt.  Skips the
leading org property drawer, `#+begin_summary'/`#+end_summary' block
bodies, and gptel org tool/reasoning scaffolding to stay consistent with
`mevedel-session-persistence--collect-prompts'."
  (save-excursion
    (save-restriction
      (widen)
      (or (plist-get
           (nth turn-n
                (mevedel-session-persistence--collect-prompts
                 (current-buffer)))
           :pos)
          (point-max)))))

(defun mevedel-session-persistence--load-rewind-target
    (session buffer target &optional before-turn)
  "Load SESSION's TARGET transcript boundary into BUFFER without publishing it.
When BEFORE-TURN is non-nil, discard TARGET itself as well as later text."
  (let* ((segment-n (plist-get target :segment))
         (segment-path
          (mevedel-session-persistence--segment-path
           (mevedel-session-save-path session) segment-n))
         (portable-p
          (mevedel-session-persistence--portable-authority-p session))
         (logical (file-name-nondirectory segment-path))
         (content
          (when portable-p
            (condition-case nil
                (mevedel-session-persistence-read-artifact
                 session logical t)
              (error
               (user-error "Published segment %d is unavailable" segment-n))))))
    (unless (or portable-p (file-exists-p segment-path))
      (user-error "Segment %d file missing: %s" segment-n segment-path))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((buffer-file-name segment-path))
          (if portable-p
              (insert
               (decode-coding-string
                content (or buffer-file-coding-system 'utf-8-unix)))
            (insert-file-contents segment-path))
          (when (derived-mode-p 'org-mode)
            (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
              (mevedel--chat-buffer-disable-org-element-cache))
            ;; Force re-restoration of GPTEL_BOUNDS from the org property.
            (when (fboundp 'gptel-org--restore-state)
              (require 'mevedel-transcript-restore)
              (mevedel-transcript-restore-sanitize-bounds)
              (gptel-org--restore-state))))
        (let* ((id (plist-get target :fork-point-id))
               (fork-point
                (cl-find id
                         (mevedel-session-persistence--fork-point-spans
                          buffer)
                         :key (lambda (entry)
                                (plist-get entry :fork-point-id))
                         :test #'equal))
               (cutoff (if before-turn
                           (plist-get target :pos)
                         (plist-get fork-point :transcript-cutoff))))
          (unless cutoff
            (error "Rewind target is missing from segment %d" segment-n))
          (when (< cutoff (point-max))
            (delete-region cutoff (point-max))))
        (mevedel-transcript-restore-properties t)
        (mevedel-session-persistence--stabilize-gptel-bounds))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun mevedel-session-persistence--resolve-fork-target (session target)
  "Resolve TARGET's stable identity against SESSION's current index."
  (let ((id (plist-get target :fork-point-id)))
    (or
     (cl-loop for (segment . prompts)
              in (mevedel-session-prompt-index session)
              for prompt =
              (cl-find id prompts
                       :key (lambda (entry)
                              (plist-get entry :fork-point-id))
                       :test #'equal)
              when prompt
              return (let ((resolved (copy-sequence prompt)))
                       (plist-put resolved :segment segment)
                       (when-let* ((reservation
                                    (plist-get target
                                               :worktree-reservation)))
                         (plist-put resolved
                                    :worktree-reservation reservation))
                       resolved))
     (user-error "Assistant fork point no longer exists"))))

(defun mevedel-session-persistence--assert-stable-source
    (session buffer operation)
  "Refuse OPERATION when SESSION or BUFFER owns live work."
  (when (mevedel-session-pending-input-p session)
    (user-error
     "Resolve pending input in the Pending Inputs cockpit or clear it with C-c C-q before %s"
     operation))
  (when (buffer-local-value 'mevedel--current-request buffer)
    (user-error "Abort the current request before %s" operation))
  (require 'mevedel-execution)
  (when (mevedel-execution-session-live-p session)
    (user-error "Stop live executions with /ps or /stop before %s" operation))
  (require 'mevedel-agent-control)
  (when (mevedel-agent-control-active-turn-p session)
    (user-error "Interrupt active agent turns before %s" operation))
  (when-let* ((goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active)))
    (user-error "Pause the active Goal before %s" operation)))

(defun mevedel-session-persistence--rewind-cleared-state (session)
  "Return user-facing names of live SESSION state cleared by Rewind."
  (delq
   nil
   (list
    (and (mevedel-session-tasks session) "tasks")
    (and (mevedel-session-goal session) "Goal")
    (and (mevedel-session-agent-registry session) "agents and mailboxes")
    (and (mevedel-session-pending-plan-approval session) "Plan approval")
    (and (mevedel-session-plan-metadata session) "Plan handoff")
    (and (mevedel-session-pending-input-p session) "pending input")
    (and (mevedel-session-permission-queue session) "permission queue")
    (and (mevedel-session-execution-state session) "execution state"))))

(defun mevedel-session-persistence--staged-file-p (path)
  "Return non-nil when PATH differs in the Git index."
  (let ((directory (file-name-directory path)))
    (when (file-directory-p directory)
      (require 'mevedel-execution-target)
      (let* ((target (mevedel-execution-target-create directory))
             (default-directory (file-name-as-directory directory))
             (remote (file-remote-p default-directory))
             (process-environment
              (unless remote process-environment)))
        (and (executable-find "git" remote)
             (= 1
                (process-file
                 "git" nil nil nil
                 "-C"
                 (mevedel-execution-target-native-path target directory)
                 "diff" "--cached" "--quiet" "--"
                 (mevedel-execution-target-native-path target path))))))))

(defun mevedel-session-persistence--detached-child-count
    (session target-turn)
  "Return direct child count detached by rewinding SESSION to TARGET-TURN."
  (let ((session-id (mevedel-session-session-id session))
        (portable-p
         (mevedel-session-persistence--portable-authority-p session))
        (count 0))
    (dolist (entry
             (mevedel-session-persistence-list-sessions
              (mevedel-session-workspace session)))
      (when-let* ((path (plist-get entry :save-path))
                  (sidecar-path
                   (if portable-p
                       (plist-get (plist-get entry :publication) :sidecar)
                     (mevedel-session-persistence--sidecar-path path)))
                  (sidecar
                   (condition-case nil
                       (mevedel-session-persistence-read sidecar-path)
                     (error nil))))
        (when (and (equal session-id
                          (plist-get sidecar :forked-from-session-id))
                   (> (or (plist-get sidecar :forked-from-turn) 0)
                      target-turn))
          (cl-incf count))))
    count))

(defun mevedel-session-persistence--rewind-impact (session target file-plan)
  "Return the complete Rewind impact for SESSION, TARGET, and FILE-PLAN."
  (let* ((target-turn (plist-get target :cum-turn))
         (surviving-turn (1- target-turn)))
    (list
     :target target
     :file-plan file-plan
     :discarded-turns
     (max 0 (1+ (- (or (mevedel-session-turn-count session) 0)
                       target-turn)))
     :discarded-prompts
     (sort
      (cl-loop
       for (segment . prompts) in (mevedel-session-prompt-index session)
       append
       (cl-loop for prompt in prompts
                when (>= (or (plist-get prompt :cum-turn) 0) target-turn)
                collect (plist-put (copy-sequence prompt)
                                   :segment segment)))
      (lambda (a b)
        (< (plist-get a :cum-turn) (plist-get b :cum-turn))))
     :checkpoint-gaps
     (append
      (mevedel-session-persistence--checkpoint-gaps session target-turn)
      (mevedel-session-persistence--directive-capture-gaps
       session target-turn))
     :external-overwrites
     (cl-count 'overwrite file-plan
               :key (lambda (entry) (plist-get entry :action)))
     :staged-files
     (cl-loop for entry in file-plan
              for path = (plist-get entry :path)
              when (mevedel-session-persistence--staged-file-p path)
              collect path)
     :detached-children
     (mevedel-session-persistence--detached-child-count
      session surviving-turn)
     :cleared-state
     (mevedel-session-persistence--rewind-cleared-state session))))

(defun mevedel-session-persistence--rewind-impact-empty-p (impact)
  "Return non-nil when IMPACT would change no Rewind-owned state."
  (and (= 0 (plist-get impact :discarded-turns))
       (null (plist-get impact :file-plan))
       (null (plist-get impact :checkpoint-gaps))
       (null (plist-get impact :cleared-state))))

(defun mevedel-session-persistence--render-rewind-impact (session impact)
  "Display inspectable SESSION Rewind IMPACT."
  (let* ((target (plist-get impact :target))
         (plan (plist-get impact :file-plan))
         (gaps (plist-get impact :checkpoint-gaps))
         (staged (plist-get impact :staged-files))
         (discarded-prompts (plist-get impact :discarded-prompts))
         (cleared (plist-get impact :cleared-state)))
    (with-current-buffer (get-buffer-create "*mevedel-rewind-impact*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Rewind %s before S%d T%d\n\n"
                        (mevedel-session-name session)
                        (plist-get target :segment)
                        (plist-get target :turn)))
        (insert (format "Later turns discarded: %d\n"
                        (plist-get impact :discarded-turns)))
        (insert (format "Captured files restored: %d\n" (length plan)))
        (insert (format "Checkpoint coverage: %s\n"
                        (if gaps
                            (format "incomplete (%d known gap%s)"
                                    (length gaps)
                                    (if (= 1 (length gaps)) "" "s"))
                          "no known gaps")))
        (insert (format "External changes overwritten: %d\n"
                        (plist-get impact :external-overwrites)))
        (insert (format "Staged files left in the index: %d\n"
                        (length staged)))
        (insert (format "Child forks detached: %d\n"
                        (plist-get impact :detached-children)))
        (insert (format "Cleared live state: %s\n"
                        (if cleared
                            (string-join cleared ", ")
                          "none")))
        (insert "Redo: none\n\n")
        (when discarded-prompts
          (insert "Discarded session events:\n")
          (dolist (prompt discarded-prompts)
            (insert (format "  S%d T%d  %s\n"
                            (plist-get prompt :segment)
                            (plist-get prompt :turn)
                            (mevedel-session-persistence--prompt-label
                             prompt))))
          (insert "\n"))
        (dolist (gap gaps)
          (insert (format "gap       %s (%s)\n"
                          (plist-get gap :path)
                          (plist-get gap :reason))))
        (dolist (entry plan)
          (let ((start (point)))
            (insert (format "%-9s %s%s\n"
                            (plist-get entry :action)
                            (plist-get entry :path)
                            (if (member (plist-get entry :path) staged)
                                " (staged index retained)"
                              "")))
            (put-text-property start (point)
                               'mevedel-plan-entry entry))))
      (special-mode)
      (use-local-map mevedel-session-persistence--plan-buffer-map)
      (setq-local mevedel-session-persistence--plan-buffer-session session)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun mevedel-session-persistence--rewind-candidate (session target)
  "Return SESSION state reduced in place semantics to TARGET."
  (let* ((candidate (copy-sequence session))
         (target-turn (plist-get target :cum-turn))
         (turn (1- target-turn))
         (segment (plist-get target :segment)))
    (setf
     (mevedel-session-tasks candidate) nil
     (mevedel-session-task-status-notes candidate) nil
     (mevedel-session-last-task-write-turn candidate) nil
     (mevedel-session-touched-files candidate) (make-hash-table :test #'equal)
     (mevedel-session-turn-count candidate) turn
     (mevedel-session-pending-reminders candidate) nil
     (mevedel-session-specialist-nudge-state candidate) nil
     (mevedel-session-deferred-pending candidate) nil
     (mevedel-session-deferred-injected candidate) nil
     (mevedel-session-deferred-used candidate) nil
     (mevedel-session-deferred-expired candidate) nil
     (mevedel-session-messages candidate) nil
     (mevedel-session-agent-registry candidate) nil
     (mevedel-session-agent-reservations candidate) nil
     (mevedel-session-agent-root-activity candidate) 'idle
     (mevedel-session-agent-root-waiter candidate) nil
     (mevedel-session-pending-steering candidate) nil
     (mevedel-session-pending-follow-ups candidate) nil
     (mevedel-session-pending-input-next-id candidate) nil
     (mevedel-session-pending-input-paused candidate) nil
     (mevedel-session-pending-input-failure-paused candidate) nil
     (mevedel-session-dropped-file-grants candidate) nil
     (mevedel-session-active-dropped-file-grants candidate) nil
     (mevedel-session-mentions-shown candidate) (make-hash-table :test #'equal)
     (mevedel-session-workspace-instruction-hashes candidate) nil
     (mevedel-session-hook-log candidate) nil
     (mevedel-session-hook-log-pending candidate) nil
     (mevedel-session-repair-log candidate) nil
     (mevedel-session-repair-log-pending candidate) nil
     (mevedel-session-permission-log-pending candidate) nil
     (mevedel-session-telemetry-pending candidate) nil
     (mevedel-session-hook-context-pending candidate) nil
     (mevedel-session-execution-state candidate) nil
     (mevedel-session-current-segment candidate) segment
     (mevedel-session-updated-at candidate) (format-time-string "%FT%H-%M-%S")
     (mevedel-session-prompt-index candidate)
     (copy-tree
      (mevedel-session-persistence--reduce-prompt-index
       (mevedel-session-prompt-index session) segment target-turn t)
      t)
     (mevedel-session-file-snapshots candidate)
     (copy-tree
      (mevedel-session-persistence--reduce-file-snapshots
       (mevedel-session-file-snapshots session) target-turn t)
      t)
     (mevedel-session-invoked-skills candidate)
     (cl-remove-if
      (lambda (record)
        (> (or (mevedel-skill-invocation-record-turn record) 0) turn))
      (mevedel-session-invoked-skills session))
     (mevedel-session-permission-queue candidate) nil
     (mevedel-session-directive-planning candidate) nil
     (mevedel-session-pending-plan-approval candidate) nil
     (mevedel-session-plan-metadata candidate) nil
     (mevedel-session-goal candidate) nil)
    (mevedel-session-persistence--prune-agent-transcripts-after-fork
     candidate turn)
    candidate))

(defun mevedel-session-persistence--copy-session-state (from to)
  "Copy every cl-struct slot from session FROM into TO."
  (dotimes (index (length from))
    (aset to index (aref from index))))

(defun mevedel-session-persistence--copy-rewind-session-state (from to)
  "Copy Rewind state from FROM into TO without replacing durability runtime."
  (let ((pending (mevedel-session-pending-publication to))
        (publication (mevedel-session-publication to))
        (queue (mevedel-session-publication-queue to))
        (uncommitted
         (mevedel-session-publication-uncommitted-batches to))
        (active (mevedel-session-publication-active-p to))
        (lease (mevedel-session-lease to))
        (timer (mevedel-session-lease-renewal-timer to)))
    (mevedel-session-persistence--copy-session-state from to)
    (setf (mevedel-session-pending-publication to) pending
          (mevedel-session-publication to) publication
          (mevedel-session-publication-queue to) queue
          (mevedel-session-publication-uncommitted-batches to) uncommitted
          (mevedel-session-publication-active-p to) active
          (mevedel-session-lease to) lease
          (mevedel-session-lease-renewal-timer to) timer)))

(defun mevedel-session-persistence--materialize-publication
    (session publication staging-path)
  "Materialize SESSION's committed PUBLICATION below STAGING-PATH.

Only the publication's logical artifacts are copied.  Lease, publication,
recovery, and other control paths are never materialized."
  (unless publication
    (error "Portable project operation requires a committed session publication"))
  (dolist (entry (plist-get publication :artifacts))
    (let* ((logical (car entry))
           (destination (expand-file-name logical staging-path))
           (content
            (mevedel-session-persistence-read-artifact session logical t)))
      (unless (file-in-directory-p destination staging-path)
        (error "Session artifact escapes staging: %s" logical))
      (make-directory (file-name-directory destination) t)
      (let ((coding-system-for-write 'no-conversion))
        (write-region content nil destination nil 'silent)))))

(defun mevedel-session-persistence--prune-remote-rewind-staging
    (candidate target staging-path)
  "Remove post-target artifacts from CANDIDATE's STAGING-PATH snapshot.
TARGET identifies the retained segment and turn."
  (let ((target-segment (plist-get target :segment))
        (target-turn (1- (plist-get target :cum-turn)))
        (agents
         (delq nil
               (mapcar
                (lambda (entry) (plist-get (cdr entry) :path))
                (mevedel-session-agent-transcripts candidate)))))
    (dolist (path (directory-files-recursively staging-path ".*"))
      (let ((logical (file-relative-name path staging-path)))
        (when
            (or
             (and (string-match
                   "\\`segment-\\([0-9]+\\)\\.chat\\.org\\'" logical)
                  (> (string-to-number (match-string 1 logical))
                     target-segment))
             (and (string-match
                   "\\`instructions/turn-\\([0-9]+\\)\\.el\\'" logical)
                  (> (string-to-number (match-string 1 logical))
                     target-turn))
             (and (string-prefix-p "agents/" logical)
                  (not (member logical agents)))
             (string-prefix-p "plans/" logical)
             (or (member logical '(".lock" ".lease" ".publications"
                                  ".recovery"))
                 (string-prefix-p ".recovery/" logical)))
          (delete-file path))))
    (let ((plans (file-name-concat staging-path "plans")))
      (when (file-directory-p plans)
        (delete-directory plans t)))))

(defun mevedel-session-persistence--stage-rewind
    (session candidate target staging-path staging-buffer
             &optional publication rollback-staging-path)
  "Stage CANDIDATE and TARGET from SESSION under STAGING-PATH.

When PUBLICATION is non-nil, materialize only its immutable logical artifacts;
portable lease and publication control directories are never copied."
  (if publication
      (progn
        (mevedel-session-persistence--materialize-publication
         session publication staging-path)
        (when rollback-staging-path
          (copy-directory staging-path rollback-staging-path nil t t)))
    (copy-directory (mevedel-session-save-path session) staging-path nil t t))
  (let ((source session))
    (when publication
      (setq source (copy-sequence session))
      (setf (mevedel-session-save-path source) staging-path))
    (mevedel-session-persistence--load-rewind-target
     source staging-buffer target t))
  (with-current-buffer staging-buffer
    (setq buffer-file-name
          (mevedel-session-persistence--segment-path
           staging-path (plist-get target :segment)))
    (mevedel-session-persistence--stabilize-gptel-bounds)
    (mevedel-session-persistence--update-prompt-index
     candidate staging-buffer)
    (mevedel-session-persistence--write-current-buffer-atomically
     buffer-file-name)
    (set-buffer-modified-p nil))
  (cl-loop for segment from (1+ (plist-get target :segment))
           to (or (mevedel-session-current-segment session) 1)
           for path = (mevedel-session-persistence--segment-path
                       staging-path segment)
           when (file-exists-p path)
           do (delete-file path))
  (let* ((instructions-dir
          (mevedel-session-persistence--instructions-dir staging-path))
         (target-instructions
          (mevedel-session-persistence--instructions-turn-path
           staging-path (1- (plist-get target :cum-turn))))
         (current-instructions
          (mevedel-session-persistence--instructions-current-path
           staging-path)))
    (when (file-exists-p target-instructions)
      (copy-file target-instructions current-instructions t))
    (when (file-directory-p instructions-dir)
      (dolist (path (directory-files instructions-dir t
                                     "\\`turn-\\([0-9]+\\)\\.el\\'"))
        (let ((name (file-name-nondirectory path)))
          (when (and (string-match
                      "\\`turn-\\([0-9]+\\)\\.el\\'" name)
                     (> (string-to-number (match-string 1 name))
                        (1- (plist-get target :cum-turn))))
            (delete-file path))))))
  (when publication
    (mevedel-session-persistence--prune-remote-rewind-staging
     candidate target staging-path))
  (mevedel-session-persistence-write
   (mevedel-session-persistence--sidecar-path staging-path)
   (mevedel-session-persistence--build-sidecar candidate staging-buffer))
  (mevedel-session-persistence-deserialize
   (mevedel-session-persistence-read
    (mevedel-session-persistence--sidecar-path staging-path))
   (mevedel-session-workspace session)))

(defun mevedel-session-persistence--backup-restore-files (plan directory)
  "Copy current PLAN file state under DIRECTORY for transaction rollback."
  (cl-loop for entry in plan
           for path = (plist-get entry :path)
           for index from 1
           for backup = (file-name-concat directory (format "%06d" index))
           collect
           (if (file-exists-p path)
               (progn
                 (copy-file path backup t t t)
                 (list :path path :existed t :backup backup))
             (list :path path :existed nil))))

(defun mevedel-session-persistence--rollback-restore-files (backups)
  "Restore file BACKUPS captured for a failed Rewind.
Return descriptions of every artifact that could not be restored."
  (let (failures)
    (dolist (entry backups)
      (let ((path (plist-get entry :path)))
        (condition-case err
            (if (plist-get entry :existed)
                (progn
                  (make-directory (file-name-directory path) t)
                  (copy-file (plist-get entry :backup) path t t t))
              (when (file-exists-p path)
                (delete-file path)))
          (error
           (push (format "%s (%s)" path (error-message-string err))
                 failures)))))
    (nreverse failures)))

(defun mevedel-session-persistence--rewind-publication-artifacts
    (session buffer staging-path &optional state)
  "Return STAGING-PATH as SESSION replacement artifacts, sidecar last.

SESSION supplies the owned publication path.  BUFFER supplies live transcript
state.  STATE, when non-nil, supplies the logical sidecar state without
replacing SESSION's live lease runtime."
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (let* ((save-path (mevedel-session-save-path session))
         (sidecar-name "session.meta.el")
         artifacts)
    (dolist (path (sort (directory-files-recursively staging-path ".*")
                        #'string<))
      (let ((logical (file-relative-name path staging-path)))
        (when (and (not (equal logical sidecar-name))
                   (mevedel-session-publication-logical-path-p logical))
          (push
           (list :path (expand-file-name logical save-path)
                 :content (mevedel-file-history--read-file-raw path))
           artifacts))))
    (append
     (nreverse artifacts)
     (list
      (list
       :path (mevedel-session-persistence--sidecar-path save-path)
       :content
       (mevedel-session-persistence--printed-value
        (mevedel-session-persistence--build-sidecar
         (or state session) buffer))
       :commit-marker t
       :replace t)))))

(defun mevedel-session-persistence--install-rewind-buffer
    (buffer staging-buffer session target)
  "Install STAGING-BUFFER as BUFFER for rewound SESSION at TARGET."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (replace-buffer-contents staging-buffer)
      (setq buffer-file-name
            (mevedel-session-persistence--segment-path
             (mevedel-session-save-path session)
             (plist-get target :segment))
            buffer-file-truename
            (file-truename buffer-file-name))
      (set-buffer-modified-p nil)
      (set-visited-file-modtime)))
  (mevedel-session-persistence--notify-session-event
   session 'reset-agent-ephemeral-state)
  (mevedel-session-persistence--notify-session-event
   session 'rerender))

(defun mevedel-session-persistence--commit-remote-rewind
    (session buffer target plan)
  "Commit portable project SESSION, BUFFER, TARGET, and file PLAN through one
head CAS."
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (let* ((workspace (mevedel-session-workspace session))
         (directives (copy-sequence
                      (mevedel-workspace-directives workspace)))
         (save-path (mevedel-session-save-path session))
         (publication
          (or (mevedel-session-publication session)
              (mevedel-session-publication-read save-path)
              (error "Portable project Rewind requires a committed publication")))
         (head-before (plist-get publication :head))
         (temporary-root (make-temp-file "mevedel-remote-rewind-" t))
         (staging-path (file-name-concat temporary-root "staging"))
         (file-backup-dir (file-name-concat temporary-root "files"))
         (candidate
          (mevedel-session-persistence--rewind-candidate session target))
         (staging-buffer
          (generate-new-buffer " *mevedel-remote-rewind-staging*"))
         file-backups
         project-restore-started
         publish-attempted
         committed
         post-commit-error
         operation-error
         rollback-failures)
    (unwind-protect
        (condition-case err
            (progn
              (with-current-buffer staging-buffer
                (funcall (buffer-local-value 'major-mode buffer)))
              (make-directory file-backup-dir t)
              ;; Materialization, project backup, recheck, and restore are one
              ;; synchronous target operation.  Timer renewal stays off-target
              ;; until the wrapper proves the same generation still owns it.
              (mevedel-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (let ((current
                        (mevedel-session-publication-read
                         save-path)))
                   (unless (and current
                                (equal head-before
                                       (plist-get current :head)))
                     (user-error
                      "Session state changed before portable project Rewind; retry"))
                   (setq publication current)
                   (setf (mevedel-session-publication session) current))
                 (setq file-backups
                       (mevedel-session-persistence--backup-restore-files
                        plan file-backup-dir))
                 (mevedel-session-persistence--stage-rewind
                  session candidate target staging-path staging-buffer
                  publication)
                 (let ((rechecked
                        (mevedel-session-persistence-restore-plan
                         session (plist-get target :cum-turn) t)))
                   (unless
                       (equal
                        (sort (copy-sequence plan)
                              (lambda (a b)
                                (string< (plist-get a :path)
                                         (plist-get b :path))))
                        (sort (copy-sequence rechecked)
                              (lambda (a b)
                                (string< (plist-get a :path)
                                         (plist-get b :path)))))
                     (error
                      "Captured files changed after Rewind confirmation")))
                 (setq project-restore-started t)
                 (let ((result
                        (mevedel-session-persistence-execute-restore
                         session plan)))
                   (when (plist-get result :failed)
                     (error "File restore failed on %s: %s"
                            (plist-get result :failed)
                            (plist-get result :error))))))
              (setq publish-attempted t)
              (condition-case publish-error
                  (mevedel-session-publication-publish
                   session
                   (mevedel-session-persistence--rewind-publication-artifacts
                    session staging-buffer staging-path candidate))
                (error
                 ;; The captured head changes at marker CAS, before final lease
                 ;; normalization.  A changed head is already committed and
                 ;; must never be rolled back as though publication failed.
                 (if (not
                      (equal head-before
                             (plist-get (mevedel-session-publication session)
                                        :head)))
                     (setq committed t
                           post-commit-error publish-error)
                   (signal (car publish-error) (cdr publish-error)))))
              (unless committed
                (unless
                    (not
                     (equal head-before
                            (plist-get (mevedel-session-publication session)
                                       :head)))
                  (error "Portable project Rewind did not commit a publication head"))
                (setq committed t))
              ;; Only the successful head CAS installs logical session state.
              (mevedel-session-persistence--copy-rewind-session-state
               candidate session)
              (mevedel-session-persistence--install-rewind-buffer
               buffer staging-buffer session target)
              (condition-case instruction-error
                  (unless
                      (mevedel-session-persistence--load-instructions
                       session buffer nil directives t)
                    (error "Published instruction snapshot is unavailable"))
                (error
                 (display-warning
                  'mevedel
                  (format
                   "Portable project Rewind committed, but instructions did not refresh: %s"
                   (error-message-string instruction-error))
                  :warning)))
              (condition-case directive-error
                  (progn
                    (mevedel-workspace-rewind-directives
                     workspace (mevedel-session-session-id session)
                     (plist-get target :cum-turn))
                    (mevedel--restore-preserved-directives workspace))
                (error
                 (display-warning
                  'mevedel
                  (format
                   "Portable project Rewind committed, but directives did not refresh: %s"
                   (error-message-string directive-error))
                  :warning)))
              (condition-case refresh-error
                  (mevedel-session-persistence--refresh-restored-buffers
                   plan (list :succeeded (length plan)))
                (error
                 (display-warning
                  'mevedel
                  (format
                   "Portable project Rewind committed, but buffers did not refresh: %s"
                   (error-message-string refresh-error))
                  :warning))))
          (error (setq operation-error err)))
      (unless committed
        (when project-restore-started
          (condition-case rollback-error
              (setq rollback-failures
                    (mevedel-session-durability-call-with-reserved-lease
                     session
                     (lambda ()
                       (mevedel-session-persistence--rollback-restore-files
                        file-backups))))
            (error
             (push
              (format "Project rollback authority (%s)"
                      (error-message-string rollback-error))
              rollback-failures))))
        (when (and publish-attempted (null rollback-failures))
          (condition-case discard-error
              (mevedel-session-publication-discard-rolled-back
               session)
            (error
             (push
              (format "Publication recovery cleanup (%s)"
                      (error-message-string discard-error))
              rollback-failures)))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (unless rollback-failures
        (when (file-directory-p temporary-root)
          (delete-directory temporary-root t))))
    (when rollback-failures
      (let ((reason
             (format
              "Portable project Rewind rollback incomplete: %s"
              (string-join (nreverse rollback-failures) ", "))))
        (condition-case nil
            (mevedel-session-recovery-record-failure
             session reason temporary-root)
          (error nil))
        (error "%s; recovery data: %s" reason temporary-root)))
    (when operation-error
      (if committed
          (error "Portable project Rewind committed, but local state failed: %s"
                 (error-message-string operation-error))
        (signal (car operation-error) (cdr operation-error))))
    (when post-commit-error
      (error "Portable project Rewind committed, but lease finalization failed: %s"
             (error-message-string post-commit-error)))
    t))

(defun mevedel-session-persistence--commit-rewind
    (session buffer target plan)
  "Commit SESSION, BUFFER, TARGET, and file PLAN as one recoverable Rewind."
  (if (mevedel-session-persistence--portable-authority-p session)
      (mevedel-session-persistence--commit-remote-rewind
       session buffer target plan)
    (mevedel-session-persistence--commit-local-rewind
     session buffer target plan)))

(defun mevedel-session-persistence--commit-local-rewind
    (session buffer target plan)
  "Commit local SESSION, BUFFER, TARGET, and file PLAN as one Rewind."
  (let* ((workspace (mevedel-session-workspace session))
         (directives (copy-sequence
                      (mevedel-workspace-directives workspace)))
         (directive-state
          (mapcar
           (lambda (directive)
             (list directive
                   :anchor (copy-tree (mevedel-directive-anchor directive))
                   :state (mevedel-directive-state directive)
                   :subdirectives
                   (mevedel-directive-subdirectives directive)
                   :attempts (mevedel-directive-attempts directive)
                   :discussion (mevedel-directive-discussion directive)))
           directives))
         (save-path (mevedel-session-save-path session))
         (parent (file-name-directory (directory-file-name save-path)))
         (temporary-root
          (make-temp-file
           (expand-file-name ".mevedel-rewind-" parent) t))
         (staging-path (file-name-concat temporary-root "staging"))
         (rollback-path (file-name-concat temporary-root "rollback"))
         (file-backup-dir (file-name-concat temporary-root "files"))
         (original-state (copy-sequence session))
         (candidate
          (mevedel-session-persistence--rewind-candidate session target))
         (staging-buffer
          (generate-new-buffer " *mevedel-rewind-staging*"))
         (original-buffer
          (generate-new-buffer " *mevedel-rewind-original*"))
         (original-file-name (buffer-local-value 'buffer-file-name buffer))
         (original-file-truename
          (buffer-local-value 'buffer-file-truename buffer))
         (original-buffer-modified
          (with-current-buffer buffer (buffer-modified-p)))
         (original-point (with-current-buffer buffer (point)))
         (original-turn (mevedel-session-turn-count session))
         file-backups source-moved published session-installed
         file-restore-started buffer-install-started committed
         rollback-failures)
    (with-current-buffer original-buffer
      (insert-buffer-substring buffer))
    (unwind-protect
        (progn
          (with-current-buffer staging-buffer
            (funcall (buffer-local-value 'major-mode buffer)))
          (make-directory file-backup-dir t)
          (setq file-backups
                (mevedel-session-persistence--backup-restore-files
                 plan file-backup-dir))
          (mevedel-session-persistence--stage-rewind
           session candidate target staging-path staging-buffer)
          (let ((rechecked
                 (mevedel-session-persistence-restore-plan
                  session (plist-get target :cum-turn) t)))
            (unless (equal
                     (sort (copy-sequence plan)
                           (lambda (a b)
                             (string< (plist-get a :path)
                                      (plist-get b :path))))
                     (sort (copy-sequence rechecked)
                           (lambda (a b)
                             (string< (plist-get a :path)
                                      (plist-get b :path)))))
              (error "Captured files changed after Rewind confirmation")))
          (setq file-restore-started t)
          (let ((result
                 (mevedel-session-persistence-execute-restore session plan)))
            (when (plist-get result :failed)
              (error "File restore failed on %s: %s"
                     (plist-get result :failed)
                     (plist-get result :error))))
          (rename-file (directory-file-name save-path)
                       rollback-path)
          (setq source-moved t)
          (rename-file staging-path (directory-file-name save-path))
          (setq published t)
          (mevedel-session-persistence--copy-session-state candidate session)
          (setq session-installed t)
          (setq buffer-install-started t)
          (mevedel-session-persistence--install-rewind-buffer
           buffer staging-buffer session target)
          (unless
              (mevedel-session-persistence--load-instructions
               session buffer (1- (plist-get target :cum-turn)) directives t)
            (error "Instruction restore failed during Rewind"))
          (mevedel-workspace-rewind-directives
           workspace (mevedel-session-session-id session)
           (plist-get target :cum-turn))
          (mevedel-session-persistence--save-instructions
           session buffer t)
          (delete-directory rollback-path t)
          (setq source-moved nil
                committed t)
          (condition-case err
              (mevedel-session-persistence--refresh-restored-buffers
               plan (list :succeeded (length plan)))
            (error
             (display-warning
              'mevedel
              (format "Rewind committed, but buffers could not refresh: %s"
                      (error-message-string err)))))
          (condition-case err
              (progn
                (mevedel--restore-preserved-directives workspace)
                (mevedel-session-persistence--save-instructions
                 session buffer t))
            (error
             (display-warning
              'mevedel
              (format "Rewind committed, but directives could not refresh: %s"
                      (error-message-string err)))))
          t)
      (unless committed
        (dolist (entry directive-state)
          (let ((directive (car entry)))
            (setf (mevedel-directive-anchor directive)
                  (copy-tree (plist-get (cdr entry) :anchor))
                  (mevedel-directive-state directive)
                  (plist-get (cdr entry) :state)
                  (mevedel-directive-subdirectives directive)
                  (plist-get (cdr entry) :subdirectives)
                  (mevedel-directive-attempts directive)
                  (plist-get (cdr entry) :attempts)
                  (mevedel-directive-discussion directive)
                  (plist-get (cdr entry) :discussion))))
        (mevedel-workspace-set-directives workspace directives)
        (when session-installed
          (mevedel-session-persistence--copy-session-state
           original-state session))
        (when published
          (condition-case err
              (progn
                (when (file-directory-p save-path)
                  (delete-directory save-path t))
                (setq published nil))
            (error
             (push (format "%s (%s)" save-path (error-message-string err))
                   rollback-failures))))
        (when source-moved
          (condition-case err
              (rename-file rollback-path (directory-file-name save-path))
            (error
             (push (format "%s (%s)" save-path (error-message-string err))
                   rollback-failures))))
        (when file-restore-started
          (setq rollback-failures
                (nconc
                 rollback-failures
                 (mevedel-session-persistence--rollback-restore-files
                  file-backups))))
        (when buffer-install-started
          (condition-case err
              (progn
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (setq buffer-file-name original-file-name
                          buffer-file-truename original-file-truename)
                    (set-visited-file-modtime)
                    (replace-buffer-contents original-buffer)
                    (set-buffer-modified-p original-buffer-modified)
                    (goto-char (min original-point (point-max)))))
                (mevedel-session-persistence--load-instructions
                 session buffer original-turn directives t)
                (mevedel-session-persistence--notify-session-event
                 session 'rerender))
            (error
             (push (format "%s (%s)"
                           (buffer-name buffer)
                           (error-message-string err))
                   rollback-failures)))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil))
        (kill-buffer staging-buffer))
      (when (buffer-live-p original-buffer)
        (kill-buffer original-buffer))
      (if rollback-failures
          (let ((reason
                 (format
                  "Rewind rollback incomplete; inconsistent artifacts: %s"
                  (string-join (nreverse rollback-failures) ", "))))
            (when (mevedel-session-persistence--portable-authority-p session)
              (require 'mevedel-session-durability)
              (require 'mevedel-session-recovery)
              (mevedel-session-recovery-record-failure
               session reason temporary-root))
            (error "%s; recovery data: %s" reason temporary-root))
        (when (file-directory-p temporary-root)
          (delete-directory temporary-root t))))))

(defun mevedel-session-persistence-rewind (buffer target)
  "Rewind BUFFER's session in place to stable assistant TARGET."
  (let ((session (buffer-local-value 'mevedel--session buffer)))
    (unless (and session (plist-get target :fork-point-id))
      (user-error "Rewind requires a settled assistant response"))
    (mevedel-session-persistence-assert-mutation-authority session buffer)
    (setq target
          (mevedel-session-persistence--resolve-fork-target
           session target))
    (mevedel-session-persistence--assert-stable-source
     session buffer "rewinding")
    (let* ((turn (plist-get target :cum-turn))
           (plan (mevedel-session-persistence-restore-plan session turn t))
           (prepared
            (mevedel-session-persistence--prepare-buffers-for-restore
             session turn plan)))
      (unless (eq prepared :abort)
        (let ((impact
               (mevedel-session-persistence--rewind-impact
                session target prepared)))
          (if (mevedel-session-persistence--rewind-impact-empty-p impact)
              (message "Already at this state")
            (mevedel-session-persistence--render-rewind-impact session impact)
            (let ((confirmed
                   (yes-or-no-p
                    (format
                     "Rewind %s to S%d T%d (%d turns, %d files; no redo)? "
                     (mevedel-session-name session)
                     (plist-get target :segment)
                     (plist-get target :turn)
                     (plist-get impact :discarded-turns)
                     (length prepared)))))
              (when confirmed
                (mevedel-session-persistence-assert-mutation-authority
                 session buffer)
                (mevedel-session-persistence--commit-rewind
                 session buffer target prepared))
              (when-let* ((impact-buffer
                           (get-buffer "*mevedel-rewind-impact*")))
                (quit-windows-on impact-buffer t)
                (when (buffer-live-p impact-buffer)
                  (kill-buffer impact-buffer)))
              (when confirmed
                (with-current-buffer buffer
                  (mevedel--run-session-start-hooks "rewind"))
                (message "mevedel: rewound %s to S%d T%d"
                         (mevedel-session-name session)
                         (plist-get target :segment)
                         (plist-get target :turn))
                t))))))))

(defun mevedel-session-persistence-rewind-checkpoint
    (workspace checkpoint &optional buffer)
  "Rewind WORKSPACE to CHECKPOINT, resuming its session when needed.
BUFFER is the already-live execution session when available."
  (let ((session-id (plist-get checkpoint :session-id))
        (turn (plist-get checkpoint :turn)))
    (unless (and (stringp session-id) (natnump turn))
      (user-error "Malformed implementation checkpoint"))
    (unless buffer
      (let ((records (copy-sequence
                      (mevedel-workspace-directives workspace))))
        (mevedel--reset-instructions-preserving-directives workspace records)
        (unwind-protect
            (setq buffer
                  (mevedel-session-persistence-resume-id
                   workspace session-id))
          (mevedel--reset-instructions-preserving-directives workspace records)
          (mevedel--restore-preserved-directives workspace))))
    (unless buffer
      (user-error "Execution session is unavailable: %s" session-id))
    (let* ((session (buffer-local-value 'mevedel--session buffer))
           (target
            (and session
                 (cl-loop
                  for (_ . candidate) in
                  (mevedel-session-persistence--prompt-candidates session)
                  when (= turn (plist-get candidate :cum-turn))
                  return candidate))))
      (unless target
        (user-error "Implementation checkpoint is unavailable: turn %s" turn))
      (mevedel-session-persistence-rewind buffer target))))

;;;###autoload
(defun mevedel-rewind ()
  "Pick a settled assistant response and Rewind the current session to it."
  (interactive)
  (let* ((buffer
          (cond
           ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           ((and (boundp 'mevedel--session) mevedel--session)
            (current-buffer))
           (t (user-error "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session buffer)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (mevedel-session-persistence--assert-stable-source
     session buffer "rewinding")
    (let* ((candidates
            (mevedel-session-persistence--prompt-candidates session)))
      (unless candidates
        (user-error "Session has no recorded user prompts"))
      (let* ((lookup (make-hash-table :test #'equal)))
        (dolist (c candidates)
          (puthash (car c) (cdr c) lookup))
        (let* ((collection
                (mevedel-session-persistence--prompt-collection-fn
                 candidates lookup))
               (default (caar (last candidates)))
               (chosen  (completing-read
                         "Rewind to: " collection nil t
                         nil 'mevedel-session-persistence--prompt-history
                         default))
               (target (gethash chosen lookup)))
          (when target
            (mevedel-session-persistence-rewind buffer target)))))))


;;
;;; Fork-on-send and rename

(defun mevedel-session-persistence--agent-files-for-segments
    (prompt-index agent-transcripts picked-segment picked-cum-turn)
  "Return transcript entries whose `:parent-turn' is in copied ranges.

PROMPT-INDEX is the session sidecar's segment prompt index.
AGENT-TRANSCRIPTS is the sidecar's transcript entry alist.
PICKED-SEGMENT and PICKED-CUM-TURN describe the rewind target.  The
copied transcript set is derived from concrete segment ranges:
predecessor segments are copied whole; the picked segment is copied
only through PICKED-CUM-TURN.  Entries with non-integer
`:parent-turn' are excluded."
  (let ((ranges nil))
    (dolist (seg-entry prompt-index)
      (let ((seg (car seg-entry))
            (prompts (cdr seg-entry)))
        (when (and (integerp seg)
                   (or (< seg picked-segment)
                       (= seg picked-segment)))
          (let* ((turns (cl-loop for p in prompts
                                 for ct = (plist-get p :cum-turn)
                                 when (and (integerp ct)
                                           (or (< seg picked-segment)
                                               (null picked-cum-turn)
                                               (<= ct picked-cum-turn)))
                                 collect ct))
                 (lo (and turns (apply #'min turns)))
                 (hi (and turns (apply #'max turns))))
            (when (and lo hi)
              (push (cons lo hi) ranges))))))
    (setq ranges (nreverse ranges))
    (cl-loop for entry in agent-transcripts
             for parent-turn = (plist-get (cdr entry) :parent-turn)
             when (and (integerp parent-turn)
                       (cl-some (lambda (range)
                                  (and (<= (car range) parent-turn)
                                       (<= parent-turn (cdr range))))
                                ranges))
             collect entry)))

(defconst mevedel-session-persistence--clone-slot-names
  '(name workspace execution-target authority-mode working-directory
    tasks task-status-notes last-task-write-turn touched-files
    permission-rules resource-grants permission-mode sandbox-mode plan-mode
    directive-planning preset-name preset-settings model-provider
    reasoning-effort turn-count reminders last-observed-date
    agent-types-snapshot skills-snapshot pending-reminders
    specialist-nudge-state deferred-set deferred-pending deferred-injected
    deferred-used deferred-expired messages agent-registry agent-reservations
    agent-root-activity agent-root-waiter agent-turn-capacity pending-steering
    pending-follow-ups pending-input-next-id pending-input-paused
    pending-input-failure-paused dropped-file-grants active-dropped-file-grants
    mentions-shown workspace-instruction-hashes skills hook-rules hook-log
    hook-log-pending repair-log
    repair-log-pending permission-log-pending telemetry-pending
    hook-context-pending execution-state audit-session pending-publication
    publication publication-queue publication-uncommitted-batches
    publication-active-p control-transfer control-transfer-drains root-buffer
    lease
    lease-renewal-timer save-path session-id created-at updated-at
    current-segment forked-from-session-id forked-from-turn fork-type
    forked-from-fork-point-id worktree-source-root worktree-directory
    worktree-branch worktree-base-commit prompt-index file-snapshots
    persisted-first-user-message durable-tree-ensured
    agent-transcripts invoked-skills permission-queue pending-plan-approval
    plan-metadata goal)
  "Every `mevedel-session' slot decided by the logical clone constructor.")

(defun mevedel-session-persistence--assert-clone-slot-completeness ()
  "Signal when the session clone policy no longer covers the struct."
  (let ((actual
         (mapcar #'car
                 (cdr (cl-struct-slot-info 'mevedel-session))))
        (expected mevedel-session-persistence--clone-slot-names))
    (unless (equal actual expected)
      (error "Session clone policy is incomplete: missing %S, extra %S"
             (cl-set-difference actual expected)
             (cl-set-difference expected actual)))
    t))

(cl-defun mevedel-session-persistence--clone-session
    (session policy &key name save-path session-id created-at updated-at
             current-segment forked-from-session-id forked-from-turn)
  "Build an explicit independent logical clone of SESSION for POLICY.

POLICY is `fork' or `save-as'.  Workspace and execution-target are shared
because they are immutable authority identities owned outside the session.
Every other slot is decided here: durable logical containers are copied,
fork-only projections are reduced, and runtime/control state starts empty.
The identity and timestamp keywords describe the new materialized child."
  (mevedel-session-persistence--assert-clone-slot-completeness)
  (unless (memq policy '(fork save-as))
    (error "Unknown session clone policy: %S" policy))
  (require 'mevedel-reminders)
  (let* ((fork-p (eq policy 'fork))
         (turn (if fork-p
                   forked-from-turn
                 (mevedel-session-turn-count session)))
         (child
          (mevedel-session--create
           :name (if (eq policy 'save-as)
                     name
                   (mevedel-session-name session))
           :workspace (mevedel-session-workspace session)
           :execution-target (mevedel-session-execution-target session)
           :authority-mode (mevedel-session-authority-mode session)
           :working-directory (mevedel-session-working-directory session)
           :tasks (unless fork-p
                    (copy-tree (mevedel-session-tasks session) t))
           :task-status-notes (unless fork-p
                                (copy-tree
                                 (mevedel-session-task-status-notes session)
                                 t))
           :last-task-write-turn (unless fork-p
                                   (mevedel-session-last-task-write-turn
                                    session))
           :touched-files (make-hash-table :test #'equal)
           :permission-rules
           (copy-tree (mevedel-session-permission-rules session) t)
           :resource-grants
           (copy-tree (mevedel-session-resource-grants session) t)
           :permission-mode (mevedel-session-permission-mode session)
           :sandbox-mode (mevedel-session-sandbox-mode session)
           :plan-mode (mevedel-session-plan-mode session)
           :directive-planning nil
           :preset-name (mevedel-session-preset-name session)
           :preset-settings
           (copy-tree (mevedel-session-preset-settings session) t)
           :model-provider (mevedel-session-model-provider session)
           :reasoning-effort (mevedel-session-reasoning-effort session)
           :turn-count turn
           :reminders
           (mevedel-reminders-clone-list
            (mevedel-session-reminders session))
           :last-observed-date (mevedel-session-last-observed-date session)
           :agent-types-snapshot
           (copy-tree (mevedel-session-agent-types-snapshot session) t)
           :skills-snapshot
           (copy-tree (mevedel-session-skills-snapshot session) t)
           :pending-reminders nil
           :specialist-nudge-state nil
           :deferred-set
           (copy-tree (mevedel-session-deferred-set session) t)
           :deferred-pending nil
           :deferred-injected nil
           :deferred-used nil
           :deferred-expired nil
           :messages (unless fork-p
                       (copy-tree (mevedel-session-messages session) t))
           :agent-registry (unless fork-p
                             (copy-tree
                              (mevedel-session-agent-registry session) t))
           :agent-reservations nil
           :agent-root-activity 'idle
           :agent-root-waiter nil
           :agent-turn-capacity
           (mevedel-session-agent-turn-capacity session)
           :pending-steering nil
           :pending-follow-ups nil
           :pending-input-next-id nil
           :pending-input-paused nil
           :pending-input-failure-paused nil
           :dropped-file-grants nil
           :active-dropped-file-grants nil
           :mentions-shown (make-hash-table :test #'equal)
           :workspace-instruction-hashes nil
           :skills (copy-tree (mevedel-session-skills session) t)
           :hook-rules (and fork-p
                            (copy-tree
                             (mevedel-session-hook-rules session) t))
           :hook-log nil
           :hook-log-pending nil
           :repair-log nil
           :repair-log-pending nil
           :permission-log-pending nil
           :telemetry-pending nil
           :hook-context-pending nil
           :execution-state nil
           :audit-session nil
           :pending-publication nil
           :publication nil
           :publication-queue nil
           :publication-uncommitted-batches nil
           :publication-active-p nil
           :control-transfer nil
           :control-transfer-drains nil
           :root-buffer nil
           :lease nil
           :lease-renewal-timer nil
           :save-path save-path
           :session-id session-id
           :created-at (or created-at
                           (mevedel-session-created-at session))
           :updated-at (or updated-at
                           (mevedel-session-updated-at session))
           :current-segment
           (if fork-p
               current-segment
             (mevedel-session-current-segment session))
           :forked-from-session-id forked-from-session-id
           :forked-from-turn turn
           :fork-type (unless fork-p
                        (mevedel-session-fork-type session))
           :forked-from-fork-point-id
           (unless fork-p
             (mevedel-session-forked-from-fork-point-id session))
           :worktree-source-root
           (unless fork-p
             (mevedel-session-worktree-source-root session))
           :worktree-directory
           (unless fork-p
             (mevedel-session-worktree-directory session))
           :worktree-branch
           (unless fork-p
             (mevedel-session-worktree-branch session))
           :worktree-base-commit
           (unless fork-p
             (mevedel-session-worktree-base-commit session))
           :prompt-index
           (copy-tree
            (if fork-p
                (mevedel-session-persistence--reduce-prompt-index
                 (mevedel-session-prompt-index session)
                 current-segment turn)
              (mevedel-session-prompt-index session))
            t)
           :file-snapshots
           (copy-tree
            (if fork-p
                (mevedel-session-persistence--reduce-file-snapshots
                 (mevedel-session-file-snapshots session) turn)
              (mevedel-session-file-snapshots session))
            t)
           :agent-transcripts
           (copy-tree (mevedel-session-agent-transcripts session) t)
           :invoked-skills
           (copy-tree
            (if fork-p
                (cl-remove-if
                 (lambda (record)
                   (> (or (mevedel-skill-invocation-record-turn record) 0)
                      (or turn 0)))
                 (mevedel-session-invoked-skills session))
              (mevedel-session-invoked-skills session))
            t)
           :permission-queue nil
           :pending-plan-approval nil
           :plan-metadata (unless fork-p
                            (copy-tree
                             (mevedel-session-plan-metadata session) t))
           :goal (unless fork-p
                   (copy-tree (mevedel-session-goal session) t)))))
    (when (and fork-p turn)
      (mevedel-session-persistence--prune-agent-transcripts-after-fork
       child turn))
    child))

(defun mevedel-session-persistence--materialize-fork-artifact
    (source logical staging-path &optional required)
  "Copy SOURCE's committed LOGICAL artifact below STAGING-PATH.

Return non-nil when the artifact was materialized.  When REQUIRED is non-nil,
signal if LOGICAL is absent.  Literal resolver bytes are used so remote fixed
caches never become fork authority."
  (if (mevedel-session-persistence-artifact-present-p source logical t)
      (let ((destination (expand-file-name logical staging-path))
            (content
             (mevedel-session-persistence-read-artifact source logical t)))
        (make-directory (file-name-directory destination) t)
        (let ((coding-system-for-write 'no-conversion))
          (write-region content nil destination nil 'silent))
        t)
    (when required
      (error "Required fork artifact is not published: %s" logical))))

(defun mevedel-session-persistence--stage-fork
    (child buffer staging-buffer parent-save-path staging-path
           picked-segment picked-cum-turn &optional additional-roots)
  "Materialize CHILD under STAGING-PATH using STAGING-BUFFER."
  (require 'mevedel-agent-persistence)
  (require 'mevedel-plan)
  (let ((source (buffer-local-value 'mevedel--session buffer)))
    (unless source
      (error "Fork source buffer has no session"))
    (make-directory (file-name-concat staging-path "agents") t)
    (make-directory (file-name-concat staging-path "file-history") t)
    (when-let ((local-source
                (and parent-save-path
                     (file-name-concat parent-save-path "local")))
               ((file-directory-p local-source)))
      (copy-directory local-source
                      (file-name-concat staging-path "local")
                      nil t t)
      ;; `local/plans' is managed plan state, not ordinary local content.
      ;; Re-add only the accepted artifact proven valid at the fork point
      ;; below.
      (let ((plans (file-name-concat staging-path "local" "plans")))
        (when (file-directory-p plans)
          (delete-directory plans t))))
    (when-let* ((source source)
                (metadata (mevedel-session-plan-metadata source))
                ((eq (plist-get metadata :status) 'accepted))
                (accepted-turn (plist-get metadata :accepted-turn))
                ((and (integerp accepted-turn)
                      (<= accepted-turn picked-cum-turn)))
                (relative-path (plist-get metadata :accepted-path))
                ((stringp relative-path))
                ;; A normalized session-relative path is the whole
                ;; containment rule: it cannot escape the session store or
                ;; name another target.
                ((mevedel-plan-artifact-path-p relative-path)))
      ;; A fork must not inherit an artifact that no longer matches the hash
      ;; the source accepted.  Only committed bytes are authority here; an
      ;; absent artifact is reported by the materialization below.
      (when (mevedel-session-persistence-artifact-present-p
             source relative-path t)
        (let ((body
               (mevedel--normalize-message-text
                (decode-coding-string
                 (mevedel-session-persistence-read-artifact
                  source relative-path t)
                 'utf-8-unix))))
          (unless (equal (plist-get metadata :accepted-hash)
                         (mevedel-plan-hash body))
            (error "Accepted plan artifact hash does not match"))))
      (mevedel-session-persistence--materialize-fork-artifact
       source relative-path staging-path t))
    (cl-loop for i from 1 below picked-segment do
             (mevedel-session-persistence--materialize-fork-artifact
              source
              (file-name-nondirectory
               (mevedel-session-persistence--segment-path parent-save-path i))
              staging-path))
    (with-current-buffer staging-buffer
      (setq-local mevedel--session child)
      (setq-local mevedel-workspace-additional-roots
                  (copy-tree additional-roots t))
      (setq buffer-file-name
            (mevedel-session-persistence--segment-path
             staging-path picked-segment)
            buffer-file-truename nil)
      (require 'mevedel-pipeline)
      (mevedel-pipeline-reconcile-lost-executions staging-buffer)
      (set-buffer-modified-p t)
      (save-buffer))
    (mevedel-session-persistence--reconcile-lost-execution-segments
     child (mevedel-session-persistence--segment-path
            staging-path picked-segment))
    (when picked-cum-turn
      (dolist (entry
               (mevedel-session-persistence--state-at-turn
                child picked-cum-turn))
        (when-let* ((backup-name (plist-get (cdr entry) :backup-name)))
          (mevedel-session-persistence--materialize-fork-artifact
           source (file-name-concat "file-history" backup-name)
           staging-path))))
    (when (and picked-cum-turn parent-save-path)
      (dolist (entry
               (mevedel-session-persistence--agent-files-for-segments
                (mevedel-session-prompt-index child)
                (mevedel-session-agent-transcripts child)
                picked-segment picked-cum-turn))
        (when-let* ((rel-path (plist-get (cdr entry) :path))
                    ((mevedel-agent-persistence-transcript-path-p
                      rel-path parent-save-path))
                    ((mevedel-agent-persistence-transcript-path-p
                      rel-path staging-path)))
          (mevedel-session-persistence--materialize-fork-artifact
           source rel-path staging-path))))
    (mevedel-session-persistence-write
     (mevedel-session-persistence--sidecar-path staging-path)
     (mevedel-session-persistence--build-sidecar child staging-buffer))
    (mevedel-session-persistence--save-instructions child buffer)
    (let* ((saved
            (mevedel-session-persistence-read
             (mevedel-session-persistence--sidecar-path staging-path)))
           (restored
            (mevedel-session-persistence-deserialize
             saved (mevedel-session-workspace child))))
      (unless (equal (mevedel-session-session-id
                      (plist-get restored :session))
                     (mevedel-session-session-id child))
        (error "Fork staging validation failed")))))

(defun mevedel-session-persistence--publish-fork
    (child buffer staging-buffer parent-save-path staging-path new-save-path
           picked-segment picked-cum-turn additional-roots)
  "Stage, publish, and restore CHILD as one session-artifact transaction."
  (let ((portable-p
         (mevedel-session-persistence--portable-authority-p child))
        child-buffer published committed)
    (unwind-protect
        (progn
          (unless (mevedel-session-persistence-lock-acquire
                   staging-path (buffer-name buffer) child)
            (error "Could not acquire fork session lock"))
          (mevedel-session-persistence--stage-fork
           child buffer staging-buffer parent-save-path staging-path
           picked-segment picked-cum-turn additional-roots)
          (if portable-p
              (progn
                (require 'mevedel-session-durability)
                (require 'mevedel-session-publication)
                ;; A fork starts a new publication history.  Publish its
                ;; complete staged snapshot before the owned lease and
                ;; immutable control state move together into discoverability.
                (mevedel-session-publication-publish
                 child
                 (mevedel-session-persistence--rewind-publication-artifacts
                  child staging-buffer staging-path))
                (mevedel-session-durability-call-with-reserved-lease
                 child
                 (lambda ()
                   (rename-file (directory-file-name staging-path)
                                (directory-file-name new-save-path))
                   (setq published t)
                   ;; Reservation finalization must address the moved lease.
                   (setf (mevedel-session-save-path child) new-save-path)))
                (setf
                 (mevedel-session-publication child)
                 (mevedel-session-publication-read new-save-path)))
            (mevedel-session-persistence-lock-release staging-path child)
            (rename-file (directory-file-name staging-path)
                         (directory-file-name new-save-path))
            (setf (mevedel-session-save-path child) new-save-path))
          (setq published t)
          (with-current-buffer staging-buffer
            (set-buffer-modified-p nil)
            (set-visited-file-name nil t)
            (setq-local kill-buffer-hook nil))
          (kill-buffer staging-buffer)
          (setq child-buffer
                (mevedel-session-persistence-restore
                 new-save-path "fork" child)
                committed t)
          child-buffer)
      (unless committed
        (when-let* ((failed-buffer
                     (or child-buffer
                         (mevedel-session-persistence--find-live-buffer
                          (mevedel-session-session-id child)
                          (mevedel-session-buffer-name
                           (mevedel-session-name child)
                           (mevedel-session-workspace child))))))
          (with-current-buffer failed-buffer
            (set-buffer-modified-p nil)
            (setq-local kill-buffer-hook nil
                        kill-buffer-query-functions nil))
          (when (buffer-live-p failed-buffer)
            (kill-buffer failed-buffer)))
        (when published
          (ignore-errors (delete-directory new-save-path t)))
        (when (file-directory-p staging-path)
          (ignore-errors (delete-directory staging-path t)))
        (let ((remaining
               (cond
                ((and published
                      (ignore-errors (file-directory-p new-save-path)))
                 new-save-path)
                ((ignore-errors (file-directory-p staging-path))
                 staging-path))))
          (if remaining
              (ignore-errors
                (mevedel-session-persistence-lock-release remaining child))
            (when portable-p
              (ignore-errors
                (mevedel-session-durability-forget-removed-session child))))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil)
          (setq-local kill-buffer-hook nil))
        (kill-buffer staging-buffer)))))

(defun mevedel-session-persistence--fork-child-name (session fork-type)
  "Return the first unused direct-child name for SESSION and FORK-TYPE."
  (let* ((source-id (mevedel-session-session-id session))
         (source-name (mevedel-session-name session))
         (type-name (symbol-name fork-type))
         (regexp
          (format "\\`%s \u00b7 %s \\([0-9]+\\)\\'"
                  (regexp-quote source-name)
                  (regexp-quote type-name)))
         used)
    (dolist (entry
             (mevedel-session-persistence-list-sessions
              (mevedel-session-workspace session)))
      (let ((summary (plist-get entry :summary)))
        (when (and (equal source-id
                          (plist-get summary :forked-from-session-id))
                   (eq fork-type (plist-get summary :fork-type))
                   (string-match regexp
                                 (or (plist-get summary :session-name) "")))
          (push (string-to-number (match-string 1
                                                (plist-get summary
                                                           :session-name)))
                used))))
    (let ((number 1))
      (while (memq number used)
        (cl-incf number))
      (format "%s · %s %d" source-name type-name number))))

(defun mevedel-session-persistence--conversation-fork-disclosure
    (session)
  "Return durable disclosure text for conversation fork SESSION."
  (format
   (concat "\n\n<system-reminder>\n"
           "Conversation Fork\n"
           "Source session: %s\n"
           "Working directory: %s\n"
           "Files were not restored.  This fork uses the current files in "
           "the Source working directory, so they may be newer than the "
           "selected conversation point and changes are shared.\n"
           "</system-reminder>\n")
   (mevedel-session-forked-from-session-id session)
   (mevedel-session-working-directory session)))

(defun mevedel-session-persistence--retarget-worktree-path (session path)
  "Retarget absolute PATH from SESSION's Source root into its worktree."
  (unless (and (stringp path) (file-name-absolute-p path))
    (error "Invalid Worktree Fork path: %S" path))
  (let* ((source-root
          (file-name-as-directory
           (expand-file-name
            (or (mevedel-session-worktree-source-root session)
                (error "Worktree Fork has no Source repository root")))))
         (worktree-root
          (file-name-as-directory
           (expand-file-name
            (or (mevedel-session-worktree-directory session)
                (error "Worktree Fork has no worktree directory")))))
         (expanded (expand-file-name path)))
    (if (string-prefix-p source-root expanded)
        (expand-file-name (substring expanded (length source-root))
                          worktree-root)
      expanded)))

(defun mevedel-session-persistence--retarget-worktree-roots
    (session roots)
  "Retarget checkout-local paths in session-local ROOTS for SESSION.
Return `(:roots ALIST :dropped LIST)'."
  (let (retargeted dropped)
    (dolist (entry roots)
      (if (and (consp entry)
               (stringp (car entry))
               (file-name-absolute-p (car entry))
               (proper-list-p (cdr entry)))
          (let (paths)
            (dolist (path (cdr entry))
              (if (and (stringp path) (file-name-absolute-p path))
                  (push
                   (mevedel-session-persistence--retarget-worktree-path
                    session path)
                   paths)
                (push (format "additional root %S" path) dropped)))
            (push (cons (car entry) (nreverse paths)) retargeted))
        (push (format "additional roots entry %S" entry) dropped)))
    (list :roots (nreverse retargeted)
          :dropped (nreverse dropped))))

(defun mevedel-session-persistence--retarget-worktree-state (session)
  "Retarget SESSION's copied repository-local path state.
Return descriptions of malformed grants and rules dropped from the child."
  (let (grants rules dropped)
    (dolist (grant (mevedel-session-resource-grants session))
      (if (and (proper-list-p grant)
               (stringp (plist-get grant :path))
               (file-name-absolute-p (plist-get grant :path)))
          (let ((copy (copy-tree grant t)))
            (plist-put
             copy :path
             (mevedel-session-persistence--retarget-worktree-path
              session (plist-get copy :path)))
            (push copy grants))
        (push (format "resource grant %S" grant) dropped)))
    (dolist (rule (mevedel-session-permission-rules session))
      (cond
       ((not (and (consp rule) (proper-list-p (cdr rule))))
        (push (format "permission rule %S" rule) dropped))
       ((not (plist-member (cdr rule) :path))
        (push (copy-tree rule t) rules))
       ((and (stringp (plist-get (cdr rule) :path))
             (file-name-absolute-p (plist-get (cdr rule) :path)))
        (let ((copy (copy-tree rule t)))
          (plist-put
           (cdr copy) :path
           (mevedel-session-persistence--retarget-worktree-path
            session (plist-get (cdr copy) :path)))
          (push copy rules)))
       (t
        (push (format "permission rule %S" rule) dropped))))
  (setf
   (mevedel-session-working-directory session)
   (mevedel-session-worktree-directory session)
   (mevedel-session-file-snapshots session)
   (mapcar
    (lambda (turn-entry)
      (cons
       (car turn-entry)
       (mapcar
        (lambda (file-entry)
          (cons
           (mevedel-session-persistence--retarget-worktree-path
            session (car file-entry))
           (copy-tree (cdr file-entry) t)))
        (cdr turn-entry))))
    (mevedel-session-file-snapshots session))
   (mevedel-session-resource-grants session)
   (nreverse grants)
   (mevedel-session-permission-rules session)
   (nreverse rules))
    (nreverse dropped)))

(defun mevedel-session-persistence--assert-worktree-target
    (worktree-root target)
  "Signal when TARGET could escape WORKTREE-ROOT through a symlink."
  (let* ((root (file-name-as-directory (file-truename worktree-root)))
         (cursor (if (file-exists-p target)
                     target
                   (file-name-directory target))))
    (while (and cursor (not (file-exists-p cursor)))
      (setq cursor
            (file-name-directory
             (directory-file-name cursor))))
    (unless (and cursor
                 (let ((resolved (file-truename cursor)))
                   (or (file-equal-p resolved root)
                       (file-in-directory-p resolved root))))
      (error "Unsafe Worktree Fork target: %s" target))))

(defun mevedel-session-persistence--restore-worktree-files
    (source child cum-turn)
  "Restore SOURCE's captured repository files into CHILD at CUM-TURN."
  (let* ((source-root
          (file-name-as-directory
           (expand-file-name
            (mevedel-session-worktree-source-root child))))
         (worktree-root
          (file-name-as-directory
           (expand-file-name
            (mevedel-session-worktree-directory child))))
         (history-dir
          (file-name-concat
           (mevedel-session-save-path source) "file-history"))
         (restored 0)
         plan
         unrestored
         external)
    (unless (file-readable-p history-dir)
      (error "Captured file-history store is unreadable: %s" history-dir))
    ;; Validate the complete plan before the first child-worktree write.
    (dolist (entry
             (mevedel-session-persistence--state-at-turn source cum-turn))
      (let ((path (car entry))
            (snapshot (cdr entry)))
        (unless (and (stringp path) (file-name-absolute-p path))
          (error "Invalid captured Worktree Fork path: %S" path))
        (if (not
             (string-prefix-p source-root (expand-file-name path)))
            (push (expand-file-name path) external)
          (let* ((target
                  (mevedel-session-persistence--retarget-worktree-path
                   child path))
                 (backup-name (plist-get snapshot :backup-name)))
            (mevedel-session-persistence--assert-worktree-target
             worktree-root target)
            (unless
                (or (null backup-name)
                    (and (stringp backup-name)
                         (equal backup-name
                                (file-name-nondirectory backup-name))))
              (error "Invalid captured backup name: %S" backup-name))
            (push (list :path target :backup-name backup-name) plan)))))
    (dolist (item (nreverse plan))
      (let ((target (plist-get item :path))
            (backup-name (plist-get item :backup-name)))
        (condition-case err
            (progn
              (if (null backup-name)
                  (when (file-exists-p target)
                    (delete-file target))
                (let ((backup
                       (mevedel-file-history--backup-path
                        (mevedel-session-save-path source) backup-name)))
                  (unless (file-readable-p backup)
                    (error "Captured backup is unavailable: %s" backup))
                  (make-directory (file-name-directory target) t)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert
                     (mevedel-file-history--read-file-raw backup))
                    (mevedel-session-persistence--write-current-buffer-atomically
                     target))))
              (cl-incf restored))
          (error
           (push (list :path target
                       :reason (error-message-string err))
                 unrestored)))))
    (list :restored restored
          :unrestored (nreverse unrestored)
          :external (nreverse external))))

(defun mevedel-session-persistence--worktree-fork-disclosure
    (session report)
  "Return durable Worktree Fork disclosure for SESSION and REPORT."
  (let* ((unrestored (plist-get report :unrestored))
         (dropped (plist-get report :dropped))
         (partial (or unrestored dropped)))
    (format
     (concat "\n\n<system-reminder>\n"
           "%s\n"
           "Source session: %s\n"
           "Worktree directory: %s\n"
           "Branch: %s\n"
           "Base commit: %s\n"
           "Captured repository files restored: %d\n"
           "%s"
           "Uncaptured files retain the base commit's contents. "
           "Uncommitted Source changes were not copied.\n"
           "%s"
           "%s"
           "%s"
           "</system-reminder>\n")
     (if partial "Worktree Fork (partial restoration)" "Worktree Fork")
     (mevedel-session-forked-from-session-id session)
     (mevedel-session-persistence--target-native-report-path
      session (mevedel-session-worktree-directory session))
     (mevedel-session-worktree-branch session)
     (mevedel-session-worktree-base-commit session)
     (plist-get report :restored)
     (if partial
         (concat
          "WARNING: Restoration was partial; this is not an exact "
          "historical checkout.\n")
       "")
     (if-let* ((external (plist-get report :external)))
         (format
          "External captured paths remain shared and non-isolated: %s\n"
          (string-join
           (mapcar
            (lambda (path)
              (mevedel-session-persistence--target-native-report-path
               session path))
            external)
           ", "))
       "")
     (if unrestored
         (format
          "Unrestored captured files:\n%s\n"
          (mapconcat
           (lambda (item)
             (format "- %s: %s"
                     (mevedel-session-persistence--target-native-report-path
                      session (plist-get item :path))
                     (plist-get item :reason)))
           unrestored "\n"))
       "")
     (if dropped
         (format
          "Dropped malformed copied path state:\n%s\n"
          (mapconcat (lambda (item) (format "- %s" item))
                     dropped "\n"))
        ""))))

(defun mevedel-session-persistence--target-native-report-path (session path)
  "Return PATH without SESSION's client-specific remote prefix."
  (if-let* ((target (mevedel-session-execution-target session))
            ((stringp path)))
      (condition-case _
          (mevedel-execution-target-native-path target path)
        (mevedel-execution-target-error "<path outside session target>"))
    path))

(defun mevedel-session-persistence--worktree-fork-retained-error
    (session failure reservation)
  "Return the retained-artifact error for SESSION, FAILURE, and RESERVATION."
  (format
   "%s; Worktree Fork artifacts retained: branch %s, directory %s. Cleanup: %s"
   (error-message-string failure)
   (plist-get reservation :branch)
   (mevedel-session-persistence--target-native-report-path
    session (plist-get reservation :directory))
   (plist-get reservation :cleanup-command)))

(defun mevedel-session-persistence-conversation-fork (buffer target)
  "Create and open a Conversation Fork of BUFFER at stable TARGET.

The child receives truncated conversation history and the Source working
directory.  Working files are neither restored nor copied.  Return the new
child data buffer without mutating the Source buffer, session, or lock."
  (let* ((session (buffer-local-value 'mevedel--session buffer))
         (_ (unless session
              (user-error "Active buffer has no mevedel session")))
         (_ (mevedel-session-persistence-assert-mutation-authority
             session buffer))
         (target
          (mevedel-session-persistence--resolve-fork-target session target))
         (_ (mevedel-session-persistence--assert-stable-source
             session buffer "forking"))
         (parent-save-path (mevedel-session-save-path session))
         (_ (unless parent-save-path
              (user-error "Save the session before forking")))
         (picked-segment (plist-get target :segment))
         (picked-cum-turn (plist-get target :cum-turn))
         (sessions-dir
          (mevedel-session-persistence--sessions-dir
           (mevedel-session-workspace session)))
         (child-name
          (mevedel-session-persistence--fork-child-name
           session 'conversation))
         (new-id
          (mevedel-session-persistence--allocate-session-id
           child-name sessions-dir))
         (new-save-path
          (file-name-as-directory (file-name-concat sessions-dir new-id)))
         (staging-path
          (file-name-as-directory
           (make-temp-file
            (expand-file-name ".mevedel-fork-" sessions-dir) t)))
         (staging-buffer
          (generate-new-buffer " *mevedel-conversation-fork*"))
         (additional-roots
          (buffer-local-value 'mevedel-workspace-additional-roots buffer))
         child)
    (unwind-protect
        (progn
          (let ((now (format-time-string "%FT%H-%M-%S")))
            (setq child
                  (mevedel-session-persistence--clone-session
                   session 'fork
                   :save-path staging-path
                   :session-id new-id
                   :created-at now
                   :updated-at now
                   :current-segment picked-segment
                   :forked-from-session-id
                   (mevedel-session-session-id session)
                   :forked-from-turn picked-cum-turn)))
          (setf (mevedel-session-name child) child-name
                (mevedel-session-fork-type child) 'conversation
                (mevedel-session-forked-from-fork-point-id child)
                (plist-get target :fork-point-id))
          (with-current-buffer staging-buffer
            (let ((org-agenda-file-menu-enabled nil))
              (org-mode)))
          (mevedel-session-persistence--load-rewind-target
           session staging-buffer target)
          (with-current-buffer staging-buffer
            (goto-char (point-max))
            (let ((start (point)))
              (insert
               (mevedel-session-persistence--conversation-fork-disclosure
                child))
              (set-text-properties start (point) nil)))
          (mevedel-session-persistence--publish-fork
           child buffer staging-buffer parent-save-path staging-path
           new-save-path picked-segment picked-cum-turn additional-roots))
      (when (file-directory-p staging-path)
        (ignore-errors (delete-directory staging-path t)))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil)
          (setq-local kill-buffer-hook nil))
        (kill-buffer staging-buffer)))))

(defun mevedel-session-persistence-worktree-fork (buffer target)
  "Create and open a Worktree Fork of BUFFER at stable TARGET."
  (let* ((session (buffer-local-value 'mevedel--session buffer))
         (_ (unless session
              (user-error "Active buffer has no mevedel session")))
         (_ (mevedel-session-persistence-assert-mutation-authority
             session buffer))
         (target
          (mevedel-session-persistence--resolve-fork-target session target))
         (_ (mevedel-session-persistence--assert-stable-source
             session buffer "forking"))
         (parent-save-path (mevedel-session-save-path session))
         (_ (unless parent-save-path
              (user-error "Save the session before forking")))
         (picked-segment (plist-get target :segment))
         (picked-cum-turn (plist-get target :cum-turn))
         (sessions-dir
          (mevedel-session-persistence--sessions-dir
           (mevedel-session-workspace session)))
         (reservation
          (progn
            (require 'mevedel-worktree)
            (or (plist-get target :worktree-reservation)
                (mevedel-worktree-fork-reservation session))))
         (_ (mevedel-worktree-fork-validate-reservation
             session reservation))
         (child-name
          (mevedel-session-persistence--fork-child-name session 'worktree))
         (new-id
          (mevedel-session-persistence--allocate-session-id
           child-name sessions-dir))
         (new-save-path
          (file-name-as-directory (file-name-concat sessions-dir new-id)))
         (staging-path
          (file-name-as-directory
           (make-temp-file
            (expand-file-name ".mevedel-worktree-fork-" sessions-dir) t)))
         (staging-buffer
          (generate-new-buffer " *mevedel-worktree-fork*"))
         (source-roots
          (buffer-local-value 'mevedel-workspace-additional-roots buffer))
         child report dropped additional-roots worktree-created failure result)
    (condition-case err
        (setq result
              (unwind-protect
                  (progn
                    (mevedel-worktree-fork-create reservation)
                    (let ((now (format-time-string "%FT%H-%M-%S")))
                      (setq worktree-created t
                            child
                            (mevedel-session-persistence--clone-session
                             session 'fork
                             :save-path staging-path
                             :session-id new-id
                             :created-at now
                             :updated-at now
                             :current-segment picked-segment
                             :forked-from-session-id
                             (mevedel-session-session-id session)
                             :forked-from-turn picked-cum-turn)))
                    (setf
                     (mevedel-session-name child) child-name
                     (mevedel-session-fork-type child) 'worktree
                     (mevedel-session-forked-from-fork-point-id child)
                     (plist-get target :fork-point-id)
                     (mevedel-session-worktree-source-root child)
                     (plist-get reservation :source-root)
                     (mevedel-session-worktree-directory child)
                     (plist-get reservation :directory)
                     (mevedel-session-worktree-branch child)
                     (plist-get reservation :branch)
                     (mevedel-session-worktree-base-commit child)
                     (plist-get reservation :base-commit))
                    (setq dropped
                          (mevedel-session-persistence--retarget-worktree-state child))
                    (let ((roots
                           (mevedel-session-persistence--retarget-worktree-roots
                            child source-roots)))
                      (setq additional-roots (plist-get roots :roots)
                            dropped
                            (nconc dropped (plist-get roots :dropped))))
                    (setq report
                          (mevedel-session-persistence--restore-worktree-files
                           session child picked-cum-turn))
                    (setq report (plist-put report :dropped dropped))
                    (with-current-buffer staging-buffer
                      (let ((org-agenda-file-menu-enabled nil))
                        (org-mode)))
                    (mevedel-session-persistence--load-rewind-target
                     session staging-buffer target)
                    (with-current-buffer staging-buffer
                      (goto-char (point-max))
                      (let ((start (point)))
                        (insert
                         (mevedel-session-persistence--worktree-fork-disclosure
                          child report))
                        (set-text-properties start (point) nil)))
                    (mevedel-session-persistence--publish-fork
                     child buffer staging-buffer parent-save-path staging-path
                     new-save-path picked-segment picked-cum-turn
                     additional-roots))
                (when (file-directory-p staging-path)
                  (ignore-errors (delete-directory staging-path t)))
                (when (buffer-live-p staging-buffer)
                  (with-current-buffer staging-buffer
                    (set-buffer-modified-p nil)
                    (setq-local kill-buffer-hook nil))
                  (kill-buffer staging-buffer))))
      (error (setq failure err)))
    (if (not failure)
        result
      (if worktree-created
          (error "%s"
                 (mevedel-session-persistence--worktree-fork-retained-error
                  session failure reservation))
        (signal (car failure) (cdr failure))))))

(defun mevedel-session-persistence--reduce-prompt-index
    (index picked-segment picked-cum-turn &optional before-turn)
  "Return a copy of INDEX trimmed to the fork's picked turn.
Drops segments past PICKED-SEGMENT entirely.  In the picked segment,
keeps only prompts whose `:cum-turn' is `<=' PICKED-CUM-TURN, or all
prompts when PICKED-CUM-TURN is nil.  When BEFORE-TURN is non-nil, drops the
picked prompt too."
  (cl-loop for (seg . prompts) in index
           when (< seg picked-segment)
           collect (cons seg (copy-sequence prompts))
           when (= seg picked-segment)
           collect (cons
                    seg
                    (cl-remove-if-not
                     (lambda (p)
                       (let ((ct (plist-get p :cum-turn)))
                         (or (null picked-cum-turn)
                             (if before-turn
                                 (< ct picked-cum-turn)
                               (<= ct picked-cum-turn)))))
                     prompts))))

(defun mevedel-session-persistence--reduce-file-snapshots
    (snapshots picked-cum-turn &optional before-turn)
  "Return SNAPSHOTS trimmed at PICKED-CUM-TURN.
SNAPSHOTS is an alist keyed by cumulative turn number.  When
PICKED-CUM-TURN is nil, returns SNAPSHOTS unchanged.  When BEFORE-TURN is
non-nil, drops the picked checkpoint too."
  (if (null picked-cum-turn)
      snapshots
    (cl-remove-if-not
     (lambda (entry)
       (if before-turn
           (< (car entry) picked-cum-turn)
         (<= (car entry) picked-cum-turn)))
     snapshots)))

(defun mevedel-session-persistence--commit-remote-rename
    (session buffer new-name new-id new-save-path)
  "Rename portable project SESSION and commit NEW-NAME through its current
lease head.

BUFFER is SESSION's root data buffer.  NEW-ID and NEW-SAVE-PATH name the moved
session tree.  Return a post-commit lease error, or nil.  A failure before the
sidecar marker CAS rolls the directory and in-memory paths back while the same
lease generation remains owned."
  (require 'mevedel-execution)
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (let* ((old-save-path (mevedel-session-save-path session))
         (old-id (mevedel-session-session-id session))
         (old-name (mevedel-session-name session))
         (old-publication
          (or (mevedel-session-publication session)
              (mevedel-session-publication-read old-save-path)))
         (head-before (plist-get old-publication :head))
         (old-buffer-file (buffer-local-value 'buffer-file-name buffer))
         (old-buffer-truename
          (buffer-local-value 'buffer-file-truename buffer))
         (new-buffer-file
          (and old-buffer-file
               (file-name-concat
                new-save-path (file-name-nondirectory old-buffer-file))))
         moved committed operation-error rollback-error post-commit-error)
    (unless head-before
      (error "Portable project Rename requires a committed publication"))
    (condition-case err
        (progn
          (mevedel-session-durability-call-with-reserved-lease
           session
           (lambda ()
             (rename-file (directory-file-name old-save-path)
                          (directory-file-name new-save-path))
             (setq moved t)
             ;; Reservation finalization must immediately follow the moved
             ;; lease at its new qualified path.
             (setf (mevedel-session-save-path session) new-save-path
                   (mevedel-session-session-id session) new-id
                   (mevedel-session-name session) new-name)
             (with-current-buffer buffer
               (setq buffer-file-name new-buffer-file
                     buffer-file-truename nil))
             (mevedel-execution-relocate-artifacts
              session old-save-path new-save-path)
             (setf (mevedel-session-publication session)
                   (or
                    (mevedel-session-publication-read new-save-path)
                    (error "Moved session has no committed publication")))))
          (condition-case publish-error
              (mevedel-session-publication-publish
               session
               (list
                (list
                 :path
                 (mevedel-session-persistence--sidecar-path new-save-path)
                 :content
                 (mevedel-session-persistence--printed-value
                  (mevedel-session-persistence--build-sidecar session buffer))
                 :commit-marker t)))
            (error
             (if (not
                  (equal head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                 (setq committed t
                       post-commit-error publish-error)
               (signal (car publish-error) (cdr publish-error)))))
          (unless committed
            (when (equal head-before
                         (plist-get (mevedel-session-publication session)
                                    :head))
              (error "Portable project Rename did not commit a publication head"))
            (setq committed t)))
      (error (setq operation-error err)))
    (when (and operation-error moved (not committed))
      (condition-case err
          (progn
            (mevedel-session-durability-call-with-reserved-lease
             session
             (lambda ()
               (rename-file (directory-file-name new-save-path)
                            (directory-file-name old-save-path))
               (setf (mevedel-session-save-path session) old-save-path
                     (mevedel-session-session-id session) old-id
                     (mevedel-session-name session) old-name
                     (mevedel-session-publication session) old-publication)
               (with-current-buffer buffer
                 (setq buffer-file-name old-buffer-file
                       buffer-file-truename old-buffer-truename))
               (mevedel-execution-relocate-artifacts
                session new-save-path old-save-path)))
            (mevedel-session-publication-discard-rolled-back
             session))
        (error (setq rollback-error err))))
    (when rollback-error
      (error
       "Portable project Rename rollback incomplete after %s: %s"
       (error-message-string operation-error)
       (error-message-string rollback-error)))
    (when operation-error
      (signal (car operation-error) (cdr operation-error)))
    post-commit-error))

;;;###autoload
(defun mevedel-rename-session (new-name)
  "Rename the current session to NEW-NAME.

Updates `:session-name', renames the on-disk session directory (so
its prefix matches the new name), updates `:save-path' / `:session-id'
on the session struct, repoints the buffer's variable `buffer-file-name'
to the renamed directory, rewrites the sidecar, and renames the chat
buffer per `mevedel-session-buffer-name'.

Works from a chat buffer or a view buffer."
  (interactive "sNew session name: ")
  (let* ((data-buf
          (cond
           ((and (boundp 'mevedel--session) mevedel--session) (current-buffer))
           ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           (t (user-error "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session data-buf)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (mevedel-session-persistence-assert-mutation-authority session data-buf)
    (let ((sanitized (mevedel-session-persistence--sanitize new-name)))
      (when (string-empty-p sanitized)
        (user-error "Empty session name"))
      ;; Rename the on-disk directory if the session is materialized.
      (let (post-commit-error)
      (when (mevedel-session-save-path session)
        (let* ((old-save-path (mevedel-session-save-path session))
               (parent-dir    (file-name-directory
                               (directory-file-name old-save-path)))
               (old-id        (or (mevedel-session-session-id session)
                                  (file-name-nondirectory
                                   (directory-file-name old-save-path))))
               (old-name-sanitized
                (mevedel-session-persistence--sanitize
                 (mevedel-session-name session)))
               ;; Derive the suffix by stripping the old sanitized
               ;; name from the start of the id.  This does not
               ;; hard-code the id format beyond "name + dash +
               ;; whatever" -- changes to the timestamp/uuid portion
               ;; do not break rename.  Fallback: rebuild a fresh id
               ;; from scratch.
               (prefix         (concat old-name-sanitized "-"))
               (suffix         (if (and (stringp old-id)
                                        (string-prefix-p prefix old-id))
                                   (substring old-id (length old-name-sanitized))
                                 nil))
               (new-id         (if suffix
                                   (concat sanitized suffix)
                                 (mevedel-session-persistence--compute-id
                                  sanitized)))
               (new-save-path  (file-name-as-directory
                                (file-name-concat parent-dir new-id))))
          (if (mevedel-session-persistence--portable-authority-p session)
              (setq post-commit-error
                    (mevedel-session-persistence--commit-remote-rename
                     session data-buf sanitized new-id new-save-path))
            (rename-file (directory-file-name old-save-path)
                         (directory-file-name new-save-path))
            (require 'mevedel-execution)
            (mevedel-execution-relocate-artifacts
             session old-save-path new-save-path)
            (setf (mevedel-session-save-path session) new-save-path
                  (mevedel-session-session-id session) new-id)
            (with-current-buffer data-buf
              (when buffer-file-name
                (setq buffer-file-name
                      (file-name-concat
                       new-save-path
                       (file-name-nondirectory buffer-file-name))))))))
      ;; PID-lock and unmaterialized sessions retain the direct metadata write.
      (unless (and (mevedel-session-save-path session)
                   (mevedel-session-persistence--portable-authority-p session))
        (setf (mevedel-session-name session) sanitized)
        (when (mevedel-session-save-path session)
          (mevedel-session-persistence-publish-text
           session
           (mevedel-session-persistence--sidecar-path
            (mevedel-session-save-path session))
           (mevedel-session-persistence--printed-value
            (mevedel-session-persistence--build-sidecar session data-buf)))))
      ;; Rename the chat buffer per the convention.  The view observes this
      ;; semantic event and derives its own presentation name.
      (let* ((workspace (mevedel-session-workspace session))
             (new-data-name (mevedel-session-buffer-name sanitized workspace)))
        (with-current-buffer data-buf
          (rename-buffer new-data-name t))
        (mevedel-session-persistence--notify-session-event
         session 'rename new-data-name))
      (message "Session renamed to %s" sanitized)
      (when post-commit-error
        (error "Portable project Rename committed, but lease finalization failed: %s"
               (error-message-string post-commit-error)))))))


;;
;;; Session listing & resume command

(defvar mevedel-session-persistence--summary-cache
  (make-hash-table :test #'equal)
  "Parsed session summaries keyed by sidecar path and file fingerprint.")

(defun mevedel-session-persistence--read-summary (sidecar-path)
  "Read picker-relevant fields from SIDECAR-PATH; nil on failure.

Cheap by design: only fields displayed in the picker (annotations,
sort key) are extracted.  The full sidecar plist is left on disk
until restore actually reads it.  An unchanged file fingerprint reuses
its previously parsed summary."
  (let* ((path (expand-file-name sidecar-path))
         (attributes (file-attributes path))
         (fingerprint
          (and attributes
               (list (file-attribute-file-identifier attributes)
                     (file-attribute-modification-time attributes)
                     (file-attribute-size attributes))))
         (cached
          (and fingerprint
               (gethash path mevedel-session-persistence--summary-cache))))
    (if (and cached (equal fingerprint (car cached)))
        (cdr cached)
      (let ((summary
             (condition-case _
                 (let* ((plist (mevedel-session-persistence-read path))
                        (_version
                         (unless
                             (equal (plist-get plist :version)
                                    mevedel-session-persistence-format-version)
                           (error "Unsupported session version")))
                        (_shape
                         (mevedel-session-persistence--validate-current-sidecar
                          plist)))
                   (list :session-id         (plist-get plist :session-id)
                         :session-name       (plist-get plist :session-name)
                         :workspace          (plist-get plist :workspace)
                         :created-at         (plist-get plist :created-at)
                         :updated-at         (plist-get plist :updated-at)
                         :current-segment    (plist-get plist :current-segment)
                         :total-turn-count   (plist-get plist :total-turn-count)
                         :first-user-message
                         (plist-get plist :first-user-message)
                         :latest-user-message
                         (plist-get plist :latest-user-message)
                         :fork-point-ids
                         (cl-loop
                          for segment in (plist-get plist :prompt-index)
                          append
                          (cl-loop
                           for prompt in (cdr segment)
                           for id = (plist-get prompt :fork-point-id)
                           when (stringp id)
                           collect id))
                         :working-directory
                         (plist-get plist :working-directory)
                         :forked-from-session-id
                         (plist-get plist :forked-from-session-id)
                         :fork-type (plist-get plist :fork-type)
                         :forked-from-fork-point-id
                         (plist-get plist :forked-from-fork-point-id)
                         :worktree-source-root
                         (plist-get plist :worktree-source-root)
                         :worktree-directory
                         (plist-get plist :worktree-directory)
                         :worktree-branch (plist-get plist :worktree-branch)
                         :worktree-base-commit
                         (plist-get plist :worktree-base-commit)))
               (error nil))))
        (when fingerprint
          (puthash path (cons fingerprint summary)
                   mevedel-session-persistence--summary-cache))
        summary))))

(defvar mevedel-session-persistence--list-sessions-cache
  (make-hash-table :test #'equal)
  "Last live session enumeration per workspace root, as (t . SESSIONS).

Enumerating a workspace costs several target round trips per session,
so decorative consumers reuse the newest live listing instead of
paying that on every redraw.  Every live enumeration refreshes it.")

(defun mevedel-session-persistence-list-sessions (workspace &optional cached)
  "Return a list of `(:save-path :summary)' plists for WORKSPACE's sessions.

Sorted by `:updated-at' descending.  Sessions whose sidecar can't be
parsed are silently dropped.  Portable sessions are listed only when their
lease names a valid immutable publication; fixed portable sidecars are ignored.

When CACHED is non-nil, reuse this process's last live enumeration for
WORKSPACE when one exists.  Only decorations tolerant of a listing as
old as the last picker, resume, or fork should pass it; anything that
decides authority or names sessions to the user enumerates live."
  (let ((root (mevedel-workspace-root workspace)))
    (or (and cached
             (cdr (gethash
                   root
                   mevedel-session-persistence--list-sessions-cache)))
        (let* ((sessions-dir
                (mevedel-session-persistence--sessions-dir workspace))
               (authority-mode
                (mevedel-session-persistence--workspace-authority-mode
                 workspace))
               (portable-p (eq authority-mode 'portable))
               (results nil))
    (when (file-directory-p sessions-dir)
      (dolist (entry
               (directory-files
                sessions-dir t
                "\\`\\(?:[^.]\\|\\.mevedel-save-as-\\)"))
        (when (file-directory-p entry)
          ;; Mixed control artifacts are an authority violation, not a
          ;; malformed optional session to hide from the picker.
          (when (or (mevedel-session-control-fs-path-exists-p
                     (file-name-concat entry ".lock"))
                    (mevedel-session-control-fs-path-exists-p
                     (file-name-concat entry ".lease"))
                    (mevedel-session-control-fs-path-exists-p
                     (file-name-concat entry "session.meta.el")))
            (mevedel-session-persistence--authority-mode-for-path
             entry nil authority-mode))
          (condition-case nil
              (let* ((publication
                      (when portable-p
                        (require 'mevedel-session-durability)
                        (require 'mevedel-session-publication)
                        (mevedel-session-publication-read entry)))
                     (sidecar
                      (if portable-p
                          (plist-get publication :sidecar)
                        (file-name-concat entry "session.meta.el")))
                     (summary
                      (and sidecar
                           (mevedel-session-persistence--read-summary sidecar))))
                (when summary
                  (let ((item
                         (list :save-path (file-name-as-directory entry)
                               :summary summary)))
                    (when publication
                      (setq item (plist-put item :publication publication)))
                    (push item results))))
            (error nil)))))
          (setq results
                (sort results
                      (lambda (a b)
                        (string-greaterp
                         (or (plist-get (plist-get a :summary) :updated-at)
                             "")
                         (or (plist-get (plist-get b :summary) :updated-at)
                             "")))))
          (puthash root (cons t results)
                   mevedel-session-persistence--list-sessions-cache)
          results))))

(defun mevedel-session-persistence-conversation-variants
    (session fork-point-id &optional sessions)
  "Return SESSION's persisted conversation variants at FORK-POINT-ID.
The Source is first, followed by direct Children.  Each returned session
entry has a `:variant-origin' of `source', `conversation', or `worktree'.
SESSIONS may supply an already-loaded session summary list."
  (let* ((session-id (mevedel-session-session-id session))
         (source-id
          (if (equal fork-point-id
                     (mevedel-session-forked-from-fork-point-id session))
              (mevedel-session-forked-from-session-id session)
            session-id))
         variants)
    (dolist
        (entry
         (or sessions
             (when-let* ((workspace
                          (mevedel-session-workspace session)))
               (mevedel-session-persistence-list-sessions workspace))))
      (let* ((summary (plist-get entry :summary))
             (id (plist-get summary :session-id))
             origin)
        (when (member fork-point-id (plist-get summary :fork-point-ids))
          (cond
           ((equal id source-id)
            (setq origin 'source))
           ((and (equal source-id
                        (plist-get summary :forked-from-session-id))
                 (equal fork-point-id
                        (plist-get summary :forked-from-fork-point-id)))
            (setq origin (plist-get summary :fork-type)))))
        (when (memq origin '(source conversation worktree))
          (setq entry (copy-sequence entry))
          (plist-put entry :variant-origin origin)
          (push entry variants))))
    (sort variants
          (lambda (left right)
            (let ((left-source
                   (eq (plist-get left :variant-origin) 'source))
                  (right-source
                   (eq (plist-get right :variant-origin) 'source)))
              (if (eq left-source right-source)
                  (let* ((left-summary (plist-get left :summary))
                         (right-summary (plist-get right :summary))
                         (left-created
                          (or (plist-get left-summary :created-at) ""))
                         (right-created
                          (or (plist-get right-summary :created-at) "")))
                    (if (equal left-created right-created)
                        (string-lessp
                         (or (plist-get left-summary :session-id) "")
                         (or (plist-get right-summary :session-id) ""))
                      (string-lessp left-created right-created)))
                left-source))))))

(defun mevedel-session-persistence--format-session-candidate (entry &optional detail)
  "Return a `completing-read' display string for session ENTRY.
Shows a relative-time annotation first so the newest session is
easiest to recognise at a glance.  DETAIL, when given, names the machine
holding the session and is placed ahead of the message preview, which is the
one field with no bound on its length."
  (let* ((s        (plist-get entry :summary))
         (updated  (plist-get s :updated-at))
         (relative (mevedel-session-persistence--format-relative-time updated))
         (name     (or (plist-get s :session-name) "?"))
         (preview  (or (plist-get s :latest-user-message)
                       (plist-get s :first-user-message)
                       ""))
         (segments (or (plist-get s :current-segment) 1))
         (turns    (or (plist-get s :total-turn-count) 0))
         (worktree
          (when (eq (plist-get s :fork-type) 'worktree)
            (let* ((origin (plist-get s :worktree-directory))
                   (current (plist-get s :working-directory))
                   (retargeted
                    (and
                     (stringp origin)
                     (stringp current)
                     (not
                      (equal
                       (file-name-as-directory (expand-file-name origin))
                       (file-name-as-directory
                        (expand-file-name current))))))
                   (origin-missing
                    (and (stringp origin)
                         (not (file-directory-p origin))))
                   (status
                    (cond
                     ((and retargeted origin-missing)
                      "retargeted; original missing")
                     (retargeted "retargeted")
                     (origin-missing "missing")
                     (t "active"))))
              (format "  Worktree Fork: %s (%s)"
                      (or current origin "?") status)))))
    (format "%-12s  %s  [%d seg, %d turns]%s%s  %s"
            relative name segments turns (or worktree "")
            (if detail (format "  (%s)" detail) "")
            preview)))

(defun mevedel-session-persistence--entry-live-buffer (entry)
  "Return the live root buffer already open for session ENTRY, or nil."
  (when-let ((session-id (plist-get (plist-get entry :summary) :session-id)))
    (require 'mevedel-session-control-transfer)
    (mevedel-session-control-transfer-root-buffer-for-id session-id)))

(defun mevedel-session-persistence--entry-authority (workspace entry)
  "Return `(:action LABEL :detail TEXT :held BOOL)' for WORKSPACE session ENTRY.

The verb names the outcome of choosing this row, which the lease decides:
whether anyone is writing the session right now, not whether its files
happen to be local or reached over TRAMP.  DETAIL names the machine
involved, which is the part a verb cannot carry.  HELD says whether some
client is writing this session now, which is a narrower question than
whether the row resumes: an expired lease offers a takeover precisely
because nobody is writing it any more.

A whole lease observation per candidate is one target round trip per row, so
the state and the holder are read together."
  (cond
   ((when-let ((buffer (mevedel-session-persistence--entry-live-buffer entry)))
      (list :action "Switch"
            :detail (if (buffer-local-value 'mevedel-session--read-only-mode
                                            buffer)
                        "already open here, read-only"
                      "already open here")
            :held t)))
   ((eq (mevedel-session-persistence--workspace-authority-mode workspace)
        'portable)
    (require 'mevedel-session-durability)
    (let* ((status (mevedel-session-durability-lease-status
                    (plist-get entry :save-path)))
           (host (plist-get status :host))
           (elsewhere (and host (not (equal host (system-name))) host)))
      (pcase (plist-get status :state)
        ('foreign
         (list :action "Join"
               :detail (format "held by %s" (or host "another client"))
               :held t))
        ('expired
         (list :action "Take over"
               :detail (if host
                           (format "lease expired, was %s" host)
                         "lease expired")
               :held nil))
        (_
         (list :action "Resume"
               :detail (and elsewhere
                            (format "last held by %s" elsewhere))
               :held nil)))))
   ((mevedel-session-persistence--active-lock-p (plist-get entry :save-path))
    (list :action "Join" :detail "locked by another process" :held t))
   (t (list :action "Resume" :detail nil :held nil))))

(defun mevedel-session-persistence--entry-action (workspace entry)
  "Return the entry action label for WORKSPACE session ENTRY."
  (plist-get (mevedel-session-persistence--entry-authority workspace entry)
             :action))

(defun mevedel-session-persistence-choose-entry (workspace)
  "Choose a persisted WORKSPACE session or return `new'.

Return nil when no persisted sessions exist.  The action offered per row
follows the session's authority: an unheld session resumes, one already open
in this Emacs is switched to, one another client holds is joined read-only
and follows the owner from there, and an expired lease is taken over."
  ;; Expired sessions and locks left behind by dead Emacsen are swept before
  ;; listing, so the chooser never offers a row that exists only because
  ;; nothing has cleaned up after a previous invocation.
  (mevedel-session-persistence-cleanup-expired workspace)
  (mevedel-session-persistence--sweep-stale-locks workspace)
  (when-let ((sessions
              (mevedel-session-persistence-list-sessions workspace)))
    (let* ((new-label "Start new session")
           (held-p nil)
           (candidates
            (mapcar
             (lambda (entry)
               (let* ((authority
                       (mevedel-session-persistence--entry-authority
                        workspace entry))
                      (action (plist-get authority :action)))
                 ;; Whether another writer is live is already answered by
                 ;; the row's own authority read; asking the target again per
                 ;; candidate is a second round trip for the same fact.
                 (when (plist-get authority :held)
                   (setq held-p t))
                 (cons
                  (format "%-10s %s" action
                          (mevedel-session-persistence--format-session-candidate
                           entry (plist-get authority :detail)))
                  entry)))
             sessions))
           (choices (cons new-label (mapcar #'car candidates)))
           (choice
            (completing-read
             "Mevedel session: "
             (mevedel-session-persistence--ordered-display-collection
              choices 'mevedel-session-entry)
             nil t nil nil (car choices))))
      (if (equal choice new-label)
          (progn
            (when held-p
              (unless
                  (yes-or-no-p
                   (concat
                    "Another session writer is active. Independent sessions "
                    "can race over project files; use a Worktree Fork for "
                    "isolation. Start anyway? "))
                (user-error "New session was not started")))
            'new)
        (mevedel-session-persistence-restore
         (plist-get (cdr (assoc choice candidates)) :save-path)
         nil nil workspace)))))

(defun mevedel-session-persistence--ordered-display-collection
    (displays category)
  "Return a completion table over DISPLAYS that preserves candidate order.
CATEGORY is exposed as completion metadata for completion UI integrations."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . ,category)
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action displays string pred))))

(defun mevedel-session-persistence-choose-conversation-variant
    (variants current-session-id)
  "Choose from ordered VARIANTS, marking CURRENT-SESSION-ID in place."
  (let* ((candidates
          (mapcar
           (lambda (entry)
             (let* ((summary (plist-get entry :summary))
                    (id (plist-get summary :session-id))
                    (origin (plist-get entry :variant-origin))
                    (cwd (or (plist-get summary :working-directory) "?"))
                    (normalized-cwd
                     (and (stringp cwd)
                          (file-name-as-directory
                           (expand-file-name cwd))))
                    (shared-p
                     (and
                      normalized-cwd
                      (> (cl-count
                          normalized-cwd variants
                          :test #'equal
                          :key
                          (lambda (candidate)
                            (when-let* ((path
                                        (plist-get
                                         (plist-get candidate :summary)
                                         :working-directory))
                                       ((stringp path)))
                              (file-name-as-directory
                               (expand-file-name path)))))
                         1)))
                    (label
                     (pcase origin
                       ('source "Source")
                       ('conversation "Conversation")
                       ('worktree "Worktree")))
                    (details
                     (pcase origin
                       ('conversation
                        (if shared-p
                            " — shared files"
                          " — independent directory"))
                       ('worktree
                        (let* ((worktree
                                (plist-get summary :worktree-directory))
                               (branch
                                (plist-get summary :worktree-branch))
                               (retargeted
                                (and
                                 (stringp worktree)
                                 (stringp cwd)
                                 (not
                                  (equal
                                   (file-name-as-directory
                                    (expand-file-name worktree))
                                   normalized-cwd))))
                               (missing
                                (and (stringp worktree)
                                     (not (file-directory-p worktree))))
                               (status
                                (cond
                                 ((and retargeted missing)
                                  "retargeted; original missing")
                                 (retargeted "retargeted")
                                 (missing "missing")
                                 (t "active"))))
                          (format " — branch %s — worktree %s (%s)"
                                  (or branch "?")
                                  (or worktree "?")
                                  status)))
                       (_ "")))
                    (preview
                     (or (plist-get summary :latest-user-message)
                         (plist-get summary :first-user-message)
                         ""))
                    (display
                     (format "%s%-12s — %s — id %s — cwd %s%s — %s"
                             (if (equal id current-session-id) "* " "  ")
                             label
                             (or (plist-get summary :session-name) "?")
                             id cwd details preview)))
               (cons display entry)))
           variants))
         (displays (mapcar #'car candidates))
         (collection
          (mevedel-session-persistence--ordered-display-collection
           displays 'mevedel-conversation-variant))
         (current
          (cl-find
           current-session-id candidates
           :test #'equal
           :key (lambda (candidate)
                  (plist-get (plist-get (cdr candidate) :summary)
                             :session-id))))
         (chosen
          (completing-read
           "Switch variant: " collection nil t nil nil
           (or (car current) (car displays)))))
    (cdr (assoc chosen candidates))))

;;;###autoload
(defun mevedel-save-session (&optional arg)
  "Save the current mevedel session to disk explicitly.

Forces a save even when nothing has changed since the last
auto-save (useful after manual edits to the chat buffer).  Triggers lazy
materialization if the session has not yet hit disk.

With a prefix ARG, prompts for a new session name and creates an independent
child under a fresh id.  File-workspace sessions clone their directory.
Portable project sessions materialize only the parent's current committed
logical artifacts and start fresh lease and publication histories.  The
current buffer is repointed at the child; the parent remains resumable.

To rename the current session in place, use `mevedel-rename-session'."
  (interactive "P")
  (let* ((data-buf
          (cond
           ((and (boundp 'mevedel--session) mevedel--session) (current-buffer))
           ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           (t (user-error "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session data-buf)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (mevedel-session-persistence-assert-mutation-authority session data-buf)
    (when arg
      (require 'mevedel-agent-control)
      (when (mevedel-agent-control-active-turn-p session)
        (user-error "Interrupt active agent turns before save-as")))
    (cond
     (arg
      (mevedel-session-persistence--save-as session data-buf))
     (t
      (mevedel-session-persistence-save session data-buf nil t)
      (message "Session saved.")))))

(defun mevedel-session-persistence--save-as (session data-buf)
  "Save SESSION as an independent child under a fresh id.
Called from `mevedel-save-session' with a prefix arg.  Portable project
sessions copy only committed logical artifacts; file-workspace sessions copy
their directory.  Repoint DATA-BUF at the child after it commits."
  ;; Force materialization so either authority mode has a durable parent.
  (unless (mevedel-session-save-path session)
    (mevedel-session-persistence-save session data-buf nil t))
  (let* ((old-save-path (mevedel-session-save-path session))
         (old-id (mevedel-session-session-id session))
         (new-name (read-string
                    "Save session as (new name): "
                    (mevedel-session-name session)))
         (sanitized (mevedel-session-persistence--sanitize new-name))
         (_ (when (string-empty-p sanitized)
              (user-error "Empty session name")))
         (parent-dir (file-name-directory
                      (directory-file-name old-save-path)))
         (new-id (mevedel-session-persistence--allocate-session-id
                  sanitized parent-dir))
         (new-save-path (file-name-as-directory
                         (file-name-concat parent-dir new-id))))
    ;; Publish the parent completely before deriving the child.
    (mevedel-session-persistence-save session data-buf)
    (require 'mevedel-session-save-as)
    (if (mevedel-session-persistence--portable-authority-p session)
        (mevedel-session-save-as-run
         session data-buf sanitized new-id new-save-path)
      (let (child-acquired child)
        (unwind-protect
            (progn
              (copy-directory old-save-path new-save-path nil t t)
              (let ((lock-in-clone
                     (mevedel-session-persistence--lock-path new-save-path)))
                (when (file-exists-p lock-in-clone)
                  (delete-file lock-in-clone)))
              (unless
                  (mevedel-session-persistence-lock-acquire
                   new-save-path (buffer-name data-buf) session)
                (error "Could not acquire cloned session lock"))
              (setq child-acquired t)
              (let ((now (format-time-string "%FT%H-%M-%S")))
                (setq child
                      (mevedel-session-persistence--clone-session
                       session 'save-as
                       :save-path new-save-path
                       :session-id new-id
                       :name sanitized
                       :created-at now
                       :updated-at now
                       :forked-from-session-id old-id)))
              (setf (mevedel-session-save-path session) new-save-path
                    (mevedel-session-session-id session) new-id
                    (mevedel-session-name session) sanitized
                    (mevedel-session-forked-from-session-id session) old-id
                    (mevedel-session-forked-from-turn session)
                    (mevedel-session-turn-count session))
              (with-current-buffer data-buf
                (when buffer-file-name
                  (setq buffer-file-name
                        (file-name-concat
                         new-save-path
                         (file-name-nondirectory buffer-file-name)))))
              (mevedel-session-persistence-publish-text
               session
               (mevedel-session-persistence--sidecar-path new-save-path)
               (mevedel-session-persistence--printed-value
                (mevedel-session-persistence--build-sidecar
                 child data-buf))))
          (when child-acquired
            (condition-case _
                (mevedel-session-persistence-lock-release old-save-path)
              (error nil))))
        (mevedel-session-save-as--rename-live-session-buffers
         session data-buf)))
    (message "Session saved as %s" sanitized)
    new-save-path))


;;
;;; Auto-cleanup

(defvar mevedel-session-persistence--cleanup-throttle
  (make-hash-table :test #'equal)
  "Workspace-key set of cleanup runs already done in this Emacs invocation.

Keyed on `(WORKSPACE-TYPE . WORKSPACE-ID)'.  Reset implicitly on Emacs
restart (defvar starts fresh)."  )

(defun mevedel-session-persistence--parse-iso-time (str)
  "Parse `YYYY-MM-DDTHH-MM-SS' STR to a time value, or nil on failure."
  (when (and str (stringp str)
             (string-match
              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
              str))
    (encode-time
     (string-to-number (match-string 6 str))
     (string-to-number (match-string 5 str))
     (string-to-number (match-string 4 str))
     (string-to-number (match-string 3 str))
     (string-to-number (match-string 2 str))
     (string-to-number (match-string 1 str)))))

(defun mevedel-session-persistence--active-lock-p (save-path)
  "Return non-nil if SAVE-PATH's `.lock' counts as active.
A same-host lock is stale when its PID is dead or when the live process
start time proves PID reuse.  Cross-host locks are always treated as
active because we cannot probe the remote process."
  (when-let* ((lock-info (mevedel-session-persistence--read-lock
                          (mevedel-session-persistence--lock-path save-path))))
    (if (equal (plist-get lock-info :hostname) (system-name))
        (mevedel-session-persistence--same-host-lock-active-p lock-info)
      ;; Cross-host: cannot verify liveness, treat as active.
      t)))

(defun mevedel-session-persistence-cleanup-expired (workspace &optional force)
  "Delete file-workspace sessions older than `mevedel-session-max-age-days'.

Scans session directories independently of resume compatibility.  Uses
`:updated-at' when available, otherwise the sidecar or directory modification
time.

Portable project stores are not auto-cleaned.  File-workspace cleanup skips
sessions with an active lock.  Cross-host locks are active.
Same-host locks are stale when their PID is dead or when the live
process start time proves PID reuse.  Throttled to at most once per
`(workspace-type . workspace-id)' per Emacs invocation; when FORCE is
non-nil the throttle is bypassed.

Returns the number of sessions deleted, or nil when the cap is nil, WORKSPACE
uses portable authority, or the throttle has already fired."
  (let ((sessions-dir
         (mevedel-session-persistence--sessions-dir workspace)))
    (when (and mevedel-session-max-age-days
               (eq (mevedel-workspace-type workspace) 'file))
      (let* ((ws-key (cons (mevedel-workspace-type workspace)
                           (mevedel-workspace-id workspace)))
             (already-ran
              (gethash ws-key mevedel-session-persistence--cleanup-throttle)))
      (when (or force (not already-ran))
        (puthash ws-key t mevedel-session-persistence--cleanup-throttle)
        (let ((threshold-secs (* mevedel-session-max-age-days 24 60 60))
              (now            (float-time))
              (deleted        0))
          (dolist (save-path
                   (and (file-directory-p sessions-dir)
                        (directory-files sessions-dir t "\\`[^.]")))
            (when (file-directory-p save-path)
              (let* ((sidecar-path
                      (mevedel-session-persistence--sidecar-path save-path))
                     (sidecar
                      (condition-case nil
                          (mevedel-session-persistence-read sidecar-path)
                        (error nil)))
                     (updated-str (plist-get sidecar :updated-at))
                     (parsed-time
                      (or (mevedel-session-persistence--parse-iso-time
                           updated-str)
                          (file-attribute-modification-time
                           (file-attributes
                            (if (file-exists-p sidecar-path)
                                sidecar-path
                              save-path))))))
                (when (and parsed-time
                           (> (- now (float-time parsed-time))
                              threshold-secs)
                           (not
                            (mevedel-session-persistence--active-lock-p
                             save-path)))
                  (delete-directory save-path t)
                  (cl-incf deleted)))))
          (when (> deleted 0)
            (message "Cleaned up %d expired session%s"
                     deleted (if (= deleted 1) "" "s")))
          deleted))))))


;;
;;; Hook plumbing

;; Per-completed-turn autosave lives as a DONE-state terminal handler
;; installed by `mevedel-preset--build-handlers' (`mevedel-presets.el').
;; That placement is necessary for the completed-turn contract: the
;; handler fires only on success (not on abort/error), runs after the
;; turn-count bump, and runs before `mevedel-request-end' clears the
;; request struct (so `mevedel-request-file-snapshots' is still live).

(defun mevedel-session-persistence--allow-emacs-exit-p ()
  "Return non-nil when every live session may exit safely."
  (require 'mevedel-execution)
  (let ((blocker
         (cl-some
          (lambda (buffer)
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (and (boundp 'mevedel--session)
                              mevedel--session)
                     (cond
                      ((mevedel-session-pending-publication
                        mevedel--session)
                       'publication)
                      ((and
                        (not
                         (eq 'foreign
                             (plist-get
                              (mevedel-session-lease mevedel--session)
                              :state)))
                        (mevedel-execution-unsettled-mutation-p
                         mevedel--session))
                       'mutation))))))
          (buffer-list))))
    (pcase blocker
      ('publication
       (message
        (concat
         "mevedel: session publication is pending; run "
         "mevedel-session-publication-retry or "
         "mevedel-session-publication-abandon first"))
       nil)
      ('mutation
       (message
        (concat
         "mevedel: target mutation is unsettled; stop live executions or run "
         "mevedel-retry-target-readiness to acknowledge it first"))
       nil)
      (_ t))))

(defun mevedel-session-persistence--kill-emacs-hook ()
  "Save sessions, clean expired state, and release locks on Emacs exit.

Runs unconditionally so that locks don't outlive the Emacs process
that wrote them.  Best-effort: individual errors are swallowed so one
bad buffer can't block exit."
  (when (fboundp 'mevedel-execution-teardown-all)
    (ignore-errors (mevedel-execution-teardown-all)))
  (let (lock-dirs)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (boundp 'mevedel--session)
                     mevedel--session)
            (when (buffer-modified-p)
              (condition-case _
                  (mevedel-session-persistence-save mevedel--session buf)
                (error nil)))
            ;; An unmodified buffer still owes its queued diagnostics: a
            ;; deferred remote flush never fires once Emacs is exiting.
            (ignore-errors
              (mevedel-session-persistence--flush-diagnostic-logs-now
               mevedel--session))
            (when-let ((dir (mevedel-session-save-path mevedel--session)))
              (cl-pushnew dir lock-dirs :test #'equal))))))
    ;; Keep live locks through cleanup so an exit-save failure cannot expose
    ;; an old session directory for deletion.
    (when (and (boundp 'mevedel-workspace--registry)
               (hash-table-p mevedel-workspace--registry))
      (maphash
       (lambda (_ workspace)
         (ignore-errors
           (mevedel-session-persistence-cleanup-expired workspace)))
       mevedel-workspace--registry))
    (dolist (dir lock-dirs)
      (condition-case _
          (mevedel-session-persistence-lock-release dir)
        (error nil)))))

;; Install at file-load time so exit persistence runs even when the user
;; never called `mevedel-install' this Emacs (e.g. running `mevedel' to
;; resume is the only command invoked).  Duplicate adds are no-ops by `add-hook'.
(add-hook 'kill-emacs-query-functions
          #'mevedel-session-persistence--allow-emacs-exit-p)
(add-hook 'kill-emacs-hook #'mevedel-session-persistence--kill-emacs-hook)


(provide 'mevedel-session-persistence)

;;; mevedel-session-persistence.el ends here
