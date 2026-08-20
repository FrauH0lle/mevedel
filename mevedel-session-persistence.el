;;; mevedel-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Session persistence facade for lifecycle, resume, listing, locking, and
;; stale-session cleanup.  Sidecar coding belongs to `mevedel-session-codec';
;; paths, artifacts, snapshots, and segment writes belong to
;; `mevedel-session-artifacts'; restore plans and the Rewind transaction belong
;; to `mevedel-session-rewind'; and Fork projection, publication, Worktree
;; restoration, and rename belong to `mevedel-session-fork'.

;;
;; The codec-owned sidecar plist shape is:
;;
;;   (:version "v0.5.3"
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

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-agents)
  (require 'mevedel-structs))

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
(defvar gptel-system-prompt)

;; `gptel-org'
(declare-function gptel-org--restore-state "ext:gptel-org" nil)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p "mevedel-agent-control" (session))

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-deserialize-registry "mevedel-agent-persistence" (raw))
(declare-function mevedel-agent-persistence-restore-tree "mevedel-agent-persistence" (session root-buffer readonly-p))
(declare-function mevedel-agent-persistence-sanitize-mailbox "mevedel-agent-persistence" (raw recipient))
(declare-function mevedel-agent-persistence-serialize-registry "mevedel-agent-persistence" (session))
(declare-function mevedel-agent-persistence-transcript-path-p "mevedel-agent-persistence" (path save-path))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-sidecar-dirty "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-transcript-relative-path "mevedel-agents" (cl-x))

;; `mevedel-chat'
(declare-function mevedel--chat-buffer-disable-org-element-cache "mevedel-chat" nil)
(declare-function mevedel--chat-buffer-init-common "mevedel-chat" (buf workspace source &optional inspection-p))
(declare-function mevedel--normalize-session-directory "mevedel-chat" (directory workspace))
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))

;; `mevedel-directive'
(declare-function mevedel-workspace-rewind-directives "mevedel-directive" (workspace session-id target-turn))

;; `mevedel-execution'
(declare-function mevedel-execution-relocate-artifacts "mevedel-execution" (session old-root new-root))
(declare-function mevedel-execution-session-live-p "mevedel-execution" (session))
(declare-function mevedel-execution-teardown-all "mevedel-execution" nil)
(declare-function mevedel-execution-unsettled-mutation-p "mevedel-execution" (session))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-acknowledge-incarnation "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-create "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-incarnation "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-incarnation-changed-p "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-native-path "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-native-root "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-observe-incarnation "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-prepare-incarnation-acknowledgement "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-probe "mevedel-execution-target" (target &optional refresh sandbox-mode))
(declare-function mevedel-execution-target-readiness "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-refresh-incarnation "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-remote-p "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-restore-incarnation "mevedel-execution-target" (target incarnation))

;; `mevedel-hooks'
(declare-function mevedel-hooks-flush-log "mevedel-hooks" (session))

;; `mevedel-permission-log'
(declare-function mevedel-permission-log-flush "mevedel-permission-log" (session))

;; `mevedel-permissions'
(declare-function mevedel-permission-deserialize-authority "mevedel-permissions" (rules grants target))
(declare-function mevedel-permission-invalidate-target-grants "mevedel-permissions" (session))
(declare-function mevedel-permission-serialize-authority "mevedel-permissions" (rules grants target))
(defvar mevedel-permission-mode)

;; `mevedel-persistence'
(declare-function mevedel--load-instructions-file "mevedel-persistence" (path &optional base-directory confirm quiet workspace directive-records preserve-directives-p))
(declare-function mevedel--reset-instructions-preserving-directives "mevedel-persistence" (workspace directives))
(declare-function mevedel--restore-preserved-directives "mevedel-persistence" (workspace))
(declare-function mevedel--serialize-instructions "mevedel-persistence" (&optional base-directory include-original-content))
(declare-function mevedel--write-instructions-file "mevedel-persistence" (path &optional base-directory write-empty quiet include-original-content))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--render-data-blocks "mevedel-pipeline" (string))
(declare-function mevedel-pipeline--render-data-call-range-p "mevedel-pipeline" (data beg end))
(declare-function mevedel-pipeline--render-data-without-owner "mevedel-pipeline" (data))
(declare-function mevedel-pipeline-reconcile-lost-executions "mevedel-pipeline" (buffer &optional successor-execution-ids))
(defvar mevedel-pipeline--render-data-close)
(defvar mevedel-pipeline--render-data-open)

;; `mevedel-reminders'
(declare-function mevedel-reminders-clone-list "mevedel-reminders" (reminders))

;; `mevedel-sandbox'
(defvar mevedel-sandbox-mode)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifact-present-p "mevedel-session-artifacts" (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-assert-mutation-authority "mevedel-session-artifacts" (session &optional buffer))
(declare-function mevedel-session-artifacts-build-sidecar "mevedel-session-artifacts" (session buffer))
(declare-function mevedel-session-artifacts-check-target-incarnation "mevedel-session-artifacts" (session buffer))
(declare-function mevedel-session-artifacts-compute-id "mevedel-session-artifacts" (name))
(declare-function mevedel-session-artifacts-content-start "mevedel-session-artifacts" (buffer))
(declare-function mevedel-session-artifacts-detect-highest-segment "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-disown-save-machinery "mevedel-session-artifacts" nil)
(declare-function mevedel-session-artifacts-finalized-segment-text "mevedel-session-artifacts" (text coding))
(declare-function mevedel-session-artifacts-find-artifact-noselect "mevedel-session-artifacts" (session logical &optional inspection))
(declare-function mevedel-session-artifacts-load-instructions "mevedel-session-artifacts" (session buffer &optional turn directive-records preserve-directives-p))
(declare-function mevedel-session-artifacts-printed-value "mevedel-session-artifacts" (value))
(declare-function mevedel-session-artifacts-publish-text "mevedel-session-artifacts" (session path content &optional coding))
(declare-function mevedel-session-artifacts-reconcile-relocation "mevedel-session-artifacts" (session saved-workspace-plist))
(declare-function mevedel-session-artifacts-sanitize "mevedel-session-artifacts" (name))
(declare-function mevedel-session-artifacts-save "mevedel-session-artifacts" (session buffer &optional settled force))
(declare-function mevedel-session-artifacts-segment-path "mevedel-session-artifacts" (save-path n))
(declare-function mevedel-session-artifacts-self-heal-segment-counter "mevedel-session-artifacts" (session save-path &optional defer-finalization-p))
(declare-function mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts" (workspace))
(declare-function mevedel-session-artifacts-sidecar-path "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-stabilize-gptel-bounds "mevedel-session-artifacts" nil)
(defvar mevedel-session-artifacts-require-agent-commit-p)

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-authority-mode "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-authority-mode-for-path "mevedel-session-codec" (session-dir &optional session explicit-mode))
(declare-function mevedel-session-codec-deserialize "mevedel-session-codec" (plist workspace))
(declare-function mevedel-session-codec-portable-authority-p "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-read "mevedel-session-codec" (path))
(declare-function mevedel-session-codec-validate-authority-mode "mevedel-session-codec" (mode workspace-plist))
(declare-function mevedel-session-codec-validate-current-sidecar "mevedel-session-codec" (plist))
(declare-function mevedel-session-codec-workspace-authority-mode "mevedel-session-codec" (workspace))
(declare-function mevedel-session-codec-write "mevedel-session-codec" (path plist))
(defvar mevedel-session-codec-format-version)

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-path-exists-p "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-physical-path "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-read-file "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-write-file "mevedel-session-control-fs" (path content))

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer-notify "mevedel-session-control-transfer" (session event &rest args))
(declare-function mevedel-session-control-transfer-observe "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-presentation-buffer "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-register-root-buffer "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-root-buffer-for-id "mevedel-session-control-transfer" (session-id))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-call-with-reserved-lease "mevedel-session-durability" (session function))
(declare-function mevedel-session-durability-disclose "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-forget-removed-session "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-acquire "mevedel-session-durability" (session-dir buffer-name &optional session))
(declare-function mevedel-session-durability-lease-owned-p "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-release "mevedel-session-durability" (session-dir &optional session))
(declare-function mevedel-session-durability-lease-state "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability-lease-status "mevedel-session-durability" (session-dir))
(defvar mevedel-session-durability--asserted-directories)
(defvar mevedel-session-durability--transaction-clock)

;; `mevedel-session-fork'
(declare-function mevedel-rename-session "mevedel-session-fork" (new-name))
(declare-function mevedel-session-fork-clone-session
                  "mevedel-session-fork"
                  (session policy &rest keys))


;; `mevedel-session-publication'
(declare-function mevedel-session-publication-committed-p "mevedel-session-publication" (session artifacts))
(declare-function mevedel-session-publication-discard-rolled-back "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-logical-path-p "mevedel-session-publication" (path))
(declare-function mevedel-session-publication-prune-committed "mevedel-session-publication" (session artifacts))
(declare-function mevedel-session-publication-publish "mevedel-session-publication" (session artifacts &optional require-commit))
(declare-function mevedel-session-publication-read "mevedel-session-publication" (session-dir))
(declare-function mevedel-session-publication-uncommitted-artifact "mevedel-session-publication" (session logical))

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery-record-failure "mevedel-session-recovery" (session reason recovery-path))
(declare-function mevedel-session-recovery-refresh "mevedel-session-recovery" (session))
(defvar mevedel-session-recovery--mutation-cache)

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-format-relative-time "mevedel-session-rewind" (iso))

;; `mevedel-session-save-as'
(declare-function mevedel-session-save-as--rename-live-session-buffers "mevedel-session-save-as" (session data-buffer))
(declare-function mevedel-session-save-as-run "mevedel-session-save-as" (session buffer new-name new-id new-save-path))

;; `mevedel-session-transfer'
(declare-function mevedel-session-transfer-decide "mevedel-session-transfer" (session decision))
(declare-function mevedel-session-transfer-poll "mevedel-session-transfer" (session))
(declare-function mevedel-session-transfer-release "mevedel-session-transfer" (session))
(declare-function mevedel-session-transfer-request "mevedel-session-transfer" (session &optional label))

;; `mevedel-transport'
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (owner remote-path function))

;; `mevedel-structs'
(declare-function mevedel-directive-attempt-checkpoint "mevedel-structs" (cl-x))
(declare-function mevedel-directive-attempt-untracked-effects "mevedel-structs" (cl-x))
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x))
(declare-function mevedel-directive-id "mevedel-structs" (cl-x))
(declare-function mevedel-goal--create "mevedel-structs" (&rest slots))
(declare-function mevedel-goal-created-at "mevedel-structs" (cl-x))
(declare-function mevedel-goal-id "mevedel-structs" (cl-x))
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x))
(declare-function mevedel-goal-plan-reference "mevedel-structs" (cl-x))
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x))
(declare-function mevedel-goal-status "mevedel-structs" (cl-x))
(declare-function mevedel-goal-time-used-seconds "mevedel-structs" (cl-x))
(declare-function mevedel-goal-token-budget "mevedel-structs" (cl-x))
(declare-function mevedel-goal-tokens-used "mevedel-structs" (cl-x))
(declare-function mevedel-goal-turns-run "mevedel-structs" (cl-x))
(declare-function mevedel-goal-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-request-file-snapshots "mevedel-structs" (cl-x))
(declare-function mevedel-session--create "mevedel-structs" (&rest slots))
(declare-function mevedel-session-agent-turn-capacity "mevedel-structs" (cl-x))
(declare-function mevedel-session-authority-mode-for-session "mevedel-structs" (session))
(declare-function mevedel-session-authority-mode-for-workspace "mevedel-structs" (workspace))
(declare-function mevedel-session-buffer-name "mevedel-structs" (session-name workspace))
(declare-function mevedel-session-control-transfer "mevedel-structs" (cl-x))
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x))
(declare-function mevedel-session-file-snapshots "mevedel-structs" (cl-x))
(declare-function mevedel-session-fork-type "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-fork-point-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-turn "mevedel-structs" (cl-x))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x))
(declare-function mevedel-session-lease "mevedel-structs" (cl-x))
(declare-function mevedel-session-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-input-p "mevedel-structs" (session))
(declare-function mevedel-session-pending-plan-approval "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-log-pending "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-queue "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x))
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication-active-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x))
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x))
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-set-root-buffer "mevedel-structs" (session buffer))
(declare-function mevedel-session-task-status-notes "mevedel-structs" (cl-x))
(declare-function mevedel-session-tasks "mevedel-structs" (cl-x))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-base-commit "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-branch "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-directory "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-source-root "mevedel-structs" (cl-x))
(declare-function mevedel-task--create "mevedel-structs" (&rest slots))
(declare-function mevedel-task-blocked-by "mevedel-structs" (cl-x))
(declare-function mevedel-task-blocks "mevedel-structs" (cl-x))
(declare-function mevedel-task-completed-turn "mevedel-structs" (cl-x))
(declare-function mevedel-task-description "mevedel-structs" (cl-x))
(declare-function mevedel-task-id "mevedel-structs" (cl-x))
(declare-function mevedel-task-metadata "mevedel-structs" (cl-x))
(declare-function mevedel-task-normalize-owner "mevedel-structs" (owner agent-registry))
(declare-function mevedel-task-owner "mevedel-structs" (cl-x))
(declare-function mevedel-task-prune-dangling-dependencies "mevedel-structs" (tasks))
(declare-function mevedel-task-status "mevedel-structs" (cl-x))
(declare-function mevedel-task-subject "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-get-or-create "mevedel-structs" (type id root name))
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-set-directives "mevedel-structs" (workspace directives))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x))
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar mevedel-session--read-only-mode)
(defvar mevedel-workspace--registry)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-record "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-start "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-flush-log "mevedel-tool-repair-diagnostics" (session))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record "mevedel-transcript-audit" (record))
(declare-function mevedel-transcript-audit-records "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-audit-spans "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-buffer-directive-ranges "mevedel-transcript-audit" (&optional allow-open))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state "mevedel-transcript-restore" nil)
(declare-function mevedel-transcript-restore-properties "mevedel-transcript-restore" (&optional only-if-missing))
(declare-function mevedel-transcript-restore-sanitize-bounds "mevedel-transcript-restore" nil)

;; `mevedel-utilities'
(declare-function mevedel--forget-place "mevedel-utilities" nil)
(declare-function mevedel--normalize-message-text "mevedel-utilities" (text))
(declare-function mevedel-version "mevedel-utilities" (&optional here message))

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" nil)
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-agent'
(declare-function mevedel-view-reset-agent-ephemeral-state "mevedel-view-agent" (&optional view-buffer))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-history'
(declare-function mevedel-view-history-load "mevedel-view-history" (&optional session))
(declare-function mevedel-view-history-save "mevedel-view-history" (&optional view-buffer))

;; `mevedel-view-render'
(declare-function mevedel-view--rebase-data-sources "mevedel-view-render" (delta))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace-ensure-generated-state-ignored "mevedel-workspace" (workspace))
(defvar mevedel-workspace-additional-roots)

;; `mevedel-workspace-identity'
(declare-function mevedel-workspace-identity-ensure "mevedel-workspace-identity" (root))
(declare-function mevedel-workspace-identity-read "mevedel-workspace-identity" (root))

;; `mevedel-worktree'
(declare-function mevedel-worktree-fork-create "mevedel-worktree" (reservation))
(declare-function mevedel-worktree-fork-preflight "mevedel-worktree" (session))
(declare-function mevedel-worktree-fork-reservation "mevedel-worktree" (session &optional preflight))
(declare-function mevedel-worktree-fork-validate-reservation "mevedel-worktree" (session reservation))

;; `nadvice'
(declare-function advice-add "nadvice" (symbol where function &optional props))
(declare-function advice-member-p "nadvice" (advice symbol))

;; `org'
(declare-function org-entry-delete "ext:org" (pom property))
(declare-function org-entry-get "ext:org" (pom property &optional inherit literal-nil))
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
;;; Sidecar lifecycle

(defun mevedel-session-persistence-write-current-buffer-atomically (path)
  "Write the current buffer to PATH through a same-directory rename."
  (require 'mevedel-session-control-fs)
  (mevedel-session-control-fs-write-file
   (mevedel-session-control-fs-physical-path path)
   (buffer-substring-no-properties (point-min) (point-max))))

(defun mevedel-session-persistence--execution-successor-ids (path)
  "Return structured execution ids present in transcript PATH."
  (when (file-regular-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (delay-mode-hooks (org-mode))
      (require 'mevedel-pipeline)
      (require 'mevedel-transcript-audit)
      (require 'mevedel-transcript-restore)
      (mevedel-transcript-restore-properties)
      (let (ids)
        (dolist (record
                 (mevedel-transcript-audit-records
                  (buffer-string)))
          (when (memq (plist-get record :type)
                      '(execution-archive execution-completion))
            (when-let* ((id (plist-get (plist-get record :render-data)
                                       :execution-id)))
              (cl-pushnew id ids :test #'equal))))
        (dolist (block
                 (mevedel-pipeline--render-data-blocks
                  (buffer-substring-no-properties (point-min) (point-max))))
          (when-let* ((stored (caddr block))
                      (begin (+ (point-min) (car block)))
                      (end (+ (point-min) (cadr block)))
                      ((mevedel-pipeline--render-data-call-range-p
                        stored begin end))
                      (data
                       (mevedel-pipeline--render-data-without-owner stored))
                      (id (plist-get data :execution-id)))
            (cl-pushnew id ids :test #'equal)))
        ids))))

(defun mevedel-session-persistence--reconcile-lost-execution-file
    (path &optional successor-execution-ids artifact-callback)
  "Mark stale running execution rows in transcript PATH as lost.

When ARTIFACT-CALLBACK is non-nil, pass it the repaired replacement instead
of writing PATH directly."
  (require 'mevedel-session-artifacts)
  (when (file-regular-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (delay-mode-hooks (org-mode))
      (require 'mevedel-pipeline)
      (require 'mevedel-transcript-restore)
      (mevedel-transcript-restore-properties)
      (let ((count
              (mevedel-pipeline-reconcile-lost-executions
              (current-buffer) successor-execution-ids)))
        (when (> count 0)
          (mevedel-session-artifacts-stabilize-gptel-bounds)
          (if artifact-callback
              (funcall
               artifact-callback
               (list :path path
                     :content (buffer-string)
                     :coding (or buffer-file-coding-system 'utf-8-unix)))
            (mevedel-session-persistence-write-current-buffer-atomically
             path)))
        count))))

(defun mevedel-session-persistence-reconcile-lost-execution-segments
    (session &optional exclude-path artifact-callback)
  "Repair stale execution rows in SESSION segments except EXCLUDE-PATH.

ARTIFACT-CALLBACK has the meaning described by
`mevedel-session-persistence--reconcile-lost-execution-file'."
  (require 'mevedel-session-artifacts)
  (let ((save-path (mevedel-session-save-path session))
        (successor-ids
         (and exclude-path
              (mevedel-session-persistence--execution-successor-ids
               exclude-path)))
        (count 0))
    (cl-loop for segment downfrom (mevedel-session-current-segment session) to 1
             for path = (mevedel-session-artifacts-segment-path
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

(defun mevedel-session-persistence-flush-diagnostic-logs (session)
  "Flush SESSION diagnostics, keeping target I/O off the caller's path.

A remote diagnostic flush is a whole publication transaction carrying
data nothing reads live, so it waits for an idle transport instead of
extending the save or settlement that queued it.  A local flush is one
append and runs inline.  Emacs exit flushes inline either way."
  (let ((save-path (mevedel-session-save-path session)))
    (if (and save-path (file-remote-p save-path))
        (progn
          ;; The zero timer lets the save or settlement that queued the
          ;; diagnostics return first; transport idleness alone would run
          ;; the flush inline, because the transport is usually idle by
          ;; the time a save reaches its cleanup.
          (require 'mevedel-transport)
          (run-at-time
           0 nil #'mevedel-transport-run-when-idle
           (list 'diagnostic-flush session) save-path
           (lambda ()
             (mevedel-session-persistence--flush-diagnostic-logs-now
              session))))
      (mevedel-session-persistence--flush-diagnostic-logs-now session))))

(defun mevedel-session-persistence-allocate-session-id (name sessions-dir)
  "Return a fresh session id for NAME below SESSIONS-DIR."
  (require 'mevedel-session-artifacts)
  (cl-loop repeat 33
           for candidate = (mevedel-session-artifacts-compute-id name)
           for path = (file-name-concat sessions-dir candidate)
           unless (or (file-exists-p path) (file-symlink-p path))
           return candidate
           finally (error "Could not allocate a unique session id after 33 attempts")))

(defun mevedel-session-persistence-shallow-ensure-files (session buffer)
  "Materialize SESSION and BUFFER paths without writing the sidecar.

Used by sub-agent allocation: a sub-agent can spawn during
the parent's first turn (before any DONE handler has run), so we
need the session directory and `agents/' subdirectory before its retained
identity exists.  The later acknowledged registry commit performs the full
root snapshot and sidecar publication.

Returns SESSION's `save-path' on success, or nil on failure.  Idempotent."
  (require 'mevedel-session-artifacts)
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
                 (sessions-dir (mevedel-session-artifacts-sessions-dir
                                (mevedel-session-workspace session)))
                 (session-id
                  (mevedel-session-persistence-allocate-session-id
                   (mevedel-session-name session) sessions-dir))
                 (save-path (file-name-as-directory
                             (file-name-concat sessions-dir session-id)))
                 (segment-path (mevedel-session-artifacts-segment-path
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
            (mevedel-session-persistence-flush-diagnostic-logs session)
            (require 'mevedel-workspace)
            (mevedel-workspace-ensure-generated-state-ignored
             (mevedel-session-workspace session))
            (with-current-buffer buffer
              (unless buffer-file-name
                (setq buffer-file-name segment-path))
              (mevedel-session-artifacts-disown-save-machinery))
            (require 'mevedel-session-control-transfer)
            (mevedel-session-control-transfer-register-root-buffer
             session buffer)
            save-path)
        (error
         (message "mevedel: shallow session materialization failed: %S" err)
         nil))))

(defun mevedel-session-persistence-record-running-transcript
    (session entry)
  "Insert ENTRY into SESSION's agent-transcripts.  ENTRY is (ID . PLIST)."
  (when (and session (consp entry))
    (setf (alist-get (car entry)
                     (mevedel-session-agent-transcripts session)
                     nil nil #'equal)
          (cdr entry))))

(defun mevedel-session-persistence-update-transcript-entry
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

(defun mevedel-session-persistence-write-sidecar-now (session buffer)
  "Best-effort sidecar rewrite for SESSION and BUFFER.

Only writes when the sidecar file already exists on disk -- i.e.
a full root snapshot has written `session.meta.el'.  Before that, the session
may be shallowly materialized (directory + lock + agents/ but no sidecar), so
this observational rewrite remains deferred to the next critical agent commit
or root autosave."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (when (and session (mevedel-session-save-path session))
    (let* ((sidecar (mevedel-session-artifacts-sidecar-path
                     (mevedel-session-save-path session)))
           (portable-p
            (mevedel-session-codec-portable-authority-p session))
           (present-p
            (if portable-p
                (mevedel-session-artifacts-artifact-present-p
                 session "session.meta.el")
              (file-exists-p sidecar))))
      (when present-p
        (condition-case err
            (if portable-p
                (progn
                  (mevedel-session-artifacts-assert-mutation-authority
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
                     (mevedel-session-artifacts-printed-value
                      (mevedel-session-artifacts-build-sidecar
                       session buffer))
                     :commit-marker t))))
              (mevedel-session-codec-write
               sidecar
               (mevedel-session-artifacts-build-sidecar session buffer)))
          (error
           (message "mevedel: sidecar rewrite failed: %S" err)
           nil)
          (:success t))))))

(defun mevedel-session-persistence-save-agent-state (session)
  "Commit SESSION's agent state through its authoritative root buffer."
  (require 'mevedel-session-artifacts)
  (when-let* ((buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer)))
    (let ((mevedel-session-artifacts-require-agent-commit-p t))
      (mevedel-session-artifacts-save session buffer nil t))))


;;
;;; First user message extraction

(defun mevedel-session-persistence-first-user-message (buffer)
  "Return a one-line preview of the first user prompt in BUFFER, or nil.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Skips the initial org property drawer and
`#+begin_summary' / `#+end_summary' block bodies, so the picker preview
reflects an actual user prompt rather than metadata, a compaction
summary, or tool/reasoning block glue.  The preview is the first
non-empty line, truncated to 120 characters."
  (require 'mevedel-session-artifacts)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (catch 'found
            (let ((pos (mevedel-session-artifacts-content-start buffer)))
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

(defun mevedel-session-persistence-notify-session-event
    (session event &rest args)
  "Send semantic lifecycle EVENT to SESSION's registered observers."
  (require 'mevedel-session-control-transfer)
  (apply #'mevedel-session-control-transfer-notify session event args))

(defun mevedel-session-persistence-root-data-buffer-p (buffer)
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

(defun mevedel-session-persistence-authoritative-buffer (buffer)
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
         ((mevedel-session-persistence-root-data-buffer-p buffer)
          buffer))))))

(defun mevedel-session-persistence-root-buffer-for-session
  (session &optional buffer)
  "Return SESSION's live root data buffer, preferring BUFFER."
  (or (let ((registered (mevedel-session-root-buffer session)))
        (and (buffer-live-p registered) registered))
      (let ((candidate
             (mevedel-session-persistence-authoritative-buffer buffer)))
        (and candidate
             (eq session
                 (buffer-local-value 'mevedel--session candidate))
             candidate))))


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
    (let ((holder-start (mevedel-session-persistence-parse-iso-time
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
  (require 'mevedel-session-codec)
  (if (eq (mevedel-session-codec-authority-mode-for-path
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
  (require 'mevedel-session-codec)
  (if (eq (mevedel-session-codec-authority-mode-for-path
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
  (require 'mevedel-session-artifacts)
  (let ((sessions-dir (mevedel-session-artifacts-sessions-dir workspace)))
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

(defun mevedel-session-persistence-release-on-kill ()
  "Buffer-local `kill-buffer-hook' that releases session mutation authority."
  (when (and (boundp 'mevedel--session)
             mevedel--session)
    (when-let ((dir (mevedel-session-save-path mevedel--session)))
      (condition-case _
          (mevedel-session-persistence-lock-release dir mevedel--session)
        (error nil)))))


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
  (require 'mevedel-session-codec)
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
               (mevedel-session-codec-read path)
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
                       mevedel-session-codec-format-version)
          (error "Unsupported session version: %s"
                 (or (plist-get plist :version) "missing")))
        (mevedel-session-codec-validate-current-sidecar plist)))))))

(defvar-local mevedel-session--read-only-mode nil
  "Non-nil when this chat buffer is in read-only session mode.
Set by the restore path when a cross-host lock cannot be safely
broken.  While set: autosave is inhibited (the terminal DONE handler
early-outs), the view buffer refuses to insert into the data buffer,
and the data buffer itself is marked `buffer-read-only'.")

(defun mevedel-session-persistence-apply-read-only-mode (buf &optional reason)
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
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (require 'mevedel-execution-target)
  (let* ((dir-name (file-name-nondirectory
                    (directory-file-name session-dir)))
         (name (if (string-match
                    "\\`\\(.*?\\)-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9a-f]+\\'"
                    dir-name)
                   (match-string 1 dir-name)
                 dir-name))
         (highest (mevedel-session-artifacts-detect-highest-segment
                   session-dir))
         (now (format-time-string "%FT%H-%M-%S")))
    (mevedel-session--create
     :name            name
     :workspace       workspace
     :execution-target
     (mevedel-execution-target-create (mevedel-workspace-root workspace))
     :authority-mode
     (mevedel-session-codec-workspace-authority-mode workspace)
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

(defun mevedel-session-persistence-find-live-buffer (session-id buf-name)
  "Return the named live root buffer for SESSION-ID, or nil.

The canonical session buffer name is the durable locator.  Persistence does
not search unrelated buffers or infer roles from view-local variables."
  (or (progn
        (require 'mevedel-session-control-transfer)
        (mevedel-session-control-transfer-root-buffer-for-id session-id))
      (let ((candidate (get-buffer buf-name)))
        (when (and (mevedel-session-persistence-root-data-buffer-p candidate)
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
  (require 'mevedel-session-artifacts)
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
      (mevedel-session-artifacts-check-target-incarnation session buf)
      (require 'mevedel-pipeline)
      (when (> (mevedel-pipeline-reconcile-lost-executions buf) 0)
        (if artifact-callback
            (funcall
             artifact-callback
             (list :path segment-path
                   :content (buffer-string)
                   :coding (or buffer-file-coding-system 'utf-8-unix)))
          (mevedel-session-persistence-write-current-buffer-atomically
           segment-path)
          (set-visited-file-modtime))
        (set-buffer-modified-p nil)))
    (unless acquired
      (mevedel-session-persistence-apply-read-only-mode buf))
    (mevedel--chat-buffer-init-common
     buf workspace (or lifecycle-source "resume") (not acquired))
    (require 'mevedel-agent-persistence)
    (prog1
        (mevedel-agent-persistence-restore-tree
         session buf (bound-and-true-p mevedel-session--read-only-mode))
      (mevedel-session-artifacts-load-instructions session buf))))

(defun mevedel-session-persistence--finish-restored-buffer
    (buf session live persist-repairs-p &optional repair-artifacts)
  "Finish restoring BUF for SESSION and return BUF.
LIVE means BUF was already initialized.  When PERSIST-REPAIRS-P is non-nil,
write repaired sidecar state before rendering the companion view.
REPAIR-ARTIFACTS are portable transcript replacements published before the
sidecar commit marker."
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (with-current-buffer buf
    (when persist-repairs-p
      (let ((portable-p
             (mevedel-session-codec-portable-authority-p session))
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
                        (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session))
                        :content
                        (mevedel-session-artifacts-printed-value
                         (mevedel-session-artifacts-build-sidecar
                          session buf))
                        :commit-marker t)))))
                (mevedel-session-codec-write
                 (mevedel-session-artifacts-sidecar-path
                  (mevedel-session-save-path session))
                 (mevedel-session-artifacts-build-sidecar session buf)))
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
      (mevedel-session-persistence-notify-session-event
       session 'load-history session))
    (mevedel-session-persistence-notify-session-event
     session 'rerender))
  buf)

(defun mevedel-session-persistence-resume-id (workspace session-id)
  "Resume WORKSPACE session SESSION-ID, or return nil when unavailable."
  (require 'mevedel-session-artifacts)
  (unless (and (stringp session-id)
               (not (string-empty-p session-id))
               (equal session-id (file-name-nondirectory session-id))
               (not (member session-id '("." ".."))))
    (error "Invalid session id: %S" session-id))
  (let ((session-dir
         (file-name-as-directory
          (file-name-concat
           (mevedel-session-artifacts-sessions-dir workspace)
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
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (let* ((direct-sidecar
          (mevedel-session-artifacts-sidecar-path session-dir))
         ;; A supplied workspace is the authority profile for cold discovery.
         ;; Without one, read the fixed sidecar only to learn which committed
         ;; profile must be verified below.
         (cold-sidecar
          (and (not workspace)
               (not session-override)
               (mevedel-session-control-fs-path-exists-p direct-sidecar)
               (mevedel-session-codec-read direct-sidecar)))
         (authority-mode
          (or (and session-override
                   (mevedel-session-codec-authority-mode
                    session-override))
              (and workspace
                   (mevedel-session-codec-workspace-authority-mode
                    workspace))
              (and cold-sidecar
                   (mevedel-session-codec-validate-authority-mode
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
          (mevedel-session-codec-authority-mode-for-path
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
                             (mevedel-session-codec-deserialize
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
      (mevedel-session-artifacts-reconcile-relocation
       session (plist-get sidecar :workspace)))
    (let* ((buf-name     (mevedel-session-buffer-name
                          (mevedel-session-name session)
                          workspace))
           (session-id   (mevedel-session-session-id session))
           ;; Prefer session-id-based lookup so two saved sessions
           ;; named `main' in one workspace don't collide.
           (live         (mevedel-session-persistence-find-live-buffer
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
                      (mevedel-session-artifacts-self-heal-segment-counter
                       session session-dir (not (null repair-callback)))))
              (let* ((segment-n (mevedel-session-current-segment session))
                     (segment-path
                      (mevedel-session-artifacts-segment-path
                       session-dir segment-n))
                     (agent-repairs 0))
                (setq
                 buf
                 (or
                  live
                  (if portable-p
                      (mevedel-session-artifacts-find-artifact-noselect
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
                  (mevedel-session-persistence-reconcile-lost-execution-segments
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
                         (mevedel-session-artifacts-finalized-segment-text
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
                          (mevedel-session-artifacts-finalized-segment-text
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
  (require 'mevedel-session-codec)
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
                 (let* ((plist (mevedel-session-codec-read path))
                        (_version
                         (unless
                             (equal (plist-get plist :version)
                                    mevedel-session-codec-format-version)
                           (error "Unsupported session version")))
                        (_shape
                         (mevedel-session-codec-validate-current-sidecar
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
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (let ((root (mevedel-workspace-root workspace)))
    (or (and cached
             (cdr (gethash
                   root
                   mevedel-session-persistence--list-sessions-cache)))
        (let* ((sessions-dir
                (mevedel-session-artifacts-sessions-dir workspace))
               (authority-mode
                (mevedel-session-codec-workspace-authority-mode
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
            (mevedel-session-codec-authority-mode-for-path
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
  (require 'mevedel-session-rewind)
  (let* ((s        (plist-get entry :summary))
         (updated  (plist-get s :updated-at))
         (relative (mevedel-session-rewind-format-relative-time updated))
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
  (require 'mevedel-session-codec)
  (cond
   ((when-let ((buffer (mevedel-session-persistence--entry-live-buffer entry)))
      (list :action "Switch"
            :detail (if (buffer-local-value 'mevedel-session--read-only-mode
                                            buffer)
                        "already open here, read-only"
                      "already open here")
            :held t)))
   ((eq (mevedel-session-codec-workspace-authority-mode workspace)
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
  (require 'mevedel-session-artifacts)
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
    (mevedel-session-artifacts-assert-mutation-authority session data-buf)
    (when arg
      (require 'mevedel-agent-control)
      (when (mevedel-agent-control-active-turn-p session)
        (user-error "Interrupt active agent turns before save-as")))
    (cond
     (arg
      (mevedel-session-persistence--save-as session data-buf))
     (t
      (mevedel-session-artifacts-save session data-buf nil t)
      (message "Session saved.")))))

(defun mevedel-session-persistence--save-as (session data-buf)
  "Save SESSION as an independent child under a fresh id.
Called from `mevedel-save-session' with a prefix arg.  Portable project
sessions copy only committed logical artifacts; file-workspace sessions copy
their directory.  Repoint DATA-BUF at the child after it commits."
  ;; Force materialization so either authority mode has a durable parent.
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-fork)
  (unless (mevedel-session-save-path session)
    (mevedel-session-artifacts-save session data-buf nil t))
  (let* ((old-save-path (mevedel-session-save-path session))
         (old-id (mevedel-session-session-id session))
         (new-name (read-string
                    "Save session as (new name): "
                    (mevedel-session-name session)))
         (sanitized (mevedel-session-artifacts-sanitize new-name))
         (_ (when (string-empty-p sanitized)
              (user-error "Empty session name")))
         (parent-dir (file-name-directory
                      (directory-file-name old-save-path)))
         (new-id (mevedel-session-persistence-allocate-session-id
                  sanitized parent-dir))
         (new-save-path (file-name-as-directory
                         (file-name-concat parent-dir new-id))))
    ;; Publish the parent completely before deriving the child.
    (mevedel-session-artifacts-save session data-buf)
    (require 'mevedel-session-save-as)
    (if (mevedel-session-codec-portable-authority-p session)
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
                      (mevedel-session-fork-clone-session
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
              (mevedel-session-artifacts-publish-text
               session
               (mevedel-session-artifacts-sidecar-path new-save-path)
               (mevedel-session-artifacts-printed-value
                (mevedel-session-artifacts-build-sidecar
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

(defun mevedel-session-persistence-parse-iso-time (str)
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
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (let ((sessions-dir
         (mevedel-session-artifacts-sessions-dir workspace)))
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
                      (mevedel-session-artifacts-sidecar-path save-path))
                     (sidecar
                      (condition-case nil
                          (mevedel-session-codec-read sidecar-path)
                        (error nil)))
                     (updated-str (plist-get sidecar :updated-at))
                     (parsed-time
                      (or (mevedel-session-persistence-parse-iso-time
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
  (require 'mevedel-session-artifacts)
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
                  (mevedel-session-artifacts-save mevedel--session buf)
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
