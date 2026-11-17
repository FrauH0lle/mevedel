;;; mevedel-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Session persistence facade for lifecycle, mid-turn checkpoints, resume,
;; listing, locking, and stale-session cleanup.  Sidecar coding belongs to `mevedel-session-codec';
;; paths, artifacts, snapshots, and segment writes belong to
;; `mevedel-session-artifacts'; restore plans and the Rewind transaction belong
;; to `mevedel-session-rewind'; and Fork projection, publication, Worktree
;; restoration belong to `mevedel-session-fork'; session naming belongs to
;; `mevedel-session-naming'.

;;; Code:

(require 'mevedel-session-artifacts)
(require 'mevedel-session-codec)
(require 'mevedel-session-durability)
(require 'mevedel-telemetry)
(require 'mevedel-utilities)

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-agents)
  (require 'mevedel-structs))

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `gptel'
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--preset)
(defvar gptel-display-buffer-action)
(defvar gptel-system-prompt)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p "mevedel-agent-control" (session))
(declare-function mevedel-agent-control-reoffer-mail "mevedel-agent-control" (session))
(autoload 'mevedel-agent-control-active-turn-p "mevedel-agent-control")
(autoload 'mevedel-agent-control-reoffer-mail "mevedel-agent-control")

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-prepare-save
                  "mevedel-agent-conversation" (invocation))
(autoload 'mevedel-agent-conversation-prepare-save "mevedel-agent-conversation")
(autoload 'mevedel-agent-conversation-save "mevedel-agent-conversation")

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-restore-tree "mevedel-agent-persistence" (session root-buffer readonly-p))
(autoload 'mevedel-agent-persistence-restore-tree "mevedel-agent-persistence")

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-dedicated-p
                  "mevedel-artifact-store" (workspace session-id id))
(autoload 'mevedel-artifact-store-dedicated-p "mevedel-artifact-store")

;; `mevedel-chat'
(declare-function mevedel--chat-buffer-disable-org-element-cache "mevedel-chat" nil)
(declare-function mevedel--chat-buffer-init-common "mevedel-chat" (buf workspace source &optional inspection-p))
(declare-function mevedel--normalize-session-directory "mevedel-chat" (directory workspace))
(autoload 'mevedel--chat-buffer-disable-org-element-cache "mevedel-chat")
(autoload 'mevedel--chat-buffer-init-common "mevedel-chat")
(autoload 'mevedel--normalize-session-directory "mevedel-chat")

;; `mevedel-engine'
(declare-function mevedel-engine-assert-local-history
                  "mevedel-engine" (session operation &optional backend root-only))
(autoload 'mevedel-engine-assert-local-history "mevedel-engine")

;; `mevedel-execution'
(declare-function mevedel-execution-teardown-all "mevedel-execution" nil)
(declare-function mevedel-execution-unsettled-mutation-p "mevedel-execution" (session))
(autoload 'mevedel-execution-unsettled-mutation-p "mevedel-execution")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create "mevedel-execution-target" (directory))
(declare-function mevedel-execution-target-incarnation-changed-p "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-observe-incarnation "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-probe "mevedel-execution-target" (target &optional refresh sandbox-mode))
(declare-function mevedel-execution-target-remote-p "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-restore-incarnation "mevedel-execution-target" (target incarnation))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-observe-incarnation "mevedel-execution-target")
(autoload 'mevedel-execution-target-probe "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")
(autoload 'mevedel-execution-target-restore-incarnation "mevedel-execution-target")

;; `mevedel-hooks'
(declare-function mevedel-hooks-flush-log "mevedel-hooks" (session))

;; `mevedel-journal-capture'
(declare-function mevedel-journal-capture--read "mevedel-journal-capture" (workspace id &optional observation))
(declare-function mevedel-journal-capture-seal-and-schedule "mevedel-journal-capture"
                  (session buffer trigger &optional captures))
(autoload 'mevedel-journal-capture-seal-and-schedule "mevedel-journal-capture")
(autoload 'mevedel-journal-capture--read "mevedel-journal-capture")


;; `mevedel-journal-pins'
(declare-function mevedel-journal-pins-observe "mevedel-journal-pins" (session-dir))
(declare-function mevedel-journal-pins-present-p "mevedel-journal-pins" (session-dir))
(autoload 'mevedel-journal-pins-present-p "mevedel-journal-pins")
(autoload 'mevedel-journal-pins-observe "mevedel-journal-pins")

;; `mevedel-journal-process'
(declare-function mevedel-journal-process-discard "mevedel-journal-process" (workspace id &optional original-source))
(autoload 'mevedel-journal-process-discard "mevedel-journal-process")
(declare-function mevedel-journal-process-stop-all "mevedel-journal-process" ())
(defvar mevedel-journal-process--inhibit-scheduling)

;; `mevedel-memory-decision'
(declare-function mevedel-memory-decision-stop-recovery "mevedel-memory-decision" ())
(defvar mevedel-memory-decision--inhibit-recovery)

;; `mevedel-memory-pass'
(declare-function mevedel-memory-pass-stop-all "mevedel-memory-pass" ())
(defvar mevedel-memory-pass--inhibit-scheduling)

;; `mevedel-permission-log'
(declare-function mevedel-permission-log-flush "mevedel-permission-log" (session))

;; `mevedel-persistence'
(declare-function mevedel-instruction-store-note-session-ids "mevedel-persistence" (workspace))

;; `mevedel-ptc-checkpoint'
(declare-function mevedel-ptc-checkpoint-reconcile
                  "mevedel-ptc-checkpoint" (session))
(autoload 'mevedel-ptc-checkpoint-reconcile "mevedel-ptc-checkpoint")

;; `mevedel-sandbox'
(defvar mevedel-sandbox-mode)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts--live-sidecar "mevedel-session-artifacts" (session buffer))
(declare-function mevedel-session-artifacts--sidecar-artifact "mevedel-session-artifacts" (session buffer &optional checkpoint))
(declare-function mevedel-session-artifacts-artifact-present-p "mevedel-session-artifacts" (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-assert-mutation-authority "mevedel-session-artifacts" (session &optional buffer))
(declare-function mevedel-session-artifacts-build-sidecar "mevedel-session-artifacts" (session buffer &optional checkpoint))
(declare-function mevedel-session-artifacts-chats "mevedel-session-artifacts" (chapters prompt-index current-segment current-title))
(declare-function mevedel-session-artifacts-check-target-incarnation "mevedel-session-artifacts" (session buffer))
(declare-function mevedel-session-artifacts-collect-prompts "mevedel-session-artifacts" (buffer &optional preview-length))
(declare-function mevedel-session-artifacts-disown-save-machinery "mevedel-session-artifacts" nil)
(declare-function mevedel-session-artifacts-find-artifact-noselect "mevedel-session-artifacts" (session logical &optional inspection committed-only))
(declare-function mevedel-session-artifacts-inhibit-so-long "mevedel-session-artifacts" ())
(declare-function mevedel-session-artifacts-inspect-cold-session "mevedel-session-artifacts" (save-path &optional publication scope))
(declare-function mevedel-session-artifacts-load-instructions "mevedel-session-artifacts" (session))
(declare-function mevedel-session-artifacts-printed-value "mevedel-session-artifacts" (value))
(declare-function mevedel-session-artifacts-publish-sidecar-state "mevedel-session-artifacts" (session root-buffer &optional token))
(declare-function mevedel-session-artifacts-publish-text "mevedel-session-artifacts" (session path content &optional coding prepared token))
(declare-function mevedel-session-artifacts-reconcile-relocation "mevedel-session-artifacts" (session saved-workspace-plist))
(declare-function mevedel-session-artifacts-save "mevedel-session-artifacts" (session buffer &optional settled force token))
(declare-function mevedel-session-artifacts-save-agent-registry "mevedel-session-artifacts" (session buffer &optional token))
(declare-function mevedel-session-artifacts-segment-path "mevedel-session-artifacts" (save-path n))
(declare-function mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts" (workspace))
(declare-function mevedel-session-artifacts-sidecar-path "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-stabilize-gptel-bounds "mevedel-session-artifacts" nil)
(autoload 'mevedel-session-artifacts-inhibit-so-long
  "mevedel-session-artifacts")
(defvar mevedel-session-artifacts-require-agent-commit-p)
(defvar mevedel-session-artifacts--terminal-sessions)

;; `mevedel-session-checkpoint'
(declare-function mevedel-session-checkpoint-capture "mevedel-session-checkpoint" (session conversation &optional root-projection))
(declare-function mevedel-session-checkpoint-install "mevedel-session-checkpoint" (session candidate))
(declare-function mevedel-session-checkpoint-project "mevedel-session-checkpoint" (session sidecar &optional candidate))
(autoload 'mevedel-session-checkpoint-capture "mevedel-session-checkpoint")
(autoload 'mevedel-session-checkpoint-install "mevedel-session-checkpoint")
(autoload 'mevedel-session-checkpoint-project "mevedel-session-checkpoint")

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-authority-mode-for-path "mevedel-session-codec" (session-dir &optional session explicit-mode))
(declare-function mevedel-session-codec-deserialize "mevedel-session-codec" (plist workspace))
(declare-function mevedel-session-codec-portable-authority-p "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-read "mevedel-session-codec" (path))
(declare-function mevedel-session-codec-validate-authority-mode "mevedel-session-codec" (mode))
(declare-function mevedel-session-codec-validate-current-sidecar "mevedel-session-codec" (plist))
(defvar mevedel-session-codec-format-version)

;; `mevedel-session-collection'
(declare-function mevedel-session-collection-schedule "mevedel-session-collection" (session))
(autoload 'mevedel-session-collection-schedule "mevedel-session-collection")

;; `mevedel-session-control-fs'
(defvar mevedel-session-control-fs--async-wait)
(declare-function mevedel-session-control-fs-append-rotating "mevedel-session-control-fs"
                  (path content max-bytes))
(declare-function mevedel-session-control-fs-async-busy-p "mevedel-session-control-fs" (directory))
(declare-function mevedel-session-control-fs-path-exists-p "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-physical-path "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-program-value "mevedel-session-control-fs" (result))
(declare-function mevedel-session-control-fs-run-program "mevedel-session-control-fs" (operations &optional lock-directory))
(declare-function mevedel-session-control-fs-tree-sizes "mevedel-session-control-fs" (paths))
(declare-function mevedel-session-control-fs-wait-async "mevedel-session-control-fs" (&optional seconds))
(declare-function mevedel-session-control-fs-write-file "mevedel-session-control-fs" (path content))
(autoload 'mevedel-session-control-fs-append-rotating
  "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-path-exists-p
  "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-physical-path
  "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-write-file
  "mevedel-session-control-fs")

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer-notify "mevedel-session-control-transfer" (session event &rest args))
(declare-function mevedel-session-control-transfer-register-root-buffer "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-root-buffer-for-id "mevedel-session-control-transfer" (session-id))
(autoload 'mevedel-session-control-transfer-notify
  "mevedel-session-control-transfer")
(autoload 'mevedel-session-control-transfer-register-root-buffer
  "mevedel-session-control-transfer")
(autoload 'mevedel-session-control-transfer-root-buffer-for-id
  "mevedel-session-control-transfer")

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-cleanup-assessment "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability-discard-session "mevedel-session-durability" (session prepare-function))
(declare-function mevedel-session-durability-call-with-reserved-lease "mevedel-session-durability" (session function))
(declare-function mevedel-session-durability-disclose "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-acquire "mevedel-session-durability" (session-dir buffer-name &optional session))
(declare-function mevedel-session-durability-lease-owned-p "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-release "mevedel-session-durability" (session-dir &optional session token))
(declare-function mevedel-session-durability-lease-status "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability-set-unsettled-mutation "mevedel-session-durability" (session value &optional token))
(defvar mevedel-session-durability--asserted-directories)
(defvar mevedel-session-durability--transaction-clock)
(autoload 'mevedel-session-durability-disclose "mevedel-session-durability")
(autoload 'mevedel-session-durability-cleanup-assessment "mevedel-session-durability")
(autoload 'mevedel-session-durability-discard-session "mevedel-session-durability")
(autoload 'mevedel-session-durability-lease-acquire
  "mevedel-session-durability")
(autoload 'mevedel-session-durability-lease-owned-p
  "mevedel-session-durability")
(autoload 'mevedel-session-durability-lease-release
  "mevedel-session-durability")
(autoload 'mevedel-session-durability-lease-status
  "mevedel-session-durability")

;; `mevedel-session-fork'
(declare-function mevedel-session-fork-clone-session
                  "mevedel-session-fork"
                  (session policy &rest keys))
(autoload 'mevedel-session-fork-clone-session "mevedel-session-fork")

;; `mevedel-session-naming'
(declare-function mevedel-rename-session "mevedel-session-naming" (new-name))
(declare-function mevedel-session-naming-adopt-explicit "mevedel-session-naming" (session name))
(declare-function mevedel-session-naming-cancel "mevedel-session-naming" ())
(declare-function mevedel-session-naming-normalize "mevedel-session-naming" (name))
(autoload 'mevedel-rename-session "mevedel-session-naming" nil t)
(autoload 'mevedel-session-naming-adopt-explicit "mevedel-session-naming")
(autoload 'mevedel-session-naming-cancel "mevedel-session-naming")
(autoload 'mevedel-session-naming-normalize "mevedel-session-naming")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-async-eligible-p
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-call-with-diagnostic-batch
                  "mevedel-session-publication" (session function))
(declare-function mevedel-session-publication-barrier
                  "mevedel-session-publication" (session &optional token))
(declare-function mevedel-session-publication-busy-p
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-defer
                  "mevedel-session-publication"
                  (session preparation &optional failure token))
(declare-function mevedel-session-publication-publish "mevedel-session-publication" (session artifacts &optional require-commit prepared token))
(declare-function mevedel-session-publication-read "mevedel-session-publication" (session-dir &optional head names))
(declare-function mevedel-session-publication-read-batch
                  "mevedel-session-publication" (directories listings))
(declare-function mevedel-session-publication-stopping
                  "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-submit
                  "mevedel-session-publication"
                  (session prepare acknowledge &optional mail-ids retain-failure select-staged superseded))
(declare-function mevedel-session-publication-diagnostic-backlog-p
                  "mevedel-session-publication" (session))
(autoload 'mevedel-session-publication-async-eligible-p "mevedel-session-publication")
(autoload 'mevedel-session-publication-barrier "mevedel-session-publication")
(autoload 'mevedel-session-publication-busy-p "mevedel-session-publication")
(autoload 'mevedel-session-publication-defer "mevedel-session-publication")
(autoload 'mevedel-session-publication-stopping "mevedel-session-publication")
(autoload 'mevedel-session-publication-submit "mevedel-session-publication")
(autoload 'mevedel-session-publication-diagnostic-backlog-p
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-call-with-diagnostic-batch
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-publish "mevedel-session-publication")
(autoload 'mevedel-session-publication-read "mevedel-session-publication")
(autoload 'mevedel-session-publication-read-batch "mevedel-session-publication")

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery--abandon "mevedel-session-recovery" (session))
(declare-function mevedel-session-recovery-read "mevedel-session-recovery" (session-dir &optional root-observation))
(autoload 'mevedel-session-recovery--abandon "mevedel-session-recovery")
(autoload 'mevedel-session-recovery-read "mevedel-session-recovery")
(defvar mevedel-session-recovery--mutation-cache)

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-format-relative-time "mevedel-session-rewind" (iso))
(autoload 'mevedel-session-rewind-format-relative-time
  "mevedel-session-rewind")

;; `mevedel-session-save-as'
(declare-function mevedel-session-save-as--rename-live-session-buffers "mevedel-session-save-as" (session data-buffer))
(declare-function mevedel-session-save-as-run "mevedel-session-save-as" (session buffer new-name new-id new-save-path))
(autoload 'mevedel-session-save-as--rename-live-session-buffers
  "mevedel-session-save-as")
(autoload 'mevedel-session-save-as-run "mevedel-session-save-as")

;; `mevedel-transport'
(declare-function mevedel-transport-cancel-idle "mevedel-transport" (table tag))
(declare-function mevedel-transport-cancel-pending
                  "mevedel-transport" (&optional key))
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (owner remote-path function))
(declare-function mevedel-transport-schedule-idle
                  "mevedel-transport" (table key tag path thunk))
(autoload 'mevedel-transport-cancel-idle "mevedel-transport")
(autoload 'mevedel-transport-cancel-pending "mevedel-transport")
(autoload 'mevedel-transport-run-when-idle "mevedel-transport")
(autoload 'mevedel-transport-schedule-idle "mevedel-transport")

;; `mevedel-structs'
(declare-function mevedel-session-authority-mode-for-session "mevedel-structs" (session))
(declare-function mevedel-session-authority-mode-for-workspace "mevedel-structs" (workspace))
(declare-function mevedel-session-buffer-name "mevedel-structs" (session-name workspace))
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-fork-point-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-turn "mevedel-structs" (cl-x))
(declare-function mevedel-session-generate-id "mevedel-structs" ())
(declare-function mevedel-session-lease "mevedel-structs" (cl-x))
(declare-function mevedel-session-mail-notifications "mevedel-structs" (cl-x))
(declare-function mevedel-session-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-log-pending "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication-active-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication-stopping-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-set-root-buffer "mevedel-structs" (session buffer))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x))
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar mevedel-session--read-only-mode)

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-flush-log "mevedel-tool-repair-diagnostics" (session))

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-blocks
                  "mevedel-tool-render-data" (string))
(declare-function mevedel-tool-render-data-call-range-p
                  "mevedel-tool-render-data" (data beg end))
(declare-function mevedel-tool-render-data-reconcile-lost-executions
                  "mevedel-tool-render-data"
                  (buffer &optional successor-execution-ids))
(declare-function mevedel-tool-render-data-without-owner
                  "mevedel-tool-render-data" (data))
(defvar mevedel-tool-render-data-close)
(defvar mevedel-tool-render-data-open)
(autoload 'mevedel-tool-render-data-blocks "mevedel-tool-render-data")
(autoload 'mevedel-tool-render-data-call-range-p "mevedel-tool-render-data")
(autoload 'mevedel-tool-render-data-reconcile-lost-executions
  "mevedel-tool-render-data")
(autoload 'mevedel-tool-render-data-without-owner
  "mevedel-tool-render-data")

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-records "mevedel-transcript-audit" (text &optional type))
(autoload 'mevedel-transcript-audit-records "mevedel-transcript-audit")

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state "mevedel-transcript-restore" nil)
(declare-function mevedel-transcript-restore-properties "mevedel-transcript-restore" (&optional only-if-missing))
(autoload 'mevedel-transcript-restore-gptel-state
  "mevedel-transcript-restore")
(autoload 'mevedel-transcript-restore-properties "mevedel-transcript-restore")

;; `mevedel-utilities'
(declare-function mevedel--call-with-bare-transcript-mode
                  "mevedel-utilities" (function))
(declare-function mevedel--forget-place "mevedel-utilities" nil)
(declare-function mevedel--ordered-completion-table "mevedel-utilities" (displays category))
(declare-function mevedel--transcript-org-mode "mevedel-utilities" nil)
(declare-function mevedel-diagnostic-message "mevedel-utilities" (scope format &rest args))
(declare-function mevedel-diagnostic-scope "mevedel-utilities" (owner))
(declare-function mevedel-diagnostic-user-error "mevedel-utilities" (scope format &rest args))
(declare-function mevedel-diagnostic-warning "mevedel-utilities" (scope type message &optional level buffer-name))
(autoload 'mevedel--call-with-bare-transcript-mode "mevedel-utilities")
(autoload 'mevedel--forget-place "mevedel-utilities")
(autoload 'mevedel--ordered-completion-table "mevedel-utilities")
(autoload 'mevedel--transcript-org-mode "mevedel-utilities")
(autoload 'mevedel-diagnostic-message "mevedel-utilities")
(autoload 'mevedel-diagnostic-scope "mevedel-utilities")
(autoload 'mevedel-diagnostic-user-error "mevedel-utilities")
(autoload 'mevedel-diagnostic-warning "mevedel-utilities")

;; `mevedel-view'
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace-ensure-generated-state-ignored "mevedel-workspace" (workspace))
(declare-function mevedel-workspace-get-or-create "mevedel-workspace" (type id root name))
(defvar mevedel-workspace-additional-roots)
(defvar mevedel-workspace--registry)
(autoload 'mevedel-workspace "mevedel-workspace")
(autoload 'mevedel-workspace-ensure-generated-state-ignored
  "mevedel-workspace")
(autoload 'mevedel-workspace-get-or-create "mevedel-workspace")

;; `mevedel-workspace-identity'
(declare-function mevedel-workspace-identity-read "mevedel-workspace-identity" (root))
(autoload 'mevedel-workspace-identity-read "mevedel-workspace-identity")
(declare-function mevedel-workspace-identity-ensure "mevedel-workspace-identity" (root))
(autoload 'mevedel-workspace-identity-ensure "mevedel-workspace-identity")

;; `org'
(defvar org-agenda-file-menu-enabled)

;; `saveplace'
(defvar save-place-mode)

;; `so-long'
(defvar so-long--inhibited)
(defvar so-long-predicate)

;;
;;; Customization

(defcustom mevedel-file-history-max-snapshot-bytes (* 1024 1024)
  "Soft size cap (bytes) for individual pre-turn file checkpoints.

Larger content is persisted as an explicit checkpoint gap with a warning.
Defaults to 1 MB."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-max-age-days 30
  "Auto-cleanup threshold for old sessions, in days.

Sessions older than this are deleted by a sweep that runs when the
`mevedel' session chooser lists sessions, after a session opens, after
a root turn ends, and when Emacs exits.  Each workspace is swept at
most once a day per Emacs, and only an interactive Emacs sweeps from
session activity or exit.  Age comes from `:updated-at', or the sidecar
or session directory modification time when metadata cannot provide it;
a portable session's last lease renewal must also be older.  Sessions
with a live, publishing, mutating or transfer-reserved lease are skipped.
Legacy pre-lease control state is always refused.  So are the
`mevedel-session-keep-recent-count' most-recently-updated sessions.  A nil
value disables auto-cleanup entirely."
  :type '(choice (integer :tag "Days")
          (const :tag "Disabled" nil))
  :group 'mevedel)

(defcustom mevedel-session-keep-recent-count 3
  "Number of most-recently-updated sessions exempt from auto-cleanup.

The newest sessions of a workspace, counted by the same timestamp
auto-cleanup ages against, survive `mevedel-session-max-age-days'
regardless of age.  Without this floor a long absence would delete the
very session a returning user opens the chooser to resume: the chooser
sweeps expired sessions before listing them, and at that point the
session is not yet locked.  A nil value applies the age cap to every
session."
  :type '(choice (integer :tag "Sessions")
          (const :tag "No floor" nil))
  :group 'mevedel)

(defcustom mevedel-session-checkpoint-interval 30
  "Seconds between conversation checkpoints while a request is in flight.

Emacs auto-saves only after keyboard input, so a turn that runs
unattended, such as a Goal or a retained agent, would otherwise reach
disk only when it settles.  While any root or agent request is in
flight, modified conversations are checkpointed this often through the
same path as `auto-save-hook'.  A nil value leaves mid-turn checkpoints
to Emacs auto-save alone."
  :type '(choice (number :tag "Seconds")
          (const :tag "Only on Emacs auto-save" nil))
  :group 'mevedel)


;;
;;; Sidecar lifecycle

(defun mevedel-session-persistence-write-current-buffer-atomically (path)
  "Write the current buffer to PATH through a same-directory rename."
  (mevedel-session-control-fs-write-file
   (mevedel-session-control-fs-physical-path path)
   (buffer-substring-no-properties (point-min) (point-max))))

(defun mevedel-session-persistence--segment-text (session path)
  "Return SESSION's committed transcript text at PATH, or nil when absent.
Portable transcripts live in the publication, not at PATH itself."
  (when (mevedel-session-artifacts-artifact-present-p
         session (file-name-nondirectory path) t)
    ;; Published artifacts are literal bytes; transcripts are UTF-8.
    (decode-coding-string
     (mevedel-session-artifacts-read-artifact
      session (file-name-nondirectory path) t)
     'utf-8-unix)))

(defun mevedel-session-persistence--execution-successor-ids (text)
  "Return structured execution ids present in transcript TEXT."
  (when (and text (string-search ":execution-id" text))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (org-mode))
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
                 (mevedel-tool-render-data-blocks
                  (buffer-substring-no-properties (point-min) (point-max))))
          (when-let* ((stored (caddr block))
                      (begin (+ (point-min) (car block)))
                      (end (+ (point-min) (cadr block)))
                      ((mevedel-tool-render-data-call-range-p
                        stored begin end))
                      (data
                       (mevedel-tool-render-data-without-owner stored))
                      (id (plist-get data :execution-id)))
            (cl-pushnew id ids :test #'equal)))
        ids))))

(defconst mevedel-session-persistence--segment-backup-max-bytes
  (* 64 1024 1024)
  "Rotation size of `segment-repair-backups.el'; at most twice this is kept.")

(defun mevedel-session-persistence--back-up-segment (path text)
  "Append transcript PATH's TEXT to its session's repair backups.
Closed segments change only through repairs, and publication cleanup drops
the replaced copy within a few saves; this keeps it for diagnosis and
rollback.  A failed backup is reported and does not block the repair."
  (condition-case err
      (let ((print-escape-newlines t) (print-length nil) (print-level nil))
        (mevedel-session-control-fs-append-rotating
         (mevedel-session-control-fs-physical-path
          (file-name-concat (file-name-directory path) "segment-repair-backups.el"))
         (concat (prin1-to-string
                  (list :time (format-time-string "%FT%T%z")
                        :segment (file-name-nondirectory path)
                        :content (substring-no-properties text)))
                 "\n")
         mevedel-session-persistence--segment-backup-max-bytes))
    (error
     (mevedel-diagnostic-warning
      (mevedel-diagnostic-scope path) 'mevedel
      (format "Could not back up %s before repairing it: %s"
              (file-name-nondirectory path) (error-message-string err))))))

(defun mevedel-session-persistence--reconcile-lost-execution-file
    (path text &optional successor-execution-ids artifact-callback)
  "Mark stale running execution rows in transcript PATH with TEXT as lost.

When ARTIFACT-CALLBACK is non-nil, pass it the repaired replacement instead
of writing PATH directly."
  (when (and text (or (string-search ":execution-id" text)
                      (string-search ":live-execution-p" text)))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (org-mode))
      (mevedel-transcript-restore-properties)
      (let ((count
              (mevedel-tool-render-data-reconcile-lost-executions
              (current-buffer) successor-execution-ids)))
        (when (> count 0)
          (mevedel-session-persistence--back-up-segment path text)
          (mevedel-session-artifacts-stabilize-gptel-bounds)
          (if artifact-callback
              (funcall
               artifact-callback
               (list :path path
                     :content (buffer-string)
                     :coding 'utf-8-unix))
            (mevedel-session-persistence-write-current-buffer-atomically
             path)))
        count))))

(defun mevedel-session-persistence-reconcile-lost-execution-segments
    (session &optional exclude-path artifact-callback)
  "Repair stale execution rows in SESSION segments except EXCLUDE-PATH.

ARTIFACT-CALLBACK has the meaning described by
`mevedel-session-persistence--reconcile-lost-execution-file'."
  (let ((save-path (mevedel-session-save-path session))
        (successor-ids
         (and exclude-path
              (mevedel-session-persistence--execution-successor-ids
               (mevedel-session-persistence--segment-text
                session exclude-path))))
        (count 0))
    (cl-loop for segment downfrom (mevedel-session-current-segment session) to 1
             for path = (mevedel-session-artifacts-segment-path
                         save-path segment)
             for text = (unless (and exclude-path
                                     (string= (expand-file-name path)
                                              (expand-file-name exclude-path)))
                          (mevedel-session-persistence--segment-text session path))
             when text
             do (cl-incf
                 count
                 (or (mevedel-session-persistence--reconcile-lost-execution-file
                      path text successor-ids artifact-callback)
                     0))
             and do
             (dolist (id (mevedel-session-persistence--execution-successor-ids text))
               (cl-pushnew id successor-ids :test #'equal)))
    count))

(defun mevedel-session-persistence--flush-diagnostic-logs-now (session)
  "Retry SESSION diagnostics queued before or after materialization.

The four logs share one lease reservation.  Each append otherwise opened a
transaction of its own -- recovery refresh, lease renewal, ownership
reading, then a reservation renewing the lease on entry and committing it
on exit -- so a flush point paid that four times over for data nothing
reads live."
  (let ((flush
         (lambda ()
           (mevedel-telemetry-flush session)
           (when (fboundp 'mevedel-hooks-flush-log)
             (mevedel-hooks-flush-log session))
           (when (fboundp 'mevedel-tool-repair-flush-log)
             (mevedel-tool-repair-flush-log session))
           (when (fboundp 'mevedel-permission-log-flush)
             (mevedel-permission-log-flush session)))))
    ;; Only reserve when a log actually has something.  Each flush already
    ;; returns early on an empty queue, and reserving for four of those
    ;; would renew the lease -- target I/O, and a warning when the lease
    ;; cannot be renewed -- to write nothing at all.  The four queues are
    ;; struct slots, so asking costs nothing.
    (if (or (mevedel-session-telemetry-pending session)
            (mevedel-session-hook-log-pending session)
            (mevedel-session-repair-log-pending session)
            (mevedel-session-permission-log-pending session)
            (mevedel-session-publication-diagnostic-backlog-p session))
        (mevedel-session-publication-call-with-diagnostic-batch
         session flush)
      (funcall flush))))

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
          (run-at-time
           0 nil #'mevedel-transport-run-when-idle
           (list 'diagnostic-flush session) save-path
           (lambda ()
             (mevedel-session-persistence--flush-diagnostic-logs-now
              session))))
      (mevedel-session-persistence--flush-diagnostic-logs-now session))))

(defun mevedel-session-persistence-allocate-session-id (sessions-dir)
  "Return a fresh id absent from SESSIONS-DIR and live session buffers."
  (cl-loop repeat 33
           for candidate = (mevedel-session-generate-id)
           for path = (file-name-concat sessions-dir candidate)
           unless (or (file-exists-p path) (file-symlink-p path)
                      (cl-some
                       (lambda (buffer)
                         (when-let* ((session (buffer-local-value
                                              'mevedel--session buffer)))
                           (equal candidate (mevedel-session-session-id session))))
                       (buffer-list)))
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
  (or (mevedel-session-save-path session)
      (condition-case err
          (let* ((_disclosure
                  (when (mevedel-execution-target-remote-p
                         (mevedel-session-execution-target session))
                    (mevedel-session-durability-disclose session)))
                 (_workspace-identity
                  (progn
                    (mevedel-workspace-identity-ensure
                     (mevedel-workspace-root
                      (mevedel-session-workspace session)))))
                 (sessions-dir (mevedel-session-artifacts-sessions-dir
                                (mevedel-session-workspace session)))
                 (session-id
                  (or (mevedel-session-session-id session)
                      (error "Session has no identity")))
                 (save-path (file-name-as-directory
                             (file-name-concat sessions-dir session-id)))
                 (segment-path (mevedel-session-artifacts-segment-path
                                save-path 1))
                 (now (format-time-string "%FT%H-%M-%S")))
            (make-directory sessions-dir t)
            (make-directory save-path)
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
            (mevedel-workspace-ensure-generated-state-ignored
             (mevedel-session-workspace session))
            (with-current-buffer buffer
              (unless buffer-file-name
                (setq buffer-file-name segment-path))
              (mevedel-session-artifacts-disown-save-machinery))
            (mevedel-session-control-transfer-register-root-buffer
             session buffer)
            save-path)
        (error
         (mevedel-diagnostic-message (mevedel-diagnostic-scope session) "Shallow session materialization failed: %S" err)
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

(defun mevedel-session-persistence-write-sidecar-now (session buffer &optional prepared checkpoint token)
  "Best-effort sidecar rewrite for SESSION and BUFFER.

Only writes when the sidecar file already exists on disk -- i.e.
a full root snapshot has written `session.meta.el'.  Before that, the session
may be shallowly materialized (directory + lock + agents/ but no sidecar), so
this observational rewrite remains deferred to the next critical agent commit
or root autosave.  PREPARED admits the owning checkpoint's final sidecar.
CHECKPOINT captures a matching transcript already written by the caller.
TOKEN is the caller's gate token."
  (when (and (memq session (bound-and-true-p mevedel-session-artifacts--terminal-sessions))
             (not prepared))
    (user-error "Session checkpoint is being prepared"))
  (when (and session (mevedel-session-save-path session)
             (mevedel-session-codec-portable-authority-p session)
             (mevedel-session-artifacts-artifact-present-p
              session "session.meta.el"))
    (condition-case err
        (progn
          ;; Drain async work before the sidecar is captured.
          (mevedel-session-publication-barrier session token)
          (mevedel-session-durability-with-transaction
            (mevedel-session-artifacts-assert-mutation-authority session buffer)
            (mevedel-session-publication-publish
             session
             (list (mevedel-session-artifacts--sidecar-artifact
                    session buffer checkpoint))
             nil prepared token)))
      (error
       (mevedel-diagnostic-message (mevedel-diagnostic-scope session)
                                   "Sidecar rewrite failed: %S" err)
       nil)
      (:success t))))

(defun mevedel-session-persistence-save-agent-state (session &optional token)
  "Commit SESSION's agent state through its authoritative root buffer.
Keep root transcript fields at the last committed checkpoint.  Before the
first root snapshot, materialize the complete root rather than a bare sidecar.
TOKEN is the caller's gate token."
  (when-let* ((buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer)))
    (let ((mevedel-session-artifacts-require-agent-commit-p t))
      (when (if (and (mevedel-session-save-path session)
                     (mevedel-session-artifacts-artifact-present-p
                      session "session.meta.el" t))
                (mevedel-session-artifacts-publish-sidecar-state
                 session buffer token)
              (mevedel-session-artifacts-save session buffer nil t token))
        ;; The committed sidecar already carries the current registry,
        ;; so a pending observational save is absorbed.
        (mevedel-session-persistence-cancel-deferred-agent-save session)
        t))))

(defcustom mevedel-session-persistence-agent-save-debounce 2.0
  "Seconds to coalesce best-effort agent-state saves before publishing.
Observational agent persists -- activity transitions, explicit mailbox
discards -- fire in bursts.  Inside this window they collapse into
one sidecar-only save, and a synchronous acknowledged commit absorbs a
pending one entirely."
  :type 'number
  :group 'mevedel)

(defvar mevedel-session-persistence--deferred-agent-saves
  (make-hash-table :test #'eq)
  "Sessions with a pending debounced agent-state save, mapped to timers.")

(defun mevedel-session-persistence-save-agent-state-soon (session)
  "Schedule one coalesced best-effort sidecar save for SESSION.
Agent state, queued input, pause and recovery changes use it.  Unlike
`mevedel-session-persistence-save-agent-state', the deferred
save is not an acknowledged agent commit: it rewrites only the session
sidecar (the transcript segment is committed at settlement), may be
queued behind an active publication, and is cancelled outright when a
synchronous commit lands first."
  (when (and (mevedel-session-save-path session)
             (not (gethash session
                           mevedel-session-persistence--deferred-agent-saves)))
    (puthash session
             (run-at-time mevedel-session-persistence-agent-save-debounce nil
                          #'mevedel-session-persistence--deferred-agent-save
                          session)
             mevedel-session-persistence--deferred-agent-saves)))

(defun mevedel-session-persistence--deferred-agent-save (session)
  "Run the debounced best-effort agent-state save for SESSION."
  (if-let* ((save-path (mevedel-session-save-path session)))
      (mevedel-transport-run-when-idle
       (list 'agent-state-save session) save-path
       (lambda ()
         ;; Keep SESSION registered while transport work is deferred so
         ;; the exit hook can still flush it inline.
         (remhash session mevedel-session-persistence--deferred-agent-saves)
         (if (mevedel-session-publication-active-p session)
             ;; Timer callbacks perform no target I/O while a critical
             ;; publication is active; re-arm and try again after it.
             (mevedel-session-persistence-save-agent-state-soon session)
           (when-let* ((buffer (mevedel-session-root-buffer session))
                       ((buffer-live-p buffer)))
             (condition-case nil
                 (mevedel-session-artifacts-save-agent-registry
                  session buffer)
               (error nil))))))
    (remhash session mevedel-session-persistence--deferred-agent-saves)))

(defun mevedel-session-persistence-cancel-deferred-agent-save (session)
  "Cancel any pending debounced agent-state save for SESSION."
  (when-let* ((timer (gethash session
                              mevedel-session-persistence--deferred-agent-saves)))
    (cancel-timer timer)
    (remhash session mevedel-session-persistence--deferred-agent-saves))
  (mevedel-transport-cancel-pending (list 'agent-state-save session)))


;;
;;; First user message extraction

(defun mevedel-session-persistence-first-user-message (buffer)
  "Return a one-line preview of the first user prompt in BUFFER, or nil.

Use the same canonical prompt derivation as the live and restored prompt
index, excluding structural mailbox deliveries, metadata, summaries and
tool/reasoning scaffolding.  Keep the first non-empty line, truncated to
120 characters."
  (plist-get (car (mevedel-session-artifacts-collect-prompts buffer 120))
             :preview))


;;
;;; Buffer selection

(defun mevedel-session-persistence-notify-session-event
    (session event &rest args)
  "Send semantic lifecycle EVENT to SESSION's registered observers."
  (apply #'mevedel-session-control-transfer-notify session event args))

(defun mevedel-session-persistence-root-data-buffer-p (buffer)
  "Return non-nil when BUFFER is an explicitly registered session root.

Unassociated ordinary buffers remain eligible for low-level inspection, but
session buffers must cross the session-owned root registration seam."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((session (and (boundp 'mevedel--session)
                             mevedel--session)))
           (and (not mevedel--agent-invocation)
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
         (mevedel--agent-invocation nil)
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

(defun mevedel-session-persistence-lock-acquire
    (session-dir buffer-name &optional session)
  "Acquire SESSION-DIR's portable lease for BUFFER-NAME.
SESSION supplies the live owner.  Return nil when another client owns the
lease, permitting read-only inspection; legacy pre-lease state is refused."
  (mevedel-session-codec-authority-mode-for-path session-dir session)
  (when session
    (mevedel-session-durability-disclose session))
  (mevedel-session-durability-lease-acquire session-dir buffer-name session))

(defun mevedel-session-persistence-lock-release (session-dir &optional session token)
  "Release this client's portable lease for SESSION-DIR.
SESSION supplies the live owner; TOKEN is the caller's mutation gate token."
  (mevedel-session-codec-authority-mode-for-path session-dir session)
  (mevedel-session-durability-lease-release session-dir session token))

(defun mevedel-session-persistence-release-on-kill ()
  "Buffer-local `kill-buffer-hook' that releases session mutation authority.
Publication work is drained before the final save and lease release.  A root
buffer also stops new admissions.  An unresolved job keeps the session's
state and lease for explicit publication recovery."
  (when (and (boundp 'mevedel--session)
             mevedel--session)
    (when (mevedel-session-persistence--drain-for-close
           mevedel--session (current-buffer))
      (mevedel-session-persistence-autosave-buffer (current-buffer))
      (when-let* ((dir (mevedel-session-save-path mevedel--session)))
        (mevedel-journal-capture-seal-and-schedule mevedel--session (current-buffer) 'session-end)
        (mevedel-session-persistence--wait-async)
        (condition-case _
            (mevedel-session-persistence-lock-release dir mevedel--session)
          (error nil))))))

(defun mevedel-session-persistence--wait-async ()
  "Let already-running asynchronous control programs deliver their callbacks.
Closing and exit call it after their last write, before a lease is released."
  (when (featurep 'mevedel-session-control-fs)
    (mevedel-session-control-fs-wait-async)))

(defun mevedel-session-persistence--drain-temporarily (session)
  "Drain SESSION's publication work with admissions and wakes stopped.
The previous stopping state is restored on every exit, so a later veto leaves
the session usable.  Notifications retained while stopped are re-armed through
`mevedel-agent-control-reoffer-mail', which never offers inline, and only when
some were actually suppressed.  An unresolved job signals."
  (let ((was-stopping (mevedel-session-publication-stopping-p session)))
    (unwind-protect (mevedel-session-publication-stopping session)
      (setf (mevedel-session-publication-stopping-p session) was-stopping)
      (when (and (not was-stopping)
                 (mevedel-session-mail-notifications session))
        (mevedel-agent-control-reoffer-mail session)))))

(defun mevedel-session-persistence--gate-owner-p (session)
  "Return non-nil when SESSION can own asynchronous publication work.
Only materialized portable sessions publish through the session gate."
  (and session
       (mevedel-session-save-path session)
       (mevedel-session-codec-portable-authority-p session)))

(defun mevedel-session-persistence--drain-for-close (session buffer)
  "Drain SESSION's publication work for closing BUFFER.
The root buffer stops new admissions; other buffers only wait.  Return non-nil
when closing may proceed with its save and lease release.  An unresolved job
warns and returns nil, leaving explicit recovery or abandonment possible."
  (or (not (mevedel-session-persistence--gate-owner-p session))
      (let ((was-stopping (mevedel-session-publication-stopping-p session)))
        (condition-case err
            (progn
              (if (eq buffer (mevedel-session-root-buffer session))
                  (mevedel-session-publication-stopping session)
                (mevedel-session-publication-barrier session))
              (when (mevedel-session-pending-publication session)
                (user-error "Session has pending publication; retry or abandon it first"))
              t)
          (error
           ;; Recovery may admit work again, so admissions reopen.
           (setf (mevedel-session-publication-stopping-p session) was-stopping)
           (mevedel-diagnostic-warning (mevedel-diagnostic-scope session)
          'mevedel
          (format "Session publication is unresolved; not saving or releasing: %s"
                  (error-message-string err))
          :warning)
           nil)))))

(defun mevedel-session-persistence-allow-buffer-close-p ()
  "Return non-nil when closing the current session buffer may proceed.
A `kill-buffer-query-functions' member: it drains an owned root buffer's
publication work while teardown has not yet started, and refuses the close
with a message when a job stays unresolved, so explicit recovery remains
possible."
  (or (not (and (boundp 'mevedel--session) mevedel--session))
      (not (mevedel-session-persistence--gate-owner-p mevedel--session))
      (not (eq (current-buffer) (mevedel-session-root-buffer mevedel--session)))
      ;; The drain is the decision.  Another query may still veto the close,
      ;; so admissions reopen; the kill hook stops them for good.
      (condition-case err
          (progn
            (mevedel-session-persistence--drain-temporarily mevedel--session)
            t)
        (error
         (mevedel-diagnostic-message (mevedel-diagnostic-scope mevedel--session)
          (concat "Session publication is unresolved (%s); run "
                  "mevedel-session-publication-retry or "
                  "mevedel-session-publication-abandon before closing")
          (error-message-string err))
         nil))))


;;
;;; Automatic persistence

(defvar-local mevedel-session--save-failed nil
  "Non-nil when the most recent auto-save failed in this buffer.
Set by terminal and lifecycle auto-save handlers on any save error and
cleared on the next successful save.  Surfaced by
`mevedel-session-persistence-header-segment' so the user has a
visible cue in addition to the `display-warning'.")

(defun mevedel-session-persistence--note-autosave-failure (buffer err &optional scope)
  "Mark BUFFER's auto-save failed with ERR and warn using captured SCOPE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local mevedel-session--save-failed t)
      (force-mode-line-update)))
  (mevedel-diagnostic-warning (or scope (mevedel-diagnostic-scope buffer))
   'mevedel
   (format "Could not auto-save %s: %s"
           (if (buffer-live-p buffer) (buffer-name buffer) "buffer")
           (error-message-string err))
   :warning))

(defun mevedel-session-persistence-autosave-buffer (buffer &optional token)
  "Save BUFFER's owned conversation, containing and reporting save failures.
Retained agents use their transcript writer.  Root buffers use session
publication.  Views and read-only inspection buffers never write.  While the
session's publication gate is owned the save is deferred before anything is
captured, and the return value is nil.  TOKEN is the caller's gate token; a
deferred save runs with its own."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (bound-and-true-p mevedel--session)
                 (mevedel-session-workspace mevedel--session)
                 ;; Opening a conversation is not durable work.  Already
                 ;; saved sessions still checkpoint an intentionally empty
                 ;; transcript, and first-turn partial text is worth saving.
                 (or (mevedel-session-save-path mevedel--session)
                     (save-restriction (widen) (> (buffer-size) 0)))
                 (not (bound-and-true-p mevedel-session--read-only-mode))
                 (not (bound-and-true-p mevedel-session--inspection-buffer-p))
                 (or mevedel--agent-invocation
                     (eq buffer (mevedel-session-root-buffer mevedel--session))))
        (let ((scope (mevedel-diagnostic-scope mevedel--session)))
        (unless (and (mevedel-session-persistence--gate-owner-p mevedel--session)
                     (mevedel-session-publication-defer
                      mevedel--session
                      (lambda (prepared-token)
                        (mevedel-session-persistence-autosave-buffer
                         buffer prepared-token))
                      (lambda (err)
                        (mevedel-session-persistence--note-autosave-failure
                         buffer err scope))
                      token))
        (let ((inhibit-quit t))
          (condition-case err
              (let* ((agent mevedel--agent-invocation)
                     (portable (mevedel-session-codec-portable-authority-p
                                mevedel--session))
                     ;; A checkpoint needs a marker, so let the transcript
                     ;; writer include it instead of publishing twice.
                     (_ (when (and agent portable)
                          (setf (mevedel-agent-invocation-sidecar-dirty agent) t)))
                     (saved
                      (if agent
                          (mevedel-agent-conversation-save agent nil token)
                        (mevedel-session-artifacts-save
                         mevedel--session buffer nil nil token))))
                (unless saved
                  (error "Conversation was not saved"))
                ;; Agent writes can be retained publication batches awaiting
                ;; a sidecar commit.  A checkpoint must be visible on resume,
                ;; even when the root transcript has not changed.
                (when (and agent portable
                           (or (mevedel-session-publication-uncommitted-batches
                                mevedel--session)
                               (mevedel-session-publication-queue mevedel--session)
                               (mevedel-agent-invocation-sidecar-dirty agent)))
                  (let ((root (mevedel-session-root-buffer mevedel--session)))
                    (if (mevedel-session-artifacts-artifact-present-p
                         mevedel--session "session.meta.el" t)
                        (mevedel-session-artifacts-publish-sidecar-state
                         mevedel--session root token)
                      (unless (with-current-buffer root
                                (mevedel-session-artifacts-save
                                 mevedel--session root nil nil token))
                        (error "Agent checkpoint has no saved root")))))
                (setq-local mevedel-session--save-failed nil)
                (force-mode-line-update)
                saved)
            ((error quit)
             (mevedel-session-persistence--note-autosave-failure buffer err scope)
             nil)))))))))

(defvar mevedel-session-persistence--autosaves (make-hash-table :test #'eq)
  "Conversation buffers with a queued auto-save opportunity.")

(defvar-local mevedel-session--checkpoint-pending nil
  "Identity of this buffer's accepted background checkpoint, or nil.")

(defun mevedel-session-persistence-checkpoint-buffer (buffer)
  "Checkpoint BUFFER in the background when its authority permits it.
Only warm local portable root and agent conversations use an asynchronous
publication job.  Other authorities use the synchronous autosave writer.
Return `pending' for accepted work, nil for an ineligible or absorbed save.
Capture and the modified/failed/uncommitted recheck run only under the gate.
Failures retain frozen publication recovery and the existing save-failed cue.
Explicit saves and teardown keep using the synchronous writer and its barrier."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and mevedel--session
                 (mevedel-session-workspace mevedel--session)
                 (not (bound-and-true-p mevedel-session--read-only-mode))
                 (not (bound-and-true-p mevedel-session--inspection-buffer-p))
                 (or mevedel--agent-invocation
                     (eq buffer (mevedel-session-root-buffer mevedel--session)))
                 (not (mevedel-session-publication-stopping-p mevedel--session))
                 (not mevedel-session--checkpoint-pending)
                 (or (buffer-modified-p) mevedel-session--save-failed
                     (mevedel-session-publication-uncommitted-batches mevedel--session)))
        (if (not (mevedel-session-publication-async-eligible-p mevedel--session))
            (mevedel-session-persistence-autosave-buffer buffer)
          (let* ((session mevedel--session)
                 (scope (mevedel-diagnostic-scope session))
                 (identity (list buffer))
                 prepared fallback
                 (acknowledge
                  (lambda (success err)
                    (when (and (buffer-live-p buffer)
                               (eq identity (buffer-local-value
                                             'mevedel-session--checkpoint-pending buffer)))
                      (with-current-buffer buffer
                        (setq mevedel-session--checkpoint-pending nil)
                        (condition-case cleanup
                            (if success
                                (if fallback
                                    ;; The gate is released before ACK.  A
                                    ;; missing canonical agent transcript must
                                    ;; use its ordinary writer, not a successful
                                    ;; no-op that clears the failure cue.
                                    (mevedel-session-persistence-autosave-buffer buffer)
                                  (when prepared (funcall (plist-get prepared :finish)))
                                  (setq mevedel-session--save-failed nil)
                                  (force-mode-line-update))
                              (mevedel-session-persistence--note-autosave-failure
                               buffer (if (stringp err) (list 'error err) err) scope))
                          ((error quit)
                           (mevedel-session-persistence--note-autosave-failure
                            buffer cleanup scope))))))))
            (setq mevedel-session--checkpoint-pending identity)
            (condition-case err
                (mevedel-session-publication-submit
                 session
                 (lambda ()
                   (when (and (buffer-live-p buffer)
                              (eq session (buffer-local-value 'mevedel--session buffer)))
                     (with-current-buffer buffer
                       ;; A synchronous save ahead of this preparation can absorb
                       ;; it.  No transcript, sidecar or hooks were frozen yet.
                       (when (or (buffer-modified-p) mevedel-session--save-failed
                                 (mevedel-session-publication-uncommitted-batches session))
                         (mevedel-session-artifacts-recover-working-directory session)
                         (mevedel-session-artifacts-check-target-incarnation session buffer)
                         (setq prepared
                               (mevedel-session-artifacts-call-with-checkpoint
                                session
                                (lambda ()
                                  (if mevedel--agent-invocation
                                      (mevedel-agent-conversation-prepare-save
                                       mevedel--agent-invocation)
                                    (mevedel-session-artifacts-prepare-save
                                     session buffer nil nil t)))))
                         (setq fallback (null prepared))
                         (prog1 (plist-get prepared :artifacts)
                           ;; The job owns captured payload strings until staging.
                           ;; ACK retains only the finish/selection continuations.
                           (setf (plist-get prepared :artifacts) nil))))))
                 acknowledge nil t
                 (lambda (batch)
                   (if-let* ((select (plist-get prepared :select-staged)))
                       (funcall select batch)
                     batch)))
              ((error quit)
               (funcall acknowledge nil err)
               nil))))))))

(defun mevedel-session-persistence-autosave-soon (buffer)
  "Queue one transport-safe checkpoint of conversation BUFFER.
Repeated calls before it runs coalesce.  The checkpoint runs only if
BUFFER still has unsaved changes by then; settlement or an explicit save
can absorb it."
  (mevedel-transport-schedule-idle
   mevedel-session-persistence--autosaves buffer 'conversation-autosave
   (buffer-local-value 'default-directory buffer)
   (lambda ()
     ;; Recheck at execution, including failed saves whose transcript
     ;; write succeeded before their marker failed.
     (when (and (buffer-live-p buffer)
                (or (buffer-modified-p buffer)
                    (buffer-local-value 'mevedel-session--save-failed buffer)
                    (with-current-buffer buffer
                      (and (bound-and-true-p mevedel--session)
                           (mevedel-session-publication-uncommitted-batches
                            mevedel--session)))))
       (mevedel-session-persistence-checkpoint-buffer buffer)))))

(defun mevedel-session-persistence-autosave ()
  "Checkpoint modified data buffers during Emacs auto-save.
Queue one transport-safe opportunity per buffer, yielding between saves.
A failed write does not prevent other conversations from being saved.
Settlement and exit still persist synchronously.  The in-flight
checkpoint timer calls this too, because Emacs auto-saves only after
input and an unattended turn receives none."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (bound-and-true-p mevedel--session))
               (or (buffer-modified-p buffer)
                   (buffer-local-value 'mevedel-session--save-failed buffer)
                   (with-current-buffer buffer
                     (and (eq buffer (mevedel-session-root-buffer
                                      mevedel--session))
                          (mevedel-session-publication-uncommitted-batches
                           mevedel--session)))))
      (mevedel-session-persistence-autosave-soon buffer))))


;;;; In-flight checkpoints

(defvar mevedel-session-persistence--checkpoint-timer nil
  "Repeating timer that checkpoints conversations during requests, or nil.")

(defun mevedel-session-persistence--request-in-flight-p ()
  "Return non-nil when some mevedel buffer has a request in flight."
  (and (boundp 'mevedel--current-request)
       (cl-some (lambda (buffer)
                  (buffer-local-value 'mevedel--current-request buffer))
                (buffer-list))))

(defun mevedel-session-persistence--checkpoint-tick ()
  "Checkpoint modified conversations; stop once no request is in flight.
The stopping tick checkpoints too: changes deferred to these checkpoints,
such as completion breadcrumbs, may have landed after the last request
settled and saved only its own buffer."
  (unless (mevedel-session-persistence--request-in-flight-p)
    (mevedel-session-persistence-stop-checkpoints))
  (mevedel-session-persistence-autosave))

(defun mevedel-session-persistence-start-checkpoints ()
  "Checkpoint conversations periodically until no request is in flight.
Request admission calls this; repeated calls share one timer, which
stops itself on the first tick that finds every request settled."
  (when (and mevedel-session-checkpoint-interval
             (not (memq mevedel-session-persistence--checkpoint-timer
                        timer-list)))
    (setq mevedel-session-persistence--checkpoint-timer
          (run-at-time mevedel-session-checkpoint-interval
                       mevedel-session-checkpoint-interval
                       #'mevedel-session-persistence--checkpoint-tick))))

(defun mevedel-session-persistence-stop-checkpoints ()
  "Stop the in-flight checkpoint timer."
  (when (timerp mevedel-session-persistence--checkpoint-timer)
    (cancel-timer mevedel-session-persistence--checkpoint-timer))
  (setq mevedel-session-persistence--checkpoint-timer nil))

(defun mevedel-session-persistence-checkpoints-active-p ()
  "Return non-nil while in-flight checkpoints periodically save conversations."
  (and (timerp mevedel-session-persistence--checkpoint-timer)
       (memq mevedel-session-persistence--checkpoint-timer timer-list)
       t))

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
                  ((memq lease-state '(foreign expired lost contested))))
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

(defun mevedel-session-persistence-load-sidecar (path &optional scope)
  "Read a current-version sidecar plist from PATH.
Return nil when the sidecar is missing or unreadable.  Signal when a
readable sidecar has an unsupported version or obsolete shape.
SCOPE is the explicit diagnostic root when the caller knows its owner."
  (let* ((result (mevedel-session-persistence--classify-sidecar path))
         (diagnostic (plist-get result :diagnostic)))
    (pcase (plist-get result :status)
      ('current (plist-get result :sidecar))
      ((or 'missing 'unreadable)
       (mevedel-diagnostic-warning (or scope 'unavailable) 'mevedel (format "%s: %s" diagnostic path) :warning)
       nil)
      (_ (error "%s" diagnostic)))))

(defun mevedel-session-persistence--classify-sidecar-data (sidecar)
  "Classify already-read SIDECAR using the current persistence schema."
  (cond
   ((not (and (plistp sidecar) (keywordp (car sidecar))))
    (list :status 'unreadable :diagnostic "Invalid session sidecar"))
   ((not (equal (plist-get sidecar :version)
                mevedel-session-codec-format-version))
    (list :status 'unsupported
          :version (plist-get sidecar :version)
          :updated-at (plist-get sidecar :updated-at)
          :diagnostic (format "Unsupported session version: %s"
                              (or (plist-get sidecar :version) "missing"))))
   (t
    (condition-case err
        (progn
          (mevedel-session-codec-validate-current-sidecar sidecar)
          (list :status 'current :sidecar sidecar))
      (error
       (list :status 'obsolete
             :version (plist-get sidecar :version)
             :updated-at (plist-get sidecar :updated-at)
             :diagnostic (error-message-string err)))))))

(defun mevedel-session-persistence--classify-sidecar (path)
  "Return the compatibility classification for the sidecar at PATH.

The returned plist has `:status' equal to `current', `missing', `unreadable',
`unsupported', or `obsolete'.  Current results carry `:sidecar'; readable
incompatible results retain their observed version, update stamp, and exact
validation diagnostic."
  (if (not (file-exists-p path))
      (list :status 'missing :diagnostic "Sidecar is missing")
    (condition-case err
        (mevedel-session-persistence--classify-sidecar-data
         (mevedel-session-codec-read path))
      (error
       (list :status 'unreadable
             :diagnostic (error-message-string err))))))

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
    (mevedel-diagnostic-message (mevedel-diagnostic-scope buf) "%s"
             (or reason
                 "session opened read-only; another client holds the lease"))))

(defun mevedel-session-persistence-find-live-buffer (session-id buf-name)
  "Return the named live root buffer for SESSION-ID, or nil.

The canonical session buffer name is the durable locator.  Persistence does
not search unrelated buffers or infer roles from view-local variables."
  (or (mevedel-session-control-transfer-root-buffer-for-id session-id)
      (let ((candidate (get-buffer buf-name)))
        (when (and (mevedel-session-persistence-root-data-buffer-p candidate)
                   (with-current-buffer candidate
                     (or (not (bound-and-true-p mevedel--session))
                         (equal (mevedel-session-session-id mevedel--session)
                                session-id))))
          candidate))))

(defun mevedel-session-persistence--hydrate-restored-buffer
    (buf session workspace segment-path acquired additional-roots
         lifecycle-source &optional artifact-callback checkpoint-callback)
  "Hydrate fresh restore buffer BUF and return its durable repair count.
SESSION and WORKSPACE are planted before mevedel restores persisted state.
SEGMENT-PATH is reconciled when ACQUIRED owns the session lock.  Additional
workspace roots and LIFECYCLE-SOURCE restore the saved session environment.
ARTIFACT-CALLBACK collects remote transcript replacements for one batch.
CHECKPOINT-CALLBACK receives the candidate frozen with repaired root bytes."
  (let ((ptc-insertions 0)
        (ptc-consumptions 0)
        checkpoint-captured)
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
      (mevedel-transcript-restore-gptel-state)
      (when acquired
        (mevedel-session-artifacts-check-target-incarnation session buf)
        (pcase-let ((`(,inserted . ,consumed)
                     (mevedel-ptc-checkpoint-reconcile session)))
          (setq ptc-insertions inserted
                ptc-consumptions consumed))
        (let ((transcript-repairs
               (+ ptc-insertions
                  (mevedel-tool-render-data-reconcile-lost-executions buf))))
          (when (or (> transcript-repairs 0) (> ptc-consumptions 0))
            ;; PTC insertions and lost-execution repairs both change transcript
            ;; length; the repair publication needs current offsets.  Rewrite
            ;; only bounds: chat setup has not installed the advice that keeps
            ;; `gptel--save-state' from writing request config to the drawer.
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (when checkpoint-callback
              (funcall checkpoint-callback
                       (mevedel-session-checkpoint-capture
                        session "/root" (mevedel-session-artifacts--live-sidecar session buf)))
              (setq checkpoint-captured t))
            (when artifact-callback
                (funcall
                 artifact-callback
                 (list :path segment-path
                       :content (buffer-string)
                       :coding (or buffer-file-coding-system 'utf-8-unix))))
            (set-buffer-modified-p nil))))
      (unless acquired
        (mevedel-session-persistence-apply-read-only-mode buf))
      ;; Freeze reconciled metadata before initialization hooks advance it.
      (when (and acquired checkpoint-callback (not checkpoint-captured))
        (funcall checkpoint-callback
                 (mevedel-session-checkpoint-capture
                  session "/root" (mevedel-session-artifacts--live-sidecar session buf))))
      (mevedel--chat-buffer-init-common
       buf workspace (or lifecycle-source "resume") (not acquired))
      (+ ptc-consumptions
         (prog1
             (mevedel-agent-persistence-restore-tree
              session buf (bound-and-true-p mevedel-session--read-only-mode))
           (mevedel-session-artifacts-load-instructions session))))))

(defun mevedel-session-persistence--finish-restored-buffer
    (buf session live persist-repairs-p &optional repair-artifacts repair-checkpoint)
  "Finish restoring BUF for SESSION and return BUF.
LIVE means BUF was already initialized.  When PERSIST-REPAIRS-P is non-nil,
write repaired sidecar state before rendering the companion view.
REPAIR-ARTIFACTS are portable transcript replacements published before the
sidecar commit marker.  REPAIR-CHECKPOINT was frozen with the root repair."
  (with-current-buffer buf
    (when persist-repairs-p
      (let ((published-p nil))
        (condition-case err
            (progn
              (mevedel-session-publication-publish
               session
               (append
                repair-artifacts
                (list (list
                       :path (mevedel-session-artifacts-sidecar-path
                              (mevedel-session-save-path session))
                       :content
                       (mevedel-session-artifacts-printed-value
                        (mevedel-session-artifacts-build-sidecar
                         session buf repair-checkpoint))
                       :checkpoint repair-checkpoint
                       :commit-marker t))))
              (setq published-p t))
          (error
           (unless (mevedel-session-pending-publication session)
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

(defconst mevedel-session-persistence--pre-lease-message
  "Session %s predates file sessions on the portable lease; close it and run scripts/migrate-sessions.el on it"
  "Format for an unmigrated PID-lock file session, given its directory.")

(defun mevedel-session-persistence--pre-lease-p (session-dir)
  "Return non-nil when SESSION-DIR is a file session from before the lease.
It has a PID lock or a plain sidecar, but no lease.  Only failure paths ask."
  (and (not (mevedel-session-control-fs-path-exists-p
             (file-name-concat session-dir ".lease")))
       (or (mevedel-session-control-fs-path-exists-p
            (file-name-concat session-dir ".lock"))
           (mevedel-session-control-fs-path-exists-p
            (mevedel-session-artifacts-sidecar-path session-dir)))))

(defun mevedel-session-persistence-restore
    (session-dir &optional lifecycle-source session-override workspace)
  "Restore the chat buffer for the session at SESSION-DIR.

Loads the exact current-version sidecar, opens the live segment file in a
buffer named
per `mevedel-session-buffer-name', enables `org-mode' and
`gptel-mode' without gptel's Org-state restore, restores text-property
bounds through mevedel's transcript path, hydrates the session struct on
the buffer, then runs `mevedel--chat-buffer-init-common'.

Returns the chat buffer.  If a buffer for this session is already
alive (matched by session-id), switches to it instead of re-loading.

LIFECYCLE-SOURCE defaults to \"resume\".  SESSION-OVERRIDE, when non-nil,
is the already-projected in-memory session used by a newly published fork.
WORKSPACE is the currently opened workspace authority.

Tasks are deserialized from the sidecar.  Touched-files and
mentions-shown reset to empty hash tables on load."
  (let* ((_authority-check
          (condition-case err
              (mevedel-session-codec-authority-mode-for-path
               session-dir session-override
               (and workspace
                    (mevedel-session-authority-mode-for-workspace workspace)))
            (error
             (if (mevedel-session-persistence--pre-lease-p session-dir)
                 (user-error mevedel-session-persistence--pre-lease-message
                             session-dir)
               (signal (car err) (cdr err))))))
         (publication
          (or (mevedel-session-publication-read session-dir)
              (if (mevedel-session-persistence--pre-lease-p session-dir)
                  (user-error mevedel-session-persistence--pre-lease-message
                              session-dir)
                (user-error "Portable session has no committed publication: %s"
                            session-dir))))
         (sidecar-path (plist-get publication :sidecar))
         (sidecar
          (mevedel-session-persistence-load-sidecar
           sidecar-path (mevedel-diagnostic-scope (or session-override workspace))))
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
                               (user-error
                                "Portable session has no valid published sidecar: %s"
                                session-dir)))
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
           ;; Fence before opening artifacts.  A foreign holder permits only
           ;; inspection; a live local buffer already owns its session.
           (acquired
           (unless live
              (if (and session-override
                       (mevedel-session-durability-lease-owned-p session))
                  t
                (mevedel-session-persistence-lock-acquire
                 session-dir buf-name session)))))
      (let ((repair-artifacts nil)
            repair-callback
            buf
            (setup-done nil))
        (unwind-protect
            (progn
              (setq repair-callback
                    (and acquired
                         (not live)
                         (lambda (artifact)
                           (push artifact repair-artifacts))))
              ;; Recheck the captured head after fencing, before restoring bytes.
              (when (and acquired (not live))
                (mevedel-session-durability-call-with-reserved-lease
                 session
                 (lambda ()
                   (let ((current (mevedel-session-publication-read session-dir)))
                     (unless (and current
                                  (equal (plist-get publication :head)
                                         (plist-get current :head)))
                       (user-error
                        "Session state changed while acquiring its lease; retry restore"))
                     (setf (mevedel-session-publication session) current)))))
              (let* ((segment-n (mevedel-session-current-segment session))
                     (segment-path
                      (mevedel-session-artifacts-segment-path
                       session-dir segment-n))
                     repair-checkpoint
                     (agent-repairs 0))
                (setq
                 buf
                 (or
                  live
                  (condition-case err
                      (mevedel-session-artifacts-find-artifact-noselect
                       session (file-name-nondirectory segment-path)
                       (not acquired))
                    (file-missing
                     (user-error "Published session segment is unavailable: %s (%s)"
                                 segment-path (error-message-string err))))))
                (unless (and buf (buffer-live-p buf))
                  (user-error "Published session segment is unavailable: %s"
                              segment-path))
                (when (and acquired (not live))
                  (mevedel-session-persistence-reconcile-lost-execution-segments
                   session segment-path repair-callback))
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
                         additional-roots lifecycle-source repair-callback
                         (lambda (candidate) (setq repair-checkpoint candidate)))))
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
                 (nreverse repair-artifacts) repair-checkpoint)
                ;; A crash may leave generations no settled turn reclaimed.
                ;; Schedule their scan without delaying the restored view.
                (when (and acquired (not live))
                  (mevedel-session-collection-schedule session))
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
  "Classified session summaries keyed by sidecar path and file fingerprint.")

(defun mevedel-session-persistence--sidecar-summary (sidecar)
  "Return picker-relevant fields extracted from current SIDECAR."
  (list :session-id (plist-get sidecar :session-id)
        :session-name (plist-get sidecar :session-name)
        :preset-name (plist-get sidecar :preset-name)
        :note-conversation (plist-get sidecar :note-conversation)
        :workspace (plist-get sidecar :workspace)
        :created-at (plist-get sidecar :created-at)
        :updated-at (plist-get sidecar :updated-at)
        :current-segment (plist-get sidecar :current-segment)
        :attached-artifacts (plist-get sidecar :attached-artifacts)
        :total-turn-count (plist-get sidecar :total-turn-count)
        :first-user-message (plist-get sidecar :first-user-message)
        :latest-user-message (plist-get sidecar :latest-user-message)
        :chats (mevedel-session-artifacts-chats
                (plist-get sidecar :chapters)
                (plist-get sidecar :prompt-index)
                (or (plist-get sidecar :current-segment) 1)
                (plist-get sidecar :session-name))
        :fork-point-ids
        (cl-loop
         for segment in (plist-get sidecar :prompt-index)
         append
         (cl-loop
          for prompt in (cdr segment)
          for id = (plist-get prompt :fork-point-id)
          when (stringp id)
          collect id))
        :working-directory (plist-get sidecar :working-directory)
        :forked-from-session-id (plist-get sidecar :forked-from-session-id)
        :fork-type (plist-get sidecar :fork-type)
        :forked-from-fork-point-id
        (plist-get sidecar :forked-from-fork-point-id)
        :worktree-source-root (plist-get sidecar :worktree-source-root)
        :dedicated-artifact (plist-get sidecar :dedicated-artifact)
        :worktree-directory (plist-get sidecar :worktree-directory)
        :worktree-branch (plist-get sidecar :worktree-branch)
        :worktree-base-commit (plist-get sidecar :worktree-base-commit)))

(defun mevedel-session-persistence--read-summary (sidecar-path)
  "Return a classified picker summary for SIDECAR-PATH.

Cheap by design: only fields displayed in the picker (annotations,
sort key, and rejection details) are retained.  The full sidecar plist is
left on disk until restore actually reads it.  An unchanged file fingerprint
reuses its previous classification."
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
      (let* ((classified
              (mevedel-session-persistence--classify-sidecar path))
             (sidecar (plist-get classified :sidecar))
             (summary
              (if sidecar
                  (list :status 'current
                        :summary
                        (mevedel-session-persistence--sidecar-summary sidecar))
                classified)))
        (when fingerprint
          (puthash path (cons fingerprint summary)
                   mevedel-session-persistence--summary-cache))
        summary))))

(defvar mevedel-session-persistence--list-sessions-cache
  (make-hash-table :test #'equal)
  "Last live session enumeration per workspace root, as (t . RESULT).

Enumerating a workspace costs several target round trips per session,
so decorative consumers reuse the newest live listing instead of
paying that on every redraw.  Every live enumeration refreshes it.")

(defun mevedel-session-persistence--control-artifacts (entries)
  "Return control probe results for ENTRIES, in one program.

Each candidate is probed for the three artifacts that make a directory a
session -- the obsolete PID lock, the portable lease, and the sidecar --
and every probe of every candidate travels in a single target program.
Probing them one at a time is one target process each, which is what made
enumerating a workspace cost hundreds of round trips.

A candidate proved free of a PID lock is recorded through
`mevedel-session-durability-note-no-pid-lock', so the assertions the
publication read repeats per entry reuse this program's proof instead of
paying for their own.  Directories that do carry one are left alone: their
own assertion reports the authority violation at its canonical call site."
  (let* ((names '(".lock" ".lease" "session.meta.el"))
         (results
          (mevedel-session-control-fs-run-program
           (cl-loop for entry in entries
                    append
                    (mapcar (lambda (name)
                              (list :op 'path-exists-p
                                    :path (file-name-concat entry name)
                                    :optional t))
                            names))))
         observed)
    (dolist (entry entries (nreverse observed))
      ;; Results arrive in the order the operations were emitted: one per
      ;; name, in `names' order, per entry.
      (let ((entry-results (list (pop results) (pop results) (pop results))))
        (when (eq 'absent (plist-get (car entry-results) :status))
          (mevedel-session-durability-note-no-pid-lock entry))
        (push (cons entry entry-results) observed)))))

(defun mevedel-session-persistence--lease-listings (entries)
  "Return an alist of ENTRIES to their observed lease-directory listings.

Every candidate's lease directory is listed in one target program.
`mevedel-session-durability--generation-paths' accepts such an observation
in place of its own round trip, so the publication read below spends no
listing of its own; an empty or absent directory is recorded as `none',
which that helper distinguishes from no observation at all.  An entry whose
listing failed is left out, and lists for itself."
  (let ((results
         (mevedel-session-control-fs-run-program
          (mapcar (lambda (entry)
                    (list :op 'list-directory
                          :path (file-name-concat entry ".lease")
                          :optional t))
                  entries)))
        alist)
    (dolist (entry entries (nreverse alist))
      (let* ((result (pop results))
             (status (plist-get result :status)))
        (when (memq status '(ok absent))
          (push (cons entry (or (plist-get result :value) 'none)) alist))))))

(defun mevedel-session-persistence--observed-update-time
    (updated-at sidecar-path save-path)
  "Return when the session at SAVE-PATH was last updated, or nil.
UPDATED-AT is authoritative when it parses as a session timestamp.
Otherwise the sidecar at SIDECAR-PATH lends its modification time, then the
directory itself."
  (or (mevedel-session-persistence-parse-iso-time updated-at)
      (file-attribute-modification-time
       (or (and sidecar-path (file-attributes sidecar-path))
           (file-attributes save-path)))))

(defun mevedel-session-persistence--incompatible-entry
    (save-path authority-mode status &optional publication sidecar-path)
  "Return an incompatible discovery record for SAVE-PATH.
AUTHORITY-MODE, STATUS, PUBLICATION, and SIDECAR-PATH describe the failed
cold discovery without making it resumable."
  (list :kind 'incompatible
        :save-path (file-name-as-directory save-path)
        :authority-mode authority-mode
        :status (plist-get status :status)
        :diagnostic (plist-get status :diagnostic)
        :version (plist-get status :version)
        :updated-at
        ;; A remote stat that fails leaves the row undated rather than
        ;; unlisted.
        (when-let* ((time (condition-case nil
                              (mevedel-session-persistence--observed-update-time
                               (plist-get status :updated-at)
                               sidecar-path save-path)
                            (file-error nil))))
          (format-time-string "%FT%H-%M-%S" time))
        :publication publication))

(defun mevedel-session-persistence--discover-entry
    (entry authority-mode control-results listing &optional observation)
  "Return the discovery record for session directory ENTRY.
AUTHORITY-MODE is the workspace's portable profile.  CONTROL-RESULTS and
LISTING are this enumeration's legacy-control and lease observations.
OBSERVATION is its batched publication/sidecar read.  Failed or contradictory
controls signal, so the caller keeps an undiscoverable, uninspectable row."
  (mevedel-session-codec-validate-authority-mode authority-mode)
  (when-let* ((failed
               (seq-find (lambda (result)
                           (not (memq (plist-get result :status) '(ok absent))))
                         control-results)))
    (mevedel-session-control-fs-program-value failed))
  (if (and (eq 'absent (plist-get (nth 1 control-results) :status))
           (or (eq 'ok (plist-get (nth 0 control-results) :status))
               (eq 'ok (plist-get (nth 2 control-results) :status))))
      (mevedel-session-persistence--incompatible-entry
       entry authority-mode
       (list :status 'unsupported
             :diagnostic (format mevedel-session-persistence--pre-lease-message entry)))
    (when-let* ((err (plist-get observation :error)))
      (signal (car err) (cdr err)))
    (when (seq-find (lambda (result) (eq (plist-get result :status) 'ok))
                   control-results)
      (mevedel-session-codec-authority-mode-for-path entry nil authority-mode))
    (let* ((publication (if observation (plist-get observation :publication)
                          (mevedel-session-publication-read entry nil listing)))
           (sidecar (plist-get publication :sidecar))
           (status
            (if sidecar
                (if observation
                    (let* ((classified
                            (mevedel-session-persistence--classify-sidecar-data
                             (plist-get observation :sidecar-data)))
                           (data (plist-get classified :sidecar)))
                      (if data
                          (list :status 'current :summary
                                (mevedel-session-persistence--sidecar-summary data))
                        classified))
                  (mevedel-session-persistence--read-summary sidecar))
              (list :status 'missing
                    :diagnostic "Portable session has no committed publication"))))
      (if (eq (plist-get status :status) 'current)
          (list :save-path (file-name-as-directory entry)
                :summary (plist-get status :summary) :publication publication)
        (mevedel-session-persistence--incompatible-entry
         entry authority-mode status publication sidecar)))))

(defun mevedel-session-persistence--enumerate-sessions
    (workspace &optional cached)
  "Return compatible and incompatible persisted sessions for WORKSPACE.

The result is `(:sessions LIST :incompatible LIST)'.  When CACHED is non-nil,
reuse the last live enumeration when one exists."
  (let ((root (mevedel-workspace-root workspace)))
    (or (and cached
             (cdr (gethash
                   root
                   mevedel-session-persistence--list-sessions-cache)))
        (let* ((sessions-dir
                (mevedel-session-artifacts-sessions-dir workspace))
               (authority-mode
                (mevedel-session-authority-mode-for-workspace workspace))
               sessions
               incompatible)
          ;; One enumeration is one transaction: the PID-lock proofs and the
          ;; target clock reading that every entry's publication read repeats
          ;; are paid once for the whole listing.
          (mevedel-session-durability-with-transaction
            (when (file-directory-p sessions-dir)
              (let* ((entries
                      (seq-filter
                       #'file-directory-p
                       (directory-files
                        sessions-dir t
                        "\\`\\(?:[^.]\\|\\.mevedel-save-as-\\)")))
                     (controls
                      (and entries
                           (mevedel-session-persistence--control-artifacts
                            entries)))
                     (listings
                      (and entries
                           (mevedel-session-persistence--lease-listings
                            entries)))
                     (observations
                      (when entries
                        ;; Keep each read batch within the native archive
                        ;; carrier's bound; all proofs belong to this listing.
                        (cl-loop for batch on entries by (lambda (rest) (nthcdr 32 rest))
                                 append (mevedel-session-publication-read-batch
                                         (seq-take batch 32) listings)))))
                (dolist (entry entries)
                  (let ((record
                         (condition-case err
                             (mevedel-session-persistence--discover-entry
                              entry authority-mode
                              (cdr (assoc entry controls))
                              (cdr (assoc entry listings))
                              (cdr (assoc entry observations)))
                           ;; Discovery itself failed, so not even the
                           ;; transcript can be trusted for inspection.
                           (error
                            (mevedel-session-persistence--incompatible-entry
                             entry authority-mode
                             (list :status 'undiscoverable
                                   :diagnostic (error-message-string err)))))))
                    (if (eq (plist-get record :kind) 'incompatible)
                        (push record incompatible)
                      (push record sessions)))))))
          (setq sessions
                (sort sessions
                      :key (lambda (item)
                             (or (plist-get (plist-get item :summary)
                                            :updated-at)
                                 ""))
                      :lessp #'string-greaterp)
                incompatible
                (sort incompatible
                      :key (lambda (item) (or (plist-get item :updated-at) ""))
                      :lessp #'string-greaterp))
          (let ((result (list :sessions sessions
                              :incompatible incompatible)))
            (puthash root (cons t result)
                     mevedel-session-persistence--list-sessions-cache)
            result)))))

(defun mevedel-session-persistence-list-sessions (workspace &optional cached)
  "Return a list of `(:save-path :summary)' plists for WORKSPACE's sessions.

Sorted by `:updated-at' descending.  Incompatible sessions remain absent from
this compatible-only interface.  Portable sessions are listed only when their
lease names a valid immutable publication; fixed portable sidecars are ignored.

When CACHED is non-nil, reuse this process's last live enumeration for
WORKSPACE when one exists.  Only decorations tolerant of a listing as
old as the last picker, resume, or fork should pass it; anything that
decides authority or names sessions to the user enumerates live."
  (plist-get (mevedel-session-persistence--enumerate-sessions workspace cached)
             :sessions))

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
         (relative (mevedel-session-rewind-format-relative-time updated))
         (name     (or (plist-get s :session-name) "?"))
         (preview  (or (plist-get s :latest-user-message)
                       (plist-get s :first-user-message)
                       ""))
         (chats    (plist-get s :chats))
         (earlier
          (mapcar (lambda (chat) (plist-get chat :title))
                  (reverse (seq-remove (lambda (chat) (plist-get chat :current-p))
                                       chats))))
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
    (format "%-12s  %s  [%d chat%s, %d turns]%s%s%s  %s"
            relative name (max 1 (length chats))
            (if (> (length chats) 1) "s" "")
            turns
            ;; Newest first, bounded: the preview after it is the row's end.
            (if earlier
                (concat "  ↩ "
                        (truncate-string-to-width
                         (string-join earlier " · ") 60 nil nil "…"))
              "")
            (or worktree "")
            (if detail (format "  (%s)" detail) "")
            preview)))

(defun mevedel-session-persistence--entry-live-buffer (entry)
  "Return the live root buffer already open for session ENTRY, or nil."
  (when-let* ((session-id (plist-get (plist-get entry :summary) :session-id)))
    (mevedel-session-control-transfer-root-buffer-for-id session-id)))

(defun mevedel-session-persistence--cleanup-reasons (assessment)
  "Describe the cleanup blockers in ASSESSMENT.
Pending journal captures are omitted: the journal settles them unattended."
  (mapconcat
   (lambda (reason)
     (pcase reason
       ('publishing "interrupted publication")
       ('unsettled-mutation "unresolved mutation")
       ('recovery "recovery data retained")
       ('live-owner "live owner")
       ('transfer-reserved "control transfer reserved")
       (_ "unreadable control data")))
   (remq 'journal-pins (plist-get assessment :reasons)) ", "))

(defun mevedel-session-persistence--entry-authority (workspace entry)
  "Return `(:action LABEL :detail TEXT :held BOOL)' for WORKSPACE session ENTRY.

The verb names the outcome of choosing this row, which the lease decides:
whether anyone is writing the session right now, not whether its files
happen to be local or reached over TRAMP.  DETAIL names the machine
involved, which is the part a verb cannot carry.  HELD says whether some
client is writing this session now, which is a narrower question than
whether the row resumes: an expired lease offers a takeover precisely
because nobody is writing it any more.

One assessment shares the lease and retention probes; individual reasons do
not trigger another authority read."
  (ignore workspace)
  (cond
   ((when-let* ((buffer (mevedel-session-persistence--entry-live-buffer entry)))
      (list :action "Switch"
            :detail (if (buffer-local-value 'mevedel-session--read-only-mode
                                            buffer)
                        "already open here, read-only"
                      "already open here")
            :held t)))
   (t
    (let* ((status (mevedel-session-durability-cleanup-assessment
                    (plist-get entry :save-path)))
           (host (plist-get status :host))
           (elsewhere (and host (not (equal host (system-name))) host)))
      (cond
       ((and (plist-get status :recoverable)
             ;; Pending captures alone are routine; the journal drains them on its own.
             (remq 'journal-pins (plist-get status :reasons)))
        (list :action "Recover"
              :detail (concat "cleanup blocked: "
                              (mevedel-session-persistence--cleanup-reasons status))
              :held nil :cleanup status))
       ((eq 'unknown (plist-get status :state))
        (list :action "Inspect" :detail "cleanup blocked: unreadable control data"
              :held nil :cleanup status))
       (t
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
                 :held nil)))))))))


(defun mevedel-session-persistence--entry-action (workspace entry)
  "Return the entry action label for WORKSPACE session ENTRY."
  (plist-get (mevedel-session-persistence--entry-authority workspace entry)
             :action))

(defun mevedel-session-persistence--size-incompatible (entries)
  "Return incompatible ENTRIES extended with their measured directory sizes.
ENTRIES themselves are cached listing state and stay untouched.  The size is
a decorative column, so a measurement that fails as a whole leaves every row
at `size unavailable' rather than hiding the chooser."
  (condition-case nil
      (cl-mapcar
       (lambda (entry result)
         (if (eq (plist-get result :status) 'ok)
             (append entry (list :size (plist-get result :value)))
           entry))
       entries
       (mevedel-session-control-fs-tree-sizes
        (mapcar (lambda (entry) (plist-get entry :save-path)) entries)))
    (error entries)))

(defun mevedel-session-persistence--incompatible-reason (entry)
  "Return the human-readable incompatibility reason for ENTRY."
  (if (eq (plist-get entry :status) 'unsupported)
      (format "unsupported %s"
              (or (plist-get entry :version) "version missing"))
    (format "%s: %s" (plist-get entry :status) (plist-get entry :diagnostic))))

(defun mevedel-session-persistence--format-incompatible-candidate (entry)
  "Return a `completing-read' display string for incompatible ENTRY."
  (format "%-12s  %-16s  %s  %s"
          (mevedel-session-rewind-format-relative-time
           (plist-get entry :updated-at))
          (if-let* ((size (plist-get entry :size)))
              (file-size-human-readable size)
            "size unavailable")
          (mevedel-session-persistence--incompatible-reason entry)
          (plist-get entry :save-path)))

(defun mevedel-session-persistence--note-p (sidecar)
  "Return non-nil when SIDECAR describes an original note conversation."
  (and (or (plist-get sidecar :note-conversation)
           (eq (plist-get sidecar :preset-name) 'mevedel-note))
       (not (plist-get sidecar :forked-from-session-id))))

(defun mevedel-session-persistence--dedicated-p (workspace sidecar)
  "Return non-nil when SIDECAR is a note or artifact conversation in WORKSPACE.
Original note sessions are always hidden from ordinary chat selection.
An artifact session becomes ordinary when its artifact no longer names it."
  (or (mevedel-session-persistence--note-p sidecar)
      (when-let* ((id (plist-get sidecar :dedicated-artifact)))
        (mevedel-artifact-store-dedicated-p
         workspace (plist-get sidecar :session-id) id))))

(defun mevedel-session-persistence-without-dedicated (workspace entries)
  "Return listing ENTRIES of WORKSPACE without artifact conversations.
An artifact's dedicated session is reached from its artifact, not listed
among the user's chats."
  (cl-remove-if (lambda (entry)
                  (mevedel-session-persistence--dedicated-p
                   workspace (plist-get entry :summary)))
                entries))

(defvar mevedel-session-persistence--shared-files-accepted-p nil
  "Non-nil after accepting the shared-project-files advisory in this Emacs.
This acknowledges a general risk, not authority over another session's
lease or files.  Refusing or quitting the prompt leaves it unacknowledged.")

(defun mevedel-session-persistence-choose-entry (workspace)
  "Choose a persisted WORKSPACE session or return `new'.

Return nil when no persisted sessions exist.  Choosing an incompatible row
returns `(:action inspect :buffer BUFFER)', where BUFFER shows authoritative
transcript bytes without becoming a resumable session.  Other rows return
the restored chat buffer and follow the session's authority: an unheld
session resumes, one already open in this Emacs is switched to, one another
client holds is joined read-only and follows the owner from there, and an
expired lease is taken over.  Starting a new session alongside an active
writer requires accepting the shared-files advisory once per Emacs process.
Recovery-blocked sessions offer Recover, Discard and Cancel.  Discard refreshes
the chooser; it never implicitly starts a replacement session."
  (let ((result 'refresh) refreshed)
    (while (eq result 'refresh)
      (setq result (mevedel-session-persistence--choose-entry-once workspace refreshed)
            refreshed t))
    result))

(defun mevedel-session-persistence--choose-entry-once (workspace refreshed)
  "Choose one WORKSPACE entry, or return `refresh' after a discard.
REFRESHED keeps an empty chooser open after deleting its last session."
  ;; Expired sessions and locks left behind by dead Emacsen are swept before
  ;; listing, so the chooser never offers a row that exists only because
  ;; nothing has cleaned up after a previous invocation.
  (mevedel-session-persistence-cleanup-expired workspace)
  (let* ((enumeration
          (mevedel-session-persistence--enumerate-sessions workspace))
         (sessions (mevedel-session-persistence-without-dedicated
                    workspace (plist-get enumeration :sessions)))
         (incompatible
          (mevedel-session-persistence--size-incompatible
           (plist-get enumeration :incompatible))))
    (when (or sessions incompatible refreshed)
      (let* ((new-label "Start new session")
             (held-p nil)
             (session-candidates
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
                    (format
                     "%-10s %s" action
                     (mevedel-session-persistence--format-session-candidate
                      entry (plist-get authority :detail)))
                    (if (plist-get authority :cleanup)
                        (append entry (list :cleanup (plist-get authority :cleanup)))
                      entry))))
               sessions))
             (incompatible-candidates
              (mapcar
               (lambda (entry)
                 (cons
                  (format
                   "%-10s %s" "Inspect"
                   (mevedel-session-persistence--format-incompatible-candidate
                    entry))
                  entry))
               incompatible))
             (candidates
              (append session-candidates incompatible-candidates))
             (choices (cons new-label (mapcar #'car candidates)))
             (choice
              (completing-read
               (if incompatible
                   (format "Mevedel session (%d cannot resume): "
                           (length incompatible))
                 "Mevedel session: ")
               (mevedel--ordered-completion-table
                choices 'mevedel-session-entry)
               nil t nil nil (car choices))))
        (if (equal choice new-label)
            (progn
              (when (and held-p
                         (not mevedel-session-persistence--shared-files-accepted-p))
                (unless
                    (yes-or-no-p
                     (concat
                      "Another session writer is active. Independent sessions "
                      "can race over project files; use a Worktree Fork for "
                      "isolation. Start anyway? "))
                  (mevedel-diagnostic-user-error (mevedel-diagnostic-scope workspace) "New session was not started"))
                (setq mevedel-session-persistence--shared-files-accepted-p t))
              'new)
          (let ((entry (cdr (assoc choice candidates))))
            (if (eq (plist-get entry :kind) 'incompatible)
                (if (eq (plist-get entry :status) 'undiscoverable)
                    (mevedel-diagnostic-user-error (mevedel-diagnostic-scope workspace) "Session transcript is not safely inspectable: %s"
						   (plist-get entry :diagnostic))
                  (list
                   :action 'inspect
                   :buffer
                   (mevedel-session-artifacts-inspect-cold-session
                    (plist-get entry :save-path)
                    (plist-get entry :publication) (mevedel-diagnostic-scope workspace))))
              (let* ((assessment (plist-get entry :cleanup))
                     (action (when (plist-get assessment :recoverable)
                               (completing-read
                                "Retained session: "
                                '("Recover" "Discard" "Cancel") nil t nil nil "Recover"))))
                (cond
                 ((equal action "Cancel") (signal 'quit nil))
                 ((equal action "Discard")
                  (if (mevedel-session-persistence-discard workspace entry)
                      'refresh
                    (signal 'quit nil)))
                 ((eq 'unknown (plist-get assessment :state))
                  (list :action 'inspect :buffer
                        (mevedel-session-artifacts-inspect-cold-session
                         (plist-get entry :save-path)
                         (plist-get entry :publication)
                         (mevedel-diagnostic-scope workspace))))
                 (t
                  (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
                    (let ((buffer (mevedel-session-persistence-restore
                                   (plist-get entry :save-path) nil nil workspace))
                          (recovery (plist-get assessment :recovery)))
                      (when (equal action "Recover")
                        (message "Recovery opened; %s. %s"
                                 (mevedel-session-persistence--cleanup-reasons assessment)
                                 (cond
                                  ((and (listp recovery) (plist-get recovery :directory))
                                   (format "Inspect recovery data at %s before explicitly abandoning it."
                                           (plist-get recovery :directory)))
                                  ((memq 'unsettled-mutation (plist-get assessment :reasons))
                                   "Verify previous processes stopped before M-x mevedel-retry-target-readiness.")
                                  (t "Review the last committed transcript; prior client-local staging may be unavailable."))))
                      buffer))))))))))))

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
          (mevedel--ordered-completion-table
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
           (t (mevedel-diagnostic-user-error (mevedel-diagnostic-scope (current-buffer)) "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session data-buf))
         (scope (mevedel-diagnostic-scope (or session data-buf))))
    (unless session
      (mevedel-diagnostic-user-error (mevedel-diagnostic-scope data-buf) "Active buffer has no mevedel session"))
    (mevedel-with-diagnostic-errors scope
      (when arg
        (mevedel-engine-assert-local-history session "Save As"))
      (mevedel-session-artifacts-assert-mutation-authority session data-buf))
    (when arg
      (when (mevedel-agent-control-active-turn-p session)
        (mevedel-diagnostic-user-error (mevedel-diagnostic-scope session) "Interrupt active agent turns before save-as"))
      ;; Queued input would otherwise be delivered from both copies.
      (when (mevedel-session-pending-input-p session)
        (mevedel-diagnostic-user-error (mevedel-diagnostic-scope session)
         "Resolve pending input in the Pending Inputs cockpit or clear it with C-c C-q before save-as")))
    (mevedel-with-diagnostic-errors scope
      (cond
       (arg
        (mevedel-session-persistence--save-as session data-buf))
       (t
        (mevedel-session-artifacts-save session data-buf nil t)
        (message "Session saved."))))))

(defun mevedel-session-persistence--save-as (session data-buf)
  "Save SESSION as an independent child under a fresh id.
Called from `mevedel-save-session' with a prefix arg.  Copy committed logical
artifacts and retained execution output, excluding unfinished spools.
Repoint DATA-BUF at the child after it commits."
  ;; Force materialization so the child has a durable parent.
  (unless (mevedel-session-save-path session)
    (mevedel-session-artifacts-save session data-buf nil t))
  (let* ((old-save-path (mevedel-session-save-path session))
         (new-name (read-string
                    "Save session as (new name): "
                    (mevedel-session-name session)))
         (display-name (mevedel-session-naming-normalize new-name))
         (parent-dir (file-name-directory
                      (directory-file-name old-save-path)))
         (new-id (mevedel-session-persistence-allocate-session-id parent-dir))
         (new-save-path (file-name-as-directory
                         (file-name-concat parent-dir new-id))))
    ;; Publish the parent completely before deriving the child.
    (with-current-buffer data-buf (mevedel-session-naming-cancel))
    (mevedel-session-artifacts-save session data-buf)
    (mevedel-session-save-as-run
     session data-buf display-name new-id new-save-path)
    (message "Session saved as %s" display-name)
    new-save-path))


;;
;;; Auto-cleanup

(defvar mevedel-session-persistence--cleanup-throttle
  (make-hash-table :test #'equal)
  "Workspace keys mapped to the `float-time' of their last expiry sweep.

Keyed on `(WORKSPACE-TYPE . WORKSPACE-ID)'.  In memory only, so a new
Emacs sweeps at its first opportunity.")

(defvar mevedel-session-persistence--cleanup-interval (* 24 60 60)
  "Least seconds between two expiry sweeps of one workspace.
Ages are counted in days, so sweeping more often finds nothing new,
while a long-running Emacs must still sweep again.")

(defvar mevedel-session-persistence--cleanups (make-hash-table :test #'equal)
  "Workspace keys with a queued expiry sweep.")

(defun mevedel-session-persistence--cleanup-key (workspace)
  "Return WORKSPACE's expiry-sweep throttle key."
  (cons (mevedel-workspace-type workspace)
        (mevedel-workspace-id workspace)))

(defun mevedel-session-persistence--cleanup-due-p (workspace)
  "Return non-nil when WORKSPACE has not been swept within the interval."
  (let ((last (gethash (mevedel-session-persistence--cleanup-key workspace)
                       mevedel-session-persistence--cleanup-throttle)))
    (or (null last)
        (>= (- (float-time) last)
            mevedel-session-persistence--cleanup-interval))))

(defun mevedel-session-persistence-schedule-cleanup (workspace)
  "Offer WORKSPACE an expiry sweep once Emacs is idle, when one is due.

Session activity calls this so a long-running Emacs keeps expiring old
sessions without waiting for the chooser or for exit.  The sweep runs
after the current command, when the workspace's transport is idle and
no input is pending.  A batch Emacs never sweeps from activity, for the
same reason it does not sweep at exit.  Return the queued timer, or
nil when nothing was queued."
  (when (and workspace
             mevedel-session-max-age-days
             (not noninteractive)
             (mevedel-session-persistence--cleanup-due-p workspace))
    (mevedel-transport-schedule-idle
     mevedel-session-persistence--cleanups
     (mevedel-session-persistence--cleanup-key workspace)
     'session-cleanup
     (mevedel-session-artifacts-sessions-dir workspace)
     (lambda ()
       (condition-case err
           (mevedel-session-persistence-cleanup-expired workspace)
         (error
          (mevedel-diagnostic-message (mevedel-diagnostic-scope workspace) "Session cleanup failed: %s"
                   (error-message-string err))))))))

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

(defun mevedel-session-persistence--discard-identity (workspace entry &optional session)
  "Validate WORKSPACE's selected ENTRY and return its cold SESSION identity.
When SESSION is non-nil, recheck its target without accepting replacement."
  (unless (eq 'portable (mevedel-session-authority-mode-for-workspace workspace))
    (user-error "Explicit recovery discard requires a portable project session"))
  (let* ((directory (file-name-as-directory
                     (mevedel-session-control-fs-physical-path
                      (plist-get entry :save-path))))
         (sessions-directory (file-name-as-directory
                              (mevedel-session-control-fs-physical-path
                               (mevedel-session-artifacts-sessions-dir workspace)))))
    (when (mevedel-session-control-fs-async-busy-p directory)
      (user-error "Session has a pending local control program; retry when it settles"))
    (unless (equal sessions-directory
                   (file-name-directory (directory-file-name directory)))
      (user-error "Session does not belong to this workspace's session directory"))
    (when (mevedel-session-persistence--entry-live-buffer entry)
      (user-error "Session is open in this Emacs; close it before discarding"))
    (let* ((publication (mevedel-session-publication-read directory))
           (sidecar (and publication
                         (mevedel-session-persistence-load-sidecar
                          (plist-get publication :sidecar)
                          (mevedel-diagnostic-scope workspace))))
           (identity (mevedel-workspace-identity-read (mevedel-workspace-root workspace))))
      (unless (and sidecar identity
                   (eq 'portable (plist-get sidecar :authority-mode))
                   (equal identity (plist-get (plist-get sidecar :workspace) :workspace-id))
                   (equal (plist-get sidecar :session-id)
                          (plist-get (plist-get entry :summary) :session-id))
                   (equal (plist-get publication :head)
                          (plist-get (plist-get entry :publication) :head)))
        (user-error "Session identity or publication changed; select it again"))
      (let ((target (if session (mevedel-session-execution-target session)
                      (mevedel-execution-target-create (mevedel-workspace-root workspace)))))
        (unless session
          (mevedel-execution-target-restore-incarnation
           target (plist-get sidecar :target-incarnation)))
        (if (and (not session) (mevedel-execution-target-remote-p target))
            (let ((readiness (mevedel-execution-target-probe target t 'off)))
              (unless (eq 'ready (plist-get readiness :status))
                (user-error "Execution target is unavailable: %s"
                            (or (plist-get readiness :error) (plist-get readiness :reason)))))
          (mevedel-execution-target-observe-incarnation target))
        (when (mevedel-execution-target-incarnation-changed-p target)
          (user-error "Execution target changed; refusing session discard"))
        (or session
            (mevedel-session--create
             :workspace workspace :authority-mode 'portable :execution-target target
             :session-id (plist-get sidecar :session-id)
             :name (plist-get sidecar :session-name) :save-path directory
             :publication publication))))))

(defun mevedel-session-persistence-discard (workspace entry)
  "Confirm and discard WORKSPACE's retained session ENTRY without restoring it.
Refuse changed identities and live owners.  Captures and recovery are settled
through their owners before deletion; failure may retain approved omissions."
  (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
    (let* ((assessment (mevedel-session-durability-cleanup-assessment
			(plist-get entry :save-path)))
           (session (mevedel-session-persistence--discard-identity workspace entry))
           (directory (mevedel-session-save-path session)))
      (unless (memq (plist-get assessment :state) '(available expired))
	(user-error "Session cannot be discarded: %s"
                    (mevedel-session-persistence--cleanup-reasons assessment)))
      (when (yes-or-no-p
             (format (concat "Discard session %s (%s) permanently, including its transcript, "
                             "recovery data and pending journal captures? Approved capture/recovery "
                             "actions may persist if deletion fails. Project-file effects remain. "
                             "Confirm the previous client and its child writers have stopped: ")
                     (mevedel-session-name session)
                     (or (and (plist-get assessment :reasons)
                              (mevedel-session-persistence--cleanup-reasons assessment))
			 "saved session data")))
	(condition-case err
            (mevedel-session-durability-discard-session
             session
             (lambda ()
               (mevedel-session-persistence--discard-identity workspace entry session)
               (dolist (proof (cdr (mevedel-journal-pins-observe directory)))
		 (when (eq 'verify (plist-get proof :op))
                   (let* ((id (file-name-base (plist-get proof :path)))
                          (capture (mevedel-journal-capture--read workspace id)))
                     (unless (and capture
                                  (equal (mevedel-session-session-id session)
					 (plist-get (plist-get capture :metadata) :session)))
                       (user-error "Journal capture %s is unreadable or belongs to another session" id))
                     (mevedel-journal-process-discard workspace id directory))))
               (when (mevedel-session-recovery-read directory)
		 (mevedel-session-recovery--abandon session))
               (when (plist-get (mevedel-session-lease session) :unsettled-mutation)
		 (unless (mevedel-session-durability-set-unsettled-mutation session nil)
                   (user-error "Could not acknowledge unresolved mutation")))
               (mevedel-session-persistence--discard-identity workspace entry session)))
          (error (user-error "Session retained; approved recovery/capture actions may have completed: %s"
                             (error-message-string err))))))))

(cl-defun mevedel-session-persistence--delete-saved
    (workspace save-path max-age-seconds
               &optional (note-session-ids 'unread))
  "Delete saved session SAVE-PATH unless something may still hold it.
Journal pins always keep it.  Deletion uses
`mevedel-session-durability-delete-abandoned' with MAX-AGE-SECONDS.
Original note conversations
require proof that WORKSPACE no longer references them.  NOTE-SESSION-IDS
is the sweep's reference snapshot, or `unread' to obtain it here.
Return non-nil only when the directory was deleted."
  (and (not (mevedel-journal-pins-present-p save-path))
       (let ((sidecar (condition-case nil
                          (let* ((publication
                                  (mevedel-session-publication-read save-path))
                                 (data (and publication
                                            (mevedel-session-codec-read
                                             (plist-get publication :sidecar)))))
                            (and (listp data) data))
                        (error nil))))
         (not
          (when (or (mevedel-session-persistence--note-p sidecar)
                    (not (and (plist-member sidecar :preset-name)
                              (symbolp (plist-get sidecar :preset-name)))))
            (when (eq note-session-ids 'unread)
              (require 'mevedel-persistence)
              (setq note-session-ids
                    (mevedel-instruction-store-note-session-ids workspace)))
            (or (eq note-session-ids t)
                (member (plist-get sidecar :session-id) note-session-ids)
                (member (file-name-nondirectory (directory-file-name save-path))
                        note-session-ids)))))
       (mevedel-session-durability-delete-abandoned
        save-path max-age-seconds)))

(defun mevedel-session-persistence-delete (workspace save-path)
  "Delete WORKSPACE's saved session at SAVE-PATH now.
The safety rules of `mevedel-session-persistence-cleanup-expired' apply
without its age cap: a session a client may still hold, or whose journal
captures are pending, is kept.  Referenced note conversations and uncertain
note stores are kept too.  Return non-nil only when it was deleted."
  (mevedel-session-persistence--delete-saved
   workspace save-path 0))

(defun mevedel-session-persistence-cleanup-expired (workspace &optional force)
  "Delete sessions older than `mevedel-session-max-age-days'.

Scans session directories independently of resume compatibility.  Uses
`:updated-at' when available, otherwise the sidecar or directory modification
time.

The `mevedel-session-keep-recent-count' most-recently-updated sessions are
never deleted regardless of age.  Original note conversations do not count
toward that floor, and are eligible only after their notes leave the store;
an uncertain reference check retains them.  Pending journal capture pins prevent
deletion.  Sessions open in this Emacs are skipped; the others use
`mevedel-session-durability-delete-abandoned', which also requires the last
lease renewal to be older than the cap.  Throttled to at most one sweep
per `(workspace-type . workspace-id)' within
`mevedel-session-persistence--cleanup-interval'; when FORCE is non-nil
the throttle is bypassed.

Returns the number of sessions deleted, or nil when the cap is nil or the
workspace was swept too recently."
  (let ((sessions-dir
         (mevedel-session-artifacts-sessions-dir workspace)))
    (when mevedel-session-max-age-days
      (let ((ws-key (mevedel-session-persistence--cleanup-key workspace)))
      (when (or force (mevedel-session-persistence--cleanup-due-p workspace))
        (puthash ws-key (float-time)
                 mevedel-session-persistence--cleanup-throttle)
        (let ((threshold-secs (* mevedel-session-max-age-days 24 60 60))
              (now            (float-time))
              (deleted        0)
              (candidates     nil)
              (note-candidates nil)
              (note-session-ids 'unread))
          (dolist (save-path
                   (and (file-directory-p sessions-dir)
                        (directory-files sessions-dir t "\\`[^.]")))
            (when (file-directory-p save-path)
              (let* ((publication
                      (condition-case nil
                          (mevedel-session-publication-read save-path)
                        (error nil)))
                     (sidecar-path (plist-get publication :sidecar))
                     (sidecar
                      (condition-case nil
                          (let ((data (and sidecar-path
                                           (mevedel-session-codec-read sidecar-path))))
                            (and (listp data) data))
                        (error nil)))
                     (parsed-time
                      (mevedel-session-persistence--observed-update-time
                       (plist-get sidecar :updated-at)
                       sidecar-path save-path)))
                (let ((candidate (list save-path
                                       (and parsed-time (float-time parsed-time))
                                       (plist-get sidecar :session-id))))
                  (cond
                   ((or (mevedel-session-persistence--note-p sidecar)
                        (not (and (plist-member sidecar :preset-name)
                                  (symbolp (plist-get sidecar :preset-name)))))
                    (when (eq note-session-ids 'unread)
                      (require 'mevedel-persistence)
                      (setq note-session-ids
                            (mevedel-instruction-store-note-session-ids workspace)))
                    (unless (or (eq note-session-ids t)
                                (member (plist-get sidecar :session-id) note-session-ids)
                                (member (file-name-nondirectory (directory-file-name save-path))
                                        note-session-ids))
                      (if (mevedel-session-persistence--note-p sidecar)
                          (push candidate note-candidates)
                        (push candidate candidates))))
                   ((not (mevedel-session-persistence--dedicated-p workspace sidecar))
                    (push candidate candidates)))))))
          ;; The newest sessions are exempt regardless of age: the chooser
          ;; sweeps before any lock is taken, so without this floor a
          ;; 30-day absence would delete the session the user came back to
          ;; resume.  A session without a readable timestamp sorts oldest,
          ;; so it never displaces a dated one from the floor.
          (setq candidates
                (sort candidates
                      (lambda (a b) (> (or (cadr a) 0) (or (cadr b) 0)))))
          (when mevedel-session-keep-recent-count
            (setq candidates
                  (nthcdr mevedel-session-keep-recent-count candidates)))
          ;; Note conversations never consume the ordinary retention floor.
          (pcase-dolist (`(,save-path ,updated-secs ,session-id)
                        (append note-candidates candidates))
            (when (and updated-secs
                       (> (- now updated-secs) threshold-secs)
                       ;; A session open here may outlive its lease after
                       ;; suspend.
                       (not (and session-id
                                 (mevedel-session-control-transfer-root-buffer-for-id
                                  session-id)))
                       ;; One unreadable or contested session must not end
                       ;; the sweep.
                       (condition-case nil
                           (mevedel-session-persistence--delete-saved
                            workspace save-path threshold-secs note-session-ids)
                         (error nil)))
              (cl-incf deleted)))
          (when (> deleted 0)
            (message "Cleaned up %d expired session%s"
                     deleted (if (= deleted 1) "" "s")))
          deleted))))))


;;
;;; Hook plumbing

;;; Exit persistence

;; Turn settlement saves success, error, and abort before request teardown,
;; while request-local file-history checkpoints are still available.

(defun mevedel-session-persistence--allow-emacs-exit-p ()
  "Return non-nil when every live session may exit safely."
  (let* ((unresolved nil)
         (scope 'unavailable)
         (blocker
         (cl-some
          (lambda (buffer)
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (and (boundp 'mevedel--session)
                              mevedel--session)
                     (setq scope (mevedel-diagnostic-scope mevedel--session))
                     (cond
                      ;; Queued and in-flight async publication drains here,
                      ;; leaving the session usable if another query vetoes.
                      ((and (mevedel-session-persistence--gate-owner-p
                             mevedel--session)
                            (mevedel-session-publication-busy-p
                             mevedel--session)
                            (condition-case err
                                (progn
                                  (mevedel-session-persistence--drain-temporarily
                                   mevedel--session)
                                  nil)
                              (error (setq unresolved
                                           (error-message-string err))
                                     t)))
                       'publication-job)
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
      ('publication-job
       (mevedel-diagnostic-message scope
        (concat
         "Session publication is unresolved (%s); run "
         "mevedel-session-publication-retry or "
         "mevedel-session-publication-abandon first")
        unresolved)
       nil)
      ('publication
       (mevedel-diagnostic-message scope
        (concat
         "Session publication is pending; run "
         "mevedel-session-publication-retry or "
         "mevedel-session-publication-abandon first"))
       nil)
      ('mutation
       (mevedel-diagnostic-message scope
        (concat
         "Target mutation is unsettled; stop live executions or run "
         "mevedel-retry-target-readiness to acknowledge it first"))
       nil)
      (_ t))))

(defun mevedel-session-persistence--stop-publications ()
  "Stop admissions and drain publication jobs of every live session.
Return the sessions whose job stayed unresolved; each is warned about once.
Their state and leases are left for explicit publication recovery."
  (let (sessions blocked)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (when-let* ((session (buffer-local-value 'mevedel--session buffer)))
          (cl-pushnew session sessions :test #'eq))))
    (maphash (lambda (session _) (cl-pushnew session sessions :test #'eq))
             mevedel-session-persistence--deferred-agent-saves)
    (dolist (session sessions)
      (when (mevedel-session-persistence--gate-owner-p session)
        (condition-case err
            (mevedel-session-publication-stopping session)
          (error
           (push session blocked)
           (mevedel-diagnostic-warning (mevedel-diagnostic-scope session)
            'mevedel
            (format "Session publication is unresolved at exit; not saving or releasing: %s"
                    (error-message-string err))
            :warning)))))
    blocked))

(defun mevedel-session-persistence--kill-emacs-hook ()
  "Save sessions, clean expired state, and release locks on Emacs exit.

Runs unconditionally so that locks don't outlive the Emacs process
that wrote them.  Best-effort: individual errors are swallowed so one
bad buffer can't block exit.  Publication jobs are drained and their
admissions stopped before execution teardown.  A session whose job stays
unresolved is neither saved, flushed nor released, so explicit recovery
remains possible; execution teardown still runs."
  (let* ((mevedel-journal-process--inhibit-scheduling t)
        (mevedel-memory-decision--inhibit-recovery t)
        (mevedel-memory-pass--inhibit-scheduling t)
        (inhibit-quit t)
        (blocked (mevedel-session-persistence--stop-publications)))
    (mevedel-transport-cancel-idle
     mevedel-session-persistence--autosaves 'conversation-autosave)
    (when (featurep 'mevedel-journal-process)
      (ignore-errors (mevedel-journal-process-stop-all)))
    (when (featurep 'mevedel-memory-decision)
      (ignore-errors (mevedel-memory-decision-stop-recovery)))
    (when (featurep 'mevedel-memory-pass)
      (ignore-errors (mevedel-memory-pass-stop-all)))
    (when (fboundp 'mevedel-execution-teardown-all)
      (ignore-errors (mevedel-execution-teardown-all)))
    ;; Agent buffers are deliberately rejected by the root writer.  Flush
    ;; them through their owner before saving sidecars or releasing leases.
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   mevedel--agent-invocation))
        (unless (memq (buffer-local-value 'mevedel--session buffer) blocked)
          (mevedel-session-persistence-autosave-buffer buffer))))
    ;; A debounced agent-state save left in its window would be lost, and
    ;; registry mutations do not mark the root buffer modified, so the
    ;; modified-only save loop below cannot cover them.
    (maphash (lambda (session timer)
               (cancel-timer timer)
               (unless (memq session blocked)
                 (mevedel-session-persistence-autosave-buffer
                  (mevedel-session-root-buffer session))))
             mevedel-session-persistence--deferred-agent-saves)
    (clrhash mevedel-session-persistence--deferred-agent-saves)
    (let (lock-dirs)
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and (boundp 'mevedel--session)
                       mevedel--session
                       (not mevedel--agent-invocation)
                       (not (memq mevedel--session blocked)))
              (when (or (buffer-modified-p)
                        mevedel-session--save-failed
                        (mevedel-session-publication-uncommitted-batches
                         mevedel--session))
                (mevedel-session-persistence-autosave-buffer
                 (mevedel-session-persistence-authoritative-buffer buf)))
              ;; Seal only the completed checkpoint, even if the exit save
              ;; above included an unfinished response.  Exit starts no model.
              (ignore-errors
                (mevedel-journal-capture-seal-and-schedule
                 mevedel--session buf 'session-end))
              ;; An unmodified buffer still owes its queued diagnostics: a
              ;; deferred remote flush never fires once Emacs is exiting,
              ;; and an asynchronous append would die with it.
              (ignore-errors
                (let ((mevedel-session-control-fs--async-wait t))
                  (mevedel-session-persistence--flush-diagnostic-logs-now
                   mevedel--session)))
              (when-let* ((dir (mevedel-session-save-path mevedel--session)))
                (cl-pushnew dir lock-dirs :test #'equal))))))
      ;; Keep live locks through cleanup so an exit-save failure cannot expose
      ;; an old session directory for deletion.  A batch Emacs (a script or
      ;; test subprocess) only registered workspaces incidentally, often its
      ;; working directory, and must not sweep the user's sessions.
      (when (and (not noninteractive)
                 (boundp 'mevedel-workspace--registry)
                 (hash-table-p mevedel-workspace--registry))
        (maphash
         (lambda (_ workspace)
           ;; Like activation, never open a connection here: C-g is
           ;; inhibited, so an unreachable host would hang the exit.
           (let ((root (mevedel-workspace-root workspace)))
             (unless (and (stringp root) (file-remote-p root)
                          (not (file-remote-p root nil t)))
               (ignore-errors
                 (mevedel-session-persistence-cleanup-expired workspace)))))
         mevedel-workspace--registry))
      (mevedel-session-persistence--wait-async)
      (dolist (dir lock-dirs)
        (condition-case _
            (mevedel-session-persistence-lock-release dir)
          (error nil))))))

;; Install at file-load time so exit persistence runs even when the user
;; never called `mevedel-install' this Emacs (e.g. running `mevedel' to
;; resume is the only command invoked).  Duplicate adds are no-ops by `add-hook'.
(add-hook 'kill-emacs-query-functions
          #'mevedel-session-persistence--allow-emacs-exit-p)
(add-hook 'auto-save-hook #'mevedel-session-persistence-autosave)
(add-hook 'kill-emacs-hook #'mevedel-session-persistence--kill-emacs-hook)


(provide 'mevedel-session-persistence)

;;; mevedel-session-persistence.el ends here
