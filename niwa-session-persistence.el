;;; niwa-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Session persistence facade for lifecycle, mid-turn checkpoints, resume,
;; listing, locking, and stale-session cleanup.  Sidecar coding belongs to `niwa-session-codec';
;; paths, artifacts, snapshots, and segment writes belong to
;; `niwa-session-artifacts'; restore plans and the Rewind transaction belong
;; to `niwa-session-rewind'; and Fork projection, publication, Worktree
;; restoration belong to `niwa-session-fork'; session naming belongs to
;; `niwa-session-naming'.

;;; Code:

(require 'niwa-session-artifacts)
(require 'niwa-session-codec)
(require 'niwa-session-durability)
(require 'niwa-telemetry)
(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-agents)
  (require 'niwa-structs))

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `gptel'
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--preset)
(defvar gptel-display-buffer-action)
(defvar gptel-system-prompt)

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-turn-p "niwa-agent-control" (session))
(declare-function niwa-agent-control-reoffer-mail "niwa-agent-control" (session))
(declare-function niwa-agent-control-restore-lost-monitors "niwa-agent-control" (session monitors))
(autoload 'niwa-agent-control-active-turn-p "niwa-agent-control")
(autoload 'niwa-agent-control-reoffer-mail "niwa-agent-control")
(autoload 'niwa-agent-control-restore-lost-monitors "niwa-agent-control")

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-prepare-save
                  "niwa-agent-conversation" (invocation))
(autoload 'niwa-agent-conversation-prepare-save "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-save "niwa-agent-conversation")

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-restore-tree "niwa-agent-persistence" (session root-buffer readonly-p))
(autoload 'niwa-agent-persistence-restore-tree "niwa-agent-persistence")

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-dedicated-p
                  "niwa-artifact-store" (workspace session-id id))
(autoload 'niwa-artifact-store-dedicated-p "niwa-artifact-store")

;; `niwa-chat'
(declare-function niwa--chat-buffer-disable-org-element-cache "niwa-chat" nil)
(declare-function niwa--chat-buffer-init-common "niwa-chat" (buf workspace source &optional inspection-p))
(declare-function niwa--normalize-session-directory "niwa-chat" (directory workspace))
(autoload 'niwa--chat-buffer-disable-org-element-cache "niwa-chat")
(autoload 'niwa--chat-buffer-init-common "niwa-chat")
(autoload 'niwa--normalize-session-directory "niwa-chat")

;; `niwa-engine'
(declare-function niwa-engine-assert-local-history
                  "niwa-engine" (session operation &optional backend root-only))
(autoload 'niwa-engine-assert-local-history "niwa-engine")

;; `niwa-execution'
(declare-function niwa-execution-teardown-all "niwa-execution" nil)
(declare-function niwa-execution-unsettled-mutation-p "niwa-execution" (session))
(autoload 'niwa-execution-unsettled-mutation-p "niwa-execution")

;; `niwa-execution-target'
(declare-function niwa-execution-target-create "niwa-execution-target" (directory))
(declare-function niwa-execution-target-incarnation-changed-p "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-observe-incarnation "niwa-execution-target" (target))
(declare-function niwa-execution-target-probe "niwa-execution-target" (target &optional refresh sandbox-mode))
(declare-function niwa-execution-target-remote-p "niwa-execution-target" (target))
(declare-function niwa-execution-target-restore-incarnation "niwa-execution-target" (target incarnation))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-observe-incarnation "niwa-execution-target")
(autoload 'niwa-execution-target-probe "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")
(autoload 'niwa-execution-target-restore-incarnation "niwa-execution-target")

;; `niwa-hooks'
(declare-function niwa-hooks-flush-log "niwa-hooks" (session))

;; `niwa-journal-capture'
(declare-function niwa-journal-capture--read "niwa-journal-capture" (workspace id &optional observation))
(declare-function niwa-journal-capture-seal-and-schedule "niwa-journal-capture"
                  (session buffer trigger &optional captures))
(autoload 'niwa-journal-capture-seal-and-schedule "niwa-journal-capture")
(autoload 'niwa-journal-capture--read "niwa-journal-capture")


;; `niwa-journal-pins'
(declare-function niwa-journal-pins-observe "niwa-journal-pins" (session-dir))
(declare-function niwa-journal-pins-present-p "niwa-journal-pins" (session-dir))
(autoload 'niwa-journal-pins-present-p "niwa-journal-pins")
(autoload 'niwa-journal-pins-observe "niwa-journal-pins")

;; `niwa-journal-process'
(declare-function niwa-journal-process-discard "niwa-journal-process" (workspace id &optional original-source))
(autoload 'niwa-journal-process-discard "niwa-journal-process")
(declare-function niwa-journal-process-stop-all "niwa-journal-process" ())
(defvar niwa-journal-process--inhibit-scheduling)

;; `niwa-memory-decision'
(declare-function niwa-memory-decision-stop-recovery "niwa-memory-decision" ())
(defvar niwa-memory-decision--inhibit-recovery)

;; `niwa-memory-pass'
(declare-function niwa-memory-pass-stop-all "niwa-memory-pass" ())
(defvar niwa-memory-pass--inhibit-scheduling)

;; `niwa-permission-log'
(declare-function niwa-permission-log-flush "niwa-permission-log" (session))

;; `niwa-persistence'
(declare-function niwa-instruction-store-note-session-ids "niwa-persistence" (workspace))

;; `niwa-ptc-checkpoint'
(declare-function niwa-ptc-checkpoint-reconcile
                  "niwa-ptc-checkpoint" (session))
(autoload 'niwa-ptc-checkpoint-reconcile "niwa-ptc-checkpoint")

;; `niwa-sandbox'
(defvar niwa-sandbox-mode)

;; `niwa-schedule'
(declare-function niwa-schedule-restore-session "niwa-schedule" (session buffer))
(autoload 'niwa-schedule-restore-session "niwa-schedule")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts--live-sidecar "niwa-session-artifacts" (session buffer))
(declare-function niwa-session-artifacts--sidecar-artifact "niwa-session-artifacts" (session buffer &optional checkpoint))
(declare-function niwa-session-artifacts-artifact-present-p "niwa-session-artifacts" (session logical &optional committed-only))
(declare-function niwa-session-artifacts-assert-mutation-authority "niwa-session-artifacts" (session &optional buffer))
(declare-function niwa-session-artifacts-build-sidecar "niwa-session-artifacts" (session buffer &optional checkpoint))
(declare-function niwa-session-artifacts-chats "niwa-session-artifacts" (chapters prompt-index current-segment current-title))
(declare-function niwa-session-artifacts-check-target-incarnation "niwa-session-artifacts" (session buffer))
(declare-function niwa-session-artifacts-collect-prompts "niwa-session-artifacts" (buffer &optional preview-length))
(declare-function niwa-session-artifacts-disown-save-machinery "niwa-session-artifacts" nil)
(declare-function niwa-session-artifacts-find-artifact-noselect "niwa-session-artifacts" (session logical &optional inspection committed-only))
(declare-function niwa-session-artifacts-inhibit-so-long "niwa-session-artifacts" ())
(declare-function niwa-session-artifacts-inspect-cold-session "niwa-session-artifacts" (save-path &optional publication scope))
(declare-function niwa-session-artifacts-load-instructions "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts-printed-value "niwa-session-artifacts" (value))
(declare-function niwa-session-artifacts-publish-sidecar-state "niwa-session-artifacts" (session root-buffer &optional token))
(declare-function niwa-session-artifacts-publish-text "niwa-session-artifacts" (session path content &optional coding prepared token))
(declare-function niwa-session-artifacts-reconcile-relocation "niwa-session-artifacts" (session saved-workspace-plist))
(declare-function niwa-session-artifacts-save "niwa-session-artifacts" (session buffer &optional settled force token))
(declare-function niwa-session-artifacts-save-agent-registry "niwa-session-artifacts" (session buffer &optional token))
(declare-function niwa-session-artifacts-segment-path "niwa-session-artifacts" (save-path n))
(declare-function niwa-session-artifacts-sessions-dir "niwa-session-artifacts" (workspace))
(declare-function niwa-session-artifacts-sidecar-path "niwa-session-artifacts" (save-path))
(declare-function niwa-session-artifacts-stabilize-gptel-bounds "niwa-session-artifacts" nil)
(autoload 'niwa-session-artifacts-inhibit-so-long
  "niwa-session-artifacts")
(defvar niwa-session-artifacts-require-agent-commit-p)
(defvar niwa-session-artifacts--terminal-sessions)

;; `niwa-session-checkpoint'
(declare-function niwa-session-checkpoint-capture "niwa-session-checkpoint" (session conversation &optional root-projection))
(declare-function niwa-session-checkpoint-install "niwa-session-checkpoint" (session candidate))
(declare-function niwa-session-checkpoint-project "niwa-session-checkpoint" (session sidecar &optional candidate))
(autoload 'niwa-session-checkpoint-capture "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-install "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-project "niwa-session-checkpoint")

;; `niwa-session-codec'
(declare-function niwa-session-codec-authority-mode-for-path "niwa-session-codec" (session-dir &optional session explicit-mode))
(declare-function niwa-session-codec-deserialize "niwa-session-codec" (plist workspace))
(declare-function niwa-session-codec-portable-authority-p "niwa-session-codec" (session))
(declare-function niwa-session-codec-read "niwa-session-codec" (path))
(declare-function niwa-session-codec-validate-authority-mode "niwa-session-codec" (mode))
(declare-function niwa-session-codec-validate-current-sidecar "niwa-session-codec" (plist))
(defvar niwa-session-codec-format-version)

;; `niwa-session-collection'
(declare-function niwa-session-collection-schedule "niwa-session-collection" (session))
(autoload 'niwa-session-collection-schedule "niwa-session-collection")

;; `niwa-session-control-fs'
(defvar niwa-session-control-fs--async-wait)
(declare-function niwa-session-control-fs-append-rotating "niwa-session-control-fs"
                  (path content max-bytes))
(declare-function niwa-session-control-fs-async-busy-p "niwa-session-control-fs" (directory))
(declare-function niwa-session-control-fs-path-exists-p "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-physical-path "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-program-value "niwa-session-control-fs" (result))
(declare-function niwa-session-control-fs-run-program "niwa-session-control-fs" (operations &optional lock-directory))
(declare-function niwa-session-control-fs-tree-sizes "niwa-session-control-fs" (paths))
(declare-function niwa-session-control-fs-wait-async "niwa-session-control-fs" (&optional seconds))
(declare-function niwa-session-control-fs-write-file "niwa-session-control-fs" (path content))
(autoload 'niwa-session-control-fs-append-rotating
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-path-exists-p
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-physical-path
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-write-file
  "niwa-session-control-fs")

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer-notify "niwa-session-control-transfer" (session event &rest args))
(declare-function niwa-session-control-transfer-register-root-buffer "niwa-session-control-transfer" (session buffer))
(declare-function niwa-session-control-transfer-root-buffer-for-id "niwa-session-control-transfer" (session-id))
(autoload 'niwa-session-control-transfer-notify
  "niwa-session-control-transfer")
(autoload 'niwa-session-control-transfer-register-root-buffer
  "niwa-session-control-transfer")
(autoload 'niwa-session-control-transfer-root-buffer-for-id
  "niwa-session-control-transfer")

;; `niwa-session-durability'
(declare-function niwa-session-durability-cleanup-assessment "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability-assess-batch "niwa-session-durability" (directories))
(declare-function niwa-session-durability-discard-session "niwa-session-durability" (session prepare-function))
(declare-function niwa-session-durability-call-with-reserved-lease "niwa-session-durability" (session function))
(declare-function niwa-session-durability-disclose "niwa-session-durability" (session))
(declare-function niwa-session-durability-lease-acquire "niwa-session-durability" (session-dir buffer-name &optional session))
(declare-function niwa-session-durability-lease-owned-p "niwa-session-durability" (session))
(declare-function niwa-session-durability-lease-release "niwa-session-durability" (session-dir &optional session token))
(declare-function niwa-session-durability-lease-status "niwa-session-durability" (session-dir))
(declare-function niwa-session-durability-set-unsettled-mutation "niwa-session-durability" (session value &optional token))
(defvar niwa-session-durability--asserted-directories)
(defvar niwa-session-durability--transaction-clock)
(autoload 'niwa-session-durability-disclose "niwa-session-durability")
(autoload 'niwa-session-durability-cleanup-assessment "niwa-session-durability")
(autoload 'niwa-session-durability-discard-session "niwa-session-durability")
(autoload 'niwa-session-durability-lease-acquire
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-owned-p
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-release
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-status
  "niwa-session-durability")

;; `niwa-session-fork'
(declare-function niwa-session-fork-clone-session
                  "niwa-session-fork"
                  (session policy &rest keys))
(autoload 'niwa-session-fork-clone-session "niwa-session-fork")

;; `niwa-session-naming'
(declare-function niwa-rename-session "niwa-session-naming" (new-name))
(declare-function niwa-session-naming-adopt-explicit "niwa-session-naming" (session name))
(declare-function niwa-session-naming-cancel "niwa-session-naming" ())
(declare-function niwa-session-naming-normalize "niwa-session-naming" (name))
(autoload 'niwa-rename-session "niwa-session-naming" nil t)
(autoload 'niwa-session-naming-adopt-explicit "niwa-session-naming")
(autoload 'niwa-session-naming-cancel "niwa-session-naming")
(autoload 'niwa-session-naming-normalize "niwa-session-naming")

;; `niwa-session-publication'
(declare-function niwa-session-publication-async-eligible-p
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-call-with-diagnostic-batch
                  "niwa-session-publication" (session function))
(declare-function niwa-session-publication-barrier
                  "niwa-session-publication" (session &optional token))
(declare-function niwa-session-publication-busy-p
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-defer
                  "niwa-session-publication"
                  (session preparation &optional failure token))
(declare-function niwa-session-publication-publish "niwa-session-publication" (session artifacts &optional require-commit prepared token))
(declare-function niwa-session-publication-read "niwa-session-publication" (session-dir &optional head names))
(declare-function niwa-session-publication-read-batch
                  "niwa-session-publication" (directories listings))
(declare-function niwa-session-publication-stopping
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-submit
                  "niwa-session-publication"
                  (session prepare acknowledge &optional mail-ids retain-failure select-staged superseded))
(declare-function niwa-session-publication-diagnostic-backlog-p
                  "niwa-session-publication" (session))
(autoload 'niwa-session-publication-async-eligible-p "niwa-session-publication")
(autoload 'niwa-session-publication-barrier "niwa-session-publication")
(autoload 'niwa-session-publication-busy-p "niwa-session-publication")
(autoload 'niwa-session-publication-defer "niwa-session-publication")
(autoload 'niwa-session-publication-stopping "niwa-session-publication")
(autoload 'niwa-session-publication-submit "niwa-session-publication")
(autoload 'niwa-session-publication-diagnostic-backlog-p
  "niwa-session-publication")
(autoload 'niwa-session-publication-call-with-diagnostic-batch
  "niwa-session-publication")
(autoload 'niwa-session-publication-publish "niwa-session-publication")
(autoload 'niwa-session-publication-read "niwa-session-publication")
(autoload 'niwa-session-publication-read-batch "niwa-session-publication")

;; `niwa-session-recovery'
(declare-function niwa-session-recovery--abandon "niwa-session-recovery" (session))
(declare-function niwa-session-recovery-read "niwa-session-recovery" (session-dir &optional root-observation))
(autoload 'niwa-session-recovery--abandon "niwa-session-recovery")
(autoload 'niwa-session-recovery-read "niwa-session-recovery")
(defvar niwa-session-recovery--mutation-cache)

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-format-relative-time "niwa-session-rewind" (iso))
(autoload 'niwa-session-rewind-format-relative-time
  "niwa-session-rewind")

;; `niwa-session-save-as'
(declare-function niwa-session-save-as--rename-live-session-buffers "niwa-session-save-as" (session data-buffer))
(declare-function niwa-session-save-as-run "niwa-session-save-as" (session buffer new-name new-id new-save-path))
(autoload 'niwa-session-save-as--rename-live-session-buffers
  "niwa-session-save-as")
(autoload 'niwa-session-save-as-run "niwa-session-save-as")

;; `niwa-transport'
(declare-function niwa-transport-cancel-idle "niwa-transport" (table tag))
(declare-function niwa-transport-cancel-pending
                  "niwa-transport" (&optional key))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (owner remote-path function))
(declare-function niwa-transport-schedule-idle
                  "niwa-transport" (table key tag path thunk))
(autoload 'niwa-transport-cancel-idle "niwa-transport")
(autoload 'niwa-transport-cancel-pending "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")
(autoload 'niwa-transport-schedule-idle "niwa-transport")

;; `niwa-structs'
(declare-function niwa-session-authority-mode-for-session "niwa-structs" (session))
(declare-function niwa-session-authority-mode-for-workspace "niwa-structs" (workspace))
(declare-function niwa-session-buffer-name "niwa-structs" (session-name workspace))
(declare-function niwa-session-created-at "niwa-structs" (cl-x))
(declare-function niwa-session-current-segment "niwa-structs" (cl-x))
(declare-function niwa-session-execution-target "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-fork-point-id "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-turn "niwa-structs" (cl-x))
(declare-function niwa-session-generate-id "niwa-structs" ())
(declare-function niwa-session-lease "niwa-structs" (cl-x))
(declare-function niwa-session-mail-notifications "niwa-structs" (cl-x))
(declare-function niwa-session-name "niwa-structs" (cl-x))
(declare-function niwa-session-pending-publication "niwa-structs" (cl-x))
(declare-function niwa-session-permission-log-pending "niwa-structs" (cl-x))
(declare-function niwa-session-publication "niwa-structs" (cl-x))
(declare-function niwa-session-publication-active-p "niwa-structs" (cl-x))
(declare-function niwa-session-publication-stopping-p "niwa-structs" (cl-x))
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x))
(declare-function niwa-session-save-path "niwa-structs" (cl-x))
(declare-function niwa-session-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-set-root-buffer "niwa-structs" (session buffer))
(declare-function niwa-session-turn-count "niwa-structs" (cl-x))
(declare-function niwa-session-updated-at "niwa-structs" (cl-x))
(declare-function niwa-session-working-directory "niwa-structs" (cl-x))
(declare-function niwa-session-workspace "niwa-structs" (cl-x))
(declare-function niwa-workspace-id "niwa-structs" (cl-x))
(declare-function niwa-workspace-root "niwa-structs" (cl-x))
(declare-function niwa-workspace-type "niwa-structs" (cl-x))
(defvar niwa--agent-invocation)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa--workspace)
(defvar niwa-session--read-only-mode)

;; `niwa-tool-repair'
(declare-function niwa-tool-repair-flush-log "niwa-tool-repair-diagnostics" (session))

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-blocks
                  "niwa-tool-render-data" (string))
(declare-function niwa-tool-render-data-call-range-p
                  "niwa-tool-render-data" (data beg end))
(declare-function niwa-tool-render-data-reconcile-lost-executions
                  "niwa-tool-render-data"
                  (buffer &optional successor-execution-ids lost-monitor))
(declare-function niwa-tool-render-data-without-owner
                  "niwa-tool-render-data" (data))
(defvar niwa-tool-render-data-close)
(defvar niwa-tool-render-data-open)
(autoload 'niwa-tool-render-data-blocks "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-call-range-p "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-reconcile-lost-executions
  "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-without-owner
  "niwa-tool-render-data")

;; `niwa-transcript-audit'
(declare-function niwa-transcript-audit-records "niwa-transcript-audit" (text &optional type))
(autoload 'niwa-transcript-audit-records "niwa-transcript-audit")

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-gptel-state "niwa-transcript-restore" nil)
(declare-function niwa-transcript-restore-properties "niwa-transcript-restore" (&optional only-if-missing))
(autoload 'niwa-transcript-restore-gptel-state
  "niwa-transcript-restore")
(autoload 'niwa-transcript-restore-properties "niwa-transcript-restore")

;; `niwa-utilities'
(declare-function niwa--call-with-bare-transcript-mode
                  "niwa-utilities" (function))
(declare-function niwa--forget-place "niwa-utilities" nil)
(declare-function niwa--ordered-completion-table "niwa-utilities" (displays category))
(declare-function niwa--transcript-org-mode "niwa-utilities" nil)
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa--call-with-bare-transcript-mode "niwa-utilities")
(autoload 'niwa--forget-place "niwa-utilities")
(autoload 'niwa--ordered-completion-table "niwa-utilities")
(autoload 'niwa--transcript-org-mode "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; `niwa-view'
(defvar niwa--data-buffer)
(defvar niwa--view-buffer)

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(declare-function niwa-workspace-ensure-generated-state-ignored "niwa-workspace" (workspace))
(declare-function niwa-workspace-get-or-create "niwa-workspace" (type id root name))
(defvar niwa-workspace-additional-roots)
(defvar niwa-workspace--registry)
(autoload 'niwa-workspace "niwa-workspace")
(autoload 'niwa-workspace-ensure-generated-state-ignored
  "niwa-workspace")
(autoload 'niwa-workspace-get-or-create "niwa-workspace")

;; `niwa-workspace-identity'
(declare-function niwa-workspace-identity-read "niwa-workspace-identity" (root))
(autoload 'niwa-workspace-identity-read "niwa-workspace-identity")
(declare-function niwa-workspace-identity-ensure "niwa-workspace-identity" (root))
(autoload 'niwa-workspace-identity-ensure "niwa-workspace-identity")

;; `org'
(defvar org-agenda-file-menu-enabled)

;; `saveplace'
(defvar save-place-mode)

;; `so-long'
(defvar so-long--inhibited)
(defvar so-long-predicate)

;;
;;; Customization

(defcustom niwa-file-history-max-snapshot-bytes (* 1024 1024)
  "Soft size cap (bytes) for individual pre-turn file checkpoints.

Larger content is persisted as an explicit checkpoint gap with a warning.
Defaults to 1 MB."
  :type 'integer
  :group 'niwa)

(defcustom niwa-session-max-age-days 30
  "Auto-cleanup threshold for old sessions, in days.

Sessions older than this are deleted by a sweep that runs when the
`niwa' session chooser lists sessions, after a session opens, after
a root turn ends, and when Emacs exits.  Each workspace is swept at
most once a day per Emacs, and only an interactive Emacs sweeps from
session activity or exit.  Age comes from `:updated-at', or the sidecar
or session directory modification time when metadata cannot provide it;
a portable session's last lease renewal must also be older.  Sessions
with a live, publishing, mutating or transfer-reserved lease are skipped.
Legacy pre-lease control state is always refused.  So are the
`niwa-session-keep-recent-count' most-recently-updated sessions.  A nil
value disables auto-cleanup entirely."
  :type '(choice (integer :tag "Days")
          (const :tag "Disabled" nil))
  :group 'niwa)

(defcustom niwa-session-keep-recent-count 3
  "Number of most-recently-updated sessions exempt from auto-cleanup.

The newest sessions of a workspace, counted by the same timestamp
auto-cleanup ages against, survive `niwa-session-max-age-days'
regardless of age.  Without this floor a long absence would delete the
very session a returning user opens the chooser to resume: the chooser
sweeps expired sessions before listing them, and at that point the
session is not yet locked.  A nil value applies the age cap to every
session."
  :type '(choice (integer :tag "Sessions")
          (const :tag "No floor" nil))
  :group 'niwa)

(defcustom niwa-session-checkpoint-interval 30
  "Seconds between conversation checkpoints while a request is in flight.

Emacs auto-saves only after keyboard input, so a turn that runs
unattended, such as a Goal or a retained agent, would otherwise reach
disk only when it settles.  While any root or agent request is in
flight, modified conversations are checkpointed this often through the
same path as `auto-save-hook'.  A nil value leaves mid-turn checkpoints
to Emacs auto-save alone."
  :type '(choice (number :tag "Seconds")
          (const :tag "Only on Emacs auto-save" nil))
  :group 'niwa)


;;
;;; Sidecar lifecycle

(defun niwa-session-persistence-write-current-buffer-atomically (path)
  "Write the current buffer to PATH through a same-directory rename."
  (niwa-session-control-fs-write-file
   (niwa-session-control-fs-physical-path path)
   (buffer-substring-no-properties (point-min) (point-max))))

(defun niwa-session-persistence--segment-text (session path)
  "Return SESSION's committed transcript text at PATH, or nil when absent.
Portable transcripts live in the publication, not at PATH itself."
  (when (niwa-session-artifacts-artifact-present-p
         session (file-name-nondirectory path) t)
    ;; Published artifacts are literal bytes; transcripts are UTF-8.
    (decode-coding-string
     (niwa-session-artifacts-read-artifact
      session (file-name-nondirectory path) t)
     'utf-8-unix)))

(defun niwa-session-persistence--execution-successor-ids (text)
  "Return structured execution ids present in transcript TEXT."
  (when (and text (string-search ":execution-id" text))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (org-mode))
      (niwa-transcript-restore-properties)
      (let (ids)
        (dolist (record
                 (niwa-transcript-audit-records
                  (buffer-string)))
          (when (memq (plist-get record :type)
                      '(execution-archive execution-completion))
            (when-let* ((id (plist-get (plist-get record :render-data)
                                       :execution-id)))
              (cl-pushnew id ids :test #'equal))))
        (dolist (block
                 (niwa-tool-render-data-blocks
                  (buffer-substring-no-properties (point-min) (point-max))))
          (when-let* ((stored (caddr block))
                      (begin (+ (point-min) (car block)))
                      (end (+ (point-min) (cadr block)))
                      ((niwa-tool-render-data-call-range-p
                        stored begin end))
                      (data
                       (niwa-tool-render-data-without-owner stored))
                      (id (plist-get data :execution-id)))
            (cl-pushnew id ids :test #'equal)))
        ids))))

(defconst niwa-session-persistence--segment-backup-max-bytes
  (* 64 1024 1024)
  "Rotation size of `segment-repair-backups.el'; at most twice this is kept.")

(defun niwa-session-persistence--back-up-segment (path text)
  "Append transcript PATH's TEXT to its session's repair backups.
Closed segments change only through repairs, and publication cleanup drops
the replaced copy within a few saves; this keeps it for diagnosis and
rollback.  A failed backup is reported and does not block the repair."
  (condition-case err
      (let ((print-escape-newlines t) (print-length nil) (print-level nil))
        (niwa-session-control-fs-append-rotating
         (niwa-session-control-fs-physical-path
          (file-name-concat (file-name-directory path) "segment-repair-backups.el"))
         (concat (prin1-to-string
                  (list :time (format-time-string "%FT%T%z")
                        :segment (file-name-nondirectory path)
                        :content (substring-no-properties text)))
                 "\n")
         niwa-session-persistence--segment-backup-max-bytes))
    (error
     (niwa-diagnostic-warning
      (niwa-diagnostic-scope path) 'niwa
      (format "Could not back up %s before repairing it: %s"
              (file-name-nondirectory path) (error-message-string err))))))

(defun niwa-session-persistence--reconcile-lost-execution-file
    (path text &optional successor-execution-ids artifact-callback lost-monitor)
  "Mark stale running execution rows in transcript PATH with TEXT as lost.

When ARTIFACT-CALLBACK is non-nil, pass it the repaired replacement instead
of writing PATH directly.  SUCCESSOR-EXECUTION-IDS and LOST-MONITOR are
passed to `niwa-tool-render-data-reconcile-lost-executions'."
  (when (and text (or (string-search ":execution-id" text)
                      (string-search ":live-execution-p" text)))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (org-mode))
      (niwa-transcript-restore-properties)
      (let ((count
              (niwa-tool-render-data-reconcile-lost-executions
              (current-buffer) successor-execution-ids lost-monitor)))
        (when (> count 0)
          (niwa-session-persistence--back-up-segment path text)
          (niwa-session-artifacts-stabilize-gptel-bounds)
          (if artifact-callback
              (funcall
               artifact-callback
               (list :path path
                     :content (buffer-string)
                     :coding 'utf-8-unix))
            (niwa-session-persistence-write-current-buffer-atomically
             path)))
        count))))

(defun niwa-session-persistence-reconcile-lost-execution-segments
    (session &optional exclude-path artifact-callback lost-monitor)
  "Repair stale execution rows in SESSION segments except EXCLUDE-PATH.

ARTIFACT-CALLBACK and LOST-MONITOR have the meanings described by
`niwa-session-persistence--reconcile-lost-execution-file'."
  (let ((save-path (niwa-session-save-path session))
        (successor-ids
         (and exclude-path
              (niwa-session-persistence--execution-successor-ids
               (niwa-session-persistence--segment-text
                session exclude-path))))
        (count 0))
    (cl-loop for segment downfrom (niwa-session-current-segment session) to 1
             for path = (niwa-session-artifacts-segment-path
                         save-path segment)
             for text = (unless (and exclude-path
                                     (string= (expand-file-name path)
                                              (expand-file-name exclude-path)))
                          (niwa-session-persistence--segment-text session path))
             when text
             do (cl-incf
                 count
                 (or (niwa-session-persistence--reconcile-lost-execution-file
                      path text successor-ids artifact-callback lost-monitor)
                     0))
             and do
             (dolist (id (niwa-session-persistence--execution-successor-ids text))
               (cl-pushnew id successor-ids :test #'equal)))
    count))

(defun niwa-session-persistence--flush-diagnostic-logs-now (session)
  "Retry SESSION diagnostics queued before or after materialization.

The four logs share one lease reservation.  Each append otherwise opened a
transaction of its own -- recovery refresh, lease renewal, ownership
reading, then a reservation renewing the lease on entry and committing it
on exit -- so a flush point paid that four times over for data nothing
reads live."
  (let ((flush
         (lambda ()
           (niwa-telemetry-flush session)
           (when (fboundp 'niwa-hooks-flush-log)
             (niwa-hooks-flush-log session))
           (when (fboundp 'niwa-tool-repair-flush-log)
             (niwa-tool-repair-flush-log session))
           (when (fboundp 'niwa-permission-log-flush)
             (niwa-permission-log-flush session)))))
    ;; Only reserve when a log actually has something.  Each flush already
    ;; returns early on an empty queue, and reserving for four of those
    ;; would renew the lease -- target I/O, and a warning when the lease
    ;; cannot be renewed -- to write nothing at all.  The four queues are
    ;; struct slots, so asking costs nothing.
    (if (or (niwa-session-telemetry-pending session)
            (niwa-session-hook-log-pending session)
            (niwa-session-repair-log-pending session)
            (niwa-session-permission-log-pending session)
            (niwa-session-publication-diagnostic-backlog-p session))
        (niwa-session-publication-call-with-diagnostic-batch
         session flush)
      (funcall flush))))

(defun niwa-session-persistence-flush-diagnostic-logs (session)
  "Flush SESSION diagnostics, keeping target I/O off the caller's path.

A remote diagnostic flush is a whole publication transaction carrying
data nothing reads live, so it waits for an idle transport instead of
extending the save or settlement that queued it.  A local flush is one
append and runs inline.  Emacs exit flushes inline either way."
  (let ((save-path (niwa-session-save-path session)))
    (if (and save-path (file-remote-p save-path))
        (progn
          ;; The zero timer lets the save or settlement that queued the
          ;; diagnostics return first; transport idleness alone would run
          ;; the flush inline, because the transport is usually idle by
          ;; the time a save reaches its cleanup.
          (run-at-time
           0 nil #'niwa-transport-run-when-idle
           (list 'diagnostic-flush session) save-path
           (lambda ()
             (niwa-session-persistence--flush-diagnostic-logs-now
              session))))
      (niwa-session-persistence--flush-diagnostic-logs-now session))))

(defun niwa-session-persistence-allocate-session-id (sessions-dir)
  "Return a fresh id absent from SESSIONS-DIR and live session buffers."
  (cl-loop repeat 33
           for candidate = (niwa-session-generate-id)
           for path = (file-name-concat sessions-dir candidate)
           unless (or (file-exists-p path) (file-symlink-p path)
                      (cl-some
                       (lambda (buffer)
                         (when-let* ((session (buffer-local-value
                                              'niwa--session buffer)))
                           (equal candidate (niwa-session-session-id session))))
                       (buffer-list)))
           return candidate
           finally (error "Could not allocate a unique session id after 33 attempts")))


(defun niwa-session-persistence-shallow-ensure-files (session buffer)
  "Materialize SESSION and BUFFER paths without writing the sidecar.

Used by sub-agent allocation: a sub-agent can spawn during
the parent's first turn (before any DONE handler has run), so we
need the session directory and `agents/' subdirectory before its retained
identity exists.  The later acknowledged registry commit performs the full
root snapshot and sidecar publication.

Returns SESSION's `save-path' on success, or nil on failure.  Idempotent."
  (or (niwa-session-save-path session)
      (condition-case err
          (let* ((_disclosure
                  (when (niwa-execution-target-remote-p
                         (niwa-session-execution-target session))
                    (niwa-session-durability-disclose session)))
                 (_workspace-identity
                  (progn
                    (niwa-workspace-identity-ensure
                     (niwa-workspace-root
                      (niwa-session-workspace session)))))
                 (sessions-dir (niwa-session-artifacts-sessions-dir
                                (niwa-session-workspace session)))
                 (session-id
                  (or (niwa-session-session-id session)
                      (error "Session has no identity")))
                 (save-path (file-name-as-directory
                             (file-name-concat sessions-dir session-id)))
                 (segment-path (niwa-session-artifacts-segment-path
                                save-path 1))
                 (now (format-time-string "%FT%H-%M-%S")))
            (make-directory sessions-dir t)
            (make-directory save-path)
            (make-directory (file-name-concat save-path "agents") t)
            (make-directory (file-name-concat save-path "file-history") t)
            (niwa-session-persistence-lock-acquire
             save-path (buffer-name buffer) session)
            (setf (niwa-session-session-id session)      session-id)
            (setf (niwa-session-save-path session)       save-path)
            (setf (niwa-session-created-at session)      now)
            (setf (niwa-session-updated-at session)      now)
            (setf (niwa-session-current-segment session) 1)
            (niwa-session-persistence-flush-diagnostic-logs session)
            (niwa-workspace-ensure-generated-state-ignored
             (niwa-session-workspace session))
            (with-current-buffer buffer
              (unless buffer-file-name
                (setq buffer-file-name segment-path))
              (niwa-session-artifacts-disown-save-machinery))
            (niwa-session-control-transfer-register-root-buffer
             session buffer)
            save-path)
        (error
         (niwa-diagnostic-message (niwa-diagnostic-scope session) "Shallow session materialization failed: %S" err)
         nil))))

(defun niwa-session-persistence-record-running-transcript
    (session entry)
  "Insert ENTRY into SESSION's agent-transcripts.  ENTRY is (ID . PLIST)."
  (when (and session (consp entry))
    (setf (alist-get (car entry)
                     (niwa-session-agent-transcripts session)
                     nil nil #'equal)
          (cdr entry))))

(defun niwa-session-persistence-update-transcript-entry
    (session agent-id updates)
  "Return nil after merging transcript data into SESSION entry for AGENT-ID.
The argument UPDATES is the change plist."
  (when (and session agent-id)
    (let ((existing (alist-get agent-id
                               (niwa-session-agent-transcripts session)
                               nil nil #'equal)))
      (when existing
        (let ((merged (copy-sequence existing)))
          (cl-loop for (k v) on updates by #'cddr do
                   (setq merged (plist-put merged k v)))
          (setf (alist-get agent-id
                           (niwa-session-agent-transcripts session)
                           nil nil #'equal)
                merged))))))

(defun niwa-session-persistence-write-sidecar-now (session buffer &optional prepared checkpoint token)
  "Best-effort sidecar rewrite for SESSION and BUFFER.

Only writes when the sidecar file already exists on disk -- i.e.
a full root snapshot has written `session.meta.el'.  Before that, the session
may be shallowly materialized (directory + lock + agents/ but no sidecar), so
this observational rewrite remains deferred to the next critical agent commit
or root autosave.  PREPARED admits the owning checkpoint's final sidecar.
CHECKPOINT captures a matching transcript already written by the caller.
TOKEN is the caller's gate token."
  (when (and (memq session (bound-and-true-p niwa-session-artifacts--terminal-sessions))
             (not prepared))
    (user-error "Session checkpoint is being prepared"))
  (when (and session (niwa-session-save-path session)
             (niwa-session-codec-portable-authority-p session)
             (niwa-session-artifacts-artifact-present-p
              session "session.meta.el"))
    (condition-case err
        (progn
          ;; Drain async work before the sidecar is captured.
          (niwa-session-publication-barrier session token)
          (niwa-session-durability-with-transaction
            (niwa-session-artifacts-assert-mutation-authority session buffer)
            (niwa-session-publication-publish
             session
             (list (niwa-session-artifacts--sidecar-artifact
                    session buffer checkpoint))
             nil prepared token)))
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope session)
                                   "Sidecar rewrite failed: %S" err)
       nil)
      (:success t))))

(defun niwa-session-persistence-save-agent-state (session &optional token)
  "Commit SESSION's agent state through its authoritative root buffer.
Keep root transcript fields at the last committed checkpoint.  Before the
first root snapshot, materialize the complete root rather than a bare sidecar.
TOKEN is the caller's gate token."
  (when-let* ((buffer (niwa-session-root-buffer session))
              ((buffer-live-p buffer)))
    (let ((niwa-session-artifacts-require-agent-commit-p t))
      (when (if (and (niwa-session-save-path session)
                     (niwa-session-artifacts-artifact-present-p
                      session "session.meta.el" t))
                (niwa-session-artifacts-publish-sidecar-state
                 session buffer token)
              (niwa-session-artifacts-save session buffer nil t token))
        ;; The committed sidecar already carries the current registry,
        ;; so a pending observational save is absorbed.
        (niwa-session-persistence-cancel-deferred-agent-save session)
        t))))

(defcustom niwa-session-persistence-agent-save-debounce 2.0
  "Seconds to coalesce best-effort agent-state saves before publishing.
Observational agent persists -- activity transitions, explicit mailbox
discards -- fire in bursts.  Inside this window they collapse into
one sidecar-only save, and a synchronous acknowledged commit absorbs a
pending one entirely."
  :type 'number
  :group 'niwa)

(defvar niwa-session-persistence--deferred-agent-saves
  (make-hash-table :test #'eq)
  "Sessions with a pending debounced agent-state save, mapped to timers.")

(defun niwa-session-persistence-save-agent-state-soon (session)
  "Schedule one coalesced best-effort sidecar save for SESSION.
Agent state, queued input, pause and recovery changes use it.  Unlike
`niwa-session-persistence-save-agent-state', the deferred
save is not an acknowledged agent commit: it rewrites only the session
sidecar (the transcript segment is committed at settlement), may be
queued behind an active publication, and is cancelled outright when a
synchronous commit lands first."
  (when (and (niwa-session-save-path session)
             (not (gethash session
                           niwa-session-persistence--deferred-agent-saves)))
    (puthash session
             (run-at-time niwa-session-persistence-agent-save-debounce nil
                          #'niwa-session-persistence--deferred-agent-save
                          session)
             niwa-session-persistence--deferred-agent-saves)))

(defun niwa-session-persistence--deferred-agent-save (session)
  "Run the debounced best-effort agent-state save for SESSION."
  (if-let* ((save-path (niwa-session-save-path session)))
      (niwa-transport-run-when-idle
       (list 'agent-state-save session) save-path
       (lambda ()
         ;; Keep SESSION registered while transport work is deferred so
         ;; the exit hook can still flush it inline.
         (remhash session niwa-session-persistence--deferred-agent-saves)
         (if (niwa-session-publication-active-p session)
             ;; Timer callbacks perform no target I/O while a critical
             ;; publication is active; re-arm and try again after it.
             (niwa-session-persistence-save-agent-state-soon session)
           (when-let* ((buffer (niwa-session-root-buffer session))
                       ((buffer-live-p buffer)))
             (condition-case nil
                 (niwa-session-artifacts-save-agent-registry
                  session buffer)
               (error nil))))))
    (remhash session niwa-session-persistence--deferred-agent-saves)))

(defun niwa-session-persistence-cancel-deferred-agent-save (session)
  "Cancel any pending debounced agent-state save for SESSION."
  (when-let* ((timer (gethash session
                              niwa-session-persistence--deferred-agent-saves)))
    (cancel-timer timer)
    (remhash session niwa-session-persistence--deferred-agent-saves))
  (niwa-transport-cancel-pending (list 'agent-state-save session)))


;;
;;; First user message extraction

(defun niwa-session-persistence-first-user-message (buffer)
  "Return a one-line preview of the first user prompt in BUFFER, or nil.

Use the same canonical prompt derivation as the live and restored prompt
index, excluding structural mailbox deliveries, metadata, summaries and
tool/reasoning scaffolding.  Keep the first non-empty line, truncated to
120 characters."
  (plist-get (car (niwa-session-artifacts-collect-prompts buffer 120))
             :preview))


;;
;;; Buffer selection

(defun niwa-session-persistence-notify-session-event
    (session event &rest args)
  "Send semantic lifecycle EVENT to SESSION's registered observers."
  (apply #'niwa-session-control-transfer-notify session event args))

(defun niwa-session-persistence-root-data-buffer-p (buffer)
  "Return non-nil when BUFFER is an explicitly registered session root.

Unassociated ordinary buffers remain eligible for low-level inspection, but
session buffers must cross the session-owned root registration seam."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((session (and (boundp 'niwa--session)
                             niwa--session)))
           (and (not niwa--agent-invocation)
                (or (and session
                         (eq buffer (niwa-session-root-buffer session)))
                    (and (null session)
                         (not (bound-and-true-p
                               niwa-session--inspection-buffer-p)))))))))

(defun niwa-session-persistence-authoritative-buffer (buffer)
  "Return the authoritative session root for BUFFER, or nil.

The root registration is the only projection boundary persistence needs to
know.  View buffers never become roots because an initialized view has
already registered the session-owned data buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((session (and (boundp 'niwa--session) niwa--session)))
        (cond
         (niwa--agent-invocation nil)
         ((and session (buffer-live-p (niwa-session-root-buffer session)))
          (niwa-session-root-buffer session))
         ((niwa-session-persistence-root-data-buffer-p buffer)
          buffer))))))

(defun niwa-session-persistence-root-buffer-for-session
  (session &optional buffer)
  "Return SESSION's live root data buffer, preferring BUFFER."
  (or (let ((registered (niwa-session-root-buffer session)))
        (and (buffer-live-p registered) registered))
      (let ((candidate
             (niwa-session-persistence-authoritative-buffer buffer)))
        (and candidate
             (eq session
                 (buffer-local-value 'niwa--session candidate))
             candidate))))


;;
;;; Locking

(defun niwa-session-persistence-lock-acquire
    (session-dir buffer-name &optional session)
  "Acquire SESSION-DIR's portable lease for BUFFER-NAME.
SESSION supplies the live owner.  Return nil when another client owns the
lease, permitting read-only inspection; legacy pre-lease state is refused."
  (niwa-session-codec-authority-mode-for-path session-dir session)
  (when session
    (niwa-session-durability-disclose session))
  (niwa-session-durability-lease-acquire session-dir buffer-name session))

(defun niwa-session-persistence-lock-release (session-dir &optional session token)
  "Release this client's portable lease for SESSION-DIR.
SESSION supplies the live owner; TOKEN is the caller's mutation gate token."
  (niwa-session-codec-authority-mode-for-path session-dir session)
  (niwa-session-durability-lease-release session-dir session token))

(defun niwa-session-persistence-release-on-kill ()
  "Buffer-local `kill-buffer-hook' that releases session mutation authority.
Publication work is drained before the final save and lease release.  A root
buffer also stops new admissions.  An unresolved job keeps the session's
state and lease for explicit publication recovery."
  (when (and (boundp 'niwa--session)
             niwa--session)
    (when (niwa-session-persistence--drain-for-close
           niwa--session (current-buffer))
      (niwa-session-persistence-autosave-buffer (current-buffer))
      (when-let* ((dir (niwa-session-save-path niwa--session)))
        (niwa-journal-capture-seal-and-schedule niwa--session (current-buffer) 'session-end)
        (niwa-session-persistence--wait-async)
        (condition-case _
            (niwa-session-persistence-lock-release dir niwa--session)
          (error nil))))))

(defun niwa-session-persistence--wait-async ()
  "Let already-running asynchronous control programs deliver their callbacks.
Closing and exit call it after their last write, before a lease is released."
  (when (featurep 'niwa-session-control-fs)
    (niwa-session-control-fs-wait-async)))

(defun niwa-session-persistence--drain-temporarily (session)
  "Drain SESSION's publication work with admissions and wakes stopped.
The previous stopping state is restored on every exit, so a later veto leaves
the session usable.  Notifications retained while stopped are re-armed through
`niwa-agent-control-reoffer-mail', which never offers inline, and only when
some were actually suppressed.  An unresolved job signals."
  (let ((was-stopping (niwa-session-publication-stopping-p session)))
    (unwind-protect (niwa-session-publication-stopping session)
      (setf (niwa-session-publication-stopping-p session) was-stopping)
      (when (and (not was-stopping)
                 (niwa-session-mail-notifications session))
        (niwa-agent-control-reoffer-mail session)))))

(defun niwa-session-persistence--gate-owner-p (session)
  "Return non-nil when SESSION can own asynchronous publication work.
Only materialized portable sessions publish through the session gate."
  (and session
       (niwa-session-save-path session)
       (niwa-session-codec-portable-authority-p session)))

(defun niwa-session-persistence--drain-for-close (session buffer)
  "Drain SESSION's publication work for closing BUFFER.
The root buffer stops new admissions; other buffers only wait.  Return non-nil
when closing may proceed with its save and lease release.  An unresolved job
warns and returns nil, leaving explicit recovery or abandonment possible."
  (or (not (niwa-session-persistence--gate-owner-p session))
      (let ((was-stopping (niwa-session-publication-stopping-p session)))
        (condition-case err
            (progn
              (if (eq buffer (niwa-session-root-buffer session))
                  (niwa-session-publication-stopping session)
                (niwa-session-publication-barrier session))
              (when (niwa-session-pending-publication session)
                (user-error "Session has pending publication; retry or abandon it first"))
              t)
          (error
           ;; Recovery may admit work again, so admissions reopen.
           (setf (niwa-session-publication-stopping-p session) was-stopping)
           (niwa-diagnostic-warning (niwa-diagnostic-scope session)
          'niwa
          (format "Session publication is unresolved; not saving or releasing: %s"
                  (error-message-string err))
          :warning)
           nil)))))

(defun niwa-session-persistence-allow-buffer-close-p ()
  "Return non-nil when closing the current session buffer may proceed.
A `kill-buffer-query-functions' member: it drains an owned root buffer's
publication work while teardown has not yet started, and refuses the close
with a message when a job stays unresolved, so explicit recovery remains
possible."
  (or (not (and (boundp 'niwa--session) niwa--session))
      (not (niwa-session-persistence--gate-owner-p niwa--session))
      (not (eq (current-buffer) (niwa-session-root-buffer niwa--session)))
      ;; The drain is the decision.  Another query may still veto the close,
      ;; so admissions reopen; the kill hook stops them for good.
      (condition-case err
          (progn
            (niwa-session-persistence--drain-temporarily niwa--session)
            t)
        (error
         (niwa-diagnostic-message (niwa-diagnostic-scope niwa--session)
          (concat "Session publication is unresolved (%s); run "
                  "niwa-session-publication-retry or "
                  "niwa-session-publication-abandon before closing")
          (error-message-string err))
         nil))))


;;
;;; Automatic persistence

(defvar-local niwa-session--save-failed nil
  "Non-nil when the most recent auto-save failed in this buffer.
Set by terminal and lifecycle auto-save handlers on any save error and
cleared on the next successful save.  Surfaced by
`niwa-session-persistence-header-segment' so the user has a
visible cue in addition to the `display-warning'.")

(defun niwa-session-persistence--note-autosave-failure (buffer err &optional scope)
  "Mark BUFFER's auto-save failed with ERR and warn using captured SCOPE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local niwa-session--save-failed t)
      (force-mode-line-update)))
  (niwa-diagnostic-warning (or scope (niwa-diagnostic-scope buffer))
   'niwa
   (format "Could not auto-save %s: %s"
           (if (buffer-live-p buffer) (buffer-name buffer) "buffer")
           (error-message-string err))
   :warning))

(defun niwa-session-persistence-autosave-buffer (buffer &optional token)
  "Save BUFFER's owned conversation, containing and reporting save failures.
Retained agents use their transcript writer.  Root buffers use session
publication.  Views and read-only inspection buffers never write.  While the
session's publication gate is owned the save is deferred before anything is
captured, and the return value is nil.  TOKEN is the caller's gate token; a
deferred save runs with its own."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (bound-and-true-p niwa--session)
                 (niwa-session-workspace niwa--session)
                 ;; Opening a conversation is not durable work.  Already
                 ;; saved sessions still checkpoint an intentionally empty
                 ;; transcript, and first-turn partial text is worth saving.
                 (or (niwa-session-save-path niwa--session)
                     (save-restriction (widen) (> (buffer-size) 0)))
                 (not (bound-and-true-p niwa-session--read-only-mode))
                 (not (bound-and-true-p niwa-session--inspection-buffer-p))
                 (or niwa--agent-invocation
                     (eq buffer (niwa-session-root-buffer niwa--session))))
        (let ((scope (niwa-diagnostic-scope niwa--session)))
        (unless (and (niwa-session-persistence--gate-owner-p niwa--session)
                     (niwa-session-publication-defer
                      niwa--session
                      (lambda (prepared-token)
                        (niwa-session-persistence-autosave-buffer
                         buffer prepared-token))
                      (lambda (err)
                        (niwa-session-persistence--note-autosave-failure
                         buffer err scope))
                      token))
        (let ((inhibit-quit t))
          (condition-case err
              (let* ((agent niwa--agent-invocation)
                     (portable (niwa-session-codec-portable-authority-p
                                niwa--session))
                     ;; A checkpoint needs a marker, so let the transcript
                     ;; writer include it instead of publishing twice.
                     (_ (when (and agent portable)
                          (setf (niwa-agent-invocation-sidecar-dirty agent) t)))
                     (saved
                      (if agent
                          (niwa-agent-conversation-save agent nil token)
                        (niwa-session-artifacts-save
                         niwa--session buffer nil nil token))))
                (unless saved
                  (error "Conversation was not saved"))
                ;; Agent writes can be retained publication batches awaiting
                ;; a sidecar commit.  A checkpoint must be visible on resume,
                ;; even when the root transcript has not changed.
                (when (and agent portable
                           (or (niwa-session-publication-uncommitted-batches
                                niwa--session)
                               (niwa-session-publication-queue niwa--session)
                               (niwa-agent-invocation-sidecar-dirty agent)))
                  (let ((root (niwa-session-root-buffer niwa--session)))
                    (if (niwa-session-artifacts-artifact-present-p
                         niwa--session "session.meta.el" t)
                        (niwa-session-artifacts-publish-sidecar-state
                         niwa--session root token)
                      (unless (with-current-buffer root
                                (niwa-session-artifacts-save
                                 niwa--session root nil nil token))
                        (error "Agent checkpoint has no saved root")))))
                (setq-local niwa-session--save-failed nil)
                (force-mode-line-update)
                saved)
            ((error quit)
             (niwa-session-persistence--note-autosave-failure buffer err scope)
             nil)))))))))

(defvar niwa-session-persistence--autosaves (make-hash-table :test #'eq)
  "Conversation buffers with a queued auto-save opportunity.")

(defvar-local niwa-session--checkpoint-pending nil
  "Identity of this buffer's accepted background checkpoint, or nil.")

(defun niwa-session-persistence-checkpoint-buffer (buffer)
  "Checkpoint BUFFER in the background when its authority permits it.
Only warm local portable root and agent conversations use an asynchronous
publication job.  Other authorities use the synchronous autosave writer.
Return `pending' for accepted work, nil for an ineligible or absorbed save.
Capture and the modified/failed/uncommitted recheck run only under the gate.
Failures retain frozen publication recovery and the existing save-failed cue.
Explicit saves and teardown keep using the synchronous writer and its barrier."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and niwa--session
                 (niwa-session-workspace niwa--session)
                 (not (bound-and-true-p niwa-session--read-only-mode))
                 (not (bound-and-true-p niwa-session--inspection-buffer-p))
                 (or niwa--agent-invocation
                     (eq buffer (niwa-session-root-buffer niwa--session)))
                 (not (niwa-session-publication-stopping-p niwa--session))
                 (not niwa-session--checkpoint-pending)
                 (or (buffer-modified-p) niwa-session--save-failed
                     (niwa-session-publication-uncommitted-batches niwa--session)))
        (if (not (niwa-session-publication-async-eligible-p niwa--session))
            (niwa-session-persistence-autosave-buffer buffer)
          (let* ((session niwa--session)
                 (scope (niwa-diagnostic-scope session))
                 (identity (list buffer))
                 prepared fallback
                 (acknowledge
                  (lambda (success err)
                    (when (and (buffer-live-p buffer)
                               (eq identity (buffer-local-value
                                             'niwa-session--checkpoint-pending buffer)))
                      (with-current-buffer buffer
                        (setq niwa-session--checkpoint-pending nil)
                        (condition-case cleanup
                            (if success
                                (if fallback
                                    ;; The gate is released before ACK.  A
                                    ;; missing canonical agent transcript must
                                    ;; use its ordinary writer, not a successful
                                    ;; no-op that clears the failure cue.
                                    (niwa-session-persistence-autosave-buffer buffer)
                                  (when prepared (funcall (plist-get prepared :finish)))
                                  (setq niwa-session--save-failed nil)
                                  (force-mode-line-update))
                              (niwa-session-persistence--note-autosave-failure
                               buffer (if (stringp err) (list 'error err) err) scope))
                          ((error quit)
                           (niwa-session-persistence--note-autosave-failure
                            buffer cleanup scope))))))))
            (setq niwa-session--checkpoint-pending identity)
            (condition-case err
                (niwa-session-publication-submit
                 session
                 (lambda ()
                   (when (and (buffer-live-p buffer)
                              (eq session (buffer-local-value 'niwa--session buffer)))
                     (with-current-buffer buffer
                       ;; A synchronous save ahead of this preparation can absorb
                       ;; it.  No transcript, sidecar or hooks were frozen yet.
                       (when (or (buffer-modified-p) niwa-session--save-failed
                                 (niwa-session-publication-uncommitted-batches session))
                         (niwa-session-artifacts-recover-working-directory session)
                         (niwa-session-artifacts-check-target-incarnation session buffer)
                         (setq prepared
                               (niwa-session-artifacts-call-with-checkpoint
                                session
                                (lambda ()
                                  (if niwa--agent-invocation
                                      (niwa-agent-conversation-prepare-save
                                       niwa--agent-invocation)
                                    (niwa-session-artifacts-prepare-save
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

(defun niwa-session-persistence-autosave-soon (buffer)
  "Queue one transport-safe checkpoint of conversation BUFFER.
Repeated calls before it runs coalesce.  The checkpoint runs only if
BUFFER still has unsaved changes by then; settlement or an explicit save
can absorb it."
  (niwa-transport-schedule-idle
   niwa-session-persistence--autosaves buffer 'conversation-autosave
   (buffer-local-value 'default-directory buffer)
   (lambda ()
     ;; Recheck at execution, including failed saves whose transcript
     ;; write succeeded before their marker failed.
     (when (and (buffer-live-p buffer)
                (or (buffer-modified-p buffer)
                    (buffer-local-value 'niwa-session--save-failed buffer)
                    (with-current-buffer buffer
                      (and (bound-and-true-p niwa--session)
                           (niwa-session-publication-uncommitted-batches
                            niwa--session)))))
       (niwa-session-persistence-checkpoint-buffer buffer)))))

(defun niwa-session-persistence-autosave ()
  "Checkpoint modified data buffers during Emacs auto-save.
Queue one transport-safe opportunity per buffer, yielding between saves.
A failed write does not prevent other conversations from being saved.
Settlement and exit still persist synchronously.  The in-flight
checkpoint timer calls this too, because Emacs auto-saves only after
input and an unattended turn receives none."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (bound-and-true-p niwa--session))
               (or (buffer-modified-p buffer)
                   (buffer-local-value 'niwa-session--save-failed buffer)
                   (with-current-buffer buffer
                     (and (eq buffer (niwa-session-root-buffer
                                      niwa--session))
                          (niwa-session-publication-uncommitted-batches
                           niwa--session)))))
      (niwa-session-persistence-autosave-soon buffer))))


;;;; In-flight checkpoints

(defvar niwa-session-persistence--checkpoint-timer nil
  "Repeating timer that checkpoints conversations during requests, or nil.")

(defun niwa-session-persistence--request-in-flight-p ()
  "Return non-nil when some niwa buffer has a request in flight."
  (and (boundp 'niwa--current-request)
       (cl-some (lambda (buffer)
                  (buffer-local-value 'niwa--current-request buffer))
                (buffer-list))))

(defun niwa-session-persistence--checkpoint-tick ()
  "Checkpoint modified conversations; stop once no request is in flight.
The stopping tick checkpoints too: changes deferred to these checkpoints,
such as completion breadcrumbs, may have landed after the last request
settled and saved only its own buffer."
  (unless (niwa-session-persistence--request-in-flight-p)
    (niwa-session-persistence-stop-checkpoints))
  (niwa-session-persistence-autosave))

(defun niwa-session-persistence-start-checkpoints ()
  "Checkpoint conversations periodically until no request is in flight.
Request admission calls this; repeated calls share one timer, which
stops itself on the first tick that finds every request settled."
  (when (and niwa-session-checkpoint-interval
             (not (memq niwa-session-persistence--checkpoint-timer
                        timer-list)))
    (setq niwa-session-persistence--checkpoint-timer
          (run-at-time niwa-session-checkpoint-interval
                       niwa-session-checkpoint-interval
                       #'niwa-session-persistence--checkpoint-tick))))

(defun niwa-session-persistence-stop-checkpoints ()
  "Stop the in-flight checkpoint timer."
  (when (timerp niwa-session-persistence--checkpoint-timer)
    (cancel-timer niwa-session-persistence--checkpoint-timer))
  (setq niwa-session-persistence--checkpoint-timer nil))

(defun niwa-session-persistence-checkpoints-active-p ()
  "Return non-nil while in-flight checkpoints periodically save conversations."
  (and (timerp niwa-session-persistence--checkpoint-timer)
       (memq niwa-session-persistence--checkpoint-timer timer-list)
       t))

(defun niwa-session-persistence-header-segment ()
  "Return a header-line fragment summarising persistence state.
Empty when the session has no unusual state; highlights save
failures and read-only mode when active."
  (let ((parts nil))
    (when (and (bound-and-true-p niwa--session)
               (niwa-session-pending-publication niwa--session))
      (push (propertize " [Publication pending] " 'face 'error) parts))
    (when (bound-and-true-p niwa--session)
      (when-let* ((lease-state
                   (plist-get (niwa-session-lease niwa--session)
                              :state))
                  ((memq lease-state '(foreign expired lost contested))))
        (push (propertize (format " [Lease: %s] " lease-state)
                          'face 'warning)
              parts)))
    (when (bound-and-true-p niwa-session--save-failed)
      (push (propertize " [Save failed] " 'face 'error) parts))
    (when (bound-and-true-p niwa-session--read-only-mode)
      (push (propertize " [Read-only] " 'face 'warning) parts))
    (if parts (apply #'concat parts) "")))


;;
;;; Read path (resume)

(defun niwa-session-persistence-load-sidecar (path &optional scope)
  "Read a current-version sidecar plist from PATH.
Return nil when the sidecar is missing or unreadable.  Signal when a
readable sidecar has an unsupported version or obsolete shape.
SCOPE is the explicit diagnostic root when the caller knows its owner."
  (let* ((result (niwa-session-persistence--classify-sidecar path))
         (diagnostic (plist-get result :diagnostic)))
    (pcase (plist-get result :status)
      ('current (plist-get result :sidecar))
      ((or 'missing 'unreadable)
       (niwa-diagnostic-warning (or scope 'unavailable) 'niwa (format "%s: %s" diagnostic path) :warning)
       nil)
      (_ (error "%s" diagnostic)))))

(defun niwa-session-persistence--classify-sidecar-data (sidecar)
  "Classify already-read SIDECAR using the current persistence schema."
  (cond
   ((not (and (plistp sidecar) (keywordp (car sidecar))))
    (list :status 'unreadable :diagnostic "Invalid session sidecar"))
   ((not (equal (plist-get sidecar :version)
                niwa-session-codec-format-version))
    (list :status 'unsupported
          :version (plist-get sidecar :version)
          :updated-at (plist-get sidecar :updated-at)
          :diagnostic (format "Unsupported session version: %s"
                              (or (plist-get sidecar :version) "missing"))))
   (t
    (condition-case err
        (progn
          (niwa-session-codec-validate-current-sidecar sidecar)
          (list :status 'current :sidecar sidecar))
      (error
       (list :status 'obsolete
             :version (plist-get sidecar :version)
             :updated-at (plist-get sidecar :updated-at)
             :diagnostic (error-message-string err)))))))

(defun niwa-session-persistence--classify-sidecar (path)
  "Return the compatibility classification for the sidecar at PATH.

The returned plist has `:status' equal to `current', `missing', `unreadable',
`unsupported', or `obsolete'.  Current results carry `:sidecar'; readable
incompatible results retain their observed version, update stamp, and exact
validation diagnostic."
  (if (not (file-exists-p path))
      (list :status 'missing :diagnostic "Sidecar is missing")
    (condition-case err
        (niwa-session-persistence--classify-sidecar-data
         (niwa-session-codec-read path))
      (error
       (list :status 'unreadable
             :diagnostic (error-message-string err))))))

(defvar-local niwa-session--read-only-mode nil
  "Non-nil when this chat buffer is in read-only session mode.
Set by the restore path when a cross-host lock cannot be safely
broken.  While set: autosave is inhibited (the terminal DONE handler
early-outs), the view buffer refuses to insert into the data buffer,
and the data buffer itself is marked `buffer-read-only'.")

(defun niwa-session-persistence-apply-read-only-mode (buf &optional reason)
  "Put BUF into read-only session mode.

REASON replaces the default explanation.  Handing control away deliberately
is not the same event as finding someone else already holding the lock, and
saying the wrong one is how a user learns to ignore the notice.

The notice is a message, not a warning: the buffer states its own authority
in the interaction zone and the cockpit header for as long as it holds, so a
popped-up window would repeat durably visible state and steal the frame at
the moment the user is watching the handoff.  A lease that fails or is lost
is a different matter and still warns.
See `niwa-session--read-only-mode' for semantics."
  (with-current-buffer buf
    (setq buffer-read-only t)
    (setq-local niwa-session--read-only-mode t)
    (niwa-diagnostic-message (niwa-diagnostic-scope buf) "%s"
             (or reason
                 "session opened read-only; another client holds the lease"))))

(defun niwa-session-persistence-find-live-buffer (session-id buf-name)
  "Return the named live root buffer for SESSION-ID, or nil.

The canonical session buffer name is the durable locator.  Persistence does
not search unrelated buffers or infer roles from view-local variables."
  (or (niwa-session-control-transfer-root-buffer-for-id session-id)
      (let ((candidate (get-buffer buf-name)))
        (when (and (niwa-session-persistence-root-data-buffer-p candidate)
                   (with-current-buffer candidate
                     (or (not (bound-and-true-p niwa--session))
                         (equal (niwa-session-session-id niwa--session)
                                session-id))))
          candidate))))

(defun niwa-session-persistence--hydrate-restored-buffer
    (buf session workspace segment-path acquired additional-roots
         lifecycle-source &optional artifact-callback checkpoint-callback lost-monitors)
  "Hydrate fresh restore buffer BUF and return its durable repair count.
SESSION and WORKSPACE are planted before niwa restores persisted state.
SEGMENT-PATH is reconciled when ACQUIRED owns the session lock.  Additional
workspace roots and LIFECYCLE-SOURCE restore the saved session environment.
ARTIFACT-CALLBACK collects remote transcript replacements for one batch.
CHECKPOINT-CALLBACK receives the candidate frozen with repaired root bytes.
LOST-MONITORS contains the lost facts already repaired in older segments."
  (let ((ptc-insertions 0)
        (ptc-consumptions 0)
        checkpoint-captured)
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (let ((org-agenda-file-menu-enabled nil))
          (org-mode)))
      (setq-local niwa--session session)
      (setq-local niwa--workspace workspace)
      (setq-local default-directory
                  (niwa-session-working-directory session))
      (when additional-roots
        (setq-local niwa-workspace-additional-roots additional-roots))
      (when (fboundp 'niwa--chat-buffer-disable-org-element-cache)
        (niwa--chat-buffer-disable-org-element-cache))
      (niwa-transcript-restore-gptel-state)
      (when acquired
        (niwa-session-artifacts-check-target-incarnation session buf)
        (pcase-let ((`(,inserted . ,consumed)
                     (niwa-ptc-checkpoint-reconcile session)))
          (setq ptc-insertions inserted
                ptc-consumptions consumed))
        (let ((transcript-repairs
               (+ ptc-insertions
                  (niwa-tool-render-data-reconcile-lost-executions
                   buf nil (lambda (facts) (push facts lost-monitors))))))
          (when lost-monitors
            (niwa-agent-control-restore-lost-monitors session lost-monitors))
          (when (or (> transcript-repairs 0) (> ptc-consumptions 0))
            ;; PTC insertions and lost-execution repairs both change transcript
            ;; length; the repair publication needs current offsets.  Rewrite
            ;; only bounds: chat setup has not installed the advice that keeps
            ;; `gptel--save-state' from writing request config to the drawer.
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (when checkpoint-callback
              (funcall checkpoint-callback
                       (niwa-session-checkpoint-capture
                        session "/root" (niwa-session-artifacts--live-sidecar session buf)))
              (setq checkpoint-captured t))
            (when artifact-callback
                (funcall
                 artifact-callback
                 (list :path segment-path
                       :content (buffer-string)
                       :coding (or buffer-file-coding-system 'utf-8-unix))))
            (set-buffer-modified-p nil))))
      (unless acquired
        (niwa-session-persistence-apply-read-only-mode buf))
      ;; Freeze reconciled metadata before initialization hooks advance it.
      (when (and acquired checkpoint-callback (not checkpoint-captured))
        (funcall checkpoint-callback
                 (niwa-session-checkpoint-capture
                  session "/root" (niwa-session-artifacts--live-sidecar session buf))))
      (niwa--chat-buffer-init-common
       buf workspace (or lifecycle-source "resume") (not acquired))
      (when acquired
        (niwa-schedule-restore-session session buf))
      (+ ptc-consumptions
         (prog1
             (niwa-agent-persistence-restore-tree
              session buf (bound-and-true-p niwa-session--read-only-mode))
           (niwa-session-artifacts-load-instructions session))))))

(defun niwa-session-persistence--finish-restored-buffer
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
              (niwa-session-publication-publish
               session
               (append
                repair-artifacts
                (list (list
                       :path (niwa-session-artifacts-sidecar-path
                              (niwa-session-save-path session))
                       :content
                       (niwa-session-artifacts-printed-value
                        (niwa-session-artifacts-build-sidecar
                         session buf repair-checkpoint))
                       :checkpoint repair-checkpoint
                       :commit-marker t))))
              (setq published-p t))
          (error
           (unless (niwa-session-pending-publication session)
             (signal (car err) (cdr err)))))
        (when (and published-p
                   (cl-find buffer-file-name repair-artifacts
                            :key (lambda (artifact)
                                   (plist-get artifact :path))
                            :test #'equal))
          (set-visited-file-modtime))))
    (unless live
      (niwa-session-persistence-notify-session-event
       session 'load-history session))
    (niwa-session-persistence-notify-session-event
     session 'rerender))
  buf)

(defun niwa-session-persistence-resume-id (workspace session-id)
  "Resume WORKSPACE session SESSION-ID, or return nil when unavailable."
  (unless (and (stringp session-id)
               (not (string-empty-p session-id))
               (equal session-id (file-name-nondirectory session-id))
               (not (member session-id '("." ".."))))
    (error "Invalid session id: %S" session-id))
  (let ((session-dir
         (file-name-as-directory
          (file-name-concat
           (niwa-session-artifacts-sessions-dir workspace)
           session-id))))
    (when (file-directory-p session-dir)
      (niwa-session-persistence-restore session-dir nil nil workspace))))

(defun niwa-session-persistence--cold-workspace (session-dir sidecar)
  "Return the workspace SIDECAR names, as reachable from SESSION-DIR.

The persisted root is target-native, so it is only a complete path for a
client whose filesystem authority is the target itself.  SESSION-DIR is a
live path below that same root, so its TRAMP prefix -- empty on the target,
the client's spelling of the connection elsewhere -- qualifies the persisted
root for the client doing the restore."
  (let* ((saved (plist-get sidecar :workspace))
         (root (concat (file-remote-p session-dir)
                       (plist-get saved :target-native-root))))
    (niwa-workspace-get-or-create
     (plist-get saved :type) root root (plist-get saved :name))))

(defconst niwa-session-persistence--pre-lease-message
  "Session %s uses an unsupported transcript format (before portable leases)"
  "Diagnostic for an unsupported PID-lock session, given its directory.")

(defun niwa-session-persistence--pre-lease-p (session-dir)
  "Return non-nil when SESSION-DIR is a file session from before the lease.
It has a PID lock or a plain sidecar, but no lease.  Only failure paths ask."
  (and (not (niwa-session-control-fs-path-exists-p
             (file-name-concat session-dir ".lease")))
       (or (niwa-session-control-fs-path-exists-p
            (file-name-concat session-dir ".lock"))
           (niwa-session-control-fs-path-exists-p
            (niwa-session-artifacts-sidecar-path session-dir)))))

(defun niwa-session-persistence-restore
    (session-dir &optional lifecycle-source session-override workspace)
  "Restore the chat buffer for the session at SESSION-DIR.

Loads the exact current-version sidecar, opens the live segment file in a
buffer named
per `niwa-session-buffer-name', enables `org-mode' and
`gptel-mode' without gptel's Org-state restore, restores text-property
bounds through niwa's transcript path, hydrates the session struct on
the buffer, then runs `niwa--chat-buffer-init-common'.

Returns the chat buffer.  If a buffer for this session is already
alive (matched by session-id), switches to it instead of re-loading.

LIFECYCLE-SOURCE defaults to \"resume\".  SESSION-OVERRIDE, when non-nil,
is the already-projected in-memory session used by a newly published fork.
WORKSPACE is the currently opened workspace authority.

Tasks are deserialized from the sidecar.  Touched-files and
mentions-shown reset to empty hash tables on load."
  (let* ((_authority-check
          (condition-case err
              (niwa-session-codec-authority-mode-for-path
               session-dir session-override
               (and workspace
                    (niwa-session-authority-mode-for-workspace workspace)))
            (error
             (if (niwa-session-persistence--pre-lease-p session-dir)
                 (user-error niwa-session-persistence--pre-lease-message
                             session-dir)
               (signal (car err) (cdr err))))))
         (publication
          (or (niwa-session-publication-read session-dir)
              (if (niwa-session-persistence--pre-lease-p session-dir)
                  (user-error niwa-session-persistence--pre-lease-message
                              session-dir)
                (user-error "Portable session has no committed publication: %s"
                            session-dir))))
         (sidecar-path (plist-get publication :sidecar))
         (sidecar
          (niwa-session-persistence-load-sidecar
           sidecar-path (niwa-diagnostic-scope (or session-override workspace))))
         (had-sidecar-p    (not (null sidecar)))
         (opened-workspace
          (or workspace
              (and session-override
                   (niwa-session-workspace session-override))
              (and sidecar
                   (niwa-session-persistence--cold-workspace
                    session-dir sidecar))
              (niwa-workspace)))
         (result           (when sidecar
                             (niwa-session-codec-deserialize
                              sidecar opened-workspace)))
         (session          (or session-override
                               (plist-get result :session)
                               (user-error
                                "Portable session has no valid published sidecar: %s"
                                session-dir)))
         (agent-registry-repaired-p
          (plist-get result :agent-registry-repaired-p))
         (additional-roots (plist-get result :additional-roots))
         (workspace        (niwa-session-workspace session))
         (sidecar-current-n (and had-sidecar-p
                                 (niwa-session-current-segment session))))
    ;; `save-path' is intentionally not serialized in the sidecar: the
    ;; session directory itself is the source of truth at restore time.
    (setf (niwa-session-save-path session)
          (file-name-as-directory session-dir)
          (niwa-session-publication session) publication)
    ;; Workspace relocation: rewrite path-bearing fields if the saved
    ;; root no longer matches the current workspace's root.
    (when had-sidecar-p
      (niwa-session-artifacts-reconcile-relocation
       session (plist-get sidecar :workspace)))
    (let* ((buf-name     (niwa-session-buffer-name
                          (niwa-session-name session)
                          workspace))
           (session-id   (niwa-session-session-id session))
           ;; Prefer session-id-based lookup so two saved sessions
           ;; named `main' in one workspace don't collide.
           (live         (niwa-session-persistence-find-live-buffer
                          session-id buf-name))
           (cwd-retargeted-p
            (when (and (not live)
                       (not (file-directory-p
                             (niwa-session-working-directory session))))
              (setf (niwa-session-working-directory session)
                    (niwa--normalize-session-directory
                     (read-directory-name
                      (format "Session directory %s is missing; resume in: "
                              (niwa-session-working-directory session))
                      (niwa-workspace-root workspace)
                      (niwa-workspace-root workspace)
                      t)
                     workspace))))
           ;; Fence before opening artifacts.  A foreign holder permits only
           ;; inspection; a live local buffer already owns its session.
           (acquired
           (unless live
              (if (and session-override
                       (niwa-session-durability-lease-owned-p session))
                  t
                (niwa-session-persistence-lock-acquire
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
                (niwa-session-durability-call-with-reserved-lease
                 session
                 (lambda ()
                   (let ((current (niwa-session-publication-read session-dir)))
                     (unless (and current
                                  (equal (plist-get publication :head)
                                         (plist-get current :head)))
                       (user-error
                        "Session state changed while acquiring its lease; retry restore"))
                     (setf (niwa-session-publication session) current)))))
              (let* ((segment-n (niwa-session-current-segment session))
                     (segment-path
                      (niwa-session-artifacts-segment-path
                       session-dir segment-n))
                     repair-checkpoint lost-monitors
                     (agent-repairs 0))
                (setq
                 buf
                 (or
                  live
                  (condition-case err
                      (niwa-session-artifacts-find-artifact-noselect
                       session (file-name-nondirectory segment-path)
                       (not acquired))
                    (file-missing
                     (user-error "Published session segment is unavailable: %s (%s)"
                                 segment-path (error-message-string err))))))
                (unless (and buf (buffer-live-p buf))
                  (user-error "Published session segment is unavailable: %s"
                              segment-path))
                (when (and acquired (not live))
                  (niwa-session-persistence-reconcile-lost-execution-segments
                   session segment-path repair-callback
                   (lambda (facts) (push facts lost-monitors))))
                (with-current-buffer buf
                  (niwa-session-set-root-buffer session buf)
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
                        'niwa--chat-buffer-disable-org-element-cache))
                    (niwa--chat-buffer-disable-org-element-cache)))
                (unless live
                  (setq agent-repairs
                        (niwa-session-persistence--hydrate-restored-buffer
                         buf session workspace segment-path acquired
                         additional-roots lifecycle-source repair-callback
                         (lambda (candidate) (setq repair-checkpoint candidate))
                         lost-monitors)))
                (niwa-session-persistence--finish-restored-buffer
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
                  (niwa-session-collection-schedule session))
                (setq setup-done t)
                buf))
          ;; Any failure after acquisition releases the session lease and
          ;; removes a freshly opened buffer.
          (unless setup-done
            (when (and acquired (not live))
              (condition-case _
                  (niwa-session-persistence-lock-release
                   session-dir session)
                (error nil)))
            (when (and (not live) buf (buffer-live-p buf))
              (condition-case _
                  (kill-buffer buf)
                (error nil)))))))))


;;
;;; Session listing & resume command

(defvar niwa-session-persistence--summary-cache
  (make-hash-table :test #'equal)
  "Classified session summaries keyed by sidecar path and file fingerprint.")

(defun niwa-session-persistence--sidecar-summary (sidecar)
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
        :chats (niwa-session-artifacts-chats
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

(defun niwa-session-persistence--read-summary (sidecar-path)
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
               (gethash path niwa-session-persistence--summary-cache))))
    (if (and cached (equal fingerprint (car cached)))
        (cdr cached)
      (let* ((classified
              (niwa-session-persistence--classify-sidecar path))
             (sidecar (plist-get classified :sidecar))
             (summary
              (if sidecar
                  (list :status 'current
                        :summary
                        (niwa-session-persistence--sidecar-summary sidecar))
                classified)))
        (when fingerprint
          (puthash path (cons fingerprint summary)
                   niwa-session-persistence--summary-cache))
        summary))))

(defvar niwa-session-persistence--list-sessions-cache
  (make-hash-table :test #'equal)
  "Last live session enumeration per workspace root, as (t . RESULT).

Decorative consumers reuse the newest live listing instead of rescanning
the workspace on every redraw.  Every live enumeration refreshes it.")

(defun niwa-session-persistence--control-artifacts (entries)
  "Return control probe results for ENTRIES, in one program.

Each candidate is probed for the three artifacts that make a directory a
session -- the obsolete PID lock, the portable lease, and the sidecar --
and every probe of every candidate travels in a single target program.
Probing them one at a time is one target process each, which is what made
enumerating a workspace cost hundreds of round trips.

A candidate proved free of a PID lock is recorded through
`niwa-session-durability-note-no-pid-lock', so the assertions the
publication read repeats per entry reuse this program's proof instead of
paying for their own.  Directories that do carry one are left alone: their
own assertion reports the authority violation at its canonical call site."
  (let* ((names '(".lock" ".lease" "session.meta.el"))
         (results
          (niwa-session-control-fs-run-program
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
          (niwa-session-durability-note-no-pid-lock entry))
        (push (cons entry entry-results) observed)))))

(defun niwa-session-persistence--lease-listings (entries)
  "Return an alist of ENTRIES to their observed lease-directory listings.

Every candidate's lease directory is listed in one target program.
`niwa-session-durability--generation-paths' accepts such an observation
in place of its own round trip, so the publication read below spends no
listing of its own; an empty or absent directory is recorded as `none',
which that helper distinguishes from no observation at all.  An entry whose
listing failed is left out, and lists for itself."
  (let ((results
         (niwa-session-control-fs-run-program
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

(defun niwa-session-persistence--directory-entries (directory regexp)
  "Observe real subdirectories in DIRECTORY matching REGEXP, with attributes."
  (condition-case nil
      (cl-loop for (path . attrs) in (directory-files-and-attributes directory t regexp)
               when (eq t (file-attribute-type attrs)) collect (cons path attrs))
    (file-missing nil)))

(defun niwa-session-persistence--publication-observations (entries)
  "Return control, listing and publication observations for ENTRIES."
  (when entries
    (let* ((controls (niwa-session-persistence--control-artifacts entries))
           (listings (niwa-session-persistence--lease-listings entries)))
      (list controls listings
            (if (file-remote-p (car entries))
                ;; Larger programs use the input carrier rather than the
                ;; optional archive; don't split a remote discovery into trips.
                (niwa-session-publication-read-batch entries listings)
              (mapcan (lambda (batch)
                        (niwa-session-publication-read-batch batch listings))
                      (seq-partition entries 32)))))))

(defun niwa-session-persistence--scan (workspace regexp)
  "Observe WORKSPACE session directories matching REGEXP and their publications."
  (niwa-session-durability-with-transaction
    (let ((entries (niwa-session-persistence--directory-entries
                    (niwa-session-artifacts-sessions-dir workspace) regexp)))
      (cons entries (niwa-session-persistence--publication-observations
                     (mapcar #'car entries))))))

(defun niwa-session-persistence--observed-update-time
    (updated-at sidecar-path save-path &optional attributes)
  "Return when the session at SAVE-PATH was last updated, or nil.
UPDATED-AT is authoritative when it parses as a session timestamp.
Otherwise the sidecar at SIDECAR-PATH lends its modification time, then the
directory itself.  ATTRIBUTES supplies the already-listed directory attributes."
  (or (niwa-session-persistence-parse-iso-time updated-at)
      (file-attribute-modification-time
       (or (and sidecar-path (file-attributes sidecar-path))
           attributes
           (file-attributes save-path)))))

(defun niwa-session-persistence--incompatible-entry
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
                              (niwa-session-persistence--observed-update-time
                               (plist-get status :updated-at)
                               sidecar-path save-path)
                            (file-error nil))))
          (format-time-string "%FT%H-%M-%S" time))
        :publication publication))

(defun niwa-session-persistence--discover-entry
    (entry authority-mode control-results listing &optional observation)
  "Return the discovery record for session directory ENTRY.
AUTHORITY-MODE is the workspace's portable profile.  CONTROL-RESULTS and
LISTING are this enumeration's legacy-control and lease observations.
OBSERVATION is its batched publication/sidecar read.  Failed or contradictory
controls signal, so the caller keeps an undiscoverable, uninspectable row."
  (niwa-session-codec-validate-authority-mode authority-mode)
  (when-let* ((failed
               (seq-find (lambda (result)
                           (not (memq (plist-get result :status) '(ok absent))))
                         control-results)))
    (niwa-session-control-fs-program-value failed))
  (if (and (eq 'absent (plist-get (nth 1 control-results) :status))
           (or (eq 'ok (plist-get (nth 0 control-results) :status))
               (eq 'ok (plist-get (nth 2 control-results) :status))))
      (niwa-session-persistence--incompatible-entry
       entry authority-mode
       (list :status 'unsupported
             :diagnostic (format niwa-session-persistence--pre-lease-message entry)))
    (when-let* ((err (plist-get observation :error)))
      (signal (car err) (cdr err)))
    (when (seq-find (lambda (result) (eq (plist-get result :status) 'ok))
                   control-results)
      ;; A shared scan may have run in a separate discovery transaction.
      (when (eq 'absent (plist-get (car control-results) :status))
        (niwa-session-durability-note-no-pid-lock entry))
      (niwa-session-codec-authority-mode-for-path entry nil authority-mode))
    (let* ((publication (if observation (plist-get observation :publication)
                          (niwa-session-publication-read entry nil listing)))
           (sidecar (plist-get publication :sidecar))
           (status
            (if sidecar
                (if observation
                    (let* ((classified
                            (niwa-session-persistence--classify-sidecar-data
                             (plist-get observation :sidecar-data)))
                           (data (plist-get classified :sidecar)))
                      (if data
                          (list :status 'current :summary
                                (niwa-session-persistence--sidecar-summary data))
                        classified))
                  (niwa-session-persistence--read-summary sidecar))
              (list :status 'missing
                    :diagnostic "Portable session has no committed publication"))))
      (if (eq (plist-get status :status) 'current)
          (list :save-path (file-name-as-directory entry)
                :summary (plist-get status :summary) :publication publication)
        (niwa-session-persistence--incompatible-entry
         entry authority-mode status publication sidecar)))))

(defun niwa-session-persistence--enumerate-sessions
    (workspace &optional cached scan)
  "Return compatible and incompatible persisted sessions for WORKSPACE.

The result is `(:sessions LIST :incompatible LIST)'.  When CACHED is non-nil,
reuse the last live enumeration when one exists.  SCAN is an optional fresh
observation from this picker invocation, with any deleted directories removed."
  (let ((root (niwa-workspace-root workspace)))
    (or (and cached
             (cdr (gethash
                   root
                   niwa-session-persistence--list-sessions-cache)))
        (let* ((authority-mode
                (niwa-session-authority-mode-for-workspace workspace))
               sessions
               incompatible)
          ;; One enumeration is one transaction: the PID-lock proofs and the
          ;; target clock reading that every entry's publication read repeats
          ;; are paid once for the whole listing.
          (niwa-session-durability-with-transaction
           (pcase-let ((`(,entries ,controls ,listings ,observations)
                        (or scan (niwa-session-persistence--scan
                                  workspace "\\`\\(?:[^.]\\|\\.niwa-save-as-\\)"))))
             (pcase-dolist (`(,entry . ,_) entries)
               (let ((record
                      (condition-case err
                          (niwa-session-persistence--discover-entry
                           entry authority-mode
                           (cdr (assoc entry controls))
                           (cdr (assoc entry listings))
                           (cdr (assoc entry observations)))
                        ;; Discovery itself failed, so not even the
                        ;; transcript can be trusted for inspection.
                        (error
                         (niwa-session-persistence--incompatible-entry
                          entry authority-mode
                          (list :status 'undiscoverable
                                :diagnostic (error-message-string err)))))))
                 (if (eq (plist-get record :kind) 'incompatible)
                     (push record incompatible)
                   (push record sessions))))))
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
                     niwa-session-persistence--list-sessions-cache)
            result)))))

(defun niwa-session-persistence-list-sessions (workspace &optional cached)
  "Return a list of `(:save-path :summary)' plists for WORKSPACE's sessions.

Sorted by `:updated-at' descending.  Incompatible sessions remain absent from
this compatible-only interface.  Portable sessions are listed only when their
lease names a valid immutable publication; fixed portable sidecars are ignored.

When CACHED is non-nil, reuse this process's last live enumeration for
WORKSPACE when one exists.  Only decorations tolerant of a listing as
old as the last picker, resume, or fork should pass it; anything that
decides authority or names sessions to the user enumerates live."
  (plist-get (niwa-session-persistence--enumerate-sessions workspace cached)
             :sessions))

(defun niwa-session-persistence-conversation-variants
    (session fork-point-id &optional sessions)
  "Return SESSION's persisted conversation variants at FORK-POINT-ID.
The Source is first, followed by direct Children.  Each returned session
entry has a `:variant-origin' of `source', `conversation', or `worktree'.
SESSIONS may supply an already-loaded session summary list."
  (let* ((session-id (niwa-session-session-id session))
         (source-id
          (if (equal fork-point-id
                     (niwa-session-forked-from-fork-point-id session))
              (niwa-session-forked-from-session-id session)
            session-id))
         variants)
    (dolist
        (entry
         (or sessions
             (when-let* ((workspace
                          (niwa-session-workspace session)))
               (niwa-session-persistence-list-sessions workspace))))
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

(defun niwa-session-persistence--format-session-candidate (entry &optional detail)
  "Return a `completing-read' display string for session ENTRY.
Shows a relative-time annotation first so the newest session is
easiest to recognise at a glance.  DETAIL, when given, names the machine
holding the session and is placed ahead of the message preview, which is the
one field with no bound on its length."
  (let* ((s        (plist-get entry :summary))
         (updated  (plist-get s :updated-at))
         (relative (niwa-session-rewind-format-relative-time updated))
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

(defun niwa-session-persistence--entry-live-buffer (entry)
  "Return the live root buffer already open for session ENTRY, or nil."
  (when-let* ((session-id (plist-get (plist-get entry :summary) :session-id)))
    (niwa-session-control-transfer-root-buffer-for-id session-id)))

(defun niwa-session-persistence--cleanup-reasons (assessment)
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

(defun niwa-session-persistence--entry-authority (workspace entry &optional assessment)
  "Return `(:action LABEL :detail TEXT :held BOOL)' for WORKSPACE session ENTRY.

The verb names the outcome of choosing this row, which the lease decides:
whether anyone is writing the session right now, not whether its files
happen to be local or reached over TRAMP.  DETAIL names the machine
involved, which is the part a verb cannot carry.  HELD says whether some
client is writing this session now, which is a narrower question than
whether the row resumes: an expired lease offers a takeover precisely
because nobody is writing it any more.

ASSESSMENT, when supplied, is the picker batch's observation of ENTRY."
  (ignore workspace)
  (cond
   ((when-let* ((buffer (niwa-session-persistence--entry-live-buffer entry)))
      (list :action "Switch"
            :detail (if (buffer-local-value 'niwa-session--read-only-mode
                                            buffer)
                        "already open here, read-only"
                      "already open here")
            :held t)))
   (t
    (let* ((status (or assessment (niwa-session-durability-cleanup-assessment
                                  (plist-get entry :save-path))))
           (host (plist-get status :host))
           (elsewhere (and host (not (equal host (system-name))) host)))
      (cond
       ((and (plist-get status :recoverable)
             ;; Pending captures alone are routine; the journal drains them on its own.
             (remq 'journal-pins (plist-get status :reasons)))
        (list :action "Recover"
              :detail (concat "cleanup blocked: "
                              (niwa-session-persistence--cleanup-reasons status))
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


(defun niwa-session-persistence--entry-action (workspace entry)
  "Return the entry action label for WORKSPACE session ENTRY."
  (plist-get (niwa-session-persistence--entry-authority workspace entry)
             :action))

(defun niwa-session-persistence--size-incompatible (entries)
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
       (niwa-session-control-fs-tree-sizes
        (mapcar (lambda (entry) (plist-get entry :save-path)) entries)))
    (error entries)))

(defun niwa-session-persistence--incompatible-reason (entry)
  "Return the human-readable incompatibility reason for ENTRY."
  (if (eq (plist-get entry :status) 'unsupported)
      (format "unsupported %s"
              (or (plist-get entry :version) "version missing"))
    (format "%s: %s" (plist-get entry :status) (plist-get entry :diagnostic))))

(defun niwa-session-persistence--format-incompatible-candidate (entry)
  "Return a `completing-read' display string for incompatible ENTRY."
  (format "%-12s  %-16s  %s  %s"
          (niwa-session-rewind-format-relative-time
           (plist-get entry :updated-at))
          (if-let* ((size (plist-get entry :size)))
              (file-size-human-readable size)
            "size unavailable")
          (niwa-session-persistence--incompatible-reason entry)
          (plist-get entry :save-path)))

(defun niwa-session-persistence--note-p (sidecar)
  "Return non-nil when SIDECAR describes an original note conversation."
  (and (or (plist-get sidecar :note-conversation)
           (eq (plist-get sidecar :preset-name) 'niwa-note))
       (not (plist-get sidecar :forked-from-session-id))))

(defun niwa-session-persistence--dedicated-p (workspace sidecar)
  "Return non-nil when SIDECAR is a note or artifact conversation in WORKSPACE.
Original note sessions are always hidden from ordinary chat selection.
An artifact session becomes ordinary when its artifact no longer names it."
  (or (niwa-session-persistence--note-p sidecar)
      (when-let* ((id (plist-get sidecar :dedicated-artifact)))
        (niwa-artifact-store-dedicated-p
         workspace (plist-get sidecar :session-id) id))))

(defun niwa-session-persistence-without-dedicated (workspace entries)
  "Return listing ENTRIES of WORKSPACE without artifact conversations.
An artifact's dedicated session is reached from its artifact, not listed
among the user's chats."
  (cl-remove-if (lambda (entry)
                  (niwa-session-persistence--dedicated-p
                   workspace (plist-get entry :summary)))
                entries))

(defvar niwa-session-persistence--shared-files-accepted-p nil
  "Non-nil after accepting the shared-project-files advisory in this Emacs.
This acknowledges a general risk, not authority over another session's
lease or files.  Refusing or quitting the prompt leaves it unacknowledged.")

(defun niwa-session-persistence-choose-entry (workspace)
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
      (setq result (niwa-session-persistence--choose-entry-once workspace refreshed)
            refreshed t))
    result))

(defun niwa-session-persistence--choose-entry-once (workspace refreshed)
  "Choose one WORKSPACE entry, or return `refresh' after a discard.
REFRESHED keeps an empty chooser open after deleting its last session."
  ;; Expired sessions and locks left behind by dead Emacsen are swept before
  ;; listing, so the chooser never offers a row that exists only because
  ;; nothing has cleaned up after a previous invocation.
  (let* ((scan (when (file-remote-p (niwa-workspace-root workspace))
                 (niwa-session-persistence--scan
                  workspace "\\`\\(?:[^.]\\|\\.niwa-save-as-\\)")))
         (enumeration
          (if scan
              (progn
                (niwa-session-persistence-cleanup-expired workspace nil scan)
                (niwa-session-persistence--enumerate-sessions workspace nil scan))
            (niwa-session-persistence-cleanup-expired workspace)
            (niwa-session-persistence--enumerate-sessions workspace)))
         (sessions (niwa-session-persistence-without-dedicated
                    workspace (plist-get enumeration :sessions)))
         (incompatible
          (niwa-session-persistence--size-incompatible
           (plist-get enumeration :incompatible))))
    (when (or sessions incompatible refreshed)
      (let* ((new-label "Start new session")
             (held-p nil)
             (assessments
              (when (and sessions (file-remote-p (niwa-workspace-root workspace)))
                (niwa-session-durability-assess-batch
                 (mapcar (lambda (entry) (plist-get entry :save-path)) sessions))))
             (session-candidates
              (mapcar
               (lambda (entry)
                 (let* ((authority
                         (if assessments
                             (niwa-session-persistence--entry-authority
                              workspace entry (cdr (assoc (plist-get entry :save-path) assessments)))
                           (niwa-session-persistence--entry-authority workspace entry)))
                        (action (plist-get authority :action)))
                   ;; Whether another writer is live is already answered by
                   ;; the row's own authority read; asking the target again per
                   ;; candidate is a second round trip for the same fact.
                   (when (plist-get authority :held)
                     (setq held-p t))
                   (cons
                    (format
                     "%-10s %s" action
                     (niwa-session-persistence--format-session-candidate
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
                   (niwa-session-persistence--format-incompatible-candidate
                    entry))
                  entry))
               incompatible))
             (candidates
              (append session-candidates incompatible-candidates))
             (choices (cons new-label (mapcar #'car candidates)))
             (choice
              (completing-read
               (if incompatible
                   (format "Niwa session (%d cannot resume): "
                           (length incompatible))
                 "Niwa session: ")
               (niwa--ordered-completion-table
                choices 'niwa-session-entry)
               nil t nil nil (car choices))))
        (if (equal choice new-label)
            (progn
              (when (and held-p
                         (not niwa-session-persistence--shared-files-accepted-p))
                (unless
                    (yes-or-no-p
                     (concat
                      "Another session writer is active. Independent sessions "
                      "can race over project files; use a Worktree Fork for "
                      "isolation. Start anyway? "))
                  (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "New session was not started"))
                (setq niwa-session-persistence--shared-files-accepted-p t))
              'new)
          (let ((entry (cdr (assoc choice candidates))))
            (if (eq (plist-get entry :kind) 'incompatible)
                (if (eq (plist-get entry :status) 'undiscoverable)
                    (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Session transcript is not safely inspectable: %s"
						   (plist-get entry :diagnostic))
                  (list
                   :action 'inspect
                   :buffer
                   (niwa-session-artifacts-inspect-cold-session
                    (plist-get entry :save-path)
                    (plist-get entry :publication) (niwa-diagnostic-scope workspace))))
              (let* ((assessment (plist-get entry :cleanup))
                     (action (when (plist-get assessment :recoverable)
                               (completing-read
                                "Retained session: "
                                '("Recover" "Discard" "Cancel") nil t nil nil "Recover"))))
                (cond
                 ((equal action "Cancel") (signal 'quit nil))
                 ((equal action "Discard")
                  (if (niwa-session-persistence-discard workspace entry)
                      'refresh
                    (signal 'quit nil)))
                 ((eq 'unknown (plist-get assessment :state))
                  (list :action 'inspect :buffer
                        (niwa-session-artifacts-inspect-cold-session
                         (plist-get entry :save-path)
                         (plist-get entry :publication)
                         (niwa-diagnostic-scope workspace))))
                 (t
                  (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
                    (let ((buffer (niwa-session-persistence-restore
                                   (plist-get entry :save-path) nil nil workspace))
                          (recovery (plist-get assessment :recovery)))
                      (when (equal action "Recover")
                        (message "Recovery opened; %s. %s"
                                 (niwa-session-persistence--cleanup-reasons assessment)
                                 (cond
                                  ((and (listp recovery) (plist-get recovery :directory))
                                   (format "Inspect recovery data at %s before explicitly abandoning it."
                                           (plist-get recovery :directory)))
                                  ((memq 'unsettled-mutation (plist-get assessment :reasons))
                                   "Verify previous processes stopped before M-x niwa-retry-target-readiness.")
                                  (t "Review the last committed transcript; prior client-local staging may be unavailable."))))
                      buffer))))))))))))

(defun niwa-session-persistence-choose-conversation-variant
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
          (niwa--ordered-completion-table
           displays 'niwa-conversation-variant))
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
(defun niwa-save-session (&optional arg)
  "Save the current niwa session to disk explicitly.

Forces a save even when nothing has changed since the last
auto-save (useful after manual edits to the chat buffer).  Triggers lazy
materialization if the session has not yet hit disk.

With a prefix ARG, prompts for a new session name and creates an independent
child under a fresh id.  File-workspace sessions clone their directory.
Portable project sessions materialize only the parent's current committed
logical artifacts and start fresh lease and publication histories.  The
current buffer is repointed at the child; the parent remains resumable.

To rename the current session in place, use `niwa-rename-session'."
  (interactive "P")
  (let* ((data-buf
          (cond
           ((and (boundp 'niwa--session) niwa--session) (current-buffer))
           ((and (boundp 'niwa--data-buffer) niwa--data-buffer
                 (buffer-live-p niwa--data-buffer))
            niwa--data-buffer)
           (t (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Not in a niwa chat or view buffer"))))
         (session (buffer-local-value 'niwa--session data-buf))
         (scope (niwa-diagnostic-scope (or session data-buf))))
    (unless session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope data-buf) "Active buffer has no niwa session"))
    (niwa-with-diagnostic-errors scope
      (when arg
        (niwa-engine-assert-local-history session "Save As"))
      (niwa-session-artifacts-assert-mutation-authority session data-buf))
    (when arg
      (when (niwa-agent-control-active-turn-p session)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Interrupt active agent turns before save-as"))
      ;; Queued input would otherwise be delivered from both copies.
      (when (niwa-session-pending-input-p session)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
         "Resolve pending input in the Pending Inputs cockpit or clear it with C-c C-q before save-as")))
    (niwa-with-diagnostic-errors scope
      (cond
       (arg
        (niwa-session-persistence--save-as session data-buf))
       (t
        (niwa-session-artifacts-save session data-buf nil t)
        (message "Session saved."))))))

(defun niwa-session-persistence--save-as (session data-buf)
  "Save SESSION as an independent child under a fresh id.
Called from `niwa-save-session' with a prefix arg.  Copy committed logical
artifacts and retained execution output, excluding unfinished spools.
Repoint DATA-BUF at the child after it commits."
  ;; Force materialization so the child has a durable parent.
  (unless (niwa-session-save-path session)
    (niwa-session-artifacts-save session data-buf nil t))
  (let* ((old-save-path (niwa-session-save-path session))
         (new-name (read-string
                    "Save session as (new name): "
                    (niwa-session-name session)))
         (display-name (niwa-session-naming-normalize new-name))
         (parent-dir (file-name-directory
                      (directory-file-name old-save-path)))
         (new-id (niwa-session-persistence-allocate-session-id parent-dir))
         (new-save-path (file-name-as-directory
                         (file-name-concat parent-dir new-id))))
    ;; Publish the parent completely before deriving the child.
    (with-current-buffer data-buf (niwa-session-naming-cancel))
    (niwa-session-artifacts-save session data-buf)
    (niwa-session-save-as-run
     session data-buf display-name new-id new-save-path)
    (message "Session saved as %s" display-name)
    new-save-path))


;;
;;; Auto-cleanup

(defvar niwa-session-persistence--cleanup-throttle
  (make-hash-table :test #'equal)
  "Workspace keys mapped to the `float-time' of their last expiry sweep.

Keyed on `(WORKSPACE-TYPE . WORKSPACE-ID)'.  In memory only, so a new
Emacs sweeps at its first opportunity.")

(defvar niwa-session-persistence--cleanup-interval (* 24 60 60)
  "Least seconds between two expiry sweeps of one workspace.
Ages are counted in days, so sweeping more often finds nothing new,
while a long-running Emacs must still sweep again.")

(defvar niwa-session-persistence--cleanups (make-hash-table :test #'equal)
  "Workspace keys with a queued expiry sweep.")

(defun niwa-session-persistence--cleanup-key (workspace)
  "Return WORKSPACE's expiry-sweep throttle key."
  (cons (niwa-workspace-type workspace)
        (niwa-workspace-id workspace)))

(defun niwa-session-persistence--cleanup-due-p (workspace)
  "Return non-nil when WORKSPACE has not been swept within the interval."
  (let ((last (gethash (niwa-session-persistence--cleanup-key workspace)
                       niwa-session-persistence--cleanup-throttle)))
    (or (null last)
        (>= (- (float-time) last)
            niwa-session-persistence--cleanup-interval))))

(defun niwa-session-persistence-schedule-cleanup (workspace)
  "Offer WORKSPACE an expiry sweep once Emacs is idle, when one is due.

Session activity calls this so a long-running Emacs keeps expiring old
sessions without waiting for the chooser or for exit.  The sweep runs
after the current command, when the workspace's transport is idle and
no input is pending.  A batch Emacs never sweeps from activity, for the
same reason it does not sweep at exit.  Return the queued timer, or
nil when nothing was queued."
  (when (and workspace
             niwa-session-max-age-days
             (not noninteractive)
             (niwa-session-persistence--cleanup-due-p workspace))
    (niwa-transport-schedule-idle
     niwa-session-persistence--cleanups
     (niwa-session-persistence--cleanup-key workspace)
     'session-cleanup
     (niwa-session-artifacts-sessions-dir workspace)
     (lambda ()
       (condition-case err
           (niwa-session-persistence-cleanup-expired workspace)
         (error
          (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "Session cleanup failed: %s"
                   (error-message-string err))))))))

(defun niwa-session-persistence-parse-iso-time (str)
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

(defun niwa-session-persistence--discard-identity (workspace entry &optional session)
  "Validate WORKSPACE's selected ENTRY and return its cold SESSION identity.
When SESSION is non-nil, recheck its target without accepting replacement."
  (unless (eq 'portable (niwa-session-authority-mode-for-workspace workspace))
    (user-error "Explicit recovery discard requires a portable project session"))
  (let* ((directory (file-name-as-directory
                     (niwa-session-control-fs-physical-path
                      (plist-get entry :save-path))))
         (sessions-directory (file-name-as-directory
                              (niwa-session-control-fs-physical-path
                               (niwa-session-artifacts-sessions-dir workspace)))))
    (when (niwa-session-control-fs-async-busy-p directory)
      (user-error "Session has a pending local control program; retry when it settles"))
    (unless (equal sessions-directory
                   (file-name-directory (directory-file-name directory)))
      (user-error "Session does not belong to this workspace's session directory"))
    (when (niwa-session-persistence--entry-live-buffer entry)
      (user-error "Session is open in this Emacs; close it before discarding"))
    (let* ((publication (niwa-session-publication-read directory))
           (sidecar (and publication
                         (niwa-session-persistence-load-sidecar
                          (plist-get publication :sidecar)
                          (niwa-diagnostic-scope workspace))))
           (identity (niwa-workspace-identity-read (niwa-workspace-root workspace))))
      (unless (and sidecar identity
                   (eq 'portable (plist-get sidecar :authority-mode))
                   (equal identity (plist-get (plist-get sidecar :workspace) :workspace-id))
                   (equal (plist-get sidecar :session-id)
                          (plist-get (plist-get entry :summary) :session-id))
                   (equal (plist-get publication :head)
                          (plist-get (plist-get entry :publication) :head)))
        (user-error "Session identity or publication changed; select it again"))
      (let ((target (if session (niwa-session-execution-target session)
                      (niwa-execution-target-create (niwa-workspace-root workspace)))))
        (unless session
          (niwa-execution-target-restore-incarnation
           target (plist-get sidecar :target-incarnation)))
        (if (and (not session) (niwa-execution-target-remote-p target))
            (let ((readiness (niwa-execution-target-probe target t 'off)))
              (unless (eq 'ready (plist-get readiness :status))
                (user-error "Execution target is unavailable: %s"
                            (or (plist-get readiness :error) (plist-get readiness :reason)))))
          (niwa-execution-target-observe-incarnation target))
        (when (niwa-execution-target-incarnation-changed-p target)
          (user-error "Execution target changed; refusing session discard"))
        (or session
            (niwa-session--create
             :workspace workspace :authority-mode 'portable :execution-target target
             :session-id (plist-get sidecar :session-id)
             :name (plist-get sidecar :session-name) :save-path directory
             :publication publication))))))

(defun niwa-session-persistence-discard (workspace entry)
  "Confirm and discard WORKSPACE's retained session ENTRY without restoring it.
Refuse changed identities and live owners.  Captures and recovery are settled
through their owners before deletion; failure may retain approved omissions."
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
    (let* ((assessment (niwa-session-durability-cleanup-assessment
			(plist-get entry :save-path)))
           (session (niwa-session-persistence--discard-identity workspace entry))
           (directory (niwa-session-save-path session)))
      (unless (memq (plist-get assessment :state) '(available expired))
	(user-error "Session cannot be discarded: %s"
                    (niwa-session-persistence--cleanup-reasons assessment)))
      (when (yes-or-no-p
             (format (concat "Discard session %s (%s) permanently, including its transcript, "
                             "recovery data and pending journal captures? Approved capture/recovery "
                             "actions may persist if deletion fails. Project-file effects remain. "
                             "Confirm the previous client and its child writers have stopped: ")
                     (niwa-session-name session)
                     (or (and (plist-get assessment :reasons)
                              (niwa-session-persistence--cleanup-reasons assessment))
			 "saved session data")))
	(condition-case err
            (niwa-session-durability-discard-session
             session
             (lambda ()
               (niwa-session-persistence--discard-identity workspace entry session)
               (dolist (proof (cdr (niwa-journal-pins-observe directory)))
		 (when (eq 'verify (plist-get proof :op))
                   (let* ((id (file-name-base (plist-get proof :path)))
                          (capture (niwa-journal-capture--read workspace id)))
                     (unless (and capture
                                  (equal (niwa-session-session-id session)
					 (plist-get (plist-get capture :metadata) :session)))
                       (user-error "Journal capture %s is unreadable or belongs to another session" id))
                     (niwa-journal-process-discard workspace id directory))))
               (when (niwa-session-recovery-read directory)
		 (niwa-session-recovery--abandon session))
               (when (plist-get (niwa-session-lease session) :unsettled-mutation)
		 (unless (niwa-session-durability-set-unsettled-mutation session nil)
                   (user-error "Could not acknowledge unresolved mutation")))
               (niwa-session-persistence--discard-identity workspace entry session)))
          (error (user-error "Session retained; approved recovery/capture actions may have completed: %s"
                             (error-message-string err))))))))

(cl-defun niwa-session-persistence--delete-saved
    (workspace save-path max-age-seconds
               &optional (note-session-ids 'unread))
  "Delete saved session SAVE-PATH unless something may still hold it.
Journal pins always keep it.  Deletion uses
`niwa-session-durability-delete-abandoned' with MAX-AGE-SECONDS.
Original note conversations
require proof that WORKSPACE no longer references them.  NOTE-SESSION-IDS
is the sweep's reference snapshot, or `unread' to obtain it here.
Return non-nil only when the directory was deleted."
  (and (not (niwa-journal-pins-present-p save-path))
       (let ((sidecar (condition-case nil
                          (let* ((publication
                                  (niwa-session-publication-read save-path))
                                 (data (and publication
                                            (niwa-session-codec-read
                                             (plist-get publication :sidecar)))))
                            (and (listp data) data))
                        (error nil))))
         (not
          (when (or (niwa-session-persistence--note-p sidecar)
                    (not (and (plist-member sidecar :preset-name)
                              (symbolp (plist-get sidecar :preset-name)))))
            (when (eq note-session-ids 'unread)
              (require 'niwa-persistence)
              (setq note-session-ids
                    (niwa-instruction-store-note-session-ids workspace)))
            (or (eq note-session-ids t)
                (member (plist-get sidecar :session-id) note-session-ids)
                (member (file-name-nondirectory (directory-file-name save-path))
                        note-session-ids)))))
       (niwa-session-durability-delete-abandoned
        save-path max-age-seconds)))

(defun niwa-session-persistence-delete (workspace save-path)
  "Delete WORKSPACE's saved session at SAVE-PATH now.
The safety rules of `niwa-session-persistence-cleanup-expired' apply
without its age cap: a session a client may still hold, or whose journal
captures are pending, is kept.  Referenced note conversations and uncertain
note stores are kept too.  Return non-nil only when it was deleted."
  (niwa-session-persistence--delete-saved
   workspace save-path 0))

(defun niwa-session-persistence-cleanup-expired (workspace &optional force scan)
  "Delete sessions older than `niwa-session-max-age-days'.

Scans session directories independently of resume compatibility.  Uses
`:updated-at' when available, otherwise the sidecar or directory modification
time.

The `niwa-session-keep-recent-count' most-recently-updated sessions are
never deleted regardless of age.  Original note conversations do not count
toward that floor, and are eligible only after their notes leave the store;
an uncertain reference check retains them.  Pending journal capture pins prevent
deletion.  Sessions open in this Emacs are skipped; the others use
`niwa-session-durability-delete-abandoned', which also requires the last
lease renewal to be older than the cap.  Throttled to at most one sweep
per `(workspace-type . workspace-id)' within
`niwa-session-persistence--cleanup-interval'; when FORCE is non-nil
the throttle is bypassed.  SCAN may supply the picker's fresh directory and
publication observations; entries deleted by the sweep are removed from it.

Returns the number of sessions deleted, or nil when the cap is nil or the
workspace was swept too recently."
  (when niwa-session-max-age-days
    (let ((ws-key (niwa-session-persistence--cleanup-key workspace)))
      (when (or force (niwa-session-persistence--cleanup-due-p workspace))
        (puthash ws-key (float-time)
                 niwa-session-persistence--cleanup-throttle)
        (let ((threshold-secs (* niwa-session-max-age-days 24 60 60))
              (now            (float-time))
              (deleted        0)
              (candidates     nil)
              (note-candidates nil)
              (note-session-ids 'unread))
          (pcase-let ((`(,entries ,_controls ,_listings ,observations)
                       (or scan (niwa-session-persistence--scan workspace "\\`[^.]"))))
            (dolist (entry entries)
              (let ((save-path (car entry)))
                (unless (string-prefix-p "." (file-name-nondirectory save-path))
                  (let* ((observation (cdr (assoc save-path observations)))
                         (publication (plist-get observation :publication))
                         (data (plist-get observation :sidecar-data))
                         (sidecar (and (listp data) data))
                         (parsed-time
                          (niwa-session-persistence--observed-update-time
                           (plist-get sidecar :updated-at)
                           (plist-get publication :sidecar) save-path (cdr entry))))
                    (let ((candidate (list save-path
                                           (and parsed-time (float-time parsed-time))
                                           (plist-get sidecar :session-id))))
                      (cond
                       ((or (niwa-session-persistence--note-p sidecar)
                            (not (and (plist-member sidecar :preset-name)
                                      (symbolp (plist-get sidecar :preset-name)))))
                        (when (eq note-session-ids 'unread)
                          (require 'niwa-persistence)
                          (setq note-session-ids
                                (niwa-instruction-store-note-session-ids workspace)))
                        (unless (or (eq note-session-ids t)
                                    (member (plist-get sidecar :session-id) note-session-ids)
                                    (member (file-name-nondirectory (directory-file-name save-path))
                                            note-session-ids))
                          (if (niwa-session-persistence--note-p sidecar)
                              (push candidate note-candidates)
                            (push candidate candidates))))
                       ((not (niwa-session-persistence--dedicated-p workspace sidecar))
                        (push candidate candidates)))))))))
          ;; The newest sessions are exempt regardless of age: the chooser
          ;; sweeps before any lock is taken, so without this floor a
          ;; 30-day absence would delete the session the user came back to
          ;; resume.  A session without a readable timestamp sorts oldest,
          ;; so it never displaces a dated one from the floor.
          (setq candidates
                (sort candidates
                      (lambda (a b) (> (or (cadr a) 0) (or (cadr b) 0)))))
          (when niwa-session-keep-recent-count
            (setq candidates
                  (nthcdr niwa-session-keep-recent-count candidates)))
          ;; Note conversations never consume the ordinary retention floor.
          (pcase-dolist (`(,save-path ,updated-secs ,session-id)
                         (append note-candidates candidates))
            (when (and updated-secs
                       (> (- now updated-secs) threshold-secs)
                       ;; A session open here may outlive its lease after
                       ;; suspend.
                       (not (and session-id
                                 (niwa-session-control-transfer-root-buffer-for-id
                                  session-id)))
                       ;; One unreadable or contested session must not end
                       ;; the sweep.
                       (condition-case nil
                           (niwa-session-persistence--delete-saved
                            workspace save-path threshold-secs note-session-ids)
                         (error nil)))
              (cl-incf deleted)
              (when scan
                (setcar scan (assoc-delete-all save-path (car scan))))))
          (when (> deleted 0)
            (message "Cleaned up %d expired session%s"
                     deleted (if (= deleted 1) "" "s")))
          deleted)))))


;;
;;; Hook plumbing

;;; Exit persistence

;; Turn settlement saves success, error, and abort before request teardown,
;; while request-local file-history checkpoints are still available.

(defun niwa-session-persistence--allow-emacs-exit-p ()
  "Return non-nil when every live session may exit safely."
  (let* ((unresolved nil)
         (scope 'unavailable)
         (blocker
         (cl-some
          (lambda (buffer)
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (and (boundp 'niwa--session)
                              niwa--session)
                     (setq scope (niwa-diagnostic-scope niwa--session))
                     (cond
                      ;; Queued and in-flight async publication drains here,
                      ;; leaving the session usable if another query vetoes.
                      ((and (niwa-session-persistence--gate-owner-p
                             niwa--session)
                            (niwa-session-publication-busy-p
                             niwa--session)
                            (condition-case err
                                (progn
                                  (niwa-session-persistence--drain-temporarily
                                   niwa--session)
                                  nil)
                              (error (setq unresolved
                                           (error-message-string err))
                                     t)))
                       'publication-job)
                      ((niwa-session-pending-publication
                        niwa--session)
                       'publication)
                      ((and
                        (not
                         (eq 'foreign
                             (plist-get
                              (niwa-session-lease niwa--session)
                              :state)))
                        (niwa-execution-unsettled-mutation-p
                         niwa--session))
                       'mutation))))))
          (buffer-list))))
    (pcase blocker
      ('publication-job
       (niwa-diagnostic-message scope
        (concat
         "Session publication is unresolved (%s); run "
         "niwa-session-publication-retry or "
         "niwa-session-publication-abandon first")
        unresolved)
       nil)
      ('publication
       (niwa-diagnostic-message scope
        (concat
         "Session publication is pending; run "
         "niwa-session-publication-retry or "
         "niwa-session-publication-abandon first"))
       nil)
      ('mutation
       (niwa-diagnostic-message scope
        (concat
         "Target mutation is unsettled; stop live executions or run "
         "niwa-retry-target-readiness to acknowledge it first"))
       nil)
      (_ t))))

(defun niwa-session-persistence--stop-publications ()
  "Stop admissions and drain publication jobs of every live session.
Return the sessions whose job stayed unresolved; each is warned about once.
Their state and leases are left for explicit publication recovery."
  (let (sessions blocked)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (when-let* ((session (buffer-local-value 'niwa--session buffer)))
          (cl-pushnew session sessions :test #'eq))))
    (maphash (lambda (session _) (cl-pushnew session sessions :test #'eq))
             niwa-session-persistence--deferred-agent-saves)
    (dolist (session sessions)
      (when (niwa-session-persistence--gate-owner-p session)
        (condition-case err
            (niwa-session-publication-stopping session)
          (error
           (push session blocked)
           (niwa-diagnostic-warning (niwa-diagnostic-scope session)
            'niwa
            (format "Session publication is unresolved at exit; not saving or releasing: %s"
                    (error-message-string err))
            :warning)))))
    blocked))

(defun niwa-session-persistence--kill-emacs-hook ()
  "Save sessions, clean expired state, and release locks on Emacs exit.

Runs unconditionally so that locks don't outlive the Emacs process
that wrote them.  Best-effort: individual errors are swallowed so one
bad buffer can't block exit.  Publication jobs are drained and their
admissions stopped before execution teardown.  A session whose job stays
unresolved is neither saved, flushed nor released, so explicit recovery
remains possible; execution teardown still runs."
  (let* ((niwa-journal-process--inhibit-scheduling t)
        (niwa-memory-decision--inhibit-recovery t)
        (niwa-memory-pass--inhibit-scheduling t)
        (inhibit-quit t)
        (blocked (niwa-session-persistence--stop-publications)))
    (niwa-transport-cancel-idle
     niwa-session-persistence--autosaves 'conversation-autosave)
    (when (featurep 'niwa-journal-process)
      (ignore-errors (niwa-journal-process-stop-all)))
    (when (featurep 'niwa-memory-decision)
      (ignore-errors (niwa-memory-decision-stop-recovery)))
    (when (featurep 'niwa-memory-pass)
      (ignore-errors (niwa-memory-pass-stop-all)))
    (when (fboundp 'niwa-execution-teardown-all)
      (ignore-errors (niwa-execution-teardown-all)))
    ;; Agent buffers are deliberately rejected by the root writer.  Flush
    ;; them through their owner before saving sidecars or releasing leases.
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   niwa--agent-invocation))
        (unless (memq (buffer-local-value 'niwa--session buffer) blocked)
          (niwa-session-persistence-autosave-buffer buffer))))
    ;; A debounced agent-state save left in its window would be lost, and
    ;; registry mutations do not mark the root buffer modified, so the
    ;; modified-only save loop below cannot cover them.
    (maphash (lambda (session timer)
               (cancel-timer timer)
               (unless (memq session blocked)
                 (niwa-session-persistence-autosave-buffer
                  (niwa-session-root-buffer session))))
             niwa-session-persistence--deferred-agent-saves)
    (clrhash niwa-session-persistence--deferred-agent-saves)
    (let (lock-dirs)
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and (boundp 'niwa--session)
                       niwa--session
                       (not niwa--agent-invocation)
                       (not (memq niwa--session blocked)))
              (when (or (buffer-modified-p)
                        niwa-session--save-failed
                        (niwa-session-publication-uncommitted-batches
                         niwa--session))
                (niwa-session-persistence-autosave-buffer
                 (niwa-session-persistence-authoritative-buffer buf)))
              ;; Seal only the completed checkpoint, even if the exit save
              ;; above included an unfinished response.  Exit starts no model.
              (ignore-errors
                (niwa-journal-capture-seal-and-schedule
                 niwa--session buf 'session-end))
              ;; An unmodified buffer still owes its queued diagnostics: a
              ;; deferred remote flush never fires once Emacs is exiting,
              ;; and an asynchronous append would die with it.
              (ignore-errors
                (let ((niwa-session-control-fs--async-wait t))
                  (niwa-session-persistence--flush-diagnostic-logs-now
                   niwa--session)))
              (when-let* ((dir (niwa-session-save-path niwa--session)))
                (cl-pushnew dir lock-dirs :test #'equal))))))
      ;; Keep live locks through cleanup so an exit-save failure cannot expose
      ;; an old session directory for deletion.  A batch Emacs (a script or
      ;; test subprocess) only registered workspaces incidentally, often its
      ;; working directory, and must not sweep the user's sessions.
      (when (and (not noninteractive)
                 (boundp 'niwa-workspace--registry)
                 (hash-table-p niwa-workspace--registry))
        (maphash
         (lambda (_ workspace)
           ;; Like activation, never open a connection here: C-g is
           ;; inhibited, so an unreachable host would hang the exit.
           (let ((root (niwa-workspace-root workspace)))
             (unless (and (stringp root) (file-remote-p root)
                          (not (file-remote-p root nil t)))
               (ignore-errors
                 (niwa-session-persistence-cleanup-expired workspace)))))
         niwa-workspace--registry))
      (niwa-session-persistence--wait-async)
      (dolist (dir lock-dirs)
        (condition-case _
            (niwa-session-persistence-lock-release dir)
          (error nil))))))

;; Install at file-load time so exit persistence runs even when the user
;; never called `niwa-install' this Emacs (e.g. running `niwa' to
;; resume is the only command invoked).  Duplicate adds are no-ops by `add-hook'.
(add-hook 'kill-emacs-query-functions
          #'niwa-session-persistence--allow-emacs-exit-p)
(add-hook 'auto-save-hook #'niwa-session-persistence-autosave)
(add-hook 'kill-emacs-hook #'niwa-session-persistence--kill-emacs-hook)


(provide 'niwa-session-persistence)

;;; niwa-session-persistence.el ends here
