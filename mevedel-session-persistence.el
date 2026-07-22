;;; mevedel-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Persistence layer for mevedel chat sessions.
;;
;; Handles sidecar serialization, lazy materialization, per-turn
;; autosave, per-session file-history snapshots, split-on-compact
;; segment rotation, lock files (with cross-host conflict handling),
;; resume, rewind, fork-on-send, session rename, workspace
;; relocation reconciliation, and auto-cleanup of stale sessions.
;;
;; Sidecar plist shape:
;;
;;   (:version "v0.5.0"
;;    :session-id "main-2026-04-23T14-30-a9f2"
;;    :session-name "main"
;;    :workspace (:type project :id ID :root ROOT :name NAME)
;;    :created-at "..." :updated-at "..."
;;    :current-segment 3 :total-turn-count 47
;;    :first-user-message "..."
;;    :latest-user-message "..."
;;    :task-status-notes ((nil :note "..." :updated-turn 12) ...)
;;    :forked-from-session-id nil :forked-from-turn nil
;;    :permission-mode ask
;;    :plan-mode nil
;;    :permission-rules ((TOOL-NAME ...) ...)
;;    :resource-grants ((:path "/abs/path" :access read) ...)
;;    :additional-roots (("name" . "/abs/path") ...)
;;    :prompt-index ((SEGMENT-N . ((:turn N :pos POS :preview STR :timestamp STR) ...)) ...)
;;    :file-snapshots ((TURN-N . ((PATH . (:backup-name STR :version INT
;;                                          :backup-time STR :file-mtime STR)) ...)) ...))
;;
;; Hash-table-valued slots on the session struct (`touched-files',
;; `mentions-shown') are NOT persisted.  Both reset to empty hash
;; tables on load; the consequence is that an LLM coming back from a
;; resume may re-Read files that were already read pre-resume (over-
;; dedup is worse than re-expansion).  Tasks are serialized as plists
;; in `:tasks' and deserialized on load.  Owner-scoped task status
;; notes are serialized in `:task-status-notes'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-structs))

(require 'mevedel-transcript)

;; `diff'
(declare-function diff "diff" (old new &optional switches no-async))

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

;; `mevedel-execution'
(declare-function mevedel-execution-relocate-artifacts
		  "mevedel-execution" (session old-root new-root))
(declare-function mevedel-execution-session-live-p "mevedel-execution"
		  (session))
(declare-function mevedel-execution-teardown-all "mevedel-execution"
		  nil)

;; `mevedel-hooks'
(declare-function mevedel-hooks--persist-log-entry "mevedel-hooks"
		  (session entry))

;; `mevedel-permission-log'
(declare-function mevedel-permission-log--persist
		  "mevedel-permission-log" (session entry))

;; `mevedel-persistence'
(declare-function mevedel--load-instructions-file
		  "mevedel-persistence"
		  (path &optional base-directory confirm quiet
			workspace))
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

;; `mevedel-reminders'
(declare-function mevedel-reminders-clone-list "mevedel-reminders"
		  (reminders))

;; `mevedel-structs'
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
(declare-function mevedel-session--create "mevedel-structs"
		  (&rest slots))
(declare-function mevedel-session-agent-turn-capacity
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-current-segment "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-file-snapshots "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-forked-from-session-id
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-turn "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
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
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry"
		  (span &rest props))
(declare-function mevedel-telemetry-record "mevedel-telemetry"
		  (session event &rest props))
(declare-function mevedel-telemetry-start "mevedel-telemetry"
		  (session event &rest props))

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair--persist-event
		  "mevedel-tool-repair-diagnostics" (session event))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-records
		  "mevedel-transcript-audit" (text &optional type))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state
		  "mevedel-transcript-restore" nil)
(declare-function mevedel-transcript-restore-properties
		  "mevedel-transcript-restore"
		  (&optional only-if-missing))
(declare-function mevedel-transcript-restore-sanitize-bounds
		  "mevedel-transcript-restore" nil)

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" nil)
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-agent'
(declare-function mevedel-view-reset-agent-ephemeral-state
		  "mevedel-view-agent" (&optional view-buffer))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-history'
(declare-function advice-add "nadvice"
		  (symbol where function &optional props))
(declare-function advice-member-p "nadvice" (advice symbol))
(declare-function gptel--get-buffer-bounds "ext:gptel" nil)
(declare-function gptel--save-state "ext:gptel" nil)
(declare-function gptel-get-preset "ext:gptel" (name))
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-org--restore-state "ext:gptel-org" nil)
(declare-function mevedel--chat-buffer-disable-org-element-cache
		  "mevedel-chat" nil)
(declare-function mevedel--chat-buffer-init-common "mevedel-chat"
		  (buf workspace source))
(declare-function mevedel--normalize-session-directory "mevedel-chat"
		  (directory workspace))
(declare-function mevedel-request-file-snapshots "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-buffer-name "mevedel-structs"
		  (session-name workspace))
(declare-function mevedel-view-history-load "mevedel-view-history"
		  (&optional session))
(declare-function mevedel-view-history-save "mevedel-view-history"
		  (&optional view-buffer))
(declare-function mevedel-workspace "mevedel-workspace"
		  (&optional buffer))
(declare-function mevedel-workspace-ensure-generated-state-ignored
		  "mevedel-workspace" (workspace))
(declare-function mevedel-workspace-get-or-create "mevedel-structs"
		  (type id root name))
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(defvar gptel--preset)
(defvar gptel-display-buffer-action)
(defvar gptel-mode)
(defvar gptel-system-prompt)
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar mevedel-permission-mode)
(defvar mevedel-workspace-additional-roots)
(defvar so-long-predicate)

;; `mevedel-view-render'
(declare-function mevedel-view--rebase-data-sources
		  "mevedel-view-render" (delta))

;; `org'
(declare-function org-entry-delete "ext:org" (pom property))
(declare-function org-entry-get "ext:org"
		  (pom property &optional inherit literal-nil))
(declare-function org-entry-put "ext:org" (epom property value))
(defvar org-agenda-file-menu-enabled)

;;
;;; Customization

(defcustom mevedel-sessions-directory (file-name-concat ".mevedel" "sessions")
  "Directory where session persistence files live.

Relative paths resolve against the workspace root at save time;
absolute paths are used as-is."
  :type 'directory
  :group 'mevedel)

(defcustom mevedel-file-history-max-snapshots 100
  "Maximum number of per-turn file snapshots retained per session.

Snapshots beyond this cap are evicted oldest-first; backup files no
longer referenced by any retained snapshot are deleted (refcount GC).
nil disables eviction (snapshots accumulate indefinitely)."
  :type '(choice (integer :tag "Max snapshots")
          (const :tag "No limit" nil))
  :group 'mevedel)

(defcustom mevedel-file-history-max-snapshot-bytes (* 1024 1024)
  "Soft size cap (bytes) for individual file snapshots.

Files larger than this are skipped at snapshot time with a warning.
Defaults to 1 MB."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-max-age-days 30
  "Auto-cleanup threshold for old sessions, in days.

Sessions whose `:updated-at' is older than this are eligible for
deletion when `mevedel-resume' runs (throttled per Emacs invocation).
Sessions with active locks are always skipped: any cross-host lock, or
a same-host lock whose PID is live and not known to have been reused.  A
nil value disables auto-cleanup entirely."
  :type '(choice (integer :tag "Days")
          (const :tag "Disabled" nil))
  :group 'mevedel)


;;
;;; Constants

(defconst mevedel-session-persistence--allowed-permission-actions
  '(allow deny ask)
  "Permission rule actions recognised by this version.
Rules with other actions are dropped on load (a future version may
add more, and we don't want to act on actions we don't understand).")

(defconst mevedel-session-persistence--required-sidecar-keys
  '(:version :session-id :session-name :workspace :working-directory
    :created-at :updated-at :current-segment :total-turn-count
    :last-task-write-turn :task-status-notes :first-user-message
    :latest-user-message :forked-from-session-id :forked-from-turn
    :permission-mode :plan-mode :permission-rules :resource-grants
    :preset-name :preset-settings
    :last-observed-date
    :agent-types-snapshot :skills-snapshot :additional-roots :tasks
    :prompt-index :file-snapshots :agent-transcripts :agent-registry
    :agent-turn-capacity :plan-metadata :goal :messages)
  "Keys required in every current-version session sidecar.")


;;
;;; Workspace serialization

(defun mevedel-session-persistence--workspace-to-plist (workspace)
  "Convert WORKSPACE to a plist for sidecar storage.

Captures only the identity tuple needed to re-register the workspace on
load: type, id, root, and name.  The file cache is process-local and
not persisted."
  (when workspace
    (list :type (mevedel-workspace-type workspace)
          :id   (mevedel-workspace-id workspace)
          :root (mevedel-workspace-root workspace)
          :name (mevedel-workspace-name workspace))))

(defun mevedel-session-persistence--workspace-from-plist (plist)
  "Resolve WORKSPACE from PLIST.

Looks up the workspace in the registry by (type . id); if not found,
registers a fresh one from the saved tuple.  The current registry's
`root' wins if it differs from the saved root (workspace relocation
is handled by the read path, not here)."
  (when plist
    (mevedel-workspace-get-or-create
     (plist-get plist :type)
     (plist-get plist :id)
     (plist-get plist :root)
     (plist-get plist :name))))


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
    (plist workspace)
  "Return PLIST's restored working directory for WORKSPACE.

When WORKSPACE is available, paths saved under a relocated workspace
root are first mapped to the current root unless
they already live under that current root, then checked with the same
containment semantics as session creation."
  (let* ((raw (plist-get plist :working-directory))
         (saved-workspace (plist-get plist :workspace))
         (saved-root (plist-get saved-workspace :root))
         (current-root (and workspace
                            (mevedel-workspace-root workspace)))
         (saved-prefix (and saved-root
                            (file-name-as-directory
                             (expand-file-name saved-root))))
         (current-prefix (and current-root
                              (file-name-as-directory
                               (expand-file-name current-root))))
         (dir (and raw
                   (file-name-as-directory
                    (expand-file-name raw)))))
    (when (and dir saved-prefix current-prefix
               (not (equal saved-prefix current-prefix))
               (not (string-prefix-p current-prefix dir))
               (string-prefix-p saved-prefix dir))
      (setq dir (concat current-prefix
                        (substring dir (length saved-prefix)))))
    (when (and dir workspace)
      (let ((root (or current-prefix
                      (file-name-as-directory
                       (expand-file-name (mevedel-workspace-root workspace))))))
        (unless (file-in-directory-p dir root)
          (user-error "Working directory must be inside workspace root %s"
                      root))))
    dir))


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
  (let ((permission-mode
         (or (mevedel-session-permission-mode session)
             (and (boundp 'mevedel-permission-mode)
                  (default-toplevel-value 'mevedel-permission-mode))
             'ask)))
    (unless (memq permission-mode '(ask auto full-auto))
      (error "Invalid persisted permission mode: %S" permission-mode))
    (list
   :version                (mevedel-version)
   :session-id             (mevedel-session-session-id session)
   :session-name           (mevedel-session-name session)
   :workspace              (mevedel-session-persistence--workspace-to-plist
                            (mevedel-session-workspace session))
   :working-directory      (or (mevedel-session-working-directory session)
                               (mevedel-workspace-root
                                (mevedel-session-workspace session)))
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
   :permission-mode        permission-mode
   :plan-mode              (and (mevedel-session-plan-mode session) t)
   :permission-rules       (mevedel-session-permission-rules session)
   :resource-grants        (mevedel-session-resource-grants session)
   :preset-name            (mevedel-session-preset-name session)
   :preset-settings        (mevedel-session-preset-settings session)
   :last-observed-date     (mevedel-session-last-observed-date session)
   :agent-types-snapshot   (mevedel-session-agent-types-snapshot session)
   :skills-snapshot        (mevedel-session-skills-snapshot session)
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
  (unless (memq (plist-get plist :permission-mode) '(ask auto full-auto))
    (error "Invalid persisted permission mode: %S"
           (plist-get plist :permission-mode)))
  (unless (booleanp (plist-get plist :plan-mode))
    (error "Invalid persisted Plan mode: %S" (plist-get plist :plan-mode)))
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

(defun mevedel-session-persistence-deserialize (plist)
  "Reconstruct a session from sidecar PLIST.

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
  (unless (equal (plist-get plist :version) (mevedel-version))
    (error "Unsupported session version: %s"
           (or (plist-get plist :version) "missing")))
  (mevedel-session-persistence--validate-current-sidecar plist)
  (require 'mevedel-agent-persistence)
  (let* ((workspace (mevedel-session-persistence--workspace-from-plist
                     (plist-get plist :workspace)))
         (working-directory
          (mevedel-session-persistence--working-directory-from-plist
           plist workspace))
         (rules     (mevedel-session-persistence--filter-permission-rules
                     (plist-get plist :permission-rules)))
         (resource-grants
          (mevedel-session-persistence--filter-resource-grants
           (plist-get plist :resource-grants)))
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
                     :working-directory working-directory
                     :touched-files    (make-hash-table :test #'equal)
                     :mentions-shown   (make-hash-table :test #'equal)
                     :tasks            tasks
                     :permission-rules rules
                     :resource-grants  resource-grants
                     :permission-mode  (plist-get plist :permission-mode)
                     :plan-mode        (plist-get plist :plan-mode)
                     :preset-name      (plist-get plist :preset-name)
                     :preset-settings  (copy-tree
                                        (plist-get plist :preset-settings))
                     :turn-count       (plist-get plist :total-turn-count)
                     :last-observed-date (plist-get plist :last-observed-date)
                     :agent-types-snapshot
                     (plist-get plist :agent-types-snapshot)
                     :skills-snapshot (plist-get plist :skills-snapshot)
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
          :additional-roots    (plist-get plist :additional-roots)
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
  (let* ((dir (file-name-directory (expand-file-name path)))
         (tmp (make-temp-file (expand-file-name
                               ".mevedel-session-meta-" dir)
                              nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((print-length nil)
                  (print-level nil)
                  (print-quoted t))
              (prin1 plist (current-buffer))))
          (rename-file tmp path t))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun mevedel-session-persistence-read (path)
  "Read sidecar plist from PATH.
Returns the raw plist.  Caller is responsible for passing it through
`mevedel-session-persistence-deserialize' for validation and hygiene."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

(defun mevedel-session-persistence--write-current-buffer-atomically (path)
  "Write the current buffer to PATH through a same-directory rename."
  (let* ((directory (file-name-directory (expand-file-name path)))
         (modes (and (file-exists-p path) (file-modes path)))
         (temporary
          (make-temp-file (expand-file-name ".mevedel-transcript-" directory))))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) temporary nil 'silent)
          (when modes (set-file-modes temporary modes))
          (rename-file temporary path t))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

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
    (path &optional successor-execution-ids)
  "Mark stale running execution rows in transcript PATH as lost."
  (when (file-regular-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (require 'mevedel-pipeline)
      (let ((count
             (mevedel-pipeline-reconcile-lost-executions
              (current-buffer) successor-execution-ids)))
        (when (> count 0)
          (mevedel-session-persistence--write-current-buffer-atomically path))
        count))))

(defun mevedel-session-persistence--reconcile-lost-execution-segments
    (session &optional exclude-path)
  "Repair stale execution rows in SESSION segments except EXCLUDE-PATH."
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
                      path successor-ids)
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

(defun mevedel-session-persistence--flush-diagnostic-logs (session)
  "Persist SESSION diagnostics buffered before materialization."
  (when (fboundp 'mevedel-telemetry-flush)
    (mevedel-telemetry-flush session))
  (when (fboundp 'mevedel-hooks--persist-log-entry)
    (dolist (entry (mevedel-session-hook-log session))
      (mevedel-hooks--persist-log-entry session entry)))
  (when (fboundp 'mevedel-tool-repair--persist-event)
    (dolist (event (mevedel-session-repair-log session))
      (mevedel-tool-repair--persist-event session event)))
  (when (fboundp 'mevedel-permission-log--persist)
    (dolist (entry (mevedel-session-permission-log-pending session))
      (mevedel-permission-log--persist session entry))
    (setf (mevedel-session-permission-log-pending session) nil)))

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
          (let* ((sessions-dir (mevedel-session-persistence--sessions-dir
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
             save-path (buffer-name buffer))
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
                (setq buffer-file-name segment-path)))
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
    (let ((sidecar (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path session))))
      (when (file-exists-p sidecar)
        (condition-case err
            (mevedel-session-persistence-write
             sidecar
             (mevedel-session-persistence--build-sidecar session buffer))
          (error
           (message "mevedel: sidecar rewrite failed: %S" err)
           nil)
          (:success t))))))

(defun mevedel-session-persistence-save-agent-state (session)
  "Best-effort persist SESSION's agent state through its root data buffer."
  (when-let* ((save-path (mevedel-session-save-path session))
              (segment-path
               (mevedel-session-persistence--segment-path
                save-path
                (or (mevedel-session-current-segment session) 1)))
              (buffer
               (cl-find-if
                (lambda (candidate)
                  (and
                   (mevedel-session-persistence--root-data-buffer-p candidate)
                   (with-current-buffer candidate
                     (and (boundp 'mevedel--session)
                          (eq mevedel--session session)
                          buffer-file-name
                          (equal (expand-file-name buffer-file-name)
                                 segment-path)))))
                (buffer-list))))
    (mevedel-session-persistence--write-sidecar-now session buffer)))


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
  "Return a list of `(:turn N :pos POS :preview STR)' plists for BUFFER.

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
          (let ((content-start
                 (mevedel-session-persistence--content-start buffer))
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
                    (let ((text (buffer-substring-no-properties
                                 prompt-start seg-end)))
                      (when (string-match "[^[:space:]].*$" text)
                        (cl-incf turn)
                        (push (list :turn turn
                                    :pos prompt-start
                                    :preview (truncate-string-to-width
                                              (match-string 0 text)
                                              80 nil nil "..."))
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
can map a picker selection back to the snapshot taken right after
that prompt's response completed."
  (let* ((current-seg (or (mevedel-session-current-segment session) 1))
         (index       (mevedel-session-prompt-index session))
         (offset
          (cl-loop for (seg . prompts) in index
                   when (< seg current-seg)
                   sum (length prompts)))
         (raw-all     (mevedel-session-persistence--collect-prompts buffer))
         (tail-count  (with-current-buffer buffer
                        (mevedel-session-persistence--segment-tail-prompt-count)))
         (skip-count  (min tail-count (length raw-all)))
         (raw         (nthcdr skip-count raw-all))
         (with-cum    (cl-loop for p in raw
                               for turn from 1
                               collect
                               (let ((copy (copy-sequence p)))
                                 (plist-put copy :turn turn)
                                 (plist-put copy :file-turn
                                            (+ skip-count turn))
                                 (plist-put copy :cum-turn (+ offset turn))
                                 copy)))
         (cell        (assoc current-seg index)))
    (if cell
        (setcdr cell with-cum)
      (setf (mevedel-session-prompt-index session)
            (cons (cons current-seg with-cum) index)))))

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

(defun mevedel-session-persistence--root-data-buffer-p (buffer)
  "Return non-nil when BUFFER is a root session data buffer.

Agent conversations and both ordinary and agent-transcript view projections
belong to different buffer roles even when they share the root session."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (not (bound-and-true-p mevedel--agent-invocation))
              (not (bound-and-true-p mevedel--data-buffer))))))

(defun mevedel-session-persistence--authoritative-buffer (buffer)
  "Return the authoritative session data buffer for BUFFER.
View buffers are reconstructable UI projections and must never become
session segment buffers.  Interactive chat views persist through their
`mevedel--data-buffer'; transcript inspection views are not session
transcript buffers and return nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((derived-mode-p 'mevedel-view-mode)
        (let ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer)))
          (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
               (mevedel-session-persistence--root-data-buffer-p data-buf)
               data-buf)))
       ((mevedel-session-persistence--root-data-buffer-p buffer)
        buffer)))))


;;
;;; Lazy materialization

(defun mevedel-session-persistence-ensure-files (session buffer)
  "Lazily materialize SESSION's on-disk artifacts.

If SESSION has no `save-path' yet, allocate a fresh session id,
create the session directory tree (session dir + `agents/' +
`file-history/'), acquire the `.lock' file, set BUFFER's
variable `buffer-file-name' to the first segment file, and save the buffer.

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
                (let* ((sessions-dir
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
                   new-save-path (buffer-name buffer))
                  (setf (mevedel-session-session-id session)      session-id)
                  (setf (mevedel-session-save-path session)       new-save-path)
                  (setf (mevedel-session-created-at session)      now)
                  (setf (mevedel-session-updated-at session)      now)
                  (setf (mevedel-session-current-segment session) 1)
                  (mevedel-session-persistence--flush-diagnostic-logs session)
                  (require 'mevedel-workspace)
                  (mevedel-workspace-ensure-generated-state-ignored
                   (mevedel-session-workspace session))
                  new-save-path)))
           (segment-number (or (mevedel-session-current-segment session) 1))
           (segment-path (mevedel-session-persistence--segment-path
                          save-path segment-number)))
      (make-directory save-path t)
      (make-directory (file-name-concat save-path "agents") t)
      (make-directory (file-name-concat save-path "file-history") t)
      (require 'mevedel-workspace)
      (mevedel-workspace-ensure-generated-state-ignored
       (mevedel-session-workspace session))
      (with-current-buffer buffer
        (unless (and buffer-file-name
                     (equal (expand-file-name buffer-file-name)
                            (expand-file-name segment-path)))
          (setq buffer-file-name segment-path))
        (unless (file-exists-p segment-path)
          (set-buffer-modified-p t)
          (save-buffer)))
      save-path))


;;
;;; Sidecar build helper

(defun mevedel-session-persistence--persisted-first-user-message (session)
  "Return SESSION's already persisted first user preview, or nil."
  (when-let* ((save-path (mevedel-session-save-path session))
              (sidecar (mevedel-session-persistence--sidecar-path save-path))
              (_ (file-exists-p sidecar)))
    (condition-case nil
        (plist-get (mevedel-session-persistence-read sidecar)
                   :first-user-message)
      (error nil))))

(defun mevedel-session-persistence--build-sidecar (session buffer)
  "Build the sidecar plist for SESSION using BUFFER for ancillary fields."
  (let* ((first-preview
          (or (mevedel-session-persistence--persisted-first-user-message
               session)
              (mevedel-session-persistence--first-user-message buffer)))
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
            (when-let* ((view (and (boundp 'mevedel--view-buffer)
                                   mevedel--view-buffer))
                        ((buffer-live-p view)))
              (when (eq (buffer-local-value 'mevedel--data-buffer view)
                        (current-buffer))
                (require 'mevedel-view-render)
                (with-current-buffer view
                  (mevedel-view--rebase-data-sources delta))))))))))

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

(defun mevedel-session-persistence--save-instructions (session buffer)
  "Persist current instruction state for SESSION and BUFFER.

Writes both `instructions/current.el' and, when the session has a turn
count, a turn-specific snapshot used by rewind/fork."
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
        (when (integerp turn)
          (mevedel--write-instructions-file
           (mevedel-session-persistence--instructions-turn-path save-path turn)
           workspace-root t t nil))))))

(defun mevedel-session-persistence--load-instructions
    (session buffer &optional turn)
  "Restore SESSION instruction snapshot into BUFFER's workspace.

When TURN is non-nil, restore the turn-specific snapshot; otherwise
restore `instructions/current.el'.  Missing snapshots are ignored."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (let ((path (if turn
                    (mevedel-session-persistence--instructions-turn-path
                     save-path turn)
                  (mevedel-session-persistence--instructions-current-path
                   save-path))))
      (when (file-exists-p path)
        (require 'mevedel-persistence)
        (condition-case err
            (with-current-buffer buffer
              (mevedel--load-instructions-file
               path
               (mevedel-workspace-root (mevedel-session-workspace session))
               nil t
               (mevedel-session-workspace session)))
          (error
           (display-warning
            'mevedel
            (format "Could not restore instruction snapshot %s: %s"
                    path (error-message-string err))
            :warning)
           nil))))))


;;
;;; Per-turn save

(defun mevedel-session-persistence-save (session buffer)
  "Save SESSION's on-disk state from BUFFER's contents.

Materializes lazily on first call.  Subsequent calls update the
`updated-at' timestamp, save the data buffer, snapshot any tool-modified
files for this turn, evict old snapshots over the cap, and rewrite the
sidecar."
  (when-let ((buffer (mevedel-session-persistence--authoritative-buffer
                      buffer)))
    (mevedel-session-persistence-ensure-files session buffer)
    (setf (mevedel-session-updated-at session)
          (format-time-string "%FT%H-%M-%S"))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (save-buffer)))
    ;; Refresh the live segment's prompt list (drives the rewind picker).
    (mevedel-session-persistence--update-prompt-index session buffer)
    ;; Snapshot files modified during the just-completed turn.
    (when (and (boundp 'mevedel--current-request)
               mevedel--current-request)
      (let ((pre-snapshots
             (mevedel-request-file-snapshots mevedel--current-request)))
        (mevedel-file-history-snapshot-modified
         session
         (or (mevedel-session-turn-count session) 0)
         pre-snapshots)
        (mevedel-file-history-evict session)))
    (mevedel-session-persistence-write
     (mevedel-session-persistence--sidecar-path
      (mevedel-session-save-path session))
     (mevedel-session-persistence--build-sidecar session buffer))
    (mevedel-session-persistence--save-instructions session buffer)
    (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buffer))
                ((buffer-live-p vb)))
      (require 'mevedel-view-history)
      (mevedel-view-history-save vb))
    (mevedel-session-save-path session)))


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
        (when (file-exists-p tmp) (delete-file tmp))))))

(defun mevedel-file-history--maybe-snapshot (session path pre-content)
  "Decide whether SESSION PATH state at turn end warrants a new backup.

PATH is an absolute filesystem path that was touched by the just-completed
turn's tools.  PRE-CONTENT is PATH's content at the start of the turn,
or nil if it did not exist.

Returns a snapshot entry `(PATH . PLIST)' when a state change is recorded
for this turn, otherwise nil.  Side-effect: writes a backup file when
the state is content-bearing.

Skip conditions:
- PATH exists but is not a regular file (devices, sockets, FIFOs).
  Symlinks to regular files pass because `file-regular-p' follows.
- PATH exists and exceeds `mevedel-file-history-max-snapshot-bytes'."
  (let ((current-exists (file-exists-p path)))
    (cond
     ;; Deleted during turn: record an absent marker (no backup file).
     ((and pre-content (not current-exists))
      (cons path
            (list :backup-name nil
                  :version     (1+ (mevedel-file-history--latest-version
                                    session path))
                  :backup-time (format-time-string "%FT%H-%M-%S")
                  :file-mtime  nil)))
     ;; Currently present: create or modify.
     (current-exists
      (cond
       ((not (file-regular-p path)) nil)
       ((let ((attrs (file-attributes path)))
          (and attrs
               (> (file-attribute-size attrs)
                  mevedel-file-history-max-snapshot-bytes)))
        (display-warning
         'mevedel
         (format "Skipping snapshot of %s: exceeds %d bytes"
                 path mevedel-file-history-max-snapshot-bytes)
         :warning)
        nil)
       (t
        (let ((current-content
               (mevedel-file-history--read-file-raw path)))
          (when (or (null pre-content)
                    (not (string-equal pre-content current-content)))
            (let* ((version (1+ (mevedel-file-history--latest-version
                                 session path)))
                   (backup-name
                    (mevedel-file-history--backup-name path version)))
              (mevedel-file-history--write-backup
               (mevedel-session-save-path session)
               backup-name
               current-content)
              (cons path
                    (list :backup-name backup-name
                          :version     version
                          :backup-time (format-time-string "%FT%H-%M-%S")
                          :file-mtime
                          (format-time-string
                           "%FT%H-%M-%S"
                           (file-attribute-modification-time
                            (file-attributes path)))))))))))
     ;; Pre-content was nil and current doesn't exist: nothing to record.
     (t nil))))

(defun mevedel-file-history-snapshot-modified (session turn-n pre-snapshots)
  "Snapshot files modified during TURN-N for SESSION.

PRE-SNAPSHOTS is a hash-table mapping absolute path to the file's content
at turn start (nil for paths that did not yet exist).  Typically the
`file-snapshots' slot of the just-completed request.

For each path where the current on-disk content differs from the pre-turn
content (or the file's existence changed), write a new backup version
and append a snapshot entry under TURN-N in the session's file-snapshots
map.  Returns the list of backup names written."
  (when (and (mevedel-session-save-path session)
             (hash-table-p pre-snapshots))
    (let (entries written)
      (maphash
       (lambda (path pre-content)
         (when-let ((entry (mevedel-file-history--maybe-snapshot
                            session path pre-content)))
           (push entry entries)
           (when-let ((name (plist-get (cdr entry) :backup-name)))
             (push name written))))
       pre-snapshots)
      ;; Sort entries by path so two saves with identical state
      ;; produce byte-identical sidecars (hash-table iteration is
      ;; otherwise non-deterministic).
      (setq entries (sort entries
                          (lambda (a b) (string< (car a) (car b)))))
      (when entries
        (let ((cell (assoc turn-n (mevedel-session-file-snapshots session))))
          (if cell
              (setcdr cell entries)
            (setf (mevedel-session-file-snapshots session)
                  (cons (cons turn-n entries)
                        (mevedel-session-file-snapshots session))))))
      (sort written #'string<))))

(defun mevedel-file-history--gc-orphans (session)
  "Delete backup files under SESSION's file-history dir that are unreferenced.

Walks `:file-snapshots', collects the referenced backup-name set, then
deletes any regular file in `<save-path>/file-history/' not in that set.
Directories and non-regular entries are skipped."
  (let* ((dir        (file-name-concat (mevedel-session-save-path session)
                                       "file-history"))
         (referenced (make-hash-table :test #'equal)))
    (dolist (turn-entry (mevedel-session-file-snapshots session))
      (dolist (file-entry (cdr turn-entry))
        (when-let ((bn (plist-get (cdr file-entry) :backup-name)))
          (puthash bn t referenced))))
    (when (file-directory-p dir)
      (dolist (name (directory-files dir nil "\\`[^.]"))
        (unless (gethash name referenced)
          (let ((full (file-name-concat dir name)))
            (when (file-regular-p full)
              (delete-file full))))))))

(defun mevedel-file-history-evict (session)
  "Evict old snapshots from SESSION's file-history beyond the cap.

When `mevedel-file-history-max-snapshots' is non-nil and the number of
per-turn snapshot entries exceeds it, drops the oldest turn entries and
then garbage-collects orphan backup files.  No-op when the cap is nil
or when the session is under-cap."
  (when (and mevedel-file-history-max-snapshots
             (mevedel-session-save-path session))
    (let* ((snapshots (mevedel-session-file-snapshots session))
           (count     (length snapshots)))
      (when (> count mevedel-file-history-max-snapshots)
        (let* ((sorted (sort (copy-sequence snapshots)
                             (lambda (a b) (< (car a) (car b)))))
               (keep-count mevedel-file-history-max-snapshots)
               (kept    (nthcdr (- count keep-count) sorted)))
          (setf (mevedel-session-file-snapshots session) kept)
          (mevedel-file-history--gc-orphans session))))))


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

(defun mevedel-session-persistence--set-visited-segment-file (file)
  "Make the current buffer visit segment FILE without changing its name."
  (let ((name (buffer-name)))
    (set-visited-file-name file t)
    (rename-buffer name t))
  (setq buffer-file-truename (file-truename file))
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
    (with-current-buffer buffer
      (require 'org)
      ;; 1. Save the current segment before replacing the buffer body.
      (let ((old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            (tail-prompt-count
             (mevedel-session-persistence--prompt-count-in-text tail-text))
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
              (when (buffer-modified-p) (save-buffer))
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
              ;; 5. Atomically publish, then repoint the canonical live buffer.
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'new-publish-start
                 :new-segment (mevedel-session-current-segment session)))
              (mevedel-session-persistence--publish-segment-text
               new-segment new-text)
              (when (fboundp 'mevedel-telemetry-record)
                (mevedel-telemetry-record
                 session 'segment-rotation-stage :stage 'new-published
                 :new-segment (mevedel-session-current-segment session)))
              ;; 6. Rewrite the sidecar with the bumped current-segment.
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (mevedel-session-persistence-write
               (mevedel-session-persistence--sidecar-path
                (mevedel-session-save-path session))
               (mevedel-session-persistence--build-sidecar session buffer))
              (mevedel-session-persistence--save-instructions session buffer)
              (mevedel-session-persistence--finalize-segment-file old-segment)
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
           (setf (mevedel-session-current-segment session) old-current-segment)
           (setf (mevedel-session-updated-at session) old-updated-at)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert old-text))
           (mevedel-session-persistence--set-visited-segment-file old-segment)
           (goto-char (min old-point (point-max)))
           (set-buffer-modified-p old-modified-p)
           (ignore-errors
             (mevedel-session-persistence-write
              (mevedel-session-persistence--sidecar-path
               (mevedel-session-save-path session))
              (mevedel-session-persistence--build-sidecar session buffer)))
           (when (and new-segment (file-exists-p new-segment))
             (delete-file new-segment))
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
    (with-current-buffer buffer
      (require 'org)
      (let ((old-segment buffer-file-name)
            (old-current-segment (mevedel-session-current-segment session))
            (old-updated-at (mevedel-session-updated-at session))
            (old-text (buffer-substring (point-min) (point-max)))
            (old-point (point))
            (old-modified-p (buffer-modified-p))
            new-segment
            new-text
            initial-position)
        (unless old-segment
          (error "No current segment file"))
        (condition-case err
            (progn
              (mevedel-session-persistence--refresh-visited-file-modtime-or-error)
              (when (buffer-modified-p) (save-buffer))
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
              (mevedel-session-persistence--publish-segment-text
               new-segment new-text)
              (setf (mevedel-session-updated-at session)
                    (format-time-string "%FT%H-%M-%S"))
              (mevedel-session-persistence-write
               (mevedel-session-persistence--sidecar-path
                (mevedel-session-save-path session))
               (mevedel-session-persistence--build-sidecar session buffer))
              (mevedel-session-persistence--save-instructions session buffer)
              (mevedel-session-persistence--finalize-segment-file old-segment)
              (when initial-position
                (goto-char initial-position)
                (insert initial-text)
                (set-buffer-modified-p nil))
              (goto-char (point-max))
              (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buffer))
                          ((buffer-live-p vb)))
                (with-current-buffer vb
                  (mevedel-view--full-rerender)))
              new-segment)
          (error
           (setf (mevedel-session-current-segment session) old-current-segment)
           (setf (mevedel-session-updated-at session) old-updated-at)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert old-text))
           (mevedel-session-persistence--set-visited-segment-file old-segment)
           (goto-char (min old-point (point-max)))
           (set-buffer-modified-p old-modified-p)
           (ignore-errors
             (mevedel-session-persistence-write
              (mevedel-session-persistence--sidecar-path
               (mevedel-session-save-path session))
              (mevedel-session-persistence--build-sidecar session buffer)))
           (when (and new-segment (file-exists-p new-segment))
             (delete-file new-segment))
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

(defun mevedel-session-persistence-lock-acquire (session-dir buffer-name)
  "Acquire SESSION-DIR's `.lock' for BUFFER-NAME.

Returns:
  t   - lock acquired (or broken from a previous holder).
  nil - user chose read-only access; caller should set variable
        `buffer-read-only'.

Signals `user-error' when the user declines to break a stale lock or
aborts any of the 3-way conflict prompts.

Behavior table:
- No existing lock: write a new lock, return t.
- Lock from same host, dead PID: prompt to break (`y-or-n-p').
- Lock from same host, live PID: 3-way prompt -- break / read-only / abort.
- Lock from different host: 3-way prompt -- break / read-only / abort."
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
        (mevedel-session-persistence-lock-acquire session-dir buffer-name))
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
          (?a (user-error "Session resume aborted"))))))))

(defun mevedel-session-persistence-lock-release (session-dir)
  "Delete SESSION-DIR's `.lock' if it was written by this Emacs."
  (let* ((lock-path (mevedel-session-persistence--lock-path session-dir))
         (existing  (mevedel-session-persistence--read-lock lock-path)))
    (when (and existing
               (eq (plist-get existing :pid) (emacs-pid))
               (equal (plist-get existing :hostname) (system-name)))
      (delete-file lock-path))))

(defun mevedel-session-persistence--sweep-stale-locks (workspace)
  "Silently remove stale `.lock' files in WORKSPACE.

A lock is stale only when its hostname matches this host and the holder
is not active: either its PID is dead, or the PID is live but the live
process start time proves PID reuse.  Cross-host locks are left alone.
Best-effort; any I/O failure is swallowed.

Called opportunistically from `mevedel-resume'."
  (let ((sessions-dir (mevedel-session-persistence--sessions-dir workspace)))
    (when (file-directory-p sessions-dir)
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
  "Buffer-local `kill-buffer-hook' that releases this session's lock."
  (when (and (boundp 'mevedel--session)
             mevedel--session)
    (when-let ((dir (mevedel-session-save-path mevedel--session)))
      (condition-case _
          (mevedel-session-persistence-lock-release dir)
        (error nil)))))


;;
;;; Workspace relocation reconciliation

(defun mevedel-session-persistence--reconcile-relocation
    (session saved-workspace-plist)
  "Reconcile SESSION's path-bearing fields against workspace relocation.

If SAVED-WORKSPACE-PLIST's `:root' differs from SESSION's current
workspace root, rewrite permission rules whose `:path' starts with
the saved root and is not already under the current root, and prune
touched-files entries pointing at vanished paths.  Logs the rewrite
count to `*Messages*'.

A no-op when the saved root is missing or matches current."
  (let* ((saved-root (plist-get saved-workspace-plist :root))
         (current-root (mevedel-workspace-root
                        (mevedel-session-workspace session)))
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

(defun mevedel-session-persistence--self-heal-segment-counter (session save-path)
  "Reconcile SESSION's `:current-segment' with the filesystem under SAVE-PATH.

If the highest-numbered segment file on disk differs from the sidecar's
recorded `:current-segment', trust the filesystem (the sidecar may be
stale from a crash mid-rotation).  Logs a warning.  When healing upward
after a crash that published a new segment before finalizing its
predecessor, mark the predecessor finalized now."
  (let* ((sidecar-n    (or (mevedel-session-current-segment session) 1))
         (filesystem-n (mevedel-session-persistence--detect-highest-segment
                        save-path)))
    (when (and (> filesystem-n 0)
               (not (= sidecar-n filesystem-n)))
      (display-warning
       'mevedel
       (format "Sidecar :current-segment %d differs from filesystem (%d); using %d"
               sidecar-n filesystem-n filesystem-n)
       :warning)
      (when (> filesystem-n sidecar-n)
        (mevedel-session-persistence--finalize-segment-file
         (mevedel-session-persistence--segment-path save-path
                                                    (1- filesystem-n))))
      (setf (mevedel-session-current-segment session) filesystem-n))))


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
  (let ((so-long-predicate (lambda () nil)))
    (find-file-noselect file)))

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
        (unless (equal (plist-get plist :version) (mevedel-version))
          (error "Unsupported session version: %s"
                 (or (plist-get plist :version) "missing")))
        (mevedel-session-persistence--validate-current-sidecar plist)))))))

(defvar-local mevedel-session--read-only-mode nil
  "Non-nil when this chat buffer is in read-only session mode.
Set by the restore path when a cross-host lock cannot be safely
broken.  While set: autosave is inhibited (the terminal DONE handler
early-outs), the view buffer refuses to insert into the data buffer,
and the data buffer itself is marked `buffer-read-only'.")

(defun mevedel-session-persistence--apply-read-only-mode (buf)
  "Put BUF into read-only session mode.
See `mevedel-session--read-only-mode' for semantics."
  (with-current-buffer buf
    (setq buffer-read-only t)
    (setq-local mevedel-session--read-only-mode t)
    (display-warning
     'mevedel
     "Session opened read-only: another host holds the lock. \
Sends are disabled and nothing will be persisted."
     :warning)))

(defun mevedel-session-persistence--synthesize-session (session-dir workspace)
  "Build a minimal `mevedel-session' when the sidecar is absent.
Used when the sidecar file for SESSION-DIR is missing or unparseable.
WORKSPACE is the current workspace (resolved by the caller)."
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
     :permission-mode 'ask)))

(defun mevedel-session-persistence--find-live-buffer (session-id buf-name)
  "Return live root data buffer for SESSION-ID and BUF-NAME, or nil.
Prefers a root buffer whose `mevedel--session' has a matching session-id so
same-named sessions in one workspace do not collide. Agent and view buffers
are never roots. Falls back to a lookup by buffer name for root buffers that
never materialized a session-id."
  (or (cl-find-if
       (lambda (b)
         (and (mevedel-session-persistence--root-data-buffer-p b)
              (with-current-buffer b
                (and (bound-and-true-p mevedel--session)
                     (equal (mevedel-session-session-id mevedel--session)
                            session-id)))))
       (buffer-list))
      ;; Fallback: buffer-name match only when the buffer has no session
      ;; id yet (e.g. not yet materialized).  If a buffer exists by name
      ;; but its session has a different id, do not return it.
      (let ((candidate (get-buffer buf-name)))
        (when (and (mevedel-session-persistence--root-data-buffer-p candidate)
                   (with-current-buffer candidate
                     (or (not (bound-and-true-p mevedel--session))
                         (null (mevedel-session-session-id
                                mevedel--session)))))
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

(defun mevedel-session-persistence-restore (session-dir)
  "Restore the chat buffer for the session at SESSION-DIR.

Loads the sidecar (or synthesizes a minimal session when the sidecar
is missing/unreadable), opens the live segment file in a buffer named
per `mevedel-session-buffer-name', enables `org-mode' and
`gptel-mode' (the latter triggers gptel's restore of text-property
bounds and config), hydrates the session struct on the buffer, then
runs `mevedel--chat-buffer-init-common'.

Returns the chat buffer.  If a buffer for this session is already
alive (matched by session-id), switches to it instead of re-loading.

Tasks are deserialized from the sidecar.  Touched-files and
mentions-shown reset to empty hash tables on load."
  (let* ((sidecar-path     (mevedel-session-persistence--sidecar-path session-dir))
         (sidecar          (mevedel-session-persistence-load-sidecar sidecar-path))
         (had-sidecar-p    (not (null sidecar)))
         (result           (when sidecar
                             (mevedel-session-persistence-deserialize sidecar)))
         (session          (or (plist-get result :session)
                               (mevedel-session-persistence--synthesize-session
                                session-dir (mevedel-workspace))))
         (agent-registry-repaired-p
          (plist-get result :agent-registry-repaired-p))
         (additional-roots (plist-get result :additional-roots))
         (workspace        (mevedel-session-workspace session))
         (sidecar-current-n (and had-sidecar-p
                                 (mevedel-session-current-segment session))))
    ;; `save-path' is intentionally not serialized in the sidecar: the
    ;; session directory itself is the source of truth at restore time.
    (setf (mevedel-session-save-path session)
          (file-name-as-directory session-dir))
    ;; Self-heal: trust the filesystem if sidecar's segment counter is stale.
    (mevedel-session-persistence--self-heal-segment-counter session session-dir)
    ;; Workspace relocation: rewrite path-bearing fields if the saved
    ;; root no longer matches the current workspace's root.
    (when had-sidecar-p
      (mevedel-session-persistence--reconcile-relocation
       session (plist-get sidecar :workspace)))
    (let* ((segment-n    (mevedel-session-current-segment session))
           (segment-path (mevedel-session-persistence--segment-path
                          session-dir segment-n))
           (buf-name     (mevedel-session-buffer-name
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
           (acquired     (unless live
                           (mevedel-session-persistence-lock-acquire
                            session-dir buf-name)))
           (buf          (or live
                             (and (file-exists-p segment-path)
                                  (mevedel-session-persistence--find-file-noselect
                                   segment-path))))
           (agent-repairs 0)
           (setup-done   nil))
      (unwind-protect
          (progn
            (unless (and buf (buffer-live-p buf) (file-exists-p segment-path))
              (mevedel-session-persistence--maybe-prune-orphan
               session-dir segment-path))
            (when (and acquired (not live))
              (mevedel-session-persistence--reconcile-lost-execution-segments
               session segment-path))
            (with-current-buffer buf
              ;; Ensure canonical name and file backing.
              (unless (equal (buffer-name) buf-name)
                (rename-buffer buf-name t))
              (unless (equal (expand-file-name buffer-file-name)
                             (expand-file-name segment-path))
                (setq buffer-file-name segment-path))
              (when (and (derived-mode-p 'org-mode)
                         (fboundp 'mevedel--chat-buffer-disable-org-element-cache))
                (mevedel--chat-buffer-disable-org-element-cache))
              (unless live
                ;; Plant the hydrated session struct BEFORE enabling
                ;; `gptel-mode' so any restore-protocol hook that reads
                ;; `mevedel--session' sees the correct value.
                (setq-local mevedel--session session)
                (setq-local mevedel--workspace workspace)
                (setq-local default-directory
                            (mevedel-session-working-directory session))
                (when additional-roots
                  (setq-local mevedel-workspace-additional-roots additional-roots))
                ;; Mode + gptel restore for freshly opened files only;
                ;; live buffers are already initialized.
                (unless (derived-mode-p 'org-mode)
                  (let ((org-agenda-file-menu-enabled nil))
                    (org-mode)))
                (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
                  (mevedel--chat-buffer-disable-org-element-cache))
                (require 'mevedel-transcript-restore)
                (mevedel-transcript-restore-gptel-state)
                (when acquired
                  (require 'mevedel-pipeline)
                  (when (> (mevedel-pipeline-reconcile-lost-executions buf) 0)
                    (mevedel-session-persistence--write-current-buffer-atomically
                     buffer-file-name)
                    (set-buffer-modified-p nil)
                    (set-visited-file-modtime)))
                (unless acquired
                  (mevedel-session-persistence--apply-read-only-mode buf))
                (mevedel--chat-buffer-init-common buf workspace "resume")
                (require 'mevedel-agent-persistence)
                (setq agent-repairs
                      (mevedel-agent-persistence-restore-tree
                       session buf
                       (bound-and-true-p
                        mevedel-session--read-only-mode))))
              (unless live
                (mevedel-session-persistence--load-instructions session buf))
              ;; Persist restore-time repairs so subsequent resumes don't
              ;; repeat them.
              (when (and acquired
                         had-sidecar-p
                         (or cwd-retargeted-p
                             agent-registry-repaired-p
                             (> agent-repairs 0)
                             (and sidecar-current-n
                                  (not (= sidecar-current-n segment-n)))))
                (condition-case _
                    (mevedel-session-persistence-write
                     (mevedel-session-persistence--sidecar-path session-dir)
                     (mevedel-session-persistence--build-sidecar session buf))
                  (error nil)))
              ;; Re-render the companion view buffer from the restored
              ;; segment.  `init-common' ensures the view buffer exists but
              ;; does not populate it; without this the user sees an empty
              ;; view after resume.
              (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buf))
                          ((buffer-live-p vb)))
                (with-current-buffer vb
                  (unless live
                    (require 'mevedel-view-history)
                    (mevedel-view-history-load session))
                  (mevedel-view--full-rerender))))
            (setq setup-done t)
            buf)
        ;; If any of the setup above failed non-locally, don't leak the
        ;; lock we just acquired or the freshly-opened buffer.
        (unless setup-done
          (when (and acquired (not live))
            (condition-case _
                (mevedel-session-persistence-lock-release session-dir)
              (error nil)))
          (when (and (not live) buf (buffer-live-p buf))
            (condition-case _
                (kill-buffer buf)
              (error nil))))))))


;;
;;; File restore plan

(defun mevedel-session-persistence--latest-snapshot-entry (session path)
  "Return the highest-version snapshot plist for PATH in SESSION, or nil."
  (let ((best nil) (best-version 0))
    (dolist (turn-entry (mevedel-session-file-snapshots session) best)
      (when-let* ((entry (assoc path (cdr turn-entry)))
                  (v     (plist-get (cdr entry) :version)))
        (when (> v best-version)
          (setq best-version v
                best          (cdr entry)))))))

(defun mevedel-session-persistence--state-at-turn (session cum-turn)
  "Return an alist (PATH . PLIST) representing tracked-file state at CUM-TURN.

For each path that ever appeared in SESSION's `:file-snapshots',
picks the latest snapshot whose turn is `<=' CUM-TURN.  This is the
target state for a rewind to CUM-TURN -- files at the moment that
turn's response was saved."
  (let ((state (make-hash-table :test #'equal)))
    (dolist (turn-entry
             (sort (copy-sequence (mevedel-session-file-snapshots session))
                   (lambda (a b) (< (car a) (car b)))))
      (let ((turn (car turn-entry)))
        (when (<= turn cum-turn)
          (dolist (file-entry (cdr turn-entry))
            (puthash (car file-entry) (cdr file-entry) state)))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) state)
      result)))

(defun mevedel-session-persistence--plan-action (session path target-plist)
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
  (let* ((target-backup-name (plist-get target-plist :backup-name))
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
              (mevedel-file-history--read-file-raw
               (mevedel-file-history--backup-path
                (mevedel-session-save-path session)
                target-backup-name)))
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
                       (mevedel-file-history--read-file-raw
                        (mevedel-file-history--backup-path
                         (mevedel-session-save-path session)
                         latest-name))))
                 (diverged (not (and latest-content
                                     (string-equal current-content
                                                   latest-content)))))
            (list :action (if diverged 'overwrite 'restore)
                  :path path
                  :backup-name target-backup-name
                  :diverged diverged))))))))

(defun mevedel-session-persistence-restore-plan (session cum-turn)
  "Compute the file-restore plan for SESSION at cumulative turn CUM-TURN.

Returns a list of plan-entry plists (see
`mevedel-session-persistence--plan-action').  An empty list means
nothing to do."
  (let ((target-state
         (mevedel-session-persistence--state-at-turn session cum-turn))
        (plan nil))
    (dolist (entry target-state)
      (push (mevedel-session-persistence--plan-action
             session (car entry) (cdr entry))
            plan))
    (cl-remove-if
     (lambda (e) (eq 'noop (plist-get e :action)))
     (nreverse plan))))

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
       (let* ((backup-path (mevedel-file-history--backup-path
                            (mevedel-session-save-path session)
                            backup-name))
              (content     (mevedel-file-history--read-file-raw backup-path)))
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
                    session cum-turn)))
            (?d
             (dolist (buf buffers)
               (with-current-buffer buf
                 (set-buffer-modified-p nil)))
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

(defvar-local mevedel-session--fork-pending nil
  "Non-nil when the current buffer is a back-in-time view of a session.

Set by `mevedel-rewind' after loading a truncated segment; cleared by
`mevedel-session-persistence-fork-now' once the next user message
materializes the fork.  When non-nil, variable `buffer-file-name' is
also nil so saves cannot overwrite the original segment file.")

(defvar-local mevedel-session--rewind-context nil
  "Plist describing this buffer's rewind preview state.

Set by `mevedel-session-persistence--load-truncated' alongside
`mevedel-session--fork-pending'.  Consumed by
`mevedel-session-persistence-fork-now' to materialize the fork.  It
needs the parent session, segment, and cumulative turn for the picked
prompt so predecessor segments and referenced file-history backups can
be copied into the fork's directory.

Plist keys:
  :parent-session-id  Parent session's id.
  :parent-save-path   Parent's session directory.
  :parent-session-name Parent's session-name (preserved across the fork).
  :picked-segment     Per-segment index of the picked prompt.
  :picked-turn        Per-segment turn of the picked prompt.
  :picked-file-turn   Raw user prompt ordinal in the segment file.
  :picked-cum-turn    Cumulative turn (used as `:file-snapshots' key).")

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
          (let* ((preview (or (plist-get prompt :preview) "(empty prompt)"))
                 (turn    (plist-get prompt :turn))
                 (file-turn (plist-get prompt :file-turn))
                 (display (format "S%d T%d  %s" segment-n turn preview)))
            (push (cons display
                        (list :segment  segment-n
                              :turn     turn
                              :file-turn file-turn
                              :cum-turn (plist-get prompt :cum-turn)
                              :pos      (plist-get prompt :pos)
                              :preview  preview))
                  all)))))
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

(defun mevedel-session-persistence--load-truncated
    (session buffer segment-n file-turn-n &optional cum-turn logical-turn-n)
  "Reload BUFFER from SESSION's SEGMENT-N truncated to FILE-TURN-N's response.

Erases BUFFER, re-inserts the segment file's content, restores gptel's
text-property bounds, then truncates everything after the picked turn's
response.  Sets variable `buffer-file-name' to nil and sets the
buffer-local `mevedel-session--fork-pending' flag so the next send
creates a fork and save cannot overwrite the original segment file.

CUM-TURN, if provided, is recorded in the rewind context for use by
`mevedel-session-persistence-fork-now'.  LOGICAL-TURN-N, if provided,
is the displayed per-segment turn after copied compaction tail prompts
have been excluded."
  (let ((segment-path (mevedel-session-persistence--segment-path
                       (mevedel-session-save-path session) segment-n)))
    (unless (file-exists-p segment-path)
      (user-error "Segment %d file missing: %s"
                  segment-n segment-path))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Read with the segment-path temporarily set as buffer-file-name
        ;; so gptel-mode's restore-state path can find the file-locals.
        (let ((buffer-file-name segment-path))
          (insert-file-contents segment-path)
          (when (derived-mode-p 'org-mode)
            (when (fboundp 'mevedel--chat-buffer-disable-org-element-cache)
              (mevedel--chat-buffer-disable-org-element-cache))
            ;; Force re-restoration of GPTEL_BOUNDS from the org property.
            (when (fboundp 'gptel-org--restore-state)
              (require 'mevedel-transcript-restore)
              (mevedel-transcript-restore-sanitize-bounds)
              (gptel-org--restore-state))))
        (let ((cutoff
               (mevedel-session-persistence--find-turn-cutoff file-turn-n)))
          (when (and cutoff (< cutoff (point-max)))
            (delete-region cutoff (point-max))))
        (mevedel-transcript-restore-properties t))
      ;; Disconnect from the original file so saves can't corrupt it.
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil)
      (setq-local mevedel-session--fork-pending t)
      (setq-local mevedel-session--rewind-context
                  (list :parent-session-id   (mevedel-session-session-id session)
                        :parent-save-path    (mevedel-session-save-path session)
                        :parent-session-name (mevedel-session-name session)
                        :picked-segment      segment-n
                        :picked-turn         (or logical-turn-n file-turn-n)
                        :picked-file-turn    file-turn-n
                        :picked-cum-turn     cum-turn))
      ;; Reset derived in-memory state so it can't reflect the post-
      ;; truncation (pre-rewind) turns.  Tasks and touched-files are
      ;; cleared rather than replayed from the buffer; if the user
      ;; needs the same task list after the rewind they can re-run
      ;; TaskCreate.  mentions-shown is always reset on rewind
      ;; because expanded @mentions from future turns shouldn't dedup
      ;; away re-expansion in the fork.
      (setf (mevedel-session-tasks session) nil)
      (clrhash (mevedel-session-touched-files session))
      (clrhash (mevedel-session-mentions-shown session))
      ;; Re-render the companion view buffer so the user sees the
      ;; truncated conversation instead of stale content.
      (when-let* ((vb (buffer-local-value 'mevedel--view-buffer buffer))
                  ((buffer-live-p vb)))
        (with-current-buffer vb
          (when (fboundp 'mevedel-view-reset-agent-ephemeral-state)
            (mevedel-view-reset-agent-ephemeral-state))
          (mevedel-view--full-rerender))))))

;;;###autoload
(defun mevedel-rewind ()
  "Pick a previous user prompt in the current session and rewind to it.

Truncates the live buffer back to the picked prompt's response, sets
variable `buffer-file-name' to nil so saves cannot corrupt the original
segment file, and flags the buffer for fork-on-next-send.  The original
session's segment files are never modified -- every back-in-time pick
is reversible (re-pick the latest prompt to come back).

Refuses with `user-error' when invoked outside a mevedel chat buffer
or while a request is in flight.  Selecting the latest prompt is a
no-op."
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
    (when (buffer-local-value 'mevedel--current-request buffer)
      (user-error "Abort the current request first"))
    (require 'mevedel-execution)
    (when (mevedel-execution-session-live-p session)
      (user-error "Stop live executions with /ps or /stop before rewinding"))
    (require 'mevedel-agent-control)
    (when (mevedel-agent-control-active-turn-p session)
      (user-error "Interrupt active agent turns before rewinding"))
    ;; Refresh the live segment before presenting the picker so it stays in
    ;; sync with manual data-buffer edits since the last save.
    (mevedel-session-persistence--update-prompt-index session buffer)
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
               (entry   (cons chosen (gethash chosen lookup))))
          (when entry
            (let ((picked-segment  (plist-get (cdr entry) :segment))
                  (picked-turn     (plist-get (cdr entry) :turn))
                  (picked-file-turn (plist-get (cdr entry) :file-turn))
                  (picked-cum-turn (plist-get (cdr entry) :cum-turn))
                  (current-segment
                   (mevedel-session-current-segment session))
                  (latest-turn
                   (when-let ((live-prompts
                               (cdr (assoc
                                     (mevedel-session-current-segment session)
                                     (mevedel-session-prompt-index session)))))
                     (plist-get (car (last live-prompts)) :turn))))
              (cond
               ((and (eq picked-segment current-segment)
                     (eq picked-turn latest-turn))
                (message "Already at the latest prompt; nothing to rewind."))
               (t
                (mevedel-session-persistence--load-truncated
                 session buffer picked-segment picked-file-turn
                 picked-cum-turn picked-turn)
                ;; Compute and (after confirmation) execute the file restore.
                (when picked-cum-turn
                  (let ((plan (mevedel-session-persistence-restore-plan
                               session picked-cum-turn)))
                    (cond
                     ((null plan)
                      (message "Rewound to S%d T%d.  No file changes to apply."
                               picked-segment picked-turn)
                      (mevedel-session-persistence--load-instructions
                       session buffer picked-cum-turn))
                     (t
                      (mevedel-session-persistence--render-plan-buffer
                       plan session)
                      (if (yes-or-no-p
                           (format "Apply restore plan (%s)? "
                                   (mevedel-session-persistence--summarize-plan
                                    plan)))
                          (let ((prepared-plan
                                 (mevedel-session-persistence--prepare-buffers-for-restore
                                  session picked-cum-turn plan)))
                            (cond
                             ((eq prepared-plan :abort)
                              (message "File restore aborted; conversation rewound only."))
                             ((null prepared-plan)
                              (message "Rewound to S%d T%d.  No file changes to apply."
                                       picked-segment picked-turn)
                              (mevedel-session-persistence--load-instructions
                               session buffer picked-cum-turn))
                             (t
                              (let ((res
                                     (mevedel-session-persistence-execute-restore
                                      session prepared-plan)))
                                (mevedel-session-persistence--refresh-restored-buffers
                                 prepared-plan res)
                                (message
                                 "Rewind: %d/%d files restored%s"
                                 (plist-get res :succeeded)
                                 (plist-get res :total)
                                 (if (plist-get res :failed)
                                     (format "; failed on %s"
                                             (plist-get res :failed))
                                   ""))
                                (mevedel-session-persistence--load-instructions
                                 session buffer picked-cum-turn)))))
                        (message
                         "File restore skipped; conversation rewound only.")))))))))))))))


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

(defun mevedel-session-persistence--fork-candidate
    (session staging-path new-id parent-id picked-segment picked-cum-turn now)
  "Return a staged fork copy of SESSION reduced to the picked turn.

Historical transcript metadata is retained, but addressable agent state and
mail are deliberately absent from the returned session."
  (require 'mevedel-reminders)
  ;; This constructor is the fork policy for every session slot.  Workspace
  ;; identity and its cache are deliberately shared.  Conversation and policy
  ;; values needed by the next send are copied into independent containers;
  ;; process, callback, interaction, task, Goal, and addressable-agent state
  ;; starts empty.  Adding a session slot therefore defaults it to absent from
  ;; forks until its ownership is decided here, instead of silently sharing it.
  (let ((child
         (mevedel-session--create
          :name (mevedel-session-name session)
          :workspace (mevedel-session-workspace session)
          :working-directory (mevedel-session-working-directory session)
          :tasks nil
          :task-status-notes nil
          :last-task-write-turn nil
          :touched-files (make-hash-table :test #'equal)
          :permission-rules
          (copy-tree (mevedel-session-permission-rules session) t)
          :resource-grants
          (copy-tree (mevedel-session-resource-grants session) t)
          :permission-mode (mevedel-session-permission-mode session)
          :plan-mode (mevedel-session-plan-mode session)
          :preset-name (mevedel-session-preset-name session)
          :preset-settings
          (copy-tree (mevedel-session-preset-settings session) t)
          :turn-count (mevedel-session-turn-count session)
          :reminders
          (mevedel-reminders-clone-list
           (mevedel-session-reminders session))
          :last-observed-date (mevedel-session-last-observed-date session)
          :agent-types-snapshot
          (copy-tree (mevedel-session-agent-types-snapshot session) t)
          :skills-snapshot
          (copy-tree (mevedel-session-skills-snapshot session) t)
          :pending-reminders
          (copy-tree (mevedel-session-pending-reminders session) t)
          :specialist-nudge-state
          (copy-tree (mevedel-session-specialist-nudge-state session) t)
          :deferred-set
          (copy-tree (mevedel-session-deferred-set session) t)
          :deferred-pending
          (copy-tree (mevedel-session-deferred-pending session) t)
          :deferred-injected
          (copy-tree (mevedel-session-deferred-injected session) t)
          :deferred-used
          (copy-tree (mevedel-session-deferred-used session) t)
          :deferred-expired
          (copy-tree (mevedel-session-deferred-expired session) t)
          :messages nil
          :agent-registry nil
          :agent-root-activity 'idle
          :agent-root-waiter nil
          :agent-turn-capacity
          (mevedel-session-agent-turn-capacity session)
          :queued-user-messages
          (copy-tree (mevedel-session-queued-user-messages session) t)
          :dropped-file-grants
          (copy-tree (mevedel-session-dropped-file-grants session) t)
          :active-dropped-file-grants
          (copy-tree (mevedel-session-active-dropped-file-grants session) t)
          :mentions-shown (make-hash-table :test #'equal)
          :skills (copy-tree (mevedel-session-skills session) t)
          :hook-rules
          (copy-tree (mevedel-session-hook-rules session) t)
          :hook-log nil
          :repair-log nil
          :permission-log-pending nil
          :hook-context-pending
          (copy-tree (mevedel-session-hook-context-pending session) t)
          :execution-state nil
          :save-path staging-path
          :session-id new-id
          :created-at now
          :updated-at now
          :current-segment picked-segment
          :forked-from-session-id parent-id
          :forked-from-turn picked-cum-turn
          :prompt-index
          (copy-tree
           (mevedel-session-persistence--reduce-prompt-index
            (mevedel-session-prompt-index session)
            picked-segment picked-cum-turn)
           t)
          :file-snapshots
          (copy-tree
           (mevedel-session-persistence--reduce-file-snapshots
            (mevedel-session-file-snapshots session)
            picked-cum-turn)
           t)
          :agent-transcripts
          (copy-tree (mevedel-session-agent-transcripts session) t)
          :invoked-skills
          (copy-tree (mevedel-session-invoked-skills session) t)
          :permission-queue nil
          :pending-plan-approval nil
          :plan-metadata
          (copy-tree (mevedel-session-plan-metadata session) t)
          :goal nil)))
    (when picked-cum-turn
      (mevedel-session-persistence--prune-agent-transcripts-after-fork
       child picked-cum-turn)
      (setf (mevedel-session-turn-count child) picked-cum-turn))
    child))

(defun mevedel-session-persistence--stage-fork
    (child buffer staging-buffer parent-save-path staging-path
           picked-segment picked-cum-turn)
  "Materialize CHILD under STAGING-PATH using STAGING-BUFFER."
  (require 'mevedel-agent-persistence)
  (make-directory (file-name-concat staging-path "agents") t)
  (make-directory (file-name-concat staging-path "file-history") t)
  (when-let* ((parent-plans-dir
               (and parent-save-path
                    (file-name-concat parent-save-path "plans")))
              ((file-directory-p parent-plans-dir)))
    (copy-directory parent-plans-dir
                    (file-name-concat staging-path "plans")
                    nil t t))
  (cl-loop for i from 1 below picked-segment do
           (let ((src (mevedel-session-persistence--segment-path
                       parent-save-path i))
                 (dst (mevedel-session-persistence--segment-path
                       staging-path i)))
             (when (file-exists-p src)
               (copy-file src dst))))
  (with-current-buffer staging-buffer
    (setq-local mevedel--session child)
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
      (when-let* ((backup-name (plist-get (cdr entry) :backup-name))
                  (src (mevedel-file-history--backup-path
                        parent-save-path backup-name))
                  ((file-exists-p src)))
        (copy-file src
                   (mevedel-file-history--backup-path
                    staging-path backup-name)))))
  (when (and picked-cum-turn parent-save-path)
    (dolist (entry
             (mevedel-session-persistence--agent-files-for-segments
              (mevedel-session-prompt-index child)
              (mevedel-session-agent-transcripts child)
              picked-segment picked-cum-turn))
      (let* ((rel-path (plist-get (cdr entry) :path))
             (src (and rel-path
                       (expand-file-name rel-path parent-save-path)))
             (dst (and rel-path
                       (expand-file-name rel-path staging-path))))
        (when (and rel-path
                   (mevedel-agent-persistence-transcript-path-p
                    rel-path parent-save-path)
                   (mevedel-agent-persistence-transcript-path-p
                    rel-path staging-path)
                   (file-exists-p src))
          (make-directory (file-name-directory dst) t)
          (copy-file src dst)))))
  (mevedel-session-persistence-write
   (mevedel-session-persistence--sidecar-path staging-path)
   (mevedel-session-persistence--build-sidecar child buffer))
  (mevedel-session-persistence--save-instructions child buffer)
  (unless (mevedel-session-persistence-lock-acquire
           staging-path (buffer-name buffer))
    (error "Could not acquire fork session lock"))
  (let* ((saved
          (mevedel-session-persistence-read
           (mevedel-session-persistence--sidecar-path staging-path)))
         (restored (mevedel-session-persistence-deserialize saved)))
    (unless (equal (mevedel-session-session-id
                    (plist-get restored :session))
                   (mevedel-session-session-id child))
      (error "Fork staging validation failed"))))

(defun mevedel-session-persistence-fork-now (buffer)
  "Materialize a fork from BUFFER's rewind preview state.

Creates a fresh fork session whose `:session-name' is inherited from
the parent.  Predecessor segments (1..picked-segment-1) are copied
verbatim; the picked segment becomes the fork's truncated current
segment file (saved from BUFFER's content).  File-history backups
referenced by the target state are copied into the fork's file-history
directory.  Eligible agent transcripts remain historical artifacts; the fork
starts with an empty addressable agent registry and mailbox.

BUFFER and its view atomically switch from the source session object to the
new child session; the source object and retained tree are not mutated.
Variable `buffer-file-name' is repointed at the fork's segment,
`:fork-pending' and the rewind context are cleared, and the new session's lock
is acquired.

Errors when BUFFER is not in rewind preview state.  Returns the
fork's save-path."
  (with-current-buffer buffer
    (unless mevedel-session--fork-pending
      (user-error "Buffer is not in rewind preview state"))
    (unless mevedel-session--rewind-context
      (user-error "Rewind context missing"))
    (require 'mevedel-execution)
    (when (mevedel-execution-session-live-p mevedel--session)
      (user-error "Stop live executions with /ps or /stop before forking"))
    (require 'mevedel-agent-control)
    (when (mevedel-agent-control-active-turn-p mevedel--session)
      (user-error "Interrupt active agent turns before forking"))
    (let* ((ctx mevedel-session--rewind-context)
           (parent-id (plist-get ctx :parent-session-id))
           (parent-save-path (plist-get ctx :parent-save-path))
           (picked-segment (plist-get ctx :picked-segment))
           (picked-cum-turn (plist-get ctx :picked-cum-turn))
           (session mevedel--session)
           (sessions-dir (mevedel-session-persistence--sessions-dir
                          (mevedel-session-workspace session)))
           (new-id
            (mevedel-session-persistence--allocate-session-id
             (mevedel-session-name session) sessions-dir))
           (new-save-path (file-name-as-directory
                           (file-name-concat sessions-dir new-id)))
           (new-segment-path (mevedel-session-persistence--segment-path
                              new-save-path picked-segment))
           (parent-lock-path
            (and parent-save-path
                 (mevedel-session-persistence--lock-path parent-save-path)))
           (parent-lock
            (and parent-lock-path
                 (mevedel-session-persistence--read-lock parent-lock-path)))
           (old-point (point))
           (old-modified-p (buffer-modified-p))
           (old-file-name buffer-file-name)
           (old-file-truename buffer-file-truename)
           (view-buffer (and (buffer-live-p mevedel--view-buffer)
                             mevedel--view-buffer))
           staging-path
           child
           staging-buffer
           rollback-buffer
           published
           buffer-installed
           committed)
      (unwind-protect
          (progn
            (setq staging-path
                  (file-name-as-directory
                   (make-temp-file
                    (expand-file-name ".mevedel-fork-" sessions-dir) t))
                  child
                  (mevedel-session-persistence--fork-candidate
                   session staging-path new-id parent-id picked-segment
                   picked-cum-turn (format-time-string "%FT%H-%M-%S")))
            (setq staging-buffer
                  (clone-buffer
                   (generate-new-buffer-name " *mevedel-fork-staging*")
                   nil))
            (mevedel-session-persistence--stage-fork
             child buffer staging-buffer parent-save-path staging-path
             picked-segment picked-cum-turn)
            (rename-file (directory-file-name staging-path)
                         (directory-file-name new-save-path))
            (setq published t)
            (setq rollback-buffer
                  (clone-buffer
                   (generate-new-buffer-name " *mevedel-fork-rollback*")
                   nil))
            (let ((inhibit-quit t)
                  (inhibit-read-only t))
              (setq buffer-installed t)
              (replace-buffer-contents staging-buffer)
              (setq buffer-file-name new-segment-path
                    buffer-file-truename (file-truename new-segment-path))
              (goto-char (min old-point (point-max)))
              (set-buffer-modified-p nil)
              (set-visited-file-modtime)
              (when parent-save-path
                (mevedel-session-persistence-lock-release parent-save-path))
              (setf (mevedel-session-save-path child) new-save-path)
              (setq-local mevedel--session child)
              (when (buffer-live-p view-buffer)
                (with-current-buffer view-buffer
                  (setq-local mevedel--session child)))
              (setq mevedel-session--fork-pending nil
                    mevedel-session--rewind-context nil
                    committed t)))
        (unless committed
          (when buffer-installed
            (let ((inhibit-read-only t))
              (replace-buffer-contents rollback-buffer)
              (goto-char (min old-point (point-max)))
              (setq buffer-file-name old-file-name
                    buffer-file-truename old-file-truename)
              (set-buffer-modified-p old-modified-p)))
          (when (and parent-lock
                     parent-lock-path
                     (not (file-exists-p parent-lock-path)))
            (ignore-errors
              (mevedel-session-persistence--write-lock
               parent-lock-path (plist-get parent-lock :buffer))))
          (when published
            (ignore-errors (delete-directory new-save-path t)))
          (when staging-path
            (ignore-errors (delete-directory staging-path t))))
        (dolist (internal-buffer (list rollback-buffer staging-buffer))
          (when (buffer-live-p internal-buffer)
            (with-current-buffer internal-buffer
              (set-buffer-modified-p nil)
              (setq-local kill-buffer-hook nil))
            (kill-buffer internal-buffer))))
      ;; UI-only branch state cannot invalidate a committed fork.
      (ignore-errors
        (when-let* ((view-buffer
                     (and (boundp 'mevedel--view-buffer)
                          mevedel--view-buffer))
                    ((buffer-live-p view-buffer))
                    ((fboundp 'mevedel-view-reset-agent-ephemeral-state)))
          (mevedel-view-reset-agent-ephemeral-state view-buffer)))
      new-save-path)))

(defun mevedel-session-persistence--reduce-prompt-index
    (index picked-segment picked-cum-turn)
  "Return a copy of INDEX trimmed to the fork's picked turn.
Drops segments past PICKED-SEGMENT entirely.  In the picked segment,
keeps only prompts whose `:cum-turn' is `<=' PICKED-CUM-TURN, or all
prompts when PICKED-CUM-TURN is nil."
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
                             (<= ct picked-cum-turn))))
                     prompts))))

(defun mevedel-session-persistence--reduce-file-snapshots
    (snapshots picked-cum-turn)
  "Return SNAPSHOTS trimmed to entries whose turn is `<=' PICKED-CUM-TURN.
SNAPSHOTS is an alist keyed by cumulative turn number.  When
PICKED-CUM-TURN is nil, returns SNAPSHOTS unchanged."
  (if (null picked-cum-turn)
      snapshots
    (cl-remove-if-not
     (lambda (entry) (<= (car entry) picked-cum-turn))
     snapshots)))

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
    (let ((sanitized (mevedel-session-persistence--sanitize new-name)))
      (when (string-empty-p sanitized)
        (user-error "Empty session name"))
      ;; Rename the on-disk directory if the session is materialized.
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
          (rename-file (directory-file-name old-save-path)
                       (directory-file-name new-save-path))
          (require 'mevedel-execution)
          (mevedel-execution-relocate-artifacts
           session old-save-path new-save-path)
          (setf (mevedel-session-save-path session) new-save-path)
          (setf (mevedel-session-session-id session) new-id)
          (with-current-buffer data-buf
            (when buffer-file-name
              (setq buffer-file-name
                    (file-name-concat
                     new-save-path
                     (file-name-nondirectory buffer-file-name)))))))
      ;; Update session-name in the struct.
      (setf (mevedel-session-name session) sanitized)
      ;; Rewrite sidecar (if materialized).
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-write
         (mevedel-session-persistence--sidecar-path
          (mevedel-session-save-path session))
         (mevedel-session-persistence--build-sidecar session data-buf)))
      ;; Rename the chat buffer per the convention, and also rename
      ;; the companion view buffer (derived from the chat buffer name).
      (let* ((workspace (mevedel-session-workspace session))
             (new-data-name (mevedel-session-buffer-name sanitized workspace))
             (view-buf (buffer-local-value 'mevedel--view-buffer data-buf)))
        (with-current-buffer data-buf
          (rename-buffer new-data-name t))
        (when (buffer-live-p view-buf)
          (let ((new-view-name
                 (if (string-match "\\*$" new-data-name)
                     (replace-match ":view*" t t new-data-name)
                   (concat new-data-name ":view"))))
            (with-current-buffer view-buf
              (rename-buffer new-view-name t)))))
      (message "Session renamed to %s" sanitized))))


;;
;;; Session listing & resume command

(defun mevedel-session-persistence--read-summary (sidecar-path)
  "Read picker-relevant fields from SIDECAR-PATH; nil on failure.

Cheap by design: only fields displayed in the picker (annotations,
sort key) are extracted.  The full sidecar plist is left on disk
until restore actually reads it."
  (condition-case _
      (let* ((plist (mevedel-session-persistence-read sidecar-path))
             (_version
              (unless (equal (plist-get plist :version) (mevedel-version))
                (error "Unsupported session version")))
             (_shape
              (mevedel-session-persistence--validate-current-sidecar plist)))
        (list :session-id         (plist-get plist :session-id)
              :session-name       (plist-get plist :session-name)
              :workspace          (plist-get plist :workspace)
              :created-at         (plist-get plist :created-at)
              :updated-at         (plist-get plist :updated-at)
              :current-segment    (plist-get plist :current-segment)
              :total-turn-count   (plist-get plist :total-turn-count)
              :first-user-message (plist-get plist :first-user-message)
              :latest-user-message (plist-get plist :latest-user-message)
              :forked-from-session-id
              (plist-get plist :forked-from-session-id)))
    (error nil)))

(defun mevedel-session-persistence-list-sessions (workspace)
  "Return a list of `(:save-path :summary)' plists for WORKSPACE's sessions.

Sorted by `:updated-at' descending.  Sessions whose sidecar can't be
parsed are silently dropped."
  (let* ((sessions-dir (mevedel-session-persistence--sessions-dir workspace))
         (results nil))
    (when (file-directory-p sessions-dir)
      (dolist (entry (directory-files sessions-dir t "\\`[^.]"))
        (when (file-directory-p entry)
          (let* ((sidecar (file-name-concat entry "session.meta.el"))
                 (summary (mevedel-session-persistence--read-summary sidecar)))
            (when summary
              (push (list :save-path (file-name-as-directory entry)
                          :summary   summary)
                    results))))))
    (sort results
          (lambda (a b)
            (string-greaterp
             (or (plist-get (plist-get a :summary) :updated-at) "")
             (or (plist-get (plist-get b :summary) :updated-at) ""))))))

(defun mevedel-session-persistence--format-session-candidate (entry)
  "Return a `completing-read' display string for session ENTRY.
Shows a relative-time annotation first so the newest session is
easiest to recognise at a glance."
  (let* ((s        (plist-get entry :summary))
         (updated  (plist-get s :updated-at))
         (relative (mevedel-session-persistence--format-relative-time updated))
         (name     (or (plist-get s :session-name) "?"))
         (preview  (or (plist-get s :latest-user-message)
                       (plist-get s :first-user-message)
                       ""))
         (segments (or (plist-get s :current-segment) 1))
         (turns    (or (plist-get s :total-turn-count) 0)))
    (format "%-12s  %s  [%d seg, %d turns]  %s"
            relative name segments turns preview)))

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

;;;###autoload
(defun mevedel-resume ()
  "Resume a saved mevedel session in the current workspace.

Pick a session via `completing-read'.  If the picked session's chat
buffer is already alive in Emacs, switch to it instead of re-loading
from disk."
  (interactive)
  ;; Entry point: pull in the rest of mevedel so calling this as the first
  ;; command in a fresh Emacs does not hit void-function errors on
  ;; `mevedel-workspace' and friends.
  (require 'mevedel)
  (let* ((workspace (mevedel-workspace))
         ;; Silently drop `.lock' files left behind by previous Emacs
         ;; invocations on this host before listing or cleaning up.
         (_         (mevedel-session-persistence--sweep-stale-locks workspace))
         ;; Run opportunistic cleanup once per workspace per Emacs invocation.
         (_         (mevedel-session-persistence-cleanup-expired workspace))
         (sessions  (mevedel-session-persistence-list-sessions workspace)))
    (unless sessions
      (user-error "No saved sessions in this workspace"))
    (let* ((target
            (let* ((candidates
                    (mapcar
                     (lambda (e)
                       (cons (mevedel-session-persistence--format-session-candidate e)
                             e))
                     sessions))
                   (displays (mapcar #'car candidates))
                   (collection
                    (mevedel-session-persistence--ordered-display-collection
                     displays 'mevedel-session))
                   (chosen (completing-read
                            "Resume session: "
                            collection nil t
                            nil nil
                            (car displays))))
              (cdr (assoc chosen candidates))))
           (save-path (plist-get target :save-path))
           (buf       (mevedel-session-persistence-restore save-path)))
      (display-buffer (or (buffer-local-value 'mevedel--view-buffer buf)
                          buf)
                      gptel-display-buffer-action)
      buf)))

;;;###autoload
(defun mevedel-save-session (&optional arg)
  "Save the current mevedel session to disk explicitly.

Forces a save even when nothing has changed since the last
auto-save (useful after manual edits to the chat buffer).  Triggers lazy
materialization if the session has not yet hit disk.

With a prefix ARG, prompts for a new session name and clones the entire
session directory under a fresh id.  The current buffer is repointed at
the clone; the original on-disk session is preserved untouched and can
be reopened with `mevedel-resume'.

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
    (when arg
      (require 'mevedel-agent-control)
      (when (mevedel-agent-control-active-turn-p session)
        (user-error "Interrupt active agent turns before save-as")))
    (cond
     (arg
      (mevedel-session-persistence--save-as session data-buf))
     (t
      (with-current-buffer data-buf
        (set-buffer-modified-p t))
      (mevedel-session-persistence-save session data-buf)
      (message "Session saved.")))))

(defun mevedel-session-persistence--save-as (session data-buf)
  "Clone SESSION's on-disk directory under a fresh id.
Called from `mevedel-save-session' with a prefix arg.  Prompts for a
new session name, copies the existing directory to the new id, and
repoints the DATA-BUF at the clone."
  ;; Force materialization before cloning so there's something to copy.
  (unless (mevedel-session-save-path session)
    (with-current-buffer data-buf (set-buffer-modified-p t))
    (mevedel-session-persistence-save session data-buf))
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
    ;; Save any pending changes into the parent first.
    (with-current-buffer data-buf
      (when (and buffer-file-name (buffer-modified-p))
        (save-buffer)))
    (mevedel-session-persistence--save-instructions session data-buf)
    ;; Release the parent's lock so the clone can acquire a fresh one.
    (condition-case _
        (mevedel-session-persistence-lock-release old-save-path)
      (error nil))
    ;; Copy directory tree verbatim.  Excludes the .lock file so the
    ;; clone starts unlocked and can be independently acquired.
    (copy-directory old-save-path new-save-path nil t t)
    (let ((lock-in-clone
           (mevedel-session-persistence--lock-path new-save-path)))
      (when (file-exists-p lock-in-clone)
        (delete-file lock-in-clone)))
    ;; Mutate the session struct to point at the clone.
    (setf (mevedel-session-session-id session) new-id)
    (setf (mevedel-session-save-path session) new-save-path)
    (setf (mevedel-session-name session) sanitized)
    (setf (mevedel-session-forked-from-session-id session) old-id)
    (setf (mevedel-session-forked-from-turn session)
          (mevedel-session-turn-count session))
    ;; Repoint buffer-file-name at the cloned segment, acquire new
    ;; lock, rename buffers, rewrite sidecar.
    (with-current-buffer data-buf
      (when buffer-file-name
        (setq buffer-file-name
              (file-name-concat new-save-path
                                (file-name-nondirectory buffer-file-name)))))
    (mevedel-session-persistence-lock-acquire
     new-save-path (buffer-name data-buf))
    (mevedel-session-persistence-write
     (mevedel-session-persistence--sidecar-path new-save-path)
     (mevedel-session-persistence--build-sidecar session data-buf))
    (let* ((workspace (mevedel-session-workspace session))
           (new-data-name (mevedel-session-buffer-name sanitized workspace))
           (view-buf (buffer-local-value 'mevedel--view-buffer data-buf)))
      (with-current-buffer data-buf
        (rename-buffer new-data-name t))
      (when (buffer-live-p view-buf)
        (let ((new-view-name
               (if (string-match "\\*$" new-data-name)
                   (replace-match ":view*" t t new-data-name)
                 (concat new-data-name ":view"))))
          (with-current-buffer view-buf
            (rename-buffer new-view-name t)))))
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
  "Delete sessions in WORKSPACE older than `mevedel-session-max-age-days'.

Skips sessions with an active lock.  Cross-host locks are active.
Same-host locks are stale when their PID is dead or when the live
process start time proves PID reuse.  Throttled to at most once per
`(workspace-type . workspace-id)' per Emacs invocation; when FORCE is
non-nil the throttle is bypassed.

Returns the number of sessions deleted, or nil when the cap is nil or
the throttle has already fired."
  (when mevedel-session-max-age-days
    (let* ((ws-key (cons (mevedel-workspace-type workspace)
                         (mevedel-workspace-id workspace)))
           (already-ran (gethash ws-key
                                 mevedel-session-persistence--cleanup-throttle)))
      (when (or force (not already-ran))
        (puthash ws-key t mevedel-session-persistence--cleanup-throttle)
        (let ((threshold-secs (* mevedel-session-max-age-days 24 60 60))
              (now            (float-time))
              (deleted        0))
          (dolist (entry (mevedel-session-persistence-list-sessions workspace))
            (let* ((save-path   (plist-get entry :save-path))
                   (summary     (plist-get entry :summary))
                   (updated-str (plist-get summary :updated-at))
                   (parsed-time (mevedel-session-persistence--parse-iso-time
                                 updated-str)))
              (when (and parsed-time
                         (> (- now (float-time parsed-time))
                            threshold-secs)
                         (not
                          (mevedel-session-persistence--active-lock-p
                           save-path)))
                (delete-directory save-path t)
                (cl-incf deleted))))
          (when (> deleted 0)
            (message "Cleaned up %d expired session%s"
                     deleted (if (= deleted 1) "" "s")))
          deleted)))))


;;
;;; Hook plumbing

;; Per-completed-turn autosave lives as a DONE-state terminal handler
;; installed by `mevedel-preset--build-handlers' (`mevedel-presets.el').
;; That placement is necessary for the completed-turn contract: the
;; handler fires only on success (not on abort/error), runs after the
;; turn-count bump, and runs before `mevedel-request-end' clears the
;; request struct (so `mevedel-request-file-snapshots' is still live).

(defun mevedel-session-persistence--kill-emacs-hook ()
  "Save modified mevedel sessions and release their locks on Emacs exit.

Runs unconditionally so that locks don't outlive the Emacs process
that wrote them.  Best-effort: individual errors are swallowed so one
bad buffer can't block exit."
  (when (fboundp 'mevedel-execution-teardown-all)
    (ignore-errors (mevedel-execution-teardown-all)))
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (boundp 'mevedel--session)
                   mevedel--session)
          (when (buffer-modified-p)
            (condition-case _
                (mevedel-session-persistence-save mevedel--session buf)
              (error nil)))
          (when-let ((dir (mevedel-session-save-path mevedel--session)))
            (condition-case _
                (mevedel-session-persistence-lock-release dir)
              (error nil))))))))

;; Install at file-load time so locks get released on exit even when
;; the user never called `mevedel-install' this Emacs (e.g. running
;; `mevedel-resume' is the only command invoked).  Duplicate adds are
;; no-ops by `add-hook'.
(add-hook 'kill-emacs-hook #'mevedel-session-persistence--kill-emacs-hook)


(provide 'mevedel-session-persistence)

;;; mevedel-session-persistence.el ends here
