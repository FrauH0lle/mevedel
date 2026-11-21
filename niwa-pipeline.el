;;; niwa-pipeline.el -- Tool execution pipeline -*- lexical-binding: t -*-

;;; Commentary:

;; Owns tool context, standard steps, sequencing, and ordering.  Each tool
;; invocation first runs the canonical pipeline: validate -> pre-tool-hooks ->
;; normalize-paths -> prepare-resources -> permission -> capture-coverage ->
;; snapshot -> handler -> render-transform -> post-tool-hooks.  Provider-facing
;; calls then append hook context, repair feedback,
;; oversized-result persistence, Goal budget guidance, render data, and media.
;; Interactive handlers own any confirmation needed after the pipeline's
;; permission and snapshot steps.
;; Permission orchestration lives in niwa-tool-permission.el.  Render-data
;; serialization, provider scrubbing, and transcript mutation live in
;; niwa-tool-render-data.el.
;;
;; The persist step saves oversized results to disk and replaces them
;; with a preview + logical artifact address, preventing LLM context overflow from
;; unexpectedly large tool output.

;;; Code:

(require 'niwa-engine)

(require 'cl-lib)

(require 'niwa-structs)
(require 'niwa-hooks)
(require 'niwa-utilities)
(require 'niwa-tool-registry)
(require 'niwa-tool-repair)

;; `gptel-request'
(declare-function gptel--to-string "ext:gptel-request" (s))
(defvar gptel-backend)

;; `niwa-bash-analysis'
(declare-function niwa-bash-analysis-analyze
                  "niwa-bash-analysis" (source))
(autoload 'niwa-bash-analysis-analyze "niwa-bash-analysis")

;; `niwa-execution-telemetry'
(declare-function niwa-execution-telemetry-sandbox-summary-class
                  "niwa-execution-telemetry" (summary))
(defvar niwa-execution-telemetry-summary-cell)

;; `niwa-execution-target'
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-prefix
                  "niwa-execution-target" (cl-x) t)

;; `niwa-goal'
(declare-function niwa-goal-pause-runtime-failure
                  "niwa-goal" (buffer reason))
(declare-function niwa-goal-tool-result-budget-warnings
                  "niwa-goal" (session fsm))
(autoload 'niwa-goal-pause-runtime-failure "niwa-goal")
(autoload 'niwa-goal-tool-result-budget-warnings "niwa-goal")

;; `niwa-hooks'
(declare-function niwa-hooks-context-audit-records
                  "niwa-hooks" (decision event type &optional omit-context))
(declare-function niwa-hooks-decision-reason
                  "niwa-hooks" (decision))
(declare-function niwa-hooks-run-event "niwa-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function niwa-hooks-tool-event-plist
                  "niwa-hooks" (event context &rest extra))

;; `niwa-ptc-checkpoint'
(declare-function niwa-ptc-checkpoint-update
                  "niwa-ptc-checkpoint" (session buffer id updates))
(autoload 'niwa-ptc-checkpoint-update "niwa-ptc-checkpoint")

;; `niwa-reminders'
(declare-function niwa-reminders-queue-turn-event
                  "niwa-reminders" (buffer key body &optional commit))
(autoload 'niwa-reminders-queue-turn-event "niwa-reminders")

;; `niwa-resource'
(declare-function niwa-resource-address-like-p "niwa-resource" (value))
(declare-function niwa-resource-artifact-address "niwa-resource"
                  (path session))
(declare-function niwa-resource-discard-attempts "niwa-resource"
                  (attempts))
(declare-function niwa-resource-error-message "niwa-resource"
                  (failure &optional address private-paths))
(declare-function niwa-resource-normalize-file-path "niwa-resource"
                  (value &optional directory))
(declare-function niwa-resource-prepare "niwa-resource"
                  (operation address context))
(defvar niwa-resource-attempts-cell)
(defvar niwa-resource-current-attempts)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts-publish-text
                  "niwa-session-artifacts"
                  (session path content &optional coding))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-text
  "niwa-session-artifacts")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-shallow-ensure-files
                  "niwa-session-persistence" (session buffer))
(autoload 'niwa-session-persistence-shallow-ensure-files
  "niwa-session-persistence")

;; `niwa-structs'
(declare-function niwa-request-ephemeral-p "niwa-structs" (cl-x) t)
(declare-function niwa-request-file-snapshots "niwa-structs" (cl-x) t)
(declare-function niwa-request-id "niwa-structs" (cl-x))
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(defvar niwa--session)

;; `niwa-telemetry'
(declare-function niwa-telemetry-detailed-p "niwa-telemetry" (session))
(declare-function niwa-telemetry-finish "niwa-telemetry" (span &rest props))
(declare-function niwa-telemetry-record-audit
                  "niwa-telemetry" (session event &rest props))
(declare-function niwa-telemetry-start
                  "niwa-telemetry" (session event &rest props))

;; `niwa-tool-exec-permission'
(defvar niwa-tool-exec-permission-approved-resources)

;; `niwa-tool-media'
(declare-function niwa-tool-media-attach-result
                  "niwa-tool-media"
                  (result media tool-results-dir tool-use-id &optional session))
(declare-function niwa-tool-media-normalize-items
                  "niwa-tool-media" (items))
(declare-function niwa-tool-media-result-for-hooks
                  "niwa-tool-media" (result media))

;; `niwa-tool-patch'
(declare-function niwa-tool-patch-parse
                  "niwa-tool-patch" (patch &optional root))
(declare-function niwa-tool-patch-prepare-resources
                  "niwa-tool-patch" (proposal))
(defvar niwa-tool-patch-prepared-proposal)
(autoload 'niwa-tool-patch-prepare-resources "niwa-tool-patch")

;; `niwa-tool-permission'
(declare-function niwa-tool-permission-deny
                  "niwa-tool-permission"
                  (context fail reason &optional model-reason provenance))
(declare-function niwa-tool-permission-log-decision
                  "niwa-tool-permission" (context decision &rest props))
(declare-function niwa-tool-permission-paths
                  "niwa-tool-permission" (tool args &optional context))
(declare-function niwa-tool-permission-read-paths
                  "niwa-tool-permission" (tool args))
(declare-function niwa-tool-permission-step
                  "niwa-tool-permission" (context next fail))

;; `niwa-tool-registry'
(declare-function niwa-tool-arg-access "niwa-tool-registry" (arg-spec))
(declare-function niwa-tool-args "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-async-p "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-get-paths "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-groups "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-handler "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-max-result-size "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-name "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-read-only-p "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-render-transform
                  "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-snapshot-p "niwa-tool-registry" (cl-x) t)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-format
                  "niwa-tool-render-data"
                  (render-data &optional tool-use-id))
(declare-function niwa-tool-render-data-size
                  "niwa-tool-render-data" (data))
(declare-function niwa-tool-render-data-strip-non-media
                  "niwa-tool-render-data"
                  (string &optional expected-tool-use-id))

;; `niwa-tool-repair-diagnostics'
(declare-function niwa-tool-repair-audit-record
                  "niwa-tool-repair-diagnostics" (state repairs))
(declare-function niwa-tool-repair-record-result
                  "niwa-tool-repair-diagnostics"
                  (entry result &optional outcome result-classification))

;; `niwa-tools'
(declare-function niwa-tools--current-context "niwa-tools" ())
(defvar niwa-tools--current-engine)

;; `niwa-transport'
(declare-function niwa-transport-cancel-pending
                  "niwa-transport" (&optional key))
(declare-function niwa-transport-busy-p
                  "niwa-transport" (&optional path))
(declare-function niwa-transport-nested-p "niwa-transport" ())
(declare-function niwa-transport-run-at-time
                  "niwa-transport" (seconds function &rest args))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel))
(autoload 'niwa-transport-cancel-pending "niwa-transport")
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-nested-p "niwa-transport")
(autoload 'niwa-transport-run-at-time "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-turn'
(declare-function niwa-current-origin "niwa-turn" ())
(declare-function niwa-request-note-untracked-effect
                  "niwa-turn" (request source reason))
(declare-function niwa-request-push-canceller
                  "niwa-turn" (request canceller))
(declare-function niwa-turn-end-at-boundary "niwa-turn" (fsm reason))

;; `niwa-workspace'
(declare-function niwa-workspace-ensure-generated-state-ignored
                  "niwa-workspace" (workspace))
(autoload 'niwa-workspace-ensure-generated-state-ignored
  "niwa-workspace")
(defvar niwa--workspace)

;; `subr'
(defvar read-eval)

(require 'niwa-execution-target)
(require 'niwa-execution-telemetry)
(require 'niwa-resource)
(require 'niwa-telemetry)
(require 'niwa-tool-media)
(require 'niwa-tool-permission)
(require 'niwa-tool-render-data)
(require 'niwa-turn)

(defvar niwa-pipeline--handler-active-p nil
  "Predicate an asynchronous handler may capture to fence late mutations.
It becomes false when its pipeline step is cancelled or settled.")

(defvar niwa-pipeline--handler-commit nil
  "Function running a handler's irreversible commit and result delivery.
Cancellation during that bounded section waits for its actual outcome.
The handler must deliver a committed result before returning from the section.")

(defvar niwa-pipeline--active-tool-use-id nil
  "Tool-use id dynamically visible while a handler starts its work.")

(defvar niwa-pipeline-permission-approvals nil
  "Accepted reviewer records dynamically visible to execution admission.
Display provenance only; never a source of permission authority.")

(defvar niwa-pipeline-tool-event-hook nil
  "Observers of invocation identity and display-only approval facts.
Each receives a plist with :type started, permission-approved, or settled;
:tool-use-id, :parent-tool-use-id, :source, :name, :args and :data-buffer.
Approval events include :permission-approvals; settlement includes :status
and :reason.  Observer errors cannot affect tool execution.")

(defvar niwa-pipeline--active-call-source nil
  "Call source dynamically visible while a handler starts its work.
`ptc' names a ToolCall nested call, whose result reaches the script
instead of the provider transcript.")

(defvar niwa-pipeline--auto-apply-edit-p nil
  "Non-nil while direct user authority auto-applies a native edit.")

(defvar niwa-pipeline--canonical-path-map nil
  "Pre-authorized lexical-to-canonical paths for the active handler.")

(defconst niwa-pipeline--handler-context-variables
  '(niwa-pipeline--handler-active-p
    niwa-pipeline--handler-commit
    niwa-pipeline--active-tool-use-id
    niwa-pipeline-permission-approvals
    niwa-pipeline--active-call-source
    niwa-pipeline--auto-apply-edit-p
    niwa-resource-current-attempts
    niwa-tool-patch-prepared-proposal
    niwa-pipeline--canonical-path-map
    niwa-execution-telemetry-summary-cell
    niwa-tool-exec-permission-approved-resources)
  "Dynamic context `niwa-pipeline--step-handler' binds around a handler.")

(defun niwa-pipeline-handler-resumable (function)
  "Return FUNCTION wrapped to continue in the calling handler's context.
An asynchronous handler that continues from a helper callback runs outside
the dynamic extent in which the pipeline bound the active tool, resource
attempts, path map and commit fence, and in whatever buffer is current.
The wrapper restores those bindings and the buffer that was current when
it was made, then applies FUNCTION to its arguments."
  (let ((buffer (current-buffer))
        (values (mapcar (lambda (symbol) (and (boundp symbol) (symbol-value symbol)))
                        niwa-pipeline--handler-context-variables)))
    (lambda (&rest args)
      (cl-flet ((resume ()
                  (cl-progv niwa-pipeline--handler-context-variables values
                    (apply function args))))
        (if (buffer-live-p buffer)
            (with-current-buffer buffer (resume))
          (resume))))))

(defun niwa-pipeline-canonical-path (path)
  "Return PATH's pre-authorized canonical value for the active handler."
  (or (cdr (assoc path niwa-pipeline--canonical-path-map)) path))


;;
;;; Result persistence

(defconst niwa-pipeline--default-max-result-size 50000
  "Global cap on tool result size in characters.
When a tool declares a `max-result-size', the effective limit is
the minimum of the tool value and this default.")

(defconst niwa-pipeline--preview-size 2000
  "Number of characters to include in the preview when a result is persisted.")

(defun niwa-pipeline--head-tail-preview (result)
  "Return a bounded head-and-tail preview of RESULT.
Short strings pass through unchanged.  Long strings retain equal head and
tail budgets, preferring nearby line boundaries, and report the exact number
of omitted characters."
  (let ((length (length result)))
    (if (<= length niwa-pipeline--preview-size)
        result
      (plist-get
       (niwa--head-tail-preview-parts
        result result length niwa-pipeline--preview-size)
       :text))))

(defun niwa-pipeline-tool-results-dir
    (session buffer &optional request existing-only-p)
  "Return SESSION's tool-results directory, materializing when possible.

When SESSION has no save path yet, use
`niwa-session-persistence-shallow-ensure-files' with BUFFER so
oversized tool output produced during the first turn can still be
owned by the session.  REQUEST defaults to BUFFER's active request.
Ephemeral requests never materialize or reuse a durable directory.
EXISTING-ONLY-P returns only an already materialized directory, without
initialization or generated-state exclusion writes.
Returns nil when there is no session, the request is ephemeral, or
shallow materialization fails."
  (let ((request
         (or request
             (and buffer
                  (buffer-live-p buffer)
                  (local-variable-p 'niwa--current-request buffer)
                  (buffer-local-value 'niwa--current-request buffer)))))
    (when (and session
               (not (and request
                         (niwa-request-ephemeral-p request))))
      (let ((save-path (or (niwa-session-save-path session)
                           (when (and (not existing-only-p)
                                      buffer (buffer-live-p buffer))
                             (niwa-session-persistence-shallow-ensure-files
                              session buffer)))))
        (when save-path
          (unless existing-only-p
            (niwa-workspace-ensure-generated-state-ignored
             (niwa-session-workspace session)))
          (file-name-concat save-path "tool-results"))))))

(defun niwa-pipeline--persist-result
    (result tool session &optional buffer existing-only-p)
  "Save RESULT to disk and return a preview string.

TOOL is the `niwa-tool' whose result exceeded its size limit.
SESSION owns the output file through its `tool-results/' directory.
BUFFER is the chat data buffer used to shallowly materialize SESSION
when it has not been saved yet.  If no session-owned directory is
available, falls back to `niwa-pipeline--truncate-result'.
EXISTING-ONLY-P forbids initializing storage or updating Git exclusions."
  (setq result (niwa--normalize-message-text result))
  (if-let* ((dir (niwa-pipeline-tool-results-dir
                 session buffer nil existing-only-p)))
      (let* ((name (niwa-tool-name tool))
             (file (concat
                    (make-temp-name
                     (file-name-concat dir (concat name "-")))
                    ".txt"))
             (preview (niwa-pipeline--head-tail-preview result)))
        (niwa-session-artifacts-publish-text
         session file result 'utf-8-unix)
        (if-let* ((address (niwa-resource-artifact-address file session)))
            (concat "<persisted-output>\n"
                    (format "Output too large (%d chars). Full output saved to: %s\n\n"
                            (length result) address)
                    (format "Use Read(file_path=%S, offset=1, limit=2000) and continue with the returned offset, or Grep(pattern=PATTERN, path=%S).\n\n"
                            address address)
                    "Preview (head and tail):\n"
                    preview "\n"
                    "</persisted-output>")
          (niwa-pipeline--truncate-result result tool)))
    (niwa-pipeline--truncate-result result tool)))

(defun niwa-pipeline--truncate-result (result tool &optional ephemeral-p)
  "Truncate RESULT to a preview without persisting to disk.

Used when the result exceeds the size limit but no session-owned
persistence directory is available, or when EPHEMERAL-P forbids creating
one.  TOOL is used only for the tool name in the message."
  (concat (if ephemeral-p
              (format "Output too large (%d chars; tool: %s). Full output \
discarded for ephemeral request.\n\n"
                      (length result) (niwa-tool-name tool))
            (format "Output too large (%d chars) and no session persistence \
directory available to persist full result (tool: %s).\n\n"
                    (length result) (niwa-tool-name tool)))
          "The omitted content is unavailable; rerun the tool with a narrower scope.\n\n"
          "Preview (head and tail):\n"
          (niwa-pipeline--head-tail-preview result)
          "\n"))

(defun niwa-pipeline--truncate-error-result
    (result tool &optional legacy-prefix-p)
  "Truncate oversized error RESULT without persisting it.

When LEGACY-PREFIX-P is non-nil, keep an `Error:' prefix for callers that
still derive status from display text.  Structured-status callers receive a
neutral preview.  TOOL is used only for the tool name in the message."
  (setq result (niwa--normalize-message-text result))
  (concat (format "%s too large (%d chars; tool: %s).\n\n"
                  (if legacy-prefix-p "Error: output" "Output")
                  (length result) (niwa-tool-name tool))
          "The omitted content is unavailable; rerun the tool with a narrower scope.\n\n"
          "Preview (head and tail):\n"
          (niwa-pipeline--head-tail-preview result)
          "\n"))


;;
;;; Pipeline runner

(defun niwa-pipeline--step-name (step)
  "Return a readable name for STEP (for latch warnings)."
  (cond
   ((symbolp step) (symbol-name step))
   ((functionp step)
    (let ((name (and (consp step) (eq 'lambda (car step))
                     "<lambda>")))
      (or name "<anonymous>")))
   (t (format "%S" step))))

(defun niwa-pipeline--format-failure (reason)
  "Format a `fail' REASON into the `Error: REASON' tool-result string.

REASON is typically a plain string.  Falls through to `%S' for any other
value so a misbehaving step still produces a legible error."
  (cond
   ((stringp reason) (format "Error: %s" reason))
   (t (format "Error: %S" reason))))

(defun niwa-pipeline--copy-display-value (value)
  "Copy display VALUE, isolating mutable strings and nested vectors."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (niwa-pipeline--copy-display-value (car value))
          (niwa-pipeline--copy-display-value (cdr value))))
   ((vectorp value)
    (apply #'vector (mapcar #'niwa-pipeline--copy-display-value value)))
   (t value)))

(defun niwa-pipeline--approval-render-data (context)
  "Return CONTEXT's render data with only accepted approval provenance.
Handler and transform authored approval fields are not trusted."
  (let ((data (cl-loop for (key value) on (plist-get context :render-data) by #'cddr
                       unless (eq key :permission-approvals)
                       append (list key value))))
    (when-let* ((approvals (plist-get context :permission-approvals)))
      (setq data (plist-put data :permission-approvals
                            (niwa-pipeline--copy-display-value approvals))))
    data))

(defun niwa-pipeline--tool-event (type context &optional outcome)
  "Notify display observers of TYPE for CONTEXT and terminal OUTCOME."
  (let ((event (list :type type
                     :tool-use-id (plist-get context :tool-use-id)
                     :parent-tool-use-id (plist-get context :parent-tool-use-id)
                     :source (plist-get context :call-source)
                     :name (niwa-tool-name (plist-get context :tool))
                     :args (plist-get context :args)
                     :data-buffer (plist-get context :buffer))))
    (pcase type
      ('permission-approved
       (setq event (plist-put event :permission-approvals
                              (plist-get context :permission-approvals))))
      ('settled
       (setq event (append event (list :status (plist-get outcome :status)
                                       :reason (plist-get outcome :reason))))))
    ;; Isolate both errors and mutation of the display snapshot, per observer.
    (dolist (observer niwa-pipeline-tool-event-hook)
      (ignore-errors
        (funcall observer (niwa-pipeline--copy-display-value event))))))

(defun niwa-pipeline--settlement (context &optional reason message)
  "Return the canonical settlement for CONTEXT.
REASON and MESSAGE describe an early pipeline failure."
  (let* ((status (if reason 'error (niwa-pipeline--context-status context)))
         (failure (and reason (niwa-pipeline--format-failure message))))
    (list :status status
          :reason (or reason (and (eq status 'error) 'tool-error))
          :result (or failure (plist-get context :result))
          :raw-result (or failure (plist-get context :raw-result))
          :render-data (niwa-pipeline--approval-render-data context)
          :media (plist-get context :media)
          :tool-use-id (plist-get context :tool-use-id)
          :parent-tool-use-id (plist-get context :parent-tool-use-id)
          :source (plist-get context :call-source)
          :failure-message (and reason message)
          :input-repairs (plist-get context :input-repairs)
          :hook-additional-context (plist-get context :hook-additional-context)
          :hook-audit-records (plist-get context :hook-audit-records))))

(defun niwa-pipeline--format-outcome-failure (outcome &optional scope)
  "Return OUTCOME's provider-facing early-failure text, reporting in SCOPE."
  (let ((failure (plist-get outcome :result)))
    (when-let* ((repairs (plist-get outcome :input-repairs)))
      (setq failure
            (condition-case nil
                (let ((reminder
                       (niwa-tool-repair-format-reminder repairs)))
                  (if (string-search reminder failure)
                      failure
                    (concat failure "\n\n" reminder)))
              (error
               (ignore-errors
                 (niwa-diagnostic-warning
                  (niwa-diagnostic-scope scope)
                  'niwa
                  "Could not append tool input repair reminder"
                  :warning))
               failure))))
    failure))

(defun niwa-pipeline--provider-result (outcome &optional scope)
  "Project canonical OUTCOME to a provider result, reporting in SCOPE."
  (if (plist-get outcome :failure-message)
      (condition-case nil
          (let ((text (niwa-pipeline-append-hook-side-channel
                       (niwa-pipeline--format-outcome-failure outcome scope) outcome))
                (data (plist-get outcome :render-data)))
            (if (plist-get data :permission-approvals)
                (concat text (niwa-tool-render-data-format
                              (plist-put (copy-sequence data) :status 'error)
                              (plist-get outcome :tool-use-id)))
              text))
        (error
         (ignore-errors
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope scope)
            'niwa "Pipeline audit formatting failed" :warning))
         (niwa-pipeline--format-outcome-failure outcome scope)))
    (plist-get outcome :result)))

(defun niwa-pipeline--context-default-directory (context)
  "Return the default directory captured for pipeline CONTEXT.

Tool dispatch should be rooted at the session working directory when a
session is available.  Falling back to the caller's original
`default-directory' preserves non-workspace uses and direct unit tests
that bypass `niwa-pipeline-run-tool'."
  (file-name-as-directory
   (or (plist-get context :default-directory)
       default-directory)))

(defun niwa-pipeline--with-context-default-directory (context thunk)
  "Call THUNK with `default-directory' set from CONTEXT."
  (let ((default-directory
         (niwa-pipeline--context-default-directory context)))
    (funcall thunk)))

(defun niwa-pipeline--current-tool-use-id (tool args)
  "Return the active gptel tool-use id for TOOL and ARGS, when known."
  (let* ((fsm (and (boundp 'niwa-tools--current-engine)
                   (symbol-value 'niwa-tools--current-engine)))
         (info (and fsm (ignore-errors (niwa-engine-info fsm))))
         (tool-use (and info (plist-get info :tool-use)))
         (name (niwa-tool-name tool))
         (call (and
                tool-use name
                (cl-find-if
                 (lambda (tc)
                   (and (not (plist-get tc :result))
                        (not (plist-get tc :niwa-claimed))
                        (equal name (plist-get tc :name))
                        (niwa-tool-repair--args-equivalent-p
                         (plist-get tc :args)
                         args)))
                 tool-use))))
    (when call
      (plist-put call :niwa-claimed t)
      (plist-get call :id))))

(defun niwa-pipeline-active-tool-use-id ()
  "Return the durable tool-use id of the currently starting handler."
  niwa-pipeline--active-tool-use-id)

(defun niwa-pipeline-active-permission-approvals ()
  "Return a copy of accepted reviewer provenance for the starting handler.
The records describe this invocation only and never grant authority."
  (niwa-pipeline--copy-display-value niwa-pipeline-permission-approvals))

(defun niwa-pipeline--discard-resource-attempts (context)
  "Discard prepared resource attempts held by CONTEXT."
  (when-let* ((cell (plist-get context :resource-attempts-cell))
              (attempts (and (consp cell) (car cell))))
    (niwa-resource-discard-attempts attempts)
    (setcar cell nil)))

(defvar niwa-pipeline--slice-start nil
  "Start of the current synchronous pipeline chain, dynamically scoped.")

(defvar niwa-pipeline--chain-depth 0
  "Number of pipeline runner frames on the current synchronous stack.")

(defvar niwa-pipeline--slice-seconds 0.02
  "Time budget before yielding between pipeline steps.
An individual step may exceed this budget; it is never interrupted midway.")

(defun niwa-pipeline--run (steps callback context)
  "Run the pipeline, calling CALLBACK with one canonical settlement.

STEPS is a list of step functions.  Each step takes (CONTEXT NEXT FAIL)
where CONTEXT is a plist of accumulated state, NEXT is a continuation
taking an updated context plist, and FAIL takes a reason string plus optional
updated context and typed reason.

Sync steps may call NEXT or FAIL directly, or `signal' a
`niwa-pipeline-error' subclass.  Async steps defer and call NEXT or
FAIL later (e.g., after a user prompt resolves); once a step has
scheduled an async continuation it must not signal -- the outer
`condition-case' has already unwound.

Each step's NEXT and FAIL continuations are wrapped in a per-step
**latch**: the first call settles the step; every later call is a no-op
logged via `niwa--warn-once'.  This is defense-in-depth -- primitives
may latch at the UI layer too -- but the runner latch is authoritative.

CALLBACK must be the once-fire wrapper installed by
`niwa-pipeline-run-tool'.  The runner's `condition-case' branches
fire CALLBACK directly with a canonical error settlement when a sync error
escapes the step body or its NEXT recursion -- the wrapper guarantees
the consumer sees exactly one outcome even when the recursion already
delivered a result before signaling.  Routing through the per-step
latch instead would deadlock here, since the latch correctly suppresses
a second outcome on a step that already fired NEXT.

Interactive chains yield between steps after their time or stack budget,
keeping cancellation and error handling active while a step is pending.  Batch
callers have no input loop to service and keep synchronous chaining.

CONTEXT is the initial plist."
  ;; Every synchronous step chain -- including a chain resumed by an
  ;; async step's continuation, which re-enters here -- runs with GC
  ;; batched: the pipeline dominated a profiled session's allocation,
  ;; and each collection paid there blocks the whole UI.
  (let ((niwa-pipeline--slice-start
         (or niwa-pipeline--slice-start (float-time)))
        (niwa-pipeline--chain-depth (1+ niwa-pipeline--chain-depth)))
    (niwa--with-gc-batched
     (if (null steps)
         (progn
           (when-let* ((cancel-cell (plist-get context :cancel-cell)))
             (setcar cancel-cell nil))
           (niwa-pipeline--with-context-default-directory
            context
            (lambda ()
              (funcall callback (niwa-pipeline--settlement context)))))
       (let* ((step (car steps))
              (rest (cdr steps))
              (step-name (niwa-pipeline--step-name step))
              (settled nil)
              (telemetry-settled nil)
              (telemetry-span
               (when (and (plist-get context :session)
                          (niwa-telemetry-detailed-p
                           (plist-get context :session)))
                 (niwa-telemetry-start
                  (plist-get context :session) 'tool-pipeline-step
                  :tool-name (niwa-tool-name (plist-get context :tool))
                  :tool-use-id (plist-get context :tool-use-id)
                  :parent-tool-use-id (plist-get context :parent-tool-use-id)
                  :call-source (plist-get context :call-source)
                  :step step-name)))
              (finish-telemetry
               (lambda (outcome &optional error-class)
                 (unless telemetry-settled
                   (setq telemetry-settled t)
                   (when telemetry-span
                     (niwa-telemetry-finish
                      telemetry-span :outcome outcome
                      :error-class error-class)))))
              (try-settle
               (lambda (which)
                 (if settled
                     (progn
                       (unless (eq settled 'cancel)
                         (niwa--warn-once
                          (niwa-diagnostic-scope
                           (or (plist-get context :diagnostic-scope)
                               (plist-get context :session)
                               (plist-get context :workspace)))
                          (list 'pipeline-duplicate-outcome step-name)
                          "Pipeline step %s called %s after already %s; \
ignoring duplicate outcome"
                          step-name which settled))
                       nil)
                   (setq settled which)
                   t)))
              (cancel-cell (plist-get context :cancel-cell))
              (timer nil)
              (clear-cancel
               (lambda ()
                 (when timer (cancel-timer timer) (setq timer nil))
                 (when cancel-cell (setcar cancel-cell nil))))
              (next-cont
               (lambda (updated-ctx)
                 (when (funcall try-settle 'next)
                   (funcall clear-cancel)
                   (funcall finish-telemetry 'next)
                   (niwa-pipeline--run rest callback updated-ctx))))
              (fail-cont
               (lambda (reason &optional updated-context kind)
                 (when (funcall try-settle 'fail)
                   (funcall clear-cancel)
                   (funcall finish-telemetry 'fail)
                   (niwa-pipeline--with-context-default-directory
                    (or updated-context context)
                    (lambda ()
                      (funcall callback
                               (niwa-pipeline--settlement
                                (or updated-context context)
                                (or kind 'pipeline-error) reason)))))))
              (cancel-cont
               (lambda ()
                 (when (funcall try-settle 'cancel)
                   (funcall clear-cancel)
                   (funcall finish-telemetry 'cancelled 'request-cancelled)
                   (niwa-pipeline--with-context-default-directory
                    context
                    (lambda ()
                      (funcall callback
                               (niwa-pipeline--settlement
                                context 'cancelled "Request cancelled")))))))
              (signal-failure
               (lambda (error-class reason message)
                 (funcall clear-cancel)
                 (funcall finish-telemetry 'error error-class)
                 (niwa-pipeline--with-context-default-directory
                  context
                  (lambda ()
                    (funcall callback
                             (niwa-pipeline--settlement
                              context reason message)))))))
         (when cancel-cell
           (setcar cancel-cell cancel-cont))
         (let ((execute
                (lambda ()
                  (when (and (not settled) cancel-cell
                             (not (eq (car cancel-cell) cancel-cont)))
                    ;; A predecessor can fail after handing off to this timer.
                    ;; Its error clears ownership and settles the whole call.
                    (setq settled 'cancel)
                    (funcall finish-telemetry 'cancelled 'request-cancelled))
                  (unless settled
                    (condition-case err
                        (niwa-pipeline--with-context-default-directory
                         context
                         (lambda ()
                           (funcall step context next-cont fail-cont)))
                      (niwa-validation-error
                       (funcall signal-failure 'validation 'validation
                                (or (cadr err) "Validation error")))
                      (niwa-resource-error
                       (funcall signal-failure 'validation 'invalid-resource
                                (or (cadr err) "Invalid resource address")))
                      (niwa-permission-denied
                       (funcall signal-failure 'permission-denied 'permission-denied
                                (if (cadr err)
                                    (format "Permission denied: %s" (cadr err))
                                  "Permission denied")))
                      (niwa-pipeline-error
                       (funcall signal-failure 'pipeline 'pipeline-error
                                (or (cadr err) "Pipeline error")))
                      (error
                       (funcall signal-failure (car-safe err) 'pipeline-error
                                (niwa-resource-error-message err))))))))
           ;; A step never runs inside a remote command.  A process sentinel
           ;; can start or continue a tool call while another operation is
           ;; mid-command; a nested step's own target I/O is then refused as
           ;; busy, or TRAMP hands it a reply that belongs to the outer one.
           ;; Fast interpreted steps can exhaust the evaluator before the time
           ;; budget expires.  Leave stack room for the final consumer, which
           ;; can synchronously render results and advance gptel's FSM.
           (if (and (not (niwa-transport-nested-p))
                    (or noninteractive
                        (and (<= niwa-pipeline--chain-depth 8)
                             (not (input-pending-p))
                             (< (- (float-time) niwa-pipeline--slice-start)
                                niwa-pipeline--slice-seconds))))
               (funcall execute)
             ;; Cancellation owns this pending step just as it owns a primitive
             ;; awaiting an async result.  Resume inside the same error boundary.
             ;; The chain can reach here from a process sentinel inside a remote
             ;; command, where a plain timer would be discarded with TRAMP's
             ;; suspended timer list and the tool call would never finish.
             (letrec ((resume
                       (lambda ()
                         ;; A timer may become due during GC or another
                         ;; callback.  Let waiting commands run first, but
                         ;; always settle a continuation that lost ownership.
                         (if (and (not settled)
                                  (or (not cancel-cell)
                                      (eq (car cancel-cell) cancel-cont))
                                  (or (> niwa-pipeline--chain-depth 0)
                                      (input-pending-p)
                                      (niwa-transport-nested-p)))
                             (setq timer (niwa-transport-run-at-time 0.01 resume))
                           (let ((niwa-pipeline--slice-start (float-time))
                                 (niwa-pipeline--chain-depth 1))
                             (funcall execute))))))
               (setq timer (niwa-transport-run-at-time 0.001 resume))))))))))


;;
;;; Standard steps

(defun niwa-pipeline--step-validate (context next _fail)
  "Validate tool arguments against the arg spec.

Signals `niwa-validation-error' on failure (the runner's
`condition-case' translates the signal into `fail'), calls NEXT on
success.  CONTEXT must contain `:tool' and `:args'.  FAIL is unused --
validation fails synchronously, which the runner catches through its
signal handler."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (issues (niwa-tool-repair-validate tool args)))
    (if issues
        (signal 'niwa-validation-error
                (list (niwa-tool-repair-format-issues tool issues)))
      (funcall next context))))

(defun niwa-pipeline--normalize-path-value
    (value &optional execution-target directory)
  "Return VALUE canonicalized into an absolute filesystem path.

Environment references are substituted before expansion, because
handlers used to do that themselves after authorization had already
run against the unsubstituted string.  A malformed substitution leaves
VALUE untouched; the handler then opens that same literal string, so
the authorized resource still matches the used one.  When
EXECUTION-TARGET is non-nil, resolve VALUE and its environment
references in that target against DIRECTORY."
  (if (or (not (stringp value)) (string-empty-p value))
      value
    (if execution-target
        (progn
          (niwa-execution-target-expand-path
           execution-target value directory))
      (niwa-resource-normalize-file-path value directory))))

(defun niwa-pipeline--canonicalize-path-value
    (value &optional execution-target)
  "Resolve VALUE's symlinks without leaving EXECUTION-TARGET.

`file-truename' preserves a nonexistent tail while resolving its
existing ancestors.  Remote lookups bypass TRAMP's attribute cache so
authorization observes the target's current link destination."
  (if (or (not (stringp value)) (string-empty-p value))
      value
    (let* ((remote-file-name-inhibit-cache t)
           (canonical (niwa-file-truename value)))
      (if execution-target
          (concat
           (niwa-execution-target-prefix execution-target)
           (niwa-execution-target-native-path
            execution-target canonical))
        canonical))))

(defun niwa-pipeline--step-normalize-paths (context next _fail)
  "Canonicalize filesystem path arguments in CONTEXT before authorization.

Runs after the pre-tool hooks, which may rewrite arguments, and before
the permission step, so the path the decision chain authorizes is
byte-identical to the one the handler receives.  Handlers must not
re-resolve these arguments.  Resource-looking `path-or-resource' values stay
authored addresses and are prepared by the resource step.  FAIL is unused:
normalization passes an unresolvable value through verbatim, and an invalid
target path signals for the pipeline runner to handle."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (session (plist-get context :session))
         (target (and session
                      (niwa-session-execution-target session)))
         (directory (plist-get context :default-directory))
         updated)
    (when (listp args)
      (dolist (spec (niwa-tool-args tool))
        (let* ((type (cadr spec))
               (key (intern (concat ":" (symbol-name (car spec)))))
               (value (plist-get (or updated args) key))
               (resource-p (and (eq type 'path-or-resource)
                                (niwa-resource-address-like-p value))))
          (when (and (memq type '(path path-or-resource))
                     (not resource-p))
            (let* ((expanded (niwa-pipeline--normalize-path-value
                              value target directory))
                   ;; Ordinary paths of either type name what the handler
                   ;; opens: a workspace symlink must not pass authorization
                   ;; as the workspace path and then read its target.
                   (normalized (niwa-pipeline--canonicalize-path-value
                                expanded target)))
              (unless (equal value normalized)
                (setq updated
                      (plist-put (or updated (copy-sequence args))
                                 key normalized))))))))
    (let ((normalized-context
           (if updated (plist-put context :args updated) context)))
      (when-let* ((getter (niwa-tool-get-paths tool)))
        (let ((default-directory (or directory default-directory))
              path-map)
          (if (eq (niwa-pipeline--resource-operation tool) 'apply-patch)
              ;; Parse the patch once for the whole request and canonicalize
              ;; the proposal in place.  The resource step reuses it, so a
              ;; symlink swapped during patch review cannot redirect an
              ;; already authorized destination.
              (let ((proposal
                     (niwa-tool-patch-parse
                      (plist-get (plist-get normalized-context :args) :patch)
                      directory)))
                (dolist (operation (plist-get proposal :operations))
                  (dolist (key '(:path :move-path))
                    (when-let* ((path (plist-get operation key))
                                (canonical
                                 (niwa-pipeline--canonicalize-path-value
                                  path target)))
                      (unless (equal path canonical)
                        (push (cons path canonical) path-map))
                      (plist-put operation key canonical))))
                (setq normalized-context
                      (plist-put normalized-context :patch-parse proposal)))
            (dolist (path
                     (let ((niwa-pipeline--canonical-path-map nil))
                       (delete-dups
                        (delq nil (funcall getter
                                           (plist-get normalized-context
                                                      :args))))))
              (push (cons path
                          (niwa-pipeline--canonicalize-path-value
                           path target))
                    path-map)))
          (setq normalized-context
                (plist-put normalized-context :canonical-path-map
                           (nreverse path-map)))))
      ;; A path the tool only reads is canonicalized like any other, so its
      ;; permission is decided on the path it actually names.
      (when-let* ((reads (niwa-tool-permission-read-paths
                          tool (plist-get normalized-context :args))))
        (setq normalized-context
              (plist-put
               normalized-context :canonical-path-map
               (append (plist-get normalized-context :canonical-path-map)
                       (mapcar (lambda (path)
                                 (cons path (niwa-pipeline--canonicalize-path-value
                                             path target)))
                               reads)))))
      (funcall next normalized-context))))

(defun niwa-pipeline--resource-operation (tool)
  "Return the resource operation implemented by TOOL, or nil."
  (pcase (niwa-tool-name tool)
    ("Read" 'read)
    ("Glob" 'glob)
    ("Grep" 'grep)
    ("ApplyPatch" 'apply-patch)))

(defun niwa-pipeline--step-prepare-resources (context next _fail)
  "Prepare addressed operands after normalization and before permission.

Preparation is deliberately content-free.  It parses each semantic
`path-or-resource' argument once and stores opaque attempts for the handler;
ordinary filesystem paths pass through unchanged.  Malformed addresses and
unsupported operation/scheme pairs signal validation failures before any
permission or handler work begins."
  (let* ((tool (plist-get context :tool))
         (operation (niwa-pipeline--resource-operation tool))
         (args (plist-get context :args))
         attempts
         patch-proposal)
    (if (eq operation 'apply-patch)
        (let ((niwa-resource-attempts-cell
               (plist-get context :resource-attempts-cell)))
          (setq patch-proposal
                (niwa-tool-patch-prepare-resources
                 (or (plist-get context :patch-parse)
                     (niwa-tool-patch-parse
                      (plist-get args :patch)
                      (plist-get context :default-directory))))))
      (dolist (spec (niwa-tool-args tool))
        (let ((spec-operation (if (eq (niwa-tool-arg-access spec) 'read)
                                  'read
                                operation)))
          (when (and spec-operation (eq (cadr spec) 'path-or-resource))
            (let* ((key (intern (concat ":" (symbol-name (car spec)))))
                   (address (plist-get args key)))
              (when (and (stringp address)
                         (niwa-resource-address-like-p address))
                (let ((attempt
                       (niwa-resource-prepare spec-operation address context)))
                  (push (cons address attempt) attempts))))))))
    (funcall next
             (cond
              (patch-proposal
               (plist-put context :patch-proposal patch-proposal))
              (attempts
               (plist-put context :resource-attempts attempts))
              (t context)))))

(defun niwa-pipeline--current-request ()
  "Return the current niwa request struct, if any."
  (and (boundp 'niwa--current-request)
       niwa--current-request))

(defun niwa-pipeline--current-invocation ()
  "Return the current agent invocation struct, if any."
  niwa--agent-invocation)

(defun niwa-pipeline--validate-updated-args (tool args)
  "Return validation error for TOOL ARGS, or nil."
  (when-let* ((issues (niwa-tool-repair-validate tool args)))
    (niwa-tool-repair-format-issues tool issues)))

(defun niwa-pipeline--append-hook-context-string (text context)
  "Append accumulated hook context from CONTEXT to TEXT."
  (let ((additional (plist-get context :hook-additional-context)))
    (if-let* (((and additional (stringp text)))
              (formatted (niwa-hooks-format-context additional)))
        (concat text
                "\n\n"
                formatted)
      text)))

(defun niwa-pipeline--append-hook-audit-records (text records)
  "Append hidden hook audit RECORDS to TEXT."
  (if (and records (stringp text))
      (concat text
              (mapconcat #'niwa--format-hook-audit-record records ""))
    text))

(defun niwa-pipeline-append-hook-side-channel (text context)
  "Append accumulated hook context and audit records from CONTEXT to TEXT."
  (niwa-pipeline--append-hook-audit-records
   (niwa-pipeline--append-hook-context-string text context)
   (plist-get context :hook-audit-records)))

(defun niwa-pipeline--hook-input-rewrite-audit-record
    (event original updated decision)
  "Return a tool input rewrite audit record for hook EVENT."
  (append
   (list :type 'tool-input-rewrite
         :event (niwa-hooks-event-display-name event)
         :original-input original
         :updated-input updated)
   (when-let* ((reason (niwa-hooks-decision-reason decision)))
     (list :reason reason))))

(defun niwa-pipeline--hook-result-rewrite-audit-record
    (event original updated decision)
  "Return a result rewrite audit record for hook EVENT."
  (append
   (list :type 'tool-result-rewrite
         :event (niwa-hooks-event-display-name event)
         :original-result (or original "")
         :updated-result (or updated ""))
   (when-let* ((reason (niwa-hooks-decision-reason decision)))
     (list :reason reason))))

(defun niwa-pipeline--hook-stop-p (decision)
  "Return non-nil when hook DECISION asks to stop processing."
  (and (plist-member decision :continue)
       (not (plist-get decision :continue))))

(defun niwa-pipeline--stop-turn-for-hook (context event decision)
  "End CONTEXT's turn at its tool boundary for EVENT's stopping DECISION.
A root turn also pauses its active Goal, whose continuation would otherwise
restart the stopped work."
  (when-let* ((fsm (plist-get context :engine))
              ((niwa-turn-end-at-boundary fsm 'hook-stop)))
    (let ((reason (format "%s hook stopped the turn: %s"
                          event
                          (or (plist-get decision :stop-reason)
                              "no reason given"))))
      (unless (plist-get context :invocation)
        (niwa-goal-pause-runtime-failure (plist-get context :buffer) reason))
      (niwa-diagnostic-message
       (niwa-diagnostic-scope
        (or (plist-get context :diagnostic-scope)
            (plist-get context :session) (plist-get context :workspace)))
       "%s" reason))))

(defun niwa-pipeline--step-pre-tool-hooks (context next fail)
  "Run `PreToolUse' hooks for CONTEXT, then call NEXT or FAIL.

Hooks see validated args and may rewrite them.  Rewritten args are
validated again before the pipeline continues.  Permission decisions
from hooks are carried in CONTEXT for the permission step, where they
can tighten policy or skip a prompt without overriding explicit denies."
  (let* ((tool (plist-get context :tool))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace))
         (request (plist-get context :request))
         (invocation (plist-get context :invocation)))
    (niwa-hooks-run-tool-event
     'PreToolUse
     (niwa-hooks-tool-event-plist 'PreToolUse context)
     (lambda (decision)
       (let ((stopped-p (niwa-pipeline--hook-stop-p decision))
             (denied-p
              (eq (plist-get decision :permission-decision) 'deny)))
         (cond
          ((or stopped-p denied-p)
           (let* ((reason
                   (format
                    "blocked by PreToolUse: %s"
                    (or (and stopped-p (plist-get decision :stop-reason))
                        (plist-get decision :permission-reason)
                        (if stopped-p
                            "hook stopped tool execution"
                          "hook denied tool execution"))))
                (updated
                 (niwa-hooks-record-tool-audit
                  (niwa-hooks-record-tool-context
                   context decision 'PreToolUse)
                  (append
                   (list
                    (niwa-hooks-tool-permission-audit-record
                     'PreToolUse 'deny decision reason))
                    (niwa-hooks-context-audit-records
                     decision 'PreToolUse 'tool-context)))))
             (niwa-tool-permission-log-decision
              context
              (list :outcome 'deny
                    :raw-outcome `(deny . ,reason)
                    :via 'pre-tool-hook))
             (when stopped-p
               (niwa-pipeline--stop-turn-for-hook
                context 'PreToolUse decision))
             (niwa-tool-permission-deny
              updated fail
              (if stopped-p reason (format "Permission denied: %s" reason))
              reason 'PreToolUse)))
          (t
           (let ((updated (niwa-hooks-record-tool-context
                           context decision 'PreToolUse)))
             (setq updated
                   (niwa-hooks-record-tool-audit
                    updated
                    (niwa-hooks-context-audit-records
                     decision 'PreToolUse 'tool-context)))
             (when (plist-member decision :permission-decision)
               (setq updated
                     (plist-put
                      updated :hook-permission-decision
                      (plist-get decision :permission-decision)))
               (setq updated
                     (plist-put updated :hook-permission-hook-decision
                                decision)))
             (if (plist-member decision :updated-input)
                 (let* ((args (plist-get decision :updated-input))
                        (err (niwa-pipeline--validate-updated-args
                              tool args)))
                   (if err
                       (funcall fail err updated 'validation)
                     (funcall next
                              (plist-put
                               (niwa-hooks-record-tool-audit
                                updated
                                (niwa-pipeline--hook-input-rewrite-audit-record
                                 'PreToolUse
                                 (plist-get context :args)
                                 args
                                 decision))
                               :args args))))
               (funcall next updated)))))))
     context session workspace request invocation)))

(defun niwa-pipeline--step-snapshot (context next _fail)
  "Snapshot files before modification.

Extracts the path from tool args via the tool's get-path function and
snapshots it.  Only included for tools declaring snapshots.  CONTEXT must
contain `:tool' and `:args'.  NEXT is called on success.  FAIL is
unused -- a snapshot failure is best-effort and should never fail the
  pipeline."
  (let ((tool (plist-get context :tool))
        (args (plist-get context :args)))
    (dolist (path (niwa-tool-permission-paths tool args context))
      (niwa-pipeline--snapshot-file-if-needed
       (plist-get context :request) path))
    (funcall next context)))

(defun niwa-pipeline--snapshot-file-if-needed (request filepath)
  "Capture FILEPATH's original state once in REQUEST.
Missing files are stored as nil; unreadable paths retain a diagnostic gap."
  (when (and request filepath (stringp filepath))
    (let* ((abs-path (expand-file-name filepath))
           (snapshots (niwa-request-file-snapshots request))
           (missing (make-symbol "missing")))
      (when (eq missing (gethash abs-path snapshots missing))
        (condition-case err
            (let ((original (when (file-exists-p abs-path)
                              (with-temp-buffer
                                (set-buffer-multibyte nil)
                                (insert-file-contents-literally abs-path)
                                (buffer-string)))))
              (puthash abs-path original snapshots))
          (error
           (puthash abs-path
                    (list :gap (error-message-string err))
                    snapshots)))))))

(defun niwa-pipeline--untracked-filesystem-effects-p (tool)
  "Return non-nil when TOOL can mutate files outside exact snapshots."
  (or (and (not (niwa-tool-read-only-p tool))
           (or (memq 'eval (niwa-tool-groups tool))
               (and (memq 'edit (niwa-tool-groups tool))
                    (not (niwa-tool-snapshot-p tool)))))
      (member (niwa-tool-name tool) '("Agent" "SendMessage"))))

(defun niwa-pipeline--step-capture-coverage (context next _fail)
  "Record untracked filesystem effects from CONTEXT for Rewind, then call NEXT."
  (let ((tool (plist-get context :tool))
        (request (plist-get context :request)))
    (when (and request (niwa-pipeline--untracked-filesystem-effects-p tool))
      (niwa-request-note-untracked-effect
       request
       (niwa-tool-name tool)
       (if (member (niwa-tool-name tool) '("Agent" "SendMessage"))
           "Delegated work can modify files outside parent snapshots"
         "Tool execution can modify files outside exact path snapshots")))
    (funcall next context)))

(defun niwa-pipeline--handler-return-p (value)
  "Return non-nil when VALUE is a proper handler return plist.

Every key must be a keyword, the plist must contain `:result', and an
explicit `:status' must be `success' or `error'."
  (and (proper-list-p value)
       (zerop (% (length value) 2))
       (cl-loop for tail on value by #'cddr
                always (keywordp (car tail)))
       (plist-member value :result)
       (let ((render-data (plist-get value :render-data)))
         (or (null render-data)
             (and (proper-list-p render-data)
                  (zerop (% (length render-data) 2))
                  (cl-loop for tail on render-data by #'cddr
                           always (keywordp (car tail))))))
       (or (not (plist-member value :status))
           (memq (plist-get value :status) '(success error)))))

(defun niwa-pipeline--context-status (context)
  "Return CONTEXT's canonical handler status."
  (or (plist-get context :handler-status)
      (plist-get context :status)
      'success))

(defun niwa-pipeline--normalize-tool-string (value)
  "Return VALUE as JSON-safe model text when VALUE is a string."
  (if (stringp value)
      ;; Handlers using `json-serialize' return unibyte UTF-8, which the
      ;; normalizer would otherwise treat as plain ASCII-width text.
      (niwa--normalize-message-text
       (if (multibyte-string-p value) value (decode-coding-string value 'utf-8)))
    value))

(defun niwa-pipeline--step-handler (context next fail)
  "Run the tool handler.

For async tools (async-p is non-nil), the handler receives a callback as
its first argument followed by the args plist.  It may return (:cancel FN)
to release pending work when this call is cancelled.  For sync tools, the
handler receives just the args plist and returns the result directly.

A handler must return a plist of the form
`(:result VALUE :status STATUS :render-data DATA :media ITEMS)'.  `:result'
is required; `:status' may be `success' or `error', and the side-channel keys
are optional.  The boundary normalizes a missing status before lifecycle
dispatch.  Invalid returns and handler signals become canonical error results
so `PostToolUseFailure' observes every failed handler execution.

Sets `:result', canonical `:handler-status', `:render-data', and `:media' in
CONTEXT for downstream steps.  Explicit handler `:status' is preserved
separately for rendering;
NEXT is called on success.  Run the handler in CONTEXT's captured dispatch
buffer because an asynchronous permission prompt may resume from the view
buffer."
  (let* ((tool (plist-get context :tool))
         (handler (niwa-tool-handler tool))
         (args (plist-get context :args))
         (repair-entry (plist-get context :repair-entry))
         (completed nil)
         (store (lambda (raw)
                  (let ((result
                         (niwa-pipeline--normalize-tool-string
                          (plist-get raw :result)))
                        (status
                         (or (plist-get raw :status)
                             (and (stringp (plist-get raw :result))
                                  (string-prefix-p
                                   "Error:" (plist-get raw :result))
                                  'error)
                             'success))
                        (updated (copy-sequence context)))
                    (setq updated
                          (plist-put
                           (plist-put
                            (plist-put updated :result result)
                            :raw-result result)
                           :render-data (plist-get raw :render-data)))
                    (setq updated
                          (plist-put updated :handler-status status))
                    (when (plist-member raw :status)
                      (setq updated
                            (plist-put updated :status
                                       (plist-get raw :status))))
                    (dolist (key '(:hook-additional-context :hook-audit-records :input-repairs))
                      (when (plist-get raw key)
                        (setq updated (plist-put updated key
                                                 (append (plist-get updated key)
                                                         (plist-get raw key))))))
                    (when (plist-member raw :result-limit)
                      (setq updated (plist-put updated :result-limit
                                               (plist-get raw :result-limit))))
                    (setq updated (plist-put updated :ptc-checkpoint-id
                                             (plist-get raw :ptc-checkpoint-id)))
                    (plist-put updated :media (plist-get raw :media)))))
         (finish
          (lambda (raw)
            (setq completed t)
            (funcall
             next
             (funcall
              store
              (if (niwa-pipeline--handler-return-p raw)
                  raw
                (list
                 :result
                 (format
                  "Error: Tool %s handler returned invalid value; expected a plist containing :result"
                  (niwa-tool-name tool))
                 :status 'error))))))
         (invoke
          (lambda ()
            (let* ((cell (plist-get context :cancel-cell))
                   (active (car cell))
                   committing
                   (niwa-pipeline--handler-active-p
                    (lambda () (or (null cell) (eq active (car cell))
                                   (and committing (eq committing (car cell))))))
                   (niwa-pipeline--handler-commit
                    (lambda (thunk)
                      (unless (or (null cell) (eq active (car cell)))
                        (error "Tool was cancelled before commit"))
                      (let* (cancelled
                             (defer (lambda () (setq cancelled t))))
                        (when cell (setcar cell defer))
                        (setq committing defer)
                        (unwind-protect (funcall thunk)
                          (setq committing nil)
                          (when (and cell (eq (car cell) defer))
                            (setcar cell active)
                            (when cancelled (funcall active)))))))
                  (niwa-pipeline--active-tool-use-id
                   (plist-get context :tool-use-id))
                  (niwa-pipeline-permission-approvals
                   (copy-tree (plist-get context :permission-approvals)))
                  (niwa-tools--current-engine (plist-get context :engine))
                  (niwa-pipeline--active-call-source
                   (plist-get context :call-source))
                  (niwa-pipeline--auto-apply-edit-p
                   (plist-get context :auto-apply-edit-p))
                  (niwa-resource-current-attempts
                   (plist-get context :resource-attempts))
                  (niwa-tool-patch-prepared-proposal
                   (plist-get context :patch-proposal))
                  (niwa-pipeline--canonical-path-map
                   (plist-get context :canonical-path-map))
                  (niwa-execution-telemetry-summary-cell
                   (plist-get context :sandbox-summary-cell))
                  (niwa-tool-exec-permission-approved-resources
                   (plist-get context :approved-resources)))
              (niwa-tool-repair-mark-executed repair-entry)
              (condition-case err
                  (if (niwa-tool-async-p tool)
                      (let* ((registration (funcall handler finish args))
                             (cleanup (and (listp registration)
                                           (plist-get registration :cancel))))
                        (when (functionp cleanup)
                          (if (and cell (eq active (car cell)))
                              (let ((cancel-step active))
                                (setq active
                                      (lambda ()
                                        (when (eq active (car cell))
                                          (setcar cell cancel-step)
                                          (unwind-protect (funcall cleanup)
                                            (funcall cancel-step)))))
                                (setcar cell active))
                            ;; Cancellation can arrive while the handler starts.
                            ;; A synchronous result, however, already owns cleanup.
                            (unless completed (funcall cleanup)))))
                    (funcall finish (funcall handler args)))
                (error
                 (funcall
                  finish
                  (list :result (format "Error: %s"
                                        (niwa-resource-error-message err))
                        :status 'error)))))))
         (dispatch-buffer (plist-get context :buffer)))
    (cond
     ((null dispatch-buffer) (funcall invoke))
     ((buffer-live-p dispatch-buffer)
      (with-current-buffer dispatch-buffer
        (funcall invoke)))
     (t (funcall fail "Tool dispatch buffer is no longer live")))))

(defun niwa-pipeline--step-repair-reminder (context next _fail)
  "Append one model-facing reminder for committed input repairs in CONTEXT."
  (let ((records (plist-get context :input-repairs))
        (result (plist-get context :result)))
    (if (and records (stringp result))
        (condition-case err
            (funcall
             next
             (plist-put
              context :result
              (concat result "\n\n"
                      (niwa-tool-repair-format-reminder records))))
          (error
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope
             (or (plist-get context :diagnostic-scope)
                 (plist-get context :session) (plist-get context :workspace)))
            'niwa
            (format "Could not append tool input repair reminder: %S" err)
            :warning)
           (funcall next context)))
      (funcall next context))))


(defconst niwa-pipeline--render-transform-max-data-size 8192
  "Maximum printed size of render-data produced by `:render-transform'.

Handler-provided render-data is not capped here because some existing
handlers intentionally carry larger structured payloads, such as edit
diffs.  Transform functions are for bounded metadata derived from a
string result, not for copying the result body into a hidden side
channel.")

(defun niwa-pipeline--step-render-transform (context next _fail)
  "Run CONTEXT's TOOL `:render-transform', then call NEXT.

The transform receives the normalized string result before oversized
result persistence and before render/media side-channel attachment.
It is skipped when the handler already supplied render-data, when the
result is not a string, or when the result status is `error'.

FAIL is unused; transform failures warn and leave CONTEXT unchanged."
  (let* ((tool (plist-get context :tool))
         (transform (and tool (niwa-tool-render-transform tool)))
         (name (and tool (niwa-tool-name tool)))
         (args (plist-get context :args))
         (result (plist-get context :result))
         (existing-render-data (plist-get context :render-data)))
    (if (or existing-render-data
            (not (functionp transform))
            (not (stringp result))
            (eq 'error (niwa-pipeline--context-status context)))
        (funcall next context)
      (condition-case err
          (let ((render-data (funcall transform name args result)))
            (cond
             ((null render-data)
              (funcall next context))
             ((progn
                (> (niwa-tool-render-data-size render-data)
                   niwa-pipeline--render-transform-max-data-size))
              (niwa-diagnostic-warning
               (niwa-diagnostic-scope
                (or (plist-get context :diagnostic-scope)
                    (plist-get context :session) (plist-get context :workspace)))
               'niwa
               (format "Render transform for %s returned oversized metadata"
                       (or name "tool"))
               :warning)
              (funcall next context))
             (t
              (funcall next
                       (plist-put context :render-data render-data)))))
        (error
         (niwa-diagnostic-warning
          (niwa-diagnostic-scope
           (or (plist-get context :diagnostic-scope)
               (plist-get context :session) (plist-get context :workspace)))
          'niwa
          (format "Render transform for %s failed: %s"
                  (or name "tool") (error-message-string err))
          :warning)
         (funcall next context))))))

(defun niwa-pipeline--step-attach-render-data (context next _fail)
  "Embed render-data from CONTEXT, then call NEXT.

When CONTEXT holds render-data or an explicit handler status, append a
hidden delimiter-wrapped block carrying the serialized data.  Non-text
results use gptel's normal result conversion at this provider-only step;
canonical outcomes and nested callers retain their typed values.
Explicit status is stored under `:status' for renderer
dispatch.  The block is propertized `invisible' for the data-buffer display
and recognised by the view interpreter via its delimiters.  An `:around'
advice on `gptel--parse-tool-results' strips the block on the LLM path only;
see `niwa-tool-render-data-format'.

FAIL is unused; render-data attachment never fails.

When neither was produced, passes CONTEXT through unchanged."
  (let* ((result (plist-get context :result))
         (status (plist-get context :status))
         (tool-use-id (plist-get context :tool-use-id))
         (render-data (niwa-pipeline--approval-render-data context))
         (sandbox-summary
          (car (plist-get context :sandbox-summary-cell)))
         (render-data
          (if (and sandbox-summary
                   (progn
                     (niwa-execution-telemetry-sandbox-summary-class
                      sandbox-summary)))
              (plist-put (copy-sequence render-data)
                         :sandbox-summary (copy-tree sandbox-summary))
            render-data))
         (render-data
          (if status
              (plist-put (copy-sequence render-data) :status status)
            render-data)))
    (if render-data
        (progn
          (funcall next
                   (plist-put context :result
                              (concat (gptel--to-string result)
                                      (niwa-tool-render-data-format
                                       render-data tool-use-id)))))
      (funcall next context))))

(defun niwa-pipeline--step-attach-media-data (context next _fail)
  "Embed media side-channel data from CONTEXT, then call NEXT.

MEDIA is a list of plists, usually carrying at least `:path', `:mime',
and `:kind'.  The block is hidden in the data buffer and stripped at the
gptel tool-result serialization boundary.  Backends that gain native
tool-result media support can read this contract at that boundary
without changing handler return shapes."
  (let ((result (plist-get context :result))
        (media (plist-get context :media)))
    (if (and media (stringp result))
        (funcall next
                 (plist-put context :result
                            (niwa-tool-media-attach-result
                             result media
                             (niwa-pipeline-tool-results-dir
                              (plist-get context :session)
                              (plist-get context :buffer)
                              (plist-get context :request))
                             (plist-get context :tool-use-id)
                             (plist-get context :session))))
      (funcall next context))))

(defun niwa-pipeline--step-persist (context next _fail)
  "Persist oversized tool results to disk.

If the tool has a `max-result-size' and the string result exceeds the
effective limit (the minimum of the tool value and
`niwa-pipeline--default-max-result-size'), saves the full result to
the session's `tool-results/' directory and replaces :result with a
preview.

When no session-owned persistence directory is available, the result
is still truncated to the preview size to prevent context overflow --
only the file write is skipped.

Skips entirely when the result is not a string.  Oversized error results are
truncated rather than persisted.  The handler boundary supplies canonical
status before this step runs.
CONTEXT must contain :tool and :result.  NEXT is called with the
possibly-updated context."
  (let* ((tool (plist-get context :tool))
         (result (plist-get context :result))
         (max-size (if (plist-member context :result-limit)
                       (plist-get context :result-limit)
                     (niwa-tool-max-result-size tool)))
         (effective (when max-size
                      (min max-size niwa-pipeline--default-max-result-size)))
         (deliver
          (lambda (updated disposition)
            (when-let* ((render-data (plist-get updated :render-data)))
              (setq updated
                    (plist-put
                     updated :render-data
                     (plist-put
                      (copy-sequence render-data) :output-accounting
                      (list :disposition disposition
                            :original-chars (and (stringp result)
                                                 (length result)))))))
            (funcall next updated))))
    (cond
     ((or (null effective)
          (null result)
          (not (stringp result))
          (<= (length result) effective))
      (funcall deliver context 'inline))
     ((eq 'error (niwa-pipeline--context-status context))
      (funcall deliver
               (plist-put context :result
                          (niwa-pipeline--truncate-error-result
                           result tool
                           (not (plist-member context :status))))
               'truncated))
     (t
      ;; Result exceeds limit -- persist or truncate.  Session/buffer
      ;; context was captured at `niwa-pipeline-run-tool'
      ;; entry; do not re-read it from `current-buffer' here because
      ;; the handler may have run (and called back) from inside a
      ;; `with-temp-buffer' wrapper.
      (let ((session (plist-get context :session))
            (buffer (plist-get context :buffer))
            (request (plist-get context :request)))
        (let ((projected
               (if (and request
                        (niwa-request-ephemeral-p request))
                   (niwa-pipeline--truncate-result result tool t)
                 (niwa-pipeline--persist-result
                  result tool session buffer
                  (and (equal (niwa-tool-name tool) "Bash")
                       (eq 'read-only
                           (plist-get
                            (niwa-bash-analysis-analyze
                             (plist-get (plist-get context :args) :command))
                            :class)))))))
          (funcall deliver
                   (plist-put context :result projected)
                   (if (string-prefix-p "<persisted-output>" projected)
                       'persisted
                     'truncated))))))))

(defun niwa-pipeline--step-ptc-checkpoint (context next fail)
  "Commit CONTEXT's final ToolCall result before delivery.
For provider output, the sidecar contains the projected preview and references
the staged full output.  Its marker commits both before NEXT delivers it.
FAIL receives a durability error; one retry records that honest outcome."
  (if-let* ((id (plist-get context :ptc-checkpoint-id)))
      (let ((session (plist-get context :session))
            (buffer (plist-get context :buffer))
            (render-data (plist-get context :render-data))
            failure)
        (condition-case err
            (unless (niwa-ptc-checkpoint-update
                     session buffer id
                     (list :state 'settled :result (plist-get context :result)
                           :render-data render-data))
              (error "ToolCall final audit could not be persisted"))
          (error (setq failure (error-message-string err))))
        (if failure
            (let ((result (niwa-pipeline--format-failure failure))
                  (data (plist-put (copy-sequence render-data)
                                   :outcome 'script-error)))
              (ignore-errors
                (niwa-ptc-checkpoint-update
                 session buffer id
                 (list :state 'settled :result result :render-data data)))
              (funcall fail failure (plist-put context :render-data data)))
          (funcall next context)))
    (funcall next context)))

(defun niwa-pipeline--step-post-tool-hooks (context next _fail)
  "Run post-tool hooks for CONTEXT, then call NEXT.

Hooks receive both `:raw-result' and the final `:result'.  Only an
explicit `:updated-result' changes the model-visible tool result."
  (let* ((media (niwa-tool-media-normalize-items
                 (plist-get context :media)))
         (context (plist-put context :media media))
         (tool-use-id (plist-get context :tool-use-id))
         (result (plist-get context :result))
         (model-result
          (niwa-tool-media-result-for-hooks
           (niwa-tool-render-data-strip-non-media
            result tool-use-id)
           media))
         (raw-result
          (niwa-tool-media-result-for-hooks
           (niwa-tool-render-data-strip-non-media
            (plist-get context :raw-result) tool-use-id)
           media))
         (error-p (eq 'error (niwa-pipeline--context-status context)))
         (event (if error-p
                    'PostToolUseFailure
                  'PostToolUse))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace)))
    (niwa-hooks-run-tool-event
     event
     (niwa-hooks-tool-event-plist
      event context
      :raw-result raw-result
      :result model-result
      :tool-response model-result
      :error (and error-p result))
     (lambda (decision)
       (let ((context (niwa-hooks-record-tool-context
                       context decision event)))
         (when (niwa-pipeline--hook-stop-p decision)
           (niwa-pipeline--stop-turn-for-hook context event decision))
         (setq context
               (niwa-hooks-record-tool-audit
                context
                (niwa-hooks-context-audit-records
                 decision event 'tool-context)))
         (cond
          ((plist-member decision :updated-result)
           (setq context
                 (niwa-hooks-record-tool-audit
                  context
                  (niwa-pipeline--hook-result-rewrite-audit-record
                   event model-result
                   (plist-get decision :updated-result)
                   decision)))
           (funcall next
                    (plist-put
                     (plist-put
                      context :result (plist-get decision :updated-result))
                     :media nil)))
          (t
           (funcall next context)))))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))

(defun niwa-pipeline--step-hook-side-channel (context next _fail)
  "Append hook context and audit prose for a provider-facing CONTEXT."
  (funcall next
           (plist-put
            context :result
            (niwa-pipeline-append-hook-side-channel
             (plist-get context :result) context))))

(defun niwa-pipeline--step-goal-budget-warning (context next _fail)
  "Queue newly crossed Goal budget warnings for CONTEXT's turn, then call NEXT.

A warning reaches the model at the same WAIT as this tool result via the
turn-event channel, so the in-flight turn can adjust without an extra request,
without the warning becoming permanent transcript history."
  (let ((result (plist-get context :result))
        (session (plist-get context :session))
        (fsm (plist-get context :engine)))
    (when (and (stringp result) session fsm)
      (dolist (warning (niwa-goal-tool-result-budget-warnings session fsm))
        (niwa-reminders-queue-turn-event
         (plist-get context :buffer) (plist-get warning :key)
         (plist-get warning :body) (plist-get warning :commit))))
    (funcall next context)))


;;
;;; Step list builder

(defun niwa-pipeline--build-steps (tool &optional outcome-only-p)
  "Build the standard step list for TOOL.

Returns a list of step functions based on TOOL's behavioral flags:
  1. validate            -- always included
  2. pre-tool-hooks      -- always included
  3. normalize-paths     -- canonicalizes `path'-typed args for authorization
  4. prepare-resources   -- parses addressed operands before permission
  5. permission          -- always included
  6. capture-coverage    -- records effects Rewind cannot revert
  7. snapshot            -- included when snapshot-p
  8. handler             -- always included
  9. render-transform    -- always included; no-op when tool has none
  10. post-tool-hooks    -- always included

Provider projection then appends hook context and repair feedback,
persists oversized output when declared, adds a Goal warning, and
attaches render-data and media.  Outcome-only consumers stop at the canonical
common boundary.  ToolCall checkpoints settle after the selected projection."
  (let ((common
         (append
          (list #'niwa-pipeline--step-validate
                #'niwa-pipeline--step-pre-tool-hooks
                #'niwa-pipeline--step-normalize-paths
                #'niwa-pipeline--step-prepare-resources
                #'niwa-tool-permission-step
                #'niwa-pipeline--step-capture-coverage)
          (when (niwa-tool-snapshot-p tool)
            (list #'niwa-pipeline--step-snapshot))
          (list #'niwa-pipeline--step-handler
                #'niwa-pipeline--step-render-transform
                #'niwa-pipeline--step-post-tool-hooks))))
    (if outcome-only-p
        (append common
                (when (equal (niwa-tool-name tool) "ToolCall")
                  (list #'niwa-pipeline--step-ptc-checkpoint)))
      (append
       common
       (list #'niwa-pipeline--step-hook-side-channel
             #'niwa-pipeline--step-repair-reminder)
       (when (niwa-tool-max-result-size tool)
         (list #'niwa-pipeline--step-persist))
       (when (equal (niwa-tool-name tool) "ToolCall")
         (list #'niwa-pipeline--step-ptc-checkpoint))
       (list #'niwa-pipeline--step-goal-budget-warning
             #'niwa-pipeline--step-attach-render-data
             #'niwa-pipeline--step-attach-media-data)))))


;;
;;; Entry point

(defun niwa-pipeline--run-tool (tool callback args outcome-only-p metadata)
  "Execute TOOL through the standard pipeline.

CALLBACK is the async result callback from gptel.  ARGS is a plist of
tool arguments (e.g., (:file_path \"/foo\" :content \"bar\")).

Captures the caller's session and workspace into the pipeline
context at entry time.  Steps that run after the handler must read
these from the context, not via `buffer-local-value' on
`current-buffer' -- handlers are free to wrap their work and the
callback in `with-temp-buffer', leaving post-handler steps
executing in a buffer that has no session binding.

CALLBACK is wrapped in a once-fire guard before being threaded into
the runner: the runner's `condition-case' branches fire it directly
on a sync error, the per-step `fail-cont' fires it on an explicit
fail, and the empty-steps branch fires it on success -- without the
guard, a sync error escaping a step's NEXT recursion (after the
recursion already delivered a success result to CALLBACK) would
double-fire.  Errors from the wrapped invocation are caught and
logged so a misbehaving CALLBACK cannot strand the pipeline."
  (let* ((dispatch-buffer (current-buffer))
         (session (and (boundp 'niwa--session) niwa--session))
         (workspace
          (cond
           (session (niwa-session-workspace session))
           ((and (boundp 'niwa--workspace) niwa--workspace))))
         (scope (niwa-diagnostic-scope (or workspace session)))
         (workdir (file-name-as-directory
                   (niwa-conversation-working-directory session)))
         (request (niwa-pipeline--current-request))
         (invocation (niwa-pipeline--current-invocation))
         (fsm (or (plist-get metadata :engine)
                  (and (boundp 'niwa-tools--current-engine)
                       niwa-tools--current-engine)))
         (tool-use-id (or (plist-get metadata :tool-use-id)
                          (niwa-pipeline--current-tool-use-id tool args)))
         (repair-entry
          (niwa-tool-repair-consume-ledger-entry tool args))
         (resource-attempts-cell (list nil))
         (provider-p (or (not outcome-only-p)
                         (eq (plist-get metadata :projection) 'provider)))
         (steps (niwa-pipeline--build-steps tool (not provider-p)))
         (cancel-cell (list nil))
         (sandbox-summary-cell (list nil))
         (context (list :tool tool :args args
                        :diagnostic-scope scope
                        :session session
                        :workspace workspace
                        :request request :invocation invocation :engine fsm
                        :tool-use-id tool-use-id
                        :repair-entry repair-entry
                        :input-repairs (plist-get repair-entry :repairs)
                        :hook-audit-records
                        (condition-case nil
                            (when-let* ((records
                                         (plist-get repair-entry :repairs))
                                        (audit
                                         (niwa-tool-repair-audit-record
                                          'committed records)))
                              (list audit))
                          (error
                           (ignore-errors
                             (niwa-diagnostic-warning
                              scope
                              'niwa
                              "Tool input repair audit construction failed"
                              :warning))
                           nil))
                        :origin (or (plist-get metadata :origin)
                                    (niwa-current-origin))
                        :call-source (plist-get metadata :source)
                        :progress-callback (plist-get metadata :progress)
                        :tool-event-callback #'niwa-pipeline--tool-event
                        :parent-tool-use-id
                        (plist-get metadata :parent-tool-use-id)
                        :buffer dispatch-buffer
                        :default-directory workdir
                        :resource-attempts-cell resource-attempts-cell
                        :cancel-cell cancel-cell
                        :sandbox-summary-cell sandbox-summary-cell))
         (called nil)
         (once-callback
          (lambda (outcome)
            (niwa-pipeline--discard-resource-attempts context)
            (cond
             ((not called)
              (setq called t)
              (let* ((classification (plist-get outcome :status))
                     (result (plist-get outcome :result))
                     (delivery
                      (if outcome-only-p
                          (if provider-p
                              (plist-put outcome :result
                                         (niwa-pipeline--provider-result
                                          outcome scope))
                            outcome)
                        (niwa-pipeline--provider-result outcome scope))))
                (niwa-telemetry-record-audit
                 session 'tool-finished
                 :tool-name (niwa-tool-name tool)
                 :tool-use-id tool-use-id
                 :parent-tool-use-id (plist-get metadata :parent-tool-use-id)
                 :call-source (plist-get metadata :source)
                 :request-id (and request (niwa-request-id request))
                 :outcome classification
                 :result-chars (and (stringp result) (length result))
                 :result-bytes (and (stringp result) (string-bytes result)))
                (niwa-tool-repair-record-result
                 repair-entry result nil classification)
                (niwa-pipeline--tool-event 'settled context outcome)
                (condition-case err
                    (funcall callback delivery)
                  (error
                   (niwa-diagnostic-warning
                    scope
                    'niwa
                    (format "Pipeline final callback signaled: %S" err)
                    :warning)))))
             (t
              ;; Symmetric with the per-step latch's warning at
              ;; `niwa-pipeline--run'.  The runner's condition-case
              ;; reaches us here when it caught a sync error escaping
              ;; from a step's NEXT recursion AFTER the recursion
              ;; already fired CALLBACK with a success result.  That is
              ;; the bug-fix path; we drop the late error but flag it so
              ;; a recurring drop is diagnosable rather than silent.
              (niwa--warn-once
               scope
               'pipeline-late-callback
               "Pipeline callback fired twice; dropping late \
delivery: %S"
               outcome))))))
    (let ((cancel
           (lambda ()
             (when-let* ((current (car cancel-cell)))
               (funcall current)))))
      (niwa-telemetry-record-audit
       session 'tool-received
       :tool-name (niwa-tool-name tool)
       :tool-use-id tool-use-id
       :parent-tool-use-id (plist-get metadata :parent-tool-use-id)
       :call-source (plist-get metadata :source)
       :request-id (and request (niwa-request-id request))
       :origin (niwa-current-origin)
       :read-only (and (niwa-tool-read-only-p tool) t))
      (niwa-pipeline--tool-event 'started context)
      (niwa-pipeline--dispatch tool once-callback context steps)
      ;; Handler cleanup must run before this outer settlement canceller.
      (when request
        (niwa-request-push-canceller request cancel))
      cancel)))

(defun niwa-pipeline--dispatch (tool callback context steps)
  "Assert mutation authority for TOOL, then run STEPS on CONTEXT.

gptel dispatches tool calls from its curl process sentinel, and applies
an async tool's function bare -- a signal escaping here kills the
sentinel and the request waits forever for a result that never comes.
Two consequences.  The authority assertion performs target I/O, and a
sentinel may run inside another remote operation's wait loop, where the
control filesystem refuses to nest; that refusal is transient, so the
dispatch defers and retries once the transport is idle instead of
failing the call.  Any error the assertion still signals must settle
the tool call as a canonical error result rather than escape."
  (let* ((session (plist-get context :session))
         ;; Bash owns mutation admission after hooks and argument analysis in
         ;; `niwa-execution-start-bash'.  Its static mutating-tool flag must
         ;; not materialize session state for readers or denied Plan calls.
         (mutating-p (and session
                          (not (niwa-tool-read-only-p tool))
                          (not (equal (niwa-tool-name tool) "Bash"))))
         (workdir (plist-get context :default-directory))
         (cancel-cell (plist-get context :cancel-cell))
         (key (list 'tool-dispatch
                    (or (plist-get context :tool-use-id)
                        (make-symbol "tool-dispatch"))))
         (finished nil)
         (finish-dispatch
          (lambda (reason message)
            (unless finished
              (setq finished t)
              (when cancel-cell
                (setcar cancel-cell nil))
              (funcall callback
                       (niwa-pipeline--settlement
                        context reason message)))))
         (cancel-dispatch
          (lambda ()
            (funcall finish-dispatch 'cancelled "Request cancelled")))
         (start
          (lambda ()
            (when cancel-cell
              (setcar cancel-cell cancel-dispatch))
            (when (if mutating-p
                      (condition-case err
                          (progn
                            (niwa-session-artifacts-assert-new-mutation-authority
                             session)
                            (not finished))
                        (error
                         (funcall finish-dispatch
                                  'pipeline-error (error-message-string err))
                         nil))
                    t)
              (unless finished
                (setq finished t)
                (niwa-pipeline--run steps callback context))))))
    (if (and mutating-p (niwa-transport-busy-p workdir))
        (progn
          (when cancel-cell
            (setcar cancel-cell
                    (lambda ()
                      (niwa-transport-cancel-pending key))))
          ;; A disabled transport drops deferred work, so fall back to the
          ;; guarded immediate path rather than strand the tool call.
          (unless
              (niwa-transport-run-when-idle
               key workdir start cancel-dispatch)
            (funcall start)))
      (funcall start))))

(defun niwa-pipeline-run-tool (tool callback args)
  "Execute TOOL and deliver its provider-facing result to CALLBACK."
  (niwa-pipeline--run-tool tool callback args nil nil))

(defun niwa-pipeline-run-tool-outcome (tool callback args &optional metadata)
  "Execute TOOL and deliver its canonical structured outcome to CALLBACK.

METADATA may supply `:tool-use-id', `:parent-tool-use-id', `:source',
`:origin', the owning `:engine', and a `:progress' callback for a nested
caller.  The progress callback receives `permission-wait' when the call
enters the permission queue, and a plist with `:type permission-approved'
and display-only `:permission-approvals' after accepted automatic review.
Provider-only reminders, persistence,
nudges, and transcript side channels are not applied unless METADATA
selects `:projection provider'.  That projection retains structured status
and media while applying the same persistence and presentation as a direct
provider call.  Return a zero-argument cancellation thunk for the call."
  (niwa-pipeline--run-tool tool callback args t metadata))


(provide 'niwa-pipeline)
;;; niwa-pipeline.el ends here
