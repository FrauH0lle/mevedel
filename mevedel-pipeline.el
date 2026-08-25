;;; mevedel-pipeline.el -- Tool execution pipeline -*- lexical-binding: t -*-

;;; Commentary:

;; Owns tool context, standard steps, sequencing, and ordering.  Each tool
;; invocation first runs the canonical pipeline: validate -> pre-tool-hooks ->
;; normalize-paths -> prepare-resources -> permission -> capture-coverage ->
;; snapshot -> handler -> render-transform -> post-tool-hooks.  Provider-facing
;; calls then append hook context, repair feedback, specialist nudges,
;; oversized-result persistence, Goal budget guidance, render data, and media.
;; Interactive handlers own any confirmation needed after the pipeline's
;; permission and snapshot steps.
;; Permission orchestration lives in mevedel-tool-permission.el.  Render-data
;; serialization, provider scrubbing, and transcript mutation live in
;; mevedel-tool-render-data.el.
;;
;; The persist step saves oversized results to disk and replaces them
;; with a preview + logical artifact address, preventing LLM context overflow from
;; unexpectedly large tool output.

;;; Code:

(require 'cl-lib)

(require 'mevedel-structs)
(require 'mevedel-hooks)
(require 'mevedel-utilities)
(require 'mevedel-tool-repair)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (fsm))
(defvar gptel-backend)

;; `mevedel-execution-telemetry'
(declare-function mevedel-execution-telemetry-sandbox-summary-class
                  "mevedel-execution-telemetry" (summary))
(defvar mevedel-execution-telemetry-summary-cell)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-prefix
                  "mevedel-execution-target" (cl-x) t)

;; `mevedel-goal'
(declare-function mevedel-goal-tool-result-budget-warning
                  "mevedel-goal" (session fsm))

;; `mevedel-hooks'
(declare-function mevedel-hooks-context-audit-records
                  "mevedel-hooks" (decision event type &optional omit-context))
(declare-function mevedel-hooks-decision-reason
                  "mevedel-hooks" (decision))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function mevedel-hooks-tool-event-plist
                  "mevedel-hooks" (event context &rest extra))

;; `mevedel-resource'
(declare-function mevedel-resource-address-like-p "mevedel-resource" (value))
(declare-function mevedel-resource-artifact-address "mevedel-resource"
                  (path session))
(declare-function mevedel-resource-discard-attempts "mevedel-resource"
                  (attempts))
(declare-function mevedel-resource-normalize-file-path "mevedel-resource"
                  (value &optional directory))
(declare-function mevedel-resource-prepare "mevedel-resource"
                  (operation address context))
(defvar mevedel-resource-attempts-cell)
(defvar mevedel-resource-current-attempts)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(declare-function mevedel-session-artifacts-publish-text
                  "mevedel-session-artifacts"
                  (session path content &optional coding))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))

;; `mevedel-specialist-nudges'
(declare-function mevedel-specialist-nudges-apply
                  "mevedel-specialist-nudges" (context))

;; `mevedel-structs'
(declare-function mevedel-request-directive-uuid "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-ephemeral-p "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-file-snapshots "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-detailed-p "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-finish "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-record-audit
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-start
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-media'
(declare-function mevedel-tool-media-attach-result
                  "mevedel-tool-media"
                  (result media tool-results-dir tool-use-id &optional session))
(declare-function mevedel-tool-media-normalize-items
                  "mevedel-tool-media" (items))
(declare-function mevedel-tool-media-result-for-hooks
                  "mevedel-tool-media" (result media))

;; `mevedel-tool-patch'
(declare-function mevedel-tool-patch-parse
                  "mevedel-tool-patch" (patch &optional root))
(declare-function mevedel-tool-patch-prepare-resources
                  "mevedel-tool-patch" (proposal))
(defvar mevedel-tool-patch-prepared-proposal)

;; `mevedel-tool-permission'
(declare-function mevedel-tool-permission-deny
                  "mevedel-tool-permission"
                  (context fail reason &optional model-reason provenance))
(declare-function mevedel-tool-permission-log-decision
                  "mevedel-tool-permission" (context decision &rest props))
(declare-function mevedel-tool-permission-paths
                  "mevedel-tool-permission" (tool args &optional context))
(declare-function mevedel-tool-permission-step
                  "mevedel-tool-permission" (context next fail))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-args "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-async-p "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-paths "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-groups "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-handler "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-max-result-size "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-render-transform
                  "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-snapshot-p "mevedel-tool-registry" (cl-x) t)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-format
                  "mevedel-tool-render-data"
                  (render-data &optional tool-use-id))
(declare-function mevedel-tool-render-data-size
                  "mevedel-tool-render-data" (data))
(declare-function mevedel-tool-render-data-strip-non-media
                  "mevedel-tool-render-data"
                  (string &optional expected-tool-use-id))

;; `mevedel-tool-repair-diagnostics'
(declare-function mevedel-tool-repair-audit-record
                  "mevedel-tool-repair-diagnostics" (state repairs))
(declare-function mevedel-tool-repair-record-result
                  "mevedel-tool-repair-diagnostics"
                  (entry result &optional outcome result-classification))

;; `mevedel-tools'
(declare-function mevedel-tools--ctx-record-used "mevedel-tools" (ctx name))
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(defvar mevedel-tools--current-fsm)

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(declare-function mevedel-request-note-untracked-effect
                  "mevedel-turn" (request source reason))
(declare-function mevedel-request-push-canceller
                  "mevedel-turn" (request canceller))

;; `mevedel-workspace'
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))
(defvar mevedel--workspace)

;; `subr'
(defvar read-eval)

(require 'mevedel-execution-target)
(require 'mevedel-execution-telemetry)
(require 'mevedel-resource)
(require 'mevedel-specialist-nudges)
(require 'mevedel-telemetry)
(require 'mevedel-tool-media)
(require 'mevedel-tool-permission)
(require 'mevedel-tool-render-data)
(require 'mevedel-turn)

(defvar mevedel-pipeline--active-tool-use-id nil
  "Tool-use id dynamically visible while a handler starts its work.")

(defvar mevedel-pipeline--active-call-source nil
  "Call source dynamically visible while a handler starts its work.
`ptc' names a ToolScript nested call, whose result reaches the script
instead of the provider transcript.")

(defvar mevedel-pipeline--auto-apply-edit-p nil
  "Non-nil while direct user authority auto-applies a native edit.")

(defvar mevedel-pipeline--canonical-path-map nil
  "Pre-authorized lexical-to-canonical paths for the active handler.")

(defun mevedel-pipeline-canonical-path (path)
  "Return PATH's pre-authorized canonical value for the active handler."
  (or (cdr (assoc path mevedel-pipeline--canonical-path-map)) path))


;;
;;; Error conditions

(define-error 'mevedel-pipeline-error "Pipeline error")
(define-error 'mevedel-permission-denied "Permission denied"
              'mevedel-pipeline-error)
(define-error 'mevedel-validation-error "Validation error"
              'mevedel-pipeline-error)


;;
;;; Result persistence

(defconst mevedel-pipeline--default-max-result-size 50000
  "Global cap on tool result size in characters.
When a tool declares a `max-result-size', the effective limit is
the minimum of the tool value and this default.")

(defconst mevedel-pipeline--preview-size 2000
  "Number of characters to include in the preview when a result is persisted.")

(defun mevedel-pipeline--head-tail-preview (result)
  "Return a bounded head-and-tail preview of RESULT.
Short strings pass through unchanged.  Long strings retain equal head and
tail budgets, preferring nearby line boundaries, and report the exact number
of omitted characters."
  (let ((length (length result)))
    (if (<= length mevedel-pipeline--preview-size)
        result
      (plist-get
       (mevedel--head-tail-preview-parts
        result result length mevedel-pipeline--preview-size)
       :text))))

(defun mevedel-pipeline-tool-results-dir (session buffer &optional request)
  "Return SESSION's tool-results directory, materializing when possible.

When SESSION has no save path yet, use
`mevedel-session-persistence-shallow-ensure-files' with BUFFER so
oversized tool output produced during the first turn can still be
owned by the session.  REQUEST defaults to BUFFER's active request.
Ephemeral requests never materialize or reuse a durable directory.
Returns nil when there is no session, the request is ephemeral, or
shallow materialization fails."
  (let ((request
         (or request
             (and buffer
                  (buffer-live-p buffer)
                  (local-variable-p 'mevedel--current-request buffer)
                  (buffer-local-value 'mevedel--current-request buffer)))))
    (when (and session
               (not (and request
                         (mevedel-request-ephemeral-p request))))
      (let ((save-path (or (mevedel-session-save-path session)
                           (when (and buffer (buffer-live-p buffer))
                             (require 'mevedel-session-persistence)
                             (mevedel-session-persistence-shallow-ensure-files
                              session buffer)))))
        (when save-path
          (require 'mevedel-workspace)
          (mevedel-workspace-ensure-generated-state-ignored
           (mevedel-session-workspace session))
          (file-name-concat save-path "tool-results"))))))

(defun mevedel-pipeline--persist-result (result tool session &optional buffer)
  "Save RESULT to disk and return a preview string.

TOOL is the `mevedel-tool' whose result exceeded its size limit.
SESSION owns the output file through its `tool-results/' directory.
BUFFER is the chat data buffer used to shallowly materialize SESSION
when it has not been saved yet.  If no session-owned directory is
available, falls back to `mevedel-pipeline--truncate-result'."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (setq result (mevedel--normalize-message-text result))
  (if-let* ((dir (mevedel-pipeline-tool-results-dir session buffer)))
      (let* ((name (mevedel-tool-name tool))
             (file (concat
                    (make-temp-name
                     (file-name-concat dir (concat name "-")))
                    ".txt"))
             (preview (mevedel-pipeline--head-tail-preview result)))
        (mevedel-session-artifacts-publish-text
         session file result 'utf-8-unix)
        (if-let* ((address (mevedel-resource-artifact-address file session)))
            (concat "<persisted-output>\n"
                    (format "Output too large (%d chars). Full output saved to: %s\n\n"
                            (length result) address)
                    (format "Use Read(file_path=%S, offset=1, limit=2000) and continue with the returned offset, or Grep(pattern=PATTERN, path=%S).\n\n"
                            address address)
                    "Preview (head and tail):\n"
                    preview "\n"
                    "</persisted-output>")
          (mevedel-pipeline--truncate-result result tool)))
    (mevedel-pipeline--truncate-result result tool)))

(defun mevedel-pipeline--truncate-result (result tool &optional ephemeral-p)
  "Truncate RESULT to a preview without persisting to disk.

Used when the result exceeds the size limit but no session-owned
persistence directory is available, or when EPHEMERAL-P forbids creating
one.  TOOL is used only for the tool name in the message."
  (concat (if ephemeral-p
              (format "Output too large (%d chars; tool: %s). Full output \
discarded for ephemeral request.\n\n"
                      (length result) (mevedel-tool-name tool))
            (format "Output too large (%d chars) and no session persistence \
directory available to persist full result (tool: %s).\n\n"
                    (length result) (mevedel-tool-name tool)))
          "The omitted content is unavailable; rerun the tool with a narrower scope.\n\n"
          "Preview (head and tail):\n"
          (mevedel-pipeline--head-tail-preview result)
          "\n"))

(defun mevedel-pipeline--truncate-error-result
    (result tool &optional legacy-prefix-p)
  "Truncate oversized error RESULT without persisting it.

When LEGACY-PREFIX-P is non-nil, keep an `Error:' prefix for callers that
still derive status from display text.  Structured-status callers receive a
neutral preview.  TOOL is used only for the tool name in the message."
  (setq result (mevedel--normalize-message-text result))
  (concat (format "%s too large (%d chars; tool: %s).\n\n"
                  (if legacy-prefix-p "Error: output" "Output")
                  (length result) (mevedel-tool-name tool))
          "The omitted content is unavailable; rerun the tool with a narrower scope.\n\n"
          "Preview (head and tail):\n"
          (mevedel-pipeline--head-tail-preview result)
          "\n"))


;;
;;; Pipeline runner

(defun mevedel-pipeline--step-name (step)
  "Return a readable name for STEP (for latch warnings)."
  (cond
   ((symbolp step) (symbol-name step))
   ((functionp step)
    (let ((name (and (consp step) (eq 'lambda (car step))
                     "<lambda>")))
      (or name "<anonymous>")))
   (t (format "%S" step))))

(defun mevedel-pipeline--format-failure (reason)
  "Format a `fail' REASON into the `Error: REASON' tool-result string.

REASON is typically a plain string.  Falls through to `%S' for any other
value so a misbehaving step still produces a legible error."
  (cond
   ((stringp reason) (format "Error: %s" reason))
   (t (format "Error: %S" reason))))

(defun mevedel-pipeline--settlement (context &optional reason message)
  "Return the canonical settlement for CONTEXT.
REASON and MESSAGE describe an early pipeline failure."
  (let* ((status (if reason 'error (mevedel-pipeline--context-status context)))
         (failure (and reason (mevedel-pipeline--format-failure message))))
    (list :status status
          :reason (or reason (and (eq status 'error) 'tool-error))
          :result (or failure (plist-get context :result))
          :raw-result (or failure (plist-get context :raw-result))
          :render-data (plist-get context :render-data)
          :media (plist-get context :media)
          :tool-use-id (plist-get context :tool-use-id)
          :parent-tool-use-id (plist-get context :parent-tool-use-id)
          :source (plist-get context :call-source)
          :failure-message (and reason message)
          :input-repairs (plist-get context :input-repairs)
          :hook-additional-context (plist-get context :hook-additional-context)
          :hook-audit-records (plist-get context :hook-audit-records))))

(defun mevedel-pipeline--format-outcome-failure (outcome)
  "Return OUTCOME's provider-facing early-failure text."
  (let ((failure (plist-get outcome :result)))
    (when-let* ((repairs (plist-get outcome :input-repairs)))
      (setq failure
            (condition-case nil
                (let ((reminder
                       (mevedel-tool-repair-format-reminder repairs)))
                  (if (string-search reminder failure)
                      failure
                    (concat failure "\n\n" reminder)))
              (error
               (ignore-errors
                 (display-warning
                  'mevedel
                  "Could not append tool input repair reminder"
                  :warning))
               failure))))
    failure))

(defun mevedel-pipeline--provider-result (outcome)
  "Project canonical OUTCOME into the provider-facing result value."
  (if (plist-get outcome :failure-message)
      (condition-case nil
          (mevedel-pipeline-append-hook-side-channel
           (mevedel-pipeline--format-outcome-failure outcome) outcome)
        (error
         (ignore-errors
           (display-warning 'mevedel "Pipeline audit formatting failed" :warning))
         (mevedel-pipeline--format-outcome-failure outcome)))
    (plist-get outcome :result)))

(defun mevedel-pipeline--context-default-directory (context)
  "Return the default directory captured for pipeline CONTEXT.

Tool dispatch should be rooted at the session working directory when a
session is available.  Falling back to the caller's original
`default-directory' preserves non-workspace uses and direct unit tests
that bypass `mevedel-pipeline-run-tool'."
  (file-name-as-directory
   (or (plist-get context :default-directory)
       default-directory)))

(defun mevedel-pipeline--with-context-default-directory (context thunk)
  "Call THUNK with `default-directory' set from CONTEXT."
  (let ((default-directory
         (mevedel-pipeline--context-default-directory context)))
    (funcall thunk)))

(defun mevedel-pipeline--current-tool-use-id (tool args)
  "Return the active gptel tool-use id for TOOL and ARGS, when known."
  (let* ((fsm (and (boundp 'mevedel-tools--current-fsm)
                   (symbol-value 'mevedel-tools--current-fsm)))
         (info (and fsm (ignore-errors (gptel-fsm-info fsm))))
         (tool-use (and info (plist-get info :tool-use)))
         (name (mevedel-tool-name tool))
         (call (and
                tool-use name
                (cl-find-if
                 (lambda (tc)
                   (and (not (plist-get tc :result))
                        (not (plist-get tc :mevedel-claimed))
                        (equal name (plist-get tc :name))
                        (mevedel-tool-repair--args-equivalent-p
                         (plist-get tc :args)
                         args)))
                 tool-use))))
    (when call
      (plist-put call :mevedel-claimed t)
      (plist-get call :id))))

(defun mevedel-pipeline-active-tool-use-id ()
  "Return the durable tool-use id of the currently starting handler."
  mevedel-pipeline--active-tool-use-id)

(defun mevedel-pipeline--discard-resource-attempts (context)
  "Discard prepared resource attempts held by CONTEXT."
  (when-let* ((cell (plist-get context :resource-attempts-cell))
              (attempts (and (consp cell) (car cell))))
    (mevedel-resource-discard-attempts attempts)
    (setcar cell nil)))

(defun mevedel-pipeline--run (steps callback context)
  "Run the pipeline, calling CALLBACK with one canonical settlement.

STEPS is a list of step functions.  Each step takes (CONTEXT NEXT FAIL)
where CONTEXT is a plist of accumulated state, NEXT is a continuation
taking an updated context plist, and FAIL takes a reason string plus optional
updated context and typed reason.

Sync steps may call NEXT or FAIL directly, or `signal' a
`mevedel-pipeline-error' subclass.  Async steps defer and call NEXT or
FAIL later (e.g., after a user prompt resolves); once a step has
scheduled an async continuation it must not signal -- the outer
`condition-case' has already unwound.

Each step's NEXT and FAIL continuations are wrapped in a per-step
**latch**: the first call settles the step; every later call is a no-op
logged via `mevedel--warn-once'.  This is defense-in-depth -- primitives
may latch at the UI layer too -- but the runner latch is authoritative.

CALLBACK must be the once-fire wrapper installed by
`mevedel-pipeline-run-tool'.  The runner's `condition-case' branches
fire CALLBACK directly with a canonical error settlement when a sync error
escapes the step body or its NEXT recursion -- the wrapper guarantees
the consumer sees exactly one outcome even when the recursion already
delivered a result before signaling.  Routing through the per-step
latch instead would deadlock here, since the latch correctly suppresses
a second outcome on a step that already fired NEXT.

CONTEXT is the initial plist."
  (if (null steps)
      (progn
        (when-let* ((cancel-cell (plist-get context :cancel-cell)))
          (setcar cancel-cell nil))
        (mevedel-pipeline--with-context-default-directory
         context
         (lambda ()
           (funcall callback (mevedel-pipeline--settlement context)))))
    (let* ((step (car steps))
           (rest (cdr steps))
           (step-name (mevedel-pipeline--step-name step))
           (settled nil)
           (telemetry-settled nil)
           (telemetry-span
            (when (and (plist-get context :session)
                       (fboundp 'mevedel-telemetry-detailed-p)
                       (mevedel-telemetry-detailed-p
                        (plist-get context :session))
                       (fboundp 'mevedel-telemetry-start))
              (mevedel-telemetry-start
               (plist-get context :session) 'tool-pipeline-step
               :tool-name (mevedel-tool-name (plist-get context :tool))
               :tool-use-id (plist-get context :tool-use-id)
               :parent-tool-use-id (plist-get context :parent-tool-use-id)
               :call-source (plist-get context :call-source)
               :step step-name)))
           (finish-telemetry
            (lambda (outcome &optional error-class)
              (unless telemetry-settled
                (setq telemetry-settled t)
                (when telemetry-span
                  (mevedel-telemetry-finish
                   telemetry-span :outcome outcome
                   :error-class error-class)))))
           (try-settle
            (lambda (which)
              (if settled
                  (progn
                    (unless (eq settled 'cancel)
                      (mevedel--warn-once
                       (list 'pipeline-duplicate-outcome step-name)
                       "Pipeline step %s called %s after already %s; \
ignoring duplicate outcome"
                       step-name which settled))
                    nil)
                (setq settled which)
                t)))
           (cancel-cell (plist-get context :cancel-cell))
           (clear-cancel
            (lambda ()
              (when cancel-cell (setcar cancel-cell nil))))
           (next-cont
            (lambda (updated-ctx)
              (when (funcall try-settle 'next)
                (funcall clear-cancel)
                (funcall finish-telemetry 'next)
                (mevedel-pipeline--run rest callback updated-ctx))))
           (fail-cont
            (lambda (reason &optional updated-context kind)
              (when (funcall try-settle 'fail)
                (funcall clear-cancel)
                (funcall finish-telemetry 'fail)
                (mevedel-pipeline--with-context-default-directory
                 (or updated-context context)
                 (lambda ()
                   (funcall callback
                            (mevedel-pipeline--settlement
                             (or updated-context context)
                             (or kind 'pipeline-error) reason)))))))
           (cancel-cont
            (lambda ()
              (when (funcall try-settle 'cancel)
                (funcall clear-cancel)
                (funcall finish-telemetry 'cancelled 'request-cancelled)
                (mevedel-pipeline--with-context-default-directory
                 context
                 (lambda ()
                   (funcall callback
                            (mevedel-pipeline--settlement
                             context 'cancelled "Request cancelled"))))))))
      (when cancel-cell
        (setcar cancel-cell cancel-cont))
      (condition-case err
          (mevedel-pipeline--with-context-default-directory
           context
           (lambda ()
             (funcall step context next-cont fail-cont)))
        (mevedel-validation-error
         (funcall clear-cancel)
         (funcall finish-telemetry 'error 'validation)
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--settlement
                      context 'validation
                      (or (cadr err) "Validation error"))))))
        (mevedel-resource-error
         (funcall clear-cancel)
         (funcall finish-telemetry 'error 'validation)
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--settlement
                      context 'invalid-resource
                      (or (cadr err) "Invalid resource address"))))))
        (mevedel-permission-denied
         (funcall clear-cancel)
         (funcall finish-telemetry 'error 'permission-denied)
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--settlement
                      context 'permission-denied
                      (if (cadr err)
                          (format "Permission denied: %s" (cadr err))
                        "Permission denied"))))))
        (mevedel-pipeline-error
         (funcall clear-cancel)
         (funcall finish-telemetry 'error 'pipeline)
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--settlement
                      context 'pipeline-error
                      (or (cadr err) "Pipeline error"))))))
        (error
         (funcall clear-cancel)
         (funcall finish-telemetry 'error (car-safe err))
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--settlement
                      context 'pipeline-error
                      (error-message-string err))))))))))


;;
;;; Standard steps

(defun mevedel-pipeline--step-validate (context next _fail)
  "Validate tool arguments against the arg spec.

Signals `mevedel-validation-error' on failure (the runner's
`condition-case' translates the signal into `fail'), calls NEXT on
success.  CONTEXT must contain `:tool' and `:args'.  FAIL is unused --
validation fails synchronously, which the runner catches through its
signal handler."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (issues (mevedel-tool-repair-validate tool args)))
    (if issues
        (signal 'mevedel-validation-error
                (list (mevedel-tool-repair-format-issues tool issues)))
      (funcall next context))))

(defun mevedel-pipeline--normalize-path-value
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
          (mevedel-execution-target-expand-path
           execution-target value directory))
      (mevedel-resource-normalize-file-path value directory))))

(defun mevedel-pipeline--canonicalize-path-value
    (value &optional execution-target)
  "Resolve VALUE's symlinks without leaving EXECUTION-TARGET.

`file-truename' preserves a nonexistent tail while resolving its
existing ancestors.  Remote lookups bypass TRAMP's attribute cache so
authorization observes the target's current link destination."
  (if (or (not (stringp value)) (string-empty-p value))
      value
    (let* ((remote-file-name-inhibit-cache t)
           (canonical (file-truename value)))
      (if execution-target
          (concat
           (mevedel-execution-target-prefix execution-target)
           (mevedel-execution-target-native-path
            execution-target canonical))
        canonical))))

(defun mevedel-pipeline--step-normalize-paths (context next _fail)
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
                      (mevedel-session-execution-target session)))
         (directory (plist-get context :default-directory))
         updated)
    (when (listp args)
      (dolist (spec (mevedel-tool-args tool))
        (let* ((type (cadr spec))
               (key (intern (concat ":" (symbol-name (car spec)))))
               (value (plist-get (or updated args) key))
               (resource-p (and (eq type 'path-or-resource)
                                (mevedel-resource-address-like-p value))))
          (when (and (memq type '(path path-or-resource))
                     (not resource-p))
            (let* ((expanded (mevedel-pipeline--normalize-path-value
                              value target directory))
                   (normalized
                    (if (eq type 'path)
                        (mevedel-pipeline--canonicalize-path-value
                         expanded target)
                      expanded)))
              (unless (equal value normalized)
                (setq updated
                      (plist-put (or updated (copy-sequence args))
                                 key normalized))))))))
    (let ((normalized-context
           (if updated (plist-put context :args updated) context)))
      (when-let* ((getter (mevedel-tool-get-paths tool)))
        (let ((default-directory (or directory default-directory))
              path-map)
          (if (eq (mevedel-pipeline--resource-operation tool) 'apply-patch)
              ;; Parse the patch once for the whole request and canonicalize
              ;; the proposal in place.  The resource step reuses it, so a
              ;; symlink swapped during patch review cannot redirect an
              ;; already authorized destination.
              (let ((proposal
                     (mevedel-tool-patch-parse
                      (plist-get (plist-get normalized-context :args) :patch)
                      directory)))
                (dolist (operation (plist-get proposal :operations))
                  (dolist (key '(:path :move-path))
                    (when-let* ((path (plist-get operation key))
                                (canonical
                                 (mevedel-pipeline--canonicalize-path-value
                                  path target)))
                      (unless (equal path canonical)
                        (push (cons path canonical) path-map))
                      (plist-put operation key canonical))))
                (setq normalized-context
                      (plist-put normalized-context :patch-parse proposal)))
            (dolist (path
                     (let ((mevedel-pipeline--canonical-path-map nil))
                       (delete-dups
                        (delq nil (funcall getter
                                           (plist-get normalized-context
                                                      :args))))))
              (push (cons path
                          (mevedel-pipeline--canonicalize-path-value
                           path target))
                    path-map)))
          (setq normalized-context
                (plist-put normalized-context :canonical-path-map
                           (nreverse path-map)))))
      (funcall next normalized-context))))

(defun mevedel-pipeline--resource-operation (tool)
  "Return the resource operation implemented by TOOL, or nil."
  (pcase (mevedel-tool-name tool)
    ("Read" 'read)
    ("Glob" 'glob)
    ("Grep" 'grep)
    ("ApplyPatch" 'apply-patch)))

(defun mevedel-pipeline--step-prepare-resources (context next _fail)
  "Prepare addressed operands after normalization and before permission.

Preparation is deliberately content-free.  It parses each semantic
`path-or-resource' argument once and stores opaque attempts for the handler;
ordinary filesystem paths pass through unchanged.  Malformed addresses and
unsupported operation/scheme pairs signal validation failures before any
permission or handler work begins."
  (let* ((tool (plist-get context :tool))
         (operation (mevedel-pipeline--resource-operation tool))
         (args (plist-get context :args))
         attempts
         patch-proposal)
    (if (eq operation 'apply-patch)
        (let ((mevedel-resource-attempts-cell
               (plist-get context :resource-attempts-cell)))
          (setq patch-proposal
                (mevedel-tool-patch-prepare-resources
                 (or (plist-get context :patch-parse)
                     (mevedel-tool-patch-parse
                      (plist-get args :patch)
                      (plist-get context :default-directory))))))
      (when operation
      (dolist (spec (mevedel-tool-args tool))
        (when (eq (cadr spec) 'path-or-resource)
          (let* ((key (intern (concat ":" (symbol-name (car spec)))))
                 (address (plist-get args key)))
            (when (and (stringp address)
                       (mevedel-resource-address-like-p address))
              (let ((attempt
                     (mevedel-resource-prepare operation address context)))
                (push (cons address attempt) attempts))))))))
    (funcall next
             (cond
              (patch-proposal
               (plist-put context :patch-proposal patch-proposal))
              (attempts
               (plist-put context :resource-attempts attempts))
              (t context)))))

(defun mevedel-pipeline--current-request ()
  "Return the current mevedel request struct, if any."
  (and (boundp 'mevedel--current-request)
       mevedel--current-request))

(defun mevedel-pipeline--current-invocation ()
  "Return the current agent invocation struct, if any."
  (and (boundp 'mevedel--agent-invocation)
       mevedel--agent-invocation))

(defun mevedel-pipeline--validate-updated-args (tool args)
  "Return validation error for TOOL ARGS, or nil."
  (when-let* ((issues (mevedel-tool-repair-validate tool args)))
    (mevedel-tool-repair-format-issues tool issues)))

(defun mevedel-pipeline-record-hook-context (context decision &optional event)
  "Append DECISION's additional hook context to CONTEXT.
EVENT labels generated hook event blocks."
  (if-let* ((entries (mevedel-hooks-context-entries
                     decision (or event 'PreToolUse))))
      (plist-put context :hook-additional-context
                 (append (plist-get context :hook-additional-context)
                         entries))
    context))

(defun mevedel-pipeline--append-hook-context-string (text context)
  "Append accumulated hook context from CONTEXT to TEXT."
  (let ((additional (plist-get context :hook-additional-context)))
    (if-let* (((and additional (stringp text)))
              (formatted (mevedel-hooks-format-context additional)))
        (concat text
                "\n\n"
                formatted)
      text)))

(defun mevedel-pipeline-record-hook-audit (context records)
  "Append hook audit RECORDS to CONTEXT."
  (let ((records (if (and (listp records)
                          (keywordp (car-safe records)))
                     (list records)
                   records)))
    (if records
        (plist-put context :hook-audit-records
                   (append (plist-get context :hook-audit-records)
                           records))
      context)))

(defun mevedel-pipeline--append-hook-audit-records (text records)
  "Append hidden hook audit RECORDS to TEXT."
  (if (and records (stringp text))
      (concat text
              (mapconcat #'mevedel--format-hook-audit-record records ""))
    text))

(defun mevedel-pipeline-append-hook-side-channel (text context)
  "Append accumulated hook context and audit records from CONTEXT to TEXT."
  (mevedel-pipeline--append-hook-audit-records
   (mevedel-pipeline--append-hook-context-string text context)
   (plist-get context :hook-audit-records)))

(defun mevedel-pipeline-hook-context-audit-records (decision event)
  "Return audit records for DECISION additional context at EVENT."
  (mevedel-hooks-context-audit-records decision event 'tool-context))

(defun mevedel-pipeline-hook-permission-audit-record
    (event outcome decision &optional reason)
  "Return a permission audit record for hook EVENT and OUTCOME."
  (append
   (list :type 'tool-permission
         :event (mevedel-hooks-event-display-name event)
         :outcome (format "%s" outcome))
   (when-let* ((reason (or reason
                           (mevedel-hooks-decision-reason decision))))
     (list :reason reason))))

(defun mevedel-pipeline--hook-input-rewrite-audit-record
    (event original updated decision)
  "Return a tool input rewrite audit record for hook EVENT."
  (append
   (list :type 'tool-input-rewrite
         :event (mevedel-hooks-event-display-name event)
         :original-input original
         :updated-input updated)
   (when-let* ((reason (mevedel-hooks-decision-reason decision)))
     (list :reason reason))))

(defun mevedel-pipeline--hook-result-rewrite-audit-record
    (event original updated decision)
  "Return a result rewrite audit record for hook EVENT."
  (append
   (list :type 'tool-result-rewrite
         :event (mevedel-hooks-event-display-name event)
         :original-result (or original "")
         :updated-result (or updated ""))
   (when-let* ((reason (mevedel-hooks-decision-reason decision)))
     (list :reason reason))))

(defun mevedel-pipeline-run-hook-event
    (event event-plist callback context session workspace request invocation)
  "Run hook EVENT with EVENT-PLIST in CONTEXT's live dispatch buffer.

CALLBACK, SESSION, WORKSPACE, REQUEST, and INVOCATION are forwarded to
the hook runner."
  (let ((buffer (plist-get context :buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (mevedel-hooks-run-event
           event event-plist callback session workspace request invocation))
      (mevedel-hooks-run-event
       event event-plist callback session workspace request invocation))))

(defun mevedel-pipeline--step-pre-tool-hooks (context next fail)
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
    (mevedel-pipeline-run-hook-event
     'PreToolUse
     (mevedel-hooks-tool-event-plist 'PreToolUse context)
     (lambda (decision)
       (let ((stopped-p
              (and (plist-member decision :continue)
                   (not (plist-get decision :continue))))
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
                 (mevedel-pipeline-record-hook-audit
                  (mevedel-pipeline-record-hook-context
                   context decision 'PreToolUse)
                  (append
                   (list
                    (mevedel-pipeline-hook-permission-audit-record
                     'PreToolUse 'deny decision reason))
                    (mevedel-pipeline-hook-context-audit-records
                    decision 'PreToolUse)))))
             (mevedel-tool-permission-log-decision
              context
              (list :outcome 'deny
                    :raw-outcome `(deny . ,reason)
                    :via 'pre-tool-hook))
             (mevedel-tool-permission-deny
              updated fail
              (if stopped-p reason (format "Permission denied: %s" reason))
              reason 'PreToolUse)))
          (t
           (let ((updated (mevedel-pipeline-record-hook-context
                           context decision 'PreToolUse)))
             (setq updated
                   (mevedel-pipeline-record-hook-audit
                    updated
                    (mevedel-pipeline-hook-context-audit-records
                     decision 'PreToolUse)))
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
                        (err (mevedel-pipeline--validate-updated-args
                              tool args)))
                   (if err
                       (funcall fail err updated 'validation)
                     (funcall next
                              (plist-put
                               (mevedel-pipeline-record-hook-audit
                                updated
                                (mevedel-pipeline--hook-input-rewrite-audit-record
                                 'PreToolUse
                                 (plist-get context :args)
                                 args
                                 decision))
                               :args args))))
               (funcall next updated)))))))
     context session workspace request invocation)))

(defun mevedel-pipeline--step-snapshot (context next _fail)
  "Snapshot files before modification.

Extracts the path from tool args via the tool's get-path function and
snapshots it.  Only included for tools declaring snapshots.  CONTEXT must
contain `:tool' and `:args'.  NEXT is called on success.  FAIL is
unused -- a snapshot failure is best-effort and should never fail the
  pipeline."
  (let ((tool (plist-get context :tool))
        (args (plist-get context :args)))
    (dolist (path (mevedel-tool-permission-paths tool args context))
      (mevedel-pipeline--snapshot-file-if-needed
       (plist-get context :request) path))
    (funcall next context)))

(defun mevedel-pipeline--snapshot-file-if-needed (request filepath)
  "Capture FILEPATH's original state once in REQUEST.
Missing files are stored as nil; unreadable paths retain a diagnostic gap."
  (when (and request filepath (stringp filepath))
    (let* ((abs-path (expand-file-name filepath))
           (snapshots (mevedel-request-file-snapshots request))
           (missing (make-symbol "missing")))
      (when (eq missing (gethash abs-path snapshots missing))
        (condition-case err
            (let ((original (when (file-exists-p abs-path)
                              (with-temp-buffer
                                (insert-file-contents abs-path)
                                (buffer-string)))))
              (puthash abs-path original snapshots))
          (error
           (puthash abs-path
                    (list :gap (error-message-string err))
                    snapshots)))))))

(defun mevedel-pipeline--untracked-filesystem-effects-p (tool)
  "Return non-nil when TOOL can mutate files outside exact snapshots."
  (or (and (not (mevedel-tool-read-only-p tool))
           (or (memq 'eval (mevedel-tool-groups tool))
               (and (memq 'edit (mevedel-tool-groups tool))
                    (not (mevedel-tool-snapshot-p tool)))))
      (member (mevedel-tool-name tool) '("Agent" "FollowupAgent"))))

(defun mevedel-pipeline--step-capture-coverage (context next _fail)
  "Record untracked directive filesystem effects from CONTEXT, then call NEXT."
  (let ((tool (plist-get context :tool))
        (request (plist-get context :request)))
    (when (and request
               (mevedel-request-directive-uuid request)
               (mevedel-pipeline--untracked-filesystem-effects-p tool))
      (mevedel-request-note-untracked-effect
       request
       (mevedel-tool-name tool)
       (if (member (mevedel-tool-name tool) '("Agent" "FollowupAgent"))
           "Delegated work can modify files outside parent snapshots"
         "Tool execution can modify files outside exact path snapshots")))
    (funcall next context)))

(defun mevedel-pipeline--record-use (tool)
  "Record that TOOL was invoked on the current turn.

Pushes the tool's name onto the current deferred context's
`deferred-used' slot so that the WAIT handler can reset the TTL for
any tool the model called since the previous turn.  The context is
either a `mevedel-session' (main chat) or a
`mevedel-agent-invocation' (spawned sub-agent), resolved via
`mevedel-tools--current-deferred-context'.  The entry is stored
regardless of whether the tool is deferred; the WAIT handler filters
against the injected set."
  (when-let* ((ctx (mevedel-tools--current-deferred-context)))
    (mevedel-tools--ctx-record-used ctx (mevedel-tool-name tool))))

(defun mevedel-pipeline--handler-return-p (value)
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

(defun mevedel-pipeline--context-status (context)
  "Return CONTEXT's canonical handler status."
  (or (plist-get context :handler-status)
      (plist-get context :status)
      'success))

(defun mevedel-pipeline--normalize-tool-string (value)
  "Return VALUE as JSON-safe model text when VALUE is a string."
  (if (stringp value)
      (mevedel--normalize-message-text value)
    value))

(defun mevedel-pipeline--step-handler (context next fail)
  "Run the tool handler.

For async tools (async-p is non-nil), the handler receives a callback as
its first argument followed by the args plist.  For sync tools, the
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
         (handler (mevedel-tool-handler tool))
         (args (plist-get context :args))
         (repair-entry (plist-get context :repair-entry))
         (store (lambda (raw)
                  (let ((result
                         (mevedel-pipeline--normalize-tool-string
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
                    (plist-put updated :media (plist-get raw :media)))))
         (finish
          (lambda (raw)
            (funcall
             next
             (funcall
              store
              (if (mevedel-pipeline--handler-return-p raw)
                  raw
                (list
                 :result
                 (format
                  "Error: Tool %s handler returned invalid value; expected a plist containing :result"
                  (mevedel-tool-name tool))
                 :status 'error))))))
         (invoke
          (lambda ()
            (let ((mevedel-pipeline--active-tool-use-id
                   (plist-get context :tool-use-id))
                  (mevedel-pipeline--active-call-source
                   (plist-get context :call-source))
                  (mevedel-pipeline--auto-apply-edit-p
                   (plist-get context :auto-apply-edit-p))
                  (mevedel-resource-current-attempts
                   (plist-get context :resource-attempts))
                  (mevedel-tool-patch-prepared-proposal
                   (plist-get context :patch-proposal))
                  (mevedel-pipeline--canonical-path-map
                   (plist-get context :canonical-path-map))
                  (mevedel-execution-telemetry-summary-cell
                   (plist-get context :sandbox-summary-cell)))
              (mevedel-tool-repair-mark-executed repair-entry)
              (mevedel-pipeline--record-use tool)
              (condition-case err
                  (if (mevedel-tool-async-p tool)
                      (funcall handler finish args)
                    (funcall finish (funcall handler args)))
                (error
                 (funcall
                  finish
                  (list :result (format "Error: %s"
                                        (error-message-string err))
                        :status 'error)))))))
         (dispatch-buffer (plist-get context :buffer)))
    (cond
     ((null dispatch-buffer) (funcall invoke))
     ((buffer-live-p dispatch-buffer)
      (with-current-buffer dispatch-buffer
        (funcall invoke)))
     (t (funcall fail "Tool dispatch buffer is no longer live")))))

(defun mevedel-pipeline--step-repair-reminder (context next _fail)
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
                      (mevedel-tool-repair-format-reminder records))))
          (error
           (display-warning
            'mevedel
            (format "Could not append tool input repair reminder: %S" err)
            :warning)
           (funcall next context)))
      (funcall next context))))


(defconst mevedel-pipeline--render-transform-max-data-size 8192
  "Maximum printed size of render-data produced by `:render-transform'.

Handler-provided render-data is not capped here because some existing
handlers intentionally carry larger structured payloads, such as edit
diffs.  Transform functions are for bounded metadata derived from a
string result, not for copying the result body into a hidden side
channel.")

(defun mevedel-pipeline--step-render-transform (context next _fail)
  "Run CONTEXT's TOOL `:render-transform', then call NEXT.

The transform receives the normalized string result before oversized
result persistence and before render/media side-channel attachment.
It is skipped when the handler already supplied render-data, when the
result is not a string, or when the result status is `error'.

FAIL is unused; transform failures warn and leave CONTEXT unchanged."
  (let* ((tool (plist-get context :tool))
         (transform (and tool (mevedel-tool-render-transform tool)))
         (name (and tool (mevedel-tool-name tool)))
         (args (plist-get context :args))
         (result (plist-get context :result))
         (existing-render-data (plist-get context :render-data)))
    (if (or existing-render-data
            (not (functionp transform))
            (not (stringp result))
            (eq 'error (mevedel-pipeline--context-status context)))
        (funcall next context)
      (condition-case err
          (let ((render-data (funcall transform name args result)))
            (cond
             ((null render-data)
              (funcall next context))
             ((progn
                (> (mevedel-tool-render-data-size render-data)
                   mevedel-pipeline--render-transform-max-data-size))
              (display-warning
               'mevedel
               (format "Render transform for %s returned oversized metadata"
                       (or name "tool"))
               :warning)
              (funcall next context))
             (t
              (funcall next
                       (plist-put context :render-data render-data)))))
        (error
         (display-warning
          'mevedel
          (format "Render transform for %s failed: %s"
                  (or name "tool") (error-message-string err))
          :warning)
         (funcall next context))))))

(defun mevedel-pipeline--step-attach-render-data (context next _fail)
  "Embed render-data from CONTEXT, then call NEXT.

When CONTEXT holds render-data or an explicit handler status and the
`:result' is a string, append a hidden delimiter-wrapped block carrying
the serialized data.  Explicit status is stored under `:status' for renderer
dispatch.  The block is propertized `invisible' for the data-buffer display
and recognised by the view interpreter via its delimiters.  An `:around'
advice on `gptel--parse-tool-results' strips the block on the LLM path only;
see `mevedel-tool-render-data-format'.

FAIL is unused; render-data attachment never fails.

When neither was produced, passes CONTEXT through unchanged."
  (let* ((result (plist-get context :result))
         (status (plist-get context :status))
         (tool-use-id (plist-get context :tool-use-id))
         (render-data (plist-get context :render-data))
         (sandbox-summary
          (car (plist-get context :sandbox-summary-cell)))
         (render-data
          (if (and sandbox-summary
                   (progn
                     (mevedel-execution-telemetry-sandbox-summary-class
                      sandbox-summary)))
              (plist-put (copy-sequence render-data)
                         :sandbox-summary (copy-tree sandbox-summary))
            render-data))
         (render-data
          (if status
              (plist-put (copy-sequence render-data) :status status)
            render-data)))
    (if (and render-data (stringp result))
        (progn
          (funcall next
                   (plist-put context :result
                              (concat result
                                      (mevedel-tool-render-data-format
                                       render-data tool-use-id)))))
      (funcall next context))))

(defun mevedel-pipeline--step-attach-media-data (context next _fail)
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
                            (mevedel-tool-media-attach-result
                             result media
                             (mevedel-pipeline-tool-results-dir
                              (plist-get context :session)
                              (plist-get context :buffer)
                              (plist-get context :request))
                             (plist-get context :tool-use-id)
                             (plist-get context :session))))
      (funcall next context))))

(defun mevedel-pipeline--step-persist (context next _fail)
  "Persist oversized tool results to disk.

If the tool has a `max-result-size' and the string result exceeds the
effective limit (the minimum of the tool value and
`mevedel-pipeline--default-max-result-size'), saves the full result to
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
         (max-size (mevedel-tool-max-result-size tool))
         (effective (when max-size
                      (min max-size mevedel-pipeline--default-max-result-size)))
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
     ((eq 'error (mevedel-pipeline--context-status context))
      (funcall deliver
               (plist-put context :result
                          (mevedel-pipeline--truncate-error-result
                           result tool
                           (not (plist-member context :status))))
               'truncated))
     (t
      ;; Result exceeds limit -- persist or truncate.  Session/buffer
      ;; context was captured at `mevedel-pipeline-run-tool'
      ;; entry; do not re-read it from `current-buffer' here because
      ;; the handler may have run (and called back) from inside a
      ;; `with-temp-buffer' wrapper.
      (let ((session (plist-get context :session))
            (buffer (plist-get context :buffer))
            (request (plist-get context :request)))
        (let ((projected
               (if (and request
                        (mevedel-request-ephemeral-p request))
                   (mevedel-pipeline--truncate-result result tool t)
                 (mevedel-pipeline--persist-result
                  result tool session buffer))))
          (funcall deliver
                   (plist-put context :result projected)
                   (if (string-prefix-p "<persisted-output>" projected)
                       'persisted
                     'truncated))))))))

(defun mevedel-pipeline--step-post-tool-hooks (context next _fail)
  "Run post-tool hooks for CONTEXT, then call NEXT.

Hooks receive both `:raw-result' and the final `:result'.  Only an
explicit `:updated-result' changes the model-visible tool result."
  (let* ((media (mevedel-tool-media-normalize-items
                 (plist-get context :media)))
         (context (plist-put context :media media))
         (tool-use-id (plist-get context :tool-use-id))
         (result (plist-get context :result))
         (model-result
          (mevedel-tool-media-result-for-hooks
           (mevedel-tool-render-data-strip-non-media
            result tool-use-id)
           media))
         (raw-result
          (mevedel-tool-media-result-for-hooks
           (mevedel-tool-render-data-strip-non-media
            (plist-get context :raw-result) tool-use-id)
           media))
         (error-p (eq 'error (mevedel-pipeline--context-status context)))
         (event (if error-p
                    'PostToolUseFailure
                  'PostToolUse))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace)))
    (mevedel-pipeline-run-hook-event
     event
     (mevedel-hooks-tool-event-plist
      event context
      :raw-result raw-result
      :result model-result
      :tool-response model-result
      :error (and error-p result))
     (lambda (decision)
       (let ((context (mevedel-pipeline-record-hook-context
                       context decision event)))
         (setq context
               (mevedel-pipeline-record-hook-audit
                context
                (mevedel-pipeline-hook-context-audit-records
                 decision event)))
         (cond
          ((plist-member decision :updated-result)
           (setq context
                 (mevedel-pipeline-record-hook-audit
                  context
                  (mevedel-pipeline--hook-result-rewrite-audit-record
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

(defun mevedel-pipeline--step-hook-side-channel (context next _fail)
  "Append hook context and audit prose for a provider-facing CONTEXT."
  (funcall next
           (plist-put
            context :result
            (mevedel-pipeline-append-hook-side-channel
             (plist-get context :result) context))))

(defun mevedel-pipeline--step-goal-budget-warning (context next _fail)
  "Append an early Goal budget warning to CONTEXT, then call NEXT."
  (let ((result (plist-get context :result))
        (session (plist-get context :session))
        (fsm (plist-get context :fsm)))
    (if (and (stringp result) session fsm)
        (progn
          (require 'mevedel-goal)
          (if-let* ((warning
                     (mevedel-goal-tool-result-budget-warning session fsm)))
              (funcall
               next
               (plist-put
                context :result
                (format "%s\n\n<system-reminder>\n%s\n</system-reminder>"
                        result warning)))
            (funcall next context)))
      (funcall next context))))


;;
;;; Specialist tool nudges

(defun mevedel-pipeline--step-specialist-nudges (context next _fail)
  "Apply specialist-tool prompting policy to CONTEXT, then call NEXT."
  (funcall next (mevedel-specialist-nudges-apply context)))

;;
;;; Step list builder

(defun mevedel-pipeline--build-steps (tool &optional outcome-only-p)
  "Build the standard step list for TOOL.

Returns a list of step functions based on TOOL's behavioral flags:
  1. validate            -- always included
  2. pre-tool-hooks      -- always included
  3. normalize-paths     -- canonicalizes `path'-typed args for authorization
  4. prepare-resources   -- parses addressed operands before permission
  5. permission          -- always included
  6. capture-coverage    -- records mutation paths without exact snapshots
  7. snapshot            -- included when snapshot-p
  8. handler             -- always included
  9. render-transform    -- always included; no-op when tool has none
  10. post-tool-hooks    -- always included

Provider projection then appends hook context, repair feedback, and specialist
nudges, persists oversized output when declared, adds a Goal warning, and
attaches render-data and media.  Outcome-only consumers stop at the canonical
common boundary."
  (let ((common
         (append
          (list #'mevedel-pipeline--step-validate
                #'mevedel-pipeline--step-pre-tool-hooks
                #'mevedel-pipeline--step-normalize-paths
                #'mevedel-pipeline--step-prepare-resources
                #'mevedel-tool-permission-step
                #'mevedel-pipeline--step-capture-coverage)
          (when (mevedel-tool-snapshot-p tool)
            (list #'mevedel-pipeline--step-snapshot))
          (list #'mevedel-pipeline--step-handler
                #'mevedel-pipeline--step-render-transform
                #'mevedel-pipeline--step-post-tool-hooks))))
    (if outcome-only-p
        common
      (append
       common
       (list #'mevedel-pipeline--step-hook-side-channel
             #'mevedel-pipeline--step-repair-reminder
             #'mevedel-pipeline--step-specialist-nudges)
       (when (mevedel-tool-max-result-size tool)
         (list #'mevedel-pipeline--step-persist))
       (list #'mevedel-pipeline--step-goal-budget-warning
             #'mevedel-pipeline--step-attach-render-data
             #'mevedel-pipeline--step-attach-media-data)))))


;;
;;; Entry point

(defun mevedel-pipeline--run-tool (tool callback args outcome-only-p metadata)
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
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (let* ((dispatch-buffer (current-buffer))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (workspace
          (cond
           (session (mevedel-session-workspace session))
           ((and (boundp 'mevedel--workspace) mevedel--workspace))))
         (workspace-root (and workspace
                              (ignore-errors
                                (mevedel-workspace-root workspace))))
         (session-dir (and session
                           (ignore-errors
                             (mevedel-session-working-directory session))))
         (workdir (file-name-as-directory
                   (or session-dir workspace-root default-directory)))
         (request (mevedel-pipeline--current-request))
         (invocation (mevedel-pipeline--current-invocation))
         (fsm (and (boundp 'mevedel-tools--current-fsm)
                   mevedel-tools--current-fsm))
         (tool-use-id (or (plist-get metadata :tool-use-id)
                          (mevedel-pipeline--current-tool-use-id tool args)))
         (repair-entry
          (mevedel-tool-repair-consume-ledger-entry tool args))
         (resource-attempts-cell (list nil))
         (steps (mevedel-pipeline--build-steps tool outcome-only-p))
         (cancel-cell (list nil))
         (sandbox-summary-cell (list nil))
         (context (list :tool tool :args args
                        :session session
                        :workspace workspace
                        :request request :invocation invocation :fsm fsm
                        :tool-use-id tool-use-id
                        :repair-entry repair-entry
                        :input-repairs (plist-get repair-entry :repairs)
                        :hook-audit-records
                        (condition-case nil
                            (when-let* ((records
                                         (plist-get repair-entry :repairs))
                                        (audit
                                         (mevedel-tool-repair-audit-record
                                          'committed records)))
                              (list audit))
                          (error
                           (ignore-errors
                             (display-warning
                              'mevedel
                              "Tool input repair audit construction failed"
                              :warning))
                           nil))
                        :origin (or (plist-get metadata :origin)
                                    (mevedel-current-origin))
                        :call-source (plist-get metadata :source)
                        :progress-callback (plist-get metadata :progress)
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
            (mevedel-pipeline--discard-resource-attempts context)
            (cond
             ((not called)
              (setq called t)
              (let* ((classification (plist-get outcome :status))
                     (result (plist-get outcome :result))
                     (delivery (if outcome-only-p
                                   outcome
                                 (mevedel-pipeline--provider-result outcome))))
                (mevedel-telemetry-record-audit
                 session 'tool-finished
                 :tool-name (mevedel-tool-name tool)
                 :tool-use-id tool-use-id
                 :parent-tool-use-id (plist-get metadata :parent-tool-use-id)
                 :call-source (plist-get metadata :source)
                 :request-id (and request (mevedel-request-id request))
                 :outcome classification
                 :result-chars (and (stringp result) (length result))
                 :result-bytes (and (stringp result) (string-bytes result)))
                (mevedel-tool-repair-record-result
                 repair-entry result nil classification)
                (condition-case err
                    (funcall callback delivery)
                  (error
                   (display-warning
                    'mevedel
                    (format "Pipeline final callback signaled: %S" err)
                    :warning)))))
             (t
              ;; Symmetric with the per-step latch's warning at
              ;; `mevedel-pipeline--run'.  The runner's condition-case
              ;; reaches us here when it caught a sync error escaping
              ;; from a step's NEXT recursion AFTER the recursion
              ;; already fired CALLBACK with a success result.  That is
              ;; the bug-fix path; we drop the late error but flag it so
              ;; a recurring drop is diagnosable rather than silent.
              (mevedel--warn-once
               'pipeline-late-callback
               "Pipeline callback fired twice; dropping late \
delivery: %S"
               outcome))))))
    (when (and session (not (mevedel-tool-read-only-p tool)))
      (mevedel-session-artifacts-assert-new-mutation-authority session))
    (let ((cancel
           (lambda ()
             (when-let* ((current (car cancel-cell)))
               (funcall current)))))
      (mevedel-telemetry-record-audit
       session 'tool-received
       :tool-name (mevedel-tool-name tool)
       :tool-use-id tool-use-id
       :parent-tool-use-id (plist-get metadata :parent-tool-use-id)
       :call-source (plist-get metadata :source)
       :request-id (and request (mevedel-request-id request))
       :origin (mevedel-current-origin)
       :read-only (and (mevedel-tool-read-only-p tool) t))
      (mevedel-pipeline--run steps once-callback context)
      ;; Handler cleanup must run before this outer settlement canceller.
      (when request
        (mevedel-request-push-canceller request cancel))
      cancel)))

(defun mevedel-pipeline-run-tool (tool callback args)
  "Execute TOOL and deliver its provider-facing result to CALLBACK."
  (mevedel-pipeline--run-tool tool callback args nil nil))

(defun mevedel-pipeline-run-tool-outcome (tool callback args &optional metadata)
  "Execute TOOL and deliver its canonical structured outcome to CALLBACK.

METADATA may supply `:tool-use-id', `:parent-tool-use-id', `:source',
`:origin', and a `:progress' callback for a nested caller.  The progress
callback receives `permission-wait' when the call enters the permission queue.
Provider-only reminders, persistence, nudges, and transcript side channels are
not applied.  Return a zero-argument cancellation thunk for the call."
  (mevedel-pipeline--run-tool tool callback args t metadata))


;;
;;; Args conversion

(defun mevedel-pipeline--positional-to-plist (arg-values arg-specs)
  "Convert positional ARG-VALUES to a keyword plist using ARG-SPECS.

ARG-SPECS is the mevedel args format: ((name type ...) ...).
ARG-VALUES is a list of values in the same order.
Returns a plist like (:name1 val1 :name2 val2 ...)."
  (let ((plist nil))
    (cl-loop for spec in arg-specs
             for val in arg-values
             do (push (intern (format ":%s" (car spec))) plist)
             (push val plist))
    (nreverse plist)))

(provide 'mevedel-pipeline)
;;; mevedel-pipeline.el ends here
