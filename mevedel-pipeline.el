;;; mevedel-pipeline.el -- Tool execution pipeline -*- lexical-binding: t -*-

;;; Commentary:

;; Sequential step-based execution engine for mevedel tools.  Each tool
;; invocation runs through a standard pipeline: validate -> pre-tool-hooks ->
;; permission -> snapshot -> handler -> repair-reminder -> render-transform ->
;; persist -> specialist-nudges -> post-hooks -> re-persist ->
;; attach-render-data -> attach-media.
;; Tool handlers that need user confirmation of a file change call
;; `mevedel-preview-mode-add-preview' directly; there is no explicit
;; confirm step in the pipeline.
;;
;; The persist step saves oversized results to disk and replaces them
;; with a preview + file path, preventing LLM context overflow from
;; unexpectedly large tool output.

;;; Code:

(require 'cl-lib)

(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-hooks)
(require 'mevedel-utilities)
(require 'mevedel-permission-log)
(require 'mevedel-tool-repair)

(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-handler "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-args "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-async-p "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-pattern "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-domain "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-max-result-size "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-render-transform
                  "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-check-permission-async "mevedel-permissions"
                  (tool-name cont &rest args))
(declare-function mevedel-check-permission-async-with-metadata
                  "mevedel-permissions" (tool-name cont &rest args))
(declare-function mevedel-permission-decision-raw-outcome
                  "mevedel-permissions" (decision))
(declare-function mevedel-permission--checker-args
                  "mevedel-permissions" (context))
(declare-function mevedel-permission--invocation-context
                  "mevedel-permissions" (&rest args))
(declare-function mevedel-permission--normalize-outcome
                  "mevedel-permissions" (outcome))
(declare-function mevedel-permission--path-protected-p
                  "mevedel-permissions" (path))
(declare-function mevedel-tool-exec--bash-decision-specifier-value
                  "mevedel-tool-exec" (command))
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))
(declare-function mevedel-permission--apply-prompt-result
                  "mevedel-permissions" (result tool-name &rest args))
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-request-id "mevedel-structs" (cl-x))
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--workspace)

(defvar mevedel-pipeline--active-tool-use-id nil
  "Tool-use id dynamically visible while a handler starts its work.")

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (fsm))
(defvar gptel-backend)

;; `mevedel-tool-fs'
(declare-function mevedel--snapshot-file-if-needed "mevedel-tool-fs" (filepath))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry" (span &rest props))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))
(declare-function mevedel-telemetry-start
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-spans
                  "mevedel-transcript-audit" (text &optional type))

;; `mevedel-tool-media'
(declare-function mevedel-tool-media-add-to-provider-result
                  "mevedel-tool-media" (backend parsed media-by-index))
(declare-function mevedel-tool-media-attach-result
                  "mevedel-tool-media"
                  (result media tool-results-dir tool-use-id))
(declare-function mevedel-tool-media-extract
                  "mevedel-tool-media"
                  (result-string &optional tool-results-dir expected-tool-use-id
                                 allow-payload-tool-use-id))
(declare-function mevedel-tool-media-prepare-tool-result
                  "mevedel-tool-media"
                  (backend tool-call tool-results-dir))
(declare-function mevedel-tool-media-result-for-hooks
                  "mevedel-tool-media" (result))
(declare-function mevedel-tool-media-strip-blocks
                  "mevedel-tool-media" (string))

;; `subr'
(defvar read-eval)

;; `mevedel-tool-repair-diagnostics'
(declare-function mevedel-tool-repair-audit-record
                  "mevedel-tool-repair-diagnostics" (state repairs))

;; `mevedel-specialist-nudges'
(declare-function mevedel-specialist-nudges-apply
                  "mevedel-specialist-nudges" (context))

;; `mevedel-tool-ui'
(require 'mevedel-permission-queue)

;; `mevedel-tools'
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--ctx-record-used "mevedel-tools" (ctx name))
(defvar mevedel-tools--current-fsm)

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

(defun mevedel-pipeline--tool-results-dir (session buffer)
  "Return SESSION's tool-results directory, materializing when possible.

When SESSION has no save path yet, use
`mevedel-session-persistence--shallow-ensure-files' with BUFFER so
oversized tool output produced during the first turn can still be
owned by the session.  Returns nil when there is no session or shallow
materialization fails."
  (when session
    (let ((save-path (or (mevedel-session-save-path session)
                         (when (and buffer (buffer-live-p buffer))
                           (require 'mevedel-session-persistence)
                           (mevedel-session-persistence--shallow-ensure-files
                            session buffer)))))
      (when save-path
        (require 'mevedel-workspace)
        (mevedel-workspace-ensure-generated-state-ignored
         (mevedel-session-workspace session))
        (file-name-concat save-path "tool-results")))))

(defun mevedel-pipeline--persist-result (result tool session &optional buffer)
  "Save RESULT to disk and return a preview string.

TOOL is the `mevedel-tool' whose result exceeded its size limit.
SESSION owns the output file through its `tool-results/' directory.
BUFFER is the chat data buffer used to shallowly materialize SESSION
when it has not been saved yet.  If no session-owned directory is
available, falls back to `mevedel-pipeline--truncate-result'."
  (setq result (mevedel--normalize-message-text result))
  (if-let* ((dir (mevedel-pipeline--tool-results-dir session buffer)))
      (let* ((name (mevedel-tool-name tool))
             (_ (make-directory dir t))
             (file (make-temp-file (file-name-concat dir (concat name "-"))
                                   nil ".txt"))
             (preview (mevedel-pipeline--head-tail-preview result)))
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region result nil file nil 'silent))
        (concat "<persisted-output>\n"
                (format "Output too large (%d chars). Full output saved to: %s\n\n"
                        (length result) file)
                "Preview (head and tail):\n"
                preview "\n"
                "</persisted-output>"))
    (mevedel-pipeline--truncate-result result tool)))

(defun mevedel-pipeline--truncate-result (result tool)
  "Truncate RESULT to a preview without persisting to disk.

Used when the result exceeds the size limit but no session-owned
persistence directory is available.  TOOL is used only for the tool
name in the message."
  (concat (format "Output too large (%d chars) and no session persistence \
directory available to persist full result (tool: %s).\n\n"
                  (length result) (mevedel-tool-name tool))
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

(defun mevedel-pipeline--format-context-failure (context reason)
  "Return a failure for REASON with CONTEXT's accumulated audit metadata."
  (let ((failure (mevedel-pipeline--format-failure reason)))
    (when-let* ((repairs (plist-get context :input-repairs)))
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
    (condition-case nil
        (let* ((plain-reason (and (stringp reason)
                                  (substring-no-properties reason)))
               (records
                (cl-remove-if
                 (lambda (record)
                   (and plain-reason
                        (string-search
                         (substring-no-properties
                          (mevedel--format-hook-audit-record record))
                         plain-reason)))
                 (plist-get context :hook-audit-records))))
          (mevedel-pipeline--append-hook-audit-records failure records))
      (error
       (ignore-errors
         (display-warning 'mevedel "Pipeline audit formatting failed" :warning))
       failure))))

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

(defun mevedel-pipeline--run (steps callback context)
  "Run the pipeline, calling CALLBACK with the result.

STEPS is a list of step functions.  Each step takes (CONTEXT NEXT FAIL)
where CONTEXT is a plist of accumulated state, NEXT is a continuation
taking an updated context plist, and FAIL is a continuation taking a
reason string.

Sync steps may call NEXT or FAIL directly, or `signal' a
`mevedel-pipeline-error' subclass.  Async steps defer and call NEXT or
FAIL later (e.g., after a user prompt resolves); once a step has
scheduled an async continuation it must not signal -- the outer
`condition-case' has already unwound.

Each step's NEXT and FAIL continuations are wrapped in a per-step
**latch**: the first call settles the step; every later call is a no-op
logged via `display-warning'.  This is defense-in-depth -- primitives
may latch at the UI layer too -- but the runner latch is authoritative.

CALLBACK must be the once-fire wrapper installed by
`mevedel-pipeline-run-tool'.  The runner's `condition-case' branches
fire CALLBACK directly with an `Error: ...' string when a sync error
escapes the step body or its NEXT recursion -- the wrapper guarantees
the consumer sees exactly one outcome even when the recursion already
delivered a result before signaling.  Routing through the per-step
latch instead would deadlock here, since the latch correctly suppresses
a second outcome on a step that already fired NEXT.

CONTEXT is the initial plist; the `:result' key holds the value passed
to CALLBACK."
  (if (null steps)
      (mevedel-pipeline--with-context-default-directory
       context
       (lambda ()
         (funcall callback (plist-get context :result))))
    (let* ((step (car steps))
           (rest (cdr steps))
           (step-name (mevedel-pipeline--step-name step))
           (settled nil)
           (telemetry-settled nil)
           (telemetry-span
            (when (and (plist-get context :session)
                       (fboundp 'mevedel-telemetry-start))
              (mevedel-telemetry-start
               (plist-get context :session) 'tool-pipeline-step
               :tool-name (mevedel-tool-name (plist-get context :tool))
               :tool-use-id (plist-get context :tool-use-id)
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
                    (display-warning
                     'mevedel
                     (format "Pipeline step %s called %s after already %s; \
ignoring duplicate outcome"
                             step-name which settled)
                     :warning)
                    nil)
                (setq settled which)
                t)))
           (next-cont
            (lambda (updated-ctx)
              (when (funcall try-settle 'next)
                (funcall finish-telemetry 'next)
                (mevedel-pipeline--run rest callback updated-ctx))))
           (fail-cont
            (lambda (reason)
              (when (funcall try-settle 'fail)
                (funcall finish-telemetry 'fail)
                (mevedel-pipeline--with-context-default-directory
                 context
                 (lambda ()
                   (funcall callback
                            (mevedel-pipeline--format-context-failure
                             context reason))))))))
      (condition-case err
          (mevedel-pipeline--with-context-default-directory
           context
           (lambda ()
             (funcall step context next-cont fail-cont)))
        (mevedel-validation-error
         (funcall finish-telemetry 'error 'validation)
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-context-failure
                      context (or (cadr err) "Validation error"))))))
        (mevedel-permission-denied
         (funcall finish-telemetry 'error 'permission-denied)
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-context-failure
                      context
                      (if (cadr err)
                          (format "Permission denied: %s" (cadr err))
                        "Permission denied"))))))
        (mevedel-pipeline-error
         (funcall finish-telemetry 'error 'pipeline)
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-context-failure
                      context (or (cadr err) "Pipeline error"))))))
        (error
         (funcall finish-telemetry 'error (car-safe err))
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-context-failure
                      context (error-message-string err))))))))))


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

(defun mevedel-pipeline--record-hook-context (context decision &optional event)
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

(defun mevedel-pipeline--record-hook-audit (context records)
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

(defun mevedel-pipeline--append-hook-side-channel (text context)
  "Append accumulated hook context and audit records from CONTEXT to TEXT."
  (mevedel-pipeline--append-hook-audit-records
   (mevedel-pipeline--append-hook-context-string text context)
   (plist-get context :hook-audit-records)))

(defun mevedel-pipeline--hook-context-audit-records (decision event)
  "Return audit records for DECISION additional context at EVENT."
  (mevedel-hooks-context-audit-records decision event 'tool-context))

(defun mevedel-pipeline--hook-permission-audit-record
    (event outcome decision &optional reason)
  "Return a permission audit record for hook EVENT and OUTCOME."
  (append
   (list :type 'tool-permission
         :event (mevedel-hooks--event-display-name event)
         :outcome (format "%s" outcome))
   (when-let* ((reason (or reason
                           (mevedel-hooks-decision-reason decision))))
     (list :reason reason))))

(defun mevedel-pipeline--hook-input-rewrite-audit-record
    (event original updated decision)
  "Return a tool input rewrite audit record for hook EVENT."
  (append
   (list :type 'tool-input-rewrite
         :event (mevedel-hooks--event-display-name event)
         :original-input original
         :updated-input updated)
   (when-let* ((reason (mevedel-hooks-decision-reason decision)))
     (list :reason reason))))

(defun mevedel-pipeline--hook-result-rewrite-audit-record
    (event original updated decision)
  "Return a result rewrite audit record for hook EVENT."
  (append
   (list :type 'tool-result-rewrite
         :event (mevedel-hooks--event-display-name event)
         :original-result (or original "")
         :updated-result (or updated ""))
   (when-let* ((reason (mevedel-hooks-decision-reason decision)))
     (list :reason reason))))

(defun mevedel-pipeline--run-hook-event
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

(defun mevedel-pipeline--permission-origin (context &optional explicit-origin)
  "Return the canonical agent path for permission CONTEXT.
EXPLICIT-ORIGIN takes precedence when non-nil."
  (or explicit-origin
      (plist-get context :origin)
      (and-let* ((request (plist-get context :request))
                 ((mevedel-request-p request)))
        (mevedel-request-origin request))
      (and-let* ((inv (plist-get context :invocation))
                 ((fboundp 'mevedel-agent-invocation-p))
                 ((mevedel-agent-invocation-p inv)))
        (mevedel-agent-invocation-path inv))
      "/root"))

(defun mevedel-pipeline--permission-sanitized-pattern (tool-name pattern)
  "Return log-safe PATTERN metadata for TOOL-NAME."
  (cond
   ((and (equal tool-name "Bash") pattern)
    (if (fboundp 'mevedel-tool-exec--bash-decision-specifier-value)
        (mevedel-tool-exec--bash-decision-specifier-value pattern)
      "shell command"))
   (t pattern)))

(defun mevedel-pipeline--permission-specifier-props (context)
  "Return sanitized permission specifier properties from CONTEXT."
  (let* ((tool (plist-get context :tool))
         (tool-name (and tool (mevedel-tool-name tool)))
         (args (plist-get context :args))
         (path (when-let* ((fn (and tool (mevedel-tool-get-path tool))))
                 (ignore-errors (funcall fn args))))
         (raw-pattern (when-let* ((fn (and tool (mevedel-tool-get-pattern tool))))
                        (ignore-errors (funcall fn args))))
         (pattern (mevedel-pipeline--permission-sanitized-pattern
                   tool-name raw-pattern))
         (domain (when-let* ((fn (and tool (mevedel-tool-get-domain tool))))
                   (ignore-errors (funcall fn args))))
         (name (when-let* ((fn (and tool (mevedel-tool-get-name tool))))
                 (ignore-errors (funcall fn args))))
         (key (cond (pattern :pattern)
                    (domain :domain)
                    (name :name)
                    (path :path)))
         (value (or pattern domain name path)))
    (append (and key (list :specifier-key key))
            (and value (list :specifier-value value))
            (and path
                 (list :protected-path
                       (mevedel-permission--path-protected-p path))))))

(defun mevedel-pipeline--log-permission-decision
    (context decision &rest props)
  "Persist sanitized DECISION diagnostics for CONTEXT with PROPS."
  (let ((session (plist-get context :session)))
    (when (and session
               (not (plist-get decision :logged)))
      (let* ((tool (plist-get context :tool))
             (tool-name (and tool (mevedel-tool-name tool)))
             (mode (or (and session (mevedel-session-permission-mode session))
                       mevedel-permission-mode))
             (raw (mevedel-permission-decision-raw-outcome decision))
             (outcome (or (plist-get decision :outcome)
                          (mevedel-permission--normalize-outcome raw))))
        (apply #'mevedel-permission-log
               session 'permission-decision
               (append
                (list :tool-name tool-name
                      :origin (mevedel-pipeline--permission-origin context)
                      :mode mode
                      :outcome outcome
                      :via (plist-get decision :via))
                (mevedel-pipeline--permission-specifier-props context)
                (when (plist-member decision :bucket)
                  (list :bucket (plist-get decision :bucket)))
                props))))))

(defun mevedel-pipeline--permission-decision-with-via
    (decision via &rest props)
  "Return DECISION metadata adjusted to VIA and PROPS."
  (let ((raw (mevedel-permission-decision-raw-outcome decision)))
    (append (list :outcome (mevedel-permission--normalize-outcome raw)
                  :raw-outcome raw
                  :via via)
            (when (plist-member decision :bucket)
              (list :bucket (plist-get decision :bucket)))
            props)))

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
    (mevedel-pipeline--run-hook-event
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
                 (mevedel-pipeline--record-hook-audit
                  (mevedel-pipeline--record-hook-context
                   context decision 'PreToolUse)
                  (append
                   (list
                    (mevedel-pipeline--hook-permission-audit-record
                     'PreToolUse 'deny decision reason))
                    (mevedel-pipeline--hook-context-audit-records
                    decision 'PreToolUse)))))
             (mevedel-pipeline--log-permission-decision
              context
              (list :outcome 'deny
                    :raw-outcome `(deny . ,reason)
                    :via 'pre-tool-hook))
             (mevedel-pipeline--fail-permission-denied
              updated fail
              (if stopped-p reason (format "Permission denied: %s" reason))
              reason 'PreToolUse)))
          (t
           (let ((updated (mevedel-pipeline--record-hook-context
                           context decision 'PreToolUse)))
             (setq updated
                   (mevedel-pipeline--record-hook-audit
                    updated
                    (mevedel-pipeline--hook-context-audit-records
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
                       (funcall fail err)
                     (funcall next
                              (plist-put
                               (mevedel-pipeline--record-hook-audit
                                updated
                                (mevedel-pipeline--hook-input-rewrite-audit-record
                                 'PreToolUse
                                 (plist-get context :args)
                                 args
                                 decision))
                               :args args))))
               (funcall next updated)))))))
     context session workspace request invocation)))

(defun mevedel-pipeline--apply-hook-permission-decision (outcome context)
  "Apply CONTEXT's hook permission decision to permission OUTCOME.

Hook `deny' always wins.  Hook `ask' can tighten an `allow' into a
prompt.  Hook `allow' is applied at the `PermissionRequest' boundary
when the normal resolver returns `ask'; explicit denials stay intact."
  (let ((decision (plist-get context :hook-permission-decision)))
    (pcase decision
      ('deny 'deny)
      ('ask
       (if (memq outcome '(allow approve implement implement-clear))
           'ask
         outcome))
      (_ outcome))))

(defun mevedel-pipeline--fail-permission-denied
    (context fail reason &optional model-reason provenance)
  "Run `PermissionDenied' hooks for CONTEXT, then call FAIL with REASON.

MODEL-REASON and PROVENANCE are included in the hook event when available."
  (let ((session (plist-get context :session))
        (workspace (plist-get context :workspace)))
    (mevedel-pipeline--run-hook-event
     'PermissionDenied
     (mevedel-hooks-tool-event-plist
      'PermissionDenied context
      :permission-reason (or model-reason reason)
      :permission-provenance provenance)
     (lambda (decision)
       (let* ((updated (mevedel-pipeline--record-hook-context
                        context decision 'PermissionDenied))
              (updated
               (mevedel-pipeline--record-hook-audit
                updated
                (mevedel-pipeline--hook-context-audit-records
                 decision 'PermissionDenied)))
              (final-reason
               (or (plist-get decision :permission-reason)
                   reason)))
         (funcall fail
                  (mevedel-pipeline--append-hook-side-channel
                   final-reason updated))))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))

(defun mevedel-pipeline--permission-denial-outcome-p (outcome)
  "Return non-nil when OUTCOME is an interactive denial."
  (or (memq outcome '(deny deny-once deny-session))
      (and (consp outcome) (memq (car outcome) '(deny feedback)))))

(defun mevedel-pipeline--permission-denial-provenance (context decision)
  "Return the original denial source from CONTEXT or DECISION."
  (or (plist-get context :permission-denial-provenance)
      (plist-get decision :via)
      'policy))

(defun mevedel-pipeline--request-permission
    (context entry session settle &optional ask-decision fallback-outcome)
  "Run `PermissionRequest' for ENTRY, then call SETTLE or enqueue.
SETTLE receives the updated CONTEXT and the permission outcome.
ASK-DECISION is logged only when hooks leave queue admission unresolved.
FALLBACK-OUTCOME settles an unresolved request without queue admission."
  (let ((workspace (plist-get context :workspace)))
    (mevedel-pipeline--run-hook-event
     'PermissionRequest
     (mevedel-hooks-tool-event-plist
      'PermissionRequest context
      :specifier-key (plist-get entry :specifier-key)
      :specifier-value (plist-get entry :specifier-value))
     (lambda (decision)
       (let* ((updated
               (mevedel-pipeline--record-hook-context
                context decision 'PermissionRequest))
              (updated
               (mevedel-pipeline--record-hook-audit
                updated
                (mevedel-pipeline--hook-context-audit-records
                 decision 'PermissionRequest)))
              (stop-p (and (plist-member decision :continue)
                           (not (plist-get decision :continue))))
              (permission-decision
               (plist-get decision :permission-decision)))
         (cond
          ((or stop-p (eq permission-decision 'deny))
           (let* ((detail
                   (or (and stop-p (plist-get decision :stop-reason))
                       (plist-get decision :permission-reason)
                       "hook denied permission"))
                  (reason (format "blocked by PermissionRequest: %s" detail)))
             (setq updated
                   (mevedel-pipeline--record-hook-audit
                    updated
                    (mevedel-pipeline--hook-permission-audit-record
                     'PermissionRequest 'deny decision reason)))
             (mevedel-pipeline--log-permission-decision
              updated
              (list :outcome 'deny
                    :raw-outcome `(deny . ,reason)
                    :via 'permission-request-hook))
             (funcall settle
                      (plist-put updated :permission-denial-provenance
                                 'PermissionRequest)
                      `(deny . ,reason))))
          ((eq permission-decision 'allow)
           (setq updated
                 (mevedel-pipeline--record-hook-audit
                  updated
                  (mevedel-pipeline--hook-permission-audit-record
                   'PermissionRequest 'allow decision)))
           (mevedel-pipeline--log-permission-decision
            updated
            (list :outcome 'allow :raw-outcome 'allow
                  :via 'permission-request-hook))
           (funcall settle updated 'allow))
          (t
           (if (and fallback-outcome (null permission-decision))
               (funcall settle updated fallback-outcome)
             (when ask-decision
               (mevedel-pipeline--log-permission-decision
                updated ask-decision))
             (let ((queued (copy-sequence entry)))
               (setq queued
                     (plist-put
                      queued :callback
                      (lambda (outcome)
                        (funcall
                         settle
                         (if (mevedel-pipeline--permission-denial-outcome-p
                              outcome)
                             (plist-put updated :permission-denial-provenance
                                        'user)
                           updated)
                         outcome))))
               (mevedel-permission--enqueue queued session)))))))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))

(defun mevedel-pipeline--step-permission (context next fail)
  "Check permission for the tool invocation.

Reads session / workspace from CONTEXT (captured at
`mevedel-pipeline-run-tool' entry) so that an async continuation
firing from another buffer still sees the correct session state.

Invokes `mevedel-check-permission-async' for the shared decision chain.
When the chain (or a tool slot) yields `ask', the step
drives the generic async prompt and applies the result through
`mevedel-permission--apply-prompt-result' so session / persistent
rule and resource-grant storage is honored.  A missing filesystem
boundary creates exact read or write authority without broadening an
allowed root.

Dispatches the final outcome through NEXT (allow-equivalent
outcomes) or FAIL (all denial shapes, plus `aborted')."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (tool-name (mevedel-tool-name tool))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace))
         (request (plist-get context :request))
         (invocation (plist-get context :invocation))
         (permission-context
          (mevedel-permission--invocation-context
           :tool tool
           :args args
           :session session
           :workspace workspace
           :request request
           :invocation invocation
           :buffer (plist-get context :buffer)
           :permission-request
           (lambda (entry queue-session settle)
             (mevedel-pipeline--request-permission
              context entry (or queue-session session)
              (lambda (updated outcome)
                (setq context updated)
                (funcall settle outcome))
              nil
              (and (eq 'allow (plist-get context :hook-permission-decision))
                   'allow)))
           :warn-no-session-p t))
         (path (plist-get permission-context :path))
         (workspace-root (plist-get permission-context :workspace-root))
         (allowed-roots (plist-get permission-context :allowed-roots)))
    (apply #'mevedel-check-permission-async-with-metadata
     tool-name
     (lambda (decision)
       (let* ((raw-outcome (mevedel-permission-decision-raw-outcome decision))
              (hooked-outcome
               (mevedel-pipeline--apply-hook-permission-decision
                raw-outcome context))
              (context
               (if (eq hooked-outcome raw-outcome)
                   context
                 (mevedel-pipeline--record-hook-audit
                  context
                  (mevedel-pipeline--hook-permission-audit-record
                   'PreToolUse hooked-outcome
                   (plist-get context :hook-permission-hook-decision)))))
              (logged-decision
               (if (eq hooked-outcome raw-outcome)
                   decision
                 (mevedel-pipeline--permission-decision-with-via
                  (plist-put (copy-sequence decision)
                             :raw-outcome hooked-outcome)
                  'pre-tool-hook))))
         (mevedel-pipeline--dispatch-permission-outcome
          hooked-outcome context next fail
          :tool-name tool-name :path path :session session
          :workspace workspace :workspace-root workspace-root
          :allowed-roots allowed-roots
          :decision logged-decision
          :permission-context permission-context)))
     (mevedel-permission--checker-args permission-context))))

(cl-defun mevedel-pipeline--dispatch-permission-outcome
    (outcome context next fail
             &key tool-name path session workspace workspace-root allowed-roots
             decision permission-context)
  "Translate permission OUTCOME for CONTEXT into NEXT or FAIL.

OUTCOME is the union of (a) results emitted by a permission slot via
`cont' (`allow', `deny', `(deny . REASON)', `(feedback . TEXT)',
`aborted', `ask') and (b) results emitted by the generic async prompt
overlay after an `ask' is routed through it (`allow-once',
`allow-session', `always-allow', `deny-once', `deny-session',
`aborted').

`ask' routes through the standard prompt path and recurses with the
user’s UI choice.  Rule-scope outcomes (`allow-session' etc.) are
pre-collapsed via `mevedel-permission--apply-prompt-result' so that
session / persistent rules land with the correct scope before the
translator fires NEXT / FAIL.

TOOL-NAME, PATH, SESSION, WORKSPACE, WORKSPACE-ROOT, ALLOWED-ROOTS,
DECISION, and PERMISSION-CONTEXT describe the permission context."
  (when (and decision (not (eq outcome 'ask)))
    (mevedel-pipeline--log-permission-decision context decision))
  (pcase outcome
    ;; `ask' arrives from the decision chain itself (steps 3/7/8/9) or
    ;; from a tool slot that defers to the generic prompt.  Drive the
    ;; prompt with workspace-boundary rule shaping identical to the
    ;; sync pipeline's.
    ('ask
     (let* ((args (plist-get context :args))
            (tool (plist-get context :tool))
            (permission-context
             (or permission-context
                 (mevedel-permission--invocation-context
                  :tool tool
                  :args args
                  :session session
                  :workspace workspace
                  :request (plist-get context :request)
                  :invocation (plist-get context :invocation)
                  :buffer (plist-get context :buffer)
                  :path path
                  :workspace-root workspace-root
                  :allowed-roots allowed-roots)))
            (rule-tool (plist-get permission-context :rule-tool))
            (rule-key (plist-get permission-context :rule-key))
            (rule-value (plist-get permission-context :rule-value))
            (decision-metadata decision)
            (resource-decision-p
             (memq (plist-get decision-metadata :via)
                   '(protected-path workspace-boundary)))
            (resource-access
             (and resource-decision-p
                  (plist-get permission-context :resource-access))))
       (mevedel-pipeline--request-permission
        context
        (list :kind 'generic
              :tool-name tool-name
              :args args
              :specifier-key rule-key
              :specifier-value rule-value
              :protected-path
              (eq (plist-get decision-metadata :via) 'protected-path)
              :resource-access resource-access
              :include-always
              (plist-get permission-context :include-always)
              :workspace workspace
              :origin (mevedel-pipeline--permission-origin context))
        session
        (lambda (prompt-context prompt-outcome)
          (condition-case err
              (let ((collapsed
                     (pcase prompt-outcome
                       ((or 'allow-once 'allow-session 'always-allow
                            'deny-once 'deny-session)
                        (mevedel-permission--apply-prompt-result
                         prompt-outcome rule-tool session workspace
                         (and (eq rule-key :path) rule-value)
                         :spec-key rule-key
                         :spec-value rule-value
                         :resource-access resource-access))
                       ((or 'allow 'deny 'aborted) prompt-outcome)
                       (other other))))
                (mevedel-pipeline--dispatch-permission-outcome
                 collapsed prompt-context next fail
                 :tool-name tool-name :path path :session session
                 :workspace workspace :workspace-root workspace-root
                 :allowed-roots allowed-roots
                 :permission-context permission-context))
            (error
             (funcall fail (error-message-string err)))))
        decision-metadata
        (and (eq 'allow (plist-get context :hook-permission-decision))
             'allow))))
    ((or 'allow 'approve 'implement 'implement-clear)
     (funcall next context))
    ('deny
     (mevedel-pipeline--fail-permission-denied
      context fail "Permission denied" nil
      (mevedel-pipeline--permission-denial-provenance context decision)))
    (`(deny . ,reason)
     (mevedel-pipeline--fail-permission-denied
      context fail (format "Permission denied: %s" reason) reason
      (mevedel-pipeline--permission-denial-provenance context decision)))
    (`(feedback . ,text)
     (mevedel-pipeline--fail-permission-denied
      context fail (format "Permission denied: %s" text) text
      (mevedel-pipeline--permission-denial-provenance context decision)))
    ('aborted
     (funcall fail "aborted"))
    ;; Defense in depth: an unrecognized outcome (slot bug, primitive
    ;; returning an unexpected symbol) fails loudly rather than
    ;; stranding the FSM with neither `next' nor `fail' fired.
    (_ (funcall fail (format "Unexpected permission outcome: %S"
                             outcome)))))

(defun mevedel-pipeline--step-snapshot (context next _fail)
  "Snapshot files before modification.

Extracts the path from tool args via the tool's get-path function and
snapshots it.  Only included for non-read-only tools.  CONTEXT must
contain `:tool' and `:args'.  NEXT is called on success.  FAIL is
unused -- a snapshot failure is best-effort and should never fail the
pipeline."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (get-path-fn (mevedel-tool-get-path tool)))
    (when get-path-fn
      (when-let* ((path (ignore-errors (funcall get-path-fn args))))
        (mevedel--snapshot-file-if-needed path)))
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
                        (updated context))
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
                   (plist-get context :tool-use-id)))
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


(defconst mevedel-pipeline--render-data-open "<!-- mevedel-render-data -->"
  "Opening delimiter marking a hidden render-data side-channel block.
Emitted inside tool results so the view buffer interpreter can extract
the serialized render-data without re-running the tool.")

(defconst mevedel-pipeline--render-data-close "<!-- /mevedel-render-data -->"
  "Closing delimiter marking the end of a render-data side-channel block.")

(defun mevedel-pipeline--plain-render-data (value)
  "Return VALUE with text properties stripped from all contained strings."
  (cond
   ((stringp value)
    (mevedel--normalize-message-text (substring-no-properties value)))
   ((consp value)
    (cons (mevedel-pipeline--plain-render-data (car value))
          (mevedel-pipeline--plain-render-data (cdr value))))
   ((vectorp value)
    (apply #'vector
           (mapcar #'mevedel-pipeline--plain-render-data value)))
   (t value)))

(defconst mevedel-pipeline--render-transform-max-data-size 8192
  "Maximum printed size of render-data produced by `:render-transform'.

Handler-provided render-data is not capped here because some existing
handlers intentionally carry larger structured payloads, such as edit
diffs.  Transform functions are for bounded metadata derived from a
string result, not for copying the result body into a hidden side
channel.")

(defun mevedel-pipeline--render-transform-data-size (data)
  "Return the printed size of DATA after stripping text properties."
  (length (prin1-to-string (mevedel-pipeline--plain-render-data data))))

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
             ((> (mevedel-pipeline--render-transform-data-size render-data)
                 mevedel-pipeline--render-transform-max-data-size)
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

(defun mevedel-pipeline--format-render-data-block (render-data)
  "Return the serialized side-channel block string for RENDER-DATA.
The returned string is propertized `invisible' = t so the data buffer
hides it as a best-effort display courtesy.

The block survives verbatim into the chat buffer (which feeds the view
parser and persistence).  An `:around' advice on
`gptel--parse-tool-results' -- installed by
`mevedel-pipeline-install-tool-result-scrubber' -- strips the block at
the single chokepoint where tool result strings become the LLM-bound
API message, without touching the callback that drives chat-buffer
display."
  (propertize
   (concat "\n" mevedel-pipeline--render-data-open "\n"
           (let ((print-level nil)
                 (print-length nil)
                 (print-circle t))
             (prin1-to-string
              (mevedel-pipeline--plain-render-data render-data)))
           "\n" mevedel-pipeline--render-data-close "\n")
   'invisible t))

(defun mevedel-pipeline--read-render-data-payload (payload)
  "Read one render-data plist from PAYLOAD or return a failure sentinel."
  (condition-case nil
      (let* ((read-eval nil)
             (parsed (read-from-string payload))
             (data (car parsed))
             (end (cdr parsed))
             (rest (substring payload end)))
        (if (and (consp data)
                 (string-blank-p rest)
                 (proper-list-p data)
                 (zerop (% (length data) 2))
                 (cl-loop for tail on data by #'cddr
                          always (keywordp (car tail))))
            data
          :mevedel-parse-failed))
    (error :mevedel-parse-failed)))

(defun mevedel-pipeline--render-data-blocks (string)
  "Return valid render-data blocks found in STRING, oldest first.
Each entry is `(BEGIN END DATA)'.  Invalid marker-looking text is skipped.
BEGIN and END include the formatter's optional surrounding newlines."
  (when (stringp string)
    (let ((search-start 0)
          blocks open)
      (while (setq open
                   (string-search mevedel-pipeline--render-data-open
                                  string search-start))
        (let* ((payload-start
                (+ open (length mevedel-pipeline--render-data-open)))
               (close
                (string-search mevedel-pipeline--render-data-close
                               string payload-start)))
          (if (not close)
              (setq search-start payload-start)
            (let* ((payload
                    (string-trim
                     (substring string payload-start close)))
                   (data
                    (mevedel-pipeline--read-render-data-payload payload))
                   (next-open
                    (string-search mevedel-pipeline--render-data-open
                                   string payload-start))
                   (close-end
                    (+ close (length mevedel-pipeline--render-data-close))))
              (cond
               ((not (eq data :mevedel-parse-failed))
                (let ((begin
                       (if (and (> open 0)
                                (eq (aref string (1- open)) ?\n))
                           (1- open)
                         open))
                      (end
                       (if (and (< close-end (length string))
                                (eq (aref string close-end) ?\n))
                           (1+ close-end)
                         close-end)))
                  (push (list begin end data) blocks))
                (setq search-start close-end))
               ((and next-open (< next-open close))
                (setq search-start next-open))
               (t
                (setq search-start close-end)))))))
      (nreverse blocks))))

(defun mevedel-pipeline--strip-render-data-blocks (string)
  "Return STRING with every valid render-data side-channel block removed."
  (if-let* ((blocks (mevedel-pipeline--render-data-blocks string)))
      (let ((cursor 0)
            parts)
        (dolist (block blocks)
          (push (substring string cursor (car block)) parts)
          (setq cursor (cadr block)))
        (push (substring string cursor) parts)
        (apply #'concat (nreverse parts)))
    string))

(defun mevedel-pipeline--strip-side-channel-blocks (string)
  "Return STRING with mevedel side-channel blocks removed."
  (require 'mevedel-tool-media)
  (mevedel-tool-media-strip-blocks
   (mevedel-pipeline--strip-non-media-side-channel-blocks string)))

(defun mevedel-pipeline--strip-non-media-side-channel-blocks (string)
  "Return STRING with non-media mevedel side-channel blocks removed."
  (mevedel--strip-hook-audit-blocks
   (mevedel-pipeline--strip-render-data-blocks string)))

(defun mevedel-pipeline--find-render-data-block-by-agent-id (agent-id)
  "Return bounds of the first render-data block for AGENT-ID.

The return value is (BEG . END), or nil when no block has a matching
`:agent-id'.
Searches the current buffer from `point-min'.  Used by the
background handle patch path to locate the block whose hidden
plist should be updated when a sub-agent's status changes."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (found)
        (while (and (not found)
                    (search-forward mevedel-pipeline--render-data-open nil t))
          (let* ((open-beg (match-beginning 0))
                 (block-beg (if (and (> open-beg (point-min))
                                     (eq (char-before open-beg) ?\n))
                                (1- open-beg)
                              open-beg)))
            (if (not (search-forward mevedel-pipeline--render-data-close nil t))
                (goto-char (point-max))
              (let* ((close-end (match-end 0))
                     (block-end (if (and (< close-end (point-max))
                                         (eq (char-after close-end) ?\n))
                                    (1+ close-end)
                                  close-end))
                     (raw (buffer-substring-no-properties block-beg block-end))
                     (parsed (mevedel-pipeline-extract-render-data raw))
                     (plist (cdr parsed)))
                (when (and (listp plist)
                           (equal (plist-get plist :agent-id) agent-id))
                  (setq found (cons block-beg block-end)))))))
        found))))

(defun mevedel-pipeline--patch-render-data-block (beg end new-plist)
  "Replace the render-data block between BEG and END with NEW-PLIST.
Preserves the surrounding text and the hidden-block delimiters.
The new block is formatted via
`mevedel-pipeline--format-render-data-block' so it stays
round-trippable through `mevedel-pipeline-extract-render-data'.

Inherits the `gptel' text property of the surrounding text onto the
inserted block.  Without this inheritance, the block becomes a hole in
the gptel-property run that delimits the tool segment; the view
buffer's `extract-segments' then splits the tool segment in two and
the LLM-invisible block leaks into the visible body of the tool
result."
  (save-excursion
    (save-restriction
      (widen)
      (let ((surrounding-gptel
             (or (and (> beg (point-min))
                      (get-text-property (1- beg) 'gptel))
                 (and (< end (point-max))
                      (get-text-property end 'gptel)))))
        (goto-char beg)
        (delete-region beg end)
        (let ((block (mevedel-pipeline--format-render-data-block new-plist)))
          (when surrounding-gptel
            (setq block (propertize block 'gptel surrounding-gptel)))
          (insert block))))))

(defun mevedel-pipeline--tool-segment-bounds (tool-use-id)
  "Return current-buffer bounds carrying TOOL-USE-ID, or nil."
  (let ((target (cons 'tool tool-use-id))
        (position (point-min))
        bounds)
    (while (and (< position (point-max)) (not bounds))
      (let* ((value (get-text-property position 'gptel))
             (next (or (next-single-property-change
                        position 'gptel nil (point-max))
                       (point-max))))
        (when (equal value target)
          (let ((end next))
            (while (and (< end (point-max))
                        (equal (get-text-property end 'gptel) target))
              (setq end (or (next-single-property-change
                             end 'gptel nil (point-max))
                            (point-max))))
            (setq bounds (cons position end))))
        (setq position next)))
    bounds))

(defun mevedel-pipeline--render-data-block-bounds (beg end)
  "Return render-data block bounds inside BEG..END, or nil."
  (save-excursion
    (goto-char beg)
    (when (search-forward mevedel-pipeline--render-data-open end t)
      (let* ((open-beg (match-beginning 0))
             (block-beg (if (and (> open-beg beg)
                                 (eq (char-before open-beg) ?\n))
                            (1- open-beg)
                          open-beg)))
        (when (search-forward mevedel-pipeline--render-data-close end t)
          (let ((close-end (match-end 0)))
            (cons block-beg
                  (if (and (< close-end end)
                           (eq (char-after close-end) ?\n))
                      (1+ close-end)
                    close-end))))))))

(defun mevedel-pipeline--next-tool-segment-start (position tool-use-id)
  "Return the first different tool segment after POSITION.
TOOL-USE-ID identifies property runs that still belong to the current tool."
  (let ((cursor position)
        found)
    (while (and (< cursor (point-max)) (not found))
      (let ((property (get-text-property cursor 'gptel)))
        (when (and (consp property)
                   (eq (car property) 'tool)
                   (not (equal (cdr property) tool-use-id)))
          (setq found cursor)))
      (unless found
        (setq cursor
              (or (next-single-property-change
                   cursor 'gptel nil (point-max))
                  (point-max)))))
    found))

(defun mevedel-pipeline--plist-merge (base updates)
  "Return a copy of BASE with each key in UPDATES replaced."
  (let ((merged (copy-tree base)))
    (while updates
      (setq merged (plist-put merged (pop updates) (pop updates))))
    merged))

(defun mevedel-pipeline-update-tool-render-data
    (buffer tool-use-id updates)
  "Merge UPDATES into TOOL-USE-ID's hidden render data in BUFFER.

Return non-nil when the authoritative tool segment was found and updated.
The side channel remains inside the segment's `gptel' property run, so it is
persisted with the transcript while the provider scrubber keeps it model-hidden."
  (when (and (buffer-live-p buffer)
             (stringp tool-use-id)
             (listp updates))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (when-let* ((segment
                     (mevedel-pipeline--tool-segment-bounds tool-use-id)))
          (let* ((beg (car segment))
                 (end (cdr segment))
                 (search-end
                  (or (mevedel-pipeline--next-tool-segment-start
                       end tool-use-id)
                      (point-max)))
                 (block
                  (mevedel-pipeline--render-data-block-bounds
                   beg search-end))
                 (existing
                  (and block
                       (cdr
                        (mevedel-pipeline-extract-render-data
                         (buffer-substring-no-properties
                          (car block) (cdr block))))))
                 (render-data
                  (mevedel-pipeline--plist-merge existing updates))
                 (inhibit-modification-hooks t)
                 (inhibit-read-only t))
            (if block
                (mevedel-pipeline--patch-render-data-block
                 (car block) (cdr block) render-data)
              (goto-char end)
              (insert
               (propertize
                (mevedel-pipeline--format-render-data-block render-data)
                'gptel (get-text-property beg 'gptel))))
            t))))))

(defun mevedel-pipeline-tool-render-data (buffer tool-use-id)
  "Return TOOL-USE-ID's hidden render data in BUFFER, or nil."
  (when (and (buffer-live-p buffer) (stringp tool-use-id))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (when-let* ((segment
                     (mevedel-pipeline--tool-segment-bounds tool-use-id))
                    (block
                     (mevedel-pipeline--render-data-block-bounds
                      (car segment) (cdr segment))))
          (cdr
           (mevedel-pipeline-extract-render-data
            (buffer-substring-no-properties (car block) (cdr block)))))))))

(defun mevedel-pipeline-reconcile-lost-executions
    (buffer &optional successor-execution-ids)
  "Mark stale running Bash render records in BUFFER as lost.

This repairs transcript state after resume or fork.  It never attempts to
reattach an operating-system process.  Records named by
SUCCESSOR-EXECUTION-IDS are marked archived because a newer segment owns their
terminal truth.  Return the number of repaired records."
  (let (records archived-records)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-min))
            (while (search-forward mevedel-pipeline--render-data-open nil t)
              (let* ((open-begin (match-beginning 0))
                     (begin
                      (if (and (> open-begin (point-min))
                               (eq (char-before open-begin) ?\n))
                          (1- open-begin)
                        open-begin)))
                (when (search-forward mevedel-pipeline--render-data-close nil t)
                  (let* ((close-end (match-end 0))
                         (end
                          (if (and (< close-end (point-max))
                                   (eq (char-after close-end) ?\n))
                              (1+ close-end)
                            close-end))
                         (parsed
                          (mevedel-pipeline-extract-render-data
                           (buffer-substring-no-properties begin end)))
                         (data (cdr-safe parsed)))
                    (when (and data
                               (or (plist-get data :execution-id)
                                   (plist-get data :live-execution-p))
                               (or (eq (plist-get data :state) 'running)
                                   (plist-get data :live-execution-p)))
                      (push (list begin end data) records)))))))
          (dolist (record records)
            (pcase-let ((`(,begin ,end ,data) record))
              (mevedel-pipeline--patch-render-data-block
               begin end
               (mevedel-pipeline--plist-merge
                data
                (if (member (plist-get data :execution-id)
                            successor-execution-ids)
                    '(:state archived :status nil :live-execution-p nil
                      :termination compacted :outcome nil)
                  '(:state lost :status error :live-execution-p nil
                    :termination lost :outcome failure))))))
          (require 'mevedel-transcript-audit)
          (setq archived-records
                (cl-remove-if-not
                 (lambda (span)
                   (let* ((record (plist-get span :record))
                          (data (plist-get record :render-data)))
                     (and (or (eq (plist-get data :state) 'running)
                              (plist-get data :live-execution-p))
                          data)))
                 (mevedel-transcript-audit-spans
                  (buffer-substring-no-properties (point-min) (point-max))
                  'execution-archive)))
          (dolist (span (reverse archived-records))
            (let* ((record (plist-get span :record))
                   (data (plist-get record :render-data))
                   (successor-p
                    (member (plist-get data :execution-id)
                            successor-execution-ids))
                   (begin (+ (point-min) (plist-get span :start)))
                   (end (+ (point-min) (plist-get span :end))))
              (setq record
                    (plist-put
                     record :type 'execution-completion))
              (setq record
                    (plist-put
                     record :render-data
                     (mevedel-pipeline--plist-merge
                      data
                      (if successor-p
                          '(:state archived :status nil
                            :live-execution-p nil
                            :termination compacted :outcome nil)
                        '(:state lost :status error
                          :live-execution-p nil
                          :termination lost :outcome failure)))))
              (delete-region begin end)
              (goto-char begin)
              (insert (mevedel--format-hook-audit-record record))))))
    (+ (length records) (length archived-records)))))

(defun mevedel--parse-tool-results-scrub-advice (orig-fun backend tool-use)
  "Strip render-data blocks from TOOL-USE results for BACKEND via ORIG-FUN.

Wraps `gptel--parse-tool-results' (a `cl-defgeneric' with per-backend
methods in gptel-openai.el, gptel-anthropic.el, ...) which is the sole
point at which `:result' strings are copied into the API-shaped
tool_result message.  Both request paths funnel through it:

- Tool-follow-up requests (`gptel--handle-tool-result' ->
  `gptel--parse-tool-results' -> `gptel--inject-prompt').
- User-initiated requests that re-parse the chat buffer
  (`gptel--parse-buffer' calls `gptel--parse-tool-results' on each
  stored tool-call region).

The advice temporarily substitutes cleaned strings into the `:result'
slot of each tool-call plist, calls ORIG-FUN so the backend method
builds its message from the scrubbed values, then restores the original
`:result' values.  Everything downstream that consumes `:tool-use' or
`:tool-result' for display (the gptel callback feeding the chat buffer,
the view parser, persistence) keeps seeing the full block."
  (require 'mevedel-tool-media)
  (let ((saved nil))
    (unwind-protect
        (let ((media-by-index nil)
              (tool-results-dir
               (when-let* ((session (bound-and-true-p mevedel--session))
                           (save-path (mevedel-session-save-path session)))
                 (file-name-concat save-path "tool-results"))))
          (dolist (tc tool-use)
            (let* ((orig (plist-get tc :result))
                   (clean-tc (copy-sequence tc))
                   (prepared
                    (progn
                      (when (stringp orig)
                        (plist-put
                         clean-tc :result
                         (mevedel-pipeline--strip-non-media-side-channel-blocks
                          orig)))
                      (mevedel-tool-media-prepare-tool-result
                       backend clean-tc tool-results-dir)))
                   (llm-result (car prepared))
                   (native-media (cdr prepared)))
              (push native-media media-by-index)
              (when (and llm-result (not (equal orig llm-result)))
                (push (cons tc orig) saved)
                (plist-put tc :result llm-result))))
          (mevedel-tool-media-add-to-provider-result
           backend
           (funcall orig-fun backend tool-use)
           (nreverse media-by-index)))
      (dolist (entry saved)
        (plist-put (car entry) :result (cdr entry))))))

(defun mevedel-pipeline-install-tool-result-scrubber ()
  "Install gptel interop advice for tool-result continuation paths."
  (require 'mevedel-tool-media)
  (advice-add 'gptel--parse-tool-results :around
              #'mevedel--parse-tool-results-scrub-advice))

(defun mevedel-pipeline-uninstall-tool-result-scrubber ()
  "Remove gptel interop advice for tool-result continuation paths."
  (advice-remove 'gptel--parse-tool-results
                 #'mevedel--parse-tool-results-scrub-advice))

(defun mevedel-pipeline-extract-render-data
    (result-string &optional session expected-tool-use-id
                   allow-payload-tool-use-id)
  "Return (VISIBLE-PART . RENDER-DATA) parsed from RESULT-STRING.
VISIBLE-PART is the tool result with the side-channel block stripped.
RENDER-DATA is the plist deserialized from inside the block, or
nil when no valid block is present.  Unparseable payloads are treated as
absent: the original string is returned verbatim in VISIBLE-PART.
SESSION, EXPECTED-TOOL-USE-ID, and ALLOW-PAYLOAD-TOOL-USE-ID
control trusted side-channel lookup."
  (require 'mevedel-tool-media)
  (let ((tool-results-dir
         (when-let* ((save-path (and session
                                     (mevedel-session-save-path session))))
           (file-name-concat save-path "tool-results"))))
    (if (not (stringp result-string))
        (cons result-string nil)
      (let ((blocks (mevedel-pipeline--render-data-blocks result-string)))
        (if (null blocks)
            (if (string-search mevedel-pipeline--render-data-open
                               result-string)
                (cons result-string nil)
            (cons
             (car (mevedel-tool-media-extract
                   (mevedel-tool-media-strip-blocks result-string)
                   tool-results-dir
                   expected-tool-use-id
                   allow-payload-tool-use-id))
             nil))
          (let* ((block (car (last blocks)))
                 (visible
                  (concat (substring result-string 0 (car block))
                          (substring result-string (cadr block)))))
            (cons (string-trim-right
                   (car (mevedel-tool-media-extract
                         (mevedel-tool-media-strip-blocks visible)
                         tool-results-dir
                         expected-tool-use-id
                         allow-payload-tool-use-id)))
                  (caddr block))))))))

(defun mevedel-pipeline--step-attach-render-data (context next _fail)
  "Embed render-data from CONTEXT, then call NEXT.

When CONTEXT holds render-data or an explicit handler status and the
`:result' is a string, append a hidden delimiter-wrapped block carrying
the serialized data.  Explicit status is stored under `:status' for renderer
dispatch.  The block is propertized `invisible' for the data-buffer display
and recognised by the view interpreter via its delimiters.  An `:around'
advice on `gptel--parse-tool-results' strips the block on the LLM path only;
see `mevedel-pipeline--format-render-data-block'.

FAIL is unused; render-data attachment never fails.

When neither was produced, passes CONTEXT through unchanged."
  (let* ((result (plist-get context :result))
         (status (plist-get context :status))
         (render-data (plist-get context :render-data))
         (render-data
          (if status
              (plist-put (copy-sequence render-data) :status status)
            render-data)))
    (if (and render-data (stringp result))
        (funcall next
                 (plist-put context :result
                            (concat result
                                    (mevedel-pipeline--format-render-data-block
                                     render-data))))
      (funcall next context))))

(defun mevedel-pipeline--step-attach-media-data (context next _fail)
  "Embed media side-channel data from CONTEXT, then call NEXT.

MEDIA is a list of plists, usually carrying at least `:path', `:mime',
and `:kind'.  The block is hidden in the data buffer and stripped at the
gptel tool-result serialization boundary.  Backends that gain native
tool-result media support can read this contract at that boundary
without changing handler return shapes."
  (require 'mevedel-tool-media)
  (let ((result (plist-get context :result))
        (media (plist-get context :media)))
    (if (and media (stringp result))
        (funcall next
                 (plist-put context :result
                            (mevedel-tool-media-attach-result
                             result media
                             (mevedel-pipeline--tool-results-dir
                              (plist-get context :session)
                              (plist-get context :buffer))
                             (plist-get context :tool-use-id))))
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
                      (min max-size mevedel-pipeline--default-max-result-size))))
    (cond
     ((or (null effective)
          (null result)
          (not (stringp result))
          (<= (length result) effective))
      (funcall next context))
     ((eq 'error (mevedel-pipeline--context-status context))
     (funcall next
               (plist-put context :result
                          (mevedel-pipeline--truncate-error-result
                           result tool
                           (not (plist-member context :status))))))
     (t
      ;; Result exceeds limit -- persist or truncate.  Session/buffer
      ;; context was captured at `mevedel-pipeline-run-tool'
      ;; entry; do not re-read it from `current-buffer' here because
      ;; the handler may have run (and called back) from inside a
      ;; `with-temp-buffer' wrapper.
      (let ((session (plist-get context :session))
            (buffer (plist-get context :buffer)))
        (funcall next
                 (plist-put context :result
                            (mevedel-pipeline--persist-result
                             result tool session buffer))))))))

(defun mevedel-pipeline--step-post-tool-hooks (context next _fail)
  "Run post-tool hooks for CONTEXT, then call NEXT.

Hooks receive both `:raw-result' and the final `:result'.  Only an
explicit `:updated-result' changes the model-visible tool result."
  (require 'mevedel-tool-media)
  (let* ((result (plist-get context :result))
         (model-result
          (mevedel-tool-media-result-for-hooks
           (mevedel-pipeline--strip-non-media-side-channel-blocks result)))
         (raw-result
          (mevedel-tool-media-result-for-hooks
           (mevedel-pipeline--strip-non-media-side-channel-blocks
            (plist-get context :raw-result))))
         (error-p (eq 'error (mevedel-pipeline--context-status context)))
         (event (if error-p
                    'PostToolUseFailure
                  'PostToolUse))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace)))
    (mevedel-pipeline--run-hook-event
     event
     (mevedel-hooks-tool-event-plist
      event context
      :raw-result raw-result
      :result model-result
      :tool-response model-result
      :error (and error-p result))
     (lambda (decision)
       (let ((context (mevedel-pipeline--record-hook-context
                       context decision event)))
         (setq context
               (mevedel-pipeline--record-hook-audit
                context
                (mevedel-pipeline--hook-context-audit-records
                 decision event)))
         (cond
          ((plist-member decision :updated-result)
           (setq context
                 (mevedel-pipeline--record-hook-audit
                  context
                  (mevedel-pipeline--hook-result-rewrite-audit-record
                   event model-result
                   (plist-get decision :updated-result)
                   decision)))
           (funcall next
                    (plist-put
                     (plist-put
                      context :result
                      (mevedel-pipeline--append-hook-side-channel
                       (plist-get decision :updated-result)
                       context))
                     :media nil)))
          (t
           (funcall next
                    (plist-put
                     context :result
                     (mevedel-pipeline--append-hook-side-channel
                      result context)))))))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))


;;
;;; Specialist tool nudges

(defun mevedel-pipeline--step-specialist-nudges (context next _fail)
  "Apply specialist-tool prompting policy to CONTEXT, then call NEXT."
  (require 'mevedel-specialist-nudges)
  (funcall next (mevedel-specialist-nudges-apply context)))

;;
;;; Step list builder

(defun mevedel-pipeline--build-steps (tool)
  "Build the standard step list for TOOL.

Returns a list of step functions based on TOOL's behavioral flags:
  1. validate            -- always included
  2. pre-tool-hooks      -- always included
  3. permission          -- always included
  4. snapshot            -- skipped if read-only-p
  5. handler             -- always included
  6. repair-reminder     -- appends feedback for committed input repairs
  7. render-transform    -- always included; no-op when tool has none
  8. persist             -- included when max-result-size is set
  9. specialist-nudges   -- bounded guidance for generic code tools
  10. post-tool-hooks    -- always included
  11. persist            -- included when max-result-size is set; bounds
                            hook-updated results
  12. attach-render-data -- always included; no-op when handler returned
                            neither render-data nor explicit status
  13. attach-media-data  -- always included; no-op when handler returned
                             no media"
  (let ((steps nil))
    (push #'mevedel-pipeline--step-attach-media-data steps)
    (push #'mevedel-pipeline--step-attach-render-data steps)
    (when (mevedel-tool-max-result-size tool)
      (push #'mevedel-pipeline--step-persist steps))
    (push #'mevedel-pipeline--step-post-tool-hooks steps)
    (push #'mevedel-pipeline--step-specialist-nudges steps)
    (when (mevedel-tool-max-result-size tool)
      (push #'mevedel-pipeline--step-persist steps))
    (push #'mevedel-pipeline--step-render-transform steps)
    (push #'mevedel-pipeline--step-repair-reminder steps)
    (push #'mevedel-pipeline--step-handler steps)
    (unless (mevedel-tool-read-only-p tool)
      (push #'mevedel-pipeline--step-snapshot steps))
    (push #'mevedel-pipeline--step-permission steps)
    (push #'mevedel-pipeline--step-pre-tool-hooks steps)
    (push #'mevedel-pipeline--step-validate steps)
    steps))


;;
;;; Entry point

(defun mevedel-pipeline-run-tool (tool callback args)
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
         (tool-use-id (mevedel-pipeline--current-tool-use-id tool args))
         (repair-entry
          (mevedel-tool-repair-consume-ledger-entry tool args))
         (steps (mevedel-pipeline--build-steps tool))
         (context (list :tool tool :args args
                        :session session :workspace workspace
                        :request request :invocation invocation
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
                        :origin (mevedel-current-origin)
                        :buffer dispatch-buffer
                        :default-directory workdir))
         (called nil)
         (once-callback
          (lambda (result)
            (cond
             ((not called)
              (setq called t)
              (when (and session (fboundp 'mevedel-telemetry-record))
                (mevedel-telemetry-record
                 session 'tool-finished
                 :tool-name (mevedel-tool-name tool)
                 :tool-use-id tool-use-id
                 :request-id (and request
                                  (fboundp 'mevedel-request-id)
                                  (mevedel-request-id request))
                 :outcome (if (and (stringp result)
                                   (string-prefix-p "Error:" result))
                              'error
                            'success)
                 :result-chars (and (stringp result) (length result))
                 :result-bytes (and (stringp result) (string-bytes result))))
              (mevedel-tool-repair-record-result repair-entry result)
              (condition-case err
                  (funcall callback result)
                (error
                 (display-warning
                  'mevedel
                  (format "Pipeline final callback signaled: %S" err)
                  :warning))))
             (t
              ;; Symmetric with the per-step latch's warning at
              ;; `mevedel-pipeline--run'.  The runner's condition-case
              ;; reaches us here when it caught a sync error escaping
              ;; from a step's NEXT recursion AFTER the recursion
              ;; already fired CALLBACK with a success result.  That is
              ;; the bug-fix path; we drop the late error but flag it so
              ;; a recurring drop is diagnosable rather than silent.
              (display-warning
               'mevedel
               (format "Pipeline callback fired twice; dropping late \
delivery: %S"
                       result)
               :warning))))))
    (when (and session (fboundp 'mevedel-telemetry-record))
      (mevedel-telemetry-record
       session 'tool-received
       :tool-name (mevedel-tool-name tool)
       :tool-use-id tool-use-id
       :request-id (and request
                        (fboundp 'mevedel-request-id)
                        (mevedel-request-id request))
       :origin (mevedel-current-origin)
       :read-only (and (mevedel-tool-read-only-p tool) t)))
    (mevedel-pipeline--run steps once-callback context)))


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
