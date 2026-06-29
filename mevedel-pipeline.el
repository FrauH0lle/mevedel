;;; mevedel-pipeline.el -- Tool execution pipeline -*- lexical-binding: t -*-

;;; Commentary:

;; Sequential step-based execution engine for mevedel tools. Each tool
;; invocation runs through a standard pipeline: validate -> permission ->
;; snapshot -> handler -> render-transform -> persist ->
;; specialist-nudges -> attach-render-data -> post-hooks -> attach-media.
;; Tool handlers that need user confirmation of a file change call
;; `mevedel-preview-mode-add-preview' directly; there is no explicit
;; confirm step in the pipeline.
;;
;; The persist step saves oversized results to disk and replaces them
;; with a preview + file path, preventing LLM context overflow from
;; unexpectedly large tool output.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-hooks)
(require 'mevedel-utilities)
(require 'mevedel-reminders)
(require 'mevedel-permission-log)

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
(declare-function mevedel-tool--validate-args "mevedel-tool-registry"
                  (tool-name args arg-specs))
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
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-turn-count
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-deferred-set
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-deferred-injected
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-specialist-nudge-state
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-set-specialist-nudge-state
                  "mevedel-agents" (invocation state))

;; `mevedel-reminders'
(declare-function mevedel-reminders-specialist-capabilities
                  "mevedel-reminders" (session))

(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar read-eval)

;; `gptel-request'
(declare-function gptel--model-capable-p "ext:gptel-request"
                  (cap &optional model))
(declare-function gptel--model-mime-capable-p "ext:gptel-request"
                  (mime &optional model))
(declare-function gptel-fsm-info "ext:gptel-request" (fsm))
(defvar gptel-backend)

;; `mevedel-tool-fs'
(declare-function mevedel--snapshot-file-if-needed "mevedel-tool-fs" (filepath))

;; `mevedel-tool-ui'
(require 'mevedel-permission-queue)

;; `mevedel-tools'
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--ctx-record-used "mevedel-tools" (ctx name))
(defvar mevedel-tools--current-fsm)

;; `mevedel-hooks'
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

(defun mevedel-pipeline--tool-results-dir (session buffer)
  "Return SESSION's tool-results directory, materializing when possible.

When SESSION has no save path yet, use
`mevedel-session-persistence--shallow-ensure-files' with BUFFER so
oversized tool output produced during the first turn can still be
owned by the session.  Returns nil when there is no session,
persistence is disabled, or shallow materialization fails."
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
             (preview-end (min (length result) mevedel-pipeline--preview-size))
             ;; Cut at last newline within preview range to avoid mid-line breaks
             (cut (let ((nl (cl-position ?\n result :from-end t
                                         :end preview-end)))
                    (if (and nl (> nl (/ preview-end 2))) nl preview-end)))
             (has-more (< cut (length result))))
        (write-region result nil file nil 'silent)
        (concat "<persisted-output>\n"
                (format "Output too large (%d chars). Full output saved to: %s\n\n"
                        (length result) file)
                (format "Preview (first %d chars):\n" cut)
                (substring result 0 cut)
                (if has-more "\n...\n" "\n")
                "</persisted-output>"))
    (mevedel-pipeline--truncate-result result tool)))

(defun mevedel-pipeline--truncate-result (result tool)
  "Truncate RESULT to a preview without persisting to disk.

Used when the result exceeds the size limit but no session-owned
persistence directory is available.  TOOL is used only for the tool
name in the message."
  (let* ((preview-end (min (length result) mevedel-pipeline--preview-size))
         (cut (let ((nl (cl-position ?\n result :from-end t :end preview-end)))
                (if (and nl (> nl (/ preview-end 2))) nl preview-end)))
         (has-more (< cut (length result))))
    (concat (format "Output too large (%d chars) and no session persistence \
directory available to persist full result (tool: %s).\n\n"
                    (length result) (mevedel-tool-name tool))
            (format "Preview (first %d chars):\n" cut)
            (substring result 0 cut)
            (if has-more "\n...\n" "\n"))))

(defun mevedel-pipeline--truncate-error-result (result tool)
  "Truncate oversized error RESULT without persisting it.

The returned string keeps an `Error:' prefix so downstream hooks and
renderers still treat the tool call as failed.  TOOL is used only for
the tool name in the message."
  (setq result (mevedel--normalize-message-text result))
  (let* ((preview-end (min (length result) mevedel-pipeline--preview-size))
         (cut (let ((nl (cl-position ?\n result :from-end t :end preview-end)))
                (if (and nl (> nl (/ preview-end 2))) nl preview-end)))
         (has-more (< cut (length result))))
    (concat (format "Error: output too large (%d chars; tool: %s).\n\n"
                    (length result) (mevedel-tool-name tool))
            (format "Preview (first %d chars):\n" cut)
            (substring result 0 cut)
            (if has-more "\n...\n" "\n"))))


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

(defun mevedel-pipeline--plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while (consp plist)
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun mevedel-pipeline--args-match-p (tool-call-args pipeline-args)
  "Return non-nil when TOOL-CALL-ARGS match PIPELINE-ARGS.
Missing optional keys and explicit nil values are treated as equivalent
because gptel dispatches positional tool arguments through the tool
schema, while mevedel normalizes them back into a full plist."
  (if (and (listp tool-call-args) (listp pipeline-args))
      (cl-every
       (lambda (key)
         (equal (plist-get tool-call-args key)
                (plist-get pipeline-args key)))
       (delete-dups
        (append (mevedel-pipeline--plist-keys tool-call-args)
                (mevedel-pipeline--plist-keys pipeline-args))))
    (equal tool-call-args pipeline-args)))

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
                        (mevedel-pipeline--args-match-p
                         (plist-get tc :args)
                         args)))
                 tool-use))))
    (when call
      (plist-put call :mevedel-claimed t)
      (plist-get call :id))))

(defun mevedel-pipeline--run (steps callback context)
  "Run pipeline STEPS sequentially, calling CALLBACK with the result.

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

CALLBACK must be the once-fire wrapper installed at the top-level entry
(`mevedel-pipeline-run-tool').  The runner's `condition-case' branches
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
                (mevedel-pipeline--run rest callback updated-ctx))))
           (fail-cont
            (lambda (reason)
              (when (funcall try-settle 'fail)
                (mevedel-pipeline--with-context-default-directory
                 context
                 (lambda ()
                   (funcall callback
                            (mevedel-pipeline--format-failure reason))))))))
      (condition-case err
          (mevedel-pipeline--with-context-default-directory
           context
           (lambda ()
             (funcall step context next-cont fail-cont)))
        (mevedel-validation-error
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-failure
                      (or (cadr err) "Validation error"))))))
        (mevedel-permission-denied
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-failure
                      (if (cadr err)
                          (format "Permission denied: %s" (cadr err))
                        "Permission denied"))))))
        (mevedel-pipeline-error
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-failure
                      (or (cadr err) "Pipeline error"))))))
        (error
         (mevedel-pipeline--with-context-default-directory
          context
          (lambda ()
            (funcall callback
                     (mevedel-pipeline--format-failure
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
         (err (mevedel-tool--validate-args
               (mevedel-tool-name tool)
               args
               (mevedel-tool-args tool))))
    (if err
        (signal 'mevedel-validation-error (list err))
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
  (mevedel-tool--validate-args
   (mevedel-tool-name tool)
   args
   (mevedel-tool-args tool)))

(defun mevedel-pipeline--record-hook-context (context decision)
  "Append DECISION's additional hook context to CONTEXT."
  (if-let* ((additional (plist-get decision :additional-context)))
      (plist-put context :hook-additional-context
                 (append (plist-get context :hook-additional-context)
                         additional))
    context))

(defun mevedel-pipeline--append-hook-context-string (text context)
  "Append accumulated hook context from CONTEXT to TEXT."
  (let ((additional (plist-get context :hook-additional-context)))
    (if (and additional (stringp text))
        (concat text
                "\n\n<hook-context>\n"
                (mapconcat (lambda (item) (format "%s" item))
                           additional
                           "\n")
                "\n</hook-context>")
      text)))

(defun mevedel-pipeline--run-hook-event
    (event event-plist callback context session workspace request invocation)
  "Run hook EVENT in CONTEXT's dispatch buffer when it is still live."
  (let ((buffer (plist-get context :buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (mevedel-hooks-run-event
           event event-plist callback session workspace request invocation))
      (mevedel-hooks-run-event
       event event-plist callback session workspace request invocation))))

(defun mevedel-pipeline--permission-origin (context)
  "Return the diagnostic origin for permission CONTEXT."
  (or (plist-get context :origin)
      (and-let* ((inv (plist-get context :invocation))
                 ((fboundp 'mevedel-agent-invocation-p))
                 ((mevedel-agent-invocation-p inv)))
        (mevedel-agent-invocation-agent-id inv))
      "main"))

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
  "Persist a sanitized permission-decision diagnostic for CONTEXT."
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
  "Run `PreToolUse' hooks before permission checking.

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
       (cond
	((and (plist-member decision :continue)
	      (not (plist-get decision :continue)))
         (let ((reason (format "blocked by PreToolUse: %s"
                               (or (plist-get decision :stop-reason)
                                   "hook stopped tool execution"))))
           (mevedel-pipeline--log-permission-decision
            context
            (list :outcome 'deny
                  :raw-outcome `(deny . ,reason)
                  :via 'pre-tool-hook))
	   (funcall fail reason)))
	((eq (plist-get decision :permission-decision) 'deny)
	 (let ((reason (format "blocked by PreToolUse: %s"
                               (or (plist-get decision :permission-reason)
	                           "hook denied tool execution"))))
                   (mevedel-pipeline--log-permission-decision
                    context
                    (list :outcome 'deny
                          :raw-outcome `(deny . ,reason)
                          :via 'pre-tool-hook))
           (mevedel-pipeline--fail-permission-denied
            context fail
            (format "Permission denied: %s" reason)
            reason)))
        (t
         (let ((updated (mevedel-pipeline--record-hook-context
                         context decision)))
           (when (plist-member decision :permission-decision)
             (setq updated
                   (plist-put
                    updated :hook-permission-decision
                    (plist-get decision :permission-decision))))
           (if (plist-member decision :updated-input)
               (let* ((args (plist-get decision :updated-input))
                      (err (mevedel-pipeline--validate-updated-args
                            tool args)))
                 (if err
                     (funcall fail err)
                   (funcall next (plist-put updated :args args))))
             (funcall next updated))))))
     context session workspace request invocation)))

(defun mevedel-pipeline--apply-hook-permission-decision (outcome context)
  "Apply CONTEXT's hook permission decision to permission OUTCOME.

Hook `deny' always wins.  Hook `ask' can tighten an `allow' into a
prompt.  Hook `allow' can skip a prompt only when the normal resolver
returned `ask'; explicit denials from the resolver stay intact."
  (let ((decision (plist-get context :hook-permission-decision)))
    (pcase decision
      ('deny 'deny)
      ('ask
       (if (memq outcome '(allow approve implement implement-clear))
           'ask
         outcome))
      ('allow
       (if (eq outcome 'ask)
           'allow
         outcome))
      (_ outcome))))

(defun mevedel-pipeline--fail-permission-denied
    (context fail reason &optional model-reason)
  "Run `PermissionDenied' hooks, then call FAIL with REASON.
MODEL-REASON is included in the hook event when available."
  (let ((session (plist-get context :session))
        (workspace (plist-get context :workspace)))
    (mevedel-pipeline--run-hook-event
     'PermissionDenied
     (mevedel-hooks-tool-event-plist
      'PermissionDenied context
      :permission-reason (or model-reason reason))
     (lambda (decision)
       (let* ((updated (mevedel-pipeline--record-hook-context
                        context decision))
              (final-reason
               (or (plist-get decision :permission-reason)
                   reason)))
         (funcall fail
                  (mevedel-pipeline--append-hook-context-string
                   final-reason updated))))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))

(defun mevedel-pipeline--step-permission (context next fail)
  "Check permission for the tool invocation.

Reads session / workspace from CONTEXT (captured at
`mevedel-pipeline-run-tool' entry) so that an async continuation
firing from another buffer still sees the correct session state.

Invokes `mevedel-check-permission-async' for the 9-step decision
chain.  When the chain (or a tool slot) yields `ask', the step
drives the generic async prompt and applies the result through
`mevedel-permission--apply-prompt-result' so session / persistent
rule storage is honored.  When a path is outside the workspace
root and no explicit rule covers it, the stored rule is
tool-agnostic (`*') and directory-scoped -- byte-for-byte the same
shaping the sync path produced.

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
  "Translate a permission OUTCOME into NEXT / FAIL for the pipeline step.

OUTCOME is the union of (a) results emitted by a permission slot via
`cont' (`allow', `deny', `(deny . REASON)', `(feedback . TEXT)',
`aborted', `ask') and (b) results emitted by the generic async prompt
overlay after an `ask' is routed through it (`allow-once',
`allow-session', `always-allow', `deny-once', `deny-session',
`aborted').

`ask' routes through the standard prompt path and recurses with the
user's UI choice.  Rule-scope outcomes (`allow-session' etc.) are
pre-collapsed via `mevedel-permission--apply-prompt-result' so that
session / persistent rules land with the correct scope before the
translator fires NEXT / FAIL."
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
            (decision-metadata decision))
       (cl-labels
           ((enqueue-prompt
              (prompt-context)
              ;; route through the session permission queue rather
              ;; than calling the prompt-async overlay directly.  When the
              ;; queue is empty, the head is rendered immediately and the
              ;; UX is identical to the prior path; when non-empty, the
              ;; entry waits its turn.  Either way the callback receives
              ;; the same prompt-outcome vocabulary as before.
              ;;
              ;; Coalesce-time re-evaluation goes back through
              ;; `mevedel-check-permission' which itself handles the
              ;; protected-path / deny-precedence rules from the decision
              ;; chain; the flag below is retained on the queue entry for
              ;; renderers and tests that need the original entry shape.
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name tool-name
                     :args args
                     :specifier-key rule-key
                     :specifier-value rule-value
                     :protected-path
                     (plist-get permission-context :protected-path)
                     :include-always
                     (plist-get permission-context :include-always)
                     :workspace workspace
                     :origin
                     ;; Resolve the leaf agent's canonical id by looking
                     ;; up the invocation captured at dispatch time;
                     ;; falls back to "main" for main-thread dispatches.
                     (or (plist-get prompt-context :origin)
                         (and-let* ((inv (plist-get context :invocation))
                                    ((fboundp 'mevedel-agent-invocation-p))
                                    ((mevedel-agent-invocation-p inv)))
                           (mevedel-agent-invocation-agent-id inv))
                         "main")
                     :callback
                     (lambda (prompt-outcome)
                       ;; This callback fires after the runner's outer
                       ;; `condition-case' has unwound, so a `signal' from
                       ;; `apply-prompt-result' (e.g. an `always-allow' write
                       ;; to `.mevedel/permissions.el' failing) would
                       ;; otherwise escape and strand the FSM in TOOL.
                       ;; Catch any error here and route through `fail' --
                       ;; the runner latch enforces exactly-once so this
                       ;; never duplicates with a successful `next' on the
                       ;; happy path.  Pre-collapse rule-scope outcomes via
                       ;; `apply-prompt-result' first so the user's scope
                       ;; choice (allow-session, always-allow, deny-session)
                       ;; persists rules before we dispatch.
                       (condition-case err
                           (let ((collapsed
                                  (pcase prompt-outcome
                                    ((or 'allow-once 'allow-session
                                         'always-allow 'deny-once
                                         'deny-session)
                                     (mevedel-permission--apply-prompt-result
                                      prompt-outcome rule-tool session workspace
                                      (and (eq rule-key :path) rule-value)
                                      :spec-key rule-key
                                      :spec-value rule-value))
                                    ((or 'allow 'deny 'aborted) prompt-outcome)
                                    (other other))))
                             (mevedel-pipeline--dispatch-permission-outcome
                              collapsed prompt-context next fail
                              :tool-name tool-name :path path :session session
                              :workspace workspace
                              :workspace-root workspace-root
                              :allowed-roots allowed-roots
                              :permission-context permission-context))
                         (error
                          (funcall fail (error-message-string err))))))
               session)))
         (mevedel-pipeline--run-hook-event
          'PermissionRequest
          (mevedel-hooks-tool-event-plist
           'PermissionRequest context
           :specifier-key rule-key
           :specifier-value rule-value)
	  (lambda (decision)
	    (let ((context (mevedel-pipeline--record-hook-context
	                    context decision)))
	      (cond
		       ((and (plist-member decision :continue)
		             (not (plist-get decision :continue)))
                        (mevedel-pipeline--log-permission-decision
                         context
                         (list :outcome 'deny
                                :raw-outcome `(deny . ,(format
                                                        "blocked by PermissionRequest: %s"
                                                        (or (plist-get decision
                                                                         :stop-reason)
                                                            "hook stopped tool")))
                                :via 'permission-request-hook))
				        (mevedel-pipeline--dispatch-permission-outcome
		         `(deny . ,(format
                                    "blocked by PermissionRequest: %s"
                                    (or (plist-get decision :stop-reason)
		                        "hook stopped tool")))
		         context next fail
		         :tool-name tool-name :path path :session session
		         :workspace workspace :workspace-root workspace-root
			 :allowed-roots allowed-roots))
	       ((eq (plist-get decision :permission-decision) 'allow)
                        (mevedel-pipeline--log-permission-decision
                         context
                         (list :outcome 'allow
                               :raw-outcome 'allow
                               :via 'permission-request-hook))
	        (mevedel-pipeline--dispatch-permission-outcome
	         'allow context next fail
	         :tool-name tool-name :path path :session session
	         :workspace workspace :workspace-root workspace-root
			 :allowed-roots allowed-roots))
		       ((eq (plist-get decision :permission-decision) 'deny)
                        (let ((raw `(deny . ,(format
                                             "blocked by PermissionRequest: %s"
                                             (or (plist-get decision
                                                            :permission-reason)
                                                 "hook denied permission")))))
                          (mevedel-pipeline--log-permission-decision
                           context
                           (list :outcome 'deny
                                 :raw-outcome raw
                                 :via 'permission-request-hook)))
		        (mevedel-pipeline--dispatch-permission-outcome
		         `(deny . ,(format
                                    "blocked by PermissionRequest: %s"
                                    (or (plist-get decision :permission-reason)
		                        "hook denied permission")))
		         context next fail
		         :tool-name tool-name :path path :session session
		         :workspace workspace :workspace-root workspace-root
			 :allowed-roots allowed-roots))
	       (t
	        (when decision-metadata
                          (mevedel-pipeline--log-permission-decision
                           context decision-metadata))
		        (enqueue-prompt context)))))
          context session workspace
          (plist-get context :request)
          (plist-get context :invocation)))))
    ((or 'allow 'approve 'implement 'implement-clear)
     (funcall next context))
    ('deny
     (mevedel-pipeline--fail-permission-denied
      context fail "Permission denied"))
    (`(deny . ,reason)
     (mevedel-pipeline--fail-permission-denied
      context fail (format "Permission denied: %s" reason) reason))
    (`(feedback . ,text)
     (mevedel-pipeline--fail-permission-denied
      context fail (format "Permission denied: %s" text) text))
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

(defun mevedel-pipeline--render-plist-p (value)
  "Return non-nil when VALUE is a handler return plist carrying render-data.
Recognizes a plist shape of the form (:result STRING :render-data DATA ...).
Required: VALUE must be a proper list whose first element is a keyword and
that contains a `:result' key."
  (and (listp value)
       (keywordp (car-safe value))
       (plist-member value :result)))

(defun mevedel-pipeline--split-handler-return (raw)
  "Split handler return RAW into a (RESULT . RENDER-DATA) cons.
When RAW matches `mevedel-pipeline--render-plist-p', destructure into its
`:result' and `:render-data' fields. Otherwise RAW is taken as the result
with no render-data."
  (if (mevedel-pipeline--render-plist-p raw)
      (cons (plist-get raw :result) (plist-get raw :render-data))
    (cons raw nil)))

(defun mevedel-pipeline--normalize-tool-string (value)
  "Return VALUE as JSON-safe model text when VALUE is a string."
  (if (stringp value)
      (mevedel--normalize-message-text value)
    value))

(defun mevedel-pipeline--step-handler (context next _fail)
  "Run the tool handler.

For async tools (async-p is non-nil), the handler receives a callback as
its first argument followed by the args plist.  For sync tools, the
handler receives just the args plist and returns the result directly.

A handler may return either a plain result string (legacy shape) or a
plist of the form (:result STRING :render-data DATA :media ITEMS).
In the latter case, the result string flows through the rest of the
pipeline and side-channel data is carried alongside in CONTEXT so the
attachment steps can embed it adjacent to the result for persistence,
view rendering, and backend serialization boundaries.

FAIL is unused -- handler-owned overlays (RequestAccess)
embed their failure modes in the result string (`Error: ...').
Adding a `fail' channel to the handler step would force every async
tool handler to take one; keeping them string-shaped preserves the
existing contract.

Sets `:result' and `:render-data' in CONTEXT for downstream steps;
NEXT is called on success."
  (let* ((tool (plist-get context :tool))
         (handler (mevedel-tool-handler tool))
         (args (plist-get context :args))
         (store (lambda (raw)
                  (let ((split (mevedel-pipeline--split-handler-return raw)))
                    (let ((result
                           (mevedel-pipeline--normalize-tool-string
                            (car split)))
                          (updated context))
                      (setq updated
                            (plist-put
                             (plist-put
                              (plist-put updated :result result)
                              :raw-result result)
                             :render-data (cdr split)))
                      (when (mevedel-pipeline--render-plist-p raw)
                        (setq updated
                              (plist-put updated :media
                                         (plist-get raw :media))))
                      updated)))))
    (mevedel-pipeline--record-use tool)
    (if (mevedel-tool-async-p tool)
        (funcall handler
                 (lambda (raw) (funcall next (funcall store raw)))
                 args)
      (funcall next (funcall store (funcall handler args))))))


(defconst mevedel-pipeline--render-data-open "<!-- mevedel-render-data -->"
  "Opening delimiter marking a hidden render-data side-channel block.
Emitted inside tool results so the view-buffer interpreter can extract
the serialized render-data without re-running the tool.")

(defconst mevedel-pipeline--render-data-close "<!-- /mevedel-render-data -->"
  "Closing delimiter marking the end of a render-data side-channel block.")

(defconst mevedel-pipeline--media-data-open "<!-- mevedel-media-data -->"
  "Opening delimiter marking a hidden tool media side-channel block.")

(defconst mevedel-pipeline--media-data-close "<!-- /mevedel-media-data -->"
  "Closing delimiter marking the end of a tool media side-channel block.")

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
  "Run TOOL's `:render-transform' to synthesize bounded render-data.

The transform receives the normalized string result before oversized
result persistence and before render/media side-channel attachment.
It is skipped when the handler already supplied render-data, when the
result is not a string, or when the result is an `Error:' string.

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
            (string-prefix-p "Error:" result))
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

(defvar mevedel-pipeline--media-store (make-hash-table :test #'equal)
  "In-memory lookup table for media side-channel records.")

(defun mevedel-pipeline--media-store-id ()
  "Return a fresh opaque id for a media side-channel record."
  (secure-hash 'sha256
               (format "%S:%S:%S" (current-time) (emacs-pid) (random t))))

(defun mevedel-pipeline--media-store-dir (session buffer)
  "Return a persistent media side-channel directory for SESSION."
  (when-let* ((dir (mevedel-pipeline--tool-results-dir session buffer)))
    (let ((media-dir (file-name-concat dir "media")))
      (make-directory media-dir t)
      media-dir)))

(defun mevedel-pipeline--write-media-store-record
    (id items session buffer tool-use-id)
  "Persist media ITEMS under ID for SESSION, returning the file path."
  (when-let* ((dir (mevedel-pipeline--media-store-dir session buffer)))
    (let ((file (file-name-concat dir (concat "media-" id ".el"))))
      (with-temp-buffer
        (let ((print-level nil)
              (print-length nil)
              (print-circle t))
          (prin1 (list :version 1 :id id
                       :tool-use-id tool-use-id
                       :items items)
                 (current-buffer)))
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region nil nil file nil 'silent)))
      file)))

(defun mevedel-pipeline--read-media-store-record
    (id session buffer expected-tool-use-id)
  "Return media items for ID from SESSION's trusted media store."
  (when-let* ((id (and (stringp id) id))
              (dir (mevedel-pipeline--media-store-dir session buffer))
              (file (file-name-concat dir (concat "media-" id ".el"))))
    (when (and (file-readable-p file)
               (string-match-p (rx string-start (+ hex) string-end) id))
      (condition-case _
          (let* ((read-eval nil)
                 (record (with-temp-buffer
                           (insert-file-contents-literally file)
                           (read (current-buffer))))
                 (items (and (equal id (plist-get record :id))
                             (equal expected-tool-use-id
                                    (plist-get record :tool-use-id))
                             (plist-get record :items))))
            (when (mevedel-pipeline--valid-media-items-p items)
              (mevedel-pipeline--sanitize-media-items items)))
        (error nil)))))

(defun mevedel-pipeline--media-store-record-items
    (record id payload-tool-use-id expected-tool-use-id)
  "Return media items from RECORD when provenance matches.
ID and PAYLOAD-TOOL-USE-ID come from the model-visible side-channel
payload.  EXPECTED-TOOL-USE-ID comes from the gptel tool-call record."
  (let ((items (and (equal id (plist-get record :id))
                    (equal payload-tool-use-id
                           (plist-get record :tool-use-id))
                    (or (null expected-tool-use-id)
                        (equal expected-tool-use-id payload-tool-use-id))
                    (plist-get record :items))))
    (when (mevedel-pipeline--valid-media-items-p items)
      (mevedel-pipeline--sanitize-media-items items))))

(defun mevedel-pipeline--store-media-data
    (media &optional session buffer tool-use-id)
  "Store MEDIA items and return a serializable side-channel reference."
  (when (mevedel-pipeline--valid-media-items-p media)
    (let* ((items (mevedel-pipeline--sanitize-media-items media))
           (id (mevedel-pipeline--media-store-id))
           (record (list :version 1 :id id
                         :tool-use-id tool-use-id
                         :items items)))
      (mevedel-pipeline--write-media-store-record
       id items session buffer tool-use-id)
      (puthash id record mevedel-pipeline--media-store)
      (append (list :id id)
              (when tool-use-id
                (list :tool-use-id tool-use-id))))))

(defun mevedel-pipeline--format-media-data-block
    (media &optional session buffer tool-use-id)
  "Return the serialized side-channel block string for MEDIA items."
  (if-let* ((ref (mevedel-pipeline--store-media-data
                  media session buffer tool-use-id)))
      (propertize
       (concat "\n" mevedel-pipeline--media-data-open "\n"
               (let ((print-level nil)
                     (print-length nil)
                     (print-circle t))
                 (prin1-to-string
                  (mevedel-pipeline--plain-render-data ref)))
               "\n" mevedel-pipeline--media-data-close "\n")
       'invisible t
       'mevedel-media-data t)
    ""))

(defun mevedel-pipeline--render-data-regexp ()
  "Return the regexp matching a render-data side-channel block.
Matches the delimiters (with their newline wrappers) and everything in
between.  Kept in sync with `mevedel-pipeline--format-render-data-block'."
  (concat "\n?"
          (regexp-quote mevedel-pipeline--render-data-open)
          "\\(?:.\\|\n\\)*?"
          (regexp-quote mevedel-pipeline--render-data-close)
          "\n?"))

(defun mevedel-pipeline--strip-render-data-blocks (string)
  "Return STRING with every render-data side-channel block removed."
  (replace-regexp-in-string
   (mevedel-pipeline--render-data-regexp) "" string t t))

(defun mevedel-pipeline--strip-media-data-blocks (string)
  "Return STRING with generated media side-channel blocks removed.
Literal delimiter text is left intact; only blocks carrying the
pipeline-owned `mevedel-media-data' text property are treated as hidden
side-channel data."
  (if (not (stringp string))
      string
    (let ((last 0)
          (search 0)
          (chunks nil)
          open)
      (while (setq open (string-search mevedel-pipeline--media-data-open
                                       string search))
        (if (not (get-text-property open 'mevedel-media-data string))
            (setq search (+ open (length mevedel-pipeline--media-data-open)))
          (let ((close (string-search mevedel-pipeline--media-data-close
                                      string open)))
            (if (null close)
                (setq search (+ open (length mevedel-pipeline--media-data-open)))
              (let* ((strip-start
                      (if (and (> open 0)
                               (= (aref string (1- open)) ?\n)
                               (get-text-property (1- open)
                                                  'mevedel-media-data string))
                          (1- open)
                        open))
                     (after-close (+ close
                                     (length
                                      mevedel-pipeline--media-data-close)))
                     (strip-end
                      (if (and (< after-close (length string))
                               (= (aref string after-close) ?\n)
                               (get-text-property after-close
                                                  'mevedel-media-data string))
                          (1+ after-close)
                        after-close)))
                (push (substring string last strip-start) chunks)
                (setq last strip-end)
                (setq search strip-end))))))
      (push (substring string last) chunks)
      (apply #'concat (nreverse chunks)))))

(defun mevedel-pipeline--strip-side-channel-blocks (string)
  "Return STRING with mevedel side-channel blocks removed."
  (mevedel-pipeline--strip-media-data-blocks
   (mevedel-pipeline--strip-render-data-blocks string)))

(defun mevedel-pipeline--read-media-data-payload
    (payload &optional session buffer expected-tool-use-id)
  "Read media side-channel PAYLOAD, returning stored media items.
The payload is read with `read-eval' disabled.  It contains only an
opaque store reference; media bytes are loaded from pipeline-owned
state, never from a model-visible transcript block."
  (condition-case _
      (let* ((read-eval nil)
             (data (read payload))
             (id (plist-get data :id))
             (tool-use-id (plist-get data :tool-use-id)))
        (or (and (stringp id)
                 (mevedel-pipeline--media-store-record-items
                  (gethash id mevedel-pipeline--media-store)
                  id tool-use-id expected-tool-use-id))
            (and expected-tool-use-id
                 (equal expected-tool-use-id tool-use-id)
                 (mevedel-pipeline--read-media-store-record
                  id session buffer expected-tool-use-id))))
    (error nil)))

(defun mevedel-pipeline--media-data-payload-tool-use-id (payload)
  "Return the tool-use id declared by media side-channel PAYLOAD."
  (condition-case _
      (let* ((read-eval nil)
             (data (read payload)))
        (plist-get data :tool-use-id))
    (error nil)))

(defun mevedel-pipeline-extract-media-data
    (result-string &optional session buffer expected-tool-use-id
                   allow-payload-tool-use-id)
  "Return (VISIBLE-PART . MEDIA) parsed from RESULT-STRING.
VISIBLE-PART is the tool result with media side-channel blocks stripped.
MEDIA is the Lisp object deserialized from inside the block, or nil."
  (if (not (stringp result-string))
      (cons result-string nil)
    (let ((open nil)
          (close nil)
          (data nil)
          (search 0))
      (while (and (not open)
                  (setq open (string-search mevedel-pipeline--media-data-open
                                            result-string search)))
        (let* ((payload-start (+ open (length
                                       mevedel-pipeline--media-data-open)))
               (candidate-close
                (string-search mevedel-pipeline--media-data-close
                               result-string payload-start))
               (candidate-payload
                (and candidate-close
                     (string-trim
                      (substring result-string payload-start
                                 candidate-close))))
               (candidate-terminal-p
                (and candidate-close
                     (string-match-p
                      "\\`[ \t\n\r]*\\'"
                      (substring result-string
                                 (+ candidate-close
                                    (length
                                     mevedel-pipeline--media-data-close))))))
               (payload-tool-use-id
                (and candidate-payload
                     allow-payload-tool-use-id
                     candidate-terminal-p
                     (mevedel-pipeline--media-data-payload-tool-use-id
                      candidate-payload)))
               (candidate-data
                (and candidate-payload
                     (or
                      (and expected-tool-use-id
                           (mevedel-pipeline--read-media-data-payload
                            candidate-payload session buffer
                            expected-tool-use-id))
                      (and payload-tool-use-id
                           (mevedel-pipeline--read-media-data-payload
                            candidate-payload session buffer
                            payload-tool-use-id)))))
               (property-data
                (and expected-tool-use-id
                     candidate-close
                     (get-text-property open 'mevedel-media-data result-string)
                     candidate-payload
                     (mevedel-pipeline--read-media-data-payload
                      candidate-payload session buffer
                      expected-tool-use-id))))
          (if (or candidate-data property-data)
              (setq close candidate-close
                    data (or candidate-data
                             property-data
                             (and expected-tool-use-id
                                  (mevedel-pipeline--read-media-data-payload
                                   candidate-payload session buffer
                                   expected-tool-use-id))
                             (and payload-tool-use-id
                                  (mevedel-pipeline--read-media-data-payload
                                   candidate-payload session buffer
                                   payload-tool-use-id))))
            (setq search (+ open
                            (length mevedel-pipeline--media-data-open))
                  open nil))))
      (if open
          (let* ((strip-start (if (and (> open 0)
                                       (= (aref result-string (1- open)) ?\n))
                                  (1- open)
                                open))
                 (after-close (+ close
                                 (length
                                  mevedel-pipeline--media-data-close)))
                 (trail-end (if (and (< after-close (length result-string))
                                     (= (aref result-string after-close) ?\n))
                                (1+ after-close)
                              after-close)))
            (cons (concat (substring result-string 0 strip-start)
                          (substring result-string trail-end))
                  data))
        (cons result-string nil)))))

(defun mevedel-pipeline--media-item-data (item)
  "Return stable base64 data from media ITEM.
Media side-channel entries carry `:data' so serialization replays the
bytes captured at tool execution time.  `:path' is metadata only; it is
not reread here because serialized tool output is not a permissioned
file-read channel."
  (plist-get item :data))

(defun mevedel-pipeline--valid-media-item-p (item)
  "Return non-nil when ITEM is a well-formed media side-channel plist."
  (let ((mime (and (listp item) (plist-get item :mime)))
        (kind (and (listp item) (plist-get item :kind)))
        (data (and (listp item) (plist-get item :data))))
    (and (listp item)
         (memq kind '(image document))
         (stringp mime)
         (member mime '("application/pdf"
                        "image/png" "image/jpeg" "image/gif" "image/webp"))
         (stringp data)
         (> (length data) 0))))

(defun mevedel-pipeline--valid-media-items-p (items)
  "Return non-nil when ITEMS is a non-empty list of valid media items."
  (and (consp items)
       (cl-every #'mevedel-pipeline--valid-media-item-p items)))

(defun mevedel-pipeline--sanitize-media-items (items)
  "Return ITEMS with only supported media side-channel keys retained."
  (mapcar
   (lambda (item)
     (let ((clean (list :mime (plist-get item :mime)
                        :kind (plist-get item :kind)
                        :data (plist-get item :data))))
       (when-let* ((path (plist-get item :path)))
         (when (stringp path)
           (setq clean (plist-put clean :path path))))
       (when-let* ((source (plist-get item :source)))
         (when (stringp source)
           (setq clean (plist-put clean :source source))))
       (when-let* ((page (plist-get item :page)))
         (when (integerp page)
           (setq clean (plist-put clean :page page))))
       clean))
   items))

(defun mevedel-pipeline--media-envelope-summary (text &optional replacement)
  "Return TEXT with base64 media payload bodies replaced by REPLACEMENT."
  (let ((body-start-marker "data:\n")
        (body-end-marker "\n</media-file>")
        (summary (or replacement "<native media block attached>"))
        (pos 0)
        (chunks nil)
        (done nil))
    (while (not done)
      (let ((body-start (string-search body-start-marker text pos)))
        (if (not body-start)
            (setq done t)
          (let* ((payload-start (+ body-start (length body-start-marker)))
                 (body-end (string-search body-end-marker text payload-start)))
            (if (not body-end)
                (setq done t)
              (push (substring text pos payload-start) chunks)
              (push summary chunks)
              (setq pos body-end))))))
    (if chunks
        (mapconcat #'identity (nreverse (cons (substring text pos) chunks)) "")
      text)))

(defun mevedel-pipeline--media-result-for-hooks (result)
  "Return RESULT with side-channel data and media payloads hidden from hooks."
  (if (not (stringp result))
      result
    (mevedel-pipeline--media-envelope-summary
     (mevedel-pipeline--strip-side-channel-blocks result)
     "<media omitted: media payload not exposed to hooks>")))

(defun mevedel-pipeline--anthropic-media-block (item)
  "Return an Anthropic content block for media ITEM."
  (let* ((mime (plist-get item :mime))
         (data (mevedel-pipeline--media-item-data item))
         (type (cond
                ((and mime (string-prefix-p "image/" mime)) "image")
                ((equal mime "application/pdf") "document")
                (t nil))))
    (when (and type data)
      `(:type ,type
        :source (:type "base64"
                 :media_type ,mime
                 :data ,data)))))

(defun mevedel-pipeline--bedrock-media-block (item)
  "Return a Bedrock content block for media ITEM."
  (let* ((mime (plist-get item :mime))
         (path (plist-get item :path))
         (data (mevedel-pipeline--media-item-data item))
         (image-format (cdr (assoc mime '(("image/jpg" . "jpeg")
                                          ("image/jpeg" . "jpeg")
                                          ("image/png" . "png")
                                          ("image/gif" . "gif")
                                          ("image/webp" . "webp")))))
         (doc-format (and (equal mime "application/pdf") "pdf")))
    (cond
     ((and image-format data)
      `(:image (:format ,image-format
                :source (:bytes ,data))))
     ((and doc-format data)
      `(:document (:format ,doc-format
                   :name ,(file-name-base (or path "document.pdf"))
                   :source (:bytes ,data)))))))

(defun mevedel-pipeline--media-data-url (item)
  "Return a data URL for media ITEM."
  (when-let* ((mime (plist-get item :mime))
              (data (mevedel-pipeline--media-item-data item)))
    (concat "data:" mime ";base64," data)))

(defun mevedel-pipeline--openai-image-media-p (item)
  "Return non-nil when ITEM can be sent through gptel's OpenAI image path."
  (and (eq (plist-get item :kind) 'image)
       (string-prefix-p "image/" (or (plist-get item :mime) ""))))

(defun mevedel-pipeline--openai-image-media-supported-p (backend media)
  "Return non-nil when BACKEND can receive MEDIA as OpenAI image messages."
  (and (or (mevedel-pipeline--backend-class-p backend 'gptel-openai)
           (mevedel-pipeline--backend-class-p backend 'gptel-openai-responses))
       (mevedel-pipeline--media-model-capable-p media)
       (cl-every #'mevedel-pipeline--openai-image-media-p media)))

(defun mevedel-pipeline--openai-responses-media-block (item)
  "Return an OpenAI Responses input_image block for media ITEM."
  (when-let* (((mevedel-pipeline--openai-image-media-p item))
              (url (mevedel-pipeline--media-data-url item)))
    `(:type "input_image" :image_url ,url)))

(defun mevedel-pipeline--openai-media-block (item)
  "Return an OpenAI chat-completions image_url block for media ITEM."
  (when-let* (((mevedel-pipeline--openai-image-media-p item))
              (url (mevedel-pipeline--media-data-url item)))
    `(:type "image_url" :image_url (:url ,url))))

(defun mevedel-pipeline--media-message-text (media)
  "Return a short model-facing description for attached MEDIA."
  (let ((paths (delq nil (mapcar (lambda (item) (plist-get item :path)) media))))
    (concat "Media returned by Read is attached as native input."
            (when paths
              (concat " Source: " (mapconcat #'identity paths ", "))))))

(defun mevedel-pipeline--openai-responses-media-message (media)
  "Return an OpenAI Responses user message carrying MEDIA."
  (when-let* ((blocks (delq nil
                            (mapcar
                             #'mevedel-pipeline--openai-responses-media-block
                             media))))
    (list :role "user"
          :content (vconcat
                    (list (list :type "input_text"
                                :text (mevedel-pipeline--media-message-text
                                       media)))
                    blocks))))

(defun mevedel-pipeline--openai-media-message (media)
  "Return an OpenAI chat-completions user message carrying MEDIA."
  (when-let* ((blocks (delq nil
                            (mapcar #'mevedel-pipeline--openai-media-block
                                    media))))
    (list :role "user"
          :content (vconcat
                    (list (list :type "text"
                                :text (mevedel-pipeline--media-message-text
                                       media)))
                    blocks))))

(defun mevedel-pipeline--backend-class-p (backend class)
  "Return non-nil when BACKEND is a cl-struct of CLASS."
  (and backend (ignore-errors (cl-typep backend class))))

(defun mevedel-pipeline--native-media-backend-p (backend)
  "Return non-nil when BACKEND supports native media in tool results."
  (or (mevedel-pipeline--backend-class-p backend 'gptel-anthropic)
      (mevedel-pipeline--backend-class-p backend 'gptel-bedrock)))

(defun mevedel-pipeline--media-model-capable-p (media)
  "Return non-nil when the current model can accept all MEDIA items."
  (and (fboundp 'gptel--model-capable-p)
       (gptel--model-capable-p 'media)
       (cl-every
        (lambda (item)
          (let ((mime (plist-get item :mime)))
            (or (not (fboundp 'gptel--model-mime-capable-p))
                (gptel--model-mime-capable-p mime))))
        media)))

(defun mevedel-pipeline--native-media-supported-p (backend media)
  "Return non-nil when BACKEND and current model support MEDIA."
  (and (mevedel-pipeline--native-media-backend-p backend)
       (mevedel-pipeline--media-model-capable-p media)))

(defun mevedel-pipeline--media-supported-p (backend media)
  "Return non-nil when BACKEND and current model can receive MEDIA natively."
  (or (mevedel-pipeline--native-media-supported-p backend media)
      (mevedel-pipeline--openai-image-media-supported-p backend media)))

(defun mevedel-pipeline--media-result-for-model (result media backend)
  "Return model-visible text for RESULT with MEDIA under BACKEND."
  (cond
   ((not (stringp result)) result)
   ((and media (mevedel-pipeline--media-supported-p backend media))
    (mevedel-pipeline--media-envelope-summary result))
   ((and media (not (mevedel-pipeline--media-model-capable-p media)))
    (mevedel-pipeline--media-envelope-summary
     result
     "<media omitted: current model does not support this media type>"))
   (media
    (mevedel-pipeline--media-envelope-summary
     result
     "<media omitted: backend cannot attach this media type>"))
   (t result)))

(defun mevedel-pipeline--maybe-add-native-media (backend parsed media-by-index)
  "Attach MEDIA-BY-INDEX to PARSED tool results when BACKEND supports it.
Returns PARSED unchanged for backends whose tool-result media shape is
unknown."
  (cond
   ((mevedel-pipeline--backend-class-p backend 'gptel-openai-responses)
    (append parsed
            (delq nil
                  (mapcar #'mevedel-pipeline--openai-responses-media-message
                          media-by-index))))
   ((mevedel-pipeline--backend-class-p backend 'gptel-openai)
    (append parsed
            (delq nil
                  (mapcar #'mevedel-pipeline--openai-media-message
                          media-by-index))))
   ((mevedel-pipeline--backend-class-p backend 'gptel-anthropic)
    (let* ((content (plist-get parsed :content))
           (blocks (and (vectorp content) (append content nil)))
           (i 0))
      (when blocks
        (plist-put
         parsed :content
         (vconcat
          (mapcar
           (lambda (block)
             (prog1
                 (if-let* ((media (nth i media-by-index)))
                     (let* ((text (plist-get block :content))
                            (native (delq nil
                                          (mapcar
                                           #'mevedel-pipeline--anthropic-media-block
                                           media))))
                       (if native
                           (plist-put
                            block :content
                            (vconcat
                             (list (list :type "text"
                                         :text (if (stringp text)
                                                   (mevedel-pipeline--media-envelope-summary text)
                                                 (prin1-to-string text))))
                             native))
                         block))
                   block)
               (cl-incf i)))
           blocks)))))
    parsed)
   ((mevedel-pipeline--backend-class-p backend 'gptel-bedrock)
    (let* ((content (plist-get parsed :content))
           (blocks (and (vectorp content) (append content nil)))
           (i 0))
      (when blocks
        (plist-put
         parsed :content
         (vconcat
          (mapcar
           (lambda (block)
             (prog1
                 (if-let* ((media (nth i media-by-index))
                           (tool-result (plist-get block :toolResult))
                           (native (delq nil
                                         (mapcar
                                          #'mevedel-pipeline--bedrock-media-block
                                          media))))
                     (progn
                       (plist-put
                        tool-result :content
                        (vconcat
                         (list (list :text
                                     (mevedel-pipeline--media-envelope-summary
                                      (or (plist-get (aref (plist-get tool-result :content) 0)
                                                     :text)
                                          ""))))
                         native))
                       block)
                   block)
               (cl-incf i)))
           blocks)))))
    parsed)
   (t parsed)))

(defun mevedel-pipeline--strip-printed-string-properties (payload)
  "Return PAYLOAD with printed propertized strings made plain.

Older render-data blocks could contain printed strings of the form
`#(\"...\" 0 N (PROP VAL ...))'.  If the saved text later changes
length, Emacs' reader can reject those property ranges before the view
can recover the payload.  The render-data side channel does not need
string text properties, so replace those printed forms with ordinary
string literals before retrying `read'."
  (with-temp-buffer
    (insert payload)
    (goto-char (point-min))
    (while (search-forward "#(\"" nil t)
      (let ((hash-start (match-beginning 0)))
        (condition-case nil
            (let* ((string-start (+ hash-start 2))
                   (form-end (scan-sexps (1+ hash-start) 1))
                   string)
              (goto-char string-start)
              (setq string (read (current-buffer)))
              (delete-region hash-start form-end)
              (insert (prin1-to-string string)))
          (error
           (goto-char (min (1+ hash-start) (point-max)))))))
    (buffer-string)))

(defun mevedel-pipeline--read-render-data-payload (payload)
  "Read render-data PAYLOAD, tolerating stale printed string properties."
  (condition-case _
      (read payload)
    (error
     (condition-case _
         (read (mevedel-pipeline--strip-printed-string-properties payload))
       (error :mevedel-parse-failed)))))

(defun mevedel-pipeline--find-render-data-block-by-agent-id (agent-id)
  "Return (BEG . END) of the first render-data block whose plist
`:agent-id' is AGENT-ID, or nil.
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

(defun mevedel--parse-tool-results-scrub-advice (orig-fun backend tool-use)
  "Strip render-data blocks from tool-call `:result' for the LLM payload.

Wraps `gptel--parse-tool-results' (a cl-defgeneric with per-backend
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
  (let ((saved nil))
    (unwind-protect
        (let ((media-by-index nil))
          (dolist (tc tool-use)
            (let* ((orig (plist-get tc :result))
                   (read-tool-p (equal (plist-get tc :name) "Read"))
                   (tool-use-id (plist-get tc :id))
                   (extracted
                    (and read-tool-p
                         tool-use-id
                         (stringp orig)
                         (mevedel-pipeline-extract-media-data
                          orig
                          (and (boundp 'mevedel--session) mevedel--session)
                          nil
                          tool-use-id)))
                   (extracted-media (cdr extracted))
                   (media (and read-tool-p
                               tool-use-id
                               (or
                                (and (mevedel-pipeline--valid-media-items-p
                                      (plist-get tc :media))
                                     (mevedel-pipeline--sanitize-media-items
                                      (plist-get tc :media)))
                                extracted-media)))
                   (cleaned (and (stringp orig)
                                 (if (and read-tool-p tool-use-id)
                                     (mevedel-pipeline--strip-render-data-blocks
                                      (car (or extracted (cons orig nil))))
                                   (mevedel-pipeline--strip-side-channel-blocks
                                    orig))))
                   (llm-result
                    (mevedel-pipeline--media-result-for-model
                     cleaned media backend))
                   (native-media
                    (and media
                         (mevedel-pipeline--media-supported-p
                          backend media)
                         media)))
              (push native-media media-by-index)
              (when (and llm-result (not (equal orig llm-result)))
                (push (cons tc orig) saved)
                (plist-put tc :result llm-result))))
          (mevedel-pipeline--maybe-add-native-media
           backend
           (funcall orig-fun backend tool-use)
           (nreverse media-by-index)))
      (dolist (entry saved)
        (plist-put (car entry) :result (cdr entry))))))

(defun mevedel-pipeline-install-tool-result-scrubber ()
  "Install gptel interop advice for tool-result continuation paths."
  (advice-add 'gptel--parse-tool-results :around
              #'mevedel--parse-tool-results-scrub-advice))

(defun mevedel-pipeline-uninstall-tool-result-scrubber ()
  "Remove gptel interop advice for tool-result continuation paths."
  (advice-remove 'gptel--parse-tool-results
                 #'mevedel--parse-tool-results-scrub-advice))

(defun mevedel-pipeline-extract-render-data
    (result-string &optional session buffer expected-tool-use-id
                   allow-payload-tool-use-id)
  "Return (VISIBLE-PART . RENDER-DATA) parsed from RESULT-STRING.
VISIBLE-PART is the tool result with the side-channel block stripped.
RENDER-DATA is the Lisp object deserialized from inside the block, or
nil when no valid block is present. Unparseable payloads are treated as
absent: the original string is returned verbatim in VISIBLE-PART."
  (if (not (stringp result-string))
      (cons result-string nil)
    (let ((open (string-search mevedel-pipeline--render-data-open
                               result-string)))
      (if (null open)
          (cons
           (car (mevedel-pipeline-extract-media-data
                 (mevedel-pipeline--strip-media-data-blocks result-string)
                 session buffer expected-tool-use-id
                 allow-payload-tool-use-id))
           nil)
        (let* ((payload-start (+ open (length mevedel-pipeline--render-data-open)))
               (close (string-search mevedel-pipeline--render-data-close
                                     result-string payload-start)))
          (if (null close)
              (cons result-string nil)
            (let* ((payload (string-trim
                             (substring result-string payload-start close)))
                   (data (mevedel-pipeline--read-render-data-payload
                          payload))
                   (trail-end (+ close
                                 (length mevedel-pipeline--render-data-close))))
              (if (eq data :mevedel-parse-failed)
                  (cons result-string nil)
                (cons (string-trim-right
                       (car (mevedel-pipeline-extract-media-data
                             (mevedel-pipeline--strip-media-data-blocks
                              (concat (substring result-string 0 open)
                                      (substring result-string trail-end)))
                             session buffer expected-tool-use-id
                             allow-payload-tool-use-id)))
                      data)))))))))

(defun mevedel-pipeline--step-attach-render-data (context next _fail)
  "Embed the render-data side-channel adjacent to the tool :result.

When CONTEXT holds a non-nil `:render-data' value and the `:result' is
a string, append a hidden delimiter-wrapped block carrying the
serialized render-data.  The block is propertized `invisible' for the
data-buffer display and recognised by the view interpreter via its
delimiters.  An `:around' advice on `gptel--parse-tool-results' strips
the block on the LLM path only -- see
`mevedel-pipeline--format-render-data-block'.

FAIL is unused; render-data attachment never fails.

When no render-data was produced, passes CONTEXT through unchanged."
  (let ((result (plist-get context :result))
        (render-data (plist-get context :render-data)))
    (if (and render-data (stringp result))
        (funcall next
                 (plist-put context :result
                            (concat result
                                    (mevedel-pipeline--format-render-data-block
                                     render-data))))
      (funcall next context))))

(defun mevedel-pipeline--step-attach-media-data (context next _fail)
  "Embed media side-channel data adjacent to the tool :result.

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
                            (concat (mevedel-pipeline--media-envelope-summary
                                     result)
                                    (mevedel-pipeline--format-media-data-block
                                     media
                                     (plist-get context :session)
                                     (plist-get context :buffer)
                                     (plist-get context :tool-use-id)))))
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

Skips entirely when the result is not a string.  Oversized error
results are truncated, not persisted, and keep an `Error:' prefix so
failure status is preserved.
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
     ((string-prefix-p "Error:" result)
      (funcall next
               (plist-put context :result
                          (mevedel-pipeline--truncate-error-result result tool))))
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
  "Run `PostToolUse' or `PostToolUseFailure' hooks.

Hooks receive both `:raw-result' and the final `:result'.  Only an
explicit `:updated-result' changes the model-visible tool result."
  (let* ((result (plist-get context :result))
         (model-result (mevedel-pipeline--media-result-for-hooks result))
         (raw-result (mevedel-pipeline--media-result-for-hooks
                      (plist-get context :raw-result)))
         (event (if (and (stringp result)
                         (string-prefix-p "Error:" result))
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
      :error (and (stringp result)
                  (string-prefix-p "Error:" result)
                  result))
     (lambda (decision)
       (let ((context (mevedel-pipeline--record-hook-context
                       context decision)))
         (cond
          ((plist-member decision :updated-result)
           (funcall next
                    (plist-put
                     (plist-put
                      context :result
                      (mevedel-pipeline--append-hook-context-string
                       (plist-get decision :updated-result)
                       context))
                     :media nil)))
          (t
           (funcall next
                    (plist-put
                     context :result
                     (mevedel-pipeline--append-hook-context-string
                      result context)))))))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))


;;
;;; Specialist tool nudges

(defconst mevedel-pipeline--specialist-nudge-max-per-family 2
  "Maximum number of times to nudge each specialist family per context.")

(defconst mevedel-pipeline--code-file-extensions
  '("c" "cc" "clj" "cljs" "cpp" "cs" "css" "dart" "el" "ex"
    "exs" "go" "h" "hpp" "hs" "java" "js" "jsx" "kt" "kts"
    "lua" "m" "mm" "php" "py" "rb" "rs" "scala" "scm" "sh"
    "swift" "ts" "tsx" "vue")
  "File extensions treated as code for specialist nudge heuristics.")

(defconst mevedel-pipeline--code-rg-types
  '("c" "clojure" "cpp" "csharp" "css" "dart" "elisp" "go"
    "haskell" "java" "js" "jsx" "kotlin" "lua" "objc" "php"
    "py" "python" "rb" "ruby" "rust" "scala" "scheme" "sh"
    "swift" "ts" "tsx")
  "Ripgrep type names treated as code for specialist nudge heuristics.")

(defconst mevedel-pipeline--structural-code-patterns
  '("class" "defclass" "defcustom" "defmacro" "defmethod" "defstruct"
    "defun" "enum" "function" "if" "import" "interface" "lambda"
    "let" "module" "namespace" "require" "struct" "switch" "try"
    "while")
  "Plain code patterns that usually indicate structural search.")

(defun mevedel-pipeline--ctx-deferred-set (ctx)
  "Return CTX's deferred-set slot, or nil."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-deferred-set ctx))
   ((mevedel-session-p ctx)
    (mevedel-session-deferred-set ctx))))

(defun mevedel-pipeline--ctx-deferred-injected (ctx)
  "Return CTX's deferred-injected slot, or nil."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-deferred-injected ctx))
   ((mevedel-session-p ctx)
    (mevedel-session-deferred-injected ctx))))

(defun mevedel-pipeline--ctx-nudge-state (ctx)
  "Return CTX's specialist nudge state."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-specialist-nudge-state ctx))
   ((mevedel-session-p ctx)
    (mevedel-session-specialist-nudge-state ctx))))

(defun mevedel-pipeline--set-ctx-nudge-state (ctx state)
  "Set CTX's specialist nudge STATE."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-set-specialist-nudge-state ctx state))
   ((mevedel-session-p ctx)
    (setf (mevedel-session-specialist-nudge-state ctx) state))))

(defun mevedel-pipeline--ctx-turn-count (ctx session)
  "Return CTX's turn count, falling back to SESSION."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (or (mevedel-agent-invocation-turn-count ctx) 0))
   ((mevedel-session-p ctx) (or (mevedel-session-turn-count ctx) 0))
   ((mevedel-session-p session) (or (mevedel-session-turn-count session) 0))
   (t 0)))

(defun mevedel-pipeline--tool-name-present-p (ctx names)
  "Return non-nil when CTX has any deferred or injected tool in NAMES."
  (or (cl-some (lambda (entry) (member (cadr (car entry)) names))
               (mevedel-pipeline--ctx-deferred-set ctx))
      (cl-some (lambda (entry) (member (car entry) names))
               (mevedel-pipeline--ctx-deferred-injected ctx))))

(defun mevedel-pipeline--nudge-context (context)
  "Return the context whose nudge throttle should be updated."
  (or (plist-get context :invocation)
      (plist-get context :session)))

(defun mevedel-pipeline--nudge-allowed-p (context family)
  "Return non-nil when FAMILY may nudge for CONTEXT, recording use."
  (if-let* ((ctx (mevedel-pipeline--nudge-context context)))
      (let* ((session (plist-get context :session))
             (turn (mevedel-pipeline--ctx-turn-count ctx session))
             (state (copy-sequence (mevedel-pipeline--ctx-nudge-state ctx)))
             (entry (plist-get state family))
             (count (or (plist-get entry :count) 0))
             (last-turn (plist-get entry :turn)))
        (when (and (< count mevedel-pipeline--specialist-nudge-max-per-family)
                   (not (equal last-turn turn)))
          (setq state (plist-put state family (list :count (1+ count) :turn turn)))
          (mevedel-pipeline--set-ctx-nudge-state ctx state)
          t))
    t))

(defun mevedel-pipeline--code-path-p (path)
  "Return non-nil when PATH looks like a code file."
  (and (stringp path)
       (member (downcase (or (file-name-extension path) ""))
               mevedel-pipeline--code-file-extensions)))

(defun mevedel-pipeline--code-glob-p (glob)
  "Return non-nil when GLOB appears scoped to code files."
  (and (stringp glob)
       (cl-some (lambda (ext)
                  (string-match-p
                   (concat "\\." (regexp-quote ext) "\\_>") glob))
                mevedel-pipeline--code-file-extensions)))

(defun mevedel-pipeline--grep-result-code-p (result)
  "Return non-nil when RESULT contains code-file paths."
  (and (stringp result)
       (cl-some (lambda (line)
                  (let ((path (car (split-string line ":" t))))
                    (mevedel-pipeline--code-path-p path)))
                (split-string result "\n" t))))

(defun mevedel-pipeline--grep-code-target-p (args result)
  "Return non-nil when Grep ARGS/RESULT point at code files."
  (or (member (downcase (or (plist-get args :type) ""))
              mevedel-pipeline--code-rg-types)
      (mevedel-pipeline--code-glob-p (plist-get args :glob))
      (mevedel-pipeline--code-path-p (plist-get args :path))
      (mevedel-pipeline--grep-result-code-p result)))

(defun mevedel-pipeline--identifier-like-pattern-p (pattern)
  "Return non-nil when PATTERN looks like a specific code identifier."
  (and (stringp pattern)
       (string-match-p "\\`[[:alpha:]_][[:alnum:]_-]*\\'" pattern)
       (or (string-match-p "[-_]" pattern)
           (let ((case-fold-search nil))
             (string-match-p "[[:lower:]][[:upper:]]\\|[[:upper:]][[:lower:]]"
                             pattern)))))

(defun mevedel-pipeline--structural-code-pattern-p (pattern)
  "Return non-nil when PATTERN looks like a structural code search."
  (and (stringp pattern)
       (member (downcase pattern)
               mevedel-pipeline--structural-code-patterns)))

(defun mevedel-pipeline--grep-comment-result-p (result)
  "Return non-nil when Grep RESULT appears to contain only comment hits."
  (and (stringp result)
       (let ((lines (split-string result "\n" t)))
         (and lines
              (cl-every
               (lambda (line)
                 (let ((text (if (string-match
                                  "\\`[^:\n]+:[0-9]+:\\(?:[0-9]+:\\)?\\(.*\\)\\'"
                                  line)
                                 (match-string 1 line)
                               line)))
                   (string-match-p
                    "\\`[[:space:]]*\\(?:;;\\|//\\|#\\|/\\*\\|\\*\\|;\\)"
                    text)))
               lines)))))

(defun mevedel-pipeline--read-text-range-p (args)
  "Return non-nil when Read ARGS request a non-default text range."
  (let ((offset (plist-get args :offset))
        (limit (plist-get args :limit)))
    (or (and offset (not (equal offset 0)))
        (and limit (not (member limit '(0 2000)))))))

(defun mevedel-pipeline--read-media-range-p (args)
  "Return non-nil when Read ARGS request explicit media handling."
  (or (let ((pages (plist-get args :pages)))
        (and pages (not (equal pages ""))))
      (cl-some (lambda (key)
                 (let ((value (plist-get args key)))
                   (and value (not (equal value 0)))))
               '(:max_width :max_height :max_tokens))))

(defun mevedel-pipeline--read-exact-range-p (args)
  "Return non-nil when Read ARGS request an exact text/media range."
  (or (mevedel-pipeline--read-text-range-p args)
      (mevedel-pipeline--read-media-range-p args)))

(defun mevedel-pipeline--specialist-capabilities (session)
  "Return specialist capabilities for SESSION, or nil if unavailable."
  (when (and session (fboundp 'mevedel-reminders-specialist-capabilities))
    (mevedel-reminders-specialist-capabilities session)))

(defun mevedel-pipeline--family-applicable-p (context caps family names)
  "Return non-nil when specialist FAMILY with NAMES applies for CONTEXT."
  (or (plist-get caps family)
      (mevedel-pipeline--tool-name-present-p
       (mevedel-pipeline--nudge-context context) names)))

(defun mevedel-pipeline--specialist-load-text (context names query)
  "Return a ToolSearch hint for NAMES/QUERY when those tools are deferred."
  (let ((ctx (mevedel-pipeline--nudge-context context)))
    (when (cl-some (lambda (entry) (member (cadr (car entry)) names))
                   (mevedel-pipeline--ctx-deferred-set ctx))
      (format " If the tool is not callable, use ToolSearch(query=\"%s\", load=true); loaded tools are available now for your next tool call."
              query))))

(defun mevedel-pipeline--grep-specialist-nudges (context args result caps)
  "Return bounded specialist nudges for a Grep call."
  (let ((pattern (plist-get args :pattern))
        (code-result-p
         (and (mevedel-pipeline--grep-code-target-p args result)
              (not (mevedel-pipeline--grep-comment-result-p result))
              (not (string-prefix-p "No matches found" result))))
        nudges)
    (when code-result-p
      (when (and (mevedel-pipeline--identifier-like-pattern-p pattern)
                 (mevedel-pipeline--family-applicable-p
                  context caps :xref
                  '("XrefReferences" "XrefDefinitions"))
                 (mevedel-pipeline--nudge-allowed-p context :xref))
        (push (concat "For precise code symbol references, prefer `XrefReferences(identifier, file_path)'; for definitions or name discovery, prefer `XrefDefinitions(pattern, file_path)'."
                      (or (mevedel-pipeline--specialist-load-text
                           context '("XrefReferences" "XrefDefinitions")
                           "xref")
                          ""))
              nudges))
      (when (and (mevedel-pipeline--identifier-like-pattern-p pattern)
                 (mevedel-pipeline--code-path-p (plist-get args :path))
                 (mevedel-pipeline--family-applicable-p
                  context caps :imenu '("Imenu"))
                 (mevedel-pipeline--nudge-allowed-p context :imenu))
        (push (concat "For a symbol outline in one known code file, prefer `Imenu(file_path)' over grepping the file for structure."
                      (or (mevedel-pipeline--specialist-load-text
                           context '("Imenu") "imenu")
                          ""))
              nudges))
      (when (and (mevedel-pipeline--structural-code-pattern-p pattern)
                 (mevedel-pipeline--family-applicable-p
                  context caps :treesitter '("Treesitter"))
                 (mevedel-pipeline--nudge-allowed-p context :treesitter))
        (push (concat "For syntax-node, AST, parent/child, or other structural code questions, prefer `Treesitter(file_path, line/column or whole_file)' over text search."
                      (or (mevedel-pipeline--specialist-load-text
                           context '("Treesitter") "treesitter")
                          ""))
              nudges)))
    (nreverse nudges)))

(defun mevedel-pipeline--read-specialist-nudges (context args result caps)
  "Return bounded specialist nudges for a Read call."
  (let ((path (plist-get args :file_path))
        nudges)
    (when (and (mevedel-pipeline--code-path-p path)
               (not (mevedel-pipeline--read-exact-range-p args))
               (not (string-prefix-p "File " result))
               (not (string-prefix-p "Error:" result)))
      (when (and (mevedel-pipeline--family-applicable-p
                  context caps :imenu '("Imenu"))
                 (mevedel-pipeline--nudge-allowed-p context :imenu))
        (push (concat "If your next step is symbol or structure discovery in this file, prefer `Imenu(file_path)' over reading or grepping more of the file."
                      (or (mevedel-pipeline--specialist-load-text
                           context '("Imenu") "imenu")
                          ""))
              nudges))
      (when (and (mevedel-pipeline--family-applicable-p
                  context caps :xref
                  '("XrefReferences" "XrefDefinitions"))
                 (mevedel-pipeline--nudge-allowed-p context :xref))
        (push (concat "For follow-up code symbol lookup, prefer `XrefReferences(identifier, file_path)' or `XrefDefinitions(pattern, file_path)' instead of Grep."
                      (or (mevedel-pipeline--specialist-load-text
                           context '("XrefReferences" "XrefDefinitions")
                          "xref")
                          ""))
              nudges))
      (when (and (mevedel-pipeline--family-applicable-p
                  context caps :treesitter '("Treesitter"))
                 (mevedel-pipeline--nudge-allowed-p context :treesitter))
        (push (concat "For syntax-node, AST, parent/child, or other structural questions in this file, prefer `Treesitter(file_path, line/column or whole_file)'."
                      (or (mevedel-pipeline--specialist-load-text
                           context '("Treesitter") "treesitter")
                          ""))
              nudges))
      (when (and (string-suffix-p ".el" path)
                 (mevedel-pipeline--family-applicable-p
                  context caps :elisp-introspection
                  '("function_source" "variable_source"
                    "function_documentation" "variable_documentation"
                    "library_source"))
                 (mevedel-pipeline--nudge-allowed-p
                  context :elisp-introspection))
        (push (concat "For Emacs Lisp loaded-state questions, prefer introspection tools such as `function_source(function)', `variable_source(variable)', documentation/manual tools, or `library_source(library)'."
                      (or (mevedel-pipeline--specialist-load-text
                           context
                           '("function_source" "variable_source"
                             "function_documentation" "variable_documentation"
                             "library_source")
                           "elisp")
                          ""))
              nudges)))
    (nreverse nudges)))

(defun mevedel-pipeline--append-specialist-nudge (result nudges)
  "Append NUDGES to RESULT as one system-reminder block."
  (if nudges
      (concat result
              "\n\n<system-reminder>\n"
              (mapconcat #'identity nudges "\n")
              "\n</system-reminder>")
    result))

(defun mevedel-pipeline--step-specialist-nudges (context next _fail)
  "Append bounded specialist-tool guidance after generic tool calls."
  (let* ((tool (plist-get context :tool))
         (name (and tool (mevedel-tool-name tool)))
         (args (plist-get context :args))
         (result (plist-get context :result))
         (session (plist-get context :session))
         (caps (mevedel-pipeline--specialist-capabilities session))
         nudges)
    (when (and (stringp result)
               (not (string-prefix-p "Error:" result)))
      (pcase name
        ("Grep"
         (setq nudges
               (mevedel-pipeline--grep-specialist-nudges
                context args result caps)))
        ("Read"
         (setq nudges
               (mevedel-pipeline--read-specialist-nudges
                context args result caps)))))
    (if nudges
        (funcall next
                 (plist-put context :result
                            (mevedel-pipeline--append-specialist-nudge
                             result nudges)))
      (funcall next context))))


;;
;;; Step list builder

(defun mevedel-pipeline--build-steps (tool)
  "Build the standard step list for TOOL.

Returns a list of step functions based on TOOL's behavioral flags:
  1. validate            -- always included
  2. permission          -- always included
  3. snapshot            -- skipped if read-only-p
  4. handler             -- always included
  5. render-transform    -- always included; no-op when tool has none
  6. persist             -- included when max-result-size is set
  7. specialist-nudges   -- bounded guidance for generic code tools
  8. attach-render-data  -- always included; no-op when handler returned
                            no render-data
  9. post-tool-hooks     -- always included
  10. persist            -- included when max-result-size is set; bounds
                            hook-updated results
  11. attach-media-data  -- always included; no-op when handler returned
                             no media"
  (let ((steps nil))
    (push #'mevedel-pipeline--step-attach-media-data steps)
    (when (mevedel-tool-max-result-size tool)
      (push #'mevedel-pipeline--step-persist steps))
    (push #'mevedel-pipeline--step-post-tool-hooks steps)
    (push #'mevedel-pipeline--step-attach-render-data steps)
    (push #'mevedel-pipeline--step-specialist-nudges steps)
    (when (mevedel-tool-max-result-size tool)
      (push #'mevedel-pipeline--step-persist steps))
    (push #'mevedel-pipeline--step-render-transform steps)
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

CALLBACK is the async result callback from gptel. ARGS is a plist of
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
         (steps (mevedel-pipeline--build-steps tool))
         (context (list :tool tool :args args
                        :session session :workspace workspace
                        :request request :invocation invocation
                        :tool-use-id tool-use-id
                        :origin
                        (and (fboundp 'mevedel-agent-invocation-p)
                             (mevedel-agent-invocation-p invocation)
                             (mevedel-agent-invocation-agent-id invocation))
                        :buffer dispatch-buffer
                        :default-directory workdir))
         (called nil)
         (once-callback
          (lambda (result)
            (cond
             ((not called)
              (setq called t)
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
