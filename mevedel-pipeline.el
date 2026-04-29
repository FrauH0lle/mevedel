;;; mevedel-pipeline.el -- Tool execution pipeline -*- lexical-binding: t -*-

;;; Commentary:

;; Sequential step-based execution engine for mevedel tools. Each tool
;; invocation runs through a standard pipeline: validate -> permission ->
;; snapshot -> handler -> persist. Tool handlers that need user confirmation
;; of a file change call `mevedel-preview-mode-add-preview' directly;
;; there is no explicit confirm step in the pipeline.
;;
;; The persist step saves oversized results to disk and replaces them
;; with a preview + file path, preventing LLM context overflow from
;; unexpectedly large tool output.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'mevedel-permissions)
(require 'mevedel-structs)

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
(declare-function mevedel-tool--validate-args "mevedel-tool-registry"
                  (tool-name args arg-specs))
(declare-function mevedel-check-permission-async "mevedel-permissions"
                  (tool-name cont &rest args))
(declare-function mevedel-permission--apply-prompt-result
                  "mevedel-permissions" (result tool-name &rest args))

(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-tool-fs'
(declare-function mevedel--snapshot-file-if-needed "mevedel-tool-fs" (filepath))

;; `mevedel-tool-ui'
(require 'mevedel-permission-queue)
(declare-function mevedel-permission--prompt-async "mevedel-tool-ui"
                  (tool-name path include-always cont))

;; `mevedel-tools'
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--ctx-record-used "mevedel-tools" (ctx name))


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

(defun mevedel-pipeline--persist-result (result tool workspace)
  "Save RESULT to disk and return a preview string.

TOOL is the `mevedel-tool' whose result exceeded its size limit.
WORKSPACE provides the `.mevedel/' state directory.  The full result
is written to `.mevedel/tool-results/' and the return value is a
preview-sized replacement for the LLM context."
  (let* ((dir (file-name-concat (mevedel-workspace-state-dir workspace)
                                "tool-results"))
         (name (mevedel-tool-name tool))
         (_ (make-directory dir t))
         (file (make-temp-file (file-name-concat dir (concat name "-"))
                               nil ".txt"))
         (preview-end (min (length result) mevedel-pipeline--preview-size))
         ;; Cut at last newline within preview range to avoid mid-line breaks
         (cut (let ((nl (cl-position ?\n result :from-end t :end preview-end)))
                (if (and nl (> nl (/ preview-end 2))) nl preview-end)))
         (has-more (< cut (length result))))
    (write-region result nil file nil 'silent)
    (concat "<persisted-output>\n"
            (format "Output too large (%d chars). Full output saved to: %s\n\n"
                    (length result) file)
            (format "Preview (first %d chars):\n" cut)
            (substring result 0 cut)
            (if has-more "\n...\n" "\n")
            "</persisted-output>")))

(defun mevedel-pipeline--truncate-result (result tool)
  "Truncate RESULT to a preview without persisting to disk.

Used when the result exceeds the size limit but no workspace is
available for file persistence.  TOOL is used only for the tool name
in the message."
  (let* ((preview-end (min (length result) mevedel-pipeline--preview-size))
         (cut (let ((nl (cl-position ?\n result :from-end t :end preview-end)))
                (if (and nl (> nl (/ preview-end 2))) nl preview-end)))
         (has-more (< cut (length result))))
    (concat (format "Output too large (%d chars) and no workspace available \
to persist full result (tool: %s).\n\n"
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
      (funcall callback (plist-get context :result))
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
                (funcall callback
                         (mevedel-pipeline--format-failure reason))))))
      (condition-case err
          (funcall step context next-cont fail-cont)
        (mevedel-validation-error
         (funcall callback
                  (mevedel-pipeline--format-failure
                   (or (cadr err) "Validation error"))))
        (mevedel-permission-denied
         (funcall callback
                  (mevedel-pipeline--format-failure
                   (if (cadr err)
                       (format "Permission denied: %s" (cadr err))
                     "Permission denied"))))
        (mevedel-pipeline-error
         (funcall callback
                  (mevedel-pipeline--format-failure
                   (or (cadr err) "Pipeline error"))))
        (error
         (funcall callback
                  (mevedel-pipeline--format-failure
                   (error-message-string err))))))))


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
         (get-path-fn (mevedel-tool-get-path tool))
         (path (when get-path-fn
                 (ignore-errors (funcall get-path-fn args))))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace))
         (workspace-root (when workspace
                           (ignore-errors
                             (mevedel-workspace-root workspace))))
         (persistent-rules (when workspace
                             (mevedel-permission--load-persistent-rules
                              workspace)))
         (session-rules (when session
                          (mevedel-session-permission-rules session)))
         (request (and (boundp 'mevedel--current-request)
                       mevedel--current-request))
         (request-rules (and request
                             (mevedel-request-skill-permission-rules request)))
         ;; spec: invocation rules come from the active sub-agent
         ;; invocation when one is in scope.  At pipeline-step time
         ;; that's `mevedel--agent-invocation' (set buffer-locally
         ;; on agent buffers by
         ;; `mevedel-agent-exec--allocate-agent-buffer').  The
         ;; `boundp' guard keeps the read safe in contexts that
         ;; have not loaded the agent runtime.
         (invocation (and (boundp 'mevedel--agent-invocation)
                          mevedel--agent-invocation))
         (invocation-rules
          (and invocation
               (mevedel-agent-invocation-skill-permission-rules invocation)))
         (mode (when session (mevedel-session-permission-mode session))))
    ;; Tripwire: a non-read-only tool reaching the permission step
    ;; without a session in context means session-scoped rules and
    ;; the active permission mode are silently invisible -- the
    ;; chain falls back to the defcustom-scoped
    ;; `mevedel-permission-rules' / `mevedel-permission-mode' alone.
    ;; That silent fallback is the actual hazard.  Make the
    ;; contract violation visible so it surfaces in *Warnings*
    ;; instead of producing surprising deny / allow outcomes.
    (unless session
      (display-warning
       'mevedel
       (format "Permission step for %s ran with no session in \
context; falling back to defcustom defaults.  Session-scoped \
rules and the active permission mode are not being consulted.  \
This usually means the tool was dispatched from a buffer whose \
`mevedel--session' was not set; in production that should not \
happen for a non-read-only tool."
               tool-name)
       :warning))
    (mevedel-check-permission-async
     tool-name
     (lambda (raw-outcome)
       (mevedel-pipeline--dispatch-permission-outcome
        raw-outcome context next fail
        :tool-name tool-name :path path :session session
        :workspace workspace :workspace-root workspace-root))
     :tool-struct tool
     :path path
     :content args
     :invocation-rules invocation-rules
     :request-rules request-rules
     :session-rules session-rules
     :persistent-rules persistent-rules
     :mode mode
     :workspace-root workspace-root)))

(cl-defun mevedel-pipeline--dispatch-permission-outcome
    (outcome context next fail
             &key tool-name path session workspace workspace-root)
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
  (pcase outcome
    ;; `ask' arrives from the decision chain itself (steps 3/7/8/9) or
    ;; from a tool slot that defers to the generic prompt.  Drive the
    ;; prompt with workspace-boundary rule shaping identical to the
    ;; sync pipeline's.
    ('ask
     (let* ((workspace-boundary-p
             (and path workspace-root
                  (not (string-prefix-p
                        (file-name-as-directory
                         (expand-file-name workspace-root))
                        (expand-file-name path)))))
            (rule-tool (if workspace-boundary-p "*" tool-name))
            (rule-path (if workspace-boundary-p
                           (concat (file-name-directory
                                    (expand-file-name path))
                                   "**")
                         path)))
       ;; Spec 23: route through the session permission queue rather
       ;; than calling the prompt-async overlay directly.  When the
       ;; queue is empty, the head is rendered immediately and the
       ;; UX is identical to the prior path; when non-empty, the
       ;; entry waits its turn.  Either way the callback receives
       ;; the same prompt-outcome vocabulary as before.
       ;;
       ;; Coalesce-time re-evaluation goes back through
       ;; `mevedel-check-permission' which itself handles the
       ;; protected-path / deny-precedence rules from the decision
       ;; chain — so the queue doesn't need to store a
       ;; protected-path flag explicitly.
       (mevedel-permission--enqueue
        (list :kind 'generic
              :tool-name tool-name
              :args (plist-get context :content)
              :specifier-value rule-path
              :include-always (not (null workspace))
              :workspace workspace
              :origin (or (plist-get context :origin) "main")
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
                             ((or 'allow-once 'allow-session 'always-allow
                                  'deny-once 'deny-session)
                              (mevedel-permission--apply-prompt-result
                               prompt-outcome rule-tool session workspace
                               rule-path))
                             ((or 'allow 'deny 'aborted) prompt-outcome)
                             (other other))))
                      (mevedel-pipeline--dispatch-permission-outcome
                       collapsed context next fail
                       :tool-name tool-name :path path :session session
                       :workspace workspace :workspace-root workspace-root))
                  (error
                   (funcall fail (error-message-string err)))))))))
    ((or 'allow 'approve 'implement 'implement-clear)
     (funcall next context))
    ('deny
     (funcall fail "Permission denied"))
    (`(deny . ,reason)
     (funcall fail (format "Permission denied: %s" reason)))
    (`(feedback . ,text)
     (funcall fail (format "Permission denied: %s" text)))
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

(defun mevedel-pipeline--step-handler (context next _fail)
  "Run the tool handler.

For async tools (async-p is non-nil), the handler receives a callback as
its first argument followed by the args plist.  For sync tools, the
handler receives just the args plist and returns the result directly.

A handler may return either a plain result string (legacy shape) or a
plist of the form (:result STRING :render-data DATA).  In the latter
case, the result string flows through the rest of the pipeline and the
render-data is carried alongside in CONTEXT so
`mevedel-pipeline--step-attach-render-data' can embed it adjacent to
the result for the view-buffer parser.

FAIL is unused -- handler-owned overlays (RequestAccess, PresentPlan)
embed their failure modes in the result string (`Error: ...').
Adding a `fail' channel to the handler step would force every async
tool handler to take one; keeping them string-shaped preserves the
existing contract.  See spec 20 \"Cancel channel for handler-step-owned
overlays\".

Sets `:result' and `:render-data' in CONTEXT for downstream steps;
NEXT is called on success."
  (let* ((tool (plist-get context :tool))
         (handler (mevedel-tool-handler tool))
         (args (plist-get context :args))
         (store (lambda (raw)
                  (let ((split (mevedel-pipeline--split-handler-return raw)))
                    (plist-put
                     (plist-put context :result (car split))
                     :render-data (cdr split))))))
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
             (prin1-to-string render-data))
           "\n" mevedel-pipeline--render-data-close "\n")
   'invisible t))

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

(defun mevedel-pipeline--find-render-data-block-by-agent-id (agent-id)
  "Return (BEG . END) of the first render-data block whose plist
`:agent-id' is AGENT-ID, or nil.
Searches the current buffer from `point-min'.  Used by spec 23's
background handle patch path to locate the block whose hidden plist
should be updated when a sub-agent's status changes."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((re (mevedel-pipeline--render-data-regexp))
            (found nil))
        (while (and (not found)
                    (re-search-forward re nil t))
          (let* ((beg (match-beginning 0))
                 (end (match-end 0))
                 (raw (buffer-substring-no-properties beg end))
                 (parsed (mevedel-pipeline-extract-render-data raw))
                 (plist (cdr parsed)))
            (when (and (listp plist)
                       (equal (plist-get plist :agent-id) agent-id))
              (setq found (cons beg end)))))
        found))))

(defun mevedel-pipeline--patch-render-data-block (beg end new-plist)
  "Replace the render-data block between BEG and END with NEW-PLIST.
Preserves the surrounding text and the hidden-block delimiters.
The new block is formatted via
`mevedel-pipeline--format-render-data-block' so it stays
round-trippable through `mevedel-pipeline-extract-render-data'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char beg)
      (delete-region beg end)
      (insert (mevedel-pipeline--format-render-data-block new-plist)))))

(defun mevedel-pipeline--decode-function-call-arguments (new-prompt)
  "Return NEW-PROMPT with Responses API function-call arguments decoded.

The streaming Responses parser reinjects function-call history with
`:arguments' set to `(gptel--json-encode ARGS)'.  When ARGS contains
non-ASCII text, that can be a unibyte UTF-8 string, which Emacs'
native JSON serializer rejects later with `json-value-p'.  Normalize
only function-call `:arguments' strings, matching gptel's own
`decode-coding-string' treatment in its transcript parsing paths."
  (let ((items (if (keywordp (car-safe new-prompt))
                   (list new-prompt)
                 new-prompt)))
    (dolist (item items)
      (when (and (listp item)
                 (equal (plist-get item :type) "function_call"))
        (let ((arguments (plist-get item :arguments)))
          (when (and (stringp arguments)
                     (not (multibyte-string-p arguments)))
            (plist-put item :arguments
                       (decode-coding-string arguments 'utf-8 t)))))))
  new-prompt)

(defun mevedel--inject-prompt-decode-function-call-arguments-advice
    (orig-fun backend data new-prompt &optional position)
  "Decode function-call `:arguments' before `gptel--inject-prompt'.

This is intentionally scoped to prompt injection rather than tool
handlers: the problematic value is the serialized function-call
argument history that gptel reinjects for continuation requests, not
the live tool argument plist delivered to mevedel handlers."
  (funcall orig-fun backend data
           (mevedel-pipeline--decode-function-call-arguments new-prompt)
           position))

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
        (progn
          (dolist (tc tool-use)
            (let* ((orig (plist-get tc :result))
                   (cleaned (and (stringp orig)
                                 (mevedel-pipeline--strip-render-data-blocks
                                  orig))))
              (when (and cleaned (not (equal orig cleaned)))
                (push (cons tc orig) saved)
                (plist-put tc :result cleaned))))
          (funcall orig-fun backend tool-use))
      (dolist (entry saved)
        (plist-put (car entry) :result (cdr entry))))))

(defun mevedel-pipeline-install-tool-result-scrubber ()
  "Install gptel interop advice for tool-result continuation paths."
  (advice-add 'gptel--inject-prompt :around
              #'mevedel--inject-prompt-decode-function-call-arguments-advice)
  (advice-add 'gptel--parse-tool-results :around
              #'mevedel--parse-tool-results-scrub-advice))

(defun mevedel-pipeline-uninstall-tool-result-scrubber ()
  "Remove gptel interop advice for tool-result continuation paths."
  (advice-remove 'gptel--inject-prompt
                 #'mevedel--inject-prompt-decode-function-call-arguments-advice)
  (advice-remove 'gptel--parse-tool-results
                 #'mevedel--parse-tool-results-scrub-advice))

(defun mevedel-pipeline-extract-render-data (result-string)
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
          (cons result-string nil)
        (let* ((payload-start (+ open (length mevedel-pipeline--render-data-open)))
               (close (string-search mevedel-pipeline--render-data-close
                                     result-string payload-start)))
          (if (null close)
              (cons result-string nil)
            (let* ((payload (string-trim
                             (substring result-string payload-start close)))
                   (data (condition-case _
                             (read payload)
                           (error :mevedel-parse-failed)))
                   (trail-end (+ close
                                 (length mevedel-pipeline--render-data-close))))
              (if (eq data :mevedel-parse-failed)
                  (cons result-string nil)
                (cons (string-trim-right
                       (concat (substring result-string 0 open)
                               (substring result-string trail-end)))
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

(defun mevedel-pipeline--step-persist (context next _fail)
  "Persist oversized tool results to disk.

If the tool has a `max-result-size' and the string result exceeds the
effective limit (the minimum of the tool value and
`mevedel-pipeline--default-max-result-size'), saves the full result to
`.mevedel/tool-results/' and replaces :result with a preview.

When no workspace is available, the result is still truncated to the
preview size to prevent context overflow -- only the file write is
skipped.

Skips entirely when the result is not a string or is an error message.
CONTEXT must contain :tool and :result.  NEXT is called with the
possibly-updated context."
  (let* ((tool (plist-get context :tool))
         (result (plist-get context :result))
         (max-size (mevedel-tool-max-result-size tool))
         (effective (when max-size
                      (min max-size mevedel-pipeline--default-max-result-size))))
    (if (or (null effective)
            (null result)
            (not (stringp result))
            (string-prefix-p "Error:" result)
            (<= (length result) effective))
        (funcall next context)
      ;; Result exceeds limit -- persist or truncate.  Workspace was
      ;; captured into the context at `mevedel-pipeline-run-tool'
      ;; entry; do not re-read it from `current-buffer' here because
      ;; the handler may have run (and called back) from inside a
      ;; `with-temp-buffer' wrapper.
      (let ((workspace (plist-get context :workspace)))
        (funcall next
                 (plist-put context :result
                            (if workspace
                                (mevedel-pipeline--persist-result
                                 result tool workspace)
                              (mevedel-pipeline--truncate-result
                               result tool))))))))


;;
;;; Step list builder

(defun mevedel-pipeline--build-steps (tool)
  "Build the standard step list for TOOL.

Returns a list of step functions based on TOOL's behavioral flags:
  1. validate            -- always included
  2. permission          -- always included
  3. snapshot            -- skipped if read-only-p
  4. handler             -- always included
  5. persist             -- included when max-result-size is set
  6. attach-render-data  -- always included; no-op when handler returned
                            no render-data"
  (let ((steps nil))
    (push #'mevedel-pipeline--step-attach-render-data steps)
    (when (mevedel-tool-max-result-size tool)
      (push #'mevedel-pipeline--step-persist steps))
    (push #'mevedel-pipeline--step-handler steps)
    (unless (mevedel-tool-read-only-p tool)
      (push #'mevedel-pipeline--step-snapshot steps))
    (push #'mevedel-pipeline--step-permission steps)
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
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (workspace
          (cond
           (session (mevedel-session-workspace session))
           ((and (boundp 'mevedel--workspace) mevedel--workspace))))
         (steps (mevedel-pipeline--build-steps tool))
         (context (list :tool tool :args args
                        :session session :workspace workspace))
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
