;;; mevedel-agent-exec.el -- Sub-agent task runner -*- lexical-binding: t -*-

;;; Commentary:

;; Mevedel-owned sub-agent request runner.  `mevedel-agent-runtime' coordinates
;; dispatch and lifecycle state; this module owns the gptel request FSM and
;; streaming callback contract.  `mevedel-agent-conversation' owns retained
;; buffers, transcript persistence, and live conversation metadata.
;;
;; The extraction fixes a streaming-truncation bug present in the upstream
;; `gptel-agent--task': that function's `(pred stringp)' branch fires its main
;; callback on every streamed chunk and has no `t' branch, so gptel's
;; stream-complete signal (`gptel-request.el' line 2864) is silently dropped.
;; Since gptel's tool-call commit path locks in the first delivered value, the
;; parent agent only ever sees the first chunk of a sub-agent's final response.
;; The runner here accumulates on string chunks and fires exactly once on `t',
;; so the full response reaches the parent.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel)
  ;; Required for the cl-defstruct `setf' expanders of `gptel-fsm-*' slots.
  ;; Without this, `(setf (gptel-fsm-handlers ...) ...)' below does not expand
  ;; to its slot-setter at compile time and falls back to looking up a
  ;; nonexistent `(setf gptel-fsm-handlers)' function at runtime.
  (require 'gptel-request)
  ;; Required for the cl-defstruct `setf' expanders on
  ;; `mevedel-agent-invocation-*' slots referenced below.
  (require 'mevedel-agents))

(require 'mevedel-models)

;; `gptel'
(declare-function gptel--apply-preset "ext:gptel" (preset &optional setter))
(declare-function gptel--handle-abort "ext:gptel" (fsm))
(declare-function gptel--handle-error "ext:gptel" (fsm))
(declare-function gptel--handle-post-insert "ext:gptel" (fsm))
(declare-function gptel--handle-token-usage "ext:gptel" (fsm))
(declare-function gptel--preset-syms "ext:gptel" (preset))
(declare-function gptel--update-status "ext:gptel"
                  (msg &optional face))
(declare-function gptel--update-tool-ask "ext:gptel" (fsm))
(declare-function gptel--update-tool-call "ext:gptel" (fsm))
(declare-function gptel-get-preset "ext:gptel" (name))
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-with-preset "ext:gptel" (name &rest body))
(defvar gptel--tool-preview-alist)
(defvar gptel--fsm-last)
(defvar gptel-send--transitions)

;; `gptel-request'
(declare-function gptel--display-tool-calls "ext:gptel-request" (calls info))
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel--handle-post-tool "ext:gptel-request" (fsm))
(declare-function gptel--handle-pre-tool "ext:gptel-request" (fsm))
(declare-function gptel--handle-tool-result "ext:gptel-request" (fsm))
(declare-function gptel--handle-tool-use "ext:gptel-request" (fsm))
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel--transform-add-context "ext:gptel-request" (fsm))
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel-request "ext:gptel-request"
                  (&optional prompt &rest args))
(defvar gptel--num-messages-to-send)
(defvar gptel--request-params)
(defvar gptel--schema)
(defvar gptel-backend)
(defvar gptel-cache)
(defvar gptel-context)
(defvar gptel-include-reasoning)
(defvar gptel-max-tokens)
(defvar gptel-mode)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-system-prompt)
(defvar gptel-temperature)
(defvar gptel-tools)
(defvar gptel-track-media)
(defvar gptel-track-response)
(defvar gptel-use-context)
(defvar gptel-use-curl)
(defvar gptel-use-tools)

;; `gptel-agent-tools'
(declare-function gptel-agent--confirm-overlay "ext:gptel-agent-tools"
                  (from to &optional no-hide))
(defvar gptel-agent-preset)

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-configure
                  "mevedel-agent-conversation" (invocation &optional buffer))
(declare-function mevedel-agent-conversation-final-response
                  "mevedel-agent-conversation" (invocation))
(declare-function mevedel-agent-conversation-record-activity
                  "mevedel-agent-conversation"
                  (invocation item &optional suppress-rerender))
(declare-function mevedel-agent-conversation-save
                  "mevedel-agent-conversation" (invocation &optional deferred))

;; `mevedel-agents'
(declare-function mevedel-agent-configuration--create
                  "mevedel-agents" (&rest args))
(declare-function mevedel-agent-configuration-p
                  "mevedel-agents" (cl-x))
(declare-function mevedel-agent-configuration-request-locals
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-freeze "mevedel-agents" (agent))
(declare-function mevedel-agent-invocation-activity
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-frozen-configuration
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-runtime-fsm
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-model-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))

;; `mevedel-compact'
(declare-function mevedel--compact-handle-agent-wait
                  "mevedel-compact" (fsm))
(declare-function mevedel--compact-record-token-baseline
                  "mevedel-compact" (fsm))

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(defvar mevedel-model-tiers)
(defvar mevedel-model-workloads)

;; `mevedel-tools'
(declare-function mevedel-tools--handle-agent-roster-inject
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-agent-turn-terminal
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-message-inject
                  "mevedel-tools" (fsm))

(defvar mevedel-agent-exec-debug nil
  "Non-nil enables request-driver lifecycle diagnostics.")

;; `mevedel-view-agent'
(declare-function mevedel-view-agent-live-transcript-finalize
                  "mevedel-view-agent" (invocation))


(defun mevedel-agent-exec--error-reason-from-info (info)
  "Extract a short human reason string from gptel-fsm INFO, or nil.
Assembles a single-line reason from the HTTP `:status' and the `:error'
value (which may be a string or a plist with `:type' / `:message').
Truncated at 200 characters."
  (when (listp info)
    (let* ((status (plist-get info :status))
           (err (plist-get info :error))
           (parts nil))
      (when (and status (stringp status) (not (string-empty-p status)))
        (push (string-trim status) parts))
      (cond
       ((stringp err)
        (push (string-trim err) parts))
       ((listp err)
        (when-let* ((type (plist-get err :type)))
          (push (string-trim (format "%s" type)) parts))
        (when-let* ((msg (plist-get err :message)))
          (push (string-trim (format "%s" msg)) parts))))
      (when parts
        (let* ((joined (mapconcat #'identity (nreverse parts) ": "))
               (max 200))
          (if (> (length joined) max)
              (concat (substring joined 0 max) "...")
            joined))))))

(defun mevedel-agent-exec--error-reason-from-fsm (fsm)
  "Extract a short human reason string from FSM's `:error' info, or nil.
Reads `gptel-fsm-info' and delegates to
`mevedel-agent-exec--error-reason-from-info'."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (mevedel-agent-exec--error-reason-from-info (gptel-fsm-info fsm))))

(defun mevedel-agent-exec--task-preview-setup (arg-values _info)
  "Tool-preview renderer for the Agent tool.

Called by gptel during tool preview for each Agent call to format the
call's (TYPE DESCRIPTION PROMPT) argument list inline.  ARG-VALUES is the
positional argument list; the second plist is the tool call info, unused
here.

Delegates the confirmation-overlay wrapping to
`gptel-agent--confirm-overlay' for now -- that helper is outside the
sub-agent-runtime extraction scope and replacing it earns little."
  (pcase-let ((from (point))
              (`(,type ,desc ,prompt) arg-values))
    (insert "("
            (propertize "Agent " 'font-lock-face 'font-lock-keyword-face)
            (propertize (prin1-to-string type)
                        'font-lock-face 'font-lock-escape-face)
            " " (propertize (prin1-to-string desc)
                            'font-lock-face
                            '(:inherit font-lock-constant-face :inherit bold))
            "\n" (propertize (prin1-to-string prompt)
                             'line-prefix "  "
                             'wrap-prefix "  "
                             'font-lock-face 'font-lock-constant-face)
            ")\n\n")
    (gptel-agent--confirm-overlay from (point) t)))


;;
;;; FSM handler table

(defun mevedel-agent-exec--invocation-from-info (info)
  "Return the `mevedel-agent-invocation' recorded on INFO, or nil."
  (plist-get info :mevedel-agent-invocation))

(defun mevedel-agent-exec--invocation-from-fsm (fsm)
  "Return the `mevedel-agent-invocation' for FSM, or nil."
  (when fsm
    (mevedel-agent-exec--invocation-from-info (gptel-fsm-info fsm))))

(defun mevedel-agent-exec--handle-tret-save (fsm)
  "Schedule an agent-buffer save for FSM after tool-result handling.

Long tool loops can run many WAIT/TOOL/TRET cycles between two DONE
  events.  Saving through a debounce keeps recent output durable without
running Org save machinery synchronously on every tool boundary."
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (mevedel-agent-conversation-save inv t)))

(defun mevedel-agent-exec--handle-wait-activity (fsm)
  "Record a sparse waiting activity item for FSM."
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (unless (eq (plist-get (car (last (mevedel-agent-invocation-activity inv)))
                           :type)
                'waiting)
      (mevedel-agent-conversation-record-activity
       inv '(:type waiting :summary "waiting")))))

(defun mevedel-agent-exec--handle-done-save (fsm)
  "Run gptel's post-insert path so `gptel-post-response-functions' fires.

The current sub-agent FSM table had no DONE entry; this hook delegates
to `gptel--handle-post-insert' and then triggers an explicit transcript
save (for completeness; the post-response hook also calls the save
helper buffer-locally)."
  (when (fboundp 'gptel--handle-post-insert)
    (condition-case _ (gptel--handle-post-insert fsm) (error nil)))
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (mevedel-agent-conversation-save inv)))

(defun mevedel-agent-exec--handle-abort-save (fsm)
  "Drive gptel's abort path for FSM."
  (when (fboundp 'gptel--handle-abort)
    (condition-case _ (gptel--handle-abort fsm) (error nil))))

(defun mevedel-agent-exec--handle-errs-save (fsm)
  "Run gptel's error path for FSM and finalize the transcript as `error'.

`gptel-post-response-functions' fires from `gptel--handle-error', which
writes an error region into the buffer; persisting that output is the
audit log's job.

Captures a short reason (HTTP status + error type/message) onto the
invocation's `terminal-reason' slot before finalize so the parent's
render-data badge can show e.g. `✗ error · 429: rate_limit_error'."
  (when (fboundp 'gptel--handle-error)
    (condition-case _ (gptel--handle-error fsm) (error nil)))
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (let ((fallback-partial (mevedel-agent-conversation-final-response inv)))
      (when-let* ((reason (mevedel-agent-exec--error-reason-from-fsm fsm)))
        (setf (mevedel-agent-invocation-terminal-reason inv) reason))
      (let* ((info (gptel-fsm-info fsm))
             (terminal-callback
              (plist-get info :mevedel-agent-terminal-callback)))
        (when (functionp terminal-callback)
          (funcall terminal-callback
                   (list :mevedel-agent-terminal-status 'error
                         :error-details (plist-get info :error)
                         :fallback-partial fallback-partial)))))))

(defvar mevedel-agent-exec--handlers
  `((WAIT ,#'mevedel-tools--handle-agent-roster-inject
     ,#'mevedel-tools--handle-message-inject
     ,#'mevedel-agent-exec--handle-wait-activity
     ,#'mevedel--compact-handle-agent-wait)
    (TPRE ,#'gptel--handle-token-usage
          ,#'mevedel--compact-record-token-baseline
          ,#'gptel--handle-pre-tool
          ,#'gptel--fsm-transition)
    (TOOL ,#'gptel--update-tool-call
          ,#'gptel--handle-tool-use
          ,#'gptel--update-tool-ask)
    (TRET ,#'gptel--handle-post-tool
          ,#'gptel--handle-tool-result
          ,#'mevedel-agent-exec--handle-tret-save)
    (DONE ,#'mevedel--compact-record-token-baseline
          ,#'mevedel-tools--handle-agent-turn-terminal
          ,#'mevedel-agent-exec--handle-done-save)
    (ABRT ,#'mevedel--compact-record-token-baseline
          ,#'mevedel-tools--handle-agent-turn-terminal
          ,#'mevedel-agent-exec--handle-abort-save)
    (ERRS ,#'mevedel--compact-record-token-baseline
          ,#'mevedel-tools--handle-agent-turn-terminal
          ,#'mevedel-agent-exec--handle-errs-save))
  "Handler table for the mevedel sub-agent FSM.

Same shape as `gptel-send--transitions': each entry is `(STATE FN ...)'
where FN is called when the FSM transitions into (or out of) STATE.
Modelled after the upstream `gptel-agent-request--handlers' table.

Additions:

- `WAIT' injects the caller's compact direct-child roster before sampling.
- `TRET' gains `mevedel-agent-exec--handle-tret-save' so transcripts are durable
  across long tool loops (gptel's post-response hook fires only at DONE/ABRT,
  not TRET).
- `DONE' is added with `gptel--handle-post-insert' delegation so
  `gptel-post-response-functions' actually runs in the agent buffer.
- `ABRT' drives the transcript through finalization with status
  `aborted'.")


;;
;;; Request buffer configuration

(defun mevedel-agent-exec--refresh-initial-transcript-state (invocation)
  "Persist INVOCATION after agent request locals have been installed."
  (when (and (mevedel-agent-invocation-p invocation)
             (mevedel-agent-invocation-transcript-relative-path invocation))
    (let ((buf (mevedel-agent-invocation-buffer invocation)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (set-buffer-modified-p t))
        (mevedel-agent-conversation-save invocation)))))

(defun mevedel-agent-exec--policy-for-invocation (agent-type invocation)
  "Return resolved model policy for AGENT-TYPE and INVOCATION.

Skill-scoped model and effort policy applies to direct skill dispatches."
  (let ((selector
         (and invocation
              (mevedel-agent-invocation-skill-model-override invocation)))
        (effort
         (and invocation
              (mevedel-agent-invocation-skill-effort-override invocation))))
    (mevedel-model-resolve-workload agent-type selector effort)))

(defun mevedel-agent-exec--request-preset (agent-type invocation)
  "Return the effective request preset for AGENT-TYPE and INVOCATION."
  (let ((agent-spec
         (unless (equal agent-type "default")
           (when-let* ((agent (mevedel-agent-invocation-agent invocation)))
             (cdr (mevedel-agent-to-gptel-spec agent))))))
    (nconc (list :use-tools t :context nil)
           (and gptel-agent-preset
                (copy-sequence
                 (cond
                  ((symbolp gptel-agent-preset)
                   (gptel-get-preset gptel-agent-preset))
                  ((listp gptel-agent-preset) gptel-agent-preset)
                  (t (error "Invalid `gptel-agent-preset': %S"
                            gptel-agent-preset)))))
           agent-spec
           (list :include-reasoning gptel-include-reasoning))))

(defconst mevedel-agent-exec--request-local-symbols
  '(gptel--num-messages-to-send
    gptel--request-params
    gptel--schema
    gptel-backend
    gptel-cache
    gptel-context
    gptel-include-reasoning
    gptel-max-tokens
    gptel-mode
    gptel-model
    gptel-reasoning-effort
    gptel-stream
    gptel-system-prompt
    gptel-temperature
    gptel-tools
    gptel-track-media
    gptel-track-response
    gptel-use-context
    gptel-use-curl
    gptel-use-tools
    mevedel-model-tiers
    mevedel-model-workloads)
  "Frozen request state installed buffer-locally for retained agent turns.")

(defun mevedel-agent-exec--request-snapshot (policy)
  "Return one frozen request-local snapshot with model POLICY applied."
  (cl-loop
   for symbol in mevedel-agent-exec--request-local-symbols
   for value = (pcase symbol
                 ('gptel-backend (plist-get policy :backend))
                 ('gptel-model (plist-get policy :model))
                 ('gptel-reasoning-effort (plist-get policy :effort))
                 (_ (and (boundp symbol) (symbol-value symbol))))
   when (and (eq symbol 'gptel-system-prompt) (functionp value))
   do (setq value (funcall value))
   collect (cons symbol (copy-tree value))))

(defun mevedel-agent-exec-freeze-configuration
    (agent-type invocation &optional model-policy)
  "Freeze AGENT-TYPE's effective request configuration for INVOCATION.
MODEL-POLICY may supply a tuple already validated before spawn admission."
  (let ((agent (mevedel-agent-freeze
                (mevedel-agent-invocation-agent invocation))))
    (setf (mevedel-agent-invocation-agent invocation) agent)
    (gptel-with-preset
     (mevedel-agent-exec--request-preset agent-type invocation)
     (let ((policy
            (or model-policy
                (mevedel-agent-exec--policy-for-invocation
                 agent-type invocation))))
       (mevedel-agent-configuration--create
        :agent agent
        :request-locals (mevedel-agent-exec--request-snapshot policy))))))


;;
;;; Task runner

(defun mevedel-agent-exec-run (main-cb agent-type description
                                        invocation agent-buffer)
  "Dispatch a sub-agent task and route its final response to MAIN-CB.

AGENT-TYPE is the registry key (e.g. `\"explorer\"', `\"verifier\"').
DESCRIPTION is a short human-facing label shown in the agent handle.
INVOCATION is the `mevedel-agent-invocation' associated with this task.
It is stashed on the FSM info plist so collaboration-mail, reminder,
and compaction handlers can reach it at ordinary request boundaries.

AGENT-BUFFER is the live per-invocation gptel buffer that holds the
sub-agent's transcript.

Callback contract. Unlike the upstream `gptel-agent--task', which fires
MAIN-CB on every streamed chunk and drops gptel's end-of-stream t
signal, this runner:

  - accumulates streamed string chunks into `partial';
  - fires MAIN-CB exactly once on the t branch, after the sub-agent turn has
    completed and no further tool-use is pending.

The wrapper forwards each insertable
event (string chunks, tool-call / tool-result entries) to gptel's stock
callback before running the mevedel bookkeeping, so the agent buffer
reflects the event before the wrapper's `partial` accumulator acts on
it. Terminal events (t, nil, abort) skip the forward step.

Returns the spawned FSM."
  (require 'mevedel-agent-conversation)
  (unless (mevedel-agent-invocation-p invocation)
    (error "Invalid sub-agent invocation"))
  (unless (buffer-live-p agent-buffer)
    (error "Sub-agent buffer is not live"))
  (let ((frozen
         (mevedel-agent-invocation-frozen-configuration invocation)))
    (unless (mevedel-agent-configuration-p frozen)
      (error "Agent request configuration is not frozen"))
    (let* ((request-locals
            (copy-tree
             (mevedel-agent-configuration-request-locals frozen)))
           (effective-backend (alist-get 'gptel-backend request-locals))
           (effective-model (alist-get 'gptel-model request-locals))
           (info (and (boundp 'gptel--fsm-last)
                      gptel--fsm-last
                      (gptel-fsm-info gptel--fsm-last)))
           (where (or (plist-get info :tracking-marker)
                      (plist-get info :position)
                      (copy-marker (point-max) nil)))
           (partial (format "%s result for task: %s\n\n"
                            (capitalize agent-type) description))
           (fsm (gptel-make-fsm :table gptel-send--transitions
                                :handlers mevedel-agent-exec--handlers))
           (mevedel-cb
            (mevedel-agent-exec--make-callback
             main-cb agent-type description where (list partial))))
      (setf (mevedel-agent-invocation-runtime-fsm invocation) fsm)
      (setf (gptel-fsm-info fsm)
            (plist-put
             (plist-put (gptel-fsm-info fsm)
                        :mevedel-agent-invocation invocation)
             :mevedel-agent-terminal-callback main-cb))
      (gptel--update-status " Calling Agent..." 'font-lock-escape-face)
      ;; Install one dispatch-local copy of the frozen request state before
      ;; gptel reads it from the agent buffer or copies it to a prompt buffer.
      (mevedel-agent-conversation-configure invocation agent-buffer)
      (mevedel-agent-exec--refresh-initial-transcript-state invocation)
      (with-current-buffer agent-buffer
        (goto-char (point-max))
        (gptel-request nil
          :buffer agent-buffer
          :fsm fsm
          :stream gptel-stream
          :system gptel-system-prompt
          :transforms (list #'gptel--transform-add-context)))
      (let* ((req-info (gptel-fsm-info fsm))
             (gptel-cb (plist-get req-info :callback))
             (wrapped
              (mevedel-agent-exec--wrap-callback gptel-cb mevedel-cb)))
        (setq req-info
              (plist-put req-info :mevedel-agent-invocation invocation))
        (setq req-info
              (plist-put
               req-info :mevedel-compaction-target-policy
               (list :backend effective-backend
                     :model effective-model
                     :max-tokens
                     (alist-get 'gptel-max-tokens request-locals)
                     :request-params
                     (alist-get 'gptel--request-params request-locals))))
        (setf (gptel-fsm-info fsm)
              (plist-put req-info :callback wrapped)))
      fsm)))

(defun mevedel-agent-exec--wrap-callback (gptel-cb mevedel-cb)
  "Build the wrap-and-chain callback for the agent-buffer dispatch path.

GPTEL-CB is gptel's stock insertion callback captured from the FSM's
`:callback' info slot (typically `gptel--insert-response' or
`gptel-curl--stream-insert-response').  MEVEDEL-CB is the bookkeeping
callback returned by `mevedel-agent-exec--make-callback'.

For each event delivered by gptel:

- terminal events (response is t, nil, or `abort'): run MEVEDEL-CB only.  These
  produce no buffer insertion; gptel's stock callback would be a no-op insert
  path.
- insertable events (string chunks, `(tool-call . ...)`, `(tool-result . ...)`,
  etc.): forward to GPTEL-CB first so the agent buffer reflects the event, then
  run MEVEDEL-CB so the partial accumulator and finalize gating see the
  post-insert state.

Errors from either delegate are caught so a misbehaving callback cannot
strand the FSM."
  (lambda (response &rest rest)
    (let ((terminal (memq response '(t nil abort))))
      (unless terminal
        (when gptel-cb
          (condition-case err (apply gptel-cb response rest)
            (error
             (message "mevedel: gptel insertion callback errored: %S" err)))))
      (when mevedel-cb
        (condition-case err (apply mevedel-cb response rest)
          (error
           (message "mevedel: agent bookkeeping callback errored: %S" err)))))))

(defun mevedel-agent-exec--make-callback (main-cb agent-type description
                                                  where partial-cell)
  "Return the callback used by `mevedel-agent-exec-run'.

MAIN-CB receives the final accumulated partial string exactly once.

AGENT-TYPE and DESCRIPTION decorate the error / abort messages.  WHERE
is the tracking-marker fallback for the initial `tool-call' dispatch.
PARTIAL-CELL is a one-element list holding the running accumulated
text seed.  String chunks are stored separately and joined only when a
terminal branch needs the final text.

The dispatch table is:

- nil: transport error; MAIN-CB receives a formatted error string.
- `(tool-call . CALLS)': update tracking marker and hand off to
  `gptel--display-tool-calls'.
- `(pred stringp)': accumulate into PARTIAL-CELL.  When `:stream' is
  absent from the info plist (non-streaming request), also treat the
  string as the terminal signal because gptel never sends `'t' in
  that mode -- see `gptel-curl--stream-cleanup' at gptel-request.el
  2864, which is the only site that fires `'t' and runs on the
  streaming curl sentinel only.  Both non-streaming paths
  \(`gptel--url-parse-response' at 2602 and the non-streaming branch
  of `gptel-curl--parse-response' at 3018) deliver the final text as
  one string and advance the FSM without any terminal event.
- `'t': stream complete; if no tool-use is pending, run the optional
  transformer over the partial and fire MAIN-CB once.
- `'abort': aborted; MAIN-CB receives a formatted abort string.

A per-closure `fired' latch makes all delivery branches idempotent so
streaming runs that emit one string chunk followed by `'t', and the
defensive corner cases, cannot double-fire.  MAIN-CB invocations are
wrapped in `condition-case' so a throw inside the caller does not escape
gptel's callback chain and strand the retained turn."
  (cl-labels ((safe-call (cb &rest args)
                (condition-case err
                    (apply cb args)
                  (error
                   (message "mevedel agent-exec main-cb error: %S" err)))))
    (let ((fired nil)
          (partial-prefix (or (car partial-cell) ""))
          (partial-chunks nil)
          (partial-chars 0))
      ;; Accept &rest so the wrap-and-chain forwarder can pass through
      ;; gptel's optional `raw' third argument (see
      ;; `gptel--insert-response' / `gptel-curl--stream-insert-response')
      ;; without tripping a wrong-number-of-arguments.
      (lambda (resp info &rest _ignored)
        (let ((ov (plist-get info :context)))
          (cl-labels ((append-partial (chunk)
                        (push chunk partial-chunks)
                        (setq partial-chars (+ partial-chars
                                               (length chunk))))
                      (partial-length ()
                        (+ (length partial-prefix) partial-chars))
                      (partial-string ()
                        (let ((text
                               (if partial-chunks
                                   (apply #'concat
                                          partial-prefix
                                          (nreverse partial-chunks))
                                 partial-prefix)))
                          (setq partial-chunks nil
                                partial-prefix text
                                partial-chars 0)
                          (setcar partial-cell text)
                          text))
                      (finalize ()
                        (when mevedel-agent-exec-debug
                          (message "mevedel AGENT-EXEC FINALIZE agent=%s desc=%S \
partial-len=%d :tool-use=%S :stream=%S"
                                   agent-type description
                                   (partial-length)
                                   (and (plist-get info :tool-use) t)
                                   (and (plist-get info :stream) t)))
                        (when (overlayp ov) (delete-overlay ov))
                        ;; Drive transcript finalization from
                        ;; the success path so a non-error completion
                        ;; lands on disk before the parent sees the
                        ;; result.
                        (let* ((text (partial-string))
                               (transformer (plist-get info :transformer))
                               (text (if transformer
                                         (funcall transformer text)
                                       text))
                               (inv (mevedel-agent-exec--invocation-from-info
                                     info))
                               (final-response
                                (mevedel-agent-conversation-final-response inv)))
                          (setq partial-prefix text)
                          (setcar partial-cell text)
                          (safe-call main-cb (or final-response text)))))
            (pcase resp
              ('nil
               (unless fired
                 (setq fired t)
                 (let* ((fallback-partial (partial-string))
                        (inv (mevedel-agent-exec--invocation-from-info info)))
                   (when-let* ((reason
                                (mevedel-agent-exec--error-reason-from-info
                                 info)))
                     (setf (mevedel-agent-invocation-terminal-reason inv)
                           reason))
                   (when (overlayp ov) (delete-overlay ov))
                   (safe-call
                    main-cb
                    (list :mevedel-agent-terminal-status 'error
                          :error-details (plist-get info :error)
                          :fallback-partial fallback-partial)))))
              (`(tool-call . ,calls)
               (unless (plist-get info :tracking-marker)
                 (plist-put info :tracking-marker where))
               (gptel--display-tool-calls calls info))
              ((pred stringp)
               (append-partial resp)
               ;; Non-streaming terminal: gptel removes `:stream' from INFO
               ;; and never fires `t'.  Treat the string as terminal when no
               ;; tool use is pending.
               (when (and (not fired)
                          (not (plist-get info :stream))
                          (not (plist-get info :tool-use)))
                 (setq fired t)
                 (finalize)))
              ('t
               (when (and (not fired)
                          (not (plist-get info :tool-use)))
                 (setq fired t)
                 (finalize)))
              ('abort
               (unless fired
                 (setq fired t)
                 (when (overlayp ov) (delete-overlay ov))
                 (safe-call
                  main-cb
                  (list :mevedel-agent-terminal-status 'aborted
                        :response
                        (format "Error: Task \"%s\" was aborted by the user. \
%s could not finish."
                                description agent-type))))))))))))


;;
;;; Preview registration

(with-eval-after-load 'gptel-agent-tools
  ;; Override the upstream entry so the Agent tool's tool-call preview
  ;; uses mevedel's renderer.  Upstream registers its version when
  ;; gptel-agent-tools loads; we stamp on top afterwards.
  (setf (alist-get "Agent" gptel--tool-preview-alist nil nil #'equal)
        #'mevedel-agent-exec--task-preview-setup))


(provide 'mevedel-agent-exec)
;;; mevedel-agent-exec.el ends here
