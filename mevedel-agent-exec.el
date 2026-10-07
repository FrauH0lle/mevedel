;;; mevedel-agent-exec.el -- Sub-agent task runner -*- lexical-binding: t -*-

;;; Commentary:

;; Mevedel-owned sub-agent request runner.  `mevedel-agent-runtime' coordinates
;; dispatch and lifecycle state; this module owns the gptel request FSM and
;; streaming callback contract.  `mevedel-agent-conversation' owns retained
;; buffers, transcript persistence, and live conversation metadata.
;;
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

(require 'mevedel-agent-conversation)
(require 'mevedel-compact-estimation)
(require 'mevedel-models)

;; `mevedel-claude-code-agent'
(declare-function mevedel-claude-code-agent-run
                  "mevedel-claude-code-agent" (invocation callback))
(autoload 'mevedel-claude-code-agent-run "mevedel-claude-code-agent")

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
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-with-preset "ext:gptel" (name &rest body))
(declare-function gptel--insert-response "ext:gptel"
                  (response info &optional raw))
(declare-function gptel-curl--stream-insert-response "ext:gptel"
                  (response info &optional raw))
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
(declare-function gptel-tool-name "ext:gptel-request" (tool))
(declare-function gptel-tool-p "ext:gptel-request" (tool))
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

;; `mevedel-compact'
(declare-function mevedel--compact-handle-agent-wait
                  "mevedel-compact" (fsm))

;; `mevedel-goal'
(declare-function mevedel-goal-agent-budget-notice "mevedel-goal" (fsm))
(declare-function mevedel-goal-charge-agent-progress "mevedel-goal" (fsm))
(autoload 'mevedel-goal-agent-budget-notice "mevedel-goal")
(autoload 'mevedel-goal-charge-agent-progress "mevedel-goal")

;; `mevedel-presets'
(declare-function mevedel-preset--build-transitions
                  "mevedel-presets" (transitions))
(autoload 'mevedel-preset--build-transitions "mevedel-presets")

;; `mevedel-reminders'
(declare-function mevedel-reminders--agent-transform
                  "mevedel-reminders" (fsm))
(declare-function mevedel-reminders--stage-batch
                  "mevedel-reminders" (fsm entries commits))
(declare-function mevedel-reminders-agent-turn-limit-context "mevedel-reminders" (invocation))
(declare-function mevedel-reminders-agent-turn-warnings "mevedel-reminders" (invocation count))
(declare-function mevedel-reminders-stage-entry
                  "mevedel-reminders" (fsm type body &optional commit))

;; `mevedel-tools'
(declare-function mevedel-tools--handle-agent-roster-inject
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-agent-turn-terminal
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-message-inject
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-plan-tool-filter
                  "mevedel-tools" (fsm))

;; `mevedel-turn'
(declare-function mevedel-turn-end-at-boundary "mevedel-turn" (fsm reason))

(defvar mevedel-agent-exec-debug nil
  "Non-nil enables request-driver lifecycle diagnostics.")

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



;;
;;; FSM handler table

(defun mevedel-agent-exec--invocation-from-fsm (fsm)
  "Return the `mevedel-agent-invocation' for FSM, or nil."
  (when fsm
    (plist-get (gptel-fsm-info fsm) :mevedel-agent-invocation)))

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

(defun mevedel-agent-exec--handle-wait-turn (fsm)
  "Count FSM's model request toward its agent's `max-turns' cap.
Every WAIT samples the model once, so each counts as one agent turn.  Near
the cap the one-shot max-turns warning is staged; the request that reaches it
is the last one: it is told to answer now and its turn ends after any tools
it still calls, settling with its latest response."
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (let* ((count (cl-incf (mevedel-agent-invocation-turn-count inv)))
           (agent (mevedel-agent-invocation-agent inv))
           (max-turns (and agent (mevedel-agent-max-turns agent))))
      (when max-turns
        (if (>= count max-turns)
            (when (mevedel-turn-end-at-boundary fsm 'agent-turn-limit)
              (mevedel-reminders-stage-entry
               fsm 'max-turns-limit
               (mevedel-reminders-agent-turn-limit-context inv)))
          (let ((staged (mevedel-reminders-agent-turn-warnings inv count)))
            (mevedel-reminders--stage-batch
             fsm (plist-get staged :entries) (plist-get staged :commits))))))))

(defun mevedel-agent-exec--handle-wait-goal-budget (fsm)
  "Stage a Goal budget notice for agent request FSM when one is due."
  (when-let* ((notice (mevedel-goal-agent-budget-notice fsm)))
    (mevedel-reminders-stage-entry
     fsm 'goal-budget (plist-get notice :body) (plist-get notice :commit))))

(defun mevedel-agent-exec--handle-done-ended (fsm)
  "Settle FSM's agent turn when it ended at a tool boundary.
Such a turn reaches DONE from its tool results, after the stream's terminal
event already deferred to the pending tool use, so deliver it now.  The
agent stopped without a final answer, so its result says why."
  (let ((info (gptel-fsm-info fsm)))
    (when-let* ((reason (plist-get info :mevedel-end-turn))
                (callback (plist-get info :mevedel-agent-terminal-callback)))
      (plist-put info :tool-use nil)
      (plist-put info :mevedel-agent-stop-note
                 (format "[Stopped before a final answer: %s.]"
                         (pcase reason
                           ('agent-turn-limit
                            (let* ((inv (plist-get
                                         info :mevedel-agent-invocation))
                                   (agent (and inv (mevedel-agent-invocation-agent
                                                    inv))))
                              (format "the %s-turn limit was reached"
                                      (or (and agent
                                               (mevedel-agent-max-turns agent))
                                          "configured"))))
                           ('hook-stop "a hook stopped the turn")
                           (_ (format "%s" reason)))))
      (funcall callback t info))))

(defun mevedel-agent-exec--handle-done-save (fsm)
  "Run gptel's post-response hooks for FSM and checkpoint their changes.
Once terminal publication has committed the answer, defer this extra save.
If settlement is still pending, save immediately to retain recoverable text."
  (when (fboundp 'gptel--handle-post-insert)
    (condition-case _ (gptel--handle-post-insert fsm) (error nil)))
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (mevedel-agent-conversation-save
     inv (mevedel-agent-invocation-runtime-settled-p inv))))

(defun mevedel-agent-exec--handle-abort-save (fsm)
  "Drive gptel's abort path for FSM."
  (when (fboundp 'gptel--handle-abort)
    (condition-case _ (gptel--handle-abort fsm) (error nil))))

(defun mevedel-agent-exec--handle-errs-save (fsm)
  "Run gptel's error path for FSM and finalize the transcript as `error'.

`gptel-post-response-functions' fires from `gptel--handle-error', which
runs the normal post-response bookkeeping before this handler persists
the transcript.

Captures a short reason and the finalized transcript response before routing
the terminal event through the request callback's exactly-once retry gate."
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
                         :fallback-partial fallback-partial)
                   info))))))

(defvar mevedel-agent-exec--handlers
  `((WAIT ,#'mevedel-tools--handle-agent-roster-inject
     ,#'mevedel-tools--handle-message-inject
     ,#'mevedel-tools--handle-plan-tool-filter
     ,#'mevedel-agent-exec--handle-wait-activity
     ,#'mevedel-agent-exec--handle-wait-turn
     ,#'mevedel-agent-exec--handle-wait-goal-budget
     ,#'mevedel--compact-handle-agent-wait)
    (TPRE ,#'gptel--handle-token-usage
          ,#'mevedel-compact-estimation-record-token-baseline
          ,#'gptel--handle-pre-tool
          ,#'gptel--fsm-transition)
    (TOOL ,#'gptel--update-tool-call
          ,#'gptel--handle-tool-use
          ,#'gptel--update-tool-ask)
    (TRET ,#'gptel--handle-post-tool
          ,#'gptel--handle-tool-result
          ,#'mevedel-goal-charge-agent-progress
          ,#'mevedel-agent-exec--handle-tret-save)
    (DONE ,#'mevedel-compact-estimation-record-token-baseline
          ,#'mevedel-goal-charge-agent-progress
          ,#'mevedel-agent-exec--handle-done-ended
          ,#'mevedel-tools--handle-agent-turn-terminal
          ,#'mevedel-agent-exec--handle-done-save)
    (ABRT ,#'mevedel-compact-estimation-record-token-baseline
          ,#'mevedel-goal-charge-agent-progress
          ,#'mevedel-tools--handle-agent-turn-terminal
          ,#'mevedel-agent-exec--handle-abort-save)
    (ERRS ,#'mevedel-compact-estimation-record-token-baseline
          ,#'mevedel-goal-charge-agent-progress
          ,#'mevedel-tools--handle-agent-turn-terminal
          ,#'mevedel-agent-exec--handle-errs-save))
  "Handler table for the mevedel sub-agent FSM.

Same shape as `gptel-send--transitions': each entry is `(STATE FN ...)'
where FN is called when the FSM transitions into (or out of) STATE.

Additions:

- `WAIT' injects the caller's compact direct-child roster, inbound messages,
  and system reminders before sampling, and counts the sample toward the
  agent's `max-turns' cap.
- `WAIT' also tells the agent when the Goal paying for it crosses a budget
  threshold; `TRET' and the terminal states charge its usage to that Goal.
- `TRET' gains `mevedel-agent-exec--handle-tret-save' so transcripts are durable
  across long tool loops (gptel's post-response hook fires only at DONE/ABRT,
  not TRET).
- `DONE' is added with `gptel--handle-post-insert' delegation so
  `gptel-post-response-functions' actually runs in the agent buffer, and
  settles a turn that `mevedel-turn-end-at-boundary' ended at its tool
  results.
- `ABRT' drives the transcript through finalization with status
  `aborted'.")


;;
;;; Request buffer configuration

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
           agent-spec
           (list :include-reasoning gptel-include-reasoning))))

(defun mevedel-agent-exec-request-snapshot (policy)
  "Return one frozen request-local snapshot with model POLICY applied."
  (cl-loop
   for symbol in mevedel-agent-request-local-symbols
   for value = (pcase symbol
                 ('gptel-backend (plist-get policy :backend))
                 ('gptel-model (plist-get policy :model))
                 ('gptel-reasoning-effort (plist-get policy :effort))
                 (_ (and (boundp symbol) (symbol-value symbol))))
   when (and (eq symbol 'gptel-system-prompt) (functionp value))
   ;; Parts of the prompt are budgeted against the model's context
   ;; window, so it is assembled under the model this request will use
   ;; rather than the one the spawning buffer happens to hold.
   do (setq value
            (let ((gptel-model (or (plist-get policy :model) gptel-model))
                  (gptel-backend (or (plist-get policy :backend)
                                     gptel-backend)))
              (funcall value)))
   collect (cons symbol (copy-tree value))))

(defun mevedel-agent-exec-freeze-configuration
    (agent-type invocation &optional model-policy)
  "Freeze AGENT-TYPE's effective request configuration for INVOCATION.
MODEL-POLICY may supply a tuple already validated before spawn admission."
  (let* ((policy
          (or model-policy
              (mevedel-agent-exec--policy-for-invocation
               agent-type invocation)))
         (gptel-model (or (plist-get policy :model) gptel-model))
         (gptel-backend (or (plist-get policy :backend) gptel-backend)))
    (gptel-with-preset
     (mevedel-agent-exec--request-preset agent-type invocation)
     (let* ((request-locals
             (mevedel-agent-exec-request-snapshot policy))
            (agent
             (copy-mevedel-agent
              (mevedel-agent-invocation-agent invocation))))
       (setf (mevedel-agent-system-prompt agent)
             (alist-get 'gptel-system-prompt request-locals))
       (setq agent (mevedel-agent-freeze agent))
       (setf (mevedel-agent-invocation-agent invocation) agent)
       (mevedel-agent-configuration--create
        :agent agent
        :request-locals request-locals)))))


;;
;;; Task runner

(cl-defun mevedel-agent-exec-run (main-cb agent-type description
                                        invocation agent-buffer)
  "Dispatch a sub-agent task and route its final response to MAIN-CB.

AGENT-TYPE is the registry key (e.g. `\"explorer\"', `\"verifier\"').
DESCRIPTION is a short human-facing label shown in the agent handle.
INVOCATION is the `mevedel-agent-invocation' associated with this task.
It is stashed on the FSM info plist so collaboration-mail, reminder,
and compaction handlers can reach it at ordinary request boundaries.

AGENT-BUFFER is the live per-invocation gptel buffer that holds the
sub-agent's transcript.

Callback contract. This runner:

  - accumulates streamed string chunks into `partial';
  - fires MAIN-CB exactly once on the t branch, after the sub-agent turn has
    completed and no further tool-use is pending.

The wrapper forwards each insertable
event (string chunks, tool-call / tool-result entries) to gptel's stock
callback before running the mevedel bookkeeping, so the agent buffer
reflects the event before the wrapper's `partial` accumulator acts on
it. Terminal events (t, nil, abort) skip the forward step.

Returns the native FSM or external invocation handle."
  (unless (mevedel-agent-invocation-p invocation)
    (error "Invalid sub-agent invocation"))
  (unless (buffer-live-p agent-buffer)
    (error "Sub-agent buffer is not live"))
  (let ((frozen
         (mevedel-agent-invocation-frozen-configuration invocation)))
    (unless (mevedel-agent-configuration-p frozen)
      (error "Agent request configuration is not frozen"))
    (when (mevedel-engine-external-p
           (alist-get 'gptel-backend (mevedel-agent-configuration-request-locals frozen)))
      (cl-return-from mevedel-agent-exec-run
        (mevedel-claude-code-agent-run
         invocation
         (mevedel-agent-exec--make-callback
          main-cb agent-type description (with-current-buffer agent-buffer (copy-marker (point-max)))
          (list (format "%s result for task: %s\n\n" (capitalize agent-type) description))))))
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
           (fsm (gptel-make-fsm
                 :table (mevedel-preset--build-transitions
                         (copy-tree (default-value 'gptel-send--transitions)))
                 :handlers mevedel-agent-exec--handlers))
           (mevedel-cb
            (mevedel-agent-exec--make-callback
             main-cb agent-type description where (list partial)))
           (wrapped (mevedel-agent-exec--wrap-callback mevedel-cb)))
      (setf (mevedel-agent-invocation-runtime-fsm invocation) fsm)
      (gptel--update-status " Calling Agent..." 'font-lock-escape-face)
      ;; Install one dispatch-local copy of the frozen request state before
      ;; gptel reads it from the agent buffer or copies it to a prompt buffer.
      (mevedel-agent-conversation-configure invocation agent-buffer)
      (with-current-buffer agent-buffer
        (goto-char (point-max))
        (gptel-request nil
          :buffer agent-buffer
          :fsm fsm
          :stream gptel-stream
          :callback wrapped
          :system gptel-system-prompt
          :transforms (list #'gptel--transform-add-context
                            #'mevedel-reminders--agent-transform)))
      (let ((req-info (gptel-fsm-info fsm)))
        ;; `gptel-request' replaces the FSM info plist wholesale, so
        ;; every mevedel key must be installed on the plist it built.
        ;; The terminal callback is what settles the invocation from the
        ;; ERRS handler; losing it leaves the agent running forever
        ;; after an error the provider callback never sees.
        (setq req-info
              (plist-put req-info :mevedel-agent-invocation invocation))
        (setq req-info
              (plist-put req-info
                         :mevedel-agent-terminal-callback mevedel-cb))
        (setq req-info
              (plist-put
               req-info :mevedel-compaction-target-policy
               (list :backend effective-backend
                     :model effective-model
                     :max-tokens
                     (alist-get 'gptel-max-tokens request-locals)
                     :request-params
                     (alist-get 'gptel--request-params request-locals))))
        ;; The callback must be supplied to gptel-request before it can
        ;; dispatch, not replaced afterwards: observers may have wrapped it.
        (setf (gptel-fsm-info fsm) req-info))
      fsm)))

(defun mevedel-agent-exec--wrap-callback (mevedel-cb)
  "Build the wrap-and-chain callback for the agent-buffer dispatch path.

MEVEDEL-CB is the bookkeeping callback returned by
`mevedel-agent-exec--make-callback'.

For each event delivered by gptel:

- terminal events (response is t, nil, or `abort'): run MEVEDEL-CB only.  These
  produce no buffer insertion; gptel's stock callback would be a no-op insert
  path.
- insertable events (string chunks, `(tool-call . ...)`, `(tool-result . ...)`,
  etc.): forward to gptel's stock insertion callback first so the agent buffer
  reflects the event, then run MEVEDEL-CB so the partial accumulator and
  finalize gating see the post-insert state.

The stock callback is chosen per event from the request's final `:stream',
as `gptel-curl-get-response' would.  gptel installs its default only when
the provider request is sent, which can happen after `gptel-request'
returns (edited-file diffs are prepared asynchronously), so it cannot be
captured at dispatch.

Insertion errors are diagnostic only.  The adapter retains a terminal event
whose runtime handoff fails and retries it from an owned timer."
  (lambda (response &rest rest)
    (let ((terminal (memq response '(t nil abort))))
      (unless terminal
        (condition-case err
            (apply (if (plist-get (car rest) :stream)
                       #'gptel-curl--stream-insert-response
                     #'gptel--insert-response)
                   response rest)
          (error
           (message "mevedel: gptel insertion callback errored: %S" err))))
      (when mevedel-cb
        (apply mevedel-cb response rest)))))

(defun mevedel-agent-exec--make-callback (main-cb agent-type description
                                                  where partial-cell)
  "Return the callback used by `mevedel-agent-exec-run'.

MAIN-CB receives the final accumulated partial string on success, or a
structured terminal event on error or abort, exactly once.

AGENT-TYPE and DESCRIPTION decorate the abort message.  WHERE
is the tracking-marker fallback for the initial `tool-call' dispatch.
PARTIAL-CELL is a one-element list holding the running accumulated
text seed.  String chunks are stored separately and joined only when a
terminal branch needs the final text.

The dispatch table is:

- nil: transport error; MAIN-CB receives a structured error event.
- a structured mevedel terminal event: deliver it through the same retry gate.
- `(tool-call . CALLS)': update tracking marker and hand off to
  `gptel--display-tool-calls'.
- `(pred stringp)': accumulate into PARTIAL-CELL.  When `:stream' is
  absent from the info plist (non-streaming request), also treat the
  string as the terminal signal.  `gptel-curl--stream-cleanup' fires
  `'t' from the streaming curl sentinel, while both non-streaming
  paths (`gptel--url-parse-response' and the
  non-streaming branch of `gptel-curl--parse-response') deliver the
  final text as one string and advance the FSM without any terminal
  event.
- `'t': stream complete; if no tool-use is pending, run the optional
  transformer over the partial and fire MAIN-CB once.
- `'abort': aborted; MAIN-CB receives a structured abort event.

Both terminal success branches first check `:error' on the info plist:
a provider can fail in-band on an HTTP 200 stream (the parser stashes
the error and `gptel-curl--stream-cleanup' still fires `'t'), and that
turn must settle as an error, not as an empty completion.

A per-closure `fired' latch makes accepted delivery idempotent.  A rejected
MAIN-CB handoff remains pending and an owned timer retries it without another
provider callback."
  (let ((fired nil)
        pending-terminal
        retry-timer
        (partial-prefix (or (car partial-cell) ""))
        (partial-chunks nil)
        (partial-chars 0))
    ;; Accept &rest so the wrap-and-chain forwarder can pass through
    ;; gptel's optional `raw' third argument (see
    ;; `gptel--insert-response' / `gptel-curl--stream-insert-response')
    ;; without tripping a wrong-number-of-arguments.
    (lambda (resp info &rest _ignored)
      (let ((ov (plist-get info :context)))
        (cl-labels ((schedule-retry ()
                      (unless retry-timer
                        (setq retry-timer
                              (run-at-time 1 nil #'retry))))
                    (retry ()
                      (setq retry-timer nil)
                      (when (and pending-terminal (not fired))
                        (deliver pending-terminal)))
                    (deliver (value)
                      (setq pending-terminal value)
                      (condition-case nil
                          (progn
                            (funcall main-cb value)
                            (setq pending-terminal nil
                                  fired t)
                            (when (timerp retry-timer)
                              (cancel-timer retry-timer))
                            (setq retry-timer nil))
                        (error (schedule-retry))))
                    (append-partial (chunk)
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
                    (deliver-error ()
                      (let* ((fallback-partial (partial-string))
                             (inv (plist-get info :mevedel-agent-invocation)))
                        (when-let* ((reason
                                     (mevedel-agent-exec--error-reason-from-info
                                      info)))
                          (setf (mevedel-agent-invocation-terminal-reason inv)
                                reason))
                        (when (overlayp ov) (delete-overlay ov))
                        (deliver
                         (list :mevedel-agent-terminal-status 'error
                               :error-details (plist-get info :error)
                               :fallback-partial fallback-partial))))
                    (finalize ()
                      (when mevedel-agent-exec-debug
                        (message "mevedel AGENT-EXEC FINALIZE agent=%s desc=%S \
partial-len=%d :tool-use=%S :stream=%S"
                                 agent-type description
                                 (partial-length)
                                 (and (plist-get info :tool-use) t)
                                 (and (plist-get info :stream) t)))
                      ;; An in-band provider error on an HTTP 200 stream
                      ;; reaches this callback as the success terminal:
                      ;; gptel's stream parser only stashes the error on
                      ;; INFO and `gptel-curl--stream-cleanup' fires t for
                      ;; any 200 close.  Settle it as an error, not as an
                      ;; empty completion.
                      (if (plist-get info :error)
                          (deliver-error)
                        (finalize-success)))
                    (finalize-success ()
                      (when (overlayp ov) (delete-overlay ov))
                      ;; Drive transcript finalization from
                      ;; the success path so a non-error completion
                      ;; lands on disk before the parent sees the
                      ;; result.
                      (let ((terminal
                             (condition-case err
                                 (let* ((text (partial-string))
                                        (transformer
                                         (plist-get info :transformer))
                                        (text
                                         (if transformer
                                             (funcall transformer text)
                                           text))
                                        (inv
                                         (plist-get info :mevedel-agent-invocation))
                                        (final-response
                                         (mevedel-agent-conversation-final-response
                                          inv)))
                                   (setq partial-prefix text)
                                   (setcar partial-cell text)
                                   (if-let* ((note (plist-get
                                                    info :mevedel-agent-stop-note)))
                                       (concat (or final-response text)
                                               "\n\n" note)
                                     (or final-response text)))
                               (error
                                (list
                                 :mevedel-agent-terminal-status 'error
                                 :error-details
                                 (error-message-string err))))))
                        (deliver terminal))))
          (when (and pending-terminal (not fired))
            (deliver pending-terminal))
          (unless fired
            (pcase resp
              ((pred (lambda (value)
                       ;; Stream events like (reasoning . t) are improper
                       ;; lists; plist-member signals on those.
                       (and (proper-list-p value)
                            (plist-member
                             value :mevedel-agent-terminal-status))))
               (deliver resp))
              ('nil (deliver-error))
              (`(tool-call . ,calls)
               (unless (plist-get info :tracking-marker)
                 (plist-put info :tracking-marker where))
               (gptel--display-tool-calls calls info))
              ((pred stringp)
               (append-partial resp)
               ;; Non-streaming terminal: gptel removes `:stream' from INFO
               ;; and never fires `t'.  Treat the string as terminal when no
               ;; tool use is pending.
               (when (and (not (plist-get info :stream))
                          (not (plist-get info :tool-use)))
                 (finalize)))
              ('t
               (unless (plist-get info :tool-use)
                 (finalize)))
              ('abort
               (when (overlayp ov) (delete-overlay ov))
               (deliver
                (list :mevedel-agent-terminal-status 'aborted
                      :response
                      (format "Error: Task \"%s\" was aborted by the user. \
%s could not finish."
                              description agent-type)))))))))))


(provide 'mevedel-agent-exec)
;;; mevedel-agent-exec.el ends here
