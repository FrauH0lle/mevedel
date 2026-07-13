;;; mevedel-agent-runtime.el --- Agent lifecycle coordination -*- lexical-binding: t -*-

;;; Commentary:

;; Canonical owner for Agent dispatch, BWAIT coordination, watchdogs,
;; stopping, recovery, completion, and runtime persistence.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel-request)
  (require 'mevedel-tool-registry)
  (require 'mevedel-agents)
  (require 'mevedel-reminders))

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(defvar gptel--request-alist)

;; `gptel-request'
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel-abort "ext:gptel-request" (&optional buf))
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-table "ext:gptel-request" (cl-x) t)

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--allocate-agent-buffer
                  "mevedel-agent-exec" (invocation parent-data-buffer))
(declare-function mevedel-agent-exec--final-activity-snapshot
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--final-response-text
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--flush-transcript-save
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--handle-update
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--insert-injected-prompt
                  "mevedel-agent-exec" (invocation block &optional position))
(declare-function mevedel-agent-exec--on-buffer-kill
                  "mevedel-agent-exec" ())
(declare-function mevedel-agent-exec--record-activity
                  "mevedel-agent-exec"
                  (invocation item &optional reserved))
(declare-function mevedel-agent-exec--run-stop-hook
                  "mevedel-agent-exec" (invocation status))
(declare-function mevedel-agent-exec--save-transcript-buffer
                  "mevedel-agent-exec" (invocation))
(defvar mevedel-agent-exec--suppress-activity-rerender)
(defvar mevedel-agent-exec-debug)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-activity
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-agents
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-result-reported-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-create "mevedel-agents" (agent))
(declare-function mevedel-agent-invocation-description
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-foreground-result-reported-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-hook-audits
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-model-tier-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-messages
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-context
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-fsm
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-tool-callback
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-turn
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--data-buffer
                  "mevedel-interaction-prompt" (&optional buffer))

;; `mevedel-models'
(declare-function mevedel-model-tier-selector "mevedel-models" (tier))

;; `mevedel-reminders'
(declare-function mevedel-reminders--collect-from "mevedel-reminders"
                  (reminders turn-count ctx))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--record-running-transcript
                  "mevedel-session-persistence" (session entry))
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--update-transcript-entry
                  "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-persistence--validate-transcript-path
                  "mevedel-session-persistence" (path save-path))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(defvar mevedel-session--read-only-mode)

;; `mevedel-structs'
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-background-agents
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-reminder
                  "mevedel-structs" (session body))
(declare-function mevedel-session-messages "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tool-task'
(declare-function mevedel-tool-task--refresh-display "mevedel-tool-task" ())
(declare-function mevedel-tool-task-finalize-owner
                  "mevedel-tool-task" (session owner status))

;; `mevedel-view'
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())

;; `mevedel-view-agent'
(declare-function mevedel-view--insert-attribution "mevedel-view-agent"
                  (agent-id &optional live-click-p calls))
(declare-function mevedel-view-agent-live-transcript-finalize
                  "mevedel-view-agent" (invocation))

;; `mevedel-view-composer'
(defvar mevedel-view--input-marker)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-anchor
                  "mevedel-view-interaction" ())
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (&optional data-buffer))
(declare-function mevedel-view--interaction-unregister
                  "mevedel-view-interaction" (id))

(defun mevedel-agent-runtime--ctx-messages (ctx)
  "Return CTX's inbound message queue."
  (if (mevedel-agent-invocation-p ctx)
      (mevedel-agent-invocation-messages ctx)
    (mevedel-session-messages ctx)))

(gv-define-setter mevedel-agent-runtime--ctx-messages (value ctx)
  (list 'if (list 'mevedel-agent-invocation-p ctx)
        (list 'setf (list 'mevedel-agent-invocation-messages ctx) value)
        (list 'setf (list 'mevedel-session-messages ctx) value)))

(defun mevedel-agent-runtime--ctx-push-message (ctx message)
  "Queue MESSAGE on CTX's inbound mailbox."
  (if (mevedel-agent-invocation-p ctx)
      (push message (mevedel-agent-invocation-messages ctx))
    (push message (mevedel-session-messages ctx))))

(defun mevedel-agent-runtime--ctx-background-agents (ctx)
  "Return CTX's running background-agent IDs."
  (if (mevedel-agent-invocation-p ctx)
      (mevedel-agent-invocation-background-agents ctx)
    (mevedel-session-background-agents ctx)))

(defun mevedel-agent-runtime--ctx-push-background-agent (ctx agent-id)
  "Add AGENT-ID to CTX's background-agent tracking list."
  (if (mevedel-agent-invocation-p ctx)
      (push agent-id (mevedel-agent-invocation-background-agents ctx))
    (push agent-id (mevedel-session-background-agents ctx))))

(defun mevedel-agent-runtime--ctx-remove-background-agent (ctx agent-id)
  "Remove AGENT-ID from CTX's background-agent tracking list."
  (if (mevedel-agent-invocation-p ctx)
      (setf (mevedel-agent-invocation-background-agents ctx)
            (delete agent-id
                    (mevedel-agent-invocation-background-agents ctx)))
    (setf (mevedel-session-background-agents ctx)
          (delete agent-id (mevedel-session-background-agents ctx)))))

(defun mevedel-agent-runtime--ctx-clear-background-agents (ctx)
  "Clear CTX's background-agent tracking list."
  (if (mevedel-agent-invocation-p ctx)
      (setf (mevedel-agent-invocation-background-agents ctx) nil)
    (setf (mevedel-session-background-agents ctx) nil)))

(defun mevedel-agent-runtime--context-for-fsm (fsm)
  "Return the agent invocation or session associated with FSM."
  (when-let* ((info (and fsm (gptel-fsm-info fsm))))
    (or (and (mevedel-agent-invocation-p
              (plist-get info :mevedel-agent-invocation))
             (plist-get info :mevedel-agent-invocation))
        (when-let* ((buffer (plist-get info :buffer))
                    ((buffer-live-p buffer)))
          (buffer-local-value 'mevedel--session buffer)))))

(defcustom mevedel-agent-runtime-debug nil
  "When non-nil, log sub-agent dispatch handoffs to `*Messages*'.

Diagnostic for the multi-agent foreground-callback path.  Three log
points fire when enabled:

  AGENT-EXEC FINALIZE  -- the streaming callback's terminal `'t' (or
    non-streaming string-as-terminal) branch is about to fire
    `main-cb' with the accumulated partial.  Confirms gptel reached
    end-of-stream and the latch picked up the terminal event.

  TASK-DISPATCH FG     -- the foreground gating wrapper in
    `mevedel-agent-runtime-dispatch' was invoked.  Shows whether the
    fired latch is set, whether the response looks like an error, and
    the current state of `background-agents' / `messages' on the
    sub-agent's invocation.  If the gate says \"still pending\", this
    is where we'd see the stale entries.

  TASK-DISPATCH FG-FIRE -- the gate decided to fire `main-cb'.  If
    FINALIZE and FG fire but FG-FIRE never does, the gate is
    incorrectly returning to BWAIT-equivalent on the terminal turn.

Set via `setopt mevedel-agent-runtime-debug t' before reproducing a
multi-agent hang.  The output goes to `*Messages*' (only)."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-agent-background-timeout 600
  "Maximum seconds between BWAIT watchdog checks.

When a background agent loses its completion callback (crash, lost
connection, host exit), the parent would otherwise park indefinitely
in BWAIT.  After this timeout, the watchdog removes only agents that
no longer have a live FSM.  Agents that are still running keep the
parent parked in BWAIT and the watchdog is armed again.

Set to nil to disable the watchdog.  The timeout starts when the FSM
enters BWAIT; a stale timer firing after the FSM has left BWAIT is a
no-op."
  :type '(choice (integer :tag "Timeout in seconds")
                 (const :tag "Disabled" nil))
  :group 'mevedel)

(defcustom mevedel-agent-no-progress-timeout 600
  "Maximum seconds a live agent may run without visible progress.

Applies to foreground Agent calls and background agents blocking a
parent in BWAIT.  Progress is measured from the last observed activity:
transcript buffer growth, tool-call count changes, or recorded agent
activity.

Set to nil to disable no-progress auto-stop.  This does not disable
stranded background-agent recovery, which is controlled by
`mevedel-agent-background-timeout'."
  :type '(choice (integer :tag "Timeout in seconds")
                 (const :tag "Disabled" nil))
  :group 'mevedel)

(defvar-local mevedel-agent-runtime--fsms nil
  "Alist mapping agents to their FSM.")

(defvar mevedel-agent-runtime--foreground-watchdogs (make-hash-table :test #'equal)
  "Hash table of foreground agent watchdog records keyed by agent id.")

(defvar mevedel-agent-runtime--background-watchdogs (make-hash-table :test #'equal)
  "Hash table of background agent no-progress records keyed by agent id.")

(defun mevedel-agent-runtime--prune-stale-agents-fsm ()
  "Remove terminal, errored, or abandoned FSMs from `mevedel-agent-runtime--fsms'.
Called before registry lookups so recipient resolution doesn't route
to a dead invocation.

An entry is considered stale when either:
- its FSM state is DONE, ERRS, or ABRT (normal termination), or
- the FSM no longer carries a live `mevedel-agent-invocation', or
- the invocation has already recorded a terminal transcript status."
  (when mevedel-agent-runtime--fsms
    (setq mevedel-agent-runtime--fsms
          (cl-remove-if
           (lambda (entry)
             (let* ((fsm (cdr entry))
                    (state (ignore-errors (gptel-fsm-state fsm)))
                    (inv (ignore-errors
                           (mevedel-agent-runtime--agent-invocation-at fsm))))
               (or (memq state '(DONE ERRS ABRT))
                   (not inv)
                   (not (mevedel-agent-runtime--agent-fsm-live-p fsm))
                   (memq (mevedel-agent-invocation-transcript-status inv)
                         '(completed error aborted incomplete)))))
           mevedel-agent-runtime--fsms))))

(defvar mevedel-agent-runtime--bwait-table-cache nil
  "Alist of (SOURCE-TABLE . INJECTED-TABLE) keyed by `eq'.
The injected table is the result of `copy-tree' + BWAIT mutation, so
a shared cache lets every sub-agent spawn and every chat-buffer
init skip the per-call copy.  Bounded in practice by the small
number of distinct upstream transition tables (typically one); the
cache is not invalidated because gptel's defvar tables don't change
at runtime.  The injected table is never mutated after creation --
gptel only reads the FSM table for transitions -- so sharing is
safe.")

(defun mevedel-agent-runtime--bwait-injected-table (source)
  "Return SOURCE with a BWAIT parking state injected.

Inserts a `mevedel-agent-runtime--background-agents-pending-p' predicate
before the `(t . DONE)' fallthrough in both TYPE and TRET, and adds
BWAIT as a terminal-like state with no outgoing transitions (the
background-agent completion callback forces BWAIT->WAIT explicitly).

If SOURCE already contains a BWAIT entry it is returned unchanged --
the mutation below unconditionally prepends the bg-pending predicate
before every `(t . _)' transition, so re-injecting a table would
accumulate duplicate predicates on every call.

Result is memoized on SOURCE identity so repeat calls (each sub-agent
spawn, each preset application) don't re-copy the full transition
table."
  (cond
   ;; Already injected: no-op so callers can pass through without
   ;; worrying about double-injection across library reloads or
   ;; nested preset chains.
   ((assq 'BWAIT source) source)
   ;; Cache hit: return the previously-injected copy.
   ((cdr (assq source mevedel-agent-runtime--bwait-table-cache)))
   ;; Miss: inject and cache.
   (t
    (let ((injected (copy-tree source))
          (pred #'mevedel-agent-runtime--background-agents-pending-p))
      (dolist (state '(TYPE TRET))
        (when-let* ((entry (assq state injected)))
          (let ((transitions (cdr entry))
                (new-transitions nil))
            (dolist (tr transitions)
              (when (eq (car tr) t)
                (push (cons pred 'BWAIT) new-transitions))
              (push tr new-transitions))
            (setcdr entry (nreverse new-transitions)))))
      (push '(BWAIT) injected)
      (push (cons source injected) mevedel-agent-runtime--bwait-table-cache)
      injected))))

(defun mevedel-agent-runtime--agent-invocation-at (fsm)
  "Return the `mevedel-agent-invocation' attached to FSM.

Returns nil if FSM is not an agent invocation."
  (when-let* ((info (and fsm (gptel-fsm-info fsm))))
    (and (mevedel-agent-invocation-p
          (plist-get info :mevedel-agent-invocation))
         (plist-get info :mevedel-agent-invocation))))

(defun mevedel-agent-runtime--handle-wait-inject (fsm)
  "WAIT-state handler: advance turn count and inject agent reminders.

Runs once per WAIT cycle for agent FSMs, before
`gptel--handle-wait' fires the HTTP request.  Looks up the
`mevedel-agent-invocation' attached to FSM; no-op outside agent
dispatch.

Actions, in order:

  1. Increment the invocation's turn counter.  A main-chat run issues
     one request per user turn, so transforms run once per turn; an
     agent run loops through many WAIT cycles from a single
     `gptel-request' call, so turn counting must happen here to
     advance between tool cycles.

  2. Evaluate the invocation's reminders against the new turn count
     via `mevedel-reminders--collect-from'.  Each firing reminder's
     LAST-FIRED slot is updated as a side effect so interval throttling
     works correctly across cycles.

  3. If any reminder fired, append a single user-role message block
     carrying the joined reminder blocks to `info :data :messages'
     via `gptel--inject-prompt'.  The next HTTP request picks up the
     mutated payload directly -- the WAIT handler is the only place in
     the FSM loop where the info plist can still be modified after
     `gptel--realize-query' has built the message vector."
  (when-let* ((inv (mevedel-agent-runtime--agent-invocation-at fsm)))
    (let* ((turn (mevedel-agent-invocation-turn-count inv))
           (blocks (mevedel-reminders--collect-from
                    (mevedel-agent-invocation-reminders inv)
                    turn inv)))
      (mevedel-agent-exec--record-activity
       inv
       (list :type 'waiting :summary "waiting"))
      (when blocks
        (let* ((info (gptel-fsm-info fsm))
               (data (plist-get info :data))
               (joined (string-join blocks "\n"))
               ;; On the first WAIT cycle, prepend ahead of the user
               ;; task prompt so the API request matches the audit
               ;; log (reminders first, then task).  Later cycles
               ;; append.
               (position (and (zerop (or turn 0)) 0)))
          ;; Write reminders into the agent buffer too so the audit
          ;; log captures what gptel--inject-prompt otherwise
          ;; leaves only in info :data.
          (mevedel-agent-exec--insert-injected-prompt
           inv joined (and position 'prepend))
          (when data
            (gptel--inject-prompt
             (plist-get info :backend) data
             (list :role "user"
                   :content joined)
             position)))))
    (cl-incf (mevedel-agent-invocation-turn-count inv))))

(cl-defun mevedel-agent-runtime--augment-agent-handlers (handlers &key prepend append)
  "Return a copy of HANDLERS with PREPEND and APPEND extras merged in.

HANDLERS is an FSM handlers alist of shape ((STATE . (h1 h2 ...)) ...).
PREPEND and APPEND are each alists of the same shape; their handler
lists are inserted at the head or tail respectively of the matching
state entries in HANDLERS.  States present in PREPEND or APPEND but
missing from HANDLERS are created.  The original HANDLERS alist is
not mutated."
  (let ((result (mapcar (lambda (entry)
                          (cons (car entry) (copy-sequence (cdr entry))))
                        handlers)))
    (dolist (entry prepend)
      (let* ((state (car entry))
             (fns (cdr entry))
             (existing (assq state result)))
        (if existing
            (setcdr existing (append fns (cdr existing)))
          (push (cons state (append fns nil)) result))))
    (dolist (entry append)
      (let* ((state (car entry))
             (fns (cdr entry))
             (existing (assq state result)))
        (if existing
            (setcdr existing (append (cdr existing) fns))
          (push (cons state (append fns nil)) result))))
    result))

(defun mevedel-agent-runtime--background-agents-pending-p (info)
  "Return non-nil if INFO's context has pending background work.

Used as a transition predicate: when the LLM produces no tool calls
but background agents are still running OR undelivered agent results
sit in the mailbox, the FSM parks in BWAIT instead of terminating in
DONE.  Checks the agent invocation on the FSM info plist first,
falling back to the buffer-local session.

Checking both `background-agents' and `messages' covers the race where
a background agent finishes and drains from `background-agents' before
the parent FSM reaches TYPE -- the result is in the mailbox and must
not be lost."
  (let ((ctx (or (and (mevedel-agent-invocation-p
                      (plist-get info :mevedel-agent-invocation))
                     (plist-get info :mevedel-agent-invocation))
                 (when-let* ((buf (plist-get info :buffer))
                             ((buffer-live-p buf)))
                   (buffer-local-value 'mevedel--session buf)))))
    (and ctx (or (mevedel-agent-runtime--ctx-background-agents ctx)
                 (mevedel-agent-runtime--ctx-messages ctx)))))

(defun mevedel-agent-runtime--background-agent-fsm (agent-id parent-buffer)
  "Return AGENT-ID's child FSM from PARENT-BUFFER's registry."
  (when (and parent-buffer (buffer-live-p parent-buffer))
    (with-current-buffer parent-buffer
      (cdr (assoc agent-id mevedel-agent-runtime--fsms)))))

(defun mevedel-agent-runtime--agent-fsm-live-p (fsm)
  "Return non-nil when FSM still represents a running agent."
  (when-let* ((invocation (and fsm
                               (ignore-errors
                                 (mevedel-agent-runtime--agent-invocation-at fsm))))
              (state (ignore-errors (gptel-fsm-state fsm)))
              ((not (memq state '(DONE ERRS ABRT))))
              (buffer (mevedel-agent-invocation-buffer invocation)))
    (buffer-live-p buffer)))

(defun mevedel-agent-runtime--background-watchdog-cancel (agent-id)
  "Clear background no-progress watchdog state for AGENT-ID."
  (when (stringp agent-id)
    (remhash agent-id mevedel-agent-runtime--background-watchdogs)))

(defun mevedel-agent-runtime--background-watchdog-arm-live (ctx parent-buffer)
  "Arm no-progress records for live background agents in CTX PARENT-BUFFER.
Return a list of grace intervals that should be considered when
scheduling the next BWAIT watchdog tick."
  (let (remaining)
    (when (and (mevedel-agent-runtime--agent-no-progress-enabled-p)
               parent-buffer
               (buffer-live-p parent-buffer))
      (dolist (agent-id (and ctx
                             (mevedel-agent-runtime--ctx-background-agents ctx)))
        (let* ((child-fsm
                (mevedel-agent-runtime--background-agent-fsm
                 agent-id parent-buffer))
               (invocation
                (and (mevedel-agent-runtime--agent-fsm-live-p child-fsm)
                     (mevedel-agent-runtime--agent-invocation-at child-fsm))))
          (when (mevedel-agent-invocation-p invocation)
            (let ((record (gethash agent-id
                                   mevedel-agent-runtime--background-watchdogs)))
              (if record
                  (let ((decision
                         (mevedel-agent-runtime--agent-no-progress-assess record)))
                    (when-let* ((updated (plist-get decision :record)))
                      (puthash agent-id updated
                               mevedel-agent-runtime--background-watchdogs))
                    (push (if (eq (plist-get decision :state) 'stop)
                              1
                            (plist-get decision :remaining))
                          remaining))
                (puthash agent-id
                         (mevedel-agent-runtime--agent-watchdog-record
                          invocation parent-buffer)
                         mevedel-agent-runtime--background-watchdogs)
                (push mevedel-agent-no-progress-timeout remaining)))))))
    (nreverse remaining)))

(defun mevedel-agent-runtime--background-watchdog-assess
    (agent-id invocation parent-buffer)
  "Assess AGENT-ID INVOCATION in PARENT-BUFFER and return a decision plist."
  (cond
   ((not (mevedel-agent-runtime--agent-no-progress-enabled-p))
    (list :state 'disabled))
   ((not (mevedel-agent-invocation-p invocation))
    (list :state 'disabled))
   (t
    (let ((record (gethash agent-id mevedel-agent-runtime--background-watchdogs)))
      (if (not record)
          (let ((record
                 (mevedel-agent-runtime--agent-watchdog-record
                  invocation parent-buffer)))
            (puthash agent-id record mevedel-agent-runtime--background-watchdogs)
            (list :state 'armed
                  :record record
                  :remaining mevedel-agent-no-progress-timeout))
        (let ((decision
               (mevedel-agent-runtime--agent-no-progress-assess record)))
          (when-let* ((updated (plist-get decision :record)))
            (puthash agent-id updated mevedel-agent-runtime--background-watchdogs))
          decision))))))

(defun mevedel-agent-runtime--bwait-watchdog-delay (remaining)
  "Return the next BWAIT watchdog delay from REMAINING grace times."
  (let (delays)
    (when (and (integerp mevedel-agent-background-timeout)
               (> mevedel-agent-background-timeout 0))
      (push mevedel-agent-background-timeout delays))
    (dolist (remaining remaining)
      (when-let* ((delay (mevedel-agent-runtime--watchdog-delay remaining)))
        (push delay delays)))
    (when delays
      (apply #'min delays))))

(defun mevedel-agent-runtime--bwait-watchdog-expire (fsm)
  "Check FSM's BWAIT background agents after the watchdog timeout.

A no-op unless FSM is still parked in BWAIT when the timer fires.

The watchdog removes stranded agents and stops live agents that made
no progress for `mevedel-agent-no-progress-timeout'.  Slow-but-running
agents stay tracked and keep the parent parked in BWAIT."
  (when (eq (gptel-fsm-state fsm) 'BWAIT)
    (let* ((info (gptel-fsm-info fsm))
           (parent-buffer (plist-get info :buffer))
           (ctx (mevedel-agent-runtime--context-for-fsm fsm))
           (pending (and ctx (mevedel-agent-runtime--ctx-background-agents ctx)))
           live
           live-remaining
           no-progress
           stranded)
      (dolist (agent-id pending)
        (let ((child-fsm
               (mevedel-agent-runtime--background-agent-fsm agent-id parent-buffer)))
          (if (mevedel-agent-runtime--agent-fsm-live-p child-fsm)
              (let* ((invocation
                      (mevedel-agent-runtime--agent-invocation-at child-fsm))
                     (decision
                      (mevedel-agent-runtime--background-watchdog-assess
                       agent-id invocation parent-buffer))
                     (state (plist-get decision :state)))
                (if (eq state 'stop)
                    (push (cons agent-id decision) no-progress)
                  (push agent-id live)
                  (when-let* ((remaining (plist-get decision :remaining)))
                    (push remaining live-remaining))))
            (push (cons agent-id child-fsm) stranded))))
      (setq live (nreverse live)
            live-remaining (nreverse live-remaining)
            no-progress (nreverse no-progress)
            stranded (nreverse stranded))
      (when stranded
        (warn "mevedel: BWAIT watchdog fired after %ss; stranded agents: %S"
              mevedel-agent-background-timeout (mapcar #'car stranded)))
      (dolist (entry stranded)
        (let ((agent-id (car entry))
	      (child-fsm (cdr entry)))
	  (when (and ctx
	             (not (mevedel-agent-runtime--delivered-agent-result-p
	                   ctx parent-buffer agent-id)))
	    (mevedel-agent-runtime--complete-stranded-background-agent
	     ctx agent-id child-fsm parent-buffer))
	  (when ctx
	    (mevedel-agent-runtime--ctx-remove-background-agent ctx agent-id))
          (mevedel-agent-runtime--background-watchdog-cancel agent-id)
	  (when (and parent-buffer (buffer-live-p parent-buffer))
	    (with-current-buffer parent-buffer
	      (setq mevedel-agent-runtime--fsms
	            (assoc-delete-all agent-id mevedel-agent-runtime--fsms))))))
      (dolist (entry no-progress)
        (let* ((agent-id (car entry))
               (decision (cdr entry))
               (elapsed (plist-get decision :elapsed))
               (reason
                (format "background agent made no progress for %ss while parent was waiting in BWAIT"
                        mevedel-agent-no-progress-timeout)))
          (message "mevedel: BWAIT watchdog stopping %s after %.0fs without progress"
                   agent-id (or elapsed mevedel-agent-no-progress-timeout))
          (condition-case err
              (progn
                (mevedel-agent-runtime-stop agent-id reason parent-buffer)
                (mevedel-agent-runtime--background-watchdog-cancel agent-id))
            (error
             (message "mevedel: BWAIT watchdog stop failed for %s: %S"
                      agent-id err)
             (push agent-id live)
             (when mevedel-agent-no-progress-timeout
               (push mevedel-agent-no-progress-timeout live-remaining))))))
      (when (eq (gptel-fsm-state fsm) 'BWAIT)
        (cond
         (live
          (message "mevedel: BWAIT watchdog still waiting after %ss; running agents: %S; use StopAgent(agent_id=\"...\") or M-x mevedel-stop-agent to stop one"
                   mevedel-agent-background-timeout live)
          (when-let* ((delay (mevedel-agent-runtime--bwait-watchdog-delay
                              live-remaining)))
            (run-at-time delay nil
                         #'mevedel-agent-runtime--bwait-watchdog-expire fsm)))
         ((and ctx (mevedel-agent-runtime--ctx-messages ctx))
          (gptel--fsm-transition fsm 'WAIT))
         (t
          (gptel--fsm-transition fsm 'DONE)))))))

(defun mevedel-agent-runtime--handle-bwait (fsm)
  "Handler for the BWAIT (background wait) state.

Parks the FSM without firing a new HTTP request.  The background
agent's completion callback will resume the FSM by transitioning
from BWAIT to WAIT.

If background agents have already delivered results to the mailbox
\(no agents pending but messages queued), transitions to WAIT
immediately so the message-inject handler can drain the mailbox.

Arms a watchdog timer for stranded-agent recovery and shared
no-progress auto-stop so a lost or stuck child cannot park the FSM
forever."
  (when-let* ((info (gptel-fsm-info fsm)))
    (let* ((parent-buffer (plist-get info :buffer))
           (ctx (or (and (mevedel-agent-invocation-p
                         (plist-get info :mevedel-agent-invocation))
                        (plist-get info :mevedel-agent-invocation))
                    (when (and parent-buffer
                               (buffer-live-p parent-buffer))
                      (buffer-local-value 'mevedel--session
                                          parent-buffer)))))
      (if (and ctx
               (not (mevedel-agent-runtime--ctx-background-agents ctx))
               (mevedel-agent-runtime--ctx-messages ctx))
          ;; No agents pending but messages waiting -- go straight to WAIT.
          (gptel--fsm-transition fsm 'WAIT)
        ;; Background agents still running -- park and wait.
        (when-let* ((buf (plist-get info :buffer))
                    ((buffer-live-p buf)))
          (with-current-buffer buf
            (gptel--update-status " Waiting for agents..." 'warning)))
        (when-let* ((delay
                     (mevedel-agent-runtime--bwait-watchdog-delay
                      (mevedel-agent-runtime--background-watchdog-arm-live
                       ctx parent-buffer))))
          (run-at-time delay nil
                       #'mevedel-agent-runtime--bwait-watchdog-expire fsm))))))

(defun mevedel-agent-runtime--inject-bwait-transition (fsm terminal-handler)
  "Modify FSM's transition table to add the BWAIT parking state.

Inserts a `mevedel-agent-runtime--background-agents-pending-p' predicate
before the `(t . DONE)' fallthrough in both the TYPE and TRET states.
When the predicate matches, the FSM parks in BWAIT instead of
terminating.

Also adds BWAIT to the handler alist with
`mevedel-agent-runtime--handle-bwait', and registers BWAIT as a valid state in
the transition table (with no outgoing transitions -- the background
agent callback forces a transition to WAIT explicitly).

The transition-table mutation is cached via
`mevedel-agent-runtime--bwait-injected-table' so repeat spawns reuse a
shared injected copy instead of paying a fresh `copy-tree' each time."
  (setf (gptel-fsm-table fsm)
        (mevedel-agent-runtime--bwait-injected-table (gptel-fsm-table fsm)))
  ;; Add BWAIT handler plus terminal-state mailbox guard so orphaned
  ;; messages are at least logged when the sub-agent ends abnormally.
  (setf (gptel-fsm-handlers fsm)
        (mevedel-agent-runtime--augment-agent-handlers
         (gptel-fsm-handlers fsm)
         :append
         (append
          `((BWAIT . (,#'mevedel-agent-runtime--handle-bwait)))
          (when terminal-handler
            `((DONE . (,terminal-handler))
              (ERRS . (,terminal-handler))
              (ABRT . (,terminal-handler))))))))

(defun mevedel-agent-runtime--configure-fsm
    (fsm invocation message-handler terminal-handler)
  "Install lifecycle handlers on FSM and register INVOCATION before launch."
  (setf (gptel-fsm-handlers fsm)
        (mevedel-agent-runtime--augment-agent-handlers
         (gptel-fsm-handlers fsm)
         :prepend
         `((WAIT . (,@(and message-handler (list message-handler))
                     ,#'mevedel-agent-runtime--handle-wait-inject)))))
  (mevedel-agent-runtime--inject-bwait-transition fsm terminal-handler)
  (when-let* ((agent-id
               (mevedel-agent-invocation-agent-id invocation))
              (parent-buffer
               (mevedel-agent-invocation-parent-data-buffer invocation))
              ((buffer-live-p parent-buffer)))
    (with-current-buffer parent-buffer
      (setf (alist-get agent-id mevedel-agent-runtime--fsms nil nil #'equal)
            fsm))))

;;
;;; Agent-result format / parse helpers

(defun mevedel-agent-runtime--xml-attr-escape (s)
  "Escape S for use as a double-quoted XML attribute value."
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    ">" "&gt;"
    (replace-regexp-in-string
     "\"" "&quot;"
     (replace-regexp-in-string
      "&" "&amp;" (or s ""))))))

(defun mevedel-agent-runtime--agent-result-body-escape (body)
  "Escape mailbox delimiter-looking text in BODY."
  (let ((text (or body "")))
    (dolist (pair '(("<agent-result" . "&lt;agent-result")
                    ("</agent-result>" . "&lt;/agent-result&gt;")
                    ("<agent-message" . "&lt;agent-message")
                    ("</agent-message>" . "&lt;/agent-message&gt;")))
      (setq text
            (replace-regexp-in-string (regexp-quote (car pair))
                                      (cdr pair)
                                      text t t)))
    text))

(defun mevedel-agent-runtime--agent-result-format (agent-id agent-type description body)
  "Return an AGENT-ID result block for AGENT-TYPE, DESCRIPTION, and BODY.

`agent-id', `type', and `description' attributes are XML-escaped so
LLM-supplied descriptions containing quote characters cannot break
out of the attribute string.  BODY delimiter strings are escaped so
nested examples cannot terminate the outer mailbox block."
  (format
   "<agent-result agent-id=\"%s\" type=\"%s\" description=\"%s\">\n%s\n</agent-result>"
   (mevedel-agent-runtime--xml-attr-escape agent-id)
   (mevedel-agent-runtime--xml-attr-escape agent-type)
   (mevedel-agent-runtime--xml-attr-escape description)
   (mevedel-agent-runtime--agent-result-body-escape body)))

(defun mevedel-agent-runtime--agent-result-parse-id (text)
  "Return the `agent-id' attribute parsed out of an `<agent-result>` in TEXT.
Returns nil if no agent-result block is found.  Used by the view
buffer scanner to join mailbox-delivered results back to their
transcript entries."
  (when (and (stringp text)
             (string-match
              "<agent-result[^>]*agent-id=\"\\([^\"]+\\)\""
              text))
    (match-string 1 text)))

(defun mevedel-agent-runtime--ctx-has-agent-result-p (ctx agent-id)
  "Return non-nil if CTX already has an `<agent-result>' for AGENT-ID."
  (cl-some
   (lambda (msg)
     (and (plist-get msg :agent-result-p)
          (equal agent-id
                 (mevedel-agent-runtime--agent-result-parse-id
                  (plist-get msg :body)))))
   (and ctx (mevedel-agent-runtime--ctx-messages ctx))))

(defun mevedel-agent-runtime--buffer-has-agent-result-p (buffer agent-id)
  "Return non-nil if BUFFER already has an agent result for AGENT-ID."
  (when (and (buffer-live-p buffer) (stringp agent-id))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (re-search-forward
           (format "<agent-result[^>]*agent-id=\"%s\""
                   (regexp-quote
                    (mevedel-agent-runtime--xml-attr-escape agent-id)))
           nil t))))))

(defun mevedel-agent-runtime--delivered-agent-result-p (ctx buffer agent-id)
  "Return non-nil if CTX or BUFFER already has a result for AGENT-ID."
  (or (mevedel-agent-runtime--ctx-has-agent-result-p ctx agent-id)
      (mevedel-agent-runtime--buffer-has-agent-result-p buffer agent-id)))

(defconst mevedel-agent-runtime--stopped-agent-partial-max-chars (* 32 1024)
  "Maximum number of partial response characters to inline after a stop.")

(defconst mevedel-agent-runtime--background-agent-result-max-chars (* 32 1024)
  "Maximum number of background agent result characters to inline.")

(defun mevedel-agent-runtime--bound-background-agent-result (invocation response)
  "Return a bounded background-agent RESPONSE for INVOCATION."
  (let ((body (or response "(no response)")))
    (if (or (not (stringp body))
            (<= (length body) mevedel-agent-runtime--background-agent-result-max-chars))
        body
      (let* ((rel (mevedel-agent-invocation-transcript-relative-path invocation))
             (cut (let ((nl (cl-position
                             ?\n body :from-end t
                             :end mevedel-agent-runtime--background-agent-result-max-chars)))
                    (if (and nl
                             (> nl (/ mevedel-agent-runtime--background-agent-result-max-chars
                                      2)))
                        nl
                      mevedel-agent-runtime--background-agent-result-max-chars))))
        (concat
         (format "Background agent result too large (%d chars).\n" (length body))
         (if (and rel (not (string-empty-p rel)))
             (format "Full transcript: %s\n\n" rel)
           "No saved transcript path was available. Showing a bounded preview.\n\n")
         (format "Preview (first %d chars):\n" cut)
         (substring body 0 cut)
         "\n...\n")))))

(defun mevedel-agent-runtime--background-response-summary (response)
  "Return a compact one-line summary from background agent RESPONSE."
  (when (stringp response)
    (let ((line (cl-find-if
                 (lambda (s) (not (string-empty-p (string-trim s))))
                 (split-string response "\n"))))
      (when line
        (let ((trimmed (string-trim line)))
          (if (> (length trimmed) 240)
              (concat (substring trimmed 0 240) "...")
            trimmed))))))

(defun mevedel-agent-runtime--queue-background-status-reminder
    (ctx agent-id agent-type description status &optional transcript response
         reason)
  "Queue CTX status reminder for AGENT-ID, AGENT-TYPE, and DESCRIPTION.
STATUS is the new state.  TRANSCRIPT, RESPONSE, and REASON add detail
when available."
  (when (mevedel-session-p ctx)
    (let ((summary (mevedel-agent-runtime--background-response-summary response)))
      (mevedel-session-enqueue-pending-reminder
       ctx
       (concat
        (format
         "Background agent status changed: `%s' (%s) is now %s."
         agent-id agent-type status)
        (when (and description (not (string-empty-p description)))
          (format " Task: %s." description))
        (when (and (stringp reason) (not (string-empty-p reason)))
          (format " Reason: %s." reason))
        (when (and transcript (not (string-empty-p transcript)))
          (format " Transcript: %s." transcript))
        (when summary
          (format " Latest summary: %s." summary))
        " Review its `<agent-result>' block before finalizing the parent task.")))))

(defun mevedel-agent-runtime--verifier-verdict (response invocation)
  "Return verifier verdict symbol parsed from RESPONSE for INVOCATION.

Only verifier agents are inspected.  Returns one of `pass', `fail',
`partial', or nil when no literal final-verdict line is present."
  (when (and (stringp response)
             (mevedel-agent-invocation-p invocation)
             (let ((agent (mevedel-agent-invocation-agent invocation)))
               (and agent (equal (mevedel-agent-name agent) "verifier"))))
    (when-let* ((final-line
                 (cl-loop for line in (nreverse (split-string response "\n"))
                          for trimmed = (string-trim line)
                          unless (string-empty-p trimmed)
                          return trimmed))
                ((string-match
                  "\\`VERDICT: \\(PASS\\|FAIL\\|PARTIAL\\)\\'" final-line)))
      (intern (downcase (match-string 1 final-line))))))

(defun mevedel-agent-runtime--record-verifier-verdict (response invocation)
  "Parse and store verifier verdict from RESPONSE on INVOCATION.

Returns the parsed verdict symbol, or nil."
  (when-let* ((verdict (mevedel-agent-runtime--verifier-verdict response invocation)))
    (setf (mevedel-agent-invocation-verdict invocation) verdict)
    (when-let* ((session (mevedel-agent-invocation-parent-session invocation))
                (agent-id (mevedel-agent-invocation-agent-id invocation)))
      (when (fboundp 'mevedel-session-persistence--update-transcript-entry)
        (mevedel-session-persistence--update-transcript-entry
         session agent-id (list :verdict verdict))))
    verdict))

(defun mevedel-agent-runtime--intentional-stop-abort-response-p
    (invocation response)
  "Return non-nil when INVOCATION RESPONSE is superseded by StopAgent output."
  (and (mevedel-agent-invocation-terminal-reason invocation)
       (eq (mevedel-agent-invocation-transcript-status invocation) 'aborted)
       (or (not (stringp response))
           (not (string-match-p
                 (regexp-quote "was stopped before it could finish")
                 response)))))

(defun mevedel-agent-runtime--request-fsm-buffer (fsm)
  "Return FSM's live request buffer, or nil."
  (when-let* ((info (and fsm (gptel-fsm-info fsm)))
              (buf (plist-get info :buffer))
              ((buffer-live-p buf)))
    buf))

(defun mevedel-agent-runtime--live-bwait-fsm-for-buffer (buffer)
  "Return BUFFER's live request FSM when it is parked in BWAIT."
  (when (and buffer (buffer-live-p buffer) (boundp 'gptel--request-alist))
    (cl-loop for entry in gptel--request-alist
             for fsm = (cadr entry)
             when (and fsm
                       (eq (gptel-fsm-state fsm) 'BWAIT)
                       (eq (mevedel-agent-runtime--request-fsm-buffer fsm) buffer))
             return fsm)))

(defun mevedel-agent-runtime--parent-bwait-fsm (invocation)
  "Return INVOCATION's live parent FSM when it is parked in BWAIT."
  (when (mevedel-agent-invocation-p invocation)
    (let ((stored (mevedel-agent-invocation-parent-fsm invocation))
          (parent-buffer
           (mevedel-agent-invocation-parent-data-buffer invocation)))
      (or (and stored
               (eq (gptel-fsm-state stored) 'BWAIT)
               stored)
          (when-let* ((fsm (mevedel-agent-runtime--live-bwait-fsm-for-buffer
                            parent-buffer)))
            (setf (mevedel-agent-invocation-parent-fsm invocation) fsm)
            fsm)))))

(defun mevedel-agent-runtime--remove-agent-registry-entry
    (invocation agent-id &optional parent-buffer)
  "Remove AGENT-ID from INVOCATION's parent agent registry."
  (mevedel-agent-runtime--foreground-watchdog-cancel agent-id)
  (mevedel-agent-runtime--background-watchdog-cancel agent-id)
  (let ((buf (or parent-buffer
                 (and (mevedel-agent-invocation-p invocation)
                      (mevedel-agent-invocation-parent-data-buffer
                       invocation)))))
    (when (and (stringp agent-id) buf (buffer-live-p buf))
      (with-current-buffer buf
        (setq mevedel-agent-runtime--fsms
              (assoc-delete-all agent-id mevedel-agent-runtime--fsms))))))

(defun mevedel-agent-runtime--complete-foreground-agent (invocation response)
  "Deliver foreground INVOCATION's RESPONSE to its parent tool callback.

Returns non-nil when the parent callback has reported a result."
  (when (mevedel-agent-invocation-p invocation)
    (let ((callback
           (mevedel-agent-invocation-parent-tool-callback invocation)))
      (when (functionp callback)
        (condition-case err
            (funcall callback response)
          (error
           (message "mevedel: foreground agent completion error: %S" err))))
      (when (mevedel-agent-invocation-foreground-result-reported-p invocation)
        (mevedel-agent-runtime--foreground-watchdog-cancel
         (mevedel-agent-invocation-agent-id invocation)))
      (mevedel-agent-invocation-foreground-result-reported-p invocation))))

(defun mevedel-agent-runtime--invoke-agent-abort-callback (fsm)
  "Invoke FSM's agent callback with `abort', returning non-nil on success."
  (when-let* ((info (and fsm (gptel-fsm-info fsm)))
              (callback (plist-get info :callback))
              ((functionp callback)))
    (condition-case err
        (progn
          (funcall callback 'abort info)
          t)
      (error
       (message "mevedel: agent abort callback error: %S" err)
       nil))))

(defun mevedel-agent-runtime--complete-background-agent (invocation response)
  "Deliver background INVOCATION's RESPONSE and clear parent tracking.

This is the shared terminal path for normal callbacks and direct FSM
ERRS handling.  It is idempotent after a successful mailbox push so
gptel's current \"callback nil, then transition ERRS\" sequence does
not deliver duplicate `<agent-result>' blocks."
  (when (and (mevedel-agent-invocation-p invocation)
             (mevedel-agent-invocation-background-p invocation)
             (not (mevedel-agent-runtime--intentional-stop-abort-response-p
                   invocation response)))
    (let* ((agent (mevedel-agent-invocation-agent invocation))
           (agent-type (or (and agent (mevedel-agent-name agent)) "agent"))
           (agent-id (mevedel-agent-invocation-agent-id invocation))
           (description (or (mevedel-agent-invocation-description invocation)
                            ""))
           (parent-ctx (mevedel-agent-invocation-parent-context invocation))
           (parent-fsm (mevedel-agent-runtime--parent-bwait-fsm invocation))
           (parent-data-buffer
            (mevedel-agent-invocation-parent-data-buffer invocation))
           (verdict (mevedel-agent-runtime--record-verifier-verdict
                     response invocation))
           (pushed nil))
      (when (and parent-ctx
                 (not (mevedel-agent-invocation-background-result-reported-p
                       invocation)))
        (condition-case err
            (progn
              (mevedel-agent-runtime--ctx-push-message
               parent-ctx
               (list :from agent-id
                     :body (mevedel-agent-runtime--agent-result-format
                            agent-id agent-type description
                            (mevedel-agent-runtime--bound-background-agent-result
                             invocation response))
                     :agent-result-p t
                     :timestamp (current-time)))
              (setq pushed t))
          (error
           (message "mevedel-agent-runtime-dispatch bg push error: %S" err)))
        (when pushed
          (setf (mevedel-agent-invocation-background-result-reported-p
                 invocation)
                t)))
      (when parent-ctx
        (condition-case err
            (mevedel-agent-runtime--ctx-remove-background-agent parent-ctx agent-id)
          (error
           (message "mevedel-agent-runtime-dispatch bg remove error: %S" err))))
      (when (mevedel-session-p parent-ctx)
        (let* ((status (or (mevedel-agent-invocation-transcript-status
                            invocation)
                           'completed))
               (reason (mevedel-agent-invocation-terminal-reason invocation))
               (transcript
                (mevedel-agent-invocation-transcript-relative-path
                 invocation)))
          (mevedel-agent-runtime--queue-background-status-reminder
           parent-ctx agent-id agent-type description status transcript
           response reason)))
      (mevedel-agent-runtime--remove-agent-registry-entry
       invocation agent-id parent-data-buffer)
      (when (and verdict (fboundp 'mevedel-agent-exec--handle-update))
        (mevedel-agent-exec--handle-update invocation))
      ;; Persist the mailbox addition so an Emacs crash between push
      ;; and the parent's next WAIT-drain does not lose the
      ;; agent-result.  Best-effort: the helper is gated on the
      ;; sidecar already existing on disk.
      (when (and pushed
                 (mevedel-session-p parent-ctx)
                 (fboundp 'mevedel-session-persistence--write-sidecar-now)
                 parent-data-buffer
                 (buffer-live-p parent-data-buffer))
        (condition-case err
            (mevedel-session-persistence--write-sidecar-now
             parent-ctx parent-data-buffer)
          (error
           (message "mevedel-agent-runtime-dispatch bg sidecar write error: %S"
                    err))))
      (when (and parent-fsm
                 (eq (gptel-fsm-state parent-fsm) 'BWAIT))
        (gptel--fsm-transition parent-fsm 'WAIT)))))


;;
;;; Agent stop control

(defun mevedel-agent-runtime--agent-terminal-status-p (status)
  "Return non-nil when STATUS is terminal for an agent transcript."
  (memq status '(completed error aborted incomplete)))

(defun mevedel-agent-runtime--agent-no-progress-enabled-p ()
  "Return non-nil when agent no-progress auto-stop is enabled."
  (and (integerp mevedel-agent-no-progress-timeout)
       (> mevedel-agent-no-progress-timeout 0)))

(defun mevedel-agent-runtime--agent-latest-activity-time (invocation)
  "Return INVOCATION's latest recorded activity time, or nil."
  (when (mevedel-agent-invocation-p invocation)
    (let (latest)
      (dolist (item (mevedel-agent-invocation-activity invocation))
        (let ((time (plist-get item :time)))
          (when (and (numberp time)
                     (or (not latest) (> time latest)))
            (setq latest time))))
      latest)))

(defun mevedel-agent-runtime--agent-progress-snapshot (invocation)
  "Return no-progress watchdog snapshot for INVOCATION."
  (let ((buf (and (mevedel-agent-invocation-p invocation)
                  (mevedel-agent-invocation-buffer invocation))))
    (list :buffer-size (and (buffer-live-p buf)
                            (with-current-buffer buf
                              (buffer-size)))
          :call-count (or (and (mevedel-agent-invocation-p invocation)
                               (mevedel-agent-invocation-call-count
                                invocation))
                          0)
          :activity-time
          (mevedel-agent-runtime--agent-latest-activity-time invocation))))

(defun mevedel-agent-runtime--agent-progress-snapshot-changed-p (snapshot record)
  "Return non-nil when SNAPSHOT differs from watchdog RECORD."
  (not (and (equal (plist-get snapshot :buffer-size)
                   (plist-get record :buffer-size))
            (equal (plist-get snapshot :call-count)
                   (plist-get record :call-count))
            (equal (plist-get snapshot :activity-time)
                   (plist-get record :activity-time)))))

(defun mevedel-agent-runtime--agent-progress-observed-at (snapshot record now)
  "Return best progress timestamp for SNAPSHOT, RECORD, and NOW."
  (let ((current-activity (plist-get snapshot :activity-time))
        (last-activity (plist-get record :activity-time)))
    (if (and (numberp current-activity)
             (or (not (numberp last-activity))
                 (> current-activity last-activity)))
        current-activity
      now)))

(defun mevedel-agent-runtime--agent-watchdog-record
    (invocation parent-buffer &optional now extra)
  "Return watchdog record for INVOCATION, PARENT-BUFFER, NOW, and EXTRA."
  (append (list :invocation invocation
                :parent-buffer parent-buffer
                :last-progress-at (or now (float-time)))
          (mevedel-agent-runtime--agent-progress-snapshot invocation)
          extra))

(defun mevedel-agent-runtime--agent-watchdog-update-record
    (record snapshot last-progress-at)
  "Return RECORD updated with SNAPSHOT and LAST-PROGRESS-AT."
  (let ((updated (copy-sequence record)))
    (setq updated (plist-put updated :buffer-size
                             (plist-get snapshot :buffer-size)))
    (setq updated (plist-put updated :call-count
                             (plist-get snapshot :call-count)))
    (setq updated (plist-put updated :activity-time
                             (plist-get snapshot :activity-time)))
    (setq updated (plist-put updated :last-progress-at last-progress-at))
    updated))

(defun mevedel-agent-runtime--agent-no-progress-assess (record &optional now)
  "Assess RECORD at NOW and return a no-progress watchdog decision plist."
  (let* ((now (or now (float-time)))
         (invocation (plist-get record :invocation))
         (snapshot (mevedel-agent-runtime--agent-progress-snapshot invocation))
         (changed (mevedel-agent-runtime--agent-progress-snapshot-changed-p
                   snapshot record))
         (last-progress-at (or (plist-get record :last-progress-at)
                               now))
         (timeout mevedel-agent-no-progress-timeout))
    (cond
     ((not (mevedel-agent-runtime--agent-no-progress-enabled-p))
      (list :state 'disabled
            :snapshot snapshot
            :last-snapshot record))
     (changed
      (let* ((progress-at
              (mevedel-agent-runtime--agent-progress-observed-at
               snapshot record now))
             (elapsed (max 0 (- now progress-at)))
             (remaining (max 0 (- timeout elapsed)))
             (updated
              (mevedel-agent-runtime--agent-watchdog-update-record
               record snapshot progress-at)))
        (if (>= elapsed timeout)
            (list :state 'stop
                  :record updated
                  :snapshot snapshot
                  :last-snapshot record
                  :last-progress-at progress-at
                  :elapsed elapsed
                  :remaining 0)
          (list :state 'progress
                :record updated
                :snapshot snapshot
                :last-snapshot record
                :last-progress-at progress-at
                :elapsed elapsed
                :remaining remaining))))
     (t
      (let* ((elapsed (max 0 (- now last-progress-at)))
             (remaining (max 0 (- timeout elapsed))))
        (if (>= elapsed timeout)
            (list :state 'stop
                  :snapshot snapshot
                  :last-snapshot record
                  :last-progress-at last-progress-at
                  :elapsed elapsed
                  :remaining 0)
          (list :state 'wait
                :record record
                :snapshot snapshot
                :last-snapshot record
                :last-progress-at last-progress-at
                :elapsed elapsed
                :remaining remaining)))))))

(defun mevedel-agent-runtime--watchdog-delay (remaining)
  "Return a positive watchdog delay from REMAINING or the shared timeout."
  (let ((delay (or remaining mevedel-agent-no-progress-timeout)))
    (when (and (numberp delay) (> delay 0))
      (max 1 (ceiling delay)))))

(defun mevedel-agent-runtime--foreground-watchdog-enabled-p ()
  "Return non-nil when foreground agent no-progress watchdog is enabled."
  (mevedel-agent-runtime--agent-no-progress-enabled-p))

(defun mevedel-agent-runtime--foreground-watchdog-cancel (agent-id)
  "Cancel the foreground no-progress watchdog for AGENT-ID."
  (when (stringp agent-id)
    (when-let* ((record (gethash agent-id
                                 mevedel-agent-runtime--foreground-watchdogs)))
      (let ((timer (plist-get record :timer)))
        (when (timerp timer)
          (cancel-timer timer))))
    (remhash agent-id mevedel-agent-runtime--foreground-watchdogs)))

(defun mevedel-agent-runtime--foreground-watchdog-schedule
    (agent-id record &optional delay)
  "Schedule foreground watchdog for AGENT-ID with RECORD and DELAY."
  (when (mevedel-agent-runtime--foreground-watchdog-enabled-p)
    (when-let* ((delay (mevedel-agent-runtime--watchdog-delay delay)))
      (let ((timer (run-at-time
                    delay nil
                    #'mevedel-agent-runtime--foreground-watchdog-expire agent-id)))
        (puthash agent-id
                 (plist-put record :timer timer)
                 mevedel-agent-runtime--foreground-watchdogs)))))

(defun mevedel-agent-runtime--foreground-watchdog-arm (invocation)
  "Arm a no-progress watchdog for foreground INVOCATION."
  (when (and (mevedel-agent-runtime--foreground-watchdog-enabled-p)
             (mevedel-agent-invocation-p invocation)
             (not (mevedel-agent-invocation-background-p invocation)))
    (when-let* ((agent-id (mevedel-agent-invocation-agent-id invocation)))
      (mevedel-agent-runtime--foreground-watchdog-cancel agent-id)
      (mevedel-agent-runtime--foreground-watchdog-schedule
       agent-id
       (mevedel-agent-runtime--agent-watchdog-record
        invocation
        (mevedel-agent-invocation-parent-data-buffer invocation))))))

(defun mevedel-agent-runtime--foreground-watchdog-record
    (invocation agent-id state reason decision)
  "Persist watchdog observation for INVOCATION, AGENT-ID, STATE, and REASON.
DECISION is the assessment plist."
  (let* ((session (and (mevedel-agent-invocation-p invocation)
                       (mevedel-agent-invocation-parent-session invocation)))
         (parent-buffer
          (and (mevedel-agent-invocation-p invocation)
               (mevedel-agent-invocation-parent-data-buffer invocation)))
         (snapshot (plist-get decision :snapshot))
         (last-snapshot (plist-get decision :last-snapshot))
         (entry (list :state state
                      :reason reason
                      :timeout mevedel-agent-no-progress-timeout
                      :current-size (plist-get snapshot :buffer-size)
                      :current-calls (plist-get snapshot :call-count)
                      :current-activity-time
                      (plist-get snapshot :activity-time)
                      :last-size (plist-get last-snapshot :buffer-size)
                      :last-calls (plist-get last-snapshot :call-count)
                      :last-activity-time
                      (plist-get last-snapshot :activity-time)
                      :elapsed (plist-get decision :elapsed)
                      :remaining (plist-get decision :remaining)
                      :updated-at (format-time-string "%FT%H-%M-%S"))))
    (message "mevedel: foreground watchdog %s %s (%s)"
             state agent-id reason)
    (when (and (mevedel-session-p session) agent-id)
      (condition-case err
          (progn
            (require 'mevedel-session-persistence)
            (mevedel-session-persistence--update-transcript-entry
             session agent-id (list :watchdog entry))
            (when (and parent-buffer (buffer-live-p parent-buffer))
              (mevedel-session-persistence--write-sidecar-now
               session parent-buffer)))
        (error
         (message "mevedel: foreground watchdog metadata write failed: %S"
                  err))))))

(defun mevedel-agent-runtime--foreground-watchdog-expire (agent-id)
  "Stop foreground AGENT-ID when it made no progress since the last tick."
  (when-let* ((record (gethash agent-id
                               mevedel-agent-runtime--foreground-watchdogs)))
    (let* ((invocation (plist-get record :invocation))
           (parent-buffer (plist-get record :parent-buffer))
           (status (and (mevedel-agent-invocation-p invocation)
                        (mevedel-agent-invocation-transcript-status
                         invocation))))
      (cond
       ((or (not (mevedel-agent-invocation-p invocation))
            (mevedel-agent-runtime--agent-terminal-status-p status)
            (mevedel-agent-invocation-foreground-result-reported-p
             invocation)
            (not (buffer-live-p parent-buffer)))
        (mevedel-agent-runtime--foreground-watchdog-cancel agent-id))
       ((not (mevedel-agent-runtime--foreground-watchdog-enabled-p))
        (mevedel-agent-runtime--foreground-watchdog-cancel agent-id))
       (t
        (let* ((decision
                (mevedel-agent-runtime--agent-no-progress-assess record))
               (state (plist-get decision :state)))
          (cond
           ((mevedel-agent-runtime--ctx-background-agents invocation)
            (mevedel-agent-runtime--foreground-watchdog-record
             invocation agent-id 'rescheduled 'background-agents decision)
            (mevedel-agent-runtime--foreground-watchdog-schedule
             agent-id
             (or (plist-get decision :record) record)
             (let ((remaining (plist-get decision :remaining)))
               (and (numberp remaining) (> remaining 0) remaining))))
           ((mevedel-agent-runtime--ctx-messages invocation)
            (mevedel-agent-runtime--foreground-watchdog-record
             invocation agent-id 'rescheduled 'messages decision)
            (mevedel-agent-runtime--foreground-watchdog-schedule
             agent-id
             (or (plist-get decision :record) record)
             (let ((remaining (plist-get decision :remaining)))
               (and (numberp remaining) (> remaining 0) remaining))))
           ((memq state '(progress wait))
            (let ((reason (if (eq state 'progress) 'progress 'grace)))
              (mevedel-agent-runtime--foreground-watchdog-record
               invocation agent-id 'rescheduled reason decision)
              (mevedel-agent-runtime--foreground-watchdog-schedule
               agent-id
               (or (plist-get decision :record) record)
               (plist-get decision :remaining))))
           ((eq state 'stop)
            (mevedel-agent-runtime--foreground-watchdog-record
             invocation agent-id 'stopping 'no-progress decision)
            (mevedel-agent-runtime--foreground-watchdog-cancel agent-id)
            (condition-case err
                (mevedel-agent-runtime-stop
                 agent-id
                 (format "foreground agent made no progress for %ss"
                         mevedel-agent-no-progress-timeout)
                 parent-buffer)
              (error
               (message "mevedel: foreground watchdog stop failed: %S"
                        err)))))))))))

(defun mevedel-agent-runtime-live-invocations (&optional parent-buffer)
  "Return live agent registry entries from PARENT-BUFFER.
Entries are `(AGENT-ID . FSM)' conses.  Dead or terminal entries are
excluded.  PARENT-BUFFER defaults to the data buffer reachable from
the current buffer."
  (let ((buf (or parent-buffer (mevedel--prompt--data-buffer))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (cl-remove-if-not
         (lambda (entry)
           (and (stringp (car entry))
                (mevedel-agent-runtime--agent-fsm-live-p (cdr entry))))
         (copy-sequence mevedel-agent-runtime--fsms))))))

(defun mevedel-agent-runtime-display-label (agent-id)
  "Return the shortened display label for canonical AGENT-ID.
Canonical ids use `<type>--<32-char-md5>'; display labels keep the type
and the first eight suffix characters.  Return nil when AGENT-ID is nil."
  (when (stringp agent-id)
    (if-let* ((separator (string-search "--" agent-id)))
        (let* ((type (substring agent-id 0 separator))
               (suffix (substring agent-id (+ separator 2)))
               (short (substring suffix 0 (min 8 (length suffix)))))
          (concat type "--" short))
      agent-id)))

(defun mevedel-agent-runtime--resolve-agent-stop-target (target &optional parent-buffer)
  "Resolve TARGET to a live registry entry in PARENT-BUFFER.
TARGET may be the full agent id or an unambiguous displayed short id
like `reviewer--73512314'."
  (unless (stringp target)
    (error "Parameter agent_id is required"))
  (let* ((entries (mevedel-agent-runtime-live-invocations parent-buffer))
         (exact (assoc target entries)))
    (or exact
        (let ((matches
               (cl-remove-if-not
                (lambda (entry)
                  (equal target
                         (mevedel-agent-runtime-display-label
                          (car entry))))
                entries)))
          (cond
           ((null matches)
            (error "No running agent found for %s" target))
           ((cdr matches)
            (error "Agent id %s is ambiguous; use the full id" target))
           (t (car matches)))))))

(defun mevedel-agent-runtime--agent-request-live-p (buffer)
  "Return non-nil when BUFFER hosts an active gptel request."
  (and buffer
       (buffer-live-p buffer)
       (boundp 'gptel--request-alist)
       (cl-some
        (lambda (entry)
          (let* ((fsm (cadr entry))
                 (info (and fsm (gptel-fsm-info fsm))))
            (eq (and info (plist-get info :buffer)) buffer)))
        gptel--request-alist)))

(defun mevedel-agent-runtime--stop-agent-descendants (invocation reason)
  "Stop INVOCATION's live child agents with REASON.
Returns the list of descendant result plists."
  (let ((buf (and (mevedel-agent-invocation-p invocation)
                  (mevedel-agent-invocation-buffer invocation)))
        results)
    (when (and buf (buffer-live-p buf))
      (dolist (entry (mevedel-agent-runtime-live-invocations buf))
        (condition-case err
            (push (mevedel-agent-runtime-stop (car entry) reason buf) results)
          (error
           (push (list :agent-id (car entry)
                       :error (format "%S" err))
                 results)))))
    (nreverse results)))

(defun mevedel-agent-runtime--context-session (ctx)
  "Return the parent session associated with CTX, or nil."
  (cond
   ((mevedel-session-p ctx) ctx)
   ((mevedel-agent-invocation-p ctx)
    (mevedel-agent-invocation-parent-session ctx))))

(defun mevedel-agent-runtime--transcript-absolute-path (session rel-path)
  "Return readable absolute transcript path for REL-PATH under SESSION."
  (when-let* (((mevedel-session-p session))
              ((stringp rel-path))
              (save-path (mevedel-session-save-path session)))
    (condition-case err
        (progn
          (require 'mevedel-session-persistence)
          (let* ((path (expand-file-name rel-path save-path))
                 (real-save (file-truename save-path))
                 (real-path (and (file-exists-p path)
                                 (file-truename path))))
            (when (and (mevedel-session-persistence--validate-transcript-path
                        rel-path save-path)
                       real-path
                       (file-in-directory-p real-path real-save)
                       (not (file-symlink-p path))
                       (file-regular-p path)
                       (file-readable-p path))
              path)))
      (error
       (message "mevedel: transcript path validation failed: %S" err)
       nil))))

(defun mevedel-agent-runtime--stopped-agent-transcript-path (invocation)
  "Return INVOCATION's absolute transcript path when it is safe to expose."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (rel (mevedel-agent-invocation-transcript-relative-path
                    invocation))
              (session (mevedel-agent-invocation-parent-session invocation)))
    (mevedel-agent-runtime--transcript-absolute-path session rel)))

(defun mevedel-agent-runtime--fallback-partial-text (partial)
  "Return sanitized fallback PARTIAL text, or nil.
PARTIAL may include the synthetic agent-result header seeded by the
sub-agent callback accumulator.  Strip that header so inline recovery
shows only model-produced work."
  (when (stringp partial)
    (let* ((text (string-trim partial))
           (text (replace-regexp-in-string
                  "\\`[[:alpha:]]+ result for task: [^\n]*\n\n" "" text)))
      (unless (string-empty-p (string-trim text))
        (string-trim text)))))

(defun mevedel-agent-runtime--agent-partial-text (invocation &optional fallback-partial)
  "Return bounded inline partial response for INVOCATION.
Prefer the latest assistant response recovered from the transcript buffer.
When that is unavailable, use FALLBACK-PARTIAL after stripping generated
callback scaffolding."
  (when-let* ((text (or (ignore-errors
                          (mevedel-agent-exec--final-response-text invocation))
                        (mevedel-agent-runtime--fallback-partial-text
                         fallback-partial)))
              (trimmed (string-trim text))
              ((not (string-empty-p trimmed))))
    (if (> (length trimmed) mevedel-agent-runtime--stopped-agent-partial-max-chars)
        (format "%s\n\n[Partial response truncated to %d characters.]"
                (substring trimmed
                           0 mevedel-agent-runtime--stopped-agent-partial-max-chars)
                mevedel-agent-runtime--stopped-agent-partial-max-chars)
      trimmed)))

(defun mevedel-agent-runtime--stopped-agent-partial-text (invocation)
  "Return a bounded inline partial response recovered from INVOCATION."
  (mevedel-agent-runtime--agent-partial-text invocation))

(defun mevedel-agent-runtime--agent-type-from-id (agent-id)
  "Infer an agent type prefix from AGENT-ID."
  (or (car (split-string (or agent-id "") "--" t))
      "agent"))

(defun mevedel-agent-runtime--stranded-agent-result-body
    (agent-id agent-type description transcript partial)
  "Return stranded result for AGENT-ID, AGENT-TYPE, DESCRIPTION, and TRANSCRIPT.
PARTIAL is included when no transcript is available."
  (concat
   (format "Error: Background agent %s became stranded before it could report a final result.

Reason: BWAIT watchdog found no live child FSM for this background agent.
Agent id: %s"
           agent-type agent-id)
   (when (and (stringp description) (not (string-empty-p description)))
     (format "\nTask: %s" description))
   (cond
    (transcript
     (format "\n\nTranscript: %s\nRead it with: Read(file_path=%S)"
             transcript transcript))
    (partial
     (format "\n\nPartial response recovered from live agent buffer:\n\n%s"
             partial))
    (t
     "\n\nNo saved transcript path was available, and no partial response \
could be recovered from the live agent buffer."))))

(defun mevedel-agent-runtime--complete-stranded-background-agent
    (ctx agent-id child-fsm parent-buffer)
  "Deliver a synthetic result for stranded background AGENT-ID.
CTX is the parent session or invocation holding the mailbox.  CHILD-FSM
may be nil or terminal; when it still carries an invocation, use it as a
last chance source for transcript and partial-response recovery."
  (let* ((invocation (and child-fsm
                          (ignore-errors
                            (mevedel-agent-runtime--agent-invocation-at child-fsm))))
         (session (or (and (mevedel-agent-invocation-p invocation)
                           (mevedel-agent-invocation-parent-session invocation))
                      (mevedel-agent-runtime--context-session ctx)))
         (entry (and session
                     (cdr (assoc agent-id
                                 (mevedel-session-agent-transcripts
                                  session)))))
         (agent (and (mevedel-agent-invocation-p invocation)
                     (mevedel-agent-invocation-agent invocation)))
         (agent-type (or (and agent (mevedel-agent-name agent))
                         (plist-get entry :agent-type)
                         (mevedel-agent-runtime--agent-type-from-id agent-id)))
         (description (or (and (mevedel-agent-invocation-p invocation)
                               (mevedel-agent-invocation-description
                                invocation))
                          (plist-get entry :description)
                          ""))
         (rel-path (or (and (mevedel-agent-invocation-p invocation)
                            (mevedel-agent-invocation-transcript-relative-path
                             invocation))
                       (plist-get entry :path)))
         (transcript (mevedel-agent-runtime--transcript-absolute-path
                      session rel-path))
         (partial (and (not transcript)
                       (mevedel-agent-invocation-p invocation)
                       (mevedel-agent-runtime--stopped-agent-partial-text invocation)))
         (response (mevedel-agent-runtime--stranded-agent-result-body
                    agent-id agent-type description transcript partial))
         (pushed nil))
    (condition-case err
        (progn
          (mevedel-agent-runtime--ctx-push-message
           ctx
           (list :from agent-id
                 :body (mevedel-agent-runtime--agent-result-format
                        agent-id agent-type description response)
                 :agent-result-p t
                 :timestamp (current-time)))
          (setq pushed t))
      (error
       (message "mevedel: BWAIT watchdog result push failed: %S" err)))
    (when session
      (condition-case err
          (progn
            (require 'mevedel-session-persistence)
            (mevedel-session-persistence--update-transcript-entry
             session agent-id
             (list :status 'incomplete
                   :reason
                   "BWAIT watchdog found no live child FSM"))
            (when (and parent-buffer (buffer-live-p parent-buffer))
              (mevedel-session-persistence--write-sidecar-now
               session parent-buffer)))
        (error
         (message "mevedel: BWAIT watchdog transcript update failed: %S"
                  err))))
    pushed))

(defun mevedel-agent-runtime--agent-recovery-text (invocation &optional fallback-partial)
  "Return transcript or partial recovery text for INVOCATION.
Prefer a safe transcript path.  If none is available, recover bounded
partial text from the live agent buffer or FALLBACK-PARTIAL."
  (let* ((transcript (mevedel-agent-runtime--stopped-agent-transcript-path invocation))
         (partial (unless transcript
                    (mevedel-agent-runtime--agent-partial-text
                     invocation fallback-partial))))
    (cond
     (transcript
      (format "\n\nTranscript: %s\nRead it with: Read(file_path=%S)"
              transcript transcript))
     (partial
      (format "\n\nPartial response recovered from live agent buffer:\n\n%s"
              partial))
     (t
      "\n\nNo saved transcript path was available, and no partial response \
could be recovered from the live agent buffer."))))

(defun mevedel-agent-runtime--agent-error-response
    (agent-id agent-type description error-details invocation
              &optional fallback-partial)
  "Return errored result body for AGENT-ID, AGENT-TYPE, and DESCRIPTION.
ERROR-DETAILS is formatted with `%S' to preserve provider/parser detail.
INVOCATION supplies recovery metadata.
FALLBACK-PARTIAL is used only when no safe transcript or transcript-buffer
assistant response can be recovered."
  (concat
   (format "Error: Task %s could not finish task \"%s\".

Error details: %S
Agent id: %s"
           agent-type description error-details agent-id)
   (mevedel-agent-runtime--agent-recovery-text invocation fallback-partial)))

(defun mevedel-agent-runtime--finalize (invocation status)
  "Commit terminal STATUS for INVOCATION exactly once.
The terminal transaction persists transcript metadata, finalizes owned
tasks, refreshes the parent view, runs the stop hook, and releases an
unobserved transcript buffer."
  (when (mevedel-agent-invocation-p invocation)
    (let ((current (mevedel-agent-invocation-transcript-status invocation)))
      (unless (memq current '(completed error aborted))
        (setf (mevedel-agent-invocation-transcript-status invocation) status)
        (let ((session (mevedel-agent-invocation-parent-session invocation))
              (parent-buf
               (mevedel-agent-invocation-parent-data-buffer invocation))
              (now (format-time-string "%FT%H-%M-%S")))
          (when session
            (require 'mevedel-session-persistence)
            (mevedel-session-persistence--update-transcript-entry
             session (mevedel-agent-invocation-agent-id invocation)
             (list :status status :updated-at now)))
          (mevedel-agent-exec--flush-transcript-save invocation)
          (let ((mevedel-agent-exec--suppress-activity-rerender t))
            (mevedel-agent-exec--record-activity
             invocation
             (list :type 'status :status status
                   :summary (format "%s" status))))
          (when (and session
                     (mevedel-agent-invocation-background-p invocation))
            (mevedel-session-persistence--update-transcript-entry
             session (mevedel-agent-invocation-agent-id invocation)
             (list :activity
                   (mevedel-agent-exec--final-activity-snapshot invocation))))
          (when (and session (eq status 'completed))
            (require 'mevedel-tool-task)
            (when (mevedel-tool-task-finalize-owner
                   session (mevedel-agent-invocation-agent-id invocation)
                   status)
              (when (buffer-live-p parent-buf)
                (with-current-buffer parent-buf
                  (mevedel-tool-task--refresh-display)))))
          (mevedel-agent-exec--handle-update invocation)
          (when (and session (buffer-live-p parent-buf))
            (mevedel-session-persistence--write-sidecar-now session parent-buf))
          (setf (mevedel-agent-invocation-activity invocation) nil)
          (mevedel-agent-exec--run-stop-hook invocation status)
          (let ((buf (mevedel-agent-invocation-buffer invocation))
                (view-kept
                 (and (fboundp 'mevedel-view-agent-live-transcript-finalize)
                      (mevedel-view-agent-live-transcript-finalize invocation))))
            (when (and buf (buffer-live-p buf)
                       (not view-kept)
                       (null (get-buffer-window-list buf nil t)))
              (with-current-buffer buf
                (remove-hook 'kill-buffer-hook
                             #'mevedel-agent-exec--on-buffer-kill t))
              (condition-case _
                  (kill-buffer buf)
                (error nil)))))))))

(defun mevedel-agent-runtime--terminal-event-response (invocation event)
  "Finalize INVOCATION and return its parent-visible terminal EVENT response."
  (let ((status (plist-get event :mevedel-agent-terminal-status)))
    (mevedel-agent-runtime--finalize invocation status)
    (pcase status
      ('error
       (let* ((agent (mevedel-agent-invocation-agent invocation))
              (agent-type (or (and agent (mevedel-agent-name agent)) "agent")))
         (mevedel-agent-runtime--agent-error-response
          (or (mevedel-agent-invocation-agent-id invocation) "unknown")
          agent-type
          (or (mevedel-agent-invocation-description invocation) "")
          (plist-get event :error-details)
          invocation
          (plist-get event :fallback-partial))))
      ('aborted
       (if-let* ((reason
                  (mevedel-agent-invocation-terminal-reason invocation)))
           (let* ((agent (mevedel-agent-invocation-agent invocation))
                  (agent-type
                   (or (and agent (mevedel-agent-name agent)) "agent")))
             (mevedel-agent-runtime--stop-agent-response
              (or (mevedel-agent-invocation-agent-id invocation) "unknown")
              agent-type
              (or (mevedel-agent-invocation-description invocation) "")
              reason invocation))
         (plist-get event :response)))
      (_ (plist-get event :response)))))

(defun mevedel-agent-runtime--normalize-terminal-response
    (invocation response)
  "Finalize INVOCATION for terminal RESPONSE and return its visible value."
  (if (and (listp response)
           (plist-get response :mevedel-agent-terminal-status))
      (mevedel-agent-runtime--terminal-event-response invocation response)
    (unless (mevedel-agent-runtime--agent-terminal-status-p
             (mevedel-agent-invocation-transcript-status invocation))
      (mevedel-agent-runtime--finalize invocation 'completed))
    response))

(defun mevedel-agent-runtime--stop-agent-response (agent-id agent-type description
                                                   reason invocation)
  "Return stopped result for AGENT-ID, AGENT-TYPE, DESCRIPTION, and REASON.
INVOCATION supplies recovery metadata."
  (concat
   (format "Error: Task %s was stopped before it could finish task \"%s\".

Stop reason: %s
Agent id: %s"
           agent-type description reason agent-id)
   (mevedel-agent-runtime--agent-recovery-text invocation)))

(defun mevedel-agent-runtime-stop (agent-id &optional reason parent-buffer)
  "Stop AGENT-ID owned by PARENT-BUFFER and return a result plist.
AGENT-ID may be a full id or unambiguous displayed short id.  REASON
defaults to a generic parent-requested stop message.  PARENT-BUFFER
defaults to the data buffer reachable from the current buffer."
  (let* ((buf (or parent-buffer (mevedel--prompt--data-buffer)))
         (entry (mevedel-agent-runtime--resolve-agent-stop-target agent-id buf))
         (resolved-id (car entry))
         (fsm (cdr entry))
         (invocation (mevedel-agent-runtime--agent-invocation-at fsm)))
    (unless (mevedel-agent-invocation-p invocation)
      (error "Agent %s has no live invocation" resolved-id))
    (let* ((agent (mevedel-agent-invocation-agent invocation))
           (agent-type (or (and agent (mevedel-agent-name agent)) "agent"))
           (description (or (mevedel-agent-invocation-description invocation)
                            ""))
           (stop-reason
            (if (and (stringp reason) (not (string-empty-p reason)))
                reason
              "stopped by parent request"))
           (previous-status
            (or (mevedel-agent-invocation-transcript-status invocation)
                (and (mevedel-agent-runtime--agent-fsm-live-p fsm) 'running)
                'unknown))
           (parent-fsm (mevedel-agent-runtime--parent-bwait-fsm invocation))
           (bwait-before (and parent-fsm
                              (eq (gptel-fsm-state parent-fsm) 'BWAIT)))
           (descendants
            (mevedel-agent-runtime--stop-agent-descendants invocation stop-reason))
           (agent-buffer (mevedel-agent-invocation-buffer invocation))
           (response (mevedel-agent-runtime--stop-agent-response
                      resolved-id agent-type description stop-reason invocation))
           (background
            (mevedel-agent-invocation-background-p invocation))
           completed-tool-callback)
      (setf (mevedel-agent-invocation-terminal-reason invocation)
            stop-reason)
      (when (mevedel-agent-runtime--agent-request-live-p agent-buffer)
        (let* ((inhibit-message t)
               (info (gptel-fsm-info fsm))
               (callback (plist-get info :callback)))
          (unwind-protect
              (progn
                (setf (gptel-fsm-info fsm)
                      (plist-put info :callback #'ignore))
                (condition-case _
                    (gptel-abort agent-buffer)
                  (error nil)))
            (setf (gptel-fsm-info fsm)
                  (plist-put (gptel-fsm-info fsm) :callback callback)))))
      (unless (mevedel-agent-runtime--agent-terminal-status-p
               (mevedel-agent-invocation-transcript-status invocation))
        (mevedel-agent-runtime--finalize invocation 'aborted))
      (unless background
        (setq completed-tool-callback
              (mevedel-agent-runtime--complete-foreground-agent
               invocation response)))
      (if background
          (mevedel-agent-runtime--complete-background-agent invocation response)
        (unless completed-tool-callback
          (when (mevedel-agent-runtime--invoke-agent-abort-callback fsm)
            (setq completed-tool-callback
                  (mevedel-agent-invocation-foreground-result-reported-p
                   invocation)))
          (unless completed-tool-callback
            (mevedel-agent-runtime--remove-agent-registry-entry
             invocation resolved-id buf))))
      (list :agent-id resolved-id
            :previous-status previous-status
            :status (or (mevedel-agent-invocation-transcript-status
                         invocation)
                        'aborted)
            :descendants descendants
            :completed-tool-callback completed-tool-callback
            :resumed-bwait
            (and bwait-before parent-fsm
                 (not (eq (gptel-fsm-state parent-fsm) 'BWAIT)))))))

(defun mevedel-agent-runtime--stop-agent-result-string (result)
  "Return a compact model-visible string for stop RESULT."
  (let ((agent-id (plist-get result :agent-id))
        (previous (plist-get result :previous-status))
        (status (plist-get result :status))
        (descendants (plist-get result :descendants))
        (resumed (plist-get result :resumed-bwait))
        (completed-tool (plist-get result :completed-tool-callback)))
    (format "Stopped agent %s. Previous status: %s. Current status: %s.%s%s%s"
            agent-id previous status
            (if descendants
                (format " Stopped %d descendant%s."
                        (length descendants)
                        (if (= (length descendants) 1) "" "s"))
              "")
            (if resumed " Parent BWAIT resumed." "")
            (if completed-tool " Parent Agent tool completed." ""))))

(defun mevedel-stop-agent (agent-id &optional reason)
  "Interactively stop AGENT-ID in the current session with REASON."
  (interactive
   (let* ((buf (or (mevedel--prompt--data-buffer)
                   (user-error "No mevedel data buffer here")))
          (choices
           (mapcar
            (lambda (entry)
              (let ((id (car entry)))
                (cons (format "%s (%s)"
                              (mevedel-agent-runtime-display-label
                               id)
                              id)
                      id)))
            (mevedel-agent-runtime-live-invocations buf)))
          (selected
           (if choices
               (cdr (assoc (completing-read "Stop agent: " choices nil t)
                           choices))
             (user-error "No running agents in this session"))))
     (list selected
           (read-string "Reason: " "stopped by user"))))
  (let ((result (mevedel-agent-runtime-stop agent-id reason)))
    (message "mevedel: %s"
             (mevedel-agent-runtime--stop-agent-result-string result))
    result))


(cl-defun mevedel-agent-runtime-dispatch
    (main-cb agent description prompt
             &key background parent-context parent-fsm
             message-handler terminal-handler model-tier
             skill-permission-rules
             skill-model-override skill-effort-override
             skill-hook-rules
             on-invocation)
  "Dispatch AGENT with DESCRIPTION and PROMPT, delivering to MAIN-CB.

AGENT is the resolved `mevedel-agent' struct -- caller has already
verified it is non-nil.  BACKGROUND, MODEL-TIER, SKILL-PERMISSION-RULES,
SKILL-MODEL-OVERRIDE, SKILL-EFFORT-OVERRIDE, SKILL-HOOK-RULES, and
ON-INVOCATION match `mevedel-agent-runtime-dispatch'.

Dispatch order:

  Metadata setup (steps 1-11):
    1.  Allocate agent-id and short-id.
    2.  Compute parent-turn = `(1+ session.turn-count)`.
    3.  Store metadata on the invocation.
    4.  Allocate the agent buffer.
    5.  Configure parent-context bindings (done by allocator).
    6.  Shallow-materialize parent session (mid-turn safe).
    7-9.  Compute path with collision avoidance, set variable
          `buffer-file-name', insert prompt.
    10.  Save initial buffer.
    11.  Add `running' entry to session slot.

  Dispatch ordering (steps 12-15):
    12-13.  Pre-register on `mevedel-agent-runtime--fsms' BEFORE
            `gptel-request' to close the abort race.
    14-15.  Dispatch and wrap the callback.

  All step-12+ work runs under `unwind-protect' so a startup
  failure unregisters the registry entry.

spec keyword args (SKILL-PERMISSION-RULES /
MODEL-TIER / SKILL-MODEL-OVERRIDE / SKILL-EFFORT-OVERRIDE) seed the spawned
invocation's matching slots so the child request's pre-realization model
resolver and permission resolver pick them up."
  (require 'mevedel-agent-exec)
  (require 'mevedel-models)
  (let* ((agent-type (mevedel-agent-name agent))
         (agent-id (concat agent-type "--"
                           (md5 (format "%s%s%s%s"
                                        (system-name) (emacs-pid)
                                        (current-time) (random)))))
         (invocation (mevedel-agent-invocation-create agent))
         (parent-data-buffer (current-buffer))
         (parent-session (and (boundp 'mevedel--session) mevedel--session))
         (parent-ctx (or parent-context parent-session))
         (this-ctx invocation)
         (fired nil))
    ;; --- Metadata setup ---
    (setf (mevedel-agent-invocation-agent-id invocation) agent-id)
    (setf (mevedel-agent-invocation-description invocation) description)
    (setf (mevedel-agent-invocation-parent-session invocation) parent-session)
    (setf (mevedel-agent-invocation-parent-data-buffer invocation)
          parent-data-buffer)
    (setf (mevedel-agent-invocation-parent-context invocation) parent-ctx)
    (setf (mevedel-agent-invocation-parent-fsm invocation) parent-fsm)
    (setf (mevedel-agent-invocation-parent-turn invocation)
          (1+ (or (and parent-session
                       (mevedel-session-turn-count parent-session))
                  0)))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'running)
    (setf (mevedel-agent-invocation-background-p invocation)
          (and background t))
    (when model-tier
      (setf (mevedel-agent-invocation-model-tier-override invocation)
            (mevedel-model-tier-selector model-tier)))
    ;; Seed the invocation's skill-* slots before child request realization so
    ;; model policy and the bucket-aware permission resolver see the caller's
    ;; skill scope inside the fork.
    (when skill-permission-rules
      (setf (mevedel-agent-invocation-skill-permission-rules invocation)
            skill-permission-rules))
    (when skill-model-override
      (setf (mevedel-agent-invocation-skill-model-override invocation)
            skill-model-override))
    (when skill-effort-override
      (setf (mevedel-agent-invocation-skill-effort-override invocation)
            skill-effort-override))
    (when skill-hook-rules
      (setf (mevedel-agent-invocation-hook-rules invocation)
            (append (mevedel-agent-invocation-hook-rules invocation)
                    skill-hook-rules)))
    (let ((agent-buffer
           (mevedel-agent-exec--allocate-agent-buffer
            invocation parent-data-buffer)))
      (setf (mevedel-agent-invocation-buffer invocation) agent-buffer)
      ;; Try to set up persistence (shallow materialize + transcript file).
      (mevedel-agent-runtime-dispatch--setup-transcript invocation agent-buffer)
      ;; Insert the initial task prompt.  Persist it before the
      ;; first request dispatch so a crash mid-first-response still
      ;; leaves the prompt on disk.  If the initial save fails (full
      ;; disk, perms, read-only mount), drop persistence: clear
      ;; `buffer-file-name', drop the in-memory transcript entry,
      ;; and continue with the agent buffer as ephemeral.
      (with-current-buffer agent-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bobp) (insert "\n"))
          (insert (format "* Agent Task: %s\n\n%s\n"
                          (or description "")
                          (or prompt ""))))
        (when (mevedel-agent-invocation-transcript-relative-path invocation)
          (let ((saved
                 (mevedel-agent-exec--save-transcript-buffer invocation)))
            (unless saved
              (mevedel-agent-runtime-dispatch--abandon-persistence invocation)))))
      (when on-invocation
        (condition-case err
            (funcall on-invocation invocation)
          (error
           (message "mevedel: agent invocation callback failed: %S" err))))
      ;; --- Build wrapped callbacks ---
      (let* ((wrapped-callback
              (cond
               (background
                ;; Background mode: deliver result to parent's mailbox.
                (lambda (response &rest _rest)
                  (mevedel-agent-runtime--complete-background-agent
                   invocation
                   (mevedel-agent-runtime--normalize-terminal-response
                    invocation response))))
               (t
                ;; Foreground mode: fire main-cb once when no pending
                ;; work remains on either axis.  Wrap success
                ;; responses with render-data so the renderer can
                ;; expose the transcript-open affordance.
                (lambda (response &rest rest)
                  (setq response
                        (mevedel-agent-runtime--normalize-terminal-response
                         invocation response))
                  (when mevedel-agent-runtime-debug
                    (let ((bg (and this-ctx
                                   (mevedel-agent-runtime--ctx-background-agents
                                    this-ctx)))
                          (msgs (and this-ctx
                                     (mevedel-agent-runtime--ctx-messages this-ctx))))
                      (message "mevedel TASK-DISPATCH FG agent=%s id=%s fired=%s \
err-prefix=%s bg=%S msgs=%d resp=%S"
                               agent-type agent-id fired
                               (and (stringp response)
                                    (string-prefix-p "Error:" response))
                               bg
                               (length msgs)
                               (and (stringp response)
                                    (substring response 0
                                               (min 80 (length response)))))))
                  (let ((finish
                         (lambda (result args)
                           (setq fired t)
                           (setf
                            (mevedel-agent-invocation-foreground-result-reported-p
                             invocation)
                            t)
                           (unwind-protect
                               (apply main-cb result args)
                             (mevedel-agent-runtime--remove-agent-registry-entry
                              invocation agent-id parent-data-buffer)))))
                    (unless fired
                      (cond
                       ((and (stringp response)
                             (string-prefix-p "Error:" response))
                        (funcall finish response rest))
                       ((and this-ctx
                             (or (mevedel-agent-runtime--ctx-background-agents
                                  this-ctx)
                                 (mevedel-agent-runtime--ctx-messages this-ctx)))
                        nil)
                       (t
                        (funcall
                         finish
                         (mevedel-agent-runtime-dispatch--wrap-foreground-response
                          response invocation)
                         rest))))))))))
        (unless background
          (setf (mevedel-agent-invocation-parent-tool-callback invocation)
                wrapped-callback))
        ;; Register background tracking BEFORE starting the child FSM.
        (when (and background parent-ctx)
          (mevedel-agent-runtime--ctx-push-background-agent parent-ctx agent-id))
        (let (agent-fsm success-p)
          (unwind-protect
              (progn
                ;; `--run' registers `(agent-id . fsm)' on the
                ;; parent's `mevedel-agent-runtime--fsms' BEFORE
                ;; calling `gptel-request' -- so a racing
                ;; `mevedel-abort' finds the entry while the HTTP
                ;; request is still being set up.
                (let ((mevedel-agent-exec-debug mevedel-agent-runtime-debug))
                  (setq agent-fsm
                        (mevedel-agent-exec--run
                         wrapped-callback agent-type description prompt
                         invocation agent-buffer
                         (lambda (fsm child-invocation)
                           (mevedel-agent-runtime--configure-fsm
                            fsm child-invocation
                            message-handler terminal-handler)))))
                (when (and agent-fsm (not background))
                  (mevedel-agent-runtime--foreground-watchdog-arm invocation))
                (setq success-p t))
            (unless success-p
              (mevedel-agent-runtime--foreground-watchdog-cancel agent-id)
              (when (mevedel-agent-invocation-transcript-relative-path
                     invocation)
                (mevedel-agent-runtime-dispatch--mark-start-blocked
                 invocation "SubagentStart hook blocked sub-agent"))
              (when (and background parent-ctx)
                (mevedel-agent-runtime--ctx-remove-background-agent
                 parent-ctx agent-id))
              (setq mevedel-agent-runtime--fsms
                    (assoc-delete-all agent-id
                                      mevedel-agent-runtime--fsms))))
          (when (and agent-fsm background)
            (when parent-ctx
              (mevedel-agent-runtime--queue-background-status-reminder
               parent-ctx agent-id agent-type description 'running
               (mevedel-agent-invocation-transcript-relative-path
                invocation)))
            ;; emit the launch result with render-data of
            ;; `:status running' so the parent's view buffer
            ;; renders the running-handle state badge from the
            ;; outset, and the deferred phase-6b live-update path
            ;; has a render-data block to patch.  The :result
            ;; string is unchanged for LLM compatibility.
            (let* ((launch-result
                    (format "Agent launched in background: %s (id: %s).

Its `<agent-result>' block will be delivered to your mailbox when \
it finishes. Be patient; depending on the task, the agent will \
need some time to respond. Do NOT summarise or declare it failed \
until you have seen that block.  Errors from other agents say \
nothing about this one -- each agent ID reports back independently.

If you have no other useful work to do while waiting, just respond \
with text and the runtime will park your turn in BWAIT until all \
background agents have reported back.

Use SendMessage(to=\"%s\", ...) to send this agent guidance."
                            agent-type agent-id agent-type))
                   (rel (and (mevedel-agent-invocation-p invocation)
                             (mevedel-agent-invocation-transcript-relative-path
                              invocation)))
                   (hook-audits (and (mevedel-agent-invocation-p invocation)
                                     (mevedel-agent-invocation-hook-audits
                                      invocation))))
              (funcall main-cb
                       (cond
                        (rel
                         (list :result launch-result
                               :render-data
                               (append
                                (list :kind 'agent-transcript
                                      :agent-id agent-id
                                      :transcript-relative-path rel
                                      :background t
                                      :status 'running
                                      :calls 0)
                                (when hook-audits
                                  (list :hook-audits hook-audits)))))
                        (t launch-result))))))))))

(defun mevedel-agent-runtime-dispatch--abandon-persistence (invocation)
  "Drop persistence state for INVOCATION after a fatal save failure.

Clears the transcript buffer's variable `buffer-file-name', removes the
running entry from the parent session's `agent-transcripts' slot,
and unsets `transcript-relative-path' on the invocation.  The
agent buffer keeps running ephemerally (no on-disk transcript)."
  (let ((session (mevedel-agent-invocation-parent-session invocation))
        (agent-id (mevedel-agent-invocation-agent-id invocation))
        (buf (mevedel-agent-invocation-buffer invocation)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)
        (rename-buffer (generate-new-buffer-name
                        (format "*mevedel-agent-%s*"
                                (or (and (stringp agent-id)
                                         (let ((bits
                                                (split-string agent-id "--" t)))
                                           (and (cadr bits)
                                                (substring (cadr bits) 0
                                                           (min 8
                                                                (length
                                                                 (cadr bits)))))))
                                    "anon"))))))
    (setf (mevedel-agent-invocation-transcript-relative-path invocation) nil)
    (setf (mevedel-agent-invocation-sidecar-dirty invocation) nil)
    (when (and session agent-id)
      (setf (mevedel-session-agent-transcripts session)
            (assoc-delete-all agent-id
                              (mevedel-session-agent-transcripts session))))))

(defun mevedel-agent-runtime-dispatch--mark-start-blocked (invocation reason)
  "Mark INVOCATION as terminal for REASON before request start."
  (when (mevedel-agent-invocation-p invocation)
    (setf (mevedel-agent-invocation-transcript-status invocation) 'error)
    (setf (mevedel-agent-invocation-terminal-reason invocation) reason)
    (when-let* ((session (mevedel-agent-invocation-parent-session invocation))
                (agent-id (mevedel-agent-invocation-agent-id invocation)))
      (require 'mevedel-session-persistence)
      (mevedel-session-persistence--update-transcript-entry
       session agent-id
       (list :status 'error
             :reason reason)))))

(defun mevedel-agent-runtime-dispatch--setup-transcript (invocation agent-buffer)
  "Best-effort transcript persistence setup for INVOCATION's AGENT-BUFFER.

Performs allocation steps 6-11: shallow materialization of
the parent session, transcript path computation with collision
avoidance, `set-visited-file-name', and a session-slot `running'
entry.  Any failure is logged and falls through to the
in-memory fallback (the agent buffer remains usable;
variable `buffer-file-name' stays nil; no sidecar entry is created)."
  (let ((session (mevedel-agent-invocation-parent-session invocation))
        (parent-buf (mevedel-agent-invocation-parent-data-buffer invocation))
        (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (when (and (mevedel-agent-invocation-p invocation)
               session
               agent-buffer
               (buffer-live-p agent-buffer)
               (buffer-live-p parent-buf)
               (not (buffer-local-value 'mevedel-session--read-only-mode
                                        parent-buf)))
      (condition-case err
          (let ((save-path
                 (mevedel-session-persistence--shallow-ensure-files
                  session parent-buf)))
            (when save-path
              (let* ((agent-type
                      (or (let ((a (mevedel-agent-invocation-agent invocation)))
                            (and a (mevedel-agent-name a)))
                          "agent"))
                     (suffix (let ((bits (split-string agent-id "--" t)))
                               (or (and (cadr bits)
                                        (substring (cadr bits) 0
                                                   (min 8
                                                        (length (cadr bits)))))
                                   "anon")))
                     (timestamp (format-time-string "%FT%H-%M-%S"))
                     (rel-path nil)
                     (abs-path nil))
                ;; Path collision: append `-2', `-3', ... until free.
                (cl-loop
                 for n from 1
                 for candidate-rel =
                 (if (= n 1)
                     (format "agents/%s--%s--%s.chat.org"
                             agent-type timestamp suffix)
                   (format "agents/%s--%s--%s-%d.chat.org"
                           agent-type timestamp suffix n))
                 for candidate-abs = (expand-file-name candidate-rel save-path)
                 while (file-exists-p candidate-abs)
                 finally (setq rel-path candidate-rel
                               abs-path candidate-abs))
                (with-current-buffer agent-buffer
                  (set-visited-file-name abs-path t t))
                (setf (mevedel-agent-invocation-transcript-relative-path
                       invocation)
                      rel-path)
                (setf (mevedel-agent-invocation-sidecar-dirty invocation) t)
                ;; Add running entry to session slot.
                (let* ((now (format-time-string "%FT%H-%M-%S"))
                       (parent-agent-id
                        (and (mevedel-agent-invocation-p
                              (mevedel-agent-invocation-parent-context
                               invocation))
                             (mevedel-agent-invocation-agent-id
                              (mevedel-agent-invocation-parent-context
                               invocation))))
                       (entry
                        (list
                         :agent-type agent-type
                         :description
                         (mevedel-agent-invocation-description invocation)
                         :path rel-path
                         :status 'running
                         :created-at now
                         :updated-at now
                         :parent-turn
                         (mevedel-agent-invocation-parent-turn invocation))))
                  (when parent-agent-id
                    (setq entry
                          (plist-put entry :parent-agent-id
                                     parent-agent-id)))
                  (mevedel-session-persistence--record-running-transcript
                   session (cons agent-id entry))))))
        (error
         (message "mevedel: transcript persistence setup failed: %S" err))))))

(defun mevedel-agent-runtime-dispatch--wrap-foreground-response (response invocation)
  "Return RESPONSE for INVOCATION wrapped with render-data when available.

The render-data includes `:calls', `:elapsed', and (when
applicable) `:reason' fields so `mevedel-tool-ui--render-agent'
can render the state badge alongside the transcript-open
affordance.  Field set:

  (:kind agent-transcript
   :agent-id ID
   :transcript-relative-path REL
   :status STATUS
   :calls N
   :elapsed SECONDS
   :reason STRING)                   ; error/aborted only

Returns RESPONSE unchanged when the invocation has no transcript
path or the response is not a string."
  (let* ((rel (and (mevedel-agent-invocation-p invocation)
                   (mevedel-agent-invocation-transcript-relative-path
                    invocation)))
         (status (and (mevedel-agent-invocation-p invocation)
                      (mevedel-agent-invocation-transcript-status
                       invocation)))
         (id (and (mevedel-agent-invocation-p invocation)
                  (mevedel-agent-invocation-agent-id invocation)))
         (calls (and (mevedel-agent-invocation-p invocation)
                     (mevedel-agent-invocation-call-count invocation)))
         (started-at (and (mevedel-agent-invocation-p invocation)
                          (mevedel-agent-invocation-started-at invocation)))
         (elapsed (when started-at
                    (float-time (time-subtract (current-time) started-at))))
         (reason (and (mevedel-agent-invocation-p invocation)
                      (mevedel-agent-invocation-terminal-reason invocation)))
         (hook-audits (and (mevedel-agent-invocation-p invocation)
                           (mevedel-agent-invocation-hook-audits
                            invocation)))
         (verdict (mevedel-agent-runtime--record-verifier-verdict
                   response invocation)))
    (cond
     ((not (stringp response)) response)
     ((not (and rel id)) response)
     (t (list :result response
              :render-data (append
                            (list :kind 'agent-transcript
                                  :agent-id id
                                  :transcript-relative-path rel
                                  :status status
                                  :calls (or calls 0))
                            (when elapsed (list :elapsed elapsed))
                            (when reason (list :reason reason))
                            (when hook-audits
                              (list :hook-audits hook-audits))
                            (when verdict (list :verdict verdict))))))))

(provide 'mevedel-agent-runtime)
;;; mevedel-agent-runtime.el ends here
