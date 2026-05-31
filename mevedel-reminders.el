;;; mevedel-reminders.el -- System reminders -*- lexical-binding: t -*-

;;; Commentary:

;; System reminders: mid-conversation guidance injected into the user
;; message as `<system-reminder>' blocks. Reminders are data: each is a
;; struct with a trigger function, a content function, and optional
;; interval throttling.
;;
;; Reminders live on the session struct for main chat sessions, and on
;; the agent struct (cloned per invocation) for sub-agents. A prompt
;; transform function prepends active reminders to the user's prompt
;; before the request is sent. Turn counting is driven by a terminal FSM
;; handler in the request pipeline.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-structs)

;; `mevedel-structs'
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)

;; `gptel'
(defvar gptel-prompt-transform-functions)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; Current prompt-transform context.
(defvar mevedel-reminders--current-chat-buffer nil
  "Chat buffer whose reminders are currently being collected.
Bound dynamically by `mevedel-reminders--transform' so reminder
triggers can distinguish the real chat buffer from gptel's temporary
prompt buffer.")

;; `mevedel-permissions'
(defvar mevedel-permission-mode)

;; `mevedel-compact'
(declare-function mevedel--estimate-tokens "mevedel-compact" ())
(declare-function mevedel--compact-threshold-tokens "mevedel-compact" ())
(declare-function mevedel--compact-usable-tokens "mevedel-compact" ())
(declare-function mevedel--compact-auto-eligible-p "mevedel-compact" ())
(defvar mevedel-compact-auto)

;; `mevedel-agent-exec'
(defvar mevedel-agent-exec--agents)

;; `mevedel-workspace'
(declare-function mevedel-workspace-root "mevedel-workspace" (workspace) t)

;; `mevedel-file-state'
(declare-function mevedel-file-cache-detect-external-changes
                  "mevedel-file-state" (cache))
(declare-function mevedel-file-cache-consume-external-changes
                  "mevedel-file-state" (cache changes))

;; `mevedel-tool-fs'
(declare-function mevedel-tools--generate-diff
                  "mevedel-tool-fs" (original modified filepath))

;; `mevedel-tool-task'
(declare-function mevedel-tool-task-format-active-groups-for-reminder
                  "mevedel-tool-task" (session))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-deferred-expired
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-set-deferred-expired
                  "mevedel-agents" (invocation value))
(declare-function mevedel-agent-invocation-deferred-set
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-turn-count
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-max-turns "mevedel-agents" (agent) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)

;; `flymake'
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))

;; `flycheck'
(declare-function flycheck-overlay-errors-in "ext:flycheck" (beg end))
(declare-function flycheck-error-line "ext:flycheck" (err) t)
(declare-function flycheck-error-level "ext:flycheck" (err) t)
(declare-function flycheck-error-message "ext:flycheck" (err) t)

;; `xref'
(declare-function xref-find-backend "xref" ())
(defvar tags-file-name)
(defvar tags-table-list)

;; `imenu'
(declare-function imenu--make-index-alist "imenu" (&optional noerror))
(defvar imenu--index-alist)

;; `treesit'
(declare-function treesit-available-p "treesit" ())
(declare-function treesit-parser-list "treesit" (&optional buffer language))


;;
;;; Reminder struct

(cl-defstruct (mevedel-reminder (:constructor mevedel-reminder--create))
  "A single system reminder.

A reminder's TRIGGER is called with a context object (session struct
for main chat, agent-specific context for sub-agents) and returns
non-nil when the reminder should fire on the current turn.  CONTENT is
called with the same context object and returns the reminder body
string.

INTERVAL controls firing frequency:
  - nil        - fire every turn the trigger returns non-nil
  - integer    - minimum number of turns between firings
  - `one-shot' - fire at most once per reminder lifetime

LAST-FIRED is the turn count when this reminder last fired, or nil if
it has never fired."
  type
  trigger
  content
  interval
  last-fired)

(cl-defun mevedel-reminder-create (&key type trigger content interval)
  "Create a new `mevedel-reminder' with TYPE, TRIGGER, CONTENT, and INTERVAL.

TYPE is a symbol identifying the reminder kind.  TRIGGER and CONTENT
are functions of one argument (the firing context).  INTERVAL controls
firing frequency: nil for every-turn, an integer for throttled firing,
or the symbol `one-shot' for fire-at-most-once."
  (unless (symbolp type)
    (error "Reminder :type must be a symbol, got %S" type))
  (unless (functionp trigger)
    (error "Reminder :trigger must be a function, got %S" trigger))
  (unless (functionp content)
    (error "Reminder :content must be a function, got %S" content))
  (when (and interval
             (not (integerp interval))
             (not (eq interval 'one-shot)))
    (error "Reminder :interval must be an integer, `one-shot', or nil, got %S"
           interval))
  (mevedel-reminder--create
   :type type
   :trigger trigger
   :content content
   :interval interval
   :last-fired nil))

(defun mevedel-reminder-clone (reminder)
  "Return a shallow copy of REMINDER with LAST-FIRED reset to nil.

Trigger, content, and interval are shared by reference; only the
per-invocation state (LAST-FIRED) is reset so cloned reminders track
their own firing history."
  (mevedel-reminder--create
   :type (mevedel-reminder-type reminder)
   :trigger (mevedel-reminder-trigger reminder)
   :content (mevedel-reminder-content reminder)
   :interval (mevedel-reminder-interval reminder)
   :last-fired nil))

(defun mevedel-reminders-clone-list (reminders)
  "Return a fresh list of cloned REMINDERS.

Each element is copied via `mevedel-reminder-clone' so the returned
list tracks its own LAST-FIRED state independently of REMINDERS."
  (mapcar #'mevedel-reminder-clone reminders))


;;
;;; Session reminder helpers

(defun mevedel-session-add-reminder (session reminder)
  "Append REMINDER to SESSION's reminder list."
  (setf (mevedel-session-reminders session)
        (append (mevedel-session-reminders session) (list reminder)))
  reminder)

(defun mevedel-session-remove-reminder (session type)
  "Remove all reminders of TYPE from SESSION."
  (setf (mevedel-session-reminders session)
        (cl-remove-if (lambda (r) (eq (mevedel-reminder-type r) type))
                      (mevedel-session-reminders session))))


;;
;;; Firing logic

(defun mevedel-reminders--should-fire-p (reminder turn-count ctx)
  "Return non-nil if REMINDER should fire at TURN-COUNT for CTX.

CTX is the firing context passed to the reminder's trigger function.
A reminder fires when its trigger returns non-nil AND the interval
policy permits firing.  For nil interval the reminder always fires,
for `one-shot' it fires only if it has never fired, for integer
interval it fires once enough turns have passed since the last fire."
  (and (funcall (mevedel-reminder-trigger reminder) ctx)
       (let ((interval (mevedel-reminder-interval reminder))
             (last-fired (mevedel-reminder-last-fired reminder)))
         (cond
          ((null interval) t)
          ((eq interval 'one-shot) (null last-fired))
          (t (or (null last-fired)
                 (>= (- turn-count last-fired) interval)))))))

(defun mevedel-reminders--format-block (content)
  "Wrap CONTENT in a `<system-reminder>' XML block."
  (format "<system-reminder>\n%s\n</system-reminder>" content))

(defun mevedel-reminders--collect-from (reminders turn-count ctx)
  "Evaluate REMINDERS at TURN-COUNT and return formatted blocks.

REMINDERS is a list of `mevedel-reminder' structs.  TURN-COUNT is the
current turn counter used for interval checks.  CTX is the firing
context passed to each reminder's trigger and content functions.

Mutates each fired reminder's LAST-FIRED.  Returns a list of block
strings in the order the reminders appear."
  (let ((blocks nil))
    (dolist (reminder reminders)
      (when (mevedel-reminders--should-fire-p reminder turn-count ctx)
        (push (mevedel-reminders--format-block
               (funcall (mevedel-reminder-content reminder) ctx))
              blocks)
        (setf (mevedel-reminder-last-fired reminder) turn-count)))
    (nreverse blocks)))

(defun mevedel-reminders--collect (session)
  "Collect firing reminders for SESSION.

Thin wrapper around `mevedel-reminders--collect-from' that pulls the
reminder list and turn count off the session struct."
  (mevedel-reminders--collect-from
   (mevedel-session-reminders session)
   (mevedel-session-turn-count session)
   session))

(defun mevedel-reminders--current-buffer ()
  "Return the chat buffer currently collecting reminders, or nil."
  (and (buffer-live-p mevedel-reminders--current-chat-buffer)
       mevedel-reminders--current-chat-buffer))

(defun mevedel-reminders--compact-token-state ()
  "Return context-pressure state for the current chat buffer.
The returned plist contains `:tokens', `:threshold', `:usable',
and `:ratio', or nil if compaction helpers are unavailable."
  (when-let* ((buf (mevedel-reminders--current-buffer))
              ((fboundp 'mevedel--estimate-tokens))
              ((fboundp 'mevedel--compact-threshold-tokens))
              ((fboundp 'mevedel--compact-usable-tokens)))
    (with-current-buffer buf
      (let* ((tokens (mevedel--estimate-tokens))
             (threshold (mevedel--compact-threshold-tokens))
             (usable (mevedel--compact-usable-tokens))
             (ratio (and (numberp usable)
                         (> usable 0)
                         (/ (float tokens) usable))))
        (list :tokens tokens
              :threshold threshold
              :usable usable
              :ratio ratio)))))

(defun mevedel-reminders--compact-auto-available-p ()
  "Return non-nil when auto-compaction can run in the current chat buffer."
  (when-let* ((buf (mevedel-reminders--current-buffer))
              ((fboundp 'mevedel--compact-auto-eligible-p)))
    (with-current-buffer buf
      (mevedel--compact-auto-eligible-p))))

(defun mevedel-reminders--agent-snapshot ()
  "Return currently visible agent types as an alist.
The shape is (NAME . DESCRIPTION), sorted by NAME.  Returns nil when
the current chat buffer has no request-local agent roster yet."
  (when-let* ((buf (mevedel-reminders--current-buffer)))
    (with-current-buffer buf
      (when (boundp 'mevedel-agent-exec--agents)
        (sort
         (mapcar (lambda (entry)
                   (cons (car entry)
                         (plist-get (cdr entry) :description)))
                 mevedel-agent-exec--agents)
         (lambda (a b) (string< (car a) (car b))))))))

(defun mevedel-reminders--format-agent-delta (added removed)
  "Format ADDED and REMOVED visible-agent deltas."
  (concat
   "Available agent types changed during this session.\n\n"
   (when added
     (concat "Added:\n"
             (mapconcat (lambda (entry)
                          (format "- %s: %s"
                                  (car entry)
                                  (or (cdr entry) "")))
                        added "\n")
             "\n"))
   (when removed
     (concat "Removed:\n"
             (mapconcat (lambda (entry)
                          (format "- %s" (car entry)))
                        removed "\n")
             "\n"))
   "\nUse the Agent tool only with currently available types."))


;;
;;; Prompt transform

(defun mevedel-reminders--transform (fsm)
  "Prepend system reminders to the current user prompt.

Operates on the current buffer, which is the temporary prompt buffer
passed by `gptel-prompt-transform-functions'.  The session lives on the
chat buffer, which is reached via FSM's info plist's :buffer entry.

FSM is mandatory (not `&optional') so that
`gptel-prompt-transform-functions' dispatch -- which inspects the
function's minimum arity -- passes the FSM argument rather than invoking
the transform with zero arguments.

Only the last user prompt is modified.  Runs after
`mevedel--transform-expand-mentions' so reminders sit above the expanded
prompt text."
  (when-let* ((chat-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p chat-buffer))
              (session (buffer-local-value 'mevedel--session chat-buffer)))
    (let ((mevedel-reminders--current-chat-buffer chat-buffer))
      (when-let* ((contexts (mevedel-session-hook-context-pending session)))
        (setf (mevedel-session-hook-context-pending session) nil)
        (text-property-search-backward 'gptel nil t)
        (let ((start (point)))
          (insert "\n<hook-context>\n"
                  (mapconcat (lambda (item) (format "%s" item))
                             contexts
                             "\n")
                  "\n</hook-context>\n")
          (remove-text-properties
           start (point)
           '(gptel nil response nil invisible nil front-sticky nil))))
      (when-let* ((blocks (mevedel-reminders--collect session)))
        (text-property-search-backward 'gptel nil t)
        (let ((start (point)))
          (insert "\n" (string-join blocks "\n") "\n")
          (remove-text-properties
           start (point)
           '(gptel nil response nil invisible nil front-sticky nil)))))))


;;
;;; Tier 1 built-in reminders

(defun mevedel-reminders--session-mode (session)
  "Return the effective permission mode for SESSION.
Falls back to the global `mevedel-permission-mode' default."
  (or (mevedel-session-permission-mode session)
      (and (boundp 'mevedel-permission-mode) mevedel-permission-mode)
      'default))

(defvar mevedel-reminders--mode-constraint-messages
  '((plan . "Permission mode: `plan'. File modifications and destructive operations are denied. Explore the codebase with read-only tools and propose changes in your response rather than applying them.")
    (accept-edits . "Permission mode: `accept-edits'. File edits are auto-approved; shell commands still require confirmation. Keep changes minimal, targeted, and correct.")
    (trust-all . "Permission mode: `trust-all'. Most confirmation prompts are skipped. Double-check destructive operations before calling tools; protected paths still prompt."))
  "Alist mapping permission mode symbols to reminder body strings.")

(defun mevedel-reminders-make-mode-constraints (&optional interval)
  "Create the mode-constraints reminder.

Fires when the session's permission mode is non-default.  INTERVAL
defaults to 5 turns so the reminder repeats sparsely across long
sessions rather than spamming every turn."
  (mevedel-reminder-create
   :type 'mode-constraints
   :trigger (lambda (session)
              (not (eq (mevedel-reminders--session-mode session) 'default)))
   :content (lambda (session)
              (let ((mode (mevedel-reminders--session-mode session)))
                (or (alist-get mode mevedel-reminders--mode-constraint-messages)
                    (format "Permission mode: `%s'." mode))))
   :interval (or interval 5)))

(defun mevedel-reminders-make-auto-mode (&optional interval)
  "Create the `auto-mode' reminder.
Fires immediately after `/auto' enters trust-all mode, then repeats
sparsely while that mode remains active."
  (mevedel-reminder-create
   :type 'auto-mode
   :trigger (lambda (session)
              (eq (mevedel-reminders--session-mode session) 'trust-all))
   :content (lambda (_session)
              "Auto mode is active. Permission prompts are skipped for Bash, Eval, and other tools unless an explicit deny or protected-path policy requires intervention. Keep tool calls deliberate and check destructive operations before running them.")
   :interval (or interval 5)))

(defun mevedel-reminders-make-auto-mode-exit ()
  "Create the one-shot `auto-mode-exit' reminder."
  (mevedel-reminder-create
   :type 'auto-mode-exit
   :trigger (lambda (session)
              (not (eq (mevedel-reminders--session-mode session) 'trust-all)))
   :content (lambda (_session)
              "Auto mode has been turned off. Permission behavior is back to `default'; ask before file edits, Bash, Eval, and other non-read-only actions unless a rule explicitly allows them.")
   :interval 'one-shot))

(defun mevedel-reminders--plan-path (session)
  "Return SESSION's latest plan path, when recorded."
  (when-let* ((metadata (mevedel-session-plan-metadata session))
              (path (plist-get metadata :path)))
    path))

(defun mevedel-reminders--plan-absolute-path (session)
  "Return SESSION's latest plan artifact absolute path, when available."
  (let ((metadata (mevedel-session-plan-metadata session)))
    (or (when-let* ((path (mevedel-reminders--plan-path session))
                    (save-path (mevedel-session-save-path session)))
          (file-name-concat save-path path))
        (plist-get metadata :absolute-path))))

(defun mevedel-reminders--plan-reference-content (session)
  "Return bounded contents of SESSION's latest plan artifact."
  (when-let* ((path (mevedel-reminders--plan-absolute-path session))
              ((file-exists-p path)))
    (with-temp-buffer
      (insert-file-contents path nil 0 12000)
      (buffer-string))))

(defun mevedel-reminders-make-plan-mode (&optional interval)
  "Create the `plan-mode' workflow reminder.
The first firing carries the full workflow. Later firings are sparse
because reminder interval state is tracked on the reminder struct."
  (mevedel-reminder-create
   :type 'plan-mode
   :trigger (lambda (session)
              (eq (mevedel-reminders--session-mode session) 'plan))
   :content (lambda (session)
              (let* ((path (mevedel-reminders--plan-path session))
                     (full-p
                      (null (mevedel-reminder-last-fired
                             (cl-find 'plan-mode
                                      (mevedel-session-reminders session)
                                      :key #'mevedel-reminder-type)))))
                (if full-p
                    (concat
                     "Plan mode is active. The user does not want execution yet. Do not edit files, run mutating commands, commit, or otherwise change the system. Use read-only exploration to understand the code, ask the user only for decisions that cannot be discovered, and finalize by emitting exactly one <proposed_plan> block.\n\n"
                     "The proposed plan must be decision-complete and formatted as Markdown inside exact line-oriented tags:\n"
                     "<proposed_plan>\n# Concrete Plan Title\n\n## Summary\n- State the root cause or goal, intended behavior change, and important non-goals.\n\n## Key Changes\n- Group implementation bullets by subsystem or behavior, not by file inventory. Mention files, public APIs, interfaces, or data shape changes only when needed to remove ambiguity.\n\n## Regression Coverage\n- List the user-visible flows, edge cases, and failure scenarios that tests must cover.\n\n## Validation\n- List exact focused test/build commands to run.\n\n## Assumptions\n- Record defaults, compatibility assumptions, and intentionally unchanged behavior.\n</proposed_plan>\n\n"
                     "Keep bullets short and high-signal. Do not use task/progress tools as the plan artifact, and do not ask for approval outside the proposed-plan block."
                     (if path
                         (format "\n\nLatest plan artifact: %s" path)
                       ""))
                  (concat
                   "Plan mode is still active. Stay read-only, continue resolving unknowns, and end with either user clarification or exactly one <proposed_plan> block for approval. Do not ask for plan approval in plain text."
                   (if path
                       (format " Latest plan artifact: %s." path)
                     "")))))
   :interval (or interval 5)))

(defun mevedel-reminders-make-plan-mode-reentry ()
  "Create the one-shot `plan-mode-reentry' reminder."
  (mevedel-reminder-create
   :type 'plan-mode-reentry
   :trigger (lambda (session)
              (eq (mevedel-reminders--session-mode session) 'plan))
   :content (lambda (session)
              (let ((path (or (mevedel-reminders--plan-path session)
                              "the latest plan artifact")))
                (format
                 "Re-entering Plan mode. A previous plan exists at %s. Before presenting a new plan, evaluate whether the current user request continues that exact task. If it does, revise the existing plan; if not, produce a fresh replacement plan. Do not request implementation until the presented plan matches the current request."
                 path)))
   :interval 'one-shot))

(defun mevedel-reminders-make-plan-mode-exit ()
  "Create the one-shot `plan-mode-exit' reminder."
  (mevedel-reminder-create
   :type 'plan-mode-exit
   :trigger (lambda (session)
              (not (eq (mevedel-reminders--session-mode session) 'plan)))
   :content (lambda (session)
              (let ((path (mevedel-reminders--plan-path session)))
                (concat
                 "Plan mode has ended. You may now execute implementation work under the restored permission mode."
                 (if path
                     (format " The approved/latest plan artifact is %s; refer back to it when needed." path)
                   ""))))
   :interval 'one-shot))

(defun mevedel-reminders-make-plan-reference ()
  "Create the one-shot `plan-reference' reminder."
  (mevedel-reminder-create
   :type 'plan-reference
   :trigger (lambda (session)
              (let ((metadata (mevedel-session-plan-metadata session)))
                (and metadata
                     (eq (plist-get metadata :status) 'approved)
                     (let ((approved-turn
                            (plist-get metadata :approved-turn)))
                       (or (not (integerp approved-turn))
                           (> (or (mevedel-session-turn-count session) 0)
                              approved-turn)))
                     (not (eq (mevedel-reminders--session-mode session)
                              'plan))
                     (mevedel-reminders--plan-reference-content session))))
   :content (lambda (session)
              (let ((path (or (mevedel-reminders--plan-path session)
                              "latest plan"))
                    (content (mevedel-reminders--plan-reference-content
                              session)))
                (format
                 "An approved plan may be relevant to this turn. Plan artifact: %s\n\n%s\n\nContinue from this plan only if it matches the current user request; otherwise treat it as historical context."
                 path
                 (or content ""))))
   :interval 'one-shot))

(defun mevedel-session-ensure-reminder (session reminder)
  "Add REMINDER to SESSION unless a reminder of the same type exists."
  (unless (memq (mevedel-reminder-type reminder)
                (mapcar #'mevedel-reminder-type
                        (mevedel-session-reminders session)))
    (mevedel-session-add-reminder session reminder)))

(defun mevedel-reminders-make-pending-events ()
  "Create the `pending-events' reminder.

Fires when runtime subsystems have queued explicit reminder text on
SESSION.  The pending FIFO is consumed by the content function so each
event is shown once."
  (mevedel-reminder-create
   :type 'pending-events
   :trigger (lambda (session)
              (and (mevedel-session-p session)
                   (mevedel-session-pending-reminders session)))
   :content (lambda (session)
              (let ((items (mevedel-session-pending-reminders session)))
                (setf (mevedel-session-pending-reminders session) nil)
                (mapconcat #'identity items "\n\n")))
   :interval nil))

(defun mevedel-reminders-make-date-change ()
  "Create the `date-change' reminder.

Fires when the local calendar date changes during a session and updates
SESSION's observed date after emitting the reminder."
  (mevedel-reminder-create
   :type 'date-change
   :trigger (lambda (session)
              (let ((current (format-time-string "%F"))
                    (previous (mevedel-session-last-observed-date session)))
                (and previous (not (equal previous current)))))
   :content (lambda (session)
              (let ((previous (mevedel-session-last-observed-date session))
                    (current (format-time-string "%F")))
                (setf (mevedel-session-last-observed-date session) current)
                (format "The current date changed during this session. Previous date context: %s. Current date: %s. Use the current date for any relative-date reasoning."
                        previous current)))
   :interval nil))

(defun mevedel-reminders-make-compaction-available (&optional threshold)
  "Create the `compaction-available' reminder.

Fires sparsely once automatic compaction is enabled and context usage
has crossed THRESHOLD of usable context.  THRESHOLD defaults to 0.70."
  (let ((threshold (or threshold 0.70)))
    (mevedel-reminder-create
     :type 'compaction-available
     :trigger (lambda (_session)
                (let ((state (mevedel-reminders--compact-token-state)))
                  (and state
                       (mevedel-reminders--compact-auto-available-p)
                       (>= (or (plist-get state :ratio) 0.0)
                           threshold))))
     :content (lambda (_session)
                (let* ((state (mevedel-reminders--compact-token-state))
                       (tokens (or (plist-get state :tokens) 0))
                       (usable (or (plist-get state :usable) 1))
                       (pct (round (* 100 (/ (float tokens)
                                             (max 1 usable))))))
                  (format "Automatic compaction is available for this session and context usage is about %d%% of the usable window. Do not stop prematurely because the thread is long; continue the task and let compaction preserve the necessary context when it runs."
                          pct)))
     :interval 'one-shot)))

(defun mevedel-reminders-make-token-usage (&optional threshold interval)
  "Create the `token-usage' context-pressure reminder.

Fires near high context usage.  THRESHOLD defaults to 0.90 of usable
context and INTERVAL defaults to 4 turns."
  (let ((threshold (or threshold 0.90)))
    (mevedel-reminder-create
     :type 'token-usage
     :trigger (lambda (_session)
                (let ((state (mevedel-reminders--compact-token-state)))
                  (and state
                       (>= (or (plist-get state :ratio) 0.0)
                           threshold))))
     :content (lambda (_session)
                (let* ((state (mevedel-reminders--compact-token-state))
                       (tokens (or (plist-get state :tokens) 0))
                       (usable (or (plist-get state :usable) 1))
                       (threshold-tokens
                        (or (plist-get state :threshold) 0))
                       (pct (round (* 100 (/ (float tokens)
                                             (max 1 usable))))))
                  (format "Context pressure is high: estimated usage is about %d%% of the usable window (%d tokens; compaction threshold %d). Be concise, avoid reprinting large context, and rely on compaction/re-reading files when needed."
                          pct tokens threshold-tokens)))
     :interval (or interval 4))))

(defun mevedel-reminders-make-agent-listing-delta ()
  "Create the `agent-listing-delta' reminder.

Compares the current request-visible Agent roster with SESSION's last
snapshot.  The initial snapshot is silent; later added or removed agent
types are reported once and become the new snapshot."
  (let (delta)
    (mevedel-reminder-create
     :type 'agent-listing-delta
     :trigger (lambda (session)
                (setq delta nil)
                (let* ((current (mevedel-reminders--agent-snapshot))
                       (previous (mevedel-session-agent-types-snapshot
                                  session)))
                  (if (eq previous :uninitialized)
                      (progn
                        (setf (mevedel-session-agent-types-snapshot session)
                              current)
                        nil)
                    (let ((added (cl-remove-if
                                  (lambda (entry)
                                    (assoc (car entry) previous))
                                  current))
                          (removed (cl-remove-if
                                    (lambda (entry)
                                      (assoc (car entry) current))
                                    previous)))
                      (when (or added removed)
                        (setq delta (list :added added :removed removed))
                        t)))))
     :content (lambda (session)
                (prog1
                    (mevedel-reminders--format-agent-delta
                     (plist-get delta :added)
                     (plist-get delta :removed))
                  (setf (mevedel-session-agent-types-snapshot session)
                        (mevedel-reminders--agent-snapshot))
                  (setq delta nil)))
     :interval nil)))

(defun mevedel-reminders-make-max-turns-warning (&optional threshold)
  "Create the max-turns-warning reminder for an agent invocation.

Fires once when the agent's turn count reaches THRESHOLD (a fraction
between 0 and 1, default 0.8) of the agent's `max-turns'.  Does
nothing for agents without a configured max-turns cap."
  (let ((threshold (or threshold 0.8)))
    (mevedel-reminder-create
     :type 'max-turns-warning
     :trigger (lambda (inv)
                (when-let* ((agent (mevedel-agent-invocation-agent inv))
                            (max-turns (mevedel-agent-max-turns agent))
                            (count (mevedel-agent-invocation-turn-count inv)))
                  (>= count (floor (* threshold max-turns)))))
     :content (lambda (inv)
                (let* ((agent (mevedel-agent-invocation-agent inv))
                       (max-turns (mevedel-agent-max-turns agent))
                       (count (mevedel-agent-invocation-turn-count inv))
                       (remaining (max 0 (- max-turns count))))
                  (format "You have used %d of %d turns (%d remaining). Wrap up your investigation and return your findings to the caller before you hit the turn limit."
                          count max-turns remaining)))
     :interval 'one-shot)))


;;
;;; Edited-file integration

(defcustom mevedel-reminders-edited-file-max-diff-lines 40
  "Maximum number of diff lines reported per externally edited file.

Each firing of the `edited-file' reminder truncates a file's unified
diff to this many lines before the ellipsis marker.  Keeps reminder
payloads bounded when large rewrites or reformats occur."
  :type 'integer
  :group 'mevedel)

(defun mevedel-reminders--truncate-diff (diff max-lines)
  "Return DIFF truncated to MAX-LINES lines, appending an ellipsis marker."
  (let ((lines (split-string diff "\n")))
    (if (<= (length lines) max-lines)
        diff
      (concat (mapconcat #'identity (seq-take lines max-lines) "\n")
              (format "\n... (%d more lines truncated)"
                      (- (length lines) max-lines))))))

(defun mevedel-reminders--format-edited-file-change (change max-diff-lines)
  "Render CHANGE (plist from detect-external-changes) as a reminder block body.
MAX-DIFF-LINES caps the unified diff size."
  (let ((path (plist-get change :path))
        (status (plist-get change :status))
        (old (plist-get change :old))
        (new (plist-get change :new)))
    (pcase status
      ('deleted (format "DELETED: %s" path))
      ('modified
       (concat (format "MODIFIED: %s\n" path)
               (mevedel-reminders--truncate-diff
                (mevedel-tools--generate-diff (or old "") (or new "") path)
                max-diff-lines))))))

(defun mevedel-reminders--format-edited-files (changes max-diff-lines)
  "Render CHANGES as a single reminder body string.
MAX-DIFF-LINES caps each file's diff size."
  (concat "Files you previously read or edited have been modified \
outside of your tools since you last saw them. Review the changes \
before making further edits; re-read any file whose diff is \
truncated.\n\n"
          (mapconcat (lambda (change)
                       (mevedel-reminders--format-edited-file-change
                        change max-diff-lines))
                     changes "\n\n")))

(defun mevedel-reminders-make-edited-file (&optional max-diff-lines)
  "Create the `edited-file' reminder.

Fires when any file in SESSION's workspace file cache has been
modified externally (by the user, a formatter, a build system, or any
other agent) since a tool last captured it.  Deletion is reported as
well.  Each firing updates the workspace cache so the same change is
not reported on later turns.

MAX-DIFF-LINES caps the per-file diff size (default
`mevedel-reminders-edited-file-max-diff-lines').  The reminder fires
every turn there are changes to report; external edits are important
enough to surface immediately rather than throttle."
  (let ((max-diff-lines (or max-diff-lines
                            mevedel-reminders-edited-file-max-diff-lines))
        ;; Shared between trigger and content so a single firing only
        ;; stats the cache once. Cleared at the start of each trigger
        ;; call and after each content call so turn N does not see
        ;; turn N-1's result.
        (memo nil))
    (mevedel-reminder-create
     :type 'edited-file
     :trigger
     (lambda (session)
       (setq memo nil)
       (when-let* ((ws (mevedel-session-workspace session))
                   (cache (mevedel-workspace-file-cache ws)))
         (with-memoization memo
           (mevedel-file-cache-detect-external-changes cache))))
     :content
     (lambda (session)
       (let* ((ws (mevedel-session-workspace session))
              (cache (mevedel-workspace-file-cache ws))
              (changes (with-memoization memo
                         (mevedel-file-cache-detect-external-changes cache))))
         (setq memo nil)
         (prog1 (mevedel-reminders--format-edited-files
                 changes max-diff-lines)
           (mevedel-file-cache-consume-external-changes cache changes))))
     :interval nil)))


;;
;;; Diagnostics integration

(defun mevedel-reminders--workspace-buffers (session)
  "Return live buffers visiting files under SESSION's workspace root."
  (when-let* ((ws (mevedel-session-workspace session))
              (root (file-name-as-directory
                     (expand-file-name (mevedel-workspace-root ws)))))
    (cl-remove-if-not
     (lambda (buf)
       (when-let* ((file (buffer-file-name buf)))
         (string-prefix-p root (expand-file-name file))))
     (buffer-list))))

(defun mevedel-reminders--collect-flymake-in-buffer ()
  "Return a list of (FILE LINE LEVEL MSG) for Flymake diagnostics.
Scans the current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (let ((file (buffer-file-name))
          (result nil))
      (dolist (diag (flymake-diagnostics))
        (let* ((beg (flymake-diagnostic-beg diag))
               (line (save-excursion (goto-char beg) (line-number-at-pos)))
               (type (flymake-diagnostic-type diag))
               (level (pcase type
                        (:error "error") (:warning "warning") (:note "note")
                        (_ (format "%s" type))))
               (msg (flymake-diagnostic-text diag)))
          (push (list file line level msg) result)))
      (nreverse result))))

(defun mevedel-reminders--collect-flycheck-in-buffer ()
  "Return a list of (FILE LINE LEVEL MSG) for Flycheck errors in current buffer."
  (when (and (bound-and-true-p flycheck-mode)
             (fboundp 'flycheck-overlay-errors-in))
    (let ((file (buffer-file-name))
          (result nil))
      (dolist (err (flycheck-overlay-errors-in (point-min) (point-max)))
        (push (list file
                    (flycheck-error-line err)
                    (format "%s" (flycheck-error-level err))
                    (flycheck-error-message err))
              result))
      (nreverse result))))

(defun mevedel-reminders--collect-diagnostics (session)
  "Collect diagnostics across SESSION's workspace buffers.
Returns a list of (FILE LINE LEVEL MSG) tuples, merging Flymake and
Flycheck output from any buffer that has either checker active."
  (let (results)
    (dolist (buf (mevedel-reminders--workspace-buffers session))
      (with-current-buffer buf
        (setq results
              (append results
                      (mevedel-reminders--collect-flymake-in-buffer)
                      (mevedel-reminders--collect-flycheck-in-buffer)))))
    results))

(defun mevedel-reminders--format-diagnostics (diags)
  "Format DIAGS as a reminder block body."
  (concat "Diagnostics reported in workspace files:\n"
          (mapconcat
           (lambda (d)
             (pcase-let ((`(,file ,line ,level ,msg) d))
               (format "  %s:%d [%s] %s"
                       (if file (file-name-nondirectory file) "?")
                       (or line 0) level msg)))
           diags "\n")))

(defun mevedel-reminders-make-diagnostics (&optional interval)
  "Create the diagnostics reminder.

Scans workspace buffers for Flymake and Flycheck output on every turn
and fires when diagnostics are present.  INTERVAL defaults to nil so
the reminder fires every turn there is something to report; pass an
integer to throttle."
  (mevedel-reminder-create
   :type 'diagnostics
   :trigger (lambda (session)
              (mevedel-reminders--collect-diagnostics session))
   :content (lambda (session)
              (mevedel-reminders--format-diagnostics
               (mevedel-reminders--collect-diagnostics session)))
   :interval interval))


;;
;;; Specialist tool availability

(defconst mevedel-reminders--xref-tool-names
  '("XrefReferences" "XrefDefinitions")
  "Tool names that provide xref-backed code navigation.")

(defconst mevedel-reminders--imenu-tool-names
  '("Imenu")
  "Tool names that provide file-local symbol outlines.")

(defconst mevedel-reminders--treesitter-tool-names
  '("Treesitter")
  "Tool names that provide syntax-tree inspection.")

(defconst mevedel-reminders--elisp-introspection-tool-names
  '("function_source" "variable_source" "function_documentation"
    "variable_documentation" "library_source" "manual_node_contents"
    "symbol_manual_section")
  "Routine Emacs Lisp introspection tools safe to recommend.
`variable_value' is intentionally omitted because values can contain
sensitive runtime state.")

(defun mevedel-reminders--deferred-tool-name-p (session names)
  "Return non-nil when SESSION has any deferred tool named in NAMES."
  (cl-some (lambda (entry)
             (member (cadr (car entry)) names))
           (and (mevedel-session-p session)
                (mevedel-session-deferred-set session))))

(defun mevedel-reminders--loaded-tool-name-p (session names)
  "Return non-nil when SESSION has any currently injected tool named in NAMES."
  (cl-some (lambda (entry)
             (member (car entry) names))
           (and (mevedel-session-p session)
                (mevedel-session-deferred-injected session))))

(defun mevedel-reminders--tool-search-sentence (session names query)
  "Return a ToolSearch sentence for NAMES with QUERY when still deferred."
  (when (mevedel-reminders--deferred-tool-name-p session names)
    (format " If the tool is not callable yet, use `ToolSearch(query=\"%s\", load=true)'; after ToolSearch returns, call the loaded tool in your next tool call."
            query)))

(defun mevedel-reminders--tags-table-available-p ()
  "Return non-nil when an etags backend has a readable tags table."
  (or (and (boundp 'tags-file-name)
           tags-file-name
           (file-readable-p tags-file-name))
      (and (boundp 'tags-table-list)
           (cl-some (lambda (path)
                      (and (stringp path) (file-readable-p path)))
                    tags-table-list))))

(defun mevedel-reminders--xref-backend-kind (backend)
  "Return a coarse symbol describing XREF BACKEND."
  (let ((name (downcase (format "%S" backend))))
    (cond
     ((or (eq backend 'eglot) (string-match-p "eglot" name)) 'eglot)
     ((or (eq backend 'lsp) (string-match-p "lsp" name)) 'lsp)
     ((or (eq backend 'etags) (string-match-p "etags" name)) 'etags)
     ((or (eq backend 'elisp) (string-match-p "elisp" name)) 'elisp)
     (backend 'other))))

(defun mevedel-reminders--xref-available-in-buffer-p ()
  "Return non-nil when current buffer has a useful xref backend."
  (when (require 'xref nil t)
    (condition-case nil
        (let ((kind (mevedel-reminders--xref-backend-kind
                     (xref-find-backend))))
          (pcase kind
            ((or 'eglot 'lsp 'elisp) t)
            ('etags (mevedel-reminders--tags-table-available-p))
            (_ nil)))
      (error nil))))

(defun mevedel-reminders--imenu-available-in-buffer-p ()
  "Return non-nil when current buffer exposes a non-empty Imenu index."
  (when (require 'imenu nil t)
    (condition-case nil
        (progn
          (imenu--make-index-alist t)
          (cl-some (lambda (item)
                     (and (consp item)
                          (stringp (car item))
                          (not (string-prefix-p "*" (car item)))))
                   imenu--index-alist))
      (error nil))))

(defun mevedel-reminders--treesitter-available-in-buffer-p ()
  "Return non-nil when current buffer has an active tree-sitter parser."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-parser-list)
       (condition-case nil
           (treesit-parser-list)
         (error nil))))

(defun mevedel-reminders--elisp-buffer-p ()
  "Return non-nil when current buffer is an Emacs Lisp buffer."
  (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode))

(defun mevedel-reminders-specialist-capabilities (session)
  "Return plist of specialist capabilities visible for SESSION.
Keys are `:xref', `:imenu', `:treesitter', and `:elisp-introspection'.
Only live workspace buffers are inspected; this function never opens
files solely to probe editor integrations."
  (let (xref imenu treesitter elisp-buffer)
    (dolist (buf (mevedel-reminders--workspace-buffers session))
      (with-current-buffer buf
        (setq xref (or xref (mevedel-reminders--xref-available-in-buffer-p)))
        (setq imenu (or imenu (mevedel-reminders--imenu-available-in-buffer-p)))
        (setq treesitter
              (or treesitter
                  (mevedel-reminders--treesitter-available-in-buffer-p)))
        (setq elisp-buffer (or elisp-buffer (mevedel-reminders--elisp-buffer-p)))))
    (list :xref xref
          :imenu imenu
          :treesitter treesitter
          :elisp-introspection
          (and elisp-buffer
               (or (mevedel-reminders--deferred-tool-name-p
                    session mevedel-reminders--elisp-introspection-tool-names)
                   (mevedel-reminders--loaded-tool-name-p
                    session mevedel-reminders--elisp-introspection-tool-names))))))

(defun mevedel-reminders-make-xref-available ()
  "Create the one-shot `xref-available' reminder."
  (mevedel-reminder-create
   :type 'xref-available
   :trigger (lambda (session)
              (plist-get (mevedel-reminders-specialist-capabilities session)
                         :xref))
   :content (lambda (session)
              (concat
               "Symbol-aware xref is available for workspace buffers. Prefer `XrefReferences' for precise symbol references/callers and `XrefDefinitions' for definitions or name discovery instead of `Grep' when working with code symbols."
               (or (mevedel-reminders--tool-search-sentence
                    session mevedel-reminders--xref-tool-names "xref")
                   "")))
   :interval 'one-shot))

(defun mevedel-reminders-make-imenu-available ()
  "Create the one-shot `imenu-available' reminder."
  (mevedel-reminder-create
   :type 'imenu-available
   :trigger (lambda (session)
              (plist-get (mevedel-reminders-specialist-capabilities session)
                         :imenu))
   :content (lambda (session)
              (concat
               "Imenu symbol outlines are available for workspace buffers. Prefer `Imenu' when you need the functions, classes, variables, or sections in one known code file instead of reading or grepping the whole file for structure."
               (or (mevedel-reminders--tool-search-sentence
                    session mevedel-reminders--imenu-tool-names "imenu")
                   "")))
   :interval 'one-shot))

(defun mevedel-reminders-make-treesitter-available ()
  "Create the one-shot `treesitter-available' reminder."
  (mevedel-reminder-create
   :type 'treesitter-available
   :trigger (lambda (session)
              (plist-get (mevedel-reminders-specialist-capabilities session)
                         :treesitter))
   :content (lambda (session)
              (concat
               "Tree-sitter syntax data is available for workspace buffers. Prefer `Treesitter' for syntax-node, AST, parent/child, or structural code questions where text search would be imprecise."
               (or (mevedel-reminders--tool-search-sentence
                    session mevedel-reminders--treesitter-tool-names
                    "treesitter")
                   "")))
   :interval 'one-shot))

(defun mevedel-reminders-make-elisp-introspection-available ()
  "Create the one-shot `elisp-introspection-available' reminder."
  (mevedel-reminder-create
   :type 'elisp-introspection-available
   :trigger (lambda (session)
              (plist-get (mevedel-reminders-specialist-capabilities session)
                         :elisp-introspection))
   :content (lambda (session)
              (concat
               "Emacs Lisp introspection tools are available. For loaded Emacs Lisp state, prefer `function_source', `variable_source', documentation/manual tools, and `library_source' over static file reads when you need what is actually loaded. Do not use `variable_value' routinely; it can expose sensitive runtime state."
               (or (mevedel-reminders--tool-search-sentence
                    session mevedel-reminders--elisp-introspection-tool-names
                    "elisp")
                   "")))
   :interval 'one-shot))


;;
;;; Deferred tools integration

(defun mevedel-reminders--format-deferred-roster (entries)
  "Format ENTRIES as a roster reminder body listing discoverable tools.
ENTRIES is an alist like `mevedel-session-deferred-set' -- each
element is a cons ((CATEGORY NAME) . SUMMARY).  SUMMARY is an
optional ultra-short one-liner the tool definition supplied via
`:summary'.  Tools without a summary list as just \"- NAME\" so
the reminder stays concise; some wrapped tools (gptel introspection
helpers, web tools) carry multi-paragraph docstrings as their
:description, which would otherwise dominate the reminder body."
  (concat "The following tools are discoverable via lazy loading but \
are not currently callable. Do not call these tool names directly. \
Call `ToolSearch' (query=EXACT_NAME_OR_KEYWORDS, load=true) first; \
after ToolSearch reports the tool loaded, call the newly available \
tool on the next model turn. Loaded tools stay available for a few \
turns; calling them resets the timer.\n\n"
          (mapconcat
           (lambda (entry)
             (let ((name (cadr (car entry)))
                   (summary (cdr entry)))
               (if (and (stringp summary) (not (string-empty-p summary)))
                   (format "- %s: %s" name summary)
                 (format "- %s" name))))
           entries "\n")))

(defun mevedel-reminders-make-deferred-tools-roster ()
  "Create the `deferred-tools-roster' reminder.

Fires once per session (interval `one-shot') when the session has a
non-empty deferred tool set.  Its body lists every tool the preset
declared as deferred, along with a usage hint for ToolSearch, so the
model learns which capabilities it can lazily load."
  (mevedel-reminder-create
   :type 'deferred-tools-roster
   :trigger (lambda (session)
              (and (mevedel-session-deferred-set session) t))
   :content (lambda (session)
              (mevedel-reminders--format-deferred-roster
               (mevedel-session-deferred-set session)))
   :interval 'one-shot))

(defun mevedel-reminders--format-deferred-expired (names)
  "Format NAMES as a reminder body announcing expired deferred tools."
  (concat "The following deferred tools have expired and are no \
longer callable: "
          (mapconcat #'identity names ", ")
          ". Do not call these tool names directly. Call \
`ToolSearch' (query=EXACT_NAME_OR_KEYWORDS, load=true) to re-activate \
them before using them again."))

(defun mevedel-reminders-make-deferred-tools-expired ()
  "Create the `deferred-tools-expired' reminder.

Fires on turns where the WAIT handler evicted one or more deferred
tools on the previous turn.  Cites the expired tool names and tells
the model how to recover them via ToolSearch.  Fires every turn there
is something to report; consumes `deferred-expired' as a side effect
so the same names are not re-reported."
  (mevedel-reminder-create
   :type 'deferred-tools-expired
   :trigger (lambda (session)
              (and (mevedel-session-deferred-expired session) t))
   :content (lambda (session)
              (let ((names (mevedel-session-deferred-expired session)))
                (prog1 (mevedel-reminders--format-deferred-expired names)
                  (setf (mevedel-session-deferred-expired session) nil))))
   :interval nil))

(defun mevedel-reminders-make-agent-deferred-tools-roster ()
  "Create the agent-scoped `deferred-tools-roster' reminder.

Mirror of `mevedel-reminders-make-deferred-tools-roster' but reads
from a `mevedel-agent-invocation' context instead of a session.
Added by `mevedel-agent-invocation-create' to any agent whose
resolved `:tools' include deferred entries."
  (mevedel-reminder-create
   :type 'deferred-tools-roster
   :trigger (lambda (inv)
              (and (mevedel-agent-invocation-deferred-set inv) t))
   :content (lambda (inv)
              (mevedel-reminders--format-deferred-roster
               (mevedel-agent-invocation-deferred-set inv)))
   :interval 'one-shot))

(defun mevedel-reminders-make-agent-deferred-tools-expired ()
  "Create the agent-scoped `deferred-tools-expired' reminder.

Mirror of `mevedel-reminders-make-deferred-tools-expired' but reads
from a `mevedel-agent-invocation' context.  Consumes the invocation's
`deferred-expired' slot so the same names are not re-reported."
  (mevedel-reminder-create
   :type 'deferred-tools-expired
   :trigger (lambda (inv)
              (and (mevedel-agent-invocation-deferred-expired inv) t))
   :content (lambda (inv)
              (let ((names (mevedel-agent-invocation-deferred-expired inv)))
                (prog1 (mevedel-reminders--format-deferred-expired names)
                  (mevedel-agent-invocation-set-deferred-expired inv nil))))
   :interval nil))

(defun mevedel-reminders-make-verifier-read-only ()
  "Create the every-turn critical read-only reminder for the verifier agent.

Reinforces that the verifier CANNOT edit, write, or create files and
that its only deliverable is a report.  Fires every turn so the
model cannot drift into implementation mode between messages."
  (mevedel-reminder-create
   :type 'verifier-read-only
   :trigger (lambda (_ctx) t)
   :content (lambda (_ctx)
              "CRITICAL: This is a VERIFICATION-ONLY task. You CANNOT edit, \
write, or create files. Your job is to try to BREAK the \
implementation, not confirm it works. Report findings — do not patch \
them. You MUST end with exactly one of: VERDICT: PASS, VERDICT: FAIL, \
or VERDICT: PARTIAL.")
   :interval nil))

(defun mevedel-reminders-make-reviewer-read-only ()
  "Create the every-turn critical read-only reminder for the reviewer agent.

Reinforces that the reviewer CANNOT edit, write, or create files and
that its only deliverable is a strict JSON review report.  Fires every
turn so the model cannot drift into implementation mode between
messages."
  (mevedel-reminder-create
   :type 'reviewer-read-only
   :trigger (lambda (_ctx) t)
   :content (lambda (_ctx)
              "CRITICAL: This is a REVIEW-ONLY task. You CANNOT edit, \
write, or create files. Inspect the code and report review findings — \
do not patch them. Return only the strict JSON review object requested \
by the reviewer prompt, with findings and overall correctness fields.")
   :interval nil))

(defun mevedel-reminders-make-task-nudge (&optional interval)
  "Create the task-nudge reminder.

Fires when the session has non-completed tasks and task status has not
been written for INTERVAL turns.  INTERVAL defaults to 8 turns."
  (mevedel-reminder-create
   :type 'task-nudge
   :trigger (lambda (session)
              (let ((stale-after (or interval 8)))
                (and (mevedel-session-p session)
                     (cl-some
                      (lambda (task)
                        (not (eq (mevedel-task-status task) 'completed)))
                      (mevedel-session-tasks session))
                     (let ((last-write
                            (mevedel-session-last-task-write-turn session)))
                       (and (integerp last-write)
                            (>= (- (or (mevedel-session-turn-count session) 0)
                                   last-write)
                                stale-after))))))
   :content (lambda (session)
              (require 'mevedel-tool-task)
              (format
               "You have active tasks that have not been updated recently. Review and update task status as you make progress (set to in_progress when starting, completed when done). Use TaskUpdate to keep task status current.\n\n%s"
               (mevedel-tool-task-format-active-groups-for-reminder
                session)))
   :interval nil))

(defun mevedel-reminders-make-verification-suggestion ()
  "Create the every-turn nudge to consider running the verifier.

Fires after the main session has touched files this turn.  Reminds
the assistant to consider spawning the verifier before declaring
non-trivial work complete."
  (mevedel-reminder-create
   :type 'verification-suggestion
   :trigger (lambda (session)
              (and (mevedel-session-p session)
                   (mevedel-session-touched-files session)
                   (> (hash-table-count
                       (mevedel-session-touched-files session))
                      0)))
   :content (lambda (session)
              (concat
               "Consider spawning the verifier agent before reporting \
completion on non-trivial implementations."
               (let ((metadata (mevedel-session-plan-metadata session)))
                 (when (and (eq (plist-get metadata :status) 'approved)
                            (plist-get metadata :verification-pending))
                   " Since you are implementing an approved plan, verify \
that the plan was actually executed, not merely that tests pass."))
               " Adversarial verification often catches regressions \
that pass local tests."))
   :interval 10))


;;
;;; Session defaults

(defun mevedel-reminders-make-agent-background-channels ()
  "Reminder fired once into a background sub-agent's first WAIT.

Tells the sub-agent it is running concurrently with its caller and
lists the routing targets available via `SendMessage'.  The content is
context-sensitive because the channel matrix is intentionally narrow:

  - coordinators and agents spawned directly by main may address
    `to=\"main\"';
  - workers spawned by a coordinator address that coordinator by its
    exact agent id;
  - agents may address only their own spawned background children by
    exact id.

Without this nudge, the LLM sees the SendMessage tool but has no
sense of when to use it (the static prompt-file does not assume
the caller is running concurrently)."
  (mevedel-reminder-create
   :type 'agent-background-channels
   :trigger (lambda (inv)
              (and (mevedel-agent-invocation-p inv)
                   (mevedel-agent-invocation-background-p inv)))
   :content (lambda (inv)
              (let* ((agent-name
                      (and (mevedel-agent-invocation-agent inv)
                           (mevedel-agent-name
                            (mevedel-agent-invocation-agent inv))))
                     (parent-buffer
                      (mevedel-agent-invocation-parent-data-buffer inv))
                     (parent-inv
                      (and (buffer-live-p parent-buffer)
                           (buffer-local-value 'mevedel--agent-invocation
                                               parent-buffer)))
                     (parent-name
                      (and (mevedel-agent-invocation-p parent-inv)
                           (mevedel-agent-invocation-agent parent-inv)
                           (mevedel-agent-name
                            (mevedel-agent-invocation-agent parent-inv))))
                     (parent-id
                      (and (mevedel-agent-invocation-p parent-inv)
                           (mevedel-agent-invocation-agent-id parent-inv))))
                (cond
                 ((equal agent-name "coordinator")
                  "You are running in the background, concurrent with main. \
Use the `SendMessage' tool only for live agent-to-agent communication:

  - `SendMessage(to=\"main\", message=\"...\")' delivers to the top-level \
mevedel session.
  - After you spawn background workers, `SendMessage(to=\"<worker-id>\", \
...)' addresses one of your own live workers by exact id.

Do not use SendMessage for user-facing questions; use the `Ask' tool \
for interactive user input.")
                 ((equal parent-name "coordinator")
                  (format
                   "You are running in the background as a worker for \
coordinator `%s'.  Use `SendMessage(to=\"%s\", message=\"...\")' to send \
partial findings, blockers, or clarification requests to that coordinator.

Do not message `main' directly and do not message sibling workers directly; \
route coordination through your coordinator.  For user-facing questions \
that need an interactive answer, use the `Ask' tool."
                   (or parent-id "coordinator")
                   (or parent-id "coordinator")))
                 ((null parent-inv)
                  "You are running in the background, concurrent with main. \
Use `SendMessage(to=\"main\", message=\"...\")' for partial findings, \
blockers, or status that should reach the top-level mevedel session before \
your final result.

Do not message sibling agents directly.  For user-facing questions that \
need an interactive answer, use the `Ask' tool.")
                 (t
                  "You are running in the background.  SendMessage is scoped \
to live agent-to-agent channels, not sibling discovery.  If you spawn your \
own background children, address those children by their exact returned id; \
otherwise report through your final result.  For user-facing questions, use \
the `Ask' tool."))))
   :interval 'one-shot))

(defun mevedel-reminders-make-background-agents-pending ()
  "Reminder fired while background sub-agents are still running.

Tells the parent LLM to wait for `<agent-result>' blocks instead of
guessing the work is done.  Without this nudge, the model often
declares all agents failed (or summarises prematurely) the moment a
single failure lands while other agents are still in flight.

Fires on every parent WAIT cycle that has a non-empty
`background-agents' list, naming the still-pending agent IDs so the
LLM can match them against `<agent-result>' deliveries it has
received."
  (mevedel-reminder-create
   :type 'background-agents-pending
   :trigger (lambda (ctx)
              (and (mevedel-session-p ctx)
                   (mevedel-session-background-agents ctx)))
   :content (lambda (ctx)
              (let* ((ids (and (mevedel-session-p ctx)
                               (mevedel-session-background-agents ctx)))
                     (count (length ids)))
                (format "You have %d background sub-agent%s still running:

%s

Do NOT produce a final summary or declare any agent as failed until
you have received an `<agent-result agent-id=\"...\">' block for
each agent ID listed above (or until the runtime parks your turn
in BWAIT and resumes with the missing result).  An error from one
agent says nothing about the others; each ID reports back
independently."
                        count
                        (if (= count 1) "" "s")
                        (mapconcat (lambda (id) (format "  - %s" id))
                                   ids "\n"))))))

(defun mevedel-reminders-install-defaults (session)
  "Install Tier 1 built-in reminders on SESSION.

Currently registers built-in session reminders.  Idempotent: reminders
with the same type are not added twice."
  (let ((existing (mapcar #'mevedel-reminder-type
                          (mevedel-session-reminders session))))
    (unless (memq 'pending-events existing)
      (mevedel-session-add-reminder session
                                    (mevedel-reminders-make-pending-events)))
    (unless (memq 'date-change existing)
      (mevedel-session-add-reminder session
                                    (mevedel-reminders-make-date-change)))
    (unless (memq 'compaction-available existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-compaction-available)))
    (unless (memq 'token-usage existing)
      (mevedel-session-add-reminder session
                                    (mevedel-reminders-make-token-usage)))
    (unless (memq 'agent-listing-delta existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-agent-listing-delta)))
    (unless (memq 'xref-available existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-xref-available)))
    (unless (memq 'imenu-available existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-imenu-available)))
    (unless (memq 'treesitter-available existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-treesitter-available)))
    (unless (memq 'elisp-introspection-available existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-elisp-introspection-available)))
    (unless (memq 'mode-constraints existing)
      (mevedel-session-add-reminder session
                                    (mevedel-reminders-make-mode-constraints)))
    (unless (memq 'diagnostics existing)
      (mevedel-session-add-reminder session
                                    (mevedel-reminders-make-diagnostics)))
    (unless (memq 'edited-file existing)
      (mevedel-session-add-reminder session
                                    (mevedel-reminders-make-edited-file)))
    (unless (memq 'deferred-tools-roster existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-deferred-tools-roster)))
    (unless (memq 'deferred-tools-expired existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-deferred-tools-expired)))
    (unless (memq 'task-nudge existing)
      (mevedel-session-add-reminder session
                                    (mevedel-reminders-make-task-nudge)))
    (unless (memq 'verification-suggestion existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-verification-suggestion)))
    (unless (memq 'plan-reference existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-plan-reference)))
    (unless (memq 'background-agents-pending existing)
      (mevedel-session-add-reminder
       session (mevedel-reminders-make-background-agents-pending))))
  session)

(provide 'mevedel-reminders)

;;; mevedel-reminders.el ends here
