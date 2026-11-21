;;; niwa-reminders.el -- System reminders -*- lexical-binding: t -*-

;;; Commentary:

;; System reminders: mid-conversation guidance injected beside the user
;; message as `<system-reminder>' blocks.  Reminders are data: each is a
;; struct with a trigger function, a content function, and optional
;; interval throttling.
;;
;; Reminders live on the session struct for main chat sessions, and on
;; the agent struct (cloned per invocation) for sub-agents.  A prompt
;; transform stages active reminders and a WAIT handler injects them as a
;; separate user-role message before the request is sent.  Turn counting is
;; driven by a terminal FSM handler in the request pipeline.

;;; Code:

(require 'niwa-utilities)


(require 'cl-lib)
(require 'niwa-engine)
(require 'niwa-structs)

(eval-when-compile
  (require 'gptel-request nil t))

;; `gptel'
(defvar gptel-backend)
(defvar gptel-prompt-transform-functions)

;; `gptel-request'
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel--parse-list "ext:gptel-request"
                  (backend prompt-list))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `niwa-agents'
(declare-function niwa-agent-invocation-agent
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-agent-id
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-tool-catalog
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-parent-data-buffer
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-parent-session
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-reminders
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-runtime-settled-p
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-turn-count
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-max-turns "niwa-agents" (agent) t)
(declare-function niwa-agent-name "niwa-agents" (cl-x) t)
(declare-function niwa-agents-specs "niwa-agents" (&optional buffer))
(declare-function niwa-plan-read-only-request-p "niwa-agents" ())
(defvar niwa--agent-invocation)

;; `niwa-compact'
(declare-function niwa--compact-auto-eligible-p "niwa-compact" ())
(defvar niwa-compact-auto)

;; `niwa-compact-estimation'
(declare-function niwa-compact-estimation-estimate-tokens
                  "niwa-compact-estimation" ())
(declare-function niwa-compact-estimation-threshold-tokens
                  "niwa-compact-estimation" (&optional usable-tokens))
(declare-function niwa-compact-estimation-usable-tokens
                  "niwa-compact-estimation" ())
(autoload 'niwa-compact-estimation-estimate-tokens
  "niwa-compact-estimation")
(autoload 'niwa-compact-estimation-threshold-tokens
  "niwa-compact-estimation")
(autoload 'niwa-compact-estimation-usable-tokens
  "niwa-compact-estimation")

;; `niwa-file-state'
(declare-function niwa-file-cache-consume-external-changes
                  "niwa-file-state" (cache changes))
(declare-function niwa-file-cache-detect-external-changes
                  "niwa-file-state" (cache))
(autoload 'niwa-file-cache-consume-external-changes "niwa-file-state")
(autoload 'niwa-file-cache-detect-external-changes "niwa-file-state")

;; `niwa-hooks'
(declare-function niwa-hooks-consume-session-context
                  "niwa-hooks" (session entries))
(declare-function niwa-hooks-format-context "niwa-hooks"
                  (entries))
(autoload 'niwa-hooks-consume-session-context "niwa-hooks")
(autoload 'niwa-hooks-format-context "niwa-hooks")

;; `niwa-plan'
(declare-function niwa-plan-resource-address "niwa-plan"
                  (relative-path))
(autoload 'niwa-plan-resource-address "niwa-plan")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer request invocation))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-permission-review'
(defvar niwa-permission-reviewer)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-read-artifact
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-artifact
  "niwa-session-artifacts")

;; `niwa-session-fork'
(declare-function niwa-session-fork-provenance-body
                  "niwa-session-fork" (session))
(autoload 'niwa-session-fork-provenance-body "niwa-session-fork")

;; `niwa-structs'
(declare-function niwa-goal-plan-reference "niwa-structs" (cl-x) t)
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-session-goal "niwa-structs" (cl-x) t)
(declare-function niwa-session-plan-metadata "niwa-structs" (cl-x) t)
(defvar niwa--current-request)

;; `niwa-telemetry'
(declare-function niwa-telemetry-current-session
                  "niwa-telemetry" (&optional buffer))
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-tool-task'
(declare-function niwa-tool-task-format-active-for-llm
                  "niwa-tool-task" (session))
(autoload 'niwa-tool-task-format-active-for-llm "niwa-tool-task")

;; `niwa-transcript'
(declare-function niwa-transcript-prompt-transform-start
                  "niwa-transcript" ())
(autoload 'niwa-transcript-prompt-transform-start "niwa-transcript")

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))
(autoload 'niwa--format-hook-audit-record "niwa-transcript-audit")

;; `niwa-utilities'
(declare-function niwa--active-response-marker
                  "niwa-utilities" (info buffer))
(declare-function niwa--plain-data-p "niwa-utilities" (value))
(declare-function niwa--split-open-reasoning-before-user-input
                  "niwa-utilities" (info))
(declare-function niwa-generate-diff
                  "niwa-utilities"
                  (original modified filepath callback &optional labels-real settlement))
(autoload 'niwa--active-response-marker "niwa-utilities")
(autoload 'niwa--plain-data-p "niwa-utilities")
(autoload 'niwa--split-open-reasoning-before-user-input
  "niwa-utilities")
(autoload 'niwa-generate-diff "niwa-utilities")

;; Current prompt-transform context.
(defvar niwa-reminders--current-chat-buffer nil
  "Chat buffer whose reminders are currently being collected.
Bound dynamically by `niwa-reminders--transform' so reminder
triggers can distinguish the real chat buffer from gptel's temporary
prompt buffer.")

(defconst niwa-reminders-reconciliation-body
  (concat "A previous session or request ended without proving that all "
          "effects settled. Processes may still be running, and aborted "
          "tools or commands may have partially changed files, tasks, or "
          "external state. Reconcile current state before continuing, "
          "prioritize the newest user request over any older ghost request, "
          "and verify effects before making final success claims.")
  "Recovery guidance shared by resumed native and external conversations.")

(defvar-local niwa-reminders--turn-events nil
  "Owner-bound reminder events for the current model turn.
The value is `(:owner OWNER :items ((KEY :body BODY :commit FUNCTION) ...))'.")

(defvar-local niwa-reminders--reserved-hook-context nil
  "Hook-context entries this buffer's in-flight request reserved.
Held out of the session's pending context while a request carries them,
and restored by `niwa-reminders-restore-reserved-context' when that
request never delivers.")


;;
;;; Reminder struct

(cl-defstruct (niwa-reminder (:constructor niwa-reminder--create))
  "A single system reminder.

A reminder's TRIGGER is called with a context object (session struct
for main chat, agent-specific context for sub-agents) and returns
non-nil when the reminder should fire on the current turn.  CONTENT is
called with the same context object and returns either the reminder body
string or a plist with `:body' and an optional `:commit' thunk, which
runs once the payload reaches the request.  A nil body stages only the
commit, for a silent acknowledgement of context already in the prompt.

INTERVAL controls firing frequency:
  - nil        - fire every turn the trigger returns non-nil
  - integer    - minimum number of turns between firings
  - `one-shot' - fire at most once per reminder lifetime

LAST-FIRED is the turn count when this reminder last fired, or nil if
it has never fired.  RECIPE is a read-safe declarative constructor form used
only when a frozen agent template must survive a cold session resume."
  type
  trigger
  content
  interval
  last-fired
  mode-constraints-observation ; last body actually delivered by mode-constraints
  recipe)

(defvar niwa-reminders--recipe-schemas
  '((mode-constraints niwa-reminders-make-mode-constraints no-args)
    (plan-mode niwa-reminders-make-plan-mode no-args)
    (plan-reference niwa-reminders-make-plan-reference no-args)
    (pending-events niwa-reminders-make-pending-events no-args)
    (date-change niwa-reminders-make-date-change no-args)
    (compaction-available
     niwa-reminders-make-compaction-available ratio)
    (token-usage niwa-reminders-make-token-usage token-usage)
    (agent-listing-delta
     niwa-reminders-make-agent-listing-delta no-args)
    (max-turns-warning niwa-reminders-make-max-turns-warning ratio)
    (edited-file niwa-reminders-make-edited-file count)
    (task-nudge niwa-reminders-make-task-nudge interval)
    (user-revised-patch niwa-reminders-make-user-revised-patch text)
    (verification-suggestion
     niwa-reminders-make-verification-suggestion no-args))
  "Trusted constructor and argument contracts for durable reminder recipes.")

(defun niwa-reminders--recipe-arguments-p (contract arguments)
  "Return non-nil when ARGUMENTS satisfy recipe argument CONTRACT."
  (and
   (proper-list-p arguments)
   (pcase contract
     ('no-args (null arguments))
     ('interval
      (and (= (length arguments) 1)
           (or (null (car arguments))
               (and (integerp (car arguments)) (>= (car arguments) 0)))))
     ('ratio
      (and (= (length arguments) 1)
           (numberp (car arguments))
           (<= 0.0 (car arguments) 1.0)))
     ('count
      (and (= (length arguments) 1)
           (integerp (car arguments))
           (>= (car arguments) 0)))
     ('text
      (and (= (length arguments) 1)
           (stringp (car arguments))))
     ('token-usage
      (and (= (length arguments) 2)
           (numberp (car arguments))
           (<= 0.0 (car arguments) 1.0)
           (or (null (cadr arguments))
               (and (integerp (cadr arguments))
                    (>= (cadr arguments) 0)))))
     (_ nil))))

(defun niwa-reminders--recipe-p (recipe)
  "Return non-nil when RECIPE names a trusted read-safe constructor."
  (when (and (proper-list-p recipe)
             (symbolp (car recipe))
             (niwa--plain-data-p recipe))
    (when-let* ((schema (assq (car recipe)
                              niwa-reminders--recipe-schemas)))
      (niwa-reminders--recipe-arguments-p
       (nth 2 schema) (cdr recipe)))))

(cl-defun niwa-reminder-create
    (&key type trigger content interval recipe)
  "Create a reminder with TYPE, TRIGGER, CONTENT, INTERVAL, and RECIPE.

TYPE is a symbol identifying the reminder kind.  TRIGGER and CONTENT
are functions of one argument (the firing context).  INTERVAL controls
firing frequency: nil for every-turn, an integer for throttled firing,
or the symbol `one-shot' for fire-at-most-once.  RECIPE is an optional
read-safe list whose first element names a trusted constructor in
`niwa-reminders--recipe-schemas'."
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
  (when (and recipe (not (niwa-reminders--recipe-p recipe)))
    (error "Reminder :recipe must name a trusted reminder constructor, got %S"
           recipe))
  (niwa-reminder--create
   :type type
   :trigger trigger
   :content content
   :interval interval
   :last-fired nil
   :recipe (copy-tree recipe)))

(defun niwa-reminder-clone (reminder)
  "Return a shallow copy of REMINDER with delivery observations reset.

Trigger, content, and interval are shared by reference; only the
per-invocation state is reset so cloned reminders track
their own firing history."
  (let ((clone (copy-niwa-reminder reminder)))
    (setf (niwa-reminder-last-fired clone) nil
          (niwa-reminder-mode-constraints-observation clone) nil
          (niwa-reminder-recipe clone)
          (copy-tree (niwa-reminder-recipe reminder)))
    clone))

(defun niwa-reminders-clone-list (reminders)
  "Return a fresh list of cloned REMINDERS.

Each element is copied via `niwa-reminder-clone' so the returned
list tracks its own LAST-FIRED state independently of REMINDERS."
  (mapcar #'niwa-reminder-clone reminders))

(defun niwa-reminders-serialize-agent-templates (reminders)
  "Return durable recipes for frozen agent REMINDERS.

A reminder without a trusted recipe is rejected rather than silently changing
a retained agent's frozen identity after resume."
  (mapcar
   (lambda (reminder)
     (unless (niwa-reminder-p reminder)
       (error "Invalid frozen agent reminder: %S" reminder))
     (let ((recipe (niwa-reminder-recipe reminder)))
       (unless (and recipe (niwa-reminders--recipe-p recipe))
         (error "Agent reminder has no durable recipe: %S"
                (niwa-reminder-type reminder)))
       (copy-tree recipe)))
   reminders))

(defun niwa-reminders-restore-agent-templates (recipes)
  "Restore frozen agent reminder RECIPES through trusted constructors.

Executable or opaque sidecar values are rejected before any trusted constructor
is called."
  (unless (proper-list-p recipes)
    (error "Invalid persisted agent reminder recipes"))
  (mapcar
   (lambda (recipe)
     (unless (niwa-reminders--recipe-p recipe)
       (error "Invalid persisted agent reminder recipe"))
     (let* ((schema (assq (car recipe) niwa-reminders--recipe-schemas))
            (factory (nth 1 schema)))
       (unless (and factory (fboundp factory))
         (error "Unknown persisted agent reminder recipe: %S"
                (car recipe)))
       (let ((reminder (apply factory (cdr recipe))))
         (unless (and (niwa-reminder-p reminder)
                      (equal recipe (niwa-reminder-recipe reminder)))
           (error "Invalid restored agent reminder recipe: %S" recipe))
         reminder)))
   recipes))


;;
;;; Session reminder helpers

(defun niwa-session-add-reminder (session reminder)
  "Append REMINDER to SESSION's reminder list."
  (setf (niwa-session-reminders session)
        (append (niwa-session-reminders session) (list reminder)))
  reminder)

(defun niwa-session-remove-reminder (session type)
  "Remove all reminders of TYPE from SESSION."
  (setf (niwa-session-reminders session)
        (cl-remove-if (lambda (r) (eq (niwa-reminder-type r) type))
                      (niwa-session-reminders session))))


;;
;;; Firing logic

(defun niwa-reminders--should-fire-p (reminder turn-count ctx)
  "Return non-nil if REMINDER should fire at TURN-COUNT for CTX.

CTX is the firing context passed to the reminder's trigger function.
A reminder fires when its trigger returns non-nil AND the interval
policy permits firing.  For nil interval the reminder always fires,
for `one-shot' it fires only if it has never fired, for integer
interval it fires once enough turns have passed since the last fire.
Mode constraints fire only when their delivered body changes."
  (and (funcall (niwa-reminder-trigger reminder) ctx)
       (let ((interval (niwa-reminder-interval reminder))
             (last-fired (niwa-reminder-last-fired reminder)))
         (let ((due (cond
                     ((null interval) t)
                     ((eq interval 'one-shot) (null last-fired))
                     (t (or (null last-fired)
                            (>= (- turn-count last-fired) interval))))))
           (if (eq (niwa-reminder-type reminder) 'mode-constraints)
               (not (equal (niwa-reminder-mode-constraints-observation reminder)
                           (funcall (niwa-reminder-content reminder) ctx)))
             due)))))

(defun niwa-reminders-format-block (content)
  "Wrap CONTENT in a `<system-reminder>' XML block."
  (format "<system-reminder>\n%s\n</system-reminder>" content))

(defun niwa-reminders--entry-label (type)
  "Return the display label for entry TYPE.
TYPE is a reminder type symbol or a cons turn-event key such as
\(hook . post-tool)."
  (if (consp type)
      (format "%s:%s" (car type) (cdr type))
    (format "%s" type)))

(defun niwa-reminders--collect-from (reminders turn-count ctx)
  "Evaluate REMINDERS at TURN-COUNT and return staged entries and commits.

REMINDERS is a list of `niwa-reminder' structs.  TURN-COUNT is the
current turn counter used for interval checks.  CTX is the firing
context passed to each reminder's trigger and content functions.

A content function returns either a body string or a plist with `:body'
and an optional `:commit' thunk, the same shape queued turn events use.
Nothing is consumed here: marking a reminder fired and running any
content commit are deferred to the returned `:commits', which the
injector runs once the payload has reached the request.  Returns a plist
with `:entries' in reminder order, each (:type TYPE :body BODY), and
`:commits'."
  (let ((entries nil)
        (commits nil))
    (dolist (reminder reminders)
      (when (niwa-reminders--should-fire-p reminder turn-count ctx)
        (let* ((result (funcall (niwa-reminder-content reminder) ctx))
               (body (if (stringp result) result (plist-get result :body)))
               (mode-constraints-p
                (eq (niwa-reminder-type reminder) 'mode-constraints))
               (commit (and (not (stringp result))
                            (plist-get result :commit))))
          (when body
            (push (list :type (niwa-reminder-type reminder) :body body)
                  entries))
          (when commit (push commit commits))
          (push (lambda ()
                  (setf (niwa-reminder-last-fired reminder) turn-count)
                  (when mode-constraints-p
                    (setf (niwa-reminder-mode-constraints-observation reminder)
                          body)))
                commits))))
    (list :entries (nreverse entries)
          :commits (nreverse commits))))

(defvar niwa-reminders--prepared-edits nil
  "Edited-file changes and diffs prepared for the dispatch in progress.
An alist from a workspace file cache to (CHANGES . DIFFS), where DIFFS is
an alist from a change in CHANGES to its unified diff.  Bound only while
`niwa-reminders-prepare-edited-files' continues a dispatch or
`niwa-reminders-collect' uses retained diffs, so the reminder reports
exactly the changes whose diffs were prepared.")

(defun niwa-reminders-collect (owner)
  "Collect OWNER's configured reminders and pending hook context for delivery.
Use the owning buffer's session or retained invocation and existing turn count.
Edited-file diffs retained by `niwa-reminders-retain-edited-files' are
used once.
Do not acknowledge or consume the captured context until the returned commits."
  (when-let* ((info (niwa-engine-info owner))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (let ((retained (plist-get info :niwa-prepared-edits)))
      (when retained
        (setf (niwa-engine-info owner)
              (plist-put info :niwa-prepared-edits nil)))
      (with-current-buffer buffer
        (let* ((invocation niwa--agent-invocation)
               (context (or invocation (bound-and-true-p niwa--session)))
               (niwa-reminders--prepared-edits
                (append retained niwa-reminders--prepared-edits))
               (niwa-reminders--current-chat-buffer buffer))
          (when context
            (let* ((batch (niwa-reminders--collect-from
                           (if invocation (niwa-agent-invocation-reminders invocation)
                             (niwa-session-reminders context))
                           (if invocation (niwa-agent-invocation-turn-count invocation)
                             (niwa-session-turn-count context))
                           context))
                   (hooks (and (not invocation) (niwa-session-hook-context-pending context))))
              (when-let* ((body (niwa-hooks-format-context hooks)))
                (setq batch
                      (list :entries (cons (list :type 'hook-context :body body) (plist-get batch :entries))
                            :commits (cons (lambda () (niwa-hooks-consume-session-context context hooks))
                                           (plist-get batch :commits)))))
              batch)))))))

(defun niwa-reminders--current-buffer ()
  "Return the chat buffer currently collecting reminders, or nil."
  (and (buffer-live-p niwa-reminders--current-chat-buffer)
       niwa-reminders--current-chat-buffer))

(defun niwa-reminders--compact-token-state ()
  "Return context-pressure state for the current chat buffer.
The returned plist contains `:tokens', `:threshold', `:usable', and `:ratio'.
Return nil without an active collection buffer or for external model history."
  (when-let* ((buf (niwa-reminders--current-buffer)))
    (with-current-buffer buf
      (unless (niwa-engine-external-p gptel-backend)
        (let* ((tokens (niwa-compact-estimation-estimate-tokens))
               (threshold (niwa-compact-estimation-threshold-tokens))
               (usable (niwa-compact-estimation-usable-tokens))
               (ratio (and (numberp usable)
                           (> usable 0)
                           (/ (float tokens) usable))))
          (list :tokens tokens
                :threshold threshold
                :usable usable
                :ratio ratio))))))

(defun niwa-reminders--compact-auto-available-p ()
  "Return non-nil when auto-compaction can run in the current chat buffer."
  (when-let* ((buf (niwa-reminders--current-buffer))
              ((fboundp 'niwa--compact-auto-eligible-p)))
    (with-current-buffer buf
      (niwa--compact-auto-eligible-p))))

(defun niwa-reminders--agent-snapshot ()
  "Return currently visible agent types as an alist.
The shape is (NAME . DESCRIPTION), sorted by NAME.  Returns nil when
the current chat buffer has no request-local agent roster yet."
  (when-let* ((buf (niwa-reminders--current-buffer)))
    (with-current-buffer buf
      (when-let* ((specs (niwa-agents-specs)))
        (sort
         (mapcar (lambda (entry)
                   (cons (car entry)
                         (plist-get (cdr entry) :description)))
                 specs)
         (lambda (a b) (string< (car a) (car b))))))))

(defun niwa-reminders--format-agent-delta (added removed)
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
;;; Prompt delivery

(defun niwa-reminders-turn-owner (&optional buffer)
  "Return the active model-turn owner for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    ;; Only `niwa-agents' creates an invocation, so test for one first.
    (or (and niwa--agent-invocation
             (niwa-agent-invocation-p niwa--agent-invocation)
             (not (niwa-agent-invocation-runtime-settled-p
                   niwa--agent-invocation))
             niwa--agent-invocation)
        niwa--current-request)))

(defun niwa-reminders-queue-turn-event (buffer key body &optional commit)
  "Queue BODY under KEY for BUFFER's current model turn.
Replacing an existing KEY coalesces repeated observations.  Run COMMIT after
the engine acknowledges delivery.  Return non-nil when BUFFER has a live
request or agent invocation that owns the event."
  (when (and (buffer-live-p buffer)
             (stringp body)
             (not (string-empty-p body)))
    (with-current-buffer buffer
      (when-let* ((owner (niwa-reminders-turn-owner buffer)))
        (unless (eq owner (plist-get niwa-reminders--turn-events :owner))
          (setq niwa-reminders--turn-events
                (list :owner owner :items nil)))
        (let ((items (plist-get niwa-reminders--turn-events :items)))
          (setq items (append (assoc-delete-all key items)
                              (list (list key :body body :commit commit))))
          (setq niwa-reminders--turn-events
                (plist-put niwa-reminders--turn-events :items items)))
        t))))

(defun niwa-reminders-restore-reserved-context (buffer)
  "Return BUFFER's reserved hook context to its session's pending list.
Called from the boundaries that settle a turn which never delivered, so
context reserved for it is offered to the next request instead of lost."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((contexts niwa-reminders--reserved-hook-context)
                  (session (bound-and-true-p niwa--session)))
        (setq-local niwa-reminders--reserved-hook-context nil)
        (setf (niwa-session-hook-context-pending session)
              (append contexts
                      (niwa-session-hook-context-pending session)))
        t))))

(defun niwa-reminders--stage-turn-events (buffer)
  "Return live turn-event entries and commits queued for BUFFER.
The queue is not cleared here: dequeueing is the last of the returned
`:commits', so staging alone acknowledges nothing.  Receipt removes only the
captured objects, preserving later events even when they replace the same key."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((owner (niwa-reminders-turn-owner buffer)))
        (if (not (eq owner
                     (plist-get niwa-reminders--turn-events :owner)))
            (progn
              (setq niwa-reminders--turn-events nil)
              nil)
          (let ((items (plist-get niwa-reminders--turn-events :items)))
            (list
             :entries
             (mapcar (lambda (entry)
                       (list :type (car entry)
                             :body (plist-get (cdr entry) :body)))
                     items)
             :commits
             (append
              (delq nil
                    (mapcar (lambda (entry)
                              (plist-get (cdr entry) :commit))
                            items))
              ;; Dequeue last, after committing the captured observations.
              (list
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (eq owner (plist-get niwa-reminders--turn-events :owner))
                       (let ((remaining
                              (cl-remove-if
                               (lambda (entry) (memq entry items))
                               (plist-get niwa-reminders--turn-events :items))))
                         (setq niwa-reminders--turn-events
                               (and remaining (list :owner owner :items remaining)))))))))))))))))

(defun niwa-reminders--injection-record (entries phase)
  "Return the complete delivered reminder record for ENTRIES in PHASE."
  (list :type 'injected-reminders :phase phase :items (copy-tree entries)))

(defun niwa-reminders--write-injection-record (info buffer entries phase)
  "Record injected ENTRIES for PHASE as hidden text in BUFFER.

`gptel--inject-prompt' mutates the realized request payload without
touching the data buffer, so the injected blocks would otherwise leave
no trace.  The record is a hook-audit side-channel block: invisible,
decoded for later provider requests and rendered as a collapsed view row.
INFO is the request's fsm info; insertion happens at its active
response marker so streamed output continues after the record.  A
missing marker signals rather than committing unrecorded guidance."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if-let* ((marker (niwa--active-response-marker info buffer)))
        (let ((inhibit-read-only t))
          (niwa--split-open-reasoning-before-user-input info)
          (save-excursion
            (goto-char marker)
            (insert (niwa--format-hook-audit-record
                     (niwa-reminders--injection-record entries phase)))
            (set-marker marker (point))))
        (error "Missing reminder transcript insertion marker")))))

(defun niwa-reminders--handle-inject (fsm)
  "Inject staged and same-turn reminders into FSM's request payload.

Consumable state is committed only once the payload exists, so a request
that fails to realize or is cancelled before this point keeps everything
for the next turn."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (data (plist-get info :data))
         (initial (plist-get info :niwa-reminder-entries))
         (staged-commits (plist-get info :niwa-reminder-commits))
         ;; Delivery and its durable record both follow the current prompt
         ;; or complete tool results.  Never split calls from their results.
         (turn-start (not (plist-get info :niwa-reminders-wait-seen))))
    (when (and data (buffer-live-p buffer))
      (let* ((events (niwa-reminders--stage-turn-events buffer))
             (entries (append initial (plist-get events :entries))))
        (when entries
          (let* ((backend (plist-get info :backend))
                 (message
                  (car (gptel--parse-list
                        backend
                        (list (cons 'prompt
                                    (mapconcat
                                     (lambda (entry)
                                       (niwa-reminders-format-block
                                        (plist-get entry :body)))
                                     entries "\n")))))))
            (let ((previous (copy-sequence data)))
              (condition-case err
                  (progn
                    (gptel--inject-prompt backend data message)
                    (niwa-reminders--write-injection-record
                     info buffer entries (if turn-start 'turn-start 'mid-turn)))
                (error
                 (setcar data (car previous))
                 (setcdr data (cdr previous))
                 (signal (car err) (cdr err)))))
            (dolist (commit (plist-get events :commits))
              (condition-case-unless-debug commit-error (funcall commit)
                (error (niwa-diagnostic-message (niwa-diagnostic-scope buffer)
                                                  "Reminder commit failed: %S" commit-error))))))
        ;; Staged commits belong to a payload existing at all: hook
        ;; context reached it as prompt text rather than as a block, so
        ;; gating these on `entries' would leave it pending forever.
        (dolist (commit staged-commits)
          (condition-case-unless-debug commit-error (funcall commit)
            (error (niwa-diagnostic-message (niwa-diagnostic-scope buffer)
                                              "Reminder commit failed: %S" commit-error))))
        (setf (gptel-fsm-info fsm)
              (plist-put
               (plist-put
                (plist-put info :niwa-reminder-entries nil)
                :niwa-reminder-commits nil)
               :niwa-reminders-wait-seen t))))))

(defun niwa-reminders--stage-batch (fsm entries commits)
  "Append ENTRIES and deferred COMMITS to FSM in delivery order.
Entries are already collected; body validation belongs to their producer."
  (let ((info (gptel-fsm-info fsm)))
    (setf (gptel-fsm-info fsm)
          (plist-put
           (plist-put info :niwa-reminder-entries
                      (append (plist-get info :niwa-reminder-entries)
                              entries))
           :niwa-reminder-commits
           (append (plist-get info :niwa-reminder-commits) commits)))))

(defun niwa-reminders--transform (fsm)
  "Stage system reminders for separate injection into FSM.

Operates on the current buffer, which is the temporary prompt buffer
passed by `gptel-prompt-transform-functions'.  The session lives on the
chat buffer, which is reached via FSM's info plist's :buffer entry.

FSM is mandatory (not `&optional') so that
`gptel-prompt-transform-functions' dispatch -- which inspects the
function's minimum arity -- passes the FSM argument rather than invoking
the transform with zero arguments.

The prompt text itself is left unchanged except for existing hook context.
Runs after `niwa--transform-expand-mentions'."
  (when-let* ((chat-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p chat-buffer))
              (session (buffer-local-value 'niwa--session chat-buffer)))
    (let ((niwa-reminders--current-chat-buffer chat-buffer))
      (let ((commits nil))
        (when-let* ((contexts (niwa-session-hook-context-pending session)))
          ;; Reserve rather than defer: these entries are in flight for
          ;; this request, so leaving them pending would let a mid-request
          ;; taker -- automatic compaction's context epoch, or a prompt
          ;; prepared in the composer -- deliver them a second time.  The
          ;; reservation is restored if the request never delivers.
          (niwa-hooks-consume-session-context session contexts)
          (with-current-buffer chat-buffer
            (setq-local niwa-reminders--reserved-hook-context
                        (append niwa-reminders--reserved-hook-context
                                contexts)))
          (push (lambda ()
                  (when (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (setq-local niwa-reminders--reserved-hook-context
                                  nil))))
                commits)
          (goto-char (niwa-transcript-prompt-transform-start))
          (let ((start (point)))
            (insert "\n"
                    (niwa-hooks-format-context contexts)
                    "\n")
            (remove-text-properties
             start (point)
             '(gptel nil response nil invisible nil front-sticky nil))))
        (let ((staged (niwa-reminders-collect fsm)))
          ;; Append: transforms at earlier depths (mentions,
          ;; skills-input) may already have staged entries.
          (niwa-reminders--stage-batch
           fsm (plist-get staged :entries)
           (append commits (plist-get staged :commits))))))))

(defun niwa-reminders--agent-transform (fsm)
  "Stage agent-invocation reminders for separate injection into FSM.

Agent conversation buffers carry no session; their reminder roster
lives on the invocation struct (configured reminders and max-turns
warning).  Runs once per agent request in gptel's temporary prompt
buffer and stages entries exactly like `niwa-reminders--transform'
does for sessions, so `niwa-reminders--handle-inject' delivers and
records them identically.

FSM is mandatory for the same arity-dispatch reason as
`niwa-reminders--transform'."
  (when-let* ((buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p buffer))
              (invocation
               (with-current-buffer buffer
                 (and (niwa-agent-invocation-p niwa--agent-invocation)
                      niwa--agent-invocation))))
    (let ((niwa-reminders--current-chat-buffer buffer))
      (let ((staged (niwa-reminders-collect fsm)))
        (niwa-reminders--stage-batch
         fsm (plist-get staged :entries) (plist-get staged :commits))))))

(defun niwa-reminders-stage-commit (fsm commit)
  "Defer COMMIT until FSM's request payload provably exists.
For consumable state whose delivery rides the prompt or the staged
entries: `niwa-reminders--handle-inject' runs COMMIT at WAIT even
when no reminder entry fired, and a request that dies earlier never
runs it."
  (when (functionp commit)
    (let ((info (gptel-fsm-info fsm)))
      (setf (gptel-fsm-info fsm)
            (plist-put info :niwa-reminder-commits
                       (append (plist-get info :niwa-reminder-commits)
                               (list commit)))))))

(defun niwa-reminders-stage-entry (fsm type body &optional commit)
  "Stage one system-reminder entry of TYPE with BODY on FSM's request.

The entry joins the synthetic user-role reminder message that
`niwa-reminders--handle-inject' injects at the request's next WAIT.
COMMIT, when non-nil, runs once the payload exists.  Callable from any
prompt transform and from WAIT-time handlers that run before injection."
  (when (and (stringp body) (not (string-empty-p body)))
    (niwa-reminders--stage-batch
     fsm (list (list :type type :body body)) (and commit (list commit)))))


;;
;;; Tier 1 built-in reminders

(defun niwa-reminders--session-mode (session)
  "Return the effective permission mode for SESSION or an agent invocation.
Falls back to the global `niwa-permission-mode' default."
  (if (niwa-session-p session)
      (niwa-permission-mode-effective session)
    (niwa-permission-mode-effective
     (niwa-agent-invocation-parent-session session) nil nil nil session)))

(defvar niwa-reminders--mode-constraint-messages
  '((ask . "Permission mode: `ask'. Full Access is not active. Native edits use patch review; uncertain Bash, live Eval and additional authority require approval. Execution uses the configured sandbox preference. Explicit hard denies, Plan restrictions and resource boundaries remain.")
    (edits . "Permission mode: `edits'. Native edits and confined Bash/fresh Eval are automatic. Ordinary native reads include OS-readable paths; configured credential masks remain. Live Eval and additional authority require approval.")
    (ask-auto . "Permission mode: `ask'. Full Access is not active. Native edits use patch review; uncertain Bash, live Eval and additional authority go to the automatic reviewer first. It typically approves task-relevant observational live Eval and clearly scoped requests without involving the user; mutations, unknown effects and unclear scope go to the user. Execution uses the configured sandbox preference. Explicit hard denies, Plan restrictions and resource boundaries remain.")
    (edits-auto . "Permission mode: `edits'. Native edits and confined Bash/fresh Eval are automatic. Ordinary native reads include OS-readable paths; configured credential masks remain. Live Eval and additional authority go to the automatic reviewer first. It typically approves task-relevant observational live Eval and clearly scoped requests without involving the user; mutations, unknown effects and unclear scope go to the user.")
    (full-auto . "Permission mode: `full-auto'. Full Access under the target OS account disables confinement and permission prompts, including default protected paths. Native tools and live Eval are automatic. Live Eval runs inside Emacs without confinement. Explicit hard denies, Plan restrictions and integrity checks remain. Keep destructive tool calls deliberate."))
  "Alist mapping mode/reviewer keys to reminder body strings.")

(defun niwa-reminders-make-mode-constraints ()
  "Create the mode-constraints reminder.

Fires initially and on effective-mode or reviewer wording changes, including
returning to Ask, without repeating an unchanged advisory.
The delivery observation changes only when the payload reaches the request."
  (niwa-reminder-create
   :type 'mode-constraints
   :recipe '(mode-constraints)
   :trigger (lambda (_session) t)
   :content (lambda (session)
              (let* ((mode (niwa-reminders--session-mode session))
                     (key (if (eq (bound-and-true-p niwa-permission-reviewer) 'auto)
                              (pcase mode ('ask 'ask-auto) ('edits 'edits-auto) (_ mode))
                            mode)))
                (or (alist-get key niwa-reminders--mode-constraint-messages)
                    (format "Permission mode: `%s'." mode))))
   :interval nil))

(defun niwa-reminders-make-plan-mode ()
  "Create Plan guidance: full on first delivery, then a recovery reference."
  (niwa-reminder-create
   :type 'plan-mode
   :recipe '(plan-mode)
   :trigger (lambda (session)
              (or (and (niwa-session-p session)
                       (niwa-session-plan-mode session))
                  (niwa-plan-read-only-request-p)))
   :content
   (lambda (context)
     (let* ((reminders (if (niwa-session-p context)
                           (niwa-session-reminders context)
                         (niwa-agent-invocation-reminders context)))
            (reminder (cl-find 'plan-mode reminders :key #'niwa-reminder-type)))
       (if (and reminder (niwa-reminder-last-fired reminder))
           "Plan mode remains active. Follow the earlier contract; if unavailable, read `niwa://plan-mode.md` (Proposal guidance). Finish with one <proposed_plan>...</proposed_plan> block replacing the prior proposal."
         (concat
          "Plan mode is active: inspect and discuss, not implement. Bash is limited to read-only-classified commands; Eval is unavailable. Standalone/sticky Plan and requests carrying inherited Plan limits permit ApplyPatch only for session-owned work:// descendants. Agents you start inherit these Plan limits. Delegate independent investigation or critique of a draft plan supplied in the task when useful. An implementation request means produce or revise the plan. Explore available evidence before asking about genuine user preferences it cannot resolve. Every new <proposed_plan> block replaces the previous proposal completely. Do not ask whether you should proceed with implementation. Finish with exactly one line-oriented block:\n\n"
          "<proposed_plan>\n"
          "# Concrete Plan Title\n\n"
          "## Summary\n"
          "- Root cause or goal, behavior change, non-goals.\n\n"
          "## Key Changes\n"
          "- Group by subsystem or behavior; name files, APIs, interfaces, or data shapes only to remove ambiguity.\n\n"
          "## Regression Coverage\n"
          "- User-visible flows, edge cases, and failures to test.\n\n"
          "## Validation\n"
          "- Exact focused test/build commands.\n\n"
          "## Assumptions\n"
          "- Defaults, compatibility assumptions, unchanged behavior.\n"
          "</proposed_plan>\n\n"))))))

(defun niwa-reminders-rearm-plan-mode (session)
  "Make SESSION's next Plan delivery include the full template again."
  (when-let* ((reminder (cl-find 'plan-mode (niwa-session-reminders session)
                                 :key #'niwa-reminder-type)))
    (setf (niwa-reminder-last-fired reminder) nil)))

(defun niwa-reminders-stage-plan-mode (fsm)
  "Restore active root Plan guidance after FSM's history is compacted.
Reuse the normal reminder and avoid duplicating an already staged entry."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (when (and (buffer-live-p buffer)
               (not (cl-find 'plan-mode
                             (plist-get info :niwa-reminder-entries)
                             :key (lambda (entry) (plist-get entry :type)))))
      (with-current-buffer buffer
        (when-let* ((session niwa--session)
                    (reminder (niwa-reminders-make-plan-mode))
                    ((funcall (niwa-reminder-trigger reminder) session)))
          (niwa-reminders-stage-entry
           fsm 'plan-mode
           (funcall (niwa-reminder-content reminder) session)))))))

(defun niwa-reminders-make-user-revised-patch (summary)
  "Create the one-shot `user-revised-patch' reminder for SUMMARY.
SUMMARY names the changes the user revised during an ApplyPatch review."
  (niwa-reminder-create
   :type 'user-revised-patch
   :recipe (list 'user-revised-patch summary)
   :trigger (lambda (_session) t)
   :content
   (lambda (_session)
     (format
      "During the last ApplyPatch review the user revised the proposal \
before approving it: %s. Those revisions are deliberate user decisions \
and the applied content is authoritative. Do not revert them or reapply \
your original version of those changes unless the user asks."
      summary))
   :interval 'one-shot))

(defun niwa-reminders--plan-path (session)
  "Return SESSION's immutable accepted plan path, when valid."
  (when-let* ((metadata (niwa-session-plan-metadata session))
              (path (plist-get metadata :accepted-path))
              (address (condition-case nil
                           (niwa-plan-resource-address path)
                         (error nil))))
    (when address path)))

(defun niwa-reminders--plan-reference-content (session)
  "Return bounded contents of SESSION's immutable accepted artifact."
  (when-let* ((path (niwa-reminders--plan-path session)))
    (when (niwa-session-artifacts-artifact-present-p session path)
      (let ((content
             (decode-coding-string
              (niwa-session-artifacts-read-artifact session path)
              'utf-8-unix)))
        (substring content 0 (min 12000 (length content)))))))

(defun niwa-reminders-make-plan-reference ()
  "Create the one-shot `plan-reference' reminder."
  (niwa-reminder-create
   :type 'plan-reference
   :recipe '(plan-reference)
   :trigger (lambda (session)
              (let ((metadata (niwa-session-plan-metadata session)))
                (and metadata
                     (eq (plist-get metadata :status) 'accepted)
                     (let ((accepted-turn
                            (plist-get metadata :accepted-turn)))
                       (or (not (integerp accepted-turn))
                           (> (or (niwa-session-turn-count session) 0)
                              accepted-turn)))
                     (not (niwa-session-plan-mode session))
                     (not
                      (when-let* ((goal (niwa-session-goal session))
                                  ((eq (niwa-goal-status goal) 'active))
                                  (reference
                                   (niwa-goal-plan-reference goal)))
                        (equal reference
                               (niwa-reminders--plan-path session))))
                     (niwa-reminders--plan-reference-content session))))
   :content (lambda (session)
              (when-let* ((path (niwa-reminders--plan-path session))
                          (address (niwa-plan-resource-address path))
                          (content (niwa-reminders--plan-reference-content
                                    session)))
                (format
                 "An accepted plan may be relevant to this turn. Plan artifact: %s\n\n%s\n\nContinue from this plan only if it matches the current user request; otherwise treat it as historical context."
                 address
                 content)))
   :interval 'one-shot))

(defun niwa-reminders-make-fork-provenance ()
  "Create the sparse `fork-provenance' reminder.

Regenerates fork provenance (source session, worktree or shared
working directory, branch, base commit) from durable session slots.
Replaces the permanent fork disclosure text older forks carried: the
provenance is session state, true for the fork's whole life, so it is
re-sent sparsely instead of costing tokens in every request's history."
  (niwa-reminder-create
   :type 'fork-provenance
   :interval 20
   :trigger (lambda (session)
              (and (niwa-session-p session)
                   (niwa-session-forked-from-session-id session)))
   :content #'niwa-session-fork-provenance-body))

(defun niwa-reminders-rearm-plan-reference (session)
  "Re-arm SESSION's one-shot accepted-plan reference after compaction."
  (when-let* ((reminder
               (cl-find 'plan-reference (niwa-session-reminders session)
                        :key #'niwa-reminder-type)))
    (setf (niwa-reminder-last-fired reminder) nil)))

(defun niwa-session-ensure-reminder (session reminder)
  "Add REMINDER to SESSION unless a reminder of the same type exists."
  (unless (memq (niwa-reminder-type reminder)
                (mapcar #'niwa-reminder-type
                        (niwa-session-reminders session)))
    (niwa-session-add-reminder session reminder)))

(defun niwa-reminders-make-pending-events ()
  "Create the `pending-events' reminder.

Fires when runtime subsystems have queued explicit reminder text on
SESSION.  Receipt consumes only the captured events, preserving late arrivals."
  (niwa-reminder-create
   :type 'pending-events
   :recipe '(pending-events)
   :trigger (lambda (session)
              (and (niwa-session-p session)
                   (niwa-session-pending-reminders session)))
   :content (lambda (session)
              (let ((items (niwa-session-pending-reminders session)))
                (list :body (mapconcat #'identity items "\n\n")
                      :commit
                      (lambda ()
                        (dolist (item items)
                          (setf (niwa-session-pending-reminders session)
                                (cl-delete item (niwa-session-pending-reminders session)
                                           :test #'eq :count 1)))))))
   :interval nil))

(defun niwa-reminders-make-date-change ()
  "Create the `date-change' reminder.

Fires when the local calendar date changes during a session and updates
SESSION's observed date after emitting the reminder."
  (niwa-reminder-create
   :type 'date-change
   :recipe '(date-change)
   :trigger (lambda (session)
              (let ((current (format-time-string "%F"))
                    (previous (niwa-session-last-observed-date session)))
                (and previous (not (equal previous current)))))
   :content (lambda (session)
              (let ((previous (niwa-session-last-observed-date session))
                    (current (format-time-string "%F")))
                (list :body
                      (format "The current date changed during this session. Previous date context: %s. Current date: %s. Use the current date for any relative-date reasoning."
                              previous current)
                      :commit
                      (lambda ()
                        (setf (niwa-session-last-observed-date session)
                              current)))))
   :interval nil))

(defun niwa-reminders-make-compaction-available (&optional threshold)
  "Create the `compaction-available' reminder.

Fires sparsely once automatic compaction is enabled and context usage
has crossed THRESHOLD of usable context.  THRESHOLD defaults to 0.70."
  (let ((threshold (or threshold 0.70)))
    (niwa-reminder-create
     :type 'compaction-available
     :recipe (list 'compaction-available threshold)
     :trigger (lambda (_session)
                (let ((state (niwa-reminders--compact-token-state)))
                  (and state
                       (niwa-reminders--compact-auto-available-p)
                       (>= (or (plist-get state :ratio) 0.0)
                           threshold))))
     :content (lambda (_session)
                (let* ((state (niwa-reminders--compact-token-state))
                       (tokens (or (plist-get state :tokens) 0))
                       (usable (or (plist-get state :usable) 1))
                       (pct (round (* 100 (/ (float tokens)
                                             (max 1 usable))))))
                  (format "Automatic compaction is available for this session and context usage is about %d%% of the usable window. Do not stop prematurely because the thread is long; continue the task and let compaction preserve the necessary context when it runs."
                          pct)))
     :interval 'one-shot)))

(defun niwa-reminders-make-token-usage (&optional threshold interval)
  "Create the `token-usage' context-pressure reminder.

Fires near high context usage.  THRESHOLD defaults to 0.90 of usable
context and INTERVAL defaults to 4 turns."
  (let ((threshold (or threshold 0.90)))
    (niwa-reminder-create
     :type 'token-usage
     :recipe (list 'token-usage threshold interval)
     :trigger (lambda (_session)
                (let ((state (niwa-reminders--compact-token-state)))
                  (and state
                       (>= (or (plist-get state :ratio) 0.0)
                           threshold))))
     :content (lambda (_session)
                (let* ((state (niwa-reminders--compact-token-state))
                       (tokens (or (plist-get state :tokens) 0))
                       (usable (or (plist-get state :usable) 1))
                       (threshold-tokens
                        (or (plist-get state :threshold) 0))
                       (pct (round (* 100 (/ (float tokens)
                                             (max 1 usable))))))
                  (format "Context pressure is high: estimated usage is about %d%% of the usable window (%d tokens; compaction threshold %d). Be concise, avoid reprinting large context, and rely on compaction/re-reading files when needed."
                          pct tokens threshold-tokens)))
     :interval (or interval 4))))

(defun niwa-reminders-make-agent-listing-delta ()
  "Create the `agent-listing-delta' reminder.

Compares the current request-visible Agent roster with SESSION's last
snapshot.  The initial snapshot is silent; later added or removed agent
types are reported once and become the new snapshot."
  (let (delta)
    (niwa-reminder-create
     :type 'agent-listing-delta
     :recipe '(agent-listing-delta)
     :trigger (lambda (session)
                (setq delta nil)
                (let* ((current (niwa-reminders--agent-snapshot))
                       (previous (niwa-session-agent-types-snapshot
                                  session)))
                  (if (eq previous :uninitialized)
                      (progn
                        (setf (niwa-session-agent-types-snapshot session)
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
                ;; Captured here: the roster reader needs the chat buffer
                ;; this transform binds, which is gone by commit time.
                (let ((current (niwa-reminders--agent-snapshot))
                      (reported delta))
                  (list :body
                        (niwa-reminders--format-agent-delta
                         (plist-get reported :added)
                         (plist-get reported :removed))
                        :commit
                        (lambda ()
                          (setf (niwa-session-agent-types-snapshot session)
                                current)
                          (setq delta nil)))))
     :interval nil)))

(defun niwa-reminders-make-max-turns-warning (&optional threshold)
  "Create the max-turns-warning reminder for an agent invocation.

Fires once when the agent's turn count reaches THRESHOLD (a fraction
between 0 and 1, default 0.8) of the agent's `max-turns'.  Does
nothing for agents without a configured max-turns cap."
  (let ((threshold (or threshold 0.8)))
    (niwa-reminder-create
     :type 'max-turns-warning
     :recipe (list 'max-turns-warning threshold)
     :trigger (lambda (inv)
                (when-let* ((agent (niwa-agent-invocation-agent inv))
                            (max-turns (niwa-agent-max-turns agent))
                            (count (niwa-agent-invocation-turn-count inv)))
                  (>= count (floor (* threshold max-turns)))))
     :content (lambda (inv)
                (let* ((agent (niwa-agent-invocation-agent inv))
                       (max-turns (niwa-agent-max-turns agent))
                       (count (niwa-agent-invocation-turn-count inv))
                       (remaining (max 0 (- max-turns count))))
                  (format "You have used %d of %d turns (%d remaining). Wrap up your investigation and return your findings to the caller before you hit the turn limit."
                          count max-turns remaining)))
     :interval 'one-shot)))

(defun niwa-reminders-agent-turn-warnings (invocation count)
  "Return INVOCATION's due max-turns warning batch at sample COUNT.
The batch has :entries and :commits, as `niwa-reminders--collect-from'."
  (niwa-reminders--collect-from
   (seq-filter (lambda (reminder) (eq 'max-turns-warning (niwa-reminder-type reminder)))
               (niwa-agent-invocation-reminders invocation))
   count invocation))

(defun niwa-reminders-agent-turn-limit-context (invocation)
  "Return current sample-limit guidance for INVOCATION, or nil without a cap.
This producer does not consume the ordinary one-shot warning.  Engines can
restore current guidance after replacing model history."
  (when-let* ((limit (niwa-agent-max-turns (niwa-agent-invocation-agent invocation))))
    (let ((count (niwa-agent-invocation-turn-count invocation)))
      (if (>= count limit)
          (format "This is your final turn (%d of %d). Reply now with your findings for the caller; any tools you still call run, then your turn ends."
                  count limit)
        (funcall (niwa-reminder-content (niwa-reminders-make-max-turns-warning)) invocation)))))


;;
;;; Edited-file integration

(defcustom niwa-reminders-edited-file-max-diff-lines 40
  "Maximum number of diff lines reported per externally edited file.

Each firing of the `edited-file' reminder truncates a file's unified
diff to this many lines before the ellipsis marker.  Keeps reminder
payloads bounded when large rewrites or reformats occur."
  :type 'integer
  :group 'niwa)

(defcustom niwa-reminders-edited-file-max-diff-bytes (* 256 1024)
  "Largest content size the `edited-file' reminder will diff.

`niwa-generate-diff' spools both sides to temporary files and forks
`diff', so a large file costs that write and that fork on every turn it
changes -- to produce at most
`niwa-reminders-edited-file-max-diff-lines' lines.  Past this size
the change is reported without a diff instead."
  :type 'integer
  :group 'niwa)

(defun niwa-reminders--format-omitted-diff-change (path size)
  "Return the reminder line for a change to PATH reported without a diff.
SIZE is the file's size on disk, when known."
  (format "MODIFIED: %s (%s, no diff available -- re-read the file to \
see the changes)"
          path
          (if (integerp size)
              (file-size-human-readable size)
            "too large to diff")))

(defun niwa-reminders--truncate-diff (diff max-lines)
  "Return DIFF truncated to MAX-LINES lines, appending an ellipsis marker."
  (let ((lines (split-string diff "\n")))
    (if (<= (length lines) max-lines)
        diff
      (concat (mapconcat #'identity (seq-take lines max-lines) "\n")
              (format "\n... (%d more lines truncated)"
                      (- (length lines) max-lines))))))

(defun niwa-reminders--diffable-change-p (change)
  "Return non-nil when edited-file CHANGE is reported with a diff."
  (and (eq 'modified (plist-get change :status))
       (not (plist-get change :content-omitted))
       (<= (length (or (plist-get change :old) ""))
           niwa-reminders-edited-file-max-diff-bytes)
       (<= (length (or (plist-get change :new) ""))
           niwa-reminders-edited-file-max-diff-bytes)))

(defun niwa-reminders-prepare-edited-files (owner continuation)
  "Call CONTINUATION with OWNER once its edited-file diffs are prepared.
OWNER is a gptel state machine, request or agent invocation.

The `edited-file' reminder reports external changes with unified diffs,
and a diff is a helper process.  Waiting for one inside the dispatch
re-entered the event loop from wherever the transition ran, including
gptel's process callbacks, where quitting is inhibited.  The changes are
therefore detected and diffed here first, and CONTINUATION stages the
reminder from them.  A dispatch with nothing to diff continues at once.

Return nil when CONTINUATION has already run.  Otherwise return a
cancellation function: it stops the pending diffs, guarantees
CONTINUATION never runs, and returns non-nil when it stopped a wait."
  (let* ((buffer (plist-get (niwa-engine-info owner) :buffer))
         (session (and (buffer-live-p buffer)
                       (buffer-local-value 'niwa--session buffer)))
         (cache (and session
                     (seq-some (lambda (reminder)
                                 (eq 'edited-file (niwa-reminder-type reminder)))
                               (niwa-session-reminders session))
                     (niwa-session-workspace session)
                     (niwa-workspace-file-cache
                      (niwa-session-workspace session))))
         (changes (and cache (niwa-file-cache-detect-external-changes cache)))
         (pending (seq-filter #'niwa-reminders--diffable-change-p changes))
         diffs cancel done)
    (cl-labels
        ((finish ()
           (setq done t)
           (let ((niwa-reminders--prepared-edits
                  (cons (cons cache (cons changes diffs))
                        niwa-reminders--prepared-edits)))
             (funcall continuation owner)))
         (next ()
           (if (null pending)
               (finish)
             (let ((change (pop pending)))
               (setq cancel
                     (niwa-generate-diff
                      (or (plist-get change :old) "")
                      (or (plist-get change :new) "")
                      (plist-get change :path)
                      (lambda (diff _error)
                        ;; A failed diff reports the change without one.
                        (unless done
                          (when diff (push (cons change diff) diffs))
                          (next)))))))))
      (if (not cache)
          (progn (funcall continuation owner) nil)
        (next)
        (unless done
          (lambda ()
            (unless done
              (setq done t)
              (when (functionp cancel) (funcall cancel))
              t)))))))

(defun niwa-reminders-retain-edited-files (owner continuation)
  "Prepare OWNER's edited-file diffs, then call CONTINUATION with no arguments.
For an engine that composes its prompt after an asynchronous startup, the
prepared diffs stay on OWNER until `niwa-reminders-collect' uses them.
Return the cancellation function of `niwa-reminders-prepare-edited-files'."
  (niwa-reminders-prepare-edited-files
   owner
   (lambda (owner)
     (when niwa-reminders--prepared-edits
       (setf (niwa-engine-info owner)
             (plist-put (niwa-engine-info owner) :niwa-prepared-edits
                        niwa-reminders--prepared-edits)))
     (funcall continuation))))

(defun niwa-reminders--prepared-changes (cache)
  "Return (CHANGES . DIFFS) prepared for CACHE in this dispatch, or nil."
  (alist-get cache niwa-reminders--prepared-edits nil nil #'eq))

(defun niwa-reminders--format-edited-file-change (change max-diff-lines &optional diff)
  "Render CHANGE (plist from detect-external-changes) as a reminder block body.
MAX-DIFF-LINES caps the unified diff size.  DIFF is CHANGE's prepared
unified diff; a modified file without one is reported without a diff."
  (let ((path (plist-get change :path))
        (status (plist-get change :status))
        (new (plist-get change :new)))
    (pcase status
      ('deleted (format "DELETED: %s" path))
      ;; A fingerprint cache entry never held the content, and diffing
      ;; content this large would cost more per turn than the forty
      ;; lines it survives as.  Either way the model is told what moved
      ;; and how to see it.
      ((guard (plist-get change :content-omitted))
       (niwa-reminders--format-omitted-diff-change
        path (plist-get change :stat-size)))
      ((guard (not (niwa-reminders--diffable-change-p change)))
       (niwa-reminders--format-omitted-diff-change
        path (length (or new ""))))
      ((guard (not diff))
       (niwa-reminders--format-omitted-diff-change
        path (length (or new ""))))
      ('modified
       (concat (format "MODIFIED: %s\n" path)
               (niwa-reminders--truncate-diff diff max-diff-lines))))))

(defun niwa-reminders--format-edited-files (changes max-diff-lines &optional diffs)
  "Return edited-file reminder body for the change list.
The argument `CHANGES' supplies the edited files, DIFFS their prepared
diffs as an alist keyed by change.  MAX-DIFF-LINES caps each file's diff
size."
  (concat "Files you previously read or edited have been modified \
outside of your tools since you last saw them. Review the changes \
before making further edits; re-read any file whose diff is truncated \
or unavailable.\n\n"
          (mapconcat (lambda (change)
                       (niwa-reminders--format-edited-file-change
                        change max-diff-lines
                        (alist-get change diffs nil nil #'eq)))
                     changes "\n\n")))

(defun niwa-reminders-make-edited-file (&optional max-diff-lines)
  "Create the `edited-file' reminder.

Fires when any file in SESSION's workspace file cache has been
modified externally (by the user, a formatter, a build system, or any
other agent) since a tool last captured it.  Deletion is reported as
well.  Each firing updates the workspace cache so the same change is
not reported on later turns.

MAX-DIFF-LINES caps the per-file diff size (default
`niwa-reminders-edited-file-max-diff-lines').  Content past
`niwa-reminders-edited-file-max-diff-bytes', and a file the cache
holds as a fingerprint only, are reported without a diff rather than
spooling megabytes through `diff' for forty surviving lines.  The
reminder fires every turn there are changes to report; external edits
are important enough to surface immediately rather than throttle.

niwa's own session bookkeeping never reaches this reminder: the
workspace file cache refuses those paths (see
`niwa-session-file-cache-excluded-p')."
  (let ((max-diff-lines (or max-diff-lines
                            niwa-reminders-edited-file-max-diff-lines))
        ;; Shared between trigger and content so a single firing only
        ;; stats the cache once. Cleared at the start of each trigger
        ;; call and after each content call so turn N does not see
        ;; turn N-1's result.
        (memo nil))
    (niwa-reminder-create
     :type 'edited-file
     :recipe (list 'edited-file max-diff-lines)
     :trigger
     (lambda (session)
       (setq memo nil)
       (when-let* ((ws (niwa-session-workspace session))
                   (cache (niwa-workspace-file-cache ws)))
         (car (or (niwa-reminders--prepared-changes cache)
                  (with-memoization memo
                    (list (niwa-file-cache-detect-external-changes cache)))))))
     :content
     (lambda (session)
       (let* ((ws (niwa-session-workspace session))
              (cache (niwa-workspace-file-cache ws))
              ;; A prepared dispatch's changes carry their diffs, so they
              ;; win over whatever an earlier trigger memoized.
              (prepared (or (niwa-reminders--prepared-changes cache)
                            (with-memoization memo
                              (list (niwa-file-cache-detect-external-changes
                                     cache)))))
              (changes (car prepared)))
         (setq memo nil)
         (list :body (niwa-reminders--format-edited-files
                      changes max-diff-lines (cdr prepared))
               :commit
               (lambda ()
                 (niwa-file-cache-consume-external-changes
                  cache changes)))))
     :interval nil)))


(defun niwa-reminders-make-task-nudge (&optional interval)
  "Create the task-nudge reminder.

Fires when the session has non-completed tasks and task status has not
been written for INTERVAL turns, at most once per INTERVAL turns.
INTERVAL defaults to 8 turns.  Child-owned work is labelled as such."
  (niwa-reminder-create
   :type 'task-nudge
   :recipe (list 'task-nudge interval)
   :trigger (lambda (session)
              (let ((stale-after (or interval 8)))
                (and (niwa-session-p session)
                     (cl-some
                      (lambda (task)
                        (not (eq (niwa-task-status task) 'completed)))
                      (niwa-session-tasks session))
                     (let ((last-write
                            (niwa-session-last-task-write-turn session)))
                       (and (integerp last-write)
                            (>= (- (or (niwa-session-turn-count session) 0)
                                   last-write)
                                stale-after))))))
   :content (lambda (session)
              (format
               "Stale open tasks: update your work with TaskWrite. Child-owned tasks (owner=/root/...) require reconciling their RESULT or following up with the owner before changing status. Successful child completion closes its tasks automatically; failure/interruption leaves them open for recovery.\n\n%s"
               ;; The panel rendering is a display concern; the model
               ;; reads the same shape TaskList returns, with canonical
               ;; owner paths and unabbreviated subjects.
               (niwa-tool-task-format-active-for-llm session)))
   :interval (or interval 8)))

(defun niwa-reminders-make-verification-suggestion ()
  "Suggest verification after recent multi-file implementation work.

At least two implementation files modified in the latest committed turn
or the active turn qualify.  Documentation and isolated edits do not.
Delivery remains throttled to once every ten turns."
  (niwa-reminder-create
   :type 'verification-suggestion
   :recipe '(verification-suggestion)
   :trigger (lambda (session)
              (and (niwa-session-p session)
                   (niwa-session-touched-files session)
                   (let ((count 0)
                         (case-fold-search t))
                     (maphash
                      (lambda (path entry)
                        (when (and (niwa-file-interaction-modified-turn entry)
                                   (>= (niwa-file-interaction-modified-turn entry)
                                       (or (niwa-session-turn-count session) 0))
                                   (not (string-match-p
                                         "/\\(?:docs?\\|node_modules\\|vendor\\|generated\\|build\\|dist\\|target\\|\\.git\\|\\.eask\\|\\.scratch\\|\\.niwa\\)/"
                                         path))
                                   (string-match-p
                                    "\\.\\(?:el\\|py\\|[cm]?js\\|jsx\\|ts\\|tsx\\|c\\|h\\|cc\\|cpp\\|cxx\\|hpp\\|cs\\|java\\|go\\|rs\\|rb\\|sh\\|bash\\|zsh\\|fish\\|ex\\|exs\\|erl\\|hs\\|lua\\|php\\|swift\\|kt\\|kts\\|scala\\|clj\\|cljs\\|vue\\|svelte\\)\\'"
                                    path))
                          (cl-incf count)))
                      (niwa-session-touched-files session))
                     (>= count 2))))
   :content (lambda (session)
              (concat
               "Multi-file implementation changes: consider independent verification before completion."
               (let ((metadata (niwa-session-plan-metadata session)))
                 (when (and (eq (plist-get metadata :status) 'accepted)
                            (plist-get metadata :verification-pending))
                   " Check that the accepted plan was executed, not only that tests pass."))))
   :interval 10))


;;
;;; Session defaults

(defun niwa-reminders-install-defaults (session)
  "Install Tier 1 built-in reminders on SESSION.

Idempotent: a reminder whose type SESSION already carries is not added
twice.  Each constructor is pure and zero-argument, and the constructed
reminder names its own type, so the guard needs no second list."
  (let ((existing (mapcar #'niwa-reminder-type
                          (niwa-session-reminders session))))
    (dolist (make (list #'niwa-reminders-make-pending-events
                        #'niwa-reminders-make-compaction-available
                        #'niwa-reminders-make-token-usage
                        #'niwa-reminders-make-agent-listing-delta
                        #'niwa-reminders-make-mode-constraints
                        #'niwa-reminders-make-plan-mode
                        #'niwa-reminders-make-edited-file
                        #'niwa-reminders-make-task-nudge
                        #'niwa-reminders-make-verification-suggestion
                        #'niwa-reminders-make-plan-reference
                        #'niwa-reminders-make-fork-provenance))
      (let ((reminder (funcall make)))
        (unless (or (memq (niwa-reminder-type reminder) existing)
                    (and (niwa-session-note-conversation session)
                         (eq (niwa-reminder-type reminder) 'edited-file)))
          (niwa-session-add-reminder session reminder)))))
  session)

(provide 'niwa-reminders)

;;; niwa-reminders.el ends here
