;;; mevedel-reminders.el -- System reminders -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'mevedel-structs)

(eval-when-compile
  (require 'gptel-request nil t))

;; `gptel'
(defvar gptel-prompt-transform-functions)

;; `gptel-request'
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel--parse-list "ext:gptel-request"
                  (backend prompt-list))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `imenu'
(declare-function imenu--make-index-alist "imenu" (&optional noerror))
(defvar imenu--index-alist)
(autoload 'imenu--make-index-alist "imenu")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-deferred-expired
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-deferred-set
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-plan-read-only-request-p "mevedel-agents" ())
(declare-function mevedel-agent-invocation-runtime-settled-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-set-deferred-expired
                  "mevedel-agents" (invocation value))
(declare-function mevedel-agent-invocation-turn-count
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-max-turns "mevedel-agents" (agent) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agents-specs "mevedel-agents" (&optional buffer))
(defvar mevedel--agent-invocation)

;; `mevedel-compact'
(declare-function mevedel--compact-auto-eligible-p "mevedel-compact" ())
(defvar mevedel-compact-auto)

;; `mevedel-compact-estimation'
(declare-function mevedel-compact-estimation-estimate-tokens
                  "mevedel-compact-estimation" ())
(declare-function mevedel-compact-estimation-threshold-tokens
                  "mevedel-compact-estimation" (&optional usable-tokens))
(declare-function mevedel-compact-estimation-usable-tokens
                  "mevedel-compact-estimation" ())
(autoload 'mevedel-compact-estimation-estimate-tokens
  "mevedel-compact-estimation")
(autoload 'mevedel-compact-estimation-threshold-tokens
  "mevedel-compact-estimation")
(autoload 'mevedel-compact-estimation-usable-tokens
  "mevedel-compact-estimation")

;; `mevedel-file-state'
(declare-function mevedel-file-cache-consume-external-changes
                  "mevedel-file-state" (cache changes))
(declare-function mevedel-file-cache-detect-external-changes
                  "mevedel-file-state" (cache))

;; `mevedel-hooks'
(declare-function mevedel-hooks-consume-session-context
                  "mevedel-hooks" (session entries))
(declare-function mevedel-hooks-format-context "mevedel-hooks"
                  (entries))

;; `mevedel-plan'
(declare-function mevedel-plan-resource-address "mevedel-plan"
                  (relative-path))
(autoload 'mevedel-plan-resource-address "mevedel-plan")

;; `mevedel-permissions'
(defvar mevedel-permission-mode)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifact-present-p
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(autoload 'mevedel-session-artifacts-artifact-present-p
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-artifact
  "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-current-session
                  "mevedel-telemetry" (&optional buffer))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-task'
(declare-function mevedel-tool-task-format-active-for-llm
                  "mevedel-tool-task" (session))
(autoload 'mevedel-tool-task-format-active-for-llm "mevedel-tool-task")

;; `mevedel-transcript'
(declare-function mevedel-transcript-prompt-transform-start
                  "mevedel-transcript" ())
(autoload 'mevedel-transcript-prompt-transform-start "mevedel-transcript")

;; `mevedel-utilities'
(declare-function mevedel--plain-data-p "mevedel-utilities" (value))
(declare-function mevedel-generate-diff
                  "mevedel-utilities"
                  (original modified filepath &optional labels-real))
(autoload 'mevedel--plain-data-p "mevedel-utilities")
(autoload 'mevedel-generate-diff "mevedel-utilities")

;; `mevedel-workspace'
(declare-function mevedel-workspace-file-buffers "mevedel-workspace"
                  (workspace))
(declare-function mevedel-workspace-root "mevedel-workspace" (workspace) t)

;; `treesit'
(declare-function treesit-available-p "treesit" ())
(declare-function treesit-parser-list "treesit" (&optional buffer language))

;; `xref'
(declare-function xref-find-backend "xref" ())
(defvar tags-file-name)
(defvar tags-table-list)

;; Current prompt-transform context.
(defvar mevedel-reminders--current-chat-buffer nil
  "Chat buffer whose reminders are currently being collected.
Bound dynamically by `mevedel-reminders--transform' so reminder
triggers can distinguish the real chat buffer from gptel's temporary
prompt buffer.")

(defvar-local mevedel-reminders--turn-events nil
  "Owner-bound reminder events for the current model turn.
The value is `(:owner OWNER :items ((KEY :body BODY :commit FUNCTION) ...))'.")

(defvar-local mevedel-reminders--reserved-hook-context nil
  "Hook-context entries this buffer's in-flight request reserved.
Held out of the session's pending context while a request carries them,
and restored by `mevedel-reminders-restore-reserved-context' when that
request never delivers.")


;;
;;; Reminder struct

(cl-defstruct (mevedel-reminder (:constructor mevedel-reminder--create))
  "A single system reminder.

A reminder's TRIGGER is called with a context object (session struct
for main chat, agent-specific context for sub-agents) and returns
non-nil when the reminder should fire on the current turn.  CONTENT is
called with the same context object and returns either the reminder body
string or a plist with `:body' and an optional `:commit' thunk, which
runs once the payload carrying the body reaches the request.

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
  recipe)

(defvar mevedel-reminders--recipe-schemas
  '((mode-constraints mevedel-reminders-make-mode-constraints interval)
    (plan-mode mevedel-reminders-make-plan-mode no-args)
    (full-auto-mode mevedel-reminders-make-full-auto-mode interval)
    (full-auto-mode-exit mevedel-reminders-make-full-auto-mode-exit no-args)
    (plan-reference mevedel-reminders-make-plan-reference no-args)
    (pending-events mevedel-reminders-make-pending-events no-args)
    (date-change mevedel-reminders-make-date-change no-args)
    (compaction-available
     mevedel-reminders-make-compaction-available ratio)
    (token-usage mevedel-reminders-make-token-usage token-usage)
    (agent-listing-delta
     mevedel-reminders-make-agent-listing-delta no-args)
    (max-turns-warning mevedel-reminders-make-max-turns-warning ratio)
    (edited-file mevedel-reminders-make-edited-file count)
    (xref-available mevedel-reminders-make-xref-available no-args)
    (imenu-available mevedel-reminders-make-imenu-available no-args)
    (treesitter-available
     mevedel-reminders-make-treesitter-available no-args)
    (elisp-introspection-available
     mevedel-reminders-make-elisp-introspection-available no-args)
    (deferred-tools-roster
     mevedel-reminders-make-deferred-tools-roster no-args)
    (deferred-tools-expired
     mevedel-reminders-make-deferred-tools-expired no-args)
    (agent-deferred-tools-roster
     mevedel-reminders-make-agent-deferred-tools-roster no-args)
    (agent-deferred-tools-expired
     mevedel-reminders-make-agent-deferred-tools-expired no-args)
    (verifier-read-only
     mevedel-reminders-make-verifier-read-only no-args)
    (reviewer-read-only mevedel-reminders-make-reviewer-read-only no-args)
    (task-nudge mevedel-reminders-make-task-nudge interval)
    (user-revised-patch mevedel-reminders-make-user-revised-patch text)
    (verification-suggestion
     mevedel-reminders-make-verification-suggestion no-args))
  "Trusted constructor and argument contracts for durable reminder recipes.")

(defun mevedel-reminders--recipe-arguments-p (contract arguments)
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

(defun mevedel-reminders--recipe-p (recipe)
  "Return non-nil when RECIPE names a trusted read-safe constructor."
  (when (and (proper-list-p recipe)
             (symbolp (car recipe))
             (mevedel--plain-data-p recipe))
    (when-let* ((schema (assq (car recipe)
                              mevedel-reminders--recipe-schemas)))
      (mevedel-reminders--recipe-arguments-p
       (nth 2 schema) (cdr recipe)))))

(cl-defun mevedel-reminder-create
    (&key type trigger content interval recipe)
  "Create a reminder with TYPE, TRIGGER, CONTENT, INTERVAL, and RECIPE.

TYPE is a symbol identifying the reminder kind.  TRIGGER and CONTENT
are functions of one argument (the firing context).  INTERVAL controls
firing frequency: nil for every-turn, an integer for throttled firing,
or the symbol `one-shot' for fire-at-most-once.  RECIPE is an optional
read-safe list whose first element names a trusted constructor in
`mevedel-reminders--recipe-schemas'."
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
  (when (and recipe (not (mevedel-reminders--recipe-p recipe)))
    (error "Reminder :recipe must name a trusted reminder constructor, got %S"
           recipe))
  (mevedel-reminder--create
   :type type
   :trigger trigger
   :content content
   :interval interval
   :last-fired nil
   :recipe (copy-tree recipe)))

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
   :last-fired nil
   :recipe (copy-tree (mevedel-reminder-recipe reminder))))

(defun mevedel-reminders-clone-list (reminders)
  "Return a fresh list of cloned REMINDERS.

Each element is copied via `mevedel-reminder-clone' so the returned
list tracks its own LAST-FIRED state independently of REMINDERS."
  (mapcar #'mevedel-reminder-clone reminders))

(defun mevedel-reminders-serialize-agent-templates (reminders)
  "Return durable recipes for frozen agent REMINDERS.

A reminder without a trusted recipe is rejected rather than silently changing
a retained agent's frozen identity after resume."
  (mapcar
   (lambda (reminder)
     (unless (mevedel-reminder-p reminder)
       (error "Invalid frozen agent reminder: %S" reminder))
     (let ((recipe (mevedel-reminder-recipe reminder)))
       (unless (and recipe (mevedel-reminders--recipe-p recipe))
         (error "Agent reminder has no durable recipe: %S"
                (mevedel-reminder-type reminder)))
       (copy-tree recipe)))
   reminders))

(defun mevedel-reminders-restore-agent-templates (recipes)
  "Restore frozen agent reminder RECIPES through trusted constructors.

Executable or opaque sidecar values are rejected before any trusted constructor
is called."
  (unless (proper-list-p recipes)
    (error "Invalid persisted agent reminder recipes"))
  (mapcar
   (lambda (recipe)
     (unless (mevedel-reminders--recipe-p recipe)
       (error "Invalid persisted agent reminder recipe"))
     (let* ((schema (assq (car recipe) mevedel-reminders--recipe-schemas))
            (factory (nth 1 schema)))
       (unless (and factory (fboundp factory))
         (error "Unknown persisted agent reminder recipe: %S"
                (car recipe)))
       (let ((reminder (apply factory (cdr recipe))))
         (unless (and (mevedel-reminder-p reminder)
                      (equal recipe (mevedel-reminder-recipe reminder)))
           (error "Invalid restored agent reminder recipe: %S" recipe))
         reminder)))
   recipes))


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

(defun mevedel-reminders-format-block (content)
  "Wrap CONTENT in a `<system-reminder>' XML block."
  (format "<system-reminder>\n%s\n</system-reminder>" content))

(defun mevedel-reminders--entry-label (type)
  "Return the display label for entry TYPE.
TYPE is a reminder type symbol or a cons turn-event key such as
\(specialist . read)."
  (cond
   ((consp type) (format "%s:%s" (car type) (cdr type)))
   ((symbolp type) (symbol-name type))
   (t (format "%s" type))))

(defun mevedel-reminders--collect-from (reminders turn-count ctx)
  "Evaluate REMINDERS at TURN-COUNT and return staged entries and commits.

REMINDERS is a list of `mevedel-reminder' structs.  TURN-COUNT is the
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
      (when (mevedel-reminders--should-fire-p reminder turn-count ctx)
        (let* ((result (funcall (mevedel-reminder-content reminder) ctx))
               (body (if (stringp result) result (plist-get result :body)))
               (commit (and (not (stringp result))
                            (plist-get result :commit))))
          (push (list :type (mevedel-reminder-type reminder) :body body)
                entries)
          (when commit (push commit commits))
          (push (lambda ()
                  (setf (mevedel-reminder-last-fired reminder) turn-count))
                commits))))
    (list :entries (nreverse entries)
          :commits (nreverse commits))))

(defun mevedel-reminders--current-buffer ()
  "Return the chat buffer currently collecting reminders, or nil."
  (and (buffer-live-p mevedel-reminders--current-chat-buffer)
       mevedel-reminders--current-chat-buffer))

(defun mevedel-reminders--compact-token-state ()
  "Return context-pressure state for the current chat buffer.
The returned plist contains `:tokens', `:threshold', `:usable',
and `:ratio', or nil when no chat buffer is collecting reminders."
  (when-let* ((buf (mevedel-reminders--current-buffer)))
    (with-current-buffer buf
      (let* ((tokens (mevedel-compact-estimation-estimate-tokens))
             (threshold (mevedel-compact-estimation-threshold-tokens))
             (usable (mevedel-compact-estimation-usable-tokens))
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
      (when-let* ((specs (mevedel-agents-specs)))
        (sort
         (mapcar (lambda (entry)
                   (cons (car entry)
                         (plist-get (cdr entry) :description)))
                 specs)
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
;;; Prompt delivery

(defun mevedel-reminders-turn-owner (&optional buffer)
  "Return the active model-turn owner for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (and (boundp 'mevedel--agent-invocation)
             (mevedel-agent-invocation-p mevedel--agent-invocation)
             (not (mevedel-agent-invocation-runtime-settled-p
                   mevedel--agent-invocation))
             mevedel--agent-invocation)
        (and (boundp 'mevedel--current-request)
             mevedel--current-request))))

(defun mevedel-reminders-queue-turn-event (buffer key body &optional commit)
  "Queue BODY under KEY for BUFFER's current model turn.
Replacing an existing KEY coalesces repeated observations.  Run COMMIT after
the event reaches the request payload.  Return non-nil when BUFFER has a live
request or agent invocation that owns the event."
  (when (and (buffer-live-p buffer)
             (stringp body)
             (not (string-empty-p body)))
    (with-current-buffer buffer
      (when-let* ((owner (mevedel-reminders-turn-owner buffer)))
        (unless (eq owner (plist-get mevedel-reminders--turn-events :owner))
          (setq mevedel-reminders--turn-events
                (list :owner owner :items nil)))
        (let ((items (plist-get mevedel-reminders--turn-events :items)))
          (setq items (append (assoc-delete-all key items)
                              (list (list key :body body :commit commit))))
          (setq mevedel-reminders--turn-events
                (plist-put mevedel-reminders--turn-events :items items)))
        t))))

(defun mevedel-reminders-restore-reserved-context (buffer)
  "Return BUFFER's reserved hook context to its session's pending list.
Called from the boundaries that settle a turn which never delivered, so
context reserved for it is offered to the next request instead of lost."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((contexts mevedel-reminders--reserved-hook-context)
                  (session (bound-and-true-p mevedel--session)))
        (setq-local mevedel-reminders--reserved-hook-context nil)
        (setf (mevedel-session-hook-context-pending session)
              (append contexts
                      (mevedel-session-hook-context-pending session)))
        t))))

(defun mevedel-reminders--stage-turn-events (buffer)
  "Return live turn-event entries and commits queued for BUFFER.
The queue is not cleared here: dequeueing is the last of the returned
`:commits', so events survive a request that never reaches injection."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((owner (mevedel-reminders-turn-owner buffer)))
        (if (not (eq owner
                     (plist-get mevedel-reminders--turn-events :owner)))
            (progn
              (setq mevedel-reminders--turn-events nil)
              nil)
          (let ((items (plist-get mevedel-reminders--turn-events :items)))
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
              ;; Dequeue last, so a payload that never reaches the
              ;; request leaves these events queued for the next turn.
              (list
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq mevedel-reminders--turn-events nil)))))))))))))

(defun mevedel-reminders--handle-inject (fsm)
  "Inject staged and same-turn reminders into FSM's request payload.

Consumable state is committed only once the payload exists, so a request
that fails to realize or is cancelled before this point keeps everything
for the next turn."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (data (plist-get info :data))
         (initial (plist-get info :mevedel-reminder-entries))
         (staged-commits (plist-get info :mevedel-reminder-commits)))
    (when (and data (buffer-live-p buffer))
      (let* ((events (mevedel-reminders--stage-turn-events buffer))
             (entries (append initial (plist-get events :entries))))
        (when entries
          (let* ((backend (plist-get info :backend))
                 (message
                  (car (gptel--parse-list
                        backend
                        (list (cons 'prompt
                                    (mapconcat
                                     (lambda (entry)
                                       (mevedel-reminders-format-block
                                        (plist-get entry :body)))
                                     entries "\n")))))))
            (gptel--inject-prompt
             backend data message (and initial -1))
            (dolist (commit (plist-get events :commits))
              (with-demoted-errors "mevedel: reminder commit failed: %S"
                (funcall commit)))))
        ;; Staged commits belong to a payload existing at all: hook
        ;; context reached it as prompt text rather than as a block, so
        ;; gating these on `entries' would leave it pending forever.
        (dolist (commit staged-commits)
          (with-demoted-errors "mevedel: reminder commit failed: %S"
            (funcall commit)))
        (when (or initial staged-commits)
          (setf (gptel-fsm-info fsm)
                (plist-put
                 (plist-put info :mevedel-reminder-entries nil)
                 :mevedel-reminder-commits nil)))))))

(defun mevedel-reminders--transform (fsm)
  "Stage system reminders for separate injection into FSM.

Operates on the current buffer, which is the temporary prompt buffer
passed by `gptel-prompt-transform-functions'.  The session lives on the
chat buffer, which is reached via FSM's info plist's :buffer entry.

FSM is mandatory (not `&optional') so that
`gptel-prompt-transform-functions' dispatch -- which inspects the
function's minimum arity -- passes the FSM argument rather than invoking
the transform with zero arguments.

The prompt text itself is left unchanged except for existing hook context.
Runs after `mevedel--transform-expand-mentions'."
  (when-let* ((chat-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p chat-buffer))
              (session (buffer-local-value 'mevedel--session chat-buffer)))
    (let ((mevedel-reminders--current-chat-buffer chat-buffer))
      (let ((commits nil))
        (when-let* ((contexts (mevedel-session-hook-context-pending session)))
          ;; Reserve rather than defer: these entries are in flight for
          ;; this request, so leaving them pending would let a mid-request
          ;; taker -- automatic compaction's context epoch, or a prompt
          ;; prepared in the composer -- deliver them a second time.  The
          ;; reservation is restored if the request never delivers.
          (mevedel-hooks-consume-session-context session contexts)
          (with-current-buffer chat-buffer
            (setq-local mevedel-reminders--reserved-hook-context
                        (append mevedel-reminders--reserved-hook-context
                                contexts)))
          (push (lambda ()
                  (when (buffer-live-p chat-buffer)
                    (with-current-buffer chat-buffer
                      (setq-local mevedel-reminders--reserved-hook-context
                                  nil))))
                commits)
          (goto-char (mevedel-transcript-prompt-transform-start))
          (let ((start (point)))
            (insert "\n"
                    (mevedel-hooks-format-context contexts)
                    "\n")
            (remove-text-properties
             start (point)
             '(gptel nil response nil invisible nil front-sticky nil))))
        (let* ((staged (mevedel-reminders--collect-from
                        (mevedel-session-reminders session)
                        (mevedel-session-turn-count session)
                        session))
               (info (gptel-fsm-info fsm)))
          ;; Append: transforms at earlier depths (mentions,
          ;; skills-input) may already have staged entries.
          (setf (gptel-fsm-info fsm)
                (plist-put
                 (plist-put info :mevedel-reminder-entries
                            (append
                             (plist-get info :mevedel-reminder-entries)
                             (plist-get staged :entries)))
                 :mevedel-reminder-commits
                 (append (plist-get info :mevedel-reminder-commits)
                         commits (plist-get staged :commits)))))))))

(defun mevedel-reminders-stage-entry (fsm type body &optional commit)
  "Stage one system-reminder entry of TYPE with BODY on FSM's request.

The entry joins the synthetic user-role reminder message that
`mevedel-reminders--handle-inject' injects at the request's next WAIT.
COMMIT, when non-nil, runs once the payload exists.  Callable from any
prompt transform and from WAIT-time handlers that run before injection."
  (when (and (stringp body) (not (string-empty-p body)))
    (let ((info (gptel-fsm-info fsm)))
      (setf (gptel-fsm-info fsm)
            (plist-put
             (plist-put info :mevedel-reminder-entries
                        (append (plist-get info :mevedel-reminder-entries)
                                (list (list :type type :body body))))
             :mevedel-reminder-commits
             (append (plist-get info :mevedel-reminder-commits)
                     (and commit (list commit))))))))


;;
;;; Tier 1 built-in reminders

(defun mevedel-reminders--session-mode (session)
  "Return the effective permission mode for SESSION.
Falls back to the global `mevedel-permission-mode' default."
  (or (mevedel-session-permission-mode session)
      (and (boundp 'mevedel-permission-mode) mevedel-permission-mode)
      'ask))

(defvar mevedel-reminders--mode-constraint-messages
  '((edits . "Permission mode: `edits'. File edits are auto-approved; shell commands still require confirmation. Keep changes minimal, targeted, and correct.")
    (full-auto . "Permission mode: `full-auto'. Most confirmation prompts are skipped. Double-check destructive operations before calling tools; protected paths still prompt."))
  "Alist mapping permission mode symbols to reminder body strings.")

(defun mevedel-reminders-make-mode-constraints (&optional interval)
  "Create the mode-constraints reminder.

Fires when the session's permission mode is not `ask'.  INTERVAL
defaults to 5 turns so the reminder repeats sparsely across long
sessions rather than spamming every turn."
  (mevedel-reminder-create
   :type 'mode-constraints
   :recipe (list 'mode-constraints interval)
   :trigger (lambda (session)
              (not (eq (mevedel-reminders--session-mode session) 'ask)))
   :content (lambda (session)
              (let ((mode (mevedel-reminders--session-mode session)))
                (or (alist-get mode mevedel-reminders--mode-constraint-messages)
                    (format "Permission mode: `%s'." mode))))
   :interval (or interval 5)))

(defun mevedel-reminders-make-plan-mode ()
  "Create the every-turn Plan conversation reminder."
  (mevedel-reminder-create
   :type 'plan-mode
   :recipe '(plan-mode)
   :trigger (lambda (session)
              (or (mevedel-session-plan-mode session)
                  (mevedel-plan-read-only-request-p)))
   :content
   (lambda (_session)
     (concat
      "Plan mode is active. Inspect and discuss the project without editing files. Bash is limited to commands classified as read-only; Eval and other tools retain normal permission policy. Treat an implementation request as a request to produce or revise the plan, not to edit. Explore available evidence before asking questions; ask only about genuine user preferences that repository evidence cannot resolve. Every new <proposed_plan> block replaces the previous proposal completely. Do not ask whether you should proceed with implementation. When the plan is complete, emit exactly one line-oriented block with this structure:\n\n"
      "<proposed_plan>\n"
      "# Concrete Plan Title\n\n"
      "## Summary\n"
      "- State the root cause or goal, intended behavior change, and important non-goals.\n\n"
      "## Key Changes\n"
      "- Group implementation bullets by subsystem or behavior, not by file inventory. Mention files, public APIs, interfaces, or data shape changes only when needed to remove ambiguity.\n\n"
      "## Regression Coverage\n"
      "- List the user-visible flows, edge cases, and failure scenarios that tests must cover.\n\n"
      "## Validation\n"
      "- List exact focused test/build commands to run.\n\n"
      "## Assumptions\n"
      "- Record defaults, compatibility assumptions, and intentionally unchanged behavior.\n"
      "</proposed_plan>\n\n"))))

(defun mevedel-reminders-make-full-auto-mode (&optional interval)
  "Create the `full-auto-mode' reminder with INTERVAL.
Fires immediately after entering full-auto mode, then repeats
sparsely while that mode remains active."
  (mevedel-reminder-create
   :type 'full-auto-mode
   :recipe (list 'full-auto-mode interval)
   :trigger (lambda (session)
              (eq (mevedel-reminders--session-mode session) 'full-auto))
   :content (lambda (_session)
              "Full-auto mode is active. Heuristic Bash and Eval prompts are skipped, but explicit denies and protected-resource authority still apply. Keep destructive tool calls deliberate.")
   :interval (or interval 5)))

(defun mevedel-reminders-make-full-auto-mode-exit ()
  "Create the one-shot `full-auto-mode-exit' reminder."
  (mevedel-reminder-create
   :type 'full-auto-mode-exit
   :recipe '(full-auto-mode-exit)
   :trigger (lambda (session)
              (not (eq (mevedel-reminders--session-mode session) 'full-auto)))
   :content (lambda (_session)
              "Full-auto mode has been turned off. Normal permission checks are active again.")
   :interval 'one-shot))

(defun mevedel-reminders-make-user-revised-patch (summary)
  "Create the one-shot `user-revised-patch' reminder for SUMMARY.
SUMMARY names the changes the user revised during an ApplyPatch review."
  (mevedel-reminder-create
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

(defun mevedel-reminders--plan-path (session)
  "Return SESSION's immutable accepted plan path, when valid."
  (when-let* ((metadata (mevedel-session-plan-metadata session))
              (path (plist-get metadata :accepted-path))
              (address (condition-case nil
                           (mevedel-plan-resource-address path)
                         (error nil))))
    (when address path)))

(defun mevedel-reminders--plan-reference-content (session)
  "Return bounded contents of SESSION's immutable accepted artifact."
  (when-let* ((path (mevedel-reminders--plan-path session)))
    (when (mevedel-session-artifacts-artifact-present-p session path)
      (let ((content
             (decode-coding-string
              (mevedel-session-artifacts-read-artifact session path)
              'utf-8-unix)))
        (substring content 0 (min 12000 (length content)))))))

(defun mevedel-reminders-make-plan-reference ()
  "Create the one-shot `plan-reference' reminder."
  (mevedel-reminder-create
   :type 'plan-reference
   :recipe '(plan-reference)
   :trigger (lambda (session)
              (let ((metadata (mevedel-session-plan-metadata session)))
                (and metadata
                     (eq (plist-get metadata :status) 'accepted)
                     (let ((accepted-turn
                            (plist-get metadata :accepted-turn)))
                       (or (not (integerp accepted-turn))
                           (> (or (mevedel-session-turn-count session) 0)
                              accepted-turn)))
                     (not (mevedel-session-plan-mode session))
                     (mevedel-reminders--plan-reference-content session))))
   :content (lambda (session)
              (when-let* ((path (mevedel-reminders--plan-path session))
                          (address (mevedel-plan-resource-address path))
                          (content (mevedel-reminders--plan-reference-content
                                    session)))
                (format
                 "An accepted plan may be relevant to this turn. Plan artifact: %s\n\n%s\n\nContinue from this plan only if it matches the current user request; otherwise treat it as historical context."
                 address
                 content)))
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
   :recipe '(pending-events)
   :trigger (lambda (session)
              (and (mevedel-session-p session)
                   (mevedel-session-pending-reminders session)))
   :content (lambda (session)
              (let ((items (mevedel-session-pending-reminders session)))
                (list :body (mapconcat #'identity items "\n\n")
                      :commit
                      (lambda ()
                        (setf (mevedel-session-pending-reminders session)
                              nil)))))
   :interval nil))

(defun mevedel-reminders-make-date-change ()
  "Create the `date-change' reminder.

Fires when the local calendar date changes during a session and updates
SESSION's observed date after emitting the reminder."
  (mevedel-reminder-create
   :type 'date-change
   :recipe '(date-change)
   :trigger (lambda (session)
              (let ((current (format-time-string "%F"))
                    (previous (mevedel-session-last-observed-date session)))
                (and previous (not (equal previous current)))))
   :content (lambda (session)
              (let ((previous (mevedel-session-last-observed-date session))
                    (current (format-time-string "%F")))
                (list :body
                      (format "The current date changed during this session. Previous date context: %s. Current date: %s. Use the current date for any relative-date reasoning."
                              previous current)
                      :commit
                      (lambda ()
                        (setf (mevedel-session-last-observed-date session)
                              current)))))
   :interval nil))

(defun mevedel-reminders-make-compaction-available (&optional threshold)
  "Create the `compaction-available' reminder.

Fires sparsely once automatic compaction is enabled and context usage
has crossed THRESHOLD of usable context.  THRESHOLD defaults to 0.70."
  (let ((threshold (or threshold 0.70)))
    (mevedel-reminder-create
     :type 'compaction-available
     :recipe (list 'compaction-available threshold)
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
     :recipe (list 'token-usage threshold interval)
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
     :recipe '(agent-listing-delta)
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
                ;; Captured here: the roster reader needs the chat buffer
                ;; this transform binds, which is gone by commit time.
                (let ((current (mevedel-reminders--agent-snapshot))
                      (reported delta))
                  (list :body
                        (mevedel-reminders--format-agent-delta
                         (plist-get reported :added)
                         (plist-get reported :removed))
                        :commit
                        (lambda ()
                          (setf (mevedel-session-agent-types-snapshot session)
                                current)
                          (setq delta nil)))))
     :interval nil)))

(defun mevedel-reminders-make-max-turns-warning (&optional threshold)
  "Create the max-turns-warning reminder for an agent invocation.

Fires once when the agent's turn count reaches THRESHOLD (a fraction
between 0 and 1, default 0.8) of the agent's `max-turns'.  Does
nothing for agents without a configured max-turns cap."
  (let ((threshold (or threshold 0.8)))
    (mevedel-reminder-create
     :type 'max-turns-warning
     :recipe (list 'max-turns-warning threshold)
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
                (mevedel-generate-diff (or old "") (or new "") path)
                max-diff-lines))))))

(defun mevedel-reminders--format-edited-files (changes max-diff-lines)
  "Return edited-file reminder body for the change list.
The argument `CHANGES' supplies the edited files.
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
     :recipe (list 'edited-file max-diff-lines)
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
         (list :body (mevedel-reminders--format-edited-files
                      changes max-diff-lines)
               :commit
               (lambda ()
                 (mevedel-file-cache-consume-external-changes
                  cache changes)))))
     :interval nil)))


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
  "Return a ToolSearch sentence for SESSION NAMES with QUERY."
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
  (condition-case nil
      (let ((kind (mevedel-reminders--xref-backend-kind
                   (xref-find-backend))))
        (pcase kind
          ((or 'eglot 'lsp 'elisp) t)
          ('etags (mevedel-reminders--tags-table-available-p))
          (_ nil)))
    (error nil)))

(defun mevedel-reminders--imenu-available-in-buffer-p ()
  "Return non-nil when current buffer exposes a non-empty Imenu index."
  (condition-case nil
      (progn
        (imenu--make-index-alist t)
        (cl-some (lambda (item)
                   (and (consp item)
                        (stringp (car item))
                        (not (string-prefix-p "*" (car item)))))
                 imenu--index-alist))
    (error nil)))

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
    (dolist (buf (mevedel-workspace-file-buffers
                  (mevedel-session-workspace session)))
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
   :recipe '(xref-available)
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
   :recipe '(imenu-available)
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
   :recipe '(treesitter-available)
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
   :recipe '(elisp-introspection-available)
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
   :recipe '(deferred-tools-roster)
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
   :recipe '(deferred-tools-expired)
   :trigger (lambda (session)
              (and (mevedel-session-deferred-expired session) t))
   :content (lambda (session)
              (let ((names (mevedel-session-deferred-expired session)))
                (list :body (mevedel-reminders--format-deferred-expired names)
                      :commit
                      (lambda ()
                        (setf (mevedel-session-deferred-expired session)
                              nil)))))
   :interval nil))

(defun mevedel-reminders-make-agent-deferred-tools-roster ()
  "Create the agent-scoped `deferred-tools-roster' reminder.

Mirror of `mevedel-reminders-make-deferred-tools-roster' but reads
from a `mevedel-agent-invocation' context instead of a session.
Added by `mevedel-agent-invocation-create' to any agent whose
resolved `:tools' include deferred entries."
  (mevedel-reminder-create
   :type 'deferred-tools-roster
   :recipe '(agent-deferred-tools-roster)
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
   :recipe '(agent-deferred-tools-expired)
   :trigger (lambda (inv)
              (and (mevedel-agent-invocation-deferred-expired inv) t))
   :content (lambda (inv)
              (let ((names (mevedel-agent-invocation-deferred-expired inv)))
                (list :body (mevedel-reminders--format-deferred-expired names)
                      :commit
                      (lambda ()
                        (mevedel-agent-invocation-set-deferred-expired
                         inv nil)))))
   :interval nil))

(defun mevedel-reminders-make-verifier-read-only ()
  "Create the every-turn critical read-only reminder for the verifier agent.

Reinforces that the verifier CANNOT edit, write, or create files and
that its only deliverable is a report.  Fires every turn so the
model cannot drift into implementation mode between messages."
  (mevedel-reminder-create
   :type 'verifier-read-only
   :recipe '(verifier-read-only)
   :trigger (lambda (_ctx) t)
   :content (lambda (_ctx)
              "CRITICAL: This is a VERIFICATION-ONLY task. You CANNOT edit, \
write, or create files. Your job is to try to BREAK the \
implementation, not confirm it works. Report findings — do not patch \
them. You MUST end with exactly one of: VERDICT: PASS, VERDICT: FAIL, \
or VERDICT: PARTIAL. PARTIAL is only for environmental limitations, \
not unfinished feasible checks.")
   :interval nil))

(defun mevedel-reminders-make-reviewer-read-only ()
  "Create the every-turn critical read-only reminder for the reviewer agent.

Reinforces that the reviewer CANNOT edit, write, or create files and
that its only deliverable is a strict JSON review report.  Fires every
turn so the model cannot drift into implementation mode between
messages."
  (mevedel-reminder-create
   :type 'reviewer-read-only
   :recipe '(reviewer-read-only)
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
   :recipe (list 'task-nudge interval)
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
              (format
               "You have active tasks that have not been updated recently. Review and update task status as you make progress (set to in_progress when starting, completed when done). Use TaskUpdate to keep task status current.\n\n%s"
               ;; The panel rendering is a display concern; the model
               ;; reads the same shape TaskList returns, with canonical
               ;; owner paths and unabbreviated subjects.
               (mevedel-tool-task-format-active-for-llm session)))
   :interval nil))

(defun mevedel-reminders-make-verification-suggestion ()
  "Create the every-turn nudge to consider running the verifier.

Fires after the main session has touched files this turn.  Reminds
the assistant to consider spawning the verifier before declaring
non-trivial work complete."
  (mevedel-reminder-create
   :type 'verification-suggestion
   :recipe '(verification-suggestion)
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
                 (when (and (eq (plist-get metadata :status) 'accepted)
                            (plist-get metadata :verification-pending))
                   " Since you are implementing an accepted plan, verify \
that the plan was actually executed, not merely that tests pass."))
               " Adversarial verification often catches regressions \
that pass local tests."))
   :interval 10))


;;
;;; Session defaults

(defun mevedel-reminders-install-defaults (session)
  "Install Tier 1 built-in reminders on SESSION.

Idempotent: a reminder whose type SESSION already carries is not added
twice.  Each constructor is pure and zero-argument, and the constructed
reminder names its own type, so the guard needs no second list."
  (let ((existing (mapcar #'mevedel-reminder-type
                          (mevedel-session-reminders session))))
    (dolist (make (list #'mevedel-reminders-make-pending-events
                        #'mevedel-reminders-make-date-change
                        #'mevedel-reminders-make-compaction-available
                        #'mevedel-reminders-make-token-usage
                        #'mevedel-reminders-make-agent-listing-delta
                        #'mevedel-reminders-make-xref-available
                        #'mevedel-reminders-make-imenu-available
                        #'mevedel-reminders-make-treesitter-available
                        #'mevedel-reminders-make-elisp-introspection-available
                        #'mevedel-reminders-make-mode-constraints
                        #'mevedel-reminders-make-plan-mode
                        #'mevedel-reminders-make-edited-file
                        #'mevedel-reminders-make-deferred-tools-roster
                        #'mevedel-reminders-make-deferred-tools-expired
                        #'mevedel-reminders-make-task-nudge
                        #'mevedel-reminders-make-verification-suggestion
                        #'mevedel-reminders-make-plan-reference))
      (let ((reminder (funcall make)))
        (unless (memq (mevedel-reminder-type reminder) existing)
          (mevedel-session-add-reminder session reminder)))))
  session)

(provide 'mevedel-reminders)

;;; mevedel-reminders.el ends here
