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
  (require 'cl-lib)
  (require 'mevedel-agents))

(require 'mevedel-structs)

;; `gptel'
(defvar gptel-prompt-transform-functions)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-permissions'
(defvar mevedel-permission-mode)

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

;; `mevedel-agents' (struct accessors + setters come from the
;; eval-when-compile require at the top of this file)
(declare-function mevedel-agent-max-turns "mevedel-agents" (agent) t)

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
              (session (buffer-local-value 'mevedel--session chat-buffer))
              (blocks (mevedel-reminders--collect session)))
    (text-property-search-backward 'gptel nil t)
    (insert "\n" (string-join blocks "\n") "\n")))


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
                            mevedel-reminders-edited-file-max-diff-lines)))
    (mevedel-reminder-create
     :type 'edited-file
     :trigger
     (lambda (session)
       (when-let* ((ws (mevedel-session-workspace session))
                   (cache (mevedel-workspace-file-cache ws)))
         (mevedel-file-cache-detect-external-changes cache)))
     :content
     (lambda (session)
       (let* ((ws (mevedel-session-workspace session))
              (cache (mevedel-workspace-file-cache ws))
              (changes (mevedel-file-cache-detect-external-changes cache)))
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
;;; Deferred tools integration

(defun mevedel-reminders--format-deferred-roster (entries)
  "Format ENTRIES as a roster reminder body listing discoverable tools.
ENTRIES is an alist like `mevedel-session-deferred-set' -- each element
is a cons ((CATEGORY NAME) . SHORT-DESCRIPTION)."
  (concat "The following tools are available via lazy loading but are \
not currently in your toolset. Use `ToolSearch' (query=KEYWORDS, \
load=true) to discover and activate them on demand. Tools you \
activate stay available for a few turns; calling them resets the \
timer.\n\n"
          (mapconcat
           (lambda (entry)
             (format "- %s: %s"
                     (cadr (car entry))
                     (or (cdr entry) "")))
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
longer in your toolset: "
          (mapconcat #'identity names ", ")
          ". Call `ToolSearch' (query=KEYWORDS, load=true) to \
re-activate them if you need them again."))

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
                  (setf (mevedel-agent-invocation-deferred-expired inv) nil))))
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
them.")
   :interval nil))

(defun mevedel-reminders-make-task-nudge (&optional interval)
  "Create the task-nudge reminder.

Fires when the session has non-completed tasks, nudging the LLM to
update task status.  INTERVAL defaults to 8 turns."
  (mevedel-reminder-create
   :type 'task-nudge
   :trigger (lambda (session)
              (and (mevedel-session-p session)
                   (cl-some (lambda (task)
                              (not (eq (mevedel-task-status task) 'completed)))
                            (mevedel-session-tasks session))))
   :content (lambda (_session)
              "You have active tasks. Review and update task status \
as you make progress (set to in_progress when starting, completed \
when done). Use TaskUpdate to keep task status current.")
   :interval (or interval 8)))

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
   :content (lambda (_session)
              "Consider spawning the verifier agent before reporting \
completion on non-trivial implementations. Adversarial verification \
often catches regressions that pass local tests.")
   :interval 10))


;;
;;; Session defaults

(defun mevedel-reminders-install-defaults (session)
  "Install Tier 1 built-in reminders on SESSION.

Currently registers `mode-constraints', `diagnostics', `edited-file',
`deferred-tools-roster', `deferred-tools-expired', `task-nudge', and
`verification-suggestion'.  Idempotent: reminders with the same type
are not added twice."
  (let ((existing (mapcar #'mevedel-reminder-type
                          (mevedel-session-reminders session))))
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
       session (mevedel-reminders-make-verification-suggestion))))
  session)

(provide 'mevedel-reminders)

;;; mevedel-reminders.el ends here
