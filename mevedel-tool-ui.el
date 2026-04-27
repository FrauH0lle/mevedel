;;; mevedel-tool-ui.el -- User interaction tools -*- lexical-binding: t -*-

;;; Commentary:

;; User-facing tool UI: directory access requests, agent delegation,
;; todo list display, and the Ask questionnaire.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel-request)
  (require 'mevedel-tool-registry)
  (require 'mevedel-agents)
  (require 'mevedel-reminders))

;; `mevedel-agent-exec' is required at runtime because
;; `mevedel-tools--task' calls into `mevedel-agent-exec--run'
;; synchronously.
(require 'mevedel-agent-exec)

;; `gptel-agent-tools'
(declare-function mevedel-tool-truthy-p "mevedel-tool-registry" (value))

(declare-function gptel-agent--task "ext:gptel-agent-tools" (main-cb agent-type description prompt))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-table "ext:gptel-request" (cl-x) t)
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest args))
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))

;; `mevedel-agents'
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-invocation-create "mevedel-agents" (agent))
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-turn
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--allocate-agent-buffer
                  "mevedel-agent-exec" (invocation parent-data-buffer))
(declare-function mevedel-agent-exec--save-transcript-buffer
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--insert-injected-prompt
                  "mevedel-agent-exec" (invocation block))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--record-running-transcript
                  "mevedel-session-persistence" (session entry))
(declare-function mevedel-session-persistence--validate-transcript-path
                  "mevedel-session-persistence" (path save-path))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-p "mevedel-structs" (cl-x))

;; `mevedel-view'
(declare-function mevedel-view-open-agent-transcript
                  "mevedel-view" (agent-id))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(defvar mevedel-session-persistence)
(defvar mevedel-session--read-only-mode)

;; `mevedel-reminders'
(declare-function mevedel-reminders--collect-from "mevedel-reminders"
                  (reminders turn-count ctx))

;; `gptel'
(defvar gptel--fsm-last)
(defvar gptel-stream)
(declare-function gptel--update-status "ext:gptel" (msg &optional face))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-tools'
(declare-function mevedel-tools--tool-search "mevedel-tools" (callback query &optional load))
(declare-function mevedel-tools--send-message "mevedel-tools" (args))
(declare-function mevedel-tools--handle-message-inject "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-terminal-mailbox "mevedel-tools" (fsm))
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--deferred-context-for "mevedel-tools" (fsm))
(declare-function mevedel-tools--ctx-push-message "mevedel-tools" (ctx msg))
(declare-function mevedel-tools--ctx-push-background-agent "mevedel-tools" (ctx agent-id))
(declare-function mevedel-tools--ctx-remove-background-agent "mevedel-tools" (ctx agent-id))
(declare-function mevedel-tools--ctx-clear-background-agents "mevedel-tools" (ctx))
(declare-function mevedel-tools--ctx-background-agents "mevedel-tools" (ctx))
(declare-function mevedel-tools--ctx-messages "mevedel-tools" (ctx))
(defvar mevedel-tools--current-fsm)

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-view'
(defvar mevedel-view--input-marker)
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))
(declare-function mevedel-add-project-root "mevedel-workspace" (directory))


;;
;;; Sub-agent dispatch debug

(defcustom mevedel-tools-task-debug nil
  "When non-nil, log sub-agent dispatch handoffs to `*Messages*'.

Diagnostic for the multi-agent foreground-callback path.  Three log
points fire when enabled:

  AGENT-EXEC FINALIZE  -- the streaming callback's terminal `'t' (or
    non-streaming string-as-terminal) branch is about to fire
    `main-cb' with the accumulated partial.  Confirms gptel reached
    end-of-stream and the latch picked up the terminal event.

  TASK-DISPATCH FG     -- the foreground gating wrapper in
    `mevedel-tools--task--dispatch' was invoked.  Shows whether the
    fired latch is set, whether the response looks like an error, and
    the current state of `background-agents' / `messages' on the
    sub-agent's invocation.  If the gate says \"still pending\", this
    is where we'd see the stale entries.

  TASK-DISPATCH FG-FIRE -- the gate decided to fire `main-cb'.  If
    FINALIZE and FG fire but FG-FIRE never does, the gate is
    incorrectly returning to BWAIT-equivalent on the terminal turn.

Set via `setopt mevedel-tools-task-debug t' before reproducing a
multi-agent hang.  The output goes to `*Messages*' (only)."
  :type 'boolean
  :group 'mevedel)


;;
;;; Directory access dedup

(defvar-local mevedel--pending-access-requests nil
  "Alist of (ROOT . (STATUS . WAITERS)) for in-flight access requests.

STATUS is one of `pending', `granted', `denied'.  WAITERS is a list
of callback thunks accumulated while STATUS is `pending'; they fire
once when the first prompt resolves.

Buffer-local per chat buffer to deduplicate access prompts within a
session.  Kept across overlay primitive callbacks via the data
buffer's binding."
  )

(defun mevedel-tools--request-access--collapse (ui-outcome)
  "Collapse a UI-OUTCOME symbol or cons into a cache status.

`approve' / `deny' / `aborted' pass through; `(feedback . TEXT)'
collapses to `deny' since later cache hits cannot replay the
feedback text.  Anything unrecognized collapses to `deny' as the
safest default."
  (pcase ui-outcome
    ('approve 'approve)
    ('aborted 'aborted)
    ((or 'deny `(feedback . ,_)) 'deny)
    (_ 'deny)))

(defun mevedel-tools--request-access (root reason callback &optional buffer)
  "Request access to ROOT with REASON, delivering UI outcome to CALLBACK.

CALLBACK is invoked exactly once with one of `approve', `deny',
`(feedback . TEXT)', or `aborted' — the same outcome vocabulary
`mevedel--prompt-user-for-access' produces.  Callers translate the
outcome into their tool-result string (e.g. preserving feedback in a
denial message, or `\"Error: aborted\"' on cancel).

Concurrent calls for the same ROOT collapse onto one prompt: the
first call shows the overlay, later calls register as waiters; the
prompt's resolution fans out the same outcome to every waiter so the
LLM-visible string for each tool call is consistent.

The cache stores only a collapsed status (`approve' / `deny' /
`aborted') for future hits in the same batch — feedback text from
the original prompt is per-call and not replayed.

BUFFER is the chat buffer for buffer-local state (defaults to
current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((entry (assoc root mevedel--pending-access-requests
                         #'string=))
           (status (and entry (car (cdr entry)))))
      (pcase status
        ((or 'approve 'deny 'aborted) (funcall callback status))
        ('pending
         ;; Append our callback to the waiters list — first prompt's
         ;; resolution fans out the same outcome to all of us.
         (setcdr entry (cons 'pending
                             (append (cdr (cdr entry))
                                     (list callback)))))
        (_
         ;; New request: install the entry and drive the prompt.
         (let ((new-entry (cons root (cons 'pending nil))))
           (push new-entry mevedel--pending-access-requests)
           (let ((chat-buf (current-buffer)))
             (mevedel--prompt-user-for-access
              root reason
              (lambda (ui-outcome)
                (when (buffer-live-p chat-buf)
                  (with-current-buffer chat-buf
                    (let ((entry (assoc root mevedel--pending-access-requests
                                        #'string=))
                          (cached (mevedel-tools--request-access--collapse
                                   ui-outcome)))
                      (when (eq ui-outcome 'approve)
                        (mevedel-add-project-root root))
                      (when entry
                        (let ((waiters (cdr (cdr entry))))
                          (setcdr entry (cons cached nil))
                          ;; Fire the original caller, then any waiters,
                          ;; with the full UI outcome (preserves feedback
                          ;; text and the abort sentinel).
                          (ignore-errors (funcall callback ui-outcome))
                          (dolist (w waiters)
                            (ignore-errors (funcall w ui-outcome)))))))))))))))))


;;
;;; Async prompt overlay primitive

(defvar-local mevedel--prompt-overlays nil
  "List of pending mevedel-user-request overlays in this buffer.
Each carries a `mevedel--callback' overlay property — a one-arg
thunk receiving `approve' / `deny' / (feedback . TEXT) / `aborted'.")

(defvar-local mevedel--prompt-canceller-registered-for nil
  "The `mevedel-request' struct we registered the dismiss canceller
onto, or nil.  Mirrors preview-mode's pattern: only the first overlay
per request pushes a canceller onto the request's cancellers list.")

(defun mevedel--prompt--data-buffer ()
  "Return the data buffer reachable from `current-buffer', else nil.
The current buffer qualifies if it carries a live `mevedel--session';
otherwise its `mevedel--data-buffer' back-pointer (set on view buffers
and derived buffers) resolves to the data buffer."
  (let ((cur (current-buffer)))
    (cond
     ((buffer-local-value 'mevedel--session cur) cur)
     ((let ((db (buffer-local-value 'mevedel--data-buffer cur)))
        (and db (buffer-live-p db)
             (buffer-local-value 'mevedel--session db)
             db))))))

(defun mevedel--prompt--register-canceller ()
  "Push the prompt-dismiss thunk onto the active request's cancellers list.

Idempotent per request: subsequent overlays in the same request do
not push a duplicate.  Also installs `mevedel--prompt-dismiss-all' on
the buffer's `kill-buffer-hook' so killing the chat buffer settles
every pending overlay with `aborted'."
  (when-let* ((data-buf (mevedel--prompt--data-buffer))
              (request (buffer-local-value 'mevedel--current-request data-buf)))
    (when (not (eq request mevedel--prompt-canceller-registered-for))
      (let ((buf (current-buffer)))
        (mevedel-request-push-canceller
         request
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (mevedel--prompt-dismiss-all))))))
      (setq mevedel--prompt-canceller-registered-for request)))
  (add-hook 'kill-buffer-hook #'mevedel--prompt-dismiss-all nil t))

(defun mevedel--prompt--settle (overlay outcome)
  "Settle OVERLAY's callback exactly once with OUTCOME.

`mevedel-settled' overlay property gates this — first call sets it
and proceeds; second call is a no-op (defense against duplicate
keypresses or aborts during user action).  Removes OVERLAY from the
buffer's pending list, deletes the overlay text/region so the user
sees it disappear, and finally fires the stored callback."
  (when (and (overlayp overlay)
             (not (overlay-get overlay 'mevedel-settled)))
    (overlay-put overlay 'mevedel-settled t)
    (let ((cb (overlay-get overlay 'mevedel--callback))
          (buf (overlay-buffer overlay))
          (s (overlay-start overlay))
          (e (overlay-end overlay)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq mevedel--prompt-overlays
                (delq overlay mevedel--prompt-overlays))
          (let ((inhibit-read-only t))
            (delete-overlay overlay)
            (when (and s e (>= e s) (not (eq s e)))
              (ignore-errors (delete-region s e))))))
      (when cb (funcall cb outcome)))))

(defun mevedel--prompt-dismiss-all ()
  "Settle every pending prompt overlay in this buffer with `aborted'.

Drains the buffer's `mevedel--prompt-overlays' list; each overlay's
callback fires with `aborted' through `mevedel--prompt--settle'.

Used as: (a) the canceller thunk pushed onto a request's cancellers
list, (b) the buffer-local `kill-buffer-hook' entry installed when
the first overlay is created.  Both routes settle stranded callbacks
so FSMs parked on a TOOL state can advance out via the tool callback."
  (let ((overlays (copy-sequence mevedel--prompt-overlays)))
    (dolist (ov overlays)
      (mevedel--prompt--settle ov 'aborted))))

(defun mevedel--approve-request ()
  "Approve the prompt overlay at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request))))
    (mevedel--prompt--settle ov 'approve)))

(defun mevedel--deny-request ()
  "Deny the prompt overlay at point.

Settles the overlay's callback with `deny'.  Does NOT call
`mevedel-abort' — deny is a scoped per-tool outcome (the LLM sees
one failed tool call and may try alternatives), not a request-wide
teardown.  Earlier behavior tore down the whole request and is
removed in spec 20."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request))))
    (mevedel--prompt--settle ov 'deny)))

(defun mevedel--feedback-request ()
  "Settle the prompt overlay at point with feedback text."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request))))
    (let ((feedback (read-string "What should be changed? ")))
      (mevedel--prompt--settle ov (cons 'feedback feedback)))))

(defun mevedel--prompt-user-with-overlay
    (title content question help-echo-text callback)
  "Display a confirmation overlay; settle CALLBACK exactly once.

CALLBACK is invoked with one of:
  `approve'             user accepted
  `deny'                user denied (no abort, scoped per-tool)
  (feedback . TEXT)     user provided feedback (treated as denial
                        by callers that map this to a scoped error)
  `aborted'             primitive torn down via the request's
                        cancellers list or the chat-buffer kill hook

Multiple concurrent calls produce multiple independent overlays that
settle in user-chosen order; no `recursive-edit', no nesting, no
queue serialization.  The first overlay per request registers a
dismiss thunk onto the request's cancellers list and a
`kill-buffer-hook' entry; subsequent overlays in the same request
piggyback on those registrations.

TITLE is the heading text (bold + warning).  CONTENT is the body
describing the request.  QUESTION is the bold final question.
HELP-ECHO-TEXT is optional hover text."
  (let* ((target-buf (or (and (boundp 'mevedel--view-buffer)
                              mevedel--view-buffer
                              (buffer-live-p mevedel--view-buffer)
                              mevedel--view-buffer)
                         (current-buffer)))
         (start nil)
         (ov nil))
    (with-current-buffer target-buf
      (save-excursion
        (goto-char (if (and (boundp 'mevedel-view--input-marker)
                            mevedel-view--input-marker)
                       mevedel-view--input-marker
                     (point-max)))
        (setq start (point))
        (let ((inhibit-read-only t))
          (insert
           (concat
            "\n"
            (propertize "\n" 'font-lock-face
                        '(:inherit warning :underline t :extend t))
            (propertize (format "%s\n" title)
                        'font-lock-face '(:inherit bold :inherit warning))
            "\n"
            content
            "\n\n"
            (propertize (format "%s\n\n" question) 'font-lock-face 'bold)))
          (insert (propertize "Keys: " 'font-lock-face 'help-key-binding))
          (insert (propertize "RET" 'font-lock-face 'help-key-binding))
          (insert " approve  ")
          (insert (propertize "q" 'font-lock-face 'help-key-binding))
          (insert " deny  ")
          (insert (propertize "f" 'font-lock-face 'help-key-binding))
          (insert " feedback\n")
          (insert (propertize "\n" 'font-lock-face
                              '(:inherit warning :underline t :extend t)))
          (setq ov (make-overlay start (point) nil t))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority 100)
          (overlay-put ov 'mevedel-user-request t)
          (overlay-put ov 'mevedel--callback callback)
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'help-echo
                       (or help-echo-text
                           (concat title ": "
                                   (propertize "Keys: C-c C-c approve  \
C-c C-k deny  f feedback"
                                               'face 'help-key-binding))))
          (overlay-put ov 'keymap
                       (define-keymap
                         "y"        #'mevedel--approve-request
                         "a"        #'mevedel--approve-request
                         "RET"      #'mevedel--approve-request
                         "<return>" #'mevedel--approve-request
                         "C-c C-c"  #'mevedel--approve-request
                         "n"        #'mevedel--deny-request
                         "d"        #'mevedel--deny-request
                         "q"        #'mevedel--deny-request
                         "C-c C-k"  #'mevedel--deny-request
                         "C-g"      #'mevedel--deny-request
                         "f"        #'mevedel--feedback-request))
          (font-lock-append-text-property
           start (point) 'font-lock-face (gptel-agent--block-bg))
          (push ov mevedel--prompt-overlays)
          (mevedel--prompt--register-canceller)))
      (goto-char start))
    ov))

(defun mevedel--prompt-user-for-access (root reason callback)
  "Display the directory-access prompt; deliver UI outcome to CALLBACK.

CALLBACK receives the bare overlay outcome (`approve' / `deny' /
(feedback . TEXT) / `aborted').  The caller is responsible for
mapping that to its tool-result string and any rule storage.

Used by `mevedel-tools--request-access' to drive the dedup wrapper —
non-grant outcomes (deny, feedback, abort) all collapse to \"not
granted\" at that layer."
  (let ((content (concat
                  "The LLM is requesting access to a directory outside the current workspace.\n\n"
                  (propertize "Directory: "
                              'font-lock-face 'font-lock-escape-face)
                  (propertize (format "%s\n" root)
                              'font-lock-face 'font-lock-constant-face)
                  (propertize "Reason: "
                              'font-lock-face 'font-lock-escape-face)
                  (format "%s" reason))))
    (mevedel--prompt-user-with-overlay
     "Directory Access Request"
     content
     "Grant access to this directory?"
     (concat "Directory access request: "
             (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                         'face 'help-key-binding))
     callback)))

(defun mevedel--clear-pending-access-requests (&rest _)
  "Clear the pending access requests cache.
Should be called after each LLM response completes."
  (setq mevedel--pending-access-requests nil))

(defun mevedel-tools--request-access--format-result (path ui-outcome)
  "Translate UI-OUTCOME into the LLM-facing tool-result string for PATH.

`approve'              → grant string.
`deny'                 → \"Access denied to PATH...\".
`(feedback . TEXT)'    → denial string with the user's feedback.
`aborted'              → \"Error: aborted\" (canceller path).
Anything else collapses to a plain denial string."
  (pcase ui-outcome
    ('approve
     (format "Access granted to %s. You can now read and write files in this directory."
             path))
    ('aborted "Error: aborted")
    (`(feedback . ,text)
     (format "Access denied to %s. Feedback: %s" path text))
    ('deny
     (format "Access denied to %s. You cannot access files in this directory."
             path))
    (_
     (format "Access denied to %s. You cannot access files in this directory."
             path))))

(cl-defun mevedel--tools-request-dir-access (callback directory reason)
  "Request user permission to access a directory.

CALLBACK is the tool's async callback; receives a tool-result string.
DIRECTORY is the path to grant access to; REASON explains why.
Routes the user's choice through `mevedel-tools--request-access'
(dedup) and `mevedel--prompt-user-for-access' (overlay).  Feedback
denials carry the user's text into the LLM-visible result; canceller
teardown produces `\"Error: aborted\"' so a parked sub-agent FSM can
advance out of TOOL."
  (mevedel-tools--validate-params callback mevedel--tools-request-dir-access
    (directory stringp)
    (reason stringp))
  (unless (and (file-readable-p directory) (file-directory-p directory))
    (cl-return-from mevedel--tools-request-dir-access
      (funcall callback
               (format "Error: directory '%s' is not readable" directory))))
  (let ((expanded (expand-file-name directory)))
    (mevedel-tools--request-access
     expanded reason
     (lambda (ui-outcome)
       (funcall callback
                (mevedel-tools--request-access--format-result
                 expanded ui-outcome))))))


;;
;;; Agent tool

(defcustom mevedel-agent-background-timeout 600
  "Maximum seconds to wait in BWAIT before forcibly resuming the FSM.

When a background agent loses its completion callback (crash, lost
connection, host exit), the parent would otherwise park indefinitely
in BWAIT.  After this timeout, the watchdog logs a warning, clears
any stranded `background-agents' entries on the context, and forces
a transition to WAIT so the FSM can finalize on its next turn.

Set to nil to disable the watchdog.  The timeout starts when the FSM
enters BWAIT; a stale timer firing after the FSM has left BWAIT is a
no-op."
  :type '(choice (integer :tag "Timeout in seconds")
                 (const :tag "Disabled" nil))
  :group 'mevedel)

(defvar-local mevedel-tools--agents-fsm nil
  "Alist mapping agents to their FSM.")

(defun mevedel-tools--prune-stale-agents-fsm ()
  "Remove terminal, errored, or abandoned FSMs from `mevedel-tools--agents-fsm'.
Called before registry lookups so recipient resolution doesn't route
to a dead invocation.

An entry is considered stale when either:
- its FSM state is DONE, ERRS, or ABRT (normal termination), or
- its `:context' overlay has been deleted.  The
  `mevedel-agent-invocation' is anchored on that overlay, so an
  overlayless FSM cannot receive SendMessage.  This catches FSMs
  that got stuck in TOOL and were later scrubbed by
  `mevedel-tools--bwait-watchdog-expire'."
  (when mevedel-tools--agents-fsm
    (setq mevedel-tools--agents-fsm
          (cl-remove-if
           (lambda (entry)
             (let* ((fsm (cdr entry))
                    (ov (plist-get (gptel-fsm-info fsm) :context)))
               (or (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT))
                   (not (and (overlayp ov) (overlay-buffer ov))))))
           mevedel-tools--agents-fsm))))

(defvar mevedel-tools--bwait-table-cache nil
  "Alist of (SOURCE-TABLE . INJECTED-TABLE) keyed by `eq'.
The injected table is the result of `copy-tree' + BWAIT mutation, so
a shared cache lets every sub-agent spawn and every chat-buffer
init skip the per-call copy.  Bounded in practice by the small
number of distinct upstream transition tables (typically one); the
cache is not invalidated because gptel's defvar tables don't change
at runtime.  The injected table is never mutated after creation --
gptel only reads the FSM table for transitions -- so sharing is
safe.")

(defun mevedel-tools--bwait-injected-table (source)
  "Return SOURCE with a BWAIT parking state injected.

Inserts a `mevedel-tools--background-agents-pending-p' predicate
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
   ((cdr (assq source mevedel-tools--bwait-table-cache)))
   ;; Miss: inject and cache.
   (t
    (let ((injected (copy-tree source))
          (pred #'mevedel-tools--background-agents-pending-p))
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
      (push (cons source injected) mevedel-tools--bwait-table-cache)
      injected))))

(defun mevedel-tools--agent-invocation-at (fsm)
  "Return the `mevedel-agent-invocation' attached to FSM's task overlay.

Returns nil if FSM is not an agent invocation."
  (when-let* ((info (and fsm (gptel-fsm-info fsm)))
              (ov (plist-get info :context))
              ((overlayp ov)))
    (overlay-get ov 'mevedel-agent-invocation)))

(defun mevedel-tools--handle-wait-inject (fsm)
  "WAIT-state handler: advance turn count and inject agent reminders.

Runs once per WAIT cycle for agent FSMs, before
`gptel-agent--indicate-wait' and `gptel--handle-wait' fire the HTTP
request.  Looks up the `mevedel-agent-invocation' attached to FSM's
task overlay; no-op outside agent dispatch.

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
  (when-let* ((inv (mevedel-tools--agent-invocation-at fsm)))
    (let* ((turn (mevedel-agent-invocation-turn-count inv))
           (blocks (mevedel-reminders--collect-from
                    (mevedel-agent-invocation-reminders inv)
                    turn inv)))
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
          (mevedel-agent-exec--insert-injected-prompt inv joined)
          (when data
            (gptel--inject-prompt
             (plist-get info :backend) data
             (list :role "user"
                   :content joined)
             position)))))
    (cl-incf (mevedel-agent-invocation-turn-count inv))))

(cl-defun mevedel-tools--augment-agent-handlers (handlers &key prepend append)
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

(defun mevedel-tools--background-agents-pending-p (info)
  "Return non-nil if INFO's context has pending background work.

Used as a transition predicate: when the LLM produces no tool calls
but background agents are still running OR undelivered agent results
sit in the mailbox, the FSM parks in BWAIT instead of terminating in
DONE.  Checks the agent invocation on the context overlay first,
falling back to the buffer-local session.

Checking both `background-agents' and `messages' covers the race where
a background agent finishes and drains from `background-agents' before
the parent FSM reaches TYPE -- the result is in the mailbox and must
not be lost."
  (let ((ctx (or (when-let* ((ov (plist-get info :context))
                             ((overlayp ov)))
                   (overlay-get ov 'mevedel-agent-invocation))
                 (when-let* ((buf (plist-get info :buffer))
                             ((buffer-live-p buf)))
                   (buffer-local-value 'mevedel--session buf)))))
    (and ctx (or (mevedel-tools--ctx-background-agents ctx)
                 (mevedel-tools--ctx-messages ctx)))))

(defun mevedel-tools--bwait-watchdog-expire (fsm)
  "Forcibly resume FSM from BWAIT after the background-agent timeout.

A no-op unless FSM is still parked in BWAIT when the timer fires.

On expiry:
  1. Warn about the stranded agent-ids for diagnostics.
  2. For each stranded agent, delete its task overlay (so the view
     buffer is not left with a permanent \"Calling Tools...\" label)
     and drop its entry from `mevedel-tools--agents-fsm' (so
     SendMessage cannot resolve to the dead invocation).
  3. Clear `background-agents' on the parent context so the FSM
     doesn't re-park on the next TYPE transition.
  4. Transition the parent FSM: to WAIT if the mailbox still has
     queued results from whichever children DID complete, so the
     drain fires a final turn that incorporates them; otherwise
     to DONE to finalize naturally without burning an extra HTTP
     round-trip.  (The prior TYPE->BWAIT diversion is what kept us
     from a natural termination; direct DONE completes that path.)"
  (when (eq (gptel-fsm-state fsm) 'BWAIT)
    (let* ((ctx (mevedel-tools--deferred-context-for fsm))
           (stranded (and ctx (mevedel-tools--ctx-background-agents ctx))))
      (warn "mevedel: BWAIT watchdog fired after %ss; stranded agents: %S"
            mevedel-agent-background-timeout stranded)
      (dolist (agent-id stranded)
        (when-let* ((entry (assoc agent-id mevedel-tools--agents-fsm))
                    (child-fsm (cdr entry))
                    (ov (plist-get (gptel-fsm-info child-fsm) :context))
                    ((overlayp ov))
                    ((overlay-buffer ov)))
          (delete-overlay ov))
        (setq mevedel-tools--agents-fsm
              (assoc-delete-all agent-id mevedel-tools--agents-fsm)))
      (when ctx
        (mevedel-tools--ctx-clear-background-agents ctx))
      (if (and ctx (mevedel-tools--ctx-messages ctx))
          (gptel--fsm-transition fsm 'WAIT)
        (gptel--fsm-transition fsm 'DONE)))))

(defun mevedel-tools--handle-bwait (fsm)
  "Handler for the BWAIT (background wait) state.

Parks the FSM without firing a new HTTP request.  The background
agent's completion callback will resume the FSM by transitioning
from BWAIT to WAIT.

If background agents have already delivered results to the mailbox
\(no agents pending but messages queued), transitions to WAIT
immediately so the message-inject handler can drain the mailbox.

When the context overlay was deleted by `gptel-agent--task's callback
\(which fires before the FSM reaches BWAIT), this handler restores it
via `move-overlay' so the user sees that the agent is still alive and
waiting for background children.

Arms a watchdog timer per `mevedel-agent-background-timeout' so a
lost completion callback cannot park the FSM forever."
  (when-let* ((info (gptel-fsm-info fsm)))
    (let ((ctx (or (when-let* ((ov (plist-get info :context))
                               ((overlayp ov)))
                     (overlay-get ov 'mevedel-agent-invocation))
                   (when-let* ((buf (plist-get info :buffer))
                               ((buffer-live-p buf)))
                     (buffer-local-value 'mevedel--session buf)))))
      (if (and ctx
               (not (mevedel-tools--ctx-background-agents ctx))
               (mevedel-tools--ctx-messages ctx))
          ;; No agents pending but messages waiting -- go straight to WAIT.
          (gptel--fsm-transition fsm 'WAIT)
        ;; Background agents still running -- park and wait.
        (when-let* ((ov (plist-get info :context))
                    ((overlayp ov)))
          ;; Restore deleted overlay so the user sees the agent is alive.
          (when (null (overlay-buffer ov))
            (when-let* ((buf (plist-get info :buffer))
                        ((buffer-live-p buf))
                        (pos (or (plist-get info :tracking-marker)
                                 (plist-get info :position))))
              (move-overlay ov pos pos buf)))
          (overlay-put
           ov 'after-string
           (concat (overlay-get ov 'msg)
                   (propertize "Waiting for background agents... "
                               'face 'warning)
                   "\n")))
        (when-let* ((buf (plist-get info :buffer))
                    ((buffer-live-p buf)))
          (with-current-buffer buf
            (gptel--update-status " Waiting for agents..." 'warning)))
        (when (and (integerp mevedel-agent-background-timeout)
                   (> mevedel-agent-background-timeout 0))
          (run-at-time mevedel-agent-background-timeout nil
                       #'mevedel-tools--bwait-watchdog-expire fsm))))))

(defun mevedel-tools--inject-bwait-transition (fsm)
  "Modify FSM's transition table to add the BWAIT parking state.

Inserts a `mevedel-tools--background-agents-pending-p' predicate
before the `(t . DONE)' fallthrough in both the TYPE and TRET states.
When the predicate matches, the FSM parks in BWAIT instead of
terminating.

Also adds BWAIT to the handler alist with
`mevedel-tools--handle-bwait', and registers BWAIT as a valid state in
the transition table (with no outgoing transitions -- the background
agent callback forces a transition to WAIT explicitly).

The transition-table mutation is cached via
`mevedel-tools--bwait-injected-table' so repeat spawns reuse a
shared injected copy instead of paying a fresh `copy-tree' each time."
  (setf (gptel-fsm-table fsm)
        (mevedel-tools--bwait-injected-table (gptel-fsm-table fsm)))
  ;; Add BWAIT handler plus terminal-state mailbox guard so orphaned
  ;; messages are at least logged when the sub-agent ends abnormally.
  (setf (gptel-fsm-handlers fsm)
        (mevedel-tools--augment-agent-handlers
         (gptel-fsm-handlers fsm)
         :append `((BWAIT . (,#'mevedel-tools--handle-bwait))
                   (DONE  . (,#'mevedel-tools--handle-terminal-mailbox))
                   (ERRS  . (,#'mevedel-tools--handle-terminal-mailbox))
                   (ABRT  . (,#'mevedel-tools--handle-terminal-mailbox))))))

;;
;;; Agent-result format / parse helpers

(defun mevedel-tools--xml-attr-escape (s)
  "Escape S for use as a double-quoted XML attribute value."
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    ">" "&gt;"
    (replace-regexp-in-string
     "\"" "&quot;"
     (replace-regexp-in-string
      "&" "&amp;" (or s ""))))))

(defun mevedel-tools--agent-result-format (agent-id agent-type description body)
  "Return a `<agent-result ...>...</agent-result>' block.

`agent-id', `type', and `description' attributes are XML-escaped so
LLM-supplied descriptions containing quote characters cannot break
out of the attribute string.  BODY is inserted verbatim."
  (format
   "<agent-result agent-id=\"%s\" type=\"%s\" description=\"%s\">\n%s\n</agent-result>"
   (mevedel-tools--xml-attr-escape agent-id)
   (mevedel-tools--xml-attr-escape agent-type)
   (mevedel-tools--xml-attr-escape description)
   (or body "")))

(defun mevedel-tools--agent-result-parse-id (text)
  "Return the `agent-id' attribute parsed out of an `<agent-result>` in TEXT.
Returns nil if no agent-result block is found.  Used by the view
buffer scanner to join mailbox-delivered results back to their
transcript entries."
  (when (and (stringp text)
             (string-match
              "<agent-result[^>]*agent-id=\"\\([^\"]+\\)\""
              text))
    (match-string 1 text)))


(defun mevedel-tools--task (main-cb agent-type description prompt
                                    &optional background)
  "Call an agent to do specific compound tasks.

Dispatches to `mevedel-agent-exec--run' and manages the entries in
`mevedel-tools--agents-fsm'.  For agents defined via
`mevedel-define-agent', a fresh `mevedel-agent-invocation' is passed
into the runner so the reminders transform and terminal turn-count
handler are wired into the spawned FSM.

MAIN-CB is the main callback to return a value to the main loop.
AGENT-TYPE is the name of the agent.
DESCRIPTION is a short description of the task.
PROMPT is the detailed prompt instructing the agent on what is required.

When BACKGROUND is non-nil the tool returns immediately: MAIN-CB is
called with a short launch confirmation so the parent FSM unblocks.
The sub-agent keeps running; when it finishes, its result is pushed to
the parent's mailbox via `mevedel-tools--ctx-push-message' and
delivered as an `<agent-message>' on the parent's next WAIT turn.

If the parent FSM has no more tool calls and would normally terminate
but still has background agents running, the FSM parks in the BWAIT
state instead.  Background agent completion resumes the parent from
BWAIT to WAIT, which drains the mailbox and fires a new LLM request.

Foreground callback gating matches the BWAIT predicate on both axes
\(`background-agents' and `messages'): the callback fires exactly once
when neither has pending work, and an idempotency latch protects
against unexpected double-fires.  Error/abort responses from the
sub-agent runner bypass the gate so the parent never hangs on a dead
child.

Rejects unknown AGENT-TYPE up front with a formatted error so the
parent tool call sees the failure immediately, before any background
tracking or FSM is created."
  (let ((agent (mevedel-agent-get agent-type)))
    (if (not agent)
        (funcall main-cb
                 (format "Error: Unknown agent type: %s" agent-type))
      (mevedel-tools--task--dispatch
       main-cb agent-type description prompt background agent))))

(defun mevedel-tools--task--dispatch (main-cb agent-type description prompt
                                               background agent)
  "Internal worker for `mevedel-tools--task'.

AGENT is the resolved `mevedel-agent' struct -- caller has already
verified it is non-nil.  Other arguments match
`mevedel-tools--task'.

Dispatch order:

  Metadata setup (steps 1-11):
    1.  Allocate agent-id and short-id.
    2.  Compute parent-turn = `(1+ session.turn-count)`.
    3.  Store metadata on the invocation.
    4.  Allocate the agent buffer.
    5.  Configure parent-context bindings (done by allocator).
    6.  Shallow-materialize parent session (mid-turn safe).
    7-9.  Compute path with collision avoidance, set
          `buffer-file-name', insert prompt.
    10.  Save initial buffer.
    11.  Add `running' entry to session slot.

  Dispatch ordering (steps 12-15):
    12-13.  Pre-register on `mevedel-tools--agents-fsm' BEFORE
            `gptel-request' to close the abort race.
    14-15.  Dispatch and wrap the callback.

  All step-12+ work runs under `unwind-protect' so a startup
  failure unregisters the registry entry."
  (let* ((agent-id (concat agent-type "--"
                           (md5 (format "%s%s%s%s"
                                        (system-name) (emacs-pid)
                                        (current-time) (random)))))
         (invocation (mevedel-agent-invocation-create agent))
         (parent-ctx (mevedel-tools--current-deferred-context))
         (parent-fsm (and background mevedel-tools--current-fsm))
         (parent-data-buffer (current-buffer))
         (parent-session (and (boundp 'mevedel--session) mevedel--session))
         (this-ctx invocation)
         (fired nil))
    ;; --- Metadata setup ---
    (setf (mevedel-agent-invocation-agent-id invocation) agent-id)
    (setf (mevedel-agent-invocation-description invocation) description)
    (setf (mevedel-agent-invocation-parent-session invocation) parent-session)
    (setf (mevedel-agent-invocation-parent-data-buffer invocation)
          parent-data-buffer)
    (setf (mevedel-agent-invocation-parent-turn invocation)
          (1+ (or (and parent-session
                       (mevedel-session-turn-count parent-session))
                  0)))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'running)
    (setf (mevedel-agent-invocation-background-p invocation)
          (and background t))
    ;; Allocate the agent buffer (best-effort; nil falls back to
    ;; the legacy parent-buffer dispatch path).
    (let ((agent-buffer
           (catch 'mevedel-agent-buffer-setup-failed
             (condition-case err
                 (mevedel-agent-exec--allocate-agent-buffer
                  invocation parent-data-buffer)
               (error
                (message "mevedel: agent-buffer allocation failed: %S" err)
                nil)))))
      ;; A `throw' from the allocator (gptel-mode failure, etc.)
      ;; resolves to a non-buffer sentinel; treat as failure and
      ;; fall back to the legacy prompt-only path.
      (unless (bufferp agent-buffer)
        (setq agent-buffer nil))
      (setf (mevedel-agent-invocation-buffer invocation) agent-buffer)
      ;; Try to set up persistence (shallow materialize + transcript file).
      (mevedel-tools--task--setup-transcript invocation agent-buffer)
      ;; Insert the initial task prompt.  Persist it before the
      ;; first request dispatch so a crash mid-first-response still
      ;; leaves the prompt on disk.  If the initial save fails (full
      ;; disk, perms, read-only mount), drop persistence: clear
      ;; `buffer-file-name', drop the in-memory transcript entry,
      ;; and continue with the agent buffer as ephemeral.
      (when (and agent-buffer (buffer-live-p agent-buffer))
        (with-current-buffer agent-buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bobp) (insert "\n"))
            (insert (format "* Agent Task: %s\n\n%s\n"
                            (or description "")
                            (or prompt "")))))
        (when (mevedel-agent-invocation-transcript-relative-path invocation)
          (let ((saved
                 (mevedel-agent-exec--save-transcript-buffer invocation)))
            (unless saved
              (mevedel-tools--task--abandon-persistence invocation)))))
      ;; --- Build wrapped callbacks ---
      (let* ((wrapped-callback
              (cond
               (background
                ;; Background mode: deliver result to parent's mailbox.
                (lambda (response &rest _rest)
                  (when parent-ctx
                    (condition-case err
                        (mevedel-tools--ctx-push-message
                         parent-ctx
                         (list :from agent-id
                               :body (mevedel-tools--agent-result-format
                                      agent-id agent-type description
                                      (or response "(no response)"))
                               :timestamp (current-time)))
                      (error
                       (message "mevedel-tools--task bg push error: %S" err)))
                    (condition-case err
                        (mevedel-tools--ctx-remove-background-agent
                         parent-ctx agent-id)
                      (error
                       (message "mevedel-tools--task bg remove error: %S" err))))
                  (setq mevedel-tools--agents-fsm
                        (assoc-delete-all agent-id mevedel-tools--agents-fsm))
                  ;; Persist the mailbox addition so an Emacs crash
                  ;; between push and the parent's next WAIT-drain
                  ;; does not lose the agent-result.  Best-effort:
                  ;; the helper is gated on the sidecar already
                  ;; existing on disk (i.e. parent has had at least
                  ;; one DONE), so during the parent's first turn
                  ;; this is a no-op and the mailbox falls back to
                  ;; in-memory until the parent's first DONE
                  ;; autosave.
                  (when (mevedel-session-p parent-ctx)
                    (condition-case err
                        (mevedel-session-persistence--write-sidecar-now
                         parent-ctx parent-data-buffer)
                      (error
                       (message "mevedel-tools--task bg sidecar write error: %S"
                                err))))
                  (when (and parent-fsm
                             (eq (gptel-fsm-state parent-fsm) 'BWAIT))
                    (gptel--fsm-transition parent-fsm 'WAIT))))
               (t
                ;; Foreground mode: fire main-cb once when no pending
                ;; work remains on either axis.  Wrap success
                ;; responses with render-data so the renderer can
                ;; expose the transcript-open affordance.
                (lambda (response &rest rest)
                  (when mevedel-tools-task-debug
                    (let ((bg (and this-ctx
                                   (mevedel-tools--ctx-background-agents
                                    this-ctx)))
                          (msgs (and this-ctx
                                     (mevedel-tools--ctx-messages this-ctx))))
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
                  (unless fired
                    (cond
                     ((and (stringp response)
                           (string-prefix-p "Error:" response))
                      (setq fired t)
                      (apply main-cb response rest))
                     ((and this-ctx
                           (or (mevedel-tools--ctx-background-agents this-ctx)
                               (mevedel-tools--ctx-messages this-ctx)))
                      nil)
                     (t
                      (setq fired t)
                      (apply main-cb
                             (mevedel-tools--task--wrap-foreground-response
                              response invocation)
                             rest))))
                  (setq mevedel-tools--agents-fsm
                        (assoc-delete-all agent-id
                                          mevedel-tools--agents-fsm)))))))
        ;; Register background tracking BEFORE starting the child FSM.
        (when (and background parent-ctx)
          (mevedel-tools--ctx-push-background-agent parent-ctx agent-id))
        (let (agent-fsm success-p)
          (unwind-protect
              (progn
                ;; `--run' registers `(agent-id . fsm)' on the
                ;; parent's `mevedel-tools--agents-fsm' BEFORE
                ;; calling `gptel-request' -- so a racing
                ;; `mevedel-abort' finds the entry while the HTTP
                ;; request is still being set up.  Backstop on
                ;; return: if `--run' returned an FSM but didn't
                ;; register it (legacy path or test stub), put it
                ;; on the registry now.  Idempotent re-set when
                ;; the pre-registration already happened.
                (setq agent-fsm
                      (mevedel-agent-exec--run
                       wrapped-callback agent-type description prompt
                       invocation agent-buffer))
                (when agent-fsm
                  (setf (alist-get agent-id
                                   mevedel-tools--agents-fsm nil nil #'equal)
                        agent-fsm))
                (setq success-p t))
            (unless success-p
              (when (and background parent-ctx)
                (mevedel-tools--ctx-remove-background-agent
                 parent-ctx agent-id))
              (setq mevedel-tools--agents-fsm
                    (assoc-delete-all agent-id
                                      mevedel-tools--agents-fsm))))
          (when (and agent-fsm background)
            (funcall main-cb
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
                             agent-type agent-id agent-type))))))))

(defun mevedel-tools--task--abandon-persistence (invocation)
  "Drop persistence state for INVOCATION after a fatal save failure.

Clears the transcript buffer's `buffer-file-name', removes the
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

(defun mevedel-tools--task--setup-transcript (invocation agent-buffer)
  "Best-effort transcript persistence setup for INVOCATION's AGENT-BUFFER.

Performs allocation steps 6-11: shallow materialization of
the parent session, transcript path computation with collision
avoidance, `set-visited-file-name', and a session-slot `running'
entry.  Any failure is logged and falls through to the
no-persistence branch (the agent buffer remains usable;
`buffer-file-name' stays nil; no sidecar entry is created)."
  (let ((session (mevedel-agent-invocation-parent-session invocation))
        (parent-buf (mevedel-agent-invocation-parent-data-buffer invocation))
        (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (when (and (boundp 'mevedel-session-persistence)
               mevedel-session-persistence
               (mevedel-agent-invocation-p invocation)
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
                  (mevedel-session-persistence--record-running-transcript
                   session (cons agent-id entry))))))
        (error
         (message "mevedel: transcript persistence setup failed: %S" err))))))

(defun mevedel-tools--task--wrap-foreground-response (response invocation)
  "Return RESPONSE wrapped with render-data when transcript metadata exists.

For successful (non-error) string responses, returns
`(:result RESPONSE :render-data (:kind agent-transcript :agent-id ID
:status STATUS))' so `mevedel-tool-ui--render-agent' can expose
the transcript-open affordance.  Returns RESPONSE unchanged when
the invocation has no transcript path or the response is not a
string."
  (let ((rel (and (mevedel-agent-invocation-p invocation)
                  (mevedel-agent-invocation-transcript-relative-path
                   invocation)))
        (status (and (mevedel-agent-invocation-p invocation)
                     (mevedel-agent-invocation-transcript-status
                      invocation)))
        (id (and (mevedel-agent-invocation-p invocation)
                 (mevedel-agent-invocation-agent-id invocation))))
    (cond
     ((not (stringp response)) response)
     ((not (and rel id)) response)
     (t (list :result response
              :render-data (list :kind 'agent-transcript
                                 :agent-id id
                                 :transcript-relative-path rel
                                 :status status))))))


;;
;;; Ask User

(cl-defun mevedel-tools--ask-user (callback questions)
  "Ask user multiple questions with navigation support using overlays.

CALLBACK is the async callback function to call with results.
QUESTIONS is an array of question plists, each with :question and :options keys."
  (mevedel-tools--validate-params callback mevedel-tools--ask-user
    (questions (vectorp . "array")))

  (let* ((questions-list (append questions nil)) ; Convert vector to list
         (answers (make-vector (length questions-list) nil))
         (chat-buffer (or (and (boundp 'mevedel--view-buffer)
                               mevedel--view-buffer
                               (buffer-live-p mevedel--view-buffer)
                               mevedel--view-buffer)
                          (current-buffer)))
         (overlay nil)
         (current-index 0))

    (cl-labels
        ((answer-question
           ()
           "Prompt user to answer current question."
           (let* ((q (nth current-index questions-list))
                  (question-text (plist-get q :question))
                  (options (append (plist-get q :options) nil))
                  (all-choices (append options '("Custom input")))
                  (prev-answer (aref answers current-index))
                  (choice (completing-read
                           (format "[Q%d/%d] %s: "
                                   (1+ current-index)
                                   (length questions-list)
                                   question-text)
                           all-choices
                           nil nil
                           prev-answer))
                  (answer (if (equal choice "Custom input")
                              (read-string (concat question-text " (custom): ")
                                           prev-answer)
                            choice)))
             (aset answers current-index answer)
             (update-overlay current-index)))

         (cycle-forward
           ()
           "Cycle to next question or confirmation screen."
           (interactive)
           (if (eq current-index 'confirm)
               ;; From confirm, go to first question
               (progn
                 (setq current-index 0)
                 (update-overlay current-index))
             ;; From a question
             (if (< current-index (1- (length questions-list)))
                 ;; Go to next question
                 (progn
                   (setq current-index (1+ current-index))
                   (update-overlay current-index))
               ;; At last question, go to confirmation
               (progn
                 (setq current-index 'confirm)
                 (show-confirmation)))))

         (cycle-backward
           ()
           "Cycle to previous question or confirmation screen."
           (interactive)
           (if (eq current-index 'confirm)
               ;; From confirm, go to last question
               (progn
                 (setq current-index (1- (length questions-list)))
                 (update-overlay current-index))
             ;; From a question
             (if (> current-index 0)
                 ;; Go to previous question
                 (progn
                   (setq current-index (1- current-index))
                   (update-overlay current-index))
               ;; At first question, go to confirmation
               (progn
                 (setq current-index 'confirm)
                 (show-confirmation)))))

         (edit-answer
           ()
           "Edit current question's answer."
           (interactive)
           (answer-question))

         (confirm-all
           ()
           "Skip to confirmation screen."
           (interactive)
           (setq current-index 'confirm)
           (show-confirmation))

         (quit-questionnaire
           ()
           "Cancel questionnaire and abort execution."
           (interactive)
           (when overlay
             (let ((inhibit-read-only t))
               (delete-region (overlay-start overlay) (overlay-end overlay))
               (delete-overlay overlay)))
           (mevedel-abort))  ; Abort entire execution

         (update-overlay
           (index)
           "Update overlay to show question at INDEX."
           (let* ((q (nth index questions-list))
                  (question-text (plist-get q :question))
                  (options (append (plist-get q :options) nil))
                  (prev-answer (aref answers index)))

             ;; Delete old overlay if exists
             (when overlay
               (let ((inhibit-read-only t))
                 (delete-region (overlay-start overlay) (overlay-end overlay))
                 (delete-overlay overlay)))

             ;; Create new overlay with keymap
             (with-current-buffer chat-buffer
               (goto-char (if (and (boundp 'mevedel-view--input-marker)
                                   mevedel-view--input-marker)
                              mevedel-view--input-marker
                            (point-max)))
               (let ((start (point))
                     (inhibit-read-only t)
                     (keymap (make-sparse-keymap)))
                 (insert "\n")

                 ;; Header
                 (insert (concat
                          (propertize (format "Question %d/%d"
                                              (1+ index)
                                              (length questions-list))
                                      'font-lock-face 'font-lock-string-face)
                          (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t))))
                 (insert "\n")
                 (insert (propertize question-text 'font-lock-face 'font-lock-escape-face))
                 (insert "\n\n")

                 ;; Options
                 (insert (propertize "Available options:\n" 'font-lock-face 'font-lock-constant-face))
                 (dolist (opt options)
                   (insert (format "  • %s\n" opt)))
                 (insert "  • Custom input\n")
                 (insert "\n")

                 ;; Current answer
                 (when prev-answer
                   (insert (propertize "Current answer: " 'font-lock-face 'warning))
                   (insert (propertize prev-answer 'font-lock-face 'bold))
                   (insert "\n\n"))

                 ;; Instructions
                 (insert (propertize "Keys: " 'font-lock-face 'help-key-binding))
                 (insert (propertize "TAB" 'font-lock-face 'help-key-binding))
                 (insert " cylce  ")
                 (insert (propertize "RET" 'font-lock-face 'help-key-binding))
                 (insert " answer  ")
                 (insert (propertize "q" 'font-lock-face 'help-key-binding))
                 (insert " cancel\n")
                 (insert (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t)))
                 (setq overlay (make-overlay start (point) nil t))
                 (overlay-put overlay 'evaporate t)
                 (overlay-put overlay 'priority 10)
                 (overlay-put overlay 'mouse-face 'highlight)

                 ;; Set up keymap
                 (define-key keymap (kbd "TAB") #'cycle-forward)
                 (define-key keymap (kbd "<tab>") #'cycle-forward)
                 (define-key keymap (kbd "S-TAB") #'cycle-backward)
                 (define-key keymap (kbd "<backtab>") #'cycle-backward)
                 (define-key keymap (kbd "RET") #'edit-answer)
                 (define-key keymap (kbd "<return>") #'edit-answer)
                 (define-key keymap (kbd "C-c C-k") #'quit-questionnaire)
                 (define-key keymap (kbd "q") #'quit-questionnaire)
                 (define-key keymap (kbd "C-g") #'quit-questionnaire)


                 (overlay-put overlay 'keymap keymap)
                 (goto-char start)))))

         (submit-answers
           ()
           "Submit all answers to LLM."
           (interactive)
           (let ((result (with-temp-buffer
                           (insert "User answered the following questions:\n\n")
                           (dotimes (i (length questions-list))
                             (let ((q (nth i questions-list))
                                   (a (aref answers i)))
                               (insert (format "Q%d: %s\n" (1+ i) (plist-get q :question)))
                               (insert (format "A%d: %s\n\n" (1+ i) a))))
                           (buffer-string))))
             (cleanup-and-return result)))

         (edit-specific-question
           ()
           "Edit a specific question by number."
           (interactive)
           (let* ((default-qnum (if (eq current-index 'confirm) 1 (1+ current-index)))
                  (qnum (read-number "Edit question number: "
                                     default-qnum)))
             (when (and (>= qnum 1) (<= qnum (length questions-list)))
               (setq current-index (1- qnum))
               (update-overlay current-index))))

         (show-confirmation
           ()
           "Show all answers in overlay and ask for final confirmation."
           ;; Update overlay with summary
           (when overlay
             (let ((inhibit-read-only t))
               (delete-region (overlay-start overlay) (overlay-end overlay))
               (delete-overlay overlay)))

           (with-current-buffer chat-buffer
             (goto-char (if (and (boundp 'mevedel-view--input-marker)
                                 mevedel-view--input-marker)
                            mevedel-view--input-marker
                          (point-max)))
             (let ((start (point))
                   (inhibit-read-only t)
                   (keymap (make-sparse-keymap)))
               (insert "\n")
               (insert (concat
                        (propertize "Review Your Answers" 'font-lock-face 'font-lock-string-face)
                        (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t))))
               (insert "\n")
               (dotimes (i (length questions-list))
                 (let ((q (nth i questions-list))
                       (a (aref answers i)))
                   (insert (propertize (format "%d. " (1+ i)) 'font-lock-face 'bold))
                   (insert (plist-get q :question))
                   (insert "\n")
                   (insert (propertize "   → " 'font-lock-face 'shadow))
                   (if a
                       (insert (propertize a 'font-lock-face 'success))
                     (insert (propertize "(not answered)" 'font-lock-face 'shadow)))
                   (insert "\n\n")))
               (insert (propertize "Keys: " 'font-lock-face 'help-key-binding))
               (insert (propertize "TAB" 'font-lock-face 'help-key-binding))
               (insert " cycle  ")
               (insert (propertize "RET" 'font-lock-face 'help-key-binding))
               (insert " submit  ")
               (insert (propertize "e" 'font-lock-face 'help-key-binding))
               (insert " edit  ")
               (insert (propertize "q" 'font-lock-face 'help-key-binding))
               (insert " cancel\n")
               (insert (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t)))
               (setq overlay (make-overlay start (point) nil t))
               (overlay-put overlay 'evaporate t)
               (overlay-put overlay 'priority 10)
               (overlay-put overlay 'mouse-face 'highlight)

               ;; Set up confirmation keymap
               (define-key keymap (kbd "TAB") #'cycle-forward)
               (define-key keymap (kbd "<tab>") #'cycle-forward)
               (define-key keymap (kbd "S-TAB") #'cycle-backward)
               (define-key keymap (kbd "<backtab>") #'cycle-backward)
               (define-key keymap (kbd "RET") #'submit-answers)
               (define-key keymap (kbd "<return>") #'submit-answers)
               (define-key keymap (kbd "C-c C-c") #'submit-answers)
               (define-key keymap (kbd "C-c C-e") #'edit-specific-question)
               (define-key keymap (kbd "e") #'edit-specific-question)
               (define-key keymap (kbd "C-c C-k") #'quit-questionnaire)
               (define-key keymap (kbd "q") #'quit-questionnaire)
               (define-key keymap (kbd "C-g") #'quit-questionnaire)

               (overlay-put overlay 'keymap keymap)
               (goto-char start))))

         (cleanup-and-return
           (result)
           "Clean up overlay and return RESULT."
           (when overlay
             (let ((inhibit-read-only t))
               (delete-region (overlay-start overlay) (overlay-end overlay))
               (delete-overlay overlay)))
           (funcall callback result)))

      ;; Start the questionnaire - show first question
      (update-overlay 0))))


;;
;;; Pipeline-compatible handlers

(defun mevedel-tool-ui--ask (callback args)
  "Ask the user questions.
CALLBACK receives the formatted answers.  ARGS is a plist with :questions."
  (let ((questions (plist-get args :questions)))
    (unless questions
      (error "Parameter questions is required"))
    (mevedel-tools--ask-user callback questions)))

(defun mevedel-tool-ui--request-access (callback args)
  "Request access to a directory outside the workspace.
CALLBACK receives the result.  ARGS is a plist with :directory and :reason."
  (let ((directory (plist-get args :directory))
        (reason (plist-get args :reason)))
    (unless (stringp directory)
      (error "Parameter directory is required"))
    (unless (stringp reason)
      (error "Parameter reason is required"))
    (if (not (and (file-readable-p directory) (file-directory-p directory)))
        (funcall callback
                 (format "Error: Directory '%s' is not readable" directory))
      (let ((expanded (expand-file-name directory)))
        (mevedel-tools--request-access
         expanded reason
         (lambda (ui-outcome)
           (funcall callback
                    (mevedel-tools--request-access--format-result
                     expanded ui-outcome))))))))

(defun mevedel-tool-ui--agent (callback args)
  "Launch a specialized agent.
CALLBACK receives the agent result.  ARGS is a plist with :subagent_type,
:description, :prompt, and optional :run_in_background."
  (let ((agent-type (plist-get args :subagent_type))
        (description (plist-get args :description))
        (prompt (plist-get args :prompt))
        (background (mevedel-tool-truthy-p
                     (plist-get args :run_in_background))))
    (unless (stringp agent-type)
      (error "Parameter subagent_type is required"))
    (unless (stringp description)
      (error "Parameter description is required"))
    (unless (stringp prompt)
      (error "Parameter prompt is required"))
    (mevedel-tools--task callback agent-type description prompt background)))

(defun mevedel-tool-ui--tool-search (callback args)
  "Search for and load deferred tools.
CALLBACK receives the search results.  ARGS is a plist with :query
and optional :load."
  (let ((query (plist-get args :query))
        (load (plist-get args :load)))
    (unless (stringp query)
      (error "Parameter query is required"))
    (mevedel-tools--tool-search callback query load)))

(defun mevedel-tool-ui--send-message (args)
  "Dispatch SendMessage to the mevedel-tools implementation.
ARGS is a plist with :to and :message."
  (mevedel-tools--send-message args))


;;
;;; Renderers

(defun mevedel-tool-ui--transcript-affordance-suffix (render-data)
  "Return a propertized header suffix for RENDER-DATA, or empty string.

When RENDER-DATA is a transcript metadata plist that passes path
hygiene against the parent session's `save-path', the suffix is a
clickable text-button that opens the transcript via
`mevedel-view-open-agent-transcript'.  In read-only attach mode
the suffix is annotated with \"live in another Emacs\" for
`running' entries.  When path validation fails, no affordance is
exposed -- the suffix is empty."
  (let ((agent-id (and (consp render-data)
                       (eq (plist-get render-data :kind) 'agent-transcript)
                       (plist-get render-data :agent-id)))
        (rel-path (and (consp render-data)
                       (plist-get render-data :transcript-relative-path)))
        (status (and (consp render-data)
                     (plist-get render-data :status))))
    (cond
     ((not (and agent-id rel-path)) "")
     ((not (let* ((session
                   (and (boundp 'mevedel--session) mevedel--session))
                  (save-path (and session
                                  (mevedel-session-save-path session))))
             (and save-path
                  (mevedel-session-persistence--validate-transcript-path
                   rel-path save-path))))
      "")
     (t
      (let* ((readonly
              (and (boundp 'mevedel-session--read-only-mode)
                   mevedel-session--read-only-mode))
             (annot (cond
                     ((and readonly (eq status 'running))
                      " (live in another Emacs)")
                     (t "")))
             (label (format " [transcript: %s%s]"
                            (or status "running") annot))
             (button-string (copy-sequence label)))
        ;; `make-text-button' on a string operates on the whole
        ;; string when END is nil; properties follow as keyword
        ;; pairs.  Returns the propertized string.
        (make-text-button
         button-string nil
         'face 'link
         'follow-link t
         'help-echo (format "Open transcript for %s" agent-id)
         'action
         (lambda (_btn)
           (mevedel-view-open-agent-transcript agent-id)))
        button-string)))))

(defun mevedel-tool-ui--render-agent (name args result render-data)
  "Rendering plist for the Agent tool.
Header shows the subagent type, its short task description, and --
when RENDER-DATA carries transcript metadata that passes path
hygiene -- a clickable transcript-open button.  Body fontifies in
the data buffer's major mode."
  (when (stringp result)
    (let* ((agent-type (or (plist-get args :subagent_type) "?"))
           (description (or (plist-get args :description) ""))
           (shown (if (string-empty-p description)
                      agent-type
                    (format "%s -- %s" agent-type description)))
           (lines (length (split-string result "\n")))
           (suffix
            (mevedel-tool-ui--transcript-affordance-suffix render-data)))
      (list :header (format "%s: %s (%d lines)%s"
                            (or name "Agent") shown lines suffix)
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :initially-collapsed-p t))))


;;
;;; Register Tools

(defun mevedel-tool-ui--register ()
  "Register user interaction tools."

  (mevedel-define-tool
    :name "Ask"
    :description "Ask the user one or more questions and wait for their responses."
    :prompt-file "tools/ask.md"
    :handler #'mevedel-tool-ui--ask
    :args ((questions array :required
                      "Array of question objects. Each question must have predefined answer options."
                      :items (:type object)))
    :async-p t
    :read-only-p t
    :groups (util))

  (mevedel-define-tool
    :name "RequestAccess"
    :description "Request access to a directory outside the current allowed project roots."
    :prompt-file "tools/requestaccess.md"
    :handler #'mevedel-tool-ui--request-access
    :args ((directory string :required
                      "Absolute or relative path to the directory you need to access.")
           (reason string :required
                   "Clear explanation of why you need access to this directory."))
    :async-p t
    :groups (util)
    :get-path (lambda (args) (plist-get args :directory))
    :read-only-p t)

  (mevedel-define-tool
    :name "Agent"
    :description "Launch a specialized agent to handle complex, multi-step tasks autonomously."
    :prompt-file "tools/agent.md"
    :handler #'mevedel-tool-ui--agent
    :args ((subagent_type string :required
                          "The type of specialized agent to use for this task.")
           (description string :required
                        "A short (3-5 word) description of the task.")
           (prompt string :required
                   "The detailed task for the agent to perform autonomously.")
           (run_in_background boolean :optional
                              "Set to true to run this agent in the background. The tool returns immediately with the agent ID; the agent's result is delivered to your mailbox when it finishes."))
    :async-p t
    :max-result-size 50000
    :groups (util)
    :get-name (lambda (args) (plist-get args :subagent_type))
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-agent)

  (mevedel-define-tool
    :name "ToolSearch"
    :description "Search for and load additional tools that are not currently available."
    :prompt-file "tools/toolsearch.md"
    :handler #'mevedel-tool-ui--tool-search
    :args ((query string :required
                  "Search query: tool name or capability description.")
           (load boolean :optional
                 "If true, load matching tools for immediate use on the next turn."))
    :async-p t
    :read-only-p t
    :groups (util))

  (mevedel-define-tool
    :name "SendMessage"
    :description "Send an asynchronous message to a running agent or back to the main chat."
    :prompt-file "tools/sendmessage.md"
    :handler #'mevedel-tool-ui--send-message
    :args ((to string :required
               "Recipient: exact agent id, \"main\", or \"coordinator\".")
           (message string :required
                    "Message body to deliver."))
    :read-only-p t
    :groups (util)))


;;
;;; Permission prompt

(defun mevedel-permission--prompt-approve-once ()
  "Allow this tool invocation once."
  (interactive)
  (mevedel-permission--prompt-finish 'allow-once))

(defun mevedel-permission--prompt-approve-session ()
  "Allow this tool for the rest of the session."
  (interactive)
  (mevedel-permission--prompt-finish 'allow-session))

(defun mevedel-permission--prompt-approve-always ()
  "Always allow this tool (persisted to disk)."
  (interactive)
  (mevedel-permission--prompt-finish 'always-allow))

(defun mevedel-permission--prompt-deny-once ()
  "Deny this tool invocation once."
  (interactive)
  (mevedel-permission--prompt-finish 'deny-once))

(defun mevedel-permission--prompt-deny-session ()
  "Deny this tool for the rest of the session."
  (interactive)
  (mevedel-permission--prompt-finish 'deny-session))

(defun mevedel-permission--prompt-finish (result)
  "Settle the permission prompt overlay at point with RESULT.

RESULT is one of the 5-button vocabulary symbols (`allow-once' etc.)
or `aborted' from the canceller path.  Routes through
`mevedel--prompt--settle' so the overlay's callback fires exactly
once and the overlay text/region is removed atomically."
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-permission-prompt))))
    (mevedel--prompt--settle ov result)))

(defun mevedel-permission--prompt-async (tool-name path include-always cont)
  "Display the permission prompt overlay; settle CONT exactly once.

Async entry point for the 5-button permission UI.  CONT receives one
of `allow-once' / `allow-session' / `always-allow' / `deny-once' /
`deny-session' / `aborted'.  When INCLUDE-ALWAYS is non-nil, the
\"Always allow\" key is offered (persists the rule to disk).

Multiple concurrent prompts produce multiple overlays; each settles
independently in user-chosen order.  The first overlay per request
registers a dismiss thunk on the request's cancellers list — shared
machinery with `mevedel--prompt-user-with-overlay'.  No
`recursive-edit', no nesting, no queue serialization."
  (let* ((target-buf (or (and (boundp 'mevedel--view-buffer)
                              mevedel--view-buffer
                              (buffer-live-p mevedel--view-buffer)
                              mevedel--view-buffer)
                         (current-buffer)))
         (ov nil)
         (content (concat
                   (propertize "Permission Request\n"
                               'font-lock-face '(:inherit bold :inherit warning))
                   "\n"
                   (propertize "Tool: " 'font-lock-face 'font-lock-escape-face)
                   (propertize (format "%s\n" tool-name)
                               'font-lock-face 'font-lock-constant-face)
                   (when path
                     (concat
                      (propertize "Path: " 'font-lock-face 'font-lock-escape-face)
                      (propertize (format "%s\n" path)
                                  'font-lock-face 'font-lock-string-face)))
                   "\n"))
         start)
    (with-current-buffer target-buf
      (save-excursion
        (goto-char (if (and (boundp 'mevedel-view--input-marker)
                            mevedel-view--input-marker)
                       mevedel-view--input-marker
                     (point-max)))
        (setq start (point))
        (let ((inhibit-read-only t))
          (insert "\n")
          (insert (propertize "\n" 'font-lock-face
                              '(:inherit warning :underline t :extend t)))
          (insert content)
          (insert (propertize "Keys: " 'font-lock-face 'help-key-binding))
          (insert (propertize "a" 'font-lock-face 'help-key-binding))
          (insert " allow-once  ")
          (insert (propertize "s" 'font-lock-face 'help-key-binding))
          (insert " allow-session  ")
          (when include-always
            (insert (propertize "A" 'font-lock-face 'help-key-binding))
            (insert " always-allow  "))
          (insert (propertize "d" 'font-lock-face 'help-key-binding))
          (insert " deny-once  ")
          (insert (propertize "D" 'font-lock-face 'help-key-binding))
          (insert " deny-session\n")
          (insert (propertize "\n" 'font-lock-face
                              '(:inherit warning :underline t :extend t)))
          (setq ov (make-overlay start (point) nil t))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority 100)
          (overlay-put ov 'mevedel-permission-prompt t)
          (overlay-put ov 'mevedel--callback cont)
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map "a"
                                     #'mevedel-permission--prompt-approve-once)
                         (define-key map "s"
                                     #'mevedel-permission--prompt-approve-session)
                         (when include-always
                           (define-key map "A"
                                       #'mevedel-permission--prompt-approve-always))
                         (define-key map "d"
                                     #'mevedel-permission--prompt-deny-once)
                         (define-key map "D"
                                     #'mevedel-permission--prompt-deny-session)
                         (define-key map [?q]
                                     #'mevedel-permission--prompt-deny-once)
                         (define-key map (kbd "C-g")
                                     #'mevedel-permission--prompt-deny-once)
                         map))
          (font-lock-append-text-property
           start (point) 'font-lock-face (gptel-agent--block-bg))
          (push ov mevedel--prompt-overlays)
          (mevedel--prompt--register-canceller)))
      (goto-char start))
    ov))

(provide 'mevedel-tool-ui)

;;; mevedel-tool-ui.el ends here
