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
(require 'mevedel-models)
(require 'mevedel-queue)
(require 'subr-x)

;; `gptel-agent-tools'
(declare-function mevedel-tool-truthy-p "mevedel-tool-registry" (value))

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get "mevedel-queue"
                  (entry key))
(declare-function mevedel-queue--entry-metadata-put "mevedel-queue"
                  (entry key value))

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
(declare-function mevedel-agent-invocation-parent-context
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-fsm
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-turn
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-result-reported-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-model-tier-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict
                  "mevedel-agents" (cl-x) t)

;; `mevedel-models'
(declare-function mevedel-model-tier-selector "mevedel-models" (tier))
(declare-function mevedel-model-normalize-tier "mevedel-models" (value))
(declare-function mevedel-model-agent-tool-description "mevedel-models" ())

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--allocate-agent-buffer
                  "mevedel-agent-exec" (invocation parent-data-buffer))
(declare-function mevedel-agent-exec--save-transcript-buffer
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--insert-injected-prompt
                  "mevedel-agent-exec" (invocation block &optional position))
(declare-function mevedel-agent-exec--record-activity
                  "mevedel-agent-exec"
                  (invocation item &optional reserved))
(declare-function mevedel-agent-exec--handle-update
                  "mevedel-agent-exec" (invocation))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--shallow-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--record-running-transcript
                  "mevedel-session-persistence" (session entry))
(declare-function mevedel-session-persistence--validate-transcript-path
                  "mevedel-session-persistence" (path save-path))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--update-transcript-entry
                  "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-p "mevedel-structs" (cl-x))

;; `mevedel-view'
(declare-function mevedel-view-open-agent-transcript
                  "mevedel-view" (agent-id))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue
                  "mevedel-structs" (cl-x) t)
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
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--interaction-overlays)
(declare-function mevedel-view--interaction-anchor "mevedel-view" ())
(declare-function mevedel-view--interaction-register "mevedel-view"
                  (descriptor))
(declare-function mevedel-view--interaction-unregister "mevedel-view"
                  (id))
(declare-function mevedel-view--interaction-target-buffer "mevedel-view"
                  (&optional data-buffer))
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())
(declare-function mevedel-view--insert-attribution "mevedel-view"
                  (agent-id &optional live-click-p calls))
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)

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
`(feedback . TEXT)', or `aborted' -- the same outcome vocabulary
`mevedel--prompt-user-for-access' produces.  Callers translate the
outcome into their tool-result string (e.g. preserving feedback in a
denial message, or `\"Error: aborted\"' on cancel).

Concurrent calls for the same ROOT collapse onto one prompt: the
first call shows the overlay, later calls register as waiters; the
prompt's resolution fans out the same outcome to every waiter so the
LLM-visible string for each tool call is consistent.

The cache stores only a collapsed status (`approve' / `deny' /
`aborted') for future hits in the same batch -- feedback text from
the original prompt is per-call and not replayed.

BUFFER is the chat buffer for buffer-local state (defaults to
current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((allowed-root
            (and (fboundp 'mevedel-workspace--file-in-allowed-roots-p)
                 (condition-case nil
                     (mevedel-workspace--file-in-allowed-roots-p
                      root (current-buffer))
                   (error nil))))
           (entry (assoc root mevedel--pending-access-requests
                         #'string=))
           (status (and entry (car (cdr entry)))))
      (pcase (if allowed-root 'approve status)
        ('approve (funcall callback 'approve))
        ((or 'deny 'aborted) (funcall callback status))
        ('pending
         ;; Append our callback to the waiters list -- first prompt's
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
Each carries a `mevedel--callback' overlay property -- a one-arg
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

`mevedel-settled' overlay property gates this -- first call sets it
and proceeds; second call is a no-op (defense against duplicate
keypresses or aborts during user action).  Removes OVERLAY from the
buffer's pending list, deletes the overlay text/region so the user
sees it disappear, and finally fires the stored callback."
  (when (and (overlayp overlay)
             (not (overlay-get overlay 'mevedel-settled)))
    (overlay-put overlay 'mevedel-settled t)
    (let ((cb (overlay-get overlay 'mevedel--callback))
          (interaction-id (overlay-get overlay 'mevedel-view-interaction-id))
          (buf (overlay-buffer overlay))
          (s (overlay-start overlay))
          (e (overlay-end overlay)))
      (unless (buffer-live-p buf)
        (setq cb nil)
        (display-warning
         'mevedel
         "Stale interaction prompt activation ignored"
         :warning))
      (when (and interaction-id
                 (boundp 'mevedel-view--interaction-overlays)
                 (hash-table-p mevedel-view--interaction-overlays)
                 (not (eq overlay
                          (gethash interaction-id
                                   mevedel-view--interaction-overlays))))
        (setq cb nil)
        (display-warning
         'mevedel
         "Stale interaction prompt activation ignored"
         :warning))
      (when (buffer-live-p buf)
        (with-current-buffer buf
	  (setq mevedel--prompt-overlays
	        (delq overlay mevedel--prompt-overlays))
          (when (and interaction-id
                     (fboundp 'mevedel-view--interaction-unregister))
            (mevedel-view--interaction-unregister interaction-id))
          (let ((inhibit-read-only t))
            (delete-overlay overlay)
            (when (and (not interaction-id)
                       s e (>= e s) (not (eq s e)))
              (ignore-errors (delete-region s e))))))
      (when cb (funcall cb outcome)))))

(defun mevedel--prompt--overlay-at-point (property)
  "Return prompt overlay at point carrying PROPERTY.
Falls back to the `mevedel-view-interaction-overlay' text property
set on materialized interaction-zone descriptor text."
  (or (cdr (get-char-property-and-overlay (point) property))
      (let ((ov (get-text-property (point) 'mevedel-view-interaction-overlay)))
        (and (overlayp ov) (overlay-get ov property) ov))))

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
  (when-let* ((ov (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (mevedel--prompt--settle ov 'approve)))

(defun mevedel--deny-request ()
  "Deny the prompt overlay at point.

Settles the overlay's callback with `deny'.  Does NOT call
`mevedel-abort' -- deny is a scoped per-tool outcome (the LLM sees
one failed tool call and may try alternatives), not a request-wide
teardown.  Earlier behavior tore down the whole request and is
removed in spec 20."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (mevedel--prompt--settle ov 'deny)))

(defun mevedel--feedback-request ()
  "Settle the prompt overlay at point with feedback text."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (let ((feedback (read-string "What should be changed? ")))
      (mevedel--prompt--settle ov (cons 'feedback feedback)))))

(defun mevedel--prompt-framed-body (content face)
  "Return CONTENT inside the standard interaction prompt frame.
FACE is the face inherited by the top and bottom rule lines.  The
same background face used by permission prompts is applied to the
whole body so domain-specific prompts share one visual container."
  (let ((body
         (concat
          "\n"
          (propertize "\n" 'font-lock-face
                      `(:inherit ,face :underline t :extend t))
          content
          (propertize "\n" 'font-lock-face
                      `(:inherit ,face :underline t :extend t)))))
    (font-lock-append-text-property
     0 (length body) 'font-lock-face (gptel-agent--block-bg) body)
    body))

(defun mevedel--prompt-key (key)
  "Return propertized KEY for prompt key-help rows."
  (propertize key 'font-lock-face 'help-key-binding))

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
  (let* ((target-buf
	          (if (fboundp 'mevedel-view--interaction-target-buffer)
	              (mevedel-view--interaction-target-buffer
	               (mevedel--prompt--data-buffer))
	            (error "No live view for queued prompt")))
         (id (list :request (gensym "request-")))
         (body
          (mevedel--prompt-framed-body
           (concat
            (propertize (format "%s\n" title)
                        'font-lock-face '(:inherit bold :inherit warning))
            "\n"
            content
            "\n\n"
            (propertize (format "%s\n\n" question) 'font-lock-face 'bold)
            (propertize "Keys: " 'font-lock-face 'help-key-binding)
            (mevedel--prompt-key "a")
            " approve  "
            (mevedel--prompt-key "d")
            " deny  "
            (mevedel--prompt-key "f")
            " feedback\n")
           'warning))
         (keymap
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
         ov)
    (with-current-buffer target-buf
      (setq ov
            (mevedel-view--interaction-register
             (list :kind 'request
                   :id id
                   :count 0
                   :body body
                   :priority 150
                   :keymap keymap
                   :help-echo
                   (or help-echo-text
                       (concat title ": "
                               (propertize "Keys: C-c C-c approve  \
C-c C-k deny  f feedback"
                                           'face 'help-key-binding)))
                   :activate callback)))
      (overlay-put ov 'mevedel-user-request t)
      (overlay-put ov 'mevedel--callback callback)
      (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
      (mevedel--prompt--register-canceller))
    ov))

(defun mevedel--prompt-user-for-access (root reason callback)
  "Display the directory-access prompt; deliver UI outcome to CALLBACK.

CALLBACK receives the bare overlay outcome (`approve' / `deny' /
(feedback . TEXT) / `aborted').  The caller is responsible for
mapping that to its tool-result string and any rule storage.

Used by `mevedel-tools--request-access' to drive the dedup wrapper --
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

`approve'              -> grant string.
`deny'                 -> \"Access denied to PATH...\".
`(feedback . TEXT)'    -> denial string with the user's feedback.
`aborted'              -> \"Error: aborted\" (canceller path).
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
- the FSM no longer carries a live `mevedel-agent-invocation'."
  (when mevedel-tools--agents-fsm
    (setq mevedel-tools--agents-fsm
          (cl-remove-if
           (lambda (entry)
             (let* ((fsm (cdr entry)))
               (or (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT))
                   (not (mevedel-tools--agent-invocation-at fsm)))))
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
  "Return the `mevedel-agent-invocation' attached to FSM.

Returns nil if FSM is not an agent invocation."
  (when-let* ((info (and fsm (gptel-fsm-info fsm))))
    (or (and (mevedel-agent-invocation-p
              (plist-get info :mevedel-agent-invocation))
             (plist-get info :mevedel-agent-invocation))
        (when-let* ((ov (plist-get info :context))
                    ((overlayp ov)))
          (overlay-get ov 'mevedel-agent-invocation)))))

(defun mevedel-tools--handle-wait-inject (fsm)
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
  (when-let* ((inv (mevedel-tools--agent-invocation-at fsm)))
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
    (and ctx (or (mevedel-tools--ctx-background-agents ctx)
                 (mevedel-tools--ctx-messages ctx)))))

(defun mevedel-tools--bwait-watchdog-expire (fsm)
  "Forcibly resume FSM from BWAIT after the background-agent timeout.

A no-op unless FSM is still parked in BWAIT when the timer fires.

On expiry:
  1. Warn about the stranded agent-ids for diagnostics.
  2. Drop each stranded agent from `mevedel-tools--agents-fsm' so
     SendMessage cannot resolve to the dead invocation.
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

Arms a watchdog timer per `mevedel-agent-background-timeout' so a
lost completion callback cannot park the FSM forever."
  (when-let* ((info (gptel-fsm-info fsm)))
    (let ((ctx (or (and (mevedel-agent-invocation-p
                        (plist-get info :mevedel-agent-invocation))
                       (plist-get info :mevedel-agent-invocation))
                   (when-let* ((buf (plist-get info :buffer))
                               ((buffer-live-p buf)))
                     (buffer-local-value 'mevedel--session buf)))))
      (if (and ctx
               (not (mevedel-tools--ctx-background-agents ctx))
               (mevedel-tools--ctx-messages ctx))
          ;; No agents pending but messages waiting -- go straight to WAIT.
          (gptel--fsm-transition fsm 'WAIT)
        ;; Background agents still running -- park and wait.
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

(defun mevedel-tool-ui--verifier-verdict (response invocation)
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

(defun mevedel-tool-ui--record-verifier-verdict (response invocation)
  "Parse and store verifier verdict from RESPONSE on INVOCATION.

Returns the parsed verdict symbol, or nil."
  (when-let* ((verdict (mevedel-tool-ui--verifier-verdict response invocation)))
    (setf (mevedel-agent-invocation-verdict invocation) verdict)
    (when-let* ((session (mevedel-agent-invocation-parent-session invocation))
                (agent-id (mevedel-agent-invocation-agent-id invocation)))
      (when (fboundp 'mevedel-session-persistence--update-transcript-entry)
        (mevedel-session-persistence--update-transcript-entry
         session agent-id (list :verdict verdict))))
    verdict))

(defun mevedel-tools--complete-background-agent (invocation response)
  "Deliver background INVOCATION's RESPONSE and clear parent tracking.

This is the shared terminal path for normal callbacks and direct FSM
ERRS handling.  It is idempotent after a successful mailbox push so
gptel's current \"callback nil, then transition ERRS\" sequence does
not deliver duplicate `<agent-result>' blocks."
  (when (and (mevedel-agent-invocation-p invocation)
             (mevedel-agent-invocation-background-p invocation))
    (let* ((agent (mevedel-agent-invocation-agent invocation))
           (agent-type (or (and agent (mevedel-agent-name agent)) "agent"))
           (agent-id (mevedel-agent-invocation-agent-id invocation))
           (description (or (mevedel-agent-invocation-description invocation)
                            ""))
           (parent-ctx (mevedel-agent-invocation-parent-context invocation))
           (parent-fsm (mevedel-agent-invocation-parent-fsm invocation))
           (parent-data-buffer
            (mevedel-agent-invocation-parent-data-buffer invocation))
           (verdict (mevedel-tool-ui--record-verifier-verdict
                     response invocation))
           (pushed nil))
      (when (and parent-ctx
                 (not (mevedel-agent-invocation-background-result-reported-p
                       invocation)))
        (condition-case err
            (progn
              (mevedel-tools--ctx-push-message
               parent-ctx
               (list :from agent-id
                     :body (mevedel-tools--agent-result-format
                            agent-id agent-type description
                            (or response "(no response)"))
                     :timestamp (current-time)))
              (setq pushed t))
          (error
           (message "mevedel-tools--task bg push error: %S" err)))
        (when pushed
          (setf (mevedel-agent-invocation-background-result-reported-p
                 invocation)
                t)))
      (when parent-ctx
        (condition-case err
            (mevedel-tools--ctx-remove-background-agent parent-ctx agent-id)
          (error
           (message "mevedel-tools--task bg remove error: %S" err))))
      (when (and parent-data-buffer (buffer-live-p parent-data-buffer))
        (with-current-buffer parent-data-buffer
          (setq mevedel-tools--agents-fsm
                (assoc-delete-all agent-id mevedel-tools--agents-fsm))))
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
           (message "mevedel-tools--task bg sidecar write error: %S"
                    err))))
      (when (and parent-fsm
                 (eq (gptel-fsm-state parent-fsm) 'BWAIT))
        (gptel--fsm-transition parent-fsm 'WAIT)))))


(cl-defun mevedel-tools--task (main-cb agent description prompt
                                       &key background
                                       model-tier
                                       skill-permission-rules
                                       skill-model-override
                                       skill-effort-override
                                       skill-hook-rules)
  "Call AGENT to do specific compound tasks.

AGENT is a resolved `mevedel-agent' struct (registry-defined or
synthetic).  Caller is responsible
for resolution; see `mevedel-tools--task-by-name' for a lookup
wrapper used by the Agent tool and similar string-name callers.

Dispatches to `mevedel-agent-exec--run' and manages the entries
in `mevedel-tools--agents-fsm'.  A fresh `mevedel-agent-invocation'
is passed into the runner so the reminders transform and terminal
turn-count handler are wired into the spawned FSM.

MAIN-CB is the main callback to return a value to the main loop.
DESCRIPTION is a short description of the task.
PROMPT is the detailed prompt instructing the agent on what is required.

Keyword arguments:

- BACKGROUND: when non-nil the tool returns immediately and
  MAIN-CB is called with a short launch confirmation so the
  parent FSM unblocks.  The sub-agent keeps running and pushes
  its eventual result to the parent's mailbox.
- MODEL-TIER: explicit Agent-tool tier selector for this invocation.
- SKILL-PERMISSION-RULES: list of mevedel permission rules to
  seed the spawned invocation's `skill-permission-rules' slot.
- SKILL-MODEL-OVERRIDE: model selector applied on the spawned
  invocation.
- SKILL-EFFORT-OVERRIDE: effort symbol (currently inert pending
  gptel support).
- SKILL-HOOK-RULES: declarative hook rules active inside the spawned
  invocation.

If the parent FSM has no more tool calls and would normally
terminate but still has background agents running, the FSM parks
in the BWAIT state instead.  Background agent completion resumes
the parent from BWAIT to WAIT.

Foreground callback gating matches the BWAIT predicate on both
axes (`background-agents' and `messages'): the callback fires
exactly once when neither has pending work."
  (mevedel-tools--task--dispatch
   main-cb (mevedel-agent-name agent) description prompt
   background agent
   :model-tier model-tier
   :skill-permission-rules skill-permission-rules
   :skill-model-override skill-model-override
   :skill-effort-override skill-effort-override
   :skill-hook-rules skill-hook-rules))

(defun mevedel-tools--task-by-name
    (main-cb agent-type description prompt &optional background model-tier)
  "Look AGENT-TYPE up in the registry and call `mevedel-tools--task'.

Compatibility wrapper for callers that still pass the agent type
as a string (Agent tool, planner tool).  Sends an error string to
MAIN-CB when AGENT-TYPE is not registered."
  (let ((agent (mevedel-agent-get agent-type)))
    (if (not agent)
        (funcall main-cb
                 (format "Error: Unknown agent type: %s" agent-type))
      (mevedel-tools--task main-cb agent description prompt
                           :background background
                           :model-tier model-tier))))

(cl-defun mevedel-tools--task--dispatch
    (main-cb agent-type description prompt background agent
             &key model-tier
             skill-permission-rules
             skill-model-override skill-effort-override
             skill-hook-rules)
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
  failure unregisters the registry entry.

spec keyword args (SKILL-PERMISSION-RULES /
MODEL-TIER / SKILL-MODEL-OVERRIDE / SKILL-EFFORT-OVERRIDE) seed the spawned
invocation's matching slots so the WAIT-state apply handler and
permission resolver pick them up."
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
    ;; Seed the invocation's skill-* slots from the keyword args so
    ;; the WAIT-state apply handler and the bucket-aware permission
    ;; resolver see the caller's skill scope inside the fork.
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
            skill-hook-rules))
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
                  (mevedel-tools--complete-background-agent
                   invocation response)))
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
              (when (mevedel-agent-invocation-transcript-relative-path
                     invocation)
                (mevedel-tools--task--mark-start-blocked
                 invocation "SubagentStart hook blocked sub-agent"))
              (when (and background parent-ctx)
                (mevedel-tools--ctx-remove-background-agent
                 parent-ctx agent-id))
              (setq mevedel-tools--agents-fsm
                    (assoc-delete-all agent-id
                                      mevedel-tools--agents-fsm))))
          (when (and agent-fsm background)
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
                              invocation))))
              (funcall main-cb
                       (cond
                        (rel
                         (list :result launch-result
                               :render-data
                               (list :kind 'agent-transcript
                                     :agent-id agent-id
                                     :transcript-relative-path rel
                                     :background t
                                     :status 'running
                                     :calls 0)))
                        (t launch-result))))))))))

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

(defun mevedel-tools--task--mark-start-blocked (invocation reason)
  "Mark INVOCATION as terminal when it is blocked before request start."
  (when (mevedel-agent-invocation-p invocation)
    (setf (mevedel-agent-invocation-transcript-status invocation) 'error)
    (setf (mevedel-agent-invocation-terminal-reason invocation) reason)
    (when-let* ((session (mevedel-agent-invocation-parent-session invocation))
                (agent-id (mevedel-agent-invocation-agent-id invocation)))
      (mevedel-session-persistence--update-transcript-entry
       session agent-id
       (list :status 'error
             :reason reason)))))

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
         (verdict (mevedel-tool-ui--record-verifier-verdict
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
                            (when verdict (list :verdict verdict))))))))

(defun mevedel-tool-ui--display-label-from-canonical (agent-id)
  "Return the display label form for AGENT-ID.
Canonical agent-id is `<type>--<32-char-md5>'; the display label
is `<type>--<idshort>' (first 8 hex chars of the suffix).  Used
in handle text and attribution fragments."
  (when (stringp agent-id)
    (if-let* ((sep (string-search "--" agent-id)))
        (let* ((type (substring agent-id 0 sep))
               (suffix (substring agent-id (+ sep 2)))
               (short (substring suffix 0 (min 8 (length suffix)))))
          (concat type "--" short))
      agent-id)))

(defun mevedel-tool-ui--handle-badge (render-data)
  "Return a propertized state-badge string for RENDER-DATA, or empty.
Maps `:status' to a visible badge with an appropriate face."
  (let* ((status (plist-get render-data :status))
         (blocked-reason (plist-get render-data :blocked-reason))
         (calls (plist-get render-data :calls))
         (elapsed (plist-get render-data :elapsed))
         (reason (plist-get render-data :reason))
         (verdict (plist-get render-data :verdict))
         ;; Suppress meaningless zeros when underlying data is
         ;; absent.  `:elapsed' is omitted from render-data when
         ;; `started-at' was missing; treat 0/nil identically.
         (calls-suffix (if (and calls (> calls 0))
                           (format " · %d calls" calls)
                         ""))
         (elapsed-suffix (if (and elapsed (> elapsed 0))
                             (format " · %.1fs" elapsed)
                           "")))
    (if blocked-reason
        (propertize (format "[blocked · awaiting %s]" blocked-reason)
                    'font-lock-face 'mevedel-view-handle-blocked)
      (pcase status
        ('running
         (propertize (format "[running%s]" calls-suffix)
                     'font-lock-face 'mevedel-view-handle-running))
        ('completed
         (pcase verdict
           ('fail
            (propertize (format "✗ verdict FAIL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('partial
            (propertize (format "○ verdict PARTIAL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('pass
            (propertize (format "✓ verdict PASS%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))
           (_
            (propertize (format "✓ done%s%s" elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))))
        ('error
         (propertize (format "✗ error%s"
                             (if reason (format " · %s" reason) ""))
                     'font-lock-face 'mevedel-view-handle-error))
        ('aborted
         (propertize "✗ aborted"
                     'font-lock-face 'mevedel-view-handle-error))
        ('incomplete
         (propertize "○ incomplete"
                     'font-lock-face 'mevedel-view-handle-error))
        (_ "")))))

(defun mevedel-tool-ui--agent-blocked-reason (agent-id session)
  "Return the visible blocked reason for AGENT-ID in SESSION, or nil."
  (when (and agent-id session)
    (cond
     ((cl-some (lambda (entry)
                 (equal (plist-get entry :origin) agent-id))
               (mevedel-session-permission-queue session))
      "permission")
     ((cl-some (lambda (entry)
                 (equal (plist-get entry :origin) agent-id))
               (mevedel-session-plan-queue session))
      "plan"))))


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
	         (chat-buffer
	          (if (fboundp 'mevedel-view--interaction-target-buffer)
	              (mevedel-view--interaction-target-buffer
	               (mevedel--prompt--data-buffer))
	            (error "No live view for Ask prompt")))
	         (interaction-id (list :ask (gensym "ask-")))
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
             (when (fboundp 'mevedel-view--interaction-unregister)
               (mevedel-view--interaction-unregister interaction-id))
             (delete-overlay overlay))
           (mevedel-abort))  ; Abort entire execution

         (ask-keymap
           (&optional confirm)
           "Return keymap for the Ask prompt.
When CONFIRM is non-nil, bind submit/edit commands for the review screen."
           (let ((keymap (make-sparse-keymap)))
             (define-key keymap (kbd "TAB") #'cycle-forward)
             (define-key keymap (kbd "<tab>") #'cycle-forward)
             (define-key keymap (kbd "S-TAB") #'cycle-backward)
             (define-key keymap (kbd "<backtab>") #'cycle-backward)
             (define-key keymap (kbd "RET")
                         (if confirm #'submit-answers #'edit-answer))
             (define-key keymap (kbd "<return>")
                         (if confirm #'submit-answers #'edit-answer))
             (when confirm
               (define-key keymap (kbd "C-c C-c") #'submit-answers)
               (define-key keymap (kbd "C-c C-e")
                           #'edit-specific-question)
               (define-key keymap (kbd "e") #'edit-specific-question))
             (define-key keymap (kbd "C-c C-k") #'quit-questionnaire)
             (define-key keymap (kbd "q") #'quit-questionnaire)
             (define-key keymap (kbd "C-g") #'quit-questionnaire)
             keymap))

         (render-ask-body
           (body keymap)
           "Render Ask BODY with KEYMAP through the interaction painter."
           (with-current-buffer chat-buffer
             (setq overlay
                   (mevedel-view--interaction-register
                    (list :kind 'ask
                          :id interaction-id
                          :count 0
                          :body body
                          :priority 150
                          :keymap keymap
                          :help-echo "Ask prompt")))
             (overlay-put overlay 'mevedel-user-request t)
             (overlay-put overlay 'mevedel--callback callback)
             (cl-pushnew overlay mevedel--prompt-overlays :test #'eq)
             (mevedel--prompt--register-canceller)))

         (update-overlay
           (index)
           "Update overlay to show question at INDEX."
           (let* ((q (nth index questions-list))
                  (question-text (plist-get q :question))
                  (options (append (plist-get q :options) nil))
                  (prev-answer (aref answers index))
                  (body
                   (concat
                    "\n"
                    (propertize (format "Question %d/%d"
                                        (1+ index)
                                        (length questions-list))
                                'font-lock-face 'font-lock-string-face)
                    (propertize "\n" 'font-lock-face
                                '(:inherit font-lock-string-face
                                  :underline t :extend t))
                    "\n"
                    (propertize question-text
                                'font-lock-face 'font-lock-escape-face)
                    "\n\n"
                    (propertize "Available options:\n"
                                'font-lock-face
                                'font-lock-constant-face)
                    (mapconcat (lambda (opt) (format "  - %s" opt))
                               options "\n")
                    "\n  - Custom input\n\n"
                    (when prev-answer
                      (concat
                       (propertize "Current answer: "
                                   'font-lock-face 'warning)
                       (propertize prev-answer 'font-lock-face 'bold)
                       "\n\n"))
                    (propertize "Keys: "
                                'font-lock-face 'help-key-binding)
                    (propertize "TAB"
                                'font-lock-face 'help-key-binding)
                    " cycle  "
                    (propertize "RET"
                                'font-lock-face 'help-key-binding)
                    " answer  "
                    (propertize "q"
                                'font-lock-face 'help-key-binding)
                    " cancel\n"
                    (propertize "\n" 'font-lock-face
                                '(:inherit font-lock-string-face
                                  :underline t :extend t)))))
             (render-ask-body body (ask-keymap))))

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
           (let ((body
                  (concat
                   "\n"
                   (propertize "Review Your Answers"
                               'font-lock-face 'font-lock-string-face)
                   (propertize "\n" 'font-lock-face
                               '(:inherit font-lock-string-face
                                 :underline t :extend t))
                   "\n"
                   (mapconcat
                    (lambda (i)
                      (let ((q (nth i questions-list))
                            (a (aref answers i)))
                        (concat
                         (propertize (format "%d. " (1+ i))
                                     'font-lock-face 'bold)
                         (plist-get q :question)
                         "\n"
                         (propertize "   -> "
                                     'font-lock-face 'shadow)
                         (if a
                             (propertize a 'font-lock-face 'success)
                           (propertize "(not answered)"
                                       'font-lock-face 'shadow)))))
                    (number-sequence 0 (1- (length questions-list)))
                    "\n\n")
                   "\n\n"
                   (propertize "Keys: "
                               'font-lock-face 'help-key-binding)
                   (propertize "TAB"
                               'font-lock-face 'help-key-binding)
                   " cycle  "
                   (propertize "RET"
                               'font-lock-face 'help-key-binding)
                   " submit  "
                   (propertize "e"
                               'font-lock-face 'help-key-binding)
                   " edit  "
                   (propertize "q"
                               'font-lock-face 'help-key-binding)
                   " cancel\n"
                   (propertize "\n" 'font-lock-face
                               '(:inherit font-lock-string-face
                                 :underline t :extend t)))))
             (render-ask-body body (ask-keymap t))))

         (cleanup-and-return
           (result)
           "Clean up overlay and return RESULT."
           (when overlay
             (when (fboundp 'mevedel-view--interaction-unregister)
               (mevedel-view--interaction-unregister interaction-id))
             (delete-overlay overlay))
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
:description, :prompt, optional :run_in_background, and optional :model."
  (let ((agent-type (plist-get args :subagent_type))
        (description (plist-get args :description))
        (prompt (plist-get args :prompt))
        (model (plist-get args :model))
        (background (mevedel-tool-truthy-p
                     (plist-get args :run_in_background))))
    (unless (stringp agent-type)
      (error "Parameter subagent_type is required"))
    (unless (stringp description)
      (error "Parameter description is required"))
    (unless (stringp prompt)
      (error "Parameter prompt is required"))
    (when (and model (not (mevedel-model-normalize-tier model)))
      (funcall callback
               (format "Error: Unknown model tier: %s. Available: fast, balanced, strong"
                       model))
      (setq agent-type nil))
    (when agent-type
      (mevedel-tools--task-by-name
       callback agent-type description prompt background
       (and model (mevedel-model-normalize-tier model))))))

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
clickable text-button for terminal transcripts.  Running transcripts
are labelled but not opened through the normal UI.  When path
validation fails, no affordance is exposed -- the suffix is empty."
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
      (let* ((terminal (memq status '(completed error aborted incomplete)))
             (label (format " [transcript: %s]" (or status "running")))
             (button-string (copy-sequence label)))
        (if terminal
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
          (add-text-properties
           0 (length button-string)
           '(font-lock-face shadow
             help-echo "Transcript available when complete")
           button-string))
        button-string)))))

(defun mevedel-tool-ui--display-line-count (text)
  "Return the number of visible lines in TEXT."
  (let ((lines (split-string (or text "") "\n")))
    (while (and lines (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    (length lines)))

(defun mevedel-tool-ui--agent-activity-display-line-count (items)
  "Return the number of displayable agent activity ITEMS."
  (let ((count 0))
    (dolist (item items count)
      (when (memq (plist-get item :type)
                  '(tool-start tool-finish tool-error waiting message status))
        (setq count (1+ count))))))

(defun mevedel-tool-ui--render-agent (name args result render-data)
  "Rendering plist for the Agent tool.
Header shows the subagent type, its short task description, the
state badge (running / done / error / aborted / incomplete),
and -- when RENDER-DATA carries transcript metadata that passes path
hygiene -- a clickable transcript-open button.  Body fontifies in
the data buffer's major mode."
  (when (stringp result)
    (let* ((agent-id (and (consp render-data)
                          (plist-get render-data :agent-id)))
           (session (and (boundp 'mevedel--session) mevedel--session))
           (sidecar-entry
            (and agent-id session
                 (cdr (assoc agent-id
                             (mevedel-session-agent-transcripts
                              session)))))
           (effective-render-data
            (if sidecar-entry
                (append (list :status (plist-get sidecar-entry :status)
                              :transcript-relative-path
                              (plist-get sidecar-entry :path))
                        render-data)
              render-data))
           (blocked-reason
            (and (eq (plist-get effective-render-data :status) 'running)
                 (mevedel-tool-ui--agent-blocked-reason
                  agent-id session)))
           (agent-type (or (plist-get args :subagent_type) "?"))
           (description (or (plist-get args :description) ""))
           (shown (if (string-empty-p description)
                      agent-type
                    (format "%s -- %s" agent-type description)))
           (activity-lines
            (and (plist-get effective-render-data :background)
                 (plist-get effective-render-data :activity)
                 (mevedel-tool-ui--agent-activity-display-line-count
                  (plist-get effective-render-data :activity))))
           (lines (or activity-lines
                      (mevedel-tool-ui--display-line-count result)))
           ;; Empty when render-data lacks a recognized :status
           ;; (e.g. legacy invocations).
           (badge (mevedel-tool-ui--handle-badge
                   (if blocked-reason
                       (plist-put (copy-sequence effective-render-data)
                                  :blocked-reason blocked-reason)
                     effective-render-data)))
           (badge-suffix (if (string-empty-p badge) "" (concat "  " badge)))
           (attribution
            (if (and agent-id (fboundp 'mevedel-view--insert-attribution))
                (concat "  " (mevedel-view--insert-attribution
                              agent-id nil
                              (plist-get effective-render-data :calls)))
              "")))
      (list :header (format "%s: %s (%d lines)%s%s"
                            (or name "Agent") shown lines
                            badge-suffix attribution)
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :vtype 'agent-handle
            :agent-id agent-id
            :agent-status (plist-get effective-render-data :status)
            :agent-background (plist-get effective-render-data :background)
            :agent-activity (plist-get effective-render-data :activity)
            :agent-description description
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
    ;; RequestAccess is itself the user-facing permission prompt for
    ;; expanding workspace access.  Let the tool run so its dedicated
    ;; directory-access UI is the only normal prompt; explicit deny
    ;; rules and protected-path checks still run before this slot.
    :check-permission (lambda (_tool _args) 'allow)
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
           (model string :optional
                  "Optional model tier for this agent invocation."
                  :enum ["fast" "balanced" "strong"])
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

(defun mevedel-permission--prompt-self-insert ()
  "Insert the typed permission key when no permission prompt is active."
  (when (and (characterp last-command-event)
             (not buffer-read-only)
             (not (get-char-property (point) 'read-only)))
    (self-insert-command 1)))

(defun mevedel-permission--prompt-finish-or-self-insert (result)
  "Settle the active permission prompt with RESULT, or insert the key.
This is used by view-buffer mode bindings so permission shortcuts
work from the editable input line while preserving normal typing
when no prompt is pending."
  (unless (mevedel-permission--prompt-finish result)
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-once ()
  "Allow this tool invocation once."
  (interactive)
  (mevedel-permission--prompt-finish-or-self-insert 'allow-once))

(defun mevedel-permission--prompt-approve-session ()
  "Allow this tool for the rest of the session."
  (interactive)
  (if-let ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (if (overlay-get ov 'mevedel-permission-suppress-allow-session)
          (message "Session allow is not available for this prompt")
        (mevedel-permission--prompt-finish 'allow-session))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-always ()
  "Always allow this tool (persisted to disk)."
  (interactive)
  (if-let ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (if (not (overlay-get ov 'mevedel-permission-include-always))
          (message "Persistent allow is not available for this prompt")
        (mevedel-permission--prompt-finish 'always-allow))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-deny-once ()
  "Deny this tool invocation once."
  (interactive)
  (mevedel-permission--prompt-finish-or-self-insert 'deny-once))

(defun mevedel-permission--prompt-deny-session ()
  "Deny this tool for the rest of the session."
  (interactive)
  (mevedel-permission--prompt-finish-or-self-insert 'deny-session))

(defun mevedel-permission--prompt-feedback ()
  "Deny this tool invocation and pass back free-form feedback to the LLM.
Reads a feedback string from the minibuffer, then settles the
prompt with `(feedback . TEXT)' so the pipeline's `:fail'
continuation surfaces the message as a `Permission denied: TEXT'
tool-result the model sees verbatim."
  (interactive)
  (if (mevedel--prompt--overlay-at-point 'mevedel-permission-prompt)
      (let ((text (read-string "Feedback: ")))
        (when (and text (not (string-empty-p (string-trim text))))
          (mevedel-permission--prompt-finish
           (cons 'feedback (string-trim text)))))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-finish (result)
  "Settle the permission prompt overlay at point with RESULT.

RESULT is one of the 6-button vocabulary symbols (`allow-once' etc.,
or a `(feedback . TEXT)' cons) or `aborted' from the canceller
path.  Routes through `mevedel--prompt--settle' so the overlay's
callback fires exactly once and the overlay text/region is removed
atomically."
  (when-let* ((ov (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt)))
    (mevedel--prompt--settle ov result)
    t))

(defun mevedel-permission--prompt-body
    (content include-always &optional suppress-allow-session)
  "Return propertized permission prompt body for CONTENT."
  (mevedel--prompt-framed-body
   (concat
    content
    (propertize "Keys: " 'font-lock-face 'help-key-binding)
    (mevedel--prompt-key "a")
    " allow-once  "
    (unless suppress-allow-session
      (concat
       (mevedel--prompt-key "s")
       " allow-session  "))
    (when include-always
      (concat
       (mevedel--prompt-key "A")
       " always-allow  "))
    (mevedel--prompt-key "d")
    " deny-once  "
    (mevedel--prompt-key "D")
    " deny-session  "
    (mevedel--prompt-key "f")
    " feedback\n")
   'warning))

(defun mevedel-permission--prompt-async-with-content
    (content include-always cont &optional count entry suppress-allow-session)
  "Display a 5-button permission prompt with a caller-built CONTENT block.
Shared engine for the generic permission prompt and the Bash
prompt.  CONTENT is a propertized string forming the body
between the upper and lower warning rules; INCLUDE-ALWAYS gates
the \"always-allow\" key; SUPPRESS-ALLOW-SESSION hides the
\"allow-session\" key.  CONT receives the queue-vocabulary
outcome (`allow-once' / `allow-session' / `always-allow' /
`deny-once' / `deny-session' / `aborted')."
  (let* ((target-buf
          (if (fboundp 'mevedel-view--interaction-target-buffer)
              (mevedel-view--interaction-target-buffer
               (mevedel--prompt--data-buffer))
            (error "No live view for queued prompt")))
         (interaction-id (or (and entry
                                  (mevedel-queue--entry-metadata-get
                                   entry :interaction-id))
                             (let ((id (list :permission
                                             (gensym "permission-"))))
                               (when entry
                                 (mevedel-queue--entry-metadata-put
                                  entry :interaction-id id))
                               id)))
         (ov nil))
    (when entry
      (mevedel-queue--entry-metadata-put entry :view-buffer target-buf))
    (with-current-buffer target-buf
      (let ((map (make-sparse-keymap)))
        (define-key map "a" #'mevedel-permission--prompt-approve-once)
        (unless suppress-allow-session
          (define-key map "s" #'mevedel-permission--prompt-approve-session))
        (when include-always
          (define-key map "A" #'mevedel-permission--prompt-approve-always))
        (define-key map "d" #'mevedel-permission--prompt-deny-once)
        (define-key map "D" #'mevedel-permission--prompt-deny-session)
        (define-key map "f" #'mevedel-permission--prompt-feedback)
        (define-key map [?q] #'mevedel-permission--prompt-deny-once)
        (define-key map (kbd "C-g") #'mevedel-permission--prompt-deny-once)
        (setq ov
              (mevedel-view--interaction-register
               (list :kind 'permission
                     :id interaction-id
                     :count (or count 1)
                     :body (mevedel-permission--prompt-body
                            content include-always suppress-allow-session)
                     :priority 100
                     :keymap map
                     :help-echo "Permission prompt"
                     :entry entry
                     :activate cont)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-permission-suppress-allow-session
                     suppress-allow-session)
        (overlay-put ov 'mevedel-permission-include-always include-always)
        (overlay-put ov 'mevedel--callback cont)
        (overlay-put ov 'mevedel-user-request t)
        (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
        (mevedel--prompt--register-canceller)
        (goto-char (mevedel-view--interaction-anchor))))
    ov))

(defun mevedel-permission--build-attribution-line (origin)
  "Return a ` from <type>--<idshort>\n' line for ORIGIN, or empty string.
Reuses `mevedel-view--insert-attribution' so the click target,
display-label derivation, and terminal-status gating are
consistent across handle / mailbox / plan / permission elements."
  (cond
   ((null origin) "")
   ((equal origin "main") "")
   ((fboundp 'mevedel-view--insert-attribution)
    (concat (mevedel-view--insert-attribution origin) "\n"))
   (t "")))

(defun mevedel-permission--prompt-async (tool-name path include-always cont
                                                   &optional count entry)
  "Display the generic permission prompt overlay; settle CONT exactly once.

Async entry point for the 5-button permission UI.  CONT receives one
of `allow-once' / `allow-session' / `always-allow' / `deny-once' /
`deny-session' / `aborted'.  When INCLUDE-ALWAYS is non-nil, the
\"Always allow\" key is offered (persists the rule to disk).

Multiple concurrent prompts produce multiple overlays; each settles
independently in user-chosen order.  The first overlay per request
registers a dismiss thunk on the request's cancellers list -- shared
machinery with `mevedel--prompt-user-with-overlay'.  No
`recursive-edit', no nesting, no queue serialization."
  (mevedel-permission--prompt-async-attributed
   tool-name path include-always nil cont count entry))

(defun mevedel-permission--prompt-async-attributed
    (tool-name path include-always origin cont &optional count entry)
  "Permission prompt with optional ORIGIN attribution header.
Permission prompts originated by sub-agents carry a
`from <type>--<idshort>' fragment so the user can see which agent
is asking.  When ORIGIN is nil or \"main\", the attribution line
is suppressed.  See
`mevedel-permission--prompt-async' for the rest of the contract."
  (let ((content (concat
                  (propertize "Permission Request\n"
                              'font-lock-face '(:inherit bold :inherit warning))
                  (mevedel-permission--build-attribution-line origin)
                  "\n"
                  (propertize "Tool: " 'font-lock-face 'font-lock-escape-face)
                  (propertize (format "%s\n" tool-name)
                              'font-lock-face 'font-lock-constant-face)
                  (when path
                    (concat
                     (propertize "Path: " 'font-lock-face 'font-lock-escape-face)
                     (propertize (format "%s\n" path)
                                 'font-lock-face 'font-lock-string-face)))
                  "\n")))
    (mevedel-permission--prompt-async-with-content
     content include-always cont count entry)))

(defun mevedel-permission--bash-guardian-label (value)
  "Return a display label for Bash guardian VALUE."
  (capitalize
   (replace-regexp-in-string "-" " " (format "%s" value))))

(defun mevedel-permission--bash-guardian-face (risk)
  "Return face for Bash guardian RISK."
  (pcase risk
    ('low 'success)
    ('medium 'warning)
    ((or 'high 'critical) 'error)
    (_ 'font-lock-comment-face)))

(defun mevedel-permission--format-bash-guardian (guardian &optional status)
  "Return formatted Bash GUARDIAN guidance.
When STATUS is `pending' and GUARDIAN is nil, return the
in-flight placeholder shown while advisory guidance is loading.
When STATUS is `unavailable', return a short unavailable note."
  (cond
   (guardian
    (let ((risk (plist-get guardian :risk))
          (recommendation (plist-get guardian :recommendation))
          (reason (plist-get guardian :reason)))
      (concat
       "\n"
       (propertize "Guardian guidance\n"
                   'font-lock-face '(:inherit bold))
       (propertize "Risk: " 'font-lock-face 'font-lock-escape-face)
       (propertize (format "%s\n" (mevedel-permission--bash-guardian-label risk))
                   'font-lock-face
                   (mevedel-permission--bash-guardian-face risk))
       (propertize "Recommendation: "
                   'font-lock-face 'font-lock-escape-face)
       (propertize
        (format "%s\n" (mevedel-permission--bash-guardian-label
                        recommendation))
        'font-lock-face 'font-lock-constant-face)
       (propertize "Reason: " 'font-lock-face 'font-lock-escape-face)
       (propertize (format "%s\n" reason)
                   'font-lock-face 'font-lock-comment-face))))
   ((eq status 'pending)
    (concat
     "\n"
     (propertize "Guardian guidance\n"
                 'font-lock-face '(:inherit bold))
     (propertize "Status: " 'font-lock-face 'font-lock-escape-face)
     (propertize "Analyzing command risk...\n"
                 'font-lock-face 'font-lock-comment-face)))
   ((eq status 'unavailable)
    (concat
     "\n"
     (propertize "Guardian guidance\n"
                 'font-lock-face '(:inherit bold))
     (propertize "Unavailable\n"
                 'font-lock-face 'font-lock-comment-face)))))

(defun mevedel-permission--prompt-async-bash
    (command dangerous include-always origin cont &optional count entry)
  "Display a Bash-specific permission prompt.
COMMAND is the parsed bash command string.  DANGEROUS is non-nil
when the command contains a dangerous binary per
`mevedel-bash-dangerous-commands' (renders prominently to warn
the user).  Dangerous prompts suppress session/permanent allow
choices.  INCLUDE-ALWAYS gates the always-allow key for
non-dangerous prompts the same way as the generic prompt.  ORIGIN
is the canonical agent-id that issued the request; renders the
attribution line when non-nil and not \"main\".  CONT receives the
queue-vocabulary outcome.

Routes Bash through the same prompt machinery as generic
permissions; when available, `allow-session' / `always-allow'
produce session / persistent pattern rules via the Bash slot
adapter."
  (let* ((commands (and entry (plist-get entry :commands)))
         (unparseable (and entry (plist-get entry :unparseable)))
         (allow-patterns (and entry (plist-get entry :allow-patterns)))
         (guardian-cell (and entry (plist-get entry :guardian-cell)))
         (guardian (and entry
                        (or (plist-get entry :guardian)
                            (car guardian-cell))))
         (guardian-status (and guardian-cell (cadr guardian-cell)))
         (content
          (concat
          (propertize (if dangerous
                          "Bash Command Execution Request — DANGEROUS\n"
                        "Bash Command Execution Request\n")
                      'font-lock-face
                      (if dangerous
                          '(:inherit bold :inherit error)
                        '(:inherit bold :inherit warning)))
          (mevedel-permission--build-attribution-line origin)
          "\n"
          (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
          (propertize (format "%s\n" command)
                      'font-lock-face 'font-lock-string-face)
          (mevedel-permission--format-bash-guardian
           guardian guardian-status)
          (when commands
            (concat
             "\n"
             (propertize "Detected commands: "
                         'font-lock-face 'font-lock-escape-face)
             (propertize (mapconcat #'identity commands ", ")
                         'font-lock-face 'font-lock-constant-face)
             "\n"))
          (when (and allow-patterns (not dangerous))
            (concat
             (propertize "Session/always allow will add: "
                         'font-lock-face 'font-lock-escape-face)
             (propertize (mapconcat
                          (lambda (pattern) (format "`%s'" pattern))
                          allow-patterns ", ")
                         'font-lock-face 'font-lock-constant-face)
             "\n"))
          (when dangerous
            (concat
             (propertize "⚠ " 'font-lock-face 'error)
             (propertize "Contains a binary on `mevedel-bash-dangerous-commands'.\n"
                         'font-lock-face 'font-lock-comment-face)
             (propertize
              "Session/permanent allow is disabled for dangerous Bash commands.\n"
              'font-lock-face 'font-lock-comment-face)))
          (when unparseable
            (propertize
             "Warning: Command contains complex syntax that could not be fully parsed.\n"
             'font-lock-face 'warning))
          "\n")))
    (mevedel-permission--prompt-async-with-content
     content (and include-always (not dangerous)) cont count entry dangerous)))

(defun mevedel-permission--prompt-async-eval
    (content cont &optional count entry)
  "Display an Eval permission prompt from caller-built CONTENT.
CONT receives `allow-once', `deny-once', `(feedback . TEXT)', or
`aborted'."
  (let* ((target-buf
          (if (fboundp 'mevedel-view--interaction-target-buffer)
              (mevedel-view--interaction-target-buffer
               (mevedel--prompt--data-buffer))
            (error "No live view for queued prompt")))
         (interaction-id (or (and entry
                                  (mevedel-queue--entry-metadata-get
                                   entry :interaction-id))
                             (let ((id (list :permission
                                             (gensym "eval-permission-"))))
                               (when entry
                                 (mevedel-queue--entry-metadata-put
                                  entry :interaction-id id))
                               id)))
         (body (concat
                "\n"
                (propertize "\n" 'font-lock-face
                            '(:inherit warning :underline t :extend t))
                content
                (propertize "Keys: " 'font-lock-face 'help-key-binding)
                (propertize "a" 'font-lock-face 'help-key-binding)
                " allow-once  "
                (propertize "d" 'font-lock-face 'help-key-binding)
                " deny-once  "
                (propertize "f" 'font-lock-face 'help-key-binding)
                " feedback\n"
                (propertize "\n" 'font-lock-face
                            '(:inherit warning :underline t :extend t))))
         ov)
    (when entry
      (mevedel-queue--entry-metadata-put entry :view-buffer target-buf))
    (font-lock-append-text-property
     0 (length body) 'font-lock-face (gptel-agent--block-bg) body)
    (with-current-buffer target-buf
      (let ((map (make-sparse-keymap)))
        (define-key map "a" #'mevedel-permission--prompt-approve-once)
        (define-key map "d" #'mevedel-permission--prompt-deny-once)
        (define-key map "f" #'mevedel-permission--prompt-feedback)
        (define-key map [?q] #'mevedel-permission--prompt-deny-once)
        (define-key map (kbd "C-g") #'mevedel-permission--prompt-deny-once)
        (setq ov
              (mevedel-view--interaction-register
               (list :kind 'permission
                     :id interaction-id
                     :count (or count 1)
                     :body body
                     :priority 100
                     :keymap map
                     :help-echo "Eval permission prompt"
                     :entry entry
                     :activate cont)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel--callback cont)
        (overlay-put ov 'mevedel-user-request t)
        (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
        (mevedel--prompt--register-canceller)
        (goto-char (mevedel-view--interaction-anchor))))
    ov))

(provide 'mevedel-tool-ui)

;;; mevedel-tool-ui.el ends here
