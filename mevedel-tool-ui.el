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
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--ctx-push-message "mevedel-tools" (ctx msg))
(declare-function mevedel-tools--ctx-push-background-agent "mevedel-tools" (ctx agent-id))
(declare-function mevedel-tools--ctx-remove-background-agent "mevedel-tools" (ctx agent-id))
(declare-function mevedel-tools--ctx-background-agents "mevedel-tools" (ctx))
(defvar mevedel-tools--current-fsm)

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-view'
(defvar mevedel-view--input-marker)

;; `mevedel-workspace'
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))
(declare-function mevedel-add-project-root "mevedel-workspace" (directory))


;;
;;; Directory access

(defvar-local mevedel--pending-access-requests nil
  "Alist of (ROOT . STATUS) for in-flight access requests.

STATUS can be \\='pending, \\='granted, or \\='denied.

This is buffer-local per chat buffer to deduplicate access prompts
within a session.")

(defvar-local mevedel--access-request-lock nil
  "Non-nil when an access request is being processed.
This is buffer-local per chat buffer to prevent race conditions.")

(defun mevedel-tools--request-access (root reason &optional buffer)
  "Request access to ROOT with REASON, handling concurrent requests gracefully.
Returns t if access granted, nil if denied or interrupted.

BUFFER is the chat buffer context for buffer-local state (defaults to
current buffer). This ensures we're operating on the correct session's
access grants."
  (with-current-buffer (or buffer (current-buffer))
    (let ((pending-status (alist-get root mevedel--pending-access-requests
                                     nil nil #'string=)))
      (cond
       ;; Already granted in this batch
       ((eq pending-status 'granted) t)

       ;; Already denied in this batch
       ((eq pending-status 'denied) nil)

       ;; Request is pending - wait for it with user interrupt support
       ((eq pending-status 'pending)
        (let ((result (mevedel--wait-for-access-resolution root)))
          (eq result 'granted)))

       ;; New request - acquire lock and prompt
       (t
        ;; Wait for lock with interrupt support
        (let ((got-lock
               (while-no-input
                 (while mevedel--access-request-lock
                   (sit-for 0.05))
                 t)))
          (when got-lock
            (setq mevedel--access-request-lock t)
            (unwind-protect
                (progn
                  ;; Double-check after acquiring lock
                  (let ((status (alist-get root mevedel--pending-access-requests
                                           nil nil #'string=)))
                    (if status
                        (eq status 'granted)
                      ;; Mark as pending
                      (setf (alist-get root mevedel--pending-access-requests
                                       nil nil #'string=) 'pending)

                      ;; Actually prompt user
                      (let* ((result (mevedel--prompt-user-for-access root reason))
                             (granted (eq result t)))
                        (setf (alist-get root mevedel--pending-access-requests
                                         nil nil #'string=)
                              (if granted 'granted 'denied))

                        ;; Update session tracking if granted
                        (when granted
                          (mevedel-add-project-root root))

                        granted))))
              (setq mevedel--access-request-lock nil)))))))))

(defun mevedel--wait-for-access-resolution (root)
  "Wait for pending access request for ROOT to resolve.

Returns \\='granted, \\='denied, or \\='interrupted."
  (let ((result
         (while-no-input
           (while (eq (alist-get root mevedel--pending-access-requests
                                 nil nil #'string=)
                      'pending)
             ;; Check every 50ms, allow redisplay
             (sit-for 0.05))
           ;; Return the final status
           (alist-get root mevedel--pending-access-requests
                      nil nil #'string=))))
    (cond
     ;; `while-no-input' returned t (user input)
     ((eq result t) 'interrupted)
     ;; User quit with C-g
     ((null result) 'interrupted)
     ;; 'granted or 'denied
     (t result))))

(defvar-local mevedel--request-overlay nil
  "Overlay for the current user prompt request, if any.")

(defvar-local mevedel--request-result nil
  "Result of the user prompt request.
Can be one of:
- t (approved)
- nil (denied)
- (feedback . TEXT) where TEXT is the user's feedback string
- \\='pending (waiting for user response)")

(defun mevedel--approve-request ()
  "Approve the request at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request)))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (setq mevedel--request-result t)
    (let ((inhibit-read-only t))
      (delete-overlay ov)
      (delete-region start end))
    (exit-recursive-edit)))

(defun mevedel--deny-request ()
  "Deny the request at point and abort execution."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request)))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (setq mevedel--request-result nil)
    (let ((inhibit-read-only t))
      (delete-overlay ov)
      (delete-region start end))
    (exit-recursive-edit)
    (mevedel-abort)))  ; Abort entire execution

(defun mevedel--feedback-request ()
  "Deny the request at point with feedback."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request)))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (let ((feedback (read-string "What should be changed? ")))
      (setq mevedel--request-result (cons 'feedback feedback))
      (let ((inhibit-read-only t))
        (delete-overlay ov)
        (delete-region start end))
      (exit-recursive-edit))))

(defun mevedel--prompt-user-with-overlay (title content question &optional help-echo-text)
  "Prompt user with an overlay in the chat buffer.

TITLE is the heading text (will be styled as bold + warning).
CONTENT is the main body text describing the request.
QUESTION is the final question text (will be styled as bold).
HELP-ECHO-TEXT is optional hover text (defaults to generic key
bindings).

Returns one of:
- t if approved
- nil if denied
- (feedback . TEXT) if user provides feedback

Displays an overlay in the chat buffer with approve/deny/feedback keybindings,
using `recursive-edit' to block until the user responds."
  (let* ((chat-buffer (or (and (boundp 'mevedel--view-buffer)
                               mevedel--view-buffer
                               (buffer-live-p mevedel--view-buffer)
                               mevedel--view-buffer)
                          (current-buffer)))
         (start nil)
         (ov nil))
    (with-current-buffer chat-buffer
      (save-excursion
        (goto-char (if (and (boundp 'mevedel-view--input-marker)
                            mevedel-view--input-marker)
                       mevedel-view--input-marker
                     (point-max)))
        (setq start (point))

        ;; Insert prompt content
        (let ((inhibit-read-only t))
          (insert (concat
                   (propertize "\n" 'font-lock-face '(:inherit warning :underline t :extend t))
                   (propertize (format "%s\n" title) 'font-lock-face '(:inherit bold :inherit warning))
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
          (insert (propertize "\n" 'font-lock-face '(:inherit warning :underline t :extend t)))

          ;; Create overlay with keymap
          (setq ov (make-overlay start (point) nil t))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority 100)
          (overlay-put ov 'mevedel-user-request t)
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'help-echo
                       (or help-echo-text
                           (concat title ": "
                                   (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                                               'face 'help-key-binding))))
          (overlay-put ov 'keymap
                       (define-keymap
                         ;; Approve bindings
                         "y"        #'mevedel--approve-request
                         "a"        #'mevedel--approve-request
                         "RET"      #'mevedel--approve-request
                         "<return>" #'mevedel--approve-request
                         "C-c C-c"  #'mevedel--approve-request
                         ;; Deny bindings
                         "n"        #'mevedel--deny-request
                         "d"        #'mevedel--deny-request
                         "q"        #'mevedel--deny-request
                         "C-c C-k"  #'mevedel--deny-request
                         "C-g"      #'mevedel--deny-request
                         ;; Feedback binding
                         "f"        #'mevedel--feedback-request))

          ;; Store overlay reference
          (setq mevedel--request-overlay ov)

          ;; Apply background
          (font-lock-append-text-property
           start (point) 'font-lock-face (gptel-agent--block-bg))))

      ;; Position cursor at the overlay
      (goto-char start))

    ;; Wait for user decision via recursive-edit.
    ;; Set pending state in BOTH buffers: the approve/deny commands
    ;; fire in the view buffer (where the overlay lives) but this
    ;; function reads the result from the calling buffer (data buffer).
    (setq mevedel--request-result 'pending)
    (when (and (buffer-live-p chat-buffer) (not (eq chat-buffer (current-buffer))))
      (with-current-buffer chat-buffer
        (setq mevedel--request-result 'pending)))

    ;; Enter recursive edit - allows user input while blocking.  A
    ;; `minibuffer-quit' (e.g. ESC in an unrelated minibuffer the user
    ;; opened while the overlay is pending) must not abort the request;
    ;; re-enter the recursive edit in that case.
    (unwind-protect
        (let (done)
          (while (not done)
            (condition-case err
                (progn (recursive-edit) (setq done t))
              (minibuffer-quit nil)
              ;; Treat quit (C-g) as a denial
              (quit (setq done t)
                    (setq mevedel--request-result nil)
                    (mevedel-abort))
              (error
               (setq done t)
               (user-error "%s" (error-message-string err))
               (setq mevedel--request-result nil)
               (mevedel-abort)))))

      ;; Clean up overlay if still present
      (when (and ov (overlay-buffer ov))
        (let ((inhibit-read-only t)
              (start (overlay-start ov))
              (end (overlay-end ov)))
          (delete-overlay ov)
          (delete-region start end))
        (when (eq mevedel--request-result 'pending)
          (setq mevedel--request-result nil))))

    ;; Read result from the view buffer where the approve/deny
    ;; commands set it, falling back to the local value.
    (let ((result (if (and (buffer-live-p chat-buffer)
                           (not (eq chat-buffer (current-buffer))))
                      (buffer-local-value 'mevedel--request-result chat-buffer)
                    mevedel--request-result)))
      ;; Treat lingering pending as denial
      (if (eq result 'pending) nil result))))

(defun mevedel--prompt-user-for-access (root reason)
  "Prompt user for access to ROOT with REASON in the chat buffer.
Returns one of:
- t if granted
- nil if denied
- (feedback . TEXT) if user provides feedback

Displays an overlay in the chat buffer with approve/deny/feedback keybindings."
  (let ((content (concat
                  "The LLM is requesting access to a directory outside the current workspace.\n\n"
                  (propertize "Directory: " 'font-lock-face 'font-lock-escape-face)
                  (propertize (format "%s\n" root) 'font-lock-face 'font-lock-constant-face)
                  (propertize "Reason: " 'font-lock-face 'font-lock-escape-face)
                  (format "%s" reason))))
    (mevedel--prompt-user-with-overlay
     "Directory Access Request"
     content
     "Grant access to this directory?"
     (concat "Directory access request: "
             (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                         'face 'help-key-binding)))))

(defun mevedel--clear-pending-access-requests (&rest _)
  "Clear the pending access requests cache.
Should be called after each LLM response completes."
  (setq mevedel--pending-access-requests nil))

(cl-defun mevedel--tools-request-dir-access (callback directory reason)
  "Request user permission to access a directory.

CALLBACK is for async execution.
DIRECTORY is the path to request access to.
REASON explains why access is needed."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel--tools-request-dir-access
    (directory stringp)
    (reason stringp))

  (unless (and (file-readable-p directory) (file-directory-p directory))
    (cl-return-from mevedel--tools-request-dir-access
      (funcall callback (format "Error: directory '%s' is not readable" directory))))
  (let ((expanded (expand-file-name directory)))
    (if (mevedel-tools--request-access expanded reason)
        (funcall callback
                 (format "Access granted to %s. You can now read and write files in this directory." expanded))
      (funcall callback
               (format "Access denied to %s. You cannot access files in this directory." expanded)))))


;;
;;; Agent tool

(defvar-local mevedel-tools--agents-fsm nil
  "Alist mapping agents to their FSM.")

(defvar mevedel-tools--agent-invocation nil
  "Dynamically bound to a `mevedel-agent-invocation' during agent dispatch.

Set by `mevedel-tools--task' around the call to `gptel-agent--task' so
`mevedel-tools--agent-request-advice' can attach reminders infrastructure
to the spawned request.  Nil outside agent dispatch, which is how the
advice stays a no-op for all non-agent `gptel-request' calls.")

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
               (data (plist-get info :data)))
          (when data
            (gptel--inject-prompt
             (plist-get info :backend) data
             (list :role "user"
                   :content (string-join blocks "\n")))))))
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
waiting for background children."
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
            (gptel--update-status " Waiting for agents..." 'warning)))))))

(defun mevedel-tools--inject-bwait-transition (fsm)
  "Modify FSM's transition table to add the BWAIT parking state.

Inserts a `mevedel-tools--background-agents-pending-p' predicate
before the `(t . DONE)' fallthrough in both the TYPE and TRET states.
When the predicate matches, the FSM parks in BWAIT instead of
terminating.

Also adds BWAIT to the handler alist with
`mevedel-tools--handle-bwait', and registers BWAIT as a valid state in
the transition table (with no outgoing transitions -- the background
agent callback forces a transition to WAIT explicitly)."
  (let ((table (copy-tree (gptel-fsm-table fsm)))
        (handlers (gptel-fsm-handlers fsm))
        (pred #'mevedel-tools--background-agents-pending-p))
    ;; Insert BWAIT transition before (t . DONE) in TYPE and TRET.
    (dolist (state '(TYPE TRET))
      (when-let* ((entry (assq state table)))
        (let ((transitions (cdr entry))
              (new-transitions nil))
          (dolist (tr transitions)
            (when (eq (car tr) t)
              ;; Insert bg-pending? -> BWAIT before the (t . DONE) catch-all.
              (push (cons pred 'BWAIT) new-transitions))
            (push tr new-transitions))
          (setcdr entry (nreverse new-transitions)))))
    ;; Add BWAIT state with no outgoing transitions (callback-driven).
    (unless (assq 'BWAIT table)
      (push '(BWAIT) table))
    (setf (gptel-fsm-table fsm) table)
    ;; Add BWAIT handler.
    (setf (gptel-fsm-handlers fsm)
          (mevedel-tools--augment-agent-handlers
           handlers
           :append `((BWAIT . (,#'mevedel-tools--handle-bwait)))))))

(defun mevedel-tools--agent-request-advice (orig-fun &rest args)
  "Around advice on `gptel-request' for agent reminder wiring.

When `mevedel-tools--agent-invocation' is bound (i.e., we are inside
`mevedel-tools--task'), stashes the invocation on the `:context'
overlay and prepends `mevedel-tools--handle-wait-inject' to the FSM's
WAIT handlers so reminders are injected and turn count advances on
every WAIT cycle.  The WAIT handler must run before
`gptel-agent--indicate-wait' and `gptel--handle-wait' so its payload
mutation lands in the next HTTP request -- hence prepend, not append.

Also injects BWAIT transitions so the FSM parks when background
agents are still running instead of terminating.
Outside agent dispatch this is a no-op."
  (if-let* ((inv mevedel-tools--agent-invocation))
      (let* ((prompt (car args))
             (kwargs (copy-sequence (cdr args)))
             (ov (plist-get kwargs :context))
             (fsm (plist-get kwargs :fsm)))
        (when (overlayp ov)
          (overlay-put ov 'mevedel-agent-invocation inv))
        (when fsm
          (setf (gptel-fsm-handlers fsm)
                (mevedel-tools--augment-agent-handlers
                 (gptel-fsm-handlers fsm)
                 :prepend
                 `((WAIT . (,#'mevedel-tools--handle-message-inject
                            ,#'mevedel-tools--handle-wait-inject)))))
          ;; Inject BWAIT transitions for background agent parking.
          (mevedel-tools--inject-bwait-transition fsm))
        ;; `gptel-request' defaults `:stream' to nil, so sub-agents
        ;; would go non-streaming even when the chat buffer is
        ;; streaming.  Forward the caller's `gptel-stream' unless the
        ;; caller already supplied `:stream' explicitly.
        (unless (plist-member kwargs :stream)
          (setq kwargs (plist-put kwargs :stream gptel-stream)))
        (apply orig-fun prompt kwargs))
    (apply orig-fun args)))

(advice-add 'gptel-request :around #'mevedel-tools--agent-request-advice)

(defun mevedel-tools--task (main-cb agent-type description prompt
                                    &optional background)
  "Call an agent to do specific compound tasks.

This is a thin wrapper around `gptel-agent--task' which manages the
entries in `mevedel-tools--agents-fsm'.  For agents defined via
`mevedel-define-agent', a fresh `mevedel-agent-invocation' is bound
around the call so the reminders transform and terminal turn-count
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
BWAIT to WAIT, which drains the mailbox and fires a new LLM request."
  (let* ((agent-id (concat agent-type "--" (md5 (format "%s%s%s%s" (system-name) (emacs-pid)
                                                        (current-time) (random)))))
         (agent (mevedel-agent-get agent-type))
         (mevedel-tools--agent-invocation
          (and agent (mevedel-agent-invocation-create agent)))
         (parent-ctx (mevedel-tools--current-deferred-context))
         (parent-fsm (and background mevedel-tools--current-fsm))
         ;; The invocation context for this agent, used by the
         ;; foreground callback to check background-agents.  Set
         ;; after the agent FSM is created (overlay stashed).
         (this-ctx mevedel-tools--agent-invocation)
         (wrapped-callback
          (if background
              ;; Background mode: deliver result to parent's mailbox.
              (lambda (response &rest _rest)
                (when parent-ctx
                  (mevedel-tools--ctx-push-message
                   parent-ctx
                   (list :from agent-id
                         :body (format "<agent-result agent-id=\"%s\" type=\"%s\" \
description=\"%s\">\n%s\n</agent-result>"
                                       agent-id agent-type description
                                       (or response "(no response)"))
                         :timestamp (current-time)))
                  ;; Remove from parent's background tracking.
                  (mevedel-tools--ctx-remove-background-agent parent-ctx agent-id))
                ;; Cleanup stale agent FSMs
                (setq mevedel-tools--agents-fsm
                      (cl-loop for (id . fsm) in mevedel-tools--agents-fsm
                               unless (eq (gptel-fsm-state fsm) 'DONE)
                               collect `(,id . ,fsm)))
                ;; If the parent FSM is parked in BWAIT, resume it.
                (when (and parent-fsm
                           (eq (gptel-fsm-state parent-fsm) 'BWAIT))
                  (gptel--fsm-transition parent-fsm 'WAIT)))
            ;; Foreground mode: return result directly -- unless this
            ;; agent still has background children running.  In that
            ;; case, stash the result and let BWAIT park the FSM.
            ;; The eventual background-agent completion callback will
            ;; forward main-cb once the last child finishes.
            (lambda (response &rest rest)
              (if (and this-ctx
                       (mevedel-tools--ctx-background-agents this-ctx))
                  ;; Stash result; BWAIT will keep the FSM alive.
                  (when this-ctx
                    (setf (mevedel-agent-invocation-stashed-result this-ctx)
                          (cons response rest)))
                ;; No children pending -- fire immediately.
                (apply main-cb response rest))
              ;; Cleanup stale agent FSMs
              (setq mevedel-tools--agents-fsm
                    (cl-loop for (id . fsm) in mevedel-tools--agents-fsm
                             unless (eq (gptel-fsm-state fsm) 'DONE)
                             collect `(,id . ,fsm))))))
         (agent-fsm (gptel-agent--task wrapped-callback agent-type description prompt)))
    (overlay-put (plist-get (gptel-fsm-info agent-fsm) :context) 'mevedel-tools--agent-id agent-id)
    (setf (alist-get agent-id mevedel-tools--agents-fsm nil nil #'equal) agent-fsm)
    ;; In background mode, track and unblock the parent FSM immediately.
    (when background
      (when parent-ctx
        (mevedel-tools--ctx-push-background-agent parent-ctx agent-id))
      (funcall main-cb
               (format "Agent launched in background: %s (id: %s). \
Its result will be delivered to your mailbox when it finishes. \
Use SendMessage(to=\"%s\", ...) to send it guidance."
                       agent-type agent-id agent-type)))))


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
        (funcall callback (format "Error: Directory '%s' is not readable" directory))
      (let ((expanded (expand-file-name directory)))
        (if (mevedel-tools--request-access expanded reason)
            (funcall callback
                     (format "Access granted to %s. You can now read and write files in this directory."
                             expanded))
          (funcall callback
                   (format "Access denied to %s. You cannot access files in this directory."
                           expanded)))))))

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
    :get-path (lambda (args) (plist-get args :directory)))

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
    :groups (util))

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
               "Recipient: agent type, agent id, or \"main\" for the chat.")
           (message string :required
                    "Message body to deliver."))
    :read-only-p t
    :groups (util)))


;;
;;; Permission prompt

(defvar-local mevedel--permission-result nil
  "Result of the permission prompt.
One of `allow-once', `allow-session', `always-allow',
`deny-once', `deny-session', or nil.")

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
  "Set RESULT and exit the permission prompt."
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-permission-prompt)))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (setq mevedel--permission-result result)
    (delete-overlay ov)
    (delete-region start end)
    (exit-recursive-edit)))

(defun mevedel-permission--prompt (tool-name &optional path include-always)
  "Prompt user for permission to use TOOL-NAME on PATH.

When INCLUDE-ALWAYS is non-nil, include the \"Always allow\"
option that persists the rule to disk.

Returns one of `allow-once', `allow-session', `always-allow',
`deny-once', or `deny-session'.

Uses `recursive-edit' to block until the user responds."
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
          ;; Key legend
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
          ;; Create overlay
          (setq ov (make-overlay start (point) nil t))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority 100)
          (overlay-put ov 'mevedel-permission-prompt t)
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
           start (point) 'font-lock-face (gptel-agent--block-bg))))
      (goto-char start)
      ;; Block until user responds
      (setq mevedel--permission-result nil)
      (unwind-protect
          (let (done)
            (while (not done)
              (condition-case _err
                  (progn (recursive-edit) (setq done t))
                (minibuffer-quit nil)
                (quit (setq done t)
                      (setq mevedel--permission-result 'deny-once)
                      (mevedel-abort)))))
        (when (and ov (overlay-buffer ov))
          (let ((inhibit-read-only t)
                (s (overlay-start ov))
                (e (overlay-end ov)))
            (delete-overlay ov)
            (delete-region s e))
          (when (null mevedel--permission-result)
            (setq mevedel--permission-result 'deny-once))))
      mevedel--permission-result)))

(provide 'mevedel-tool-ui)

;;; mevedel-tool-ui.el ends here
