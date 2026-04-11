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
(declare-function gptel-agent--task "ext:gptel-agent-tools" (main-cb agent-type description prompt))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest args))

;; `mevedel-agents'
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-reminders "mevedel-agents" (agent) t)
(declare-function mevedel-agent-invocation-create "mevedel-agents" (agent))

;; `mevedel-reminders'
(declare-function mevedel-reminders--collect-from "mevedel-reminders"
                  (reminders turn-count ctx))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline"
                  (tool callback args))
(declare-function mevedel-pipeline--positional-to-plist "mevedel-pipeline"
                  (arg-values arg-specs))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-register "mevedel-tool-registry")

;; `gptel'
(defvar gptel--fsm-last)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-tools'
(declare-function mevedel-tools--tool-search "mevedel-tools" (callback query &optional load))

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
    (delete-overlay ov)
    (delete-region start end)
    (exit-recursive-edit)))

(defun mevedel--deny-request ()
  "Deny the request at point and abort execution."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request)))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (setq mevedel--request-result nil)
    (delete-overlay ov)
    (delete-region start end)
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
      (delete-overlay ov)
      (delete-region start end)
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
  (let* ((chat-buffer (current-buffer))
         (info (gptel-fsm-info gptel--fsm-last))
         (position (plist-get info :tracking-marker))
         (start position)
         (ov nil))
    (with-current-buffer chat-buffer
      (save-excursion
        (goto-char (or position (point-max)))
        (setq start (point))

        ;; Insert prompt content
        (insert "\n")
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
         start (point) 'font-lock-face (gptel-agent--block-bg)))

      ;; Position cursor at the overlay
      (goto-char start))

    ;; Wait for user decision via recursive-edit
    (setq mevedel--request-result 'pending)

    ;; Enter recursive edit - allows user input while blocking
    (unwind-protect
        (condition-case err
            (recursive-edit)
          ;; Treat quit (C-g) as a denial
          (quit (setq mevedel--request-result nil)
                (mevedel-abort))
          (error
           (user-error "%s" (error-message-string err))
           (setq mevedel--request-result nil)
           (mevedel-abort)))

      ;; Clean up overlay if still present
      (when (and ov (overlay-buffer ov))
        (let ((start (overlay-start ov))
              (end (overlay-end ov)))
          (delete-overlay ov)
          (delete-region start end))
        (when (eq mevedel--request-result 'pending)
          (setq mevedel--request-result nil))))

    ;; Return result (t for approved, nil for denied, (feedback . TEXT) for feedback)
    mevedel--request-result))

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

(defun mevedel-tools--agent-reminders-transform (fsm)
  "Prepend agent reminders to the current agent prompt buffer.

Mirrors `mevedel-reminders--transform' for spawned agents.  Looks up
the `mevedel-agent-invocation' via FSM's task overlay and collects any
reminders that should fire at the invocation's current turn count.

FSM is mandatory (not `&optional') so that
`gptel-prompt-transform-functions' dispatch — which inspects the
function's minimum arity — passes the FSM argument rather than invoking
the transform with zero arguments."
  (when-let* ((inv (mevedel-tools--agent-invocation-at fsm))
              (blocks (mevedel-reminders--collect-from
                       (mevedel-agent-invocation-reminders inv)
                       (mevedel-agent-invocation-turn-count inv)
                       inv)))
    (text-property-search-backward 'gptel nil t)
    (insert (string-join blocks "\n") "\n")))

(defun mevedel-tools--agent-turn-handler (fsm)
  "Terminal FSM handler: bump the agent invocation's turn count.

Parallel to the main-chat turn-count handler in
`mevedel-preset--build-handlers' but scoped to the per-invocation
counter on FSM's attached `mevedel-agent-invocation'."
  (when-let* ((inv (mevedel-tools--agent-invocation-at fsm)))
    (cl-incf (mevedel-agent-invocation-turn-count inv))))

(defun mevedel-tools--augment-agent-handlers (handlers extra)
  "Return a copy of HANDLERS with EXTRA appended to DONE and ERRS entries.

HANDLERS is an FSM handlers alist.  EXTRA is a handler function to add
to both terminal states (creating missing entries as needed).  The
original alist is not mutated."
  (let ((result (mapcar (lambda (entry) (cons (car entry) (copy-sequence (cdr entry))))
                        handlers)))
    (dolist (state '(DONE ERRS))
      (let ((entry (assq state result)))
        (if entry
            (setcdr entry (append (cdr entry) (list extra)))
          (push (list state extra) result))))
    result))

(defun mevedel-tools--agent-request-advice (orig-fun &rest args)
  "Around advice on `gptel-request' for agent reminder wiring.

When `mevedel-tools--agent-invocation' is bound (i.e., we are inside
`mevedel-tools--task'), appends the reminders transform to `:transforms',
augments the `:fsm' handlers with a terminal turn-count handler, and
stashes the invocation on the `:context' overlay so the transform and
handler can retrieve it later.  Outside agent dispatch this is a no-op."
  (if-let* ((inv mevedel-tools--agent-invocation))
      (let* ((prompt (car args))
             (kwargs (copy-sequence (cdr args)))
             (ov (plist-get kwargs :context))
             (fsm (plist-get kwargs :fsm))
             (existing-transforms (plist-get kwargs :transforms)))
        (setq kwargs
              (plist-put kwargs :transforms
                         (append existing-transforms
                                 (list #'mevedel-tools--agent-reminders-transform))))
        (when (overlayp ov)
          (overlay-put ov 'mevedel-agent-invocation inv))
        (when fsm
          (setf (gptel-fsm-handlers fsm)
                (mevedel-tools--augment-agent-handlers
                 (gptel-fsm-handlers fsm)
                 #'mevedel-tools--agent-turn-handler)))
        (apply orig-fun prompt kwargs))
    (apply orig-fun args)))

(advice-add 'gptel-request :around #'mevedel-tools--agent-request-advice)

(defun mevedel-tools--task (main-cb agent-type description prompt)
  "Call an agent to do specific compound tasks.

This is a thin wrapper around `gptel-agent--task' which manages the
entries in `mevedel-tools--agents-fsm'.  For agents defined via
`mevedel-define-agent', a fresh `mevedel-agent-invocation' is bound
around the call so the reminders transform and terminal turn-count
handler are wired into the spawned FSM.

MAIN-CB is the main callback to return a value to the main loop.
AGENT-TYPE is the name of the agent.
DESCRIPTION is a short description of the task.
PROMPT is the detailed prompt instructing the agent on what is required."
  (let* ((agent-id (concat agent-type "--" (md5 (format "%s%s%s%s" (system-name) (emacs-pid)
                                                        (current-time) (random)))))
         (agent (mevedel-agent-get agent-type))
         (mevedel-tools--agent-invocation
          (and agent (mevedel-agent-invocation-create agent)))
         (wrapped-callback
          (lambda (response &rest rest)
            (apply main-cb response rest)
            ;; Cleanup stale agent FSMs
            (setq mevedel-tools--agents-fsm
                  (cl-loop for (id . fsm) in mevedel-tools--agents-fsm
                           unless (eq (gptel-fsm-state fsm) 'DONE)
                           collect `(,id . ,fsm)))))
         (agent-fsm (gptel-agent--task wrapped-callback agent-type description prompt)))
    (overlay-put (plist-get (gptel-fsm-info agent-fsm) :context) 'mevedel-tools--agent-id agent-id)
    (setf (alist-get agent-id mevedel-tools--agents-fsm nil nil #'equal) agent-fsm)))


;;
;;; Todo List

(defvar-local mevedel-tools--todos nil
  "Alist mapping caller IDs to todo vectors.
Keys are agent-id strings for agents, nil for the main LLM.")

(defconst mevedel-tools--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))

(defun mevedel-toggle-todos ()
  "Toggle the display of the todo list."
  (interactive)
  (pcase-let ((`(,prop-value . ,ov)
               (or (get-char-property-and-overlay (point) 'mevedel-tools--todos)
                   (get-char-property-and-overlay
                    (previous-single-char-property-change
                     (point) 'mevedel-tools--todos nil (point-min))
                    'mevedel-tools--todos))))
    (if-let* ((fmt (overlay-get ov 'after-string)))
        (progn (overlay-put ov 'mevedel-tools--todos fmt)
               (overlay-put ov 'after-string nil))
      (overlay-put ov 'after-string
                   (and (stringp prop-value) prop-value))
      (overlay-put ov 'mevedel-tools--todos t))))

(defun mevedel-tools--todo-caller-id ()
  "Return the caller ID for the current TodoWrite/TodoRead call.
Scans `mevedel-tools--agents-fsm' for an agent FSM in TOOL state.
Returns the agent-id string if called from an agent, nil if from the
main LLM."
  (cl-loop for (id . fsm) in mevedel-tools--agents-fsm
           when (eq (gptel-fsm-state fsm) 'TOOL)
           return id))

(defun mevedel-tools--agent-display-name (agent-id)
  "Extract a display name from AGENT-ID.
Takes the part before \"--\" and capitalizes it. E.g.
\"explore--abc123\" -> \"Explore\"."
  (capitalize (car (split-string agent-id "--"))))

(defun mevedel-tools--format-todos-section (todos)
  "Format a single todo list section.
TODOS is a vector of plists with keys :content, :activeForm, and
:status. Returns (FORMATTED-STRING . IN-PROGRESS-ACTIVE-FORM)."
  (let ((formatted
         (mapconcat
          (lambda (todo)
            (pcase (plist-get todo :status)
              ("completed"
               (concat "✓ " (propertize (plist-get todo :content)
                                        'face '(:inherit success :strike-through t))))
              ("in_progress"
               (concat "→ " (propertize (plist-get todo :activeForm)
                                        'face '(:inherit bold :inherit warning))))
              (_ (concat "○ " (plist-get todo :content)))))
          todos "\n"))
        (in-progress
         (cl-loop for todo across todos
                  when (equal (plist-get todo :status) "in_progress")
                  return (plist-get todo :activeForm))))
    (cons formatted in-progress)))

(defun mevedel-tools--todo-cleanup-stale ()
  "Remove todo entries for agents whose context overlay has been deleted."
  (setq mevedel-tools--todos
        (cl-remove-if
         (lambda (entry)
           (when-let* ((id (car entry))
                       (fsm (alist-get id mevedel-tools--agents-fsm nil nil #'equal)))
             (let ((ctx-ov (plist-get (gptel-fsm-info fsm) :context)))
               (or (null ctx-ov)
                   (null (overlay-buffer ctx-ov))))))
         mevedel-tools--todos)))

(defvar-local mevedel-tools--prev-todo-ov nil)

(defun mevedel-tools--display-todo-overlay ()
  "Display a formatted task list in the buffer using an overlay.
Reads from the `mevedel-tools--todos' alist.  When multiple contexts
have todos, section headers are shown for each context."
  (mevedel-tools--todo-cleanup-stale)
  (let* ((info (gptel-fsm-info gptel--fsm-last))
         (where-from
          (previous-single-property-change
           (plist-get info :tracking-marker) 'gptel nil (point-min)))
         (where-to (plist-get info :tracking-marker)))
    (unless (= where-from where-to)
      (pcase-let ((`(,_ . ,todo-ov)
                   (or (cons nil mevedel-tools--prev-todo-ov)
                       (get-char-property-and-overlay where-from 'mevedel-tools--todos))))
        (if todo-ov
            (move-overlay todo-ov where-from where-to)
          (setq todo-ov (make-overlay where-from where-to nil t))
          (overlay-put todo-ov 'mevedel-tools--todos t)
          (overlay-put todo-ov 'evaporate t)
          (overlay-put todo-ov 'priority -40)
          (overlay-put todo-ov 'keymap (define-keymap
                                         "<tab>" #'mevedel-toggle-todos
                                         "TAB"   #'mevedel-toggle-todos))
          (setq mevedel-tools--prev-todo-ov todo-ov))

        ;; Build combined display from all active todo entries
        (let* ((active-entries
                (cl-remove-if (lambda (e) (= 0 (length (cdr e)))) mevedel-tools--todos))
               (sorted-entries
                (sort active-entries
                      (lambda (a b)
                        (cond ((null (car a)) t)
                              ((null (car b)) nil)
                              (t (string< (mevedel-tools--agent-display-name (car a))
                                          (mevedel-tools--agent-display-name (car b))))))))
               (multi-context (> (length sorted-entries) 1))
               (body
                (mapconcat
                 (lambda (entry)
                   (pcase-let* ((`(,id . ,todos) entry)
                                (`(,formatted . ,_)
                                 (mevedel-tools--format-todos-section todos)))
                     (if multi-context
                         (concat "\n"
                                 (propertize
                                  (concat "── "
                                          (if id
                                              (mevedel-tools--agent-display-name id)
                                            "Main")
                                          " ──")
                                  'face 'font-lock-comment-face)
                                 "\n" formatted)
                       formatted)))
                 sorted-entries "\n"))
               (todo-display
                (concat
                 (unless (= (char-before (overlay-end todo-ov)) 10) "\n")
                 mevedel-tools--hrule
                 (propertize "Current Tasks: [ "
                             'face '(:inherit font-lock-comment-face :inherit bold))
                 (save-excursion
                   (goto-char (1- (overlay-end todo-ov)))
                   (propertize (substitute-command-keys "\\[mevedel-toggle-todos]")
                               'face 'help-key-binding))
                 (propertize " to toggle display ]\n" 'face 'font-lock-comment-face)
                 body "\n"
                 mevedel-tools--hrule)))
          (overlay-put todo-ov 'after-string todo-display))))))

(defun mevedel-tools--write-todo (todos)
  "Display a formatted task list in the buffer.

TODOS is a list of plists with keys :content, :activeForm, and :status.
Completed items are displayed with strikethrough and shadow face.
Exactly one item should have status \"in_progress\"."
  (mevedel-tools--validate-params nil mevedel-tools--write-todo
    (todos (vectorp . "array")))
  (let ((caller (mevedel-tools--todo-caller-id)))
    (setf (alist-get caller mevedel-tools--todos nil nil #'equal) todos)
    (mevedel-tools--display-todo-overlay))
  t)

(defun mevedel-tools--read-todo ()
  "Display a formatted task list in the buffer."
  (mevedel-tools--display-todo-overlay)
  (alist-get (mevedel-tools--todo-caller-id) mevedel-tools--todos nil nil #'equal))


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
         (chat-buffer (current-buffer))
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
             (delete-region (overlay-start overlay) (overlay-end overlay))
             (delete-overlay overlay))
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
               (delete-region (overlay-start overlay) (overlay-end overlay))
               (delete-overlay overlay))

             ;; Create new overlay with keymap
             (with-current-buffer chat-buffer
               (goto-char (point-max))
               (let ((start (point))
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
             (delete-region (overlay-start overlay) (overlay-end overlay))
             (delete-overlay overlay))

           (with-current-buffer chat-buffer
             (goto-char (point-max))
             (let ((start (point))
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
             (delete-region (overlay-start overlay) (overlay-end overlay))
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
:description, and :prompt."
  (let ((agent-type (plist-get args :subagent_type))
        (description (plist-get args :description))
        (prompt (plist-get args :prompt)))
    (unless (stringp agent-type)
      (error "Parameter subagent_type is required"))
    (unless (stringp description)
      (error "Parameter description is required"))
    (unless (stringp prompt)
      (error "Parameter prompt is required"))
    (mevedel-tools--task callback agent-type description prompt)))

(defun mevedel-tool-ui--tool-search (callback args)
  "Search for and load deferred tools.
CALLBACK receives the search results.  ARGS is a plist with :query
and optional :load."
  (let ((query (plist-get args :query))
        (load (plist-get args :load)))
    (unless (stringp query)
      (error "Parameter query is required"))
    (mevedel-tools--tool-search callback query load)))


;;
;;; Register Tools

(defun mevedel-tool-ui--register ()
  "Register user interaction tools."

  ;; TodoWrite and TodoRead removed: will be reimplemented as
  ;; mevedel-define-tool in spec 13 (task system).  Handler functions
  ;; (mevedel-tools--write-todo, mevedel-tools--read-todo) and overlay
  ;; display code are kept for reuse.

  (mevedel-define-tool
    :name "Ask"
    :description "Ask the user one or more questions and wait for their responses."
    :prompt-file "tools/ask.md"
    :handler #'mevedel-tool-ui--ask
    :args ((questions array :required
                     "Array of question objects. Each question must have predefined answer options."))
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
                  "The detailed task for the agent to perform autonomously."))
    :async-p t
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
  (let* ((info (gptel-fsm-info gptel--fsm-last))
         (position (plist-get info :tracking-marker))
         (start position)
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
                   "\n")))
    (save-excursion
      (goto-char (or position (point-max)))
      (setq start (point))
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
       start (point) 'font-lock-face (gptel-agent--block-bg)))
    (goto-char start)
    ;; Block until user responds
    (setq mevedel--permission-result nil)
    (unwind-protect
        (condition-case _err
            (recursive-edit)
          (quit (setq mevedel--permission-result 'deny-once)
                (mevedel-abort)))
      (when (and ov (overlay-buffer ov))
        (let ((s (overlay-start ov))
              (e (overlay-end ov)))
          (delete-overlay ov)
          (delete-region s e))
        (when (null mevedel--permission-result)
          (setq mevedel--permission-result 'deny-once))))
    mevedel--permission-result))

(provide 'mevedel-tool-ui)

;;; mevedel-tool-ui.el ends here
