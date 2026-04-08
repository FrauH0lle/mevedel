;;; mevedel-tool-ui.el -- User interaction tools -*- lexical-binding: t -*-

;;; Commentary:

;; User-facing tool UI: directory access requests, agent delegation,
;; todo list display, and the Ask questionnaire.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `gptel-agent-tools'
(declare-function gptel-agent--task "ext:gptel-agent-tools" (main-cb agent-type description prompt))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)

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

(defun mevedel-tools--task (main-cb agent-type description prompt)
  "Call an agent to do specific compound tasks.

This is a thin wrapper around `gptel-agent--task' which manages the
entries in `mevedel-tools--agents-fsm'.

MAIN-CB is the main callback to return a value to the main loop.
AGENT-TYPE is the name of the agent.
DESCRIPTION is a short description of the task.
PROMPT is the detailed prompt instructing the agent on what is required."
  (let* ((agent-id (concat agent-type "--" (md5 (format "%s%s%s%s" (system-name) (emacs-pid)
                                                        (current-time) (random)))))
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
\"researcher--abc123\" -> \"Researcher\"."
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
;;; Register Tools

(defun mevedel-tool-ui--register ()
  "Register user interaction tools."

  (gptel-make-tool
   :name "TodoWrite"
   :description "Create and manage a structured task list for your current session.
Helps track progress and organize complex tasks. Use proactively for
multi-step work.

### When to use `TodoWrite`

Use this tool in any of the following scenarios:

- Task has 3+ distinct steps or phases
- Task will span multiple responses or tool calls
- Task requires careful planning or coordination
- You receive new instructions with multiple requirements
- Work might benefit from tracking progress
- After completing a task. Mark it as completed and add any new follow-up tasks

### When NOT to use `TodoWrite`

- Single, straightforward tasks (one clear action)
- Trivial tasks with no organizational benefit
- Tasks completable in less than 3 steps
- Purely conversational or informational requests
- User provides a simple question requiring a simple answer

### Examples of good usage

<example>
User: I want to create an authentication system for my application. Please add tests and run them afterwards.
Assistant: *Creates a todo list to track the implementation*
TodoWrite(todos=[
  {content: \"Read and analyze existing authentication code\", status: \"pending\", activeForm: \"Reading authentication code\"},
  {content: \"Design new JWT token structure\", status: \"pending\", activeForm: \"Designing JWT structure\"},
  {content: \"Implement token generation and validation\", status: \"pending\", activeForm: \"Implementing token generation\"},
  {content: \"Add unit tests for authentication\", status: \"pending\", activeForm: \"Adding authentication tests\"}
])

<reasoning>
The assistant used the todo list because:
1. Adding authentication is a multi-step feature
2. The user explicitly requested tests
</reasoning>
</example>

### Examples of bad usage

<example>
TodoWrite(todos=[
  {content: \"Fix typo in README\", status: \"in_progress\", activeForm: \"Fixing typo\"}
])
<reasoning>
Single task doesn't need a todo list.
</reasoning>
</example>

### Task management

1. **Task States:**
   - `pending`: Task not yet started
   - `in_progress`: Currently working on (exactly one at a time)
   - `completed`: Task finished successfully

2. **Task management**
   - Always update task status in real-time as you work
   - Mark tasks completed IMMEDIATELY after finishing (don't batch completions)
   - Exactly ONE task must be `in_progress` at any time
   - Complete current tasks before starting new ones
   - Send entire todo list with each call (not just changed items)

3. **Task Completion**
   - ONLY mark completed when FULLY accomplished - if errors occur, keep
     as in_progress
   - When blocked, create a new task describing what needs to be resolved
   - Never mark a task as completed if:
     - Tests are failing
     - Implementation is partial
     - You encountered unresolved errors
     - You couldn't find necessary files or dependencies

4. **Task Breakdown**
   - Create specific, actionable items
   - Break complex tasks into smaller, manageable steps
   - Use clear, descriptive task names
   - Always provide both `content` (imperative: \"Run tests\") and
     `activeForm` (present continuous: \"Running tests\")

When in doubt, use this tool. Being proactive with task management
demonstrates attentiveness and ensures you complete all requirements
successfully.
"
   :function #'mevedel-tools--write-todo
   :args
   '((:name "todos"
      :description "The updated todo list"
      :type array
      :items
      (:type object
       :properties
       (:content
        (:type string :minLength 1
         :description "Imperative form describing what needs to be done (e.g., 'Run tests')")
        :status
        (:type string
         :enum ["pending" "in_progress" "completed"]
         :description "Task status: pending, in_progress (exactly one), or completed")
        :activeForm
        (:type string :minLength 1
         :description "Present continuous form shown during execution (e.g., 'Running tests')")))))
   :category "mevedel")

  (gptel-make-tool
   :name "TodoRead"
   :description "Use this tool to read the current to-do list for the session.
This tool should be used proactively and frequently to ensure that you
are aware of the status of the current task list.

### When to use `TodoRead`

You should make use of this tool as often as possible, especially in the
following situations:

- At the beginning of conversations to see what's pending
- Before starting new tasks to prioritize work
- When the user asks about previous tasks or plans
- Whenever you're uncertain about what to do next
- After completing tasks to update your understanding of remaining work
- After every few messages to ensure you're on track

### How to use `TodoRead`

- This tool takes in no parameters. So leave the input blank or empty.
  DO NOT include a dummy object, placeholder string or a key like
  \"input\" or \"empty\". LEAVE IT BLANK.
- Returns a list of todo items with their status and content
- Use this information to track progress and plan next steps
- If no todos exist yet, an empty list will be returned

### Examples of good usage

<example>
- Check what tasks are pending before continuing work
TodoRead()
</example>

### Examples of bad usage

<example>
- Calling TodoRead() multiple times in the same response without taking action
<reasoning>
Only call it once when you need to check status.
</reasoning>
</example>
"
   :function #'mevedel-tools--read-todo
   :args nil
   :category "mevedel")

  ;; Tool for LLM to ask user questions during execution
  (gptel-make-tool
   :name "Ask"
   :function #'mevedel-tools--ask-user
   :description "Ask the user one or more questions and wait for their responses.
Use this when you need clarification or user input to proceed with a
task.

Supports multiple questions in a single call with navigation between them.

Each question MUST provide predefined answer options. Users can always
provide custom input.

### When to use `Ask`

- You need user input or clarification to proceed
- Multiple implementation approaches exist and user should decide
- Gathering user preferences or requirements
- Making decisions that affect the outcome significantly
- User needs to choose between trade-offs

### When NOT to use `Ask`

- You can make a reasonable default choice
- The question is trivial or has an obvious answer
- You're overthinking and should just proceed

### How to use `Ask`

- Can ask multiple related questions in one call (better than separate calls)
- Each question MUST provide predefined answer options
- The tool automatically presents a custom input option to users; do NOT include
  a 'custom', 'other' or similar choice in your options list
- Questions are presented one at a time with navigation:
  - Users can go back to previous questions
  - Users can edit answers before submitting
  - Final confirmation screen shows all answers for review
- Format questions clearly and make options concise
- Provide 2-4 good default options per question

### Examples of good usage

<example>
Ask(questions=[{question: \"Which authentication method should we use?\", options: [\"JWT\", \"Session cookies\", \"OAuth2\"]}])
</example>

<example>
Ask(questions=[{question: \"How should errors be handled?\", options: [\"Return null\", \"Throw exception\", \"Return Result type\"]}])
</example>

### Examples of bad usage

<example>
Ask(questions=[{question: \"Should I continue?\", options: [\"Yes\", \"No\"]}])
<reasoning>
Just proceed instead of asking for permission to continue.
</reasoning>
</example>

<example>
Ask(questions=[{question: \"What should we name this variable?\", options: [\"data\", \"result\", \"output\"]}])
<reasoning>
Make reasonable naming choices without asking.
</reasoning>
</example>

<example>
Ask(questions=[{question: \"Which framework was mentioned earlier?\", options: [\"React\", \"Vue\", \"Angular\"]}])
<reasoning>
The answer is already in the conversation - review it instead.
</reasoning>
</example>
"
   :args '((:name "questions"
            :type array
            :items (:type object
                    :properties (:question (:type string
                                            :description "The question text to display")
                                           :options (:type array
                                                     :items (:type string)
                                                     :description "Predefined answer choices (user can also provide custom input)")))
            :description "Array of question objects. Each question must have predefined answer options."))
   :async t
   :include t
   :category "mevedel")

  ;; Tool for LLM to request access to new directories
  (gptel-make-tool
   :name "RequestAccess"
   :function #'mevedel--tools-request-dir-access
   :description "Request access to a directory outside the current allowed project roots.
You must explain why you need access to this directory.

### When to use `RequestAccess`

- Need to access files outside the current workspace
- Working with configuration files in user's home directory
- Accessing shared libraries or dependencies
- Reading files from system directories

### When NOT to use `RequestAccess`

- Files are already within the workspace
- You haven't tried accessing the file yet (try first, then request if denied)

### How to use `RequestAccess`

- Provide the directory path you need to access
- Provide a clear reason explaining why access is needed
- User will approve or deny the request
- After approval, you can use Read, Write, Edit tools on files in that directory

### Examples of good usage

<example>
- Access home directory config
RequestAccess(directory=\"~/.config\", reason=\"Need to read user's git configuration to understand repository settings\")
</example>

<example>
- Access system library
RequestAccess(directory=\"/usr/local/lib/mylib\", reason=\"Need to check library version for compatibility analysis\")
</example>

### Examples of bad usage

<example>
- Requesting workspace directory
RequestAccess(directory=\".\", reason=\"Need to read files\")
<reasoning>
Workspace is already accessible, no need to request.
</reasoning>
</example>
"
   :args '((:name "directory"
            :type string
            :description "Absolute or relative path to the directory you need to access")
           (:name "reason"
            :type string
            :description "Clear explanation of why you need access to this directory and what you plan to do there"))
   :async t
   :confirm nil  ;; Confirmation handled within the tool
   :include t
   :category "mevedel")

  (gptel-make-tool
   :name "Agent"
   :description "Launch a specialized agent to handle complex, multi-step tasks
autonomously.

Agents run independently and return results in one message. Use for
open-ended searches, complex research, or when uncertain about finding
results in first few tries."
   :function #'mevedel-tools--task
   :args '((:name "subagent_type"
            :type string
            :enum ["researcher" "introspector"]
            :description "The type of specialized agent to use for this task")
           (:name "description"
            :type string
            :description "A short (3-5 word) description of the task")
           (:name "prompt"
            :type "string"
            :description "The detailed task for the agent to perform autonomously.  \
Should include exactly what information the agent should return."))
   :category "mevedel"
   :async t
   :confirm t
   :include t)

  (gptel-make-tool
   :name "ToolSearch"
   :description "Search for and load additional tools that are not currently available.

Some tools are deferred (not loaded by default) to save context.
Use ToolSearch to discover and activate them when needed.

### When to use `ToolSearch`

- When you need a capability that isn't available in your current tool set
- When a task requires tools beyond basic reading and searching
- At the start of complex tasks to check what specialized tools exist

### How to use `ToolSearch`

1. Call with a query to search by name or capability
2. Review the results to see what's available
3. Call again with load=true to activate matching tools

### Examples

<example>
ToolSearch(query=\"xref\", load=true)
→ Loads XrefReferences and XrefDefinitions tools
</example>

<example>
ToolSearch(query=\"edit\")
→ Shows available editing tools without loading them
</example>"
   :function #'mevedel-tools--tool-search
   :args '((:name "query"
            :type string
            :description "Search query: tool name or capability description.
Examples: \"xref\", \"edit\", \"treesitter\", \"code navigation\"")
           (:name "load"
            :type boolean
            :optional t
            :description "If true, load matching tools for immediate use on the next turn."))
   :category "mevedel"
   :async t))

(provide 'mevedel-tool-ui)

;;; mevedel-tool-ui.el ends here
