;;; mevedel-tool-plan.el -- Planning tool -*- lexical-binding: t -*-

;;; Commentary:

;; Implementation plan creation and interactive presentation.  The
;; CreatePlan tool delegates to the planner agent, and PresentPlan
;; displays the plan inline with interactive controls for the user.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `gptel-agent'
(defvar gptel-agent--agents)

;; `gptel-agent-tools'
(declare-function gptel-agent--fontify-block "ext:gptel-agent-tools" (path-or-mode start end))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))
(declare-function mevedel--plans-directory "mevedel-chat" ())
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(defvar mevedel--pending-plan-action)

;; `mevedel-agents'
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))

;; `mevedel-tool-ui'
(declare-function mevedel-tools--task "mevedel-tool-ui" (callback agent-type description prompt))


;;
;;; Planning

(defun mevedel-tools--create-plan (callback description prompt)
  "Launch the planner agent to create an implementation plan.

CALLBACK is the async callback function.
DESCRIPTION is a short description of the planning task.
PROMPT is the detailed prompt for the planner agent."
  (mevedel-tools--validate-params callback mevedel-tools--create-plan
    (description (stringp . "string"))
    (prompt (stringp . "string")))
  ;; Ensure the planner agent spec is registered buffer-locally
  (unless (assoc-string "planner" gptel-agent--agents)
    (when-let* ((agent (mevedel-agent-get "planner")))
      (setq-local gptel-agent--agents
                  (append gptel-agent--agents
                          (list (mevedel-agent-to-gptel-spec agent))))))
  (mevedel-tools--task callback "planner" description prompt))

(defun mevedel-tools--post-tool-plan-intercept (info)
  "Intercept tool completion to trigger plan implementation.

When `mevedel--pending-plan-action' is set (by PresentPlan), this hook
stops the current request and schedules `mevedel--implement-plan' to
fire the implementation request directly.

INFO is a plist with tool call details, as specified by
`gptel-post-tool-call-functions'."
  (ignore info)
  (when-let* ((action-plist mevedel--pending-plan-action))
    (setq mevedel--pending-plan-action nil)
    ;; Schedule implementation after FSM cleanup
    (let ((buf (current-buffer)))
      (run-at-time 0 nil (lambda ()
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (mevedel--implement-plan action-plist))))))
    ;; Stop the main FSM
    (list :stop t :stop-reason "Implementing accepted plan")))

(cl-defun mevedel-tools--present-plan (callback plan)
  "Present PLAN to user for interactive feedback.

CALLBACK is the async callback function to call with user response.
PLAN is a plist with :title, :summary, and :sections keys.

The user can:
- Implement the plan (with full conversation context)
- Implement with clear context (fresh request)
- Provide feedback to revise the plan
- Abort planning entirely"
  (mevedel-tools--validate-params callback mevedel-tools--present-plan
    (plan (listp . "object")))

  (let* ((chat-buffer (current-buffer))
         (overlay nil)
         (title (or (plist-get plan :title) "Untitled Plan"))
         (summary (or (plist-get plan :summary) "No summary provided"))
         (sections (append (plist-get plan :sections) nil))
         (plan-markdown (concat
                         "# Plan: " title "\n\n"
                         "## Summary\n"
                         summary "\n\n"
                         (mapconcat
                          (lambda (section)
                            (let ((heading (or (plist-get section :heading) "Unnamed Section"))
                                  (content (or (plist-get section :content) "No content"))
                                  (type (or (plist-get section :type) "step")))
                              (format "## %s `[%s]`\n%s\n" heading type content)))
                          sections
                          "\n"))))

    (cl-labels
        ((save-plan
           ()
           "Save plan to file and return filepath."
           (let* ((plans-dir (with-current-buffer chat-buffer
                               (mevedel--plans-directory)))
                  (filename (format "plan-%s.md" (format-time-string "%Y%m%d-%H%M%S")))
                  (filepath (expand-file-name filename plans-dir)))
             (write-region plan-markdown nil filepath nil 'silent)
             filepath))

         (implement-plan
           ()
           "Implement plan with full conversation context."
           (interactive)
           (condition-case err
               (let ((filepath (save-plan)))
                 (with-current-buffer chat-buffer
                   (setq mevedel--pending-plan-action
                         (list :action 'implement
                               :plan-file filepath
                               :plan-markdown plan-markdown)))
                 (cleanup-and-return
                  (format "User accepted the plan and chose to implement it.\n\nPlan saved to: %s"
                          filepath)))
             (error
              (cleanup-and-return
               (format "User accepted the plan, but failed to save to file: %S\n\nHere is the plan:\n\n%s"
                       err plan-markdown)))))

         (implement-plan-clear
           ()
           "Implement plan with clear context (fresh request)."
           (interactive)
           (condition-case err
               (let ((filepath (save-plan)))
                 (with-current-buffer chat-buffer
                   (setq mevedel--pending-plan-action
                         (list :action 'implement-clear
                               :plan-file filepath
                               :plan-markdown plan-markdown)))
                 (cleanup-and-return
                  (format "User accepted the plan and chose to implement with clear context.\n\nPlan saved to: %s"
                          filepath)))
             (error
              (cleanup-and-return
               (format "User accepted the plan, but failed to save to file: %S\n\nHere is the plan:\n\n%s"
                       err plan-markdown)))))

         (reject-plan-feedback
           ()
           "User rejects plan with feedback."
           (interactive)
           (let ((feedback (read-string "Feedback on this plan: ")))
             (cleanup-and-return
              (format "User rejected the plan.\n\nFeedback: %s\n\nOriginal plan:\n%s\n\nPlease revise the plan addressing this feedback."
                      feedback plan-markdown))))

         (abort-plan
           ()
           "Abort planning tool."
           (interactive)
           (cleanup-and-return
            "User aborted planning tool.")
           (mevedel-abort))

         (cleanup-and-return
           (result)
           "Clean up overlay and return RESULT to callback."
           (when overlay
             (let ((inhibit-read-only t))
               (delete-region (overlay-start overlay) (overlay-end overlay))
               (delete-overlay overlay)))
           (funcall callback result)))

      ;; Build plan display in markdown
      (let* ((keymap (make-sparse-keymap))
             (start (point-max)))

        ;; Insert plan markdown
        (with-current-buffer chat-buffer
          (save-excursion
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert "\n")
              (insert (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t)))
              (let ((content-start (point)))
                (insert "\n" plan-markdown "\n")
                ;; Apply markdown syntax highlighting
                (gptel-agent--fontify-block 'markdown-mode content-start (point))
                ;; Apply background color
                (font-lock-append-text-property
                 content-start (point) 'font-lock-face (gptel-agent--block-bg)))
              (insert "\n\n")
              (insert (propertize "Keys: " 'font-lock-face 'help-key-binding))
              (insert (propertize "RET" 'font-lock-face 'help-key-binding))
              (insert " implement  ")
              (insert (propertize "I" 'font-lock-face 'help-key-binding))
              (insert " implement (clear context)  ")
              (insert (propertize "f" 'font-lock-face 'help-key-binding))
              (insert " feedback  ")
              (insert (propertize "q" 'font-lock-face 'help-key-binding))
              (insert " abort\n")
              (insert (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t))))))

        ;; Create overlay for interactivity
        (setq overlay (make-overlay start (point-max) chat-buffer))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'mevedel-plan t)

        ;; Define keybindings
        (define-key keymap (kbd "RET") #'implement-plan)
        (define-key keymap (kbd "<return>") #'implement-plan)
        (define-key keymap (kbd "i") #'implement-plan)
        (define-key keymap (kbd "C-c C-c") #'implement-plan)
        (define-key keymap (kbd "I") #'implement-plan-clear)
        (define-key keymap (kbd "f") #'reject-plan-feedback)
        (define-key keymap (kbd "q") #'abort-plan)
        (define-key keymap (kbd "C-c C-k") #'abort-plan)
        (define-key keymap (kbd "C-g") #'abort-plan)
        (overlay-put overlay 'keymap keymap)

        ;; Focus user attention and enter recursive-edit to catch C-g
        (with-current-buffer chat-buffer
          (goto-char (point-max))
          (goto-char start)
          (when-let* ((buf-win (get-buffer-window chat-buffer)))
            (with-selected-window buf-win
              (recenter-top-bottom 1)))
          (condition-case err
              ;; Wait for user action
              (recursive-edit)
            ;; C-g pressed - abort entire session
            (quit
             ;; Clean up overlay
             (when overlay
               (let ((inhibit-read-only t))
                 (delete-region (overlay-start overlay) (overlay-end overlay))
                 (delete-overlay overlay)))
             (mevedel-abort))
            (error
             (user-error "%s" (error-message-string err))
             ;; Clean up overlay on error
             (when overlay
               (let ((inhibit-read-only t))
                 (delete-region (overlay-start overlay) (overlay-end overlay))
                 (delete-overlay overlay)))
             (mevedel-abort))))))))


;;
;;; Pipeline-compatible handlers

(defun mevedel-tool-plan--present (callback args)
  "Present a plan for user feedback.
CALLBACK receives the user response.  ARGS is a plist with :plan."
  (let ((plan (plist-get args :plan)))
    (unless plan
      (error "Parameter plan is required"))
    (mevedel-tools--present-plan callback plan)))

(defun mevedel-tool-plan--create (callback args)
  "Launch the planner agent to create a plan.
CALLBACK receives the result.  ARGS is a plist with :description
and :prompt."
  (let ((description (plist-get args :description))
        (prompt (plist-get args :prompt)))
    (unless (stringp description)
      (error "Parameter description is required"))
    (unless (stringp prompt)
      (error "Parameter prompt is required"))
    (mevedel-tools--create-plan callback description prompt)))


;;; Tool registration

(defun mevedel-tool-plan--register ()
  "Register planning tools (PresentPlan, CreatePlan)."

  (mevedel-define-tool
    :name "PresentPlan"
    :description "Present an implementation plan to the user and wait for feedback."
    :prompt-file "tools/presentplan.md"
    :handler #'mevedel-tool-plan--present
    :args ((plan object :required
                "The plan object with title, summary, and sections."))
    :async-p t)

  (mevedel-define-tool
    :name "CreatePlan"
    :description "Launch the planner agent to create an implementation plan."
    :prompt-file "tools/createplan.md"
    :handler #'mevedel-tool-plan--create
    :args ((description string :required
                       "A short (3-5 word) description of what is being planned.")
           (prompt string :required
                  "Detailed prompt for the planner: what needs to be implemented, constraints, requirements."))
    :async-p t))

(provide 'mevedel-tool-plan)
;;; mevedel-tool-plan.el ends here
