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

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))
(declare-function mevedel--plans-directory "mevedel-chat" ())
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(defvar mevedel--pending-plan-action)

;; `mevedel-agents'
(defvar mevedel-agents--planner-spec)

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
    (setq-local gptel-agent--agents
                (append gptel-agent--agents (list mevedel-agents--planner-spec))))
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
;;; Tool registration

(defun mevedel-tool-plan--register ()
  "Register planning tools (PresentPlan, CreatePlan)."

  ;; Tool for presenting interactive implementation plans
  (gptel-make-tool
   :name "PresentPlan"
   :function #'mevedel-tools--present-plan
   :description "Present an implementation plan to the user and wait for feedback.

**IMPORTANT**: This MUST be your FINAL tool call. Do not call any other
  tools or add text after this.

Use this tool after drafting a plan to get user approval. The plan will
be displayed inline in the chat buffer with interactive controls, and
user feedback will be returned automatically.

User can:
- Implement the plan (begins implementation automatically)
- Implement with clear context (fresh request without conversation history)
- Reject the plan with feedback (you revise and call PresentPlan again)

This tool handles all user interaction - treat it as your exit point.
When the user chooses to implement, the plan is saved and implementation
starts automatically - no further action is needed from you.

### When to use `PresentPlan`

- After drafting an implementation plan that needs user approval
- When presenting multiple implementation approaches for user to choose
- Before proceeding with complex multi-file changes
- User explicitly requested to see a plan first
- Plan involves architectural decisions or tradeoffs

### When NOT to use `PresentPlan`

- Simple single-file changes that don't need planning
- User already approved approach in conversation
- Task is obvious and low-risk
- You're not in the planner agent context

### How to use `PresentPlan`

- **CRITICAL**: This MUST be your FINAL tool call - do not call any
    other tools after this
- **CRITICAL**: Do not add any text after calling PresentPlan - it
    handles all user interaction
- Structure plan hierarchically with clear sections
- Use section types: 'step' (default), 'risk', 'alternative', 'dependency'
- Include specific file paths and line numbers where possible
- Mark dependencies between steps clearly
- Be concise but comprehensive

### Response handling

- If user implements: Your task is complete, implementation starts
  automatically
- If rejected: You receive user feedback + original plan; revise and
  call PresentPlan again
- You can call PresentPlan multiple times to iterate until plan is
  accepted
- Think of PresentPlan as an 'exit' command that terminates your
  planning session

### Plan structure example

{
  \"title\": \"Implementation Plan: Add Authentication\",
  \"summary\": \"Add JWT-based auth with user registration and login\",
  \"sections\": [
    {
      \"heading\": \"Phase 1: Database Schema\",
      \"content\": \"Create users table in db/schema.sql...\",
      \"type\": \"step\"
    },
    {
      \"heading\": \"Risk: Password Storage\",
      \"content\": \"Must use bcrypt with cost 12+...\",
      \"type\": \"risk\"
    }
  ]
}

### Examples of good usage

<example>
- Presenting a complex feature plan:
PresentPlan({
  \"title\": \"Add User Profile System\",
  \"summary\": \"Implement user profiles with avatar upload and bio editing\",
  \"sections\": [
    {
      \"heading\": \"Database Migration\",
      \"content\": \"Create profiles table in migrations/2024-01-15-add-profiles.sql\",
      \"type\": \"step\"
    },
    {
      \"heading\": \"Avatar Upload Risk\",
      \"content\": \"Need file size limits and virus scanning for security\",
      \"type\": \"risk\"
    }
  ]
})
</example>

### Examples of bad usage

<example>
- Using for simple one-line changes:
PresentPlan({
  \"title\": \"Fix Typo\",
  \"summary\": \"Change 'recevied' to 'received' in README.md\",
  \"sections\": [{\"heading\": \"Edit typo\", \"content\": \"Fix spelling error\", \"type\": \"step\"}]
})
<reasoning>
Should just make the edit directly without a plan.
</reasoning>
</example>
"
   :args
   '((:name "plan"
      :type object
      :description "The plan object with title, summary, and sections"
      :properties
      (:title
       (:type string
        :description "Plan title (e.g., 'Implementation Plan: Add Dark Mode')")
       :summary
       (:type string
        :description "Brief 1-2 sentence overview of the plan")
       :sections
       (:type array
        :description "Ordered sections of the plan"
        :items
        (:type object
         :properties
         (:heading
          (:type string
           :description "Section heading")
          :content
          (:type string
           :description "Section content in markdown")
          :type
          (:type string
           :enum ["step" "risk" "alternative" "dependency"]
           :optional t
           :description "Section type")))))))
   :async t
   :category "mevedel")

  ;; Tool to launch the planner agent for creating implementation plans
  (gptel-make-tool
   :name "CreatePlan"
   :function #'mevedel-tools--create-plan
   :description "Launch the planner agent to create an implementation plan.

Use this tool when the task is complex enough to warrant planning before
implementation.  The planner agent will explore the codebase, draft a
structured plan, and present it to the user for approval.

After the user approves the plan, implementation begins automatically -
you do not need to do anything further.

### When to use `CreatePlan`

- Complex multi-file changes that benefit from upfront planning
- Architectural decisions or significant refactoring
- User explicitly asks for a plan before implementation
- Task involves tradeoffs that should be discussed first

### When NOT to use `CreatePlan`

- Simple, obvious changes (single file edits, typo fixes)
- User has already described exactly what to do
- Task is straightforward with no ambiguity

### How it works

1. You call CreatePlan with a description and detailed prompt
2. The planner agent explores the codebase and drafts a plan
3. The plan is presented to the user for approval
4. If approved, the plan is implemented automatically
5. You do NOT need to implement the plan yourself after this tool returns
"
   :args '((:name "description"
            :type string
            :description "A short (3-5 word) description of what is being planned")
           (:name "prompt"
            :type string
            :description "Detailed prompt for the planner: what needs to be implemented, \
constraints, requirements, and any context the planner should consider."))
   :category "mevedel"
   :async t
   :confirm t
   :include t))

(provide 'mevedel-tool-plan)
;;; mevedel-tool-plan.el ends here
