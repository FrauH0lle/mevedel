;;; mevedel-tool-plan.el -- Planning tool -*- lexical-binding: t -*-

;;; Commentary:

;; Implementation plan creation and interactive presentation.  The
;; CreatePlan tool delegates to the planner agent, and PresentPlan
;; displays the plan inline with interactive controls for the user.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `gptel-agent-tools'
(declare-function gptel-agent--fontify-block "ext:gptel-agent-tools" (path-or-mode start end))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `mevedel-agent-exec'
(defvar mevedel-agent-exec--agents)

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
(declare-function mevedel--prompt--register-canceller "mevedel-tool-ui" ())
(declare-function mevedel--prompt--settle "mevedel-tool-ui" (overlay outcome))
(defvar mevedel--prompt-overlays)

;; `mevedel-view'
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())


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
  ;; Ensure the planner agent spec is registered buffer-locally.
  (unless (assoc-string "planner" mevedel-agent-exec--agents)
    (when-let* ((agent (mevedel-agent-get "planner"))
                (spec (mevedel-agent-to-gptel-spec agent)))
      (setq-local mevedel-agent-exec--agents
                  (append mevedel-agent-exec--agents (list spec)))))
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

(defun mevedel-tools--plan--save (plan-markdown chat-buffer)
  "Persist PLAN-MARKDOWN under the workspace's plans directory.
Returns the absolute path of the written file.  CHAT-BUFFER is the
chat buffer whose workspace owns the directory."
  (let* ((plans-dir (with-current-buffer chat-buffer
                      (mevedel--plans-directory)))
         (filename (format "plan-%s.md" (format-time-string "%Y%m%d-%H%M%S")))
         (filepath (expand-file-name filename plans-dir)))
    (write-region plan-markdown nil filepath nil 'silent)
    filepath))

(defun mevedel-tools--plan--implement-result (action plan-markdown chat-buffer
                                                     callback)
  "Save plan, set pending action ACTION, fire CALLBACK with status string.
Used as the implement / implement-clear branch of the overlay
callback.  ACTION is `implement' or `implement-clear'."
  (condition-case err
      (let ((filepath (mevedel-tools--plan--save plan-markdown chat-buffer)))
        (with-current-buffer chat-buffer
          (setq mevedel--pending-plan-action
                (list :action action
                      :plan-file filepath
                      :plan-markdown plan-markdown)))
        (funcall callback
                 (format
                  (if (eq action 'implement-clear)
                      "User accepted the plan and chose to implement with clear context.\n\nPlan saved to: %s"
                    "User accepted the plan and chose to implement it.\n\nPlan saved to: %s")
                  filepath)))
    (error
     (funcall callback
              (format "User accepted the plan, but failed to save to file: %S\n\nHere is the plan:\n\n%s"
                      err plan-markdown)))))

(cl-defun mevedel-tools--present-plan (callback plan)
  "Present PLAN to user for interactive feedback.

CALLBACK is the tool's async callback; receives a tool-result string.
PLAN is a plist with `:title', `:summary', and `:sections' keys.

The overlay is callback-driven (no `recursive-edit'): each command
calls `mevedel--prompt--settle' with a symbolic outcome, which the
overlay's adapter maps into the appropriate tool-result string and
side effects (saving the plan, marking `mevedel--pending-plan-action').

Outcomes:
  `implement' / `implement-clear' — save plan, set pending action,
                                    LLM continues
  (feedback . TEXT)               — LLM revises the plan
  `aborted'                       — canceller-driven teardown; LLM
                                    receives `Error: aborted'

The `q' / `C-c C-k' / `C-g' keys call `mevedel-abort' directly: that
drains the request's cancellers, which fires this overlay's callback
with `aborted'.  No separate direct-fire path."
  (mevedel-tools--validate-params callback mevedel-tools--present-plan
    (plan (listp . "object")))
  (let* ((chat-buffer (current-buffer))
         (title (or (plist-get plan :title) "Untitled Plan"))
         (summary (or (plist-get plan :summary) "No summary provided"))
         (sections (append (plist-get plan :sections) nil))
         (plan-markdown
          (concat
           "# Plan: " title "\n\n"
           "## Summary\n"
           summary "\n\n"
           (mapconcat
            (lambda (section)
              (let ((heading (or (plist-get section :heading)
                                 "Unnamed Section"))
                    (content (or (plist-get section :content) "No content"))
                    (type (or (plist-get section :type) "step")))
                (format "## %s `[%s]`\n%s\n" heading type content)))
            sections
            "\n")))
         (overlay-callback
          (lambda (outcome)
            (pcase outcome
              ('implement
               (mevedel-tools--plan--implement-result
                'implement plan-markdown chat-buffer callback))
              ('implement-clear
               (mevedel-tools--plan--implement-result
                'implement-clear plan-markdown chat-buffer callback))
              (`(feedback . ,text)
               (funcall callback
                        (format
                         "User rejected the plan.\n\nFeedback: %s\n\nOriginal plan:\n%s\n\nPlease revise the plan addressing this feedback."
                         text plan-markdown)))
              ('aborted
               (funcall callback "Error: aborted"))
              (_ (funcall callback "Error: aborted")))))
         overlay)
    (cl-labels
        ((settle (sym)
           (when overlay
             (mevedel--prompt--settle overlay sym)))
         (implement-plan ()
           (interactive) (settle 'implement))
         (implement-plan-clear ()
           (interactive) (settle 'implement-clear))
         (reject-plan-feedback ()
           (interactive)
           (let ((feedback (read-string "Feedback on this plan: ")))
             (settle (cons 'feedback feedback))))
         (abort-plan ()
           ;; The `q' / `C-c C-k' / `C-g' user intent is "tear down this
           ;; request".  `mevedel-abort' drains cancellers, including
           ;; this overlay's; the overlay's callback fires with
           ;; `aborted' through the standard path.  No direct settle
           ;; here — that would race the canceller drain.
           (interactive)
           (mevedel-abort)))
      ;; Defensive: read `point-max', insert content, build the overlay,
      ;; and register the canceller all from inside `chat-buffer'.  The
      ;; current handler is invoked with `chat-buffer' already current,
      ;; but if a future caller wraps the dispatch in `with-temp-buffer'
      ;; (the way Grep/Glob do per the pipeline-context-hazard rule)
      ;; reading `point-max' or pushing onto `mevedel--prompt-overlays'
      ;; from the wrong buffer would corrupt state silently.
      (with-current-buffer chat-buffer
        (let* ((keymap (make-sparse-keymap))
               (start (point-max)))
          (save-excursion
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert "\n")
              (insert (propertize "\n" 'font-lock-face
                                  '(:inherit font-lock-string-face :underline t :extend t)))
              (let ((content-start (point)))
                (insert "\n" plan-markdown "\n")
                (gptel-agent--fontify-block 'markdown-mode content-start (point))
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
              (insert (propertize "\n" 'font-lock-face
                                  '(:inherit font-lock-string-face :underline t :extend t)))))
          (setq overlay (make-overlay start (point-max) chat-buffer))
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'mevedel-plan t)
          (overlay-put overlay 'mevedel-user-request t)
          (overlay-put overlay 'mevedel--callback overlay-callback)
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
          (push overlay mevedel--prompt-overlays)
          (mevedel--prompt--register-canceller)
          (goto-char start)
          (when-let* ((buf-win (get-buffer-window chat-buffer)))
            (with-selected-window buf-win
              (recenter-top-bottom 1))))))))


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


;;; Renderers

(defun mevedel-tool-plan--render-create (name args result _render-data)
  "Rendering plist for the CreatePlan tool.
Header shows the short task description; body fontifies the planner's
plan output in the data buffer's major mode (org when the chat buffer
is org-mode and gptel has converted the response, markdown otherwise)."
  (when (stringp result)
    (let* ((description (or (plist-get args :description) ""))
           (lines (length (split-string result "\n"))))
      (list :header (format "%s: %s (%d lines)"
                            (or name "CreatePlan") description lines)
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :initially-collapsed-p t))))


;;
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
    :async-p t
    :read-only-p t)

  (mevedel-define-tool
    :name "CreatePlan"
    :description "Launch the planner agent to create an implementation plan."
    :prompt-file "tools/createplan.md"
    :handler #'mevedel-tool-plan--create
    :args ((description string :required
                       "A short (3-5 word) description of what is being planned.")
           (prompt string :required
                  "Detailed prompt for the planner: what needs to be implemented, constraints, requirements."))
    :async-p t
    :read-only-p t
    :renderer #'mevedel-tool-plan--render-create))

(provide 'mevedel-tool-plan)
;;; mevedel-tool-plan.el ends here
