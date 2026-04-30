;;; mevedel-tool-plan.el -- Planning tool -*- lexical-binding: t -*-

;;; Commentary:

;; Implementation plan creation and interactive presentation.  The
;; CreatePlan tool delegates to the planner agent, and PresentPlan
;; displays the plan inline with interactive controls for the user.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))
(require 'mevedel-queue)

;; `gptel-agent-tools'
(declare-function gptel-agent--fontify-block "ext:gptel-agent-tools" (path-or-mode start end))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `mevedel-agent-exec'
(defvar mevedel-agent-exec--agents)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))
(declare-function mevedel--plans-directory "mevedel-chat" ())
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agent-exec" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--pending-plan-action)
(defvar mevedel--session)

;; `mevedel-agents'
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))

;; `mevedel-tool-ui'
(declare-function mevedel-tools--task-by-name "mevedel-tool-ui"
                  (callback agent-type description prompt &optional background))
(declare-function mevedel--prompt--register-canceller "mevedel-tool-ui" ())
(declare-function mevedel--prompt--settle "mevedel-tool-ui" (overlay outcome))
(defvar mevedel--prompt-overlays)

;; `mevedel-view'
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())
(declare-function mevedel-view--interaction-anchor "mevedel-view" ())
(declare-function mevedel-view--interaction-register "mevedel-view"
                  (descriptor))
(declare-function mevedel-view--insert-attribution "mevedel-view"
                  (agent-id &optional live-click-p calls))
(defvar mevedel--view-buffer)


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
  (mevedel-tools--task-by-name callback "planner" description prompt))

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

(defun mevedel-tools--plan-origin (buffer)
  "Return the canonical origin for a PresentPlan dispatched in BUFFER."
  (or (and-let* (((buffer-live-p buffer))
                 ((local-variable-p 'mevedel--agent-invocation buffer))
                 (inv (buffer-local-value 'mevedel--agent-invocation
                                          buffer)))
        (mevedel-agent-invocation-agent-id inv))
      "main"))

(defun mevedel-plan-queue--current-session ()
  "Resolve the session struct that owns the PresentPlan FIFO."
  (mevedel-queue--current-session))

(defvar mevedel-plan-queue--spec
  (mevedel-queue-spec--create
   :name 'plan-queue
   :get (lambda (session) (mevedel-session-plan-queue session))
   :set (lambda (session queue)
          (setf (mevedel-session-plan-queue session) queue))
   :render #'mevedel-plan-queue--render-entry
   :settle (lambda (entry outcome)
             (when-let* ((callback (plist-get entry :callback)))
               (funcall callback outcome)))
   :entry-origin (lambda (entry) (plist-get entry :origin)))
  "Shared FIFO spec for PresentPlan confirmations.")

(defun mevedel-plan-queue--get (&optional session)
  "Return SESSION's plan queue."
  (when-let* ((sess (or session (mevedel-plan-queue--current-session))))
    (mevedel-queue--get mevedel-plan-queue--spec sess)))

(defun mevedel-plan-queue--set (queue &optional session)
  "Set SESSION's plan queue to QUEUE."
  (when-let* ((sess (or session (mevedel-plan-queue--current-session))))
    (mevedel-queue--set mevedel-plan-queue--spec sess queue)))

(defun mevedel-plan-queue--enqueue (entry)
  "Append PresentPlan ENTRY to the session FIFO and render the head."
  (mevedel-queue--enqueue mevedel-plan-queue--spec entry))

(defun mevedel-plan-queue--render-head (&optional session)
  "Render the current head of SESSION's PresentPlan FIFO."
  (mevedel-queue--render-head mevedel-plan-queue--spec
                              (or session
                                  (mevedel-plan-queue--current-session))))

(defun mevedel-plan-queue--on-head-outcome (entry outcome)
  "Settle PresentPlan ENTRY with OUTCOME and render the next head."
  (mevedel-queue--pop mevedel-plan-queue--spec entry outcome))

(defun mevedel-plan-queue-abort-all (&optional session)
  "Flush SESSION's plan FIFO, firing `aborted' on every entry."
  (mevedel-queue--abort-all mevedel-plan-queue--spec 'aborted session))

(defun mevedel-plan-queue-sweep-agent (origin &optional session)
  "Abort queued plans whose `:origin' matches ORIGIN."
  (mevedel-queue--sweep-origin
   mevedel-plan-queue--spec origin 'aborted session))

(defun mevedel-plan-queue--render-entry (entry)
  "Render PresentPlan queue ENTRY in the interaction zone."
  (let ((plan-markdown (plist-get entry :body))
        (chat-buffer (plist-get entry :chat-buffer))
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
           (interactive) (settle 'aborted)))
      (let ((target-buf (or (and (boundp 'mevedel--view-buffer)
                                 (buffer-local-value
                                  'mevedel--view-buffer chat-buffer)
                                 (buffer-live-p
                                  (buffer-local-value
                                   'mevedel--view-buffer chat-buffer))
                                 (buffer-local-value
                                  'mevedel--view-buffer chat-buffer))
                            chat-buffer)))
        (with-current-buffer target-buf
          (let* ((keymap (make-sparse-keymap))
                 (interaction-id (or (plist-get entry :interaction-id)
                                     (let ((id (list :plan (gensym "plan-"))))
                                       (plist-put entry :interaction-id id)
                                       id)))
                 (body
                  (concat
                   "\n"
                   (propertize
                    "\n" 'font-lock-face
                    '(:inherit font-lock-string-face :underline t :extend t))
                   (propertize (format "\n%s\n" plan-markdown)
                               'font-lock-face (gptel-agent--block-bg))
                   "\n"
                   (propertize "Keys: " 'font-lock-face 'help-key-binding)
                   (propertize "RET" 'font-lock-face 'help-key-binding)
                   " implement  "
                   (propertize "I" 'font-lock-face 'help-key-binding)
                   " implement (clear context)  "
                   (propertize "f" 'font-lock-face 'help-key-binding)
                   " feedback  "
                   (propertize "q" 'font-lock-face 'help-key-binding)
                   " cancel\n"
                   (propertize
                    "\n" 'font-lock-face
                    '(:inherit font-lock-string-face :underline t :extend t)))))
            (define-key keymap (kbd "RET") #'implement-plan)
            (define-key keymap (kbd "<return>") #'implement-plan)
            (define-key keymap (kbd "i") #'implement-plan)
            (define-key keymap (kbd "C-c C-c") #'implement-plan)
            (define-key keymap (kbd "I") #'implement-plan-clear)
            (define-key keymap (kbd "f") #'reject-plan-feedback)
            (define-key keymap (kbd "q") #'abort-plan)
            (define-key keymap (kbd "C-c C-k") #'abort-plan)
            (define-key keymap (kbd "C-g") #'abort-plan)
            (setq overlay
                  (mevedel-view--interaction-register
                   (list :kind 'plan
                         :id interaction-id
                         :count (length (mevedel-plan-queue--get
                                         (plist-get entry :session)))
                         :body body
                         :priority 200
                         :keymap keymap
                         :help-echo "PresentPlan confirmation"
                         :entry entry
                         :activate
                         (lambda (outcome)
                           (mevedel-plan-queue--on-head-outcome
                            entry outcome)))))
            (overlay-put overlay 'mevedel-plan t)
            (overlay-put overlay 'mevedel-user-request t)
            (overlay-put overlay 'mevedel--callback
                         (lambda (outcome)
                           (mevedel-plan-queue--on-head-outcome
                            entry outcome)))
            (overlay-put overlay 'keymap keymap)
            (push overlay mevedel--prompt-overlays)
            (mevedel--prompt--register-canceller)
            (goto-char (mevedel-view--interaction-anchor))
            (when-let* ((buf-win (get-buffer-window target-buf)))
              (with-selected-window buf-win
                (recenter-top-bottom 1)))))))))

(defun mevedel-tools--plan--implement-result (action plan-markdown chat-buffer
                                                     callback)
  "Save plan, set pending action ACTION, fire CALLBACK with result.

Returns the standard tool-result shape with render-data side
channel:

  (:result \"User accepted...\"
   :render-data (:kind plan-summary
                 :body PLAN-MARKDOWN
                 :origin ORIGIN
                 :outcome ACTION
                 :timestamp ISO-TIMESTAMP))

The result string is the LLM-facing payload (a short
acknowledgement plus the plan filepath); the render-data is the
view-side persistence so the plan summary survives buffer
rerender, session resume, and compaction without view-side
overlay bookkeeping."
  (condition-case err
      (let ((filepath (mevedel-tools--plan--save plan-markdown chat-buffer))
            ;; Resolve the canonical agent-id when PresentPlan was
            ;; dispatched from a sub-agent buffer (e.g., by the
            ;; planner agent which owns this tool); fall back to
            ;; "main" otherwise.
            (origin (mevedel-tools--plan-origin chat-buffer)))
        (with-current-buffer chat-buffer
          (setq mevedel--pending-plan-action
                (list :action action
                      :plan-file filepath
                      :plan-markdown plan-markdown)))
        (funcall callback
                 (list
                  :result
                  (format
                   (if (eq action 'implement-clear)
                       "User accepted the plan and chose to implement with clear context.\n\nPlan saved to: %s"
                     "User accepted the plan and chose to implement it.\n\nPlan saved to: %s")
                   filepath)
                  :render-data
                  (list :kind 'plan-summary
                        :body plan-markdown
                        :origin origin
                        :outcome action
                        ;; Match the agent transcript filename timestamp shape
                        ;; (`mevedel-tool-ui.el:1273` uses %FT%H-%M-%S),
                        ;; so render and transcript timestamps stay
                        ;; consistent across the codebase.
                        :timestamp (format-time-string "%FT%H-%M-%S")))))
    (error
     ;; Failure during save: LLM-facing result is authoritative.
     ;; The user already confirmed; no render-data is emitted (the
     ;; failure path doesn't persist a summary).  Surface the
     ;; failure via `display-warning' so a subsequent
     ;; `mevedel-view-rerender' has a diagnosable reason for the
     ;; missing visible card.
     (display-warning
      'mevedel
      (format "Plan save failed: %S" err)
      :warning)
     (funcall callback
              (format "Error: User accepted the plan, but failed to save it: %S"
                      err)))))

(cl-defun mevedel-tools--present-plan (callback plan)
  "Present PLAN to user for interactive feedback.

CALLBACK is the tool's async callback; receives a tool-result string.
PLAN is a plist with `:title', `:summary', and `:sections' keys.

The overlay is callback-driven (no `recursive-edit'): each command
calls `mevedel--prompt--settle' with a symbolic outcome, which the
overlay's adapter maps into the appropriate tool-result string and
side effects (saving the plan, marking `mevedel--pending-plan-action').

Outcomes:
  `implement' / `implement-clear' -- save plan, set pending action,
                                    LLM continues
  (feedback . TEXT)               -- LLM revises the plan
  `aborted'                       -- canceller-driven teardown; LLM
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
              (_ (funcall callback "Error: aborted"))))))
    (mevedel-plan-queue--enqueue
     (list :body plan-markdown
           :chat-buffer chat-buffer
           :origin (mevedel-tools--plan-origin chat-buffer)
           :callback overlay-callback))))


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

(defun mevedel-tool-plan--render-present (name args result render-data)
  "Renderer for PresentPlan results.
When RENDER-DATA carries `:kind plan-summary' as emitted by
`mevedel-tools--plan--implement-result', produce a
collapsible card whose header reads
`> Plan from <agent-id> [<outcome> at <timestamp>]' and whose
body is the markdown plan.  Otherwise fall through to the
default tool-result rendering by returning nil."
  (ignore name args)
  (when (and (consp render-data)
             (eq (plist-get render-data :kind) 'plan-summary))
    (let* ((body (or (plist-get render-data :body)
                     (and (stringp result) result) ""))
           (origin (or (plist-get render-data :origin) "main"))
           (attribution
            (if (and (not (equal origin "main"))
                     (fboundp 'mevedel-view--insert-attribution))
                (mevedel-view--insert-attribution origin)
              (format "from %s" origin)))
           (outcome (plist-get render-data :outcome))
           (timestamp (plist-get render-data :timestamp))
           (outcome-label
            (pcase outcome
              ('implement (format "implemented at %s" (or timestamp "?")))
              ('implement-clear
               (format "implemented · cleared at %s" (or timestamp "?")))
              (_ (format "%s" (or outcome "?"))))))
      (list :header (concat "Plan " attribution "  ["
                            outcome-label "]")
            :body body
            :body-mode 'markdown-mode
            :vtype 'plan-summary
            :initially-collapsed-p t))))

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
    :read-only-p t
    :renderer #'mevedel-tool-plan--render-present)

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
