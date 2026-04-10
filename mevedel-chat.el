;;; mevedel-chat.el -- Chat buffer management and directive processing -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel))

;; `gptel'
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel--apply-preset "ext:gptel" (preset &optional setter))
(declare-function gptel-request "ext:gptel-request")
(declare-function gptel-fsm-info "ext:gptel-request")
(declare-function gptel-fsm-state "ext:gptel-request")
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-send "ext:gptel" ())
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(defvar gptel-default-mode)
(defvar gptel-mode)
(defvar gptel-use-header-line)
(defvar gptel--header-line-info)
(defvar gptel-display-buffer-action)
(defvar gptel-stream)
(defvar gptel-prompt-transform-functions)
(defvar gptel-send--handlers)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)
(defvar gptel--markdown-block-map)

;; `mevedel-structs'
(declare-function mevedel-session-create "mevedel-structs" (name workspace))
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-workspace--name "mevedel-workspace" (workspace))
(defvar mevedel--workspace)
(defvar mevedel-workspace-additional-roots)

;; `mevedel-compact'
(declare-function mevedel--token-header-segment "mevedel-compact")

;; `mevedel-overlays'
(declare-function mevedel--topmost-instruction "mevedel-overlays" (instruction type))
(declare-function mevedel--highest-priority-instruction "mevedel-overlays" (instructions &optional non-processing))
(declare-function mevedel--instructions-at "mevedel-overlays" (position &optional type))
(declare-function mevedel--directive-text "mevedel-overlays" (directive))
(declare-function mevedel--directive-llm-prompt "mevedel-overlays" (directive))
(declare-function mevedel--directivep "mevedel-overlays" (instruction))
(declare-function mevedel--child-instructions "mevedel-overlays" (instruction))
(declare-function mevedel--delete-instruction "mevedel-overlays" (instruction))
(declare-function mevedel--update-instruction-overlay "mevedel-overlays" (instruction &optional force))

;; `mevedel-tool-fs'
(declare-function mevedel-tools--generate-diff "mevedel-tool-fs" (original modified filepath))
(defvar mevedel--request-file-snapshots)

;; `mevedel-tool-ui'
(declare-function mevedel--clear-pending-access-requests "mevedel-tool-ui" (&rest _))

;; `org-src'
(declare-function org-escape-code-in-string "ext:org-src" (s))

;; `mevedel-presets'
(defvar mevedel-action-preset-alist)
(defvar mevedel-preset--registry)
(declare-function mevedel-preset--build-handlers "mevedel-presets" (handlers))
(declare-function mevedel-preset--setup-deferred "mevedel-presets" (preset-name))

;; `mevedel-agents'
(declare-function mevedel-agents--setup-for-request "mevedel-agents" (&optional preset-name))


;;
;;; Customization

(defcustom mevedel-show-patch-buffer nil
  "Control if the mevedel patch buffer should be shown automatically.

If non-nil, the patch buffer will automatically be displayed after a
query completes."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel--diff-preview-buffer-name "*mevedel-diff-preview*"
  "Name of the `diff' preview buffer.")

(defcustom mevedel-show-chat-buffer t
  "Control if the mevedel chat buffer should be shown automatically.

If non-nil, the chat buffer will automatically be displayed."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-plans-directory (file-name-concat ".mevedel" "plans")
  "Directory where implementation plans are stored.

If this is a relative path, it is resolved relative to the workspace
root at plan-save time.  If absolute, it is used as-is."
  :type 'directory
  :group 'mevedel)


;;
;;; Buffer management

(defun mevedel--chat-buffer (session-name &optional create workspace)
  "Get or create the mevedel chat buffer SESSION-NAME for WORKSPACE.

This buffer is where LLM interactions occur. If CREATE is non-nil,
create the buffer if it doesn't exist. WORKSPACE should be a
`mevedel-workspace' struct, or nil to use the current buffer's
workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (buf (mevedel--get-buffer session-name workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (mevedel--chat-buffer-setup buf workspace session-name))
    buf))

(defun mevedel--tutor-buffer (&optional create workspace)
  "Get or create the mevedel tutor buffer for WORKSPACE.

This buffer is where LLM interactions occur. If CREATE is non-nil,
create the buffer if it doesn't exist. WORKSPACE should be a
`mevedel-workspace' struct, or nil to use the current buffer's
workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (buf (mevedel--get-buffer "tutor" workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (mevedel--chat-buffer-setup buf workspace "tutor"))
    buf))

(defun mevedel--chat-buffer-setup (buf workspace session-name)
  "Setup chat buffer BUF in WORKSPACE with SESSION-NAME."
  (with-current-buffer buf
    ;; Set major mode first -- this calls `kill-all-local-variables'.
    ;; Buffer-locals set before this point are wiped unless permanent-local.
    (funcall (or gptel-default-mode #'text-mode))
    ;; Enable `gptel-mode'
    (gptel-mode +1)
    ;; Create session after mode setup so it isn't wiped
    (setq-local mevedel--session
                (mevedel-session-create session-name workspace))
    ;; Right-align token count segment in gptel's header-line
    ;; HACK 2026-02-13: It is brittle and I do not like this approach but could
    ;;   not come up with something more robust. Let's hope `gptel' keeps it
    ;;   that way for some time.
    (when (and gptel-mode gptel-use-header-line header-line-format)
      (setq-local gptel--header-line-info
                  '(:eval
                    (let* ((base (eval (cadr (default-value 'gptel--header-line-info))))
                           (token (mevedel--token-header-segment)))
                      (if (string-empty-p token)
                          base
                        (setq base (copy-sequence base))
                        (let* ((disp (get-text-property 0 'display base))
                               (align-to (plist-get (cdr disp) :align-to))
                               (offset (nth 2 align-to)))
                          (put-text-property
                           0 1 'display
                           `(space :align-to (- right ,(+ offset 1 (length token))))
                           base)
                          (concat base token)))))))
    ;; Wrap lines
    (visual-line-mode +1)
    ;; Auto-scroll when at end of buffer
    (setq-local window-point-insertion-type t)
    ;; Set `default-directory' to workspace root
    (setq-local default-directory (mevedel-workspace-root workspace))
    ;; Make workspace-additional-roots buffer-local for session-specific access grants
    ;; Start with a copy of the global value so pre-configured roots are available
    (setq-local mevedel-workspace-additional-roots
                (copy-alist mevedel-workspace-additional-roots))
    (add-hook 'gptel-post-response-functions #'mevedel--clear-pending-access-requests nil t)))

(defun mevedel--patch-buffer (&optional create workspace)
  "Get or create the mevedel patch staging buffer for WORKSPACE.

This buffer shows diffs generated by the LLM that are awaiting review
and application. If CREATE is non-nil, create the buffer if it doesn't
exist. WORKSPACE should be a `mevedel-workspace' struct, or nil to use
the current buffer's workspace."
  (let* ((buf (mevedel--get-buffer "patch" workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (with-current-buffer buf
        (diff-mode)
        (setq buffer-read-only t)))
    buf))

(defun mevedel--get-buffer (name &optional workspace create-p)
  "Get or create a mevedel buffer named NAME in WORKSPACE.

NAME is a string used in the buffer name. For session buffers, use the
session name (e.g., \"main\", \"tutor\"). For auxiliary buffers, use a
descriptive name (e.g., \"patch\").

Buffer name format: *mevedel:NAME@WORKSPACE*.

Returns (BUFFER . CREATED-P) where CREATED-P indicates if buffer was
created. When CREATE-P is non-nil and buffer doesn't exist, create it
with workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (workspace-name (mevedel-workspace--name workspace))
         (buf-name (format "*mevedel:%s@%s*" name workspace-name))
         (target-buf (get-buffer buf-name))
         created-p)
    (when (and (not target-buf) create-p)
      (setq target-buf (get-buffer-create buf-name)
            created-p t)
      (with-current-buffer target-buf
        ;; Cache workspace struct for pre-session access
        (setq-local mevedel--workspace workspace)))
    (when target-buf
      (cons target-buf created-p))))

(defun mevedel--workspace-sessions (workspace)
  "Return alist of (SESSION-NAME . BUFFER) for WORKSPACE.

Scans live buffers for those with a `mevedel--session' whose workspace
matches WORKSPACE by type and id."
  (let ((ws-type (mevedel-workspace-type workspace))
        (ws-id (mevedel-workspace-id workspace))
        sessions)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (when-let* ((session (buffer-local-value 'mevedel--session buf))
                    (sw (mevedel-session-workspace session))
                    ((eq (mevedel-workspace-type sw) ws-type))
                    ((equal (mevedel-workspace-id sw) ws-id)))
          (push (cons (mevedel-session-name session) buf) sessions))))
    (nreverse sessions)))

(defun mevedel--active-chat-buffer (&optional workspace)
  "Find the active chat buffer for WORKSPACE.

If already in a mevedel chat buffer, return it.  Otherwise scan for
session buffers matching WORKSPACE: if one exists return it, if
multiple return the most recently used one.  Returns nil if none found."
  (cond
   ;; Already in a chat buffer with a session
   ((and (boundp 'mevedel--session) mevedel--session)
    (current-buffer))
   ;; Search for session buffers
   (t
    (when-let* ((workspace (or workspace (mevedel-workspace)))
                (sessions (mevedel--workspace-sessions workspace)))
      (if (= (length sessions) 1)
          (cdar sessions)
        ;; Multiple sessions: return most recently used (earliest in buffer-list)
        (let ((buf-list (buffer-list)))
          (cdr (car (cl-sort (copy-sequence sessions) #'<
                             :key (lambda (s)
                                    (or (cl-position (cdr s) buf-list)
                                        most-positive-fixnum)))))))))))

(defun mevedel--generate-final-patch (&optional workspace)
  "Generate final diffs for all tracked files in current request.

Returns a unified diff string showing original -> final state for each
file. Uses the `mevedel--request-file-snapshots' to compare original
states with current file contents in WORKSPACE."
  (let ((diffs "")
        (workspace-root (mevedel-workspace--root (or workspace (mevedel-workspace)))))
    (dolist (snapshot mevedel--request-file-snapshots)
      (let* ((filepath (car snapshot))
             (original (cdr snapshot))
             (current (when (file-exists-p filepath)
                        (with-temp-buffer
                          (insert-file-contents filepath)
                          (buffer-string))))
             (relpath (file-relative-name filepath workspace-root)))

        ;; Generate diff if file changed, was deleted, or was created
        (when (or
               ;; Modified
               (and original current (not (string= original current)))
               ;; Deleted
               (and original (not current))
               ;; Created
               (and (not original) current))
          (setq diffs (concat diffs
                              (format "diff --git a/%s b/%s\n" relpath relpath)
                              (cond
                               ((and (or (not original) (string-empty-p original))
                                     (and current (not (string-empty-p current))))
                                "new file mode 100644\n")
                               ((and (and original (not (string-empty-p original)))
                                     (or (not current) (string-empty-p current)))
                                "deleted file mode 100644\n"))
                              (mevedel-tools--generate-diff
                               (or original "")
                               (or current "")
                               relpath)
                              "\n")))))
    diffs))

(defun mevedel--replace-patch-buffer (patch-content)
  "Replace patch buffer contents with PATCH-CONTENT.
If PATCH-CONTENT is empty, does nothing."
  (when (and patch-content (> (length patch-content) 0))
    (with-current-buffer (mevedel--patch-buffer t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert patch-content)
        (diff-mode)
        (goto-char (point-min))))
    (mevedel--indicate-patch-ready)))

(defun mevedel--indicate-patch-ready ()
  "Provide visual feedback that a patch is ready for review."
  (message "Patch ready in *mevedel-patch* buffer")
  (when mevedel-show-patch-buffer
    (display-buffer (mevedel--patch-buffer))))

;;;###autoload
(defun mevedel-clear-patch-buffer ()
  "Clear the patch buffer."
  (interactive)
  (when-let ((buf (mevedel--patch-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Patch buffer cleared")))


;;
;;; Prompt generation

(defun mevedel--implement-directive-prompt (content)
  "Generate an implementation prompt for CONTENT in the current buffer."
  (format
   "## TASK: Implement the following request.

### INSTRUCTIONS:

1. Read and understand the implementation request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools as needed
5. Create working, complete code that fulfills the request

### IMPLEMENTATION REQUEST:

%s"
   content))

(defun mevedel--revise-directive-prompt (content &optional patch-buffer directive)
  "Generate a prompt for revising based on CONTENT (revision instructions).

The patch comes from either:
1. DIRECTIVE's stored patch (if provided and has one)
2. PATCH-BUFFER contents (defaulting to the mevedel patch buffer)

DIRECTIVE is the instruction overlay being revised."
  (let* ((directive-patch (when directive
                            (overlay-get directive 'mevedel-directive-patch)))
         (patch-buffer (or patch-buffer (mevedel--patch-buffer)))
         (patch-content
          (cond
           (directive-patch directive-patch)
           (patch-buffer
            (with-current-buffer patch-buffer
              (buffer-substring-no-properties (point-min) (point-max))))
           (t (user-error "No patch found for revision")))))
    (format
     "## TASK: Revise your previous implementation based on new feedback.

### INSTRUCTIONS:

1. Read the revision instructions below (if any)
2. Review your previous patch
3. Read and understand all provided references
4. Use the references to complete the request
5. Understand what needs to be changed or improved
6. Create a NEW implementation that addresses the feedback
7. Use your tools to make the changes
%s
==================================
YOUR PREVIOUS WORK (for reference)
==================================

%s"
     (if (and content (not (string-empty-p content)))
         (format "\n### REVISION INSTRUCTIONS:\n\n%s\n\n" content)
       "")
     patch-content)))

(defun mevedel--discuss-directive-prompt (content)
  "Generate a discussion prompt for CONTENT in the current buffer."
  (format
   "## TASK: Answer the following request.

### INSTRUCTIONS:

1. Read and understand the request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools to access files as needed

### REQUEST:

%s"
   content))


;;
;;; Directive processing

(defvar-local mevedel--current-directive-uuid nil
  "UUID of the directive currently being processed.")

(defvar-local mevedel--pending-plan-action nil
  "Pending plan implementation action, set by `PresentPlan'.

When non-nil, this is a plist with keys:
  :action       - Symbol: `implement' or `implement-clear'
  :plan-file    - Path to the saved plan file
  :plan-markdown - The plan text as markdown")

(defvar mevedel-default-chat-preset)

(defun mevedel--process-directive (directive preset prompt-fn callback)
  "Process DIRECTIVE using PRESET and PROMPT-FN, calling CALLBACK when complete.

DIRECTIVE is the instruction overlay to process.
PRESET is the gptel preset to use (mevedel-implement, mevedel-revise,
mevedel-discuss).
PROMPT-FN is a function that generates the prompt from the directive
content.
CALLBACK is called with (err fsm) when processing completes.

Updates directive status and overlay, handles success/failure states."
  ;; Save any unsaved buffers first
  (save-some-buffers nil (lambda () (and (buffer-file-name) (buffer-modified-p))))

  (let* ((directive-text (mevedel--directive-text directive))
         (content (mevedel--directive-llm-prompt directive))
         (prompt (funcall prompt-fn content))
         ;; Get chat buffer for the directive's buffer workspace
         (workspace (with-current-buffer (overlay-buffer directive)
                      (mevedel-workspace)))
         (chat-buffer (mevedel--chat-buffer "main" t workspace))
         (callback-fn (lambda (err fsm)
                        (if err
                            (let ((reason (if (eq err 'abort) "aborted" (format "%s" err))))
                              (overlay-put directive 'mevedel-directive-status 'failed)
                              (overlay-put directive 'mevedel-directive-fail-reason reason)
                              (mevedel--update-instruction-overlay directive t)
                              (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))
                              (setq mevedel--current-directive-uuid nil)
                              (when callback
                                (funcall callback err fsm)))

                          (overlay-put directive 'mevedel-directive-status 'succeeded)
                          (with-current-buffer (overlay-buffer directive)
                            ;; Delete any child directives of the top-level directive
                            (let ((child-directives (cl-remove-if-not #'mevedel--directivep
                                                                      (mevedel--child-instructions directive))))
                              (dolist (child-directive child-directives)
                                (mevedel--delete-instruction child-directive)))
                            (save-excursion
                              (goto-char (overlay-start directive))
                              (overlay-put directive 'evaporate t)))
                          (mevedel--update-instruction-overlay directive t)
                          (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))
                          (setq mevedel--current-directive-uuid nil)
                          (when callback
                            (funcall callback err fsm))))))

    (with-current-buffer chat-buffer
      (setq mevedel--current-directive-uuid (overlay-get directive 'mevedel-uuid)))

    (overlay-put directive 'mevedel-directive-status 'processing)
    (mevedel--update-instruction-overlay directive t)
    (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))

    ;; Display chat buffer if configured
    (when mevedel-show-chat-buffer
      (display-buffer chat-buffer gptel-display-buffer-action))

    ;; Execute with gptel-request
    (with-current-buffer chat-buffer
      (gptel--apply-preset
       (alist-get mevedel-default-chat-preset mevedel-action-preset-alist)
       (lambda (sym val) (set (make-local-variable sym) val)))

      (let* ((prompt prompt)
             (summary directive-text)
             (action (overlay-get directive 'mevedel-directive-action))
             (action-str (symbol-name action))
             (is-org-mode (derived-mode-p 'org-mode))
             (header-prefix
              (if is-org-mode
                  ""
                (format "`%s` " action-str)))
             (header-postfix
              (if is-org-mode
                  ;; Add the action as a tag at the end of the headline.
                  (format " :%s:" action-str)
                ""))
             ;; Extract the first non-whitespace line from the summary and
             ;; truncate to fill-column.
             (truncated-summary
              (let* ((lines (split-string summary "\n" t "[[:space:]]*"))
                     (first-line (or (car lines) ""))
                     ;; Calculate available space: total fill-column minus prefix, action, and spacing.
                     (prefix (or (alist-get major-mode gptel-prompt-prefix-alist) ""))
                     (used-length (+ (length prefix) (length header-prefix) (length header-postfix)))
                     (available-length (max 10 (- (or fill-column 70) used-length))))
                (truncate-string-to-width first-line available-length nil nil "...")))
             ;; Make the separation between prompt/response clearer using a
             ;; foldable block in org-mode
             (full-prompt-str
              (if is-org-mode
                  (progn
                    ;; Should already be required, but just for good measure.
                    (require 'org-src)
                    (concat (format ":PROMPT:\n") (org-escape-code-in-string prompt) "\n:END:\n"))
                (concat "``` prompt\n" prompt "\n```\n"))))

        (goto-char (point-max))

        ;; Insert the prefix if point isn't immediately preceded by it.
        (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
          (let ((prefix-length (length prefix)))
            (unless (and (>= (point) (+ (point-min) prefix-length))
                         (string=
                          (buffer-substring-no-properties (- (point) prefix-length) (point)) prefix))
              ;; Ensure prefix starts on its own line.
              (unless (bolp)
                (insert "\n"))
              (insert prefix))))

        ;; Header string.
        (insert (format "%s%s\n" header-prefix truncated-summary))
        ;; Add the demarcated prompt text.
        (let ((cur-pt (point)))
          (insert (if (derived-mode-p 'markdown-mode)
                      (propertize full-prompt-str 'gptel 'ignore 'keymap gptel--markdown-block-map)
                    (propertize full-prompt-str 'gptel 'ignore)))
          ;; Fold the prompt immediately.
          (ignore-errors
            (if (derived-mode-p 'org-mode)
                (save-excursion
                  (search-backward ":PROMPT:" cur-pt t)
                  (when (looking-at "^:PROMPT:")
                    (org-cycle)))
              (save-excursion
                (when (re-search-backward "^```" cur-pt t)
                  (gptel-markdown-cycle-block)))))))

      (gptel-with-preset preset
        ;; Set up agents, deferred tools, and FSM handlers at request time
        (mevedel-agents--setup-for-request preset)
        (mevedel-preset--setup-deferred preset)
        (let* ((gptel-send--handlers
                (mevedel-preset--build-handlers gptel-send--handlers))
               (request-callback
                (lambda (exit-code fsm)
                  (let* ((state (gptel-fsm-state fsm))
                         (error
                          (cond
                           ;; If we have a non-nil exit code (i.e. 'abort), just
                           ;; use it as the error.
                           (exit-code)
                           ;; If the FSM is in an errored state, extract the
                           ;; error text.
                           ((eq state 'ERRS)
                            (let* ((info (gptel-fsm-info fsm))
                                   (error (plist-get info :error))
                                   (http-msg (plist-get info :status))
                                   (error-type (plist-get error :type))
                                   (error-msg (plist-get error :message)))
                              (or error-msg (format "%s: %s" error-type http-msg))))
                           ;; Otherwise, consider the request successful
                           (t
                            nil))))

                    ;; Call overlay callback, including the original callback if
                    ;; provided
                    (when (functionp callback-fn)
                      (funcall callback-fn error fsm)))))
               (fsm (gptel-request prompt
                      :buffer chat-buffer
                      ;; NOTE 2025-11-03: This seems not to be necessary?
                      ;; :position (point-max)
                      :stream gptel-stream
                      :transforms gptel-prompt-transform-functions
                      :fsm (gptel-make-fsm :handlers gptel-send--handlers)))
               ;; Extract the actual gptel callback for handling responses. By
               ;; default this will generally be `gptel--insert-response' or
               ;; `gptel-curl--stream-insert-response'.
               (info (gptel-fsm-info fsm))
               (fsm-callback (plist-get info :callback))
               (wrapped-callback
                (lambda (response &rest rest)
                  "Invoke the user-provided callback after the request is aborted.
Intercept tool results for patch capture. Then pass arguments through to
the original callback."
                  (when (eq response 'abort)
                    (funcall request-callback 'abort fsm))
                  ;; Always pass through to original callback for normal display
                  (apply fsm-callback response rest))))
          (setf (gptel-fsm-info fsm) (plist-put info :callback wrapped-callback))
          (setf (gptel-fsm-info fsm) (plist-put info :mevedel-request-callback request-callback))
          fsm)))))

(defun mevedel-abort (&optional buf)
  "Abort any active request associated with buffer BUF.

Thus, abort `gptel' requests running in the mevedel chat buffer
associated with the `mevedel-workspace' for BUF.

If a callback was provided to the original request, it will be called
with the \\='abort symbol as the error parameter.

BUF defaults to the current buffer if not specified."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when-let* ((chat-buffer (mevedel--active-chat-buffer))
                (_ (buffer-live-p chat-buffer)))
      (gptel-abort chat-buffer))))


;;
;;; Plan implementation

(defun mevedel--plans-directory ()
  "Return the resolved plans directory for the current workspace.

If `mevedel-plans-directory' is a relative path, resolve it against the
workspace root.  If absolute, use as-is.  Creates the directory if it
does not exist."
  (let* ((workspace (mevedel-workspace))
         (dir (if (file-name-absolute-p mevedel-plans-directory)
                  mevedel-plans-directory
                (expand-file-name mevedel-plans-directory
                                  (mevedel-workspace--root workspace)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun mevedel--cleanup-plan-overlays ()
  "Remove agent and plan overlays from the current chat buffer.

Cleans up `gptel-agent' overlays (from the planner agent task) and
`mevedel-plan' overlays (from the PresentPlan tool) that remain after
the planning phase completes."
  (let ((inhibit-read-only t))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (or (overlay-get ov 'gptel-agent)
                (overlay-get ov 'mevedel-plan))
        (delete-overlay ov)))))

(defun mevedel--close-unclosed-blocks ()
  "Close any unclosed blocks at the end of the buffer.

When the main FSM is stopped mid-response (e.g., after plan acceptance),
the LLM may have left an open block.  This handles:
- Markdown fenced code blocks (``` reasoning, etc.)
- Org-mode blocks (#+begin_reasoning, etc.)"
  (let ((inhibit-read-only t))
    (save-excursion
      (cond
       ;; Markdown: count ``` fences; odd count means unclosed block
       ((derived-mode-p 'markdown-mode)
        (let ((fence-count 0))
          (goto-char (point-min))
          (while (re-search-forward "^```" nil t)
            (cl-incf fence-count))
          (when (cl-oddp fence-count)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "```\n")
            (gptel-markdown-cycle-block))))
       ;; Org-mode: find last unclosed #+begin_ block
       ((derived-mode-p 'org-mode)
        (let ((last-open nil))
          (goto-char (point-min))
          (while (re-search-forward
                  "^#\\+\\(begin\\|end\\)_\\([[:alpha:]_]+\\)" nil t)
            (if (string-equal-ignore-case (match-string 1) "begin")
                (setq last-open (match-string 2))
              (setq last-open nil)))
          (when last-open
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "#+end_%s\n" last-open))
            (org-cycle))))))))

(defun mevedel--implement-plan (action-plist)
  "Implement the plan described by ACTION-PLIST.

ACTION-PLIST is a plist with keys:
  :action        - Symbol: `implement' or `implement-clear'
  :plan-file     - Path to the saved plan file
  :plan-markdown - The plan text as markdown

For `implement', the plan is inserted into the chat buffer as a user
message and sent via `gptel-send', including full conversation context.

For `implement-clear', a fresh `gptel-request' is fired with the plan
as a string prompt, without prior conversation context."
  (let* ((plan-file (plist-get action-plist :plan-file))
         (chat-buffer (current-buffer))
         (plan-content (with-temp-buffer
                         (insert-file-contents plan-file)
                         (buffer-string)))
         (prompt (format "Implement the following plan:\n\n%s" plan-content)))
    (with-current-buffer chat-buffer
      ;; Clean up agent overlays left from the planning phase
      (mevedel--cleanup-plan-overlays)
      ;; Close any unclosed fenced code blocks (e.g., ``` reasoning)
      (mevedel--close-unclosed-blocks)
      (pcase (plist-get action-plist :action)
        ('implement
         ;; Insert plan as user message and send with full conversation context
         (goto-char (point-max))
         (insert gptel-response-separator)
         (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
           (let ((prefix-length (length prefix)))
             (unless (and (>= (point) (+ (point-min) prefix-length))
                          (string= (buffer-substring-no-properties
                                    (- (point) prefix-length) (point))
                                   prefix))
               (unless (bolp) (insert "\n"))
               (insert prefix))))
         (insert prompt "\n")
         (gptel-send))
        ('implement-clear
         ;; Fresh request without conversation context
         (goto-char (point-max))
         (insert gptel-response-separator)
         (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
           (let ((prefix-length (length prefix)))
             (unless (and (>= (point) (+ (point-min) prefix-length))
                          (string= (buffer-substring-no-properties
                                    (- (point) prefix-length) (point))
                                   prefix))
               (unless (bolp) (insert "\n"))
               (insert prefix))))
         (insert prompt "\n")
         (gptel-with-preset 'mevedel-implement
           (mevedel-agents--setup-for-request 'mevedel-implement)
           (mevedel-preset--setup-deferred 'mevedel-implement)
           (let ((gptel-send--handlers
                  (mevedel-preset--build-handlers gptel-send--handlers)))
             (gptel-request prompt
               :buffer chat-buffer
               :stream gptel-stream
               :transforms gptel-prompt-transform-functions
               :fsm (gptel-make-fsm :handlers gptel-send--handlers)))))))))

(provide 'mevedel-chat)

;;; mevedel-chat.el ends here
