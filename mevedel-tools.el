;;; mevedel-tools.el -- Tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `diff-mode'
(declare-function diff-beginning-of-hunk "diff-mode" (&optional try-harder))
(declare-function diff-filename-drop-dir "diff-mode" (file))
(declare-function diff-hunk-file-names "diff-mode" (&optional old))
(declare-function diff-hunk-next "diff-mode" (&optional count))
(declare-function diff-setup-buffer-type "diff-mode" ())

;; `gptel-agent-tools'
(declare-function gptel-agent--fontify-block "ext:gptel-agent-tools" (path-or-mode start end))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `gptel'
(defvar gptel--fsm-last)
(defvar gptel--header-line-info)
(defvar gptel-display-buffer-action)
(defvar gptel-mode)
(defvar gptel-use-header-line)

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `imenu'
(declare-function imenu--make-index-alist "imenu" (&optional noerror))
(defvar imenu--index-alist)

;; `mevedel'
(defvar mevedel--diff-preview-buffer-name)
(defvar mevedel-plans-directory)

;; `mevedel-diff-apply'
(declare-function mevedel-diff-apply-buffer "mevedel-diff-apply" ())

;; `mevedel-utilities'
(declare-function mevedel-ediff-patch "mevedel-utilities" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))
(declare-function mevedel-add-project-root "mevedel-workspace" (directory))

;; `org-src'
(declare-function org-escape-code-in-region "org-src" (beg end))

;; `treesit'
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-field-name "treesit" (node))
(declare-function treesit-node-text "treesit" (node &optional no-property))

;; `xref'
(declare-function xref-backend-references "xref" (backend identifier))
(declare-function xref-item-location "xref" (cl-x) t)
(declare-function xref-item-summary "xref" (cl-x) t)


;;
;;; Customization

(defcustom mevedel-inline-preview-threshold 0.8
  "Ratio of chat buffer height to use for inline preview threshold.

Diffs with fewer lines than this ratio times the chat buffer height
will be displayed inline as overlays in the chat buffer. Larger diffs
will be displayed in a separate buffer.

Set to 0 to always use separate buffer, or 1.0 to always prefer inline
display (when possible)."
  :type 'number
  :group 'mevedel)


;;
;;; Variables

(defconst mevedel-tools--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))

(defvar mevedel-tools--util-tools
  '(;; Todo list
    ("mevedel" "TodoWrite")
    ("mevedel" "TodoRead")
    ;; Ask user
    ("mevedel" "Ask")
    ;; Directory access
    ("mevedel" "RequestAccess")
    ;; Agent
    ("gptel-agent" "Agent")
    ;; Web search
    ("gptel-agent" "WebSearch")
    ("gptel-agent" "WebFetch")
    ("gptel-agent" "YouTube")))

(defvar mevedel-tools--read-tools
  '(;; File reading
    ("mevedel" "Read")
    ("mevedel" "Glob")
    ("mevedel" "Grep")
    ;; ;; Present Plan
    ;; "PresentPlan"
    ))

(defvar mevedel-tools--code-tools
  '(;; Xref
    ("mevedel" "XrefReferences")
    ("mevedel" "XrefDefinitions")
    ;; Imenu
    ("mevedel" "Imenu")
    ;; Treesitter
    ("mevedel" "Treesitter")))

(defvar mevedel-tools--edit-tools
  '(;; File editing
    ("mevedel" "Write")
    ("mevedel" "Edit")
    ("mevedel" "Insert")
    ;; Create directory
    ("mevedel" "MkDir")))

(defvar mevedel-tools--eval-tools
  '(;; Bash
    ("mevedel" "Bash")
    ;; Eval
    ("gptel-agent" "Eval")))


;;
;;; Validation & Permission Macros

(defmacro mevedel-tools--validate-params (callback function-name &rest param-specs)
  "Validate parameters for tool functions.

CALLBACK is the callback function to call with error messages (can be
nil for sync functions).
FUNCTION-NAME is the name of the function being validated (symbol, can
be nil for lambdas).
PARAM-SPECS is a list of (VAR TYPE-SPEC) or (VAR TYPE-SPEC REQUIRED)
forms where:

  - VAR is the parameter variable name (symbol)
  - TYPE-SPEC is either:
    - A predicate function symbol (e.g., stringp, integerp)
      Special: booleanp handles both t and :json-false automatically
    - A cons (PRED . TYPE-NAME) for custom type names
      e.g., (vectorp . \"array\") checks with vectorp, reports \"array\"
    - A lambda for custom validation
      e.g., (lambda (x) (and (numberp x) (> x 0)))
    - A cons (LAMBDA . TYPE-NAME) for lambda with custom name
  - REQUIRED is optional, defaults to t. If nil, skip validation when
    VAR is nil.

Examples:
  (mevedel-tools--validate-params callback my-func
    (name stringp)                    ; Required string
    (enabled booleanp)                ; Boolean (handles :json-false)
    (count integerp nil)              ; Optional integer
    (items (vectorp . \"array\"))     ; Vector reported as \"array\"
    (score (lambda (x) (and (numberp x) (>= x 0) (<= x 100))))
    (id ((lambda (x) (stringp x)) . \"non-empty string\")))

Returns validation code that uses `cl-return-from' if CALLBACK is
non-nil, otherwise `error', to exit early."
  (declare (indent defun) (debug t))
  (let ((clauses nil))
    (dolist (spec param-specs)
      (cl-destructuring-bind (var type-spec &optional (required t)) spec
        (let* ((var-name (symbol-name var))
               ;; Handle plain symbols, cons, and lambda expressions
               (type-pred (cond
                           ;; Plain lambda: (lambda (x) ...)
                           ((and (consp type-spec)
                                 (eq 'lambda (car type-spec)))
                            type-spec)
                           ;; Lambda with custom name: ((lambda ...) . "type")
                           ((and (consp type-spec)
                                 (consp (car type-spec))
                                 (eq 'lambda (car (car type-spec))))
                            (car type-spec))
                           ;; Pred with custom name: (pred . "type")
                           ((consp type-spec) (car type-spec))
                           ;; Plain predicate symbol
                           (t type-spec)))
               (type-name (cond
                           ;; Custom type name in cdr
                           ((and (consp type-spec) (stringp (cdr type-spec)))
                            (cdr type-spec))
                           ;; Lambda without custom name
                           ((and (consp type-pred) (eq 'lambda (car type-pred)))
                            "valid value")
                           ;; Plain predicate - derive from name
                           (t (replace-regexp-in-string
                               "p$" "" (symbol-name type-pred)))))
               ;; Build the type check form
               (type-check-form
                (cond
                 ;; Special case: booleanp handles both t and :json-false
                 ((eq type-pred 'booleanp)
                  `(or (eq ,var t) (eq ,var :json-false)))
                 ;; Lambda expression: use funcall with quoted lambda
                 ((and (consp type-pred) (eq 'lambda (car type-pred)))
                  `(funcall ,type-pred ,var))
                 ;; Regular predicate function
                 (t `(,type-pred ,var)))))

          ;; Add required check
          (when required
            (push `((not ,var)
                    ,(if callback
                         `(cl-return-from ,function-name
                            (funcall ,callback
                                     ,(format "Error: '%s' parameter is required" var-name)))
                       `(error ,(format "'%s' parameter is required" var-name))))
                  clauses))

          ;; Add type check
          (let ((err-msg `(format ,(format "%s'%s' must be %s%%s, received %%s: %%S"
                                           (if callback "Error: " "")
                                           var-name
                                           (if (string-match-p "^[aeiou]" (downcase type-name))
                                               (concat "an " type-name)
                                             (concat "a " type-name)))
                           ,(if required "" " (when provided)")
                           (type-of ,var) ,var)))
            (push `(,(if required
                         `(not ,type-check-form)
                       `(and ,var (not ,type-check-form)))
                    ,(if callback
                         `(cl-return-from ,function-name
                            (funcall ,callback ,err-msg))
                       `(error "%s" ,err-msg)))
                  clauses)))))
    `(cond ,@(nreverse clauses))))

(defmacro mevedel-tools--check-directory-permissions (path reason function-name callback)
  "Check and request directory access permissions for PATH.

Verifies that PATH is within workspace-allowed roots. If not, requests
user permission to access the directory containing PATH.

Arguments:
  PATH          - File or directory path to check (will be expanded).
  REASON        - String explaining why access is needed (shown to user).
  FUNCTION-NAME - Name of the calling function (for early return via
                  `cl-return-from' when CALLBACK is provided).
  CALLBACK      - If non-nil, call with error message on denial instead
                  of signaling an error. Should be a function accepting
                  a format string and arguments.

If access is denied:

  - With CALLBACK: returns early from FUNCTION-NAME by calling CALLBACK
    with an error message.
  - Without CALLBACK: signals an error."
  (declare (indent defun) (debug t))
  `(let* ((target-path (expand-file-name ,path))
          (file-root (mevedel-workspace--file-in-allowed-roots-p target-path)))
     ;; No access yet, request it
     (unless file-root
       (let* ((requested-root (or (file-name-directory target-path)
                                  target-path))
              (reason ,reason)
              (granted (mevedel-tools--request-access requested-root reason)))
         ;; Access denied
         (unless granted
           ,(if callback
                `(cl-return-from ,function-name
                   (funcall ,callback "Error: Access denied to %s" requested-root))
              `(error "Access denied to %s" requested-root)))))))


;;
;;; Diff Utilities

(defun mevedel-tools--generate-diff (original modified filepath)
  "Generate unified diff between ORIGINAL and MODIFIED content for FILEPATH."
  (with-temp-buffer
    (let ((orig-file (make-temp-file "mevedel-orig-"))
          (mod-file (make-temp-file "mevedel-mod-")))
      (unwind-protect
          (progn
            (with-temp-file orig-file (when original (insert original)))
            (with-temp-file mod-file (when modified (insert modified)))
            (call-process "diff" nil t nil
                          "-u"
                          "--label" (if (and original (not (string-empty-p original)))
                                        (concat "a/" filepath)
                                      "/dev/null")
                          "--label" (if (and modified (not (string-empty-p modified)))
                                        (concat "b/" filepath)
                                      "/dev/null")
                          orig-file mod-file)
            (buffer-string))
        (when (file-exists-p orig-file) (delete-file orig-file))
        (when (file-exists-p mod-file) (delete-file mod-file))))))

(defun mevedel-tools--setup-diff-buffer (temp-file real-path workspace root
                                                   &optional chat-buffer final-callback
                                                   user-modified original-window-config)
  "Setup diff buffer with content and full configuration.

Creates and configures `mevedel--diff-preview-buffer-name' with:
- Diff content between REAL-PATH and TEMP-FILE
- Read-only diff-mode with truncated lines
- Proper buffer-local variables for workspace context
- Header line with file path and action hints

Arguments:
- TEMP-FILE: Path to file with proposed changes
- REAL-PATH: Path to actual file
- WORKSPACE: Workspace identifier
- ROOT: Workspace root directory
- CHAT-BUFFER: Optional chat buffer reference
- FINAL-CALLBACK: Optional callback function
- USER-MODIFIED: Optional flag for user modifications
- ORIGINAL-WINDOW-CONFIG: Optional saved window configuration

Returns the configured diff buffer."
  (let* ((rel-path (file-relative-name real-path root))
         (original-content (when (file-exists-p real-path)
                             (with-temp-buffer
                               (insert-file-contents real-path)
                               (buffer-string))))
         (modified-content (with-temp-buffer
                             (insert-file-contents temp-file)
                             (buffer-string)))
         (diff (mevedel-tools--generate-diff original-content modified-content rel-path))
         (diff-buffer (get-buffer-create mevedel--diff-preview-buffer-name)))
    (with-current-buffer diff-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))
        (insert diff)
        (diff-mode)
        ;; Always use read-only mode for safety
        (read-only-mode 1)
        ;; Always truncate lines for better diff readability
        (setq-local truncate-lines t)
        ;; Re-detect patch type (i.e. 'git) now that buffer is populated
        (when (derived-mode-p 'diff-mode)
          (diff-setup-buffer-type))

        ;; Always set these buffer-local variables
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--temp-file temp-file
                    mevedel--real-path real-path
                    mevedel--chat-buffer chat-buffer
                    mevedel--final-callback final-callback
                    mevedel--user-modified user-modified
                    mevedel--original-window-config original-window-config)

        (goto-char (point-min))))
    diff-buffer))


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

(defvar-local mevedel--request-file-snapshots nil
  "Alist of (FILEPATH . ORIGINAL-CONTENT) tracking files modified.

Each entry stores the original state of a file before any modifications
in the current request. ORIGINAL-CONTENT is nil if the file didn't exist
before the request. Cleared when request completes.")

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
  "Deny the request at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-user-request)))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (setq mevedel--request-result nil)
    (delete-overlay ov)
    (delete-region start end)
    (exit-recursive-edit)))

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
        (insert (propertize "y" 'font-lock-face 'help-key-binding))
        (insert " approve  ")
        (insert (propertize "n" 'font-lock-face 'help-key-binding))
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
                       ;; Feedback binding
                       "f"        #'mevedel--feedback-request))

        ;; Store overlay reference
        (setq mevedel--request-overlay ov)

        ;; Apply background
        (font-lock-append-text-property
         start (point) 'font-lock-face (gptel-agent--block-bg))))

    ;; Show the chat buffer and position cursor at the overlay
    (display-buffer chat-buffer gptel-display-buffer-action)
    (goto-char start)
    (recenter)

    ;; Wait for user decision via recursive-edit
    (setq mevedel--request-result 'pending)

    ;; Enter recursive edit - allows user input while blocking
    (unwind-protect
        (condition-case err
            (recursive-edit)
          ;; Treat quit (C-g) as a denial
          (quit (setq mevedel--request-result nil))
          (error
           (user-error "%s" (error-message-string err))
           (setq mevedel--request-result nil)))

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

(defun mevedel--prompt-user-for-bash-command (command)
  "Prompt user for permission to execute COMMAND in the chat buffer.
Returns one of:
- t if approved
- nil if denied
- (feedback . TEXT) if user provides feedback

Displays an overlay showing the command and extracted sub-commands."
  (let* ((extraction (mevedel-tools--extract-commands command))
         (commands (car extraction))
         (unparseable (cdr extraction))
         (content (concat
                   "The LLM is requesting permission to execute a bash command.\n\n"
                   (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
                   (propertize (format "%s\n\n" command) 'font-lock-face 'font-lock-string-face)
                   (when commands
                     (concat
                      (propertize "Detected commands: " 'font-lock-face 'font-lock-escape-face)
                      (propertize (mapconcat #'identity commands ", ")
                                  'font-lock-face 'font-lock-constant-face)
                      "\n\n"))
                   (when unparseable
                     (propertize "⚠ Warning: Command contains complex syntax that could not be fully parsed.\n\n"
                                 'font-lock-face 'warning)))))
    (mevedel--prompt-user-with-overlay
     "Bash Command Execution Request"
     content
     "Execute this command?"
     (concat "Bash command execution: "
             (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                         'face 'help-key-binding)))))

(defun mevedel--clear-pending-access-requests (&rest _)
  "Clear the pending access requests cache.
Should be called after each LLM response completes."
  (setq mevedel--pending-access-requests nil))


;;
;;; Command Execution

(defcustom mevedel-bash-permissions
  '(;; Default: ask for everything not explicitly allowed/denied
    ("*" . ask)

    ;; File inspection (read-only)
    ("ls*" . allow)
    ("cat*" . allow)
    ("head*" . allow)
    ("tail*" . allow)
    ("less*" . allow)
    ("more*" . allow)
    ("file*" . allow)
    ("stat*" . allow)
    ("wc*" . allow)
    ("du*" . allow)
    ("df*" . allow)

    ;; Directory operations (read-only)
    ("pwd*" . allow)
    ("cd*" . allow)

    ;; Text processing (read-only)
    ("grep*" . allow)
    ("egrep*" . allow)
    ("fgrep*" . allow)
    ("rg*" . allow)
    ("ag*" . allow)
    ("awk*" . allow)
    ("cut*" . allow)
    ("sort*" . allow)
    ("uniq*" . allow)
    ("tr*" . allow)
    ("diff*" . allow)

    ;; File search (read-only)
    ("find*" . allow)
    ("which*" . allow)
    ("whereis*" . allow)
    ("type*" . allow)

    ;; Version control (read operations)
    ("git status*" . allow)
    ("git log*" . allow)
    ("git diff*" . allow)
    ("git show*" . allow)
    ("git branch*" . allow)
    ("git tag*" . allow)
    ("git remote*" . allow)
    ("git ls-files*" . allow)
    ("git config --get*" . allow)
    ("git config --list*" . allow)

    ;; Process inspection (read-only)
    ("ps*" . allow)
    ("pgrep*" . allow)

    ;; System information (read-only)
    ("uname*" . allow)
    ("hostname*" . allow)
    ("whoami*" . allow)
    ("id*" . allow)
    ("date*" . allow)
    ("uptime*" . allow)
    ("printenv*" . allow)

    ;; Echo (safe output)
    ("echo*" . allow)
    ("printf*" . allow))
  "Permission settings for bash commands.
Each entry is (PATTERN . ACTION) where PATTERN is a shell glob and
ACTION is one of the symbols `allow`, `deny`, or `ask'. Later entries
override earlier ones.

This default configuration allows common read-only operations used in
development workflows. Dangerous commands are still caught by
`mevedel-bash-dangerous-commands' even if patterns would allow them.

IMPORTANT: Put specific patterns LAST since later entries override
earlier ones. Example: ((\"*\" . deny) (\"ls*\" . allow)) denies
everything except ls."
  :type '(repeat (cons (string :tag "Glob pattern")
                       (choice :tag "Action" (const allow) (const deny) (const ask))))
  :group 'mevedel)

(defcustom mevedel-bash-dangerous-commands
  '("rm" "sudo" "dd" "mkfs" "fdisk" "parted"
    "chmod" "chown" "chgrp" "chattr"
    "kill" "pkill" "killall"
    "curl" "wget" "nc" "ncat" "telnet"
    "ssh" "scp" "rsync" "sftp"
    "iptables" "systemctl" "service"
    "reboot" "shutdown" "poweroff" "halt")
  "Commands that always require explicit confirmation.
Even if a pattern in `mevedel-bash-permissions' would allow these
commands, they will still trigger a confirmation prompt due to their
potential for system modification, data loss, or external network
access."
  :type '(repeat string)
  :group 'mevedel)

(defcustom mevedel-tools--bash-fail-safe-on-complex-syntax t
  "When non-nil, always ask for permission when complex syntax is detected.
Complex syntax includes: variable expansion ($VAR, ${VAR}), eval, exec,
nested quotes, here-documents, and other constructs that cannot be
reliably parsed.

When nil, the system will attempt to extract commands from complex
syntax, which may miss dangerous commands hidden in variable expansions
or other dynamic constructs.

Recommended value: t (fail-safe behavior)."
  :type 'boolean
  :group 'mevedel)

(defun mevedel-tools--permission-action (command permissions)
  "Return the action for COMMAND given PERMISSIONS.
Returns (ACTION . MATCHED-PATTERN) cons cell."
  (let ((action 'ask)
        (matched-pattern nil))
    (dolist (entry permissions)
      (let ((pattern (car entry))
            (value (let ((val (cdr entry)))
                     (cond
                      ((memq val '(allow deny ask)) val)
                      ((and (stringp val) (not (string-empty-p val)))
                       (pcase (intern (downcase val))
                         ('allow 'allow)
                         ('deny 'deny)
                         ('ask 'ask)
                         (_ 'ask)))
                      (t 'ask)))))
        (when (and (stringp pattern)
                   (mevedel-tools--match-pattern pattern command))
          (setq action value)
          (setq matched-pattern pattern))))
    (cons action matched-pattern)))

(defun mevedel-tools--match-pattern (pattern command)
  "Return non-nil when COMMAND matches shell glob PATTERN."
  (condition-case nil
      (string-match-p (wildcard-to-regexp pattern) command)
    (error nil)))

(defun mevedel-tools--quotes-balanced-p (str)
  "Return t if quotes in STR are properly balanced, nil otherwise.
Handles single quotes, double quotes, and backslash escaping."
  (let ((in-single nil)
        (in-double nil)
        (escaped nil)
        (i 0)
        (len (length str)))
    (catch 'unbalanced
      (while (< i len)
        (let ((c (aref str i)))
          (cond
           ;; Handle escape sequences
           (escaped
            (setq escaped nil))

           ;; Backslash starts escape
           ((eq c ?\\)
            (setq escaped t))

           ;; Single quote toggle (only outside double quotes)
           ((and (eq c ?') (not in-double))
            (setq in-single (not in-single)))

           ;; Double quote toggle (only outside single quotes)
           ((and (eq c ?\") (not in-single))
            (setq in-double (not in-double))))
          (setq i (1+ i))))

      ;; Quotes are balanced if we're not currently inside any quotes
      ;; and not in an escaped state
      (and (not in-single) (not in-double) (not escaped)))))

(defun mevedel-tools--contains-complex-syntax-p (str)
  "Return t if STR's syntax is too complex to parse safely, nil otherwise.

Complex syntax includes:
- Variable expansion: $VAR, ${VAR}
- Eval or exec commands
- Here documents
- Brace expansion
- Unbalanced quotes"
  (or
   ;; Variable expansion (but not command substitution which we handle)
   (and (string-match-p "\\$[{A-Za-z_]" str) t)

   ;; Eval or exec
   (and (string-match-p "\\b\\(eval\\|exec\\)\\b" str) t)

   ;; Here documents
   (and (string-match-p "<<-?\\s-*['\"]?\\w" str) t)

   ;; Brace expansion that could hide commands
   (and (string-match-p "{[^}]*,[^}]*}" str) t)

   ;; Unmatched quotes
   (not (mevedel-tools--quotes-balanced-p str))))

(defun mevedel-tools--split-command-chain (str)
  "Split STR on command separators, respecting quotes.
Handles: && || ; | and newlines.
Returns list of command segments."
  (let ((result '())
        (current "")
        (in-single nil)
        (in-double nil)
        (escaped nil)
        (i 0)
        (len (length str)))
    (while (< i len)
      (let ((c (aref str i))
            (next (when (< (1+ i) len) (aref str (1+ i)))))
        (cond
         ;; Handle escape sequences
         (escaped
          (setq current (concat current (char-to-string c)))
          (setq escaped nil))

         ;; Backslash starts escape
         ((eq c ?\\)
          (setq current (concat current (char-to-string c)))
          (setq escaped t))

         ;; Single quote toggle (only outside double quotes)
         ((and (eq c ?') (not in-double))
          (setq current (concat current (char-to-string c)))
          (setq in-single (not in-single)))

         ;; Double quote toggle (only outside single quotes)
         ((and (eq c ?\") (not in-single))
          (setq current (concat current (char-to-string c)))
          (setq in-double (not in-double)))

         ;; Handle separators outside quotes
         ((and (not in-single) (not in-double))
          (cond
           ;; && separator
           ((and (eq c ?&) (eq next ?&))
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current "")
            (setq i (1+ i))) ; skip next &

           ;; || separator
           ((and (eq c ?|) (eq next ?|))
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current "")
            (setq i (1+ i))) ; skip next |

           ;; Single | (pipe)
           ((and (eq c ?|) (not (eq next ?|)))
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current ""))

           ;; ; separator
           ((eq c ?\;)
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current ""))

           ;; Newline separator
           ((eq c ?\n)
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current ""))

           ;; Regular character
           (t (setq current (concat current (char-to-string c))))))

         ;; Inside quotes - accumulate
         (t (setq current (concat current (char-to-string c)))))
        (setq i (1+ i))))

    ;; Add final segment
    (when (> (length (string-trim current)) 0)
      (push (string-trim current) result))

    (nreverse result)))

(defun mevedel-tools--extract-substitutions (str)
  "Extract command substitutions from STR: $(...) and `...`.
Returns list of substitution contents.
Handles nested $(...)."
  (let ((result '()))
    ;; Extract $(...)
    (let ((pos 0))
      (while (string-match "\\$(" str pos)
        (let ((start (match-end 0))
              (depth 1)
              (i (match-end 0)))
          (while (and (< i (length str)) (> depth 0))
            (let ((c (aref str i)))
              (cond
               ((eq c ?\() (setq depth (1+ depth)))
               ((eq c ?\)) (setq depth (1- depth))))
              (setq i (1+ i))))
          (when (= depth 0)
            (push (substring str start (1- i)) result)
            (setq pos i)))))

    ;; Extract `...` (backticks)
    (let ((pos 0))
      (while (string-match "`\\([^`]*\\)`" str pos)
        (push (match-string 1 str) result)
        (setq pos (match-end 0))))

    (nreverse result)))

(defun mevedel-tools--remove-substitutions (str)
  "Remove command substitutions from STR, replacing with placeholder.
Returns cleaned string with substitutions removed."
  (let ((result str))
    ;; Remove $(...) - use simple regex replacement
    (while (string-match "\\$(([^)]*)" result)
      (setq result (replace-match "__SUBST__" t t result)))

    ;; Remove backticks
    (while (string-match "`[^`]*`" result)
      (setq result (replace-match "__SUBST__" t t result)))

    result))

(defun mevedel-tools--extract-command-name (segment)
  "Extract the command name from SEGMENT.
Handles prefixes like sudo, env, and paths like /bin/cmd.
Returns command name string or nil."
  (when (and segment (> (length segment) 0))
    (condition-case nil
        (let* ((words (split-string-and-unquote segment))
               ;; Skip variable assignments (VAR=value)
               (words (seq-drop-while
                       (lambda (w)
                         (string-match-p "^[A-Za-z_][A-Za-z0-9_]*=" w))
                       words))
               (first-word (car words)))
          (when first-word
            (cond
             ;; sudo, doas, su - return the actual command after the prefix
             ((member first-word '("sudo" "doas" "su"))
              (or (cadr words) first-word))

             ;; nice with optional -n flag
             ((string-equal first-word "nice")
              (let ((rest (cdr words)))
                (or (if (and rest (string-equal (car rest) "-n"))
                        (nth 3 words)  ; skip nice, -n, and value (4th element)
                      (cadr words))    ; just skip nice (2nd element)
                    "nice")))

             ;; timeout - skip timeout and its duration argument
             ((string-equal first-word "timeout")
              (or (caddr words) "timeout"))  ; skip timeout and duration

             ;; nohup, time - next word is the actual command
             ((member first-word '("nohup" "time"))
              (or (cadr words) first-word))

             ;; env with args - find first non-assignment
             ((string-equal first-word "env")
              (or (car (seq-drop-while
                        (lambda (w) (string-match-p "=" w))
                        (cdr words)))
                  "env"))

             ;; Absolute/relative path - extract basename
             ((string-match-p "/" first-word)
              (file-name-nondirectory first-word))

             ;; Regular command
             (t first-word))))
      (error nil))))

(defun mevedel-tools--extract-commands (command-string)
  "Extract all command names from COMMAND-STRING.
Returns (COMMANDS . UNPARSEABLE) where:
- COMMANDS is a list of extracted command names
- UNPARSEABLE is t if complex syntax was detected, nil otherwise."
  (let ((commands '())
        (unparseable nil))

    ;; Check for complex syntax and mark it, but continue extraction
    ;; The check-bash-permission function will decide what to do based on fail-safe setting
    (when (mevedel-tools--contains-complex-syntax-p command-string)
      (setq unparseable t))

    ;; Split on command separators
    (dolist (segment (mevedel-tools--split-command-chain command-string))
      (let ((segment-commands '())
            (words (condition-case nil
                       (split-string-and-unquote segment)
                     (error nil))))

        ;; Build commands in correct order: sudo (if present), main cmd, substitutions

        ;; 1. Check if segment starts with sudo/doas/su - add the prefix
        (when (and words (member (car words) '("sudo" "doas" "su")))
          (setq segment-commands (append segment-commands (list (car words)))))

        ;; 2. Extract and add the main command name
        (when-let ((cmd (mevedel-tools--extract-command-name segment)))
          (setq segment-commands (append segment-commands (list cmd))))

        ;; 3. Extract and recursively process command substitutions
        (dolist (subst (mevedel-tools--extract-substitutions segment))
          (let ((sub-result (mevedel-tools--extract-commands subst)))
            (setq segment-commands (append segment-commands (car sub-result)))
            (when (cdr sub-result)
              (setq unparseable t))))

        ;; Add all segment commands to main commands list
        (setq commands (append commands segment-commands))))

    (cons commands unparseable)))

(cl-defun mevedel-tools--check-bash-permission (command)
  "Check if COMMAND is allowed based on permission rules.
Extracts all commands from COMMAND string (including commands in chains,
pipes, and substitutions) and checks each against permission rules and
the dangerous command blocklist.

Returns one of the symbols:
- `allow': Command is allowed to execute
- `deny': Command is denied
- `ask': User should be prompted for confirmation"
  (let* ((extraction (mevedel-tools--extract-commands command))
         (commands (car extraction))
         (unparseable (cdr extraction)))

    ;; If unparseable and fail-safe is enabled, always ask
    (when (and unparseable mevedel-tools--bash-fail-safe-on-complex-syntax)
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

    ;; If no commands were extracted, ask for safety
    (when (null commands)
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

    ;; Check the full command string first
    (let* ((full-result (mevedel-tools--permission-action command mevedel-bash-permissions))
           (full-action (car full-result))
           (full-pattern (cdr full-result))
           ;; Check if command contains shell operators (chains, pipes, etc.)
           (has-operators (string-match-p "&&\\|||\\||\\|;\\|\n" command))
           ;; Specific match: non-generic pattern AND no shell operators
           (specific-match (and full-pattern
                                (not (member full-pattern '("*" "**")))
                                (not has-operators))))

      ;; If full command matched a SPECIFIC pattern AND has no operators, trust
      ;; that match (this handles "git status", "git log args", etc.)
      (if specific-match
          ;; Specific match: only check dangerous blocklist, don't check
          ;; extracted commands
          (if (and (eq full-action 'allow)
                   (seq-some (lambda (cmd) (member cmd mevedel-bash-dangerous-commands))
                             commands))
              'ask
            full-action)

        ;; Otherwise: check ALL extracted commands for defense-in-depth
        (let ((actions (list full-action)))
          ;; Check each extracted command against patterns
          (dolist (cmd commands)
            (push (car (mevedel-tools--permission-action cmd mevedel-bash-permissions)) actions))

          ;; Apply dangerous command blocklist
          (when (seq-some (lambda (cmd) (member cmd mevedel-bash-dangerous-commands))
                          commands)
            (push 'ask actions))

          ;; Combine with precedence: deny > ask > allow
          (cond
           ((memq 'deny actions) 'deny)
           ((memq 'ask actions) 'ask)
           (t 'allow)))))))


;;
;;; File Snapshotting

(defun mevedel--snapshot-file-if-needed (filepath)
  "Capture original state of FILEPATH before first modification in request.
Does nothing if FILEPATH has already been snapshotted in this request.
Stores nil for ORIGINAL-CONTENT if file doesn't exist yet."
  (when (and filepath (stringp filepath))
    (let ((abs-path (expand-file-name filepath)))
      (unless (assoc abs-path mevedel--request-file-snapshots)
        (push (cons abs-path
                    (when (file-exists-p abs-path)
                      (with-temp-buffer
                        (insert-file-contents abs-path)
                        (buffer-string))))
              mevedel--request-file-snapshots)))))

(defun mevedel--snapshot-files-from-diff (diff-str)
  "Extract and snapshot all files referenced in DIFF-STR.
Uses `diff-mode' to parse the diff and extract file paths.

Must be called from the mevedel chat buffer context to preserve
buffer-local snapshots."
  ;; Validate that we're running in the chat buffer context
  (unless (buffer-local-value 'mevedel--workspace (current-buffer))
    (error "`mevedel--snapshot-files-from-diff' must be called from chat buffer context"))
  ;; Extract file paths in a temp buffer, but snapshot in the current buffer
  (let* ((workspace (mevedel-workspace))
         (files-to-snapshot
          (with-temp-buffer
            (insert diff-str)
            (diff-mode)
            (goto-char (point-min))
            (let ((files nil))
              (condition-case nil
                  (progn
                    (diff-beginning-of-hunk t)
                    (while (not (eobp))
                      (let ((file-names (diff-hunk-file-names)))
                        (when file-names
                          (let* ((new-file (car file-names))
                                 (old-file (cadr file-names))
                                 (ws-root (mevedel-workspace--root workspace)))
                            ;; Collect files to snapshot
                            (unless (string-match-p "dev/null" new-file)
                              (let ((filepath (expand-file-name (diff-filename-drop-dir new-file) ws-root)))
                                (push filepath files)))
                            (unless (string-match-p "dev/null" old-file)
                              (let ((filepath (expand-file-name (diff-filename-drop-dir old-file) ws-root)))
                                (push filepath files))))))
                      (condition-case nil
                          (diff-hunk-next)
                        (error (goto-char (point-max))))))
                (error nil))
              files))))
    ;; Now snapshot files in the CURRENT buffer context (not temp buffer) and
    ;; deduplicate the file list first
    (dolist (filepath (delete-dups files-to-snapshot))
      (mevedel--snapshot-file-if-needed filepath))))


;;
;;; Inline preview

(defun mevedel-tools--show-changes-and-confirm (temp-file original-content real-path final-callback
                                                          &optional tool-name)
  "Show diff between ORIGINAL-CONTENT and TEMP-FILE, ask user to confirm.

TEMP-FILE - path to file with proposed changes
ORIGINAL-CONTENT - original file content
REAL-PATH - path to real file
FINAL-CALLBACK - callback to return final result to LLM
TOOL-NAME - optional tool name for display (e.g., \"Edit\", \"Write\", \"Insert\")"
  ;; Validate that we're running in the chat buffer context (tools should be called by gptel from chat buffer)
  (unless (buffer-local-value 'mevedel--workspace (current-buffer))
    (error "`mevedel-tools--show-changes-and-confirm' must be called from chat buffer context"))
  (let* ((chat-buffer (current-buffer))
         ;; The file we are editing can be in the in the main workspace root or
         ;; in another allowed one
         (root (mevedel-workspace--file-in-allowed-roots-p real-path chat-buffer))
         (workspace (mevedel-workspace chat-buffer))
         (rel-path (file-relative-name real-path root))
         (diff-buffer (mevedel-tools--setup-diff-buffer
                       temp-file real-path workspace root
                       chat-buffer
                       final-callback
                       nil  ; user-modified
                       (current-window-configuration)))
         (diff (with-current-buffer diff-buffer (buffer-string))))

    ;; Decide whether to use inline or separate buffer display
    (if (mevedel-tools--should-use-inline-preview-p diff chat-buffer)
        ;; Use inline preview
        (mevedel-tools--show-inline-preview diff temp-file original-content
                                            real-path final-callback
                                            chat-buffer workspace root rel-path
                                            tool-name)
      ;; Use separate buffer (existing behavior)
      (with-current-buffer diff-buffer
        ;; Set helpful header line
        (setq header-line-format
              (concat
               (propertize (format " Proposed changes to %s. -- " rel-path) 'face 'success)
               (propertize "Choose: (a)pprove, (r)eject, (e)dit, (f)eedback and reject" 'face 'help-key-binding))))

      ;; Show the diff buffer and prompt
      (pop-to-buffer diff-buffer)
      (mevedel-tools--prompt-for-changes))))

(defun mevedel-tools--should-use-inline-preview-p (diff-string chat-buffer)
  "Return t if DIFF-STRING should be displayed inline in CHAT-BUFFER.

Compares the number of lines in DIFF-STRING against
`mevedel-inline-preview-threshold' times the visible height of
CHAT-BUFFER's window."
  (and (> mevedel-inline-preview-threshold 0)
       (let* ((diff-lines (with-temp-buffer
                            (insert diff-string)
                            (count-lines (point-min) (point-max))))
              (chat-window (get-buffer-window chat-buffer))
              (chat-height (and chat-window (window-height chat-window))))
         (and chat-height
              (<= diff-lines (* chat-height mevedel-inline-preview-threshold))))))

(defun mevedel-tools--show-inline-preview (diff-string temp-file _original-content
                                                       real-path final-callback
                                                       chat-buffer workspace root rel-path
                                                       &optional tool-name)
  "Show DIFF-STRING as an inline overlay in CHAT-BUFFER.

Arguments:
- DIFF-STRING: The unified diff to display
- TEMP-FILE: Path to temporary file with proposed changes
- _ORIGINAL-CONTENT: Original file content (unused, for signature compatibility)
- REAL-PATH: Actual file path
- FINAL-CALLBACK: Async callback to return result
- CHAT-BUFFER: The chat buffer context
- WORKSPACE: Current workspace
- ROOT: Workspace root directory
- REL-PATH: Relative path for display
- TOOL-NAME: Optional tool name for display (e.g., \"Edit\", \"Write\", \"Insert\")"
  (let ((ov (mevedel-tools--create-inline-preview-overlay
             diff-string temp-file real-path final-callback
             chat-buffer workspace root rel-path
             nil nil tool-name)))
    ;; Show the chat buffer and position cursor at the overlay
    (with-current-buffer chat-buffer
      (goto-char (overlay-start ov)))
    (display-buffer chat-buffer gptel-display-buffer-action)))

(defun mevedel-tools--create-inline-preview-overlay (diff-string temp-file real-path
                                                                 final-callback chat-buffer
                                                                 workspace root rel-path
                                                                 &optional user-modified position tool-name)
  "Create an inline preview overlay in CHAT-BUFFER at POSITION.

Arguments:
- DIFF-STRING: The unified diff to display
- TEMP-FILE: Path to temporary file with proposed changes
- REAL-PATH: Path to actual file
- FINAL-CALLBACK: Async callback to return result
- CHAT-BUFFER: The chat buffer context
- WORKSPACE: Current workspace
- ROOT: Workspace root directory
- REL-PATH: Relative path for display
- USER-MODIFIED: Optional flag indicating user edits (shows warning)
- POSITION: Optional position to insert at (defaults to point-max)
- TOOL-NAME: Optional tool name for display (e.g., \"Edit\", \"Write\",
  \"Insert\")

Returns the created overlay."
  (with-current-buffer chat-buffer
    (goto-char (or position (point-max)))
    (let ((start (point)))
      ;; Insert header
      (insert "\n")
      (insert (concat
               (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t))
               (if tool-name
                   (concat
                    (propertize (format "%s: " tool-name) 'font-lock-face 'font-lock-escape-face)
                    "Proposed changes to "
                    (propertize (format "%s\n" rel-path) 'font-lock-face 'font-lock-constant-face))
                 (concat
                  "Proposed changes to "
                  (propertize (format "%s\n" rel-path) 'font-lock-face 'font-lock-constant-face)))
               "\n"))

      (when user-modified
        (insert (propertize "[Modified via ediff]\n" 'face 'warning)))

      ;; Insert diff content
      (let ((diff-start (point)))
        (insert diff-string)
        (insert "\n")
        ;; Apply syntax highlighting to diff content
        (gptel-agent--fontify-block 'diff-mode diff-start (point))
        ;; Apply background color
        (font-lock-append-text-property
         start (point) 'font-lock-face (gptel-agent--block-bg))
        (when (derived-mode-p 'org-mode)
          (org-escape-code-in-region start (1- (point)))))

      (insert (propertize "Keys: " 'font-lock-face 'help-key-binding))
      (insert (propertize "RET" 'font-lock-face 'help-key-binding))
      (insert " approve  ")
      (insert (propertize "q" 'font-lock-face 'help-key-binding))
      (insert " reject  ")
      (insert (propertize "e" 'font-lock-face 'help-key-binding))
      (insert " edit  ")
      (insert (propertize "f" 'font-lock-face 'help-key-binding))
      (insert " feedback  ")
      (insert (propertize "TAB" 'font-lock-face 'help-key-binding))
      (insert " toggle\n")
      (insert (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t)))

      ;; Create overlay with context
      (let ((ov (mevedel-tools--confirm-overlay start (point) t)))
        (overlay-put ov 'mevedel--temp-file temp-file)
        (overlay-put ov 'mevedel--real-path real-path)
        (overlay-put ov 'mevedel--final-callback final-callback)
        (overlay-put ov 'mevedel--user-modified user-modified)
        (overlay-put ov 'mevedel--chat-buffer chat-buffer)
        (overlay-put ov 'mevedel--workspace workspace)
        (overlay-put ov 'mevedel--root root)
        (when tool-name
          (overlay-put ov 'mevedel--tool-name tool-name))
        ov))))

(defun mevedel-tools--confirm-overlay (from to &optional no-hide)
  "Set up tool call preview overlay FROM TO.

If NO-HIDE is non-nil, don't hide the overlay body by default."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'mevedel-inline-preview t)
    (overlay-put ov 'priority 10)
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'help-echo
                 (concat "Approval requested: "
                         (propertize "Keys: C-c C-c approve  C-c C-k reject  C-c C-e edit  C-c C-f feedback  TAB toggle\n"
                                     'face 'help-key-binding)))
    (overlay-put ov 'keymap
                 (define-keymap
                   ;; Mouse support
                   "<mouse-1>" #'mevedel-tools--dispatch-inline-preview
                   ;; Navigation keys (simple, less likely to conflict)
                   "n"        #'mevedel-tools--next-preview-overlay
                   "p"        #'mevedel-tools--previous-preview-overlay
                   "<tab>"    #'mevedel-tools--cycle-overlay
                   "TAB"      #'mevedel-tools--cycle-overlay
                   ;; Action keys
                   "C-c C-c"  #'mevedel-tools--approve-inline-preview
                   "C-c C-k"  #'mevedel-tools--reject-inline-preview
                   "C-c C-e"  #'mevedel-tools--edit-inline-preview
                   "C-c C-f"  #'mevedel-tools--feedback-inline-preview
                   "RET"      #'mevedel-tools--approve-inline-preview
                   "<return>" #'mevedel-tools--approve-inline-preview
                   "a"        #'mevedel-tools--approve-inline-preview
                   "r"        #'mevedel-tools--reject-inline-preview
                   "q"        #'mevedel-tools--reject-inline-preview
                   "e"        #'mevedel-tools--edit-inline-preview
                   "f"        #'mevedel-tools--feedback-inline-preview))
    (unless no-hide
      (mevedel-tools--cycle-overlay ov))
    ov))

(defun mevedel-tools--cycle-overlay (ov)
  "Cycle tool call preview overlay OV between collapsed and expanded."
  (interactive (list (cdr (get-char-property-and-overlay
                           (point) 'mevedel-inline-preview))))
  (when ov
    (save-excursion
      (goto-char (overlay-start ov))
      (let ((line-end (line-end-position))
            (end      (overlay-end ov)))
        (pcase-let ((`(,value . ,hide-ov)
                     (get-char-property-and-overlay line-end 'invisible)))
          (if (and hide-ov (eq value t))
              (delete-overlay hide-ov)
            (unless hide-ov (setq hide-ov (make-overlay line-end (1- end) nil t)))
            (overlay-put hide-ov 'evaporate t)
            (overlay-put hide-ov 'invisible t)
            (overlay-put hide-ov 'before-string " ▼")))))))

(defun mevedel-tools--dispatch-inline-preview ()
  "Prompt user for action on inline preview via mouse click."
  (interactive)
  (let ((choice (read-char-choice
                 "Action: (a)pprove, (r)eject, (e)dit, (f)eedback: "
                 '(?a ?r ?e ?f))))
    (pcase choice
      (?a (call-interactively #'mevedel-tools--approve-inline-preview))
      (?r (call-interactively #'mevedel-tools--reject-inline-preview))
      (?e (call-interactively #'mevedel-tools--edit-inline-preview))
      (?f (call-interactively #'mevedel-tools--feedback-inline-preview)))))

(defun mevedel-tools--approve-inline-preview ()
  "Approve the inline preview at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (final-callback (overlay-get ov 'mevedel--final-callback))
              (chat-buffer (overlay-get ov 'mevedel--chat-buffer))
              (workspace (overlay-get ov 'mevedel--workspace))
              (root (overlay-get ov 'mevedel--root)))
    (let ((user-modified (overlay-get ov 'mevedel--user-modified)))
      ;; Apply changes using mevedel-diff-apply-buffer for proper overlay preservation
      (condition-case err
          (progn
            ;; Create and setup diff buffer, then apply it
            (let ((diff-buffer (get-buffer mevedel--diff-preview-buffer-name)))
              (with-current-buffer diff-buffer
                ;; Apply the diff with overlay preservation
                (mevedel-diff-apply-buffer))
              ;; Clean up temp buffer
              (kill-buffer diff-buffer))

            (delete-file temp-file)
            (funcall final-callback
                     (if user-modified
                         (format "Changes approved and applied to %s, but were modified by user via ediff. You may need to read the relevant sections of the file to see the final applied changes." real-path)
                       (format "Changes approved and applied to %s" real-path)))
            ;; Clean up overlay
            (let ((start (overlay-start ov))
                  (end (overlay-end ov)))
              (delete-overlay ov)
              (delete-region start end)))
        (error
         (funcall final-callback
                  (format "Error applying changes: %s" (error-message-string err)))
         (let ((start (overlay-start ov))
               (end (overlay-end ov)))
           (delete-overlay ov)
           (delete-region start end)))))))

(defun mevedel-tools--reject-inline-preview ()
  "Reject the inline preview at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (final-callback (overlay-get ov 'mevedel--final-callback)))
    (funcall final-callback
             (format "User rejected changes to %s" real-path))
    (delete-file temp-file)
    (let ((start (overlay-start ov))
          (end (overlay-end ov)))
      (delete-overlay ov)
      (delete-region start end))))

(defun mevedel-tools--feedback-inline-preview ()
  "Reject the inline preview at point with feedback."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (final-callback (overlay-get ov 'mevedel--final-callback)))
    (let ((feedback (read-string "What should be changed? ")))
      (funcall final-callback
               (format "Changes rejected by user. User feedback: %s" feedback))
      (delete-file temp-file)
      (let ((start (overlay-start ov))
            (end (overlay-end ov)))
        (delete-overlay ov)
        (delete-region start end)))))

(defvar mevedel-tools--current-inline-preview-overlay nil
  "Stores the current inline preview overlay during ediff session.")

(defun mevedel-tools--edit-inline-preview ()
  "Edit the inline preview at point using ediff."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (chat-buffer (overlay-get ov 'mevedel--chat-buffer))
              (workspace (overlay-get ov 'mevedel--workspace))
              (root (overlay-get ov 'mevedel--root)))
    ;; Mark as user-modified
    (overlay-put ov 'mevedel--user-modified t)
    ;; Store overlay in a variable for the hook to find
    (setq mevedel-tools--current-inline-preview-overlay ov)
    ;; Create and setup diff buffer for ediff
    (let ((diff-buffer (mevedel-tools--setup-diff-buffer
                        temp-file real-path workspace root chat-buffer)))
      ;; Add one-shot hook to return to inline preview after ediff
      (add-hook 'mevedel--ediff-finished-hook
                #'mevedel-tools--return-to-inline-preview)
      (with-current-buffer diff-buffer
        (mevedel-ediff-patch)))))

(defun mevedel-tools--return-to-inline-preview ()
  "Return to inline preview after ediff session completes.

Updates the inline preview with any changes made during the ediff session."
  (remove-hook 'mevedel--ediff-finished-hook
               #'mevedel-tools--return-to-inline-preview)
  ;; After ediff, the diff buffer has been updated with user edits
  (when-let ((ov mevedel-tools--current-inline-preview-overlay)
             (chat-buffer (overlay-get ov 'mevedel--chat-buffer))
             (diff-buffer (get-buffer mevedel--diff-preview-buffer-name)))
    (with-current-buffer chat-buffer
      ;; Get the updated diff content from the diff buffer
      (let* ((updated-diff (with-current-buffer diff-buffer
                             (buffer-string)))
             (overlay-start (overlay-start ov))
             (overlay-end (overlay-end ov))
             (temp-file (overlay-get ov 'mevedel--temp-file))
             (real-path (overlay-get ov 'mevedel--real-path))
             (final-callback (overlay-get ov 'mevedel--final-callback))
             (workspace (overlay-get ov 'mevedel--workspace))
             (root (overlay-get ov 'mevedel--root))
             (tool-name (overlay-get ov 'mevedel--tool-name))
             (rel-path (file-relative-name real-path root)))

        ;; Delete the old overlay and its region
        (delete-overlay ov)
        (delete-region overlay-start overlay-end)

        ;; Recreate the preview with updated content at the same position
        (mevedel-tools--create-inline-preview-overlay
         updated-diff temp-file real-path final-callback
         chat-buffer workspace root rel-path
         t  ; user-modified = t
         overlay-start
         tool-name)

        ;; Show the chat buffer to the user
        (display-buffer chat-buffer gptel-display-buffer-action)
        (goto-char overlay-start))

      (setq mevedel-tools--current-inline-preview-overlay nil))))

(defun mevedel-tools--next-preview-overlay ()
  "Jump to the next mevedel inline preview overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (end (overlay-end ov)))
    (when (get-char-property end 'mevedel-inline-preview)
      (goto-char end))))

(defun mevedel-tools--previous-preview-overlay ()
  "Jump to the previous mevedel inline preview overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (start (overlay-start ov)))
    (goto-char start)
    (when-let ((prev-ov (cdr (get-char-property-and-overlay
                              (1- start) 'mevedel-inline-preview))))
      (goto-char (overlay-start prev-ov)))))


;;
;;; Buffer preview

(defun mevedel-tools--prompt-for-changes ()
  "Prompt user to approve/reject/edit change.

Expects buffer-local variables to be set in
`mevedel--diff-preview-buffer-name' buffer."
  (let ((diff-buffer (get-buffer mevedel--diff-preview-buffer-name)))
    (unless diff-buffer
      (error "No diff-preview buffer found"))

    (pop-to-buffer diff-buffer)

    (let ((choice (read-char-choice
                   "Apply changes? (a)pprove, (r)eject, (e)dit, (f)eedback: "
                   '(?a ?r ?e ?f)))
          (temp-file (buffer-local-value 'mevedel--temp-file diff-buffer))
          (real-path (buffer-local-value 'mevedel--real-path diff-buffer))
          (final-callback (buffer-local-value 'mevedel--final-callback diff-buffer))
          (user-modified (buffer-local-value 'mevedel--user-modified diff-buffer))
          (original-wconf (buffer-local-value 'mevedel--original-window-config diff-buffer)))

      (pcase choice
        (?a
         ;; Approved - apply changes
         (with-current-buffer diff-buffer
           (mevedel-diff-apply-buffer))
         ;; Note: Patch buffer will be updated with final diffs at request end
         (funcall final-callback
                  (if user-modified
                      (format "Changes approved and applied to %s, but were modified by user via ediff. You may need to read the relevant sections of the file to see the final applied changes." real-path)
                    (format "Changes approved and applied to %s" real-path)))
         ;; Cleanup and restore window config
         (kill-buffer diff-buffer)
         (delete-file temp-file)
         (when (window-configuration-p original-wconf)
           (set-window-configuration original-wconf)))
        (?r
         ;; Rejected without feedback
         (funcall final-callback
                  "Changes rejected by user. No feedback provided.")
         ;; Cleanup and restore window config
         (kill-buffer diff-buffer)
         (delete-file temp-file)
         (when (window-configuration-p original-wconf)
           (set-window-configuration original-wconf)))
        (?e
         ;; Run `ediff' on patch - set up hook to return here after ediff
         ;; Do NOT restore window config - let ediff manage windows
         (with-current-buffer diff-buffer
           (setq-local mevedel--user-modified t))
         ;; Add one-shot hook to return to confirmation after ediff
         (add-hook 'mevedel--ediff-finished-hook #'mevedel-tools--prompt-for-changes)
         (with-current-buffer diff-buffer
           (mevedel-ediff-patch)))
        (?f
         ;; Rejected with feedback
         (let ((feedback (read-string "What should be changed? ")))
           (funcall final-callback
                    (format "Changes rejected by user. User feedback: %s" feedback)))
         ;; Cleanup and restore window config
         (kill-buffer diff-buffer)
         (delete-file temp-file)
         (when (window-configuration-p original-wconf)
           (set-window-configuration original-wconf)))))))


;;
;;; File Reading

(cl-defun mevedel-tools--read-file-lines (callback filename start-line end-line)
  "Return lines START-LINE to END-LINE fom FILENAME via CALLBACK."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--read-file-lines
                                  (filename stringp)
                                  (start-line integerp nil)
                                  (end-line integerp nil))

  (unless (file-readable-p filename)
    (cl-return-from mevedel-tools--read-file-lines
      (funcall callback (format "Error: File %s is not readable" filename))))

  (when (file-directory-p filename)
    (cl-return-from mevedel-tools--read-file-lines
      (funcall callback (format "Error: Cannot read directory %s as file" filename))))

  (when (file-symlink-p filename)
    (setq filename (file-truename filename)))

  ;; Check directory permissions
  (mevedel-tools--check-directory-permissions filename
                                              (format "Need to read file: %s" filename)
                                              mevedel-tools--read-file-lines callback)

  (if (and (not start-line) (not end-line)) ;read full file
      (if (> (file-attribute-size (file-attributes filename))
             (* 512 1024))
          (cl-return-from mevedel-tools--read-file-lines
            (funcall callback "Error: File is too large (> 512 KB).
Please specify a line range to read"))
        (with-temp-buffer
          (insert-file-contents filename)
          ;; Add line numbers (cat -n style)
          (goto-char (point-min))
          (let ((line-num 1)
                (max-lines (count-lines (point-min) (point-max))))
            ;; Calculate width for line numbers
            (let ((width (length (number-to-string max-lines))))
              (while (not (eobp))
                (insert (format (format "%%%dd\t" width) line-num))
                (forward-line 1)
                (setq line-num (1+ line-num)))))
          (funcall callback (buffer-substring-no-properties (point-min) (point-max)))))
    ;; TODO: Handle nil start-line OR nil end-line
    (let ((original-start-line start-line))  ; Store for line numbering
      (cl-decf start-line)
      (let* ((file-size (nth 7 (file-attributes filename)))
             (chunk-size (min file-size (* 512 1024)))
             (byte-offset 0) (line-offset (- end-line start-line)))
        (with-temp-buffer
          ;; Go to start-line
          (while (and (> start-line 0)
                      (< byte-offset file-size))
            (insert-file-contents
             filename nil byte-offset (+ byte-offset chunk-size))
            (setq byte-offset (+ byte-offset chunk-size))
            (setq start-line (forward-line start-line))
            (when (eobp)
              (if (/= (line-beginning-position) (line-end-position))
                  ;; forward-line counted 1 extra line
                  (cl-incf start-line))
              (delete-region (point-min) (line-beginning-position))))

          (delete-region (point-min) (point))

          ;; Go to end-line, forward by line-offset
          (cl-block nil
            (while (> line-offset 0)
              (setq line-offset (forward-line line-offset))
              (when (and (eobp) (/= (line-beginning-position) (line-end-position)))
                ;; forward-line counted 1 extra line
                (cl-incf line-offset))
              (if (= line-offset 0)
                  (delete-region (point) (point-max))
                (if (>= byte-offset file-size)
                    (cl-return)
                  (insert-file-contents
                   filename nil byte-offset (+ byte-offset chunk-size))
                  (setq byte-offset (+ byte-offset chunk-size))))))

          ;; Add line numbers (cat -n style)
          (goto-char (point-min))
          (let ((line-num original-start-line)
                (width (length (number-to-string end-line))))
            (while (not (eobp))
              (insert (format (format "%%%dd\t" width) line-num))
              (forward-line 1)
              (setq line-num (1+ line-num))))

          (funcall callback (buffer-substring-no-properties (point-min) (point-max))))))))

(cl-defun mevedel-tools--grep (callback regex path &optional glob context-lines)
  "Search for REGEX in file or directory at PATH using ripgrep.

CALLBACK is a function to call with the results.
REGEX is a PCRE-format regular expression to search for.
PATH can be a file or directory to search in.

Optional arguments:
GLOB restricts the search to files matching the glob pattern.
  Examples: \"*.el\", \"*.md\", \"*.rs\"
CONTEXT-LINES specifies the number of lines of context to show
  around each match (0-15 inclusive, defaults to 0).

Returns a string containing matches grouped by file, with line numbers
and optional context. Results are sorted by modification time and
limited to 1000 matches per file."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--grep
                                  (regex stringp)
                                  (path stringp)
                                  (glob stringp nil)
                                  (context-lines integerp nil))

  (unless (file-readable-p path)
    (cl-return-from mevedel-tools--grep
      (funcall callback (format "Error: File or directory %s is not readable" path))))
  (let ((grepper (executable-find "rg")))
    (unless grepper
      (cl-return-from mevedel-tools--grep
        (funcall callback "Error: `ripgrep` not installed. This tool cannot be used")))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions path
                                                (format "Need to grep in: %s" path)
                                                mevedel-tools--grep callback)

    (with-temp-buffer
      (let* ((args (delq nil (list "--sort=modified"
                                   (and (natnump context-lines)
                                        (format "--context=%d" context-lines))
                                   (and glob (format "--glob=%s" glob))
                                   ;; "--files-with-matches"
                                   "--max-count=1000"
                                   "--heading" "--line-number" "-e" regex
                                   (expand-file-name (substitute-in-file-name path)))))
             (exit-code (apply #'call-process grepper nil '(t t) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
        (funcall callback (buffer-string))))))


;;
;;; File Editing Tools

(cl-defun mevedel-tools--edit-files (callback path &optional old-str new-str-or-diff use-diff)
  "Edit file(s) at PATH using either string matching or unified diff.

This function supports two distinct modes of operation:

1. STRING REPLACEMENT MODE (USE-DIFF is nil or :json-false):
   - Searches for OLD-STR in the file at PATH
   - Replaces it with NEW-STR-OR-DIFF
   - Requires OLD-STR to match exactly once (uniquely) in the file
   - Only works on single files, not directories

2. DIFF/PATCH MODE (when USE-DIFF is non-nil and not :json-false):
   - Applies NEW-STR-OR-DIFF as a unified diff using the `patch` command
   - Works on both single files and directories
   - OLD-STR is ignored in this mode
   - NEW-STR-OR-DIFF can contain the diff in fenced code blocks
     (=diff or =patch)
   - Uses the -N (--forward) option to ignore already-applied patches


CALLBACK - async callback for results
PATH - file to edit
OLD-STR - text to find (string mode only)
NEW-STR-OR-DIFF - replacement text or diff
USE-DIFF - if non-nil, treat NEW-STR as diff

Workflow:
1. Check access to file, request inline if needed
2. Apply changes to temp copy of file
3. Generate diff showing actual changes
4. Show diff to user for approval
5. If approved: apply to real file and add to patch buffer
6. If rejected: optionally get feedback for LLM"
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--edit-files
                                  (path stringp)
                                  (new-str-or-diff stringp))

  (unless (file-readable-p path)
    (cl-return-from mevedel-tools--edit-files
      (funcall callback (format "Error: File or directory %s is not readable" path))))

  (let* ((expanded-path (expand-file-name path)))
    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions expanded-path
                                                (format "Need to edit file: %s" path)
                                                mevedel-tools--edit-files callback)

    (mevedel-tools--edit-files-1 callback expanded-path old-str new-str-or-diff use-diff)))

(cl-defun mevedel-tools--edit-files-1 (callback path old-str new-str-or-diff use-diff)
  "Perform the actual file edit operation for PATH.

CALLBACK - async callback for results
PATH - absolute path to file to edit
OLD-STR - text to find (string mode only)
NEW-STR-OR-DIFF - replacement text or diff
USE-DIFF - if non-nil, treat NEW-STR as diff

This is the internal function that does the actual work after access has
been verified."
  ;; Verify path is valid
  (unless (and path (stringp path))
    (cl-return-from mevedel-tools--edit-files-1
      (funcall callback (format "Error: Invalid path argument: %S" path))))

  ;; Snapshot the file(s) before any modifications
  ;; Use the SAME condition as the mode determination below
  (if (or (eq use-diff :json-false) old-str)
      ;; STRING MODE: Snapshot single file
      (mevedel--snapshot-file-if-needed path)
    ;; DIFF MODE: Extract files from diff and snapshot each
    (mevedel--snapshot-files-from-diff new-str-or-diff))

  (let* ((temp-file (make-temp-file "mevedel-edit-"))
         (original-content (when (file-exists-p path)
                             (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string)))))

    (condition-case err
        (progn
          ;; Copy original to temp file (empty if file doesn't exist)
          (with-temp-file temp-file
            (when original-content
              (insert original-content)))

          ;; Apply edit to temp file (string or diff mode)
          (if (or (eq use-diff :json-false) old-str)
              ;; STRING REPLACEMENT MODE
              (progn
                (when (file-directory-p path)
                  (cl-return-from mevedel-tools--edit-files-1
                    (funcall
                     callback
                     (format "Error: String replacement is intended for single files, not directories (%s)" path))))
                (mevedel-tools--apply-string-replacement temp-file old-str new-str-or-diff
                                                         (lambda (success-or-error)
                                                           (if (stringp success-or-error)
                                                               ;; Error
                                                               (progn
                                                                 (delete-file temp-file)
                                                                 (funcall callback success-or-error))
                                                             ;; Success - show diff and confirm
                                                             (mevedel-tools--show-changes-and-confirm
                                                              temp-file original-content path callback "Edit")))))

            ;; DIFF MODE
            (mevedel-tools--apply-diff-to-temp temp-file new-str-or-diff
                                               (lambda (success-or-error)
                                                 (if (stringp success-or-error)
                                                     ;; Error
                                                     (progn
                                                       (delete-file temp-file)
                                                       (funcall callback success-or-error))
                                                   ;; Success - show diff and confirm
                                                   (mevedel-tools--show-changes-and-confirm
                                                    temp-file original-content path callback "Edit"))))))

      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (funcall callback (format "Error: %s" (error-message-string err)))))))

(defun mevedel-tools--apply-string-replacement (temp-file old-str new-str-or-diff callback)
  "Apply string replacement to TEMP-FILE.
Calls CALLBACK with t on success or error string on failure.
CALLBACK is the function to call with the result.
OLD-STR and NEW-STR-OR-DIFF are the replacement parameters."
  (condition-case err
      (let (success)
        (with-temp-buffer
          (insert-file-contents temp-file)
          (goto-char (point-min))
          (if (search-forward old-str nil t)
              (if (save-excursion (search-forward old-str nil t))
                  (funcall callback "Error: Match is not unique.
Consider providing more context for the replacement, or a unified diff")
                ;; Unique match found - replace it
                (replace-match (string-replace "\\" "\\\\" new-str-or-diff))
                (write-region nil nil temp-file nil 'silent)
                (setq success t))
            (funcall callback (format "Error: Could not find old_str \"%s\" in file"
                                      (truncate-string-to-width old-str 20)))))
        (when success
          (funcall callback success)))
    (error
     (funcall callback (format "Error: %s" (error-message-string err))))))

(cl-defun mevedel-tools--apply-diff-to-temp (temp-file diff callback)
  "Apply DIFF to TEMP-FILE using patch command.
Calls CALLBACK with t on success or error string on failure."
  (unless (executable-find "patch")
    (cl-return-from mevedel-tools--apply-diff-to-temp
      (funcall callback "Error: Command \"patch\" not available, cannot apply diffs.
Use string replacement instead")))

  (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
         ;; Initialize to a known non-zero value
         (exit-status -1)
         (result-output ""))

    (unwind-protect
        (let ((default-directory (file-name-directory (expand-file-name temp-file)))
              (patch-options '("--forward" "--verbose")))

          (with-temp-message
              (format "Applying diff to: `%s` with options: %s"
                      temp-file patch-options)

            (with-temp-buffer
              (insert diff)
              ;; Ensure trailing newline
              (unless (eq (char-before (point-max)) ?\n)
                (goto-char (point-max))
                (insert "\n"))
              (goto-char (point-min))
              ;; Remove code fences if present
              (when (looking-at-p "^ *```\\(diff\\|patch\\)\n")
                (delete-line)
                (goto-char (point-max))
                (forward-line -1)
                (when (looking-at-p "^ *```")
                  (delete-line)))

              ;; Fix line numbers in hunk headers
              (mevedel-tools--fix-patch-headers)

              (setq exit-status
                    (apply #'call-process-region
                           (point-min) (point-max)
                           "patch" nil (list out-buf-name t)
                           nil (append patch-options (list (file-name-nondirectory temp-file)))))))
          ;; Retrieve content from buffers using their names
          (when-let* ((stdout-buf (get-buffer out-buf-name)))
            (when (buffer-live-p stdout-buf)
              (with-current-buffer stdout-buf
                (setq result-output (buffer-string)))))

          (if (= exit-status 0)
              (funcall callback t)
            (let ((err (format "Error: Failed to apply diff to %s (exit status %s).
Patch command options: %s
Patch STDOUT:\n%s"
                               temp-file exit-status patch-options
                               result-output)))
              (funcall callback err))))
      ;; Clean up
      (let ((stdout-buf-obj (get-buffer out-buf-name)))
        (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))))))

(defun mevedel-tools--fix-patch-headers ()
  "Fix line numbers in hunks in diff at point."
  ;; Find and process each hunk header
  (while (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) +\\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
    (let ((hunk-start (line-beginning-position))
          (orig-line (string-to-number (match-string 1)))
          (new-line (string-to-number (match-string 3)))
          (orig-count 0)
          (new-count 0))

      ;; Count lines in this hunk until we hit the next @@ or EOF
      (goto-char hunk-start)
      (forward-line 1)
      (save-match-data
        (while (and (not (eobp))
                    (not (looking-at-p "^@@")))
          (cond
           ;; Removed lines (not ---)
           ((looking-at-p "^-[^-]")
            (cl-incf orig-count))
           ;; Added lines (not +++)
           ((looking-at-p "^\\+[^+]")
            (cl-incf new-count))
           ;; Context lines (space at start)
           ((looking-at-p "^ ")
            (cl-incf orig-count)
            (cl-incf new-count)))
          (forward-line 1)))

      ;; Replace the hunk header with corrected counts
      (goto-char hunk-start)
      (delete-line)
      (insert (format "@@ -%d,%d +%d,%d @@\n"
                      orig-line orig-count new-line new-count)))))

(cl-defun mevedel-tools--insert-in-file (callback path line-number new-str)
  "Insert NEW-STR at LINE-NUMBER in file at PATH.

CALLBACK is a function to call with the result.
LINE-NUMBER conventions:
- 0 inserts at the beginning of the file
- -1 inserts at the end of the file
- N > 1 inserts before line N"
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--insert-in-file
                                  (path stringp)
                                  (line-number integerp)
                                  (new-str stringp))

  (unless (file-readable-p path)
    (cl-return-from mevedel-tools--insert-in-file
      (funcall callback (format "Error: File %s is not readable" path))))

  (when (file-directory-p path)
    (cl-return-from mevedel-tools--insert-in-file
      (funcall callback (format "Error: Cannot insert into directory %s" path))))

  ;; Snapshot the file before any modifications
  (mevedel--snapshot-file-if-needed path)

  (let ((temp-file (make-temp-file "mevedel-edit-")))
    (condition-case err
        (let* ((expanded-path (expand-file-name path))
               (original-content (with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string))))

          ;; Check directory permissions
          (mevedel-tools--check-directory-permissions expanded-path
                                                      (format "Need to insert into file: %s" path)
                                                      mevedel-tools--insert-in-file callback)

          (with-temp-file temp-file
            (insert original-content)

            (pcase line-number
              (0 (goto-char (point-min)))       ; Insert at the beginning
              (-1 (goto-char (point-max)))      ; Insert at the end
              (_ (goto-char (point-min))
                 (forward-line line-number)))   ; Insert before line N

            ;; Insert the new string
            (insert new-str)

            ;; Ensure there's a newline after the inserted text if not already present
            (unless (or (string-suffix-p "\n" new-str) (eobp))
              (insert "\n")))
          ;; Show diff and confirm
          (mevedel-tools--show-changes-and-confirm
           temp-file original-content path callback "Insert"))

      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (funcall callback (format "Error: %s" (error-message-string err)))))))


;;
;;; Todo List

(defvar-local mevedel-tools--todos nil)

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

(defun mevedel-tools--display-todo-overlay (todos)
  "Display a formatted task list in the buffer using an overlay.

TODOS is a list of plists with keys :content, :activeForm, and :status.
Completed items are displayed with strikethrough and shadow face.
Exactly one item should have status \"in_progress\".

The overlay is only displayed in the main LLM context, not when running
as an agent, to avoid cluttering the user's view with agent task lists."
  (let* ((info (gptel-fsm-info gptel--fsm-last))
         (context-ov (plist-get info :context)))
    ;; Only display if NOT in an agent context
    ;; Agent contexts have :context set to an overlay with 'gptel-agent property
    (unless (and (overlayp context-ov)
                 (overlay-get context-ov 'gptel-agent))
      (let* ((where-from
              (previous-single-property-change
               (plist-get info :position) 'gptel nil (point-min)))
             (where-to (plist-get info :position)))
        (unless (= where-from where-to)
          (pcase-let ((`(,_ . ,todo-ov)
                       (get-char-property-and-overlay where-from 'mevedel-tools--todos)))
            (if todo-ov
                ;; Move if reusing an old overlay and the text has changed.
                (move-overlay todo-ov where-from where-to)
              (setq todo-ov (make-overlay where-from where-to nil t))
              (overlay-put todo-ov 'mevedel-tools--todos t)
              (overlay-put todo-ov 'evaporate t)
              (overlay-put todo-ov 'priority -40)
              (overlay-put todo-ov 'keymap (define-keymap
                                             "<tab>" #'mevedel-toggle-todos
                                             "TAB"   #'mevedel-toggle-todos))
              (plist-put
               info :post              ; Don't use push, see note in gptel-anthropic
               (cons (lambda (&rest _)      ; Clean up header line after tasks are done
                       (when (and gptel-mode gptel-use-header-line header-line-format)
                         (setf (nth 2 header-line-format) gptel--header-line-info)))
                     (plist-get info :post))))
            (let* ((formatted-todos         ; Format the todo list
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
                             return (plist-get todo :activeForm)))
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
                     formatted-todos "\n"
                     mevedel-tools--hrule)))
              (overlay-put todo-ov 'after-string todo-display)
              (when (and gptel-mode gptel-use-header-line in-progress header-line-format)
                (setf (nth 2 header-line-format)
                      (concat (propertize
                               " " 'display
                               `(space :align-to (- right ,(+ 5 (length in-progress)))))
                              (propertize (concat "Task: " in-progress)
                                          'face 'font-lock-escape-face)))))))))))

(defun mevedel-tools--write-todo (todos)
  "Display a formatted task list in the buffer.

TODOS is a list of plists with keys :content, :activeForm, and :status.
Completed items are displayed with strikethrough and shadow face.
Exactly one item should have status \"in_progress\"."
  (mevedel-tools--validate-params nil nil
                                  (todos (vectorp . "array")))
  (setq mevedel-tools--todos todos)
  (mevedel-tools--display-todo-overlay todos)
  t)

(defun mevedel-tools--read-todo ()
  "Display a formatted task list in the buffer."
  (mevedel-tools--display-todo-overlay mevedel-tools--todos)
  mevedel-tools--todos)


;;
;;; Xref Integration

(cl-defun mevedel-tools--xref-find-references (callback identifier file-path)
  "Find references to IDENTIFIER in the current session's project.

CALLBACK is the async callback function to return results.
IDENTIFIER is the symbol to find references for.
FILE-PATH specifies which file's buffer context to use for the search."
  (require 'xref)
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--xref-find-references
                                  (identifier stringp)
                                  (file-path stringp))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path)))
         (identifier-str (format "%s" identifier)))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
                                                (format "Need to read file: %s" file-path)
                                                mevedel-tools--xref-find-references callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--xref-find-references
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (with-current-buffer target-buffer
      (condition-case err
          (let ((backend (xref-find-backend)))
            (if (not backend)
                (format "No xref backend available for %s" file-path)
              (let ((xref-items (xref-backend-references backend identifier-str)))
                (if xref-items
                    (funcall callback
                             (string-join
                              (mapcar (lambda (item)
                                        (let* ((location (xref-item-location item))
                                               (file (xref-location-group location))
                                               (marker (xref-location-marker location))
                                               (line (with-current-buffer (marker-buffer marker)
                                                       (save-excursion
                                                         (goto-char marker)
                                                         (line-number-at-pos))))
                                               (summary (xref-item-summary item)))
                                          (format "%s:%d: %s" file line summary)))
                                      xref-items)
                              "\n"))
                  (funcall callback (format "No references found for '%s'" identifier-str))))))
        (error
         (funcall callback (format "Error searching for '%s' in %s: %s"
                                   identifier-str file-path (error-message-string err))))))))

(cl-defun mevedel-tools--xref-find-apropos (callback pattern file-path)
  "Find symbols matching PATTERN across the entire project.

CALLBACK is the async callback function to call with results.
FILE-PATH specifies which file's buffer context to use for the search.
This function uses the session context to operate in the correct
project."
  (require 'xref)
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--xref-find-apropos
                                  (pattern stringp)
                                  (file-path stringp))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path)))
         (pattern-str (format "%s" pattern)))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
                                                (format "Need to read file: %s" file-path)
                                                mevedel-tools--xref-find-apropos callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--xref-find-apropos
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (with-current-buffer target-buffer
      (condition-case err
          (let ((backend (xref-find-backend)))
            (cond
             ((not backend)
              (funcall callback (format "No xref backend available for %s" file-path)))
             ;; Special handling for etags without tags table
             ((and (eq backend 'etags)
                   (not (or (and (boundp 'tags-file-name) tags-file-name
                                 (file-exists-p tags-file-name))
                            (and (boundp 'tags-table-list) tags-table-list
                                 (cl-some #'file-exists-p tags-table-list)))))
              (funcall callback (format "No tags table available for %s" file-path)))
             (t
              (let ((xref-items (xref-backend-apropos backend pattern-str)))
                (if xref-items
                    (funcall callback
                             (string-join
                              (mapcar (lambda (item)
                                        (let* ((location (xref-item-location item))
                                               (file (xref-location-group location))
                                               (marker (xref-location-marker location))
                                               (line (with-current-buffer (marker-buffer marker)
                                                       (save-excursion
                                                         (goto-char marker)
                                                         (line-number-at-pos))))
                                               (summary (xref-item-summary item)))
                                          (format "%s:%d: %s" file line summary)))
                                      xref-items)
                              "\n"))
                  (funcall callback (format "No symbols found matching pattern '%s'" pattern-str)))))))
        (error
         (funcall callback (format "Error searching for pattern '%s' in %s: %s"
                                   pattern-str file-path (error-message-string err))))))))


;;
;;; Imenu Integration

(cl-defun mevedel-tools--imenu-list-symbols (callback file-path)
  "List all symbols in FILE-PATH using imenu.
CALLBACK is the async callback function to call with results.
Returns a list of symbols with their types and positions."
  (require 'imenu)
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--imenu-list-symbols
                                  (file-path stringp))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
                                                (format "Need to read file: %s" file-path)
                                                mevedel-tools--imenu-list-symbols callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--imenu-list-symbols
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (condition-case err
        (with-current-buffer target-buffer
          ;; Generate or update imenu index
          (imenu--make-index-alist)
          (if imenu--index-alist
              (let ((results '()))
                ;; Process the imenu index
                (dolist (item imenu--index-alist)
                  (cond
                   ;; Skip special entries
                   ((string-match-p "^\\*" (car item)) nil)
                   ;; Handle simple entries (name . position)
                   ((markerp (cdr item))
                    (let ((line (line-number-at-pos (marker-position (cdr item)))))
                      (push (format "%s:%d: %s"
                                    file-path
                                    line
                                    (car item))
                            results)))
                   ;; Handle position numbers
                   ((numberp (cdr item))
                    (let ((line (line-number-at-pos (cdr item))))
                      (push (format "%s:%d: %s"
                                    file-path
                                    line
                                    (car item))
                            results)))
                   ;; Handle nested entries (category . items)
                   ((listp (cdr item))
                    (let ((category (car item)))
                      (dolist (subitem (cdr item))
                        (when (and (consp subitem)
                                   (or (markerp (cdr subitem))
                                       (numberp (cdr subitem))))
                          (let ((line (line-number-at-pos
                                       (if (markerp (cdr subitem))
                                           (marker-position (cdr subitem))
                                         (cdr subitem)))))
                            (push (format "%s:%d: [%s] %s"
                                          file-path
                                          line
                                          category
                                          (car subitem))
                                  results))))))))
                (if results
                    (funcall callback (string-join (nreverse results) "\n"))
                  (funcall callback (format "No symbols found in %s" file-path))))
            (funcall callback (format "No imenu support or no symbols found in %s" file-path))))
      (error
       (funcall callback (format "Error listing symbols in %s: %s"
                                 file-path (error-message-string err)))))))


;;
;;; Tree-sitter Integration

(defun mevedel-tools--treesit-info (callback file-path &optional line column whole_file include_ancestors include_children)
  "Get tree-sitter parse tree information for FILE-PATH.
CALLBACK is the async callback function to call with results.
Optional LINE and COLUMN specify the position (1-based line, 0-based column).
If WHOLE_FILE is non-nil, show the entire file's syntax tree.
If neither position is specified, defaults to current cursor position (point).
If INCLUDE_ANCESTORS is non-nil, include parent node hierarchy.
If INCLUDE_CHILDREN is non-nil, include child nodes."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--treesit-info
                                  (file-path stringp)
                                  (line integerp nil)
                                  (column integerp nil)
                                  (whole_file booleanp nil)
                                  (include_ancestors booleanp nil)
                                  (include_children booleanp nil))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
                                                (format "Need to read file: %s" file-path)
                                                mevedel-tools--treesit-info callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--treesit-info
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (condition-case err
        (if (not (treesit-available-p))
            (funcall callback "Tree-sitter is not available in this Emacs build")
          (with-current-buffer target-buffer
            (let* ((parsers (treesit-parser-list))
                   (parser (car parsers)))
              (if (not parser)
                  (funcall callback (format "No tree-sitter parser available for %s" file-path))
                (let* ((root-node (treesit-parser-root-node parser))
                       ;; Determine position from line/column or use current point
                       (pos (cond (whole_file nil)
                                  (line (mevedel-tools--treesit-line-column-to-point
                                         line (or column 0)))
                                  ;; Use current point in the target buffer
                                  (t (point))))
                       (node (if whole_file
                                 root-node
                               (treesit-node-at pos parser)))
                       (results '()))
                  (if (not node)
                      (funcall callback "No tree-sitter node found")
                    ;; For full tree, use a different display function
                    (if whole_file
                        (mevedel-tools--treesit-format-tree root-node 0 20)
                      ;; Basic node information for specific position
                      (push (format "Node Type: %s" (treesit-node-type node)) results)
                      (push (format "Range: %d-%d"
                                    (treesit-node-start node)
                                    (treesit-node-end node)) results)
                      (push (format "Text: %s"
                                    (truncate-string-to-width
                                     (treesit-node-text node t)
                                     80 nil nil "...")) results)

                      ;; Check if node is named
                      (when (treesit-node-check node 'named)
                        (push "Named: yes" results))

                      ;; Field name if available
                      (let ((field-name (treesit-node-field-name node)))
                        (when field-name
                          (push (format "Field: %s" field-name) results)))

                      ;; Include ancestors if requested
                      (when include_ancestors
                        (push "\nAncestors:" results)
                        (let ((parent (treesit-node-parent node))
                              (level 1))
                          (while (and parent (< level 10))
                            (push (format "  %s[%d] %s (%d-%d)"
                                          (make-string level ?-)
                                          level
                                          (treesit-node-type parent)
                                          (treesit-node-start parent)
                                          (treesit-node-end parent))
                                  results)
                            (setq parent (treesit-node-parent parent))
                            (cl-incf level))))

                      ;; Include children if requested
                      (when include_children
                        (push "\nChildren:" results)
                        (let ((child-count (treesit-node-child-count node))
                              (i 0))
                          (if (= child-count 0)
                              (push "  (no children)" results)
                            (while (< i (min child-count 20))
                              (let ((child (treesit-node-child node i)))
                                (when child
                                  (push (format "  [%d] %s%s (%d-%d)"
                                                i
                                                (treesit-node-type child)
                                                (if (treesit-node-check child 'named)
                                                    " (named)" "")
                                                (treesit-node-start child)
                                                (treesit-node-end child))
                                        results)))
                              (cl-incf i))
                            (when (> child-count 20)
                              (push (format "  ... and %d more children"
                                            (- child-count 20))
                                    results)))))

                      ;; Return formatted results
                      (funcall callback (string-join (nreverse results) "\n")))))))))
      (error
       (funcall callback (format "Error getting tree-sitter info for %s: %s"
                                 file-path (error-message-string err)))))))

(defun mevedel-tools--treesit-format-tree (node level max-depth)
  "Format NODE and its children as a tree string.
LEVEL is the current indentation level.
MAX-DEPTH is the maximum depth to traverse."
  (if (or (not node) (>= level max-depth))
      ""
    (let* ((indent (make-string (* level 2) ?\s))
           (type (treesit-node-type node))
           (named (if (treesit-node-check node 'named) " (named)" ""))
           (start (treesit-node-start node))
           (end (treesit-node-end node))
           (field-name (treesit-node-field-name node))
           (field-str (if field-name (format " [%s]" field-name) ""))
           (text (treesit-node-text node t))
           (text-preview (if (and (< (length text) 40)
                                  (not (string-match-p "\n" text)))
                             (format " \"%s\"" text)
                           ""))
           (result (format "%s%s%s%s (%d-%d)%s\n"
                           indent type named field-str
                           start end text-preview))
           (child-count (treesit-node-child-count node)))
      ;; Add children
      (dotimes (i child-count)
        (when-let ((child (treesit-node-child node i)))
          (setq result (concat result
                               (mevedel-tools--treesit-format-tree
                                child (1+ level) max-depth)))))
      result)))

(defun mevedel-tools--treesit-line-column-to-point (line column)
  "Convert LINE and COLUMN to point position in current buffer.
LINE is 1-based, COLUMN is 0-based (Emacs convention)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))


;;
;;; Present Plan

(cl-defun mevedel-tools--present-plan (callback plan)
  "Present PLAN to user for interactive feedback.

CALLBACK is the async callback function to call with user response.
PLAN is a plist with :title, :summary, and :sections keys."
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
        ((accept-plan
           ()
           "User accepts plan."
           (interactive)
           (let* ((filename (format "plan-%s.md" (format-time-string "%Y%m%d-%H%M%S")))
                  (filepath (expand-file-name filename mevedel-plans-directory)))
             ;; Ensure directory exists
             (unless (file-directory-p mevedel-plans-directory)
               (make-directory mevedel-plans-directory t))
             ;; Write plan to file
             (condition-case err
                 (progn
                   (write-region plan-markdown nil filepath nil 'silent)
                   (cleanup-and-return
                    (format "User accepted the plan.\n\nPlan saved to: %s\n\nPlease read this file to see the full implementation plan."
                            filepath)))
               (error
                (cleanup-and-return
                 (format "User accepted the plan, but failed to save to file: %S\n\nHere is the plan:\n\n%s"
                         err plan-markdown))))))

         (reject-plan
           ()
           "User rejects plan with feedback."
           (interactive)
           (let ((feedback (read-string "Feedback on this plan: ")))
             (cleanup-and-return
              (format "User rejected the plan.\n\nFeedback: %s\n\nOriginal plan:\n%s\n\nPlease revise the plan addressing this feedback."
                      feedback plan-markdown))))

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
              (insert " accept  ")
              (insert (propertize "C-c C-k" 'font-lock-face 'help-key-binding))
              (insert " reject\n")
              (insert (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t))))))

        ;; Create overlay for interactivity
        (setq overlay (make-overlay start (point-max) chat-buffer))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'mevedel-plan t)

        ;; Define keybindings
        (define-key keymap (kbd "RET") #'accept-plan)
        (define-key keymap (kbd "<return>") #'accept-plan)
        (define-key keymap (kbd "C-c C-k") #'reject-plan)
        (define-key keymap (kbd "q") #'reject-plan)
        (overlay-put overlay 'keymap keymap)

        ;; Focus user attention
        (goto-char (point-max))
        (goto-char start)))))


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
           "Cancel the questionnaire."
           (interactive)
           (cleanup-and-return "User cancelled questionnaire"))

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
                 (insert (propertize "C-c C-k" 'font-lock-face 'help-key-binding))
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

                 (overlay-put overlay 'keymap keymap)
                 (goto-char start)))

             ;; Show buffer
             (display-buffer chat-buffer gptel-display-buffer-action)))

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
               (insert (propertize "C-c C-e" 'font-lock-face 'help-key-binding))
               (insert " edit  ")
               (insert (propertize "C-c C-k" 'font-lock-face 'help-key-binding))
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
               (define-key keymap (kbd "C-c C-k") #'quit-questionnaire)

               (overlay-put overlay 'keymap keymap)
               (goto-char start)))

           (display-buffer chat-buffer gptel-display-buffer-action))

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
;;; Hint Tracking (Teaching Preset)

(defvar-local mevedel-tools--hint-history nil
  "Buffer-local hint history per directive.
Format: ((directive-uuid . ((hints . [list of hint records])
                            (hint-count . number))))")

(defvar mevedel-tools--hrule-hints
  (propertize (concat (make-string 70 ?─) "\n")
              'face 'font-lock-comment-face)
  "Horizontal rule for hint display.")

(defun mevedel-tools--display-hint-overlay (directive-data)
  "Display hint history in the buffer using an overlay.
DIRECTIVE-DATA is the data for the current directive."
  (let* ((info (gptel-fsm-info gptel--fsm-last))
         (context-ov (plist-get info :context)))
    ;; Only display if NOT in an agent context
    (unless (and (overlayp context-ov)
                 (overlay-get context-ov 'gptel-agent))
      (let* ((where-from
              (previous-single-property-change
               (plist-get info :position) 'gptel nil (point-min)))
             (where-to (plist-get info :position)))
        (unless (= where-from where-to)
          (pcase-let ((`(,_ . ,hint-ov)
                       (get-char-property-and-overlay where-from 'mevedel-tools--hints)))
            (if hint-ov
                ;; Move if reusing an old overlay
                (move-overlay hint-ov where-from where-to)
              (setq hint-ov (make-overlay where-from where-to nil t))
              (overlay-put hint-ov 'mevedel-tools--hints t)
              (overlay-put hint-ov 'evaporate t)
              (overlay-put hint-ov 'priority -40)
              (overlay-put hint-ov 'keymap (define-keymap
                                             "<tab>" #'mevedel-toggle-hints
                                             "TAB"   #'mevedel-toggle-hints)))
            (let* ((hints (reverse (alist-get 'hints directive-data)))
                   (hint-count (or (alist-get 'hint-count directive-data) 0))
                   (concepts-explained (delete-dups (mapcar (lambda (h) (plist-get h :concept)) hints)))
                   (suggested-depth (mevedel-tools--calculate-hint-depth hint-count))
                   (formatted-hints
                    (if hints
                        (mapconcat
                         (lambda (hint)
                           (let* ((type (plist-get hint :type))
                                  (depth (plist-get hint :depth))
                                  (summary (plist-get hint :summary))
                                  (concept (plist-get hint :concept))
                                  (icon (pcase type
                                          ("socratic-question" "?")
                                          ("technique-hint" "🗬")
                                          ("doc-reference" "🕮")
                                          ("problem-decomposition" "→")
                                          (_ "•")))
                                  (depth-color (cond
                                                ((<= depth 2) 'success)
                                                ((<= depth 4) 'warning)
                                                (t 'error))))
                             (concat icon " "
                                     (propertize (format "[depth %d] " depth)
                                                 'face `(:inherit ,depth-color))
                                     summary
                                     (propertize (format " (%s)" concept)
                                                 'face 'font-lock-comment-face))))
                         hints "\n")
                      (propertize "No hints given yet"
                                  'face 'font-lock-comment-face)))
                   (hint-display
                    (concat
                     (unless (= (char-before (overlay-end hint-ov)) 10) "\n")
                     mevedel-tools--hrule-hints
                     (propertize "Hint History: [ "
                                 'face '(:inherit font-lock-comment-face :inherit bold))
                     (save-excursion
                       (goto-char (1- (overlay-end hint-ov)))
                       (propertize (substitute-command-keys "\\[mevedel-toggle-hints]")
                                   'face 'help-key-binding))
                     (propertize " to toggle display ]\n" 'face 'font-lock-comment-face)
                     (propertize (format "Hints given: %d | Suggested depth: %d/5\n"
                                         hint-count suggested-depth)
                                 'face 'font-lock-doc-face)
                     (when concepts-explained
                       (propertize (format "Concepts: %s\n"
                                           (mapconcat #'identity concepts-explained ", "))
                                   'face 'font-lock-string-face))
                     formatted-hints "\n"
                     mevedel-tools--hrule-hints)))
              (overlay-put hint-ov 'after-string hint-display))))))))

(defun mevedel-toggle-hints ()
  "Toggle hint history display visibility."
  (interactive)
  (if-let* ((ov (cl-loop for ov in (overlays-in (point-min) (point-max))
                         when (overlay-get ov 'mevedel-tools--hints)
                         return ov)))
      (if (overlay-get ov 'after-string)
          (overlay-put ov 'after-string nil)
        ;; Regenerate display
        (let* ((directive-uuid mevedel--current-directive-uuid)
               (directive-data (alist-get directive-uuid mevedel-tools--hint-history)))
          (mevedel-tools--display-hint-overlay directive-data)))
    (message "No hint history found")))

(defun mevedel-tools--calculate-hint-depth (hint-count)
  "Calculate suggested hint depth based on HINT-COUNT.
More hints given suggests the user is struggling more and needs
more detailed guidance.  Returns depth from 1 (gentle) to 5 (very detailed)."
  (cond
   ((< hint-count 2) 1)   ; First hint, be gentle
   ((< hint-count 4) 2)   ; A few hints, still gentle
   ((< hint-count 7) 3)   ; Multiple hints, medium detail
   ((< hint-count 10) 4)  ; Many hints, more detail
   (t 5)))                ; Lots of hints, very detailed

(defun mevedel-tools--record-hint (hint_type concept hint_summary depth)
  "Record a hint.  Called by RecordHint tool.
HINT_TYPE is the teaching method used.
CONCEPT is the topic addressed.
HINT_SUMMARY is a one-line description.
DEPTH is the hint detail level (1-5)."
  (let* ((directive-uuid mevedel--current-directive-uuid)
         (timestamp (current-time))
         (hint-record (list :type hint_type
                            :concept concept
                            :summary hint_summary
                            :depth depth
                            :timestamp timestamp))
         (directive-data (alist-get directive-uuid mevedel-tools--hint-history))
         (hints (alist-get 'hints directive-data)))
    ;; Add hint
    (push hint-record hints)
    (setf (alist-get 'hints directive-data) hints)
    (setf (alist-get 'hint-count directive-data)
          (1+ (or (alist-get 'hint-count directive-data) 0)))
    (setf (alist-get directive-uuid mevedel-tools--hint-history) directive-data)
    ;; Update overlay display
    (mevedel-tools--display-hint-overlay directive-data)
    ;; Return confirmation (visible to user)
    (format "✓ Hint recorded: %s (depth %d)" hint_summary depth)))

(defun mevedel-tools--get-hints ()
  "Retrieve hint history.  Called by GetHints tool."
  (let* ((directive-uuid mevedel--current-directive-uuid)
         (directive-data (alist-get directive-uuid mevedel-tools--hint-history))
         (hints (reverse (alist-get 'hints directive-data)))  ; Chronological
         (hint-count (or (alist-get 'hint-count directive-data) 0))
         (concepts-explained (delete-dups (mapcar (lambda (h) (plist-get h :concept)) hints)))
         (suggested-depth (mevedel-tools--calculate-hint-depth hint-count)))
    ;; Update overlay display
    (mevedel-tools--display-hint-overlay directive-data)
    ;; Return formatted history (visible to user AND LLM)
    (concat
     (format "=== Hint History ===\n\n")
     (format "Hints given: %d\n" hint-count)
     (format "Suggested hint depth: %d/5\n\n" suggested-depth)
     (if hints
         (concat
          "Previous hints:\n"
          (mapconcat
           (lambda (hint)
             (format "- [%s, depth %d] %s (concept: %s)"
                     (plist-get hint :type)
                     (plist-get hint :depth)
                     (plist-get hint :summary)
                     (plist-get hint :concept)))
           hints "\n")
          (format "\n\nConcepts explained: %s\n"
                  (mapconcat #'identity concepts-explained ", ")))
       "No hints given yet. Start with gentle guidance (depth 1-2).\n"))))


;;
;;; Register Tools

;;;###autoload
(defun mevedel--define-read-tools ()
  "Define custom read-only tools for `mevedel'."

  (gptel-make-tool
   :name "TodoWrite"
   :description "Create and manage a structured task list for your current session. \
Helps track progress and organize complex tasks. Use proactively for multi-step work.

Only one todo can be `in_progress` at a time."
   :function #'mevedel-tools--write-todo
   :args
   '((:name "todos"
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
This tool should be used proactively and frequently to ensure that you are aware of the status of the current task list.
You should make use of this tool as often as possible, especially in the following situations:

- At the beginning of conversations to see what's pending
- Before starting new tasks to prioritize work
- When the user asks about previous tasks or plans
- Whenever you're uncertain about what to do next
- After completing tasks to update your understanding of remaining work
- After every few messages to ensure you're on track

Usage:
- This tool takes in no parameters. So leave the input blank or empty. DO NOT include a dummy object, placeholder string or a key like \"input\" or \"empty\". LEAVE IT BLANK.
- Returns a list of todo items with their status and content
- Use this information to track progress and plan next steps
- If no todos exist yet, an empty list will be returned"
   :function #'mevedel-tools--read-todo
   :args nil
   :category "mevedel")

  ;; Adapted from claude-code-ide.el

  (gptel-make-tool
   :name "XrefReferences"
   :description "Find where a function, variable, or class is used throughout your codebase.
Perfect for understanding code dependencies and impact analysis"
   :function #'mevedel-tools--xref-find-references
   :args '((:name "identifier"
            :type string
            :description "The identifier to find references for")
           (:name "file_path"
            :type string
            :description "File path to use as context for the search"))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "XrefDefinitions"
   :description "Search for functions, variables, or classes by name pattern across your project.
Helps you discover code elements when you know part of the name"
   :function #'mevedel-tools--xref-find-apropos
   :args '((:name "pattern"
            :type string
            :description "The pattern to search for symbols")
           (:name "file_path"
            :type string
            :description "File path to use as context for the search"))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "Imenu"
   :description "Navigate and explore a file's structure by listing all its functions, classes, and variables with their locations."
   :function #'mevedel-tools--imenu-list-symbols
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze for symbols"))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "Treesitter"
   :description "Get tree-sitter syntax tree information for a file, including node types, ranges, and hierarchical structure.
Useful for understanding code structure and AST analysis"
   :function #'mevedel-tools--treesit-info
   :args
   '((:name "file_path"
      :type string
      :description "Path to the file to analyze")
     (:name "line"
      :type number
      :optional t
      :description "Line number (1-based)")
     (:name "column"
      :type number
      :optional t
      :description "Column number (0-based)")
     (:name "whole_file"
      :type boolean
      :optional t
      :description "Show the entire file's syntax tree")
     (:name "include_ancestors"
      :type boolean
      :optional t
      :description "Include parent node hierarchy")
     (:name "include_children"
      :type boolean
      :optional t
      :description "Include child nodes"))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "Glob"
   :description "Recursively find files matching a provided glob pattern.

- Supports glob patterns like \"*.md\" or \"*test*.py\".
  The glob applies to the basename of the file (with extension).
- Returns matching file paths at all depths sorted by modification time.
  Limit the depth of the search by providing the `depth` argument.
- When you are doing an open ended search that may require multiple rounds
  of globbing and grepping, use the \"task\" tool instead
- You can call multiple tools in a single response.  It is always better to
  speculatively perform multiple searches in parallel if they are potentially useful."
   :function (lambda (callback pattern &optional path depth)
               (cl-block nil
                 ;; Validate input
                 (mevedel-tools--validate-params callback nil
                                                 (pattern stringp)
                                                 (path stringp nil)
                                                 (depth integerp nil))

                 (when (string-empty-p pattern)
                   (cl-return
                    (funcall callback "Error: pattern must not be empty")))
                 (if path
                     (unless (and (file-readable-p path) (file-directory-p path))
                       (cl-return
                        (funcall callback (format "Error: path %s is not readable" path))))
                   (setq path "."))
                 (unless (executable-find "rg")
                   (cl-return
                    (funcall callback "Error: `ripgrep` not installed. This tool cannot be used")))

                 ;; Check directory permissions
                 (mevedel-tools--check-directory-permissions path (format "Need to find files in: %s" path) nil callback)

                 (with-temp-buffer
                   (let* ((args (list "--files" "--hidden" "--color=never"
                                      "--follow" "--sort" "modified"
                                      "--iglob" pattern))
                          (args (if (natnump depth)
                                    (nconc args (list "--max-depth" (number-to-string depth)))
                                  args))
                          (args (nconc args (ensure-list (expand-file-name path))))
                          (exit-code (apply #'call-process "rg" nil t nil args)))
                     (when (/= exit-code 0)
                       (goto-char (point-min))
                       (insert (format "Glob failed with exit code %d\n.STDOUT:\n\n"
                                       exit-code))))
                   (funcall callback (buffer-string)))))
   :args '((:name "pattern"
            :type string
            :description "Glob pattern to match, for example \"*.el\". Must not be empty.
Use \"*\" to list all files in a directory.")
           (:name "path"
            :type string
            :description "Directory to search in.  Supports relative paths and defaults to \".\""
            :optional t)
           (:name "depth"
            :description "Limit directory depth of search, 1 or higher. Defaults to no limit."
            :type integer
            :optional t))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "Read"
   :description "Read file contents between specified line numbers `start_line` and `end_line`,
with both ends included.

Consider using the \"grep_files\" tool to find the right range to read first.

Reads the whole file if the line range is not provided.

Files over 512 KB in size can only be read by specifying a line range."
   :function #'mevedel-tools--read-file-lines
   :args '((:name "file_path"
            :type string
            :description "The path to the file to be read."
            :type string)
           (:name "start_line"
            :type integer
            :description "The line to start reading from, defaults to the start of the file"
            :optional t)
           (:name "end_line"
            :type integer
            :description "The line up to which to read, defaults to the end of the file."
            :optional t))
   :category "mevedel"
   :async t
   :include t)

  (gptel-make-tool
   :name "Grep"
   :description "Search for text in file(s) at `path`.

Use this tool to find relevant parts of files to read.

Returns a list of matches prefixed by the line number, and grouped by file.
Can search an individual file (if providing a file path) or a directory.
Consider using this tool to find the right line range for the \"read_file_lines\" tool.

When searching directories, optionally restrict the types of files in the search with a `glob`.
Can request context lines around each match using the `context_lines` parameters."
   :function #'mevedel-tools--grep
   :args '((:name "regex"
            :description "Regular expression to search for in file contents."
            :type string)
           (:name "path"
            :description "File or directory to search in."
            :type string)
           (:name "glob"
            :description "Optional glob to restrict file types to search for.
Only required when path is a directory.
Examples: *.md, *.rs"
            :type string
            :optional t)
           (:name "context_lines"
            :description "Number of lines of context to retrieve around each match (0-15 inclusive).
Optional, defaults to 0."
            :optional t
            :type integer
            :maximum 15))
   :async t
   :category "mevedel")

  ;; Tool for LLM to ask user questions during execution
  (gptel-make-tool
   :name "Ask"
   :function #'mevedel-tools--ask-user
   :description "Ask the user one or more questions and wait for their responses.
Use this when you need clarification or user input to proceed with a task.

Supports multiple questions in a single call with navigation between them.
Each question MUST provide predefined answer options. Users can always provide custom input."
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

  ;; Tool for presenting interactive implementation plans
  (gptel-make-tool
   :name "PresentPlan"
   :function #'mevedel-tools--present-plan
   :description "Present an implementation plan to the user and wait for feedback.

**IMPORTANT**: This MUST be your FINAL tool call. Do not call any other tools or add text after this.

Use this tool after drafting a plan to get user approval. The plan will be displayed
inline in the chat buffer with interactive controls, and user feedback will be returned
automatically.

User can:
- Accept the plan (your task is complete, approved plan is returned)
- Reject the plan with feedback (you revise and call PresentPlan again)

This tool handles all user interaction - treat it as your exit point."
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

  ;; Tool for LLM to request access to new directories
  (gptel-make-tool
   :name "RequestAccess"
   :function (lambda (callback directory reason)
               "Request user permission to access a directory.

CALLBACK is for async execution.
DIRECTORY is the path to request access to.
REASON explains why access is needed."
               (cl-block nil
                 ;; Validate input
                 (mevedel-tools--validate-params callback nil (directory stringp) (reason stringp))

                 (unless (and (file-readable-p directory) (file-directory-p directory))
                   (cl-return
                    (funcall callback (format "Error: directory '%s' is not readable" directory))))
                 (let ((expanded (expand-file-name directory)))
                   (if (mevedel-tools--request-access expanded reason)
                       (funcall callback
                                (format "Access granted to %s. You can now read and write files in this directory." expanded))
                     (funcall callback
                              (format "Access denied to %s. You cannot access files in this directory." expanded))))))
   :description "Request access to a directory outside the current allowed project roots. You must explain why you need access to this directory."
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
   :name "Bash"
   :function (lambda (callback command)
               "Execute a bash command and return its output.

COMMAND is the bash command string to execute."
               (cl-block nil
                 ;; Validate input
                 (mevedel-tools--validate-params callback nil (command stringp))

                 ;; Check permissions
                 (let ((permission (mevedel-tools--check-bash-permission command)))
                   (cond
                    ;; Denied by permission rules
                    ((eq permission 'deny)
                     (cl-return
                      (funcall callback (format "Error: Command denied by permission rules: %s" command))))

                    ;; Ask user for confirmation with overlay
                    ((eq permission 'ask)
                     (let ((result (mevedel--prompt-user-for-bash-command command)))
                       (unless (eq result t)
                         (cl-return
                          (funcall callback
                                   (if (consp result)
                                       (format "Error: Command execution cancelled by user. Feedback: %s"
                                               (cdr result))
                                     "Error: Command execution cancelled by user"))))))

                    ;; Allow - proceed with execution
                    ((eq permission 'allow)
                     nil))) ; continue to execution

                 ;; Execute command
                 (with-temp-buffer
                   (let* ((exit-code (call-process "bash" nil (current-buffer) nil "-c" command))
                          (output (buffer-string)))
                     (if (zerop exit-code)
                         (funcall callback output)
                       (funcall callback (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s" exit-code output)))))))
   :description "Execute Bash commands.

This tool provides access to a Bash shell with GNU coreutils (or equivalents) available.
Use this to inspect system state, run builds, tests or other development or system administration tasks.

Do NOT use this for file operations, finding, reading or editing files.
Use the provided file tools instead: `read_file_lines`, `write_file`, `edit_files`, \
`glob_files`, `grep_files`

- Quote file paths with spaces using double quotes.
- Chain dependent commands with && (or ; if failures are OK)
- Use absolute paths instead of cd when possible
- For parallel commands, make multiple `execute_bash` calls in one message
- Run tests, check your work or otherwise close the loop to verify changes you make.

EXAMPLES:
- List files with details: 'ls -lah /path/to/dir'
- Find recent errors: 'grep -i error /var/log/app.log | tail -20'
- Check file type: 'file document.pdf'
- Count lines: 'wc -l *.txt'

The command will be executed in the current working directory.  Output is
returned as a string.  Long outputs should be filtered/limited using pipes."
   :args '((:name "command"
            :type string
            :description "The Bash command to execute.  \
Can include pipes and standard shell operators.
Example: 'ls -la | head -20' or 'grep -i error app.log | tail -50'"))
   :async t
   :confirm nil  ;; Permission checking handled by mevedel-tools--check-bash-permission
   :include t
   :category "mevedel")

  (gptel-make-tool
   :name "GetHints"
   :description "Retrieve the history of hints given for the current directive.

Use this tool at the START of each teaching interaction to:
1. See what hints have already been given
2. Avoid repeating hints
3. Determine appropriate depth for next hint
4. Build on previous explanations

Returns:
- List of previous hints with types, concepts, and summaries
- Suggested next hint depth based on history
- Concepts already explained (to avoid repetition)"
   :function #'mevedel-tools--get-hints
   :args nil
   :category "mevedel")

  (gptel-make-tool
   :name "RecordHint"
   :description "Record a hint that you just gave to the user.

Use this tool EVERY TIME you provide a hint, question, or guidance. This helps track
what has been explained and prevents repetition.

Parameters:
- hint_type: One of 'socratic-question', 'technique-hint', 'doc-reference', 'problem-decomposition'
- concept: Brief description of what this hint addresses (e.g., 'closure-capture', 'async-await')
- hint_summary: One-line summary of the hint (shown to user in hint history)
- depth: Hint detail level 1-5 (1=gentle nudge, 5=very detailed)

Example: After asking 'What happens when the closure captures the loop variable?'
Call: RecordHint(hint_type='socratic-question', concept='closure-capture',
                hint_summary='Asked about closure variable capture', depth=2)"
   :function #'mevedel-tools--record-hint
   :args (list '(:name "hint_type"
                 :type string
                 :enum ["socratic-question" "technique-hint" "doc-reference" "problem-decomposition"])
               '(:name "concept"
                 :type string)
               '(:name "hint_summary"
                 :type string)
               '(:name "depth"
                 :type number))
   :category "mevedel"))

;;;###autoload
(defun mevedel--define-edit-tools ()
  "Define custom mevedel tools."

  (gptel-make-tool
   :name "MkDir"
   :description "Create a new directory with the given name in the specified parent directory"
   :function (lambda (parent name)
               ;; Validate input
               (mevedel-tools--validate-params nil nil (parent stringp) (name stringp))
               ;; Check directory permissions
               (mevedel-tools--check-directory-permissions parent
                                                           (format "Need to create directory in: %s" parent) nil nil)

               (condition-case errdata
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s:\n%S" name parent errdata))))
   :args (list '(:name "parent"
                 :type string
                 :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                 :type string
                 :description "The name of the new directory to create, e.g. testdir"))
   :category "mevedel"
   :confirm t)

  (gptel-make-tool
   :name "Write"
   :description "Create a new file with the specified content.
Overwrites an existing file, so use with care!
Consider using the more granular tools \"Insert\" or \"Edit\" first."
   :function (lambda (callback path filename content)
               (cl-block nil
                 ;; Validate input
                 (mevedel-tools--validate-params callback nil
                                                 (path stringp)
                                                 (filename stringp)
                                                 (content stringp))

                 (let* ((full-path (expand-file-name filename path)))
                   ;; Check directory permissions
                   (mevedel-tools--check-directory-permissions full-path
                                                               (format "Need to create %s in directory: %s" filename path)
                                                               nil callback)

                   ;; Snapshot the file before any modifications
                   (mevedel--snapshot-file-if-needed full-path)
                   ;; Access granted, proceed
                   (condition-case errdata
                       (let* ((temp-file (make-temp-file "mevedel-edit-" nil nil content))
                              (original-content (when (file-exists-p full-path)
                                                  (with-temp-buffer
                                                    (insert-file-contents full-path)
                                                    (buffer-string)))))
                         ;; Show diff and confirm
                         (mevedel-tools--show-changes-and-confirm
                          temp-file original-content full-path callback "Write"))
                     (error (funcall callback (format "Error: Could not write file %s:\n%S" path errdata)))))))
   :args (list '(:name "path"
                 :type string
                 :description "The directory where to create the file, \".\" is the current directory.")
               '(:name "filename"
                 :type string
                 :description "The name of the file to create.")
               '(:name "content"
                 :type string
                 :description "The content to write to the file"))
   :category "mevedel"
   :async t
   :confirm t)

  ;; Custom Edit tool with user confirmation
  (gptel-make-tool
   :name "Edit"
   :function #'mevedel-tools--edit-files
   :description "Replace text in one or more files.

To edit a single file, provide the file `path`.

For the replacement, there are two methods:
- Short replacements: Provide both `old_str` and `new_str`, in which case `old_str` \
needs to exactly match one unique section of the original file, including any whitespace.  \
Make sure to include enough context that the match is not ambiguous.  \
The entire original string will be replaced with `new str`.
- Long or involved replacements: set the `diff` parameter to true and provide a unified diff \
in `new_str`. `old_str` can be ignored.

To edit multiple files,
- provide the directory path,
- set the `diff` parameter to true
- and provide a unified diff in `new_str`.

Diff instructions:

- The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format.
- The LLM should generate the diff such that the file paths within the diff \
  (e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'path'.

To simply insert text at some line, use the \"Insert\" instead."
   :args '((:name "path"
            :type string
            :description "File path or directory to edit")
           (:name "old_str"
            :type string
            :optional t
            :description "Original string to replace. If providing a unified diff, this should be false")
           (:name "new_str"
            :type string
            :description "Replacement text (for string mode) or unified diff (for diff mode)")
           (:name "use_diff"
            :type boolean
            :description "If true, new_str is treated as a unified diff to apply"))
   :async t
   :include t
   :category "mevedel")

  (gptel-make-tool
   :name "Insert"
   :description "Insert `new_str` after `line_number` in file at `path`.

Use this tool for purely additive actions: adding text to a file at a \
specific location with no changes to the surrounding context."
   :function #'mevedel-tools--insert-in-file
   :args '((:name "path"
            :description "Path of file to edit."
            :type string)
           (:name "line_number"
            :description "The line number at which to insert `new_str`, with
- 0 to insert at the beginning, and
- -1 to insert at the end."
            :type integer)
           (:name "new_str"
            :description "String to insert at `line_number`."
            :type string))
   :category "mevedel"
   :async t
   :include t))

(provide 'mevedel-tools)
;;; mevedel-tools.el ends here
