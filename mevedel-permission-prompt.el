;;; mevedel-permission-prompt.el -- Permission prompt UI -*- lexical-binding: t -*-

;;; Commentary:

;; Renders and settles generic, Bash, Eval, and execution-authority prompts.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--data-buffer
                  "mevedel-interaction-prompt" (&optional buffer))
(declare-function mevedel--prompt--overlay-at-point
                  "mevedel-interaction-prompt" (property))
(declare-function mevedel--prompt--register-canceller
                  "mevedel-interaction-prompt" (buffer overlay))
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay result))
(declare-function mevedel--prompt-announce
                  "mevedel-interaction-prompt" (overlay))
(declare-function mevedel--prompt-attribution-line
                  "mevedel-interaction-prompt" (origin))
(declare-function mevedel--prompt-framed-body
                  "mevedel-interaction-prompt" (body face))
(declare-function mevedel--prompt-key
                  "mevedel-interaction-prompt" (key))
(defvar mevedel--prompt-overlays)

(autoload 'mevedel--prompt--data-buffer "mevedel-interaction-prompt")
(autoload 'mevedel--prompt--overlay-at-point "mevedel-interaction-prompt")
(autoload 'mevedel--prompt--register-canceller "mevedel-interaction-prompt")
(autoload 'mevedel--prompt--settle "mevedel-interaction-prompt")
(autoload 'mevedel--prompt-announce "mevedel-interaction-prompt")
(autoload 'mevedel--prompt-attribution-line "mevedel-interaction-prompt")
(autoload 'mevedel--prompt-framed-body "mevedel-interaction-prompt")
(autoload 'mevedel--prompt-key "mevedel-interaction-prompt")

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue--log
                  "mevedel-permission-queue" (event entry &optional session &rest props))
(declare-function mevedel-permission-queue--render-head
                  "mevedel-permission-queue" (&optional session))
(declare-function mevedel-permission-queue-justification
                  "mevedel-permission-queue" (entry))
(declare-function mevedel-permission-queue-resource-needs-tree-p
                  "mevedel-permission-queue" (entry grant))
(declare-function mevedel-permission-queue-select-resource
                  "mevedel-permission-queue" (entry previous selected))
(declare-function mevedel-permission-queue-validate-approval
                  "mevedel-permission-queue" (entry outcome))
(autoload 'mevedel-permission-queue--log "mevedel-permission-queue")
(autoload 'mevedel-permission-queue--render-head "mevedel-permission-queue")
(autoload 'mevedel-permission-queue-justification "mevedel-permission-queue")
(autoload 'mevedel-permission-queue-resource-needs-tree-p "mevedel-permission-queue")
(autoload 'mevedel-permission-queue-select-resource "mevedel-permission-queue")
(autoload 'mevedel-permission-queue-validate-approval "mevedel-permission-queue")

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-resource-granted-p
                  "mevedel-permission-rules" (path access grants &optional recursive))
(autoload 'mevedel-permission-rules-resource-granted-p "mevedel-permission-rules")

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get
                  "mevedel-queue" (entry key))
(declare-function mevedel-queue--entry-metadata-put
                  "mevedel-queue" (entry key value))
(autoload 'mevedel-queue--entry-metadata-get "mevedel-queue")
(autoload 'mevedel-queue--entry-metadata-put "mevedel-queue")

;; `mevedel-side-conversation'
(declare-function mevedel-side-conversation-mutation-warning
                  "mevedel-side-conversation" (record effect))
(declare-function mevedel-side-conversation-mutation-warning-pending-p
                  "mevedel-side-conversation" (record))
(autoload 'mevedel-side-conversation-mutation-warning
  "mevedel-side-conversation")
(autoload 'mevedel-side-conversation-mutation-warning-pending-p
  "mevedel-side-conversation")

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (session))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (data-buffer))
(autoload 'mevedel-view--interaction-register "mevedel-view-interaction")
(autoload 'mevedel-view--interaction-target-buffer
  "mevedel-view-interaction")


;;
;;; Permission prompt controls

(defun mevedel-permission--display-path (entry path)
  "Return PATH for display in the execution domain of ENTRY."
  (if-let* ((session (plist-get entry :session))
            (target (mevedel-session-execution-target session))
            ((mevedel-execution-target-remote-p target))
            ((file-remote-p path)))
      (mevedel-execution-target-native-path target path)
    path))

(defun mevedel-permission--resource-label (entry grant)
  "Return a display label for resource GRANT in ENTRY's path domain."
  (format "%s %s%s"
          (capitalize (symbol-name (plist-get grant :access)))
          (mevedel-permission--display-path entry (plist-get grant :path))
          (if (plist-get grant :recursive) " (recursive)" " (exact)")))

(defun mevedel-permission--prompt-resources (entry)
  "Return the resources ENTRY's remembering controls operate on.
The selected scope replaces the requested grants once the user changes it."
  (or (car (plist-get entry :resource-selection-cell))
      (plist-get (plist-get entry :requested-additional-permissions)
                 :file-system)))

(defun mevedel-permission--prompt-select-resource ()
  "Select exact or containing directory authority for the prompt at point.
Selection changes the visible card only; approval chooses its lifetime."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt))
              (entry (overlay-get ov 'mevedel-view-interaction-entry))
              (cell (plist-get entry :resource-selection-cell)))
    (let* ((originals (plist-get entry :resource-originals))
           (original
            (if (cdr originals)
                (let ((choices
                       (mapcar (lambda (grant)
                                 (cons (mevedel-permission--resource-label entry grant)
                                       grant))
                               originals)))
                  (cdr (assoc (completing-read "Resource to change: " choices nil t)
                              choices)))
              (car originals)))
           (index (cl-position original originals :test #'equal))
           (previous (nth index (car cell)))
           (path (plist-get original :path))
           (access (plist-get original :access))
           (directory (if (file-directory-p path)
                          (expand-file-name path)
                        (file-name-directory path)))
           (scopes (list (append (list :path path)
                                (and (plist-get original :recursive)
                                     '(:recursive t)))))
           choices)
      ;; A missing file's nearest parents may not exist yet; offer only
      ;; directories that do.
      (while directory
        (when (file-directory-p directory)
          (setq scopes
                (append scopes (list (list :path (directory-file-name directory)
                                           :recursive t)))))
        (setq directory (file-name-parent-directory directory)))
      (dolist (scope scopes)
        (dolist (choice-access (if (eq access 'write) '(write) '(read write)))
          (let ((grant (append scope (list :access choice-access))))
            (unless (mevedel-permission-queue-resource-needs-tree-p entry grant)
              (cl-pushnew (cons (mevedel-permission--resource-label entry grant) grant)
                          choices :test #'equal)))))
      (setq choices (nreverse choices))
      (when-let* ((selected
                   (cdr (assoc
                         (completing-read "Approve resource scope: "
                                          choices nil t)
                         choices))))
        (mevedel-permission-queue-select-resource entry previous selected)
        (mevedel-permission-queue--render-head (plist-get entry :session))))))

(defun mevedel-permission--prompt-self-insert ()
  "Insert the typed permission key when no permission prompt is active."
  (when (and (characterp last-command-event)
             (not buffer-read-only)
             (not (get-char-property (point) 'read-only)))
    (self-insert-command 1)))

(defun mevedel-permission--prompt-finish-or-self-insert (result)
  "Settle the permission prompt at point with RESULT, or insert the key."
  (unless (mevedel-permission--prompt-finish result)
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-once ()
  "Allow this tool invocation once."
  (interactive)
  (if-let* ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (let ((entry (overlay-get ov 'mevedel-view-interaction-entry)))
        (if (and (plist-get entry :mutation-p)
                 (mevedel-side-conversation-mutation-warning-pending-p entry))
            (progn
              (mevedel-permission-queue--render-head
               (plist-get entry :session)))
          (mevedel-permission--prompt-finish 'allow-once)))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-session ()
  "Allow this tool for the rest of the session."
  (interactive)
  (if-let* ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (if (overlay-get ov 'mevedel-permission-suppress-allow-session)
          (message "Session allow is not available for this prompt")
        (mevedel-permission--prompt-finish 'allow-session))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-always ()
  "Always allow this tool (persisted to disk)."
  (interactive)
  (if-let* ((ov (mevedel--prompt--overlay-at-point
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
  "Deny this tool invocation and pass free-form feedback to the LLM."
  (interactive)
  (if (mevedel--prompt--overlay-at-point 'mevedel-permission-prompt)
      (let ((text (read-string "Feedback: ")))
        (when (and text (not (string-empty-p (string-trim text))))
          (mevedel-permission--prompt-finish
           (cons 'feedback (string-trim text)))))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-toggle-remember ()
  "Toggle one remembered capability on the permission prompt at point."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt))
              (entry (overlay-get ov 'mevedel-view-interaction-entry))
              (cell (plist-get entry :remember-authority-cell)))
    (let* ((selection (copy-tree (car cell)))
           (resources (mevedel-permission--prompt-resources entry)))
      (pcase last-command-event
        (?c
         (setq selection
               (plist-put selection :operation
                          (not (plist-get selection :operation))))
         (unless (plist-get selection :operation)
           (setq selection (plist-put selection :network nil)
                 selection (plist-put selection :file-system nil))))
        (?n
         (setq selection
               (plist-put selection :network
                          (not (plist-get selection :network))))
         (when (plist-get selection :network)
           (setq selection (plist-put selection :operation t))))
        (?p
         (when resources
           (let* ((choices
                   (mapcar
                    (lambda (grant)
                      (cons (mevedel-permission--resource-label entry grant)
                            grant))
                    resources))
                  (grant
                   (cdr
                    (assoc
                     (completing-read "Remember capability: " choices nil t)
                     choices)))
                  (scopes
                   (append
                    (when (plist-get entry :reusable-operation-p)
                      '(("With this command" . :file-system)))
                    '(("Independent path access" . :resource-grants)
                      ("Do not remember" . nil))))
                  (scope (cdr (assoc (completing-read "Remember path access: "
                                                     scopes nil t)
                                     scopes))))
             (dolist (key '(:file-system :resource-grants))
               (setq selection
                     (plist-put selection key
                                (delete grant (plist-get selection key)))))
             (when scope
               (setq selection
                     (plist-put selection scope
                                (append (plist-get selection scope) (list grant)))))
             (when (eq scope :file-system)
               (setq selection (plist-put selection :operation t)))))))
      (setcar cell selection)
      (when-let* ((session (plist-get entry :session)))
        (mevedel-permission-queue--render-head session)))))

(defun mevedel-permission--prompt-finish (result)
  "Settle the permission prompt overlay at point with RESULT."
  (when-let* ((ov (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt)))
    (mevedel-permission-queue-validate-approval
     (overlay-get ov 'mevedel-view-interaction-entry) result)
    (mevedel--prompt--settle ov result)
    t))

(defcustom mevedel-permission-command-display-limit 400
  "Maximum number of characters of a Bash command shown inline.
Longer commands are elided with a TAB toggle that reveals the rest.
The detected-command summary is never elided, and
a remote collaborator always receives the whole command."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-eval-expression-display-limit 8
  "Maximum number of lines to show inline in the Eval permission prompt.
Expressions longer than this are truncated with a toggle to expand."
  :type 'integer
  :group 'mevedel)

(defun mevedel-permission--elide (text entry &optional line-limit char-limit)
  "Return TEXT for prompt display, elided unless ENTRY is expanded.
LINE-LIMIT caps the collapsed line count and CHAR-LIMIT the collapsed
character count; either may be nil.  Text properties on TEXT survive, so
callers face TEXT before eliding it.  ENTRY owns the toggle state, so a
prompt without one is returned whole."
  (let* ((lines (and line-limit (split-string text "\n")))
         (long-p (or (and line-limit (> (length lines) line-limit))
                     (and char-limit (> (length text) char-limit)))))
    (cond
     ((or (null entry) (not long-p)) text)
     ((mevedel-queue--entry-metadata-get entry :expanded)
      (concat text "\n"
              (mevedel--prompt-key "TAB")
              (propertize " to collapse" 'font-lock-face 'shadow)))
     (t
      (let* ((head (if (and line-limit (> (length lines) line-limit))
                       (string-join (seq-take lines line-limit) "\n")
                     text))
             (head (if (and char-limit (> (length head) char-limit))
                       (substring head 0 char-limit)
                     head)))
        (concat head
                (propertize "..." 'font-lock-face 'shadow)
                "\n"
                (propertize (format "%d more characters, "
                                    (- (length text) (length head)))
                            'font-lock-face 'shadow)
                (mevedel--prompt-key "TAB")
                (propertize " to expand" 'font-lock-face 'shadow)))))))

(defun mevedel-permission--prompt-toggle-expand ()
  "Toggle full display of the elided command or expression at point."
  (interactive)
  (when-let* ((ov (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt))
              (entry (overlay-get ov 'mevedel-view-interaction-entry)))
    (mevedel-queue--entry-metadata-put
     entry :expanded
     (not (mevedel-queue--entry-metadata-get entry :expanded)))
    (when-let* ((session (plist-get entry :session)))
      (mevedel-permission-queue--render-head session))))

(defun mevedel-permission--prompt-body
    (content include-always &optional suppress-allow-session once-only toggle)
  "Return the permission prompt body for CONTENT.
INCLUDE-ALWAYS enables persistent allow.  SUPPRESS-ALLOW-SESSION hides
session allow.  ONCE-ONLY hides every session-scoped choice.  TOGGLE is
`collapsed' or `expanded' when TAB toggles the card's details."
  (mevedel--prompt-framed-body
   (concat
    content
    (propertize "Keys: " 'font-lock-face 'help-key-binding)
    (mevedel--prompt-key "RET")
    " allow once  "
    (unless (or suppress-allow-session once-only)
      (concat (mevedel--prompt-key "s") " remember for session  "))
    (when (and include-always (not once-only))
      (concat (mevedel--prompt-key "A") " remember in workspace  "))
    (mevedel--prompt-key "d")
    " deny  "
    (unless once-only
      (concat (mevedel--prompt-key "D") " deny session  "))
    (mevedel--prompt-key "f")
    " feedback"
    (pcase toggle
      ('collapsed (concat "  " (mevedel--prompt-key "TAB") " details"))
      ('expanded (concat "  " (mevedel--prompt-key "TAB") " fewer details")))
    "\n")
   'warning))

(defun mevedel-permission--expanded-p (entry)
  "Return non-nil when ENTRY's card shows its details locally.
A prompt without an entry has no toggle state and shows everything."
  (or (null entry) (mevedel-queue--entry-metadata-get entry :expanded)))

(defun mevedel-permission--format-justification (entry &optional justification)
  "Format ENTRY's model-supplied reason as quoted explanation, not authority.
JUSTIFICATION, when non-blank, supplies the reason directly."
  (when-let* ((text (if (and (stringp justification)
                             (not (string-empty-p (string-trim justification))))
                        (string-trim justification)
                      (mevedel-permission-queue-justification entry))))
    (concat (propertize "Model's reason: " 'font-lock-face 'font-lock-escape-face)
            (propertize text 'font-lock-face 'italic)
            "\n")))

(defun mevedel-permission--warning (text)
  "Return warning TEXT as an always-visible card line."
  (concat (propertize "⚠ " 'font-lock-face 'error)
          (propertize text 'font-lock-face 'warning)
          "\n"))

(defun mevedel-permission--execution-warnings (entry)
  "Return confinement warnings that stay visible on ENTRY's folded card.
Live Eval and full escalation have unrestricted effects whichever card
kind requests them, including generic hook cards."
  (let* ((kind (plist-get entry :kind))
         (args (plist-get entry :args))
         (live-p
          (pcase kind
            ('eval (equal (or (plist-get entry :mode) "live") "live"))
            ('generic (and (equal (plist-get entry :tool-name) "Eval")
                           (not (equal (plist-get args :mode) "batch"))))))
         (escalated-p
          (or (eq (plist-get entry :sandbox-permissions) 'require-escalated)
              (equal (plist-get args :sandbox_permissions) "require_escalated"))))
    (concat
     (when live-p
       (mevedel-permission--warning
        "Live Eval runs inside this Emacs: filesystem, network and processes are unrestricted."))
     (when escalated-p
       (mevedel-permission--warning
        (concat "This command runs directly as your user. Filesystem, network, "
                "and process confinement are all disabled for this invocation.")))
     (when (plist-get entry :protected-path)
       (mevedel-permission--warning
        "The requested path is protected by default.")))))

(defun mevedel-permission--format-scope (entry)
  "Format the complete additive authority an approval of ENTRY uses."
  (let ((items
         (append
          (and (plist-get (plist-get entry :requested-additional-permissions) :network)
               '("Network"))
          (mapcar (lambda (grant) (mevedel-permission--resource-label entry grant))
                  (mevedel-permission--prompt-resources entry)))))
    (when items
      (concat (propertize "Scope: " 'font-lock-face 'font-lock-escape-face)
              (propertize (string-join items ", ") 'font-lock-face 'font-lock-constant-face)
              "\n"))))

(defun mevedel-permission--format-remember-summary (entry)
  "Summarize changed remembering choices on ENTRY's folded card.
Unchanged defaults stay in the details; a change shows what session or
workspace approval would store."
  (when-let* ((cell (plist-get entry :remember-authority-cell))
              (default (mevedel-queue--entry-metadata-get entry :remember-default))
              ((not (equal (car default) (car cell)))))
    (let* ((selection (car cell))
           (label (lambda (grant) (mevedel-permission--resource-label entry grant)))
           (items
            (append
             (and (plist-get selection :operation) '("command"))
             (and (plist-get selection :network) '("network with command"))
             (mapcar (lambda (grant) (concat (funcall label grant) " with command"))
                     (plist-get selection :file-system))
             (mapcar (lambda (grant) (concat (funcall label grant) " independently"))
                     (plist-get selection :resource-grants)))))
      (concat (propertize "Session/workspace approval remembers: "
                          'font-lock-face 'font-lock-escape-face)
              (if items (string-join items ", ") "nothing")
              "\n"))))

(defun mevedel-permission--format-review-fallback (entry)
  "Explain why automatic review left ENTRY to the user, or nil."
  (when-let* ((fallback (plist-get entry :review-fallback)))
    (format "Automatic review deferred: %s.\n"
            (pcase fallback
              (`(reviewer ,reason) (format "reviewer asked the user (%s)" reason))
              ('timeout "review timed out")
              ('changed "evidence changed during review")
              ('missing-intent "no bounded user intent was available")
              ('disabled "automatic review was disabled")
              (_ "reviewer unavailable or its reply was invalid")))))

(defun mevedel-permission--format-cause (entry)
  "Describe ENTRY's captured admission mode and permission cause."
  (let ((fallback (mevedel-permission--format-review-fallback entry)))
    (when-let* ((mode (or (plist-get entry :permission-mode-effective)
                          (and fallback 'unknown))))
      (concat
       (unless (eq mode 'unknown)
         (format "Permission mode at admission: %s\n" mode))
     (when-let* ((cause
                  (pcase (plist-get entry :permission-via)
                    ('workspace-boundary "resource outside the allowed roots")
                    ('protected-path "protected resource needs explicit authority")
                    ('rule "an explicit rule requests approval")
                    ('pre-tool-hook "a PreToolUse hook requests approval")
                    ('permission-request-hook "a PermissionRequest hook requests approval")
                    ('one-shot-mutation "this mutation requires one-time approval")
                    ('mode "this operation requires approval in the admission mode")
                    ('sandbox-network "additional network authority is missing")
                    ('sandbox-filesystem "additional filesystem authority is missing")
                    ('sandbox-full-escalation "the operation requests execution without confinement"))))
       (format "Approval required: %s.\n" cause))
       fallback
       "\n"))))

(defun mevedel-permission--prompt-async-with-content
    (content include-always cont
             &optional count entry suppress-allow-session once-only)
  "Display a permission prompt for CONTENT and call CONT with its outcome."
  (let* ((cause (mevedel-permission--format-cause entry))
         (expanded-p (and entry (mevedel-queue--entry-metadata-get entry :expanded)))
         (authority
          (when-let* ((selected (car (plist-get entry :resource-selection-cell))))
            (concat "Selected authority:\n"
                    (mapconcat
                     (lambda (grant) (mevedel-permission--resource-label entry grant))
                     selected "\n")
                    "\n(g selects exact resource or directory tree and access)\n\n")))
         ;; Explanations and controls fold locally; the guest has no TAB, so
         ;; its descriptor always receives the complete card.
         (details (concat cause authority))
         (remote-body (and entry (mevedel-queue--entry-metadata-get entry :remote-body)))
         (remote-content (concat (or remote-body content) details))
         ;; TAB is offered only when folding hides something.
         (toggle (and entry
                      (cond
                       (expanded-p 'expanded)
                       ((or (not (string-empty-p details))
                            (and remote-body
                                 (not (equal remote-body
                                             (substring-no-properties content)))))
                        'collapsed))))
         (content (concat content (and (or (null entry) expanded-p) details)))
         ;; The entry's session decides where its card goes.  A prompt can be
         ;; admitted from an unrelated buffer, such as the permission
         ;; reviewer's request buffer when it leaves the decision to a human.
         (source-buffer (let ((owner (and entry (plist-get entry :data-buffer))))
                          (if (buffer-live-p owner) owner (current-buffer))))
         (target-buf
          (if (fboundp 'mevedel-view--interaction-target-buffer)
              (mevedel-view--interaction-target-buffer
               (mevedel--prompt--data-buffer source-buffer))
            (error "No live view for queued prompt")))
         (interaction-id
          (or (and entry
                   (mevedel-queue--entry-metadata-get entry :interaction-id))
              (let ((id (list :permission (gensym "permission-"))))
                (when entry
                  (mevedel-queue--entry-metadata-put
                   entry :interaction-id id))
                id)))
         (parent-active-warning
          (and (plist-get entry :mutation-p)
               (mevedel-side-conversation-mutation-warning
                entry "this approved change")))
         ov)
    (when entry
      (mevedel-queue--entry-metadata-put entry :view-buffer target-buf))
    (with-current-buffer target-buf
      (let ((map (make-sparse-keymap)))
        (define-key map "a" #'mevedel-permission--prompt-approve-once)
        (define-key map (kbd "RET") #'mevedel-permission--prompt-approve-once)
        (define-key map (kbd "<return>")
                    #'mevedel-permission--prompt-approve-once)
        (unless (or suppress-allow-session once-only)
          (define-key map "s" #'mevedel-permission--prompt-approve-session))
        (when (and include-always (not once-only))
          (define-key map "A" #'mevedel-permission--prompt-approve-always))
        (define-key map "d" #'mevedel-permission--prompt-deny-once)
        (unless once-only
          (define-key map "D" #'mevedel-permission--prompt-deny-session))
        (define-key map "f" #'mevedel-permission--prompt-feedback)
        (when (plist-get entry :resource-selection-cell)
          (define-key map "g" #'mevedel-permission--prompt-select-resource))
        (when entry
          (define-key map (kbd "TAB")
                      #'mevedel-permission--prompt-toggle-expand))
        (when (and entry
                   (plist-get entry :remember-authority-cell)
                   (plist-get entry :reusable-operation-p))
          (define-key map "c" #'mevedel-permission--prompt-toggle-remember)
          (when (plist-get
                 (plist-get entry :requested-additional-permissions)
                 :network)
            (define-key map "n"
                        #'mevedel-permission--prompt-toggle-remember)))
        (when (plist-get
               (plist-get entry :requested-additional-permissions)
               :file-system)
          (define-key map "p" #'mevedel-permission--prompt-toggle-remember))
        (define-key map [?q] #'mevedel-permission--prompt-deny-once)
        (define-key map (kbd "C-g") #'mevedel-permission--prompt-deny-once)
        (setq ov
              (mevedel-view--interaction-register
               (list :kind 'permission
                     :id interaction-id
                     :origin (or (plist-get entry :origin) "/root")
                     :count (or count 1)
                     :body (mevedel-permission--prompt-body
                            (concat
                             content
                             (when parent-active-warning
                               (propertize parent-active-warning
                                           'font-lock-face 'warning)))
                            include-always
                            suppress-allow-session once-only
                            toggle)
                     :priority 100
                     :keymap map
                     :help-echo (if once-only
                                    "One-time permission prompt"
                                  "Permission prompt")
                     :entry entry
                     :activate cont)))
        (when (and entry
                   (not (mevedel-queue--entry-metadata-get entry :displayed)))
          (mevedel-queue--entry-metadata-put entry :displayed t)
          (mevedel-permission-queue--log 'permission-displayed entry))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-permission-suppress-allow-session
                     suppress-allow-session)
        (overlay-put ov 'mevedel-permission-include-always include-always)
        (overlay-put ov 'mevedel--callback cont)
        (overlay-put ov 'mevedel-user-request t)
        ;; The remote surface gets the one-shot outcomes only: durable
        ;; session or workspace authority is never mintable from a guest.
        (overlay-put ov 'mevedel--remote
                     (list :body (substring-no-properties remote-content)
                           :options '((allow-once . "Allow once")
                                      (deny-once . "Deny"))
                           :feedback t))
        (unless entry
          (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
          (mevedel--prompt--register-canceller source-buffer ov))
        (mevedel--prompt-announce ov)))
    ov))


;;
;;; Prompt rendering

(defun mevedel-permission--format-authority-capabilities (entry)
  "Format requested invocation authority from ENTRY."
  (when (plist-get entry :show-operation-authority)
    (let* ((requested
            (plist-get entry :requested-additional-permissions))
           (missing
            (plist-get entry :missing-additional-permissions))
           (granted (plist-get (plist-get entry :granted-additional-permissions)
                               :file-system)))
      (concat
       (propertize "Authority for this execution\n"
                   'font-lock-face '(:inherit bold))
       (propertize
        "[x] already granted · [ ] granted by this approval\n"
        'font-lock-face 'font-lock-comment-face)
       (format "[%s] Command\n"
               (if (plist-get entry :operation-pending-p) " " "x"))
       (when (plist-get requested :network)
         (format "[%s] Network\n"
                 (if (plist-get missing :network) " " "x")))
       (mapconcat
        (lambda (grant)
          (format "[%s] %s"
                  (if (and (mevedel-permission-rules-resource-granted-p
                            (plist-get grant :path) (plist-get grant :access) granted)
                           (mevedel-permission-rules-resource-granted-p
                            (plist-get grant :path) (plist-get grant :access)
                            (or (plist-get entry :available-resource-grants) granted)
                            (plist-get grant :recursive)))
                      "x" " ")
                  (mevedel-permission--resource-label entry grant)))
        (mevedel-permission--prompt-resources entry)
        "\n")
       (and (plist-get requested :file-system) "\n")
       "\n"))))

(defun mevedel-permission--format-remember-authority (entry)
  "Format reusable authority selections from ENTRY."
  (when-let* ((cell (plist-get entry :remember-authority-cell)))
    (let* ((selection (car cell))
           (resources (mevedel-permission--prompt-resources entry))
           (operation-p (plist-get entry :reusable-operation-p))
           (network-p (and operation-p
                           (plist-get (plist-get entry :requested-additional-permissions)
                                      :network))))
      (when (or operation-p network-p resources)
        (concat
         (propertize "Session/workspace approval remembers the selected authority\n"
                     'font-lock-face '(:inherit bold))
         (when operation-p
           (format "[%s] Command  (c toggles)\n"
                   (if (plist-get selection :operation) "x" " ")))
         (when network-p
           (format "[%s] Network with command  (n toggles)\n"
                   (if (plist-get selection :network) "x" " ")))
         (mapconcat
          (lambda (grant)
            (format
             "%s -- %s"
             (mevedel-permission--resource-label entry grant)
             (cond
              ((member grant (plist-get selection :file-system)) "With this command")
              ((member grant (plist-get selection :resource-grants)) "Independent path access")
              (t "Do not remember"))))
          resources "\n")
         (and resources "\n(p selects a path and remembering scope)\n")
         "\n")))))

(defun mevedel-permission-prompt-render (entry origin cont count)
  "Render permission ENTRY with ORIGIN attribution and pending COUNT.
CONT receives the user's outcome; the caller owns queue settlement."
  (pcase (plist-get entry :kind)
    ('generic
     (mevedel-permission--prompt-async-attributed
      (plist-get entry :tool-name) (plist-get entry :specifier-value)
      (plist-get entry :include-always) origin cont count entry))
    ('bash
     (mevedel-permission--prompt-async-bash
      (plist-get entry :command) (plist-get entry :command-class)
      (plist-get entry :include-always) origin cont count entry))
    ('eval
     (mevedel-permission--prompt-async-eval
      (plist-get entry :expression) origin cont count entry))
    ('sandbox
     (mevedel-permission--prompt-async-sandbox
      (plist-get entry :tool-name) (plist-get entry :detail)
      (plist-get entry :justification) origin cont count entry))
    (_ (error "Unknown permission card kind: %s" (plist-get entry :kind)))))

(defun mevedel-permission--prompt-async-card
    (build include-always cont count entry &optional suppress-allow-session once-only)
  "Render the card BUILD describes and call CONT with its outcome.
BUILD receives REMOTE-P and returns card text.  The local card folds its
details until TAB expands them; the guest descriptor, which has no TAB,
always receives the complete card.  INCLUDE-ALWAYS, COUNT, ENTRY,
SUPPRESS-ALLOW-SESSION and ONCE-ONLY follow
`mevedel-permission--prompt-async-with-content'."
  (when entry
    (when-let* ((cell (plist-get entry :remember-authority-cell))
                ((not (mevedel-queue--entry-metadata-get entry :remember-default))))
      ;; Wrapped so an empty default selection is still recorded.
      (mevedel-queue--entry-metadata-put
       entry :remember-default (list (copy-tree (car cell)))))
    (mevedel-queue--entry-metadata-put
     entry :remote-body (substring-no-properties (funcall build t))))
  (mevedel-permission--prompt-async-with-content
   (funcall build nil) include-always cont count entry
   suppress-allow-session once-only))

(defun mevedel-permission--card-title (text &optional face)
  "Return card title TEXT with FACE, which defaults to `warning'."
  (propertize (concat text "\n")
              'font-lock-face (list :inherit 'bold :inherit (or face 'warning))))

(defun mevedel-permission--card-field (label value &optional face)
  "Return a card line for LABEL and VALUE, using FACE for VALUE."
  (concat (propertize label 'font-lock-face 'font-lock-escape-face)
          (propertize (format "%s" value)
                      'font-lock-face (or face 'font-lock-constant-face))
          "\n"))

(defun mevedel-permission--card-details (entry remote-p)
  "Return ENTRY's authority breakdown and remembering controls when shown.
REMOTE-P requests them regardless of the local fold state."
  (when (and entry (or remote-p (mevedel-permission--expanded-p entry)))
    (let ((details (concat
                    (mevedel-permission--format-authority-capabilities entry)
                    (mevedel-permission--format-remember-authority entry))))
      (unless (string-empty-p details) (concat "\n" details)))))

(defun mevedel-permission--prompt-async-attributed
    (tool-name path include-always origin cont &optional count entry)
  "Display an attributed permission prompt and call CONT with its outcome.
TOOL-NAME requests PATH; INCLUDE-ALWAYS, ORIGIN, COUNT and ENTRY follow
the shared permission prompt contract."
  (let* ((path (and path (mevedel-permission--display-path entry path)))
         (once-only (and entry (plist-get entry :once-only)))
         (build
          (lambda (_remote-p)
            (concat
             (mevedel-permission--card-title "Permission Request")
             (mevedel--prompt-attribution-line origin)
             "\n"
             (mevedel-permission--card-field "Tool: " tool-name)
             (when path
               (concat
                (mevedel-permission--card-field "Path: " path 'font-lock-string-face)
                (when-let* ((access (and entry (plist-get entry :resource-access))))
                  (mevedel-permission--card-field "Access: " access))))
             (mevedel-permission--format-justification entry)
             (mevedel-permission--format-scope entry)
             (mevedel-permission--format-remember-summary entry)
             (mevedel-permission--execution-warnings entry)
             "\n"))))
    (mevedel-permission--prompt-async-card
     build (and include-always (not once-only)) cont count entry
     once-only once-only)))

(defun mevedel-permission--prompt-async-bash
    (command command-class include-always origin cont &optional count entry)
  "Display a Bash permission prompt and call CONT with its outcome.
COMMAND has COMMAND-CLASS; INCLUDE-ALWAYS, ORIGIN, COUNT and ENTRY follow
the shared permission prompt contract."
  (let* ((dangerous (eq command-class 'dangerous))
         (once-only (and entry (plist-get entry :once-only)))
         (rule-creating-disabled-p
          (or once-only
              (not (and entry (plist-get entry :reusable-operation-p)))))
         (commands (and entry (plist-get entry :commands)))
         (commands-summary
          (and entry
               (or (plist-get entry :commands-summary)
                   (and commands (mapconcat #'identity commands ", ")))))
         (unparseable (and entry (plist-get entry :unparseable)))
         (allow-patterns (and entry (plist-get entry :allow-patterns)))
         (faced-command (propertize command 'font-lock-face 'font-lock-string-face))
         (build
          (lambda (remote-p)
            (concat
             (mevedel-permission--card-title
              (if dangerous
                  "Bash Command Execution Request — DANGEROUS"
                "Bash Command Execution Request")
              (if dangerous 'error 'warning))
             (mevedel--prompt-attribution-line origin)
             "\n"
             (mevedel-permission--format-justification entry)
             (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
             (if remote-p
                 faced-command
               (mevedel-permission--elide
                faced-command entry nil mevedel-permission-command-display-limit))
             "\n"
             (when commands-summary
               (mevedel-permission--card-field "Detected commands: " commands-summary))
             (mevedel-permission--format-scope entry)
             (when (and allow-patterns (not rule-creating-disabled-p)
                        (or (not (plist-get entry :remember-authority-cell))
                            (plist-get (car (plist-get entry :remember-authority-cell))
                                       :operation)))
               (mevedel-permission--card-field
                "Session/always allow will add: "
                (mapconcat (lambda (pattern) (format "`%s'" pattern))
                           allow-patterns ", ")))
             (mevedel-permission--format-remember-summary entry)
             (when dangerous
               (concat
                (mevedel-permission--warning
                 "Contains a binary on `mevedel-bash-dangerous-commands'.")
                (when rule-creating-disabled-p
                  (propertize
                   "Session/permanent allow is disabled for dynamic dangerous commands.\n"
                   'font-lock-face 'font-lock-comment-face))))
             (when unparseable
               (concat
                (mevedel-permission--warning
                 "Command contains unsupported or dynamic shell syntax.")
                (propertize
                 "Session/permanent allow is disabled for complex Bash commands.\n"
                 'font-lock-face 'font-lock-comment-face)))
             (mevedel-permission--execution-warnings entry)
             (mevedel-permission--card-details entry remote-p)
             "\n"))))
    (mevedel-permission--prompt-async-card
     build (and include-always (not rule-creating-disabled-p))
     cont count entry rule-creating-disabled-p once-only)))

(defun mevedel-permission--prompt-async-eval
    (expression origin cont count entry)
  "Display EXPRESSION from ENTRY with ORIGIN attribution and queue COUNT.
Call CONT with the chosen outcome.  Local elision retains the full expression
in the remote descriptor; ENTRY supplies execution mode and remembering scope."
  (let* ((mode (or (plist-get entry :mode) "live"))
         (preserve-ui (plist-get entry :preserve-ui))
         (faced-expr (propertize expression 'font-lock-face 'font-lock-string-face))
         (build
          (lambda (remote-p)
            (concat
             (mevedel-permission--card-title "Elisp Evaluation Request")
             (mevedel--prompt-attribution-line origin)
             "\n"
             (propertize "Mode: " 'font-lock-face 'font-lock-escape-face)
             mode
             (when (equal mode "live")
               (format " (inherently unconfined; preserve_ui: %s)"
                       (if preserve-ui "true" "false")))
             "\n"
             (mevedel-permission--format-justification entry)
             (propertize "Expression:\n" 'font-lock-face 'font-lock-escape-face)
             (if remote-p
                 faced-expr
               (mevedel-permission--elide
                faced-expr entry mevedel-eval-expression-display-limit))
             "\n"
             (mevedel-permission--format-scope entry)
             (mevedel-permission--format-remember-summary entry)
             (mevedel-permission--execution-warnings entry)
             (mevedel-permission--card-details entry remote-p)
             "\n"))))
    (mevedel-permission--prompt-async-card
     build (plist-get entry :include-always) cont count entry
     (not (plist-get entry :remember-authority-cell))
     (not (plist-get entry :remember-authority-cell)))))

(defun mevedel-permission--prompt-async-sandbox
    (tool-name detail justification origin cont &optional count entry)
  "Prompt for changed child authority for TOOL-NAME and DETAIL.
JUSTIFICATION is the model's user-facing reason.  ORIGIN, CONT, COUNT, and
ENTRY follow the shared permission prompt contract."
  (let* ((full-p (eq (plist-get entry :sandbox-permissions)
                     'require-escalated))
         (once-only (plist-get entry :once-only))
         (missing (plist-get entry :missing-additional-permissions))
         (missing-filesystem-p (plist-get missing :file-system))
         (full-rule-disabled-p
          (and full-p
               (or once-only
                   (not (plist-get entry :include-always)))))
         (faced-detail (propertize detail 'font-lock-face 'font-lock-string-face))
         (build
          (lambda (remote-p)
            (concat
             (mevedel-permission--card-title
              (if full-p
                  "Full Execution Escalation Request"
                "Invocation Authority Request"))
             (mevedel--prompt-attribution-line origin)
             "\n"
             (mevedel-permission--card-field "Tool: " tool-name)
             (mevedel-permission--format-justification entry justification)
             (propertize "Request: " 'font-lock-face 'font-lock-escape-face)
             (if remote-p
                 faced-detail
               (mevedel-permission--elide
                faced-detail entry nil mevedel-permission-command-display-limit))
             "\n"
             (unless full-p (mevedel-permission--format-scope entry))
             (mevedel-permission--format-remember-summary entry)
             (mevedel-permission--execution-warnings entry)
             (when (or remote-p (mevedel-permission--expanded-p entry))
               (concat
                (unless full-p (mevedel-permission--card-details entry t))
                (unless full-p
                  (propertize
                   (concat
                    "Approval grants every unchecked capability to this invocation. "
                    "Checked authority is already available; other resources and "
                    "the selected process confinement remain unchanged.\n")
                   'font-lock-face 'font-lock-comment-face))
                (when full-rule-disabled-p
                  (propertize
                   (concat
                    "Reusable allow is disabled for this request. Author a "
                    "qualified rule deliberately if recurring authority is needed.\n")
                   'font-lock-face 'font-lock-comment-face))))
             "\n"))))
    (mevedel-permission--prompt-async-card
     build
     (and (not once-only)
          (or missing-filesystem-p full-p)
          (or full-p (plist-get entry :remember-authority-cell))
          (plist-get entry :include-always))
     cont count entry
     (or once-only
         (if full-p
             full-rule-disabled-p
           (not (plist-get entry :remember-authority-cell))))
     (or once-only
         (and (not full-p)
              (not (plist-get entry :remember-authority-cell)))))))

(provide 'mevedel-permission-prompt)

;;; mevedel-permission-prompt.el ends here
