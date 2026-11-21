;;; niwa-permission-prompt.el -- Permission prompt UI -*- lexical-binding: t -*-

;;; Commentary:

;; Renders and settles generic, Bash, Eval, and execution-authority prompts.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'niwa-utilities)

;; `niwa-execution-target'
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-interaction-prompt'
(declare-function niwa--prompt--data-buffer
                  "niwa-interaction-prompt" (&optional buffer))
(declare-function niwa--prompt--overlay-at-point
                  "niwa-interaction-prompt" (property))
(declare-function niwa--prompt--register-canceller
                  "niwa-interaction-prompt" (buffer overlay))
(declare-function niwa--prompt--settle
                  "niwa-interaction-prompt" (overlay result))
(declare-function niwa--prompt-announce
                  "niwa-interaction-prompt" (overlay))
(declare-function niwa--prompt-attribution-line
                  "niwa-interaction-prompt" (origin))
(declare-function niwa--prompt-framed-body
                  "niwa-interaction-prompt" (body face))
(declare-function niwa--prompt-key
                  "niwa-interaction-prompt" (key))
(defvar niwa--prompt-overlays)

(autoload 'niwa--prompt--data-buffer "niwa-interaction-prompt")
(autoload 'niwa--prompt--overlay-at-point "niwa-interaction-prompt")
(autoload 'niwa--prompt--register-canceller "niwa-interaction-prompt")
(autoload 'niwa--prompt--settle "niwa-interaction-prompt")
(autoload 'niwa--prompt-announce "niwa-interaction-prompt")
(autoload 'niwa--prompt-attribution-line "niwa-interaction-prompt")
(autoload 'niwa--prompt-framed-body "niwa-interaction-prompt")
(autoload 'niwa--prompt-key "niwa-interaction-prompt")

;; `niwa-permission-queue'
(declare-function niwa-permission-absence-deadline
                  "niwa-permission-queue" (session))
(declare-function niwa-permission-queue--log
                  "niwa-permission-queue" (event entry &optional session &rest props))
(declare-function niwa-permission-queue--render-head
                  "niwa-permission-queue" (&optional session))
(declare-function niwa-permission-queue-justification
                  "niwa-permission-queue" (entry))
(declare-function niwa-permission-queue-resource-needs-tree-p
                  "niwa-permission-queue" (entry grant))
(declare-function niwa-permission-queue-select-resource
                  "niwa-permission-queue" (entry previous selected))
(declare-function niwa-permission-queue-validate-approval
                  "niwa-permission-queue" (entry outcome))
(defvar niwa-permission-queue--settled-cells)
(autoload 'niwa-permission-absence-deadline "niwa-permission-queue")
(autoload 'niwa-permission-queue--log "niwa-permission-queue")
(autoload 'niwa-permission-queue--render-head "niwa-permission-queue")
(autoload 'niwa-permission-queue-justification "niwa-permission-queue")
(autoload 'niwa-permission-queue-resource-needs-tree-p "niwa-permission-queue")
(autoload 'niwa-permission-queue-select-resource "niwa-permission-queue")
(autoload 'niwa-permission-queue-validate-approval "niwa-permission-queue")

;; `niwa-permission-rules'
(declare-function niwa-permission-rules-resource-granted-p
                  "niwa-permission-rules" (path access grants &optional recursive))
(autoload 'niwa-permission-rules-resource-granted-p "niwa-permission-rules")

;; `niwa-queue'
(declare-function niwa-queue--entry-metadata-get
                  "niwa-queue" (entry key))
(declare-function niwa-queue--entry-metadata-put
                  "niwa-queue" (entry key value))
(autoload 'niwa-queue--entry-metadata-get "niwa-queue")
(autoload 'niwa-queue--entry-metadata-put "niwa-queue")

;; `niwa-side-conversation'
(declare-function niwa-side-conversation-mutation-warning
                  "niwa-side-conversation" (record effect))
(declare-function niwa-side-conversation-mutation-warning-pending-p
                  "niwa-side-conversation" (record))
(autoload 'niwa-side-conversation-mutation-warning
  "niwa-side-conversation")
(autoload 'niwa-side-conversation-mutation-warning-pending-p
  "niwa-side-conversation")

;; `niwa-structs'
(declare-function niwa-session-execution-target
                  "niwa-structs" (session))

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-register
                  "niwa-view-interaction" (descriptor))
(declare-function niwa-view--interaction-target-buffer
                  "niwa-view-interaction" (data-buffer))
(autoload 'niwa-view--interaction-register "niwa-view-interaction")
(autoload 'niwa-view--interaction-target-buffer
  "niwa-view-interaction")


;;
;;; Permission prompt controls

(defun niwa-permission--display-path (entry path)
  "Return PATH for display in the execution domain of ENTRY."
  (if-let* ((session (plist-get entry :session))
            (target (niwa-session-execution-target session))
            ((niwa-execution-target-remote-p target))
            ((file-remote-p path)))
      (niwa-execution-target-native-path target path)
    path))

(defun niwa-permission--resource-label (entry grant)
  "Return a display label for resource GRANT in ENTRY's path domain."
  (format "%s %s%s"
          (capitalize (symbol-name (plist-get grant :access)))
          (niwa-permission--display-path entry (plist-get grant :path))
          (if (plist-get grant :recursive) " (recursive)" " (exact)")))

(defun niwa-permission--prompt-resources (entry)
  "Return the resources ENTRY's remembering controls operate on.
The selected scope replaces the requested grants once the user changes it."
  (or (car (plist-get entry :resource-selection-cell))
      (plist-get (plist-get entry :requested-additional-permissions)
                 :file-system)))

(defun niwa-permission--prompt-select-resource ()
  "Select exact or containing directory authority for the prompt at point.
Selection changes the visible card only; approval chooses its lifetime."
  (interactive)
  (when-let* ((ov (niwa--prompt--overlay-at-point
                   'niwa-permission-prompt))
              (entry (overlay-get ov 'niwa-view-interaction-entry))
              (cell (plist-get entry :resource-selection-cell)))
    (let* ((originals (plist-get entry :resource-originals))
           (original
            (if (cdr originals)
                (let ((choices
                       (mapcar (lambda (grant)
                                 (cons (niwa-permission--resource-label entry grant)
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
            (unless (niwa-permission-queue-resource-needs-tree-p entry grant)
              (cl-pushnew (cons (niwa-permission--resource-label entry grant) grant)
                          choices :test #'equal)))))
      (setq choices (nreverse choices))
      (when-let* ((selected
                   (cdr (assoc
                         (completing-read "Approve resource scope: "
                                          choices nil t)
                         choices))))
        (niwa-permission-queue-select-resource entry previous selected)
        (niwa-permission-queue--render-head (plist-get entry :session))))))

(defun niwa-permission--prompt-self-insert ()
  "Insert the typed permission key when no permission prompt is active."
  (when (and (characterp last-command-event)
             (not buffer-read-only)
             (not (get-char-property (point) 'read-only)))
    (self-insert-command 1)))

(defun niwa-permission--prompt-finish-or-self-insert (result)
  "Settle the permission prompt at point with RESULT, or insert the key."
  (unless (niwa-permission--prompt-finish result)
    (niwa-permission--prompt-self-insert)))

(defun niwa-permission--prompt-approve-once ()
  "Allow this tool invocation once."
  (interactive)
  (if-let* ((ov (niwa--prompt--overlay-at-point
                'niwa-permission-prompt)))
      (let ((entry (overlay-get ov 'niwa-view-interaction-entry)))
        (if (and (plist-get entry :mutation-p)
                 (niwa-side-conversation-mutation-warning-pending-p entry))
            (progn
              (niwa-permission-queue--render-head
               (plist-get entry :session)))
          (niwa-permission--prompt-finish 'allow-once)))
    (niwa-permission--prompt-self-insert)))

(defun niwa-permission--prompt-approve-session ()
  "Allow this tool for the rest of the session."
  (interactive)
  (if-let* ((ov (niwa--prompt--overlay-at-point
                'niwa-permission-prompt)))
      (if (overlay-get ov 'niwa-permission-suppress-allow-session)
          (niwa-diagnostic-message
           (niwa-diagnostic-scope
            (plist-get (overlay-get ov 'niwa-view-interaction-entry) :session))
           "Session allow is not available for this prompt")
        (niwa-permission--prompt-finish 'allow-session))
    (niwa-permission--prompt-self-insert)))

(defun niwa-permission--prompt-approve-always ()
  "Always allow this tool (persisted to disk)."
  (interactive)
  (if-let* ((ov (niwa--prompt--overlay-at-point
                'niwa-permission-prompt)))
      (if (not (overlay-get ov 'niwa-permission-include-always))
          (niwa-diagnostic-message
           (niwa-diagnostic-scope
            (plist-get (overlay-get ov 'niwa-view-interaction-entry) :session))
           "Persistent allow is not available for this prompt")
        (niwa-permission--prompt-finish 'always-allow))
    (niwa-permission--prompt-self-insert)))

(defun niwa-permission--prompt-deny-once ()
  "Deny this tool invocation once."
  (interactive)
  (niwa-permission--prompt-finish-or-self-insert 'deny-once))

(defun niwa-permission--prompt-deny-session ()
  "Deny this tool for the rest of the session."
  (interactive)
  (niwa-permission--prompt-finish-or-self-insert 'deny-session))

(defun niwa-permission--prompt-feedback ()
  "Deny this tool invocation and pass free-form feedback to the LLM."
  (interactive)
  (if-let* ((ov (niwa--prompt--overlay-at-point 'niwa-permission-prompt)))
      (let* ((entry (overlay-get ov 'niwa-view-interaction-entry))
             (scope (niwa-diagnostic-scope
                     (or (plist-get entry :session) (plist-get entry :workspace))))
             (cell (and entry
                        (gethash entry niwa-permission-queue--settled-cells)))
             (text (read-string "Feedback: ")))
        ;; Timers and browser answers can retire the card while the minibuffer
        ;; is active.  Never redirect its feedback to a replacement at point.
        (cond
         ((eq (cdr cell) 'expired)
          (niwa-diagnostic-message scope "Request expired; nothing was approved"))
         ((or (car cell) (overlay-get ov 'niwa-settled)
              (not (buffer-live-p (overlay-buffer ov))))
          (niwa-diagnostic-message
           scope "Request is no longer pending; nothing was approved."))
         ((and text (not (string-empty-p (string-trim text))))
          (niwa--prompt--settle ov (cons 'feedback (string-trim text))))))
    (niwa-permission--prompt-self-insert)))

(defun niwa-permission--prompt-toggle-remember ()
  "Toggle one remembered capability on the permission prompt at point."
  (interactive)
  (when-let* ((ov (niwa--prompt--overlay-at-point
                   'niwa-permission-prompt))
              (entry (overlay-get ov 'niwa-view-interaction-entry))
              (cell (plist-get entry :remember-authority-cell)))
    (let* ((selection (copy-tree (car cell)))
           (resources (niwa-permission--prompt-resources entry)))
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
                      (cons (niwa-permission--resource-label entry grant)
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
        (niwa-permission-queue--render-head session)))))

(defun niwa-permission--prompt-finish (result)
  "Settle the permission prompt overlay at point with RESULT."
  (when-let* ((ov (niwa--prompt--overlay-at-point
                   'niwa-permission-prompt)))
    (let* ((entry (overlay-get ov 'niwa-view-interaction-entry))
           (scope (niwa-diagnostic-scope
                   (or (plist-get entry :session) (plist-get entry :workspace)))))
      (condition-case err
          (niwa-permission-queue-validate-approval entry result)
        (user-error
         (niwa-diagnostic-user-error scope "%s" (error-message-string err)))))
    (niwa--prompt--settle ov result)
    t))

(defcustom niwa-permission-command-display-limit 400
  "Maximum number of characters of a Bash command shown inline.
Longer commands are elided with a TAB toggle that reveals the rest.
The detected-command summary is never elided, and
a remote collaborator always receives the whole command."
  :type 'integer
  :group 'niwa)

(defcustom niwa-eval-expression-display-limit 8
  "Maximum number of lines to show inline in the Eval permission prompt.
Expressions longer than this are truncated with a toggle to expand."
  :type 'integer
  :group 'niwa)

(defun niwa-permission--anchor (text section)
  "Return TEXT tagged as the card's SECTION, a symbol.
The view finds point and window starts again by this tag when the card
redraws, so expanding details or toggling a choice above the Keys row
keeps point on the row it was on instead of an offset into other text."
  (propertize text 'niwa-view-zone-anchor (list 'permission section)))

(defun niwa-permission--elide (text entry &optional line-limit char-limit)
  "Return TEXT for prompt display, elided unless ENTRY is expanded.
LINE-LIMIT caps the collapsed line count and CHAR-LIMIT the collapsed
character count; either may be nil.  Text properties on TEXT survive, so
callers face TEXT before eliding it.  ENTRY owns the toggle state, so a
prompt without one is returned whole."
  (let* ((lines (and line-limit (split-string text "\n")))
         (long-p (or (and line-limit (> (length lines) line-limit))
                     (and char-limit (> (length text) char-limit)))))
    (niwa-permission--anchor
     (cond
     ((or (null entry) (not long-p)) text)
     ((niwa-queue--entry-metadata-get entry :expanded)
      (concat text "\n"
              (niwa--prompt-key "TAB")
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
                (niwa--prompt-key "TAB")
                (propertize " to expand" 'font-lock-face 'shadow)))))
     'command)))

(defun niwa-permission--prompt-toggle-expand ()
  "Toggle full display of the elided command or expression at point."
  (interactive)
  (when-let* ((ov (niwa--prompt--overlay-at-point
                   'niwa-permission-prompt))
              (entry (overlay-get ov 'niwa-view-interaction-entry)))
    (niwa-queue--entry-metadata-put
     entry :expanded
     (not (niwa-queue--entry-metadata-get entry :expanded)))
    (when-let* ((session (plist-get entry :session)))
      (niwa-permission-queue--render-head session))))

(defun niwa-permission--prompt-body
    (content include-always &optional suppress-allow-session once-only toggle)
  "Return the permission prompt body for CONTENT.
INCLUDE-ALWAYS enables persistent allow.  SUPPRESS-ALLOW-SESSION hides
session allow.  ONCE-ONLY hides every session-scoped choice.  TOGGLE is
`collapsed' or `expanded' when TAB toggles the card's details."
  (niwa--prompt-framed-body
   (concat
    content
    (niwa-permission--anchor
     (concat
      (propertize "Keys: " 'font-lock-face 'help-key-binding)
      (niwa--prompt-key "RET")
      " allow once  "
      (unless (or suppress-allow-session once-only)
        (concat (niwa--prompt-key "s") " remember for session  "))
      (when (and include-always (not once-only))
        (concat (niwa--prompt-key "A") " remember in workspace  "))
      (niwa--prompt-key "d")
      " deny  "
      (unless once-only
        (concat (niwa--prompt-key "D") " deny session  "))
      (niwa--prompt-key "f")
      " feedback"
      (pcase toggle
        ('collapsed (concat "  " (niwa--prompt-key "TAB") " details"))
        ('expanded (concat "  " (niwa--prompt-key "TAB") " fewer details")))
      "\n")
     'keys))
   'warning))

(defun niwa-permission--expanded-p (entry)
  "Return non-nil when ENTRY's card shows its details locally.
A prompt without an entry has no toggle state and shows everything."
  (or (null entry) (niwa-queue--entry-metadata-get entry :expanded)))

(defun niwa-permission--format-justification (entry &optional justification)
  "Format ENTRY's model-supplied reason as quoted explanation, not authority.
JUSTIFICATION, when non-blank, supplies the reason directly."
  (when-let* ((text (if (and (stringp justification)
                             (not (string-empty-p (string-trim justification))))
                        (string-trim justification)
                      (niwa-permission-queue-justification entry))))
    (concat (propertize "Model's reason: " 'font-lock-face 'font-lock-escape-face)
            (propertize text 'font-lock-face 'italic)
            "\n")))

(defun niwa-permission--warning (text)
  "Return warning TEXT as an always-visible card line."
  (concat (propertize "⚠ " 'font-lock-face 'error)
          (propertize text 'font-lock-face 'warning)
          "\n"))

(defun niwa-permission--execution-warnings (entry)
  "Return confinement warnings that stay visible on ENTRY's folded card.
Live Eval and full escalation have unrestricted effects whichever card
kind requests them, including generic hook cards."
  (let* ((kind (plist-get entry :kind))
         (args (plist-get entry :args))
         (live-p
          (pcase kind
            ('eval (equal (or (plist-get entry :mode) "live") "live"))
            ('generic (and (equal (plist-get entry :tool-name) "Eval")
                           (not (member (plist-get args :mode) '("fresh" "sandbox")))))))
         (escalated-p
          (or (eq (plist-get entry :sandbox-permissions) 'require-escalated)
              (equal (plist-get args :sandbox_permissions) "require_escalated"))))
    (concat
     (when live-p
       (niwa-permission--warning
        "Live Eval runs inside this Emacs: filesystem, network and processes are unrestricted."))
     (when escalated-p
       (niwa-permission--warning
        (concat "This command runs directly as your user. Filesystem, network, "
                "and process confinement are all disabled for this invocation.")))
     (when (plist-get entry :protected-path)
       (niwa-permission--warning
        "The requested path is protected by default.")))))

(defun niwa-permission--format-scope (entry)
  "Format the complete additive authority an approval of ENTRY uses."
  (let ((items
         (append
          (and (plist-get (plist-get entry :requested-additional-permissions) :network)
               '("Network"))
          (mapcar (lambda (grant) (niwa-permission--resource-label entry grant))
                  (niwa-permission--prompt-resources entry)))))
    (when items
      (concat (propertize "Scope: " 'font-lock-face 'font-lock-escape-face)
              (propertize (string-join items ", ") 'font-lock-face 'font-lock-constant-face)
              "\n"))))

(defun niwa-permission--format-remember-summary (entry)
  "Summarize changed remembering choices on ENTRY's folded card.
Unchanged defaults stay in the details; a change shows what session or
workspace approval would store."
  (when-let* ((cell (plist-get entry :remember-authority-cell))
              (default (niwa-queue--entry-metadata-get entry :remember-default))
              ((not (equal (car default) (car cell)))))
    (let* ((selection (car cell))
           (label (lambda (grant) (niwa-permission--resource-label entry grant)))
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

(defun niwa-permission--format-review-fallback (entry)
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

(defun niwa-permission--format-cause (entry)
  "Describe ENTRY's captured admission mode and permission cause."
  (let ((fallback (niwa-permission--format-review-fallback entry)))
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

(defun niwa-permission--prompt-async-with-content
    (content include-always cont
             &optional count entry suppress-allow-session once-only)
  "Display a permission prompt for CONTENT and call CONT with its outcome."
  (let* ((cause (niwa-permission--format-cause entry))
         (expanded-p (and entry (niwa-queue--entry-metadata-get entry :expanded)))
         (authority
          (when-let* ((selected (car (plist-get entry :resource-selection-cell))))
            (concat "Selected authority:\n"
                    (mapconcat
                     (lambda (grant) (niwa-permission--resource-label entry grant))
                     selected "\n")
                    "\n(g selects exact resource or directory tree and access)\n\n")))
         ;; Explanations and controls fold locally; the guest has no TAB, so
         ;; its descriptor always receives the complete card.
         (details (concat cause authority))
         (expiry-line
          (when-let* ((session (plist-get entry :session))
                      (deadline (niwa-permission-absence-deadline session)))
            (format "Unanswered requests expire at %s (session policy).\n\n"
                    (format-time-string "%H:%M" deadline))))
         (remote-body (and entry (niwa-queue--entry-metadata-get entry :remote-body)))
         (remote-content (concat (or remote-body content) expiry-line details))
         ;; TAB is offered only when folding hides something.
         (toggle (and entry
                      (cond
                       (expanded-p 'expanded)
                       ((or (not (string-empty-p details))
                            (and remote-body
                                 (not (equal remote-body
                                             (substring-no-properties content)))))
                        'collapsed))))
         (content (concat content expiry-line
                          (and (or (null entry) expanded-p)
                               (niwa-permission--anchor details 'details))))
         ;; The entry's session decides where its card goes.  A prompt can be
         ;; admitted from an unrelated buffer, such as the permission
         ;; reviewer's request buffer when it leaves the decision to a human.
         (source-buffer (let ((owner (and entry (plist-get entry :data-buffer))))
                          (if (buffer-live-p owner) owner (current-buffer))))
         (target-buf
          (if (fboundp 'niwa-view--interaction-target-buffer)
              (niwa-view--interaction-target-buffer
               (niwa--prompt--data-buffer source-buffer))
            (error "No live view for queued prompt")))
         (interaction-id
          (or (and entry
                   (niwa-queue--entry-metadata-get entry :interaction-id))
              (let ((id (list :permission (gensym "permission-"))))
                (when entry
                  (niwa-queue--entry-metadata-put
                   entry :interaction-id id))
                id)))
         (parent-active-warning
          (and (plist-get entry :mutation-p)
               (niwa-side-conversation-mutation-warning
                entry "this approved change")))
         ov)
    (when entry
      (niwa-queue--entry-metadata-put entry :view-buffer target-buf))
    (with-current-buffer target-buf
      (let ((map (make-sparse-keymap)))
        (define-key map "a" #'niwa-permission--prompt-approve-once)
        (define-key map (kbd "RET") #'niwa-permission--prompt-approve-once)
        (define-key map (kbd "<return>")
                    #'niwa-permission--prompt-approve-once)
        (unless (or suppress-allow-session once-only)
          (define-key map "s" #'niwa-permission--prompt-approve-session))
        (when (and include-always (not once-only))
          (define-key map "A" #'niwa-permission--prompt-approve-always))
        (define-key map "d" #'niwa-permission--prompt-deny-once)
        (unless once-only
          (define-key map "D" #'niwa-permission--prompt-deny-session))
        (define-key map "f" #'niwa-permission--prompt-feedback)
        (when (plist-get entry :resource-selection-cell)
          (define-key map "g" #'niwa-permission--prompt-select-resource))
        (when entry
          (define-key map (kbd "TAB")
                      #'niwa-permission--prompt-toggle-expand))
        (when (and entry
                   (plist-get entry :remember-authority-cell)
                   (plist-get entry :reusable-operation-p))
          (define-key map "c" #'niwa-permission--prompt-toggle-remember)
          (when (plist-get
                 (plist-get entry :requested-additional-permissions)
                 :network)
            (define-key map "n"
                        #'niwa-permission--prompt-toggle-remember)))
        (when (plist-get
               (plist-get entry :requested-additional-permissions)
               :file-system)
          (define-key map "p" #'niwa-permission--prompt-toggle-remember))
        (define-key map [?q] #'niwa-permission--prompt-deny-once)
        (define-key map (kbd "C-g") #'niwa-permission--prompt-deny-once)
        (setq ov
              (niwa-view--interaction-register
               (list :kind 'permission
                     :id interaction-id
                     :origin (or (plist-get entry :origin) "/root")
                     :count (or count 1)
                     :body (niwa-permission--prompt-body
                            (concat
                             content
                             (when parent-active-warning
                               (propertize parent-active-warning
                                           'font-lock-face 'warning)))
                            include-always
                            suppress-allow-session once-only
                            toggle)
                     :keymap map
                     :help-echo (if once-only
                                    "One-time permission prompt"
                                  "Permission prompt")
                     :entry entry
                     :activate cont)))
        (when (and entry
                   (not (niwa-queue--entry-metadata-get entry :displayed)))
          (niwa-queue--entry-metadata-put entry :displayed t)
          (niwa-permission-queue--log 'permission-displayed entry))
        (overlay-put ov 'niwa-permission-prompt t)
        (overlay-put ov 'niwa-lobby-woken
                     (niwa-queue--entry-metadata-get entry :lobby-woken))
        (overlay-put ov 'niwa-permission-suppress-allow-session
                     suppress-allow-session)
        (overlay-put ov 'niwa-permission-include-always include-always)
        (overlay-put ov 'niwa--callback cont)
        (overlay-put ov 'niwa-user-request t)
        ;; The remote surface gets the one-shot outcomes only: durable
        ;; session or workspace authority is never mintable from a guest.
        (overlay-put ov 'niwa--remote
                     (list :body (substring-no-properties
                                  (concat remote-content parent-active-warning))
                           :options '((allow-once . "Allow once")
                                      (deny-once . "Deny"))
                           :feedback t))
        (unless entry
          (cl-pushnew ov niwa--prompt-overlays :test #'eq)
          (niwa--prompt--register-canceller source-buffer ov))
        (niwa--prompt-announce ov)))
    ov))


;;
;;; Prompt rendering

(defun niwa-permission--format-authority-capabilities (entry)
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
                  (if (and (niwa-permission-rules-resource-granted-p
                            (plist-get grant :path) (plist-get grant :access) granted)
                           (niwa-permission-rules-resource-granted-p
                            (plist-get grant :path) (plist-get grant :access)
                            (or (plist-get entry :available-resource-grants) granted)
                            (plist-get grant :recursive)))
                      "x" " ")
                  (niwa-permission--resource-label entry grant)))
        (niwa-permission--prompt-resources entry)
        "\n")
       (and (plist-get requested :file-system) "\n")
       "\n"))))

(defun niwa-permission--format-remember-authority (entry)
  "Format reusable authority selections from ENTRY."
  (when-let* ((cell (plist-get entry :remember-authority-cell)))
    (let* ((selection (car cell))
           (resources (niwa-permission--prompt-resources entry))
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
             (niwa-permission--resource-label entry grant)
             (cond
              ((member grant (plist-get selection :file-system)) "With this command")
              ((member grant (plist-get selection :resource-grants)) "Independent path access")
              (t "Do not remember"))))
          resources "\n")
         (and resources "\n(p selects a path and remembering scope)\n")
         "\n")))))

(defun niwa-permission-prompt-render (entry origin cont count)
  "Render permission ENTRY with ORIGIN attribution and pending COUNT.
CONT receives the user's outcome; the caller owns queue settlement."
  (pcase (plist-get entry :kind)
    ('generic
     (niwa-permission--prompt-async-attributed
      (plist-get entry :tool-name) (plist-get entry :specifier-value)
      (plist-get entry :include-always) origin cont count entry))
    ('bash
     (niwa-permission--prompt-async-bash
      (plist-get entry :command) (plist-get entry :command-class)
      (plist-get entry :include-always) origin cont count entry))
    ('eval
     (niwa-permission--prompt-async-eval
      (plist-get entry :expression) origin cont count entry))
    ('sandbox
     (niwa-permission--prompt-async-sandbox
      (plist-get entry :tool-name) (plist-get entry :detail)
      (plist-get entry :justification) origin cont count entry))
    ('schedule
     (niwa-permission--prompt-async-schedule origin cont count entry))
    (_ (error "Unknown permission card kind: %s" (plist-get entry :kind)))))

(defun niwa-permission--prompt-async-card
    (build include-always cont count entry &optional suppress-allow-session once-only)
  "Render the card BUILD describes and call CONT with its outcome.
BUILD receives REMOTE-P and returns card text.  The local card folds its
details until TAB expands them; the guest descriptor, which has no TAB,
always receives the complete card.  INCLUDE-ALWAYS, COUNT, ENTRY,
SUPPRESS-ALLOW-SESSION and ONCE-ONLY follow
`niwa-permission--prompt-async-with-content'."
  (when entry
    (when-let* ((cell (plist-get entry :remember-authority-cell))
                ((not (niwa-queue--entry-metadata-get entry :remember-default))))
      ;; Wrapped so an empty default selection is still recorded.
      (niwa-queue--entry-metadata-put
       entry :remember-default (list (copy-tree (car cell)))))
    (niwa-queue--entry-metadata-put
     entry :remote-body (substring-no-properties (funcall build t))))
  (niwa-permission--prompt-async-with-content
   (funcall build nil) include-always cont count entry
   suppress-allow-session once-only))

(defun niwa-permission--card-title (text &optional face)
  "Return card title TEXT with FACE, which defaults to `warning'."
  (propertize (concat text "\n")
              'font-lock-face (list :inherit 'bold :inherit (or face 'warning))))

(defun niwa-permission--card-field (label value &optional face)
  "Return a card line for LABEL and VALUE, using FACE for VALUE."
  (concat (propertize label 'font-lock-face 'font-lock-escape-face)
          (propertize (format "%s" value)
                      'font-lock-face (or face 'font-lock-constant-face))
          "\n"))

(defun niwa-permission--card-details (entry remote-p)
  "Return ENTRY's authority breakdown and remembering controls when shown.
REMOTE-P requests them regardless of the local fold state."
  (when (and entry (or remote-p (niwa-permission--expanded-p entry)))
    (let ((details (concat
                    (niwa-permission--format-authority-capabilities entry)
                    (niwa-permission--format-remember-authority entry))))
      (unless (string-empty-p details)
        (niwa-permission--anchor (concat "\n" details) 'authority)))))

(defun niwa-permission--prompt-async-attributed
    (tool-name path include-always origin cont &optional count entry)
  "Display an attributed permission prompt and call CONT with its outcome.
TOOL-NAME requests PATH; INCLUDE-ALWAYS, ORIGIN, COUNT and ENTRY follow
the shared permission prompt contract."
  (let* ((path (and path (niwa-permission--display-path entry path)))
         (once-only (and entry (plist-get entry :once-only)))
         (build
          (lambda (_remote-p)
            (concat
             (niwa-permission--card-title "Permission Request")
             (niwa--prompt-attribution-line origin)
             "\n"
             (niwa-permission--card-field "Tool: " tool-name)
             (when path
               (concat
                (niwa-permission--card-field "Path: " path 'font-lock-string-face)
                (when-let* ((access (and entry (plist-get entry :resource-access))))
                  (niwa-permission--card-field "Access: " access))))
             (niwa-permission--format-justification entry)
             (niwa-permission--format-scope entry)
             (niwa-permission--format-remember-summary entry)
             (niwa-permission--execution-warnings entry)
             "\n"))))
    (niwa-permission--prompt-async-card
     build (and include-always (not once-only)) cont count entry
     once-only once-only)))

(defun niwa-permission--prompt-async-bash
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
             (niwa-permission--card-title
              (format "%s Command Execution Request%s"
                      (or (plist-get entry :tool-name) "Bash")
                      (if dangerous " — DANGEROUS" ""))
              (if dangerous 'error 'warning))
             (niwa--prompt-attribution-line origin)
             "\n"
             (niwa-permission--format-justification entry)
             (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
             (if remote-p
                 faced-command
               (niwa-permission--elide
                faced-command entry nil niwa-permission-command-display-limit))
             "\n"
             (when commands-summary
               (niwa-permission--card-field "Detected commands: " commands-summary))
             (niwa-permission--format-scope entry)
             (when (and allow-patterns (not rule-creating-disabled-p)
                        (or (not (plist-get entry :remember-authority-cell))
                            (plist-get (car (plist-get entry :remember-authority-cell))
                                       :operation)))
               (niwa-permission--card-field
                "Session/always allow will add: "
                (mapconcat (lambda (pattern) (format "`%s'" pattern))
                           allow-patterns ", ")))
             (niwa-permission--format-remember-summary entry)
             (when dangerous
               (concat
                (niwa-permission--warning
                 "Contains a binary on `niwa-bash-dangerous-commands'.")
                (when rule-creating-disabled-p
                  (propertize
                   "Session/permanent allow is disabled for dynamic dangerous commands.\n"
                   'font-lock-face 'font-lock-comment-face))))
             (when unparseable
               (concat
                (niwa-permission--warning
                 "Command contains unsupported or dynamic shell syntax.")
                (propertize
                 "Session/permanent allow is disabled for complex Bash commands.\n"
                 'font-lock-face 'font-lock-comment-face)))
             (niwa-permission--execution-warnings entry)
             (niwa-permission--card-details entry remote-p)
             "\n"))))
    (niwa-permission--prompt-async-card
     build (and include-always (not rule-creating-disabled-p))
     cont count entry rule-creating-disabled-p once-only)))

(defun niwa-permission--prompt-async-eval
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
             (niwa-permission--card-title "Elisp Evaluation Request")
             (niwa--prompt-attribution-line origin)
             "\n"
             (propertize "Mode: " 'font-lock-face 'font-lock-escape-face)
             mode
             (when (equal mode "live")
               (format " (inherently unconfined; preserve_ui: %s)"
                       (if preserve-ui "true" "false")))
             "\n"
             (niwa-permission--format-justification entry)
             (propertize "Expression:\n" 'font-lock-face 'font-lock-escape-face)
             (if remote-p
                 faced-expr
               (niwa-permission--elide
                faced-expr entry niwa-eval-expression-display-limit))
             "\n"
             (niwa-permission--format-scope entry)
             (niwa-permission--format-remember-summary entry)
             (niwa-permission--execution-warnings entry)
             (niwa-permission--card-details entry remote-p)
             "\n"))))
    (niwa-permission--prompt-async-card
     build (plist-get entry :include-always) cont count entry
     (not (plist-get entry :remember-authority-cell))
     (not (plist-get entry :remember-authority-cell)))))

(defun niwa-permission--prompt-async-sandbox
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
             (niwa-permission--card-title
              (if full-p
                  "Full Execution Escalation Request"
                "Invocation Authority Request"))
             (niwa--prompt-attribution-line origin)
             "\n"
             (niwa-permission--card-field "Tool: " tool-name)
             (niwa-permission--format-justification entry justification)
             (propertize "Request: " 'font-lock-face 'font-lock-escape-face)
             (if remote-p
                 faced-detail
               (niwa-permission--elide
                faced-detail entry nil niwa-permission-command-display-limit))
             "\n"
             (unless full-p (niwa-permission--format-scope entry))
             (niwa-permission--format-remember-summary entry)
             (niwa-permission--execution-warnings entry)
             (when (or remote-p (niwa-permission--expanded-p entry))
               (concat
                (unless full-p (niwa-permission--card-details entry t))
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
    (niwa-permission--prompt-async-card
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

(defun niwa-permission--prompt-async-schedule (origin cont count entry)
  "Display ENTRY's workspace schedule for one-time approval.
ENTRY carries `:card-title', `:card-fields' as (LABEL . VALUE) pairs, the
run prompt as `:card-text' and an optional `:card-warning'.  ORIGIN, CONT and
COUNT follow the shared permission prompt contract.  Standing authority is
approved once per creation or change, so nothing is offered to remember."
  (niwa-permission--prompt-async-card
   (lambda (_remote-p)
     (concat
      (niwa-permission--card-title (plist-get entry :card-title))
      (niwa--prompt-attribution-line origin)
      "\n"
      (when-let* ((warning (plist-get entry :card-warning)))
        (niwa-permission--warning warning))
      (mapconcat (lambda (field)
                   (niwa-permission--card-field (concat (car field) ": ") (cdr field)))
                 (plist-get entry :card-fields) "")
      (propertize "Prompt:\n" 'font-lock-face 'font-lock-escape-face)
      (propertize (plist-get entry :card-text) 'font-lock-face 'font-lock-string-face)
      "\n\n"))
   nil cont count entry t t))

(provide 'niwa-permission-prompt)

;;; niwa-permission-prompt.el ends here
