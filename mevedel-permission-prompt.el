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

(autoload 'mevedel--prompt-attribution-line "mevedel-interaction-prompt")

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue--render-head
                  "mevedel-permission-queue" (&optional session))

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get
                  "mevedel-queue" (entry key))
(declare-function mevedel-queue--entry-metadata-put
                  "mevedel-queue" (entry key value))

;; `mevedel-side-conversation'
(declare-function mevedel-side-conversation-mutation-warning
                  "mevedel-side-conversation" (record effect))
(declare-function mevedel-side-conversation-mutation-warning-pending-p
                  "mevedel-side-conversation" (record))

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (session))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (data-buffer))


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
  (if-let ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (let ((entry (overlay-get ov 'mevedel-view-interaction-entry)))
        (when (plist-get entry :mutation-p)
          (require 'mevedel-side-conversation))
        (if (and (plist-get entry :mutation-p)
                 (mevedel-side-conversation-mutation-warning-pending-p entry))
            (progn
              (require 'mevedel-permission-queue)
              (mevedel-permission-queue--render-head
               (plist-get entry :session)))
          (mevedel--prompt--settle ov 'allow-once)))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-session ()
  "Allow this tool for the rest of the session."
  (interactive)
  (if-let ((ov (mevedel--prompt--overlay-at-point
                'mevedel-permission-prompt)))
      (if (overlay-get ov 'mevedel-permission-suppress-allow-session)
          (message "Session allow is not available for this prompt")
        (mevedel-permission--prompt-finish 'allow-session))
    (mevedel-permission--prompt-self-insert)))

(defun mevedel-permission--prompt-approve-always ()
  "Always allow this tool (persisted to disk)."
  (interactive)
  (if-let ((ov (mevedel--prompt--overlay-at-point
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
           (missing (plist-get entry :missing-additional-permissions))
           (resources (plist-get missing :file-system)))
      (pcase last-command-event
        (?c
         (setq selection
               (plist-put selection :operation
                          (not (plist-get selection :operation))))
         (unless (plist-get selection :operation)
           (setq selection (plist-put selection :network nil))))
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
                      (cons
                       (format "%s %s"
                               (capitalize
                                (symbol-name (plist-get grant :access)))
                               (mevedel-permission--display-path
                                entry (plist-get grant :path)))
                       grant))
                    resources))
                  (grant
                   (cdr
                    (assoc
                     (completing-read "Remember capability: " choices nil t)
                     choices)))
                  (selected (plist-get selection :file-system)))
             (setq selection
                   (plist-put
                    selection :file-system
                    (if (member grant selected)
                        (delete grant selected)
                      (append selected (list grant)))))))))
      (setcar cell selection)
      (when-let* ((session (plist-get entry :session)))
        (require 'mevedel-permission-queue)
        (mevedel-permission-queue--render-head session)))))

(defun mevedel-permission--prompt-finish (result)
  "Settle the permission prompt overlay at point with RESULT."
  (when-let* ((ov (mevedel--prompt--overlay-at-point
                   'mevedel-permission-prompt)))
    (mevedel--prompt--settle ov result)
    t))

(defun mevedel-permission--prompt-body
    (content include-always &optional suppress-allow-session once-only)
  "Return the permission prompt body for CONTENT.
INCLUDE-ALWAYS enables persistent allow.  SUPPRESS-ALLOW-SESSION hides
session allow.  ONCE-ONLY hides every session-scoped choice."
  (mevedel--prompt-framed-body
   (concat
    content
    (propertize "Keys: " 'font-lock-face 'help-key-binding)
    (mevedel--prompt-key "RET")
    " allow once  "
    (unless (or suppress-allow-session once-only)
      (concat (mevedel--prompt-key "s")
              " remember selected profile for session  "))
    (when (and include-always (not once-only))
      (concat (mevedel--prompt-key "A")
              " remember selected profile in workspace  "))
    (mevedel--prompt-key "d")
    " deny-once  "
    (unless once-only
      (concat (mevedel--prompt-key "D") " deny-session  "))
    (mevedel--prompt-key "f")
    " feedback\n")
   'warning))

(defun mevedel-permission--prompt-async-with-content
    (content include-always cont
             &optional count entry suppress-allow-session once-only)
  "Display a permission prompt for CONTENT and call CONT with its outcome."
  (require 'mevedel-interaction-prompt)
  (require 'mevedel-side-conversation)
  (let* ((source-buffer (current-buffer))
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
        (when (and entry
                   (plist-get entry :remember-authority-cell)
                   (plist-get entry :reusable-operation-p))
          (define-key map "c" #'mevedel-permission--prompt-toggle-remember)
          (when (plist-get
                 (plist-get entry :missing-additional-permissions)
                 :network)
            (define-key map "n"
                        #'mevedel-permission--prompt-toggle-remember)))
        (when (plist-get
               (plist-get entry :missing-additional-permissions)
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
                            suppress-allow-session once-only)
                     :priority 100
                     :keymap map
                     :help-echo (if once-only
                                    "One-time permission prompt"
                                  "Permission prompt")
                     :entry entry
                     :activate cont)))
        (overlay-put ov 'mevedel-permission-prompt t)
        (overlay-put ov 'mevedel-permission-suppress-allow-session
                     suppress-allow-session)
        (overlay-put ov 'mevedel-permission-include-always include-always)
        (overlay-put ov 'mevedel--callback cont)
        (overlay-put ov 'mevedel-user-request t)
        ;; The remote surface gets the one-shot outcomes only: durable
        ;; session or workspace authority is never mintable from a guest.
        (overlay-put ov 'mevedel--remote-body
                     (substring-no-properties content))
        (overlay-put ov 'mevedel--remote-options
                     '((allow-once . "Allow once") (deny-once . "Deny")))
        (overlay-put ov 'mevedel--remote-feedback t)
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
           (missing-grants (plist-get missing :file-system)))
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
          (format "[%s] %s %s"
                  (if (member grant missing-grants) " " "x")
                  (capitalize (symbol-name (plist-get grant :access)))
                  (mevedel-permission--display-path
                   entry (plist-get grant :path))))
        (plist-get requested :file-system)
        "\n")
       (and (plist-get requested :file-system) "\n")
       "\n"))))

(defun mevedel-permission--format-remember-authority (entry)
  "Format reusable authority selections from ENTRY."
  (when-let* ((cell (plist-get entry :remember-authority-cell)))
    (let* ((selection (car cell))
           (missing (plist-get entry :missing-additional-permissions))
           (resources (plist-get missing :file-system))
           (operation-p (plist-get entry :reusable-operation-p))
           (network-p (and operation-p (plist-get missing :network))))
      (when (or operation-p network-p resources)
        (concat
         (propertize "Session/workspace approval remembers the complete selected profile\n"
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
             "[%s] %s %s"
             (if (member grant (plist-get selection :file-system))
                 "x"
               " ")
             (capitalize (symbol-name (plist-get grant :access)))
             (mevedel-permission--display-path
              entry (plist-get grant :path))))
          resources "\n")
         (and resources "\n(p selects an exact path)\n")
         "\n")))))

(defun mevedel-permission--prompt-async-attributed
    (tool-name path include-always origin cont &optional count entry)
  "Display an attributed permission prompt and call CONT with its outcome."
  (let* ((path (and path (mevedel-permission--display-path entry path)))
         (once-only (and entry (plist-get entry :once-only)))
         (content
         (concat
          (propertize "Permission Request\n"
                      'font-lock-face '(:inherit bold :inherit warning))
          (mevedel--prompt-attribution-line origin)
          "\n"
          (propertize "Tool: " 'font-lock-face 'font-lock-escape-face)
          (propertize (format "%s\n" tool-name)
                      'font-lock-face 'font-lock-constant-face)
          (when path
            (concat
             (propertize "Path: " 'font-lock-face 'font-lock-escape-face)
             (propertize (format "%s\n" path)
                         'font-lock-face 'font-lock-string-face)
             (when-let* ((access (and entry
                                      (plist-get entry :resource-access))))
               (concat
                (propertize "Access: "
                            'font-lock-face 'font-lock-escape-face)
                (propertize (format "%s\n" access)
                            'font-lock-face 'font-lock-constant-face)))))
          "\n")))
    (mevedel-permission--prompt-async-with-content
     content (and include-always (not once-only)) cont count entry
     once-only once-only)))

(defun mevedel-permission--bash-guardian-label (value)
  "Return a display label for Bash guardian VALUE."
  (capitalize (replace-regexp-in-string "-" " " (format "%s" value))))

(defun mevedel-permission--bash-guardian-face (risk)
  "Return face for Bash guardian RISK."
  (pcase risk
    ('low 'success)
    ('medium 'warning)
    ((or 'high 'critical) 'error)
    (_ 'font-lock-comment-face)))

(defun mevedel-permission--format-bash-guardian (guardian &optional status)
  "Return formatted Bash GUARDIAN guidance for optional STATUS."
  (cond
   (guardian
    (let ((risk (plist-get guardian :risk))
          (recommendation (plist-get guardian :recommendation))
          (reason (plist-get guardian :reason)))
      (concat
       "\n"
       (propertize "Guardian guidance\n" 'font-lock-face '(:inherit bold))
       (propertize "Risk: " 'font-lock-face 'font-lock-escape-face)
       (propertize (format "%s\n" (mevedel-permission--bash-guardian-label risk))
                   'font-lock-face
                   (mevedel-permission--bash-guardian-face risk))
       (propertize "Recommendation: "
                   'font-lock-face 'font-lock-escape-face)
       (propertize
        (format "%s\n"
                (mevedel-permission--bash-guardian-label recommendation))
        'font-lock-face 'font-lock-constant-face)
       (propertize "Reason: " 'font-lock-face 'font-lock-escape-face)
       (propertize (format "%s\n" reason)
                   'font-lock-face 'font-lock-comment-face))))
   ((eq status 'pending)
    (concat
     "\n"
     (propertize "Guardian guidance\n" 'font-lock-face '(:inherit bold))
     (propertize "Status: " 'font-lock-face 'font-lock-escape-face)
     (propertize "Analyzing command risk...\n"
                 'font-lock-face 'font-lock-comment-face)))
   ((eq status 'unavailable)
    (concat
     "\n"
     (propertize "Guardian guidance\n" 'font-lock-face '(:inherit bold))
     (propertize "Unavailable\n"
                 'font-lock-face 'font-lock-comment-face)))))

(defun mevedel-permission--prompt-async-bash
    (command command-class include-always origin cont &optional count entry)
  "Display a Bash permission prompt and call CONT with its outcome."
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
         (guardian-cell (and entry (plist-get entry :guardian-cell)))
         (guardian (and entry
                        (or (plist-get entry :guardian)
                            (car guardian-cell))))
         (guardian-status (and guardian-cell (cadr guardian-cell)))
         (content
          (concat
           (propertize
            (if dangerous
                "Bash Command Execution Request — DANGEROUS\n"
              "Bash Command Execution Request\n")
            'font-lock-face
            (if dangerous
                '(:inherit bold :inherit error)
              '(:inherit bold :inherit warning)))
           (mevedel--prompt-attribution-line origin)
           "\n"
           (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
           (propertize (format "%s\n" command)
                       'font-lock-face 'font-lock-string-face)
           (mevedel-permission--format-authority-capabilities entry)
           (mevedel-permission--format-remember-authority entry)
           (mevedel-permission--format-bash-guardian
            guardian guardian-status)
           (when commands-summary
             (concat
              "\n"
              (propertize "Detected commands: "
                          'font-lock-face 'font-lock-escape-face)
              (propertize commands-summary
                          'font-lock-face 'font-lock-constant-face)
              "\n"))
           (when (and allow-patterns (not rule-creating-disabled-p))
             (concat
              (propertize "Session/always allow will add: "
                          'font-lock-face 'font-lock-escape-face)
              (propertize
               (mapconcat (lambda (pattern) (format "`%s'" pattern))
                          allow-patterns ", ")
               'font-lock-face 'font-lock-constant-face)
              "\n"))
           (when dangerous
             (concat
              (propertize "⚠ " 'font-lock-face 'error)
              (propertize
               "Contains a binary on `mevedel-bash-dangerous-commands'.\n"
               'font-lock-face 'font-lock-comment-face)
              (when rule-creating-disabled-p
                (propertize
                 "Session/permanent allow is disabled for dynamic dangerous commands.\n"
                 'font-lock-face 'font-lock-comment-face))))
           (when unparseable
             (concat
              (propertize
               "Warning: Command contains unsupported or dynamic shell syntax.\n"
               'font-lock-face 'warning)
              (propertize
               "Session/permanent allow is disabled for complex Bash commands.\n"
               'font-lock-face 'font-lock-comment-face)))
           "\n")))
    (mevedel-permission--prompt-async-with-content
     content (and include-always (not rule-creating-disabled-p))
     cont count entry rule-creating-disabled-p once-only)))

(defun mevedel-permission--prompt-async-eval
    (content cont &optional count entry)
  "Display an Eval permission prompt and call CONT with its outcome."
  (mevedel-permission--prompt-async-with-content
   content (plist-get entry :include-always)
   cont count entry
   (not (plist-get entry :remember-authority-cell))
   (not (plist-get entry :remember-authority-cell))))

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
         (content
         (concat
          (propertize
           (cond
            (full-p "Full Execution Escalation Request\n")
            (t "Invocation Authority Request\n"))
                      'font-lock-face '(:inherit bold :inherit warning))
          (mevedel--prompt-attribution-line origin)
          "\n"
          (propertize "Tool: " 'font-lock-face 'font-lock-escape-face)
          (format "%s\n" tool-name)
          (when full-p
            (concat
             (propertize "Confinement: "
                         'font-lock-face 'font-lock-escape-face)
             "disabled for this invocation\n"))
          (unless full-p
            (concat
             (mevedel-permission--format-authority-capabilities entry)
             (mevedel-permission--format-remember-authority entry)))
          (propertize "Justification: "
                      'font-lock-face 'font-lock-escape-face)
          (format "%s\n\n" justification)
          (propertize "Request:\n" 'font-lock-face 'font-lock-escape-face)
          (propertize (format "%s\n\n" detail)
                      'font-lock-face 'font-lock-string-face)
          (propertize
           (cond
            (full-p
             (concat
              "Warning: this command runs directly as your user. Filesystem, "
              "network, and process confinement are all disabled for this "
              "invocation.\n"))
            (t
             (concat
              "Approval grants every unchecked capability to this invocation. "
              "Checked authority is already available; other resources and "
              "the selected process confinement remain unchanged.\n")))
           'font-lock-face 'font-lock-comment-face)
          (when full-rule-disabled-p
            (propertize
             (concat
              "Reusable allow is disabled for this request. Author a "
              "qualified rule deliberately if recurring authority is needed.\n")
             'font-lock-face 'font-lock-comment-face)))))
    (mevedel-permission--prompt-async-with-content
     content
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
