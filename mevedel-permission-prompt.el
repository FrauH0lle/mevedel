;;; mevedel-permission-prompt.el -- Permission prompt UI -*- lexical-binding: t -*-

;;; Commentary:

;; Renders and settles generic, Bash, Eval, and execution-authority prompts.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--data-buffer
                  "mevedel-interaction-prompt" (&optional buffer))
(declare-function mevedel--prompt--overlay-at-point
                  "mevedel-interaction-prompt" (property))
(declare-function mevedel--prompt--register-canceller
                  "mevedel-interaction-prompt" (buffer overlay))
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay result))
(declare-function mevedel--prompt-framed-body
                  "mevedel-interaction-prompt" (body face))
(declare-function mevedel--prompt-key
                  "mevedel-interaction-prompt" (key))
(defvar mevedel--prompt-overlays)

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get
                  "mevedel-queue" (entry key))
(declare-function mevedel-queue--entry-metadata-put
                  "mevedel-queue" (entry key value))

;; `mevedel-view-agent'
(declare-function mevedel-view--insert-attribution
                  "mevedel-view-agent" (agent-id))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (data-buffer))


;;
;;; Permission prompt controls

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
  (mevedel-permission--prompt-finish-or-self-insert 'allow-once))

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
    " allow-once  "
    (unless (or suppress-allow-session once-only)
      (concat (mevedel--prompt-key "s") " allow-session  "))
    (when (and include-always (not once-only))
      (concat (mevedel--prompt-key "A") " always-allow  "))
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
        (define-key map [?q] #'mevedel-permission--prompt-deny-once)
        (define-key map (kbd "C-g") #'mevedel-permission--prompt-deny-once)
        (setq ov
              (mevedel-view--interaction-register
               (list :kind 'permission
                     :id interaction-id
                     :count (or count 1)
                     :body (mevedel-permission--prompt-body
                            content include-always
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
        (unless entry
          (cl-pushnew ov mevedel--prompt-overlays :test #'eq)
          (mevedel--prompt--register-canceller source-buffer ov))))
    ov))


;;
;;; Prompt rendering

(defun mevedel-permission--build-attribution-line (origin)
  "Return a ` from <type>--<idshort>\n' line for ORIGIN, or empty string."
  (cond
   ((null origin) "")
   ((equal origin "main") "")
   ((fboundp 'mevedel-view--insert-attribution)
    (concat (mevedel-view--insert-attribution origin) "\n"))
   (t "")))

(defun mevedel-permission--prompt-async-attributed
    (tool-name path include-always origin cont &optional count entry)
  "Display an attributed permission prompt and call CONT with its outcome."
  (let ((content
         (concat
          (propertize "Permission Request\n"
                      'font-lock-face '(:inherit bold :inherit warning))
          (mevedel-permission--build-attribution-line origin)
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
     content include-always cont count entry)))

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
         (rule-creating-disabled-p
          (memq command-class '(dangerous complex)))
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
           (mevedel-permission--build-attribution-line origin)
           "\n"
           (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
           (propertize (format "%s\n" command)
                       'font-lock-face 'font-lock-string-face)
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
              (propertize
               "Session/permanent allow is disabled for dangerous Bash commands.\n"
               'font-lock-face 'font-lock-comment-face)))
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
     cont count entry rule-creating-disabled-p)))

(defun mevedel-permission--prompt-async-eval
    (content cont &optional count entry)
  "Display an Eval permission prompt and call CONT with its outcome."
  (mevedel-permission--prompt-async-with-content
   content nil cont count entry t t))

(defun mevedel-permission--prompt-async-sandbox
    (tool-name detail justification origin cont &optional count entry)
  "Prompt for changed child authority for TOOL-NAME and DETAIL.
JUSTIFICATION is the model's user-facing reason.  ORIGIN, CONT, COUNT, and
ENTRY follow the shared permission prompt contract."
  (let* ((path (plist-get entry :resource-path))
         (access (plist-get entry :resource-access))
         (filesystem-p (and path access))
         (full-p (eq (plist-get entry :sandbox-permissions)
                     'require-escalated))
         (full-rule-disabled-p
          (and full-p (not (plist-get entry :include-always))))
         (content
         (concat
          (propertize
           (cond
            (full-p "Full Execution Escalation Request\n")
            (filesystem-p "Additional Filesystem Permission Request\n")
            (t "Additional Network Permission Request\n"))
                      'font-lock-face '(:inherit bold :inherit warning))
          (mevedel-permission--build-attribution-line origin)
          "\n"
          (propertize "Tool: " 'font-lock-face 'font-lock-escape-face)
          (format "%s\n" tool-name)
          (cond
           (full-p
            (concat
             (propertize "Confinement: "
                         'font-lock-face 'font-lock-escape-face)
             "disabled for this invocation\n"))
           (filesystem-p
            (concat
             (propertize "Path: " 'font-lock-face 'font-lock-escape-face)
             (propertize (format "%s\n" path)
                         'font-lock-face 'font-lock-string-face)
             (propertize "Access: " 'font-lock-face 'font-lock-escape-face)
             (format "%s\n" access)))
           (t
            (concat
             (propertize "Network: " 'font-lock-face 'font-lock-escape-face)
             "unrestricted for this invocation\n")))
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
            (filesystem-p
             (concat
              "Only the named resource is reopened at the requested access "
              "level. Other protected paths, network, and process "
              "confinement remain unchanged.\n"))
            (t
             (concat
              "Network access is the only requested change. The selected "
              "filesystem and process profile remains unchanged.\n")))
           'font-lock-face 'font-lock-comment-face)
          (when full-rule-disabled-p
            (propertize
             (concat
              "Reusable allow is disabled for this request. Author a "
              "qualified rule deliberately if recurring authority is needed.\n")
             'font-lock-face 'font-lock-comment-face)))))
    (mevedel-permission--prompt-async-with-content
     content
     (and (or filesystem-p full-p) (plist-get entry :include-always))
     cont count entry
     (or (not (or filesystem-p full-p)) full-rule-disabled-p)
     (not (or filesystem-p full-p)))))

(provide 'mevedel-permission-prompt)

;;; mevedel-permission-prompt.el ends here
