;;; mevedel-skills-ui.el -- Skill commands and editor UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the user-facing skill surface: local slash commands, the skills
;; cockpit, slash and skill completion, font-lock, and gptel-send dispatch
;; composition.  Skill discovery and invocation remain in the lower modules.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'tabulated-list))

(require 'mevedel-models)
(require 'mevedel-permissions)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-structs)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel-backend-models "ext:gptel-request" (cl-x) t)
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-prompt-prefix-alist)

;; `mevedel-chat'
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context "mevedel-cockpit" ())
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-show-help
                  "mevedel-cockpit" (buffer text))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-details "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-key-help-text
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-compact'
(declare-function mevedel--estimate-tokens "mevedel-compact" ())
(declare-function mevedel-compact "mevedel-compact" (&optional aggressive instructions))

;; `mevedel-execution'
(declare-function mevedel-execution-stop-user
                  "mevedel-execution" (session execution-id))

;; `mevedel-executions-list'
(declare-function mevedel-executions-list-open
                  "mevedel-executions-list" (&optional context))

;; `mevedel-goal'
(declare-function mevedel-goal-clear "mevedel-goal" ())
(declare-function mevedel-goal-edit "mevedel-goal" (objective))
(declare-function mevedel-goal-description "mevedel-goal" (&optional goal))
(declare-function mevedel-goal-pause "mevedel-goal" ())
(declare-function mevedel-goal-resume "mevedel-goal" (&optional input))
(declare-function mevedel-goal-set-budget "mevedel-goal" (value))
(declare-function mevedel-goal-start
                  "mevedel-goal"
                  (objective))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-ranges
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings" (start end binding &optional object))
(declare-function mevedel-mention-bindings-skill-token-start-p
                  "mevedel-mention-bindings" (text start))

;; `mevedel-mentions'
(declare-function mevedel-mentions-prepare-user-input
                  "mevedel-mentions" (text &optional session))

;; `mevedel-menu'
(declare-function mevedel-menu-open "mevedel-menu" (area))

;; `mevedel-models'
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))

;; `mevedel-permissions'
(defvar mevedel-permission-mode)

;; `mevedel-plugins'
(declare-function mevedel-plugin-name "mevedel-plugins" (cl-x) t)
(declare-function mevedel-plugins-list "mevedel-plugins" ())
(declare-function mevedel-plugins-slash-command "mevedel-plugins" (args))
(autoload 'mevedel-plugins-slash-command "mevedel-plugins" nil nil)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--refresh-visited-file-modtime-or-error
                  "mevedel-session-persistence" (&optional expected-texts))
(declare-function mevedel-session-persistence-start-fresh-segment
                  "mevedel-session-persistence" (session buffer &rest args))
(defvar mevedel-session--read-only-mode)

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--clear-pending-inline-attachments
                  "mevedel-skills-invoke" ())
(declare-function mevedel-skills--command-delete-context
                  "mevedel-skills-invoke" (command-pos))
(declare-function mevedel-skills--dispatch-inline-attachments
                  "mevedel-skills-invoke"
                  (&optional continue-fn allow-root))
(declare-function mevedel-skills--dispatch-skill-command
                  "mevedel-skills-invoke" (&optional continue-fn))
(declare-function mevedel-skills--ensure-fresh-line
                  "mevedel-skills-invoke" ())
(declare-function mevedel-skills--parse-prefixed-line
                  "mevedel-skills-invoke" (text prefix))
(declare-function mevedel-skills--scan-skill-tokens
                  "mevedel-skills-invoke" (text lookup &optional allow-root))
(declare-function mevedel-skills-prepare-user-input
                  "mevedel-skills-invoke" (text session))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode-enter
                  "mevedel-plan-mode" (&optional session))

;; `mevedel-structs'
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view-refresh-input-prompt
                  "mevedel-view-composer" ())

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())


;;
;;; Local slash commands

(defvar mevedel-slash-commands)

(defconst mevedel-skills--mode-command-candidates
  '(("ask" . " prompt for edits and uncertain execution")
    ("auto" . " auto-apply edit previews")
    ("full-auto" . " auto-allow tools")
    ("edit" . " alias for auto"))
  "Completion candidates and annotations for `/mode'.")

(defconst mevedel-skills--validation-target-command-candidates
  '(("current" . " current changes")
    ("uncommitted" . " current changes")
    ("HEAD" . " last commit")
    ("last" . " last commit")
    ("branch:" . " base branch")
    ("base:" . " base branch")
    ("commit:" . " specific commit"))
  "Completion candidates and annotations for `/review' and `/verify'.")

(defconst mevedel-skills--plugin-command-candidates
  '(("list" . " installed plugins")
    ("enable" . " enable plugin")
    ("disable" . " disable plugin")
    ("hooks" . " manage plugin hooks")
    ("install" . " install GitHub plugin")
    ("update" . " update installed plugin")
    ("remove" . " remove installed plugin")
    ("uninstall" . " remove installed plugin")
    ("reload" . " rescan current session plugin skills"))
  "Completion candidates and annotations for `/plugin'.")

(defconst mevedel-skills--skills-command-candidates
  '(("list" . " list available skills")
    ("help" . " show help for a skill")
    ("enable" . " enable a skill")
    ("disable" . " disable a skill"))
  "Completion candidates and annotations for `/skills'.")

(defconst mevedel-skills--goal-command-candidates
  '(("auto" . " start with guarded automatic approval")
    ("approval" . " show or set supervised/automatic approval")
    ("edit" . " edit the objective and pause")
    ("pause" . " pause after the active request")
    ("resume" . " resume from the saved phase")
    ("clear" . " clear lifecycle state only"))
  "Completion candidates and annotations for `/goal'.")

(defconst mevedel-skills--slash-command-annotations
  '(("tokens" . " [command] no args; estimate tokens")
    ("model" . " [command] model name")
    ("compact" . " [command] optional summary guidance")
    ("goal" . " [command] objective | auto OBJECTIVE | approval [POLICY] | edit | pause | resume | clear")
    ("plan" . " [command] optional prompt; enter Plan mode")
    ("mode" . " [command] ask | auto | full-auto")
    ("skills" . " [command] list | help NAME | enable NAME | disable NAME")
    ("tools" . " [command] list")
    ("ps" . " [command] no args; list live executions")
    ("stop" . " [command] optional execution ID")
    ("auto" . " [command] no args; toggle auto mode")
    ("clear" . " [command] no args; start a fresh segment")
    ("help" . " [command] no args; list commands and skills")
    ("init" . " [command] optional repository bootstrap focus")
    ("plugin" . " [command] list | enable | disable | hooks | install | update | remove | uninstall | reload")
    ("review" . " [command] picker; target args or custom instructions")
    ("verify" . " [command] picker; target args or custom instructions")
    ("worktree" . " [command] status | create"))
  "Root completion annotations for included slash commands.")

(defun mevedel-cmd--tokens (_args)
  "Print the estimated token usage of the current chat buffer."
  (message "Estimated tokens in this buffer: %d"
           (mevedel--estimate-tokens)))

(defun mevedel-skills--open-menu-or-message (area format-string &rest args)
  "Open cockpit AREA, or message FORMAT-STRING with ARGS."
  (if (fboundp 'mevedel-menu-open)
      (condition-case nil
          (mevedel-menu-open area)
        (user-error
         (apply #'message format-string args)))
    (apply #'message format-string args)))

(defun mevedel-cmd--model (args)
  "Show or set the gptel model for the current chat buffer.
With a non-empty ARGS string, set `gptel-model' to the interned symbol.
A BACKEND:MODEL argument sets both buffer-local `gptel-backend' and
`gptel-model'.  With no ARGS, open the model cockpit surface when the
current buffer belongs to a live session pair."
  (if (and args (not (string-blank-p args)))
      (let ((trimmed (string-trim args)))
        (if-let* ((provider (mevedel-model-resolve-provider trimmed t)))
            (let ((backend (plist-get provider :backend))
                  (model (plist-get provider :model)))
              (setq-local gptel-backend backend)
              (setq-local gptel-model model)
              (message "Model set to %s" trimmed))
          (let ((model (intern trimmed)))
            (setq-local gptel-model model)
            (message "Model set to %s" model))))
    (mevedel-skills--open-menu-or-message
     'model "Current model: %s" gptel-model)))

(defun mevedel-cmd--compact (args)
  "Run `mevedel-compact' on the current chat buffer with ARGS."
  (mevedel-compact nil args))

(defun mevedel-skills--refresh-view-input-prompt ()
  "Refresh the associated view prompt when it is available."
  (let ((view-buf (cond
                   ((and (boundp 'mevedel--view-buffer)
                         (buffer-live-p mevedel--view-buffer))
                    mevedel--view-buffer)
                   ((and (boundp 'mevedel--data-buffer)
                         (buffer-live-p mevedel--data-buffer))
                    (buffer-local-value 'mevedel--view-buffer
                                        mevedel--data-buffer)))))
    (when (and view-buf
               (fboundp 'mevedel-view-refresh-input-prompt))
      (with-current-buffer view-buf
        (mevedel-view-refresh-input-prompt)))))

(defun mevedel-cmd--mode (args)
  "Show or set `mevedel-permission-mode' for the current chat buffer.
ARGS is the raw slash-command argument string.
Recognized modes: ask, auto, full-auto, and the UI alias edit.

Routes through the lifecycle-aware permission transition path."
  (if (and args (not (string-blank-p args)))
      (let ((mode (mevedel-permission-mode-parse-user-input args)))
        (mevedel-permission-mode-transition mode)
        (message "Permission mode set to %s" mode))
    (mevedel-skills--open-menu-or-message
     'mode "Current permission mode: %s" mevedel-permission-mode)))

(defun mevedel-cmd--plan (args)
  "Enter Plan mode; composer-owned nonblank ARGS are submitted as a turn."
  (unless (string-blank-p (or args ""))
    (user-error "/plan takes no arguments"))
  (require 'mevedel-plan-mode)
  (mevedel-plan-mode-enter)
  (message "mevedel: Plan mode on"))

(defun mevedel-cmd--goal (args)
  "Run the `/goal' lifecycle command described by ARGS."
  (require 'mevedel-goal)
  (let* ((args (string-trim (or args "")))
         (parts (split-string args "[ \t\n]+" t))
         (action (car parts))
         (rest (string-join (cdr parts) " ")))
    (pcase action
      ((or 'nil "")
       (require 'mevedel-menu)
       (mevedel-menu-open 'goal))
      ("pause" (mevedel-goal-pause))
      ("budget" (mevedel-goal-set-budget rest))
      ("edit" (mevedel-goal-edit rest))
      ("resume" (mevedel-goal-resume rest))
      ("clear" (mevedel-goal-clear))
      (_
       (mevedel-goal-start args)
       'mevedel-view-sent))))

(defun mevedel-cmd--auto (_args)
  "Toggle auto permission mode for the current session."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (let* ((current (or (mevedel-session-permission-mode mevedel--session)
                      mevedel-permission-mode
                      'ask))
         (auto-on-p (eq current 'auto)))
    (mevedel-permission-mode-transition
     (if auto-on-p 'ask 'auto))
    (if auto-on-p
        (message "mevedel: auto mode off")
      (message "mevedel: auto mode on"))))

(defun mevedel-cmd--clear-trim-bare-prefix (prefix)
  "Delete PREFIX when it is the only text on the pending prompt line."
  (when (and prefix (not (string-empty-p prefix)))
    (let* ((end (point-max))
           (start (- end (length prefix))))
      (when (and (<= (point-min) start)
                 (equal prefix
                        (buffer-substring-no-properties start end))
                 (save-excursion
                   (goto-char start)
                   (= start (line-beginning-position))))
        (delete-region start end)))))

(defun mevedel-cmd--clear (_args)
  "Start a new, empty chat segment."
  (let ((prefix (or (alist-get major-mode gptel-prompt-prefix-alist) "")))
    (cond
     ((bound-and-true-p mevedel-session--read-only-mode)
      (user-error "Session is read-only"))
     ((and (bound-and-true-p mevedel--session)
           (mevedel-session-save-path mevedel--session)
           buffer-file-name)
      (require 'mevedel-session-persistence)
      (mevedel-session-persistence--refresh-visited-file-modtime-or-error)
      (let ((inhibit-read-only t))
        (mevedel-cmd--clear-trim-bare-prefix prefix))
      (mevedel-session-persistence-start-fresh-segment
       mevedel--session (current-buffer)
       :initial-text prefix)
      (mevedel--run-session-start-hooks "clear")
      (message "mevedel: started a fresh chat segment"))
     (t
      (when (yes-or-no-p "Clear all chat buffer content? ")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert prefix)
          (goto-char (point-max)))
        (when (bound-and-true-p mevedel--session)
          (mevedel--run-session-start-hooks "clear"))
        (message "mevedel: cleared chat buffer"))))))

(defun mevedel-cmd--help (_args)
  "Show the list of local slash commands and available `$' skills."
  (let* ((locals (mapconcat
                  (lambda (cell) (format "/%s" (car cell)))
                  mevedel-slash-commands
                  "  "))
         (skills (and mevedel--session
                      (mapconcat
                       (lambda (skill)
                         (format "$%s" (mevedel-skill-name skill)))
                       (mevedel-skills--user-visible-skills
                        mevedel--session)
                       "  "))))
    (mevedel-skills--open-menu-or-message
     'help
     "Commands: %s%s"
     locals
     (if (and skills (not (string-empty-p skills)))
         (format "\nSkills: %s" skills)
       ""))))

(defun mevedel-cmd--skills--require-name (name action)
  "Return NAME or signal a usage error for ACTION."
  (if (and (stringp name) (not (string-empty-p name)))
      name
    (user-error "Usage: /skills %s NAME" action)))

(defconst mevedel-skills-list-buffer-name "*mevedel skills*"
  "Name of the skill listing buffer.")

(defconst mevedel-skills-help-buffer-name "*mevedel skills help*"
  "Name of the skills cockpit help buffer.")

(defun mevedel-skills--skill-status-label (skill)
  "Return user-facing enabled status for SKILL."
  (if (mevedel-skills--skill-enabled-p skill) "enabled" "disabled"))

(defun mevedel-skills--skill-source-label (skill)
  "Return user-facing source label for SKILL."
  (let ((source (or (mevedel-skill-source skill) 'unknown))
        (family (mevedel-skill-source-family skill)))
    (if family
        (format "%s/%s" source family)
      (symbol-name source))))

(defun mevedel-skills-count-label (session)
  "Return enabled/total skill count label for SESSION."
  (let ((enabled 0)
        (total 0))
    (dolist (skill (and session (mevedel-session-skills session)))
      (setq total (1+ total))
      (when (mevedel-skills--skill-enabled-p skill)
        (setq enabled (1+ enabled))))
    (format "%d/%d" enabled total)))

(defun mevedel-skills-list--item-id (skill)
  "Return stable tabulated row id for SKILL."
  (mevedel-skill-name skill))

(defun mevedel-skills-list--status-cell (skill)
  "Return the propertized table status cell for SKILL."
  (let ((status (mevedel-skills--skill-status-label skill)))
    (propertize status
                'face
                (if (mevedel-skills--skill-enabled-p skill)
                    'success
                  'shadow))))

(defun mevedel-skills-list--entry (skill &optional _context)
  "Return a `tabulated-list-mode' row for SKILL."
  (list
   (mevedel-skills-list--item-id skill)
   (vector
    (mevedel-skills-list--status-cell skill)
    (mevedel-skill-name skill)
    (mevedel-skills--skill-source-label skill)
    (or (mevedel-skill-description skill) ""))))

(defun mevedel-skills--skill-detail-text (skill &optional _context)
  "Return detail text for SKILL."
  (concat
   (format
    "Skill %s [%s]\nSource: %s\nDescription: %s"
    (mevedel-skill-name skill)
    (mevedel-skills--skill-status-label skill)
    (mevedel-skills--skill-source-label skill)
    (or (mevedel-skill-description skill) ""))
   (if-let* ((file (mevedel-skill-source-file skill)))
       (format "\nFile: %s" file)
     "")
   (when-let* ((warnings (mevedel-skill-warnings skill)))
     (concat "\nWarnings:\n- " (string-join warnings "\n- ")))))

(defun mevedel-skills-list--session-label (&optional context)
  "Return CONTEXT's skills cockpit session label."
  (if-let* ((session (and context
                          (mevedel-cockpit-context-session context))))
      (mevedel-session-name session)
    "unknown"))

(defun mevedel-skills-list--header-line (&optional items context)
  "Return the skills cockpit header line for ITEMS and CONTEXT."
  (let* ((total (length items))
         (enabled (cl-count-if #'mevedel-skills--skill-enabled-p
                               items)))
    (format "%s  %s  %d/%d enabled    RET details  e toggle  o source  g refresh  ? help  q back"
            (propertize "mevedel: skills"
                        'face 'font-lock-function-name-face)
            (mevedel-skills-list--session-label context)
            enabled
            total)))

(defun mevedel-skills-list--collect (context)
  "Return skill cockpit items for CONTEXT."
  (or (mevedel-cockpit-context-session context)
      (user-error "No mevedel session in this buffer"))
  (mevedel-session-skills (mevedel-cockpit-context-session context)))

(defun mevedel-skills-list-refresh ()
  "Refresh the current skill listing buffer."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-refresh))

(defun mevedel-skills-list--skill-at-point ()
  "Return the skill at point in a skill listing buffer."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-selected))

(defun mevedel-skills-list-details ()
  "Show details for the skill at point."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-details))

(defun mevedel-skills-list-toggle-enabled ()
  "Toggle enabled state for the skill at point."
  (interactive)
  (let* ((skill (mevedel-skills-list--skill-at-point))
         (name (mevedel-skill-name skill))
         (enable (not (mevedel-skills--skill-enabled-p skill))))
    (mevedel-skills--set-enabled skill enable)
    (when-let* ((data-buffer (mevedel-cockpit-context-data-buffer
                              (mevedel-cockpit-surface-context))))
      (with-current-buffer data-buffer
        (mevedel-skills--refresh-view-input-prompt)))
    (mevedel-skills-list-refresh)
    (message "mevedel: skill %s %s"
             name
             (if enable "enabled" "disabled"))))

(defun mevedel-skills-list-open-source ()
  "Open the selected skill's source file or directory."
  (interactive)
  (let* ((skill (mevedel-skills-list--skill-at-point))
         (file (mevedel-skill-source-file skill))
         (dir (mevedel-skill-source-dir skill))
         (target (cond
                  ((and file (file-readable-p file)) file)
                  ((and dir (file-directory-p dir)) dir))))
    (unless target
      (user-error "Skill source is not readable: %s"
                  (or file dir "unknown")))
    (find-file target)))

(defconst mevedel-skills-list--surface
  `(:buffer-name ,mevedel-skills-list-buffer-name
    :label "skills cockpit"
    :row-label "skill"
    :mode mevedel-skills-list-mode
    :format [("State" 9 t)
             ("Name" 24 t)
             ("Source" 18 t)
             ("Description" 0 t)]
    :sort-key ("Name" . nil)
    :require-session t
    :collect mevedel-skills-list--collect
    :entry mevedel-skills-list--entry
    :header mevedel-skills-list--header-line
    :details mevedel-skills--skill-detail-text
    :details-buffer "*mevedel skill details*"
    :help-buffer ,mevedel-skills-help-buffer-name
    :help-function mevedel-skills-list--help-text
    :keys (("e" "Enable or disable selected skill"
            mevedel-skills-list-toggle-enabled)
           ("o" "Open selected skill source"
            mevedel-skills-list-open-source)))
  "Cockpit surface spec for the skill list.")

(defun mevedel-skills-list--help-text (&optional _context)
  "Return help text for the skills cockpit."
  (concat
   "mevedel skills cockpit\n\n"
   "Keys\n"
   (mevedel-cockpit-surface-key-help-text mevedel-skills-list--surface)
   "\n\n"
   "Slash equivalents\n"
   "/skills help NAME\n"
   "/skills enable NAME, /skills disable NAME\n"))

(defun mevedel-skills-list-help ()
  "Open skills cockpit help."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-show-help
   mevedel-skills-help-buffer-name
   (mevedel-skills-list--help-text)))

(defun mevedel-skills-list-quit ()
  "Quit the skills cockpit and return to the main session cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-quit "skills cockpit"))

(define-derived-mode mevedel-skills-list-mode tabulated-list-mode
  "mevedel-skills"
  "Major mode for managing mevedel skills."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-skills-list--surface))

(defun mevedel-skills-list-open (&optional context)
  "Open the skill listing buffer for CONTEXT."
  (require 'mevedel-cockpit)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (unless (mevedel-cockpit-context-session context)
      (user-error "No mevedel session in this buffer"))
    (mevedel-cockpit-open-surface mevedel-skills-list--surface context)))

(defun mevedel-skills--list-message-text (session)
  "Return fallback `/skills list' message text for SESSION."
  (concat
   (format "mevedel skills for %s\n\n"
           (mevedel-session-name session))
   (if-let* ((skills (mevedel-session-skills session)))
       (mapconcat
        (lambda (skill)
          (format "%s [%s] source:%s%s"
                  (mevedel-skill-name skill)
                  (mevedel-skills--skill-status-label skill)
                  (mevedel-skills--skill-source-label skill)
                  (if-let* ((desc (mevedel-skill-description skill)))
                      (if (string-empty-p desc)
                          ""
                        (format " - %s" desc))
                    "")))
        skills
        "\n")
     "No skills available.")))

(defun mevedel-cmd--skills--help (session name)
  "Message help for skill NAME in SESSION."
  (if-let* ((skill (mevedel-session-get-skill session name)))
      (message "%s" (mevedel-skills--skill-detail-text skill))
    (message "Unknown skill: %s" name)))

(defun mevedel-cmd--skills (args)
  "List, describe, enable, or disable skills using ARGS."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (let* ((parts (split-string (or args "") "[ \t\n]+" t))
         (action (or (car parts) "list"))
         (name (cadr parts)))
    (pcase action
      ("list"
       (mevedel-skills--open-menu-or-message
        'skills "%s" (mevedel-skills--list-message-text mevedel--session)))
      ("help"
       (mevedel-cmd--skills--help
        mevedel--session
        (mevedel-cmd--skills--require-name name "help")))
      ("enable"
       (setq name (mevedel-cmd--skills--require-name name "enable"))
       (mevedel-skills--set-enabled
        (or (mevedel-session-get-skill mevedel--session name)
            (user-error "Unknown skill: %s" name))
        t)
       (mevedel-skills--refresh-view-input-prompt)
       (message "Skill %s enabled" name))
      ("disable"
       (setq name (mevedel-cmd--skills--require-name name "disable"))
       (mevedel-skills--set-enabled
        (or (mevedel-session-get-skill mevedel--session name)
            (user-error "Unknown skill: %s" name))
        nil)
       (mevedel-skills--refresh-view-input-prompt)
       (message "Skill %s disabled" name))
      (_
       (message "Usage: /skills [list|help NAME|enable NAME|disable NAME]")))))

(defun mevedel-cmd--tools (args)
  "Open the tools surface using ARGS."
  (let ((args (string-trim (or args ""))))
    (if (member args '("" "list"))
        (if (fboundp 'mevedel-menu-open)
            (mevedel-menu-open 'tools)
          (user-error "No mevedel session cockpit here"))
      (message "Usage: /tools [list]"))))

(defun mevedel-cmd--ps (args)
  "Open the live execution cockpit using ARGS."
  (if (string-blank-p (or args ""))
      (progn
        (require 'mevedel-executions-list)
        (mevedel-executions-list-open))
    (message "Usage: /ps")))

(defun mevedel-cmd--stop (args)
  "Stop the execution named by ARGS, or open the execution cockpit."
  (let ((execution-id (string-trim (or args ""))))
    (if (string-empty-p execution-id)
        (mevedel-cmd--ps nil)
      (unless (bound-and-true-p mevedel--session)
        (user-error "No mevedel session in this buffer"))
      (require 'mevedel-execution)
      (mevedel-execution-stop-user mevedel--session execution-id)
      (message "mevedel: execution %s stopping" execution-id))))

(defvar mevedel-slash-commands
  '(("tokens"  . mevedel-cmd--tokens)
    ("model"   . mevedel-cmd--model)
    ("compact" . mevedel-cmd--compact)
    ("goal"    . mevedel-cmd--goal)
    ("plan"    . mevedel-cmd--plan)
    ("mode"    . mevedel-cmd--mode)
    ("skills"  . mevedel-cmd--skills)
    ("tools"   . mevedel-cmd--tools)
    ("ps"      . mevedel-cmd--ps)
    ("stop"    . mevedel-cmd--stop)
    ("auto"    . mevedel-cmd--auto)
    ("clear"   . mevedel-cmd--clear)
    ("help"    . mevedel-cmd--help)
    ("plugin"  . mevedel-plugins-slash-command))
  "Alist of local slash commands.
Each entry is a (NAME . HANDLER) pair.  HANDLER is a function
accepting a single ARGS string (the text after the command name,
trimmed), and is expected to execute immediately and report its own
result.  If a handler returns a string, the dispatch path shows it with
`message'.
Handlers have access to the buffer-local `mevedel--session'.")

(defun mevedel-skills-local-command-active-request-p (name args)
  "Return non-nil when local command NAME with ARGS may run mid-request."
  (or (member name '("ps" "stop"))
      (and (string= name "goal")
           (member (car (split-string (or args "") "[ \t\n]+" t))
                   '("pause" "edit")))))


;;
;;; Slash-command dispatch

(defun mevedel-skills--parse-slash-line (text)
  "Parse TEXT for a leading `/command [args]' line.

Local slash commands (`/model', `/mode', etc.) parse only the first
whitespace-separated token from ARGS and ignore the rest, so extending
ARGS to include subsequent lines does not change their behavior."
  (mevedel-skills--parse-prefixed-line text ?/))

(defun mevedel-skills--text-after-local-command-delete
    (delete-start region-end after-prefix)
  "Return buffer text after deleting a local slash command region.
DELETE-START and REGION-END bound the command text.  AFTER-PREFIX means
the deleted command followed the prompt prefix."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (delete-region delete-start region-end)
      (unless after-prefix
        (mevedel-skills--ensure-fresh-line))
      (buffer-string))))

(defun mevedel-skills--refresh-visited-file-before-local-edit
    (delete-start region-end after-prefix)
  "Refresh stale visited-file metadata before slash command edits.
DELETE-START and REGION-END bound the command text.  AFTER-PREFIX means
the deleted command followed the prompt prefix."
  (when (and buffer-file-name
             (bound-and-true-p mevedel--session)
             (mevedel-session-save-path mevedel--session))
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence--refresh-visited-file-modtime-or-error
     (mevedel-skills--text-after-local-command-delete
      delete-start region-end after-prefix))))

(defun mevedel-skills--dispatch-slash-command ()
  "Parse and dispatch a local `/command' in the current chat buffer.

Returns:
- `local'   a local command ran; caller should abort the send.
- `unknown' a `/' line was present but matched nothing; caller
            should abort the send.
- nil       no `/command' present; caller should proceed as usual."
  (when-let* ((region (mevedel-skills--current-prompt-region))
              (text (buffer-substring-no-properties (car region) (cdr region)))
              (parsed (mevedel-skills--parse-slash-line text)))
    (let* ((name (nth 0 parsed))
           (args (nth 1 parsed))
           (slash-pos (+ (car region) (nth 2 parsed)))
           (local (assoc name mevedel-slash-commands))
           (delete-context
            (mevedel-skills--command-delete-context slash-pos))
           (delete-start (plist-get delete-context :delete-start))
           (after-prefix (plist-get delete-context :after-prefix)))
      (cond
       (local
        (mevedel-skills--refresh-visited-file-before-local-edit
         delete-start (cdr region) after-prefix)
        (delete-region delete-start (cdr region))
        (unless after-prefix
          (mevedel-skills--ensure-fresh-line))
        (when-let* ((result (funcall (cdr local) args))
                    ((stringp result)))
          (message "%s" result))
        'local)
       (t
        (message "Unknown slash command: /%s" name)
        'unknown)))))

(defun mevedel-skills--gptel-send-advice (orig-fn &rest args)
  "`:around' advice on raw `gptel-send' buffers for command dispatch.

ORIG-FN and ARGS are the original `gptel-send' function and arguments.

Dispatches the leading `/command' or `$skill' on the prompt region first.
- Local commands and unknown slashes abort the send (do not call
  ORIG-FN).
- Inline skills install body + pending-stash, then ORIG-FN is called
  from the invocation callback.
- Fork skills dispatch an agent directly and never call ORIG-FN for
  the `$skill' command.
- No command present -> proceed unchanged.

Paired mevedel view/data buffers already own a deterministic submission plan;
the advice must not rescan their derived prompt text.  Pending-stash cleanup
is tied to the continuation that actually resumes ORIG-FN so async shell
preparation does not clear the stash before the request begin handler can
drain it."
  (if (or (not (bound-and-true-p mevedel--session))
          (and (boundp 'mevedel--view-buffer)
               (buffer-live-p mevedel--view-buffer)))
      (apply orig-fn args)
    (cl-labels
        ((continue ()
           (unwind-protect
               (apply orig-fn args)
             (mevedel-skills--clear-pending-inline-attachments))))
      (when-let* ((region (mevedel-skills--current-prompt-region)))
        (require 'mevedel-mention-bindings)
        (require 'mevedel-mentions)
        (let* ((text (buffer-substring (car region) (cdr region)))
               (prepared
                (mevedel-mentions-prepare-user-input
                 (mevedel-skills-prepare-user-input
                  text mevedel--session)
                 mevedel--session)))
          (dolist (range (mevedel-mention-bindings-ranges prepared))
            (mevedel-mention-bindings-set
             (+ (car region) (plist-get range :start))
             (+ (car region) (plist-get range :end))
             (plist-get range :binding)))))
      (pcase (mevedel-skills--dispatch-slash-command)
        ((or 'local 'unknown) nil)
        (_
         (pcase (mevedel-skills--dispatch-skill-command #'continue)
           ((or 'unknown 'skill) nil)
           (_
            (pcase (mevedel-skills--dispatch-inline-attachments
                    #'continue t)
              ((or 'unknown 'skill) nil)
              (_ (continue))))))))))


;;
;;; Completion at point

(defun mevedel-skills--progressive-argument-hint (skill)
  "Return SKILL's argument-hint string for completion annotation.

If `argument-hint' is set, return it.  Otherwise generate from
`argument-names': `[name1] [name2] ...'.  Returns nil when neither
is available."
  (let ((hint (mevedel-skill-argument-hint skill))
        (names (mevedel-skill-argument-names skill)))
    (cond
     ((and (stringp hint) (not (string-empty-p hint))) hint)
     (names
      (mapconcat (lambda (n) (format "[%s]" n)) names " "))
     (t nil))))

(defun mevedel-skills--remaining-argument-hint (skill arguments)
  "Return display-only remaining argument hint for SKILL and ARGUMENTS.

Explicit `argument-hint' text is shown only before the user starts
typing arguments.  Named `arguments' frontmatter is shown as the
remaining positional slots after shell-style tokenization."
  (let ((hint (mevedel-skill-argument-hint skill))
        (names (mevedel-skill-argument-names skill))
        (tokens (mevedel-skills--parse-arguments arguments)))
    (cond
     ((and (stringp hint)
           (not (string-empty-p hint))
           (null tokens))
      hint)
     (names
      (let ((remaining (nthcdr (length tokens) names)))
        (and remaining
             (mapconcat (lambda (n) (format "[%s]" n))
                        remaining " "))))
     (t nil))))

(defun mevedel-skills--user-visible-skills (session &optional inline-only)
  "Return user-invocable skills visible in `$' completion for SESSION.
When INLINE-ONLY is non-nil, return only inline-context skills."
  (when session
    (cl-remove-if-not
     (lambda (skill)
       (and (mevedel-skill-user-invocable-p skill)
            (mevedel-skills--skill-enabled-p skill)
            (or (not inline-only)
                (eq (mevedel-skill-context skill) 'inline))))
     (mevedel-session-skills session))))

(defun mevedel-skills--slash-candidates (buffer session local-commands)
  "Return fresh slash command completion candidates.
BUFFER and SESSION are accepted for call-site symmetry with skill
completion.  LOCAL-COMMANDS is the slash command alist captured when
the CAPF table was created."
  (ignore buffer session)
  (mapcar #'car local-commands))

(defun mevedel-skills--skill-candidates
    (buffer session &optional inline-only)
  "Return fresh `$' skill completion candidates for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (mevedel-skills--ensure-fresh buffer session)))
  (mapcar #'mevedel-skill-name
          (mevedel-skills--user-visible-skills session inline-only)))

(defun mevedel-skills--skill-by-name
    (buffer session name &optional inline-only)
  "Return fresh `$' completion skill named NAME for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (mevedel-skills--ensure-fresh buffer session)))
  (cl-find name (mevedel-skills--user-visible-skills session inline-only)
           :key #'mevedel-skill-name
           :test #'equal))

(defun mevedel-skills--slash-completion-table
    (buffer session local-commands)
  "Return dynamic completion table for BUFFER, SESSION, and LOCAL-COMMANDS."
  (lambda (string pred action)
    (complete-with-action
     action
     (mevedel-skills--slash-candidates buffer session local-commands)
     string pred)))

(defun mevedel-skills--skill-completion-table
    (buffer session &optional inline-only)
  "Return dynamic `$' skill completion table for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (lambda (string pred action)
    (complete-with-action
     action
     (mevedel-skills--skill-candidates buffer session inline-only)
     string pred)))

(defun mevedel-skills--slash-command-annotation (name)
  "Return root completion annotation for local slash command NAME."
  (or (cdr (assoc name mevedel-skills--slash-command-annotations))
      " [command]"))

(defun mevedel-skills--slash-annotation
    (name _buffer _session local-commands)
  "Return completion annotation for slash command NAME.
LOCAL-COMMANDS is the slash-command alist."
  (and (assoc name local-commands)
       (mevedel-skills--slash-command-annotation name)))

(defun mevedel-skills--skill-annotation
    (name buffer session &optional inline-only)
  "Return completion annotation for `$' skill candidate NAME.
BUFFER and SESSION identify the current chat."
  (let* ((skill (mevedel-skills--skill-by-name
                 buffer session name inline-only))
         (annotation " [skill]")
         (dormant (and skill
                       (mevedel-skill-path-patterns skill)
                       (not (mevedel-skill-active-p skill))))
         (hint (and skill
                    (mevedel-skills--progressive-argument-hint skill))))
    (concat annotation
            (when dormant " [dormant]")
            (when hint (concat " " hint)))))

(defun mevedel-skills--model-command-candidates ()
  "Return current-backend model names suitable for `/model' completion."
  (let ((models (and (bound-and-true-p gptel-backend)
                     (fboundp 'gptel-backend-models)
                     (gptel-backend-models gptel-backend)))
        result)
    (dolist (model models)
      (let ((name (cond
                   ((fboundp 'gptel--model-name)
                    (gptel--model-name model))
                   ((symbolp model) (symbol-name model))
                   ((stringp model) model)
                   (t nil))))
        (when (and (stringp name) (not (string-empty-p name)))
          (push name result))))
    (when (bound-and-true-p gptel-model)
      (let ((name (if (symbolp gptel-model)
                      (symbol-name gptel-model)
                    (format "%s" gptel-model))))
        (unless (string-empty-p name)
          (push name result))))
    (sort (delete-dups result) #'string<)))

(defun mevedel-skills--plugin-name-candidates ()
  "Return installed plugin names for slash command argument completion."
  (require 'mevedel-plugins)
  (sort (delete-dups
         (mapcar #'mevedel-plugin-name (mevedel-plugins-list)))
        #'string<))

(defun mevedel-skills--plugin-command-argument-candidates (args arg-index)
  "Return `/plugin' completion candidates for ARGS at ARG-INDEX.
ARGS is the list of completed arguments before the argument being completed."
  (pcase arg-index
    (0 (mapcar #'car mevedel-skills--plugin-command-candidates))
    (1 (pcase args
         ((or `("enable") `("disable") `("update")
              `("remove") `("uninstall"))
          (mevedel-skills--plugin-name-candidates))
         (`("hooks")
          (delete-dups
           (append '("enable" "disable")
                   (mevedel-skills--plugin-name-candidates))))
         (_ nil)))
    (2 (pcase args
         (`("hooks" ,action)
          (if (member action '("enable" "disable"))
              (mevedel-skills--plugin-name-candidates)
            '("on" "off")))
         (_ nil)))
    (_ nil)))

(defun mevedel-skills--slash-command-argument-candidates
    (name &optional args arg-index)
  "Return completion candidates for slash command NAME.
ARGS is the list of completed command arguments, and ARG-INDEX is the zero-based
index of the argument being completed."
  (let ((arg-index (or arg-index 0)))
    (pcase name
      ("goal"
       (pcase arg-index
         (0 (mapcar #'car mevedel-skills--goal-command-candidates))
         (1 (and (equal args '("approval"))
                 '("supervised" "automatic")))))
      ("mode" (and (zerop arg-index)
                   (mapcar #'car mevedel-skills--mode-command-candidates)))
      ("skills" (and (zerop arg-index)
                     (mapcar #'car mevedel-skills--skills-command-candidates)))
      ("model" (and (zerop arg-index)
                    (mevedel-skills--model-command-candidates)))
      ("plugin" (mevedel-skills--plugin-command-argument-candidates
                 args arg-index))
      ("worktree" (and (zerop arg-index) '("status" "create")))
      ((or "review" "verify")
       (and (zerop arg-index)
            (mapcar #'car mevedel-skills--validation-target-command-candidates)))
      (_ nil))))

(defun mevedel-skills--plugin-command-argument-annotation
    (candidate args arg-index)
  "Return `/plugin' argument annotation for CANDIDATE.
ARGS is the list of completed arguments before ARG-INDEX."
  (pcase arg-index
    (0 (cdr (assoc candidate mevedel-skills--plugin-command-candidates)))
    (1 (pcase args
         ((or `("enable") `("disable") `("update")
              `("remove") `("uninstall"))
          " installed plugin")
         (`("hooks")
          (if (member candidate '("enable" "disable"))
              " hook command"
            " installed plugin"))
         (_ nil)))
    (2 (pcase args
         (`("hooks" ,action)
          (if (member action '("enable" "disable"))
              " installed plugin"
            " hook state"))
         (_ nil)))
    (_ nil)))

(defun mevedel-skills--slash-command-argument-annotation
    (name candidate &optional args arg-index)
  "Return annotation for slash command NAME argument CANDIDATE.
ARGS is the list of completed command arguments, and ARG-INDEX is the zero-based
index of the argument being completed."
  (let ((arg-index (or arg-index 0)))
    (pcase name
      ("goal"
       (pcase arg-index
         (0 (cdr (assoc candidate mevedel-skills--goal-command-candidates)))
         (1 (and (equal args '("approval")) " approval policy"))))
      ("mode" (and (zerop arg-index)
                   (cdr (assoc candidate
                               mevedel-skills--mode-command-candidates))))
      ("skills" (and (zerop arg-index)
                     (cdr (assoc candidate
                                 mevedel-skills--skills-command-candidates))))
      ("model" (and (zerop arg-index) " model"))
      ("plugin" (mevedel-skills--plugin-command-argument-annotation
                 candidate args arg-index))
      ("worktree" (and (zerop arg-index) " command"))
      ((or "review" "verify")
       (and (zerop arg-index)
            (cdr (assoc candidate
                        mevedel-skills--validation-target-command-candidates))))
      (_ nil))))

(defun mevedel-skills--slash-command-argument-table (name args arg-index)
  "Return dynamic completion table for slash command NAME.
ARGS and ARG-INDEX describe the argument position being completed."
  (lambda (string pred action)
    (complete-with-action
     action
     (mevedel-skills--slash-command-argument-candidates
      name args arg-index)
     string pred)))

(defun mevedel-skills--slash-root-exit-function (_candidate status)
  "Insert a real separator after completing a root command candidate.
STATUS is the completion exit status.  Ghost argument hints render after point,
so without an inserted space a user can visually see `$skill [arg]' while the
buffer still contains `$skill'."
  (when (and (memq status '(finished exact sole))
             (or (eobp)
                 (not (memq (char-after) '(?\s ?\t ?\n)))))
    (insert " ")))

(defun mevedel-skills--skill-completion-exit-function
    (buffer session candidate status)
  "Bind completed skill CANDIDATE, then finish root completion.
BUFFER and SESSION identify the discovery context.  STATUS is the
completion exit status."
  (when (memq status '(finished exact sole))
    (when-let* ((skill
                 (and (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (mevedel-session-get-skill session candidate))))
                (source-file (mevedel-skill-source-file skill))
                (end (point))
                (start (- end (length candidate) 1))
                ((>= start (point-min)))
                ((equal (buffer-substring-no-properties start end)
                        (concat "$" candidate))))
      (require 'mevedel-mention-bindings)
      (mevedel-mention-bindings-set
       start end
       (list :kind 'skill
             :token (concat "$" candidate)
             :source-file source-file))))
  (mevedel-skills--slash-root-exit-function candidate status))

(defun mevedel-skills--slash-command-start (&optional input-start)
  "Return slash command start position on this line, or nil.

INPUT-START, when non-nil, is the first editable input position in a
view buffer.  Without INPUT-START, a leading gptel prompt prefix on the
current line is skipped."
  (let ((line-start (line-beginning-position)))
    (cond
     (input-start
      (when (and (<= input-start (point))
                 (= line-start
                    (save-excursion
                      (goto-char input-start)
                      (line-beginning-position))))
        input-start))
     (t
      (let ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
        (if (and prefix
                 (not (string-empty-p prefix))
                 (save-excursion
                   (goto-char line-start)
                   (looking-at-p (regexp-quote prefix))))
            (+ line-start (length prefix))
          line-start))))))

(defun mevedel-skills--slash-capf-context (&optional input-start)
  "Return slash completion context at point.

INPUT-START constrains completion to the first view-composer line.  The return
value is a plist with :kind `root' or `argument' plus :start, :end, and
command-specific fields.  Argument contexts include :args and :arg-index for the
completed arguments before point."
  (catch 'context
    (let* ((slash-start (mevedel-skills--slash-command-start input-start))
           (line-end (line-end-position)))
      (when (and slash-start
                 (<= slash-start (point))
                 (< slash-start line-end)
                 (eq (char-after slash-start) ?/))
        (let* ((name-start (1+ slash-start))
               (name-end (save-excursion
                           (goto-char name-start)
                           (skip-chars-forward "A-Za-z0-9_.:-" line-end)
                           (point))))
          (when (and (<= name-start (point))
                     (<= (point) name-end))
            (throw 'context
                   (list :kind 'root :start name-start :end name-end)))
          (when (and (< name-start name-end)
                     (< name-end line-end)
                     (memq (char-after name-end) '(?\s ?\t))
                     (> (point) name-end))
            (let* ((name (buffer-substring-no-properties
                          name-start name-end))
                   (args-start (save-excursion
                                 (goto-char name-end)
                                 (skip-chars-forward " \t" line-end)
                                 (point))))
              (when (<= args-start (point))
                (let* ((arg-start (save-excursion
                                    (skip-chars-backward "^ \t\n" args-start)
                                    (point)))
                       (args-before
                        (split-string
                         (buffer-substring-no-properties args-start arg-start)
                         "[ \t]+" t)))
                  (throw 'context
                         (list :kind 'argument
                               :name name
                               :args args-before
                               :arg-index (length args-before)
                               :start arg-start
                               :end (point))))))))))))

(defun mevedel-skills--skill-capf-context (&optional input-start)
  "Return `$' skill completion context at point.

INPUT-START constrains completion to the first view-composer line.  The
return value is a plist with :kind `root', :start, :end, and
:inline-only."
  (catch 'context
    (let* ((command-start (mevedel-skills--slash-command-start input-start))
           (line-start (line-beginning-position))
           (line-end (line-end-position)))
      (when (and command-start
                 (<= command-start (point))
                 (< command-start line-end)
                 (eq (char-after command-start) ?$))
        (let* ((name-start (1+ command-start))
               (name-end (save-excursion
                           (goto-char name-start)
                           (skip-chars-forward "A-Za-z0-9_.:-" line-end)
                           (point))))
          (when (and (<= name-start (point))
                     (<= (point) name-end))
            (throw 'context
                   (list :kind 'root
                         :start name-start
                         :end name-end
                         :inline-only nil)))))
      (when (and (or (not input-start)
                     (<= input-start (point)))
                 (not (and command-start
                           (= command-start (point)))))
        (let* ((limit (max line-start (or input-start line-start)))
               (name-start (save-excursion
                             (skip-chars-backward
                              "A-Za-z0-9_.:-" limit)
                             (point)))
               (dollar-pos (1- name-start)))
          (when (and (<= limit dollar-pos)
                     (eq (char-after dollar-pos) ?$))
            (let* ((line-text (buffer-substring-no-properties
                               line-start line-end))
                   (relative-dollar (- dollar-pos line-start)))
              (require 'mevedel-mention-bindings)
              (when (and (mevedel-mention-bindings-skill-token-start-p
                          line-text relative-dollar)
                         (not (mevedel-skills--escaped-position-p
                               line-text relative-dollar)))
                (throw 'context
                       (list :kind 'root
                             :start name-start
                             :end (point)
                             :inline-only t))))))))))

(defun mevedel-skills--slash-capf
    (buffer session local-commands &optional input-start)
  "Return command and skill CAPF for the current buffer.

Slash completion offers local slash commands and command arguments.
Dollar completion offers user-invocable skills.  BUFFER and SESSION are
used for skill discovery.  LOCAL-COMMANDS is the current slash command
alist.  INPUT-START constrains completion to the first view-composer
line when called from the view buffer."
  (when session
    (let ((slash-context (mevedel-skills--slash-capf-context input-start))
          (skill-context (mevedel-skills--skill-capf-context input-start)))
      (pcase (plist-get slash-context :kind)
        ('root
         (list (plist-get slash-context :start)
               (plist-get slash-context :end)
               (mevedel-skills--slash-completion-table
                buffer session local-commands)
               :exclusive 'no
               :annotation-function
               (lambda (name)
                 (mevedel-skills--slash-annotation
                  name buffer session local-commands))
               :exit-function
               #'mevedel-skills--slash-root-exit-function))
        ('argument
         (let* ((name (plist-get slash-context :name))
                (args (plist-get slash-context :args))
                (arg-index (plist-get slash-context :arg-index))
                (candidates
                 (and (assoc name local-commands)
                      (mevedel-skills--slash-command-argument-candidates
                       name args arg-index))))
           (when candidates
             (list (plist-get slash-context :start)
                   (plist-get slash-context :end)
                   (mevedel-skills--slash-command-argument-table
                    name args arg-index)
                   :exclusive 'no
                   :annotation-function
                   (lambda (candidate)
                     (mevedel-skills--slash-command-argument-annotation
                      name candidate args arg-index))))))
        (_
         (pcase (plist-get skill-context :kind)
           ('root
            (mevedel-skills--ensure-fresh buffer session)
            (let ((inline-only (plist-get skill-context :inline-only)))
              (list (plist-get skill-context :start)
                    (plist-get skill-context :end)
                    (mevedel-skills--skill-completion-table
                     buffer session inline-only)
                    :exclusive 'no
                    :annotation-function
                    (lambda (name)
                      (mevedel-skills--skill-annotation
                       name buffer session inline-only))
                    :exit-function
                    (lambda (candidate status)
                      (mevedel-skills--skill-completion-exit-function
                       buffer session candidate status)))))))))))

(defun mevedel-slash-capf ()
  "Completion-at-point for slash commands, `$' skills, and options.

Root completion is active at the line-start `/name' or `$name' position,
after an optional gptel prompt prefix.  Inline `$name' completion is
active in prose for inline-context skills.  Command option completion
is active for slash commands that declare finite argument choices."
  (when (bound-and-true-p mevedel--session)
    (mevedel-skills--slash-capf
     (current-buffer) mevedel--session mevedel-slash-commands)))


;;
;;; Font-lock

(defun mevedel-skills--fontify-dollar-keyword (end)
  "Find a known `$skill' mention before END for font-lock."
  (when-let* ((session (and (bound-and-true-p mevedel--session)
                            mevedel--session)))
    (let* ((origin (point-min))
           (text (buffer-substring-no-properties origin end))
           (scan (- (point) origin))
           (token
            (cl-find-if
             (lambda (candidate) (>= (plist-get candidate :start) scan))
             (mevedel-skills--scan-skill-tokens
              text
              (lambda (name _start _end)
                (mevedel-session-get-skill session name))
              t))))
      (when token
        (let ((start (+ origin (plist-get token :start)))
              (token-end (+ origin (plist-get token :end))))
          (set-match-data (list start token-end))
          (goto-char token-end)
          t)))))

(defun mevedel-skills-install-font-lock ()
  "Install font-lock support for known `$skill' mentions."
  (font-lock-add-keywords
   nil
   '((mevedel-skills--fontify-dollar-keyword
      0 font-lock-keyword-face prepend))
   t))


;;
;;; Installation

;;;###autoload
(defun mevedel-skills-install-slash-commands ()
  "Install the slash-command advice on `gptel-send'.

`:around' so the advice can wrap the call in `unwind-protect' for
pending-stash cleanup."
  (advice-add 'gptel-send :around
              #'mevedel-skills--gptel-send-advice))

(defun mevedel-skills-uninstall-slash-commands ()
  "Remove the slash-command advice from `gptel-send'."
  (advice-remove 'gptel-send
                 #'mevedel-skills--gptel-send-advice))

(provide 'mevedel-skills-ui)
;;; mevedel-skills-ui.el ends here
