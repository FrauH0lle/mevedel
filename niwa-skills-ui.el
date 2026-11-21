;;; niwa-skills-ui.el -- Skill commands and editor UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the user-facing skill surface: local slash commands, the skills
;; cockpit, slash and skill completion, font-lock, and gptel-send dispatch
;; composition.  Skill discovery and invocation remain in the lower modules.

;;; Code:

(require 'niwa-report)

(require 'cl-lib)

(eval-when-compile
  (require 'tabulated-list))

(require 'niwa-models)
(require 'niwa-skills-core)
(require 'niwa-structs)
(require 'niwa-utilities)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel-backend-models "ext:gptel-request" (cl-x) t)
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-backend)
(defvar gptel-model)

;; `niwa-background-list'
(declare-function niwa-background-list-open
                  "niwa-background-list" (&optional context))
(autoload 'niwa-background-list-open "niwa-background-list" nil t)

;; `niwa-buddy-feed'
(declare-function niwa-buddy-feed-slash-command "niwa-buddy-feed" (args))
(autoload 'niwa-buddy-feed-slash-command "niwa-buddy-feed")

;; `niwa-chat'
(defvar niwa--dispatching)
(declare-function niwa--dispatch-request "niwa-chat" (model-input local-send &optional native-input))
(declare-function niwa--run-session-start-hooks "niwa-chat" (source))
(autoload 'niwa--dispatch-request "niwa-chat")
(autoload 'niwa--run-session-start-hooks "niwa-chat")

;; `niwa-collaboration'
(declare-function niwa-collaboration-status
                  "niwa-collaboration" nil)
(declare-function niwa-collaboration-stop
                  "niwa-collaboration" nil)
(declare-function niwa-collaboration-stop-all
                  "niwa-collaboration" nil)
(declare-function niwa-collaboration-view
                  "niwa-collaboration" nil)
(autoload 'niwa-collaboration-status "niwa-collaboration" nil t)
(autoload 'niwa-collaboration-stop "niwa-collaboration" nil t)
(autoload 'niwa-collaboration-stop-all "niwa-collaboration" nil t)
(autoload 'niwa-collaboration-view "niwa-collaboration" nil t)

;; `niwa-collaboration-lobby'
(declare-function niwa-collaboration-lobby
                  "niwa-collaboration-lobby" nil)
(declare-function niwa-collaboration-lobby-rotate
                  "niwa-collaboration-lobby" nil)
(declare-function niwa-collaboration-lobby-stop
                  "niwa-collaboration-lobby" nil)
(autoload 'niwa-collaboration-lobby "niwa-collaboration-lobby")
(autoload 'niwa-collaboration-lobby-rotate "niwa-collaboration-lobby")
(autoload 'niwa-collaboration-lobby-stop "niwa-collaboration-lobby")

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-data-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-workspace
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context "niwa-cockpit" ())
(declare-function niwa-cockpit-format-header
                  "niwa-cockpit" (name scope state))
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(declare-function niwa-cockpit-show-help
                  "niwa-cockpit" (buffer text))
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-details "niwa-cockpit" ())
(declare-function niwa-cockpit-surface-key-help-text
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))
(declare-function niwa-cockpit-surface-selected
                  "niwa-cockpit" (&optional no-error))
(autoload 'niwa-cockpit-context-data-buffer "niwa-cockpit")
(autoload 'niwa-cockpit-context-session "niwa-cockpit")
(autoload 'niwa-cockpit-context-workspace "niwa-cockpit")
(autoload 'niwa-cockpit-current-context "niwa-cockpit")
(autoload 'niwa-cockpit-format-header "niwa-cockpit")
(autoload 'niwa-cockpit-open-surface "niwa-cockpit")
(autoload 'niwa-cockpit-quit "niwa-cockpit" nil t)
(autoload 'niwa-cockpit-setup-tabulated-surface "niwa-cockpit")
(autoload 'niwa-cockpit-show-help "niwa-cockpit")
(autoload 'niwa-cockpit-surface-context "niwa-cockpit")
(autoload 'niwa-cockpit-surface-details "niwa-cockpit" nil t)
(autoload 'niwa-cockpit-surface-key-help-text "niwa-cockpit")
(autoload 'niwa-cockpit-surface-refresh "niwa-cockpit" nil t)
(autoload 'niwa-cockpit-surface-selected "niwa-cockpit")

;; `niwa-compact'
(declare-function niwa-compact "niwa-compact" (&optional aggressive instructions))
(autoload 'niwa-compact "niwa-compact" nil t)

;; `niwa-compact-estimation'
(declare-function niwa-compact-estimation-clear-baseline
                  "niwa-compact-estimation" ())
(declare-function niwa-compact-estimation-estimate-tokens
                  "niwa-compact-estimation" ())
(autoload 'niwa-compact-estimation-clear-baseline
  "niwa-compact-estimation")
(autoload 'niwa-compact-estimation-estimate-tokens
  "niwa-compact-estimation")

;; `niwa-engine'
(declare-function niwa-engine-assert-local-history
                  "niwa-engine" (session operation &optional backend root-only))
(autoload 'niwa-engine-assert-local-history "niwa-engine")

;; `niwa-execution'
(declare-function niwa-execution-stop-all-user
                  "niwa-execution" (session))
(declare-function niwa-execution-stop-user
                  "niwa-execution" (session execution-id))
(autoload 'niwa-execution-stop-all-user "niwa-execution")
(autoload 'niwa-execution-stop-user "niwa-execution")

;; `niwa-goal'
(declare-function niwa-goal-clear "niwa-goal" ())
(declare-function niwa-goal-edit "niwa-goal" (objective))
(declare-function niwa-goal-pause "niwa-goal" ())
(declare-function niwa-goal-resume "niwa-goal" (&optional input))
(declare-function niwa-goal-set-budget "niwa-goal" (value))
(declare-function niwa-goal-start
                  "niwa-goal"
                  (objective))
(autoload 'niwa-goal-clear "niwa-goal")
(autoload 'niwa-goal-edit "niwa-goal")
(autoload 'niwa-goal-pause "niwa-goal")
(autoload 'niwa-goal-resume "niwa-goal")
(autoload 'niwa-goal-set-budget "niwa-goal")
(autoload 'niwa-goal-start "niwa-goal")

;; `niwa-memory-list'
(declare-function niwa-remember "niwa-memory-list" (&optional focus context))
(declare-function niwa-remember-drain "niwa-memory-list" (&optional context))
(autoload 'niwa-remember "niwa-memory-list")
(autoload 'niwa-remember-drain "niwa-memory-list")

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-ranges
                  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-set
                  "niwa-mention-bindings" (start end binding &optional object))
(declare-function niwa-mention-bindings-skill-token-start-p
                  "niwa-mention-bindings" (text start))
(autoload 'niwa-mention-bindings-ranges "niwa-mention-bindings")
(autoload 'niwa-mention-bindings-set "niwa-mention-bindings")
(autoload 'niwa-mention-bindings-skill-token-start-p
  "niwa-mention-bindings")

;; `niwa-mentions'
(declare-function niwa-mentions-prepare-user-input
                  "niwa-mentions" (text &optional session))
(autoload 'niwa-mentions-prepare-user-input "niwa-mentions")

;; `niwa-menu'
(declare-function niwa-menu-open "niwa-menu" (area))

;; `niwa-models'
(declare-function niwa-model-resolve-provider
                  "niwa-models" (spec &optional noerror))

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-normalize
                  "niwa-permission-mode" (mode))
(declare-function niwa-permission-mode-transition
                  "niwa-permission-mode" (mode))
(defvar niwa-permission-mode)
(autoload 'niwa-permission-mode-normalize "niwa-permission-mode")
(autoload 'niwa-permission-mode-transition "niwa-permission-mode")

;; `niwa-plan-mode'
(declare-function niwa-plan-mode-enter
                  "niwa-plan-mode" (&optional session))
(autoload 'niwa-plan-mode-enter "niwa-plan-mode" nil t)

;; `niwa-plugin-registry'
(declare-function niwa-plugin-name
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugins-list
                  "niwa-plugin-registry" (&optional workspace))
(autoload 'niwa-plugin-name "niwa-plugin-registry")
(autoload 'niwa-plugins-list "niwa-plugin-registry")

;; `niwa-plugin-ui'
(declare-function niwa-plugins-slash-command "niwa-plugin-ui" (args))
(declare-function niwa-plugins-message-local-result "niwa-plugin-ui" (scope result))
(autoload 'niwa-plugins-slash-command "niwa-plugin-ui" nil nil)
(autoload 'niwa-plugins-message-local-result "niwa-plugin-ui")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-refresh-visited-file-modtime-or-error
                  "niwa-session-artifacts" (&optional expected-texts))
(declare-function niwa-session-artifacts-start-fresh-segment
                  "niwa-session-artifacts"
                  (session buffer &rest keys))
(autoload 'niwa-session-artifacts-refresh-visited-file-modtime-or-error
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-start-fresh-segment
  "niwa-session-artifacts")

;; `niwa-session-naming'
(declare-function niwa-session-naming-cancel "niwa-session-naming" ())
(autoload 'niwa-session-naming-cancel "niwa-session-naming")

;; `niwa-session-persistence'
(defvar niwa-session--read-only-mode)

;; `niwa-side-conversation'
(declare-function niwa-side-conversation-open
                  "niwa-side-conversation" (&optional initial-prompt))
(autoload 'niwa-side-conversation-open "niwa-side-conversation")

;; `niwa-skills-input'
(declare-function niwa-skills-input-clear-pending
                  "niwa-skills-input" ())
(declare-function niwa-skills-input-command-delete-start
                  "niwa-skills-input" (command-pos))
(declare-function niwa-skills-input-current-prompt-region
                  "niwa-skills-input" ())
(declare-function niwa-skills-input-dispatch-command
                  "niwa-skills-input" (&optional continue-fn))
(declare-function niwa-skills-input-dispatch-inline-attachments
                  "niwa-skills-input"
                  (&optional continue-fn allow-root))
(declare-function niwa-skills-input-ensure-fresh-line
                  "niwa-skills-input" ())
(declare-function niwa-skills-input-escaped-position-p
                  "niwa-skills-input" (text pos))
(declare-function niwa-skills-input-parse-prefixed-line
                  "niwa-skills-input" (text prefix))
(declare-function niwa-skills-input-prepare-user-input
                  "niwa-skills-input" (text session))
(declare-function niwa-skills-input-scan-tokens
                  "niwa-skills-input" (text resolver &optional allow-root))
(autoload 'niwa-skills-input-clear-pending "niwa-skills-input")
(autoload 'niwa-skills-input-command-delete-start "niwa-skills-input")
(autoload 'niwa-skills-input-current-prompt-region "niwa-skills-input")
(autoload 'niwa-skills-input-dispatch-command "niwa-skills-input")
(autoload 'niwa-skills-input-dispatch-inline-attachments
  "niwa-skills-input")
(autoload 'niwa-skills-input-ensure-fresh-line "niwa-skills-input")
(autoload 'niwa-skills-input-escaped-position-p "niwa-skills-input")
(autoload 'niwa-skills-input-parse-prefixed-line "niwa-skills-input")
(autoload 'niwa-skills-input-prepare-user-input "niwa-skills-input")
(autoload 'niwa-skills-input-scan-tokens "niwa-skills-input")

;; `niwa-skills-preparation'
(declare-function niwa-skills-preparation-parse-arguments
                  "niwa-skills-preparation" (arguments))
(autoload 'niwa-skills-preparation-parse-arguments
  "niwa-skills-preparation")

;; `niwa-structs'
(declare-function niwa-goal-objective "niwa-structs" (cl-x) t)
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-session-goal "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-input-p
                  "niwa-structs" (session))
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-subscription-usage'
(autoload 'niwa-subscription-usage-show "niwa-subscription-usage" nil t)

;; `niwa-system'
(declare-function niwa-inspect-effective-prompt "niwa-system" ())
(autoload 'niwa-inspect-effective-prompt "niwa-system" nil t)

;; `niwa-turn'
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-turn-busy-p "niwa-turn")

;; `niwa-view-composer'
(declare-function niwa-view-refresh-associated-input-prompt
                  "niwa-view-composer" ())
(autoload 'niwa-view-refresh-associated-input-prompt
  "niwa-view-composer")

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())


;;
;;; Local slash commands

(defvar niwa-slash-commands)

(defconst niwa-skills--mode-command-candidates
  '(("ask" . " prompt for edits and uncertain execution")
    ("edits" . " auto-apply edit previews")
    ("full-auto" . " auto-allow tools"))
  "Completion candidates and annotations for `/mode'.")

(defconst niwa-skills--validation-target-command-candidates
  '(("current" . " current changes")
    ("uncommitted" . " current changes")
    ("HEAD" . " last commit")
    ("last" . " last commit")
    ("branch:" . " base branch")
    ("base:" . " base branch")
    ("commit:" . " specific commit"))
  "Completion candidates and annotations for `/review' and `/verify'.")

(defconst niwa-skills--plugin-command-candidates
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

(defconst niwa-skills--skills-command-candidates
  '(("list" . " list available skills")
    ("help" . " show help for a skill")
    ("enable" . " enable a skill")
    ("disable" . " disable a skill"))
  "Completion candidates and annotations for `/skills'.")

(defconst niwa-skills--goal-command-candidates
  '(("budget" . " set or remove the token limit")
    ("edit" . " replace the objective without resetting the run")
    ("pause" . " pause after the active request")
    ("resume" . " resume, queueing any steering first")
    ("clear" . " clear lifecycle state only"))
  "Completion candidates and annotations for `/goal'.")

(defconst niwa-skills--slash-command-annotations
  '(("btw" . " [command] optional prompt; open an ephemeral side conversation")
    ("usage" . " [command] no args; subscription usage")
    ("tokens" . " [command] no args; estimate tokens")
    ("model" . " [command] model name")
    ("compact" . " [command] optional summary guidance")
    ("remember" . " [command] optional focus or drain; review workspace memory")
    ("goal" . " [command] objective | budget N|none | edit OBJECTIVE | pause | resume [steering] | clear")
    ("plan" . " [command] optional prompt; enter Plan mode")
    ("buddy" . " [command] optional brief | off; let Buddy guide your own implementation")
    ("prompt" . " [command] no args; inspect the effective prompt and tools")
    ("mode" . " [command] ask | edits | full-auto")
    ("skills" . " [command] list | help NAME | enable NAME | disable NAME")
    ("tools" . " [command] list")
    ("ps" . " [command] no args; list live executions")
    ("stop" . " [command] optional execution ID")
    ("edits" . " [command] no args; toggle edits mode")
    ("clear" . " [command] no args; start a fresh segment")
    ("help" . " [command] no args; list commands and skills")
    ("init" . " [command] optional repository bootstrap focus")
    ("plugin" . " [command] list | enable | disable | hooks | install | update | remove | uninstall | reload")
    ("review" . " [command] picker; target args or custom instructions")
    ("verify" . " [command] picker; target args or custom instructions")
    ("worktree" . " [command] status | create"))
  "Root completion annotations for included slash commands.")

(defun niwa-cmd--usage (_args)
  "Display account-wide quotas for all configured subscription backends."
  (niwa-subscription-usage-show))

(defun niwa-cmd--tokens (_args)
  "Print the estimated token usage of the current chat buffer."
  (message "Estimated tokens in this buffer: %d"
           (niwa-compact-estimation-estimate-tokens)))

(defun niwa-skills--open-menu-or-message (area format-string &rest args)
  "Open cockpit AREA, or message FORMAT-STRING with ARGS."
  (if (fboundp 'niwa-menu-open)
      (condition-case nil
          (niwa-menu-open area)
        (user-error
         (apply #'message format-string args)))
    (apply #'message format-string args)))

(defun niwa-cmd--model (args)
  "Show or set the gptel model for the current chat buffer.
With a non-empty ARGS string, set `gptel-model' to the interned symbol.
A BACKEND:MODEL argument sets both buffer-local `gptel-backend' and
`gptel-model'.  With no ARGS, open the model cockpit surface when the
current buffer belongs to a live session pair."
  (if (and args (not (string-blank-p args)))
      (let ((trimmed (string-trim args)))
        (if-let* ((provider (niwa-model-resolve-provider trimmed t)))
            (let ((session (and (boundp 'niwa--session)
                                niwa--session)))
              (if session
                  (niwa-model-set-session-provider
                   session provider (current-buffer))
                (setq-local gptel-backend (plist-get provider :backend)
                            gptel-model (plist-get provider :model)))
              (message "Model set to %s" trimmed))
          (let ((model (intern trimmed)))
            (setq-local gptel-model model)
            (message "Model set to %s" model))))
    (niwa-skills--open-menu-or-message
     'model "Current model: %s" gptel-model)))

(defun niwa-cmd--compact (args)
  "Run `niwa-compact' on the current chat buffer with ARGS."
  (niwa-compact nil args))

(defun niwa-cmd--remember (args)
  "Run workspace memory consolidation with optional focus or drain ARGS."
  (if (equal (string-trim (or args "")) "drain")
      (niwa-remember-drain)
    (niwa-remember args))
  nil)

(defun niwa-cmd--mode (args)
  "Show or set `niwa-permission-mode' for the current chat buffer.
ARGS is the raw slash-command argument string.
Recognized modes: ask, edits, and full-auto.

Routes through the lifecycle-aware permission transition path."
  (if (and args (not (string-blank-p args)))
      (let ((mode (niwa-permission-mode-normalize args)))
        (niwa-permission-mode-transition mode)
        (message "Permission mode set to %s" mode))
    (niwa-skills--open-menu-or-message
     'mode "Current permission mode: %s" niwa-permission-mode)))

(defun niwa-cmd--plan (args)
  "Enter Plan mode; composer-owned nonblank ARGS are submitted as a turn."
  (unless (string-blank-p (or args ""))
    (user-error "/plan takes no arguments"))
  (niwa-plan-mode-enter)
  (message "niwa: Plan mode on"))

(defun niwa-cmd--goal (args)
  "Run the `/goal' lifecycle command described by ARGS."
  (let* ((args (string-trim (or args "")))
         (parts (split-string args "[ \t\n]+" t))
         (action (car parts))
         (rest (string-join (cdr parts) " ")))
    (pcase action
      ((or 'nil "")
       (require 'niwa-menu)
       (niwa-menu-open 'goal))
      ("pause" (niwa-goal-pause))
      ("budget" (niwa-goal-set-budget rest))
      ("edit" (niwa-goal-edit rest))
      ("resume" (niwa-goal-resume rest))
      ("clear" (niwa-goal-clear))
      (_
       (niwa-goal-start args)
       'niwa-view-sent))))

(defun niwa-cmd--edits (_args)
  "Toggle edits permission mode for the current session."
  (unless (bound-and-true-p niwa--session)
    (user-error "No niwa session in this buffer"))
  (let* ((current (or (niwa-session-permission-mode niwa--session)
                      niwa-permission-mode
                      'ask))
         (edits-on-p (eq current 'edits)))
    (niwa-permission-mode-transition
     (if edits-on-p 'ask 'edits))
    (if edits-on-p
        (message "niwa: edits mode off")
      (message "niwa: edits mode on"))))

(defun niwa-cmd--clear (_args)
  "Start a new, empty chat segment."
  (when (and (bound-and-true-p niwa--session)
             (niwa-session-pending-input-p niwa--session))
    (user-error
     "Resolve pending input in the Pending Inputs cockpit or clear it with C-c C-q first"))
  (cond
   ((bound-and-true-p niwa-session--read-only-mode)
    (user-error "Session is read-only"))
   ((and (bound-and-true-p niwa--session)
         (niwa-session-save-path niwa--session)
         buffer-file-name)
    (niwa-session-artifacts-refresh-visited-file-modtime-or-error)
    (niwa-session-artifacts-start-fresh-segment
     niwa--session (current-buffer) :clear t)
    (niwa--run-session-start-hooks "clear")
    (message "niwa: started a fresh chat segment"))
   (t
    (when (yes-or-no-p "Clear all chat buffer content? ")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (niwa-compact-estimation-clear-baseline)
      (when (bound-and-true-p niwa--session)
        (niwa-session-naming-cancel)
        (unless (eq (niwa-session-naming-state niwa--session) 'explicit)
          (setf (niwa-session-naming-state niwa--session) 'pending
                (niwa-session-naming-input niwa--session) nil))
        (niwa--run-session-start-hooks "clear"))
      (message "niwa: cleared chat buffer")))))

(defun niwa-cmd--help (_args)
  "Show the list of local slash commands and available `$' skills."
  (let* ((locals (mapconcat
                  (lambda (cell) (format "/%s" (car cell)))
                  niwa-slash-commands
                  "  "))
         (skills (and niwa--session
                      (mapconcat
                       (lambda (skill)
                         (format "$%s" (niwa-skill-name skill)))
                       (niwa-skills-user-visible-skills
                        niwa--session)
                       "  "))))
    (niwa-skills--open-menu-or-message
     'help
     "Commands: %s%s"
     locals
     (if (and skills (not (string-empty-p skills)))
         (format "\nSkills: %s" skills)
       ""))))

(defun niwa-cmd--skills--require-name (name action)
  "Return NAME or signal a usage error for ACTION."
  (if (and (stringp name) (not (string-empty-p name)))
      name
    (user-error "Usage: /skills %s NAME" action)))

(defconst niwa-skills-list-buffer-name "*niwa skills*"
  "Name of the skill listing buffer.")

(defconst niwa-skills-help-buffer-name "*niwa skills help*"
  "Name of the skills cockpit help buffer.")

(defun niwa-skills--skill-status-label (skill)
  "Return user-facing enabled status for SKILL."
  (if (niwa-skills-skill-enabled-p skill) "enabled" "disabled"))

(defun niwa-skills--skill-source-label (skill)
  "Return user-facing source label for SKILL."
  (let ((source (or (niwa-skill-source skill) 'unknown))
        (family (niwa-skill-source-family skill)))
    (if family
        (format "%s/%s" source family)
      (symbol-name source))))

(defun niwa-skills-count-label (session)
  "Return enabled/total skill count label for SESSION."
  (let ((enabled 0)
        (total 0))
    (dolist (skill (and session (niwa-session-skills session)))
      (setq total (1+ total))
      (when (niwa-skills-skill-enabled-p skill)
        (setq enabled (1+ enabled))))
    (format "%d/%d" enabled total)))

(defun niwa-skills-list--item-id (skill)
  "Return stable tabulated row id for SKILL."
  (niwa-skill-name skill))

(defun niwa-skills-list--status-cell (skill)
  "Return the propertized table status cell for SKILL."
  (let ((status (niwa-skills--skill-status-label skill)))
    (propertize status
                'face
                (if (niwa-skills-skill-enabled-p skill)
                    'success
                  'shadow))))

(defun niwa-skills-list--entry (skill &optional _context)
  "Return a `tabulated-list-mode' row for SKILL."
  (list
   (niwa-skills-list--item-id skill)
   (vector
    (niwa-skills-list--status-cell skill)
    (niwa-skill-name skill)
    (niwa-skills--skill-source-label skill)
    (or (niwa-skill-description skill) ""))))

(defun niwa-skills--skill-detail-report (skill &optional context)
  "Return the complete information report for SKILL."
  (let ((session (and context (niwa-cockpit-context-session context))))
    (list :title "Skill"
          :subtitle (format "%s [%s]" (niwa-skill-name skill) (niwa-skills--skill-status-label skill))
          :identity (niwa-skill-source-file skill)
          :sections
          (list
           (list :id 'skill :title "Skill"
                 :body (niwa-report-fields
                        (list "Name" (niwa-skill-name skill))
                        (list "Status" (niwa-skills--skill-status-label skill))
                        (list "Source" (niwa-skills--skill-source-label skill))
                        (list "Description" (or (niwa-skill-description skill) ""))
                        (when-let* ((file (niwa-skill-source-file skill))) (list "File" file))))
           (list :id 'dependencies :title "Required skills"
                 :body (or (when-let* ((dependencies (niwa-skill-dependencies skill)))
			     (concat
			      "\nRequired skills:\n"
			      (mapconcat
			       (lambda (dependency)
				 (let* ((source-key (niwa-skill-dependency-source-key dependency))
					(child
					 (and session source-key
					      (cl-find source-key (niwa-session-skills session)
						       :key (lambda (candidate)
							      (niwa-skills-source-key
							       (niwa-skill-source-file candidate)))
						       :test #'equal))))
				   (format "- %s%s%s"
					   (niwa-skill-dependency-name dependency)
					   (if source-key (format " -> %s" source-key) "")
					   (if (and child (not (niwa-skill-active-p child)))
					       " [dormant]"
					     ""))))
			       dependencies "\n"))) "No required skills."))
           (list :id 'diagnostics :title "Diagnostics"
                 :body (let ((text (concat      (when-let* ((diagnostics (niwa-skill-dependency-diagnostics skill)))
						  (concat "\nDependency errors:\n- "
							  (string-join diagnostics "\n- ")))
						(when (and (niwa-skill-model-invocable-p skill)
							   (not (niwa-skill-effective-model-invocable-p skill)))
						  "\nModel invocation: disabled by a required skill")
						(when-let* ((warnings (niwa-skill-warnings skill)))
						  (concat "\nWarnings:\n- " (string-join warnings "\n- "))))))
                         (if (string-empty-p text) "No dependency errors or warnings." text)))))))

(defun niwa-skills-list--workspace-label (&optional context)
  "Return CONTEXT's skills cockpit workspace root label."
  (if-let* ((workspace (and context
                            (niwa-cockpit-context-workspace context))))
      (abbreviate-file-name (niwa-workspace-root workspace))
    "no workspace"))

(defun niwa-skills-list--header-line (&optional items context)
  "Return the skills cockpit header line for ITEMS and CONTEXT."
  (let* ((total (length items))
         (enabled (cl-count-if #'niwa-skills-skill-enabled-p
                               items)))
    (niwa-cockpit-format-header
     "skills"
     (niwa-skills-list--workspace-label context)
     (format "%d/%d enabled" enabled total))))

(defun niwa-skills-list--collect (context)
  "Return skill cockpit items for CONTEXT."
  (or (niwa-cockpit-context-session context)
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope (niwa-cockpit-context-workspace context))
       "No niwa session in this buffer"))
  (niwa-session-skills (niwa-cockpit-context-session context)))

(defun niwa-skills-list-refresh ()
  "Refresh the current skill listing buffer."
  (interactive)
  (niwa-cockpit-surface-refresh))

(defun niwa-skills-list--skill-at-point ()
  "Return the skill at point in a skill listing buffer."
  (niwa-cockpit-surface-selected))

(defun niwa-skills-list-details ()
  "Show details for the skill at point."
  (interactive)
  (niwa-cockpit-surface-details))

(defun niwa-skills-list-toggle-enabled ()
  "Toggle enabled state for the skill at point."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (skill (niwa-skills-list--skill-at-point))
         (name (niwa-skill-name skill))
         (enable (not (niwa-skills-skill-enabled-p skill))))
    (niwa-skills-set-enabled
     skill enable (niwa-cockpit-context-session context))
    (when-let* ((data-buffer (niwa-cockpit-context-data-buffer
                              context)))
      (with-current-buffer data-buffer
        (niwa-view-refresh-associated-input-prompt)))
    (niwa-skills-list-refresh)
    (message "niwa: skill %s %s"
             name
             (if enable "enabled" "disabled"))))

(defun niwa-skills-list-open-source ()
  "Open the selected skill's source file or directory."
  (interactive)
  (let* ((skill (niwa-skills-list--skill-at-point))
         (file (niwa-skill-source-file skill))
         (dir (niwa-skill-source-dir skill))
         (target (cond
                  ((and file (file-readable-p file)) file)
                  ((and dir (file-directory-p dir)) dir))))
    (unless target
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope (niwa-skill-workspace skill))
       "Skill source is not readable: %s"
                  (or file dir "unknown")))
    (find-file target)))

(defconst niwa-skills-list--surface
  `(:buffer-name ,niwa-skills-list-buffer-name
    :label "skills cockpit"
    :row-label "skill"
    :mode niwa-skills-list-mode
    :format [("State" 9 t)
             ("Name" 24 t)
             ("Source" 18 t)
             ("Description" 0 t)]
    :sort-key ("Name" . nil)
    :require-session t
    :collect niwa-skills-list--collect
    :entry niwa-skills-list--entry
    :header niwa-skills-list--header-line
    :details niwa-skills--skill-detail-report
    :details-buffer "*niwa skill details*"
    :help-buffer ,niwa-skills-help-buffer-name
    :help-function niwa-skills-list--help-report
    :keys (("e" "Enable or disable selected skill"
            niwa-skills-list-toggle-enabled)
           ("o" "Open selected skill source"
            niwa-skills-list-open-source)))
  "Cockpit surface spec for the skill list.")

(defun niwa-skills-list--help-report (&optional _context)
  "Return the complete cockpit help report."
  (list :title "Skills help"
        :sections (list (list :id 'keys :title "Keys"
                              :body (niwa-cockpit-surface-key-help-text niwa-skills-list--surface))
			(list :id 'slash :title "Slash equivalents" :body "/skills help NAME\n/skills enable NAME, /skills disable NAME\n"))))

(defun niwa-skills-list-help ()
  "Open skills cockpit help."
  (interactive)
  (niwa-cockpit-show-help
   niwa-skills-help-buffer-name
   (niwa-skills-list--help-report)))

(defun niwa-skills-list-quit ()
  "Close the skills cockpit and restore the invoking layout."
  (interactive)
  (niwa-cockpit-quit "skills cockpit"))

(define-derived-mode niwa-skills-list-mode tabulated-list-mode
  "niwa-skills"
  "Major mode for managing niwa skills."
  (niwa-cockpit-setup-tabulated-surface
   niwa-skills-list--surface))

(defun niwa-skills-list-open (&optional context)
  "Open the skill listing buffer for CONTEXT."
  (let ((context (or context (niwa-cockpit-current-context))))
    (unless (niwa-cockpit-context-session context)
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope (niwa-cockpit-context-workspace context))
       "No niwa session in this buffer"))
    (niwa-cockpit-open-surface niwa-skills-list--surface context)))

(defun niwa-skills--list-message-text (session)
  "Return fallback `/skills list' message text for SESSION."
  (concat
   (format "niwa skills for workspace %s\n\n"
           (abbreviate-file-name
            (niwa-workspace-root (niwa-session-workspace session))))
   (if-let* ((skills (niwa-session-skills session)))
       (mapconcat
        (lambda (skill)
          (format "%s [%s] source:%s%s"
                  (niwa-skill-name skill)
                  (niwa-skills--skill-status-label skill)
                  (niwa-skills--skill-source-label skill)
                  (if-let* ((desc (niwa-skill-description skill)))
                      (if (string-empty-p desc)
                          ""
                        (format " - %s" desc))
                    "")))
        skills
        "\n")
     "No skills available.")))

(defun niwa-cmd--skills--help (session name)
  "Message help for skill NAME in SESSION."
  (if-let* ((skill (niwa-session-get-skill session name)))
      (niwa-cockpit-show-help "*niwa skill details*"
                                 (niwa-skills--skill-detail-report skill))
    (message "Unknown skill: %s" name)))

(defun niwa-cmd--skills (args)
  "List, describe, enable, or disable skills using ARGS."
  (unless (bound-and-true-p niwa--session)
    (user-error "No niwa session in this buffer"))
  (let* ((parts (split-string (or args "") "[ \t\n]+" t))
         (action (or (car parts) "list"))
         (name (cadr parts)))
    (pcase action
      ("list"
       (niwa-skills--open-menu-or-message
        'skills "%s" (niwa-skills--list-message-text niwa--session)))
      ("help"
       (niwa-cmd--skills--help
        niwa--session
        (niwa-cmd--skills--require-name name "help")))
      ((or "enable" "disable")
       (setq name (niwa-cmd--skills--require-name name action))
       (niwa-skills-set-enabled
        (or (niwa-session-get-skill niwa--session name)
            (user-error "Unknown skill: %s" name))
        (equal action "enable") niwa--session)
       (niwa-view-refresh-associated-input-prompt)
       (message "Skill %s %sd" name action))
      (_
       (message "Usage: /skills [list|help NAME|enable NAME|disable NAME]")))))

(defun niwa-cmd--tools (args)
  "Open the tools surface using ARGS."
  (let ((args (string-trim (or args ""))))
    (if (member args '("" "list"))
        (if (fboundp 'niwa-menu-open)
            (niwa-menu-open 'tools)
          (user-error "No niwa session cockpit here"))
      (message "Usage: /tools [list]"))))

(defun niwa-cmd--ps (args)
  "Open the background-work cockpit using ARGS."
  (if (string-blank-p (or args ""))
      (progn
        (niwa-background-list-open))
    (message "Usage: /ps")))

(defun niwa-cmd--stop (args)
  "Stop the execution named by ARGS, or every live execution when blank."
  (let ((execution-id (string-trim (or args ""))))
    (unless (bound-and-true-p niwa--session)
      (user-error "No niwa session in this buffer"))
    (if (string-empty-p execution-id)
        (let ((count (niwa-execution-stop-all-user niwa--session)))
          (message "niwa: %d execution%s stopping"
                   count (if (= count 1) "" "s")))
      (niwa-execution-stop-user niwa--session execution-id)
      (message "niwa: execution %s stopping" execution-id))))

(defun niwa-cmd--btw (args)
  "Open an ephemeral side conversation using ARGS as its first prompt."
  (niwa-side-conversation-open args)
  'niwa-view-sent)

(defun niwa-cmd--prompt (_args)
  "Inspect the current session's effective prompt and tools."
  (niwa-inspect-effective-prompt))

(defun niwa-cmd--collab (args)
  "Run the `/collab' command described by ARGS."
  (pcase (string-trim (or args ""))
    ((or "" "view") (niwa-collaboration-view) nil)
    ("status" (niwa-collaboration-status) nil)
    ("stop" (niwa-collaboration-stop) nil)
    ("stop all" (niwa-collaboration-stop-all) nil)
    ("lobby" (niwa-collaboration-lobby) nil)
    ("lobby stop" (niwa-collaboration-lobby-stop) nil)
    ("lobby rotate" (niwa-collaboration-lobby-rotate) nil)
    (_ (message "Usage: /collab [status|stop [all]|lobby [stop|rotate]]") nil)))

(defvar niwa-slash-commands
  '(("btw"     . niwa-cmd--btw)
    ("collab"  . niwa-cmd--collab)
    ("usage"   . niwa-cmd--usage)
    ("tokens"  . niwa-cmd--tokens)
    ("model"   . niwa-cmd--model)
    ("compact" . niwa-cmd--compact)
    ("remember" . niwa-cmd--remember)
    ("goal"    . niwa-cmd--goal)
    ("plan"    . niwa-cmd--plan)
    ("buddy"   . niwa-buddy-feed-slash-command)
    ("prompt"  . niwa-cmd--prompt)
    ("mode"    . niwa-cmd--mode)
    ("skills"  . niwa-cmd--skills)
    ("tools"   . niwa-cmd--tools)
    ("ps"      . niwa-cmd--ps)
    ("stop"    . niwa-cmd--stop)
    ("edits"   . niwa-cmd--edits)
    ("clear"   . niwa-cmd--clear)
    ("help"    . niwa-cmd--help)
    ("plugin"  . niwa-plugins-slash-command))
  "Alist of local slash commands.
Each entry is a (NAME . HANDLER) pair.  HANDLER is a function
accepting a single ARGS string (the text after the command name,
trimmed), and is expected to execute immediately and report its own
result.  If a handler returns a string, the dispatch path shows it with
`message'.
Handlers have access to the buffer-local `niwa--session'.")

(defun niwa-skills-local-command-active-request-p (name args)
  "Return non-nil when local command NAME with ARGS may run mid-request."
  (or (member name '("btw" "collab" "ps" "stop" "remember" "usage"))
      (and (string= name "goal")
           (member (car (split-string (or args "") "[ \t\n]+" t))
                   '("pause" "edit")))))


;;
;;; Slash-command dispatch

(defun niwa-skills-parse-slash-line (text)
  "Parse TEXT for a leading `/command [args]' line.

Local slash commands (`/model', `/mode', etc.) parse only the first
whitespace-separated token from ARGS and ignore the rest, so extending
ARGS to include subsequent lines does not change their behavior."
  (niwa-skills-input-parse-prefixed-line text ?/))

(defun niwa-skills--text-after-local-command-delete
    (delete-start region-end)
  "Return buffer text after deleting a local slash command region.
DELETE-START and REGION-END bound the command text."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (delete-region delete-start region-end)
      (niwa-skills-input-ensure-fresh-line)
      (buffer-string))))

(defun niwa-skills--refresh-visited-file-before-local-edit
    (delete-start region-end)
  "Refresh stale visited-file metadata before slash command edits.
DELETE-START and REGION-END bound the command text."
  (when (and buffer-file-name
             (bound-and-true-p niwa--session)
             (niwa-session-save-path niwa--session))
    (niwa-session-artifacts-refresh-visited-file-modtime-or-error
     (niwa-skills--text-after-local-command-delete
      delete-start region-end))))

(defun niwa-skills-message-command-result (scope name result)
  "Present local command NAME's RESULT under captured SCOPE."
  (when (stringp result)
    (if (equal name "plugin")
        (niwa-plugins-message-local-result scope result)
      (message "%s" result))))

(defun niwa-skills--dispatch-slash-command ()
  "Parse and dispatch a local `/command' in the current chat buffer.

Returns:
- `local'   a local command ran; caller should abort the send.
- `unknown' a `/' line was present but matched nothing; caller
            should abort the send.
- nil       no `/command' present; caller should proceed as usual."
  (when-let* ((region (niwa-skills-input-current-prompt-region))
              (text (buffer-substring-no-properties (car region) (cdr region)))
              (parsed (niwa-skills-parse-slash-line text)))
    (let* ((name (nth 0 parsed))
           (scope (niwa-diagnostic-scope (current-buffer)))
           (args (nth 1 parsed))
           (slash-pos (+ (car region) (nth 2 parsed)))
           (local (assoc name niwa-slash-commands))
           (delete-start
            (niwa-skills-input-command-delete-start slash-pos)))
      (cond
       (local
        (niwa-skills--refresh-visited-file-before-local-edit
         delete-start (cdr region))
        (delete-region delete-start (cdr region))
        (niwa-skills-input-ensure-fresh-line)
        (niwa-skills-message-command-result scope name (funcall (cdr local) args))
        'local)
       (t
        (niwa-diagnostic-message
         (niwa-diagnostic-scope (current-buffer))
         "Unknown slash command: /%s" name)
        'unknown)))))

(defun niwa-skills--gptel-send-advice (orig-fn &rest args)
  "`:around' advice on raw `gptel-send' buffers for command dispatch.

ORIG-FN and ARGS are the original `gptel-send' function and arguments.

Dispatches the leading `/command' or `$skill' on the prompt region first.
- Local commands and unknown slashes abort the send (do not call
  ORIG-FN).
- Inline skills install body + pending-stash, then the invocation
  callback dispatches through the selected conversation engine.
- Fork skills dispatch an agent directly and never call ORIG-FN for
  the `$skill' command.
- No command present -> dispatch through the selected engine.
ORIG-FN remains the send implementation for the gptel engine.

Paired niwa view/data buffers already own a deterministic submission plan;
the advice must not rescan their derived prompt text.  Pending-stash cleanup
is tied to the continuation that actually submits the turn so async shell
preparation does not clear the stash before the request begin handler can
drain it."
  (when (and (eq (car args) 0) (bound-and-true-p niwa--session))
    (niwa-engine-assert-local-history nil "Steering" gptel-backend))
  (if (or (eq (car args) 0) ;Steering does not submit the buffer prompt.
          (equal (car args) '(4)) ;The menu does not submit either.
          (not (bound-and-true-p niwa--session))
          ;; An engine dispatch already owns this send and its model input.
          (bound-and-true-p niwa--dispatching))
      (apply orig-fn args)
    (cl-labels
        ((continue ()
           (unwind-protect
               (progn
                 (when (niwa-turn-busy-p)
                   (niwa-diagnostic-user-error
                    (niwa-diagnostic-scope (current-buffer))
                    "A request is already active -- wait or abort first"))
                 (when (bound-and-true-p niwa-session--read-only-mode)
                   (niwa-diagnostic-user-error
                    (niwa-diagnostic-scope (current-buffer))
                    "Session is open read-only (another host holds the lock)"))
                 (niwa--dispatch-request nil (lambda () (apply orig-fn args))))
             (niwa-skills-input-clear-pending))))
      (if (and (boundp 'niwa--view-buffer)
               (buffer-live-p niwa--view-buffer))
          (continue)
        (when-let* ((region (niwa-skills-input-current-prompt-region)))
          (let* ((text (buffer-substring (car region) (cdr region)))
                 (prepared
                  (niwa-mentions-prepare-user-input
                   (niwa-skills-input-prepare-user-input
                    text niwa--session)
                   niwa--session)))
            (dolist (range (niwa-mention-bindings-ranges prepared))
              (niwa-mention-bindings-set
               (+ (car region) (plist-get range :start))
               (+ (car region) (plist-get range :end))
               (plist-get range :binding)))))
        (pcase (niwa-skills--dispatch-slash-command)
          ((or 'local 'unknown) nil)
          (_
           (pcase (niwa-skills-input-dispatch-command #'continue)
             ((or 'unknown 'skill) nil)
             (_
              (pcase (niwa-skills-input-dispatch-inline-attachments
                      #'continue t)
                ((or 'unknown 'skill) nil)
                (_ (continue)))))))))))


;;
;;; Completion at point

(defun niwa-skills--progressive-argument-hint (skill)
  "Return SKILL's argument-hint string for completion annotation.

If `argument-hint' is set, return it.  Otherwise generate from
`argument-names': `[name1] [name2] ...'.  Returns nil when neither
is available."
  (let ((hint (niwa-skill-argument-hint skill))
        (names (niwa-skill-argument-names skill)))
    (cond
     ((and (stringp hint) (not (string-empty-p hint))) hint)
     (names
      (mapconcat (lambda (n) (format "[%s]" n)) names " "))
     (t nil))))

(defun niwa-skills-remaining-argument-hint (skill arguments)
  "Return display-only remaining argument hint for SKILL and ARGUMENTS.

Explicit `argument-hint' text is shown only before the user starts
typing arguments.  Named `arguments' frontmatter is shown as the
remaining positional slots after shell-style tokenization."
  (let ((hint (niwa-skill-argument-hint skill))
        (names (niwa-skill-argument-names skill))
        (tokens (niwa-skills-preparation-parse-arguments arguments)))
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

(defun niwa-skills-user-visible-skills (session &optional inline-only)
  "Return user-invocable skills visible in `$' completion for SESSION.
When INLINE-ONLY is non-nil, return only inline-context skills."
  (when session
    (cl-remove-if-not
     (lambda (skill)
       (and (niwa-skill-user-invocable-p skill)
            (niwa-skills-skill-enabled-p skill)
            (or (not inline-only)
                (eq (niwa-skill-context skill) 'inline))))
     (niwa-session-skills session))))

(defun niwa-skills--skill-candidates
    (buffer session &optional inline-only)
  "Return fresh `$' skill completion candidates for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (niwa-skills-ensure-fresh buffer session)))
  (mapcar #'niwa-skill-name
          (niwa-skills-user-visible-skills session inline-only)))

(defun niwa-skills--skill-by-name
    (buffer session name &optional inline-only)
  "Return fresh `$' completion skill named NAME for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (niwa-skills-ensure-fresh buffer session)))
  (cl-find name (niwa-skills-user-visible-skills session inline-only)
           :key #'niwa-skill-name
           :test #'equal))

(defun niwa-skills--slash-completion-table (local-commands)
  "Return a dynamic completion table for LOCAL-COMMANDS."
  (lambda (string pred action)
    (complete-with-action
     action
     (mapcar #'car local-commands)
     string pred)))

(defun niwa-skills--skill-completion-table
    (buffer session &optional inline-only)
  "Return dynamic `$' skill completion table for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (lambda (string pred action)
    (complete-with-action
     action
     (niwa-skills--skill-candidates buffer session inline-only)
     string pred)))

(defun niwa-skills--slash-command-annotation (name)
  "Return root completion annotation for local slash command NAME."
  (or (cdr (assoc name niwa-skills--slash-command-annotations))
      " [command]"))

(defun niwa-skills--slash-annotation (name local-commands)
  "Return completion annotation for slash command NAME.
LOCAL-COMMANDS is the slash-command alist."
  (and (assoc name local-commands)
       (niwa-skills--slash-command-annotation name)))

(defun niwa-skills--skill-annotation
    (name buffer session &optional inline-only)
  "Return completion annotation for `$' skill candidate NAME.
BUFFER and SESSION identify the current chat."
  (let* ((skill (niwa-skills--skill-by-name
                 buffer session name inline-only))
         (annotation " [skill]")
         (dormant (and skill
                       (niwa-skill-path-patterns skill)
                       (not (niwa-skill-active-p skill))))
         (hint (and skill
                    (niwa-skills--progressive-argument-hint skill))))
    (concat annotation
            (when dormant " [dormant]")
            (when hint (concat " " hint)))))

(defun niwa-skills--model-command-candidates ()
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

(defun niwa-skills--plugin-name-candidates ()
  "Return installed plugin names for slash command argument completion."
  (sort (delete-dups
         (mapcar #'niwa-plugin-name (niwa-plugins-list)))
        #'string<))

(defun niwa-skills--plugin-command-argument-candidates (args arg-index)
  "Return `/plugin' completion candidates for ARGS at ARG-INDEX.
ARGS is the list of completed arguments before the argument being completed."
  (pcase arg-index
    (0 (mapcar #'car niwa-skills--plugin-command-candidates))
    (1 (pcase args
         ((or `("enable") `("disable") `("update")
              `("remove") `("uninstall"))
          (niwa-skills--plugin-name-candidates))
         (`("hooks")
          (delete-dups
           (append '("enable" "disable")
                   (niwa-skills--plugin-name-candidates))))
         (_ nil)))
    (2 (pcase args
         (`("hooks" ,action)
          (if (member action '("enable" "disable"))
              (niwa-skills--plugin-name-candidates)
            '("on" "off")))
         (_ nil)))
    (_ nil)))

(defun niwa-skills--slash-command-argument-candidates
    (name &optional args arg-index)
  "Return completion candidates for slash command NAME.
ARGS is the list of completed command arguments, and ARG-INDEX is the zero-based
index of the argument being completed."
  (let ((arg-index (or arg-index 0)))
    (pcase name
      ("goal"
       (and (zerop arg-index)
            (mapcar #'car niwa-skills--goal-command-candidates)))
      ("mode" (and (zerop arg-index)
                   (mapcar #'car niwa-skills--mode-command-candidates)))
      ("skills" (and (zerop arg-index)
                     (mapcar #'car niwa-skills--skills-command-candidates)))
      ("model" (and (zerop arg-index)
                    (niwa-skills--model-command-candidates)))
      ("plugin" (niwa-skills--plugin-command-argument-candidates
                 args arg-index))
      ("worktree" (and (zerop arg-index) '("status" "create")))
      ("buddy" (and (zerop arg-index) '("off")))
      ((or "review" "verify")
       (and (zerop arg-index)
            (mapcar #'car niwa-skills--validation-target-command-candidates)))
      (_ nil))))

(defun niwa-skills--plugin-command-argument-annotation
    (candidate args arg-index)
  "Return `/plugin' argument annotation for CANDIDATE.
ARGS is the list of completed arguments before ARG-INDEX."
  (pcase arg-index
    (0 (cdr (assoc candidate niwa-skills--plugin-command-candidates)))
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

(defun niwa-skills--slash-command-argument-annotation
    (name candidate &optional args arg-index)
  "Return annotation for slash command NAME argument CANDIDATE.
ARGS is the list of completed command arguments, and ARG-INDEX is the zero-based
index of the argument being completed."
  (let ((arg-index (or arg-index 0)))
    (pcase name
      ("goal"
       (and (zerop arg-index)
            (cdr (assoc candidate
                        niwa-skills--goal-command-candidates))))
      ("mode" (and (zerop arg-index)
                   (cdr (assoc candidate
                               niwa-skills--mode-command-candidates))))
      ("skills" (and (zerop arg-index)
                     (cdr (assoc candidate
                                 niwa-skills--skills-command-candidates))))
      ("model" (and (zerop arg-index) " model"))
      ("plugin" (niwa-skills--plugin-command-argument-annotation
                 candidate args arg-index))
      ("worktree" (and (zerop arg-index) " command"))
      ((or "review" "verify")
       (and (zerop arg-index)
            (cdr (assoc candidate
                        niwa-skills--validation-target-command-candidates))))
      (_ nil))))

(defun niwa-skills--slash-command-argument-table (name args arg-index)
  "Return dynamic completion table for slash command NAME.
ARGS and ARG-INDEX describe the argument position being completed."
  (lambda (string pred action)
    (complete-with-action
     action
     (niwa-skills--slash-command-argument-candidates
      name args arg-index)
     string pred)))

(defun niwa-skills--slash-root-exit-function (_candidate status)
  "Insert a real separator after completing a root command candidate.
STATUS is the completion exit status.  Ghost argument hints render after point,
so without an inserted space a user can visually see `$skill [arg]' while the
buffer still contains `$skill'."
  (when (and (memq status '(finished exact sole))
             (not (memq (char-after) '(?\s ?\t ?\n))))
    (insert " ")))

(defun niwa-skills--skill-completion-exit-function
    (buffer session candidate status)
  "Bind completed skill CANDIDATE, then finish root completion.
BUFFER and SESSION identify the discovery context.  STATUS is the
completion exit status."
  (when (memq status '(finished exact sole))
    (when-let* ((skill
                 (and (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (niwa-session-get-skill session candidate))))
                (source-file (niwa-skill-source-file skill))
                (end (point))
                (start (- end (length candidate) 1))
                ((>= start (point-min)))
                ((equal (buffer-substring-no-properties start end)
                        (concat "$" candidate))))
      (niwa-mention-bindings-set
       start end
       (list :kind 'skill
             :token (concat "$" candidate)
             :source-file source-file))))
  (niwa-skills--slash-root-exit-function candidate status))

(defun niwa-skills--slash-command-start (&optional input-start)
  "Return slash command start position on this line, or nil.

INPUT-START, when non-nil, is the first editable position in the
composer; without it, a command starts at the beginning of the line."
  (let ((line-start (line-beginning-position)))
    (if input-start
        (when (and (<= input-start (point))
                   (= line-start
                      (save-excursion
                        (goto-char input-start)
                        (line-beginning-position))))
          input-start)
      line-start)))

(defun niwa-skills-slash-capf-context (&optional input-start)
  "Return slash completion context at point.

INPUT-START constrains completion to the first view-composer line.  The return
value is a plist with :kind `root' or `argument' plus :start, :end, and
command-specific fields.  Argument contexts include :args and :arg-index for the
completed arguments before point."
  (catch 'context
    (let* ((slash-start (niwa-skills--slash-command-start input-start))
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

(defun niwa-skills--skill-capf-context (&optional input-start)
  "Return `$' skill completion context at point.

INPUT-START constrains completion to the first view-composer line.  The
return value is a plist with :kind `root', :start, :end, and
:inline-only."
  (catch 'context
    (let* ((command-start (niwa-skills--slash-command-start input-start))
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
              (when (and (niwa-mention-bindings-skill-token-start-p
                          line-text relative-dollar)
                         (not (niwa-skills-input-escaped-position-p
                               line-text relative-dollar)))
                (throw 'context
                       (list :kind 'root
                             :start name-start
                             :end (point)
                             :inline-only t))))))))))

(defun niwa-skills-slash-capf
    (buffer session local-commands &optional input-start)
  "Return command and skill CAPF for the current buffer.

Slash completion offers local slash commands and command arguments.
Dollar completion offers user-invocable skills.  BUFFER and SESSION are
used for skill discovery.  LOCAL-COMMANDS is the current slash command
alist.  INPUT-START gives the editable origin in a composer buffer."
  (when session
    (let ((slash-context (niwa-skills-slash-capf-context input-start))
          (skill-context (niwa-skills--skill-capf-context input-start)))
      (pcase (plist-get slash-context :kind)
        ('root
         (list (plist-get slash-context :start)
               (plist-get slash-context :end)
               (niwa-skills--slash-completion-table local-commands)
               :exclusive 'no
               :annotation-function
               (lambda (name)
                 (niwa-skills--slash-annotation
                  name local-commands))
               :exit-function
               #'niwa-skills--slash-root-exit-function))
        ('argument
         (let* ((name (plist-get slash-context :name))
                (args (plist-get slash-context :args))
                (arg-index (plist-get slash-context :arg-index))
                (candidates
                 (and (assoc name local-commands)
                      (niwa-skills--slash-command-argument-candidates
                       name args arg-index))))
           (when candidates
             (list (plist-get slash-context :start)
                   (plist-get slash-context :end)
                   (niwa-skills--slash-command-argument-table
                    name args arg-index)
                   :exclusive 'no
                   :annotation-function
                   (lambda (candidate)
                     (niwa-skills--slash-command-argument-annotation
                      name candidate args arg-index))))))
        (_
         (pcase (plist-get skill-context :kind)
           ('root
            (niwa-skills-ensure-fresh buffer session)
            (let ((inline-only (plist-get skill-context :inline-only)))
              (list (plist-get skill-context :start)
                    (plist-get skill-context :end)
                    (niwa-skills--skill-completion-table
                     buffer session inline-only)
                    :exclusive 'no
                    :annotation-function
                    (lambda (name)
                      (niwa-skills--skill-annotation
                       name buffer session inline-only))
                    :exit-function
                    (lambda (candidate status)
                      (niwa-skills--skill-completion-exit-function
                       buffer session candidate status)))))))))))

(defun niwa-slash-capf ()
  "Completion-at-point for slash commands, `$' skills, and options.

Root completion is active at the line-start `/name' or `$name' position,
after an optional gptel prompt prefix.  Inline `$name' completion is
active in prose for inline-context skills.  Command option completion
is active for slash commands that declare finite argument choices."
  (when (bound-and-true-p niwa--session)
    (niwa-skills-slash-capf
     (current-buffer) niwa--session niwa-slash-commands)))


;;
;;; Font-lock

(defvar-local niwa-skills--font-lock-origin nil
  "Function returning the position `$skill' fontification starts from.
Nil scans from the beginning of the buffer.  The composer installs its
input start here, so a keystroke scans the draft rather than the whole
transcript in front of it -- the matcher runs on every refontification,
and the transcript prefix made each one cost seconds on a long session.")

(defun niwa-skills--fontify-dollar-keyword (end)
  "Find a known `$skill' mention before END for font-lock."
  (when-let* ((session (or (bound-and-true-p niwa--session)
                          (and (boundp 'niwa--data-buffer)
                               (buffer-live-p niwa--data-buffer)
                               (buffer-local-value 'niwa--session
                                                   niwa--data-buffer))))
              (origin (max (point-min)
                           (or (and niwa-skills--font-lock-origin
                                    (funcall
                                     niwa-skills--font-lock-origin))
                               (point-min))))
              ((<= origin end)))
    (let* ((text (buffer-substring-no-properties origin end))
           (scan (max 0 (- (point) origin)))
           (token
            (cl-find-if
             (lambda (candidate) (>= (plist-get candidate :start) scan))
             (niwa-skills-input-scan-tokens
              text
              (lambda (name _start _end)
                (niwa-session-get-skill session name))
              t))))
      (when token
        (let ((start (+ origin (plist-get token :start)))
              (token-end (+ origin (plist-get token :end))))
          (set-match-data (list start token-end))
          (goto-char token-end)
          t)))))

(defun niwa-skills-install-font-lock (&optional origin-function)
  "Install font-lock support for known `$skill' mentions.
ORIGIN-FUNCTION, when non-nil, returns the position fontification
starts from; mentions before it are left alone."
  (setq-local niwa-skills--font-lock-origin origin-function)
  (font-lock-add-keywords
   nil
   '((niwa-skills--fontify-dollar-keyword
      0 font-lock-keyword-face prepend))
   t))


;;
;;; Installation

;;;###autoload
(defun niwa-skills-install-slash-commands ()
  "Install the slash-command advice on `gptel-send'.

`:around' so the advice can wrap the call in `unwind-protect' for
pending-stash cleanup."
  (advice-add 'gptel-send :around
              #'niwa-skills--gptel-send-advice))

(defun niwa-skills-uninstall-slash-commands ()
  "Remove the slash-command advice from `gptel-send'."
  (advice-remove 'gptel-send
                 #'niwa-skills--gptel-send-advice))

(provide 'niwa-skills-ui)
;;; niwa-skills-ui.el ends here
