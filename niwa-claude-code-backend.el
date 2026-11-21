;;; niwa-claude-code-backend.el --- Claude provider identity -*- lexical-binding: t -*-

;;; Commentary:
;; Backend type, model metadata, installation settings and dispatch without
;; the Claude runtime.  Loaded with niwa, so maintenance and readiness can
;; read the settings for users who never start Claude.

;;; Code:

(require 'niwa-engine)
(require 'niwa-models)

(autoload 'niwa-claude-code-request-text "niwa-claude-code")
(autoload 'niwa-claude-code-request-workload "niwa-claude-code")
(autoload 'niwa-claude-code-unavailable-reason "niwa-claude-code")
(autoload 'niwa-claude-code-setup "niwa-claude-code" nil t)
(autoload 'niwa-claude-code-install-adapter "niwa-claude-code" nil t)

(defconst niwa-claude-code--aliases
  '(("sonnet" low medium high xhigh max)
    ("opus" low medium high xhigh max)
    ("fable" low medium high xhigh max)
    ("haiku" low medium high xhigh max))
  "Documented alias effort choices before ACP reports live capabilities.
See https://code.claude.com/docs/en/model-config.")

(defcustom niwa-claude-code-executable "claude"
  "Installed, unmodified Claude Code executable."
  :type 'string :group 'niwa)

(defcustom niwa-claude-code-login-warning-days 3
  "Days before a Claude Code subscription login expires to warn once.
Nil disables advance warnings, not expired-login diagnostics."
  :type '(choice (const :tag "No advance warning" nil) number)
  :group 'niwa)

(defconst niwa-claude-code--context-windows
  '(("claude-sonnet-5-5" . 1000) ("claude-sonnet-5" . 1000)
    ("claude-opus-5-5" . 1000) ("claude-opus-5" . 1000)
    ("claude-opus-4-8" . 1000) ("claude-opus-4-7" . 1000)
    ("claude-fable-5-1" . 1000) ("claude-fable-5" . 1000)
    ("claude-haiku-5-5" . 1000)
    ("claude-sonnet-4-6" . 200) ("claude-sonnet-4-5" . 200)
    ("claude-opus-4-6" . 200) ("claude-opus-4-5" . 200)
    ("claude-haiku-4-5-20251001" . 200))
  "Documented native model windows in thousands of tokens, without [1m].
See https://code.claude.com/docs/en/model-config#extended-context and
https://platform.claude.com/docs/en/about-claude/models/overview.
Unknown custom IDs deliberately retain the generic model fallback.")

(defcustom niwa-claude-code-adapter-executable nil
  "ACP adapter executable, or nil for the managed installation and PATH."
  :type '(choice (const :tag "Find installed adapter" nil) file)
  :group 'niwa)

(defcustom niwa-claude-code-agent-log-directory "~/.niwa/claude-agent-logs"
  "Local directory for Claude ACP adapter diagnostic logs, or nil to disable.
Set CLAUDE_AGENT_LOGS in the adapter's process environment to this expanded
directory unless that variable is already set in the inherited environment.
An inherited value, including an empty string, always wins.  Nil disables
only niwa's default; it does not remove an inherited value."
  :type '(choice (const :tag "Do not set a default" nil) directory)
  :group 'niwa)

(defcustom niwa-claude-code-directory
  (file-name-concat user-emacs-directory "niwa" "claude-code")
  "Local adapter installation and neutral conversation working directory.
Keep this location stable to resume the installed CLI's retained histories."
  :type 'directory :group 'niwa)

(defconst niwa-claude-code--cli-version "2.1.293"
  "Minimum supported Claude Code CLI release.")

(defconst niwa-claude-code--adapter-version "0.86.0"
  "Minimum supported Claude ACP adapter release.")

(defun niwa-claude-code--version (output minimum)
  "Return the version reported in OUTPUT when it is at least MINIMUM."
  (and (string-match "[0-9]+\\.[0-9]+\\.[0-9]+" output)
       (version<= minimum (match-string 0 output))
       (match-string 0 output)))

(cl-defstruct (niwa-claude-code-backend
               (:include gptel-backend)
               (:constructor niwa-claude-code--make-backend)))

(cl-defmethod gptel--request-data ((_backend niwa-claude-code-backend) _prompts)
  "Refuse HTTP dispatch for a subscription-backed conversation."
  (user-error "Claude Code requires niwa's external conversation engine"))

(cl-defmethod niwa-engine-external-p ((_backend niwa-claude-code-backend)) t)

(cl-defmethod niwa-engine-request-text
  ((_backend niwa-claude-code-backend) prompt system callback &optional stream _context ready)
  (niwa-claude-code-request-text prompt system callback stream ready))

(cl-defmethod niwa-engine-text-unavailable ((_backend niwa-claude-code-backend))
  (niwa-claude-code-unavailable-reason))

(defun niwa-claude-code--context-window (name)
  "Return NAME's native context budget in thousands of tokens, or nil.
Honor explicit extended-context opt-out and alias environment overrides."
  (let* ((alias (assoc (string-remove-suffix "[1m]" name)
                       niwa-claude-code--aliases))
         (override (and alias
                        (getenv (format "ANTHROPIC_DEFAULT_%s_MODEL"
                                        (upcase (car alias))))))
         (override (and override (not (string-empty-p override)) override))
         (resolved (or override name))
         (window
          (cond
           ((or (string-suffix-p "[1m]" name)
                (string-suffix-p "[1m]" resolved)) 1000)
           ((and alias (not override)) 1000)
           (t (cdr (assoc resolved niwa-claude-code--context-windows))))))
    (if (and window (equal (getenv "CLAUDE_CODE_DISABLE_1M_CONTEXT") "1"))
        (min window 200)
      window)))

(defun niwa-claude-code--model (name &optional existing)
  "Return a backend-owned model named NAME, reusing EXISTING when possible.
Uninterned symbols keep native capabilities separate from API model metadata.
Known aliases start with documented effort choices, refreshed on connection.
Other model IDs accept configured symbols pending capability discovery."
  (or (cl-find name existing :key #'gptel--model-name :test #'equal)
      (car (gptel--process-models
            (list (list (make-symbol name) :description "Claude Code subscription"
                        ;; Current native aliases default to 1M.  The CLI's
                        ;; explicit opt-out keeps even these models at 200K.
                        :context-window (niwa-claude-code--context-window name)
                        :capabilities '(tool-use media)
                        :mime-types '("image/jpeg" "image/png" "image/gif" "image/webp")
                        :reasoning-effort
                        (if-let* ((alias (assoc name niwa-claude-code--aliases)))
                            (and (cdr alias) (cons 'member (cdr alias)))
                          'symbol)))))))

(cl-defmethod niwa-model--find-model ((_backend niwa-claude-code-backend) model-name)
  ;; Accept persisted or configured IDs without growing
  ;; the catalog.  Each session validates against its discovered catalog
  ;; before dispatch and never chooses a fallback.
  (or (cl-call-next-method) (niwa-claude-code--model model-name)))

(defun niwa-claude-code-register ()
  "Register Claude Code in the ordinary provider and workload selection."
  (if-let* ((backend (alist-get "Claude Code" gptel--known-backends nil nil #'equal))
            ((niwa-claude-code-backend-p backend)))
      backend
    (setf (alist-get "Claude Code" gptel--known-backends nil nil #'equal)
          (niwa-claude-code--make-backend
           :name "Claude Code" :stream t
           :models (mapcar #'niwa-claude-code--model (mapcar #'car niwa-claude-code--aliases))))))

(autoload 'niwa-claude-code-send "niwa-claude-code-session")
(autoload 'niwa-claude-code-release-history "niwa-claude-code-session")
(autoload 'niwa-claude-code-recover-history "niwa-claude-code-session" nil t)


(provide 'niwa-claude-code-backend)
;;; niwa-claude-code-backend.el ends here
