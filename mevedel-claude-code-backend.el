;;; mevedel-claude-code-backend.el --- Claude provider identity -*- lexical-binding: t -*-

;;; Commentary:
;; Backend type, model metadata, installation settings and dispatch without
;; the Claude runtime.  Loaded with mevedel, so maintenance and readiness can
;; read the settings for users who never start Claude.

;;; Code:

(require 'mevedel-engine)
(require 'mevedel-models)

(autoload 'mevedel-claude-code-request-text "mevedel-claude-code")
(autoload 'mevedel-claude-code-request-workload "mevedel-claude-code")
(autoload 'mevedel-claude-code-setup "mevedel-claude-code" nil t)
(autoload 'mevedel-claude-code-install-adapter "mevedel-claude-code" nil t)

(defconst mevedel-claude-code--aliases
  '(("sonnet" low medium high xhigh max)
    ("opus" low medium high xhigh max)
    ("fable" low medium high xhigh max)
    ("haiku" low medium high xhigh max))
  "Documented alias effort choices before ACP reports live capabilities.
See https://code.claude.com/docs/en/model-config.")

(defcustom mevedel-claude-code-executable "claude"
  "Installed, unmodified Claude Code executable."
  :type 'string :group 'mevedel)

(defcustom mevedel-claude-code-adapter-executable nil
  "ACP adapter executable, or nil for the managed installation and PATH."
  :type '(choice (const :tag "Find installed adapter" nil) file)
  :group 'mevedel)

(defcustom mevedel-claude-code-directory
  (file-name-concat user-emacs-directory "mevedel" "claude-code")
  "Local adapter installation and neutral conversation working directory.
Keep this location stable to resume the installed CLI's retained histories."
  :type 'directory :group 'mevedel)

(defconst mevedel-claude-code--cli-version "2.1.293"
  "Minimum supported Claude Code CLI release.")

(defconst mevedel-claude-code--adapter-version "0.86.0"
  "Minimum supported Claude ACP adapter release.")

(defun mevedel-claude-code--version (output minimum)
  "Return the version reported in OUTPUT when it is at least MINIMUM."
  (and (string-match "[0-9]+\\.[0-9]+\\.[0-9]+" output)
       (version<= minimum (match-string 0 output))
       (match-string 0 output)))

(cl-defstruct (mevedel-claude-code-backend
               (:include gptel-backend)
               (:constructor mevedel-claude-code--make-backend)))

(cl-defmethod gptel--request-data ((_backend mevedel-claude-code-backend) _prompts)
  "Refuse HTTP dispatch for a subscription-backed conversation."
  (user-error "Claude Code requires mevedel's external conversation engine"))

(cl-defmethod mevedel-engine-external-p ((_backend mevedel-claude-code-backend)) t)

(cl-defmethod mevedel-engine-request-text
  ((_backend mevedel-claude-code-backend) prompt system callback &optional stream _context)
  (mevedel-claude-code-request-text prompt system callback stream))

(defun mevedel-claude-code--model (name &optional existing)
  "Return a backend-owned model named NAME, reusing EXISTING when possible.
Uninterned symbols keep native capabilities separate from API model metadata.
Known aliases start with documented effort choices, refreshed on connection.
Other model IDs accept configured symbols pending capability discovery."
  (or (cl-find name existing :key #'gptel--model-name :test #'equal)
      (car (gptel--process-models
            (list (list (make-symbol name) :description "Claude Code subscription"
                        :capabilities '(tool-use media)
                        :mime-types '("image/jpeg" "image/png" "image/gif" "image/webp")
                        :reasoning-effort
                        (if-let* ((alias (assoc name mevedel-claude-code--aliases)))
                            (and (cdr alias) (cons 'member (cdr alias)))
                          'symbol)))))))

(cl-defmethod mevedel-model--find-model ((_backend mevedel-claude-code-backend) model-name)
  ;; Accept persisted or configured IDs without growing
  ;; the catalog.  Each session validates against its discovered catalog
  ;; before dispatch and never chooses a fallback.
  (or (cl-call-next-method) (mevedel-claude-code--model model-name)))

(defun mevedel-claude-code-register ()
  "Register Claude Code in the ordinary provider and workload selection."
  (if-let* ((backend (alist-get "Claude Code" gptel--known-backends nil nil #'equal))
            ((mevedel-claude-code-backend-p backend)))
      backend
    (setf (alist-get "Claude Code" gptel--known-backends nil nil #'equal)
          (mevedel-claude-code--make-backend
           :name "Claude Code" :stream t
           :models (mapcar #'mevedel-claude-code--model (mapcar #'car mevedel-claude-code--aliases))))))

(autoload 'mevedel-claude-code-send "mevedel-claude-code-session")
(autoload 'mevedel-claude-code-release-history "mevedel-claude-code-session")
(autoload 'mevedel-claude-code-recover-history "mevedel-claude-code-session" nil t)


(provide 'mevedel-claude-code-backend)
;;; mevedel-claude-code-backend.el ends here
