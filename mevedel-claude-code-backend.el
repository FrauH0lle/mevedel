;;; mevedel-claude-code-backend.el --- Claude provider identity -*- lexical-binding: t -*-

;;; Commentary:
;; Backend type, model metadata and dispatch without the Claude runtime.

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
    ("haiku"))
  "Documented alias effort choices before ACP reports live capabilities.
See https://code.claude.com/docs/en/model-config.")

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

(cl-defmethod mevedel-engine-request-workload
  ((_backend mevedel-claude-code-backend) prompt system tools callback before-tool boundary)
  (mevedel-claude-code-request-workload prompt system tools callback before-tool boundary))

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
