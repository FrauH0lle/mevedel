;;; niwa-tool-usage.el --- Subscription quota tool -*- lexical-binding: t -*-

;;; Commentary:
;; SubscriptionUsage gives the model plain quota readings for the session's
;; backend and every other configured Codex OAuth or Claude Code backend.
;; Retrieval is the usage provider's asynchronous path; this module only selects
;; backends and serializes the provider's plain readings as JSON.

;;; Code:

(require 'cl-lib)
(require 'niwa-tool-registry)
(require 'gptel-request)
(require 'json)

(declare-function niwa-subscription-usage-provider-readings
                  "niwa-subscription-usage-provider" (backend callback))
(declare-function gptel-openai-oauth-p "gptel-openai-oauth" (object))
(declare-function niwa-claude-code-backend-p "niwa-claude-code" (backend))

(defun niwa-tool-usage--backends ()
  "Return (BACKEND . CURRENT) for the session and other subscription backends."
  (require 'niwa-subscription-usage-provider)
  (let ((current gptel-backend)
        (result nil))
    (dolist (backend (cons current (mapcar #'cdr gptel--known-backends)))
      (when (and (or (gptel-openai-oauth-p backend)
                     (niwa-claude-code-backend-p backend))
                 (not (assq backend result)))
        (push (cons backend (eq backend current)) result)))
    (nreverse result)))

(defun niwa-tool-usage--entry (backend current readings error)
  "Return the JSON object for BACKEND, CURRENT, its READINGS or ERROR."
  (append
   `((backend . ,(gptel-backend-name backend))
     (current . ,(if current t :false)))
   (if error
       `((error . ,error))
     `((readings
        . ,(vconcat
            (mapcar (lambda (reading)
                      (cl-loop for (key field) in '((quota :quota) (window :window)
                                                    (used_percent :used-percent)
                                                    (resets :resets))
                               collect (cons key (or (plist-get reading field) :null))))
                    readings)))))))

(defun niwa-tool-usage--handle (callback _args)
  "Deliver plain subscription quota readings as JSON to CALLBACK.
Return (:cancel FN); FN cancels every pending provider retrieval."
  (let* ((backends (niwa-tool-usage--backends))
         (entries (make-vector (length backends) nil))
         (pending (length backends))
         (model gptel-model)
         (index 0)
         cancellers done)
    (if (null backends)
        (funcall callback (list :result "No Codex OAuth or Claude Code backend is configured."))
      (pcase-dolist (`(,backend . ,current) backends)
        (let ((slot index)
              (gptel-model (if current model
                             (or (car (gptel-backend-models backend)) model))))
          (push (niwa-subscription-usage-provider-readings
                 backend
                 (lambda (readings error)
                   (unless done
                     (aset entries slot
                           (niwa-tool-usage--entry backend current readings error))
                     (when (zerop (cl-decf pending))
                       (setq done t)
                       (funcall callback (list :result (json-serialize entries)))))))
                cancellers))
        (cl-incf index)))
    (list :cancel (lambda ()
                    (unless done
                      (setq done t)
                      (mapc #'funcall cancellers))))))

;;;###autoload
(defun niwa-tool-usage--register ()
  "Register the SubscriptionUsage tool."
  (niwa-define-tool
   :name "SubscriptionUsage"
   :description "Read account-wide subscription quota windows (used percentage and reset time) for the session's backend and other configured Codex OAuth or Claude Code backends."
   :summary "Read subscription quota percentages and reset times as plain data."
   :prompt-file "prompts/tools/subscriptionusage.md"
   :handler #'niwa-tool-usage--handle
   :args ()
   :async-p t
   :groups (util)
   :read-only-p t
   :max-result-size 10000))

(provide 'niwa-tool-usage)
;;; niwa-tool-usage.el ends here
