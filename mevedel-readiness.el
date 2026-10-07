;;; mevedel-readiness.el --- Provider readiness before input commit -*- lexical-binding: t -*-
;;; Commentary:
;; Readiness runs before a prompt leaves the retained queue.  The same checks
;; power local sends and browser repair; no model request is retried here.
;;; Code:
(require 'mevedel-auth)
(require 'mevedel-models)
(require 'mevedel-claude-code-backend)

(autoload 'mevedel-acp-open "mevedel-acp")
(autoload 'mevedel-acp-close "mevedel-acp")
(autoload 'mevedel-claude-code-launch "mevedel-claude-code")
(defvar mevedel-claude-code-executable)
(defvar mevedel-claude-code-directory)

;; `mevedel-pending-inputs'
(declare-function mevedel-view--schedule-late-follow-up-drain "mevedel-pending-inputs" ())
;; `mevedel-view'
(declare-function mevedel-view--render-status "mevedel-view" (&optional data-buf))
(defvar mevedel--view-buffer)

(defvar-local mevedel-readiness--claude nil
  "Claude preflight state, with :time, :state and :cancel.")

(defun mevedel-readiness-changed (backend state)
  "Revalidate sessions using BACKEND after safe authentication STATE changes."
  (dolist (buffer (if (plist-get state :checked)
                      (list (plist-get state :checked))
                    (buffer-list)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(when-let* ((session (bound-and-true-p mevedel--session))
                    ((eq buffer (mevedel-session-root-buffer session)))
                    ((if (plist-get state :checked)
			 (eq buffer (plist-get state :checked))
                       (or (eq backend gptel-backend)
                           (eq backend mevedel-auth--waiting-backend)
                           (condition-case nil
                               (equal (mevedel-auth--key backend)
                                      (mevedel-auth--key (or mevedel-auth--waiting-backend gptel-backend)))
                             (user-error nil))))))
          (if (equal "ready" (plist-get state :status))
              (progn
		(setq mevedel-auth--waiting-backend nil)
		(mevedel-recovery-clear session "authentication")
		(mevedel-recovery-clear session "input")
		(when (equal "authentication"
                             (plist-get (cl-find "request" (mevedel-session-recovery-issues session)
						 :key (lambda (row) (plist-get row :id)) :test #'equal) :category))
                  (mevedel-recovery-clear session "request"))
		(when (and (mevedel-claude-code-backend-p backend)
                           (not (plist-get state :checked)))
                  (mevedel-readiness-stop))
		(when (and (boundp 'mevedel--view-buffer) (buffer-live-p mevedel--view-buffer))
                  (with-current-buffer mevedel--view-buffer
                    (mevedel-view--render-status buffer)
                    (mevedel-view--schedule-late-follow-up-drain))))
            (mevedel-recovery-report session "authentication" (or (plist-get state :category) 'authentication)
                                     (or (plist-get state :message) "Provider login was cancelled") t))
          (mevedel-recovery-save session)
          (when (fboundp 'mevedel-collaboration-notify-request-changed)
            (mevedel-collaboration-notify-request-changed buffer)))))))

(defun mevedel-readiness-runtime-changed (state)
  "Publish safe installation STATE and wake previously unsent work.
An installed runtime does not prove that the provider is authenticated."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((session (bound-and-true-p mevedel--session))
                  ((eq buffer (mevedel-session-root-buffer session)))
                  ((mevedel-claude-code-backend-p gptel-backend)))
        (if (equal (plist-get state :status) "ready")
            (progn
              (mevedel-recovery-clear session "runtime")
              (mevedel-readiness-stop)
              ;; Dependency failures are rechecked before the input is committed.
              (dolist (issue (mevedel-session-recovery-issues session))
                (when (equal "dependency" (plist-get issue :category))
                  (mevedel-recovery-clear session (plist-get issue :id))
                  (mevedel-recovery-clear session "input")))
              (when (buffer-live-p mevedel--view-buffer)
                (with-current-buffer mevedel--view-buffer
                  (mevedel-view--render-status buffer)
                  (mevedel-view--schedule-late-follow-up-drain)))
              (when (fboundp 'mevedel-collaboration-notify-request-changed)
                (mevedel-collaboration-notify-request-changed buffer)))
          (mevedel-recovery-report session "runtime" 'dependency
                                   (plist-get state :message) nil))
        (mevedel-recovery-save session))))
  nil)

(defun mevedel-readiness-assert (session)
  "Check SESSION's provider before committing input; defer work through issues."
  (mevedel-model-apply-session-policy session)
  (cond
   ((gptel-openai-oauth-p gptel-backend)
    (unless (mevedel-auth-codex-ready-p gptel-backend)
      (mevedel-recovery-report session "authentication" 'authentication "Codex login needs renewal" t)
      (mevedel-auth-refresh gptel-backend)
      (user-error "Codex login needs renewal; your input is retained")))
   ((mevedel-claude-code-backend-p gptel-backend)
    (require 'mevedel-claude-code)
    (let ((key (list mevedel-claude-code-executable mevedel-claude-code-directory
                     (getenv "CLAUDE_CONFIG_DIR") (gptel--model-name gptel-model))))
      (unless (equal key (plist-get mevedel-readiness--claude :key))
        (mevedel-readiness-stop))
      (unless (and (eq (plist-get mevedel-readiness--claude :state) 'ready)
                   (< (- (float-time) (plist-get mevedel-readiness--claude :time)) 30))
        (unless (eq (plist-get mevedel-readiness--claude :state) 'checking)
          (setq mevedel-readiness--claude (list :key key :state 'checking))
          (let ((backend gptel-backend) (buffer (current-buffer))
                (operation mevedel-readiness--claude))
            (condition-case err
                (let* ((launch (mevedel-claude-code-launch "Readiness check" [] (gptel--model-name gptel-model) nil))
                       (connection
                        (mevedel-acp-open
                         launch
                         (lambda (checked)
                           (mevedel-acp-close checked)
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (when (eq operation mevedel-readiness--claude)
                                 (setq mevedel-readiness--claude
                                       (list :key (append (butlast key) (list (gptel--model-name gptel-model)))
                                             :state 'ready :time (float-time)))
                                 (mevedel-readiness-changed backend (list :status "ready" :checked buffer))))))
                         (lambda (message)
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (when (eq operation mevedel-readiness--claude)
                                 (setq mevedel-readiness--claude (list :key key :state 'failed))
                                 (mevedel-readiness-changed
                                  gptel-backend
                                  (if (not (eq backend gptel-backend)) (list :status "ready" :checked buffer)
                                    (list :status "required" :message message :checked buffer
                                          :category (mevedel-recovery-category message)))))))))))
                  (plist-put operation :cancel (lambda () (mevedel-acp-close connection))))
              (error (setq mevedel-readiness--claude (list :key key :state 'failed))
                     (mevedel-recovery-report session "authentication"
                                              (mevedel-recovery-category (error-message-string err))
                                              (error-message-string err) t)))))
        (user-error "Checking Claude readiness; your input is retained")))))
  (mevedel-recovery-clear session "authentication")
  (mevedel-recovery-assert-ready session))

(defun mevedel-readiness-stop ()
  "Cancel the outstanding Claude readiness check."
  (let ((cancel (plist-get mevedel-readiness--claude :cancel)))
    (setq mevedel-readiness--claude nil)
    (when cancel (funcall cancel))))
(add-hook 'kill-buffer-hook #'mevedel-readiness-stop)

(provide 'mevedel-readiness)
;;; mevedel-readiness.el ends here
