;;; mevedel-readiness.el --- Provider readiness before input commit -*- lexical-binding: t -*-

;;; Commentary:
;; Readiness runs before a prompt leaves the composer, so a provider that
;; cannot run refuses while the input is still retained.  Codex credentials
;; are checked directly.  Claude readiness is learned from root turn startup:
;; unknown readiness never refuses, because the turn's own launch is the
;; probe.  A failed startup or check refuses with its cause and checks again;
;; a configuration, login or runtime change forgets the result.  No model
;; request is retried here.

;;; Code:

(require 'mevedel-auth)
(require 'mevedel-claude-code-backend)
(require 'mevedel-recovery)

;; `gptel'
(declare-function gptel--model-name "gptel" (model))

;; `mevedel-acp'
(declare-function mevedel-acp-close "mevedel-acp" (connection))
(declare-function mevedel-acp-open "mevedel-acp" (launch ready failed))
(autoload 'mevedel-acp-close "mevedel-acp")
(autoload 'mevedel-acp-open "mevedel-acp")

;; `mevedel-claude-code'
(declare-function mevedel-claude-code-launch "mevedel-claude-code"
                  (system mcp model effort &optional resume hook-command))
(autoload 'mevedel-claude-code-launch "mevedel-claude-code")

;; `mevedel-pending-inputs'
(declare-function mevedel-view--schedule-late-follow-up-drain "mevedel-pending-inputs" ())
(defvar mevedel--view-buffer)

(defvar-local mevedel-readiness--claude nil
  "Claude readiness learned in this root buffer.
A plist with :key, :state (`ready', `failed' or `checking'), the failure
:message and an in-flight check's :cancel function.")

(defun mevedel-readiness--key ()
  "Return the Claude configuration identity readiness is learned for."
  (list mevedel-claude-code-executable mevedel-claude-code-adapter-executable
        mevedel-claude-code-directory (getenv "CLAUDE_CONFIG_DIR")
        (gptel--model-name gptel-model)))

(defun mevedel-readiness--resume (session &rest ids)
  "Clear SESSION's resolved readiness issues IDS and wake its retained input."
  (when (apply #'mevedel-recovery-clear session ids)
    (mevedel-recovery-save session)
    (when (and (boundp 'mevedel--view-buffer) (buffer-live-p mevedel--view-buffer))
      (with-current-buffer mevedel--view-buffer
        (mevedel-view--schedule-late-follow-up-drain)))))

(defun mevedel-readiness-record (ready &optional failure)
  "Record the current root buffer's Claude readiness.
READY is non-nil after a startup reached its prompt; otherwise FAILURE is
the startup's cause, shown in the echo area and the recovery issue."
  (let ((session (bound-and-true-p mevedel--session))
        (previous (plist-get mevedel-readiness--claude :state)))
    (mevedel-readiness-stop)
    (setq mevedel-readiness--claude
          (list :key (mevedel-readiness--key) :state (if ready 'ready 'failed)
                :message failure))
    (if ready
        (progn
          (when (memq previous '(failed checking))
            (message "mevedel: Claude is ready; send your retained input again"))
          (when session (mevedel-readiness--resume session "authentication")))
      (message "mevedel: Claude is not ready: %s" failure)
      (when (and session
                 (mevedel-recovery-report
                  session "authentication" (mevedel-recovery-category failure) failure t))
        (mevedel-recovery-save session)))))

(defun mevedel-readiness-record-turn (info outcome message)
  "Learn Claude readiness from a root turn's INFO and terminal OUTCOME.
Reaching the prompt proves startup; an error before it fails with MESSAGE."
  (when (mevedel-claude-code-backend-p (plist-get info :backend))
    (cond
     ((plist-get info :mevedel-acp-prompted) (mevedel-readiness-record t))
     ((eq outcome 'error)
      (mevedel-readiness-record nil (or message "Claude startup failed"))))))

(defun mevedel-readiness-check ()
  "Check Claude readiness for the current root buffer without a prompt.
The check opens and closes one native session.  Its result is recorded like a
turn startup's; a check already in flight is kept."
  (unless (eq 'checking (plist-get mevedel-readiness--claude :state))
    (mevedel-readiness-stop)
    (let ((buffer (current-buffer))
          (operation (list :key (mevedel-readiness--key) :state 'checking)))
      (setq mevedel-readiness--claude operation)
      (add-hook 'kill-buffer-hook #'mevedel-readiness-stop nil t)
      (cl-flet ((settle (failure)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (eq operation mevedel-readiness--claude)
                        (plist-put operation :cancel nil)
                        (mevedel-readiness-record (not failure) failure))))))
        (condition-case err
            (let ((connection
                   (mevedel-acp-open
                    (mevedel-claude-code-launch
                     "Readiness check" [] (gptel--model-name gptel-model) nil)
                    (lambda (checked)
                      (mevedel-acp-close checked)
                      (settle nil))
                    #'settle)))
              (when (eq operation mevedel-readiness--claude)
                (plist-put operation :cancel (lambda () (mevedel-acp-close connection)))))
          (error (settle (error-message-string err))))))))

(defun mevedel-readiness-assert (session &optional backend)
  "Check SESSION's provider BACKEND before committing input.
BACKEND defaults to `gptel-backend'.  A refusal leaves the caller's input
uncommitted; blocking recovery issues still refuse afterwards."
  (let ((backend (or backend gptel-backend)))
    (condition-case err
        (mevedel-auth-assert-ready backend)
      (user-error
       (mevedel-recovery-report session "authentication" 'authentication
                                "Codex login needs renewal" t)
       (signal (car err) (cdr err))))
    (when (mevedel-claude-code-backend-p backend)
      (unless (equal (mevedel-readiness--key) (plist-get mevedel-readiness--claude :key))
        (mevedel-readiness-stop))
      (pcase (plist-get mevedel-readiness--claude :state)
        ('checking (user-error "Checking Claude readiness; your input is retained"))
        ('failed
         (let ((failure (plist-get mevedel-readiness--claude :message)))
           (mevedel-readiness-check)
           (user-error "%s; checking Claude again, your input is retained" failure)))))
    (mevedel-recovery-clear session "authentication")
    (mevedel-recovery-assert-ready session)))

(defun mevedel-readiness-uses-p (backend)
  "Return non-nil when the current buffer's requests depend on BACKEND."
  (or (eq backend gptel-backend)
      (eq backend mevedel-auth--waiting-backend)
      (condition-case nil
          (equal (mevedel-auth--key backend)
                 (mevedel-auth--key (or mevedel-auth--waiting-backend gptel-backend)))
        (user-error nil))))

(defun mevedel-readiness-changed (backend state)
  "Revalidate root sessions using BACKEND after authentication STATE changes."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((session (bound-and-true-p mevedel--session))
                  ((eq buffer (mevedel-session-root-buffer session)))
                  ((mevedel-readiness-uses-p backend)))
        (if (equal "ready" (plist-get state :status))
            (progn
              (setq mevedel-auth--waiting-backend nil)
              (when (mevedel-claude-code-backend-p backend)
                (mevedel-readiness-stop))
              (mevedel-readiness--resume session "authentication"))
          (let ((message (or (plist-get state :message) "Provider login was cancelled")))
            (when (mevedel-claude-code-backend-p backend)
              (mevedel-readiness-stop)
              (setq mevedel-readiness--claude
                    (list :key (mevedel-readiness--key) :state 'failed :message message)))
            (when (mevedel-recovery-report session "authentication"
                                           (or (plist-get state :category) 'authentication)
                                           message t)
              (mevedel-recovery-save session))))))))

(defun mevedel-readiness-runtime-changed (state)
  "Publish Claude runtime STATE to Claude sessions and wake unsent work."
  (let ((ready (equal (plist-get state :status) "ready")))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((session (bound-and-true-p mevedel--session))
                    ((eq buffer (mevedel-session-root-buffer session)))
                    ((mevedel-claude-code-backend-p gptel-backend)))
          (if ready
              (progn (mevedel-readiness-stop)
                     (mevedel-readiness--resume session "runtime" "authentication"))
            (when (mevedel-recovery-report
                   session "runtime" 'dependency
                   (or (plist-get state :message) (mevedel-recovery-message 'dependency))
                   nil)
              (mevedel-recovery-save session)))))))
  nil)

(defun mevedel-readiness-stop ()
  "Forget this buffer's Claude readiness and cancel an in-flight check."
  (let ((cancel (plist-get mevedel-readiness--claude :cancel)))
    (setq mevedel-readiness--claude nil)
    (remove-hook 'kill-buffer-hook #'mevedel-readiness-stop t)
    (when cancel (funcall cancel))))

(provide 'mevedel-readiness)
;;; mevedel-readiness.el ends here
