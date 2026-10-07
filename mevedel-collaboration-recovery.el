;;; mevedel-collaboration-recovery.el --- Owner recovery actions -*- lexical-binding: t -*-

;;; Commentary:
;; Closed browser actions reuse host model, preset, login and history APIs.
;; Login challenges are ephemeral and sent only to authenticated owner peers
;; of rooms whose session uses that provider or whose owner started the login.

;;; Code:

(require 'mevedel-collaboration-owner)
(require 'mevedel-readiness)
(require 'mevedel-presets)
(require 'mevedel-claude-code-maintenance)
(require 'mevedel-pending-inputs)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p "mevedel-agent-control" (session))

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--publish-queue "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish-status "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-data-buffer "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-buffer "mevedel-collaboration" (data-buffer))
(defvar mevedel-collaboration--rooms)

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-turn'
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))

(defun mevedel-collaboration-recovery-send (room peer &optional backend)
  "Send owner PEER current ROOM recovery choices and BACKEND login state."
  (when (mevedel-collaboration--owner room peer)
    (when-let* ((buffer (mevedel-collaboration--room-data-buffer room)))
      (with-current-buffer buffer
        (let* ((session (plist-get room :session))
               (backend (or backend gptel-backend)))
          (mevedel-collaboration--transport-send
           (plist-get room :transport) peer
           (list :t "recovery" :models (vconcat (mapcar #'car (mevedel-model-candidates)))
                 :presets (vconcat (mapcar (lambda (row) (symbol-name (car row))) mevedel-preset--registry))
                 :model (mevedel-model-current-label buffer)
                 :provider (and backend (gptel-backend-name backend))
                 :providers (vconcat (mapcar #'car gptel--known-backends))
                 :auth (condition-case nil (mevedel-auth-state backend) (user-error nil))
                 :issues (vconcat (mevedel-session-recovery-issues session))
                 :steering (vconcat (mapcar (lambda (entry)
                                              (list :id (plist-get entry :id) :text (plist-get entry :input)))
                                            (cl-remove-if-not
                                             (lambda (entry) (eq 'failed-turn (plist-get entry :state)))
                                             (append (mevedel-session-pending-steering session)
                                                     (mevedel-session-pending-follow-ups session)))))
                 ;; Only histories `mevedel-claude-code-recover-history' accepts.
                 :histories (vconcat (cl-loop for (scope . _) in (mevedel-session-external-conversations session)
                                              when (or (equal scope "root")
                                                       (assoc scope (mevedel-session-agent-registry session)))
                                              collect scope))
                 ;; Runtime maintenance and its update check exist only for Claude.
                 :runtime (when (mevedel-claude-code-backend-p gptel-backend)
                            (let ((state (mevedel-claude-code-maintenance-state)))
                              (list :status (plist-get state :status)
                                    :message (plist-get state :message)))))))))))

(defun mevedel-collaboration-recovery--send-owners (room &optional backend)
  "Send every owner peer of ROOM its recovery frame with BACKEND login state."
  (maphash (lambda (peer _guest) (mevedel-collaboration-recovery-send room peer backend))
           (plist-get room :guests)))

(defun mevedel-collaboration-recovery-refresh (buffer)
  "Resend BUFFER's room owners their recovery frame after its issues change."
  (when-let* (((boundp 'mevedel-collaboration--rooms))
              (room (mevedel-collaboration--room-for-buffer buffer)))
    (mevedel-collaboration-recovery--send-owners room (plist-get room :recovery-login))))

(defun mevedel-collaboration-recovery-auth-changed (backend _state)
  "Publish BACKEND's login state to owners of rooms that use it.
A room uses BACKEND when its session does or its owner started that login."
  (when (boundp 'mevedel-collaboration--rooms)
    (maphash
     (lambda (buffer room)
       (when (and (buffer-live-p buffer)
                  (or (eq backend (plist-get room :recovery-login))
                      (with-current-buffer buffer
                        (mevedel-readiness-uses-p backend))))
         (mevedel-collaboration-recovery--send-owners room backend)))
     mevedel-collaboration--rooms)))
(add-hook 'mevedel-auth-changed-hook #'mevedel-collaboration-recovery-auth-changed)

(defun mevedel-collaboration-recovery-runtime-changed (_state)
  "Publish current Claude installation status to owners of Claude rooms."
  (when (boundp 'mevedel-collaboration--rooms)
    (maphash
     (lambda (buffer room)
       (when (and (buffer-live-p buffer)
                  (mevedel-claude-code-backend-p (buffer-local-value 'gptel-backend buffer)))
         (mevedel-collaboration-recovery--send-owners room (plist-get room :recovery-login))))
     mevedel-collaboration--rooms)))
(add-hook 'mevedel-claude-code-maintenance-changed-hook #'mevedel-collaboration-recovery-runtime-changed)

(defun mevedel-collaboration-recovery--apply-preset (name buffer session)
  "Apply preset NAME in BUFFER without changing SESSION's permission authority.
Permission and sandbox modes keep both SESSION's slots and BUFFER's own
bindings, including the absence of one."
  (let ((saved (mapcar (lambda (symbol)
                         (let ((local (local-variable-p symbol buffer)))
                           (list symbol local (and local (buffer-local-value symbol buffer)))))
                       '(mevedel-permission-mode mevedel-sandbox-mode)))
        (mode (mevedel-session-permission-mode session))
        (sandbox (mevedel-session-sandbox-mode session)))
    (unwind-protect (mevedel-preset-apply name buffer)
      (setf (mevedel-session-permission-mode session) mode
            (mevedel-session-sandbox-mode session) sandbox)
      (with-current-buffer buffer
        (pcase-dolist (`(,symbol ,local ,value) saved)
          (if local (set (make-local-variable symbol) value)
            (kill-local-variable symbol)))))))

(defun mevedel-collaboration-recovery-handle (room peer frame)
  "Apply the closed recovery action in FRAME for an authenticated ROOM owner."
  (unless (mevedel-collaboration--owner room peer)
    (user-error "An owner link is required for recovery"))
  (let* ((buffer (mevedel-collaboration--room-data-buffer room))
         (session (plist-get room :session))
         (action (plist-get frame :action))
         (value (plist-get frame :value))
         (session-action (not (member action '("status" "login" "login-code" "cancel-login" "update")))))
    (unless (and session (buffer-live-p buffer)) (user-error "Session is unavailable"))
    (with-current-buffer buffer
      (let ((backend (if (plist-get frame :provider)
                         (gptel-get-backend (plist-get frame :provider))
                       gptel-backend)))
        (when session-action
          (when (or (mevedel-turn-busy-p buffer) (mevedel-agent-control-active-turn-p session))
            (user-error "Wait for or interrupt running turns before recovering the session"))
          (mevedel-session-artifacts-assert-new-mutation-authority session))
        (pcase action
          ("status" nil)
          ("model"
           (mevedel-model-set-session-provider
            session (or (cdr (assoc value (mevedel-model-candidates)))
                        (user-error "Model is unavailable"))
            buffer)
           ;; The previous provider's login no longer gates this session.
           (mevedel-recovery-clear session "authentication"))
          ("preset"
           (let ((name (and (stringp value) (intern-soft value))))
             (unless (assq name mevedel-preset--registry) (user-error "Preset is unavailable"))
             (mevedel-collaboration-recovery--apply-preset name buffer session)))
          ("history" (mevedel-claude-code-recover-history (or value "root")))
          ((or "input-discard" "input-requeue")
           (let ((entry (cl-find value (append (mevedel-session-pending-steering session)
                                               (mevedel-session-pending-follow-ups session))
                                 :key (lambda (row) (plist-get row :id)))))
             (unless (and entry (eq 'failed-turn (plist-get entry :state)))
               (user-error "Retained input is no longer awaiting review"))
             (mevedel-pending-inputs--discard (list entry))
             (let ((replacement (and (equal action "input-requeue")
                                     (append (list :category 'follow-up)
                                             (cl-loop for key in mevedel-recovery--input-keys
                                                      when (and (not (memq key '(:category :state :request-id)))
                                                                (plist-member entry key))
                                                      append (list key (copy-tree (plist-get entry key))))))))
               (mevedel-pending-inputs--set-queues
                session 'steering (remq entry (mevedel-session-pending-steering session))
                'follow-up (append (remq entry (mevedel-session-pending-follow-ups session))
                                   (and replacement (list replacement)))))))
          ("login"
           (setq room (plist-put room :recovery-login backend))
           (mevedel-auth-start backend))
          ("login-code" (mevedel-auth-submit-code backend (plist-get frame :id) value))
          ("cancel-login" (mevedel-auth-cancel backend))
          ("update"
           (unless (mevedel-claude-code-backend-p gptel-backend)
             (user-error "Runtime updates apply only to Claude Code sessions"))
           (mevedel-claude-code-maintenance-check t))
          ("retry"
           (when (cl-find 'failed-turn (append (mevedel-session-pending-steering session)
                                               (mevedel-session-pending-follow-ups session))
                          :key (lambda (entry) (plist-get entry :state)))
             (user-error "Resolve retained input before resuming the queue"))
           (mevedel-session-set-pending-input-failure-paused session nil)
           ;; A provider known to be failing is checked again; its success
           ;; wakes the queue.
           (condition-case nil (mevedel-readiness-assert session) (user-error nil)))
          (_ (user-error "Unknown recovery action")))
        (when session-action
          (mevedel-recovery-save session)
          (when (buffer-live-p mevedel--view-buffer)
            (with-current-buffer mevedel--view-buffer
              (mevedel-view--schedule-late-follow-up-drain))))
        (mevedel-collaboration--publish-status room)
        (mevedel-collaboration--publish-queue room)
        (mevedel-collaboration-recovery-send room peer backend)))))

(provide 'mevedel-collaboration-recovery)
;;; mevedel-collaboration-recovery.el ends here
