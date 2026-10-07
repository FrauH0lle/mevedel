;;; mevedel-collaboration-recovery.el --- Owner recovery actions -*- lexical-binding: t -*-
;;; Commentary:
;; Closed browser actions reuse host model, preset, login and history APIs.
;; Login challenges are ephemeral and sent only to authenticated owner peers.
;;; Code:

(require 'mevedel-collaboration-owner)
(require 'mevedel-readiness)
(require 'mevedel-presets)
(require 'mevedel-claude-code-maintenance)
(require 'mevedel-pending-inputs)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p "mevedel-agent-control" (session))

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--publish "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish-queue "mevedel-collaboration" (room))
(defvar mevedel-collaboration--rooms)

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-turn'
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))

(defun mevedel-collaboration-recovery-send (room peer)
  "Send owner PEER changed ROOM recovery choices and selected login state."
  (when-let* ((guest (mevedel-collaboration--owner room peer))
              (buffer (mevedel-collaboration--room-data-buffer room)))
    (unless (plist-member room :recovery-runtime)
      (plist-put room :recovery-runtime (mevedel-claude-code-maintenance-state)))
    (with-current-buffer buffer
      (let* ((provider (or (plist-get guest :recovery-backend) gptel-backend))
             (session (plist-get room :session))
             (frame
              (list
               :t "recovery"
               :models (vconcat (mapcar #'car (mevedel-model-candidates)))
               :presets (vconcat (mapcar (lambda (row) (symbol-name (car row)))
                                        mevedel-preset--registry))
               :model (mevedel-model-current-label buffer)
               :provider (and provider (gptel-backend-name provider))
               :providers (vconcat (mapcar #'car gptel--known-backends))
               :auth (condition-case nil (mevedel-auth-state provider) (user-error nil))
               :steering
               (vconcat
                (mapcar (lambda (entry)
                          (list :id (plist-get entry :id) :text (plist-get entry :input)))
                        (cl-remove-if-not
                         (lambda (entry) (eq 'failed-turn (plist-get entry :state)))
                         (append (mevedel-session-pending-steering session)
                                 (mevedel-session-pending-follow-ups session)))))
               :histories (vconcat (mapcar #'car (mevedel-session-external-conversations session)))
               :runtime (let ((state (plist-get room :recovery-runtime)))
                          (list :status (plist-get state :status)
                                :message (plist-get state :message))))))
        (unless (equal frame (plist-get guest :recovery-state))
          (when (mevedel-collaboration--transport-send
                 (plist-get room :transport) peer frame)
            (plist-put guest :recovery-state frame)))))))

(defun mevedel-collaboration-recovery-auth-changed (backend _state)
  "Publish BACKEND's login state to owners viewing its credential store."
  (when (boundp 'mevedel-collaboration--rooms)
    (maphash
     (lambda (_buffer room)
       (when-let* ((buffer (mevedel-collaboration--room-data-buffer room)))
         (maphash
          (lambda (peer guest)
            (let ((selected (or (plist-get guest :recovery-backend)
                                (buffer-local-value 'gptel-backend buffer))))
              (when (or (null backend)
                        (condition-case nil
                            (equal (mevedel-auth--key backend) (mevedel-auth--key selected))
                          (user-error nil)))
                (mevedel-collaboration-recovery-send room peer))))
          (plist-get room :guests))))
     mevedel-collaboration--rooms)))

(add-hook 'mevedel-auth-changed-hook #'mevedel-collaboration-recovery-auth-changed)

(defun mevedel-collaboration-recovery-runtime-changed (state)
  "Publish current installation STATE to live owners without reading storage."
  (when (boundp 'mevedel-collaboration--rooms)
    (maphash (lambda (_buffer room) (plist-put room :recovery-runtime state))
             mevedel-collaboration--rooms))
  (mevedel-collaboration-recovery-auth-changed nil nil))
(add-hook 'mevedel-claude-code-maintenance-changed-hook #'mevedel-collaboration-recovery-runtime-changed)

(defun mevedel-collaboration-recovery-handle (room peer frame)
  "Apply the closed recovery action in FRAME for an authenticated ROOM owner."
  (unless (mevedel-collaboration--owner room peer)
    (user-error "An owner link is required for recovery"))
  (let* ((buffer (mevedel-collaboration--room-data-buffer room))
         (session (plist-get room :session))
         (action (plist-get frame :action))
         (value (plist-get frame :value)))
    (unless (and session (buffer-live-p buffer)) (user-error "Session is unavailable"))
    (with-current-buffer buffer
      (let ((backend (if (plist-get frame :provider)
                         (gptel-get-backend (plist-get frame :provider)) gptel-backend)))
        (when (and (member action '("login" "login-code" "cancel-login")) (not backend))
          (user-error "Provider is unavailable"))
        (unless (member action '("status" "login" "login-code" "cancel-login" "update"))
          (when (or (mevedel-turn-busy-p buffer) (mevedel-agent-control-active-turn-p session))
            (user-error "Wait for or interrupt running turns before recovering the session"))
          (mevedel-session-artifacts-assert-new-mutation-authority session))
        (when (and (plist-get frame :provider)
                   (member action '("status" "login" "login-code" "cancel-login")))
          (plist-put (mevedel-collaboration--owner room peer) :recovery-backend backend))
        (pcase action
          ("status" nil)
          ("model"
           (mevedel-model-set-session-provider session (mevedel-model-resolve-provider value) buffer)
           (mevedel-recovery-clear session "model")
           (mevedel-recovery-clear session "request")
           (mevedel-recovery-clear session "authentication")
           (mevedel-recovery-clear session "input"))
          ("preset"
           (let ((name (and (stringp value) (intern-soft value)))
                 (mode (mevedel-session-permission-mode session))
                 (sandbox (mevedel-session-sandbox-mode session))
                 (mevedel-permission-mode mevedel-permission-mode)
                 (mevedel-sandbox-mode mevedel-sandbox-mode))
             (unless (assq name mevedel-preset--registry) (user-error "Preset is unavailable"))
             (unwind-protect (mevedel-preset-apply name buffer)
               (setf (mevedel-session-permission-mode session) mode
                     (mevedel-session-sandbox-mode session) sandbox))
             (mevedel-recovery-clear session "preset")
             (mevedel-recovery-clear session "input")))
          ("history"
           (mevedel-claude-code-recover-history (or value "root"))
           (mevedel-recovery-clear session "request"))
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
                                                      when (and (not (memq key '(:category :state :blocked :request-id)))
                                                                (plist-member entry key))
                                                      append (list key (copy-tree (plist-get entry key))))))))
               (mevedel-pending-inputs--set-queues
                session 'steering (remq entry (mevedel-session-pending-steering session))
                'follow-up (append (remq entry (mevedel-session-pending-follow-ups session))
                                   (and replacement (list replacement)))))))
          ("login" (mevedel-auth-start backend))
          ("login-code" (mevedel-auth-submit-code backend (plist-get frame :id) value))
          ("cancel-login" (mevedel-auth-cancel backend))
          ("update" (mevedel-claude-code-maintenance-check t))
          ("retry"
           (when (cl-find 'failed-turn (append (mevedel-session-pending-steering session)
                                               (mevedel-session-pending-follow-ups session))
                          :key (lambda (entry) (plist-get entry :state)))
             (user-error "Resolve retained input before resuming the queue"))
           (mevedel-session-set-pending-input-failure-paused session nil)
           (mevedel-recovery-clear session "input")
           (mevedel-recovery-clear session "request")
           (condition-case nil (mevedel-readiness-assert session) (user-error nil)))
          (_ (user-error "Unknown recovery action")))
        (unless (member action '("status" "login" "login-code" "cancel-login" "update"))
          (mevedel-recovery-save session)
          (when (buffer-live-p mevedel--view-buffer)
            (with-current-buffer mevedel--view-buffer
              (mevedel-view--schedule-late-follow-up-drain))))
        (mevedel-collaboration--publish-status room)
        (mevedel-collaboration--publish-queue room)
        (mevedel-collaboration-recovery-send room peer)))))

(provide 'mevedel-collaboration-recovery)
;;; mevedel-collaboration-recovery.el ends here
