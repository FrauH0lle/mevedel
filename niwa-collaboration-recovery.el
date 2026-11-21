;;; niwa-collaboration-recovery.el --- Owner recovery actions -*- lexical-binding: t -*-

;;; Commentary:
;; Closed browser actions reuse host model, preset, login and history APIs.
;; Login challenges are ephemeral and sent only to authenticated owner peers
;; viewing that provider's credential store; each owner keeps the provider it
;; last selected.

;;; Code:

(require 'niwa-collaboration-owner)
(require 'niwa-readiness)
(require 'niwa-presets)
(require 'niwa-claude-code-maintenance)
(require 'niwa-pending-inputs)

;; `gptel'
(defvar gptel-reasoning-effort)

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-turn-p "niwa-agent-control" (session))

;; `niwa-claude-code-history'
(declare-function niwa-claude-code-history-unresumable-p "niwa-claude-code-history" (record))
(autoload 'niwa-claude-code-history-unresumable-p "niwa-claude-code-history")

;; `niwa-collaboration'
(declare-function niwa-collaboration--guest-text "niwa-collaboration" (value))
(declare-function niwa-collaboration--publish-queue "niwa-collaboration" (room))
(declare-function niwa-collaboration--publish-status "niwa-collaboration" (room))
(declare-function niwa-collaboration--room-data-buffer "niwa-collaboration" (room))
(declare-function niwa-collaboration--room-for-buffer "niwa-collaboration" (data-buffer))
(defvar niwa-collaboration--rooms)

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-send "niwa-collaboration-transport" (transport peer frame))

;; `niwa-session-naming'
(declare-function niwa-session-naming-rename "niwa-session-naming" (session buffer name &optional current-p))
(autoload 'niwa-session-naming-rename "niwa-session-naming")

;; `niwa-turn'
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))

(defun niwa-collaboration-recovery--recoverable-histories (session)
  "Return SESSION's retained native history scopes offered for recovery."
  (cl-loop for (scope . record) in (niwa-session-external-conversations session)
           when (and (or (equal scope "root")
                         (assoc scope (niwa-session-agent-registry session)))
                     (niwa-claude-code-history-unresumable-p record))
           collect scope))

;; Frames are sent only when they change; installation state is cached in
;; the room and refreshed by maintenance events instead of read per frame.
(defun niwa-collaboration-recovery-send (room peer)
  "Send owner PEER ROOM's recovery choices and its selected login state.
The selected provider is the one this owner last chose, else the session's."
  (when-let* ((guest (niwa-collaboration--owner room peer))
              (buffer (niwa-collaboration--room-data-buffer room)))
    (with-current-buffer buffer
      (let* ((provider (or (plist-get guest :recovery-backend)
                           (and (niwa-auth-supported-p gptel-backend)
                                gptel-backend)))
             (session (plist-get room :session))
             (frame
              (list
               :t "recovery" :models (vconcat (mapcar #'car (niwa-model-candidates)))
               :presets (vconcat (mapcar (lambda (row) (symbol-name (car row))) niwa-preset--registry))
               ;; The picker selects by its option labels, which are exact
               ;; BACKEND:MODEL pairs; a bare name matches none of them.
               :model (niwa-model-current-provider-label buffer)
               :efforts (vconcat (mapcar #'symbol-name
                                        (niwa-model-supported-efforts gptel-model)))
               :effort (when (bound-and-true-p gptel-reasoning-effort)
                         (format "%s" gptel-reasoning-effort))
               :provider (and provider (gptel-backend-name provider))
               ;; Only providers with a browser login can be signed in to.
               :providers (vconcat (cl-loop for (name . backend) in gptel--known-backends
                                            when (niwa-auth-supported-p backend)
                                            collect name))
               :auth (and provider
                          (condition-case nil (niwa-auth-state provider) (user-error nil)))
               ;; Resuming the queue is offered only while a failure paused it.
               :paused (if (niwa-session-pending-input-failure-paused session) t :json-false)
               :issues (vconcat (niwa-session-recovery-issues session))
               :steering (vconcat (mapcar (lambda (entry)
                                            (list :id (plist-get entry :id) :text (plist-get entry :input)))
                                          (cl-remove-if-not
                                           (lambda (entry) (eq 'failed-turn (plist-get entry :state)))
                                           (append (niwa-session-pending-steering session)
                                                   (niwa-session-pending-follow-ups session)))))
               ;; Only histories `niwa-claude-code-recover-history' accepts,
               ;; and only once Claude cannot or did not resume them.
               :histories (vconcat (niwa-collaboration-recovery--recoverable-histories session))
               ;; Runtime maintenance and its update check exist only for Claude.
               :runtime (when (niwa-claude-code-backend-p gptel-backend)
                          (let ((state (if (plist-member room :recovery-runtime)
                                           (plist-get room :recovery-runtime)
                                         (plist-get (plist-put room :recovery-runtime
                                                               (niwa-claude-code-maintenance-state))
                                                    :recovery-runtime))))
                            (list :status (plist-get state :status)
                                  :message (plist-get state :message)))))))
        (unless (equal frame (plist-get guest :recovery-state))
          (when (niwa-collaboration--transport-send (plist-get room :transport) peer frame)
            (plist-put guest :recovery-state frame)))))))

(defun niwa-collaboration-recovery--send-owners (room)
  "Send every owner peer of ROOM its changed recovery frame."
  (maphash (lambda (peer _guest) (niwa-collaboration-recovery-send room peer))
           (plist-get room :guests)))

(defun niwa-collaboration-recovery-refresh (buffer)
  "Resend BUFFER's room owners their recovery frame after its issues change."
  (when-let* (((boundp 'niwa-collaboration--rooms))
              (room (niwa-collaboration--room-for-buffer buffer)))
    (niwa-collaboration-recovery--send-owners room)))

(defun niwa-collaboration-recovery-auth-changed (backend _state)
  "Publish BACKEND's login state to owners viewing its credential store.
A nil BACKEND refreshes every owner."
  (when (boundp 'niwa-collaboration--rooms)
    (maphash
     (lambda (_buffer room)
       (when-let* ((buffer (niwa-collaboration--room-data-buffer room)))
         (maphash
          (lambda (peer guest)
            (let ((selected (or (plist-get guest :recovery-backend)
                                (buffer-local-value 'gptel-backend buffer))))
              (when (or (null backend)
                        (condition-case nil
                            (equal (niwa-auth--key backend) (niwa-auth--key selected))
                          (user-error nil)))
                (niwa-collaboration-recovery-send room peer))))
          (plist-get room :guests))))
     niwa-collaboration--rooms)))
(add-hook 'niwa-auth-changed-hook #'niwa-collaboration-recovery-auth-changed)

(defun niwa-collaboration-recovery-runtime-changed (state)
  "Publish current installation STATE to live owners without reading storage."
  (when (boundp 'niwa-collaboration--rooms)
    (maphash (lambda (_buffer room) (plist-put room :recovery-runtime state))
             niwa-collaboration--rooms))
  (niwa-collaboration-recovery-auth-changed nil nil))
(add-hook 'niwa-claude-code-maintenance-changed-hook #'niwa-collaboration-recovery-runtime-changed)

(defun niwa-collaboration-recovery--apply-preset (name buffer session)
  "Apply preset NAME in BUFFER without changing SESSION's permission authority.
Permission and sandbox modes keep both SESSION's slots and BUFFER's own
bindings, including the absence of one."
  (let ((saved (mapcar (lambda (symbol)
                         (let ((local (local-variable-p symbol buffer)))
                           (list symbol local (and local (buffer-local-value symbol buffer)))))
                       '(niwa-permission-mode niwa-sandbox-mode)))
        (mode (niwa-session-permission-mode session))
        (sandbox (niwa-session-sandbox-mode session)))
    (unwind-protect (niwa-preset-apply name buffer)
      (setf (niwa-session-permission-mode session) mode
            (niwa-session-sandbox-mode session) sandbox)
      (with-current-buffer buffer
        (pcase-dolist (`(,symbol ,local ,value) saved)
          (if local (set (make-local-variable symbol) value)
            (kill-local-variable symbol)))))))

(defun niwa-collaboration-recovery-handle (room peer frame)
  "Apply the closed recovery action in FRAME for an authenticated ROOM owner."
  (unless (niwa-collaboration--owner room peer)
    (user-error "An owner link is required for recovery"))
  (let* ((buffer (niwa-collaboration--room-data-buffer room))
         (session (plist-get room :session))
         (action (plist-get frame :action))
         (value (plist-get frame :value))
         (session-action (not (member action '("status" "login" "login-code" "cancel-login" "update" "rename")))))
    (unless (and session (buffer-live-p buffer)) (user-error "Session is unavailable"))
    (with-current-buffer buffer
      (let ((backend (if (plist-get frame :provider)
                         (gptel-get-backend (plist-get frame :provider))
                       gptel-backend)))
        (when session-action
          (when (or (niwa-turn-busy-p buffer) (niwa-agent-control-active-turn-p session))
            (user-error "Wait for or interrupt running turns before recovering the session"))
          (niwa-session-artifacts-assert-new-mutation-authority session))
        (when (and (plist-get frame :provider)
                   (member action '("status" "login" "login-code" "cancel-login")))
          (unless (and backend (niwa-auth-supported-p backend))
            (user-error "Provider does not support browser sign-in")))
        (pcase action
          ("status" nil)
          ("rename"
           (niwa-session-naming-rename
            session buffer (or (niwa-collaboration--guest-text value)
                               (user-error "Empty session name"))))
          ("model"
           (niwa-model-set-session-provider
            session (or (cdr (assoc value (niwa-model-candidates)))
                        (user-error "Model is unavailable"))
            buffer)
           ;; The previous provider's login no longer gates this session.
           (niwa-recovery-clear session "authentication"))
          ("effort"
           (let ((effort (and (stringp value)
                              (cl-find value (niwa-model-supported-efforts gptel-model)
                                       :key #'symbol-name :test #'equal))))
             ;; An explicit nil selects the default.  Do not intern browser
             ;; strings or mistake an unknown effort for a default request.
             (unless (and (plist-member frame :value) (or (null value) effort))
               (user-error "Reasoning effort is unavailable"))
             (niwa-model-set-session-effort session effort buffer)))
          ("preset"
           (let ((name (and (stringp value) (intern-soft value))))
             (unless (assq name niwa-preset--registry) (user-error "Preset is unavailable"))
             (niwa-collaboration-recovery--apply-preset name buffer session)))
          ("history"
           (unless (member (or value "root")
                           (niwa-collaboration-recovery--recoverable-histories session))
             (user-error "This native history is not awaiting recovery"))
           (niwa-claude-code-recover-history (or value "root")))
          ((or "input-discard" "input-requeue")
           (let ((entry (cl-find value (append (niwa-session-pending-steering session)
                                               (niwa-session-pending-follow-ups session))
                                 :key (lambda (row) (plist-get row :id)))))
             (unless (and entry (eq 'failed-turn (plist-get entry :state)))
               (user-error "Retained input is no longer awaiting review"))
             (niwa-pending-inputs--discard (list entry))
             (let ((replacement (and (equal action "input-requeue")
                                     (append (list :category 'follow-up)
                                             (cl-loop for key in niwa-recovery--input-keys
                                                      when (and (not (memq key '(:category :state :request-id)))
                                                                (plist-member entry key))
                                                      append (list key (copy-tree (plist-get entry key))))))))
               (niwa-pending-inputs--set-queues
                session 'steering (remq entry (niwa-session-pending-steering session))
                'follow-up (append (remq entry (niwa-session-pending-follow-ups session))
                                   (and replacement (list replacement)))))))
          ("login" (niwa-auth-start backend))
          ("login-code" (niwa-auth-submit-code backend (plist-get frame :id) value))
          ("cancel-login" (niwa-auth-cancel backend))
          ("update"
           (unless (niwa-claude-code-backend-p gptel-backend)
             (user-error "Runtime updates apply only to Claude Code sessions"))
           (niwa-claude-code-maintenance-check t))
          ("retry"
           (when (cl-find 'failed-turn (append (niwa-session-pending-steering session)
                                               (niwa-session-pending-follow-ups session))
                          :key (lambda (entry) (plist-get entry :state)))
             (user-error "Resolve retained input before resuming the queue"))
           (niwa-session-set-pending-input-failure-paused session nil)
           ;; A provider known to be failing is checked again; its success
           ;; wakes the queue.
           (condition-case nil (niwa-readiness-assert session) (user-error nil))
           (when (and (niwa-claude-code-backend-p gptel-backend)
                      (eq 'failed (plist-get niwa-readiness--claude :state)))
             (niwa-readiness-check)))
          (_ (user-error "Unknown recovery action")))
        (when (and (plist-get frame :provider)
                   (member action '("status" "login" "login-code" "cancel-login")))
          (plist-put (niwa-collaboration--owner room peer) :recovery-backend backend))
        (when session-action
          (niwa-recovery-save session)
          (when (buffer-live-p niwa--view-buffer)
            (with-current-buffer niwa--view-buffer
              (niwa-pending-inputs-schedule-held-input-delivery))))
        (niwa-collaboration--publish-status room)
        (niwa-collaboration--publish-queue room)
        ;; Settings such as effort can change without changing public status.
        ;; Build each owner's frame separately to preserve its login selection.
        (if session-action
            (niwa-collaboration-recovery--send-owners room)
          (niwa-collaboration-recovery-send room peer))))))

(provide 'niwa-collaboration-recovery)
;;; niwa-collaboration-recovery.el ends here
