;;; mevedel-note-settings.el -- Per-note conversation settings -*- lexical-binding: t -*-

;;; Commentary:

;; Before sending, settings belong to the note; afterward the ordinary session
;; owns them.  Model and effort selection reuse the chat's caller-owned picker.

;;; Code:

(require 'mevedel-menu)
(require 'mevedel-notes)
(require 'mevedel-permission-mode)
(require 'mevedel-persistence)
(require 'mevedel-presets)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-persistence)

(defun mevedel-note-settings--note (scope)
  "Resolve SCOPE's note after persistence may have rebuilt its overlay."
  (or (mevedel-note-resolve (plist-get scope :uuid) (plist-get scope :workspace))
      (user-error "Note is no longer available")))

(defun mevedel-note-settings-apply (settings data)
  "Apply present keys in SETTINGS to the session owned by DATA.
Apply the base preset first, then model provider, reasoning effort and
permission mode.  A present nil effort selects the provider default."
  (unless (buffer-live-p data) (user-error "Note conversation is no longer available"))
  (with-current-buffer data
    (when (or buffer-read-only mevedel-session--read-only-mode)
      (user-error "Note conversation is read-only"))
    (mevedel-session-artifacts-assert-mutation-authority mevedel--session data)
    (when (plist-member settings :preset)
      (mevedel-preset-apply (plist-get settings :preset) data))
    (when (plist-member settings :provider)
      (mevedel-model-set-session-provider
       mevedel--session (mevedel-model-resolve-provider (plist-get settings :provider)) data))
    (when (plist-member settings :effort)
      (mevedel-model-set-session-effort mevedel--session (plist-get settings :effort) data))
    (when (plist-member settings :permission-mode)
      (mevedel-permission-mode-transition (plist-get settings :permission-mode)))
    (mevedel-menu--refresh-plan-approval mevedel--session))
  (force-mode-line-update t))

(defun mevedel-note-settings--values (scope)
  "Return current settings owned by SCOPE."
  (if-let* ((data (plist-get scope :data)))
      (with-current-buffer data
        (list :preset (mevedel-session-preset-name mevedel--session)
              :provider (mevedel-model-current-provider-label data)
              :effort (bound-and-true-p gptel-reasoning-effort)
              :permission-mode (mevedel-permission-mode-effective mevedel--session data)))
    (overlay-get (mevedel-note-settings--note scope) 'mevedel-note-settings)))

(defun mevedel-note-settings--update (scope values)
  "Apply setting VALUES to the note owned by SCOPE."
  (let ((note (mevedel-note-settings--note scope))
        (data (plist-get scope :data)))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope (plist-get scope :workspace))
      (unless (overlay-buffer note) (user-error "Note is no longer available"))
      (if data
          (mevedel-note-settings-apply values data)
        (mevedel-note-set-settings note values)))))

(defun mevedel-note-settings--preset ()
  "Choose the base preset for the note in this menu."
  (interactive)
  (let* ((names (mapcar (lambda (entry) (symbol-name (car entry)))
                        mevedel-preset--registry))
         (preset (intern (completing-read "Preset: " names nil t))))
    (mevedel-note-settings--update (transient-scope) (list :preset preset))))

(defun mevedel-note-settings--static-model (name &optional seen)
  "Project literal model settings from NAME without executing preset code.
SEEN records parent names already visited to avoid cycles."
  (unless (member name seen)
    (let ((spec (gptel-get-preset name)) values)
      (dolist (parent (ensure-list (plist-get spec :parents)))
        (setq values (map-merge 'plist values (mevedel-note-settings--static-model parent (cons name seen)))))
      (dolist (key '(:backend :model :reasoning-effort))
        (when (and (plist-member spec key) (not (consp (plist-get spec key))))
          (setq values (plist-put values key (plist-get spec key)))))
      values)))

(defun mevedel-note-settings--model ()
  "Open the chat's model and effort picker for this note."
  (interactive)
  (let* ((scope (transient-scope))
         (settings (mevedel-note-settings--values scope))
         (preview (unless (plist-get scope :data)
                    (mevedel-note-settings--static-model (or (plist-get settings :preset) 'mevedel-implement))))
         (backend (plist-get preview :backend))
         (gptel-backend (or (and (stringp backend) (ignore-errors (gptel-get-backend backend)))
                            (and (gptel-backend-p backend) backend) gptel-backend))
         (gptel-model (or (plist-get preview :model) gptel-model))
         (effort
          (if (plist-member preview :reasoning-effort) (plist-get preview :reasoning-effort)
            (bound-and-true-p gptel-reasoning-effort))))
    (mevedel-menu-open-model-selection
     :title "Note model"
     :provider (or (plist-get settings :provider)
                   (mevedel-model-current-provider-label))
     :effort (if (plist-member settings :effort) (plist-get settings :effort)
               effort)
     :update (lambda (provider effort)
               (mevedel-note-settings--update scope (list :provider provider :effort effort))))))

(defun mevedel-note-settings--permission ()
  "Choose this note's permission mode using the chat's mode vocabulary."
  (interactive)
  (mevedel-note-settings--update
   (transient-scope)
   (list :permission-mode
         (mevedel-permission-mode-normalize
          (completing-read "Permission mode: " '("ask" "edits" "full-auto") nil t)))))

(defun mevedel-note-settings--description ()
  "Describe this note's current settings without opening a conversation."
  (let* ((scope (transient-scope))
         (settings (mevedel-note-settings--values scope)))
    (format "Note %s · %s\nBase: %s · Model: %s · Effort: %s · Permission: %s"
            (overlay-get (mevedel-note-settings--note scope) 'mevedel-id)
            (if (plist-get scope :data) "live session" "before first send")
            (or (plist-get settings :preset) 'mevedel-implement)
            (or (plist-get settings :provider) "preset/default")
            (if (plist-member settings :effort)
                (or (plist-get settings :effort) "default") "preset/default")
            (or (plist-get settings :permission-mode) "preset/default"))))

(transient-define-prefix mevedel-note-settings--menu ()
  "Settings for one note conversation."
  [:description mevedel-note-settings--description
   ("p" "Base preset" mevedel-note-settings--preset :transient t)
   ("m" "Model / effort" mevedel-note-settings--model)
   ("k" "Permission mode" mevedel-note-settings--permission :transient t)
   ("q" "Close" transient-quit-one)])

;;;###autoload
(defun mevedel-note-settings (&optional note)
  "Configure NOTE's conversation, without sending an unsent note.
Default to the note at point."
  (interactive)
  (setq note (or note (mevedel-notes--at-point)))
  (let* ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note)))
         (id (overlay-get note 'mevedel-note-session-id)))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (mevedel-menu--setup
       'mevedel-note-settings--menu
       (list :uuid (overlay-get note 'mevedel-uuid) :workspace workspace
             :data (when id
                     (or (mevedel-session-control-transfer-root-buffer-for-id id)
                         (mevedel-session-persistence-resume-id workspace id)
                         (user-error "Note conversation is unavailable: %s" id))))))))

(provide 'mevedel-note-settings)
;;; mevedel-note-settings.el ends here
