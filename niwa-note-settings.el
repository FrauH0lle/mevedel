;;; niwa-note-settings.el -- Per-note conversation settings -*- lexical-binding: t -*-

;;; Commentary:

;; Before sending, settings belong to the note; afterward the ordinary session
;; owns them.  Model and effort selection reuse the chat's caller-owned picker.

;;; Code:

(require 'niwa-menu)
(require 'niwa-notes)
(require 'niwa-permission-mode)
(require 'niwa-persistence)
(require 'niwa-presets)
(require 'niwa-session-control-transfer)
(require 'niwa-session-persistence)

(defun niwa-note-settings--note (scope)
  "Resolve SCOPE's note after persistence may have rebuilt its overlay."
  (or (niwa-note-resolve (plist-get scope :uuid) (plist-get scope :workspace))
      (user-error "Note is no longer available")))

(defun niwa-note-settings-apply (settings data)
  "Apply present keys in SETTINGS to the session owned by DATA.
Apply the base preset first, then model provider, reasoning effort and
permission mode.  A present nil effort selects the provider default."
  (unless (buffer-live-p data) (user-error "Note conversation is no longer available"))
  (with-current-buffer data
    (when (or buffer-read-only niwa-session--read-only-mode)
      (user-error "Note conversation is read-only"))
    (niwa-session-artifacts-assert-mutation-authority niwa--session data)
    (when (plist-member settings :preset)
      (niwa-preset-apply (plist-get settings :preset) data))
    (when (plist-member settings :provider)
      (niwa-model-set-session-provider
       niwa--session (niwa-model-resolve-provider (plist-get settings :provider)) data))
    (when (plist-member settings :effort)
      (niwa-model-set-session-effort niwa--session (plist-get settings :effort) data))
    (when (plist-member settings :permission-mode)
      (niwa-permission-mode-transition (plist-get settings :permission-mode)))
    (niwa-menu--refresh-plan-approval niwa--session))
  (force-mode-line-update t))

(defun niwa-note-settings--values (scope)
  "Return current settings owned by SCOPE."
  (if-let* ((data (plist-get scope :data)))
      (with-current-buffer data
        (list :preset (niwa-session-preset-name niwa--session)
              :provider (niwa-model-current-provider-label data)
              :effort (bound-and-true-p gptel-reasoning-effort)
              :permission-mode (niwa-permission-mode-effective niwa--session data)))
    (overlay-get (niwa-note-settings--note scope) 'niwa-note-settings)))

(defun niwa-note-settings--update (scope values)
  "Apply setting VALUES to the note owned by SCOPE."
  (let ((note (niwa-note-settings--note scope))
        (data (plist-get scope :data)))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope (plist-get scope :workspace))
      (unless (overlay-buffer note) (user-error "Note is no longer available"))
      (if data
          (niwa-note-settings-apply values data)
        (niwa-note-set-settings note values)))))

(defun niwa-note-settings--preset ()
  "Choose the base preset for the note in this menu."
  (interactive)
  (let* ((names (mapcar (lambda (entry) (symbol-name (car entry)))
                        niwa-preset--registry))
         (preset (intern (completing-read "Preset: " names nil t))))
    (niwa-note-settings--update (transient-scope) (list :preset preset))))

(defun niwa-note-settings--static-model (name &optional seen)
  "Project literal model settings from NAME without executing preset code.
SEEN records parent names already visited to avoid cycles."
  (unless (member name seen)
    (let ((spec (gptel-get-preset name)) values)
      (dolist (parent (ensure-list (plist-get spec :parents)))
        (setq values (map-merge 'plist values (niwa-note-settings--static-model parent (cons name seen)))))
      (dolist (key '(:backend :model :reasoning-effort))
        (when (and (plist-member spec key) (not (consp (plist-get spec key))))
          (setq values (plist-put values key (plist-get spec key)))))
      values)))

(defun niwa-note-settings--model ()
  "Open the chat's model and effort picker for this note."
  (interactive)
  (let* ((scope (transient-scope))
         (settings (niwa-note-settings--values scope))
         (preview (unless (plist-get scope :data)
                    (niwa-note-settings--static-model (or (plist-get settings :preset) 'niwa-implement))))
         (backend (plist-get preview :backend))
         (gptel-backend (or (and (stringp backend) (ignore-errors (gptel-get-backend backend)))
                            (and (gptel-backend-p backend) backend) gptel-backend))
         (gptel-model (or (plist-get preview :model) gptel-model))
         (effort
          (if (plist-member preview :reasoning-effort) (plist-get preview :reasoning-effort)
            (bound-and-true-p gptel-reasoning-effort))))
    (niwa-menu-open-model-selection
     :title "Note model"
     :provider (or (plist-get settings :provider)
                   (niwa-model-current-provider-label))
     :effort (if (plist-member settings :effort) (plist-get settings :effort)
               effort)
     :update (lambda (provider effort)
               (niwa-note-settings--update scope (list :provider provider :effort effort))))))

(defun niwa-note-settings--permission ()
  "Choose this note's permission mode using the chat's mode vocabulary."
  (interactive)
  (niwa-note-settings--update
   (transient-scope)
   (list :permission-mode
         (niwa-permission-mode-normalize
          (completing-read "Permission mode: " '("ask" "edits" "full-auto") nil t)))))

(defun niwa-note-settings--description ()
  "Describe this note's current settings without opening a conversation."
  (let* ((scope (transient-scope))
         (settings (niwa-note-settings--values scope)))
    (format "Note %s · %s\nBase: %s · Model: %s · Effort: %s · Permission: %s"
            (overlay-get (niwa-note-settings--note scope) 'niwa-id)
            (if (plist-get scope :data) "live session" "before first send")
            (or (plist-get settings :preset) 'niwa-implement)
            (or (plist-get settings :provider) "preset/default")
            (if (plist-member settings :effort)
                (or (plist-get settings :effort) "default") "preset/default")
            (or (plist-get settings :permission-mode) "preset/default"))))

(transient-define-prefix niwa-note-settings--menu ()
  "Settings for one note conversation."
  [:description niwa-note-settings--description
   ("p" "Base preset" niwa-note-settings--preset :transient t)
   ("m" "Model / effort" niwa-note-settings--model)
   ("k" "Permission mode" niwa-note-settings--permission :transient t)
   ("q" "Close" transient-quit-one)])

;;;###autoload
(defun niwa-note-settings (&optional note)
  "Configure NOTE's conversation, without sending an unsent note.
Default to the note at point."
  (interactive)
  (setq note (or note (niwa-notes--at-point)))
  (let* ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note)))
         (id (overlay-get note 'niwa-note-session-id)))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (niwa-menu--setup
       'niwa-note-settings--menu
       (list :uuid (overlay-get note 'niwa-uuid) :workspace workspace
             :data (when id
                     (or (niwa-session-control-transfer-root-buffer-for-id id)
                         (niwa-session-persistence-resume-id workspace id)
                         (user-error "Note conversation is unavailable: %s" id))))))))

(provide 'niwa-note-settings)
;;; niwa-note-settings.el ends here
