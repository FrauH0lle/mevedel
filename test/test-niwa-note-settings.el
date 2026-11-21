;;; test-niwa-note-settings.el -- Note settings UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive the public settings command and its real transient keys in source files.

;;; Code:

(require 'niwa)
(require 'niwa-chat)
(require 'niwa-journal-process)
(require 'niwa-menu)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-presets)
(require 'niwa-note-settings)
(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-note-settings-test--with-source (&rest body)
  "Run BODY in a real source file with isolated notes and buffers."
  (declare (indent 0))
  `(let* ((root (make-temp-file "niwa-note-settings-" t))
          (file (file-name-concat root "source.el"))
          (niwa-sessions-directory (file-name-concat root "sessions"))
          (niwa-user-dir (file-name-concat root "user"))
          (niwa-journal-enabled nil)
          (workspace (niwa-workspace--create
                      :type 'file :id root :root root :name "settings"))
          (niwa--instruction-states (make-hash-table :test #'equal))
          (niwa-instruction-store--dirty nil)
          (niwa-instruction-store--failed nil)
          (gptel--known-presets (copy-tree gptel--known-presets))
          (niwa-preset--registry (copy-tree niwa-preset--registry))
          (transport-enabled niwa-transport--enabled-p)
          (before (buffer-list))
          (timers (append timer-list timer-idle-list nil))
          source)
     (unwind-protect
         (save-window-excursion
           (make-directory (file-name-concat root ".niwa"))
           (write-region "before\nselected code\nafter\n" nil file nil 'silent)
           (niwa-install)
           (setq source (find-file-noselect file))
           (switch-to-buffer source)
           (setq-local niwa--workspace workspace)
           (goto-char (point-min))
           (forward-line 1)
           ,@body)
       (transient--emergency-exit :test)
       (dolist (buffer (cl-set-difference (buffer-list) before))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (set-buffer-modified-p nil)
             (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
       (dolist (timer (append timer-list timer-idle-list nil))
         (unless (memq timer timers) (cancel-timer timer)))
       (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
       (niwa-uninstall)
       (when transport-enabled (niwa-transport-install))
       (delete-directory root t))))

(niwa-deftest niwa-note-settings-test--with-source
  (:quiet t :doc "cleanup preserves both enabled and disabled transport state")
  (let ((transport-enabled niwa-transport--enabled-p))
    (unwind-protect
        (dolist (enabled '(t nil))
          (if enabled (niwa-transport-install) (niwa-transport-uninstall))
          (niwa-note-settings-test--with-source
            (should niwa-transport--enabled-p))
          (should (eq enabled niwa-transport--enabled-p)))
      (if transport-enabled
          (niwa-transport-install)
        (niwa-transport-uninstall)))))

(niwa-deftest niwa-note-settings
  (:quiet t)
  ,test
  (test)
  :doc "closing settings preserves the source note and every later kill hook's buffer"
  (niwa-note-settings-test--with-source
    (let* ((note (niwa-note-create "Keep this visible"))
           (label (overlay-get note 'niwa-label-overlay))
           (text (overlay-get label 'after-string))
           (position (point))
           killed-buffer
           (observer (lambda () (setq killed-buffer (current-buffer)))))
      (unwind-protect
          (progn
            (add-hook 'kill-buffer-hook observer 95)
            (niwa-note-settings note)
            (let ((popup (get-buffer transient--buffer-name)))
              (should (buffer-live-p popup))
              (execute-kbd-macro (kbd "q"))
              (should-not (buffer-live-p popup))
              (should (eq popup killed-buffer))))
        (remove-hook 'kill-buffer-hook observer))
      (should (eq source (overlay-buffer note)))
      (should (eq source (overlay-buffer label)))
      (should (equal-including-properties text (overlay-get label 'after-string)))
      (should (= position (point)))
      (should (equal "before\nselected code\nafter\n" (buffer-string)))))
  :doc "unsent preset choice persists without creating a conversation"
  (niwa-note-settings-test--with-source
    (let* ((note (niwa-note-create "Explain"))
           (uuid (overlay-get note 'niwa-uuid)))
      (niwa-note-settings note)
      (with-current-buffer transient--buffer-name
        (should (string-search "Base: niwa-implement" (buffer-string))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "niwa-implement")))
        (call-interactively (lookup-key transient--transient-map (kbd "p"))))
      (should (eq 'niwa-implement
                  (plist-get (overlay-get note 'niwa-note-settings) :preset)))
      (should-not (overlay-get note 'niwa-note-session-id))
      (should-not (cl-some (lambda (buffer)
                             (buffer-local-value 'niwa--session buffer))
                           (cl-set-difference (buffer-list) before)))
      (transient--emergency-exit :test)
      (niwa-instruction-store-flush t)
      (niwa--clear-instruction-state workspace)
      (niwa-instruction-store-observe workspace)
      (should (eq 'niwa-implement
                  (plist-get (overlay-get (niwa-note-resolve uuid workspace)
                                          'niwa-note-settings) :preset)))))
  :doc "unsent model and effort choices stay on the note"
  (niwa-note-settings-test--with-source
    (let* ((gptel--known-backends nil)
           (gptel-backend (gptel-make-openai "Settings" :key "test"
                                           :models '(alpha beta)))
           (gptel-model 'alpha)
           (old-effort (get 'beta :reasoning-effort))
           (note (niwa-note-create "Explain"))
           (uuid (overlay-get note 'niwa-uuid)))
      (unwind-protect
          (progn
            (put 'beta :reasoning-effort '(member low high))
            (niwa-note-settings note)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "Settings:beta")))
              (execute-kbd-macro (kbd "m RET e q")))
            (setq note (niwa-note-resolve uuid workspace))
            (should (equal '(:provider "Settings:beta" :effort low)
                           (overlay-get note 'niwa-note-settings)))
            (should-not (overlay-get note 'niwa-note-session-id)))
        (put 'beta :reasoning-effort old-effort))))
  :doc "unsent picker previews inherited static settings without executing preset hooks"
  (niwa-note-settings-test--with-source
    (let* ((gptel--known-backends nil)
           (gptel-backend (gptel-make-openai "Fallback" :key "test" :models '(alpha)))
           (gptel-model 'alpha)
           (backend (gptel-make-openai "Static" :key "test" :models '(alpha beta)))
           (old-effort (get 'beta :reasoning-effort))
           (note (niwa-note-create "Explain"))
           (uuid (overlay-get note 'niwa-uuid)))
      (unwind-protect
          (progn
            (put 'beta :reasoning-effort '(member low high))
            (gptel-make-preset 'settings-first :backend backend :model 'alpha :reasoning-effort 'low)
            (gptel-make-preset 'settings-last :backend "Static" :model 'beta :reasoning-effort 'high)
            (gptel-make-preset 'settings-child :parents '(settings-first settings-last)
                              :model '(:eval (error "Dynamic preview executed"))
                              :post (lambda () (error "Preset hook executed"))
                              :system (lambda () (error "System prompt executed")))
            (overlay-put note 'niwa-note-settings '(:preset settings-child))
            (niwa-note-settings note)
            (execute-kbd-macro (kbd "m"))
            (with-current-buffer transient--buffer-name
              (should (string-search "Static:beta" (buffer-string)))
              (should (string-search "effort high" (buffer-string))))
            (execute-kbd-macro (kbd "e q"))
            (setq note (niwa-note-resolve uuid workspace))
            (should (equal '(:preset settings-child :provider "Static:beta" :effort nil)
                           (overlay-get note 'niwa-note-settings)))
            (should-not (overlay-get note 'niwa-note-session-id)))
        (put 'beta :reasoning-effort old-effort))))
  :doc "unsent permission choice never changes the global default"
  (niwa-note-settings-test--with-source
    (let ((permission (default-toplevel-value 'niwa-permission-mode))
          (note (niwa-note-create "Explain")))
      (niwa-note-settings note)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "full-auto")))
        (call-interactively (lookup-key transient--transient-map (kbd "k"))))
      (should (eq 'full-auto
                  (plist-get (overlay-get note 'niwa-note-settings) :permission-mode)))
      (should (eq permission (default-toplevel-value 'niwa-permission-mode)))
      (should-not (overlay-get note 'niwa-note-session-id))))
  :doc "linked permission choice updates only its live owning session"
  (niwa-note-settings-test--with-source
    (let* ((note (niwa-note-create "Explain"))
           (data (niwa--chat-buffer nil t workspace root))
           (session (buffer-local-value 'niwa--session data))
           (permission (default-toplevel-value 'niwa-permission-mode)))
      (niwa-note-mark-sent (list note) (niwa-session-session-id session))
      (niwa-note-settings note)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "full-auto")))
        (call-interactively (lookup-key transient--transient-map (kbd "k"))))
      (should (eq 'full-auto (niwa-session-permission-mode session)))
      (should (eq permission (default-toplevel-value 'niwa-permission-mode)))
      (should-not (overlay-get note 'niwa-note-settings))))
  :doc "linked preset choice belongs to its session, not the note or another chat"
  (niwa-note-settings-test--with-source
    (let* ((niwa-preset--registry (copy-tree niwa-preset--registry))
           (gptel--known-presets (copy-tree gptel--known-presets))
           (note (niwa-note-create "Explain"))
           (data (niwa--chat-buffer nil t workspace root))
           (session (buffer-local-value 'niwa--session data))
           (other (niwa--chat-buffer nil t workspace root)))
      (niwa-define-preset settings-team :parents (niwa-implement))
      (overlay-put note 'niwa-note-session-id (niwa-session-session-id session))
      (niwa-note-settings note)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "settings-team")))
        (call-interactively (lookup-key transient--transient-map (kbd "p"))))
      (should (eq 'settings-team (niwa-session-preset-name session)))
      (should-not (eq 'settings-team
                      (niwa-session-preset-name (buffer-local-value 'niwa--session other))))
      (should-not (overlay-get note 'niwa-note-settings))))
  :doc "linked model choice updates its session and keeps another chat unchanged"
  (niwa-note-settings-test--with-source
    (let* ((gptel--known-backends nil)
           (backend (gptel-make-openai "Settings" :key "test" :models '(alpha beta)))
           (note (niwa-note-create "Explain"))
           (data (niwa--chat-buffer nil t workspace root))
           (session (buffer-local-value 'niwa--session data))
           (other (niwa--chat-buffer nil t workspace root))
           (other-provider (buffer-local-value 'gptel-model other)))
      (with-current-buffer data
        (niwa-model-set-session-provider session (list :backend backend :model 'alpha)))
      (overlay-put note 'niwa-note-session-id (niwa-session-session-id session))
      (niwa-note-settings note)
      (call-interactively (lookup-key transient--transient-map (kbd "m")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Settings:beta")))
        (call-interactively (lookup-key transient--transient-map (kbd "RET"))))
      (should (equal "Settings:beta" (niwa-session-model-provider session)))
      (should (eq 'beta (buffer-local-value 'gptel-model data)))
      (should (eq other-provider (buffer-local-value 'gptel-model other)))
      (should-not (overlay-get note 'niwa-note-settings))))
  :doc "linked read-only conversation refuses settings mutations"
  (niwa-note-settings-test--with-source
    (let* ((note (niwa-note-create "Explain"))
           (data (niwa--chat-buffer nil t workspace root))
           (session (buffer-local-value 'niwa--session data)))
      (overlay-put note 'niwa-note-session-id (niwa-session-session-id session))
      (with-current-buffer data (setq buffer-read-only t))
      (niwa-note-settings note)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "full-auto")))
        (should-error
         (call-interactively (lookup-key transient--transient-map (kbd "k")))
         :type 'user-error))
      (should (eq 'ask (niwa-session-permission-mode session))))))

(provide 'test-niwa-note-settings)
;;; test-niwa-note-settings.el ends here
