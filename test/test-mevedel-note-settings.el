;;; test-mevedel-note-settings.el -- Note settings UI tests -*- lexical-binding: t -*-

;;; Commentary:

;; Drive the public settings command and its real transient keys in source files.

;;; Code:

(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-journal-process)
(require 'mevedel-menu)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-presets)
(require 'mevedel-note-settings)
(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-note-settings-test--with-source (&rest body)
  "Run BODY in a real source file with isolated notes and buffers."
  (declare (indent 0))
  `(let* ((root (make-temp-file "mevedel-note-settings-" t))
          (file (file-name-concat root "source.el"))
          (mevedel-sessions-directory (file-name-concat root "sessions"))
          (mevedel-user-dir (file-name-concat root "user"))
          (mevedel-journal-enabled nil)
          (workspace (mevedel-workspace--create
                      :type 'file :id root :root root :name "settings"))
          (mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel-instruction-store--dirty nil)
          (mevedel-instruction-store--failed nil)
          (gptel--known-presets (copy-tree gptel--known-presets))
          (mevedel-preset--registry (copy-tree mevedel-preset--registry))
          (transport-enabled mevedel-transport--enabled-p)
          (before (buffer-list))
          (timers (append timer-list timer-idle-list nil))
          source)
     (unwind-protect
         (save-window-excursion
           (make-directory (file-name-concat root ".mevedel"))
           (write-region "before\nselected code\nafter\n" nil file nil 'silent)
           (mevedel-install)
           (setq source (find-file-noselect file))
           (switch-to-buffer source)
           (setq-local mevedel--workspace workspace)
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
       (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
       (mevedel-uninstall)
       (when transport-enabled (mevedel-transport-install))
       (delete-directory root t))))

(mevedel-deftest mevedel-note-settings-test--with-source
  (:quiet t :doc "cleanup preserves both enabled and disabled transport state")
  (let ((transport-enabled mevedel-transport--enabled-p))
    (unwind-protect
        (dolist (enabled '(t nil))
          (if enabled (mevedel-transport-install) (mevedel-transport-uninstall))
          (mevedel-note-settings-test--with-source
            (should mevedel-transport--enabled-p))
          (should (eq enabled mevedel-transport--enabled-p)))
      (if transport-enabled
          (mevedel-transport-install)
        (mevedel-transport-uninstall)))))

(mevedel-deftest mevedel-note-settings
  (:quiet t)
  ,test
  (test)
  :doc "unsent preset choice persists without creating a conversation"
  (mevedel-note-settings-test--with-source
    (let* ((note (mevedel-note-create "Explain"))
           (uuid (overlay-get note 'mevedel-uuid)))
      (mevedel-note-settings note)
      (with-current-buffer transient--buffer-name
        (should (string-search "Base: mevedel-implement" (buffer-string))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "mevedel-implement")))
        (call-interactively (lookup-key transient--transient-map (kbd "p"))))
      (should (eq 'mevedel-implement
                  (plist-get (overlay-get note 'mevedel-note-settings) :preset)))
      (should-not (overlay-get note 'mevedel-note-session-id))
      (should-not (cl-some (lambda (buffer)
                             (buffer-local-value 'mevedel--session buffer))
                           (cl-set-difference (buffer-list) before)))
      (transient--emergency-exit :test)
      (mevedel-instruction-store-flush t)
      (mevedel--clear-instruction-state workspace)
      (mevedel-instruction-store-observe workspace)
      (should (eq 'mevedel-implement
                  (plist-get (overlay-get (mevedel-note-resolve uuid workspace)
                                          'mevedel-note-settings) :preset)))))
  :doc "unsent model and effort choices stay on the note"
  (mevedel-note-settings-test--with-source
    (let* ((gptel--known-backends nil)
           (gptel-backend (gptel-make-openai "Settings" :key "test"
                                           :models '(alpha beta)))
           (gptel-model 'alpha)
           (old-effort (get 'beta :reasoning-effort))
           (note (mevedel-note-create "Explain"))
           (uuid (overlay-get note 'mevedel-uuid)))
      (unwind-protect
          (progn
            (put 'beta :reasoning-effort '(member low high))
            (mevedel-note-settings note)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "Settings:beta")))
              (execute-kbd-macro (kbd "m RET e q")))
            (setq note (mevedel-note-resolve uuid workspace))
            (should (equal '(:provider "Settings:beta" :effort low)
                           (overlay-get note 'mevedel-note-settings)))
            (should-not (overlay-get note 'mevedel-note-session-id)))
        (put 'beta :reasoning-effort old-effort))))
  :doc "unsent picker previews inherited static settings without executing preset hooks"
  (mevedel-note-settings-test--with-source
    (let* ((gptel--known-backends nil)
           (gptel-backend (gptel-make-openai "Fallback" :key "test" :models '(alpha)))
           (gptel-model 'alpha)
           (backend (gptel-make-openai "Static" :key "test" :models '(alpha beta)))
           (old-effort (get 'beta :reasoning-effort))
           (note (mevedel-note-create "Explain"))
           (uuid (overlay-get note 'mevedel-uuid)))
      (unwind-protect
          (progn
            (put 'beta :reasoning-effort '(member low high))
            (gptel-make-preset 'settings-first :backend backend :model 'alpha :reasoning-effort 'low)
            (gptel-make-preset 'settings-last :backend "Static" :model 'beta :reasoning-effort 'high)
            (gptel-make-preset 'settings-child :parents '(settings-first settings-last)
                              :model '(:eval (error "Dynamic preview executed"))
                              :post (lambda () (error "Preset hook executed"))
                              :system (lambda () (error "System prompt executed")))
            (overlay-put note 'mevedel-note-settings '(:preset settings-child))
            (mevedel-note-settings note)
            (execute-kbd-macro (kbd "m"))
            (with-current-buffer transient--buffer-name
              (should (string-search "Static:beta" (buffer-string)))
              (should (string-search "effort high" (buffer-string))))
            (execute-kbd-macro (kbd "e q"))
            (setq note (mevedel-note-resolve uuid workspace))
            (should (equal '(:preset settings-child :provider "Static:beta" :effort nil)
                           (overlay-get note 'mevedel-note-settings)))
            (should-not (overlay-get note 'mevedel-note-session-id)))
        (put 'beta :reasoning-effort old-effort))))
  :doc "unsent permission choice never changes the global default"
  (mevedel-note-settings-test--with-source
    (let ((permission (default-toplevel-value 'mevedel-permission-mode))
          (note (mevedel-note-create "Explain")))
      (mevedel-note-settings note)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "full-auto")))
        (call-interactively (lookup-key transient--transient-map (kbd "k"))))
      (should (eq 'full-auto
                  (plist-get (overlay-get note 'mevedel-note-settings) :permission-mode)))
      (should (eq permission (default-toplevel-value 'mevedel-permission-mode)))
      (should-not (overlay-get note 'mevedel-note-session-id))))
  :doc "linked permission choice updates only its live owning session"
  (mevedel-note-settings-test--with-source
    (let* ((note (mevedel-note-create "Explain"))
           (data (mevedel--chat-buffer nil t workspace root))
           (session (buffer-local-value 'mevedel--session data))
           (permission (default-toplevel-value 'mevedel-permission-mode)))
      (mevedel-note-mark-sent (list note) (mevedel-session-session-id session))
      (mevedel-note-settings note)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "full-auto")))
        (call-interactively (lookup-key transient--transient-map (kbd "k"))))
      (should (eq 'full-auto (mevedel-session-permission-mode session)))
      (should (eq permission (default-toplevel-value 'mevedel-permission-mode)))
      (should-not (overlay-get note 'mevedel-note-settings))))
  :doc "linked preset choice belongs to its session, not the note or another chat"
  (mevedel-note-settings-test--with-source
    (let* ((mevedel-preset--registry (copy-tree mevedel-preset--registry))
           (gptel--known-presets (copy-tree gptel--known-presets))
           (note (mevedel-note-create "Explain"))
           (data (mevedel--chat-buffer nil t workspace root))
           (session (buffer-local-value 'mevedel--session data))
           (other (mevedel--chat-buffer nil t workspace root)))
      (mevedel-define-preset settings-team :parents (mevedel-implement))
      (overlay-put note 'mevedel-note-session-id (mevedel-session-session-id session))
      (mevedel-note-settings note)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "settings-team")))
        (call-interactively (lookup-key transient--transient-map (kbd "p"))))
      (should (eq 'settings-team (mevedel-session-preset-name session)))
      (should-not (eq 'settings-team
                      (mevedel-session-preset-name (buffer-local-value 'mevedel--session other))))
      (should-not (overlay-get note 'mevedel-note-settings))))
  :doc "linked model choice updates its session and keeps another chat unchanged"
  (mevedel-note-settings-test--with-source
    (let* ((gptel--known-backends nil)
           (backend (gptel-make-openai "Settings" :key "test" :models '(alpha beta)))
           (note (mevedel-note-create "Explain"))
           (data (mevedel--chat-buffer nil t workspace root))
           (session (buffer-local-value 'mevedel--session data))
           (other (mevedel--chat-buffer nil t workspace root))
           (other-provider (buffer-local-value 'gptel-model other)))
      (with-current-buffer data
        (mevedel-model-set-session-provider session (list :backend backend :model 'alpha)))
      (overlay-put note 'mevedel-note-session-id (mevedel-session-session-id session))
      (mevedel-note-settings note)
      (call-interactively (lookup-key transient--transient-map (kbd "m")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Settings:beta")))
        (call-interactively (lookup-key transient--transient-map (kbd "RET"))))
      (should (equal "Settings:beta" (mevedel-session-model-provider session)))
      (should (eq 'beta (buffer-local-value 'gptel-model data)))
      (should (eq other-provider (buffer-local-value 'gptel-model other)))
      (should-not (overlay-get note 'mevedel-note-settings))))
  :doc "linked read-only conversation refuses settings mutations"
  (mevedel-note-settings-test--with-source
    (let* ((note (mevedel-note-create "Explain"))
           (data (mevedel--chat-buffer nil t workspace root))
           (session (buffer-local-value 'mevedel--session data)))
      (overlay-put note 'mevedel-note-session-id (mevedel-session-session-id session))
      (with-current-buffer data (setq buffer-read-only t))
      (mevedel-note-settings note)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "full-auto")))
        (should-error
         (call-interactively (lookup-key transient--transient-map (kbd "k")))
         :type 'user-error))
      (should (eq 'ask (mevedel-session-permission-mode session))))))

(provide 'test-mevedel-note-settings)
;;; test-mevedel-note-settings.el ends here
