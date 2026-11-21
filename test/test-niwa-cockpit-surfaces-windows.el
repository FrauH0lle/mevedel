;;; test-niwa-cockpit-surfaces-windows.el --- Surface window integration -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise every tabulated cockpit owner's real open command and q binding.
;; Real paired views and sessions retain their drafts and window layouts; only
;; the shared view fixture's recovery writes and batch geometry are replaced.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-artifacts-list)
(require 'niwa-background-list)
(require 'niwa-cockpit)
(require 'niwa-memory-list)
(require 'niwa-menu)
(require 'niwa-pending-inputs)
(require 'niwa-permissions-list)
(require 'niwa-plugin-ui)
(require 'niwa-recovery)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'niwa-tools-list)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)
(require 'niwa-worktree)

(defun niwa-cockpit-surfaces-windows-test--layout ()
  "Return the frame's window tree, buffers, edges and selection."
  (list (window-tree)
        (mapcar (lambda (window)
                  (list window (window-buffer window) (window-edges window)))
                (window-list nil 'no-minibuf))
        (selected-window)))

(defun niwa-cockpit-surfaces-windows-test--pending (session)
  "Give SESSION one held steering entry without scheduling a model turn."
  (let ((entry (niwa-session-enqueue-pending-input
                session 'steering '(:input "Inspect this queued steering"))))
    ;; The failure latch is independent of the cockpit's lifetime-owned pause.
    ;; Keeping it set lets q resume that pause without dispatching a request.
    (niwa-session-set-pending-input-failure-paused session t)
    entry))

(defun niwa-cockpit-surfaces-windows-test--round-trip (open mode layout)
  "Check OPEN and MODE restore LAYOUT through the surface's real q binding."
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((root (file-name-as-directory
                    (make-temp-file "niwa-surface-windows-" t)))
             (default-directory root)
             (workspace (niwa-workspace--create
                         :type 'project :id "surface-windows"
                         :root root :name "surface-windows"))
             (session (niwa-session-create "surface-windows" workspace))
             (other (generate-new-buffer " *surface-windows-other*"))
             (foreign (memq layout '(foreign foreign-split)))
             (draft "> keep this draft\nand its second line")
             (pending (when (eq mode 'niwa-pending-inputs-mode)
                        (niwa-cockpit-surfaces-windows-test--pending session)))
             surface menu-called diagnostics)
        (unwind-protect
            (progn
              ;; Worktree collection runs actual Git against a private repo.
              (should (zerop (call-process "git" nil nil nil
                                           "init" "--quiet" root)))
              (with-current-buffer data-buf
                (setq-local default-directory root
                            niwa--session session
                            niwa--workspace workspace
                            niwa--view-buffer view-buf))
              (with-current-buffer view-buf
                (setq-local default-directory root
                            niwa--session session)
                (niwa-view-test--insert-composer-draft draft 7))
              (delete-other-windows)
              (switch-to-buffer (if foreign other view-buf))
              (when (memq layout '(split foreign-split))
                (set-window-buffer (split-window-right)
                                   (if foreign view-buf other)))
              ;; A fresh batch frame reports top=0 despite menu-bar-lines=1
              ;; until its first configuration restoration (also without
              ;; niwa loaded).  Materialize that geometry before measuring.
              (set-window-configuration (current-window-configuration))
              (let* ((origin (current-buffer))
                     (context (niwa-cockpit-context-for-buffer view-buf))
                     (before (niwa-cockpit-surfaces-windows-test--layout))
                     (draft-point (with-current-buffer view-buf
                                    (niwa-view-test--composer-point-offset))))
                (when foreign
                  (setq context (plist-put context :origin-buffer origin)))
                ;; Observe menu entry, not display/quit or domain collection.
                ;; Its defining owner is explicitly required above.
                (cl-letf (((symbol-function 'niwa-menu)
                           (lambda () (setq menu-called t))))
                  (niwa-test--with-captured-diagnostics diagnostics
                    ;; Normal view entry resolves its own context.  Foreign
                    ;; callers carry the same real owners explicitly.
                    (setq surface (if foreign (funcall open context)
                                    (funcall open)))
                    (should (eq surface (window-buffer (selected-window))))
                    (should (eq mode (buffer-local-value 'major-mode surface)))
                    (should (eq session
                                (niwa-cockpit-context-session
                                 (buffer-local-value 'niwa-cockpit--context
                                                     surface))))
                    (when pending
                      (should (niwa-session-pending-input-paused session))
                      (should (= 1 (length tabulated-list-entries))))
                    (let ((quit (key-binding (kbd "q"))))
                      (should (commandp quit))
                      (call-interactively quit)))
                  (should (equal "" diagnostics)))
                (should-not menu-called)
                (should-not (buffer-live-p surface))
                (let ((after (niwa-cockpit-surfaces-windows-test--layout)))
                  (ert-info ((format "Before: %S\nAfter: %S" before after))
                    (should (equal before after))))
                (with-current-buffer view-buf
                  (should (equal draft (niwa-view-composer-draft-text)))
                  (should (= draft-point
                             (niwa-view-test--composer-point-offset))))
                (when pending
                  (should-not (niwa-session-pending-input-paused session))
                  (should (niwa-session-pending-input-failure-paused session))
                  (should (eq pending
                              (car (niwa-session-pending-steering session)))))))
          (when (buffer-live-p surface) (kill-buffer surface))
          (when (buffer-live-p other) (kill-buffer other))
          (delete-directory root t))))))

(niwa-deftest niwa-cockpit-surfaces-open-quit-windows
  ()
  (dolist (layout '(single split foreign foreign-split))
    (ert-info ((format "Surface %s, layout %s" ',open layout))
      (niwa-cockpit-surfaces-windows-test--round-trip ',open ',mode layout)))
  (open mode)
  :doc "tools restores direct-entry layouts without reopening the menu"
  niwa-tools-list-open niwa-tools-list-mode
  :doc "skills restores direct-entry layouts without reopening the menu"
  niwa-skills-list-open niwa-skills-list-mode
  :doc "plugins restores direct-entry layouts without reopening the menu"
  niwa-plugins-list-open niwa-plugins-list-mode
  :doc "permissions restores direct-entry layouts without reopening the menu"
  niwa-permissions-list-open niwa-permissions-list-mode
  :doc "artifacts restores direct-entry layouts without reopening the menu"
  niwa-artifacts-list-open niwa-artifacts-list-mode
  :doc "background work restores direct-entry layouts without reopening the menu"
  niwa-background-list-open niwa-background-list-mode
  :doc "worktrees restores direct-entry layouts without reopening the menu"
  niwa-worktree-list-open niwa-worktree-list-mode
  :doc "memory restores direct-entry layouts without reopening the menu"
  niwa-memory-list-open niwa-memory-list-mode
  :doc "pending inputs restores layouts and releases only the cockpit pause"
  niwa-pending-inputs-open niwa-pending-inputs-mode)

(provide 'test-niwa-cockpit-surfaces-windows)
;;; test-niwa-cockpit-surfaces-windows.el ends here
