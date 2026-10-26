;;; test-mevedel-cockpit-surfaces-windows.el --- Surface window integration -*- lexical-binding: t -*-

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
(require 'mevedel-artifacts-list)
(require 'mevedel-background-list)
(require 'mevedel-cockpit)
(require 'mevedel-memory-list)
(require 'mevedel-menu)
(require 'mevedel-pending-inputs)
(require 'mevedel-permissions-list)
(require 'mevedel-plugin-ui)
(require 'mevedel-recovery)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-tools-list)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-worktree)

(defun mevedel-cockpit-surfaces-windows-test--layout ()
  "Return the frame's window tree, buffers, edges and selection."
  (list (window-tree)
        (mapcar (lambda (window)
                  (list window (window-buffer window) (window-edges window)))
                (window-list nil 'no-minibuf))
        (selected-window)))

(defun mevedel-cockpit-surfaces-windows-test--pending (session)
  "Give SESSION one held steering entry without scheduling a model turn."
  (let ((entry (mevedel-session-enqueue-pending-input
                session 'steering '(:input "Inspect this queued steering"))))
    ;; The failure latch is independent of the cockpit's lifetime-owned pause.
    ;; Keeping it set lets q resume that pause without dispatching a request.
    (mevedel-session-set-pending-input-failure-paused session t)
    entry))

(defun mevedel-cockpit-surfaces-windows-test--round-trip (open mode layout)
  "Check OPEN and MODE restore LAYOUT through the surface's real q binding."
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (let* ((root (file-name-as-directory
                    (make-temp-file "mevedel-surface-windows-" t)))
             (default-directory root)
             (workspace (mevedel-workspace--create
                         :type 'project :id "surface-windows"
                         :root root :name "surface-windows"))
             (session (mevedel-session-create "surface-windows" workspace))
             (other (generate-new-buffer " *surface-windows-other*"))
             (foreign (memq layout '(foreign foreign-split)))
             (draft "> keep this draft\nand its second line")
             (pending (when (eq mode 'mevedel-pending-inputs-mode)
                        (mevedel-cockpit-surfaces-windows-test--pending session)))
             surface menu-called diagnostics)
        (unwind-protect
            (progn
              ;; Worktree collection runs actual Git against a private repo.
              (should (zerop (call-process "git" nil nil nil
                                           "init" "--quiet" root)))
              (with-current-buffer data-buf
                (setq-local default-directory root
                            mevedel--session session
                            mevedel--workspace workspace
                            mevedel--view-buffer view-buf))
              (with-current-buffer view-buf
                (setq-local default-directory root
                            mevedel--session session)
                (goto-char (mevedel-view--input-start))
                (insert draft)
                (goto-char (+ (mevedel-view--input-start) 7)))
              (delete-other-windows)
              (switch-to-buffer (if foreign other view-buf))
              (when (memq layout '(split foreign-split))
                (set-window-buffer (split-window-right)
                                   (if foreign view-buf other)))
              ;; A fresh batch frame reports top=0 despite menu-bar-lines=1
              ;; until its first configuration restoration (also without
              ;; mevedel loaded).  Materialize that geometry before measuring.
              (set-window-configuration (current-window-configuration))
              (let* ((origin (current-buffer))
                     (context (mevedel-cockpit-context-for-buffer view-buf))
                     (before (mevedel-cockpit-surfaces-windows-test--layout))
                     (draft-point (with-current-buffer view-buf
                                    (- (point) (mevedel-view--input-start)))))
                (when foreign
                  (setq context (plist-put context :origin-buffer origin)))
                ;; Observe menu entry, not display/quit or domain collection.
                ;; Its defining owner is explicitly required above.
                (cl-letf (((symbol-function 'mevedel-menu)
                           (lambda () (setq menu-called t))))
                  (mevedel-test--with-captured-diagnostics diagnostics
                    ;; Normal view entry resolves its own context.  Foreign
                    ;; callers carry the same real owners explicitly.
                    (setq surface (if foreign (funcall open context)
                                    (funcall open)))
                    (should (eq surface (window-buffer (selected-window))))
                    (should (eq mode (buffer-local-value 'major-mode surface)))
                    (should (eq session
                                (mevedel-cockpit-context-session
                                 (buffer-local-value 'mevedel-cockpit--context
                                                     surface))))
                    (when pending
                      (should (mevedel-session-pending-input-paused session))
                      (should (= 1 (length tabulated-list-entries))))
                    (let ((quit (key-binding (kbd "q"))))
                      (should (commandp quit))
                      (call-interactively quit)))
                  (should (equal "" diagnostics)))
                (should-not menu-called)
                (should-not (buffer-live-p surface))
                (let ((after (mevedel-cockpit-surfaces-windows-test--layout)))
                  (ert-info ((format "Before: %S\nAfter: %S" before after))
                    (should (equal before after))))
                (with-current-buffer view-buf
                  (should (equal draft (mevedel-view--input-text)))
                  (should (= draft-point
                             (- (point) (mevedel-view--input-start)))))
                (when pending
                  (should-not (mevedel-session-pending-input-paused session))
                  (should (mevedel-session-pending-input-failure-paused session))
                  (should (eq pending
                              (car (mevedel-session-pending-steering session)))))))
          (when (buffer-live-p surface) (kill-buffer surface))
          (when (buffer-live-p other) (kill-buffer other))
          (delete-directory root t))))))

(mevedel-deftest mevedel-cockpit-surfaces-open-quit-windows
  ()
  (dolist (layout '(single split foreign foreign-split))
    (ert-info ((format "Surface %s, layout %s" ',open layout))
      (mevedel-cockpit-surfaces-windows-test--round-trip ',open ',mode layout)))
  (open mode)
  :doc "tools restores direct-entry layouts without reopening the menu"
  mevedel-tools-list-open mevedel-tools-list-mode
  :doc "skills restores direct-entry layouts without reopening the menu"
  mevedel-skills-list-open mevedel-skills-list-mode
  :doc "plugins restores direct-entry layouts without reopening the menu"
  mevedel-plugins-list-open mevedel-plugins-list-mode
  :doc "permissions restores direct-entry layouts without reopening the menu"
  mevedel-permissions-list-open mevedel-permissions-list-mode
  :doc "artifacts restores direct-entry layouts without reopening the menu"
  mevedel-artifacts-list-open mevedel-artifacts-list-mode
  :doc "background work restores direct-entry layouts without reopening the menu"
  mevedel-background-list-open mevedel-background-list-mode
  :doc "worktrees restores direct-entry layouts without reopening the menu"
  mevedel-worktree-list-open mevedel-worktree-list-mode
  :doc "memory restores direct-entry layouts without reopening the menu"
  mevedel-memory-list-open mevedel-memory-list-mode
  :doc "pending inputs restores layouts and releases only the cockpit pause"
  mevedel-pending-inputs-open mevedel-pending-inputs-mode)

(provide 'test-mevedel-cockpit-surfaces-windows)
;;; test-mevedel-cockpit-surfaces-windows.el ends here
