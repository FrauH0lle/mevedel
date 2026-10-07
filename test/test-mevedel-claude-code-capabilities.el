;;; test-mevedel-claude-code-capabilities.el --- External history controls -*- lexical-binding: t -*-

;;; Commentary:
;; User commands and menu actions refuse unsupported history operations.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-menu)
(require 'mevedel-side-conversation)
(require 'gptel-transient)

(mevedel-deftest mevedel-menu/claude-history (:quiet t)
  (mevedel-engine-test--with-claude-session
    (let (view)
      (unwind-protect
          (progn
            (setq-local gptel-system-prompt "History control fixture" gptel-tools nil)
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          nil))))
              (mevedel--insert-user-turn "history evidence")
              (mevedel--send-request)
              (mevedel-test--await 5 "History fixture did not settle"
                (not (mevedel-turn-busy-p buffer))))
            (setq view (mevedel-view--ensure buffer))
            (with-current-buffer view (mevedel-view--full-rerender))
            (let ((before (buffer-string))
                  (id (mevedel-session-session-id session))
                  (history (copy-tree (mevedel-session-external-conversations session))))
              (with-current-buffer view
                (goto-char (mevedel-view--input-start))
                (insert "> preserve this draft\nsecond line"))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) (ert-fail "Unsupported operation asked for input")))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _) (ert-fail "Unsupported operation opened a picker")))
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) (ert-fail "Unsupported operation asked for approval"))))
                (dolist (command `(mevedel-compact mevedel-rewind mevedel-redo
                                   mevedel-side-conversation-open
                                   ,(lambda () (mevedel-session-control-transfer-request session))
                                   ,(lambda () (mevedel-session-control-transfer-acquire session buffer))
                                   (lambda () (mevedel-save-session t))))
                  (ert-info ((format "Direct command %S" command))
                    (should (string-match-p
                             "unavailable for external conversations"
                             (error-message-string (should-error (funcall command) :type 'user-error))))))
                (with-current-buffer view
                  (dolist (command '(mevedel-view-arm-conversation-fork
                                     mevedel-view-arm-worktree-fork mevedel-view-rewind-at-point
                                     mevedel-menu--fork-conversation-here mevedel-menu--fork-worktree-here
                                     mevedel-menu--rewind-here mevedel-menu--redo mevedel-menu--compact))
                    (goto-char (or (text-property-any (point-min) (point-max)
                                                     'mevedel-view-turn-role 'assistant)
                                   (ert-fail "Missing assistant response")))
                    (ert-info ((format "View/menu command %S" command))
                      (should (string-match-p
                               "unavailable for external conversations"
                               (error-message-string (should-error (funcall command) :type 'user-error))))))
                  (should-not mevedel-view--armed-session-fork)
                  (should (equal "> preserve this draft\nsecond line" (mevedel-view--input-text)))))
              (let ((lease (copy-tree (mevedel-session-lease session))))
                (setq-local mevedel-session--read-only-mode t)
                (unwind-protect
                    (with-current-buffer view
                      (dolist (command '(mevedel-take-control mevedel-menu--take-control))
                        (should (string-match-p
                                 "unavailable for external conversations"
                                 (error-message-string (should-error (funcall command) :type 'user-error))))))
                  (setq-local mevedel-session--read-only-mode nil))
                (should (equal lease (mevedel-session-lease session))))
              (should (equal before (buffer-string)))
              (should (equal id (mevedel-session-session-id session)))
              (should (equal history (mevedel-session-external-conversations session)))
              (should-not mevedel--current-request)))
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest mevedel-gptel-bridge-open/claude (:quiet t)
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (let ((advice-state
           (mapcar (lambda (pair)
                     (list (car pair) (cdr pair) (advice-member-p (cdr pair) (car pair))))
                   '((gptel-menu . mevedel-gptel-bridge--assert-menu-backend)
                     (gptel-send--steer . mevedel-gptel-bridge--steer-advice)
                     (gptel--suffix-steer . mevedel-gptel-bridge--steer-advice)
                     (gptel--steer-tool-calls . mevedel-gptel-bridge--tool-steer-advice))))
          view)
      (unwind-protect
          (progn
            (setq view (mevedel-view--ensure buffer))
            (mevedel-gptel-bridge-install)
            (cl-letf (((symbol-function 'transient-setup)
                       (lambda (&rest _) (ert-fail "API request controls were opened"))))
              (dolist (command '(mevedel-gptel-bridge-open mevedel-menu--open-gptel
                                 gptel-menu (lambda () (gptel-send '(4)))))
                (ert-info ((format "External menu command %S" command))
                  (let ((error (should-error (funcall command) :type 'user-error)))
                    (should (string-search "gptel request controls" (error-message-string error)))
                    (should (string-search "model menu" (error-message-string error))))))
              (with-current-buffer view
                (should-error (mevedel-gptel-bridge-open) :type 'user-error)))
            (should-not mevedel-gptel-bridge--return-view-buffer)
            (should-not mevedel--current-request)
            (should (= 0 (mevedel-session-turn-count session))))
        (dolist (entry advice-state)
          (unless (nth 2 entry) (advice-remove (car entry) (cadr entry))))
        (mevedel-gptel-bridge--clear-return-state)
        (mevedel-gptel-bridge--cleanup-advice)
        (when (buffer-live-p view) (kill-buffer view))))))

(provide 'test-mevedel-claude-code-capabilities)
;;; test-mevedel-claude-code-capabilities.el ends here
