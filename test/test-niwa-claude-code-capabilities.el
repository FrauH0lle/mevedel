;;; test-niwa-claude-code-capabilities.el --- External history controls -*- lexical-binding: t -*-

;;; Commentary:
;; User commands and menu actions refuse unsupported history operations.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code)
(require 'niwa-claude-code-session)
(require 'niwa-claude-code-btw)
(require 'niwa-menu)
(require 'niwa-session-rewind)
(require 'niwa-side-conversation)
(require 'gptel-transient)

(niwa-deftest niwa-take-control/claude-resume (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let ((history `(("root" :engine claude-code :id "retained-native-id"
                            :host ,(system-name)
                            :directory ,(expand-file-name niwa-claude-code-directory)
                            :state ready)))
          view)
      (unwind-protect
          (progn
            (setf (niwa-session-external-conversations session) history)
            (insert "Retained transcript evidence\n")
            (niwa-session-artifacts-save session buffer t)
            (niwa-session-durability-lease-release
             (niwa-session-save-path session) session)
            (niwa-session-persistence-apply-read-only-mode buffer "old owner stopped")
            (setq view (niwa-view--ensure buffer))
            (with-current-buffer view
              (niwa-view--full-rerender)
              (niwa-view-composer-insert "> preserve this draft\nsecond line")
              (call-interactively
               (lookup-key niwa-view--control-transfer-request-map (kbd "r")))
              (should (equal "> preserve this draft\nsecond line"
                             (niwa-view-composer-draft-text))))
            (should-not niwa-session--read-only-mode)
            (should-not buffer-read-only)
            (should (eq 'owned (niwa-session-durability-lease-state
                               (niwa-session-save-path session))))
            (should (equal history (niwa-session-external-conversations session)))
            (should (string-match-p "Retained transcript evidence" (buffer-string)))
            (should-not niwa--current-request))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-menu/claude-history (:quiet t)
  (niwa-engine-test--with-claude-session
    (let (view)
      (unwind-protect
          (progn
            (setq-local gptel-system-prompt "History control fixture" gptel-tools nil)
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          nil))))
              (niwa--insert-user-turn "history evidence")
              (niwa--send-request)
              (niwa-test--await 5 "History fixture did not settle"
                (not (niwa-turn-busy-p buffer))))
            ;; Native /btw is supported, unlike durable history operations.
            (let* ((process-environment (copy-sequence process-environment))
                   (native-file (file-name-concat root "fixture-session.json"))
                   (fork-file (file-name-concat root "fixture-fork.json"))
                   (parent-text (buffer-string))
                   side-view)
              (setenv "NIWA_ACP_TEST_HISTORY" root)
              (with-temp-file native-file (insert "[\"native history\"]"))
              (unwind-protect
                  (cl-letf (((symbol-function 'niwa-claude-code-launch)
                             (niwa-engine-test--claude-launch
                              (lambda (&rest _) nil)))
                            ((symbol-function 'pop-to-buffer)
                             (lambda (value &rest _) (setq side-view value))))
                    (niwa-side-conversation-open)
                    (niwa-test--await 10 "Native side did not open" side-view)
                    (should (buffer-local-value 'niwa-view--side-conversation-p side-view))
                    (should (file-exists-p fork-file))
                    (should (equal parent-text (buffer-string)))
                    (with-current-buffer side-view (niwa-side-conversation-close))
                    (niwa-test--await 10 "Native side fork was not deleted"
                      (not niwa-claude-code-btw--forks))
                    (should-not (file-exists-p fork-file))
                    (should (equal "[\"native history\"]"
                                   (with-temp-buffer
                                     (insert-file-contents native-file)
                                     (buffer-string)))))
                (when (buffer-live-p side-view) (kill-buffer side-view))
                (niwa-claude-code-btw-shutdown)))
            (setq view (niwa-view--ensure buffer))
            (with-current-buffer view (niwa-view--full-rerender))
            (let ((before (buffer-string))
                  (id (niwa-session-session-id session))
                  (history (copy-tree (niwa-session-external-conversations session))))
              (with-current-buffer view
                (niwa-view-composer-insert "> preserve this draft\nsecond line"))
              (let (calls)
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (&rest args) (nth 6 args)))
                          ((symbol-function 'niwa-session-rewind--choose-scope)
                           (lambda (&rest _) 'conversation))
                          ((symbol-function 'niwa-session-rewind-published-heads)
                           (lambda (_) '((:head "prior" :turn-count 0))))
                          ((symbol-function 'niwa-session-rewind-rewind)
                           (lambda (source target &rest _)
                             (should (eq buffer source))
                             (should target)
                             (push 'rewind calls)))
                          ((symbol-function 'niwa-session-rewind-restore-head)
                           (lambda (owner source head &rest _)
                             (should (eq session owner))
                             (should (eq buffer source))
                             (should (equal "prior" head))
                             (push 'redo calls))))
                  (niwa-rewind)
                  (niwa-redo)
                  (with-current-buffer view
                    (dolist (command '(niwa-view-rewind-at-point
                                       niwa-menu--rewind-here niwa-menu--redo))
                      (goto-char (or (text-property-any (point-min) (point-max)
                                                       'niwa-view-turn-role 'assistant)
                                     (ert-fail "Missing assistant response")))
                      (funcall command))))
                (should (equal '(redo rewind rewind redo rewind) calls)))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) (ert-fail "Unsupported operation asked for input")))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _) (ert-fail "Unsupported operation opened a picker")))
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) (ert-fail "Unsupported operation asked for approval"))))
                (dolist (command `(niwa-compact
                                   ,(lambda () (niwa-session-control-transfer-request session))
                                   (lambda () (niwa-save-session t))))
                  (ert-info ((format "Direct command %S" command))
                    (should (string-match-p
                             "unavailable for external conversations"
                             (error-message-string (should-error (funcall command) :type 'user-error))))))
                (with-current-buffer view
                  (dolist (command '(niwa-view-arm-conversation-fork
                                     niwa-view-arm-worktree-fork
                                     niwa-menu--fork-conversation-here niwa-menu--fork-worktree-here
                                     niwa-menu--compact))
                    (goto-char (or (text-property-any (point-min) (point-max)
                                                     'niwa-view-turn-role 'assistant)
                                   (ert-fail "Missing assistant response")))
                    (ert-info ((format "View/menu command %S" command))
                      (should (string-match-p
                               "unavailable for external conversations"
                               (error-message-string (should-error (funcall command) :type 'user-error))))))
                  (should-not niwa-view--armed-session-fork)
                  (should (equal "> preserve this draft\nsecond line" (niwa-view-composer-draft-text)))))
              (let ((lease (copy-tree (niwa-session-lease session))))
                (setq-local niwa-session--read-only-mode t)
                (unwind-protect
                    (cl-letf (((symbol-function 'niwa-session-durability-lease-state)
                               (lambda (_) 'foreign)))
                      (with-current-buffer view
                        (dolist (command '(niwa-take-control niwa-menu--take-control))
                          (should (string-match-p
                                   "unavailable for external conversations"
                                   (error-message-string (should-error (funcall command) :type 'user-error)))))))
                  (setq-local niwa-session--read-only-mode nil))
                (should (equal lease (niwa-session-lease session))))
              (should (equal before (buffer-string)))
              (should (equal id (niwa-session-session-id session)))
              (should (equal history (niwa-session-external-conversations session)))
              (should-not niwa--current-request)))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-gptel-bridge-open/claude (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let ((advice-state
           (mapcar (lambda (pair)
                     (list (car pair) (cdr pair) (advice-member-p (cdr pair) (car pair))))
                   '((gptel-menu . niwa-gptel-bridge--assert-menu-backend)
                     (gptel-send--steer . niwa-gptel-bridge--steer-advice)
                     (gptel--suffix-steer . niwa-gptel-bridge--steer-advice)
                     (gptel--steer-tool-calls . niwa-gptel-bridge--tool-steer-advice))))
          view)
      (unwind-protect
          (progn
            (setq view (niwa-view--ensure buffer))
            (niwa-gptel-bridge-install)
            (cl-letf (((symbol-function 'transient-setup)
                       (lambda (&rest _) (ert-fail "API request controls were opened"))))
              (dolist (command '(niwa-gptel-bridge-open niwa-menu--open-gptel
                                 gptel-menu (lambda () (gptel-send '(4)))))
                (ert-info ((format "External menu command %S" command))
                  (let ((error (should-error (funcall command) :type 'user-error)))
                    (should (string-search "gptel request controls" (error-message-string error)))
                    (should (string-search "model menu" (error-message-string error))))))
              (with-current-buffer view
                (should-error (niwa-gptel-bridge-open) :type 'user-error)))
            (should-not niwa-gptel-bridge--return-view-buffer)
            (should-not niwa--current-request)
            (should (= 0 (niwa-session-turn-count session))))
        (dolist (entry advice-state)
          (unless (nth 2 entry) (advice-remove (car entry) (cadr entry))))
        (niwa-gptel-bridge--clear-return-state)
        (niwa-gptel-bridge--cleanup-advice)
        (when (buffer-live-p view) (kill-buffer view))))))

(provide 'test-niwa-claude-code-capabilities)
;;; test-niwa-claude-code-capabilities.el ends here
