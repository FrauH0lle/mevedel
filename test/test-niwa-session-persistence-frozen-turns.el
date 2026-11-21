;;; test-niwa-session-persistence-frozen-turns.el -- Saved directive turns -*- lexical-binding: t -*-

;;; Commentary:
;; A sanitized real old session and instruction snapshot resume normally.
;; The fixture README distinguishes copied data from added boundary records.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-transcript-audit)
(require 'niwa-transcript-restore)
(require 'niwa-transport)
(require 'gptel-openai)

;; `niwa-journal-capture'
(defvar niwa-journal-enabled)

(niwa-deftest niwa-session-persistence-resume-id/frozen-directive-turns
  (:quiet t :doc "old session snapshots and marked turns resume as ordinary view and model context")
  (let* ((gptel--known-tools (copy-tree gptel--known-tools))
         (gptel--known-presets (copy-tree gptel--known-presets))
         (gptel-prompt-transform-functions (copy-tree gptel-prompt-transform-functions))
         (gptel--known-backends nil)
         (gptel-backend (gptel-make-openai "Frozen" :key "test" :models '(test-model)))
         (gptel-model 'test-model)
         (niwa-journal-enabled nil)
         (transport-enabled niwa-transport--enabled-p)
         (before (buffer-list))
         (timers (append timer-list timer-idle-list nil))
         (fixture (test-niwa-session-persistence--make-tempdir-workspace))
         (workspace (car fixture))
         (tempdir (cdr fixture))
         (session (niwa-session-create "frozen-turns" workspace))
         (owner (generate-new-buffer " *frozen-turns-owner*"))
         restored view payload)
    (unwind-protect
        (progn
          ;; Real transform hooks, as an interactive session installs them.
          (niwa-install)
          (with-current-buffer owner
            (org-mode)
            (setq-local gptel-response-separator "\n\n"
                        niwa--session session
                        niwa--workspace workspace)
            (insert-file-contents
             (expand-file-name "test/fixtures/old-notes-session/segment-0001.chat.org"
                               niwa-test--worktree-root))
            (gptel--restore-props (read (org-entry-get (point-min) "GPTEL_BOUNDS")))
            (niwa-transcript-restore-properties)
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (niwa-session-artifacts-save session owner))
          (let* ((save-path (niwa-session-save-path session))
                 (snapshot (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name "test/fixtures/old-notes-session/instructions.data"
                                                niwa-test--worktree-root))
                             (buffer-string))))
            ;; A session written before snapshots were dropped still has them.
            (make-directory (file-name-concat save-path "instructions") t)
            (dolist (name '("current.el" "turn-000000.el"))
              (write-region snapshot nil (file-name-concat save-path "instructions" name)
                            nil 'silent)))
          (test-niwa-session-persistence--release-and-kill owner session)
          (setq owner nil)
          (when (file-exists-p (niwa-instruction-store-path workspace))
            (delete-file (niwa-instruction-store-path workspace)))
          (test-niwa-session-persistence--reset-instructions)
          ;; Reopen from committed artifacts through the production resume path.
          (setq restored (niwa-session-persistence-resume-id
                          workspace (niwa-session-session-id session)))
          (should (buffer-live-p restored))
          (should-not (file-exists-p (niwa-instruction-store-path workspace)))
          (setq view (buffer-local-value 'niwa--view-buffer restored))
          (should (buffer-live-p view))
          (with-current-buffer view
            (niwa-view--full-rerender)
            (let ((text (buffer-substring-no-properties
                         (point-min) (niwa-view-transcript-tail))))
              (dolist (ordinary '("ORDINARY-EARLIER-QUESTION" "ORDINARY-EARLIER-ANSWER"
                                  "ORDINARY-LATER-QUESTION" "ORDINARY-LATER-ANSWER"
                                  "Historical directive" "OLD-DIRECTIVE-REPLY"))
                (should (string-search ordinary text)))
              (should (string-search "hook audit" text))
              (should-not (string-search "directive-turn-boundary" text))))
          (with-current-buffer restored
            (goto-char (point-max))
            (insert "\n\nORDINARY-FOLLOW-UP")
            (setq payload (niwa-view-test--dry-run-request-data)))
          (dolist (ordinary '("ORDINARY-EARLIER-QUESTION" "ORDINARY-EARLIER-ANSWER"
                              "ORDINARY-LATER-QUESTION" "ORDINARY-LATER-ANSWER"
                              "ORDINARY-FOLLOW-UP" "Historical directive"
                              "OLD-DIRECTIVE-REPLY"))
            (should (string-search ordinary payload)))
          ;; Ordinary Org drawers are stripped by gptel, not a directive reader.
          (should-not (string-search "OLD-DIRECTIVE-REQUEST" payload))
          (should-not (string-search "directive-turn-boundary" payload)))
      (test-niwa-session-persistence--release-and-kill owner session)
      (when (buffer-live-p restored)
        (test-niwa-session-persistence--release-and-kill
         restored (buffer-local-value 'niwa--session restored)))
      (dolist (buf (cl-set-difference (buffer-list) before))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
      (dolist (timer (append timer-list timer-idle-list nil))
        (unless (memq timer timers) (cancel-timer timer)))
      (niwa-uninstall)
      (when transport-enabled (niwa-transport-install))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-session-persistence-frozen-turns)
;;; test-niwa-session-persistence-frozen-turns.el ends here
