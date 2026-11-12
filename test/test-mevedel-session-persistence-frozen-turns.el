;;; test-mevedel-session-persistence-frozen-turns.el -- Saved directive turns -*- lexical-binding: t -*-

;;; Commentary:
;; A sanitized real old session and instruction snapshot resume normally.
;; The fixture README distinguishes copied data from added boundary records.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-transcript-audit)
(require 'mevedel-transcript-restore)
(require 'mevedel-transport)
(require 'gptel-openai)

;; `mevedel-journal-capture'
(defvar mevedel-journal-enabled)

(mevedel-deftest mevedel-session-persistence-resume-id/frozen-directive-turns
  (:quiet t :doc "old session snapshots and marked turns resume as ordinary view and model context")
  (let* ((gptel--known-tools (copy-tree gptel--known-tools))
         (gptel--known-presets (copy-tree gptel--known-presets))
         (gptel-prompt-transform-functions (copy-tree gptel-prompt-transform-functions))
         (gptel--known-backends nil)
         (gptel-backend (gptel-make-openai "Frozen" :key "test" :models '(test-model)))
         (gptel-model 'test-model)
         (mevedel-journal-enabled nil)
         (transport-enabled mevedel-transport--enabled-p)
         (before (buffer-list))
         (timers (append timer-list timer-idle-list nil))
         (fixture (test-mevedel-session-persistence--make-tempdir-workspace))
         (workspace (car fixture))
         (tempdir (cdr fixture))
         (session (mevedel-session-create "frozen-turns" workspace))
         (owner (generate-new-buffer " *frozen-turns-owner*"))
         restored view payload)
    (unwind-protect
        (progn
          ;; Real transform hooks, as an interactive session installs them.
          (mevedel-install)
          (with-current-buffer owner
            (org-mode)
            (setq-local gptel-response-separator "\n\n"
                        mevedel--session session
                        mevedel--workspace workspace)
            (insert-file-contents
             (expand-file-name "test/fixtures/old-notes-session/segment-0001.chat.org"
                               mevedel-test--worktree-root))
            (gptel--restore-props (read (org-entry-get (point-min) "GPTEL_BOUNDS")))
            (mevedel-transcript-restore-properties)
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (mevedel-session-artifacts-save session owner))
          (let* ((save-path (mevedel-session-save-path session))
                 (snapshot (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name "test/fixtures/old-notes-session/instructions.data"
                                                mevedel-test--worktree-root))
                             (buffer-string))))
            (dolist (name '("current.el" "turn-000000.el"))
              (write-region snapshot nil (file-name-concat save-path "instructions" name)
                            nil 'silent))
            (should (equal snapshot (mevedel-session-artifacts-read-artifact
                                     session "instructions/current.el" t))))
          (test-mevedel-session-persistence--release-and-kill owner session)
          (setq owner nil)
          (when (file-exists-p (mevedel-instruction-store-path workspace))
            (delete-file (mevedel-instruction-store-path workspace)))
          (test-mevedel-session-persistence--reset-instructions)
          ;; Reopen from committed artifacts through the production resume path.
          (setq restored (mevedel-session-persistence-resume-id
                          workspace (mevedel-session-session-id session)))
          (should (buffer-live-p restored))
          (should-not (file-exists-p (mevedel-instruction-store-path workspace)))
          (setq view (buffer-local-value 'mevedel--view-buffer restored))
          (should (buffer-live-p view))
          (with-current-buffer view
            (mevedel-view--full-rerender)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (dolist (ordinary '("ORDINARY-EARLIER-QUESTION" "ORDINARY-EARLIER-ANSWER"
                                  "ORDINARY-LATER-QUESTION" "ORDINARY-LATER-ANSWER"
                                  "Historical directive" "OLD-DIRECTIVE-REPLY"))
                (should (string-search ordinary text)))
              (should (string-search "hook audit" text))
              (should-not (string-search "directive-turn-boundary" text))))
          (with-current-buffer restored
            (goto-char (point-max))
            (insert "\n\nORDINARY-FOLLOW-UP")
            (setq payload (mevedel-view-test--dry-run-request-data)))
          (dolist (ordinary '("ORDINARY-EARLIER-QUESTION" "ORDINARY-EARLIER-ANSWER"
                              "ORDINARY-LATER-QUESTION" "ORDINARY-LATER-ANSWER"
                              "ORDINARY-FOLLOW-UP" "Historical directive"
                              "OLD-DIRECTIVE-REPLY"))
            (should (string-search ordinary payload)))
          ;; Ordinary Org drawers are stripped by gptel, not a directive reader.
          (should-not (string-search "OLD-DIRECTIVE-REQUEST" payload))
          (should-not (string-search "directive-turn-boundary" payload)))
      (test-mevedel-session-persistence--release-and-kill owner session)
      (when (buffer-live-p restored)
        (test-mevedel-session-persistence--release-and-kill
         restored (buffer-local-value 'mevedel--session restored)))
      (dolist (buf (cl-set-difference (buffer-list) before))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
      (dolist (timer (append timer-list timer-idle-list nil))
        (unless (memq timer timers) (cancel-timer timer)))
      (mevedel-uninstall)
      (when transport-enabled (mevedel-transport-install))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-session-persistence-frozen-turns)
;;; test-mevedel-session-persistence-frozen-turns.el ends here
