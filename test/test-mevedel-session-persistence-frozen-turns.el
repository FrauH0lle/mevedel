;;; test-mevedel-session-persistence-frozen-turns.el -- Saved directive turns -*- lexical-binding: t -*-

;;; Commentary:
;; A saved transcript keeps historical directive turns between persisted
;; boundary markers.  After a real resume they must stay out of the companion
;; view's ordinary content and the provider payload, without any live
;; directive record to consult.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-transcript-audit)
(require 'mevedel-transport)
(require 'gptel-openai)

;; `mevedel-journal-capture'
(defvar mevedel-journal-enabled)

(mevedel-deftest mevedel-session-persistence-resume-id/frozen-directive-turns
  (:quiet t :doc "a resumed transcript hides marked directive bodies from view and payload")
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
            (setq-local gptel-response-separator "\n\n")
            (insert "\n\nORDINARY-EARLIER-QUESTION\n")
            (insert (propertize "ORDINARY-EARLIER-ANSWER\n" 'gptel 'response))
            (insert (mevedel--format-hook-audit-record
                     '(:type directive-turn-boundary :edge start
                       :directive-id "retired-directive" :action implement :turn 2)))
            (insert "Historical directive :implement:\n:PROMPT:\n"
                    "SECRET-DIRECTIVE-REQUEST\n:END:\n")
            (insert (propertize "SECRET-DIRECTIVE-REPLY\n" 'gptel 'response))
            (insert (mevedel--format-hook-audit-record
                     '(:type directive-turn-boundary :edge end
                       :directive-id "retired-directive" :action implement :turn 2
                       :outcome success :activity-kind attempt :sequence 1)))
            (insert "ORDINARY-LATER-QUESTION\n")
            (insert (propertize "ORDINARY-LATER-ANSWER\n" 'gptel 'response))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (mevedel-session-artifacts-save session owner))
          (test-mevedel-session-persistence--release-and-kill owner session)
          (setq owner nil)
          ;; Reopen from committed artifacts through the production resume path.
          (setq restored (mevedel-session-persistence-resume-id
                          workspace (mevedel-session-session-id session)))
          (should (buffer-live-p restored))
          ;; The exclusion must come from the transcript markers alone.
          (should-not (mevedel-workspace-directives workspace))
          (setq view (buffer-local-value 'mevedel--view-buffer restored))
          (should (buffer-live-p view))
          (with-current-buffer view
            (mevedel-view--full-rerender)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (dolist (ordinary '("ORDINARY-EARLIER-QUESTION" "ORDINARY-EARLIER-ANSWER"
                                  "ORDINARY-LATER-QUESTION" "ORDINARY-LATER-ANSWER"))
                (should (string-search ordinary text)))
              ;; The directive turn is rendered as its own row, not dropped.
              (should (string-search "◆" text))
              (should-not (string-search "SECRET-DIRECTIVE-REQUEST" text))
              (should-not (string-search "SECRET-DIRECTIVE-REPLY" text)))
            (goto-char (point-min))
            (search-forward "◆")
            (mevedel-view-toggle-section)
            (should (string-search "SECRET-DIRECTIVE-REPLY" (buffer-string))))
          (with-current-buffer restored
            (goto-char (point-max))
            (insert "\n\nORDINARY-FOLLOW-UP")
            (setq payload (mevedel-view-test--dry-run-request-data)))
          (dolist (ordinary '("ORDINARY-EARLIER-QUESTION" "ORDINARY-EARLIER-ANSWER"
                              "ORDINARY-LATER-QUESTION" "ORDINARY-LATER-ANSWER"
                              "ORDINARY-FOLLOW-UP"))
            (should (string-search ordinary payload)))
          (should-not (string-search "SECRET-DIRECTIVE" payload))
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
