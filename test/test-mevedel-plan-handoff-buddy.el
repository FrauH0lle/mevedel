;;; test-mevedel-plan-handoff-buddy.el -- Human implementation handoff -*- lexical-binding: t -*-

;;; Commentary:

;; Accept a plan through its real approval card; only model transport is canned.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-plan-mode)
(require 'mevedel-plan-handoff)
(require 'mevedel-notes-list)
(require 'mevedel-note-write)
(require 'mevedel-tool-note)
(require 'mevedel-transport)
(require 'mevedel-journal-process)
(require 'gptel-openai)
(require 'gptel-request)

(mevedel-deftest mevedel-plan-approval-settle/human-implementation
  (:quiet t :doc "acceptance creates proposed step notes in one batch, not source changes")
  (let* ((root (make-temp-file "plan-to-buddy-" t))
         (file (file-name-concat root "source.el"))
         (mevedel-sessions-directory (file-name-concat root "sessions"))
         (mevedel--instruction-states (make-hash-table :test #'equal))
         (mevedel--instruction-current-state-key :global)
         (mevedel-instruction-store--dirty nil)
         (mevedel-instruction-store--failed nil)
         (mevedel-journal-enabled nil)
         (gptel--known-tools (copy-tree gptel--known-tools))
         (gptel--known-presets (copy-tree gptel--known-presets))
         (gptel-prompt-transform-functions (copy-tree gptel-prompt-transform-functions))
         (gptel--known-backends nil)
         (gptel-backend (gptel-make-openai "Plans" :key "test" :models '(test-model)))
         (gptel-model 'test-model)
         (gptel-use-curl t)
         (gptel-stream nil)
         (transport-enabled mevedel-transport--enabled-p)
         (before (buffer-list))
         (timers (append timer-list timer-idle-list nil))
         (plan "# Human implementation\n1. Add input validation at source.el:2.\n2. Check empty input at source.el:3.")
         (syncs 0)
         (counter (lambda (&rest _) (cl-incf syncs)))
         workspace data view sent)
    (unwind-protect
        (save-window-excursion
          (make-directory (file-name-concat root ".mevedel"))
          (write-region "before\nvalidate\ncheck\n" nil file nil 'silent)
          (setq workspace (mevedel-workspace-get-or-create 'file root root "plans"))
          (mevedel-install)
          (cl-letf (((symbol-function 'gptel-curl-get-response)
                     (lambda (fsm) (push fsm sent))))
            (setq data (mevedel--chat-buffer "plan-to-buddy" t workspace)
                  view (mevedel-view--ensure data))
            (mevedel--display-chat-buffer data)
            (with-current-buffer data
              (mevedel-plan-mode-enter)
              (mevedel-plan-write-current plan mevedel--session data)
              (mevedel-plan--metadata-put mevedel--session :status 'proposed)
              (mevedel-plan--metadata-put mevedel--session :proposal-id
                                         (list 1 2 (mevedel-plan-hash plan)))
              (mevedel-plan--metadata-put
               mevedel--session :selection
               '(:location here :context current :execution direct :mode edits
                 :model-provider "Plans:test-model" :reasoning-effort nil))
              (should (mevedel-plan-mode-restore-pending-approval mevedel--session data)))
            (switch-to-buffer view)
            ;; Exercise the same cycle and accept commands as the user.
            (dotimes (_ 2)
              (let ((card (cl-find-if (lambda (ov) (overlay-get ov 'mevedel-plan))
                                      (overlays-in (point-min) (point-max)))))
                (should card)
                (call-interactively (lookup-key (overlay-get card 'keymap) (kbd "e")))))
            (let ((card (cl-find-if (lambda (ov) (overlay-get ov 'mevedel-plan))
                                    (overlays-in (point-min) (point-max)))))
              (should (string-search "Implement myself with notes"
                                     (buffer-string)))
              (call-interactively (lookup-key (overlay-get card 'keymap) (kbd "RET"))))
            (should (= 1 (length sent)))
            (let* ((fsm (car sent))
                   (info (gptel-fsm-info fsm))
                   (payload (gptel--json-encode (plist-get info :data))))
              (dolist (text '("Read the accepted plan" "one note per plan step"
                              "one NoteWrite batch" "Do not edit code"
                              "Do not prepare changes" "omit prepared_change"
                              "Do not send, take or close notes"
                              "most relevant file you read" "notes list"))
                (should (string-search text payload)))
              (should (string-search "Add input validation" payload))
              (with-current-buffer data
                (should-not (mevedel-session-plan-mode mevedel--session))
                (should (eq 'ask (mevedel-session-permission-mode mevedel--session)))
                (should (eq 'edits
                            (plist-get (mevedel-request-implementation-policy
                                        mevedel--current-request) :permission-mode)))
                (should-not (mevedel-session-goal mevedel--session)))
              (advice-add 'mevedel-instruction-store-sync :before counter)
              (plist-put info :http-status "200")
              (plist-put info :status "HTTP/1.1 200 OK")
              (gptel--fsm-transition fsm)
              (plist-put info :tool-use
                         (list (list :id "step-notes" :name "ToolCall"
                                     :args
                                     (list :expression
                                           (format
                                            "(NoteWrite :operations %S)"
                                            (vector
                                             (list :action "create" :file_path file :start_line 2
                                                   :text "Step 1: Add input validation.")
                                             (list :action "create" :file_path file :start_line 3
                                                   :text "Step 2: Check empty input.")))))))
              (gptel--fsm-transition fsm)
              (mevedel-test--await 5 "Note batch did not finish" (= 2 (length sent)))
              (ert-info ((with-current-buffer data (buffer-string)))
                (should (= 1 syncs)))
              (let ((reply-info (gptel-fsm-info (car sent))))
                (plist-put reply-info :http-status "200")
                (plist-put reply-info :status "HTTP/1.1 200 OK")
                (gptel--fsm-transition (car sent))
                (gptel--insert-response
                 "Created #1: Add input validation; #2: Check empty input. Open the notes list to take, send, edit or delete them."
                 reply-info)
                (gptel--fsm-transition (car sent))))
            (mevedel-test--await 5 "Handoff did not settle" (not (mevedel-turn-busy-p data)))
            (with-current-buffer data
              (should-not (plist-get (mevedel-session-plan-metadata mevedel--session)
                                     :implementation-retry)))
            (let ((entries (mevedel-notes-open workspace)))
              (should (= 2 (length entries)))
              (dolist (entry entries)
                (let ((note (mevedel-note-resolve (plist-get entry :uuid) workspace)))
                  (should (eq 'unsent (overlay-get note 'mevedel-note-state)))
                  (should-not (overlay-get note 'mevedel-note-prepared-change))
                  (should-not (overlay-get note 'mevedel-note-session-id))
                  (should (equal file (buffer-file-name (overlay-buffer note))))
                  (should (= (1+ (overlay-get note 'mevedel-id))
                             (with-current-buffer (overlay-buffer note)
                               (line-number-at-pos (overlay-start note))))))))
            (should (equal "before\nvalidate\ncheck\n" (mevedel-test--read-file file)))
            (with-current-buffer (find-buffer-visiting file)
              (should-not (buffer-modified-p)))
            (with-current-buffer view
              (should (string-search "Created #1:" (buffer-string))))
            (mevedel-notes-list workspace)
            (dolist (text '("Step 1: Add input validation." "Step 2: Check empty input."
                            "source.el:2" "source.el:3" "unsent"))
              (should (string-search text (buffer-string))))))
      (advice-remove 'mevedel-instruction-store-sync counter)
      (dolist (buf (cl-set-difference (buffer-list) before))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
      (dolist (timer (append timer-list timer-idle-list nil))
        (unless (memq timer timers) (cancel-timer timer)))
      (mevedel-workspace-clear-registry)
      (setq mevedel-instruction-store--dirty nil)
      (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
      (mevedel-uninstall)
      (when transport-enabled (mevedel-transport-install))
      (delete-directory root t))))

(provide 'test-mevedel-plan-handoff-buddy)
;;; test-mevedel-plan-handoff-buddy.el ends here
