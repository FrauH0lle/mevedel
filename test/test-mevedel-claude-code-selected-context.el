;;; test-mevedel-claude-code-selected-context.el --- Selected native context -*- lexical-binding: t -*-

;;; Commentary:
;; Select real text sources and submit them through the ordinary session view.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(defconst mevedel-claude-code-selected-context-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-view-send/claude-selected-context (:quiet t)
  (dolist (placement '(user system))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (let* ((view (generate-new-buffer " *claude-context-composer*"))
             (source (generate-new-buffer " *selected-source*"))
             (gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (file (file-name-concat root "selected.txt"))
             launch-system overlay)
        (unwind-protect
            (progn
              (write-region "SELECTED FILE EVIDENCE" nil file nil 'silent)
              (with-current-buffer source
                (insert "PRIVATE BEFORE\n")
                (let ((start (point)))
                  (insert "SELECTED BUFFER EVIDENCE")
                  (setq overlay (make-overlay start (point) source)))
                (insert "\nPRIVATE AFTER\n"))
              (mevedel-claude-code-register)
              (mevedel-model-set-session-provider
               session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
              (setq-local gptel-system-prompt "Context fixture" gptel-tools nil
                          gptel-use-context placement
                          gptel-context (list (list source :overlays (list overlay))
                                             (list file :mime "text/plain")))
              (mevedel-chat-install-request-hooks)
              (mevedel-view--setup view buffer)
              (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                         (lambda (system mcp _model _effort &optional id _hook)
                           (setq launch-system system)
                           (list :command (executable-find "python3")
                                 :args (list mevedel-claude-code-selected-context-test--peer)
                                 :cwd root :mcp mcp :session-id id
                                 :meta '((echoAllText . t))
                                 :observe #'mevedel-claude-code-context-observe
                                 :check-context #'mevedel-claude-code-context-check))))
                (with-current-buffer view
                  (goto-char (mevedel-view--input-start))
                  (insert "Describe the selected evidence")
                  (mevedel-view-send))
                (with-timeout (5 (ert-fail "Selected context turn did not settle"))
                  (while (or (zerop (mevedel-session-turn-count session))
                             (mevedel-turn-busy-p buffer))
                    (accept-process-output nil 0.01))))
              (let ((delivered (if (eq placement 'system) launch-system (buffer-string))))
                (should (string-search "SELECTED FILE EVIDENCE" delivered))
                (should (string-search "SELECTED BUFFER EVIDENCE" delivered))
                (should-not (string-search "PRIVATE BEFORE" delivered))
                (should-not (string-search "PRIVATE AFTER" delivered)))
              (should (equal "Context fixture" gptel-system-prompt)))
          (when (buffer-live-p source) (kill-buffer source))
          (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel-view-send/claude-async-context (:quiet t)
  (pcase-dolist (`(,placement ,abort-p) '((user nil) (system nil) (system t) (user invalid)))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (let* ((view (generate-new-buffer " *claude-async-context*"))
             (gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (file (file-name-concat root "selected.txt"))
             (launches 0) resume launch-system)
        (unwind-protect
            (progn
              (write-region "ASYNC SELECTED EVIDENCE" nil file nil 'silent)
              (mevedel-claude-code-register)
              (mevedel-model-set-session-provider
               session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
              (setq-local gptel-system-prompt (lambda () (format "Async fixture %d" (length gptel-tools)))
                          gptel-tools nil
                          gptel-use-context placement gptel-context (list file)
                          gptel-context-string-function
                          (lambda (callback sources)
                            (setq resume (lambda () (funcall callback (if (eq abort-p 'invalid) 42
                                                                    (gptel-context--string sources)))))))
              (mevedel-chat-install-request-hooks)
              (mevedel-view--setup view buffer)
              (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                         (lambda (system mcp _model _effort &optional id _hook)
                           (cl-incf launches)
                           (setq launch-system system)
                           (list :command (executable-find "python3")
                                 :args (list mevedel-claude-code-selected-context-test--peer)
                                 :cwd root :mcp mcp :session-id id :meta '((echoAllText . t))
                                 :observe #'mevedel-claude-code-context-observe
                                 :check-context #'mevedel-claude-code-context-check))))
                (with-current-buffer view
                  (goto-char (mevedel-view--input-start))
                  (insert "Use the selected context")
                  (mevedel-view-send)
                  (goto-char (mevedel-view--input-start))
                  (insert "> Keep this draft\nand its second line"))
                (should (functionp resume))
                (should (zerop launches))
                (should (mevedel-turn-busy-p buffer))
                (setq-local gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
                (when (eq abort-p t) (mevedel-abort buffer))
                ;; A formatter can complete in a different buffer, and a
                ;; duplicate or late callback must not dispatch another turn.
                (with-temp-buffer (funcall resume) (funcall resume))
                (with-timeout (5 (ert-fail "Async context turn did not settle"))
                  (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
              (should (= (if abort-p 0 1) launches))
              (when (eq abort-p 'invalid)
                (should (string-search "Context formatter must return text" (buffer-string))))
              (should (= 1 (mevedel-session-turn-count session)))
              (unless abort-p
                (should (string-search "Async fixture 0" launch-system))
                (should (string-search "ASYNC SELECTED EVIDENCE"
                                       (if (eq placement 'system) launch-system (buffer-string)))))
              (with-current-buffer view
                (should (equal "> Keep this draft\nand its second line" (mevedel-view--input-text)))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(mevedel-deftest mevedel--send-request/claude-selected-context-restoration (:quiet t)
  (dolist (receipt '(t nil))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (file (file-name-concat root "selected.txt")))
        (write-region "RESTORE SELECTED EVIDENCE" nil file nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Restore fixture" gptel-use-context 'user
                    gptel-context (list file)
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (_system mcp _model _effort &optional id hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-selected-context-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :tool-id-field :claudecode/toolUseId
                           :control #'mevedel-claude-code--control
                           :observe #'mevedel-claude-code-context-observe
                           :check-context #'mevedel-claude-code-context-check
                           :meta `((hookCommand . ,hook) (compactBeforeBatch . 0)
                                   (compactAcknowledgement . ,(if receipt t :false))
                                   (responseText . "Completed")
                                   (toolBatches . [[((name . "Read") (id . "read-selected")
                                                    (args . ((file_path . ,file))))]]))))))
          (insert "Use the selected evidence")
          (mevedel--send-request "Use the selected evidence")
          (with-timeout (5 (ert-fail "Selected context restoration did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
        (should (eq (if receipt 'success 'error)
                    (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
        (should (= (if receipt 1 0)
                   (how-many "mevedel-restoration:" (point-min) (point-max))))
        (should (= (if receipt 1 0) (how-many "^#\\+begin_tool" (point-min) (point-max))))
        (when receipt
          (let ((text (buffer-string)))
            (should (< (string-match "RESTORE SELECTED EVIDENCE" text)
                       (string-match "^#\\+begin_tool" text)))))))))

(provide 'test-mevedel-claude-code-selected-context)
;;; test-mevedel-claude-code-selected-context.el ends here
