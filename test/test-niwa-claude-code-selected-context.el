;;; test-niwa-claude-code-selected-context.el --- Selected native context -*- lexical-binding: t -*-

;;; Commentary:
;; Select real text sources and submit them through the ordinary session view.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)

(niwa-deftest niwa-view-send/claude-selected-context (:quiet t)
  (dolist (placement '(user system))
    (niwa-engine-test--with-claude-session
      (niwa-request-end)
      (let* ((view (generate-new-buffer " *claude-context-composer*"))
             (source (generate-new-buffer " *selected-source*"))
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
              (setq-local gptel-system-prompt "Context fixture" gptel-tools nil
                          gptel-use-context placement
                          gptel-context (list (list source :overlays (list overlay))
                                             (list file :mime "text/plain")))
              (niwa-chat-install-request-hooks)
              (niwa-view--setup view buffer)
              (cl-letf (((symbol-function 'niwa-claude-code-launch)
                         (niwa-engine-test--claude-launch
                          (lambda (system mcp _model _effort &optional id _hook)
                            (setq launch-system system)
                            (list :meta '((echoAllText . t)))))))
                (with-current-buffer view
                  (niwa-view-composer-insert "Describe the selected evidence")
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send)))
                (niwa-test--await 5 "Selected context turn did not settle"
                  (not (or (zerop (niwa-session-turn-count session))
                           (niwa-turn-busy-p buffer)))))
              (let ((delivered (if (eq placement 'system) launch-system (buffer-string))))
                (should (string-search "SELECTED FILE EVIDENCE" delivered))
                (should (string-search "SELECTED BUFFER EVIDENCE" delivered))
                (should-not (string-search "PRIVATE BEFORE" delivered))
                (should-not (string-search "PRIVATE AFTER" delivered)))
              (should (equal "Context fixture" gptel-system-prompt)))
          (when (buffer-live-p source) (kill-buffer source))
          (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa-view-send/claude-async-context (:quiet t)
  (pcase-dolist (`(,placement ,abort-p) '((user nil) (system nil) (system t) (user invalid)))
    (niwa-engine-test--with-claude-session
      (niwa-request-end)
      (let* ((view (generate-new-buffer " *claude-async-context*"))
             (file (file-name-concat root "selected.txt"))
             (launches 0) resume launch-system)
        (unwind-protect
            (progn
              (write-region "ASYNC SELECTED EVIDENCE" nil file nil 'silent)
              (setq-local gptel-system-prompt (lambda () (format "Async fixture %d" (length gptel-tools)))
                          gptel-tools nil
                          gptel-use-context placement gptel-context (list file)
                          gptel-context-string-function
                          (lambda (callback sources)
                            (setq resume (lambda () (funcall callback (if (eq abort-p 'invalid) 42
                                                                    (gptel-context--string sources)))))))
              (niwa-chat-install-request-hooks)
              (niwa-view--setup view buffer)
              (cl-letf (((symbol-function 'niwa-claude-code-launch)
                         (niwa-engine-test--claude-launch
                          (lambda (system mcp _model _effort &optional id _hook)
                            (cl-incf launches)
                            (setq launch-system system)
                            (list :meta '((echoAllText . t)))))))
                (with-current-buffer view
                  (niwa-view-composer-insert "Use the selected context")
                  (with-current-buffer (niwa-view-composer-buffer)
                    (niwa-view-send))
                  (niwa-view-composer-insert "> Keep this draft\nand its second line"))
                (should (functionp resume))
                (should (zerop launches))
                (should (niwa-turn-busy-p buffer))
                (setq-local gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
                (when (eq abort-p t) (niwa-abort buffer))
                ;; A formatter can complete in a different buffer, and a
                ;; duplicate or late callback must not dispatch another turn.
                (with-temp-buffer (funcall resume) (funcall resume))
                (niwa-test--await 5 "Async context turn did not settle"
                  (not (niwa-turn-busy-p buffer))))
              (should (= (if abort-p 0 1) launches))
              (when (eq abort-p 'invalid)
                (should (string-search "Context formatter must return text" (buffer-string))))
              (should (= 1 (niwa-session-turn-count session)))
              (unless abort-p
                (should (string-search "Async fixture 0" launch-system))
                (should (string-search "ASYNC SELECTED EVIDENCE"
                                       (if (eq placement 'system) launch-system (buffer-string)))))
              (with-current-buffer view
                (should (equal "> Keep this draft\nand its second line" (niwa-view-composer-draft-text)))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa--send-request/claude-selected-context-restoration (:quiet t)
  (dolist (receipt '(t nil))
    (niwa-engine-test--with-claude-session
      (let ((file (file-name-concat root "selected.txt")))
        (write-region "RESTORE SELECTED EVIDENCE" nil file nil 'silent)
        (setq-local gptel-system-prompt "Restore fixture" gptel-use-context 'user
                    gptel-context (list file)
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :meta `((hookCommand . ,hook) (compactBeforeBatch . 0)
                                    (compactAcknowledgement . ,(if receipt t :false))
                                    (responseText . "Completed")
                                    (toolBatches . [[((name . "Read") (id . "read-selected")
                                                      (args . ((file_path . ,file))))]])))))))
          (insert "Use the selected evidence")
          (niwa--send-request "Use the selected evidence")
          (niwa-test--await 5 "Selected context restoration did not settle"
            (not (niwa-turn-busy-p buffer))))
        (should (eq (if receipt 'success 'error)
                    (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
        ;; The wire marker stays out of the transcript; the typed record remains.
        (should-not (string-search "niwa-delivery:" (buffer-string)))
        (let ((spans (seq-filter
                      (lambda (span)
                        (seq-find (lambda (item) (eq 'selected-context (plist-get item :type)))
                                  (plist-get (plist-get span :record) :items)))
                      (niwa-transcript-audit-buffer-spans 'injected-reminders))))
          (should (= (if receipt 1 0) (length spans)))
          (should (= (if receipt 1 0) (how-many "^#\\+begin_tool" (point-min) (point-max))))
          (when receipt
            (should (string-search "RESTORE SELECTED EVIDENCE"
                                   (plist-get (seq-find (lambda (item) (eq 'selected-context (plist-get item :type)))
                                                        (plist-get (plist-get (car spans) :record) :items))
                                              :body)))
            (should (<= (plist-get (car spans) :end)
                        (1+ (string-match "^#\\+begin_tool" (buffer-string)))))))))))

(provide 'test-niwa-claude-code-selected-context)
;;; test-niwa-claude-code-selected-context.el ends here
