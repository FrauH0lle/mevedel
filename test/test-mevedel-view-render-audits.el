;;; test-mevedel-view-render-audits.el --- Activity audit projection -*- lexical-binding: t -*-

;;; Commentary:

;; Standalone audits between deliveries and tools keep their own semantics.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-audit)
(require 'mevedel-view-agent)
(require 'mevedel-transcript)
(require 'mevedel-tool-registry)

(mevedel-deftest mevedel-view--render-assistant-turn ()
  ,test
  (test)
  :doc "standalone audits after deliveries never become tool disclosures"
  (dolist (threshold '(1 100))
    (dolist (record '((:type provider-tool-batch-start :id "next")
                      (:type tool-result-rewrite :event "PostToolUse"
                       :original-result "raw" :updated-result "updated")))
      (with-temp-buffer
        (org-mode)
        (insert "#+begin_tool (Bash)\n"
                (propertize "(:name \"Bash\" :args nil)\n\nDone\n"
                            'gptel '(tool . "previous"))
                "#+end_tool\n"
                "<agent-message type=\"EXECUTION\" sender=\"/root\" recipient=\"/root\">\n"
                "Ready.\n</agent-message>\n"
                (mevedel--format-hook-audit-record record)
                "#+begin_tool (Read)\n"
                (propertize "(:name \"Read\" :args (:file_path \"file.el\"))\n\nContents\n"
                            'gptel '(tool . "next"))
                "#+end_tool\n")
        (let ((data (current-buffer))
              (segments (mevedel-transcript-segments (point-min) (point-max)))
              (mevedel-view-tool-group-collapse-threshold threshold))
          (should (equal '(tool mailbox ignored tool) (mapcar #'car segments)))
          (with-temp-buffer
            (let ((inhibit-read-only t))
              (mevedel-view--render-assistant-turn segments data))
            (let ((text (buffer-string)))
              (should-not (string-match-p "Tool (" text))
              (should-not (string-match-p "mevedel-hook-audit" text))
              (when (eq (plist-get record :type) 'tool-result-rewrite)
                (should (string-match-p "hook changed tool result" text))))))))))

(provide 'test-mevedel-view-render-audits)
;;; test-mevedel-view-render-audits.el ends here
