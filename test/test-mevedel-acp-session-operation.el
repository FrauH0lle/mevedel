;;; test-mevedel-acp-session-operation.el --- Native fork control tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise control operations through the stdio peer and persisted histories.

;;; Code:

(require 'mevedel-acp)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(mevedel-deftest mevedel-acp-session-operation ()
  ,test
  (test)
  :doc "fork and delete preserve parent history without creating or prompting a session"
  (let* ((directory (make-temp-file "acp-fork-" t))
         (process-environment (copy-sequence process-environment))
         (parent (file-name-concat directory "fixture-session.json"))
         (fork (file-name-concat directory "fixture-fork.json"))
         (trace (file-name-concat directory "trace.jsonl"))
         (launch (list :command (executable-find "python3")
                       :args (list mevedel-test--acp-peer) :cwd directory))
         outcome connection)
    (unwind-protect
        (progn
          (setenv "MEVEDEL_ACP_TEST_HISTORY" directory)
          (setenv "MEVEDEL_ACP_TEST_TRACE" trace)
          (with-temp-file parent (insert "[\"full native history\"]"))
          (setq connection
                (mevedel-acp-session-operation
                 launch 'fork "fixture-session" (lambda (value) (setq outcome value))))
          (mevedel-test--await 10 "Fork reply" outcome)
          (should (eq 'success (plist-get outcome :status)))
          (should (equal "fixture-fork" (plist-get outcome :session-id)))
          (should (file-exists-p fork))
          (should (equal (with-temp-buffer (insert-file-contents parent) (buffer-string))
                         (with-temp-buffer (insert-file-contents fork) (buffer-string))))
          (setq outcome nil connection
                (mevedel-acp-session-operation
                 launch 'delete "fixture-fork" (lambda (value) (setq outcome value))))
          (mevedel-test--await 10 "Delete reply" outcome)
          (should (eq 'success (plist-get outcome :status)))
          (should-not (file-exists-p fork))
          (should (file-exists-p parent))
          (let ((requests (with-temp-buffer
                            (insert-file-contents trace)
                            (mapcar (lambda (line)
                                      (json-parse-string line :object-type 'alist))
                                    (split-string (buffer-string) "\n" t)))))
            (should (equal '("initialize" "session/fork" "initialize" "session/delete")
                           (mapcar (lambda (entry) (alist-get 'method entry)) requests)))
            (should (equal "fixture-fork"
                           (alist-get 'sessionId (alist-get 'params (car (last requests))))))))
      (when connection (mevedel-acp-close connection))
      (delete-directory directory t)))

  :doc "missing capabilities and invalid fork identities refuse without deleting the parent"
  (dolist (case '(("fork" nil) ("resume" nil) ("delete" nil)
                  (nil "fixture-session") (nil "")))
    (let* ((directory (make-temp-file "acp-fork-refusal-" t))
           (process-environment (copy-sequence process-environment))
           (parent (file-name-concat directory "fixture-session.json"))
           outcome connection)
      (unwind-protect
          (progn
            (setenv "MEVEDEL_ACP_TEST_HISTORY" directory)
            (setenv "MEVEDEL_ACP_TEST_MISSING_CAP" (car case))
            (setenv "MEVEDEL_ACP_TEST_FORK_ID" (cadr case))
            (with-temp-file parent (insert "[\"parent\"]"))
            (setq connection
                  (mevedel-acp-session-operation
                   (list :command (executable-find "python3")
                         :args (list mevedel-test--acp-peer) :cwd directory)
                   'fork "fixture-session" (lambda (value) (setq outcome value))))
            (mevedel-test--await 10 "Refusal reply" outcome)
            (should (eq 'error (plist-get outcome :status)))
            (should (string-match-p "capabilit\\|identity" (plist-get outcome :message)))
            (should (equal "[\"parent\"]"
                           (with-temp-buffer (insert-file-contents parent) (buffer-string)))))
        (when connection (mevedel-acp-close connection))
        (delete-directory directory t)))))

(provide 'test-mevedel-acp-session-operation)
;;; test-mevedel-acp-session-operation.el ends here
