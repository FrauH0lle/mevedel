;;; test-mevedel-tool-ptc-mcp.el --- MCP calling integration -*- lexical-binding: t -*-

;;; Commentary:
;; Exercises the installed MCP text adapter over a real local stdio process.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tools)
(require 'mevedel-tool-ptc)
(require 'mevedel-ptc-driver)
(require 'mevedel-pipeline)
(require 'mcp)

(defconst test-mevedel-tool-ptc-mcp--server
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures/toolcall-mcp.py"))

(mevedel-deftest mevedel-tool-ptc-mcp-integration
  (:quiet t)
  ,test
  (test)
  :doc "discovers and calls a configured MCP tool with nested JSON through the real pipeline"
  (let ((mcp-server-connections (make-hash-table :test #'equal))
        (mcp-server-wait-initial-time 0)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
        (buffers-before (buffer-list))
        (name "toolcall-mcp-fixture")
        result done)
    (unwind-protect
        (progn
          (mcp-connect-server name :command "python3"
                              :args (list test-mevedel-tool-ptc-mcp--server)
                              :syncp t :timeout 3)
          (let* ((source (apply #'gptel-make-tool
                                :category name (mcp-make-text-tool name "Probe" t)))
                 (wrapped (mevedel-tool--register-wrap :source source :read-only-p t))
                 (call-name (mevedel-tool-call-name wrapped)))
            (with-temp-buffer
              (setq-local mevedel--session
                          (mevedel-session--create
                           :tool-catalog (list (cons (list (mevedel-tool-category wrapped) "Probe") "Echo nested JSON"))))
              (mevedel-tools--tool-search (lambda (value) (setq result value)) call-name)
              (should (string-search call-name result))
              (should (string-search "Standalone only" result))
              (cl-labels
                  ((call (label)
                     (setq done nil result nil)
                     (mevedel-ptc-driver-run
                      (lambda (value) (setq result value done t))
                      (format "(%s :payload '(:flag :json-false :label %S))" call-name label)
                      (list call-name) (list call-name))
                     (let ((deadline (+ (float-time) 3)))
                       (while (and (not done) (< (float-time) deadline))
                         (accept-process-output nil 0.01)))
                     (should done)))
                (call "echo")
                (should (eq 'success (plist-get result :status)))
                (should (equal '(:payload (:flag :json-false :label "echo"))
                               (json-parse-string (plist-get result :result)
                                                  :object-type 'plist :false-object :json-false)))
                (call "fail")
                (should (eq 'error (plist-get result :status)))
                (should (string-search "fixture failure" (plist-get result :result)))
                (mcp-stop-server name)
                (call "echo")
                (should (eq 'error (plist-get result :status)))
                (mcp-connect-server name :command "python3"
                                    :args (list test-mevedel-tool-ptc-mcp--server)
                                    :syncp t :timeout 3)
                (apply #'gptel-make-tool :category name (mcp-make-text-tool name "Probe" t))
                (call "echo")
                (should (eq 'success (plist-get result :status))))
              ;; A disconnected registration fails clearly; it cannot silently
              ;; resolve a same-named tool from another server.
              (setf (alist-get "Probe" (alist-get name gptel--known-tools nil nil #'equal)
                               nil t #'equal) nil)
              (funcall (mevedel-tool-handler wrapped) (lambda (value) (setq result value))
                       '(:payload (:flag t :label "echo")))
              (should (string-search "unregistered" (plist-get result :result))))))
      (mcp-stop-server name)
      (dolist (buffer (cl-set-difference (buffer-list) buffers-before))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'test-mevedel-tool-ptc-mcp)
;;; test-mevedel-tool-ptc-mcp.el ends here
