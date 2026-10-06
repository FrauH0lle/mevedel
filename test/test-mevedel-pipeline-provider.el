;;; test-mevedel-pipeline-provider.el --- Structured provider outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; External providers consume the same persisted results and status as gptel.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))
(require 'mevedel-tools)

(mevedel-deftest mevedel-pipeline-run-tool-outcome/provider (:quiet t)
  (let* ((root (make-temp-file "mevedel-provider-outcome-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "provider" workspace))
         (buffer (generate-new-buffer " *provider-pipeline*"))
         (text (make-string (* 128 1024) ?x))
         (tool (mevedel-tool--create
                :name "ProviderProbe" :category "mevedel"
                :handler (lambda (_args)
                           (list :result text :status 'success
                                 :render-data '(:kind probe)))
                :description "Test result" :prompt "Test result"
                :read-only-p t :max-result-size 30000))
         delivered)
    (unwind-protect
        (with-current-buffer buffer
          (mevedel-chat-prepare-transcript-buffer)
          (setq-local mevedel--session session mevedel--workspace workspace
                      default-directory (file-name-as-directory root))
          (setf (mevedel-session-permission-mode session) 'full-auto)
          (mevedel-session-set-root-buffer session buffer)
          (mevedel-session-artifacts-ensure-files session buffer)
          (mevedel-pipeline-run-tool-outcome
           tool (lambda (outcome) (setq delivered outcome)) nil
           '(:tool-use-id "toolu_provider" :source mcp :projection provider))
          (with-timeout (3 (ert-fail "Tool completion was not delivered"))
            (while (not delivered) (accept-process-output nil 0.01)))
          (should (eq 'success (plist-get delivered :status)))
          (should (equal "toolu_provider" (plist-get delivered :tool-use-id)))
          (should (equal text (plist-get delivered :raw-result)))
          (should (string-prefix-p "<persisted-output>"
                                   (plist-get delivered :result)))
          (should (< (length (plist-get delivered :result)) 30000))
          (should (eq 'persisted
                      (plist-get (plist-get (plist-get delivered :render-data)
                                           :output-accounting) :disposition)))
          (let ((result (plist-get delivered :result)))
            (should (string-match "artifact://\\([^\" \n]+\\)" result))
            (should (equal text (mevedel-session-artifacts-read-artifact
                                session (file-name-concat
                                         "tool-results"
                                         (match-string 1 result)))))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (mevedel-workspace-clear-registry)
      (delete-directory root t))))

(provide 'test-mevedel-pipeline-provider)
;;; test-mevedel-pipeline-provider.el ends here
