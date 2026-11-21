;;; test-niwa-view-composer-diagnostics.el --- View diagnostic ownership -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise command refusals and deferred failure presentations with explicit
;; workspace ownership, including a different current buffer at completion.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-audit)
(require 'niwa-view-composer)
(require 'niwa-view-control-transfer)
(require 'niwa-view-disclosure)
(require 'niwa-view-execution)
(require 'niwa-view-input-files)
(require 'niwa-view-interaction)
(require 'niwa-view-markdown)
(require 'niwa-view-segments)
(require 'niwa-view-zone)
(require 'niwa-execution-spool)

(niwa-deftest niwa-view--transcript-gptel-send-blocked/diagnostics ()
  ,test
  (test)
  :doc "each command owner attributes refusals without changing the condition type"
  (with-temp-buffer
    (setq-local niwa--workspace
                (niwa-workspace--create :root "/tmp/worktree % project/"))
    (insert "plain text")
    (goto-char (point-min))
    (dolist (command '((niwa-view--transcript-gptel-send-blocked)
                       (niwa-view-disclosure--toggle-mailbox)
                       (niwa-view-execution--current different-session nil)
                       (niwa-view-jump-to-interaction)
                       (niwa-view--visit-resource "not-an-address")
                       (niwa-view-segments--prompt-entry nil nil)
                       (niwa-view-control-transfer-grant)
                       (niwa-view-audit-show-result nil)
                       (niwa-view--media-dir)
                       (niwa-view-zone-toggle-collapsed)))
      (let ((err (should-error (apply (car command) (cdr command)) :type 'user-error)))
        (should (string-prefix-p "niwa [/tmp/worktree % project/]: " (cadr err)))
        (should (= 1 (niwa-view-test--count-substring "niwa [" (cadr err))))))))

(niwa-deftest niwa-view--finish-fork-skill-outcome/diagnostics ()
  ,test
  (test)
  :doc "a skill finishing while another workspace is current reports its data owner"
  (let ((origin (generate-new-buffer " *diagnostic-origin*"))
        (view (generate-new-buffer " *diagnostic-view*")) messages)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local niwa--workspace (niwa-workspace--create :root "/tmp/origin/")))
          (with-temp-buffer
            (setq-local niwa--workspace (niwa-workspace--create :root "/tmp/other/"))
            (niwa-test--with-captured-messages messages
              (niwa-view--finish-fork-skill-outcome
               "test" '(:status ok :kind unsupported) view origin nil))
            (should (string-prefix-p "niwa [/tmp/origin/]: " messages))))
      (kill-buffer origin)
      (kill-buffer view))))

(niwa-deftest niwa-view-execution--full-output/diagnostics ()
  ,test
  (test)
  :doc "a delayed retained-output failure keeps its root after the origin dies"
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create
                     :workspace (niwa-workspace--create :root "/tmp/output-origin/")
                     :root-buffer data-buf))
           (snapshot (list :session session :owner "/root" :execution-id "missing"
                           :state 'lost :terminal-p t :output-bytes 10))
           (spool (make-temp-file "diagnostic-output-" nil ".log.gz"))
           done report)
      (unwind-protect
          (progn
            (with-current-buffer data-buf (setq-local niwa--session session))
            (with-current-buffer view-buf
              (setq snapshot (plist-put snapshot :artifact-path spool))
              (setq niwa-view-execution--selected snapshot)
              (cl-letf (((symbol-function 'niwa-execution-spool-with-path)
                         (lambda (_path _operation callback _executor &optional _max)
                           (setq done callback)))
                        ((symbol-function 'display-buffer)
                         (lambda (buffer &rest _) (setq report buffer))))
                (niwa-view-execution--full-output
                 session (plist-put (copy-sequence snapshot) :artifact-path spool))
                (should done)
                (kill-buffer data-buf)
                (with-temp-buffer
                  (setq-local niwa--workspace (niwa-workspace--create :root "/tmp/other/"))
                  (funcall done '(:status error :result "niwa [/tmp/old/]: Read failed")))
                (with-current-buffer report
                  (should (string-search "niwa [/tmp/output-origin/]: Read failed" (buffer-string)))
                  (should (string-search "niwa [/tmp/output-origin/]: Full output unavailable." (buffer-string)))
                  (should-not (string-search "/tmp/other/" (buffer-string)))
                  (should-not (string-search "/tmp/old/" (buffer-string)))))))
        (when (buffer-live-p report) (kill-buffer report))
        (delete-file spool)))))

(niwa-deftest niwa-view--block-planned-submission/diagnostics ()
  ,test
  (test)
  :doc "a caught preparation failure is reprojected once rather than wrapped"
  (with-temp-buffer
    (setq-local niwa--workspace (niwa-workspace--create :root "/tmp/preparation/"))
    (let (messages)
      (niwa-test--with-captured-messages messages
        (niwa-view--block-planned-submission
         (list :data-buffer (current-buffer))
         '(:name "example" :message "niwa [/tmp/old/]: Expansion failed")))
      (should (string-prefix-p "niwa [/tmp/preparation/]: Expansion failed" messages))
      (should (= 1 (niwa-view-test--count-substring "niwa [" messages))))))

(provide 'test-niwa-view-composer-diagnostics)
;;; test-niwa-view-composer-diagnostics.el ends here
