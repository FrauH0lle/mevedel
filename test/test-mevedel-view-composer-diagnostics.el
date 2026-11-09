;;; test-mevedel-view-composer-diagnostics.el --- View diagnostic ownership -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise command refusals and deferred failure presentations with explicit
;; workspace ownership, including a different current buffer at completion.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-audit)
(require 'mevedel-view-composer)
(require 'mevedel-view-control-transfer)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-execution)
(require 'mevedel-view-input-files)
(require 'mevedel-view-interaction)
(require 'mevedel-view-markdown)
(require 'mevedel-view-segments)
(require 'mevedel-view-zone)
(require 'mevedel-execution-spool)

(mevedel-deftest mevedel-view--transcript-gptel-send-blocked/diagnostics ()
  ,test
  (test)
  :doc "each command owner attributes refusals without changing the condition type"
  (with-temp-buffer
    (setq-local mevedel--workspace
                (mevedel-workspace--create :root "/tmp/worktree % project/"))
    (insert "plain text")
    (goto-char (point-min))
    (dolist (command '((mevedel-view--transcript-gptel-send-blocked)
                       (mevedel-view-disclosure--toggle-mailbox)
                       (mevedel-view-execution--current different-session nil)
                       (mevedel-view-jump-to-interaction)
                       (mevedel-view--visit-resource "not-an-address")
                       (mevedel-view-segments--prompt-entry nil nil)
                       (mevedel-view-control-transfer-grant)
                       (mevedel-view-audit-show-result nil)
                       (mevedel-view--media-dir)
                       (mevedel-view-zone-toggle-collapsed)))
      (let ((err (should-error (apply (car command) (cdr command)) :type 'user-error)))
        (should (string-prefix-p "mevedel [/tmp/worktree % project/]: " (cadr err)))
        (should (= 1 (mevedel-view-test--count-substring "mevedel [" (cadr err))))))))

(mevedel-deftest mevedel-view--finish-fork-skill-outcome/diagnostics ()
  ,test
  (test)
  :doc "a skill finishing while another workspace is current reports its data owner"
  (let ((origin (generate-new-buffer " *diagnostic-origin*"))
        (view (generate-new-buffer " *diagnostic-view*")) messages)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local mevedel--workspace (mevedel-workspace--create :root "/tmp/origin/")))
          (with-temp-buffer
            (setq-local mevedel--workspace (mevedel-workspace--create :root "/tmp/other/"))
            (mevedel-test--with-captured-messages messages
              (mevedel-view--finish-fork-skill-outcome
               "test" '(:status ok :kind unsupported) view origin nil))
            (should (string-prefix-p "mevedel [/tmp/origin/]: " messages))))
      (kill-buffer origin)
      (kill-buffer view))))

(mevedel-deftest mevedel-view-execution--full-output/diagnostics ()
  ,test
  (test)
  :doc "a delayed retained-output failure keeps its root after the origin dies"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create
                     :workspace (mevedel-workspace--create :root "/tmp/output-origin/")
                     :root-buffer data-buf))
           (snapshot (list :session session :owner "/root" :execution-id "missing"
                           :state 'lost :terminal-p t :output-bytes 10))
           (spool (make-temp-file "diagnostic-output-" nil ".log.gz"))
           done report)
      (unwind-protect
          (progn
            (with-current-buffer data-buf (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq snapshot (plist-put snapshot :artifact-path spool))
              (setq mevedel-view-execution--selected snapshot)
              (cl-letf (((symbol-function 'mevedel-execution-spool-with-path)
                         (lambda (_path _operation callback _executor &optional _max)
                           (setq done callback)))
                        ((symbol-function 'display-buffer)
                         (lambda (buffer &rest _) (setq report buffer))))
                (mevedel-view-execution--full-output
                 session (plist-put (copy-sequence snapshot) :artifact-path spool))
                (should done)
                (kill-buffer data-buf)
                (with-temp-buffer
                  (setq-local mevedel--workspace (mevedel-workspace--create :root "/tmp/other/"))
                  (funcall done '(:status error :result "mevedel [/tmp/old/]: Read failed")))
                (with-current-buffer report
                  (should (string-search "mevedel [/tmp/output-origin/]: Read failed" (buffer-string)))
                  (should (string-search "mevedel [/tmp/output-origin/]: Full output unavailable." (buffer-string)))
                  (should-not (string-search "/tmp/other/" (buffer-string)))
                  (should-not (string-search "/tmp/old/" (buffer-string)))))))
        (when (buffer-live-p report) (kill-buffer report))
        (delete-file spool)))))

(mevedel-deftest mevedel-view--block-planned-submission/diagnostics ()
  ,test
  (test)
  :doc "a caught preparation failure is reprojected once rather than wrapped"
  (with-temp-buffer
    (setq-local mevedel--workspace (mevedel-workspace--create :root "/tmp/preparation/"))
    (let (messages)
      (mevedel-test--with-captured-messages messages
        (mevedel-view--block-planned-submission
         (list :data-buffer (current-buffer))
         '(:name "example" :message "mevedel [/tmp/old/]: Expansion failed")))
      (should (string-prefix-p "mevedel [/tmp/preparation/]: Expansion failed" messages))
      (should (= 1 (mevedel-view-test--count-substring "mevedel [" messages))))))

(provide 'test-mevedel-view-composer-diagnostics)
;;; test-mevedel-view-composer-diagnostics.el ends here
