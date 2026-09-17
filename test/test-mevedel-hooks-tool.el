;;; test-mevedel-hooks-tool.el -- Tool hook context tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tool hook context ordering and dispatch-buffer ownership.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-hooks)

(mevedel-deftest mevedel-hooks-record-tool-context ()
  (let* ((context (list :args '(:text "input")
                        :hook-additional-context
                        '((:event "PreToolUse" :body "earlier"))))
         (updated (mevedel-hooks-record-tool-context
                   context '(:additional-context ("later" "later"))
                   'PermissionRequest)))
    (should (equal '(:text "input") (plist-get updated :args)))
    (should (equal '((:event "PreToolUse" :body "earlier")
                     (:event "PermissionRequest" :body "later")
                     (:event "PermissionRequest" :body "later"))
                   (plist-get updated :hook-additional-context)))
    (should (eq updated (mevedel-hooks-record-tool-context updated nil)))))

(mevedel-deftest mevedel-hooks-record-tool-audit ()
  (let* ((context (list :args '(:text "input")
                        :hook-audit-records '((:event "earlier"))))
         (updated (mevedel-hooks-record-tool-audit
                   context '(:event "single")))
         (updated (mevedel-hooks-record-tool-audit
                   updated '((:event "first") (:event "second")))))
    (should (equal '((:event "earlier") (:event "single")
                     (:event "first") (:event "second"))
                   (plist-get updated :hook-audit-records)))
    (should (equal '(:text "input") (plist-get updated :args)))
    (should (eq updated (mevedel-hooks-record-tool-audit updated nil)))))

(mevedel-deftest mevedel-hooks-tool-permission-audit-record ()
  (let ((decision '(:permission-reason "policy" :system-message "notice")))
    (should
     (equal '(:type tool-permission :event "PermissionRequest"
                    :outcome "deny" :reason "specific")
            (mevedel-hooks-tool-permission-audit-record
             'PermissionRequest 'deny decision "specific")))
    (should
     (equal '(:type tool-permission :event "PreToolUse"
                    :outcome "ask" :reason "policy")
            (mevedel-hooks-tool-permission-audit-record
             'PreToolUse 'ask decision)))
    (should-not
     (plist-member (mevedel-hooks-tool-permission-audit-record
                    'PermissionRequest 'allow nil)
                   :reason))))

(mevedel-deftest mevedel-hooks-run-tool-event ()
  (let ((session (mevedel-session--create :name "tool-hook"))
        (owner (generate-new-buffer " *tool-hook-owner*"))
        observed)
    (unwind-protect
        (progn
          (with-current-buffer owner
            (setq-local mevedel-permission-request-functions
                        (list (lambda (payload)
                                (push (list 'handler (current-buffer)
                                            (plist-get payload :tool-name))
                                      observed)
                                '(:additional-context "owner policy")))))
          (with-temp-buffer
            (mevedel-hooks-run-tool-event
             'PermissionRequest '(:tool-name "Probe")
             (lambda (decision)
               (push (list 'callback (current-buffer)
                           (plist-get decision :additional-context))
                     observed))
             (list :buffer owner) session nil nil nil))
          (should (equal (list (list 'callback owner '("owner policy"))
                               (list 'handler owner "Probe"))
                         observed))
          (kill-buffer owner)
          (with-temp-buffer
            (let ((fallback (current-buffer)) called)
              (mevedel-hooks-run-tool-event
               'PermissionRequest '(:tool-name "Probe")
               (lambda (_decision) (setq called (current-buffer)))
               (list :buffer owner) session nil nil nil)
              (should (eq fallback called)))))
      (when (buffer-live-p owner) (kill-buffer owner)))))

(provide 'test-mevedel-hooks-tool)
;;; test-mevedel-hooks-tool.el ends here
