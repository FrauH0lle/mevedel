;;; test-mevedel-permissions-diagnostics.el --- Permission attribution -*- lexical-binding: t -*-

;;; Commentary:

;; Permission failures retain their explicit owner across buffer changes.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-permission-log)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-permissions)
(require 'mevedel-utilities)

(mevedel-deftest mevedel-permission-log--persist-content-diagnostic
  (:doc "a failed append reports the session workspace, not the current buffer")
  (let* ((root (make-temp-file "mevedel-permission-diagnostic-" t))
         (blocker (file-name-concat root "blocker"))
         (session (mevedel-session--create
                   :workspace (mevedel-workspace--create :root root)
                   :save-path (file-name-concat blocker "session")))
         (mevedel-permission-log-enabled t)
         text)
    (unwind-protect
        (progn
          (write-region "file" nil blocker nil 'silent)
          (with-temp-buffer
            (setq-local mevedel--session
                        (mevedel-session--create
                         :workspace (mevedel-workspace--create :root "/other")))
            (mevedel-test--with-captured-messages text
              (should-not (mevedel-permission-log--persist-content session "data")))
            (should (string-prefix-p
                     (format "mevedel [%s]: Permission log persistence failed:" root)
                     text))))
      (delete-directory root t))))

(mevedel-deftest mevedel-permission--run-async ()
  ,test
  (test)
  :doc "callback failures retain captured ownership after the origin dies"
  (let ((origin (generate-new-buffer " *permission-origin*")) callback text)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local mevedel--session
                        (mevedel-session--create
                         :workspace (mevedel-workspace--create :root "/origin")))
            (mevedel-permission--run-async
             (mevedel-diagnostic-scope origin)
             (lambda (finish _guard) (setq callback finish))
             (lambda (_) (error "Injected callback failure"))))
          (kill-buffer origin)
          (with-temp-buffer
            (setq-local mevedel--session
                        (mevedel-session--create
                         :workspace (mevedel-workspace--create :root "/other")))
            (mevedel-test--with-captured-messages text
              (funcall callback 'allow))
            (should (string-prefix-p
                     "mevedel [/origin]: Permission callback failed:" text))))
      (when (buffer-live-p origin) (kill-buffer origin))))

  :doc "an attributed command refusal is rendered once without mutating its condition"
  (let ((condition (list 'user-error
                         (mevedel-diagnostic-format "/origin" "Permission store unavailable")))
        text)
    (mevedel-test--with-captured-messages text
      (mevedel-permission--run-async
       "/origin" (lambda (finish _guard) (funcall finish 'allow))
       (lambda (_) (signal (car condition) (cdr condition)))))
    (should (equal "mevedel [/origin]: Permission store unavailable" (string-trim text)))
    (should (equal (list 'user-error
                         (concat "mevedel " "[/origin]: Permission store unavailable"))
                   condition))))

(mevedel-deftest mevedel-permission--prompt-finish
  (:doc "local command refusal attributes a raw shared validation condition")
  (with-temp-buffer
    (insert "card")
    (goto-char 1)
    (let ((ov (make-overlay 1 5)))
      (overlay-put ov 'mevedel-permission-prompt t)
      (overlay-put ov 'mevedel-view-interaction-entry
                   (list :session (mevedel-session--create
                                   :workspace (mevedel-workspace--create
                                               :root "/origin"))))
      (cl-letf (((symbol-function 'mevedel-permission-queue-validate-approval)
                 (lambda (&rest _) (user-error "Select directory-tree scope"))))
        (should (equal (list 'user-error
                             (concat "mevedel " "[/origin]: Select directory-tree scope"))
                       (should-error (mevedel-permission--prompt-finish 'allow-once)
                                     :type 'user-error)))))))

(provide 'test-mevedel-permissions-diagnostics)
;;; test-mevedel-permissions-diagnostics.el ends here
