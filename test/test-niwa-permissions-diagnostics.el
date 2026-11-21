;;; test-niwa-permissions-diagnostics.el --- Permission attribution -*- lexical-binding: t -*-

;;; Commentary:

;; Permission failures retain their explicit owner across buffer changes.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-permission-log)
(require 'niwa-permission-prompt)
(require 'niwa-permission-queue)
(require 'niwa-permissions)
(require 'niwa-utilities)

(niwa-deftest niwa-permission-log--persist-content-diagnostic
  (:doc "a failed append reports the session workspace, not the current buffer")
  (let* ((root (make-temp-file "niwa-permission-diagnostic-" t))
         (blocker (file-name-concat root "blocker"))
         (session (niwa-session--create
                   :workspace (niwa-workspace--create :root root)
                   :save-path (file-name-concat blocker "session")))
         (niwa-permission-log-enabled t)
         text)
    (unwind-protect
        (progn
          (write-region "file" nil blocker nil 'silent)
          (with-temp-buffer
            (setq-local niwa--session
                        (niwa-session--create
                         :workspace (niwa-workspace--create :root "/other")))
            (niwa-test--with-captured-messages text
              (should-not (niwa-permission-log--persist-content session "data")))
            (should (string-prefix-p
                     (format "niwa [%s]: Permission log persistence failed:" root)
                     text))))
      (delete-directory root t))))

(niwa-deftest niwa-permission--run-async ()
  ,test
  (test)
  :doc "callback failures retain captured ownership after the origin dies"
  (let ((origin (generate-new-buffer " *permission-origin*")) callback text)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local niwa--session
                        (niwa-session--create
                         :workspace (niwa-workspace--create :root "/origin")))
            (niwa-permission--run-async
             (niwa-diagnostic-scope origin)
             (lambda (finish _guard) (setq callback finish))
             (lambda (_) (error "Injected callback failure"))))
          (kill-buffer origin)
          (with-temp-buffer
            (setq-local niwa--session
                        (niwa-session--create
                         :workspace (niwa-workspace--create :root "/other")))
            (niwa-test--with-captured-messages text
              (funcall callback 'allow))
            (should (string-prefix-p
                     "niwa [/origin]: Permission callback failed:" text))))
      (when (buffer-live-p origin) (kill-buffer origin))))

  :doc "an attributed command refusal is rendered once without mutating its condition"
  (let ((condition (list 'user-error
                         (niwa-diagnostic-format "/origin" "Permission store unavailable")))
        text)
    (niwa-test--with-captured-messages text
      (niwa-permission--run-async
       "/origin" (lambda (finish _guard) (funcall finish 'allow))
       (lambda (_) (signal (car condition) (cdr condition)))))
    (should (equal "niwa [/origin]: Permission store unavailable" (string-trim text)))
    (should (equal (list 'user-error
                         (concat "niwa " "[/origin]: Permission store unavailable"))
                   condition))))

(niwa-deftest niwa-permission--prompt-finish
  (:doc "local command refusal attributes a raw shared validation condition")
  (with-temp-buffer
    (insert "card")
    (goto-char 1)
    (let ((ov (make-overlay 1 5)))
      (overlay-put ov 'niwa-permission-prompt t)
      (overlay-put ov 'niwa-view-interaction-entry
                   (list :session (niwa-session--create
                                   :workspace (niwa-workspace--create
                                               :root "/origin"))))
      (cl-letf (((symbol-function 'niwa-permission-queue-validate-approval)
                 (lambda (&rest _) (user-error "Select directory-tree scope"))))
        (should (equal (list 'user-error
                             (concat "niwa " "[/origin]: Select directory-tree scope"))
                       (should-error (niwa-permission--prompt-finish 'allow-once)
                                     :type 'user-error)))))))

(provide 'test-niwa-permissions-diagnostics)
;;; test-niwa-permissions-diagnostics.el ends here
