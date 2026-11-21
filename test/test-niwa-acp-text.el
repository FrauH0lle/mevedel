;;; test-niwa-acp-text.el --- Isolated ACP text requests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise stream delivery and teardown over a real protocol subprocess.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-acp-text)

(niwa-deftest niwa-acp-text-request-authentication ()
  ,test
  (test)
  :doc "reports the same advertised sign-in methods for startup and prompt failures"
  (dolist (case '(("session/new" . startup) ("session/prompt" . response)))
    (with-temp-buffer
      (let ((process-environment (copy-sequence process-environment)) result)
        (setenv "NIWA_ACP_TEST_AUTH_FAILURE" (car case))
        (niwa-acp-text-request
         (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
               :cwd temporary-file-directory)
         "hello" (lambda (response info) (setq result (cons response info))))
        (niwa-test--await 5 "ACP authentication failure did not settle" result)
        (should-not (car result))
        (should (eq (cdr case) (plist-get (cdr result) :stage)))
        (should (= -32000 (plist-get (cdr result) :code)))
        (should (equal (plist-get (cdr result) :error)
                       "Authentication required for fixture-agent. Advertised sign-in methods: Browser sign-in, backup."))))))

(niwa-deftest niwa-acp-text-request (:quiet t)
  ,test
  (test)

  :doc "streams owned text and delivers one terminal callback with normalized usage"
  (with-temp-buffer
    (let (responses terminal-info)
      (niwa-acp-text-request
       (list :command (executable-find "python3")
             :args (list niwa-test--acp-peer) :cwd temporary-file-directory
             :normalize-outcome (lambda (outcome) (plist-put outcome :tokens '(:input 17 :output 3))))
       "hello"
       (lambda (response info)
         (push response responses)
         (when (eq t response) (setq terminal-info info)))
       t)
      (niwa-test--await 5 "Isolated ACP text did not finish" terminal-info)
      (should (equal '("answer:" "hello" t) (reverse responses)))
      (should (equal '(:input 17 :output 3) (plist-get terminal-info :tokens)))
      (should (eq t (plist-get terminal-info :stream)))))

  :doc "nonstream delivery collects text once and cancellation is idempotent"
  (with-temp-buffer
    (let (responses)
      (let ((cancel
             (niwa-acp-text-request
              (list :command (executable-find "python3")
                    :args (list niwa-test--acp-peer) :cwd temporary-file-directory)
              "hello" (lambda (response _info) (push response responses)))))
        (niwa-test--await 5 "Nonstream ACP text did not finish" responses)
        (funcall cancel)
        (should (equal '("answer:hello") responses)))))

  :doc "notifies readiness once, before the prompt's response, and marks its stage"
  (with-temp-buffer
    (let (events)
      (niwa-acp-text-request
       (list :command (executable-find "python3")
             :args (list niwa-test--acp-peer) :cwd temporary-file-directory)
       "hello"
       (lambda (response info) (push (list response (plist-get info :stage)) events))
       nil (lambda () (push 'ready events)))
      (niwa-test--await 5 "ACP text did not finish" (cl-find-if #'consp events))
      (should (equal '(ready ("answer:hello" response)) (reverse events)))))

  :doc "a startup failure reports its stage and structured code without readiness"
  (with-temp-buffer
    (let (result)
      (niwa-acp-text-request
       (list :command (executable-find "python3")
             :args (list niwa-test--acp-peer) :cwd temporary-file-directory
             :prepare-launch (lambda (_ready failure)
                               (funcall failure "Sign in to continue" 'unavailable)
                               #'ignore))
       "hello"
       (lambda (response info) (setq result (list response info)))
       nil (lambda () (ert-fail "A failed startup reported readiness")))
      (niwa-test--await 5 "ACP startup failure was not delivered" result)
      (should-not (car result))
      (should (eq 'startup (plist-get (cadr result) :stage)))
      (should (eq 'unavailable (plist-get (cadr result) :code)))
      (should (equal "Sign in to continue" (plist-get (cadr result) :error)))))

  :doc "killing the owning workload buffer cancels its live prompt once"
  (let ((buffer (generate-new-buffer " *acp-text-owner*")) responses cancel)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq cancel
                  (niwa-acp-text-request
                   (list :command (executable-find "python3")
                         :args (list niwa-test--acp-peer) :cwd temporary-file-directory)
                   "wait" (lambda (response _info) (push response responses)) t)))
          (niwa-test--await 5 "ACP text never became active" responses)
          (kill-buffer buffer)
          (funcall cancel)
          (should (equal '(abort "waiting") responses)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'test-niwa-acp-text)
;;; test-niwa-acp-text.el ends here
