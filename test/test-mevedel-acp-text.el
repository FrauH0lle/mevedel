;;; test-mevedel-acp-text.el --- Isolated ACP text requests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise stream delivery and teardown over a real protocol subprocess.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-acp-text)

(mevedel-deftest mevedel-acp-text-request (:quiet t)
  ,test
  (test)

  :doc "streams owned text and delivers one terminal callback with normalized usage"
  (with-temp-buffer
    (let (responses terminal-info)
      (mevedel-acp-text-request
       (list :command (executable-find "python3")
             :args (list mevedel-test--acp-peer) :cwd temporary-file-directory
             :normalize-outcome (lambda (outcome) (plist-put outcome :tokens '(:input 17 :output 3))))
       "hello"
       (lambda (response info)
         (push response responses)
         (when (eq t response) (setq terminal-info info)))
       t)
      (mevedel-test--await 5 "Isolated ACP text did not finish" terminal-info)
      (should (equal '("answer:" "hello" t) (reverse responses)))
      (should (equal '(:input 17 :output 3) (plist-get terminal-info :tokens)))
      (should (eq t (plist-get terminal-info :stream)))))

  :doc "nonstream delivery collects text once and cancellation is idempotent"
  (with-temp-buffer
    (let (responses)
      (let ((cancel
             (mevedel-acp-text-request
              (list :command (executable-find "python3")
                    :args (list mevedel-test--acp-peer) :cwd temporary-file-directory)
              "hello" (lambda (response _info) (push response responses)))))
        (mevedel-test--await 5 "Nonstream ACP text did not finish" responses)
        (funcall cancel)
        (should (equal '("answer:hello") responses)))))

  :doc "killing the owning workload buffer cancels its live prompt once"
  (let ((buffer (generate-new-buffer " *acp-text-owner*")) responses cancel)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq cancel
                  (mevedel-acp-text-request
                   (list :command (executable-find "python3")
                         :args (list mevedel-test--acp-peer) :cwd temporary-file-directory)
                   "wait" (lambda (response _info) (push response responses)) t)))
          (mevedel-test--await 5 "ACP text never became active" responses)
          (kill-buffer buffer)
          (funcall cancel)
          (should (equal '(abort "waiting") responses)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'test-mevedel-acp-text)
;;; test-mevedel-acp-text.el ends here
