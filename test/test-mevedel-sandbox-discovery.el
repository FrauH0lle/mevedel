;;; test-mevedel-sandbox-discovery.el --- Discovery handoff tests -*- lexical-binding: t -*-

;;; Commentary:

;; Failed and incomplete discovery cannot become a launchable preparation.

;;; Code:

(require 'helpers (file-name-concat (file-name-directory
                                    (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-sandbox)
(require 'mevedel-permission-rules)

(mevedel-deftest mevedel-sandbox-prepare-start
  (:doc "refuses incomplete children and ignores delivery after cancellation")
  (progn
    (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")))
    (dolist (reply '((:exit-code 1 :output "")
                     (:exit-code 0 :output "P/truncated")
                     (:exit-code 0 :output "bad\0")
                     (:exit-code 0 :output "Prelative\0")
                     (:exit-code 0 :output "" :output-limit-p t)
                     (:exit-code 0 :output "" :timed-out-p t)
                     (:exit-code 0 :output "" :error (error "Failed"))
                     cancel))
      (let* ((root (make-temp-file "mevedel-scan-refusal-" t))
             (mevedel-protected-paths '(("**/.git/**" . read-only)))
             (mevedel-sandbox--probe-cache '((nil . (:available t :executable "unused"))))
             (calls 0) receive stopped result cancel)
        (unwind-protect
            (progn
              (setq cancel
                    (mevedel-sandbox-prepare-start
                     (lambda (preparation) (cl-incf calls) (setq result preparation))
                     (lambda (_command callback)
                       (setq receive callback)
                       (lambda () (setq stopped t)))
                     '("true") root (list root) nil nil 'required))
              (should receive)
              (should-not result)
              (if (eq reply 'cancel)
                  (progn
                    (funcall cancel)
                    (should stopped)
                    (funcall receive '(:exit-code 0 :output ""))
                    (should (= 0 calls))
                    (should-not (file-exists-p (file-name-concat root ".git"))))
                (funcall receive reply)
                (should (= 1 calls))
                (should (eq 'refused (plist-get result :state)))
                (should-not (plist-get result :command)))
              (funcall cancel))
          (delete-directory root t))))))

(mevedel-deftest mevedel-sandbox-prepare-start/filename-coding
  (:doc "restores filename bytes from the execution result's UTF-8 text")
  (progn
    (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")))
    (let* ((root (make-temp-file "mevedel-scan-coding-" t))
           (file-name-coding-system 'iso-latin-1)
           (path (file-name-concat root "caf\u00c3\u00a9" ".git"))
           (mevedel-protected-paths '(("**/.git/**" . read-only)))
           (mevedel-sandbox--probe-cache '((nil . (:available t :executable "unused"))))
           result)
      (unwind-protect
          (progn
            (make-directory path t)
            (mevedel-sandbox-prepare-start
             (lambda (preparation) (setq result preparation))
             (lambda (_command callback)
               (funcall callback (list :exit-code 0
                                       :output (concat "P" root "/caf\u00e9/.git\0")))
               #'ignore)
             '("true") root (list root) nil nil 'required)
            (should (eq 'confined (plist-get result :state)))
            (should (member path (plist-get result :command))))
        (when result (mevedel-sandbox-cleanup result))
        (delete-directory root t)))))

(provide 'test-mevedel-sandbox-discovery)
;;; test-mevedel-sandbox-discovery.el ends here
