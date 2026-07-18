;;; test-mevedel-execution-helper.el --- Tests for external child helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the confined external-helper interface owned by `mevedel-execution'.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-execution-helper--workspace (root)
  "Return a minimal test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'test :id root :root root :name "execution-helper"))

(mevedel-deftest mevedel-execution-start-helper ()
  ,test
  (test)
  :doc "cleans its private working directory after asynchronous settlement"
  (let ((mevedel-sandbox-mode 'off)
        result done)
    (mevedel-execution-start-helper
     (lambda (child-result)
       (setq result child-result
             done t))
     "mevedel-test-helper-async" '("pwd") nil nil)
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.05)))
    (let ((scratch (string-trim (plist-get result :output))))
      (should (string-match-p "mevedel-helper-" scratch))
      (should-not (file-exists-p scratch))))
  :doc "declares read paths, artifact roots, and a private writable scratch root"
  (let* ((root (make-temp-file "mevedel-helper-profile-" t))
         (input (file-name-concat root "input"))
         captured)
    (unwind-protect
        (progn
          (with-temp-file input (insert "input"))
          (cl-letf (((symbol-function 'mevedel-execution-start-one-shot)
                     (lambda (callback &rest args)
                       (setq captured args)
                       (funcall callback
                                '(:exit-code 0 :output ""
                                  :timed-out-p nil)))))
            (mevedel-execution-start-helper
             #'ignore "mevedel-test-helper-profile" '("true") (list input)
             (list root)))
          (should (member (file-name-as-directory root)
                          (plist-get captured :writable-roots)))
          (should
           (equal (list :file-system
                        (list (list :path input :access 'read)))
                  (plist-get captured :additional-permissions)))
          (should-not (file-exists-p (plist-get captured :workdir))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-run-helper ()
  ,test
  (test)
  :doc "preserves helper output, artifact writes, and confinement facts"
  (let* ((root (make-temp-file "mevedel-helper-run-" t))
         (input (file-name-concat root "input"))
         (output (file-name-concat root "output"))
         (session (mevedel-session-create
                   "main" (test-mevedel-execution-helper--workspace root)))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (progn
          (with-temp-file input (insert "helper-input"))
          (let ((result
                 (mevedel-execution-run-helper
                  "mevedel-test-helper"
                  (list "sh" "-c"
                        "cat \"$1\"; printf helper-output > \"$2\""
                        "mevedel-test-helper" input output)
                  (list input) (list root) :session session)))
            (should (= 0 (plist-get result :exit-code)))
            (should (equal "helper-input" (plist-get result :output)))
            (should (eq 'off
                        (plist-get (plist-get result :sandbox-facts)
                                   :sandbox)))
            (should-not (processp (mevedel-session-execution-state session)))
            (should-not (plist-member result :process))
            (should (equal "helper-output"
                           (with-temp-buffer
                             (insert-file-contents output)
                             (buffer-string))))))
      (delete-directory root t)))
  :doc "required confinement refuses a helper before execution"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache
         '(:available nil :reason "test unavailable")))
    (let ((result
           (mevedel-execution-run-helper
            "mevedel-test-helper-required" '("true") nil nil)))
      (should (= -1 (plist-get result :exit-code)))
      (should (string-match-p "test unavailable"
                              (error-message-string
                               (plist-get result :error))))))
  :doc "owner teardown settles the caller and removes helper scratch"
  (let* ((root (make-temp-file "mevedel-helper-teardown-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-execution-helper--workspace root)
                   root))
         (temporary-file-directory (file-name-as-directory root))
         (mevedel-sandbox-mode 'off)
         result)
    (unwind-protect
        (progn
          (run-at-time
           0.05 nil
           (lambda ()
             (mevedel-execution-stop-owner session "agent-a")))
          (setq result
                (mevedel-execution-run-helper
                 "mevedel-test-helper-teardown"
                 '("sh" "-c" "sleep 30") nil nil
                 :session session :owner "agent-a"))
          (should (= -1 (plist-get result :exit-code)))
          (should (string-match-p
                   "owner was torn down"
                   (error-message-string (plist-get result :error))))
          (should-not
           (cl-find-if
            (lambda (name) (string-prefix-p "mevedel-helper-" name))
            (directory-files root nil directory-files-no-dot-files-regexp))))
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(provide 'test-mevedel-execution-helper)
;;; test-mevedel-execution-helper.el ends here
