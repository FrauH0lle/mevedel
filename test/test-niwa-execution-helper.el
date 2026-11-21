;;; test-niwa-execution-helper.el --- Tests for external child helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the confined external-helper interface owned by `niwa-execution'.

;;; Code:

(require 'cl-lib)
(require 'niwa-execution)
(require 'niwa-sandbox)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-niwa-execution-helper--workspace (root)
  "Return a minimal test workspace rooted at ROOT."
  (niwa-workspace--create
   :type 'file :id root :root root :name "execution-helper"))

(niwa-deftest niwa-execution-start-helper ()
  ,test
  (test)
  :doc "cleans its private working directory after asynchronous settlement"
  (let ((niwa-sandbox-mode 'off)
        result done)
    (niwa-execution-start-helper
     (lambda (child-result)
       (setq result child-result
             done t))
     "niwa-test-helper-async" '("pwd") nil nil)
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.05)))
    (let ((scratch (string-trim (plist-get result :output))))
      (should (string-match-p "niwa-helper-" scratch))
      (should-not (file-exists-p scratch))))
  :doc "declares read paths, artifact roots, and a private writable scratch root"
  (let* ((root (make-temp-file "niwa-helper-profile-" t))
         (input (file-name-concat root "input"))
         captured)
    (unwind-protect
        (progn
          (with-temp-file input (insert "input"))
          (cl-letf (((symbol-function 'niwa-execution-start-one-shot)
                     (lambda (callback &rest args)
                       (setq captured args)
                       (funcall callback
                                '(:exit-code 0 :output ""
                                  :timed-out-p nil)))))
            (niwa-execution-start-helper
             #'ignore "niwa-test-helper-profile" '("true") (list input)
             (list root)))
          (should (member (file-name-as-directory root)
                          (plist-get captured :writable-roots)))
          (should
           (equal (list :file-system
                        (list (list :path input :access 'read)))
                  (plist-get captured :additional-permissions)))
          (should-not (file-exists-p (plist-get captured :workdir))))
      (delete-directory root t)))
  :doc "returns an idempotent canceller for a sessionless helper"
  (let* ((niwa-sandbox-mode 'off)
         (root (make-temp-file "niwa-helper-cancel-" t))
         (marker (file-name-concat root "started"))
         result (settlements 0) cancel scratch)
    (unwind-protect
        (progn
          (setq cancel
                (niwa-execution-start-helper
                 (lambda (value) (setq result value) (cl-incf settlements))
                 "niwa-test-helper-cancel"
                 (list "sh" "-c" "pwd > \"$1.tmp\"; mv \"$1.tmp\" \"$1\"; exec sleep 30"
                       "niwa-test-helper-cancel" marker)
                 nil nil))
          (should (functionp cancel))
          (with-timeout (5 (error "Helper did not start"))
            (while (not (file-exists-p marker))
              (accept-process-output nil 0.01)))
          (setq scratch
                (string-trim
                 (with-temp-buffer
                   (insert-file-contents marker)
                   (buffer-string))))
          (should (file-directory-p scratch))
          (funcall cancel)
          (funcall cancel)
          (with-timeout (5 (error "Cancelled helper did not settle"))
            (while (not result) (accept-process-output nil 0.01)))
          (should (= settlements 1))
          (should (eq 'aborted (plist-get result :termination)))
          (should-not (file-exists-p scratch))
          (funcall cancel)
          (should (= settlements 1)))
      (niwa-execution-teardown-all)
      (delete-directory root t))))

(niwa-deftest niwa-execution-run-helper ()
  ,test
  (test)
  :doc "preserves helper output, artifact writes, and confinement facts"
  (let* ((root (make-temp-file "niwa-helper-run-" t))
         (input (file-name-concat root "input"))
         (output (file-name-concat root "output"))
         (session (niwa-session-create
                   "main" (test-niwa-execution-helper--workspace root))))
    (unwind-protect
        (progn
          (setf (niwa-session-sandbox-mode session) 'off)
          (with-temp-file input (insert "helper-input"))
          (let ((result
                 (niwa-execution-run-helper
                  "niwa-test-helper"
                  (list "sh" "-c"
                        "cat \"$1\"; printf helper-output > \"$2\""
                        "niwa-test-helper" input output)
                  (list input) (list root) :session session)))
            (should (= 0 (plist-get result :exit-code)))
            (should (equal "helper-input" (plist-get result :output)))
            (should (eq 'off
                        (plist-get (plist-get result :sandbox-facts)
                                   :sandbox)))
            (should-not (processp (niwa-session-execution-state session)))
            (should-not (plist-member result :process))
            (should (equal "helper-output"
                           (with-temp-buffer
                             (insert-file-contents output)
                             (buffer-string))))))
      (delete-directory root t)))
  :doc "required confinement refuses a helper before execution"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache
         '((nil . (:available nil :reason "test unavailable")))))
    (let ((result
           (niwa-execution-run-helper
            "niwa-test-helper-required" '("true") nil nil)))
      (should (= -1 (plist-get result :exit-code)))
      (should (string-match-p "test unavailable"
                              (error-message-string
                               (plist-get result :error))))))
  :doc "owner teardown settles the caller and removes helper scratch"
  (let* ((root (make-temp-file "niwa-helper-teardown-" t))
         (session (niwa-session-create
                   "main" (test-niwa-execution-helper--workspace root)
                   root))
         (temporary-file-directory (file-name-as-directory root))
         (niwa-sandbox-mode 'off)
         result)
    (unwind-protect
        (progn
          (run-at-time
           0.05 nil
           (lambda ()
             (niwa-execution-stop-owner session "agent-a")))
          (setq result
                (niwa-execution-run-helper
                 "niwa-test-helper-teardown"
                 '("sh" "-c" "sleep 30") nil nil
                 :session session :owner "agent-a"))
          (should (= -1 (plist-get result :exit-code)))
          (should (string-match-p
                   "owner was torn down"
                   (error-message-string (plist-get result :error))))
          (should-not
           (cl-find-if
            (lambda (name) (string-prefix-p "niwa-helper-" name))
            (directory-files root nil directory-files-no-dot-files-regexp))))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(provide 'test-niwa-execution-helper)
;;; test-niwa-execution-helper.el ends here
