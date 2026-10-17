;;; test-mevedel-tool-exec-registration.el --- Exec catalog tests -*- lexical-binding: t -*-

;;; Commentary:

;; Monitor's discovery contract does not start a process.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tool-exec-registration)

(mevedel-deftest mevedel-tool-exec--register/monitor
  (:before-each (mevedel-tool-exec--register))
  ,test
  (test)
  :doc "Monitor shares Bash authority with required description and bounded lifetime"
  (let* ((tool (mevedel-tool-get "Monitor"))
         (bash (mevedel-tool-get "Bash"))
         (args (gptel-tool-args (mevedel-tool-gptel-tool tool)))
         (bash-args (gptel-tool-args (mevedel-tool-gptel-tool bash)))
         (timeout (seq-find (lambda (arg) (equal "timeout_ms" (plist-get arg :name))) args)))
    (should (eq (mevedel-tool-handler tool) #'mevedel-tool-exec--monitor))
    (should (eq (mevedel-tool-check-permission-async tool)
                (mevedel-tool-check-permission-async bash)))
    (should (eq (mevedel-tool-renderer tool) (mevedel-tool-renderer bash)))
    (should (gptel-tool-async (mevedel-tool-gptel-tool tool)))
    (should (= 1000 (plist-get timeout :minimum)))
    (should (= 3600000 (plist-get timeout :maximum)))
    (should (plist-get timeout :optional))
    (dolist (name '("command" "description"))
      (let ((arg (seq-find (lambda (arg) (equal name (plist-get arg :name))) args)))
        (should arg)
        (should-not (plist-get arg :optional))))
    (dolist (name '("tty" "yield_time_ms"))
      (should-not (seq-find (lambda (arg) (equal name (plist-get arg :name))) args)))
    (dolist (name '("sandbox_permissions" "additional_permissions" "justification"))
      (should (equal (seq-find (lambda (arg) (equal name (plist-get arg :name))) args)
                     (seq-find (lambda (arg) (equal name (plist-get arg :name))) bash-args))))))

(mevedel-deftest mevedel-tool-ensure/monitor
  (:doc "Monitor discovery restores a missing built-in registration")
  (progn
    (mevedel-tool-clear-registry)
    (should-not (mevedel-tool-get "Monitor"))
    (should (equal "Monitor" (mevedel-tool-name (mevedel-tool-ensure "Monitor"))))))

(mevedel-deftest mevedel-tool-exec--register/bash-description
  (:before-each (mevedel-tool-exec--register))
  ,test
  (test)
  :doc "Bash exposes an optional human-readable description"
  (let* ((tool (mevedel-tool-get "Bash"))
         (args (gptel-tool-args (mevedel-tool-gptel-tool tool)))
         (description (seq-find (lambda (arg)
                                  (equal "description" (plist-get arg :name)))
                                args)))
    (should description)
    (should (plist-get description :optional))
    (should (equal "string" (plist-get description :type)))))

(provide 'test-mevedel-tool-exec-registration)
;;; test-mevedel-tool-exec-registration.el ends here
