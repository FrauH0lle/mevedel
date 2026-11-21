;;; test-niwa-tool-exec-registration.el --- Exec catalog tests -*- lexical-binding: t -*-

;;; Commentary:

;; Monitor's discovery contract does not start a process.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-tool-exec-registration)

(niwa-deftest niwa-tool-exec--register/monitor
  (:before-each (niwa-tool-exec--register))
  ,test
  (test)
  :doc "Monitor shares Bash authority with required description and bounded lifetime"
  (let* ((tool (niwa-tool-get "Monitor"))
         (bash (niwa-tool-get "Bash"))
         (args (gptel-tool-args (niwa-tool-gptel-tool tool)))
         (bash-args (gptel-tool-args (niwa-tool-gptel-tool bash)))
         (timeout (seq-find (lambda (arg) (equal "timeout_ms" (plist-get arg :name))) args)))
    (should (eq (niwa-tool-handler tool) #'niwa-tool-exec--monitor))
    (should (eq (niwa-tool-check-permission-async tool)
                (niwa-tool-check-permission-async bash)))
    (should (eq (niwa-tool-renderer tool) (niwa-tool-renderer bash)))
    (should (gptel-tool-async (niwa-tool-gptel-tool tool)))
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

(niwa-deftest niwa-tool-ensure/monitor
  (:doc "Monitor discovery restores a missing built-in registration")
  (progn
    (niwa-tool-clear-registry)
    (should-not (niwa-tool-get "Monitor"))
    (should (equal "Monitor" (niwa-tool-name (niwa-tool-ensure "Monitor"))))))

(niwa-deftest niwa-tool-exec--register/bash-description
  (:before-each (niwa-tool-exec--register))
  ,test
  (test)
  :doc "Bash requires a human-readable description in native and common schemas"
  (let* ((tool (niwa-tool-get "Bash"))
         (args (gptel-tool-args (niwa-tool-gptel-tool tool)))
         (description (seq-find (lambda (arg)
                                  (equal "description" (plist-get arg :name)))
                                args)))
    (should description)
    (should-not (plist-get description :optional))
    (should (equal "string" (plist-get description :type)))
    (should (eq :required (nth 2 (assq 'description (niwa-tool-args tool)))))
    (let* ((native (aref (gptel--parse-tools nil (list (niwa-tool-gptel-tool tool))) 0))
           (schema (plist-get (plist-get native :function) :parameters)))
      (should (member "description" (append (plist-get schema :required) nil))))))

(niwa-deftest niwa-tool-exec--register/eval-guidance
  (:before-each (niwa-tool-exec--register))
  ,test
  (test)
  :doc "Eval's native contract distinguishes Lisp work from shell execution"
  (let* ((tool (niwa-tool-get "Eval"))
         (native (aref (gptel--parse-tools
                        nil (list (niwa-tool-gptel-tool tool))) 0))
         (description (plist-get (plist-get native :function) :description)))
    (should (string-match-p "editor inspection or Lisp computation"
                            (niwa-tool-description tool)))
    (dolist (text '("result depends on the user's running Emacs state"
                    "isolated Emacs Lisp checks that do not need live state"
                    "Use Bash for shell commands"
                    "Appropriate Emacs"
                    "shell-command-to-string"
                    "prefer Read, Glob, Grep"
                    "use Bash's recovery path"
                    "live Eval goes to the selected permission reviewer"))
      (should (string-match-p (regexp-quote text) description)))))

(provide 'test-niwa-tool-exec-registration)
;;; test-niwa-tool-exec-registration.el ends here
