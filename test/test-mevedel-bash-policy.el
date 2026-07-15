;;; test-mevedel-bash-policy.el --- Tests for Bash policies -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for argument-aware command safety policies.

;;; Code:

(require 'mevedel-bash-policy)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Read-only policies

(mevedel-deftest mevedel-bash-policy-read-only-p ()
  ,test
  (test)
  :doc "safe variants:
`mevedel-bash-policy-read-only-p' accepts recognized inspection arguments"
  (dolist (argv
           '(("git" "status")
             ("git" "--no-pager" "log" "-1")
             ("git" "branch" "--show-current")
             ("find" "." "-name" "*.el")
             ("rg" "TODO" "src")
             ("base64" "file")
             ("sed" "-n" "1,5p" "file")
             ("awk" "{print $1}" "file")))
    (should (mevedel-bash-policy-read-only-p argv)))
  :doc "unsafe variants:
`mevedel-bash-policy-read-only-p' rejects writing and helper execution"
  (dolist (argv
           '(("git" "diff" "--output=file")
             ("git" "-c" "core.pager=cat" "log")
             ("git" "branch" "new-name")
             ("find" "." "-delete")
             ("find" "." "-exec" "printf" "{}" ";")
             ("rg" "--pre" "helper" "TODO")
             ("rg" "--search-zip" "TODO")
             ("base64" "-o" "output" "file")
             ("sed" "-n" "1,5d" "file")
             ("awk" "{system(\"id\")}" "file")
             ("awk" "BEGIN { f = \"sys\" \"tem\"; @f(\"id\") }")
             ("awk" "{print $1 > \"out\"}" "file")))
    (should-not (mevedel-bash-policy-read-only-p argv))))

(provide 'test-mevedel-bash-policy)

;;; test-mevedel-bash-policy.el ends here
