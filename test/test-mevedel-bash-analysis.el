;;; test-mevedel-bash-analysis.el --- Tests for Bash analysis -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for normalized conservative Bash analysis.

;;; Code:

(require 'cl-lib)
(require 'mevedel-bash-analysis)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Analysis

(mevedel-deftest mevedel-bash-analysis-analyze ()
  ,test
  (test)
  :doc "normalized result:
`mevedel-bash-analysis-analyze' returns every required fact"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((analysis (mevedel-bash-analysis-analyze "pwd && cat ./README.md")))
      (should (equal 'read-only (plist-get analysis :class)))
      (should (equal '(("pwd") ("cat" "./README.md"))
                     (plist-get analysis :commands)))
      (should (equal 'heuristic (plist-get analysis :parser)))
      (should (equal '("./README.md") (plist-get analysis :resources)))
      (should (consp (plist-get analysis :reasons)))))
  :doc "bare directory resources:
`mevedel-bash-analysis-analyze' preserves current and parent directory operands"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (should
     (equal '("." "..")
            (plist-get
             (mevedel-bash-analysis-analyze "rg TODO . && rg TODO ..")
             :resources))))
  :doc "unknown command:
`mevedel-bash-analysis-analyze' classifies an understood command conservatively"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (should (equal 'unknown
                   (plist-get (mevedel-bash-analysis-analyze "make test")
                              :class))))
  :doc "path-qualified executable:
`mevedel-bash-analysis-analyze' does not trust a known basename at another path"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (should (equal 'unknown
                   (plist-get (mevedel-bash-analysis-analyze "./cat file")
                              :class))))
  :doc "Bash escaping:
`mevedel-bash-analysis-analyze' returns shell-accurate plain argv"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (should (equal '(("echo" "foo bar"))
                   (plist-get
                    (mevedel-bash-analysis-analyze "echo foo\\ bar")
                    :commands))))
  :doc "line continuation:
`mevedel-bash-analysis-analyze' removes Bash backslash-newline pairs"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((analysis
           (mevedel-bash-analysis-analyze
            (concat "cat ~/.ss\\" "\n" "h/id_rsa"))))
      (should (equal '(("cat" "~/.ssh/id_rsa"))
                     (plist-get analysis :commands)))
      (should (equal '("~/.ssh/id_rsa")
                     (plist-get analysis :resources)))))
  :doc "quoted line continuation:
`mevedel-bash-analysis-analyze' preserves backslash-newline in single quotes"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (should
     (equal `(("echo" ,(concat "a\\" "\n" "b")))
            (plist-get
             (mevedel-bash-analysis-analyze
              (concat "echo 'a\\" "\n" "b'"))
             :commands))))
  :doc "dangerous precedence:
`mevedel-bash-analysis-analyze' lets a dangerous compound component win"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((mevedel-bash-dangerous-commands '("rm")))
      (should (equal 'dangerous
                     (plist-get
                      (mevedel-bash-analysis-analyze "pwd && rm ./file")
                      :class)))))
  :doc "complex forms:
`mevedel-bash-analysis-analyze' rejects unsupported shell effects"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (dolist (source '("echo x > out"
                      "echo $(pwd)"
                      "echo `pwd`"
                      "echo $HOME"
                      "FOO=bar make test"
                      "FOO+=bar make test"
                      "(pwd)"
                      "pwd
cat file"
                      "cat <<EOF\nx\nEOF"
                      "if true; then pwd; fi"
                      "coproc cat file"
                      "echo x & pwd"
                      "echo \"unterminated"))
      (should (equal 'complex
                     (plist-get (mevedel-bash-analysis-analyze source)
                                :class)))))
  :doc "dangerous harvesting:
`mevedel-bash-analysis-analyze' detects danger inside complex input"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((mevedel-bash-dangerous-commands '("rm")))
      (let ((analysis
             (mevedel-bash-analysis-analyze "echo $(rm ./file)")))
        (should (equal 'dangerous (plist-get analysis :class)))
        (should (member "rm ./file" (plist-get analysis :candidates))))))
  :doc "nested candidate harvesting:
`mevedel-bash-analysis-analyze' splits command chains inside substitutions"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((analysis
           (mevedel-bash-analysis-analyze
            "echo \"$(pwd && rm file && echo x)\"")))
      (should (member "rm file" (plist-get analysis :candidates)))))
  :doc "quoted parentheses in substitutions:
`mevedel-bash-analysis-analyze' does not truncate nested command facts"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((analysis
           (mevedel-bash-analysis-analyze
            "echo \"$(printf ')' && cat .git/config && rm file)\"")))
      (should (member "rm file" (plist-get analysis :candidates)))
      (should (member ".git/config" (plist-get analysis :resources)))))
  :doc "nested substitution parsing:
`mevedel-bash-analysis-analyze' preserves recursively quoted substitutions"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((analysis
           (mevedel-bash-analysis-analyze
            "echo \"$(printf ')' && echo \"$(rm file)\")\"")))
      (should (member "rm file" (plist-get analysis :candidates)))))
  :doc "complex resource harvesting:
`mevedel-bash-analysis-analyze' preserves literal protected-path candidates"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (should (equal '("~/.ssh/my key" ".git/config")
                   (plist-get
                    (mevedel-bash-analysis-analyze
                     "FOO=bar cat ~/.ssh/my\\ key >.git/config")
                    :resources))))
  :doc "substitution resource harvesting:
`mevedel-bash-analysis-analyze' preserves paths inside quoted substitutions"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (should (member ".git/config"
                    (plist-get
                     (mevedel-bash-analysis-analyze
                      "echo \"$(cat .git/config)\"")
                     :resources))))
  :doc "tree-sitter source:
`mevedel-bash-analysis-analyze' uses the configured Bash grammar when present"
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    (let ((analysis (mevedel-bash-analysis-analyze "pwd && cat README.md")))
      (should (equal 'treesit (plist-get analysis :parser)))
      (should (equal '(("pwd") ("cat" "README.md"))
                     (plist-get analysis :commands)))))
  :doc "tree-sitter complex syntax:
`mevedel-bash-analysis-analyze' rejects redirection structurally"
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    (dolist (source '("echo x > out" "pwd\ncat file"))
      (should (equal 'complex
                     (plist-get (mevedel-bash-analysis-analyze source)
                                :class))))))

(provide 'test-mevedel-bash-analysis)

;;; test-mevedel-bash-analysis.el ends here
