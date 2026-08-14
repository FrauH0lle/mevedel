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
  :doc "test brackets:
`mevedel-bash-analysis-analyze' recognizes literal bracket command delimiters"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((analysis (mevedel-bash-analysis-analyze "[ 1 = 2 ]")))
      (should (equal 'unknown (plist-get analysis :class)))
      (should (equal '(("[" "1" "=" "2" "]"))
                     (plist-get analysis :commands))))
    (should (equal 'complex
                   (plist-get (mevedel-bash-analysis-analyze "echo [12]")
                              :class))))
  :doc "Bash escaping:
`mevedel-bash-analysis-analyze' returns shell-accurate plain argv"
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((shell-file-name-quote-list nil))
      (should (equal '(("echo" "foo bar"))
                     (plist-get
                      (mevedel-bash-analysis-analyze "echo foo\\ bar")
                      :commands)))))
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
    (let ((shell-file-name-quote-list nil))
      (should (equal '("~/.ssh/my key" ".git/config")
                     (plist-get
                      (mevedel-bash-analysis-analyze
                       "FOO=bar cat ~/.ssh/my\\ key >.git/config")
                      :resources)))))
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
  :doc "tree-sitter agreement:
`mevedel-bash-analysis-analyze' reads a construct the same way either parser does"
  ;; Installing the grammar must not reclassify a command.  These two are the
  ;; ones the grammar sees as their own node types where the scanner sees
  ;; plain words, so they are where the analyzers drift apart first.
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    (dolist (source '("NODE_ENV=test npm run test" "[ 1 = 2 ]"))
      (let ((grammar (mevedel-bash-analysis-analyze source))
            (scanner (cl-letf (((symbol-function 'treesit-language-available-p)
                                (lambda (_language) nil)))
                       (mevedel-bash-analysis-analyze source))))
        (should (equal 'treesit (plist-get grammar :parser)))
        (should (equal (plist-get scanner :class)
                       (plist-get grammar :class)))
        (should (equal (plist-get scanner :commands)
                       (plist-get grammar :commands))))))
  :doc "tree-sitter double brackets:
`mevedel-bash-analysis-analyze' keeps `[[' complex, unlike `['"
  ;; It changes quoting and globbing, so the single-bracket allowance below
  ;; must not extend to it.
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    (should (equal 'complex
                   (plist-get (mevedel-bash-analysis-analyze "[[ -f x ]]")
                              :class))))
  :doc "tree-sitter complex syntax:
`mevedel-bash-analysis-analyze' rejects redirection structurally"
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    (dolist (source '("echo x > out" "pwd\ncat file"))
      (should (equal 'complex
                     (plist-get (mevedel-bash-analysis-analyze source)
                                :class)))))
  :doc "background fact:
`mevedel-bash-analysis-analyze' distinguishes native background operators"
  (progn
    (should (plist-get (mevedel-bash-analysis-analyze "sleep 1 &")
                       :background-p))
    (should (plist-get (mevedel-bash-analysis-analyze "coproc sleep 1")
                       :background-p))
    (dolist (source '("if true; then coproc sleep 1; fi"
                      "while true; do coproc sleep 1; done"
                      "if false; then :; else coproc sleep 1; fi"
                      "{ coproc sleep 1; }"
                      "time coproc sleep 1"
                      "time -p ! coproc sleep 1"
                      "! time -p coproc sleep 1"
                      "if true; then time coproc sleep 1; fi"
                      "echo \"$(sleep 1 &)\""
                      "echo `sleep 1 &`"
                      "echo $(( $(sleep 1 &) + 1 ))"
                      "(( $(sleep 1 &) + 1 ))"))
      (should (plist-get (mevedel-bash-analysis-analyze source)
                         :background-p)))
    (should-not (plist-get (mevedel-bash-analysis-analyze
                            "printf '%s' '&'")
                           :background-p))
    (dolist (source '("printf hi 2>&1" "printf hi &>out" "a |& b"))
      (should-not (plist-get (mevedel-bash-analysis-analyze source)
                             :background-p)))
    (should-not (plist-get (mevedel-bash-analysis-analyze
                            "echo \"$((1 & 2))\"")
                           :background-p))
    (dolist (source '("echo $((1 & 2))"
                      "((1 & 2))"
                      "for ((i = 0; i & 3; i++)); do :; done"
                      "printf %s '$(sleep 1 &)'"
                      "printf %s '`sleep 1 &`'"
                      "printf %s \"\\$(sleep 1 &)\""
                      "printf %s \"\\`sleep 1 &\\`\""))
      (should-not (plist-get (mevedel-bash-analysis-analyze source)
                             :background-p)))))

(mevedel-deftest mevedel-bash-analysis--treesit ()
  ,test
  (test)
  :doc "scanner parity:
`mevedel-bash-analysis--treesit' matches the scanner on syntax it rejects"
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    ;; A configured grammar must not make analysis less precise than the
    ;; conservative scanner alone.  Every source here is one the grammar
    ;; either rejects outright or, for single-bracket predicates, must
    ;; accept exactly as the scanner does.
    (dolist (source '("NODE_ENV=test npm run test"
                      "DOCKER_HOST=tcp://example docker ps"
                      "FOO='bar baz' rm file"
                      "A+=1 cmd"
                      "[ 1 = 2 ]"
                      "[ -f x ]"
                      "[ ! -f x ]"
                      "[[ 1 = 2 ]]"
                      "grep needle >out"
                      "for f in *; do echo $f; done"))
      (let ((treesit (mevedel-bash-analysis--treesit source))
            (heuristic (mevedel-bash-analysis--heuristic source)))
        (dolist (key '(:class :commands :segments :candidates :resources))
          (should (equal (plist-get heuristic key)
                         (plist-get treesit key)))))))
  :doc "deny surface:
`mevedel-bash-analysis--treesit' keeps candidates for rejected syntax"
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    ;; An environment assignment must not hide the command it prefixes,
    ;; because explicit deny rules match against `:candidates'.
    (let ((analysis (mevedel-bash-analysis--treesit "FOO='bar baz' rm file")))
      (should (equal 'treesit (plist-get analysis :parser)))
      (should (member "rm file" (plist-get analysis :candidates)))))
  :doc "single-bracket predicates:
`mevedel-bash-analysis--treesit' reports `[' as a plain command"
  (progn
    (skip-unless (treesit-language-available-p 'bash))
    (let ((analysis (mevedel-bash-analysis--treesit "[ 1 = 2 ]")))
      (should (equal '(("[" "1" "=" "2" "]")) (plist-get analysis :commands)))
      (should-not (plist-get analysis :complex-p)))
    ;; The extended form, negation, and patterns stay unsupported.
    (dolist (source '("[[ 1 = 2 ]]" "[ ! -f x ]" "[ a != b* ]"))
      (should (plist-get (mevedel-bash-analysis--treesit source) :complex-p)))))

(provide 'test-mevedel-bash-analysis)

;;; test-mevedel-bash-analysis.el ends here
