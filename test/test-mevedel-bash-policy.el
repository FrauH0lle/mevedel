;;; test-mevedel-bash-policy.el -- Tests for Bash authorization and permission policy -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for Bash authorization and permission policy.

;;; Code:

(require 'gptel)
(require 'mevedel-bash-policy)
(require 'cl-lib)
(require 'seq)
(require 'mevedel-agents)
(require 'mevedel-bash-analysis)
(require 'mevedel-structs)
(require 'mevedel-execution-target)
(require 'mevedel-permission-rules)
(require 'mevedel-plan-mode)
(require 'mevedel-sandbox)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-bash-policy-read-only-p ()
  ,test
  (test)
  :doc "safe variants:
`mevedel-bash-policy-read-only-p' accepts recognized inspection arguments"
  (dolist (argv
           '(("find" "." "-name" "*.el")
             ("rg" "TODO" "src")
             ("base64" "file")
             ("sed" "-n" "1,5p" "file")
             ("awk" "{print $1}" "file")
             ("diff" "-u" "one" "two")
             ("cmp" "--bytes=10" "one" "two")
             ("readlink" "-f" "file")
             ("realpath" "--relative-to=." "file")
             ("du" "-sh" ".")
             ("uniq")
             ("uniq" "-ci" "input")
             ("uniq" "--" "input" "-")
             ("test" "-f" "file")
             ("[" "1" "=" "2" "]")
             ("test" "1" "-eq" "2")))
    (should (mevedel-bash-policy-read-only-p argv)))
  :doc "Git variants:
`mevedel-bash-policy-read-only-p' accepts hardened Git inspection arguments"
  (dolist (argv
           '(("git" "status")
             ("git" "--no-pager" "log" "-1")
             ("git" "branch" "--show-current")))
    (should (mevedel-bash-policy-read-only-p argv)))
  :doc "unsafe variants:
`mevedel-bash-policy-read-only-p' rejects writing and helper execution"
  (dolist (argv
           '(("git" "diff" "--output=file")
             ("diff" "--output=out" "one" "two")
             ("du" "--unknown" ".")
             ("uniq" "input" "output")
             ("uniq" "input" "-c")
             ("uniq" "--unknown")
             ("test" "-v" "x[$(touch marker)]")
             ("test" "x[$(touch marker)]" "-eq" "1")
             ("[" "-f" "file")
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

(mevedel-deftest mevedel-bash-policy--uniq-read-only-p ()
  ,test
  (test)
  :doc "recognized uniq flags and numeric values preserve stdout-only invocations"
  (dolist (argv
           '(("uniq") ("uniq" "input") ("uniq" "-") ("uniq" "-" "-")
             ("uniq" "input" "-") ("uniq" "--" "-named-input" "-")
             ("uniq" "--") ("uniq" "-cdiuz" "input") ("uniq" "-D" "input")
             ("uniq" "-cf" "2" "input") ("uniq" "-f2" "-s" "3" "-w4" "input")
             ("uniq" "-if+2" "input")
             ("uniq" "--skip-fields=2" "--skip-chars" "0" "--check-chars=10" "input")
             ("uniq" "--count" "--repeated" "--ignore-case" "--zero-terminated" "input")
             ("uniq" "--unique" "input") ("uniq" "--help") ("uniq" "--version")
             ("uniq" "--all-repeated" "input")
             ("uniq" "--all-repeated=none" "input")
             ("uniq" "--all-repeated=prepend" "input")
             ("uniq" "--all-repeated=separate" "input")
             ("uniq" "--group" "input") ("uniq" "--group=separate" "input")
             ("uniq" "--group=prepend" "input") ("uniq" "--group=append" "input")
             ("uniq" "--group=both" "input" "-")))
    (should (mevedel-bash-policy--uniq-read-only-p argv)))
  :doc "GNU option case remains significant regardless of ambient case folding"
  (dolist (ambient '(t nil))
    (let ((case-fold-search ambient))
      (should (mevedel-bash-policy--uniq-read-only-p '("uniq" "-cdiuz" "input")))
      (should (mevedel-bash-policy-read-only-p '("uniq" "-cd" "input")))
      (dolist (argv '(("uniq" "-cD" "input") ("uniq" "-C" "input")
                      ("uniq" "-I" "input") ("uniq" "-F1" "input")
                      ("uniq" "--GROUP=both" "input")
                      ("uniq" "--group=BOTH" "input")
                      ("uniq" "--SKIP-FIELDS=1" "input")))
        (should-not (mevedel-bash-policy--uniq-read-only-p argv)))
      (should (eq case-fold-search ambient))))
  :doc "output files, post-input options and malformed or unknown variants fail closed"
  (dolist (argv
           '(("uniq" "input" "output") ("uniq" "-c" "input" "output")
             ("uniq" "--" "input" "-c") ("uniq" "--" "input" "--output=out")
             ("uniq" "input" "-c") ("uniq" "input" "--help")
             ("uniq" "input" "-c" "-") ("uniq" "input" "-f" "1")
             ("uniq" "input" "-" "extra") ("uniq" "--" "input" "-" "extra")
             ("uniq" "--output=out") ("uniq" "--unknown") ("uniq" "--cou")
             ("uniq" "-Q") ("uniq" "-cQ") ("uniq" "-2")
             ("uniq" "-f") ("uniq" "-cf") ("uniq" "-f" "input")
             ("uniq" "-f-1") ("uniq" "-f1x") ("uniq" "-f" "-1")
             ("uniq" "--skip-fields") ("uniq" "--skip-chars=")
             ("uniq" "--check-chars=1k") ("uniq" "--skip-fields" "-")
             ("uniq" "--group=") ("uniq" "--group=unknown")
             ("uniq" "--all-repeated=both") ("uniq" "--count=yes")
             ("uniq" "--group" "-c") ("uniq" "--group" "-d")
             ("uniq" "--group" "-D") ("uniq" "--group" "-u")
             ("uniq" "-cD") ("uniq" "--count" "--all-repeated")
             ("uniq" nil) ("uniq" 3) ("cat" "input") nil))
    (should-not (mevedel-bash-policy--uniq-read-only-p argv)))
  (should-not (mevedel-bash-policy--uniq-read-only-p
               (list "uniq" (concat "input" (string 0))))))

(mevedel-deftest mevedel-bash-policy-uniq-plan-fixture ()
  ,test
  (test)
  :doc "Plan rejects real uniq output writes while stdout forms leave the fixture unchanged"
  (skip-unless (executable-find "uniq"))
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-uniq-policy-" t)))
         (default-directory root)
         (process-environment (copy-sequence process-environment))
         (mevedel-permission-rules nil)
         (input (file-name-concat root "input")))
    (unwind-protect
        (progn
          (with-temp-file input (insert "one\none\ntwo\n"))
          (dolist (directive '(nil t))
            (let* ((session (mevedel-session--create
                             :authority-mode 'pid-lock :name "uniq-inspection"
                             :plan-mode (not directive)
                             :directive-planning
                             (when directive '(:directive-id "d1" :phase planning))))
                   (mevedel--current-request
                    (when directive
                      (mevedel-request--create :session session :plan-read-only t))))
              (dolist (mode '(ask edits full-auto))
                (let ((context (list :mode mode :session session :buckets nil
                                     :execution-directory root :allowed-roots (list root))))
                  (dolist (source '("uniq input" "uniq -c input" "uniq input -"
                                    "uniq -- input -"))
                    (should (eq 'allow (mevedel-bash-policy-check-permission
                                       source :permission-context context))))
                  (dolist (source '("uniq input output" "uniq -c input output"
                                    "uniq input -c" "uniq -- input -c"))
                    (should (eq 'deny (mevedel-bash-policy-check-permission
                                      source :permission-context context))))))))
          (dolist (posix '(nil "1"))
            (setenv "POSIXLY_CORRECT" posix)
            (dolist (args '(("input") ("input" "-") ("--" "input" "-")))
              (with-temp-buffer
                (should (equal (apply #'process-file "uniq" nil t nil args) 0))
                (should (equal (buffer-string) "one\ntwo\n"))))
            (should (equal (directory-files root nil "\\`[^.]") '("input"))))
          ;; Positive controls bypass policy intentionally, only inside our root.
          (setenv "POSIXLY_CORRECT" nil)
          (with-temp-buffer
            (should (equal (process-file "uniq" nil t nil "input" "output") 0))
            (should (equal (buffer-string) "")))
          (should (equal (with-temp-buffer
                           (insert-file-contents (file-name-concat root "output"))
                           (buffer-string))
                         "one\ntwo\n"))
          (setenv "POSIXLY_CORRECT" "1")
          (with-temp-buffer
            (should (equal (process-file "uniq" nil t nil "input" "-c") 0))
            (should (equal (buffer-string) "")))
          (should (equal (with-temp-buffer
                           (insert-file-contents (file-name-concat root "-c"))
                           (buffer-string))
                         "one\ntwo\n"))
          (should (equal (with-temp-buffer (insert-file-contents input) (buffer-string))
                         "one\none\ntwo\n")))
      (delete-directory root t))))

(mevedel-deftest mevedel-bash-policy-plan-inspection ()
  ,test
  (test)
  :doc "inspection families and compositions work in standalone and directive Plan"
  (dolist (directive '(nil t))
    (let* ((mevedel-permission-rules nil)
           (session (mevedel-session--create
                     :authority-mode 'pid-lock :name "inspection"
                     :plan-mode (not directive)
                     :directive-planning
                     (when directive '(:directive-id "d1" :phase planning))))
           (mevedel--current-request
            (when directive
              (mevedel-request--create :session session :plan-read-only t))))
      (dolist (mode '(ask edits full-auto))
        (let ((context (list :mode mode :session session :buckets nil
                             :execution-directory default-directory
                             :allowed-roots (list default-directory))))
          (dolist (source '("diff -u one two" "cmp one two"
                            "readlink -f file" "realpath file" "du -sh ."
                            "test -f file" "[ 1 -eq 2 ]"
                            "git status --short && git diff --stat"
                            "cat file | head -10 || pwd; ls"))
            (should (eq 'allow (mevedel-bash-policy-check-permission
                               source :permission-context context))))
          (dolist (source '("cat file >out" "git status && touch file"
                            "git reset --hard" "find . -delete"
                            "git diff --ext-diff" "unknown-reader file"))
            (should (eq 'deny (mevedel-bash-policy-check-permission
                              source :permission-context context))))))))
  :doc "retained agents inherit reading permission but not mutation authority"
  (let* ((session (mevedel-session--create
                   :authority-mode 'pid-lock :name "parent" :plan-mode t))
         (mevedel-permission-rules nil))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create :parent-session session))
      (let ((context '(:mode edits :buckets nil)))
        (should (eq 'allow (mevedel-bash-policy-check-permission
                           "git status --short && git diff --stat"
                           :permission-context context)))
        (should (eq 'deny (mevedel-bash-policy-check-permission
                          "git status && touch file"
                          :permission-context context))))))
  :doc "explicit denies still override reading classification"
  (let* ((session (mevedel-session--create
                   :authority-mode 'pid-lock :name "deny" :plan-mode t))
         (mevedel-permission-rules nil)
         (context '(:mode edits :buckets
                   ((:session ("Bash" :pattern "git diff:*" :action deny))))))
    (setq context (plist-put context :session session))
    (should (eq 'deny (mevedel-bash-policy-check-permission
                      "git status --short && git diff --stat"
                      :permission-context context)))))

(mevedel-deftest mevedel-bash-policy-commands-summary ()
  ,test
  (test)
  :doc "unique commands:
`mevedel-bash-policy-commands-summary' keeps unique commands unchanged"
  (should (equal "git, bash"
                 (mevedel-bash-policy-commands-summary
                  '("git" "bash"))))
  :doc "repeated commands:
`mevedel-bash-policy-commands-summary' counts repeated commands"
  (should (equal "git (6)"
                 (mevedel-bash-policy-commands-summary
                  '("git" "git" "git" "git" "git" "git"))))
  :doc "first-seen order:
`mevedel-bash-policy-commands-summary' preserves first-seen order"
  (should (equal "git (2), bash, make (3)"
                 (mevedel-bash-policy-commands-summary
                  '("git" "bash" "git" "make" "make" "make"))))
  :doc "invalid entries:
`mevedel-bash-policy-commands-summary' ignores invalid or empty entries"
  (should (equal "git (2)"
                 (mevedel-bash-policy-commands-summary
                  '("" nil git "git" "git"))))
  :doc "empty list:
`mevedel-bash-policy-commands-summary' returns nil for no commands"
  (should-not (mevedel-bash-policy-commands-summary nil)))

(mevedel-deftest mevedel-bash-policy-allow-patterns ()
  ,test
  (test)
  :doc "subcommand prefixes:
`mevedel-bash-policy-allow-patterns' generalizes stable subcommands"
  (should (equal '("git log:*")
                 (mevedel-bash-policy-allow-patterns
                  "git log --oneline --graph")))
  :doc "compound commands:
`mevedel-bash-policy-allow-patterns' returns one rule per segment"
  (should (equal '("pwd" "git log:*")
                 (mevedel-bash-policy-allow-patterns
                  "pwd && git log --oneline")))
  :doc "flag arguments:
`mevedel-bash-policy-allow-patterns' keeps exact command when token 2 is a flag"
  (should (equal '("pytest -q test/test-mevedel-tools.el")
                 (mevedel-bash-policy-allow-patterns
                  "pytest -q test/test-mevedel-tools.el")))
  :doc "safe env vars:
`mevedel-bash-policy-allow-patterns' skips safe env assignments"
  (should (equal '("npm run:*")
                 (mevedel-bash-policy-allow-patterns
                  "NODE_ENV=test npm run test")))
  :doc "unsafe env vars:
`mevedel-bash-policy-allow-patterns' keeps exact command with unknown env vars"
  (should (equal '("DOCKER_HOST=tcp://example docker ps")
                 (mevedel-bash-policy-allow-patterns
                  "DOCKER_HOST=tcp://example docker ps")))
  :doc "dangerous commands:
`mevedel-bash-policy-allow-patterns' does not generalize dangerous commands"
  (let ((mevedel-bash-dangerous-commands '("curl")))
    (should (equal '("curl get https://example.com")
                   (mevedel-bash-policy-allow-patterns
                    "curl get https://example.com")))))

(mevedel-deftest mevedel-bash-policy-reusable-operation-p ()
  ,test
  (test)
  :doc "literal dangerous commands are reusable"
  (let ((mevedel-bash-dangerous-commands '("rm")))
    (should
     (mevedel-bash-policy-reusable-operation-p "rm -rf /tmp/build")))
  :doc "dynamic dangerous commands are not reusable"
  (let ((mevedel-bash-dangerous-commands '("rm")))
    (should-not
     (mevedel-bash-policy-reusable-operation-p "rm -rf \"$TARGET\"")))
  :doc "glob-bearing commands are not reusable"
  (should-not
   (mevedel-bash-policy-reusable-operation-p "printf '%s' '*.tmp'")))

(mevedel-deftest mevedel-bash-policy-effective-sandbox-mode ()
  ,test
  (test)
  :doc "uses the permission context's session policy"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :sandbox-mode 'required)))
    (should
     (eq 'required
         (mevedel-bash-policy-effective-sandbox-mode
          (list :session session))))))

(mevedel-deftest mevedel-bash-policy-check-permission ()
  ,test
  (test)
  :doc "read-only policy:
\`mevedel-bash-policy-check-permission' allows recognized inspection"
  (let ((mevedel-permission-rules nil))
    (should (eq 'allow (mevedel-bash-policy-check-permission "pwd && cat file"))))
  :doc "Plan mode allows only recognized read-only Bash"
  (let* ((mevedel-permission-rules nil)
         (session (mevedel-session--create :authority-mode 'pid-lock :name "plan" :plan-mode t)))
    (dolist (mode '(ask edits full-auto))
      (let ((context (list :mode mode :session session :buckets nil)))
        (should (eq 'allow
                    (mevedel-bash-policy-check-permission
                     "pwd && cat file" :permission-context context)))
        (should (eq 'deny
                    (mevedel-bash-policy-check-permission
                     "make test" :permission-context context)))
        (should (eq 'deny
                    (mevedel-bash-policy-check-permission
                     "rm file"
                     :permission-context
                     (plist-put context :buckets
                                '((:session ("Bash" :pattern "rm file"
                                             :action allow))))))))))
  :doc "directive planning denies effectful Bash even in Full-auto"
  (let* ((mevedel-permission-rules nil)
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "directive-plan"
           :directive-planning '(:directive-id "d1" :phase planning)))
         (mevedel--current-request
          (mevedel-request--create :session session :plan-read-only t))
         (context (list :mode 'full-auto :session session :buckets nil)))
    (should (eq 'allow
                (mevedel-bash-policy-check-permission
                 "pwd" :permission-context context)))
    (should (eq 'deny
                (mevedel-bash-policy-check-permission
                 "make test" :permission-context context))))
  :doc "Plan mode follows a retained agent's parent session"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "plan" :plan-mode t))
         (mevedel-permission-rules nil))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create
                   :parent-session session))
      (dolist (mode '(ask edits full-auto))
        (let ((context
               (list :mode mode
                     :buckets
                     '((:session
                        ("Bash" :pattern "make test" :action allow))))))
          (should
           (eq 'deny
               (mevedel-bash-policy-check-permission
                "make test" :permission-context context)))))))
  :doc "argument-aware read-only policies:
\`mevedel-bash-policy-check-permission' allows safe inspection variants"
  (let ((mevedel-permission-rules nil))
    (dolist (command
             '("find . -name '*.el'"
               "git status"
               "git --no-pager log -1"
               "git diff -p"
               "git show HEAD"
               "git branch --show-current"
               "rg TODO src"
               "base64 file"
               "sed -n 1,5p file"
               "awk '{print $1}' file"))
      (should (eq 'allow
                  (mevedel-bash-policy-check-permission command)))))
  :doc "argument-aware unsafe or unproven policies:
\`mevedel-bash-policy-check-permission' asks for unproven command variants"
  (let ((mevedel-permission-rules nil))
    (dolist (command
             '("git diff --output=file"
               "git -c core.pager=cat log"
               "git --paginate log"
               "git branch new-name"
               "git branch -d old"
               "find . -delete"
               "find . -exec printf {} \\;"
               "rg --pre helper TODO"
               "rg --search-zip TODO"
               "base64 -o output file"
               "sed -n 1,5d file"
               "awk '{system(\"id\")}' file"
               "awk 'BEGIN { f = \"sys\" \"tem\"; @f(\"id\") }'"
               "awk '{print $1 > \"out\"}' file"))
      (should (eq 'ask
                  (mevedel-bash-policy-check-permission command)))))
  :doc "unknown policy:
\`mevedel-bash-policy-check-permission' asks for unknown commands"
  (let ((mevedel-permission-rules nil))
    (should (eq 'ask (mevedel-bash-policy-check-permission "make test"))))
  :doc "dangerous policy:
\`mevedel-bash-policy-check-permission' asks for dangerous commands"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm")))
    (should (eq 'ask (mevedel-bash-policy-check-permission "rm file"))))
  :doc "complex policy:
\`mevedel-bash-policy-check-permission' asks for complex syntax"
  (let ((mevedel-permission-rules nil))
    (should (eq 'ask (mevedel-bash-policy-check-permission "FOO=bar make test"))))
  :doc "session authority:
\`mevedel-bash-policy-check-permission' honors a direct dangerous allow"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm")))
    (should
     (eq 'allow
         (mevedel-bash-policy-check-permission
          "rm file"
          :permission-context
          '(:mode ask
                  :buckets ((:session . (("Bash" :pattern "rm *" :action allow)))))))))
  :doc "segment authority:
\`mevedel-bash-policy-check-permission' honors a direct dangerous segment allow"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm")))
    (should
     (eq 'allow
         (mevedel-bash-policy-check-permission
          "pwd && rm file"
          :permission-context
          '(:mode ask
                  :buckets ((:session . (("Bash" :pattern "rm *" :action allow)))))))))
  :doc "segment ask authority:
\`mevedel-bash-policy-check-permission' keeps an effective segment ask final"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm")))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "pwd && cat file && rm file"
          :permission-context
          '(:mode ask
                  :buckets
                  ((:session . (("Bash" :pattern "rm *" :action allow)
                                ("Bash" :pattern "cat *" :action ask)))))))))
  :doc "persistent authority:
\`mevedel-bash-policy-check-permission' honors a direct complex allow"
  (let ((mevedel-permission-rules nil))
    (should
     (eq 'allow
         (mevedel-bash-policy-check-permission
          "FOO=bar make test"
          :permission-context
          '(:mode ask
                  :buckets
                  ((:persistent .
                                (("Bash" :pattern "FOO=bar make test" :action allow)))))))))
  :doc "global authority:
\`mevedel-bash-policy-check-permission' treats configured global rules as direct"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "echo $HOME" :action allow))))
    (should (eq 'allow
                (mevedel-bash-policy-check-permission "echo $HOME"))))
  :doc "delegated dangerous rule:
\`mevedel-bash-policy-check-permission' ignores invocation authority for danger"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm")))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "rm file"
          :permission-context
          '(:mode ask
                  :buckets
                  ((:invocation . (("Bash" :pattern "rm *" :action allow)))))))))
  :doc "delegated complex rule:
\`mevedel-bash-policy-check-permission' ignores request authority for complexity"
  (let ((mevedel-permission-rules nil))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "FOO=bar make test"
          :permission-context
          '(:mode ask
                  :buckets
                  ((:request .
                             (("Bash" :pattern "FOO=bar make test" :action allow)))))))))
  :doc "delegated append assignment:
\`mevedel-bash-policy-check-permission' reserves append assignments for users"
  (let ((mevedel-permission-rules nil))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "FOO+=bar make test"
          :permission-context
          '(:mode ask
                  :buckets
                  ((:request .
                             (("Bash" :pattern "FOO+=bar make test" :action allow)))))))))
  :doc "delegated unknown rule:
\`mevedel-bash-policy-check-permission' permits ordinary delegated commands"
  (let ((mevedel-permission-rules nil))
    (should
     (eq 'allow
         (mevedel-bash-policy-check-permission
          "make test"
          :permission-context
          '(:mode ask
                  :buckets
                  ((:request . (("Bash" :pattern "make test" :action allow)))))))))
  :doc "generic deny across buckets:
\`mevedel-bash-policy-check-permission' keeps an outer user deny final"
  (let ((mevedel-permission-rules nil))
    (should
     (eq 'deny
         (mevedel-bash-policy-check-permission
          "make test"
          :permission-context
          '(:mode ask
                  :buckets
                  ((:request . (("Bash" :pattern "make test" :action allow)))
                   (:defcustom . (("Bash" :action deny)))))))))
  :doc "explicit deny:
\`mevedel-bash-policy-check-permission' keeps deny final"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm")))
    (should
     (eq 'deny
         (mevedel-bash-policy-check-permission
          "rm file"
          :permission-context
          '(:mode full-auto
                  :buckets
                  ((:session . (("Bash" :pattern "rm *" :action allow)))
                   (:persistent . (("Bash" :pattern "rm *" :action deny)))))))))
  :doc "explicit deny in complex syntax:
\`mevedel-bash-policy-check-permission' checks harvested command components"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands nil))
    (should
     (eq 'deny
         (mevedel-bash-policy-check-permission
          "echo $(rm file)"
          :permission-context
          '(:mode full-auto
                  :buckets
                  ((:persistent . (("Bash" :pattern "rm *" :action deny)))))))))
  :doc "explicit deny after a quoted parenthesis:
\`mevedel-bash-policy-check-permission' fully scans substitution bodies"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands nil))
    (should
     (eq 'deny
         (mevedel-bash-policy-check-permission
          "echo \"$(printf ')' && rm file)\""
          :permission-context
          '(:mode full-auto
                  :buckets
                  ((:persistent . (("Bash" :pattern "rm *" :action deny)))))))))
  :doc "explicit deny in a nested chain:
`mevedel-bash-policy-check-permission' checks substitution components"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands nil))
    (should
     (eq 'deny
         (mevedel-bash-policy-check-permission
          "echo \"$(pwd && rm file && echo x)\""
          :permission-context
          '(:mode full-auto
                  :buckets
                  ((:persistent . (("Bash" :pattern "rm *" :action deny)))))))))
  :doc "explicit deny normalizes executable paths and quoted assignments:
\`mevedel-bash-policy-check-permission' cannot disguise a denied command"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands nil)
        (context
         '(:mode full-auto
                 :buckets
                 ((:persistent . (("Bash" :pattern "rm *" :action deny)))))))
    (dolist (command '("/bin/rm file" "FOO='bar baz' rm file"))
      (should
       (eq 'deny
           (mevedel-bash-policy-check-permission
            command :permission-context context)))))
  :doc "full-auto:
\`mevedel-bash-policy-check-permission' bypasses heuristic prompts"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm")))
    (should
     (eq 'allow
         (mevedel-bash-policy-check-permission
          "rm file" :permission-context '(:mode full-auto :buckets nil))))
    (should
     (eq 'allow
         (mevedel-bash-policy-check-permission
          "echo $HOME" :permission-context '(:mode full-auto :buckets nil)))))
  :doc "one-shot mutations:
\`mevedel-bash-policy-check-permission' keeps inspection automatic but asks
for effects despite reusable authority"
  (let ((mevedel-permission-rules nil)
        (context
         '(:mode ask
                 :one-shot-mutations-p t
                 :buckets
                 ((:request . (("Bash" :pattern "make test" :action allow)))))))
    (should
     (eq 'allow
         (mevedel-bash-policy-check-permission
          "rg TODO" :permission-context context)))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "make test" :permission-context context))))
  :doc "protected path:
\`mevedel-bash-policy-check-permission' asks before protected resources"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "cat .git/config"
          :permission-context
          `(:mode ask
                  :buckets nil
                  :execution-directory ,default-directory)))))
  :doc "protected symlink:
\`mevedel-bash-policy-check-permission' checks the resolved resource"
  (let* ((root (make-temp-file "mevedel-bash-protected-link-" t))
         (git-dir (file-name-concat root ".git"))
         (config (file-name-concat git-dir "config"))
         (link (file-name-concat root "innocent"))
         (default-directory (file-name-as-directory root))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory git-dir)
          (write-region "secret" nil config nil 'silent)
          (make-symbolic-link config link)
          (should
           (eq 'ask
               (mevedel-bash-policy-check-permission
                (format "cat %s" link)
                :permission-context
                `(:mode ask
                        :buckets nil
                        :execution-directory ,default-directory)))))
      (delete-directory root t)))
  :doc "continued protected path:
\`mevedel-bash-policy-check-permission' checks Bash line continuations"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths '(("~/.ssh/**" . inaccessible))))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          (concat "cat ~/.ss\\" "\n" "h/id_rsa")
          :permission-context
          `(:mode ask
                  :buckets nil
                  :execution-directory ,default-directory)))))
  :doc "protected path inside substitution:
`mevedel-bash-policy-check-permission' asks before nested protected resources"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "echo \"$(cat .git/config)\""
          :permission-context
          `(:mode ask
                  :buckets nil
                  :execution-directory ,default-directory)))))
  :doc "protected path after a quoted parenthesis:
\`mevedel-bash-policy-check-permission' fully scans nested protected resources"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "echo \"$(printf ')' && cat .git/config && echo x)\""
          :permission-context
          `(:mode ask
                  :buckets nil
                  :execution-directory ,default-directory)))))
  :doc "complex protected path:
\`mevedel-bash-policy-check-permission' keeps resource checks after direct allow"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "FOO=bar cat ~/.ssh/key" :action allow)))
        (mevedel-protected-paths '(("**/.ssh/**" . inaccessible)))
        (context
         `(:mode ask
                 :execution-directory ,default-directory
                 :buckets
                 ((:defcustom .
                              (("Bash" :pattern "FOO=bar cat ~/.ssh/key" :action allow)))))))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "FOO=bar cat ~/.ssh/key"
          :permission-context context))))
  :doc "direct user patterns deliberately authorize dangerous commands"
  (let ((mevedel-bash-dangerous-commands '("rm"))
        (mevedel-permission-rules '(("Bash" :pattern "rm *" :action allow))))
    (should (eq 'allow
                (mevedel-bash-policy-check-permission "rm /tmp/foo"))))

  :doc "direct user patterns deliberately authorize complex syntax"
  (let ((mevedel-permission-rules '(("Bash" :pattern "echo *" :action allow))))
    (should (eq 'allow
                (mevedel-bash-policy-check-permission
                 "echo $VAR"))))

  :doc "explicit deny rejects dangerous commands"
  (let ((mevedel-permission-rules '(("Bash" :pattern "rm *" :action deny))))
    (should (eq 'deny
                (mevedel-bash-policy-check-permission
                 "rm /tmp/foo"))))

  :doc "decision metadata distinguishes rule, mode, and one-shot asks"
  (let ((mevedel-permission-rules nil))
    (dolist (case '((ask nil nil mode)
                    (edits (("Bash" :pattern "make *" :action ask)) nil rule)
                    (edits nil t one-shot-mutation)))
      (pcase-let ((`(,mode ,rules ,one-shot ,via) case))
        (let ((decision
               (mevedel-bash-policy-check-permission
                "make test" :metadata-p t
                :permission-context
                (list :mode mode :buckets (list (cons :session rules))
                      :one-shot-mutations-p one-shot))))
          (should (eq (plist-get decision :outcome) 'ask))
          (should (eq (plist-get decision :via) via))))))

  :doc "captured context fences ambient request and invocation authority"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (mevedel--current-request
          (mevedel-request--create
           :session session
           :skill-permission-rules
           '(("Bash" :pattern "make test" :action allow))))
         (mevedel--agent-invocation
          (mevedel-agent-invocation--create
           :skill-permission-rules
           '(("Bash" :pattern "make test" :action allow))))
         (mevedel-permission-rules nil))
    (should
     (eq 'ask
         (mevedel-bash-policy-check-permission
          "make test"
          :permission-context
          `(:session nil :execution-directory ,default-directory)))))

  :doc "skill bucket allows Bash even without session/global rule"
  (let* ((root (make-temp-file "mevedel-bash-policy-skill-" t))
         (ws (mevedel-workspace--create
              :type 'file :id "b" :root root :name "b"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create
                   :session session
                   :skill-permission-rules
                   '(("Bash" :pattern "gh *" :action allow))))
         (mevedel-permission-rules nil))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setq-local mevedel--current-request request)
          (should (eq 'allow
                      (mevedel-bash-policy-check-permission
                       "gh issue list"))))
      (delete-directory root t)))

  :doc "session deny beats invocation/request skill allow on Bash"
  (let* ((root (make-temp-file "mevedel-bash-policy-deny-" t))
         (ws (mevedel-workspace--create
              :type 'file :id "b2" :root root :name "b2"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create
                   "main" ws))
         (mevedel-permission-rules nil))
    (unwind-protect
        (progn
          (setf (mevedel-session-permission-rules session)
                '(("Bash" :pattern "rm *" :action deny)))
          (let ((request (mevedel-request--create
                          :session session
                          :skill-permission-rules
                          '(("Bash" :action allow)))))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (setq-local mevedel--current-request request)
              (should (eq 'deny
                          (mevedel-bash-policy-check-permission
                           "rm /tmp/foo"))))))
      (delete-directory root t))))


(mevedel-deftest mevedel-bash-policy-missing-resource-paths ()
  ,test
  (test)
  :doc "requires exact authority only for resources outside allowed roots"
  (let* ((parent (make-temp-file "mevedel-bash-resource-paths-" t))
         (root (file-name-concat parent "workspace"))
         (default-directory (file-name-as-directory root))
         (context `(:allowed-roots (,root) :resource-grants nil))
         (parent-path (directory-file-name parent)))
    (unwind-protect
        (progn
          (make-directory root)
          (should-not
           (mevedel-bash-policy-missing-resource-paths
            "rg TODO ." context '(:level use-default)))
          (unless (eq system-type 'windows-nt)
            (should-not
             (mevedel-bash-policy-missing-resource-paths
              "diff /dev/null ./mevedel.el" context '(:level use-default))))
          (should
           (equal (list parent-path)
                  (mevedel-bash-policy-missing-resource-paths
                   "rg TODO .." context '(:level use-default))))
          (should-not
           (mevedel-bash-policy-missing-resource-paths
            "rg TODO .." context
            `(:level additive
                     :additional-permissions
                     (:file-system ((:path ,parent-path :access read)))))))
      (delete-directory parent t)))
  :doc "a recursive grant on an ancestor covers descendant resources"
  (let* ((parent (make-temp-file "mevedel-bash-resource-tree-" t))
         (root (file-name-concat parent "workspace"))
         (tree (file-name-concat parent "tree"))
         (leaf (file-name-concat tree "sub" "file"))
         (default-directory (file-name-as-directory root)))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory (file-name-directory leaf) t)
          (write-region "leaf" nil leaf nil 'silent)
          (should
           (equal (list leaf)
                  (mevedel-bash-policy-missing-resource-paths
                   (format "cat %s" leaf)
                   `(:allowed-roots (,root)
                     :resource-grants ((:path ,tree :access read)))
                   '(:level use-default))))
          (should-not
           (mevedel-bash-policy-missing-resource-paths
            (format "cat %s" leaf)
            `(:allowed-roots (,root)
              :resource-grants ((:path ,tree :access read :recursive t)))
            '(:level use-default))))
      (delete-directory parent t)))
  :doc "resolves symlinks before allowed-root and exact-grant checks"
  (let* ((parent (make-temp-file "mevedel-bash-resource-link-" t))
         (root (file-name-concat parent "workspace"))
         (secret (file-name-concat parent "secret"))
         (link (file-name-concat root "innocent"))
         (default-directory (file-name-as-directory root))
         (context `(:allowed-roots (,root) :resource-grants nil)))
    (unwind-protect
        (progn
          (make-directory root)
          (write-region "secret" nil secret nil 'silent)
          (make-symbolic-link secret link)
          (should
           (equal (list secret)
                  (mevedel-bash-policy-missing-resource-paths
                   (format "cat %s" link)
                   context '(:level use-default))))
          (should
           (equal (list secret)
                  (mevedel-bash-policy-missing-resource-paths
                   (format "cat %s" link)
                   context
                   `(:level additive
                            :additional-permissions
                            (:file-system ((:path ,link :access read)))))))
          (should-not
           (mevedel-bash-policy-missing-resource-paths
            (format "cat %s" link)
            context
            `(:level additive
                     :additional-permissions
                     (:file-system ((:path ,secret :access read)))))))
      (delete-directory parent t)))
  :doc "remote resources stay target-native after symlink resolution"
  (let* ((parent (make-temp-file "mevedel-bash-remote-resource-" t))
         (root (file-name-concat parent "workspace"))
         (outside (file-name-concat parent "outside"))
         (link (file-name-concat root "linked"))
         (native-resource (file-name-concat link "missing" "file"))
         (canonical-resource (file-name-concat outside "missing" "file"))
         (remote-root (format "/mevedelmock:resource:%s/" root)))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory outside)
          (make-symbolic-link outside link)
          (mevedel-test--with-local-shell-tramp '("resource")
                                                (let* ((target (mevedel-execution-target-create remote-root))
                                                       (session
                                                        (mevedel-session--create
                                                         :authority-mode 'pid-lock
                                                         :execution-target target
                                                         :working-directory remote-root))
                                                       (context `(:session ,session
                                                                           :allowed-roots (,remote-root)
                                                                           :resource-grants nil)))
                                                  (should
                                                   (equal (list canonical-resource)
                                                          (mevedel-bash-policy-missing-resource-paths
                                                           (format "rg TODO %s" native-resource)
                                                           context '(:level use-default))))
                                                  (should-not
                                                   (mevedel-bash-policy-missing-resource-paths
                                                    "diff /dev/null ." context '(:level use-default)))
                                                  (let ((default-directory remote-root))
                                                    (should
                                                     (equal (list canonical-resource)
                                                            (mevedel-bash-policy-missing-resource-paths
                                                             (format "rg TODO %s" native-resource)
                                                             `(:allowed-roots (,remote-root)
                                                                              :resource-grants nil)
                                                             '(:level use-default))))))))
      (delete-directory parent t)))

  :doc "captured nil session fences ambient resource resolution"
  (let* ((parent (make-temp-file "mevedel-bash-context-fence-" t))
         (ambient (file-name-concat parent "ambient"))
         (captured (file-name-concat parent "captured"))
         (default-directory (file-name-as-directory captured))
         (mevedel--session
          (mevedel-session--create :working-directory ambient))
         (context `(:session nil
                            :execution-directory ,captured
                            :allowed-roots (,captured)
                            :resource-grants nil)))
    (unwind-protect
        (progn
          (make-directory ambient)
          (make-directory captured)
          (should-not
           (mevedel-bash-policy-missing-resource-paths
            "cat ./file" context '(:level use-default))))
      (delete-directory parent t))))

(provide 'test-mevedel-bash-policy)

;;; test-mevedel-bash-policy.el ends here
