;;; test-mevedel-bash-git.el --- Git inspection tests -*- lexical-binding: t -*-

;;; Commentary:

;; Policy and real isolated Git tests, including hostile helper configuration
;; and metadata snapshots.  No network or real repositories are mutated.

;;; Code:

(require 'mevedel-bash-git)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-bash-git-test--run (argv)
  "Run executable ARGV, returning output and asserting a successful exit."
  (with-temp-buffer
    (let ((status (apply #'process-file (car argv) nil t nil (cdr argv))))
      (should (equal status 0))
      (buffer-string))))

(defun mevedel-bash-git-test--snapshot (root)
  "Return contents, modes and modification/change times of ROOT's tree."
  (mapcar
   (lambda (file)
     (let ((attributes (file-attributes file 'integer)))
       (list (file-relative-name file root)
             (file-attribute-type attributes)
             (file-attribute-modes attributes)
             (file-attribute-modification-time attributes)
             (file-attribute-status-change-time attributes)
             (unless (file-directory-p file)
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally file)
                 (secure-hash 'sha256 (current-buffer)))))))
   (sort (cons root (directory-files-recursively root "." t)) #'string<)))

(mevedel-deftest mevedel-bash-git-prepare/target-environment ()
  ,test
  (test)
  :doc "clears target-only Git controls and fails closed on enumeration errors"
  (let* ((root (make-temp-file "mevedel-git-target-env-" t))
         (default-directory (file-name-as-directory root))
         (git (file-name-concat root "git"))
         (env (file-name-concat root "env"))
         (marker (file-name-concat root "executed"))
         ;; Preparation cannot know these entries: they arrive at the target
         ;; afterward, including a name Bash cannot import as a variable.
         (prepared (mevedel-bash-git-prepare '("git" "rev-parse" "--show-toplevel")))
         (process-environment
          (append (list "GIT_TEST_TARGET_ONLY=value\nwith=delimiters"
                        "GIT_TEST-INVALID_NAME=another=value"
                        "GIT_TRACE2=hostile" "PAGER=hostile" "SSH_ASKPASS=hostile"
                        "MEVEDEL_TEST_KEEP=value\nwith=delimiters"
                        (concat "PATH=" root path-separator (getenv "PATH")))
                  process-environment))
         (exec-path (cons root exec-path)))
    (unwind-protect
        (progn
          ;; A trusted Git spy checks the actual launched environment. Real
          ;; Git and repository-preservation coverage remain separate below.
          (with-temp-file git
            (insert "#!/bin/sh\nprintf ran > " (shell-quote-argument marker)
                    "\nexec env -0\n"))
          (set-file-modes git #o700)
          (let ((original (mevedel-bash-git-test--run '("git"))))
            (should (string-search "GIT_TEST_TARGET_ONLY=" original))
            (should (string-search "GIT_TEST-INVALID_NAME=" original)))
          (delete-file marker)
          (let ((entries (split-string (mevedel-bash-git-test--run prepared) "\0")))
            (should (file-exists-p marker))
            (should-not (cl-some (lambda (entry)
                                  (or (string-prefix-p "GIT_TEST" entry)
                                      (string-prefix-p "PAGER=" entry)
                                      (string-prefix-p "SSH_ASKPASS=" entry)))
                                entries))
            (should (member "MEVEDEL_TEST_KEEP=value\nwith=delimiters" entries))
            (should (member "GIT_OPTIONAL_LOCKS=0" entries))
            (should (member "GIT_TRACE2=0" entries))
            (should (member "GIT_TERMINAL_PROMPT=0" entries)))
          (delete-file marker)
          (with-temp-file env (insert "#!/bin/sh\nexit 7\n"))
          (set-file-modes env #o700)
          (with-temp-buffer
            (should (equal 7 (apply #'process-file (car prepared) nil t nil
                                   (cdr prepared)))))
          (should-not (file-exists-p marker)))
      (delete-directory root t))))

(mevedel-deftest mevedel-bash-git--value-p ()
  ,test
  (test)
  :doc "bounds and value domains are explicit"
  (dolist (value '("0" "1" "1000000"))
    (should (mevedel-bash-git--value-p value '(number))))
  (dolist (value '("" "-1" "1x" "1000001" "999999999999999999999999"))
    (should-not (mevedel-bash-git--value-p value '(number))))
  (should (mevedel-bash-git--value-p "auto" '(enum "always" "auto" "never")))
  (should-not (mevedel-bash-git--value-p "helper" '(enum "auto" "never")))
  (should (mevedel-bash-git--value-p "abcd" '(hex)))
  (should-not (mevedel-bash-git--value-p "abcdz" '(hex)))
  (should (mevedel-bash-git--value-p "HEAD" '(operand)))
  (should-not (mevedel-bash-git--value-p "--show-signature" '(operand)))
  (should (mevedel-bash-git--value-p "-pattern" '(text)))
  (should (mevedel-bash-git--value-p "%h %s" '(format)))
  (should (mevedel-bash-git--value-p "format:literal" '(pretty)))
  (should (mevedel-bash-git--value-p "medium" '(pretty)))
  (should-not (mevedel-bash-git--value-p "hostile-alias" '(pretty)))
  (should-not (mevedel-bash-git--value-p "-*signature:grade" '(sort)))
  (should-not (mevedel-bash-git--value-p "%G?" '(format)))
  (should-not (mevedel-bash-git--value-p "%(signature:grade)" '(format)))
  (should-not (mevedel-bash-git--value-p "%(*signature:grade)" '(format)))
  (should-not (mevedel-bash-git--value-p "x" '(unknown))))

(mevedel-deftest mevedel-bash-git--parse ()
  ,test
  (test)
  :doc "preserves argv and consumes joined short flags without losing operands"
  (let* ((argv '("git" "-C" "directory with spaces" "--no-pager"
                 "log" "-n2" "-U3" "HEAD" "--" "--ext-diff"))
         (copy (copy-tree argv)))
    (should (equal (mevedel-bash-git--parse argv)
                   '(("-C" "directory with spaces" "--no-pager")
                     "log" ("-n2" "-U3" "HEAD" "--" "--ext-diff"))))
    (should (equal argv copy)))
  :doc "cat-file and merge-base enforce finite operand modes"
  (dolist (argv '(("git" "cat-file" "-p")
                  ("git" "cat-file" "-p" "-s" "HEAD")
                  ("git" "cat-file" "blob" "HEAD:file" "extra")
                  ("git" "ls-tree")
                  ("git" "merge-base" "HEAD")
                  ("git" "merge-base" "--is-ancestor" "A" "B" "C")
                  ("git" "merge-base" "--fork-point" "A" "B" "C")))
    (should-error (mevedel-bash-git--parse argv))))

(mevedel-deftest mevedel-bash-git-read-only-p ()
  ,test
  (test)
  :doc "accepts ordinary bounded inspection flags in every supported family"
  (dolist (argv
           '(("git" "status") ("git" "status" "--short" "--branch" "-uall")
             ("git" "-C" "repo with spaces" "-C" "subdir" "status" "--porcelain=2")
             ("git" "--no-pager" "diff" "--stat")
             ("git" "diff" "--cached" "-U3" "-M" "HEAD" "--" "--output=file")
             ("git" "diff" "--name-only" "--" "$(literal)")
             ("git" "log" "--oneline" "--graph" "-n" "10" "--all")
             ("git" "log" "--format=%h %s" "--since=2026-01-01" "-10")
             ("git" "show" "--stat" "HEAD:sample.dat")
             ("git" "show" "--pretty" "format:%h" "--no-patch" "HEAD")
             ("git" "rev-parse" "--verify" "HEAD^{commit}")
             ("git" "rev-parse" "--show-toplevel" "--local-env-vars")
             ("git" "rev-parse" "--short=12" "HEAD")
             ("git" "rev-parse" "--path-format=absolute" "--git-path" "objects")
             ("git" "ls-files" "--stage" "-z" "--" "--filters")
             ("git" "ls-files" "--others" "--exclude-standard")
             ("git" "ls-tree" "-r" "--name-only" "HEAD" "--" "--output")
             ("git" "cat-file" "-p" "HEAD")
             ("git" "cat-file" "blob" "HEAD:sample.dat")
             ("git" "merge-base" "HEAD" "HEAD~1")
             ("git" "merge-base" "--is-ancestor" "HEAD~1" "HEAD")
             ("git" "merge-base" "--fork-point" "main")
             ("git" "for-each-ref" "--count=10" "--format=%(refname)" "refs/heads/")
             ("git" "branch" "--list" "-vv" "main*")
             ("git" "branch" "--show-current")))
    (should (mevedel-bash-git-read-only-p argv)))
  :doc "rejects mutation, output, helpers, abbreviations and unsupported modes"
  (dolist (argv
           '(("git" "pull") ("git" "add" ".") ("git" "config" "--list")
             ("git" "-c" "core.pager=helper" "status")
             ("git" "--config-env=core.pager=HELPER" "status")
             ("git" "--exec-path=/tmp" "status")
             ("git" "--git-dir=/tmp" "status")
             ("git" "--paginate" "log") ("git" "-C")
             ("git" "-C" "" "status") ("git" "status" "--shor")
             ("git" "diff" "--output=result") ("git" "diff" "--output" "result")
             ("git" "diff" "--ext-diff") ("git" "diff" "--textconv")
             ("git" "diff" "--no-index" "a" "b")
             ("git" "diff" "--submodule=diff")
             ("git" "log" "--show-signature") ("git" "log" "--format=%G?")
             ("git" "log" "--format=hostile-alias")
             ("git" "show" "--pretty=hostile-alias")
             ("git" "show" "--pretty=format:%GG")
             ("git" "log" "--alternate-refs") ("git" "log" "--reflog")
             ("git" "rev-parse" "--parseopt")
             ("git" "rev-parse" "--sq-quote" "--keep-dashdash")
             ("git" "ls-files" "--recurse-submodules")
             ("git" "cat-file" "--filters" "HEAD:file")
             ("git" "cat-file" "--textconv" "HEAD:file")
             ("git" "cat-file" "--batch") ("git" "cat-file" "--batch-command")
             ("git" "for-each-ref" "--stdin")
             ("git" "for-each-ref" "--format=%(signature:signer)")
             ("git" "for-each-ref" "--sort=signature:grade")
             ("git" "branch" "--list" "--sort=-*signature:grade")
             ("git" "branch") ("git" "branch" "new-branch")
             ("git" "branch" "--list" "--delete" "main")
             ("git" "branch" "--list" "--edit-description")
             ("git" "branch" "--show-current" "new-branch")
             ("git" "log" "--max-count=-1") ("git" "log" "-n")
             ("git" "show" "--format=") ("git" "rev-parse" "--git-path")
             ("git" "diff" "--unified=1000001")))
    (should-not (mevedel-bash-git-read-only-p argv))
    (should-error (mevedel-bash-git-prepare argv)))
  :doc "rejects malformed vectors and noncanonical executables"
  (dolist (argv (list nil '("git") '("git" "status" 1)
                     '("git" . "status") '["git" "status"]
                     '("/tmp/git" "status") '("git" "-C" "~/repo" "status")
                     (list "git" "status" (concat "x" (string 0) "y"))))
    (should-not (mevedel-bash-git-read-only-p argv))))

(mevedel-deftest mevedel-bash-git-prepare ()
  ,test
  (test)
  :doc "hardening precedes literal operands and does not mutate input or environment"
  (let* ((process-environment (cons "GIT_TEST_UNDOCUMENTED=unsafe" process-environment))
         (environment (copy-sequence process-environment))
         (argv '("git" "-C" "some repo" "diff" "--stat" "--" "--ext-diff"))
         (prepared (mevedel-bash-git-prepare argv)))
    (should (equal (car prepared) "bash"))
    (should (equal (nth 5 prepared) mevedel-bash-git--environment-guard))
    (should (equal process-environment environment))
    (should (equal (last prepared 9)
                   '("-C" "some repo" "diff" "--no-ext-diff" "--no-textconv"
                     "--stat" "--ignore-submodules=all" "--" "--ext-diff")))
    (should (equal argv '("git" "-C" "some repo" "diff" "--stat" "--" "--ext-diff"))))
  :doc "trusted temporary roots are separate launcher arguments and invalid roots fail closed"
  (let* ((argv '("git" "status" "--short"))
         (root "/native target/tmp with spaces")
         (prepared (mevedel-bash-git-prepare argv root))
         (launch (member "mevedel-git-inspection" prepared)))
    (should (equal (nth 2 launch) root))
    (should (equal (nth 3 launch) "git"))
    (should (equal argv '("git" "status" "--short"))))
  (dolist (root (list t "" "relative/tmp" "~/tmp" (concat "/tmp/" (string 0))))
    (should-error (mevedel-bash-git-prepare '("git" "status") root)))
  :doc "explicit temporary roots override unusable TMPDIR and never fall back when unavailable"
  (let* ((real-mktemp (or (executable-find "mktemp") (ert-skip "Mktemp unavailable")))
         (root (progn
                 (skip-unless (and (executable-find "git") (executable-find "env")
                                   (executable-find "bash")))
                 (file-name-as-directory (make-temp-file "mevedel-git-temporary-root-" t))))
         (repo (file-name-concat root "repo"))
         (temporary-root (file-name-concat root "tmp 'quoted ; $(literal)"))
         (missing-root (file-name-concat root "unavailable"))
         (bin (file-name-concat root "bin"))
         (record (file-name-concat root "mktemp-template"))
         (default-directory root)
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (mevedel-bash-git-test--run (list "git" "init" "-q" "-b" "main" repo))
          (make-directory temporary-root)
          (make-directory bin)
          (with-temp-file (file-name-concat bin "mktemp")
            (insert "#!/bin/sh\nprintf '%s\\n' \"$2\" > " (shell-quote-argument record)
                    "\nexec " (shell-quote-argument real-mktemp) " \"$@\"\n"))
          (set-file-modes (file-name-concat bin "mktemp") #o700)
          (setenv "PATH" (concat bin path-separator (getenv "PATH")))
          (setenv "TMPDIR" missing-root)
          (let ((before (mevedel-bash-git-test--snapshot repo)))
            (mevedel-bash-git-test--run
             (mevedel-bash-git-prepare (list "git" "-C" repo "status" "--short") temporary-root))
            (should (equal (concat temporary-root "/mevedel-git-inspection.XXXXXXXXXX\n")
                           (with-temp-buffer
                             (insert-file-contents record)
                             (buffer-string))))
            (should (equal (directory-files temporary-root nil "\\`[^.]") nil))
            (should (equal before (mevedel-bash-git-test--snapshot repo)))
            ;; A valid default must not rescue an explicitly unavailable root.
            (setenv "TMPDIR" temporary-root)
            (with-temp-buffer
              (let ((argv (mevedel-bash-git-prepare
                           (list "git" "-C" repo "status" "--short") missing-root)))
                (should-not (equal (apply #'process-file (car argv) nil t nil (cdr argv)) 0))))
            (should-not (file-exists-p missing-root))
            (should (equal (directory-files temporary-root nil "\\`[^.]") nil))
            (should (equal before (mevedel-bash-git-test--snapshot repo)))))
      (delete-directory root t)))
  :doc "real Git keeps config semantics but never executes hostile helpers or changes metadata"
  (let* ((root (progn
                 (skip-unless (and (executable-find "git") (executable-find "env")
                                   (executable-find "bash")))
                 (file-name-as-directory (make-temp-file "mevedel-git-policy-" t))))
         (repo (file-name-concat root "repo with spaces"))
         (helper (file-name-concat root "hostile-helper"))
         (marker (file-name-concat root "helper-ran"))
         (trace (file-name-concat root "trace-written"))
         (process-environment (copy-sequence process-environment))
         (default-directory root))
    (unwind-protect
        (progn
          (setenv "HOME" root)
          (setenv "XDG_CONFIG_HOME" (file-name-concat root ".config"))
          (mevedel-bash-git-test--run (list "git" "init" "-q" "-b" "main" repo))
          (setq default-directory (file-name-as-directory repo))
          (mevedel-bash-git-test--run '("git" "config" "user.name" "Test"))
          (mevedel-bash-git-test--run '("git" "config" "user.email" "test@example.test"))
          (with-temp-file (file-name-concat repo ".gitattributes")
            (insert "*.dat diff=hostile filter=hostile\n"))
          (with-temp-file (file-name-concat repo "sample.dat") (insert "one\n"))
          (with-temp-file (file-name-concat repo "--ext-diff") (insert "one\n"))
          (mevedel-bash-git-test--run '("git" "add" "."))
          (mevedel-bash-git-test--run '("git" "commit" "-q" "-m" "base"))
          (with-temp-file (file-name-concat repo "sample.dat") (insert "one\ntwo\n"))
          (with-temp-file helper
            (insert "#!/bin/sh\n: > " (shell-quote-argument marker) "\nexit 1\n"))
          (set-file-modes helper #o700)
          ;; Install a commit with a fake signature: verification would launch
          ;; gpg.program, even though signing itself never runs in this fixture.
          (let* ((commit (mevedel-bash-git-test--run '("git" "cat-file" "commit" "HEAD")))
                 (signed (replace-regexp-in-string
                          "\n\n" "\ngpgsig -----BEGIN PGP SIGNATURE-----\n fake\n -----END PGP SIGNATURE-----\n\n"
                          commit t t))
                 (input (file-name-concat root "signed-commit")))
            (with-temp-file input (insert signed))
            (let ((sha (string-trim
                        (mevedel-bash-git-test--run
                         (list "git" "hash-object" "-t" "commit" "-w" input)))))
              (mevedel-bash-git-test--run (list "git" "update-ref" "refs/heads/main" sha))))
          (dolist (key '("core.pager" "pager.status" "pager.diff" "pager.log" "pager.show"
                         "core.fsmonitor" "diff.external" "diff.hostile.command"
                         "diff.hostile.textconv"
                         "gpg.program" "core.alternateRefsCommand"))
            (mevedel-bash-git-test--run (list "git" "config" key helper)))
          (dolist (key '("trace2.normalTarget" "trace2.eventTarget" "trace2.perfTarget"))
            (mevedel-bash-git-test--run (list "git" "config" "--global" key trace)))
          (mevedel-bash-git-test--run '("git" "config" "log.showSignature" "true"))
          (mevedel-bash-git-test--run '("git" "config" "format.pretty" "%G?"))
          (mevedel-bash-git-test--run '("git" "config" "pretty.hostile-alias" "%G?"))
          (mevedel-bash-git-test--run '("git" "config" "branch.sort" "signature:grade"))
          (with-temp-file (file-name-concat repo ".git" "included.conf")
            (insert "[status]\n short\n[custom \"section.with.dots\"]\n"
                    " value = \"quotes\\\" backslash\\\\ tabs\\t newline\\n\"\n"))
          (mevedel-bash-git-test--run '("git" "config" "include.path" "included.conf"))
          (dolist (name '("status" "diff" "log" "show"))
            (mevedel-bash-git-test--run
             (list "git" "config" (concat "alias." name) (concat "!" helper))))
          ;; Global LFS-style definitions alone must not block a repository
          ;; whose attributes never select those drivers.
          (mevedel-bash-git-test--run
           (list "git" "config" "--global" "filter.inactive.clean" helper))
          (mevedel-bash-git-test--run
           (list "git" "config" "--global" "filter.inactive.process" helper))
          ;; Positive controls prove the fake signature reaches the configured
          ;; verifier through default formats, named aliases, and ref sorting.
          (dolist (args '(("log" "-1") ("log" "-1" "--format=hostile-alias")
                          ("branch" "--list")
                          ("for-each-ref" "--sort=signature:grade")))
            (with-temp-buffer
              (apply #'process-file "git" nil t nil
                     "--no-pager" "-c" "core.fsmonitor=false" args))
            (should (file-exists-p marker))
            (delete-file marker))
          (let* ((before (mevedel-bash-git-test--snapshot root))
                 (process-environment (copy-sequence process-environment)))
            (dolist (name '("GIT_PAGER" "PAGER" "GIT_EXTERNAL_DIFF" "GIT_ASKPASS"
                            "SSH_ASKPASS" "GIT_SSH_COMMAND"))
              (setenv name helper))
            (dolist (name '("GIT_TRACE" "GIT_TRACE2" "GIT_TRACE2_EVENT" "GIT_TRACE2_PERF"))
              (setenv name trace))
            (setenv "GIT_CONFIG_COUNT" "1")
            (setenv "GIT_CONFIG_KEY_0" "log.showSignature")
            (setenv "GIT_CONFIG_VALUE_0" "true")
            (setenv "GIT_CONFIG_PARAMETERS" "'core.fsmonitor'='hostile'")
            (setenv "BASH_ENV" helper)
            (setenv "ENV" helper)
            (setenv "BASH_FUNC_git%%" (concat "() { : > " (shell-quote-argument marker) "; }"))
            (dolist (args '(("status") ("diff" "--stat")
                            ("diff" "--" "--ext-diff")
                            ("log" "-1") ("log" "-1" "--format=%h %s") ("show" "HEAD")
                            ("rev-parse" "--local-env-vars")
                            ("ls-files" "--stage") ("ls-tree" "-r" "HEAD")
                            ("cat-file" "-p" "HEAD:sample.dat")
                            ("merge-base" "HEAD" "HEAD")
                            ("for-each-ref" "--count=2" "--format=%(refname)")
                            ("branch" "--list") ("branch" "--show-current")))
              (mevedel-bash-git-test--run
               (mevedel-bash-git-prepare (append (list "git" "-C" repo) args)))
              (ert-info ((format "Hardened Git %S" args))
                (should-not (file-exists-p marker))))
            (let ((output (mevedel-bash-git-test--run
                           (mevedel-bash-git-prepare '("git" "status")))))
              (should (string-match-p " M sample.dat" output))
              (should-not (string-match-p "On branch" output)))
            (should-not (file-exists-p marker))
            (should (equal before (mevedel-bash-git-test--snapshot root))))
          ;; Worktree filters cannot be disabled without changing legitimate
          ;; comparison semantics, so these routes refuse before inspection.
          (dolist (key '("filter.hostile.clean" "filter.hostile.process"))
            (mevedel-bash-git-test--run (list "git" "config" key helper)))
          (let ((before (mevedel-bash-git-test--snapshot root)))
            (dolist (args '(("status") ("diff")))
              (with-temp-buffer
                (let* ((argv (mevedel-bash-git-prepare (cons "git" args)))
                       (status (apply #'process-file (car argv) nil t nil (cdr argv))))
                  (should (equal status 126))
                  (should (string-match-p "configured worktree filters" (buffer-string))))))
            (should-not (file-exists-p marker))
            (should (equal before (mevedel-bash-git-test--snapshot root))))
          (mevedel-bash-git-test--run '("git" "config" "--remove-section" "filter.hostile"))
          (let ((sha (string-trim (mevedel-bash-git-test--run '("git" "rev-parse" "HEAD")))))
            (mevedel-bash-git-test--run
             (list "git" "-c" "core.fsmonitor=false" "update-index" "--add"
                   "--cacheinfo" (concat "160000," sha ",nested"))))
          (let ((before (mevedel-bash-git-test--snapshot root)))
            (dolist (args '(("status") ("diff" "--stat")))
              (with-temp-buffer
                (let* ((argv (mevedel-bash-git-prepare (cons "git" args)))
                       (status (apply #'process-file (car argv) nil t nil (cdr argv))))
                  (should (equal status 126))
                  (should (string-match-p "submodule inspection" (buffer-string))))))
            (should-not (file-exists-p marker))
            (should (equal before (mevedel-bash-git-test--snapshot root)))))
      (delete-directory root t)))
  :doc "missing promisor objects fail without fetching, helpers, or metadata writes"
  (let* ((root (progn
                 (skip-unless (and (executable-find "git") (executable-find "env")))
                 (file-name-as-directory (make-temp-file "mevedel-git-promisor-" t))))
         (default-directory root)
         (helper (file-name-concat root "hostile-remote"))
         (marker (file-name-concat root "remote-ran")))
    (unwind-protect
        (progn
          (mevedel-bash-git-test--run '("git" "init" "-q" "-b" "main" "."))
          (with-temp-file helper
            (insert "#!/bin/sh\n: > " (shell-quote-argument marker) "\nexit 1\n"))
          (set-file-modes helper #o700)
          (dolist (pair '(("core.repositoryFormatVersion" . "1")
                          ("extensions.partialClone" . "origin")
                          ("remote.origin.promisor" . "true")
                          ("remote.origin.partialCloneFilter" . "blob:none")
                          ("protocol.ext.allow" . "always")))
            (mevedel-bash-git-test--run (list "git" "config" (car pair) (cdr pair))))
          (mevedel-bash-git-test--run
           (list "git" "config" "remote.origin.url" (concat "ext::" helper)))
          (let ((before (mevedel-bash-git-test--snapshot root))
                (argv (mevedel-bash-git-prepare
                       '("git" "cat-file" "-p" "1111111111111111111111111111111111111111"))))
            (with-temp-buffer
              (should-not (equal (apply #'process-file (car argv) nil t nil (cdr argv)) 0)))
            (should-not (file-exists-p marker))
            (should (equal before (mevedel-bash-git-test--snapshot root)))))
      (delete-directory root t)))
  :doc "post-probe config, attribute and index edits cannot enable filter or submodule helpers"
  (let* ((real-git (or (executable-find "git") (ert-skip "Git unavailable")))
         (root (file-name-as-directory (make-temp-file "mevedel-git-race-" t)))
         (repo (file-name-concat root "repo with spaces"))
         (bin (file-name-concat root "bin"))
         (helper (file-name-concat root "hostile-helper"))
         (marker (file-name-concat root "helper-ran"))
         (nested (file-name-concat repo "nested"))
         (nested-helper (file-name-concat root "nested-helper"))
         (nested-marker (file-name-concat root "nested-helper-ran"))
         nested-sha
         (record (file-name-concat root "shadow-record"))
         (value-record (file-name-concat root "config-record"))
         (index-record (file-name-concat root "index-record"))
         (default-directory root)
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (mevedel-bash-git-test--run (list real-git "init" "-q" "-b" "main" repo))
          (setq default-directory (file-name-as-directory repo))
          (mevedel-bash-git-test--run (list real-git "config" "user.name" "Test"))
          (mevedel-bash-git-test--run (list real-git "config" "user.email" "test@example.test"))
          (with-temp-file (file-name-concat repo "sample.dat") (insert "one\n"))
          (with-temp-file (file-name-concat repo ".gitattributes") (insert "\n"))
          (mevedel-bash-git-test--run (list real-git "add" "."))
          (mevedel-bash-git-test--run (list real-git "commit" "-q" "-m" "base"))
          (with-temp-file (file-name-concat repo "sample.dat") (insert "two\n"))
          (with-temp-file helper
            (insert "#!/bin/sh\n: > " (shell-quote-argument marker) "\nexit 1\n"))
          (set-file-modes helper #o700)
          (mevedel-bash-git-test--run (list real-git "init" "-q" "-b" "main" nested))
          (let ((default-directory (file-name-as-directory nested)))
            (mevedel-bash-git-test--run (list real-git "config" "user.name" "Test"))
            (mevedel-bash-git-test--run (list real-git "config" "user.email" "test@example.test"))
            (with-temp-file (file-name-concat nested "sample.dat") (insert "one\n"))
            (with-temp-file (file-name-concat nested ".gitattributes")
              (insert "*.dat filter=nested\n"))
            (mevedel-bash-git-test--run (list real-git "add" "."))
            (mevedel-bash-git-test--run (list real-git "commit" "-q" "-m" "nested"))
            (setq nested-sha (string-trim
                              (mevedel-bash-git-test--run (list real-git "rev-parse" "HEAD"))))
            (with-temp-file nested-helper
              (insert "#!/bin/sh\n: > " (shell-quote-argument nested-marker) "\nexit 1\n"))
            (set-file-modes nested-helper #o700)
            (mevedel-bash-git-test--run (list real-git "config" "core.fsmonitor" nested-helper))
            (mevedel-bash-git-test--run (list real-git "config" "filter.nested.clean" nested-helper))
            (with-temp-file (file-name-concat nested "sample.dat") (insert "two\n")))
          (with-temp-file (file-name-concat repo ".git" "info" "exclude")
            (insert "nested/\n"))
          (with-temp-file (file-name-concat repo ".git" "included.conf")
            (insert "[status]\n short\n[custom]\n strange = \"quote\\\" slash\\\\ tab\\t newline\\n\"\n"
                    "[filter \"late\"]\n clean = " helper "\n"))
          (with-temp-file (file-name-concat repo ".git" "late.conf")
            (insert "[filter \"late\"]\n clean = " helper "\n"
                    "[filter \"new\"]\n process = " helper "\n clean = " helper "\n"))
          (make-directory bin)
          ;; This trusted test executable simulates an adversary exactly after
          ;; all probes, immediately before exec of the final inspection.
          (with-temp-file (file-name-concat bin "git")
            (insert "#!/bin/sh\nmode=''\n"
                    "for arg; do case \"$arg\" in status|diff) mode=$arg ;; esac; done\n"
                    "if [ -n \"$mode\" ]; then\n"
                    " printf '%s\\n' \"$GIT_DIR\" > " (shell-quote-argument record) "\n"
                    " " (shell-quote-argument real-git)
                    " config --get custom.strange > " (shell-quote-argument value-record) " || exit $?\n"
                    " env -u GIT_DIR -u GIT_COMMON_DIR -u GIT_WORK_TREE "
                    (shell-quote-argument real-git) " -C " (shell-quote-argument repo)
                    " config include.path late.conf || exit $?\n"
                    " env -u GIT_DIR -u GIT_COMMON_DIR -u GIT_WORK_TREE "
                    (shell-quote-argument real-git) " -C " (shell-quote-argument repo)
                    " update-index --add --cacheinfo "
                    (shell-quote-argument (concat "160000," nested-sha ",nested")) " || exit $?\n"
                    " " (shell-quote-argument real-git)
                    " ls-files --stage > " (shell-quote-argument index-record) " || exit $?\n"
                    " driver=late; if [ \"$mode\" = diff ]; then driver=new; fi\n"
                    " printf '*.dat filter=%s\\n' \"$driver\" > "
                    (shell-quote-argument (file-name-concat repo ".gitattributes")) "\nfi\n"
                    "exec " (shell-quote-argument real-git) " \"$@\"\n"))
          (set-file-modes (file-name-concat bin "git") #o700)
          (setenv "PATH" (concat bin path-separator (getenv "PATH")))
          (dolist (command '("status" "diff"))
            (mevedel-bash-git-test--run (list real-git "config" "include.path" "included.conf"))
            (mevedel-bash-git-test--run (list real-git "update-index" "--force-remove" "nested"))
            (with-temp-file (file-name-concat repo ".gitattributes") (insert "\n"))
            (let ((expected (mevedel-bash-git-test--run
                             (list real-git "config" "--get" "custom.strange"))))
              (mevedel-bash-git-test--run (mevedel-bash-git-prepare (list "git" command)))
              (should-not (file-exists-p marker))
              (should-not (file-exists-p nested-marker))
              (should-not (string-match-p "^160000 " (with-temp-buffer
                                                        (insert-file-contents index-record)
                                                        (buffer-string))))
              (should (string-match-p "^160000 "
                                      (mevedel-bash-git-test--run
                                       (list real-git "ls-files" "--stage"))))
              (should (equal expected (with-temp-buffer
                                       (insert-file-contents value-record)
                                       (buffer-string))))
              (let ((shadow (string-trim (with-temp-buffer
                                           (insert-file-contents record)
                                           (buffer-string)))))
                (should-not (equal shadow (file-name-concat repo ".git")))
                (should-not (file-exists-p (file-name-directory shadow)))))
            ;; The exact source repo left by the injected edit is unsafe under
            ;; ordinary Git, confirming that the final launch really differed.
            (with-temp-buffer
              (process-file real-git nil t nil "--no-optional-locks"
                            "status" "--short" "--ignore-submodules=none"))
            (should (file-exists-p marker))
            (should (file-exists-p nested-marker))
            (delete-file marker)
            (delete-file nested-marker)))
      (delete-directory root t)))
  :doc "linked worktrees, worktree config, and split indexes retain inspection semantics without source writes"
  (let* ((root (progn
                 (skip-unless (and (executable-find "git") (executable-find "env")
                                   (executable-find "bash")))
                 (file-name-as-directory (make-temp-file "mevedel-git-linked-" t))))
         (base (file-name-concat root "base"))
         (linked (file-name-concat root "linked worktree"))
         (default-directory root))
    (unwind-protect
        (progn
          (mevedel-bash-git-test--run (list "git" "init" "-q" "-b" "main" base))
          (setq default-directory (file-name-as-directory base))
          (mevedel-bash-git-test--run '("git" "config" "user.name" "Test"))
          (mevedel-bash-git-test--run '("git" "config" "user.email" "test@example.test"))
          (with-temp-file (file-name-concat base "sample.dat") (insert "one\n"))
          (mevedel-bash-git-test--run '("git" "add" "."))
          (mevedel-bash-git-test--run '("git" "commit" "-q" "-m" "base"))
          (mevedel-bash-git-test--run '("git" "update-index" "--split-index"))
          (mevedel-bash-git-test--run (list "git" "worktree" "add" "-q" "-b" "inspect" linked))
          (mevedel-bash-git-test--run '("git" "config" "extensions.worktreeConfig" "true"))
          (setq default-directory (file-name-as-directory linked))
          (mevedel-bash-git-test--run '("git" "config" "--worktree" "status.short" "true"))
          (mevedel-bash-git-test--run '("git" "update-index" "--split-index"))
          (with-temp-file (file-name-concat linked "sample.dat") (insert "two\n"))
          (let ((gitdir (mevedel-bash-git-test--run '("git" "rev-parse" "--absolute-git-dir")))
                (before (mevedel-bash-git-test--snapshot root)))
            (let ((status (mevedel-bash-git-test--run
                           (mevedel-bash-git-prepare '("git" "status" "--branch")))))
              (should (string-match-p "## inspect" status))
              (should (string-match-p " M sample.dat" status)))
            (should (string-match-p
                     "sample.dat"
                     (mevedel-bash-git-test--run
                      (mevedel-bash-git-prepare '("git" "diff" "--stat")))))
            (should (equal gitdir (mevedel-bash-git-test--run
                                  (mevedel-bash-git-prepare
                                   '("git" "rev-parse" "--absolute-git-dir")))))
            (should (equal before (mevedel-bash-git-test--snapshot root)))))
      (delete-directory root t))))

(provide 'test-mevedel-bash-git)
;;; test-mevedel-bash-git.el ends here
