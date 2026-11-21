;;; test-niwa-review.el --- Tests for review workflow -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-agent-control)
(require 'niwa-agent-conversation)
(require 'niwa-agent-exec)
(require 'niwa-agents)
(require 'niwa-bash-policy)
(require 'niwa-execution-target)
(require 'niwa-hooks)
(require 'niwa-prompt-submission)
(require 'niwa-review)
(require 'niwa-structs)
(require 'niwa-tool-render-data)
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-segments)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar niwa--agent-invocation)
(defvar niwa-agents--specs)
(defvar niwa-bash-dangerous-commands)
(defvar niwa-session--read-only-mode)


(niwa-deftest niwa-review--git-lines ()
  ,test
  (test)
  :doc "does not forward client-only environment variables to target Git"
  (let ((process-environment
         (cons "NIWA_CLIENT_SECRET=do-not-forward" process-environment)))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _in destination _display &rest _args)
                 (should-not (getenv "NIWA_CLIENT_SECRET"))
                 (with-current-buffer (if (eq destination t)
                                          (current-buffer)
                                        destination)
                   (insert "main\ntopic\n"))
                 0)))
      (should
       (equal '("main" "topic")
              (niwa-review--git-lines
               "/ssh:user@host:/srv/project/" "branch"))))))

(niwa-deftest niwa-review--git-string ()
  ,test
  (test)
  :doc "does not forward client-only environment variables to target Git"
  (let ((process-environment
         (cons "NIWA_CLIENT_SECRET=do-not-forward" process-environment)))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _in destination _display &rest _args)
                 (should-not (getenv "NIWA_CLIENT_SECRET"))
                 (with-current-buffer (if (eq destination t)
                                          (current-buffer)
                                        destination)
                   (insert "abc123\n"))
                 0)))
      (should
       (equal "abc123"
              (niwa-review--git-string
               "/ssh:user@host:/srv/project/" "rev-parse" "HEAD"))))))

(niwa-deftest niwa-review--git-output ()
  ,test
  (test)
  :doc "does not launch a diff-producing command after an option separator"
  (let* ((root (make-temp-file "niwa-review-git-guard-" t))
         (git (file-name-concat root "git"))
         (marker (file-name-concat root "git-ran"))
         (exec-path (cons root exec-path)))
    (unwind-protect
        (progn
          (with-temp-file git
            (insert (format "#!/bin/sh\n: > %s\n"
                            (shell-quote-argument marker))))
          (set-file-modes git #o700)
          (should-not (niwa-review--git-output root "show" "--"))
          (should-not (file-exists-p marker)))
      (delete-directory root t))))

(niwa-deftest niwa-review--repo-root ()
  ,test
  (test)
  :doc "requalifies the target-native Git root through the current target"
  (progn
    (skip-unless (executable-find "git"))
    (let* ((tmp-dir (file-name-as-directory
                     (make-temp-file "niwa-review-remote-" t)))
           (child (file-name-concat tmp-dir "nested"))
           (remote-child (format "/niwamock:review:%s/" child)))
      (unwind-protect
          (progn
            (make-directory child t)
            (let ((default-directory tmp-dir))
              (should (zerop (process-file "git" nil nil nil
                                           "init" "--quiet"))))
            (niwa-test--with-local-shell-tramp '("review")
              (should
               (equal (format "/niwamock:review:%s" tmp-dir)
                      (niwa-review--repo-root remote-child)))))
        (delete-directory tmp-dir t)))))

(niwa-deftest niwa-review--target-native-path ()
  ,test
  (test)
  :doc "removes the transport prefix from model-facing review paths"
  (should
   (equal "/srv/project/.niwa/state/review-packages/review.md"
          (niwa-review--target-native-path
           "/ssh:user@host:/srv/project/"
           "/ssh:user@host:/srv/project/.niwa/state/review-packages/review.md")))
  :doc "preserves local review paths"
  (should
   (equal "/tmp/project/review.md"
          (niwa-review--target-native-path
           "/tmp/project/" "/tmp/project/review.md"))))


(niwa-deftest niwa-review--prompt-and-hint ()
  ,test
  (test)
  :doc "both commands include the pre-resolved merge base and their own wording"
  (cl-letf (((symbol-function 'niwa-review--git-string)
             (lambda (_cwd &rest args)
               (and (equal args '("merge-base" "HEAD" "main"))
                    "abc123"))))
    (dolist (command '(review verify))
      (let ((prompt+hint
             (niwa-review--prompt-and-hint
              command '(:type base-branch :branch "main") "/tmp/project/")))
        (should (string-search "merge base commit for this comparison is abc123"
                               (car prompt+hint)))
        (should (string-search "git diff abc123" (car prompt+hint)))
        (should (eq (eq command 'verify)
                    (not (null (string-search "VERDICT: PASS" (car prompt+hint))))))
        (should (equal "changes against 'main'" (cdr prompt+hint))))))

  :doc "custom targets trim instructions and keep the verifier verdict requirement"
  (dolist (command '(review verify))
    (let ((prompt+hint
           (niwa-review--prompt-and-hint
            command '(:type custom :instructions "  Check only tests.  ")
            "/tmp/project/")))
      (if (eq command 'verify)
          (progn
            (should (string-search "Check only tests." (car prompt+hint)))
            (should (string-search "VERDICT: PASS" (car prompt+hint))))
        (should (equal "Check only tests." (car prompt+hint))))
      (should (equal "Check only tests." (cdr prompt+hint))))))

(niwa-deftest niwa-review--parse-target-arg
  (:doc "explicit slash target args parse but free-form text stays custom")
  ,test
  (test)
  (should (equal '(:type uncommitted)
                 (niwa-review--parse-target-arg "current")))
  (should (equal '(:type commit :sha "HEAD" :title "")
                 (niwa-review--parse-target-arg "HEAD")))
  (should (equal '(:type base-branch :branch "main")
                 (niwa-review--parse-target-arg "branch:main")))
  (should (equal '(:type commit :sha "abc123" :title "")
                 (niwa-review--parse-target-arg "commit:abc123")))
  (should-not (niwa-review--parse-target-arg "current changes")))

(niwa-deftest niwa-review--read-target ()
  ,test
  (test)
  :doc "verify custom target uses verify-specific prompt text"
  (let (seen-prompt)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "custom instructions"))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _args)
                 (setq seen-prompt prompt)
                 "  Try edge cases.  ")))
      (should (equal '(:type custom :instructions "Try edge cases.")
                     (niwa-review--read-target "/tmp/project/" 'verify)))
      (should (equal "Verify instructions: " seen-prompt)))))

(niwa-deftest niwa-review--write-package ()
  ,test
  (test)
  :doc "writes a review package file for a git range"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-review-package-" t)))
         (package-file (file-name-concat root ".niwa"
                                         "state" "review-packages"
                                         "range.diff")))
    (unwind-protect
        (progn
          (should (zerop (process-file "git" nil nil nil
                                       "init" "-q" "-b" "main" root)))
          (let ((default-directory root))
            (process-file "git" nil nil nil "config" "user.name" "Test")
            (process-file "git" nil nil nil
                          "config" "user.email" "test@example.test"))
          (with-temp-file (file-name-concat root "a.txt")
            (insert "one\n"))
          (let ((default-directory root))
            (should (zerop (process-file "git" nil nil nil "add" "a.txt")))
            (should (zerop (process-file "git" nil nil nil
                                         "commit" "-q" "-m" "base"))))
          (let ((base (with-temp-buffer
                        (let ((default-directory root))
                          (should (zerop (process-file "git" nil t nil
                                                       "rev-parse" "HEAD"))))
                        (string-trim (buffer-string)))))
            (with-temp-file (file-name-concat root "a.txt")
              (insert "one\ntwo\n"))
            (let ((default-directory root))
              (should (zerop (process-file "git" nil nil nil "add" "a.txt")))
              (should (zerop (process-file "git" nil nil nil
                                           "commit" "-q" "-m" "change"))))
            (should (equal package-file
                           (iter-do (_ (niwa-review--write-package
                            root
                            (list :type 'range
                                  :base base
                                  :head "HEAD")
                            package-file)))))
            (let ((text (with-temp-buffer
                          (insert-file-contents package-file)
                          (buffer-string))))
              (should (string-search "# Review package:" text))
              (should (string-search "## Commits" text))
              (should (string-search "change" text))
              (should (string-search "## Diff" text))
              (should (string-search "+two" text)))))
      (delete-directory root t)))

  :doc "does not execute configured Git diff helpers locally or on target"
  (niwa-test--with-local-shell-tramp '("review")
    (dolist (remote '(nil t))
      (let* ((root (file-name-as-directory
                    (make-temp-file "niwa-review-helper-" t)))
             (cwd (if remote
                      (format "/niwamock:review:%s" root)
                    root))
             (helper (file-name-concat root "hostile-helper"))
             (marker (file-name-concat root "helper-ran"))
             (package-file (file-name-concat cwd "review.md")))
        (unwind-protect
            (progn
              (should (zerop (process-file "git" nil nil nil
                                           "init" "-q" "-b" "main" root)))
              (let ((default-directory root))
                (should (zerop (process-file "git" nil nil nil
                                             "config" "user.name" "Test")))
                (should (zerop (process-file "git" nil nil nil
                                             "config" "user.email"
                                             "test@example.test")))
                (with-temp-file helper
                  (insert (format "#!/bin/sh\n: > %s\ncat \"$1\"\n"
                                  (shell-quote-argument marker))))
                (set-file-modes helper #o700)
                (with-temp-file (file-name-concat root ".gitattributes")
                  (insert "*.dat diff=hostile\n"))
                (with-temp-file (file-name-concat root "sample.dat")
                  (insert "one\n"))
                (with-temp-file (file-name-concat root "--no-ext-diff")
                  (insert "one\n"))
                (should (zerop (process-file "git" nil nil nil "add" ".")))
                (should (zerop (process-file "git" nil nil nil
                                             "commit" "-q" "-m" "base")))
                (with-temp-file (file-name-concat root "sample.dat")
                  (insert "one\ntwo\n"))
                (with-temp-file (file-name-concat root "--no-ext-diff")
                  (insert "one\ntwo\n"))
                (should (zerop (process-file "git" nil nil nil "add" ".")))
                (should (zerop (process-file "git" nil nil nil
                                             "commit" "-q" "-m" "change")))
                (should (zerop (process-file "git" nil nil nil
                                             "config" "diff.hostile.textconv"
                                             helper)))
                (should (zerop (process-file "git" nil nil nil
                                             "config" "diff.external"
                                             helper))))
              (let ((process-environment
                     (cons (concat "GIT_EXTERNAL_DIFF=" helper)
                           process-environment)))
                (should (equal package-file
                               (iter-do (_ (niwa-review--write-package
                                cwd '(:type commit :sha "--ext-diff")
                                package-file))))))
              (should-not (file-exists-p marker))
              (let ((text (with-temp-buffer
                            (insert-file-contents package-file)
                            (buffer-string))))
                (should (string-search "+two" text)))
              (should (equal package-file
                             (iter-do (_ (niwa-review--write-package
                              cwd '(:type commit :sha "--") package-file)))))
              (should-not (file-exists-p marker)))
          (delete-directory root t)))))

  :doc "allocates collision-free package paths and removes failed allocations"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-review-unique-" t))))
    (unwind-protect
        (progn
          (should (zerop (process-file "git" nil nil nil
                                       "init" "-q" "-b" "main" root)))
          (let ((first (iter-do (_ (niwa-review--write-package
                        root '(:type uncommitted)))))
                (second (iter-do (_ (niwa-review--write-package
                         root '(:type uncommitted))))))
            (should (equal (file-name-directory first)
                           (file-name-as-directory
                            (file-name-concat root ".niwa" "state" "review-packages"))))
            (should-not (equal first second))
            (should (file-exists-p first))
            (should (file-exists-p second)))
          (should-error
           (iter-do (_ (niwa-review--write-package root '(:type unsupported))))
           :type 'user-error)
          (should (= 2 (length (directory-files
                                (niwa-review--package-directory root)
                                nil "\\.md\\'")))))
      (delete-directory root t))))

(niwa-deftest niwa-review--prompt-with-package
  (:doc "tells reviewers to read the package before broad git inspection")
  (let ((prompt (niwa-review--prompt-with-package
                 "Review this range."
                 "/tmp/review.diff"
                 'review)))
    (should (string-search "Review package file: /tmp/review.diff" prompt))
    (should (string-search "Read that file first" prompt))
    (should (string-search "Do not rerun broad git commands" prompt))))

(niwa-deftest niwa-review-parse-output ()
  ,test
  (test)
  :doc "parses JSON surrounded by stray prose"
  (let* ((json "{\"findings\":[{\"title\":\"[P2] Fix thing\",\"body\":\"body\",\"priority\":2,\"confidence_score\":0.8,\"code_location\":{\"absolute_file_path\":\"/tmp/a.el\",\"line_range\":{\"start\":3,\"end\":3}}}],\"overall_correctness\":\"patch is incorrect\",\"overall_explanation\":\"One issue.\",\"overall_confidence_score\":0.7}")
         (parsed (niwa-review-parse-output (concat "Here:\n" json "\nDone"))))
    (should (equal "One issue."
                   (plist-get parsed :overall_explanation)))
    (should (= 1 (length (plist-get parsed :findings))))
    (should (equal "[P2] Fix thing"
                   (plist-get (car (plist-get parsed :findings))
                              :title))))

  :doc "falls back to overall explanation when JSON parse fails"
  (let ((parsed (niwa-review-parse-output "plain review text")))
    (should (equal "plain review text"
                   (plist-get parsed :overall_explanation)))
    (should (null (plist-get parsed :findings))))

  :doc "keeps findings with unspecified or integer priorities"
  (dolist (entry '((nil nil) ("null" :null)
                   ("0" 0) ("1" 1) ("2" 2) ("3" 3)))
    (let* ((literal (car entry))
           (text (format
                  "{\"findings\":[{\"title\":\"Fix thing\",\"body\":\"body\",%s\"confidence_score\":0.8,\"code_location\":{\"absolute_file_path\":\"/tmp/a.el\",\"line_range\":{\"start\":3,\"end\":3}}}],\"overall_correctness\":\"patch is incorrect\",\"overall_explanation\":\"One issue.\",\"overall_confidence_score\":0.7}"
                  (if literal (format "\"priority\":%s," literal) "")))
           (parsed (niwa-review-parse-output text))
           (findings (plist-get parsed :findings)))
      (ert-info ((format "Priority %S" literal))
        (should (equal "One issue." (plist-get parsed :overall_explanation)))
        (should (= 1 (length findings)))
        (should (equal "Fix thing" (plist-get (car findings) :title)))
        (should (equal "body" (plist-get (car findings) :body)))
        (should (equal (cadr entry) (plist-get (car findings) :priority)))
        (should (eq (not (null literal))
                    (not (null (plist-member (car findings) :priority))))))))

  :doc "rejects priorities outside the nullable integer contract"
  (dolist (literal '("-1" "4" "1.5" "true" "false" "\"2\"" "[]" "{}"))
    (let* ((text (format
                  "{\"findings\":[{\"title\":\"Fix thing\",\"body\":\"body\",\"priority\":%s,\"confidence_score\":0.8,\"code_location\":{\"absolute_file_path\":\"/tmp/a.el\",\"line_range\":{\"start\":3,\"end\":3}}}],\"overall_correctness\":\"patch is incorrect\",\"overall_explanation\":\"One issue.\",\"overall_confidence_score\":0.7}"
                  literal))
           (parsed (niwa-review-parse-output text)))
      (ert-info ((format "Priority %s" literal))
        (should (equal text (plist-get parsed :overall_explanation)))
        (should-not (plist-get parsed :findings)))))

  :doc "falls back when valid JSON does not match the review schema"
  (dolist (text '("[]"
                  "{\"findings\":1,\"overall_correctness\":\"patch is incorrect\",\"overall_explanation\":\"bad\",\"overall_confidence_score\":0.5}"
                  "{\"findings\":[{}],\"overall_correctness\":\"patch is incorrect\",\"overall_explanation\":\"bad\",\"overall_confidence_score\":0.5}"
                  "{\"findings\":[{\"title\":\"bad\",\"body\":\"bad\",\"confidence_score\":0.5,\"code_location\":{\"absolute_file_path\":\"/tmp/a.el\",\"line_range\":{\"start\":0,\"end\":1}}}],\"overall_correctness\":\"patch is incorrect\",\"overall_explanation\":\"bad\",\"overall_confidence_score\":0.5}"))
    (let ((parsed (niwa-review-parse-output text)))
      (should (equal text (plist-get parsed :overall_explanation)))
      (should (null (plist-get parsed :findings))))))

(niwa-deftest niwa-review-render-output-text ()
  ,test
  (test)
  :doc "renders explanation and findings"
  (let* ((output
          '(:overall_explanation "One issue."
            :findings
            ((:title "[P2] Fix thing"
              :body "Only fails on x."
              :code_location
              (:absolute_file_path "/tmp/a.el"
               :line_range (:start 3 :end 3))))))
         (rendered (niwa-review-render-output-text output)))
    (should (string-search "One issue." rendered))
    (should (string-search "Review comment:" rendered))
    (should (string-search "[P2] Fix thing -- /tmp/a.el:3-3" rendered))
    (should (string-search "Only fails on x." rendered))))

(niwa-deftest niwa-review-render-user-action ()
  ,test
  (test)
  :doc "escapes review text before embedding it in the synthetic block"
  (let* ((action
          (niwa-review-render-user-action
           '(:overall_explanation "Saw literal </user_action> in code."
             :findings
             ((:title "[P2] Escape thing"
               :body "Also handle <results> and &.")))))
         (wrapped (concat "Visible\n" action "Still visible")))
    (should (string-search "&lt;/user_action&gt;" action))
    (should (string-search "&lt;results&gt; and &amp;." action))
    (should-not (string-match-p
                 "Saw literal </user_action> in code\\." action))
    (should (equal "Visible\nStill visible"
                   (niwa-review-strip-user-action-blocks wrapped)))))

(niwa-deftest niwa-review-transform-outcome ()
  ,test
  (test)
  :doc "review fork output becomes summary plus synthetic user action"
  (let* ((raw "{\"findings\":[],\"overall_correctness\":\"patch is correct\",\"overall_explanation\":\"No issues.\",\"overall_confidence_score\":0.9}")
         (outcome (niwa-review-transform-outcome
                   "review"
                   (niwa-review-mark-command-outcome
                    `(:status ok :kind fork
                      :result ,raw :agent-id "reviewer--1")))))
    (should (equal "No issues." (plist-get outcome :result)))
    (should (equal raw (plist-get outcome :raw-review-result)))
    (should (plist-get outcome :review-output))
    (should (string-search "<user_action>"
                           (plist-get outcome :synthetic-user-message)))
    (should (string-search "<action>review</action>"
                           (plist-get outcome :synthetic-user-message))))

  :doc "does not transform unmarked user skills named review"
  (let* ((raw "{\"findings\":[],\"overall_explanation\":\"No issues.\"}")
         (outcome `(:status ok :kind fork :result ,raw :agent-id "reviewer--1")))
    (should (eq outcome (niwa-review-transform-outcome "review" outcome)))))

(niwa-deftest niwa-review--permission-rules ()
  ,test
  (test)
  :doc "allow git inspection while denying other Bash commands"
  (let ((rules (niwa-review--permission-rules)))
    (should (eq 'allow
                (niwa-permission-rules-action
                 rules "Bash" :pattern "git diff --stat")))
    (should (eq 'deny
                (niwa-permission-rules-action
                 rules "Bash" :pattern "make test"))))

  :doc "verify grants git inspection without denying validation commands"
  (let ((rules (niwa-review--verify-permission-rules)))
    (should (eq 'allow
                (niwa-permission-rules-action
                 rules "Bash" :pattern "git diff --stat")))
    (should (null
             (niwa-permission-rules-action
              rules "Bash" :pattern "make test")))))

(niwa-deftest niwa-review--bash-permissions ()
  ,test
  (test)
  :doc "allow common reviewer git inspection commands"
  (let* ((rules (niwa-review--permission-rules))
         (context (list :mode 'ask :buckets
                        (list (cons :defcustom rules))))
         (niwa-bash-dangerous-commands nil))
    (cl-labels ((decide (command)
                  (niwa-bash-policy-check-permission
                   command :permission-context context)))
      (should
       (eq 'allow
           (decide
            "git show --stat --oneline --decorate --no-renames 52e4748 && git show --format=fuller --no-ext-diff --unified=80 --no-renames 52e4748")))
      (should
       (eq 'allow
           (decide
            "git rev-parse 52e4748 && git diff --stat 52e4748^ 52e4748")))
      (should
       (eq 'allow
           (decide "git --no-pager diff --name-only 52e4748^ 52e4748")))
      (should
       (eq 'allow
           (decide "git --no-pager diff --name-only '52e4748^' 52e4748")))
      (should
       (eq 'allow
           (decide "git cat-file -p 52e4748 | head")))
      (should
       (eq 'allow
           (decide
            "git diff --stat HEAD~2 HEAD~1 && git diff --unified=80 HEAD~2 HEAD~1 -- niwa-review.el")))))

  :doc "deny commands outside the reviewer inspection allowlist"
  (let* ((rules (niwa-review--permission-rules))
         (context (list :mode 'ask :buckets
                        (list (cons :defcustom rules))))
         (niwa-bash-dangerous-commands nil))
    (cl-labels ((decide (command)
                  (niwa-bash-policy-check-permission
                   command :permission-context context)))
      (should (eq 'deny (decide "git checkout main")))
      (should (eq 'deny (decide "GIT_EXTERNAL_DIFF=sh git diff HEAD")))
      (should (eq 'deny (decide "head /etc/passwd")))
      (should (eq 'deny (decide "head -n 5 ~/.authinfo")))
      (should (eq 'deny (decide "make test"))))))

(niwa-deftest niwa-review--ensure-dispatch-deps ()
  ,test
  (test)
  :doc "loads the reviewer agent registry for autoloaded dispatch"
  (niwa-review--ensure-dispatch-deps)
  (should (fboundp 'niwa-agent-get))
  (should (niwa-agent-get "reviewer"))

  :doc "loads the verifier agent registry for autoloaded dispatch"
  (progn
    (niwa-review--ensure-dispatch-deps 'verify)
    (should (niwa-agent-get "verifier")))

  :doc "reports a missing agent instead of dispatching without one"
  ;; The registry is populated when the agents module loads; an empty
  ;; one means a broken install, which must fail loudly.
  (let ((niwa-agent--registry nil))
    (should-error (niwa-review--ensure-dispatch-deps)
                  :type 'user-error)
    (should-error (niwa-review--ensure-dispatch-deps 'verify)
                  :type 'user-error)))

(niwa-deftest niwa-review--next-task-name ()
  ,test
  (test)
  :doc "reserves monotonically suffixed review and verify path names"
  (let* ((session (niwa-session--create :name "validation"))
         (review (niwa-agent-record--create :path "/root/review"))
         (review-2 (niwa-agent-record--create :path "/root/review_2"))
         (verify (niwa-agent-record--create :path "/root/verify")))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/review" review)
                (cons "/root/review_2" review-2)
                (cons "/root/verify" verify)))
    (should (equal "review_3"
                   (niwa-review--next-task-name session 'review)))
    (should (equal "verify_2"
                   (niwa-review--next-task-name session 'verify)))))

(niwa-deftest niwa-review--result-outcome ()
  ,test
  (test)
  :doc "maps completed RESULT into the existing fork outcome contract"
  (should
   (equal '(:status ok :kind fork :result "findings"
            :agent-path "/root/review")
          (niwa-review--result-outcome
           '(:outcome completed :payload "findings" :sender "/root/review"))))

  :doc "maps interrupted and malformed RESULT into workflow errors"
  (let ((interrupted
         (niwa-review--result-outcome
          '(:outcome interrupted :payload "stopped" :sender "/root/verify")))
        (invalid (niwa-review--result-outcome '(:outcome unknown))))
    (should (eq 'error (plist-get interrupted :status)))
    (should (eq 'agent-interrupted (plist-get interrupted :reason)))
    (should (equal "stopped" (plist-get interrupted :message)))
    (should (eq 'invalid-agent-result (plist-get invalid :reason))))

  :doc "retains the child's token usage on completed and failed outcomes"
  (dolist (outcome '(completed errored))
    (should (= 12 (plist-get (niwa-review--result-outcome
                              `(:outcome ,outcome :payload "x" :usage 12))
                             :usage)))))

(niwa-deftest niwa-review--write-target-package ()
  ,test
  (test)
  :doc "abort and source death remove unfinished packages without delivery"
  (dolist (action '(abort kill))
    (let* ((root (make-temp-file "niwa-review-cancel-" t))
           (directory (file-name-concat root ".niwa/state/review-packages"))
           (source (generate-new-buffer " *review-cancel*"))
           cancel delivered)
      (unwind-protect
          (progn
            (should (zerop (process-file "git" nil nil nil "init" "-q" root)))
            (with-current-buffer source
              (setq cancel (niwa-review--write-target-package
                            root '(:type uncommitted) (lambda (_path) (setq delivered t)))))
            (let ((deadline (+ (float-time) 5)))
              (while (and (not (file-directory-p directory)) (< (float-time) deadline))
                (sleep-for .001)))
            (should (file-directory-p directory))
            (if (eq action 'kill) (kill-buffer source) (funcall cancel))
            (funcall cancel)
            (sleep-for .02)
            (should-not delivered)
            (should-not (directory-files directory nil "\\.md\\'")))
        (when cancel (funcall cancel))
        (when (buffer-live-p source) (kill-buffer source))
        (delete-directory root t)))))

(niwa-deftest niwa-review--run-task/preparation ()
  ,test
  (test)
  :doc "prepares real Git evidence after returning and services input before dispatch"
  (let* ((root (make-temp-file "niwa-review-preparation-" t))
         (data (generate-new-buffer " *review-preparation*"))
         timer input-ran dispatched)
    (unwind-protect
        (progn
          (should (zerop (process-file "git" nil nil nil "init" "-q" root)))
          (with-temp-file (file-name-concat root "example.txt") (insert "review evidence\n"))
          (with-current-buffer data
            (setq default-directory root)
            (setq-local niwa--session (niwa-session--create :name "review"))
            (setq-local niwa--current-request (niwa-request--create :session niwa--session))
            (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                       (lambda (_session _name prompt _callback &rest _options)
                         (setq dispatched prompt)
                         #'ignore)))
              (niwa-review--run-task "Review this change" "target" #'ignore
                                        nil nil 'review root '(:type uncommitted))
              (should-not dispatched)
              (setq timer (run-at-time 0 nil (lambda () (setq input-ran t))))
              (let ((deadline (+ (float-time) 5)))
                (while (and (not dispatched) (< (float-time) deadline)) (sleep-for .002)))
              (should input-ran)
              (should (string-search "Review package file:" dispatched))
              (let ((files (directory-files
                            (file-name-concat root ".niwa/state/review-packages") t "\\.md\\'")))
                (should (= 1 (length files)))
                (with-temp-buffer
                  (insert-file-contents (car files))
                  (should (string-search "example.txt" (buffer-string)))))
              (mapc #'funcall (niwa-request-cancellers niwa--current-request)))))
      (when timer (cancel-timer timer))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory root t))))

(niwa-deftest niwa-review-verify ()
  ,test
  (test)
  :doc "uses Goal review policy with the verifier role and returns verdict and usage"
  (niwa-skills-test--with-model-backends
    (let ((data (generate-new-buffer " *niwa-review-verify*"))
          captured-options captured-message outcome)
      (unwind-protect
          (with-current-buffer data
            (setq-local gptel-backend (gptel-get-backend "Fast")
                        gptel-model 'fast-model
                        gptel-reasoning-effort 'low
                        niwa-model-workloads
                        '((goal-review :provider "Balanced:balanced-model" :effort high)
                          (verifier :provider "Missing:model")))
            (setq-local niwa--session
                        (niwa-session--create :name "goal"))
            (setq-local niwa--current-request
                        (niwa-request--create :session niwa--session))
            (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                       (lambda (_session task-name message callback &rest options)
                         (setq captured-options options captured-message message)
                         (funcall callback
                                  (list :outcome 'success
                                        :record (niwa-agent-record--create
                                                 :path (concat "/root/" task-name))))
                         #'ignore)))
              (niwa-review-verify "Is the objective met?"
                                     (lambda (value) (setq outcome value))
                                     'goal-review)
              (should (equal (niwa-model-resolve-workload 'goal-review)
                             (plist-get captured-options :model-policy)))
              (should (eq 'fast-model gptel-model))
              (should (eq 'low gptel-reasoning-effort))
              (should (equal "Is the objective met?" captured-message))
              (should (equal "verifier"
                             (niwa-agent-name (plist-get captured-options :agent))))
              (should (equal "none" (plist-get captured-options :context)))
              (should (equal "Verify Goal completion"
                             (plist-get captured-options :description)))
              (should (equal (niwa-review--verify-permission-rules)
                             (plist-get captured-options :skill-permission-rules)))
              (funcall (plist-get captured-options :result-handler)
                       '(:outcome completed :payload "evidence\nVERDICT: FAIL"
                                  :sender "/root/verify" :usage 42))
              (should (eq 'fail (plist-get outcome :verdict)))
              (should (= 42 (plist-get outcome :usage)))))
        (kill-buffer data))))

  :doc "rejects missing Goal review provider when the configured fallback is also missing"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "goal")
                niwa--current-request
                (niwa-request--create :session niwa--session)
                niwa-model-workloads
                '((goal-review :provider "Missing:model")))
    (let ((niwa-model-fallback-provider "Missing:fallback") outcome)
      (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                 (lambda (&rest _args) (ert-fail "Unexpected verifier dispatch"))))
        (niwa-review-verify "Check completion"
                               (lambda (value) (setq outcome value))
                               'goal-review))
      (should (eq 'error (plist-get outcome :status)))
      (should (eq 'agent-dispatch-failed (plist-get outcome :reason)))
      (should (string-match-p "Missing" (plist-get outcome :message))))))

(niwa-deftest niwa-review--run-task ()
  ,test
  (test)
  :doc "spawns and awaits a retained reviewer leaf through agent control"
  (let ((data (generate-new-buffer " *niwa-review-run-task*"))
        ;; Spawn is stubbed here, so this pins that dispatch leaves the
        ;; roster untouched, not that the gate would have passed.
        (roster (list (cons "worker" (list :system "w" :tools nil))))
        captured-options captured-message captured-name captured-session
        outcomes)
    (unwind-protect
        (with-current-buffer data
          (setq-local niwa--session
                      (niwa-session--create :name "review"))
          (setq-local niwa--current-request
                      (niwa-request--create :session niwa--session))
          (setq-local niwa-agents--specs roster)
          (let ((progress-callback #'ignore))
            (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                       (lambda (session task-name message callback
                                &rest options)
                         (setq captured-session session
                               captured-name task-name
                               captured-message message
                               captured-options options)
                         (let ((record
                                (niwa-agent-record--create
                                 :path (concat "/root/" task-name))))
                           (funcall callback
                                    (list :outcome 'success :record record)))
                         #'ignore)))
              (niwa-review--run-task
               "prompt" "target"
               (lambda (result) (push result outcomes))
               "<hook-context>extra</hook-context>"
               progress-callback)
              (should (eq niwa--session captured-session))
              (should (equal "review" captured-name))
              (should (equal "prompt\n\n<hook-context>extra</hook-context>"
                             captured-message))
              ;; Passing the resolved agent asks nothing of the roster, so
              ;; the gate that reads it has nothing to bypass.
              (should-not (plist-get captured-options :role))
              (should (equal "reviewer"
                             (niwa-agent-name
                              (plist-get captured-options :agent))))
              (should (equal roster niwa-agents--specs))
              (should (equal "none"
                             (plist-get captured-options :context)))
              (should (equal "target"
                             (plist-get captured-options :description)))
              (should (functionp
                       (plist-get captured-options :on-invocation)))
              (should (equal (niwa-review--permission-rules)
                             (plist-get captured-options
                                        :skill-permission-rules)))
              (should-not outcomes)
              (should (= 1 (length (niwa-request-cancellers
                                    niwa--current-request))))
              (let ((handler (plist-get captured-options :result-handler)))
                (funcall handler
                         '(:outcome completed :payload "review json"
                           :sender "/root/review"))
                (funcall handler
                         '(:outcome completed :payload "duplicate"
                           :sender "/root/review")))
              (should (= 1 (length outcomes)))
              (let ((outcome (car outcomes)))
                (should (eq 'ok (plist-get outcome :status)))
                (should (eq 'fork (plist-get outcome :kind)))
                (should (equal "review json" (plist-get outcome :result)))
                (should (equal "/root/review"
                               (plist-get outcome :agent-path)))
                (should-not (plist-get outcome :niwa-review-command))))))
      (kill-buffer data)))

  :doc "keeps verifier role, policy, and interrupted outcome distinct"
  (let ((data (generate-new-buffer " *niwa-verify-run-task*"))
        captured-options outcome)
    (unwind-protect
        (with-current-buffer data
          (setq-local niwa--session
                      (niwa-session--create :name "verify"))
          (setq-local niwa--current-request
                      (niwa-request--create :session niwa--session))
          (setq-local niwa-model-workloads
                      '((goal-review :provider "Missing:model")))
          (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                     (lambda (_session task-name _message callback
                              &rest options)
                       (setq captured-options options)
                       (let ((record
                              (niwa-agent-record--create
                               :path (concat "/root/" task-name))))
                         (funcall callback
                                  (list :outcome 'success :record record)))
                       #'ignore)))
            (niwa-review--run-task
             "prompt" "target"
             (lambda (result) (setq outcome result))
             nil nil 'verify)
            (should-not (plist-get captured-options :role))
            (should-not (plist-get captured-options :model-policy))
            (should (equal "verifier"
                           (niwa-agent-name
                            (plist-get captured-options :agent))))
            (should (equal (niwa-review--verify-permission-rules)
                           (plist-get captured-options
                                      :skill-permission-rules)))
            (funcall (plist-get captured-options :result-handler)
                     '(:outcome interrupted :payload "stopped"
                       :sender "/root/verify"))
            (should (eq 'error (plist-get outcome :status)))
            (should (eq 'agent-interrupted (plist-get outcome :reason)))
            (should (equal "/root/verify"
                           (plist-get outcome :agent-path)))
            (should-not (plist-get outcome :niwa-review-command))))
      (kill-buffer data)))

  :doc "accepts one exact final verifier verdict and rejects malformed reports"
  (dolist (case '(("evidence\nVERDICT: PASS" pass nil)
                  ("VERDICT: PASS\nextra" nil t)
                  ("VERDICT: PASS\nVERDICT: FAIL" nil t)))
    (let ((data (generate-new-buffer " *niwa-verify-verdict*"))
          (invocation (niwa-agent-invocation--create
                       :path "/root/verify"))
          result-handler
          outcome)
      (unwind-protect
          (with-current-buffer data
            (setq-local niwa--session
                        (niwa-session--create :name "verify"))
            (setq-local niwa--current-request
                        (niwa-request--create :session niwa--session))
            (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                       (lambda (_session task-name _message callback
                                &rest options)
                         (setq result-handler
                               (plist-get options :result-handler))
                         (funcall (plist-get options :on-invocation) invocation)
                         (let ((record
                                (niwa-agent-record--create
                                 :path (concat "/root/" task-name))))
                           (funcall callback
                                    (list :outcome 'success :record record)))
                         #'ignore)))
              (niwa-review--run-task
               "prompt" "target" (lambda (value) (setq outcome value))
               nil nil 'verify)
              (funcall result-handler
                       `(:outcome completed :payload ,(car case)
                         :sender "/root/verify"))
              (should (eq (cadr case)
                          (niwa-agent-invocation-verdict invocation)))
              (if (caddr case)
                  (progn
                    (should (plist-get outcome :verification-rejected))
                    (should (string-search "Verification report rejected"
                                           (plist-get outcome :result)))
                    (should (string-search (car case)
                                           (plist-get outcome :result))))
                (should (eq 'pass (plist-get outcome :verdict))))))
        (kill-buffer data))))

  :doc "reads the verdict from the complete report behind a bounded preview"
  (dolist (case '(("VERDICT: PASS" pass) ("VERDICT: FAIL\nmore\nVERDICT: PASS" nil)))
    (let* ((data (generate-new-buffer " *niwa-verify-full*"))
           (full (concat (make-string 40000 ?x) "\n" (car case)))
           (preview "head ... tail\n\nFull transcript: /root/verify")
           (record (niwa-agent-record--create
                    :path "/root/verify" :activity 'idle
                    :settled-result full :settled-outcome 'completed))
           result-handler outcome)
      (unwind-protect
          (with-current-buffer data
            (setq-local niwa--session
                        (niwa-session--create :name "verify"
                                                 :agent-registry
                                                 (list (cons "/root/verify" record))))
            (setq-local niwa--current-request
                        (niwa-request--create :session niwa--session))
            (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                       (lambda (_session _name _message callback &rest options)
                         (setq result-handler (plist-get options :result-handler))
                         (funcall callback (list :outcome 'success :record record))
                         #'ignore)))
              (niwa-review--run-task
               "prompt" "target" (lambda (value) (setq outcome value)) nil nil 'verify)
              (funcall result-handler
                       `(:outcome completed :payload ,preview :sender "/root/verify"))
              (should (eq (cadr case) (plist-get outcome :verdict)))
              (if (cadr case)
                  (should (equal preview (plist-get outcome :result)))
                (should (plist-get outcome :verification-rejected))
                (should-not (string-search full (plist-get outcome :result))))))
        (kill-buffer data))))

  :doc "request cancellation interrupts once and suppresses the late RESULT"
  (let ((data (generate-new-buffer " *niwa-review-cancel-task*"))
        result-handler interrupt-target callback-called)
    (unwind-protect
        (with-current-buffer data
          (setq-local niwa--session
                      (niwa-session--create :name "cancel"))
          (setq-local niwa--current-request
                      (niwa-request--create :session niwa--session))
          (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                     (lambda (_session task-name _message callback
                              &rest options)
                       (setq result-handler
                             (plist-get options :result-handler))
                       (let ((record
                              (niwa-agent-record--create
                               :path (concat "/root/" task-name))))
                         (funcall callback
                                  (list :outcome 'success :record record)))
                       #'ignore))
                    ((symbol-function 'niwa-agent-control-interrupt)
                     (lambda (_session target)
                       (setq interrupt-target target)
                       (funcall result-handler
                                `(:outcome interrupted :payload "cancelled"
                                  :sender ,target)))))
            (niwa-review--run-task
             "prompt" "target"
             (lambda (_result) (setq callback-called t)))
            (niwa-request-drain-cancellers niwa--current-request)
            (should (equal "/root/review" interrupt-target))
            (should-not callback-called)))
      (kill-buffer data)))

  :doc "reports retained-agent dispatch errors through the callback"
  (let ((data (generate-new-buffer " *niwa-review-run-task-error*"))
        outcome)
    (unwind-protect
        (with-current-buffer data
          (setq-local niwa--session
                      (niwa-session--create :name "error"))
          (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                     (lambda (&rest _args)
                       (error "Dispatch exploded"))))
            (niwa-review--run-task
             "prompt" "target"
             (lambda (result) (setq outcome result)))
            (should (eq 'error (plist-get outcome :status)))
            (should (eq 'agent-dispatch-failed
                        (plist-get outcome :reason)))
            (should (equal "Dispatch exploded"
                           (plist-get outcome :message)))))
      (kill-buffer data)))

  :doc "creates package evidence only when the accepted task starts"
  (let ((data (generate-new-buffer " *niwa-review-packaged-task*"))
        (root (file-name-as-directory
               (make-temp-file "niwa-review-task-package-" t)))
        captured-message)
    (unwind-protect
        (progn
          (should (zerop (process-file "git" nil nil nil
                                       "init" "-q" "-b" "main" root)))
          (with-current-buffer data
            (setq-local niwa--session
                        (niwa-session--create :name "review")
                        niwa--current-request
                        (niwa-request--create :session niwa--session))
            (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                       (lambda (_session task-name message callback &rest _)
                         (setq captured-message message)
                         (funcall callback
                                  (list :outcome 'success
                                        :record
                                        (niwa-agent-record--create
                                         :path (concat "/root/" task-name))))
                         #'ignore)))
              (niwa-review--run-task
               "prompt" "target" #'ignore nil nil 'review
               root '(:type uncommitted))
              (let ((deadline (+ (float-time) 5)))
                (while (and (not captured-message) (< (float-time) deadline)) (sleep-for .002)))
              (should (string-match
                       "Review package file: \\([^\n]+\\)"
                       captured-message))
              (should (file-exists-p (match-string 1 captured-message))))))
      (kill-buffer data)
      (delete-directory root t))))

(niwa-deftest niwa-review--insert-progress-handle ()
  ,test
  (test)
  :doc "review status redraw preserves a multiline leading-> composer draft"
  (niwa-view-test--with-buffers
    (let* ((draft "> quoted\nsecond line")
           (session (niwa-session--create :name "review-redraw"))
           (invocation
            (niwa-agent-invocation--create
             :agent (niwa-agent-get "reviewer")
             :agent-id "reviewer--draft"
             :path "/root/review"
             :description "review draft"
             :parent-session session
             :parent-data-buffer data-buf
             :transcript-status 'running
             :transcript-relative-path "agents/reviewer--draft.chat.org")))
      (with-current-buffer data-buf
        (setq-local niwa--session session)
        (setq-local niwa-session--read-only-mode nil))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft draft 4))
      (with-current-buffer data-buf
        (niwa-review--insert-progress-handle
         invocation "review draft" 'review))
      (with-current-buffer view-buf
        (should (string= draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-test--composer-point-offset))))
      (setf (niwa-agent-invocation-transcript-status invocation) 'completed)
      (setf (niwa-agent-invocation-call-count invocation) 2)
      (let ((niwa-view-agent-refresh-delay 0))
        (niwa-agent-conversation-refresh invocation))
      (with-current-buffer data-buf
        (pcase-let ((`(,start . ,end)
                     (niwa-tool-render-data-find-agent-block
                      "reviewer--draft")))
          (let* ((raw (buffer-substring start end))
                 (render-data (cdr (niwa-tool-render-data-extract raw))))
            (should (eq 'completed (plist-get render-data :status)))
            (should (= 2 (plist-get render-data :calls))))))
      (with-current-buffer view-buf
        (should (string= draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-test--composer-point-offset)))))))

(niwa-deftest niwa-review--handle-view-outcome ()
  ,test
  (test)
  :doc "validation result redraw preserves a multiline leading-> composer draft"
  (niwa-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer data-buf
        (setq-local niwa-session--read-only-mode nil))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 4))
      (cl-letf (((symbol-function 'niwa-skills-input-insert-fork-result)
                 (lambda (_outcome)
                   (with-current-buffer view-buf
                     (niwa-view--full-rerender)))))
        (niwa-review--handle-view-outcome
         '(:status ok :kind fork :result "VERDICT: PASS")
         view-buf data-buf 'verify))
      (with-current-buffer view-buf
        (should (string= draft (niwa-view-composer-draft-text nil t)))
        (should (= 4 (niwa-view-test--composer-point-offset))))))

  :doc "schema-invalid reviewer JSON still settles the view request"
  (niwa-view-test--with-buffers
    (let ((raw "{\"findings\":{\"title\":\"bad\"}}"))
      (with-current-buffer data-buf
        (setq-local niwa--session
                    (niwa-session--create :name "review"))
        (niwa-request-begin niwa--session))
      (cl-letf (((symbol-function 'niwa-skills-input-insert-fork-result)
                 (lambda (outcome)
                   (should (equal raw (plist-get outcome :result)))
                   (niwa-request-end))))
        (niwa-review--handle-view-outcome
         `(:status ok :kind fork :result ,raw)
         view-buf data-buf 'review))
      (with-current-buffer data-buf
        (should-not niwa--current-request)))))

(niwa-deftest niwa-review--current-data-buffer ()
  ,test
  (test)
  :doc "does not treat ordinary buffers as review transcript targets"
  (with-temp-buffer
    (should-not (niwa-review--current-data-buffer)))

  :doc "resolves a view buffer to its backing data buffer"
  (let ((data (generate-new-buffer " *niwa-review-data*"))
        (view (generate-new-buffer " *niwa-review-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local niwa--session
                        (niwa-session--create :name "review")))
          (with-current-buffer view
            (setq-local niwa--data-buffer data)
            (should (eq data (niwa-review--current-data-buffer)))))
      (kill-buffer data)
      (kill-buffer view))))

(niwa-deftest niwa-review ()
  ,test
  (test)
  :doc "refuses direct invocation from historical segment inspection"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'niwa-view-historical-segment-p)
                 (lambda () t)))
        (should-error (niwa-review) :type 'user-error)))))

(niwa-deftest niwa-verify ()
  ,test
  (test)
  :doc "refuses direct invocation from historical segment inspection"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'niwa-view-historical-segment-p)
                 (lambda () t)))
        (should-error (niwa-verify) :type 'user-error)))))

(niwa-deftest niwa-review--send-from-view ()
  ,test
  (test)
  :doc "a draft typed while its prompt hook runs survives the review send"
  (let* ((root (make-temp-file "niwa-review-redraft" t))
         (workspace (niwa-workspace-get-or-create
                     'project "review-redraft" root "review-redraft"))
         (session (niwa-session-create "main" workspace root))
         (niwa-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type command
                                       :command "sleep 0.2; printf '{}'"
                                       :timeout 5)))))))
         (started 0))
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local niwa--session session
                        niwa--workspace workspace)
            (niwa-session-set-root-buffer session data-buf))
          (with-current-buffer view-buf
            (setq-local niwa--session session
                        niwa--workspace workspace))
          (cl-letf (((symbol-function 'niwa-review--run-task)
                     (lambda (&rest _) (cl-incf started) #'ignore)))
            (with-current-buffer view-buf
              (niwa-view-test--insert-composer-draft "/review target")
              (niwa-review--send-from-view
               "/review target" "prompt" "target" view-buf data-buf)
              (should (= 0 started))
              ;; The user keeps typing while the hook runs.
              (with-current-buffer (niwa-view-composer-buffer)
                (delete-region (niwa-view-composer-draft-start) (niwa-view-composer-draft-end)))
              (with-current-buffer (niwa-view-composer-buffer)
                (goto-char (niwa-view-composer-draft-end))
                (insert "> quoted\nsecond line"))
              (let ((deadline (+ (float-time) 10)))
                (while (and (= 0 started) (< (float-time) deadline))
                  (accept-process-output nil 0.05))
                (should (= 1 started))
                (should (equal "> quoted\nsecond line"
                               (niwa-view-composer-draft-text nil t)))))))
      (delete-directory root t)))

  :doc "persists consumed start context across a full transcript rerender"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'file :id "review-context" :root (niwa-test--placeholder-root)
                       :name "review-context"))
           (session (niwa-session-create "main" workspace))
           task-context)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace workspace)
        (niwa-hooks-record-session-context
         session '(:additional-context ("review startup context"))
         'SessionStart))
      (with-current-buffer view-buf
        (setq-local niwa--session session
                    niwa--workspace workspace))
      (cl-letf (((symbol-function 'niwa-review--run-task)
                 (lambda (_prompt _hint _callback &optional context &rest _)
                   (setq task-context context))))
        (niwa-review--send-from-view
         "/review target" "prompt" "target" view-buf data-buf))
      (should (string-match-p "review startup context" task-context))
      (should-not (niwa-session-hook-context-pending session))
      (with-current-buffer data-buf
        (should (string-match-p "review startup context" (buffer-string))))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) (niwa-view-transcript-tail))))
          (should (string-match-p "hook context added" text))
          (should-not (string-match-p "review startup context" text)))
        (goto-char (point-min))
        (search-forward "hook context added")
        (niwa-view-toggle-section)
        (should (string-match-p
                 "review startup context"
                 (buffer-substring-no-properties
                  (point-min) (niwa-view-transcript-tail)))))))

  :doc "does not prepare package evidence when the submit hook rejects"
  (niwa-view-test--with-buffers
    (cl-letf (((symbol-function 'niwa-view--run-prompt-submit-hook)
               (lambda (&rest _args) nil))
              ((symbol-function 'niwa-review--write-target-package)
               (lambda (&rest _args)
                 (ert-fail "package preparation ran before acceptance"))))
      (niwa-review--send-from-view
       "/review target" "prompt" "target" view-buf data-buf
       'review "/tmp/" '(:type uncommitted)))))

(niwa-deftest niwa-review--dispatch (:quiet t)
  ,test
  (test)
  :doc "routes standalone review output to a safe data buffer"
  (let ((source (generate-new-buffer " *niwa-review-source*"))
        (data (generate-new-buffer " *niwa-review-data*"))
        invoke-buffer task-agent task-description task-prompt)
    (unwind-protect
        (with-current-buffer source
          (insert "source text")
          (with-current-buffer data
            (setq-local niwa--session
                        (niwa-session--create :name "review")))
          (cl-letf (((symbol-function 'niwa-review--ensure-dispatch-deps)
                     #'ignore)
                    ((symbol-function
                      'niwa-review--ensure-standalone-data-buffer)
                     (lambda (_cwd) data))
                    ((symbol-function 'niwa-review--run-task)
                     (lambda (prompt hint callback &optional _context
                                     _progress _command &rest _)
                       (setq invoke-buffer (current-buffer))
                       (setq task-agent "reviewer")
                       (setq task-description hint)
                       (setq task-prompt prompt)
                       (funcall callback
                                '(:status ok :kind fork
                                  :result "review result"))))
                    ((symbol-function 'niwa-skills-input-insert-fork-result)
                     (lambda (outcome)
                       (insert (plist-get outcome :result)))))
            (niwa-review--dispatch "prompt" "target" "/tmp/")
            (should (equal "source text" (buffer-string)))
            (should (eq data invoke-buffer))
            (should (equal "reviewer" task-agent))
            (should (equal "target" task-description))
            (should (equal "prompt" task-prompt))
            (with-current-buffer data
              ;; Dispatch leaves the preset-owned role roster alone.
              (should-not niwa-agents--specs)
              (let ((text (buffer-string)))
                (should (string-search "/review target" text))
                (should (string-search "review result" text))
                (should (< (string-match-p "/review target" text)
                           (string-match-p "review result" text)))))))
      (kill-buffer source)
      (kill-buffer data)))

  :doc "rejects direct authority gates before creating package evidence"
  (let ((data (generate-new-buffer " *niwa-review-busy-data*"))
        (root (file-name-as-directory
               (make-temp-file "niwa-review-rejected-" t))))
    (unwind-protect
        (dolist (state '(request compaction read-only))
          (with-current-buffer data
            (setq-local niwa--session
                        (niwa-session--create
                         :name "review")
                        niwa--current-request (eq state 'request)
                        niwa-compact-run-in-flight (eq state 'compaction)
                        niwa-session--read-only-mode (eq state 'read-only))
            (cl-letf (((symbol-function 'niwa-review--ensure-dispatch-deps)
                       #'ignore)
                      ((symbol-function 'niwa-review--current-data-buffer)
                       (lambda () data)))
              (should-error
               (niwa-review--dispatch
                "prompt" "target" root 'review '(:type uncommitted))
               :type 'user-error)
              (should-not
               (file-exists-p
                (niwa-review--package-directory root))))))
      (kill-buffer data)
      (delete-directory root t))))

(niwa-deftest niwa-review--handle-direct-outcome ()
  ,test
  (test)
  :doc "does not insert failed direct review outcomes"
  (let ((data (generate-new-buffer " *niwa-review-error-data*"))
        message-text inserted)
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local niwa--workspace
                        (niwa-workspace--create :root "/tmp/review-origin")))
          (cl-letf (((symbol-function 'niwa-skills-input-insert-fork-result)
                   (lambda (_outcome)
                     (setq inserted t)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq message-text (apply #'format format-string args)))))
          (niwa-review--handle-direct-outcome
           '(:status error :reason hook-blocked :message "blocked")
           data)
          (should-not inserted)
          (should (equal "niwa [/tmp/review-origin]: blocked (review failed)" message-text))
          (should (equal "" (with-current-buffer data (buffer-string))))))
      (kill-buffer data)))

  :doc "inserts successful direct fork outcomes"
  (let ((data (generate-new-buffer " *niwa-review-ok-data*")))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-skills-input-insert-fork-result)
                   (lambda (outcome)
                     (insert (plist-get outcome :result)))))
          (niwa-review--handle-direct-outcome
           '(:status ok :kind fork :result "plain review")
           data)
          (with-current-buffer data
            (should (string-search "plain review" (buffer-string)))))
      (kill-buffer data)))

  :doc "schema-invalid reviewer JSON still reaches terminal settlement"
  (let ((data (generate-new-buffer " *niwa-review-invalid-json*"))
        (raw "{\"findings\":1,\"overall_explanation\":\"bad\"}"))
    (unwind-protect
        (with-current-buffer data
          (setq-local niwa--session
                      (niwa-session--create :name "review"))
          (niwa-request-begin niwa--session)
          (cl-letf (((symbol-function 'niwa-skills-input-insert-fork-result)
                     (lambda (outcome)
                       (should (equal raw (plist-get outcome :result)))
                       (niwa-request-end))))
            (niwa-review--handle-direct-outcome
             `(:status ok :kind fork :result ,raw) data)
            (should-not niwa--current-request)))
      (kill-buffer data))))

(niwa-deftest niwa-review-strip-user-action-blocks ()
  ,test
  (test)
  :doc "removes only synthetic review action blocks"
  (let* ((text "Visible\n<user_action>\n  <action>review</action>\n  <results>\n  hidden\n  </results>\n</user_action>\nStill visible")
         (stripped (niwa-review-strip-user-action-blocks text)))
    (should (equal "Visible\nStill visible" stripped)))

  :doc "preserves non-review user action text and blank lines"
  (let ((text "Visible\n\n<user_action>\n  <action>other</action>\n</user_action>\n\nStill visible"))
    (should (equal text (niwa-review-strip-user-action-blocks text)))))

(niwa-deftest niwa-review-strip-user-action-blocks/no-op ()
  ,test
  (test)
  :doc "plain text retains identity and trimming preserves properties"
  (let* ((case-fold-search nil)
         (text (propertize "Visible text" 'face 'bold)))
    (should (eq text (niwa-review-strip-user-action-blocks text)))
    (should (equal-including-properties
             text (niwa-review-strip-user-action-blocks
                   (concat " \n" text "\t")))))
  :doc "matching and incomplete blocks retain their cleanup semantics"
  (let ((case-fold-search t))
    (with-temp-buffer
      (setq-local case-fold-search nil)
      (should (equal "Visible" (niwa-review-strip-user-action-blocks
                               "<USER_ACTION><ACTION>REVIEW</ACTION></USER_ACTION>\nVisible")))
      (should (equal "<user_action>unfinished"
                     (niwa-review-strip-user-action-blocks
                      " <user_action>unfinished "))))))

(niwa-deftest niwa-review-strip-user-action-blocks/default-case-folding ()
  (let ((old (default-value 'case-fold-search))
        (text "<USER_ACTION><ACTION>REVIEW</ACTION></USER_ACTION>"))
    (unwind-protect
        (progn
          (set-default 'case-fold-search nil)
          (should (eq text (niwa-review-strip-user-action-blocks text))))
      (set-default 'case-fold-search old))))

(provide 'test-niwa-review)

;;; test-niwa-review.el ends here
