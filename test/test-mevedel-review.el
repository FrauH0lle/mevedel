;;; test-mevedel-review.el --- Tests for review workflow -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-review)
(require 'mevedel-agents)
(require 'mevedel-agent-control)
(require 'mevedel-agent-conversation)
(require 'mevedel-agent-exec)
(require 'mevedel-tool-exec)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar mevedel--agent-invocation)
(defvar mevedel-agents--specs)
(defvar mevedel-bash-dangerous-commands)
(defvar mevedel-session--read-only-mode)


(mevedel-deftest mevedel-review--target-prompt-and-hint ()
  ,test
  (test)
  :doc "base branch prompt includes pre-resolved merge base"
  (cl-letf (((symbol-function 'mevedel-review--git-string)
             (lambda (_cwd &rest args)
               (and (equal args '("merge-base" "HEAD" "main"))
                    "abc123"))))
    (let ((prompt+hint
           (mevedel-review--target-prompt-and-hint
            '(:type base-branch :branch "main")
            "/tmp/project/")))
      (should (string-search "merge base commit for this comparison is abc123"
                             (car prompt+hint)))
      (should (string-search "git diff abc123" (car prompt+hint)))
      (should (equal "changes against 'main'" (cdr prompt+hint)))))

  :doc "custom target uses trimmed instructions"
  (let ((prompt+hint
         (mevedel-review--target-prompt-and-hint
          '(:type custom :instructions "  Check only tests.  ")
          "/tmp/project/")))
    (should (equal "Check only tests." (car prompt+hint)))
    (should (equal "Check only tests." (cdr prompt+hint))))

  :doc "explicit slash target args parse but free-form text stays custom"
  (should (equal '(:type uncommitted)
                 (mevedel-review--parse-target-arg "current")))
  (should (equal '(:type commit :sha "HEAD" :title "")
                 (mevedel-review--parse-target-arg "HEAD")))
  (should (equal '(:type base-branch :branch "main")
                 (mevedel-review--parse-target-arg "branch:main")))
  (should (equal '(:type commit :sha "abc123" :title "")
                 (mevedel-review--parse-target-arg "commit:abc123")))
  (should-not (mevedel-review--parse-target-arg "current changes"))

  :doc "verify prompt mirrors review targets with verifier verdict wording"
  (cl-letf (((symbol-function 'mevedel-review--git-string)
             (lambda (_cwd &rest args)
               (and (equal args '("merge-base" "HEAD" "main"))
                    "abc123"))))
    (let ((prompt+hint
           (mevedel-review--verify-target-prompt-and-hint
            '(:type base-branch :branch "main")
            "/tmp/project/")))
      (should (string-search "merge base commit for this comparison is abc123"
                             (car prompt+hint)))
      (should (string-search "git diff abc123" (car prompt+hint)))
      (should (string-search "VERDICT: PASS" (car prompt+hint)))
      (should (equal "changes against 'main'" (cdr prompt+hint))))))

(mevedel-deftest mevedel-review--read-target ()
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
                     (mevedel-review--read-target "/tmp/project/" 'verify)))
      (should (equal "Verify instructions: " seen-prompt)))))

(mevedel-deftest mevedel-review--write-package
  (:doc "writes a review package file for a git range")
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-review-package-" t)))
         (package-file (file-name-concat root ".mevedel"
                                         "review-packages"
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
                           (mevedel-review--write-package
                            root
                            (list :type 'range
                                  :base base
                                  :head "HEAD")
                            package-file)))
            (let ((text (with-temp-buffer
                          (insert-file-contents package-file)
                          (buffer-string))))
              (should (string-search "# Review package:" text))
              (should (string-search "## Commits" text))
              (should (string-search "change" text))
              (should (string-search "## Diff" text))
              (should (string-search "+two" text)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-review--prompt-with-package
  (:doc "tells reviewers to read the package before broad git inspection")
  (let ((prompt (mevedel-review--prompt-with-package
                 "Review this range."
                 "/tmp/review.diff"
                 'review)))
    (should (string-search "Review package file: /tmp/review.diff" prompt))
    (should (string-search "Read that file first" prompt))
    (should (string-search "Do not rerun broad git commands" prompt))))

(mevedel-deftest mevedel-review-parse-output ()
  ,test
  (test)
  :doc "parses JSON surrounded by stray prose"
  (let* ((json "{\"findings\":[{\"title\":\"[P2] Fix thing\",\"body\":\"body\",\"priority\":2,\"confidence_score\":0.8,\"code_location\":{\"absolute_file_path\":\"/tmp/a.el\",\"line_range\":{\"start\":3,\"end\":3}}}],\"overall_correctness\":\"patch is incorrect\",\"overall_explanation\":\"One issue.\",\"overall_confidence_score\":0.7}")
         (parsed (mevedel-review-parse-output (concat "Here:\n" json "\nDone"))))
    (should (equal "One issue."
                   (plist-get parsed :overall_explanation)))
    (should (= 1 (length (plist-get parsed :findings))))
    (should (equal "[P2] Fix thing"
                   (plist-get (car (plist-get parsed :findings))
                              :title))))

  :doc "falls back to overall explanation when JSON parse fails"
  (let ((parsed (mevedel-review-parse-output "plain review text")))
    (should (equal "plain review text"
                   (plist-get parsed :overall_explanation)))
    (should (null (plist-get parsed :findings)))))

(mevedel-deftest mevedel-review-render-output-text ()
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
         (rendered (mevedel-review-render-output-text output)))
    (should (string-search "One issue." rendered))
    (should (string-search "Review comment:" rendered))
    (should (string-search "[P2] Fix thing -- /tmp/a.el:3-3" rendered))
    (should (string-search "Only fails on x." rendered))))

(mevedel-deftest mevedel-review-render-user-action ()
  ,test
  (test)
  :doc "escapes review text before embedding it in the synthetic block"
  (let* ((action
          (mevedel-review-render-user-action
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
                   (mevedel-review-strip-user-action-blocks wrapped)))))

(mevedel-deftest mevedel-review-transform-outcome ()
  ,test
  (test)
  :doc "review fork output becomes summary plus synthetic user action"
  (let* ((raw "{\"findings\":[],\"overall_correctness\":\"patch is correct\",\"overall_explanation\":\"No issues.\",\"overall_confidence_score\":0.9}")
         (outcome (mevedel-review-transform-outcome
                   "review"
                   (mevedel-review--mark-command-outcome
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
    (should (eq outcome (mevedel-review-transform-outcome "review" outcome)))))

(mevedel-deftest mevedel-review--permission-rules ()
  ,test
  (test)
  :doc "allow git inspection while denying other Bash commands"
  (require 'mevedel-permissions)
  (let ((rules (mevedel-review--permission-rules)))
    (should (eq 'allow
                (mevedel-permission--rules-action
                 rules "Bash" :pattern "git diff --stat")))
    (should (eq 'deny
                (mevedel-permission--rules-action
                 rules "Bash" :pattern "make test"))))

  :doc "verify grants git inspection without denying validation commands"
  (require 'mevedel-permissions)
  (let ((rules (mevedel-review--verify-permission-rules)))
    (should (eq 'allow
                (mevedel-permission--rules-action
                 rules "Bash" :pattern "git diff --stat")))
    (should (null
             (mevedel-permission--rules-action
              rules "Bash" :pattern "make test")))))

(mevedel-deftest mevedel-review--bash-permissions ()
  ,test
  (test)
  :doc "allow common reviewer git inspection commands"
  (let* ((rules (mevedel-review--permission-rules))
         (context (list :mode 'ask :buckets
                        (list (cons :defcustom rules))))
         (mevedel-bash-dangerous-commands nil))
    (cl-labels ((decide (command)
                  (mevedel-tools--check-bash-permission
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
            "git diff --stat HEAD~2 HEAD~1 && git diff --unified=80 HEAD~2 HEAD~1 -- mevedel-review.el")))))

  :doc "deny commands outside the reviewer inspection allowlist"
  (let* ((rules (mevedel-review--permission-rules))
         (context (list :mode 'ask :buckets
                        (list (cons :defcustom rules))))
         (mevedel-bash-dangerous-commands nil))
    (cl-labels ((decide (command)
                  (mevedel-tools--check-bash-permission
                   command :permission-context context)))
      (should (eq 'deny (decide "git checkout main")))
      (should (eq 'deny (decide "GIT_EXTERNAL_DIFF=sh git diff HEAD")))
      (should (eq 'deny (decide "make test"))))))

(mevedel-deftest mevedel-review--ensure-dispatch-deps ()
  ,test
  (test)
  :doc "loads the reviewer agent registry for autoloaded dispatch"
  (mevedel-review--ensure-dispatch-deps)
  (should (fboundp 'mevedel-agent-get))
  (should (mevedel-agent-get "reviewer"))

  :doc "re-registers reviewer after another test clears the registry"
  (let ((mevedel-agent--registry nil))
    (mevedel-review--ensure-dispatch-deps)
    (should (mevedel-agent-get "reviewer")))

  :doc "loads the verifier agent registry for autoloaded dispatch"
  (mevedel-review--ensure-dispatch-deps 'verify)
  (should (mevedel-agent-get "verifier"))

  :doc "re-registers verifier after another test clears the registry"
  (let ((mevedel-agent--registry nil))
    (mevedel-review--ensure-dispatch-deps 'verify)
    (should (mevedel-agent-get "verifier"))))

(mevedel-deftest mevedel-review--ensure-agent-spec ()
  ,test
  (test)
  :doc "installs reviewer spec into the dispatch data buffer"
  (let ((data (generate-new-buffer " *mevedel-review-agent-spec*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel-agents--specs nil))
          (mevedel-review--ensure-agent-spec data 'review)
          (with-current-buffer data
            (let ((spec (cdr (assoc "reviewer"
                                    mevedel-agents--specs))))
              (should spec)
              (should (plist-get spec :system))
              (should (plist-get spec :tools)))))
      (kill-buffer data)))

  :doc "installs verifier spec into the dispatch data buffer"
  (let ((data (generate-new-buffer " *mevedel-verify-agent-spec*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel-agents--specs nil))
          (mevedel-review--ensure-agent-spec data 'verify)
          (with-current-buffer data
            (let ((spec (cdr (assoc "verifier"
                                    mevedel-agents--specs))))
              (should spec)
              (should (plist-get spec :system))
              (should (plist-get spec :tools)))))
      (kill-buffer data))))

(mevedel-deftest mevedel-review--next-task-name ()
  ,test
  (test)
  :doc "reserves monotonically suffixed review and verify path names"
  (let* ((session (mevedel-session--create :name "validation"))
         (review (mevedel-agent-record--create :path "/root/review"))
         (review-2 (mevedel-agent-record--create :path "/root/review_2"))
         (verify (mevedel-agent-record--create :path "/root/verify")))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/review" review)
                (cons "/root/review_2" review-2)
                (cons "/root/verify" verify)))
    (should (equal "review_3"
                   (mevedel-review--next-task-name session 'review)))
    (should (equal "verify_2"
                   (mevedel-review--next-task-name session 'verify)))))

(mevedel-deftest mevedel-review--result-outcome ()
  ,test
  (test)
  :doc "maps completed RESULT into the existing fork outcome contract"
  (should
   (equal '(:status ok :kind fork :result "findings"
            :agent-path "/root/review")
          (mevedel-review--result-outcome
           '(:outcome completed :payload "findings" :sender "/root/review"))))

  :doc "maps interrupted and malformed RESULT into workflow errors"
  (let ((interrupted
         (mevedel-review--result-outcome
          '(:outcome interrupted :payload "stopped" :sender "/root/verify")))
        (invalid (mevedel-review--result-outcome '(:outcome unknown))))
    (should (eq 'error (plist-get interrupted :status)))
    (should (eq 'agent-interrupted (plist-get interrupted :reason)))
    (should (equal "stopped" (plist-get interrupted :message)))
    (should (eq 'invalid-agent-result (plist-get invalid :reason)))))

(mevedel-deftest mevedel-review--run-task ()
  ,test
  (test)
  :doc "spawns and awaits a retained reviewer leaf through agent control"
  (let ((data (generate-new-buffer " *mevedel-review-run-task*"))
        captured-options captured-message captured-name captured-session
        outcomes)
    (unwind-protect
        (with-current-buffer data
          (setq-local mevedel--session
                      (mevedel-session--create :name "review"))
          (setq-local mevedel--current-request
                      (mevedel-request--create :session mevedel--session))
          (let ((progress-callback #'ignore))
            (cl-letf (((symbol-function 'mevedel-agent-control-spawn)
                       (lambda (session task-name message &rest options)
                         (setq captured-session session
                               captured-name task-name
                               captured-message message
                               captured-options options)
                         (mevedel-agent-record--create
                          :path (concat "/root/" task-name)))))
              (mevedel-review--run-task
               "prompt" "target"
               (lambda (result) (push result outcomes))
               "<hook-context>extra</hook-context>"
               progress-callback)
              (should (eq mevedel--session captured-session))
              (should (equal "review" captured-name))
              (should (equal "prompt\n\n<hook-context>extra</hook-context>"
                             captured-message))
              (should (equal "reviewer"
                             (plist-get captured-options :role)))
              (should (equal "none"
                             (plist-get captured-options :fork-turns)))
              (should (equal "target"
                             (plist-get captured-options :description)))
              (should (eq progress-callback
                          (plist-get captured-options :on-invocation)))
              (should (equal (mevedel-review--permission-rules)
                             (plist-get captured-options
                                        :skill-permission-rules)))
              (should-not outcomes)
              (should (= 1 (length (mevedel-request-cancellers
                                    mevedel--current-request))))
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
                (should-not (plist-get outcome :mevedel-review-command))))))
      (kill-buffer data)))

  :doc "keeps verifier role, policy, and interrupted outcome distinct"
  (let ((data (generate-new-buffer " *mevedel-verify-run-task*"))
        captured-options outcome)
    (unwind-protect
        (with-current-buffer data
          (setq-local mevedel--session
                      (mevedel-session--create :name "verify"))
          (setq-local mevedel--current-request
                      (mevedel-request--create :session mevedel--session))
          (cl-letf (((symbol-function 'mevedel-agent-control-spawn)
                     (lambda (_session task-name _message &rest options)
                       (setq captured-options options)
                       (mevedel-agent-record--create
                        :path (concat "/root/" task-name)))))
            (mevedel-review--run-task
             "prompt" "target"
             (lambda (result) (setq outcome result))
             nil nil 'verify)
            (should (equal "verifier"
                           (plist-get captured-options :role)))
            (should (equal (mevedel-review--verify-permission-rules)
                           (plist-get captured-options
                                      :skill-permission-rules)))
            (funcall (plist-get captured-options :result-handler)
                     '(:outcome interrupted :payload "stopped"
                       :sender "/root/verify"))
            (should (eq 'error (plist-get outcome :status)))
            (should (eq 'agent-interrupted (plist-get outcome :reason)))
            (should (equal "/root/verify"
                           (plist-get outcome :agent-path)))
            (should-not (plist-get outcome :mevedel-review-command))))
      (kill-buffer data)))

  :doc "request cancellation interrupts once and suppresses the late RESULT"
  (let ((data (generate-new-buffer " *mevedel-review-cancel-task*"))
        result-handler interrupt-target callback-called)
    (unwind-protect
        (with-current-buffer data
          (setq-local mevedel--session
                      (mevedel-session--create :name "cancel"))
          (setq-local mevedel--current-request
                      (mevedel-request--create :session mevedel--session))
          (cl-letf (((symbol-function 'mevedel-agent-control-spawn)
                     (lambda (_session task-name _message &rest options)
                       (setq result-handler
                             (plist-get options :result-handler))
                       (mevedel-agent-record--create
                        :path (concat "/root/" task-name))))
                    ((symbol-function 'mevedel-agent-control-interrupt)
                     (lambda (_session target)
                       (setq interrupt-target target)
                       (funcall result-handler
                                `(:outcome interrupted :payload "cancelled"
                                  :sender ,target)))))
            (mevedel-review--run-task
             "prompt" "target"
             (lambda (_result) (setq callback-called t)))
            (mevedel-request-drain-cancellers mevedel--current-request)
            (should (equal "/root/review" interrupt-target))
            (should-not callback-called)))
      (kill-buffer data)))

  :doc "reports retained-agent dispatch errors through the callback"
  (let ((data (generate-new-buffer " *mevedel-review-run-task-error*"))
        outcome)
    (unwind-protect
        (with-current-buffer data
          (setq-local mevedel--session
                      (mevedel-session--create :name "error"))
          (cl-letf (((symbol-function 'mevedel-agent-control-spawn)
                     (lambda (&rest _args)
                       (error "Dispatch exploded"))))
            (mevedel-review--run-task
             "prompt" "target"
             (lambda (result) (setq outcome result)))
            (should (eq 'error (plist-get outcome :status)))
            (should (eq 'agent-dispatch-failed
                        (plist-get outcome :reason)))
            (should (equal "Dispatch exploded"
                           (plist-get outcome :message)))))
      (kill-buffer data))))

(mevedel-deftest mevedel-review--insert-progress-handle ()
  ,test
  (test)
  :doc "review status redraw preserves a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let* ((draft "> quoted\nsecond line")
           (session (mevedel-session--create :name "review-redraw"))
           (invocation
            (mevedel-agent-invocation--create
             :agent (mevedel-agent-get "reviewer")
             :agent-id "reviewer--draft"
             :path "/root/review"
             :description "review draft"
             :parent-session session
             :parent-data-buffer data-buf
             :transcript-status 'running
             :transcript-relative-path "agents/reviewer--draft.chat.org")))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-session--read-only-mode nil))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view-test--insert-composer-draft draft 4))
      (with-current-buffer data-buf
        (mevedel-review--insert-progress-handle
         invocation "review draft" 'review))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4))))
      (setf (mevedel-agent-invocation-transcript-status invocation) 'completed)
      (setf (mevedel-agent-invocation-call-count invocation) 2)
      (let ((mevedel-view-agent-refresh-delay 0))
        (mevedel-agent-conversation-refresh invocation))
      (with-current-buffer data-buf
        (pcase-let ((`(,start . ,end)
                     (mevedel-pipeline--find-render-data-block-by-agent-id
                      "reviewer--draft")))
          (let* ((raw (buffer-substring-no-properties start end))
                 (render-data (cdr (mevedel-pipeline-extract-render-data raw))))
            (should (eq 'completed (plist-get render-data :status)))
            (should (= 2 (plist-get render-data :calls))))))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))))))

(mevedel-deftest mevedel-review--handle-view-outcome ()
  ,test
  (test)
  :doc "validation result redraw preserves a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer data-buf
        (setq-local mevedel-session--read-only-mode nil))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4))
      (cl-letf (((symbol-function 'mevedel-skills--insert-fork-result)
                 (lambda (_outcome)
                   (with-current-buffer view-buf
                     (mevedel-view--full-rerender)))))
        (mevedel-review--handle-view-outcome
         '(:status ok :kind fork :result "VERDICT: PASS")
         view-buf data-buf 'verify))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))))))

(mevedel-deftest mevedel-review--current-data-buffer ()
  ,test
  (test)
  :doc "does not treat ordinary buffers as review transcript targets"
  (with-temp-buffer
    (should-not (mevedel-review--current-data-buffer)))

  :doc "resolves a view buffer to its backing data buffer"
  (let ((data (generate-new-buffer " *mevedel-review-data*"))
        (view (generate-new-buffer " *mevedel-review-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel--session
                        (mevedel-session--create :name "review")))
          (with-current-buffer view
            (setq-local mevedel--data-buffer data)
            (should (eq data (mevedel-review--current-data-buffer)))))
      (kill-buffer data)
      (kill-buffer view))))

(mevedel-deftest mevedel-review--dispatch ()
  ,test
  (test)
  :doc "routes standalone review output to a safe data buffer"
  (let ((source (generate-new-buffer " *mevedel-review-source*"))
        (data (generate-new-buffer " *mevedel-review-data*"))
        (mevedel-agent--registry nil)
        invoke-buffer task-agent task-description task-prompt)
    (unwind-protect
        (with-current-buffer source
          (insert "source text")
          (with-current-buffer data
            (setq-local mevedel--session
                        (mevedel-session--create :name "review")))
          (mevedel-agents-ensure-reviewer)
          (cl-letf (((symbol-function 'mevedel-review--ensure-dispatch-deps)
                     #'ignore)
                    ((symbol-function
                      'mevedel-review--ensure-standalone-data-buffer)
                     (lambda (_cwd) data))
                    ((symbol-function 'mevedel-review--run-task)
                     (lambda (prompt hint callback &optional _context
                                     _progress _command)
                       (setq invoke-buffer (current-buffer))
                       (setq task-agent "reviewer")
                       (setq task-description hint)
                       (setq task-prompt prompt)
                       (funcall callback
                                '(:status ok :kind fork
                                  :result "review result"))))
                    ((symbol-function 'mevedel-skills--insert-fork-result)
                     (lambda (outcome)
                       (insert (plist-get outcome :result)))))
            (mevedel-review--dispatch "prompt" "target" "/tmp/")
            (should (equal "source text" (buffer-string)))
            (should (eq data invoke-buffer))
            (should (equal "reviewer" task-agent))
            (should (equal "target" task-description))
            (should (equal "prompt" task-prompt))
            (with-current-buffer data
              (should (assoc "reviewer" mevedel-agents--specs))
              (let ((text (buffer-string)))
                (should (string-search "/review target" text))
                (should (string-search "review result" text))
                (should (< (string-match-p "/review target" text)
                           (string-match-p "review result" text)))))))
      (kill-buffer source)
      (kill-buffer data)))

  :doc "rejects direct dispatch while a request is active"
  (let ((data (generate-new-buffer " *mevedel-review-busy-data*")))
    (unwind-protect
        (with-current-buffer data
          (setq-local mevedel--session
                      (mevedel-session--create :name "review"))
          (setq-local mevedel--current-request t)
          (cl-letf (((symbol-function 'mevedel-review--ensure-dispatch-deps)
                    #'ignore)
                    ((symbol-function 'mevedel-review--current-data-buffer)
                     (lambda () data)))
            (should-error
             (mevedel-review--dispatch "prompt" "target" "/tmp/")
             :type 'user-error)))
      (kill-buffer data))))

(mevedel-deftest mevedel-review--handle-direct-outcome ()
  ,test
  (test)
  :doc "does not insert failed direct review outcomes"
  (let ((data (generate-new-buffer " *mevedel-review-error-data*"))
        message-text inserted)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-skills--insert-fork-result)
                   (lambda (_outcome)
                     (setq inserted t)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq message-text (apply #'format format-string args)))))
          (mevedel-review--handle-direct-outcome
           '(:status error :reason hook-blocked :message "blocked")
           data)
          (should-not inserted)
          (should (equal "mevedel: review failed: blocked" message-text))
          (should (equal "" (with-current-buffer data (buffer-string)))))
      (kill-buffer data)))

  :doc "inserts successful direct fork outcomes"
  (let ((data (generate-new-buffer " *mevedel-review-ok-data*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-skills--insert-fork-result)
                   (lambda (outcome)
                     (insert (plist-get outcome :result)))))
          (mevedel-review--handle-direct-outcome
           '(:status ok :kind fork :result "plain review")
           data)
          (with-current-buffer data
            (should (string-search "plain review" (buffer-string)))))
      (kill-buffer data))))

(mevedel-deftest mevedel-review-strip-user-action-blocks ()
  ,test
  (test)
  :doc "removes only synthetic review action blocks"
  (let* ((text "Visible\n<user_action>\n  <action>review</action>\n  <results>\n  hidden\n  </results>\n</user_action>\nStill visible")
         (stripped (mevedel-review-strip-user-action-blocks text)))
    (should (equal "Visible\nStill visible" stripped)))

  :doc "preserves non-review user action text and blank lines"
  (let ((text "Visible\n\n<user_action>\n  <action>other</action>\n</user_action>\n\nStill visible"))
    (should (equal text (mevedel-review-strip-user-action-blocks text)))))

(provide 'test-mevedel-review)

;;; test-mevedel-review.el ends here
