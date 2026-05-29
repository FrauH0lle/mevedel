;;; test-mevedel-review.el --- Tests for review workflow -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-review)
(require 'mevedel-tool-exec)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


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

(mevedel-deftest mevedel-review--augment-skill ()
  ,test
  (test)
  :doc "adds review permission rules to a copied skill"
  (let ((skill (mevedel-skill--create :name "review")))
    (cl-letf (((symbol-function 'mevedel-review--permission-rules)
               (lambda () '(("Bash" :pattern "git diff:*" :action allow)))))
      (let ((augmented (mevedel-review--augment-skill skill)))
        (should-not (eq skill augmented))
        (should (null (mevedel-skill-allowed-tool-rules skill)))
        (should (equal mevedel-review--allowed-tool-entries
                       (mevedel-skill-allowed-tools augmented)))
        (should (equal '(("Bash" :pattern "git diff:*" :action allow))
                       (mevedel-skill-allowed-tool-rules augmented)))))))

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
  (let ((mevedel-permission-rules (mevedel-review--permission-rules))
        (mevedel-bash-dangerous-commands nil)
        (mevedel--session nil)
        (mevedel--current-request nil)
        (mevedel--agent-invocation nil))
    (should
     (eq 'allow
         (mevedel-tools--check-bash-permission
          "git show --stat --oneline --decorate --no-renames 52e4748 && git show --format=fuller --no-ext-diff --unified=80 --no-renames 52e4748")))
    (should
     (eq 'allow
         (mevedel-tools--check-bash-permission
          "git rev-parse 52e4748 && git diff --stat 52e4748^ 52e4748")))
    (should
     (eq 'allow
         (mevedel-tools--check-bash-permission
          "GIT_PAGER=cat git diff --name-only 52e4748^ 52e4748")))
    (should
     (eq 'allow
         (mevedel-tools--check-bash-permission
          "GIT_PAGER=cat git diff --name-only '52e4748^' 52e4748")))
    (should
     (eq 'allow
         (mevedel-tools--check-bash-permission
          "git cat-file -p 52e4748 | head")))
    (should
     (eq 'allow
         (mevedel-tools--check-bash-permission
          "git diff --stat HEAD~2 HEAD~1 && git diff --unified=80 HEAD~2 HEAD~1 -- mevedel-review.el"))))

  :doc "deny commands outside the reviewer inspection allowlist"
  (let ((mevedel-permission-rules (mevedel-review--permission-rules))
        (mevedel-bash-dangerous-commands nil)
        (mevedel--session nil)
        (mevedel--current-request nil)
        (mevedel--agent-invocation nil))
    (should
     (eq 'deny
         (mevedel-tools--check-bash-permission "git checkout main")))
    (should
     (eq 'deny
         (mevedel-tools--check-bash-permission
          "GIT_EXTERNAL_DIFF=sh git diff HEAD")))
    (should
     (eq 'deny
         (mevedel-tools--check-bash-permission "make test")))))

(mevedel-deftest mevedel-review--review-skill ()
  ,test
  (test)
  :doc "uses bundled review skill even when session has an override"
  (let* ((override (mevedel-skill--create
                    :name "review" :context 'inline :agent "other"
                    :source 'project))
         (session (mevedel-session--create
                   :name "main" :skills (list override)))
         (skill (mevedel-review--review-skill session)))
    (should (eq 'bundled (mevedel-skill-source skill)))
    (should (eq 'fork (mevedel-skill-context skill)))
    (should (equal "reviewer" (mevedel-skill-agent skill))))

  :doc "rejects a malformed bundled skill definition"
  (cl-letf (((symbol-function 'mevedel-skills--build-skill)
             (lambda (_file _source)
               (mevedel-skill--create
                :name "review" :context 'inline :agent "other"
                :source 'bundled))))
    (should-error (mevedel-review--review-skill nil) :type 'user-error)))

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

(mevedel-deftest mevedel-review--ensure-reviewer-agent-spec ()
  ,test
  (test)
  :doc "installs reviewer spec into the dispatch data buffer"
  (let ((data (generate-new-buffer " *mevedel-review-agent-spec*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel-agent-exec--agents nil))
          (mevedel-review--ensure-reviewer-agent-spec data)
          (with-current-buffer data
            (let ((spec (cdr (assoc "reviewer"
                                    mevedel-agent-exec--agents))))
              (should spec)
              (should (plist-get spec :system))
              (should (plist-get spec :tools)))))
      (kill-buffer data)))

  :doc "installs verifier spec into the dispatch data buffer"
  (let ((data (generate-new-buffer " *mevedel-verify-agent-spec*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel-agent-exec--agents nil))
          (mevedel-review--ensure-agent-spec data 'verify)
          (with-current-buffer data
            (let ((spec (cdr (assoc "verifier"
                                    mevedel-agent-exec--agents))))
              (should spec)
              (should (plist-get spec :system))
              (should (plist-get spec :tools)))))
      (kill-buffer data))))

(mevedel-deftest mevedel-review--run-task ()
  ,test
  (test)
  :doc "dispatches the reviewer agent directly with expansion and review rules"
  (let ((data (generate-new-buffer " *mevedel-review-run-task*"))
        captured-agent captured-description captured-prompt captured-rules
        outcome)
    (unwind-protect
        (with-current-buffer data
          (require 'mevedel-tool-ui)
          (setq-local mevedel--session
                      (mevedel-session--create :name "review"))
          (cl-letf (((symbol-function 'mevedel-review--review-skill)
                     (lambda (_session)
                       (mevedel-skill--create
                        :name "review" :context 'fork
                        :agent "reviewer" :source 'bundled
                        :allowed-tool-rules '((rule . git))
                        :hooks '((Stop ((:hooks ((:elisp ignore)))))))))
                    ((symbol-function 'mevedel-agent-get)
                     (lambda (name)
                       (and (equal name "reviewer")
                            (mevedel-agent--create :name "reviewer"))))
                    ((symbol-function 'mevedel-skills--run-expansion-hook)
                     (lambda (_skill arguments prompt trigger _session callback)
                       (should (equal "prompt" arguments))
                       (should (equal "prompt" prompt))
                       (should (eq 'user-slash trigger))
                       (funcall callback
                                "expanded prompt"
                                '(:continue t))))
                    ((symbol-function 'mevedel-tools--task)
                     (lambda (main-cb agent description prompt &rest args)
                       (setq captured-agent (mevedel-agent-name agent))
                       (setq captured-description description)
                       (setq captured-prompt prompt)
                       (setq captured-rules
                             (plist-get args :skill-permission-rules))
                       (funcall main-cb
                                '(:result "review json"
                                  :render-data
                                  (:agent-id "reviewer--abc"))))))
            (mevedel-review--run-task
             "prompt" "target"
             (lambda (result) (setq outcome result))
             "<hook-context>extra</hook-context>")
            (should (equal "reviewer" captured-agent))
            (should (equal "target" captured-description))
            (should (equal
                     "expanded prompt\n\n<hook-context>extra</hook-context>"
                     captured-prompt))
            (should (equal '((rule . git)) captured-rules))
            (should (eq 'ok (plist-get outcome :status)))
            (should (eq 'fork (plist-get outcome :kind)))
            (should (equal "review json" (plist-get outcome :result)))
            (should (equal "reviewer--abc" (plist-get outcome :agent-id)))
            (should (plist-get outcome :mevedel-review-command))))
      (kill-buffer data)))

  :doc "dispatches the verifier agent without review output marking"
  (let ((data (generate-new-buffer " *mevedel-verify-run-task*"))
        captured-agent captured-rules outcome)
    (unwind-protect
        (with-current-buffer data
          (require 'mevedel-tool-ui)
          (setq-local mevedel--session
                      (mevedel-session--create :name "verify"))
          (cl-letf (((symbol-function 'mevedel-review--verify-skill)
                     (lambda (_session)
                       (mevedel-skill--create
                        :name "verify" :context 'fork
                        :agent "verifier" :source 'bundled
                        :allowed-tool-rules '((rule . git)))))
                    ((symbol-function 'mevedel-agent-get)
                     (lambda (name)
                       (and (equal name "verifier")
                            (mevedel-agent--create :name "verifier"))))
                    ((symbol-function 'mevedel-skills--run-expansion-hook)
                     (lambda (_skill arguments prompt trigger _session callback)
                       (should (equal "prompt" arguments))
                       (should (equal "prompt" prompt))
                       (should (eq 'user-slash trigger))
                       (funcall callback "expanded prompt" '(:continue t))))
                    ((symbol-function 'mevedel-tools--task)
                     (lambda (main-cb agent _description _prompt &rest args)
                       (setq captured-agent (mevedel-agent-name agent))
                       (setq captured-rules
                             (plist-get args :skill-permission-rules))
                       (funcall main-cb
                                '(:result "verifier result"
                                  :render-data
                                  (:agent-id "verifier--abc"))))))
            (mevedel-review--run-task
             "prompt" "target"
             (lambda (result) (setq outcome result))
             nil nil 'verify)
            (should (equal "verifier" captured-agent))
            (should (equal '((rule . git)) captured-rules))
            (should (eq 'ok (plist-get outcome :status)))
            (should (equal "verifier result" (plist-get outcome :result)))
            (should (equal "verifier--abc" (plist-get outcome :agent-id)))
            (should-not (plist-get outcome :mevedel-review-command))))
      (kill-buffer data)))

  :doc "reports pre-dispatch task errors through the callback"
  (let ((data (generate-new-buffer " *mevedel-review-run-task-error*"))
        outcome)
    (unwind-protect
        (with-current-buffer data
          (require 'mevedel-tool-ui)
          (cl-letf (((symbol-function 'mevedel-review--review-skill)
                     (lambda (_session)
                       (mevedel-skill--create
                        :name "review" :context 'fork
                        :agent "reviewer" :source 'bundled)))
                    ((symbol-function 'mevedel-agent-get)
                     (lambda (name)
                       (and (equal name "reviewer")
                            (mevedel-agent--create :name "reviewer"))))
                    ((symbol-function 'mevedel-review--permission-rules)
                     (lambda () nil))
                    ((symbol-function 'mevedel-skills--run-expansion-hook)
                     (lambda (_skill _arguments prompt _trigger _session callback)
                       (funcall callback prompt nil)))
                    ((symbol-function 'mevedel-tools--task)
                     (lambda (&rest _args)
                       (error "dispatch exploded"))))
            (mevedel-review--run-task
             "prompt" "target"
             (lambda (result) (setq outcome result)))
            (should (eq 'error (plist-get outcome :status)))
            (should (eq 'agent-dispatch-failed
                        (plist-get outcome :reason)))
            (should (equal "dispatch exploded"
                           (plist-get outcome :message)))))
      (kill-buffer data))))

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
        invoke-buffer task-agent task-description task-prompt)
    (unwind-protect
        (with-current-buffer source
          (insert "source text")
          (with-current-buffer data
            (setq-local mevedel--session
                        (mevedel-session--create :name "review")))
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
              (should (assoc "reviewer" mevedel-agent-exec--agents))
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
                     (lambda () data))
                    ((symbol-function 'mevedel-review--review-skill)
                     (lambda (_session)
                       (mevedel-skill--create
                        :name "review" :context 'fork
                        :agent "reviewer" :source 'bundled))))
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
