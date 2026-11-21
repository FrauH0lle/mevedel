;;; niwa-review.el -- Codex-style code review command -*- lexical-binding: t -*-

;;; Commentary:

;; Implements the `/review' and `/verify' workflows: choose a shared
;; validation target, await a dedicated asynchronous reviewer or verifier
;; leaf task, and route the result back into the parent transcript.  Review
;; output is parsed from structured JSON and mirrored into a synthetic
;; action so follow-up prompts can refer to findings by number.

;;; Code:

(require 'niwa-utilities)

(require 'cl-lib)
(require 'generator)

(require 'subr-x)
(require 'niwa-skills-ui)
(require 'niwa-prompt-submission)
(require 'niwa-structs)
(require 'niwa-transport)

(eval-when-compile (require 'niwa-agents))

;; `gptel'
(declare-function gptel--update-status
                  "ext:gptel" (status &optional face))
(defvar gptel-display-buffer-action)

;; `niwa-agent-control'
(declare-function niwa-agent-control-interrupt
                  "niwa-agent-control" (session target))
(declare-function niwa-agent-control-settled-result
                  "niwa-agent-control" (record))
(declare-function niwa-agent-control-spawn
                  "niwa-agent-control" t t)
(declare-function niwa-agent-record-path
                  "niwa-agent-control" (cl-x) t)
(autoload 'niwa-agent-control-interrupt "niwa-agent-control")
(autoload 'niwa-agent-control-settled-result "niwa-agent-control")
(autoload 'niwa-agent-control-spawn "niwa-agent-control")
(autoload 'niwa-agent-record-path "niwa-agent-control")

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-refresh
                  "niwa-agent-conversation" (invocation))
(autoload 'niwa-agent-conversation-refresh "niwa-agent-conversation")

;; `niwa-agents'
(declare-function niwa-agent-get "niwa-agents" (name))
(declare-function niwa-agent-invocation-agent "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-agent-id "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-call-count "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-description
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-p "niwa-agents" (object))
(declare-function niwa-agent-invocation-parent-data-buffer
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-path
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-transcript-relative-path
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-transcript-status
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-verdict
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-name "niwa-agents" (cl-x) t)
(declare-function niwa-agent-resolve-role "niwa-agents" (role))

;; `niwa-chat'
(declare-function niwa--active-chat-buffer
                  "niwa-chat" (&optional workspace))
(declare-function niwa--chat-buffer
                  "niwa-chat"
                  (session-name &optional create workspace working-directory))

;; `niwa-compact-run'
(defvar niwa-compact-run-in-flight)

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")

;; `niwa-models'
(declare-function niwa-model-resolve-workload
                  "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(autoload 'niwa-model-resolve-workload "niwa-models")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-skills-core'
(declare-function niwa-skill-agent "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-context "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-name "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-p "niwa-skills-core" (object))
(declare-function niwa-skill-source "niwa-skills-core" (cl-x) t)

;; `niwa-skills-input'
(declare-function niwa-skills-input-insert-fork-result
                  "niwa-skills-input" (outcome))
(autoload 'niwa-skills-input-insert-fork-result "niwa-skills-input")

;; `niwa-skills-ui'
(defvar niwa-slash-commands)

;; `niwa-structs'
(declare-function niwa-session-agent-registry
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)
(defvar niwa-session--read-only-mode)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-format
                  "niwa-tool-render-data" (render-data &optional tool-use-id))
(autoload 'niwa-tool-render-data-format "niwa-tool-render-data")

;; `niwa-turn'
(declare-function niwa-request-begin
                  "niwa-turn" (session &optional options))
(declare-function niwa-request-call-owned
                  "niwa-turn" (request buffer function &rest args))
(declare-function niwa-request-end
                  "niwa-turn" (&optional abort-plan-approval))
(declare-function niwa-request-push-canceller
                  "niwa-turn" (request canceller))
(autoload 'niwa-request-begin "niwa-turn")
(autoload 'niwa-request-call-owned "niwa-turn")
(autoload 'niwa-request-end "niwa-turn")
(autoload 'niwa-request-push-canceller "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--insert-user-turn
                  "niwa-utilities" (input))
(autoload 'niwa--insert-user-turn "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))
(autoload 'niwa-view-rerender "niwa-view")

;; `niwa-view-composer'
(declare-function niwa-view--assert-live-tip
                  "niwa-view-composer" (&optional allow-armed-fork))
(declare-function niwa-view--forward-input
                  "niwa-view-composer" (input &rest args))
(declare-function niwa-view--run-prompt-submit-hook
                  "niwa-view-composer"
                  (input display-text callback &optional blocked-callback
                         prior-context))
(declare-function niwa-view--start-fork-skill-turn
                  "niwa-view-composer"
                  (input display-text &optional hook-context
                         submitted-draft dispatch))
(declare-function niwa-view--visible-draft "niwa-view-composer" ())
(autoload 'niwa-view--assert-live-tip "niwa-view-composer")
(autoload 'niwa-view--forward-input "niwa-view-composer")
(autoload 'niwa-view--run-prompt-submit-hook "niwa-view-composer")
(autoload 'niwa-view--start-fork-skill-turn "niwa-view-composer")
(autoload 'niwa-view--visible-draft "niwa-view-composer")

;; `niwa-view-history'
(declare-function niwa-view-history-add
                  "niwa-view-history" (text))
(autoload 'niwa-view-history-add "niwa-view-history")

;; `niwa-view-stream'
(declare-function niwa-view-stream-ensure-progress
                  "niwa-view-stream" (view data-buf))
(declare-function niwa-view-stream-end-progress
                  "niwa-view-stream" (view))
(autoload 'niwa-view-stream-ensure-progress "niwa-view-stream")
(autoload 'niwa-view-stream-end-progress "niwa-view-stream")

;; `niwa-workspace'
(declare-function niwa-workspace
                  "niwa-workspace" (&optional buffer))

;;
;;; Review target prompts

(defconst niwa-review--uncommitted-prompt
  "Review the current code changes (staged, unstaged, and untracked files) and provide prioritized findings."
  "Prompt for reviewing the current working tree.")

(defconst niwa-review--base-branch-prompt
  "Review the code changes against the base branch '%s'. The merge base commit for this comparison is %s. Run `git diff %s` to inspect the changes relative to %s. Provide prioritized, actionable findings."
  "Prompt format for a base-branch review with a resolved merge base.")

(defconst niwa-review--base-branch-backup-prompt
  "Review the code changes against the base branch '%s'. Start by finding the merge diff between the current branch and %s's upstream e.g. (`git merge-base HEAD \"$(git rev-parse --abbrev-ref \"%s@{upstream}\")\"`), then run `git diff` against that SHA to see what changes we would merge into the %s branch. Provide prioritized, actionable findings."
  "Prompt format for a base-branch review without a resolved merge base.")

(defconst niwa-review--commit-prompt
  "Review the code changes introduced by commit %s. Provide prioritized, actionable findings."
  "Prompt format for a commit review without a title.")

(defconst niwa-review--commit-with-title-prompt
  "Review the code changes introduced by commit %s (\"%s\"). Provide prioritized, actionable findings."
  "Prompt format for a commit review with a title.")

(defconst niwa-review--verify-uncommitted-prompt
  "Verify the current code changes (staged, unstaged, and untracked files). Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt for verifying the current working tree.")

(defconst niwa-review--verify-base-branch-prompt
  "Verify the code changes against the base branch '%s'. The merge base commit for this comparison is %s. Run `git diff %s` to inspect the changes relative to %s. Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a base-branch verification with a resolved merge base.")

(defconst niwa-review--verify-base-branch-backup-prompt
  "Verify the code changes against the base branch '%s'. Start by finding the merge diff between the current branch and %s's upstream e.g. (`git merge-base HEAD \"$(git rev-parse --abbrev-ref \"%s@{upstream}\")\"`), then run `git diff` against that SHA to see what changes we would merge into the %s branch. Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a base-branch verification without a resolved merge base.")

(defconst niwa-review--verify-commit-prompt
  "Verify the code changes introduced by commit %s. Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a commit verification without a title.")

(defconst niwa-review--verify-commit-with-title-prompt
  "Verify the code changes introduced by commit %s (\"%s\"). Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a commit verification with a title.")

(defconst niwa-review--verify-custom-prompt
  "Verify according to these instructions:\n\n%s\n\nInspect the target adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for custom verification instructions.")

(defconst niwa-review--allowed-bash-patterns
  '("git diff:*"
    "git status:*"
    "git log:*"
    "git show:*"
    "git merge-base:*"
    "git rev-parse:*"
    "git ls-files:*"
    "git cat-file:*"
    "git --no-pager diff:*"
    "head")
  "Skill-scoped permission grants for the reviewer's git inspection.")

(defconst niwa-review--bash-deny-rule
  '("Bash" :action deny)
  "Generic Bash deny rule that constrains the reviewer to git grants.")

(defconst niwa-review--command-specs
  '((review :name "review"
            :agent "reviewer"
            :label "Review"
            :handle review
            :description "Review code changes against a target")
    (verify :name "verify"
            :agent "verifier"
            :label "Verify"
            :handle verify
            :description "Verify code changes against a target"))
  "Dispatch metadata for first-class validation commands.")

(defun niwa-review--command-spec (command)
  "Return validation command metadata for COMMAND."
  (or (cdr (assq (or command 'review) niwa-review--command-specs))
      (cdr (assq 'review niwa-review--command-specs))))

(defun niwa-review--command-name (&optional command)
  "Return the slash command name for COMMAND."
  (plist-get (niwa-review--command-spec command) :name))

(defun niwa-review--command-agent-name (&optional command)
  "Return the agent name for COMMAND."
  (plist-get (niwa-review--command-spec command) :agent))

(defun niwa-review--command-label (&optional command)
  "Return the user-facing label for COMMAND."
  (plist-get (niwa-review--command-spec command) :label))

(defun niwa-review--command-description (&optional command)
  "Return the task description for COMMAND."
  (plist-get (niwa-review--command-spec command) :description))

(defun niwa-review--command-handle (&optional command)
  "Return the progress-handle symbol for COMMAND."
  (plist-get (niwa-review--command-spec command) :handle))

(defun niwa-review--cwd ()
  "Return the review working directory for the current buffer."
  (let* ((data-buffer (niwa-review--current-data-buffer))
         (session (and (buffer-live-p data-buffer)
                       (buffer-local-value 'niwa--session data-buffer))))
    (file-name-as-directory
     (or (and session (niwa-session-working-directory session))
         default-directory))))

(defun niwa-review--current-data-buffer ()
  "Return the current niwa data buffer, or nil outside niwa."
  (when (bound-and-true-p niwa--data-buffer)
    (niwa-view--assert-live-tip))
  (cond
   ((and (boundp 'niwa--data-buffer)
         niwa--data-buffer
         (niwa-review--data-buffer-p niwa--data-buffer))
    niwa--data-buffer)
   ((niwa-review--data-buffer-p (current-buffer))
    (current-buffer))
   ((fboundp 'niwa--active-chat-buffer)
    (niwa--active-chat-buffer))))

(defun niwa-review--data-buffer-p (buffer)
  "Return non-nil when BUFFER is a niwa chat data buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'niwa--session)
              niwa--session
              (not (bound-and-true-p niwa--data-buffer))))))

(defun niwa-review--standalone-session-name (workspace cwd)
  "Return a review session name for WORKSPACE and CWD."
  (let* ((root (file-name-as-directory
                (expand-file-name (niwa-workspace-root workspace))))
         (dir (file-name-as-directory (expand-file-name cwd)))
         (relative (directory-file-name (file-relative-name dir root))))
    (if (or (equal relative "") (equal relative "."))
        "review"
      (concat "review:"
              (replace-regexp-in-string "/" ":" relative t t)))))

(defun niwa-review--ensure-standalone-data-buffer (cwd)
  "Return a safe niwa data buffer for a standalone review in CWD."
  (require 'gptel)
  (require 'niwa-workspace)
  (require 'niwa-models)
  (require 'niwa-tools)
  (require 'niwa-system)
  (require 'niwa-agents)
  (require 'niwa-presets)
  (require 'niwa-compact)
  (require 'niwa-reminders)
  (require 'niwa-chat)
  (require 'niwa-view)
  (let* ((workspace (niwa-workspace))
         (cwd (file-name-as-directory (expand-file-name cwd)))
         (buffer (or (niwa--active-chat-buffer workspace)
                     (niwa--chat-buffer
                      (niwa-review--standalone-session-name workspace cwd)
                      t workspace cwd))))
    (when (buffer-live-p buffer)
      (display-buffer (or (buffer-local-value 'niwa--view-buffer buffer)
                          buffer)
                      gptel-display-buffer-action))
    buffer))

(defun niwa-review--git-output (cwd &rest args)
  "Run hardened Git ARGS in CWD and return raw output, or nil on failure."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory cwd)
              (process-environment
               (unless (file-remote-p cwd)
                 (copy-sequence process-environment))))
          (dolist (variable '("GIT_EXTERNAL_DIFF" "GIT_PAGER" "PAGER"))
            (setenv variable nil))
          (setenv "GIT_TERMINAL_PROMPT" "0")
          (unless (and (member (car args) '("diff" "log" "show"))
                       (member "--" args))
            (let ((args
                   (append
                    '("--no-pager" "--no-replace-objects"
                      "-c" "core.fsmonitor=false")
                    (list (car args))
                    (cdr args)
                    (when (member (car args) '("diff" "log" "show"))
                      '("--no-ext-diff" "--no-textconv")))))
              (when (zerop (apply #'process-file "git" nil t nil args))
                (buffer-string))))))
    (error nil)))

(defun niwa-review--git-lines (cwd &rest args)
  "Run Git ARGS in CWD and return output lines, or nil on failure."
  (when-let* ((output (apply #'niwa-review--git-output cwd args)))
    (split-string (string-trim output) "\n" t)))

(defun niwa-review--git-string (cwd &rest args)
  "Run Git ARGS in CWD and return trimmed output, or nil on failure."
  (when-let* ((output (apply #'niwa-review--git-output cwd args))
              (output (string-trim output))
              ((not (string-empty-p output))))
    output))

(defun niwa-review--local-branches (cwd)
  "Return local branch names in CWD."
  (niwa-review--git-lines cwd "branch" "--format=%(refname:short)"))

(defun niwa-review--recent-commits (cwd)
  "Return up to 100 recent commits as plists in CWD."
  (mapcar
   (lambda (line)
     (if (string-match "\\`\\([0-9a-fA-F]+\\)\\(?:[ \t]+\\(.*\\)\\)?\\'" line)
         (list :sha (match-string 1 line)
               :title (or (match-string 2 line) ""))
       (list :sha line :title "")))
   (or (niwa-review--git-lines cwd "log" "--oneline" "-n" "100")
       nil)))

(defun niwa-review--parse-target-arg (args)
  "Return a validation target plist parsed from ARGS, or nil.
Only explicit target forms are parsed so free-form `/review' arguments keep
working as custom instructions.  Accepted forms are `current', `uncommitted',
`HEAD', `last', `branch:NAME', `base:NAME', and `commit:REV'."
  (let ((arg (string-trim (or args ""))))
    (cond
     ((member arg '("current" "uncommitted"))
      (list :type 'uncommitted))
     ((member arg '("HEAD" "last"))
      (list :type 'commit :sha "HEAD" :title ""))
     ((string-match "\\`\\(?:branch\\|base\\):\\(.+\\)\\'" arg)
      (let ((branch (string-trim (match-string 1 arg))))
        (unless (string-empty-p branch)
          (list :type 'base-branch :branch branch))))
     ((string-match "\\`commit:\\(.+\\)\\'" arg)
      (let ((sha (string-trim (match-string 1 arg))))
        (unless (string-empty-p sha)
          (list :type 'commit :sha sha :title "")))))))

(defun niwa-review--read-target (&optional cwd command)
  "Read and return a validation target plist.
CWD is the directory where git helper commands run.  COMMAND selects the
prompt label and defaults to `review'."
  (let* ((cwd (or cwd (niwa-review--cwd)))
         (prompt (if (eq command 'verify) "Verify target: " "Review target: "))
         (choice
          (completing-read
           prompt
           '("uncommitted changes" "base branch" "specific commit"
             "last commit" "custom instructions")
           nil t nil nil "uncommitted changes")))
    (pcase choice
      ("uncommitted changes" (list :type 'uncommitted))
      ("base branch"
       (let* ((branches (niwa-review--local-branches cwd))
              (branch (if branches
                          (completing-read "Base branch: " branches nil t)
                        (read-string "Base branch: "))))
         (list :type 'base-branch :branch branch)))
      ("specific commit"
       (let* ((commits (niwa-review--recent-commits cwd))
              (candidates
               (mapcar (lambda (entry)
                         (let ((sha (plist-get entry :sha))
                               (title (plist-get entry :title)))
                           (cons (if (string-empty-p title)
                                     sha
                                   (format "%s %s" sha title))
                                 entry)))
                       commits))
              (picked (if candidates
                          (cdr (assoc (completing-read
                                       "Commit: "
                                       (mapcar #'car candidates)
                                       nil t)
                                      candidates))
                        (list :sha (read-string "Commit SHA: ")
                              :title ""))))
         (list :type 'commit
               :sha (plist-get picked :sha)
               :title (plist-get picked :title))))
      ("last commit"
       (let* ((entry (car (niwa-review--recent-commits cwd)))
              (sha (or (plist-get entry :sha)
                       (read-string "Commit SHA: " "HEAD")))
              (title (or (plist-get entry :title) "")))
         (list :type 'commit :sha sha :title title)))
      ("custom instructions"
       (let ((instructions
              (string-trim
               (read-string
                (if (eq command 'verify)
                    "Verify instructions: "
                  "Review instructions: ")))))
         (when (string-empty-p instructions)
           (user-error
            (if (eq command 'verify)
                "Verify prompt cannot be empty"
              "Review prompt cannot be empty")))
         (list :type 'custom :instructions instructions))))))

(defun niwa-review--prompt-and-hint (command target &optional cwd)
  "Return (PROMPT . HINT) for validation COMMAND and TARGET.
CWD is used for git merge-base resolution.  COMMAND defaults to review."
  (let ((cwd (or cwd (niwa-review--cwd)))
        (verify-p (eq command 'verify)))
    (pcase (plist-get target :type)
      ('uncommitted
       (cons (if verify-p niwa-review--verify-uncommitted-prompt
               niwa-review--uncommitted-prompt)
             "current changes"))
      ('base-branch
       (let* ((branch (plist-get target :branch))
              (merge-base (and branch
                               (niwa-review--git-string
                                cwd "merge-base" "HEAD" branch))))
         (cons (if merge-base
                   (format (if verify-p niwa-review--verify-base-branch-prompt
                             niwa-review--base-branch-prompt)
                           branch merge-base merge-base branch)
                 (format (if verify-p niwa-review--verify-base-branch-backup-prompt
                           niwa-review--base-branch-backup-prompt)
                         branch branch branch branch))
               (format "changes against '%s'" branch))))
      ('commit
       (let ((sha (plist-get target :sha))
             (title (string-trim (or (plist-get target :title) ""))))
         (cons (if (string-empty-p title)
                   (format (if verify-p niwa-review--verify-commit-prompt
                             niwa-review--commit-prompt)
                           sha)
                 (format (if verify-p niwa-review--verify-commit-with-title-prompt
                           niwa-review--commit-with-title-prompt)
                         sha title))
               (if (string-empty-p title)
                   (format "commit %s" (substring sha 0 (min 7 (length sha))))
                 (format "commit %s: %s"
                         (substring sha 0 (min 7 (length sha)))
                         title)))))
      ('custom
       (let ((instructions (string-trim
                            (or (plist-get target :instructions) ""))))
         (when (string-empty-p instructions)
           (user-error (if verify-p "Verify prompt cannot be empty"
                         "Review prompt cannot be empty")))
         (cons (if verify-p
                   (format niwa-review--verify-custom-prompt instructions)
                 instructions)
               instructions)))
      (_ (user-error (if verify-p "Unknown verify target: %S"
                       "Unknown review target: %S")
                     target)))))


;;
;;; Review packages

(defun niwa-review--repo-root (cwd)
  "Return the Git repository root for CWD, or CWD if unavailable."
  (let ((target (niwa-execution-target-create cwd)))
    (file-name-as-directory
     (niwa-execution-target-expand-path
      target
      (or (niwa-review--git-string cwd "rev-parse" "--show-toplevel")
          cwd)
      cwd))))

(defun niwa-review--package-directory (cwd)
  "Return review package directory for CWD."
  (file-name-concat (niwa-review--repo-root cwd)
                    ".niwa" "state" "review-packages"))

(defun niwa-review--target-native-path (cwd path)
  "Return PATH in CWD's target-native path domain."
  (niwa-execution-target-native-path
   (niwa-execution-target-create cwd) path))

(defun niwa-review--insert-package-output (cwd title args &optional mode)
  "Insert TITLE and output from git ARGS in CWD.
MODE is the optional markdown fence language."
  (insert (format "## %s\n\n" title))
  (let ((output (apply #'niwa-review--git-output cwd args)))
    (if (or (null output) (string-empty-p output))
        (insert "_No output._\n\n")
      (when mode
        (insert (format "```%s\n" mode)))
      (insert output)
      (unless (string-suffix-p "\n" output)
        (insert "\n"))
      (when mode
        (insert "```\n"))
      (insert "\n"))))

(defun niwa-review--package-sections (target)
  "Return TARGET's heading and ordered (TITLE ARGS MODE) Git sections."
  (let* ((base (plist-get target :base))
         (head (or (plist-get target :head) "HEAD"))
         (range (format "%s..%s" base head)))
    (pcase (plist-get target :type)
      ('range
       (list (format "- Type: range\n- Base: %s\n- Head: %s\n\n" base head)
             (list "Commits" (list "log" "--oneline" range))
             (list "Diff Stat" (list "diff" "--stat" base head) "")
             (list "Diff" (list "diff" "--find-renames" "-U10" base head) "diff")))
      ('commit
       (let ((sha (plist-get target :sha)))
         (list (format "- Type: commit\n- Commit: %s\n\n" sha)
               (list "Commit" (list "show" "--stat" "--format=medium" "--patch"
                                    "--find-renames" "-U10" sha) "diff"))))
      ('uncommitted
       '("- Type: uncommitted changes\n\n"
         ("Status" ("status" "--short") "")
         ("Staged Diff Stat" ("diff" "--cached" "--stat") "")
         ("Staged Diff" ("diff" "--cached" "--find-renames" "-U10") "diff")
         ("Unstaged Diff Stat" ("diff" "--stat") "")
         ("Unstaged Diff" ("diff" "--find-renames" "-U10") "diff")
         ("Untracked Files" ("ls-files" "--others" "--exclude-standard") "")))
      (_ (user-error "Unsupported review package target: %S" target)))))

(iter-defun niwa-review--write-package (cwd target &optional output-file)
  "Yield between Git sections for TARGET in CWD, returning the package path.
Closing the iterator removes its unfinished generated package."
  (let* ((cwd (file-name-as-directory (expand-file-name cwd)))
         (generated-p (null output-file))
         (directory (if output-file
                        (file-name-directory output-file)
                      (niwa-review--package-directory cwd)))
         (sections (niwa-review--package-sections target))
         (buffer (generate-new-buffer " *niwa-review-package*"))
         complete)
    (unwind-protect
        (progn
          (iter-yield nil)
          (make-directory directory t)
          (when generated-p
            (setq output-file (make-nearby-temp-file
                               (file-name-concat directory "review-") nil ".md")))
          (with-current-buffer buffer
            (insert (format "# Review package: %s\n\n"
                            (or (plist-get target :type) "unknown")))
            (insert (format "- Working directory: %s\n"
                            (niwa-review--target-native-path cwd cwd)))
            (insert (format "- Generated: %s\n\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S %z")))
            (insert "## Target\n\n" (car sections)))
          (dolist (section (cdr sections))
            (iter-yield nil)
            (with-current-buffer buffer
              (apply #'niwa-review--insert-package-output cwd section)))
          (iter-yield nil)
          (with-current-buffer buffer
            (write-region (point-min) (point-max) output-file nil 'silent))
          (setq complete t)
          output-file)
      (kill-buffer buffer)
      (when (and generated-p output-file (not complete))
        (ignore-errors (delete-file output-file))))))

(defun niwa-review--target-package-spec (target cwd)
  "Return package target spec for TARGET in CWD, or nil."
  (pcase (plist-get target :type)
    ('uncommitted (list :type 'uncommitted))
    ('commit (list :type 'commit
                   :sha (plist-get target :sha)))
    ('base-branch
     (when-let* ((branch (plist-get target :branch))
                 (merge-base (niwa-review--git-string
                              cwd "merge-base" "HEAD" branch)))
       (list :type 'range :base merge-base :head "HEAD" :branch branch)))
    ('range target)
    (_ nil)))

(defun niwa-review--write-target-package (cwd target callback)
  "Prepare TARGET's package in CWD in scheduled steps, then call CALLBACK.
CALLBACK receives the target-native path, or nil on preparation failure.
Return an idempotent canceller. Source death also cancels preparation."
  (let ((source (current-buffer)) iterator timer finished stepping)
    (cl-labels
        ((cleanup ()
           (unless stepping
             (when timer (cancel-timer timer) (setq timer nil))
             (when iterator (iter-close iterator) (setq iterator nil))
             (when (buffer-live-p source)
               (with-current-buffer source (remove-hook 'kill-buffer-hook #'cancel t)))))
         (cancel () (setq finished t) (cleanup))
         (finish (path)
           (unless finished
             (setq finished t)
             (when (buffer-live-p source)
               (with-current-buffer source (funcall callback path)))))
         (step ()
           (setq timer nil stepping t)
           (unwind-protect
               (unless finished
                 (cond
                  ((not (buffer-live-p source)) (cancel))
                  ((niwa-transport-busy-p cwd)
                   (setq timer (run-at-time .1 nil #'step)))
                  (t
                   (condition-case nil
                       (progn
                         (unless iterator
                           (if-let* ((spec (niwa-review--target-package-spec target cwd)))
                               (setq iterator (niwa-review--write-package cwd spec))
                             (finish nil)))
                         (unless finished
                           (condition-case done
                               (iter-next iterator)
                             (iter-end-of-sequence
                              (finish (niwa-review--target-native-path cwd (cdr done)))))))
                     (error (finish nil)))
                   (unless finished (setq timer (run-at-time .001 nil #'step))))))
             (setq stepping nil)
             (when finished (cleanup)))))
      (add-hook 'kill-buffer-hook #'cancel nil t)
      (setq timer (run-at-time .001 nil #'step))
      #'cancel)))

(defun niwa-review--prompt-with-package (prompt package-file command)
  "Return PROMPT augmented with PACKAGE-FILE instructions for COMMAND."
  (let ((label (if (eq command 'verify)
                   "Verify package file"
                 "Review package file")))
    (format (concat "%s\n\n%s: %s\n"
                    "Read that file first. Do not rerun broad git commands "
                    "unless the package is missing information needed for a "
                    "specific finding; prefer targeted file reads or focused "
                    "git commands.")
            prompt label package-file)))


;;
;;; Output parsing and rendering

(defun niwa-review--parse-json (text)
  "Parse TEXT as JSON object, returning a plist or nil."
  (condition-case nil
      (json-parse-string text
                         :object-type 'plist
                         :array-type 'list
                         :null-object :null
                         :false-object :false)
    (error nil)))

(defun niwa-review-parse-output (text)
  "Parse reviewer TEXT into a normalized review output plist.
Falls back to storing TEXT in `:overall_explanation' when parsing or
schema validation fails."
  (cl-labels
      ((plist-shape-p
        (value)
        (and (proper-list-p value) (cl-evenp (length value))))
       (score-p
        (value)
        (and (numberp value) (<= 0 value) (<= value 1)))
       (line-range-p
        (value)
        (and (plist-shape-p value)
             (let ((start (plist-get value :start))
                   (end (plist-get value :end)))
               (and (fixnump start) (> start 0)
                    (fixnump end) (>= end start)))))
       (location-p
        (value)
        (and (plist-shape-p value)
             (stringp (plist-get value :absolute_file_path))
             (line-range-p (plist-get value :line_range))))
       (finding-p
        (value)
        (and (plist-shape-p value)
             (stringp (plist-get value :title))
             (stringp (plist-get value :body))
             (score-p (plist-get value :confidence_score))
             (or (not (plist-member value :priority))
                 (let ((priority (plist-get value :priority)))
                   (or (eq priority :null)
                       (and (fixnump priority) (<= 0 priority 3)))))
             (location-p (plist-get value :code_location))))
       (output-p
        (value)
        (and (plist-shape-p value)
             (plist-member value :findings)
             (proper-list-p (plist-get value :findings))
             (cl-every #'finding-p (plist-get value :findings))
             (member (plist-get value :overall_correctness)
                     '("patch is correct" "patch is incorrect"))
             (stringp (plist-get value :overall_explanation))
             (score-p (plist-get value :overall_confidence_score)))))
    (let ((parsed
           (and (stringp text)
                (or (niwa-review--parse-json text)
                    (let ((start (string-search "{" text))
                          (end (cl-position ?} text :from-end t)))
                      (and start end (< start end)
                           (niwa-review--parse-json
                            (substring text start (1+ end)))))))))
      (if (output-p parsed)
          parsed
        (list :findings nil
              :overall_correctness "patch is incorrect"
              :overall_explanation (or text "")
              :overall_confidence_score 0.0)))))

(defun niwa-review--finding-location (finding)
  "Return a compact location string for FINDING, or nil."
  (let* ((loc (plist-get finding :code_location))
         (range (plist-get loc :line_range))
         (path (plist-get loc :absolute_file_path))
         (start (plist-get range :start))
         (end (plist-get range :end)))
    (when path
      (if (and start end)
          (format "%s:%s-%s" path start end)
        (format "%s" path)))))

(defun niwa-review-format-findings-block (findings)
  "Return a human-readable findings block for FINDINGS."
  (let ((findings (if (vectorp findings) (append findings nil) findings))
        lines)
    (when findings
      (push (if (> (length findings) 1)
                "Full review comments:"
              "Review comment:")
            lines)
      (dolist (finding findings)
        (let* ((title (or (plist-get finding :title) "Untitled finding"))
               (body (or (plist-get finding :body) ""))
               (loc (niwa-review--finding-location finding)))
          (push "" lines)
          (push (format "- %s%s"
                        title
                        (if loc (format " -- %s" loc) ""))
                lines)
          (dolist (line (split-string body "\n"))
            (push (concat "  " line) lines))))
      (string-join (nreverse lines) "\n"))))

(defun niwa-review-render-output-text (output)
  "Render normalized review OUTPUT as user-facing text."
  (let* ((explanation (string-trim
                       (format "%s"
                               (or (plist-get output :overall_explanation)
                                   ""))))
         (findings (plist-get output :findings))
         (findings-block (niwa-review-format-findings-block findings))
         sections)
    (unless (string-empty-p explanation)
      (push explanation sections))
    (when (and findings-block (not (string-empty-p findings-block)))
      (push findings-block sections))
    (if sections
        (string-join (nreverse sections) "\n\n")
      "Reviewer failed to output a response.")))

(defun niwa-review--indent-results (text)
  "Indent review result TEXT for the `<user_action>' block."
  (mapconcat (lambda (line) (concat "  " line))
             (split-string (or text "") "\n")
             "\n"))

(defun niwa-review--xml-text-escape (text)
  "Escape TEXT for insertion as XML-ish element text."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string
     "&" "&amp;" (or text "") t t)
    t t)
   t t))

(defun niwa-review-render-user-action (output)
  "Render OUTPUT as a model-visible synthetic review user action."
  (let ((results (niwa-review--xml-text-escape
                  (niwa-review-render-output-text output))))
    (format (concat "<user_action>\n"
                    "  <context>User initiated a review task. Here's the full review output from reviewer model. User may select one or more comments to resolve.</context>\n"
                    "  <action>review</action>\n"
                    "  <results>\n"
                    "%s\n"
                    "  </results>\n"
                    "</user_action>\n")
            (niwa-review--indent-results results))))

(defun niwa-review-command-skill-p (skill)
  "Return non-nil when SKILL backs the local `/review' command."
  (and (niwa-skill-p skill)
       (equal "review" (niwa-skill-name skill))
       (eq 'fork (niwa-skill-context skill))
       (equal "reviewer" (niwa-skill-agent skill))
       (eq 'bundled (niwa-skill-source skill))))

(defun niwa-review-mark-command-outcome (outcome)
  "Return a copy of OUTCOME marked as owned by `/review' dispatch."
  (plist-put (copy-sequence outcome) :niwa-review-command t))

(defun niwa-review-transform-outcome (skill-name outcome)
  "Transform review fork OUTCOME for SKILL-NAME.
Non-review outcomes are returned unchanged.  Review outcomes get a
human-readable assistant result plus `:synthetic-user-message' carrying
the `<user_action>' block for parent-history continuity."
  (if (and (plist-get outcome :niwa-review-command)
           (equal skill-name "review")
           (eq (plist-get outcome :status) 'ok)
           (eq (plist-get outcome :kind) 'fork)
           (not (plist-member outcome :review-output)))
      (let* ((raw (or (plist-get outcome :result) ""))
             (output (niwa-review-parse-output raw))
             (assistant (niwa-review-render-output-text output))
             (user-action (niwa-review-render-user-action output))
             (copy (copy-sequence outcome)))
        (setq copy (plist-put copy :raw-review-result raw))
        (setq copy (plist-put copy :review-output output))
        (setq copy (plist-put copy :result assistant))
        (setq copy (plist-put copy :synthetic-user-message user-action))
        copy)
    outcome))

(defun niwa-review-strip-user-action-blocks (text)
  "Return TEXT without synthetic review `<user_action>' blocks."
  (let ((case-fold-search (default-value 'case-fold-search)))
    (if (not (string-match-p "<user_action>" text))
        (string-trim text)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward "<user_action>" nil t)
          (let ((start (match-beginning 0)))
            (if (search-forward "</user_action>" nil t)
                (let ((end (point))
                      (block (buffer-substring-no-properties start (point))))
                  (when (string-match-p "<action>review</action>" block)
                    (when (and (< end (point-max))
                               (eq (char-after end) ?\n))
                      (cl-incf end))
                    (delete-region start end)
                    (goto-char start)))
              (goto-char (point-max)))))
        (string-trim (buffer-string))))))


;;
;;; Dispatch

(defun niwa-review--next-task-name (session command)
  "Return the next unreserved task name for COMMAND in SESSION."
  (let* ((base (niwa-review--command-name command))
         (name base)
         (index 2)
         (registry (niwa-session-agent-registry session)))
    (while (assoc (concat "/root/" name) registry)
      (setq name (format "%s_%d" base index))
      (cl-incf index))
    name))

(defun niwa-review--result-outcome (result)
  "Return a fork-style workflow outcome for canonical RESULT.
The child's token `:usage' is retained when RESULT reports it."
  (let* ((payload (or (plist-get result :payload) "Agent returned no result."))
         (path (plist-get result :sender))
         (outcome
          (pcase (plist-get result :outcome)
            ('completed
             (list :status 'ok :kind 'fork :result payload :agent-path path))
            ('interrupted
             (list :status 'error :reason 'agent-interrupted
                   :message payload :agent-path path))
            ('errored
             (list :status 'error :reason 'agent-errored
                   :message payload :agent-path path))
            (_
             (list :status 'error :reason 'invalid-agent-result
                   :message "Agent returned an invalid terminal result"
                   :agent-path path)))))
    (if-let* ((usage (plist-get result :usage)))
        (append outcome (list :usage usage))
      outcome)))

(defun niwa-review--git-allow-rules ()
  "Return validation skill-scoped git inspection allow rules."
  (mapcar (lambda (pattern)
            (list "Bash" :pattern pattern :action 'allow))
          niwa-review--allowed-bash-patterns))

(defun niwa-review--permission-rules ()
  "Return reviewer's skill-scoped permission rules, or nil."
  (when-let* ((rules (niwa-review--git-allow-rules)))
    (append rules (list niwa-review--bash-deny-rule))))

(defun niwa-review--verify-permission-rules ()
  "Return verifier git inspection grants, or nil.
Unlike reviewer rules, these do not deny other Bash commands; normal
permission policy decides whether verifier validation commands may run."
  (niwa-review--git-allow-rules))

(defun niwa-review--ensure-dispatch-deps (&optional command)
  "Load modules needed when a validation COMMAND is autoloaded directly.
Loading the agents module registers the bundled agents."
  (require 'niwa-agents)
  (unless (niwa-agent-get (if (eq command 'verify) "verifier" "reviewer"))
    (user-error "The %s agent is not available"
                (if (eq command 'verify) "verifier" "reviewer"))))

(defun niwa-review--ensure-dispatch-allowed (data-buffer)
  "Signal if DATA-BUFFER cannot accept a direct review dispatch."
  (with-current-buffer data-buffer
    (when (bound-and-true-p niwa--current-request)
      (user-error "A request is already active -- wait or abort first"))
    (when (bound-and-true-p niwa-compact-run-in-flight)
      (user-error "Compaction in progress"))
    (when (bound-and-true-p niwa-session--read-only-mode)
      (user-error "Session is open read-only (another host holds the lock)"))))

(defun niwa-review--record-direct-turn (display data-buffer &optional dispatch)
  "Record direct no-view review DISPLAY in DATA-BUFFER, then call DISPATCH.
Dispatch only while the admitted request still owns DATA-BUFFER."
  (with-current-buffer data-buffer
    (let (request)
      (when niwa--session
        (niwa-session-artifacts-assert-new-mutation-authority
         niwa--session)
        (setq request
              (niwa-request-begin
               niwa--session)))
      (goto-char (point-max))
      (niwa--insert-user-turn display)
      (when dispatch
        (niwa-request-call-owned request data-buffer dispatch)))))

(defun niwa-review--end-direct-request (data-buffer)
  "End DATA-BUFFER's direct review request if one is active."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (bound-and-true-p niwa--current-request)
        (niwa-request-end)))))

(defun niwa-review--progress-render-data (invocation hint &optional command)
  "Return render data for INVOCATION, HINT, and COMMAND."
  (let* ((agent (and (niwa-agent-invocation-p invocation)
                     (niwa-agent-invocation-agent invocation)))
         (agent-id (and (niwa-agent-invocation-p invocation)
                        (niwa-agent-invocation-agent-id invocation)))
         (path (and (niwa-agent-invocation-p invocation)
                    (niwa-agent-invocation-path invocation)))
         (rel (and (niwa-agent-invocation-p invocation)
                   (niwa-agent-invocation-transcript-relative-path
                    invocation)))
         (status (or (and (niwa-agent-invocation-p invocation)
                          (niwa-agent-invocation-transcript-status
                           invocation))
                     'running))
         (calls (and (niwa-agent-invocation-p invocation)
                     (niwa-agent-invocation-call-count invocation)))
         (description
          (or (and (niwa-agent-invocation-p invocation)
                   (niwa-agent-invocation-description invocation))
              hint
              (niwa-review--command-description command))))
    (append
     (list :kind 'collaboration-event
           :event 'started
           :path path
           :agent-id agent-id
           :role (or (and agent (niwa-agent-name agent))
                     (niwa-review--command-agent-name command))
           :name (niwa-review--command-label command)
           :description description
           :progress-handle (niwa-review--command-handle command)
           :default-expanded t
           :status status
           :calls (or calls 0)
           :body "")
     (when rel
       (list :transcript-relative-path rel)))))

(defun niwa-review--insert-progress-handle (invocation hint &optional command)
  "Insert hidden progress handle for INVOCATION, HINT, and COMMAND."
  (when-let* (((niwa-agent-invocation-p invocation))
              (path (niwa-agent-invocation-path invocation)))
    (let* ((render-data
            (niwa-review--progress-render-data invocation hint command))
           (block (niwa-tool-render-data-format render-data)))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert block)
      (when-let* ((view-buffer (and (boundp 'niwa--view-buffer)
                                    niwa--view-buffer))
                  ((buffer-live-p view-buffer)))
        (let ((data-buffer (current-buffer)))
          (niwa-view-rerender view-buffer)
          (niwa-view-stream-ensure-progress view-buffer data-buffer))))))

(defun niwa-review--verify-outcome (outcome invocation &optional report)
  "Validate verifier OUTCOME and record its verdict on INVOCATION.
REPORT is the complete settled report; the verdict is read from it because
OUTCOME's `:result' is a bounded preview that can omit the final line."
  (if (not (and (eq (plist-get outcome :status) 'ok)
                (eq (plist-get outcome :kind) 'fork)
                (stringp (plist-get outcome :result))))
      outcome
    (let* ((preview (plist-get outcome :result))
           (report (if (stringp report) report preview))
           (lines (split-string report "\n"))
           (nonblank (cl-remove-if #'string-blank-p lines))
           (verdict-lines
            (cl-remove-if-not
             (lambda (line)
               (string-match-p "\\`VERDICT: \\(PASS\\|FAIL\\|PARTIAL\\)\\'"
                               line))
             lines))
           (final (car (last nonblank))))
      (if (and (= (length verdict-lines) 1)
               (equal final (car verdict-lines)))
          (let ((verdict
                 (intern (downcase (substring final (length "VERDICT: "))))))
            (when (niwa-agent-invocation-p invocation)
              (setf (niwa-agent-invocation-verdict invocation) verdict)
              (when (buffer-live-p
                     (niwa-agent-invocation-parent-data-buffer invocation))
                (niwa-agent-conversation-refresh invocation)))
            (plist-put outcome :verdict verdict))
        (when (niwa-agent-invocation-p invocation)
          (setf (niwa-agent-invocation-verdict invocation) nil))
        (setq outcome (plist-put outcome :verification-rejected t))
        (plist-put
         outcome :result
         (concat
          "Verification report rejected: expected exactly one final "
          "VERDICT: PASS, VERDICT: FAIL, or VERDICT: PARTIAL line.\n\n"
          "Original report:\n\n" preview))))))

(defun niwa-review--run-task
    (prompt hint callback &optional submit-context progress-callback command
            cwd target workload)
  "Run and await the dedicated validation leaf for PROMPT and HINT.
CALLBACK receives the normalized fork-style outcome. SUBMIT-CONTEXT is appended
when non-empty. PROGRESS-CALLBACK receives the invocation before dispatch.
COMMAND defaults to `review'. CWD and TARGET schedule package preparation only
after the parent has accepted the review turn; cancellation covers preparation.
WORKLOAD overrides the role's model policy when non-nil."
  (let* ((command (or command 'review))
         (session niwa--session)
         (request niwa--current-request)
         (data-buffer (current-buffer)))
    (if (null session)
        (funcall callback '(:status error :reason no-session
				    :message "Validation requires an active session"))
      (let (path invocation preparation-cancel cancelled-p settled-p)
        (cl-labels
            ((finish (result)
               (unless settled-p
		 (setq settled-p t)
		 (unless cancelled-p
                   (niwa-request-call-owned
                    request data-buffer
                    (lambda ()
                      (let ((outcome (niwa-review--result-outcome result)))
			(funcall callback
				 (if (eq command 'verify)
                                     (niwa-review--verify-outcome
                                      outcome invocation
                                      (plist-get
                                       (niwa-agent-control-settled-result
                                        (alist-get (plist-get result :sender)
                                                   (niwa-session-agent-registry session)
                                                   nil nil #'equal))
                                       :payload))
                                   outcome))))))))
             (cancel ()
               (unless (or settled-p cancelled-p)
		 (setq cancelled-p t)
		 (if path
                     (niwa-agent-control-interrupt session path)
                   (when preparation-cancel (funcall preparation-cancel)))))
             (prepared (outcome)
               (pcase (plist-get outcome :outcome)
		 ('success
		  (setq path (niwa-agent-record-path (plist-get outcome :record))))
		 ((or 'error 'aborted)
		  (unless cancelled-p
                    (finish (list :type 'RESULT :outcome 'errored
				  :payload (or (plist-get outcome :error)
                                               "Agent preparation was cancelled")))))))
             (dispatch (package-file)
               (unless cancelled-p
		 (niwa-request-call-owned
		  request data-buffer
		  (lambda ()
                    (let* ((prompt (if package-file
                                       (niwa-review--prompt-with-package prompt package-file command)
                                     prompt))
                           (message (if (and (stringp submit-context) (not (string-empty-p submit-context)))
					(concat prompt "\n\n" submit-context)
                                      prompt)))
                      (condition-case err
			  (progn
                            (setq preparation-cancel
				  (niwa-agent-control-spawn
                                   session (niwa-review--next-task-name session command)
                                   message #'prepared
                                   :agent (niwa-agent-resolve-role (niwa-review--command-agent-name command))
                                   :context "none"
                                   :model-policy
                                   (and workload
                                        (niwa-model-resolve-workload workload))
                                   :description (or hint (niwa-review--command-description command))
                                   :skill-permission-rules
                                   (if (eq command 'verify) (niwa-review--verify-permission-rules)
                                     (niwa-review--permission-rules))
                                   :on-invocation
                                   (lambda (value)
                                     (setq invocation value path (niwa-agent-invocation-path value))
                                     (when progress-callback (funcall progress-callback value)))
                                   :result-handler #'finish))
                            (when (and cancelled-p preparation-cancel) (funcall preparation-cancel)))
			(error
			 (unless settled-p
                           (setq settled-p t)
                           (funcall callback (list :status 'error :reason 'agent-dispatch-failed
                                                   :message (error-message-string err))))))))))))
          (if (and cwd target)
              (setq preparation-cancel (niwa-review--write-target-package cwd target #'dispatch))
            (dispatch nil))
          (unless settled-p
            (niwa-request-push-canceller request #'cancel)))))))

(defun niwa-review-verify (prompt callback &optional workload)
  "Run the verifier for PROMPT from the current root request.
CALLBACK receives one outcome plist.  A verified report has `:verdict' `pass',
`fail' or `partial' with its bounded `:result'; a report without exactly one
final VERDICT line has `:verification-rejected'.  Failures carry `:status'
`error' with `:reason' and `:message'.  Any outcome may carry the verifier's
token `:usage'.  Cancelling the request interrupts the verifier and drops its
outcome.  WORKLOAD overrides the verifier's model policy when non-nil."
  (niwa-review--ensure-dispatch-deps 'verify)
  (niwa-review--run-task prompt "Verify Goal completion" callback
                            nil nil 'verify nil nil workload))

(defun niwa-review--transform-command-outcome (outcome &optional command)
  "Transform validation OUTCOME for COMMAND before parent insertion."
  (if (eq (or command 'review) 'review)
      (niwa-review-transform-outcome
       "review" (niwa-review-mark-command-outcome outcome))
    outcome))

(defun niwa-review--handle-direct-outcome (outcome data-buffer &optional command)
  "Handle OUTCOME for COMMAND direct dispatch targeting DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (niwa-skills-input-insert-fork-result
             (niwa-review--transform-command-outcome outcome command))))
         (_
          (niwa-review--end-direct-request data-buffer)
          (niwa-diagnostic-message (niwa-diagnostic-scope data-buffer)
                   "%s returned unsupported outcome: %S"
                   (niwa-review--command-name command) outcome))))
      (_
       (niwa-review--end-direct-request data-buffer)
       (niwa-diagnostic-message (niwa-diagnostic-scope data-buffer)
                "%s (%s failed)"
                (or (plist-get outcome :message)
                    "unknown error")
                (niwa-review--command-name command))))))

(defun niwa-review--handle-view-outcome
    (outcome view-buffer data-buffer &optional command)
  "Handle OUTCOME for COMMAND dispatch from VIEW-BUFFER to DATA-BUFFER."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (niwa-skills-input-insert-fork-result
             (niwa-review--transform-command-outcome outcome command))))
         (_
          (niwa-review--end-direct-request data-buffer)
          (niwa-view-stream-end-progress view-buffer)
          (niwa-diagnostic-message (niwa-diagnostic-scope data-buffer)
                   "%s returned unsupported outcome: %S"
                   (niwa-review--command-label command) outcome))))
      (_
       (niwa-view-stream-end-progress view-buffer)
       (niwa-diagnostic-message (niwa-diagnostic-scope data-buffer)
                "%s (%s failed)"
                (or (plist-get outcome :message)
                    "unknown error")
                (niwa-review--command-label command))
       (niwa-review--end-direct-request data-buffer)
       (with-current-buffer data-buffer
         (gptel--update-status " Ready" 'success))))))

(defun niwa-review--send-from-view
    (display prompt hint view-buffer data-buffer &optional command cwd target)
  "Run COMMAND task for DISPLAY, PROMPT, and HINT from VIEW-BUFFER.
DATA-BUFFER receives the task transcript."
  (with-current-buffer view-buffer
    ;; Captured before the hook runs: it may be asynchronous, and the
    ;; composer must keep whatever the user types meanwhile.
    (let ((submitted-draft (niwa-view--visible-draft)))
      (niwa-view--run-prompt-submit-hook
       display display
       (lambda (submission)
         (when (and (buffer-live-p view-buffer)
                    (buffer-live-p data-buffer))
           (let ((hook-input (niwa-prompt-submission-input submission))
                 (hook-context
                  (niwa-prompt-submission-context submission)))
             (if (not (equal hook-input display))
                 (let ((model-input (if hook-context
                                        (concat hook-input "\n\n" hook-context)
                                      hook-input)))
                   (niwa-view--forward-input
                    model-input
                    :display-text hook-input
                    :before-send
                    (lambda ()
                      (niwa-view-history-add hook-input))
                    :prompt-checked t
                    :submission submission
                    :submitted-draft submitted-draft))
               (niwa-view-history-add display)
               (niwa-view--start-fork-skill-turn
                (if hook-context
                    (concat display "\n\n" hook-context)
                  display)
                display hook-context submitted-draft
                (lambda (request)
                  (niwa-prompt-submission-commit submission)
                  (niwa-request-call-owned
                   request data-buffer
                   (lambda ()
                     (niwa-review--run-task
                      prompt hint
                      (lambda (outcome)
			(niwa-review--handle-view-outcome
                         outcome view-buffer data-buffer command))
                      hook-context
                      (lambda (invocation)
			(niwa-review--insert-progress-handle
                         invocation hint command))
                      command cwd target)))))))))))))

(defun niwa-review--dispatch (prompt hint &optional cwd command target)
  "Dispatch COMMAND with PROMPT, HINT, CWD, and optional package TARGET."
  (let ((command (or command 'review)))
    (niwa-review--ensure-dispatch-deps command)
    (let* ((data-buffer (or (niwa-review--current-data-buffer)
                            (niwa-review--ensure-standalone-data-buffer
                             (or cwd default-directory))))
         (view-buffer (and (buffer-live-p data-buffer)
                           (buffer-local-value 'niwa--view-buffer
                                               data-buffer)
                           (buffer-live-p
                            (buffer-local-value 'niwa--view-buffer
                                                data-buffer))
                           (buffer-local-value 'niwa--view-buffer
                                               data-buffer)))
           (command-name (niwa-review--command-name command))
           (display (format "/%s %s" command-name hint)))
      (unless (buffer-live-p data-buffer)
        (user-error "No niwa chat buffer available for %s output"
                    command-name))
      (niwa-review--ensure-dispatch-allowed data-buffer)
      (if view-buffer
          (progn
            (niwa-review--send-from-view
             display prompt hint view-buffer data-buffer command cwd target)
            'niwa-view-sent)
        (message "niwa: running %s for %s" command-name hint)
        (niwa-review--record-direct-turn
         display data-buffer
         (lambda ()
           (niwa-review--run-task
            prompt hint
            (lambda (outcome)
              (niwa-review--handle-direct-outcome outcome data-buffer command))
            nil
            (lambda (invocation)
              (niwa-review--insert-progress-handle
               invocation hint command))
            command cwd target)))))))

(defun niwa-review--target-from-instructions
    (instructions cwd command)
  "Return a target for INSTRUCTIONS, CWD, and validation COMMAND."
  (if (and instructions (not (string-blank-p instructions)))
      (or (niwa-review--parse-target-arg instructions)
          (list :type 'custom :instructions instructions))
    (niwa-review--read-target cwd command)))

(defun niwa-review--run-command (&optional instructions command)
  "Run validation COMMAND using optional target INSTRUCTIONS."
  (let* ((command (or command 'review))
         (cwd (niwa-review--cwd))
         (target (niwa-review--target-from-instructions
                  instructions cwd command))
         (prompt+hint (niwa-review--prompt-and-hint command target cwd)))
    (niwa-review--dispatch
     (car prompt+hint) (cdr prompt+hint) cwd command target)))

;;;###autoload
(defun niwa-review (&optional instructions)
  "Pick a review target and run the reviewer.
When INSTRUCTIONS is non-empty, parse explicit target forms or run a custom
review with that prompt instead of opening the target picker."
  (interactive)
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope (current-buffer))
    (niwa-review--run-command instructions 'review)))

;;;###autoload
(defun niwa-verify (&optional instructions)
  "Pick a verification target and run the verifier.
When INSTRUCTIONS is non-empty, parse explicit target forms or run a custom
verification with that prompt instead of opening the target picker."
  (interactive)
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope (current-buffer))
    (niwa-review--run-command instructions 'verify)))

(defun niwa-cmd--review (args)
  "Run `/review' with optional custom ARGS."
  (niwa-review args))

(defun niwa-cmd--verify (args)
  "Run `/verify' with optional target or custom ARGS."
  (niwa-verify args))

(defun niwa-review-install-slash-command ()
  "Install `/review' and `/verify' into `niwa-slash-commands'."
  (setf (alist-get "review" niwa-slash-commands nil nil #'equal)
        #'niwa-cmd--review)
  (setf (alist-get "verify" niwa-slash-commands nil nil #'equal)
        #'niwa-cmd--verify))

(niwa-review-install-slash-command)

(provide 'niwa-review)

;;; niwa-review.el ends here
