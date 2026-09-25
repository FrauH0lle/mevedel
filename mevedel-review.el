;;; mevedel-review.el -- Codex-style code review command -*- lexical-binding: t -*-

;;; Commentary:

;; Implements the `/review' and `/verify' workflows: choose a shared
;; validation target, await a dedicated asynchronous reviewer or verifier
;; leaf task, and route the result back into the parent transcript.  Review
;; output is parsed from structured JSON and mirrored into a synthetic
;; action so follow-up prompts can refer to findings by number.

;;; Code:

(require 'cl-lib)
(require 'generator)

(require 'subr-x)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-transport)

;; `gptel'
(declare-function gptel--update-status
                  "ext:gptel" (status &optional face))
(defvar gptel-display-buffer-action)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-interrupt
                  "mevedel-agent-control" (session target))
(declare-function mevedel-agent-control-settled-result
                  "mevedel-agent-control" (record))
(declare-function mevedel-agent-control-spawn
                  "mevedel-agent-control" t t)
(declare-function mevedel-agent-record-path
                  "mevedel-agent-control" (cl-x) t)
(autoload 'mevedel-agent-control-interrupt "mevedel-agent-control")
(autoload 'mevedel-agent-control-settled-result "mevedel-agent-control")
(autoload 'mevedel-agent-control-spawn "mevedel-agent-control")
(autoload 'mevedel-agent-record-path "mevedel-agent-control")

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-refresh
                  "mevedel-agent-conversation" (invocation))
(autoload 'mevedel-agent-conversation-refresh "mevedel-agent-conversation")

;; `mevedel-agents'
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (object))
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict
                  "mevedel-agents" (cl-x) t)
(declare-function \(setf\ mevedel-agent-invocation-verdict\)
                  "mevedel-agents" (value cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-resolve-role "mevedel-agents" (role))

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer
                  "mevedel-chat" (&optional workspace))
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))

;; `mevedel-compact-run'
(defvar mevedel-compact-run-in-flight)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-commit
                  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-context
                  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-input
                  "mevedel-prompt-submission" (cl-x) t)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(autoload 'mevedel-session-artifacts-assert-new-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-skills-core'
(declare-function mevedel-skill-agent "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-context "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-p "mevedel-skills-core" (object))
(declare-function mevedel-skill-source "mevedel-skills-core" (cl-x) t)

;; `mevedel-skills-input'
(declare-function mevedel-skills-input-insert-fork-result
                  "mevedel-skills-input" (outcome))
(autoload 'mevedel-skills-input-insert-fork-result "mevedel-skills-input")

;; `mevedel-skills-ui'
(defvar mevedel-slash-commands)

;; `mevedel-structs'
(declare-function mevedel-session-agent-registry
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel-session--read-only-mode)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-format
                  "mevedel-tool-render-data" (render-data &optional tool-use-id))
(autoload 'mevedel-tool-render-data-format "mevedel-tool-render-data")

;; `mevedel-turn'
(declare-function mevedel-request-begin
                  "mevedel-turn" (session &optional directive-uuid))
(declare-function mevedel-request-call-owned
                  "mevedel-turn" (request buffer function &rest args))
(declare-function mevedel-request-end
                  "mevedel-turn" (&optional abort-plan-approval))
(declare-function mevedel-request-push-canceller
                  "mevedel-turn" (request canceller))
(autoload 'mevedel-request-begin "mevedel-turn")
(autoload 'mevedel-request-call-owned "mevedel-turn")
(autoload 'mevedel-request-end "mevedel-turn")
(autoload 'mevedel-request-push-canceller "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--insert-user-turn
                  "mevedel-utilities" (input))
(autoload 'mevedel--insert-user-turn "mevedel-utilities")

;; `mevedel-view'
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))

;; `mevedel-view-composer'
(declare-function mevedel-view--assert-live-tip
                  "mevedel-view-composer" (&optional allow-armed-fork))
(declare-function mevedel-view--forward-input
                  "mevedel-view-composer" (input &rest args))
(declare-function mevedel-view--run-prompt-submit-hook
                  "mevedel-view-composer"
                  (input display-text callback &optional blocked-callback
                         prior-context))
(declare-function mevedel-view--start-fork-skill-turn
                  "mevedel-view-composer"
                  (input display-text &optional hook-context
                         submitted-draft dispatch))
(declare-function mevedel-view--visible-draft "mevedel-view-composer" ())
(autoload 'mevedel-view--assert-live-tip "mevedel-view-composer")

;; `mevedel-view-history'
(declare-function mevedel-view-history-add
                  "mevedel-view-history" (text))

;; `mevedel-view-stream'
(declare-function mevedel-view--ensure-request-progress
                  "mevedel-view-stream" (&optional data-buf status))
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace
                  "mevedel-workspace" (&optional buffer))

;;
;;; Review target prompts

(defconst mevedel-review--uncommitted-prompt
  "Review the current code changes (staged, unstaged, and untracked files) and provide prioritized findings."
  "Prompt for reviewing the current working tree.")

(defconst mevedel-review--base-branch-prompt
  "Review the code changes against the base branch '%s'. The merge base commit for this comparison is %s. Run `git diff %s` to inspect the changes relative to %s. Provide prioritized, actionable findings."
  "Prompt format for a base-branch review with a resolved merge base.")

(defconst mevedel-review--base-branch-backup-prompt
  "Review the code changes against the base branch '%s'. Start by finding the merge diff between the current branch and %s's upstream e.g. (`git merge-base HEAD \"$(git rev-parse --abbrev-ref \"%s@{upstream}\")\"`), then run `git diff` against that SHA to see what changes we would merge into the %s branch. Provide prioritized, actionable findings."
  "Prompt format for a base-branch review without a resolved merge base.")

(defconst mevedel-review--commit-prompt
  "Review the code changes introduced by commit %s. Provide prioritized, actionable findings."
  "Prompt format for a commit review without a title.")

(defconst mevedel-review--commit-with-title-prompt
  "Review the code changes introduced by commit %s (\"%s\"). Provide prioritized, actionable findings."
  "Prompt format for a commit review with a title.")

(defconst mevedel-review--verify-uncommitted-prompt
  "Verify the current code changes (staged, unstaged, and untracked files). Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt for verifying the current working tree.")

(defconst mevedel-review--verify-base-branch-prompt
  "Verify the code changes against the base branch '%s'. The merge base commit for this comparison is %s. Run `git diff %s` to inspect the changes relative to %s. Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a base-branch verification with a resolved merge base.")

(defconst mevedel-review--verify-base-branch-backup-prompt
  "Verify the code changes against the base branch '%s'. Start by finding the merge diff between the current branch and %s's upstream e.g. (`git merge-base HEAD \"$(git rev-parse --abbrev-ref \"%s@{upstream}\")\"`), then run `git diff` against that SHA to see what changes we would merge into the %s branch. Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a base-branch verification without a resolved merge base.")

(defconst mevedel-review--verify-commit-prompt
  "Verify the code changes introduced by commit %s. Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a commit verification without a title.")

(defconst mevedel-review--verify-commit-with-title-prompt
  "Verify the code changes introduced by commit %s (\"%s\"). Inspect the changes adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for a commit verification with a title.")

(defconst mevedel-review--verify-custom-prompt
  "Verify according to these instructions:\n\n%s\n\nInspect the target adversarially, run or recommend relevant checks when allowed, and finish with a final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line."
  "Prompt format for custom verification instructions.")

(defconst mevedel-review--allowed-bash-patterns
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

(defconst mevedel-review--bash-deny-rule
  '("Bash" :action deny)
  "Generic Bash deny rule that constrains the reviewer to git grants.")

(defconst mevedel-review--command-specs
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

(defun mevedel-review--command-spec (command)
  "Return validation command metadata for COMMAND."
  (or (cdr (assq (or command 'review) mevedel-review--command-specs))
      (cdr (assq 'review mevedel-review--command-specs))))

(defun mevedel-review--command-name (&optional command)
  "Return the slash command name for COMMAND."
  (plist-get (mevedel-review--command-spec command) :name))

(defun mevedel-review--command-agent-name (&optional command)
  "Return the agent name for COMMAND."
  (plist-get (mevedel-review--command-spec command) :agent))

(defun mevedel-review--command-label (&optional command)
  "Return the user-facing label for COMMAND."
  (plist-get (mevedel-review--command-spec command) :label))

(defun mevedel-review--command-description (&optional command)
  "Return the task description for COMMAND."
  (plist-get (mevedel-review--command-spec command) :description))

(defun mevedel-review--command-handle (&optional command)
  "Return the progress-handle symbol for COMMAND."
  (plist-get (mevedel-review--command-spec command) :handle))

(defun mevedel-review--cwd ()
  "Return the review working directory for the current buffer."
  (let* ((data-buffer (mevedel-review--current-data-buffer))
         (session (and (buffer-live-p data-buffer)
                       (buffer-local-value 'mevedel--session data-buffer))))
    (file-name-as-directory
     (or (and session (mevedel-session-working-directory session))
         default-directory))))

(defun mevedel-review--current-data-buffer ()
  "Return the current mevedel data buffer, or nil outside mevedel."
  (when (bound-and-true-p mevedel--data-buffer)
    (mevedel-view--assert-live-tip))
  (cond
   ((and (boundp 'mevedel--data-buffer)
         mevedel--data-buffer
         (mevedel-review--data-buffer-p mevedel--data-buffer))
    mevedel--data-buffer)
   ((mevedel-review--data-buffer-p (current-buffer))
    (current-buffer))
   ((fboundp 'mevedel--active-chat-buffer)
    (mevedel--active-chat-buffer))))

(defun mevedel-review--data-buffer-p (buffer)
  "Return non-nil when BUFFER is a mevedel chat data buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'mevedel--session)
              mevedel--session
              (not (bound-and-true-p mevedel--data-buffer))))))

(defun mevedel-review--standalone-session-name (workspace cwd)
  "Return a review session name for WORKSPACE and CWD."
  (let* ((root (file-name-as-directory
                (expand-file-name (mevedel-workspace-root workspace))))
         (dir (file-name-as-directory (expand-file-name cwd)))
         (relative (directory-file-name (file-relative-name dir root))))
    (if (or (equal relative "") (equal relative "."))
        "review"
      (concat "review:"
              (replace-regexp-in-string "/" ":" relative t t)))))

(defun mevedel-review--ensure-standalone-data-buffer (cwd)
  "Return a safe mevedel data buffer for a standalone review in CWD."
  (require 'gptel)
  (require 'mevedel-workspace)
  (require 'mevedel-models)
  (require 'mevedel-tools)
  (require 'mevedel-system)
  (require 'mevedel-agents)
  (require 'mevedel-presets)
  (require 'mevedel-compact)
  (require 'mevedel-reminders)
  (require 'mevedel-chat)
  (require 'mevedel-view)
  (let* ((workspace (mevedel-workspace))
         (cwd (file-name-as-directory (expand-file-name cwd)))
         (buffer (or (mevedel--active-chat-buffer workspace)
                     (mevedel--chat-buffer
                      (mevedel-review--standalone-session-name workspace cwd)
                      t workspace cwd))))
    (when (buffer-live-p buffer)
      (display-buffer (or (buffer-local-value 'mevedel--view-buffer buffer)
                          buffer)
                      gptel-display-buffer-action))
    buffer))

(defun mevedel-review--git-output (cwd &rest args)
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

(defun mevedel-review--git-lines (cwd &rest args)
  "Run Git ARGS in CWD and return output lines, or nil on failure."
  (when-let* ((output (apply #'mevedel-review--git-output cwd args)))
    (split-string (string-trim output) "\n" t)))

(defun mevedel-review--git-string (cwd &rest args)
  "Run Git ARGS in CWD and return trimmed output, or nil on failure."
  (when-let* ((output (apply #'mevedel-review--git-output cwd args))
              (output (string-trim output))
              ((not (string-empty-p output))))
    output))

(defun mevedel-review--local-branches (cwd)
  "Return local branch names in CWD."
  (mevedel-review--git-lines cwd "branch" "--format=%(refname:short)"))

(defun mevedel-review--recent-commits (cwd)
  "Return up to 100 recent commits as plists in CWD."
  (mapcar
   (lambda (line)
     (if (string-match "\\`\\([0-9a-fA-F]+\\)\\(?:[ \t]+\\(.*\\)\\)?\\'" line)
         (list :sha (match-string 1 line)
               :title (or (match-string 2 line) ""))
       (list :sha line :title "")))
   (or (mevedel-review--git-lines cwd "log" "--oneline" "-n" "100")
       nil)))

(defun mevedel-review--parse-target-arg (args)
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

(defun mevedel-review--read-target (&optional cwd command)
  "Read and return a validation target plist.
CWD is the directory where git helper commands run.  COMMAND selects the
prompt label and defaults to `review'."
  (let* ((cwd (or cwd (mevedel-review--cwd)))
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
       (let* ((branches (mevedel-review--local-branches cwd))
              (branch (if branches
                          (completing-read "Base branch: " branches nil t)
                        (read-string "Base branch: "))))
         (list :type 'base-branch :branch branch)))
      ("specific commit"
       (let* ((commits (mevedel-review--recent-commits cwd))
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
       (let* ((entry (car (mevedel-review--recent-commits cwd)))
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

(defun mevedel-review--prompt-and-hint (command target &optional cwd)
  "Return (PROMPT . HINT) for validation COMMAND and TARGET.
CWD is used for git merge-base resolution.  COMMAND defaults to review."
  (let ((cwd (or cwd (mevedel-review--cwd)))
        (verify-p (eq command 'verify)))
    (pcase (plist-get target :type)
      ('uncommitted
       (cons (if verify-p mevedel-review--verify-uncommitted-prompt
               mevedel-review--uncommitted-prompt)
             "current changes"))
      ('base-branch
       (let* ((branch (plist-get target :branch))
              (merge-base (and branch
                               (mevedel-review--git-string
                                cwd "merge-base" "HEAD" branch))))
         (cons (if merge-base
                   (format (if verify-p mevedel-review--verify-base-branch-prompt
                             mevedel-review--base-branch-prompt)
                           branch merge-base merge-base branch)
                 (format (if verify-p mevedel-review--verify-base-branch-backup-prompt
                           mevedel-review--base-branch-backup-prompt)
                         branch branch branch branch))
               (format "changes against '%s'" branch))))
      ('commit
       (let ((sha (plist-get target :sha))
             (title (string-trim (or (plist-get target :title) ""))))
         (cons (if (string-empty-p title)
                   (format (if verify-p mevedel-review--verify-commit-prompt
                             mevedel-review--commit-prompt)
                           sha)
                 (format (if verify-p mevedel-review--verify-commit-with-title-prompt
                           mevedel-review--commit-with-title-prompt)
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
                   (format mevedel-review--verify-custom-prompt instructions)
                 instructions)
               instructions)))
      (_ (user-error (if verify-p "Unknown verify target: %S"
                       "Unknown review target: %S")
                     target)))))


;;
;;; Review packages

(defun mevedel-review--repo-root (cwd)
  "Return the Git repository root for CWD, or CWD if unavailable."
  (let ((target (mevedel-execution-target-create cwd)))
    (file-name-as-directory
     (mevedel-execution-target-expand-path
      target
      (or (mevedel-review--git-string cwd "rev-parse" "--show-toplevel")
          cwd)
      cwd))))

(defun mevedel-review--package-directory (cwd)
  "Return review package directory for CWD."
  (file-name-concat (mevedel-review--repo-root cwd)
                    ".mevedel" "state" "review-packages"))

(defun mevedel-review--target-native-path (cwd path)
  "Return PATH in CWD's target-native path domain."
  (mevedel-execution-target-native-path
   (mevedel-execution-target-create cwd) path))

(defun mevedel-review--insert-package-output (cwd title args &optional mode)
  "Insert TITLE and output from git ARGS in CWD.
MODE is the optional markdown fence language."
  (insert (format "## %s\n\n" title))
  (let ((output (apply #'mevedel-review--git-output cwd args)))
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

(defun mevedel-review--package-sections (target)
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

(iter-defun mevedel-review--write-package (cwd target &optional output-file)
  "Yield between Git sections for TARGET in CWD, returning the package path.
Closing the iterator removes its unfinished generated package."
  (let* ((cwd (file-name-as-directory (expand-file-name cwd)))
         (generated-p (null output-file))
         (directory (if output-file
                        (file-name-directory output-file)
                      (mevedel-review--package-directory cwd)))
         (sections (mevedel-review--package-sections target))
         (buffer (generate-new-buffer " *mevedel-review-package*"))
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
                            (mevedel-review--target-native-path cwd cwd)))
            (insert (format "- Generated: %s\n\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S %z")))
            (insert "## Target\n\n" (car sections)))
          (dolist (section (cdr sections))
            (iter-yield nil)
            (with-current-buffer buffer
              (apply #'mevedel-review--insert-package-output cwd section)))
          (iter-yield nil)
          (with-current-buffer buffer
            (write-region (point-min) (point-max) output-file nil 'silent))
          (setq complete t)
          output-file)
      (kill-buffer buffer)
      (when (and generated-p output-file (not complete))
        (ignore-errors (delete-file output-file))))))

(defun mevedel-review--target-package-spec (target cwd)
  "Return package target spec for TARGET in CWD, or nil."
  (pcase (plist-get target :type)
    ('uncommitted (list :type 'uncommitted))
    ('commit (list :type 'commit
                   :sha (plist-get target :sha)))
    ('base-branch
     (when-let* ((branch (plist-get target :branch))
                 (merge-base (mevedel-review--git-string
                              cwd "merge-base" "HEAD" branch)))
       (list :type 'range :base merge-base :head "HEAD" :branch branch)))
    ('range target)
    (_ nil)))

(defun mevedel-review--write-target-package (cwd target callback)
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
                  ((mevedel-transport-busy-p cwd)
                   (setq timer (run-at-time .1 nil #'step)))
                  (t
                   (condition-case nil
                       (progn
                         (unless iterator
                           (if-let* ((spec (mevedel-review--target-package-spec target cwd)))
                               (setq iterator (mevedel-review--write-package cwd spec))
                             (finish nil)))
                         (unless finished
                           (condition-case done
                               (iter-next iterator)
                             (iter-end-of-sequence
                              (finish (mevedel-review--target-native-path cwd (cdr done)))))))
                     (error (finish nil)))
                   (unless finished (setq timer (run-at-time .001 nil #'step))))))
             (setq stepping nil)
             (when finished (cleanup)))))
      (add-hook 'kill-buffer-hook #'cancel nil t)
      (setq timer (run-at-time .001 nil #'step))
      #'cancel)))

(defun mevedel-review--prompt-with-package (prompt package-file command)
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

(defun mevedel-review--parse-json (text)
  "Parse TEXT as JSON object, returning a plist or nil."
  (condition-case nil
      (json-parse-string text
                         :object-type 'plist
                         :array-type 'list
                         :null-object :null
                         :false-object :false)
    (error nil)))

(defun mevedel-review-parse-output (text)
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
                (or (mevedel-review--parse-json text)
                    (let ((start (string-search "{" text))
                          (end (cl-position ?} text :from-end t)))
                      (and start end (< start end)
                           (mevedel-review--parse-json
                            (substring text start (1+ end)))))))))
      (if (output-p parsed)
          parsed
        (list :findings nil
              :overall_correctness "patch is incorrect"
              :overall_explanation (or text "")
              :overall_confidence_score 0.0)))))

(defun mevedel-review--finding-location (finding)
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

(defun mevedel-review-format-findings-block (findings)
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
               (loc (mevedel-review--finding-location finding)))
          (push "" lines)
          (push (format "- %s%s"
                        title
                        (if loc (format " -- %s" loc) ""))
                lines)
          (dolist (line (split-string body "\n"))
            (push (concat "  " line) lines))))
      (string-join (nreverse lines) "\n"))))

(defun mevedel-review-render-output-text (output)
  "Render normalized review OUTPUT as user-facing text."
  (let* ((explanation (string-trim
                       (format "%s"
                               (or (plist-get output :overall_explanation)
                                   ""))))
         (findings (plist-get output :findings))
         (findings-block (mevedel-review-format-findings-block findings))
         sections)
    (unless (string-empty-p explanation)
      (push explanation sections))
    (when (and findings-block (not (string-empty-p findings-block)))
      (push findings-block sections))
    (if sections
        (string-join (nreverse sections) "\n\n")
      "Reviewer failed to output a response.")))

(defun mevedel-review--indent-results (text)
  "Indent review result TEXT for the `<user_action>' block."
  (mapconcat (lambda (line) (concat "  " line))
             (split-string (or text "") "\n")
             "\n"))

(defun mevedel-review--xml-text-escape (text)
  "Escape TEXT for insertion as XML-ish element text."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string
     "&" "&amp;" (or text "") t t)
    t t)
   t t))

(defun mevedel-review-render-user-action (output)
  "Render OUTPUT as a model-visible synthetic review user action."
  (let ((results (mevedel-review--xml-text-escape
                  (mevedel-review-render-output-text output))))
    (format (concat "<user_action>\n"
                    "  <context>User initiated a review task. Here's the full review output from reviewer model. User may select one or more comments to resolve.</context>\n"
                    "  <action>review</action>\n"
                    "  <results>\n"
                    "%s\n"
                    "  </results>\n"
                    "</user_action>\n")
            (mevedel-review--indent-results results))))

(defun mevedel-review-command-skill-p (skill)
  "Return non-nil when SKILL backs the local `/review' command."
  (and (mevedel-skill-p skill)
       (equal "review" (mevedel-skill-name skill))
       (eq 'fork (mevedel-skill-context skill))
       (equal "reviewer" (mevedel-skill-agent skill))
       (eq 'bundled (mevedel-skill-source skill))))

(defun mevedel-review-mark-command-outcome (outcome)
  "Return a copy of OUTCOME marked as owned by `/review' dispatch."
  (plist-put (copy-sequence outcome) :mevedel-review-command t))

(defun mevedel-review-transform-outcome (skill-name outcome)
  "Transform review fork OUTCOME for SKILL-NAME.
Non-review outcomes are returned unchanged.  Review outcomes get a
human-readable assistant result plus `:synthetic-user-message' carrying
the `<user_action>' block for parent-history continuity."
  (if (and (plist-get outcome :mevedel-review-command)
           (equal skill-name "review")
           (eq (plist-get outcome :status) 'ok)
           (eq (plist-get outcome :kind) 'fork)
           (not (plist-member outcome :review-output)))
      (let* ((raw (or (plist-get outcome :result) ""))
             (output (mevedel-review-parse-output raw))
             (assistant (mevedel-review-render-output-text output))
             (user-action (mevedel-review-render-user-action output))
             (copy (copy-sequence outcome)))
        (setq copy (plist-put copy :raw-review-result raw))
        (setq copy (plist-put copy :review-output output))
        (setq copy (plist-put copy :result assistant))
        (setq copy (plist-put copy :synthetic-user-message user-action))
        copy)
    outcome))

(defun mevedel-review-strip-user-action-blocks (text)
  "Return TEXT without synthetic review `<user_action>' blocks."
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
    (string-trim (buffer-string))))


;;
;;; Dispatch

(defun mevedel-review--next-task-name (session command)
  "Return the next unreserved task name for COMMAND in SESSION."
  (let* ((base (mevedel-review--command-name command))
         (name base)
         (index 2)
         (registry (mevedel-session-agent-registry session)))
    (while (assoc (concat "/root/" name) registry)
      (setq name (format "%s_%d" base index))
      (cl-incf index))
    name))

(defun mevedel-review--result-outcome (result)
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

(defun mevedel-review--git-allow-rules ()
  "Return validation skill-scoped git inspection allow rules."
  (mapcar (lambda (pattern)
            (list "Bash" :pattern pattern :action 'allow))
          mevedel-review--allowed-bash-patterns))

(defun mevedel-review--permission-rules ()
  "Return reviewer's skill-scoped permission rules, or nil."
  (when-let* ((rules (mevedel-review--git-allow-rules)))
    (append rules (list mevedel-review--bash-deny-rule))))

(defun mevedel-review--verify-permission-rules ()
  "Return verifier git inspection grants, or nil.
Unlike reviewer rules, these do not deny other Bash commands; normal
permission policy decides whether verifier validation commands may run."
  (mevedel-review--git-allow-rules))

(defun mevedel-review--ensure-dispatch-deps (&optional command)
  "Load modules needed when a validation COMMAND is autoloaded directly.
Loading the agents module registers the bundled agents."
  (require 'mevedel-agents)
  (unless (mevedel-agent-get (if (eq command 'verify) "verifier" "reviewer"))
    (user-error "The %s agent is not available"
                (if (eq command 'verify) "verifier" "reviewer"))))

(defun mevedel-review--ensure-dispatch-allowed (data-buffer)
  "Signal if DATA-BUFFER cannot accept a direct review dispatch."
  (with-current-buffer data-buffer
    (when (bound-and-true-p mevedel--current-request)
      (user-error "A request is already active -- wait or abort first"))
    (when (bound-and-true-p mevedel-compact-run-in-flight)
      (user-error "Compaction in progress"))
    (when (bound-and-true-p mevedel-session--read-only-mode)
      (user-error "Session is open read-only (another host holds the lock)"))))

(defun mevedel-review--record-direct-turn (display data-buffer &optional dispatch)
  "Record direct no-view review DISPLAY in DATA-BUFFER, then call DISPATCH.
Dispatch only while the admitted request still owns DATA-BUFFER."
  (with-current-buffer data-buffer
    (let (request)
      (when mevedel--session
        (mevedel-session-artifacts-assert-new-mutation-authority
         mevedel--session)
        (setq request
              (mevedel-request-begin
               mevedel--session
               (and (boundp 'mevedel--current-directive-uuid)
                    mevedel--current-directive-uuid))))
      (goto-char (point-max))
      (mevedel--insert-user-turn display)
      (when dispatch
        (mevedel-request-call-owned request data-buffer dispatch)))))

(defun mevedel-review--end-direct-request (data-buffer)
  "End DATA-BUFFER's direct review request if one is active."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (bound-and-true-p mevedel--current-request)
        (mevedel-request-end)))))

(defun mevedel-review--progress-render-data (invocation hint &optional command)
  "Return render data for INVOCATION, HINT, and COMMAND."
  (let* ((agent (and (mevedel-agent-invocation-p invocation)
                     (mevedel-agent-invocation-agent invocation)))
         (agent-id (and (mevedel-agent-invocation-p invocation)
                        (mevedel-agent-invocation-agent-id invocation)))
         (path (and (mevedel-agent-invocation-p invocation)
                    (mevedel-agent-invocation-path invocation)))
         (rel (and (mevedel-agent-invocation-p invocation)
                   (mevedel-agent-invocation-transcript-relative-path
                    invocation)))
         (status (or (and (mevedel-agent-invocation-p invocation)
                          (mevedel-agent-invocation-transcript-status
                           invocation))
                     'running))
         (calls (and (mevedel-agent-invocation-p invocation)
                     (mevedel-agent-invocation-call-count invocation)))
         (description
          (or (and (mevedel-agent-invocation-p invocation)
                   (mevedel-agent-invocation-description invocation))
              hint
              (mevedel-review--command-description command))))
    (append
     (list :kind 'collaboration-event
           :event 'started
           :path path
           :agent-id agent-id
           :role (or (and agent (mevedel-agent-name agent))
                     (mevedel-review--command-agent-name command))
           :name (mevedel-review--command-label command)
           :description description
           :progress-handle (mevedel-review--command-handle command)
           :default-expanded t
           :status status
           :calls (or calls 0)
           :body "")
     (when rel
       (list :transcript-relative-path rel)))))

(defun mevedel-review--insert-progress-handle (invocation hint &optional command)
  "Insert hidden progress handle for INVOCATION, HINT, and COMMAND."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (path (mevedel-agent-invocation-path invocation)))
    (let* ((render-data
            (mevedel-review--progress-render-data invocation hint command))
           (block (mevedel-tool-render-data-format render-data)))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert block)
      (when-let* ((view-buffer (and (boundp 'mevedel--view-buffer)
                                    mevedel--view-buffer))
                  ((buffer-live-p view-buffer)))
        (let ((data-buffer (current-buffer)))
          (mevedel-view-rerender view-buffer)
          (with-current-buffer view-buffer
            (mevedel-view--ensure-request-progress data-buffer)))))))

(defun mevedel-review--verify-outcome (outcome invocation &optional report)
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
            (when (mevedel-agent-invocation-p invocation)
              (setf (mevedel-agent-invocation-verdict invocation) verdict)
              (when (buffer-live-p
                     (mevedel-agent-invocation-parent-data-buffer invocation))
                (mevedel-agent-conversation-refresh invocation)))
            (plist-put outcome :verdict verdict))
        (when (mevedel-agent-invocation-p invocation)
          (setf (mevedel-agent-invocation-verdict invocation) nil))
        (setq outcome (plist-put outcome :verification-rejected t))
        (plist-put
         outcome :result
         (concat
          "Verification report rejected: expected exactly one final "
          "VERDICT: PASS, VERDICT: FAIL, or VERDICT: PARTIAL line.\n\n"
          "Original report:\n\n" preview))))))

(defun mevedel-review--run-task
    (prompt hint callback &optional submit-context progress-callback command
            cwd target)
  "Run and await the dedicated validation leaf for PROMPT and HINT.
CALLBACK receives the normalized fork-style outcome. SUBMIT-CONTEXT is appended
when non-empty. PROGRESS-CALLBACK receives the invocation before dispatch.
COMMAND defaults to `review'. CWD and TARGET schedule package preparation only
after the parent has accepted the review turn; cancellation covers preparation."
  (let* ((command (or command 'review))
         (session mevedel--session)
         (request mevedel--current-request)
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
                   (mevedel-request-call-owned
                    request data-buffer
                    (lambda ()
                      (let ((outcome (mevedel-review--result-outcome result)))
			(funcall callback
				 (if (eq command 'verify)
                                     (mevedel-review--verify-outcome
                                      outcome invocation
                                      (plist-get
                                       (mevedel-agent-control-settled-result
                                        (alist-get (plist-get result :sender)
                                                   (mevedel-session-agent-registry session)
                                                   nil nil #'equal))
                                       :payload))
                                   outcome))))))))
             (cancel ()
               (unless (or settled-p cancelled-p)
		 (setq cancelled-p t)
		 (if path
                     (mevedel-agent-control-interrupt session path)
                   (when preparation-cancel (funcall preparation-cancel)))))
             (prepared (outcome)
               (pcase (plist-get outcome :outcome)
		 ('success
		  (setq path (mevedel-agent-record-path (plist-get outcome :record))))
		 ((or 'error 'aborted)
		  (unless cancelled-p
                    (finish (list :type 'RESULT :outcome 'errored
				  :payload (or (plist-get outcome :error)
                                               "Agent preparation was cancelled")))))))
             (dispatch (package-file)
               (unless cancelled-p
		 (mevedel-request-call-owned
		  request data-buffer
		  (lambda ()
                    (let* ((prompt (if package-file
                                       (mevedel-review--prompt-with-package prompt package-file command)
                                     prompt))
                           (message (if (and (stringp submit-context) (not (string-empty-p submit-context)))
					(concat prompt "\n\n" submit-context)
                                      prompt)))
                      (condition-case err
			  (progn
                            (setq preparation-cancel
				  (mevedel-agent-control-spawn
                                   session (mevedel-review--next-task-name session command)
                                   message #'prepared
                                   :agent (mevedel-agent-resolve-role (mevedel-review--command-agent-name command))
                                   :context "none"
                                   :description (or hint (mevedel-review--command-description command))
                                   :skill-permission-rules
                                   (if (eq command 'verify) (mevedel-review--verify-permission-rules)
                                     (mevedel-review--permission-rules))
                                   :on-invocation
                                   (lambda (value)
                                     (setq invocation value path (mevedel-agent-invocation-path value))
                                     (when progress-callback (funcall progress-callback value)))
                                   :result-handler #'finish))
                            (when (and cancelled-p preparation-cancel) (funcall preparation-cancel)))
			(error
			 (unless settled-p
                           (setq settled-p t)
                           (funcall callback (list :status 'error :reason 'agent-dispatch-failed
                                                   :message (error-message-string err))))))))))))
          (if (and cwd target)
              (setq preparation-cancel (mevedel-review--write-target-package cwd target #'dispatch))
            (dispatch nil))
          (unless settled-p
            (mevedel-request-push-canceller request #'cancel)))))))

(defun mevedel-review-verify (prompt callback)
  "Run the verifier for PROMPT from the current root request.
CALLBACK receives one outcome plist.  A verified report has `:verdict' `pass',
`fail' or `partial' with its bounded `:result'; a report without exactly one
final VERDICT line has `:verification-rejected'.  Failures carry `:status'
`error' with `:reason' and `:message'.  Any outcome may carry the verifier's
token `:usage'.  Cancelling the request interrupts the verifier and drops its
outcome."
  (mevedel-review--ensure-dispatch-deps 'verify)
  (mevedel-review--run-task prompt "Verify Goal completion" callback
                            nil nil 'verify))

(defun mevedel-review--transform-command-outcome (outcome &optional command)
  "Transform validation OUTCOME for COMMAND before parent insertion."
  (if (eq (or command 'review) 'review)
      (mevedel-review-transform-outcome
       "review" (mevedel-review-mark-command-outcome outcome))
    outcome))

(defun mevedel-review--handle-direct-outcome (outcome data-buffer &optional command)
  "Handle OUTCOME for COMMAND direct dispatch targeting DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (mevedel-skills-input-insert-fork-result
             (mevedel-review--transform-command-outcome outcome command))))
         (_
          (mevedel-review--end-direct-request data-buffer)
          (message "mevedel: %s returned unsupported outcome: %S"
                   (mevedel-review--command-name command) outcome))))
      (_
       (mevedel-review--end-direct-request data-buffer)
       (message "mevedel: %s failed: %s"
                (mevedel-review--command-name command)
                (or (plist-get outcome :message)
                    "unknown error"))))))

(defun mevedel-review--handle-view-outcome
    (outcome view-buffer data-buffer &optional command)
  "Handle OUTCOME for COMMAND dispatch from VIEW-BUFFER to DATA-BUFFER."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (mevedel-skills-input-insert-fork-result
             (mevedel-review--transform-command-outcome outcome command))))
         (_
          (mevedel-review--end-direct-request data-buffer)
          (with-current-buffer view-buffer
            (mevedel-view--stop-request-progress))
          (message "%s returned unsupported outcome: %S"
                   (mevedel-review--command-label command) outcome))))
      (_
       (with-current-buffer view-buffer
         (mevedel-view--stop-request-progress)
         (message "%s failed: %s"
                  (mevedel-review--command-label command)
                  (or (plist-get outcome :message)
                      "unknown error")))
       (mevedel-review--end-direct-request data-buffer)
       (with-current-buffer data-buffer
         (gptel--update-status " Ready" 'success))))))

(defun mevedel-review--send-from-view
    (display prompt hint view-buffer data-buffer &optional command cwd target)
  "Run COMMAND task for DISPLAY, PROMPT, and HINT from VIEW-BUFFER.
DATA-BUFFER receives the task transcript."
  (with-current-buffer view-buffer
    ;; Captured before the hook runs: it may be asynchronous, and the
    ;; composer must keep whatever the user types meanwhile.
    (let ((submitted-draft (mevedel-view--visible-draft)))
      (mevedel-view--run-prompt-submit-hook
       display display
       (lambda (submission)
         (when (and (buffer-live-p view-buffer)
                    (buffer-live-p data-buffer))
           (let ((hook-input (mevedel-prompt-submission-input submission))
                 (hook-context
                  (mevedel-prompt-submission-context submission)))
             (if (not (equal hook-input display))
                 (let ((model-input (if hook-context
                                        (concat hook-input "\n\n" hook-context)
                                      hook-input)))
                   (mevedel-view--forward-input
                    model-input
                    :display-text hook-input
                    :before-send
                    (lambda ()
                      (mevedel-view-history-add hook-input))
                    :prompt-checked t
                    :submission submission
                    :submitted-draft submitted-draft))
               (mevedel-view-history-add display)
               (mevedel-view--start-fork-skill-turn
                (if hook-context
                    (concat display "\n\n" hook-context)
                  display)
                display hook-context submitted-draft
                (lambda (request)
                  (mevedel-prompt-submission-commit submission)
                  (mevedel-request-call-owned
                   request data-buffer
                   (lambda ()
                     (mevedel-review--run-task
                      prompt hint
                      (lambda (outcome)
			(mevedel-review--handle-view-outcome
                         outcome view-buffer data-buffer command))
                      hook-context
                      (lambda (invocation)
			(mevedel-review--insert-progress-handle
                         invocation hint command))
                      command cwd target)))))))))))))

(defun mevedel-review--dispatch (prompt hint &optional cwd command target)
  "Dispatch COMMAND with PROMPT, HINT, CWD, and optional package TARGET."
  (let ((command (or command 'review)))
    (mevedel-review--ensure-dispatch-deps command)
    (let* ((data-buffer (or (mevedel-review--current-data-buffer)
                            (mevedel-review--ensure-standalone-data-buffer
                             (or cwd default-directory))))
         (view-buffer (and (buffer-live-p data-buffer)
                           (buffer-local-value 'mevedel--view-buffer
                                               data-buffer)
                           (buffer-live-p
                            (buffer-local-value 'mevedel--view-buffer
                                                data-buffer))
                           (buffer-local-value 'mevedel--view-buffer
                                               data-buffer)))
           (command-name (mevedel-review--command-name command))
           (display (format "/%s %s" command-name hint)))
      (unless (buffer-live-p data-buffer)
        (user-error "No mevedel chat buffer available for %s output"
                    command-name))
      (mevedel-review--ensure-dispatch-allowed data-buffer)
      (if view-buffer
          (progn
            (mevedel-review--send-from-view
             display prompt hint view-buffer data-buffer command cwd target)
            'mevedel-view-sent)
        (message "mevedel: running %s for %s" command-name hint)
        (mevedel-review--record-direct-turn
         display data-buffer
         (lambda ()
           (mevedel-review--run-task
            prompt hint
            (lambda (outcome)
              (mevedel-review--handle-direct-outcome outcome data-buffer command))
            nil
            (lambda (invocation)
              (mevedel-review--insert-progress-handle
               invocation hint command))
            command cwd target)))))))

(defun mevedel-review--target-from-instructions
    (instructions cwd command)
  "Return a target for INSTRUCTIONS, CWD, and validation COMMAND."
  (if (and instructions (not (string-blank-p instructions)))
      (or (mevedel-review--parse-target-arg instructions)
          (list :type 'custom :instructions instructions))
    (mevedel-review--read-target cwd command)))

(defun mevedel-review--run-command (&optional instructions command)
  "Run validation COMMAND using optional target INSTRUCTIONS."
  (let* ((command (or command 'review))
         (cwd (mevedel-review--cwd))
         (target (mevedel-review--target-from-instructions
                  instructions cwd command))
         (prompt+hint (mevedel-review--prompt-and-hint command target cwd)))
    (mevedel-review--dispatch
     (car prompt+hint) (cdr prompt+hint) cwd command target)))

;;;###autoload
(defun mevedel-review (&optional instructions)
  "Pick a review target and run the reviewer.
When INSTRUCTIONS is non-empty, parse explicit target forms or run a custom
review with that prompt instead of opening the target picker."
  (interactive)
  (mevedel-review--run-command instructions 'review))

;;;###autoload
(defun mevedel-verify (&optional instructions)
  "Pick a verification target and run the verifier.
When INSTRUCTIONS is non-empty, parse explicit target forms or run a custom
verification with that prompt instead of opening the target picker."
  (interactive)
  (mevedel-review--run-command instructions 'verify))

(defun mevedel-cmd--review (args)
  "Run `/review' with optional custom ARGS."
  (mevedel-review args))

(defun mevedel-cmd--verify (args)
  "Run `/verify' with optional target or custom ARGS."
  (mevedel-verify args))

(defun mevedel-review-install-slash-command ()
  "Install `/review' and `/verify' into `mevedel-slash-commands'."
  (setf (alist-get "review" mevedel-slash-commands nil nil #'equal)
        #'mevedel-cmd--review)
  (setf (alist-get "verify" mevedel-slash-commands nil nil #'equal)
        #'mevedel-cmd--verify))

(mevedel-review-install-slash-command)

(provide 'mevedel-review)

;;; mevedel-review.el ends here
