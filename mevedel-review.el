;;; mevedel-review.el -- Codex-style code review command -*- lexical-binding: t -*-

;;; Commentary:

;; Implements the `/review' and `/verify' workflows: choose a shared
;; validation target, run a dedicated foreground reviewer or verifier
;; task, and route the result back into the parent transcript.  Review
;; output is parsed from structured JSON and mirrored into a synthetic
;; action so follow-up prompts can refer to findings by number.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)
(require 'subr-x)
(require 'mevedel-skills)
(require 'mevedel-structs)

;; `gptel'
(defvar gptel-display-buffer-action)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)
(declare-function gptel--update-status "gptel" (status &optional face))

;; `mevedel-skills'
(defvar mevedel-skills--bundled-dir)
(defvar mevedel-slash-commands)
(declare-function mevedel-skills--build-skill
                  "mevedel-skills" (skill-file source))
(declare-function mevedel-skills--insert-fork-result "mevedel-skills" (outcome))
(declare-function mevedel-skills-invoke "mevedel-skills" t t)
(declare-function copy-mevedel-skill "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-allowed-tool-rules "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-allowed-tools "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-agent "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-context "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-name "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-p "mevedel-skills" (object))
(declare-function mevedel-skill-source "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill--create "mevedel-skills" (&rest slots))

;; `mevedel-permissions'
(declare-function mevedel-permission--parse-rule-strings
                  "mevedel-permissions" (entries))

;; `mevedel-tool-exec'
(declare-function mevedel-tool-exec--register "mevedel-tool-exec" ())

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get "mevedel-tool-registry" (name))

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
                  "mevedel-utilities" (start end))

;; `mevedel-structs'
(defvar mevedel--data-buffer)
(defvar mevedel--current-request)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--compaction-in-flight)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel-session--read-only-mode)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())

;; `mevedel-agents'
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (object))
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))
(declare-function mevedel-agents-ensure-reviewer "mevedel-agents" ())
(declare-function mevedel-agents-ensure-verifier "mevedel-agents" ())

;; `mevedel-agent-exec'
(defvar mevedel-agent-exec--agents)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--format-render-data-block
                  "mevedel-pipeline" (render-data))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer "mevedel-chat" (&optional workspace))
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))

;; `mevedel-view'
(declare-function mevedel-view--fork-if-pending "mevedel-view" ())
(declare-function mevedel-view--forward-input
                  "mevedel-view"
                  (input &optional display-text before-send prompt-checked
                         on-block hook-context))
(declare-function mevedel-view--run-prompt-submit-hook
                  "mevedel-view"
                  (input display-text callback &optional blocked-callback))
(declare-function mevedel-view--start-fork-skill-turn
                  "mevedel-view"
                  (input display-text &optional hook-context))
(declare-function mevedel-view--ensure-request-progress
                  "mevedel-view" (&optional data-buf status))
(declare-function mevedel-view--stop-request-progress "mevedel-view" ())
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))
(declare-function mevedel-view-history-add "mevedel-view-history" (text))


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

(defconst mevedel-review--allowed-tool-entries
  '("Bash(git diff:*)"
    "Bash(git status:*)"
    "Bash(git log:*)"
    "Bash(git show:*)"
    "Bash(git merge-base:*)"
    "Bash(git rev-parse:*)"
    "Bash(git ls-files:*)"
    "Bash(git cat-file:*)"
    "Bash(GIT_PAGER=cat git diff:*)"
    "Bash(head:*)")
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

(defun mevedel-review--git-lines (cwd &rest args)
  "Run git ARGS in CWD and return output lines, or nil on failure."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory cwd))
          (when (zerop (apply #'process-file "git" nil t nil args))
            (split-string (string-trim (buffer-string)) "\n" t))))
    (error nil)))

(defun mevedel-review--git-string (cwd &rest args)
  "Run git ARGS in CWD and return trimmed output, or nil on failure."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory cwd))
          (when (zerop (apply #'process-file "git" nil t nil args))
            (let ((out (string-trim (buffer-string))))
              (unless (string-empty-p out) out)))))
    (error nil)))

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

(defun mevedel-review--target-prompt-and-hint (target &optional cwd)
  "Return (PROMPT . HINT) for review TARGET.
CWD is used for git merge-base resolution."
  (let ((cwd (or cwd (mevedel-review--cwd))))
    (pcase (plist-get target :type)
      ('uncommitted
       (cons mevedel-review--uncommitted-prompt "current changes"))
      ('base-branch
       (let* ((branch (plist-get target :branch))
              (merge-base (and branch
                               (mevedel-review--git-string
                                cwd "merge-base" "HEAD" branch))))
         (cons (if merge-base
                   (format mevedel-review--base-branch-prompt
                           branch merge-base merge-base branch)
                 (format mevedel-review--base-branch-backup-prompt
                         branch branch branch branch))
               (format "changes against '%s'" branch))))
      ('commit
       (let ((sha (plist-get target :sha))
             (title (string-trim (or (plist-get target :title) ""))))
         (cons (if (string-empty-p title)
                   (format mevedel-review--commit-prompt sha)
                 (format mevedel-review--commit-with-title-prompt sha title))
               (if (string-empty-p title)
                   (format "commit %s" (substring sha 0 (min 7 (length sha))))
                 (format "commit %s: %s"
                         (substring sha 0 (min 7 (length sha)))
                         title)))))
      ('custom
       (let ((instructions (string-trim
                            (or (plist-get target :instructions) ""))))
         (when (string-empty-p instructions)
           (user-error "Review prompt cannot be empty"))
         (cons instructions instructions)))
      (_ (user-error "Unknown review target: %S" target)))))

(defun mevedel-review--verify-target-prompt-and-hint (target &optional cwd)
  "Return (PROMPT . HINT) for verify TARGET.
CWD is used for git merge-base resolution."
  (let ((cwd (or cwd (mevedel-review--cwd))))
    (pcase (plist-get target :type)
      ('uncommitted
       (cons mevedel-review--verify-uncommitted-prompt "current changes"))
      ('base-branch
       (let* ((branch (plist-get target :branch))
              (merge-base (and branch
                               (mevedel-review--git-string
                                cwd "merge-base" "HEAD" branch))))
         (cons (if merge-base
                   (format mevedel-review--verify-base-branch-prompt
                           branch merge-base merge-base branch)
                 (format mevedel-review--verify-base-branch-backup-prompt
                         branch branch branch branch))
               (format "changes against '%s'" branch))))
      ('commit
       (let ((sha (plist-get target :sha))
             (title (string-trim (or (plist-get target :title) ""))))
         (cons (if (string-empty-p title)
                   (format mevedel-review--verify-commit-prompt sha)
                 (format mevedel-review--verify-commit-with-title-prompt
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
           (user-error "Verify prompt cannot be empty"))
         (cons (format mevedel-review--verify-custom-prompt instructions)
               instructions)))
      (_ (user-error "Unknown verify target: %S" target)))))

(defun mevedel-review--prompt-and-hint (command target &optional cwd)
  "Return (PROMPT . HINT) for COMMAND and TARGET."
  (if (eq command 'verify)
      (mevedel-review--verify-target-prompt-and-hint target cwd)
    (mevedel-review--target-prompt-and-hint target cwd)))


;;
;;; Review packages

(defun mevedel-review--git-output (cwd &rest args)
  "Run git ARGS in CWD and return raw output, or nil on failure."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory cwd))
          (when (zerop (apply #'process-file "git" nil t nil args))
            (buffer-string))))
    (error nil)))

(defun mevedel-review--repo-root (cwd)
  "Return the Git repository root for CWD, or CWD if unavailable."
  (file-name-as-directory
   (expand-file-name
    (or (mevedel-review--git-string cwd "rev-parse" "--show-toplevel")
        cwd))))

(defun mevedel-review--package-directory (cwd)
  "Return review package directory for CWD."
  (file-name-concat (mevedel-review--repo-root cwd)
                    ".mevedel" "review-packages"))

(defun mevedel-review--default-package-file (cwd target)
  "Return a deterministic-ish review package path for TARGET in CWD."
  (let* ((digest (substring (secure-hash 'sha1 (prin1-to-string target))
                            0 8))
         (stamp (format-time-string "%Y%m%d-%H%M%S")))
    (file-name-concat
     (mevedel-review--package-directory cwd)
     (format "%s-%s.md" stamp digest))))

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

(defun mevedel-review--write-range-package (cwd target)
  "Insert review package sections for range TARGET in CWD."
  (let* ((base (plist-get target :base))
         (head (or (plist-get target :head) "HEAD"))
         (range (format "%s..%s" base head)))
    (insert "## Target\n\n")
    (insert (format "- Type: range\n- Base: %s\n- Head: %s\n\n"
                    base head))
    (mevedel-review--insert-package-output
     cwd "Commits" (list "log" "--oneline" range))
    (mevedel-review--insert-package-output
     cwd "Diff Stat" (list "diff" "--stat" base head) "")
    (mevedel-review--insert-package-output
     cwd "Diff" (list "diff" "--find-renames" "-U10" base head) "diff")))

(defun mevedel-review--write-commit-package (cwd target)
  "Insert review package sections for commit TARGET in CWD."
  (let ((sha (plist-get target :sha)))
    (insert "## Target\n\n")
    (insert (format "- Type: commit\n- Commit: %s\n\n" sha))
    (mevedel-review--insert-package-output
     cwd "Commit" (list "show" "--stat" "--format=medium"
                        "--patch" "--find-renames" "-U10" sha)
     "diff")))

(defun mevedel-review--write-uncommitted-package (cwd)
  "Insert review package sections for uncommitted changes in CWD."
  (insert "## Target\n\n")
  (insert "- Type: uncommitted changes\n\n")
  (mevedel-review--insert-package-output
   cwd "Status" (list "status" "--short") "")
  (mevedel-review--insert-package-output
   cwd "Staged Diff Stat" (list "diff" "--cached" "--stat") "")
  (mevedel-review--insert-package-output
   cwd "Staged Diff" (list "diff" "--cached" "--find-renames" "-U10") "diff")
  (mevedel-review--insert-package-output
   cwd "Unstaged Diff Stat" (list "diff" "--stat") "")
  (mevedel-review--insert-package-output
   cwd "Unstaged Diff" (list "diff" "--find-renames" "-U10") "diff")
  (mevedel-review--insert-package-output
   cwd "Untracked Files" (list "ls-files" "--others" "--exclude-standard") ""))

(defun mevedel-review--write-package (cwd target &optional output-file)
  "Write a review package for TARGET in CWD and return its path."
  (let* ((cwd (file-name-as-directory (expand-file-name cwd)))
         (output-file (or output-file
                          (mevedel-review--default-package-file cwd target))))
    (make-directory (file-name-directory output-file) t)
    (with-temp-file output-file
      (insert (format "# Review package: %s\n\n"
                      (or (plist-get target :type) "unknown")))
      (insert (format "- Working directory: %s\n" cwd))
      (insert (format "- Generated: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (pcase (plist-get target :type)
        ('range (mevedel-review--write-range-package cwd target))
        ('commit (mevedel-review--write-commit-package cwd target))
        ('uncommitted (mevedel-review--write-uncommitted-package cwd))
        (_ (user-error "Unsupported review package target: %S" target))))
    output-file))

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

(defun mevedel-review--write-target-package (cwd target)
  "Write TARGET's review package in CWD, returning the path or nil."
  (condition-case nil
      (when-let* ((spec (mevedel-review--target-package-spec target cwd)))
        (mevedel-review--write-package cwd spec))
    (error nil)))

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
                         :null-object nil
                         :false-object :false)
    (error nil)))

(defun mevedel-review-parse-output (text)
  "Parse reviewer TEXT into a normalized review output plist.
Falls back to storing TEXT in `:overall_explanation' when JSON parsing
fails."
  (or (and (stringp text)
           (or (mevedel-review--parse-json text)
               (let ((start (string-search "{" text))
                     (end (cl-position ?} text :from-end t)))
                 (and start end (< start end)
                      (mevedel-review--parse-json
                       (substring text start (1+ end)))))))
      (list :findings nil
            :overall_correctness "patch is incorrect"
            :overall_explanation (or text "")
            :overall_confidence_score 0.0)))

(defun mevedel-review--plist-get-any (plist &rest keys)
  "Return the first non-nil value in PLIST for KEYS."
  (catch 'found
    (dolist (key keys)
      (when (plist-member plist key)
        (throw 'found (plist-get plist key))))
    nil))

(defun mevedel-review--finding-location (finding)
  "Return a compact location string for FINDING, or nil."
  (let* ((loc (mevedel-review--plist-get-any
               finding :code_location :codeLocation))
         (range (and (listp loc)
                     (mevedel-review--plist-get-any
                      loc :line_range :lineRange)))
         (path (and (listp loc)
                    (mevedel-review--plist-get-any
                     loc :absolute_file_path :absoluteFilePath)))
         (start (and (listp range)
                     (mevedel-review--plist-get-any range :start)))
         (end (and (listp range)
                   (mevedel-review--plist-get-any range :end))))
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
        (let* ((title (or (mevedel-review--plist-get-any finding :title)
                          "Untitled finding"))
               (body (or (mevedel-review--plist-get-any finding :body) ""))
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
                               (or (mevedel-review--plist-get-any
                                    output :overall_explanation
                                    :overallExplanation)
                                   ""))))
         (findings (mevedel-review--plist-get-any output :findings))
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
  "Return non-nil when SKILL is the bundled `/review' command skill."
  (and (mevedel-skill-p skill)
       (equal "review" (mevedel-skill-name skill))
       (eq 'fork (mevedel-skill-context skill))
       (equal "reviewer" (mevedel-skill-agent skill))
       (eq 'bundled (mevedel-skill-source skill))))

(defun mevedel-review--mark-command-outcome (outcome)
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

(defun mevedel-review--git-allow-rules ()
  "Return validation skill-scoped git inspection allow rules, or nil."
  (condition-case nil
      (progn
        (require 'mevedel-tool-exec)
        (require 'mevedel-permissions)
        (unless (mevedel-tool-get "Bash")
          (mevedel-tool-exec--register))
        (mevedel-permission--parse-rule-strings
         mevedel-review--allowed-tool-entries))
    (error nil)))

(defun mevedel-review--permission-rules ()
  "Return reviewer's skill-scoped permission rules, or nil."
  (when-let* ((rules (mevedel-review--git-allow-rules)))
    (append rules (list mevedel-review--bash-deny-rule))))

(defun mevedel-review--verify-permission-rules ()
  "Return verifier git inspection grants, or nil.
Unlike reviewer rules, these do not deny other Bash commands; normal
permission policy decides whether verifier validation commands may run."
  (mevedel-review--git-allow-rules))

(defun mevedel-review--augment-skill (skill)
  "Return a copy of SKILL with review permission grants installed."
  (let ((copy (copy-mevedel-skill skill))
        (rules (mevedel-review--permission-rules)))
    (when rules
      (setf (mevedel-skill-allowed-tools copy)
            mevedel-review--allowed-tool-entries)
      (setf (mevedel-skill-allowed-tool-rules copy) rules))
    copy))

(defun mevedel-review--bundled-skill-file ()
  "Return the bundled review SKILL.md path."
  (file-name-concat mevedel-skills--bundled-dir "review" "SKILL.md"))

(defun mevedel-review--bundled-skill ()
  "Load and return the bundled review skill, or signal."
  (let* ((file (mevedel-review--bundled-skill-file))
         (skill (and (file-readable-p file)
                     (mevedel-skills--build-skill file 'bundled))))
    (unless skill
      (user-error "Bundled review skill is not available"))
    (unless (and (equal "review" (mevedel-skill-name skill))
                 (eq 'fork (mevedel-skill-context skill))
                 (equal "reviewer" (mevedel-skill-agent skill))
                 (eq 'bundled (mevedel-skill-source skill)))
      (user-error "Bundled review skill is malformed"))
    skill))

(defun mevedel-review--review-skill (_session)
  "Return the bundled review skill, ignoring session skill overrides."
  (mevedel-review--augment-skill (mevedel-review--bundled-skill)))

(defun mevedel-review--verify-skill (_session)
  "Return the synthetic first-class verify skill."
  (let ((rules (mevedel-review--verify-permission-rules)))
    (mevedel-skill--create
     :name "verify"
     :context 'fork
     :agent "verifier"
     :source 'bundled
     :allowed-tools mevedel-review--allowed-tool-entries
     :allowed-tool-rules rules)))

(defun mevedel-review--command-skill (command session)
  "Return synthetic or bundled skill metadata for COMMAND and SESSION."
  (if (eq command 'verify)
      (mevedel-review--verify-skill session)
    (mevedel-review--review-skill session)))

(defun mevedel-review--ensure-dispatch-deps (&optional command)
  "Load modules needed when a validation COMMAND is autoloaded directly."
  (require 'mevedel-agents)
  (if (eq command 'verify)
      (progn
        (mevedel-agents-ensure-verifier)
        (unless (mevedel-agent-get "verifier")
          (user-error "Verifier agent is not available")))
    (mevedel-agents-ensure-reviewer)
    (unless (mevedel-agent-get "reviewer")
      (user-error "Reviewer agent is not available"))))

(defun mevedel-review--ensure-agent-spec (data-buffer &optional command)
  "Ensure DATA-BUFFER can dispatch validation COMMAND's agent."
  (require 'mevedel-agent-exec)
  (require 'mevedel-agents)
  (mevedel-review--ensure-dispatch-deps command)
  (let* ((agent-name (mevedel-review--command-agent-name command))
         (agent (mevedel-agent-get agent-name)))
    (unless agent
      (user-error "%s agent is not available"
                  (mevedel-review--command-label command)))
    (with-current-buffer data-buffer
      (setq-local
       mevedel-agent-exec--agents
       (cons (mevedel-agent-to-gptel-spec agent)
             (cl-remove agent-name mevedel-agent-exec--agents
                        :key #'car :test #'equal))))))

(defun mevedel-review--ensure-dispatch-allowed (data-buffer)
  "Signal if DATA-BUFFER cannot accept a direct review dispatch."
  (with-current-buffer data-buffer
    (when (bound-and-true-p mevedel--current-request)
      (user-error "A request is already active -- wait or abort first"))
    (when (bound-and-true-p mevedel--compaction-in-flight)
      (user-error "Compaction in progress"))
    (when (bound-and-true-p mevedel-session--read-only-mode)
      (user-error "Session is open read-only (another host holds the lock)"))))

(defun mevedel-review--record-direct-turn (display data-buffer)
  "Record direct no-view review DISPLAY in DATA-BUFFER."
  (require 'mevedel-utilities)
  (with-current-buffer data-buffer
    (when mevedel--session
      (mevedel-request-begin mevedel--session
                             (and (boundp 'mevedel--current-directive-uuid)
                                  mevedel--current-directive-uuid)))
    (goto-char (point-max))
    (let ((user-turn-start (point)))
      (insert gptel-response-separator)
      (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
        (let ((prefix-length (length prefix)))
          (unless (and (>= (point) (+ (point-min) prefix-length))
                       (string= (buffer-substring-no-properties
                                 (- (point) prefix-length) (point))
                                prefix))
            (unless (bolp) (insert "\n"))
            (insert prefix))))
      (insert display "\n")
      (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))))

(defun mevedel-review--end-direct-request (data-buffer)
  "End DATA-BUFFER's direct review request if one is active."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (bound-and-true-p mevedel--current-request)
        (mevedel-request-end)))))

(defun mevedel-review--progress-render-data (invocation hint &optional command)
  "Return parent-view render-data for validation INVOCATION and HINT."
  (let* ((agent (and (mevedel-agent-invocation-p invocation)
                     (mevedel-agent-invocation-agent invocation)))
         (agent-id (and (mevedel-agent-invocation-p invocation)
                        (mevedel-agent-invocation-agent-id invocation)))
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
     (list :kind 'agent-transcript
           :agent-id agent-id
           :agent-type (or (and agent (mevedel-agent-name agent))
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
  "Insert a hidden live progress handle for validation INVOCATION."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (require 'mevedel-pipeline)
    (let* ((render-data
            (mevedel-review--progress-render-data invocation hint command))
           (block (mevedel-pipeline--format-render-data-block render-data))
           (start nil))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (setq start (point))
      (insert block)
      (add-text-properties start (point) '(gptel ignore))
      (when-let* ((view-buffer (and (boundp 'mevedel--view-buffer)
                                    mevedel--view-buffer))
                  ((buffer-live-p view-buffer)))
        (let ((data-buffer (current-buffer)))
          (mevedel-view-rerender view-buffer)
          (with-current-buffer view-buffer
            (mevedel-view--ensure-request-progress data-buffer)))))))

(defun mevedel-review--run-task
    (prompt hint callback &optional submit-context progress-callback command)
  "Run the dedicated validation task for PROMPT and HINT.
CALLBACK receives the normalized fork-style outcome.  SUBMIT-CONTEXT, when
non-empty, is appended after `UserPromptExpansion' handling.  PROGRESS-CALLBACK,
when non-nil, receives the spawned invocation before the child request is
dispatched.  COMMAND defaults to `review'."
  (let* ((command (or command 'review))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (skill (mevedel-review--command-skill command session)))
    (mevedel-skills-invoke
     skill prompt
     (lambda (outcome)
       (funcall callback
                (if (and (eq command 'review)
                         (eq (plist-get outcome :status) 'ok))
                    (mevedel-review--mark-command-outcome outcome)
                  outcome)))
     :trigger 'user-slash
     :description (or hint (mevedel-review--command-description command))
     :additional-context submit-context
     :on-invocation progress-callback
     :skip-gates t)))

(defun mevedel-review--transform-command-outcome (outcome &optional command)
  "Transform validation OUTCOME for COMMAND before parent insertion."
  (if (eq (or command 'review) 'review)
      (mevedel-review-transform-outcome
       "review" (mevedel-review--mark-command-outcome outcome))
    outcome))

(defun mevedel-review--handle-direct-outcome (outcome data-buffer &optional command)
  "Handle validation OUTCOME for a direct dispatch targeting DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (mevedel-skills--insert-fork-result
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
  "Handle validation OUTCOME for a view-backed dispatch."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (mevedel-skills--insert-fork-result
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
    (display prompt hint view-buffer data-buffer &optional command)
  "Run a dedicated validation task from VIEW-BUFFER for DATA-BUFFER."
  (with-current-buffer view-buffer
    (mevedel-view--run-prompt-submit-hook
     display display
     (lambda (hook-input hook-context)
       (when (and (buffer-live-p view-buffer)
                  (buffer-live-p data-buffer))
         (cond
          ((not (equal hook-input display))
           (let ((model-input (if hook-context
                                  (concat hook-input "\n\n" hook-context)
                                hook-input)))
             (mevedel-view--forward-input
              model-input hook-input
              (lambda ()
                (mevedel-view-history-add hook-input)
                (mevedel-view--fork-if-pending))
              t nil hook-context)))
          (t
           (mevedel-view-history-add display)
           (mevedel-view--fork-if-pending)
           (mevedel-view--start-fork-skill-turn
            display display hook-context)
           (with-current-buffer data-buffer
             (mevedel-review--run-task
              prompt hint
              (lambda (outcome)
                (mevedel-review--handle-view-outcome
                 outcome view-buffer data-buffer command))
              hook-context
              (lambda (invocation)
                (mevedel-review--insert-progress-handle
                 invocation hint command))
              command)))))))))

(defun mevedel-review--dispatch (prompt hint &optional cwd command)
  "Dispatch validation COMMAND with PROMPT and user-facing HINT."
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
      (mevedel-review--ensure-agent-spec data-buffer command)
      (if view-buffer
          (progn
            (mevedel-review--send-from-view
             display prompt hint view-buffer data-buffer command)
            'mevedel-view-sent)
        (message "mevedel: running %s for %s" command-name hint)
        (mevedel-review--record-direct-turn display data-buffer)
        (with-current-buffer data-buffer
          (mevedel-review--run-task
           prompt hint
           (lambda (outcome)
             (mevedel-review--handle-direct-outcome outcome data-buffer command))
           nil
           (lambda (invocation)
             (mevedel-review--insert-progress-handle
              invocation hint command))
           command))))))

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
         (prompt+hint (mevedel-review--prompt-and-hint command target cwd))
         (package-file (mevedel-review--write-target-package cwd target))
         (prompt (if package-file
                     (mevedel-review--prompt-with-package
                      (car prompt+hint) package-file command)
                   (car prompt+hint))))
    (mevedel-review--dispatch
     prompt (cdr prompt+hint) cwd command)))

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
