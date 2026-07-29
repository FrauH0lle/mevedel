;;; test-mevedel-menu.el -- Tests for session cockpit -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'gptel)
(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-cockpit)
(require 'mevedel-execution)
(require 'mevedel-executions-list)
(require 'mevedel-gptel-bridge)
(require 'mevedel-menu)
(require 'mevedel-goal)
(require 'mevedel-models)
(require 'mevedel-mentions)
(require 'mevedel-permissions)
(require 'mevedel-plugins)
(require 'mevedel-presets)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-ui)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-tools-list)
(require 'mevedel-view)
(require 'mevedel-workspace)
(require 'mevedel-worktree)

;; `gptel'
(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-tools)

;; `transient'
(declare-function transient--emergency-exit "transient" (&optional id))
(defvar transient--buffer-name)
(defvar transient--transient-map)

(defmacro mevedel-menu-test--with-model-backends (&rest body)
  "Run BODY with isolated gptel model backends."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     ,@body))

(defmacro mevedel-menu-test--with-buffers (&rest body)
  "Execute BODY with a paired data and view buffer."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-menu-root-" t))
          (mevedel-user-dir (file-name-as-directory
                             (make-temp-file "mevedel-menu-user-" t)))
          (workspace (mevedel-workspace-get-or-create
                      'project (format "menu-%s" root) root "mevedel"))
          (session (mevedel-session-create "main" workspace))
          (data-buf (generate-new-buffer " *menu-data*"))
          (view-buf (generate-new-buffer " *menu-view*")))
     (unwind-protect
         (progn
           (mevedel-gptel-bridge--clear-return-state)
           (mevedel-gptel-bridge--cleanup-advice)
           (with-current-buffer data-buf
             (org-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local mevedel--session session)
             (setq-local mevedel-permission-mode 'ask)
             (setq-local gptel-model 'gpt-5.5)
             (setq-local gptel-tools '(read edit)))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (mevedel-gptel-bridge--clear-return-state)
       (mevedel-gptel-bridge--cleanup-advice)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t))
       (when (file-directory-p root)
         (delete-directory root t)))))

(mevedel-deftest mevedel-menu ()
  ,test
  (test)
  :doc "opens the top cockpit from the view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-prefix called-buffer)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix
                         called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (call-interactively #'mevedel-menu)))
      (should (eq called-prefix 'mevedel-menu--top))
      (should (eq called-buffer view-buf))))

  :doc "opens the top cockpit from the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-prefix called-buffer)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix
                         called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (call-interactively #'mevedel-menu)))
      (should (eq called-prefix 'mevedel-menu--top))
      (should (eq called-buffer data-buf)))))

(mevedel-deftest mevedel-menu-open ()
  ,test
  (test)
  :doc "opens the requested top area"
  (mevedel-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (mevedel-menu-open 'top)))
      (should (eq called-prefix 'mevedel-menu--top))))

  :doc "opens requested mode, permissions, model, Goal, and Preset surfaces"
  (mevedel-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (dolist (area '((mode . mevedel-menu--mode)
                          (permissions . mevedel-menu--permissions)
                          (model . mevedel-menu--model-selection)
                          (goal . mevedel-menu--goal)
                          (preset . mevedel-menu--preset)))
            (setq called-prefix nil)
            (mevedel-menu-open (car area))
            (should (eq called-prefix (cdr area))))))))

  :doc "opens requested tools, executions, skills, and plugins surfaces"
  (mevedel-menu-test--with-buffers
    (let (tools-context tools-buffer executions-context executions-buffer
          skills-context skills-buffer
          plugins-context plugins-buffer)
      (cl-letf (((symbol-function 'mevedel-tools-list-open)
                 (lambda (context)
                   (setq tools-context context
                         tools-buffer (current-buffer))))
                ((symbol-function 'mevedel-executions-list-open)
                 (lambda (context)
                   (setq executions-context context
                         executions-buffer (current-buffer))))
                ((symbol-function 'mevedel-skills-list-open)
                 (lambda (context)
                   (setq skills-context context
                         skills-buffer (current-buffer))))
                ((symbol-function 'mevedel-plugins-list-open)
                 (lambda (context)
                   (setq plugins-context context
                         plugins-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu-open 'tools)
          (mevedel-menu-open 'executions)
          (mevedel-menu-open 'skills)
          (mevedel-menu-open 'plugins)))
      (should (eq (mevedel-cockpit-context-session tools-context) session))
      (should (eq (mevedel-cockpit-context-view-buffer tools-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer tools-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer tools-context)
                  view-buf))
      (should (eq tools-buffer data-buf))
      (should (eq (mevedel-cockpit-context-session executions-context)
                  session))
      (should (eq (mevedel-cockpit-context-view-buffer executions-context)
                  view-buf))
      (should (eq executions-buffer data-buf))
      (should (eq (mevedel-cockpit-context-session skills-context)
                  session))
      (should (eq (mevedel-cockpit-context-view-buffer skills-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer skills-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer skills-context)
                  view-buf))
      (should (eq skills-buffer data-buf))
      (should (eq (mevedel-cockpit-context-workspace plugins-context)
                  (mevedel-session-workspace session)))
      (should (eq (mevedel-cockpit-context-view-buffer plugins-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer plugins-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer plugins-context)
                  view-buf))
      (should (eq plugins-buffer data-buf))))

  :doc "opens plugins management surface from the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (plugins-context plugins-buffer)
      (cl-letf (((symbol-function 'mevedel-plugins-list-open)
                 (lambda (context)
                   (setq plugins-context context
                         plugins-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu-open 'plugins)))
      (should (eq (mevedel-cockpit-context-view-buffer plugins-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer plugins-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer plugins-context)
                  data-buf))
      (should (eq plugins-buffer data-buf))))

  :doc "opens requested worktree and help cockpit surfaces"
  (mevedel-menu-test--with-buffers
    (let (worktree-buffer help-opened)
      (cl-letf (((symbol-function 'mevedel-worktree-status-open)
                 (lambda () (setq worktree-buffer (current-buffer))))
                ((symbol-function 'mevedel-menu-help-open)
                 (lambda () (setq help-opened t))))
        (with-current-buffer view-buf
          (mevedel-menu-open 'worktree)
          (mevedel-menu-open 'help)))
      (should (eq worktree-buffer data-buf))
      (should help-opened)))

  :doc "opens the requested gptel bridge area"
  (mevedel-menu-test--with-buffers
    (let (called-context)
      (cl-letf (((symbol-function 'mevedel-gptel-bridge-open)
                 (lambda (context)
                   (setq called-context context))))
        (with-current-buffer data-buf
          (mevedel-menu-open 'gptel)))
      (should (eq (mevedel-cockpit-context-data-buffer called-context)
                  data-buf))))

  :doc "signals outside a live view/data pair"
  (with-temp-buffer
    (should-error (mevedel-menu-open 'top) :type 'user-error)))

(mevedel-deftest mevedel-menu--goal-description ()
  ,test
  (test)
  :doc "shows Goal lifecycle, accounting, and accepted-plan reference"
  (mevedel-menu-test--with-buffers
    (let ((goal
           (mevedel-goal--create
            :id "g1" :objective "Ship the feature" :status 'paused
            :token-budget 1000 :tokens-used 400
            :time-used-seconds 12 :turns-run 3
            :reason "Provider credits exhausted"
            :plan-reference "plans/accepted.md")))
      (setf (mevedel-session-goal session) goal)
      (with-current-buffer view-buf
        (let ((text (mevedel-menu--goal-description)))
          (dolist (needle '("Ship the feature" "Status: paused"
                            "Provider credits exhausted" "400/1000"
                            "Turns: 3" "Elapsed: 12s" "plans/accepted.md"))
            (should (string-match-p (regexp-quote needle) text))))))))
  :doc "shows an unbounded Goal budget consistently"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-goal session)
          (mevedel-goal--create
           :status 'active :tokens-used 3 :turns-run 0
           :time-used-seconds 0))
    (with-current-buffer view-buf
      (should (string-match-p
               "Budget: 3 tokens / unbounded"
               (mevedel-menu--goal-description)))))

(mevedel-deftest mevedel-menu--preset-description ()
  ,test
  (test)
  :doc "shows resolved workload routing without Goal-only workloads"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast")
                    gptel-model 'fast-model
                    mevedel-model-tiers
                    '((strong :provider "Balanced:balanced-model"))
                    mevedel-model-workloads
                    '((planning :tier strong)))
        (setf (mevedel-session-preset-name session) 'my-team))
      (with-current-buffer view-buf
        (let ((text (mevedel-menu--preset-description)))
          (dolist (needle '("Preset: my-team" "planning"
                            "Balanced:balanced-model"))
            (should (string-match-p (regexp-quote needle) text))))))))

(mevedel-deftest mevedel-menu--goal-resumable-p ()
  ,test
  (test)
  :doc "enables resume only for paused and blocked Goals"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-session-id session) "s1")
    (dolist (entry '((active . nil) (paused . t) (blocked . t)
                     (complete . nil)))
      (setf (mevedel-session-goal session)
            (mevedel-goal--create :status (car entry)))
      (with-current-buffer view-buf
        (should (eq (and (mevedel-menu--goal-resumable-p) t)
                    (cdr entry)))))))

(mevedel-deftest mevedel-menu--current-goal ()
  ,test (test)
  :doc "returns the owning session Goal during an active request"
  (mevedel-menu-test--with-buffers
    (let ((goal (mevedel-goal--create :id "g1")))
      (setf (mevedel-session-goal session) goal)
      (with-current-buffer data-buf
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (with-current-buffer view-buf
        (should (eq goal (mevedel-menu--current-goal)))))))

(mevedel-deftest mevedel-menu--goal-active-p ()
  ,test (test)
  :doc "requires active status"
  (mevedel-menu-test--with-buffers
    (let ((goal (mevedel-goal--create :status 'active)))
      (setf (mevedel-session-goal session) goal)
      (with-current-buffer view-buf (should (mevedel-menu--goal-active-p)))
      (setf (mevedel-goal-status goal) 'paused)
      (with-current-buffer view-buf
        (should-not (mevedel-menu--goal-active-p))))))

(mevedel-deftest mevedel-menu--goal-start-inapt-p ()
  ,test (test)
  :doc "permits replacement only for absent or complete Goals"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should-not (mevedel-menu--goal-start-inapt-p))
      (setf (mevedel-session-goal session)
            (mevedel-goal--create :status 'active))
      (should (mevedel-menu--goal-start-inapt-p))
      (setf (mevedel-goal-status (mevedel-session-goal session)) 'complete)
      (should-not (mevedel-menu--goal-start-inapt-p)))))

(mevedel-deftest mevedel-menu--goal-clearable-p ()
  ,test (test)
  :doc "rejects clear while a request is active"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-goal session) (mevedel-goal--create :status 'paused))
    (with-current-buffer view-buf (should (mevedel-menu--goal-clearable-p)))
    (with-current-buffer data-buf (setq-local mevedel--current-request t))
    (with-current-buffer view-buf
      (should-not (mevedel-menu--goal-clearable-p)))))

(mevedel-deftest mevedel-menu--owned-goal ()
  ,test (test)
  :doc "returns the current session's direct Goal"
  (mevedel-menu-test--with-buffers
    (let ((goal (mevedel-goal--create :id "g1")))
      (setf (mevedel-session-goal session) goal)
      (with-current-buffer view-buf
        (should (eq goal (mevedel-menu--owned-goal)))))))

(mevedel-deftest mevedel-menu--open-goal ()
  ,test (test)
  :doc "routes to the Goal area"
  (let (area)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (value) (setq area value))))
      (mevedel-menu--open-goal))
    (should (eq 'goal area))))

(mevedel-deftest mevedel-menu--open-preset ()
  ,test (test)
  :doc "routes to the Preset area"
  (let (area)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (value) (setq area value))))
      (mevedel-menu--open-preset))
    (should (eq 'preset area))))

(mevedel-deftest mevedel-menu--open-executions ()
  ,test (test)
  :doc "routes to the executions area"
  (let (area)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (value) (setq area value))))
      (mevedel-menu--open-executions))
    (should (eq 'executions area))))

(mevedel-deftest mevedel-menu--goal-call ()
  ,test (test)
  :doc "runs lifecycle functions in the data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (with-current-buffer view-buf
        (mevedel-menu--goal-call
         (lambda () (setq called-buffer (current-buffer)))))
      (should (eq data-buf called-buffer)))))

(mevedel-deftest mevedel-menu--goal-start ()
  ,test (test)
  :doc "starts a Goal from prompted input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Ship"))
              ((symbol-function 'mevedel-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (mevedel-menu--goal-start))
    (should (equal (cdr call) '("Ship")))))

(mevedel-deftest mevedel-menu--goal-edit ()
  ,test (test)
  :doc "edits a Goal from prompted input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Revised"))
              ((symbol-function 'mevedel-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (mevedel-menu--goal-edit))
    (should (equal (cdr call) '("Revised")))))

(mevedel-deftest mevedel-menu--goal-budget ()
  ,test (test)
  :doc "adjusts a Goal budget from prompted input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "12000"))
              ((symbol-function 'mevedel-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (mevedel-menu--goal-budget))
    (should (equal (cdr call) '("12000")))))

(mevedel-deftest mevedel-menu--select-preset ()
  ,test
  (test)
  :doc "selects in the owning data buffer only and preserves a multiline draft"
  (mevedel-menu-test--with-buffers
    (let* ((other (mevedel-session-create "other" workspace))
           (mevedel-preset--registry '((team :settings nil)))
           applied-buffer)
      (with-current-buffer view-buf
        (goto-char (point-max))
        (insert "> draft\nsecond line"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "team"))
                ((symbol-function 'mevedel-preset-apply)
                 (lambda (name &optional buffer)
                   (setq applied-buffer (or buffer (current-buffer)))
                   (setf (mevedel-session-preset-name mevedel--session)
                         name))))
        (with-current-buffer view-buf
          (mevedel-menu--select-preset)))
      (should (eq applied-buffer data-buf))
      (should (eq 'team (mevedel-session-preset-name session)))
      (should-not (mevedel-session-preset-name other))
      (with-current-buffer view-buf
        (should (string-suffix-p "> draft\nsecond line"
                                 (buffer-string)))))))

(mevedel-deftest mevedel-menu--header ()
  ,test
  (test)
  :doc "shows session orientation and idle state"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (let ((header (mevedel-menu--header)))
        (should (string-match-p "mevedel: main" header))
        (should (string-match-p "ask" header))
        (should (string-match-p "idle" header)))))

  :doc "shows running request state"
  (mevedel-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local mevedel--current-request t))
    (with-current-buffer view-buf
      (should (string-match-p "running" (mevedel-menu--header))))))

(mevedel-deftest mevedel-menu--worktree-label ()
  ,test
  (test)
  :doc "shows the current branch"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "main" (mevedel-menu--worktree-label))))))

  :doc "falls back to detached HEAD"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "detached abc123"))))
      (with-current-buffer view-buf
        (should (string= "detached abc123"
                         (mevedel-menu--worktree-label))))))

  :doc "reports non-Git directories"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state not-git :label "not-git"))))
      (with-current-buffer view-buf
        (should (string= "not-git" (mevedel-menu--worktree-label)))))))

(mevedel-deftest mevedel-menu--worktree-description ()
  ,test
  (test)
  :doc "shows worktree description with branch label"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "Worktree  main"
                         (substring-no-properties
                          (mevedel-menu--worktree-description))))))))

(mevedel-deftest mevedel-menu--executions-description ()
  ,test
  (test)
  :doc "shows the current live execution count"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-execution-count-user)
               (lambda (seen-session)
                 (should (eq session seen-session))
                 3)))
      (with-current-buffer view-buf
        (should (string= "Processes 3 live"
                         (substring-no-properties
                          (mevedel-menu--executions-description))))))))

(mevedel-deftest mevedel-menu--top-descriptions ()
  ,test
  (test)
  :doc "shows padded top-level state rows"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "Mode      ask"
                         (substring-no-properties
                          (mevedel-menu--mode-description))))
        (should (string= "Model     gpt-5.5"
                         (substring-no-properties
                          (mevedel-menu--model-description))))
        (should (string= "Tools     2 active"
                         (substring-no-properties
                          (mevedel-menu--tools-description))))
        (should (string-match-p
                 (rx string-start "Skills" (+ space) (+ digit) "/" (+ digit)
                     string-end)
                 (substring-no-properties
                  (mevedel-menu--skills-description))))
        (should (string-match-p
                 (rx string-start "Plugins" (+ space) (+ digit) "/" (+ digit)
                     string-end)
                 (substring-no-properties
                  (mevedel-menu--plugins-description))))
        (should (string= "Worktree  main"
                         (substring-no-properties
                          (mevedel-menu--worktree-description))))))))

(mevedel-deftest mevedel-menu--mode-choice-description ()
  ,test
  (test)
  :doc "marks the active mode without exposing internal mode names"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (string= "ask     current prompt for edits and uncertain execution"
                       (substring-no-properties
                        (mevedel-menu--mode-ask-description))))
      (should (string= "edits           auto-apply edit previews"
                       (substring-no-properties
                        (mevedel-menu--mode-edits-description))))))

  :doc "updates the current marker when the session mode changes"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-menu--set-mode 'full-auto)
      (should (string= "full-auto current auto-allow tools"
                       (substring-no-properties
                        (mevedel-menu--mode-full-auto-description))))
      (should (string= "ask             prompt for edits and uncertain execution"
                       (substring-no-properties
                        (mevedel-menu--mode-ask-description))))))

  :doc "marks Plan instead of its underlying permission policy"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-plan-mode session) t
          (mevedel-session-permission-mode session) 'full-auto)
    (with-current-buffer view-buf
      (should (string= "Mode      Plan/full-auto"
                       (substring-no-properties
                        (mevedel-menu--mode-description))))
      (should (string= "Plan    current inspect and discuss without direct edits"
                       (substring-no-properties
                        (mevedel-menu--mode-plan-description))))
      (should (string= "full-auto         auto-allow tools"
                       (substring-no-properties
                        (mevedel-menu--mode-full-auto-description)))))))

(mevedel-deftest mevedel-menu-help--text ()
  ,test
  (test)
  :doc "covers command discovery without duplicating transient keys"
  (let ((text (mevedel-menu-help--text)))
    (dolist (needle '("Session cockpit"
                      "transient menu is the live key reference"
                      "Slash commands that open UI"
                      "Direct slash commands"
                      "/mode MODE, /model MODEL"
                      "/ps"
                      "/stop [EXECUTION_ID]"
                      "Modes"
                      "View and data buffers"))
      (should (string-match-p (regexp-quote needle) text)))
    (dolist (stale '("N Next query"
                     "B Previous query"
                     "b Previous display"))
      (should-not (string-match-p (regexp-quote stale) text)))))

(mevedel-deftest mevedel-menu--mode-plan-description ()
  ,test
  (test)
  :doc "marks Plan as current without hiding its workflow description"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-plan-mode session) t)
    (with-current-buffer view-buf
      (let ((text (substring-no-properties
                   (mevedel-menu--mode-plan-description))))
        (should (string-match-p "Plan" text))
        (should (string-match-p "current" text))
        (should (string-match-p "without direct edits" text))))))

(mevedel-deftest mevedel-menu--enter-plan ()
  ,test
  (test)
  :doc "enters Plan through the cockpit's authoritative data buffer"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-menu--enter-plan))
    (should (mevedel-session-plan-mode session))))

(mevedel-deftest mevedel-menu--mode-symbol ()
  ,test
  (test)
  :doc "uses the session's canonical permission mode"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-permission-mode session) 'full-auto)
    (with-current-buffer view-buf
      (should (eq (mevedel-menu--mode-symbol
                   session data-buf view-buf)
                  'full-auto))
      (should (string= "Mode      full-auto"
                       (substring-no-properties
                        (mevedel-menu--mode-description)))))))

(mevedel-deftest mevedel-menu--set-mode ()
  ,test
  (test)
  :doc "mode setter updates the paired data buffer session"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-menu--set-mode 'edits))
    (with-current-buffer data-buf
      (should (eq 'edits
                  (mevedel-session-permission-mode mevedel--session)))
      (should (eq 'edits mevedel-permission-mode))))

  :doc "an explicit permission choice exits Plan mode"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-plan-mode session) t)
    (with-current-buffer view-buf
      (mevedel-menu--set-mode 'ask))
    (should-not (mevedel-session-plan-mode session))))

(mevedel-deftest mevedel-menu--model-selection-description ()
  ,test
  (test)
  :doc "describes the shared selection and marks inherited values"
  (let ((scope '(:title "Directive model"
                 :model-provider "Fast:fast-model"
                 :reasoning-effort high
                 :inherited t)))
    (cl-letf (((symbol-function 'transient-scope)
               (lambda (&rest _) scope)))
      (should
       (equal "Directive model: Fast:fast-model · effort high · session"
              (substring-no-properties
               (mevedel-menu--model-selection-description)))))))

(mevedel-deftest mevedel-menu--refresh-plan-approval ()
  ,test
  (test)
  :doc "rerenders only when the session owns a pending Plan approval"
  (let ((session (mevedel-session--create :name "main"))
        rendered)
    (cl-letf (((symbol-function 'mevedel-plan-approval-render)
               (lambda (value) (setq rendered value))))
      (mevedel-menu--refresh-plan-approval session)
      (should-not rendered)
      (setf (mevedel-session-pending-plan-approval session) '(:pending t))
      (mevedel-menu--refresh-plan-approval session)
      (should (eq session rendered)))))

(mevedel-deftest mevedel-menu-open-model-selection ()
  ,test
  (test)
  :doc "opens the shared model-selection surface with caller-owned state"
  (let (prefix scope)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (value &rest args)
                 (setq prefix value
                       scope (plist-get args :scope)))))
      (mevedel-menu-open-model-selection
       :title "Directive model"
       :provider "Fast:fast-model"
       :effort 'high
       :update #'ignore
       :reset #'identity
       :inherited t))
    (should (eq 'mevedel-menu--model-selection prefix))
    (should (equal "Directive model" (plist-get scope :title)))
    (should (equal "Fast:fast-model" (plist-get scope :model-provider)))
    (should (eq 'high (plist-get scope :reasoning-effort)))
    (should (eq #'ignore (plist-get scope :update)))
    (should (eq #'identity (plist-get scope :reset)))
    (should (eq t (plist-get scope :inherited))))

  :doc "binds both Return events while advertising only RET"
  (unwind-protect
      (progn
        (mevedel-menu-open-model-selection
         :title "Directive model"
         :provider "Fast:fast-model"
         :effort nil
         :update #'ignore
         :inherited nil)
        (should
         (eq #'mevedel-menu--model-selection-select-model
             (lookup-key transient--transient-map (kbd "RET"))))
        (should
         (eq #'mevedel-menu--model-selection-select-model
             (lookup-key transient--transient-map (kbd "<return>"))))
        (let ((display
               (with-current-buffer transient--buffer-name
                 (substring-no-properties (buffer-string)))))
          (should (string-match-p "RET +Select model" display))
          (should-not (string-match-p "<return>" display))))
    (transient--emergency-exit :test)
    (should-not
     (memq transient--transient-map overriding-terminal-local-map))))

(mevedel-deftest mevedel-menu--open-model ()
  ,test
  (test)
  :doc "opens shared selection and applies updates to the session"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (let (options)
        (with-current-buffer data-buf
          (setq-local gptel-backend (gptel-get-backend "Fast")
                      gptel-model 'fast-model
                      gptel-reasoning-effort 'high))
        (cl-letf (((symbol-function 'mevedel-menu-open-model-selection)
                   (lambda (&rest args) (setq options args))))
          (with-current-buffer view-buf
            (mevedel-menu--open-model)))
        (should (equal "Session model" (plist-get options :title)))
        (should (equal "Fast:fast-model" (plist-get options :provider)))
        (should (eq 'high (plist-get options :effort)))
        (funcall (plist-get options :update)
                 "Balanced:balanced-model" nil)
        (with-current-buffer data-buf
          (should (eq 'balanced-model gptel-model))
          (should-not gptel-reasoning-effort))
        (should (equal "Balanced:balanced-model"
                       (mevedel-session-model-provider session)))
        (should-not (mevedel-session-reasoning-effort session))))))

(mevedel-deftest mevedel-menu--model-selection-select-model ()
  ,test
  (test)
  :doc "updates caller state and resets unsupported effort"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (let ((scope '(:model-provider "Fast:fast-model"
                     :reasoning-effort high
                     :inherited t))
            update)
        (with-current-buffer data-buf
          (setq-local gptel-backend (gptel-get-backend "Fast")
                      gptel-model 'fast-model
                      gptel-reasoning-effort 'high))
        (setf (plist-get scope :update)
              (lambda (provider effort)
                (setq update (list provider effort))))
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda (&rest _) scope))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Balanced:balanced-model")))
          (mevedel-menu--model-selection-select-model))
        (should (equal '("Balanced:balanced-model" nil) update))
        (should (equal "Balanced:balanced-model"
                       (plist-get scope :model-provider)))
        (should-not (plist-get scope :reasoning-effort))
        (should-not (plist-get scope :inherited))
        (with-current-buffer data-buf
          (should (equal "Fast" (gptel-backend-name gptel-backend)))
          (should (eq 'fast-model gptel-model))
          (should (eq 'high gptel-reasoning-effort)))
        (should-not (mevedel-session-model-provider session))))))

(mevedel-deftest mevedel-menu--model-selection-select-effort ()
  ,test
  (test)
  :doc "effort selection updates caller state without changing the session"
  (mevedel-menu-test--with-model-backends
    (let ((old-effort (get 'balanced-model :reasoning-effort))
          (scope '(:model-provider "Balanced:balanced-model"
                   :reasoning-effort nil
                   :inherited t))
          update)
      (unwind-protect
          (progn
            (put 'balanced-model :reasoning-effort '(member low high))
            (setf (plist-get scope :update)
                  (lambda (provider effort)
                    (setq update (list provider effort))))
            (cl-letf (((symbol-function 'transient-scope)
                       (lambda (&rest _) scope))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "high")))
              (mevedel-menu--model-selection-select-effort))
            (should (equal '("Balanced:balanced-model" high) update))
            (should (eq 'high (plist-get scope :reasoning-effort)))
            (should-not (plist-get scope :inherited)))
        (put 'balanced-model :reasoning-effort old-effort)))))

(mevedel-deftest mevedel-menu--model-selection-reset ()
  ,test
  (test)
  :doc "reset restores caller-provided inherited values"
  (let ((scope '(:model-provider "Fast:fast-model"
                 :reasoning-effort high
                 :inherited nil))
        reset-called)
    (setf (plist-get scope :reset)
          (lambda ()
            (setq reset-called t)
            '("Session:session-model" low)))
    (cl-letf (((symbol-function 'transient-scope)
               (lambda (&rest _) scope)))
      (mevedel-menu--model-selection-reset))
    (should reset-called)
    (should (equal "Session:session-model"
                   (plist-get scope :model-provider)))
    (should (eq 'low (plist-get scope :reasoning-effort)))
    (should (eq t (plist-get scope :inherited)))))

(mevedel-deftest mevedel-menu--send ()
  ,test
  (test)
  :doc "send runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-view-send)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--send))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--abort ()
  ,test
  (test)
  :doc "abort runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-view-abort)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--abort))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--rewind-here ()
  ,test
  (test)
  :doc "rewind-at-point runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-view-rewind-at-point)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--rewind-here))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--switch-variant-here ()
  ,test
  (test)
  :doc "variant switching at point runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf
          (((symbol-function
             'mevedel-view-switch-conversation-variant-at-point)
            (lambda ()
              (interactive)
              (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--switch-variant-here))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--fork-conversation-here ()
  ,test
  (test)
  :doc "Conversation Fork arming runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-view-arm-conversation-fork)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--fork-conversation-here))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--fork-worktree-here ()
  ,test
  (test)
  :doc "Worktree Fork arming runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-view-arm-worktree-fork)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--fork-worktree-here))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--call-live-tip-data ()
  ,test
  (test)
  :doc "refuses data actions while the paired view inspects history"
  (mevedel-menu-test--with-buffers
    (let (called)
      (with-current-buffer view-buf
        (setq-local mevedel-view--historical-segment-number 1
                    mevedel-view--historical-segment-buffer data-buf)
        (should-error
         (mevedel-menu--call-live-tip-data
          (lambda () (setq called t)))
         :type 'user-error))
      (should-not called))))

(mevedel-deftest mevedel-menu--compact ()
  ,test
  (test)
  :doc "compact runs in the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-compact)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu--compact))
        (should (eq called-buffer data-buf))))))

(mevedel-deftest mevedel-menu--review ()
  ,test
  (test)
  :doc "review runs in the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-review)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu--review))
        (should (eq called-buffer data-buf))))))

(mevedel-deftest mevedel-menu--verify ()
  ,test
  (test)
  :doc "verify runs in the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-verify)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu--verify))
        (should (eq called-buffer data-buf))))))

(mevedel-deftest mevedel-menu--toggle-data-view ()
  ,test
  (test)
  :doc "toggle data/view switches both directions"
  (mevedel-menu-test--with-buffers
    (switch-to-buffer view-buf)
    (mevedel-menu--toggle-data-view)
    (should (eq (window-buffer (selected-window)) data-buf))
    (with-current-buffer data-buf
      (mevedel-menu--toggle-data-view))
    (should (eq (window-buffer (selected-window)) view-buf))))

(mevedel-deftest mevedel-menu-help-open
  (:after-each (when (get-buffer mevedel-menu-help-buffer-name)
                 (kill-buffer mevedel-menu-help-buffer-name)))
  ,test
  (test)
  :doc "opens the help surface buffer"
  (let ((buffer (mevedel-menu-help-open)))
    (with-current-buffer buffer
      (should (derived-mode-p 'special-mode)))))

(mevedel-deftest mevedel-menu--open-gptel ()
  ,test
  (test)
  :doc "delegates to the gptel bridge with the current cockpit context"
  (mevedel-menu-test--with-buffers
    (let (called-context)
      (cl-letf (((symbol-function 'mevedel-gptel-bridge-open)
                 (lambda (context)
                   (setq called-context context))))
        (with-current-buffer view-buf
          (mevedel-menu--open-gptel)))
      (should (eq (mevedel-cockpit-context-view-buffer called-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer called-context)
                  data-buf)))))

(mevedel-deftest mevedel-menu--send-inapt-p ()
  ,test
  (test)
  :doc "idle session can send"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should-not (mevedel-menu--send-inapt-p))))

  :doc "running session cannot send"
  (mevedel-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local mevedel--current-request t))
    (with-current-buffer view-buf
      (should (mevedel-menu--send-inapt-p)))))

(mevedel-deftest mevedel-menu--abort-inapt-p ()
  ,test
  (test)
  :doc "idle session cannot abort"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (mevedel-menu--abort-inapt-p))))

  :doc "running session can abort"
  (mevedel-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local mevedel--current-request t))
    (with-current-buffer view-buf
      (should-not (mevedel-menu--abort-inapt-p)))))

(mevedel-deftest mevedel-menu--permissions ()
  ,test
  (test)
  :doc "lists distinct session operation, network, and resource authority"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)
            ("Bash" :pattern "npx test*" :network t :action allow)))
    (setf (mevedel-session-resource-grants session)
          '((:path "/tmp/external" :access read)))
    (with-current-buffer view-buf
      (let ((description (mevedel-menu--permissions-description)))
        (should (string-match-p "Session operation" description))
        (should (string-match-p "Session network" description))
        (should (string-match-p "Session resource" description)))))
  :doc "revokes a session network rule without changing operation or resource authority"
  (mevedel-menu-test--with-buffers
    (let ((operation '("Bash" :pattern "npx test*" :action allow))
          (network
           '("Bash" :pattern "npx test*" :network t :action allow))
          (resource '(:path "/tmp/external" :access read)))
      (setf (mevedel-session-permission-rules session)
            (list operation network))
      (setf (mevedel-session-resource-grants session)
            (list resource))
      (with-current-buffer view-buf
        (let* ((candidate
                (seq-find
                 (lambda (item)
                   (string-match-p "Session network" (car item)))
                 (mevedel-menu--permission-candidates)))
               (label (car candidate)))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) label)))
            (mevedel-menu--permissions-revoke))))
      (should
       (equal (list operation)
              (mevedel-session-permission-rules session)))
      (should
       (equal (list resource)
              (mevedel-session-resource-grants session)))))
  :doc "revokes one workspace resource without changing workspace network authority"
  (mevedel-menu-test--with-buffers
    (let ((path "/tmp/workspace-external")
          (network
           '("Bash" :pattern "npx test*" :network t :action allow)))
      (mevedel-permission--save-persistent-rule
       workspace "Bash" 'allow nil
       :spec-key :pattern :spec-value "npx test*" :network t)
      (mevedel-permission--save-persistent-resource-grant
       workspace path 'read)
      (with-current-buffer view-buf
        (let* ((candidate
                (seq-find
                 (lambda (item)
                   (string-match-p "Workspace resource" (car item)))
                 (mevedel-menu--permission-candidates)))
               (label (car candidate)))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) label)))
            (mevedel-menu--permissions-revoke))))
      (let ((authority
             (mevedel-permission-persistent-authority workspace)))
        (should (equal (list network) (plist-get authority :rules)))
        (should-not (plist-get authority :resource-grants))))))

(provide 'test-mevedel-menu)

;;; test-mevedel-menu.el ends here
