;;; test-mevedel-menu.el -- Tests for session cockpit -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
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
(defvar gptel-tools)

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

  :doc "opens requested mode, model, Goal, and Preset cockpit surfaces"
  (mevedel-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (dolist (area '((mode . mevedel-menu--mode)
                          (model . mevedel-menu--model)
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
  :doc "shows lifecycle, policy, artifacts, guardian, and recovery state"
  (mevedel-menu-test--with-buffers
    (let ((goal
           (mevedel-goal--create
            :id "g1" :objective "Ship the feature" :status 'paused
            :phase 'implementing :cycle 2 :approval-policy 'automatic
            :implementation-context 'focused :token-budget 1000
            :token-usage 400 :reason "Provider credits exhausted"
            :current-plan '(:path "goals/g1/cycle-002-plan.md")
            :review-findings "One test remains"
            :checkpoint '(:dispatch-state failed)
            :owner-session (mevedel-session-session-id session)
            :execution-home
            (list :kind 'worktree :directory "/tmp/goal/"
                  :session-id (mevedel-session-session-id session))
            :cycles '((:cycle 2 :guardian-audits
                       ((:verdict ask :reason "Needs confirmation")))))))
      (setf (mevedel-session-goal session) goal)
      (with-current-buffer view-buf
        (let ((text (mevedel-menu--goal-description)))
          (dolist (needle '("Ship the feature" "paused / implementing"
                            "Approval: automatic · Tool permissions: ask"
                            "400/1000"
                            "worktree" "focused" "cycle-002-plan.md"
                            "One test remains" "Needs confirmation"
                            "failed — Provider credits exhausted"))
            (should (string-match-p (regexp-quote needle) text))))))))

(mevedel-deftest mevedel-menu--preset-description ()
  ,test
  (test)
  :doc "shows resolved workload routing and actual phase provider effort"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast")
                    gptel-model 'fast-model
                    mevedel-model-tiers
                    '((strong :provider "Balanced:balanced-model"))
                    mevedel-model-workloads
                    '((planning :tier strong) (implementation)))
        (setf (mevedel-session-preset-name session) 'my-team
              (mevedel-session-goal session)
              (mevedel-goal--create
               :status 'active :phase 'planning :cycle 1
               :cycles '((:cycle 1 :providers
                          ((planning :provider "Fast:fast-model"
                                     :effort high)))))))
      (with-current-buffer view-buf
        (let ((text (mevedel-menu--preset-description)))
          (dolist (needle '("Preset: my-team" "planning"
                            "Balanced:balanced-model"
                            "actual Fast:fast-model / effort high"))
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
            (mevedel-goal--create
             :status (car entry)
             :owner-session (mevedel-session-session-id session)
             :execution-home
             (list :session-id (mevedel-session-session-id session))))
      (with-current-buffer view-buf
        (should (eq (and (mevedel-menu--goal-resumable-p) t)
                    (cdr entry)))))))

(mevedel-deftest mevedel-menu--current-goal ()
  ,test (test)
  :doc "returns the owning session Goal"
  (mevedel-menu-test--with-buffers
    (let ((goal (mevedel-goal--create :id "g1")))
      (setf (mevedel-session-goal session) goal)
      (with-current-buffer view-buf
        (should (eq goal (mevedel-menu--current-goal)))))))

(mevedel-deftest mevedel-menu--goal-active-p ()
  ,test (test)
  :doc "requires active status and local ownership"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-session-id session) "s1")
    (let ((goal (mevedel-goal--create
                 :status 'active
                 :owner-session (mevedel-session-session-id session)
                 :execution-home
                 (list :session-id (mevedel-session-session-id session)))))
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

(mevedel-deftest mevedel-menu--goal-editable-p ()
  ,test (test)
  :doc "rejects complete and non-owned Goals"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-session-id session) "s1")
    (let ((goal (mevedel-goal--create
                 :status 'paused
                 :owner-session (mevedel-session-session-id session)
                 :execution-home
                 (list :session-id (mevedel-session-session-id session)))))
      (setf (mevedel-session-goal session) goal)
      (with-current-buffer view-buf (should (mevedel-menu--goal-editable-p)))
      (setf (mevedel-goal-status goal) 'complete)
      (with-current-buffer view-buf
        (should-not (mevedel-menu--goal-editable-p))))))

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
  :doc "returns only a Goal owned by the current session"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-session-id session) "s1"
          (mevedel-session-goal session)
          (mevedel-goal--create
           :owner-session "s1" :execution-home '(:session-id "s1")))
    (with-current-buffer view-buf (should (mevedel-menu--owned-goal)))
    (setf (mevedel-goal-owner-session (mevedel-session-goal session)) "s2")
    (with-current-buffer view-buf
      (should-not (mevedel-menu--owned-goal)))))

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
  :doc "starts a supervised Goal from prompted input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Ship"))
              ((symbol-function 'mevedel-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (mevedel-menu--goal-start))
    (should (equal (cdr call) '("Ship")))))

(mevedel-deftest mevedel-menu--goal-start-auto ()
  ,test (test)
  :doc "starts an automatic Goal explicitly"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Ship"))
              ((symbol-function 'mevedel-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (mevedel-menu--goal-start-auto))
    (should (equal (cdr call) '("Ship" "Ship" automatic)))))

(mevedel-deftest mevedel-menu--goal-edit ()
  ,test (test)
  :doc "routes edited objective input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "New"))
              ((symbol-function 'mevedel-menu--goal-call)
               (lambda (&rest args) (setq call args)))
              ((symbol-function 'mevedel-menu--current-goal) (lambda () nil)))
      (mevedel-menu--goal-edit))
    (should (equal (cdr call) '("New")))))

(mevedel-deftest mevedel-menu--goal-set-budget ()
  ,test (test)
  :doc "routes parsed token budget input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "500"))
              ((symbol-function 'mevedel-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (mevedel-menu--goal-set-budget))
    (should (equal (cdr call) '(500)))))

(mevedel-deftest mevedel-menu--goal-approval-toggle-description ()
  ,test (test)
  :doc "shows the current and next approval policies"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-goal session)
          (mevedel-goal--create
           :status 'active :approval-policy 'supervised
           :owner-session (mevedel-session-session-id session)
           :execution-home
           (list :session-id (mevedel-session-session-id session))))
    (with-current-buffer view-buf
      (should (equal "Approval: supervised → automatic"
                     (mevedel-menu--goal-approval-toggle-description))))
    (setf (mevedel-goal-approval-policy (mevedel-session-goal session))
          'automatic)
    (with-current-buffer view-buf
      (should (equal "Approval: automatic → supervised"
                     (mevedel-menu--goal-approval-toggle-description))))))

(mevedel-deftest mevedel-menu--goal-toggle-approval-policy ()
  ,test (test)
  :doc "routes the inverse policy in data and preserves a multiline draft"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-goal session)
          (mevedel-goal--create
           :status 'active :approval-policy 'supervised
           :owner-session (mevedel-session-session-id session)
           :execution-home
           (list :session-id (mevedel-session-session-id session))))
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "> draft\nsecond line"))
    (let (called-buffer policy)
      (cl-letf (((symbol-function 'mevedel-goal-set-approval-policy)
                 (lambda (value)
                   (setq called-buffer (current-buffer)
                         policy value))))
        (with-current-buffer view-buf
          (mevedel-menu--goal-toggle-approval-policy)))
      (should (eq data-buf called-buffer))
      (should (eq 'automatic policy))
      (with-current-buffer view-buf
        (should (string-suffix-p "> draft\nsecond line"
                                 (buffer-string)))))))

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
      (should (string= "auto            auto-apply edit previews"
                       (substring-no-properties
                        (mevedel-menu--mode-auto-description))))))

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
  :doc "uses the cockpit view as the permission surface"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (setq-local mevedel-permission-mode 'full-auto)
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
      (mevedel-menu--set-mode 'auto))
    (with-current-buffer data-buf
      (should (eq 'auto
                  (mevedel-session-permission-mode mevedel--session)))
      (should (eq 'auto mevedel-permission-mode))))

  :doc "an explicit permission choice exits Plan mode"
  (mevedel-menu-test--with-buffers
    (setf (mevedel-session-plan-mode session) t)
    (with-current-buffer view-buf
      (mevedel-menu--set-mode 'ask))
    (should-not (mevedel-session-plan-mode session))))

(mevedel-deftest mevedel-menu--model-surface-description ()
  ,test
  (test)
  :doc "model surface description shows current backend and model"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast"))
        (setq-local gptel-model 'fast-model))
      (with-current-buffer view-buf
        (should (string= "Current model: Fast:fast-model"
                         (substring-no-properties
                          (mevedel-menu--model-surface-description))))))))

(mevedel-deftest mevedel-menu--select-model ()
  ,test
  (test)
  :doc "model selector applies the chosen registered provider"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Balanced:balanced-model")))
        (with-current-buffer view-buf
          (mevedel-menu--select-model)))
      (with-current-buffer data-buf
        (should (equal "Balanced" (gptel-backend-name gptel-backend)))
        (should (eq 'balanced-model gptel-model)))))

  :doc "model selector rejects an empty model registry"
  (let ((gptel--known-backends nil))
    (mevedel-menu-test--with-buffers
      (with-current-buffer view-buf
        (should-error (mevedel-menu--select-model) :type 'user-error)))))

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

(provide 'test-mevedel-menu)

;;; test-mevedel-menu.el ends here
