;;; test-niwa-menu.el -- Tests for session cockpit -*- lexical-binding: t -*-

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
(require 'niwa-cockpit)
(require 'niwa-collaboration)
(require 'niwa-collaboration-artifact-room)
(require 'niwa-collaboration-lobby)
(require 'niwa-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-report-test-support"))
(require 'niwa-execution)
(require 'niwa-execution-target)
(require 'niwa-background-list)
(require 'niwa-gptel-bridge)
(require 'niwa-menu)
(require 'niwa-goal)
(require 'niwa-models)
(require 'niwa-mentions)
(require 'niwa-permissions)
(require 'niwa-permissions-list)
(require 'niwa-plugins)
(require 'niwa-presets)
(require 'niwa-remote-artifact)
(require 'niwa-session-persistence)
(require 'niwa-skills-ui)
(require 'niwa-structs)
(require 'niwa-tools)
(require 'niwa-tools-list)
(require 'niwa-view)
(require 'niwa-workspace)
(require 'niwa-worktree)
(require 'niwa-plugin-ui)

;; `gptel'
(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-tools)

;; `transient'
(declare-function transient--emergency-exit "transient" (&optional id))
(defvar transient--buffer-name)
(defvar transient--suffixes)
(defvar transient--transient-map)

(defmacro niwa-menu-test--with-model-backends (&rest body)
  "Run BODY with isolated gptel model backends."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     ,@body))

(defmacro niwa-menu-test--with-buffers (&rest body)
  "Execute BODY with a paired data and view buffer."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-menu-root-" t))
          (niwa-user-dir (file-name-as-directory
                             (make-temp-file "niwa-menu-user-" t)))
          (workspace (niwa-workspace-get-or-create
                      'project (format "menu-%s" root) root "niwa"))
          (session (niwa-session-create "main" workspace))
          (data-buf (generate-new-buffer " *menu-data*"))
          (view-buf (generate-new-buffer " *menu-view*")))
     (unwind-protect
         (progn
           (niwa-gptel-bridge--clear-return-state)
           (niwa-gptel-bridge--cleanup-advice)
           (with-current-buffer data-buf
             (org-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local niwa--session session)
             (setq-local niwa-permission-mode 'ask)
             (setq-local gptel-model 'gpt-5.5)
             (setq-local gptel-tools '(read edit)))
           (niwa-view--setup view-buf data-buf)
           ,@body)
       (niwa-gptel-bridge--clear-return-state)
       (niwa-gptel-bridge--cleanup-advice)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p niwa-user-dir)
         (delete-directory niwa-user-dir t))
       (when (file-directory-p root)
         (delete-directory root t)))))

(niwa-deftest niwa-menu ()
  ,test
  (test)
  :doc "opens the top cockpit from the view buffer"
  (niwa-menu-test--with-buffers
    (let (called-prefix called-buffer)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix
                         called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (call-interactively #'niwa-menu)))
      (should (eq called-prefix 'niwa-menu--top))
      (should (eq called-buffer view-buf))))

  :doc "opens the top cockpit from the paired data buffer"
  (niwa-menu-test--with-buffers
    (let (called-prefix called-buffer)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix
                         called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (call-interactively #'niwa-menu)))
      (should (eq called-prefix 'niwa-menu--top))
      (should (eq called-buffer data-buf)))))

(niwa-deftest niwa-menu-open ()
  ,test
  (test)
  :doc "opens the requested top area"
  (niwa-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (niwa-menu-open 'top)))
      (should (eq called-prefix 'niwa-menu--top))))

  :doc "refuses where nobody can answer a cockpit"
  (niwa-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _) (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (let ((inhibit-interaction t))
            (should-error (niwa-menu-open 'goal) :type 'inhibited-interaction))))
      (should-not called-prefix)))

  :doc "opens requested mode, navigate, model, Goal, and Preset surfaces"
  (niwa-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (dolist (area '((mode . niwa-menu--mode)
                          (navigate . niwa-menu--navigate)
                          (model . niwa-menu--model-selection)
                          (goal . niwa-menu--goal)
                          (preset . niwa-menu--preset)))
            (setq called-prefix nil)
            (niwa-menu-open (car area))
            (should (eq called-prefix (cdr area))))))))

  :doc "top menu never collects stale memory evidence and preserves the composer"
  (niwa-menu-test--with-buffers
    (let* ((collections 0)
           (probe (lambda (&rest _) (cl-incf collections))))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
      (require 'niwa-memory-list)
      (unwind-protect
          (progn
            (advice-add 'niwa-memory-list--collect :before probe)
            (dolist (buffer (list view-buf data-buf))
              (setf (niwa-workspace-memory-observation workspace)
                    '(:at 0 :pending 7 :recovery 1 :unavailable 0))
              (save-window-excursion
                (unwind-protect
                    (with-current-buffer buffer
                      (call-interactively #'niwa-menu)
                      (should (= collections 0))
                      ;; Two alerts fill the row; one leaves room for the marker.
                      (should (string-match-p "7 pending, 1 recovery" (niwa-menu--memory-description)))
                      (setf (niwa-workspace-memory-observation workspace)
                            '(:at 0 :pending 7 :recovery 0 :unavailable 0))
                      (should (string-match-p "7 pending (cached)" (niwa-menu--memory-description))))
                  (transient--emergency-exit))))
            (setf (niwa-workspace-memory-observation workspace) nil)
            (save-window-excursion
              (unwind-protect
                  (with-current-buffer view-buf
                    (call-interactively #'niwa-menu)
                    (should (string-search "not checked" (niwa-menu--memory-description))))
                (transient--emergency-exit)))
            (should (= collections 0))
            (with-current-buffer view-buf
              (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
              (should (= 4 (niwa-view-test--composer-point-offset)))))
        (advice-remove 'niwa-memory-list--collect probe))))

  :doc "opens the permissions cockpit from the owning data buffer"
  (niwa-menu-test--with-buffers
    (let (opened-context opened-buffer)
      (cl-letf (((symbol-function 'niwa-permissions-list-open)
                 (lambda (context)
                   (setq opened-context context
                         opened-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (niwa-menu-open 'permissions)))
      (should (eq (niwa-cockpit-context-session opened-context) session))
      (should (eq opened-buffer data-buf))))

  :doc "opens the session info panel"
  (niwa-menu-test--with-buffers
		  (save-window-excursion
		    (unwind-protect
			(progn
        (with-current-buffer view-buf
			    (niwa-menu-open 'session-info))
			  (with-current-buffer niwa-menu-session-info-buffer-name
			    (should buffer-read-only)
			    (should (string-search "** Request" (buffer-string)))
			    (should (string-search "** Workspace & target" (buffer-string)))
			    (should (string-search "** Persistence" (buffer-string)))
			    (should (string-search "main" (buffer-string)))))
		      (when (get-buffer niwa-menu-session-info-buffer-name)
			(kill-buffer niwa-menu-session-info-buffer-name)))))

  :doc "opens tables in the owning data buffer with the complete caller context"
  (niwa-menu-test--with-buffers
    (dolist (entry '((tools . niwa-tools-list-open)
                     (background . niwa-background-list-open)
                     (artifacts . niwa-artifacts-list-open)
                     (skills . niwa-skills-list-open)
                     (plugins . niwa-plugins-list-open)))
      (let ((opener (cdr entry)) opened-context opened-buffer)
        (cl-letf (((symbol-function opener)
                   (lambda (context)
                     (setq opened-context context
                           opened-buffer (current-buffer)))))
          (with-current-buffer view-buf
            (niwa-menu-open (car entry))))
        (should (eq (niwa-cockpit-context-session opened-context) session))
        (should (eq (niwa-cockpit-context-workspace opened-context)
                    (niwa-session-workspace session)))
        (should (eq (niwa-cockpit-context-view-buffer opened-context) view-buf))
        (should (eq (niwa-cockpit-context-data-buffer opened-context) data-buf))
        (should (eq (niwa-cockpit-context-origin-buffer opened-context) view-buf))
        (should (eq opened-buffer data-buf)))))

  :doc "opens plugins management surface from the paired data buffer"
  (niwa-menu-test--with-buffers
    (let (plugins-context plugins-buffer)
      (cl-letf (((symbol-function 'niwa-plugins-list-open)
                 (lambda (context)
                   (setq plugins-context context
                         plugins-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (niwa-menu-open 'plugins)))
      (should (eq (niwa-cockpit-context-view-buffer plugins-context)
                  view-buf))
      (should (eq (niwa-cockpit-context-data-buffer plugins-context)
                  data-buf))
      (should (eq (niwa-cockpit-context-origin-buffer plugins-context)
                  data-buf))
      (should (eq plugins-buffer data-buf))))

  :doc "opens requested worktree and help cockpit surfaces"
  (niwa-menu-test--with-buffers
    (let (worktree-buffer help-opened)
      (cl-letf (((symbol-function 'niwa-worktree-status-open)
                 (lambda () (setq worktree-buffer (current-buffer))))
                ((symbol-function 'niwa-menu-help-open)
                 (lambda () (setq help-opened t))))
        (with-current-buffer view-buf
          (niwa-menu-open 'worktree)
          (niwa-menu-open 'help)))
      (should (eq worktree-buffer data-buf))
      (should help-opened)))

  :doc "opens the requested gptel bridge area"
  (niwa-menu-test--with-buffers
    (let (called-context)
      (cl-letf (((symbol-function 'niwa-gptel-bridge-open)
                 (lambda (context)
                   (setq called-context context))))
        (with-current-buffer data-buf
          (niwa-menu-open 'gptel)))
      (should (eq (niwa-cockpit-context-data-buffer called-context)
                  data-buf))))

  :doc "signals outside a live view/data pair"
  (with-temp-buffer
    (should-error (niwa-menu-open 'top) :type 'user-error)))

(niwa-deftest niwa-menu--goal-description ()
  ,test
  (test)
  :doc "shows objective, status, turns, and accounting on one line"
  (niwa-menu-test--with-buffers
    (let ((goal
           (niwa-goal--create
            :id "g1" :objective "Ship the feature" :status 'paused
            :token-budget 1000 :tokens-used 400
            :time-used-seconds 12 :turns-run 3
            :reason "Provider credits exhausted"
            :plan-reference "local/plans/accepted.md")))
      (setf (niwa-session-goal session) goal)
      (with-current-buffer view-buf
        (let ((text (substring-no-properties
                     (niwa-menu--goal-description))))
          (dolist (needle '("Ship the feature" "paused" "3 turns"
                            "400/1000 tokens"))
            (should (string-match-p (regexp-quote needle) text)))
          ;; The reason, elapsed time, and plan reference belong to the
          ;; record panel, not to the header.
          (dolist (needle '("Provider credits exhausted" "12s"
                            "local/plans/accepted.md" "\n"))
            (should-not (string-match-p (regexp-quote needle) text)))))))

  :doc "labels incomplete native usage as a lower bound"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-goal session)
          (niwa-goal--create :id "g" :objective "Ship" :status 'budget-limited
                               :turns-run 1 :tokens-used 12 :tokens-incomplete-p t
                               :token-budget 100))
    (with-current-buffer view-buf
      (should (string-search "at least 12 (incomplete)/100 tokens"
                             (niwa-menu--goal-description)))))

  :doc "truncates a long objective so status and accounting fit"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-goal session)
          (niwa-goal--create
           :id "g1" :objective (concat "Implement\n" (make-string 300 ?x))
           :status 'active :turns-run 2 :tokens-used 5))
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'frame-width) (lambda (&rest _) 80)))
        (let ((text (substring-no-properties
                     (niwa-menu--goal-description))))
          (should (<= (string-width text) 80))
          (should (string-match-p "\\`Goal  Implement x+… · active · 2 turns"
                                  text))
          (should (string-suffix-p "5 tokens · unbounded" text))))))

  :doc "shows the empty state with the way to start a Goal"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (string-match-p
               "none — s starts one, or /goal OBJECTIVE"
               (substring-no-properties
                (niwa-menu--goal-description)))))))

(niwa-deftest niwa-menu--goal-record-report ()
  ,test
  (test)
  :doc "shows Goal lifecycle, accounting, and accepted-plan reference"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-goal session)
          (niwa-goal--create
           :id "g1" :objective "Ship the feature" :status 'paused
           :token-budget 1000 :tokens-used 400
           :time-used-seconds 12 :turns-run 3
           :reason "Provider credits exhausted"
           :plan-reference "local/plans/accepted.md"))
    (with-current-buffer view-buf
		    (let ((text (niwa-report-test-text (niwa-menu--goal-record-report))))
        (dolist (needle '("Ship the feature" "paused"
                          "Provider credits exhausted" "400/1000 tokens"
                          "3 · elapsed 12s" "local/plans/accepted.md"))
          (should (string-match-p (regexp-quote needle) text))))))

  :doc "shows an unbounded Goal budget consistently"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-goal session)
          (niwa-goal--create
           :status 'active :tokens-used 3 :turns-run 0
           :time-used-seconds 0))
    (with-current-buffer view-buf
      (should (string-match-p
               "3 tokens · unbounded"
			     (niwa-report-test-text (niwa-menu--goal-record-report))))))

  :doc "explains the empty state"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (string-match-p
               "No active Goal"
			     (niwa-report-test-text (niwa-menu--goal-record-report)))))))

(niwa-deftest niwa-menu--preset-description ()
  ,test
  (test)
  :doc "summarizes the resolved policy counts on one line"
  (niwa-menu-test--with-model-backends
    (niwa-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast")
                    gptel-model 'fast-model
                    gptel-reasoning-effort nil
                    niwa-model-tiers
                    '((fast)
                      (strong :provider "Balanced:balanced-model" :effort high)
                      (strong :provider "Fast:fast-model"))
                    niwa-model-workloads
                    '((planning :provider "Balanced:balanced-model")))
        (setf (niwa-session-preset-name session) 'my-team))
      (with-current-buffer view-buf
        (should
         (equal
          (substring-no-properties (niwa-menu--preset-description))
          "Preset  my-team · 2 tiers · 1 workloads · all resolved")))))

  :doc "names the first broken policy on an alert line"
  (niwa-menu-test--with-model-backends
    (niwa-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast")
                    gptel-model 'fast-model
                    gptel-reasoning-effort nil
                    niwa-model-tiers
                    '((broken :provider "Missing:no-model")
                      (fast))
                    niwa-model-workloads nil)
        (setf (niwa-session-preset-name session) 'broken))
      (with-current-buffer view-buf
        (should
         (equal
          (substring-no-properties (niwa-menu--preset-description))
          (string-join
           '("Preset  broken · 2 tiers · 0 workloads · 1 broken"
             "! tier broken does not resolve — fix before dispatch")
           "\n")))))))

(niwa-deftest niwa-menu--preset-report ()
  ,test
  (test)
  :doc "shows resolved tier and workload policies in configured order"
  (niwa-menu-test--with-model-backends
    (niwa-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast")
                    gptel-model 'fast-model
                    gptel-reasoning-effort nil
                    niwa-model-tiers
                    '((fast)
                      (strong :provider "Balanced:balanced-model" :effort high)
                      (strong :provider "Fast:fast-model"))
                    niwa-model-workloads
                    '((planning :provider "Balanced:balanced-model")))
        (setf (niwa-session-preset-name session) 'my-team))
      (with-current-buffer view-buf
		     (let ((text (niwa-report-test-text (niwa-menu--preset-report))))
		       (dolist (pattern '("my-team" "Tiers" "fast +Fast:fast-model · effort default"
					  "strong +Balanced:balanced-model · effort high"
					  "Workloads" "planning +Balanced:balanced-model · effort default"))
			 (should (string-match-p pattern text)))))))

  :doc "keeps rendering after an invalid tier policy"
  (niwa-menu-test--with-model-backends
    (niwa-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast")
                    gptel-model 'fast-model
                    gptel-reasoning-effort nil
                    niwa-model-tiers
                    '((broken :provider "Missing:no-model")
                      (fast))
                    niwa-model-workloads nil)
        (setf (niwa-session-preset-name session) 'broken))
      (with-current-buffer view-buf
		     (let ((text (niwa-report-test-text (niwa-menu--preset-report))))
		       (should (string-match-p "broken +ERROR:.*Missing" text))
		       (should (string-match-p "fast +Fast:fast-model · effort default" text))
		       (should (string-match-p "Workloads" text)))))))

(niwa-deftest niwa-menu--goal-resumable-p ()
  ,test
  (test)
  :doc "enables resume only for paused and blocked Goals"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-session-id session) "s1")
    (dolist (entry '((active . nil) (paused . t) (blocked . t)
                     (complete . nil)))
      (setf (niwa-session-goal session)
            (niwa-goal--create :status (car entry)))
      (with-current-buffer view-buf
        (should (eq (and (niwa-menu--goal-resumable-p) t)
                    (cdr entry)))))))

(niwa-deftest niwa-menu--current-goal ()
  ,test (test)
  :doc "returns the owning session Goal during an active request"
  (niwa-menu-test--with-buffers
    (let ((goal (niwa-goal--create :id "g1")))
      (setf (niwa-session-goal session) goal)
      (with-current-buffer data-buf
        (setq-local niwa--current-request
                    (niwa-request--create :session session)))
      (with-current-buffer view-buf
        (should (eq goal (niwa-menu--current-goal)))))))

(niwa-deftest niwa-menu--goal-active-p ()
  ,test (test)
  :doc "requires active status"
  (niwa-menu-test--with-buffers
    (let ((goal (niwa-goal--create :status 'active)))
      (setf (niwa-session-goal session) goal)
      (with-current-buffer view-buf (should (niwa-menu--goal-active-p)))
      (setf (niwa-goal-status goal) 'paused)
      (with-current-buffer view-buf
        (should-not (niwa-menu--goal-active-p))))))

(niwa-deftest niwa-menu--goal-start-inapt-p ()
  ,test (test)
  :doc "permits replacement only for absent or complete Goals"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (should-not (niwa-menu--goal-start-inapt-p))
      (setf (niwa-session-goal session)
            (niwa-goal--create :status 'active))
      (should (niwa-menu--goal-start-inapt-p))
      (setf (niwa-goal-status (niwa-session-goal session)) 'complete)
      (should-not (niwa-menu--goal-start-inapt-p)))))

(niwa-deftest niwa-menu--goal-clearable-p ()
  ,test (test)
  :doc "rejects clear while a request is active"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-goal session) (niwa-goal--create :status 'paused))
    (with-current-buffer view-buf (should (niwa-menu--goal-clearable-p)))
    (with-current-buffer data-buf (setq-local niwa--current-request
                  (niwa-request--create :session niwa--session)))
    (with-current-buffer view-buf
      (should-not (niwa-menu--goal-clearable-p)))))

(niwa-deftest niwa-menu--open-goal ()
  ,test (test)
  :doc "routes to the Goal area"
  (let (area)
    (cl-letf (((symbol-function 'niwa-menu-open)
               (lambda (value) (setq area value))))
      (niwa-menu--open-goal))
    (should (eq 'goal area))))

(niwa-deftest niwa-menu--open-preset ()
  ,test (test)
  :doc "routes to the Preset area"
  (let (area)
    (cl-letf (((symbol-function 'niwa-menu-open)
               (lambda (value) (setq area value))))
      (niwa-menu--open-preset))
    (should (eq 'preset area))))

(niwa-deftest niwa-menu--open-preset-report ()
  ,test
  (test)
  :doc "shows the resolved policy report in the preset info panel"
  (niwa-menu-test--with-buffers
    (let (shown-buffer shown-text)
      (cl-letf (((symbol-function 'niwa-cockpit-show-help)
                 (lambda (buffer text)
                   (setq shown-buffer buffer shown-text text))))
        (with-current-buffer view-buf
          (niwa-menu--open-preset-report)))
      (should (equal shown-buffer niwa-menu-preset-report-buffer-name))
		    (let ((text (niwa-report-test-text shown-text)))
		      (should (string-match-p "Preset" text))
		      (should (string-match-p "Tiers" text))))))

(niwa-deftest niwa-menu--open-goal-record ()
  ,test
  (test)
  :doc "shows the Goal record in the Goal info panel"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-goal session)
          (niwa-goal--create
           :objective "Ship it" :status 'active
           :tokens-used 0 :time-used-seconds 0 :turns-run 0))
    (let (shown-buffer shown-text)
      (cl-letf (((symbol-function 'niwa-cockpit-show-help)
                 (lambda (buffer text)
                   (setq shown-buffer buffer shown-text text))))
        (with-current-buffer view-buf
          (niwa-menu--open-goal-record)))
      (should (equal shown-buffer niwa-menu-goal-record-buffer-name))
		    (let ((text (niwa-report-test-text shown-text)))
		      (should (string-match-p "Objective" text))
		      (should (string-match-p "Ship it" text))))))

(niwa-deftest niwa-menu--open-background ()
  ,test (test)
  :doc "routes to the background-work area"
  (let (area)
    (cl-letf (((symbol-function 'niwa-menu-open)
               (lambda (value) (setq area value))))
      (niwa-menu--open-background))
    (should (eq 'background area))))

(niwa-deftest niwa-menu--goal-call ()
  ,test (test)
  :doc "runs lifecycle functions in the data buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (with-current-buffer view-buf
        (niwa-menu--goal-call
         (lambda () (setq called-buffer (current-buffer)))))
      (should (eq data-buf called-buffer)))))

(niwa-deftest niwa-menu--goal-start ()
  ,test (test)
  :doc "starts a Goal from prompted input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Ship"))
              ((symbol-function 'niwa-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (niwa-menu--goal-start))
    (should (equal (cdr call) '("Ship")))))

(niwa-deftest niwa-menu--goal-edit ()
  ,test (test)
  :doc "edits a Goal from prompted input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Revised"))
              ((symbol-function 'niwa-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (niwa-menu--goal-edit))
    (should (equal (cdr call) '("Revised")))))

(niwa-deftest niwa-menu--goal-budget ()
  ,test (test)
  :doc "adjusts a Goal budget from prompted input"
  (let (call)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "12000"))
              ((symbol-function 'niwa-menu--goal-call)
               (lambda (&rest args) (setq call args))))
      (niwa-menu--goal-budget))
    (should (equal (cdr call) '("12000")))))

(niwa-deftest niwa-menu--select-preset (:quiet t)
  ,test
  (test)
  :doc "selects in the owning data buffer only and preserves a multiline draft"
  (niwa-menu-test--with-buffers
    (let* ((other (niwa-session-create "other" workspace))
           (niwa-preset--registry '((team :settings nil)))
           applied-buffer)
      (with-current-buffer view-buf
        (goto-char (point-max))
        (insert "> draft\nsecond line"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "team"))
                ((symbol-function 'niwa-preset-apply)
                 (lambda (name &optional buffer)
                   (setq applied-buffer (or buffer (current-buffer)))
                   (setf (niwa-session-preset-name niwa--session)
                         name))))
        (with-current-buffer view-buf
          (niwa-menu--select-preset)))
      (should (eq applied-buffer data-buf))
      (should (eq 'team (niwa-session-preset-name session)))
      (should-not (niwa-session-preset-name other))
      (with-current-buffer view-buf
        (should (string-suffix-p "> draft\nsecond line"
                                 (buffer-string)))))))

(niwa-deftest niwa-menu--header ()
  ,test
  (test)
  :doc "shows session orientation and idle state on one line"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (let ((header (substring-no-properties (niwa-menu--header))))
        (should (string-match-p "niwa main" header))
        (should (string-match-p "ask" header))
        (should (string-match-p "idle" header))
        ;; A nominal session earns exactly one header line.
        (should (equal "" (nth 1 (split-string header "\n")))))))

  :doc "shows running request state"
  (niwa-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--current-request
                  (niwa-request--create :session niwa--session)))
    (with-current-buffer view-buf
      (should (string-match-p "running" (niwa-menu--header)))))

  :doc "keeps nominal target and durability state out of the header"
  (niwa-menu-test--with-buffers
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (remote-session
            (niwa-session--create
             :name "remote" :workspace workspace
             :working-directory "/ssh:user@host:/srv/project/"
             :execution-target target :permission-mode 'ask)))
      (setf (niwa-execution-target-readiness target)
            '(:status ready :sandbox-mode best-effort
              :sandbox-status bubblewrap)
            (niwa-session-lease remote-session) '(:state owned))
      (with-current-buffer data-buf
        (setq-local niwa--session remote-session))
      (unwind-protect
          (with-current-buffer view-buf
            (let ((header (substring-no-properties (niwa-menu--header))))
              (should (string-match-p "ssh:user@host" header))
              (should-not (string-match-p "supported" header))
              (should-not (string-match-p "Persistence" header))
              (should-not (string-match-p "!" header))))
        ;; Leave the workspace session in place so teardown does not try to
        ;; save the substituted remote session and warn.
        (with-current-buffer data-buf
          (setq-local niwa--session session)))))

  :doc "raises off-nominal target and publication state as an alert line"
  (niwa-menu-test--with-buffers
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (remote-session
            (niwa-session--create
             :name "remote" :workspace workspace
             :working-directory "/ssh:user@host:/srv/project/"
             :execution-target target :permission-mode 'ask)))
      (setf (niwa-execution-target-readiness target)
            '(:status not-probed :sandbox-mode best-effort
              :sandbox-status unavailable)
            (niwa-session-lease remote-session) '(:state owned)
            (niwa-session-pending-publication remote-session)
            '(:reason "remote write failed"))
      (with-current-buffer data-buf
        (setq-local niwa--session remote-session))
      (unwind-protect
          (with-current-buffer view-buf
            (let ((alert (nth 1 (split-string
                                 (substring-no-properties
                                  (niwa-menu--header))
                                 "\n"))))
              (should (string-match-p "target not-probed" alert))
              (should (string-match-p "sandbox unavailable" alert))
              (should (string-match-p "publication pending" alert))))
        ;; The header is what this case is about; leaving the substituted
        ;; session in the buffer would make teardown warn about saving it.
        (setf (niwa-session-pending-publication remote-session) nil)
        (with-current-buffer data-buf
          (setq-local niwa--session session))))))

(niwa-deftest niwa-menu--navigate-description ()
  ,test
  (test)
  :doc "names the live segment when no archived segment is projected"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-current-segment session) 4)
    (with-current-buffer view-buf
      (should (string= "Navigate  main · segment live (4)"
                       (substring-no-properties
                        (niwa-menu--navigate-description))))))

  :doc "names the projected segment and the live total"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-current-segment session) 9)
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'niwa-view-segments-current-number)
                 (lambda () 4)))
        (should (string= "Navigate  main · segment 4/9"
                         (substring-no-properties
                          (niwa-menu--navigate-description))))))))

(niwa-deftest niwa-menu--session-info-report ()
  ,test
  (test)
  :doc "renders complete target and durability state as aligned rows"
  (niwa-menu-test--with-buffers
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (remote-session
            (niwa-session--create
             :name "remote" :workspace workspace
             :working-directory "/ssh:user@host:/srv/project/"
             :execution-target target :permission-mode 'ask)))
      (setf (niwa-execution-target-readiness target)
            '(:status ready :sandbox-mode best-effort
              :sandbox-status bubblewrap)
            (niwa-session-lease remote-session) '(:state owned))
      (with-current-buffer data-buf
        (setq-local niwa--session remote-session))
      (unwind-protect
          (with-current-buffer view-buf
			  (let ((text (niwa-report-test-text
				       (niwa-menu--session-info-report))))
			    (should (string-match-p "Session info\nremote" text))
              (should (string-match-p "Target.*ssh:user@host" text))
			    (should (string-match-p "Support tier +supported\nReadiness +ready" text))
			    (should (string-match-p "Sandbox +bubblewrap" text))
              (should (string-match-p "Persistence" text))
			    (should (string-match-p "Lease +owned\nPublication +published"
                                      text))
			    (should (string-match-p "State +idle" text))
			    (should (string-match-p "Mode +ask" text))
              (should (string-match-p "Model" text))))
        (with-current-buffer data-buf
          (setq-local niwa--session session))))))

(niwa-deftest niwa-menu--worktree-label ()
  ,test
  (test)
  :doc "shows the current branch"
  (niwa-menu-test--with-buffers
    (cl-letf (((symbol-function 'niwa-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "main" (niwa-menu--worktree-label))))))

  :doc "falls back to detached HEAD"
  (niwa-menu-test--with-buffers
    (cl-letf (((symbol-function 'niwa-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "detached abc123"))))
      (with-current-buffer view-buf
        (should (string= "detached abc123"
                         (niwa-menu--worktree-label))))))

  :doc "reports non-Git directories"
  (niwa-menu-test--with-buffers
    (cl-letf (((symbol-function 'niwa-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state not-git :label "not-git"))))
      (with-current-buffer view-buf
        (should (string= "not-git" (niwa-menu--worktree-label)))))))

(niwa-deftest niwa-menu--worktree-description ()
  ,test
  (test)
  :doc "shows worktree description with branch label"
  (niwa-menu-test--with-buffers
    (cl-letf (((symbol-function 'niwa-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "Worktree   main"
                         (substring-no-properties
                          (niwa-menu--worktree-description))))))))

(niwa-deftest niwa-menu--background-description ()
  ,test
  (test)
  :doc "counts live executions and active agents together"
  (niwa-menu-test--with-buffers
    (cl-letf (((symbol-function 'niwa-execution-count-user)
               (lambda (seen-session)
                 (should (eq session seen-session))
                 3))
              ((symbol-function 'niwa-view-agent-active-rows)
               (lambda (seen-session)
                 (should (eq session seen-session))
                 '((:path "/root/a") (:path "/root/b")))))
      (with-current-buffer view-buf
        (should (string= "Background work 5 live"
                         (substring-no-properties
                          (niwa-menu--background-description))))))))

(niwa-deftest niwa-menu--top-descriptions ()
  ,test
  (test)
  :doc "shows padded top-level state rows"
  (niwa-menu-test--with-buffers
    (cl-letf (((symbol-function 'niwa-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "Mode       ask"
                         (substring-no-properties
                          (niwa-menu--mode-description))))
        (should (string= "Model      gpt-5.5"
                         (substring-no-properties
                          (niwa-menu--model-description))))
        (should (string= "Tools      2 active"
                         (substring-no-properties
                          (niwa-menu--tools-description))))
        (should (string-match-p
                 (rx string-start "Skills" (+ space) (+ digit) "/" (+ digit)
                     string-end)
                 (substring-no-properties
                  (niwa-menu--skills-description))))
        (should (string-match-p
                 (rx string-start "Plugins" (+ space) (+ digit) "/" (+ digit)
                     string-end)
                 (substring-no-properties
                  (niwa-menu--plugins-description))))
        (should (string= "Worktree   main"
                         (substring-no-properties
                          (niwa-menu--worktree-description))))
        ;; Nominal states are spelled out but dimmed.
        (dolist (row (list (cons "Preset     none" #'niwa-menu--preset-summary)
                           (cons "Goal       none" #'niwa-menu--goal-summary)
                           (cons "Control    writable" #'niwa-menu--control-summary)
                           (cons "Artifacts  0/0" #'niwa-menu--artifacts-description)))
          (let ((text (funcall (cdr row))))
            (should (string= (car row) (substring-no-properties text)))
            (should (eq 'transient-inactive-value
                        (get-text-property 11 'face text)))))))))

(niwa-deftest niwa-menu--mode-choice-description ()
  ,test
  (test)
  :doc "marks the active mode without exposing internal mode names"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (string=
               (concat "ask        "
                       "prompt for edits and uncertain execution current")
               (substring-no-properties
                (niwa-menu--mode-ask-description))))
      (should (string= "edits      automatic edits and confined execution"
                       (substring-no-properties
                        (niwa-menu--mode-edits-description))))))

  :doc "updates the current marker when the session mode changes"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (niwa-menu--set-mode 'full-auto)
      (should (string=
               (concat "full-auto  "
                       "Full Access, no confinement or prompts   current")
               (substring-no-properties
                (niwa-menu--mode-full-auto-description))))
      (should (string= (concat "ask        "
                               "prompt for edits and uncertain execution")
                       (substring-no-properties
                        (niwa-menu--mode-ask-description))))))

  :doc "marks Plan instead of its underlying permission policy"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-plan-mode session) t
          (niwa-session-permission-mode session) 'full-auto)
    (with-current-buffer view-buf
      (should (string= "Mode       Plan/full-auto"
                       (substring-no-properties
                        (niwa-menu--mode-description))))
      (should (string=
               "Plan mode  on · inspect and discuss, no edits"
               (substring-no-properties
                (niwa-menu--mode-plan-description))))
      (should (string= "full-auto  Full Access, no confinement or prompts"
                       (substring-no-properties
                        (niwa-menu--mode-full-auto-description)))))))

(niwa-deftest niwa-menu--mode-surface-description ()
  ,test
  (test)
  :doc "shows both mode axes on one line"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (string= "Mode  permission ask · Plan off"
                       (substring-no-properties
                        (niwa-menu--mode-surface-description))))))

  :doc "shows Plan as its own axis, not as a permission mode"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-plan-mode session) t
          (niwa-session-permission-mode session) 'full-auto)
    (with-current-buffer view-buf
      (should (string= "Mode  permission full-auto · Plan on"
                       (substring-no-properties
                        (niwa-menu--mode-surface-description)))))))

(niwa-deftest niwa-menu-help--report ()
  ,test
  (test)
  :doc "covers command discovery without duplicating transient keys"
		 (let ((text (niwa-report-test-text (niwa-menu-help--report))))
    (dolist (needle '("Session cockpit"
                      "transient menu is the live key reference"
                      "Slash commands that open UI"
                      "Direct slash commands"
                      "/mode MODE, /model MODEL"
                      "/goal OBJECTIVE, /goal budget N|none"
                      "/ps"
                      "/stop [EXECUTION_ID]"
                      "Modes"
                      "View and data buffers"))
      (should (string-match-p (regexp-quote needle) text)))
    (dolist (stale '("N Next query"
                     "B Previous query"
                     "b Previous display"
                     "approval [supervised|automatic]"))
      (should-not (string-match-p (regexp-quote stale) text)))))

(niwa-deftest niwa-menu--mode-plan-description ()
  ,test
  (test)
  :doc "marks Plan as current without hiding its workflow description"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-plan-mode session) t)
    (with-current-buffer view-buf
      (let ((text (substring-no-properties
                   (niwa-menu--mode-plan-description))))
        (should (string-match-p "Plan" text))
        (should (string-match-p "on ·" text))
        (should (string-match-p "no edits" text))))))

(niwa-deftest niwa-menu--enter-plan ()
  ,test
  (test)
  :doc "enters Plan through the cockpit's authoritative data buffer"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (niwa-menu--enter-plan))
    (should (niwa-session-plan-mode session))))

(niwa-deftest niwa-menu--mode-symbol ()
  ,test
  (test)
  :doc "uses the session's canonical permission mode"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-permission-mode session) 'full-auto)
    (with-current-buffer view-buf
      (should (eq (niwa-menu--mode-symbol
                   session data-buf view-buf)
                  'full-auto))
      (should (string= "Mode       full-auto"
                       (substring-no-properties
                        (niwa-menu--mode-description)))))))

(niwa-deftest niwa-menu--set-mode ()
  ,test
  (test)
  :doc "mode setter updates the paired data buffer session"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (niwa-menu--set-mode 'edits))
    (with-current-buffer data-buf
      (should (eq 'edits
                  (niwa-session-permission-mode niwa--session)))
      (should (eq 'edits niwa-permission-mode))))

  :doc "an explicit permission choice exits Plan mode"
  (niwa-menu-test--with-buffers
    (setf (niwa-session-plan-mode session) t)
    (with-current-buffer view-buf
      (niwa-menu--set-mode 'ask))
    (should-not (niwa-session-plan-mode session))))

(niwa-deftest niwa-menu--model-selection-description ()
  ,test
  (test)
  :doc "describes the shared selection and marks inherited values"
  (let ((scope '(:title "Note model"
                 :model-provider "Fast:fast-model"
                 :reasoning-effort high
                 :inherited t)))
    (cl-letf (((symbol-function 'transient-scope)
               (lambda (&rest _) scope)))
      (should
       (equal "Note model  Fast:fast-model · effort high · session"
              (substring-no-properties
               (niwa-menu--model-selection-description)))))))

(niwa-deftest niwa-menu--refresh-plan-approval ()
  ,test
  (test)
  :doc "rerenders only when the session owns a pending Plan approval"
  (let ((session (niwa-session--create :name "main"))
        rendered)
    (cl-letf (((symbol-function 'niwa-plan-approval-render)
               (lambda (value) (setq rendered value))))
      (niwa-menu--refresh-plan-approval session)
      (should-not rendered)
      (setf (niwa-session-pending-plan-approval session) '(:pending t))
      (niwa-menu--refresh-plan-approval session)
      (should (eq session rendered)))))

(niwa-deftest niwa-menu-open-model-selection ()
  ,test
  (test)
  :doc "opens the shared model-selection surface with caller-owned state"
  (let (prefix scope)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (value &rest args)
                 (setq prefix value
                       scope (plist-get args :scope)))))
      (niwa-menu-open-model-selection
       :title "Note model"
       :provider "Fast:fast-model"
       :effort 'high
       :update #'ignore
       :reset #'identity
       :inherited t))
    (should (eq 'niwa-menu--model-selection prefix))
    (should (equal "Note model" (plist-get scope :title)))
    (should (equal "Fast:fast-model" (plist-get scope :model-provider)))
    (should (eq 'high (plist-get scope :reasoning-effort)))
    (should (eq #'ignore (plist-get scope :update)))
    (should (eq #'identity (plist-get scope :reset)))
    (should (eq t (plist-get scope :inherited))))

  :doc "binds both Return events while advertising only RET"
  (unwind-protect
      (progn
        (niwa-menu-open-model-selection
         :title "Note model"
         :provider "Fast:fast-model"
         :effort nil
         :update #'ignore
         :inherited nil)
        (should
         (eq #'niwa-menu--model-selection-select-model
             (lookup-key transient--transient-map (kbd "RET"))))
        (should
         (eq #'niwa-menu--model-selection-select-model
             (lookup-key transient--transient-map (kbd "<return>"))))
        (let ((display
               (with-current-buffer transient--buffer-name
                 (substring-no-properties (buffer-string)))))
          (should (string-match-p "RET +Choose model" display))
          (should-not (string-match-p "<return>" display))))
    (transient--emergency-exit :test)
    (should-not
     (memq transient--transient-map overriding-terminal-local-map))))

(niwa-deftest niwa-menu--open-model (:quiet t)
  ,test
  (test)
  :doc "opens shared selection and applies updates to the session"
  (niwa-menu-test--with-model-backends
    (niwa-menu-test--with-buffers
      (let (options)
        (with-current-buffer data-buf
          (setq-local gptel-backend (gptel-get-backend "Fast")
                      gptel-model 'fast-model
                      gptel-reasoning-effort 'high))
        (cl-letf (((symbol-function 'niwa-menu-open-model-selection)
                   (lambda (&rest args) (setq options args))))
          (with-current-buffer view-buf
            (niwa-menu--open-model)))
        (should (equal "Session model" (plist-get options :title)))
        (should (equal "Fast:fast-model" (plist-get options :provider)))
        (should (eq 'high (plist-get options :effort)))
        (funcall (plist-get options :update)
                 "Balanced:balanced-model" nil)
        (with-current-buffer data-buf
          (should (eq 'balanced-model gptel-model))
          (should-not gptel-reasoning-effort))
        (should (equal "Balanced:balanced-model"
                       (niwa-session-model-provider session)))
        (should-not (niwa-session-reasoning-effort session))))))

(niwa-deftest niwa-menu--model-selection-select-model ()
  ,test
  (test)
  :doc "updates caller state and resets unsupported effort"
  (niwa-menu-test--with-model-backends
    (niwa-menu-test--with-buffers
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
          (niwa-menu--model-selection-select-model))
        (should (equal '("Balanced:balanced-model" nil) update))
        (should (equal "Balanced:balanced-model"
                       (plist-get scope :model-provider)))
        (should-not (plist-get scope :reasoning-effort))
        (should-not (plist-get scope :inherited))
        (with-current-buffer data-buf
          (should (equal "Fast" (gptel-backend-name gptel-backend)))
          (should (eq 'fast-model gptel-model))
          (should (eq 'high gptel-reasoning-effort)))
        (should-not (niwa-session-model-provider session))))))

(niwa-deftest niwa-menu--model-selection-cycle-effort ()
  ,test
  (test)
  :doc "cycles through the model's supported efforts and back to default"
  (niwa-menu-test--with-model-backends
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
                       (lambda (&rest _) scope)))
              (niwa-menu--model-selection-cycle-effort)
              (should (equal '("Balanced:balanced-model" low) update))
              (should (eq 'low (plist-get scope :reasoning-effort)))
              (should-not (plist-get scope :inherited))
              (niwa-menu--model-selection-cycle-effort)
              (should (eq 'high (plist-get scope :reasoning-effort)))
              ;; The cycle ends on the provider default, not on a wrap
              ;; back to the first supported effort.
              (niwa-menu--model-selection-cycle-effort)
              (should-not (plist-get scope :reasoning-effort))
              (niwa-menu--model-selection-cycle-effort)
              (should (eq 'low (plist-get scope :reasoning-effort)))))
        (put 'balanced-model :reasoning-effort old-effort))))

  :doc "starts the cycle over when the current effort is unsupported"
  (niwa-menu-test--with-model-backends
    (let ((old-effort (get 'balanced-model :reasoning-effort))
          (scope (list :model-provider "Balanced:balanced-model"
                       :reasoning-effort 'max
                       :inherited nil
                       :update #'ignore)))
      (unwind-protect
          (progn
            (put 'balanced-model :reasoning-effort '(member low high))
            (cl-letf (((symbol-function 'transient-scope)
                       (lambda (&rest _) scope)))
              (niwa-menu--model-selection-cycle-effort)
              (should (eq 'low (plist-get scope :reasoning-effort)))))
        (put 'balanced-model :reasoning-effort old-effort)))))

(niwa-deftest niwa-menu--model-selection-effort-description ()
  ,test
  (test)
  :doc "names the current effort and the one the next press selects"
  (niwa-menu-test--with-model-backends
    (let ((old-effort (get 'balanced-model :reasoning-effort))
          (scope '(:model-provider "Balanced:balanced-model"
                   :reasoning-effort high)))
      (unwind-protect
          (progn
            (put 'balanced-model :reasoning-effort '(member low high))
            (cl-letf (((symbol-function 'transient-scope)
                       (lambda (&rest _) scope)))
              (should
               (equal "Cycle effort  high → default"
                      (substring-no-properties
                       (niwa-menu--model-selection-effort-description))))))
        (put 'balanced-model :reasoning-effort old-effort)))))

(niwa-deftest niwa-menu--model-selection-reset ()
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
      (niwa-menu--model-selection-reset))
    (should reset-called)
    (should (equal "Session:session-model"
                   (plist-get scope :model-provider)))
    (should (eq 'low (plist-get scope :reasoning-effort)))
    (should (eq t (plist-get scope :inherited)))))

(niwa-deftest niwa-menu--send ()
  ,test
  (test)
  :doc "send runs in the paired view buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-view-send)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (niwa-menu--send))
        (should (eq called-buffer view-buf))))))

(niwa-deftest niwa-menu--take-control ()
  ,test
  (test)
  :doc "control suffixes run in the paired view buffer"
  ;; A transient fires wherever the cockpit was opened from, which is often
  ;; the data buffer; the control commands resolve their pair from the
  ;; current buffer and would find the wrong half.
  (niwa-menu-test--with-buffers
    (let (called)
      (cl-letf (((symbol-function 'niwa-take-control)
                 (lambda () (interactive) (push (cons 'take (current-buffer))
                                                called)))
                ((symbol-function 'niwa-release-control)
                 (lambda () (interactive) (push (cons 'release (current-buffer))
                                                called)))
                ((symbol-function 'niwa-toggle-follow)
                 (lambda () (interactive) (push (cons 'follow (current-buffer))
                                                called)))
                ((symbol-function 'niwa-refresh-session)
                 (lambda () (interactive) (push (cons 'refresh (current-buffer))
                                                called)))
                ((symbol-function 'niwa-view-control-transfer-grant)
                 (lambda () (interactive) (push (cons 'grant (current-buffer))
                                                called)))
                ((symbol-function 'niwa-view-control-transfer-keep)
                 (lambda () (interactive) (push (cons 'keep (current-buffer))
                                                called))))
        (with-current-buffer data-buf
          (niwa-menu--take-control)
          (niwa-menu--release-control)
          (niwa-menu--grant-control)
          (niwa-menu--keep-control)
          (niwa-menu--toggle-follow)
          (niwa-menu--refresh-session))
        (should (= 6 (length called)))
        (should (equal '(take release grant keep follow refresh)
                       (mapcar #'car (reverse called))))
        (should (cl-every (lambda (entry) (eq view-buf (cdr entry)))
                          called))))))

(niwa-deftest niwa-menu--abort ()
  ,test
  (test)
  :doc "abort runs in the paired view buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-view-abort)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (niwa-menu--abort))
        (should (eq called-buffer view-buf))))))

(niwa-deftest niwa-menu--rewind-here ()
  ,test
  (test)
  :doc "rewind-at-point runs in the paired view buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-view-rewind-at-point)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (niwa-menu--rewind-here))
        (should (eq called-buffer view-buf))))))

(niwa-deftest niwa-menu--redo ()
  ,test
  (test)
  :doc "redo runs against the session's data buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-redo)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (niwa-menu--redo))
        (should (eq called-buffer data-buf))))))

(niwa-deftest niwa-menu--redo-inapt-p ()
  ,test
  (test)
  :doc "offers redo only where a session publishes restorable heads"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (setf (niwa-session-authority-mode session) 'portable)
      (should-not (niwa-menu--redo-inapt-p))
      ;; A workspace-free session has no durable heads to restore.
      (setf (niwa-session-authority-mode session) nil)
      (should (niwa-menu--redo-inapt-p)))))

(niwa-deftest niwa-menu--switch-variant-here ()
  ,test
  (test)
  :doc "variant switching at point runs in the paired view buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf
          (((symbol-function
             'niwa-view-switch-conversation-variant-at-point)
            (lambda ()
              (interactive)
              (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (niwa-menu--switch-variant-here))
        (should (eq called-buffer view-buf))))))

(niwa-deftest niwa-menu--fork-conversation-here ()
  ,test
  (test)
  :doc "Conversation Fork arming runs in the paired view buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-view-arm-conversation-fork)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (niwa-menu--fork-conversation-here))
        (should (eq called-buffer view-buf))))))

(niwa-deftest niwa-menu--fork-worktree-here ()
  ,test
  (test)
  :doc "Worktree Fork arming runs in the paired view buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-view-arm-worktree-fork)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (niwa-menu--fork-worktree-here))
        (should (eq called-buffer view-buf))))))

(niwa-deftest niwa-menu--call-live-tip-data ()
  ,test
  (test)
  :doc "refuses data actions while the paired view inspects history"
  (niwa-menu-test--with-buffers
    (let (called)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-view-historical-segment-p)
                   (lambda () t)))
          (should-error
           (niwa-menu--call-live-tip-data
            (lambda () (setq called t)))
           :type 'user-error)))
      (should-not called))))

(niwa-deftest niwa-menu--compact ()
  ,test
  (test)
  :doc "compact runs in the paired data buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-compact)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (niwa-menu--compact))
        (should (eq called-buffer data-buf))))))

(niwa-deftest niwa-menu--review ()
  ,test
  (test)
  :doc "review runs in the paired data buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-review)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (niwa-menu--review))
        (should (eq called-buffer data-buf))))))

(niwa-deftest niwa-menu--verify ()
  ,test
  (test)
  :doc "verify runs in the paired data buffer"
  (niwa-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'niwa-verify)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (niwa-menu--verify))
        (should (eq called-buffer data-buf))))))

(niwa-deftest niwa-menu--toggle-data-view ()
  ,test
  (test)
  :doc "toggle data/view switches both directions"
  (niwa-menu-test--with-buffers
    (switch-to-buffer view-buf)
    (niwa-menu--toggle-data-view)
    (should (eq (window-buffer (selected-window)) data-buf))
    (with-current-buffer data-buf
      (niwa-menu--toggle-data-view))
    (should (eq (window-buffer (selected-window)) view-buf))))

(niwa-deftest niwa-menu-help-open
  (:after-each (when (get-buffer niwa-menu-help-buffer-name)
                 (kill-buffer niwa-menu-help-buffer-name)))
  ,test
  (test)
  :doc "opens the help surface buffer"
  (let ((buffer (niwa-menu-help-open)))
    (with-current-buffer buffer
      (should (derived-mode-p 'special-mode)))))

(niwa-deftest niwa-menu--open-gptel ()
  ,test
  (test)
  :doc "delegates to the gptel bridge with the current cockpit context"
  (niwa-menu-test--with-buffers
    (let (called-context)
      (cl-letf (((symbol-function 'niwa-gptel-bridge-open)
                 (lambda (context)
                   (setq called-context context))))
        (with-current-buffer view-buf
          (niwa-menu--open-gptel)))
      (should (eq (niwa-cockpit-context-view-buffer called-context)
                  view-buf))
      (should (eq (niwa-cockpit-context-data-buffer called-context)
                  data-buf)))))

(niwa-deftest niwa-menu--send-inapt-p ()
  ,test
  (test)
  :doc "idle session can send"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (should-not (niwa-menu--send-inapt-p))))

  :doc "running session cannot send"
  (niwa-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--current-request
                  (niwa-request--create :session niwa--session)))
    (with-current-buffer view-buf
      (should (niwa-menu--send-inapt-p))))

  :doc "settling session cannot send"
  (niwa-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--turn-settlements-pending '(settlement)))
    (with-current-buffer view-buf
      (should (niwa-menu--send-inapt-p)))))

(niwa-deftest niwa-menu--abort-inapt-p ()
  ,test
  (test)
  :doc "idle session cannot abort"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (niwa-menu--abort-inapt-p))))

  :doc "running session can abort"
  (niwa-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--current-request
                  (niwa-request--create :session niwa--session)))
    (with-current-buffer view-buf
      (should-not (niwa-menu--abort-inapt-p)))))

(defconst niwa-menu-test--top-keys
  '("RET" "a" "c" "r" "v" "N" "f" "F" "R" "D" "B" "m" "M" "P" "u" "G"
    "x" "w" "A" "l" "S" "n" "C" "t" "s" "p" "d" "i" "U" "g" "?")
  "Every key of the top cockpit.")

(niwa-deftest niwa-menu--layout ()
  ,test
  (test)
  :doc "chooses the arrangement from the frame width"
  (dolist (case '((240 . wide) (170 . wide) (169 . medium) (100 . medium)
                  (99 . narrow) (60 . narrow)))
    (cl-letf (((symbol-function 'frame-width) (lambda (&rest _) (car case))))
      (should (eq (cdr case) (niwa-menu--layout)))
      (should (eq (< (car case) niwa-menu--footer-width)
                  (niwa-menu--split-footer-p))))))

(niwa-deftest niwa-menu--fit ()
  ,test
  (test)
  :doc "shortens long row values so the layouts keep their widths"
  (should (equal "gpt-5.5" (niwa-menu--fit "gpt-5.5")))
  (should (equal (make-string niwa-menu--value-width ?x)
                 (niwa-menu--fit (make-string niwa-menu--value-width ?x))))
  (let ((long (niwa-menu--fit "claude-opus-4-5-20251101-long-model-name")))
    (should (= niwa-menu--value-width (string-width long)))
    (should (string-suffix-p "…" long)))
  (niwa-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local gptel-model 'claude-opus-4-5-20251101-long-model-name))
    (with-current-buffer view-buf
      (should (string-suffix-p "…" (substring-no-properties
                                    (niwa-menu--model-description)))))))

(niwa-deftest niwa-menu--top ()
  ,test
  (test)
  :doc "binds every key once in each layout and keeps utilities on their own rows"
  (niwa-menu-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 4))
    (dolist (width '(200 120 80 60))
      (cl-letf (((symbol-function 'frame-width) (lambda (&rest _) width))
                ;; Opening the menu shares nothing and starts no editor.
                ((symbol-function 'niwa-collaboration--start)
                 (lambda (&rest _) (ert-fail "Menu started a share")))
                ((symbol-function 'niwa-collaboration-lobby--credentials)
                 (lambda (&rest _) (ert-fail "Menu read lobby credentials")))
                ((symbol-function 'niwa-shared-editing--process)
                 (lambda (&rest _) (ert-fail "Menu started the editing helper"))))
        (save-window-excursion
          (unwind-protect
              (with-current-buffer view-buf
                (call-interactively #'niwa-menu)
                (let ((keys (mapcar (lambda (suffix) (oref suffix key))
                                    transient--suffixes)))
                  (ert-info ((format "width %d keys %S" width keys))
                    (dolist (key niwa-menu-test--top-keys)
                      (should (= 1 (cl-count key keys :test #'equal))))))
                (let ((lines (split-string
                              (with-current-buffer transient--buffer-name
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))
                              "\n")))
                  (ert-info ((format "width %d menu:\n%s" width
                                     (string-join lines "\n")))
                    ;; Groups stack instead of overflowing narrower frames.
                    (should (cl-find-if (lambda (line)
                                          (string-match-p
                                           "Conversation.*History" line))
                                        lines))
                    (should (eq (>= width 100)
                                (and (cl-find-if (lambda (line)
                                                   (string-match-p
                                                    "History.*Configure" line))
                                                 lines)
                                     t)))
                    (should (eq (>= width 170)
                                (and (cl-find-if (lambda (line)
                                                   (string-match-p "Configure.*Extend" line))
                                                 lines)
                                     t)))
                    (if (< width niwa-menu--footer-width)
                        (progn
                          (should (cl-find-if (lambda (line)
                                                (string-match-p "Data view.*Usage" line))
                                              lines))
                          (should-not (cl-find-if (lambda (line)
                                                    (string-match-p "Usage.*Help" line))
                                                  lines)))
                      (should (cl-find-if (lambda (line)
                                            (string-match-p
                                             "Data view.*Session info.*Usage.*gptel menu.*Help"
                                             line))
                                          lines))))))
            (transient--emergency-exit)))))
    (with-current-buffer view-buf
      (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
      (should (= 4 (niwa-view-test--composer-point-offset))))))

(niwa-deftest niwa-menu--share-summary ()
  ,test
  (test)
  :doc "shows the session's sharing state and guest count without its links"
  (niwa-menu-test--with-buffers
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer data-buf :guests guests
                       :link-full "https://relay.example/#room.full-secret"))
           (niwa-collaboration--rooms (niwa-test-room-registry))
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal)))
      (with-current-buffer view-buf
        (should (string= "Share…     not shared"
                         (substring-no-properties (niwa-menu--share-summary))))
        (should (eq 'transient-inactive-value
                    (get-text-property 11 'face (niwa-menu--share-summary))))
        (should (niwa-menu--not-shared-p))
        (should (niwa-menu--lobby-down-p))
        (puthash data-buf room niwa-collaboration--rooms)
        (puthash 1 (list :name "Phone" :guest-id "a") guests)
        (should (string= "Share…     shared · 1 guest"
                         (substring-no-properties (niwa-menu--share-summary))))
        (puthash (niwa-workspace-root workspace) (list :project "p")
                 niwa-collaboration-lobby--lobbies)
        (let ((header (substring-no-properties (niwa-menu--share-description))))
          (should (string-match-p "session shared · 1 guest · lobby shared" header))
          (should-not (string-search "secret" header)))
        (should-not (niwa-menu--not-shared-p))
        (should-not (niwa-menu--lobby-down-p)))))

  :doc "counts shares in other sessions and workspaces, and lights up for them"
  (niwa-menu-test--with-buffers
    (let* ((other (generate-new-buffer " *menu-other*"))
           (niwa-collaboration--rooms
            (niwa-test-room-registry
             (list :data-buffer other :session-label "Other"
                   :guests (make-hash-table :test #'eql))))
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal)))
      (unwind-protect
          (with-current-buffer view-buf
            (should (string= "Share…     not shared · +1 more"
                             (substring-no-properties
                              (niwa-menu--share-summary))))
            (should (eq 'transient-value
                        (get-text-property 11 'face (niwa-menu--share-summary))))
            ;; This workspace's own lobby is not "elsewhere".
            (puthash (niwa-workspace-root workspace)
                     (list :project "p" :guests (make-hash-table :test #'eql))
                     niwa-collaboration-lobby--lobbies)
            (should (string= "Share…     not shared · +1 more"
                             (substring-no-properties
                              (niwa-menu--share-summary)))))
        (kill-buffer other)))))

(niwa-deftest niwa-menu--share-active-rows ()
  ,test
  (test)
  :doc "lists this session's share first, one digit per share, without links"
  (niwa-menu-test--with-buffers
    (let* ((other (generate-new-buffer " *menu-other*"))
           (mine-guests (make-hash-table :test #'eql))
           (niwa-collaboration--rooms
            (niwa-test-room-registry
             (list :data-buffer other :session-label "Other session"
                   :guests (make-hash-table :test #'eql)
                   :link-full "https://relay.example/#room.full-secret")
             (list :data-buffer data-buf :session-label "This session"
                   :guests mine-guests)))
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (niwa-menu--share-entries nil))
      (unwind-protect
          (with-current-buffer view-buf
            (puthash 1 (list :guest-id "a") mine-guests)
            (niwa-collaboration-lobby--set-intended "/srv/excal/" t)
            (let ((rows (niwa-menu--share-active-rows nil)))
              (should (equal '("1" "2" "3")
                             (mapcar (lambda (row) (plist-get (cdr row) :key))
                                     rows)))
              (should (equal '(niwa-menu--share-select)
                             (seq-uniq (mapcar (lambda (row)
                                                 (plist-get (cdr row) :command))
                                               rows))))
              (let ((labels (mapcar (lambda (row)
                                      (substring-no-properties
                                       (plist-get (cdr row) :description)))
                                    rows)))
                (should (string-match-p
                         "\\`● room +This session +1 guest · reconnecting · this session\\'"
                         (nth 0 labels)))
                (should (string-match-p "\\`● room +Other session +0 guests" (nth 1 labels)))
                (should (string-match-p "\\`○ lobby +excal +recorded, not running\\'"
                                        (nth 2 labels)))
                (should-not (seq-some (lambda (label) (string-search "secret" label))
                                      labels)))
              (should (eq data-buf (plist-get (car niwa-menu--share-entries)
                                              :data-buffer)))))
        (kill-buffer other)))))

(niwa-deftest niwa-menu--share-select ()
  ,test
  (test)
  :doc "opens the share surface for the pressed digit's share"
  (let ((niwa-menu--share-entries (list (list :kind 'room :label "a")
                                           (list :kind 'lobby :label "b")))
        scoped)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (prefix _layout _edit &rest args)
                 (setq scoped (list prefix (plist-get args :scope))))))
      (let ((last-command-event ?2))
        (niwa-menu--share-select))
      (should (equal (list 'niwa-menu--share-item
                           (list :kind 'lobby :label "b"))
                     scoped))
      (let ((last-command-event ?3))
        (should-error (niwa-menu--share-select) :type 'user-error)))))

(niwa-deftest niwa-menu--share-choose ()
  ,test
  (test)
  :doc "offers every share with unique full labels and routes beyond the ninth row"
  (niwa-menu-test--with-buffers
    (let* ((label "Two identically named sessions with a long title")
           (entries (cl-loop repeat 11 collect (list :kind 'room :label label)))
           (recorded (list :kind 'recorded-lobby :label "Project"
                           :directory "/srv/project/"))
           scoped)
      (setq entries (append entries (list recorded)))
      (cl-letf (((symbol-function 'niwa-menu--shares) (lambda () entries))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices _predicate require-match &rest _)
                   (should require-match)
                   (should (= 12 (length choices)))
                   (should (= 12 (length (seq-uniq (mapcar #'car choices)))))
                   (should (string-search label (caar choices)))
                   (should (string-search "/srv/project/" (car (nth 11 choices))))
                   (car (nth 11 choices))))
                ((symbol-function 'transient-setup)
                 (lambda (prefix _layout _edit &rest args)
                   (setq scoped (list prefix (plist-get args :scope))))))
        (with-current-buffer view-buf
          (should (string-search "a chooses any share"
                                 (niwa-menu--share-active-description)))
          (niwa-menu--share-choose)
          (should (eq 'niwa-menu--share-item (car scoped)))
          (dolist (key '(:kind :label :directory))
            (should (equal (plist-get recorded key)
                           (plist-get (cadr scoped) key))))
          (should (eq view-buf
                      (plist-get (plist-get (cadr scoped) :niwa-context)
                                 :origin-buffer)))
          (setq entries nil)
          (should-error (niwa-menu--share-choose) :type 'user-error))))))

(niwa-deftest niwa-menu--share-item ()
  ,test
  (test)
  :doc "routes the chosen share's actions to the collaboration module"
  (let* ((buffer (generate-new-buffer " *share-item*"))
         (entry (list :kind 'room :label "Review" :guests 2 :connected t
                      :data-buffer buffer))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) entry))
                  ((symbol-function 'niwa-collaboration-show-share)
                   (lambda (shown) (push (list 'show shown) calls)))
                  ((symbol-function 'niwa-collaboration-stop-share)
                   (lambda (shown) (push (list 'stop shown) calls)))
                  ((symbol-function 'niwa--display-chat-buffer)
                   (lambda (shown) (push (list 'open shown) calls))))
          (should (string-match-p "room Review\n2 guests connected · relay connected"
                                  (substring-no-properties
                                   (niwa-menu--share-item-description))))
          (should (equal "Stop sharing" (niwa-menu--share-item-stop-description)))
          (niwa-menu--share-item-open)
          (niwa-menu--share-item-links)
          (niwa-menu--share-item-stop)
          (should (equal (list (list 'open buffer) (list 'show entry)
                               (list 'stop entry))
                         (nreverse calls)))
          (setq entry (list :kind 'recorded-lobby :label "excal"
                            :directory "/srv/excal/"))
          (should (niwa-menu--share-item-recorded-p))
          (should-not (niwa-menu--share-item-room-p))
          (should (string-match-p "Forget" (niwa-menu--share-item-stop-description)))
          (should-error (niwa-menu--share-item-open) :type 'user-error))
      (kill-buffer buffer))))

(niwa-deftest niwa-menu--share-stop-all ()
  ,test
  (test)
  :doc "stops everything only after a confirmation that counts it"
  (niwa-menu-test--with-buffers
    (let* ((niwa-collaboration--rooms
            (niwa-test-room-registry
             (list :data-buffer data-buf :session-label "s"
                   :guests (make-hash-table :test #'eql))))
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           stopped messages prompt)
      (cl-letf (((symbol-function 'niwa-collaboration-stop-all)
                 (lambda () (setq stopped t))))
        (with-current-buffer view-buf
          (should-not (niwa-menu--no-shares-p))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (text) (setq prompt text) nil)))
            (niwa-test--with-captured-messages messages
              (niwa-menu--share-stop-all)))
          (should (string-match-p "Stop 1 share and 0 lobbies" prompt))
          (should-not stopped)
          (should (string-match-p "shares kept" messages))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (niwa-menu--share-stop-all))
          (should stopped)
          (clrhash niwa-collaboration--rooms)
          (should (niwa-menu--no-shares-p)))))))

(niwa-deftest niwa-menu--share ()
  ,test
  (test)
  :doc "opens from the top menu and routes each action to its owner"
  (niwa-menu-test--with-buffers
    (let (calls)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _) (push (list 'setup prefix) calls)))
                ((symbol-function 'niwa-collaboration-view)
                 (lambda () (push (list 'view (current-buffer)) calls)))
                ((symbol-function 'niwa-collaboration-stop-session)
                 (lambda (buffer) (push (list 'stop buffer) calls)))
                ((symbol-function 'niwa-collaboration-lobby)
                 (lambda () (push (list 'lobby (current-buffer)) calls)))
                ((symbol-function 'niwa-collaboration-lobby-stop)
                 (lambda () (push (list 'lobby-stop (current-buffer)) calls)))
                ((symbol-function 'niwa-collaboration-lobby-rotate)
                 (lambda () (push (list 'rotate (current-buffer)) calls))))
        (with-current-buffer view-buf
          (niwa-menu-open 'share)
          (niwa-menu--share-session)
          (niwa-menu--share-stop)
          (niwa-menu--share-lobby)
          (niwa-menu--share-lobby-stop)))
      (should (equal (list (list 'setup 'niwa-menu--share)
                           (list 'view view-buf)
                           (list 'stop data-buf)
                           (list 'lobby data-buf)
                           (list 'lobby-stop data-buf))
                     (nreverse calls)))))

  :doc "rotates the lobby links only after confirmation"
  (niwa-menu-test--with-buffers
    (let (rotated messages)
      (cl-letf (((symbol-function 'niwa-collaboration-lobby-rotate)
                 (lambda () (setq rotated (current-buffer)))))
        (with-current-buffer view-buf
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (niwa-test--with-captured-messages messages
              (niwa-menu--share-lobby-rotate)))
          (should-not rotated)
          (should (string-match-p "lobby links kept" messages))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (should (string-match-p "stops working" prompt))
                       t)))
            (niwa-menu--share-lobby-rotate))
          (should (eq data-buf rotated))))))

  :doc "runs the artifact sharing and remote artifact commands in the session's workspace"
  (niwa-menu-test--with-buffers
    (let (calls)
      (cl-letf (((symbol-function 'niwa-collaboration-share-artifact)
                 (lambda () (interactive) (push (list 'share (current-buffer)) calls)))
                ((symbol-function 'niwa-collaboration-rotate-artifact)
                 (lambda () (interactive) (push (list 'rotate (current-buffer)) calls)))
                ((symbol-function 'niwa-collaboration-revoke-artifact)
                 (lambda () (interactive) (push (list 'revoke (current-buffer)) calls)))
                ((symbol-function 'niwa-remote-artifact-attach)
                 (lambda () (interactive) (push (list 'attach (current-buffer)) calls)))
                ((symbol-function 'niwa-remote-artifact-detach)
                 (lambda () (interactive) (push (list 'detach (current-buffer)) calls))))
        (with-current-buffer view-buf
          (niwa-menu--share-artifact)
          (niwa-menu--rotate-artifact)
          (niwa-menu--revoke-artifact)
          (niwa-menu--attach-remote-artifact)
          (niwa-menu--detach-remote-artifact)))
      (should (equal (list (list 'share data-buf) (list 'rotate data-buf)
                           (list 'revoke data-buf) (list 'attach data-buf)
                           (list 'detach data-buf))
                     (nreverse calls))))))

(defun niwa-menu-test--window-layout ()
  "Return window topology, buffers and selection for the selected frame."
  (list (window-tree)
        (mapcar (lambda (window)
                  (list window (window-buffer window) (window-edges window)))
                (window-list nil 'no-minibuf))
        (selected-window)))

(niwa-deftest niwa-menu--setup (:quiet t)
  ,test
  (test)

  :doc "command-loop menu, cockpit return and final quit restore the layout"
  (niwa-menu-test--with-buffers
    (save-window-excursion
      (let ((original-global-map (current-global-map))
            (global-map (copy-keymap (current-global-map)))
            (transient-show-popup t))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer view-buf)
              (with-current-buffer data-buf (setq-local gptel-tools nil))
              (set-window-buffer (split-window-right) data-buf)
              (define-key global-map (kbd "<f12>") #'niwa-menu)
              (use-global-map global-map)
              (let ((before (niwa-menu-test--window-layout)))
                (execute-kbd-macro (kbd "<f12>"))
                (ert-info ("after opening menu") (should transient--prefix))
                (should (eq (oref transient--prefix command) 'niwa-menu--top))
                (execute-kbd-macro (kbd "t"))
                (should (derived-mode-p 'niwa-tools-list-mode))
                (execute-kbd-macro (kbd "q"))
                (ert-info ("after quitting cockpit") (should transient--prefix))
                (should (eq (oref transient--prefix command) 'niwa-menu--top))
                (execute-kbd-macro (kbd "C-g"))
                (should-not transient--prefix)
                (should (equal before (niwa-menu-test--window-layout)))))
          (transient--emergency-exit)
          (use-global-map original-global-map)
          (when (get-buffer niwa-tools-list-buffer-name)
            (kill-buffer niwa-tools-list-buffer-name))))))

  :doc "menu dismissal preserves data and view buffers even with same-window rules"
  (niwa-menu-test--with-buffers
    (save-window-excursion
      (let ((original-global-map (current-global-map))
            (global-map (copy-keymap (current-global-map)))
            (transient-show-popup t)
            (transient-display-buffer-action '(display-buffer-same-window)))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer data-buf)
              (display-buffer-in-side-window
               view-buf '((side . bottom) (slot . 0) (window-height . 5)))
              (define-key global-map (kbd "<f12>") #'niwa-menu)
              (use-global-map global-map)
              (let ((before (niwa-menu-test--window-layout)))
                (execute-kbd-macro (kbd "<f12> C-g"))
                (should-not transient--prefix)
                (should (equal before (niwa-menu-test--window-layout)))))
          (transient--emergency-exit)
          (use-global-map original-global-map)))))

  :doc "user side-window rules and slot limits cannot consume existing windows"
  (niwa-menu-test--with-buffers
    (save-window-excursion
      (let ((original-global-map (current-global-map))
            (map (copy-keymap (current-global-map)))
            (transient-show-popup t)
            (display-buffer-alist
             '(("\\` \\*transient\\*" (display-buffer-in-side-window)
                (side . bottom) (slot . 0))))
            (window-sides-slots '(nil nil nil 1)))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer data-buf)
              (display-buffer-in-side-window
               view-buf '((side . bottom) (slot . 0) (window-height . 5)))
              (define-key map (kbd "<f12>") #'niwa-menu)
              (use-global-map map)
              (let ((before (niwa-menu-test--window-layout)))
                (execute-kbd-macro (kbd "<f12> C-g"))
                (should-not transient--prefix)
                (should (equal before (niwa-menu-test--window-layout)))))
          (transient--emergency-exit)
          (use-global-map original-global-map)))))

  :doc "worktree status and its report return only along their explicit menu path"
  (niwa-menu-test--with-buffers
    (save-window-excursion
      (let ((original-global-map (current-global-map))
            (map (copy-keymap (current-global-map)))
            (transient-show-popup t))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer view-buf)
              (define-key map (kbd "<f12>") #'niwa-menu)
              (define-key map (kbd "<f11>") #'niwa-worktree-status-open)
              (use-global-map map)
              (let ((before (niwa-menu-test--window-layout)))
                (execute-kbd-macro (kbd "<f11>"))
                (should (eq (oref transient--prefix command) 'niwa-worktree-status))
                (execute-kbd-macro (kbd "q"))
                (should-not transient--prefix)
                (should (equal before (niwa-menu-test--window-layout)))
                (execute-kbd-macro (kbd "<f12> w i"))
                (should (derived-mode-p 'niwa-report-mode))
                (execute-kbd-macro (kbd "q"))
                (should (eq (oref transient--prefix command) 'niwa-worktree-status))
                (execute-kbd-macro (kbd "q"))
                (should (eq (oref transient--prefix command) 'niwa-menu--top))
                (execute-kbd-macro (kbd "C-g"))
                (should-not transient--prefix)
                (should (equal before (niwa-menu-test--window-layout)))))
          (transient--emergency-exit)
          (use-global-map original-global-map)
          (when (get-buffer niwa-worktree-details-buffer-name)
            (kill-buffer niwa-worktree-details-buffer-name)))))))

(provide 'test-niwa-menu)

;;; test-niwa-menu.el ends here
