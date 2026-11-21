;;; test-niwa-goal.el --- Tests for niwa-goal.el -*- lexical-binding: t -*-

;;; Commentary:

;; Goal record, continuation, settlement, and command tests.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))
(require 'gptel-request)
(require 'niwa)
(require 'niwa-goal)
(require 'niwa-session-persistence)
(require 'niwa-tool-goal)
(require 'niwa-tools)
(require 'niwa-pending-inputs)
(require 'niwa-plan)
(require 'niwa-view-composer)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-goal-create
  ()
  ,test
  (test)
  :doc "restores the prior Goal when persistence fails"
  (let* ((previous (niwa-goal--create :id "previous" :status 'complete))
         (session (niwa-session--create :name "main" :goal previous)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest _) (error "Disk full"))))
        (should (equal "Disk full"
                       (error-message-string
                        (should-error
                         (niwa-goal-create
                          "Ship" :token-budget 50
                          :implementation-policy
                          '(:model-provider "Unavailable:implementer"
                            :reasoning-effort high :permission-mode edits)))))))
      (should (eq previous (niwa-session-goal session)))))

  :doc "preserves newer public Goal controls when the older constructor save fails"
  (dolist (control '(clear replace pause edit budget))
    (ert-info ((format "Public control during save: %s" control))
      (let* ((previous (niwa-goal--create :id "previous" :status 'complete))
             (session (niwa-session--create :name "main" :goal previous))
             (policy '(:model-provider "Unavailable:implementer"
                       :reasoning-effort high :permission-mode edits))
             (saves 0)
             provisional newer)
        (with-temp-buffer
          (setq-local niwa--session session niwa-goal-token-budget 90)
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "now"))
                    ((symbol-function 'niwa-goal-new-id)
                     (lambda () "edited"))
                    ((symbol-function 'niwa-goal-schedule-continuation) #'ignore)
                    ((symbol-function 'niwa-session-artifacts-save)
                     (lambda (saved-session saved-buffer &rest _)
                       (should (eq session saved-session))
                       (should (eq (current-buffer) saved-buffer))
                       (cl-incf saves)
                       ;; Only the older save fails; nested public saves succeed.
                       (when (= saves 1)
                         (setq provisional (niwa-session-goal session))
                         (pcase control
                           ('clear (niwa-goal-clear))
                           ('replace
                            (niwa-goal-clear)
                            (setq newer
                                  (niwa-goal-create
                                   "Ship" :id "provisional" :token-budget 50
                                   :implementation-policy policy))
                            ;; Equal-shaped replacement still has a newer owner.
                            (should (equal provisional newer))
                            (should-not (eq provisional newer)))
                           ('pause (setq newer (niwa-goal-pause)))
                           ('edit (setq newer (niwa-goal-edit "Revised")))
                           ('budget (setq newer (niwa-goal-set-budget 75))))
                         (error "Disk full")))))
            (should (equal "Disk full"
                           (error-message-string
                            (should-error
                             (niwa-goal-create
                              "Ship" :id "provisional" :token-budget 50
                              :implementation-policy policy)))))
            (should (= saves (if (eq control 'replace) 3 2)))
            (should (eq newer (niwa-session-goal session)))
            (should-not (eq previous (niwa-session-goal session)))
            (pcase control
              ('clear (should-not (niwa-session-goal session)))
              ('replace (should-not (eq provisional newer)))
              (_ (should (eq provisional newer))))
            (when newer
              (should (equal "now" (niwa-goal-updated-at newer)))
              (should (equal policy (niwa-goal-implementation-policy newer)))
              (should (= (if (eq control 'budget) 75 50)
                         (niwa-goal-token-budget newer)))
              (should (eq (if (eq control 'pause) 'paused 'active)
                          (niwa-goal-status newer)))
              (should (equal (and (eq control 'pause) "paused by user")
                             (niwa-goal-reason newer)))
              (should (equal (if (eq control 'edit) "edited" "provisional")
                             (niwa-goal-id newer)))
              (should (equal (if (eq control 'edit) "Revised" "Ship")
                             (niwa-goal-objective newer))))
            (should (= 90 niwa-goal-token-budget)))))))

  :doc "preserves public mutation authority even when the provisional record returns to its original contents"
  (dolist (control '(pause-resume budget-return))
    (ert-info ((format "Same-clock public mutation: %s" control))
      (let* ((previous (niwa-goal--create :id "previous" :status 'complete))
             (session (niwa-session--create :name "main" :goal previous))
             (policy '(:model-provider "Unavailable:implementer"
                       :reasoning-effort high :permission-mode edits))
             (saves 0)
             provisional original-state)
        (with-temp-buffer
          (setq-local niwa--session session niwa-goal-token-budget 90)
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "now"))
                    ((symbol-function 'niwa-goal-schedule-continuation) #'ignore)
                    ((symbol-function 'niwa-session-artifacts-save)
                     (lambda (saved-session saved-buffer &rest _)
                       (should (eq session saved-session))
                       (should (eq (current-buffer) saved-buffer))
                       (cl-incf saves)
                       (when (= saves 1)
                         (setq provisional (niwa-session-goal session)
                               original-state (copy-tree provisional t))
                         (pcase control
                           ('pause-resume
                            (should (eq provisional (niwa-goal-pause)))
                            (should (eq 'paused (niwa-goal-status provisional)))
                            (should (eq provisional (niwa-goal-resume))))
                           ('budget-return
                            (should (eq provisional (niwa-goal-set-budget 75)))
                            (should (= 75 (niwa-goal-token-budget provisional)))
                            (should (eq provisional (niwa-goal-set-budget 50)))))
                         ;; Both identity and durable contents are back at A.
                         (should (eq provisional (niwa-session-goal session)))
                         (should (equal original-state provisional))
                         (error "Disk full")))))
            (should (equal "Disk full"
                           (error-message-string
                            (should-error
                             (niwa-goal-create
                              "Ship" :id "provisional" :token-budget 50
                              :implementation-policy policy)))))
            (should (= 3 saves))
            (should (eq provisional (niwa-session-goal session)))
            (should (equal original-state (niwa-session-goal session)))
            (should-not (eq previous (niwa-session-goal session)))
            (should (= 90 niwa-goal-token-budget)))))))

  :doc "retains rollback authority when public controls touch an unrelated Goal"
  (let* ((previous (niwa-goal--create :id "previous" :status 'complete))
         (session (niwa-session--create :name "main" :goal previous))
         (other (niwa-goal--create
                 :id "provisional" :objective "Other" :status 'active))
         (other-session (niwa-session--create :name "other" :goal other))
         (saves 0))
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (&rest _) "now"))
                ((symbol-function 'niwa-goal-schedule-continuation) #'ignore)
                ((symbol-function 'niwa-session-artifacts-save)
                 (lambda (saved-session saved-buffer &rest _)
                   (should (eq (current-buffer) saved-buffer))
                   (cl-incf saves)
                   (when (eq session saved-session)
                     (with-temp-buffer
                       (setq-local niwa--session other-session)
                       (niwa-goal-pause)
                       (niwa-goal-resume))
                     (error "Disk full")))))
        (should (equal "Disk full"
                       (error-message-string
                        (should-error
                         (niwa-goal-create "Ship" :id "provisional")))))
        (should (= 3 saves))
        (should (eq previous (niwa-session-goal session)))
        (should (eq other (niwa-session-goal other-session)))
        (should (eq 'active (niwa-goal-status other)))))
    (should-not niwa-goal--provisional-owners))

  :doc "creates and persists the phase-free durable Goal record"
  (let* ((saved nil)
         (session (niwa-session--create :name "main"))
         (goal
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (&rest _) (setq saved t))))
            (with-temp-buffer
              (setq-local niwa--session session)
              (niwa-goal-create "Ship it" :session session :id "goal-1")))))
    (should saved)
    (should (eq goal (niwa-session-goal session)))
    (should (equal "goal-1" (niwa-goal-id goal)))
    (should (equal "Ship it" (niwa-goal-objective goal)))
    (should (eq 'active (niwa-goal-status goal)))
    (should-not (niwa-goal-reason goal))
    (should-not (niwa-goal-implementation-policy goal))
    (should-not (niwa-goal-token-budget goal))
    (should (= 0 (niwa-goal-tokens-used goal)))
    (should (= 0 (niwa-goal-time-used-seconds goal)))
    (should (= 0 (niwa-goal-turns-run goal)))
    (should-not (niwa-goal-plan-reference goal))
    (should (stringp (niwa-goal-created-at goal)))
    (should (equal (niwa-goal-created-at goal)
                   (niwa-goal-updated-at goal))))

  :doc "stores a copied policy and distinguishes omitted budget from explicit unlimited"
  (with-temp-buffer
    (let ((session (niwa-session--create :name "main"))
          (policy (list :model-provider "Unavailable:implementer"
                        :reasoning-effort nil :permission-mode 'edits))
          (global-budget (default-value 'niwa-goal-token-budget)))
      (setq-local niwa--session session niwa-goal-token-budget 90)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore))
        (let ((goal (niwa-goal-create
                     "Unlimited" :implementation-policy policy :token-budget nil)))
          (should-not (niwa-goal-token-budget goal))
          (should (equal policy (niwa-goal-implementation-policy goal)))
          (should-not (eq policy (niwa-goal-implementation-policy goal)))
          (plist-put policy :permission-mode 'ask)
          (should (eq 'edits
                      (plist-get (niwa-goal-implementation-policy goal)
                                 :permission-mode))))
        (should (= 50 (niwa-goal-token-budget
                       (niwa-goal-create "Bounded" :token-budget 50))))
        (let ((ordinary (niwa-goal-create "Ordinary")))
          (should (= 90 (niwa-goal-token-budget ordinary)))
          (should-not (niwa-goal-implementation-policy ordinary))))
      (should (= 90 niwa-goal-token-budget))
      (should (equal global-budget (default-value 'niwa-goal-token-budget)))))

  :doc "rejects malformed policy or explicit budget before replacing the Goal"
  (with-temp-buffer
    (let* ((previous (niwa-goal--create :id "previous" :status 'complete))
           (session (niwa-session--create :name "main" :goal previous)))
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest _) (ert-fail "Saved invalid Goal"))))
        (should-error (niwa-goal-create "Ship" :token-budget 0))
        (should-error
         (niwa-goal-create "Ship" :implementation-policy
                              '(:model-provider "Unavailable:implementer"))))
      (should (eq previous (niwa-session-goal session))))))

(niwa-deftest niwa-goal-start
  ()
  ,test
  (test)
  :doc "attributes a Goal started interactively during an ordinary root request"
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "main"))
           (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                            :tokens-full '(:input 12 :output 8)))))
      (setq-local niwa--session session
                  niwa--current-request (niwa-request--create :session session :fsm fsm))
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                ((symbol-function 'niwa-goal-schedule-continuation) #'ignore))
        (niwa-goal-start "Ship"))
      (let ((goal (niwa-session-goal session)))
        (should (equal (niwa-goal-id goal) (plist-get (gptel-fsm-info fsm) :niwa-goal-id)))
        (niwa-goal-settle-turn fsm)
        (should (= 20 (niwa-goal-tokens-used goal)))
        (should (= 1 (niwa-goal-turns-run goal))))))

  :doc "rejects an invalid budget before settling a replaced same-turn Goal"
  (with-temp-buffer
    (let* ((previous (niwa-goal--create :id "old" :status 'complete :tokens-used 0))
           (session (niwa-session--create :name "main" :goal previous))
           (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                            :niwa-goal-id "old"
                                            :niwa-goal-accounting-id "old"
                                            :tokens-full '(:input 12 :output 8)))))
      (setq-local niwa--session session
                  niwa--current-request (niwa-request--create :session session :fsm fsm)
                  niwa-goal-token-budget 0)
      (should-error (niwa-goal-start "Ship"))
      (should (eq previous (niwa-session-goal session)))
      (should (= 0 (niwa-goal-tokens-used previous)))
      (should-not (plist-get (gptel-fsm-info fsm) :niwa-goal-accounted))))

  :doc "rejects Plan mode before creating a competing execution lifecycle"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "main" :plan-mode t))
    (should-error (niwa-goal-start "Ship") :type 'user-error)
    (should-not (niwa-session-goal niwa--session)))

  :doc "rejects a pending Plan handoff reservation"
  (dolist (metadata
           '((:implementation-retry (:goal-id "reserved"))))
    (let ((session (niwa-session--create
                    :name "main" :plan-metadata metadata)))
      (with-temp-buffer
        (setq-local niwa--session session)
        (should-error (niwa-goal-start "Competing Goal")
                      :type 'user-error))
      (should-not (niwa-session-goal session)))))

(niwa-deftest niwa-goal--assert-token-budget
  (:doc "checks explicit budgets independently of the effective default")
  (with-temp-buffer
    (setq-local niwa-goal-token-budget 0)
    (should-error (niwa-goal--assert-token-budget))
    (should-not (niwa-goal--assert-token-budget nil))
    (should-not (niwa-goal--assert-token-budget 50))
    (dolist (invalid '(0 -1 1.5 "50"))
      (should-error (niwa-goal--assert-token-budget invalid)))))

(niwa-deftest niwa-goal-ensure
  ()
  ,test
  (test)
  :doc "reactivates only the matching reserved Goal without scheduling"
  (let* ((goal (niwa-goal--create
                :id "reserved" :status 'paused :reason "session resumed"
                :plan-reference "local/plans/accepted.md"))
         (session (niwa-session--create :name "target" :goal goal))
         saved scheduled)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest _) (setq saved t)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq scheduled t)))
                ((symbol-function 'niwa-goal-create)
                 (lambda (&rest _) (ert-fail "Constructed duplicate Goal"))))
        (should
         (eq goal
             (niwa-goal-ensure
              "Objective" session "local/plans/accepted.md" "reserved")))))
    (should saved)
    (should-not scheduled)
    (should (eq 'active (niwa-goal-status goal)))
    (should-not (niwa-goal-reason goal))
    (setf (niwa-goal-plan-reference goal) "local/plans/other.md")
    (should-error
     (with-temp-buffer
       (niwa-goal-ensure
        "Objective" session "local/plans/accepted.md" "reserved")))
    (should (eq goal (niwa-session-goal session))))

  :doc "preserves the reserved policy and edited budget across recovery"
  (with-temp-buffer
    (let* ((policy '(:model-provider "Unavailable:implementer"
                     :reasoning-effort high :permission-mode edits))
           (goal (niwa-goal--create
                  :id "reserved" :status 'paused :reason "session resumed"
                  :implementation-policy policy :token-budget 150
                  :tokens-used 12 :turns-run 2
                  :plan-reference "local/plans/accepted.md"))
           (session (niwa-session--create :name "target" :goal goal)))
      (setq-local niwa--session session niwa-goal-token-budget 90)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore))
        (should (eq goal
                    (niwa-goal-ensure
                     "Objective" session "local/plans/accepted.md" "reserved"
                     :implementation-policy
                     '(:permission-mode edits :reasoning-effort high
                       :model-provider "Unavailable:implementer")
                     :token-budget 50))))
      (should (eq 'active (niwa-goal-status goal)))
      (should (equal policy (niwa-goal-implementation-policy goal)))
      (should (= 150 (niwa-goal-token-budget goal)))
      (should (= 12 (niwa-goal-tokens-used goal)))
      (should (= 2 (niwa-goal-turns-run goal)))
      (should (= 90 niwa-goal-token-budget))))

  :doc "rejects each reserved policy conflict before reactivation or save"
  (with-temp-buffer
    (let* ((policy '(:model-provider "Unavailable:implementer"
                     :reasoning-effort high :permission-mode edits))
           (goal (niwa-goal--create
                  :id "reserved" :status 'paused :reason "session resumed"
                  :implementation-policy policy :token-budget 150
                  :plan-reference "local/plans/accepted.md"))
           (session (niwa-session--create :name "target" :goal goal)))
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest _) (ert-fail "Saved conflicting Goal"))))
        (dolist (change '((:model-provider "Unavailable:other")
                          (:reasoning-effort nil)
                          (:permission-mode ask)))
          (let ((conflict (copy-tree policy)))
            (plist-put conflict (car change) (cadr change))
            (should-error
             (niwa-goal-ensure
              "Objective" session "local/plans/accepted.md" "reserved"
              :implementation-policy conflict))))
        (should-error
         (niwa-goal-ensure
          "Objective" session "local/plans/accepted.md" "reserved")))
      (should (eq 'paused (niwa-goal-status goal)))
      (should (equal "session resumed" (niwa-goal-reason goal)))
      (should (equal policy (niwa-goal-implementation-policy goal)))
      (should (= 150 (niwa-goal-token-budget goal)))))

  :doc "creates a reserved Goal with explicit unlimited budget without changing defaults"
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "target"))
           (policy '(:model-provider "Unavailable:implementer"
                     :reasoning-effort nil :permission-mode ask)))
      (setq-local niwa--session session niwa-goal-token-budget 90)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore))
        (let ((goal (niwa-goal-ensure
                     "Objective" session "local/plans/accepted.md" "reserved"
                     :implementation-policy policy :token-budget nil)))
          (should (equal "reserved" (niwa-goal-id goal)))
          (should (equal policy (niwa-goal-implementation-policy goal)))
          (should-not (niwa-goal-token-budget goal))))
      (should (= 90 niwa-goal-token-budget)))))

(niwa-deftest niwa-goal-active-context
  (:doc "pauses when verified publication bytes violate the accepted hash")
  (progn
   (let* ((root (make-temp-file "niwa-goal-plan-" t))
         (plan-file (file-name-concat root "accepted-plan.md"))
         (session (niwa-session--create
                   :name "main" :save-path root))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :plan-reference "accepted-plan.md"
                :created-at "now" :updated-at "now"))
         saved)
    (unwind-protect
        (progn
          (write-region "poisoned fixed cache" nil plan-file nil 'silent)
          (setf (niwa-session-goal session) goal
                (niwa-session-plan-metadata session)
                (list :accepted-path "accepted-plan.md"
                      :accepted-hash (niwa-plan-hash "different")))
          (with-temp-buffer
            (setq-local niwa--session session)
            (cl-letf (((symbol-function
                        'niwa-session-artifacts-artifact-present-p)
                       (lambda (seen-session logical)
                         (should (eq session seen-session))
                         (should (equal "accepted-plan.md" logical))
                         t))
                      ((symbol-function
                        'niwa-session-artifacts-read-artifact)
                       (lambda (_session _logical &optional _committed-only)
                         (encode-coding-string "accepted" 'utf-8-unix)))
                      ((symbol-function 'niwa-session-artifacts-save)
                       (lambda (&rest _) (setq saved t))))
              (should-error (niwa-goal-active-context session))))
          (should saved)
          (should (eq 'paused (niwa-goal-status goal)))
          (should (string-match-p "hash" (niwa-goal-reason goal))))
      (delete-directory root t)))
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 7 :time-used-seconds 0 :turns-run 1
                :created-at "now" :updated-at "now"))
         replacements)
    (setf (niwa-session-goal session) goal)
    (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
               (lambda (_path values) (setq replacements values) "context")))
      (should (equal "context" (niwa-goal-active-context session))))
    (should (equal "unbounded" (cdr (assoc "token-budget" replacements))))
    ;; Usage changes with every charge and would re-deliver the context.
    (should-not (assoc "tokens-used" replacements))
    (should-not (assoc "tokens-remaining" replacements)))
  (let* ((root (make-temp-file "niwa-goal-plan-address-" t))
         (relative "local/plans/accepted-20260813-120000.md")
         (plan-file (file-name-concat root relative))
         (session (test-niwa-session-persistence--leased-session root :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :plan-reference relative
                :created-at "now" :updated-at "now"))
         replacements)
    (unwind-protect
        (progn
          (make-directory (file-name-directory plan-file) t)
          (write-region "accepted" nil plan-file nil 'silent)
          (setf (niwa-session-goal session) goal
                (niwa-session-plan-metadata session)
                (list :accepted-path relative
                      :accepted-hash (niwa-plan-hash "accepted")))
          (niwa-session-publication-publish
           session
           (list (list :path plan-file :content "accepted")
                 (list :path (file-name-concat root "session.meta.el")
                       :content (niwa-session-artifacts-printed-value
                                 (niwa-session-codec-serialize session))
                       :commit-marker t)))
          (cl-letf (((symbol-function 'niwa-system-render-prompt-file)
                     (lambda (_path values) (setq replacements values)
                       "context")))
            (should (equal "context" (niwa-goal-active-context session))))
          ;; Goal context names the plan by address, never by storage path.
          (let ((line (cdr (assoc "plan-reference-line" replacements))))
            (should (string-match-p
                     "work://plans/accepted-20260813-120000\\.md" line))
            (should-not (string-match-p (regexp-quote root) line))))
      (niwa-session-persistence-lock-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t)))))

(niwa-deftest niwa-goal-capture-request
  (:doc "attributes root requests and excludes child and compaction requests")
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *niwa-goal-capture*")))
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local niwa--session session
                        niwa--current-request
                        (niwa-request--create :session session)))
          (let ((root (gptel-make-fsm :info (list :buffer buffer :data "abc"))))
            ;; A replaced Goal's plan grant does not survive capture.
            (setf (niwa-request-goal-plan-read-path
                   (buffer-local-value 'niwa--current-request buffer))
                  "stale-plan.md")
            (niwa-goal-capture-request root)
            (should-not (niwa-request-goal-plan-read-path
                         (buffer-local-value 'niwa--current-request buffer)))
            (should (equal "goal-1"
                           (plist-get (gptel-fsm-info root)
                                      :niwa-goal-id)))
            (should (equal "goal-1"
                           (plist-get (gptel-fsm-info root)
                                      :niwa-goal-accounting-id))))
          (setf (niwa-goal-status goal) 'complete)
          (let ((after-completion
                 (gptel-make-fsm :info (list :buffer buffer :data "next"))))
            (niwa-goal-capture-request after-completion)
            (should-not (plist-get (gptel-fsm-info after-completion)
                                   :niwa-goal-id)))
          (setf (niwa-goal-status goal) 'active)
          (with-current-buffer buffer
            (setq-local niwa--agent-invocation
                        (niwa-agent-invocation--create)))
          (let ((child (gptel-make-fsm :info (list :buffer buffer))))
            (niwa-goal-capture-request child)
            (should-not (plist-get (gptel-fsm-info child)
                                   :niwa-goal-id)))
          (with-current-buffer buffer
            (setq-local niwa--agent-invocation nil))
          (let ((compact
                 (gptel-make-fsm
                  :info (list :buffer buffer
                              :context '(:niwa-context-summary t)))))
            (niwa-goal-capture-request compact)
            (should-not (plist-get (gptel-fsm-info compact)
                                   :niwa-goal-id))))
      (kill-buffer buffer))))

(niwa-deftest niwa-goal-continue-if-idle
  (:doc "applies idle, queue, interaction, and budget gates before dispatch")
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :token-budget 10 :tokens-used 0
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *niwa-goal-gates*"))
         dispatched scheduled)
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                     (lambda (&rest _) (setq dispatched t)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest args) (setq scheduled args))))
            (should (eq 'unavailable
                        (niwa-goal-continue-if-idle session (current-buffer))))
            (with-current-buffer buffer (setq-local niwa--view-buffer buffer))
            (cl-letf (((symbol-function 'niwa-view-interaction-pending-p)
                       (lambda (view) (should (eq view buffer)) t)))
              (should (eq 'interaction (niwa-goal-continue-if-idle session buffer))))
            (with-current-buffer buffer (setq-local niwa--view-buffer nil))
            (should (eq 'dispatched
                        (niwa-goal-continue-if-idle session buffer)))
            (should dispatched)
            (with-current-buffer buffer
              (setq-local niwa--current-request
                          (niwa-request--create :session session)))
            (should (eq 'request
                        (niwa-goal-continue-if-idle session buffer)))
            (with-current-buffer buffer
              (setq-local niwa--current-request nil))
            (setf (niwa-session-pending-follow-ups session)
                  '((:input "steer")))
            (should (eq 'follow-up
                        (niwa-goal-continue-if-idle session buffer)))
            (should scheduled)
            (setf (niwa-session-pending-follow-ups session) nil)
            ;; Background work that reported runs before the Goal does.
            (setq scheduled nil dispatched nil)
            (niwa-session--set-messages
             session (list (list :type 'EXECUTION :wake t)))
            (should (eq 'event (niwa-goal-continue-if-idle session buffer)))
            (should (eq 'niwa-pending-inputs--run-event-wake (nth 2 scheduled)))
            (should-not dispatched)
            (niwa-session--set-messages session nil)
            (setf (niwa-session-permission-queue session) '(pending))
            (should (eq 'interaction
                        (niwa-goal-continue-if-idle session buffer)))
            (setf (niwa-session-permission-queue session) nil
                  (niwa-goal-tokens-used goal) 10)
            (should (eq 'budget
                        (niwa-goal-continue-if-idle session buffer)))))
      (kill-buffer buffer))))

(niwa-deftest niwa-goal-settle-turn
  (:doc "settles only the Goal lineage attributed to the request")
  ,test
  (test)
  :doc "charges provider tokens, elapsed time, and one turn"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 2 :time-used-seconds 1 :turns-run 3
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *niwa-goal-settle*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :niwa-goal-id "goal-1"
                           :niwa-goal-accounting-id "goal-1"
                           :niwa-goal-started-at (- (float-time) 2)
                           :tokens-full '(:input 10 :output 3)))))
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (niwa-goal-settle-turn fsm)
          (niwa-goal-settle-turn fsm)
          (should (= 15 (niwa-goal-tokens-used goal)))
          (should (>= (niwa-goal-time-used-seconds goal) 3))
          (should (= 4 (niwa-goal-turns-run goal))))
      (kill-buffer buffer)))

  :doc "enforces budget thresholds and terminal Goal states"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-2" :objective "Ship" :status 'active
                :token-budget 100 :tokens-used 49
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *niwa-goal-budget-crossing*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :niwa-goal-id "goal-2"
                           :niwa-goal-accounting-id "goal-2"
                           :tokens-full '(:input 40 :output 11 :cached 500)))))
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer (setq-local niwa--session session))
          (niwa-goal-settle-turn fsm)
          (should (= 100 (niwa-goal-tokens-used goal)))
          (should (eq 'budget-limited (niwa-goal-status goal)))
          (should (string-match-p "100/100" (niwa-goal-reason goal)))
          (should (equal '("50%" "80%" "100%")
                         (mapcar
                          (lambda (reminder)
                            (and (string-match "\\(50%\\|80%\\|100%\\)"
                                               reminder)
                                 (match-string 1 reminder)))
                          (niwa-session-pending-reminders session))))
          (should (string-match-p
                   "Prioritize the remaining requirements"
                   (nth 0 (niwa-session-pending-reminders session))))
          (should (string-match-p
                   "avoid low-value detours"
                   (nth 1 (niwa-session-pending-reminders session))))
          (niwa-goal-settle-turn
           (gptel-make-fsm
            :info (list :buffer buffer :niwa-goal-id "goal-2"
                        :niwa-goal-accounting-id "goal-2"
                        :tokens-full '(:input 1 :output 0))))
          (should (= 3 (length (niwa-session-pending-reminders session)))))
      (kill-buffer buffer)))

  :doc "does not reactivate a complete or blocked Goal"
  (dolist (status '(complete blocked))
    (let* ((session (niwa-session--create :name "main"))
           (goal (niwa-goal--create
                  :id "goal-3" :objective "Ship" :status status
                  :reason (and (eq status 'blocked) "Need access")
                  :token-budget 100 :tokens-used 99
                  :time-used-seconds 0 :turns-run 0
                  :created-at "now" :updated-at "now"))
           (buffer (generate-new-buffer " *niwa-goal-budget-terminal*")))
      (unwind-protect
          (progn
            (setf (niwa-session-goal session) goal)
            (with-current-buffer buffer (setq-local niwa--session session))
            (niwa-goal-settle-turn
             (gptel-make-fsm
              :info (list :buffer buffer :niwa-goal-id "goal-3"
                          :niwa-goal-accounting-id "goal-3"
                          :tokens-full '(:input 1 :output 0))))
            (should (eq status (niwa-goal-status goal))))
        (kill-buffer buffer))))

  :doc "does not charge a replacement created after clear"
  (let* ((session (niwa-session--create :name "main"))
         (old-goal (niwa-goal--create
                    :id "goal-old" :objective "Old" :status 'active
                    :tokens-used 0 :time-used-seconds 0 :turns-run 0))
         (buffer (generate-new-buffer " *niwa-goal-replaced-success*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :niwa-goal-id "goal-old"
                           :niwa-goal-accounting-id "goal-old"
                           :tokens-full '(:input 7 :output 3)))))
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) old-goal)
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (cl-letf (((symbol-function 'niwa-goal--assert-mutation-authority)
                       #'ignore)
                      ((symbol-function 'niwa-session-artifacts-save)
                       #'ignore)
                      ((symbol-function 'niwa-goal-new-id)
                       (lambda () "goal-new"))
                      ((symbol-function 'niwa-telemetry-record) #'ignore)
                      ((symbol-function 'run-at-time) #'ignore))
              (niwa-goal-clear)
              (niwa-goal-start "Replacement")))
          (let ((replacement (niwa-session-goal session)))
            (niwa-goal-settle-turn fsm)
            (should (equal "goal-new" (niwa-goal-id replacement)))
            (should (= 0 (niwa-goal-tokens-used replacement)))
            (should (= 0 (niwa-goal-turns-run replacement)))))
      (kill-buffer buffer))))

(ert-deftest niwa-goal-settle-failure/permanent-provider-guard ()
  "An authoritative permanent provider error wins over transient-looking text."
  (dolist (permanent '(t nil))
    (with-temp-buffer
      (let* ((reason "Model at capacity; temporarily unavailable, retry later")
             (goal (niwa-goal--create :id "g" :status 'active :token-budget 100
                                        :tokens-used 0 :time-used-seconds 0 :turns-run 0))
             (session (niwa-session--create :name "main" :goal goal))
             (fsm (gptel-make-fsm
                   :info (list :buffer (current-buffer) :niwa-goal-id "g"
                               :niwa-goal-accounting-id "g"
                               :niwa-retry-permanent permanent
                               :tokens-full '(:input 7 :output 2)
                               :error (list :type "insufficient_quota"
                                            :code "insufficient_quota" :message reason)))))
        (setq-local niwa--session session niwa-goal--transient-retries 0)
        ;; Prove this message would otherwise enter the settled-turn retry path.
        (should (niwa-goal--transient-failure-p reason))
        (niwa-test--with-captured-messages nil
          (niwa-goal-settle-failure fsm 'error))
        (should (eq (if permanent 'paused 'active) (niwa-goal-status goal)))
        (should (= (if permanent 0 1) niwa-goal--transient-retries))
        (should (= 9 (niwa-goal-tokens-used goal)))
        (should (= 1 (niwa-goal-turns-run goal)))
        (when permanent
          (should (equal reason (niwa-goal-reason goal)))
          (should-not (niwa-session-pending-reminders session))
          (niwa-goal-settle-failure fsm 'error)
          (should (= 1 (niwa-goal-turns-run goal)))
          (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                     (lambda (&rest _) (ert-fail "Permanent failure dispatched a Goal retry"))))
            (should (eq 'inactive (niwa-goal-continue-if-idle session)))))))))

(niwa-deftest niwa-goal-settle-failure
  (:doc "settles failure only against the attributed Goal lineage")
  ,test
  (test)
  :doc "queues no budget instructions for a failed turn that pauses the Goal"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :status 'active :token-budget 100
                                       :tokens-used 0 :time-used-seconds 0 :turns-run 0))
           (session (niwa-session--create :name "main" :goal goal)))
      (setq-local niwa--session session)
      (niwa-goal-settle-failure
       (gptel-make-fsm :info (list :buffer (current-buffer) :niwa-goal-id "g"
                                   :niwa-goal-accounting-id "g"
                                   :tokens-full '(:input 50 :output 10)
                                   :error "invalid request"))
       'error)
      (should (eq 'paused (niwa-goal-status goal)))
      (should (= 60 (niwa-goal-tokens-used goal)))
      (should-not (niwa-session-pending-reminders session))))

  :doc "request retry exhaustion pauses without multiplying the provider budget"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :status 'active :token-budget 100
                                       :tokens-used 0 :time-used-seconds 0 :turns-run 0))
           (session (niwa-session--create :name "main" :goal goal))
           (fsm (gptel-make-fsm
                 :info (list :buffer (current-buffer) :niwa-goal-id "g"
                             :niwa-goal-accounting-id "g"
                             :niwa-retry-exhausted t
                             :tokens-full '(:input 50 :output 10)
                             :error "temporary network timeout"))))
      (setq-local niwa--session session niwa-goal--transient-retries 0)
      (niwa-goal-settle-failure fsm 'error)
      (should (eq 'paused (niwa-goal-status goal)))
      (should (equal "temporary network timeout" (niwa-goal-reason goal)))
      (should (= 0 niwa-goal--transient-retries))
      (should (= 60 (niwa-goal-tokens-used goal)))
      (should (= 1 (niwa-goal-turns-run goal)))
      (should-not (niwa-session-pending-reminders session))
      (niwa-goal-settle-failure fsm 'error)
      (should (= 1 (niwa-goal-turns-run goal)))
      (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                 (lambda (&rest _) (ert-fail "Paused Goal dispatched a retry"))))
        (should (eq 'inactive (niwa-goal-continue-if-idle session))))))

  :doc "retries transient failures with backoff and pauses once exhausted or terminal"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *niwa-goal-failure*")))
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local niwa--session session
                        niwa-goal--transient-retries 0))
          (let (messages)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              ;; Each is a separate failed turn, not a duplicate settlement.
              (dolist (reason '("temporary network timeout"
                                "Curl failed with exit code 6. See Curl manpage for details."
                                "Curl failed with exit code 56."
                                ;; Overload text naming the model is transient.
                                "HTTP/2 503: The model gpt-5 is currently unavailable"
                                "temporary network timeout"))
                (niwa-goal-settle-failure
                 (gptel-make-fsm
                  :info (list :buffer buffer :niwa-goal-id "goal-1"
                              :niwa-goal-accounting-id "goal-1"
                              :error reason))
                 'error)
                (should (eq 'active (niwa-goal-status goal))))
              (should (= 5 (buffer-local-value
                            'niwa-goal--transient-retries buffer)))
              (should (string-match-p "retrying in 15s" (car (last messages))))
              (should (string-match-p "retrying in 240s" (car messages)))
              (niwa-goal-settle-failure
               (gptel-make-fsm
                :info (list :buffer buffer :niwa-goal-id "goal-1"
                            :niwa-goal-accounting-id "goal-1"
                            :error "temporary network timeout"))
               'error))
            (should (= 5 (length messages)))
            (should (eq 'paused (niwa-goal-status goal)))
            (should (string-match-p "timeout" (niwa-goal-reason goal))))
          (setf (niwa-goal-status goal) 'active
                (niwa-goal-reason goal) nil)
          (with-current-buffer buffer
            (setq-local niwa-goal--transient-retries 0))
          (dolist (reason '("authentication failed"
                            "Curl failed with exit code 60."))
            (setf (niwa-goal-status goal) 'active)
            (niwa-goal-settle-failure
             (gptel-make-fsm
              :info (list :buffer buffer :niwa-goal-id "goal-1"
                          :niwa-goal-accounting-id "goal-1"
                          :error reason))
             'error)
            (should (eq 'paused (niwa-goal-status goal))))
          (setf (niwa-goal-status goal) 'complete
                (niwa-goal-reason goal) nil)
          (niwa-goal-settle-failure
           (gptel-make-fsm
            :info (list :buffer buffer :niwa-goal-id "goal-1"
                        :niwa-goal-accounting-id "goal-1"
                        :error "provider failed"))
           'error)
          (should (eq 'complete (niwa-goal-status goal))))
      (kill-buffer buffer)))

  :doc "does not pause a current Goal with a different identity"
  (let* ((session (niwa-session--create :name "main"))
         (replacement (niwa-goal--create
                       :id "goal-new" :objective "Replacement"
                       :status 'active :tokens-used 0
                       :time-used-seconds 0 :turns-run 0))
         (buffer (generate-new-buffer " *niwa-goal-replaced-failure*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :niwa-goal-id "goal-old"
                           :niwa-goal-accounting-id "goal-old"
                           :error "authentication failed"))))
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) replacement)
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (niwa-goal-settle-failure fsm 'error)
          (should (eq 'active (niwa-goal-status replacement)))
          (should-not (niwa-goal-reason replacement))
          (should (= 0 (niwa-goal-tokens-used replacement)))
          (should (= 0 (niwa-goal-turns-run replacement))))
      (kill-buffer buffer))))

(niwa-deftest niwa-goal-pause-runtime-failure
  (:doc "persists a concrete runtime failure only for an active Goal")
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *niwa-goal-runtime-failure*"))
         saved)
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (&rest _) (setq saved t))))
            (should (eq goal (niwa-goal-pause-runtime-failure
                              buffer "Compaction failed: boom"))))
          (should saved)
          (should (eq 'paused (niwa-goal-status goal)))
          (should (equal "Compaction failed: boom"
                         (niwa-goal-reason goal))))
      (kill-buffer buffer))))

(niwa-deftest niwa-tool-goal-update
  (:doc "accepts only matching active terminal transitions and requires blocked detail")
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now")))
    (setf (niwa-session-goal session) goal)
    (should-error (niwa-tool-goal-update 'complete nil session "old"))
    (should-error (niwa-tool-goal-update 'blocked "  " session "goal-1"))
    (should (equal "Goal status changed to blocked"
                   (niwa-tool-goal-update
                    "blocked" " Need a credential. " session "goal-1")))
    (should (eq 'blocked (niwa-goal-status goal)))
    (should (equal "Need a credential." (niwa-goal-reason goal)))
    (should-error (niwa-tool-goal-update 'complete nil session "goal-1"))))

(niwa-deftest niwa-goal-clear
  (:doc "keeps policy through lifecycle controls and drops it on clear or ordinary replacement")
  (with-temp-buffer
    (let* ((policy '(:model-provider "Unavailable:implementer"
                     :reasoning-effort high :permission-mode edits))
           (session (niwa-session--create :name "main"))
           saved-policies)
      (setq-local niwa--session session niwa-goal-token-budget 90)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                 (lambda (saved-session &rest _)
                   (push (when-let* ((saved-goal (niwa-session-goal saved-session)))
                           (copy-tree (niwa-goal-implementation-policy saved-goal)))
                         saved-policies)))
                ((symbol-function 'niwa-goal-schedule-continuation) #'ignore))
        (let ((goal (niwa-goal-create
                     "Ship" :implementation-policy policy :token-budget 50)))
          (niwa-goal-pause)
          (should (eq 'paused (niwa-goal-status goal)))
          (niwa-goal-resume)
          (niwa-tool-goal-update 'blocked "Need input" session (niwa-goal-id goal))
          (should (equal policy (niwa-goal-implementation-policy goal)))
          (niwa-goal-resume)
          (let ((old-id (niwa-goal-id goal)))
            (niwa-goal-edit "Revised objective")
            (should-not (equal old-id (niwa-goal-id goal))))
          (should (equal policy (niwa-goal-implementation-policy goal)))
          (setf (niwa-goal-tokens-used goal) 10)
          (niwa-goal-set-budget 10)
          (should (eq 'budget-limited (niwa-goal-status goal)))
          (niwa-goal-set-budget "none")
          (should (eq 'active (niwa-goal-status goal)))
          (niwa-tool-goal-update 'complete nil session (niwa-goal-id goal))
          (should (equal policy (niwa-goal-implementation-policy goal)))
          (should-error (niwa-goal-resume) :type 'user-error)
          (should (cl-every (lambda (saved) (equal policy saved)) saved-policies))
          (setq niwa-goal--rejected-completions 2)
          (niwa-goal-clear)
          (should-not (niwa-session-goal session))
          (should (= 0 niwa-goal--rejected-completions))
          (should-not (car saved-policies))
          (niwa-goal-start "Ordinary replacement")
          (let ((replacement (niwa-session-goal session)))
            (should-not (eq goal replacement))
            (should-not (niwa-goal-implementation-policy replacement))
            (should (= 90 (niwa-goal-token-budget replacement))))))
      (should (= 90 niwa-goal-token-budget)))))

(niwa-deftest niwa-goal-record-rejected-completion
  (:doc "counts rejected claims per root buffer and returns the running total")
  (let ((root (generate-new-buffer " *goal-root*"))
        (other (generate-new-buffer " *goal-other*")))
    (unwind-protect
        (progn
          (should (= 1 (niwa-goal-record-rejected-completion root)))
          (should (= 2 (niwa-goal-record-rejected-completion root)))
          (should (= 1 (niwa-goal-record-rejected-completion other)))
          (should (= 2 (buffer-local-value 'niwa-goal--rejected-completions root))))
      (kill-buffer root)
      (kill-buffer other))))

(niwa-deftest niwa-goal-resume
  ()
  ,test
  (test)
  :doc "rejects exhausted paused or blocked Goals and resets retries on an admitted resume"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :status 'paused
                                       :token-budget 10 :tokens-used 10))
           (session (niwa-session--create :name "main" :goal goal)))
      (setq-local niwa--session session niwa-goal--transient-retries 1
                  niwa-goal--rejected-completions 2)
      (dolist (status '(paused blocked))
        (setf (niwa-goal-status goal) status)
        (should-error (niwa-goal-resume) :type 'user-error)
        (should (eq status (niwa-goal-status goal))))
      (setf (niwa-goal-token-budget goal) 20)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                ((symbol-function 'niwa-goal-schedule-continuation) #'ignore))
        (niwa-goal-resume))
      (should (= 0 niwa-goal--transient-retries))
      (should (= 0 niwa-goal--rejected-completions))
      (should (eq 'active (niwa-goal-status goal)))))

  :doc "queues optional steering before rearming continuation"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'paused
                :reason "paused by user" :tokens-used 0
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         scheduled)
    (setf (niwa-session-goal session) goal
          (niwa-session-plan-metadata session)
          '(:status accepted))
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (&rest args) (setq scheduled args))))
        (niwa-goal-resume "  steer first  ")))
    (should scheduled)
    (should (eq 'active (niwa-goal-status goal)))
    (should-not (niwa-goal-reason goal))
    (let ((entry (car (niwa-session-pending-follow-ups session))))
      (should (equal "steer first" (plist-get entry :input)))
      (should (= 1 (plist-get entry :id)))
      (should (eq 'follow-up (plist-get entry :category))))
    (setf (niwa-goal-status goal) 'budget-limited)
    (with-temp-buffer
      (setq-local niwa--session session)
      (should-error (niwa-goal-resume) :type 'user-error))))

(niwa-deftest niwa-goal-set-budget
  (:doc "adjusts durable limits, emits reminders, and reactivates limited Goals")
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :token-budget 100 :tokens-used 60
                :time-used-seconds 1 :turns-run 2
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *niwa-goal-budget*"))
         (saved 0)
         (scheduled 0))
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local niwa--session session
                        niwa--current-request
                        (niwa-request--create :session session)))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (&rest _) (cl-incf saved)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _) (cl-incf scheduled))))
            (with-current-buffer buffer
              (dolist (invalid '("" "0" "-1" "many"))
                (should-error (niwa-goal-set-budget invalid)
                              :type 'user-error))
              (niwa-goal-set-budget "50")
              (should (eq 'budget-limited (niwa-goal-status goal)))
              (should (string-match-p "100.*50.*60.*0.*budget-limited"
                                      (car (last
                                            (niwa-session-pending-reminders
                                             session)))))
              (niwa-goal-set-budget "80")
              (should (eq 'active (niwa-goal-status goal)))
              (should-not (niwa-goal-reason goal))
              (should (eq 'request
                          (niwa-goal-continue-if-idle session buffer)))
              (niwa-goal-set-budget "none")
              (should-not (niwa-goal-token-budget goal))))
          (should (= 3 saved))
          (should (= 1 scheduled))
          (should (= 3 (length (niwa-session-pending-reminders session))))
          (should (string-match-p
                   "80.*unbounded.*60.*unbounded.*active"
                   (car (last (niwa-session-pending-reminders session))))))
      (kill-buffer buffer))))

(niwa-deftest niwa-goal-charge-tokens
  (:doc "charges the accounted Goal once and leaves the budget limit to settlement")
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :status 'active :token-budget 100
                                       :tokens-used 40 :time-used-seconds 0 :turns-run 0))
           (session (niwa-session--create :name "main" :goal goal))
           (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                            :niwa-goal-id "g"
                                            :niwa-goal-accounting-id "g"
                                            :tokens-full '(:input 5 :output 5)))))
      (setq-local niwa--session session)
      (dolist (tokens '(nil 0 -3 1.5))
        (should-not (niwa-goal-charge-tokens fsm tokens)))
      (should (= 40 (niwa-goal-tokens-used goal)))
      (should (niwa-goal-charge-tokens fsm 20))
      (should (= 60 (niwa-goal-tokens-used goal)))
      (should (= 1 (length (niwa-session-pending-reminders session))))
      (should (niwa-goal-charge-tokens fsm 45))
      (should (= 105 (niwa-goal-tokens-used goal)))
      ;; 80% and 100% each queue once; the status waits for settlement.
      (should (= 3 (length (niwa-session-pending-reminders session))))
      (should (eq 'active (niwa-goal-status goal)))
      (niwa-goal-settle-turn fsm)
      (should (= 115 (niwa-goal-tokens-used goal)))
      (should (eq 'budget-limited (niwa-goal-status goal)))
      (should (= 3 (length (niwa-session-pending-reminders session))))
      ;; A cleared or replaced Goal is never charged for this request.
      (setf (niwa-session-goal session)
            (niwa-goal--create :id "other" :status 'active :tokens-used 0))
      (should-not (niwa-goal-charge-tokens fsm 10))
      (should (= 0 (niwa-goal-tokens-used (niwa-session-goal session)))))))

(niwa-deftest niwa-goal-tool-result-budget-warnings
  (:doc "warns only for the attributed Goal lineage")
  ,test
  (test)
  :doc "commits the 100% tool-boundary warning only after delivery"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :token-budget 100 :tokens-used 90
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (fsm (gptel-make-fsm
               :info '(:niwa-goal-id "goal-1"
                       :niwa-goal-accounting-id "goal-1"
                       :tokens-full (:input 7 :output 3 :cached 1000)))))
    (setf (niwa-session-goal session) goal)
    (dolist (status '(complete blocked paused))
      (setf (niwa-goal-status goal) status)
      (should-not (niwa-goal-tool-result-budget-warnings session fsm)))
    (setf (niwa-goal-status goal) 'active)
    (let ((warnings
           (niwa-goal-tool-result-budget-warnings session fsm)))
      (should (= 1 (length warnings)))
      (should (eq 'goal-budget-100 (plist-get (car warnings) :key)))
      (should (string-match-p "crossed 100%.*Stop new substantive work"
                              (plist-get (car warnings) :body)))
      (should-not (plist-get (gptel-fsm-info fsm)
                             :niwa-goal-budget-warnings))
      (funcall (plist-get (car warnings) :commit)))
    (should-not (niwa-goal-tool-result-budget-warnings session fsm))
    (should (= 90 (niwa-goal-tokens-used goal))))

  :doc "reports every threshold one long turn crosses, once each"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :token-budget 100 :tokens-used 40))
         (fsm (gptel-make-fsm
               :info '(:niwa-goal-accounting-id "goal-1"
                       :tokens-full (:input 40 :output 5)))))
    (setf (niwa-session-goal session) goal)
    (let ((warnings (niwa-goal-tool-result-budget-warnings session fsm)))
      (should (equal '(goal-budget-50 goal-budget-80)
                     (mapcar (lambda (w) (plist-get w :key)) warnings)))
      (mapc (lambda (w) (funcall (plist-get w :commit))) warnings))
    (should-not (niwa-goal-tool-result-budget-warnings session fsm)))

  :doc "does not warn a current Goal with a different identity"
  (let* ((session (niwa-session--create :name "main"))
         (replacement (niwa-goal--create
                       :id "goal-new" :objective "Replacement"
                       :status 'active :token-budget 100 :tokens-used 0
                       :time-used-seconds 0 :turns-run 0))
         (fsm (gptel-make-fsm
               :info '(:niwa-goal-id "goal-old"
                       :niwa-goal-accounting-id "goal-old"
                       :tokens-full (:input 100 :output 0)))))
    (setf (niwa-session-goal session) replacement)
    (should-not (niwa-goal-tool-result-budget-warnings session fsm))
    (should-not (plist-get (gptel-fsm-info fsm)
                           :niwa-goal-budget-warnings))))

(niwa-deftest niwa-goal-accounting-owner ()
  ,test
  (test)
  :doc "finds the Goal-charged root request, inherited by agent buffers"
  (let ((root (gptel-make-fsm :info (list :niwa-goal-accounting-id "g")))
        (plain (gptel-make-fsm :info (list :buffer nil))))
    (with-temp-buffer
      (setq-local niwa--current-request (niwa-request--create :fsm root))
      (should (eq root (niwa-goal-accounting-owner)))
      (setq-local niwa--current-request (niwa-request--create :fsm plain))
      (should-not (niwa-goal-accounting-owner)))
    (with-temp-buffer
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create :goal-owner root))
      (should (eq root (niwa-goal-accounting-owner))))))

(niwa-deftest niwa-goal-charge-agent-progress ()
  ,test
  (test)
  :doc "charges only new agent usage and reports crossings to the running root turn"
  (let* ((root-buffer (generate-new-buffer " *niwa-goal-agent-charge*"))
         (goal (niwa-goal--create :id "g" :objective "Ship" :status 'active
                                     :token-budget 100 :tokens-used 40))
         (session (niwa-session--create :name "main" :goal goal))
         (root (gptel-make-fsm
                :info (list :buffer root-buffer :niwa-goal-accounting-id "g")))
         (agent-info (list :niwa-agent-invocation
                           (niwa-agent-invocation--create :goal-owner root)
                           :tokens-full (list :input 8 :output 2)))
         (agent (gptel-make-fsm :info agent-info)))
    (unwind-protect
        (progn
          (with-current-buffer root-buffer
            (setq-local niwa--session session
                        niwa--current-request
                        (niwa-request--create :session session :fsm root)))
          (niwa-goal-charge-agent-progress agent)
          (should (= 50 (niwa-goal-tokens-used goal)))
          (with-current-buffer root-buffer
            (should (assq 'goal-budget-50
                          (plist-get niwa-reminders--turn-events :items))))
          (should-not (niwa-session-pending-reminders session))
          ;; Only usage since the last charge counts.
          (plist-put (gptel-fsm-info agent) :tokens-full (list :input 12 :output 3))
          (niwa-goal-charge-agent-progress agent)
          (should (= 55 (niwa-goal-tokens-used goal)))
          (niwa-goal-charge-agent-progress agent)
          (should (= 55 (niwa-goal-tokens-used goal)))
          ;; After the root turn settled, a crossing waits for the next request.
          (plist-put (gptel-fsm-info root) :niwa-goal-accounted t)
          (plist-put (gptel-fsm-info agent) :tokens-full (list :input 40 :output 0))
          (niwa-goal-charge-agent-progress agent)
          (should (= 80 (niwa-goal-tokens-used goal)))
          (should (string-match-p "crossed 80%"
                                  (car (niwa-session-pending-reminders session)))))
      (kill-buffer root-buffer)))

  :doc "records missing native child usage only at terminal settlement"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :objective "Ship" :status 'active
                                      :token-budget 100 :tokens-used 0))
           (session (niwa-session--create :name "main" :goal goal))
           (root (gptel-make-fsm :info (list :buffer (current-buffer)
                                             :niwa-goal-accounting-id "g")))
           (backend (progn (require 'niwa-claude-code)
                           (niwa-claude-code--make-backend :name "Native")))
           (agent (gptel-make-fsm
                   :info (list :backend backend :niwa-acp-prompted t :tokens-full '(:input 12)
                               :niwa-agent-invocation
                               (niwa-agent-invocation--create :goal-owner root)))))
      (setq-local niwa--session session
                  niwa--current-request (niwa-request--create :session session :fsm root))
      (niwa-goal-charge-agent-progress agent)
      (should (= 12 (niwa-goal-tokens-used goal)))
      (should-not (niwa-goal-tokens-incomplete-p goal))
      (niwa-goal-charge-agent-progress agent t)
      (should (= 12 (niwa-goal-tokens-used goal)))
      (should (niwa-goal-tokens-incomplete-p goal))
      ;; The running root may still complete or block the Goal before settling.
      (should (eq 'active (niwa-goal-status goal)))
      (should (assq 'goal-usage-incomplete (plist-get niwa-reminders--turn-events :items)))
      (plist-put (gptel-fsm-info root) :niwa-goal-accounted t)
      (niwa-goal-charge-agent-progress agent t)
      (should (eq 'budget-limited (niwa-goal-status goal)))
      ;; A late event must not poison a replacement Goal.
      (setf (niwa-session-goal session)
            (niwa-goal--create :id "replacement" :status 'active :tokens-used 0))
      (niwa-goal-charge-agent-progress agent t)
      (should-not (niwa-goal-tokens-incomplete-p (niwa-session-goal session)))))

  :doc "leaves agents without a Goal uncharged"
  (let ((agent (gptel-make-fsm
                :info (list :niwa-agent-invocation
                            (niwa-agent-invocation--create)
                            :tokens-full (list :input 8 :output 2)))))
    (should-not (niwa-goal-charge-agent-progress agent))))

(niwa-deftest niwa-goal-agent-budget-notice ()
  ,test
  (test)
  :doc "tells an agent the highest threshold reached, once, counting root in-flight usage"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :objective "Ship" :status 'active
                                       :token-budget 100 :tokens-used 70))
           (root (gptel-make-fsm
                  :info (list :buffer (current-buffer)
                              :niwa-goal-accounting-id "g"
                              :tokens-full (list :input 10 :output 5))))
           (agent (gptel-make-fsm
                   :info (list :niwa-agent-invocation
                               (niwa-agent-invocation--create :goal-owner root)))))
      (setq-local niwa--session
                  (niwa-session--create :name "main" :goal goal))
      (let ((notice (niwa-goal-agent-budget-notice agent)))
        (should (string-match-p "used 80% of its token budget (85/100"
                                (plist-get notice :body)))
        (funcall (plist-get notice :commit)))
      (should-not (niwa-goal-agent-budget-notice agent))
      (setf (niwa-goal-tokens-used goal) 90)
      (should (string-match-p "whole token budget (105/100.*return your findings"
                              (plist-get (niwa-goal-agent-budget-notice agent)
                                         :body)))
      (setf (niwa-goal-status goal) 'paused)
      (should-not (niwa-goal-agent-budget-notice agent)))))

(niwa-deftest niwa-goal--end-running-turn
  ()
  ,test
  (test)
  :doc "pause and a reached budget end only the running turn charged to the Goal"
  (dolist (case '((pause "goal-1" goal-paused)
                  (pause "other" nil)
                  (budget "goal-1" goal-budget-limited)))
    (with-temp-buffer
      (let* ((goal (niwa-goal--create
                    :id "goal-1" :objective "Ship" :status 'active
                    :token-budget 100 :tokens-used 60))
             (session (niwa-session--create :name "main" :goal goal))
             (fsm (gptel-make-fsm
                   :info (list :buffer (current-buffer)
                               :niwa-goal-accounting-id (nth 1 case))))
             woken)
        (setq-local niwa--session session
                    niwa--current-request
                    (niwa-request--create :session session :fsm fsm))
        (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                  ((symbol-function 'niwa-agent-control-steer-user)
                   (lambda (_session message) (setq woken message))))
          (pcase (car case)
            ('pause (niwa-goal-pause))
            ('budget (niwa-goal-set-budget "50"))))
        (should (eq (nth 2 case)
                    (niwa-turn-end-requested-p (gptel-fsm-info fsm))))
        ;; A turn waiting in WaitAgent is woken to reach its boundary.
        (should (equal (and (nth 2 case) t)
                       (and woken (string-match-p "Stop here" woken) t)))))))

(niwa-deftest niwa-goal-edit
  ()
  ,test
  (test)
  :doc "keeps queued follow-ups held when editing a paused Goal"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "old" :status 'paused :objective "Old"))
           (session (niwa-session--create
                     :name "main" :goal goal
                     :pending-follow-ups
                     (list (list :input "Finish checks" :queued-at-goal-id "old")
                           (list :input "Unrelated" :queued-at-goal-id "other")))))
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore))
        (niwa-goal-edit "New"))
      (should (niwa-view--follow-up-auto-drain-blocked-p session))
      (should (equal "other" (plist-get (cadr (niwa-session-pending-follow-ups session))
                                        :queued-at-goal-id)))))

  :doc "rotates identity, retains the run, and refreshes an in-flight Goal"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create
                :id "goal-1" :objective "Old" :status 'active
                :token-budget 100 :tokens-used 20
                :time-used-seconds 30 :turns-run 4
                :plan-reference "accepted-plan.md"
                :created-at "created" :updated-at "old-update"))
         (buffer (generate-new-buffer " *niwa-goal-edit*"))
         (request-fsm
          (gptel-make-fsm
           :info (list :buffer buffer :niwa-goal-id "goal-1"
                       :niwa-goal-accounting-id "goal-1"
                       :tokens-full '(:input 3 :output 2))))
         (request (niwa-request--create
                   :session session :fsm request-fsm))
         saved scheduled refreshed)
    (unwind-protect
        (progn
          (setf (niwa-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local niwa--session session
                        niwa--current-request request))
          (with-current-buffer buffer
            (should-error (niwa-goal-edit "  ") :type 'user-error))
          (should (equal "goal-1" (niwa-goal-id goal)))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (&rest _) (setq saved t)))
                    ((symbol-function 'niwa-goal-new-id)
                     (lambda () "goal-2"))
                    ((symbol-function 'niwa-goal-active-context)
                     (lambda (_session) "updated context"))
                    ((symbol-function 'niwa-agent-control-steer-user)
                     (lambda (actual-session message)
                       (setq refreshed (list actual-session message))))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest args) (setq scheduled args))))
            (with-current-buffer buffer
              (should (eq goal (niwa-goal-edit "  New objective  ")))))
          (should saved)
          (should scheduled)
          (should (equal (list session "updated context") refreshed))
          (should (eq 'goal-edited
                      (niwa-turn-end-requested-p
                       (gptel-fsm-info request-fsm))))
          (should (equal "goal-2" (niwa-goal-id goal)))
          (should (equal "New objective" (niwa-goal-objective goal)))
          (should (eq 'active (niwa-goal-status goal)))
          (should (= 100 (niwa-goal-token-budget goal)))
          (should (= 20 (niwa-goal-tokens-used goal)))
          (should (= 30 (niwa-goal-time-used-seconds goal)))
          (should (= 4 (niwa-goal-turns-run goal)))
          (should (equal "accepted-plan.md"
                         (niwa-goal-plan-reference goal)))
          (should (equal "created" (niwa-goal-created-at goal)))
          (should-not (equal "old-update" (niwa-goal-updated-at goal)))
          (should (= 1 (length (niwa-session-pending-reminders session))))
          (should (string-match-p
                   "New objective"
                   (car (niwa-session-pending-reminders session))))
          (with-current-buffer buffer
            (should (eq 'request
                        (niwa-goal-continue-if-idle session buffer))))
          (should-error
           (niwa-tool-goal-update 'complete nil session "goal-1"))
          (should (equal "goal-1"
                         (plist-get (gptel-fsm-info request-fsm)
                                    :niwa-goal-id)))
          (should (equal "goal-2"
                         (plist-get (gptel-fsm-info request-fsm)
                                    :niwa-goal-accounting-id)))
          (niwa-goal-settle-turn request-fsm)
          (should (= 25 (niwa-goal-tokens-used goal)))
          (should (= 5 (niwa-goal-turns-run goal)))
          (should (equal "Goal status changed to complete"
                         (niwa-tool-goal-update
                          'complete nil session "goal-2"))))
      (kill-buffer buffer))))

(niwa-deftest niwa-goal-dispatch-after-turn
  (:doc "continues any active Goal after success but only its own after failure")
  (with-temp-buffer
    (let ((scheduled 0)
          delay
          (unattributed (gptel-make-fsm :info (list :buffer (current-buffer))))
          (attributed (gptel-make-fsm :info (list :buffer (current-buffer)
                                                  :niwa-goal-id "g"))))
      (setq-local niwa--session (niwa-session--create :name "main"))
      (cl-letf (((symbol-function 'niwa-goal-schedule-continuation)
                 (lambda (&rest args)
                   (cl-incf scheduled)
                   (setq delay (nth 3 args)))))
        (niwa-goal-dispatch-after-turn unattributed)
        (should (= 0 scheduled))
        (niwa-goal-dispatch-after-turn unattributed t)
        (should (= 1 scheduled))
        (should-not delay)
        (niwa-goal-dispatch-after-turn attributed)
        (should (= 2 scheduled))
        (should (eql 0 delay))
        ;; A retried transient failure waits out its backoff delay.
        (setq-local niwa-goal--transient-retries 2)
        (niwa-goal-dispatch-after-turn attributed)
        (should (eql 30 delay))
        (niwa-goal-dispatch-after-turn attributed t)
        (should-not delay)))))

(niwa-deftest niwa-goal-schedule-continuation
  (:doc "defers the continuation check for a live buffer by the given delay")
  (with-temp-buffer
    (let ((session (niwa-session--create :name "main"))
          scheduled)
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args) (push args scheduled))))
        (niwa-goal-schedule-continuation)
        (niwa-goal-schedule-continuation session (current-buffer) nil 15)
        (setq-local niwa--session nil)
        (niwa-goal-schedule-continuation))
      (should (equal `((15 nil niwa-goal--scheduled-continuation
                           ,session ,(current-buffer) nil)
                       (0 nil niwa-goal--scheduled-continuation
                          ,session ,(current-buffer) nil))
                     scheduled)))))

(niwa-deftest niwa-goal--known-token-count
  (:doc "distinguishes zero normalized usage from unavailable usage and applies the baseline")
  (progn
    (should (= 0 (niwa-goal--known-token-count
                  '(:tokens-full (:input 0 :output 0 :cached 1000)))))
    (should-not (niwa-goal--known-token-count '(:tokens-full nil)))
    (should (= 5 (niwa-goal--known-token-count
                  '(:tokens-full (:input 20 :output 5)
                                 :niwa-goal-token-baseline 20))))))

(niwa-deftest niwa-goal--scheduled-continuation
  (:quiet t)
  ,test
  (test)
  :doc "pauses a Goal with the concrete dispatch failure instead of leaving it active"
  (with-temp-buffer
    (let* ((goal (niwa-goal--create :id "g" :status 'active))
           (session (niwa-session--create :name "main" :goal goal))
           saved)
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (_key _path thunk) (funcall thunk)))
                ((symbol-function 'niwa--submit-generated-turn)
                 (lambda (&rest _) (error "Provider unavailable")))
                ((symbol-function 'niwa-session-artifacts-save)
                 (lambda (&rest _) (setq saved t))))
        (niwa-goal--scheduled-continuation session (current-buffer) nil))
      (should saved)
      (should (eq 'paused (niwa-goal-status goal)))
      (should (string-match-p "Provider unavailable" (niwa-goal-reason goal))))))

(niwa-deftest niwa-goal--native-usage-incomplete-p ()
  (let ((backend (progn (require 'niwa-claude-code)
                         (niwa-claude-code--make-backend :name "Native"))))
    (should-not (niwa-goal--native-usage-incomplete-p nil))
    (dolist (fields '(nil (:tokens-full (:input 12))
                      (:tokens-full (:input 12 :output 3) :niwa-usage-pending t)
                      (:tokens-full (:input 12 :output 3) :niwa-usage-incomplete t)))
      (should (niwa-goal--native-usage-incomplete-p (cons :backend (cons backend fields)))))
    (should-not (niwa-goal--native-usage-incomplete-p
                 (list :backend backend :tokens-full '(:input 0 :output 0))))))

(provide 'test-niwa-goal)
;;; test-niwa-goal.el ends here
