;;; test-niwa-skills-invoke.el -- Skill invocation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests request context, invocation, fork dispatch, and model tools.

;;; Code:

(require 'niwa-tool-exec-registration)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'gptel-openai)
(require 'niwa-agent-control)
(require 'niwa-agents)
(require 'niwa-bash-analysis)
(require 'niwa-hooks)
(require 'niwa-models)
(require 'niwa-permissions)
(require 'niwa-pipeline)
(require 'niwa-skills-core)
(require 'niwa-skills-invoke)
(require 'niwa-skills-preparation)
(require 'niwa-structs)
(require 'niwa-tool-exec)
(require 'niwa-tool-render-data)
(require 'niwa-view-render)
(require 'niwa-tools)
(require 'niwa-workspace)



;;
;;; Invocation helpers and core behavior

(defun niwa-skills-test--expansion-fn (_event)
  "Test hook used by skill expansion."
  '(:updated-input "Expanded by hook"
                   :additional-context "expansion context"))

(defun niwa-skills-test--block-expansion-fn (_event)
  "Test hook that blocks skill expansion."
  '(:continue nil :stop-reason "blocked expansion"))


;;
;;; Phase B -- substitution, shell injection, execution


;;
;;; Request-scoped skill context

(niwa-deftest niwa-skills-activate-context
  (:doc "model and internal grants append only to the innermost owner")
  (dolist (origin '(model internal))
    (dolist (request-p '(nil t))
      (dolist (invocation-p '(nil t))
        (let ((request (niwa-request--create
                        :skill-permission-rules '(existing-rule)
                        :hook-rules '(existing-hook)))
              (invocation (niwa-agent-invocation--create
                           :skill-permission-rules '(existing-rule)
                           :hook-rules '(existing-hook))))
          (with-temp-buffer
            (setq-local niwa--current-request (and request-p request)
                        niwa--agent-invocation (and invocation-p invocation))
            (niwa-skills-activate-context
             origin :permission-rules '(new-rule) :hook-rules '(new-hook)))
          (should (equal (if invocation-p '(existing-rule new-rule)
                           '(existing-rule))
                         (niwa-agent-invocation-skill-permission-rules invocation)))
          (should (equal (if invocation-p '(existing-hook new-hook)
                           '(existing-hook))
                         (niwa-agent-invocation-hook-rules invocation)))
          (should (equal (if (and request-p (not invocation-p))
                             '(existing-rule new-rule) '(existing-rule))
                         (niwa-request-skill-permission-rules request)))
          (should (equal (if (and request-p (not invocation-p))
                             '(existing-hook new-hook) '(existing-hook))
                         (niwa-request-hook-rules request))))))))

(niwa-deftest niwa-skills-commit-invoked-records ()
  ,test
  (test)
  :doc "appends prepared records in order and treats nil as a no-op"
  (let* ((session (niwa-skills-test--make-session))
         (first (niwa-skill-invocation-record--create :name "first"))
         (second (niwa-skill-invocation-record--create :name "second")))
    (niwa-skills-commit-invoked-records session (list first))
    (niwa-skills-commit-invoked-records session nil)
    (niwa-skills-commit-invoked-records session (list second))
    (should (equal (list first second)
                   (niwa-session-invoked-skills session)))))

(niwa-deftest niwa-skills--drain-pending-context ()
  ,test
  (test)
  :doc "drain commits non-policy context and clears the buffer-local stash"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/d"))
         (request (niwa-request--create :session session))
         (rules '(("Bash" :pattern "echo *" :action allow)))
         (records (list (niwa-skill-invocation-record--create
                         :name "demo" :args "x"
                         :role 'command :origin 'user
                         :turn 1 :source-path "/tmp/demo/SKILL.md"
                         :prepared-body "Hello"))))
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa-skills--pending-request-context
                  (list :permission-rules rules
                        :model (niwa-model-tier-selector 'fast)
                        :effort 'high
                        :ptc-primitives nil
                        :invoked-skills records))
      (niwa-skills--drain-pending-context request)
      (should (equal rules
                     (niwa-request-skill-permission-rules request)))
      (should (null (niwa-request-ptc-primitives request)))
      (should (equal records (niwa-session-invoked-skills session)))
      ;; Stash is cleared after drain.
      (should (null niwa-skills--pending-request-context))))

  :doc "drain is a no-op when no stash present"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/d"))
         (request (niwa-request--create :session session)))
    (with-temp-buffer
      (setq-local niwa--session session)
      ;; No stash.
      (niwa-skills--drain-pending-context request)
      (should (null (niwa-request-skill-permission-rules request))))))

(niwa-deftest niwa-skills--transform-apply-request-model-policy ()
  ,test
  (test)
  :doc "pending slash tier sets prompt-buffer backend and model locals"
  (niwa-skills-test--with-model-backends
   (let* ((session (niwa-skills-test--make-session))
          (niwa-model-tiers
           '((fast :provider "Fast:fast-model")
             (balanced :provider "Balanced:balanced-model")
             (strong)))
          (niwa-model-workloads '((planning :tier balanced)))
          (chat (generate-new-buffer " *skill-model-chat*")))
     (unwind-protect
         (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
           (with-current-buffer chat
             (setf (niwa-session-plan-mode session) t)
             (setq-local niwa--session session
                         niwa-skills--pending-request-context
                         (list :model (niwa-model-tier-selector 'fast))))
           (with-temp-buffer
             (setq-local gptel-backend (gptel-get-backend "Balanced"))
             (setq-local gptel-model 'balanced-model)
             (niwa-skills--transform-apply-request-model-policy fsm)
             (should (equal "Fast" (gptel-backend-name gptel-backend)))
             (should (eq 'fast-model gptel-model))))
       (kill-buffer chat))))

  :doc "Plan root requests use planning while retained agents keep their base"
  (niwa-skills-test--with-model-backends
   (let* ((session (niwa-skills-test--make-session))
          (niwa-model-tiers
           '((fast :provider "Fast:fast-model")
             (balanced :provider "Balanced:balanced-model")))
          (niwa-model-workloads '((planning :tier fast)))
          (chat (generate-new-buffer " *planning-model-chat*")))
     (unwind-protect
         (progn
           (setf (niwa-session-plan-mode session) t)
           (with-current-buffer chat
             (setq-local niwa--session session
                         gptel-backend (gptel-get-backend "Balanced")
                         gptel-model 'balanced-model))
           (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
             (with-temp-buffer
               (niwa-skills--transform-apply-request-model-policy fsm)
               (should (eq 'fast-model gptel-model)))
             (with-current-buffer chat
               (setq-local niwa--agent-invocation t))
             (with-temp-buffer
               (niwa-skills--transform-apply-request-model-policy fsm)
               (should (eq 'balanced-model gptel-model)))))
       (kill-buffer chat))))

  :doc "invalid planning policy fails before request realization"
  (niwa-skills-test--with-model-backends
   (let* ((session (niwa-skills-test--make-session))
          (niwa-model-workloads
           '((planning :tier fast :provider "Fast:fast-model")))
          (chat (generate-new-buffer " *invalid-planning-model-chat*")))
     (unwind-protect
         (progn
           (setf (niwa-session-plan-mode session) t)
           (with-current-buffer chat
             (setq-local niwa--session session))
           (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
             (with-temp-buffer
               (should-error
                (niwa-skills--transform-apply-request-model-policy fsm)
                :type 'user-error))))
       (kill-buffer chat))))

  :doc "pending concrete provider sets prompt-buffer backend and model locals"
  (niwa-skills-test--with-model-backends
   (let ((chat (generate-new-buffer " *skill-model-chat*")))
     (unwind-protect
         (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
           (with-current-buffer chat
             (setq-local niwa-skills--pending-request-context
                         (list :model
                               (niwa-model-resolve-provider
                                "Balanced:balanced-model"))))
           (with-temp-buffer
             (setq-local gptel-backend (gptel-get-backend "Fast"))
             (setq-local gptel-model 'fast-model)
             (niwa-skills--transform-apply-request-model-policy fsm)
             (should (equal "Balanced" (gptel-backend-name gptel-backend)))
             (should (eq 'balanced-model gptel-model))))
       (kill-buffer chat))))

  :doc "pending effort uses gptel validation and reaches the prompt buffer"
  (niwa-skills-test--with-model-backends
   (let ((chat (generate-new-buffer " *skill-effort-chat*"))
         (old-custom (get 'gptel-reasoning-effort 'custom-type))
         (old-effort (get 'fast-model :reasoning-effort)))
     (unwind-protect
         (progn
           (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
           (put 'fast-model :reasoning-effort '(member low high))
           (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
             (with-current-buffer chat
               (setq-local niwa-skills--pending-request-context
                           (list :model
                                 (niwa-model-resolve-provider
                                  "Fast:fast-model")
                                 :effort 'high)))
             (with-temp-buffer
               (niwa-skills--transform-apply-request-model-policy fsm)
               (should (eq 'fast-model gptel-model))
               (should (eq 'high gptel-reasoning-effort))
               (should (eq 'high
                           (plist-get (gptel-fsm-info fsm)
                                      :reasoning-effort))))))
       (put 'gptel-reasoning-effort 'custom-type old-custom)
       (put 'fast-model :reasoning-effort old-effort)
       (kill-buffer chat)))))


(niwa-deftest niwa-skills--invoke-inline/elisp-injection ()
  ,test
  (test)
  :doc "skill allowed-tools [Eval] authorizes elisp body injection end to end"
  (niwa-tool-exec--register)
  (let* ((niwa-skills-include-bundled nil)
         (root (make-temp-file "niwa-skills-eval-" t))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         outcome)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           root "eval-skill"
           "name: eval-skill
allowed-tools:
  - Eval
" "result=!el`(+ 2 4)`")
          (let ((skill (car (niwa-skills-scan root '(".")))))
            (with-temp-buffer
              (setq-local niwa--session session)
              (niwa-skills-invoke
               skill nil (lambda (o) (setq outcome o))
               :origin 'user)
              (while (null outcome)
                (accept-process-output nil 0.01)))
            (should (eq 'ok (plist-get outcome :status)))
            (should (equal "result=6" (plist-get outcome :body)))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

;;
;;; niwa-skills-invoke (unified invocation API)

(niwa-deftest niwa-skills--preparation-rejection ()
  ,test
  (test)
  :doc "rejects invalid skill, role, origin, and named fork agent"
  (should (eq 'unknown-skill
              (plist-get (niwa-skills--preparation-rejection
                          nil 'command 'user)
                         :reason)))
  (let ((skill (niwa-skill--create :name "alpha" :body "Body")))
    (should (eq 'invalid-role
                (plist-get (niwa-skills--preparation-rejection
                            skill 'other 'user)
                           :reason)))
    (should (eq 'invalid-origin
                (plist-get (niwa-skills--preparation-rejection
                            skill 'command 'other)
                           :reason))))
  (let ((skill (niwa-skill--create
                :name "alpha" :body "Body" :context 'fork :agent "missing")))
    (cl-letf (((symbol-function 'niwa-agent-get) (lambda (_) nil)))
      (should (eq 'unknown-agent
                  (plist-get (niwa-skills--preparation-rejection
                              skill 'command 'user)
                             :reason)))))

  :doc "accepts a valid preparation request"
  (let ((skill (niwa-skill--create :name "alpha" :body "Body")))
    (should-not
     (niwa-skills--preparation-rejection skill 'command 'model))))

(niwa-deftest niwa-skills--preparation-policy ()
  ,test
  (test)
  :doc "an owner merges preset policy and validates its request workload"
  (let* ((skill (niwa-skill--create
                 :name "alpha" :context 'fork :agent "reviewer"
                 :model "fast" :effort 'low))
         (niwa-model-tiers '((fast) (strong)))
         (niwa-model-workloads '(($alpha :tier strong :effort high)))
         call
         outcome)
    (cl-letf (((symbol-function 'niwa-model-resolve-workload)
               (lambda (workload selector effort)
                 (setq call (list workload selector effort))
                 '(:backend inherited :model inherited :effort high))))
      (setq outcome (niwa-skills--preparation-policy skill 'user t)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal '(:tier strong) (plist-get outcome :model)))
    (should (eq 'high (plist-get outcome :effort)))
    (should (equal '("reviewer" (:tier strong) high) call)))

  :doc "a model-side inline non-owner reports fields without parsing them"
  (let* ((skill (niwa-skill--create
                 :name "alpha" :context 'inline
                 :model "invalid selector" :effort 'impossible))
         (niwa-model-workloads
          '(($alpha :tier missing :provider "Missing:model"))))
    (cl-letf (((symbol-function 'niwa-model-merge-skill-policy)
               (lambda (&rest _) (ert-fail "non-owner merged policy")))
              ((symbol-function 'niwa-model-resolve-workload)
               (lambda (&rest _) (ert-fail "non-owner resolved policy"))))
      (let ((outcome
             (niwa-skills--preparation-policy skill 'model nil)))
        (should (eq 'ok (plist-get outcome :status)))
        (should (equal '(model effort)
                       (plist-get outcome :ignored-fields))))))

  :doc "a non-model non-owner silently retains session policy"
  (let ((skill (niwa-skill--create
                :name "alpha" :model "invalid" :effort 'impossible)))
    (should (equal '(:status ok :ignored-fields nil)
                   (niwa-skills--preparation-policy skill 'user nil))))

  :doc "owner validation failures become structured invocation failures"
  (let ((skill (niwa-skill--create :name "alpha" :model "fast"))
        outcome)
    (cl-letf (((symbol-function 'niwa-model-resolve-workload)
               (lambda (&rest _) (user-error "Unsupported effort"))))
      (setq outcome (niwa-skills--preparation-policy skill 'user t)))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'invalid-policy (plist-get outcome :reason)))
    (should (equal "Unsupported effort" (plist-get outcome :message)))))

(niwa-deftest niwa-skills--preparation-settler ()
  ,test
  (test)
  :doc "between turns, holds the slot with a preparation request and settles once"
  (let* ((session (niwa-skills-test--make-session))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse nil)))
         outcomes)
    (with-temp-buffer
      (let ((settle (niwa-skills--preparation-settler
                     session rules hooks
                     (lambda (outcome) (push outcome outcomes)))))
        (should (eq session
                    (niwa-request-session niwa--current-request)))
        (should (equal rules
                       (niwa-request-skill-permission-rules
                        niwa--current-request)))
        (should (equal hooks
                       (niwa-request-hook-rules niwa--current-request)))
        (funcall settle 'first)
        (should-not niwa--current-request)
        (funcall settle 'second)
        (should (equal '(first) outcomes)))))

  :doc "a request admitted after preparation teardown keeps the slot"
  (let ((session (niwa-skills-test--make-session))
        (admitted (niwa-request--create :id "admitted")))
    (with-temp-buffer
      (let ((settle (niwa-skills--preparation-settler
                     session nil nil #'ignore)))
        (setq-local niwa--current-request admitted)
        (funcall settle 'done)
        (should (eq admitted niwa--current-request)))))

  :doc "during a turn, lends the skill's rules to the turn's own request"
  ;; Regression: preparation used to take the turn's slot, so a turn ending
  ;; meanwhile ended nothing and was put back afterwards, running forever,
  ;; and an ACP turn took the stand-in for a replacement and stopped.
  (let* ((session (niwa-skills-test--make-session))
         (earlier '("Bash" :action allow))
         (rules (list earlier '("Read" :action allow)))
         (hooks '((PreToolUse nil)))
         (turn (niwa-request--create :session session
                                        :skill-permission-rules (list earlier)))
         outcomes)
    (with-temp-buffer
      (setq-local niwa--current-request turn)
      (let ((settle (niwa-skills--preparation-settler
                     session rules hooks
                     (lambda (outcome) (push outcome outcomes)))))
        (should (eq turn niwa--current-request))
        (should (equal (cons earlier rules)
                       (niwa-request-skill-permission-rules turn)))
        (should (equal hooks (niwa-request-hook-rules turn)))
        ;; The turn ends while its skill prepares; nothing puts it back.
        (setq-local niwa--current-request nil)
        (funcall settle 'first)
        (funcall settle 'second)
        (should-not niwa--current-request)
        (should (equal (list earlier) (niwa-request-skill-permission-rules turn)))
        (should-not (niwa-request-hook-rules turn))
        (should (equal '(first) outcomes)))))

  :doc "during an agent's turn, lends the rules to its invocation"
  (let* ((session (niwa-skills-test--make-session))
         (rules '(("Read" :action allow)))
         (turn (niwa-request--create :session session))
         (invocation (niwa-agent-invocation--create
                      :path "/root/explore" :parent-session session)))
    (with-temp-buffer
      (setq-local niwa--current-request turn)
      (setq-local niwa--agent-invocation invocation)
      (let ((settle (niwa-skills--preparation-settler
                     session rules nil #'ignore)))
        (should (eq invocation niwa--agent-invocation))
        (should (equal rules (niwa-agent-invocation-skill-permission-rules
                              invocation)))
        (should-not (niwa-request-skill-permission-rules turn))
        (funcall settle 'done)
        (should-not (niwa-agent-invocation-skill-permission-rules invocation)))))

  :doc "keeps the caller's Plan read-only authority for preparation commands"
  (let ((session (niwa-skills-test--make-session)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create
                   :path "/root/explore" :parent-session session
                   :plan-read-only t))
      (let ((settle (niwa-skills--preparation-settler
                     session nil nil #'ignore)))
        (should-not niwa--agent-invocation)
        (should (niwa-request-plan-read-only niwa--current-request))
        (should (niwa-plan-read-only-p session))
        (funcall settle 'done)
        (should (niwa-agent-invocation-plan-read-only
                 niwa--agent-invocation))))
    (with-temp-buffer
      (setq-local niwa--session session)
      (setf (niwa-session-plan-mode session) t)
      (niwa-skills--preparation-settler session nil nil #'ignore)
      (should (niwa-request-plan-read-only niwa--current-request)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (setf (niwa-session-plan-mode session) nil)
      (niwa-skills--preparation-settler session nil nil #'ignore)
      (should-not (niwa-request-plan-read-only
                   niwa--current-request)))))

(niwa-deftest niwa-skills--preparation-success-outcome ()
  ,test
  (test)
  :doc "builds command body, policy context, and invocation record"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "alpha" :body "Body" :source-file "/tmp/alpha/SKILL.md"))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse nil)))
         (metadata
          (list :skill skill :arguments "task" :role 'command
                :origin 'user :session session :rules rules :hooks hooks
                :model '(:tier fast) :effort 'high))
         (outcome (niwa-skills--preparation-success-outcome
                   metadata "original" "expanded"
                   '(:additional-context ("expansion context"))))
         (context (plist-get outcome :request-context))
         (record (car (plist-get context :invoked-skills))))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "expanded" (plist-get outcome :body)))
    (should (string-match-p "expansion context"
                            (plist-get outcome :hook-context)))
    (should (equal rules (plist-get context :permission-rules)))
    (should (equal hooks (plist-get context :hook-rules)))
    (should (equal '(:tier fast) (plist-get context :model)))
    (should (eq 'high (plist-get context :effort)))
    (should (equal "alpha" (niwa-skill-invocation-record-name record)))
    (should (equal "task" (niwa-skill-invocation-record-args record)))
    (should (eq 'command (niwa-skill-invocation-record-role record)))
    (should (eq 'user (niwa-skill-invocation-record-origin record)))
    (should (equal "/root"
                   (niwa-skill-invocation-record-agent-path record)))
    (should (equal "expanded"
                   (niwa-skill-invocation-record-prepared-body record))))

  :doc "instruction outcome omits command policy"
  (let* ((skill (niwa-skill--create
                 :name "alpha" :body "Body" :context 'fork))
         (metadata (list :skill skill :arguments "" :role 'instruction
                         :origin 'user :rules '(ignored)
                         :model 'ignored :effort 'ignored :hooks '(ignored)))
         (outcome (niwa-skills--preparation-success-outcome
                   metadata "original" "expanded" nil))
         (context (plist-get outcome :request-context)))
    (should (eq 'instruction (plist-get outcome :kind)))
    (should-not (plist-get context :permission-rules))
    (should-not (plist-get context :model))
    (should-not (plist-get context :effort))
    (should-not (plist-get context :hook-rules))))

(niwa-deftest niwa-skills-prepare ()
  ,test
  (test)
  :doc "instruction preparation isolates command metadata and forces empty args"
  (let* ((session (niwa-skills-test--make-session))
         (rules '(("Bash" :pattern "echo *" :action allow)))
         (hooks '((PreToolUse ((:hooks nil)))))
         (skill (niwa-skill--create
                 :name "alpha"
                 :body "$ARGUMENTS|$0"
                 :context 'fork
                 :model "fast"
                 :effort 'high
                 :agent "reviewer"
                 :allowed-tool-rules rules
                 :hooks hooks))
         outcome
         injection-rules
         expansion-hooks
         dispatched)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-skills-preparation-expand-body)
                 (lambda (body callback &rest _)
                   (setq injection-rules
                         (niwa-request-skill-permission-rules
                          niwa--current-request))
                   (funcall callback (list :status 'ok :body body))))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq expansion-hooks
                         (niwa-request-hook-rules
                          niwa--current-request))
                   (funcall callback nil)))
                ((symbol-function 'niwa-agent-control-spawn)
                 (lambda (&rest _args) (setq dispatched t))))
        (niwa-skills-prepare
         skill "ignored"
         (lambda (value) (setq outcome value))
         :role 'instruction :origin 'user)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'instruction (plist-get outcome :kind)))
    (should (equal "|" (plist-get outcome :body)))
    (should (equal rules injection-rules))
    (should-not expansion-hooks)
    (should-not dispatched)
    (let* ((context (plist-get outcome :request-context))
           (record (car (plist-get context :invoked-skills))))
      (should-not (plist-get context :permission-rules))
      (should-not (plist-get context :model))
      (should-not (plist-get context :effort))
      (should-not (plist-get context :hook-rules))
      (should (eq 'instruction
                  (niwa-skill-invocation-record-role record)))
      (should (eq 'user
                  (niwa-skill-invocation-record-origin record)))))

  :doc "a turn's preparation runs commands only while that turn does"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "alpha" :body "body"))
         (turn (niwa-request--create :session session))
         live-p)
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa--current-request turn)
      (cl-letf (((symbol-function 'niwa-skills-preparation-expand-body)
                 (lambda (_body _callback _skill _session live)
                   (setq live-p live))))
        (niwa-skills-prepare skill "" #'ignore :role 'instruction :origin 'user))
      (should (funcall live-p))
      (setf (niwa-request-cancelled-p turn) t)
      (should-not (funcall live-p))
      (setf (niwa-request-cancelled-p turn) nil)
      (setq-local niwa--current-request (niwa-request--create :session session))
      (should-not (funcall live-p))))

  :doc "command preparation returns policy context without committing it"
  (let* ((session (niwa-skills-test--make-session))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse ((:hooks nil)))))
         (skill (niwa-skill--create
                 :name "alpha" :body "Do $ARGUMENTS"
                 :model "fast" :effort 'high
                 :allowed-tool-rules rules :hooks hooks))
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'niwa-model-resolve-workload)
                 (lambda (&rest _) '(:model inherited :effort high))))
        (niwa-skills-prepare
         skill "work"
         (lambda (value) (setq outcome value))
         :role 'command :origin 'user :policy-owner-p t)
        (should-not niwa-skills--pending-request-context)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "Do work" (plist-get outcome :body)))
    (let* ((context (plist-get outcome :request-context))
           (record (car (plist-get context :invoked-skills))))
      (should (equal rules (plist-get context :permission-rules)))
      (should (equal hooks (plist-get context :hook-rules)))
      (should (equal (niwa-model-tier-selector 'fast)
                     (plist-get context :model)))
      (should (eq 'high (plist-get context :effort)))
      (should (eq 'command
                  (niwa-skill-invocation-record-role record)))
      (should (eq 'user
                  (niwa-skill-invocation-record-origin record)))))

  :doc "user and model commands omit untrusted project skill hooks"
  (let* ((root (make-temp-file "niwa-skill-hook-origin-" t))
         (user-dir (make-temp-file "niwa-skill-hook-state-" t))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "hook-origin" workspace root))
         (niwa-user-dir (file-name-as-directory user-dir))
         (niwa-hooks-require-project-trust t)
         (niwa-skills-include-bundled nil))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           (file-name-concat root ".niwa" "skills") "hooked"
           "name: hooked
description: Hooked
hooks:
  PreToolUse:
    - matcher: Bash
      hooks:
        - type: elisp
          function: niwa-skills-test--hook-fn
" "Body")
          (let ((skill (car (niwa-skills-scan
                             root '(".niwa/skills") workspace))))
            (with-temp-buffer
              (setq-local niwa--session session)
              (dolist (origin '(user model))
                (let (outcome)
                  (niwa-skills-prepare
                   skill "" (lambda (value) (setq outcome value))
                   :role 'command :origin origin
                   :policy-owner-p (eq origin 'user))
                  (should (eq 'ok (plist-get outcome :status)))
                  (should-not
                   (plist-get (plist-get outcome :request-context)
                              :hook-rules)))))))
      (delete-directory root t)
      (delete-directory user-dir t)))

  :doc "remote project and same-target plugin shell bodies use Bash with native skill paths"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-skill-remote" t)))
         (remote-root (format "/niwamock:%s:%s"
                              (system-name) root))
         (native-skill-dir (file-name-concat root "skill"))
         (remote-skill-dir (file-name-concat remote-root "skill"))
         (workspace (niwa-workspace--create
                     :type 'file :id "remote-skill" :root remote-root
                     :name "remote-skill"
                     :file-cache (niwa-file-cache--create
                                  :table (make-hash-table :test #'equal)
                                  :order nil :total-bytes 0)))
         session
         calls)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session
                                                    (niwa-session-create "remote-skill" workspace remote-root))
                                              (with-temp-buffer
                                                (setq-local niwa--session session)
                                                (cl-letf (((symbol-function 'niwa-pipeline-run-tool)
                                                           (lambda (tool callback args)
                                                             (push (list (niwa-tool-name tool)
                                                                         (plist-get args :command))
                                                                   calls)
                                                             (funcall callback "expanded"))))
                                                  (dolist (source '(project plugin))
                                                    (let ((skill (niwa-skill--create
                                                                  :name (symbol-name source)
                                                                  :source source
                                                                  :source-dir remote-skill-dir
                                                                  :body (concat
                                                                         "dir=${NIWA_SKILL_DIR}|"
                                                                         "${CLAUDE_SKILL_DIR} shell=!`pwd`")))
                                                          outcome)
                                                      (niwa-skills-prepare
                                                       skill "" (lambda (value) (setq outcome value))
                                                       :role 'command :origin 'model)
                                                      (should (eq 'ok (plist-get outcome :status)))
                                                      (should
                                                       (equal (format "dir=%s|%s shell=expanded"
                                                                      native-skill-dir native-skill-dir)
                                                              (plist-get outcome :body))))))))
      (delete-directory root t))
    (should (equal '(("Bash" "pwd") ("Bash" "pwd")) calls)))

  :doc "discovers target project shell skills without dispatching local resources remotely"
  (let* ((target-root (file-name-as-directory
                       (make-temp-file "niwa-skill-target" t)))
         (client-root (file-name-as-directory
                       (make-temp-file "niwa-skill-client" t)))
         (project-skills (file-name-concat target-root ".niwa" "skills"))
         (user-skills (file-name-concat client-root "skills"))
         (remote-root (format "/niwamock:%s:%s"
                              (system-name) target-root))
         (workspace (niwa-workspace--create
                     :type 'file :id remote-root :root remote-root
                     :name "remote-discovery"
                     :file-cache (niwa-test-file-cache-create)))
         (niwa-skills-include-bundled nil)
         session
         calls
         project-outcome
         user-outcome)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           project-skills "project-shell"
           "name: project-shell\ndescription: Target shell resource\n"
           "project=!`printf project`")
          (niwa-skills-test--write-skill
           user-skills "user-shell"
           "name: user-shell\ndescription: Client shell resource\n"
           "user=!`printf user`")
          (niwa-test--with-local-shell-tramp nil
                                                (setq session
                                                      (niwa-session-create
                                                       "remote-discovery" workspace remote-root))
                                                (let* ((skills (niwa-skills-scan
                                                                remote-root
                                                                (list ".niwa/skills" user-skills)))
                                                       (project (cl-find "project-shell" skills
                                                                         :key #'niwa-skill-name :test #'equal))
                                                       (user (cl-find "user-shell" skills
                                                                      :key #'niwa-skill-name :test #'equal)))
                                                  (should (eq 'project (niwa-skill-source project)))
                                                  (should (file-remote-p (niwa-skill-source-file project)))
                                                  (should (eq 'user (niwa-skill-source user)))
                                                  (should-not (file-remote-p (niwa-skill-source-file user)))
                                                  (with-temp-buffer
                                                    (setq-local default-directory remote-root)
                                                    (setq-local niwa--session session)
                                                    (cl-letf (((symbol-function 'niwa-pipeline-run-tool)
                                                               (lambda (tool callback args)
                                                                 (push (list (niwa-tool-name tool)
                                                                             (plist-get args :command))
                                                                       calls)
                                                                 (funcall callback "expanded"))))
                                                      (niwa-skills-prepare
                                                       project "" (lambda (value) (setq project-outcome value))
                                                       :role 'command :origin 'model)
                                                      (niwa-skills-prepare
                                                       user "" (lambda (value) (setq user-outcome value))
                                                       :role 'command :origin 'model)))))
          (should (equal '(("Bash" "printf project")) calls))
          (should (eq 'ok (plist-get project-outcome :status)))
          (should (equal "project=expanded"
                         (plist-get project-outcome :body)))
          (should (eq 'resource-target (plist-get user-outcome :reason)))
          (should
           (equal '("project-shell")
                  (mapcar #'file-name-nondirectory
                          (directory-files project-skills nil
                                           directory-files-no-dot-files-regexp)))))
      (delete-directory target-root t)
      (delete-directory client-root t)))

  :doc "remote sessions refuse client-local and foreign shell bodies before Bash"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-skill-origin" t)))
         (remote-root (format "/niwamock:%s:%s"
                              (system-name) root))
         (foreign-root (format "/niwamock:foreign:%s" root))
         (workspace (niwa-workspace--create
                     :type 'file :id "remote-origin" :root remote-root
                     :name "remote-origin"
                     :file-cache (niwa-file-cache--create
                                  :table (make-hash-table :test #'equal)
                                  :order nil :total-bytes 0)))
         session
         dispatched)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("foreign")
                                              (setq session
                                                    (niwa-session-create "remote-origin" workspace remote-root))
                                              (with-temp-buffer
                                                (setq-local niwa--session session)
                                                (cl-letf (((symbol-function 'niwa-pipeline-run-tool)
                                                           (lambda (&rest _)
                                                             (setq dispatched t))))
                                                  (dolist (skill
                                                           (list
                                                            (niwa-skill--create
                                                             :name "user" :source 'user
                                                             :source-dir (file-name-concat root "user")
                                                             :body "!`pwd`")
                                                            (niwa-skill--create
                                                             :name "bundled" :source 'bundled
                                                             :source-dir (file-name-concat root "bundled")
                                                             :body "!`pwd`")
                                                            (niwa-skill--create
                                                             :name "managed" :source 'managed
                                                             :source-dir (file-name-concat root "managed")
                                                             :body "!`pwd`")
                                                            (niwa-skill--create
                                                             :name "client-plugin" :source 'plugin
                                                             :source-dir (file-name-concat root "plugin")
                                                             :body "!`pwd`")
                                                            (niwa-skill--create
                                                             :name "foreign-plugin" :source 'plugin
                                                             :source-dir (file-name-concat foreign-root "plugin")
                                                             :body "!`pwd`")))
                                                    (let (outcome)
                                                      (niwa-skills-prepare
                                                       skill "" (lambda (value) (setq outcome value))
                                                       :role 'command :origin 'model)
                                                      (should (eq 'error (plist-get outcome :status)))
                                                      (should (eq 'resource-target (plist-get outcome :reason)))
                                                      (should (string-match-p "execution target"
                                                                              (plist-get outcome :message))))))))
      (delete-directory root t))
    (should-not dispatched))

  :doc "remote sessions keep client-local Elisp body injections local"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-skill-elisp" t)))
         (remote-root (format "/niwamock:%s:%s"
                              (system-name) root))
         (workspace (niwa-workspace--create
                     :type 'file :id "remote-elisp" :root remote-root
                     :name "remote-elisp"
                     :file-cache (niwa-file-cache--create
                                  :table (make-hash-table :test #'equal)
                                  :order nil :total-bytes 0)))
         session
         (skill (niwa-skill--create
                 :name "user" :source 'user
                 :source-dir (file-name-concat root "user")
                 :body "value=!el`(+ 1 2)`"))
         outcome expression)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session
                                                    (niwa-session-create "remote-elisp" workspace remote-root))
                                              (with-temp-buffer
                                                (setq-local niwa--session session)
                                                (cl-letf (((symbol-function
                                                            'niwa-skills-preparation--run-elisp-expression-async)
                                                           (lambda (form _marker callback)
                                                             (setq expression form)
                                                             (funcall callback '(:status ok :output "3"))))
                                                          ((symbol-function 'niwa-pipeline-run-tool)
                                                           (lambda (&rest _)
                                                             (ert-fail "Elisp injection reached Bash"))))
                                                  (niwa-skills-prepare
                                                   skill "" (lambda (value) (setq outcome value))
                                                   :role 'command :origin 'model))))
      (delete-directory root t))
    (should (equal "(+ 1 2)" expression))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "value=3" (plist-get outcome :body)))))

(niwa-deftest niwa-skills-prepare-many ()
  ,test
  (test)
  :doc "preflights, deduplicates, and prepares dependency-first with isolated rules"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-graph-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (root-buffer (generate-new-buffer " *niwa-skill-graph-root*"))
         session
         refresh-buffer
         expanded
         outcome)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n"
           "Child($ARGUMENTS)")
          (niwa-skills-test--write-skill
           dir "first" "name: first\ndescription: First\n"
           "First $0\n!$child -- $0")
          (niwa-skills-test--write-skill
           dir "second" "name: second\ndescription: Second\n"
           "Second\n!$child -- VALUE")
          (setq session (niwa-session-create "graph" workspace root))
          (niwa-session-set-root-buffer session root-buffer)
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (let ((child (niwa-session-get-skill session "child"))
                (first (niwa-session-get-skill session "first"))
                (second (niwa-session-get-skill session "second")))
            (setf (niwa-skill-allowed-tool-rules child) '(child-rule)
                  (niwa-skill-allowed-tool-rules first) '(first-rule)
                  (niwa-skill-allowed-tool-rules second) '(second-rule))
            (with-temp-buffer
              (setq-local niwa--session session)
              (setq-local niwa--agent-invocation
                          (niwa-agent-invocation--create
                           :path "/root/reviewer"
                           :skill-permission-rules '(ambient-rule)))
              (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
                         (lambda (buffer _session)
                           (setq refresh-buffer buffer)))
                        ((symbol-function
                          'niwa-skills-preparation-expand-body)
                         (lambda (body callback skill &rest _)
                           (setq expanded
                                 (append
                                  expanded
                                  (list
                                   (list (niwa-skill-name skill) body
                                         (niwa-request-skill-permission-rules
                                          niwa--current-request)
                                         niwa--agent-invocation))))
                           (funcall callback (list :status 'ok :body body))))
                        ((symbol-function 'niwa-skills--run-expansion-hook)
                         (lambda (skill _arguments prompt _origin _session callback)
                           (funcall callback prompt
                                    (list :additional-context
                                          (list
                                           (concat (niwa-skill-name skill)
                                                   "-context")))))))
                (niwa-skills-prepare-many
                 (list (list :skill first :arguments "VALUE"
                             :role 'command :policy-owner-p nil)
                       (list :skill second :arguments ""
                             :role 'command :policy-owner-p nil))
                 (lambda (value) (setq outcome value))
                 :origin 'user))))
          (should (eq root-buffer refresh-buffer))
          (should (equal '("child" "first" "second")
                         (mapcar #'car expanded)))
          (should (equal "Child(VALUE)" (cadar expanded)))
          (should (equal '((child-rule) (first-rule) (second-rule))
                         (mapcar #'caddr expanded)))
          (should-not (cl-some #'cadddr expanded))
          (should (equal '("child" "first" "second")
                         (mapcar #'niwa-skill-invocation-record-name
                                 (plist-get outcome :invoked-skills))))
          (should (equal '("first" "second")
                         (mapcar
                          (lambda (prepared)
                            (niwa-skill-name (plist-get prepared :skill)))
                          (plist-get outcome :outcomes))))
          (let* ((first-outcome (car (plist-get outcome :outcomes)))
                 (context (plist-get first-outcome :request-context))
                 (input (niwa-skills-format-model-input first-outcome))
                 (hook-context (plist-get first-outcome :hook-context)))
            (should (equal '(first-rule)
                           (plist-get context :permission-rules)))
            (should (equal '("child" "first")
                           (mapcar #'niwa-skill-invocation-record-name
                                   (plist-get context :invoked-skills))))
            (should (< (string-search "child-context" hook-context)
                       (string-search "first-context" hook-context)))
            (should (string-match-p "Child(VALUE)" input))
            (should (string-match-p "First VALUE" input))
            (should (string-match-p
                     (regexp-quote "[skill:child -- attached]") input)))
          (let* ((hook-context (plist-get outcome :hook-context))
                 (child-position (string-search "child-context" hook-context)))
            (should (< child-position
                       (string-search "first-context" hook-context)))
            (should (< (string-search "first-context" hook-context)
                       (string-search "second-context" hook-context)))
            (should-not (string-search "child-context" hook-context
                                       (1+ child-position)))))
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (delete-directory root t)))

  :doc "allows user-hidden children but rejects conflicts and model-disabled children before effects"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-graph-reject-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         session
         expanded
         conflict
         model-rejection
         runtime-failure
         user-outcome)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child"
           (concat "name: child\ndescription: Child\n"
                   "user-invocable: false\n"
                   "disable-model-invocation: true\n")
           "Child")
          (niwa-skills-test--write-skill
           dir "one" "name: one\ndescription: One\n" "!$child -- ONE")
          (niwa-skills-test--write-skill
           dir "two" "name: two\ndescription: Two\n" "!$child -- TWO")
          (setq session (niwa-session-create "graph" workspace root))
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (let ((one (niwa-session-get-skill session "one"))
                (two (niwa-session-get-skill session "two")))
            (should-not
             (niwa-skill-user-invocable-p
              (niwa-session-get-skill session "child")))
            (with-temp-buffer
              (setq-local niwa--session session)
              (cl-letf (((symbol-function
                          'niwa-skills-preparation-expand-body)
                         (lambda (&rest _) (setq expanded t))))
                (niwa-skills-prepare-many
                 (list (list :skill one :arguments "" :role 'command)
                       (list :skill two :arguments "" :role 'command))
                 (lambda (value) (setq conflict value))
                 :origin 'user)
                (should (eq 'dependency-conflict
                            (plist-get conflict :reason)))
                (should-not expanded)
                (niwa-skills-prepare
                 one "" (lambda (value) (setq model-rejection value))
                 :role 'command :origin 'model)
                (should (eq 'disabled
                            (plist-get model-rejection :reason)))
                (should-not expanded))
              (niwa-skills-prepare
               one "" (lambda (value) (setq user-outcome value))
               :role 'command :origin 'user)
              (should (eq 'ok (plist-get user-outcome :status)))
              (cl-letf (((symbol-function
                          'niwa-skills-preparation-expand-body)
                         (lambda (body callback skill &rest _)
                           (funcall callback
                                    (if (equal (niwa-skill-name skill) "child")
                                        '(:status error :reason injection-failed
                                          :message "child failed")
                                      (list :status 'ok :body body))))))
                (niwa-skills-prepare-many
                 (list (list :skill one :arguments ""
                             :role 'command :policy-owner-p nil))
                 (lambda (value) (setq runtime-failure value))
                 :origin 'user))
              (should (eq 'injection-failed
                          (plist-get runtime-failure :reason)))
              (should (equal "child" (plist-get runtime-failure :name))))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-recursive-delivery ()
  ,test
  (test)
  :doc "model inline and direct fork consumers receive complete required input"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-delivery-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "delivery" workspace root))
         model-result
         fork-prompt
         fork-result)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n" "Child contract")
          (niwa-skills-test--write-skill
           dir "inline" "name: inline\ndescription: Inline\n"
           "Inline root\n!$child")
          (niwa-skills-test--write-skill
           dir "fork"
           "name: fork\ndescription: Fork\ncontext: fork\n"
           "Fork root\n!$child")
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (with-temp-buffer
            (setq-local niwa--session session)
            (niwa-skills--invoke-handler
             (lambda (value) (setq model-result (plist-get value :result)))
             '(:name "inline"))
            (cl-letf (((symbol-function
                        'niwa-skills-dispatch-prepared-fork)
                       (lambda (_prepared callback &rest keys)
                         (setq fork-prompt (plist-get keys :prompt))
                         (funcall callback '(:status ok :kind fork)))))
              (niwa-skills--invoke-fork-direct
               (niwa-session-get-skill session "fork") ""
               (lambda (value) (setq fork-result value))
               :origin 'user)))
          (dolist (text (list model-result fork-prompt))
            (should (string-match-p "Child contract" text))
            (let* ((child (niwa-session-get-skill session "child"))
                   (skill (niwa-session-get-skill
                           session (if (eq text model-result) "inline" "fork")))
                   (child-address (niwa-resource--skill-address child))
                   (root-address (niwa-resource--skill-address skill)))
              (should (< (string-search child-address text)
                         (string-search root-address text))))
            (should (string-match-p
                     (if (eq text model-result) "Inline root" "Fork root")
                     text)))
          (should (eq 'ok (plist-get fork-result :status))))
      (delete-directory root t)))

  :doc "dependency markers generated during expansion remain inert"
  (let* ((skill (niwa-skill--create
                 :name "generated" :body "!el`ignored`"))
         outcome)
    (cl-letf (((symbol-function 'niwa-skills-preparation-expand-body)
               (lambda (_body callback &rest _)
                 (funcall callback '(:status ok :body "!$not-a-dependency")))))
      (niwa-skills-prepare
       skill "" (lambda (value) (setq outcome value))
       :role 'command :origin 'user))
    (should (eq 'ok (plist-get outcome :status)))
    (should-not (plist-get outcome :required-attachments))
    (should (equal "!$not-a-dependency" (plist-get outcome :body))))

  :doc "model-required children remain attached to the active request"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-idempotent-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "idempotent" workspace root))
         (request (niwa-request--create :session session))
         parent-outcome
         child-outcome)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n" "Child")
          (niwa-skills-test--write-skill
           dir "parent" "name: parent\ndescription: Parent\n"
           "!$child\nParent")
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (with-temp-buffer
            (setq-local niwa--session session)
            (setq-local niwa--current-request request)
            (niwa-skills-invoke
             (niwa-session-get-skill session "parent") ""
             (lambda (value) (setq parent-outcome value))
             :origin 'model)
            (niwa-skills-invoke
             (niwa-session-get-skill session "child") ""
             (lambda (value) (setq child-outcome value))
             :origin 'model))
          (should (eq 'ok (plist-get parent-outcome :status)))
          (should (eq 'already-attached (plist-get child-outcome :kind)))
          (should
           (equal '("child" "parent")
                  (mapcar #'niwa-skill-invocation-record-name
                          (niwa-request-attached-skill-records request)))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-invoke (:quiet t)
  ,test
  (test)
  :doc "inline skill yields :status ok :kind inline with prepared body"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/i"))
         (skill (niwa-skill--create
                 :name "shout"
                 :body "YELL $ARGUMENTS"))
         outcome)
    (with-temp-buffer
      (setq niwa--session session)
      (niwa-skills-invoke
       skill "loudly"
       (lambda (o) (setq outcome o))
       :origin 'model))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "YELL loudly" (plist-get outcome :body))))

  :doc "user origin installs the pending stash"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/s"))
         (skill (niwa-skill--create
                 :name "demo"
                 :body "Hello"
                 :model "fast"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq niwa--session session)
      (setq-local niwa-skills--pending-request-context nil)
      (niwa-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user)
      (let ((stash niwa-skills--pending-request-context))
        (should (equal (niwa-model-tier-selector 'fast)
                       (plist-get stash :model)))
        (should (equal '(("Read" :action allow))
                       (plist-get stash :permission-rules)))
        (should (= 1 (length (plist-get stash :invoked-skills))))))
    (should (eq 'ok (plist-get outcome :status))))

  :doc "UserPromptExpansion can rewrite user-origin inline skill output"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/slash-expansion"))
         (skill (niwa-skill--create
                 :name "demo"
                 :body "Original body"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         (niwa-hook-rules
          '((UserPromptExpansion
             ((:hooks ((:type elisp
                              :function niwa-skills-test--expansion-fn)))))))
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (niwa-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Expanded by hook" (plist-get outcome :body)))
    (should (string-match-p "expansion context"
                            (plist-get outcome :hook-context)))
    (let ((audit (car (plist-get outcome :hook-audits))))
      (should (eq (plist-get audit :type) 'prompt-rewrite))
      (should (equal (plist-get audit :event) "UserPromptExpansion"))
      (should (equal (plist-get audit :original) "Original body"))
      (should (equal (plist-get audit :submitted) "Expanded by hook"))))

  :doc "malformed UserPromptExpansion decision does not abort user skill"
  (let* ((ws (niwa-workspace--create
              :type 'file :id "slash-expansion-malformed"
              :root "/tmp/slash-expansion-malformed"
              :name "slash-expansion-malformed"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "demo"
                 :body "Original body"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback 'passed))))
        (niwa-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :origin 'user)
        (should niwa-skills--pending-request-context)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Original body" (plist-get outcome :body))))

  :doc "UserPromptExpansion can block user-origin inline skill output"
  (let* ((ws (niwa-workspace--create
              :type 'file :id "slash-expansion-block"
              :root "/tmp/slash-expansion-block"
              :name "slash-expansion-block"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (skill (niwa-skill--create
                 :name "demo"
                 :body "Original body"))
         (niwa-hook-rules
          '((UserPromptExpansion
             ((:hooks ((:type elisp
                              :function
                              niwa-skills-test--block-expansion-fn)))))))
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa-skills--pending-request-context nil)
      (niwa-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user)
      (should-not niwa-skills--pending-request-context))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'hook-blocked (plist-get outcome :reason)))
    (should (equal "blocked expansion" (plist-get outcome :message))))

  :doc "user-origin preparation failure leaves the pending stash empty"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/slash-fail"))
         (skill (niwa-skill--create
                 :name "demo"
                 :body "Hello"
                 :allowed-tool-rules '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq niwa--session session)
      (setq-local niwa-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'niwa-skills-preparation-expand-body)
                 (lambda (_text callback &rest _)
                   (funcall callback
                            '(:status error
                                      :reason injection-failed
                                      :message "boom")))))
        (niwa-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :origin 'user)
        (should (null niwa-skills--pending-request-context))
        (should-not (bound-and-true-p niwa--current-request))))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'injection-failed (plist-get outcome :reason))))

  :doc "model inline origin installs additive context but ignores policy"
  (let* ((session (niwa-skills-test--make-session))
         (request (niwa-request--create :session session))
         (skill (niwa-skill--create
                 :name "demo"
                 :body "Hi"
                 :model "fast"
                 :allowed-tool-rules
                 '(("Bash" :pattern "ls" :action allow))))
         outcome)
    (with-temp-buffer
      (setq niwa--session session)
      (setq-local niwa--current-request request)
      (niwa-skills-invoke
       skill nil
       (lambda (value) (setq outcome value))
       :origin 'model))
    (should (equal '(model)
                   (plist-get outcome :ignored-policy-fields)))
    (should (equal '(("Bash" :pattern "ls" :action allow))
                   (niwa-request-skill-permission-rules request))))

  :doc "disabled user skill tells the user how to enable or escape it"
  (let ((skill (niwa-skill--create
                :name "hidden"
                :body "X"))
        outcome)
    (cl-letf (((symbol-function 'niwa-skills-skill-enabled-p)
               (lambda (_) nil)))
      (niwa-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason)))
    (should (string-match-p "/skills enable hidden"
                            (plist-get outcome :message)))
    (should (string-search "\\$hidden" (plist-get outcome :message))))

  :doc "user-invocable: false rejects user origin"
  (let ((skill (niwa-skill--create
                :name "internal-only"
                :body "X"
                :user-invocable-p nil))
        outcome)
    (niwa-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'user)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason))))

  :doc "disable-model-invocation rejects model origin"
  (let ((skill (niwa-skill--create
                :name "human-only"
                :body "X"
                :model-invocable-p nil))
        outcome)
    (niwa-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'model)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason))))

  :doc "skip-gates lets internal commands invoke disabled skills"
  (let ((skill (niwa-skill--create
                :name "human-only"
                :body "X"
                :model-invocable-p nil))
        outcome)
    (cl-letf (((symbol-function 'niwa-skills-skill-enabled-p)
               (lambda (_) nil)))
      (niwa-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'model
       :skip-gates t))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "X" (plist-get outcome :body))))

  :doc "missing body returns load-failure error"
  (let ((skill (niwa-skill--create :name "no-body"))
        outcome)
    (niwa-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'model)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'load-failure (plist-get outcome :reason))))

  :doc "display-callback receives done event on success"
  (let ((skill (niwa-skill--create :name "ok" :body "Hi"))
        events)
    (niwa-skills-invoke
     skill nil
     (lambda (_) nil)
     :origin 'internal
     :display-callback (lambda (e) (push e events)))
    (should (cl-some (lambda (e) (eq (plist-get e :event) 'done))
                     events)))

  :doc "display-callback receives error event on failure"
  (let ((skill (niwa-skill--create :name "no-body"))
        events)
    (niwa-skills-invoke
     skill nil
     (lambda (_) nil)
     :origin 'internal
     :display-callback (lambda (e) (push e events)))
    (should (cl-some (lambda (e) (eq (plist-get e :event) 'error))
                     events))))


;;
;;; Phase 6: build-fork-agent + fork dispatch routing

(niwa-deftest niwa-skills--build-fork-agent ()
  ,test
  (test)
  :doc "named-agent path looks up via the registry"
  (let ((agent (niwa-agent--create :name "explorer" :tools nil
                                      :system-prompt "")))
    (cl-letf (((symbol-function 'niwa-agent-get)
               (lambda (n) (and (equal n "explorer") agent))))
      (let ((skill (niwa-skill--create
                    :name "demo" :context 'fork :agent "explorer")))
        (should (eq agent (niwa-skills--build-fork-agent skill))))))

  :doc "named-agent path returns nil for unknown agent"
  (cl-letf (((symbol-function 'niwa-agent-get) (lambda (_) nil)))
    (let ((skill (niwa-skill--create
                  :name "demo" :context 'fork :agent "missing")))
      (should (null (niwa-skills--build-fork-agent skill)))))

  :doc "parent-inherited path synthesizes a `skill:<name>' agent"
  ;; The synthetic agent's name is `skill:<skill-name>' and its
  ;; system prompt is captured from the calling buffer's
  ;; `gptel-system-prompt' at spawn time.
  (let ((skill (niwa-skill--create
                :name "demo" :context 'fork
                :description "A test skill")))
    (with-temp-buffer
      (setq-local gptel-system-prompt "captured-system-prompt")
      (setq-local niwa-agents--specs nil)
      (let ((agent (niwa-skills--build-fork-agent skill)))
        (should (niwa-agent-p agent))
        (should (equal "skill:demo" (niwa-agent-name agent)))
        (should (equal "captured-system-prompt"
                       (niwa-agent-system-prompt agent)))
        ;; The dispatch passes this agent directly, so a name only the
        ;; fork knows never enters the preset-owned role roster.
        (should-not niwa-agents--specs)))))

(niwa-deftest niwa-skills-dispatch-prepared-fork ()
  ,test
  (test)
  :doc "spawns one retained fork and maps its canonical RESULT"
  (let* ((session (niwa-skills-test--make-session))
         (agent (niwa-agent--create :name "explorer"))
         (skill (niwa-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "unused"))
         (record (niwa-skill-invocation-record--create
                  :name "demo" :args "task" :role 'command :origin 'user))
         (context (list :permission-rules '(("Read" :action allow))
                        :model '(:tier fast)
                        :effort 'high
                        :hook-rules '((PreToolUse nil))
                        :invoked-skills (list record)))
         (prepared (list :status 'ok :kind 'fork :skill skill
                         :body "prepared body"
                         :hook-audits '((:event "expansion"))
                         :request-context context))
         dispatched
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-agent-get)
                 (lambda (_) agent))
                ((symbol-function 'niwa-agent-control-spawn)
                 (lambda (actual-session task-name message callback
                                         &rest keys)
                   (let* ((path (concat "/root/" task-name))
                          (retained
                           (niwa-agent-record--create
                            :id "storage-id"
                            :path path
                            :parent-path "/root"
                            :role "explorer"
                            :activity 'idle
                            :conversation-location "agents/demo.chat.org")))
                     (setf (niwa-session-agent-registry actual-session)
                           (list (cons path retained)))
                     (setq dispatched
                           (list :session actual-session
                                 :task-name task-name
                                 :message message
                                 :keys keys))
                     (funcall callback
                              (list :outcome 'success :record retained))
                     (funcall
                      (plist-get keys :result-handler)
                      (list :type 'RESULT :sender path :recipient "/root"
                            :outcome 'completed :payload "done"))
                     #'ignore))))
        (niwa-skills-dispatch-prepared-fork
         prepared (lambda (value) (setq outcome value)))))
    (should (eq session (plist-get dispatched :session)))
    (should (equal "skill_demo" (plist-get dispatched :task-name)))
    (should (equal "prepared body" (plist-get dispatched :message)))
    (let ((keys (plist-get dispatched :keys)))
      (should (eq agent (plist-get keys :agent)))
      (should (equal "none" (plist-get keys :context)))
      (should (equal '(("Read" :action allow))
                     (plist-get keys :skill-permission-rules)))
      (should (equal '(:tier fast) (plist-get keys :model)))
      (should (eq 'high (plist-get keys :effort)))
      (should (equal '((PreToolUse nil))
                     (plist-get keys :skill-hook-rules))))
    (should (equal (list record)
                   (niwa-session-invoked-skills session)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "done" (plist-get outcome :result)))
    (should (equal "/root/skill_demo" (plist-get outcome :agent-path)))
    (should (equal "/root/skill_demo"
                   (plist-get (plist-get outcome :render-data) :path)))
    (should (equal '((:event "expansion"))
                   (plist-get outcome :hook-audits))))

  :doc "maps a non-completed RESULT to a normalized skill error"
  (let* ((session (niwa-skills-test--make-session))
         (agent (niwa-agent--create :name "explorer"))
         (skill (niwa-skill--create
                 :name "demo" :context 'fork :agent "explorer" :body "Body"))
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-agent-get) (lambda (_) agent))
                ((symbol-function 'niwa-agent-control-spawn)
                 (lambda (_session _task _message callback &rest keys)
                   (let ((record
                          (niwa-agent-record--create
                           :path "/root/skill_demo" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_demo"
                            :recipient "/root" :outcome interrupted
                            :payload "Stopped"))
                   #'ignore)))
        (niwa-skills-invoke
         skill nil (lambda (value) (setq outcome value)) :origin 'model)))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'agent-interrupted (plist-get outcome :reason)))
    (should (equal "Stopped" (plist-get outcome :message))))

  :doc "rejects an invalid prepared outcome without dispatching"
  (let (outcome dispatched)
    (cl-letf (((symbol-function 'niwa-agent-control-spawn)
               (lambda (&rest _) (setq dispatched t))))
      (niwa-skills-dispatch-prepared-fork
       '(:status error :reason failed)
       (lambda (value) (setq outcome value))))
    (should-not dispatched)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'invalid-prepared-fork (plist-get outcome :reason)))))

(niwa-deftest niwa-skills-invoke-fork ()
  ,test
  (test)
  :doc "forwards prompt context, description, and invocation callback"
  (let* ((session (niwa-skills-test--make-session))
         (agent (niwa-agent--create :name "explorer"))
         (skill (niwa-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body $ARGUMENTS"))
         (progress-callback #'ignore)
         captured
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-agent-get) (lambda (_) agent))
                ((symbol-function 'niwa-agent-control-spawn)
                 (lambda (_session _task message callback &rest keys)
                   (setq captured (cons message keys))
                   (let ((record
                          (niwa-agent-record--create
                           :path "/root/skill_demo" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_demo"
                            :recipient "/root" :outcome completed
                            :payload "agent finished"))
                   #'ignore)))
        (niwa-skills-invoke
         skill "the task" (lambda (value) (setq outcome value))
         :origin 'user
         :additional-context "<hook-context>ctx</hook-context>"
         :description "target hint"
         :on-invocation progress-callback)))
    (should (string-match-p "the task" (car captured)))
    (should-not (string-search "skill://" (car captured)))
    (should (string-match-p "<hook-context>ctx</hook-context>" (car captured)))
    (should (equal "target hint" (plist-get (cdr captured) :description)))
    (should (eq progress-callback
                (plist-get (cdr captured) :on-invocation)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "agent finished" (plist-get outcome :result))))

  :doc "unknown named roles fail before spawning"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "demo" :context 'fork :agent "missing"))
         outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-agent-get) (lambda (_) nil)))
        (niwa-skills-invoke
         skill nil (lambda (value) (setq outcome value)) :origin 'model)))
    (should (eq 'unknown-agent (plist-get outcome :reason))))

  :doc "omitted roles spawn a parent-inherited synthetic agent"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "demo" :context 'fork :body "Body"))
         dispatched-agent)
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa-agents--specs nil)
      (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                 (lambda (_session _task _message callback &rest keys)
                   (setq dispatched-agent (plist-get keys :agent))
                   (let ((record
                          (niwa-agent-record--create
                           :path "/root/skill_demo" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_demo"
                            :recipient "/root" :outcome completed :payload "result"))
                   #'ignore)))
        (let (outcome)
          (niwa-skills-invoke
           skill nil (lambda (value) (setq outcome value)) :origin 'model)
          (should (eq 'ok (plist-get outcome :status))))))
    (should (niwa-agent-p dispatched-agent))
    (should (equal "skill:demo" (niwa-agent-name dispatched-agent)))))


(defun test-niwa-skills--handler-result (envelope)
  "Return the required result from handler ENVELOPE."
  (should (plist-member envelope :result))
  (plist-get envelope :result))

(niwa-deftest niwa-skills--render-skill-tool ()
  ,test
  (test)
  :doc "ignored model-side inline policy renders a warning without changing result"
  (let ((rendering
         (niwa-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body"
          '(:kind skill-invocation
                  :ignored-policy-fields (model effort)))))
    (should (eq 'warning (plist-get rendering :status)))
    (should (string-match-p "ignored model, effort"
                            (plist-get rendering :header)))
    (should (string-match-p "context: fork" (plist-get rendering :body)))
    (should (string-match-p "Prepared body" (plist-get rendering :body))))

  :doc "warning names only the policy field actually ignored"
  (let ((rendering
         (niwa-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body"
          '(:kind skill-invocation
                  :ignored-policy-fields (effort)))))
    (should (string-match-p "ignored effort" (plist-get rendering :header)))
    (should-not (string-match-p "ignored model" (plist-get rendering :header))))

  :doc "ordinary skill rendering remains successful and unchanged"
  (let ((rendering
         (niwa-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body" nil)))
    (should-not (plist-get rendering :status))
    (should (equal "Prepared body" (plist-get rendering :body)))
    (should-not (string-match-p "ignored" (plist-get rendering :header))))

  :doc "attachment reminders leave the body and name themselves in the header"
  (let ((rendering
         (niwa-skills--render-skill-tool
          "Skill" '(:name "root")
          (concat "<system-reminder>\nAttached child\n</system-reminder>"
                  "\n\nRoot body")
          '(:kind skill-invocation :prompt "Root body"
                  :attachments ("child" "grandchild")))))
    (should (equal "Root body" (plist-get rendering :body)))
    (should (string-match-p "(1 line; attached child, grandchild)"
                            (plist-get rendering :header)))))

(niwa-deftest niwa-skills--invoke-handler ()
  ,test
  (test)
  :doc "unknown skill returns an error"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/u"))
         received)
    (with-temp-buffer
      (setq niwa--session session)
      (niwa-skills--invoke-handler
       (lambda (r)
         (setq received (test-niwa-skills--handler-result r)))
       (list :name "nope")))
    (should (string-match-p "Unknown skill" received)))

  :doc "known inline skill is dispatched and body returned"
  (let* ((dir (make-temp-file "niwa-skills-test-" t))
         (ws (niwa-workspace--create
              :type 'file :id dir :root dir :name "h"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         received)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "shout"
           "name: shout
description: Yell
"
           "YELL $ARGUMENTS")
          (setf (niwa-session-skills session)
                (niwa-skills-scan dir '(".")))
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--invoke-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             (list :name "shout" :arguments "loudly")))
          (let* ((skill (niwa-session-get-skill session "shout"))
                 (address (niwa-resource--skill-address skill)))
            (should (string-search address received))
            (should (string-suffix-p "YELL loudly" received))))
      (delete-directory dir t)))

  :doc "user-attached skill is an idempotent model-side success"
  (let* ((session (niwa-skills-test--make-session))
         (source "/tmp/implement/SKILL.md")
         (skill (niwa-skill--create
                 :name "implement"
                 :body "BODY MUST NOT BE RETURNED"
                 :source-file source
                 :context 'inline
                 :model-invocable-p nil))
         (record (niwa-skill-invocation-record--create
                  :name "implement"
                  :role 'instruction
                  :origin 'user
                  :source-path source
                  :prepared-body "BODY MUST NOT BE RETURNED"))
         (request (niwa-request--create :session session))
         received)
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local niwa--session session
                  niwa--current-request request
                  niwa-skills--pending-request-context
                  (list :invoked-skills (list record)))
      (niwa-skills--drain-pending-context request)
      (niwa-skills--invoke-handler
       (lambda (value)
         (setq received (test-niwa-skills--handler-result value)))
       '(:name "implement")))
    (should (string-match-p "already attached" received))
    (should-not (string-match-p "BODY MUST NOT BE RETURNED" received))
    (should (equal (list record) (niwa-session-invoked-skills session))))

  :doc "user-only guidance survives a new request without invoking preparation again"
  (let* ((session (niwa-skills-test--make-session))
         (source "/tmp/resolved/SKILL.md")
         (skill (niwa-skill--create :name "resolved" :source-file source
                                      :body "UNPREPARED" :context 'inline
                                      :model-invocable-p nil))
         (record (niwa-skill-invocation-record--create
                  :name "resolved" :origin 'user :role 'command :args "accepted task"
                  :source-path source :agent-path "/root" :turn 1
                  :prepared-body "Resolved instructions for accepted task"))
         received)
    (setf (niwa-session-skills session) (list skill)
          (niwa-session-invoked-skills session) (list record))
    (with-temp-buffer
      (setq-local niwa--session session
                  niwa--current-request (niwa-request--create :session session))
      (cl-letf (((symbol-function 'niwa-skills--invoke-inline)
                 (lambda (&rest _) (ert-fail "Must not prepare or invoke again"))))
        (niwa-skills--invoke-handler
         (lambda (value) (setq received (test-niwa-skills--handler-result value)))
         '(:name "resolved")))
      (should (string-search "Previously prepared user skill" received))
      (should (string-search "Resolved instructions for accepted task" received))
      (should-not (string-search "UNPREPARED" received))
      (should-not (niwa-request-attached-skill-records niwa--current-request))
      (should (equal (list record) (niwa-session-invoked-skills session)))
      ;; Neither a different conversation nor new arguments can reuse it.
      (setf (niwa-skill-invocation-record-agent-path record) "/root/other")
      (niwa-skills--invoke-handler
       (lambda (value) (setq received (test-niwa-skills--handler-result value)))
       '(:name "resolved"))
      (should (string-search "not model-invocable" received))
      (setf (niwa-skill-invocation-record-agent-path record) "/root")
      (niwa-skills--invoke-handler
       (lambda (value) (setq received (test-niwa-skills--handler-result value)))
       '(:name "resolved" :arguments "different task"))
      (should (string-search "not model-invocable" received))))

  :doc "cold model-side reuse loads its caller-identity dependency on demand"
  (let ((program
         `(progn
            (setq load-path ',load-path)
            (require 'niwa-skills-invoke)
            (when (featurep 'niwa-agent-control)
              (error "Cold probe preloaded agent control"))
            (let* ((skill (niwa-skill--create
                           :name "resolved" :source-file "/tmp/resolved/SKILL.md"
                           :model-invocable-p nil))
                   (record (niwa-skill-invocation-record--create
                            :name "resolved" :origin 'user :agent-path "/root"
                            :source-path "/tmp/resolved/SKILL.md" :prepared-body "RESOLVED"))
                   (session (niwa-session--create :name "cold" :skills (list skill)
                                                     :invoked-skills (list record)))
                   received)
              (with-temp-buffer
                (setq-local niwa--session session)
                (niwa-skills--invoke-handler
                 (lambda (value) (setq received (plist-get value :result)))
                 '(:name "resolved")))
              (unless (and (stringp received) (string-search "RESOLVED" received))
                (error "Cold Skill reuse failed: %S" received))
              (princ "reused")))))
    (with-temp-buffer
      (should (zerop (call-process
                      (expand-file-name invocation-name invocation-directory)
                      nil t nil "-Q" "--batch" "--eval" (prin1-to-string program))))
      (should (equal "reused" (buffer-string)))))

  :doc "required attachments lead the result and travel beside it"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-attach-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "attach" workspace root))
         envelope)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n" "Child contract")
          (niwa-skills-test--write-skill
           dir "root" "name: root\ndescription: Root\n" "Root body\n!$child")
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (with-temp-buffer
            (setq-local niwa--session session)
            (niwa-skills--invoke-handler
             (lambda (value) (setq envelope value))
             '(:name "root")))
          (let ((result (plist-get envelope :result))
                (render-data (plist-get envelope :render-data)))
            (should (string-match-p "<system-reminder>" result))
            (should (string-match-p "Child contract" result))
            (should (equal '("child") (plist-get render-data :attachments)))
            (niwa-skills-test--write-skill
             dir "child" "name: child\ndescription: Child\n" "Changed after delivery")
            (let* ((encoded (concat result (niwa-tool-render-data-format render-data)))
                   (restored (niwa-tool-render-data-extract encoded)))
              (should (equal result (car restored)))
              (should (string-match-p
                       "Child contract"
                       (cdr (assoc "child" (plist-get (cdr restored) :attachment-bodies)))))
              (should-not (string-match-p
                           "Changed after delivery"
                           (cdr (assoc "child" (plist-get (cdr restored) :attachment-bodies))))))
            (should (string-match-p "Root body"
                                    (plist-get render-data :prompt)))
            (should-not (string-match-p "<system-reminder>"
                                        (plist-get render-data :prompt)))
            (should-not (string-match-p "Child contract"
                                        (plist-get render-data :prompt)))))
      (delete-directory root t)))

  :doc "model-side inline policy is view-only render-data"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "costly" :body "Prepared body"
                 :context 'inline :model "OpenAI:gpt-5-mini" :effort 'low))
         envelope)
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq niwa--session session)
      (niwa-skills--invoke-handler
       (lambda (value) (setq envelope value))
       '(:name "costly")))
    (should (equal "Prepared body" (plist-get envelope :result)))
    (should (equal '(:kind skill-invocation
                           :prompt "Prepared body" :attachments nil
                           :ignored-policy-fields (model effort))
                   (plist-get envelope :render-data))))

  :doc "registered Skill pipeline separates model result from warning view"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "costly" :body "Prepared body"
                 :context 'inline :model "invalid" :effort 'low))
         (tool (niwa-tool-ensure "Skill"))
         pipeline-result)
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local niwa--session session
                  niwa--current-request
                  (niwa-request--create
                   :session session
                   :file-snapshots (make-hash-table :test #'equal)))
      (niwa-pipeline-run-tool
       tool (lambda (value) (setq pipeline-result value))
       '(:name "costly")))
    (let* ((parts (niwa-tool-render-data-extract
                   pipeline-result session))
           (visible (car parts))
           (render-data (cdr parts))
           (rendering
            (niwa-view--invoke-renderer
             tool render-data '(:name "costly") visible)))
      (should (equal "Prepared body" visible))
      (should (equal '(:kind skill-invocation
                             :prompt "Prepared body" :attachments nil
                             :ignored-policy-fields (model effort))
                     render-data))
      (should (eq 'warning (plist-get rendering :status)))
      (should (string-match-p "context: fork"
                              (plist-get rendering :body)))))

  :doc "model-side inline skill without policy has no render-data"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "plain" :body "Prepared body" :context 'inline))
         envelope)
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq niwa--session session)
      (niwa-skills--invoke-handler
       (lambda (value) (setq envelope value))
       '(:name "plain")))
    (should (equal "Prepared body" (plist-get envelope :result)))
    (should-not (plist-member envelope :render-data)))

  :doc "model-side fork policy owns its child and produces no warning"
  (let* ((niwa-model-tiers '((fast)))
         (session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "isolated" :body "Prepared body"
                 :context 'fork :model "fast"))
         envelope)
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq niwa--session session)
      (cl-letf (((symbol-function 'niwa-agent-control-spawn)
                 (lambda (_session _task _message callback &rest keys)
                   (let ((record
                          (niwa-agent-record--create
                           :path "/root/skill_isolated" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_isolated"
                            :recipient "/root" :outcome completed
                            :payload "Child result"))
                   #'ignore)))
        (niwa-skills--invoke-handler
         (lambda (value) (setq envelope value))
         '(:name "isolated"))))
    (should (equal "Child result" (plist-get envelope :result)))
    (should-not (plist-member envelope :render-data)))

  :doc "model-side invocation uses visible prefixed names after conflicts"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (ws (niwa-workspace--create
              :type 'file :id "collision" :root "/tmp/collision"
              :name "collision"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (local (niwa-skill--create
                 :name "shared"
                 :source 'project
                 :source-family 'niwa
                 :body "LOCAL $ARGUMENTS"))
         (global (niwa-skill--create
                  :name "shared"
                  :source 'user
                  :source-family 'niwa
                  :body "GLOBAL $ARGUMENTS"))
         received)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session)
                (niwa-skills--qualify-conflicting-names
                 (list local global)))
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--invoke-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             (list :name "local:shared" :arguments "now"))
            (should (equal "LOCAL now" received))
            (setq received nil)
            (niwa-skills--invoke-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             (list :name "shared"))
            (should (string-match-p "Unknown skill 'shared'" received))))
      (delete-directory user-dir t)))

  :doc "disabled skill is rejected before model invocation"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (skill (niwa-skills-test--stateful-skill
                 :name "hidden"
                 :body "should not run"
                 :workspace (niwa-session-workspace session)))
         received)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list skill))
          (niwa-skills-set-enabled skill nil)
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--invoke-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             (list :name "hidden")))
          (should (string-match-p "disabled" received)))
      (delete-directory user-dir t))))

(niwa-deftest niwa-skills--list-handler ()
  ,test
  (test)
  :doc "returns active model-invocable enabled skills"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (alpha (niwa-skill--create
                 :name "alpha" :description "Alpha helper"
                 :active-p t :model-invocable-p t))
         (beta (niwa-skills-test--stateful-skill
                :name "beta" :description "Beta helper"
                :active-p t :model-invocable-p t
                :workspace (niwa-session-workspace session)))
         (model-disabled (niwa-skill--create
                          :name "internal" :description "Internal"
                          :active-p t :model-invocable-p nil))
         (dormant (niwa-skill--create
                   :name "dormant" :description "Dormant"
                   :active-p nil :model-invocable-p t
                   :path-patterns '("*.el")))
         received)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session)
                (list alpha beta model-disabled dormant))
          (niwa-skills-set-enabled beta nil)
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--list-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             (list :query "alp")))
          (should (string-match-p "alpha: Alpha helper" received))
          (should-not (string-match-p "beta" received))
          (should-not (string-match-p "internal" received))
          (should-not (string-match-p "dormant" received))
          (setq received nil)
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--list-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             (list :query "Dormant")))
          (should (string-match-p "dormant \\[dormant path-scoped\\]: Dormant"
                                  received))
          (should-not (string-match-p "internal" received)))
      (delete-directory user-dir t)))

  :doc "a query marks matching user-only skills instead of hiding them"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (user-only (niwa-skill--create
                     :name "retro" :description "Retrospective review"
                     :active-p t :model-invocable-p nil :user-invocable-p t))
         (disabled (niwa-skills-test--stateful-skill
                    :name "retro-old" :description "Old retrospective"
                    :active-p t :model-invocable-p nil :user-invocable-p t
                    :workspace (niwa-session-workspace session)))
         (hidden (niwa-skill--create
                  :name "retro-internal" :description "Internal retrospective"
                  :active-p t :model-invocable-p nil :user-invocable-p nil))
         received)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list user-only disabled hidden))
          (niwa-skills-set-enabled disabled nil)
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--list-handler
             (lambda (r) (setq received (test-niwa-skills--handler-result r)))
             (list :query "retro")))
          (should (string-match-p "No model-invocable skills match" received))
          (should (string-match-p "User-only.*\\$retro\\b" received))
          (should-not (string-match-p "retro-old" received))
          (should-not (string-match-p "retro-internal" received))
          ;; The unqueried listing stays the model-invocable roster.
          (setq received nil)
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--list-handler
             (lambda (r) (setq received (test-niwa-skills--handler-result r)))
             nil))
          (should-not (string-match-p "retro" received)))
      (delete-directory user-dir t)))

  :doc "refreshes session skills before listing"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session))
         (fresh (niwa-skill--create
                 :name "fresh" :description "Fresh helper"
                 :active-p t :model-invocable-p t))
         refreshed
         received)
    (unwind-protect
        (with-temp-buffer
          (setq niwa--session session)
          (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
                     (lambda (_buffer s)
                       (setq refreshed s)
                       (setf (niwa-session-skills s) (list fresh)))))
            (niwa-skills--list-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             nil))
          (should (eq refreshed session))
          (should (string-match-p "fresh: Fresh helper" received)))
      (delete-directory user-dir t)))

  :doc "returns prefixed visible names that can be used with Skill"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session))
         (local (niwa-skill--create
                 :name "shared"
                 :description "Local helper"
                 :source 'project
                 :source-family 'niwa
                 :active-p t
                 :model-invocable-p t))
         (global (niwa-skill--create
                  :name "shared"
                  :description "Global helper"
                  :source 'user
                  :source-family 'niwa
                  :active-p t
                  :model-invocable-p t))
         received)
    (unwind-protect
        (progn
          (setf (niwa-session-skills session)
                (niwa-skills--qualify-conflicting-names
                 (list local global)))
          (with-temp-buffer
            (setq niwa--session session)
            (niwa-skills--list-handler
             (lambda (r)
               (setq received (test-niwa-skills--handler-result r)))
             (list :query "shared")))
          (should (string-match-p "local:shared: Local helper" received))
          (should (string-match-p "global:shared: Global helper" received))
          (should-not (string-match-p "\nshared: " received)))
      (delete-directory user-dir t))))


;;
;;; ListSkills selection primitives

(niwa-deftest niwa-skills--listing-describe ()
  ,test
  (test)
  :doc "short entries are returned as-is"
  (let ((skill (niwa-skill--create
                :name "simplify"
                :description "Review changed code for reuse")))
    (should (equal "- simplify: Review changed code for reuse"
                   (niwa-skills--listing-describe skill))))

  :doc "entries longer than the cap are truncated with ellipsis"
  (let* ((niwa-skills-listing-max-entry-chars 20)
         (skill (niwa-skill--create
                 :name "n"
                 :description "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
         (entry (niwa-skills--listing-describe skill)))
    (should (= 20 (length entry)))
    (should (string-suffix-p "..." entry)))

  :doc "empty descriptions still produce a stable entry"
  (let ((skill (niwa-skill--create :name "demo")))
    (should (equal "- demo: "
                   (niwa-skills--listing-describe skill)))))

(niwa-deftest niwa-skills--listing-candidates ()
  ,test
  (test)
  :doc "returns only active, model-invocable skills"
  (let* ((session (niwa-skills-test--make-session nil "/tmp/l"))
         (active-invocable
          (niwa-skill--create :name "a" :description "A"
                                 :model-invocable-p t :active-p t))
         (dormant
          (niwa-skill--create :name "b" :description "B"
                                 :model-invocable-p t
                                 :path-patterns '("*.el")
                                 :active-p nil))
         (disabled
          (niwa-skill--create :name "c" :description "C"
                                 :model-invocable-p nil :active-p t)))
    (setf (niwa-session-skills session)
          (list active-invocable dormant disabled))
    (let ((names (mapcar #'niwa-skill-name
                         (niwa-skills--listing-candidates session))))
      (should (equal '("a") names))))

  :doc "omits user-disabled skills"
  (let* ((user-dir (make-temp-file "niwa-skills-state-" t))
         (niwa-user-dir (file-name-as-directory user-dir))
         (session (niwa-skills-test--make-session nil user-dir))
         (enabled (niwa-skill--create
                   :name "enabled" :description "E"
                   :model-invocable-p t :active-p t))
         (disabled (niwa-skills-test--stateful-skill
                    :name "disabled" :description "D"
                    :model-invocable-p t :active-p t
                    :workspace (niwa-session-workspace session))))
    (unwind-protect
        (progn
          (setf (niwa-session-skills session) (list enabled disabled))
          (niwa-skills-set-enabled disabled nil)
          (let ((names (mapcar #'niwa-skill-name
                               (niwa-skills--listing-candidates session))))
            (should (equal '("enabled") names))))
      (delete-directory user-dir t))))

(niwa-deftest niwa-skills-format-attachment
  (:doc "formats one prepared attachment as a mandatory instruction")
  (let ((text (niwa-skills-format-attachment
               '(:name "review" :body "Check it"))))
    (should (string-match-p "`\\$review`" text))
    (should-not (string-search "skill://" text))
    (should (string-suffix-p "Check it" text))))

(niwa-deftest niwa-skills-format-body ()
  ,test
  (test)
  :doc "discloses the exact source without altering the prepared body"
  (let ((root (make-temp-file "skill-source-body-" t)))
    (unwind-protect
        (let* ((source (niwa-skills-test--write-skill
                        root "review" "name: review\ndescription: Review\n"
                        "Authored body"))
               (skill (niwa-skill--create
                       :name "review" :source-file source))
               (body "Prepared $ARGUMENTS\n\n!$inert\n")
               (text (niwa-skills-format-body skill body)))
          (should (string-search (niwa-resource--skill-address skill) text))
          (should (string-suffix-p body text))
          (should (equal "Prepared $ARGUMENTS\n\n!$inert\n" body)))
      (delete-directory root t)))

  :doc "source-less skills retain the exact body and acquire no fake locator"
  (let* ((skill (niwa-skill--create :name "synthetic"))
         (body "Synthetic body\n"))
    (should (equal body (niwa-skills-format-body skill body)))
    (should (equal body (niwa-skills-format-body nil body)))))

(niwa-deftest niwa-skills-format-model-input ()
  ,test
  (test)
  :doc "puts formatted dependency attachments before the root body"
  (let ((text (niwa-skills-format-model-input
               '(:required-attachments ((:name "child" :body "Child"))
                 :body "Root"))))
    (should-not (string-search "skill://" text))
    (should (< (string-search "Child" text) (string-search "Root" text))))

  :doc "root and dependency disclose their own sources dependency first"
  (let ((dir (make-temp-file "skill-source-input-" t)))
    (unwind-protect
        (let* ((root-source (niwa-skills-test--write-skill
                             dir "root" "name: root\ndescription: Root\n" "Root"))
               (child-source (niwa-skills-test--write-skill
                              dir "child" "name: child\ndescription: Child\n" "Child"))
               (skill (niwa-skill--create :name "root" :source-file root-source))
               (child (niwa-skill--create :name "child" :source-file child-source))
               (prepared (list :skill skill :body "Root prepared"
                               :required-attachments
                               (list (list :name "child" :source-file child-source
                                           :body "Child prepared"))))
               (text (niwa-skills-format-model-input prepared)))
          (should (< (string-search (niwa-resource--skill-address child) text)
                     (string-search (niwa-resource--skill-address skill) text)))
          (should (string-suffix-p "Root prepared" text))
          (should (equal "Root prepared" (plist-get prepared :body))))
      (delete-directory dir t))))

(niwa-deftest niwa-skills--dependency-skill
  (:doc "finds a session skill by canonical source key")
  (let* ((skill (niwa-skill--create :source-file "/tmp/skill/SKILL.md"))
         (session (niwa-session--create :skills (list skill))))
    (should (eq skill
                (niwa-skills--dependency-skill
                 session (niwa-skills-source-key
                          "/tmp/skill/SKILL.md"))))
    (should-not (niwa-skills--dependency-skill session "file:/missing"))))

(niwa-deftest niwa-skills--preparation-plan
  (:doc "returns a validated node plan before any preparation effect")
  (let* ((skill (niwa-skill--create :name "one" :body "body"))
         (plan (niwa-skills--preparation-plan
                (list (list :skill skill :arguments ""
                            :role 'instruction :policy-owner-p nil))
                'user :skip-gates t)))
    (should (eq 'ok (plist-get plan :status)))
    (should (eq skill (plist-get (car (plist-get plan :nodes)) :skill)))))

(niwa-deftest niwa-skills--prepare-single
  ()
  ,test
  (test)
  :doc "expands and settles one preflighted node"
  (let* ((skill (niwa-skill--create :name "one"))
         (node (list :skill skill :body "body" :arguments ""
                     :root '(:role instruction) :dependency-depth 0))
         outcome)
    (cl-letf (((symbol-function 'niwa-skills-preparation-expand-body)
               (lambda (body callback &rest _)
                 (funcall callback (list :status 'ok :body body))))
              ((symbol-function 'niwa-skills--run-expansion-hook)
               (lambda (_skill _arguments prompt _origin _session callback)
                 (funcall callback prompt nil))))
      (niwa-skills--prepare-single
       node 'user (lambda (value) (setq outcome value))))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "body" (plist-get outcome :body))))

  :doc "attributes late display failures after the originating buffer dies"
  (let* ((workspace (niwa-workspace--create :root "/workspace/skill-origin/"))
         (skill (niwa-skill--create :name "one"))
         (node (list :skill skill :body "body" :arguments ""
                     :root (list :role 'instruction :display-callback
                                 (lambda (_) (error "Broken display")))))
         (niwa--warn-once-table (make-hash-table :test #'equal))
         pending warnings)
    (cl-letf (((symbol-function 'niwa-skills-preparation-expand-body)
               (lambda (_body callback &rest _) (setq pending callback)))
              ((symbol-function 'display-warning)
               (lambda (_type text &rest _) (push text warnings))))
      (with-temp-buffer
        (setq-local niwa--session (niwa-session-create "main" workspace))
        (niwa-skills--prepare-single node 'internal #'ignore))
      (with-temp-buffer
        (setq-local niwa--workspace
                    (niwa-workspace--create :root "/workspace/other/"))
        (funcall pending '(:status ok :body "body"))))
    (should (equal '("niwa [/workspace/skill-origin/]: Skill display-callback error: Broken display")
                   warnings))))

(niwa-deftest niwa-skills--node-reachable-p
  (:doc "finds identity through a nested dependency tree")
  (let* ((leaf (list :name 'leaf))
         (child (list :dependencies (list leaf)))
         (root (list :dependencies (list child))))
    (should (niwa-skills--node-reachable-p leaf root))
    (should-not (niwa-skills--node-reachable-p (list :name 'other) root))))

(provide 'test-niwa-skills-invoke)
;;; test-niwa-skills-invoke.el ends here
