;;; test-mevedel-skills-invoke.el -- Skill invocation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests request context, invocation, fork dispatch, and model tools.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'gptel-openai)
(require 'mevedel-agent-control)
(require 'mevedel-agents)
(require 'mevedel-bash-analysis)
(require 'mevedel-hooks)
(require 'mevedel-models)
(require 'mevedel-permissions)
(require 'mevedel-pipeline)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-preparation)
(require 'mevedel-structs)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-render-data)
(require 'mevedel-tools)
(require 'mevedel-workspace)



;;
;;; Invocation helpers and core behavior

(defun mevedel-skills-test--expansion-fn (_event)
  "Test hook used by skill expansion."
  '(:updated-input "Expanded by hook"
                   :additional-context "expansion context"))

(defun mevedel-skills-test--block-expansion-fn (_event)
  "Test hook that blocks skill expansion."
  '(:continue nil :stop-reason "blocked expansion"))


;;
;;; Phase B -- substitution, shell injection, execution


;;
;;; Request-scoped skill context

(mevedel-deftest mevedel-skills-commit-invoked-records ()
  ,test
  (test)
  :doc "appends prepared records in order and treats nil as a no-op"
  (let* ((session (mevedel-skills-test--make-session))
         (first (mevedel-skill-invocation-record--create :name "first"))
         (second (mevedel-skill-invocation-record--create :name "second")))
    (mevedel-skills-commit-invoked-records session (list first))
    (mevedel-skills-commit-invoked-records session nil)
    (mevedel-skills-commit-invoked-records session (list second))
    (should (equal (list first second)
                   (mevedel-session-invoked-skills session)))))

(mevedel-deftest mevedel-skills--drain-pending-context ()
  ,test
  (test)
  :doc "drain commits non-policy context and clears the buffer-local stash"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "d" :root "/tmp/d" :name "d"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session))
         (rules '(("Bash" :pattern "echo *" :action allow)))
         (records (list (mevedel-skill-invocation-record--create
                         :name "demo" :args "x"
                         :role 'command :origin 'user
                         :turn 1 :source-path "/tmp/demo/SKILL.md"
                         :prepared-body "Hello"))))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context
                  (list :permission-rules rules
                        :model (mevedel-model-tier-selector 'fast)
                        :effort 'high
                        :ptc-primitives nil
                        :invoked-skills records))
      (mevedel-skills--drain-pending-context request)
      (should (equal rules
                     (mevedel-request-skill-permission-rules request)))
      (should (null (mevedel-request-ptc-primitives request)))
      (should (equal records (mevedel-session-invoked-skills session)))
      ;; Stash is cleared after drain.
      (should (null mevedel-skills--pending-request-context))))

  :doc "drain is a no-op when no stash present"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "d" :root "/tmp/d" :name "d"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      ;; No stash.
      (mevedel-skills--drain-pending-context request)
      (should (null (mevedel-request-skill-permission-rules request))))))

(mevedel-deftest mevedel-skills--transform-apply-request-model-policy ()
  ,test
  (test)
  :doc "pending slash tier sets prompt-buffer backend and model locals"
  (mevedel-skills-test--with-model-backends
   (let* ((session (mevedel-skills-test--make-session))
          (mevedel-model-tiers
           '((fast :provider "Fast:fast-model")
             (balanced :provider "Balanced:balanced-model")
             (strong)))
          (mevedel-model-workloads '((planning :tier balanced)))
          (chat (generate-new-buffer " *skill-model-chat*")))
     (unwind-protect
         (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
           (with-current-buffer chat
             (setf (mevedel-session-plan-mode session) t)
             (setq-local mevedel--session session
                         mevedel-skills--pending-request-context
                         (list :model (mevedel-model-tier-selector 'fast))))
           (with-temp-buffer
             (setq-local gptel-backend (gptel-get-backend "Balanced"))
             (setq-local gptel-model 'balanced-model)
             (mevedel-skills--transform-apply-request-model-policy fsm)
             (should (equal "Fast" (gptel-backend-name gptel-backend)))
             (should (eq 'fast-model gptel-model))))
       (kill-buffer chat))))

  :doc "Plan root requests use planning while retained agents keep their base"
  (mevedel-skills-test--with-model-backends
   (let* ((session (mevedel-skills-test--make-session))
          (mevedel-model-tiers
           '((fast :provider "Fast:fast-model")
             (balanced :provider "Balanced:balanced-model")))
          (mevedel-model-workloads '((planning :tier fast)))
          (chat (generate-new-buffer " *planning-model-chat*")))
     (unwind-protect
         (progn
           (setf (mevedel-session-plan-mode session) t)
           (with-current-buffer chat
             (setq-local mevedel--session session
                         gptel-backend (gptel-get-backend "Balanced")
                         gptel-model 'balanced-model))
           (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
             (with-temp-buffer
               (mevedel-skills--transform-apply-request-model-policy fsm)
               (should (eq 'fast-model gptel-model)))
             (with-current-buffer chat
               (setq-local mevedel--agent-invocation t))
             (with-temp-buffer
               (mevedel-skills--transform-apply-request-model-policy fsm)
               (should (eq 'balanced-model gptel-model)))))
       (kill-buffer chat))))

  :doc "invalid planning policy fails before request realization"
  (mevedel-skills-test--with-model-backends
   (let* ((session (mevedel-skills-test--make-session))
          (mevedel-model-workloads
           '((planning :tier fast :provider "Fast:fast-model")))
          (chat (generate-new-buffer " *invalid-planning-model-chat*")))
     (unwind-protect
         (progn
           (setf (mevedel-session-plan-mode session) t)
           (with-current-buffer chat
             (setq-local mevedel--session session))
           (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
             (with-temp-buffer
               (should-error
                (mevedel-skills--transform-apply-request-model-policy fsm)
                :type 'user-error))))
       (kill-buffer chat))))

  :doc "pending concrete provider sets prompt-buffer backend and model locals"
  (mevedel-skills-test--with-model-backends
   (let ((chat (generate-new-buffer " *skill-model-chat*")))
     (unwind-protect
         (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
           (with-current-buffer chat
             (setq-local mevedel-skills--pending-request-context
                         (list :model
                               (mevedel-model-resolve-provider
                                "Balanced:balanced-model"))))
           (with-temp-buffer
             (setq-local gptel-backend (gptel-get-backend "Fast"))
             (setq-local gptel-model 'fast-model)
             (mevedel-skills--transform-apply-request-model-policy fsm)
             (should (equal "Balanced" (gptel-backend-name gptel-backend)))
             (should (eq 'balanced-model gptel-model))))
       (kill-buffer chat))))

  :doc "pending effort uses gptel validation and reaches the prompt buffer"
  (mevedel-skills-test--with-model-backends
   (let ((chat (generate-new-buffer " *skill-effort-chat*"))
         (old-custom (get 'gptel-reasoning-effort 'custom-type))
         (old-effort (get 'fast-model :reasoning-effort)))
     (unwind-protect
         (progn
           (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
           (put 'fast-model :reasoning-effort '(member low high))
           (let ((fsm (gptel-make-fsm :info (list :buffer chat))))
             (with-current-buffer chat
               (setq-local mevedel-skills--pending-request-context
                           (list :model
                                 (mevedel-model-resolve-provider
                                  "Fast:fast-model")
                                 :effort 'high)))
             (with-temp-buffer
               (mevedel-skills--transform-apply-request-model-policy fsm)
               (should (eq 'fast-model gptel-model))
               (should (eq 'high gptel-reasoning-effort))
               (should (eq 'high
                           (plist-get (gptel-fsm-info fsm)
                                      :reasoning-effort))))))
       (put 'gptel-reasoning-effort 'custom-type old-custom)
       (put 'fast-model :reasoning-effort old-effort)
       (kill-buffer chat)))))


(mevedel-deftest mevedel-skills--invoke-inline/elisp-injection ()
  ,test
  (test)
  :doc "skill allowed-tools [Eval] authorizes elisp body injection end to end"
  (mevedel-tool-exec--register)
  (let* ((mevedel-skills-include-bundled nil)
         (root (make-temp-file "mevedel-skills-eval-" t))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         outcome)
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           root "eval-skill"
           "name: eval-skill
allowed-tools:
  - Eval
" "result=!el`(+ 2 4)`")
          (let ((skill (car (mevedel-skills-scan root '(".")))))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (mevedel-skills-invoke
               skill nil (lambda (o) (setq outcome o))
               :origin 'user)
              (while (null outcome)
                (accept-process-output nil 0.01)))
            (should (eq 'ok (plist-get outcome :status)))
            (should (equal "result=6" (plist-get outcome :body)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

;;
;;; mevedel-skills-invoke (unified invocation API)

(mevedel-deftest mevedel-skills--preparation-rejection ()
  ,test
  (test)
  :doc "rejects invalid skill, role, origin, and named fork agent"
  (should (eq 'unknown-skill
              (plist-get (mevedel-skills--preparation-rejection
                          nil 'command 'user)
                         :reason)))
  (let ((skill (mevedel-skill--create :name "alpha" :body "Body")))
    (should (eq 'invalid-role
                (plist-get (mevedel-skills--preparation-rejection
                            skill 'other 'user)
                           :reason)))
    (should (eq 'invalid-origin
                (plist-get (mevedel-skills--preparation-rejection
                            skill 'command 'other)
                           :reason))))
  (let ((skill (mevedel-skill--create
                :name "alpha" :body "Body" :context 'fork :agent "missing")))
    (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
      (should (eq 'unknown-agent
                  (plist-get (mevedel-skills--preparation-rejection
                              skill 'command 'user)
                             :reason)))))

  :doc "accepts a valid preparation request"
  (let ((skill (mevedel-skill--create :name "alpha" :body "Body")))
    (should-not
     (mevedel-skills--preparation-rejection skill 'command 'model))))

(mevedel-deftest mevedel-skills--preparation-policy ()
  ,test
  (test)
  :doc "an owner merges preset policy and validates its request workload"
  (let* ((skill (mevedel-skill--create
                 :name "alpha" :context 'fork :agent "reviewer"
                 :model "fast" :effort 'low))
         (mevedel-model-tiers '((fast) (strong)))
         (mevedel-model-workloads '(($alpha :tier strong :effort high)))
         call
         outcome)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (workload selector effort)
                 (setq call (list workload selector effort))
                 '(:backend inherited :model inherited :effort high))))
      (setq outcome (mevedel-skills--preparation-policy skill 'user t)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal '(:tier strong) (plist-get outcome :model)))
    (should (eq 'high (plist-get outcome :effort)))
    (should (equal '("reviewer" (:tier strong) high) call)))

  :doc "a model-side inline non-owner reports fields without parsing them"
  (let* ((skill (mevedel-skill--create
                 :name "alpha" :context 'inline
                 :model "invalid selector" :effort 'impossible))
         (mevedel-model-workloads
          '(($alpha :tier missing :provider "Missing:model"))))
    (cl-letf (((symbol-function 'mevedel-model-merge-skill-policy)
               (lambda (&rest _) (ert-fail "non-owner merged policy")))
              ((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _) (ert-fail "non-owner resolved policy"))))
      (let ((outcome
             (mevedel-skills--preparation-policy skill 'model nil)))
        (should (eq 'ok (plist-get outcome :status)))
        (should (equal '(model effort)
                       (plist-get outcome :ignored-fields))))))

  :doc "a non-model non-owner silently retains session policy"
  (let ((skill (mevedel-skill--create
                :name "alpha" :model "invalid" :effort 'impossible)))
    (should (equal '(:status ok :ignored-fields nil)
                   (mevedel-skills--preparation-policy skill 'user nil))))

  :doc "owner validation failures become structured invocation failures"
  (let ((skill (mevedel-skill--create :name "alpha" :model "fast"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _) (user-error "Unsupported effort"))))
      (setq outcome (mevedel-skills--preparation-policy skill 'user t)))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'invalid-policy (plist-get outcome :reason)))
    (should (equal "Unsupported effort" (plist-get outcome :message)))))

(mevedel-deftest mevedel-skills--preparation-settler ()
  ,test
  (test)
  :doc "installs temporary request, restores prior request, and settles once"
  (let* ((session (mevedel-skills-test--make-session))
         (previous (mevedel-request--create :session session))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse nil)))
         outcomes)
    (with-temp-buffer
      (setq-local mevedel--current-request previous)
      (let ((settle (mevedel-skills--preparation-settler
                     session rules hooks
                     (lambda (outcome) (push outcome outcomes)))))
        (should-not (eq previous mevedel--current-request))
        (should (eq session
                    (mevedel-request-session mevedel--current-request)))
        (should (equal rules
                       (mevedel-request-skill-permission-rules
                        mevedel--current-request)))
        (should (equal hooks
                       (mevedel-request-hook-rules mevedel--current-request)))
        (funcall settle 'first)
        (should (eq previous mevedel--current-request))
        (funcall settle 'second)
        (should (equal '(first) outcomes))))))

(mevedel-deftest mevedel-skills--preparation-success-outcome ()
  ,test
  (test)
  :doc "builds command body, policy context, and invocation record"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "alpha" :body "Body" :source-file "/tmp/alpha/SKILL.md"))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse nil)))
         (metadata
          (list :skill skill :arguments "task" :role 'command
                :origin 'user :session session :rules rules :hooks hooks
                :model '(:tier fast) :effort 'high))
         (outcome (mevedel-skills--preparation-success-outcome
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
    (should (equal "alpha" (mevedel-skill-invocation-record-name record)))
    (should (equal "task" (mevedel-skill-invocation-record-args record)))
    (should (eq 'command (mevedel-skill-invocation-record-role record)))
    (should (eq 'user (mevedel-skill-invocation-record-origin record)))
    (should (equal "/root"
                   (mevedel-skill-invocation-record-agent-path record)))
    (should (equal "expanded"
                   (mevedel-skill-invocation-record-prepared-body record))))

  :doc "instruction outcome omits command policy"
  (let* ((skill (mevedel-skill--create
                 :name "alpha" :body "Body" :context 'fork))
         (metadata (list :skill skill :arguments "" :role 'instruction
                         :origin 'user :rules '(ignored)
                         :model 'ignored :effort 'ignored :hooks '(ignored)))
         (outcome (mevedel-skills--preparation-success-outcome
                   metadata "original" "expanded" nil))
         (context (plist-get outcome :request-context)))
    (should (eq 'instruction (plist-get outcome :kind)))
    (should-not (plist-get context :permission-rules))
    (should-not (plist-get context :model))
    (should-not (plist-get context :effort))
    (should-not (plist-get context :hook-rules))))

(mevedel-deftest mevedel-skills-prepare ()
  ,test
  (test)
  :doc "instruction preparation isolates command metadata and forces empty args"
  (let* ((session (mevedel-skills-test--make-session))
         (rules '(("Bash" :pattern "echo *" :action allow)))
         (hooks '((PreToolUse ((:hooks nil)))))
         (skill (mevedel-skill--create
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
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-skills-preparation-expand-body)
                 (lambda (body callback &rest _)
                   (setq injection-rules
                         (mevedel-request-skill-permission-rules
                          mevedel--current-request))
                   (funcall callback (list :status 'ok :body body))))
                ((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq expansion-hooks
                         (mevedel-request-hook-rules
                          mevedel--current-request))
                   (funcall callback nil)))
                ((symbol-function 'mevedel-agent-control-spawn)
                 (lambda (&rest _args) (setq dispatched t))))
        (mevedel-skills-prepare
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
                  (mevedel-skill-invocation-record-role record)))
      (should (eq 'user
                  (mevedel-skill-invocation-record-origin record)))))

  :doc "command preparation returns policy context without committing it"
  (let* ((session (mevedel-skills-test--make-session))
         (rules '(("Read" :action allow)))
         (hooks '((PreToolUse ((:hooks nil)))))
         (skill (mevedel-skill--create
                 :name "alpha" :body "Do $ARGUMENTS"
                 :model "fast" :effort 'high
                 :allowed-tool-rules rules :hooks hooks))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (&rest _) '(:model inherited :effort high))))
        (mevedel-skills-prepare
         skill "work"
         (lambda (value) (setq outcome value))
         :role 'command :origin 'user :policy-owner-p t)
        (should-not mevedel-skills--pending-request-context)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "Do work" (plist-get outcome :body)))
    (let* ((context (plist-get outcome :request-context))
           (record (car (plist-get context :invoked-skills))))
      (should (equal rules (plist-get context :permission-rules)))
      (should (equal hooks (plist-get context :hook-rules)))
      (should (equal (mevedel-model-tier-selector 'fast)
                     (plist-get context :model)))
      (should (eq 'high (plist-get context :effort)))
      (should (eq 'command
                  (mevedel-skill-invocation-record-role record)))
      (should (eq 'user
                  (mevedel-skill-invocation-record-origin record)))))

  :doc "user and model commands omit untrusted project skill hooks"
  (let* ((root (make-temp-file "mevedel-skill-hook-origin-" t))
         (user-dir (make-temp-file "mevedel-skill-hook-state-" t))
         (workspace (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "hook-origin" workspace root))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (mevedel-hooks-require-project-trust t)
         (mevedel-skills-include-bundled nil))
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           (file-name-concat root ".mevedel" "skills") "hooked"
           "name: hooked
description: Hooked
hooks:
  PreToolUse:
    - matcher: Bash
      hooks:
        - type: elisp
          function: mevedel-skills-test--hook-fn
" "Body")
          (let ((skill (car (mevedel-skills-scan
                             root '(".mevedel/skills") workspace))))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (dolist (origin '(user model))
                (let (outcome)
                  (mevedel-skills-prepare
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
                (make-temp-file "mevedel-skill-remote" t)))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) root))
         (native-skill-dir (file-name-concat root "skill"))
         (remote-skill-dir (file-name-concat remote-root "skill"))
         (workspace (mevedel-workspace--create
                     :type 'file :id "remote-skill" :root remote-root
                     :name "remote-skill"
                     :file-cache (mevedel-file-cache--create
                                  :table (make-hash-table :test #'equal)
                                  :order nil :total-bytes 0)))
         session
         calls)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
                                              (setq session
                                                    (mevedel-session-create "remote-skill" workspace remote-root))
                                              (with-temp-buffer
                                                (setq-local mevedel--session session)
                                                (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
                                                           (lambda (tool callback args)
                                                             (push (list (mevedel-tool-name tool)
                                                                         (plist-get args :command))
                                                                   calls)
                                                             (funcall callback "expanded"))))
                                                  (dolist (source '(project plugin))
                                                    (let ((skill (mevedel-skill--create
                                                                  :name (symbol-name source)
                                                                  :source source
                                                                  :source-dir remote-skill-dir
                                                                  :body (concat
                                                                         "dir=${MEVEDEL_SKILL_DIR}|"
                                                                         "${CLAUDE_SKILL_DIR} shell=!`pwd`")))
                                                          outcome)
                                                      (mevedel-skills-prepare
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
                       (make-temp-file "mevedel-skill-target" t)))
         (client-root (file-name-as-directory
                       (make-temp-file "mevedel-skill-client" t)))
         (project-skills (file-name-concat target-root ".mevedel" "skills"))
         (user-skills (file-name-concat client-root "skills"))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) target-root))
         (workspace (mevedel-workspace--create
                     :type 'file :id remote-root :root remote-root
                     :name "remote-discovery"
                     :file-cache (mevedel-test-file-cache-create)))
         (mevedel-skills-include-bundled nil)
         session
         calls
         project-outcome
         user-outcome)
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           project-skills "project-shell"
           "name: project-shell\ndescription: Target shell resource\n"
           "project=!`printf project`")
          (mevedel-skills-test--write-skill
           user-skills "user-shell"
           "name: user-shell\ndescription: Client shell resource\n"
           "user=!`printf user`")
          (mevedel-test--with-local-shell-tramp nil
                                                (setq session
                                                      (mevedel-session-create
                                                       "remote-discovery" workspace remote-root))
                                                (let* ((skills (mevedel-skills-scan
                                                                remote-root
                                                                (list ".mevedel/skills" user-skills)))
                                                       (project (cl-find "project-shell" skills
                                                                         :key #'mevedel-skill-name :test #'equal))
                                                       (user (cl-find "user-shell" skills
                                                                      :key #'mevedel-skill-name :test #'equal)))
                                                  (should (eq 'project (mevedel-skill-source project)))
                                                  (should (file-remote-p (mevedel-skill-source-file project)))
                                                  (should (eq 'user (mevedel-skill-source user)))
                                                  (should-not (file-remote-p (mevedel-skill-source-file user)))
                                                  (with-temp-buffer
                                                    (setq-local default-directory remote-root)
                                                    (setq-local mevedel--session session)
                                                    (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
                                                               (lambda (tool callback args)
                                                                 (push (list (mevedel-tool-name tool)
                                                                             (plist-get args :command))
                                                                       calls)
                                                                 (funcall callback "expanded"))))
                                                      (mevedel-skills-prepare
                                                       project "" (lambda (value) (setq project-outcome value))
                                                       :role 'command :origin 'model)
                                                      (mevedel-skills-prepare
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
                (make-temp-file "mevedel-skill-origin" t)))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) root))
         (foreign-root (format "/mevedelmock:foreign:%s" root))
         (workspace (mevedel-workspace--create
                     :type 'file :id "remote-origin" :root remote-root
                     :name "remote-origin"
                     :file-cache (mevedel-file-cache--create
                                  :table (make-hash-table :test #'equal)
                                  :order nil :total-bytes 0)))
         session
         dispatched)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("foreign")
                                              (setq session
                                                    (mevedel-session-create "remote-origin" workspace remote-root))
                                              (with-temp-buffer
                                                (setq-local mevedel--session session)
                                                (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
                                                           (lambda (&rest _)
                                                             (setq dispatched t))))
                                                  (dolist (skill
                                                           (list
                                                            (mevedel-skill--create
                                                             :name "user" :source 'user
                                                             :source-dir (file-name-concat root "user")
                                                             :body "!`pwd`")
                                                            (mevedel-skill--create
                                                             :name "bundled" :source 'bundled
                                                             :source-dir (file-name-concat root "bundled")
                                                             :body "!`pwd`")
                                                            (mevedel-skill--create
                                                             :name "managed" :source 'managed
                                                             :source-dir (file-name-concat root "managed")
                                                             :body "!`pwd`")
                                                            (mevedel-skill--create
                                                             :name "client-plugin" :source 'plugin
                                                             :source-dir (file-name-concat root "plugin")
                                                             :body "!`pwd`")
                                                            (mevedel-skill--create
                                                             :name "foreign-plugin" :source 'plugin
                                                             :source-dir (file-name-concat foreign-root "plugin")
                                                             :body "!`pwd`")))
                                                    (let (outcome)
                                                      (mevedel-skills-prepare
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
                (make-temp-file "mevedel-skill-elisp" t)))
         (remote-root (format "/mevedelmock:%s:%s"
                              (system-name) root))
         (workspace (mevedel-workspace--create
                     :type 'file :id "remote-elisp" :root remote-root
                     :name "remote-elisp"
                     :file-cache (mevedel-file-cache--create
                                  :table (make-hash-table :test #'equal)
                                  :order nil :total-bytes 0)))
         session
         (skill (mevedel-skill--create
                 :name "user" :source 'user
                 :source-dir (file-name-concat root "user")
                 :body "value=!el`(+ 1 2)`"))
         outcome expression)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
                                              (setq session
                                                    (mevedel-session-create "remote-elisp" workspace remote-root))
                                              (with-temp-buffer
                                                (setq-local mevedel--session session)
                                                (cl-letf (((symbol-function
                                                            'mevedel-skills-preparation--run-elisp-expression-async)
                                                           (lambda (form _marker callback)
                                                             (setq expression form)
                                                             (funcall callback '(:status ok :output "3"))))
                                                          ((symbol-function 'mevedel-pipeline-run-tool)
                                                           (lambda (&rest _)
                                                             (ert-fail "Elisp injection reached Bash"))))
                                                  (mevedel-skills-prepare
                                                   skill "" (lambda (value) (setq outcome value))
                                                   :role 'command :origin 'model))))
      (delete-directory root t))
    (should (equal "(+ 1 2)" expression))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "value=3" (plist-get outcome :body)))))

(mevedel-deftest mevedel-skills-invoke (:quiet t)
  ,test
  (test)
  :doc "inline skill yields :status ok :kind inline with prepared body"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "i" :root "/tmp/i" :name "i"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "shout"
                 :body "YELL $ARGUMENTS"))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills-invoke
       skill "loudly"
       (lambda (o) (setq outcome o))
       :origin 'model))
    (should (eq 'ok (plist-get outcome :status)))
    (should (eq 'inline (plist-get outcome :kind)))
    (should (equal "YELL loudly" (plist-get outcome :body))))

  :doc "user origin installs the pending stash"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "s" :root "/tmp/s" :name "s"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Hello"
                 :model "fast"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user)
      (let ((stash mevedel-skills--pending-request-context))
        (should (equal (mevedel-model-tier-selector 'fast)
                       (plist-get stash :model)))
        (should (equal '(("Read" :action allow))
                       (plist-get stash :permission-rules)))
        (should (= 1 (length (plist-get stash :invoked-skills))))))
    (should (eq 'ok (plist-get outcome :status))))

  :doc "UserPromptExpansion can rewrite user-origin inline skill output"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "slash-expansion" :root "/tmp/slash-expansion"
              :name "slash-expansion"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Original body"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         (mevedel-hook-rules
          '((UserPromptExpansion
             ((:hooks ((:type elisp
                              :function mevedel-skills-test--expansion-fn)))))))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (mevedel-skills-invoke
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
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "slash-expansion-malformed"
              :root "/tmp/slash-expansion-malformed"
              :name "slash-expansion-malformed"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Original body"
                 :allowed-tool-rules
                 '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback 'passed))))
        (mevedel-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :origin 'user)
        (should mevedel-skills--pending-request-context)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "Original body" (plist-get outcome :body))))

  :doc "UserPromptExpansion can block user-origin inline skill output"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "slash-expansion-block"
              :root "/tmp/slash-expansion-block"
              :name "slash-expansion-block"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Original body"))
         (mevedel-hook-rules
          '((UserPromptExpansion
             ((:hooks ((:type elisp
                              :function
                              mevedel-skills-test--block-expansion-fn)))))))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user)
      (should-not mevedel-skills--pending-request-context))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'hook-blocked (plist-get outcome :reason)))
    (should (equal "blocked expansion" (plist-get outcome :message))))

  :doc "user-origin preparation failure leaves the pending stash empty"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "slash-fail" :root "/tmp/slash-fail"
              :name "slash-fail"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Hello"
                 :allowed-tool-rules '(("Read" :action allow))))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel-skills--pending-request-context nil)
      (cl-letf (((symbol-function 'mevedel-skills-preparation-expand-body)
                 (lambda (_text callback &rest _)
                   (funcall callback
                            '(:status error
                                      :reason injection-failed
                                      :message "boom")))))
        (mevedel-skills-invoke
         skill nil
         (lambda (o) (setq outcome o))
         :origin 'user)
        (should (null mevedel-skills--pending-request-context))
        (should-not (bound-and-true-p mevedel--current-request))))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'injection-failed (plist-get outcome :reason))))

  :doc "model inline origin installs additive context but ignores policy"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "t" :root "/tmp/t" :name "t"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create :session session))
         (skill (mevedel-skill--create
                 :name "demo"
                 :body "Hi"
                 :model "fast"
                 :allowed-tool-rules
                 '(("Bash" :pattern "ls" :action allow))))
         outcome)
    (with-temp-buffer
      (setq mevedel--session session)
      (setq-local mevedel--current-request request)
      (mevedel-skills-invoke
       skill nil
       (lambda (value) (setq outcome value))
       :origin 'model))
    (should (equal '(model)
                   (plist-get outcome :ignored-policy-fields)))
    (should (equal '(("Bash" :pattern "ls" :action allow))
                   (mevedel-request-skill-permission-rules request))))

  :doc "disabled user skill tells the user how to enable or escape it"
  (let ((skill (mevedel-skill--create
                :name "hidden"
                :body "X"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-skills-skill-enabled-p)
               (lambda (_) nil)))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'user))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason)))
    (should (string-match-p "/skills enable hidden"
                            (plist-get outcome :message)))
    (should (string-search "\\$hidden" (plist-get outcome :message))))

  :doc "user-invocable: false rejects user origin"
  (let ((skill (mevedel-skill--create
                :name "internal-only"
                :body "X"
                :user-invocable-p nil))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'user)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason))))

  :doc "disable-model-invocation rejects model origin"
  (let ((skill (mevedel-skill--create
                :name "human-only"
                :body "X"
                :model-invocable-p nil))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'model)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'disabled (plist-get outcome :reason))))

  :doc "skip-gates lets internal commands invoke disabled skills"
  (let ((skill (mevedel-skill--create
                :name "human-only"
                :body "X"
                :model-invocable-p nil))
        outcome)
    (cl-letf (((symbol-function 'mevedel-skills-skill-enabled-p)
               (lambda (_) nil)))
      (mevedel-skills-invoke
       skill nil
       (lambda (o) (setq outcome o))
       :origin 'model
       :skip-gates t))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "X" (plist-get outcome :body))))

  :doc "missing body returns load-failure error"
  (let ((skill (mevedel-skill--create :name "no-body"))
        outcome)
    (mevedel-skills-invoke
     skill nil
     (lambda (o) (setq outcome o))
     :origin 'model)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'load-failure (plist-get outcome :reason))))

  :doc "display-callback receives done event on success"
  (let ((skill (mevedel-skill--create :name "ok" :body "Hi"))
        events)
    (mevedel-skills-invoke
     skill nil
     (lambda (_) nil)
     :origin 'internal
     :display-callback (lambda (e) (push e events)))
    (should (cl-some (lambda (e) (eq (plist-get e :event) 'done))
                     events)))

  :doc "display-callback receives error event on failure"
  (let ((skill (mevedel-skill--create :name "no-body"))
        events)
    (mevedel-skills-invoke
     skill nil
     (lambda (_) nil)
     :origin 'internal
     :display-callback (lambda (e) (push e events)))
    (should (cl-some (lambda (e) (eq (plist-get e :event) 'error))
                     events))))


;;
;;; Phase 6: build-fork-agent + fork dispatch routing

(mevedel-deftest mevedel-skills--build-fork-agent ()
  ,test
  (test)
  :doc "named-agent path looks up via the registry"
  (let ((agent (mevedel-agent--create :name "explorer" :tools nil
                                      :system-prompt "")))
    (cl-letf (((symbol-function 'mevedel-agent-get)
               (lambda (n) (and (equal n "explorer") agent))))
      (let ((skill (mevedel-skill--create
                    :name "demo" :context 'fork :agent "explorer")))
        (should (eq agent (mevedel-skills--build-fork-agent skill))))))

  :doc "named-agent path returns nil for unknown agent"
  (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
    (let ((skill (mevedel-skill--create
                  :name "demo" :context 'fork :agent "missing")))
      (should (null (mevedel-skills--build-fork-agent skill)))))

  :doc "parent-inherited path synthesizes a `skill:<name>' agent"
  ;; The synthetic agent's name is `skill:<skill-name>' and its
  ;; system prompt is captured from the calling buffer's
  ;; `gptel-system-prompt' at spawn time.
  (let ((skill (mevedel-skill--create
                :name "demo" :context 'fork
                :description "A test skill")))
    (with-temp-buffer
      (setq-local gptel-system-prompt "captured-system-prompt")
      (setq-local mevedel-agents--specs nil)
      (let ((agent (mevedel-skills--build-fork-agent skill)))
        (should (mevedel-agent-p agent))
        (should (equal "skill:demo" (mevedel-agent-name agent)))
        (should (equal "captured-system-prompt"
                       (mevedel-agent-system-prompt agent)))
        ;; The dispatch passes this agent directly, so a name only the
        ;; fork knows never enters the preset-owned role roster.
        (should-not mevedel-agents--specs)))))

(mevedel-deftest mevedel-skills-dispatch-prepared-fork ()
  ,test
  (test)
  :doc "spawns one retained fork and maps its canonical RESULT"
  (let* ((session (mevedel-skills-test--make-session))
         (agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "unused"))
         (record (mevedel-skill-invocation-record--create
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
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-agent-get)
                 (lambda (_) agent))
                ((symbol-function 'mevedel-agent-control-spawn)
                 (lambda (actual-session task-name message callback
                                         &rest keys)
                   (let* ((path (concat "/root/" task-name))
                          (retained
                           (mevedel-agent-record--create
                            :id "storage-id"
                            :path path
                            :parent-path "/root"
                            :role "explorer"
                            :activity 'idle
                            :conversation-location "agents/demo.chat.org")))
                     (setf (mevedel-session-agent-registry actual-session)
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
        (mevedel-skills-dispatch-prepared-fork
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
                   (mevedel-session-invoked-skills session)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "done" (plist-get outcome :result)))
    (should (equal "/root/skill_demo" (plist-get outcome :agent-path)))
    (should (equal "/root/skill_demo"
                   (plist-get (plist-get outcome :render-data) :path)))
    (should (equal '((:event "expansion"))
                   (plist-get outcome :hook-audits))))

  :doc "maps a non-completed RESULT to a normalized skill error"
  (let* ((session (mevedel-skills-test--make-session))
         (agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer" :body "Body"))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) agent))
                ((symbol-function 'mevedel-agent-control-spawn)
                 (lambda (_session _task _message callback &rest keys)
                   (let ((record
                          (mevedel-agent-record--create
                           :path "/root/skill_demo" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_demo"
                            :recipient "/root" :outcome interrupted
                            :payload "Stopped"))
                   #'ignore)))
        (mevedel-skills-invoke
         skill nil (lambda (value) (setq outcome value)) :origin 'model)))
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'agent-interrupted (plist-get outcome :reason)))
    (should (equal "Stopped" (plist-get outcome :message))))

  :doc "rejects an invalid prepared outcome without dispatching"
  (let (outcome dispatched)
    (cl-letf (((symbol-function 'mevedel-agent-control-spawn)
               (lambda (&rest _) (setq dispatched t))))
      (mevedel-skills-dispatch-prepared-fork
       '(:status error :reason failed)
       (lambda (value) (setq outcome value))))
    (should-not dispatched)
    (should (eq 'error (plist-get outcome :status)))
    (should (eq 'invalid-prepared-fork (plist-get outcome :reason)))))

(mevedel-deftest mevedel-skills-invoke-fork ()
  ,test
  (test)
  :doc "forwards prompt context, description, and invocation callback"
  (let* ((session (mevedel-skills-test--make-session))
         (agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "explorer"
                 :body "Task body $ARGUMENTS"))
         (progress-callback #'ignore)
         captured
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) agent))
                ((symbol-function 'mevedel-agent-control-spawn)
                 (lambda (_session _task message callback &rest keys)
                   (setq captured (cons message keys))
                   (let ((record
                          (mevedel-agent-record--create
                           :path "/root/skill_demo" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_demo"
                            :recipient "/root" :outcome completed
                            :payload "agent finished"))
                   #'ignore)))
        (mevedel-skills-invoke
         skill "the task" (lambda (value) (setq outcome value))
         :origin 'user
         :additional-context "<hook-context>ctx</hook-context>"
         :description "target hint"
         :on-invocation progress-callback)))
    (should (string-match-p "the task" (car captured)))
    (should (string-match-p "<hook-context>ctx</hook-context>" (car captured)))
    (should (equal "target hint" (plist-get (cdr captured) :description)))
    (should (eq progress-callback
                (plist-get (cdr captured) :on-invocation)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (equal "agent finished" (plist-get outcome :result))))

  :doc "unknown named roles fail before spawning"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :agent "missing"))
         outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-agent-get) (lambda (_) nil)))
        (mevedel-skills-invoke
         skill nil (lambda (value) (setq outcome value)) :origin 'model)))
    (should (eq 'unknown-agent (plist-get outcome :reason))))

  :doc "omitted roles spawn a parent-inherited synthetic agent"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "demo" :context 'fork :body "Body"))
         dispatched-agent)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel-agents--specs nil)
      (cl-letf (((symbol-function 'mevedel-agent-control-spawn)
                 (lambda (_session _task _message callback &rest keys)
                   (setq dispatched-agent (plist-get keys :agent))
                   (let ((record
                          (mevedel-agent-record--create
                           :path "/root/skill_demo" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_demo"
                            :recipient "/root" :outcome completed :payload "result"))
                   #'ignore)))
        (let (outcome)
          (mevedel-skills-invoke
           skill nil (lambda (value) (setq outcome value)) :origin 'model)
          (should (eq 'ok (plist-get outcome :status))))))
    (should (mevedel-agent-p dispatched-agent))
    (should (equal "skill:demo" (mevedel-agent-name dispatched-agent)))))


(defun test-mevedel-skills--handler-result (envelope)
  "Return the required result from handler ENVELOPE."
  (should (plist-member envelope :result))
  (plist-get envelope :result))

(mevedel-deftest mevedel-skills--render-skill-tool ()
  ,test
  (test)
  :doc "ignored model-side inline policy renders a warning without changing result"
  (let ((rendering
         (mevedel-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body"
          '(:kind skill-policy-warning
                  :ignored-policy-fields (model effort)))))
    (should (eq 'warning (plist-get rendering :status)))
    (should (string-match-p "ignored model, effort"
                            (plist-get rendering :header)))
    (should (string-match-p "context: fork" (plist-get rendering :body)))
    (should (string-match-p "Prepared body" (plist-get rendering :body))))

  :doc "warning names only the policy field actually ignored"
  (let ((rendering
         (mevedel-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body"
          '(:kind skill-policy-warning
                  :ignored-policy-fields (effort)))))
    (should (string-match-p "ignored effort" (plist-get rendering :header)))
    (should-not (string-match-p "ignored model" (plist-get rendering :header))))

  :doc "ordinary skill rendering remains successful and unchanged"
  (let ((rendering
         (mevedel-skills--render-skill-tool
          "Skill" '(:name "review") "Prepared body" nil)))
    (should-not (plist-get rendering :status))
    (should (equal "Prepared body" (plist-get rendering :body)))
    (should-not (string-match-p "ignored" (plist-get rendering :header)))))

(mevedel-deftest mevedel-skills--invoke-handler ()
  ,test
  (test)
  :doc "unknown skill returns an error"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "u" :root "/tmp/u" :name "u"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         received)
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills--invoke-handler
       (lambda (r)
         (setq received (test-mevedel-skills--handler-result r)))
       (list :name "nope")))
    (should (string-match-p "Unknown skill" received)))

  :doc "known inline skill is dispatched and body returned"
  (let* ((dir (make-temp-file "mevedel-skills-test-" t))
         (ws (mevedel-workspace--create
              :type 'file :id dir :root dir :name "h"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         received)
    (unwind-protect
        (progn
          (mevedel-skills-test--write-skill
           dir "shout"
           "name: shout
description: Yell
"
           "YELL $ARGUMENTS")
          (setf (mevedel-session-skills session)
                (mevedel-skills-scan dir '(".")))
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--invoke-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :name "shout" :arguments "loudly")))
          (should (equal "YELL loudly" received)))
      (delete-directory dir t)))

  :doc "user-attached skill is an idempotent model-side success"
  (let* ((session (mevedel-skills-test--make-session))
         (source "/tmp/implement/SKILL.md")
         (skill (mevedel-skill--create
                 :name "implement"
                 :body "BODY MUST NOT BE RETURNED"
                 :source-file source
                 :context 'inline
                 :model-invocable-p nil))
         (record (mevedel-skill-invocation-record--create
                  :name "implement"
                  :role 'instruction
                  :origin 'user
                  :source-path source
                  :prepared-body "BODY MUST NOT BE RETURNED"))
         (request (mevedel-request--create :session session))
         received)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local mevedel--session session
                  mevedel--current-request request
                  mevedel-skills--pending-request-context
                  (list :invoked-skills (list record)))
      (mevedel-skills--drain-pending-context request)
      (mevedel-skills--invoke-handler
       (lambda (value)
         (setq received (test-mevedel-skills--handler-result value)))
       '(:name "implement")))
    (should (string-match-p "already attached" received))
    (should-not (string-match-p "BODY MUST NOT BE RETURNED" received))
    (should (equal (list record) (mevedel-session-invoked-skills session))))

  :doc "model-side inline policy is view-only render-data"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "costly" :body "Prepared body"
                 :context 'inline :model "OpenAI:gpt-5-mini" :effort 'low))
         envelope)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills--invoke-handler
       (lambda (value) (setq envelope value))
       '(:name "costly")))
    (should (equal "Prepared body" (plist-get envelope :result)))
    (should (equal '(:kind skill-policy-warning
                           :ignored-policy-fields (model effort))
                   (plist-get envelope :render-data))))

  :doc "registered Skill pipeline separates model result from warning view"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "costly" :body "Prepared body"
                 :context 'inline :model "invalid" :effort 'low))
         (tool (mevedel-tool-ensure "Skill"))
         pipeline-result)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq-local mevedel--session session
                  mevedel--current-request
                  (mevedel-request--create
                   :session session
                   :file-snapshots (make-hash-table :test #'equal)))
      (mevedel-pipeline-run-tool
       tool (lambda (value) (setq pipeline-result value))
       '(:name "costly")))
    (let* ((parts (mevedel-tool-render-data-extract
                   pipeline-result session))
           (visible (car parts))
           (render-data (cdr parts))
           (rendering
            (funcall (mevedel-tool-renderer tool)
                     "Skill" '(:name "costly") visible render-data)))
      (should (equal "Prepared body" visible))
      (should (equal '(:kind skill-policy-warning
                             :ignored-policy-fields (model effort))
                     render-data))
      (should (eq 'warning (plist-get rendering :status)))
      (should (string-match-p "context: fork"
                              (plist-get rendering :body)))))

  :doc "model-side inline skill without policy has no render-data"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "plain" :body "Prepared body" :context 'inline))
         envelope)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (mevedel-skills--invoke-handler
       (lambda (value) (setq envelope value))
       '(:name "plain")))
    (should (equal "Prepared body" (plist-get envelope :result)))
    (should-not (plist-member envelope :render-data)))

  :doc "model-side fork policy owns its child and produces no warning"
  (let* ((mevedel-model-tiers '((fast)))
         (session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "isolated" :body "Prepared body"
                 :context 'fork :model "fast"))
         envelope)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-agent-control-spawn)
                 (lambda (_session _task _message callback &rest keys)
                   (let ((record
                          (mevedel-agent-record--create
                           :path "/root/skill_isolated" :activity 'idle)))
                     (funcall callback
                              (list :outcome 'success :record record)))
                   (funcall
                    (plist-get keys :result-handler)
                    '(:type RESULT :sender "/root/skill_isolated"
                            :recipient "/root" :outcome completed
                            :payload "Child result"))
                   #'ignore)))
        (mevedel-skills--invoke-handler
         (lambda (value) (setq envelope value))
         '(:name "isolated"))))
    (should (equal "Child result" (plist-get envelope :result)))
    (should-not (plist-member envelope :render-data)))

  :doc "model-side invocation uses visible prefixed names after conflicts"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (ws (mevedel-workspace--create
              :type 'file :id "collision" :root "/tmp/collision"
              :name "collision"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (local (mevedel-skill--create
                 :name "shared"
                 :source 'project
                 :source-family 'mevedel
                 :body "LOCAL $ARGUMENTS"))
         (global (mevedel-skill--create
                  :name "shared"
                  :source 'user
                  :source-family 'mevedel
                  :body "GLOBAL $ARGUMENTS"))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (mevedel-skills--qualify-conflicting-names
                 (list local global)))
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--invoke-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :name "local:shared" :arguments "now"))
            (should (equal "LOCAL now" received))
            (setq received nil)
            (mevedel-skills--invoke-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :name "shared"))
            (should (string-match-p "Unknown skill 'shared'" received))))
      (delete-directory user-dir t)))

  :doc "disabled skill is rejected before model invocation"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session nil user-dir))
         (skill (mevedel-skills-test--stateful-skill
                 :name "hidden"
                 :body "should not run"
                 :workspace (mevedel-session-workspace session)))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-skills-set-enabled skill nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--invoke-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :name "hidden")))
          (should (string-match-p "disabled" received)))
      (delete-directory user-dir t))))

(mevedel-deftest mevedel-skills--list-handler ()
  ,test
  (test)
  :doc "returns active model-invocable enabled skills"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session nil user-dir))
         (alpha (mevedel-skill--create
                 :name "alpha" :description "Alpha helper"
                 :active-p t :model-invocable-p t))
         (beta (mevedel-skills-test--stateful-skill
                :name "beta" :description "Beta helper"
                :active-p t :model-invocable-p t
                :workspace (mevedel-session-workspace session)))
         (model-disabled (mevedel-skill--create
                          :name "internal" :description "Internal"
                          :active-p t :model-invocable-p nil))
         (dormant (mevedel-skill--create
                   :name "dormant" :description "Dormant"
                   :active-p nil :model-invocable-p t
                   :path-patterns '("*.el")))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (list alpha beta model-disabled dormant))
          (mevedel-skills-set-enabled beta nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--list-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :query "alp")))
          (should (string-match-p "alpha: Alpha helper" received))
          (should-not (string-match-p "beta" received))
          (should-not (string-match-p "internal" received))
          (should-not (string-match-p "dormant" received))
          (setq received nil)
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--list-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :query "Dormant")))
          (should (string-match-p "dormant \\[dormant path-scoped\\]: Dormant"
                                  received))
          (should-not (string-match-p "internal" received)))
      (delete-directory user-dir t)))

  :doc "refreshes session skills before listing"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (fresh (mevedel-skill--create
                 :name "fresh" :description "Fresh helper"
                 :active-p t :model-invocable-p t))
         refreshed
         received)
    (unwind-protect
        (with-temp-buffer
          (setq mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
                     (lambda (_buffer s)
                       (setq refreshed s)
                       (setf (mevedel-session-skills s) (list fresh)))))
            (mevedel-skills--list-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             nil))
          (should (eq refreshed session))
          (should (string-match-p "fresh: Fresh helper" received)))
      (delete-directory user-dir t)))

  :doc "returns prefixed visible names that can be used with Skill"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session))
         (local (mevedel-skill--create
                 :name "shared"
                 :description "Local helper"
                 :source 'project
                 :source-family 'mevedel
                 :active-p t
                 :model-invocable-p t))
         (global (mevedel-skill--create
                  :name "shared"
                  :description "Global helper"
                  :source 'user
                  :source-family 'mevedel
                  :active-p t
                  :model-invocable-p t))
         received)
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session)
                (mevedel-skills--qualify-conflicting-names
                 (list local global)))
          (with-temp-buffer
            (setq mevedel--session session)
            (mevedel-skills--list-handler
             (lambda (r)
               (setq received (test-mevedel-skills--handler-result r)))
             (list :query "shared")))
          (should (string-match-p "local:shared: Local helper" received))
          (should (string-match-p "global:shared: Global helper" received))
          (should-not (string-match-p "\nshared: " received)))
      (delete-directory user-dir t))))


;;
;;; ListSkills selection primitives

(mevedel-deftest mevedel-skills--listing-describe ()
  ,test
  (test)
  :doc "short entries are returned as-is"
  (let ((skill (mevedel-skill--create
                :name "simplify"
                :description "Review changed code for reuse")))
    (should (equal "- simplify: Review changed code for reuse"
                   (mevedel-skills--listing-describe skill))))

  :doc "entries longer than the cap are truncated with ellipsis"
  (let* ((mevedel-skills-listing-max-entry-chars 20)
         (skill (mevedel-skill--create
                 :name "n"
                 :description "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
         (entry (mevedel-skills--listing-describe skill)))
    (should (= 20 (length entry)))
    (should (string-suffix-p "..." entry)))

  :doc "empty descriptions still produce a stable entry"
  (let ((skill (mevedel-skill--create :name "demo")))
    (should (equal "- demo: "
                   (mevedel-skills--listing-describe skill)))))

(mevedel-deftest mevedel-skills--listing-candidates ()
  ,test
  (test)
  :doc "returns only active, model-invocable skills"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "l" :root "/tmp/l" :name "l"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (active-invocable
          (mevedel-skill--create :name "a" :description "A"
                                 :model-invocable-p t :active-p t))
         (dormant
          (mevedel-skill--create :name "b" :description "B"
                                 :model-invocable-p t
                                 :path-patterns '("*.el")
                                 :active-p nil))
         (disabled
          (mevedel-skill--create :name "c" :description "C"
                                 :model-invocable-p nil :active-p t)))
    (setf (mevedel-session-skills session)
          (list active-invocable dormant disabled))
    (let ((names (mapcar #'mevedel-skill-name
                         (mevedel-skills--listing-candidates session))))
      (should (equal '("a") names))))

  :doc "omits user-disabled skills"
  (let* ((user-dir (make-temp-file "mevedel-skills-state-" t))
         (mevedel-user-dir (file-name-as-directory user-dir))
         (session (mevedel-skills-test--make-session nil user-dir))
         (enabled (mevedel-skill--create
                   :name "enabled" :description "E"
                   :model-invocable-p t :active-p t))
         (disabled (mevedel-skills-test--stateful-skill
                    :name "disabled" :description "D"
                    :model-invocable-p t :active-p t
                    :workspace (mevedel-session-workspace session))))
    (unwind-protect
        (progn
          (setf (mevedel-session-skills session) (list enabled disabled))
          (mevedel-skills-set-enabled disabled nil)
          (let ((names (mapcar #'mevedel-skill-name
                               (mevedel-skills--listing-candidates session))))
            (should (equal '("enabled") names))))
      (delete-directory user-dir t))))

(provide 'test-mevedel-skills-invoke)
;;; test-mevedel-skills-invoke.el ends here
