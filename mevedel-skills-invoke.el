;;; mevedel-skills-invoke.el -- Skill invocation and dispatch -*- lexical-binding: t -*-

;;; Commentary:

;; Owns request-scoped skill context, preparation policy, invocation records,
;; fork dispatch, and model-facing skill handlers.  User input handling lives
;; in `mevedel-skills-input'; body expansion lives in
;; `mevedel-skills-preparation'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents))

(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-models)
(require 'mevedel-skills-core)
(require 'mevedel-turn)

;; `gptel'
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-system-prompt)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(autoload 'gptel-fsm-info "gptel-request")

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-current-path
                  "mevedel-agent-control" (session))
(declare-function mevedel-agent-control-spawn
                  "mevedel-agent-control" t t)
(declare-function mevedel-agent-record-conversation-location
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-id
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-role
                  "mevedel-agent-control" (cl-x) t)

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent--create "mevedel-agents" (&rest args))
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-invocation-hook-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(autoload 'mevedel-agent--create "mevedel-agents")
(autoload 'mevedel-agent-get "mevedel-agents")
(autoload 'mevedel-agent-invocation-hook-rules "mevedel-agents")
(autoload 'mevedel-agent-invocation-skill-permission-rules "mevedel-agents")
(autoload 'mevedel-agent-name "mevedel-agents")

;; `mevedel-hooks'
(declare-function mevedel-hooks-additional-context-string
                  "mevedel-hooks" (decision &optional event))
(declare-function mevedel-hooks-decision-reason
                  "mevedel-hooks" (decision))
(declare-function mevedel-hooks-event-plist
                  "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-run-event
                  "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function mevedel-hooks-sanitize-final-decision
                  "mevedel-hooks" (event decision))
(autoload 'mevedel-hooks-additional-context-string "mevedel-hooks")
(autoload 'mevedel-hooks-decision-reason "mevedel-hooks")
(autoload 'mevedel-hooks-event-plist "mevedel-hooks")
(autoload 'mevedel-hooks-run-event "mevedel-hooks")
(autoload 'mevedel-hooks-sanitize-final-decision "mevedel-hooks")

;; `mevedel-models'
(declare-function mevedel-model-merge-skill-policy
                  "mevedel-models" (skill-name model effort))
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function mevedel-model-skill-policy-fields
                  "mevedel-models" (skill-name model effort))

;; `mevedel-reminders'
(declare-function mevedel-reminders-format-block
                  "mevedel-reminders" (content))
(autoload 'mevedel-reminders-format-block "mevedel-reminders")

;; `mevedel-skills-preparation'
(declare-function mevedel-skills-preparation-expand-body
                  "mevedel-skills-preparation"
                  (text callback &optional skill session))
(declare-function mevedel-skills-preparation-substitute
                  "mevedel-skills-preparation"
                  (text arguments session skill))
(autoload 'mevedel-skills-preparation-expand-body
  "mevedel-skills-preparation")
(autoload 'mevedel-skills-preparation-substitute
  "mevedel-skills-preparation")

;; `mevedel-skills-syntax'
(declare-function mevedel-skills-syntax-parse-dependencies
                  "mevedel-skills-syntax" (text))
(autoload 'mevedel-skills-syntax-parse-dependencies "mevedel-skills-syntax")

;; `mevedel-structs'
(declare-function mevedel-request-attached-skill-records
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-hook-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-ptc-primitives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-registry
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-invoked-skills
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-current-session
                  "mevedel-telemetry" (&optional buffer))
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get "mevedel-tool-registry"
                  (name &optional category))

;; `mevedel-transcript-audit'
(declare-function mevedel--hook-prompt-rewrite-audit-record
                  "mevedel-transcript-audit"
                  (event original submitted &optional reason))
(autoload 'mevedel--hook-prompt-rewrite-audit-record
  "mevedel-transcript-audit")

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(declare-function mevedel-request-begin "mevedel-turn"
                  (session &optional directive-uuid))

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))
(autoload 'mevedel--warn-once "mevedel-utilities")

;;
;;; Request-scoped skill context

(defun mevedel-skills--activate-request-context (request rules hooks)
  "Append skill-scoped RULES and HOOKS to REQUEST."
  (when rules
    (setf (mevedel-request-skill-permission-rules request)
          (append (mevedel-request-skill-permission-rules request)
                  rules)))
  (when hooks
    (setf (mevedel-request-hook-rules request)
          (append (mevedel-request-hook-rules request)
                  hooks))))

(defun mevedel-skills-commit-invoked-records (session records)
  "Append skill invocation RECORDS to SESSION for compaction and replay."
  (when (and session records)
    (setf (mevedel-session-invoked-skills session)
          (append (mevedel-session-invoked-skills session) records))))

(defvar-local mevedel-skills--pending-request-context nil
  "Buffer-local pending request context for the next mevedel-request.

A plist of the form
  (:permission-rules RULES :model MODEL :effort EFFORT
   :hook-rules HOOKS :ptc-primitives NAMES :invoked-skills SKILLS)

populated by user-dispatched skill invocation before `gptel-send'
fires.  The prompt transform consumes MODEL and EFFORT before request
realization.  The WAIT-state begin handler in `mevedel-presets.el' drains
permission and hook rules into the new request and records invoked skills on
the session (see `mevedel-skills--drain-pending-context').

Cleared on drain.  Cleared by an `unwind-protect' in the slash
or skill dispatch path if `gptel-send' aborts before request creation.")

(put 'mevedel-skills--pending-request-context 'permanent-local t)

(defun mevedel-skills-clear-pending-context ()
  "Clear pending skill request context in the current buffer."
  (setq-local mevedel-skills--pending-request-context nil))


(defun mevedel-skills--current-invocation ()
  "Return the active sub-agent invocation, or nil.
Reads the buffer-local `mevedel--agent-invocation' set by
`mevedel-agent-conversation-open' on agent buffers;
returns nil when called outside any sub-agent."
  (and (boundp 'mevedel--agent-invocation)
       mevedel--agent-invocation))

(defun mevedel-skills--current-request ()
  "Return the active request struct, or nil."
  (and (boundp 'mevedel--current-request)
       mevedel--current-request))

(defun mevedel-skills--drain-pending-context (request)
  "Drain `mevedel-skills--pending-request-context' (buffer-local) into REQUEST.

After this call the buffer-local stash is nil.  No-op when no stash
is present.

The stash plist keys map onto request/session state:

- :permission-rules -> `mevedel-request-skill-permission-rules'
- :model/:effort    -> consumed by the pre-realization prompt transform
- :hook-rules       -> `mevedel-request-hook-rules'
- :ptc-primitives   -> `mevedel-request-ptc-primitives'
- :invoked-skills   -> user-origin records identify skill bodies already
                       attached to this request, and all records are appended
                       to `mevedel-session-invoked-skills'"
  (when-let* ((ctx mevedel-skills--pending-request-context))
    (when-let* ((rules (plist-get ctx :permission-rules)))
      (setf (mevedel-request-skill-permission-rules request) rules))
    (when-let* ((hooks (plist-get ctx :hook-rules)))
      (setf (mevedel-request-hook-rules request) hooks))
    (when (plist-member ctx :ptc-primitives)
      (setf (mevedel-request-ptc-primitives request)
            (plist-get ctx :ptc-primitives)))
    (when-let* ((skills (plist-get ctx :invoked-skills))
                (session mevedel--session))
      (setf (mevedel-request-attached-skill-records request)
            (cl-remove-if-not
             (lambda (record)
               (and (mevedel-skill-invocation-record-p record)
                    (eq (mevedel-skill-invocation-record-origin record)
                        'user)))
             skills))
      (mevedel-skills-commit-invoked-records session skills))
    (setq-local mevedel-skills--pending-request-context nil)))

(defun mevedel-skills-request-model-policy ()
  "Resolve the model policy the current buffer's pending request will use.
Root Plan requests use the `planning' workload; a leading user skill
may override model or effort through the pending request context.
gptel funcalls the function-valued system prompt before the prompt
transforms apply this policy to the temp buffer, so any request-time
consumer of the effective model -- the roster budget included -- must
resolve through this one seam rather than read the buffer's
`gptel-model' directly."
  (mevedel-model-resolve-workload
   (and (bound-and-true-p mevedel--session)
        (not (bound-and-true-p mevedel--agent-invocation))
        (mevedel-session-plan-mode mevedel--session)
        'planning)
   (plist-get mevedel-skills--pending-request-context :model)
   (plist-get mevedel-skills--pending-request-context :effort)))

(defun mevedel-skills--transform-apply-request-model-policy (fsm)
  "Pre-realize transform: apply the root request policy to prompt locals.

FSM is the active gptel request state machine.

gptel realizes request payloads from the temp prompt buffer's
buffer-local `gptel-backend' and `gptel-model'.  Root Plan requests use
the `planning' workload; a leading user skill may override its model or
effort.  Retained child-agent requests own their policy and are excluded.
Policy never changes after this realization boundary."
  (let* ((info (gptel-fsm-info fsm))
         (chat-buffer (plist-get info :buffer)))
    (when (and chat-buffer (buffer-live-p chat-buffer))
      (let ((policy
             (with-current-buffer chat-buffer
               (mevedel-skills-request-model-policy))))
        (setq-local gptel-backend (plist-get policy :backend))
        (setq-local gptel-model (plist-get policy :model))
        (setq-local gptel-reasoning-effort (plist-get policy :effort))
        (plist-put info :reasoning-effort gptel-reasoning-effort)))))

;;
;;; Unified skill invocation API

(defun mevedel-skills--display-event (display-callback event)
  "Funcall DISPLAY-CALLBACK with EVENT, ignoring errors.
DISPLAY-CALLBACK may be nil; EVENT is a lifecycle event plist
\\=."
  (when display-callback
    (condition-case err
        (funcall display-callback event)
      (error
       (mevedel--warn-once
        'skill-display-callback
        "Skill display-callback error: %s"
        (error-message-string err))))))

(defun mevedel-skills--invoke-error (skill reason message
                                           callback display-callback)
  "Emit SKILL error event, then deliver REASON and MESSAGE to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event error :skill ,skill-name
              :reason ,reason :message ,message))
    (funcall callback
             `(:status error :reason ,reason :message ,message))))

(defun mevedel-skills--run-expansion-hook
    (skill arguments prompt origin session callback)
  "Run `UserPromptExpansion' for a user SKILL expansion.
ARGUMENTS is the raw user argument string.  SESSION supplies workspace
context.  CALLBACK receives (PROMPT DECISION).  Non-user ORIGIN values
skip the hook and call CALLBACK with PROMPT and nil."
  (if (not (eq origin 'user))
      (funcall callback prompt nil)
    (let* ((workspace (and session (mevedel-session-workspace session)))
           (request (and (boundp 'mevedel--current-request)
                         mevedel--current-request)))
      (mevedel-hooks-run-event
       'UserPromptExpansion
       (mevedel-hooks-event-plist
        'UserPromptExpansion session workspace
        :skill-name (mevedel-skill-name skill)
        :arguments arguments
        :prompt prompt)
       (lambda (decision)
         (let* ((updated (plist-get decision :updated-input))
                (prompt (if (stringp updated) updated prompt)))
           (funcall callback prompt decision)))
       session workspace request nil))))

(defun mevedel-skills--invoke-done (skill outcome callback display-callback)
  "Emit SKILL done event, then deliver OUTCOME to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event done :skill ,skill-name))
    (funcall callback outcome)))

(defun mevedel-skills-format-attachment (attachment)
  "Return the model instruction body for prepared skill ATTACHMENT."
  (format "The host already attached the skill `$%s` for this request. You must follow it without calling `Skill`.\n\n%s"
          (plist-get attachment :name)
          (or (plist-get attachment :body) "")))

(defun mevedel-skills-format-model-input (prepared)
  "Return PREPARED's complete dependency attachments and root body."
  (mapconcat
   #'identity
   (append
    (mapcar (lambda (attachment)
              (mevedel-reminders-format-block
               (mevedel-skills-format-attachment attachment)))
            (plist-get prepared :required-attachments))
    (list (or (plist-get prepared :body) "")))
   "\n\n"))

(defun mevedel-skills--prompt-rewrite-audit-record (original decision)
  "Return a `UserPromptExpansion' rewrite audit record, or nil."
  (when-let* ((updated (plist-get decision :updated-input))
              ((stringp updated)))
    (mevedel--hook-prompt-rewrite-audit-record
     'UserPromptExpansion original updated
     (mevedel-hooks-decision-reason decision))))

(cl-defun mevedel-skills-activate-context
    (origin &key permission-rules model effort hook-rules
            (ptc-primitives :unrestricted) invoked-skill)
  "Apply skill-scoped overrides to the active context.

ORIGIN selects the install path:

- `user': append onto the buffer-local pending stash
  (`mevedel-skills--pending-request-context'); drained at request
  begin by the WAIT-state begin handler in `mevedel-presets.el'.
  Used because user skill dispatch fires before the `mevedel-request'
  has been created.
- `model' / `internal': mutate the active sub-agent
  invocation (innermost) or request directly.  Model and effort are never
  installed on an already-realized request.

PERMISSION-RULES is a list of parsed mevedel rules to append.
MODEL is a selector plist or nil.  EFFORT is an opaque gptel value or nil.
HOOK-RULES is a list of normalized hook rules.  PTC-PRIMITIVES narrows the
request's nested ToolScript roster; `:unrestricted' is the identity.
INVOKED-SKILL
is a `mevedel-skill-invocation-record' to record on the session for
compaction/replay."
  (cond
   ((eq origin 'user)
    (let ((existing mevedel-skills--pending-request-context))
      (when permission-rules
        (setq existing
              (plist-put existing :permission-rules
                         (append (plist-get existing :permission-rules)
                                 permission-rules))))
      (when model
        (setq existing (plist-put existing :model model)))
      (when effort
        (setq existing (plist-put existing :effort effort)))
      (when hook-rules
        (setq existing
              (plist-put existing :hook-rules
                         (append (plist-get existing :hook-rules)
                                 hook-rules))))
      (unless (eq ptc-primitives :unrestricted)
        (setq existing
              (plist-put
               existing :ptc-primitives
               (mevedel-skills-intersect-ptc-primitives
                (if (plist-member existing :ptc-primitives)
                    (plist-get existing :ptc-primitives)
                  :unrestricted)
                ptc-primitives))))
      (when invoked-skill
        (setq existing
              (plist-put existing :invoked-skills
                         (append (plist-get existing :invoked-skills)
                                 (list invoked-skill)))))
      (setq-local mevedel-skills--pending-request-context existing)))
   (t
    (let ((req (mevedel-skills--current-request))
          (inv (mevedel-skills--current-invocation)))
      ;; Permission rules accumulate on the innermost slot.
      (when permission-rules
        (cond
         (inv
          (setf (mevedel-agent-invocation-skill-permission-rules inv)
                (append (mevedel-agent-invocation-skill-permission-rules inv)
                        permission-rules)))
         (req
          (mevedel-skills--activate-request-context
           req permission-rules nil))))
      (when hook-rules
        (cond
         (inv
          (setf (mevedel-agent-invocation-hook-rules inv)
                (append (mevedel-agent-invocation-hook-rules inv)
                        hook-rules)))
	 (req
	  (mevedel-skills--activate-request-context
	   req nil hook-rules))))
      (when (and req (not (eq ptc-primitives :unrestricted)))
        (setf (mevedel-request-ptc-primitives req)
              (mevedel-skills-intersect-ptc-primitives
               (mevedel-request-ptc-primitives req)
               ptc-primitives)))
      ;; Record on the session.
      (when invoked-skill
        (when req
          (setf (mevedel-request-attached-skill-records req)
                (append (mevedel-request-attached-skill-records req)
                        (list invoked-skill))))
        (when-let* ((session (and (boundp 'mevedel--session) mevedel--session)))
          (mevedel-skills-commit-invoked-records
           session (list invoked-skill))))))))

(cl-defun mevedel-skills--invoke-inline
    (skill arguments callback &key origin display-callback skip-gates)
  "Prepare and commit inline SKILL with ARGUMENTS from ORIGIN.
CALLBACK receives the normalized outcome.  DISPLAY-CALLBACK receives
lifecycle events from the canonical preparation pipeline."
  (mevedel-skills-prepare
   skill arguments
   (lambda (outcome)
     (when (eq (plist-get outcome :status) 'ok)
       (let ((context (plist-get outcome :request-context)))
         (mevedel-skills-activate-context
          origin
          :permission-rules (plist-get context :permission-rules)
          :model (plist-get context :model)
          :effort (plist-get context :effort)
          :hook-rules (plist-get context :hook-rules)
          :ptc-primitives
          (if (eq origin 'model)
              :unrestricted
            (plist-get context :ptc-primitives)))
         (dolist (record (plist-get context :invoked-skills))
           (mevedel-skills-activate-context
            origin :invoked-skill record))))
     (funcall callback outcome))
   :role 'command
   :origin origin
   :policy-owner-p (not (eq origin 'model))
   :display-callback display-callback
   :skip-gates skip-gates))

(defun mevedel-skills--preparation-rejection (skill role origin)
  "Return a structured preflight rejection for SKILL, ROLE, and ORIGIN."
  (cond
   ((not (mevedel-skill-p skill))
    '(:reason unknown-skill :message "Invalid skill struct"))
   ((not (memq role '(command instruction)))
    (list :reason 'invalid-role
          :message (format "Invalid skill invocation role: %S" role)))
   ((not (memq origin '(user model internal)))
    (list :reason 'invalid-origin
          :message (format "Invalid skill invocation origin: %S" origin)))
   ((and (eq role 'command)
         (eq (mevedel-skill-context skill) 'fork)
         (stringp (mevedel-skill-agent skill))
         (not (string-empty-p (mevedel-skill-agent skill)))
         (null (mevedel-agent-get (mevedel-skill-agent skill))))
    (list :reason 'unknown-agent
          :message
          (format "Skill '%s' references unknown agent '%s'"
                  (mevedel-skill-name skill) (mevedel-skill-agent skill))))))

(defun mevedel-skills--preparation-policy
    (skill origin policy-owner-p)
  "Return structured request policy metadata for SKILL.
ORIGIN identifies the caller.  POLICY-OWNER-P means the command owns a future
request and may resolve and validate model/effort policy.  A model-origin
inline command that does not own policy records only the ignored field names."
  (if policy-owner-p
      (condition-case err
          (let* ((merged
                  (mevedel-model-merge-skill-policy
                   (mevedel-skill-name skill)
                   (mevedel-skill-model skill)
                   (mevedel-skill-effort skill)))
                 (selector (plist-get merged :model))
                 (effort (plist-get merged :effort))
                 (workload
                  (and (eq (mevedel-skill-context skill) 'fork)
                       (mevedel-skill-agent skill))))
            ;; Validate against the request owner's workload now, but retain the
            ;; selector/effort pair so the actual request boundary remains the
            ;; common resolver's source of truth.
            (mevedel-model-resolve-workload workload selector effort)
            (list :status 'ok :model selector :effort effort))
        (error
         (list :status 'error :reason 'invalid-policy
               :message (error-message-string err))))
    (list :status 'ok
          :ignored-fields
          (and (eq origin 'model)
               (eq (mevedel-skill-context skill) 'inline)
               (mevedel-model-skill-policy-fields
                (mevedel-skill-name skill)
                (mevedel-skill-model skill)
                (mevedel-skill-effort skill))))))

(defun mevedel-skills--dependency-skill (session source-key)
  "Return SESSION's skill bound to exact SOURCE-KEY."
  (cl-find source-key (and session (mevedel-session-skills session))
           :key (lambda (skill)
                  (mevedel-skills-source-key
                   (mevedel-skill-source-file skill)))
           :test #'equal))

(cl-defun mevedel-skills--preparation-plan
    (roots origin &key skip-gates)
  "Return a fully validated dependency preparation plan for ROOTS.
ROOTS is a list of plists containing :skill, :arguments, :role, and
:policy-owner-p.  No body injection or hook runs during this function."
  (let* ((roots (mapcar (lambda (root)
                          (append root (list :node nil :policy nil)))
                        roots))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (nodes (make-hash-table :test #'equal))
         (arguments-by-source (make-hash-table :test #'equal))
         (states (make-hash-table :test #'equal))
         order
         root-nodes)
    (when session
      (mevedel-skills-ensure-fresh
       (or (and (buffer-live-p (mevedel-session-root-buffer session))
                (mevedel-session-root-buffer session))
           (current-buffer))
       session))
    (catch 'invalid
      (cl-labels
          ((reject (reason format-string &rest args)
             (throw 'invalid
                    (list :status 'error :reason reason
                          :message (apply #'format format-string args))))
           (bind-root (skill)
             (if-let* ((source (mevedel-skill-source-file skill)))
                 (or (mevedel-skills--dependency-skill
                      session (mevedel-skills-source-key source))
                     (and (null (and session
                                     (mevedel-session-skills session)))
                          skill)
                     (reject 'dependency-source
                             "Skill '%s' is no longer bound to %s"
                             (mevedel-skill-name skill) source))
               skill))
           (validate (skill root-p)
             (unless (mevedel-skill-p skill)
               (reject 'unknown-skill "Invalid skill struct"))
             (when-let* ((diagnostics
                          (mevedel-skill-dependency-diagnostics skill)))
               (reject 'dependency-invalid "%s"
                       (mapconcat #'identity diagnostics "; ")))
             (unless (or skip-gates
                         (mevedel-skills-skill-enabled-p skill))
               (reject 'disabled "Skill '%s' is disabled"
                       (mevedel-skill-name skill)))
             (unless skip-gates
               (cond
                ((and root-p
                      (eq origin 'user)
                      (not (mevedel-skill-user-invocable-p skill)))
                 (reject 'disabled "Skill '%s' is not user-invocable"
                         (mevedel-skill-name skill)))
                ((and (eq origin 'model)
                      (not (mevedel-skill-model-invocable-p skill)))
                 (reject 'disabled "Skill '%s' is not model-invocable"
                         (mevedel-skill-name skill)))))
             (when (and root-p
                        (not (memq origin '(user model internal))))
               (reject 'invalid-origin "Invalid skill invocation origin: %S"
                       origin)))
           (visit (skill arguments path required-by-source-path depth)
             (validate skill nil)
             (let* ((arguments (or arguments ""))
                    (source-key
                     (or (mevedel-skills-source-key
                          (mevedel-skill-source-file skill))
                         skill))
                    (prior-arguments
                     (gethash source-key arguments-by-source 'missing))
                    (key (cons source-key arguments)))
               (when (and (not (eq prior-arguments 'missing))
                          (not (equal prior-arguments arguments)))
                 (reject 'dependency-conflict
                         "Skill '%s' is required with conflicting arguments"
                         (mevedel-skill-name skill)))
               (puthash source-key arguments arguments-by-source)
               (pcase (gethash key states)
                 ('visiting
                  (reject 'dependency-cycle
                          "Required skill dependency cycle: %s"
                          (mapconcat #'identity
                                     (append path
                                             (list (mevedel-skill-name skill)))
                                     " -> ")))
                 ('done (gethash key nodes))
                 (_
                  (puthash key 'visiting states)
                  (when-let* ((file (mevedel-skill-source-file skill))
                              ((not (file-readable-p file))))
                    (reject 'dependency-source
                            "Skill '%s' source is not readable: %s"
                            (mevedel-skill-name skill) file))
                  (let* ((body
                          (progn
                            (when (mevedel-skill-source-file skill)
                              (setf (mevedel-skill-body skill) nil))
                            (mevedel-skill-load-body skill)))
                         (substituted
                          (and body
                               (condition-case err
                                   (mevedel-skills-preparation-substitute
                                    (copy-sequence body) arguments session skill)
                                 (error
                                  (reject 'resource-target "%s"
                                          (error-message-string err))))))
                         (parsed
                          (and substituted
                               (mevedel-skills-syntax-parse-dependencies
                                substituted)))
                         (runtime-dependencies
                          (plist-get parsed :dependencies))
                         (bindings (mevedel-skill-dependencies skill))
                         (node (list :skill skill :arguments arguments
                                     :body (plist-get parsed :body)
                                     :required-by-source-path
                                     required-by-source-path
                                     :dependency-depth depth
                                     :dependencies nil :root nil :outcome nil))
                         children)
                    (unless body
                      (reject 'load-failure
                              "Skill %s could not be loaded: %s"
                              (mevedel-skill-name skill)
                              (or (mevedel-skill-source-file skill)
                                  "unknown source")))
                    (unless (= (length runtime-dependencies)
                               (length bindings))
                      (reject 'dependency-source
                              "Skill '%s' dependency bindings are stale"
                              (mevedel-skill-name skill)))
                    (puthash key node nodes)
                    (cl-mapc
                     (lambda (dependency binding)
                       (unless (equal (plist-get dependency :name)
                                      (mevedel-skill-dependency-name binding))
                         (reject 'dependency-source
                                 "Skill '%s' dependency bindings are stale"
                                 (mevedel-skill-name skill)))
                       (let* ((child-source
                               (mevedel-skill-dependency-source-key binding))
                              (child
                               (mevedel-skills--dependency-skill
                                session child-source)))
                         (unless child
                           (reject 'dependency-source
                                   "Required skill '%s' is no longer bound"
                                   (mevedel-skill-dependency-name binding)))
                         (push
                          (visit child
                                 (or (plist-get dependency
                                                :argument-template)
                                     "")
                                 (append path
                                         (list (mevedel-skill-name skill)))
                                 (mevedel-skill-source-file skill)
                                 (1+ depth))
                          children)))
                     runtime-dependencies bindings)
                    (setf (plist-get node :dependencies) (nreverse children))
                    (puthash key 'done states)
                    (push node order)
                    node))))))
        (dolist (root roots)
          (let* ((skill (bind-root (plist-get root :skill)))
                 (role (plist-get root :role))
                 (rejection
                  (mevedel-skills--preparation-rejection skill role origin)))
            (when rejection
              (reject (plist-get rejection :reason) "%s"
                      (plist-get rejection :message)))
            (validate skill t)
            (let ((policy
                   (mevedel-skills--preparation-policy
                    skill origin
                    (and (eq role 'command)
                         (plist-get root :policy-owner-p)))))
              (unless (eq (plist-get policy :status) 'ok)
                (throw 'invalid policy))
              (let ((node (visit skill
                                 (if (eq role 'instruction)
                                     ""
                                   (or (plist-get root :arguments) ""))
                                 nil nil 0)))
                (setf (plist-get root :skill) skill
                      (plist-get root :node) node
                      (plist-get root :policy) policy)
                (when (or (eq role 'command)
                          (null (plist-get node :root)))
                  (setf (plist-get node :root) root))
                (push node root-nodes)))))
        (list :status 'ok :roots roots :root-nodes (nreverse root-nodes)
              :nodes (nreverse order))))))

(defun mevedel-skills--preparation-settler
    (session rules hooks callback)
  "Install a temporary preparation request and return its settlement closure.
The returned function restores the previous request and calls CALLBACK with
its outcome exactly once."
  (let ((origin-buffer (current-buffer))
        (origin (mevedel-current-origin))
        (previous-request (and (boundp 'mevedel--current-request)
                               mevedel--current-request))
        (invocation-local-p
         (local-variable-p 'mevedel--agent-invocation))
        (previous-invocation
         (and (boundp 'mevedel--agent-invocation)
              mevedel--agent-invocation))
        settled)
    (setq-local mevedel--current-request
                (mevedel-request--create
                 :session session
                 :origin origin
                 :file-snapshots (make-hash-table :test #'equal)
                 :skill-permission-rules rules
                 :hook-rules hooks))
    (setq-local mevedel--agent-invocation nil)
    (lambda (outcome)
      (unless settled
        (setq settled t)
        (when (buffer-live-p origin-buffer)
          (with-current-buffer origin-buffer
            (setq-local mevedel--current-request previous-request)
            (if invocation-local-p
                (setq-local mevedel--agent-invocation previous-invocation)
              (kill-local-variable 'mevedel--agent-invocation))))
        (funcall callback outcome)))))

(defun mevedel-skills--preparation-success-outcome
    (metadata original expanded decision)
  "Build the successful preparation outcome from METADATA.
ORIGINAL and EXPANDED are the pre-hook and post-hook bodies.  DECISION is the
sanitized `UserPromptExpansion' hook decision."
  (let* ((skill (plist-get metadata :skill))
         (arguments (plist-get metadata :arguments))
         (role (plist-get metadata :role))
         (origin (plist-get metadata :origin))
         (session (plist-get metadata :session))
         (command-p (eq role 'command))
         (record
          (mevedel-skill-invocation-record--create
           :name (mevedel-skill-name skill)
           :args arguments
           :role role
           :origin origin
           :agent-path
           (and session
                (if (fboundp 'mevedel-agent-control-current-path)
                    (mevedel-agent-control-current-path session)
                  "/root"))
           :turn (and session (mevedel-session-turn-count session))
           :source-path (mevedel-skill-source-file skill)
           :required-by-source-path
           (plist-get metadata :required-by-source-path)
           :dependency-depth (plist-get metadata :dependency-depth)
           :prepared-body expanded))
         (context
          (if command-p
              (list :permission-rules (plist-get metadata :rules)
                    :model (plist-get metadata :model)
                    :effort (plist-get metadata :effort)
                    :hook-rules (plist-get metadata :hooks)
                    :ptc-primitives (mevedel-skill-ptc-primitives skill)
                    :invoked-skills (list record))
            (list :invoked-skills (list record))))
         (kind (cond
                ((eq role 'instruction) 'instruction)
                ((eq (mevedel-skill-context skill) 'fork) 'fork)
                (t 'inline)))
         (audit (mevedel-skills--prompt-rewrite-audit-record original decision))
         (hook-context
          (mevedel-hooks-additional-context-string
           decision 'UserPromptExpansion)))
    (list :status 'ok :kind kind :skill skill
          :body expanded :arguments arguments
          :hook-context hook-context
          :hook-audits (and audit (list audit))
          :ignored-policy-fields (plist-get metadata :ignored-policy-fields)
          :request-context context)))

(defun mevedel-skills--prepare-single
    (node origin callback)
  "Prepare preflighted NODE from ORIGIN, then call CALLBACK."
  (let* ((root (plist-get node :root))
         (skill (plist-get node :skill))
         (skill-name (mevedel-skill-name skill))
         (arguments (plist-get node :arguments))
         (body (plist-get node :body))
         (role (or (plist-get root :role) 'instruction))
         (policy (or (plist-get root :policy) '(:status ok)))
         (display-callback (plist-get root :display-callback))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (command-p (eq role 'command))
         (rules (mevedel-skill-allowed-tool-rules skill))
         (hooks (and command-p (mevedel-skill-hooks skill)))
         (metadata
          (list :skill skill :arguments arguments :role role
                :origin origin :session session :rules rules
                :hooks hooks
                :required-by-source-path
                (plist-get node :required-by-source-path)
                :dependency-depth (plist-get node :dependency-depth)
                :model (plist-get policy :model)
                :effort (plist-get policy :effort)
                :ignored-policy-fields (plist-get policy :ignored-fields)))
         (finish (mevedel-skills--preparation-settler
                  session rules hooks callback)))
    (cl-labels
        ((fail (reason message)
           (mevedel-skills--display-event
            display-callback
            `(:event error :skill ,skill-name :reason ,reason :message ,message))
           (funcall finish
                    (list :status 'error :reason reason :message message)))
         (complete (original expanded decision)
           (setq decision
                 (mevedel-hooks-sanitize-final-decision
                  'UserPromptExpansion decision))
           (if (and (plist-member decision :continue)
                    (not (plist-get decision :continue)))
               (fail 'hook-blocked
                     (or (plist-get decision :stop-reason)
                         "UserPromptExpansion hook stopped skill"))
             (mevedel-skills--display-event
              display-callback `(:event done :skill ,skill-name))
             (funcall finish
                      (mevedel-skills--preparation-success-outcome
                       metadata original expanded decision)))))
      (mevedel-skills-preparation-expand-body
       body
       (lambda (injection-outcome)
         (if (eq (plist-get injection-outcome :status) 'ok)
             (mevedel-skills--run-expansion-hook
              skill arguments (plist-get injection-outcome :body)
              origin session
              (lambda (expanded decision)
                (complete (plist-get injection-outcome :body)
                          expanded decision)))
           (fail (plist-get injection-outcome :reason)
                 (plist-get injection-outcome :message))))
       skill session))))

(defun mevedel-skills--node-reachable-p (candidate root)
  "Return non-nil when CANDIDATE is ROOT or one of its dependencies."
  (or (eq candidate root)
      (cl-some (lambda (child)
                 (mevedel-skills--node-reachable-p candidate child))
               (plist-get root :dependencies))))

(cl-defun mevedel-skills-prepare-many
    (roots callback &key origin skip-gates cancelled-p)
  "Preflight and sequentially prepare ROOTS as one dependency graph.
Each ROOT is a plist with :skill, :arguments, :role, :policy-owner-p, and an
optional :display-callback.  CALLBACK receives one atomic aggregate outcome."
  (let ((plan (mevedel-skills--preparation-plan
               roots origin :skip-gates skip-gates)))
    (if (not (eq (plist-get plan :status) 'ok))
        (progn
          (when-let* ((root (car roots)))
            (mevedel-skills--display-event
             (plist-get root :display-callback)
             `(:event error
                      :skill ,(and (mevedel-skill-p (plist-get root :skill))
                                   (mevedel-skill-name
                                    (plist-get root :skill)))
                      :reason ,(plist-get plan :reason)
                      :message ,(plist-get plan :message))))
          (funcall callback plan))
      (let ((remaining (copy-sequence (plist-get plan :nodes)))
            prepared
            settled)
        (cl-labels
            ((finish (outcome)
               (unless settled
                 (setq settled t)
                 (funcall callback outcome)))
             (next ()
               (if (and cancelled-p (funcall cancelled-p))
                   (finish '(:status error :reason cancelled
                                      :message "Skill preparation was cancelled"))
                 (if-let* ((node (pop remaining)))
                   (mevedel-skills--prepare-single
                    node origin
                    (lambda (outcome)
                      (if (and cancelled-p (funcall cancelled-p))
                          (finish '(:status error :reason cancelled
                                             :message "Skill preparation was cancelled"))
                        (if (not (eq (plist-get outcome :status) 'ok))
                          (finish
                           (plist-put
                            (copy-sequence outcome) :name
                            (mevedel-skill-name (plist-get node :skill))))
                          (setf (plist-get node :outcome) outcome)
                          (push outcome prepared)
                          (next)))))
                   (let* ((prepared (nreverse prepared))
                        (records
                         (mapcar
                          (lambda (outcome)
                            (car (plist-get
                                  (plist-get outcome :request-context)
                                  :invoked-skills)))
                          prepared))
                        (hook-contexts
                         (delq nil
                               (mapcar (lambda (outcome)
                                         (plist-get outcome :hook-context))
                                       prepared)))
                        (hook-audits
                         (mapcan (lambda (outcome)
                                   (copy-sequence
                                    (plist-get outcome :hook-audits)))
                                 prepared))
                        outcomes)
                   (dolist (root (plist-get plan :roots))
                     (let* ((root-node (plist-get root :node))
                            (outcome (copy-tree (plist-get root-node :outcome)))
                            (closure
                             (cl-remove-if-not
                              (lambda (node)
                                (mevedel-skills--node-reachable-p
                                 node root-node))
                              (plist-get plan :nodes)))
                            (root-records
                             (mapcar
                              (lambda (node)
                                (car
                                 (plist-get
                                  (plist-get
                                   (plist-get node :outcome) :request-context)
                                  :invoked-skills)))
                              closure))
                            (attachments
                             (mapcar
                              (lambda (node)
                                (let* ((outcome (plist-get node :outcome))
                                       (skill (plist-get outcome :skill)))
                                  (list
                                   :name (mevedel-skill-name skill)
                                   :body (plist-get outcome :body)
                                   :arguments (plist-get outcome :arguments)
                                   :source-file
                                   (mevedel-skill-source-file skill)
                                   :skill skill)))
                              (cl-remove root-node closure :test #'eq)))
                            (contexts
                             (delq nil
                                   (mapcar
                                    (lambda (node)
                                      (plist-get (plist-get node :outcome)
                                                 :hook-context))
                                    (cl-remove root-node closure :test #'eq))))
                            (audits
                             (mapcan
                              (lambda (node)
                                (copy-sequence
                                 (plist-get (plist-get node :outcome)
                                            :hook-audits)))
                              closure))
                            (context (plist-get outcome :request-context))
                            (root-hook-context
                             (plist-get outcome :hook-context)))
                       (setf (plist-get context :invoked-skills) root-records
                             (plist-get outcome :required-attachments)
                             attachments
                             (plist-get outcome :hook-context)
                             (when-let* ((all-contexts
                                          (append contexts
                                                  (and root-hook-context
                                                       (list root-hook-context)))))
                               (mapconcat #'identity all-contexts "\n\n"))
                             (plist-get outcome :hook-audits)
                             (or audits (plist-get outcome :hook-audits)))
                       (push outcome outcomes)))
                   (finish (list :status 'ok
                                 :outcomes (nreverse outcomes)
                                 :invoked-skills records
                                 :hook-context
                                 (and hook-contexts
                                      (mapconcat #'identity hook-contexts "\n\n"))
                                 :hook-audits hook-audits)))))))
          (next))))))

(cl-defun mevedel-skills-prepare
    (skill arguments callback
           &key role origin policy-owner-p display-callback skip-gates)
  "Prepare SKILL and its dependencies without dispatching policy.
ROLE is `command' or `instruction'.  Required children prepare first and
contribute only instruction context."
  (mevedel-skills-prepare-many
   (list (list :skill skill :arguments arguments :role role
               :policy-owner-p policy-owner-p
               :display-callback display-callback))
   (lambda (outcome)
     (funcall callback
              (if (eq (plist-get outcome :status) 'ok)
                  (car (plist-get outcome :outcomes))
                outcome)))
   :origin origin :skip-gates skip-gates))

(defun mevedel-skills--build-parent-inherited-agent (skill)
  "Build a synthetic `mevedel-agent' for SKILL with no `agent' field.

Captures the calling buffer's current gptel state at spawn time
and returns a `mevedel-agent' struct named `skill:<skill-name>'.
The agent inherits the parent's system prompt directly; tools are
inherited via the request-locals snapshot captured by
`mevedel-agent-exec-run' at dispatch time, which carries the
calling buffer's `gptel-tools' through to the spawned agent
buffer.
"
  (let* ((skill-name (mevedel-skill-name skill))
         (agent-name (concat "skill:" skill-name))
         (parent-system (and (boundp 'gptel-system-prompt)
                             gptel-system-prompt))
         (agent
          (mevedel-agent--create
           :name agent-name
           :description (or (mevedel-skill-description skill)
                            (format "Parent-inherited fork of skill %s"
                                    skill-name))
           :tools nil
           :system-prompt (or parent-system "")
           :max-turns nil
           :reminders nil)))
    agent))

(defun mevedel-skills--build-fork-agent (skill)
  "Return a `mevedel-agent' struct to use for SKILL's fork dispatch.

If SKILL declares an `agent' field, look it up in the registry
and return that agent.  Returns nil for unknown agent names so
the caller can produce an `unknown-agent' outcome.

If SKILL does not declare an `agent' field, build a synthetic
parent-inherited agent via
`mevedel-skills--build-parent-inherited-agent'.  The synthetic
agent's name is `skill:<skill-name>'; system prompt is
snapshotted from the calling buffer's `gptel-system-prompt';
tools propagate through the spawn path's request-locals capture."
  (let ((agent-name (mevedel-skill-agent skill)))
    (cond
     ((and (stringp agent-name) (not (string-empty-p agent-name)))
      (mevedel-agent-get agent-name))
     (t
      (mevedel-skills--build-parent-inherited-agent skill)))))

(defun mevedel-skills--fork-task-name (session skill-name)
  "Return an unused retained-agent task name for SKILL-NAME in SESSION."
  (let* ((base
          (concat
           "skill_"
           (replace-regexp-in-string
            "[^a-z0-9_]+" "_" (downcase skill-name))))
         (parent (mevedel-agent-control-current-path session))
         (candidate base)
         (suffix 2))
    (while (assoc (concat parent "/" candidate)
                  (mevedel-session-agent-registry session))
      (setq candidate (format "%s_%d" base suffix)
            suffix (1+ suffix)))
    candidate))

(defun mevedel-skills--fork-result-render-data (session envelope skill description)
  "Return transcript render data for retained result ENVELOPE in SESSION."
  (let* ((path (plist-get envelope :sender))
         (record (cdr (assoc path (mevedel-session-agent-registry session)))))
    (when record
      (list :kind 'collaboration-event
            :event 'started
            :path path
            :agent-id (mevedel-agent-record-id record)
            :role (mevedel-agent-record-role record)
            :name (mevedel-skill-name skill)
            :description description
            :status 'completed
            :calls 0
            :body ""
            :transcript-relative-path
            (mevedel-agent-record-conversation-location record)))))

(defun mevedel-skills--handle-fork-result
    (session skill prepared hook-audits description callback display-callback
             envelope)
  "Deliver retained fork ENVELOPE through the ordinary skill callbacks."
  (let ((outcome (plist-get envelope :outcome))
        (payload (plist-get envelope :payload))
        (path (plist-get envelope :sender)))
    (if (eq outcome 'completed)
        (mevedel-skills--invoke-done
         skill
         (list :status 'ok
               :kind 'fork
               :result payload
               :agent-path path
               :hook-audits (or hook-audits
                                (plist-get prepared :hook-audits))
               :render-data
               (mevedel-skills--fork-result-render-data
                session envelope skill description))
         callback display-callback)
      (mevedel-skills--invoke-error
       skill
       (intern (format "agent-%s" outcome))
       (if (stringp payload) payload (format "%S" payload))
       callback display-callback))))

(cl-defun mevedel-skills-dispatch-prepared-fork
    (prepared callback &key prompt request-context hook-audits
              description on-invocation display-callback)
  "Dispatch an already PREPARED fork command and call CALLBACK.
PROMPT is the complete post-plan, post-submit-hook child prompt and
defaults to PREPARED's body.  REQUEST-CONTEXT is the aggregate plan
context and defaults to PREPARED's context.  HOOK-AUDITS are attached
to the eventual fork outcome.  DESCRIPTION, ON-INVOCATION, and
DISPLAY-CALLBACK retain the normal fork dispatch meanings."
  (let* ((skill (plist-get prepared :skill))
         (skill-name (and skill (mevedel-skill-name skill)))
         (agent (and skill (mevedel-skills--build-fork-agent skill)))
         (context (or request-context
                      (plist-get prepared :request-context)))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (task-description
          (or description
              (and skill (mevedel-skill-description skill))
              skill-name)))
    (cond
     ((not (and (eq (plist-get prepared :status) 'ok)
                (eq (plist-get prepared :kind) 'fork)
                (mevedel-skill-p skill)))
      (mevedel-skills--invoke-error
       skill 'invalid-prepared-fork "Invalid prepared fork outcome"
       callback display-callback))
     ((null agent)
      (mevedel-skills--invoke-error
       skill 'unknown-agent
       (format "Skill '%s' references unknown agent '%s'"
               skill-name (mevedel-skill-agent skill))
       callback display-callback))
     ((null session)
      (mevedel-skills--invoke-error
       skill 'missing-session "Fork skills require an active session"
       callback display-callback))
     (t
      (mevedel-skills-commit-invoked-records
       session (plist-get context :invoked-skills))
      (require 'mevedel-agent-control)
      (condition-case err
          (mevedel-agent-control-spawn
           session
           (mevedel-skills--fork-task-name session skill-name)
           (or prompt (plist-get prepared :body) "")
           (lambda (outcome)
             (unless (eq (plist-get outcome :outcome) 'success)
               (mevedel-skills--invoke-error
                skill 'agent-dispatch-failed
                (or (plist-get outcome :error)
                    "Agent preparation was cancelled")
                callback display-callback)))
           :agent agent
           :context "none"
           :model (plist-get context :model)
           :effort (plist-get context :effort)
           :description task-description
           :skill-permission-rules (plist-get context :permission-rules)
           :skill-hook-rules (plist-get context :hook-rules)
           :on-invocation on-invocation
           :result-handler
           (apply-partially
            #'mevedel-skills--handle-fork-result
            session skill prepared hook-audits task-description
            callback display-callback))
        (error
         (mevedel-skills--invoke-error
          skill 'agent-dispatch-failed (error-message-string err)
          callback display-callback)))))))

(cl-defun mevedel-skills--invoke-fork-direct
    (skill arguments callback &key origin display-callback
           additional-context description on-invocation skip-gates)
  "Prepare and asynchronously dispatch fork SKILL with ARGUMENTS.

ORIGIN identifies the invocation source.  ADDITIONAL-CONTEXT is appended to
the prepared child prompt.  DESCRIPTION, ON-INVOCATION, DISPLAY-CALLBACK, and
CALLBACK retain the public invocation lifecycle semantics."
  (mevedel-skills-prepare
   skill arguments
   (lambda (prepared)
     (if (not (eq (plist-get prepared :status) 'ok))
         (funcall callback prepared)
       (let ((prompt (mevedel-skills-format-model-input prepared)))
         (when (and (stringp additional-context)
                    (not (string-empty-p additional-context)))
           (setq prompt (concat prompt "\n\n" additional-context)))
         (mevedel-skills-dispatch-prepared-fork
          prepared callback
          :prompt prompt
          :description description
          :on-invocation on-invocation
          :display-callback display-callback))))
   :role 'command
   :origin origin
   :policy-owner-p t
   :display-callback display-callback
   :skip-gates skip-gates))

(cl-defun mevedel-skills-invoke
    (skill arguments callback &key origin display-callback
           additional-context description on-invocation skip-gates)
  "Invoke SKILL with ARGUMENTS through the unified skill API.

CALLBACK is invoked with a normalized invocation outcome plist:

  (:status ok    :kind inline :body BODY :request-context CTX)
  (:status ok    :kind fork   :result RESULT :agent-path PATH
                  :render-data DATA)
  (:status error :reason REASON :message MESSAGE)

ORIGIN is `user', `model', or `internal' and determines the blocking
model implicitly: `user' blocks chat input; `model' blocks the parent
tool call.

DISPLAY-CALLBACK is an optional lifecycle event sink that
receives `agent-progress' (fork only), `done', and `error'
events.

ADDITIONAL-CONTEXT is appended to fork-skill agent prompts after body
injections have prepared the prompt.

DESCRIPTION overrides the task description for fork skills.
ON-INVOCATION receives the retained invocation for fork skills.
SKIP-GATES bypasses user-disabled/user-invocable/model-invocable gates
for first-class local commands that own their dispatch semantics.

Inline and fork contexts are callback-driven.  Inline invocation
calls CALLBACK with a prepared body; fork invocation spawns a retained
asynchronous agent and calls CALLBACK when that turn settles."
  (let* ((skill-name (and skill (mevedel-skill-name skill)))
         (session (and (fboundp 'mevedel-telemetry-current-session)
                       (mevedel-telemetry-current-session)))
         (started-at (float-time))
         (original-callback callback)
         (callback
          (lambda (outcome)
            (when (and session (fboundp 'mevedel-telemetry-record))
              (mevedel-telemetry-record
               session 'skill-invocation-settled
               :skill-name skill-name
               :origin origin
               :status (plist-get outcome :status)
               :kind (plist-get outcome :kind)
               :reason (plist-get outcome :reason)
               :duration-ms (round (* 1000.0
                                      (- (float-time) started-at)))))
            (funcall original-callback outcome))))
    (when (and session (fboundp 'mevedel-telemetry-record))
      (mevedel-telemetry-record
       session 'skill-invocation-requested
       :skill-name skill-name
       :origin origin
       :context (and skill (mevedel-skill-context skill))
       :skip-gates (and skip-gates t)))
    (cond
     ((not (mevedel-skill-p skill))
      (mevedel-skills--invoke-error
       skill 'unknown-skill
       "Invalid skill struct"
       callback display-callback))
     ((and (eq origin 'model)
           (when-let* ((request (mevedel-skills--current-request))
                       (source-key
                        (mevedel-skills-source-key
                         (mevedel-skill-source-file skill))))
             (cl-some
              (lambda (record)
                (equal source-key
                       (mevedel-skills-source-key
                        (mevedel-skill-invocation-record-source-path record))))
              (mevedel-request-attached-skill-records request))))
      (mevedel-skills--invoke-done
       skill
       (list :status 'ok
             :kind 'already-attached
             :body (format
                    "Skill '$%s' is already attached for this request."
                    skill-name))
       callback display-callback))
     ;; User-disabled skill gating.
     ((and (not skip-gates)
           (not (mevedel-skills-skill-enabled-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (if (eq origin 'user)
           (format "Skill $%s is disabled. Enable it with /skills enable %s or escape it as \\$%s."
                   skill-name skill-name skill-name)
         (format "Skill '%s' is disabled" skill-name))
       callback display-callback))
     ;; User-slash gating.
     ((and (not skip-gates)
           (eq origin 'user)
           (not (mevedel-skill-user-invocable-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not user-invocable" skill-name)
       callback display-callback))
     ;; Model-side gating.
     ((and (not skip-gates)
           (eq origin 'model)
           (not (mevedel-skill-model-invocable-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not model-invocable" skill-name)
       callback display-callback))
     (t
      (pcase (mevedel-skill-context skill)
        ('inline
         (mevedel-skills--invoke-inline
          skill arguments callback
          :origin origin :display-callback display-callback
          :skip-gates skip-gates))
        ('fork
         (mevedel-skills--invoke-fork-direct
          skill arguments callback
          :origin origin :display-callback display-callback
          :additional-context additional-context
          :description description
          :on-invocation on-invocation
          :skip-gates skip-gates))
        (other
         (mevedel-skills--invoke-error
          skill 'unknown-skill
          (format "Skill '%s' has unsupported context: %S"
                  skill-name other)
          callback display-callback)))))))


;;
;;; Skill tool handler

(defun mevedel-skills--render-skill-tool (name args result render-data)
  "Return rendering plist for NAME, ARGS, and RESULT from the Skill tool."
  (when (stringp result)
    (let* ((skill-name (or (plist-get args :name) "?"))
           (lines (length (split-string result "\n" t)))
           (fields (plist-get render-data :ignored-policy-fields))
           (ignored-fields
            (and (eq (plist-get render-data :kind) 'skill-policy-warning)
                 (member fields '((model) (effort) (model effort)))
                 fields))
           (ignored-names (mapconcat #'symbol-name ignored-fields ", "))
           (ignored-description (mapconcat #'symbol-name ignored-fields " and ")))
      (list :header (format "%s: %s (%d %s%s)"
                            (or name "Skill")
                            skill-name
                            lines
                            (if (= lines 1) "line" "lines")
                            (if ignored-fields
                                (format "; ignored %s" ignored-names)
                              ""))
            :body (if ignored-fields
                      (format
                       (concat
                        "Warning: The skill's %s %s ignored because a "
                        "model-side inline invocation cannot change its "
                        "already-realized parent request. Use `context: fork` "
                        "to give the skill its own request.\n\n%s")
                       ignored-description
                       (if (cdr ignored-fields) "overrides were" "override was")
                       result)
                    result)
            :body-mode 'markdown-mode
            :status (cond
                     ((string-prefix-p "Error:" result) 'error)
                     (ignored-fields 'warning))
            :initially-collapsed-p t))))

(defun mevedel-skills--invoke-handler (callback args)
  "Pipeline handler for the `Skill' tool.

CALLBACK is the async tool callback.  ARGS is a plist with :name
and optional :arguments.

Routes through `mevedel-skills-invoke' with model origin
and projects the outcome plist to a tool-result string: success
returns the body; error returns a `Error: ' prefixed message."
  (let* ((name (plist-get args :name))
         (arguments (plist-get args :arguments))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (skill (and session (mevedel-session-get-skill session name)))
         (return (lambda (result &optional ignored-fields)
                   (funcall callback
                            (if ignored-fields
                                (list :result result
                                      :render-data
                                      (list :kind 'skill-policy-warning
                                            :ignored-policy-fields
                                            ignored-fields))
                              (list :result result))))))
    (cond
     ((not (stringp name))
      (funcall return "Error: Skill name is required."))
     ((not session)
      (funcall return "Error: No active mevedel session."))
     ((not skill)
      (funcall return (format "Error: Unknown skill '%s'." name)))
     (t
      (mevedel-skills-invoke
       skill arguments
       (lambda (outcome)
         (pcase (plist-get outcome :status)
           ('ok
            (funcall return
                     (or (and (plist-get outcome :body)
                              (mevedel-skills-format-model-input outcome))
                         (plist-get outcome :result)
                         (format "Skill '%s' produced no body." name))
                     (plist-get outcome :ignored-policy-fields)))
           ('error
            (funcall return
                     (format "Error: %s"
                             (or (plist-get outcome :message)
                                 "skill invocation failed"))))))
       :origin 'model)))))

(defconst mevedel-skills--list-tool-limit 25
  "Maximum entries returned by the ListSkills tool without narrowing.")

(defun mevedel-skills--model-visible-p (skill &optional active-only)
  "Return non-nil when SKILL may be shown to the model.
When ACTIVE-ONLY is non-nil, dormant path-scoped skills are excluded."
  (and (mevedel-skill-model-invocable-p skill)
       (mevedel-skill-effective-model-invocable-p skill)
       (mevedel-skills-skill-enabled-p skill)
       (or (not active-only)
           (mevedel-skill-active-p skill))))

(defun mevedel-skills--skill-matches-query-p (skill query)
  "Return non-nil when SKILL matches QUERY."
  (or (not (and (stringp query) (not (string-empty-p query))))
      (let ((case-fold-search t)
            (needle (regexp-quote query)))
        (cl-some
         (lambda (value)
           (and (stringp value) (string-match-p needle value)))
         (list (mevedel-skill-name skill)
               (mevedel-skill-display-name skill)
               (mevedel-skill-description skill))))))

(defun mevedel-skills--format-list-tool-result
    (skills omitted &optional mark-dormant)
  "Format SKILLS for the ListSkills tool, noting OMITTED entries."
  (if (null skills)
      "No model-invocable skills match."
    (let ((body
           (mapconcat (lambda (skill)
                        (mevedel-skills--listing-describe
                         skill
                         (and mark-dormant
                              (mevedel-skills--model-visible-p skill)
                              (mevedel-skill-path-patterns skill)
                              (not (mevedel-skill-active-p skill)))))
                      skills "\n")))
      (if (> omitted 0)
          (concat body
                  (format "\n\n%d more skill(s) omitted; use query to narrow."
                          omitted))
        body))))

(defun mevedel-skills--list-handler (callback args)
  "Pipeline handler for the `ListSkills' tool.
CALLBACK is the async tool callback.  ARGS is a plist with optional :query."
  (let* ((query (plist-get args :query))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (return (lambda (result)
                   (funcall callback (list :result result)))))
    (cond
     ((not session)
      (funcall return "Error: No active mevedel session."))
     ((and query (not (stringp query)))
      (funcall return "Error: query must be a string."))
     (t
      (when (buffer-live-p (current-buffer))
        (mevedel-skills-ensure-fresh (current-buffer) session))
      (let* ((narrowed (and (stringp query)
                            (not (string-empty-p (string-trim query)))))
             (pool (if narrowed
                       (cl-remove-if-not
                        #'mevedel-skills--model-visible-p
                        (mevedel-session-skills session))
                     (mevedel-skills--listing-candidates session)))
             (matches
              (cl-remove-if-not
               (lambda (skill)
                 (mevedel-skills--skill-matches-query-p skill query))
               pool))
             (shown (cl-subseq matches 0
                                (min (length matches)
                                     mevedel-skills--list-tool-limit)))
             (omitted (max 0 (- (length matches) (length shown)))))
        (funcall return
                 (mevedel-skills--format-list-tool-result
                  shown omitted narrowed)))))))


;;
;;; Shared model listing primitives

(defcustom mevedel-skills-listing-max-entry-chars 1536
  "Maximum characters per skill entry in the model-facing skills roster.

Entries longer than this are truncated with an ellipsis so a single
verbose description cannot starve the rest of the listing.  The
default cap is 1,536 chars."
  :type 'integer
  :group 'mevedel)

(defconst mevedel-skills--source-priority
  '((project . mevedel)
    (project . agents)
    (user . mevedel)
    (user . agents)
    bundled
    managed
    plugin)
  "Source priority for model-facing skills roster ordering.")

(defun mevedel-skills--source-priority-key (skill)
  "Return ordering key for SKILL in model-facing rosters."
  (or (cl-position
       (cond
        ((and (mevedel-skills--ordinary-skill-p skill)
              (mevedel-skill-source-family skill))
         (cons (mevedel-skill-source skill)
               (mevedel-skill-source-family skill)))
        ((eq (mevedel-skill-source skill) 'project)
         '(project . mevedel))
        ((eq (mevedel-skill-source skill) 'user)
         '(user . mevedel))
        (t
         (mevedel-skill-source skill)))
       mevedel-skills--source-priority
       :test #'equal)
      most-positive-fixnum))

(defun mevedel-skills--truncate-text (text limit)
  "Return TEXT truncated to LIMIT characters, using `...' when possible."
  (let ((text (or text "")))
    (cond
     ((<= limit 0) "")
     ((<= (length text) limit) text)
     ((<= limit 3) (substring text 0 limit))
     (t (concat (substring text 0 (- limit 3)) "...")))))

(defun mevedel-skills--entry-base (skill &optional dormant)
  "Return the roster line prefix for SKILL.
When DORMANT is non-nil, mark the skill as dormant path-scoped."
  (format "- %s%s:"
          (mevedel-skill-name skill)
          (if dormant " [dormant path-scoped]" "")))

(defun mevedel-skills--entry-description (skill &optional dormant)
  "Return SKILL's description capped for a single roster entry."
  (let* ((base (mevedel-skills--entry-base skill dormant))
         (limit (max 0 (- mevedel-skills-listing-max-entry-chars
                          (length base)
                          1))))
    (mevedel-skills--truncate-text
     (or (mevedel-skill-description skill) "")
     limit)))

(defun mevedel-skills--listing-describe (skill &optional dormant)
  "Return a one-line entry for SKILL.

Format:
  - name: description

`mevedel-skills-listing-max-entry-chars' (1,536 by default) caps entries by
truncation with an ellipsis so a single verbose skill cannot starve
the rest of the listing.  When DORMANT is non-nil, mark the skill as
dormant path-scoped for `ListSkills(query)' output."
  (concat (mevedel-skills--entry-base skill dormant)
          " "
          (mevedel-skills--entry-description skill dormant)))

(defun mevedel-skills--listing-candidates (session)
  "Return SESSION's model-invocable, currently active skills.

Sorted by configured resource precedence so budget pressure drops
global/bundled/plugin entries before local resource entries."
  (let ((candidates
         (cl-remove-if-not
          (lambda (s)
            (mevedel-skills--model-visible-p s t))
          (mevedel-session-skills session))))
    (cl-sort (copy-sequence candidates)
             (lambda (a b)
               (< (mevedel-skills--source-priority-key a)
                  (mevedel-skills--source-priority-key b))))))


(provide 'mevedel-skills-invoke)
;;; mevedel-skills-invoke.el ends here
