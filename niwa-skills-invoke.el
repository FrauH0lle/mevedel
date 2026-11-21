;;; niwa-skills-invoke.el -- Skill invocation and dispatch -*- lexical-binding: t -*-

;;; Commentary:

;; Owns request-scoped skill context, preparation policy, invocation records,
;; fork dispatch, and model-facing skill handlers.  User input handling lives
;; in `niwa-skills-input'; body expansion lives in
;; `niwa-skills-preparation'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'niwa-agents))

(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-tool-registry)
(require 'niwa-models)
(require 'niwa-implementation-policy)
(require 'niwa-skills-core)
(require 'niwa-turn)
(require 'niwa-utilities)

;; `gptel'
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-system-prompt)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(autoload 'gptel-fsm-info "gptel-request")

;; `niwa-agent-control'
(declare-function niwa-agent-control-current-path
                  "niwa-agent-control" (session))
(declare-function niwa-agent-control-spawn
                  "niwa-agent-control" t t)
(declare-function niwa-agent-record-conversation-location
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-id
                  "niwa-agent-control" (cl-x) t)
(declare-function niwa-agent-record-role
                  "niwa-agent-control" (cl-x) t)
(autoload 'niwa-agent-control-current-path "niwa-agent-control")

;; `niwa-agent-conversation'
(defvar niwa--agent-invocation)

;; `niwa-agents'
(declare-function niwa-agent--create "niwa-agents" (&rest args))
(declare-function niwa-agent-get "niwa-agents" (name))
(declare-function niwa-agent-invocation-hook-rules
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-skill-permission-rules
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-name "niwa-agents" (cl-x) t)
(declare-function niwa-plan-read-only-p "niwa-agents"
                  (&optional session))
(autoload 'niwa-agent--create "niwa-agents")
(autoload 'niwa-agent-get "niwa-agents")
(autoload 'niwa-agent-invocation-hook-rules "niwa-agents")
(autoload 'niwa-agent-invocation-skill-permission-rules "niwa-agents")
(autoload 'niwa-agent-name "niwa-agents")
(autoload 'niwa-plan-read-only-p "niwa-agents")

;; `niwa-hooks'
(declare-function niwa-hooks-additional-context-string
                  "niwa-hooks" (decision &optional event))
(declare-function niwa-hooks-decision-reason
                  "niwa-hooks" (decision))
(declare-function niwa-hooks-event-plist
                  "niwa-hooks"
                  (event &optional session workspace &rest extra))
(declare-function niwa-hooks-run-event
                  "niwa-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function niwa-hooks-sanitize-final-decision
                  "niwa-hooks" (event decision &optional scope))
(autoload 'niwa-hooks-additional-context-string "niwa-hooks")
(autoload 'niwa-hooks-decision-reason "niwa-hooks")
(autoload 'niwa-hooks-event-plist "niwa-hooks")
(autoload 'niwa-hooks-run-event "niwa-hooks")
(autoload 'niwa-hooks-sanitize-final-decision "niwa-hooks")

;; `niwa-models'
(declare-function niwa-model-merge-skill-policy
                  "niwa-models" (skill-name model effort))
(declare-function niwa-model-resolve-workload
                  "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function niwa-model-skill-policy-fields
                  "niwa-models" (skill-name model effort))

;; `niwa-reminders'
(declare-function niwa-reminders-format-block
                  "niwa-reminders" (content))
(autoload 'niwa-reminders-format-block "niwa-reminders")

;; `niwa-resource'
(declare-function niwa-resource--skill-address
                  "niwa-resource" (skill &optional components))
(autoload 'niwa-resource--skill-address "niwa-resource")

;; `niwa-skills-preparation'
(declare-function niwa-skills-preparation-expand-body
                  "niwa-skills-preparation"
                  (text callback &optional skill session live-p))
(declare-function niwa-skills-preparation-substitute
                  "niwa-skills-preparation"
                  (text arguments session skill))
(autoload 'niwa-skills-preparation-expand-body
  "niwa-skills-preparation")
(autoload 'niwa-skills-preparation-substitute
  "niwa-skills-preparation")

;; `niwa-skills-syntax'
(declare-function niwa-skills-syntax-parse-dependencies
                  "niwa-skills-syntax" (text))
(autoload 'niwa-skills-syntax-parse-dependencies "niwa-skills-syntax")

;; `niwa-structs'
(declare-function niwa-request-attached-skill-records
                  "niwa-structs" (cl-x) t)
(declare-function niwa-request-hook-rules
                  "niwa-structs" (cl-x) t)
(declare-function niwa-request-ptc-primitives
                  "niwa-structs" (cl-x) t)
(declare-function niwa-request-skill-permission-rules
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-agent-registry
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-invoked-skills
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-plan-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-telemetry'
(declare-function niwa-telemetry-current-session
                  "niwa-telemetry" (&optional buffer))
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-tool-registry'
(declare-function niwa-tool-get "niwa-tool-registry"
                  (name &optional category))

;; `niwa-transcript-audit'
(declare-function niwa--hook-prompt-rewrite-audit-record
                  "niwa-transcript-audit"
                  (event original submitted &optional reason))
(autoload 'niwa--hook-prompt-rewrite-audit-record
  "niwa-transcript-audit")

;; `niwa-turn'
(declare-function niwa-current-origin "niwa-turn" ())
(declare-function niwa-request-begin "niwa-turn"
                  (session &optional options))

;;
;;; Request-scoped skill context

(defun niwa-skills-commit-invoked-records (session records)
  "Append skill invocation RECORDS to SESSION for compaction and replay."
  (when (and session records)
    (setf (niwa-session-invoked-skills session)
          (append (niwa-session-invoked-skills session) records))))

(defvar-local niwa-skills--pending-request-context nil
  "Buffer-local pending request context for the next niwa-request.

A plist of the form
  (:permission-rules RULES :model MODEL :effort EFFORT
   :hook-rules HOOKS :ptc-primitives NAMES :invoked-skills SKILLS)

populated by user-dispatched skill invocation before `gptel-send'
fires.  The prompt transform consumes MODEL and EFFORT before request
realization.  The WAIT-state begin handler in `niwa-presets.el' drains
permission and hook rules into the new request and records invoked skills on
the session (see `niwa-skills--drain-pending-context').

Cleared on drain.  Cleared by an `unwind-protect' in the slash
or skill dispatch path if `gptel-send' aborts before request creation.")

(put 'niwa-skills--pending-request-context 'permanent-local t)

(defun niwa-skills-clear-pending-context ()
  "Clear pending skill request context in the current buffer."
  (setq-local niwa-skills--pending-request-context nil))


(defun niwa-skills--current-invocation ()
  "Return the active sub-agent invocation, or nil.
Reads the buffer-local `niwa--agent-invocation' set by
`niwa-agent-conversation-open' on agent buffers;
returns nil when called outside any sub-agent."
  niwa--agent-invocation)

(defun niwa-skills--current-request ()
  "Return the active request struct, or nil."
  (and (boundp 'niwa--current-request)
       niwa--current-request))

(defun niwa-skills--drain-pending-context (request)
  "Drain `niwa-skills--pending-request-context' (buffer-local) into REQUEST.

After this call the buffer-local stash is nil.  No-op when no stash
is present.

The stash plist keys map onto request/session state:

- :permission-rules -> `niwa-request-skill-permission-rules'
- :model/:effort    -> resolved before engine dispatch and request realization
- :hook-rules       -> `niwa-request-hook-rules'
- :ptc-primitives   -> `niwa-request-ptc-primitives'
- :invoked-skills   -> user-origin records identify skill bodies already
                       attached to this request, and all records are appended
                       to `niwa-session-invoked-skills'"
  (when-let* ((ctx niwa-skills--pending-request-context))
    (when-let* ((rules (plist-get ctx :permission-rules)))
      (setf (niwa-request-skill-permission-rules request) rules))
    (when-let* ((hooks (plist-get ctx :hook-rules)))
      (setf (niwa-request-hook-rules request) hooks))
    (when (plist-member ctx :ptc-primitives)
      (setf (niwa-request-ptc-primitives request)
            (plist-get ctx :ptc-primitives)))
    (when-let* ((skills (plist-get ctx :invoked-skills))
                (session niwa--session))
      (setf (niwa-request-attached-skill-records request)
            (cl-remove-if-not
             (lambda (record)
               (and (niwa-skill-invocation-record-p record)
                    (eq (niwa-skill-invocation-record-origin record)
                        'user)))
             skills))
      (niwa-skills-commit-invoked-records session skills))
    (setq-local niwa-skills--pending-request-context nil)))

(defun niwa-skills-request-model-policy ()
  "Resolve the model policy the current buffer's pending request will use.
Root Plan requests use the `planning' workload; a leading user skill
may override model or effort through the pending request context.
gptel funcalls the function-valued system prompt before the prompt
transforms apply this policy to the temp buffer, so any request-time
consumer of the effective model -- the roster budget included -- must
resolve through this one seam rather than read the buffer's
`gptel-model' directly.  Once admitted, use the captured effective policy
even after the pending skill context has been consumed."
  (or (and niwa--current-request
           (niwa-request-model-policy niwa--current-request))
      (let* ((implementation (niwa-implementation-policy-current))
             (base (niwa-implementation-policy-model implementation))
             (gptel-backend (if base (plist-get base :backend) gptel-backend))
             (gptel-model (if base (plist-get base :model) gptel-model))
             (gptel-reasoning-effort
              (if base (plist-get base :effort)
                (and (boundp 'gptel-reasoning-effort) gptel-reasoning-effort))))
        (niwa-model-resolve-workload
         (and (bound-and-true-p niwa--session)
              (not niwa--agent-invocation)
              (niwa-session-plan-mode niwa--session)
              'planning)
         (plist-get niwa-skills--pending-request-context :model)
         (plist-get niwa-skills--pending-request-context :effort)))))

(defun niwa-skills--transform-apply-request-model-policy (fsm)
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
               ;; This synchronous first transform runs before asynchronous
               ;; preparation.  Terminal cleanup must identify this submission,
               ;; not whichever pending policy a later callback happens to see.
               (plist-put info :niwa-pending-implementation-policy
                          niwa--pending-implementation-policy)
               (plist-put info :niwa-pending-permission-mode
                          niwa--pending-permission-mode)
               (plist-put info :niwa-implementation-preparation
                          (niwa-implementation-policy-capture-preparation))
               (niwa-skills-request-model-policy))))
        (setq-local gptel-backend (plist-get policy :backend))
        (setq-local gptel-model (plist-get policy :model))
        (setq-local gptel-reasoning-effort (plist-get policy :effort))
        (plist-put info :reasoning-effort gptel-reasoning-effort)))))

;;
;;; Unified skill invocation API

(defun niwa-skills--display-event (scope display-callback event)
  "Funcall DISPLAY-CALLBACK with EVENT, ignoring errors.
SCOPE is the originating workspace root captured before async preparation.
DISPLAY-CALLBACK may be nil; EVENT is a lifecycle event plist
\\=."
  (when display-callback
    (condition-case err
        (funcall display-callback event)
      (error
       (niwa--warn-once
        scope
        'skill-display-callback
        "Skill display-callback error: %s"
        (error-message-string err))))))

(defun niwa-skills--invoke-error (skill reason message
                                           callback display-callback)
  "Emit SKILL error event, then deliver REASON and MESSAGE to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (niwa-skill-name skill))))
    (niwa-skills--display-event
     (niwa-diagnostic-scope (and skill (niwa-skill-workspace skill)))
     display-callback
     `(:event error :skill ,skill-name
              :reason ,reason :message ,message))
    (funcall callback
             `(:status error :reason ,reason :message ,message))))

(defun niwa-skills--run-expansion-hook
    (skill arguments prompt origin session callback)
  "Run `UserPromptExpansion' for a user SKILL expansion.
ARGUMENTS is the raw user argument string.  SESSION supplies workspace
context.  CALLBACK receives (PROMPT DECISION).  Non-user ORIGIN values
skip the hook and call CALLBACK with PROMPT and nil."
  (if (not (eq origin 'user))
      (funcall callback prompt nil)
    (let* ((workspace (and session (niwa-session-workspace session)))
           (request (and (boundp 'niwa--current-request)
                         niwa--current-request)))
      (niwa-hooks-run-event
       'UserPromptExpansion
       (niwa-hooks-event-plist
        'UserPromptExpansion session workspace
        :skill-name (niwa-skill-name skill)
        :arguments arguments
        :prompt prompt)
       (lambda (decision)
         (let* ((updated (plist-get decision :updated-input))
                (prompt (if (stringp updated) updated prompt)))
           (funcall callback prompt decision)))
       session workspace request nil))))

(defun niwa-skills--invoke-done (skill outcome callback display-callback)
  "Emit SKILL done event, then deliver OUTCOME to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (niwa-skill-name skill))))
    (niwa-skills--display-event
     (niwa-diagnostic-scope (and skill (niwa-skill-workspace skill)))
     display-callback
     `(:event done :skill ,skill-name))
    (funcall callback outcome)))

(defun niwa-skills-format-body (skill body)
  "Return prepared BODY with SKILL's exact source address when available.
Keep BODY unchanged for a source-less SKILL.  This delivery wrapper does not
modify prepared bodies or invocation records."
  (if (and skill (niwa-skill-source-file skill))
      (format "Skill source: `%s`\n\n%s"
              (niwa-resource--skill-address skill) body)
    body))

(defun niwa-skills-format-attachment (attachment)
  "Return the model instruction body for prepared skill ATTACHMENT."
  (format "The host already attached the skill `$%s` for this request. You must follow it without calling `Skill`.\n\n%s"
          (plist-get attachment :name)
          (niwa-skills-format-body
           (or (plist-get attachment :skill)
               (when-let* ((source (plist-get attachment :source-file)))
                 (niwa-skill--create
                  :name (plist-get attachment :name) :source-file source)))
           (or (plist-get attachment :body) ""))))

(defun niwa-skills-format-model-input (prepared)
  "Return PREPARED's complete dependency attachments and root body."
  (mapconcat
   #'identity
   (append
    (mapcar (lambda (attachment)
              (niwa-reminders-format-block
               (niwa-skills-format-attachment attachment)))
            (plist-get prepared :required-attachments))
    (list (niwa-skills-format-body
           (plist-get prepared :skill) (or (plist-get prepared :body) ""))))
   "\n\n"))

(defun niwa-skills--prompt-rewrite-audit-record (original decision)
  "Return a `UserPromptExpansion' rewrite audit record, or nil."
  (when-let* ((updated (plist-get decision :updated-input))
              ((stringp updated)))
    (niwa--hook-prompt-rewrite-audit-record
     'UserPromptExpansion original updated
     (niwa-hooks-decision-reason decision))))

(cl-defun niwa-skills-activate-context
    (origin &key permission-rules model effort hook-rules
            (ptc-primitives :unrestricted) invoked-skill)
  "Apply skill-scoped overrides to the active context.

ORIGIN selects the install path:

- `user': append onto the buffer-local pending stash
  (`niwa-skills--pending-request-context'); drained at request
  begin by the WAIT-state begin handler in `niwa-presets.el'.
  Used because user skill dispatch fires before the `niwa-request'
  has been created.
- `model' / `internal': mutate the active sub-agent
  invocation (innermost) or request directly.  Model and effort are never
  installed on an already-realized request.

PERMISSION-RULES is a list of parsed niwa rules to append.
MODEL is a selector plist or nil.  EFFORT is an opaque gptel value or nil.
HOOK-RULES is a list of normalized hook rules.  PTC-PRIMITIVES narrows the
request's nested ToolCall roster; `:unrestricted' is the identity.
INVOKED-SKILL
is a `niwa-skill-invocation-record' to record on the session for
compaction/replay."
  (cond
   ((eq origin 'user)
    (let ((existing niwa-skills--pending-request-context))
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
               (niwa-skills-intersect-ptc-primitives
                (if (plist-member existing :ptc-primitives)
                    (plist-get existing :ptc-primitives)
                  :unrestricted)
                ptc-primitives))))
      (when invoked-skill
        (setq existing
              (plist-put existing :invoked-skills
                         (append (plist-get existing :invoked-skills)
                                 (list invoked-skill)))))
      (setq-local niwa-skills--pending-request-context existing)))
   (t
    (let ((req (niwa-skills--current-request))
          (inv (niwa-skills--current-invocation)))
      ;; Permission and hook rules accumulate on the innermost slot.
      (cond
       (inv
        (when permission-rules
          (setf (niwa-agent-invocation-skill-permission-rules inv)
                (append (niwa-agent-invocation-skill-permission-rules inv)
                        permission-rules)))
        (when hook-rules
          (setf (niwa-agent-invocation-hook-rules inv)
                (append (niwa-agent-invocation-hook-rules inv)
                        hook-rules))))
       (req
        (when permission-rules
          (setf (niwa-request-skill-permission-rules req)
                (append (niwa-request-skill-permission-rules req)
                        permission-rules)))
        (when hook-rules
          (setf (niwa-request-hook-rules req)
                (append (niwa-request-hook-rules req) hook-rules)))))
      (when (and req (not (eq ptc-primitives :unrestricted)))
        (setf (niwa-request-ptc-primitives req)
              (niwa-skills-intersect-ptc-primitives
               (niwa-request-ptc-primitives req)
               ptc-primitives)))
      ;; Record on the session.
      (when invoked-skill
        (when req
          (setf (niwa-request-attached-skill-records req)
                (append (niwa-request-attached-skill-records req)
                        (list invoked-skill))))
        (when-let* ((session (and (boundp 'niwa--session) niwa--session)))
          (niwa-skills-commit-invoked-records
           session (list invoked-skill))))))))

(cl-defun niwa-skills--invoke-inline
    (skill arguments callback &key origin display-callback skip-gates)
  "Prepare and commit inline SKILL with ARGUMENTS from ORIGIN.
CALLBACK receives the normalized outcome.  DISPLAY-CALLBACK receives
lifecycle events from the canonical preparation pipeline."
  (niwa-skills-prepare
   skill arguments
   (lambda (outcome)
     (when (eq (plist-get outcome :status) 'ok)
       (let ((context (plist-get outcome :request-context)))
         (niwa-skills-activate-context
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
           (niwa-skills-activate-context
            origin :invoked-skill record))))
     (funcall callback outcome))
   :role 'command
   :origin origin
   :policy-owner-p (not (eq origin 'model))
   :display-callback display-callback
   :skip-gates skip-gates))

(defun niwa-skills--preparation-rejection (skill role origin)
  "Return a structured preflight rejection for SKILL, ROLE, and ORIGIN."
  (cond
   ((not (niwa-skill-p skill))
    '(:reason unknown-skill :message "Invalid skill struct"))
   ((not (memq role '(command instruction)))
    (list :reason 'invalid-role
          :message (format "Invalid skill invocation role: %S" role)))
   ((not (memq origin '(user model internal)))
    (list :reason 'invalid-origin
          :message (format "Invalid skill invocation origin: %S" origin)))
   ((and (eq role 'command)
         (eq (niwa-skill-context skill) 'fork)
         (stringp (niwa-skill-agent skill))
         (not (string-empty-p (niwa-skill-agent skill)))
         (null (niwa-agent-get (niwa-skill-agent skill))))
    (list :reason 'unknown-agent
          :message
          (format "Skill '%s' references unknown agent '%s'"
                  (niwa-skill-name skill) (niwa-skill-agent skill))))))

(defun niwa-skills--preparation-policy
    (skill origin policy-owner-p)
  "Return structured request policy metadata for SKILL.
ORIGIN identifies the caller.  POLICY-OWNER-P means the command owns a future
request and may resolve and validate model/effort policy.  A model-origin
inline command that does not own policy records only the ignored field names."
  (if policy-owner-p
      (condition-case err
          (let* ((merged
                  (niwa-model-merge-skill-policy
                   (niwa-skill-name skill)
                   (niwa-skill-model skill)
                   (niwa-skill-effort skill)))
                 (selector (plist-get merged :model))
                 (effort (plist-get merged :effort))
                 (workload
                  (and (eq (niwa-skill-context skill) 'fork)
                       (niwa-skill-agent skill))))
            ;; Validate against the request owner's workload now, but retain the
            ;; selector/effort pair so the actual request boundary remains the
            ;; common resolver's source of truth.
            (niwa-model-resolve-workload workload selector effort)
            (list :status 'ok :model selector :effort effort))
        (error
         (list :status 'error :reason 'invalid-policy
               :message (error-message-string err))))
    (list :status 'ok
          :ignored-fields
          (and (eq origin 'model)
               (eq (niwa-skill-context skill) 'inline)
               (niwa-model-skill-policy-fields
                (niwa-skill-name skill)
                (niwa-skill-model skill)
                (niwa-skill-effort skill))))))

(defun niwa-skills--dependency-skill (session source-key)
  "Return SESSION's skill bound to exact SOURCE-KEY."
  (cl-find source-key (and session (niwa-session-skills session))
           :key (lambda (skill)
                  (niwa-skills-source-key
                   (niwa-skill-source-file skill)))
           :test #'equal))

(cl-defun niwa-skills--preparation-plan
    (roots origin &key skip-gates)
  "Return a fully validated dependency preparation plan for ROOTS.
ROOTS is a list of plists containing :skill, :arguments, :role, and
:policy-owner-p.  No body injection or hook runs during this function."
  (let* ((roots (mapcar (lambda (root)
                          (append root (list :node nil :policy nil)))
                        roots))
         (session (and (boundp 'niwa--session) niwa--session))
         (nodes (make-hash-table :test #'equal))
         (arguments-by-source (make-hash-table :test #'equal))
         (states (make-hash-table :test #'equal))
         order)
    (when session
      (niwa-skills-ensure-fresh
       (or (and (buffer-live-p (niwa-session-root-buffer session))
                (niwa-session-root-buffer session))
           (current-buffer))
       session))
    (catch 'invalid
      (cl-labels
          ((reject (reason format-string &rest args)
             (throw 'invalid
                    (list :status 'error :reason reason
                          :message (apply #'format format-string args))))
           (bind-root (skill)
             (if-let* ((source (niwa-skill-source-file skill)))
                 (or (niwa-skills--dependency-skill
                      session (niwa-skills-source-key source))
                     (and (null (and session
                                     (niwa-session-skills session)))
                          skill)
                     (reject 'dependency-source
                             "Skill '%s' is no longer bound to %s"
                             (niwa-skill-name skill) source))
               skill))
           (validate (skill root-p)
             (unless (niwa-skill-p skill)
               (reject 'unknown-skill "Invalid skill struct"))
             (when-let* ((diagnostics
                          (niwa-skill-dependency-diagnostics skill)))
               (reject 'dependency-invalid "%s"
                       (mapconcat #'identity diagnostics "; ")))
             (unless (or skip-gates
                         (niwa-skills-skill-enabled-p skill))
               (reject 'disabled "Skill '%s' is disabled"
                       (niwa-skill-name skill)))
             (unless skip-gates
               (cond
                ((and root-p
                      (eq origin 'user)
                      (not (niwa-skill-user-invocable-p skill)))
                 (reject 'disabled "Skill '%s' is not user-invocable"
                         (niwa-skill-name skill)))
                ((and (eq origin 'model)
                      (not (niwa-skill-model-invocable-p skill)))
                 (reject 'disabled "Skill '%s' is not model-invocable"
                         (niwa-skill-name skill)))))
             (when (and root-p
                        (not (memq origin '(user model internal))))
               (reject 'invalid-origin "Invalid skill invocation origin: %S"
                       origin)))
           (visit (skill arguments path required-by-source-path depth)
             (validate skill nil)
             (let* ((arguments (or arguments ""))
                    (source-key
                     (or (niwa-skills-source-key
                          (niwa-skill-source-file skill))
                         skill))
                    (prior-arguments
                     (gethash source-key arguments-by-source 'missing))
                    (key (cons source-key arguments)))
               (when (and (not (eq prior-arguments 'missing))
                          (not (equal prior-arguments arguments)))
                 (reject 'dependency-conflict
                         "Skill '%s' is required with conflicting arguments"
                         (niwa-skill-name skill)))
               (puthash source-key arguments arguments-by-source)
               (pcase (gethash key states)
                 ('visiting
                  (reject 'dependency-cycle
                          "Required skill dependency cycle: %s"
                          (mapconcat #'identity
                                     (append path
                                             (list (niwa-skill-name skill)))
                                     " -> ")))
                 ('done (gethash key nodes))
                 (_
                  (puthash key 'visiting states)
                  (when-let* ((file (niwa-skill-source-file skill))
                              ((not (file-readable-p file))))
                    (reject 'dependency-source
                            "Skill '%s' source is not readable: %s"
                            (niwa-skill-name skill) file))
                  (let* ((body
                          (progn
                            (when (niwa-skill-source-file skill)
                              (setf (niwa-skill-body skill) nil))
                            (niwa-skill-load-body skill)))
                         (substituted
                          (and body
                               (condition-case err
                                   (niwa-skills-preparation-substitute
                                    (copy-sequence body) arguments session skill)
                                 (error
                                  (reject 'resource-target "%s"
                                          (error-message-string err))))))
                         (parsed
                          (and substituted
                               (niwa-skills-syntax-parse-dependencies
                                substituted)))
                         (runtime-dependencies
                          (plist-get parsed :dependencies))
                         (bindings (niwa-skill-dependencies skill))
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
                              (niwa-skill-name skill)
                              (or (niwa-skill-source-file skill)
                                  "unknown source")))
                    (unless (= (length runtime-dependencies)
                               (length bindings))
                      (reject 'dependency-source
                              "Skill '%s' dependency bindings are stale"
                              (niwa-skill-name skill)))
                    (puthash key node nodes)
                    (cl-mapc
                     (lambda (dependency binding)
                       (unless (equal (plist-get dependency :name)
                                      (niwa-skill-dependency-name binding))
                         (reject 'dependency-source
                                 "Skill '%s' dependency bindings are stale"
                                 (niwa-skill-name skill)))
                       (let* ((child-source
                               (niwa-skill-dependency-source-key binding))
                              (child
                               (niwa-skills--dependency-skill
                                session child-source)))
                         (unless child
                           (reject 'dependency-source
                                   "Required skill '%s' is no longer bound"
                                   (niwa-skill-dependency-name binding)))
                         (push
                          (visit child
                                 (or (plist-get dependency
                                                :argument-template)
                                     "")
                                 (append path
                                         (list (niwa-skill-name skill)))
                                 (niwa-skill-source-file skill)
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
                  (niwa-skills--preparation-rejection skill role origin)))
            (when rejection
              (reject (plist-get rejection :reason) "%s"
                      (plist-get rejection :message)))
            (validate skill t)
            (let ((policy
                   (niwa-skills--preparation-policy
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
                  (setf (plist-get node :root) root))))))
        (list :status 'ok :roots roots :nodes (nreverse order))))))

(defun niwa-skills--without-added (list added)
  "Return LIST without the last occurrence of each element of ADDED."
  (dolist (item added list)
    (setq list (cl-remove item list :test #'eq :count 1 :from-end t))))

(defmacro niwa-skills--in-buffer (buffer &rest body)
  "Run BODY in BUFFER while it is live, else in the current buffer."
  (declare (indent 1) (debug t))
  `(if (buffer-live-p ,buffer)
       (with-current-buffer ,buffer ,@body)
     (progn ,@body)))

(defun niwa-skills--preparation-settler
    (session rules hooks callback)
  "Scope preparation's commands and return its settlement closure.
While a turn holds the buffer's request slot, preparation runs under that
turn: the slot never changes hands, so the turn settles, aborts and reads
as running as it would without a preparation.  RULES and HOOKS join its
innermost scope, the agent invocation if any, until the closure runs.
Otherwise a temporary preparation request holds the slot, keeping the
caller's Plan limits.  The closure undoes either and calls CALLBACK with
its outcome exactly once."
  (let ((origin-buffer (current-buffer))
        (turn (and (boundp 'niwa--current-request) niwa--current-request))
        (invocation niwa--agent-invocation)
        settled)
    (if turn
        (cl-flet ((adjust (function)
                    (if invocation
                        (setf (niwa-agent-invocation-skill-permission-rules invocation)
                              (funcall function
                                       (niwa-agent-invocation-skill-permission-rules
                                        invocation)
                                       rules)
                              (niwa-agent-invocation-hook-rules invocation)
                              (funcall function
                                       (niwa-agent-invocation-hook-rules invocation)
                                       hooks))
                      (setf (niwa-request-skill-permission-rules turn)
                            (funcall function (niwa-request-skill-permission-rules turn)
                                     rules)
                            (niwa-request-hook-rules turn)
                            (funcall function (niwa-request-hook-rules turn) hooks)))))
          (adjust #'append)
          (lambda (outcome)
            (unless settled
              (setq settled t)
              (adjust #'niwa-skills--without-added)
              (niwa-skills--in-buffer origin-buffer (funcall callback outcome)))))
      (let ((invocation-local-p (local-variable-p 'niwa--agent-invocation))
            (request (niwa-request--create
                      :session session
                      :origin (niwa-current-origin)
                      :file-snapshots (make-hash-table :test #'equal)
                      :skill-permission-rules rules
                      :hook-rules hooks
                      :plan-read-only (and (niwa-plan-read-only-p session) t))))
        (setq-local niwa--current-request request)
        (setq-local niwa--agent-invocation nil)
        (lambda (outcome)
          (unless settled
            (setq settled t)
            (when (buffer-live-p origin-buffer)
              (with-current-buffer origin-buffer
                ;; A request admitted after this one was torn down keeps the
                ;; slot, and its agent invocation.
                (when (memq niwa--current-request (list request nil))
                  (setq-local niwa--current-request nil)
                  (if invocation-local-p
                      (setq-local niwa--agent-invocation invocation)
                    (kill-local-variable 'niwa--agent-invocation)))))
            (niwa-skills--in-buffer origin-buffer (funcall callback outcome))))))))

(defun niwa-skills--preparation-success-outcome
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
          (niwa-skill-invocation-record--create
           :name (niwa-skill-name skill)
           :args arguments
           :role role
           :origin origin
           :agent-path
           (and session
                (if (fboundp 'niwa-agent-control-current-path)
                    (niwa-agent-control-current-path session)
                  "/root"))
           :turn (and session (niwa-session-turn-count session))
           :source-path (niwa-skill-source-file skill)
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
                    :ptc-primitives (niwa-skill-ptc-primitives skill)
                    :invoked-skills (list record))
            (list :invoked-skills (list record))))
         (kind (cond
                ((eq role 'instruction) 'instruction)
                ((eq (niwa-skill-context skill) 'fork) 'fork)
                (t 'inline)))
         (audit (niwa-skills--prompt-rewrite-audit-record original decision))
         (hook-context
          (niwa-hooks-additional-context-string
           decision 'UserPromptExpansion)))
    (list :status 'ok :kind kind :skill skill
          :body expanded :arguments arguments
          :hook-context hook-context
          :hook-audits (and audit (list audit))
          :ignored-policy-fields (plist-get metadata :ignored-policy-fields)
          :request-context context)))

(defun niwa-skills--prepare-single
    (node origin callback)
  "Prepare preflighted NODE from ORIGIN, then call CALLBACK."
  (let* ((root (plist-get node :root))
         (skill (plist-get node :skill))
         (skill-name (niwa-skill-name skill))
         (arguments (plist-get node :arguments))
         (body (plist-get node :body))
         (role (or (plist-get root :role) 'instruction))
         (policy (or (plist-get root :policy) '(:status ok)))
         (display-callback (plist-get root :display-callback))
         (session (and (boundp 'niwa--session) niwa--session))
         (scope (niwa-diagnostic-scope
                 (or session (niwa-skill-workspace skill))))
         (command-p (eq role 'command))
         (rules (niwa-skill-allowed-tool-rules skill))
         (hooks (and command-p (niwa-skill-hooks skill)))
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
         ;; A turn's preparation runs only while that turn does.
         (turn (and (boundp 'niwa--current-request) niwa--current-request))
         (live-p (and turn
                      (lambda ()
                        (and (eq turn niwa--current-request)
                             (not (niwa-request-cancelled-p turn))))))
         (finish (niwa-skills--preparation-settler
                  session rules hooks callback)))
    (cl-labels
        ((fail (reason message)
           (niwa-skills--display-event
            scope display-callback
            `(:event error :skill ,skill-name :reason ,reason :message ,message))
           (funcall finish
                    (list :status 'error :reason reason :message message)))
         (complete (original expanded decision)
           (setq decision
                 (niwa-hooks-sanitize-final-decision
                  'UserPromptExpansion decision scope))
           (if (and (plist-member decision :continue)
                    (not (plist-get decision :continue)))
               (fail 'hook-blocked
                     (or (plist-get decision :stop-reason)
                         "UserPromptExpansion hook stopped skill"))
             (niwa-skills--display-event
              scope display-callback `(:event done :skill ,skill-name))
             (funcall finish
                      (niwa-skills--preparation-success-outcome
                       metadata original expanded decision)))))
      (niwa-skills-preparation-expand-body
       body
       (lambda (injection-outcome)
         (if (eq (plist-get injection-outcome :status) 'ok)
             (niwa-skills--run-expansion-hook
              skill arguments (plist-get injection-outcome :body)
              origin session
              (lambda (expanded decision)
                (complete (plist-get injection-outcome :body)
                          expanded decision)))
           (fail (plist-get injection-outcome :reason)
                 (plist-get injection-outcome :message))))
       skill session live-p))))

(defun niwa-skills--node-reachable-p (candidate root)
  "Return non-nil when CANDIDATE is ROOT or one of its dependencies."
  (or (eq candidate root)
      (cl-some (lambda (child)
                 (niwa-skills--node-reachable-p candidate child))
               (plist-get root :dependencies))))

(cl-defun niwa-skills-prepare-many
    (roots callback &key origin skip-gates cancelled-p)
  "Preflight and sequentially prepare ROOTS as one dependency graph.
Each ROOT is a plist with :skill, :arguments, :role, :policy-owner-p, and an
optional :display-callback.  CALLBACK receives one atomic aggregate outcome."
  (let ((plan (niwa-skills--preparation-plan
               roots origin :skip-gates skip-gates)))
    (if (not (eq (plist-get plan :status) 'ok))
        (progn
          (when-let* ((root (car roots)))
            (niwa-skills--display-event
             (niwa-diagnostic-scope
              (and (niwa-skill-p (plist-get root :skill))
                   (niwa-skill-workspace (plist-get root :skill))))
             (plist-get root :display-callback)
             `(:event error
                      :skill ,(and (niwa-skill-p (plist-get root :skill))
                                   (niwa-skill-name
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
                   (niwa-skills--prepare-single
                    node origin
                    (lambda (outcome)
                      (if (and cancelled-p (funcall cancelled-p))
                          (finish '(:status error :reason cancelled
                                             :message "Skill preparation was cancelled"))
                        (if (not (eq (plist-get outcome :status) 'ok))
                          (finish
                           (plist-put
                            (copy-sequence outcome) :name
                            (niwa-skill-name (plist-get node :skill))))
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
                                (niwa-skills--node-reachable-p
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
                                   :name (niwa-skill-name skill)
                                   :body (plist-get outcome :body)
                                   :arguments (plist-get outcome :arguments)
                                   :source-file
                                   (niwa-skill-source-file skill)
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

(cl-defun niwa-skills-prepare
    (skill arguments callback
           &key role origin policy-owner-p display-callback skip-gates)
  "Prepare SKILL and its dependencies without dispatching policy.
ROLE is `command' or `instruction'.  Required children prepare first and
contribute only instruction context."
  (niwa-skills-prepare-many
   (list (list :skill skill :arguments arguments :role role
               :policy-owner-p policy-owner-p
               :display-callback display-callback))
   (lambda (outcome)
     (funcall callback
              (if (eq (plist-get outcome :status) 'ok)
                  (car (plist-get outcome :outcomes))
                outcome)))
   :origin origin :skip-gates skip-gates))

(defun niwa-skills--build-parent-inherited-agent (skill)
  "Build a synthetic `niwa-agent' for SKILL with no `agent' field.

Captures the calling buffer's current gptel state at spawn time
and returns a `niwa-agent' struct named `skill:<skill-name>'.
The agent inherits the parent's system prompt directly; tools are
inherited via the request-locals snapshot captured by
`niwa-agent-exec-run' at dispatch time, which carries the
calling buffer's `gptel-tools' through to the spawned agent
buffer.
"
  (let* ((skill-name (niwa-skill-name skill))
         (agent-name (concat "skill:" skill-name))
         (parent-system (and (boundp 'gptel-system-prompt)
                             gptel-system-prompt))
         (agent
          (niwa-agent--create
           :name agent-name
           :description (or (niwa-skill-description skill)
                            (format "Parent-inherited fork of skill %s"
                                    skill-name))
           :tools nil
           :system-prompt (or parent-system "")
           :max-turns nil
           :reminders nil)))
    agent))

(defun niwa-skills--build-fork-agent (skill)
  "Return a `niwa-agent' struct to use for SKILL's fork dispatch.

If SKILL declares an `agent' field, look it up in the registry
and return that agent.  Returns nil for unknown agent names so
the caller can produce an `unknown-agent' outcome.

If SKILL does not declare an `agent' field, build a synthetic
parent-inherited agent via
`niwa-skills--build-parent-inherited-agent'.  The synthetic
agent's name is `skill:<skill-name>'; system prompt is
snapshotted from the calling buffer's `gptel-system-prompt';
tools propagate through the spawn path's request-locals capture."
  (let ((agent-name (niwa-skill-agent skill)))
    (cond
     ((and (stringp agent-name) (not (string-empty-p agent-name)))
      (niwa-agent-get agent-name))
     (t
      (niwa-skills--build-parent-inherited-agent skill)))))

(defun niwa-skills--fork-task-name (session skill-name)
  "Return an unused retained-agent task name for SKILL-NAME in SESSION."
  (let* ((base
          (concat
           "skill_"
           (replace-regexp-in-string
            "[^a-z0-9_]+" "_" (downcase skill-name))))
         (parent (niwa-agent-control-current-path session))
         (candidate base)
         (suffix 2))
    (while (assoc (concat parent "/" candidate)
                  (niwa-session-agent-registry session))
      (setq candidate (format "%s_%d" base suffix)
            suffix (1+ suffix)))
    candidate))

(defun niwa-skills--fork-result-render-data (session envelope skill description)
  "Return transcript render data for retained result ENVELOPE in SESSION."
  (let* ((path (plist-get envelope :sender))
         (record (cdr (assoc path (niwa-session-agent-registry session)))))
    (when record
      (list :kind 'collaboration-event
            :event 'started
            :path path
            :agent-id (niwa-agent-record-id record)
            :role (niwa-agent-record-role record)
            :name (niwa-skill-name skill)
            :description description
            :status 'completed
            :calls 0
            :body ""
            :transcript-relative-path
            (niwa-agent-record-conversation-location record)))))

(defun niwa-skills--handle-fork-result
    (session skill prepared hook-audits description callback display-callback
             envelope)
  "Deliver retained fork ENVELOPE through the ordinary skill callbacks."
  (let ((outcome (plist-get envelope :outcome))
        (payload (plist-get envelope :payload))
        (path (plist-get envelope :sender)))
    (if (eq outcome 'completed)
        (niwa-skills--invoke-done
         skill
         (list :status 'ok
               :kind 'fork
               :result payload
               :agent-path path
               :hook-audits (or hook-audits
                                (plist-get prepared :hook-audits))
               :render-data
               (niwa-skills--fork-result-render-data
                session envelope skill description))
         callback display-callback)
      (niwa-skills--invoke-error
       skill
       (intern (format "agent-%s" outcome))
       (if (stringp payload) payload (format "%S" payload))
       callback display-callback))))

(cl-defun niwa-skills-dispatch-prepared-fork
    (prepared callback &key prompt request-context hook-audits
              description on-invocation display-callback)
  "Dispatch an already PREPARED fork command and call CALLBACK.
PROMPT is the complete post-plan, post-submit-hook child prompt and
defaults to PREPARED's body.  REQUEST-CONTEXT is the aggregate plan
context and defaults to PREPARED's context.  HOOK-AUDITS are attached
to the eventual fork outcome.  DESCRIPTION, ON-INVOCATION, and
DISPLAY-CALLBACK retain the normal fork dispatch meanings."
  (let* ((skill (plist-get prepared :skill))
         (skill-name (and skill (niwa-skill-name skill)))
         (agent (and skill (niwa-skills--build-fork-agent skill)))
         (context (or request-context
                      (plist-get prepared :request-context)))
         (session (and (boundp 'niwa--session) niwa--session))
         (task-description
          (or description
              (and skill (niwa-skill-description skill))
              skill-name)))
    (cond
     ((not (and (eq (plist-get prepared :status) 'ok)
                (eq (plist-get prepared :kind) 'fork)
                (niwa-skill-p skill)))
      (niwa-skills--invoke-error
       skill 'invalid-prepared-fork "Invalid prepared fork outcome"
       callback display-callback))
     ((null agent)
      (niwa-skills--invoke-error
       skill 'unknown-agent
       (format "Skill '%s' references unknown agent '%s'"
               skill-name (niwa-skill-agent skill))
       callback display-callback))
     ((null session)
      (niwa-skills--invoke-error
       skill 'missing-session "Fork skills require an active session"
       callback display-callback))
     (t
      (niwa-skills-commit-invoked-records
       session (plist-get context :invoked-skills))
      (require 'niwa-agent-control)
      (condition-case err
          (niwa-agent-control-spawn
           session
           (niwa-skills--fork-task-name session skill-name)
           (or prompt (plist-get prepared :body) "")
           (lambda (outcome)
             (unless (eq (plist-get outcome :outcome) 'success)
               (niwa-skills--invoke-error
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
            #'niwa-skills--handle-fork-result
            session skill prepared hook-audits task-description
            callback display-callback))
        (error
         (niwa-skills--invoke-error
          skill 'agent-dispatch-failed (error-message-string err)
          callback display-callback)))))))

(cl-defun niwa-skills--invoke-fork-direct
    (skill arguments callback &key origin display-callback
           additional-context description on-invocation skip-gates)
  "Prepare and asynchronously dispatch fork SKILL with ARGUMENTS.

ORIGIN identifies the invocation source.  ADDITIONAL-CONTEXT is appended to
the prepared child prompt.  DESCRIPTION, ON-INVOCATION, DISPLAY-CALLBACK, and
CALLBACK retain the public invocation lifecycle semantics."
  (niwa-skills-prepare
   skill arguments
   (lambda (prepared)
     (if (not (eq (plist-get prepared :status) 'ok))
         (funcall callback prepared)
       (let ((prompt (niwa-skills-format-model-input prepared)))
         (when (and (stringp additional-context)
                    (not (string-empty-p additional-context)))
           (setq prompt (concat prompt "\n\n" additional-context)))
         (niwa-skills-dispatch-prepared-fork
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

(cl-defun niwa-skills-invoke
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
  (let* ((skill-name (and skill (niwa-skill-name skill)))
         (session (niwa-telemetry-current-session))
         (started-at (float-time))
         (original-callback callback)
         (callback
          (lambda (outcome)
            (when session
              (niwa-telemetry-record
               session 'skill-invocation-settled
               :skill-name skill-name
               :origin origin
               :status (plist-get outcome :status)
               :kind (plist-get outcome :kind)
               :reason (plist-get outcome :reason)
               :duration-ms (round (* 1000.0
                                      (- (float-time) started-at)))))
            (funcall original-callback outcome))))
    (when session
      (niwa-telemetry-record
       session 'skill-invocation-requested
       :skill-name skill-name
       :origin origin
       :context (and skill (niwa-skill-context skill))
       :skip-gates (and skip-gates t)))
    (cond
     ((not (niwa-skill-p skill))
      (niwa-skills--invoke-error
       skill 'unknown-skill
       "Invalid skill struct"
       callback display-callback))
     ((and (eq origin 'model)
           (when-let* ((request (niwa-skills--current-request))
                       (source-key
                        (niwa-skills-source-key
                         (niwa-skill-source-file skill))))
             (cl-some
              (lambda (record)
                (equal source-key
                       (niwa-skills-source-key
                        (niwa-skill-invocation-record-source-path record))))
              (niwa-request-attached-skill-records request))))
      (niwa-skills--invoke-done
       skill
       (list :status 'ok
             :kind 'already-attached
             :body (format
                    "Skill '$%s' is already attached for this request."
                    skill-name))
       callback display-callback))
     ;; User-disabled skill gating.
     ((and (not skip-gates)
           (not (niwa-skills-skill-enabled-p skill)))
      (niwa-skills--invoke-error
       skill 'disabled
       (if (eq origin 'user)
           (format "Skill $%s is disabled. Enable it with /skills enable %s or escape it as \\$%s."
                   skill-name skill-name skill-name)
         (format "Skill '%s' is disabled" skill-name))
       callback display-callback))
     ;; User-slash gating.
     ((and (not skip-gates)
           (eq origin 'user)
           (not (niwa-skill-user-invocable-p skill)))
      (niwa-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not user-invocable" skill-name)
       callback display-callback))
     ;; Model-side gating.
     ((and (not skip-gates)
           (eq origin 'model)
           (not (niwa-skill-model-invocable-p skill)))
      (let* ((source (niwa-skills-source-key (niwa-skill-source-file skill)))
             (path (and session (niwa-agent-control-current-path session)))
             (prepared
              (and source path
                   (cl-find-if
                    (lambda (record)
                      (and (eq 'user (niwa-skill-invocation-record-origin record))
                           (equal path (niwa-skill-invocation-record-agent-path record))
                           (equal source (niwa-skills-source-key
                                          (niwa-skill-invocation-record-source-path record)))
                           (or (not arguments) (equal arguments "")
                               (equal arguments (niwa-skill-invocation-record-args record)))
                           (stringp (niwa-skill-invocation-record-prepared-body record))))
                    (reverse (niwa-session-invoked-skills session))))))
        (if prepared
            ;; Re-reading resolved user guidance does not invoke the skill:
            ;; no source reload, injections, policy, hooks or new record.
            (niwa-skills--invoke-done
             skill
             (list :status 'ok :kind 'already-prepared
                   :body (concat "Previously prepared user skill; reuse this guidance where it remains applicable, without invoking it again.\n\n"
                                 (niwa-skill-invocation-record-prepared-body prepared)))
             callback display-callback)
          (niwa-skills--invoke-error
           skill 'disabled
           (format "Skill '%s' is not model-invocable" skill-name)
           callback display-callback))))
     (t
      (pcase (niwa-skill-context skill)
        ('inline
          (niwa-skills--invoke-inline
           skill arguments callback
           :origin origin :display-callback display-callback
           :skip-gates skip-gates))
        ('fork
         (niwa-skills--invoke-fork-direct
          skill arguments callback
          :origin origin :display-callback display-callback
          :additional-context additional-context
          :description description
          :on-invocation on-invocation
          :skip-gates skip-gates))
        (other
         (niwa-skills--invoke-error
          skill 'unknown-skill
          (format "Skill '%s' has unsupported context: %S"
                  skill-name other)
          callback display-callback)))))))


;;
;;; Skill tool handler

(defun niwa-skills--render-skill-tool (name args result render-data)
  "Return rendering plist for NAME, ARGS, and RESULT from the Skill tool.

Required-attachment reminders lead RESULT so the model reads its
dependencies before the skill body.  The row shows the skill's own body
and names the attachments in the header, so no reminder markup reaches
the reader as prose."
  (when (stringp result)
    (let* ((skill-name (or (plist-get args :name) "?"))
           (body (or (plist-get render-data :prompt) result))
           (attachments (plist-get render-data :attachments))
           (lines (length (split-string body "\n" t)))
           (fields (plist-get render-data :ignored-policy-fields))
           (ignored-fields
            (and (eq (plist-get render-data :kind) 'skill-invocation)
                 (member fields '((model) (effort) (model effort)))
                 fields))
           (ignored-names (mapconcat #'symbol-name ignored-fields ", "))
           (ignored-description (mapconcat #'symbol-name ignored-fields " and ")))
      (list :header (format "%s: %s (%d %s%s%s)"
                            (or name "Skill")
                            skill-name
                            lines
                            (if (= lines 1) "line" "lines")
                            (if attachments
                                (format "; attached %s"
                                        (mapconcat #'identity attachments ", "))
                              "")
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
                       body)
                    body)
            :body-mode 'markdown-mode
            :status (cond
                     ((string-prefix-p "Error:" result) 'error)
                     (ignored-fields 'warning))
            :initially-collapsed-p t))))

(defun niwa-skills--invoke-handler (callback args)
  "Pipeline handler for the `Skill' tool.

CALLBACK is the async tool callback.  ARGS is a plist with :name
and optional :arguments.

Routes through `niwa-skills-invoke' with model origin
and projects the outcome plist to a tool-result string: success
returns the body; error returns a `Error: ' prefixed message."
  (let* ((name (plist-get args :name))
         (arguments (plist-get args :arguments))
         (session (and (boundp 'niwa--session) niwa--session))
         (skill (and session (niwa-session-get-skill session name)))
         (return
          (lambda (result &optional outcome)
            ;; OUTCOME is present only for a successful invocation.
            ;; Raw bodies travel beside delivery wrappers as `:prompt', so
            ;; source headers and dependency reminders stay out of the row.
            (let* ((attachments
                    (and (plist-get outcome :body)
                         (mapcar (lambda (attachment)
                                   (plist-get attachment :name))
                                 (plist-get outcome :required-attachments))))
                   (ignored (plist-get outcome :ignored-policy-fields))
                   (render-data
                    (when (or attachments ignored
                              (and (plist-get outcome :body)
                                   (plist-get outcome :skill)
                                   (niwa-skill-source-file
                                    (plist-get outcome :skill))))
                      (list :kind 'skill-invocation
                            :prompt (plist-get outcome :body)
                            :attachments attachments
                            :ignored-policy-fields ignored))))
              (when attachments
                (setq render-data
                      (plist-put render-data :attachment-bodies
                                 (mapcar
                                  (lambda (attachment)
                                    (cons (plist-get attachment :name)
                                          (or (plist-get attachment :body) "")))
                                  (plist-get outcome :required-attachments)))))
              (funcall callback
                       (if render-data
                           (list :result result :render-data render-data)
                         (list :result result)))))))
    (cond
     ((not (stringp name))
      (funcall return "Error: Skill name is required."))
     ((not session)
      (funcall return "Error: No active niwa session."))
     ((not skill)
      (funcall return (format "Error: Unknown skill '%s'." name)))
     (t
      (niwa-skills-invoke
       skill arguments
       (lambda (outcome)
         (pcase (plist-get outcome :status)
           ('ok
            (funcall return
                     (or (and (plist-get outcome :body)
                              (niwa-skills-format-model-input outcome))
                         (plist-get outcome :result)
                         (format "Skill '%s' produced no body." name))
                     outcome))
           ('error
            (funcall return
                     (format "Error: %s"
                             (or (plist-get outcome :message)
                                 "skill invocation failed"))))))
       :origin 'model)))))

(defconst niwa-skills--list-tool-limit 25
  "Maximum entries returned by the ListSkills tool without narrowing.")

(defun niwa-skills--model-visible-p (skill &optional active-only)
  "Return non-nil when SKILL may be shown to the model.
When ACTIVE-ONLY is non-nil, dormant path-scoped skills are excluded."
  (and (niwa-skill-model-invocable-p skill)
       (niwa-skill-effective-model-invocable-p skill)
       (niwa-skills-skill-enabled-p skill)
       (or (not active-only)
           (niwa-skill-active-p skill))))

(defun niwa-skills--skill-matches-query-p (skill query)
  "Return non-nil when SKILL matches QUERY."
  (or (not (and (stringp query) (not (string-empty-p query))))
      (let ((case-fold-search t)
            (needle (regexp-quote query)))
        (cl-some
         (lambda (value)
           (and (stringp value) (string-match-p needle value)))
         (list (niwa-skill-name skill)
               (niwa-skill-display-name skill)
               (niwa-skill-description skill))))))

(defun niwa-skills--format-list-tool-result
    (skills omitted &optional mark-dormant user-only)
  "Format SKILLS for the ListSkills tool, noting OMITTED entries.
USER-ONLY names matching skills the model cannot invoke, so it can point
the user at them instead of concluding they do not exist."
  (concat
   (if (null skills)
       "No model-invocable skills match."
     (let ((body
            (mapconcat (lambda (skill)
                         (niwa-skills--listing-describe
                          skill
                          (and mark-dormant
                               (niwa-skills--model-visible-p skill)
                               (niwa-skill-path-patterns skill)
                               (not (niwa-skill-active-p skill)))))
                       skills "\n")))
       (if (> omitted 0)
           (concat body
                   (format "\n\n%d more skill(s) omitted; use query to narrow."
                           omitted))
         body)))
   (when user-only
     (concat "\n\nUser-only skills (Skill cannot invoke them; the user can run them): "
             (mapconcat (lambda (skill) (concat "$" (niwa-skill-name skill)))
                        user-only ", ")))))

(defun niwa-skills--list-handler (callback args)
  "Pipeline handler for the `ListSkills' tool.
CALLBACK is the async tool callback.  ARGS is a plist with optional :query."
  (let* ((query (plist-get args :query))
         (session (and (boundp 'niwa--session) niwa--session))
         (return (lambda (result)
                   (funcall callback (list :result result)))))
    (cond
     ((not session)
      (funcall return "Error: No active niwa session."))
     ((and query (not (stringp query)))
      (funcall return "Error: query must be a string."))
     (t
      (when (buffer-live-p (current-buffer))
        (niwa-skills-ensure-fresh (current-buffer) session))
      (let* ((narrowed (and (stringp query)
                            (not (string-empty-p (string-trim query)))))
             (pool (if narrowed
                       (cl-remove-if-not
                        #'niwa-skills--model-visible-p
                        (niwa-session-skills session))
                     (niwa-skills--listing-candidates session)))
             (matches
              (cl-remove-if-not
               (lambda (skill)
                 (niwa-skills--skill-matches-query-p skill query))
               pool))
             (shown (cl-subseq matches 0
                                (min (length matches)
                                     niwa-skills--list-tool-limit)))
             (omitted (max 0 (- (length matches) (length shown))))
             (user-only
              (and narrowed
                   (seq-take
                    (cl-remove-if-not
                     (lambda (skill)
                       (and (niwa-skill-user-invocable-p skill)
                            (niwa-skills-skill-enabled-p skill)
                            (not (niwa-skills--model-visible-p skill))
                            (niwa-skills--skill-matches-query-p skill query)))
                     (niwa-session-skills session))
                    niwa-skills--list-tool-limit))))
        (funcall return
                 (niwa-skills--format-list-tool-result
                  shown omitted narrowed user-only)))))))


;;
;;; Shared model listing primitives

(defcustom niwa-skills-listing-max-entry-chars 1536
  "Maximum characters per skill entry in the model-facing skills roster.

Entries longer than this are truncated with an ellipsis so a single
verbose description cannot starve the rest of the listing.  The
default cap is 1,536 chars."
  :type 'integer
  :group 'niwa)

(defconst niwa-skills--source-priority
  '((project . niwa)
    (project . agents)
    (user . niwa)
    (user . agents)
    bundled
    managed
    plugin)
  "Source priority for model-facing skills roster ordering.")

(defun niwa-skills--source-priority-key (skill)
  "Return ordering key for SKILL in model-facing rosters."
  (or (cl-position
       (cond
        ((and (niwa-skills--ordinary-skill-p skill)
              (niwa-skill-source-family skill))
         (cons (niwa-skill-source skill)
               (niwa-skill-source-family skill)))
        ((eq (niwa-skill-source skill) 'project)
         '(project . niwa))
        ((eq (niwa-skill-source skill) 'user)
         '(user . niwa))
        (t
         (niwa-skill-source skill)))
       niwa-skills--source-priority
       :test #'equal)
      most-positive-fixnum))

(defun niwa-skills--truncate-text (text limit)
  "Return TEXT truncated to LIMIT characters, using `...' when possible."
  (let ((text (or text "")))
    (cond
     ((<= limit 0) "")
     ((<= (length text) limit) text)
     ((<= limit 3) (substring text 0 limit))
     (t (concat (substring text 0 (- limit 3)) "...")))))

(defun niwa-skills--entry-base (skill &optional dormant)
  "Return the roster line prefix for SKILL.
When DORMANT is non-nil, mark the skill as dormant path-scoped."
  (format "- %s%s%s:"
          (niwa-skill-name skill)
          (if dormant " [dormant path-scoped]" "")
          (if (niwa-skill-source-file skill)
              (format " [Read %s]" (niwa-resource--skill-address skill))
            "")))

(defun niwa-skills--entry-description (skill &optional dormant)
  "Return SKILL's description capped for a single roster entry."
  (let* ((base (niwa-skills--entry-base skill dormant))
         (limit (max 0 (- niwa-skills-listing-max-entry-chars
                          (length base)
                          1))))
    (niwa-skills--truncate-text
     (or (niwa-skill-description skill) "")
     limit)))

(defun niwa-skills--listing-describe (skill &optional dormant)
  "Return a one-line entry for SKILL.

Format:
  - name [Read skill://name@source-identity]: description

`niwa-skills-listing-max-entry-chars' (1,536 by default) caps entries by
truncation with an ellipsis so a single verbose skill cannot starve
the rest of the listing.  When DORMANT is non-nil, mark the skill as
dormant path-scoped for `ListSkills(query)' output."
  (concat (niwa-skills--entry-base skill dormant)
          " "
          (niwa-skills--entry-description skill dormant)))

(defun niwa-skills--listing-candidates (session)
  "Return SESSION's model-invocable, currently active skills.

Sorted by configured resource precedence so budget pressure drops
global/bundled/plugin entries before local resource entries."
  (let ((candidates
         (cl-remove-if-not
          (lambda (s)
            (niwa-skills--model-visible-p s t))
          (niwa-session-skills session))))
    (cl-sort (copy-sequence candidates)
             (lambda (a b)
               (< (niwa-skills--source-priority-key a)
                  (niwa-skills--source-priority-key b))))))


(provide 'niwa-skills-invoke)
;;; niwa-skills-invoke.el ends here
