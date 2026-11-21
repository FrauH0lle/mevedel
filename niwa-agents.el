;;; niwa-agents.el -- Agent definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative definitions for the specialised sub-agents that niwa spawns
;; through the Agent tool: `worker' (implementation), `explorer' (read-only
;; investigation), `verifier' (adversarial read-only verification), and
;; `reviewer' (structured code review).  Uses the `niwa-define-agent' macro
;; to bundle tool groups, prompt files, turn limits, and reminders.
;;
;; Per-turn state (cloned reminders and deferred-tool lifecycle)
;; lives on `niwa-agent-invocation' structs created at dispatch time rather
;; than on the agent definition itself.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'niwa-tool-registry)
(require 'niwa-engine)
(require 'niwa-models)
(require 'niwa-system)

;; `cl-seq'
(declare-function cl-delete-duplicates "cl-seq" (cl-seq &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-some "cl-seq" (cl-pred cl-seq &rest cl-rest))

;; `gptel-request'
(declare-function gptel--copy-tool "ext:gptel-request" (arg))
(declare-function gptel--model-name "ext:gptel-request" (model))
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-p "ext:gptel-request" (cl-x))
(declare-function gptel-tool-args "ext:gptel-request" (cl-x) t)
(defvar gptel-tools)

;; `niwa-agent-conversation'
(defvar niwa--agent-invocation)

;; `niwa-hooks'
(declare-function niwa-hooks-annotate-rules-source
                  "niwa-hooks"
                  (rules source &optional source-file source-root))
(declare-function niwa-hooks-normalize-rules
                  "niwa-hooks" (rules &optional scope))
(autoload 'niwa-hooks-annotate-rules-source "niwa-hooks")
(autoload 'niwa-hooks-normalize-rules "niwa-hooks")

;; `niwa-plan-mode'
(declare-function niwa-plan-mode-active-p "niwa-plan-mode"
                  (&optional session))
(autoload 'niwa-plan-mode-active-p "niwa-plan-mode")

;; `niwa-presets'
(declare-function niwa-preset--resolved-metadata
                  "niwa-presets" (name))

;; `niwa-reminders'
(declare-function niwa-reminders-clone-list "niwa-reminders" (reminders))
(declare-function niwa-reminders-make-max-turns-warning "niwa-reminders"
                  (&optional threshold))
(declare-function niwa-reminders-serialize-agent-templates
                  "niwa-reminders" (reminders))
(autoload 'niwa-reminders-clone-list "niwa-reminders")
(autoload 'niwa-reminders-make-max-turns-warning "niwa-reminders")
(autoload 'niwa-reminders-serialize-agent-templates "niwa-reminders")

;; `niwa-structs'
(declare-function niwa-agent-path-p "niwa-structs" (path))
(declare-function niwa-request-p "niwa-structs" (cl-x))
(declare-function niwa-request-plan-read-only "niwa-structs" (cl-x) t)
(declare-function niwa-request-permission-mode-override
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(autoload 'niwa-agent-path-p "niwa-structs")
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-tool-ptc'
(declare-function niwa-tool-ptc--roster "niwa-tool-ptc" (&optional include-hidden))
(autoload 'niwa-tool-ptc--roster "niwa-tool-ptc")

;; `niwa-tools'
(declare-function niwa-tools--ctx-tool-catalog "niwa-tools" (ctx))
(declare-function niwa-tools--current-context "niwa-tools" ())
(defvar niwa-tools--current-engine)
(autoload 'niwa-tools--ctx-tool-catalog "niwa-tools")
(autoload 'niwa-tools--current-context "niwa-tools")

;;
;;; Agent struct and registry

(cl-defstruct (niwa-agent (:constructor niwa-agent--create))
  "Agent definition for niwa sub-agents."
  (name nil :type string)
  (description nil :type string)
  (tools nil :type list)
  (system-prompt nil :type (or string function))
  (context-components nil :type list)
  (max-turns nil :type (or null integer))
  (reminders nil :type list)
  (hook-rules nil :type list)
  (frozen-p nil :type boolean))

(cl-defstruct
    (niwa-agent-configuration
     (:constructor niwa-agent-configuration--create))
  "Frozen identity and request state for one retained agent."
  (agent nil :type niwa-agent)
  (request-locals nil :type list))

(defconst niwa-agent-request-local-symbols
  '(gptel--num-messages-to-send
    gptel--request-params
    gptel--schema
    gptel-backend
    gptel-cache
    gptel-context
    gptel-include-reasoning
    gptel-max-tokens
    gptel-mode
    gptel-model
    gptel-reasoning-effort
    gptel-stream
    gptel-system-prompt
    gptel-temperature
    gptel-tools
    gptel-track-media
    gptel-track-response
    gptel-use-context
    gptel-use-curl
    gptel-use-tools
    niwa-model-tiers
    niwa-model-workloads)
  "Closed buffer-local schema for frozen retained-agent requests.")

(defun niwa-agent-request-locals-p (locals &optional complete)
  "Return non-nil when LOCALS has unique allowed keys.
When COMPLETE is non-nil, require every request-local key exactly once."
  (and (proper-list-p locals)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (memq (car entry)
                              niwa-agent-request-local-symbols)))
                 locals)
       (= (length locals)
          (length (delete-dups (mapcar #'car locals))))
       (or (not complete)
           (= (length locals)
              (length niwa-agent-request-local-symbols)))))

(defun niwa-agent-configuration-model-metadata (configuration)
  "Return the provider, model and effort CONFIGURATION sends, or nil.
The value is (:backend NAME :model NAME :effort NAME), all strings.  A nil
effort means none was selected, so the provider's default applies.  Return
nil unless CONFIGURATION names both a backend and a model."
  (when (niwa-agent-configuration-p configuration)
    (let* ((locals (niwa-agent-configuration-request-locals configuration))
           (backend (alist-get 'gptel-backend locals))
           (model (alist-get 'gptel-model locals))
           (effort (alist-get 'gptel-reasoning-effort locals)))
      (when (and (gptel-backend-p backend) model)
        (list :backend (gptel-backend-name backend)
              :model (gptel--model-name model)
              :effort (and effort (format "%s" effort)))))))

(defun niwa-agent-model-metadata-p (value)
  "Return non-nil when VALUE has the provider metadata shape.
See `niwa-agent-configuration-model-metadata'."
  (and (proper-list-p value)
       (= (length value) 6)
       (stringp (plist-get value :backend))
       (stringp (plist-get value :model))
       (plist-member value :effort)
       (or (null (plist-get value :effort))
           (stringp (plist-get value :effort)))))

(defvar niwa-agent--registry nil
  "Alist mapping agent name strings to `niwa-agent' structs.")

(defconst niwa-agent-task-path-property "NIWA_AGENT_PATH"
  "Org property that identifies a retained agent's own task heading.")

(defvar niwa-agent--default
  (niwa-agent--create
   :name "default"
   :description "Agent inheriting its delegator's effective configuration.")
  "Default retained-agent role configuration.
Its context observations are inherited when it freezes; see
`niwa-agent--effective-context-components'.")

(defconst niwa-agent--root-only-components
  '(active-goal goal-policy journal)
  "Root context observations that no delegated agent inherits.
The Goal belongs to the root's continuation, and the journal map serves the
root's memory review rather than delegated work.")

(defun niwa-agent-default ()
  "Return the default inherited agent role."
  niwa-agent--default)

(defcustom niwa-agent-extra-tool-specs nil
  "Alist mapping agent names to extra tool specs.

Each entry is (AGENT-SYMBOL . SPEC-LIST) where SPEC-LIST uses the same
forms accepted by `niwa-define-agent''s :tools keyword (bare
symbols, (:group X), (:tool X), (:discoverable X)).

Merged into the role's resolved tool list before a retained agent freezes its
configuration. Active entries flow into the sub-agent's gptel tool set and
deferred entries seed the invocation's `tool-catalog' so ToolSearch running
inside the agent can discover them."
  :group 'niwa
  :type '(alist :key-type symbol :value-type (repeat sexp)))

(defconst niwa-agent--communication-tool-specs
  '((:tool "SendMessage") (:discoverable (:tool "ListAgents")))
  "Tools available to every named role.
SendMessage is native because agents routinely report to their parent.")

(defconst niwa-agent--control-tool-specs
  '((:discoverable (:tool "WaitAgent"))
    (:discoverable (:tool "InterruptAgent")))
  "Tools added to every named role that possesses Agent.")

(defun niwa-agent-default-native-tools (catalog tools)
  "Return inherited native TOOLS plus native communication tools in CATALOG.
A default child inherits its delegator's native tools and its permitted
specialist CATALOG.  A communication tool that named roles receive natively
becomes native here too, but only when the inherited catalog permits it."
  (cl-delete-duplicates
   (append tools
           (cl-loop for spec in niwa-agent--communication-tool-specs
                    for name = (and (eq (car spec) :tool) (cadr spec))
                    when (and name
                              (cl-some (lambda (entry)
                                         (equal (car entry) (list "niwa" name)))
                                       catalog))
                    collect (niwa-tool-gptel-tool (niwa-tool-ensure name))))
   :test #'eq :from-end t))

(defun niwa-agent--specs-contain-tool-p (specs name)
  "Return non-nil when resolved SPECS contain a tool named NAME."
  (when-let* ((resolved (ignore-errors (niwa-tool-resolve specs))))
    (cl-some (lambda (tool)
               (equal name (niwa-tool-name tool)))
             (append (plist-get resolved :active)
                     (plist-get resolved :discoverable)))))

(defun niwa-agent--declared-specs (agent)
  "Return AGENT's declared tool specs with user extras appended."
  (if (niwa-agent-frozen-p agent)
      (niwa-agent-tools agent)
    (let* ((name (niwa-agent-name agent))
           (extras
            (alist-get (intern name) niwa-agent-extra-tool-specs)))
      (append (niwa-agent-tools agent) extras))))

(defun niwa-agent--effective-specs (agent)
  "Return AGENT's tool specs merged with user-declared extras.

Combines `niwa-agent-tools' with any matching entry in
`niwa-agent-extra-tool-specs'.  The agent's own specs come first so
the built-in set stays stable; user extras are appended.  An unfrozen
default role also inherits the caller's effective specialist identities.
Freezing these explicit specs preserves membership on later invocations."
  (let* ((name (niwa-agent-name agent))
         (base (niwa-agent--declared-specs agent)))
    ;; Restored frozen specs still need their lazy tool groups registered.
    (niwa-tool-ensure "NoteWrite")
    (cond
     ((niwa-agent-frozen-p agent) base)
     ((equal name "default")
      ;; Preparation may finish from an async callback during another tool's
      ;; dispatch.  Inherit from the delegating buffer, like request locals,
      ;; not from that unrelated dynamic engine binding.
      (let* ((niwa-tools--current-engine nil)
             (owner (niwa-tools--current-context))
             (catalog (and owner (niwa-tools--ctx-tool-catalog owner)))
             ;; A later follow-up can leave Plan.  Freeze capability membership,
             ;; not this turn's transient visibility; dispatch filters each turn.
             (roster (and catalog (niwa-tool-ptc--roster t))))
        (append
         base
         (cl-loop for (path . _summary) in catalog
                  for tool = (niwa-tool-get (cadr path) (car path))
                  when (and tool (member (niwa-tool-call-name tool) roster))
                  collect (list :discoverable (list :tool (copy-tree path)))))))
     (t
      (cl-delete-duplicates
       (append base
               '((:tool "ToolSearch") (:tool "ToolCall") (:discoverable notes))
               niwa-agent--communication-tool-specs
               (when (niwa-agent--specs-contain-tool-p base "Agent")
                 niwa-agent--control-tool-specs))
       :test #'equal)))))

(defun niwa-agent-get (name)
  "Get the `niwa-agent' struct for NAME (symbol or string)."
  (alist-get (if (symbolp name) (symbol-name name) name)
             niwa-agent--registry nil nil #'equal))

(defun niwa-agent--effective-context-components (agent)
  "Return the context observations AGENT receives.
A named role selects its own.  The unfrozen default role inherits its
delegator's: a delegating agent's own selection, or the root's observations
except `niwa-agent--root-only-components'.  The default role inherits the
delegator's system prompt, whose changing parts arrive as these observations,
so without them it would lack workspace guidance and environment facts."
  (if (or (niwa-agent-frozen-p agent)
          (not (equal "default" (niwa-agent-name agent))))
      (niwa-agent-context-components agent)
    ;; Resolve the delegator from its buffer, as for inherited tool specs.
    (let* ((niwa-tools--current-engine nil)
           (owner (niwa-tools--current-context)))
      (if (niwa-agent-invocation-p owner)
          (copy-sequence
           (niwa-agent-context-components
            (niwa-agent-invocation-agent owner)))
        (cl-remove-if (lambda (name)
                        (memq name niwa-agent--root-only-components))
                      (niwa-system-context-components niwa--session))))))

(defun niwa-agent-resolve-role (role)
  "Resolve optional ROLE to an agent definition or signal a user error."
  (cond
   ((null role) (niwa-agent-default))
   ((not (and (stringp role) (not (string-empty-p role))))
    (user-error "Agent role must be a non-empty string when provided"))
   ((niwa-agent-get role))
   (t (user-error "Unknown agent role: %s" role))))

(defun niwa-agent-freeze (agent)
  "Return a frozen configuration snapshot of AGENT.

Dynamic system instructions, inherited context observations, and user tool
augmentation are materialized in the caller's current session context, so a
retained follow-up keeps them.  Reminders and hooks remain templates,
but are copied so later role redefinition cannot alter retained follow-ups.
Every reminder must have a durable recipe because retained agents survive a
cold session resume."
  (if (niwa-agent-frozen-p agent)
      agent
    (let ((frozen (copy-niwa-agent agent))
          (system-prompt (niwa-agent-system-prompt agent)))
      (niwa-reminders-serialize-agent-templates
       (niwa-agent-reminders agent))
      (setf (niwa-agent-tools frozen)
            (copy-tree (niwa-agent--effective-specs agent))
            (niwa-agent-context-components frozen)
            (niwa-agent--effective-context-components agent)
            (niwa-agent-system-prompt frozen)
            (cond
             ((stringp system-prompt) system-prompt)
             ((functionp system-prompt) (funcall system-prompt))
             ((null system-prompt) nil)
             (t (error "Agent %s has invalid system-prompt: %S"
                       (niwa-agent-name agent) system-prompt)))
            (niwa-agent-reminders frozen)
            (niwa-reminders-clone-list
             (niwa-agent-reminders agent))
            (niwa-agent-hook-rules frozen)
            (copy-tree (niwa-agent-hook-rules agent))
            (niwa-agent-frozen-p frozen) t)
      frozen)))

(defmacro niwa-define-agent (name &rest keys)
  "Define a niwa agent NAME with declarative KEYS.

KEYS is a plist with the following recognized keys:

  :description    STRING    -- agent description
  :tools          LIST      -- tool specs for `niwa-tool-resolve-gptel'
  :system-components LIST   -- ordered prompt profile components
  :max-turns      INTEGER   -- max conversation turns (nil = unlimited)
  :reminders      LIST      -- list of `niwa-reminder' structs used as
                               templates; cloned per invocation for
                               independent `last-fired' tracking
  :hooks          LIST      -- declarative hook rules scoped to this agent.
                               Local `Stop' entries are treated as
                               `SubagentStop'

Creates a `niwa-agent' struct and registers it in
`niwa-agent--registry'."
  (declare (indent 1))
  (unless (zerop (% (length keys) 2))
    (error "Agent definition keys must form a plist: %s" name))
  (let ((remaining keys))
    (while remaining
      (unless (memq (car remaining)
                    '(:description :tools :system-components
                      :max-turns :reminders :hooks))
        (error "Unknown agent definition key: %s" (car remaining)))
      (setq remaining (cddr remaining))))
  (let* ((name-str (symbol-name name))
         (system-components (plist-get keys :system-components))
         (tool-specs (plist-get keys :tools))
         (hooks (plist-get keys :hooks))
         (system-prompt-form
          (when system-components
            `(lambda ()
               (niwa-system-build-prompt
                (list :workspace-aware t
                      :components ,system-components)
                :retained t
                :workspace (and niwa--session
                                (niwa-session-workspace niwa--session))
                :working-directory
                (niwa-conversation-working-directory niwa--session)
                :session niwa--session
                :refresh-buffer (current-buffer))))))
    `(let ((agent (niwa-agent--create
                   :name ,name-str
                   :description ,(plist-get keys :description)
                   :tools ',tool-specs
                   :system-prompt ,system-prompt-form
                   :context-components
                   (cl-intersection ,system-components
                                    niwa-system-retained-components)
                   :max-turns ,(plist-get keys :max-turns)
                   :reminders ,(plist-get keys :reminders)
                   :hook-rules
                   ,(when hooks
                      `(progn
                         (niwa-hooks-annotate-rules-source
                          (niwa-hooks-normalize-rules ',hooks 'agent)
                          'user nil user-emacs-directory))))))
       (setf (alist-get ,name-str niwa-agent--registry nil nil #'equal)
             agent)
       agent)))

(cl-defstruct (niwa-agent-invocation
               (:constructor niwa-agent-invocation--create))
  "Per-invocation state for a spawned sub-agent.

Holds the cloned reminders list and a turn counter that are private to
this particular invocation of AGENT. Created fresh each time
`niwa-agent-runtime-dispatch' starts a turn, then stored both buffer-locally
and on the provider request so transforms, tools, and terminal callbacks can
find it throughout that turn.

The `deferred-*' slots mirror the same-named slots on `niwa-session'
and give the spawned agent its own deferred-tool lifecycle, independent
of the main chat's session state. The polymorphic accessors in
`niwa-tools.el' dispatch on struct type so the WAIT handler,
pipeline, and reminders can share one code path for both contexts.

Persistence slots. AGENT-ID is the internal join key in the parent session's
`agent-transcripts' alist and in the on-disk transcript filename.  PATH is the
canonical model-visible address of this retained conversation in the session
agent tree.
PARENT-DATA-BUFFER points back at the parent chat (data) buffer;
PARENT-SESSION points at the top-level session (transcripts always live
under the top-level session's `agents/' subdirectory). PARENT-TURN is
the in-flight parent turn number at allocation time, computed as
`niwa-current-turn' so it reflects
the current turn rather than the last completed one.  BUFFER is
the agent's own gptel buffer; TRANSCRIPT-RELATIVE-PATH is the
path to its on-disk transcript file relative to the top-level
session directory.  TRANSCRIPT-STATUS tracks running / completed
/ error / aborted; SIDECAR-DIRTY signals that a sidecar write
failed and should be retried at the next save point.  The save timer
and render-data markers are runtime-only caches for cheap live updates."
  (agent nil :type niwa-agent)
  (reminders nil :type list)
  (turn-count 0 :type integer)
  (tool-catalog nil :type list)
  (plan-read-only nil :type boolean)
  ;; Accepted implementation permission inherited by this invocation only.
  permission-mode-override
  ;; Persistence
  (agent-id nil :type (or null string))
  (path nil :type (or null string))
  (description nil :type (or null string))
  (parent-session nil)
  (working-directory nil :type (or null string))
  (parent-data-buffer nil)
  (parent-turn nil :type (or null integer))
  ;; Root engine owner whose Goal pays for this invocation's usage, or nil.
  (goal-owner nil)
  (buffer nil)
  (transcript-relative-path nil :type (or null string))
  (transcript-status nil :type (or null symbol))
  (sidecar-dirty nil :type boolean)
  ;; Why transcript setup or its latest save failed, for error reports.
  (persistence-failure nil :type (or null string))
  ;; Rules accumulate across nested skills (additive); model/effort are
  ;; last-writer-wins.  A fork starts with only the fork skill's own rules;
  ;; the parent's request rules never propagate to a child.
  (skill-permission-rules nil :type list)
  frozen-configuration
  ;; Skill-scoped selector, stored as (:tier TIER) or (:backend BACKEND :model
  ;; MODEL). The historical slot name is kept because request-scoped skill code
  ;; already uses the same terminology.
  skill-model-override
  skill-effort-override
  hook-rules
  ;; handle-state metadata for the badge. CALL-COUNT increments on each
  ;; gptel-pre-tool-call-functions firing within this invocation's buffer.
  ;; STARTED-AT is the wall-clock at allocation; the difference at completion
  ;; gives ELAPSED for the done badge. TERMINAL-REASON carries an error / abort
  ;; reason string for the error / aborted badges.
  (call-count 0 :type integer)
  (started-at nil)
  (terminal-reason nil :type (or null string))
  (verdict nil :type (or null symbol))
  (activity nil :type list)
  ;; Runtime-only caches for live agent updates.
  (hook-audits nil :type list)
  (transcript-save-timer nil)
  (render-data-start-marker nil)
  (render-data-end-marker nil)
  (parent-tool-use-id nil :type (or null string))
  (sandbox-summary-cell nil :type list)
  ;; Runtime-only provider-turn state.  Durable identity and unread mail live
  ;; on the root session's `niwa-agent-record'.
  ;; Nil for inspection/preparation handles that have not admitted a provider.
  (ownership-epoch nil :type (or null integer))
  runtime-fsm
  runtime-cancel
  (runtime-context nil :type list)
  runtime-settle-callback
  runtime-pending-response
  runtime-budget-timer
  (runtime-execution-results nil :type list)
  (runtime-settled-p nil :type boolean))

(cl-defmethod niwa-engine-info ((owner niwa-agent-invocation))
  (if-let* ((fsm (niwa-agent-invocation-runtime-fsm owner)))
      (niwa-engine-info fsm)
    (niwa-agent-invocation-runtime-context owner)))

(cl-defmethod (setf niwa-engine-info)
    (value (owner niwa-agent-invocation))
  (if-let* ((fsm (niwa-agent-invocation-runtime-fsm owner)))
      (setf (niwa-engine-info fsm) value)
    (setf (niwa-agent-invocation-runtime-context owner) value)))

(defun niwa-plan-read-only-request-p ()
  "Return non-nil when the ambient request or invocation is Plan read-only."
  (or (and (boundp 'niwa--current-request)
           niwa--current-request
           (niwa-request-plan-read-only niwa--current-request))
      (and niwa--agent-invocation
           (niwa-agent-invocation-plan-read-only
            niwa--agent-invocation))))

(defun niwa-plan-read-only-p (&optional session)
  "Return non-nil when the ambient caller works under Plan read-only authority.
That is either the immutable request or invocation stamp, or the live sticky
Plan flag of SESSION.  SESSION defaults to the current buffer's session, else
the ambient agent invocation's root session."
  (or (niwa-plan-read-only-request-p)
      (niwa-plan-mode-active-p
       (or session
           (and (boundp 'niwa--session) niwa--session)
           (and niwa--agent-invocation
                (niwa-agent-invocation-parent-session
                 niwa--agent-invocation))))))

(defun niwa-agent-invocation-require-path (invocation)
  "Return INVOCATION's canonical path, or signal an error."
  (let ((path (niwa-agent-invocation-path invocation)))
    (unless (niwa-agent-path-p path)
      (error "Agent invocation has no canonical path: %s"
             (niwa-agent-invocation-agent-id invocation)))
    path))

(defun niwa-agent-invocation-create (agent)
  "Create a fresh `niwa-agent-invocation' for AGENT.

The invocation gets an independent clone of AGENT's reminders so
`last-fired' tracking is isolated from other invocations.  When the agent
has a `max-turns' cap, a one-shot max-turns-warning reminder is
prepended automatically so the agent gets a single nudge near the turn
limit without any per-agent declaration.

The agent's `:tools' spec is resolved; the `:discoverable' portion seeds the
invocation's `tool-catalog' so ToolSearch retrieves the agent's own
specialist contracts for ToolCall.  Their native descriptions explain
discovery without adding an availability reminder."
  (niwa-tool-ensure "ToolSearch")
  (niwa-tool-ensure "ToolCall")
  (let* ((reminders (niwa-reminders-clone-list
                     (niwa-agent-reminders agent)))
         (resolved (niwa-tool-resolve (niwa-agent--effective-specs agent)))
         (deferred-tools (plist-get resolved :discoverable))
         (tool-catalog
          (mapcar (lambda (tool)
                    (cons (list (niwa-tool-category tool)
                                (niwa-tool-name tool))
                          ;; ToolSearch uses summaries for broad results.
                          (niwa-tool-summary tool)))
                  deferred-tools)))
    (when (niwa-agent-max-turns agent)
      (push (niwa-reminders-make-max-turns-warning) reminders))
    (niwa-agent-invocation--create
     :agent agent
     :permission-mode-override
     (if niwa--current-request
         (niwa-request-permission-mode-override niwa--current-request)
       (and niwa--agent-invocation
            (niwa-agent-invocation-permission-mode-override
             niwa--agent-invocation)))
     :reminders reminders
     :hook-rules (copy-sequence (niwa-agent-hook-rules agent))
     :turn-count 0
     :tool-catalog tool-catalog
     ;; stamp wall-clock at invocation creation so the completed-handle badge
     ;; can compute elapsed time.
     :started-at (current-time))))

(defun niwa-agent-to-gptel-spec (agent)
  "Convert `niwa-agent' AGENT to a gptel agent plist.

Returns a cons (NAME . PLIST) suitable for the request-local role roster."
  (let* ((tool-specs (niwa-agent--effective-specs agent))
         (sys-prompt (niwa-agent-system-prompt agent))
         (system-spec (cond
                       ((stringp sys-prompt) sys-prompt)
                       ((functionp sys-prompt)
                        (list :function
                              (lambda (_system) sys-prompt)))
                       (t (error "Agent %s has invalid system-prompt: %S"
                                 (niwa-agent-name agent) sys-prompt)))))
    (cons (niwa-agent-name agent)
          (list :description (niwa-agent-description agent)
                :tools `(:function
                         (lambda (_tools)
                           (cl-delete-duplicates
                            (plist-get
                             (niwa-tool-resolve-gptel ',tool-specs)
                             :active))))
                :system system-spec))))


;;
;;; Agent definitions

(niwa-define-agent worker
  :description "Implementation agent with broad repository tools and recursive
delegation authority."
  :tools ((:tool "Read") (:tool "Glob") (:tool "Grep")
          (:discoverable read)
          (:tool "ApplyPatch") (:discoverable edit) (:tool "Bash")
          (:tool "Eval")
          (:tool "WriteStdin")
          (:discoverable code)
          (:discoverable web)
          (:discoverable elisp)
          (:discoverable (:tool "ListExecutions"))
          (:discoverable (:tool "StopExecution"))
          (:discoverable (:tool "Ask"))
          (:discoverable (:tool "Skill"))
          (:discoverable (:tool "ListSkills"))
          (:discoverable (:tool "TaskWrite"))
          (:discoverable (:tool "TaskList"))
          (:discoverable (:tool "Agent")))
  :system-components
  '((role :file "agents/worker.md")
    task-policy
    report-tone
    memory-policy
    memory-save-policy
    tool-orchestration
    resources
    workspace-config
    memory
    environment
    skill-policy
    tools
    skills))

(niwa-define-agent explorer
  :description "Read-only exploration agent for codebase investigation and, when
needed, web research.  Caller specifies the thoroughness level
(quick/moderate/thorough) in the prompt.  Returns a structured report -- never
modifies files."
  :tools ((:tool "Read") (:tool "Glob") (:tool "Grep")
          (:discoverable read)
          (:discoverable code)
          (:discoverable web)
          (:discoverable elisp)
          (:discoverable (:tool "Ask"))
          (:discoverable (:tool "Skill"))
          (:discoverable (:tool "ListSkills"))
          (:discoverable (:tool "TaskWrite"))
          (:discoverable (:tool "TaskList"))
          (:discoverable (:tool "Agent")))
  :system-components
  '((role :file "agents/explorer.md")
    task-policy
    report-tone
    tool-orchestration
    resources
    workspace-config
    environment
    skill-policy
    tools
    skills))

(niwa-define-agent verifier
  :description "Adversarial verification specialist.  Read-only -- \
tries to break implementations through edge cases, tests, and code \
review.  Cannot edit, write, or create files."
  :tools ((:tool "Read") (:tool "Glob") (:tool "Grep")
          (:discoverable read) (:tool "Bash") (:tool "Eval") (:tool "WriteStdin")
          (:discoverable code)
          (:discoverable elisp)
          (:discoverable (:tool "Ask")))
  :system-components
  '((role :file "agents/verifier.md")
    task-policy
    report-tone
    tool-orchestration
    resources
    workspace-config
    environment
    tools))

(niwa-define-agent reviewer
  :description "Dedicated code review agent.  Read-only -- inspects diffs and \
returns prioritized structured findings as JSON."
  :tools ((:tool "Read") (:tool "Glob") (:tool "Grep")
          (:discoverable read) (:tool "Bash") (:tool "WriteStdin")
          (:discoverable code))
  :system-components
  '((role :file "agents/reviewer.md")
    task-policy
    tool-orchestration
    resources
    workspace-config
    environment
    tools))


;;
;;; Request-time agent setup

(defvar-local niwa-agents--specs nil
  "Request-local alist of available agent role specifications.")

(defun niwa-agents-specs (&optional buffer)
  "Return the agent role specifications visible in BUFFER.
BUFFER defaults to the current buffer."
  (buffer-local-value 'niwa-agents--specs
                      (or buffer (current-buffer))))

(defun niwa-agents-set-specs (specs)
  "Install request-local agent role SPECS in the current buffer."
  (setq-local niwa-agents--specs specs))

(defun niwa-agents--setup-for-request (&optional preset-name)
  "Set up agents for the current request.

If PRESET-NAME is non-nil and declares an `:agents' entry in
`niwa-preset--registry', only those agents are registered -- an
explicit empty declaration registers none.  A preset with no `:agents'
key at all registers the whole registry.

Populates the request-local role roster and installs its Agent schema.  Must
be called in the chat buffer."
  (let* ((meta (and preset-name
                    (niwa-preset--resolved-metadata preset-name)))
         (declared (plist-member meta :agents))
         (allowed-names (and declared
                             (mapcar #'symbol-name (cadr declared))))
         (niwa-specs
          (mapcar (lambda (entry)
                    (niwa-agent-to-gptel-spec (cdr entry)))
                  (if declared
                      (cl-remove-if-not
                       (lambda (entry) (member (car entry) allowed-names))
                       niwa-agent--registry)
                    niwa-agent--registry))))
    (niwa-agents-set-specs niwa-specs))
  (niwa-agents-install-agent-schema)
  nil)

(defun niwa-agents-agent-contract (args summary)
  "Return the Agent tool's (ARGS . SUMMARY) for the current buffer.
ARGS and SUMMARY are the registered specification.  The `role' and `model'
enums list this buffer's roster and model tiers, the `model' description
resolves each tier, and SUMMARY names the tier models.  The registered
specification is not edited."
  (cons
   (mapcar
    (lambda (arg)
      (pcase (car arg)
        ('role
         (niwa-agents--arg-with
          arg (nth 3 arg) (vconcat (mapcar #'car (niwa-agents-specs)))))
        ('model
         (niwa-agents--arg-with
          arg (niwa-model-agent-tool-description)
          (vconcat (mapcar (lambda (entry) (symbol-name (car entry)))
                           niwa-model-tiers))))
        (_ arg)))
    args)
   (concat summary " Model tiers: " (niwa-model-tier-guide t) ".")))

(defun niwa-agents--arg-with (arg description enum)
  "Return a copy of tool argument spec ARG with DESCRIPTION and ENUM.
ARG is (NAME TYPE REQUIREMENT DESCRIPTION [KEY VALUE]...); the first three
elements are kept and the trailing keys, such as `:enum', are copied."
  (append (seq-take arg 3) (list description)
          (plist-put (copy-sequence (nthcdr 4 arg)) :enum enum)))

(defun niwa-agents-install-agent-schema ()
  "Give the current buffer its own Agent schema for its role roster.
When `gptel-tools' offers the native Agent tool directly, replace it with a
copy of the registered schema whose role and model enums list this buffer's
roles and model tiers.  The registered tool is shared by every buffer, so it
is never edited.  A copy from an earlier call still resolves to the native
tool, so it is replaced rather than duplicated; a foreign tool of the same
name is left alone.  Other tools keep their order and identity.  Call this
once the buffer's `gptel-tools' are final; a discoverable Agent needs no
schema here."
  (when-let* ((native (niwa-tool-get "Agent" "niwa"))
              (registered (niwa-tool-gptel-tool native))
              (agent-p (lambda (tool)
                         (eq native (niwa-tool-for-gptel tool))))
              ((cl-some agent-p gptel-tools)))
    (let ((local (gptel--copy-tool registered))
          installed)
      (setf (gptel-tool-args local)
            ;; Only description and enum differ from the registered schema.
            (cl-mapcar
             (lambda (old fresh)
               (let ((arg (copy-sequence old)))
                 (dolist (key '(:description :enum) arg)
                   (when (plist-member fresh key)
                     (setq arg (plist-put arg key (plist-get fresh key)))))))
             (gptel-tool-args registered)
             (niwa-tool--args-to-gptel
              (car (niwa-tool-contextual-contract native)))))
      (setq-local gptel-tools
                  (delq nil
                        (mapcar (lambda (tool)
                                  (cond ((not (funcall agent-p tool)) tool)
                                        (installed nil)
                                        (t (setq installed local))))
                                gptel-tools))))))

(provide 'niwa-agents)
;;; niwa-agents.el ends here
