;;; mevedel-agents.el -- Agent definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative definitions for the specialised sub-agents that mevedel spawns
;; through the Agent tool: `worker' (implementation), `explorer' (read-only
;; investigation), `verifier' (adversarial read-only verification), and
;; `reviewer' (structured code review).  Uses the `mevedel-define-agent' macro
;; to bundle tool groups, prompt files, turn limits, and reminders.
;;
;; Per-turn state (cloned reminders and deferred-tool lifecycle)
;; lives on `mevedel-agent-invocation' structs created at dispatch time rather
;; than on the agent definition itself.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'mevedel-tool-registry)
(require 'mevedel-models)

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(declare-function gptel-get-tool "ext:gptel" (name))
(defvar gptel-post-tool-call-functions)

;; `gptel-request'
(declare-function gptel-tool-args "ext:gptel-request" (cl-x) t)

;; `mevedel-agent-exec'
(defvar mevedel-agent-exec--agents)

;; `mevedel-hooks'
(declare-function mevedel-hooks-normalize-rules
                  "mevedel-hooks" (rules &optional scope))

;; `mevedel-models'
(declare-function mevedel-model-agent-tool-description "mevedel-models" ())
(defvar mevedel-model-tiers)

;; `mevedel-presets'
(declare-function mevedel-preset--resolved-metadata
                  "mevedel-presets" (name))

;; `mevedel-reminders'
(declare-function mevedel-reminders-clone-list "mevedel-reminders" (reminders))
(declare-function mevedel-reminders-make-agent-deferred-tools-expired
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-agent-deferred-tools-roster
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-max-turns-warning "mevedel-reminders"
                  (&optional threshold))
(declare-function mevedel-reminders-make-reviewer-read-only
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-verifier-read-only
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-serialize-agent-templates
                  "mevedel-reminders" (reminders))

;; `mevedel-structs'
(declare-function mevedel-agent-path-p "mevedel-structs" (path))
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-build-agent-prompt
                  "mevedel-system"
                  (base-prompt &rest keys))
(declare-function mevedel-system-render-agent-prompt-file
                  "mevedel-system" (relative-path &optional replacements))
(defvar mevedel-system--tone-prompt)


;;
;;; Agent struct and registry

(cl-defstruct (mevedel-agent (:constructor mevedel-agent--create))
  "Agent definition for mevedel sub-agents."
  (name nil :type string)
  (description nil :type string)
  (tools nil :type list)
  (system-prompt nil :type (or string function))
  (max-turns nil :type (or null integer))
  (reminders nil :type list)
  (hook-rules nil :type list)
  (frozen-p nil :type boolean))

(cl-defstruct
    (mevedel-agent-configuration
     (:constructor mevedel-agent-configuration--create))
  "Frozen identity and request state for one retained agent."
  (agent nil :type mevedel-agent)
  (request-locals nil :type list))

(defvar mevedel-agent--registry nil
  "Alist mapping agent name strings to `mevedel-agent' structs.")

(defconst mevedel-agent-task-path-property "MEVEDEL_AGENT_PATH"
  "Org property that identifies a retained agent's own task heading.")

(defvar mevedel-agent--default
  (mevedel-agent--create
   :name "default"
   :description "Agent inheriting its delegator's effective configuration.")
  "Default retained-agent role configuration.")

(defun mevedel-agent-default ()
  "Return the default inherited agent role."
  mevedel-agent--default)

(defcustom mevedel-agent-extra-tool-specs nil
  "Alist mapping agent names to extra tool specs.

Each entry is (AGENT-SYMBOL . SPEC-LIST) where SPEC-LIST uses the same
forms accepted by `mevedel-define-agent''s :tools keyword (bare
symbols, (:group X), (:tool X), (:deferred X)).

Merged into the role's resolved tool list before a retained agent freezes its
configuration. Active entries flow into the sub-agent's gptel tool set and
deferred entries seed the invocation's `deferred-set' so ToolSearch running
inside the agent can discover them."
  :group 'mevedel
  :type '(alist :key-type symbol :value-type (repeat sexp)))

(defconst mevedel-agent--communication-tool-specs
  '((:tool "SendMessage") (:tool "ListAgents"))
  "Tools available to every named role.")

(defconst mevedel-agent--control-tool-specs
  '((:tool "FollowupAgent") (:tool "WaitAgent")
    (:tool "InterruptAgent"))
  "Tools added to every named role that possesses Agent.")

(defun mevedel-agent--specs-contain-tool-p (specs name)
  "Return non-nil when resolved SPECS contain a tool named NAME."
  (when-let* ((resolved (ignore-errors (mevedel-tool-resolve specs))))
    (cl-some (lambda (tool)
               (equal name (mevedel-tool-name tool)))
             (plist-get resolved :active))))

(defun mevedel-agent--declared-specs (agent)
  "Return AGENT's declared tool specs with user extras appended."
  (if (mevedel-agent-frozen-p agent)
      (mevedel-agent-tools agent)
    (let* ((name (mevedel-agent-name agent))
           (extras
            (alist-get (intern name) mevedel-agent-extra-tool-specs)))
      (append (mevedel-agent-tools agent) extras))))

(defun mevedel-agent--effective-specs (agent)
  "Return AGENT's tool specs merged with user-declared extras.

Combines `mevedel-agent-tools' with any matching entry in
`mevedel-agent-extra-tool-specs'.  The agent's own specs come first so
the built-in set stays stable; user extras are appended."
  (let* ((name (mevedel-agent-name agent))
         (base (mevedel-agent--declared-specs agent)))
    (if (or (mevedel-agent-frozen-p agent)
            (equal name "default"))
        base
      (cl-delete-duplicates
       (append base
               mevedel-agent--communication-tool-specs
               (when (mevedel-agent--specs-contain-tool-p base "Agent")
                 mevedel-agent--control-tool-specs))
       :test #'equal))))

(defun mevedel-agent-get (name)
  "Get the `mevedel-agent' struct for NAME (symbol or string)."
  (alist-get (if (symbolp name) (symbol-name name) name)
             mevedel-agent--registry nil nil #'equal))

(defun mevedel-agent-resolve-role (role)
  "Resolve optional ROLE to an agent definition or signal a user error."
  (cond
   ((null role) (mevedel-agent-default))
   ((not (and (stringp role) (not (string-empty-p role))))
    (user-error "Agent role must be a non-empty string when provided"))
   ((mevedel-agent-get role))
   (t (user-error "Unknown agent role: %s" role))))

(defun mevedel-agent-freeze (agent)
  "Return a frozen configuration snapshot of AGENT.

Dynamic system instructions and user tool augmentation are materialized in
the caller's current session context.  Reminders and hooks remain templates,
but are copied so later role redefinition cannot alter retained follow-ups.
Every reminder must have a durable recipe because retained agents survive a
cold session resume."
  (require 'mevedel-reminders)
  (if (mevedel-agent-frozen-p agent)
      agent
    (let ((frozen (copy-mevedel-agent agent))
          (system-prompt (mevedel-agent-system-prompt agent)))
      (mevedel-reminders-serialize-agent-templates
       (mevedel-agent-reminders agent))
      (setf (mevedel-agent-tools frozen)
            (copy-tree (mevedel-agent--effective-specs agent))
            (mevedel-agent-system-prompt frozen)
            (cond
             ((stringp system-prompt) system-prompt)
             ((functionp system-prompt) (funcall system-prompt))
             ((null system-prompt) nil)
             (t (error "Agent %s has invalid system-prompt: %S"
                       (mevedel-agent-name agent) system-prompt)))
            (mevedel-agent-reminders frozen)
            (mevedel-reminders-clone-list
             (mevedel-agent-reminders agent))
            (mevedel-agent-hook-rules frozen)
            (copy-tree (mevedel-agent-hook-rules agent))
            (mevedel-agent-frozen-p frozen) t)
      frozen)))

(defun mevedel-agent-skill-tool-capable-p (agent)
  "Return non-nil when AGENT's resolved active tools include skills."
  (when agent
    (let* ((specs (mevedel-agent--declared-specs agent))
           (resolved
            (ignore-errors (mevedel-tool-resolve specs))))
      (cl-some (lambda (tool)
                 (member (mevedel-tool-name tool)
                         '("Skill" "ListSkills")))
               (plist-get resolved :active)))))

(defmacro mevedel-define-agent (name &rest keys)
  "Define a mevedel agent NAME with declarative KEYS.

KEYS is a plist with the following recognized keys:

  :description    STRING    -- agent description
  :tools          LIST      -- tool specs for `mevedel-tool-resolve-gptel'
  :system-prompt  FUNCTION  -- function returning system prompt string
  :prompt-file    STRING    -- load prompt body from file (relative to
                               mevedel source dir).  Mutually exclusive
                               with `:system-prompt'.  Template expansion
                               uses gptel-agent's `{{VAR}}' infrastructure
                               at runtime, and the result is passed through
                               `mevedel-system-build-agent-prompt'.
  :max-turns      INTEGER   -- max conversation turns (nil = unlimited)
  :reminders      LIST      -- list of `mevedel-reminder' structs used as
                               templates; cloned per invocation for
                               independent `last-fired' tracking
  :hooks          LIST      -- declarative hook rules scoped to this agent.
                               Local `Stop' entries are treated as
                               `SubagentStop'
  :include-workspace-config BOOLEAN -- include AGENTS.md
  :include-memory BOOLEAN -- include configured memory indexes
  :include-environment BOOLEAN -- include environment section

Creates a `mevedel-agent' struct and registers it in
`mevedel-agent--registry'."
  (declare (indent 1))
  (let* ((name-str (symbol-name name))
         (prompt-file (plist-get keys :prompt-file))
         (explicit-sp (plist-get keys :system-prompt))
         (tool-specs (plist-get keys :tools))
         (_ (when (and prompt-file explicit-sp)
              (error "Cannot combine :prompt-file and :system-prompt for agent %s"
                     name-str)))
         (_ (when prompt-file
              (let ((path (expand-file-name
                           prompt-file
                           mevedel-tool-registry--source-dir)))
                (unless (file-exists-p path)
                  (error "Agent prompt file not found: %s" path)))))
         (include-workspace-config
          (if (plist-member keys :include-workspace-config)
              (plist-get keys :include-workspace-config)
            t))
         (include-memory
          (if (plist-member keys :include-memory)
              (plist-get keys :include-memory)
            t))
         (include-environment
          (if (plist-member keys :include-environment)
              (plist-get keys :include-environment)
            t))
         (hooks (plist-get keys :hooks))
         (system-prompt-form
          (cond
           (prompt-file
            `(lambda ()
               (require 'mevedel-system)
               (mevedel-system-build-agent-prompt
                (mevedel-system-render-agent-prompt-file
                 ,prompt-file
                 (list (cons "TONE_PROMPT"
                             mevedel-system--tone-prompt)))
                :workspace-config ,include-workspace-config
                :memory ,include-memory
                :environment ,include-environment
                :workspace (and mevedel--session
                                (mevedel-session-workspace mevedel--session))
                :working-directory
                (and mevedel--session
                     (mevedel-session-working-directory mevedel--session))
                :session mevedel--session
                :refresh-buffer (current-buffer)
                :skills (mevedel-agent-skill-tool-capable-p
                         (mevedel-agent-get ,name-str)))))
           (explicit-sp
            `(lambda ()
               (let ((prompt ,explicit-sp))
                 (let ((body (cond
                              ((functionp prompt) (funcall prompt))
                              ((stringp prompt) prompt)
                              (t (error "Invalid agent system prompt: %S"
                                        prompt)))))
                   (if (mevedel-agent-skill-tool-capable-p
                        (mevedel-agent-get ,name-str))
                       (progn
                         (require 'mevedel-system)
                         (mevedel-system-build-agent-prompt
                          body
                          :workspace-config ,include-workspace-config
                          :memory ,include-memory
                          :environment ,include-environment
                          :workspace
                          (and mevedel--session
                               (mevedel-session-workspace mevedel--session))
                          :working-directory
                          (and mevedel--session
                               (mevedel-session-working-directory
                                mevedel--session))
                          :session mevedel--session
                          :refresh-buffer (current-buffer)
                          :skills t))
                     body)))))
           (t explicit-sp))))
    `(let ((agent (mevedel-agent--create
                   :name ,name-str
                   :description ,(plist-get keys :description)
                   :tools ',tool-specs
                   :system-prompt ,system-prompt-form
                   :max-turns ,(plist-get keys :max-turns)
                   :reminders ,(plist-get keys :reminders)
                   :hook-rules
                   ,(when hooks
                      `(progn
                         (require 'mevedel-hooks)
                         (mevedel-hooks-normalize-rules ',hooks 'agent))))))
       (setf (alist-get ,name-str mevedel-agent--registry nil nil #'equal)
             agent)
       agent)))

(cl-defstruct (mevedel-agent-invocation
               (:constructor mevedel-agent-invocation--create))
  "Per-invocation state for a spawned sub-agent.

Holds the cloned reminders list and a turn counter that are private to
this particular invocation of AGENT. Created fresh each time
`mevedel-agent-runtime-dispatch' starts a turn, then stored both buffer-locally
and on the provider request so transforms, tools, and terminal callbacks can
find it throughout that turn.

The `deferred-*' slots mirror the same-named slots on `mevedel-session'
and give the spawned agent its own deferred-tool lifecycle, independent
of the main chat's session state. The polymorphic accessors in
`mevedel-tools.el' dispatch on struct type so the WAIT handler,
pipeline, and reminders can share one code path for both contexts.

Persistence slots. AGENT-ID is the internal join key in the parent session's
`agent-transcripts' alist and in the on-disk transcript filename.  PATH is the
canonical model-visible address of this retained conversation in the session
agent tree.
PARENT-DATA-BUFFER points back at the parent chat (data) buffer;
PARENT-SESSION points at the top-level session (transcripts always live
under the top-level session's `agents/' subdirectory). PARENT-TURN is
the in-flight parent turn number at allocation time, computed as
`(1+ (mevedel-session-turn-count parent-session))' so it reflects
the current turn rather than the last completed one.  BUFFER is
the agent's own gptel buffer; TRANSCRIPT-RELATIVE-PATH is the
path to its on-disk transcript file relative to the top-level
session directory.  TRANSCRIPT-STATUS tracks running / completed
/ error / aborted; SIDECAR-DIRTY signals that a sidecar write
failed and should be retried at the next save point.  The save timer
and render-data markers are runtime-only caches for cheap live updates."
  (agent nil :type mevedel-agent)
  (reminders nil :type list)
  (turn-count 0 :type integer)
  (deferred-set nil :type list)
  (deferred-pending nil :type list)
  (deferred-injected nil :type list)
  (deferred-used nil :type list)
  (deferred-expired nil :type list)
  (specialist-nudge-state nil :type list)
  ;; Persistence
  (agent-id nil :type (or null string))
  (path nil :type (or null string))
  (description nil :type (or null string))
  (parent-session nil)
  (parent-data-buffer nil)
  (parent-turn nil :type (or null integer))
  (buffer nil)
  (transcript-relative-path nil :type (or null string))
  (transcript-status nil :type (or null symbol))
  (sidecar-dirty nil :type boolean)
  ;; Rules accumulate across nested skills (additive); model/effort are
  ;; last-writer-wins. Forks are seeded from
  ;; parent's currently active rules + the fork skill's own rules at spawn time;
  ;; later additions on either side do not propagate.
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
  ;; Runtime-only provider-turn state.  Durable identity and unread mail live
  ;; on the root session's `mevedel-agent-record'.
  runtime-fsm
  runtime-settle-callback
  runtime-pending-response
  (runtime-execution-results nil :type list)
  (runtime-settled-p nil :type boolean))

(defun mevedel-agent-invocation-require-path (invocation)
  "Return INVOCATION's canonical path, or signal an error."
  (require 'mevedel-structs)
  (let ((path (mevedel-agent-invocation-path invocation)))
    (unless (mevedel-agent-path-p path)
      (error "Agent invocation has no canonical path: %s"
             (mevedel-agent-invocation-agent-id invocation)))
    path))

(defun mevedel-agent-invocation-set-specialist-nudge-state (invocation state)
  "Set INVOCATION's specialist nudge STATE."
  (setf (mevedel-agent-invocation-specialist-nudge-state invocation) state))

(defun mevedel-agent-invocation-set-deferred-expired (invocation value)
  "Set INVOCATION's deferred-expired slot to VALUE."
  (setf (mevedel-agent-invocation-deferred-expired invocation) value))

(defun mevedel-agent-invocation-set-activity (invocation activity)
  "Set INVOCATION's ACTIVITY list."
  (setf (mevedel-agent-invocation-activity invocation) activity))

(defun mevedel-agent-invocation-create (agent)
  "Create a fresh `mevedel-agent-invocation' for AGENT.

The invocation gets an independent clone of AGENT's reminders so
`last-fired' tracking is isolated from other invocations.  When the agent
has a `max-turns' cap, a one-shot max-turns-warning reminder is
prepended automatically so the agent gets a single nudge near the turn
limit without any per-agent declaration.

The agent's `:tools' spec is resolved; the `:deferred' portion seeds the
invocation's `deferred-set' so ToolSearch running inside the spawned
sub-agent can discover the agent's own lazy tools.  When the deferred set
is non-empty, invocation-scoped roster and expiry reminders are added so
the sub-agent learns which tools it can activate without polluting the
main session's reminder list."
  (require 'mevedel-reminders)
  (let* ((reminders (mevedel-reminders-clone-list
                     (mevedel-agent-reminders agent)))
         (resolved (mevedel-tool-resolve (mevedel-agent--declared-specs agent)))
         (deferred-tools (plist-get resolved :deferred))
         (deferred-set
          (mapcar (lambda (tool)
                    (cons (list (mevedel-tool-category tool)
                                (mevedel-tool-name tool))
                          ;; Tool-supplied one-liner if any; nil means the
                          ;; roster reminder lists just the name. Full
                          ;; descriptions are too long for the roster (some
                          ;; wrapped tools carry multi-paragraph docstrings).
                          (mevedel-tool-summary tool)))
                  deferred-tools)))
    (when (mevedel-agent-max-turns agent)
      (push (mevedel-reminders-make-max-turns-warning) reminders))
    (when (equal (mevedel-agent-name agent) "verifier")
      (push (mevedel-reminders-make-verifier-read-only) reminders))
    (when (equal (mevedel-agent-name agent) "reviewer")
      (push (mevedel-reminders-make-reviewer-read-only) reminders))
    (when deferred-set
      (push (mevedel-reminders-make-agent-deferred-tools-expired) reminders)
      (push (mevedel-reminders-make-agent-deferred-tools-roster) reminders))
    (mevedel-agent-invocation--create
     :agent agent
     :reminders reminders
     :hook-rules (copy-sequence (mevedel-agent-hook-rules agent))
     :turn-count 0
     :deferred-set deferred-set
     ;; stamp wall-clock at invocation creation so the completed-handle badge
     ;; can compute elapsed time.
     :started-at (current-time))))

(defun mevedel-agent-to-gptel-spec (agent)
  "Convert `mevedel-agent' AGENT to a gptel agent plist.

Returns a cons (NAME . PLIST) suitable for `mevedel-agent-exec--agents'."
  (let* ((tool-specs (mevedel-agent--effective-specs agent))
         (sys-prompt (mevedel-agent-system-prompt agent))
         (system-spec (cond
                       ((stringp sys-prompt) sys-prompt)
                       ((functionp sys-prompt)
                        `(:function
                          (lambda (_system)
                            (funcall ,sys-prompt))))
                       (t (error "Agent %s has invalid system-prompt: %S"
                                 (mevedel-agent-name agent) sys-prompt)))))
    (cons (mevedel-agent-name agent)
          (list :description (mevedel-agent-description agent)
                :tools `(:function
                         (lambda (_tools)
                           (cl-delete-duplicates
                            (plist-get (mevedel-tool-resolve-gptel ',tool-specs)
                                       :active))))
                :system system-spec))))


;;
;;; Agent definitions

(mevedel-define-agent worker
  :description "Implementation agent with broad repository tools and recursive
delegation authority."
  :tools (read edit code eval
          (:tool "Ask")
          (:tool "Skill") (:tool "ListSkills")
          (:tool "ToolSearch")
          (:tool "TaskCreate") (:tool "TaskUpdate")
          (:tool "TaskList") (:tool "TaskGet") (:tool "TaskNote")
          (:tool "Agent")
          (:deferred web)
          (:deferred elisp))
  :prompt-file "agents/worker.md"
  :max-turns 50)

(mevedel-define-agent explorer
  :description "Read-only exploration agent for codebase investigation and, when
needed, web research.  Caller specifies the thoroughness level
(quick/moderate/thorough) in the prompt.  Returns a structured report -- never
modifies files."
  :tools (read
          (:tool "Ask")
          (:tool "Skill") (:tool "ListSkills")
          (:tool "ToolSearch")
          (:tool "TaskCreate") (:tool "TaskUpdate")
          (:tool "TaskList") (:tool "TaskGet") (:tool "TaskNote")
          (:tool "Agent")
          (:deferred code)
          (:deferred web)
          (:deferred elisp))
  :prompt-file "agents/explorer.md"
  :include-workspace-config nil
  :include-memory nil
  :max-turns 30)

(defun mevedel-agents--register-verifier ()
  "Register and return the bundled verifier agent."
  (mevedel-define-agent verifier
    :description "Adversarial verification specialist.  Read-only -- \
tries to break implementations through edge cases, tests, and code \
review.  Cannot edit, write, or create files."
    :tools (read code
            (:tool "Bash") (:tool "Eval")
            (:tool "Ask")
            (:tool "ToolSearch")
            (:deferred elisp))
    :prompt-file "agents/verifier.md"
    :include-memory nil
    :max-turns 20))

(mevedel-agents--register-verifier)

(defun mevedel-agents--register-reviewer ()
  "Register and return the bundled reviewer agent."
  (mevedel-define-agent reviewer
    :description "Dedicated code review agent.  Read-only -- inspects diffs and \
returns prioritized structured findings as JSON."
    :tools (read code (:tool "Bash"))
    :prompt-file "agents/reviewer.md"
    :include-memory nil
    :max-turns 12))

(mevedel-agents--register-reviewer)

(defun mevedel-agents-ensure-reviewer ()
  "Ensure the bundled reviewer agent is registered."
  (unless (mevedel-agent-get "reviewer")
    (mevedel-agents--register-reviewer)))

(defun mevedel-agents-ensure-verifier ()
  "Ensure the bundled verifier agent is registered."
  (unless (mevedel-agent-get "verifier")
    (mevedel-agents--register-verifier)))


;;
;;; Request-time agent setup

(defun mevedel-agents--setup-for-request (&optional preset-name)
  "Set up agents for the current request.

If PRESET-NAME is non-nil and has an `:agents' entry in
`mevedel-preset--registry', only those agents are registered.  Otherwise
all agents in `mevedel-agent--registry' are registered.

Populates the buffer-local `mevedel-agent-exec--agents' and updates the
Agent tool's role and model enums.  Must be called in the chat buffer."
  (let* ((meta (and preset-name
                    (mevedel-preset--resolved-metadata preset-name)))
         (allowed (plist-get meta :agents))
         (allowed-names (and allowed (mapcar #'symbol-name allowed)))
         (mevedel-specs
          (mapcar (lambda (entry)
                    (mevedel-agent-to-gptel-spec (cdr entry)))
                  (if allowed-names
                      (cl-remove-if-not
                       (lambda (entry) (member (car entry) allowed-names))
                       mevedel-agent--registry)
                    mevedel-agent--registry))))
    (setq-local mevedel-agent-exec--agents mevedel-specs))
  ;; Update Agent tool enums to list available role and model names.
  (when-let* ((agent-tool (gptel-get-tool '("mevedel" "Agent")))
              (args (gptel-tool-args agent-tool)))
    (when-let* ((role-arg
                 (cl-find-if (lambda (arg)
                               (equal (plist-get arg :name) "role"))
                             args)))
      (setf (plist-get role-arg :enum)
            (vconcat (mapcar #'car mevedel-agent-exec--agents))))
    (when-let* ((model-arg
                 (cl-find-if (lambda (arg)
                               (equal (plist-get arg :name) "model"))
                             args)))
      (setf (plist-get model-arg :enum)
            (vconcat (mapcar (lambda (entry)
                               (symbol-name (car entry)))
                             mevedel-model-tiers)))
      (setf (plist-get model-arg :description)
            (mevedel-model-agent-tool-description))))
  nil)

(provide 'mevedel-agents)
;;; mevedel-agents.el ends here
