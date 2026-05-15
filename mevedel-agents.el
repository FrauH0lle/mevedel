;;; mevedel-agents.el -- Agent definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative definitions for the specialised sub-agents that mevedel
;; spawns through the Agent tool: `explorer' (read-only investigation),
;; `verifier' (adversarial read-only verification), `reviewer'
;; (structured code review), and `coordinator' (orchestration agent
;; that dispatches background workers).  Uses the
;; `mevedel-define-agent' macro to bundle tool groups, prompt files,
;; turn limits, and reminders.
;;
;; Per-invocation state (cloned reminders, deferred-tool lifecycle,
;; mailbox) lives on `mevedel-agent-invocation' structs created at
;; dispatch time rather than on the agent definition itself.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-tool-registry)
(require 'mevedel-models)

;; `gptel'
(declare-function gptel-get-tool "ext:gptel" (name))

;; `gptel-request'
(declare-function gptel-tool-args "ext:gptel-request" (cl-x) t)

;; `mevedel-agent-exec'
(defvar mevedel-agent-exec--agents)

;; `mevedel-models'
(declare-function mevedel-model-agent-tool-description "mevedel-models" ())

;; `mevedel-hooks'
(declare-function mevedel-hooks-normalize-rules
                  "mevedel-hooks" (rules &optional scope))


;; `mevedel-reminders'
(declare-function mevedel-reminders-clone-list "mevedel-reminders" (reminders))
(declare-function mevedel-reminders-make-max-turns-warning "mevedel-reminders"
                  (&optional threshold))
(declare-function mevedel-reminders-make-agent-deferred-tools-roster
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-agent-deferred-tools-expired
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-verifier-read-only
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-agent-background-channels
                  "mevedel-reminders" ())

;; `mevedel-system'
(defvar mevedel-system--tone-prompt)
(declare-function mevedel-system-render-agent-prompt-file
                  "mevedel-system" (relative-path &optional replacements))
(declare-function mevedel-system-build-prompt
                  "mevedel-system"
                  (base-prompt &optional workspace working-directory))
(declare-function mevedel-system-build-agent-prompt
                  "mevedel-system"
                  (base-prompt &rest keys))

;; `mevedel-presets'
(defvar mevedel-preset--registry)

;; `gptel'
(defvar gptel-post-tool-call-functions)


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
  (include-workspace-config t :type boolean)
  (include-memory t :type boolean)
  (include-environment t :type boolean))

(defvar mevedel-agent--registry nil
  "Alist mapping agent name strings to `mevedel-agent' structs.")

(defcustom mevedel-agent-extra-tool-specs nil
  "Alist mapping agent names to extra tool specs.

Each entry is (AGENT-SYMBOL . SPEC-LIST) where SPEC-LIST uses the
same forms accepted by `mevedel-define-agent''s :tools keyword
(bare symbols, (:group X), (:tool X), (:deferred X)).

Merged into the agent's resolved tool list at every invocation --
active entries flow into the sub-agent's gptel tool set and
deferred entries seed the invocation's `deferred-set' so
ToolSearch running inside the agent can discover them."
  :group 'mevedel
  :type '(alist :key-type symbol :value-type (repeat sexp)))

(defun mevedel-agent--effective-specs (agent)
  "Return AGENT's tool specs merged with user-declared extras.

Combines `mevedel-agent-tools' with any matching entry in
`mevedel-agent-extra-tool-specs'.  The agent's own specs come first
so the built-in set stays stable; user extras are appended."
  (let* ((name (mevedel-agent-name agent))
         (sym (intern name))
         (extras (alist-get sym mevedel-agent-extra-tool-specs)))
    (if extras
        (append (mevedel-agent-tools agent) extras)
      (mevedel-agent-tools agent))))

(defun mevedel-agent-get (name)
  "Get the `mevedel-agent' struct for NAME (symbol or string)."
  (alist-get (if (symbolp name) (symbol-name name) name)
             mevedel-agent--registry nil nil #'equal))

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
  :include-workspace-config BOOLEAN -- include AGENTS.md/CLAUDE.md
  :include-memory BOOLEAN -- include `.mevedel/memory/MEMORY.md'
  :include-environment BOOLEAN -- include environment section

Creates a `mevedel-agent' struct and registers it in
`mevedel-agent--registry'."
  (declare (indent 1))
  (let* ((name-str (symbol-name name))
         (prompt-file (plist-get keys :prompt-file))
         (explicit-sp (plist-get keys :system-prompt))
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
                :environment ,include-environment)))
           (t explicit-sp))))
    `(let ((agent (mevedel-agent--create
                   :name ,name-str
                   :description ,(plist-get keys :description)
                   :tools ',(plist-get keys :tools)
                   :system-prompt ,system-prompt-form
                   :max-turns ,(plist-get keys :max-turns)
                   :reminders ,(plist-get keys :reminders)
                   :hook-rules
                   ,(when hooks
                      `(progn
                         (require 'mevedel-hooks)
                         (mevedel-hooks-normalize-rules ',hooks 'agent)))
                   :include-workspace-config ,include-workspace-config
                   :include-memory ,include-memory
                   :include-environment ,include-environment)))
       (setf (alist-get ,name-str mevedel-agent--registry nil nil #'equal)
             agent)
       agent)))

(cl-defstruct (mevedel-agent-invocation
               (:constructor mevedel-agent-invocation--create))
  "Per-invocation state for a spawned sub-agent.

Holds the cloned reminders list and a turn counter that are private
to this particular invocation of AGENT.  Created fresh each time
`mevedel-tools--task' spawns an agent, attached to the agent's task
overlay so the reminders transform and terminal FSM handler can find
it without relying on FSM info survival across `gptel-request'.

The `deferred-*' slots mirror the same-named slots on
`mevedel-session' and give the spawned agent its own deferred-tool
lifecycle, independent of the main chat's session state.  The
polymorphic accessors in `mevedel-tools.el' dispatch on struct type so
the WAIT handler, pipeline, and reminders can share one code path for
both contexts.

Persistence slots.  AGENT-ID is the stable identifier
used as the join key in the parent session's
`agent-transcripts' alist, in `mevedel-tools--agents-fsm', and in
the on-disk transcript filename.  PARENT-DATA-BUFFER points back
at the parent chat (data) buffer; PARENT-SESSION points at the
top-level session (transcripts always live under the top-level
session's `agents/' subdirectory).  PARENT-TURN is the in-flight
parent turn number at allocation time, computed as
`(1+ (mevedel-session-turn-count parent-session))' so it reflects
the current turn rather than the last completed one.  BUFFER is
the agent's own gptel buffer; TRANSCRIPT-RELATIVE-PATH is the
path to its on-disk transcript file relative to the top-level
session directory.  TRANSCRIPT-STATUS tracks running / completed
/ error / aborted; SIDECAR-DIRTY signals that a sidecar write
failed and should be retried at the next save point."
  (agent nil :type mevedel-agent)
  (reminders nil :type list)
  (turn-count 0 :type integer)
  (deferred-set nil :type list)
  (deferred-pending nil :type list)
  (deferred-injected nil :type list)
  (deferred-used nil :type list)
  (deferred-expired nil :type list)
  (messages nil :type list)
  (background-agents nil :type list)
  ;; Persistence
  (agent-id nil :type (or null string))
  (description nil :type (or null string))
  (parent-session nil)
  (parent-data-buffer nil)
  (parent-turn nil :type (or null integer))
  (buffer nil)
  (transcript-relative-path nil :type (or null string))
  (transcript-status nil :type (or null symbol))
  (sidecar-dirty nil :type boolean)
  ;; Set by `mevedel-tools--task--dispatch' from its BACKGROUND
  ;; argument.  When non-nil, the agent runs concurrently with its
  ;; caller; the dispatcher injects `SendMessage' into the agent
  ;; buffer's `gptel-tools' so the agent can communicate through the
  ;; route matrix enforced by `mevedel-tools--resolve-recipient'.
  ;; Foreground sub-agents
  ;; have their caller parked in TOOL state -- live messaging is
  ;; pointless there, so SendMessage is not injected.
  (background-p nil :type boolean)
  ;; spec Request-Scoped Skill Context: rules accumulate across
  ;; nested skills (additive); model/effort are last-writer-wins.
  ;; Forks are seeded from parent's currently active rules + the
  ;; fork skill's own rules at spawn time; later additions on either
  ;; side do not propagate.
  (skill-permission-rules nil :type list)
  ;; Explicit Agent-tool tier selector, stored as (:tier TIER).  This
  ;; suppresses the agent default even when the tier resolves to inherit.
  model-tier-override
  ;; Skill-scoped selector, stored as (:tier TIER) or
  ;; (:backend BACKEND :model MODEL).  The historical slot name is kept
  ;; because request-scoped skill code already uses the same terminology.
  skill-model-override
  skill-effort-override
  hook-rules
  ;; handle-state metadata for the badge.
  ;; CALL-COUNT increments on each gptel-pre-tool-call-functions
  ;; firing within this invocation's buffer.  STARTED-AT is the
  ;; wall-clock at allocation; the difference at completion gives
  ;; ELAPSED for the done badge.  TERMINAL-REASON carries an
  ;; error / abort reason string for the error / aborted badges.
  (call-count 0 :type integer)
  (started-at nil)
  (terminal-reason nil :type (or null string))
  (verdict nil :type (or null symbol))
  (activity nil :type list)
  ;; Immediate parent runtime context.  PARENT-SESSION is always the
  ;; top-level persisted session; a background agent may instead have
  ;; another invocation as its immediate caller.  These slots are
  ;; intentionally runtime-only and let ERRS/ABRT handlers complete the
  ;; same cleanup path as the normal callback closure.
  (parent-context nil)
  (parent-fsm nil)
  (background-result-reported-p nil :type boolean))

(defun mevedel-agent-invocation-create (agent)
  "Create a fresh `mevedel-agent-invocation' for AGENT.

The invocation gets an independent clone of AGENT's reminders so
`last-fired' tracking is isolated from other invocations.  When the
agent has a `max-turns' cap, a one-shot max-turns-warning reminder is
prepended automatically so the agent gets a single nudge near the
turn limit without any per-agent declaration.

The agent's `:tools' spec is resolved; the `:deferred' portion seeds
the invocation's `deferred-set' so ToolSearch running inside the
spawned sub-agent can discover the agent's own lazy tools.  When the
deferred set is non-empty, invocation-scoped roster and expiry
reminders are added so the sub-agent learns which tools it can
activate without polluting the main session's reminder list."
  (let* ((reminders (mevedel-reminders-clone-list
                     (mevedel-agent-reminders agent)))
         (resolved (mevedel-tool-resolve (mevedel-agent--effective-specs agent)))
         (deferred-tools (plist-get resolved :deferred))
         (deferred-set
          (mapcar (lambda (tool)
                    (cons (list (mevedel-tool-category tool)
                                (mevedel-tool-name tool))
                          ;; Tool-supplied one-liner if any; nil
                          ;; means the roster reminder lists just
                          ;; the name.  Full descriptions are too
                          ;; long for the roster (some wrapped tools
                          ;; carry multi-paragraph docstrings).
                          (mevedel-tool-summary tool)))
                  deferred-tools)))
    (when (mevedel-agent-max-turns agent)
      (push (mevedel-reminders-make-max-turns-warning) reminders))
    (when (equal (mevedel-agent-name agent) "verifier")
      (push (mevedel-reminders-make-verifier-read-only) reminders))
    (when deferred-set
      (push (mevedel-reminders-make-agent-deferred-tools-expired) reminders)
      (push (mevedel-reminders-make-agent-deferred-tools-roster) reminders))
    ;; Install the background-channels one-shot reminder for every
    ;; invocation; its trigger checks `background-p' on the
    ;; invocation, which the dispatcher sets after this struct is
    ;; created (so we cannot gate the install here on background-p).
    ;; Foreground invocations carry the reminder dormant -- never
    ;; firing -- which is harmless.
    (push (mevedel-reminders-make-agent-background-channels) reminders)
    (mevedel-agent-invocation--create
     :agent agent
     :reminders reminders
     :hook-rules (copy-sequence (mevedel-agent-hook-rules agent))
     :turn-count 0
     :deferred-set deferred-set
     ;; stamp wall-clock at invocation creation so the
     ;; completed-handle badge can compute elapsed time.
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

(mevedel-define-agent explorer
  :description "Read-only exploration agent for codebase investigation and, when
needed, web research.  Caller specifies the thoroughness level
(quick/moderate/thorough) in the prompt.  Returns a structured report -- never
modifies files."
  :tools (read (:tool "Bash")
          (:tool "Ask") (:tool "RequestAccess")
          (:tool "ToolSearch")
          (:tool "TaskCreate") (:tool "TaskUpdate")
          (:tool "TaskList") (:tool "TaskGet")
          (:deferred code)
          (:deferred web)
          (:deferred elisp))
  :prompt-file "agents/explorer.md"
  :include-workspace-config nil
  :include-memory nil
  :max-turns 30)

(mevedel-define-agent coordinator
  :description "Orchestration agent that dispatches and monitors workers via
Agent, SendMessage, and the task system.  Never implements directly -- delegates
all code changes to worker agents and verifies results before reporting."
  ;; Orchestration-only tool set: deliberately no `read' group.  The
  ;; coordinator must not Glob, Grep, Read, or otherwise self-investigate;
  ;; that work belongs to dispatched workers.  Removing the tools removes
  ;; the temptation.  TaskGet is included alongside Create/Update/List so
  ;; the coordinator can inspect a single task's state without a TaskList
  ;; round-trip.
  :tools ((:tool "Ask") (:tool "RequestAccess")
          (:tool "Agent") (:tool "StopAgent") (:tool "SendMessage")
          (:tool "TaskCreate") (:tool "TaskUpdate")
          (:tool "TaskList") (:tool "TaskGet")
          (:tool "ToolSearch"))
  :prompt-file "skills/coordinator/SKILL.md"
  :include-workspace-config nil
  :include-memory nil
  :max-turns 50)

(mevedel-define-agent verifier
  :description "Adversarial verification specialist.  Read-only -- \
tries to break implementations through edge cases, tests, and code \
review.  Cannot edit, write, or create files."
  :tools (read code
          (:tool "Bash") (:tool "Eval")
          (:tool "Ask") (:tool "RequestAccess")
          (:tool "ToolSearch")
          (:deferred elisp))
  :prompt-file "agents/verifier.md"
  :include-memory nil
  :max-turns 20)

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


;;
;;; Request-time agent setup

(defun mevedel-agents--setup-for-request (&optional preset-name)
  "Set up agents for the current request.

If PRESET-NAME is non-nil and has an `:agents' entry in
`mevedel-preset--registry', only those agents are registered.
Otherwise all agents in `mevedel-agent--registry' are registered.

Populates the buffer-local `mevedel-agent-exec--agents' and updates the
Agent tool's `:enum' slot.  Must be called in the chat buffer."
  (let* ((meta (and preset-name
                    (alist-get preset-name mevedel-preset--registry)))
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
  ;; Update Agent tool enum to list available agent names
  (when-let* ((agent-tool (gptel-get-tool '("mevedel" "Agent")))
              (args (gptel-tool-args agent-tool))
              (first-arg (car args)))
    (setf (plist-get first-arg :enum)
          (vconcat (mapcar #'car mevedel-agent-exec--agents)))
    (when-let* ((model-arg
                 (cl-find-if (lambda (arg)
                               (equal (plist-get arg :name) "model"))
                             args)))
      (setf (plist-get model-arg :enum)
            (vconcat '("fast" "balanced" "strong")))
      (setf (plist-get model-arg :description)
            (mevedel-model-agent-tool-description))))
  nil)

(provide 'mevedel-agents)
;;; mevedel-agents.el ends here
