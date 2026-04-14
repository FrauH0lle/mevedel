;;; mevedel-agents.el -- Agent definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative definitions for the specialised sub-agents that mevedel
;; spawns through the Agent tool: `explore' (read-only investigation),
;; `planner' (interactive plan building with PresentPlan), `verifier'
;; (adversarial read-only review), and `coordinator' (orchestration
;; agent that dispatches background workers).  Uses the
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

;; `gptel'
(declare-function gptel-get-tool "ext:gptel" (name))

;; `gptel-agent'
(declare-function gptel-agent-update "ext:gptel-agent" ())
(defvar gptel-agent--agents)

;; `gptel-request'
(declare-function gptel-tool-args "ext:gptel-request" (cl-x) t)


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

;; `mevedel-tool-plan'
(declare-function mevedel-tools--post-tool-plan-intercept "mevedel-tool-plan" (info))

;; `mevedel-system'
(defvar mevedel-system--tone-prompt)
(declare-function mevedel-system-build-prompt "mevedel-system" (base-prompt &optional workspace))

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
  (reminders nil :type list))

(defvar mevedel-agent--registry nil
  "Alist mapping agent name strings to `mevedel-agent' structs.")

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
                               with `:system-prompt'.  Any occurrence of
                               the literal `{{TONE_PROMPT}}' in the file is
                               substituted with `mevedel-system--tone-prompt'
                               at runtime, and the result is passed through
                               `mevedel-system-build-prompt'.
  :max-turns      INTEGER   -- max conversation turns (nil = unlimited)
  :reminders      LIST      -- list of `mevedel-reminder' structs used as
                               templates; cloned per invocation for
                               independent `last-fired' tracking

Creates a `mevedel-agent' struct and registers it in
`mevedel-agent--registry'."
  (declare (indent 1))
  (let* ((name-str (symbol-name name))
         (prompt-file (plist-get keys :prompt-file))
         (explicit-sp (plist-get keys :system-prompt))
         (_ (when (and prompt-file explicit-sp)
              (error "Cannot combine :prompt-file and :system-prompt for agent %s"
                     name-str)))
         (loaded (when prompt-file
                   (let ((path (expand-file-name
                                prompt-file
                                mevedel-tool-registry--source-dir)))
                     (unless (file-exists-p path)
                       (error "Agent prompt file not found: %s" path))
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))))
         (system-prompt-form
          (cond
           (loaded
            `(lambda ()
               (mevedel-system-build-prompt
                (string-replace "{{TONE_PROMPT}}"
                                mevedel-system--tone-prompt
                                ,loaded))))
           (t explicit-sp))))
    `(let ((agent (mevedel-agent--create
                   :name ,name-str
                   :description ,(plist-get keys :description)
                   :tools ',(plist-get keys :tools)
                   :system-prompt ,system-prompt-form
                   :max-turns ,(plist-get keys :max-turns)
                   :reminders ,(plist-get keys :reminders))))
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
both contexts."
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
  (stashed-result nil :type list))

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
         (resolved (mevedel-tool-resolve (mevedel-agent-tools agent)))
         (deferred-tools (plist-get resolved :deferred))
         (deferred-set
          (mapcar (lambda (tool)
                    (cons (list (mevedel-tool-category tool)
                                (mevedel-tool-name tool))
                          (or (mevedel-tool-description tool) "")))
                  deferred-tools)))
    (when (mevedel-agent-max-turns agent)
      (push (mevedel-reminders-make-max-turns-warning) reminders))
    (when (equal (mevedel-agent-name agent) "verifier")
      (push (mevedel-reminders-make-verifier-read-only) reminders))
    (when deferred-set
      (push (mevedel-reminders-make-agent-deferred-tools-expired) reminders)
      (push (mevedel-reminders-make-agent-deferred-tools-roster) reminders))
    (mevedel-agent-invocation--create
     :agent agent
     :reminders reminders
     :turn-count 0
     :deferred-set deferred-set)))

(defun mevedel-agent-to-gptel-spec (agent)
  "Convert `mevedel-agent' AGENT to a gptel agent plist.

Returns a cons (NAME . PLIST) suitable for `gptel-agent--agents'."
  (let* ((tool-specs (mevedel-agent-tools agent))
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

(mevedel-define-agent explore
  :description "Read-only exploration agent for codebase investigation and, when
needed, web research.  Caller specifies the thoroughness level
(quick/moderate/thorough) in the prompt.  Returns a structured report — never
modifies files."
  :tools (read (:tool "Bash")
          (:tool "Ask") (:tool "RequestAccess")
          (:tool "ToolSearch")
          (:deferred code)
          (:deferred web))
  :prompt-file "agents/explore.md"
  :max-turns 30)

(mevedel-define-agent planner
  :description "Specialized agent for creating interactive implementation plans.
Reads codebase to understand context, then presents structured plans for user feedback.
Iterates on plans based on user acceptance, rejection, or modification requests."
  :tools (read (:tool "Ask") (:tool "RequestAccess") (:tool "PresentPlan")
          (:tool "ToolSearch")
          (:deferred code)
          (:deferred (:tool "Eval")))
  :prompt-file "agents/planner.md"
  :max-turns 30)

(mevedel-define-agent coordinator
  :description "Orchestration agent that dispatches and monitors workers via
Agent, SendMessage, and the task system.  Never implements directly — delegates
all code changes to worker agents and verifies results before reporting."
  :tools (read (:tool "Ask") (:tool "RequestAccess")
          (:tool "Agent") (:tool "SendMessage")
          (:tool "TaskCreate") (:tool "TaskUpdate") (:tool "TaskList")
          (:tool "ToolSearch")
          (:deferred code))
  :prompt-file "skills/coordinator/SKILL.md"
  :max-turns 50)

(mevedel-define-agent verifier
  :description "Adversarial verification specialist.  Read-only — \
tries to break implementations through edge cases, tests, and code \
review.  Cannot edit, write, or create files."
  :tools (read code
          (:tool "Bash") (:tool "Eval")
          (:tool "Ask") (:tool "RequestAccess")
          (:tool "ToolSearch"))
  :prompt-file "agents/verifier.md"
  :max-turns 20)


;;
;;; Request-time agent setup

(defun mevedel-agents--setup-for-request (&optional preset-name)
  "Set up agents for the current request.

If PRESET-NAME is non-nil and has an `:agents' entry in
`mevedel-preset--registry', only those agents are registered.
Otherwise all agents in `mevedel-agent--registry' plus the
introspector are registered.

The introspector comes from `gptel-agent' and is only included if it
appears in the preset's agent list (or no filtering is active).

Populates buffer-local `gptel-agent--agents', updates the Agent tool's
`:enum' slot, and registers the plan intercept hook.  Must be called
in the chat buffer."
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
                    mevedel-agent--registry)))
         (include-introspector
          (or (null allowed) (memq 'introspector allowed))))
    (setq-local gptel-agent--agents
                (if include-introspector
                    (append mevedel-specs
                            (list (mevedel-agents--make-introspector-spec)))
                  mevedel-specs)))
  ;; Update Agent tool enum to list available agent names
  (when-let* ((agent-tool (gptel-get-tool '("mevedel" "Agent")))
              (args (gptel-tool-args agent-tool))
              (first-arg (car args)))
    (setf (plist-get first-arg :enum)
          (vconcat (mapcar #'car gptel-agent--agents))))
  ;; Register post-tool hook for plan implementation interception
  (add-hook 'gptel-post-tool-call-functions
            #'mevedel-tools--post-tool-plan-intercept nil t))

(defun mevedel-agents--make-introspector-spec ()
  "Build the introspector agent spec from `gptel-agent'.

Fetches the introspector definition from `gptel-agent-update',
replaces its tools with mevedel-specific ones (introspection, Eval,
Ask, RequestAccess), and appends a clarification hint to the system
prompt."
  (with-temp-buffer
    (make-local-variable 'gptel-agent--agents)
    (gptel-agent-update)
    (let* ((spec (assoc-string "introspector" gptel-agent--agents))
           (plist (cdr spec)))
      (setq plist
            (plist-put plist :tools
                       '(:function
                         (lambda (_tools)
                           (append
                            (gptel-get-tool "introspection")
                            (list
                             (gptel-get-tool '("mevedel" "Eval"))
                             (gptel-get-tool '("mevedel" "Ask"))
                             (gptel-get-tool '("mevedel" "RequestAccess"))))))))
      (setq plist
            (plist-put plist :system
                       (concat (plist-get plist :system)
                               "\nIn case you need clarification, use your 'Ask' tool to interact with the user.")))
      (cons "introspector" plist))))

(provide 'mevedel-agents)
;;; mevedel-agents.el ends here
