;;; mevedel-tools.el -- Tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Tool aggregator.  `require's every `mevedel-tool-*' module and exposes
;; `mevedel-tools-register' as the single initializer for the complete
;; built-in tool surface, including Skill and ListSkills.
;;
;; Also hosts the deferred-tool (ToolSearch) infrastructure that does
;; not yet belong to any single tool module: the polymorphic
;; `mevedel-tools--ctx-*' accessors that dispatch on session vs.
;; invocation state, the WAIT-state handler that drains queued
;; `<agent-message>' blocks, and the `gptel-send' advice that
;; dispatches slash commands before they reach the model.

;;; Code:

(require 'cl-lib)
(require 'mevedel-tool-registry)

(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'mevedel-agents)
(require 'mevedel-agent-conversation)
(require 'mevedel-interaction-prompt)
(require 'mevedel-permission-prompt)
(require 'mevedel-tool-ask)
(require 'mevedel-tool-code)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-goal)
(require 'mevedel-tool-introspect)
(require 'mevedel-goal)
(require 'mevedel-tool-skills)
(require 'mevedel-tool-task)
(require 'mevedel-tool-tutor)
(require 'mevedel-tool-ui)
(require 'mevedel-tool-web)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `gptel'
(defvar gptel-tools)

;; `gptel-request'
(declare-function gptel--handle-tool-use "ext:gptel-request" (fsm))
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel--parse-tools "ext:gptel-request" (backend tools))
(declare-function gptel--process-tool-call
                  "ext:gptel-request" (fsm tool-spec tool-call result))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel--ersatz-json-tool)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-clear-context-mailbox
                  "mevedel-agent-control" (context))
(declare-function mevedel-agent-control-context-mailbox
                  "mevedel-agent-control" (context))
(declare-function mevedel-agent-control-context-path
                  "mevedel-agent-control" (context))
(declare-function mevedel-agent-control-direct-children
                  "mevedel-agent-control" (session parent-path))

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-insert-user-block
                  "mevedel-agent-conversation"
                  (invocation block &optional marker))
(declare-function mevedel-agent-conversation-record-activity
                  "mevedel-agent-conversation"
                  (invocation item &optional suppress-rerender))
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)

;; `mevedel-goal'

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue-sweep-origin
                  "mevedel-permission-queue"
                  (origin &optional session no-render))

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-groups "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-truthy-p "mevedel-tool-registry" (value))

;;
;;; Tool registration

;;;###autoload
(defun mevedel-tools-register ()
  "Register the complete built-in tool surface with the mevedel registry."
  (mevedel-tool-web--register)
  (mevedel-tool-fs--register)
  (mevedel-tool-code--register)
  (mevedel-tool-tutor--register)
  (mevedel-tool-exec--register)
  (mevedel-tool-goal--register)
  (mevedel-tool-ui--register)
  (mevedel-tool-skills--register)
  (mevedel-tool-task--register)
  (mevedel-tool-introspect--register))

(defun mevedel-tools-active-count (&optional buffer)
  "Return the number of active gptel tools in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (length (and (boundp 'gptel-tools) gptel-tools)))
      0)))

(defun mevedel-tools--request-data-set-tools (info)
  "Serialize INFO's active tools back into its provider request data."
  (let* ((data (plist-get info :data))
         (parsed (gptel--parse-tools (plist-get info :backend)
                                     (plist-get info :tools)))
         (tool-config (plist-get data :toolConfig)))
    (if (and (listp tool-config) (plist-member tool-config :tools))
        (plist-put tool-config :tools parsed)
      (plist-put data :tools parsed))))

(defun mevedel-tools--handle-plan-tool-filter (fsm)
  "Apply Plan and active-Goal request-time tool visibility to FSM."
  (let* ((info (gptel-fsm-info fsm))
         (invocation (plist-get info :mevedel-agent-invocation))
         (buffer (plist-get info :buffer))
         (session
          (or (and (mevedel-agent-invocation-p invocation)
                   (mevedel-agent-invocation-parent-session invocation))
              (and (buffer-live-p buffer)
                   (buffer-local-value 'mevedel--session buffer))))
         (tools (plist-get info :tools))
         (active-root-goal-p
          (and session
               (not (or invocation
                        (mevedel-tools--buffer-local-agent-invocation buffer)))
               (when-let* ((goal (mevedel-session-goal session)))
                 (eq (mevedel-goal-status goal) 'active)))))
    (when tools
      (let ((filtered
             (cl-remove-if
              (lambda (tool)
                (let ((name (gptel-tool-name tool)))
                  (or (and (equal name "UpdateGoal")
                           (not active-root-goal-p))
                      (and session
                           (mevedel-session-plan-mode session)
                           (when-let* ((registered
                                       (mevedel-tool-get name)))
                             (memq 'edit
                                   (mevedel-tool-groups registered)))))))
              tools)))
        (unless (= (length filtered) (length tools))
          (plist-put info :tools filtered)
          (mevedel-tools--request-data-set-tools info))))))


;;
;;; Deferred Tool Loading (ToolSearch)

(defcustom mevedel-deferred-tool-ttl 5
  "Number of turns a deferred tool stays active after its last use.

Injected deferred tools start with this counter and are removed from
the active payload when it reaches zero.  Calling the tool resets the
counter.  Set higher for looser timeouts, lower to prune more
aggressively."
  :type 'integer
  :group 'mevedel)

;;
;;; Polymorphic deferred-slot accessors
;;
;; Both `mevedel-session' and `mevedel-agent-invocation' carry the
;; same five deferred-* slots.  These accessors dispatch on struct type
;; so the WAIT handler, pipeline, ToolSearch, and reminders can all
;; operate on whichever context is current without branching on type.

(defmacro mevedel-tools--define-deferred-accessor (slot)
  "Define a polymorphic accessor for SLOT on session or agent-invocation.

Creates `mevedel-tools--ctx-deferred-SLOT' that reads from either a
`mevedel-session' or a `mevedel-agent-invocation', plus a `setf'
expander that writes to the correct underlying `cl-defstruct' slot."
  (let* ((getter (intern (format "mevedel-tools--ctx-deferred-%s" slot)))
         (session-acc (intern (format "mevedel-session-deferred-%s" slot)))
         (inv-acc (intern (format "mevedel-agent-invocation-deferred-%s" slot))))
    `(progn
       (defun ,getter (ctx)
         ,(format "Return CTX's deferred-%s slot (session or invocation)." slot)
         (if (mevedel-agent-invocation-p ctx)
             (,inv-acc ctx)
           (,session-acc ctx)))
       (gv-define-setter ,getter (val ctx)
         (list 'if (list 'mevedel-agent-invocation-p ctx)
               (list 'setf (list ',inv-acc ctx) val)
               (list 'setf (list ',session-acc ctx) val))))))

(mevedel-tools--define-deferred-accessor set)
(mevedel-tools--define-deferred-accessor pending)
(mevedel-tools--define-deferred-accessor injected)
(mevedel-tools--define-deferred-accessor used)
(mevedel-tools--define-deferred-accessor expired)

(defun mevedel-tools--ctx-record-used (ctx name)
  "Push tool NAME onto CTX's deferred-used slot.

Exposed as a plain function so callers outside `mevedel-tools' (which
cannot require it due to the load cycle through
`mevedel-tool-registry') can mutate the slot without depending on the
polymorphic `gv-setter' being visible at byte-compile time."
  (cl-pushnew name (mevedel-tools--ctx-deferred-used ctx) :test #'equal))

;;
;;; FSM tracking for pipeline context dispatch

(defvar mevedel-tools--current-fsm nil
  "Dynamically bound to the currently-executing gptel FSM.

Set by `mevedel-tools--handle-tool-use-advice' around
`gptel--handle-tool-use' so the pipeline and ToolSearch can determine
which context (session vs agent invocation) owns the current tool
call.  Nil outside tool dispatch.")

(defun mevedel-tools--tool-call-result-p (tool-call)
  "Return non-nil when TOOL-CALL already carries a result."
  (plist-get tool-call :result))

(defun mevedel-tools--active-tool-call-name-p (name tools)
  "Return non-nil when NAME is present in active gptel TOOLS."
  (cl-find-if (lambda (tool)
                (equal name (gptel-tool-name tool)))
              tools))

(defun mevedel-tools--ersatz-json-tool-p (name)
  "Return non-nil when NAME is gptel's structured-output pseudo tool."
  (and (boundp 'gptel--ersatz-json-tool)
       (equal name gptel--ersatz-json-tool)))

(defun mevedel-tools--deferred-entry-name-p (name entries)
  "Return non-nil when a deferred tool named NAME is in ENTRIES."
  (cl-some (lambda (entry)
             (equal name (cadr (car entry))))
           entries))

(defun mevedel-tools--deferred-tool-name-p (ctx name)
  "Return non-nil when CTX knows NAME as deferred or recently expired."
  (and ctx
       (or (mevedel-tools--deferred-entry-name-p
            name (mevedel-tools--ctx-deferred-set ctx))
           (member name (mevedel-tools--ctx-deferred-expired ctx)))))

(defun mevedel-tools--unknown-tool-result (ctx name)
  "Return the synthetic tool result for unknown tool NAME in CTX."
  (if (mevedel-tools--deferred-tool-name-p ctx name)
      (format (concat "Error: Tool %s is not currently loaded. "
                      "Call ToolSearch(query=%S, load=true) first, "
                      "then call %s after ToolSearch returns.")
              name name name)
    (format (concat "Error: Unknown tool %s. Use an available tool, "
                    "or ToolSearch if this is a deferred capability.")
            name)))

(defun mevedel-tools--synthetic-unknown-tool (name)
  "Return an unregistered display-only gptel tool for unknown NAME."
  (gptel-make-tool
   :name name
   :function (lambda (&rest _) "")
   :description (format "Synthetic placeholder for unknown tool %s" name)
   :args nil
   :category "mevedel"))

(defun mevedel-tools--unknown-tool-call-p (tool-call tools)
  "Return non-nil when TOOL-CALL names a missing tool in active TOOLS."
  (let ((name (plist-get tool-call :name)))
    (and name
         (not (mevedel-tools--tool-call-result-p tool-call))
         (not (mevedel-tools--active-tool-call-name-p name tools))
         (not (mevedel-tools--ersatz-json-tool-p name)))))

(defun mevedel-tools--settle-unknown-tool-calls (fsm)
  "Convert unresolved unknown tool-use entries in FSM into errors."
  (when-let* ((info (gptel-fsm-info fsm)))
    (let ((ctx (mevedel-tools--deferred-context-for fsm))
          (tools (plist-get info :tools)))
      (dolist (tool-call (plist-get info :tool-use))
        (when (mevedel-tools--unknown-tool-call-p tool-call tools)
          (let ((name (plist-get tool-call :name)))
            (gptel--process-tool-call
             fsm
             (mevedel-tools--synthetic-unknown-tool name)
             tool-call
             (mevedel-tools--unknown-tool-result ctx name))))))))

(defun mevedel-tools--handle-tool-use-advice (orig-fun fsm)
  "Dyn-bind `mevedel-tools--current-fsm' around ORIG-FUN.
Used as an `:around' advice on `gptel--handle-tool-use' so that tool
handlers (via the pipeline) can recover the FSM that triggered them
without threading it through every call site.  Settle unknown tool calls
before ORIG-FUN so mevedel can preserve deferred-tool guidance before
gptel's generic unknown-tool fallback consumes those calls."
  (let ((mevedel-tools--current-fsm fsm))
    (mevedel-tools--settle-unknown-tool-calls fsm)
    (funcall orig-fun fsm)))

(advice-add 'gptel--handle-tool-use :around
            #'mevedel-tools--handle-tool-use-advice)

(defun mevedel-tools--buffer-local-agent-invocation (buffer)
  "Return BUFFER's local agent invocation, when it has one."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (and (boundp 'mevedel--agent-invocation)
           (mevedel-agent-invocation-p mevedel--agent-invocation)
           mevedel--agent-invocation))))

(defun mevedel-tools--buffer-local-session (buffer)
  "Return BUFFER's local session, when it has one."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (and (boundp 'mevedel--session)
           mevedel--session))))

(defun mevedel-tools--deferred-context-for (fsm)
  "Return the deferred context (invocation or session) for FSM.

First checks FSM's info plist for an attached
`mevedel-agent-invocation'.  Falls back to the request buffer's
local `mevedel--agent-invocation' before its parent
`mevedel--session', because agent transcript buffers intentionally
carry both."
  (or (and fsm
           (when-let* ((info (gptel-fsm-info fsm))
                       (inv (plist-get info :mevedel-agent-invocation))
                       ((mevedel-agent-invocation-p inv)))
             inv))
      (when-let* ((fsm fsm)
                  (info (gptel-fsm-info fsm))
                  (buffer (plist-get info :buffer))
                  ((buffer-live-p buffer)))
        (mevedel-tools--buffer-local-agent-invocation buffer))
      (when-let* ((fsm fsm)
                  (info (gptel-fsm-info fsm))
                  (buffer (plist-get info :buffer))
                  ((buffer-live-p buffer)))
        (mevedel-tools--buffer-local-session buffer))))

(defun mevedel-tools--current-deferred-context ()
  "Return the deferred context for the currently-executing tool call.

Prefers `mevedel-tools--current-fsm' (set during tool dispatch).
Falls back to the current buffer's `mevedel--agent-invocation' before
`mevedel--session' when no FSM is bound (e.g., direct calls from
tests or tool dispatch paths already inside an agent buffer)."
  (if mevedel-tools--current-fsm
      (mevedel-tools--deferred-context-for mevedel-tools--current-fsm)
    (or (and (boundp 'mevedel--agent-invocation)
             (mevedel-agent-invocation-p mevedel--agent-invocation)
             mevedel--agent-invocation)
        (and (boundp 'mevedel--session) mevedel--session))))

(defun mevedel-tools--handle-deferred-inject (fsm)
  "Manage deferred tool lifecycle in FSM's request payload.

Runs as a WAIT state handler, before `gptel--handle-wait' fires the
HTTP request.  Operates on the session or agent invocation that owns
FSM, reading and mutating its deferred state slots:

  - `deferred-set'       alist (PATH . DESC) of discoverable tools
  - `deferred-pending'   `gptel-tools' queued by ToolSearch(load=t)
  - `deferred-injected'  alist of tool-name -> remaining TTL counter
  - `deferred-used'      tool-names invoked since the last turn
  - `deferred-expired'   tool-names evicted this turn (for reminder)

Each WAIT cycle:

  1. Reset TTL for any currently-injected tool that the model called
     since the last turn (record from `deferred-used').
  2. Decrement TTL for all other currently-injected tools.
  3. Remove tools whose TTL reached zero from the active payload,
     return them to `deferred-set', and record their names on
     `deferred-expired' so the next-turn reminder can cite them.
  4. Inject newly-pending tools with an initial TTL of
     `mevedel-deferred-tool-ttl'.
  5. Clear `deferred-used' and `deferred-pending'.
  6. Re-serialize the provider's request tools via `gptel--parse-tools'
     when anything changed."
  (let ((info (gptel-fsm-info fsm)))
    (when-let* ((ctx (mevedel-tools--deferred-context-for fsm)))
      (let* ((used (mevedel-tools--ctx-deferred-used ctx))
             (injected (mevedel-tools--ctx-deferred-injected ctx))
             (pending (mevedel-tools--ctx-deferred-pending ctx))
             (expired nil)
             (kept nil)
             (changed nil))
        ;; Phase 1+2: reset TTL for used tools, decrement others.
        (dolist (entry injected)
          (let* ((name (car entry))
                 (ttl (cdr entry))
                 (new-ttl (if (member name used)
                              mevedel-deferred-tool-ttl
                            (1- ttl))))
            (if (> new-ttl 0)
                (push (cons name new-ttl) kept)
              (push name expired))))
        ;; Phase 3: drop expired tools from the active payload and
        ;; return their registry entries to the deferred set.
        (when expired
          (plist-put info :tools
                     (cl-remove-if (lambda (ts)
                                     (member (gptel-tool-name ts) expired))
                                   (plist-get info :tools)))
          (setq changed t))
        (setf (mevedel-tools--ctx-deferred-injected ctx) (nreverse kept))
        ;; Phase 4: inject newly-pending tools with initial TTL.
        (when pending
          (dolist (tool pending)
            (let ((name (gptel-tool-name tool)))
              (unless (cl-find-if
                       (lambda (ts) (equal (gptel-tool-name ts) name))
                       (plist-get info :tools))
                (plist-put info :tools (cons tool (plist-get info :tools)))
                (push (cons name mevedel-deferred-tool-ttl)
                      (mevedel-tools--ctx-deferred-injected ctx))
                (setq changed t)))))
        ;; Clear single-turn slots.
        (setf (mevedel-tools--ctx-deferred-pending ctx) nil)
        (setf (mevedel-tools--ctx-deferred-used ctx) nil)
        ;; Expose expired names for the next-turn reminder.
        (setf (mevedel-tools--ctx-deferred-expired ctx) expired)
        ;; Phase 6: re-serialize when the active tool list changed.
        (when changed
          (mevedel-tools--request-data-set-tools info))))))

;;
;;; Deferred Tool Loading -- Search and ToolSearch tool

(defun mevedel-tools--search-deferred (ctx query)
  "Search CTX's deferred tool set for entries matching QUERY.

CTX is a `mevedel-session' or `mevedel-agent-invocation'.  QUERY is
split into whitespace-separated terms.  An entry matches if ANY term
appears as a substring in the tool name or its short description.
Matching is case-insensitive.

Returns a list of (TOOL-PATH . SHORT-DESCRIPTION) pairs from CTX's
`deferred-set' slot."
  (let ((terms (mapcar (lambda (term) (regexp-quote (downcase term)))
                       (split-string query nil t))))
    (cl-remove-if-not
     (lambda (entry)
       (let* ((path (car entry))
              (category (car path))
              (name (cadr path))
              (tool (and category name (mevedel-tool-get name category)))
              (groups (and tool
                           (mapconcat #'symbol-name
                                      (mevedel-tool-groups tool)
                                      " ")))
              (text (downcase
                     (concat category " " name " " (cdr entry) " " groups))))
         (cl-some (lambda (term) (string-match-p term text)) terms)))
     (mevedel-tools--ctx-deferred-set ctx))))

(defconst mevedel-tools--tool-search-usage-hints
  '(("XrefReferences" . "Usage: XrefReferences(identifier, file_path) for references/callers.")
    ("XrefDefinitions" . "Usage: XrefDefinitions(pattern, file_path) for definitions/name discovery.")
    ("Imenu" . "Usage: Imenu(file_path) for symbols in one known file.")
    ("Treesitter" . "Usage: Treesitter(file_path, line/column or whole_file) for syntax structure.")
    ("function_source" . "Usage: function_source(function) for loaded function source.")
    ("variable_source" . "Usage: variable_source(variable) for variable source.")
    ("function_documentation" . "Usage: function_documentation(function) for docstrings.")
    ("variable_documentation" . "Usage: variable_documentation(variable) for docstrings.")
    ("library_source" . "Usage: library_source(library) for load-path library source."))
  "Concise call-shape hints for ToolSearch results.")

(defun mevedel-tools--tool-search-format-entry (entry)
  "Format one deferred tool search ENTRY with a concise usage hint."
  (let* ((name (cadr (car entry)))
         (summary (cdr entry))
         (usage (cdr (assoc name mevedel-tools--tool-search-usage-hints)))
         (base (if (and (stringp summary) (not (string-empty-p summary)))
                   (format "- %s: %s" name summary)
                 (format "- %s" name))))
    (if usage
        (format "%s\n  %s" base usage)
      base)))

(cl-defun mevedel-tools--tool-search (callback query &optional load)
  "Search deferred tools matching QUERY, optionally LOAD them.

CALLBACK is the async callback.  QUERY is a search string matched
against tool names and descriptions.  When LOAD is non-nil (or
:json-false for false), matching tools are injected into the
current request for immediate use.

Dispatches on the current deferred context: when running inside a
spawned sub-agent, queues pending tools on the agent invocation;
otherwise queues them on the chat buffer's session."
  (mevedel-tools--validate-params callback mevedel-tools--tool-search
                                  (query (stringp . "string"))
                                  (load booleanp nil))
  (setq load (mevedel-tool-truthy-p load))
  (let* ((ctx (mevedel-tools--current-deferred-context))
         (matches (and ctx
                       (mevedel-tools--search-deferred ctx query)))
         (result
          (if matches
              (mapconcat #'mevedel-tools--tool-search-format-entry
                         matches "\n")
            "No matching tools found.")))
    (when (and load matches ctx)
      ;; Resolve tool structs from the registry and queue on the context.
      (dolist (entry matches)
        (when-let* ((tool (ignore-errors (gptel-get-tool (car entry)))))
          (let ((tool-list (ensure-list tool)))
            (dolist (t1 tool-list)
              (unless (cl-find-if (lambda (pending)
                                    (equal (gptel-tool-name pending)
                                           (gptel-tool-name t1)))
                                  (mevedel-tools--ctx-deferred-pending ctx))
                (push t1 (mevedel-tools--ctx-deferred-pending ctx))))))))
    (funcall callback
             (if matches
                 (format "Found %d tool(s):\n%s%s"
                         (length matches) result
                         (if load "\n\nTools loaded. They are available now; call them in your next tool call."
                           "\n\nCall ToolSearch again with load=true to activate these tools. Search by exact tool name when known (for example XrefReferences or Imenu), or by capability group such as xref, imenu, treesitter, elisp, or web."))
               result))))


;;
;;; Agent mailbox injection

(defun mevedel-tools--mailbox-body-escape (body)
  "Escape mailbox delimiter-looking text in BODY."
  (let ((text (or body "")))
    (dolist (pair '(("<agent-result" . "&lt;agent-result")
                    ("</agent-result>" . "&lt;/agent-result&gt;")
                    ("<agent-message" . "&lt;agent-message")
                    ("</agent-message>" . "&lt;/agent-message&gt;")))
      (setq text
            (replace-regexp-in-string (regexp-quote (car pair))
                                      (cdr pair)
                                      text t t)))
    text))

(defun mevedel-tools--message-delivery-block (msg)
  "Return the user-role delivery block for mailbox MSG."
  (pcase (plist-get msg :type)
    ('RESULT
     (format "<agent-result sender=\"%s\" recipient=\"%s\" outcome=\"%s\">\n%s\n</agent-result>"
             (plist-get msg :sender)
             (plist-get msg :recipient)
             (plist-get msg :outcome)
             (mevedel-tools--mailbox-body-escape
              (or (plist-get msg :payload) ""))))
    ('MAIL
     (format "<agent-message type=\"MAIL\" sender=\"%s\" recipient=\"%s\">\n%s\n</agent-message>"
             (plist-get msg :sender)
             (plist-get msg :recipient)
             (mevedel-tools--mailbox-body-escape
              (or (plist-get msg :payload) ""))))
    ('USER
     (or (plist-get msg :payload) ""))
    (_ (error "Unknown agent mailbox record: %S" (plist-get msg :type)))))

(defun mevedel-tools--live-buffer-marker-p (marker buffer)
  "Return non-nil when MARKER points into BUFFER."
  (and (markerp marker)
       (marker-position marker)
       (eq (marker-buffer marker) buffer)))

(defun mevedel-tools--active-response-marker (info buffer)
  "Return INFO's active response insertion marker for BUFFER."
  (let ((tracking (plist-get info :tracking-marker))
        (position (plist-get info :position)))
    (cond
     ((mevedel-tools--live-buffer-marker-p tracking buffer) tracking)
     ((mevedel-tools--live-buffer-marker-p position buffer) position))))

(defun mevedel-tools--insert-session-injected-prompt
    (session fsm message block)
  "Insert injected MESSAGE for SESSION and FSM into the data buffer.
BLOCK is the model-visible form.  MESSAGE may provide a separate transcript
payload and hidden hook audit records.

`gptel--inject-prompt' mutates the realized request payload, but
does not write that synthetic user-role message back to the data
buffer.  This helper keeps the main transcript and view buffer in
sync with what the model actually saw."
  (when (and (mevedel-session-p session)
             (stringp block)
             (not (string-empty-p block)))
    (when-let* ((info (and fsm (gptel-fsm-info fsm)))
                (buf (plist-get info :buffer))
                ((buffer-live-p buf))
                ((not (mevedel-tools--buffer-local-agent-invocation buf)))
                ((eq session (mevedel-tools--buffer-local-session buf))))
      (condition-case err
          (with-current-buffer buf
            (let* ((inhibit-read-only t)
                   (marker (mevedel-tools--active-response-marker info buf))
                   (transcript-block
                    (or (plist-get message :transcript-payload) block))
                   (range
                    (mevedel--insert-user-role-block-at-marker
                     transcript-block marker)))
              (when range
                (save-excursion
                  (goto-char (cdr range))
                  (dolist (audit (plist-get message :hook-audits))
                    (insert (mevedel--format-hook-audit-record audit)))
                  (when marker
                    (set-marker marker (point)))))))
        (error
         (message "mevedel: insert session injected prompt failed: %S"
                  err))))))

(defun mevedel-tools--handle-message-inject (fsm)
  "WAIT-state handler: drain FSM's inbox into the next request.

Runs before `gptel--handle-wait' fires the HTTP request.  For the
context that owns FSM, injects each unread record as a separate user-role
communication block, and then removes it from the retained FIFO.  Each
injected block is also written to the owning transcript, preserving the
model-visible communication in conversation history."
  (when-let* ((ctx (mevedel-tools--deferred-context-for fsm)))
    (require 'mevedel-agent-control)
    (let* ((agent-p (mevedel-agent-invocation-p ctx))
           (messages (mevedel-agent-control-context-mailbox ctx))
           (info (gptel-fsm-info fsm))
           (data (plist-get info :data))
           (prepend-p
            (and agent-p
                 (zerop (or (mevedel-agent-invocation-turn-count ctx) 0)))))
      (when (and messages data)
        (cl-loop
         for message in messages
         for index from 0
         for block = (mevedel-tools--message-delivery-block message)
         for sender = (or (plist-get message :sender) "unknown")
         do
         (when agent-p
           (mevedel-agent-conversation-record-activity
            ctx
            (list :type 'message
                  :from sender
                  :summary (format "message from %s" sender)))
           (mevedel-agent-conversation-insert-user-block
            ctx block
            (mevedel-tools--active-response-marker
             info (mevedel-agent-invocation-buffer ctx))))
         (unless agent-p
           (mevedel-tools--insert-session-injected-prompt
            ctx fsm message block))
         (gptel--inject-prompt
          (plist-get info :backend) data
          (list :role "user" :content block)
          (and prepend-p index))))
      (when (or (null messages) data)
        (mevedel-agent-control-clear-context-mailbox ctx)))))

(defun mevedel-tools--handle-agent-roster-inject (fsm)
  "WAIT-state handler: expose direct children to FSM exactly once."
  (when-let* ((ctx (mevedel-tools--deferred-context-for fsm))
              (session
               (if (mevedel-session-p ctx)
                   ctx
                 (mevedel-agent-invocation-parent-session ctx)))
              (parent-path
               (progn
                 (require 'mevedel-agent-control)
                 (mevedel-agent-control-context-path ctx))))
    (let* ((info (gptel-fsm-info fsm))
           (initialized-p
            (plist-member info :mevedel-agent-child-paths))
           (children
            (mevedel-agent-control-direct-children session parent-path))
           (paths (mapcar (lambda (entry) (plist-get entry :path)) children))
           (known (plist-get info :mevedel-agent-child-paths))
           (new
            (cl-remove-if
             (lambda (entry)
               (member (plist-get entry :path) known))
             children))
           (data (plist-get info :data)))
      (when (or (null new) data)
        (when new
          (gptel--inject-prompt
           (plist-get info :backend) data
           (list
            :role "user"
            :content
            (concat
             "<agent-roster>\n"
             (if initialized-p
                 "New direct child agents:\n"
               "Direct child agents:\n")
             (mapconcat
              (lambda (entry)
                (format "- `%s` (`%s`)"
                        (plist-get entry :path)
                        (plist-get entry :role)))
              new "\n")
             "\n</agent-roster>"))))
        (setf (gptel-fsm-info fsm)
              (plist-put info :mevedel-agent-child-paths paths))))))

(defun mevedel-tools--handle-agent-turn-terminal (fsm)
  "Sweep pending human interactions owned by FSM's settling agent turn."
  (let ((ctx (mevedel-tools--deferred-context-for fsm)))
    (when (and ctx
               (fboundp 'mevedel-agent-invocation-p)
               (mevedel-agent-invocation-p ctx)
               (fboundp 'mevedel-permission-queue-sweep-origin))
      (let ((agent-path (mevedel-agent-invocation-path ctx))
            (parent-session
             (mevedel-agent-invocation-parent-session ctx)))
        (when (and agent-path parent-session)
          (mevedel-permission-queue-sweep-origin
           agent-path parent-session))))))


(provide 'mevedel-tools)
;;; mevedel-tools.el ends here
