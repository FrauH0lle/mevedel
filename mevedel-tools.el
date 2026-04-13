;;; mevedel-tools.el -- Tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-tool-code)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-plan)
(require 'mevedel-tool-task)
(require 'mevedel-tool-tutor)
(require 'mevedel-tool-ui)
(require 'mevedel-tool-web)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `gptel-request'
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(declare-function gptel--parse-tools "ext:gptel-request" (backend tools))
(declare-function gptel--handle-tool-use "ext:gptel-request" (fsm))
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))

;; `mevedel-tool-ui'
(defvar mevedel-tools--agents-fsm)
(declare-function mevedel-tools--agent-invocation-at "mevedel-tool-ui" (fsm))


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
expander that writes to the correct underlying cl-defstruct slot."
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

(defun mevedel-tools--ctx-messages (ctx)
  "Return CTX's inbound message queue (session or invocation)."
  (if (mevedel-agent-invocation-p ctx)
      (mevedel-agent-invocation-messages ctx)
    (mevedel-session-messages ctx)))

(gv-define-setter mevedel-tools--ctx-messages (val ctx)
  (list 'if (list 'mevedel-agent-invocation-p ctx)
        (list 'setf (list 'mevedel-agent-invocation-messages ctx) val)
        (list 'setf (list 'mevedel-session-messages ctx) val)))

(defun mevedel-tools--ctx-push-message (ctx msg)
  "Queue MSG on CTX's inbound mailbox.

Exposed as a plain function so callers in other files (which cannot
require `mevedel-tools' due to the load cycle through
`mevedel-tool-registry') can still append to the mailbox without
seeing the polymorphic `gv-setter' at byte-compile time.  MSG is
appended so messages are delivered in arrival order."
  (if (mevedel-agent-invocation-p ctx)
      (setf (mevedel-agent-invocation-messages ctx)
            (append (mevedel-agent-invocation-messages ctx) (list msg)))
    (setf (mevedel-session-messages ctx)
          (append (mevedel-session-messages ctx) (list msg)))))

(defun mevedel-tools--ctx-background-agents (ctx)
  "Return CTX's list of running background agent IDs."
  (if (mevedel-agent-invocation-p ctx)
      (mevedel-agent-invocation-background-agents ctx)
    (mevedel-session-background-agents ctx)))

(defun mevedel-tools--ctx-push-background-agent (ctx agent-id)
  "Add AGENT-ID to CTX's background agent tracking list."
  (if (mevedel-agent-invocation-p ctx)
      (push agent-id (mevedel-agent-invocation-background-agents ctx))
    (push agent-id (mevedel-session-background-agents ctx))))

(defun mevedel-tools--ctx-remove-background-agent (ctx agent-id)
  "Remove AGENT-ID from CTX's background agent tracking list."
  (if (mevedel-agent-invocation-p ctx)
      (setf (mevedel-agent-invocation-background-agents ctx)
            (delete agent-id (mevedel-agent-invocation-background-agents ctx)))
    (setf (mevedel-session-background-agents ctx)
          (delete agent-id (mevedel-session-background-agents ctx)))))

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

(defun mevedel-tools--handle-tool-use-advice (orig-fun fsm)
  "Dyn-bind `mevedel-tools--current-fsm' around ORIG-FUN.
Used as an `:around' advice on `gptel--handle-tool-use' so that tool
handlers (via the pipeline) can recover the FSM that triggered them
without threading it through every call site."
  (let ((mevedel-tools--current-fsm fsm))
    (funcall orig-fun fsm)))

(advice-add 'gptel--handle-tool-use :around
            #'mevedel-tools--handle-tool-use-advice)

(defun mevedel-tools--deferred-context-for (fsm)
  "Return the deferred context (invocation or session) for FSM.

First checks FSM's `:context' overlay for an attached
`mevedel-agent-invocation'.  Falls back to the buffer-local
`mevedel--session' of FSM's request buffer."
  (or (and fsm
           (when-let* ((info (gptel-fsm-info fsm))
                       (ov (plist-get info :context))
                       ((overlayp ov)))
             (overlay-get ov 'mevedel-agent-invocation)))
      (when-let* ((fsm fsm)
                  (info (gptel-fsm-info fsm))
                  (buffer (plist-get info :buffer))
                  ((buffer-live-p buffer)))
        (buffer-local-value 'mevedel--session buffer))))

(defun mevedel-tools--current-deferred-context ()
  "Return the deferred context for the currently-executing tool call.

Prefers `mevedel-tools--current-fsm' (set during tool dispatch).
Falls back to `mevedel--session' in the current buffer when no FSM
is bound (e.g., direct calls from tests)."
  (if mevedel-tools--current-fsm
      (mevedel-tools--deferred-context-for mevedel-tools--current-fsm)
    (and (boundp 'mevedel--session) mevedel--session)))

(defun mevedel-tools--handle-deferred-inject (fsm)
  "Manage deferred tool lifecycle in FSM's request payload.

Runs as a WAIT state handler, before `gptel--handle-wait' fires the
HTTP request.  Operates on whichever deferred context owns FSM
(session or agent invocation), reading and mutating its deferred
state slots:

  - `deferred-set'       alist (PATH . DESC) of discoverable tools
  - `deferred-pending'   gptel-tools queued by ToolSearch(load=t)
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
  6. Re-serialize `info :data :tools' via `gptel--parse-tools' when
     anything changed, keeping the payload backend-agnostic.

NOTE: This assumes tools are at (plist-get data :tools) which holds
for OpenAI, Anthropic, Ollama, and Gemini backends.  Bedrock uses a
different nesting (:toolConfig :tools) and is not yet supported."
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
          (plist-put (plist-get info :data) :tools
                     (gptel--parse-tools (plist-get info :backend)
                                         (plist-get info :tools))))))))

;;
;;; Deferred Tool Loading — Search and ToolSearch tool

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
       (let ((text (downcase (concat (cadr (car entry)) " " (cdr entry)))))
         (cl-some (lambda (term) (string-match-p term text)) terms)))
     (mevedel-tools--ctx-deferred-set ctx))))

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
  ;; Normalize boolean
  (when (eq load :json-false) (setq load nil))
  (let* ((ctx (mevedel-tools--current-deferred-context))
         (matches (and ctx
                       (mevedel-tools--search-deferred ctx query)))
         (result
          (if matches
              (mapconcat
               (lambda (entry)
                 (format "- %s: %s" (cadr (car entry)) (cdr entry)))
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
                         (if load "\n\nTools loaded and ready to use on the next turn."
                           "\n\nCall ToolSearch again with load=true to activate these tools."))
               result))))


;;
;;; Inter-agent messaging (SendMessage)

(defun mevedel-tools--sender-name (ctx)
  "Return the display name for sender context CTX.
Agent invocations use their agent's name; sessions fall back to
\"main\"."
  (cond
   ((mevedel-agent-invocation-p ctx)
    (mevedel-agent-name (mevedel-agent-invocation-agent ctx)))
   ((mevedel-session-p ctx)
    "main")
   (t "unknown")))

(defun mevedel-tools--resolve-recipient (to chat-buffer)
  "Resolve recipient TO to an inbox context bound to CHAT-BUFFER.

TO is a string naming the destination:
  - \"main\" / \"chat\" / \"coordinator\" → the chat buffer's session
  - an agent-id stored in `mevedel-tools--agents-fsm' (exact match)
  - an agent type prefix (e.g. \"explore\") matching an id like
    \"explore--<hash>\"; the first live invocation wins

Returns the resolved `mevedel-session' or `mevedel-agent-invocation',
or nil when no recipient matches."
  (unless (and (stringp to) (not (string-empty-p to)))
    (error "Recipient must be a non-empty string"))
  (cond
   ((member (downcase to) '("main" "chat" "coordinator"))
    (buffer-local-value 'mevedel--session chat-buffer))
   (t
    (with-current-buffer chat-buffer
      (let ((fsms mevedel-tools--agents-fsm))
        (or (when-let* ((pair (assoc to fsms)))
              (mevedel-tools--agent-invocation-at (cdr pair)))
            (cl-some (lambda (pair)
                       (let ((id (car pair)))
                         (when (string-prefix-p (concat to "--") id)
                           (mevedel-tools--agent-invocation-at (cdr pair)))))
                     fsms)))))))

(defun mevedel-tools--current-chat-buffer ()
  "Return the chat buffer associated with the current tool call.
Prefers the FSM bound during dispatch; falls back to
`current-buffer'."
  (or (when-let* ((fsm mevedel-tools--current-fsm)
                  (info (gptel-fsm-info fsm))
                  (buf (plist-get info :buffer))
                  ((buffer-live-p buf)))
        buf)
      (current-buffer)))

(defun mevedel-tools--send-message (args)
  "Queue a message on a recipient agent's mailbox.
ARGS is a plist with :to (recipient name or id) and :message (body).
Returns a short confirmation string.  The message is delivered
asynchronously on the recipient's next FSM turn via
`mevedel-tools--handle-message-inject'."
  (let ((to (plist-get args :to))
        (body (plist-get args :message)))
    (unless (and (stringp to) (not (string-empty-p to)))
      (error "Parameter `to' is required"))
    (unless (and (stringp body) (not (string-empty-p body)))
      (error "Parameter `message' is required"))
    (let* ((chat-buffer (mevedel-tools--current-chat-buffer))
           (target (mevedel-tools--resolve-recipient to chat-buffer))
           (sender (mevedel-tools--sender-name
                    (mevedel-tools--current-deferred-context))))
      (unless target
        (error "Unknown recipient: %s" to))
      (mevedel-tools--ctx-push-message
       target
       (list :from sender :body body :timestamp (current-time)))
      (format "Message delivered to %s." to))))

(defun mevedel-tools--handle-message-inject (fsm)
  "WAIT-state handler: drain FSM's inbox into the next request.

Runs before `gptel--handle-wait' fires the HTTP request.  For the
context that owns FSM (session or agent invocation), reads the
`messages' mailbox, wraps each queued message in an
`<agent-message from=\"...\">' block, and appends a single user-role
message to `info :data :messages' via `gptel--inject-prompt'.  The
mailbox is cleared after draining so each message is delivered
exactly once."
  (when-let* ((ctx (mevedel-tools--deferred-context-for fsm))
              (messages (mevedel-tools--ctx-messages ctx)))
    (let* ((info (gptel-fsm-info fsm))
           (data (plist-get info :data))
           (blocks (mapcar
                    (lambda (msg)
                      (format "<agent-message from=\"%s\">\n%s\n</agent-message>"
                              (or (plist-get msg :from) "unknown")
                              (or (plist-get msg :body) "")))
                    messages)))
      (when data
        (gptel--inject-prompt
         (plist-get info :backend) data
         (list :role "user"
               :content (string-join blocks "\n\n"))))
      (setf (mevedel-tools--ctx-messages ctx) nil))))


(provide 'mevedel-tools)
;;; mevedel-tools.el ends here
