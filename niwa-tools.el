;;; niwa-tools.el -- Tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Tool aggregator.  Loads the complete registration catalog and exposes
;; `niwa-tools-register' as the single initializer for the complete
;; built-in tool surface, including Skill and ListSkills.
;;
;; Also owns specialist discovery (ToolSearch), tool-call context and unknown
;; tool recovery, and request-boundary delivery of steering, mail and rosters.

;;; Code:

(require 'niwa-engine)

(require 'cl-lib)
(eval-when-compile
  (require 'gptel-request))
(require 'niwa-tool-registry)

(require 'niwa-structs)
(require 'niwa-utilities)
(require 'niwa-agents)
(require 'niwa-agent-control)
(require 'niwa-agent-conversation)
(require 'niwa-tool-ask)
(require 'niwa-tool-code)
(require 'niwa-tool-exec-registration)
(require 'niwa-tool-fs)
(require 'niwa-tool-goal)
(require 'niwa-tool-introspect)
(require 'niwa-tool-patch-registration)
(require 'niwa-tool-ptc)
(require 'niwa-tool-schedule)
(require 'niwa-goal)
(require 'niwa-prompt-submission)
(require 'niwa-transcript-audit)
(require 'niwa-tool-skills)
(require 'niwa-tool-task)
(require 'niwa-tool-note)
(require 'niwa-tool-notify)
(require 'niwa-tool-editing)
(require 'niwa-tool-ui)
(require 'niwa-tool-usage)
(require 'niwa-tool-web-registration)
(require 'xml)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `gptel'
(defvar gptel-tools)

;; `gptel-request'
(declare-function gptel--handle-tool-use "ext:gptel-request" (fsm))
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel--parse-list "ext:gptel-request"
                  (backend prompt-list))
(declare-function gptel--parse-tools "ext:gptel-request" (backend tools))
(declare-function gptel--process-tool-call
                  "ext:gptel-request" (fsm tool-spec tool-call result))
(declare-function gptel--trim-prefixes "ext:gptel-request" (string))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel--ersatz-json-tool)

;; `niwa-compact'
(declare-function niwa--compact-defer-steering-p
                  "niwa-compact" (fsm))
(autoload 'niwa--compact-defer-steering-p "niwa-compact")

;; `niwa-mentions'
(declare-function niwa-mentions-commit-expansion
                  "niwa-mentions" (session expansion))
(declare-function niwa-mentions-expand-user-input
                  "niwa-mentions" (text session &optional fresh-p model))
(declare-function niwa-mentions-wrap-prompt-media
                  "niwa-mentions" (prompt contexts backend model request))
(autoload 'niwa-mentions-expand-user-input "niwa-mentions")
(autoload 'niwa-mentions-commit-expansion "niwa-mentions")

;; `niwa-models'
(declare-function niwa-model-effective-context-window "niwa-models"
                  (&optional model))
(autoload 'niwa-model-effective-context-window "niwa-models")

;; `niwa-reminders'
(declare-function niwa-reminders-stage-entry
                  "niwa-reminders" (fsm type body &optional commit))
(autoload 'niwa-reminders-stage-entry "niwa-reminders")

;; `niwa-permission-queue'
(declare-function niwa-permission-queue-sweep-request
                  "niwa-permission-queue"
                  (request-id &optional session no-render))

;; `niwa-skills-invoke'
(declare-function niwa-skills-commit-invoked-records
                  "niwa-skills-invoke" (session records))
(autoload 'niwa-skills-commit-invoked-records "niwa-skills-invoke")

;; `niwa-view-interaction'
(declare-function niwa-view-interaction-blocking-p
                  "niwa-view-interaction" (&optional view-buffer))
(autoload 'niwa-view-interaction-blocking-p
  "niwa-view-interaction")

;;
;;; Tool registration

;;;###autoload
(defun niwa-tools-register ()
  "Register the complete built-in tool surface with the niwa registry."
  (niwa-tool-web--register)
  (niwa-tool-fs--register)
  (niwa-tool-patch-register)
  (niwa-tool-code--register)
  (niwa-tool-exec--register)
  (niwa-tool-goal--register)
  (niwa-tool-schedule--register)
  (niwa-tool-ui--register)
  (niwa-tool-skills--register)
  (niwa-tool-task--register)
  (niwa-tool-note-register)
  (niwa-tool-notify-register)
  (niwa-tool-editing--register)
  (niwa-tool-ptc--register)
  (niwa-tool-introspect--register)
  (niwa-tool-usage--register))

(defun niwa-tools-active-count (&optional buffer)
  "Return the number of active gptel tools in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (if (buffer-live-p buffer)
        (length (buffer-local-value 'gptel-tools buffer))
      0)))

(defun niwa-tools--request-data-set-tools (info)
  "Serialize INFO's active tools with gptel's chosen cache boundary.
Use cache annotations from the realized request, not current buffer settings.
Keep them request-local across temporary removal of all callable tools."
  (let* ((data (plist-get info :data))
         (parsed (gptel--parse-tools (plist-get info :backend)
                                     (plist-get info :tools)))
         (tool-config (plist-get data :toolConfig))
         (container (if (and (listp tool-config)
                             (plist-member tool-config :tools))
                        tool-config data)))
    (unless (plist-member info :niwa-tool-cache-boundary)
      (let* ((original (plist-get container :tools))
             (last (and (vectorp original) (> (length original) 0)
                        (aref original (1- (length original)))))
             (boundary
              (cond
               ((plist-member last :cache_control)
                (cons 'inline (list :cache_control
                                    (copy-tree (plist-get last :cache_control) t))))
               ((plist-member last :cachePoint)
                (cons 'suffix (copy-tree last t))))))
        (plist-put info :niwa-tool-cache-boundary boundary)))
    (when (and (vectorp parsed) (> (length parsed) 0))
      (pcase (plist-get info :niwa-tool-cache-boundary)
        (`(inline . ,annotation)
         (let ((index (1- (length parsed))))
           (aset parsed index (append (aref parsed index)
                                      (copy-tree annotation t)))))
        (`(suffix . ,annotation)
         (setq parsed (vconcat parsed (vector (copy-tree annotation t)))))))
    (plist-put container :tools parsed)))

(defun niwa-tools-visibility (&optional owner)
  "Return a predicate of tool names visible to OWNER's next model sample.
OWNER is a native gptel FSM or an external turn owner.  Plan sessions and
requests carrying inherited Plan limits hide editing tools except ApplyPatch;
Goal tools follow availability.
Monitor, Notify and Schedule are visible only to the root, never to an agent
invocation.
Without OWNER, use the current conversation, including before admission."
  (let* ((info (and owner (niwa-engine-info owner)))
         (buffer (or (plist-get info :buffer) (current-buffer)))
         (invocation (or (and (niwa-agent-invocation-p owner) owner)
                         (plist-get info :niwa-agent-invocation)
                         (niwa-tools--buffer-local-agent-invocation buffer)))
         (session
          (or (and (niwa-agent-invocation-p invocation)
                   (niwa-agent-invocation-parent-session invocation))
              (and (buffer-live-p buffer)
                   (buffer-local-value 'niwa--session buffer))))
         (request
          (or (and (niwa-request-p owner) owner)
              (and (buffer-live-p buffer)
                   (buffer-local-value 'niwa--current-request buffer))))
         (plan-read-only-p
          (and session
               (or (niwa-session-plan-mode session)
                   (and (niwa-request-p request)
                        (niwa-request-plan-read-only request))
                   (and invocation
                        (niwa-agent-invocation-plan-read-only invocation))))))
    (lambda (name)
      (and (not (and invocation (member name '("Monitor" "Notify" "Schedule"))))
           (or (not (equal name "Notify"))
               (and session (not (niwa-goal--context-summary-request-p info))
                    (or (null request)
                        (and (equal (niwa-request-origin request) "/root")
                             (not (niwa-request-ephemeral-p request))
                             (not (niwa-request-cancelled-p request))))))
           (niwa-tool-goal-available-p name owner)
           (not (and plan-read-only-p
                     (when-let* ((registered (or (niwa-tool-for-call name)
                                                (niwa-tool-get name))))
                       (and (not (equal name "ApplyPatch"))
                            (or (equal name "Eval")
                                (memq 'edit (niwa-tool-groups registered)))))))))))

(defun niwa-tools-native-roster (owner)
  "Return the registered tools for the current `gptel-tools' offered to OWNER.
An external engine fixes its roster for the whole turn, so both Goal mutators
stay offered while either is visible: creating or completing a Goal can enable
the other before the turn ends.  Calls still face current authority.  Capture
OWNER's Goal attribution first; UpdateGoal visibility depends on it."
  (let* ((visible (niwa-tools-visibility owner))
         (mutators '("CreateGoal" "UpdateGoal"))
         (goal-p (cl-some visible mutators)))
    (cl-loop for tool in gptel-tools
             for name = (gptel-tool-name tool)
             when (if (member name mutators) goal-p (funcall visible name))
             collect (or (niwa-tool-for-gptel tool)
                         (error "Tool %s is not registered with niwa" name)))))

(defun niwa-tools--handle-plan-tool-filter (fsm)
  "Apply Plan and Goal request-time tool visibility to FSM."
  (let* ((info (gptel-fsm-info fsm))
         (tools (plist-get info :tools))
         (goal-tools
          (cl-union
           (plist-get info :niwa-goal-tools)
           (cl-remove-if-not
            (lambda (tool)
              (member (gptel-tool-name tool) niwa-tool-goal-names))
            tools)
           :key #'gptel-tool-name :test #'equal)))
    ;; Goal creation can activate UpdateGoal within this same request.  Keep
    ;; the offered Goal schemas so a previous WAIT's filtering is reversible.
    (when goal-tools
      (plist-put info :niwa-goal-tools goal-tools)
      (setq tools (append tools (cl-set-difference
                                 goal-tools tools
                                 :key #'gptel-tool-name :test #'equal))))
    (when tools
      (let* ((visible (niwa-tools-visibility fsm))
             (filtered (cl-remove-if-not
                        (lambda (tool) (funcall visible (gptel-tool-name tool)))
                        tools)))
        (unless (equal filtered (plist-get info :tools))
          (plist-put info :tools filtered)
          (niwa-tools--request-data-set-tools info))))))


;;
;;; Specialist discovery (ToolSearch)

;;
;;; Catalog ownership
;;
;; Both `niwa-session' and `niwa-agent-invocation' carry the
;; same tool-catalog slot.  These accessors dispatch on struct type
;; so the pipeline, ToolSearch, and reminders can all
;; operate on whichever context is current without branching on type.

(defun niwa-tools--ctx-tool-catalog (ctx)
  "Return CTX's discoverable tool catalog."
  (if (niwa-agent-invocation-p ctx)
      (niwa-agent-invocation-tool-catalog ctx)
    (niwa-session-tool-catalog ctx)))

(gv-define-setter niwa-tools--ctx-tool-catalog (value ctx)
  (list 'if (list 'niwa-agent-invocation-p ctx)
        (list 'setf (list 'niwa-agent-invocation-tool-catalog ctx) value)
        (list 'setf (list 'niwa-session-tool-catalog ctx) value)))

;;
;;; Engine tracking for pipeline context dispatch

(defvar niwa-tools--current-engine nil
  "Dynamically bound to the current tool call's engine owner.

Set by native gptel dispatch or the external tool pipeline so
tool handlers and ToolSearch can determine
which context (session vs agent invocation) owns the current tool
call.  Nil outside tool dispatch.")

(defun niwa-tools--unknown-tool-result (ctx name)
  "Return repair guidance for unknown native tool NAME in CTX."
  (format "Error: %s %s. Use ToolSearch(query=%S) for its contract, then ToolCall(expression) to invoke it."
          (if (and ctx
                   (cl-some (lambda (entry) (equal name (cadr (car entry))))
                            (niwa-tools--ctx-tool-catalog ctx)))
              "Specialist is available through ToolCall:" "Unknown tool")
          name name))

(defun niwa-tools--synthetic-unknown-tool (name)
  "Return an unregistered display-only gptel tool for unknown NAME."
  (gptel-make-tool
   :name name
   :function (lambda (&rest _) "")
   :description (format "Synthetic placeholder for unknown tool %s" name)
   :args nil
   :category "niwa"))

(defun niwa-tools--unknown-tool-call-p (tool-call tools)
  "Return non-nil when TOOL-CALL names a missing tool in active TOOLS."
  (let ((name (plist-get tool-call :name)))
    (and name
         (not (plist-get tool-call :result))
         (not (cl-find-if (lambda (tool) (equal name (gptel-tool-name tool)))
                          tools))
         (not (and (boundp 'gptel--ersatz-json-tool)
                   (equal name gptel--ersatz-json-tool))))))

(defun niwa-tools--settle-unknown-tool-calls (fsm)
  "Convert unresolved unknown tool-use entries in FSM into errors."
  (when-let* ((info (gptel-fsm-info fsm)))
    (let ((ctx (niwa-tools--context-for fsm))
          (tools (plist-get info :tools)))
      (dolist (tool-call (plist-get info :tool-use))
        (when (niwa-tools--unknown-tool-call-p tool-call tools)
          (let ((name (plist-get tool-call :name)))
            (gptel--process-tool-call
             fsm
             (niwa-tools--synthetic-unknown-tool name)
             tool-call
             (niwa-tools--unknown-tool-result ctx name))))))))

(defun niwa-tools--handle-tool-use-advice (orig-fun fsm)
  "Dyn-bind `niwa-tools--current-engine' around ORIG-FUN.
Used as an `:around' advice on `gptel--handle-tool-use' so that tool
handlers (via the pipeline) can recover the FSM that triggered them
without threading it through every call site.  Settle unknown tool calls
before ORIG-FUN so niwa can preserve specialist guidance before
gptel's generic unknown-tool fallback consumes those calls."
  (let ((niwa-tools--current-engine fsm))
    (niwa-tools--settle-unknown-tool-calls fsm)
    (funcall orig-fun fsm)))

(advice-add 'gptel--handle-tool-use :around
            #'niwa-tools--handle-tool-use-advice)

(defun niwa-tools--buffer-local-agent-invocation (buffer)
  "Return BUFFER's local agent invocation, when it has one."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (and (niwa-agent-invocation-p niwa--agent-invocation)
           niwa--agent-invocation))))

(defun niwa-tools--buffer-local-session (buffer)
  "Return BUFFER's local session, when it has one."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (and (boundp 'niwa--session)
           niwa--session))))

(defun niwa-tools--context-for (fsm)
  "Return the tool context (invocation or session) for FSM.

First checks FSM's info plist for an attached
`niwa-agent-invocation'.  Falls back to the request buffer's
local `niwa--agent-invocation' before its parent
`niwa--session', because agent transcript buffers intentionally
carry both."
  (when fsm
    (let* ((info (niwa-engine-info fsm))
           (inv (plist-get info :niwa-agent-invocation))
           (buffer (plist-get info :buffer)))
      (or (and (niwa-agent-invocation-p inv) inv)
          (niwa-tools--buffer-local-agent-invocation buffer)
          (niwa-tools--buffer-local-session buffer)))))

(defun niwa-tools--current-context ()
  "Return the tool context for the currently-executing tool call.

Prefers `niwa-tools--current-engine' (set during tool dispatch).
Falls back to the current buffer's `niwa--agent-invocation' before
`niwa--session' when no FSM is bound (e.g., direct calls from
tests or tool dispatch paths already inside an agent buffer)."
  (if niwa-tools--current-engine
      (niwa-tools--context-for niwa-tools--current-engine)
    (or (and (niwa-agent-invocation-p niwa--agent-invocation)
             niwa--agent-invocation)
        (and (boundp 'niwa--session) niwa--session))))

(defun niwa-tools--search-catalog (ctx query)
  "Search CTX's catalog using case-insensitive OR terms from QUERY.
An exact name or category/name selects only those identities for that
term.  Otherwise match substrings in names, summaries and groups.
Return each (TOOL-PATH . SHORT-DESCRIPTION) once, exact identities first,
with catalog order preserved within each group."
  (let* ((catalog (niwa-tools--ctx-tool-catalog ctx))
         (terms (mapcar #'downcase (split-string query nil t)))
         (selected nil)
         (exact-identities nil))
    (dolist (term terms)
      (let ((exact
             (cl-remove-if-not
              (lambda (entry)
                (let ((path (car entry)))
                  (or (equal term (downcase (cadr path)))
                      (equal term (downcase (mapconcat #'identity path "/"))))))
              catalog)))
        (dolist (entry exact)
          (cl-pushnew (car entry) exact-identities :test #'equal))
        (dolist (entry
                 (or exact
                     (cl-remove-if-not
                      (lambda (entry)
                        (let* ((path (car entry))
                               (tool (niwa-tool-get (cadr path) (car path)))
                               (groups (and tool (niwa-tool-groups tool)))
                               (text (concat (mapconcat #'identity path "/") " "
                                             (cdr entry) " "
                                             (mapconcat #'symbol-name groups " "))))
                          (string-match-p (regexp-quote term) (downcase text))))
                      catalog)))
          (cl-pushnew (car entry) selected :test #'equal))))
    (let (seen)
      (cl-stable-sort
       (cl-remove-if-not
        (lambda (entry)
          (when (and (member (car entry) selected)
                     (not (member (car entry) seen)))
            (push (car entry) seen)))
        catalog)
       (lambda (a b)
         (and (member (car a) exact-identities)
              (not (member (car b) exact-identities))))))))

(defun niwa-tools--tool-search-format-entry (entry)
  "Return the complete callable contract for catalog ENTRY."
  (let* ((path (car entry))
         (tool (niwa-tool-get (cadr path) (car path)))
         (name (niwa-tool-call-name tool))
         (args (car (niwa-tool-contextual-contract tool))))
    (format "%s\n\n%s\n\nArguments (name type requirement description schema):\n%S\n\nCalling signature: %s\n\nExpression template (replace placeholders using the contract above):\n%s\n%s"
            name (or (niwa-tool-prompt tool) (niwa-tool-description tool)) args
            (niwa-tool-ptc--tool-declaration name)
            (niwa-tool-ptc--call-template name)
            (if (niwa-tool-ptc--composable-p name)
                "May be composed with other tool calls."
              "Standalone only: use this single call as the entire expression."))))

(defun niwa-tools--tool-search-report (matches catalog)
  "Return complete contracts or a bounded summary catalog for MATCHES.
On a miss, suggest search terms from the effective callable CATALOG."
  (cond
   ((null matches)
    (if (null catalog)
        "No matching tools found."
      (let (terms)
        (dolist (entry catalog)
          (let* ((path (car entry))
                 (tool (niwa-tool-get (cadr path) (car path))))
            (cl-pushnew (car path) terms :test #'equal)
            (dolist (group (niwa-tool-groups tool))
              (cl-pushnew (symbol-name group) terms :test #'equal))))
        (setq terms (sort terms #'string<))
        (format "No matching tools found. Try a capability keyword or one of these categories/groups%s: %s."
                (if (> (length terms) 20)
                    (format " (first 20 of %d)" (length terms)) "")
                (mapconcat #'identity (seq-take terms 20) ", ")))))
   ((<= (length matches) 3)
    (concat (mapconcat #'niwa-tools--tool-search-format-entry matches "\n\n---\n\n")
            "\n\nFill the expression template with actual values and pass it to ToolCall(expression)."))
   (t
    (format "Found %d tools%s:\n%s\n\nCall a listed shape through ToolCall; bracketed arguments are optional. Search one or two exact names for their full contracts."
            (length matches) (if (> (length matches) 20) "; showing the first 20" "")
            (mapconcat
             (lambda (entry)
               (niwa-tool-ptc--tool-declaration
                (niwa-tool-call-name
                 (niwa-tool-get (cadr (car entry)) (car (car entry))))))
             (seq-take matches 20) "\n")))))

(cl-defun niwa-tools--tool-search (callback query)
  "Retrieve contracts matching QUERY and deliver them through CALLBACK."
  (niwa-tools--validate-params callback niwa-tools--tool-search
                                  (query (stringp . "string")))
  (let* ((ctx (niwa-tools--current-context))
         (roster (niwa-tool-ptc--roster))
         (catalog
          (seq-filter
           (lambda (entry)
             (when-let* ((tool (niwa-tool-get (cadr (car entry)) (car (car entry)))))
               (member (niwa-tool-call-name tool) roster)))
           (and ctx (niwa-tools--ctx-tool-catalog ctx))))
         (matches
          (seq-filter (lambda (entry) (member entry catalog))
                      (and ctx (niwa-tools--search-catalog ctx query)))))
    (funcall callback (niwa-tools--tool-search-report matches catalog))))

(defcustom niwa-tools-listing-budget 0.02
  "Fraction of the context window allotted to the specialist tool listing.
The listing names each callable specialist with its argument shape and
summary.  This fraction of the active model's context window, converted to
characters at four characters per token, caps it; tools beyond the cap stay
reachable through ToolSearch."
  :type 'float
  :group 'niwa)

(defun niwa-tools-listing-section ()
  "Return the current conversation's specialist tool listing, or nil.
It lists catalog tools the current role and request may call through
ToolCall, excluding native tools.  Without native ToolCall there is none."
  (let* ((niwa-tools--current-engine
          (cl-find-if
           (lambda (owner)
             (and owner
                  (eq (plist-get (niwa-engine-info owner) :buffer)
                      (current-buffer))))
           (list niwa--agent-invocation
                 (niwa-engine-owner niwa--current-request)
                 niwa-tools--current-engine)))
         (ctx (niwa-tools--current-context))
         (native (niwa-tool-ptc--active-tool-names))
         (roster (niwa-tool-ptc--roster))
         (names
          (and ctx
               (cl-some (lambda (provider)
                          (when-let* ((tool (niwa-tool-for-gptel provider)))
                            (eq tool (niwa-tool-get "ToolCall" "niwa"))))
                        (niwa-tool-ptc--native-tools))
               (delete-dups
                (cl-loop for (path . _) in (niwa-tools--ctx-tool-catalog ctx)
                         for tool = (niwa-tool-get (cadr path) (car path))
                         for name = (and tool (niwa-tool-call-name tool))
                         when (and name (member name roster)
                                   (not (member name native)))
                         collect name)))))
    (when names
      (let ((budget (floor (* niwa-tools-listing-budget
                              (niwa-model-effective-context-window)
                              4)))
            (used 0)
            lines)
        (dolist (name names)
          (let ((line (niwa-tool-ptc--tool-declaration name)))
            (when (<= (+ used (length line) 1) budget)
              (cl-incf used (1+ (length line)))
              (push line lines))))
        (let ((omitted (- (length names) (length lines))))
          (concat
           "## Tools\n"
           "Specialist tools you can call through ToolCall without searching "
           "first, as `(NAME :ARG VALUE ...)`; bracketed arguments are optional. "
           "Tools marked standalone only must be the entire expression. "
           "ToolSearch with an exact name returns the full contract when "
           "argument meaning or usage rules matter.\n\n"
           (string-join (reverse lines) "\n")
           (if (> omitted 0)
               (format "\n\n%d more tools are omitted; find them with ToolSearch."
                       omitted)
             "")))))))


;;
;;; Agent mailbox injection

(defun niwa-tools--mailbox-body-escape (body)
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

(defun niwa-tools--meta-attributes (meta)
  "Return opening-tag attributes for mailbox record META, with a leading space.
META is a plist of structural facts; nil values are omitted."
  (mapconcat
   (lambda (entry)
     (if-let* ((value (plist-get meta (car entry))))
         (format " %s=\"%s\"" (cdr entry)
                 (xml-escape-string
                  (replace-regexp-in-string
                   "[\n\r\t ]+" " " (format "%s" value))))
       ""))
   '((:execution-id . "execution_id") (:description . "description")
     (:command . "command") (:event-count . "event_count")
     (:termination . "termination") (:outcome . "outcome")
     (:exit-code . "exit_code") (:omitted . "omitted")
     (:output-path . "output_path") (:output-bytes . "output_bytes")
     (:omitted-output-bytes . "omitted_output_bytes")
     (:schedule-id . "schedule_id") (:scheduled-at . "scheduled_at")
     (:fired-at . "fired_at") (:overdue-by . "overdue_by"))
   ""))

(defun niwa-tools--message-delivery-block (msg)
  "Return the user-role delivery block for mailbox MSG."
  (pcase (plist-get msg :type)
    ('RESULT
     (format "<agent-result sender=\"%s\" recipient=\"%s\" outcome=\"%s\">\n%s\n</agent-result>"
             (plist-get msg :sender)
             (plist-get msg :recipient)
             (plist-get msg :outcome)
             (niwa-tools--mailbox-body-escape
              (or (plist-get msg :payload) ""))))
    ((or 'EXECUTION 'MAIL 'WAKE_UP)
     (format "<agent-message type=\"%s\" sender=\"%s\" recipient=\"%s\"%s>\n%s\n</agent-message>"
             (plist-get msg :type)
             (plist-get msg :sender)
             (plist-get msg :recipient)
             (niwa-tools--meta-attributes (plist-get msg :meta))
             (niwa-tools--mailbox-body-escape
              (or (plist-get msg :payload) ""))))
    ((or 'MONITOR 'MONITOR_TERMINAL)
     (format "<agent-message type=\"%s\" sender=\"%s\" recipient=\"%s\" execution_id=\"%s\" description=\"%s\"%s>\n%s\n</agent-message>"
             (plist-get msg :type)
             (plist-get msg :sender)
             (plist-get msg :recipient)
             (xml-escape-string (or (plist-get msg :execution-id) ""))
             (xml-escape-string
              (replace-regexp-in-string
               "[\n\r]+" " " (or (plist-get msg :description) "")))
             (niwa-tools--meta-attributes (plist-get msg :meta))
             (niwa-tools--mailbox-body-escape
              (or (plist-get msg :payload) ""))))
    ('USER
     (or (plist-get msg :payload) ""))
    (_ (error "Unknown agent mailbox record: %S" (plist-get msg :type)))))

(defun niwa-tools--message-transcript-block (message block)
  "Return MESSAGE's transcript form of model-visible BLOCK with its receipt.
Only the durable identity is retained in hidden metadata; payloads remain in
their normal user-visible communication blocks."
  (concat (or (plist-get message :transcript-payload) block)
          (mapconcat #'niwa--format-hook-audit-record
                     (plist-get message :hook-audits) "")
          (when (memq (plist-get message :type)
                      '(EXECUTION MAIL MONITOR MONITOR_TERMINAL RESULT USER WAKE_UP))
            (niwa-agent-control-message-receipt message))))

(defun niwa-tools--mailbox-new-messages (context buffer)
  "Return CONTEXT's FIFO mail not already represented in BUFFER.
Restored durable mail can overlap a receiving transcript that committed before
its mailbox projection.  Consume that overlap by identity, not payload text."
  (let ((messages (niwa-agent-control-context-mailbox context)))
    (if (not (and messages (buffer-live-p buffer)))
        messages
      (let ((delivered (make-hash-table :test #'equal)) repeated fresh)
        (with-current-buffer buffer
          (dolist (span (niwa-transcript-audit-buffer-spans 'mailbox-receipt))
            (when-let* ((id (plist-get (plist-get span :record) :id)))
              (puthash id t delivered))))
        (dolist (message messages)
          (if (gethash (plist-get message :id) delivered)
              (push message repeated)
            (push message fresh)))
        (when repeated
          (niwa-agent-control-acknowledge-mail context repeated))
        (nreverse fresh)))))

(defun niwa-tools--insert-session-injected-prompt
    (session fsm message block)
  "Insert injected MESSAGE for SESSION and FSM into the data buffer.
BLOCK is the model-visible form.  MESSAGE may provide a separate transcript
payload and hidden hook audit records.

`gptel--inject-prompt' mutates the realized request payload, but
does not write that synthetic user-role message back to the data
buffer.  This helper keeps the main transcript and view buffer in
sync with what the model actually saw.  Return non-nil on successful insertion."
  (when (and (niwa-session-p session)
             (stringp block)
             (not (string-empty-p block)))
    (when-let* ((info (and fsm (gptel-fsm-info fsm)))
                (buf (plist-get info :buffer))
                ((buffer-live-p buf))
                ((not (niwa-tools--buffer-local-agent-invocation buf)))
                ((eq session (niwa-tools--buffer-local-session buf))))
      (condition-case err
          (with-current-buffer buf
            (let* ((inhibit-read-only t)
                   (marker (niwa--active-response-marker info buf))
                   (transcript-block
                    (niwa-tools--message-transcript-block message block)))
              (niwa--split-open-reasoning-before-user-input info)
              (and (niwa--insert-user-role-block-at-marker
                    transcript-block marker)
                   t)))
        (error
         (niwa-diagnostic-message
          (niwa-diagnostic-scope session)
          "insert session injected prompt failed: %S" err)
         nil)))))

;;
;;; Steering

(defun niwa-tools-steering-entries (session request-id)
  "Return SESSION's pending steering for REQUEST-ID in FIFO order."
  (and session request-id
       (cl-remove-if-not
        (lambda (entry) (equal request-id (plist-get entry :request-id)))
        (niwa-session-pending-steering session))))

(defun niwa-tools-steering-claim (session entries)
  "Reserve SESSION's steering ENTRIES before expansion or native delivery.
The whole snapshot is owned before any file read can process user input.
Ownership ends at receipt/commit or dead-turn settlement, not at native send."
  (dolist (entry entries)
    (unless (and (memq entry (niwa-session-pending-steering session))
                 (not (plist-get entry :delivering)))
      (user-error "Steering input is no longer available for dispatch")))
  (dolist (entry entries)
    (plist-put entry :delivering t)))

(defun niwa-tools--assert-steering-claim (session entry)
  "Refuse a stale delivery of SESSION's claimed steering ENTRY."
  (unless (and (memq entry (niwa-session-pending-steering session))
               (plist-get entry :delivering))
    (user-error "Steering ownership changed before delivery")))

(defun niwa-tools-steering-held-p (session buffer)
  "Return non-nil while SESSION's steering delivery is held.
The cockpit or a failure pauses delivery; an unresolved interaction in root
data BUFFER's view postpones it."
  (or (niwa-session-pending-input-delivery-paused-p session)
      (when-let* (((buffer-live-p buffer))
                  (view (buffer-local-value 'niwa--view-buffer buffer))
                  ((buffer-live-p view)))
        (niwa-view-interaction-blocking-p view))))

(defun niwa-tools-steering-expand (session entry &optional fresh model)
  "Expand steering ENTRY for SESSION from the current request buffer.
Expansion reads a dropped file through a real Read check, so ENTRY's grants
are activated first; the caller restores them when delivery fails.  FRESH
excludes historical mention deduplication.  MODEL optionally supplies the
captured request's media capabilities; otherwise use the ambient model.
The caller delivers the
expansion's `:media-contexts' with the prompt text."
  (niwa-session-activate-dropped-file-grants
   session (plist-get entry :dropped-file-grants))
  (let ((claimed (plist-get entry :delivering))
        (expansion (niwa-mentions-expand-user-input
                    (or (plist-get entry :model-input) (plist-get entry :input))
                    session fresh model)))
    ;; Expansion can process an abort, whose settlement replaces the entries.
    (when claimed (niwa-tools--assert-steering-claim session entry))
    expansion))

(defun niwa-tools-steering-commit (session entry expansion &optional fresh)
  "Consume delivered steering ENTRY and commit its EXPANSION for SESSION.
FRESH skips the mention deduplication a separate conversation never saw."
  (niwa-tools--assert-steering-claim session entry)
  (niwa-session-set-pending-inputs
   session 'steering
   (cl-remove (plist-get entry :id) (niwa-session-pending-steering session)
              :key (lambda (pending) (plist-get pending :id)) :test #'equal))
  (unless fresh
    (niwa-mentions-commit-expansion session expansion))
  (niwa-skills-commit-invoked-records
   session (plist-get (plist-get entry :request-context) :invoked-skills))
  (when-let* ((submission (plist-get entry :submission)))
    (niwa-prompt-submission-commit submission)))

(defun niwa-tools--pending-steering-p (info)
  "Return non-nil when INFO's root request has pending steering."
  (when-let* ((buffer (plist-get info :buffer))
              ((buffer-live-p buffer))
              ((not (niwa-tools--buffer-local-agent-invocation buffer)))
              (session (niwa-tools--buffer-local-session buffer)))
    (niwa-tools-steering-entries session (plist-get info :niwa-request-id))))

(defun niwa-tools--handle-steering-inject
    (fsm &optional skip-compaction-gate)
  "WAIT-state handler: inject root steering for FSM's request.
SKIP-COMPACTION-GATE avoids repeating a completed automatic compaction gate."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (session
          (and (buffer-live-p buffer)
               (not (niwa-tools--buffer-local-agent-invocation buffer))
               (niwa-tools--buffer-local-session buffer)))
         (held (and session (niwa-tools-steering-held-p session buffer)))
         (matching (niwa-tools-steering-entries
                    session (plist-get info :niwa-request-id)))
         (compaction
          (and (not skip-compaction-gate)
               matching
               (niwa--compact-defer-steering-p fsm)))
         (snapshot (and (not held) (not compaction) matching)))
    (plist-put info :niwa-pending-input-hold (and matching held t))
    (when snapshot
      (let ((backend (plist-get info :backend))
            (data (plist-get info :data)))
        (when (eq (car (plist-get info :history)) 'TYPE)
          (when-let* ((start
                       (or (plist-get info
                                      :niwa-steering-response-start)
                           (plist-get info :position)))
                      (end (plist-get info :tracking-marker))
                      ((and (markerp start) (marker-position start)
                            (markerp end) (marker-position end)))
                      (response
                       (with-current-buffer buffer
                         (gptel--trim-prefixes
                          (buffer-substring-no-properties start end)))))
            (gptel--inject-prompt
             backend data
             (car (gptel--parse-list
                   backend (list (cons 'response response)))))))
        (niwa-tools-steering-claim session snapshot)
        (dolist (entry snapshot)
          ;; The grant is provisional until the prompt reaches the request.
          (let ((restore
                 (copy-sequence
                  (niwa-session-active-dropped-file-grants session)))
                delivered finished)
            (unwind-protect
                (let* ((expansion
                        (with-current-buffer buffer
                          (niwa-tools-steering-expand
                           session entry nil (plist-get info :model))))
                       (prompt
                        (car (gptel--parse-list
                              backend
                              (list (cons 'prompt (plist-get expansion :text))))))
                       (media (plist-get expansion :media-contexts)))
                  (when media
                    (setq prompt
                          (niwa-mentions-wrap-prompt-media
                           prompt media backend (plist-get info :model)
                           (buffer-local-value
                            'niwa--current-request buffer))))
                  (niwa-tools--assert-steering-claim session entry)
                  (gptel--inject-prompt backend data prompt)
                  (setq delivered t)
                  ;; Mention reminders ride the reminder injector,
                  ;; which runs later in this same WAIT.
                  (dolist (item (plist-get expansion :reminder-items))
                    (niwa-reminders-stage-entry
                     fsm (or (plist-get item :key) 'mention)
                     (plist-get item :body)))
                  (niwa-tools--insert-session-injected-prompt
                   session fsm entry
                   (or (plist-get entry :transcript-payload)
                       (plist-get entry :input)))
                  ;; The steering prompt is already in the payload, so
                  ;; the dedup commit may run directly.
                  (niwa-tools-steering-commit session entry expansion)
                  (setq finished t))
              ;; An exception leaves later snapshot members unattempted.
              ;; Release those claims, but never withdraw an injected prompt.
              (unless finished
                (dolist (unattempted (cdr (memq entry snapshot)))
                  (plist-put unattempted :delivering nil)))
              ;; A delivered entry keeps its grant: the model can act on the
              ;; prompt even if a later commit step fails.
              (unless delivered
                (plist-put entry :delivering nil)
                (niwa-session--set-active-dropped-file-grants
                 session restore)))))
        (when-let* ((marker
                     (niwa--active-response-marker info buffer)))
          (plist-put info :niwa-steering-response-start
                     (copy-marker marker nil)))))))

(defun niwa-tools--handle-message-inject (fsm)
  "WAIT-state handler: drain FSM's inbox into the next request.

Runs before `gptel--handle-wait' fires the HTTP request.  For the
context that owns FSM, injects each unread record as a separate user-role
communication block, and then removes it from the retained FIFO.  Each
injected block is also written to the owning transcript, preserving the
model-visible communication in conversation history."
  (when-let* ((ctx (niwa-tools--context-for fsm)))
    (let* ((agent-p (niwa-agent-invocation-p ctx))
           (info (gptel-fsm-info fsm))
           (messages (niwa-tools--mailbox-new-messages
                      ctx (plist-get info :buffer)))
           (data (plist-get info :data)))
      (when (and messages data)
        (when agent-p
          (niwa--split-open-reasoning-before-user-input info))
        (cl-loop
         for message in messages
         for block = (niwa-tools--message-delivery-block message)
         for sender = (or (plist-get message :sender) "unknown")
         do
         (when agent-p
           (niwa-agent-conversation-record-activity
            ctx
            (list :type 'message
                  :from sender
                  :summary (format "message from %s" sender)))
           (unless (niwa-agent-conversation-insert-user-block
                    ctx (niwa-tools--message-transcript-block message block)
                    (niwa--active-response-marker
                     info (niwa-agent-invocation-buffer ctx)))
             (error "Mailbox transcript insertion failed")))
         (unless agent-p
           (unless (niwa-tools--insert-session-injected-prompt
                    ctx fsm message block)
             (error "Mailbox transcript insertion failed")))
         (gptel--inject-prompt
          (plist-get info :backend) data
          (list :role "user" :content block))))
      (when (and messages data)
        (niwa-agent-control-acknowledge-mail ctx messages)))))

(defun niwa-tools-agent-roster (owner &optional full-p)
  "Return OWNER's new direct children as reminder body and deferred commit.
The engine commits the captured roster only after acknowledging delivery.
With FULL-P, restore all direct children after context loss."
  (when-let* ((ctx (niwa-tools--context-for owner))
              (session
               (if (niwa-session-p ctx)
                   ctx
                 (niwa-agent-invocation-parent-session ctx)))
              (parent-path
               (niwa-agent-control-context-path ctx)))
    (let* ((info (niwa-engine-info owner))
           (initialized-p
            (and (not full-p) (plist-member info :niwa-agent-child-paths)))
           (children
            (niwa-agent-control-direct-children session parent-path))
           (paths (mapcar (lambda (entry) (plist-get entry :path)) children))
           (known (unless full-p (plist-get info :niwa-agent-child-paths)))
           (new
            (cl-remove-if
             (lambda (entry)
               (member (plist-get entry :path) known))
             children)))
      (when new
        (list :body
              (concat
               "<agent-roster>\n"
               (if initialized-p "New direct child agents:\n" "Direct child agents:\n")
               (mapconcat
                (lambda (entry)
                  (format "- `%s` (`%s`)"
                          (plist-get entry :path) (plist-get entry :role)))
                new "\n")
               "\n</agent-roster>")
              :commit
              (lambda ()
                (setf (niwa-engine-info owner)
                      (plist-put (niwa-engine-info owner)
                                 :niwa-agent-child-paths paths))))))))

(defun niwa-tools--handle-agent-roster-inject (fsm)
  "WAIT-state handler: expose direct children to FSM exactly once."
  (when-let* (((plist-get (gptel-fsm-info fsm) :data))
              (roster (niwa-tools-agent-roster fsm)))
    (niwa-reminders-stage-entry
     fsm 'agent-roster (plist-get roster :body) (plist-get roster :commit))))


(defun niwa-tools--handle-agent-turn-terminal (fsm)
  "Sweep pending human interactions owned by FSM's settling request."
  (let* ((info (gptel-fsm-info fsm))
         (request-id (plist-get info :niwa-request-id))
         (ctx (niwa-tools--context-for fsm)))
    (when (and ctx
               request-id
               (fboundp 'niwa-agent-invocation-p)
               (niwa-agent-invocation-p ctx)
               (fboundp 'niwa-permission-queue-sweep-request))
      (let ((parent-session
             (niwa-agent-invocation-parent-session ctx)))
        (when parent-session
          (niwa-permission-queue-sweep-request
           request-id parent-session))))))


(provide 'niwa-tools)
;;; niwa-tools.el ends here
