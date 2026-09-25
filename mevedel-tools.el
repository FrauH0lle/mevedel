;;; mevedel-tools.el -- Tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Tool aggregator.  `require's every `mevedel-tool-*' module and exposes
;; `mevedel-tools-register' as the single initializer for the complete
;; built-in tool surface, including Skill and ListSkills.
;;
;; Also owns specialist discovery (ToolSearch), tool-call context and unknown
;; tool recovery, and request-boundary delivery of steering, mail and rosters.

;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'gptel-request))
(require 'mevedel-tool-registry)

(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'mevedel-agents)
(require 'mevedel-agent-control)
(require 'mevedel-agent-conversation)
(require 'mevedel-interaction-prompt)
(require 'mevedel-permission-prompt)
(require 'mevedel-tool-ask)
(require 'mevedel-tool-code)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-fs)
(require 'mevedel-tool-goal)
(require 'mevedel-tool-introspect)
(require 'mevedel-tool-patch)
(require 'mevedel-tool-ptc)
(require 'mevedel-goal)
(require 'mevedel-tool-skills)
(require 'mevedel-tool-task)
(require 'mevedel-tool-editing)
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

;; `mevedel-compact'
(declare-function mevedel--compact-defer-steering-p
                  "mevedel-compact" (fsm))
(autoload 'mevedel--compact-defer-steering-p "mevedel-compact")

;; `mevedel-mentions'
(declare-function mevedel-mentions-commit-expansion
                  "mevedel-mentions" (session expansion))
(declare-function mevedel-mentions-expand-user-input
                  "mevedel-mentions" (text session))
(autoload 'mevedel-mentions-expand-user-input "mevedel-mentions")

;; `mevedel-reminders'
(declare-function mevedel-reminders-stage-entry
                  "mevedel-reminders" (fsm type body &optional commit))
(autoload 'mevedel-reminders-stage-entry "mevedel-reminders")

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue-sweep-request
                  "mevedel-permission-queue"
                  (request-id &optional session no-render))

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-commit
                  "mevedel-prompt-submission" (submission))

;; `mevedel-skills-invoke'
(declare-function mevedel-skills-commit-invoked-records
                  "mevedel-skills-invoke" (session records))

;; `mevedel-view-interaction'
(declare-function mevedel-view-interaction-blocking-p
                  "mevedel-view-interaction" (&optional view-buffer))
(autoload 'mevedel-view-interaction-blocking-p
  "mevedel-view-interaction")

;;
;;; Tool registration

;;;###autoload
(defun mevedel-tools-register ()
  "Register the complete built-in tool surface with the mevedel registry."
  (mevedel-tool-web--register)
  (mevedel-tool-fs--register)
  (mevedel-tool-patch-register)
  (mevedel-tool-code--register)
  (mevedel-tool-exec--register)
  (mevedel-tool-goal--register)
  (mevedel-tool-ui--register)
  (mevedel-tool-skills--register)
  (mevedel-tool-task--register)
  (mevedel-tool-editing--register)
  (mevedel-tool-ptc--register)
  (mevedel-tool-introspect--register))

(defun mevedel-tools-active-count (&optional buffer)
  "Return the number of active gptel tools in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (length (and (boundp 'gptel-tools) gptel-tools)))
      0)))

(defun mevedel-tools--request-data-set-tools (info)
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
    (unless (plist-member info :mevedel-tool-cache-boundary)
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
        (plist-put info :mevedel-tool-cache-boundary boundary)))
    (when (and (vectorp parsed) (> (length parsed) 0))
      (pcase (plist-get info :mevedel-tool-cache-boundary)
        (`(inline . ,annotation)
         (let ((index (1- (length parsed))))
           (aset parsed index (append (aref parsed index)
                                      (copy-tree annotation t)))))
        (`(suffix . ,annotation)
         (setq parsed (vconcat parsed (vector (copy-tree annotation t)))))))
    (plist-put container :tools parsed)))

(defun mevedel-tools--handle-plan-tool-filter (fsm)
  "Apply Plan and Goal request-time tool visibility to FSM."
  (let* ((info (gptel-fsm-info fsm))
         (invocation (plist-get info :mevedel-agent-invocation))
         (buffer (plist-get info :buffer))
         (session
          (or (and (mevedel-agent-invocation-p invocation)
                   (mevedel-agent-invocation-parent-session invocation))
              (and (buffer-live-p buffer)
                   (buffer-local-value 'mevedel--session buffer))))
         (request
          (and (buffer-live-p buffer)
               (buffer-local-value 'mevedel--current-request buffer)))
         (directive-plan-p
          (mevedel-plan-directive-p session request))
         (plan-read-only-p
          (or (and session (mevedel-session-plan-mode session))
              (and (mevedel-request-p request)
                   (mevedel-request-plan-read-only request))
              (and invocation
                   (mevedel-agent-invocation-plan-read-only invocation))))
         (apply-patch-visible-p
          (and plan-read-only-p (not directive-plan-p)))
         (tools (plist-get info :tools))
         (goal-tools
          (cl-union
           (plist-get info :mevedel-goal-tools)
           (cl-remove-if-not
            (lambda (tool)
              (member (gptel-tool-name tool) mevedel-tool-goal-names))
            tools)
           :key #'gptel-tool-name :test #'equal)))
    ;; Goal creation can activate UpdateGoal within this same request.  Keep
    ;; the offered Goal schemas so a previous WAIT's filtering is reversible.
    (when goal-tools
      (plist-put info :mevedel-goal-tools goal-tools)
      (setq tools (append tools (cl-set-difference
                                 goal-tools tools
                                 :key #'gptel-tool-name :test #'equal))))
    (when tools
      (let ((filtered
             (cl-remove-if
              (lambda (tool)
                (let ((name (gptel-tool-name tool)))
                  (or
                   (not (mevedel-tool-goal-available-p name fsm))
                   (and session
                        plan-read-only-p
                        (when-let* ((registered
                                    (mevedel-tool-get name)))
                          (if (equal name "ApplyPatch")
                              (not apply-patch-visible-p)
                            (or (equal name "Eval")
                                (memq 'edit
                                      (mevedel-tool-groups
                                       registered)))))))))
              tools)))
        (unless (equal filtered (plist-get info :tools))
          (plist-put info :tools filtered)
          (mevedel-tools--request-data-set-tools info))))))


;;
;;; Specialist discovery (ToolSearch)

;;
;;; Catalog ownership
;;
;; Both `mevedel-session' and `mevedel-agent-invocation' carry the
;; same tool-catalog slot.  These accessors dispatch on struct type
;; so the pipeline, ToolSearch, and reminders can all
;; operate on whichever context is current without branching on type.

(defun mevedel-tools--ctx-tool-catalog (ctx)
  "Return CTX's discoverable tool catalog."
  (if (mevedel-agent-invocation-p ctx)
      (mevedel-agent-invocation-tool-catalog ctx)
    (mevedel-session-tool-catalog ctx)))

(gv-define-setter mevedel-tools--ctx-tool-catalog (value ctx)
  (list 'if (list 'mevedel-agent-invocation-p ctx)
        (list 'setf (list 'mevedel-agent-invocation-tool-catalog ctx) value)
        (list 'setf (list 'mevedel-session-tool-catalog ctx) value)))

;;
;;; FSM tracking for pipeline context dispatch

(defvar mevedel-tools--current-fsm nil
  "Dynamically bound to the currently-executing gptel FSM.

Set by `mevedel-tools--handle-tool-use-advice' around
`gptel--handle-tool-use' so the pipeline and ToolSearch can determine
which context (session vs agent invocation) owns the current tool
call.  Nil outside tool dispatch.")

(defun mevedel-tools--unknown-tool-result (ctx name)
  "Return repair guidance for unknown native tool NAME in CTX."
  (format "Error: %s %s. Use ToolSearch(query=%S) for its contract, then ToolCall(expression) to invoke it."
          (if (and ctx
                   (cl-some (lambda (entry) (equal name (cadr (car entry))))
                            (mevedel-tools--ctx-tool-catalog ctx)))
              "Specialist is available through ToolCall:" "Unknown tool")
          name name))

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
         (not (plist-get tool-call :result))
         (not (cl-find-if (lambda (tool) (equal name (gptel-tool-name tool)))
                          tools))
         (not (and (boundp 'gptel--ersatz-json-tool)
                   (equal name gptel--ersatz-json-tool))))))

(defun mevedel-tools--settle-unknown-tool-calls (fsm)
  "Convert unresolved unknown tool-use entries in FSM into errors."
  (when-let* ((info (gptel-fsm-info fsm)))
    (let ((ctx (mevedel-tools--context-for fsm))
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
before ORIG-FUN so mevedel can preserve specialist guidance before
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

(defun mevedel-tools--context-for (fsm)
  "Return the tool context (invocation or session) for FSM.

First checks FSM's info plist for an attached
`mevedel-agent-invocation'.  Falls back to the request buffer's
local `mevedel--agent-invocation' before its parent
`mevedel--session', because agent transcript buffers intentionally
carry both."
  (when fsm
    (let* ((info (gptel-fsm-info fsm))
           (inv (plist-get info :mevedel-agent-invocation))
           (buffer (plist-get info :buffer)))
      (or (and (mevedel-agent-invocation-p inv) inv)
          (mevedel-tools--buffer-local-agent-invocation buffer)
          (mevedel-tools--buffer-local-session buffer)))))

(defun mevedel-tools--current-context ()
  "Return the tool context for the currently-executing tool call.

Prefers `mevedel-tools--current-fsm' (set during tool dispatch).
Falls back to the current buffer's `mevedel--agent-invocation' before
`mevedel--session' when no FSM is bound (e.g., direct calls from
tests or tool dispatch paths already inside an agent buffer)."
  (if mevedel-tools--current-fsm
      (mevedel-tools--context-for mevedel-tools--current-fsm)
    (or (and (boundp 'mevedel--agent-invocation)
             (mevedel-agent-invocation-p mevedel--agent-invocation)
             mevedel--agent-invocation)
        (and (boundp 'mevedel--session) mevedel--session))))

(defun mevedel-tools--search-catalog (ctx query)
  "Search CTX's catalog using case-insensitive OR terms from QUERY.
An exact name or category/name selects only those identities for that
term.  Otherwise match substrings in names, summaries and groups.
Return each (TOOL-PATH . SHORT-DESCRIPTION) once, exact identities first,
with catalog order preserved within each group."
  (let* ((catalog (mevedel-tools--ctx-tool-catalog ctx))
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
                               (tool (mevedel-tool-get (cadr path) (car path)))
                               (groups (and tool (mevedel-tool-groups tool)))
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

(defun mevedel-tools--tool-search-format-entry (entry)
  "Return the complete callable contract for catalog ENTRY."
  (let* ((path (car entry))
         (tool (mevedel-tool-get (cadr path) (car path)))
         (name (mevedel-tool-call-name tool)))
    (format "%s\n\n%s\n\nArguments (name type requirement description schema):\n%S\n\nCalling signature: %s\n\nExpression template (replace placeholders using the contract above):\n%s\n%s"
            name (or (mevedel-tool-prompt tool) (mevedel-tool-description tool)) (mevedel-tool-args tool)
            (mevedel-tool-ptc--tool-declaration name)
            (mevedel-tool-ptc--call-template name)
            (if (mevedel-tool-ptc--composable-p name)
                "May be composed with other tool calls."
              "Standalone only: use this single call as the entire expression."))))

(defun mevedel-tools--tool-search-report (matches catalog)
  "Return complete contracts or a bounded summary catalog for MATCHES.
On a miss, suggest search terms from the effective callable CATALOG."
  (cond
   ((null matches)
    (if (null catalog)
        "No matching tools found."
      (let (terms)
        (dolist (entry catalog)
          (let* ((path (car entry))
                 (tool (mevedel-tool-get (cadr path) (car path))))
            (cl-pushnew (car path) terms :test #'equal)
            (dolist (group (mevedel-tool-groups tool))
              (cl-pushnew (symbol-name group) terms :test #'equal))))
        (setq terms (sort terms #'string<))
        (format "No matching tools found. Try a capability keyword or one of these categories/groups%s: %s."
                (if (> (length terms) 20)
                    (format " (first 20 of %d)" (length terms)) "")
                (mapconcat #'identity (seq-take terms 20) ", ")))))
   ((<= (length matches) 3)
    (concat (mapconcat #'mevedel-tools--tool-search-format-entry matches "\n\n---\n\n")
            "\n\nFill the expression template with actual values and pass it to ToolCall(expression)."))
   (t
    (format "Found %d tools%s:\n%s\n\nSearch one or two exact names to retrieve their full contracts."
            (length matches) (if (> (length matches) 20) "; showing the first 20" "")
            (mapconcat
             (lambda (entry)
               (let ((tool (mevedel-tool-get (cadr (car entry)) (car (car entry)))))
                 (format "- %s%s" (mevedel-tool-call-name tool)
                         (if (cdr entry) (concat ": " (cdr entry)) ""))))
             (seq-take matches 20) "\n")))))

(cl-defun mevedel-tools--tool-search (callback query)
  "Retrieve contracts matching QUERY and deliver them through CALLBACK."
  (mevedel-tools--validate-params callback mevedel-tools--tool-search
                                  (query (stringp . "string")))
  (let* ((ctx (mevedel-tools--current-context))
         (roster (mevedel-tool-ptc--roster))
         (catalog
          (seq-filter
           (lambda (entry)
             (when-let* ((tool (mevedel-tool-get (cadr (car entry)) (car (car entry)))))
               (member (mevedel-tool-call-name tool) roster)))
           (and ctx (mevedel-tools--ctx-tool-catalog ctx))))
         (matches
          (seq-filter (lambda (entry) (member entry catalog))
                      (and ctx (mevedel-tools--search-catalog ctx query)))))
    (funcall callback (mevedel-tools--tool-search-report matches catalog))))


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
    ((or 'EXECUTION 'MAIL)
     (format "<agent-message type=\"%s\" sender=\"%s\" recipient=\"%s\">\n%s\n</agent-message>"
             (plist-get msg :type)
             (plist-get msg :sender)
             (plist-get msg :recipient)
             (mevedel-tools--mailbox-body-escape
              (or (plist-get msg :payload) ""))))
    ('USER
     (or (plist-get msg :payload) ""))
    (_ (error "Unknown agent mailbox record: %S" (plist-get msg :type)))))

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
                   (marker (mevedel--active-response-marker info buf))
                   (transcript-block
                    (or (plist-get message :transcript-payload) block)))
              (mevedel--split-open-reasoning-before-user-input info)
              (when-let* ((range
                           (mevedel--insert-user-role-block-at-marker
                            transcript-block marker)))
                (save-excursion
                  (goto-char (cdr range))
                  (dolist (audit (plist-get message :hook-audits))
                    (insert (mevedel--format-hook-audit-record audit)))
                  (when marker
                    (set-marker marker (point)))))))
        (error
         (message "mevedel: insert session injected prompt failed: %S"
                  err))))))

(defun mevedel-tools--pending-steering-p (info)
  "Return non-nil when INFO's root request has pending steering."
  (when-let* ((buffer (plist-get info :buffer))
              ((buffer-live-p buffer))
              ((not (mevedel-tools--buffer-local-agent-invocation buffer)))
              (session (mevedel-tools--buffer-local-session buffer))
              (request-id (plist-get info :mevedel-request-id)))
    (cl-some
     (lambda (entry)
       (equal request-id (plist-get entry :request-id)))
     (mevedel-session-pending-steering session))))

(defun mevedel-tools--handle-steering-inject
    (fsm &optional skip-compaction-gate)
  "WAIT-state handler: inject root steering for FSM's request.
SKIP-COMPACTION-GATE avoids repeating a completed automatic compaction gate."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (session
          (and (buffer-live-p buffer)
               (not (mevedel-tools--buffer-local-agent-invocation buffer))
               (mevedel-tools--buffer-local-session buffer)))
         (request-id (plist-get info :mevedel-request-id))
         (paused
          (and session
               (mevedel-session-pending-input-delivery-paused-p session)))
         (interaction
          (and session
               (buffer-live-p buffer)
               (when-let* ((view (buffer-local-value
                                  'mevedel--view-buffer buffer))
                           ((buffer-live-p view)))
                 (mevedel-view-interaction-blocking-p view))))
         (matching
          (and session request-id
               (cl-remove-if-not
                (lambda (entry)
                  (equal request-id (plist-get entry :request-id)))
                (mevedel-session-pending-steering session))))
         (compaction
          (and (not skip-compaction-gate)
               matching
               (mevedel--compact-defer-steering-p fsm)))
         (snapshot
          (and (not paused) (not interaction) (not compaction) matching)))
    (plist-put info :mevedel-pending-input-hold
               (and matching (or paused interaction) t))
    (when snapshot
      (let ((backend (plist-get info :backend))
            (data (plist-get info :data)))
        (when (eq (car (plist-get info :history)) 'TYPE)
          (when-let* ((start
                       (or (plist-get info
                                      :mevedel-steering-response-start)
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
        (dolist (entry snapshot)
          ;; Expansion reads the dropped file through a real Read check, so
          ;; the grant is provisional until the prompt reaches the request.
          (let ((restore
                 (copy-sequence
                  (mevedel-session-active-dropped-file-grants session)))
                delivered)
            (unwind-protect
                (let* ((input (or (plist-get entry :model-input)
                                  (plist-get entry :input)))
                       (_
                        (mevedel-session-activate-dropped-file-grants
                         session
                         (plist-get entry :dropped-file-grants)))
                       (expansion
                        (with-current-buffer buffer
                          (mevedel-mentions-expand-user-input input session)))
                       (media-contexts
                        (plist-get expansion :media-contexts))
                       (block (plist-get expansion :text))
                       (prompt
                        (car (gptel--parse-list
                              backend (list (cons 'prompt block))))))
                  (when media-contexts
                    (error "Media steering cannot be delivered"))
                  (gptel--inject-prompt backend data prompt)
                  ;; Mention reminders ride the reminder injector,
                  ;; which runs later in this same WAIT.
                  (dolist (item (plist-get expansion :reminder-items))
                    (mevedel-reminders-stage-entry
                     fsm (or (plist-get item :key) 'mention)
                     (plist-get item :body)))
                  (setq delivered t)
                  (mevedel-session-set-pending-inputs
                   session 'steering
                   (cl-remove
                    (plist-get entry :id)
                    (mevedel-session-pending-steering session)
                    :key (lambda (pending)
                           (plist-get pending :id))
                    :test #'equal))
                  (mevedel-tools--insert-session-injected-prompt
                   session fsm entry
                   (or (plist-get entry :transcript-payload)
                       (plist-get entry :input)))
                  ;; The steering prompt is already in the payload, so
                  ;; the dedup commit may run directly.
                  (mevedel-mentions-commit-expansion session expansion)
                  (mevedel-skills-commit-invoked-records
                   session
                   (plist-get (plist-get entry :request-context)
                              :invoked-skills))
                  (when-let* ((submission (plist-get entry :submission)))
                    (mevedel-prompt-submission-commit submission)))
              ;; A delivered entry keeps its grant: the model can act on the
              ;; prompt even if a later commit step fails.
              (unless delivered
                (mevedel-session--set-active-dropped-file-grants
                 session restore)))))
        (when-let* ((marker
                     (mevedel--active-response-marker info buffer)))
          (plist-put info :mevedel-steering-response-start
                     (copy-marker marker nil)))))))

(defun mevedel-tools--handle-message-inject (fsm)
  "WAIT-state handler: drain FSM's inbox into the next request.

Runs before `gptel--handle-wait' fires the HTTP request.  For the
context that owns FSM, injects each unread record as a separate user-role
communication block, and then removes it from the retained FIFO.  Each
injected block is also written to the owning transcript, preserving the
model-visible communication in conversation history."
  (when-let* ((ctx (mevedel-tools--context-for fsm)))
    (let* ((agent-p (mevedel-agent-invocation-p ctx))
           (messages (mevedel-agent-control-context-mailbox ctx))
           (info (gptel-fsm-info fsm))
           (data (plist-get info :data))
           (prepend-p
            (and agent-p
                 (zerop (or (mevedel-agent-invocation-turn-count ctx) 0)))))
      (when (and messages data)
        (when agent-p
          (mevedel--split-open-reasoning-before-user-input info))
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
            (mevedel--active-response-marker
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
  (when-let* ((ctx (mevedel-tools--context-for fsm))
              (session
               (if (mevedel-session-p ctx)
                   ctx
                 (mevedel-agent-invocation-parent-session ctx)))
              (parent-path
               (mevedel-agent-control-context-path ctx)))
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
          (mevedel-reminders-stage-entry
           fsm 'agent-roster
           (concat
            "<agent-roster>\n"
            (if initialized-p "New direct child agents:\n" "Direct child agents:\n")
            (mapconcat
             (lambda (entry)
               (format "- `%s` (`%s`)"
                       (plist-get entry :path) (plist-get entry :role)))
             new "\n")
            "\n</agent-roster>")
           (lambda ()
             (setf (gptel-fsm-info fsm)
                   (plist-put (gptel-fsm-info fsm)
                              :mevedel-agent-child-paths paths)))))))))


(defun mevedel-tools--handle-agent-turn-terminal (fsm)
  "Sweep pending human interactions owned by FSM's settling request."
  (let* ((info (gptel-fsm-info fsm))
         (request-id (plist-get info :mevedel-request-id))
         (ctx (mevedel-tools--context-for fsm)))
    (when (and ctx
               request-id
               (fboundp 'mevedel-agent-invocation-p)
               (mevedel-agent-invocation-p ctx)
               (fboundp 'mevedel-permission-queue-sweep-request))
      (let ((parent-session
             (mevedel-agent-invocation-parent-session ctx)))
        (when parent-session
          (mevedel-permission-queue-sweep-request
           request-id parent-session))))))


(provide 'mevedel-tools)
;;; mevedel-tools.el ends here
