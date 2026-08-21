;;; mevedel-permissions.el -- Unified permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Permission preflight and decision facade for all mevedel tools.  It combines
;; mode, rule, and persistent-authority owners with tool policy in one decision
;; chain: extract context -> absolute denies -> tool policy -> permission rules
;; -> mode -> independent filesystem resource authority -> mode/default ask.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-plan-directive-p "mevedel-agents"
                  (&optional session request))
(declare-function mevedel-plan-read-only-request-p "mevedel-agents" ())
(defvar mevedel--agent-invocation)

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-data-buffer
                  "mevedel-permission-mode" ())
(declare-function mevedel-permission-mode-decision
                  "mevedel-permission-mode"
                  (mode read-only-p &optional native-edit-p reviewed-edit-p))
(defvar mevedel-permission-mode)

;; `mevedel-permission-persistence'
(declare-function mevedel-permission-persistence-editable-store
                  "mevedel-permission-persistence" (file &optional target))
(declare-function mevedel-permission-persistence-file
                  "mevedel-permission-persistence" (workspace))
(declare-function mevedel-permission-persistence-load-resource-grants
                  "mevedel-permission-persistence" (workspace))
(declare-function mevedel-permission-persistence-load-rules
                  "mevedel-permission-persistence" (workspace))
(declare-function mevedel-permission-persistence-save-resource-grant
                  "mevedel-permission-persistence" (workspace path access))
(declare-function mevedel-permission-persistence-save-rule
                  "mevedel-permission-persistence"
                  (workspace tool-name action &optional path &rest keys))
(declare-function mevedel-permission-persistence-write-store
                  "mevedel-permission-persistence"
                  (file store &optional target))

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-build-rule
                  "mevedel-permission-rules"
                  (tool-name action spec-key spec-value &rest keys))
(declare-function mevedel-permission-rules-collect-buckets
                  "mevedel-permission-rules"
                  (invocation-rules request-rules session-rules persistent-rules))
(declare-function mevedel-permission-rules-first-action-with-bucket
                  "mevedel-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission-rules-first-deny-bucket
                  "mevedel-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission-rules-merge-resource-grant
                  "mevedel-permission-rules" (grants path access))
(declare-function mevedel-permission-rules-path-in-allowed-roots-p
                  "mevedel-permission-rules" (path roots))
(declare-function mevedel-permission-rules-path-in-exact-allowed-paths-p
                  "mevedel-permission-rules" (path allowed-paths))
(declare-function mevedel-permission-rules-path-protected-p
                  "mevedel-permission-rules" (path &optional target))
(declare-function mevedel-permission-rules-resource-grant
                  "mevedel-permission-rules" (path access))
(declare-function mevedel-permission-rules-resource-granted-p
                  "mevedel-permission-rules" (path access grants))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode-active-p
                  "mevedel-plan-mode" (&optional session))
(declare-function mevedel-plan-mode-exit
                  "mevedel-plan-mode" (&optional session))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-mutation-authority
                  "mevedel-session-artifacts" (session &optional buffer))

;; `mevedel-structs'
(declare-function mevedel-request-goal-plan-read-path
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-one-shot-mutations-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-active-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; setf expander for session struct
(eval-when-compile
  (require 'mevedel-structs))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-check-permission "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-check-permission-async "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-ensure "mevedel-tool-registry" (name))
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-get-domain "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-paths "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-pattern "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-groups "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (cl-x) t)

;; `mevedel-workspace'
(declare-function mevedel--all-allowed-roots
                  "mevedel-workspace" (&optional buffer))


;;
;;; Frozen request authority

(defvar-local mevedel-permission--context-frozen-p nil
  "Non-nil when persistent permission authority must not be reloaded.")

(defvar-local mevedel-permission--frozen-persistent-rules nil
  "Persistent permission rules captured for a frozen request context.")

(defvar-local mevedel-permission--frozen-resource-grants nil
  "Persistent resource grants captured for a frozen request context.")

(defun mevedel-permission-freeze-context (persistent-rules resource-grants)
  "Freeze the current buffer's persistent permission authority.

PERSISTENT-RULES and RESOURCE-GRANTS replace on-disk lookups for the
buffer's lifetime; later persisted changes are not observed.  Callers
own copying the passed structures."
  (setq-local mevedel-permission--context-frozen-p t
              mevedel-permission--frozen-persistent-rules persistent-rules
              mevedel-permission--frozen-resource-grants resource-grants))


(defun mevedel-permission--normalize-outcome (outcome)
  "Return the log-safe decision symbol for permission OUTCOME."
  (pcase outcome
    (`(deny . ,_) 'deny)
    (`(feedback . ,_) 'deny)
    (_ outcome)))

(defun mevedel-permission--decision
    (outcome via &rest props)
  "Return permission decision metadata for OUTCOME through VIA with PROPS."
  (let ((normalized (mevedel-permission--normalize-outcome outcome)))
    (append (list :outcome normalized
                  :raw-outcome outcome
                  :via via)
            props)))

(defun mevedel-permission-decision-raw-outcome (decision)
  "Return the pipeline outcome represented by DECISION."
  (if (and (listp decision) (plist-member decision :outcome))
      (if (plist-member decision :raw-outcome)
          (plist-get decision :raw-outcome)
        (plist-get decision :outcome))
    decision))

(defun mevedel-permission-decision-metadata-p (value)
  "Return non-nil when VALUE is permission decision metadata."
  (and (listp value)
       (keywordp (car-safe value))
       (plist-member value :outcome)
       (plist-member value :via)))

(defun mevedel-permission--metadata-content (content)
  "Return CONTENT marked for permission metadata collection when possible."
  (if (and (listp content) (keywordp (car-safe content)))
      (plist-put (copy-sequence content) :permission-decision-metadata t)
    content))

(defun mevedel-permission--one-shot-mutations-p (request &optional explicit)
  "Return the one-shot mutation policy for REQUEST.
EXPLICIT takes precedence when non-nil."
  (or explicit
      (and request (mevedel-request-one-shot-mutations-p request))))

(defun mevedel-permission--one-shot-prompt-entry (entry &optional data-buffer)
  "Return one-time permission ENTRY owned by DATA-BUFFER."
  (let ((entry (copy-sequence entry)))
    (dolist (pair '((:once-only . t)
                    (:include-always . nil)
                    (:remember-authority-cell . nil)
                    (:reusable-operation-p . nil)))
      (setq entry (plist-put entry (car pair) (cdr pair))))
    (when data-buffer
      (setq entry (plist-put entry :data-buffer data-buffer)))
    entry))

(defun mevedel-permission--one-shot-prompt-outcome (outcome)
  "Collapse reusable permission OUTCOME to its one-time equivalent."
  (pcase outcome
    ((or 'allow-session 'always-allow) 'allow-once)
    ('deny-session 'deny-once)
    (_ outcome)))

(cl-defun mevedel-permission--invocation-context
    (&key tool tool-name args session workspace request invocation buffer
          path pattern domain name mode workspace-root allowed-roots
          exact-allowed-paths invocation-rules request-rules session-rules
          persistent-rules resource-grants permission-request
          one-shot-mutations-p patch-local-only-p warn-no-session-p)
  "Return normalized permission invocation context.

The context concentrates facts shared by the permission decision
chain, prompt queue, and Bash/Eval adapters.

TOOL, TOOL-NAME, ARGS, SESSION, WORKSPACE, REQUEST, INVOCATION, BUFFER,
PATH, PATTERN, DOMAIN, NAME, MODE, WORKSPACE-ROOT, ALLOWED-ROOTS,
EXACT-ALLOWED-PATHS, INVOCATION-RULES, REQUEST-RULES, SESSION-RULES,
PERSISTENT-RULES, RESOURCE-GRANTS, and WARN-NO-SESSION-P provide the
context facts.  ONE-SHOT-MUTATIONS-P defaults from REQUEST.
PATCH-LOCAL-ONLY-P is the prepared ApplyPatch classification used by the Plan
boundary.
PERMISSION-REQUEST admits an interactive request at its hook boundary before
it enters the shared queue."
  (require 'mevedel-permission-persistence)
  (require 'mevedel-tool-registry)
  (setq tool (or tool
                 (and tool-name (mevedel-tool-ensure tool-name)))
        tool-name (or tool-name (and tool (mevedel-tool-name tool))))
  (setq workspace (or workspace
                      (and session (mevedel-session-workspace session)))
        workspace-root
        (or workspace-root
            (and workspace
                 (ignore-errors (mevedel-workspace-root workspace))))
        allowed-roots
        (or allowed-roots
            (when (and workspace (fboundp 'mevedel--all-allowed-roots))
              (ignore-errors (mevedel--all-allowed-roots buffer))))
        exact-allowed-paths
        (or exact-allowed-paths
            (and (equal tool-name "Read")
                 session
                 (mevedel-session-active-dropped-file-grants session)))
        invocation-rules
        (or invocation-rules
            (and invocation
                 (mevedel-agent-invocation-skill-permission-rules
                  invocation)))
        request-rules
        (or request-rules
            (and request (mevedel-request-skill-permission-rules request)))
        session-rules
        (or session-rules
            (and session (mevedel-session-permission-rules session)))
        persistent-rules
        (or persistent-rules
            (and mevedel-permission--context-frozen-p
                 mevedel-permission--frozen-persistent-rules)
            (and (not mevedel-permission--context-frozen-p) workspace
                 (mevedel-permission-persistence-load-rules workspace)))
        resource-grants
        (or resource-grants
            (append (when-let* ((path (and request
                                           (mevedel-request-goal-plan-read-path
                                            request))))
                      (list (list :path path :access 'read)))
                    (and session (mevedel-session-resource-grants session))
                    (and mevedel-permission--context-frozen-p
                         mevedel-permission--frozen-resource-grants)
                    (and (not mevedel-permission--context-frozen-p) workspace
                         (mevedel-permission-persistence-load-resource-grants
                          workspace))))
        one-shot-mutations-p
        (mevedel-permission--one-shot-mutations-p
         request one-shot-mutations-p)
        mode (or mode (and session (mevedel-session-permission-mode session))))
  (when (and warn-no-session-p (not session))
    (display-warning
     'mevedel
     (format "Permission step for %s ran with no session in \
context; falling back to defcustom defaults.  Session-scoped \
rules and the active permission mode are not being consulted.  \
This usually means the tool was dispatched from a buffer whose \
`mevedel--session' was not set; in production that should not \
happen for a non-read-only tool."
             tool-name)
     :warning))
  (let* ((context
          (mevedel-permission--preflight
           tool-name
           :tool-struct tool
           :path path
           :pattern pattern
           :domain domain
           :name name
           :content args
           :invocation-rules invocation-rules
           :request-rules request-rules
           :session-rules session-rules
           :persistent-rules persistent-rules
           :mode mode
           :session session
           :request request
           :workspace-root workspace-root
           :allowed-roots allowed-roots
           :exact-allowed-paths exact-allowed-paths
           :resource-grants resource-grants
           :one-shot-mutations-p one-shot-mutations-p
           :patch-local-only-p patch-local-only-p))
         (path (plist-get context :path))
         (pattern (plist-get context :pattern))
         (domain (plist-get context :domain))
         (name (plist-get context :name))
         (specifier-key (cond (pattern :pattern)
                              (domain :domain)
                              (name :name)
                              (path :path)))
         (specifier-value (or pattern domain name path))
         (workspace-boundary-p
          (plist-get context :workspace-boundary-p))
         (rule-key (if workspace-boundary-p :path specifier-key))
         (rule-value (if workspace-boundary-p
                         (expand-file-name path)
                       specifier-value)))
    (append
     context
     (list :args args
           :session session
           :workspace workspace
           :request request
           :invocation invocation
           :buffer buffer
           :specifier-key specifier-key
           :specifier-value specifier-value
           :protected-path (plist-get context :protected-path-p)
           :workspace-root workspace-root
           :rule-tool (if workspace-boundary-p "*" tool-name)
           :rule-key rule-key
           :rule-value rule-value
           :include-always (not (null workspace))
           :invocation-rules invocation-rules
           :request-rules request-rules
           :session-rules session-rules
           :persistent-rules persistent-rules
           :permission-request permission-request))))

(defun mevedel-permission--checker-args (context)
  "Return `mevedel-check-permission' keyword args for CONTEXT."
  (let ((content (plist-get context :args))
        (tool-name (plist-get context :tool-name)))
    (when (and (member tool-name '("Bash" "Eval"))
               (listp content)
               (keywordp (car-safe content)))
      (setq content (plist-put (copy-sequence content)
                               :permission-context context)))
    (list :normalized-context
          (plist-put (copy-sequence context) :content content))))

(defun mevedel-permission--plan-mode-p (&optional session)
  "Return non-nil when the owning session is planning read-only work."
  (require 'mevedel-agents)
  (let ((owner
         (or session
             (and (boundp 'mevedel--session) mevedel--session)
             (and (boundp 'mevedel--agent-invocation)
                  mevedel--agent-invocation
                  (mevedel-agent-invocation-parent-session
                   mevedel--agent-invocation)))))
    (or (mevedel-plan-read-only-request-p)
        (and (fboundp 'mevedel-plan-mode-active-p)
             (mevedel-plan-mode-active-p owner)))))

(cl-defun mevedel-permission--preflight
    (tool-name &key tool-struct path pattern domain name content request
               invocation-rules request-rules session-rules persistent-rules
               mode session workspace-root allowed-roots exact-allowed-paths
               resource-access resource-grants one-shot-mutations-p
               patch-local-only-p
               normalized-context)
  "Return normalized permission facts and any decision before the tool slot.

This pure preflight owns specifier extraction, rule buckets, absolute
denials, protected-path facts, and allowed-root normalization.  The returned
plist's `:early-decision' is nil when the tool-owned permission slot and
the remaining decision chain still need to run.

TOOL-NAME identifies the tool.  TOOL-STRUCT and CONTENT supply its policy
and input.  REQUEST identifies the owning request when available.  PATH,
PATTERN, DOMAIN, and NAME may supply pre-extracted
specifiers.  INVOCATION-RULES, REQUEST-RULES, SESSION-RULES, and
PERSISTENT-RULES are the ordered rule sources.  MODE selects the permission
mode.  WORKSPACE-ROOT, ALLOWED-ROOTS, EXACT-ALLOWED-PATHS, and
RESOURCE-GRANTS define the filesystem boundary.  NORMALIZED-CONTEXT, when
non-nil, is returned unchanged so a caller can reuse an invocation preflight.
ONE-SHOT-MUTATIONS-P requires explicit approval for non-read-only tools.
PATCH-LOCAL-ONLY-P is true only for a prepared all-local ApplyPatch proposal."
  (require 'mevedel-permission-mode)
  (require 'mevedel-permission-rules)
  (if normalized-context
      normalized-context
    (setq mode (or mode mevedel-permission-mode))
    (when (and tool-struct content)
      (cl-flet ((extract (getter current)
                  (or current
                      (when-let* ((fn (funcall getter tool-struct)))
                        (ignore-errors (funcall fn content))))))
        (setq path (extract #'mevedel-tool-get-path path)
              pattern (extract #'mevedel-tool-get-pattern pattern)
              domain (extract #'mevedel-tool-get-domain domain)
              name (extract #'mevedel-tool-get-name name))))
    (let* ((allowed-roots (or allowed-roots
                              (and workspace-root (list workspace-root))))
           (read-only-p
            (when tool-struct (mevedel-tool-read-only-p tool-struct)))
           (native-edit-p
            (and tool-struct (memq 'edit (mevedel-tool-groups tool-struct))))
           (resource-access (or resource-access
                                (if read-only-p 'read 'write)))
           (resource-granted-p
            (mevedel-permission-rules-resource-granted-p
             path resource-access resource-grants))
           (buckets
            (mevedel-permission-rules-collect-buckets
             invocation-rules request-rules session-rules persistent-rules))
           (deny-bucket
            (mevedel-permission-rules-first-deny-bucket
             buckets tool-name path pattern domain name))
           (early-decision
            (cond
             (deny-bucket
              (mevedel-permission--decision
               'deny 'deny-rule :bucket deny-bucket))
             ((and (mevedel-permission--plan-mode-p session)
                   (or (and (equal tool-name "ApplyPatch")
                            (or (not patch-local-only-p)
                                (mevedel-plan-directive-p
                                 session request)))
                       (and (not (equal tool-name "ApplyPatch"))
                            (or native-edit-p (equal tool-name "Eval")))))
              (mevedel-permission--decision 'deny 'plan-mode)))))
      (list :tool-name tool-name
            :tool tool-struct
            :content content
            :path path
            :pattern pattern
            :domain domain
            :name name
            :mode mode
            :read-only-p read-only-p
            :one-shot-mutations-p one-shot-mutations-p
            :buckets buckets
            :allowed-roots allowed-roots
            :exact-allowed-paths exact-allowed-paths
            :resource-access resource-access
            :resource-grants resource-grants
            :request request
            :patch-local-only-p patch-local-only-p
            :resource-granted-p resource-granted-p
            :protected-path-p
            (mevedel-permission-rules-path-protected-p
             path (and session
                       (mevedel-session-execution-target session)))
            :workspace-boundary-p
            (and path
                 (not (mevedel-permission-rules-path-in-allowed-roots-p
                       path allowed-roots)))
            :early-decision early-decision))))

(defun mevedel-permission--sync-tool-decision (context)
  "Return the synchronous tool-slot decision for preflight CONTEXT.

Return nil when the tool has no synchronous slot or its slot declines to
decide.  Permission denials retain their reason as decision metadata;
other slot errors are reported and treated as no decision."
  (when-let* ((tool (plist-get context :tool))
              (check-fn (mevedel-tool-check-permission tool)))
    (let ((result
           (condition-case err
               (funcall check-fn tool (plist-get context :content))
             (mevedel-permission-denied
              (mevedel-permission--decision
               (cons 'deny (cadr err)) 'tool-slot))
             (error
              (message "mevedel: check-permission error: %S" err)
              nil))))
      (when result
        (if (mevedel-permission-decision-metadata-p result)
            result
          (mevedel-permission--decision result 'tool-slot))))))

(defun mevedel-check-permission-with-metadata (tool-name &rest args)
  "Check permission for TOOL-NAME using keyword ARGS.

ARGS are the inputs documented by `mevedel-permission--preflight'.

Returns a plist describing an `allow', `deny', or `ask' decision.

The decision chain:
  1. Extract specifier values via tool-struct getters when missing
  2. Resolve absolute decisions across all buckets:
       any bucket yields `deny' -> deny;
       standalone/sticky Plan allows only a prepared all-local ApplyPatch;
       directive Planning denies all native edits, including that ApplyPatch,
       and Eval -> deny
  3. Call the tool checker, when present, to decide command authority
  4. Resolve allow/ask rules innermost-first:
       invocation -> request -> session -> persistent -> defcustom.
  5. For a path, independently require allowed-root, exact-path, or exact
     resource-grant authority; otherwise ask
  6. Apply the mode/default decision

For tools with a checker, both command authority and resource authority must
allow.  Neither can substitute for the other."
  (let* ((context (apply #'mevedel-permission--preflight tool-name args))
         (tool (plist-get context :tool))
         (early-decision (plist-get context :early-decision)))
    (or early-decision
        (if (and tool (mevedel-tool-check-permission tool))
            (mevedel-permission--finish-tool-decision
             context (mevedel-permission--sync-tool-decision context))
          (mevedel-check-permission--tail-decision context)))))

(defun mevedel-check-permission (tool-name &rest args)
  "Check permission for TOOL-NAME with ARGS.

Return `allow', `deny', or `ask'."
  (mevedel-permission-decision-raw-outcome
   (apply #'mevedel-check-permission-with-metadata tool-name args)))


;;
;;; Async decision chain

(defun mevedel-check-permission-async-with-metadata
    (tool-name cont &rest args)
  "Async permission decision for TOOL-NAME using keyword ARGS.

Invokes CONT with permission decision metadata.  The original pipeline
outcome is available through `mevedel-permission-decision-raw-outcome'.
ARGS are the inputs documented by `mevedel-permission--preflight'.

Normalization, absolute policy, rules, modes, and resource authority run
synchronously just like `mevedel-check-permission'.  The tool command-policy
slot may run async when it defines `:check-permission-async'; the sync-slot
adapter preserves the denial REASON captured from a
`mevedel-permission-denied' signal so `(deny . REASON)' reaches CONT.

Bucket-aware; see `mevedel-check-permission' for the keyword-arg
semantics.  EXACT-ALLOWED-PATHS is passed to the shared tail as an
exact-match in-bounds path list."
  (let* ((context (apply #'mevedel-permission--preflight tool-name args))
         (tool (plist-get context :tool))
         (early-decision (plist-get context :early-decision))
         (finish
          (lambda (slot-decision)
            (funcall cont
                     (mevedel-permission--finish-tool-decision
                      context slot-decision)))))
    (cond
     (early-decision (funcall cont early-decision))
     ((and tool (mevedel-tool-check-permission-async tool))
      (funcall
       (mevedel-tool-check-permission-async tool)
       tool
       (mevedel-permission--metadata-content (plist-get context :content))
       (lambda (slot-result)
         (funcall
          finish
          (and slot-result
               (if (mevedel-permission-decision-metadata-p slot-result)
                   slot-result
                 (mevedel-permission--decision
                  slot-result
                  (cond
                   ((equal tool-name "Bash") 'bash-classifier)
                   ((equal tool-name "Eval") 'eval-policy)
                   (t 'tool-slot)))))))))
     (t
      (if (and tool (mevedel-tool-check-permission tool))
          (funcall finish
                   (mevedel-permission--sync-tool-decision context))
        (funcall cont
                 (mevedel-check-permission--tail-decision context)))))))

(defun mevedel-check-permission-async (tool-name cont &rest args)
  "Check TOOL-NAME permission with ARGS, then call CONT asynchronously."
  (apply #'mevedel-check-permission-async-with-metadata
         tool-name
         (lambda (decision)
           (funcall cont
                    (mevedel-permission-decision-raw-outcome decision)))
         args))

(defun mevedel-permission--resource-decision (context)
  "Return CONTEXT's independent filesystem resource decision, or nil."
  (require 'mevedel-permission-rules)
  (when-let* ((path (plist-get context :path)))
    (let ((granted-p (plist-get context :resource-granted-p)))
      (cond
       ((and (plist-get context :protected-path-p) granted-p)
        (mevedel-permission--decision 'allow 'resource-grant))
       ((plist-get context :protected-path-p)
        (mevedel-permission--decision 'ask 'protected-path))
       ((not (plist-get context :workspace-boundary-p))
        (mevedel-permission--decision 'allow 'allowed-root))
       ((mevedel-permission-rules-path-in-exact-allowed-paths-p
         path (plist-get context :exact-allowed-paths))
        (mevedel-permission--decision 'allow 'exact-path))
       (granted-p
        (mevedel-permission--decision 'allow 'resource-grant))
       (t
        (mevedel-permission--decision 'ask 'workspace-boundary))))))

(defun mevedel-permission--finish-tool-decision (context slot-decision)
  "Combine CONTEXT's command SLOT-DECISION with resource authority.

Under one-shot mutation policy a slot allow arriving `:via' `tool-slot'
is discarded so the shared tail re-decides; the tail's one-shot step
then forces the one-time prompt for mutations.  A slot decision may
deliberately claim a different `:via' (e.g. `execution-control' for
WriteStdin polling of an already-approved execution) to keep its allow
authoritative despite one-shot."
  (let* ((slot-outcome
          (and slot-decision
               (mevedel-permission-decision-raw-outcome slot-decision)))
         (command-context
          (plist-put (copy-sequence context) :skip-resource-boundary-p t))
         (policy-decision
          (if (and (plist-get context :one-shot-mutations-p)
                   (eq slot-outcome 'allow)
                   (eq (plist-get slot-decision :via) 'tool-slot))
              (mevedel-check-permission--tail-decision command-context)
            (or slot-decision
                (mevedel-check-permission--tail-decision command-context))))
         (policy-outcome
          (mevedel-permission-decision-raw-outcome policy-decision)))
    (if (eq policy-outcome 'allow)
        (or (mevedel-permission--resource-decision context)
            policy-decision)
      policy-decision)))

(defun mevedel-check-permission--tail-decision (context)
  "Return decision metadata for preflight CONTEXT's permission-chain tail.

The preflight owns normalization and absolute denials.  Callers own the
tool-specific command-policy slot; this function covers the shared rule,
mode, and native-resource tail."
  (require 'mevedel-permission-mode)
  (require 'mevedel-permission-rules)
  (let* ((tool-name (plist-get context :tool-name))
         (buckets (plist-get context :buckets))
         (path (plist-get context :path))
         (pattern (plist-get context :pattern))
         (domain (plist-get context :domain))
         (name (plist-get context :name))
         (skip-resource-boundary-p
          (plist-get context :skip-resource-boundary-p))
         (mode (plist-get context :mode))
         (read-only-p (plist-get context :read-only-p))
         (tool (plist-get context :tool))
         (native-edit-p
          (and tool (memq 'edit (mevedel-tool-groups tool))))
         (reviewed-edit-p
          (and tool (memq 'reviewed-edit (mevedel-tool-groups tool))))
         (resource-decision
          (and (not skip-resource-boundary-p)
               (mevedel-permission--resource-decision context))))
    (cond
     ;; Protected resources require exact grants even when a path rule allows.
     ((and (not skip-resource-boundary-p)
           (plist-get context :protected-path-p)
           (not (plist-get context :resource-granted-p)))
      (mevedel-permission--decision 'ask 'protected-path))
     ((and (plist-get context :one-shot-mutations-p)
           (not (or read-only-p reviewed-edit-p)))
      (mevedel-permission--decision 'ask 'one-shot-mutation))
     ;; Step 5: pass 2 -- allow/ask innermost-first across buckets.
     ((when-let* ((action-bucket
                   (mevedel-permission-rules-first-action-with-bucket
                    buckets tool-name path pattern domain name)))
        (let ((action (car action-bucket))
              (bucket (cdr action-bucket)))
          (cond
           ((eq action 'allow)
            (mevedel-permission--decision 'allow 'rule :bucket bucket))
           ((eq action 'ask)
            (mevedel-permission--decision 'ask 'rule :bucket bucket))))))
     ;; Steps 6-7: missing native resource authority forces a prompt.  An
     ;; allowed root or exact grant satisfies only the resource half; the mode
     ;; still decides whether the operation itself is automatic.
     ((and resource-decision
           (eq (mevedel-permission-decision-raw-outcome resource-decision)
               'ask))
      resource-decision)
     ;; Step 8: mode/default decision.
     (t (let ((mode-result (mevedel-permission-mode-decision
                            mode read-only-p native-edit-p
                            reviewed-edit-p)))
          (if (and (eq mode-result 'allow)
                   resource-decision
                   (eq (mevedel-permission-decision-raw-outcome
                        resource-decision)
                       'allow))
              resource-decision
            (mevedel-permission--decision mode-result 'mode)))))))


(defun mevedel-permission-add-session-resource-grant (session path access)
  "Grant SESSION exact PATH access at READ or WRITE level."
  (require 'mevedel-permission-rules)
  (require 'mevedel-session-artifacts)
  (mevedel-session-artifacts-assert-mutation-authority session)
  (let ((grant (mevedel-permission-rules-resource-grant path access)))
    (setf (mevedel-session-resource-grants session)
          (mevedel-permission-rules-merge-resource-grant
           (mevedel-session-resource-grants session) path access))
    grant))

(defun mevedel-permission-remove-session-resource-grant
    (session path access)
  "Revoke SESSION's exact PATH ACCESS resource grant."
  (require 'mevedel-permission-rules)
  (require 'mevedel-session-artifacts)
  (mevedel-session-artifacts-assert-mutation-authority session)
  (let ((grant (mevedel-permission-rules-resource-grant path access)))
    (setf (mevedel-session-resource-grants session)
          (delete grant
                  (copy-sequence
                   (mevedel-session-resource-grants session))))))

(defun mevedel-permission-remove-session-rule (session rule)
  "Revoke exact permission RULE from SESSION."
  (require 'mevedel-session-artifacts)
  (mevedel-session-artifacts-assert-mutation-authority session)
  (setf (mevedel-session-permission-rules session)
        (delete rule
                (copy-sequence
                 (mevedel-session-permission-rules session)))))


(cl-defun mevedel-permission--add-session-rule
    (session tool-name action &optional path
             &key spec-key spec-value network file-system
             sandbox-permissions)
  "Add a permission rule to SESSION's rule list.

TOOL-NAME is the tool name string.  ACTION is `allow' or `deny'.

Positional PATH is retained for existing call sites; when supplied it is
equivalent to SPEC-KEY `:path' with that value.  Callers specifying
another specifier should pass SPEC-KEY (e.g. `:pattern') and SPEC-VALUE
instead, leaving PATH nil.  NETWORK and FILE-SYSTEM record matching additive
execution authority.  SANDBOX-PERMISSIONS qualifies an already requested
execution level.

Mutates SESSION's `permission-rules' slot via `setf' -- this is a
**by-reference** write.  Sub-agents share the parent session by
reference (see `mevedel-agent-conversation-open'), so a
rule recorded inside any sub-agent's permission prompt, such as
\"allow-session\" or \"deny-session\", immediately applies to the parent
and to every other live sub-agent sharing the same session struct.  This
is a deliberate contract, not an accident of the buffer-local plumbing."
  (require 'mevedel-permission-rules)
  (require 'mevedel-session-artifacts)
  (mevedel-session-artifacts-assert-mutation-authority session)
  (let* ((key (or spec-key (and path :path)))
         (value (or spec-value path))
         (rule (mevedel-permission-rules-build-rule
                tool-name action key value
                :network network
                :file-system file-system
                :sandbox-permissions sandbox-permissions))
         (rules (mevedel-session-permission-rules session)))
    (unless (member rule rules)
      (setf (mevedel-session-permission-rules session)
            (append rules (list rule))))))


(defun mevedel-permission-invalidate-target-grants (session)
  "Revoke SESSION's exact authority after target replacement."
  (require 'mevedel-permission-mode)
  (require 'mevedel-permission-persistence)
  (require 'mevedel-session-artifacts)
  (mevedel-session-artifacts-assert-mutation-authority session)
  (setf (mevedel-session-resource-grants session) nil
        (mevedel-session-dropped-file-grants session) nil
        (mevedel-session-active-dropped-file-grants session) nil)
  (when-let* ((data-buffer (mevedel-permission-mode-data-buffer)))
    (with-current-buffer data-buffer
      (setq-local mevedel-permission--frozen-resource-grants nil)))
  (let* ((workspace (mevedel-session-workspace session))
         (file (mevedel-permission-persistence-file workspace))
         (target (mevedel-session-execution-target session)))
    (when (file-exists-p file)
      (let ((store (mevedel-permission-persistence-editable-store file target)))
        (when (plist-get store :resource-grants)
          (mevedel-permission-persistence-write-store
           file (plist-put store :resource-grants nil) target)))))
  t)


;;
;;; Prompt result dispatch

(cl-defun mevedel-permission--apply-prompt-result
    (result tool-name &optional session workspace path
            &key spec-key spec-value resource-access network
            file-system sandbox-permissions)
  "Dispatch a permission prompt RESULT to the correct storage.

RESULT is one of:
  `allow-once'    -- return `allow', no storage
  `allow-session' -- add a session rule or resource grant, return `allow'
  `always-allow'  -- save a persistent rule or resource grant, return `allow'
  `deny-once'     -- return `deny', no storage
  `deny-session'  -- add session deny rule, return `deny'

TOOL-NAME is the tool being permitted.  SESSION and WORKSPACE are used
for storage.  Positional PATH scopes the authority to a file path (kept
for call sites that already pass it).  SPEC-KEY/SPEC-VALUE allow rule
scoping by any other specifier (`:pattern', `:domain', `:name').
RESOURCE-ACCESS stores exact path authority separately from rules.
NETWORK and FILE-SYSTEM store a capability-qualified operation rule.
SANDBOX-PERMISSIONS qualifies an already requested execution level."
  (require 'mevedel-permission-persistence)
  (cl-flet ((session-rule (action)
              (when session
                (mevedel-permission--add-session-rule
                 session tool-name action path
                 :spec-key spec-key :spec-value spec-value
                 :network network
                 :file-system file-system
                 :sandbox-permissions sandbox-permissions)))
            (session-resource-grant ()
              (when (and session path resource-access)
                (mevedel-permission-add-session-resource-grant
                 session path resource-access)))
            (persistent-resource-grant ()
              (when (and workspace path resource-access)
                (mevedel-permission-persistence-save-resource-grant
                 workspace path resource-access)))
            (persistent-rule (action)
              (cond
               (workspace
                (mevedel-permission-persistence-save-rule
                 workspace tool-name action path
                 :spec-key spec-key :spec-value spec-value
                 :network network
                 :file-system file-system
                 :sandbox-permissions sandbox-permissions))
               (t
                ;; User clicked always-allow but no workspace is
                ;; in scope (gone since enqueue, or session not
                ;; bound to one).  Silently dropping the persistent
                ;; rule write produces a session-only rule and
                ;; surprises the user on next Emacs start.  Warn so
                ;; the gap is at least diagnosable.
                (display-warning
                 'mevedel
                 (format "Persistent rule for %s skipped: no workspace in context"
                         tool-name)
                 :warning)))))
    (pcase result
      ('allow-once 'allow)
      ('allow-session
       (if resource-access
           (session-resource-grant)
         (session-rule 'allow))
       'allow)
      ('always-allow
       (if resource-access
           (persistent-resource-grant)
         (persistent-rule 'allow)
         (session-rule 'allow))
       'allow)
      ('deny-once 'deny)
      ('deny-session (session-rule 'deny) 'deny)
      (_ 'deny))))

(provide 'mevedel-permissions)
;;; mevedel-permissions.el ends here
