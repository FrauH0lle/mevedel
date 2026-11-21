;;; niwa-permissions.el -- Unified permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Permission preflight and decision facade for all niwa tools.  It combines
;; mode, rule, and persistent-authority owners with tool policy in one decision
;; chain: extract context -> absolute denies -> tool policy -> permission rules
;; -> mode -> independent filesystem resource authority -> mode/default ask.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'niwa-agents)
(require 'niwa-permission-mode)
(require 'niwa-permission-persistence)
(require 'niwa-permission-rules)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'niwa-utilities)

;; `niwa-agents'
(declare-function niwa-agent-invocation-skill-permission-rules
                  "niwa-agents" (cl-x) t)
(declare-function niwa-plan-read-only-p "niwa-agents"
                  (&optional session))
(defvar niwa--agent-invocation)

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-data-buffer
                  "niwa-permission-mode" ())
(declare-function niwa-permission-mode-decision
                  "niwa-permission-mode"
                  (mode read-only-p &optional native-edit-p reviewed-edit-p))
(defvar niwa-permission-mode)

;; `niwa-permission-persistence'
(declare-function niwa-permission-persistence-load-resource-grants
                  "niwa-permission-persistence" (workspace))
(declare-function niwa-permission-persistence-load-rules
                  "niwa-permission-persistence" (workspace))
(declare-function niwa-permission-persistence-save-resource-grant
                  "niwa-permission-persistence"
                  (workspace path access &optional recursive origin))
(declare-function niwa-permission-persistence-save-rule
                  "niwa-permission-persistence"
                  (workspace rule))

;; `niwa-permission-rules'
(declare-function niwa-permission-rules-build-rule
                  "niwa-permission-rules"
                  (tool-name action spec-key spec-value &rest keys))
(declare-function niwa-permission-rules-collect-buckets
                  "niwa-permission-rules"
                  (invocation-rules request-rules session-rules persistent-rules))
(declare-function niwa-permission-rules-first-action-with-bucket
                  "niwa-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function niwa-permission-rules-first-deny-bucket
                  "niwa-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function niwa-permission-rules-merge-resource-grant
                  "niwa-permission-rules"
                  (grants path access &optional recursive origin))
(declare-function niwa-permission-rules-path-in-allowed-roots-p
                  "niwa-permission-rules" (path roots))
(declare-function niwa-permission-rules-path-in-exact-allowed-paths-p
                  "niwa-permission-rules" (path allowed-paths))
(declare-function niwa-permission-rules-path-protected-p
                  "niwa-permission-rules" (path &optional target access))
(declare-function niwa-permission-rules-resource-grant
                  "niwa-permission-rules" (path access &optional recursive origin))
(declare-function niwa-permission-rules-resource-granted-p
                  "niwa-permission-rules"
                  (path access grants &optional recursive))

;; `niwa-plan-mode'
(declare-function niwa-plan-mode-exit
                  "niwa-plan-mode" (&optional session))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-mutation-authority
                  "niwa-session-artifacts" (session &optional buffer))

;; `niwa-structs'
(declare-function niwa-request-goal-plan-read-path
                  "niwa-structs" (cl-x) t)
(declare-function niwa-request-one-shot-mutations-p
                  "niwa-structs" (cl-x) t)
(declare-function niwa-request-skill-permission-rules
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-active-dropped-file-grants
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-dropped-file-grants
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-rules "niwa-structs" (cl-x) t)
(declare-function niwa-session-resource-grants "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)

;; setf expander for session struct
(eval-when-compile
  (require 'niwa-structs))

;; `niwa-tool-registry'
(declare-function niwa-tool-check-permission "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-check-permission-async "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-ensure "niwa-tool-registry" (name))
(declare-function niwa-tool-get "niwa-tool-registry" (name &optional category))
(declare-function niwa-tool-get-domain "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-get-name "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-get-path "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-get-paths "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-get-pattern "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-groups "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-name "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-read-only-p "niwa-tool-registry" (cl-x) t)
(defvar niwa-tool-registry--source-dir)

;; `niwa-workspace'
(declare-function niwa--all-allowed-roots
                  "niwa-workspace" (&optional buffer))


;;
;;; Frozen request authority

(defvar-local niwa-permission--context-frozen-p nil
  "Non-nil when persistent permission authority must not be reloaded.")

(defvar-local niwa-permission--frozen-persistent-rules nil
  "Persistent permission rules captured for a frozen request context.")

(defvar-local niwa-permission--frozen-resource-grants nil
  "Persistent resource grants captured for a frozen request context.")

(defun niwa-permission-freeze-context (persistent-rules resource-grants)
  "Freeze the current buffer's persistent permission authority.

PERSISTENT-RULES and RESOURCE-GRANTS replace on-disk lookups for the
buffer's lifetime; later persisted changes are not observed.  Callers
own copying the passed structures."
  (setq-local niwa-permission--context-frozen-p t
              niwa-permission--frozen-persistent-rules persistent-rules
              niwa-permission--frozen-resource-grants resource-grants))


(defun niwa-permission--normalize-outcome (outcome)
  "Return the log-safe decision symbol for permission OUTCOME."
  (pcase outcome
    (`(deny . ,_) 'deny)
    (`(feedback . ,_) 'deny)
    (_ outcome)))

(defun niwa-permission--decision
    (outcome via &rest props)
  "Return permission decision metadata for OUTCOME through VIA with PROPS."
  (let ((normalized (niwa-permission--normalize-outcome outcome)))
    (append (list :outcome normalized
                  :raw-outcome outcome
                  :via via)
            props)))

(defun niwa-permission-decision-raw-outcome (decision)
  "Return the pipeline outcome represented by DECISION."
  (if (and (consp decision) (keywordp (car decision))
           (plist-member decision :outcome))
      (if (plist-member decision :raw-outcome)
          (plist-get decision :raw-outcome)
        (plist-get decision :outcome))
    decision))

(defun niwa-permission-decision-metadata-p (value)
  "Return non-nil when VALUE is permission decision metadata."
  (and (listp value)
       (keywordp (car-safe value))
       (plist-member value :outcome)
       (plist-member value :via)))

(defun niwa-permission--metadata-content (content)
  "Return CONTENT marked for permission metadata collection when possible."
  (if (and (listp content) (keywordp (car-safe content)))
      (plist-put (copy-sequence content) :permission-decision-metadata t)
    content))

(defun niwa-permission--error-decision (err)
  "Return a denial for an expected policy ERR, or an internal-error decision."
  (let ((conditions (get (car err) 'error-conditions)))
    (cond
     ((memq 'niwa-permission-denied conditions)
      (niwa-permission--decision
       (cons 'deny (cadr err)) 'tool-slot))
     ((or (memq 'niwa-sandbox-policy-error conditions)
          (memq 'niwa-execution-policy-error conditions))
      (niwa-permission--decision
       (cons 'deny (error-message-string err)) 'sandbox-policy))
     (t (niwa-permission--decision 'internal-error 'tool-slot :error err)))))

(defun niwa-permission--run-async (scope start cont)
  "Run START with guarded callbacks and deliver one decision to CONT.
SCOPE is the originating diagnostic root captured before asynchronous work.
START receives FINISH and GUARD functions.  FINISH settles the decision;
GUARD wraps asynchronous callbacks so errors become decisions rather than
escaping the event loop.  Late callbacks do nothing after settlement."
  (let (settled)
    (cl-labels
     ((finish (decision)
        (unless settled
          (setq settled t)
          (condition-case-unless-debug err
              (funcall cont decision)
            (error
             (niwa-diagnostic-message
              scope "%s"
              (if (memq 'user-error (get (car err) 'error-conditions))
                  (error-message-string err)
                (format "Permission callback failed: %S" err)))))))
      (guard (callback)
        (lambda (&rest values)
          (unless settled
            (condition-case err
                (apply callback values)
              (error
               (unless settled
                 (finish (niwa-permission--error-decision err)))))))))
     (funcall (guard start) #'finish #'guard))))

(defun niwa-permission--one-shot-mutations-p (request &optional explicit)
  "Return the one-shot mutation policy for REQUEST.
EXPLICIT takes precedence when non-nil."
  (or explicit
      (and request (niwa-request-one-shot-mutations-p request))))

(defun niwa-permission--one-shot-prompt-entry (entry &optional data-buffer)
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

(defun niwa-permission--one-shot-prompt-outcome (outcome)
  "Collapse reusable permission OUTCOME to its one-time equivalent."
  (pcase outcome
    ((or 'allow-session 'always-allow) 'allow-once)
    ('deny-session 'deny-once)
    (_ outcome)))

(cl-defun niwa-permission--invocation-context
    (&key tool tool-name args session workspace request invocation buffer
          path pattern domain name mode workspace-root allowed-roots
          exact-allowed-paths invocation-rules request-rules session-rules
          persistent-rules persistent-snapshot resource-grants
          resource-access permission-request execution-directory
          one-shot-mutations-p patch-session-only-p warn-no-session-p)
  "Return normalized permission invocation context.

The context concentrates facts shared by the permission decision
chain, prompt queue, and Bash/Eval adapters.

TOOL, TOOL-NAME, ARGS, SESSION, WORKSPACE, REQUEST, INVOCATION, BUFFER,
PATH, PATTERN, DOMAIN, NAME, MODE, WORKSPACE-ROOT, ALLOWED-ROOTS,
EXACT-ALLOWED-PATHS, INVOCATION-RULES, REQUEST-RULES, SESSION-RULES,
PERSISTENT-RULES, PERSISTENT-SNAPSHOT, RESOURCE-GRANTS, and
WARN-NO-SESSION-P provide the context facts.  PERSISTENT-SNAPSHOT is the
invocation's already-refreshed `:rules' and `:resource-grants' plist,
including when either value is nil.  RESOURCE-ACCESS, `read' or `write',
overrides the access the tool's mutability implies for PATH.
ONE-SHOT-MUTATIONS-P defaults from REQUEST.
PATCH-SESSION-ONLY-P classifies prepared ApplyPatch proposals containing only
session-owned work descendants for the Plan boundary.
PERMISSION-REQUEST admits an interactive request at its hook boundary before
it enters the shared queue.  EXECUTION-DIRECTORY captures the caller's relative
path base; absent an explicit snapshot, use BUFFER's conversation directory."
  (setq tool (or tool
                 (and tool-name (niwa-tool-ensure tool-name)))
        tool-name (or tool-name (and tool (niwa-tool-name tool))))
  (setq workspace (or workspace
                      (and session (niwa-session-workspace session)))
        workspace-root
        (or workspace-root
            (and workspace
                 (ignore-errors (niwa-workspace-root workspace))))
        allowed-roots
        (or allowed-roots
            (when (and workspace (fboundp 'niwa--all-allowed-roots))
              (ignore-errors (niwa--all-allowed-roots buffer))))
        exact-allowed-paths
        (or exact-allowed-paths
            (and (equal tool-name "Read")
                 session
                 (niwa-session-active-dropped-file-grants session)))
        invocation-rules
        (or invocation-rules
            (and invocation
                 (niwa-agent-invocation-skill-permission-rules
                  invocation)))
        request-rules
        (or request-rules
            (and request (niwa-request-skill-permission-rules request)))
        session-rules
        (or session-rules
            (and session (niwa-session-permission-rules session)))
        persistent-rules
        (if persistent-snapshot
            (plist-get persistent-snapshot :rules)
          (or persistent-rules
              (and niwa-permission--context-frozen-p
                   niwa-permission--frozen-persistent-rules)
              (and (not niwa-permission--context-frozen-p) workspace
                   (niwa-permission-persistence-load-rules workspace))))
        resource-grants
        (or resource-grants
            (append (when-let* ((path (and request
                                           (niwa-request-goal-plan-read-path
                                            request))))
                      (list (list :path path :access 'read)))
                    (and session (niwa-session-resource-grants session))
                    (and niwa-permission--context-frozen-p
                         niwa-permission--frozen-resource-grants)
                    (and (not niwa-permission--context-frozen-p)
                         workspace
                         (if persistent-snapshot
                             (plist-get persistent-snapshot :resource-grants)
                           (niwa-permission-persistence-load-resource-grants
                            workspace)))))
        one-shot-mutations-p
        (niwa-permission--one-shot-mutations-p
         request one-shot-mutations-p)
        mode (or mode (niwa-permission-mode-effective
                       session buffer nil request invocation)))
  ;; Configuration snapshots remain immutable; filesystem eligibility does not.
  ;; Keep these probes outside the pure decision preflight.
  (setq persistent-rules (niwa-permission-origin-usable-rules persistent-rules)
        session-rules (niwa-permission-origin-usable-rules session-rules)
        resource-grants (niwa-permission-origin-usable-grants resource-grants path))
  (when (and warn-no-session-p (not session))
    (niwa--warn-once
     (niwa-diagnostic-scope (or workspace-root workspace buffer))
     (list 'permission-no-session tool-name)
     "Permission step for %s ran with no session in \
context; falling back to defcustom defaults.  Session-scoped \
rules and the active permission mode are not being consulted.  \
This usually means the tool was dispatched from a buffer whose \
`niwa--session' was not set; in production that should not \
happen for a non-read-only tool."
     tool-name))
  (let* ((context
          (niwa-permission--preflight
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
           :invocation invocation
           :workspace-root workspace-root
           :allowed-roots allowed-roots
           :exact-allowed-paths exact-allowed-paths
           :resource-grants resource-grants
           :resource-access resource-access
           :one-shot-mutations-p one-shot-mutations-p
           :patch-session-only-p patch-session-only-p))
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
           :execution-directory
           (or execution-directory
               (niwa-conversation-working-directory session buffer))
           :specifier-key specifier-key
           :specifier-value specifier-value
           :protected-path (plist-get context :protected-path-p)
           :workspace-root workspace-root
           :rule-tool (if workspace-boundary-p "*"
                        (niwa-permission-policy-tool-name tool-name))
           :rule-key rule-key
           :rule-value rule-value
           :include-always (not (null workspace))
           :invocation-rules invocation-rules
           :request-rules request-rules
           :session-rules session-rules
           :persistent-rules persistent-rules
           :permission-request permission-request))))

(defun niwa-permission-policy-tool-name (tool-name)
  "Return the permission policy identity of TOOL-NAME.
Monitor executes Bash commands under Bash rules and execution authority.
Keep the original name in invocation contexts and presentation metadata."
  (if (equal tool-name "Monitor") "Bash" tool-name))

(defun niwa-permission--checker-args (context)
  "Return `niwa-check-permission' keyword args for CONTEXT."
  (let ((content (plist-get context :args))
        (tool-name (niwa-permission-policy-tool-name
                    (plist-get context :tool-name))))
    (when (and (member tool-name '("Bash" "Eval"))
               (listp content)
               (keywordp (car-safe content)))
      (setq content (plist-put (copy-sequence content)
                               :permission-context context)))
    (list :normalized-context
          (plist-put (copy-sequence context) :content content))))

(cl-defun niwa-permission--preflight
    (tool-name &key tool-struct path pattern domain name content
               (request nil request-supplied-p)
               (invocation nil invocation-supplied-p)
               invocation-rules request-rules session-rules persistent-rules
               mode session workspace-root allowed-roots exact-allowed-paths
               resource-access resource-grants one-shot-mutations-p
               patch-session-only-p
               normalized-context)
  "Return normalized permission facts and any decision before the tool slot.

This pure preflight owns specifier extraction, rule buckets, absolute
denials, protected-path facts, and allowed-root normalization.  The returned
plist's `:early-decision' is nil when the tool-owned permission slot and
the remaining decision chain still need to run.

TOOL-NAME identifies the tool.  TOOL-STRUCT and CONTENT supply its policy
and input.  REQUEST and INVOCATION identify the captured owners.  When supplied,
even as nil, their Plan stamps replace ambient owners; the owning SESSION's
live Plan flag still applies.  Omitted owners use ambient stamps.  PATH,
PATTERN, DOMAIN, and NAME may supply pre-extracted
specifiers.  INVOCATION-RULES, REQUEST-RULES, SESSION-RULES, and
PERSISTENT-RULES are the ordered rule sources.  MODE selects the permission
mode.  WORKSPACE-ROOT, ALLOWED-ROOTS, EXACT-ALLOWED-PATHS, and
RESOURCE-GRANTS define the filesystem boundary.  NORMALIZED-CONTEXT, when
non-nil, is returned unchanged so a caller can reuse an invocation preflight.
ONE-SHOT-MUTATIONS-P requires explicit approval for non-read-only tools.
PATCH-SESSION-ONLY-P is true only for a prepared ApplyPatch proposal containing
session-owned work descendants; workspace-owned shared files are excluded."
  (if normalized-context
      normalized-context
    (setq mode (or mode (if request
                           (niwa-permission-mode-effective session nil nil request)
                         (niwa-permission-mode-effective session))))
    (when (and tool-struct content)
      (cl-flet ((extract (getter current)
                  (or current
                      (when-let* ((fn (funcall getter tool-struct)))
                        (ignore-errors (funcall fn content))))))
        (setq path (extract #'niwa-tool-get-path path)
              pattern (extract #'niwa-tool-get-pattern pattern)
              domain (extract #'niwa-tool-get-domain domain)
              name (extract #'niwa-tool-get-name name))))
    (let* ((niwa--current-request
            (if request-supplied-p request niwa--current-request))
           (niwa--agent-invocation
            (if invocation-supplied-p invocation niwa--agent-invocation))
           (read-only-p
            (when tool-struct (niwa-tool-read-only-p tool-struct)))
           (package-source-root
            (and read-only-p
                 (boundp 'niwa-tool-registry--source-dir)
                 niwa-tool-registry--source-dir
                 (file-name-as-directory
                  (expand-file-name niwa-tool-registry--source-dir))))
           (allowed-roots
            (delete-dups
             (append (and package-source-root (list package-source-root))
                     (or allowed-roots
                         (and workspace-root (list workspace-root))))))
           (native-edit-p
            (and tool-struct (memq 'edit (niwa-tool-groups tool-struct))))
           (resource-access (or resource-access
                                (if read-only-p 'read 'write)))
           (resource-granted-p
            (niwa-permission-rules-resource-granted-p
             path resource-access resource-grants))
           (buckets
            (niwa-permission-rules-collect-buckets
             invocation-rules request-rules session-rules persistent-rules))
           (policy-tool (niwa-permission-policy-tool-name tool-name))
           (deny-bucket
            (or (niwa-permission-rules-first-deny-bucket
                 buckets policy-tool path pattern domain name)
                (and (not (equal policy-tool tool-name))
                     (niwa-permission-rules-first-deny-bucket
                      buckets tool-name path pattern domain name))))
           (early-decision
            (cond
             (deny-bucket
              (niwa-permission--decision
               'deny 'deny-rule :bucket deny-bucket))
             ((and (niwa-plan-read-only-p session)
                   (or (and (equal tool-name "ApplyPatch")
                            (not patch-session-only-p))
                       (and (not (equal tool-name "ApplyPatch"))
                            (or native-edit-p (equal tool-name "Eval")))))
              (niwa-permission--decision 'deny 'plan-mode)))))
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
            :patch-session-only-p patch-session-only-p
            :resource-granted-p resource-granted-p
            :protected-path-p
            (niwa-permission-rules-path-protected-p
             path (and session
                       (niwa-session-execution-target session))
             (and (eq mode 'edits) resource-access))
            :workspace-boundary-p
            (and path
                 (not (niwa-permission-rules-path-in-allowed-roots-p
                       path allowed-roots)))
            :early-decision early-decision))))

(defun niwa-permission--sync-tool-decision (context)
  "Return the synchronous tool-slot decision for preflight CONTEXT.

Return nil when the tool has no synchronous slot or its slot declines to
decide.  Expected policy refusals remain denials; unexpected errors stop
resolution with explicit internal-error metadata."
  (when-let* ((tool (plist-get context :tool))
              (check-fn (niwa-tool-check-permission tool)))
    (let ((result
           (condition-case err
               (funcall check-fn tool (plist-get context :content))
             (error (niwa-permission--error-decision err)))))
      (when result
        (if (niwa-permission-decision-metadata-p result)
            result
          (niwa-permission--decision result 'tool-slot))))))

(defun niwa-check-permission-with-metadata (tool-name &rest args)
  "Check permission for TOOL-NAME using keyword ARGS.

ARGS are the inputs documented by `niwa-permission--preflight'.

Returns a plist describing an `allow', `deny', or `ask' decision.
Unexpected checker failures return `internal-error' with the original
condition in `:error'; they cannot grant authority or become a denial.

The decision chain:
  1. Extract specifier values via tool-struct getters when missing
  2. Resolve absolute decisions across all buckets:
       any bucket yields `deny' -> deny;
       standalone/sticky Plan and requests carrying inherited Plan limits
       allow only ApplyPatch proposals confined to session-owned work
       descendants; other native edits and Eval -> deny
  3. Call the tool checker, when present, to decide command authority
  4. Resolve allow/ask rules innermost-first:
       invocation -> request -> session -> persistent -> defcustom.
  5. For a path, independently require allowed-root, exact-path, or covering
     resource-grant authority; otherwise ask
  6. Apply the mode/default decision

For tools with a checker, both command authority and resource authority must
allow.  Neither can substitute for the other."
  (let* ((context (apply #'niwa-permission--preflight tool-name args))
         (tool (plist-get context :tool))
         (early-decision (plist-get context :early-decision)))
    (or early-decision
        (if (and tool (niwa-tool-check-permission tool))
            (niwa-permission--finish-tool-decision
             context (niwa-permission--sync-tool-decision context))
          (niwa-check-permission--tail-decision context)))))

(defun niwa-check-permission (tool-name &rest args)
  "Check permission for TOOL-NAME with ARGS.

Return `allow', `deny', `ask', or `internal-error'."
  (niwa-permission-decision-raw-outcome
   (apply #'niwa-check-permission-with-metadata tool-name args)))


;;
;;; Async decision chain

(defun niwa-check-permission-async-with-metadata
    (tool-name cont &rest args)
  "Async permission decision for TOOL-NAME using keyword ARGS.

Invokes CONT with permission decision metadata.  The original pipeline
outcome is available through `niwa-permission-decision-raw-outcome'.
ARGS are the inputs documented by `niwa-permission--preflight'.
Unexpected errors return `internal-error' metadata with an `:error'
condition.  Asynchronous callbacks and settlement are guarded exactly once.

Normalization, absolute policy, rules, modes, and resource authority run
synchronously just like `niwa-check-permission'.  The tool command-policy
slot may run async when it defines `:check-permission-async'; the sync-slot
adapter preserves the denial REASON captured from a
`niwa-permission-denied' signal so `(deny . REASON)' reaches CONT.

Bucket-aware; see `niwa-check-permission' for the keyword-arg
semantics.  EXACT-ALLOWED-PATHS is passed to the shared tail as an
exact-match in-bounds path list."
  (niwa-permission--run-async
   (let ((context (plist-get args :normalized-context)))
     (niwa-diagnostic-scope
      (or (plist-get context :workspace-root)
          (plist-get context :workspace) (plist-get context :session)
          (plist-get context :buffer)
          (plist-get args :workspace-root) (plist-get args :session))))
   (lambda (cont guard)
     (let* ((context (apply #'niwa-permission--preflight tool-name args))
            (tool (plist-get context :tool))
            (early-decision (plist-get context :early-decision))
            (finish
             (lambda (slot-decision)
               (funcall cont
			(niwa-permission--finish-tool-decision
			 context slot-decision)))))
       (cond
	(early-decision (funcall cont early-decision))
	((and tool (niwa-tool-check-permission-async tool))
	 (funcall
	  (niwa-tool-check-permission-async tool)
	  tool
	  (niwa-permission--metadata-content (plist-get context :content))
	  (funcall guard (lambda (slot-result)
			   (funcall
			    finish
			    (and slot-result
				 (if (niwa-permission-decision-metadata-p slot-result)
				     slot-result
				   (niwa-permission--decision
				    slot-result
				    (cond
				     ((equal (niwa-permission-policy-tool-name tool-name)
					     "Bash")
				      'bash-classifier)
				     ((equal tool-name "Eval") 'eval-policy)
				     (t 'tool-slot))))))))))
	(t
	 (if (and tool (niwa-tool-check-permission tool))
             (funcall finish
                      (niwa-permission--sync-tool-decision context))
           (funcall cont
                    (niwa-check-permission--tail-decision context)))))))
   cont))

(defun niwa-check-permission-async (tool-name cont &rest args)
  "Check TOOL-NAME permission with ARGS, then call CONT asynchronously."
  (apply #'niwa-check-permission-async-with-metadata
         tool-name
         (lambda (decision)
           (funcall cont
                    (niwa-permission-decision-raw-outcome decision)))
         args))

(defun niwa-permission--resource-decision (context)
  "Return CONTEXT's independent filesystem resource decision, or nil."
  (when-let* ((path (plist-get context :path)))
    (let ((granted-p (plist-get context :resource-granted-p)))
      (cond
       ((eq (plist-get context :mode) 'full-auto)
        (niwa-permission--decision 'allow 'mode))
       ((and (plist-get context :protected-path-p) granted-p)
        (niwa-permission--decision 'allow 'resource-grant))
       ((plist-get context :protected-path-p)
        (niwa-permission--decision 'ask 'protected-path))
       ((and (eq (plist-get context :mode) 'edits)
             (eq (plist-get context :resource-access) 'read))
        (niwa-permission--decision 'allow 'sandbox-read))
       ((not (plist-get context :workspace-boundary-p))
        (niwa-permission--decision 'allow 'allowed-root))
       ((niwa-permission-rules-path-in-exact-allowed-paths-p
         path (plist-get context :exact-allowed-paths))
        (niwa-permission--decision 'allow 'exact-path))
       (granted-p
        (niwa-permission--decision 'allow 'resource-grant))
       (t
        (niwa-permission--decision 'ask 'workspace-boundary))))))

(defun niwa-permission--finish-tool-decision (context slot-decision)
  "Combine CONTEXT's command SLOT-DECISION with resource authority.

Under one-shot mutation policy a slot allow arriving `:via' `tool-slot'
is discarded so the shared tail re-decides; the tail's one-shot step
then forces the one-time prompt for mutations.  A slot decision may
deliberately claim a different `:via' (e.g. `execution-control' for
WriteStdin polling of an already-approved execution) to keep its allow
authoritative despite one-shot."
  (let* ((slot-outcome
          (and slot-decision
               (niwa-permission-decision-raw-outcome slot-decision)))
         (command-context
          (plist-put (copy-sequence context) :skip-resource-boundary-p t))
         (policy-decision
          (cond
           ((and (eq (plist-get context :mode) 'full-auto)
                 (memq slot-outcome '(nil allow ask)))
            (niwa-permission--decision 'allow 'mode))
           ((and (plist-get context :one-shot-mutations-p)
                   (eq slot-outcome 'allow)
                   (eq (plist-get slot-decision :via) 'tool-slot))
            (niwa-check-permission--tail-decision command-context))
           (t (or slot-decision
                  (niwa-check-permission--tail-decision command-context)))))
         (policy-outcome
          (niwa-permission-decision-raw-outcome policy-decision)))
    (if (eq policy-outcome 'allow)
        (or (niwa-permission--resource-decision context)
            policy-decision)
      policy-decision)))

(defun niwa-check-permission--tail-decision (context)
  "Return decision metadata for preflight CONTEXT's permission-chain tail.

The preflight owns normalization and absolute denials.  Callers own the
tool-specific command-policy slot; this function covers the shared rule,
mode, and native-resource tail."
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
          (and tool (memq 'edit (niwa-tool-groups tool))))
         (reviewed-edit-p
          (and tool (memq 'reviewed-edit (niwa-tool-groups tool))))
         (resource-decision
          (and (not skip-resource-boundary-p)
               (niwa-permission--resource-decision context))))
    (cond
     ((eq mode 'full-auto)
      (niwa-permission--decision 'allow 'mode))
     ;; Protected resources require covering grants even when a path rule
     ;; allows.
     ((and (not skip-resource-boundary-p)
           (plist-get context :protected-path-p)
           (not (plist-get context :resource-granted-p)))
      (niwa-permission--decision 'ask 'protected-path))
     ((and (plist-get context :one-shot-mutations-p)
           (not (or read-only-p reviewed-edit-p)))
      (niwa-permission--decision 'ask 'one-shot-mutation))
     ;; Step 5: pass 2 -- allow/ask innermost-first across buckets.
     ((when-let* ((action-bucket
                   (niwa-permission-rules-first-action-with-bucket
                    buckets tool-name path pattern domain name)))
        (let ((action (car action-bucket))
              (bucket (cdr action-bucket)))
          (cond
           ((eq action 'allow)
            (niwa-permission--decision 'allow 'rule :bucket bucket))
           ((eq action 'ask)
            (niwa-permission--decision 'ask 'rule :bucket bucket))))))
     ;; Steps 6-7: missing native resource authority forces a prompt.  An
     ;; allowed root or resource grant satisfies only the resource half; the
     ;; mode still decides whether the operation itself is automatic.
     ((and resource-decision
           (eq (niwa-permission-decision-raw-outcome resource-decision)
               'ask))
      resource-decision)
     ;; Step 8: mode/default decision.
     (t (let ((mode-result (niwa-permission-mode-decision
                            mode read-only-p native-edit-p
                            reviewed-edit-p)))
          (if (and (eq mode-result 'allow)
                   resource-decision
                   (eq (niwa-permission-decision-raw-outcome
                        resource-decision)
                       'allow))
              resource-decision
            (niwa-permission--decision mode-result 'mode)))))))


(defun niwa-permission-add-session-resource-grant
    (session path access &optional recursive)
  "Grant SESSION PATH access at READ or WRITE level.
RECURSIVE non-nil covers PATH and all descendants."
  (require 'niwa-session-artifacts)
  (niwa-session-artifacts-assert-mutation-authority session)
  (let ((grant (niwa-permission-rules-resource-grant
                path access recursive)))
    (setf (niwa-session-resource-grants session)
          (niwa-permission-rules-merge-resource-grant
           (niwa-session-resource-grants session) path access recursive))
    grant))

(defun niwa-permission-remove-session-resource-grant
    (session path access &optional recursive)
  "Revoke SESSION's PATH ACCESS resource grant with RECURSIVE scope."
  (require 'niwa-session-artifacts)
  (niwa-session-artifacts-assert-mutation-authority session)
  (let ((grant (niwa-permission-rules-resource-grant
                path access recursive)))
    (setf (niwa-session-resource-grants session)
          (delete grant
                  (copy-sequence
                   (niwa-session-resource-grants session))))))

(defun niwa-permission-remove-session-rule (session rule)
  "Revoke exact permission RULE from SESSION."
  (require 'niwa-session-artifacts)
  (niwa-session-artifacts-assert-mutation-authority session)
  (setf (niwa-session-permission-rules session)
        (delete rule
                (copy-sequence
                 (niwa-session-permission-rules session)))))


(defun niwa-permission--add-session-rule (session rule)
  "Append RULE to SESSION's permission rules unless already present.
RULE uses the `niwa-permission-rules' format.  Sub-agents share the
root SESSION by reference, so the rule immediately applies to the tree."
  (require 'niwa-session-artifacts)
  (niwa-session-artifacts-assert-mutation-authority session)
  (let ((rules (niwa-session-permission-rules session)))
    (unless (member rule rules)
      (setf (niwa-session-permission-rules session)
            (append rules (list rule))))))


(defun niwa-permission-invalidate-target-grants (session)
  "Revoke SESSION's filesystem authority after target replacement.

Only session-scoped authority is bound to a target incarnation: the exact
and dropped-file grants, command profile paths, and the frozen copy a
running request holds.  Command and network approvals remain effective.
The workspace store in `.niwa/permissions.el' is deliberate, durable
configuration shared through version control between machines, so a
reboot or a different host must leave it alone; a grant there that does
not apply on this target simply matches nothing."
  (require 'niwa-session-artifacts)
  (niwa-session-artifacts-assert-mutation-authority session)
  (cl-labels
      ((without-filesystem (rules)
         (mapcar
          (lambda (rule)
            (if (eq (plist-get (cdr rule) :action) 'allow)
                (cons (car rule)
                      (cl-loop for (key value) on (cdr rule) by #'cddr
                               unless (eq key :file-system)
                               append (list key value)))
              rule))
          rules)))
    (setf (niwa-session-resource-grants session) nil
          (niwa-session-dropped-file-grants session) nil
          (niwa-session-active-dropped-file-grants session) nil
          (niwa-session-permission-rules session)
          (without-filesystem (niwa-session-permission-rules session)))
    (when-let* ((data-buffer (niwa-permission-mode-data-buffer)))
      (with-current-buffer data-buffer
        (setq-local niwa-permission--frozen-resource-grants nil
                    niwa-permission--frozen-persistent-rules
                    (without-filesystem niwa-permission--frozen-persistent-rules)))))
  t)


;;
;;; Prompt result dispatch

(cl-defun niwa-permission--apply-prompt-result
    (result tool-name &optional session workspace path
            &key spec-key spec-value resource-access resource-recursive
            resource-origin network file-system sandbox-permissions)
  "Dispatch a permission prompt RESULT to the correct storage.

RESULT is one of:
  `allow-once'    -- return `allow', no storage
  `allow-session' -- add a session rule or resource grant, return `allow'
  `always-allow'  -- save a persistent rule or resource grant, return `allow'
  `deny-once'     -- return `deny', no storage
  `deny-session'  -- add session deny rule, return `deny'

TOOL-NAME is the tool being permitted.  SESSION and WORKSPACE are used
for storage.  PATH scopes resource authority; SPEC-KEY/SPEC-VALUE scope
operation rules (`:path', `:pattern', `:expression', `:domain', `:name').
When no rule specifier is supplied, PATH also scopes the operation rule.
RESOURCE-ACCESS stores path authority separately from rules;
RESOURCE-RECURSIVE non-nil extends it to all descendants of PATH.
RESOURCE-ORIGIN retains an existing workspace-relative grant identity.
NETWORK and FILE-SYSTEM store a capability-qualified operation rule.
SANDBOX-PERMISSIONS qualifies an already requested execution level."
  (cl-flet ((rule (action)
              (niwa-permission-rules-build-rule
               tool-name action (or spec-key (and path :path))
               (or spec-value path)
               :network network :file-system file-system
               :sandbox-permissions sandbox-permissions))
            (session-rule (rule)
              (when session
                ;; A new explicit session approval is ordinary absolute
                ;; authority, not a durable copy of persistent provenance.
                (let ((copy (copy-tree rule)))
                  (when (plist-member (cdr copy) :file-system)
                    (setcdr copy
                            (plist-put
                             (cdr copy) :file-system
                             (mapcar
                              (lambda (grant)
                                (cl-loop for (key value) on grant by #'cddr
                                         unless (eq key :workspace-origin)
                                         append (list key value)))
                              (plist-get (cdr copy) :file-system)))))
                  (niwa-permission--add-session-rule session copy))))
            (session-resource-grant ()
              (when (and session path resource-access)
                (niwa-permission-add-session-resource-grant
                 session path resource-access resource-recursive)))
            (persistent-resource-grant ()
              (when (and workspace path resource-access)
                (niwa-permission-persistence-save-resource-grant
                 workspace path resource-access resource-recursive resource-origin)))
            (persistent-rule (rule)
              (cond
               (workspace
                (niwa-permission-persistence-save-rule workspace rule))
               (t
                ;; User clicked always-allow but no workspace is
                ;; in scope (gone since enqueue, or session not
                ;; bound to one).  Silently dropping the persistent
                ;; rule write produces a session-only rule and
                ;; surprises the user on next Emacs start.  Warn so
                ;; the gap is at least diagnosable.
                (niwa-diagnostic-warning
                 (niwa-diagnostic-scope session) 'niwa
                 (format "Persistent rule for %s skipped: no workspace in context"
                         tool-name)
                 :warning)))))
    (pcase result
      ('allow-once 'allow)
      ('allow-session
       (if resource-access
           (session-resource-grant)
         (session-rule (rule 'allow)))
       'allow)
      ('always-allow
       (if resource-access
           (persistent-resource-grant)
         (let ((rule (rule 'allow)))
           (persistent-rule rule)
           (unless (cl-some (lambda (grant) (plist-get grant :workspace-origin))
                            (plist-get (cdr rule) :file-system))
             (session-rule rule))))
       'allow)
      ('deny-once 'deny)
      ('deny-session (session-rule (rule 'deny)) 'deny)
      (_ 'deny))))

(provide 'niwa-permissions)
;;; niwa-permissions.el ends here
