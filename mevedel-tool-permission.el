;;; mevedel-tool-permission.el -- Tool permission pipeline step -*- lexical-binding: t -*-

;;; Commentary:

;; Owns tool permission-path fan-out, decision logging, permission hooks, and
;; prompt orchestration.  Shared tool-hook operations live in mevedel-hooks;
;; the pipeline retains step ordering.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mevedel-agents)
(require 'mevedel-execution)
(require 'mevedel-hooks)
(require 'mevedel-permission-log)
(require 'mevedel-permission-queue)
(require 'mevedel-permission-rules)
(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'mevedel-tool-registry)

;; `mevedel-bash-policy'
(declare-function mevedel-bash-policy-decision-specifier-value
                  "mevedel-bash-policy" (command))
(autoload 'mevedel-bash-policy-decision-specifier-value "mevedel-bash-policy")

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-effective
                  "mevedel-permission-mode"
                  (&optional session data-buffer surface-buffer))
(defvar mevedel-permission-mode)

;; `mevedel-permission-persistence'
(declare-function mevedel-permission-persistence-load-resource-grants
                  "mevedel-permission-persistence" (workspace))
(declare-function mevedel-permission-persistence-load-rules
                  "mevedel-permission-persistence" (workspace))
(declare-function mevedel-permission-persistence-refresh
                  "mevedel-permission-persistence"
                  (workspace continuation &optional on-cancel on-error))

;; `mevedel-tool-patch'
(declare-function mevedel-tool-patch-get-paths-from-proposal
                  "mevedel-tool-patch" (proposal))
(declare-function mevedel-tool-patch-sanitize-error
                  "mevedel-tool-patch" (message proposal))
(autoload 'mevedel-tool-patch-get-paths-from-proposal "mevedel-tool-patch")
(autoload 'mevedel-tool-patch-sanitize-error "mevedel-tool-patch")

(defun mevedel-tool-permission--origin (context &optional explicit-origin)
  "Return the canonical agent path for permission CONTEXT.
EXPLICIT-ORIGIN takes precedence when non-nil."
  (or explicit-origin
      (plist-get context :origin)
      (and-let* ((request (plist-get context :request))
                 ((mevedel-request-p request)))
        (mevedel-request-origin request))
      (and-let* ((inv (plist-get context :invocation))
                 ((fboundp 'mevedel-agent-invocation-p))
                 ((mevedel-agent-invocation-p inv)))
        (mevedel-agent-invocation-path inv))
      "/root"))

(defun mevedel-tool-permission--sanitized-pattern (tool-name pattern)
  "Return log-safe PATTERN metadata for TOOL-NAME."
  (cond
   ((and (equal tool-name "Bash") pattern)
    (mevedel-bash-policy-decision-specifier-value pattern))
   (t pattern)))

(defun mevedel-tool-permission--specifier-props (context)
  "Return sanitized permission specifier properties from CONTEXT."
  (let* ((tool (plist-get context :tool))
         (tool-name (and tool (mevedel-tool-name tool)))
         (args (plist-get context :args))
         (path (or (plist-get context :permission-path)
                   (when-let* ((fn (and tool (mevedel-tool-get-path tool))))
                     (ignore-errors (funcall fn args)))))
         (raw-pattern (when-let* ((fn (and tool (mevedel-tool-get-pattern tool))))
                        (ignore-errors (funcall fn args))))
         (pattern (mevedel-tool-permission--sanitized-pattern
                   tool-name raw-pattern))
         (domain (when-let* ((fn (and tool (mevedel-tool-get-domain tool))))
                   (ignore-errors (funcall fn args))))
         (name (when-let* ((fn (and tool (mevedel-tool-get-name tool))))
                 (ignore-errors (funcall fn args))))
         (key (cond (pattern :pattern)
                    (domain :domain)
                    (name :name)
                    (path :path)))
         (value (or pattern domain name path)))
    (append (and key (list :specifier-key key))
            (and value (list :specifier-value value))
            (and path
                 (list :protected-path
                       (mevedel-permission-rules-path-protected-p path))))))

(defun mevedel-tool-permission-log-decision
    (context decision &rest props)
  "Persist sanitized DECISION diagnostics for CONTEXT with PROPS."
  (let ((session (plist-get context :session)))
    (when (and session
               (not (plist-get decision :logged)))
      (let* ((tool (plist-get context :tool))
             (tool-name (and tool (mevedel-tool-name tool)))
             (mode (or (and session (mevedel-session-permission-mode session))
                       mevedel-permission-mode))
             (raw (mevedel-permission-decision-raw-outcome decision))
             (outcome (or (plist-get decision :outcome)
                          (mevedel-permission--normalize-outcome raw)))
             (specifier (mevedel-tool-permission--specifier-props context)))
        (apply #'mevedel-permission-log
               session 'permission-decision
               (append
                (list :tool-name tool-name
                      :tool-use-id (plist-get context :tool-use-id)
                      :parent-tool-use-id
                      (plist-get context :parent-tool-use-id)
                      :call-source (plist-get context :call-source)
                      :origin (mevedel-tool-permission--origin context)
                      :mode mode
                      :outcome outcome
                      :via (plist-get decision :via))
                specifier
                (when (plist-member decision :bucket)
                  (list :bucket (plist-get decision :bucket)))
                props))
        ;; The permission log above keeps the resource paths and human
        ;; justifications of the decision; only its own file does.  Telemetry
        ;; drops them, and this forwards a fixed value-free subset to a
        ;; distinct durable audit target.
        (when (mevedel-telemetry-forwarded-audit-p session)
          (apply #'mevedel-telemetry-record-audit
                 session 'permission-decision
                 (append
                  (list :tool-name tool-name
                        :tool-use-id (plist-get context :tool-use-id)
                        :parent-tool-use-id
                        (plist-get context :parent-tool-use-id)
                        :call-source (plist-get context :call-source)
                        :origin (mevedel-tool-permission--origin context)
                        :mode mode :outcome outcome
                        :via (plist-get decision :via))
                  (when (plist-member specifier :specifier-key)
                    (list :specifier-key
                          (plist-get specifier :specifier-key)))
                  (when (plist-member specifier :protected-path)
                    (list :protected-path
                          (and (plist-get specifier :protected-path) t)))
                  (when (plist-member decision :bucket)
                    (list :bucket (plist-get decision :bucket))))))))))

(defun mevedel-tool-permission--decision-with-via
    (decision via &rest props)
  "Return DECISION metadata adjusted to VIA and PROPS."
  (let ((raw (mevedel-permission-decision-raw-outcome decision)))
    (append (list :outcome (mevedel-permission--normalize-outcome raw)
                  :raw-outcome raw
                  :via via)
            (when (plist-member decision :bucket)
              (list :bucket (plist-get decision :bucket)))
            props)))

(defun mevedel-tool-permission--apply-hook-decision (outcome context)
  "Apply CONTEXT's hook permission decision to permission OUTCOME.

Hook `deny' always wins.  Hook `ask' can tighten an `allow' into a
prompt.  Hook `allow' is applied at the `PermissionRequest' boundary
when the normal resolver returns `ask'; explicit denials stay intact."
  (let ((decision (plist-get context :hook-permission-decision)))
    (pcase decision
      ('deny 'deny)
      ('ask
       (if (and (not (eq 'full-auto
                        (mevedel-permission-mode-effective
                         (plist-get context :session)
                         (plist-get context :buffer))))
                (memq outcome '(allow approve implement implement-clear)))
           'ask
         outcome))
      (_ outcome))))

(defun mevedel-tool-permission-deny
    (context fail reason &optional model-reason provenance)
  "Run `PermissionDenied' hooks for CONTEXT, then call FAIL.

FAIL receives REASON, the hook-updated context, and
`permission-denied'.

MODEL-REASON and PROVENANCE are included in the hook event when available."
  (let ((session (plist-get context :session))
        (workspace (plist-get context :workspace)))
    (mevedel-hooks-run-tool-event
     'PermissionDenied
     (mevedel-hooks-tool-event-plist
      'PermissionDenied context
      :permission-reason (or model-reason reason)
      :permission-provenance provenance)
     (lambda (decision)
       (let* ((updated (mevedel-hooks-record-tool-context
                        context decision 'PermissionDenied))
              (updated
               (mevedel-hooks-record-tool-audit
                updated
                (mevedel-hooks-context-audit-records
                 decision 'PermissionDenied 'tool-context)))
              (final-reason
               (or (plist-get decision :permission-reason)
                   reason)))
         (when-let* ((proposal (plist-get updated :patch-proposal)))
           (setq final-reason (mevedel-tool-patch-sanitize-error final-reason proposal)))
         (funcall fail final-reason updated 'permission-denied)))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))

(defun mevedel-tool-permission--denial-outcome-p (outcome)
  "Return non-nil when OUTCOME is an interactive denial."
  (or (memq outcome '(deny deny-once deny-session))
      (and (consp outcome) (memq (car outcome) '(deny feedback)))))

(defun mevedel-tool-permission--denial-provenance (context decision)
  "Return the original denial source from CONTEXT or DECISION."
  (or (plist-get context :permission-denial-provenance)
      (plist-get decision :via)
      'policy))

(defun mevedel-tool-permission--request
    (context entry session settle &optional ask-decision fallback-outcome)
  "Run `PermissionRequest' for ENTRY, then call SETTLE or enqueue.
SETTLE receives the updated CONTEXT and the permission outcome.
ASK-DECISION is logged only when hooks leave queue admission unresolved.
FALLBACK-OUTCOME settles an unresolved request without queue admission."
  (let* ((one-shot-p (plist-get context :one-shot-mutations-p))
         (entry
          (append
           (list :tool-use-id (plist-get context :tool-use-id)
                 :approval-source-cell (list 'user)
                 :args (plist-get context :args)
                 :patch-session-only-p
                 (plist-get (plist-get context :patch-proposal) :session-only-p)
                 :data-buffer (plist-get context :buffer)
                 :request (plist-get context :request)
                 :invocation (plist-get context :invocation)
                 :parent-tool-use-id
                 (plist-get context :parent-tool-use-id)
                 :call-source (plist-get context :call-source))
           entry))
         (entry (if one-shot-p
                    (mevedel-permission--one-shot-prompt-entry
                     entry (plist-get context :buffer))
                  entry))
         (original-settle settle)
         (settle
          (lambda (updated outcome)
            (when one-shot-p
              (setq outcome (mevedel-permission--one-shot-prompt-outcome outcome)))
            (unless (eq 'full-auto
                        (mevedel-permission-mode-effective
                         session (plist-get context :buffer)))
              (condition-case err
                  (mevedel-permission-queue-validate-approval entry outcome)
                (error (setq outcome (cons 'deny (error-message-string err))))))
            ;; Every approval source crosses this boundary, including hooks
            ;; and fallback outcomes before any queue entry exists.
            (when (memq outcome '(allow allow-once allow-session always-allow))
              (let* ((full-access-p
                      (eq 'full-auto (mevedel-permission-mode-effective
                                      session (plist-get context :buffer))))
                     (resolved (mevedel-permission-queue--reevaluate
                                (plist-put (copy-sequence entry) :session session)))
                     (denied-p
                      (or (mevedel-tool-permission--denial-outcome-p resolved)
                          (and full-access-p (not (eq resolved 'allow))))))
                (when denied-p
                  (setq outcome (if (consp resolved) resolved 'deny)
                        updated (plist-put updated :permission-denial-provenance 'policy)))
                (when (or full-access-p denied-p)
                  (mevedel-tool-permission-log-decision
                   updated (list :outcome (mevedel-permission--normalize-outcome outcome)
                                 :raw-outcome outcome :via 'policy)))))
            (when-let* ((request (plist-get context :request))
                        ((mevedel-request-cancelled-p request)))
              (setq outcome 'aborted))
            (funcall original-settle updated outcome)))
         (workspace (plist-get context :workspace)))
    (mevedel-hooks-run-tool-event
     'PermissionRequest
     (mevedel-hooks-tool-event-plist
      'PermissionRequest context
      :specifier-key (plist-get entry :specifier-key)
      :specifier-value (plist-get entry :specifier-value))
     (lambda (decision)
       (let* ((updated
               (mevedel-hooks-record-tool-context
                context decision 'PermissionRequest))
              (updated
               (mevedel-hooks-record-tool-audit
                updated
                (mevedel-hooks-context-audit-records
                 decision 'PermissionRequest 'tool-context)))
              (stop-p (and (plist-member decision :continue)
                           (not (plist-get decision :continue))))
              (permission-decision
               (plist-get decision :permission-decision)))
         (cond
          ((or stop-p (eq permission-decision 'deny))
           (let* ((detail
                   (or (and stop-p (plist-get decision :stop-reason))
                       (plist-get decision :permission-reason)
                       "hook denied permission"))
                  (reason (format "blocked by PermissionRequest: %s" detail)))
             (setq updated
                   (mevedel-hooks-record-tool-audit
                    updated
                    (mevedel-hooks-tool-permission-audit-record
                     'PermissionRequest 'deny decision reason)))
             (mevedel-tool-permission-log-decision
              updated
              (list :outcome 'deny
                    :raw-outcome `(deny . ,reason)
                    :via 'permission-request-hook))
             (funcall settle
                      (plist-put updated :permission-denial-provenance
                                 'PermissionRequest)
                      `(deny . ,reason))))
          ((eq 'full-auto
               (mevedel-permission-mode-effective
                session (plist-get context :buffer)))
           (funcall settle updated 'allow))
          ((and (eq permission-decision 'allow) (not one-shot-p))
           (setq updated
                 (mevedel-hooks-record-tool-audit
                  updated
                  (mevedel-hooks-tool-permission-audit-record
                   'PermissionRequest 'allow decision)))
           (mevedel-tool-permission-log-decision
            updated
            (list :outcome 'allow :raw-outcome 'allow
                  :via 'permission-request-hook))
           (funcall settle updated 'allow))
          (t
           (if (and fallback-outcome (null permission-decision)
                    (not one-shot-p))
               (funcall settle updated fallback-outcome)
             (when ask-decision
               (mevedel-tool-permission-log-decision
                updated ask-decision))
             (let ((queued (copy-sequence entry)))
               (setq queued
                     (plist-put queued :permission-via
                                (cond
                                 ((eq permission-decision 'ask) 'permission-request-hook)
                                 ((eq (plist-get context :hook-permission-decision) 'ask)
                                  'pre-tool-hook)
                                 (ask-decision (plist-get ask-decision :via))
                                 (t (plist-get entry :permission-via)))))
               (when-let* ((request (plist-get context :request)))
                 (setq queued
                       (plist-put queued :request-id
                                  (mevedel-request-id request))))
               (setq queued
                     (plist-put
                      queued :callback
                      (lambda (outcome)
                        (when (memq outcome '(allow-once allow-session always-allow))
                          (setq updated
                                (plist-put updated :approved-resources
                                           (copy-tree
                                            (car (plist-get queued
                                                            :resource-selection-cell))))))
                        (funcall
                         settle
                         (if (mevedel-tool-permission--denial-outcome-p
                              outcome)
                             (plist-put updated :permission-denial-provenance
                                        (car (plist-get queued :approval-source-cell)))
                           updated)
                         outcome))))
               (when-let* ((progress (plist-get context :progress-callback)))
                 (ignore-errors (funcall progress 'permission-wait)))
               (mevedel-permission--enqueue queued session)))))))
     context session workspace
     (plist-get context :request)
     (plist-get context :invocation))))

(defun mevedel-tool-permission--step-one (context next fail)
  "Check permission for the tool invocation.

Reads session / workspace from CONTEXT (captured at
`mevedel-pipeline-run-tool' entry) so that an async continuation
firing from another buffer still sees the correct session state.

Invokes `mevedel-check-permission-async' for the shared decision chain.
When the chain (or a tool slot) yields `ask', the step
drives the generic async prompt and applies the result through
`mevedel-permission--apply-prompt-result' so session / persistent
rule and resource-grant storage is honored.  A missing filesystem
boundary creates exact read or write authority without broadening an
allowed root.

Dispatches the final outcome through NEXT (allow-equivalent
outcomes) or FAIL (all denial shapes, plus `aborted')."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (tool-name (mevedel-tool-name tool))
         (session (plist-get context :session))
         (workspace (or (plist-get context :workspace)
                        (and session (mevedel-session-workspace session))))
         (request (plist-get context :request))
         (invocation (plist-get context :invocation))
         (one-shot-mutations-p
          (mevedel-permission--one-shot-mutations-p request))
         (context
          (plist-put context :one-shot-mutations-p one-shot-mutations-p))
         (permission-context
          (mevedel-permission--invocation-context
           :tool tool
           :args args
           :session session
           :workspace workspace
           :request request
           :invocation invocation
           :persistent-snapshot
           (plist-get context :persistent-permission-snapshot)
           :one-shot-mutations-p one-shot-mutations-p
           :patch-session-only-p
           (plist-get (plist-get context :patch-proposal) :session-only-p)
           :buffer (plist-get context :buffer)
           :path (plist-get context :permission-path)
           :permission-request
           (lambda (entry queue-session settle)
             (mevedel-tool-permission--request
              context entry (or queue-session session)
              (lambda (updated outcome)
                (setq context updated)
                (funcall settle outcome))
              nil
              (and (eq 'allow (plist-get context :hook-permission-decision))
                   'allow)))
           :warn-no-session-p t))
         (path (plist-get permission-context :path))
         (workspace-root (plist-get permission-context :workspace-root))
         (allowed-roots (plist-get permission-context :allowed-roots)))
    (apply #'mevedel-check-permission-async-with-metadata
           tool-name
           (lambda (decision)
             (let* ((raw-outcome (mevedel-permission-decision-raw-outcome decision))
                    (hooked-outcome
                     (mevedel-tool-permission--apply-hook-decision
                      raw-outcome context))
                    (context
                     (if (eq hooked-outcome raw-outcome)
                         context
                       (mevedel-hooks-record-tool-audit
                        context
                        (mevedel-hooks-tool-permission-audit-record
                         'PreToolUse hooked-outcome
                         (plist-get context :hook-permission-hook-decision)))))
                    (logged-decision
                     (if (eq hooked-outcome raw-outcome)
                         decision
                       (mevedel-tool-permission--decision-with-via
                        (plist-put (copy-sequence decision)
                                   :raw-outcome hooked-outcome)
                        'pre-tool-hook))))
               (when (and (memq 'edit (mevedel-tool-groups tool))
                          (eq 'allow
                              (mevedel-permission-decision-raw-outcome
                               logged-decision))
                          (eq 'rule (plist-get logged-decision :via))
                          (memq (plist-get logged-decision :bucket)
                                '(:session :persistent :defcustom)))
                 (setq context (plist-put context :auto-apply-edit-p t)))
               (mevedel-tool-permission--dispatch-outcome
                hooked-outcome context next fail
                :tool-name tool-name :path path :session session
                :workspace workspace :workspace-root workspace-root
                :allowed-roots allowed-roots
                :decision logged-decision
                :permission-context permission-context)))
           (mevedel-permission--checker-args permission-context))))

(defun mevedel-tool-permission-paths (tool args &optional context)
  "Return every filesystem path declared by TOOL for ARGS.

Addressed resource operands are already authorized by their resource
attempt; a prepared writable resource contributes only the backing path its
proposal records.  Ordinary paths retain the existing path extraction
behavior."
  (let* ((proposal (plist-get context :patch-proposal))
         (paths
          (condition-case nil
              (if proposal
                  (mevedel-tool-patch-get-paths-from-proposal proposal)
                (cond
                 ((mevedel-tool-get-paths tool)
                  (funcall (mevedel-tool-get-paths tool) args))
                 ((mevedel-tool-get-path tool)
                  (list (funcall (mevedel-tool-get-path tool) args)))))
            (error nil)))
         (attempts (plist-get context :resource-attempts))
         (canonical (plist-get context :canonical-path-map)))
    (delete-dups
     (delq nil
           (mapcar
            (lambda (path)
              (if (and (stringp path) (cdr (assoc path attempts)))
                  nil
                (or (cdr (assoc path canonical)) path)))
            paths)))))

(defun mevedel-tool-permission-decide-now (tool-name args &optional buffer reason)
  "Return `allow', `deny' or `ask' for a call of TOOL-NAME with ARGS now.
Decide without prompting under the policy of BUFFER, which defaults to
the current buffer: its session, request, agent invocation and
buffer-local rules.  Only a dispatched call runs PreToolUse hooks, so an
`allow' becomes `ask' when such a hook would run.  A dead BUFFER, or one
without a session, yields `ask'.  The decision is recorded in the
permission log with REASON."
  (let ((buffer (or buffer (current-buffer))))
    (or (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (when-let* ((session (bound-and-true-p mevedel--session)))
                 (let* ((request (bound-and-true-p mevedel--current-request))
                        (invocation (bound-and-true-p mevedel--agent-invocation))
                        (workspace (mevedel-session-workspace session))
                        (context (mevedel-permission--invocation-context
                                  :tool-name tool-name :args args
                                  :session session :workspace workspace
                                  :request request :invocation invocation
                                  :buffer buffer))
                        (decision (apply #'mevedel-check-permission-with-metadata
                                         tool-name
                                         (mevedel-permission--checker-args context)))
                        (outcome (mevedel-permission--normalize-outcome
                                  (mevedel-permission-decision-raw-outcome decision)))
                        (hooked-p
                         (and (eq outcome 'allow)
                              (< 0 (plist-get
                                    (mevedel-hooks-run-dry
                                     'PreToolUse
                                     (list :tool-name tool-name :tool-input args)
                                     session workspace request invocation)
                                    :handler-count))))
                        (outcome (if hooked-p 'ask outcome)))
                   (mevedel-tool-permission-log-decision
                    context
                    (list :outcome outcome :raw-outcome outcome
                          :via (if hooked-p 'pre-tool-hook (plist-get decision :via)))
                    :reason reason)
                   outcome))))
        'ask)))

(defun mevedel-tool-permission-step (context next fail)
  "Authorize each filesystem path in CONTEXT before continuing.

FAIL receives a reason string and may additionally receive an updated
context and typed reason."
  (let* ((tool (plist-get context :tool))
         (session (plist-get context :session))
         (workspace (or (plist-get context :workspace)
                        (and session (mevedel-session-workspace session))))
         (buffer (plist-get context :buffer))
         (frozen-p
          (if (buffer-live-p buffer)
              (buffer-local-value
               'mevedel-permission--context-frozen-p buffer)
            mevedel-permission--context-frozen-p))
         (paths (mevedel-tool-permission-paths
                 tool (plist-get context :args) context)))
    (cl-labels
        ((run ()
           (cond
            ((and session
                  (not (mevedel-tool-read-only-p tool))
                  (mevedel-execution-mutation-refused-p session))
             (funcall fail mevedel-execution-mutation-blocked-message))
            ((null paths)
             (mevedel-tool-permission--step-one context next fail))
            (t
             (cl-labels
                 ((authorize (remaining current all-direct-p)
                    (if (null remaining)
                        (funcall next
                                 (plist-put current :auto-apply-edit-p
                                            all-direct-p))
                      (let ((path-context
                             (plist-put
                              (plist-put (copy-sequence current)
                                         :permission-path (car remaining))
                              :auto-apply-edit-p nil)))
                        (mevedel-tool-permission--step-one
                         path-context
                         (lambda (updated)
                           (authorize
                            (cdr remaining) updated
                            (and all-direct-p
                                 (plist-get updated :auto-apply-edit-p))))
                         fail)))))
               (authorize paths context t))))))
      (cond
       ((and workspace (not frozen-p))
          (mevedel-permission-persistence-refresh
           workspace
           (lambda ()
             (setq context
                   (plist-put
                    context :persistent-permission-snapshot
                    (list
                     :rules
                     (mevedel-permission-persistence-load-rules workspace)
                     :resource-grants
                     (mevedel-permission-persistence-load-resource-grants
                      workspace))))
             (run))
           (lambda ()
             (funcall fail "Permission store refresh cancelled"))
           (lambda (err)
             (funcall fail (format "Permission store refresh failed: %s"
                                  (error-message-string err))))))
       (frozen-p
        (setq context
              (plist-put
               context :persistent-permission-snapshot
               (list
                :rules
                (if (buffer-live-p buffer)
                    (buffer-local-value
                     'mevedel-permission--frozen-persistent-rules buffer)
                  mevedel-permission--frozen-persistent-rules)
                :resource-grants
                (if (buffer-live-p buffer)
                    (buffer-local-value
                     'mevedel-permission--frozen-resource-grants buffer)
                  mevedel-permission--frozen-resource-grants))))
        (run))
       (t (run))))))

(cl-defun mevedel-tool-permission--dispatch-outcome
    (outcome context next fail
             &key tool-name path session workspace workspace-root allowed-roots
             decision permission-context)
  "Translate permission OUTCOME for CONTEXT into NEXT or FAIL.

OUTCOME is the union of (a) results emitted by a permission slot via
`cont' (`allow', `deny', `(deny . REASON)', `(feedback . TEXT)',
`aborted', `ask') and (b) results emitted by the generic async prompt
overlay after an `ask' is routed through it (`allow-once',
`allow-session', `always-allow', `deny-once', `deny-session',
`aborted').

`ask' routes through the standard prompt path and recurses with the
user's UI choice.  Rule-scope outcomes (`allow-session' etc.) are
pre-collapsed via `mevedel-permission--apply-prompt-result' so that
session / persistent rules land with the correct scope before the
translator fires NEXT / FAIL.

TOOL-NAME, PATH, SESSION, WORKSPACE, WORKSPACE-ROOT, ALLOWED-ROOTS,
DECISION, and PERMISSION-CONTEXT describe the permission context."
  (when (and decision (not (eq outcome 'ask)))
    (mevedel-tool-permission-log-decision context decision))
  (pcase outcome
    ;; `ask' arrives from the decision chain itself (steps 3/7/8/9) or
    ;; from a tool slot that defers to the generic prompt.  Drive the
    ;; prompt with workspace-boundary rule shaping identical to the
    ;; sync pipeline's.
    ('ask
     (let* ((args (plist-get context :args))
            (tool (plist-get context :tool))
            (permission-context
             (or permission-context
                 (mevedel-permission--invocation-context
                  :tool tool
                  :args args
                  :session session
                  :workspace workspace
                  :request (plist-get context :request)
                  :invocation (plist-get context :invocation)
                  :buffer (plist-get context :buffer)
                  :path path
                  :workspace-root workspace-root
                  :allowed-roots allowed-roots)))
            (rule-tool (plist-get permission-context :rule-tool))
            (rule-key (plist-get permission-context :rule-key))
            (rule-value (plist-get permission-context :rule-value))
            (decision-metadata decision)
            (resource-decision-p
             (memq (plist-get decision-metadata :via)
                   '(protected-path workspace-boundary)))
            (resource-access
             (and resource-decision-p
                  (plist-get permission-context :resource-access)))
            (resource-selection
             (and resource-access (eq rule-key :path)
                  (list (list (list :path rule-value
                                    :access resource-access))))))
       (mevedel-tool-permission--request
        context
        (list :kind 'generic
              :tool-name tool-name
              :args args
              :mutation-p (not (mevedel-tool-read-only-p tool))
              :specifier-key rule-key
              :specifier-value rule-value
              :protected-path
              (eq (plist-get decision-metadata :via) 'protected-path)
              :resource-access resource-access
              :permission-mode-effective (plist-get permission-context :mode)
              :resource-selection-cell resource-selection
              :resource-originals (copy-tree (car resource-selection))
              :include-always
              (plist-get permission-context :include-always)
              :workspace workspace
              :origin (mevedel-tool-permission--origin context))
        session
        (lambda (prompt-context prompt-outcome)
          (condition-case err
              (let* ((selected (caar resource-selection))
                     (approve-p (memq prompt-outcome
                                      '(allow-once allow-session always-allow)))
                     (collapsed
                     (pcase prompt-outcome
                       ((or 'allow-once 'allow-session 'always-allow
                            'deny-once 'deny-session)
                        (mevedel-permission--apply-prompt-result
                         prompt-outcome rule-tool session workspace
                         (if (and approve-p selected)
                             (plist-get selected :path)
                           (and (eq rule-key :path) rule-value))
                         :spec-key rule-key
                         :spec-value rule-value
                         :resource-access
                         (if (and approve-p selected)
                             (plist-get selected :access)
                           resource-access)
                         :resource-recursive
                         (and approve-p (plist-get selected :recursive))))
                       ((or 'allow 'deny 'aborted) prompt-outcome)
                       (other other))))
                (mevedel-tool-permission--dispatch-outcome
                 collapsed prompt-context next fail
                 :tool-name tool-name :path path :session session
                 :workspace workspace :workspace-root workspace-root
                 :allowed-roots allowed-roots
                 :permission-context permission-context))
            (error
             (funcall fail (error-message-string err)))))
        decision-metadata
        (and (eq 'allow (plist-get context :hook-permission-decision))
             'allow))))
    ((or 'allow 'approve 'implement 'implement-clear)
     (funcall next context))
    ('deny
     (mevedel-tool-permission-deny
      context fail "Permission denied" nil
      (mevedel-tool-permission--denial-provenance context decision)))
    (`(deny . ,reason)
     (mevedel-tool-permission-deny
      context fail (format "Permission denied: %s" reason) reason
      (mevedel-tool-permission--denial-provenance context decision)))
    (`(feedback . ,text)
     (mevedel-tool-permission-deny
      context fail (format "Permission denied: %s" text) text
      (mevedel-tool-permission--denial-provenance context decision)))
    ('aborted
     (funcall fail "aborted"))
    ;; Defense in depth: an unrecognized outcome (slot bug, primitive
    ;; returning an unexpected symbol) fails loudly rather than
    ;; stranding the FSM with neither `next' nor `fail' fired.
    (_ (funcall fail (format "Unexpected permission outcome: %S"
                             outcome)))))

(provide 'mevedel-tool-permission)
;;; mevedel-tool-permission.el ends here
