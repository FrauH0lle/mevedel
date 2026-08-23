;;; mevedel-permission-queue.el -- Session permission FIFO queue -*- lexical-binding: t -*-

;;; Commentary:

;; Heterogeneous FIFO on the session struct holding generic
;; permission, Bash, Eval, and execution-authority entries.  Render-head
;; dispatches on `:kind' so a single visible prompt covers all cases at any
;; moment.  Coalesce on rule-creating outcomes
;; (`allow-session', `deny-session', `always-allow') re-evaluates
;; queued entries through the decision chain; protected paths skip
;; allow rules but coalesce on deny.  Per-agent terminal-state sweep
;; fires `'aborted' on entries owned by an agent that has unwound.
;;
;; The queue is transient runtime state: never persisted to the
;; sidecar, empty at every completed-turn boundary.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-structs)
(require 'mevedel-permission-log)
(require 'mevedel-queue)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-block-turn
                  "mevedel-agent-control" (session path activity))

;; `mevedel-bash-policy'
(declare-function mevedel-bash-policy-check-permission
                  "mevedel-bash-policy" (command &rest keys))

;; `mevedel-permission-prompt'
(declare-function mevedel-permission--prompt-async-attributed
                  "mevedel-permission-prompt"
                  (tool-name path include-always origin cont
                             &optional count entry))
(declare-function mevedel-permission--prompt-async-bash
                  "mevedel-permission-prompt"
                  (command dangerous include-always origin cont
                           &optional count entry))
(declare-function mevedel-permission--prompt-async-sandbox
                  "mevedel-permission-prompt"
                  (tool-name detail justification origin cont
                             &optional count entry))

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-bucket-decision
                  "mevedel-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission-rules-resource-granted-p
                  "mevedel-permission-rules" (path access grants))

;; `mevedel-permissions'
(declare-function mevedel-check-permission
                  "mevedel-permissions" (tool-name &rest args))
(declare-function mevedel-permission--checker-args
                  "mevedel-permissions" (context))
(declare-function mevedel-permission--invocation-context
                  "mevedel-permissions" (&rest args))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))

;; `mevedel-structs'
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-forwarded-audit-p
                  "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-record-audit
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tool-exec-permission'
(declare-function mevedel-tool-exec-permission-full-escalation-rule-decision
                  "mevedel-tool-exec-permission"
                  (tool-name detail buckets level))
(declare-function mevedel-tool-exec-permission-prompt-eval
                  "mevedel-tool-exec-permission"
                  (expression callback &optional origin count entry
                              mode preserve-ui))

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `mevedel-workspace'
(declare-function mevedel--all-allowed-roots
                  "mevedel-workspace" (&optional buffer))

(defvar mevedel-permission-queue--settled-cells
  (make-hash-table :test #'eq :weakness 'key)
  "Entry identity table for exactly-once permission settlement.")


(defun mevedel-permission-queue--current-session ()
  "Resolve the session struct that owns the permission queue.

Reads `mevedel--session' from the current buffer, falling back
to `mevedel--data-buffer''s buffer-local binding when present.
View buffers expose the data buffer reference but not the session struct."
  (mevedel-queue--current-session))

(defun mevedel-permission-queue--get (&optional session)
  "Return SESSION's permission-queue slot, or nil.
SESSION defaults to the current session resolved via
`mevedel-permission-queue--current-session'.  Caller must `setf'
the slot through `mevedel-session-permission-queue' to mutate."
  (when-let* ((sess (or session
                        (mevedel-permission-queue--current-session))))
    (mevedel-session-permission-queue sess)))

(defun mevedel-permission-queue--set (queue &optional session)
  "Set SESSION's permission-queue slot to QUEUE.
SESSION defaults to the current session."
  (when-let* ((sess (or session
                        (mevedel-permission-queue--current-session))))
    (setf (mevedel-session-permission-queue sess) queue)))

(defun mevedel-permission-queue--ensure-settled-cell (entry)
  "Return ENTRY's settled cell, adding one when absent."
  (or (gethash entry mevedel-permission-queue--settled-cells)
      (let ((cell (cons nil nil)))
        (puthash entry cell mevedel-permission-queue--settled-cells)
        cell)))

(defun mevedel-permission-queue--same-interaction-entry-p (a b)
  "Return non-nil when permission entries A and B own one interaction."
  (let ((a-id (mevedel-queue--entry-metadata-get a :interaction-id))
        (b-id (mevedel-queue--entry-metadata-get b :interaction-id)))
    (and a-id b-id (equal a-id b-id))))

(defun mevedel-permission-queue--safe-settle (entry outcome phase)
  "Settle ENTRY once with OUTCOME during PHASE.
Return non-nil when this call delivered or consumed the outcome."
  (let ((cell (mevedel-permission-queue--ensure-settled-cell entry)))
    (unless (car cell)
      (setcar cell t)
      (unwind-protect
          (condition-case err
              (when-let* ((callback (plist-get entry :callback)))
                (funcall callback outcome))
            (error
             (mevedel--warn-once
              (list 'permission-queue-callback phase)
              "permission-queue: %s callback error: %S" phase err)))
        (mevedel-queue--unregister-entry-interaction entry))
      t)))

(defun mevedel-permission-queue--attribution-origin (entry)
  "Return ENTRY's non-root canonical path for prompt attribution."
  (let ((origin (plist-get entry :origin)))
    (and (not (equal origin "/root")) origin)))

(defun mevedel-permission-queue--log-props (entry &rest props)
  "Return sanitized permission diagnostic properties for ENTRY plus PROPS."
  (let ((base nil))
    (dolist (key '(:kind :tool-name :specifier-key :specifier-value
                   :protected-path :resource-path :resource-access
                   :origin :command-class
                   :mode :commands-summary :sandbox-permissions
                   :additional-permissions
                   :requested-additional-permissions
                   :missing-additional-permissions
                   :granted-additional-permissions
                   :justification))
      (when (plist-member entry key)
        (setq base (plist-put base key (plist-get entry key)))))
    (when-let* ((id (mevedel-queue--entry-metadata-get
                    entry :interaction-id)))
      (setq base (plist-put base :interaction-id id)))
    (append base props)))

(defun mevedel-permission-queue--log (event entry &optional session &rest props)
  "Log permission queue EVENT for ENTRY in SESSION with PROPS."
  (require 'mevedel-telemetry)
  (when-let* ((sess (or session
                        (plist-get entry :session)
                        (mevedel-permission-queue--current-session))))
    (let ((queue-depth
           (+ (length (mevedel-session-permission-queue sess))
              (if (eq event 'permission-enqueued) 1 0))))
      (apply #'mevedel-permission-log
             sess event
             (apply #'mevedel-permission-queue--log-props
                    entry :queue-depth queue-depth props))
      ;; Queue entries retain exact resources and human explanations for the
      ;; transient interaction.  Only this fixed categorical subset may cross
      ;; into a distinct durable session's unified telemetry.
      (when (mevedel-telemetry-forwarded-audit-p sess)
        (let ((safe (list :queue-depth queue-depth)))
          (dolist (key '(:kind :tool-name :specifier-key :protected-path
                         :resource-access :origin :command-class :mode
                         :sandbox-permissions))
            (when (plist-member entry key)
              (setq safe (plist-put safe key (plist-get entry key)))))
          (dolist (key '(:outcome :resolved))
            (when (plist-member props key)
              (setq safe (plist-put safe key (plist-get props key)))))
          (apply #'mevedel-telemetry-record-audit sess event safe))))))

(defun mevedel-permission--enqueue (entry &optional session)
  "Append ENTRY (a plist) to the session permission queue.
If the queue was empty, render ENTRY as the visible head immediately.

When SESSION is non-nil, attach ENTRY to that session explicitly.
When no session is available, settle ENTRY as aborted; the queue's
ordering and coalesce semantics require a session struct.

ENTRY plist keys:
  :kind                  -- `generic' / `bash' / `eval' / `sandbox'
  :tool-name             -- string (`generic' and `sandbox')
  :args                  -- keyword plist
  :specifier-key         -- `:path' / `:pattern' / `:domain' / `:name'
  :specifier-value       -- display path / pattern / domain
  :protected-path        -- non-nil when the original path is protected
  :resource-path         -- exact additive filesystem path (`sandbox' only)
  :resource-access       -- `read' / `write' for exact filesystem grants
  :include-always        -- boolean
  :workspace             -- workspace struct or nil
  :origin                -- canonical requesting agent path
  :command               -- string (`bash' only)
  :analysis              -- normalized Bash analysis (`bash' only)
  :command-class         -- Bash command class (`bash' only)
  :expression            -- string (`eval' only)
  :detail                -- command or expression (`sandbox' only)
  :additional-permissions -- additive profile (`sandbox' only)
  :requested-additional-permissions -- complete additive profile
  :missing-additional-permissions -- unresolved additive profile
  :granted-additional-permissions -- previously granted additive profile
  :justification         -- user-facing reason (`sandbox' only)
  :callback              -- function: (lambda (outcome) ...)"
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (let ((origin (plist-get entry :origin)))
    (unless (mevedel-agent-path-p origin)
      (error "Invalid permission queue origin: %S" origin)))
  (let ((session (or session (mevedel-permission-queue--current-session))))
    ;; A permission entry without a request id starts new work.  Entries
    ;; attached to an already-live request are allowed to settle while the
    ;; owner drains for cooperative transfer.
    (when (and session
               (plist-get (mevedel-session-control-transfer session) :state)
               (not (plist-get entry :request-id)))
      (mevedel-session-artifacts-assert-new-mutation-authority session))
    (mevedel-permission-queue--log 'permission-enqueued entry session)
    (let* ((release
            (and session
                 (progn
                   (require 'mevedel-agent-control)
                   (mevedel-agent-control-block-turn
                    session (plist-get entry :origin)
                    'permission-blocked))))
           (callback (plist-get entry :callback))
           (wrapped
            (if release
                (lambda (outcome)
                  (funcall release)
                  (when callback
                    (funcall callback outcome)))
              callback))
           (entry (plist-put (copy-sequence entry) :callback wrapped)))
      (condition-case err
          (if (not session)
              (progn
                (mevedel--warn-once
                 'permission-queue-no-session
                 "permission-queue: enqueue with no session")
                (mevedel-permission-queue--safe-settle
                 entry 'aborted "no-session"))
            (setq entry (plist-put entry :session session))
            (mevedel-permission-queue--ensure-settled-cell entry)
            (mevedel-permission-queue--set
             (append (mevedel-permission-queue--get session) (list entry))
             session)
            ;; Re-render the head so its pending count includes new siblings.
            (mevedel-permission-queue--render-head session))
        (error
         (when release
           (funcall release))
         (signal (car err) (cdr err)))))))

(defun mevedel-permission-queue--render-entry (entry)
  "Render ENTRY directly via the kind-specific dispatcher.
Used by the permission queue's head renderer."
  (pcase (plist-get entry :kind)
    ('generic (mevedel-permission-queue--render-generic entry))
    ('bash (mevedel-permission-queue--render-bash entry))
    ('eval (mevedel-permission-queue--render-eval entry))
    ('sandbox (mevedel-permission-queue--render-sandbox entry))
    (_
     (mevedel--warn-once
      'permission-queue-unknown-kind
      "permission-queue: unknown :kind %S, dropping"
      (plist-get entry :kind))
     (let ((cb (plist-get entry :callback)))
       (when (functionp cb)
         (condition-case _ (funcall cb 'aborted) (error nil)))))))

(defun mevedel-permission-queue--render-head (&optional session)
  "Render the current head of SESSION's permission queue.
Dispatches on entry's `:kind' via `--render-entry'."
  (when-let* ((session (or session
                           (mevedel-permission-queue--current-session)))
              (head (car (mevedel-permission-queue--get session))))
    (condition-case err
        (mevedel-permission-queue--render-entry head)
      (error
       (mevedel--warn-once
        'permission-queue-render
        "permission-queue: render error: %S" err)
       (mevedel-permission-queue--pop
        head
        (pcase (plist-get head :kind)
          ('bash '(deny . "Bash permission UI unavailable"))
          ('sandbox '(deny . "Additional permission UI unavailable"))
          (_ 'aborted)))))))

(defun mevedel-permission-queue--pop (entry outcome)
  "Settle queue head ENTRY with OUTCOME and render the next head."
  (let* ((session (plist-get entry :session))
         (queue (and session (mevedel-permission-queue--get session)))
         (head (car queue)))
    (cond
     ((not session)
      (mevedel-permission-queue--safe-settle entry outcome "pop"))
     ((not (or (eq entry head)
               (mevedel-permission-queue--same-interaction-entry-p
                entry head)))
      (mevedel--warn-once
       'permission-queue-stale-settlement
       "permission-queue: stale queue entry settlement ignored"))
     (t
      (setq entry head)
      (mevedel-permission-queue--set (cdr queue) session)
      (when (mevedel-permission-queue--safe-settle entry outcome "pop")
        (when (memq outcome '(allow-session deny-session always-allow))
          (condition-case err
              (mevedel-permission-queue--coalesce outcome session)
            (error
             (mevedel--warn-once
              'permission-queue-coalesce
              "permission-queue: coalesce error: %S" err))))
        (mevedel-permission-queue--render-head session))))))

(defun mevedel-permission-queue--render-generic (entry)
  "Render a generic-kind permission ENTRY as the visible head."
  (require 'mevedel-permission-prompt)
  (let ((tool-name (plist-get entry :tool-name))
        (path (plist-get entry :specifier-value))
        (include-always (plist-get entry :include-always))
        (count (length (mevedel-permission-queue--get
                        (plist-get entry :session))))
        (origin (mevedel-permission-queue--attribution-origin entry))
        (cb (lambda (outcome)
              (mevedel-permission-queue--on-head-outcome entry outcome))))
    (mevedel-permission--prompt-async-attributed
     tool-name path include-always origin cb count entry)))

(defun mevedel-permission-queue--render-bash (entry)
  "Render a bash-kind permission ENTRY using the Bash permission UI.

Bash uses the same FIFO machinery as generic permissions.  Read-only and
unknown commands may offer rule-creating outcomes; dangerous and complex
commands do not.  If the helper is unavailable, signal so the permission queue
removes the head and returns the pinned tool-level denial."
  (require 'mevedel-permission-prompt)
  (let ((command (plist-get entry :command))
        (command-class (plist-get entry :command-class))
        (include-always (plist-get entry :include-always))
        (count (length (mevedel-permission-queue--get
                        (plist-get entry :session)))))
    (unless (fboundp 'mevedel-permission--prompt-async-bash)
      (error "Bash permission UI unavailable"))
    (mevedel-permission--prompt-async-bash
     command command-class include-always
     (mevedel-permission-queue--attribution-origin entry)
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome))
     count entry)))

(defun mevedel-permission-queue--render-eval (entry)
  "Render an eval-kind permission ENTRY using the specialized Eval UI.
Calls `mevedel-tool-exec-permission-prompt-eval' with the entry's
`:expression'.  The UI returns one of `'allow-once' / `'deny-once' /
`(feedback . TEXT)' / `'aborted'; the queue passes these through
unchanged to the entry's callback (the eval slot adapter does the
final mapping)."
  (require 'mevedel-tool-exec-permission)
  (let ((expr (plist-get entry :expression))
        (mode (plist-get entry :mode))
        (preserve-ui (plist-get entry :preserve-ui))
        (origin (mevedel-permission-queue--attribution-origin entry))
        (count (length (mevedel-permission-queue--get
                        (plist-get entry :session)))))
    (mevedel-tool-exec-permission-prompt-eval
     expr
     (lambda (outcome)
       (mevedel-permission-queue--on-head-outcome entry outcome))
     origin count entry mode preserve-ui)))

(defun mevedel-permission-queue--render-sandbox (entry)
  "Render a child-execution permission ENTRY."
  (require 'mevedel-permission-prompt)
  (unless (fboundp 'mevedel-permission--prompt-async-sandbox)
    (error "Additional permission UI unavailable"))
  (mevedel-permission--prompt-async-sandbox
   (plist-get entry :tool-name)
   (plist-get entry :detail)
   (plist-get entry :justification)
   (mevedel-permission-queue--attribution-origin entry)
   (lambda (outcome)
     (mevedel-permission-queue--on-head-outcome entry outcome))
   (length (mevedel-permission-queue--get (plist-get entry :session)))
   entry))

(defun mevedel-permission-queue--on-head-outcome (entry outcome)
  "Settle ENTRY with OUTCOME, then advance ENTRY's session queue.
Coalesce on rule-creating outcomes (`allow-session',
`deny-session', `always-allow').  Then render the next head.

Uses the session reference captured on ENTRY at enqueue time
rather than reading the ambient `mevedel--session', so settlement
runs correctly regardless of which buffer fired the keypress."
  (mevedel-permission-queue--log
   'permission-resolved entry nil :outcome outcome)
  (mevedel-permission-queue--pop entry outcome))

(defun mevedel-permission-queue--translate-coalesce-outcome (kind resolved)
  "Translate RESOLVED (`'allow' / `'deny') into the vocabulary KIND expects.
Generic entries and Bash adapters can consume `'allow' / `'deny'
from coalescing directly.  Eval does not coalesce because Eval
always asks, but keep a defensive mapping to its authoritative
queue vocabulary."
  (pcase kind
    ((or 'generic 'bash)
     ;; The pipeline's wrapper at mevedel-pipeline.el handles
     ;; `'allow' / `'deny' directly; Bash's adapter does too.
     resolved)
    ((or 'eval 'sandbox)
     (pcase resolved
       ('allow 'allow-once)
       ('deny 'deny-once)
       (_ resolved)))
    (_ resolved)))

(defun mevedel-permission-queue--coalesce (_rule-outcome &optional session)
  "Re-evaluate SESSION's queued entries against newly stored authority.
Entries that resolve to a non-`ask' outcome via
`mevedel-check-permission' fire their callbacks with that outcome
translated for their kind and are removed from the queue.  Entries that
still resolve to `ask' stay in place.

Protected-path and deny precedence is handled inside
`mevedel-check-permission': a protected resource needs an exact grant,
while deny rules remain final."
  (let (kept settled)
    (dolist (entry (mevedel-permission-queue--get session))
      (let ((resolved (mevedel-permission-queue--reevaluate entry)))
        (if (eq resolved 'ask)
            (push entry kept)
          (push
           (list entry resolved
                 (mevedel-permission-queue--translate-coalesce-outcome
                  (plist-get entry :kind) resolved))
           settled))))
    ;; Remove every resolved entry before callbacks can reenter queue teardown.
    (mevedel-permission-queue--set (nreverse kept) session)
    (dolist (item (nreverse settled))
      (pcase-let ((`(,entry ,resolved ,outcome) item))
        (mevedel-permission-queue--log
         'permission-coalesced entry session
         :resolved resolved :outcome outcome)
        (mevedel-permission-queue--safe-settle
         entry outcome "coalesced")))))

(defun mevedel-permission-queue--reevaluate (entry)
  "Re-evaluate ENTRY through the decision chain with current rules.
Return one of `allow' / `deny' / `ask'.

Dispatches on `:kind' (generic/bash/eval/sandbox).

Critical: `mevedel-check-permission' consumes session-rules,
persistent-rules, mode, and workspace-root via keyword args; it
does not read `mevedel--session'.  An earlier draft only bound
`mevedel--session' inside this function and the just-created
session rule was invisible to queued sibling re-evaluation --
the FIFO queue's central rule-coalescing was effectively a
no-op.  This function now extracts the rule context from the
entry's captured :session and passes it explicitly.

For Bash, the entry's captured execution directory remains part of the
re-evaluation context."
  (require 'mevedel-bash-policy)
  (require 'mevedel-permission-rules)
  (require 'mevedel-permissions)
  (require 'mevedel-tool-exec-permission)
  (let* ((session (plist-get entry :session))
         (workspace
          (and session (mevedel-session-workspace session)))
         (allowed-roots
          (when (and workspace (fboundp 'mevedel--all-allowed-roots))
            (ignore-errors (mevedel--all-allowed-roots))))
         (mevedel--session (or session
                               (and (boundp 'mevedel--session)
                                    mevedel--session))))
    (pcase (plist-get entry :kind)
      ('generic
       (let ((tool-name (plist-get entry :tool-name))
             (spec-key (or (plist-get entry :specifier-key) :path))
             (spec-value (plist-get entry :specifier-value)))
         (condition-case _err
             (let ((context
                    (mevedel-permission--invocation-context
                     :tool-name tool-name
                     :session session
                     :workspace workspace
                     :allowed-roots allowed-roots
                     :path (and (eq spec-key :path) spec-value)
                     :pattern (and (eq spec-key :pattern) spec-value)
                     :domain (and (eq spec-key :domain) spec-value)
                     :name (and (eq spec-key :name) spec-value))))
               (when-let* ((access (plist-get entry :resource-access)))
                 (setq context
                       (plist-put context :resource-access access)))
               (apply #'mevedel-check-permission
                      tool-name
                      (mevedel-permission--checker-args context)))
           (error 'ask))))
      ('bash
       (let* ((command (plist-get entry :command))
              (context
               (mevedel-permission--invocation-context
                :tool-name "Bash"
                :session session
                :workspace workspace
                :allowed-roots allowed-roots
                :pattern command))
              (context
               (plist-put context :execution-directory
                          (plist-get entry :execution-directory)))
              (rule-decision
               (condition-case _err
                   (apply #'mevedel-check-permission
                          "Bash"
                          (mevedel-permission--checker-args context))
                 (error 'ask))))
         (cond
          ((eq rule-decision 'deny) 'deny)
          ((eq rule-decision 'allow)
           (let ((safety
                  (condition-case _err
                      (mevedel-bash-policy-check-permission
                       command :trust-literal-p nil
                       :permission-context context)
                    (error 'ask))))
             (if (memq safety '(allow deny)) safety 'ask)))
          (t 'ask))))
      ('sandbox
       (if (eq (plist-get entry :sandbox-permissions) 'require-escalated)
           (let* ((tool-name (plist-get entry :tool-name))
                  (detail (plist-get entry :detail))
                  (context
                   (mevedel-permission--invocation-context
                    :tool-name tool-name
                    :session session
                    :workspace workspace
                    :pattern detail))
                  (buckets (plist-get context :buckets))
                  (level-action
                   (mevedel-tool-exec-permission-full-escalation-rule-decision
                    tool-name detail buckets 'require-escalated)))
             (cond
              ((eq level-action 'deny) 'deny)
              ((eq level-action 'allow) 'allow)
              (t 'ask)))
         (let* ((missing
                 (plist-get entry :missing-additional-permissions))
                (network (plist-get missing :network))
                (resources
                 (or (plist-get missing :file-system)
                     (when-let* ((path (plist-get entry :resource-path))
                                 (access (plist-get entry :resource-access)))
                       (list (list :path path :access access)))))
                (tool-name (plist-get entry :tool-name))
                decisions)
           (dolist (resource resources)
             (let* ((path (plist-get resource :path))
                    (access (plist-get resource :access))
                    (context
                     (plist-put
                      (mevedel-permission--invocation-context
                       :tool-name tool-name
                       :session session
                       :workspace workspace
                       :path path)
                      :resource-access access))
                    (rule-action
                     (mevedel-permission-rules-bucket-decision
                      (plist-get context :buckets)
                      tool-name path nil nil nil)))
               (push
                (cond
                 ((memq rule-action '(deny ask)) rule-action)
                 ((mevedel-permission-rules-resource-granted-p
                   path access (plist-get context :resource-grants))
                  'allow)
                 (t 'ask))
                decisions)))
           (cond
            ((memq 'deny decisions) 'deny)
            ((or network (memq 'ask decisions) (null decisions)) 'ask)
            (t 'allow)))))
      ('eval 'ask)
      (_ 'ask))))

(defun mevedel-permission-queue-abort-all (&optional session)
  "Flush SESSION's queue, firing `'aborted' on every entry's callback.
Called from `mevedel-abort' / request-cancel-fn."
  (let* ((session (or session (mevedel-permission-queue--current-session)))
         (queue (and session (mevedel-permission-queue--get session))))
    (dolist (entry queue)
      (mevedel-permission-queue--log
       'permission-aborted entry session :outcome 'aborted))
    (when session
      (mevedel-permission-queue--set nil session))
    (dolist (entry queue)
      (mevedel-permission-queue--safe-settle entry 'aborted "abort"))))

(defun mevedel-permission-queue-sweep-request
    (request-id &optional session no-render)
  "Abort queued entries for REQUEST-ID in SESSION.

When NO-RENDER is non-nil, do not render the next head entry after
sweeping."
  (when request-id
    (let* ((session (or session (mevedel-permission-queue--current-session)))
           (queue (and session (mevedel-permission-queue--get session)))
           (head-before (car queue))
           kept swept)
      (dolist (entry queue)
        (if (equal (plist-get entry :request-id) request-id)
            (push entry swept)
          (push entry kept)))
      (when session
        (setq kept (nreverse kept))
        (mevedel-permission-queue--set kept session)
        (dolist (entry (nreverse swept))
          (mevedel-permission-queue--log
           'permission-swept entry session
           :outcome 'aborted :sweep-request-id request-id)
          (mevedel-permission-queue--safe-settle
           entry 'aborted "sweep"))
        (when (and kept
                   (not no-render)
                   (not (eq head-before (car kept))))
          (mevedel-permission-queue--render-head session))))))

(provide 'mevedel-permission-queue)

;;; mevedel-permission-queue.el ends here
