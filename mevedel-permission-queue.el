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

(require 'map)
(require 'mevedel-structs)
(require 'mevedel-permission-log)
(require 'mevedel-queue)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-block-turn
                  "mevedel-agent-control" (session path activity))
(autoload 'mevedel-agent-control-block-turn "mevedel-agent-control")

;; `mevedel-agents'
(defvar mevedel--agent-invocation)

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
(autoload 'mevedel-permission--prompt-async-attributed
  "mevedel-permission-prompt")
(autoload 'mevedel-permission--prompt-async-bash "mevedel-permission-prompt")
(autoload 'mevedel-permission--prompt-async-sandbox
  "mevedel-permission-prompt")

;; `mevedel-permissions'
(declare-function mevedel-check-permission
                  "mevedel-permissions" (tool-name &rest args))
(declare-function mevedel-permission--checker-args
                  "mevedel-permissions" (context))
(declare-function mevedel-permission--invocation-context
                  "mevedel-permissions" (&rest args))
(declare-function mevedel-permission--normalize-outcome
                  "mevedel-permissions" (outcome))
(autoload 'mevedel-check-permission "mevedel-permissions")
(autoload 'mevedel-permission--checker-args "mevedel-permissions")
(autoload 'mevedel-permission--invocation-context "mevedel-permissions")
(autoload 'mevedel-permission--normalize-outcome "mevedel-permissions")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(autoload 'mevedel-session-artifacts-assert-new-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-forwarded-audit-p
                  "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-record-audit
                  "mevedel-telemetry" (session event &rest props))
(autoload 'mevedel-telemetry-forwarded-audit-p "mevedel-telemetry")
(autoload 'mevedel-telemetry-record-audit "mevedel-telemetry")

;; `mevedel-tool-exec-permission'
(declare-function mevedel-tool-exec-permission-prompt-eval
                  "mevedel-tool-exec-permission"
                  (expression callback &optional origin count entry
                              mode preserve-ui))
(declare-function mevedel-tool-exec-permission-reevaluate
                  "mevedel-tool-exec-permission" (entry context))
(autoload 'mevedel-tool-exec-permission-prompt-eval
  "mevedel-tool-exec-permission")
(autoload 'mevedel-tool-exec-permission-reevaluate "mevedel-tool-exec-permission")

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))
(autoload 'mevedel--warn-once "mevedel-utilities")

(defcustom mevedel-permission-notify-function nil
  "Function called when an interactive permission card enters the queue.

Called once per card, at admission -- never for decisions the rule
chain or hooks settle without prompting, and never again when the card
is re-rendered, coalesced, or resolved.  No rate limiting is applied.
Firing at enqueue is deliberate: the machine is certainly awake then,
while an idle timer would die with a suspend.

The single argument is the card's entry plist without its internal
`:callback', so the function is always a small wrapper that formats a
message from the entry -- never `notifications-notify' itself, whose
keywords the entry does not carry.  Stable keys: `:kind' (`generic' /
`eval' / `bash' / `sandbox'), `:tool-name', `:specifier-key',
`:specifier-value' (display path, pattern, domain, or name),
`:command' (bash), `:expression' (eval), `:detail' and
`:justification' (sandbox), `:origin' (requesting agent path), and
`:session'.

The return value is ignored, nothing waits on it, and it cannot answer
the prompt.  Errors are demoted: a broken notifier must not break
admission.  Typical wrappers call `notifications-notify' or the
`notify-send' binary for a desktop notification, or a push service
such as ntfy to reach a phone; see the examples in
docs/permissions.md."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'mevedel)

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
PHASE is the settlement source symbol recorded in the permission log.
Return non-nil when this call delivered or consumed the outcome."
  (let ((cell (mevedel-permission-queue--ensure-settled-cell entry)))
    (unless (car cell)
      (setcar cell t)
      (apply #'mevedel-permission-queue--log
             (pcase phase
               ('pop 'permission-resolved)
               ('coalesced 'permission-coalesced)
               ('sweep 'permission-swept)
               (_ 'permission-aborted))
             entry nil :outcome outcome :settlement-source phase
             (when (eq phase 'coalesced)
               (list :resolved (if (memq outcome '(allow allow-once)) 'allow 'deny))))
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
  "Return ENTRY's user-facing prompt attribution, or nil for root."
  (let ((origin (plist-get entry :origin))
        (source (plist-get entry :call-source)))
    (cond
     ((eq source 'ptc)
      (let ((parent (plist-get entry :parent-tool-use-id))
            (child (plist-get entry :tool-use-id)))
        (format "%sToolCall %s%s"
                (if (equal origin "/root") "" (concat origin ": "))
                (or parent "script")
                (if child (format " (child %s)" child) ""))))
     ((not (equal origin "/root")) origin))))

(defun mevedel-permission-queue--log-props (entry &rest props)
  "Return sanitized permission diagnostic properties for ENTRY plus PROPS."
  (let ((base nil))
    (when (plist-member props :outcome)
      (setq props
            (plist-put (copy-sequence props) :outcome
                       (mevedel-permission--normalize-outcome
                        (plist-get props :outcome)))))
    (dolist (key '(:kind :tool-name :specifier-key :specifier-value
                   :protected-path :resource-path :resource-access
                   :origin :tool-use-id :parent-tool-use-id :call-source
                   :command-class :request-id :permission-id
                   :permission-mode-base :permission-mode-effective
                   :resource-originals :permission-via
                   :commands-summary :sandbox-permissions
                   :additional-permissions
                   :requested-additional-permissions
                   :missing-additional-permissions
                   :granted-additional-permissions
                   :justification))
      (when (plist-member entry key)
        (setq base (plist-put base key (plist-get entry key)))))
    ;; Only batch Eval reaches the child-confinement capability card.
    (when-let* ((eval-mode
                 (pcase (plist-get entry :kind)
                   ('eval (plist-get entry :mode))
                   ('sandbox (and (equal (plist-get entry :tool-name) "Eval") "batch")))))
      (setq base (plist-put base :eval-mode eval-mode)))
    (when-let* ((selection (car (plist-get entry :resource-selection-cell))))
      (setq base (plist-put base :selected-resources (copy-tree selection))))
    (when-let* ((lifetime
                 (pcase (plist-get props :outcome)
                   ('allow-once 'invocation)
                   ('allow-session 'session)
                   ('always-allow 'workspace))))
      (setq base (plist-put base :approval-lifetime lifetime)))
    (when-let* ((id (mevedel-queue--entry-metadata-get
                    entry :interaction-id)))
      (setq base (plist-put base :interaction-id id)))
    (append base props)))

(defun mevedel-permission-queue--log (event entry &optional session &rest props)
  "Log permission queue EVENT for ENTRY in SESSION with PROPS."
  (when-let* ((sess (or session
                        (plist-get entry :session)
                        (mevedel-permission-queue--current-session))))
    (let* ((queue-depth
            (+ (length (mevedel-session-permission-queue sess))
               (if (eq event 'permission-enqueued) 1 0)))
           (diagnostic
            (apply #'mevedel-permission-queue--log-props
                   entry :queue-depth queue-depth props)))
      (apply #'mevedel-permission-log sess event diagnostic)
      ;; Queue entries retain exact resources and human explanations for the
      ;; transient interaction.  Only this fixed categorical subset may cross
      ;; into a distinct durable session's unified telemetry.
      (when (mevedel-telemetry-forwarded-audit-p sess)
        (let ((safe (list :queue-depth queue-depth)))
          (dolist (key '(:kind :tool-name :specifier-key :protected-path
                         :resource-access :origin :command-class
                         :permission-id :permission-mode-base :permission-mode-effective
                         :permission-via :approval-lifetime :eval-mode
                         :outcome :resolved :settlement-source
                         :sandbox-permissions))
            (when (plist-member diagnostic key)
              (setq safe (plist-put safe key (plist-get diagnostic key)))))
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
  :tool-use-id           -- nested or provider tool-call identity
  :parent-tool-use-id    -- owning compound tool-call identity
  :call-source           -- nested call source such as `ptc'
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
  (let ((origin (plist-get entry :origin)))
    (unless (mevedel-agent-path-p origin)
      (error "Invalid permission queue origin: %S" origin)))
  (let ((session (or session (mevedel-permission-queue--current-session))))
    (setq entry
          (append
           (list :permission-id (format "%s-%s" (format-time-string "%s%N")
                                        (gensym "permission-"))
                 :permission-mode-base
                 (and session (mevedel-session-permission-mode session)))
           entry))
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
                 (mevedel-agent-control-block-turn
                  session (plist-get entry :origin)
                  'permission-blocked)))
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
                 entry 'aborted 'no-session))
            (setq entry (plist-put entry :session session))
            (mevedel-permission-queue--ensure-settled-cell entry)
            (mevedel-permission-queue--set
             (append (mevedel-permission-queue--get session) (list entry))
             session)
            (when mevedel-permission-notify-function
              (with-demoted-errors "mevedel: permission notify failed: %S"
                (funcall mevedel-permission-notify-function
                         (map-delete (copy-sequence entry) :callback))))
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
    (_ (error "Unknown permission card kind: %s" (plist-get entry :kind)))))

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
          (_ 'aborted))
        'render-failed)))))

(defun mevedel-permission-queue--pop (entry outcome &optional phase)
  "Settle queue head ENTRY with OUTCOME and render the next head."
  (let* ((session (plist-get entry :session))
         (queue (and session (mevedel-permission-queue--get session)))
         (head (car queue)))
    (cond
     ((not session)
      (mevedel-permission-queue--safe-settle entry outcome (or phase 'pop)))
     ((not (or (eq entry head)
               (mevedel-permission-queue--same-interaction-entry-p
                entry head)))
      (mevedel--warn-once
       'permission-queue-stale-settlement
       "permission-queue: stale queue entry settlement ignored"))
     (t
      (setq entry head)
      (mevedel-permission-queue--set (cdr queue) session)
      (when (mevedel-permission-queue--safe-settle entry outcome (or phase 'pop))
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
  (mevedel-permission-queue--pop
   entry outcome (if (eq outcome 'aborted) 'cancelled 'pop)))

(defun mevedel-permission-queue--translate-coalesce-outcome (kind resolved)
  "Translate RESOLVED (`'allow' / `'deny') into the vocabulary KIND expects.
Generic entries and Bash adapters consume `'allow' / `'deny' directly.
Eval and sandbox adapters use their one-invocation outcome vocabulary;
the covering authority has already been stored by another approval."
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
`mevedel-check-permission': a protected resource needs a covering grant,
while deny rules remain final."
  (let (kept settled)
    (dolist (entry (mevedel-permission-queue--get session))
      (let ((resolved (mevedel-permission-queue--reevaluate entry)))
        (if (eq resolved 'ask)
            (push entry kept)
          (push
           (list entry
                 (mevedel-permission-queue--translate-coalesce-outcome
                  (plist-get entry :kind) resolved))
           settled))))
    ;; Remove every resolved entry before callbacks can reenter queue teardown.
    (mevedel-permission-queue--set (nreverse kept) session)
    (dolist (item (nreverse settled))
      (pcase-let ((`(,entry ,outcome) item))
        (mevedel-permission-queue--safe-settle
         entry outcome 'coalesced)))))

(defun mevedel-permission-queue--reevaluate (entry)
  "Re-evaluate ENTRY with current policy and its captured invocation facts.
Return `allow', `deny', or `ask'.  Execution's policy owner checks operation
and complete capability authority together.  Hooks and one-shot restrictions
cannot be cleared by a remembered sibling approval."
  (condition-case _err
      (with-current-buffer (if (buffer-live-p (plist-get entry :data-buffer))
                               (plist-get entry :data-buffer)
                             (current-buffer))
        (let* ((session (plist-get entry :session))
               (mevedel--session session)
               (mevedel--current-request (plist-get entry :request))
               (mevedel--agent-invocation (plist-get entry :invocation))
               (kind (plist-get entry :kind))
               (tool-name (pcase kind
                            ('bash "Bash")
                            ('eval "Eval")
                            (_ (plist-get entry :tool-name))))
               (spec-key (if (eq kind 'generic)
                             (or (plist-get entry :specifier-key) :path)
                           :pattern))
               (spec-value (if (eq kind 'generic)
                               (plist-get entry :specifier-value)
                             (or (plist-get entry :command)
                                 (plist-get entry :expression)
                                 (plist-get entry :detail))))
               (context
                (mevedel-permission--invocation-context
                 :tool-name tool-name :session session
                 :workspace (and session (mevedel-session-workspace session))
                 :buffer (plist-get entry :data-buffer)
                 :request (plist-get entry :request)
                 :invocation (plist-get entry :invocation)
                 :args (plist-get entry :args)
                 :one-shot-mutations-p (plist-get entry :once-only)
                 :path (and (eq spec-key :path) spec-value)
                 :pattern (and (eq spec-key :pattern) spec-value)
                 :domain (and (eq spec-key :domain) spec-value)
                 :name (and (eq spec-key :name) spec-value))))
          (setq context (plist-put context :execution-directory
                                   (plist-get entry :execution-directory)))
          (when-let* ((access (plist-get entry :resource-access)))
            (setq context (plist-put context :resource-access access)))
          (let ((resolved
                 (pcase kind
                   ('generic
                    (apply #'mevedel-check-permission tool-name
                           (mevedel-permission--checker-args context)))
                   ((or 'bash 'eval 'sandbox)
                    (mevedel-tool-exec-permission-reevaluate entry context))
                   (_ 'ask))))
            (if (and (eq resolved 'allow)
                     (or (memq (plist-get entry :permission-via)
                               '(pre-tool-hook permission-request-hook))
                         (plist-get entry :once-only)))
                'ask
              resolved))))
    (error 'ask)))

(defun mevedel-permission-queue-abort-all (&optional session)
  "Flush SESSION's queue, firing `'aborted' on every entry's callback.
Called from `mevedel-abort' / request-cancel-fn."
  (let* ((session (or session (mevedel-permission-queue--current-session)))
         (queue (and session (mevedel-permission-queue--get session))))
    (when session
      (mevedel-permission-queue--set nil session))
    (dolist (entry queue)
      (mevedel-permission-queue--safe-settle entry 'aborted 'abort))))

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
          (mevedel-permission-queue--safe-settle
           entry 'aborted 'sweep))
        (when (and kept
                   (not no-render)
                   (not (eq head-before (car kept))))
          (mevedel-permission-queue--render-head session))))))

(provide 'mevedel-permission-queue)

;;; mevedel-permission-queue.el ends here
