;;; mevedel-permission-queue.el -- Session permission FIFO queue -*- lexical-binding: t -*-

;;; Commentary:

;; Heterogeneous FIFO on the session struct holding generic
;; permission, Bash, Eval, and execution-authority entries.  The prompt UI
;; renders the head so a single visible prompt covers all cases at any
;; moment.  Coalesce on rule-creating outcomes
;; (`allow-session', `deny-session', `always-allow') re-evaluates
;; queued entries through the decision chain; protected paths skip
;; allow rules but coalesce on deny.  Request teardown sweeps its entries
;; with `aborted' while unrelated retained-agent prompts remain queued.
;;
;; The queue is transient runtime state: never persisted to the
;; sidecar, cleared when the root session is torn down.

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


;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-effective
                  "mevedel-permission-mode"
                  (&optional session data-buffer surface-buffer))
(autoload 'mevedel-permission-mode-effective "mevedel-permission-mode")

;; `mevedel-permission-prompt'
(declare-function mevedel-permission-prompt-render
                  "mevedel-permission-prompt" (entry origin cont count))
(autoload 'mevedel-permission-prompt-render "mevedel-permission-prompt")

;; `mevedel-permission-review'
(declare-function mevedel-permission-review-cancel
                  "mevedel-permission-review" (session &optional request-id reason))
(declare-function mevedel-permission-review-start
                  "mevedel-permission-review" (entry fallback))
(autoload 'mevedel-permission-review-start "mevedel-permission-review")
(defvar mevedel-permission-reviewer 'user)

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-resource-granted-p
                  "mevedel-permission-rules" (path access grants &optional recursive))
(autoload 'mevedel-permission-rules-resource-granted-p "mevedel-permission-rules")

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

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-mode-effective
                  "mevedel-sandbox" (&optional session permission-mode))
(declare-function mevedel-sandbox-pending-facts
                  "mevedel-sandbox"
                  (&optional additional-permissions sandbox-permissions mode workdir))
(autoload 'mevedel-sandbox-mode-effective "mevedel-sandbox")
(autoload 'mevedel-sandbox-pending-facts "mevedel-sandbox")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(autoload 'mevedel-session-artifacts-assert-new-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-request-cancelled-p "mevedel-structs" (cl-x) t)
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
(declare-function mevedel-tool-exec-permission-eval-mode
                  "mevedel-tool-exec-permission" (args))
(declare-function mevedel-tool-exec-permission-reevaluate
                  "mevedel-tool-exec-permission" (entry context))
(autoload 'mevedel-tool-exec-permission-eval-mode "mevedel-tool-exec-permission")
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


(defun mevedel-permission-queue--get (&optional session)
  "Return SESSION's permission-queue slot, or nil.
SESSION defaults to the current session resolved via
`mevedel-queue--current-session'.  Caller must `setf'
the slot through `mevedel-session-permission-queue' to mutate."
  (when-let* ((sess (or session
                        (mevedel-queue--current-session))))
    (mevedel-session-permission-queue sess)))

(defun mevedel-permission-queue--set (queue &optional session)
  "Set SESSION's permission-queue slot to QUEUE.
SESSION defaults to the current session."
  (when-let* ((sess (or session
                        (mevedel-queue--current-session))))
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
                        (mevedel-queue--current-session))))
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
                         :review-source :review-seconds
                         :sandbox-permissions))
            (when (plist-member diagnostic key)
              (setq safe (plist-put safe key (plist-get diagnostic key)))))
          (apply #'mevedel-telemetry-record-audit sess event safe))))))

(defun mevedel-permission--enqueue (entry &optional session)
  "Resolve automatic review for ENTRY before human admission to SESSION."
  (let* ((session (or session (mevedel-queue--current-session)))
         (entry (plist-put (copy-sequence entry) :session session))
         (mode (mevedel-permission-mode-effective session (plist-get entry :data-buffer))))
    (setq entry
          (plist-put entry :permission-id
                     (or (plist-get entry :permission-id)
                         (format "%s-%s" (format-time-string "%s%N")
                                 (gensym "permission-")))))
    (cond
     ((when-let* ((request (plist-get entry :request)))
        (mevedel-request-cancelled-p request))
      (mevedel-permission-queue--safe-settle entry 'aborted 'cancelled))
     ((and session (eq mode 'full-auto))
      (mevedel-permission-queue--safe-settle
       entry
       (pcase (mevedel-permission-queue--reevaluate entry)
         ('allow 'allow-once) ('deny 'deny-once) (_ 'aborted))
       'mode))
     ((and session (eq mevedel-permission-reviewer 'auto))
      (mevedel-permission-review-start
       entry (lambda () (mevedel-permission-queue--admit entry session))))
     (t (mevedel-permission-queue--admit entry session)))))

(defun mevedel-permission-queue--admit (entry &optional session)
  "Append ENTRY (a plist) to the session permission queue.
If the queue was empty, render ENTRY as the visible head immediately.

When SESSION is non-nil, attach ENTRY to that session explicitly.
When no session is available, settle ENTRY as aborted; the queue's
ordering and coalesce semantics require a session struct.

ENTRY plist keys:
  :kind                  -- `generic' / `bash' / `eval' / `sandbox'
  :tool-name             -- string (`generic' and `sandbox')
  :args                  -- keyword plist
  :specifier-key         -- `:path', `:pattern', `:expression',
                            `:domain', or `:name'
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
  (let ((session (or session (mevedel-queue--current-session))))
    (setq entry
          (append
           (list :permission-id (plist-get entry :permission-id)
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
  "Render ENTRY with settlement bound to its owning session."
  (mevedel-permission-prompt-render
   entry (mevedel-permission-queue--attribution-origin entry)
   (lambda (outcome)
     (mevedel-permission-queue--on-head-outcome entry outcome))
   (length (mevedel-permission-queue--get (plist-get entry :session)))))

(defun mevedel-permission-queue--render-head (&optional session)
  "Render the current head of SESSION's permission queue."
  (when-let* ((session (or session
                           (mevedel-queue--current-session)))
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

(defun mevedel-permission-queue-validate-approval (entry outcome)
  "Reject an unrepresentable child directory grant in ENTRY's OUTCOME.
Validation precedes settlement and persistence.  Native, live Eval and
unconfined access need no child mount.  A covering tree grant may
subsume an exact grant, but this function never broadens selected authority."
  (when (and (memq outcome '(allow allow-once allow-session always-allow))
             (memq (plist-get entry :kind) '(bash eval sandbox))
             (not (and (eq (plist-get entry :kind) 'eval)
                       (eq (mevedel-tool-exec-permission-eval-mode
                            (or (plist-get entry :args)
                                (list :mode (plist-get entry :mode))))
                           'live))))
    (let ((session (plist-get entry :session))
          (grants (or (car (plist-get entry :resource-selection-cell))
                      (plist-get (plist-get entry :requested-additional-permissions)
                                 :file-system))))
      (dolist (grant grants)
        (let ((path (plist-get grant :path)))
          (when (and (eq (plist-get grant :access) 'write)
                     (not (plist-get grant :recursive))
                     (file-directory-p path)
                     (not (mevedel-permission-rules-resource-granted-p
                           path 'write grants t))
                     (memq
                      (plist-get
                       (mevedel-sandbox-pending-facts
                        nil (plist-get entry :sandbox-permissions)
                        (mevedel-sandbox-mode-effective
                         session (mevedel-permission-mode-effective
                                  session (plist-get entry :data-buffer)))
                        (or (plist-get entry :execution-directory)
                            (and session (mevedel-session-working-directory session))
                            default-directory))
                       :sandbox)
                      '(bubblewrap refused)))
            (user-error
             "Exact directory writes cannot be confined: %s; select directory-tree scope before approving"
             path)))))))

(defun mevedel-permission-queue--pop (entry outcome &optional phase)
  "Settle queue head ENTRY with OUTCOME and render the next head."
  (let* ((session (plist-get entry :session))
         (queue (and session (mevedel-permission-queue--get session)))
         (head (car queue)))
    (cond
     ((not session)
      (mevedel-permission-queue-validate-approval entry outcome)
      (mevedel-permission-queue--safe-settle entry outcome (or phase 'pop)))
     ((not (or (eq entry head)
               (mevedel-permission-queue--same-interaction-entry-p
                entry head)))
      (mevedel--warn-once
       'permission-queue-stale-settlement
       "permission-queue: stale queue entry settlement ignored"))
     (t
      (setq entry head)
      (mevedel-permission-queue-validate-approval entry outcome)
      ;; Directory and sandbox validation can yield to cancellation or enqueue.
      ;; Only remove the validated head, from the queue that exists now.
      (setq queue (mevedel-permission-queue--get session))
      (when (eq entry (car queue))
        (mevedel-permission-queue--set (cdr queue) session)
        (when (mevedel-permission-queue--safe-settle entry outcome (or phase 'pop))
          (when (memq outcome '(allow-session deny-session always-allow))
            (condition-case err
                (mevedel-permission-queue--coalesce outcome session)
              (error
               (mevedel--warn-once
                'permission-queue-coalesce
                "permission-queue: coalesce error: %S" err))))
          (mevedel-permission-queue--render-head session)))))))

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
                 :patch-session-only-p (plist-get entry :patch-session-only-p)
                 :path (and (eq spec-key :path) spec-value)
                 :pattern (and (eq spec-key :pattern) spec-value)
                 :domain (and (eq spec-key :domain) spec-value)
                 :name (and (eq spec-key :name) spec-value))))
          (setq context (plist-put context :execution-directory
                                   (plist-get entry :execution-directory)))
          (when-let* ((access (plist-get entry :resource-access)))
            (setq context (plist-put context :resource-access access)))
          (let ((resolved
                 (cond
                   ((member tool-name '("Bash" "Eval"))
                    (mevedel-tool-exec-permission-reevaluate entry context))
                   ((eq kind 'generic)
                    (apply #'mevedel-check-permission tool-name
                           (mevedel-permission--checker-args context)))
                   (t 'ask))))
            (if (and (eq resolved 'allow)
                     (not (eq (plist-get context :mode) 'full-auto))
                     (or (memq (plist-get entry :permission-via)
                               '(pre-tool-hook permission-request-hook))
                         (plist-get entry :once-only)))
                'ask
              resolved))))
    (error 'ask)))

(defun mevedel-permission-queue-abort-all (&optional session)
  "Flush SESSION's queue, firing `'aborted' on every entry's callback.
Called from `mevedel-abort' / request-cancel-fn."
  (let* ((session (or session (mevedel-queue--current-session)))
         (queue (and session (mevedel-permission-queue--get session))))
    (when session
      (when (fboundp 'mevedel-permission-review-cancel)
        (mevedel-permission-review-cancel session))
      (mevedel-permission-queue--set nil session))
    (dolist (entry queue)
      (mevedel-permission-queue--safe-settle entry 'aborted 'abort))))

(defun mevedel-permission-queue-sweep-request
    (request-id &optional session no-render)
  "Abort queued entries for REQUEST-ID in SESSION.

When NO-RENDER is non-nil, do not render the next head entry after
sweeping."
  (when request-id
    (let* ((session (or session (mevedel-queue--current-session)))
           (queue (and session (mevedel-permission-queue--get session)))
           (head-before (car queue))
           kept swept)
      (when (fboundp 'mevedel-permission-review-cancel)
        (mevedel-permission-review-cancel session request-id))
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
