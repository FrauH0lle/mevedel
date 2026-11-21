;;; niwa-permission-queue.el -- Session permission FIFO queue -*- lexical-binding: t -*-

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
(require 'niwa-execution-grants)
(require 'niwa-structs)
(require 'niwa-permission-log)
(require 'niwa-notify)
(require 'niwa-queue)
(require 'niwa-utilities)

;; `niwa-agent-control'
(declare-function niwa-agent-control-block-turn
                  "niwa-agent-control" (session path activity))
(autoload 'niwa-agent-control-block-turn "niwa-agent-control")

;; `niwa-agents'
(defvar niwa--agent-invocation)

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration-retire-permission-requests
                  "niwa-collaboration-guest" (session entries))
(autoload 'niwa-collaboration-retire-permission-requests
  "niwa-collaboration-guest")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer request invocation))
(declare-function niwa-permission-mode-for-context
                  "niwa-permission-mode" (context))
(autoload 'niwa-permission-mode-for-context "niwa-permission-mode")
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-permission-prompt'
(declare-function niwa-permission-prompt-render
                  "niwa-permission-prompt" (entry origin cont count))
(autoload 'niwa-permission-prompt-render "niwa-permission-prompt")

;; `niwa-permission-review'
(declare-function niwa-permission-review-cancel
                  "niwa-permission-review" (session &optional request-id reason))
(declare-function niwa-permission-review-start
                  "niwa-permission-review" (entry fallback))
(autoload 'niwa-permission-review-start "niwa-permission-review")
(defvar niwa-permission-reviewer 'user)

;; `niwa-permissions'
(declare-function niwa-check-permission
                  "niwa-permissions" (tool-name &rest args))
(declare-function niwa-permission--checker-args
                  "niwa-permissions" (context))
(declare-function niwa-permission--invocation-context
                  "niwa-permissions" (&rest args))
(declare-function niwa-permission--normalize-outcome
                  "niwa-permissions" (outcome))
(autoload 'niwa-check-permission "niwa-permissions")
(autoload 'niwa-permission--checker-args "niwa-permissions")
(autoload 'niwa-permission--invocation-context "niwa-permissions")
(autoload 'niwa-permission--normalize-outcome "niwa-permissions")

;; `niwa-sandbox'
(declare-function niwa-sandbox-directory-read-masked-p
                  "niwa-sandbox" (path workdir &optional target))
(declare-function niwa-sandbox-mode-effective
                  "niwa-sandbox" (&optional session permission-mode))
(declare-function niwa-sandbox-pending-facts
                  "niwa-sandbox"
                  (&optional additional-permissions sandbox-permissions mode workdir))
(autoload 'niwa-sandbox-directory-read-masked-p "niwa-sandbox")
(autoload 'niwa-sandbox-mode-effective "niwa-sandbox")
(autoload 'niwa-sandbox-pending-facts "niwa-sandbox")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-structs'
(declare-function niwa-request-cancelled-p "niwa-structs" (cl-x) t)
(declare-function niwa-session-control-transfer
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-absence
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-telemetry'
(declare-function niwa-telemetry-forwarded-audit-p
                  "niwa-telemetry" (session))
(declare-function niwa-telemetry-record-audit
                  "niwa-telemetry" (session event &rest props))
(autoload 'niwa-telemetry-forwarded-audit-p "niwa-telemetry")
(autoload 'niwa-telemetry-record-audit "niwa-telemetry")

;; `niwa-tool-exec-permission'
(declare-function niwa-tool-exec-permission-eval-mode
                  "niwa-tool-exec-permission" (args))
(declare-function niwa-tool-exec-permission-reevaluate
                  "niwa-tool-exec-permission" (entry context))
(autoload 'niwa-tool-exec-permission-eval-mode "niwa-tool-exec-permission")
(autoload 'niwa-tool-exec-permission-reevaluate "niwa-tool-exec-permission")

;; `niwa-transport'
(declare-function niwa-transport-run-at-time
                  "niwa-transport" (seconds function &rest args))
(autoload 'niwa-transport-run-at-time "niwa-transport")

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-rebuild
                  "niwa-view-interaction" ())
(autoload 'niwa-view--interaction-rebuild "niwa-view-interaction")

(defun niwa-permission-queue--set-timeout (symbol value)
  "Set SYMBOL to VALUE; turning the timeout off drops all absence state."
  (set-default symbol value)
  (unless (niwa-permission-queue--timeout-seconds)
    (niwa-permission-queue--disable-all)))

(defcustom niwa-permission-queue-timeout nil
  "Seconds without a human answer before the permission queue expires.

Nil, the default, keeps every permission prompt waiting indefinitely.
With a number, the whole unanswered human queue of a root session expires
once that many seconds pass without a human answering one of its cards:
every queued request is denied with a message telling the model the user
may be absent, and nothing is granted or remembered.  The first card
admitted to an empty queue starts the period; only a real answer from the
local card or an authorized browser restarts it.  Reviewer results,
coalescing, mode changes, folding a card and typing do not.

After expiry the session waits for the user: new requests that need a
human are denied at once, and Goal continuation and queued follow-ups do
not start, until you send a new message.  Turning this option off cancels
a running period and ends any wait.  A changed duration applies from the
next admission or answer.  Unanswered prompts keep a turn, and with it the
system sleep inhibitor, alive, so a value such as 3600 lets an abandoned
session settle."
  :type '(choice (const :tag "Disabled" nil)
                 (natnum :tag "Seconds without an answer" :value 3600))
  :initialize #'custom-initialize-default
  :set #'niwa-permission-queue--set-timeout
  :group 'niwa)

(defvar niwa-permission-queue--settled-cells
  (make-hash-table :test #'eq :weakness 'key)
  "Entry identity table for exactly-once permission settlement.")


(defun niwa-permission-queue--get (&optional session)
  "Return SESSION's permission-queue slot, or nil.
SESSION defaults to the current session resolved via
`niwa-queue--current-session'.  Caller must `setf'
the slot through `niwa-session-permission-queue' to mutate."
  (when-let* ((sess (or session
                        (niwa-queue--current-session))))
    (niwa-session-permission-queue sess)))

(defun niwa-permission-queue--set (queue &optional session)
  "Set SESSION's permission-queue slot to QUEUE.
SESSION defaults to the current session.  An empty QUEUE cancels the
session's armed absence deadline; an established hold stays."
  (when-let* ((sess (or session
                        (niwa-queue--current-session))))
    (setf (niwa-session-permission-queue sess) queue)
    (unless queue
      (niwa-permission-queue--disarm sess))))


;;
;;; Absence deadline and hold

(defconst niwa-permission-queue--expired-reason
  "Permission request expired without a human response; the user may be absent.  This invocation was not authorized.  Do not repeat it solely to obtain an answer.  Requests that need approval will be denied until the user sends a new message; finish already-authorized work and report what is blocked."
  "Model-visible denial for a request whose queue expired.")

(defconst niwa-permission-queue--held-reason
  "Permission denied without prompting: an earlier request expired without a human response and the user may be absent.  Do not repeat it solely to obtain an answer.  Requests that need approval are denied until the user sends a new message; finish already-authorized work and report what is blocked."
  "Model-visible denial for a request made while the session waits.")

(defun niwa-permission-queue--timeout-seconds ()
  "Return the configured absence timeout in seconds, or nil when off."
  (and (numberp niwa-permission-queue-timeout)
       (> niwa-permission-queue-timeout 0)
       niwa-permission-queue-timeout))

(defun niwa-permission-absence-deadline (session)
  "Return the absolute time SESSION's unanswered queue expires, or nil."
  (and session (niwa-permission-queue--timeout-seconds)
       (plist-get (niwa-session-permission-absence session) :deadline)))

(defun niwa-permission-absence-hold (session)
  "Return SESSION's expired-queue hold plist, or nil.
The plist carries `:expired-at' and `:count'."
  (and session (niwa-permission-queue--timeout-seconds)
       (let ((state (niwa-session-permission-absence session)))
         (and (plist-get state :expired-at) state))))

(defun niwa-permission-absence-held-p (session)
  "Return non-nil while SESSION waits for a user message after expiry."
  (and (niwa-permission-absence-hold session) t))

(defun niwa-permission-queue--rebuild-view (session)
  "Rebuild SESSION's root interaction zone, if its view is live."
  (when-let* ((data (niwa-session-root-buffer session))
              ((buffer-live-p data))
              (view (buffer-local-value 'niwa--view-buffer data))
              ((buffer-live-p view)))
    (with-current-buffer view
      (niwa-view--interaction-rebuild))))

(defun niwa-permission-queue--disarm (session)
  "Cancel SESSION's armed deadline, leaving any hold in place."
  (let ((state (niwa-session-permission-absence session)))
    (when (plist-member state :deadline)
      (when (timerp (plist-get state :timer))
        (cancel-timer (plist-get state :timer)))
      (setf (niwa-session-permission-absence session) nil))))

(defun niwa-permission-queue--arm (session)
  "Start SESSION's absence period from now, replacing an armed one.
Do nothing when the timeout is off or the session already waits."
  (when-let* ((seconds (niwa-permission-queue--timeout-seconds))
              ((not (niwa-permission-absence-held-p session))))
    (niwa-permission-queue--disarm session)
    (let ((state (list :timer nil :deadline (+ (float-time) seconds))))
      (setf (niwa-session-permission-absence session) state)
      ;; The state plist is the generation: a stale timer finds another one.
      ;; A timer armed inside a TRAMP handler would otherwise be discarded.
      (plist-put state :timer
                 (niwa-transport-run-at-time
                  seconds #'niwa-permission-queue--expire
                  session state)))))

(defun niwa-permission-queue--ensure-armed (session)
  "Arm SESSION's absence period unless one is already running."
  (unless (plist-get (niwa-session-permission-absence session) :deadline)
    (niwa-permission-queue--arm session)))

(defun niwa-permission-absence-clear (session)
  "End SESSION's wait for a user message.  Return non-nil when one ended.
Callers are the actions that submit a user message; nothing else resumes."
  (when (and session
             (plist-get (niwa-session-permission-absence session)
                        :expired-at))
    (setf (niwa-session-permission-absence session) nil)
    (niwa-permission-queue--rebuild-view session)
    t))

(defun niwa-permission-queue--disable-all ()
  "Drop every live session's deadline and hold."
  (let (sessions)
    (dolist (buffer (buffer-list))
      (when-let* ((session (and (buffer-live-p buffer)
                                (buffer-local-value 'niwa--session buffer)))
                  ((niwa-session-p session)))
        (cl-pushnew session sessions)))
    (dolist (session sessions)
      ;; A card showing the cancelled deadline must redraw without it.
      (when (niwa-session-permission-absence session)
        (niwa-permission-queue--disarm session)
        (setf (niwa-session-permission-absence session) nil)
        (niwa-permission-queue--rebuild-view session)))))

(defun niwa-permission-queue--mark-absence (entry &optional expired)
  "Record absence provenance on ENTRY; with EXPIRED, mark its settled cell."
  (when-let* ((cell (plist-get entry :approval-decision-cell)))
    (setcar cell (list :source 'absence)))
  (when expired
    (setcdr (niwa-permission-queue--ensure-settled-cell entry) 'expired)))

(defun niwa-permission-queue--note-stale (entry)
  "Tell the user that a late answer for ENTRY, retired by expiry, is void."
  (when (eq (cdr (gethash entry niwa-permission-queue--settled-cells))
            'expired)
    (niwa-diagnostic-message
     (niwa-diagnostic-scope (or (plist-get entry :session)
                                 (plist-get entry :workspace)))
     "Request expired; nothing was approved")
    t))

(defun niwa-permission-queue--expire (session state)
  "Expire SESSION's whole unanswered queue when STATE is still its deadline."
  (when (and (eq state (niwa-session-permission-absence session))
             (niwa-permission-queue--timeout-seconds))
    (let ((queue (niwa-permission-queue--get session)))
      ;; The hold exists before any callback can run or enqueue again.
      (setf (niwa-session-permission-absence session)
            (and queue (list :expired-at (float-time)
                             :count (length queue))))
      (when queue
        (niwa-permission-queue--set nil session)
        (dolist (entry queue)
          (niwa-permission-queue--mark-absence entry t))
        (condition-case err
            (niwa-collaboration-retire-permission-requests session queue)
          (error
           (niwa--warn-once
            (niwa-diagnostic-scope session)
            'permission-queue-retire
            "permission-queue: retiring remote requests failed: %S" err)))
        (dolist (entry queue)
          (niwa-permission-queue--safe-settle
           entry (cons 'deny niwa-permission-queue--expired-reason)
           'expired))
        (niwa-permission-queue--rebuild-view session)))))

(defun niwa-permission-queue--deny-if-held (entry session)
  "Deny ENTRY without a card while SESSION waits for a user message.
Return non-nil when it was denied."
  (when (and session (niwa-permission-absence-held-p session))
    (niwa-permission-queue--mark-absence entry)
    (niwa-permission-queue--safe-settle
     entry (cons 'deny niwa-permission-queue--held-reason) 'absence-held)
    t))

(defun niwa-permission-queue--ensure-settled-cell (entry)
  "Return ENTRY's settled cell, adding one when absent."
  (or (gethash entry niwa-permission-queue--settled-cells)
      (let ((cell (cons nil nil)))
        (puthash entry cell niwa-permission-queue--settled-cells)
        cell)))

(defun niwa-permission-queue--same-interaction-entry-p (a b)
  "Return non-nil when permission entries A and B own one interaction."
  (let ((a-id (niwa-queue--entry-metadata-get a :interaction-id))
        (b-id (niwa-queue--entry-metadata-get b :interaction-id)))
    (and a-id b-id (equal a-id b-id))))

(defun niwa-permission-queue--safe-settle (entry outcome phase)
  "Settle ENTRY once with OUTCOME during PHASE.
PHASE is the settlement source symbol recorded in the permission log.
Return non-nil when this call delivered or consumed the outcome."
  (let ((cell (niwa-permission-queue--ensure-settled-cell entry)))
    (unless (car cell)
      (setcar cell t)
      ;; Reusing stored authority is not a new approval of the requested
      ;; exact resources.  Keep that distinction across the callback boundary.
      (when (eq phase 'coalesced)
        (when-let* ((approval (plist-get entry :approval-decision-cell)))
          (setcar approval (list :source 'policy :via 'coalesced))))
      (apply #'niwa-permission-queue--log
             (pcase phase
               ('pop 'permission-resolved)
               ('coalesced 'permission-coalesced)
               ('sweep 'permission-swept)
               ((or 'expired 'absence-held) 'permission-expired)
               (_ 'permission-aborted))
             entry nil :outcome outcome :settlement-source phase
             (when (eq phase 'coalesced)
               (list :resolved (if (memq outcome '(allow allow-once)) 'allow 'deny))))
      (unwind-protect
          (condition-case err
              (when-let* ((callback (plist-get entry :callback)))
                (funcall callback outcome))
            (error
             (niwa--warn-once
              (niwa-diagnostic-scope (or (plist-get entry :session)
                                          (plist-get entry :workspace)))
              (list 'permission-queue-callback phase)
              "%s"
              (if (memq 'user-error (get (car err) 'error-conditions))
                  (error-message-string err)
                (format "permission-queue: %s callback error: %S" phase err)))))
        (niwa-queue--unregister-entry-interaction entry))
      t)))

(defun niwa-permission-queue--attribution-origin (entry)
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

(defun niwa-permission-queue--log-props (entry &rest props)
  "Return sanitized permission diagnostic properties for ENTRY plus PROPS."
  (let ((base nil))
    (when (plist-member props :outcome)
      (setq props
            (plist-put (copy-sequence props) :outcome
                       (niwa-permission--normalize-outcome
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
    ;; Only fresh Eval reaches the child-confinement capability card.
    (when-let* ((eval-mode
                 (pcase (plist-get entry :kind)
                   ('eval (plist-get entry :mode))
                   ('generic (and (equal (plist-get entry :tool-name) "Eval")
                                  (or (plist-get (plist-get entry :args) :mode) "live")))
                   ('sandbox (and (equal (plist-get entry :tool-name) "Eval") "fresh")))))
      (setq base (plist-put base :eval-mode eval-mode)))
    (when-let* ((selection (car (plist-get entry :resource-selection-cell))))
      (setq base (plist-put base :selected-resources (copy-tree selection))))
    (when-let* ((lifetime
                 (pcase (plist-get props :outcome)
                   ('allow-once 'invocation)
                   ('allow-session 'session)
                   ('always-allow 'workspace))))
      (setq base (plist-put base :approval-lifetime lifetime)))
    (when-let* ((id (niwa-queue--entry-metadata-get
                    entry :interaction-id)))
      (setq base (plist-put base :interaction-id id)))
    (append base props)))

(defun niwa-permission-queue--log (event entry &optional session &rest props)
  "Log permission queue EVENT for ENTRY in SESSION with PROPS."
  (when-let* ((sess (or session
                        (plist-get entry :session)
                        (niwa-queue--current-session))))
    (let* ((queue-depth
            (+ (length (niwa-session-permission-queue sess))
               (if (eq event 'permission-enqueued) 1 0)))
           (diagnostic
            (apply #'niwa-permission-queue--log-props
                   entry :queue-depth queue-depth props)))
      (apply #'niwa-permission-log sess event diagnostic)
      ;; Queue entries retain exact resources and human explanations for the
      ;; transient interaction.  Only this fixed categorical subset may cross
      ;; into a distinct durable session's unified telemetry.
      (when (niwa-telemetry-forwarded-audit-p sess)
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
          (apply #'niwa-telemetry-record-audit sess event safe))))))

(defun niwa-permission-queue-justification (entry)
  "Return ENTRY's optional model-supplied reason, or nil when absent or blank.
The reason may travel in the tool arguments for any card kind, including
live Eval and hook cards.  It is quoted explanation, never authorization."
  (let ((text (or (plist-get entry :justification)
                  (plist-get (plist-get entry :args) :justification))))
    (and (stringp text)
         (not (string-empty-p (string-trim text)))
         (string-trim text))))

(defun niwa-permission-request (entry &optional session)
  "Resolve automatic review for ENTRY before human admission to SESSION."
  (let* ((session (or session (niwa-queue--current-session)))
         (entry (plist-put (copy-sequence entry) :session session))
         (mode (niwa-permission-mode-for-context entry)))
    (setq entry
          (plist-put entry :permission-id
                     (or (plist-get entry :permission-id)
                         (format "%s-%s" (format-time-string "%s%N")
                                 (gensym "permission-")))))
    ;; Normalize once for the reviewer and the card.
    (setq entry (plist-put entry :justification
                           (niwa-permission-queue-justification entry)))
    (cond
     ((when-let* ((request (plist-get entry :request)))
        (niwa-request-cancelled-p request))
      (niwa-permission-queue--safe-settle entry 'aborted 'cancelled))
     ;; Standing authority is never granted by a mode or a reviewer.
     ((eq 'schedule (plist-get entry :kind))
      (niwa-permission-queue--admit entry session))
     ((and session (eq mode 'full-auto))
      (niwa-permission-queue--safe-settle
       entry
       (pcase (niwa-permission-queue--reevaluate entry)
         ('allow 'allow-once) ('deny 'deny-once) (_ 'aborted))
       'mode))
     ((and session (eq niwa-permission-reviewer 'auto))
      (niwa-permission-review-start
       entry (lambda () (niwa-permission-queue--admit entry session))))
     (t (niwa-permission-queue--admit entry session)))))

(defun niwa-permission-queue--admit (entry &optional session)
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
  :justification         -- optional model-supplied reason, any kind
  :review-fallback       -- why automatic review left the card to the user
  :callback              -- function: (lambda (outcome) ...)"
  (let ((origin (plist-get entry :origin)))
    (unless (niwa-agent-path-p origin)
      (error "Invalid permission queue origin: %S" origin)))
  (let ((session (or session (niwa-queue--current-session))))
    (setq entry
          (append
           (list :permission-id (plist-get entry :permission-id)
                 :permission-mode-base
                 (and session (niwa-session-permission-mode session))
                 ;; Audit facts describe admission, not later session changes.
                 ;; Policy reevaluation never treats this observation as authority.
                 :permission-mode-effective
                 (or (plist-get entry :permission-mode-effective)
                     (niwa-permission-mode-for-context
                      (plist-put (copy-sequence entry) :session session))))
           entry))
    ;; A permission entry without a request id starts new work.  Entries
    ;; attached to an already-live request are allowed to settle while the
    ;; owner drains for cooperative transfer.
    (when (and session
               (plist-get (niwa-session-control-transfer session) :state)
               (not (plist-get entry :request-id)))
      (niwa-session-artifacts-assert-new-mutation-authority session))
    ;; Automatic review has already prepared the same scope; preparation is
    ;; idempotent, so fallback keeps it.  Hooks saw the original request.
    (setq entry (plist-put entry :session session))
    ;; A session waiting for the user gets no card, notification or deadline.
    (unless (niwa-permission-queue--deny-if-held entry session)
      (when session
        (niwa-permission-queue-prepare-resources entry))
      (niwa-permission-queue--log 'permission-enqueued entry session)
      (let* ((release
              (and session
                   (niwa-agent-control-block-turn
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
            (cond
             ((or (not session)
                  (when-let* ((request (plist-get entry :request)))
                    (niwa-request-cancelled-p request)))
              (unless session
                (niwa--warn-once
                 (niwa-diagnostic-scope (plist-get entry :workspace))
                 'permission-queue-no-session
                 "permission-queue: enqueue with no session"))
              (niwa-permission-queue--safe-settle
               entry 'aborted (if session 'cancelled 'no-session)))
             ;; Preparation can yield; the hold may have begun meanwhile.
             ((niwa-permission-queue--deny-if-held entry session))
             (t
              (niwa-permission-queue--ensure-settled-cell entry)
              (niwa-permission-queue--set
               (append (niwa-permission-queue--get session) (list entry))
               session)
              (niwa-permission-queue--ensure-armed session)
              (when-let* ((woken (niwa-notify session 'permission
                                                 (map-delete (copy-sequence entry) :callback))))
                (niwa-queue--entry-metadata-put entry :lobby-woken woken))
              ;; Re-render the head so its pending count includes new siblings.
              (niwa-permission-queue--render-head session)))
          (error
           (when release
             (funcall release))
           (signal (car err) (cdr err))))))))

(defun niwa-permission-queue--render-entry (entry)
  "Render ENTRY with settlement bound to its owning session."
  (niwa-permission-prompt-render
   entry (niwa-permission-queue--attribution-origin entry)
   (lambda (outcome)
     (niwa-permission-queue--on-head-outcome entry outcome))
   (length (niwa-permission-queue--get (plist-get entry :session)))))

(defun niwa-permission-queue--render-head (&optional session)
  "Render the current head of SESSION's permission queue."
  (when-let* ((session (or session
                           (niwa-queue--current-session)))
              (head (car (niwa-permission-queue--get session))))
    (condition-case err
        (niwa-permission-queue--render-entry head)
      (error
       (niwa--warn-once
        (niwa-diagnostic-scope session)
        'permission-queue-render
        "permission-queue: render error: %S" err)
       (niwa-permission-queue--pop
        head
        (pcase (plist-get head :kind)
          ('bash '(deny . "Bash permission UI unavailable"))
          ('sandbox '(deny . "Additional permission UI unavailable"))
          (_ 'aborted))
        'render-failed)))))

(defun niwa-permission-queue-resource-needs-tree-p (entry grant)
  "Return non-nil when ENTRY cannot confine exact directory GRANT."
  (let* ((session (plist-get entry :session))
         (path (plist-get grant :path))
         (directory (or (plist-get entry :execution-directory)
                        (and session (niwa-session-working-directory session))
                        default-directory)))
    (and (memq (plist-get entry :kind) '(bash eval sandbox))
         (not (and (eq (plist-get entry :kind) 'eval)
                   (eq (niwa-tool-exec-permission-eval-mode
                        (or (plist-get entry :args)
                            (list :mode (plist-get entry :mode))))
                       'live)))
         (not (plist-get grant :recursive))
         (file-directory-p path)
         (memq (plist-get
                (niwa-sandbox-pending-facts
                 nil (plist-get entry :sandbox-permissions)
                 (niwa-sandbox-mode-effective
                  session (niwa-permission-mode-for-context entry))
                 directory)
                :sandbox)
               '(bubblewrap refused))
         (or (eq (plist-get grant :access) 'write)
             (niwa-sandbox-directory-read-masked-p
              path directory (and session (niwa-session-execution-target session)))))))

(defun niwa-permission-queue-select-resource (entry previous selected)
  "Replace PREVIOUS with SELECTED in ENTRY's current and remembered authority."
  (dolist (cell-key '(:resource-selection-cell :remember-authority-cell))
    (when-let* ((cell (plist-get entry cell-key)))
      (if (eq cell-key :resource-selection-cell)
          (setcar cell (mapcar (lambda (grant) (if (equal grant previous) selected grant))
                              (car cell)))
        (dolist (key '(:file-system :resource-grants))
          (when (plist-member (car cell) key)
            (setcar cell
                    (plist-put (car cell) key
                               (mapcar (lambda (grant)
                                         (if (equal grant previous) selected grant))
                                       (plist-get (car cell) key))))))))))

(defun niwa-permission-queue-prepare-resources (entry)
  "Select enforceable defaults for ENTRY before review or human admission.
Only the same existing directory is widened to its tree, and only when its
exact access cannot be confined.  Prepared selections are left unchanged."
  (dolist (grant (car (plist-get entry :resource-selection-cell)))
    (when (niwa-permission-queue-resource-needs-tree-p entry grant)
      (niwa-permission-queue-select-resource
       entry grant (plist-put (copy-sequence grant) :recursive t)))))

(defun niwa-permission-queue-validate-approval (entry outcome)
  "Reject an unrepresentable child directory grant in ENTRY's OUTCOME.
Validation precedes settlement and persistence.  Native, live Eval and
unconfined access need no child mount.  A covering tree grant may
subsume an exact grant, but this function never broadens selected authority."
  (when (memq outcome '(allow allow-once allow-session always-allow))
    (let ((grants (or (car (plist-get entry :resource-selection-cell))
                     (plist-get (plist-get entry :requested-additional-permissions)
                                :file-system))))
      (dolist (grant grants)
        (let ((path (plist-get grant :path)))
          (when (and (niwa-permission-queue-resource-needs-tree-p entry grant)
                     (not (niwa-execution-grants-covering-grant
                           path (plist-get grant :access) grants t
                           (and (plist-get entry :session)
                                (niwa-session-execution-target
                                 (plist-get entry :session))))))
            (user-error
             "Exact directory access cannot be confined: %s; select directory-tree scope before approving"
             path)))))))

(defun niwa-permission-queue--pop (entry outcome &optional phase)
  "Settle queue head ENTRY with OUTCOME and render the next head."
  (let* ((session (plist-get entry :session))
         (queue (and session (niwa-permission-queue--get session)))
         (head (car queue)))
    (cond
     ((not session)
      (niwa-permission-queue-validate-approval entry outcome)
      (niwa-permission-queue--safe-settle entry outcome (or phase 'pop)))
     ((not (or (eq entry head)
               (niwa-permission-queue--same-interaction-entry-p
                entry head)))
      (unless (niwa-permission-queue--note-stale entry)
        (niwa--warn-once
         (niwa-diagnostic-scope session)
         'permission-queue-stale-settlement
         "permission-queue: stale queue entry settlement ignored")))
     (t
      (setq entry head)
      (niwa-permission-queue-validate-approval entry outcome)
      ;; Directory and sandbox validation can yield to cancellation or enqueue.
      ;; Only remove the validated head, from the queue that exists now.
      (setq queue (niwa-permission-queue--get session))
      (if (not (eq entry (car queue)))
          (niwa-permission-queue--note-stale entry)
        (niwa-permission-queue--set (cdr queue) session)
        ;; A real answer restarts the shared period before any callback runs.
        (when (and (eq (or phase 'pop) 'pop)
                   (not (eq outcome 'aborted))
                   (cdr queue))
          (niwa-permission-queue--arm session))
        (when (niwa-permission-queue--safe-settle entry outcome (or phase 'pop))
          (when (memq outcome '(allow-session deny-session always-allow))
            (condition-case err
                (niwa-permission-queue--coalesce outcome session)
              (error
               (niwa--warn-once
                (niwa-diagnostic-scope session)
                'permission-queue-coalesce
                "permission-queue: coalesce error: %S" err))))
          (niwa-permission-queue--render-head session)))))))

(defun niwa-permission-queue--on-head-outcome (entry outcome)
  "Settle ENTRY with OUTCOME, then advance ENTRY's session queue.
Coalesce on rule-creating outcomes (`allow-session',
`deny-session', `always-allow').  Then render the next head.

Uses the session reference captured on ENTRY at enqueue time
rather than reading the ambient `niwa--session', so settlement
runs correctly regardless of which buffer fired the keypress."
  (niwa-permission-queue--pop
   entry outcome (if (eq outcome 'aborted) 'cancelled 'pop)))

(defun niwa-permission-queue--translate-coalesce-outcome (kind resolved)
  "Translate RESOLVED (`'allow' / `'deny') into the vocabulary KIND expects.
Generic entries and Bash adapters consume `'allow' / `'deny' directly.
Eval and sandbox adapters use their one-invocation outcome vocabulary;
the covering authority has already been stored by another approval."
  (pcase kind
    ((or 'generic 'bash)
     ;; The pipeline's wrapper at niwa-pipeline.el handles
     ;; `'allow' / `'deny' directly; Bash's adapter does too.
     resolved)
    ((or 'eval 'sandbox)
     (pcase resolved
       ('allow 'allow-once)
       ('deny 'deny-once)
       (_ resolved)))
    (_ resolved)))

(defun niwa-permission-queue--coalesce (_rule-outcome &optional session)
  "Re-evaluate SESSION's queued entries against newly stored authority.
Entries that resolve to a non-`ask' outcome via
`niwa-check-permission' fire their callbacks with that outcome
translated for their kind and are removed from the queue.  Entries that
still resolve to `ask' stay in place.

Protected-path and deny precedence is handled inside
`niwa-check-permission': a protected resource needs a covering grant,
while deny rules remain final."
  (let (settled)
    (dolist (entry (niwa-permission-queue--get session))
      (let ((resolved (niwa-permission-queue--reevaluate entry)))
        (unless (eq resolved 'ask)
          (push
           (list entry
                 (niwa-permission-queue--translate-coalesce-outcome
                  (plist-get entry :kind) resolved))
           settled))))
    ;; Reevaluation can yield: expiry, a sibling's answer or a new admission
    ;; may have changed the queue.  Settle only entries still queued, and
    ;; keep the live queue's other entries, in order.
    (let ((live (niwa-permission-queue--get session)))
      (setq settled (cl-remove-if-not (lambda (item) (memq (car item) live))
                                      settled))
      ;; Remove every resolved entry before callbacks can reenter queue teardown.
      (niwa-permission-queue--set
       (cl-remove-if (lambda (entry) (assq entry settled)) live) session))
    (dolist (item (nreverse settled))
      (pcase-let ((`(,entry ,outcome) item))
        (niwa-permission-queue--safe-settle
         entry outcome 'coalesced)))))

(defun niwa-permission-queue--reevaluate (entry)
  "Re-evaluate ENTRY with current policy and its captured invocation facts.
Return `allow', `deny', or `ask'.  Execution's policy owner checks operation
and complete capability authority together.  Hooks and one-shot restrictions
cannot be cleared by a remembered sibling approval."
  (condition-case _err
      (with-current-buffer (if (buffer-live-p (plist-get entry :data-buffer))
                               (plist-get entry :data-buffer)
                             (current-buffer))
        (let* ((session (plist-get entry :session))
               (niwa--session session)
               (niwa--current-request (plist-get entry :request))
               (niwa--agent-invocation (plist-get entry :invocation))
               (kind (plist-get entry :kind))
               (tool-name (or (plist-get entry :tool-name)
                              (pcase kind ('bash "Bash") ('eval "Eval"))))
               (spec-key (if (eq kind 'generic)
                             (or (plist-get entry :specifier-key) :path)
                           :pattern))
               (spec-value (if (eq kind 'generic)
                               (plist-get entry :specifier-value)
                             (or (plist-get entry :command)
                                 (plist-get entry :expression)
                                 (plist-get entry :detail))))
               (context
                (niwa-permission--invocation-context
                 :tool-name tool-name :session session
                 :workspace (and session (niwa-session-workspace session))
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
                   ((member (niwa-permission-policy-tool-name tool-name)
                            '("Bash" "Eval"))
                    (niwa-tool-exec-permission-reevaluate entry context))
                   ((eq kind 'generic)
                    (apply #'niwa-check-permission tool-name
                           (niwa-permission--checker-args context)))
                   (t 'ask))))
            (if (and (eq resolved 'allow)
                     (not (eq (plist-get context :mode) 'full-auto))
                     (or (memq (plist-get entry :permission-via)
                               '(pre-tool-hook permission-request-hook))
                         (plist-get entry :once-only)))
                'ask
              resolved))))
    (error 'ask)))

(defun niwa-permission-queue-abort-all (&optional session)
  "Flush SESSION's queue, firing `'aborted' on every entry's callback.
Called from `niwa-abort' / request-cancel-fn."
  (let* ((session (or session (niwa-queue--current-session)))
         (queue (and session (niwa-permission-queue--get session))))
    (when session
      (when (fboundp 'niwa-permission-review-cancel)
        (niwa-permission-review-cancel session))
      (niwa-permission-queue--set nil session))
    (dolist (entry queue)
      (niwa-permission-queue--safe-settle entry 'aborted 'abort))))

(defun niwa-permission-queue-sweep-request
    (request-id &optional session no-render)
  "Abort queued entries for REQUEST-ID in SESSION.

When NO-RENDER is non-nil, do not render the next head entry after
sweeping."
  (when request-id
    (let* ((session (or session (niwa-queue--current-session)))
           (queue (and session (niwa-permission-queue--get session)))
           (head-before (car queue))
           kept swept)
      (when (fboundp 'niwa-permission-review-cancel)
        (niwa-permission-review-cancel session request-id))
      (dolist (entry queue)
        (if (equal (plist-get entry :request-id) request-id)
            (push entry swept)
          (push entry kept)))
      (when session
        (setq kept (nreverse kept))
        (niwa-permission-queue--set kept session)
        (dolist (entry (nreverse swept))
          (niwa-permission-queue--safe-settle
           entry 'aborted 'sweep))
        (when (and kept
                   (not no-render)
                   (not (eq head-before (car kept))))
          (niwa-permission-queue--render-head session))))))

(provide 'niwa-permission-queue)

;;; niwa-permission-queue.el ends here
