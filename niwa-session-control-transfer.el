;;; niwa-session-control-transfer.el -- Transfer coordination -*- lexical-binding: t; -*-

;;; Commentary:

;; Coordinates cooperative portable-session control transfer.  Persistence
;; calls this coordinator for polling, admission, and committed-state
;; adoption.  The transfer module owns the durable request/lease protocol;
;; views register transient drain predicates and render semantic descriptors.

;;; Code:

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-turn-p
                  "niwa-agent-control" (session))
(declare-function niwa-agent-control-interrupt-user
                  "niwa-agent-control" (session path))
(autoload 'niwa-agent-control-active-turn-p "niwa-agent-control")
(autoload 'niwa-agent-control-interrupt-user "niwa-agent-control")

;; `niwa-engine'
(declare-function niwa-engine-assert-local-history "niwa-engine"
                  (session operation &optional backend root-only))
(autoload 'niwa-engine-assert-local-history "niwa-engine")

;; `niwa-execution'
(declare-function niwa-execution-session-live-p
                  "niwa-execution" (session))
(declare-function niwa-execution-stop-services "niwa-execution" (session))
(declare-function niwa-execution-teardown-session "niwa-execution" (session))
(declare-function niwa-execution-unsettled-mutation-p
                  "niwa-execution" (session))
(autoload 'niwa-execution-session-live-p "niwa-execution")
(autoload 'niwa-execution-stop-services "niwa-execution")
(autoload 'niwa-execution-teardown-session "niwa-execution")
(autoload 'niwa-execution-unsettled-mutation-p "niwa-execution")

;; `niwa-instruction-registry'
(declare-function niwa--instruction-state-rollback
                  "niwa-instruction-registry" (workspace))
(autoload 'niwa--instruction-state-rollback
  "niwa-instruction-registry")

;; `niwa-permission-queue'
(declare-function niwa-permission-queue-abort-all
                  "niwa-permission-queue" (&optional session))
(autoload 'niwa-permission-queue-abort-all "niwa-permission-queue")

;; `niwa-plan-mode'
(declare-function niwa-plan-approval-abort
                  "niwa-plan-mode" (&optional session outcome))
(autoload 'niwa-plan-approval-abort "niwa-plan-mode")

;; `niwa-schedule'
(declare-function niwa-schedule-restore-session "niwa-schedule" (session buffer))
(autoload 'niwa-schedule-restore-session "niwa-schedule")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-check-target-incarnation
                  "niwa-session-artifacts" (session buffer))
(declare-function niwa-session-artifacts-load-instructions
                  "niwa-session-artifacts"
                  (session))
(declare-function niwa-session-artifacts-read-artifact
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-replace-transcript-contents
                  "niwa-session-artifacts" (source))
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force))
(declare-function niwa-session-artifacts-segment-path
                  "niwa-session-artifacts" (save-path n))
(autoload 'niwa-session-artifacts-check-target-incarnation
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-load-instructions
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-artifact
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-replace-transcript-contents
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-segment-path
  "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-deserialize
                  "niwa-session-codec" (plist workspace))
(autoload 'niwa-session-codec-deserialize "niwa-session-codec")

;; `niwa-session-durability'
(declare-function niwa-session-durability--cancel-renewal
                  "niwa-session-durability" (session))
(declare-function niwa-session-durability--gate
                  "niwa-session-durability" (session token &optional reentrant))
(declare-function niwa-session-durability-call-outside-transaction
                  "niwa-session-durability" (function))
(declare-function niwa-session-durability-holder-label
                  "niwa-session-durability" (label))
(declare-function niwa-session-durability-lease-acquire
                  "niwa-session-durability"
                  (session-dir buffer-name &optional session token force))
(declare-function niwa-session-durability-lease-release
                  "niwa-session-durability"
                  (session-dir &optional session))
(declare-function niwa-session-durability-publication-head
                  "niwa-session-durability" (session-dir &optional names))
(defvar niwa-session-durability--heartbeat-inhibit)
(autoload 'niwa-session-durability--gate "niwa-session-durability")
(autoload 'niwa-session-durability-call-outside-transaction "niwa-session-durability")
(autoload 'niwa-session-durability-holder-label "niwa-session-durability")
(autoload 'niwa-session-durability-lease-acquire
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-release
  "niwa-session-durability")
(autoload 'niwa-session-durability-publication-head
  "niwa-session-durability")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-apply-read-only-mode
                  "niwa-session-persistence" (buf &optional reason))
(declare-function niwa-session-persistence-load-sidecar
                  "niwa-session-persistence" (path &optional scope))
(defvar niwa-session--checkpoint-pending)
(autoload 'niwa-session-persistence-apply-read-only-mode
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-load-sidecar
  "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication-busy-p
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-clear-transient
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-finishing-p
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-read
                  "niwa-session-publication"
                  (session-dir &optional head names))
(declare-function niwa-session-publication-settle-taken-over
                  "niwa-session-publication" (session))
(autoload 'niwa-session-publication-busy-p "niwa-session-publication")
(autoload 'niwa-session-publication-clear-transient "niwa-session-publication")
(autoload 'niwa-session-publication-finishing-p "niwa-session-publication")
(autoload 'niwa-session-publication-read "niwa-session-publication")
(autoload 'niwa-session-publication-settle-taken-over "niwa-session-publication")

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-copy-session-state
                  "niwa-session-rewind" (from to))
(autoload 'niwa-session-rewind-copy-session-state
  "niwa-session-rewind")

;; `niwa-session-transfer'
(declare-function niwa-session-transfer-decide
                  "niwa-session-transfer" (session decision))
(declare-function niwa-session-transfer-observe-decision
                  "niwa-session-transfer" (session request))
(declare-function niwa-session-transfer-poll
                  "niwa-session-transfer" (session &optional admission))
(declare-function niwa-session-transfer-release
                  "niwa-session-transfer" (session))
(declare-function niwa-session-transfer-request
                  "niwa-session-transfer" (session &optional label))
(autoload 'niwa-session-transfer-decide "niwa-session-transfer")
(autoload 'niwa-session-transfer-observe-decision
  "niwa-session-transfer")
(autoload 'niwa-session-transfer-poll "niwa-session-transfer")
(autoload 'niwa-session-transfer-release "niwa-session-transfer")
(autoload 'niwa-session-transfer-request "niwa-session-transfer")

;; `niwa-structs'
(declare-function niwa-session-adopt-committed-state
                  "niwa-structs"
                  (session workspace save-path lease ownership-epoch lease-renewal-timer
                           publication control-transfer control-transfer-drains
                           root-buffer))
(declare-function niwa-session-agent-registry "niwa-structs" (cl-x) t)
(declare-function niwa-session-control-transfer
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-control-transfer-drains
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-current-segment
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-lease
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-lease-renewal-timer
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-p
                  "niwa-structs" (cl-x))
(declare-function niwa-session-pending-input-p
                  "niwa-structs" (session))
(declare-function niwa-session-pending-plan-approval
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-queue
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication-active-p
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-root-buffer
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-set-control-transfer
                  "niwa-structs" (session transfer))
(declare-function niwa-session-set-control-transfer-drains
                  "niwa-structs" (session predicates))
(declare-function niwa-session-set-execution-target
                  "niwa-structs" (session target))
(declare-function niwa-session-set-root-buffer
                  "niwa-structs" (session buffer))
(declare-function niwa-session-workspace
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa-session--read-only-mode)

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-gptel-state
                  "niwa-transcript-restore" ())
(autoload 'niwa-transcript-restore-gptel-state
  "niwa-transcript-restore")

;; `niwa-transport'
(declare-function niwa-transport-busy-p
                  "niwa-transport" (&optional path))
(autoload 'niwa-transport-busy-p "niwa-transport")

;; `niwa-turn'
(declare-function niwa-request-active-p
                  "niwa-turn" (&optional buffer))
(declare-function niwa-request-cancel "niwa-turn"
                  (request &optional abort-plan-approval))
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-request-active-p "niwa-turn")
(autoload 'niwa-request-cancel "niwa-turn")
(autoload 'niwa-turn-busy-p "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--transcript-org-mode "niwa-utilities" ())
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(autoload 'niwa--transcript-org-mode "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")

(defcustom niwa-session-follow-published t
  "Whether a non-owner session buffer follows the owner's committed state.

A joined client and a host that has handed control away are the same thing:
a buffer whose session is being written somewhere else.  Following re-reads
each new committed publication so the buffer shows the owner's work instead
of the snapshot it opened with.  Updates arrive one publication at a time,
which is turn-granular, not streaming.

Each poll costs one lease observation, and each advance costs the committed
sidecar and segment.  Set to nil to hold a non-owner buffer at the state it
opened with; the value is read per buffer, so a single session can opt out
with `niwa-toggle-follow'."
  :type 'boolean
  :group 'niwa)

(defvar niwa-session-control-transfer--observers
  (make-hash-table :test #'eq :weakness 'key)
  "Session-owned UI observers keyed by live session objects.")

(defvar niwa-session-control-transfer--roots
  (make-hash-table :test #'equal)
  "Registered root buffers keyed by durable session ids.")

(defun niwa-session-control-transfer-register-observer
    (session observer)
  "Register OBSERVER for semantic SESSION lifecycle events.

OBSERVER receives EVENT followed by its arguments.  Return OBSERVER as the
unregister token.  Observers are transient and are never persisted."
  (unless (functionp observer)
    (error "Session observer must be callable"))
  (puthash session
           (cons observer
                 (delq observer
                       (gethash session
                                niwa-session-control-transfer--observers)))
           niwa-session-control-transfer--observers)
  observer)

(defun niwa-session-control-transfer-unregister-observer
    (session observer)
  "Unregister OBSERVER from SESSION's semantic event stream."
  (when session
    (let ((observers
           (delq observer
                 (gethash session
                          niwa-session-control-transfer--observers))))
      (if observers
          (puthash session observers
                   niwa-session-control-transfer--observers)
        (remhash session niwa-session-control-transfer--observers))))
  nil)

(defun niwa-session-control-transfer-notify
    (session event &rest args)
  "Notify SESSION observers of semantic EVENT and ARGS.

Observer failures are diagnostic-only: durable state transitions must not be
rolled back because a view was closed during a redraw."
  (dolist (observer
           (copy-sequence
            (gethash session niwa-session-control-transfer--observers)))
    (condition-case err
        (apply observer event args)
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope session) "Session observer failed for %s: %s"
                event (error-message-string err)))))
  nil)

(defun niwa-session-control-transfer-register-drain
    (session predicate)
  "Register PREDICATE as transient drain work for SESSION.

PREDICATE must return non-nil while its owner still has work that prevents a
lease handoff.  Return PREDICATE as the unregister token."
  (unless (functionp predicate)
    (error "Control-transfer drain must be callable"))
  (niwa-session-set-control-transfer-drains
   session
   (cons predicate
         (delq predicate
               (niwa-session-control-transfer-drains session))))
  predicate)

(defun niwa-session-control-transfer-unregister-drain
    (session predicate)
  "Remove PREDICATE from SESSION's transient drain registry."
  (when session
    (niwa-session-set-control-transfer-drains
     session
     (delq predicate (niwa-session-control-transfer-drains session))))
  nil)

(defun niwa-session-control-transfer-register-root-buffer
    (session buffer)
  "Register BUFFER as SESSION's authoritative root data buffer.

The registration is transient and is cleared when the owning data buffer dies.
Return BUFFER so lifecycle hooks can use this as their value."
  (unless (buffer-live-p buffer)
    (error "Control-transfer root buffer must be live"))
  (maphash
   (lambda (id registered)
     (when (eq registered buffer)
       (remhash id niwa-session-control-transfer--roots)))
   niwa-session-control-transfer--roots)
  (niwa-session-set-root-buffer session buffer)
  (when-let* ((id (niwa-session-session-id session)))
    (puthash id buffer niwa-session-control-transfer--roots))
  buffer)

(defun niwa-session-control-transfer-unregister-root-buffer
    (session buffer)
  "Clear SESSION's root registration when it names BUFFER."
  (when (and session (eq buffer (niwa-session-root-buffer session)))
    (niwa-session-set-root-buffer session nil)
    (when-let* ((id (niwa-session-session-id session)))
      (when (eq buffer (gethash id niwa-session-control-transfer--roots))
        (remhash id niwa-session-control-transfer--roots))))
  nil)

(defun niwa-session-control-transfer-root-buffer (session)
  "Return SESSION's registered live root data buffer, or nil."
  (let ((buffer (niwa-session-root-buffer session)))
    (when (buffer-live-p buffer)
      buffer)))

(defun niwa-session-control-transfer-root-buffer-for-id (session-id)
  "Return the registered live root buffer for SESSION-ID, or nil."
  (when-let* ((buffer (gethash session-id
                             niwa-session-control-transfer--roots)))
    (if (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (or (not (boundp 'niwa--session))
                   (null niwa--session)
                   (and (niwa-session-p niwa--session)
                        (equal session-id
                               (niwa-session-session-id
                                niwa--session))))))
        buffer
      (remhash session-id niwa-session-control-transfer--roots)
      nil)))

(defun niwa-session-control-transfer-drained-p (session)
  "Return non-nil when SESSION has no work that blocks lease handoff.

Admission and its user-visible explanation share the same blocker policy."
  (null (niwa-session-control-transfer-drain-blocker session)))

(defun niwa-session-control-transfer-drain-blocker (session)
  "Return a short phrase naming what blocks SESSION's handoff, or nil.

Only the owner can answer this: a requester sees `quiescing' and nothing
about why.  The first blocker is enough -- the user wants to know whether the
wait is theirs to end, not an inventory.  A failed transient drain predicate
is conservatively treated as still draining."
  (cond
   ((niwa-request-active-p
     (niwa-session-control-transfer-root-buffer session))
    "a running request")
   ((niwa-turn-busy-p
     (niwa-session-control-transfer-root-buffer session))
    "a settling turn")
   ((niwa-execution-session-live-p session) "a live execution")
   ((niwa-session-permission-queue session) "a permission prompt")
   ((niwa-session-pending-plan-approval session) "a plan approval")
   ((niwa-session-pending-input-p session) "queued input")
   ((niwa-agent-control-active-turn-p session) "an agent turn")
   ((niwa-execution-unsettled-mutation-p session) "an unsettled mutation")
   ((or (niwa-session-pending-publication session)
        (niwa-session-publication-active-p session))
    "a publication")
   ((seq-some (lambda (predicate)
                (condition-case nil
                    (funcall predicate)
                  (error t)))
              (niwa-session-control-transfer-drains session))
    "the view")))

(defun niwa-session-control-transfer-observe (session &optional admission)
  "Refresh SESSION's durable transfer state without settling it.

Admission checks use this operation so observing a grant never saves or
releases the owner's session.  ADMISSION combines the recovery observations
with the transfer read inside the caller's durable transaction."
  (let ((state (niwa-session-transfer-poll session admission)))
    (niwa-session-set-control-transfer
     session (or state (niwa-session-control-transfer session)))
    (niwa-session-control-transfer session)))

(defun niwa-session-control-transfer-taken-over (session lease)
  "Stop SESSION's unpublished work after another client activated LEASE.
Storage is already fenced.  An active publication must finish before agent
interrupts can run; the existing poll retries this transition, not a queued
interrupt that publication cleanup could discard."
  (when-let* ((buffer (niwa-session-control-transfer-root-buffer session))
              ((not (eq 'owned (plist-get (niwa-session-lease session) :state)))))
    (unless (buffer-local-value 'niwa-session--read-only-mode buffer)
      (niwa-session-set-control-transfer session (list :state 'taken-over))
      (niwa-session-persistence-apply-read-only-mode
       buffer
       (format "control taken over by %s at %s; unpublished changes here were not kept"
               (niwa-session-durability-holder-label
                (plist-get lease :host))
               (format-time-string "%FT%T%z"
                                   (seconds-to-time (plist-get lease :renewed-at))))))
    (when (and (eq 'taken-over
                   (plist-get (niwa-session-control-transfer session) :state))
               (not (niwa-session-publication-active-p session))
               (not (niwa-session-publication-busy-p session))
               (not (niwa-session-publication-finishing-p session))
               (not (niwa-transport-busy-p (niwa-session-save-path session))))
      (niwa-session-durability-call-outside-transaction
       (lambda ()
         (let ((stopped t))
           ;; A failed canceller must not leave the other producers running.
           (dolist (stop
                    (append
                     (list (lambda ()
                             (with-current-buffer buffer
                               (niwa-request-cancel niwa--current-request t)))
                           (lambda ()
                             (niwa-permission-queue-abort-all session)
                             (niwa-plan-approval-abort session))
                           (lambda ()
                             (let ((inhibit-message t))
                               (while (cl-find-if
                                       (lambda (entry)
                                         (eq buffer (plist-get
                                                     (gptel-fsm-info (cadr entry))
                                                     :buffer)))
                                       gptel--request-alist)
                                 (gptel-abort buffer)))))
                     (mapcar (lambda (entry)
                               (lambda ()
                                 (if (or (niwa-session-publication-active-p session)
                                         (niwa-session-publication-busy-p session)
                                         (niwa-session-publication-finishing-p session))
                                     (setq stopped nil)
                                   (niwa-agent-control-interrupt-user session (car entry)))))
                             (niwa-session-agent-registry session))
                     (list (lambda () (niwa-execution-teardown-session session)))))
             (condition-case err
                 (funcall stop)
               (error
                (setq stopped nil)
                (niwa-diagnostic-message
                 (niwa-diagnostic-scope session)
                 "Could not stop work after control loss: %s" (error-message-string err)))))
           (when stopped
             (niwa-session-publication-clear-transient session)
             (with-current-buffer buffer
               (setq niwa--current-request nil
                     niwa-session--checkpoint-pending nil)
               (set-buffer-modified-p nil))
             (unless (eq 'owned (plist-get (niwa-session-lease session) :state))
               (niwa-session-durability--cancel-renewal session))
             (niwa-session-set-control-transfer session (list :state 'following)))
           (niwa-session-control-transfer-notify session 'rerender)))))))

(defun niwa-session-control-transfer--poll-owner (session)
  "Poll owner-side state, save drained work, and release the granted lease."
  (let ((state (niwa-session-transfer-poll session)))
    (when (and (eq (plist-get state :state) 'quiescing)
               (niwa-session-control-transfer-drained-p session))
      (let ((buffer (niwa-session-control-transfer-root-buffer session)))
        (when (buffer-live-p buffer)
          (niwa-session-artifacts-save session buffer t)
          (niwa-execution-stop-services session)
          (niwa-session-transfer-release session)
          (niwa-session-persistence-apply-read-only-mode
           buffer
           (format "control transferred to %s; this session is read-only here"
                   (or (plist-get (plist-get state :request)
                                  :requester-label)
                       "another client"))))))
    (niwa-session-control-transfer session)))

(defun niwa-session-control-transfer--insert-committed-segment
    (session buffer)
  "Replace BUFFER with SESSION's current committed segment bytes.

The buffer is left unmodified and read-only-neutral: the caller decides
whether the result is a writable owner buffer or a read-only follower.  The
segment number comes from the committed session state, so a rotation on the
owner's side repoints the visited file here too."
  (let* ((save-path (niwa-session-save-path session))
         (logical
          (file-name-nondirectory
           (niwa-session-artifacts-segment-path
            save-path (niwa-session-current-segment session))))
         (segment-path (expand-file-name logical save-path))
         (content
          (niwa-session-artifacts-read-artifact session logical t)))
    (with-current-buffer buffer
      ;; The staging buffer is a transcript, not scratch text: the segment's
      ;; hidden records and persisted gptel bounds are only parseable in the
      ;; transcript's own major mode, and `gptel-mode' refuses any other.
      (unless (derived-mode-p 'org-mode)
        (niwa--transcript-org-mode))
      (setq buffer-file-name segment-path
            buffer-file-truename nil
            default-directory (niwa-session-working-directory session))
      (set-visited-file-modtime)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (decode-coding-string
          content (or buffer-file-coding-system 'utf-8-unix))))
      (niwa-transcript-restore-gptel-state)
      (set-buffer-modified-p nil)
      (set-visited-file-modtime))))

(defun niwa-session-control-transfer--stage-session
    (session refreshed publication buffer)
  "Attach SESSION's live runtime owners to REFRESHED for staged adoption."
  (niwa-session-set-execution-target
   refreshed
   (when-let* ((target (niwa-session-execution-target session)))
     (copy-sequence target)))
  (niwa-session-adopt-committed-state
   refreshed
   (niwa-session-workspace session)
   (niwa-session-save-path session)
   (niwa-session-lease session)
   (niwa-session-ownership-epoch session)
   (niwa-session-lease-renewal-timer session)
   publication
   (niwa-session-control-transfer session)
   (niwa-session-control-transfer-drains session)
   buffer)
  refreshed)

(defun niwa-session-control-transfer--install-staged-segment
    (buffer staging-buffer)
  "Install STAGING-BUFFER into BUFFER, restoring BUFFER if a hook fails."
  (let ((original (generate-new-buffer " *niwa-transfer-original*"))
        (file-name (buffer-local-value 'buffer-file-name buffer))
        (file-truename (buffer-local-value 'buffer-file-truename buffer))
        (directory (buffer-local-value 'default-directory buffer))
        (coding (buffer-local-value 'buffer-file-coding-system buffer))
        (modified (with-current-buffer buffer (buffer-modified-p)))
        (modtime (with-current-buffer buffer (visited-file-modtime)))
        (point (with-current-buffer buffer (point)))
        (narrowing
         (with-current-buffer buffer
           (and (buffer-narrowed-p) (cons (point-min) (point-max))))))
    (unwind-protect
        (progn
          (with-current-buffer original
            (insert
             (with-current-buffer buffer
               (save-restriction
                 (widen)
                 (buffer-substring (point-min) (point-max))))))
          (condition-case err
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (let ((inhibit-read-only t))
                    (setq buffer-file-name
                          (buffer-local-value
                           'buffer-file-name staging-buffer)
                          buffer-file-truename nil
                          default-directory
                          (buffer-local-value
                           'default-directory staging-buffer)
                          buffer-file-coding-system
                          (buffer-local-value
                           'buffer-file-coding-system staging-buffer))
                    (niwa-session-artifacts-replace-transcript-contents
                     staging-buffer)
                    (set-buffer-modified-p nil)
                    (set-visited-file-modtime))))
            (error
             (with-current-buffer buffer
               (save-restriction
                 (widen)
                 (let ((inhibit-modification-hooks t)
                       (inhibit-read-only t))
                   (setq buffer-file-name file-name
                         buffer-file-truename file-truename
                         default-directory directory
                         buffer-file-coding-system coding)
                   (niwa-session-artifacts-replace-transcript-contents
                    original)
                   (set-buffer-modified-p modified)
                   (set-visited-file-modtime modtime)
                   (when narrowing
                     (narrow-to-region (car narrowing) (cdr narrowing)))
                   (goto-char (min point (point-max))))))
             (signal (car err) (cdr err)))))
      (when (buffer-live-p original)
        (with-current-buffer original
          (set-buffer-modified-p nil))
        (kill-buffer original)))))

(defun niwa-session-control-transfer--follow-published
    (session buffer &optional force)
  "Advance read-only BUFFER to SESSION's newest committed publication.

FORCE re-reads even when this buffer has following turned off, which is what
an explicit refresh command asks for.

Return non-nil when the buffer moved to a newer publication.  A non-owner
sees only what the owner has committed, so the buffer advances one whole
publication at a time and never mid-turn.  The publication head names the
generation cheaply, so an unchanged owner costs one lease observation and no
artifact reads at all.

A locally modified buffer is left alone.  Those edits are exactly what the
transfer path refuses to discard, and a follow tick must not resolve that
conflict on the user's behalf."
  (when (and (buffer-live-p buffer)
             (or force
                 (buffer-local-value 'niwa-session-follow-published buffer))
             (not (buffer-modified-p buffer))
             (niwa-session-save-path session))
    (let* ((save-path (niwa-session-save-path session))
           (head (niwa-session-durability-publication-head save-path)))
      (when (and head
                 (not (equal head
                             (plist-get (niwa-session-publication session)
                                        :head))))
        (let* ((workspace (niwa-session-workspace session))
               (publication (niwa-session-publication-read save-path))
               (sidecar
                (and publication
                     (niwa-session-persistence-load-sidecar
                      (plist-get publication :sidecar) (niwa-diagnostic-scope session))))
               (refreshed
                (and sidecar
                     (plist-get
                      (niwa-session-codec-deserialize
                       sidecar workspace)
                      :session)))
               (staging-buffer
                (and refreshed
                     (generate-new-buffer " *niwa-follow-staging*"))))
          (when refreshed
            (unwind-protect
                (progn
                  (niwa-session-control-transfer--stage-session
                   session refreshed publication staging-buffer)
                  (niwa-session-control-transfer--insert-committed-segment
                   refreshed staging-buffer)
                  (niwa-session-control-transfer--install-staged-segment
                   buffer staging-buffer)
                  (niwa-session-rewind-copy-session-state
                   refreshed session)
                  (niwa-session-set-root-buffer session buffer)
                  t)
              (when (buffer-live-p staging-buffer)
                (with-current-buffer staging-buffer
                  (set-buffer-modified-p nil))
                (kill-buffer staging-buffer)))))))))

(defun niwa-session-control-transfer--adopt-control
    (session buffer &optional direct-acquisition)
  "Adopt SESSION's committed state into BUFFER under a freshly held lease.

The caller has already acquired the lease.  Losing it again on any failure is
the point of the unwind: a client that cannot finish adopting committed state
must not keep other clients out.

The target incarnation, transcript, and instruction restore are staged before
the live session or buffer changes.  DIRECT-ACQUISITION resumes an unheld
session, including its native history, rather than a cooperative transfer."
  ;; Staging copies runtime slots before yielding instruction work.  A heartbeat
  ;; started there would be erased by the later whole-session copy, hiding a
  ;; running target program from the mutation gate.
  (niwa-session-durability--gate session nil)
  (condition-case err
      (let* ((niwa-session-durability--heartbeat-inhibit t)
             (save-path (niwa-session-save-path session))
             (workspace (niwa-session-workspace session))
             (transfer (niwa-session-control-transfer session))
             (publication
              (or (niwa-session-publication-read save-path)
                  (error "Transferred session has no committed publication")))
             (sidecar
              (niwa-session-persistence-load-sidecar
               (plist-get publication :sidecar) (niwa-diagnostic-scope session)))
             (refreshed
              (plist-get
               (niwa-session-codec-deserialize sidecar workspace)
               :session))
             (staging-buffer
              (generate-new-buffer " *niwa-transfer-staging*"))
             (instruction-rollback
              (niwa--instruction-state-rollback workspace)))
        (unless refreshed
          (error "Transferred session has no valid committed sidecar"))
        (unwind-protect
            (progn
              (unless direct-acquisition
                (niwa-engine-assert-local-history refreshed "Session control transfer"))
              (niwa-session-control-transfer--stage-session
               session refreshed publication staging-buffer)
              (niwa-session-control-transfer--insert-committed-segment
               refreshed staging-buffer)
              (niwa-session-control-transfer-register-root-buffer
               refreshed staging-buffer)
              (niwa-session-artifacts-check-target-incarnation
               refreshed staging-buffer)
              (condition-case install-error
                  (progn
                    (unless
                        (niwa-session-artifacts-load-instructions refreshed)
                      (error "Transferred workspace instructions could not be restored"))
                    (niwa-session-control-transfer--install-staged-segment
                     buffer staging-buffer))
                (error
                 (funcall instruction-rollback)
                 (signal (car install-error) (cdr install-error))))
              (niwa-session-rewind-copy-session-state
               refreshed session)
              (niwa-session-control-transfer-register-root-buffer
               session buffer)
              (with-current-buffer buffer
                (setq buffer-read-only nil
                      niwa-session--read-only-mode nil))
              (niwa-schedule-restore-session session buffer)
              (niwa-session-set-control-transfer
               session
               (list :state 'acquired :request (plist-get transfer :request)))
              (message "niwa: control acquired; session is writable")
              t)
          (when (buffer-live-p staging-buffer)
            (niwa-session-control-transfer-register-root-buffer
             session buffer)
            (with-current-buffer staging-buffer
              (set-buffer-modified-p nil))
            (kill-buffer staging-buffer))))
    (error
     (niwa-session-durability-lease-release
      (niwa-session-save-path session) session)
     (signal (car err) (cdr err)))))

(defun niwa-session-control-transfer--poll-requester (session buffer)
  "Acquire SESSION into BUFFER after its committed release fence is visible."
  (when (and (buffer-live-p buffer)
             (niwa-session-save-path session)
             (memq (plist-get (niwa-session-control-transfer session) :state)
                   '(requested quiescing)))
    (niwa-engine-assert-local-history session "Session control transfer")
    (niwa-session-control-transfer-register-root-buffer session buffer)
    (when (buffer-modified-p buffer)
      (user-error "Read-only session changed locally; refresh before transfer"))
    (let* ((transfer (niwa-session-control-transfer session))
           (request (plist-get transfer :request))
           (decision
            (or (plist-get transfer :decision)
                (niwa-session-transfer-observe-decision
                 session request))))
      (cond
       ((eq 'reject (plist-get decision :decision))
        (niwa-session-set-control-transfer
         session (list :state 'rejected :request request :decision decision))
        t)
       (t
        (when decision
          (niwa-session-set-control-transfer
           session
           (list :state 'quiescing :request request :decision decision)))
        (when (niwa-session-durability-lease-acquire
               (niwa-session-save-path session)
               (buffer-name buffer)
               session)
          (niwa-session-control-transfer--adopt-control
           session buffer)))))))

(defun niwa-session-control-transfer-acquire (session buffer &optional force)
  "Take SESSION's unheld lease directly into BUFFER.

For a lease nobody holds there is no owner to request control from, and for
an expired one the lease layer's own takeover confirmation is the whole
negotiation.  Both reduce to acquiring and adopting committed state, which is
also what a granted transfer ends with.  Native history is preserved just as
on ordinary resume; the engine validates its installation on the next send.
FORCE also offers confirmation to take over from a live holder."
  (unless (buffer-live-p buffer)
    (error "Session buffer is not live"))
  (unless (niwa-session-save-path session)
    (user-error "Session has not been materialized yet"))
  (when (eq 'taken-over
            (plist-get (niwa-session-control-transfer session) :state))
    (niwa-session-control-transfer-taken-over session (niwa-session-lease session))
    (when (eq 'taken-over
              (plist-get (niwa-session-control-transfer session) :state))
      (user-error "Still stopping this session's previous work; try again in a moment")))
  (when (buffer-modified-p buffer)
    (user-error "Read-only session changed locally; refresh before taking control"))
  (niwa-session-control-transfer-register-root-buffer session buffer)
  (if (niwa-session-durability-lease-acquire
       (niwa-session-save-path session) (buffer-name buffer) session nil force)
      (niwa-session-control-transfer--adopt-control session buffer t)
    (user-error "Another client took this session's lease first")))

(defun niwa-session-control-transfer-take-over (session buffer)
  "Confirm forced takeover of SESSION into read-only BUFFER."
  (niwa-session-control-transfer-acquire session buffer t))

(defun niwa-session-control-transfer-poll (session buffer read-only-p)
  "Poll SESSION's durable transfer state for BUFFER.

READ-ONLY-P selects requester admission; an owning session drains and settles
its committed transfer.  Return non-nil when a non-owner buffer changed --
either by acquiring control or by advancing to a newer publication -- so the
caller knows to redraw.

Polling runs from a timer, and a timer fires wherever the main loop happens to
be waiting, including inside a TRAMP operation.  Target I/O from there is a
reentrant TRAMP call that can wedge the operation already in progress, and the
owner poll may itself publish.  The poll therefore performs no target I/O
while another TRAMP operation is in progress, or while a publication owns the
bounded window; the next tick observes the same durable state once the
transport is free."
  (unless (niwa-transport-busy-p (niwa-session-save-path session))
    (niwa-session-publication-settle-taken-over session)
    (when (eq 'taken-over
              (plist-get (niwa-session-control-transfer session) :state))
      (niwa-session-control-transfer-taken-over
       session (niwa-session-lease session))))
  (unless (or (niwa-session-publication-active-p session)
              (eq 'taken-over
                  (plist-get (niwa-session-control-transfer session) :state))
              (niwa-transport-busy-p (niwa-session-save-path session)))
    (if read-only-p
        (or (niwa-session-control-transfer--poll-requester session buffer)
            (niwa-session-control-transfer--follow-published
             session buffer))
      (niwa-session-control-transfer--poll-owner session))))

(defun niwa-session-control-transfer-request (session)
  "Record a control request for SESSION's current owner."
  (let ((request (niwa-session-transfer-request session)))
    (when request
      (niwa-session-set-control-transfer
       session (list :state 'requested :request request)))
    request))

(defun niwa-session-control-transfer-decide (session decision)
  "Record DECISION for SESSION's current transfer request."
  (niwa-session-transfer-decide session decision))

(defun niwa-session-control-transfer-descriptor (session read-only-p)
  "Return the semantic interaction descriptor for SESSION.

The descriptor carries no faces, overlays, or keymaps: `:title', `:detail',
and `:keys' say what is true and what the user may do, and the view decides
how prominent that is.  `:attention' marks the states that are waiting on a
person rather than on the protocol.

READ-ONLY-P selects the side.  Owner and requester share one durable state
and need opposite presentations of it: only the owner can decide, and only
the owner can see what a handoff is still waiting for."
  (let* ((transfer (niwa-session-control-transfer session))
         (state (plist-get transfer :state))
         (request (plist-get transfer :request))
         (label (or (plist-get request :requester-label) "another client")))
    (cond
     ((and (not read-only-p) (eq state 'requested))
      (list :kind 'control-transfer
            :action 'grant
            :attention t
            :title (format "%s is asking for control of this session" label)
            :detail "Granted automatically if you do not answer."
            :keys '(("g" . "grant now") ("k" . "keep control"))
            :help-echo "Grant or keep the current lease"))
     ((and (not read-only-p) (eq state 'quiescing))
      (list :kind 'control-transfer
            :action 'status
            :title (format "Handing control to %s" label)
            :detail (if-let* ((blocker
                              (niwa-session-control-transfer-drain-blocker
                               session)))
                        (format "Finishing %s first." blocker)
                      "Publishing the final state.")
            :help-echo "Control moves once this session has drained"))
     ((not read-only-p) nil)
     ((eq state 'requested)
      (list :kind 'control-transfer
            :action 'status
            :title "Control requested"
            :detail "The owner grants automatically if it does not answer."
            :keys '(("b" . "take over"))
            :help-echo "Waiting for the owner to answer or time out"))
     ((eq state 'quiescing)
      (list :kind 'control-transfer
            :action 'status
            :title "Control granted"
            :detail "Waiting for the owner to finish its current work."
            :keys '(("b" . "take over"))
            :help-echo "Control arrives once the owner has drained its work"))
     ((eq state 'rejected)
      (list :kind 'control-transfer
            :action 'request
            :attention t
            :title "Control request was declined"
            :keys '(("r" . "take control") ("b" . "take over"))
            :help-echo "Acquire an available lease or request cooperative control"))
     (t
      (list :kind 'control-transfer
            :action 'request
            :title "This session is read-only here"
            :detail "Take control to acquire its lease or ask its current owner."
            :keys '(("r" . "take control") ("b" . "take over"))
            :help-echo "Acquire an available lease or request cooperative control")))))

(provide 'niwa-session-control-transfer)

;;; niwa-session-control-transfer.el ends here
