;;; mevedel-session-control-transfer.el --- transfer coordination -*- lexical-binding: t; -*-

;;; Commentary:

;; Coordinates cooperative portable-session control transfer.  Persistence
;; calls this coordinator for polling, admission, and committed-state
;; adoption.  The transfer module owns the durable request/lease protocol;
;; views register transient drain predicates and render semantic descriptors.

;;; Code:

;; `mevedel-structs'
(declare-function mevedel-request-active-p
                  "mevedel-structs" (&optional buffer))
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-control-transfer-drains
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-execution-target
                  "mevedel-structs" (session target))
(declare-function mevedel-session-lease
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease-renewal-timer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-p
                  "mevedel-structs" (session))
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-active-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p
                  "mevedel-structs" (cl-x))
(declare-function mevedel-session-set-control-transfer-drains
                  "mevedel-structs" (session predicates))
(declare-function mevedel-session-set-control-transfer
                  "mevedel-structs" (session transfer))
(declare-function mevedel-session-root-buffer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-adopt-committed-state
                  "mevedel-structs"
                  (session workspace save-path lease lease-renewal-timer
                           publication control-transfer root-buffer))
(declare-function mevedel-session-set-root-buffer
                  "mevedel-structs" (session buffer))
(declare-function mevedel-session-save-path
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel-session--read-only-mode)

(defcustom mevedel-session-follow-published t
  "Whether a non-owner session buffer follows the owner's committed state.

A joined client and a host that has handed control away are the same thing:
a buffer whose session is being written somewhere else.  Following re-reads
each new committed publication so the buffer shows the owner's work instead
of the snapshot it opened with.  Updates arrive one publication at a time,
which is turn-granular, not streaming.

Each poll costs one lease observation, and each advance costs the committed
sidecar and segment.  Set to nil to hold a non-owner buffer at the state it
opened with; the value is read per buffer, so a single session can opt out
with `mevedel-toggle-follow'."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel-session-control-transfer--observers
  (make-hash-table :test #'eq :weakness 'key)
  "Session-owned UI observers keyed by live session objects.")

(defvar mevedel-session-control-transfer--roots
  (make-hash-table :test #'equal)
  "Registered root buffers keyed by durable session ids.")

(defvar mevedel-session-control-transfer--presentations
  (make-hash-table :test #'eq :weakness 'key)
  "Registered transient presentation buffers keyed by session.")

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p
                  "mevedel-agent-control" (session))

;; `mevedel-execution'
(declare-function mevedel-execution-session-live-p
                  "mevedel-execution" (session))
(declare-function mevedel-execution-unsettled-mutation-p
                  "mevedel-execution" (session))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--apply-read-only-mode
                  "mevedel-session-persistence" (buffer &optional reason))
(declare-function mevedel-session-persistence--check-target-incarnation
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--copy-session-state
                  "mevedel-session-persistence" (from to))
(declare-function mevedel-session-persistence--load-instructions
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--segment-path
                  "mevedel-session-persistence" (save-path segment))
(declare-function mevedel-session-persistence-read-artifact
                  "mevedel-session-persistence" (session logical &optional required))
(declare-function mevedel-session-persistence-deserialize
                  "mevedel-session-persistence" (plist workspace))
(declare-function mevedel-session-persistence-load-sidecar
                  "mevedel-session-persistence" (path))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer &optional settled))
(declare-function mevedel-session-persistence--sidecar-path
                  "mevedel-session-persistence" (save-path))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-lease-acquire
                  "mevedel-session-durability"
                  (session-dir buffer-name &optional session))
(declare-function mevedel-session-durability-lease-release
                  "mevedel-session-durability"
                  (session-dir &optional session))
(declare-function mevedel-session-durability-publication-head
                  "mevedel-session-durability" (session-dir))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state
                  "mevedel-transcript-restore" ())

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-read
                  "mevedel-session-publication" (session-dir))

;; `mevedel-session-transfer'
(declare-function mevedel-session-transfer-request
                  "mevedel-session-transfer" (session &optional label))
(declare-function mevedel-session-transfer-poll
                  "mevedel-session-transfer" (session))
(declare-function mevedel-session-transfer-decide
                  "mevedel-session-transfer" (session decision))
(declare-function mevedel-session-transfer-release
                  "mevedel-session-transfer" (session))

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p
                  "mevedel-transport" (&optional path))

(defun mevedel-session-control-transfer-register-observer
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
                                mevedel-session-control-transfer--observers)))
           mevedel-session-control-transfer--observers)
  observer)

(defun mevedel-session-control-transfer-unregister-observer
    (session observer)
  "Unregister OBSERVER from SESSION's semantic event stream."
  (when session
    (let ((observers
           (delq observer
                 (gethash session
                          mevedel-session-control-transfer--observers))))
      (if observers
          (puthash session observers
                   mevedel-session-control-transfer--observers)
        (remhash session mevedel-session-control-transfer--observers))))
  nil)

(defun mevedel-session-control-transfer-notify
    (session event &rest args)
  "Notify SESSION observers of semantic EVENT and ARGS.

Observer failures are diagnostic-only: durable state transitions must not be
rolled back because a view was closed during a redraw."
  (dolist (observer
           (copy-sequence
            (gethash session mevedel-session-control-transfer--observers)))
    (condition-case err
        (apply observer event args)
      (error
       (message "mevedel: session observer failed for %s: %s"
                event (error-message-string err)))))
  nil)

(defun mevedel-session-control-transfer-register-drain
    (session predicate)
  "Register PREDICATE as transient drain work for SESSION.

PREDICATE must return non-nil while its owner still has work that prevents a
lease handoff.  Return PREDICATE as the unregister token."
  (unless (functionp predicate)
    (error "Control-transfer drain must be callable"))
  (mevedel-session-set-control-transfer-drains
   session
   (cons predicate
         (delq predicate
               (mevedel-session-control-transfer-drains session))))
  predicate)

(defun mevedel-session-control-transfer-unregister-drain
    (session predicate)
  "Remove PREDICATE from SESSION's transient drain registry."
  (when session
    (mevedel-session-set-control-transfer-drains
     session
     (delq predicate (mevedel-session-control-transfer-drains session))))
  nil)

(defun mevedel-session-control-transfer-register-root-buffer
    (session buffer)
  "Register BUFFER as SESSION's authoritative root data buffer.

The registration is transient and is cleared when the owning data buffer dies.
Return BUFFER so lifecycle hooks can use this as their value."
  (unless (buffer-live-p buffer)
    (error "Control-transfer root buffer must be live"))
  (maphash
   (lambda (id registered)
     (when (eq registered buffer)
       (remhash id mevedel-session-control-transfer--roots)))
   mevedel-session-control-transfer--roots)
  (mevedel-session-set-root-buffer session buffer)
  (when-let ((id (mevedel-session-session-id session)))
    (puthash id buffer mevedel-session-control-transfer--roots))
  buffer)

(defun mevedel-session-control-transfer-unregister-root-buffer
    (session buffer)
  "Clear SESSION's root registration when it names BUFFER."
  (when (and session (eq buffer (mevedel-session-root-buffer session)))
    (mevedel-session-set-root-buffer session nil)
    (when-let ((id (mevedel-session-session-id session)))
      (when (eq buffer (gethash id mevedel-session-control-transfer--roots))
        (remhash id mevedel-session-control-transfer--roots))))
  nil)

(defun mevedel-session-control-transfer-root-buffer (session)
  "Return SESSION's registered live root data buffer, or nil."
  (let ((buffer (mevedel-session-root-buffer session)))
    (when (buffer-live-p buffer)
      buffer)))

(defun mevedel-session-control-transfer-root-buffer-for-id (session-id)
  "Return the registered live root buffer for SESSION-ID, or nil."
  (when-let ((buffer (gethash session-id
                             mevedel-session-control-transfer--roots)))
    (if (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (or (not (boundp 'mevedel--session))
                   (null mevedel--session)
                   (and (mevedel-session-p mevedel--session)
                        (equal session-id
                               (mevedel-session-session-id
                                mevedel--session))))))
        buffer
      (remhash session-id mevedel-session-control-transfer--roots)
      nil)))

(defun mevedel-session-control-transfer-register-presentation
    (session buffer)
  "Register the transient presentation BUFFER for SESSION."
  (unless (buffer-live-p buffer)
    (error "Session presentation buffer must be live"))
  (puthash session buffer mevedel-session-control-transfer--presentations)
  buffer)

(defun mevedel-session-control-transfer-unregister-presentation
    (session buffer)
  "Unregister BUFFER when it is SESSION's presentation."
  (when (eq buffer (gethash session
                            mevedel-session-control-transfer--presentations))
    (remhash session mevedel-session-control-transfer--presentations))
  nil)

(defun mevedel-session-control-transfer-presentation-buffer (session)
  "Return SESSION's live transient presentation buffer, or nil."
  (when-let ((buffer (gethash session
                             mevedel-session-control-transfer--presentations)))
    (if (buffer-live-p buffer)
        buffer
      (remhash session mevedel-session-control-transfer--presentations)
      nil)))

(defun mevedel-session-control-transfer-drained-p (session)
  "Return non-nil when SESSION has no work that blocks lease handoff.

The coordinator owns the session-state part of this decision.  UI and other
transient owners contribute through the registered drain predicates; a failed
predicate is conservatively treated as still draining."
  (and (not (mevedel-request-active-p
             (mevedel-session-control-transfer-root-buffer session)))
       (not (mevedel-session-pending-publication session))
       (not (mevedel-session-publication-active-p session))
       (not (mevedel-session-pending-input-p session))
       (not (mevedel-session-pending-plan-approval session))
       (not (mevedel-session-permission-queue session))
       (not (mevedel-execution-session-live-p session))
       (not (mevedel-execution-unsettled-mutation-p session))
       (not (mevedel-agent-control-active-turn-p session))
       (seq-every-p (lambda (predicate)
                      (not (condition-case nil
                               (funcall predicate)
                             (error t))))
                    (mevedel-session-control-transfer-drains session))))

(defun mevedel-session-control-transfer-drain-blocker (session)
  "Return a short phrase naming what blocks SESSION's handoff, or nil.

Only the owner can answer this: a requester sees `quiescing' and nothing
about why.  The first blocker is enough -- the user wants to know whether the
wait is theirs to end, not an inventory."
  (cond
   ((mevedel-request-active-p
     (mevedel-session-control-transfer-root-buffer session))
    "a running request")
   ((mevedel-execution-session-live-p session) "a live execution")
   ((mevedel-session-permission-queue session) "a permission prompt")
   ((mevedel-session-pending-plan-approval session) "a plan approval")
   ((mevedel-session-pending-input-p session) "queued input")
   ((mevedel-agent-control-active-turn-p session) "an agent turn")
   ((mevedel-execution-unsettled-mutation-p session) "an unsettled mutation")
   ((or (mevedel-session-pending-publication session)
        (mevedel-session-publication-active-p session))
    "a publication")
   ((not (mevedel-session-control-transfer-drained-p session))
    "the view")))

(defun mevedel-session-control-transfer-observe (session)
  "Refresh SESSION's durable transfer state without settling it.

Admission checks use this operation so observing a grant never saves or
releases the owner's session."
  (require 'mevedel-session-transfer)
  (let ((state (mevedel-session-transfer-poll session)))
    (mevedel-session-set-control-transfer
     session (or state (mevedel-session-control-transfer session)))
    (mevedel-session-control-transfer session)))

(defun mevedel-session-control-transfer--poll-owner (session)
  "Poll owner-side state, save drained work, and release the granted lease."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-transfer)
  (let ((state (mevedel-session-transfer-poll session)))
    (when (and (eq (plist-get state :state) 'quiescing)
               (mevedel-session-control-transfer-drained-p session))
      (let ((buffer (mevedel-session-control-transfer-root-buffer session)))
        (when (buffer-live-p buffer)
          (mevedel-session-persistence-save session buffer t)
          (mevedel-session-transfer-release session)
          (mevedel-session-persistence--apply-read-only-mode
           buffer
           (format "Control transferred to %s"
                   (or (plist-get (plist-get state :request)
                                  :requester-label)
                       "another client"))))))
    (mevedel-session-control-transfer session)))

(defun mevedel-session-control-transfer--insert-committed-segment
    (session buffer)
  "Replace BUFFER with SESSION's current committed segment bytes.

The buffer is left unmodified and read-only-neutral: the caller decides
whether the result is a writable owner buffer or a read-only follower.  The
segment number comes from the committed session state, so a rotation on the
owner's side repoints the visited file here too."
  (let* ((save-path (mevedel-session-save-path session))
         (logical
          (file-name-nondirectory
           (mevedel-session-persistence--segment-path
            save-path (mevedel-session-current-segment session))))
         (segment-path (expand-file-name logical save-path))
         (content
          (mevedel-session-persistence-read-artifact session logical t)))
    (with-current-buffer buffer
      (setq buffer-file-name segment-path
            buffer-file-truename nil
            default-directory (mevedel-session-working-directory session))
      (set-visited-file-modtime)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (decode-coding-string
          content (or buffer-file-coding-system 'utf-8-unix))))
      (require 'mevedel-transcript-restore)
      (mevedel-transcript-restore-gptel-state)
      (set-buffer-modified-p nil)
      (set-visited-file-modtime))))

(defun mevedel-session-control-transfer--follow-published
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
                 (buffer-local-value 'mevedel-session-follow-published buffer))
             (not (buffer-modified-p buffer))
             (mevedel-session-save-path session))
    (require 'mevedel-session-durability)
    (require 'mevedel-session-publication)
    (let* ((save-path (mevedel-session-save-path session))
           (head (mevedel-session-durability-publication-head save-path)))
      (when (and head
                 (not (equal head
                             (plist-get (mevedel-session-publication session)
                                        :head))))
        (let* ((workspace (mevedel-session-workspace session))
               (target (mevedel-session-execution-target session))
               (lease (mevedel-session-lease session))
               (timer (mevedel-session-lease-renewal-timer session))
               (transfer (mevedel-session-control-transfer session))
               (publication (mevedel-session-publication-read save-path))
               (sidecar
                (and publication
                     (mevedel-session-persistence-load-sidecar
                      (plist-get publication :sidecar))))
               (refreshed
                (and sidecar
                     (plist-get
                      (mevedel-session-persistence-deserialize
                       sidecar workspace)
                      :session))))
          (when refreshed
            (mevedel-session-persistence--copy-session-state refreshed session)
            (mevedel-session-set-execution-target session target)
            ;; The follower holds no lease and runs no renewal timer; adopting
            ;; committed state must not invent either.
            (mevedel-session-adopt-committed-state
             session workspace save-path lease timer publication transfer
             buffer)
            (mevedel-session-control-transfer--insert-committed-segment
             session buffer)
            t))))))

(defun mevedel-session-control-transfer--adopt-control (session buffer)
  "Adopt SESSION's committed state into BUFFER under a freshly held lease.

The caller has already acquired the lease.  Losing it again on any failure is
the point of the unwind: a client that cannot finish adopting committed state
must not keep other clients out.

The target incarnation check runs after committed session state is copied and
before transcript bytes are inserted or the buffer becomes writable."
  (condition-case err
      (let* ((save-path (mevedel-session-save-path session))
             (workspace (mevedel-session-workspace session))
             (target (mevedel-session-execution-target session))
             (lease (mevedel-session-lease session))
             (timer (mevedel-session-lease-renewal-timer session))
             (transfer (mevedel-session-control-transfer session))
             (publication
              (or (mevedel-session-publication-read save-path)
                  (error "Transferred session has no committed publication")))
             (sidecar
              (mevedel-session-persistence-load-sidecar
               (plist-get publication :sidecar)))
             (refreshed
              (plist-get
               (mevedel-session-persistence-deserialize sidecar workspace)
               :session)))
        (unless refreshed
          (error "Transferred session has no valid committed sidecar"))
        (mevedel-session-persistence--copy-session-state refreshed session)
        (mevedel-session-set-execution-target session target)
        (mevedel-session-adopt-committed-state
         session workspace save-path lease timer publication transfer buffer)
        (mevedel-session-control-transfer-register-root-buffer session buffer)
        (mevedel-session-persistence--check-target-incarnation session buffer)
        (mevedel-session-control-transfer--insert-committed-segment
         session buffer)
        (with-current-buffer buffer
          (setq buffer-read-only nil
                mevedel-session--read-only-mode nil))
        (mevedel-session-persistence--load-instructions session buffer)
        (mevedel-session-set-control-transfer
         session
         (list :state 'acquired :request (plist-get transfer :request)))
        (message "mevedel: control acquired; session is writable")
        t)
    (error
     (mevedel-session-durability-lease-release
      (mevedel-session-save-path session) session)
     (signal (car err) (cdr err)))))

(defun mevedel-session-control-transfer--poll-requester (session buffer)
  "Acquire SESSION into BUFFER after its committed release fence is visible."
  (when (and (buffer-live-p buffer)
             (mevedel-session-save-path session)
             (eq (plist-get (mevedel-session-control-transfer session) :state)
                 'requested))
    (mevedel-session-control-transfer-register-root-buffer session buffer)
    (when (buffer-modified-p buffer)
      (user-error "Read-only session changed locally; refresh before transfer"))
    (require 'mevedel-session-durability)
    (require 'mevedel-session-publication)
    (when (mevedel-session-durability-lease-acquire
           (mevedel-session-save-path session)
           (buffer-name buffer)
           session)
      (mevedel-session-control-transfer--adopt-control session buffer))))

(defun mevedel-session-control-transfer-acquire (session buffer)
  "Take SESSION's unheld lease directly into BUFFER.

For a lease nobody holds there is no owner to request control from, and for
an expired one the lease layer's own takeover confirmation is the whole
negotiation.  Both reduce to acquiring and adopting committed state, which is
also what a granted transfer ends with."
  (unless (buffer-live-p buffer)
    (error "Session buffer is not live"))
  (unless (mevedel-session-save-path session)
    (user-error "Session has not been materialized yet"))
  (when (buffer-modified-p buffer)
    (user-error "Read-only session changed locally; refresh before taking control"))
  (require 'mevedel-session-durability)
  (require 'mevedel-session-publication)
  (mevedel-session-control-transfer-register-root-buffer session buffer)
  (if (mevedel-session-durability-lease-acquire
       (mevedel-session-save-path session) (buffer-name buffer) session)
      (mevedel-session-control-transfer--adopt-control session buffer)
    (user-error "Another client took this session's lease first")))

(defun mevedel-session-control-transfer-poll (session buffer read-only-p)
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
  (require 'mevedel-transport)
  (unless (or (mevedel-session-publication-active-p session)
              (mevedel-transport-busy-p (mevedel-session-save-path session)))
    (if read-only-p
        (or (mevedel-session-control-transfer--poll-requester session buffer)
            (mevedel-session-control-transfer--follow-published
             session buffer))
      (mevedel-session-control-transfer--poll-owner session))))

(defun mevedel-session-control-transfer-request (session)
  "Record a control request for SESSION's current owner."
  (require 'mevedel-session-transfer)
  (let ((request (mevedel-session-transfer-request session)))
    (when request
      (mevedel-session-set-control-transfer
       session (list :state 'requested :request request)))
    request))

(defun mevedel-session-control-transfer-decide (session decision)
  "Record DECISION for SESSION's current transfer request."
  (require 'mevedel-session-transfer)
  (mevedel-session-transfer-decide session decision))

(defun mevedel-session-control-transfer-descriptor (session read-only-p)
  "Return the semantic interaction descriptor for SESSION.

The descriptor contains no view overlays or keymaps.  A view may render it
using its own interaction presentation."
  (let* ((transfer (mevedel-session-control-transfer session))
         (state (plist-get transfer :state))
         (request (plist-get transfer :request))
         (label (plist-get request :requester-label)))
    (cond
     ;; The owner decides.  A requester in the same durable state is waiting
     ;; on its own request and has nothing to grant.
     ((and (not read-only-p) (memq state '(requested quiescing)))
      (list :kind 'control-transfer
            :action 'grant
            :body
            (if (eq state 'quiescing)
                (format "Control granted to %s  finishing %s"
                        (or label "another client")
                        (or (mevedel-session-control-transfer-drain-blocker
                             session)
                            "up"))
              (format "Control transfer requested by %s  [g]rant  [k]eep"
                      (or label "another client")))
            :help-echo "Grant or keep the current lease"))
     ((and read-only-p (eq state 'requested))
      (list :kind 'control-transfer
            :action 'requested
            :body "Control requested  waiting for the owner"
            :help-echo "The owner grants automatically once the request times out"))
     ((and read-only-p (eq state 'quiescing))
      (list :kind 'control-transfer
            :action 'requested
            :body "Control granted  waiting for the owner to finish"
            :help-echo "Control arrives once the owner has drained its work"))
     ((and read-only-p (eq state 'rejected))
      (list :kind 'control-transfer
            :action 'request
            :body "Control request was declined  [r]equest again"
            :help-echo "Request cooperative control"))
     (read-only-p
      (list :kind 'control-transfer
            :action 'request
            :body "Session is read-only  [r]equest control"
            :help-echo "Request cooperative control")))))

(provide 'mevedel-session-control-transfer)

;;; mevedel-session-control-transfer.el ends here
