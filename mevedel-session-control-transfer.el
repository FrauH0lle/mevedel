;;; mevedel-session-control-transfer.el -- Transfer coordination -*- lexical-binding: t; -*-

;;; Commentary:

;; Coordinates cooperative portable-session control transfer.  Persistence
;; calls this coordinator for polling, admission, and committed-state
;; adoption.  The transfer module owns the durable request/lease protocol;
;; views register transient drain predicates and render semantic descriptors.

;;; Code:

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-active-turn-p
                  "mevedel-agent-control" (session))

;; `mevedel-execution'
(declare-function mevedel-execution-session-live-p
                  "mevedel-execution" (session))
(declare-function mevedel-execution-unsettled-mutation-p
                  "mevedel-execution" (session))

;; `mevedel-overlays'
(declare-function mevedel--instruction-state-rollback
                  "mevedel-overlays" (workspace))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-lease-acquire
                  "mevedel-session-durability"
                  (session-dir buffer-name &optional session))
(declare-function mevedel-session-durability-lease-release
                  "mevedel-session-durability"
                  (session-dir &optional session))
(declare-function mevedel-session-durability-publication-head
                  "mevedel-session-durability" (session-dir))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--apply-read-only-mode
                  "mevedel-session-persistence" (buffer &optional reason))
(declare-function mevedel-session-persistence--check-target-incarnation
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence--copy-session-state
                  "mevedel-session-persistence" (from to))
(declare-function mevedel-session-persistence--load-instructions
                  "mevedel-session-persistence"
                  (session buffer &optional turn directive-records
                           preserve-directives-p))
(declare-function mevedel-session-persistence--segment-path
                  "mevedel-session-persistence" (save-path segment))
(declare-function mevedel-session-persistence-deserialize
                  "mevedel-session-persistence" (plist workspace))
(declare-function mevedel-session-persistence-load-sidecar
                  "mevedel-session-persistence" (path))
(declare-function mevedel-session-persistence-read-artifact
                  "mevedel-session-persistence"
                  (session logical &optional committed-only))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence"
                  (session buffer &optional settled force))

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-read
                  "mevedel-session-publication" (session-dir))

;; `mevedel-session-transfer'
(declare-function mevedel-session-transfer-decide
                  "mevedel-session-transfer" (session decision))
(declare-function mevedel-session-transfer-observe-decision
                  "mevedel-session-transfer" (session request))
(declare-function mevedel-session-transfer-poll
                  "mevedel-session-transfer" (session))
(declare-function mevedel-session-transfer-request
                  "mevedel-session-transfer" (session &optional label))
(declare-function mevedel-session-transfer-release
                  "mevedel-session-transfer" (session))

;; `mevedel-structs'
(declare-function mevedel-request-active-p
                  "mevedel-structs" (&optional buffer))
(declare-function mevedel-session-adopt-committed-state
                  "mevedel-structs"
                  (session workspace save-path lease lease-renewal-timer
                           publication control-transfer control-transfer-drains
                           root-buffer))
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-control-transfer-drains
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease-renewal-timer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p
                  "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-input-p
                  "mevedel-structs" (session))
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-active-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-root-buffer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-control-transfer
                  "mevedel-structs" (session transfer))
(declare-function mevedel-session-set-control-transfer-drains
                  "mevedel-structs" (session predicates))
(declare-function mevedel-session-set-execution-target
                  "mevedel-structs" (session target))
(declare-function mevedel-session-set-root-buffer
                  "mevedel-structs" (session buffer))
(declare-function mevedel-session-workspace
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel-session--read-only-mode)

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-gptel-state
                  "mevedel-transcript-restore" ())

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p
                  "mevedel-transport" (&optional path))

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
           (format "control transferred to %s; this session is read-only here"
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

(defun mevedel-session-control-transfer--stage-session
    (session refreshed publication buffer)
  "Attach SESSION's live runtime owners to REFRESHED for staged adoption."
  (mevedel-session-set-execution-target
   refreshed
   (when-let ((target (mevedel-session-execution-target session)))
     (copy-sequence target)))
  (mevedel-session-adopt-committed-state
   refreshed
   (mevedel-session-workspace session)
   (mevedel-session-save-path session)
   (mevedel-session-lease session)
   (mevedel-session-lease-renewal-timer session)
   publication
   (mevedel-session-control-transfer session)
   (mevedel-session-control-transfer-drains session)
   buffer)
  refreshed)

(defun mevedel-session-control-transfer--install-staged-segment
    (buffer staging-buffer)
  "Install STAGING-BUFFER into BUFFER, restoring BUFFER if a hook fails."
  (let ((original (generate-new-buffer " *mevedel-transfer-original*"))
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
                    (replace-buffer-contents staging-buffer)
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
                   (replace-buffer-contents original)
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
                      :session)))
               (staging-buffer
                (and refreshed
                     (generate-new-buffer " *mevedel-follow-staging*"))))
          (when refreshed
            (unwind-protect
                (progn
                  (mevedel-session-control-transfer--stage-session
                   session refreshed publication staging-buffer)
                  (mevedel-session-control-transfer--insert-committed-segment
                   refreshed staging-buffer)
                  (mevedel-session-control-transfer--install-staged-segment
                   buffer staging-buffer)
                  (mevedel-session-persistence--copy-session-state
                   refreshed session)
                  (mevedel-session-set-root-buffer session buffer)
                  t)
              (when (buffer-live-p staging-buffer)
                (with-current-buffer staging-buffer
                  (set-buffer-modified-p nil))
                (kill-buffer staging-buffer)))))))))

(defun mevedel-session-control-transfer--adopt-control (session buffer)
  "Adopt SESSION's committed state into BUFFER under a freshly held lease.

The caller has already acquired the lease.  Losing it again on any failure is
the point of the unwind: a client that cannot finish adopting committed state
must not keep other clients out.

The target incarnation, transcript, and instruction restore are staged before
the live session or buffer changes."
  (require 'mevedel-overlays)
  (condition-case err
      (let* ((save-path (mevedel-session-save-path session))
             (workspace (mevedel-session-workspace session))
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
               :session))
             (staging-buffer
              (generate-new-buffer " *mevedel-transfer-staging*"))
             (instruction-rollback
              (mevedel--instruction-state-rollback workspace)))
        (unless refreshed
          (error "Transferred session has no valid committed sidecar"))
        (unwind-protect
            (progn
              (mevedel-session-control-transfer--stage-session
               session refreshed publication staging-buffer)
              (mevedel-session-control-transfer--insert-committed-segment
               refreshed staging-buffer)
              (mevedel-session-control-transfer-register-root-buffer
               refreshed staging-buffer)
              (mevedel-session-persistence--check-target-incarnation
               refreshed staging-buffer)
              (condition-case install-error
                  (progn
                    (unless
                        (mevedel-session-persistence--load-instructions
                         refreshed staging-buffer)
                      (error "Transferred instruction snapshot is invalid"))
                    (mevedel-session-control-transfer--install-staged-segment
                     buffer staging-buffer))
                (error
                 (funcall instruction-rollback)
                 (signal (car install-error) (cdr install-error))))
              (mevedel-session-persistence--copy-session-state
               refreshed session)
              (mevedel-session-control-transfer-register-root-buffer
               session buffer)
              (with-current-buffer buffer
                (setq buffer-read-only nil
                      mevedel-session--read-only-mode nil))
              (mevedel-session-set-control-transfer
               session
               (list :state 'acquired :request (plist-get transfer :request)))
              (message "mevedel: control acquired; session is writable")
              t)
          (when (buffer-live-p staging-buffer)
            (mevedel-session-control-transfer-register-root-buffer
             session buffer)
            (with-current-buffer staging-buffer
              (set-buffer-modified-p nil))
            (kill-buffer staging-buffer))))
    (error
     (mevedel-session-durability-lease-release
      (mevedel-session-save-path session) session)
     (signal (car err) (cdr err)))))

(defun mevedel-session-control-transfer--poll-requester (session buffer)
  "Acquire SESSION into BUFFER after its committed release fence is visible."
  (when (and (buffer-live-p buffer)
             (mevedel-session-save-path session)
             (memq (plist-get (mevedel-session-control-transfer session) :state)
                   '(requested quiescing)))
    (mevedel-session-control-transfer-register-root-buffer session buffer)
    (when (buffer-modified-p buffer)
      (user-error "Read-only session changed locally; refresh before transfer"))
    (require 'mevedel-session-durability)
    (require 'mevedel-session-publication)
    (require 'mevedel-session-transfer)
    (let* ((transfer (mevedel-session-control-transfer session))
           (request (plist-get transfer :request))
           (decision
            (or (plist-get transfer :decision)
                (mevedel-session-transfer-observe-decision
                 session request))))
      (cond
       ((eq 'reject (plist-get decision :decision))
        (mevedel-session-set-control-transfer
         session (list :state 'rejected :request request :decision decision))
        t)
       (t
        (when decision
          (mevedel-session-set-control-transfer
           session
           (list :state 'quiescing :request request :decision decision)))
        (when (mevedel-session-durability-lease-acquire
               (mevedel-session-save-path session)
               (buffer-name buffer)
               session)
          (mevedel-session-control-transfer--adopt-control
           session buffer)))))))

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

The descriptor carries no faces, overlays, or keymaps: `:title', `:detail',
and `:keys' say what is true and what the user may do, and the view decides
how prominent that is.  `:attention' marks the states that are waiting on a
person rather than on the protocol.

READ-ONLY-P selects the side.  Owner and requester share one durable state
and need opposite presentations of it: only the owner can decide, and only
the owner can see what a handoff is still waiting for."
  (let* ((transfer (mevedel-session-control-transfer session))
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
            :detail (if-let ((blocker
                              (mevedel-session-control-transfer-drain-blocker
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
            :help-echo "Waiting for the owner to answer or time out"))
     ((eq state 'quiescing)
      (list :kind 'control-transfer
            :action 'status
            :title "Control granted"
            :detail "Waiting for the owner to finish its current work."
            :help-echo "Control arrives once the owner has drained its work"))
     ((eq state 'rejected)
      (list :kind 'control-transfer
            :action 'request
            :attention t
            :title "Control request was declined"
            :keys '(("r" . "request again"))
            :help-echo "Request cooperative control"))
     (t
      (list :kind 'control-transfer
            :action 'request
            :title "This session is read-only here"
            :detail "Another client holds its lease."
            :keys '(("r" . "request control"))
            :help-echo "Request cooperative control")))))

(provide 'mevedel-session-control-transfer)

;;; mevedel-session-control-transfer.el ends here
