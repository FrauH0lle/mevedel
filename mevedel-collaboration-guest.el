;;; mevedel-collaboration-guest.el --- collaboration guest protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; Handles untrusted guest protocol frames for a live collaboration room:
;; authentication, Web Push registration, prompt and attachment intake,
;; retraction, abort, and relay peer lifecycle.  Room ownership, projection,
;; outbound publication, and public commands remain in the collaboration
;; facade.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--base64url-decode
                  "mevedel-collaboration" (string))
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--guest-text
                  "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish-queue
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish-status
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--queue-position
                  "mevedel-collaboration" (room entry))
(declare-function mevedel-collaboration--room-data-buffer
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-buffer
                  "mevedel-collaboration" (data-buffer))
(declare-function mevedel-collaboration--room-for-overlay
                  "mevedel-collaboration" (overlay))
(declare-function mevedel-collaboration--room-list
                  "mevedel-collaboration" ())
(declare-function mevedel-collaboration--sanitize-guest-id
                  "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--sanitize-guest-name
                  "mevedel-collaboration" (name))
(declare-function mevedel-collaboration--send-queue-state
                  "mevedel-collaboration" (room peer guest &optional force))
(declare-function mevedel-collaboration--status-frame
                  "mevedel-collaboration" (room))
(defvar mevedel-collaboration--attachment-extensions)
(defvar mevedel-collaboration--duplicate-prompt-window)
(defvar mevedel-collaboration--max-attachment-bytes)
(defvar mevedel-collaboration--max-prompt-attachments)
(defvar mevedel-collaboration--max-prompt-bytes)
(defvar mevedel-collaboration-guest-skills)
(defvar mevedel-collaboration-remote-interactions)
(defvar mevedel-collaboration-unsafe-guest-commands)

;; `mevedel-collaboration-agent'
(declare-function mevedel-collaboration--agents-frame
                  "mevedel-collaboration-agent" (room))
(declare-function mevedel-collaboration--handle-fetch-agent
                  "mevedel-collaboration-agent" (room peer frame))

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration--handle-artifact-get
                  "mevedel-collaboration-artifact" (room peer frame))

;; `mevedel-collaboration-owner'
(declare-function mevedel-collaboration--handle-new-session
                  "mevedel-collaboration-owner" (room peer frame))
(declare-function mevedel-collaboration--handle-set-mode
                  "mevedel-collaboration-owner" (room peer frame))
(autoload 'mevedel-collaboration--handle-new-session
  "mevedel-collaboration-owner")
(autoload 'mevedel-collaboration--handle-set-mode
  "mevedel-collaboration-owner")

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection" (data-buffer))
(declare-function mevedel-collaboration--json-record
                  "mevedel-collaboration-projection" (record))
(declare-function mevedel-collaboration--json-string
                  "mevedel-collaboration-projection" (object))
(defvar mevedel-collaboration--protocol-version)

;; `mevedel-collaboration-task'
(declare-function mevedel-collaboration--tasks-frame
                  "mevedel-collaboration-task" (room))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-control
                  "mevedel-collaboration-transport" (transport control))
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(defvar mevedel-collaboration--max-frame-json-bytes)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))
(declare-function mevedel--prompt-user-with-overlay
                  "mevedel-interaction-prompt"
                  (title content question help-echo-text callback
                         &optional host-only))

;; `mevedel-pending-inputs'
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-pending-inputs"
                  (data-buffer text &rest keys))
(autoload 'mevedel-view-enqueue-external-follow-up "mevedel-pending-inputs")

;; `mevedel-skills-core'
(declare-function mevedel-skill-argument-hint
                  "mevedel-skills-core" (skill))
(declare-function mevedel-skill-name "mevedel-skills-core" (skill))

;; `mevedel-skills-ui'
(declare-function mevedel-skills-user-visible-skills
                  "mevedel-skills-ui" (session &optional inline-only))
(autoload 'mevedel-skills-user-visible-skills "mevedel-skills-ui")

;; `mevedel-structs'
(declare-function mevedel-directive-id "mevedel-structs" (record))
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (session))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(declare-function mevedel-session-workspace "mevedel-structs" (session))
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (session))
(declare-function mevedel-workspace-directives "mevedel-structs" (workspace))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-sanitize
                  "mevedel-session-artifacts" (name))

;; `mevedel-view'
(declare-function mevedel-view--abort-data-buffer
                  "mevedel-view" (data-buffer))
(autoload 'mevedel-view--abort-data-buffer "mevedel-view")

;; `mevedel-view-composer'
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(declare-function mevedel-view-invocation-kind
                  "mevedel-view-composer" (name &optional session))
(autoload 'mevedel-view-abort "mevedel-view-composer")
(autoload 'mevedel-view-invocation-kind "mevedel-view-composer")

;; `mevedel-view-input-files'
(declare-function mevedel-view--media-dir "mevedel-view-input-files" ())
(autoload 'mevedel-view--media-dir "mevedel-view-input-files")

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())
(autoload 'mevedel-view--interaction-rebuild "mevedel-view-interaction")


;;
;;; Guest protocol primitives

(defun mevedel-collaboration--request-id-p (value)
  "Return non-nil when VALUE is a bounded browser request id."
  (and (integerp value) (<= 0 value #x1fffffffffffff)))


;;
;;; Snapshot delivery

(defun mevedel-collaboration--snapshot-frame-overhead ()
  "Return the encoded bytes a snapshot frame costs before its records.
Measured with an empty record array and the longer `final' spelling, so a
chunk that turns out not to be the last one cannot overflow.  A JSON array
adds one separator per record after the first, which is what the record
sizes alone never accounted for."
  (string-bytes
   (mevedel-collaboration--json-string
    (list :t "snapshot-chunk"
          :records (vconcat nil)
          :final :json-false))))

(defun mevedel-collaboration--snapshot-chunks (records &optional overhead)
  "Split RECORDS into lists of JSON records each under the wire bound.
The bound belongs to the frame that goes on the wire, not to the records
in it.  A record too large to travel in a frame of its own is dropped:
emitting a frame the relay must refuse costs the host connection, and the
relay collects the room with it, so one oversized record would end the
session for every guest.  OVERHEAD is the encoded bytes the carrying
frame costs before its records; it defaults to the snapshot frame's."
  (let* ((overhead (or overhead
                       (mevedel-collaboration--snapshot-frame-overhead)))
         (limit mevedel-collaboration--max-frame-json-bytes)
         chunks current (size 0))
    (dolist (record records)
      (let* ((json (mevedel-collaboration--json-record record))
             (bytes (string-bytes
                     (mevedel-collaboration--json-string json))))
        (unless (> (+ overhead bytes) limit)
          ;; One separator for every record after the first in the chunk.
          (when (and current
                     (> (+ overhead size 1 bytes) limit))
            (push (nreverse current) chunks)
            (setq current nil size 0))
          (push json current)
          (setq size (+ size bytes (if (cdr current) 1 0))))))
    (when current
      (push (nreverse current) chunks))
    (nreverse chunks)))

(defconst mevedel-collaboration--command-hints
  '(("plan" . "[prompt]")
    ("goal" . "[objective]")
    ("compact" . "[instructions]")
    ("stop" . "[execution]"))
  "Argument hints for the local slash commands a guest may invoke.
Only commands whose arguments a guest can meaningfully supply appear;
anything absent is offered as an argument-less button.")

(defun mevedel-collaboration--guest-invocable-p (name)
  "Return non-nil when NAME may be invoked by a guest at all."
  (and (stringp name)
       (member name mevedel-collaboration-guest-skills)
       (not (member name mevedel-collaboration-unsafe-guest-commands))))

(defun mevedel-collaboration--guest-roster (room)
  "Return ROOM's allowlisted invocations as JSON-safe descriptors.

Each entry carries the name, which namespace it belongs to, and its
argument hint, because a guest button has to render the right sigil and
say whether the invocation wants arguments.  A name that resolves to
neither namespace is dropped rather than offered as a button that
cannot work."
  (when-let* ((data-buffer (mevedel-collaboration--room-data-buffer room))
              (view-buffer (buffer-local-value 'mevedel--view-buffer
                                               data-buffer))
              ((buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (let ((session (plist-get room :session))
            roster)
        (dolist (name mevedel-collaboration-guest-skills)
          (when (mevedel-collaboration--guest-invocable-p name)
            (when-let* ((kind (mevedel-view-invocation-kind name session)))
              (push (append
                     (list (cons "name" name)
                           (cons "kind" (symbol-name kind)))
                     (when-let* ((hint (mevedel-collaboration--invocation-hint
                                        name kind session)))
                       (list (cons "hint" hint))))
                    roster))))
        (nreverse roster)))))

(defun mevedel-collaboration--invocation-hint (name kind session)
  "Return the argument hint for invocation NAME of KIND, or nil."
  (pcase kind
    ('skill
     (when-let* ((skill (cl-find name
                                 (mevedel-skills-user-visible-skills session)
                                 :key #'mevedel-skill-name :test #'equal)))
       (mevedel-skill-argument-hint skill)))
    ('command
     (cdr (assoc name mevedel-collaboration--command-hints)))))

(defun mevedel-collaboration--send-snapshot (room peer)
  "Send ROOM's welcome and chunked snapshot to guest PEER."
  (let* ((transport (plist-get room :transport))
         (guest (mevedel-collaboration--guest room peer))
         (records (plist-get room :records))
         (chunks (or (mevedel-collaboration--snapshot-chunks records)
                     (list nil))))
    (mevedel-collaboration--transport-send
     transport peer
     (append
      (list :t "welcome"
            :proto mevedel-collaboration--protocol-version
            :readOnly (if (plist-get guest :writable) :json-false t)
            ;; Count what is actually sent: a record too large for a frame of
            ;; its own is dropped, and promising it would leave the guest
            ;; waiting for a chunk that never arrives.
            :recordCount (apply #'+ (mapcar #'length chunks)))
      ;; The host-curated roster is the guest's whole discovery
      ;; surface; a view link gets none, having no way to use it.
      (when-let* (((plist-get guest :writable))
                  (roster (mevedel-collaboration--guest-roster room)))
        (list :commands (vconcat roster)))))
    (cl-loop for rest on chunks do
             (mevedel-collaboration--transport-send
              transport peer
              (list :t "snapshot-chunk"
                    :records (vconcat (car rest))
                    :final (if (cdr rest) :json-false t))))))


;;
;;; Remote interactions

(defvar mevedel-collaboration--ui-request-counter 0
  "Monotonic id source for ui-request frames within this Emacs process.")

(defvar mevedel-collaboration-remote-guest nil
  "Display name of the guest whose answer is being applied, or nil.
Bound around a ui-response handler so downstream effects -- such as a
plan revision request queued by remote feedback -- can attribute their
output to the answering guest.")

(defun mevedel-collaboration--writable-peers (room)
  "Return the peer ids of ROOM's writable guests."
  (let (peers)
    (maphash (lambda (peer guest)
               (when (plist-get guest :writable)
                 (push peer peers)))
             (plist-get room :guests))
    (nreverse peers)))

(defun mevedel-collaboration--audience-peer-p (room peer audience)
  "Return non-nil when PEER in ROOM belongs to AUDIENCE.

AUDIENCE narrows an interaction below the writable default.  `:owner\='
restricts it to owner-link guests.  A nil AUDIENCE is every writable
guest, the ordinary case."
  (let ((guest (mevedel-collaboration--guest room peer)))
    (and guest
         (or (null (plist-get audience :owner))
             (plist-get guest :owner)))))

(defun mevedel-collaboration--audience-peers-for (room audience)
  "Return ROOM's writable peers that belong to AUDIENCE."
  (if (null audience)
      (mevedel-collaboration--writable-peers room)
    (seq-filter (lambda (peer)
                  (mevedel-collaboration--audience-peer-p
                   room peer audience))
                (mevedel-collaboration--writable-peers room))))

(defun mevedel-collaboration--audience-peers (room overlay)
  "Return ROOM's peers that may see OVERLAY's interaction."
  (mevedel-collaboration--audience-peers-for
   room (plist-get (overlay-get overlay 'mevedel--remote) :audience)))

(defun mevedel-collaboration--ui-request-frame (request-id overlay)
  "Return the ui-request frame for OVERLAY under REQUEST-ID."
  (let ((remote (overlay-get overlay 'mevedel--remote)))
    (append
     (list :t "ui-request"
           :reqId request-id
           :body (or (plist-get remote :body) "")
           :bodyKind (or (plist-get remote :body-kind) "text")
           :options
           (vconcat
            (cl-loop for (_outcome . label) in (plist-get remote :options)
                     for index from 0
                     collect `(("id" . ,index) ("label" . ,label))))
           :allowFeedback
           (if (plist-get remote :feedback) t :json-false))
     ;; A cancel handler settles just this interaction -- for the Ask
     ;; questionnaire, the run continues -- so the guest gets a Dismiss.
     (when (plist-get remote :cancel)
       (list :allowCancel t))
     ;; A questionnaire travels structurally; the guest answers all
     ;; questions atomically through the :answers response field.
     (when-let* ((questions (plist-get remote :questions)))
       (list :questions (vconcat (funcall questions)))))))

(defun mevedel-collaboration--on-prompt-created (overlay)
  "Present prompt OVERLAY to the active room's writable guests.

A re-render of the same interaction -- the permission queue redraws its
head on every selection change -- reuses the existing request id, so a
guest sees one card updated in place instead of an accumulating pile."
  (when-let* ((room (mevedel-collaboration--room-for-overlay overlay))
              (remote (overlay-get overlay 'mevedel--remote)))
    (when mevedel-collaboration-remote-interactions
      (let* ((requests (plist-get room :ui-requests))
             (interaction-id
              (overlay-get overlay 'mevedel-view-interaction-id))
             (existing-id
              (and interaction-id
                   (catch 'found
                     (maphash
                      (lambda (id tracked)
                        (when (and (overlayp tracked)
                                   (equal interaction-id
                                          (overlay-get
                                           tracked
                                           'mevedel-view-interaction-id)))
                          (throw 'found id)))
                      requests)
                     nil)))
             (request-id
              (or existing-id
                  (cl-incf mevedel-collaboration--ui-request-counter)))
             (peers (mevedel-collaboration--audience-peers room overlay)))
        (puthash request-id overlay requests)
        (let ((frame (mevedel-collaboration--ui-request-frame
                      request-id overlay)))
          (dolist (peer peers)
            (mevedel-collaboration--transport-send
             (plist-get room :transport) peer frame)))
        (unless existing-id
          ;; Waking a guest for a decision it is not shown is noise, so
          ;; the push follows the same audience the frame did.
          (if (plist-get remote :audience)
              (mevedel-collaboration--push-guests
               room (delq nil
                          (mapcar (lambda (peer)
                                    (plist-get
                                     (mevedel-collaboration--guest room peer)
                                     :guest-id))
                                  peers)))
            (mevedel-collaboration--push-writable-guests room)))))))

(defun mevedel-collaboration--on-prompt-settled (overlay)
  "Dismiss OVERLAY's ui-request from every guest surface.
Every room is searched rather than the overlay's buffer resolved: a
settled overlay may already be deleted, and a deleted overlay no longer
knows where it lived."
  (dolist (room (mevedel-collaboration--room-list))
    (let ((requests (plist-get room :ui-requests)))
      (maphash
       (lambda (request-id tracked)
         (when (eq tracked overlay)
           (remhash request-id requests)
           (dolist (peer (mevedel-collaboration--writable-peers room))
             (mevedel-collaboration--transport-send
              (plist-get room :transport) peer
              (list :t "ui-request-end" :reqId request-id)))))
       requests))))

(defun mevedel-collaboration--send-ui-requests (room peer)
  "Send ROOM's active ui-requests to writable guest PEER.

A narrowed interaction is withheld here as well as on creation: a guest
must not collect a decision it may not see by reconnecting."
  (let ((requests (plist-get room :ui-requests))
        ids)
    (maphash (lambda (request-id _overlay) (push request-id ids)) requests)
    (dolist (request-id (sort ids #'<))
      (let* ((overlay (gethash request-id requests))
             (audience (plist-get (overlay-get overlay 'mevedel--remote)
                                  :audience)))
        (when (or (null audience)
                  (mevedel-collaboration--audience-peer-p
                   room peer audience))
          (mevedel-collaboration--transport-send
           (plist-get room :transport) peer
           (mevedel-collaboration--ui-request-frame request-id overlay)))))))

(defun mevedel-collaboration--handle-ui-response (room peer frame)
  "Settle the ui-request answered by writable guest PEER through FRAME.

The first answer -- from Emacs or any guest -- wins; the shared settle
already guards exactly-once, and a request no longer in the registry is
ignored silently.  A function option runs in the prompt's buffer so an
answer can execute the same path the host key binding would."
  (let ((guest (mevedel-collaboration--guest room peer))
        (request-id (plist-get frame :reqId)))
    (when (and guest
               (plist-get guest :writable)
               mevedel-collaboration-remote-interactions
               (integerp request-id))
      (when-let* ((overlay (gethash request-id (plist-get room :ui-requests)))
                  ;; Seeing a narrowed interaction and answering it are
                  ;; the same authority, so the audience is rechecked
                  ;; here: a request id is guessable, the audience is not.
                  ((mevedel-collaboration--audience-peer-p
                    room peer
                    (plist-get (overlay-get overlay 'mevedel--remote)
                               :audience))))
        (let* ((remote (overlay-get overlay 'mevedel--remote))
               (options (plist-get remote :options))
               (feedback (plist-get frame :feedback))
               (option (plist-get frame :option))
               (answers (plist-get frame :answers))
               (feedback-handler (plist-get remote :feedback))
               (answer-handler (plist-get remote :answer))
               (cancel-handler (plist-get remote :cancel))
               (outcome
                (cond
                 ;; A cancel settles just this interaction through the
                 ;; handler the prompt offered; nothing else is touched.
                 ((and (eq (plist-get frame :cancel) t)
                       (functionp cancel-handler))
                  cancel-handler)
                 ;; A questionnaire response is submitted atomically.  A
                 ;; blank string is the Ask tool's explicit no-preference.
                 ((and answers
                       (functionp answer-handler)
                       (listp answers)
                       (let ((trimmed
                              (mapcar
                               (lambda (answer)
                                 (and
                                  (stringp answer)
                                  (<= (string-bytes answer)
                                      mevedel-collaboration--max-prompt-bytes)
                                  (string-trim answer)))
                               answers)))
                         (and (not (memq nil trimmed))
                              ;; Every answer lands in one tool result, so
                              ;; the set shares the budget its parts pass.
                              (<= (apply #'+ (mapcar #'string-bytes trimmed))
                                  mevedel-collaboration--max-prompt-bytes)
                              (lambda ()
                                (funcall answer-handler trimmed))))))
                 ((and feedback-handler
                       (mevedel-collaboration--guest-text feedback))
                  ;; A function handler owns the whole feedback flow, for
                  ;; prompts whose feedback is not a plain settle outcome.
                  (let ((text (mevedel-collaboration--guest-text feedback)))
                    (if (functionp feedback-handler)
                        (lambda () (funcall feedback-handler text))
                      (cons 'feedback text))))
                 ((and (integerp option) (nth option options))
                  (car (nth option options))))))
          (when (and outcome (buffer-live-p (overlay-buffer overlay)))
            (message "mevedel: interaction answered by guest %s"
                     (plist-get guest :name))
            (with-current-buffer (overlay-buffer overlay)
              (let ((mevedel-collaboration-remote-guest
                     (plist-get guest :name)))
                (condition-case err
                    (if (functionp outcome)
                        (funcall outcome)
                      (mevedel--prompt--settle overlay outcome))
                  (user-error
                   (message "mevedel: remote answer rejected: %s"
                            (error-message-string err))))))))))))


;;
;;; Inbound guest frames

(defun mevedel-collaboration--handle-hello (room peer frame)
  "Register guest PEER from its hello FRAME and send the snapshot."
  (let ((proto (plist-get frame :proto)))
    (if (not (equal proto mevedel-collaboration--protocol-version))
        (mevedel-collaboration--transport-send
         (plist-get room :transport) peer
         (list :t "error"
               :message (format "protocol mismatch: host speaks %d"
                                mevedel-collaboration--protocol-version)))
      (let* ((name (mevedel-collaboration--sanitize-guest-name
                    (plist-get frame :name)))
             (claimed (mevedel-collaboration--base64url-decode
                       (plist-get frame :writeToken)))
             (writable (and claimed
                            (equal claimed (plist-get room :write-token))))
             (claimed-owner (mevedel-collaboration--base64url-decode
                             (plist-get frame :ownerToken)))
             ;; Owner authority is never granted on its own: the owner
             ;; link contains the write token, so a peer claiming one
             ;; without the other is a forgery attempt, not a tier.
             (owner (and writable claimed-owner
                         (equal claimed-owner (plist-get room :owner-token))))
             (guest (list :name name :writable writable :owner owner
                          :ready t
                          :guest-id (mevedel-collaboration--sanitize-guest-id
                                     (plist-get frame :guestId)))))
        (puthash peer guest (plist-get room :guests))
        (mevedel-collaboration--send-snapshot room peer)
        ;; Queue and busy state travel only on change, so a joining
        ;; guest is told the current ones directly.
        (mevedel-collaboration--send-queue-state room peer guest t)
        (mevedel-collaboration--transport-send
         (plist-get room :transport) peer
         (mevedel-collaboration--status-frame room))
        ;; The roster broadcast is latched on change, so a joining guest
        ;; is told the current one directly -- an empty roster included,
        ;; because a reconnecting viewer must clear stale rows.
        (mevedel-collaboration--transport-send
         (plist-get room :transport) peer
         (mevedel-collaboration--agents-frame room))
        ;; The task list is latched the same way.
        (mevedel-collaboration--transport-send
         (plist-get room :transport) peer
         (mevedel-collaboration--tasks-frame room))
        (when (and writable mevedel-collaboration-remote-interactions)
          (mevedel-collaboration--send-ui-requests room peer))))))

(defconst mevedel-collaboration--max-push-endpoint-bytes 2048
  "Maximum encoded bytes accepted for a browser push endpoint.")

(defun mevedel-collaboration--push-endpoint-p (value)
  "Return non-nil when VALUE is a bounded HTTPS push endpoint."
  (and (stringp value)
       (<= (string-bytes value)
           mevedel-collaboration--max-push-endpoint-bytes)
       (string-match-p "\\`https://[^[:space:][:cntrl:]#]+\\'" value)))

(defun mevedel-collaboration--handle-push-subscription (room peer frame)
  "Forward authenticated PEER's push subscription FRAME for ROOM."
  (when-let* ((guest (mevedel-collaboration--guest room peer))
              ((plist-get guest :ready))
              (guest-id (plist-get guest :guest-id)))
    (pcase (plist-get frame :t)
      ("push-subscribe"
       (when-let* ((endpoint (plist-get frame :endpoint))
                   ((mevedel-collaboration--push-endpoint-p endpoint)))
         (puthash guest-id
                  (list :endpoint endpoint
                        :writable (plist-get guest :writable))
                  (plist-get room :push-guests))
         (mevedel-collaboration--transport-control
          (plist-get room :transport)
          (list :t "push-subscribe" :peer peer :guestId guest-id
                :endpoint endpoint
                :active (if (eq t (plist-get frame :active))
                            t :json-false)))))
      ("push-unsubscribe"
       (remhash guest-id (plist-get room :push-guests))
       (mevedel-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-unsubscribe" :guestId guest-id)))
      ("push-state"
       (mevedel-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-state" :peer peer :guestId guest-id
              :active (if (eq t (plist-get frame :active))
                          t :json-false)))))))

(defun mevedel-collaboration--push-guests (room guest-ids)
  "Ask ROOM's relay to wake subscribed GUEST-IDS."
  (when guest-ids
    (mevedel-collaboration--transport-control
     (plist-get room :transport)
     (list :t "push" :guestIds (vconcat guest-ids)))))

(defun mevedel-collaboration--push-writable-guests (room)
  "Wake every notification-subscribed writable guest of ROOM."
  (let (guest-ids)
    (maphash (lambda (guest-id subscription)
               (when (plist-get subscription :writable)
                 (push guest-id guest-ids)))
             (plist-get room :push-guests))
    (mevedel-collaboration--push-guests room guest-ids)))

(defun mevedel-collaboration--restore-push-subscriptions (room)
  "Restore ROOM's Web Push endpoints after a relay reconnect."
  (when-let* ((subscriptions (plist-get room :push-guests)))
    (maphash
     (lambda (guest-id subscription)
       (mevedel-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-subscribe" :peer 0 :guestId guest-id
              :endpoint (plist-get subscription :endpoint)
              :active :json-false)))
     subscriptions)))

(cl-defun mevedel-collaboration--handle-prompt (room peer frame)
  "Queue the prompt in FRAME from writable guest PEER as a follow-up.

The prompt enters the ordinary pending-input queue: delivered when the
session is idle, queued behind a running request, paused while the
Pending Inputs cockpit is open.  The guest name is attribution only and
never enters model-visible context.

FRAME may carry an `:invoke\=' naming an allowlisted command or skill,
in which case the text is that invocation\='s arguments rather than a
prompt.  The name travels as its own field and is validated here: guest
text is never scanned for a sigil, so a pasted log line cannot invoke
anything."
  (let* ((guest (mevedel-collaboration--guest room peer))
         (invoke (plist-get frame :invoke))
         (text (plist-get frame :text)))
    (when (and invoke
               (not (mevedel-collaboration--guest-invocable-p invoke)))
      (cl-return-from mevedel-collaboration--handle-prompt))
    (when (and guest
               (plist-get guest :writable)
               ;; An invocation may carry no arguments at all; a plain
               ;; prompt still has to say something.
               (or (and invoke (or (null text) (stringp text)))
                   (mevedel-collaboration--guest-text text)))
      ;; The prompt frame may carry a fresher display name than the hello
      ;; did; the badge should show what the guest typed.
      (when (stringp (plist-get frame :name))
        (plist-put guest :name (mevedel-collaboration--sanitize-guest-name
                                (plist-get frame :name))))
      ;; Drop a byte-identical repeat inside the duplicate window: a
      ;; double-fired client submit, not a second question.  Prompts
      ;; carrying attachments are never deduplicated -- consecutive
      ;; sends legitimately reuse the same placeholder text.
      (let ((last (plist-get guest :last-prompt))
            ;; An invocation and a prompt with the same text are
            ;; different sends, so the latch keys on both.
            (dedup-key (if invoke (format "%s\0%s" invoke (or text "")) text))
            (now (float-time)))
        (when (and last
                   (not (plist-get frame :images))
                   (equal (car last) dedup-key)
                   (< (- now (cdr last))
                      mevedel-collaboration--duplicate-prompt-window))
          (cl-return-from mevedel-collaboration--handle-prompt))
        (let* ((data-buffer (mevedel-collaboration--room-data-buffer room))
               (view-buffer (and data-buffer
                                 (buffer-local-value 'mevedel--view-buffer
                                                     data-buffer))))
          (when (buffer-live-p view-buffer)
            ;; Attachments ride the same pipeline as clipboard images
            ;; pasted in Emacs: saved under the session media directory,
            ;; then mentioned and read-granted by the queue seam.  Read
            ;; decides text or media from the extension, so nothing here
            ;; has to.
            ;; Both are durable, and the queue seam still refuses a prompt
            ;; whose session view is not live, so neither outlives a prompt
            ;; that was not queued.
            (let ((paths
                   (condition-case err
                       (with-current-buffer view-buffer
                         (mevedel-collaboration--save-guest-attachments
                          (plist-get frame :images)))
                     (error
                      ;; A failed media write is this prompt's problem, not
                      ;; the room's: letting it reach the frame handler
                      ;; tears the session down for every guest.
                      (display-warning
                       'mevedel
                       (format "Guest attachment could not be saved: %s"
                               (error-message-string err))
                       :warning)
                      (cl-return-from
                          mevedel-collaboration--handle-prompt))))
                  (queued nil))
              ;; Latch before the enqueue, which redraws and can therefore
              ;; re-enter, and give the latch back when nothing was queued.
              (plist-put guest :last-prompt (cons dedup-key now))
              (unwind-protect
                  (progn
                    (setq queued
                          (mevedel-view-enqueue-external-follow-up
                           data-buffer (or text "")
                           :guest-name (plist-get guest :name)
                           :guest-id (plist-get guest :guest-id)
                           :paths paths
                           :invoke invoke
                           :directive-id
                           (unless invoke
                             (mevedel-collaboration--guest-directive-id
                              room frame))))
                    (when queued
                      (mevedel-collaboration--transport-send
                       (plist-get room :transport) peer
                       (append
                        (list :t "queued")
                        (when-let* ((id (plist-get queued :id)))
                          (list :id id))
                        (when-let* ((position
                                     (mevedel-collaboration--queue-position
                                      room queued)))
                          (list :position position))))))
                (unless queued
                  (plist-put guest :last-prompt nil)
                  (dolist (path paths)
                    (when (file-exists-p path)
                      (ignore-errors (delete-file path)))))))))))))

(defun mevedel-collaboration--guest-directive-id (room frame)
  "Return the directive id FRAME asks ROOM to scope its prompt to, or nil.

The viewer sends the id its transcript filter is showing.  An id for a
directive the workspace no longer has yields nil, so a stale filter
sends to main chat instead of failing the prompt."
  (when-let* ((id (plist-get frame :directive))
              ((stringp id))
              (session (plist-get room :session))
              (workspace (mevedel-session-workspace session))
              ((cl-find id (mevedel-workspace-directives workspace)
                        :key #'mevedel-directive-id :test #'equal)))
    id))

(defun mevedel-collaboration--save-guest-attachments (images)
  "Save valid guest attachments IMAGES under the session media directory.
IMAGES is the decoded frame list of (:mime STRING :data BASE64) plists.
Return the saved absolute paths.  Runs in the view buffer.  Anything
invalid -- unknown type, undecodable data, or a set over the byte
budget -- drops the whole set rather than attaching a partial one."
  (when (and images (listp images)
             (<= (length images)
                 mevedel-collaboration--max-prompt-attachments))
    (catch 'invalid
      (let ((total 0)
            (decoded nil))
        (dolist (image images)
          (let* ((extension (cdr (assoc (plist-get image :mime)
                                        mevedel-collaboration--attachment-extensions)))
                 (bytes (and extension
                             (stringp (plist-get image :data))
                             (condition-case nil
                                 (base64-decode-string
                                  (plist-get image :data))
                               (error nil)))))
            (unless (and bytes (> (length bytes) 0))
              (throw 'invalid nil))
            (cl-incf total (length bytes))
            (when (> total mevedel-collaboration--max-attachment-bytes)
              (throw 'invalid nil))
            (push (cons extension bytes) decoded)))
        (let ((dir (mevedel-view--media-dir))
              (stamp (format-time-string "%Y%m%d-%H%M%S"))
              (n 0)
              (complete nil)
              paths)
          (unwind-protect
              (progn
                (dolist (entry (nreverse decoded))
                  (let ((path nil))
                    ;; `excl' makes the name its own claim: testing first and
                    ;; writing after leaves a window another writer can take,
                    ;; and remote media I/O can yield inside it.
                    (while (null path)
                      (let ((candidate
                             (file-name-concat
                              dir (format "guest-%s-%d.%s" stamp
                                          (cl-incf n) (car entry))))
                            (coding-system-for-write 'binary))
                        (condition-case nil
                            (progn
                              (write-region (cdr entry) nil candidate nil
                                            'silent nil 'excl)
                              (setq path candidate))
                          (file-already-exists nil))))
                    (push path paths)))
                (setq complete t)
                (nreverse paths))
            ;; A set is attached whole or not at all, so a set that failed
            ;; part way through takes its own files with it.
            (unless complete
              (dolist (path paths)
                (when (file-exists-p path)
                  (ignore-errors (delete-file path)))))))))))

(defun mevedel-collaboration--handle-retract (room peer frame)
  "Remove the pending entry FRAME names when guest PEER queued it.

Authority is per entry: the id must belong to an entry this guest's
stable id queued, so no guest can delete another guest's or the host's
pending input.  The entry's attachment files leave with it, mirroring
the failed-enqueue cleanup."
  (let ((guest (mevedel-collaboration--guest room peer))
        (id (plist-get frame :id)))
    (when (and guest
               (plist-get guest :writable)
               (plist-get guest :guest-id)
               (integerp id))
      (when-let* ((session (plist-get room :session))
                  (entries (mevedel-session-pending-follow-ups session))
                  (entry (cl-find-if
                          (lambda (candidate)
                            (and (equal id (plist-get candidate :id))
                                 (equal (plist-get guest :guest-id)
                                        (plist-get candidate :guest-id))
                                 ;; The drain is delivering it: the files
                                 ;; are about to be read mid-turn, so it
                                 ;; is no longer the guest's to take back.
                                 (not (plist-get candidate :delivering))))
                          entries)))
        (mevedel-session-set-pending-inputs
         session 'follow-up (delq entry entries))
        (dolist (path (plist-get entry :guest-paths))
          (when (file-exists-p path)
            (ignore-errors (delete-file path))))
        (when-let* ((data-buffer (mevedel-collaboration--room-data-buffer
                                  room))
                    (view-buffer (buffer-local-value 'mevedel--view-buffer
                                                     data-buffer))
                    ((buffer-live-p view-buffer)))
          (with-current-buffer view-buffer
            (mevedel-view--interaction-rebuild)))
        (mevedel-collaboration--publish-queue room)))))

(defun mevedel-collaboration--handle-abort (room peer)
  "Abort the running request for writable guest PEER."
  (let ((guest (mevedel-collaboration--guest room peer)))
    (when (and guest (plist-get guest :writable))
      (when-let* ((data-buffer (mevedel-collaboration--room-data-buffer room)))
        (let ((view-buffer
               (buffer-local-value 'mevedel--view-buffer data-buffer)))
          (if (buffer-live-p view-buffer)
              (with-current-buffer view-buffer
                (mevedel-view-abort))
            (mevedel-view--abort-data-buffer data-buffer)))))))

(defun mevedel-collaboration--on-frame (data-buffer peer frame)
  "Dispatch decoded guest FRAME from PEER for DATA-BUFFER's room.

Failure isolation mirrors the gptel observers: a fault in guest input
handling stops the room instead of leaking into the session."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (condition-case nil
        (pcase (plist-get frame :t)
          ("hello" (mevedel-collaboration--handle-hello room peer frame))
          ((or "push-subscribe" "push-unsubscribe" "push-state")
           (mevedel-collaboration--handle-push-subscription
            room peer frame))
          ("prompt" (mevedel-collaboration--handle-prompt room peer frame))
          ("abort" (mevedel-collaboration--handle-abort room peer))
          ("fetch-agent"
           (mevedel-collaboration--handle-fetch-agent room peer frame))
          ("artifact-get"
           (mevedel-collaboration--handle-artifact-get room peer frame))
          ("retract" (mevedel-collaboration--handle-retract room peer frame))
          ("ui-response"
           (mevedel-collaboration--handle-ui-response room peer frame))
          ("set-mode"
           (mevedel-collaboration--handle-set-mode room peer frame))
          ("new-session"
           (mevedel-collaboration--handle-new-session room peer frame)))
      (error (mevedel-collaboration--observer-failure room)))))

(defun mevedel-collaboration--on-control (data-buffer event peer)
  "Handle relay control EVENT for PEER in DATA-BUFFER's room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (pcase event
      ;; A joined peer becomes a guest only through its hello frame.
      ('peer-joined nil)
      ('peer-left (remhash peer (plist-get room :guests))))))

(defun mevedel-collaboration--on-state (data-buffer state)
  "Track relay transport STATE for DATA-BUFFER's room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (pcase state
      ;; The relay garbage-collects the room with the host connection, so
      ;; a drop invalidated every guest; they rejoin and re-hello against
      ;; the re-created room.
      ('down
       (clrhash (plist-get room :guests))
       ;; The links and QR are handed out before the async dial settles.
       ;; A dial that has never succeeded -- wrong relay URL or a missing or
       ;; stale configured host token -- would otherwise retry forever
       ;; with the user none the wiser that the share is dead.
       (unless (or (plist-get room :was-open)
                   (plist-get room :dial-warned))
         (setq room (plist-put room :dial-warned t))
         (display-warning
          'mevedel
          (concat "Collaboration relay dial failing; the share is not "
                  "live. Check `mevedel-collaboration-relay-url' and "
                  "the relay's optional host-token configuration.")
          :warning)))
      ('open
       (setq room (plist-put room :was-open t))
       (mevedel-collaboration--restore-push-subscriptions room))
      ('stopped nil))))

;; The dispatch above is the cold feature boundary for the two read-only
;; guest extensions.  Neither extension requires this module back.
(require 'mevedel-collaboration-agent)
(require 'mevedel-collaboration-artifact)

(provide 'mevedel-collaboration-guest)
;;; mevedel-collaboration-guest.el ends here
