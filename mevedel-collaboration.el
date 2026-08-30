;;; mevedel-collaboration.el --- live browser collaboration -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the process-wide room, public commands, guest authority, and gptel
;; lifecycle hooks.  Canonical projection lives in the projection module;
;; the sealed relay client lives in the transport module.
;;
;; The host dials a self-hosted relay and never listens.  A share creates
;; one room with two bearer links: the view link carries the bare room key
;; and grants live read access; the full link appends a write token and
;; additionally grants prompting (through the ordinary pending-input queue)
;; and interrupting.  Everything that manipulates durable session state
;; stays host-only regardless of link strength.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel'
(defvar gptel-post-tool-call-functions)
(defvar gptel-pre-tool-call-functions)

;; `mevedel-chat'
(defvar mevedel-session-end-hook)

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection" (data-buffer))
(declare-function mevedel-collaboration--json-record
                  "mevedel-collaboration-projection" (record))
(declare-function mevedel-collaboration--json-string
                  "mevedel-collaboration-projection" (object))
(declare-function mevedel-collaboration--pending-tool-match
                  "mevedel-collaboration-projection" (info pending))
(declare-function mevedel-collaboration--project-records
                  "mevedel-collaboration-projection" (room))
(declare-function mevedel-collaboration--record
                  "mevedel-collaboration-projection" (id kind &rest fields))
(declare-function mevedel-collaboration--record-without-revision
                  "mevedel-collaboration-projection" (record))
(declare-function mevedel-collaboration--reuse-record-ids
                  "mevedel-collaboration-projection" (old new))
(declare-function mevedel-collaboration--stable-record-id
                  "mevedel-collaboration-projection"
                  (kind text &optional occurrence))
(declare-function mevedel-collaboration--tool-call-key
                  "mevedel-collaboration-projection" (info))
(declare-function mevedel-collaboration--tool-extras
                  "mevedel-collaboration-projection" (name args))
(declare-function mevedel-collaboration--tool-records
                  "mevedel-collaboration-projection" (records))
(declare-function mevedel-collaboration--tool-result-fields
                  "mevedel-collaboration-projection" (result))
(defvar mevedel-collaboration--protocol-version)

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-open
                  "mevedel-collaboration-transport" (url key &rest callbacks))
(declare-function mevedel-collaboration--transport-open-p
                  "mevedel-collaboration-transport" (transport))
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(declare-function mevedel-collaboration--transport-stop
                  "mevedel-collaboration-transport" (transport))
(defvar mevedel-collaboration--max-frame-json-bytes)
(defvar mevedel-collaboration--max-message-bytes)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))

;; `mevedel-pending-inputs'
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-pending-inputs"
                  (data-buffer text &rest keys))
(autoload 'mevedel-view-enqueue-external-follow-up "mevedel-pending-inputs")

;; `mevedel-structs'
(declare-function mevedel-directive-id "mevedel-structs" (record))
(declare-function mevedel-session-pending-follow-ups "mevedel-structs" (session))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(declare-function mevedel-session-pending-input-failure-paused
                  "mevedel-structs" (session))
(declare-function mevedel-session-pending-input-paused
                  "mevedel-structs" (session))
(declare-function mevedel-session-session-id "mevedel-structs" (session))
(declare-function mevedel-session-workspace "mevedel-structs" (session))
(declare-function mevedel-workspace-directives "mevedel-structs" (workspace))
(defvar mevedel--current-request)

;; `mevedel-view'
(declare-function mevedel-view--abort-data-buffer
                  "mevedel-view" (data-buffer))
(autoload 'mevedel-view--abort-data-buffer "mevedel-view")

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())
(autoload 'mevedel-view--interaction-rebuild "mevedel-view-interaction")

;; `mevedel-view-composer'
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(autoload 'mevedel-view-abort "mevedel-view-composer")

;; `mevedel-view-input-files'
(declare-function mevedel-view--media-dir "mevedel-view-input-files" ())
(autoload 'mevedel-view--media-dir "mevedel-view-input-files")

;; `qrencode'
(declare-function qrencode "ext:qrencode"
                  (s &optional mode errcorr return-raw))

;;
;;; Customization and state

(defcustom mevedel-collaboration-relay-url "ws://127.0.0.1:7466"
  "WebSocket origin of the collaboration relay.

The relay is the small self-hosted Go binary in the repository's
`relay' directory.  It is the only transport: local sharing runs the
same binary on localhost.  The value is a `ws://' or `wss://' origin
without a path, for example `wss://collab.example.net'."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-collaboration-relay-host-token nil
  "Token this host sends when creating a relay room, or nil for none.

A relay started with `-host-token' refuses to create a room without
it, which keeps strangers who discover the endpoint from opening rooms
and holding idle connections on the operator's server.  Guests never
send it: their authority is the bearer link.  It travels as a
handshake header rather than a query parameter because reverse proxies
log query strings."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Token"))
  :group 'mevedel)

(defcustom mevedel-collaboration-share-ttl 3600
  "Seconds after which an active share stops itself, or nil for no limit.

The room and both bearer links die with the share, so forgotten links
do not stay live.  The relay's max-room-age is only a backstop against
a crashed host; this timer is the policy."
  :type '(choice (const :tag "Until stopped" nil)
                 (integer :tag "Seconds"))
  :group 'mevedel)

(defcustom mevedel-collaboration-guest-skills nil
  "Skill and command names a full-link guest may invoke as buttons.

Each name is offered to write-token guests as a tappable chip and
validated against this same list when the typed skill frame arrives
and again when the queued invocation is delivered, so removing a name
takes effect immediately.  Names are slash lines without the slash:
\"plan\" runs what typing /plan in the composer would.

Guest free text is never parsed for slash commands regardless of this
list; the typed frame is the only skill surface a guest has."
  :type '(repeat string)
  :group 'mevedel)

(defcustom mevedel-collaboration-remote-interactions t
  "Whether full-link guests may answer pending interactions.

When non-nil, pending permission, Ask, and plan-approval interactions
are presented to write-token guests, and the first answer settles them.
When nil, full links are capped at prompting and interrupting."
  :type 'boolean
  :group 'mevedel)

(defconst mevedel-collaboration--publish-delay 0.1)
(defconst mevedel-collaboration--max-prompt-bytes (* 256 1024))
(defconst mevedel-collaboration--max-guest-name-chars 32)
(defconst mevedel-collaboration--max-prompt-attachments 3)
(defconst mevedel-collaboration--max-attachment-bytes (* 1280 1024)
  "Total decoded attachment bytes one guest prompt may carry.

The whole sealed prompt frame must clear the relay's 2 MiB read limit,
and the relay closes the connection that overruns it.  Base64 costs a
third, and the prompt text beside the attachments is worth 256 KiB of
the same frame, so the decoded budget has to leave room for both:
base64 of this budget plus a maximum-length prompt still lands about
85 KiB short of the limit.  Images are downscaled to fit; anything
else is refused, because a log cannot be made smaller by resampling.")
(defconst mevedel-collaboration--attachment-extensions
  '(("image/jpeg" . "jpg") ("image/png" . "png") ("image/webp" . "webp")
    ("application/pdf" . "pdf") ("text/plain" . "txt")
    ("text/markdown" . "md") ("text/csv" . "csv")
    ("application/json" . "json") ("text/x-patch" . "patch"))
  "Accepted guest attachment MIME types and their file extensions.

Extensions the Read tool treats as media -- pdf and the image types --
reach the model as media; the rest reach it as text.  Nothing here
comes from the guest: the saved name is host-generated, so a guest
filename can never steer a write.")
(defconst mevedel-collaboration--duplicate-prompt-window 3.0
  "Seconds within which an identical prompt from one guest is dropped.
A human re-sending the same text this fast is a double-fired client
event (double click, stale viewer), not a second question.")

(defvar mevedel-collaboration--rooms (make-hash-table :test #'eq)
  "Live collaboration rooms, keyed by their owning data buffer.
Each shared session has its own room, key, bearer links, TTL, and
guest set; nothing about one room reaches another room's guests.")

;;
;;; Small data helpers

(defun mevedel-collaboration--room-for-buffer (data-buffer)
  "Return the live room owned by DATA-BUFFER, or nil."
  (and (bufferp data-buffer)
       (gethash data-buffer mevedel-collaboration--rooms)))

(defun mevedel-collaboration--room-list ()
  "Return every live collaboration room."
  (let (rooms)
    (maphash (lambda (_buffer room) (push room rooms))
             mevedel-collaboration--rooms)
    (nreverse rooms)))

(defun mevedel-collaboration--room-for-overlay (overlay)
  "Return the room an interaction OVERLAY belongs to, or nil.
Interactions render into a session's view buffer, whose
`mevedel--data-buffer' names the owning data buffer.  A /btw side
conversation renders prompts in its own side view; those belong to the
parent session's room, reached through the side data buffer's parent."
  (when-let* ((buffer (overlay-buffer overlay))
              ((buffer-live-p buffer)))
    (or (mevedel-collaboration--room-for-buffer buffer)
        (when-let* ((data-buffer
                     (and (local-variable-p 'mevedel--data-buffer buffer)
                          (buffer-local-value 'mevedel--data-buffer buffer)))
                    ((buffer-live-p data-buffer)))
          (or (mevedel-collaboration--room-for-buffer data-buffer)
              (when-let* (((boundp
                            'mevedel-side-conversation--parent-buffer))
                          (parent (buffer-local-value
                                   'mevedel-side-conversation--parent-buffer
                                   data-buffer)))
                (mevedel-collaboration--room-for-buffer parent)))))))

(defun mevedel-collaboration--room-data-buffer (room)
  "Return the live data buffer for ROOM, or nil."
  (let ((buffer (plist-get room :data-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun mevedel-collaboration--current-data-buffer ()
  "Return the data buffer associated with the current command context."
  (cond
   ((and (boundp 'mevedel--data-buffer)
         (buffer-live-p mevedel--data-buffer))
    mevedel--data-buffer)
   ((and (boundp 'mevedel--session)
         mevedel--session
         (local-variable-p 'mevedel--session)
         (not (local-variable-p 'mevedel--view-buffer)))
    (current-buffer))
   ((and (boundp 'mevedel--view-buffer)
         (buffer-live-p mevedel--view-buffer))
    (with-current-buffer mevedel--view-buffer
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           mevedel--data-buffer)))))

(defun mevedel-collaboration--session-label (session data-buffer)
  "Return a safe display label for SESSION and DATA-BUFFER."
  (or (and (fboundp 'mevedel-session-session-id)
           session
           (mevedel-session-session-id session))
      (and (buffer-live-p data-buffer) (buffer-name data-buffer))
      "session"))

(defun mevedel-collaboration--random-bytes (count)
  "Return COUNT bytes from the operating system random source."
  (condition-case error-data
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally "/dev/urandom" nil nil count)
        (let ((bytes (buffer-string)))
          (if (= (string-bytes bytes) count)
              bytes
            (error "Short read from OS random source"))))
    (error
     (user-error "Cannot start collaboration: OS random source unavailable (%s)"
                 (error-message-string error-data)))))

(defun mevedel-collaboration--base64url (bytes)
  "Return unpadded base64url encoding of unibyte BYTES."
  (replace-regexp-in-string
   "=+$" ""
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string
     "\\+" "-"
     (base64-encode-string bytes t)))))

(defun mevedel-collaboration--base64url-decode (text)
  "Decode unpadded base64url TEXT to unibyte bytes, or nil when malformed."
  (when (stringp text)
    (let* ((standard (replace-regexp-in-string
                      "_" "/"
                      (replace-regexp-in-string "-" "+" text)))
           (padded (concat standard
                           (make-string (mod (- (length standard)) 4) ?=))))
      (condition-case nil
          (base64-decode-string padded)
        (error nil)))))

(defun mevedel-collaboration--host-headers ()
  "Return the extra handshake headers for this host's relay dial."
  (when (and (stringp mevedel-collaboration-relay-host-token)
             (not (string-empty-p mevedel-collaboration-relay-host-token)))
    (list (cons "X-Mevedel-Host-Token"
                mevedel-collaboration-relay-host-token))))

(defun mevedel-collaboration--relay-origins ()
  "Return (WS-ORIGIN . WEB-ORIGIN) from the configured relay URL.
Signal `user-error' when the configured value is not a ws origin."
  (let ((value (string-trim-right
                (or mevedel-collaboration-relay-url "") "/")))
    (unless (string-match "\\`\\(wss?\\)://\\([^/#?]+\\)\\'" value)
      (user-error
       "'mevedel-collaboration-relay-url' must be a ws:// or wss:// origin"))
    (cons value
          (concat (if (equal (match-string 1 value) "wss") "https" "http")
                  "://" (match-string 2 value)))))

;;
;;; Guest registry

(defun mevedel-collaboration--guest (room peer)
  "Return the registered guest plist for PEER in ROOM, or nil."
  (when-let* ((guests (plist-get room :guests)))
    (gethash peer guests)))

(defun mevedel-collaboration--sanitize-guest-name (name)
  "Return a bounded, control-character-free display NAME.
The name is display-only everywhere and never enters model context."
  (let* ((name (if (stringp name) name ""))
         (clean (replace-regexp-in-string "[[:cntrl:]]+" " " name))
         (clean (string-trim clean)))
    (cond ((string-empty-p clean) "guest")
          ((> (length clean) mevedel-collaboration--max-guest-name-chars)
           (substring clean 0 mevedel-collaboration--max-guest-name-chars))
          (t clean))))

(defun mevedel-collaboration--sanitize-guest-id (value)
  "Return VALUE when it is a well-formed stable guest id, else nil.
The viewer mints one random id per browser so a guest's own queued
entries survive reconnects; peer numbers do not, so they cannot be the
identity.  The id never enters model context or the transcript."
  (and (stringp value)
       (string-match-p "\\`[A-Za-z0-9_-]\\{8,64\\}\\'" value)
       value))

;;
;;; Room publication

(defun mevedel-collaboration--broadcast (room frame)
  "Broadcast FRAME to every guest in ROOM when the transport is live."
  (when-let* ((transport (plist-get room :transport))
             (guests (plist-get room :guests)))
    (when (> (hash-table-count guests) 0)
      (mevedel-collaboration--transport-send transport 0 frame))))

(defun mevedel-collaboration--publish (room)
  "Publish changed records from ROOM to its connected guests."
  (when room
    (let* ((old (plist-get room :records))
           (new (mevedel-collaboration--reuse-record-ids
                 old (mevedel-collaboration--project-records room)))
           (old-by-id (make-hash-table :test #'equal))
           (new-by-id (make-hash-table :test #'equal))
           changed removed)
      (dolist (record old)
        (puthash (plist-get record :id) record old-by-id))
      (dolist (record new)
        (let* ((id (plist-get record :id))
               (previous (gethash id old-by-id))
               (revision (if previous
                             (or (plist-get previous :revision) 0)
                           0)))
          (unless (and previous
                       (equal (mevedel-collaboration--record-without-revision
                               previous)
                              (mevedel-collaboration--record-without-revision
                               record)))
            (setq revision (1+ revision))
            (push (setq record (plist-put record :revision revision)) changed))
          (puthash id record new-by-id)))
      (dolist (record old)
        (unless (gethash (plist-get record :id) new-by-id)
          (push (plist-get record :id) removed)))
      (setq changed (nreverse changed)
            removed (nreverse removed))
      (setq new (mapcar (lambda (record)
                          (or (gethash (plist-get record :id) new-by-id)
                              record))
                        new))
      (setq room (plist-put room :records new))
      (dolist (record changed)
        (mevedel-collaboration--broadcast
         room (list :t "record"
                    :record (mevedel-collaboration--json-record record))))
      (when removed
        (mevedel-collaboration--broadcast
         room (list :t "remove" :ids (vconcat removed))))
      (mevedel-collaboration--publish-queue room)
      (mevedel-collaboration--publish-status room))))

(defun mevedel-collaboration--queue-state (room)
  "Return ROOM's guest-visible pending queue state as a plist.

Only the count and the paused flag: a delivered prompt shows up in the
transcript on its own, and per-entry previews would hand every guest
every other guest's unsent text."
  (when-let* ((session (plist-get room :session)))
    (list :pending (length (mevedel-session-pending-follow-ups session))
          :paused (and (or (mevedel-session-pending-input-paused session)
                           (mevedel-session-pending-input-failure-paused
                            session))
                       t))))

(defun mevedel-collaboration--queue-position (room entry)
  "Return ENTRY's 1-based place in ROOM's pending follow-up queue.
Nil when the entry is no longer queued, which a drain between the
enqueue and this call makes possible."
  (when-let* ((session (plist-get room :session))
              (index (cl-position (plist-get entry :id)
                                  (mevedel-session-pending-follow-ups session)
                                  :key (lambda (candidate)
                                         (plist-get candidate :id))
                                  :test #'equal)))
    (1+ index)))

(defun mevedel-collaboration--guest-queue-state (room guest)
  "Return the queue state GUEST in ROOM should see, or nil.

The global part is the pending count and paused flag.  A guest with a
stable id additionally sees its own entries -- id, live position, and
its own text echoed back so a reloaded viewer can rebuild its card.
Only its own: another guest's unsent text never travels to it."
  (when-let* ((state (mevedel-collaboration--queue-state room))
              (session (plist-get room :session)))
    (let ((guest-id (plist-get guest :guest-id))
          (position 0)
          own)
      (when guest-id
        (dolist (entry (mevedel-session-pending-follow-ups session))
          (cl-incf position)
          (when (equal guest-id (plist-get entry :guest-id))
            (push (list :id (plist-get entry :id)
                        :position position
                        :text (or (plist-get entry :input) ""))
                  own))))
      (append state (when own (list :own (nreverse own)))))))

(defun mevedel-collaboration--send-queue-state (room peer guest &optional force)
  "Send PEER its queue state for ROOM when it changed, or on FORCE.
GUEST caches the last state sent so an unchanged queue costs nothing."
  (when-let* ((state (mevedel-collaboration--guest-queue-state room guest)))
    (when (or force (not (equal state (plist-get guest :queue-state))))
      (plist-put guest :queue-state state)
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (append (list :t "queue"
                     :pending (plist-get state :pending)
                     :paused (plist-get state :paused))
               (when-let* ((own (plist-get state :own)))
                 (list :own
                       (vconcat
                        (mapcar
                         (lambda (entry)
                           `(("id" . ,(plist-get entry :id))
                             ("position" . ,(plist-get entry :position))
                             ("text" . ,(plist-get entry :text))))
                         own)))))))))

(defun mevedel-collaboration--publish-queue (room)
  "Send ROOM's queue state to each registered guest when it changed.
Per-peer rather than broadcast, because a guest's frame carries that
guest's own pending entries."
  (when room
    (maphash (lambda (peer guest)
               (when (plist-get guest :ready)
                 (mevedel-collaboration--send-queue-state room peer guest)))
             (plist-get room :guests))))

(defun mevedel-collaboration--busy-p (room)
  "Return non-nil while ROOM's session has a running request."
  (when-let* ((data-buffer (mevedel-collaboration--room-data-buffer room)))
    (and (buffer-local-value 'mevedel--current-request data-buffer) t)))

(defun mevedel-collaboration--publish-status (room)
  "Broadcast ROOM's busy state to its guests when it has changed.
The busy true-to-false transition is what a hidden viewer tab turns
into its turn-finished notification."
  (let ((busy (mevedel-collaboration--busy-p room)))
    (unless (eq busy (plist-get room :busy))
      (setq room (plist-put room :busy busy))
      (mevedel-collaboration--broadcast
       room (list :t "status" :busy (if busy t :json-false))))))

(defun mevedel-collaboration--publish-timer (data-buffer)
  "Run the coalesced publication timer for DATA-BUFFER's room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (setq room (plist-put room :publish-timer nil))
    (mevedel-collaboration--publish room)))

(defun mevedel-collaboration--safe-accepted-prompt (data-buffer)
  "Publish DATA-BUFFER immediately after an accepted prompt is inserted.

This observer is failure-isolated so a collaboration viewer cannot block the
request or prompt transaction."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (condition-case nil
        (mevedel-collaboration--publish room)
      (error (mevedel-collaboration--observer-failure room))))
  nil)

(defun mevedel-collaboration--schedule-publish (room)
  "Coalesce assistant stream updates for ROOM."
  (when (and room (not (plist-get room :publish-timer)))
    (setq room
          (plist-put room :publish-timer
                     (run-at-time mevedel-collaboration--publish-delay nil
                                  #'mevedel-collaboration--publish-timer
                                  (plist-get room :data-buffer))))))

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

(defun mevedel-collaboration--snapshot-chunks (records)
  "Split RECORDS into lists of JSON records each under the wire bound.
The bound belongs to the frame that goes on the wire, not to the records
in it.  A record too large to travel in a frame of its own is dropped:
emitting a frame the relay must refuse costs the host connection, and the
relay collects the room with it, so one oversized record would end the
session for every guest."
  (let* ((overhead (mevedel-collaboration--snapshot-frame-overhead))
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
      ;; The host-curated skill roster is the guest's whole discovery
      ;; surface; a view link gets none, having no way to use it.
      (when (and (plist-get guest :writable)
                 mevedel-collaboration-guest-skills)
        (list :skills (vconcat mevedel-collaboration-guest-skills)))))
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
             (request-id
              (or (and interaction-id
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
                         nil))
                  (cl-incf mevedel-collaboration--ui-request-counter))))
        (puthash request-id overlay requests)
        (let ((frame (mevedel-collaboration--ui-request-frame
                      request-id overlay)))
          (dolist (peer (mevedel-collaboration--writable-peers room))
            (mevedel-collaboration--transport-send
             (plist-get room :transport) peer frame)))))))

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
  "Send ROOM's active ui-requests to writable guest PEER."
  (let ((requests (plist-get room :ui-requests))
        ids)
    (maphash (lambda (request-id _overlay) (push request-id ids)) requests)
    (dolist (request-id (sort ids #'<))
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (mevedel-collaboration--ui-request-frame
        request-id (gethash request-id requests))))))

(defun mevedel-collaboration--guest-text (value)
  "Return VALUE trimmed when it is text this host may act on, else nil.
A guest is untrusted, and every string one sends -- a prompt, a
questionnaire answer, interaction feedback -- reaches model-visible
context and the transcript the same way, so one budget covers them all."
  (and (stringp value)
       (<= (string-bytes value) mevedel-collaboration--max-prompt-bytes)
       (let ((trimmed (string-trim value)))
         (and (not (string-empty-p trimmed)) trimmed))))

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
      (when-let* ((overlay (gethash request-id (plist-get room :ui-requests))))
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
                 ;; A complete questionnaire response: every answer a
                 ;; nonblank string, submitted atomically.
                 ((and answers
                       (functionp answer-handler)
                       (listp answers)
                       (let ((trimmed
                              (mapcar #'mevedel-collaboration--guest-text
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
             (guest (list :name name :writable writable :ready t
                          :guest-id (mevedel-collaboration--sanitize-guest-id
                                     (plist-get frame :guestId)))))
        (puthash peer guest (plist-get room :guests))
        (mevedel-collaboration--send-snapshot room peer)
        ;; Queue and busy state travel only on change, so a joining
        ;; guest is told the current ones directly.
        (mevedel-collaboration--send-queue-state room peer guest t)
        (mevedel-collaboration--transport-send
         (plist-get room :transport) peer
         (list :t "status"
               :busy (if (mevedel-collaboration--busy-p room)
                         t :json-false)))
        (when (and writable mevedel-collaboration-remote-interactions)
          (mevedel-collaboration--send-ui-requests room peer))))))

(cl-defun mevedel-collaboration--handle-prompt (room peer frame)
  "Queue the prompt in FRAME from writable guest PEER as a follow-up.

The prompt enters the ordinary pending-input queue: delivered when the
session is idle, queued behind a running request, paused while the
Pending Inputs cockpit is open.  The guest name is attribution only and
never enters model-visible context."
  (let ((guest (mevedel-collaboration--guest room peer))
        (text (plist-get frame :text)))
    (when (and guest
               (plist-get guest :writable)
               (mevedel-collaboration--guest-text text))
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
            (now (float-time)))
        (when (and last
                   (not (plist-get frame :images))
                   (equal (car last) text)
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
              (plist-put guest :last-prompt (cons text now))
              (unwind-protect
                  (progn
                    (setq queued
                          (mevedel-view-enqueue-external-follow-up
                           data-buffer text
                           :guest-name (plist-get guest :name)
                           :guest-id (plist-get guest :guest-id)
                           :paths paths
                           :directive-id
                           (mevedel-collaboration--guest-directive-id
                            room frame)))
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
                          (list :position position))))
                      ;; The periodic publish only runs while a request
                      ;; streams, so an idle session would leave every
                      ;; other guest's count stale until one starts.
                      (mevedel-collaboration--publish-queue room)))
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

(defun mevedel-collaboration--handle-skill (room peer frame)
  "Queue the allowlisted skill FRAME names for writable guest PEER.

The name is validated against `mevedel-collaboration-guest-skills'
here and rechecked at delivery.  This typed frame is a guest's only
skill surface: free text is never parsed for slash commands, so a
pasted log line starting with a slash can never invoke anything."
  (let ((guest (mevedel-collaboration--guest room peer))
        (name (plist-get frame :name)))
    (when (and guest
               (plist-get guest :writable)
               (stringp name)
               (member name mevedel-collaboration-guest-skills))
      ;; A chip is even easier to double-fire than a submit button; the
      ;; prompt path's duplicate window covers the same event class.
      (let ((line (concat "/" name))
            (last (plist-get guest :last-prompt))
            (now (float-time)))
        (unless (and last
                     (equal (car last) line)
                     (< (- now (cdr last))
                        mevedel-collaboration--duplicate-prompt-window))
          (plist-put guest :last-prompt (cons line now))
          (let ((queued
                 (when-let* ((data-buffer
                              (mevedel-collaboration--room-data-buffer
                               room)))
                   (mevedel-view-enqueue-external-follow-up
                    data-buffer line
                    :guest-name (plist-get guest :name)
                    :guest-id (plist-get guest :guest-id)
                    :skill name))))
            (if (null queued)
                (plist-put guest :last-prompt nil)
              (mevedel-collaboration--transport-send
               (plist-get room :transport) peer
               (append
                (list :t "queued")
                (when-let* ((id (plist-get queued :id)))
                  (list :id id))
                (when-let* ((position
                             (mevedel-collaboration--queue-position
                              room queued)))
                  (list :position position))))
              (mevedel-collaboration--publish-queue room))))))))

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
          ("prompt" (mevedel-collaboration--handle-prompt room peer frame))
          ("abort" (mevedel-collaboration--handle-abort room peer))
          ("skill" (mevedel-collaboration--handle-skill room peer frame))
          ("retract" (mevedel-collaboration--handle-retract room peer frame))
          ("ui-response"
           (mevedel-collaboration--handle-ui-response room peer frame)))
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
       ;; A dial that has never succeeded -- wrong relay URL, missing or
       ;; stale host token answered 404 -- would otherwise retry forever
       ;; with the user none the wiser that the share is dead.
       (unless (or (plist-get room :was-open)
                   (plist-get room :dial-warned))
         (setq room (plist-put room :dial-warned t))
         (display-warning
          'mevedel
          (concat "Collaboration relay dial failing; the share is not "
                  "live. Check `mevedel-collaboration-relay-url' and "
                  "`mevedel-collaboration-relay-host-token'.")
          :warning)))
      ('open (setq room (plist-put room :was-open t)))
      ('stopped nil))))

;;
;;; Room lifecycle

(defun mevedel-collaboration--stop-internal (room &optional reason)
  "Stop ROOM and all associated processes and timers."
  (when (and room
             (eq room (mevedel-collaboration--room-for-buffer
                       (plist-get room :data-buffer))))
    ;; Clear the authority before any teardown operation can signal.  The
    ;; local ROOM still supplies the transport and timers to close below.
    (remhash (plist-get room :data-buffer) mevedel-collaboration--rooms)
    ;; The global hooks serve every room; they leave with the last one.
    (when (zerop (hash-table-count mevedel-collaboration--rooms))
      (remove-hook 'kill-emacs-hook #'mevedel-collaboration--stop-for-emacs)
      (remove-hook 'mevedel-interaction-prompt-created-hook
                   #'mevedel-collaboration--on-prompt-created)
      (remove-hook 'mevedel-interaction-prompt-settled-hook
                   #'mevedel-collaboration--on-prompt-settled))
    (when-let* ((data-buffer (plist-get room :data-buffer)))
      (when (buffer-live-p data-buffer)
        (with-current-buffer data-buffer
          (remove-hook 'gptel-pre-tool-call-functions
                       #'mevedel-collaboration--safe-pre-tool)
          (remove-hook 'gptel-post-tool-call-functions
                       #'mevedel-collaboration--safe-post-tool))))
    (dolist (key '(:publish-timer :ttl-timer))
      (when-let* ((timer (plist-get room key)))
        (cancel-timer timer)))
    (when-let* ((transport (plist-get room :transport)))
      (unless (eq reason 'emacs-exit)
        (condition-case nil
            (when (> (hash-table-count (plist-get room :guests)) 0)
              (mevedel-collaboration--transport-send
               transport 0 (list :t "bye" :reason (format "%s" reason))))
          (error nil)))
      (condition-case nil
          (mevedel-collaboration--transport-stop transport)
        (error nil)))))

(defun mevedel-collaboration--stop-for-buffer ()
  "Stop sharing when the owning data buffer is killed."
  (when-let* ((room (mevedel-collaboration--room-for-buffer
                     (current-buffer))))
    (mevedel-collaboration--stop-internal room 'data-buffer-killed)))

(defun mevedel-collaboration--stop-for-session ()
  "Stop sharing from a data buffer's SessionEnd hook."
  (mevedel-collaboration--stop-for-buffer))

(defun mevedel-collaboration--stop-for-emacs ()
  "Stop every share before Emacs exits."
  (dolist (room (mevedel-collaboration--room-list))
    (mevedel-collaboration--stop-internal room 'emacs-exit)))

(defun mevedel-collaboration--stop-for-ttl (data-buffer)
  "Stop DATA-BUFFER's share when its TTL expires."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (mevedel-collaboration--stop-internal room 'ttl-expired)
    (message "mevedel: collaboration share expired for %s"
             (plist-get room :session-label))))

(cl-defun mevedel-collaboration--start (session data-buffer)
  "Start a room for SESSION and DATA-BUFFER and return the room.

Each session gets its own room; sharing a second session never touches
the first.  The early return below needs the block a `cl-defun'
establishes; a plain `defun' would signal `no-catch' instead of
returning the live room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (cl-return-from mevedel-collaboration--start room))
  (pcase-let* ((`(,ws-origin . ,web-origin)
                (mevedel-collaboration--relay-origins))
               (room-id (mevedel-collaboration--base64url
                         (mevedel-collaboration--random-bytes 16)))
               (key (mevedel-collaboration--random-bytes 32))
               (write-token (mevedel-collaboration--random-bytes 16))
               (records (mevedel-collaboration--canonical-records
                         data-buffer)))
    (unless (require 'websocket nil t)
      (user-error "Collaboration requires the 'websocket' package; install it first"))
    (let (transport)
      (condition-case error-data
          (progn
            (setq transport
                  (mevedel-collaboration--transport-open
                   (format "%s/r/%s?role=host" ws-origin room-id)
                   key
                   :headers (mevedel-collaboration--host-headers)
                   :on-frame (lambda (peer frame)
                               (mevedel-collaboration--on-frame
                                data-buffer peer frame))
                   :on-control (lambda (event peer)
                                 (mevedel-collaboration--on-control
                                  data-buffer event peer))
                   :on-state (lambda (state)
                               (mevedel-collaboration--on-state
                                data-buffer state))))
            (puthash data-buffer
                  (list :transport transport
                        :session session
                        :data-buffer data-buffer
                        :session-label
                        (mevedel-collaboration--session-label
                         session data-buffer)
                        :room-id room-id
                        :key key
                        :write-token write-token
                        :link-view
                        (format "%s/#%s.%s" web-origin room-id
                                (mevedel-collaboration--base64url key))
                        :link-full
                        (format "%s/#%s.%s" web-origin room-id
                                (mevedel-collaboration--base64url
                                 (concat key write-token)))
                        :records records
                        :queue nil
                        :pending-tools nil
                        :tool-call-occurrences
                        (make-hash-table :test #'equal)
                        :guests (make-hash-table :test #'eql)
                        :ui-requests (make-hash-table :test #'eql)
                        :publish-timer nil
                        :ttl-timer
                        (when mevedel-collaboration-share-ttl
                          (run-at-time mevedel-collaboration-share-ttl nil
                                       #'mevedel-collaboration--stop-for-ttl
                                       data-buffer)))
                  mevedel-collaboration--rooms)
            (with-current-buffer data-buffer
              (add-hook 'kill-buffer-hook
                        #'mevedel-collaboration--stop-for-buffer nil t)
              (add-hook 'mevedel-session-end-hook
                        #'mevedel-collaboration--stop-for-session nil t)
              (add-hook 'gptel-pre-tool-call-functions
                        #'mevedel-collaboration--safe-pre-tool nil t)
              (add-hook 'gptel-post-tool-call-functions
                        #'mevedel-collaboration--safe-post-tool nil t))
            (add-hook 'kill-emacs-hook
                      #'mevedel-collaboration--stop-for-emacs)
            (add-hook 'mevedel-interaction-prompt-created-hook
                      #'mevedel-collaboration--on-prompt-created)
            (add-hook 'mevedel-interaction-prompt-settled-hook
                      #'mevedel-collaboration--on-prompt-settled)
            (mevedel-collaboration--room-for-buffer data-buffer))
        (error
         (condition-case nil
             (let ((room (mevedel-collaboration--room-for-buffer
                          data-buffer)))
               (if (and room (eq (plist-get room :transport) transport))
                   (mevedel-collaboration--stop-internal room 'start-failed)
                 (when transport
                   (mevedel-collaboration--transport-stop transport))))
           (error nil))
         (signal (car error-data) (cdr error-data)))))))

;;
;;; Share surface

(defvar-local mevedel-collaboration--share-room nil
  "Room whose bearer links this share buffer presents.")

(defvar-local mevedel-collaboration--share-which 'view
  "Which link's QR the share buffer shows: `view' or `full'.")

(defun mevedel-collaboration--share-content (room which)
  "Return the share buffer text for ROOM showing WHICH link's QR.

WHICH is `view' or `full'.  One QR at a time, the view link by
default: two codes side by side is how a colleague scans the wrong one
and walks away with write authority."
  (let* ((full (eq which 'full))
         (link (plist-get room (if full :link-full :link-view)))
         ;; The QR is the convenience; the link beneath it is the
         ;; payload.  An encoder that is missing or signals must cost
         ;; the code, never the share.
         (code (condition-case error-data
                   (progn (require 'qrencode)
                          (propertize (qrencode link) 'face '(:height 1.6)))
                 (error
                  (propertize
                   (format "QR unavailable (%s); copy the link below."
                           (error-message-string error-data))
                   'face 'shadow)))))
    (concat
     (propertize (format "Share: %s\n" (plist-get room :session-label))
                 'face 'bold)
     (if full
         (propertize
          "FULL CONTROL link — grants prompting, interrupting, answering\n"
          'face 'error)
       (propertize "View link — read-only\n" 'face 'success))
     "\n"
     ;; Scaled so a phone camera resolves the half-block modules from a
     ;; normal viewing distance.
     code
     "\n\n"
     link
     "\n\n"
     (propertize
      (concat "TAB show " (if full "view" "full control") " QR"
              "  ·  c copy view  ·  f copy full  ·  q close\n"
              "Links are bearer credentials; treat them like secrets.")
      'face 'shadow))))

(defun mevedel-collaboration--share-render ()
  "Repaint the current share buffer from its room and selection."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (mevedel-collaboration--share-content
             mevedel-collaboration--share-room
             mevedel-collaboration--share-which))
    (goto-char (point-min))))

(defun mevedel-collaboration-share-toggle ()
  "Show the other bearer link's QR."
  (interactive)
  (setq mevedel-collaboration--share-which
        (if (eq mevedel-collaboration--share-which 'full) 'view 'full))
  (mevedel-collaboration--share-render))

(defun mevedel-collaboration-share-copy-view ()
  "Copy the view link to the kill ring."
  (interactive)
  (kill-new (plist-get mevedel-collaboration--share-room :link-view))
  (message "mevedel: view link copied"))

(defun mevedel-collaboration-share-copy-full ()
  "Copy the full-control link to the kill ring."
  (interactive)
  (kill-new (plist-get mevedel-collaboration--share-room :link-full))
  (message "mevedel: full-control link copied"))

(defun mevedel-collaboration-share-quit ()
  "Close the share surface."
  (interactive)
  (if (frame-parent (selected-frame))
      (delete-frame)
    (quit-window t)))

(defvar-keymap mevedel-collaboration--share-map
  :doc "Keys available in the collaboration share buffer."
  "TAB" #'mevedel-collaboration-share-toggle
  "<tab>" #'mevedel-collaboration-share-toggle
  "c" #'mevedel-collaboration-share-copy-view
  "f" #'mevedel-collaboration-share-copy-full
  "q" #'mevedel-collaboration-share-quit)

(defun mevedel-collaboration--show-share-frame (room)
  "Present ROOM's bearer links and QR code on a dedicated surface.
A child frame on a graphical display, an ordinary window otherwise."
  (let ((buffer (get-buffer-create "*mevedel share*")))
    (with-current-buffer buffer
      (setq-local mevedel-collaboration--share-room room)
      (setq-local mevedel-collaboration--share-which 'view)
      (setq buffer-read-only t
            truncate-lines t
            cursor-type nil)
      (use-local-map mevedel-collaboration--share-map)
      (mevedel-collaboration--share-render))
    (if (display-graphic-p)
        (condition-case nil
            (when-let* ((window
                         (display-buffer
                          buffer
                          '((display-buffer-in-child-frame)
                            (child-frame-parameters
                             . ((minibuffer . nil)
                                (undecorated . t))))))
                        (frame (window-frame window)))
              (fit-frame-to-buffer frame)
              (select-frame-set-input-focus frame))
          ;; Creating a frame realizes every face for it, so a defect
          ;; entirely outside mevedel -- a theme whose face specs form
          ;; an inheritance cycle -- signals here.  The share must still
          ;; be presentable: fall back to an ordinary window.
          (error (pop-to-buffer buffer)))
      (pop-to-buffer buffer))))

(defun mevedel-collaboration--report-links (room)
  "Copy ROOM's full link and open the share surface with both links.
The links render there rather than in *Messages*, whose log is durable
and easy to leak."
  (kill-new (plist-get room :link-full))
  (mevedel-collaboration--show-share-frame room)
  (message "mevedel: full-control link copied to kill ring"))

(defun mevedel-collaboration-view ()
  "Start live collaboration, or report the active room's links."
  (interactive)
  (require 'mevedel-collaboration-projection)
  (require 'mevedel-collaboration-transport)
  (let* ((data-buffer (mevedel-collaboration--current-data-buffer))
         (session (and data-buffer
                       (with-current-buffer data-buffer
                         (and (boundp 'mevedel--session)
                              mevedel--session))))
         (room (mevedel-collaboration--room-for-buffer data-buffer)))
    (unless (and data-buffer session)
      (user-error "No active mevedel session in this buffer"))
    (cond
     (room
      (mevedel-collaboration--report-links room))
     ((not
       (yes-or-no-p
        (concat
         "Share visible prompts, responses, paths, and tool results, which "
         "may contain credentials or secrets, through the relay? Frames are "
         "sealed end to end; the links are bearer credentials. ")))
      (user-error "Collaboration not started"))
     (t
      (mevedel-collaboration--report-links
       (mevedel-collaboration--start session data-buffer))))))

(defun mevedel-collaboration-stop ()
  "Stop the current session's share, or every share outside a session.
Run from a session that is not shared, another session's share is
never touched: that is a report, not a teardown."
  (interactive)
  (let ((rooms (mevedel-collaboration--room-list))
        (data-buffer (mevedel-collaboration--current-data-buffer)))
    (cond
     ((null rooms)
      (message "mevedel: collaboration is not active"))
     ((when-let* ((room (mevedel-collaboration--room-for-buffer
                         data-buffer)))
        (mevedel-collaboration--stop-internal room 'user-stop)
        (message "mevedel: collaboration stopped for %s"
                 (plist-get room :session-label))
        t))
     (data-buffer
      (message "mevedel: no active share for this session; %d share%s live elsewhere"
               (length rooms) (if (= 1 (length rooms)) "" "s")))
     (t
      (dolist (room rooms)
        (mevedel-collaboration--stop-internal room 'user-stop))
      (message "mevedel: stopped %d collaboration share%s"
               (length rooms) (if (= 1 (length rooms)) "" "s"))))))

(defun mevedel-collaboration--room-status (room)
  "Return a status line for ROOM without exposing its secrets."
  (let ((transport (plist-get room :transport))
        (guests (plist-get room :guests))
        names)
    (maphash (lambda (_peer guest)
               (push (format "%s%s" (plist-get guest :name)
                             (if (plist-get guest :writable) "" " (view)"))
                     names))
             guests)
    (format "%s: relay %s; %s"
            (plist-get room :session-label)
            (if (mevedel-collaboration--transport-open-p transport)
                "connected" "reconnecting")
            (if names
                (format "guests: %s"
                        (mapconcat #'identity (nreverse names) ", "))
              "no guest connected"))))

(defun mevedel-collaboration-status ()
  "Report every active share's status without exposing its secrets."
  (interactive)
  (if-let* ((rooms (mevedel-collaboration--room-list)))
      (message "mevedel: collaboration active for %s"
               (mapconcat #'mevedel-collaboration--room-status rooms "; "))
    (message "mevedel: collaboration inactive")))


;;
;;; gptel and lifecycle hooks

(defun mevedel-collaboration--pre-tool (info)
  "Publish a running tool record for gptel tool-call INFO.
Runs from a buffer-local hook, so the current buffer names the room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer
                     (current-buffer))))
    (progn
      (let* ((name (format "%s" (plist-get info :name)))
             (call-key (mevedel-collaboration--tool-call-key info))
             (pending (plist-get room :pending-tools))
             existing)
        (dolist (entry pending)
          (when (and (null existing)
                     (equal (plist-get entry :call-key) call-key)
                     (equal (plist-get entry :status) "running"))
            (setq existing entry)))
        (unless existing
          (let* ((canonical
                  (mevedel-collaboration--canonical-records
                   (plist-get room :data-buffer)))
                 (occurrences (plist-get room :tool-call-occurrences))
                 (occurrence (gethash call-key occurrences 0))
                 (explicit-id (or (plist-get info :id)
                                  (plist-get info :call-id)
                                  (plist-get info :tool-call-id)
                                  (plist-get info :tool_call_id)))
                 (id (if explicit-id
                         (format "tool-%s" explicit-id)
                       (mevedel-collaboration--stable-record-id
                        "tool" call-key occurrence)))
                 (entry (apply
                         #'mevedel-collaboration--record
                         id "tool"
                         :revision 0
                         :name name
                         :status "running"
                         :summary name
                         :result ""
                         :truncated nil
                         :pending t
                         :identity-fixed t
                         :call-key call-key
                         :baseline-tool-count
                         (length (mevedel-collaboration--tool-records canonical))
                         :baseline-record-count (length canonical)
                         (mevedel-collaboration--tool-extras
                          name (plist-get info :args)))))
            (puthash call-key (1+ occurrence) occurrences)
            (setq room (plist-put room :pending-tools
                                  (append pending (list entry))))))
        ;; Tool start is intentionally published immediately.  A short tool
        ;; still has a truthful completion transition, while a long tool is
        ;; visible as running before it produces a result.
        (mevedel-collaboration--publish room))))
  nil)

(defun mevedel-collaboration--post-tool (info)
  "Publish the settled result for gptel tool-call INFO.
Runs from a buffer-local hook, so the current buffer names the room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer
                     (current-buffer))))
    (progn
      (let* ((pending (plist-get room :pending-tools))
             entry)
        (dolist (candidate pending)
          (when (and (null entry)
                     (equal (plist-get candidate :status) "running")
                     (mevedel-collaboration--pending-tool-match
                      info candidate))
            (setq entry candidate)))
        (when entry
          (let ((fields (mevedel-collaboration--tool-result-fields
                         (plist-get info :result))))
            (dolist (key '(:status :result :truncated))
              (setf (plist-get entry key) (plist-get fields key)))
            (mevedel-collaboration--publish room))))))
  nil)

(defun mevedel-collaboration--safe-pre-tool (info)
  "Run the live tool-start observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--pre-tool info)
    (error (mevedel-collaboration--observer-failure
            (mevedel-collaboration--room-for-buffer (current-buffer)))))
  nil)

(defun mevedel-collaboration--safe-post-tool (info)
  "Run the live tool-settlement observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-tool info)
    (error (mevedel-collaboration--observer-failure
            (mevedel-collaboration--room-for-buffer (current-buffer)))))
  nil)

(defun mevedel-collaboration--observer-failure (room)
  "Stop ROOM after an observer failure without affecting the request."
  (when room
    (condition-case nil
        (mevedel-collaboration--stop-internal room 'observer-failure)
      (error nil)))
  (condition-case nil
      (display-warning
       'mevedel "Live collaboration stopped after an observer failure" :warning)
    (error nil)))

(defun mevedel-collaboration--post-stream ()
  "Schedule a coalesced publication after gptel inserts response text."
  (when-let* ((room (mevedel-collaboration--room-for-buffer
                     (current-buffer))))
    (mevedel-collaboration--schedule-publish room)))

(defun mevedel-collaboration--post-response (_start _end)
  "Publish the settled response for the active data buffer."
  (when-let* ((room (mevedel-collaboration--room-for-buffer
                     (current-buffer))))
    (mevedel-collaboration--schedule-publish room)))

(defun mevedel-collaboration--safe-post-stream ()
  "Run the stream observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-stream)
    (error (mevedel-collaboration--observer-failure
            (mevedel-collaboration--room-for-buffer (current-buffer))))))

(defun mevedel-collaboration--safe-post-response (start end)
  "Run the response observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-response start end)
    (error (mevedel-collaboration--observer-failure
            (mevedel-collaboration--room-for-buffer (current-buffer))))))

(provide 'mevedel-collaboration)
;;; mevedel-collaboration.el ends here
