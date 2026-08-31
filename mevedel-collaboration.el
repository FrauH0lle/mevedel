;;; mevedel-collaboration.el --- live browser collaboration -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns per-session rooms, publication, public commands, and gptel lifecycle
;; hooks.  Canonical projection lives in the projection module; untrusted
;; guest frames live in the guest module; agent and artifact sharing live in
;; their feature modules; the sealed relay client lives in the transport
;; module.
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
(defvar gptel-model)
(defvar gptel-post-tool-call-functions)
(defvar gptel-pre-tool-call-functions)

;; `mevedel-chat'
(defvar mevedel-session-end-hook)

;; `mevedel-collaboration-agent'
(declare-function mevedel-collaboration--publish-agents
                  "mevedel-collaboration-agent" (room))

;; `mevedel-collaboration-artifact-projection'
(declare-function mevedel-collaboration--artifact-stat-invalidate
                  "mevedel-collaboration-artifact-projection" ())

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--on-control
                  "mevedel-collaboration-guest" (data-buffer event peer))
(declare-function mevedel-collaboration--on-frame
                  "mevedel-collaboration-guest" (data-buffer peer frame))
(declare-function mevedel-collaboration--on-prompt-created
                  "mevedel-collaboration-guest" (overlay))
(declare-function mevedel-collaboration--on-prompt-settled
                  "mevedel-collaboration-guest" (overlay))
(declare-function mevedel-collaboration--on-state
                  "mevedel-collaboration-guest" (data-buffer state))

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection" (data-buffer))
(declare-function mevedel-collaboration--json-record
                  "mevedel-collaboration-projection" (record))
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

;; `mevedel-collaboration-share'
(declare-function mevedel-collaboration-share-dismiss
                  "mevedel-collaboration-share" (room))
(declare-function mevedel-collaboration-share-present
                  "mevedel-collaboration-share" (room))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-control
                  "mevedel-collaboration-transport" (transport control))
(declare-function mevedel-collaboration--transport-open
                  "mevedel-collaboration-transport" (url key &rest callbacks))
(declare-function mevedel-collaboration--transport-open-p
                  "mevedel-collaboration-transport" (transport))
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(declare-function mevedel-collaboration--transport-stop
                  "mevedel-collaboration-transport" (transport))

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-effective
                  "mevedel-permission-mode"
                  (&optional session data-buffer surface-buffer))
(autoload 'mevedel-permission-mode-effective "mevedel-permission-mode")

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode-active-p "mevedel-plan-mode"
                  (&optional session))
(autoload 'mevedel-plan-mode-active-p "mevedel-plan-mode")

;; `mevedel-structs'
(declare-function mevedel-session-pending-follow-ups "mevedel-structs" (session))
(declare-function mevedel-session-pending-input-failure-paused
                  "mevedel-structs" (session))
(declare-function mevedel-session-pending-input-paused
                  "mevedel-structs" (session))
(declare-function mevedel-session-session-id "mevedel-structs" (session))

;; `mevedel-turn'
(defvar mevedel--current-request)

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
  "Token this host sends when creating a relay room.

When the relay has `-host-token' configured, this value must match it.  Leave
both unset for a tokenless localhost or test relay.  Public-facing relays
should use a token to keep strangers from opening rooms, holding idle
connections, or driving outbound Web Push.  Guests never send it: their
authority is the bearer link.  It travels as a handshake header rather than a
query parameter because reverse proxies log query strings."
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
  "Command and skill names a full-link guest may invoke as buttons.

A name is offered to write-token guests as a tappable chip and
validated against this same list when the invocation arrives and again
when the queued entry is delivered, so removing a name takes effect
immediately.  Write the bare name without a sigil: mevedel resolves
whether it is a local slash command or a skill and uses the right one.

Names in `mevedel-collaboration-unsafe-guest-commands\=' are refused
even when listed here.  Guest free text is never parsed for slash
commands regardless of this list; the typed frame is the only
invocation surface a guest has."
  :type '(repeat string)
  :group 'mevedel)

(defconst mevedel-collaboration-unsafe-guest-commands
  '("mode" "model" "clear" "tools" "plugin" "skills" "prompt" "edits"
    "collab" "help" "btw")
  "Local slash commands a guest may never invoke, whatever the allowlist.

These either escalate authority (`mode\=' reaches full-auto, whose whole
point is skipping the permission prompts a guest would otherwise have
to be asked), mutate durable session state (`clear\='), manage the share
itself (`collab\='), or open a transient on the host\='s display that no
guest can see or dismiss.  Keeping the refusal here rather than in the
host\='s judgement means a mistaken allowlist entry cannot become an
escalation.")

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

(defun mevedel-collaboration--room-for-session (session)
  "Return the live room sharing SESSION, or nil."
  (when session
    (cl-find session (mevedel-collaboration--room-list)
             :key (lambda (room) (plist-get room :session)))))

(defun mevedel-collaboration-notify-queue-changed (session)
  "Re-publish SESSION\='s queue to its guests after a queue change.

Most queue changes ride a request, whose observers publish anyway.  A
local command drains without starting one, and a host-side edit starts
nothing at all, so without this seam the guest keeps a card for an
entry that is already gone."
  (when-let* ((room (mevedel-collaboration--room-for-session session)))
    (condition-case nil
        (progn
          (mevedel-collaboration--publish-queue room)
          (mevedel-collaboration--publish-status room))
      (error (mevedel-collaboration--observer-failure room)))))

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
      (mevedel-collaboration--publish-status room)
      (mevedel-collaboration--publish-agents room))))

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

(defun mevedel-collaboration--status-frame (room)
  "Return ROOM\='s guest-visible session status.

Busy drives the viewer\='s turn-finished notification; the model and
permission mode are what its status strip reports, the same two facts
the Emacs mode line carries."
  (let ((busy (mevedel-collaboration--busy-p room))
        (data-buffer (mevedel-collaboration--room-data-buffer room)))
    (list :t "status"
          :busy (if busy t :json-false)
          :mode (when-let* ((mode (ignore-errors
                                    (mevedel-permission-mode-effective
                                     (plist-get room :session)
                                     data-buffer))))
                  (format "%s" mode))
          ;; Plan is a mode the guest can enter from a button, so the
          ;; strip has to say when it is on.
          :plan (if (and (plist-get room :session)
                         (ignore-errors
                           (mevedel-plan-mode-active-p
                            (plist-get room :session))))
                    t :json-false)
          :model (when data-buffer
                   (when-let* ((model (buffer-local-value 'gptel-model
                                                          data-buffer)))
                     (format "%s" model))))))

(defun mevedel-collaboration--publish-status (room)
  "Broadcast ROOM\='s session status to its guests when it has changed."
  (let ((old (plist-get room :status))
        (status (mevedel-collaboration--status-frame room)))
    (unless (equal status (plist-get room :status))
      (setq room (plist-put room :status status))
      (mevedel-collaboration--broadcast room status)
      (when (and (eq t (plist-get old :busy))
                 (eq :json-false (plist-get status :busy)))
        (mevedel-collaboration--transport-control
         (plist-get room :transport) (list :t "push"))))))

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
;;; Room lifecycle

(defun mevedel-collaboration--stop-internal (room &optional reason)
  "Stop ROOM and all associated processes and timers."
  (when (and room
             (eq room (mevedel-collaboration--room-for-buffer
                       (plist-get room :data-buffer))))
    ;; Clear the authority before any teardown operation can signal.  The
    ;; local ROOM still supplies the transport and timers to close below.
    (remhash (plist-get room :data-buffer) mevedel-collaboration--rooms)
    (condition-case nil
        (mevedel-collaboration-share-dismiss room)
      (error nil))
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
  (require 'mevedel-collaboration-guest)
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
                        :push-guests (make-hash-table :test #'equal)
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

(defun mevedel-collaboration-view ()
  "Start live collaboration, or report the active room's links."
  (interactive)
  (require 'mevedel-collaboration-projection)
  (require 'mevedel-collaboration-share)
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
      (mevedel-collaboration-share-present room))
     ((not
       (yes-or-no-p
        (concat
         "Share visible prompts, responses, paths, and tool results, which "
         "may contain credentials or secrets, through the relay? Frames are "
         "sealed end to end; the links are bearer credentials. ")))
      (user-error "Collaboration not started"))
     (t
      (mevedel-collaboration-share-present
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
      ;; A settled ApplyPatch may have replaced a published artifact, whose
      ;; cached stat would otherwise keep the old size on its card.
      (when (equal (format "%s" (plist-get info :name)) "ApplyPatch")
        ;; Clear the small shared cache instead of translating the tool's
        ;; target-native paths a second time.  In particular, a remote patch
        ;; does not name the TRAMP-qualified keys stored by projection.
        (mevedel-collaboration--artifact-stat-invalidate))
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
