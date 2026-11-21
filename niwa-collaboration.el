;;; niwa-collaboration.el --- live browser collaboration -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns per-session rooms, publication, public commands, and gptel lifecycle
;; hooks.  Canonical projection lives in the projection module; untrusted
;; guest frames live in the guest module; agent and artifact sharing live in
;; their feature modules; the sealed relay client lives in the transport
;; module.
;;
;; The host dials a self-hosted relay and never listens.  A share creates
;; one room with three bearer links, each a prefix of the next: the view
;; link carries the bare room key and grants live read access; the full
;; link appends a write token and additionally grants prompting (through
;; the ordinary pending-input queue) and interrupting; the owner link
;; appends an owner token and additionally grants the two authorities a
;; host would otherwise have to be at the keyboard for -- changing the
;; permission mode and creating a session.  Everything else that
;; manipulates durable session state stays host-only regardless of link
;; strength.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'niwa-recovery)
(require 'niwa-utilities)

;; `gptel'
(defvar gptel-model)
(defvar gptel-post-tool-call-functions)
(defvar gptel-pre-tool-call-functions)

;; `browse-url'
(declare-function browse-url "browse-url" (url &rest args))

;; `niwa-chat'
(defvar niwa-session-end-hook)
(defvar niwa-session-end-reason)

;; `niwa-collaboration-agent'
(declare-function niwa-collaboration--publish-agents
                  "niwa-collaboration-agent" (room))
(autoload 'niwa-collaboration--publish-agents "niwa-collaboration-agent")

;; `niwa-collaboration-artifact'
(declare-function niwa-collaboration-item-link
                  "niwa-collaboration-artifact" (link id &optional route))
(autoload 'niwa-collaboration-item-link "niwa-collaboration-artifact")

;; `niwa-collaboration-artifact-projection'
(declare-function niwa-collaboration--artifact-stat-invalidate
                  "niwa-collaboration-artifact-projection" ())
(autoload 'niwa-collaboration--artifact-stat-invalidate "niwa-collaboration-artifact-projection")

;; `niwa-collaboration-background'
(declare-function niwa-collaboration--publish-background
                  "niwa-collaboration-background" (room))
(autoload 'niwa-collaboration--publish-background "niwa-collaboration-background")

;; `niwa-collaboration-editing'
(declare-function niwa-collaboration-editing-depart
                  "niwa-collaboration-editing" (room peer))
(autoload 'niwa-collaboration-editing-depart "niwa-collaboration-editing")

;; `niwa-collaboration-goal'
(declare-function niwa-collaboration--publish-goal
                  "niwa-collaboration-goal" (room))
(autoload 'niwa-collaboration--publish-goal "niwa-collaboration-goal")

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--on-control
                  "niwa-collaboration-guest" (data-buffer event peer))
(declare-function niwa-collaboration--on-frame
                  "niwa-collaboration-guest" (data-buffer peer frame))
(declare-function niwa-collaboration--on-prompt-created
                  "niwa-collaboration-guest" (overlay))
(declare-function niwa-collaboration--on-prompt-settled
                  "niwa-collaboration-guest" (overlay))
(declare-function niwa-collaboration--on-state
                  "niwa-collaboration-guest" (data-buffer state))
(declare-function niwa-collaboration--push-audience
                  "niwa-collaboration-guest" (session room audience))
(autoload 'niwa-collaboration--push-audience "niwa-collaboration-guest")
(autoload 'niwa-collaboration--on-control "niwa-collaboration-guest")
(autoload 'niwa-collaboration--on-frame "niwa-collaboration-guest")
(autoload 'niwa-collaboration--on-prompt-created "niwa-collaboration-guest")
(autoload 'niwa-collaboration--on-prompt-settled "niwa-collaboration-guest")
(autoload 'niwa-collaboration--on-state "niwa-collaboration-guest")

;; `niwa-collaboration-history'
(declare-function niwa-collaboration--publish-history "niwa-collaboration-history" (room &optional peer))
(autoload 'niwa-collaboration--publish-history "niwa-collaboration-history")

;; `niwa-collaboration-lobby'
(declare-function niwa-collaboration-lobby--status
                  "niwa-collaboration-lobby" ())
(declare-function niwa-collaboration-lobby-shares
                  "niwa-collaboration-lobby" ())
(declare-function niwa-collaboration-lobby-show-directory
                  "niwa-collaboration-lobby" (root &optional restart))
(declare-function niwa-collaboration-lobby-stop-directory
                  "niwa-collaboration-lobby" (root))
(declare-function niwa-collaboration-lobby-stop-every
                  "niwa-collaboration-lobby" ())
(defvar niwa-collaboration-lobby--lobbies)

;; `niwa-collaboration-presence'
(declare-function niwa-collaboration-presence-publish
                  "niwa-collaboration-presence" (room))

;; `niwa-collaboration-projection'
(declare-function niwa-collaboration--canonical-records
                  "niwa-collaboration-projection"
                  (data-buffer &optional completion-buffer completions cache own-path))
(declare-function niwa-collaboration--json-record
                  "niwa-collaboration-projection" (record))
(declare-function niwa-collaboration--pending-tool-match
                  "niwa-collaboration-projection" (info pending))
(declare-function niwa-collaboration--project-records
                  "niwa-collaboration-projection" (room))
(declare-function niwa-collaboration--projection-cache-create
                  "niwa-collaboration-projection" (&rest slots))
(declare-function niwa-collaboration--record
                  "niwa-collaboration-projection" (id kind &rest fields))
(declare-function niwa-collaboration--record-without-revision
                  "niwa-collaboration-projection" (record))
(declare-function niwa-collaboration--room-canonical-records
                  "niwa-collaboration-projection" (room))
(declare-function niwa-collaboration--routine-poll-p
                  "niwa-collaboration-projection" (name args data status))
(declare-function niwa-collaboration--reuse-record-ids
                  "niwa-collaboration-projection" (old new))
(declare-function niwa-collaboration--stable-record-id
                  "niwa-collaboration-projection"
                  (kind text &optional occurrence))
(declare-function niwa-collaboration--tool-call-key
                  "niwa-collaboration-projection" (info))
(declare-function niwa-collaboration--tool-extras
                  "niwa-collaboration-projection" (name args))
(declare-function niwa-collaboration--tool-records
                  "niwa-collaboration-projection" (records))
(declare-function niwa-collaboration--tool-result-fields
                  "niwa-collaboration-projection" (result))
(declare-function niwa-collaboration-tool-presentation
                  "niwa-collaboration-projection" (parsed))
(autoload 'niwa-collaboration--canonical-records "niwa-collaboration-projection")
(autoload 'niwa-collaboration--json-record "niwa-collaboration-projection")
(autoload 'niwa-collaboration--pending-tool-match "niwa-collaboration-projection")
(autoload 'niwa-collaboration--project-records "niwa-collaboration-projection")
(autoload 'niwa-collaboration--projection-cache-create "niwa-collaboration-projection")
(autoload 'niwa-collaboration--record "niwa-collaboration-projection")
(autoload 'niwa-collaboration--record-without-revision "niwa-collaboration-projection")
(autoload 'niwa-collaboration--reuse-record-ids "niwa-collaboration-projection")
(autoload 'niwa-collaboration--room-canonical-records "niwa-collaboration-projection")
(autoload 'niwa-collaboration--routine-poll-p "niwa-collaboration-projection")
(autoload 'niwa-collaboration--stable-record-id "niwa-collaboration-projection")
(autoload 'niwa-collaboration--tool-call-key "niwa-collaboration-projection")
(autoload 'niwa-collaboration--tool-extras "niwa-collaboration-projection")
(autoload 'niwa-collaboration--tool-records "niwa-collaboration-projection")
(autoload 'niwa-collaboration--tool-result-fields "niwa-collaboration-projection")

;; `niwa-collaboration-share'
(declare-function niwa-collaboration-share-dismiss
                  "niwa-collaboration-share" (room))
(declare-function niwa-collaboration-share-present
                  "niwa-collaboration-share" (room))

;; `niwa-collaboration-recovery'
(declare-function niwa-collaboration-recovery-send
                  "niwa-collaboration-recovery" (room peer))
(autoload 'niwa-collaboration-recovery-send "niwa-collaboration-recovery")

;; `niwa-collaboration-task'
(declare-function niwa-collaboration--publish-tasks
                  "niwa-collaboration-task" (room))
(autoload 'niwa-collaboration--publish-tasks "niwa-collaboration-task")

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-open
                  "niwa-collaboration-transport" (url key &rest callbacks))
(declare-function niwa-collaboration--transport-open-p
                  "niwa-collaboration-transport" (transport))
(declare-function niwa-collaboration--transport-send
                  "niwa-collaboration-transport" (transport peer frame))
(declare-function niwa-collaboration--transport-stop
                  "niwa-collaboration-transport" (transport))
(autoload 'niwa-collaboration--transport-open "niwa-collaboration-transport")
(autoload 'niwa-collaboration--transport-open-p "niwa-collaboration-transport")
(autoload 'niwa-collaboration--transport-send "niwa-collaboration-transport")
(autoload 'niwa-collaboration--transport-stop "niwa-collaboration-transport")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-plan-mode'
(declare-function niwa-plan-mode-active-p "niwa-plan-mode"
                  (&optional session))
(autoload 'niwa-plan-mode-active-p "niwa-plan-mode")

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer-register-observer
                  "niwa-session-control-transfer" (session observer))
(declare-function niwa-session-control-transfer-unregister-observer
                  "niwa-session-control-transfer" (session observer))
(autoload 'niwa-session-control-transfer-register-observer
  "niwa-session-control-transfer")
(autoload 'niwa-session-control-transfer-unregister-observer
  "niwa-session-control-transfer")

;; `niwa-sleep'
(declare-function niwa-sleep-update "niwa-sleep" (&rest _))
(autoload 'niwa-sleep-update "niwa-sleep")

;; `niwa-structs'
(declare-function niwa-session-name "niwa-structs" (session))
(declare-function niwa-session-pending-follow-ups "niwa-structs" (session))
(declare-function niwa-session-pending-input-failure-paused
                  "niwa-structs" (session))
(declare-function niwa-session-pending-input-paused
                  "niwa-structs" (session))
(declare-function niwa-session-session-id "niwa-structs" (session))

;; `niwa-transport'
(declare-function niwa-transport-run-at-time "niwa-transport" (seconds function &rest args))
(autoload 'niwa-transport-run-at-time "niwa-transport")

;; `niwa-utilities'
(declare-function niwa--ui-timer-cancel "niwa-utilities" (timer))
(declare-function niwa--ui-timer-pending-p "niwa-utilities" (timer))
(autoload 'niwa--ui-timer-cancel "niwa-utilities")
(autoload 'niwa--ui-timer-pending-p "niwa-utilities")

;; `niwa-turn'
(defvar niwa--current-request)

;; `niwa-view-interaction'
(defvar niwa-view--interaction-lifecycle)
(defvar niwa-view--interaction-overlays)

;;
;;; Customization and state

(defcustom niwa-collaboration-relay-url "ws://127.0.0.1:7466"
  "WebSocket origin of the collaboration relay.

The relay is the small self-hosted Go binary in the repository's
`relay' directory.  It is the only transport: local sharing runs the
same binary on localhost.  The value is a `ws://' or `wss://' origin
without a path, for example `wss://collab.example.net'."
  :type 'string
  :group 'niwa)

(defcustom niwa-collaboration-relay-host-token nil
  "Token this host sends when creating a relay room.

When the relay has `-host-token' configured, this value must match it.  Leave
both unset for a tokenless localhost or test relay.  Public-facing relays
should use a token to keep strangers from opening rooms, holding idle
connections, or driving outbound Web Push.  Guests never send it: their
authority is the bearer link.  It travels as a handshake header rather than a
query parameter because reverse proxies log query strings."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Token"))
  :group 'niwa)

(defcustom niwa-collaboration-keep-awake-idle-time 7200
  "Seconds an empty share or lobby requests that the host stay awake.
The interval starts when sharing starts or the last guest leaves,
whichever is later.  Connected guests always request keep-awake.
Nil means no idle cutoff: request keep-awake until sharing stops.
This does not close the share or override the system inhibitor setting."
  :type '(choice (const :tag "No idle cutoff" nil)
                 (natnum :tag "Idle seconds"))
  :group 'niwa)

(defcustom niwa-collaboration-guest-skills '((owner . t))
  "Commands and skills a writable guest may invoke, by link tier.

An alist keyed by the symbols `full\=' and `owner\='.  Each value follows
the shape of `global-minor-mode\=' mode lists: t admits every local slash
command and user-invocable skill, nil admits none, and a list is read
left to right until an element decides -- a bare name admits that name,
`(not NAME...)\=' refuses those names, t admits, nil refuses, and an
implicit nil ends the list.  For instance

  ((full \"plan\" \"goal\" \"review\" \"verify\")
   (owner (not \"learn\" \"remember\") t))

offers a full-link guest four buttons and an owner-link guest everything
but the two memory-writing skills.  An owner link with no entry of its
own uses the `full\=' entry.  Write bare names without a sigil: niwa
resolves whether a name is a local slash command or a skill and uses the
right one, commands first.

The admitted names are offered to the guest as tappable chips in the
welcome, validated against this same value when an invocation arrives,
and again when the queued entry is delivered, so a change takes effect
immediately.  Names in `niwa-collaboration-unsafe-guest-commands\='
are refused even when admitted here.  Guest free text is never parsed
for slash commands; the typed frame is the only invocation surface a
guest has."
  :type '(alist :key-type (choice (const full) (const owner))
                :value-type (choice (const :tag "Everything" t)
                                    (const :tag "Nothing" nil)
                                    (repeat :tag "Mode-list"
                                            (choice string
                                                    (const t)
                                                    (const nil)
                                                    (cons (const not)
                                                          (repeat string))))))
  :group 'niwa)

(defconst niwa-collaboration-unsafe-guest-commands
  '("mode" "model" "clear" "tools" "plugin" "skills" "prompt" "edits"
    "collab" "help" "btw" "tokens" "ps" "worktree" "buddy")
  "Local slash commands a guest may never invoke, whatever the allowlist.

These either escalate authority (`mode\=' reaches full-auto, whose whole
point is skipping the permission prompts a guest would otherwise have
to be asked), mutate durable session state (`clear\='), manage the share
itself (`collab\='), switch on host-wide behavior (`buddy\=' watches
every buffer of the host\='s Emacs), or only report to the host\='s
display -- an echo area message, a cockpit, a transient -- that no guest
can see or dismiss.  Keeping the refusal here rather than in the
host\='s judgement means a mistaken allowlist entry cannot become an
escalation or a dead button.")

(defcustom niwa-collaboration-remote-interactions t
  "Whether full-link guests may answer pending interactions.

When non-nil, pending permission, Ask, and plan-approval interactions
are presented to write-token guests, and the first answer settles them.
When nil, full links are capped at prompting and interrupting."
  :type 'boolean
  :group 'niwa)

(defconst niwa-collaboration--publish-delay 0.1)
(defconst niwa-collaboration--publish-cost-factor 4
  "Coalesced publication waits at least this many times the last publish.
A publish blocks the one Emacs thread every room shares, and its cost
grows with the transcript.  Pacing streamed updates by that cost keeps a
streaming room near a fifth of the thread, so edits, cursors and joins in
every room still run between publishes.  One slow publish, such as a
garbage collection, delays the next by at most a second.")
(defconst niwa-collaboration--max-prompt-bytes (* 256 1024))
(defconst niwa-collaboration--max-guest-name-chars 32)
(defconst niwa-collaboration--max-prompt-attachments 3)
(defconst niwa-collaboration--max-attachment-bytes (* 1280 1024)
  "Total decoded attachment bytes one guest prompt may carry.

The whole sealed prompt frame must clear the relay's 2 MiB read limit,
and the relay closes the connection that overruns it.  Base64 costs a
third, and the prompt text beside the attachments is worth 256 KiB of
the same frame, so the decoded budget has to leave room for both:
base64 of this budget plus a maximum-length prompt still lands about
85 KiB short of the limit.  Images are downscaled to fit; anything
else is refused, because a log cannot be made smaller by resampling.")
(defconst niwa-collaboration--attachment-extensions
  '(("image/jpeg" . "jpg") ("image/png" . "png") ("image/webp" . "webp")
    ("application/pdf" . "pdf") ("text/plain" . "txt")
    ("text/markdown" . "md") ("text/csv" . "csv")
    ("application/json" . "json") ("text/x-patch" . "patch"))
  "Accepted guest attachment MIME types and their file extensions.

Extensions the Read tool treats as media -- pdf and the image types --
reach the model as media; the rest reach it as text.  Nothing here
comes from the guest: the saved name is host-generated, so a guest
filename can never steer a write.")
(defconst niwa-collaboration--duplicate-prompt-window 3.0
  "Seconds within which an identical prompt from one guest is dropped.
A human re-sending the same text this fast is a double-fired client
event (double click, stale viewer), not a second question.")

(defvar niwa-collaboration--rooms (make-hash-table :test #'eq)
  "Live collaboration rooms, keyed by their owning data buffer.
Each shared session has its own room, key, bearer links, and guest set;
nothing about one room reaches another room's guests.")

(defvar niwa-collaboration-artifact-room--rooms (make-hash-table :test #'equal)
  "Live artifact rooms, keyed by (WORKSPACE-ROOT . ARTIFACT-ID).
`niwa-collaboration-artifact-room' starts and stops them; presence and
keep-awake read them here, empty until an artifact is shared.")


;;
;;; Small data helpers

(defun niwa-collaboration--room-for-buffer (data-buffer)
  "Return the live room owned by DATA-BUFFER, or nil."
  (and (bufferp data-buffer)
       (gethash data-buffer niwa-collaboration--rooms)))

(defun niwa-collaboration--room-for-session (session)
  "Return the live room sharing SESSION, or nil."
  (when session
    (cl-find session (niwa-collaboration--room-list)
             :key (lambda (room) (plist-get room :session)))))

(defun niwa-collaboration--guest-text (value)
  "Return VALUE trimmed when it is text this host may act on, else nil.
A guest is untrusted, and every string one sends -- a prompt, a
questionnaire answer, interaction feedback -- reaches model-visible
context and the transcript the same way, so one budget covers them all."
  (and (stringp value)
       (<= (string-bytes value) niwa-collaboration--max-prompt-bytes)
       (let ((trimmed (string-trim value)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun niwa-collaboration-notify-queue-changed (session)
  "Re-publish SESSION\='s queue to its guests after a queue change.

Most queue changes ride a request, whose observers publish anyway.  A
local command drains without starting one, and a host-side edit starts
nothing at all, so without this seam the guest keeps a card for an
entry that is already gone."
  (when-let* ((room (niwa-collaboration--room-for-session session)))
    (condition-case err
        (progn
          (niwa-collaboration--publish-queue room)
          (niwa-collaboration--publish-status room)
          (maphash (lambda (peer guest)
                     (when (plist-get guest :ready)
                       (niwa-collaboration-recovery-send room peer)))
                   (plist-get room :guests)))
      (error (niwa-collaboration--observer-failure room err)))))

(defconst niwa-collaboration-needs-host-message
  "This needs a decision in Emacs on the host first"
  "What a guest is told when its action would have asked in Emacs.")

(defun niwa-collaboration-notify-guest (session guest-id message)
  "Show MESSAGE to SESSION's guests whose stable identity is GUEST-ID.
A notice is informational: the guest stays connected."
  (when-let* ((room (niwa-collaboration--room-for-session session))
              ((stringp guest-id)))
    (condition-case nil
        (maphash (lambda (peer guest)
                   (when (equal guest-id (plist-get guest :guest-id))
                     (niwa-collaboration--transport-send
                      (plist-get room :transport) peer
                      (list :t "notice" :message message))))
                 (plist-get room :guests))
      (error nil))))

(defun niwa-collaboration--sharing-p ()
  "Return non-nil while a collaboration room or lobby is live."
  (or (> (hash-table-count niwa-collaboration--rooms) 0)
      (and (boundp 'niwa-collaboration-lobby--lobbies)
           (> (hash-table-count niwa-collaboration-lobby--lobbies) 0))))

(defun niwa-collaboration--refuse-unseen-question ()
  "Refuse a minibuffer question nobody can see, from `minibuffer-setup-hook'.
An Emacs daemon without a client frame reads the minibuffer on its
invisible initial terminal: the question waits forever while guests keep
the event loop running inside it.  While a room or lobby is live, such a
question signals `inhibited-interaction' with its prompt instead, from
whichever step of a guest's turn asked it, so the step fails visibly.
A question on a client frame reaches the person there as usual."
  (when-let* (((daemonp))
              ((niwa-collaboration--sharing-p))
              (window (active-minibuffer-window))
              ((frame-initial-p (window-frame window))))
    (signal 'inhibited-interaction
            (list (string-trim (substring-no-properties
                                (or (minibuffer-prompt) "")))))))

(add-hook 'minibuffer-setup-hook #'niwa-collaboration--refuse-unseen-question)

(defun niwa-collaboration--room-list ()
  "Return every live collaboration room."
  (let (rooms)
    (maphash (lambda (_buffer room) (push room rooms))
             niwa-collaboration--rooms)
    (nreverse rooms)))

(defun niwa-collaboration--room-for-overlay (overlay)
  "Return the room an interaction OVERLAY belongs to, or nil.
Interactions render into a session's view buffer, whose
`niwa--data-buffer' names the owning data buffer.  A /btw side
conversation renders prompts in its own side view; those belong to the
parent session's room, reached through the side data buffer's parent."
  (when-let* ((buffer (overlay-buffer overlay))
              ((buffer-live-p buffer)))
    (or (niwa-collaboration--room-for-buffer buffer)
        (when-let* ((data-buffer
                     (and (local-variable-p 'niwa--data-buffer buffer)
                          (buffer-local-value 'niwa--data-buffer buffer)))
                    ((buffer-live-p data-buffer)))
          (or (niwa-collaboration--room-for-buffer data-buffer)
              (when-let* (((boundp
                            'niwa-side-conversation--parent-buffer))
                          (parent (buffer-local-value
                                   'niwa-side-conversation--parent-buffer
                                   data-buffer)))
                (niwa-collaboration--room-for-buffer parent)))))))

(defun niwa-collaboration--room-data-buffer (room)
  "Return the live data buffer for ROOM, or nil."
  (let ((buffer (plist-get room :data-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun niwa-collaboration--current-data-buffer ()
  "Return the data buffer associated with the current command context."
  (cond
   ((and (boundp 'niwa--data-buffer)
         (buffer-live-p niwa--data-buffer))
    niwa--data-buffer)
   ((and (boundp 'niwa--session)
         niwa--session
         (local-variable-p 'niwa--session)
         (not (local-variable-p 'niwa--view-buffer)))
    (current-buffer))
   ((and (boundp 'niwa--view-buffer)
         (buffer-live-p niwa--view-buffer))
    (with-current-buffer niwa--view-buffer
      (and (boundp 'niwa--data-buffer)
           (buffer-live-p niwa--data-buffer)
           niwa--data-buffer)))))

(defun niwa-collaboration--session-label (session data-buffer)
  "Return a safe display label for SESSION and DATA-BUFFER."
  (or (and (fboundp 'niwa-session-session-id)
           session
           (niwa-session-session-id session))
      (and (buffer-live-p data-buffer) (buffer-name data-buffer))
      "session"))

(defun niwa-collaboration--random-bytes (count)
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

(defun niwa-collaboration--base64url (bytes)
  "Return unpadded base64url encoding of unibyte BYTES."
  (replace-regexp-in-string
   "=+$" ""
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string
     "\\+" "-"
     (base64-encode-string bytes t)))))

(defun niwa-collaboration--base64url-decode (text)
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

(defun niwa-collaboration--links
    (web-origin room-id key write-token owner-token)
  "Return the view, full and owner links for ROOM-ID at WEB-ORIGIN.
The result is a plist of `:link-view', `:link-full' and `:link-owner'.
Each tier's secret is a prefix of the next: KEY, then WRITE-TOKEN, then
OWNER-TOKEN."
  (let ((link (lambda (secret)
                (format "%s/#%s.%s" web-origin room-id
                        (niwa-collaboration--base64url secret)))))
    (list :link-view (funcall link key)
          :link-full (funcall link (concat key write-token))
          :link-owner (funcall link (concat key write-token owner-token)))))

(defun niwa-collaboration--guest-link (room guest)
  "Return ROOM's link at GUEST's own tier.
Handing a guest a room is never a way to gain authority."
  (plist-get room (cond ((plist-get guest :owner) :link-owner)
                        ((plist-get guest :writable) :link-full)
                        (t :link-view))))

(defun niwa-collaboration--host-headers ()
  "Return the extra handshake headers for this host's relay dial."
  (when (and (stringp niwa-collaboration-relay-host-token)
             (not (string-empty-p niwa-collaboration-relay-host-token)))
    (list (cons "X-Niwa-Host-Token"
                niwa-collaboration-relay-host-token))))

(defun niwa-collaboration--relay-origins ()
  "Return (WS-ORIGIN . WEB-ORIGIN) from the configured relay URL.
Signal `user-error' when the configured value is not a ws origin."
  (let ((value (string-trim-right
                (or niwa-collaboration-relay-url "") "/")))
    (unless (string-match "\\`\\(wss?\\)://\\([^/#?]+\\)\\'" value)
      (user-error
       "'niwa-collaboration-relay-url' must be a ws:// or wss:// origin"))
    (cons value
          (concat (if (equal (match-string 1 value) "wss") "https" "http")
                  "://" (match-string 2 value)))))


;;
;;; Guest registry

(defun niwa-collaboration--guest (room peer)
  "Return the registered guest plist for PEER in ROOM, or nil."
  (when-let* ((guests (plist-get room :guests)))
    (gethash peer guests)))

(defun niwa-collaboration--sanitize-guest-name (name)
  "Return a bounded, control-character-free display NAME.
The name is display-only everywhere and never enters model context."
  (let* ((name (if (stringp name) name ""))
         (clean (replace-regexp-in-string "[[:cntrl:]]+" " " name))
         (clean (string-trim clean)))
    (cond ((string-empty-p clean) "guest")
          ((> (length clean) niwa-collaboration--max-guest-name-chars)
           (substring clean 0 niwa-collaboration--max-guest-name-chars))
          (t clean))))

(defun niwa-collaboration--sanitize-guest-id (value)
  "Return VALUE when it is a well-formed stable guest id, else nil.
The viewer mints one random id per browser so a guest's own queued
entries survive reconnects; peer numbers do not, so they cannot be the
identity.  The id never enters model context or the transcript."
  (and (stringp value)
       (string-match-p "\\`[A-Za-z0-9_-]\\{8,64\\}\\'" value)
       value))


;;
;;; Room publication

(defun niwa-collaboration--skip-oversize (room send)
  "Call SEND; when a frame it sends is too large, report it and return nil.
One oversized frame must not cost the guests the frames sent after it.
The diagnostic for ROOM carries only the sizes, never frame content."
  (condition-case err
      (funcall send)
    (niwa-collaboration-frame-too-large
     (niwa-diagnostic-message
      (niwa-diagnostic-scope (or (plist-get room :session)
                                    (plist-get room :workspace)
                                    'unavailable))
      "Collaboration frame not sent: %s" (error-message-string err))
     nil)))

(defun niwa-collaboration--send-guarded (room peer frame)
  "Send FRAME to PEER through ROOM's transport, skipping it if too large.
Loops that send several frames use this per frame, so one oversized
frame costs only itself."
  (niwa-collaboration--skip-oversize
   room (lambda ()
          (niwa-collaboration--transport-send
           (plist-get room :transport) peer frame))))

(defun niwa-collaboration--broadcast (room frame)
  "Broadcast FRAME to every guest in ROOM when the transport is live."
  (when-let* ((transport (plist-get room :transport))
             (guests (plist-get room :guests)))
    (when (> (hash-table-count guests) 0)
      (niwa-collaboration--skip-oversize
       room (lambda () (niwa-collaboration--transport-send transport 0 frame))))))

(defun niwa-collaboration--publish (room)
  "Publish changed records from ROOM to its connected guests.
Record the publish's cost on ROOM to pace later coalesced publishes."
  (when room
    (let* ((started (float-time))
           (old (plist-get room :records))
           (new (niwa-collaboration--reuse-record-ids
                 old (niwa-collaboration--project-records room)))
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
                       (equal (niwa-collaboration--record-without-revision
                               previous)
                              (niwa-collaboration--record-without-revision
                               record)))
            (setq revision (1+ revision))
            (push record changed))
          (setq record (plist-put record :revision revision))
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
      ;; A joining guest's snapshot encoding is of the records just replaced.
      (plist-put room :snapshot-chunks nil)
      (dolist (record changed)
        (niwa-collaboration--broadcast
         room (list :t "record"
                    :record (niwa-collaboration--json-record record))))
      (when removed
        (niwa-collaboration--broadcast
         room (list :t "remove" :ids (vconcat removed))))
      (niwa-collaboration--publish-queue room)
      (niwa-collaboration--publish-status room)
      (niwa-collaboration--publish-history room)
      (niwa-collaboration--publish-agents room)
      (niwa-collaboration--publish-background room)
      (niwa-collaboration--publish-tasks room)
      (niwa-collaboration--publish-goal room)
      (plist-put room :publish-cost (- (float-time) started))
      (plist-put room :observer-failed nil))))

(defun niwa-collaboration--queue-state (room)
  "Return ROOM's guest-visible pending queue state as a plist.

Only the count and the paused flag: a delivered prompt shows up in the
transcript on its own, and per-entry previews would hand every guest
every other guest's unsent text."
  (when-let* ((session (plist-get room :session)))
    (list :pending (length (niwa-session-pending-follow-ups session))
          :paused (and (or (niwa-session-pending-input-paused session)
                           (niwa-session-pending-input-failure-paused
                            session))
                       t))))

(defun niwa-collaboration--queue-position (room entry)
  "Return ENTRY's 1-based place in ROOM's pending follow-up queue.
Nil when the entry is no longer queued, which a drain between the
enqueue and this call makes possible."
  (when-let* ((session (plist-get room :session))
              (index (cl-position (plist-get entry :id)
                                  (niwa-session-pending-follow-ups session)
                                  :key (lambda (candidate)
                                         (plist-get candidate :id))
                                  :test #'equal)))
    (1+ index)))

(defun niwa-collaboration--guest-queue-state (room guest)
  "Return the queue state GUEST in ROOM should see, or nil.

The global part is the pending count and paused flag.  A guest with a
stable id additionally sees its own entries -- id, live position, and
its own text echoed back so a reloaded viewer can rebuild its card.
Only its own: another guest's unsent text never travels to it."
  (when-let* ((state (niwa-collaboration--queue-state room))
              (session (plist-get room :session)))
    (let ((guest-id (plist-get guest :guest-id))
          (position 0)
          own)
      (when guest-id
        (dolist (entry (niwa-session-pending-follow-ups session))
          (cl-incf position)
          (when (equal guest-id (plist-get entry :guest-id))
            (push (list :id (plist-get entry :id)
                        :position position
                        :text (or (plist-get entry :input) "")
                        :shared (plist-get entry :shared-question))
                  own))))
      (append state (when own (list :own (nreverse own)))))))

(defun niwa-collaboration--send-queue-state (room peer guest &optional force)
  "Send PEER its queue state for ROOM when it changed, or on FORCE.
GUEST caches the last state sent so an unchanged queue costs nothing."
  (when-let* ((state (niwa-collaboration--guest-queue-state room guest)))
    (when (or force (not (equal state (plist-get guest :queue-state))))
      (plist-put guest :queue-state state)
      (niwa-collaboration--send-guarded
       room peer
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
                             ("text" . ,(plist-get entry :text))
                             ("shared" . ,(plist-get entry :shared))))
                         own)))))))))

(defun niwa-collaboration--publish-queue (room)
  "Send ROOM's queue state to each registered guest when it changed.
Per-peer rather than broadcast, because a guest's frame carries that
guest's own pending entries."
  (when room
    (maphash (lambda (peer guest)
               (when (plist-get guest :ready)
                 (niwa-collaboration--send-queue-state room peer guest)))
             (plist-get room :guests))))

(defun niwa-collaboration--busy-p (room)
  "Return non-nil while ROOM's session has a running request."
  (when-let* ((data-buffer (niwa-collaboration--room-data-buffer room)))
    (and (buffer-local-value 'niwa--current-request data-buffer) t)))

(defun niwa-collaboration--status-frame (room)
  "Return ROOM\='s guest-visible session status.

The model and permission mode match those in the Emacs mode line.
The session's display name heads the
room, and follows a rename, including an automatic title."
  (let ((busy (niwa-collaboration--busy-p room))
        (data-buffer (niwa-collaboration--room-data-buffer room)))
    (list :t "status"
          :name (when-let* ((session (plist-get room :session)))
                  (niwa-session-name session))
          :turnCount (when-let* ((session (plist-get room :session)))
                       (or (niwa-session-turn-count session) 0))
          ;; Host diagnostics reach owners only, in their recovery frame.
          :issues (vconcat
                   (when-let* ((session (plist-get room :session)))
                     (mapcar (lambda (issue)
                               (list :id (plist-get issue :id)
                                     :category (plist-get issue :category)
                                     :blocking (plist-get issue :blocking)
                                     :message (niwa-recovery-public-message issue)))
                             (niwa-session-recovery-issues session))))
          :outcome (when-let* ((session (plist-get room :session))
                               (outcome (niwa-session-last-outcome session)))
                     (symbol-name outcome))
          :busy (if busy t :json-false)
          :mode (when-let* ((mode (ignore-errors
                                    (niwa-permission-mode-effective
                                     (plist-get room :session)
                                     data-buffer))))
                  (format "%s" mode))
          ;; Plan is a mode the guest can enter from a button, so the
          ;; strip has to say when it is on.
          :plan (if (and (plist-get room :session)
                         (ignore-errors
                           (niwa-plan-mode-active-p
                            (plist-get room :session))))
                    t :json-false)
          :model (when data-buffer
                   (when-let* ((model (buffer-local-value 'gptel-model
                                                          data-buffer)))
                     (format "%s" model))))))

(defun niwa-collaboration--publish-status (room)
  "Broadcast ROOM\='s session status to its guests when it has changed."
  (let ((status (niwa-collaboration--status-frame room)))
    (unless (equal status (plist-get room :status))
      (setq room (plist-put room :status status))
      (niwa-collaboration--broadcast room status)
      (maphash (lambda (peer guest)
                 (when (plist-get guest :ready)
                   (niwa-collaboration-recovery-send room peer)))
               (plist-get room :guests)))))

(defun niwa-collaboration--publish-timer (data-buffer)
  "Run the coalesced publication timer for DATA-BUFFER's room."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (setq room (plist-put room :publish-timer nil))
    (niwa-collaboration--publish room)))

(defun niwa-collaboration-notify-request-changed (data-buffer &optional completed-trigger)
  "Publish DATA-BUFFER's status.
COMPLETED-TRIGGER names the finished root turn's initiator."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (condition-case err
        (progn
          (niwa-collaboration--publish-status room)
          (when (and (eq completed-trigger 'user)
                     (not (niwa-collaboration--busy-p room)))
            (niwa-collaboration--push-audience (plist-get room :session) room 'all)
            (niwa-collaboration--broadcast
             room (list :t "notice" :kind "completion"
                        :outcome (plist-get (plist-get room :status) :outcome)))))
      (error (niwa-collaboration--observer-failure room err)))))

(defun niwa-collaboration--on-session-event (data-buffer event)
  "Publish what DATA-BUFFER's session EVENT changes for its room.
A rename, manual or automatic, changes the name the room is headed by
and the title of its current chat; a history save may change its Goal."
  (pcase event
    ('rename
     (niwa-collaboration-notify-request-changed data-buffer)
     (niwa-collaboration-notify-history-changed data-buffer))
    ('replace-history
     (niwa-collaboration-notify-history-changed data-buffer))
    ('save-history
     (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
       (niwa-collaboration--publish-goal room)))))

(defun niwa-collaboration-notify-history-changed (data-buffer)
  "Refresh DATA-BUFFER's browser history after a segment transition commits."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (condition-case err
        (niwa-collaboration--schedule-publish room)
      (error (niwa-collaboration--observer-failure room err)))))

(defun niwa-collaboration--safe-accepted-prompt (data-buffer)
  "Publish DATA-BUFFER immediately after an accepted prompt is inserted.

This observer is failure-isolated so a collaboration viewer cannot block the
request or prompt transaction."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (condition-case err
        (niwa-collaboration--publish room)
      (error (niwa-collaboration--observer-failure room err))))
  nil)

(defun niwa-collaboration--schedule-publish (room)
  "Coalesce assistant stream updates for ROOM, paced by its last publish."
  ;; A plain timer armed while TRAMP suspends timers would vanish with that
  ;; list and block every later publish; hold it until the wait returns.
  (when (and room (not (niwa--ui-timer-pending-p (plist-get room :publish-timer))))
    (setq room
          (plist-put room :publish-timer
                     (niwa-transport-run-at-time
                      (max niwa-collaboration--publish-delay
                           (min 1.0 (* niwa-collaboration--publish-cost-factor
                                       (or (plist-get room :publish-cost) 0))))
                      #'niwa-collaboration--publish-timer
                      (plist-get room :data-buffer))))))


;;
;;; Room lifecycle

(defun niwa-collaboration--stop-internal (room &optional reason)
  "Stop ROOM and its associated transport and publish timer."
  (when (and room
             (eq room (niwa-collaboration--room-for-buffer
                       (plist-get room :data-buffer))))
    ;; Clear the authority before any teardown operation can signal.  The
    ;; local ROOM still supplies the transport and timers to close below.
    (remhash (plist-get room :data-buffer) niwa-collaboration--rooms)
    (when (hash-table-p (plist-get room :guests))
      (maphash (lambda (peer _guest)
                 (condition-case nil (niwa-collaboration-editing-depart room peer)
                   (error nil)))
               (plist-get room :guests)))
    (condition-case nil
        (niwa-collaboration-share-dismiss room)
      (error nil))
    ;; The global hooks serve every room; they leave with the last one.
    (when (zerop (hash-table-count niwa-collaboration--rooms))
      (remove-hook 'kill-emacs-hook #'niwa-collaboration--stop-for-emacs)
      (remove-hook 'niwa-interaction-prompt-created-hook
                   #'niwa-collaboration--on-prompt-created)
      (remove-hook 'niwa-interaction-prompt-settled-hook
                   #'niwa-collaboration--on-prompt-settled))
    (when-let* ((data-buffer (plist-get room :data-buffer)))
      (when (buffer-live-p data-buffer)
        (with-current-buffer data-buffer
          (remove-hook 'gptel-pre-tool-call-functions
                       #'niwa-collaboration--safe-pre-tool)
          (remove-hook 'gptel-post-tool-call-functions
                       #'niwa-collaboration--safe-post-tool))))
    (when-let* ((timer (plist-get room :publish-timer)))
      (niwa--ui-timer-cancel timer))
    (when-let* ((observer (plist-get room :session-observer)))
      (niwa-session-control-transfer-unregister-observer
       (plist-get room :session) observer))
    (when-let* ((transport (plist-get room :transport)))
      (unless (eq reason 'emacs-exit)
        (condition-case nil
            (niwa-collaboration--farewell room reason)
          (error nil)))
      (condition-case nil
          (niwa-collaboration--transport-stop transport)
        (error nil)))
    ;; The workspace's other rooms and lobby lose this room's guests.
    (unless (eq reason 'emacs-exit)
      (condition-case nil
          (niwa-collaboration-presence-publish room)
        (error nil)))))

(defun niwa-collaboration--farewell (room reason)
  "Tell ROOM's guests that it ends for REASON.
A room handed off to a successor tells each guest where the work went,
with the successor's name when it has one and a link at the tier that
guest already holds; otherwise one broadcast says why the room ended."
  (let ((transport (plist-get room :transport))
        (guests (plist-get room :guests))
        (successor (plist-get room :successor)))
    (when (> (hash-table-count guests) 0)
      (if (null successor)
          (niwa-collaboration--transport-send
           transport 0 (list :t "bye" :reason (format "%s" reason)))
        (let ((name (niwa-collaboration--room-name successor)))
          (maphash (lambda (peer guest)
                     (niwa-collaboration--transport-send
                      transport peer
                      (list :t "bye" :reason (format "%s" reason)
                            :name name
                            :link (niwa-collaboration--guest-link
                                   successor guest))))
                   guests))))))

(defun niwa-collaboration--room-name (room)
  "Return the name ROOM's session goes by, or nil while it has none.
An unnamed session is known by its ID, which tells a guest nothing."
  (when-let* ((session (plist-get room :session))
              (name (niwa-session-name session))
              ((not (equal name (niwa-session-session-id session)))))
    name))

(defun niwa-collaboration-hand-off (source target)
  "Move the guests of SOURCE's room to a room for TARGET.
SOURCE and TARGET are data buffers.  When SOURCE is not shared, do
nothing and return nil.  Otherwise share TARGET, end SOURCE's room
telling each guest where the work continues, with a link to TARGET's
room at the tier that guest already holds, and return TARGET's room.

The guests already follow this work, so TARGET is shared without the
consent question `/collab' asks.  Its relay connection completes
asynchronously; the links are valid at once and a guest joining early
waits for the host.  This never signals: when TARGET cannot be shared,
the host is warned and SOURCE's guests are told the work continues in
Emacs, so a failed share never blocks the caller's own handoff."
  (when-let* ((room (niwa-collaboration--room-for-buffer source)))
    (let* ((scope (niwa-diagnostic-scope target))
           (successor
           (condition-case err
               (progn
                 (niwa-collaboration--load-room-modules)
                 (niwa-collaboration--start
                  (niwa-collaboration--data-buffer-session target)
                  target))
             (error
              (niwa-diagnostic-warning
               scope 'niwa
               (format "Could not share %s for its predecessor's guests: %s"
                       (if (buffer-live-p target) (buffer-name target) target)
                       (error-message-string err)))
              nil))))
      (plist-put room :successor successor)
      (niwa-collaboration--stop-internal
       room (if successor 'handoff 'handoff-failed))
      successor)))

(defvar niwa-collaboration-stop-reason nil
  "Why a data buffer is being killed, told to its room's guests.
Nil reports `data-buffer-killed'; callers that kill a session for a
reason guests should see bind it around `kill-buffer'.")

(defun niwa-collaboration--stop-for-buffer ()
  "Stop sharing when the owning data buffer is killed."
  (when-let* ((room (niwa-collaboration--room-for-buffer
                     (current-buffer))))
    (niwa-collaboration--stop-internal
     room (or niwa-collaboration-stop-reason 'data-buffer-killed))))

(defun niwa-collaboration--stop-for-session ()
  "Stop sharing from a data buffer's SessionEnd hook.
A session ending because Emacs exits stops the way Emacs exit does."
  (if (equal niwa-session-end-reason "exit")
      (when-let* ((room (niwa-collaboration--room-for-buffer
                         (current-buffer))))
        (niwa-collaboration--stop-internal room 'emacs-exit))
    (niwa-collaboration--stop-for-buffer)))

(defun niwa-collaboration--stop-for-emacs ()
  "Stop every share before Emacs exits."
  (dolist (room (niwa-collaboration--room-list))
    (niwa-collaboration--stop-internal room 'emacs-exit)))

(cl-defun niwa-collaboration--start (session data-buffer)
  "Start a room for SESSION and DATA-BUFFER and return the room.

Each session gets its own room; sharing a second session never touches
the first.  The early return below needs the block a `cl-defun'
establishes; a plain `defun' would signal `no-catch' instead of
returning the live room."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (cl-return-from niwa-collaboration--start room))
  (require 'niwa-collaboration-guest)
  (require 'niwa-collaboration-task)
  (require 'niwa-collaboration-goal)
  (require 'niwa-collaboration-background)
  (pcase-let* ((`(,ws-origin . ,web-origin)
                (niwa-collaboration--relay-origins))
               (room-id (niwa-collaboration--base64url
                         (niwa-collaboration--random-bytes 16)))
               (key (niwa-collaboration--random-bytes 32))
               (write-token (niwa-collaboration--random-bytes 16))
               (owner-token (niwa-collaboration--random-bytes 16))
               ;; The room keeps the projection its first publish reuses.
               (projection-cache (niwa-collaboration--projection-cache-create))
               (records (niwa-collaboration--canonical-records
                         data-buffer nil nil projection-cache)))
    (unless (require 'websocket nil t)
      (user-error "Collaboration requires the 'websocket' package; install it first"))
    (let (transport)
      (condition-case error-data
          (progn
            (setq transport
                  (niwa-collaboration--transport-open
                   (format "%s/r/%s?role=host" ws-origin room-id)
                   key
                   :headers (niwa-collaboration--host-headers)
                   :on-frame (lambda (peer frame)
                               (niwa-collaboration--on-frame
                                data-buffer peer frame))
                   :on-control (lambda (event peer)
                                 (niwa-collaboration--on-control
                                  data-buffer event peer))
                   :on-state (lambda (state)
                               (niwa-collaboration--on-state
                                data-buffer state))))
            (puthash data-buffer
                  (append
                   (list :transport transport
                         :session session
                         :started-at (float-time)
                         :last-guest-departure nil
                         :data-buffer data-buffer
                         :session-label
                         (niwa-collaboration--session-label
                          session data-buffer)
                         :room-id room-id
                         :key key
                         :write-token write-token
                         :owner-token owner-token
                         :records records
                         :projection-cache projection-cache
                         :queue nil
                         :pending-tools nil
                         :tool-call-occurrences
                         (make-hash-table :test #'equal)
                         :guests (make-hash-table :test #'eql)
                         :push-guests (make-hash-table :test #'equal)
                         :ui-requests (make-hash-table :test #'eql)
                         :publish-timer nil
                         :session-observer
                         (niwa-session-control-transfer-register-observer
                          session
                          (lambda (event &rest _)
                            (niwa-collaboration--on-session-event
                             data-buffer event))))
                   (niwa-collaboration--links
                    web-origin room-id key write-token owner-token))
                  niwa-collaboration--rooms)
            (with-current-buffer data-buffer
              (add-hook 'kill-buffer-hook
                        #'niwa-collaboration--stop-for-buffer nil t)
              (add-hook 'niwa-session-end-hook
                        #'niwa-collaboration--stop-for-session nil t)
              (add-hook 'gptel-pre-tool-call-functions
                        #'niwa-collaboration--safe-pre-tool nil t)
              (add-hook 'gptel-post-tool-call-functions
                        #'niwa-collaboration--safe-post-tool nil t))
            (add-hook 'kill-emacs-hook
                      #'niwa-collaboration--stop-for-emacs)
            (add-hook 'niwa-interaction-prompt-created-hook
                      #'niwa-collaboration--on-prompt-created)
            (add-hook 'niwa-interaction-prompt-settled-hook
                      #'niwa-collaboration--on-prompt-settled)
            (niwa-sleep-update)
            (let ((room (niwa-collaboration--room-for-buffer data-buffer)))
              (niwa-collaboration--adopt-prompts room)
              room))
        (error
         (condition-case nil
             (let ((room (niwa-collaboration--room-for-buffer
                          data-buffer)))
               (if (and room (eq (plist-get room :transport) transport))
                   (niwa-collaboration--stop-internal room 'start-failed)
                 (when transport
                   (niwa-collaboration--transport-stop transport))))
           (error nil))
         (signal (car error-data) (cdr error-data)))))))

(defun niwa-collaboration--adopt-prompts (room)
  "Register the interactions ROOM's session raised before ROOM started.
A prompt announces itself once, when it is created, so a session
shared while a question, approval or review waits would otherwise never
show it to guests.  Every prompt a guest can answer is registered in its
view's interaction zone; prompts are adopted in the order they opened."
  (when (boundp 'niwa-view--interaction-overlays)
    (let (pending)
      (dolist (buffer (buffer-list))
        (let ((overlays (buffer-local-value 'niwa-view--interaction-overlays
                                            buffer))
              (lifecycle (buffer-local-value 'niwa-view--interaction-lifecycle
                                             buffer)))
          (when (hash-table-p overlays)
            (maphash
             (lambda (id overlay)
               (when (and (overlay-buffer overlay)
                          (not (overlay-get overlay 'niwa-settled))
                          (overlay-get overlay 'niwa--remote)
                          (eq room (niwa-collaboration--room-for-overlay
                                    overlay)))
                 (let ((opened (and (hash-table-p lifecycle)
                                    (gethash id lifecycle))))
                   ;; Arrival orders one view's prompts even within one
                   ;; clock tick; the time orders different views'.
                   (push (cons (list (or (plist-get opened :opened-at) 0)
                                     (or (plist-get opened :arrival) 0))
                               overlay)
                         pending))))
             overlays))))
      (dolist (entry (sort pending :key #'car))
        (niwa-collaboration--on-prompt-created (cdr entry))))))

(defun niwa-collaboration--load-room-modules ()
  "Load the modules a room needs before one starts."
  (require 'niwa-collaboration-projection)
  (require 'niwa-collaboration-share)
  (require 'niwa-collaboration-transport))

(defun niwa-collaboration--confirm-share (&optional owner)
  "Ask to share OWNER's session; signal `user-error' if declined."
  (unless (yes-or-no-p
           (concat
            "Share visible prompts, responses, paths, and tool results, which "
            "may contain credentials or secrets, through the relay? Frames are "
            "sealed end to end; the links are bearer credentials. "))
    (niwa-diagnostic-user-error
     (niwa-diagnostic-scope (or owner (current-buffer))) "Collaboration not started")))

(defun niwa-collaboration--data-buffer-session (data-buffer)
  "Return DATA-BUFFER's session, or signal `user-error'."
  (or (and (buffer-live-p data-buffer)
           (buffer-local-value 'niwa--session data-buffer))
      (user-error "No active niwa session in this buffer")))

(defun niwa-collaboration--room-or-start (data-buffer)
  "Return (ROOM . STARTED) for DATA-BUFFER's room.
Without a room, start one after the user consents; STARTED is then t."
  (niwa-collaboration--load-room-modules)
  (let ((scope (niwa-diagnostic-scope data-buffer)))
    (condition-case err
        (let ((session (niwa-collaboration--data-buffer-session data-buffer))
              (room (niwa-collaboration--room-for-buffer data-buffer)))
          (if room
              (cons room nil)
            (niwa-collaboration--confirm-share session)
            (cons (niwa-collaboration--start session data-buffer) t)))
      (user-error (niwa-diagnostic-user-error scope "%s" (error-message-string err))))))

(defun niwa-collaboration-view ()
  "Start live collaboration, or report the active room's links."
  (interactive)
  (niwa-collaboration-share-present
   (car (niwa-collaboration--room-or-start
         (niwa-collaboration--current-data-buffer)))))

(defun niwa-collaboration-item-opener (&optional data-buffer)
  "Settle now that a shared item may open in a browser; return its opener.
The editor exists only in a room.  Unless DATA-BUFFER's session is shared
already, this asks now, with `/collab''s disclosure, to share the
session the item will open in; nil DATA-BUFFER stands for a session that
does not exist yet.  Declining signals `user-error'.

The opener takes a data buffer, an item ID and optionally the viewer
ROUTE of `niwa-collaboration-item-link'.  It opens the item in that
buffer's room with the full-control link, as the host itself,
starting the room under the consent given here.  It never asks, so it
may run later from a timer; a room that ended meanwhile without consent
having been given is refused rather than reshared."
  (niwa-collaboration--load-room-modules)
  (let ((consented
         (unless (and data-buffer
                      (niwa-collaboration--data-buffer-session data-buffer)
                      (niwa-collaboration--room-for-buffer data-buffer))
           (niwa-collaboration--confirm-share data-buffer)
           t)))
    (lambda (buffer id &optional route)
      (let ((session (niwa-collaboration--data-buffer-session buffer))
            (room (niwa-collaboration--room-for-buffer buffer))
            started)
        (unless room
          (unless consented
            (niwa-diagnostic-user-error
             (niwa-diagnostic-scope session)
             "This session is no longer shared; open the item again"))
          (setq room (niwa-collaboration--start session buffer)
                started t))
        ;; The viewer opens `?shared=ID' directly as that item's editor tab.
        (browse-url (niwa-collaboration-item-link (plist-get room :link-full) id route))
        (when started
          (message "niwa: sharing %s; /collab stop ends the room"
                   (plist-get room :session-label)))))))

(defun niwa-collaboration-open-shared-item (data-buffer id)
  "Open the workspace's shared item ID in DATA-BUFFER's room in a web browser.
The editor exists only in a room, so without one this asks to start
sharing the session first."
  (funcall (niwa-collaboration-item-opener data-buffer) data-buffer id))

(defun niwa-collaboration-stop ()
  "Stop the current session's share, or every share outside a session.
Run from a session that is not shared, another session's share is
never touched: that is a report, not a teardown."
  (interactive)
  (let ((rooms (niwa-collaboration--room-list))
        (data-buffer (niwa-collaboration--current-data-buffer)))
    (cond
     ((null rooms)
      (message "niwa: collaboration is not active"))
     ((niwa-collaboration--room-for-buffer data-buffer)
      (niwa-collaboration-stop-session data-buffer))
     (data-buffer
      (message "niwa: no active share for this session; %d share%s live elsewhere"
               (length rooms) (if (= 1 (length rooms)) "" "s")))
     (t
      (dolist (room rooms)
        (niwa-collaboration--stop-internal room 'user-stop))
      (message "niwa: stopped %d collaboration share%s"
               (length rooms) (if (= 1 (length rooms)) "" "s"))))))

(defun niwa-collaboration-stop-session (data-buffer)
  "Stop the share of DATA-BUFFER's session; return non-nil if one stopped.
No other session's share is touched, whichever are live."
  (if-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
      (progn
        (niwa-collaboration--stop-internal room 'user-stop)
        (message "niwa: collaboration stopped for %s"
                 (plist-get room :session-label))
        t)
    (message "niwa: this session is not shared")
    nil))

(defun niwa-collaboration-room-summary (data-buffer)
  "Return a summary of DATA-BUFFER's live room, or nil when it is not shared.
The plist holds `:guests', the number of distinct guests connected.  It is
read from memory, so status displays may call it freely; it never carries
a bearer link."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (list :guests (niwa-collaboration--guest-count room))))

(defun niwa-collaboration--guest-count (room)
  "Return the number of distinct guests connected to ROOM.
One guest may hold several tabs, which share its stable identity."
  (let (ids)
    (when-let* ((guests (plist-get room :guests)))
      (maphash (lambda (peer guest)
                 (cl-pushnew (or (plist-get guest :guest-id) peer) ids
                             :test #'equal))
               guests))
    (length ids)))

(defun niwa-collaboration-shares ()
  "Return every session share and lobby of this Emacs, without secrets.
Each entry is a plist with `:kind' (`room', `lobby', or `recorded-lobby'
for a lobby recorded to run that is not running), `:label', `:guests',
and `:connected'.  A room names its owning `:data-buffer' and retains its
`:diagnostic-scope' for local refusals after it stops; a lobby names its
workspace root `:directory'.  Rooms come first, in the order they started.
Read from memory and the small lobby record, so status displays may call
it freely."
  (append
   (mapcar (lambda (room)
             (let ((transport (plist-get room :transport)))
               (list :kind 'room
                     :label (plist-get room :session-label)
                     :guests (niwa-collaboration--guest-count room)
                     :connected (and transport
                                     (niwa-collaboration--transport-open-p
                                      transport)
                                     t)
                     :data-buffer (plist-get room :data-buffer)
                     :diagnostic-scope (niwa-diagnostic-scope (plist-get room :session)))))
           (niwa-collaboration--room-list))
   ;; A lobby exists only once its module is loaded.
   (and (fboundp 'niwa-collaboration-lobby-shares)
        (niwa-collaboration-lobby-shares))))

(defun niwa-collaboration--share-room (entry)
  "Return the live room of room ENTRY, or signal that it has stopped."
  (or (niwa-collaboration--room-for-buffer (plist-get entry :data-buffer))
      (niwa-diagnostic-user-error
       (or (plist-get entry :diagnostic-scope)
           (niwa-diagnostic-scope (plist-get entry :data-buffer)))
       "%s is no longer shared" (plist-get entry :label))))

(defun niwa-collaboration-show-share (entry)
  "Show the links of ENTRY from `niwa-collaboration-shares'.
A lobby recorded to run but not running is started first, as it would be
at the next Emacs start.  A share that stopped meanwhile is refused rather
than started again."
  (pcase (plist-get entry :kind)
    ('room
     (niwa-collaboration-share-present
      (niwa-collaboration--share-room entry)))
    ((or 'lobby 'recorded-lobby)
     (niwa-collaboration-lobby-show-directory
      (plist-get entry :directory)
      (eq (plist-get entry :kind) 'recorded-lobby)))))

(defun niwa-collaboration-stop-share (entry)
  "Stop ENTRY from `niwa-collaboration-shares'.
A stopped lobby is no longer recorded to restart, and a recorded lobby
that is not running is forgotten."
  (pcase (plist-get entry :kind)
    ('room
     (niwa-collaboration-stop-session
      (plist-get (niwa-collaboration--share-room entry) :data-buffer)))
    ((or 'lobby 'recorded-lobby)
     (niwa-collaboration-lobby-stop-directory
      (plist-get entry :directory)))))

;;;###autoload
(defun niwa-collaboration-stop-all ()
  "Stop every session share and lobby in this Emacs.
Stopped lobbies are no longer recorded to restart, including lobbies that
failed to restart; their stored credentials stay, so starting one again
revives its links."
  (interactive)
  ;; A recorded lobby must be forgotten even before startup restoration has
  ;; loaded its owner.  Loading the module does not start any lobby.
  (require 'niwa-collaboration-lobby)
  (let ((rooms (niwa-collaboration--room-list))
        lobbies)
    (dolist (room rooms)
      (niwa-collaboration--stop-internal room 'user-stop))
    (setq lobbies (niwa-collaboration-lobby-stop-every))
    (if (and (null rooms) (zerop lobbies))
        (message "niwa: collaboration is not active")
      (message "niwa: stopped %d share%s and %d lobb%s"
               (length rooms) (if (= 1 (length rooms)) "" "s")
               lobbies (if (= 1 lobbies) "y" "ies")))))

(defun niwa-collaboration--keep-awake-state (room now)
  "Return ROOM's keep-awake state at float-time NOW.
Return t while guests are connected or the idle cutoff is disabled,
the expiry time while the idle interval runs, or nil after it expires.
ROOM may also be a lobby.  Only registered rooms are considered open;
the caller owns that check."
  (let ((guests (plist-get room :guests)))
    (if (or (null niwa-collaboration-keep-awake-idle-time)
            (and guests (> (hash-table-count guests) 0)))
        t
      (let ((expiry (+ (max (or (plist-get room :started-at) 0)
                           (or (plist-get room :last-guest-departure) 0))
                       niwa-collaboration-keep-awake-idle-time)))
        (and (< now expiry) expiry)))))

(defun niwa-collaboration-sleep-reasons (now)
  "Return stable keep-awake reasons for open shares, lobbies and artifacts at NOW.
NOW is a float-time.  Transient relay outages do not close a share."
  (let (share lobby artifact)
    (maphash (lambda (_buffer room)
               (when (niwa-collaboration--keep-awake-state room now)
                 (setq share t)))
             niwa-collaboration--rooms)
    (when (boundp 'niwa-collaboration-lobby--lobbies)
      (maphash (lambda (_root room)
                 (when (niwa-collaboration--keep-awake-state room now)
                   (setq lobby t)))
               niwa-collaboration-lobby--lobbies))
    (maphash (lambda (_key room)
               (when (niwa-collaboration--keep-awake-state room now)
                 (setq artifact t)))
             niwa-collaboration-artifact-room--rooms)
    (delq nil (list (and share "collaboration share active")
                    (and lobby "collaboration lobby active")
                    (and artifact "shared artifact active")))))

(defun niwa-collaboration--keep-awake-status (room now)
  "Return ROOM's keep-awake request status at float-time NOW."
  (let ((state (niwa-collaboration--keep-awake-state room now)))
    (concat "Keep-awake request: "
            (cond ((eq state t) "active")
                  (state (format "until %s (idle cutoff)"
                                 (format-time-string "%Y-%m-%d %H:%M:%S %Z"
                                                     state)))
                  (t "idle cutoff reached")))))

(defun niwa-collaboration--room-status (room)
  "Return a status line for ROOM without exposing its secrets."
  (let ((transport (plist-get room :transport))
        (guests (plist-get room :guests))
        names)
    (maphash (lambda (_peer guest)
               (push (format "%s%s" (plist-get guest :name)
                             (if (plist-get guest :writable) "" " (view)"))
                     names))
             guests)
    (format "%s: relay %s; %s; %s"
            (plist-get room :session-label)
            (if (niwa-collaboration--transport-open-p transport)
                "connected" "reconnecting")
            (if names
                (format "guests: %s"
                        (mapconcat #'identity (nreverse names) ", "))
              "no guest connected")
            (niwa-collaboration--keep-awake-status room (float-time)))))

(defun niwa-collaboration-status ()
  "Report every active share and lobby without exposing secrets."
  (interactive)
  (let ((lines (delq nil
                     (append
                      (mapcar #'niwa-collaboration--room-status
                              (niwa-collaboration--room-list))
                      ;; A lobby exists only once its module is loaded.
                      (list (and (fboundp 'niwa-collaboration-lobby--status)
                                 (niwa-collaboration-lobby--status)))))))
    (if lines
        (message "niwa: collaboration active for %s"
                 (mapconcat #'identity lines "; "))
      (message "niwa: collaboration inactive"))))


;;
;;; gptel and lifecycle hooks

(defun niwa-collaboration--pre-tool (info)
  "Publish a running tool record for gptel tool-call INFO.
Runs from a buffer-local hook, so the current buffer names the room."
  (when-let* ((room (niwa-collaboration--room-for-buffer
                     (current-buffer)))
              ((not (niwa-collaboration--routine-poll-p
                     (plist-get info :name) (plist-get info :args) nil nil))))
    (progn
      (let* ((name (format "%s" (plist-get info :name)))
             (call-key (niwa-collaboration--tool-call-key info))
             (pending (plist-get room :pending-tools))
             existing)
        (dolist (entry pending)
          (when (and (null existing)
                     (equal (plist-get entry :tool-use-id)
                            (plist-get info :tool-use-id))
                     (equal (plist-get entry :call-key) call-key)
                     (equal (plist-get entry :status) "running"))
            (setq existing entry)))
        (unless existing
          (let* ((canonical
                  (niwa-collaboration--room-canonical-records room))
                 (occurrences (plist-get room :tool-call-occurrences))
                 (occurrence (gethash call-key occurrences 0))
                 (explicit-id (or (plist-get info :tool-use-id)
                                  (plist-get info :id)
                                  (plist-get info :call-id)
                                  (plist-get info :tool-call-id)
                                  (plist-get info :tool_call_id)))
                 (id (if explicit-id
                         (format "tool-%s" explicit-id)
                       (niwa-collaboration--stable-record-id
                        "tool"
                        (let ((print-length nil)
                              (print-level nil)
                              (print-circle t))
                          (prin1-to-string call-key))
                        occurrence)))
                 (entry (apply
                         #'niwa-collaboration--record
                         id "tool"
                         :revision 0
                         :name name
                         :status "running"
                         :summary name
                         :result ""
                         :truncated nil
                         :pending t
                         :tool-use-id (plist-get info :tool-use-id)
                         :pipeline-calls nil
                         :presentation nil
                         :identity-fixed t
                         :call-key call-key
                         :baseline-buffer-end
                         (with-current-buffer (plist-get room :data-buffer)
                           (point-max))
                         :baseline-tool-count
                         (length (niwa-collaboration--tool-records canonical))
                         :baseline-record-count (length canonical)
                         (niwa-collaboration--tool-extras
                          name (plist-get info :args)))))
            (puthash call-key (1+ occurrence) occurrences)
            (setq room (plist-put room :pending-tools
                                  (append pending (list entry))))))
        ;; Tool start is intentionally published immediately.  A short tool
        ;; still has a truthful completion transition, while a long tool is
        ;; visible as running before it produces a result.
        (niwa-collaboration--publish room))))
  nil)

(defun niwa-collaboration--pipeline-tool-event (event)
  "Project invocation-scoped approval EVENT onto its existing browser row.
Nested invocations update their facts immediately and coalesce presentation
with stream publication.  Root events publish immediately.  The transient
tree is replaced by canonical transcript metadata when the result is inserted."
  (when-let* ((data (plist-get event :data-buffer))
              ((buffer-live-p data))
              (room (niwa-collaboration--room-for-buffer data))
              (id (plist-get event :tool-use-id)))
    (with-current-buffer data
      (let* ((parent (plist-get event :parent-tool-use-id))
             (type (plist-get event :type))
             (pending (plist-get room :pending-tools))
             (entry (if parent
                        (cl-find-if
                         (lambda (row)
                           (cl-find parent (plist-get row :pipeline-calls)
                                    :key (lambda (call) (plist-get call :tool-use-id))
                                    :test #'equal)) pending)
                      (cl-find id pending :key (lambda (row) (plist-get row :tool-use-id))
                               :test #'equal))))
        (when (and (eq type 'started) (not parent) (not entry)
                   (not (niwa-collaboration--routine-poll-p
                         (plist-get event :name) (plist-get event :args) nil nil)))
          (setq entry (cl-find-if
                       (lambda (row) (niwa-collaboration--pending-tool-match
                                      (list :name (plist-get event :name)
                                            :args (plist-get event :args)) row)) pending))
          (unless entry
            (niwa-collaboration--pre-tool event)
            (setq entry (cl-find id (plist-get room :pending-tools)
                                 :key (lambda (row) (plist-get row :tool-use-id)) :test #'equal)))
          (when entry (setf (plist-get entry :tool-use-id) id)))
        (when entry
          (let* ((calls (plist-get entry :pipeline-calls))
                 (call (cl-find id calls :key (lambda (node) (plist-get node :tool-use-id))
                                :test #'equal))
                 changed)
            (pcase type
              ('started
               (unless call
                 (setq call (list :tool-use-id id :parent-tool-use-id parent
                                  :name (plist-get event :name) :args (plist-get event :args)
                                  :status 'running :permission-approvals nil)
                       calls (append calls (list call)) changed t)
                 (setf (plist-get entry :pipeline-calls) calls)))
              ('permission-approved
               (when (eq (plist-get call :status) 'running)
                 (setf (plist-get call :permission-approvals)
                       (copy-tree (plist-get event :permission-approvals)))
                 (setq changed t)))
              ('settled
               (when (eq (plist-get call :status) 'running)
                 (setf (plist-get call :status) (plist-get event :status))
                 (setq changed t))))
            (when changed
              (plist-put entry :pipeline-dirty t)
              (if parent
                  (niwa-collaboration--schedule-publish room)
                (niwa-collaboration--publish room))))))))
  nil)

(add-hook 'niwa-pipeline-tool-event-hook #'niwa-collaboration--pipeline-tool-event)

(defun niwa-collaboration--post-tool (info)
  "Publish the settled result for gptel tool-call INFO.
Runs from a buffer-local hook, so the current buffer names the room."
  (when-let* ((room (niwa-collaboration--room-for-buffer
                     (current-buffer))))
    (progn
      ;; A settled ApplyPatch may have replaced a published artifact, whose
      ;; cached stat would otherwise keep the old size on its card.
      (when (member (format "%s" (plist-get info :name)) '("ApplyPatch" "ToolCall"))
        ;; Clear the small shared cache instead of translating the tool's
        ;; target-native paths a second time.  In particular, a remote patch
        ;; does not name the TRAMP-qualified keys stored by projection.
        (niwa-collaboration--artifact-stat-invalidate))
      (let* ((pending (plist-get room :pending-tools))
             entry)
        (dolist (candidate pending)
          (when (and (null entry)
                     (equal (plist-get candidate :status) "running")
                     (niwa-collaboration--pending-tool-match
                      info candidate))
            (setq entry candidate)))
        (when entry
          (let ((fields (niwa-collaboration--tool-result-fields
                         (plist-get info :result))))
            (dolist (key '(:status :result :truncated))
              (setf (plist-get entry key) (plist-get fields key)))))
        ;; Empty-input observations have no pending row, but their owner may
        ;; now have a new live output tail or terminal facts to publish.
        (when (or entry
                  (and (equal (plist-get info :name) "WriteStdin")
                       (equal (or (plist-get (plist-get info :args) :chars) "") "")))
          (niwa-collaboration--publish room)))))
  nil)

(defun niwa-collaboration--safe-pre-tool (info)
  "Run the live tool-start observer without signaling into gptel."
  (condition-case err
      (niwa-collaboration--pre-tool info)
    (error (niwa-collaboration--observer-failure
            (niwa-collaboration--room-for-buffer (current-buffer)) err)))
  nil)

(defun niwa-collaboration--safe-post-tool (info)
  "Run the live tool-settlement observer without signaling into gptel."
  (condition-case err
      (niwa-collaboration--post-tool info)
    (error (niwa-collaboration--observer-failure
            (niwa-collaboration--room-for-buffer (current-buffer)) err)))
  nil)

(defun niwa-collaboration--observer-failure (room &optional err)
  "Keep ROOM and its last good snapshot after observer failure ERR.
The host is warned and guests told once; the next successful publication
re-arms the report, so a repeating fault does not flood either."
  (when (and room (not (plist-get room :observer-failed)))
    (plist-put room :observer-failed t)
    (condition-case nil
        (progn
          (niwa-diagnostic-warning
           (niwa-diagnostic-scope (plist-get room :session))
           'niwa (format "Live collaboration update failed: %s"
                            (if err (error-message-string err) "unknown error"))
           :warning)
          (niwa-collaboration--broadcast
           room (list :t "notice" :message
                      "Live updates failed. Reconnect to resynchronize.")))
      (error nil))))

(defun niwa-collaboration--safe-post-response (&rest _positions)
  "Schedule response publication without signaling into gptel.
Accept both the stream hook and settled-response hook's arguments."
  (condition-case err
      (when-let* ((room (niwa-collaboration--room-for-buffer
                         (current-buffer))))
        (niwa-collaboration--schedule-publish room))
    (error (niwa-collaboration--observer-failure
            (niwa-collaboration--room-for-buffer (current-buffer)) err))))

(provide 'niwa-collaboration)
;;; niwa-collaboration.el ends here
