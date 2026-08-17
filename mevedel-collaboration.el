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
(declare-function mevedel-collaboration--tool-detail
                  "mevedel-collaboration-projection" (args))
(declare-function mevedel-collaboration--tool-records
                  "mevedel-collaboration-projection" (records))
(declare-function mevedel-collaboration--tool-result-fields
                  "mevedel-collaboration-projection" (result))
(defvar mevedel-collaboration--max-message-bytes)
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

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))

;; `mevedel-structs'
(declare-function mevedel-session-enqueue-pending-input
                  "mevedel-structs" (session category entry))
(declare-function mevedel-session-session-id "mevedel-structs" (session))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text
                  "mevedel-utilities" (text))

;; `mevedel-view'
(declare-function mevedel-view--abort-data-buffer
                  "mevedel-view" (data-buffer))

;; `mevedel-view-composer'
(declare-function mevedel-view--schedule-late-follow-up-drain
                  "mevedel-view-composer" ())

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())

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

(defcustom mevedel-collaboration-share-ttl 3600
  "Seconds after which an active share stops itself, or nil for no limit.

The room and both bearer links die with the share, so forgotten links
do not stay live.  The relay's max-room-age is only a backstop against
a crashed host; this timer is the policy."
  :type '(choice (const :tag "Until stopped" nil)
                 (integer :tag "Seconds"))
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
(defconst mevedel-collaboration--duplicate-prompt-window 3.0
  "Seconds within which an identical prompt from one guest is dropped.
A human re-sending the same text this fast is a double-fired client
event (double click, stale viewer), not a second question.")

(defvar mevedel-collaboration--room nil
  "The one process-wide live collaboration room, or nil.")

;;
;;; Small data helpers

(defun mevedel-collaboration--room-data-buffer (&optional room)
  "Return the live data buffer for ROOM, or nil."
  (let ((buffer (plist-get (or room mevedel-collaboration--room)
                           :data-buffer)))
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
  (when-let ((guests (plist-get room :guests)))
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

;;
;;; Room publication

(defun mevedel-collaboration--broadcast (room frame)
  "Broadcast FRAME to every guest in ROOM when the transport is live."
  (when-let ((transport (plist-get room :transport))
             (guests (plist-get room :guests)))
    (when (> (hash-table-count guests) 0)
      (mevedel-collaboration--transport-send transport 0 frame))))

(defun mevedel-collaboration--publish (&optional room)
  "Publish changed records from ROOM to its connected guests."
  (setq room (or room mevedel-collaboration--room))
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
      (setq mevedel-collaboration--room room)
      (dolist (record changed)
        (mevedel-collaboration--broadcast
         room (list :t "record"
                    :record (mevedel-collaboration--json-record record))))
      (when removed
        (mevedel-collaboration--broadcast
         room (list :t "remove" :ids (vconcat removed)))))))

(defun mevedel-collaboration--publish-timer ()
  "Run the coalesced collaboration publication timer."
  (let ((room mevedel-collaboration--room))
    (when room
      (setq mevedel-collaboration--room
            (plist-put room :publish-timer nil))
      (mevedel-collaboration--publish room))))

(defun mevedel-collaboration--safe-accepted-prompt (data-buffer)
  "Publish DATA-BUFFER immediately after an accepted prompt is inserted.

This observer is failure-isolated so a collaboration viewer cannot block the
request or prompt transaction."
  (condition-case nil
      (when-let ((room mevedel-collaboration--room))
        (when (eq data-buffer (plist-get room :data-buffer))
          (mevedel-collaboration--publish room)))
    (error (mevedel-collaboration--observer-failure)))
  nil)

(defun mevedel-collaboration--schedule-publish ()
  "Coalesce assistant stream updates for the active room."
  (when-let ((room mevedel-collaboration--room))
    (unless (plist-get room :publish-timer)
      (setq mevedel-collaboration--room
            (plist-put room :publish-timer
                       (run-at-time mevedel-collaboration--publish-delay nil
                                    #'mevedel-collaboration--publish-timer))))))

;;
;;; Snapshot delivery

(defun mevedel-collaboration--snapshot-chunks (records)
  "Split RECORDS into lists of JSON records each under the wire bound."
  (let ((limit (- mevedel-collaboration--max-message-bytes 1024))
        chunks current (size 0))
    (dolist (record records)
      (let* ((json (mevedel-collaboration--json-record record))
             (bytes (string-bytes
                     (mevedel-collaboration--json-string json))))
        (when (and current (> (+ size bytes) limit))
          (push (nreverse current) chunks)
          (setq current nil size 0))
        (push json current)
        (setq size (+ size bytes))))
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
     (list :t "welcome"
           :proto mevedel-collaboration--protocol-version
           :readOnly (if (plist-get guest :writable) :json-false t)
           :recordCount (length records)))
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
  (list :t "ui-request"
        :reqId request-id
        :body (or (overlay-get overlay 'mevedel--remote-body) "")
        :bodyKind (or (overlay-get overlay 'mevedel--remote-body-kind)
                      "text")
        :options
        (vconcat
         (cl-loop for (_outcome . label)
                  in (overlay-get overlay 'mevedel--remote-options)
                  for index from 0
                  collect `(("id" . ,index) ("label" . ,label))))
        :allowFeedback
        (if (overlay-get overlay 'mevedel--remote-feedback) t :json-false)))

(defun mevedel-collaboration--on-prompt-created (overlay)
  "Present prompt OVERLAY to the active room's writable guests.

A re-render of the same interaction -- the permission queue redraws its
head on every selection change -- reuses the existing request id, so a
guest sees one card updated in place instead of an accumulating pile."
  (when-let ((room mevedel-collaboration--room))
    (when (and mevedel-collaboration-remote-interactions
               (overlay-get overlay 'mevedel--remote-options))
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
  "Dismiss OVERLAY's ui-request from every guest surface."
  (when-let ((room mevedel-collaboration--room))
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
      (when-let ((overlay (gethash request-id (plist-get room :ui-requests))))
        (let* ((options (overlay-get overlay 'mevedel--remote-options))
               (feedback (plist-get frame :feedback))
               (option (plist-get frame :option))
               (feedback-handler
                (overlay-get overlay 'mevedel--remote-feedback))
               (outcome
                (cond
                 ((and (stringp feedback)
                       (not (string-empty-p (string-trim feedback)))
                       feedback-handler)
                  ;; A function handler owns the whole feedback flow, for
                  ;; prompts whose feedback is not a plain settle outcome.
                  (if (functionp feedback-handler)
                      (let ((text (string-trim feedback)))
                        (lambda () (funcall feedback-handler text)))
                    (cons 'feedback (string-trim feedback))))
                 ((and (integerp option) (nth option options))
                  (car (nth option options))))))
          (when (and outcome (buffer-live-p (overlay-buffer overlay)))
            (message "mevedel: interaction answered by guest %s"
                     (plist-get guest :name))
            (with-current-buffer (overlay-buffer overlay)
              (condition-case err
                  (if (functionp outcome)
                      (funcall outcome)
                    (mevedel--prompt--settle overlay outcome))
                (user-error
                 (message "mevedel: remote answer rejected: %s"
                          (error-message-string err)))))))))))

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
                            (equal claimed (plist-get room :write-token)))))
        (puthash peer (list :name name :writable writable :ready t)
                 (plist-get room :guests))
        (mevedel-collaboration--send-snapshot room peer)
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
               (stringp text)
               (<= (string-bytes text)
                   mevedel-collaboration--max-prompt-bytes)
               (not (string-empty-p (string-trim text))))
      ;; The prompt frame may carry a fresher display name than the hello
      ;; did; the badge should show what the guest typed.
      (when (stringp (plist-get frame :name))
        (plist-put guest :name (mevedel-collaboration--sanitize-guest-name
                                (plist-get frame :name))))
      ;; Drop a byte-identical repeat inside the duplicate window: a
      ;; double-fired client submit, not a second question.
      (let ((last (plist-get guest :last-prompt))
            (now (float-time)))
        (when (and last
                   (equal (car last) text)
                   (< (- now (cdr last))
                      mevedel-collaboration--duplicate-prompt-window))
          (cl-return-from mevedel-collaboration--handle-prompt))
        (plist-put guest :last-prompt (cons text now)))
      (let* ((data-buffer (mevedel-collaboration--room-data-buffer room))
             (view-buffer (and data-buffer
                               (buffer-local-value 'mevedel--view-buffer
                                                   data-buffer)))
             (session (and data-buffer
                           (buffer-local-value 'mevedel--session
                                               data-buffer))))
        (when (and session (buffer-live-p view-buffer))
          (with-current-buffer view-buffer
            (require 'mevedel-view-composer)
            (require 'mevedel-utilities)
            (mevedel-session-enqueue-pending-input
             session 'follow-up
             (list :input (mevedel--normalize-message-text text)
                   :guest-name (plist-get guest :name)
                   :queued-at-time (float-time)
                   :queued-at-turn
                   (or (mevedel-session-turn-count session) 0)))
            (mevedel-view--interaction-rebuild)
            (mevedel-view--schedule-late-follow-up-drain))
          (mevedel-collaboration--transport-send
           (plist-get room :transport) peer (list :t "queued")))))))

(defun mevedel-collaboration--handle-abort (room peer)
  "Abort the running request for writable guest PEER."
  (let ((guest (mevedel-collaboration--guest room peer)))
    (when (and guest (plist-get guest :writable))
      (when-let ((data-buffer (mevedel-collaboration--room-data-buffer room)))
        (require 'mevedel-view)
        (mevedel-view--abort-data-buffer data-buffer)))))

(defun mevedel-collaboration--on-frame (peer frame)
  "Dispatch decoded guest FRAME from PEER for the active room.

Failure isolation mirrors the gptel observers: a fault in guest input
handling stops the room instead of leaking into the session."
  (condition-case nil
      (when-let ((room mevedel-collaboration--room))
        (pcase (plist-get frame :t)
          ("hello" (mevedel-collaboration--handle-hello room peer frame))
          ("prompt" (mevedel-collaboration--handle-prompt room peer frame))
          ("abort" (mevedel-collaboration--handle-abort room peer))
          ("ui-response"
           (mevedel-collaboration--handle-ui-response room peer frame))))
    (error (mevedel-collaboration--observer-failure))))

(defun mevedel-collaboration--on-control (event peer)
  "Handle relay control EVENT for PEER."
  (when-let ((room mevedel-collaboration--room))
    (pcase event
      ;; A joined peer becomes a guest only through its hello frame.
      ('peer-joined nil)
      ('peer-left (remhash peer (plist-get room :guests))))))

(defun mevedel-collaboration--on-state (state)
  "Track relay transport STATE for the active room."
  (when-let ((room mevedel-collaboration--room))
    (pcase state
      ;; The relay garbage-collects the room with the host connection, so
      ;; a drop invalidated every guest; they rejoin and re-hello against
      ;; the re-created room.
      ('down (clrhash (plist-get room :guests)))
      ('open nil)
      ('stopped nil))))

;;
;;; Room lifecycle

(defun mevedel-collaboration--stop-internal (&optional reason)
  "Stop the active room and all associated processes and timers."
  (when-let ((room mevedel-collaboration--room))
    ;; Clear the authority before any teardown operation can signal.  The
    ;; local ROOM still supplies the transport and timers to close below.
    (setq mevedel-collaboration--room nil)
    (remove-hook 'kill-emacs-hook #'mevedel-collaboration--stop-for-emacs)
    (remove-hook 'mevedel-interaction-prompt-created-hook
                 #'mevedel-collaboration--on-prompt-created)
    (remove-hook 'mevedel-interaction-prompt-settled-hook
                 #'mevedel-collaboration--on-prompt-settled)
    (when-let ((data-buffer (plist-get room :data-buffer)))
      (when (buffer-live-p data-buffer)
        (with-current-buffer data-buffer
          (remove-hook 'gptel-pre-tool-call-functions
                       #'mevedel-collaboration--safe-pre-tool)
          (remove-hook 'gptel-post-tool-call-functions
                       #'mevedel-collaboration--safe-post-tool))))
    (dolist (key '(:publish-timer :ttl-timer))
      (when-let ((timer (plist-get room key)))
        (cancel-timer timer)))
    (when-let ((transport (plist-get room :transport)))
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
  (when (eq (current-buffer)
            (mevedel-collaboration--room-data-buffer))
    (mevedel-collaboration--stop-internal 'data-buffer-killed)))

(defun mevedel-collaboration--stop-for-session ()
  "Stop sharing from a data buffer's SessionEnd hook."
  (mevedel-collaboration--stop-for-buffer))

(defun mevedel-collaboration--stop-for-emacs ()
  "Stop sharing before Emacs exits."
  (mevedel-collaboration--stop-internal 'emacs-exit))

(defun mevedel-collaboration--stop-for-ttl ()
  "Stop sharing when the share TTL expires."
  (when mevedel-collaboration--room
    (mevedel-collaboration--stop-internal 'ttl-expired)
    (message "mevedel: collaboration share expired")))

(cl-defun mevedel-collaboration--start (session data-buffer)
  "Start a room for SESSION and DATA-BUFFER and return the room.

The early return below needs the block a `cl-defun' establishes; a plain
`defun' would signal `no-catch' instead of returning the live room."
  (when (plist-get mevedel-collaboration--room :transport)
    (let ((room mevedel-collaboration--room))
      (if (eq session (plist-get room :session))
          (cl-return-from mevedel-collaboration--start room)
        (user-error "Collaboration already belongs to session %s"
                    (plist-get room :session-label)))))
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
                   :on-frame #'mevedel-collaboration--on-frame
                   :on-control #'mevedel-collaboration--on-control
                   :on-state #'mevedel-collaboration--on-state))
            (setq mevedel-collaboration--room
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
                        :pending-tools nil
                        :tool-call-occurrences
                        (make-hash-table :test #'equal)
                        :guests (make-hash-table :test #'eql)
                        :ui-requests (make-hash-table :test #'eql)
                        :publish-timer nil
                        :ttl-timer
                        (when mevedel-collaboration-share-ttl
                          (run-at-time mevedel-collaboration-share-ttl nil
                                       #'mevedel-collaboration--stop-for-ttl))))
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
            mevedel-collaboration--room)
        (error
         (condition-case nil
             (if (and mevedel-collaboration--room
                      (eq (plist-get mevedel-collaboration--room :transport)
                          transport))
                 (mevedel-collaboration--stop-internal 'start-failed)
               (when transport
                 (mevedel-collaboration--transport-stop transport)))
           (error nil))
         (signal (car error-data) (cdr error-data)))))))

(defun mevedel-collaboration--report-links (room)
  "Copy ROOM's full link and report both bearer links."
  (kill-new (plist-get room :link-full))
  (message
   (concat "mevedel: full-control link copied to kill ring\n"
           "full: %s\nview: %s\n"
           "Anyone holding a link gets its powers; share both like secrets")
   (plist-get room :link-full)
   (plist-get room :link-view)))

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
         (room mevedel-collaboration--room))
    (unless (and data-buffer session)
      (user-error "No active mevedel session in this buffer"))
    (cond
     ((and room (eq session (plist-get room :session)))
      (mevedel-collaboration--report-links room))
     (room
      (user-error "Collaboration already belongs to session %s"
                  (plist-get room :session-label)))
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
  "Stop the active collaboration room."
  (interactive)
  (if mevedel-collaboration--room
      (progn
        (mevedel-collaboration--stop-internal 'user-stop)
        (message "mevedel: collaboration stopped"))
    (message "mevedel: collaboration is not active")))

(defun mevedel-collaboration-status ()
  "Report active collaboration status without exposing its secrets."
  (interactive)
  (if-let ((room mevedel-collaboration--room))
      (let* ((transport (plist-get room :transport))
             (guests (plist-get room :guests))
             names)
        (maphash (lambda (_peer guest)
                   (push (format "%s%s" (plist-get guest :name)
                                 (if (plist-get guest :writable) "" " (view)"))
                         names))
                 guests)
        (message "mevedel: collaboration active for %s; relay %s; %s"
                 (plist-get room :session-label)
                 (if (mevedel-collaboration--transport-open-p transport)
                     "connected" "reconnecting")
                 (if names
                     (format "guests: %s"
                             (mapconcat #'identity (nreverse names) ", "))
                   "no guest connected")))
    (message "mevedel: collaboration inactive")))


;;
;;; gptel and lifecycle hooks

(defun mevedel-collaboration--pre-tool (info)
  "Publish a running tool record for gptel tool-call INFO."
  (when-let ((room mevedel-collaboration--room))
    (when (eq (current-buffer) (plist-get room :data-buffer))
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
                         (append
                          (when-let ((detail
                                      (mevedel-collaboration--tool-detail
                                       (plist-get info :args))))
                            (list :detail detail))
                          (when-let (((equal name "ApplyPatch"))
                                     (patch (plist-get
                                             (plist-get info :args) :patch))
                                     ((stringp patch)))
                            (list :diff patch))))))
            (puthash call-key (1+ occurrence) occurrences)
            (setq room (plist-put room :pending-tools
                                  (append pending (list entry))))
            (setq mevedel-collaboration--room room)))
        ;; Tool start is intentionally published immediately.  A short tool
        ;; still has a truthful completion transition, while a long tool is
        ;; visible as running before it produces a result.
        (mevedel-collaboration--publish room))))
  nil)

(defun mevedel-collaboration--post-tool (info)
  "Publish the settled result for gptel tool-call INFO."
  (when-let ((room mevedel-collaboration--room))
    (when (eq (current-buffer) (plist-get room :data-buffer))
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
    (error (mevedel-collaboration--observer-failure)))
  nil)

(defun mevedel-collaboration--safe-post-tool (info)
  "Run the live tool-settlement observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-tool info)
    (error (mevedel-collaboration--observer-failure)))
  nil)

(defun mevedel-collaboration--observer-failure ()
  "Stop the room after an observer failure without affecting the request."
  (condition-case nil
      (mevedel-collaboration--stop-internal 'observer-failure)
    (error nil))
  (condition-case nil
      (display-warning
       'mevedel "Live collaboration stopped after an observer failure" :warning)
    (error nil)))

(defun mevedel-collaboration--post-stream ()
  "Schedule a coalesced publication after gptel inserts response text."
  (when (eq (current-buffer) (mevedel-collaboration--room-data-buffer))
    (mevedel-collaboration--schedule-publish)))

(defun mevedel-collaboration--post-response (_start _end)
  "Publish the settled response for the active data buffer."
  (when (eq (current-buffer) (mevedel-collaboration--room-data-buffer))
    (mevedel-collaboration--schedule-publish)))

(defun mevedel-collaboration--safe-post-stream ()
  "Run the stream observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-stream)
    (error (mevedel-collaboration--observer-failure))))

(defun mevedel-collaboration--safe-post-response (start end)
  "Run the response observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-response start end)
    (error (mevedel-collaboration--observer-failure))))

(provide 'mevedel-collaboration)
;;; mevedel-collaboration.el ends here
