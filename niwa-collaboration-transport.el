;;; niwa-collaboration-transport.el --- collaboration relay transport -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the sealed WebSocket client transport to a collaboration relay.
;;
;; The host is a dialing client, never a listener: it connects to
;; `wss://<relay>/r/<roomId>?role=host' and exchanges binary envelopes of the
;; shape "[4-byte big-endian peerId][sealed payload]".  Payloads are JSON
;; frames sealed with AES-256-GCM under the room key; the relay routes them
;; blindly and never holds the key.  Unencrypted TEXT control messages from
;; the relay ("peer-joined", "peer-left") carry no session data.
;;
;; This module knows nothing about rooms, guests, or the projection: it
;; delivers decoded frames and control events to callbacks in arrival order
;; once remote I/O is idle, and reconnects
;; with bounded backoff when the relay connection drops.  It pings the relay
;; on its own interval and drops a connection that has carried nothing back
;; for longer than its liveness window, so a connection that died while this
;; machine slept or changed networks is redialed instead of looking open
;; forever.  Each dial gets a deadline too, so one that stalls is retried
;; instead of connecting forever.
;;
;; The relay may record and replay sealed frames, so every guest frame is
;; bound to its connection: on `peer-joined' the host sends that peer a
;; random challenge, and the peer's frames must echo it with a sequence
;; number above the previous one.  Others are refused before delivery.
;; The other way, each guest sends a random nonce of its own, which every
;; frame sent to that peer alone carries back.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)

(defconst niwa-collaboration--protocol-version 5
  "The sealed host/guest payload protocol, checked at hello.
See relay/README.md for what changes with it.")

;; `niwa-collaboration'
(declare-function niwa-collaboration--base64url "niwa-collaboration" (bytes))
(declare-function niwa-collaboration--random-bytes "niwa-collaboration" (count))
(autoload 'niwa-collaboration--base64url "niwa-collaboration")
(autoload 'niwa-collaboration--random-bytes "niwa-collaboration")

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional path))
(declare-function niwa-transport-cancel-pending "niwa-transport" (&optional key))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel delay))
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-cancel-pending "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-utilities'
(declare-function niwa--delete-process "niwa-utilities" (process))
(autoload 'niwa--delete-process "niwa-utilities")

;; `websocket'
(declare-function make-websocket-frame "websocket"
                  (&rest args))
(declare-function websocket-close "websocket" (websocket))
(declare-function websocket-conn "websocket" (cl-x) t)
(declare-function websocket-frame-opcode "websocket" (cl-x) t)
(declare-function websocket-frame-payload "websocket" (cl-x) t)
(declare-function websocket-frame-text "websocket" (frame))
(declare-function websocket-open "websocket"
                  (url &rest plist))
(declare-function websocket-openp "websocket" (websocket))
(declare-function websocket-send "websocket" (websocket frame))
(declare-function websocket-send-text "websocket" (websocket text))


;;
;;; Sealing

(defconst niwa-collaboration--max-message-bytes (* 1 1024 1024)
  "Encoded bytes one sealed frame may carry.
The bound belongs here because it is the wire's: the relay refuses a
larger frame by closing the connection it arrived on, and for the host
connection it collects the room along with it.")

(defconst niwa-collaboration--nonce-bytes 12)
(defconst niwa-collaboration--tag-bytes 16)

(defun niwa-collaboration--seal (key plaintext)
  "Seal multibyte PLAINTEXT with the 32-byte unibyte KEY.

Return a unibyte string \"[12-byte nonce][ciphertext with appended
16-byte tag]\", the exact shape WebCrypto's AES-GCM produces and
consumes.  GnuTLS zeroes the key it is handed, so KEY is copied."
  (let* ((input (encode-coding-string plaintext 'utf-8 t))
         (result (gnutls-symmetric-encrypt
                  "AES-256-GCM" (copy-sequence key)
                  (list 'iv-auto niwa-collaboration--nonce-bytes)
                  input)))
    (unless result
      (error "Sealing a collaboration frame failed"))
    (concat (cadr result) (car result))))

(defun niwa-collaboration--unseal (key sealed)
  "Unseal unibyte SEALED with KEY and return the multibyte plaintext.

Return nil for anything that does not authenticate: wrong key, tampered
bytes, or a payload too short to carry a nonce and tag.  Hostile input
must never signal out of the transport."
  (when (and (stringp sealed)
             (>= (length sealed)
                 (+ niwa-collaboration--nonce-bytes
                    niwa-collaboration--tag-bytes)))
    (let ((nonce (substring sealed 0 niwa-collaboration--nonce-bytes))
          (input (substring sealed niwa-collaboration--nonce-bytes)))
      (when-let* ((result (condition-case nil
                             (gnutls-symmetric-decrypt
                              "AES-256-GCM" (copy-sequence key) nonce input)
                           (error nil))))
        (decode-coding-string (car result) 'utf-8)))))


;;
;;; Envelope and frame codec

(defconst niwa-collaboration--envelope-header-bytes 4)

(defun niwa-collaboration--envelope-pack (peer sealed)
  "Prefix unibyte SEALED with PEER as a 4-byte big-endian id."
  (concat (unibyte-string (logand (ash peer -24) #xff)
                          (logand (ash peer -16) #xff)
                          (logand (ash peer -8) #xff)
                          (logand peer #xff))
          sealed))

(defun niwa-collaboration--envelope-unpack (envelope)
  "Return (PEER . SEALED) from unibyte ENVELOPE, or nil when malformed."
  (when (and (stringp envelope)
             (>= (length envelope)
                 niwa-collaboration--envelope-header-bytes))
    (cons (logior (ash (aref envelope 0) 24)
                  (ash (aref envelope 1) 16)
                  (ash (aref envelope 2) 8)
                  (aref envelope 3))
          (substring envelope
                     niwa-collaboration--envelope-header-bytes))))

(defconst niwa-collaboration--max-wire-json-bytes
  (- niwa-collaboration--max-message-bytes
     niwa-collaboration--envelope-header-bytes
     niwa-collaboration--nonce-bytes
     niwa-collaboration--tag-bytes)
  "Encoded JSON bytes one frame may carry on the wire.
The wire bound covers what is actually written: the peer header, the
nonce, and the authentication tag travel with the sealed JSON, so they
come out of the same budget.")

(defconst niwa-collaboration--max-guest-nonce-chars 64
  "Longest guest nonce the host accepts; it is base64url text.")

(defconst niwa-collaboration--max-binding-bytes
  (string-bytes (format ",\"nonce\":\"%s\""
                        (make-string niwa-collaboration--max-guest-nonce-chars ?n)))
  "Bytes the transport may add to a frame for one guest: its nonce.")

(defconst niwa-collaboration--max-frame-json-bytes
  (- niwa-collaboration--max-wire-json-bytes
     niwa-collaboration--max-binding-bytes)
  "Encoded JSON bytes a caller's frame may use.
The transport adds the receiving guest's nonce to a frame for one guest,
so callers filling frames to this bound leave room for it.")

(define-error 'niwa-collaboration-frame-too-large
  "Collaboration frame too large to send")

(defconst niwa-collaboration--max-control-bytes 4096
  "Encoded bytes one unencrypted relay control may carry.")

(defun niwa-collaboration--frame-decode (text)
  "Parse JSON TEXT into a plist frame, or nil when malformed."
  (condition-case nil
      (json-parse-string text :object-type 'plist :array-type 'list
                         :null-object nil :false-object nil)
    (error nil)))


;;
;;; Relay connection

(defconst niwa-collaboration--backoff-initial 1)
(defconst niwa-collaboration--backoff-max 30)
(defconst niwa-collaboration--keepalive-seconds 15
  "Seconds between the host's pings to the relay.")

(defconst niwa-collaboration--liveness-seconds 45
  "Seconds a live relay connection may go without inbound bytes.
A healthy connection answers each keepalive ping with a pong and also
carries the relay's own pings, so silence for this long means the path
to the relay is gone even though nothing reported it.  The window spans
several keepalives so one late pong is not a drop, and it outlasts the
relay's own dead-host detection (a 30 second ping with a 10 second
timeout), so the redial finds the old room already collected instead of
being refused as a second host.")

(defvar niwa-collaboration--connect-timeout-seconds 30
  "Seconds a dial may go unanswered before it is abandoned and retried.
A nonblocking connect can stall without ever failing: an Emacs daemon
that dialed its relay while starting up was found still `connecting'
long afterwards, with its room unknown to the relay and every link to
it reporting the room closed.  Neither the keepalive, which pings only
open connections, nor a close event would ever have ended it.")

(defvar niwa-collaboration--dialing nil
  "Transport being created during a call to `websocket-open'.")

(defun niwa-collaboration--transport-handshake
    (original url conn key protocols extensions headers nowait)
  "Send this transport's handshake only after CONN finishes connecting.
ORIGINAL is `websocket-ensure-handshake'; remaining arguments are its inputs.
Websocket's async path sends while the process is still `connect', which
makes `process-send-string' wait for TCP/TLS.  Tag our connection during
creation so its later sentinel also uses the established-connection path.
Other websocket clients retain their own behavior."
  (if (or (process-get conn 'niwa-collaboration)
          (and niwa-collaboration--dialing
               (equal url (plist-get niwa-collaboration--dialing :url))))
      (progn
        (unless (process-get conn 'niwa-collaboration)
          (process-put conn 'niwa-collaboration 'pending))
        (when (and (memq (process-status conn) '(open run))
                   (not (eq (process-get conn 'niwa-collaboration) 'sent)))
          (process-put conn 'niwa-collaboration 'sent)
          (funcall original url conn key protocols extensions headers nil)))
    (funcall original url conn key protocols extensions headers nowait)))

(defun niwa-collaboration--transport-open (url key &rest callbacks)
  "Open a reconnecting sealed transport to the relay room at URL.

URL is the complete host room endpoint, for example
\"wss://relay.example.net/r/<roomId>?role=host\".  KEY is the 32-byte
unibyte room key.  CALLBACKS is a plist:

  :on-frame   (lambda (peer frame)) -- an authenticated guest frame,
              already unsealed and decoded to a plist.
  :on-control (lambda (event peer)) -- relay control, EVENT is
              `peer-joined' or `peer-left'.
  :on-state   (lambda (state)) -- `open' after each (re)connect,
              `down' after a drop that will be retried, `stopped'
              after `niwa-collaboration--transport-stop'.
  :headers    an alist of extra handshake headers, resent on every
              reconnect.  The relay may require one to create a room.

Return the transport handle without waiting for TCP/TLS connection setup.
TLS uses Emacs' configured certificate and hostname verification policy.
The connection retries with bounded exponential backoff until stopped.
A dial the relay has not answered within
`niwa-collaboration--connect-timeout-seconds', and an open connection
that has received nothing within `niwa-collaboration--liveness-seconds',
count as drops; undecryptable or malformed input is dropped silently."
  (advice-add 'websocket-ensure-handshake :around
              #'niwa-collaboration--transport-handshake)
  (let ((transport (list :url url
                         :key key
                         :headers (plist-get callbacks :headers)
                         :ws nil
                         :state 'connecting
                         :backoff niwa-collaboration--backoff-initial
                         :reconnect-timer nil
                         :connect-timer nil
                         :on-frame (plist-get callbacks :on-frame)
                         :on-control (plist-get callbacks :on-control)
                         :on-state (plist-get callbacks :on-state)
                         ;; Peer id -> (CHALLENGE LAST-SEQ REFUSED NONCE).
                         :peers (make-hash-table :test #'eql)
                         :inbound-at nil
                         :keepalive-timer nil)))
    (niwa-collaboration--transport-dial transport)
    (plist-put transport :keepalive-timer
               (run-at-time niwa-collaboration--keepalive-seconds
                            niwa-collaboration--keepalive-seconds
                            #'niwa-collaboration--transport-keepalive
                            transport))
    transport))

(defun niwa-collaboration--transport-keepalive (transport)
  "Ping the relay through TRANSPORT, or drop a connection gone silent.

The relay pings every 30 seconds and collects the room when a ping times
out, but websocket.el answers a ping inside its own filter and never
surfaces one or a pong: a host cannot see the relay's traffic stop.  A
connection whose path died -- a suspended machine, a network change, a
relay that became unreachable -- therefore keeps a socket the kernel
calls open, and a write only fails once the kernel receives a reset or
gives up retransmitting, which can take far longer than guests wait.

So every keepalive first checks that something arrived within
`niwa-collaboration--liveness-seconds'; `:inbound-at' records the last
bytes the connection's process received, pongs and relay pings included.
A silent connection is dropped and redialed, which re-creates the room.
Otherwise a ping is written, and a write that fails drops it too.

The ping carries a payload because websocket.el encodes a payload-less one
unmasked, which is exactly the frame a server must refuse."
  (when (niwa-collaboration--transport-open-p transport)
    (let ((ws (plist-get transport :ws)))
      (when (niwa-collaboration--transport-silent-p transport)
        ;; Emacs runs due timers before reading waiting output, so after a
        ;; long busy spell the pongs may be queued unread: read them first.
        (ignore-errors
          (accept-process-output (websocket-conn ws) 0 nil 1)))
      (cond
       ;; Reading the queued input closed the connection itself.
       ((not (eq ws (plist-get transport :ws))))
       ((niwa-collaboration--transport-silent-p transport)
        (niwa-collaboration--transport-down transport ws))
       (t
        (condition-case nil
            (websocket-send ws
                            (make-websocket-frame
                             :opcode 'ping :payload "m" :completep t))
          (error (niwa-collaboration--transport-down transport ws))))))))

(defun niwa-collaboration--transport-silent-p (transport)
  "Return non-nil when TRANSPORT received nothing within its liveness window."
  (> (- (float-time) (or (plist-get transport :inbound-at) 0))
     niwa-collaboration--liveness-seconds))

(defun niwa-collaboration--transport-watch-inbound (transport ws)
  "Record in TRANSPORT's `:inbound-at' when WS's process receives bytes.
The connection's own filter is wrapped, so every frame counts, including
the pongs and pings websocket.el handles without surfacing them."
  (add-function :before (process-filter (websocket-conn ws))
                (lambda (_process _output)
                  (when (eq ws (plist-get transport :ws))
                    (plist-put transport :inbound-at (float-time))))))

(defun niwa-collaboration--transport-notify (transport state)
  "Report STATE through TRANSPORT's `:on-state' callback, if any."
  (when-let* ((callback (plist-get transport :on-state)))
    (funcall callback state)))

(defun niwa-collaboration--transport-cancel-connect-timer (transport)
  "Cancel TRANSPORT's pending dial deadline, if any."
  (when-let* ((timer (plist-get transport :connect-timer)))
    (cancel-timer timer)
    (plist-put transport :connect-timer nil)))

(defun niwa-collaboration--transport-connect-timeout (transport ws)
  "Abandon TRANSPORT's dial of WS when the relay still has not answered.
The dial is dropped like a broken connection, so it is retried with the
usual backoff."
  (plist-put transport :connect-timer nil)
  (when (and (eq (plist-get transport :state) 'connecting)
             (eq ws (plist-get transport :ws)))
    (niwa-collaboration--transport-down transport ws)))

(defun niwa-collaboration--transport-dial (transport)
  "Dial TRANSPORT's relay URL asynchronously and install socket callbacks.
The dial has `niwa-collaboration--connect-timeout-seconds' to open."
  (plist-put transport :reconnect-timer nil)
  (condition-case nil
      (let ((ws
             (let ((niwa-collaboration--dialing transport))
               (websocket-open
                (plist-get transport :url)
                :nowait t
                :custom-header-alist (plist-get transport :headers)
                :on-open
                (lambda (ws)
                  (when (and (eq ws (plist-get transport :ws))
                             (eq (plist-get transport :state) 'connecting))
                    (niwa-collaboration--transport-cancel-connect-timer
                     transport)
                    (plist-put transport :state 'open)
                    (plist-put transport :backoff
                               niwa-collaboration--backoff-initial)
                    (niwa-collaboration--transport-notify transport 'open)))
                :on-message
                (lambda (ws frame)
                  (when (eq ws (plist-get transport :ws))
                    (niwa-collaboration--transport-receive transport frame)))
                :on-close
                (lambda (ws)
                  (niwa-collaboration--transport-down transport ws))
                :on-error
                (lambda (_ws _type _error)
                  ;; Callback errors must not leak into websocket.el's filter;
                  ;; a broken connection surfaces through on-close.
                  nil)))))
        (plist-put transport :ws ws)
        (niwa-collaboration--transport-watch-inbound transport ws)
        (plist-put transport :connect-timer
                   (run-at-time niwa-collaboration--connect-timeout-seconds nil
                                #'niwa-collaboration--transport-connect-timeout
                                transport ws)))
    ;; A synchronous dial failure (DNS, refused) retries like a drop.
    (error (niwa-collaboration--transport-down transport nil))))

(defun niwa-collaboration--transport-discard-input (transport)
  "Forget TRANSPORT's queued callbacks and cancel their idle opportunity."
  (plist-put transport :input-queue nil)
  (plist-put transport :input-tail nil)
  (when-let* ((key (plist-get transport :input-key)))
    (niwa-transport-cancel-pending key)))

(defun niwa-collaboration--transport-deliver (transport callback &rest args)
  "Deliver CALLBACK with ARGS in TRANSPORT's input order when target I/O is idle.
A callback may yield to another websocket filter during remote I/O; that
filter appends its callback rather than nesting it.  Frames and peer controls
share this queue so a departure cannot overtake an earlier hello."
  (unless (memq (plist-get transport :state) '(down stopped))
    (let ((cell (list (cons callback args))))
      (if-let* ((tail (plist-get transport :input-tail)))
          (setcdr tail cell)
        (plist-put transport :input-queue cell))
      (plist-put transport :input-tail cell))
    (unless (plist-get transport :input-active)
      (let ((key (or (plist-get transport :input-key)
                     (let ((owner (make-symbol "collaboration-input")))
                       (plist-put transport :input-key owner)
                       owner))))
        (cl-labels
            ((discard () (niwa-collaboration--transport-discard-input transport))
             (drain ()
               (plist-put transport :input-active t)
               (unwind-protect
                   (while (and (not (memq (plist-get transport :state) '(down stopped)))
                               (plist-get transport :input-queue)
                               (not (niwa-transport-busy-p)))
                     (let* ((queue (plist-get transport :input-queue))
                            (entry (car queue)))
                       (plist-put transport :input-queue (cdr queue))
                       (unless (cdr queue) (plist-put transport :input-tail nil))
                       ;; Preserve the existing per-frame failure isolation.
                       (condition-case nil (apply (car entry) (cdr entry))
                         (error nil))))
                 (plist-put transport :input-active nil))
               (when (plist-get transport :input-queue)
                 ;; A callback can start asynchronous target work.  Re-admit
                 ;; the remainder on another opportunity, without recursion.
                 (unless (niwa-transport-run-when-idle key nil #'drain #'discard 0)
                   (discard)))))
          (unless (niwa-transport-run-when-idle key nil #'drain #'discard)
            (discard)))))))


(defconst niwa-collaboration--max-unbound-peers 256
  "Unbound peers remembered as already told, so each hears once.")

(cl-defstruct (niwa-collaboration--binding
               (:constructor niwa-collaboration--make-binding))
  "State of one guest connection.  Unannounced peers have no challenge."
  challenge (last-seq 0) refused nonce)

(defun niwa-collaboration--protocol-error ()
  "Return the refusal for a guest speaking an incompatible protocol."
  (list :t "error" :code "protocol-mismatch" :proto niwa-collaboration--protocol-version
        :message (format "Protocol mismatch: host speaks %d. Reload the viewer after updating the relay, or update niwa on the host."
                         niwa-collaboration--protocol-version)))

(defun niwa-collaboration--transport-refusal (transport peer frame)
  "Return nil when guest FRAME belongs to PEER's current TRANSPORT connection.
Otherwise return t to drop it silently, or the frame that refuses it.
Only a binding's first refusal is answered.  A frame without a challenge
comes from a client of an older protocol; one with a stale challenge or a
sequence number at or below the last accepted was repeated or reordered."
  (let* ((peers (plist-get transport :peers))
         (binding (gethash peer peers))
         (expected (and binding (niwa-collaboration--binding-challenge binding)))
         (challenge (plist-get frame :challenge))
         (nonce (plist-get frame :nonce))
         (seq (plist-get frame :seq)))
    (cond
     ((and expected (equal challenge expected)
           (natnump seq) (> seq (niwa-collaboration--binding-last-seq binding))
           ;; The nonce identifies the socket: every frame needs a valid one,
           ;; so the first accepted frame always adopts it, and a relay
           ;; handing another socket this peer's recorded challenge cannot pass.
           (stringp nonce)
           (string-match-p
            (format "\\`[A-Za-z0-9_-]\\{1,%d\\}\\'"
                    niwa-collaboration--max-guest-nonce-chars)
            nonce)
           (let ((bound-nonce (niwa-collaboration--binding-nonce binding)))
             (or (null bound-nonce) (equal nonce bound-nonce))))
      (setf (niwa-collaboration--binding-last-seq binding) seq
            (niwa-collaboration--binding-nonce binding) nonce)
      nil)
     ((and binding (niwa-collaboration--binding-refused binding)) t)
     ((not binding)
      (if (>= (hash-table-count peers) niwa-collaboration--max-unbound-peers)
          t
        (puthash peer (niwa-collaboration--make-binding :refused t) peers)
        (list :t "notice"
              :message "The host ignored a frame from a connection it has not bound yet.")))
     (t
      (setf (niwa-collaboration--binding-refused binding) t)
      (if challenge
          (list :t "notice"
                :message "The host ignored a frame the relay repeated or reordered.")
        (niwa-collaboration--protocol-error))))))

(defun niwa-collaboration--transport-receive (transport frame)
  "Decode websocket FRAME for TRANSPORT and dispatch it."
  (condition-case nil
      (pcase (websocket-frame-opcode frame)
        ('binary
         (when-let* ((envelope (niwa-collaboration--envelope-unpack
                                (websocket-frame-payload frame)))
                     (text (niwa-collaboration--unseal
                            (plist-get transport :key) (cdr envelope)))
                     (decoded (niwa-collaboration--frame-decode text))
                     (callback (plist-get transport :on-frame)))
           (let* ((peer (car envelope))
                  (refusal (niwa-collaboration--transport-refusal
                            transport peer decoded)))
             (cond
              ((consp refusal)
               (niwa-collaboration--transport-deliver
                transport #'niwa-collaboration--transport-send
                transport peer refusal))
              ((not refusal)
               (niwa-collaboration--transport-deliver
                transport callback peer decoded))))))
        ('text
         (when-let* ((control (niwa-collaboration--frame-decode
                               (websocket-frame-text frame)))
                     (event (pcase (plist-get control :t)
                              ("peer-joined" 'peer-joined)
                              ("peer-left" 'peer-left))))
           (let ((peer (plist-get control :peer))
                 (peers (plist-get transport :peers)))
             (if (eq event 'peer-left)
                 (remhash peer peers)
               ;; A fresh challenge per connection, a reconnect included.
               (let ((challenge (niwa-collaboration--base64url
                                 (niwa-collaboration--random-bytes 16))))
                 (puthash peer (niwa-collaboration--make-binding :challenge challenge) peers)
                 (niwa-collaboration--transport-deliver
                  transport #'niwa-collaboration--transport-send
                  transport peer (list :t "challenge" :challenge challenge))))
             (when-let* ((callback (plist-get transport :on-control)))
               (niwa-collaboration--transport-deliver
                transport callback event peer))))))
    (error nil)))

(defun niwa-collaboration--transport-down (transport ws)
  "Handle TRANSPORT's dropped or failed WS connection.
WS is nil when dialing failed before a connection was created."
  (when (and (not (memq (plist-get transport :state) '(stopped down)))
             (eq ws (plist-get transport :ws)))
    (niwa-collaboration--transport-cancel-connect-timer transport)
    (plist-put transport :ws nil)
    (plist-put transport :state 'down)
    ;; The relay drops every guest with the host; each rejoins afresh.
    (clrhash (plist-get transport :peers))
    (niwa-collaboration--transport-discard-input transport)
    (when ws
      (ignore-errors (niwa--delete-process (websocket-conn ws))))
    (niwa-collaboration--transport-notify transport 'down)
    (let ((backoff (plist-get transport :backoff)))
      (plist-put transport :backoff
                 (min niwa-collaboration--backoff-max (* 2 backoff)))
      (plist-put transport :reconnect-timer
                 (run-at-time backoff nil
                              #'niwa-collaboration--transport-redial
                              transport)))))

(defun niwa-collaboration--transport-redial (transport)
  "Retry TRANSPORT's connection from its reconnect timer."
  (unless (eq (plist-get transport :state) 'stopped)
    (plist-put transport :state 'connecting)
    (niwa-collaboration--transport-dial transport)))

(defun niwa-collaboration--transport-open-p (transport)
  "Return non-nil when TRANSPORT has a live relay connection."
  (and (eq (plist-get transport :state) 'open)
       (when-let* ((ws (plist-get transport :ws)))
         (websocket-openp ws))))

(defun niwa-collaboration--websocket-frame (payload)
  "Return a final, masked binary websocket frame carrying unibyte PAYLOAD.
websocket.el builds a list of every payload byte to mask and assemble a
frame, about 450 ms for one 400 KB snapshot chunk on the deployed host;
this loop masks in place."
  (let* ((size (length payload))
         (k0 (random 256)) (k1 (random 256)) (k2 (random 256)) (k3 (random 256))
         (masked (make-string size 0))
         (index 0))
    (while (< index size)
      (aset masked index
            (logxor (aref payload index)
                    (pcase (logand index 3) (0 k0) (1 k1) (2 k2) (_ k3))))
      (setq index (1+ index)))
    (concat
     ;; FIN and the binary opcode, then the masked length.
     (cond ((< size 126) (unibyte-string #x82 (logior #x80 size)))
           ((< size 65536)
            (unibyte-string #x82 (logior #x80 126) (ash size -8) (logand size #xff)))
           (t (apply #'unibyte-string #x82 (logior #x80 127)
                     (cl-loop for shift from 56 downto 0 by 8
                              collect (logand (ash size (- shift)) #xff)))))
     (unibyte-string k0 k1 k2 k3)
     masked)))

(defconst niwa-collaboration--broadcast-types
  '("record" "remove" "notice" "status" "store-artifacts" "agents"
    "history-index" "goal" "tasks" "background-work" "discussions" "bye"
    "editing-event")
  "Frame types the host may send to every guest at once.
Broadcasts carry no guest's nonce, so a relay can replay them to any
guest; a reply or anything else meant for one guest must name its peer.")

(defun niwa-collaboration--transport-send (transport peer frame)
  "Seal and send plist FRAME to PEER through TRANSPORT.

PEER 0 broadcasts to every guest, only the frame types in
`niwa-collaboration--broadcast-types' (others signal an error);
PEER N targets one guest.  Return
non-nil when the frame was written.  A closed connection drops the
frame: guests recover state by re-sending hello after their own
reconnect, so nothing is queued across a relay outage.

A frame over the wire bound signals `niwa-collaboration-frame-too-large'
rather than being sent: the relay refuses an oversized frame by closing
the connection it arrived on, which for the host collects the room and
ends the session for every guest.  Callers keep frames within
`niwa-collaboration--max-frame-json-bytes', which leaves room for the
nonce added here; the error surfaces any frame that does not, instead of
losing it silently."
  (when (and (zerop peer)
             (not (member (plist-get frame :t) niwa-collaboration--broadcast-types)))
    (error "Frame type %S may not be broadcast; send it to its peer" (plist-get frame :t)))
  (when (niwa-collaboration--transport-open-p transport)
    (when-let* ((encoded
                 (condition-case nil
                     (let ((nonce (when-let* (((/= peer 0))
                                              (peers (plist-get transport :peers))
                                              (binding (gethash peer peers)))
                                    (niwa-collaboration--binding-nonce binding))))
                       ;; A frame for one peer carries back its nonce, so the
                       ;; relay cannot answer a later connection with it.
                       (json-encode (if nonce (plist-put (copy-sequence frame) :nonce nonce) frame)))
                   (error nil))))
      (let ((bytes (string-bytes encoded)))
        (when (> bytes niwa-collaboration--max-wire-json-bytes)
          (signal 'niwa-collaboration-frame-too-large
                  (list (format "%d bytes over the %d-byte bound"
                                (- bytes niwa-collaboration--max-wire-json-bytes)
                                niwa-collaboration--max-wire-json-bytes)))))
      (condition-case nil
          (let ((ws (plist-get transport :ws)))
            (unless (websocket-openp ws)
              (signal 'websocket-closed (list frame)))
            (process-send-string
             (websocket-conn ws)
             (niwa-collaboration--websocket-frame
              (niwa-collaboration--envelope-pack
               peer
               (niwa-collaboration--seal (plist-get transport :key) encoded))))
            t)
        (websocket-closed (niwa-collaboration--transport-down
                           transport (plist-get transport :ws))
                          nil)
        (error nil)))))

(defun niwa-collaboration--transport-control (transport control)
  "Send unencrypted relay CONTROL through TRANSPORT.

Controls carry notification routing metadata only, never session data.
Return non-nil when the bounded JSON object was written."
  (when (niwa-collaboration--transport-open-p transport)
    (condition-case nil
        (let ((encoded (json-encode control)))
          (when (<= (string-bytes encoded)
                    niwa-collaboration--max-control-bytes)
            (websocket-send-text (plist-get transport :ws) encoded)
            t))
      (websocket-closed (niwa-collaboration--transport-down
                         transport (plist-get transport :ws))
                        nil)
      (error nil))))

(defun niwa-collaboration--transport-stop (transport)
  "Stop TRANSPORT: cancel retries and close the connection."
  (plist-put transport :state 'stopped)
  (niwa-collaboration--transport-discard-input transport)
  (dolist (key '(:reconnect-timer :connect-timer :keepalive-timer))
    (when-let* ((timer (plist-get transport key)))
      (cancel-timer timer)
      (plist-put transport key nil)))
  (when-let* ((ws (plist-get transport :ws)))
    (plist-put transport :ws nil)
    (condition-case nil
        (if (eq (process-status (websocket-conn ws)) 'connect)
            (niwa--delete-process (websocket-conn ws))
          (websocket-close ws))
      (error nil)))
  (niwa-collaboration--transport-notify transport 'stopped))

(provide 'niwa-collaboration-transport)
;;; niwa-collaboration-transport.el ends here
