;;; mevedel-collaboration-transport.el --- collaboration relay transport -*- lexical-binding: t; -*-

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
;; delivers decoded frames and control events to callbacks and reconnects
;; with bounded backoff when the relay connection drops.  It pings the relay
;; on its own interval so a connection that died while this machine slept
;; reports itself instead of looking open forever.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)

;; `json'
(declare-function json-encode "json" (object))

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

(defconst mevedel-collaboration--max-message-bytes (* 1 1024 1024)
  "Encoded bytes one sealed frame may carry.
The bound belongs here because it is the wire's: the relay refuses a
larger frame by closing the connection it arrived on, and for the host
connection it collects the room along with it.")

(defconst mevedel-collaboration--nonce-bytes 12)
(defconst mevedel-collaboration--tag-bytes 16)

(defun mevedel-collaboration--seal (key plaintext)
  "Seal multibyte PLAINTEXT with the 32-byte unibyte KEY.

Return a unibyte string \"[12-byte nonce][ciphertext with appended
16-byte tag]\", the exact shape WebCrypto's AES-GCM produces and
consumes.  GnuTLS zeroes the key it is handed, so KEY is copied."
  (let* ((input (encode-coding-string plaintext 'utf-8 t))
         (result (gnutls-symmetric-encrypt
                  "AES-256-GCM" (copy-sequence key)
                  (list 'iv-auto mevedel-collaboration--nonce-bytes)
                  input)))
    (unless result
      (error "Sealing a collaboration frame failed"))
    (concat (cadr result) (car result))))

(defun mevedel-collaboration--unseal (key sealed)
  "Unseal unibyte SEALED with KEY and return the multibyte plaintext.

Return nil for anything that does not authenticate: wrong key, tampered
bytes, or a payload too short to carry a nonce and tag.  Hostile input
must never signal out of the transport."
  (when (and (stringp sealed)
             (>= (length sealed)
                 (+ mevedel-collaboration--nonce-bytes
                    mevedel-collaboration--tag-bytes)))
    (let ((nonce (substring sealed 0 mevedel-collaboration--nonce-bytes))
          (input (substring sealed mevedel-collaboration--nonce-bytes)))
      (when-let* ((result (condition-case nil
                             (gnutls-symmetric-decrypt
                              "AES-256-GCM" (copy-sequence key) nonce input)
                           (error nil))))
        (decode-coding-string (car result) 'utf-8)))))


;;
;;; Envelope and frame codec

(defconst mevedel-collaboration--envelope-header-bytes 4)

(defun mevedel-collaboration--envelope-pack (peer sealed)
  "Prefix unibyte SEALED with PEER as a 4-byte big-endian id."
  (concat (unibyte-string (logand (ash peer -24) #xff)
                          (logand (ash peer -16) #xff)
                          (logand (ash peer -8) #xff)
                          (logand peer #xff))
          sealed))

(defun mevedel-collaboration--envelope-unpack (envelope)
  "Return (PEER . SEALED) from unibyte ENVELOPE, or nil when malformed."
  (when (and (stringp envelope)
             (>= (length envelope)
                 mevedel-collaboration--envelope-header-bytes))
    (cons (logior (ash (aref envelope 0) 24)
                  (ash (aref envelope 1) 16)
                  (ash (aref envelope 2) 8)
                  (aref envelope 3))
          (substring envelope
                     mevedel-collaboration--envelope-header-bytes))))

(defconst mevedel-collaboration--max-frame-json-bytes
  (- mevedel-collaboration--max-message-bytes
     mevedel-collaboration--envelope-header-bytes
     mevedel-collaboration--nonce-bytes
     mevedel-collaboration--tag-bytes)
  "Encoded JSON bytes one frame may carry.
The wire bound covers what is actually written: the peer header, the
nonce, and the authentication tag travel with the sealed JSON, so they
come out of the same budget.")

(defconst mevedel-collaboration--max-control-bytes 4096
  "Encoded bytes one unencrypted relay control may carry.")

(defun mevedel-collaboration--frame-encode (frame)
  "Serialize FRAME as a JSON string.
FRAME is a plist with keyword keys; nested values may be the alist
shapes the projection already produces, which `json-encode' accepts."
  (json-encode frame))

(defun mevedel-collaboration--frame-decode (text)
  "Parse JSON TEXT into a plist frame, or nil when malformed."
  (condition-case nil
      (json-parse-string text :object-type 'plist :array-type 'list
                         :null-object nil :false-object nil)
    (error nil)))


;;
;;; Relay connection

(defconst mevedel-collaboration--backoff-initial 1)
(defconst mevedel-collaboration--backoff-max 30)
(defconst mevedel-collaboration--keepalive-seconds 30)

(defun mevedel-collaboration--transport-open (url key &rest callbacks)
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
              after `mevedel-collaboration--transport-stop'.
  :headers    an alist of extra handshake headers, resent on every
              reconnect.  The relay may require one to create a room.

Return the transport handle.  The connection retries with bounded
exponential backoff until stopped; undecryptable or malformed input is
dropped silently."
  (let ((transport (list :url url
                         :key key
                         :headers (plist-get callbacks :headers)
                         :ws nil
                         :state 'connecting
                         :backoff mevedel-collaboration--backoff-initial
                         :reconnect-timer nil
                         :on-frame (plist-get callbacks :on-frame)
                         :on-control (plist-get callbacks :on-control)
                         :on-state (plist-get callbacks :on-state)
                         :keepalive-timer nil)))
    (mevedel-collaboration--transport-dial transport)
    (plist-put transport :keepalive-timer
               (run-at-time mevedel-collaboration--keepalive-seconds
                            mevedel-collaboration--keepalive-seconds
                            #'mevedel-collaboration--transport-keepalive
                            transport))
    transport))

(defun mevedel-collaboration--transport-keepalive (transport)
  "Write a ping to TRANSPORT so a dead relay connection reports itself.

The relay pings every 30 seconds and collects the room when a ping times
out, but websocket.el answers a ping inside its own filter and never
surfaces one: a host cannot notice the relay's pings stopping.  A machine
that suspends therefore wakes with a socket its kernel still calls open, a
room the relay collected while it slept, and no reason to redial -- the
share link stays dead until something happens to be sent.  Writing is what
ends it: the relay closed its side, so the peer answers with a reset, the
process sentinel closes the websocket, and the redial re-creates the room.

The ping carries a payload because websocket.el encodes a payload-less one
unmasked, which is exactly the frame a server must refuse.

The reset is what makes this prompt, so a relay that has become entirely
unreachable -- packets dropped rather than refused -- is still only noticed
when the kernel gives up retransmitting.  Detecting that would need a pong
deadline, which websocket.el cannot report without patching it."
  (when (mevedel-collaboration--transport-open-p transport)
    (let ((ws (plist-get transport :ws)))
      (condition-case nil
          (websocket-send ws
                          (make-websocket-frame
                           :opcode 'ping :payload "m" :completep t))
        (error (mevedel-collaboration--transport-down transport ws))))))

(defun mevedel-collaboration--transport-notify (transport state)
  "Report STATE through TRANSPORT's `:on-state' callback, if any."
  (when-let* ((callback (plist-get transport :on-state)))
    (funcall callback state)))

(defun mevedel-collaboration--transport-dial (transport)
  "Dial TRANSPORT's relay URL and install the socket callbacks."
  (plist-put transport :reconnect-timer nil)
  (condition-case nil
      (plist-put
       transport :ws
       (websocket-open
        (plist-get transport :url)
        :custom-header-alist (plist-get transport :headers)
        :on-open
        (lambda (_ws)
          (plist-put transport :state 'open)
          (plist-put transport :backoff
                     mevedel-collaboration--backoff-initial)
          (mevedel-collaboration--transport-notify transport 'open))
        :on-message
        (lambda (_ws frame)
          (mevedel-collaboration--transport-receive transport frame))
        :on-close
        (lambda (ws)
          (mevedel-collaboration--transport-down transport ws))
        :on-error
        (lambda (_ws _type _error)
          ;; Callback errors must not leak into websocket.el's filter;
          ;; a broken connection surfaces through on-close.
          nil)))
    ;; A synchronous dial failure (DNS, refused) retries like a drop.
    (error (mevedel-collaboration--transport-down transport nil))))

(defun mevedel-collaboration--transport-receive (transport frame)
  "Decode websocket FRAME for TRANSPORT and dispatch it."
  (condition-case nil
      (pcase (websocket-frame-opcode frame)
        ('binary
         (when-let* ((envelope (mevedel-collaboration--envelope-unpack
                                (websocket-frame-payload frame)))
                     (text (mevedel-collaboration--unseal
                            (plist-get transport :key) (cdr envelope)))
                     (decoded (mevedel-collaboration--frame-decode text))
                     (callback (plist-get transport :on-frame)))
           (funcall callback (car envelope) decoded)))
        ('text
         (when-let* ((control (mevedel-collaboration--frame-decode
                               (websocket-frame-text frame)))
                     (event (pcase (plist-get control :t)
                              ("peer-joined" 'peer-joined)
                              ("peer-left" 'peer-left)))
                     (callback (plist-get transport :on-control)))
           (funcall callback event (plist-get control :peer)))))
    (error nil)))

(defun mevedel-collaboration--transport-down (transport ws)
  "Handle TRANSPORT's dropped or failed WS connection.
WS is nil when dialing failed before a connection was created."
  (when (and (not (memq (plist-get transport :state) '(stopped down)))
             (eq ws (plist-get transport :ws)))
    (plist-put transport :ws nil)
    (plist-put transport :state 'down)
    (when ws
      (ignore-errors (delete-process (websocket-conn ws))))
    (mevedel-collaboration--transport-notify transport 'down)
    (let ((backoff (plist-get transport :backoff)))
      (plist-put transport :backoff
                 (min mevedel-collaboration--backoff-max (* 2 backoff)))
      (plist-put transport :reconnect-timer
                 (run-at-time backoff nil
                              #'mevedel-collaboration--transport-redial
                              transport)))))

(defun mevedel-collaboration--transport-redial (transport)
  "Retry TRANSPORT's connection from its reconnect timer."
  (unless (eq (plist-get transport :state) 'stopped)
    (plist-put transport :state 'connecting)
    (mevedel-collaboration--transport-dial transport)))

(defun mevedel-collaboration--transport-open-p (transport)
  "Return non-nil when TRANSPORT has a live relay connection."
  (and (eq (plist-get transport :state) 'open)
       (when-let* ((ws (plist-get transport :ws)))
         (websocket-openp ws))))

(defun mevedel-collaboration--transport-send (transport peer frame)
  "Seal and send plist FRAME to PEER through TRANSPORT.

PEER 0 broadcasts to every guest; PEER N targets one guest.  Return
non-nil when the frame was written.  A closed connection drops the
frame: guests recover state by re-sending hello after their own
reconnect, so nothing is queued across a relay outage.

A frame over the wire bound is dropped here rather than sent.  This is
the one place every frame passes, and the relay must refuse an oversized
one by closing the connection it arrived on -- which for the host means
the relay collects the room, ending the session for every guest."
  (when (mevedel-collaboration--transport-open-p transport)
    (condition-case nil
        (let ((encoded (mevedel-collaboration--frame-encode frame)))
          (when (<= (string-bytes encoded)
                    mevedel-collaboration--max-frame-json-bytes)
            (websocket-send
             (plist-get transport :ws)
             (make-websocket-frame
              :opcode 'binary
              :payload (mevedel-collaboration--envelope-pack
                        peer
                        (mevedel-collaboration--seal
                         (plist-get transport :key)
                         encoded))
              :completep t))
            t))
      (websocket-closed (mevedel-collaboration--transport-down
                         transport (plist-get transport :ws))
                        nil)
      (error nil))))

(defun mevedel-collaboration--transport-control (transport control)
  "Send unencrypted relay CONTROL through TRANSPORT.

Controls carry notification routing metadata only, never session data.
Return non-nil when the bounded JSON object was written."
  (when (mevedel-collaboration--transport-open-p transport)
    (condition-case nil
        (let ((encoded (mevedel-collaboration--frame-encode control)))
          (when (<= (string-bytes encoded)
                    mevedel-collaboration--max-control-bytes)
            (websocket-send-text (plist-get transport :ws) encoded)
            t))
      (websocket-closed (mevedel-collaboration--transport-down
                         transport (plist-get transport :ws))
                        nil)
      (error nil))))

(defun mevedel-collaboration--transport-stop (transport)
  "Stop TRANSPORT: cancel retries and close the connection."
  (plist-put transport :state 'stopped)
  (dolist (key '(:reconnect-timer :keepalive-timer))
    (when-let* ((timer (plist-get transport key)))
      (cancel-timer timer)
      (plist-put transport key nil)))
  (when-let* ((ws (plist-get transport :ws)))
    (plist-put transport :ws nil)
    (condition-case nil
        (websocket-close ws)
      (error nil)))
  (mevedel-collaboration--transport-notify transport 'stopped))

(provide 'mevedel-collaboration-transport)
;;; mevedel-collaboration-transport.el ends here
