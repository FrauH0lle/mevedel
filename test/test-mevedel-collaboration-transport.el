;;; test-mevedel-collaboration-transport.el --- Sealed relay transport tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Crypto, envelope, and codec tests plus live socket tests against an
;; in-process elisp stub relay implementing the exact relay wire contract
;; the Go binary in relay/ speaks.  The Go relay's own behavior is covered
;; by its `go test' suite; this stub keeps `eask test' toolchain-free.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'websocket)
(require 'mevedel-collaboration-transport)


;;
;;; Stub relay

;; websocket.el's server discards the HTTP request path, but the relay
;; contract routes on it.  The stub captures the request line per client
;; process before delegating to the real server filter.

(defun mevedel-test--relay-path (process)
  "Return the captured request path for client PROCESS."
  (process-get process :mevedel-test-path))

(defmacro mevedel-test--with-path-capture (&rest body)
  "Run BODY with `websocket-server-filter' capturing request paths."
  `(cl-letf* ((original (symbol-function 'websocket-server-filter))
              ((symbol-function 'websocket-server-filter)
               (lambda (process output)
                 (unless (process-get process :mevedel-test-path)
                   (when (string-match "\\`[A-Z]+ \\([^ ]+\\)" output)
                     (process-put process :mevedel-test-path
                                  (match-string 1 output))))
                 (funcall original process output))))
     ,@body))

(defun mevedel-test--stub-relay-start (state port)
  "Start a stub relay on PORT recording rooms in STATE.
STATE is a plist placed in a cons cell so handlers can mutate it."
  (websocket-server
   port
   :host 'local
   :on-open
   (lambda (ws)
     (let ((path (mevedel-test--relay-path (websocket-conn ws))))
       (cond
        ((and path (string-match "\\?role=host\\'" path))
         (setcar state (plist-put (car state) :host ws)))
        ((and path (string-match "\\?role=guest\\'" path))
         (let* ((plist (car state))
                (peer (or (plist-get plist :next-peer) 1))
                (guests (or (plist-get plist :guests)
                            (make-hash-table :test #'eql))))
           (puthash peer ws guests)
           (setcar state (plist-put
                          (plist-put (plist-put plist :guests guests)
                                     :next-peer (1+ peer))
                          :peer-of
                          (cons (cons ws peer)
                                (plist-get plist :peer-of))))
           (when-let ((host (plist-get (car state) :host)))
             (websocket-send-text
              host (format "{\"t\":\"peer-joined\",\"peer\":%d}" peer))))))))
   :on-message
   (lambda (ws frame)
     (when (eq (websocket-frame-opcode frame) 'binary)
       (let* ((payload (websocket-frame-payload frame))
              (plist (car state))
              (host (plist-get plist :host))
              (guests (plist-get plist :guests)))
         (when (>= (length payload) 4)
           (if (eq ws host)
               (let ((peer (logior (ash (aref payload 0) 24)
                                   (ash (aref payload 1) 16)
                                   (ash (aref payload 2) 8)
                                   (aref payload 3))))
                 (if (zerop peer)
                     (when guests
                       (maphash (lambda (_peer guest)
                                  (mevedel-test--relay-send-binary
                                   guest payload))
                                guests))
                   (when-let ((guest (and guests (gethash peer guests))))
                     (mevedel-test--relay-send-binary guest payload))))
             (when host
               (let* ((peer (or (cdr (assq ws (plist-get plist :peer-of))) 0))
                      (rewritten (concat (unibyte-string
                                          (logand (ash peer -24) #xff)
                                          (logand (ash peer -16) #xff)
                                          (logand (ash peer -8) #xff)
                                          (logand peer #xff))
                                         (substring payload 4))))
                 (mevedel-test--relay-send-binary host rewritten))))))))
   :on-close
   (lambda (ws)
     (let* ((plist (car state))
            (host (plist-get plist :host))
            (guests (plist-get plist :guests)))
       (cond
        ((eq ws host)
         (setcar state (plist-put plist :host nil))
         (when guests
           (maphash (lambda (_peer guest)
                      (ignore-errors
                        (websocket-send-text guest "{\"t\":\"room-closed\"}")
                        (websocket-close guest)))
                    guests)
           (clrhash guests)))
        ((and guests
              (cl-loop for peer being the hash-keys of guests
                       when (eq (gethash peer guests) ws) return peer))
         (let ((peer (cl-loop for peer being the hash-keys of guests
                              when (eq (gethash peer guests) ws)
                              return peer)))
           (remhash peer guests)
           (when host
             (ignore-errors
               (websocket-send-text
                host
                (format "{\"t\":\"peer-left\",\"peer\":%d}" peer)))))))))))

(defun mevedel-test--relay-send-binary (ws payload)
  "Send unibyte PAYLOAD as one binary frame on WS."
  (ignore-errors
    (websocket-send ws (make-websocket-frame :opcode 'binary
                                             :payload payload
                                             :completep t))))

(defun mevedel-test--pump (predicate &optional timeout)
  "Run the event loop until PREDICATE returns non-nil or TIMEOUT expires.
Return the predicate's final value."
  (let ((deadline (+ (float-time) (or timeout 5)))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    result))

(defun mevedel-test--free-port ()
  "Return a free loopback TCP port."
  (let* ((server (make-network-process :name "mevedel-test-port-probe"
                                       :server t :host 'local :service t
                                       :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defmacro mevedel-test--with-stub-relay (bindings &rest body)
  "Run BODY with a live stub relay bound per BINDINGS (STATE PORT SERVER).
STATE is bound to the one-element mutable list whose car holds the
relay's room plist."
  (declare (indent 1))
  (pcase-let ((`(,state ,port ,server) bindings))
    `(mevedel-test--with-path-capture
      (let* ((,state (list (list :next-peer 1)))
             (,port (mevedel-test--free-port))
             (,server (mevedel-test--stub-relay-start ,state ,port)))
        (ignore ,state)
        (unwind-protect
            (progn ,@body)
          (websocket-server-close ,server))))))


;;
;;; Sealing

(mevedel-deftest mevedel-collaboration--seal
  (:doc "matches the NIST AES-256-GCM vector shape WebCrypto produces")
  (progn
    ;; NIST GCM: 32 zero-byte key, 12 zero-byte nonce, 16 zero-byte
    ;; plaintext -> ciphertext cea7...9d18 with tag d0d1...b919 appended,
    ;; exactly WebCrypto's ciphertext||tag output.
    (let* ((key (make-string 32 0))
           (nonce (make-string 12 0))
           (result (gnutls-symmetric-encrypt
                    "AES-256-GCM" (copy-sequence key) nonce
                    (make-string 16 0)))
           (hex (mapconcat (lambda (byte) (format "%02x" byte))
                           (car result) "")))
      (should (equal (concat "cea7403d4d606b6e074ec5d3baf39d18"
                             "d0d1c8a799996bf0265b98b5d48ab919")
                     hex)))
    (let* ((key (make-string 32 7))
           (sealed (mevedel-collaboration--seal key "héllo → wörld")))
      (should-not (multibyte-string-p sealed))
      (should (equal "héllo → wörld"
                     (mevedel-collaboration--unseal key sealed)))
      ;; Fresh random nonce per frame.
      (should-not (equal sealed
                         (mevedel-collaboration--seal key "héllo → wörld"))))))

(mevedel-deftest mevedel-collaboration--unseal
  (:doc "returns nil for tampered, wrong-key, and short input")
  (let* ((key (make-string 32 7))
         (sealed (mevedel-collaboration--seal key "payload")))
    (should (equal "payload" (mevedel-collaboration--unseal key sealed)))
    (let ((tampered (copy-sequence sealed)))
      (aset tampered (1- (length tampered))
            (logxor (aref tampered (1- (length tampered))) 1))
      (should-not (mevedel-collaboration--unseal key tampered)))
    (should-not (mevedel-collaboration--unseal (make-string 32 8) sealed))
    (should-not (mevedel-collaboration--unseal key "short"))
    (should-not (mevedel-collaboration--unseal key nil))))

;;
;;; Envelope and frame codec

(mevedel-deftest mevedel-collaboration--envelope-pack
  (:doc "round-trips peer ids as a 4-byte big-endian prefix")
  (progn
    (dolist (peer (list 0 1 255 65536 4294967295))
      (let ((envelope (mevedel-collaboration--envelope-pack peer "sealed")))
        (should (equal (cons peer "sealed")
                       (mevedel-collaboration--envelope-unpack envelope)))))
    (should (equal (unibyte-string 0 0 1 0)
                   (substring (mevedel-collaboration--envelope-pack 256 "")
                              0 4)))
    (should-not (mevedel-collaboration--envelope-unpack "abc"))
    (should-not (mevedel-collaboration--envelope-unpack nil))))

(mevedel-deftest mevedel-collaboration--frame-decode
  (:doc "parses frames to plists and returns nil for malformed JSON")
  (progn
    (should (equal '(:t "hello" :proto 2)
                   (mevedel-collaboration--frame-decode
                    "{\"t\":\"hello\",\"proto\":2}")))
    (should-not (mevedel-collaboration--frame-decode "not json"))
    (should-not (mevedel-collaboration--frame-decode ""))
    ;; Encode and decode compose across the sealing boundary.
    (let* ((key (make-string 32 3))
         (frame (list :t "record" :record '(("id" . "a") ("revision" . 1))))
         (roundtrip (mevedel-collaboration--frame-decode
                     (mevedel-collaboration--unseal
                      key
                      (mevedel-collaboration--seal
                       key
                       (mevedel-collaboration--frame-encode frame))))))
      (should (equal "record" (plist-get roundtrip :t)))
      (should (equal "a" (plist-get (plist-get roundtrip :record) :id))))))

;;
;;; Live relay contract

(mevedel-deftest mevedel-collaboration--transport-open
  (:doc "delivers sealed frames and control messages both ways through a relay")
  (mevedel-test--with-stub-relay (state port server)
    (let* ((key (make-string 32 5))
           (frames nil)
           (controls nil)
           (states nil)
           (transport
            (mevedel-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             key
             :on-frame (lambda (peer frame) (push (cons peer frame) frames))
             :on-control (lambda (event peer) (push (cons event peer) controls))
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (mevedel-test--pump
                     (lambda ()
                       (mevedel-collaboration--transport-open-p transport))))
            (should (equal '(open) states))
            ;; A guest joins: the host sees the relay control message.
            (let* ((guest-frames nil)
                   (guest (websocket-open
                           (format
                            "ws://127.0.0.1:%d/r/roomroomroomroom?role=guest"
                            port)
                           :on-message
                           (lambda (_ws frame)
                             (push frame guest-frames)))))
              (unwind-protect
                  (progn
                    (should (mevedel-test--pump
                             (lambda () (equal controls
                                               '((peer-joined . 1))))))
                    ;; Guest -> host: sealed hello arrives decoded with the
                    ;; relay-assigned peer id even when the guest lies.
                    (websocket-send
                     guest
                     (make-websocket-frame
                      :opcode 'binary
                      :payload (mevedel-collaboration--envelope-pack
                                999
                                (mevedel-collaboration--seal
                                 key "{\"t\":\"hello\",\"proto\":2}"))
                      :completep t))
                    (should (mevedel-test--pump (lambda () frames)))
                    (should (equal 1 (caar frames)))
                    (should (equal "hello" (plist-get (cdar frames) :t)))
                    ;; Host -> guest: targeted and broadcast envelopes
                    ;; arrive sealed and unseal to the sent frame.
                    (should (mevedel-collaboration--transport-send
                             transport 1 (list :t "welcome" :proto 2)))
                    (should (mevedel-collaboration--transport-send
                             transport 0 (list :t "record")))
                    (should (mevedel-test--pump
                             (lambda () (= 2 (length guest-frames)))))
                    (let ((decoded
                           (mapcar
                            (lambda (frame)
                              (mevedel-collaboration--frame-decode
                               (mevedel-collaboration--unseal
                                key
                                (cdr (mevedel-collaboration--envelope-unpack
                                      (websocket-frame-payload frame))))))
                            (nreverse guest-frames))))
                      (should (equal '("welcome" "record")
                                     (mapcar (lambda (frame)
                                               (plist-get frame :t))
                                             decoded))))
                    ;; An undecryptable binary frame is dropped silently.
                    (setq frames nil)
                    (websocket-send
                     guest
                     (make-websocket-frame
                      :opcode 'binary
                      :payload (mevedel-collaboration--envelope-pack
                                1 "garbage-not-sealed")
                      :completep t))
                    (websocket-send
                     guest
                     (make-websocket-frame
                      :opcode 'binary
                      :payload (mevedel-collaboration--envelope-pack
                                1 (mevedel-collaboration--seal
                                   key "{\"t\":\"abort\"}"))
                      :completep t))
                    (should (mevedel-test--pump (lambda () frames)))
                    (should (= 1 (length frames)))
                    (should (equal "abort" (plist-get (cdar frames) :t))))
                (websocket-close guest))
              ;; Guest departure reaches the host as peer-left.
              (should (mevedel-test--pump
                       (lambda () (assq 'peer-left controls))))))
        (mevedel-collaboration--transport-stop transport)
        (should (memq 'stopped states))))))

(mevedel-deftest mevedel-collaboration--transport-down
  (:doc "reconnects with backoff after the relay drops and stops cleanly")
  (mevedel-test--with-path-capture
   (let* ((state (list (list :next-peer 1)))
          (port (mevedel-test--free-port))
          (server (mevedel-test--stub-relay-start state port))
          (key (make-string 32 5))
          (states nil)
          (transport
           (mevedel-collaboration--transport-open
            (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
            key
            :on-state (lambda (new) (push new states)))))
     (unwind-protect
         (progn
           (should (mevedel-test--pump
                    (lambda ()
                      (mevedel-collaboration--transport-open-p transport))))
           ;; Relay goes away: the transport reports down and schedules a
           ;; retry instead of dying.
           (websocket-server-close server)
           (should (mevedel-test--pump (lambda () (memq 'down states))))
           (should (timerp (plist-get transport :reconnect-timer)))
           ;; The relay returns on the same port: the transport reconnects
           ;; by itself within the backoff window.
           (setq state (list (list :next-peer 1))
                 server (mevedel-test--stub-relay-start state port))
           (should (mevedel-test--pump
                    (lambda ()
                      (mevedel-collaboration--transport-open-p transport))
                    10))
           (should (equal 'open (car states))))
       (mevedel-collaboration--transport-stop transport)
       (ignore-errors (websocket-server-close server))
       ;; Stopping cancels any retry so no timer leaks.
       (should-not (plist-get transport :reconnect-timer))))))

;;; test-mevedel-collaboration-transport.el ends here
