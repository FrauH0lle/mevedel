;;; test-mevedel-collaboration-transport.el --- Sealed relay transport tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Crypto, envelope, and codec tests plus live socket tests against an
;; in-process elisp stub relay implementing the exact relay wire contract
;; the Go binary in relay/ speaks.  The Go relay's own behavior is covered
;; by its `go test' suite; this stub keeps `eask test' toolchain-free.

;;; Code:

(require 'json)
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
(require 'mevedel-session-control-fs)
(require 'mevedel-transport)


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
         (if (plist-get (car state) :host)
             (websocket-close ws)
           (setcar state (plist-put (car state) :host ws))))
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
           (when-let* ((host (plist-get (car state) :host)))
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
                   (when-let* ((guest (and guests (gethash peer guests))))
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
    (should (equal '(:t "hello" :proto 3)
                   (mevedel-collaboration--frame-decode
                    "{\"t\":\"hello\",\"proto\":3}")))
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
                       (json-encode frame))))))
      (should (equal "record" (plist-get roundtrip :t)))
      (should (equal "a" (plist-get (plist-get roundtrip :record) :id))))))


;;
;;; Live relay contract

(mevedel-deftest mevedel-collaboration--transport-handshake
  (:doc "gates only collaboration handshakes on an established connection")
  (let* ((server (make-network-process :name "mevedel-test-handshake"
                                       :server t :host 'local :service t
                                       :noquery t))
         (url "ws://127.0.0.1/fixture")
         (mevedel-collaboration--dialing nil)
         (original (lambda (&rest args) (car (last args))))
         client)
    (unwind-protect
        (progn
          ;; An unrelated client keeps the original async handshake behavior.
          (should (mevedel-collaboration--transport-handshake
                   original url server "key" nil nil nil t))
          (should-not (process-get server 'mevedel-collaboration))
          ;; A connection tagged during creation waits for its sentinel.
          (let ((mevedel-collaboration--dialing (list :url url)))
            (should-not (mevedel-collaboration--transport-handshake
                         original url server "key" nil nil nil t)))
          (should (process-get server 'mevedel-collaboration))
          (should-not (mevedel-collaboration--transport-handshake
                       original url server "key" nil nil nil t))
          (setq client (make-network-process
                        :name "mevedel-test-handshake-client" :host "127.0.0.1"
                        :service (process-contact server :service) :noquery t))
          (process-put client 'mevedel-collaboration t)
          (let (called)
            (mevedel-collaboration--transport-handshake
             (lambda (&rest args) (setq called t) (should-not (car (last args))))
             url client "key" nil nil nil t)
            (should called)
            (setq called nil)
            (mevedel-collaboration--transport-handshake
             (lambda (&rest _) (setq called t))
             url client "key" nil nil nil t)
            (should-not called)))
      (when client (delete-process client))
      (delete-process server))))

(mevedel-deftest mevedel-collaboration--transport-dial
  ()
  ,test
  (test)
  :doc "connects without waiting and ignores callbacks after cancellation"
  (mevedel-test--with-stub-relay (state port server)
    (let ((original (symbol-function 'make-network-process))
          network-options transport states)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'make-network-process)
                       (lambda (&rest options)
                         (setq network-options options)
                         (apply original options))))
              (setq transport
                    (mevedel-collaboration--transport-open
                     (format "ws://127.0.0.1:%d/r/async?role=host" port)
                     (make-string 32 5)
                     :on-state (lambda (value) (push value states)))))
            ;; Observe the real socket call, before pumping any callbacks.
            (should (plist-get network-options :nowait))
            (should (eq 'connecting (plist-get transport :state)))
            (let* ((ws (plist-get transport :ws))
                   (late-open (websocket-on-open ws)))
              (mevedel-collaboration--transport-stop transport)
              (funcall late-open ws)
              (should (eq 'stopped (plist-get transport :state)))
              (should (equal '(stopped) states))
              (should-not (plist-get transport :ws))
              (should-not (plist-get transport :reconnect-timer))
              (should-not (plist-get transport :connect-timer))
              (should-not (plist-get transport :keepalive-timer))))
        (when transport
          (mevedel-collaboration--transport-stop transport)))))

  :doc "TLS returns before handshake and preserves certificate verification"
  (let* ((server (make-network-process :name "mevedel-test-tls-pending"
                                       :server t :host 'local :service t
                                       :noquery t))
         (port (process-contact server :service))
         (original (symbol-function 'make-network-process))
         (gnutls-verify-error t)
         (open (symbol-function 'websocket-open))
         options transport open-error)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'websocket-open)
                     (lambda (&rest args)
                       (condition-case err (apply open args)
                         (error (setq open-error err)
                                (signal (car err) (cdr err))))))
                    ((symbol-function 'make-network-process)
                     (lambda (&rest args)
                       (setq options args)
                       (apply original args))))
            (setq transport
                  (mevedel-collaboration--transport-open
                   (format "wss://127.0.0.1:%d/r/async?role=host" port)
                   (make-string 32 5))))
          (should-not open-error)
          ;; This listener deliberately has no TLS responder.  No handshake
          ;; can have completed when dialing returns to the caller.
          (should (eq 'connecting (plist-get transport :state)))
          (should (timerp (plist-get transport :connect-timer)))
          (should (plist-get options :nowait))
          (should (eq t (plist-get (cdr (plist-get options :tls-parameters))
                                   :verify-error)))
          (should gnutls-verify-error))
      (when transport (mevedel-collaboration--transport-stop transport))
      (delete-process server))))

(mevedel-deftest mevedel-collaboration--transport-connect-timeout
  ()
  ,test
  (test)
  :doc "abandons a dial nobody answers and retries it with backoff"
  ;; A listener that accepts and never answers: the dial stays
  ;; `connecting' until its deadline drops it.
  (let* ((server (make-network-process :name "mevedel-test-silent-relay"
                                       :server t :host 'local :service t
                                       :noquery t))
         (port (process-contact server :service))
         (mevedel-collaboration--connect-timeout-seconds 0.2)
         transport states)
    (unwind-protect
        (progn
          (setq transport
                (mevedel-collaboration--transport-open
                 (format "ws://127.0.0.1:%d/r/silent?role=host" port)
                 (make-string 32 5)
                 :on-state (lambda (value) (push value states))))
          (let ((ws (plist-get transport :ws)))
            (should (eq 'connecting (plist-get transport :state)))
            (should (mevedel-test--pump (lambda () (memq 'down states)) 3))
            (should (equal '(down) states))
            (should (eq 'down (plist-get transport :state)))
            (should-not (plist-get transport :ws))
            (should-not (process-live-p (websocket-conn ws)))
            (should-not (plist-get transport :connect-timer))
            (should (timerp (plist-get transport :reconnect-timer)))))
      (when transport (mevedel-collaboration--transport-stop transport))
      (delete-process server)))

  :doc "ignores a deadline that outlived its dial"
  (let* ((stale (list 'stale))
         (transport (list :state 'connecting :ws (list 'current)
                          :connect-timer 'fired)))
    (mevedel-collaboration--transport-connect-timeout transport stale)
    (should (eq 'connecting (plist-get transport :state)))
    (should (equal '(current) (plist-get transport :ws)))
    (should-not (plist-get transport :connect-timer))
    ;; An opened connection is no longer the deadline's concern.
    (setq transport (list :state 'open :ws stale :connect-timer 'fired))
    (mevedel-collaboration--transport-connect-timeout transport stale)
    (should (eq 'open (plist-get transport :state)))
    (should (eq stale (plist-get transport :ws)))))

(mevedel-deftest mevedel-collaboration--transport-cancel-connect-timer
  (:doc "cancels a pending dial deadline and tolerates none")
  (let* ((fired nil)
         (timer (run-at-time 60 nil (lambda () (setq fired t))))
         (transport (list :connect-timer timer)))
    (unwind-protect
        (progn
          (mevedel-collaboration--transport-cancel-connect-timer transport)
          (should-not (plist-get transport :connect-timer))
          (should-not (memq timer timer-list))
          (mevedel-collaboration--transport-cancel-connect-timer transport)
          (should-not (plist-get transport :connect-timer))
          (should-not fired))
      (cancel-timer timer))))

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
            ;; An answered dial has no deadline left, and its handshake
            ;; already counts as inbound traffic.
            (should-not (plist-get transport :connect-timer))
            (should-not (mevedel-collaboration--transport-silent-p transport))
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
                                 key "{\"t\":\"hello\",\"proto\":3}"))
                      :completep t))
                    (should (mevedel-test--pump (lambda () frames)))
                    (should (equal 1 (caar frames)))
                    (should (equal "hello" (plist-get (cdar frames) :t)))
                    ;; Host -> guest: targeted and broadcast envelopes
                    ;; arrive sealed and unseal to the sent frame.
                    (should (mevedel-collaboration--transport-send
                             transport 1 (list :t "welcome" :proto 3)))
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

(mevedel-deftest mevedel-collaboration--transport-deliver ()
  ,test
  (test)
  :doc "authenticated callbacks wait for a remote operation before reading target files"
  (let* ((path (make-temp-file "mevedel-inbound-target-" nil nil "bytes"))
         (key (make-string 32 5))
         nested delivered
         (transport (list :state 'open :key key
                          :on-frame (lambda (_peer _frame)
                                      (setq nested (mevedel-transport-nested-p))
                                      (setq delivered (mevedel-session-control-fs-read-file path)))))
         (frame (make-websocket-frame
                 :opcode 'binary :completep t
                 :payload (mevedel-collaboration--envelope-pack
                           1 (mevedel-collaboration--seal key "{\"t\":\"store-list\"}")))))
    (unwind-protect
        (progn
          (mevedel-transport--handler-advice
           (lambda ()
             (let (timer-list timer-idle-list)
               (mevedel-collaboration--transport-receive transport frame))))
          (should-not nested)
          (should-not delivered)
          (should (mevedel-test--pump (lambda () delivered)))
          (should (equal "bytes" delivered)))
      (mevedel-collaboration--transport-stop transport)
      (delete-file path)))

  :doc "queued frames, new arrivals and nested peer departure retain arrival order"
  (let* ((key (make-string 32 5))
         (transport (list :state 'open :key key))
         seen)
    (plist-put transport :on-control
               (lambda (event _peer) (push event seen)))
    (plist-put transport :on-frame
               (lambda (_peer frame)
                 (let ((number (plist-get frame :n)))
                   (push number seen)
                   (when (= number 1)
                     (mevedel-collaboration--transport-receive
                      transport (make-websocket-frame
                                 :opcode 'text :completep t
                                 :payload "{\"t\":\"peer-left\",\"peer\":1}"))
                     (push 'first-returned seen))
                   ;; A failed callback must not abandon later queued input.
                   (when (= number 2) (error "One frame failed")))))
    (unwind-protect
        (cl-labels ((receive (number)
                      (mevedel-collaboration--transport-receive
                       transport (make-websocket-frame
                                  :opcode 'binary :completep t
                                  :payload (mevedel-collaboration--envelope-pack
                                            1 (mevedel-collaboration--seal
                                               key (json-encode (list :n number))))))))
          (mevedel-transport--handler-advice
           (lambda ()
             (let (timer-list timer-idle-list)
               (receive 1)
               (receive 2))))
          (should-not seen)
          ;; An idle arrival must drain the older entries before itself.
          (receive 3)
          (should (equal '(1 first-returned 2 3 peer-left) (nreverse seen)))
          (should-not (plist-get transport :input-queue))
          (should-not (gethash (plist-get transport :input-key) mevedel-transport--pending)))
      (mevedel-collaboration--transport-stop transport)))

  :doc "a callback that starts target work defers the rest of the queue"
  (let ((transport (list :state 'open)) busy seen)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-transport-busy-p)
                   (lambda (&optional _) busy)))
          (mevedel-collaboration--transport-deliver
           transport
           (lambda ()
             (push 'first seen)
             (mevedel-collaboration--transport-deliver
              transport (lambda () (push 'second seen)))
             (setq busy t)))
          (should (equal '(first) seen))
          (should (gethash (plist-get transport :input-key) mevedel-transport--pending))
          (setq busy nil)
          (should (mevedel-test--pump (lambda () (= 2 (length seen)))))
          (should (equal '(second first) seen)))
      (mevedel-collaboration--transport-stop transport))))

(mevedel-deftest mevedel-collaboration--transport-discard-input
  (:doc "disconnect and stop discard queued callbacks and fence a late timer")
  (dolist (action '(down stopped))
    (let ((transport (list :state 'open :backoff 60)) delivered)
      (unwind-protect
          (progn
            (mevedel-transport--handler-advice
             (lambda ()
               (let (timer-list timer-idle-list)
                 (mevedel-collaboration--transport-deliver
                  transport (lambda () (push 'old delivered))))))
            (let* ((key (plist-get transport :input-key))
                   (timer (car (gethash key mevedel-transport--pending)))
                   (function (timer--function timer))
                   (args (timer--args timer)))
              (if (eq action 'down)
                  (mevedel-collaboration--transport-down transport nil)
                (mevedel-collaboration--transport-stop transport))
              (should-not (gethash key mevedel-transport--pending))
              (apply function args)
              (mevedel-collaboration--transport-deliver
               transport (lambda () (push 'closed delivered)))
              (should-not delivered)
              (should-not (plist-get transport :input-queue))
              (when (eq action 'down)
                (plist-put transport :state 'open)
                (mevedel-collaboration--transport-deliver
                 transport (lambda () (push 'new delivered)))
                (should (equal '(new) delivered)))))
        (mevedel-collaboration--transport-stop transport)))))

(mevedel-deftest mevedel-collaboration--transport-send
  (:doc "drops a frame over the wire bound instead of sending it")
  (let* ((sent nil)
         (transport (list :state 'open :ws 'ws :key (make-string 32 ?k))))
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t))
              ((symbol-function 'websocket-conn) (lambda (_ws) 'conn))
              ((symbol-function 'process-send-string)
               (lambda (_process frame) (push frame sent))))
      (should (mevedel-collaboration--transport-send
               transport 1 (list :t "record" :text "small")))
      (should (= 1 (length sent)))
      ;; Already encoded frames travel as they are.
      (should (mevedel-collaboration--transport-send
               transport 1 "{\"t\":\"record\"}"))
      (should (equal "{\"t\":\"record\"}"
                     (mevedel-collaboration--unseal
                      (make-string 32 ?k)
                      (cdr (mevedel-collaboration--envelope-unpack
                            (websocket-frame-payload
                             (websocket-read-frame (car sent))))))))
      (setq sent (cdr sent))
      ;; The relay must refuse an oversized frame by closing the connection
      ;; it arrived on, and for the host that ends the room for every guest.
      (should-not
       (mevedel-collaboration--transport-send
        transport 1
        (list :t "record"
              :text (make-string mevedel-collaboration--max-message-bytes ?x))))
      (should (= 1 (length sent))))))

(mevedel-deftest mevedel-collaboration--websocket-frame
  (:doc "frames every length class as a masked binary frame websocket.el reads back")
  (dolist (size '(0 125 126 65535 65536 300000))
    (let* ((payload (apply #'unibyte-string
                           (cl-loop for index below size collect (% (* index 7) 256))))
           (encoded (mevedel-collaboration--websocket-frame payload))
           (frame (websocket-read-frame encoded)))
      (should-not (multibyte-string-p encoded))
      ;; The mask bit is set: a client must mask what it sends.
      (should (= #x80 (logand (aref encoded 1) #x80)))
      (should (eq 'binary (websocket-frame-opcode frame)))
      (should (websocket-frame-completep frame))
      (should (equal payload (websocket-frame-payload frame)))
      (should (= (length encoded) (websocket-frame-length frame))))))

(mevedel-deftest mevedel-collaboration--transport-control
  (:doc "sends bounded unencrypted relay controls only on an open transport")
  (let* ((transport (list :state 'open :ws 'ws))
         sent)
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t))
              ((symbol-function 'websocket-send-text)
               (lambda (_ws text) (push text sent))))
      (should (mevedel-collaboration--transport-control
               transport (list :t "push")))
      (should (equal "push"
                     (plist-get (json-parse-string
                                 (car sent) :object-type 'plist)
                                :t)))
      (plist-put transport :state 'down)
      (should-not (mevedel-collaboration--transport-control
                   transport (list :t "push")))
      (should (= 1 (length sent))))))

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

(mevedel-deftest mevedel-collaboration--transport-keepalive
  ()
  ,test
  (test)
  :doc "a keepalive ping carries a payload so websocket.el masks it"
  (let ((transport (list :state 'open :ws 'ws :inbound-at (float-time)))
        sent)
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t))
              ((symbol-function 'websocket-send)
               (lambda (_ws frame) (setq sent frame))))
      (mevedel-collaboration--transport-keepalive transport))
    (should (eq 'ping (websocket-frame-opcode sent)))
    (should (= 128
               (logand 128
                       (aref (websocket-encode-frame sent t) 1)))))

  :doc "a failing write redials and a stale close leaves the replacement open"
  (mevedel-test--with-stub-relay (state port server)
    (let* ((states nil)
           (transport
            (mevedel-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             (make-string 32 5)
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (mevedel-test--pump
                     (lambda ()
                       (mevedel-collaboration--transport-open-p transport))))
            (should (timerp (plist-get transport :keepalive-timer)))
            (let* ((old (plist-get transport :ws))
                   (stale-close (websocket-on-close old)))
              ;; A suspended machine wakes with this exact state: the process
              ;; still reads open, and only the write finds the reset.
              (cl-letf (((symbol-function 'websocket-send)
                         (lambda (&rest _) (signal 'error (list "reset")))))
                (mevedel-collaboration--transport-keepalive transport))
              (should-not (websocket-openp old))
              (should (memq 'down states))
              (should (timerp (plist-get transport :reconnect-timer)))
              (should (mevedel-test--pump
                       (lambda ()
                         (mevedel-collaboration--transport-open-p transport))))
              (let ((replacement (plist-get transport :ws)))
                (funcall stale-close old)
                (should (eq replacement (plist-get transport :ws)))
                (should (mevedel-collaboration--transport-open-p transport)))))
        (mevedel-collaboration--transport-stop transport)
        (should-not (plist-get transport :keepalive-timer)))))

  :doc "a connection silent past the liveness window is dropped and redialed"
  (mevedel-test--with-stub-relay (state port server)
    (let* ((states nil)
           (transport
            (mevedel-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             (make-string 32 5)
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (mevedel-test--pump
                     (lambda ()
                       (mevedel-collaboration--transport-open-p transport))))
            (let ((old (plist-get transport :ws))
                  (pinged nil))
              ;; A network change leaves exactly this: the relay's side has
              ;; gone quiet, and no write ever fails.
              (set-process-filter
               (websocket-conn (plist-get (car state) :host)) #'ignore)
              (plist-put transport :inbound-at
                         (- (float-time)
                            mevedel-collaboration--liveness-seconds 1))
              (cl-letf (((symbol-function 'websocket-send)
                         (lambda (&rest _) (setq pinged t))))
                (mevedel-collaboration--transport-keepalive transport))
              (should-not pinged)
              (should-not (websocket-openp old))
              (should (equal '(down open) states))
              (should (timerp (plist-get transport :reconnect-timer)))
              (should (mevedel-test--pump
                       (lambda ()
                         (mevedel-collaboration--transport-open-p transport))))
              (should-not (eq old (plist-get transport :ws)))
              (should-not (mevedel-collaboration--transport-silent-p
                           transport))))
        (mevedel-collaboration--transport-stop transport))))

  :doc "input queued while Emacs was busy counts before silence is judged"
  (mevedel-test--with-stub-relay (state port server)
    (let* ((states nil)
           (transport
            (mevedel-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             (make-string 32 5)
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (mevedel-test--pump
                     (lambda ()
                       (mevedel-collaboration--transport-open-p transport))))
            (let ((ws (plist-get transport :ws))
                  (pinged nil))
              (plist-put transport :inbound-at
                         (- (float-time)
                            mevedel-collaboration--liveness-seconds 1))
              ;; The relay's traffic is waiting in the socket, unread.
              (websocket-send-text (plist-get (car state) :host)
                                   "{\"t\":\"peer-joined\",\"peer\":7}")
              (cl-letf* ((send (symbol-function 'websocket-send))
                         ((symbol-function 'websocket-send)
                          (lambda (target frame)
                            (when (eq target ws) (setq pinged t))
                            (funcall send target frame))))
                (mevedel-collaboration--transport-keepalive transport))
              (should pinged)
              (should (eq ws (plist-get transport :ws)))
              (should (equal '(open) states))
              (should-not (mevedel-collaboration--transport-silent-p
                           transport))))
        (mevedel-collaboration--transport-stop transport)))))

(mevedel-deftest mevedel-collaboration--transport-silent-p
  (:doc "measures the time since the connection last received anything")
  (let ((transport (list :inbound-at (float-time))))
    (should-not (mevedel-collaboration--transport-silent-p transport))
    (plist-put transport :inbound-at
               (- (float-time) mevedel-collaboration--liveness-seconds 1))
    (should (mevedel-collaboration--transport-silent-p transport))
    ;; A connection that never received anything is silent.
    (should (mevedel-collaboration--transport-silent-p (list :ws 'ws)))))

(mevedel-deftest mevedel-collaboration--transport-watch-inbound
  (:doc "stamps inbound bytes on the current connection only")
  (let* ((process (make-pipe-process :name "mevedel-test-inbound"
                                     :noquery t :filter #'ignore))
         (ws (websocket-inner-create :conn process :url "ws://fixture"
                                     :accept-string ""))
         (transport (list :ws ws :inbound-at nil)))
    (unwind-protect
        (progn
          (mevedel-collaboration--transport-watch-inbound transport ws)
          (funcall (process-filter process) process "bytes")
          (should (numberp (plist-get transport :inbound-at)))
          ;; A replaced connection no longer speaks for the transport.
          (plist-put transport :inbound-at nil)
          (plist-put transport :ws 'replacement)
          (funcall (process-filter process) process "bytes")
          (should-not (plist-get transport :inbound-at)))
      (delete-process process))))


;;; test-mevedel-collaboration-transport.el ends here
