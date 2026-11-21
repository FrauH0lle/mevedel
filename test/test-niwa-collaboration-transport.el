;;; test-niwa-collaboration-transport.el --- Sealed relay transport tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Crypto, envelope, and codec tests plus live socket tests against an
;; in-process elisp stub relay implementing the exact relay wire contract
;; the Go binary in relay/ speaks.  The Go relay's own behavior is covered
;; by its `go test' suite; this stub keeps `eask test' toolchain-free.

;;; Code:

(require 'gnutls)
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
(require 'niwa-collaboration-transport)
(require 'niwa-session-control-fs)
(require 'niwa-transport)
(require 'niwa-collaboration-relay-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-collaboration-relay-test-support"))
(require 'niwa-sigpipe-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-sigpipe-test-support"))

(defconst niwa-collaboration-transport-test--file
  (or load-file-name buffer-file-name)
  "This file, which the SIGPIPE-ignoring child loads.")


(niwa-deftest niwa-relay-test--stub-relay-stop
  (:doc "teardown releases accepted sockets without writing to cancelled peers")
  (let ((send (symbol-function 'websocket-send))
        transport client tearing-down cleanup-writes)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'websocket-send)
                     (lambda (&rest args)
                       (if tearing-down
                           (push args cleanup-writes)
                         (apply send args)))))
            (niwa-relay-test--with-stub-relay (state port server)
              (setq transport
                    (niwa-collaboration--transport-open
                     (format "ws://127.0.0.1:%d/r/cleanup?role=host" port)
                     (make-string 32 5)))
              (should (niwa-relay-test--pump
                       (lambda () (eq 'open (plist-get transport :state))) 3))
              (setq client
                    (cl-find-if (lambda (ws) (eq server (websocket-server-conn ws)))
                                websocket-server-websockets))
              (should client)
              (niwa-collaboration--transport-stop transport)
              (setq tearing-down t)))
          (should-not cleanup-writes)
          (should-not (memq client websocket-server-websockets))
          (should-not (process-live-p (websocket-conn client))))
      (when transport (niwa-collaboration--transport-stop transport))
      (when client
        (setq websocket-server-websockets
              (delq client websocket-server-websockets))))))

;;
;;; Sealing

(niwa-deftest niwa-collaboration--seal
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
           (sealed (niwa-collaboration--seal key "héllo → wörld")))
      (should-not (multibyte-string-p sealed))
      (should (equal "héllo → wörld"
                     (niwa-collaboration--unseal key sealed)))
      ;; Fresh random nonce per frame.
      (should-not (equal sealed
                         (niwa-collaboration--seal key "héllo → wörld"))))))

(niwa-deftest niwa-collaboration--unseal
  (:doc "returns nil for tampered, wrong-key, and short input")
  (let* ((key (make-string 32 7))
         (sealed (niwa-collaboration--seal key "payload")))
    (should (equal "payload" (niwa-collaboration--unseal key sealed)))
    (let ((tampered (copy-sequence sealed)))
      (aset tampered (1- (length tampered))
            (logxor (aref tampered (1- (length tampered))) 1))
      (should-not (niwa-collaboration--unseal key tampered)))
    (should-not (niwa-collaboration--unseal (make-string 32 8) sealed))
    (should-not (niwa-collaboration--unseal key "short"))
    (should-not (niwa-collaboration--unseal key nil))))


;;
;;; Envelope and frame codec

(niwa-deftest niwa-collaboration--envelope-pack
  (:doc "round-trips peer ids as a 4-byte big-endian prefix")
  (progn
    (dolist (peer (list 0 1 255 65536 4294967295))
      (let ((envelope (niwa-collaboration--envelope-pack peer "sealed")))
        (should (equal (cons peer "sealed")
                       (niwa-collaboration--envelope-unpack envelope)))))
    (should (equal (unibyte-string 0 0 1 0)
                   (substring (niwa-collaboration--envelope-pack 256 "")
                              0 4)))
    (should-not (niwa-collaboration--envelope-unpack "abc"))
    (should-not (niwa-collaboration--envelope-unpack nil))))

(niwa-deftest niwa-collaboration--frame-decode
  (:doc "parses frames to plists and returns nil for malformed JSON")
  (progn
    (should (equal '(:t "hello" :proto 3)
                   (niwa-collaboration--frame-decode
                    "{\"t\":\"hello\",\"proto\":3}")))
    (should-not (niwa-collaboration--frame-decode "not json"))
    (should-not (niwa-collaboration--frame-decode ""))
    ;; Encode and decode compose across the sealing boundary.
    (let* ((key (make-string 32 3))
         (frame (list :t "record" :record '(("id" . "a") ("revision" . 1))))
         (roundtrip (niwa-collaboration--frame-decode
                     (niwa-collaboration--unseal
                      key
                      (niwa-collaboration--seal
                       key
                       (json-encode frame))))))
      (should (equal "record" (plist-get roundtrip :t)))
      (should (equal "a" (plist-get (plist-get roundtrip :record) :id))))))


;;
;;; Live relay contract

(niwa-deftest niwa-collaboration--transport-handshake
  (:doc "gates only collaboration handshakes on an established connection")
  (let* ((server (make-network-process :name "niwa-test-handshake"
                                       :server t :host 'local :service t
                                       :noquery t))
         (url "ws://127.0.0.1/fixture")
         (niwa-collaboration--dialing nil)
         (original (lambda (&rest args) (car (last args))))
         client)
    (unwind-protect
        (progn
          ;; An unrelated client keeps the original async handshake behavior.
          (should (niwa-collaboration--transport-handshake
                   original url server "key" nil nil nil t))
          (should-not (process-get server 'niwa-collaboration))
          ;; A connection tagged during creation waits for its sentinel.
          (let ((niwa-collaboration--dialing (list :url url)))
            (should-not (niwa-collaboration--transport-handshake
                         original url server "key" nil nil nil t)))
          (should (process-get server 'niwa-collaboration))
          (should-not (niwa-collaboration--transport-handshake
                       original url server "key" nil nil nil t))
          (setq client (make-network-process
                        :name "niwa-test-handshake-client" :host "127.0.0.1"
                        :service (process-contact server :service) :noquery t))
          (process-put client 'niwa-collaboration t)
          (let (called)
            (niwa-collaboration--transport-handshake
             (lambda (&rest args) (setq called t) (should-not (car (last args))))
             url client "key" nil nil nil t)
            (should called)
            (setq called nil)
            (niwa-collaboration--transport-handshake
             (lambda (&rest _) (setq called t))
             url client "key" nil nil nil t)
            (should-not called)))
      (when client (delete-process client))
      (delete-process server))))

(niwa-deftest niwa-collaboration--transport-dial
  ()
  ,test
  (test)
  :doc "connects without waiting and ignores callbacks after cancellation"
  (niwa-relay-test--with-stub-relay (state port server)
    (let ((original (symbol-function 'make-network-process))
          network-options transport states)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'make-network-process)
                       (lambda (&rest options)
                         (setq network-options options)
                         (apply original options))))
              (setq transport
                    (niwa-collaboration--transport-open
                     (format "ws://127.0.0.1:%d/r/async?role=host" port)
                     (make-string 32 5)
                     :on-state (lambda (value) (push value states)))))
            ;; Observe the real socket call, before pumping any callbacks.
            (should (plist-get network-options :nowait))
            (should (eq 'connecting (plist-get transport :state)))
            (let* ((ws (plist-get transport :ws))
                   (late-open (websocket-on-open ws)))
              (niwa-collaboration--transport-stop transport)
              (funcall late-open ws)
              (should (eq 'stopped (plist-get transport :state)))
              (should (equal '(stopped) states))
              (should-not (plist-get transport :ws))
              (should-not (plist-get transport :reconnect-timer))
              (should-not (plist-get transport :connect-timer))
              (should-not (plist-get transport :keepalive-timer))
              ;; Resolving sockets retire at their sentinel, not before open.
              (niwa-test--await 5 "Cancelled transport socket remained live"
                (not (process-live-p (websocket-conn ws))))))
        (when transport
          (niwa-collaboration--transport-stop transport)))))

  :doc "TLS returns before handshake and preserves certificate verification"
  (niwa-test-run-sigpipe-tests
   niwa-collaboration-transport-test--file
   "\\`niwa-collaboration--transport-dial/tls/")

  :doc "cancellation retires a resolving socket after setup without late callbacks"
  (let* ((buffer (generate-new-buffer " *collaboration-resolving*"))
         (process (make-pipe-process :name "collaboration-resolving"
                                     :buffer buffer :noquery t))
         (ws (websocket-inner-create :conn process :url "ws://fixture"
                                     :on-close #'ignore))
         (state 'connect) states deleted-pending
         (transport (list :state 'connecting :ws ws
                          :on-state (lambda (value) (push value states)))))
    (unwind-protect
        (cl-letf* ((status (symbol-function 'process-status))
                   (delete (symbol-function 'delete-process))
                   ((symbol-function 'process-status)
                    (lambda (value)
                      (if (eq value process) state (funcall status value))))
                   ((symbol-function 'delete-process)
                    (lambda (value)
                      (if (and (eq value process) (eq state 'connect))
                          (setq deleted-pending t)
                        (funcall delete value)))))
          (niwa-collaboration--transport-stop transport)
          (should-not deleted-pending)
          (should (equal '(stopped) states))
          (should-not (plist-get transport :ws))
          (should-not (process-buffer process))
          (should (process-live-p process))
          (setq state 'open)
          (funcall (process-sentinel process) process "open")
          (should (eq 'closed (funcall status process)))
          (should (equal '(stopped) states)))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

;; The deliberately unanswered TLS handshake can write after its peer closes.
(when (niwa-test-sigpipe-child-p)
  (niwa-deftest niwa-collaboration--transport-dial/tls
    (:doc "TLS returns before handshake and preserves certificate verification")
    (let* ((server (make-network-process :name "niwa-test-tls-pending"
                                         :server t :host 'local :service t
                                         :noquery t))
           (port (process-contact server :service))
           (original (symbol-function 'make-network-process))
           (gnutls-verify-error t)
           (open (symbol-function 'websocket-open))
           options transport open-error connection)
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
                    (niwa-collaboration--transport-open
                     (format "wss://127.0.0.1:%d/r/async?role=host" port)
                     (make-string 32 5)))
              (setq connection (websocket-conn (plist-get transport :ws))))
            (should-not open-error)
            ;; This listener deliberately has no TLS responder.  No handshake
            ;; can have completed when dialing returns to the caller.
            (should (eq 'connecting (plist-get transport :state)))
            (should (timerp (plist-get transport :connect-timer)))
            (should (plist-get options :nowait))
            (should (eq t (plist-get (cdr (plist-get options :tls-parameters))
                                     :verify-error)))
            (should gnutls-verify-error))
        (when transport (niwa-collaboration--transport-stop transport))
        (delete-process server)
        (when connection
          (niwa-test--await 5 "Cancelled TLS socket remained live"
            (not (process-live-p connection))))))))

(niwa-deftest niwa-collaboration--transport-connect-timeout
  ()
  ,test
  (test)
  :doc "abandons a dial nobody answers and retries it with backoff"
  ;; A listener that accepts and never answers: the dial stays
  ;; `connecting' until its deadline drops it.
  (let* ((server (make-network-process :name "niwa-test-silent-relay"
                                       :server t :host 'local :service t
                                       :noquery t))
         (port (process-contact server :service))
         (niwa-collaboration--connect-timeout-seconds 0.2)
         transport states)
    (unwind-protect
        (progn
          (setq transport
                (niwa-collaboration--transport-open
                 (format "ws://127.0.0.1:%d/r/silent?role=host" port)
                 (make-string 32 5)
                 :on-state (lambda (value) (push value states))))
          (let ((ws (plist-get transport :ws)))
            (should (eq 'connecting (plist-get transport :state)))
            (should (niwa-relay-test--pump (lambda () (memq 'down states)) 3))
            (should (equal '(down) states))
            (should (eq 'down (plist-get transport :state)))
            (should-not (plist-get transport :ws))
            (should-not (process-live-p (websocket-conn ws)))
            (should-not (plist-get transport :connect-timer))
            (should (timerp (plist-get transport :reconnect-timer)))))
      (when transport (niwa-collaboration--transport-stop transport))
      (delete-process server)))

  :doc "ignores a deadline that outlived its dial"
  (let* ((stale (list 'stale))
         (transport (list :state 'connecting :ws (list 'current)
                          :connect-timer 'fired)))
    (niwa-collaboration--transport-connect-timeout transport stale)
    (should (eq 'connecting (plist-get transport :state)))
    (should (equal '(current) (plist-get transport :ws)))
    (should-not (plist-get transport :connect-timer))
    ;; An opened connection is no longer the deadline's concern.
    (setq transport (list :state 'open :ws stale :connect-timer 'fired))
    (niwa-collaboration--transport-connect-timeout transport stale)
    (should (eq 'open (plist-get transport :state)))
    (should (eq stale (plist-get transport :ws)))))

(niwa-deftest niwa-collaboration--transport-cancel-connect-timer
  (:doc "cancels a pending dial deadline and tolerates none")
  (let* ((fired nil)
         (timer (run-at-time 60 nil (lambda () (setq fired t))))
         (transport (list :connect-timer timer)))
    (unwind-protect
        (progn
          (niwa-collaboration--transport-cancel-connect-timer transport)
          (should-not (plist-get transport :connect-timer))
          (should-not (memq timer timer-list))
          (niwa-collaboration--transport-cancel-connect-timer transport)
          (should-not (plist-get transport :connect-timer))
          (should-not fired))
      (cancel-timer timer))))

(defun test-niwa-collaboration-transport--peers (peer)
  "Return a peer table binding PEER's connection to challenge \"c\"."
  (let ((peers (make-hash-table :test #'eql)))
    (puthash peer (niwa-collaboration--make-binding :challenge "c") peers)
    peers))

(niwa-deftest niwa-collaboration--transport-refusal
  (:doc "answers a binding's first refusal by cause, then drops silently")
  (let ((transport (list :peers (test-niwa-collaboration-transport--peers 1))))
    ;; A peer the relay never announced hears once that it is unbound.
    (should (equal '(:t "notice" :message "The host ignored a frame from a connection it has not bound yet.")
                   (niwa-collaboration--transport-refusal
                    transport 2 '(:t "abort" :challenge "c" :seq 1))))
    (should (eq t (niwa-collaboration--transport-refusal
                   transport 2 '(:t "abort" :challenge "c" :seq 2))))
    ;; An older client is told the protocol, once.
    (should (equal (plist-get (niwa-collaboration--protocol-error) :message)
                   (plist-get (niwa-collaboration--transport-refusal
                               transport 1 '(:t "hello" :proto 3))
                              :message)))
    (should (eq t (niwa-collaboration--transport-refusal
                   transport 1 '(:t "abort" :challenge "x" :seq 9))))
    ;; Refusals never block the binding's own frames.
    (should-not (niwa-collaboration--transport-refusal
                 transport 1 '(:t "abort" :challenge "c" :seq 1 :nonce "n")))
    ;; Unbound peers are remembered only up to the bound.
    (let ((niwa-collaboration--max-unbound-peers 3))
      (should (consp (niwa-collaboration--transport-refusal
                      transport 3 '(:t "abort" :challenge "c" :seq 1))))
      (should (eq t (niwa-collaboration--transport-refusal
                     transport 4 '(:t "abort" :challenge "c" :seq 1))))
      (should-not (gethash 4 (plist-get transport :peers))))))

(niwa-deftest niwa-collaboration--transport-refusal/spliced-socket
  (:doc "refuses another socket the relay hands a live peer's recorded challenge")
  (let ((transport (list :peers (test-niwa-collaboration-transport--peers 1))))
    ;; Peer 1's own socket binds its nonce with its first frame.
    (should-not (niwa-collaboration--transport-refusal
                 transport 1 '(:t "hello" :challenge "c" :seq 1 :nonce "socket-a")))
    ;; A second socket, routed by the relay as peer 1 with peer 1's recorded
    ;; challenge and a sequence number past it, is still refused.
    (should (equal "notice"
                   (plist-get (niwa-collaboration--transport-refusal
                               transport 1 '(:t "set-mode" :challenge "c" :seq 50
                                             :nonce "socket-b"))
                              :t)))
    (should (eq t (niwa-collaboration--transport-refusal
                   transport 1 '(:t "prompt" :challenge "c" :seq 51))))
    ;; Peer 1's own frames keep passing.
    (should-not (niwa-collaboration--transport-refusal
                 transport 1 '(:t "prompt" :challenge "c" :seq 2 :nonce "socket-a")))))

(niwa-deftest niwa-collaboration--transport-refusal/missing-nonce
  (:doc "refuses a bound peer's frames without a valid nonce, so the first accepted adopts one")
  (let ((transport (list :peers (test-niwa-collaboration-transport--peers 1))))
    ;; Without a nonce, or with one too long to keep, nothing is accepted
    ;; or adopted; only the first refusal is answered.
    (should (equal "notice"
                   (plist-get (niwa-collaboration--transport-refusal
                               transport 1 '(:t "hello" :challenge "c" :seq 1))
                              :t)))
    (should (eq t (niwa-collaboration--transport-refusal
                   transport 1 (list :t "hello" :challenge "c" :seq 2
                                     :nonce (make-string 65 ?n)))))
    (should-not (niwa-collaboration--binding-nonce (gethash 1 (plist-get transport :peers))))
    ;; A later valid frame is accepted and adopts its nonce ...
    (should-not (niwa-collaboration--transport-refusal
                 transport 1 '(:t "hello" :challenge "c" :seq 3 :nonce "socket-a")))
    (should (equal "socket-a" (niwa-collaboration--binding-nonce (gethash 1 (plist-get transport :peers)))))
    ;; ... which then holds every frame to it.
    (should (eq t (niwa-collaboration--transport-refusal
                   transport 1 '(:t "prompt" :challenge "c" :seq 4))))))

(niwa-deftest niwa-collaboration--transport-send/broadcast-types
  (:doc "refuses to broadcast a frame type outside the bounded list")
  ;; No connection is needed: the check precedes any write.
  (let ((transport (list :state 'down :peers (make-hash-table :test #'eql))))
    (dolist (frame (list '(:t "new-session" :reqId 1 :link "owner-link")
                         '(:t "editing" :reqId 3 :offset 0 :total 4 :data "e30=")
                         "{\"t\":\"queued\",\"id\":1}"
                         "{\"records\":[]}"))
      (should-error (niwa-collaboration--transport-send transport 0 frame)))
    (dolist (frame (list '(:t "record" :record nil)
                         '(:t "editing-event" :offset 0 :total 4 :data "e30=")
                         '(:records [] :t "history-index")))
      (should-not (niwa-collaboration--transport-send transport 0 frame)))
    ;; A frame for one peer may be of any type.
    (should-not (niwa-collaboration--transport-send
                 transport 1 '(:t "new-session" :reqId 1)))))

(niwa-deftest niwa-collaboration--transport-open
  (:doc "delivers sealed frames and control messages both ways through a relay")
  (niwa-relay-test--with-stub-relay (state port server)
    (let* ((key (make-string 32 5))
           (frames nil)
           (controls nil)
           (states nil)
           (transport
            (niwa-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             key
             :on-frame (lambda (peer frame) (push (cons peer frame) frames))
             :on-control (lambda (event peer) (push (cons event peer) controls))
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (niwa-relay-test--pump
                     (lambda ()
                       (niwa-collaboration--transport-open-p transport))))
            (should (equal '(open) states))
            ;; An answered dial has no deadline left, and its handshake
            ;; already counts as inbound traffic.
            (should-not (plist-get transport :connect-timer))
            (should-not (niwa-collaboration--transport-silent-p transport))
            ;; A guest joins: the host sees the relay control message.
            (let* ((guest-frames nil)
                   (challenge nil)
                   (guest (websocket-open
                           (format
                            "ws://127.0.0.1:%d/r/roomroomroomroom?role=guest"
                            port)
                           :on-message
                           (lambda (_ws frame)
                             (push frame guest-frames)))))
              (cl-labels
                  ((decode (frame)
                     (niwa-collaboration--frame-decode
                      (niwa-collaboration--unseal
                       key (cdr (niwa-collaboration--envelope-unpack
                                 (websocket-frame-payload frame))))))
                   ;; A guest frame of TYPE bound to this connection.
                   (bound (peer type seq)
                     (make-websocket-frame
                      :opcode 'binary :completep t
                      :payload (niwa-collaboration--envelope-pack
                                peer (niwa-collaboration--seal
                                      key (json-encode
                                           (list :t type :proto 5
                                                 :challenge challenge
                                                 :seq seq :nonce "guest-nonce")))))))
              (unwind-protect
                  (progn
                    (should (niwa-relay-test--pump
                             (lambda () (equal controls
                                               '((peer-joined . 1))))))
                    ;; The host binds the connection with a fresh challenge.
                    (should (niwa-relay-test--pump (lambda () guest-frames)))
                    (setq challenge
                          (plist-get (decode (car guest-frames)) :challenge))
                    (should (equal "challenge"
                                   (plist-get (decode (car guest-frames)) :t)))
                    (should (= 22 (length challenge)))
                    (setq guest-frames nil)
                    ;; Guest -> host: sealed hello arrives decoded with the
                    ;; relay-assigned peer id even when the guest lies.
                    (websocket-send guest (bound 999 "hello" 1))
                    (should (niwa-relay-test--pump (lambda () frames)))
                    (should (equal 1 (caar frames)))
                    (should (equal "hello" (plist-get (cdar frames) :t)))
                    ;; Host -> guest: targeted and broadcast envelopes
                    ;; arrive sealed and unseal to the sent frame; one for
                    ;; this peer alone carries back its nonce, encoded or not.
                    (should (niwa-collaboration--transport-send
                             transport 1 (list :t "welcome" :proto 5)))
                    (should (niwa-collaboration--transport-send
                             transport 0 (list :t "record")))
                    (should (niwa-collaboration--transport-send
                             transport 1 '(:t "chunk")))
                    (should (niwa-relay-test--pump
                             (lambda () (= 3 (length guest-frames)))))
                    (let ((decoded (mapcar #'decode (nreverse guest-frames))))
                      (should (equal '(("welcome" . "guest-nonce") ("record")
                                       ("chunk" . "guest-nonce"))
                                     (mapcar (lambda (frame)
                                               (cons (plist-get frame :t)
                                                     (plist-get frame :nonce)))
                                             decoded))))
                    ;; An undecryptable binary frame is dropped silently.
                    (setq frames nil)
                    (websocket-send
                     guest
                     (make-websocket-frame
                      :opcode 'binary
                      :payload (niwa-collaboration--envelope-pack
                                1 "garbage-not-sealed")
                      :completep t))
                    (let ((abort (bound 1 "abort" 2)))
                      (websocket-send guest abort)
                      (should (niwa-relay-test--pump (lambda () frames)))
                      (should (= 1 (length frames)))
                      (should (equal "abort" (plist-get (cdar frames) :t)))
                      ;; The same sealed bytes again, a frame from an older
                      ;; client and one from another connection are never
                      ;; delivered; only the first refusal is answered.
                      (setq guest-frames nil)
                      (websocket-send guest abort)
                      (websocket-send
                       guest
                       (make-websocket-frame
                        :opcode 'binary :completep t
                        :payload (niwa-collaboration--envelope-pack
                                  1 (niwa-collaboration--seal
                                     key "{\"t\":\"hello\",\"proto\":3}"))))
                      (let ((own challenge))
                        (setq challenge "AAAAAAAAAAAAAAAAAAAAAA")
                        (websocket-send guest (bound 1 "abort" 3))
                        ;; A valid frame afterwards still arrives: ordering
                        ;; proves the refused ones were handled first.
                        (setq challenge own)
                        (websocket-send guest (bound 1 "set-name" 4)))
                      (should (niwa-relay-test--pump (lambda () (= 2 (length frames)))))
                      (should (equal "set-name" (plist-get (cdar frames) :t)))
                      ;; Every refusal was sent before that delivery; a host
                      ;; marker sent now arrives after all of them.
                      (should (niwa-collaboration--transport-send
                               transport 1 (list :t "marker")))
                      (should (niwa-relay-test--pump
                               (lambda () (equal "marker"
                                                 (plist-get (decode (car guest-frames)) :t)))))
                      (pop guest-frames)
                      (should (equal
                               '(("notice" . "The host ignored a frame the relay repeated or reordered."))
                               (mapcar (lambda (frame)
                                         (let ((frame (decode frame)))
                                           (cons (plist-get frame :t)
                                                 (plist-get frame :message))))
                                       (reverse guest-frames))))))
                (websocket-close guest)))
              ;; Guest departure reaches the host as peer-left.
              (should (niwa-relay-test--pump
                       (lambda () (assq 'peer-left controls))))))
        (niwa-collaboration--transport-stop transport)
        (should (memq 'stopped states))))))

(niwa-deftest niwa-collaboration--transport-deliver ()
  ,test
  (test)
  :doc "authenticated callbacks wait for a remote operation before reading target files"
  (let* ((path (make-temp-file "niwa-inbound-target-" nil nil "bytes"))
         (key (make-string 32 5))
         nested delivered
         (transport (list :state 'open :key key
                          :peers (test-niwa-collaboration-transport--peers 1)
                          :on-frame (lambda (_peer _frame)
                                      (setq nested (niwa-transport-nested-p))
                                      (setq delivered (niwa-session-control-fs-read-file path)))))
         (frame (make-websocket-frame
                 :opcode 'binary :completep t
                 :payload (niwa-collaboration--envelope-pack
                           1 (niwa-collaboration--seal
                              key "{\"t\":\"store-list\",\"challenge\":\"c\",\"seq\":1,\"nonce\":\"n\"}")))))
    (unwind-protect
        (progn
          (niwa-transport--handler-advice
           (lambda ()
             (let (timer-list timer-idle-list)
               (niwa-collaboration--transport-receive transport frame))))
          (should-not nested)
          (should-not delivered)
          (should (niwa-relay-test--pump (lambda () delivered)))
          (should (equal "bytes" delivered)))
      (niwa-collaboration--transport-stop transport)
      (delete-file path)))

  :doc "queued frames, new arrivals and nested peer departure retain arrival order"
  (let* ((key (make-string 32 5))
         (transport (list :state 'open :key key
                          :peers (test-niwa-collaboration-transport--peers 1)))
         seen)
    (plist-put transport :on-control
               (lambda (event _peer) (push event seen)))
    (plist-put transport :on-frame
               (lambda (_peer frame)
                 (let ((number (plist-get frame :n)))
                   (push number seen)
                   (when (= number 1)
                     (niwa-collaboration--transport-receive
                      transport (make-websocket-frame
                                 :opcode 'text :completep t
                                 :payload "{\"t\":\"peer-left\",\"peer\":1}"))
                     (push 'first-returned seen))
                   ;; A failed callback must not abandon later queued input.
                   (when (= number 2) (error "One frame failed")))))
    (unwind-protect
        (cl-labels ((receive (number)
                      (niwa-collaboration--transport-receive
                       transport (make-websocket-frame
                                  :opcode 'binary :completep t
                                  :payload (niwa-collaboration--envelope-pack
                                            1 (niwa-collaboration--seal
                                               key (json-encode
                                                    (list :n number :challenge "c"
                                                          :seq number :nonce "n"))))))))
          (niwa-transport--handler-advice
           (lambda ()
             (let (timer-list timer-idle-list)
               (receive 1)
               (receive 2))))
          (should-not seen)
          ;; An idle arrival must drain the older entries before itself.
          (receive 3)
          (should (equal '(1 first-returned 2 3 peer-left) (nreverse seen)))
          (should-not (plist-get transport :input-queue))
          (should-not (gethash (plist-get transport :input-key) niwa-transport--pending)))
      (niwa-collaboration--transport-stop transport)))

  :doc "a callback that starts target work defers the rest of the queue"
  (let ((transport (list :state 'open)) busy seen)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-transport-busy-p)
                   (lambda (&optional _) busy)))
          (niwa-collaboration--transport-deliver
           transport
           (lambda ()
             (push 'first seen)
             (niwa-collaboration--transport-deliver
              transport (lambda () (push 'second seen)))
             (setq busy t)))
          (should (equal '(first) seen))
          (should (gethash (plist-get transport :input-key) niwa-transport--pending))
          (setq busy nil)
          (should (niwa-relay-test--pump (lambda () (= 2 (length seen)))))
          (should (equal '(second first) seen)))
      (niwa-collaboration--transport-stop transport))))

(niwa-deftest niwa-collaboration--transport-discard-input
  (:doc "disconnect and stop discard queued callbacks and fence a late timer")
  (dolist (action '(down stopped))
    (let ((transport (list :state 'open :backoff 60
                           :peers (make-hash-table :test #'eql)))
          delivered)
      (unwind-protect
          (progn
            (niwa-transport--handler-advice
             (lambda ()
               (let (timer-list timer-idle-list)
                 (niwa-collaboration--transport-deliver
                  transport (lambda () (push 'old delivered))))))
            (let* ((key (plist-get transport :input-key))
                   (timer (car (gethash key niwa-transport--pending)))
                   (function (timer--function timer))
                   (args (timer--args timer)))
              (if (eq action 'down)
                  (niwa-collaboration--transport-down transport nil)
                (niwa-collaboration--transport-stop transport))
              (should-not (gethash key niwa-transport--pending))
              (apply function args)
              (niwa-collaboration--transport-deliver
               transport (lambda () (push 'closed delivered)))
              (should-not delivered)
              (should-not (plist-get transport :input-queue))
              (when (eq action 'down)
                (plist-put transport :state 'open)
                (niwa-collaboration--transport-deliver
                 transport (lambda () (push 'new delivered)))
                (should (equal '(new) delivered)))))
        (niwa-collaboration--transport-stop transport)))))

(niwa-deftest niwa-collaboration--transport-send
  (:doc "refuses a frame over the wire bound loudly, and fits one at the caller budget with a guest's longest nonce")
  (let* ((sent nil)
         ;; A real struct: compiled callers inline its accessors.
         (transport (list :state 'open :ws (websocket-inner-create :url "ws://relay" :conn 'conn)
                          :key (make-string 32 ?k))))
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t))
              ((symbol-function 'process-send-string)
               (lambda (_process frame) (push frame sent))))
      (should (niwa-collaboration--transport-send
               transport 1 (list :t "record" :text "small")))
      (should (= 1 (length sent)))
      (should (niwa-collaboration--transport-send
               transport 1 '(:t "record")))
      (should (equal "{\"t\":\"record\"}"
                     (niwa-collaboration--unseal
                      (make-string 32 ?k)
                      (cdr (niwa-collaboration--envelope-unpack
                            (websocket-frame-payload
                             (websocket-read-frame (car sent))))))))
      (setq sent (cdr sent))
      ;; The relay must refuse an oversized frame by closing the connection
      ;; it arrived on, and for the host that ends the room for every guest.
      ;; Sending it signals rather than losing it silently.
      (should-error
       (niwa-collaboration--transport-send
        transport 1
        (list :t "record"
              :text (make-string niwa-collaboration--max-message-bytes ?x)))
       :type 'niwa-collaboration-frame-too-large)
      (should (= 1 (length sent)))
      ;; A frame filled to the caller budget still fits once the transport
      ;; adds the longest nonce a guest may have, encoded or not.
      (let* ((nonce (make-string niwa-collaboration--max-guest-nonce-chars ?n))
             (peers (make-hash-table :test #'eql))
             (full (list :t "record" :text
                         (make-string (- niwa-collaboration--max-frame-json-bytes
                                         (string-bytes (json-encode '(:t "record" :text ""))))
                                      ?x))))
        (puthash 1 (niwa-collaboration--make-binding :challenge "c" :last-seq 1 :nonce nonce) peers)
        (plist-put transport :peers peers)
        (should (= (string-bytes (json-encode full)) niwa-collaboration--max-frame-json-bytes))
        (should (niwa-collaboration--transport-send transport 1 full))
        (should (= 2 (length sent)))))))

(niwa-deftest niwa-collaboration--websocket-frame
  (:doc "frames every length class as a masked binary frame websocket.el reads back")
  (dolist (size '(0 125 126 65535 65536 300000))
    (let* ((payload (apply #'unibyte-string
                           (cl-loop for index below size collect (% (* index 7) 256))))
           (encoded (niwa-collaboration--websocket-frame payload))
           (frame (websocket-read-frame encoded)))
      (should-not (multibyte-string-p encoded))
      ;; The mask bit is set: a client must mask what it sends.
      (should (= #x80 (logand (aref encoded 1) #x80)))
      (should (eq 'binary (websocket-frame-opcode frame)))
      (should (websocket-frame-completep frame))
      (should (equal payload (websocket-frame-payload frame)))
      (should (= (length encoded) (websocket-frame-length frame))))))

(niwa-deftest niwa-collaboration--transport-control
  (:doc "sends bounded unencrypted relay controls only on an open transport")
  (let* ((transport (list :state 'open :ws 'ws))
         sent)
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t))
              ((symbol-function 'websocket-send-text)
               (lambda (_ws text) (push text sent))))
      (should (niwa-collaboration--transport-control
               transport (list :t "push")))
      (should (equal "push"
                     (plist-get (json-parse-string
                                 (car sent) :object-type 'plist)
                                :t)))
      (plist-put transport :state 'down)
      (should-not (niwa-collaboration--transport-control
                   transport (list :t "push")))
      (should (= 1 (length sent))))))

(niwa-deftest niwa-collaboration--transport-down
  (:doc "reconnects with backoff after the relay drops and stops cleanly")
  (niwa-relay-test--with-path-capture
   (let* ((state (list (list :next-peer 1)))
          (port (niwa-relay-test--free-port))
          (server (niwa-relay-test--stub-relay-start state port))
          (key (make-string 32 5))
          (states nil)
          (transport
           (niwa-collaboration--transport-open
            (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
            key
            :on-state (lambda (new) (push new states)))))
     (unwind-protect
         (progn
           (should (niwa-relay-test--pump
                    (lambda ()
                      (niwa-collaboration--transport-open-p transport))))
           ;; Relay goes away: the transport reports down and schedules a
           ;; retry instead of dying.
           (websocket-server-close server)
           (should (niwa-relay-test--pump (lambda () (memq 'down states))))
           (should (timerp (plist-get transport :reconnect-timer)))
           ;; The relay returns on the same port: the transport reconnects
           ;; by itself within the backoff window.
           (setq state (list (list :next-peer 1))
                 server (niwa-relay-test--stub-relay-start state port))
           (should (niwa-relay-test--pump
                    (lambda ()
                      (niwa-collaboration--transport-open-p transport))
                    10))
           (should (equal 'open (car states))))
       (niwa-collaboration--transport-stop transport)
       (ignore-errors (niwa-relay-test--stub-relay-stop server))
       ;; Stopping cancels any retry so no timer leaks.
       (should-not (plist-get transport :reconnect-timer))))))

(niwa-deftest niwa-collaboration--transport-keepalive
  ()
  ,test
  (test)
  :doc "a keepalive ping carries a payload so websocket.el masks it"
  (let ((transport (list :state 'open :ws 'ws :inbound-at (float-time)))
        sent)
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t))
              ((symbol-function 'websocket-send)
               (lambda (_ws frame) (setq sent frame))))
      (niwa-collaboration--transport-keepalive transport))
    (should (eq 'ping (websocket-frame-opcode sent)))
    (should (= 128
               (logand 128
                       (aref (websocket-encode-frame sent t) 1)))))

  :doc "a failing write redials and a stale close leaves the replacement open"
  (niwa-relay-test--with-stub-relay (state port server)
    (let* ((states nil)
           (transport
            (niwa-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             (make-string 32 5)
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (niwa-relay-test--pump
                     (lambda ()
                       (niwa-collaboration--transport-open-p transport))))
            (should (timerp (plist-get transport :keepalive-timer)))
            (let* ((old (plist-get transport :ws))
                   (stale-close (websocket-on-close old)))
              ;; A suspended machine wakes with this exact state: the process
              ;; still reads open, and only the write finds the reset.
              (cl-letf (((symbol-function 'websocket-send)
                         (lambda (&rest _) (signal 'error (list "reset")))))
                (niwa-collaboration--transport-keepalive transport))
              (should-not (websocket-openp old))
              (should (memq 'down states))
              (should (timerp (plist-get transport :reconnect-timer)))
              (should (niwa-relay-test--pump
                       (lambda ()
                         (niwa-collaboration--transport-open-p transport))))
              (let ((replacement (plist-get transport :ws)))
                (funcall stale-close old)
                (should (eq replacement (plist-get transport :ws)))
                (should (niwa-collaboration--transport-open-p transport)))))
        (niwa-collaboration--transport-stop transport)
        (should-not (plist-get transport :keepalive-timer)))))

  :doc "a connection silent past the liveness window is dropped and redialed"
  (niwa-relay-test--with-stub-relay (state port server)
    (let* ((states nil)
           (transport
            (niwa-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             (make-string 32 5)
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (niwa-relay-test--pump
                     (lambda ()
                       (niwa-collaboration--transport-open-p transport))))
            (let ((old (plist-get transport :ws))
                  (pinged nil))
              ;; A network change leaves exactly this: the relay's side has
              ;; gone quiet, and no write ever fails.
              (set-process-filter
               (websocket-conn (plist-get (car state) :host)) #'ignore)
              (plist-put transport :inbound-at
                         (- (float-time)
                            niwa-collaboration--liveness-seconds 1))
              (cl-letf (((symbol-function 'websocket-send)
                         (lambda (&rest _) (setq pinged t))))
                (niwa-collaboration--transport-keepalive transport))
              (should-not pinged)
              (should-not (websocket-openp old))
              (should (equal '(down open) states))
              (should (timerp (plist-get transport :reconnect-timer)))
              (should (niwa-relay-test--pump
                       (lambda ()
                         (niwa-collaboration--transport-open-p transport))))
              (should-not (eq old (plist-get transport :ws)))
              (should-not (niwa-collaboration--transport-silent-p
                           transport))))
        (niwa-collaboration--transport-stop transport))))

  :doc "input queued while Emacs was busy counts before silence is judged"
  (niwa-relay-test--with-stub-relay (state port server)
    (let* ((states nil)
           (transport
            (niwa-collaboration--transport-open
             (format "ws://127.0.0.1:%d/r/roomroomroomroom?role=host" port)
             (make-string 32 5)
             :on-state (lambda (new) (push new states)))))
      (unwind-protect
          (progn
            (should (niwa-relay-test--pump
                     (lambda ()
                       (niwa-collaboration--transport-open-p transport))))
            (let ((ws (plist-get transport :ws))
                  (pinged nil))
              (plist-put transport :inbound-at
                         (- (float-time)
                            niwa-collaboration--liveness-seconds 1))
              ;; The relay's traffic is waiting in the socket, unread.
              (websocket-send-text (plist-get (car state) :host)
                                   "{\"t\":\"peer-joined\",\"peer\":7}")
              (cl-letf* ((send (symbol-function 'websocket-send))
                         ((symbol-function 'websocket-send)
                          (lambda (target frame)
                            (when (eq target ws) (setq pinged t))
                            (funcall send target frame))))
                (niwa-collaboration--transport-keepalive transport))
              (should pinged)
              (should (eq ws (plist-get transport :ws)))
              (should (equal '(open) states))
              (should-not (niwa-collaboration--transport-silent-p
                           transport))))
        (niwa-collaboration--transport-stop transport)))))

(niwa-deftest niwa-collaboration--transport-silent-p
  (:doc "measures the time since the connection last received anything")
  (let ((transport (list :inbound-at (float-time))))
    (should-not (niwa-collaboration--transport-silent-p transport))
    (plist-put transport :inbound-at
               (- (float-time) niwa-collaboration--liveness-seconds 1))
    (should (niwa-collaboration--transport-silent-p transport))
    ;; A connection that never received anything is silent.
    (should (niwa-collaboration--transport-silent-p (list :ws 'ws)))))

(niwa-deftest niwa-collaboration--transport-watch-inbound
  (:doc "stamps inbound bytes on the current connection only")
  (let* ((process (make-pipe-process :name "niwa-test-inbound"
                                     :noquery t :filter #'ignore))
         (ws (websocket-inner-create :conn process :url "ws://fixture"
                                     :accept-string ""))
         (transport (list :ws ws :inbound-at nil)))
    (unwind-protect
        (progn
          (niwa-collaboration--transport-watch-inbound transport ws)
          (funcall (process-filter process) process "bytes")
          (should (numberp (plist-get transport :inbound-at)))
          ;; A replaced connection no longer speaks for the transport.
          (plist-put transport :inbound-at nil)
          (plist-put transport :ws 'replacement)
          (funcall (process-filter process) process "bytes")
          (should-not (plist-get transport :inbound-at)))
      (delete-process process))))


;;; test-niwa-collaboration-transport.el ends here
