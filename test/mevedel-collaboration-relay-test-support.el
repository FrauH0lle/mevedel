;;; mevedel-collaboration-relay-test-support.el --- In-process stub relay for tests -*- lexical-binding: t; -*-

;;; Commentary:

;; An in-process Elisp stub relay implementing the relay wire contract the
;; Go binary in relay/ speaks: one host and many guests per room, binary
;; envelopes routed by peer id, and the `peer-joined', `peer-left' and
;; `room-closed' controls.  Transport tests, and room tests that drive a
;; host the way a guest would, share it; the Go relay's own behaviour is
;; covered by its `go test' suite.

;;; Code:

(require 'cl-lib)
(require 'websocket)

;; websocket.el's server discards the HTTP request path, but the relay
;; contract routes on it.  The stub captures the request line per client
;; process before delegating to the real server filter.

(defun mevedel-relay-test--relay-path (process)
  "Return the captured request path for client PROCESS."
  (process-get process :mevedel-test-path))

(defmacro mevedel-relay-test--with-path-capture (&rest body)
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

(defun mevedel-relay-test--stub-relay-start (state port)
  "Start a stub relay on PORT recording rooms in STATE.
STATE is a plist placed in a cons cell so handlers can mutate it."
  (websocket-server
   port
   :host 'local
   :on-open
   (lambda (ws)
     (let ((path (mevedel-relay-test--relay-path (websocket-conn ws))))
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
                                  (mevedel-relay-test--relay-send-binary
                                   guest payload))
                                guests))
                   (when-let* ((guest (and guests (gethash peer guests))))
                     (mevedel-relay-test--relay-send-binary guest payload))))
             (when host
               (let* ((peer (or (cdr (assq ws (plist-get plist :peer-of))) 0))
                      (rewritten (concat (unibyte-string
                                          (logand (ash peer -24) #xff)
                                          (logand (ash peer -16) #xff)
                                          (logand (ash peer -8) #xff)
                                          (logand peer #xff))
                                         (substring payload 4))))
                 (mevedel-relay-test--relay-send-binary host rewritten))))))))
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

(defun mevedel-relay-test--relay-send-binary (ws payload)
  "Send unibyte PAYLOAD as one binary frame on WS."
  (ignore-errors
    (websocket-send ws (make-websocket-frame :opcode 'binary
                                             :payload payload
                                             :completep t))))

(defun mevedel-relay-test--pump (predicate &optional timeout)
  "Run the event loop until PREDICATE returns non-nil or TIMEOUT expires.
Return the predicate's final value."
  (let ((deadline (+ (float-time) (or timeout 5)))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    result))

(defun mevedel-relay-test--free-port ()
  "Return a free loopback TCP port."
  (let* ((server (make-network-process :name "mevedel-test-port-probe"
                                       :server t :host 'local :service t
                                       :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun mevedel-relay-test--stub-relay-stop (server)
  "Release SERVER and its accepted sockets without sending close frames."
  (dolist (ws (copy-sequence websocket-server-websockets))
    (when (eq server (websocket-server-conn ws))
      (let ((client (websocket-conn ws)))
        (set-process-filter client #'ignore)
        (set-process-sentinel client #'ignore)
        (delete-process client))
      (setq websocket-server-websockets
            (delq ws websocket-server-websockets))))
  (delete-process server))

(defmacro mevedel-relay-test--with-stub-relay (bindings &rest body)
  "Run BODY with a live stub relay bound per BINDINGS (STATE PORT SERVER).
STATE is bound to the one-element mutable list whose car holds the
relay's room plist."
  (declare (indent 1))
  (pcase-let ((`(,state ,port ,server) bindings))
    `(mevedel-relay-test--with-path-capture
      (let* ((,state (list (list :next-peer 1)))
             (,port (mevedel-relay-test--free-port))
             (,server (mevedel-relay-test--stub-relay-start ,state ,port)))
        (ignore ,state)
        (unwind-protect
            (progn ,@body)
          (mevedel-relay-test--stub-relay-stop ,server))))))

(provide 'mevedel-collaboration-relay-test-support)
;;; mevedel-collaboration-relay-test-support.el ends here
