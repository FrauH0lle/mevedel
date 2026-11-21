;;; niwa-collaboration-relay-test-support.el --- In-process stub relay for tests -*- lexical-binding: t; -*-

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

(defun niwa-relay-test--relay-path (process)
  "Return the captured request path for client PROCESS."
  (process-get process :niwa-test-path))

(defmacro niwa-relay-test--with-path-capture (&rest body)
  "Run BODY with a non-reentrant server filter capturing request paths.
Socket backpressure can run a filter inside `process-send-string'.  Queue
that raw input until the original invocation finishes all its callbacks,
preserving frame order and avoiding recursive forwarding of large transfers."
  `(cl-letf* ((original (symbol-function 'websocket-server-filter))
              ((symbol-function 'websocket-server-filter)
               (lambda (process output)
                 (process-put process :niwa-test-input
                              (nconc (process-get process :niwa-test-input)
                                     (list output)))
                 (unless (process-get process :niwa-test-filter-active)
                   (process-put process :niwa-test-filter-active t)
                   (unwind-protect
                       (while (process-get process :niwa-test-input)
                         (let* ((pending (process-get process :niwa-test-input))
                                (chunk (car pending)))
                           (process-put process :niwa-test-input (cdr pending))
                           (unless (process-get process :niwa-test-path)
                             (when (string-match "\\`[A-Z]+ \\([^ ]+\\)" chunk)
                               (process-put process :niwa-test-path
                                            (match-string 1 chunk))))
                           (funcall original process chunk)))
                     (process-put process :niwa-test-filter-active nil))))))
     ,@body))

(defun niwa-relay-test--stub-relay-start (state port)
  "Start a stub relay on PORT recording rooms in STATE.
STATE is a plist placed in a cons cell so handlers can mutate it."
  (websocket-server
   port
   :host 'local
   :on-open
   (lambda (ws)
     (let ((path (niwa-relay-test--relay-path (websocket-conn ws))))
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
                                  (niwa-relay-test--relay-send-binary
                                   guest payload))
                                guests))
                   (when-let* ((guest (and guests (gethash peer guests))))
                     (niwa-relay-test--relay-send-binary guest payload))))
             (when host
               (let* ((peer (or (cdr (assq ws (plist-get plist :peer-of))) 0))
                      (rewritten (concat (unibyte-string
                                          (logand (ash peer -24) #xff)
                                          (logand (ash peer -16) #xff)
                                          (logand (ash peer -8) #xff)
                                          (logand peer #xff))
                                         (substring payload 4))))
                 (niwa-relay-test--relay-send-binary host rewritten))))))))
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

(defun niwa-relay-test--relay-send-binary (ws payload)
  "Send unibyte PAYLOAD as one binary frame on WS."
  (ignore-errors
    (websocket-send ws (make-websocket-frame :opcode 'binary
                                             :payload payload
                                             :completep t))))

(defun niwa-relay-test--pump (predicate &optional timeout)
  "Run the event loop until PREDICATE returns non-nil or TIMEOUT expires.
Return the predicate's final value."
  (let ((deadline (+ (float-time) (or timeout 5)))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    result))

(defun niwa-relay-test--free-port ()
  "Return a free loopback TCP port."
  (let* ((server (make-network-process :name "niwa-test-port-probe"
                                       :server t :host 'local :service t
                                       :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun niwa-relay-test--stub-relay-stop (server)
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

(defmacro niwa-relay-test--with-stub-relay (bindings &rest body)
  "Run BODY with a live stub relay bound per BINDINGS (STATE PORT SERVER).
STATE is bound to the one-element mutable list whose car holds the
relay's room plist."
  (declare (indent 1))
  (pcase-let ((`(,state ,port ,server) bindings))
    `(niwa-relay-test--with-path-capture
      (let* ((,state (list (list :next-peer 1)))
             (,port (niwa-relay-test--free-port))
             (,server (niwa-relay-test--stub-relay-start ,state ,port)))
        (ignore ,state)
        (unwind-protect
            (progn ,@body)
          (niwa-relay-test--stub-relay-stop ,server))))))

(provide 'niwa-collaboration-relay-test-support)
;;; niwa-collaboration-relay-test-support.el ends here
