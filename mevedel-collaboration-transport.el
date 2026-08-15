;;; mevedel-collaboration-transport.el --- collaboration HTTP/WebSocket transport -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the bounded loopback HTTP/WebSocket protocol, authenticated viewer
;; admission, ACK-window output pump, and packaged viewer routes.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'eieio))

;; `json'
(declare-function json-parse-string "json" (string &rest args))

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--random-token
                  "mevedel-collaboration" ())
(defvar mevedel-collaboration--room)

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--json-record
                  "mevedel-collaboration-projection" (record))
(declare-function mevedel-collaboration--json-string
                  "mevedel-collaboration-projection" (object))
(declare-function mevedel-collaboration--project-records
                  "mevedel-collaboration-projection" (room))
(declare-function mevedel-collaboration--truncate-bytes
                  "mevedel-collaboration-projection" (string limit))
(defvar mevedel-collaboration--max-message-bytes)
(defvar mevedel-collaboration--protocol-version)

;; `web-server'
(declare-function eieio-oref "eieio" (object slot))
(declare-function eieio-oset "eieio" (object slot value))
(declare-function ws-response-header "web-server" (process code &rest headers))
(declare-function ws-start "web-server" (handlers port &optional log-buffer &rest network-args))
(declare-function ws-stop "web-server" (server))
(declare-function ws-web-socket-connect "web-server" (request handler))
(declare-function ws-web-socket-filter "web-server" (process string))
(declare-function ws-web-socket-frame "web-server" (string &optional opcode))
(declare-function ws-filter "web-server" (process string))
(declare-function requests "web-server" (server))

(defconst mevedel-collaboration--max-pending-bytes (* 2 1024 1024))
(defconst mevedel-collaboration--sequence-overhead-bytes 96
  "Bytes reserved for a sequence number and 43-character ACK token.")
(defconst mevedel-collaboration--auth-timeout 5)
(defconst mevedel-collaboration--max-pending-auth 1)
(defconst mevedel-collaboration--frame-idle-timeout 5)
(defconst mevedel-collaboration--send-interval 0.05)
(defconst mevedel-collaboration--max-pending-age 5.0)
(defconst mevedel-collaboration--max-preupgrade-bytes (* 64 1024))
(defconst mevedel-collaboration--preupgrade-idle-timeout 2)
(defconst mevedel-collaboration--preupgrade-total-timeout 10)
(defconst mevedel-collaboration--max-preupgrade-children 8
  "Incomplete HTTP connections one room accepts at once.
The accepted slice serves a single viewer, so a larger set can only come
from a client holding connections open.")

(defvar mevedel-collaboration--preupgrade-state
  (make-hash-table :test #'eq :weakness 'key))

(defun mevedel-collaboration--preupgrade-cancel (process slot)
  "Cancel pre-upgrade timer SLOT on PROCESS."
  (when-let ((timer (process-get process slot)))
    (cancel-timer timer)
    (process-put process slot nil)))

(defun mevedel-collaboration--forget-request (process &optional server)
  "Drop PROCESS's `web-server' request record from SERVER.

SERVER defaults to the live room's server.  A retained record keeps a dead
process object and its buffered bytes alive for the room's whole lifetime and
is rescanned on every later chunk."
  (require 'eieio)
  (when-let ((server (or server (plist-get mevedel-collaboration--room :server))))
    (let (kept)
      (dolist (request (mevedel-collaboration--web-server-slot
                        server 'requests))
        (unless (eq process
                    (mevedel-collaboration--web-server-slot
                     request 'process))
          (push request kept)))
      (eieio-oset server 'requests (nreverse kept)))))

(defun mevedel-collaboration--preupgrade-remove (process &optional preserve-request)
  "Remove pre-upgrade PROCESS from tracked children and web-server requests."
  (require 'cl-lib)
  (let* ((state (gethash process mevedel-collaboration--preupgrade-state))
         (owner (or (plist-get state :owner)
                    (process-get process :mevedel-collaboration-preupgrade-owner))))
    (dolist (timer (list (plist-get state :idle)
                         (plist-get state :total)))
      (when (timerp timer)
        (cancel-timer timer)))
    (dolist (slot '(:mevedel-collaboration-preupgrade-idle
                    :mevedel-collaboration-preupgrade-total))
      (mevedel-collaboration--preupgrade-cancel process slot))
    (when owner
      (process-put owner :mevedel-collaboration-preupgrade-children
                   (delq process
                         (process-get owner
                                      :mevedel-collaboration-preupgrade-children)))
      (unless preserve-request
        (mevedel-collaboration--forget-request
         process (plist-get (process-plist owner) :server)))))
  (remhash process mevedel-collaboration--preupgrade-state)
  (process-put process :mevedel-collaboration-preupgrade-owner nil)
  (process-put process :mevedel-collaboration-preupgrade-bytes nil))

(defun mevedel-collaboration--preupgrade-drop (process)
  "Cancel tracking and close incomplete HTTP PROCESS."
  (mevedel-collaboration--preupgrade-remove process)
  (when (process-live-p process)
    (delete-process process)))

(defun mevedel-collaboration--preupgrade-timeout (process slot)
  "Close incomplete HTTP PROCESS when timer SLOT expires."
  (ignore slot)
  (when (process-live-p process)
    (mevedel-collaboration--preupgrade-drop process)))

(defun mevedel-collaboration--preupgrade-sentinel (process _event)
  "Clean pre-upgrade state when PROCESS disconnects."
  (unless (process-live-p process)
    (mevedel-collaboration--preupgrade-remove process)))

(defun mevedel-collaboration--preupgrade-filter (process string)
  "Count pre-upgrade bytes, then delegate unchanged to `ws-filter'."
  (let* ((state (or (gethash process mevedel-collaboration--preupgrade-state)
                    (list :owner (process-get process
                                              :mevedel-collaboration-preupgrade-owner))))
         (bytes (+ (or (plist-get state :bytes) 0)
                   (string-bytes string))))
    (if (> bytes mevedel-collaboration--max-preupgrade-bytes)
        (mevedel-collaboration--preupgrade-drop process)
      (setf (plist-get state :bytes) bytes)
      (when-let ((timer (plist-get state :idle)))
        (cancel-timer timer))
      (setf (plist-get state :idle)
            (run-at-time mevedel-collaboration--preupgrade-idle-timeout nil
                         #'mevedel-collaboration--preupgrade-timeout process
                         :mevedel-collaboration-preupgrade-idle))
      (puthash process state mevedel-collaboration--preupgrade-state)
      (process-put process :mevedel-collaboration-preupgrade-bytes bytes)
      (process-put process :mevedel-collaboration-preupgrade-idle
                   (plist-get state :idle))
      (funcall #'ws-filter process string)
      ;; Only a real upgrade ends the bounds.  End of headers does not: a
      ;; multipart request keeps parsing past its terminator and holds the
      ;; connection open, so releasing the deadlines there would leave an
      ;; untracked socket that outlives the room.  `ws-web-socket-connect'
      ;; replaces the process plist with its own message object.
      (when-let ((message (plist-get (process-plist process) :message)))
        (mevedel-collaboration--preupgrade-remove process message)))))

(defun mevedel-collaboration--preupgrade-accept (listener process _message)
  "Track PROCESS accepted by LISTENER before `web-server' reads its bytes."
  (when (process-live-p process)
    (if (>= (length (process-get listener
                                 :mevedel-collaboration-preupgrade-children))
            mevedel-collaboration--max-preupgrade-children)
        (delete-process process)
      (let ((state (list :owner listener :bytes 0)))
        (process-put process :mevedel-collaboration-preupgrade-owner listener)
        (process-put process :mevedel-collaboration-preupgrade-bytes 0)
        (process-put listener :mevedel-collaboration-preupgrade-children
                     (cons process
                           (process-get listener
                                        :mevedel-collaboration-preupgrade-children)))
        (set-process-filter process #'mevedel-collaboration--preupgrade-filter)
        (set-process-sentinel process #'mevedel-collaboration--preupgrade-sentinel)
        (setf (plist-get state :idle)
              (run-at-time mevedel-collaboration--preupgrade-idle-timeout nil
                           #'mevedel-collaboration--preupgrade-timeout process
                           :mevedel-collaboration-preupgrade-idle)
              (plist-get state :total)
              (run-at-time mevedel-collaboration--preupgrade-total-timeout nil
                           #'mevedel-collaboration--preupgrade-timeout process
                           :mevedel-collaboration-preupgrade-total))
        (puthash process state mevedel-collaboration--preupgrade-state)
        (process-put process :mevedel-collaboration-preupgrade-idle
                     (plist-get state :idle))
        (process-put process :mevedel-collaboration-preupgrade-total
                     (plist-get state :total))))))

(defun mevedel-collaboration--ws-start (handler port)
  "Start web-server HANDLER with a scoped native accept callback."
  (require 'cl-lib)
  (let ((original (symbol-function 'make-network-process)))
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest args)
                 (let (clean)
                   (while args
                     (let ((key (pop args)) (value (pop args)))
                       (unless (eq key :log)
                         (setq clean (append clean (list key value))))))
                   (apply original
                          (append clean
                                  (list :log
                                        #'mevedel-collaboration--preupgrade-accept)))))))
      (ws-start handler port nil :host "127.0.0.1"))))

(defun mevedel-collaboration--preupgrade-stop (server)
  "Close and forget all incomplete children accepted by SERVER."
  (dolist (process (process-get
                    (mevedel-collaboration--web-server-slot server 'process)
                    :mevedel-collaboration-preupgrade-children))
    (mevedel-collaboration--preupgrade-drop process)))


(defvar mevedel-collaboration--asset-directory
  (file-name-concat
   (file-name-directory
    (or load-file-name
        (locate-library "mevedel-collaboration-transport")
        default-directory))
   "collaboration"))

(defun mevedel-collaboration--asset-path (name)
  "Return the bundled asset path for safe literal NAME, or nil."
  (when (member name '("index.html" "viewer.css" "viewer.js"))
    (file-name-concat mevedel-collaboration--asset-directory name)))

(defun mevedel-collaboration--read-asset (name)
  "Return bundled asset NAME as an unibyte string."
  (when-let ((path (mevedel-collaboration--asset-path name)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (buffer-string))))

(defun mevedel-collaboration--web-server-slot (object slot)
  "Read EIEIO SLOT from an installed GNU ELPA web-server OBJECT.

Slot accessors are function aliases in some releases of the dependency, so
this reads the slot itself."
  (eieio-oref object slot))

;;
;;; Transport output and protocol

(defun mevedel-collaboration--guest-live-p (guest)
  "Return non-nil when GUEST has a live process."
  (and guest (process-live-p (plist-get guest :process))))

(defun mevedel-collaboration--room-pending-auth (room)
  "Return ROOM's live unauthenticated guests in admission order."
  (let (pending)
    (dolist (guest (plist-get room :pending-auth))
      (when (mevedel-collaboration--guest-live-p guest)
        (push guest pending)))
    (nreverse pending)))

(defun mevedel-collaboration--close-code (reason)
  "Return a WebSocket close code for REASON."
  (cond
   ((memq reason '(auth-rejected auth-timeout inbound-after-auth)) 1008)
   ;; A slow reader may reconnect for a fresh snapshot; this is temporary
   ;; resource pressure, not a terminal link or authentication rejection.
   ((eq reason 'guest-too-slow) 1013)
   ((memq reason '(message-too-large incoming-too-large)) 1009)
   ((memq reason '(invalid too-large malformed-frame fragmented-message
                           unexpected-continuation)) 1002)
   (t 1000)))

(defun mevedel-collaboration--send-close-frame (process reason)
  "Send a minimal unmasked WebSocket close frame for REASON to PROCESS."
  (let ((code (mevedel-collaboration--close-code reason)))
    (process-send-string
     process (unibyte-string #x88 2 (ash code -8) (logand code #xff)))))

(defun mevedel-collaboration--guest-cancel-timers (guest)
  "Cancel all timers owned by GUEST and clear their slots."
  (dolist (slot '(:auth-timer :pump-timer :snapshot-timer
                              :receive-timer :close-timer))
    (when-let ((timer (plist-get guest slot)))
      (cancel-timer timer)
      (setf (plist-get guest slot) nil)))
  guest)

(defun mevedel-collaboration--guest-close (guest &optional reason)
  "Close GUEST and cancel its timers.
REASON is only retained for debugging and is never sent to the browser."
  (when guest
    (mevedel-collaboration--guest-cancel-timers guest)
    (let ((process (plist-get guest :process)))
      (when (process-live-p process)
        (condition-case nil
            (mevedel-collaboration--send-close-frame process reason)
          (error nil))
        (set-process-sentinel process nil)
        ;; Let the network process flush the close frame before dropping the
        ;; socket.  The guest is detached from the room immediately below.
        (setf (plist-get guest :close-timer)
              (run-at-time
               0.05 nil
               (lambda (socket owner)
                 (setf (plist-get owner :close-timer) nil)
                 (when (process-live-p socket)
                   (delete-process socket))
                 ;; The sentinel is detached for a host-initiated close, so
                 ;; drop the retained request record here instead.
                 (mevedel-collaboration--forget-request socket))
               process guest))))
    (let ((room mevedel-collaboration--room))
      (when room
        (when (eq (plist-get room :guest) guest)
          (setq room (plist-put room :guest nil)))
        (when (memq guest (plist-get room :pending-auth))
          (setq room
                (plist-put room :pending-auth
                           (delq guest (plist-get room :pending-auth)))))
        (setq mevedel-collaboration--room room)))
    nil))

(defun mevedel-collaboration--receive-timeout (process)
  "Close PROCESS when an incomplete frame makes no progress in time."
  (when-let ((guest (process-get process 'mevedel-collaboration-guest)))
    (setf (plist-get guest :receive-timer) nil)
    (mevedel-collaboration--guest-close guest 'incomplete-frame-timeout)))

(defun mevedel-collaboration--guest-arm-receive-timer (guest)
  "Arm GUEST's idle timer while its next frame is incomplete."
  (when (mevedel-collaboration--guest-live-p guest)
    (when-let ((timer (plist-get guest :receive-timer)))
      (cancel-timer timer))
    (let ((timer (run-at-time mevedel-collaboration--frame-idle-timeout nil
                              #'mevedel-collaboration--receive-timeout
                              (plist-get guest :process))))
      (setf (plist-get guest :receive-timer) timer)
      (process-put (plist-get guest :process)
                   'mevedel-collaboration-guest guest))))

(defun mevedel-collaboration--guest-clear-receive-timer (guest)
  "Cancel GUEST's incomplete-frame idle timer."
  (when-let ((timer (plist-get guest :receive-timer)))
    (cancel-timer timer)
    (setf (plist-get guest :receive-timer) nil))
  guest)

(defun mevedel-collaboration--guest-next-sequence (guest)
  "Reserve and return the next output sequence for GUEST."
  (let ((sequence (1+ (or (plist-get guest :next-sequence) 0))))
    (setf (plist-get guest :next-sequence) sequence)
    sequence))

(defun mevedel-collaboration--guest-json-with-sequence (guest object)
  "Encode OBJECT with a peer acknowledgement sequence for GUEST."
  (let* ((sequence (mevedel-collaboration--guest-next-sequence guest))
         (ack-token (mevedel-collaboration--random-token))
         (json (mevedel-collaboration--json-string
                (append object (list (cons "seq" sequence)
                                     (cons "ack-token" ack-token))))))
    (list :sequence sequence :ack-token ack-token :json json)))

(defun mevedel-collaboration--guest-pump (guest)
  "Send at most one queued frame for GUEST without blocking the host.

The `web-process' API exposes no kernel write-queue or drain callback for a
network process.  Every application frame therefore stays in flight until
the browser acknowledges its sequence.  The pending byte count includes that
frame and all queued frames, so a non-reading peer is closed at the bounded
application limit instead of being treated as drained by elapsed time."
  (when (mevedel-collaboration--guest-live-p guest)
    (setq guest (plist-put guest :pump-timer nil))
    (when-let ((in-flight (plist-get guest :in-flight)))
      (if (and (equal (plist-get guest :acknowledged-sequence)
                      (plist-get in-flight :sequence))
               (equal (plist-get guest :acknowledged-token)
                      (plist-get in-flight :ack-token)))
          (progn
            (setq guest (plist-put guest :in-flight nil))
            (setq guest
                  (plist-put guest :acknowledged-sequence nil))
            (setq guest
                  (plist-put guest :acknowledged-token nil))
            (setq guest
                  (plist-put guest :pending-bytes
                             (- (plist-get guest :pending-bytes)
                                (plist-get in-flight :bytes)))))
        (if (> (- (float-time) (plist-get in-flight :enqueued-at))
               mevedel-collaboration--max-pending-age)
            (mevedel-collaboration--guest-close guest 'guest-too-slow)
          (setf (plist-get guest :pump-timer)
                (run-at-time mevedel-collaboration--send-interval nil
                             #'mevedel-collaboration--guest-pump guest))))))
  (when (and (mevedel-collaboration--guest-live-p guest)
             (null (plist-get guest :in-flight)))
    (let ((item (car (plist-get guest :queue))))
      (cond
       ((and item
             (> (- (float-time) (plist-get item :enqueued-at))
                mevedel-collaboration--max-pending-age))
        (mevedel-collaboration--guest-close guest 'guest-too-slow))
       (item
        (setq guest (plist-put guest :queue
                               (cdr (plist-get guest :queue))))
        (setq guest (plist-put guest :in-flight item))
        (let ((process (plist-get guest :process)))
          (process-put process 'mevedel-collaboration-guest guest)
          (condition-case nil
              (progn
                (process-send-string process (plist-get item :frame))
                (setf (plist-get guest :pump-timer)
                      (run-at-time mevedel-collaboration--send-interval nil
                                   #'mevedel-collaboration--guest-pump guest)))
            (error (mevedel-collaboration--guest-close guest)))
          (when (process-live-p process)
            (process-put process 'mevedel-collaboration-guest guest))))
       ((plist-get guest :snapshot-queue)
        (setf (plist-get guest :snapshot-timer)
              (run-at-time 0 nil #'mevedel-collaboration--snapshot-pump
                           guest)))))))

(defun mevedel-collaboration--record-coalesce-key (object)
  "Return the unsent-update key for record OBJECT, or nil."
  (when (equal (cdr (assoc "type" object)) "record")
    (let* ((record (cdr (assoc "record" object)))
           (id (cdr (assoc "id" record))))
      (and (stringp id)
           (concat "record\0" id)))))

(defun mevedel-collaboration--guest-enqueue
    (guest json &optional coalesce-key sequence ack-token)
  "Queue JSON text for GUEST, enforcing frame and pending bounds.

When COALESCE-KEY is non-nil, replace an older unsent item with the same
key in place.  An in-flight frame is never replaced, so the guest still sees
the ordered transition from the frame already handed to the process to the
newest queued revision.  SEQUENCE and ACK-TOKEN identify the frame that the
browser must acknowledge before another frame is sent."
  (let* ((wire (encode-coding-string json 'utf-8 t))
         (bytes (string-bytes wire))
         (frame (and (<= bytes mevedel-collaboration--max-message-bytes)
                     (ws-web-socket-frame wire)))
         (frame-bytes (and frame (string-bytes frame)))
         (queue (plist-get guest :queue))
         existing)
    (dolist (item queue)
      (when (and (null existing)
                 coalesce-key
                 (equal coalesce-key (plist-get item :coalesce-key)))
        (setq existing item)))
    (let* ((pending-bytes (or (plist-get guest :pending-bytes) 0))
           (new-pending-bytes
            (and frame-bytes
                 (+ pending-bytes
                    (if existing
                        (- frame-bytes (plist-get existing :bytes))
                      frame-bytes)))))
      (cond
       ((or (> bytes mevedel-collaboration--max-message-bytes)
            (null frame))
        (mevedel-collaboration--guest-close guest 'message-too-large)
        nil)
       ((> new-pending-bytes mevedel-collaboration--max-pending-bytes)
        (mevedel-collaboration--guest-close guest 'guest-too-slow)
        nil)
       ((not (mevedel-collaboration--guest-live-p guest)) nil)
       (t
        (if existing
            (setf (plist-get existing :frame) frame
                  (plist-get existing :bytes) frame-bytes
                  (plist-get existing :sequence) sequence
                  (plist-get existing :ack-token) ack-token
                  (plist-get existing :enqueued-at) (float-time))
          (setq guest
                (plist-put guest :queue
                           (append queue
                                   (list (list :frame frame
                                               :bytes frame-bytes
                                               :sequence sequence
                                               :ack-token ack-token
                                               :coalesce-key coalesce-key
                                               :enqueued-at (float-time)))))))
        (setq guest
              (plist-put guest :pending-bytes
                         new-pending-bytes))
        (process-put (plist-get guest :process)
                     'mevedel-collaboration-guest guest)
        (unless (or (plist-get guest :pump-timer)
                    (plist-get guest :in-flight))
          (setf (plist-get guest :pump-timer)
                (run-at-time 0 nil #'mevedel-collaboration--guest-pump guest)))
        t)))))

(defun mevedel-collaboration--guest-send (guest object)
  "JSON-encode OBJECT and queue it for GUEST after any snapshot."
  (let* ((sequenced (mevedel-collaboration--guest-json-with-sequence
                     guest object))
         (sequence (plist-get sequenced :sequence))
         (ack-token (plist-get sequenced :ack-token))
         (json (plist-get sequenced :json))
         (coalesce-key
          (mevedel-collaboration--record-coalesce-key object))
         (wire (encode-coding-string json 'utf-8 t))
         (bytes (string-bytes wire))
         (frame (and (<= bytes mevedel-collaboration--max-message-bytes)
                     (condition-case nil
                         (ws-web-socket-frame wire)
                       (error nil))))
         (frame-bytes (and frame (string-bytes frame))))
    (cond
     ((> bytes mevedel-collaboration--max-message-bytes)
      (mevedel-collaboration--guest-close guest 'message-too-large)
      nil)
     ((plist-get guest :snapshot-active)
      (let* ((messages (plist-get guest :after-snapshot))
             existing)
        (dolist (item messages)
          (when (and (null existing)
                     coalesce-key
                     (equal coalesce-key (plist-get item :coalesce-key)))
            (setq existing item)))
        (let* ((after-bytes (or (plist-get guest :after-snapshot-bytes) 0))
               (new-after-bytes
                (and frame-bytes
                     (+ after-bytes
                        (if existing
                            (- frame-bytes (plist-get existing :bytes))
                          frame-bytes)))))
          (if (or (null frame)
                  (null new-after-bytes)
                  (> (+ (or (plist-get guest :pending-bytes) 0)
                        new-after-bytes)
                     mevedel-collaboration--max-pending-bytes))
              (progn
                (mevedel-collaboration--guest-close guest 'guest-too-slow)
                nil)
            (if existing
                (setf (plist-get existing :json) json
                      (plist-get existing :bytes) frame-bytes
                      (plist-get existing :sequence) sequence
                      (plist-get existing :ack-token) ack-token)
              (setf (plist-get guest :after-snapshot)
                    (append messages
                            (list (list :json json
                                        :bytes frame-bytes
                                        :sequence sequence
                                        :ack-token ack-token
                                        :coalesce-key coalesce-key)))))
            (setf (plist-get guest :after-snapshot-bytes) new-after-bytes)
            t))))
     (t (mevedel-collaboration--guest-enqueue
         guest json coalesce-key sequence ack-token)))))

(defun mevedel-collaboration--guest-send-immediate (guest object)
  "Send one small final OBJECT directly to GUEST before closing it."
  (when (mevedel-collaboration--guest-live-p guest)
    (let* ((wire (encode-coding-string
                  (mevedel-collaboration--json-string object) 'utf-8 t))
           (frame (ws-web-socket-frame wire)))
      (when (<= (string-bytes wire)
                mevedel-collaboration--max-message-bytes)
        (condition-case nil
            (progn
              (process-send-string (plist-get guest :process) frame)
              ;; A small WebSocket frame is handed to Emacs' network process
              ;; synchronously.  Teardown closes the process immediately after
              ;; this call, so no host callback is held waiting for a drain.
              nil)
          (error nil))))))

(defun mevedel-collaboration--flush-after-snapshot (guest)
  "Release deferred live JSON messages after GUEST's snapshot queue."
  (let ((messages (plist-get guest :after-snapshot)))
    (setf (plist-get guest :after-snapshot) nil
          (plist-get guest :after-snapshot-bytes) 0
          (plist-get guest :snapshot-active) nil)
    (dolist (message messages)
      (unless (mevedel-collaboration--guest-enqueue
               guest (plist-get message :json)
               (plist-get message :coalesce-key)
               (plist-get message :sequence)
               (plist-get message :ack-token))
        (setq messages nil)))))

(defun mevedel-collaboration--snapshot-pump (guest)
  "Queue one staged snapshot message for GUEST after prior output drains."
  (when (mevedel-collaboration--guest-live-p guest)
    (setf (plist-get guest :snapshot-timer) nil)
    (when-let ((message (car (plist-get guest :snapshot-queue))))
      (setf (plist-get guest :snapshot-queue)
            (cdr (plist-get guest :snapshot-queue)))
      (let* ((sequenced (mevedel-collaboration--guest-json-with-sequence
                         guest message))
             (sequence (plist-get sequenced :sequence))
             (ack-token (plist-get sequenced :ack-token)))
        (unless (mevedel-collaboration--guest-enqueue
                 guest (plist-get sequenced :json) nil sequence ack-token)
          (setf (plist-get guest :snapshot-queue) nil))
        (when (null (plist-get guest :snapshot-queue))
          (mevedel-collaboration--flush-after-snapshot guest))))))

(defun mevedel-collaboration--snapshot-chunks (records)
  "Return bounded JSON snapshot messages for RECORDS."
  (let ((snapshot-id (format "s-%s" (substring (secure-hash 'sha1
                                                            (format "%s" records))
                                               0 16)))
        chunks
        current)
    (dolist (record records)
      (let* ((single `(("type" . "snapshot-chunk")
                       ("version" . ,mevedel-collaboration--protocol-version)
                       ("snapshot" . ,snapshot-id)
                       ("records" . ,(list
                                      (mevedel-collaboration--json-record
                                       record)))))
             (single-bytes
              (string-bytes (mevedel-collaboration--json-string single)))
             (candidate (append current (list record)))
             (message `(("type" . "snapshot-chunk")
                        ("version" . ,mevedel-collaboration--protocol-version)
                        ("snapshot" . ,snapshot-id)
                        ("records" . ,(mapcar #'mevedel-collaboration--json-record
                                              candidate))))
             (bytes (string-bytes (mevedel-collaboration--json-string message))))
        (when (> (+ single-bytes
                    mevedel-collaboration--sequence-overhead-bytes)
                 mevedel-collaboration--max-message-bytes)
          (user-error "A collaboration snapshot record exceeds 1 MiB"))
        (if (and current
                 (> (+ bytes mevedel-collaboration--sequence-overhead-bytes)
                    mevedel-collaboration--max-message-bytes))
            (progn
              (push current chunks)
              (setq current (list record)))
          (setq current candidate))))
    (when current (push current chunks))
    (setq chunks (nreverse chunks))
    (cons snapshot-id
          (mapcar
           (lambda (chunk)
             `(("type" . "snapshot-chunk")
               ("version" . ,mevedel-collaboration--protocol-version)
               ("snapshot" . ,snapshot-id)
               ("records" . ,(mapcar #'mevedel-collaboration--json-record
                                     chunk))))
           chunks))))

(defun mevedel-collaboration--send-snapshot (guest room)
  "Send ROOM's current bounded snapshot to authenticated GUEST."
  (let* ((records (or (plist-get room :records)
                      (mevedel-collaboration--project-records room)))
         (snapshot (mevedel-collaboration--snapshot-chunks records))
         (snapshot-id (car snapshot))
         (messages (append
                    (list `(("type" . "snapshot-begin")
                            ("version" . ,mevedel-collaboration--protocol-version)
                            ("snapshot" . ,snapshot-id)
                            ("status" . "running")))
                    (cdr snapshot)
                    (list `(("type" . "snapshot-end")
                            ("version" . ,mevedel-collaboration--protocol-version)
                            ("snapshot" . ,snapshot-id))))))
    (setf (plist-get guest :snapshot-active) t
          (plist-get guest :snapshot-queue) messages
          (plist-get guest :after-snapshot) nil
          (plist-get guest :after-snapshot-bytes) 0)
    (unless (plist-get guest :snapshot-timer)
      (setf (plist-get guest :snapshot-timer)
            (run-at-time 0 nil #'mevedel-collaboration--snapshot-pump guest)))))
;;; WebSocket input and HTTP routes

(defun mevedel-collaboration--request-header (request name)
  "Return case-insensitive header NAME from REQUEST."
  (cdr (assq (intern (concat ":" (upcase name)))
             (mevedel-collaboration--web-server-slot request 'headers))))

(defun mevedel-collaboration--allowed-origin-p (room origin)
  "Return non-nil when ORIGIN is one of ROOM's exact allowlisted origins."
  (and (stringp origin)
       (member origin (plist-get room :origins))))

(defun mevedel-collaboration--valid-websocket-key-p (key)
  "Return non-nil when KEY is the canonical base64 form of 16 bytes."
  (condition-case nil
      (and (stringp key)
           (string-match-p "\\`[A-Za-z0-9+/]+\\(?:=\\{0,2\\}\\)\\'" key)
           (let ((decoded (base64-decode-string key)))
             (and (= (string-bytes decoded) 16)
                  (equal key (base64-encode-string decoded t)))))
    (error nil)))

(defun mevedel-collaboration--send-http (process code type body)
  "Send bounded HTTP CODE response with TYPE and BODY to PROCESS."
  (let ((body (mevedel-collaboration--truncate-bytes
               (or body "") mevedel-collaboration--max-message-bytes)))
    (ws-response-header
     process code
     (cons "Content-Type" type)
     (cons "Content-Length" (string-bytes body))
     (cons "Content-Security-Policy"
           "default-src 'none'; script-src 'self'; style-src 'self'; connect-src 'self'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'")
     (cons "X-Content-Type-Options" "nosniff")
     (cons "Referrer-Policy" "no-referrer")
     (cons "Cache-Control" "no-store"))
    (process-send-string process body)))

(defun mevedel-collaboration--frame-info (bytes)
  "Return parsing information for the first frame in unibyte BYTES.

The result is a plist with `:state' `incomplete', `complete', `invalid', or
`too-large'.  A complete frame includes its byte length, opcode, FIN bit, and
payload length.  Keeping this small receive buffer outside GNU ELPA's parser
prevents it from slicing an incomplete frame before the package sees it."
  (if (< (length bytes) 2)
      '(:state incomplete)
    (let* ((first (aref bytes 0))
           (second (aref bytes 1))
           (fin (= (logand first #x80) #x80))
           (rsv (logand first #x70))
           (opcode (logand first #x0f))
           (masked (= (logand second #x80) #x80))
           (length-code (logand second #x7f))
           (extension-bytes (cond ((< length-code 126) 0)
                                  ((= length-code 126) 2)
                                  (t 8)))
           (header-length (+ 2 extension-bytes (if masked 4 0))))
      (cond
       ((or (/= rsv 0) (not masked) (not fin)
            (not (memq opcode '(0 1 2 8 9 10))))
        '(:state invalid))
       ((< (length bytes) header-length)
        '(:state incomplete))
       (t
        (let ((payload-length
               (cond
                ((< length-code 126) length-code)
                ((= length-code 126)
                 (+ (ash (aref bytes 2) 8) (aref bytes 3)))
                (t
                 (let ((value 0))
                   (dotimes (index 8)
                     (setq value (+ (ash value 8)
                                    (aref bytes (+ 2 index)))))
                   value)))))
          (cond
           ((or (> payload-length mevedel-collaboration--max-message-bytes)
                (and (memq opcode '(8 9 10))
                     (or (> payload-length 125) (not fin))))
            (list :state 'too-large :opcode opcode :fin fin
                  :payload-length payload-length))
           ((< (length bytes) (+ header-length payload-length))
            (list :state 'incomplete :opcode opcode :fin fin
                  :payload-length payload-length))
           (t
            (list :state 'complete
                  :length (+ header-length payload-length)
                  :opcode opcode :fin fin
                  :payload-length payload-length)))))))))

(defun mevedel-collaboration--frame-payload (frame info)
  "Return the unmasked payload from complete FRAME using INFO."
  (let* ((second (aref frame 1))
         (length-code (logand second #x7f))
         (extension-bytes (cond ((< length-code 126) 0)
                                ((= length-code 126) 2)
                                (t 8)))
         (mask-start (+ 2 extension-bytes))
         (payload-start (+ mask-start 4))
         (payload (copy-sequence
                   (substring frame payload-start
                              (+ payload-start (plist-get info :payload-length)))))
         (key (substring frame mask-start (+ mask-start 4))))
    (dotimes (index (length payload))
      (aset payload index
            (logxor (aref payload index) (aref key (mod index 4)))))
    payload))

(defun mevedel-collaboration--control-frame (opcode payload)
  "Return an unmasked final control frame with OPCODE and PAYLOAD."
  (let ((length (length payload)))
    (when (> length 125)
      (error "WebSocket control payload is too large"))
    (concat (unibyte-string (logior #x80 opcode) length) payload)))

(defun mevedel-collaboration--ws-filter (process string)
  "Bound and defragment raw WebSocket input before package parsing."
  (let ((pending (concat (or (process-get
                              process 'mevedel-collaboration-pending)
                             "")
                         string))
        done)
    (while (and (not done) (process-live-p process))
      (let ((info (mevedel-collaboration--frame-info pending)))
        (pcase (plist-get info :state)
          ('incomplete
           (mevedel-collaboration--guest-arm-receive-timer
            (process-get process 'mevedel-collaboration-guest))
           (setq done t))
          ((or 'invalid 'too-large)
           (mevedel-collaboration--guest-close
            (process-get process 'mevedel-collaboration-guest)
            (plist-get info :state))
           (setq pending "" done t))
          ('complete
           (let* ((frame-length (plist-get info :length))
                  (frame (substring pending 0 frame-length))
                  (opcode (plist-get info :opcode))
                  (fin (plist-get info :fin))
                  (payload-length (plist-get info :payload-length))
                  (fragmented (process-get
                               process 'mevedel-collaboration-fragmented))
                  (message-bytes
                   (or (process-get process
                                    'mevedel-collaboration-message-bytes)
                       0)))
             (cond
              ((and (= opcode 0) (not fragmented))
               (mevedel-collaboration--guest-close
                (process-get process 'mevedel-collaboration-guest)
                'unexpected-continuation)
               (setq pending "" done t))
              ((and (memq opcode '(1 2)) fragmented)
               (mevedel-collaboration--guest-close
                (process-get process 'mevedel-collaboration-guest)
                'fragmented-message)
               (setq pending "" done t))
              ((memq opcode '(9 10))
               (setq pending (substring pending frame-length))
               (process-put process 'mevedel-collaboration-pending pending)
               (when (= opcode 9)
                 (condition-case nil
                     (process-send-string
                      process
                      (mevedel-collaboration--control-frame
                       10 (mevedel-collaboration--frame-payload frame info)))
                   (error
                    (mevedel-collaboration--guest-close
                     (process-get process 'mevedel-collaboration-guest)
                     'malformed-frame))))
               (mevedel-collaboration--guest-clear-receive-timer
                (process-get process 'mevedel-collaboration-guest)))
              ((and (= opcode 8) (not fragmented))
               (setq pending (substring pending frame-length))
               (process-put process 'mevedel-collaboration-pending pending)
               (condition-case nil
                   (ws-web-socket-filter process frame)
                 (error
                  (mevedel-collaboration--guest-close
                   (process-get process 'mevedel-collaboration-guest)
                   'malformed-frame)))
               (when (= opcode 8) (setq done t)))
              (t
               (let ((total (+ message-bytes payload-length)))
                 (if (> total mevedel-collaboration--max-message-bytes)
                     (progn
                       (mevedel-collaboration--guest-close
                        (process-get process 'mevedel-collaboration-guest)
                        'incoming-too-large)
                       (setq pending "" done t))
                   (setq pending (substring pending frame-length))
                   (process-put process
                                'mevedel-collaboration-pending pending)
                   (process-put process 'mevedel-collaboration-message-bytes
                                (if fin 0 total))
                   (process-put process 'mevedel-collaboration-fragmented
                                (and (not fin) t))
                   (condition-case nil
                       (ws-web-socket-filter process frame)
                     (error
                      (mevedel-collaboration--guest-close
                       (process-get process 'mevedel-collaboration-guest)
                       'malformed-frame))))))))))))
    (when (process-live-p process)
      (process-put process 'mevedel-collaboration-pending pending)
      (if (> (length pending) 0)
          (mevedel-collaboration--guest-arm-receive-timer
           (process-get process 'mevedel-collaboration-guest))
        (mevedel-collaboration--guest-clear-receive-timer
         (process-get process 'mevedel-collaboration-guest))))))

(defun mevedel-collaboration--auth-timeout (process)
  "Close unauthenticated PROCESS after the auth deadline."
  (when-let ((guest (process-get process 'mevedel-collaboration-guest)))
    (unless (plist-get guest :authenticated)
      (mevedel-collaboration--guest-close guest 'auth-timeout))))

(defun mevedel-collaboration--ws-sentinel (process _event)
  "Forget a disconnected collaboration PROCESS."
  (when-let ((guest (process-get process 'mevedel-collaboration-guest)))
    ;; The process is already detached: cancel timers, but do not send a
    ;; close frame through the dead process.
    (mevedel-collaboration--guest-cancel-timers guest)
    ;; The upgraded socket keeps its request record so that `ws-stop' can
    ;; close it.  A dead socket must not keep that record, or every
    ;; handshake leaves one behind.
    (mevedel-collaboration--forget-request process)
    (when-let ((server (plist-get guest :close-server)))
      (setf (plist-get guest :close-server) nil)
      (condition-case nil
          (progn
            (mevedel-collaboration--preupgrade-stop server)
            (ws-stop server))
        (error nil)))
    (let ((room mevedel-collaboration--room))
      (when room
        (when (eq (plist-get room :guest) guest)
          (setq room (plist-put room :guest nil)))
        (when (memq guest (plist-get room :pending-auth))
          (setq room
                (plist-put room :pending-auth
                           (delq guest (plist-get room :pending-auth)))))
        (setq mevedel-collaboration--room room)))))

(defun mevedel-collaboration--auth-message (process data)
  "Authenticate PROCESS with its first DATA message."
  (let* ((guest (process-get process 'mevedel-collaboration-guest))
         (room mevedel-collaboration--room)
         object
         decoded)
    (require 'json)
    (condition-case nil
        (progn
          (setq decoded (decode-coding-string data 'utf-8))
          (unless (equal data (encode-coding-string decoded 'utf-8 t))
            (error "Invalid UTF-8"))
          (setq object
                (json-parse-string decoded
                                   :object-type 'plist
                                   :array-type 'list)))
      (error (setq object nil)))
    (if (and room guest
             (memq guest (plist-get room :pending-auth))
             (equal (plist-get object :type) "auth")
             (equal (plist-get object :version)
                    mevedel-collaboration--protocol-version)
             (equal (plist-get object :room) (plist-get room :room-id))
             ;; Compare digests so the one credential check does not leak
             ;; its matching prefix length through comparison time.
             (let ((offered (plist-get object :token))
                   (expected (plist-get room :token)))
               (and (stringp offered) (stringp expected)
                    (equal (secure-hash 'sha256 offered)
                           (secure-hash 'sha256 expected)))))
        (let ((current (plist-get room :guest)))
          (when (and current
                     (not (mevedel-collaboration--guest-live-p current)))
            (setq room (plist-put room :guest nil))
            (setq current nil))
          (if current
              (mevedel-collaboration--guest-close guest 'auth-rejected)
            (when-let ((timer (plist-get guest :auth-timer)))
              (cancel-timer timer))
            (setq room
                  (plist-put room :pending-auth
                             (delq guest (plist-get room :pending-auth))))
            (setq guest (plist-put guest :authenticated t))
            (setq room (plist-put room :guest guest))
            (setq mevedel-collaboration--room room)
            (process-put process 'mevedel-collaboration-guest guest)
            (mevedel-collaboration--send-snapshot guest room)))
      (mevedel-collaboration--guest-close guest 'auth-rejected))))

(defun mevedel-collaboration--ack-message (process data)
  "Accept the output acknowledgement DATA from authenticated PROCESS."
  (let ((guest (process-get process 'mevedel-collaboration-guest))
        object
        decoded)
    (require 'json)
    (condition-case nil
        (progn
          (setq decoded (decode-coding-string data 'utf-8))
          (unless (equal data (encode-coding-string decoded 'utf-8 t))
            (error "Invalid UTF-8"))
          (setq object
                (json-parse-string decoded
                                   :object-type 'plist
                                   :array-type 'list)))
      (error (setq object nil)))
    (let ((in-flight (and guest (plist-get guest :in-flight)))
          (sequence (and object (plist-get object :seq)))
          (ack-token (and object (plist-get object :ack-token))))
      (if (and (equal (plist-get object :type) "ack")
               (integerp sequence)
               (>= sequence 1)
               (stringp ack-token)
               in-flight
               (equal sequence (plist-get in-flight :sequence))
               (equal ack-token (plist-get in-flight :ack-token)))
          (progn
            (setf (plist-get guest :acknowledged-sequence) sequence)
            (setf (plist-get guest :acknowledged-token) ack-token)
            (when-let ((timer (plist-get guest :pump-timer)))
              (cancel-timer timer)
              (setf (plist-get guest :pump-timer) nil))
            (setf (plist-get guest :pump-timer)
                  (run-at-time 0 nil #'mevedel-collaboration--guest-pump
                               guest))
            (process-put process 'mevedel-collaboration-guest guest))
        (mevedel-collaboration--guest-close guest 'inbound-after-auth)))))

(defun mevedel-collaboration--ws-message (process data)
  "Handle one complete inbound WebSocket DATA message."
  (let ((guest (process-get process 'mevedel-collaboration-guest)))
    (if (not (plist-get guest :authenticated))
        (mevedel-collaboration--auth-message process data)
      (mevedel-collaboration--ack-message process data))))

(defun mevedel-collaboration--finish-stop (guest server reason)
  "Close GUEST and stop SERVER after a final status has been handed off."
  (mevedel-collaboration--guest-close guest reason)
  (mevedel-collaboration--preupgrade-stop server)
  (condition-case nil
      (ws-stop server)
    (error nil)))

(defun mevedel-collaboration--websocket (request room)
  "Upgrade REQUEST for ROOM, or send a bounded rejection."
  (let* ((process (mevedel-collaboration--web-server-slot request 'process))
         (upgrade (downcase (or (mevedel-collaboration--request-header
                                 request "upgrade") "")))
         (connection (downcase (or (mevedel-collaboration--request-header
                                    request "connection") "")))
         (version (mevedel-collaboration--request-header
                   request "sec-websocket-version"))
         (origin (mevedel-collaboration--request-header request "origin"))
         (key (mevedel-collaboration--request-header
               request "sec-websocket-key"))
         (guest (plist-get room :guest)))
    (cond
     ((not (mevedel-collaboration--allowed-origin-p room origin))
      (mevedel-collaboration--send-http
       process 403 "text/plain; charset=utf-8" "Origin rejected"))
     ((or (not (string-match-p "\\bwebsocket\\b" upgrade))
          (not (string-match-p "\\bupgrade\\b" connection))
          (not (equal version "13"))
          (not (mevedel-collaboration--valid-websocket-key-p key)))
      (mevedel-collaboration--send-http
       process 400 "text/plain; charset=utf-8" "Malformed WebSocket upgrade"))
     ((mevedel-collaboration--guest-live-p guest)
      (mevedel-collaboration--send-http
       process 409 "text/plain; charset=utf-8" "A viewer is already connected"))
     (t
      ;; Keep unauthenticated handshakes separate from the single
      ;; authenticated viewer slot.  Evict the oldest pending handshake so a
      ;; legitimate token holder can always take the slot.
      (let ((pending (mevedel-collaboration--room-pending-auth room)))
        (while (>= (length pending) mevedel-collaboration--max-pending-auth)
          (mevedel-collaboration--guest-close (car pending) 'auth-rejected)
          (setq pending (cdr pending)))
        (setq room (or mevedel-collaboration--room room)))
      ;; GNU ELPA 0.1.2 documents a `:keep-alive' throw on successful
      ;; upgrade, while the installed source returns the process.  Catch both
      ;; forms before installing our state; either way the package has already
      ;; created its `ws-message' parser object and binary filter.
      (let ((connected
             (catch 'close-connection
               (ws-web-socket-connect
                request #'mevedel-collaboration--ws-message))))
        (if (not (and (process-live-p process)
                      (plist-get (process-plist process) :message)))
            (mevedel-collaboration--send-http
             process 400 "text/plain; charset=utf-8" "Malformed WebSocket upgrade")
          (let* ((guest (list :process process
                              :authenticated nil
                              :pending-bytes 0
                              :queue nil
                              :in-flight nil
                              :acknowledged-sequence nil
                              :acknowledged-token nil
                              :next-sequence 0
                              :auth-timer nil
                              :pump-timer nil
                              :snapshot-queue nil
                              :snapshot-timer nil
                              :snapshot-active nil
                              :receive-timer nil
                              :after-snapshot-bytes 0
                              :close-timer nil
                              :close-server nil
                              :after-snapshot nil))
                 (timer (run-at-time mevedel-collaboration--auth-timeout nil
                                     #'mevedel-collaboration--auth-timeout
                                     process)))
            (setq guest (plist-put guest :auth-timer timer))
            (process-put process 'mevedel-collaboration-guest guest)
            (process-put process 'mevedel-collaboration-pending "")
            (process-put process 'mevedel-collaboration-message-bytes 0)
            (process-put process 'mevedel-collaboration-fragmented nil)
            (set-process-filter process #'mevedel-collaboration--ws-filter)
            (set-process-sentinel process #'mevedel-collaboration--ws-sentinel)
            (setq mevedel-collaboration--room
                  (plist-put room :pending-auth
                             (append
                              (mevedel-collaboration--room-pending-auth room)
                              (list guest))))
            (ignore connected)
            :keep-alive)))))))

(defun mevedel-collaboration--request-handler (request)
  "Serve one HTTP REQUEST for the active room."
  (let* ((room mevedel-collaboration--room)
         (process (mevedel-collaboration--web-server-slot request 'process))
         (path (cdr (assq :GET
                          (mevedel-collaboration--web-server-slot
                           request 'headers)))))
    (cond
     ((null room)
      (mevedel-collaboration--send-http
       process 410 "text/plain; charset=utf-8" "Collaboration ended"))
     ((equal path "/ws")
      (mevedel-collaboration--websocket request room))
     ((member path '("/" "/index.html" "/viewer.css" "/viewer.js"))
      (let ((asset (mevedel-collaboration--read-asset
                    (if (equal path "/") "index.html"
                      (substring path 1)))))
        (if asset
            (mevedel-collaboration--send-http
             process 200
             (cond ((string-suffix-p ".css" path) "text/css; charset=utf-8")
                   ((string-suffix-p ".js" path) "text/javascript; charset=utf-8")
                   (t "text/html; charset=utf-8"))
             asset)
          (mevedel-collaboration--send-http
           process 404 "text/plain; charset=utf-8" "Not found"))))
     (t
      ;; `web-server' returns 500 for an unmatched handler, so keep this
      ;; explicit fallback to make the route boundary a 404.
      (mevedel-collaboration--send-http
       process 404 "text/plain; charset=utf-8" "Not found")))))


(provide 'mevedel-collaboration-transport)
;;; mevedel-collaboration-transport.el ends here
