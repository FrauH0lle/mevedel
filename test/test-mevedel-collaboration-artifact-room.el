;;; test-mevedel-collaboration-artifact-room.el --- Artifact room tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Drives an origin's artifact room from outside, as a consumer of an
;; artifact link would: sealed frames through the in-process stub relay,
;; against a real workspace store and editing helper.  The frame exchanges
;; come from the fixtures the remote-artifact package replays
;; (remote-artifact/test/fake-room.mjs), so both sides test the same
;; protocol.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-collaboration-relay-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-collaboration-relay-test-support"))
(require 'cl-lib)
(require 'json)
(require 'websocket)
(require 'mevedel-artifact-lease)
(require 'mevedel-artifact-store)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-artifact-room)
(require 'mevedel-collaboration-transport)
(require 'mevedel-shared-editing)
(require 'mevedel-workspace)

(defconst mevedel-artifact-room-test--fixtures
  (file-name-concat
   (file-name-directory
    (or buffer-file-name load-file-name byte-compile-current-file))
   "fixtures" "remote-artifact")
  "Directory of the frame fixtures shared with the remote-artifact package.")


;;
;;; Origin fixture

(defun mevedel-artifact-room-test--sockets-closed (port)
  "Wait until no client WebSocket to the stub relay at PORT is live."
  (mevedel-relay-test--pump
   (lambda ()
     (not (cl-some (lambda (process)
                     (and (process-live-p process)
                          (string-prefix-p
                           (format "websocket to ws://127.0.0.1:%d/" port)
                           (process-name process))))
                   (process-list))))
   5))

(defmacro mevedel-artifact-room-test--with-origin (bindings &rest body)
  "Run BODY with an isolated origin bound per BINDINGS.
BINDINGS is (ROOT WORKSPACE STATE PORT).  ROOT is a fresh workspace
directory and WORKSPACE its workspace.  STATE is the stub relay's room
state and PORT its port; the relay URL points at it.  Artifact rooms,
editing runtimes, leases and the workspace registry are private to BODY,
and every room it started is stopped afterwards."
  (declare (indent 1) (debug t))
  (pcase-let ((`(,root ,workspace ,state ,port) bindings))
    `(mevedel-relay-test--with-stub-relay (,state ,port _server)
       (let* ((,root (file-name-as-directory
                      (make-temp-file "mevedel-artifact-room-" t)))
              (mevedel-user-dir (file-name-as-directory
                                 (file-name-concat ,root "user")))
              (mevedel-workspace--registry (make-hash-table :test #'equal))
              (mevedel-collaboration-artifact-room--rooms
               (make-hash-table :test #'equal))
              (mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
              (mevedel-artifact-lease--held (make-hash-table :test #'equal))
              (mevedel-session-durability--client-id (make-string 64 ?a))
              (mevedel-collaboration-relay-url
               (format "ws://127.0.0.1:%d" ,port))
              (mevedel-collaboration-relay-host-token nil)
              (,workspace (with-temp-buffer
                            (setq default-directory ,root)
                            (mevedel-workspace))))
         (cl-letf (((symbol-function 'mevedel-sleep-update) #'ignore))
           (unwind-protect (progn ,@body)
             (mevedel-collaboration-artifact-room-stop-all 'test)
             (mevedel-artifact-room-test--sockets-closed ,port)
             (mevedel-shared-editing-stop)
             (maphash (lambda (_directory held)
                        (when (timerp (plist-get held :timer))
                          (cancel-timer (plist-get held :timer))))
                      mevedel-artifact-lease--held)
             (delete-directory ,root t)))))))

(defun mevedel-artifact-room-test--call (workspace args)
  "Run editing ARGS for WORKSPACE through its queue and return the reply."
  (let (reply)
    (mevedel-shared-editing-call workspace args (lambda (value) (setq reply value)))
    (mevedel-test--await 10 "editing reply" reply)
    reply))

(defun mevedel-artifact-room-test--board (workspace title)
  "Create a whiteboard TITLE with one rectangle in WORKSPACE; return its id."
  (let (reply)
    (mevedel-shared-editing-create workspace "whiteboard" title "Host"
                                   (lambda (value) (setq reply value)))
    (mevedel-test--await 10 "creation settles" reply)
    (should-not (plist-get reply :error))
    (let ((id (plist-get reply :id)))
      (should-not
       (plist-get
        (mevedel-artifact-room-test--call
         workspace
         (list :action "patch" :id id :actor "Host" :opId "fixture-box"
               :changes (vector (list :id "box"
                                      :after (list :type "rectangle" :x 0 :y 0
                                                   :width 120 :height 60)))))
        :error))
      id)))

(defun mevedel-artifact-room-test--secret (link)
  "Return (ROOM-ID . SECRET-BYTES) from artifact LINK."
  (should (string-match "#\\([A-Za-z0-9_-]+\\)\\.\\([A-Za-z0-9_-]+\\)\\'" link))
  (cons (match-string 1 link)
        (mevedel-collaboration--base64url-decode (match-string 2 link))))


;;
;;; Fixtures and a consumer

(defun mevedel-artifact-room-test--fixture (name bindings)
  "Return the frames of fixture NAME with BINDINGS substituted.
BINDINGS is an alist of placeholder strings to values; the fixture's
own examples stand for any placeholder BINDINGS leaves out.  Each frame
is (FROM . FRAME), FROM being \"consumer\" or \"origin\"."
  (let* ((fixture (json-parse-string
                   (with-temp-buffer
                     (insert-file-contents
                      (file-name-concat mevedel-artifact-room-test--fixtures
                                        (concat name ".json")))
                     (buffer-string))
                   :object-type 'alist :array-type 'array
                   :false-object :false :null-object :null))
         (examples (mapcar (lambda (entry)
                             (cons (symbol-name (car entry)) (cdr entry)))
                           (alist-get 'bindings fixture)))
         (bind nil))
    (setq bind (lambda (value)
                 (cond
                  ((stringp value)
                   (if-let* ((entry (or (assoc value bindings)
                                        (assoc value examples))))
                       (cdr entry)
                     value))
                  ((vectorp value) (vconcat (mapcar bind value)))
                  ((consp value)
                   (mapcar (lambda (entry) (cons (car entry) (funcall bind (cdr entry))))
                           value))
                  (t value))))
    (mapcar (lambda (entry)
              (cons (alist-get 'from entry)
                    (funcall bind (alist-get 'frame entry))))
            (alist-get 'frames fixture))))

(defun mevedel-artifact-room-test--normal (value)
  "Return JSON VALUE with every object's keys in a stable order."
  (cond
   ((vectorp value) (vconcat (mapcar #'mevedel-artifact-room-test--normal value)))
   ((and (consp value) (consp (car value)))
    (sort (mapcar (lambda (entry)
                    (cons (car entry) (mevedel-artifact-room-test--normal (cdr entry))))
                  value)
          (lambda (a b) (string< (car a) (car b)))))
   (t value)))

(defun mevedel-artifact-room-test--parse (text)
  "Parse frame JSON TEXT the way fixtures are parsed."
  (json-parse-string text :object-type 'alist :array-type 'array
                     :false-object :false :null-object :null))

(defun mevedel-artifact-room-test--consumer (port link)
  "Connect a consumer of artifact LINK through the stub relay at PORT.
Return a plist holding its socket, room key and received frames."
  (pcase-let* ((`(,room-id . ,secret) (mevedel-artifact-room-test--secret link))
               (consumer (list :key (substring secret 0 32) :secret secret
                               :texts nil :ws nil)))
    (plist-put
     consumer :ws
     (websocket-open
      (format "ws://127.0.0.1:%d/r/%s?role=guest" port room-id)
      :on-message
      (lambda (_ws frame)
        (when (eq 'binary (websocket-frame-opcode frame))
          (when-let* ((text (mevedel-collaboration--unseal
                             (plist-get consumer :key)
                             (cdr (mevedel-collaboration--envelope-unpack
                                   (websocket-frame-payload frame))))))
            (plist-put consumer :texts
                       (append (plist-get consumer :texts) (list text))))))))
    (should (mevedel-relay-test--pump
             (lambda () (websocket-openp (plist-get consumer :ws)))))
    consumer))

(defun mevedel-artifact-room-test--write-token (consumer)
  "Return CONSUMER's write token as a hello carries it."
  (mevedel-collaboration--base64url (substring (plist-get consumer :secret) 32 48)))

(defun mevedel-artifact-room-test--send (consumer frame)
  "Send fixture FRAME from CONSUMER, sealed, as wire frames.
An editing frame's body travels as the chunked transfer it stands for."
  (let ((texts
         (if (and (equal "editing" (alist-get 't frame)) (assq 'body frame))
             (let* ((data (base64-encode-string
                           (encode-coding-string
                            (json-serialize (alist-get 'body frame)
                                            :false-object :false :null-object :null)
                            'utf-8)
                           t))
                    (total (length data))
                    (offset 0) chunks)
               (while (< offset total)
                 (let ((end (min total (+ offset 65536))))
                   (push (json-serialize
                          `((t . "editing") (reqId . ,(alist-get 'reqId frame))
                            (offset . ,offset) (total . ,total)
                            (data . ,(substring data offset end))))
                         chunks)
                   (setq offset end)))
               (nreverse chunks))
           (list (json-serialize frame :false-object :false :null-object :null)))))
    (dolist (text texts)
      (websocket-send
       (plist-get consumer :ws)
       (make-websocket-frame
        :opcode 'binary :completep t
        :payload (mevedel-collaboration--envelope-pack
                  0 (mevedel-collaboration--seal (plist-get consumer :key) text)))))))

(defun mevedel-artifact-room-test--frames (consumer)
  "Return the frames CONSUMER received, parsed like fixture frames.
An editing transfer appears once, complete, as its `body' shorthand."
  (let (frames transfers)
    (dolist (text (plist-get consumer :texts) (nreverse frames))
      (let ((frame (mevedel-artifact-room-test--parse text)))
        (if (not (equal "editing" (alist-get 't frame)))
            (push frame frames)
          (let* ((req-id (alist-get 'reqId frame))
                 (parts (concat (cdr (assoc req-id transfers))
                                (alist-get 'data frame))))
            (setf (alist-get req-id transfers nil nil #'equal) parts)
            (when (= (length parts) (alist-get 'total frame))
              (setq transfers (assoc-delete-all req-id transfers))
              (push `((t . "editing") (reqId . ,req-id)
                      (body . ,(mevedel-artifact-room-test--parse
                                (decode-coding-string (base64-decode-string parts)
                                                      'utf-8))))
                    frames))))))))

(defun mevedel-artifact-room-test--await (consumer count)
  "Wait until CONSUMER received COUNT frames; return them all."
  (mevedel-relay-test--pump
   (lambda () (>= (length (mevedel-artifact-room-test--frames consumer)) count)))
  (mevedel-artifact-room-test--frames consumer))

(defun mevedel-artifact-room-test--close (consumer)
  "Close CONSUMER's socket."
  (websocket-close (plist-get consumer :ws)))

(defun mevedel-artifact-room-test--host-joined (state)
  "Wait until the origin's room has dialed the stub relay with STATE."
  (should (mevedel-relay-test--pump (lambda () (plist-get (car state) :host)))))


;;
;;; Sharing

(mevedel-deftest mevedel-collaboration-artifact-room-share
  ()
  ,test
  (test)
  :doc "returns persistent view and edit links that carry no artifact id"
  (mevedel-artifact-room-test--with-origin (root workspace _state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (view (plist-get links :link-view))
           (edit (plist-get links :link-edit))
           (credentials (file-name-concat
                         (mevedel-artifact-store-bookkeeping-directory workspace id)
                         "link")))
      ;; WEB-ORIGIN/#ROOM-ID.SECRET, 32 bytes to view and 48 to edit.
      (should (string-prefix-p (format "http://127.0.0.1:%d/#" port) view))
      (should (string-prefix-p (format "http://127.0.0.1:%d/#" port) edit))
      (should (= 32 (length (cdr (mevedel-artifact-room-test--secret view)))))
      (should (= 48 (length (cdr (mevedel-artifact-room-test--secret edit)))))
      (should (equal (car (mevedel-artifact-room-test--secret view))
                     (car (mevedel-artifact-room-test--secret edit))))
      (should (string-prefix-p (cdr (mevedel-artifact-room-test--secret view))
                               (cdr (mevedel-artifact-room-test--secret edit))))
      (should-not (string-search id view))
      (should-not (string-search id edit))
      ;; Credentials live privately in the artifact's bookkeeping.
      (should (file-exists-p credentials))
      (should (= #o600 (file-modes credentials)))
      ;; Sharing again returns the same links, also once the room stopped.
      (should (equal links (mevedel-collaboration-artifact-room-share root id)))
      (mevedel-collaboration-artifact-room-stop-all 'test)
      (should (equal links (mevedel-collaboration-artifact-room-share root id)))))

  :doc "refuses an artifact that does not exist"
  (mevedel-artifact-room-test--with-origin (root _workspace _state _port)
    (should-error (mevedel-collaboration-artifact-room-share root "missing")
                  :type 'user-error)))

(mevedel-deftest mevedel-collaboration-share-artifact
  ()
  ,test
  (test)
  :doc "shares the chosen artifact, copies its edit link and shows both links"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (kill-ring nil)
           (default-directory root)
           candidates shown)
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (setq candidates (all-completions "" collection))
                       (car candidates)))
                    ((symbol-function 'yes-or-no-p) #'always))
            (mevedel-test--with-captured-messages nil
              (call-interactively #'mevedel-collaboration-share-artifact))
            (let ((links (mevedel-collaboration-artifact-room-share root id)))
              (should (equal 1 (length candidates)))
              (should (string-match-p "Pipeline" (car candidates)))
              (should (equal (plist-get links :link-edit) (car kill-ring)))
              (setq shown (with-current-buffer
                              mevedel-collaboration-artifact-room--links-buffer
                            (buffer-string)))
              (should (string-search (plist-get links :link-view) shown))
              (should (string-search (plist-get links :link-edit) shown))))
        (when-let* ((buffer (get-buffer mevedel-collaboration-artifact-room--links-buffer)))
          (dolist (window (get-buffer-window-list buffer nil t))
            (quit-window t window))
          (when (buffer-live-p buffer) (kill-buffer buffer))))))

  :doc "shares nothing when the host declines"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
          (default-directory root))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (car (all-completions "" collection))))
                ((symbol-function 'yes-or-no-p) #'ignore))
        (should-error (call-interactively #'mevedel-collaboration-share-artifact)
                      :type 'user-error))
      (should-not (file-exists-p
                   (file-name-concat
                    (mevedel-artifact-store-bookkeeping-directory workspace id)
                    "link"))))))


;;
;;; Joining

(mevedel-deftest mevedel-collaboration-artifact-room--on-frame
  ()
  ,test
  (test)
  :doc "answers an edit link's hello with the artifact welcome only"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (_ (mevedel-artifact-room-test--host-joined state))
           (consumer (mevedel-artifact-room-test--consumer
                      port (plist-get links :link-edit)))
           (bindings `(("$ID" . ,id) ("$TITLE" . "Pipeline")
                       ("$WRITE_TOKEN"
                        . ,(mevedel-artifact-room-test--write-token consumer)))))
      (unwind-protect
          (progn
            (dolist (frame (mevedel-artifact-room-test--fixture "hello" bindings))
              (mevedel-artifact-room-test--send consumer (cdr frame)))
            (mevedel-artifact-room-test--await consumer 1)
            ;; Anything more would arrive with the welcome.
            (accept-process-output nil 0.2)
            (should (equal (mapcar #'mevedel-artifact-room-test--normal
                                   (mapcar #'cdr (mevedel-artifact-room-test--fixture
                                                  "welcome" bindings)))
                           (mapcar #'mevedel-artifact-room-test--normal
                                   (mevedel-artifact-room-test--frames consumer)))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "welcomes a view link read-only and an edit link writable"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (_ (mevedel-artifact-room-test--host-joined state))
           (view (mevedel-artifact-room-test--consumer
                  port (plist-get links :link-view)))
           (edit (mevedel-artifact-room-test--consumer
                  port (plist-get links :link-edit))))
      (unwind-protect
          (progn
            (mevedel-artifact-room-test--send
             view '((t . "hello") (proto . 3) (name . "viewer")
                    (guestId . "consumer-view")))
            (mevedel-artifact-room-test--send
             edit `((t . "hello") (proto . 3) (name . "owner?")
                    (guestId . "consumer-edit")
                    (writeToken . ,(mevedel-artifact-room-test--write-token edit))))
            (should (eq t (alist-get 'readOnly
                                     (car (mevedel-artifact-room-test--await view 1)))))
            (should (eq :false (alist-get 'readOnly
                                          (car (mevedel-artifact-room-test--await edit 1))))))
        (mevedel-artifact-room-test--close view)
        (mevedel-artifact-room-test--close edit)))))


;;
;;; Reading

(defun mevedel-artifact-room-test--joined (state port links tier)
  "Return a consumer admitted to the room of LINKS at TIER (`view' or `edit').
STATE and PORT are the stub relay's."
  (mevedel-artifact-room-test--host-joined state)
  (let ((consumer (mevedel-artifact-room-test--consumer
                   port (plist-get links (if (eq tier 'edit) :link-edit :link-view)))))
    (mevedel-artifact-room-test--send
     consumer `((t . "hello") (proto . 3) (name . "remote agent")
                (guestId . "consumer-0001")
                ,@(when (eq tier 'edit)
                    `((writeToken . ,(mevedel-artifact-room-test--write-token
                                      consumer))))))
    (mevedel-artifact-room-test--await consumer 1)
    (plist-put consumer :texts nil)
    consumer))

(defun mevedel-artifact-room-test--request (consumer req-id body)
  "Send editing BODY as request REQ-ID from CONSUMER; return the reply body."
  (mevedel-artifact-room-test--send
   consumer `((t . "editing") (reqId . ,req-id) (body . ,body)))
  (mevedel-relay-test--pump
   (lambda () (cl-find req-id (mevedel-artifact-room-test--frames consumer)
                       :key (lambda (frame) (alist-get 'reqId frame)))))
  (alist-get 'body (cl-find req-id (mevedel-artifact-room-test--frames consumer)
                            :key (lambda (frame) (alist-get 'reqId frame)))))

(mevedel-deftest mevedel-collaboration-artifact-room--read
  ()
  ,test
  (test)
  :doc "returns the board's lines exactly as shared://ID shows them at the origin"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (text (plist-get
                  (plist-get (mevedel-artifact-room-test--call
                              workspace (list :action "view" :id id :part "overview"))
                             :result)
                  :text))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'view))
           (frames (mevedel-artifact-room-test--fixture
                    "read" `(("$ID" . ,id) ("$TEXT" . ,text)))))
      (unwind-protect
          (progn
            (should (string-match-p "\"id\":\"box\"" text))
            (mevedel-artifact-room-test--send consumer (cdr (assoc "consumer" frames)))
            (mevedel-artifact-room-test--await consumer 1)
            (should (equal (list (mevedel-artifact-room-test--normal
                                  (cdr (assoc "origin" frames))))
                           (mapcar #'mevedel-artifact-room-test--normal
                                   (mevedel-artifact-room-test--frames consumer)))))
        (mevedel-artifact-room-test--close consumer)))))


;;
;;; Authority

(mevedel-deftest mevedel-collaboration-artifact-room--dispatch
  ()
  ,test
  (test)
  :doc "drops every frame type outside the allowlist unanswered"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            (dolist (type '("prompt" "abort" "ui-response" "new-session" "fork"
                            "files" "file-get" "file-upload" "discussion"
                            "store-list" "store-action" "artifact-get"
                            "artifact-delete" "recovery" "lobby-refresh"
                            "open-session" "viewing" "editing-presence"))
              (mevedel-artifact-room-test--send
               consumer `((t . ,type) (reqId . 9) (id . ,id) (text . "hi"))))
            ;; A request after them is answered, so they were all handled.
            (should (mevedel-artifact-room-test--request
                     consumer 1 `((action . "read") (id . ,id))))
            (should (equal '(1) (mapcar (lambda (frame) (alist-get 'reqId frame))
                                        (mevedel-artifact-room-test--frames consumer)))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses editing actions outside the allowlist and any other artifact"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (other (mevedel-artifact-room-test--board workspace "Private"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (req-id 0))
      (unwind-protect
          (progn
            (dolist (body `(((action . "list"))
                            ((action . "status"))
                            ((action . "create") (id . "new-one") (kind . "document")
                             (title . "New"))
                            ((action . "delete") (id . ,id))
                            ((action . "update") (id . ,id) (opId . "x") (update . ""))
                            ((action . "export") (id . ,id))
                            ((action . "discussion") (id . ,id) (op . "list"))
                            ((action . "library"))
                            ((action . "read") (id . ,other))
                            ((action . "read") (id . "no-such-item"))))
              (let ((reply (mevedel-artifact-room-test--request
                            consumer (cl-incf req-id) body)))
                (should (stringp (alist-get 'error reply)))
                (should-not (assq 'result reply))))
            ;; Nothing changed: both items are still there, unrenamed.
            (should (equal '("Pipeline" "Private")
                           (mapcar (lambda (item)
                                     (plist-get (mevedel-artifact-store-meta
                                                 workspace item)
                                                :title))
                                   (list id other)))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "answers no request from a peer that never said hello"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (_ (mevedel-artifact-room-test--host-joined state))
           (stranger (mevedel-artifact-room-test--consumer
                      port (plist-get links :link-edit))))
      (unwind-protect
          (progn
            (mevedel-artifact-room-test--send
             stranger `((t . "editing") (reqId . 1)
                        (body . ((action . "read") (id . ,id)))))
            (accept-process-output nil 0.5)
            (should-not (mevedel-artifact-room-test--frames stranger)))
        (mevedel-artifact-room-test--close stranger)))))

(provide 'test-mevedel-collaboration-artifact-room)
;;; test-mevedel-collaboration-artifact-room.el ends here
