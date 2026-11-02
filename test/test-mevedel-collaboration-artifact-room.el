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
(require 'mevedel-collaboration-artifact)
(require 'mevedel-collaboration-artifact-room)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-share)
(require 'mevedel-collaboration-transport)
(require 'mevedel-shared-editing)
(require 'mevedel-shared-library)
(require 'mevedel-structs)
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

(defun mevedel-artifact-room-test--fixture (name bindings &optional keep)
  "Return the frames of fixture NAME with BINDINGS substituted.
BINDINGS is an alist of placeholder strings to values; the fixture's
own examples stand for any placeholder BINDINGS leaves out.  Each frame
is (FROM . FRAME), FROM being \"consumer\" or \"origin\".  With KEEP,
placeholders BINDINGS leaves out stay as they are, for
`mevedel-artifact-room-test--matches'."
  (let* ((fixture (json-parse-string
                   (with-temp-buffer
                     (insert-file-contents
                      (file-name-concat mevedel-artifact-room-test--fixtures
                                        (concat name ".json")))
                     (buffer-string))
                   :object-type 'alist :array-type 'array
                   :false-object :false :null-object :null))
         (examples (unless keep
                     (mapcar (lambda (entry)
                               (cons (symbol-name (car entry)) (cdr entry)))
                             (alist-get 'bindings fixture))))
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

(defun mevedel-artifact-room-test--kill-links-buffer ()
  "Close the artifact links buffer and its windows."
  (when-let* ((buffer (get-buffer mevedel-collaboration-artifact-room--links-buffer)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (quit-window t window))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(mevedel-deftest mevedel-collaboration-revoke-artifact
  ()
  ,test
  (test)
  :doc "offers only shared artifacts and revokes the chosen one once confirmed"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (_other (mevedel-artifact-room-test--board workspace "Private"))
           (default-directory root)
           candidates confirm)
      (mevedel-collaboration-artifact-room-share root id)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq candidates (all-completions "" collection))
                   (car candidates)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) confirm)))
        (should-error (call-interactively #'mevedel-collaboration-revoke-artifact)
                      :type 'user-error)
        (should (mevedel-artifact-store-shared-p workspace id))
        (setq confirm t)
        (mevedel-test--with-captured-messages nil
          (call-interactively #'mevedel-collaboration-revoke-artifact)))
      (should (equal 1 (length candidates)))
      (should (string-match-p "Pipeline" (car candidates)))
      (should-not (mevedel-artifact-store-shared-p workspace id))
      ;; Nothing is left to revoke.
      (should-error (call-interactively #'mevedel-collaboration-revoke-artifact)
                    :type 'user-error))))

(mevedel-deftest mevedel-collaboration-rotate-artifact
  ()
  ,test
  (test)
  :doc "replaces the chosen artifact's links and shows the new ones"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (old (mevedel-collaboration-artifact-room-share root id))
           (kill-ring nil)
           (default-directory root))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (car (all-completions "" collection))))
                    ((symbol-function 'yes-or-no-p) #'always))
            (mevedel-test--with-captured-messages nil
              (call-interactively #'mevedel-collaboration-rotate-artifact))
            (let ((links (mevedel-collaboration-artifact-room-share root id))
                  (shown (with-current-buffer
                             mevedel-collaboration-artifact-room--links-buffer
                           (buffer-string))))
              (should-not (equal old links))
              (should (equal (plist-get links :link-edit) (car kill-ring)))
              (should (string-search (plist-get links :link-view) shown))
              (should-not (string-search (plist-get old :link-view) shown))))
        (mevedel-artifact-room-test--kill-links-buffer)))))


;;
;;; Joining

(mevedel-deftest mevedel-collaboration-artifact-room-origin-privacy
  ()
  ,test
  (test)
  :doc "default welcome omits origin rather than disclosing workspace or host"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((mevedel-collaboration-artifact-room-origin-name nil)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (_ (mevedel-artifact-room-test--host-joined state))
           (consumer (mevedel-artifact-room-test--consumer
                      port (plist-get links :link-view))))
      (unwind-protect
          (progn
            (mevedel-artifact-room-test--send
             consumer '((t . "hello") (proto . 3) (name . "viewer")
                        (guestId . "privacy-consumer")))
            (should
             (equal (mapcar #'cdr (mevedel-artifact-room-test--fixture
                                  "welcome-private" `(("$ID" . ,id))))
                    (mevedel-artifact-room-test--await consumer 1))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--on-frame
  ()
  ,test
  (test)
  :doc "answers an edit link's hello with the artifact welcome only"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((mevedel-collaboration-artifact-room-origin-name "Shared studio")
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (_ (mevedel-artifact-room-test--host-joined state))
           (consumer (mevedel-artifact-room-test--consumer
                      port (plist-get links :link-edit)))
           (bindings `(("$ID" . ,id) ("$TITLE" . "Pipeline")
                       ("$ORIGIN" . "Shared studio")
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

(defun mevedel-artifact-room-test--request (consumer req-id body &optional timeout)
  "Send editing BODY as request REQ-ID from CONSUMER; return the reply body.
Wait at most TIMEOUT seconds, 5 by default, for the reply's last chunk;
only the newest frame is decoded while waiting, so a large reply costs
no more than its size."
  (mevedel-artifact-room-test--send
   consumer `((t . "editing") (reqId . ,req-id) (body . ,body)))
  (mevedel-relay-test--pump
   (lambda ()
     (when-let* ((text (car (last (plist-get consumer :texts))))
                 (frame (mevedel-artifact-room-test--parse text)))
       (and (equal req-id (alist-get 'reqId frame))
            (= (+ (alist-get 'offset frame) (length (alist-get 'data frame)))
               (alist-get 'total frame)))))
   timeout)
  (alist-get 'body (cl-find req-id (mevedel-artifact-room-test--frames consumer)
                            :key (lambda (frame) (alist-get 'reqId frame)))))

(defun mevedel-artifact-room-test--matches (expected actual)
  "Return non-nil when frame ACTUAL has fixture frame EXPECTED's shape.
A placeholder string in EXPECTED matches any string; everything else,
object keys included, must be equal."
  (cond
   ((and (stringp expected) (string-prefix-p "$" expected)) (stringp actual))
   ((vectorp expected)
    (and (vectorp actual) (= (length expected) (length actual))
         (cl-every #'mevedel-artifact-room-test--matches expected actual)))
   ((and (consp expected) (consp (car expected)))
    (and (consp actual) (= (length expected) (length actual))
         (cl-every (lambda (entry)
                     (and (assq (car entry) actual)
                          (mevedel-artifact-room-test--matches
                           (cdr entry) (alist-get (car entry) actual))))
                   expected)))
   (t (equal expected actual))))

(defun mevedel-artifact-room-test--replies (consumer)
  "Return the request replies CONSUMER received, without change events."
  (cl-remove "event" (mevedel-artifact-room-test--frames consumer)
             :key (lambda (frame) (alist-get 'reqId frame)) :test #'equal))

(defun mevedel-artifact-room-test--exchange (consumer name bindings)
  "Play fixture NAME's consumer frames from CONSUMER with BINDINGS.
Return (EXPECTED . ACTUAL): the fixture's origin frames, placeholders
left unbound, and the replies the origin sent."
  (let* ((frames (mevedel-artifact-room-test--fixture name bindings t))
         (expected (mapcar #'cdr (cl-remove "consumer" frames :key #'car :test #'equal))))
    (dolist (frame frames)
      (when (equal "consumer" (car frame))
        (mevedel-artifact-room-test--send consumer (cdr frame))))
    (mevedel-relay-test--pump
     (lambda () (>= (length (mevedel-artifact-room-test--replies consumer))
                    (length expected))))
    (cons expected (mevedel-artifact-room-test--replies consumer))))

(defun mevedel-artifact-room-test--view (workspace id &optional part)
  "Return the text of WORKSPACE's item ID as the origin shows PART of it.
PART defaults to \"overview\", the lines of `shared://ID'."
  (plist-get (plist-get (mevedel-artifact-room-test--call
                         workspace (list :action "view" :id id
                                         :part (or part "overview")))
                        :result)
             :text))

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

(mevedel-deftest mevedel-collaboration-artifact-room--read-history
  ()
  ,test
  (test)
  :doc "returns the recent contributions as shared://ID/history shows them, to any link"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (text (mevedel-artifact-room-test--view workspace id "history"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (viewer (mevedel-artifact-room-test--joined state port links 'view))
           (editor (mevedel-artifact-room-test--joined state port links 'edit))
           (frames (mevedel-artifact-room-test--fixture
                    "history" `(("$ID" . ,id) ("$TEXT" . ,text)))))
      (unwind-protect
          (progn
            (should (string-search "fixture-box · revision 2 · Host" text))
            (dolist (consumer (list viewer editor))
              (mevedel-artifact-room-test--send consumer (cdr (assoc "consumer" frames)))
              (mevedel-artifact-room-test--await consumer 1)
              (should (equal (list (mevedel-artifact-room-test--normal
                                    (cdr (assoc "origin" frames))))
                             (mapcar #'mevedel-artifact-room-test--normal
                                     (mevedel-artifact-room-test--frames consumer))))))
        (mevedel-artifact-room-test--close viewer)
        (mevedel-artifact-room-test--close editor)))))


;;
;;; Editing

(mevedel-deftest mevedel-collaboration-artifact-room--edit
  ()
  ,test
  (test)
  :doc "patches through an edit link, answering with the changed lines and picture"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           attached)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-artifact-store-attach)
                     (lambda (&rest args) (push args attached))))
            (pcase-let* ((`(,expected . ,actual)
                          (mevedel-artifact-room-test--exchange
                           consumer "patch" `(("$ID" . ,id))))
                         (result (alist-get 'result (alist-get 'body (car actual)))))
              (should (= 1 (length actual)))
              (should (mevedel-artifact-room-test--matches (car expected) (car actual)))
              ;; The changed element's stored line, and the board as drawn.
              (should (string-match-p "\\\\\"id\\\\\":\\\\\"next\\\\\"" (alist-get 'text result)))
              (should-not (string-match-p "\\\\\"id\\\\\":\\\\\"box\\\\\"" (alist-get 'text result)))
              (should (string-prefix-p "\x89PNG" (base64-decode-string (alist-get 'png result)))))
            ;; The origin holds the change, attributed to the remote agent by
            ;; its hello name, and attached the board to no session.
            (should (string-match-p "\"id\":\"next\"" (mevedel-artifact-room-test--view workspace id)))
            (should (string-search "Agent: remote agent (remote)"
                                   (mevedel-artifact-room-test--view workspace id "history")))
            (should-not attached))
        (mevedel-artifact-room-test--close consumer))))

  :doc "rejects a stale patch whole, returning the targets' current lines"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (before (mevedel-artifact-room-test--view workspace id))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (pcase-let* ((`(,expected . ,actual)
                        (mevedel-artifact-room-test--exchange
                         consumer "stale-patch" `(("$ID" . ,id)
                                                  ("$STALE_HASH" . "000000000000"))))
                       (body (alist-get 'body (car actual))))
            (should (mevedel-artifact-room-test--matches (car expected) (car actual)))
            ;; The current line is the one shared://ID shows for the box.
            (should (string-search
                     (concat "\n" (alist-get 'line (aref (alist-get 'targets body) 0)) "\n")
                     (concat "\n" before "\n")))
            (should (equal before (mevedel-artifact-room-test--view workspace id))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses a view link's edit as view-only and changes nothing"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (before (mevedel-artifact-room-test--view workspace id))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (pcase-let ((`(,expected . ,actual)
                       (mevedel-artifact-room-test--exchange
                        consumer "view-only" `(("$ID" . ,id)))))
            (should (equal (mapcar #'mevedel-artifact-room-test--normal expected)
                           (mapcar #'mevedel-artifact-room-test--normal actual)))
            (should (equal before (mevedel-artifact-room-test--view workspace id))))
        (mevedel-artifact-room-test--close consumer)))))

(defconst mevedel-artifact-room-test--png
  (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQ"
          "DwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
  "A 1x1 PNG, base64.")

(defun mevedel-artifact-room-test--contribution (reply)
  "Return the contribution id an edit REPLY body names."
  (let ((text (alist-get 'text (alist-get 'result reply))))
    (should (stringp text))
    (alist-get 'contribution (json-parse-string text :object-type 'alist))))

(defconst mevedel-artifact-room-test--next
  '((action . "patch")
    (changes . [((id . "next")
                 (after . ((type . "rectangle") (x . 200) (y . 0)
                           (width . 120) (height . 60))))]))
  "A consumer's patch adding the rectangle \"next\".")

(mevedel-deftest mevedel-collaboration-artifact-room--revert
  ()
  ,test
  (test)
  :doc "reverts a listed contribution through an edit link, as SharedEdit does"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (let ((contribution (mevedel-artifact-room-test--contribution
                               (mevedel-artifact-room-test--request
                                consumer 7 `((id . ,id) ,@mevedel-artifact-room-test--next)))))
            ;; The contribution read_artifact's history lists is the one reverted.
            (should (string-search contribution
                                   (mevedel-artifact-room-test--view workspace id "history")))
            (plist-put consumer :texts nil)
            (pcase-let ((`(,expected . ,actual)
                         (mevedel-artifact-room-test--exchange
                          consumer "revert" `(("$ID" . ,id)
                                              ("$TRANSACTION" . ,contribution)))))
              (should (= 1 (length actual)))
              (should (mevedel-artifact-room-test--matches (car expected) (car actual))))
            (should-not (string-match-p "\"id\":\"next\""
                                        (mevedel-artifact-room-test--view workspace id)))
            (should (string-match-p "\"id\":\"box\""
                                    (mevedel-artifact-room-test--view workspace id))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses a revert whose targets changed since, changing nothing"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (contribution (mevedel-artifact-room-test--contribution
                          (mevedel-artifact-room-test--request
                           consumer 1 `((id . ,id) ,@mevedel-artifact-room-test--next)))))
      (unwind-protect
          (progn
            ;; The origin moves the consumer's rectangle afterwards.
            (let* ((lines (mevedel-artifact-room-test--view workspace id))
                   (hash (and (string-match "^\\([0-9a-f]+\\) {\"id\":\"next\"" lines)
                              (match-string 1 lines))))
              (should hash)
              (should-not (plist-get (mevedel-artifact-room-test--call
                                      workspace (list :action "patch" :id id :actor "Host"
                                                      :opId "move-next"
                                                      :changes (vector (list :id "next" :hash hash
                                                                             :set (list :x 400)))))
                                     :error)))
            (let ((before (mevedel-artifact-room-test--view workspace id))
                  (reply (mevedel-artifact-room-test--request
                          consumer 2 `((action . "revert") (id . ,id)
                                       (transaction . ,contribution)))))
              (should (stringp (alist-get 'error reply)))
              (should-not (assq 'result reply))
              (should (equal before (mevedel-artifact-room-test--view workspace id)))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses a view link's revert as view-only"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (viewer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (should (equal "This artifact link is view-only"
                         (alist-get 'error (mevedel-artifact-room-test--request
                                            viewer 1 `((action . "revert") (id . ,id)
                                                       (transaction . "fixture-box"))))))
        (mevedel-artifact-room-test--close viewer))))

  :doc "lets the origin revert a consumer's contribution, as its Contributions offers"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (contribution (mevedel-artifact-room-test--contribution
                          (mevedel-artifact-room-test--request
                           consumer 1 `((id . ,id) ,@mevedel-artifact-room-test--next)))))
      (unwind-protect
          (progn
            ;; The browser offers Revert for contributions by an "Agent:" actor.
            (should (string-search (concat contribution " · revision 3 · Agent: ")
                                   (mevedel-artifact-room-test--view workspace id "history")))
            ;; Its Revert button sends this request.
            (should-not (plist-get (mevedel-artifact-room-test--call
                                    workspace (list :action "revert" :id id
                                                    :transaction contribution
                                                    :actor "Guest: Host" :opId "undo-remote"))
                                   :error))
            (should-not (string-match-p "\"id\":\"next\""
                                        (mevedel-artifact-room-test--view workspace id))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--edit-actions
  ()
  ,test
  (test)
  :doc "renames and recolours the artifact with SharedEdit's arguments"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            (should (assq 'result (mevedel-artifact-room-test--request
                                   consumer 1 `((action . "rename") (id . ,id)
                                                (title . "Pipeline v2")))))
            (should (assq 'result (mevedel-artifact-room-test--request
                                   consumer 2 `((action . "background") (id . ,id)
                                                (background . "#ffeedd")))))
            (should (equal "Pipeline v2"
                           (plist-get (mevedel-artifact-store-meta workspace id) :title)))
            (should (string-search "#ffeedd" (mevedel-artifact-room-test--view workspace id))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "uploads an image, then shows it and the board as pictures to any link"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (viewer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (let* ((added (mevedel-artifact-room-test--request
                         consumer 1 `((action . "image") (id . ,id)
                                      (data . ,mevedel-artifact-room-test--png)
                                      (x . 0) (y . 200) (width . 40))))
                 (text (alist-get 'text (alist-get 'result added)))
                 (file-id (and (string-match "\\\\\"fileId\\\\\":\\\\\"\\([^\\\"]+\\)" text)
                               (match-string 1 text))))
            (should file-id)
            (should (alist-get 'png (alist-get 'result added)))
            (let ((image (alist-get 'result (mevedel-artifact-room-test--request
                                             viewer 1 `((action . "view") (id . ,id)
                                                        (image . ,file-id)))))
                  (board (alist-get 'result (mevedel-artifact-room-test--request
                                             viewer 2 `((action . "view") (id . ,id))))))
              (should (equal "image/png" (alist-get 'mime image)))
              (should (equal (base64-decode-string mevedel-artifact-room-test--png)
                             (base64-decode-string (alist-get 'png image))))
              (should (string-prefix-p "\x89PNG" (base64-decode-string (alist-get 'png board))))))
        (mevedel-artifact-room-test--close consumer)
        (mevedel-artifact-room-test--close viewer))))

  :doc "refuses an image above SharedEdit's 4.7 MB limit"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (should (string-match-p
                   "limited to 4700000 bytes"
                   (alist-get 'error (mevedel-artifact-room-test--request
                                      consumer 1 `((action . "image") (id . ,id)
                                                   (data . ,(make-string 6300000 ?A)))))))
        (mevedel-artifact-room-test--close consumer)))))

(defun mevedel-artifact-room-test--withdraw-at (function withdraw)
  "Return a stand-in for FUNCTION that runs WITHDRAW once, before it.
FUNCTION is a step of the editing queue: admission or commit."
  (let ((original (symbol-function function)) (done nil))
    (lambda (&rest args)
      (unless done
        (setq done t)
        (funcall withdraw))
      (apply original args))))

(mevedel-deftest mevedel-collaboration-artifact-room--authorize
  ()
  ,test
  (test)
  :doc "drops an edit whose link lost write access after dispatch, before commit"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (before (mevedel-artifact-room-test--view workspace id))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (room (mevedel-collaboration-artifact-room--find root id)))
      (unwind-protect
          (progn
            ;; The helper computed the edit; the link is narrowed before
            ;; the queue commits it.
            (cl-letf (((symbol-function 'mevedel-shared-editing--accept)
                       (mevedel-artifact-room-test--withdraw-at
                        'mevedel-shared-editing--accept
                        (lambda ()
                          (maphash (lambda (_peer guest) (plist-put guest :writable nil))
                                   (plist-get room :guests))))))
              (should (equal "Editing authority ended"
                             (alist-get 'error
                                        (mevedel-artifact-room-test--request
                                         consumer 1
                                         `((action . "patch") (id . ,id)
                                           (changes . [((id . "next")
                                                        (after . ((type . "rectangle")
                                                                  (x . 200) (y . 0)
                                                                  (width . 10)
                                                                  (height . 10))))])))))))
            (should (equal before (mevedel-artifact-room-test--view workspace id))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "drops a queued edit whose room stopped before the queue admitted it"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (before (mevedel-artifact-room-test--view workspace id))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (room (mevedel-collaboration-artifact-room--find root id))
           (dispatched nil))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-shared-editing--drain)
                     (mevedel-artifact-room-test--withdraw-at
                      'mevedel-shared-editing--drain
                      (lambda ()
                        (setq dispatched t)
                        (mevedel-collaboration-artifact-room--stop room 'test)))))
            (mevedel-artifact-room-test--send
             consumer `((t . "editing") (reqId . 1)
                        (body . ((action . "rename") (id . ,id) (title . "Taken")))))
            (should (mevedel-relay-test--pump (lambda () dispatched)))
            ;; The queue is serial: once a later call settles, the edit has.
            (should (equal before (mevedel-artifact-room-test--view workspace id)))
            (should (equal "Pipeline"
                           (plist-get (mevedel-artifact-store-meta workspace id) :title))))
        (mevedel-artifact-room-test--close consumer)))))

;;
;;; Versions

(defun mevedel-artifact-room-test--versions (workspace id)
  "Return how many versions WORKSPACE's artifact ID has after queued saves."
  ;; A whiteboard's version is saved through its editing queue, which is
  ;; serial: once a later call settles, an earlier save has too.
  (mevedel-artifact-room-test--view workspace id)
  (length (mevedel-artifact-store-versions workspace id)))

(defun mevedel-artifact-room-test--rename (consumer req-id id title)
  "Rename artifact ID to TITLE from CONSUMER as request REQ-ID."
  (should (assq 'result (mevedel-artifact-room-test--request
                         consumer req-id `((action . "rename") (id . ,id)
                                           (title . ,title))))))

(mevedel-deftest mevedel-collaboration-artifact-room--versions
  ()
  ,test
  (test)
  :doc "records one version once the room's writes pause, each write restarting the wait"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((mevedel-collaboration-artifact-room-version-idle 1.0)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (before (mevedel-artifact-room-test--versions workspace id)))
      (unwind-protect
          (progn
            (mevedel-artifact-room-test--rename consumer 1 id "Pipeline v2")
            (accept-process-output nil 0.6)
            (mevedel-artifact-room-test--rename consumer 2 id "Pipeline v3")
            (accept-process-output nil 0.6)
            ;; Longer than the pause since the first write, not since the last.
            (should (= before (mevedel-artifact-room-test--versions workspace id)))
            (should (mevedel-relay-test--pump
                     (lambda () (> (mevedel-artifact-room-test--versions workspace id)
                                   before))
                     5))
            (accept-process-output nil 1.2)
            ;; One version for the whole stretch, holding its last write.
            (should (= (1+ before) (mevedel-artifact-room-test--versions workspace id)))
            (should (string-search
                     "Pipeline v3"
                     (with-temp-buffer
                       (insert-file-contents
                        (mevedel-artifact-store-version-path workspace id (1+ before)))
                       (buffer-string)))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "records the stretch's version when its consumer disconnects"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (before (mevedel-artifact-room-test--versions workspace id)))
      (mevedel-artifact-room-test--rename consumer 1 id "Pipeline v2")
      (should (= before (mevedel-artifact-room-test--versions workspace id)))
      (mevedel-artifact-room-test--close consumer)
      (should (mevedel-relay-test--pump
               (lambda () (= (1+ before)
                             (mevedel-artifact-room-test--versions workspace id)))
               5))))

  :doc "records no version for a stretch of reads"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((mevedel-collaboration-artifact-room-version-idle 0.2)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (before (mevedel-artifact-room-test--versions workspace id)))
      (should (mevedel-artifact-room-test--request
               consumer 1 `((action . "read") (id . ,id))))
      (accept-process-output nil 0.5)
      (mevedel-artifact-room-test--close consumer)
      (accept-process-output nil 0.5)
      (should (= before (mevedel-artifact-room-test--versions workspace id))))))

(mevedel-deftest mevedel-collaboration-artifact-room--changed
  ()
  ,test
  (test)
  :doc "tells a room's consumers of its artifact's changes and of no other's"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (other (mevedel-artifact-room-test--board workspace "Private"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'view))
           (events (lambda ()
                     (mapcar (lambda (frame) (alist-get 'body frame))
                             (cl-remove-if-not
                              (lambda (frame) (equal "event" (alist-get 'reqId frame)))
                              (mevedel-artifact-room-test--frames consumer))))))
      (unwind-protect
          (progn
            ;; Edits at the origin, as a local agent or browser makes them.
            (should-not (plist-get (mevedel-artifact-room-test--call
                                    workspace (list :action "rename" :id other
                                                    :title "Still private" :actor "Host" :opId "rename-other"))
                                   :error))
            (should-not (plist-get (mevedel-artifact-room-test--call
                                    workspace (list :action "rename" :id id
                                                    :title "Pipeline v2" :actor "Host" :opId "rename-own"))
                                   :error))
            (should (mevedel-relay-test--pump (lambda () (funcall events))))
            (accept-process-output nil 0.2)
            ;; One event: the other board's change never reached this room.
            (should (equal '(((event . "changed") (id . "ID") (kind . "whiteboard")
                              (revision . 3) (title . "Pipeline v2")))
                           (mapcar (lambda (event)
                                     (mevedel-artifact-room-test--normal
                                      (cons '(id . "ID")
                                            (assq-delete-all 'id (copy-sequence event)))))
                                   (funcall events))))
            (should (equal id (alist-get 'id (car (funcall events))))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "tells an HTML room's consumers when its files change, and of no other artifact's"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (mevedel-artifact-room-test--site workspace "landing")
    (mevedel-artifact-room-test--site workspace "other")
    (let* ((links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'view))
           (events (lambda ()
                     (mapcar (lambda (frame)
                               (mevedel-artifact-room-test--normal (alist-get 'body frame)))
                             (cl-remove-if-not
                              (lambda (frame) (equal "event" (alist-get 'reqId frame)))
                              (mevedel-artifact-room-test--frames consumer))))))
      (unwind-protect
          (progn
            ;; A store change that leaves this artifact's files alone says nothing.
            (mevedel-artifact-store-write-file workspace "other" "index.html" "<p>other</p>"
                                               mevedel-artifact-room-test--page-hash)
            (accept-process-output nil 0.2)
            (should-not (funcall events))
            ;; A write at the origin, as its own ApplyPatch makes, reaches it.
            (mevedel-artifact-store-write-file workspace "landing" "site.css" "h1 {}" nil)
            (should (mevedel-relay-test--pump (lambda () (funcall events))))
            (accept-process-output nil 0.2)
            (should (equal '(((event . "changed") (id . "landing") (kind . "html")
                              (title . "Landing")))
                           (funcall events))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--roster
  ()
  ,test
  (test)
  :doc "lists a connected consumer by name on its artifact for origin browsers"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (session (mevedel-session--create :name "s1" :workspace workspace))
           ;; An origin browser has the board open in a session room.
           (browser (list :session session :transport 'browser
                          :guests (let ((guests (make-hash-table :test #'eql)))
                                    (puthash 1 (list :name "Host" :guest-id "host-browser"
                                                     :viewing id :active t)
                                             guests)
                                    guests)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry browser))
           (send (symbol-function 'mevedel-collaboration--transport-send))
           (roster nil)
           (names (lambda ()
                    (mapcar (lambda (person) (plist-get person :name))
                            (plist-get (car roster) :people)))))
      (setf (mevedel-session-session-id session) "s1")
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (transport peer frame)
                   (if (eq transport 'browser)
                       (when (equal "presence" (plist-get frame :t))
                         (push frame roster))
                     (funcall send transport peer frame)))))
        (let ((consumer (mevedel-artifact-room-test--joined state port links 'edit)))
          (unwind-protect
              (progn
                (should (equal '("remote agent (remote)") (funcall names)))
                ;; Consumers report no page and hear no presence.
                (should (mevedel-artifact-room-test--request
                         consumer 1 `((action . "read") (id . ,id))))
                (should (equal '(1) (mapcar (lambda (frame) (alist-get 'reqId frame))
                                            (mevedel-artifact-room-test--frames consumer)))))
            (mevedel-artifact-room-test--close consumer))
          (should (mevedel-relay-test--pump (lambda () (null (funcall names))) 5))))))

  :doc "takes its consumers off the roster when the room stops"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (session (mevedel-session--create :name "s1" :workspace workspace))
           (browser (list :session session :transport 'browser
                          :guests (let ((guests (make-hash-table :test #'eql)))
                                    (puthash 1 (list :name "Host" :guest-id "host-browser"
                                                     :viewing id :active t)
                                             guests)
                                    guests)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry browser))
           (send (symbol-function 'mevedel-collaboration--transport-send))
           (roster nil))
      (setf (mevedel-session-session-id session) "s1")
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (transport peer frame)
                   (if (eq transport 'browser)
                       (when (equal "presence" (plist-get frame :t))
                         (push frame roster))
                     (funcall send transport peer frame)))))
        (let ((consumer (mevedel-artifact-room-test--joined state port links 'view)))
          (unwind-protect
              (progn
                (should (= 1 (length (plist-get (car roster) :people))))
                (mevedel-collaboration-artifact-room--stop
                 (mevedel-collaboration-artifact-room--find root id) 'test)
                (should (equal [] (plist-get (car roster) :people))))
            (mevedel-artifact-room-test--close consumer)))))))


;;
;;; Libraries

(defconst mevedel-artifact-room-test--library
  (concat "{\"type\":\"excalidrawlib\",\"version\":2,\"libraryItems\":"
          "[{\"id\":\"cube\",\"name\":\"Cube\",\"status\":\"published\","
          "\"created\":1,\"elements\":[{\"id\":\"lib-a\",\"type\":\"rectangle\","
          "\"x\":10,\"y\":10,\"width\":50,\"height\":40,\"angle\":0,"
          "\"strokeColor\":\"#1e1e1e\",\"backgroundColor\":\"transparent\","
          "\"fillStyle\":\"solid\",\"strokeWidth\":2,\"strokeStyle\":\"solid\","
          "\"roughness\":1,\"opacity\":100,\"groupIds\":[],\"frameId\":null,"
          "\"roundness\":null,\"seed\":1,\"version\":1,\"versionNonce\":1,"
          "\"isDeleted\":false,\"boundElements\":null,\"updated\":1,\"link\":null,"
          "\"locked\":false,\"index\":\"a0\"}]}]}")
  "A one-item Excalidraw library, as a user's library file holds it.")

(defmacro mevedel-artifact-room-test--with-library (&rest body)
  "Run BODY with a private library directory holding the user's library."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "mevedel-artifact-library-" t))
          (mevedel-shared-library-directory directory))
     (unwind-protect
         (progn
           (write-region mevedel-artifact-room-test--library nil
                         (expand-file-name "My library.excalidrawlib" directory)
                         nil 'silent)
           ,@body)
       (delete-directory directory t))))

(defun mevedel-artifact-room-test--document (workspace title)
  "Create a document TITLE in WORKSPACE; return its id."
  (let (reply)
    (mevedel-shared-editing-create workspace "document" title "Host"
                                   (lambda (value) (setq reply value)))
    (mevedel-test--await 10 "creation settles" reply)
    (should-not (plist-get reply :error))
    (plist-get reply :id)))

(mevedel-deftest mevedel-collaboration-artifact-room--library
  ()
  ,test
  (test)
  :doc "lists the origin user's libraries and previews them to a view link"
  (mevedel-artifact-room-test--with-library
    (mevedel-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
             (links (mevedel-collaboration-artifact-room-share root id))
             (viewer (mevedel-artifact-room-test--joined state port links 'view)))
        (unwind-protect
            (let ((listing (alist-get 'result
                                      (mevedel-artifact-room-test--request
                                       viewer 1 '((action . "library")))))
                  (sheet (alist-get 'result
                                    (mevedel-artifact-room-test--request
                                     viewer 2 '((action . "library") (part . "sheet")
                                                (library . "My library")))))
                  (item (alist-get 'result
                                   (mevedel-artifact-room-test--request
                                    viewer 3 '((action . "library") (part . "item")
                                               (item . "My library/cube"))))))
              (should (string-search "My library/cube" (alist-get 'text listing)))
              (should (string-search "Built-in" (alist-get 'text listing)))
              (should (equal "image/png" (alist-get 'mime sheet)))
              (should (string-prefix-p "\x89PNG" (base64-decode-string
                                                  (alist-get 'png sheet))))
              ;; One item previews alone, unlike the library's sheet.
              (should (equal "image/png" (alist-get 'mime item)))
              (should (string-prefix-p "\x89PNG" (base64-decode-string (alist-get 'png item))))
              (should-not (equal (alist-get 'png sheet) (alist-get 'png item)))
              (should (string-match-p
                       "No library item My library/sphere"
                       (alist-get 'error
                                  (mevedel-artifact-room-test--request
                                   viewer 4 '((action . "library") (part . "item")
                                              (item . "My library/sphere")))))))
          (mevedel-artifact-room-test--close viewer)))))

  :doc "reports an unknown library as an error"
  (mevedel-artifact-room-test--with-library
    (mevedel-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
             (links (mevedel-collaboration-artifact-room-share root id))
             (viewer (mevedel-artifact-room-test--joined state port links 'view)))
        (unwind-protect
            (should (string-match-p
                     "No element library"
                     (alist-get 'error
                                (mevedel-artifact-room-test--request
                                 viewer 1 '((action . "library") (library . "Nope"))))))
          (mevedel-artifact-room-test--close viewer)))))

  :doc "inserts a library item through an edit link and refuses a view link"
  (mevedel-artifact-room-test--with-library
    (mevedel-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
             (links (mevedel-collaboration-artifact-room-share root id))
             (editor (mevedel-artifact-room-test--joined state port links 'edit))
             (viewer (mevedel-artifact-room-test--joined state port links 'view)))
        (unwind-protect
            (let ((before (mevedel-artifact-room-test--view workspace id)))
              (should (equal "This artifact link is view-only"
                             (alist-get 'error
                                        (mevedel-artifact-room-test--request
                                         viewer 1 `((action . "insert") (id . ,id)
                                                    (item . "My library/cube")
                                                    (x . 300) (y . 0))))))
              (should (equal before (mevedel-artifact-room-test--view workspace id)))
              (let ((reply (mevedel-artifact-room-test--request
                            editor 1 `((action . "insert") (id . ,id)
                                       (item . "My library/cube") (x . 300) (y . 0)))))
                (should (assq 'result reply))
                (should (alist-get 'png (alist-get 'result reply))))
              (should (string-search "\"type\":\"rectangle\""
                                     (mevedel-artifact-room-test--view workspace id)))
              (should (string-search "Agent: remote agent (remote)"
                                     (mevedel-artifact-room-test--view workspace id "history")))
              (should (string-match-p
                       "No element library"
                       (alist-get 'error
                                  (mevedel-artifact-room-test--request
                                   editor 2 `((action . "insert") (id . ,id)
                                              (item . "Nope/cube") (x . 0) (y . 0)))))))
          (mevedel-artifact-room-test--close editor)
          (mevedel-artifact-room-test--close viewer)))))

  :doc "gives a document room no library access, even to an edit link"
  (mevedel-artifact-room-test--with-library
    (mevedel-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (mevedel-artifact-room-test--document workspace "Notes"))
             (links (mevedel-collaboration-artifact-room-share root id))
             (editor (mevedel-artifact-room-test--joined state port links 'edit))
             (req-id 0))
        (unwind-protect
            (dolist (body `(((action . "library"))
                            ((action . "library") (part . "sheet"))
                            ((action . "insert") (id . ,id) (item . "My library/cube"))))
              (let ((reply (mevedel-artifact-room-test--request
                            editor (cl-incf req-id) body)))
                (should (stringp (alist-get 'error reply)))
                (should-not (assq 'result reply))))
          (mevedel-artifact-room-test--close editor)))))

  :doc "never lets a link change the origin user's libraries"
  (mevedel-artifact-room-test--with-library
    (mevedel-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
             (links (mevedel-collaboration-artifact-room-share root id))
             (editor (mevedel-artifact-room-test--joined state port links 'edit))
             (file (expand-file-name "My library.excalidrawlib"
                                     mevedel-shared-library-directory))
             (req-id 0))
        (unwind-protect
            (progn
              (dolist (body `(((action . "library-add")
                               (text . ,mevedel-artifact-room-test--library))
                              ((action . "library-remove") (ids . ["cube"]))
                              ((action . "library-uninstall") (name . "My library"))))
                (should (stringp (alist-get 'error
                                            (mevedel-artifact-room-test--request
                                             editor (cl-incf req-id) body)))))
              (should (equal mevedel-artifact-room-test--library
                             (with-temp-buffer (insert-file-contents file)
                                               (buffer-string)))))
          (mevedel-artifact-room-test--close editor))))))


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
                            ;; Versions stay the origin's: none listed, saved or restored.
                            ((action . "version") (id . ,id))
                            ((action . "versions") (id . ,id))
                            ((action . "restore") (id . ,id) (n . 1))
                            ((action . "library-add") (text . "{}"))
                            ((action . "library-remove") (ids . ["a"]))
                            ((action . "library-uninstall") (name . "Built-in"))
                            ((action . "library-install") (source . "a/b.excalidrawlib"))
                            ((action . "library-catalog"))
                            ((action . "library-fetch") (path . "x"))
                            ((action . "library-view") (part . "list"))
                            ((action . "read") (id . ,other))
                            ((action . "read") (id . "no-such-item"))
                            ;; Files belong to HTML artifacts only.
                            ((action . "read") (id . ,id) (file . "index.html"))
                            ((action . "files") (id . ,id))
                            ((action . "write-file") (id . ,id) (file . "x.html") (text . "x"))
                            ((action . "delete-file") (id . ,id) (file . "x.html")
                             (hash . ,(make-string 64 ?0)))))
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


;;
;;; HTML files

(defconst mevedel-artifact-room-test--page "<!doctype html>\n<h1>Hello</h1>\n"
  "The primary file of the HTML artifact the fixtures use.")

(defconst mevedel-artifact-room-test--page-hash
  "28c3689be4e0c696a6a45b9d7df5a41c8328b1c9939e240f27f3c6ac9a635099"
  "SHA-256 of `mevedel-artifact-room-test--page', computed by sha256sum.")

(defun mevedel-artifact-room-test--site (workspace id)
  "Create HTML artifact ID in WORKSPACE with a page and a binary asset.
Return the artifact's directory."
  (mevedel-artifact-store-create-meta workspace id "index.html" nil "Landing")
  (let ((directory (mevedel-artifact-store-artifact-directory workspace id)))
    (make-directory (file-name-concat directory "assets") t)
    (let ((coding-system-for-write 'no-conversion))
      (write-region mevedel-artifact-room-test--page nil
                    (file-name-concat directory "index.html") nil 'silent)
      (write-region (unibyte-string #x89 ?P ?N ?G 0 #xff 1 2) nil
                    (file-name-concat directory "assets" "logo.png") nil 'silent))
    directory))

(defun mevedel-artifact-room-test--file (directory file)
  "Return the bytes of FILE in DIRECTORY, or nil when it is absent."
  (let ((path (file-name-concat directory file)))
    (when (file-exists-p path)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (buffer-string)))))

(mevedel-deftest mevedel-collaboration-artifact-room--html-read
  ()
  ,test
  (test)
  :doc "shares an HTML artifact and reads its files through a view link"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (mevedel-artifact-room-test--site workspace "landing")
    (let* ((links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (pcase-let ((`(,expected . ,actual)
                         (mevedel-artifact-room-test--exchange
                          consumer "read-file" '(("$ID" . "landing")))))
              (should (equal (mapcar #'mevedel-artifact-room-test--normal expected)
                             (mapcar #'mevedel-artifact-room-test--normal actual))))
            ;; Without a file, the primary file.
            (let ((reply (alist-get 'result (mevedel-artifact-room-test--request
                                             consumer 2 '((action . "read"))))))
              (should (equal "index.html" (alist-get 'file reply)))
              (should (equal mevedel-artifact-room-test--page (alist-get 'text reply))))
            ;; Bytes that are not text arrive as base64 data.
            (let ((reply (alist-get 'result (mevedel-artifact-room-test--request
                                             consumer 3 '((action . "read")
                                                          (file . "assets/logo.png"))))))
              (should-not (assq 'text reply))
              (should (equal 8 (alist-get 'bytes reply)))
              (should (equal (unibyte-string #x89 ?P ?N ?G 0 #xff 1 2)
                             (base64-decode-string (alist-get 'data reply)))))
            ;; Every file of the artifact's directory is listed.
            (should (equal '(((bytes . 8) (file . "assets/logo.png"))
                             ((bytes . 31) (file . "index.html")))
                           (append
                            (alist-get 'files
                                       (mevedel-artifact-room-test--normal
                                        (alist-get 'result (mevedel-artifact-room-test--request
                                                            consumer 4 '((action . "files"))))))
                            nil))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--html-write
  ()
  ,test
  (test)
  :doc "replaces a file whose hash still matches and tells the store's observers"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (changed nil)
           (mevedel-artifact-store-changed-functions
            (list (lambda (changed-workspace) (push changed-workspace changed)))))
      (unwind-protect
          (pcase-let ((`(,expected . ,actual)
                       (mevedel-artifact-room-test--exchange
                        consumer "write-file" '(("$ID" . "landing")))))
            (should (equal (mapcar #'mevedel-artifact-room-test--normal expected)
                           (mapcar #'mevedel-artifact-room-test--normal actual)))
            (should (equal "<!doctype html>\n<h1>Hello, world</h1>\n"
                           (mevedel-artifact-room-test--file directory "index.html")))
            ;; Origin rooms and viewers follow the store as after a local write.
            (should (memq workspace changed)))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses a stale write, returning the current hash and content"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            ;; Someone at the origin changed the page after the consumer read it.
            (let ((coding-system-for-write 'no-conversion))
              (write-region "<!doctype html>\n<h1>Hello, world</h1>\n" nil
                            (file-name-concat directory "index.html") nil 'silent))
            (pcase-let ((`(,expected . ,actual)
                         (mevedel-artifact-room-test--exchange
                          consumer "write-conflict" '(("$ID" . "landing")))))
              (should (equal (mapcar #'mevedel-artifact-room-test--normal expected)
                             (mapcar #'mevedel-artifact-room-test--normal actual))))
            (should (equal "<!doctype html>\n<h1>Hello, world</h1>\n"
                           (mevedel-artifact-room-test--file directory "index.html"))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "creates a file, and its directories, only where none exists"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (css "h1 { color: teal; }\n"))
      (unwind-protect
          (let ((created (mevedel-artifact-room-test--request
                          consumer 1 `((action . "write-file") (file . "styles/site.css")
                                       (hash . :null) (text . ,css))))
                (again (mevedel-artifact-room-test--request
                        consumer 2 '((action . "write-file") (file . "index.html")
                                     (text . "<p>replaced</p>"))))
                (binary (mevedel-artifact-room-test--request
                         consumer 3 `((action . "write-file") (file . "assets/dot.png")
                                      (data . ,(base64-encode-string
                                                (unibyte-string #x89 ?P 0 #xfe)))))))
            (should (eq t (alist-get 'created (alist-get 'result created))))
            (should (equal css (mevedel-artifact-room-test--file directory "styles/site.css")))
            ;; An existing file is not created over: the consumer gets it instead.
            (should (equal "stale" (alist-get 'code again)))
            (should (equal mevedel-artifact-room-test--page-hash (alist-get 'hash again)))
            (should (equal mevedel-artifact-room-test--page (alist-get 'text again)))
            (should (equal mevedel-artifact-room-test--page
                           (mevedel-artifact-room-test--file directory "index.html")))
            (should (assq 'result binary))
            (should (equal (unibyte-string #x89 ?P 0 #xfe)
                           (mevedel-artifact-room-test--file directory "assets/dot.png"))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses a view link's write"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (let ((reply (mevedel-artifact-room-test--request
                        consumer 1 `((action . "write-file") (file . "index.html")
                                     (hash . ,mevedel-artifact-room-test--page-hash)
                                     (text . "<p>defaced</p>")))))
            (should (equal "This artifact link is view-only" (alist-get 'error reply)))
            (should (equal mevedel-artifact-room-test--page
                           (mevedel-artifact-room-test--file directory "index.html"))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--html-paths
  ()
  ,test
  (test)
  :doc "refuses paths outside the artifact directory, through links or into bookkeeping"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (outside (make-temp-file "mevedel-artifact-outside-" t))
           (bookkeeping (mevedel-artifact-store-bookkeeping-directory workspace "landing"))
           (_ (make-symbolic-link (file-name-concat outside "secret.html")
                                  (file-name-concat directory "linked.html")))
           (_ (make-symbolic-link outside (file-name-concat directory "linked")))
           (_ (with-temp-file (file-name-concat outside "secret.html") (insert "secret")))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (meta (mevedel-artifact-room-test--file bookkeeping "meta.el"))
           (req-id 0))
      (unwind-protect
          (progn
            (dolist (file (list "../.state/landing/meta.el" "../landing/index.html"
                                (file-name-concat bookkeeping "meta.el") "/etc/passwd"
                                "assets/../index.html" "./index.html" "assets/" ""
                                "linked.html" "linked/secret.html" "linked/new.html"))
              (dolist (body `(((action . "read") (file . ,file))
                              ((action . "write-file") (file . ,file) (text . "x"))
                              ((action . "delete-file") (file . ,file)
                               (hash . ,mevedel-artifact-room-test--page-hash))))
                (let ((reply (mevedel-artifact-room-test--request
                              consumer (cl-incf req-id) body)))
                  (should (string-prefix-p "Refused path" (or (alist-get 'error reply) "")))
                  (should-not (assq 'result reply)))))
            ;; Nothing outside the artifact changed, nor its bookkeeping.
            (should (equal "secret" (mevedel-artifact-room-test--file outside "secret.html")))
            (should-not (file-exists-p (file-name-concat outside "new.html")))
            (should (equal meta (mevedel-artifact-room-test--file bookkeeping "meta.el")))
            ;; Links are left out of the listing.
            (should (equal ["assets/logo.png" "index.html"]
                           (cl-map 'vector (lambda (row) (alist-get 'file row))
                                   (alist-get 'files
                                              (alist-get 'result
                                                         (mevedel-artifact-room-test--request
                                                          consumer (cl-incf req-id)
                                                          '((action . "files")))))))))
        (mevedel-artifact-room-test--close consumer)
        (delete-directory outside t))))

  :doc "carries files up to 16 MiB and refuses larger ones"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (largest (string-to-unibyte
                     (make-string (* 16 1024 1024) (unibyte-char-to-multibyte #xff)))))
      (unwind-protect
          (progn
            ;; A binary asset at the limit travels as base64 inside the transfer.
            (should (assq 'result (mevedel-artifact-room-test--request
                                   consumer 1 `((action . "write-file")
                                                (file . "assets/large.bin")
                                                (data . ,(base64-encode-string largest t))))))
            (should (equal largest (mevedel-artifact-room-test--file directory
                                                                     "assets/large.bin")))
            ;; And reads back whole through the link.
            (let ((read (alist-get 'result (mevedel-artifact-room-test--request
                                            consumer 4 '((action . "read")
                                                         (file . "assets/large.bin"))
                                            60))))
              (should (equal (* 16 1024 1024) (alist-get 'bytes read)))
              (should (equal largest (base64-decode-string (alist-get 'data read)))))
            ;; A larger file placed at the origin is listed but not read.
            (let ((coding-system-for-write 'no-conversion))
              (write-region (concat largest "x") nil
                            (file-name-concat directory "assets/huge.bin") nil 'silent))
            (should (string-search
                     "larger than"
                     (alist-get 'error (mevedel-artifact-room-test--request
                                        consumer 2 '((action . "read")
                                                     (file . "assets/huge.bin"))))))
            (delete-file (file-name-concat directory "assets/huge.bin"))
            (let ((reply (mevedel-artifact-room-test--request
                          consumer 3 `((action . "write-file") (file . "assets/huge.bin")
                                       (data . ,(base64-encode-string
                                                 (concat largest "x") t))))))
              (should (string-search "larger than" (alist-get 'error reply))))
            (should-not (file-exists-p (file-name-concat directory "assets/huge.bin"))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--html-delete
  ()
  ,test
  (test)
  :doc "deletes a file whose hash still matches, never the primary file or the artifact"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (logo (alist-get 'hash (alist-get 'result (mevedel-artifact-room-test--request
                                                      consumer 1 '((action . "read")
                                                                   (file . "assets/logo.png"))))))
           (stale (mevedel-artifact-room-test--request
                   consumer 2 `((action . "delete-file") (file . "assets/logo.png")
                                (hash . ,mevedel-artifact-room-test--page-hash))))
           (deleted (mevedel-artifact-room-test--request
                     consumer 3 `((action . "delete-file") (file . "assets/logo.png")
                                  (hash . ,logo))))
           (primary (mevedel-artifact-room-test--request
                     consumer 4 `((action . "delete-file") (file . "index.html")
                                  (hash . ,mevedel-artifact-room-test--page-hash))))
           (artifact (mevedel-artifact-room-test--request
                      consumer 5 '((action . "delete") (id . "landing")))))
      (unwind-protect
          (progn
            (should (equal "stale" (alist-get 'code stale)))
            (should (equal logo (alist-get 'hash stale)))
            (should (assq 'result deleted))
            (should-not (file-exists-p (file-name-concat directory "assets/logo.png")))
            (should (string-search "primary file" (alist-get 'error primary)))
            (should (stringp (alist-get 'error artifact)))
            (should (equal mevedel-artifact-room-test--page
                           (mevedel-artifact-room-test--file directory "index.html")))
            (should (mevedel-artifact-store-meta workspace "landing")))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses a view link's deletion"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (should (equal "This artifact link is view-only"
                           (alist-get 'error (mevedel-artifact-room-test--request
                                              consumer 1 '((action . "delete-file")
                                                           (file . "assets/logo.png")
                                                           (hash . "0"))))))
            (should (file-exists-p (file-name-concat directory "assets/logo.png"))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--html-write-files
  ()
  ,test
  (test)
  :doc "changes several files at once when every hash still matches"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (pcase-let ((`(,expected . ,actual)
                       (mevedel-artifact-room-test--exchange
                        consumer "write-files" '(("$ID" . "landing")))))
            (should (equal (mapcar #'mevedel-artifact-room-test--normal expected)
                           (mapcar #'mevedel-artifact-room-test--normal actual)))
            (should (equal "<!doctype html>\n<h1>Hello, world</h1>\n"
                           (mevedel-artifact-room-test--file directory "index.html")))
            (should (equal "h1 { color: teal; }\n"
                           (mevedel-artifact-room-test--file directory "styles/site.css")))
            (should-not (file-exists-p (file-name-concat directory "assets/logo.png"))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "changes nothing when one hash is stale, and names that file"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (reply (mevedel-artifact-room-test--request
                   consumer 1 `((action . "write-files")
                                (changes . [((file . "index.html")
                                             (hash . ,mevedel-artifact-room-test--page-hash)
                                             (text . "<p>mine</p>"))
                                            ((file . "new.css") (text . "p {}"))
                                            ((file . "assets/logo.png")
                                             (hash . ,mevedel-artifact-room-test--page-hash)
                                             (delete . t))])))))
      (unwind-protect
          (progn
            (should (equal "stale" (alist-get 'code reply)))
            (should (equal "assets/logo.png" (alist-get 'file reply)))
            (should (equal mevedel-artifact-room-test--page
                           (mevedel-artifact-room-test--file directory "index.html")))
            (should-not (file-exists-p (file-name-concat directory "new.css")))
            (should (file-exists-p (file-name-concat directory "assets/logo.png"))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses the primary file's deletion and a view link, changing nothing"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (editor (mevedel-artifact-room-test--joined state port links 'edit))
           (viewer (mevedel-artifact-room-test--joined state port links 'view))
           (changes `[((file . "new.css") (text . "p {}"))
                      ((file . "index.html")
                       (hash . ,mevedel-artifact-room-test--page-hash) (delete . t))]))
      (unwind-protect
          (progn
            (should (string-search "primary file"
                                   (alist-get 'error (mevedel-artifact-room-test--request
                                                      editor 1 `((action . "write-files")
                                                                 (changes . ,changes))))))
            (should (equal "This artifact link is view-only"
                           (alist-get 'error (mevedel-artifact-room-test--request
                                              viewer 1 `((action . "write-files")
                                                         (changes . ,changes))))))
            (should-not (file-exists-p (file-name-concat directory "new.css")))
            (should (file-exists-p (file-name-concat directory "index.html"))))
        (mevedel-artifact-room-test--close editor)
        (mevedel-artifact-room-test--close viewer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--html-write-files-rollback
  ()
  ,test
  (test)
  :doc "rolls back a batch when a later file's directory is unwritable"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (assets (file-name-concat directory "assets"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (changed nil)
           (mevedel-artifact-store-changed-functions
            (cons (lambda (_workspace) (setq changed t))
                  mevedel-artifact-store-changed-functions))
           (versions (mevedel-artifact-store-versions workspace "landing")))
      (unwind-protect
          (progn
            (set-file-modes assets #o555)
            (should-not (file-writable-p assets))
            (let ((reply (mevedel-artifact-room-test--request
                          consumer 1 `((action . "write-files")
                                       (changes . [((file . "index.html")
                                                    (hash . ,mevedel-artifact-room-test--page-hash)
                                                    (text . "<p>must roll back</p>"))
                                                   ((file . "assets/new.css")
                                                    (text . "p {}"))])))))
              (should (string-search "all changes rolled back" (alist-get 'error reply)))
              (should-not (assq 'result reply)))
            (should (equal mevedel-artifact-room-test--page
                           (mevedel-artifact-room-test--file directory "index.html")))
            (should-not (file-exists-p (file-name-concat assets "new.css")))
            (should-not changed)
            (accept-process-output nil 0.1)
            (should-not (cl-find "event" (mevedel-artifact-room-test--frames consumer)
                                 :key (lambda (frame) (alist-get 'reqId frame))
                                 :test #'equal))
            (mevedel-artifact-room-test--close consumer)
            (should (equal versions (mevedel-artifact-store-versions workspace "landing"))))
        (set-file-modes assets #o755)
        (mevedel-artifact-room-test--close consumer))))

  :doc "also restores deleted bytes and modes, and removes created directories"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (mevedel-artifact-room-test--site workspace "landing"))
           (blocked (file-name-concat directory "blocked"))
           (logo (file-name-concat directory "assets/logo.png"))
           (bytes (mevedel-artifact-room-test--file directory "assets/logo.png"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            (make-directory blocked)
            (set-file-modes blocked #o555)
            (set-file-modes logo #o640)
            (let ((reply (mevedel-artifact-room-test--request
                          consumer 1 `((action . "write-files")
                                       (changes . [((file . "new/nested/site.css") (text . "p {}"))
                                                   ((file . "assets/logo.png")
                                                    (hash . ,(secure-hash 'sha256 bytes))
                                                    (delete . t))
                                                   ((file . "blocked/new.css") (text . "p {}"))])))))
              (should (string-search "all changes rolled back" (alist-get 'error reply))))
            (should (equal bytes (mevedel-artifact-room-test--file directory "assets/logo.png")))
            (should (= #o640 (file-modes logo)))
            (should-not (file-exists-p (file-name-concat directory "new"))))
        (set-file-modes blocked #o755)
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--html-versions
  ()
  ,test
  (test)
  :doc "records one version for a stretch that wrote the primary file, none for assets alone"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((mevedel-collaboration-artifact-room-version-idle 0.3)
           (_ (mevedel-artifact-room-test--site workspace "landing"))
           (links (mevedel-collaboration-artifact-room-share root "landing"))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            (should (assq 'result (mevedel-artifact-room-test--request
                                   consumer 1 '((action . "write-file")
                                                (file . "assets/extra.css")
                                                (text . "p {}")))))
            (accept-process-output nil 0.8)
            (should-not (mevedel-artifact-store-versions workspace "landing"))
            (should (assq 'result (mevedel-artifact-room-test--request
                                   consumer 2 `((action . "write-file") (file . "index.html")
                                                (hash . ,mevedel-artifact-room-test--page-hash)
                                                (text . "<p>v2</p>")))))
            (should (mevedel-relay-test--pump
                     (lambda () (mevedel-artifact-store-versions workspace "landing"))
                     5))
            (should (= 1 (length (mevedel-artifact-store-versions workspace "landing"))))
            (should (equal "<p>v2</p>"
                           (mevedel-artifact-room-test--file
                            (file-name-directory
                             (mevedel-artifact-store-version-path workspace "landing" 1))
                            (file-name-nondirectory
                             (mevedel-artifact-store-version-path workspace "landing" 1))))))
        (mevedel-artifact-room-test--close consumer)))))


;;
;;; Revoking, rotating, deleting

(defun mevedel-artifact-room-test--goodbye (consumer reason)
  "Check that CONSUMER was told goodbye for REASON, as the fixture shows."
  (mevedel-artifact-room-test--await consumer 1)
  (should (equal (mapcar #'mevedel-artifact-room-test--normal
                         (mapcar #'cdr (mevedel-artifact-room-test--fixture
                                        "goodbye" `(("$REASON" . ,reason)))))
                 (mapcar #'mevedel-artifact-room-test--normal
                         (mevedel-artifact-room-test--frames consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room-revoke
  ()
  ,test
  (test)
  :doc "disconnects connected consumers with revoked and ends the links for good"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (should-not (mevedel-collaboration-artifact-room-revoke root id))
            (mevedel-artifact-room-test--goodbye consumer "revoked")
            (should-not (mevedel-artifact-store-shared-p workspace id))
            ;; Sharing again mints new links; the old ones stay dead.
            (let ((again (mevedel-collaboration-artifact-room-share root id)))
              (should-not (equal (plist-get links :link-view) (plist-get again :link-view)))
              (should-not (equal (plist-get links :link-edit) (plist-get again :link-edit)))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "leaves an artifact that is not shared unshared"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let ((id (mevedel-artifact-room-test--board workspace "Pipeline")))
      (should-not (mevedel-collaboration-artifact-room-revoke root id))
      (should-not (mevedel-artifact-store-shared-p workspace id)))))

(mevedel-deftest mevedel-collaboration-artifact-room-rotate
  ()
  ,test
  (test)
  :doc "replaces the links, telling connected consumers revoked"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'edit))
           (rotated (mevedel-collaboration-artifact-room-rotate root id)))
      (unwind-protect
          (progn
            (mevedel-artifact-room-test--goodbye consumer "revoked")
            (should-not (equal (car (mevedel-artifact-room-test--secret
                                     (plist-get links :link-view)))
                               (car (mevedel-artifact-room-test--secret
                                     (plist-get rotated :link-view)))))
            (should (mevedel-artifact-store-shared-p workspace id))
            ;; The new links work and are what sharing returns from now on.
            (should (equal rotated (mevedel-collaboration-artifact-room-share root id)))
            (let ((fresh (mevedel-artifact-room-test--joined state port rotated 'view)))
              (unwind-protect
                  (should (alist-get 'result
                                     (mevedel-artifact-room-test--request
                                      fresh 1 `((action . "read") (id . ,id)))))
                (mevedel-artifact-room-test--close fresh))))
        (mevedel-artifact-room-test--close consumer))))

  :doc "refuses an artifact that does not exist"
  (mevedel-artifact-room-test--with-origin (root _workspace _state _port)
    (should-error (mevedel-collaboration-artifact-room-rotate root "missing")
                  :type 'user-error)))

(mevedel-deftest mevedel-collaboration-artifact-room-sharing
  ()
  ,test
  (test)
  :doc "is one host operation for sharing, revoking and rotating"
  (mevedel-artifact-room-test--with-origin (_root workspace _state _port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-sharing workspace id 'share)))
      (should (equal links (mevedel-collaboration-artifact-room-sharing workspace id 'share)))
      (let ((rotated (mevedel-collaboration-artifact-room-sharing workspace id 'rotate)))
        (should (plist-get rotated :link-edit))
        (should-not (equal links rotated)))
      (should-not (mevedel-collaboration-artifact-room-sharing workspace id 'revoke))
      (should-not (mevedel-artifact-store-shared-p workspace id))
      (should-error (mevedel-collaboration-artifact-room-sharing workspace id 'publish)))))

(mevedel-deftest mevedel-collaboration--store-sharing
  ()
  ,test
  (test)
  :doc "shares, rotates and revokes from the browser for owner links only"
  (mevedel-artifact-room-test--with-origin (_root workspace _state _port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (lobby (list :workspace workspace :transport 'lobby
                        :guests (make-hash-table :test #'eql)))
           (owner (list :name "owner" :writable t :owner t :ready t))
           (full (list :name "full" :writable t :ready t))
           (view (list :name "view" :writable nil :ready t))
           (changes 0)
           (mevedel-artifact-store-changed-functions
            (list (lambda (_workspace) (cl-incf changes)))))
      (cl-flet ((sharing (guest operation)
                  (mevedel-collaboration--store-action
                   lobby guest (list :action "sharing" :id id :operation operation)))
                (listed ()
                  (plist-get (cl-find id (mevedel-collaboration--store-rows lobby)
                                      :key (lambda (row) (plist-get row :id))
                                      :test #'equal)
                             :shared)))
        ;; Full and view links cannot share, rotate or revoke.
        (dolist (guest (list full view))
          (dolist (operation '("share" "rotate" "revoke"))
            (should (equal "Only the host or an owner link can share artifacts"
                           (cadr (should-error (sharing guest operation)))))))
        (should-not (mevedel-artifact-store-shared-p workspace id))
        (should (eq :json-false (listed)))
        (should (zerop changes))
        ;; Sharing returns the same links the Emacs commands show.
        (let ((links (sharing owner "share")))
          (should (eq t (plist-get links :shared)))
          (should (equal (mevedel-collaboration-artifact-room-sharing workspace id 'share)
                         (list :link-view (plist-get links :linkView)
                               :link-edit (plist-get links :linkEdit))))
          (should (eq t (listed)))
          (should (> changes 0))
          (should-error (sharing full "revoke"))
          (should (mevedel-artifact-store-shared-p workspace id))
          (let ((rotated (sharing owner "rotate")))
            (should (eq t (plist-get rotated :shared)))
            (should (stringp (plist-get rotated :linkEdit)))
            (should-not (equal (plist-get links :linkEdit) (plist-get rotated :linkEdit)))))
        (setq changes 0)
        (should (equal (list :id id :shared :json-false) (sharing owner "revoke")))
        (should-not (mevedel-artifact-store-shared-p workspace id))
        (should (eq :json-false (listed)))
        (should (> changes 0))
        (should (equal "Unknown sharing operation"
                       (cadr (should-error (sharing owner "publish")))))))))

(mevedel-deftest mevedel-collaboration-artifact-room--deleted
  ()
  ,test
  (test)
  :doc "deleting a shared artifact disconnects its consumers with artifact-deleted"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'view))
           (deleted 'pending))
      (unwind-protect
          (progn
            (mevedel-artifact-store-delete workspace id "Host"
                                           (lambda (failure) (setq deleted failure)))
            (mevedel-test--await 10 "deletion" (not (eq deleted 'pending)))
            (should-not deleted)
            (mevedel-artifact-room-test--goodbye consumer "artifact-deleted")
            (should-not (mevedel-collaboration-artifact-room--find root id))
            (should-not (mevedel-artifact-store-meta workspace id)))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room--duplicate
  ()
  ,test
  (test)
  :doc "a duplicate of a shared artifact starts unshared"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id)))
      (mevedel-artifact-store-duplicate workspace id "pipeline-copy")
      (should (mevedel-artifact-store-meta workspace "pipeline-copy"))
      (should-not (mevedel-artifact-store-shared-p workspace "pipeline-copy"))
      (should-not (mevedel-collaboration-artifact-room--find root "pipeline-copy"))
      (should (equal links (mevedel-collaboration-artifact-room-share root id))))))


;;
;;; Origin restarts

(mevedel-deftest mevedel-collaboration-artifact-room--stop-for-emacs
  ()
  ,test
  (test)
  :doc "an exiting origin tells consumers origin-stopped and stays shared"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((kill-emacs-hook nil)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (should (memq #'mevedel-collaboration-artifact-room--stop-for-emacs
                          kill-emacs-hook))
            (run-hooks 'kill-emacs-hook)
            (mevedel-artifact-room-test--goodbye consumer "origin-stopped")
            (should-not (mevedel-collaboration-artifact-room--find root id))
            (should (mevedel-artifact-store-shared-p workspace id))
            (should (equal (list root) (mevedel-collaboration-artifact-room--recorded))))
        (mevedel-artifact-room-test--close consumer)))))

(mevedel-deftest mevedel-collaboration-artifact-room-restore
  ()
  ,test
  (test)
  :doc "redials each recorded root's shared artifacts under their stored room ids"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (let* ((kill-emacs-hook nil)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id)))
      (mevedel-artifact-room-test--host-joined state)
      (run-hooks 'kill-emacs-hook)
      (should (mevedel-relay-test--pump (lambda () (not (plist-get (car state) :host)))))
      ;; The per-user record is a private data file.
      (let ((record (file-name-concat mevedel-user-dir "artifact-rooms.eld")))
        (should (file-exists-p record))
        (should (= #o600 (file-modes record))))
      ;; The next Emacs knows nothing but the record and the bookkeeping.
      (clrhash mevedel-collaboration-artifact-room--rooms)
      (mevedel-collaboration-artifact-room-restore)
      (let ((room (mevedel-collaboration-artifact-room--find root id)))
        (should room)
        (should (equal (car (mevedel-artifact-room-test--secret (plist-get links :link-view)))
                       (plist-get room :room-id))))
      ;; The links handed out before the restart work again.
      (let ((consumer (mevedel-artifact-room-test--joined state port links 'edit)))
        (unwind-protect
            (should (alist-get 'result
                               (mevedel-artifact-room-test--request
                                consumer 1 `((action . "read") (id . ,id)))))
          (mevedel-artifact-room-test--close consumer)))))

  :doc "drops roots that have no shared artifacts left or no longer exist"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((kill-emacs-hook nil)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (gone (file-name-as-directory (file-name-concat root "gone"))))
      (mevedel-collaboration-artifact-room-share root id)
      (should (equal (list root) (mevedel-collaboration-artifact-room--recorded)))
      ;; Revoking the last shared artifact drops the root at once.
      (mevedel-collaboration-artifact-room-revoke root id)
      (should-not (mevedel-collaboration-artifact-room--recorded))
      ;; A record that outlived its shares or its directory is cleaned up.
      (mevedel-collaboration-artifact-room--set-recorded root t)
      (mevedel-collaboration-artifact-room--set-recorded gone t)
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (mevedel-collaboration-artifact-room-restore))
      (should-not (mevedel-collaboration-artifact-room--recorded))
      (should (zerop (hash-table-count mevedel-collaboration-artifact-room--rooms)))))

  :doc "keeps a root recorded when its rooms cannot start yet"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((kill-emacs-hook nil)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           warnings)
      (mevedel-collaboration-artifact-room-share root id)
      (run-hooks 'kill-emacs-hook)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-open)
                 (lambda (&rest _) (error "Relay unavailable")))
                ((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        (mevedel-collaboration-artifact-room-restore))
      (should warnings)
      (should (equal (list root) (mevedel-collaboration-artifact-room--recorded)))
      (should (mevedel-artifact-store-shared-p workspace id))))

  :doc "deleting the last shared artifact drops its root"
  (mevedel-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((kill-emacs-hook nil)
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (deleted 'pending))
      (mevedel-collaboration-artifact-room-share root id)
      (mevedel-artifact-store-delete workspace id "Host"
                                     (lambda (failure) (setq deleted failure)))
      (mevedel-test--await 10 "deletion" (not (eq deleted 'pending)))
      (should-not (mevedel-collaboration-artifact-room--recorded)))))


;;
;;; Independence from the lobby

(mevedel-deftest mevedel-collaboration-artifact-room--lobby
  ()
  ,test
  (test)
  :doc "starting, rotating and stopping the lobby leaves artifact rooms alone"
  (mevedel-artifact-room-test--with-origin (root workspace state port)
    (require 'mevedel-collaboration-lobby)
    (let* ((kill-emacs-hook nil)
           (mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (id (mevedel-artifact-room-test--board workspace "Pipeline"))
           (links (mevedel-collaboration-artifact-room-share root id))
           (consumer (mevedel-artifact-room-test--joined state port links 'view))
           (room (mevedel-collaboration-artifact-room--find root id))
           (open (symbol-function 'mevedel-collaboration--transport-open))
           (stop (symbol-function 'mevedel-collaboration--transport-stop))
           (default-directory root))
      (unwind-protect
          ;; The stub relay hosts one room, the artifact's; the lobby
          ;; gets a transport of its own that goes nowhere.
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-open)
                     (lambda (url &rest args)
                       (if (string-search (plist-get room :room-id) url)
                           (apply open url args)
                         (list :fake-lobby t))))
                    ((symbol-function 'mevedel-collaboration--transport-stop)
                     (lambda (transport)
                       (unless (plist-get transport :fake-lobby)
                         (funcall stop transport))))
                    ((symbol-function 'mevedel-collaboration-share-present) #'ignore)
                    ((symbol-function 'yes-or-no-p) #'always))
            (mevedel-test--with-captured-messages nil
              (mevedel-collaboration-lobby)
              (should (mevedel-collaboration-lobby--find workspace))
              (mevedel-collaboration-lobby-rotate)
              (should (mevedel-collaboration-lobby--find workspace))
              (mevedel-collaboration-lobby-stop))
            (should-not (mevedel-collaboration-lobby--find workspace))
            (should (eq room (mevedel-collaboration-artifact-room--find root id)))
            (should (equal links (mevedel-collaboration-artifact-room-share root id)))
            (should (alist-get 'result
                               (mevedel-artifact-room-test--request
                                consumer 1 `((action . "read") (id . ,id)))))
            (should-not (cl-some (lambda (frame) (equal "bye" (alist-get 't frame)))
                                 (mevedel-artifact-room-test--frames consumer))))
        (mevedel-artifact-room-test--close consumer)))))

(provide 'test-mevedel-collaboration-artifact-room)
;;; test-mevedel-collaboration-artifact-room.el ends here
