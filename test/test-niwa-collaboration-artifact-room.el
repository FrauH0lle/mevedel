;;; test-niwa-collaboration-artifact-room.el --- Artifact room tests -*- lexical-binding: t; -*-

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
(require 'niwa-collaboration-relay-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-collaboration-relay-test-support"))
(require 'cl-lib)
(require 'json)
(require 'websocket)
(require 'niwa-artifact-lease)
(require 'niwa-artifact-store)
(require 'niwa-collaboration)
(require 'niwa-collaboration-artifact)
(require 'niwa-collaboration-artifact-room)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-share)
(require 'niwa-collaboration-transport)
(require 'niwa-shared-editing)
(require 'niwa-shared-library)
(require 'niwa-structs)
(require 'niwa-workspace)

(defconst niwa-artifact-room-test--fixtures
  (file-name-concat
   (file-name-directory
    (or buffer-file-name load-file-name byte-compile-current-file))
   "fixtures" "remote-artifact")
  "Directory of the frame fixtures shared with the remote-artifact package.")


;;
;;; Origin fixture

(defun niwa-artifact-room-test--sockets-closed (port)
  "Wait until no client WebSocket to the stub relay at PORT is live."
  (niwa-relay-test--pump
   (lambda ()
     (not (cl-some (lambda (process)
                     (and (process-live-p process)
                          (string-prefix-p
                           (format "websocket to ws://127.0.0.1:%d/" port)
                           (process-name process))))
                   (process-list))))
   5))

(defmacro niwa-artifact-room-test--with-origin (bindings &rest body)
  "Run BODY with an isolated origin bound per BINDINGS.
BINDINGS is (ROOT WORKSPACE STATE PORT).  ROOT is a fresh workspace
directory and WORKSPACE its workspace.  STATE is the stub relay's room
state and PORT its port; the relay URL points at it.  Artifact rooms,
editing runtimes, leases and the workspace registry are private to BODY,
and every room it started is stopped afterwards."
  (declare (indent 1) (debug t))
  (pcase-let ((`(,root ,workspace ,state ,port) bindings))
    `(niwa-relay-test--with-stub-relay (,state ,port _server)
       (let* ((,root (file-name-as-directory
                      (make-temp-file "niwa-artifact-room-" t)))
              (niwa-user-dir (file-name-as-directory
                                 (file-name-concat ,root "user")))
              (niwa-workspace--registry (make-hash-table :test #'equal))
              (niwa-collaboration-artifact-room--rooms
               (make-hash-table :test #'equal))
              (niwa-shared-editing--runtimes (make-hash-table :test #'equal))
              (niwa-artifact-lease--held (make-hash-table :test #'equal))
              (niwa-session-durability--client-id (make-string 64 ?a))
              (niwa-collaboration-relay-url
               (format "ws://127.0.0.1:%d" ,port))
              (niwa-collaboration-relay-host-token nil)
              (,workspace (with-temp-buffer
                            (setq default-directory ,root)
                            (niwa-workspace))))
         (cl-letf (((symbol-function 'niwa-sleep-update) #'ignore))
           (unwind-protect (progn ,@body)
             (niwa-collaboration-artifact-room-stop-all 'test)
             (niwa-artifact-room-test--sockets-closed ,port)
             (niwa-shared-editing-stop)
             (maphash (lambda (_directory held)
                        (when (timerp (plist-get held :timer))
                          (cancel-timer (plist-get held :timer))))
                      niwa-artifact-lease--held)
             (delete-directory ,root t)))))))

(defun niwa-artifact-room-test--call (workspace args)
  "Run editing ARGS for WORKSPACE through its queue and return the reply."
  (let (reply)
    (niwa-shared-editing-call workspace args (lambda (value) (setq reply value)))
    (niwa-test--await 10 "editing reply" reply)
    reply))

(defun niwa-artifact-room-test--board (workspace title)
  "Create a whiteboard TITLE with one rectangle in WORKSPACE; return its id."
  (let (reply)
    (niwa-shared-editing-create workspace "whiteboard" title "Host"
                                   (lambda (value) (setq reply value)))
    (niwa-test--await 10 "creation settles" reply)
    (should-not (plist-get reply :error))
    (let ((id (plist-get reply :id)))
      (should-not
       (plist-get
        (niwa-artifact-room-test--call
         workspace
         (list :action "patch" :id id :actor "Host" :opId "fixture-box"
               :changes (vector (list :id "box"
                                      :after (list :type "rectangle" :x 0 :y 0
                                                   :width 120 :height 60)))))
        :error))
      id)))

(defun niwa-artifact-room-test--secret (link)
  "Return (ROOM-ID . SECRET-BYTES) from artifact LINK."
  (should (string-match "#\\([A-Za-z0-9_-]+\\)\\.\\([A-Za-z0-9_-]+\\)\\'" link))
  (cons (match-string 1 link)
        (niwa-collaboration--base64url-decode (match-string 2 link))))


;;
;;; Fixtures and a consumer

(defun niwa-artifact-room-test--fixture (name bindings &optional keep)
  "Return the frames of fixture NAME with BINDINGS substituted.
BINDINGS is an alist of placeholder strings to values; the fixture's
own examples stand for any placeholder BINDINGS leaves out.  Each frame
is (FROM . FRAME), FROM being \"consumer\" or \"origin\".  With KEEP,
placeholders BINDINGS leaves out stay as they are, for
`niwa-artifact-room-test--matches'."
  (let* ((fixture (json-parse-string
                   (with-temp-buffer
                     (insert-file-contents
                      (file-name-concat niwa-artifact-room-test--fixtures
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

(defun niwa-artifact-room-test--normal (value)
  "Return JSON VALUE with every object's keys in a stable order."
  (cond
   ((vectorp value) (vconcat (mapcar #'niwa-artifact-room-test--normal value)))
   ((and (consp value) (consp (car value)))
    (sort (mapcar (lambda (entry)
                    (cons (car entry) (niwa-artifact-room-test--normal (cdr entry))))
                  value)
          (lambda (a b) (string< (car a) (car b)))))
   (t value)))

(defun niwa-artifact-room-test--parse (text)
  "Parse frame JSON TEXT the way fixtures are parsed."
  (json-parse-string text :object-type 'alist :array-type 'array
                     :false-object :false :null-object :null))

(defun niwa-artifact-room-test--consumer (port link)
  "Connect a consumer of artifact LINK through the stub relay at PORT.
Return a plist holding its socket, room key and received frames."
  (pcase-let* ((`(,room-id . ,secret) (niwa-artifact-room-test--secret link))
               (consumer (list :key (substring secret 0 32) :secret secret
                               :texts nil :ws nil)))
    (plist-put
     consumer :ws
     (websocket-open
      (format "ws://127.0.0.1:%d/r/%s?role=guest" port room-id)
      :on-message
      (lambda (_ws frame)
        (when (eq 'binary (websocket-frame-opcode frame))
          (when-let* ((text (niwa-collaboration--unseal
                             (plist-get consumer :key)
                             (cdr (niwa-collaboration--envelope-unpack
                                   (websocket-frame-payload frame))))))
            ;; The connection's challenge binds every frame the consumer
            ;; sends; it is not part of any exchange.
            (let ((parsed (niwa-artifact-room-test--parse text)))
              (cond
               ((equal "challenge" (alist-get 't parsed))
                (plist-put consumer :challenge (alist-get 'challenge parsed)))
               ;; A frame for this consumer alone carries back its nonce,
               ;; which is not part of any exchange either.
               ((member (alist-get 'nonce parsed) '(nil "consumer-nonce"))
                (plist-put consumer :texts
                           (append (plist-get consumer :texts)
                                   (list (json-serialize
                                          (assq-delete-all 'nonce parsed)
                                          :false-object :false
                                          :null-object :null))))))))))))
    (should (niwa-relay-test--pump
             (lambda () (plist-get consumer :challenge))))
    consumer))

(defun niwa-artifact-room-test--write-token (consumer)
  "Return CONSUMER's write token as a hello carries it."
  (niwa-collaboration--base64url (substring (plist-get consumer :secret) 32 48)))

(defun niwa-artifact-room-test--send (consumer frame)
  "Send fixture FRAME from CONSUMER, sealed and bound to its connection.
An editing frame's body travels as the chunked transfer it stands for."
  (let ((wire
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
                   (push `((t . "editing") (reqId . ,(alist-get 'reqId frame))
                           (offset . ,offset) (total . ,total)
                           (data . ,(substring data offset end)))
                         chunks)
                   (setq offset end)))
               (nreverse chunks))
           (list frame))))
    (dolist (part wire)
      (plist-put consumer :seq (1+ (or (plist-get consumer :seq) 0)))
      (websocket-send
       (plist-get consumer :ws)
       (make-websocket-frame
        :opcode 'binary :completep t
        :payload (niwa-collaboration--envelope-pack
                  0 (niwa-collaboration--seal
                     (plist-get consumer :key)
                     (json-serialize
                      (append part `((challenge . ,(plist-get consumer :challenge))
                                     (seq . ,(plist-get consumer :seq))
                                     (nonce . "consumer-nonce")))
                      :false-object :false :null-object :null))))))))

(defun niwa-artifact-room-test--frames (consumer)
  "Return the frames CONSUMER received, parsed like fixture frames.
An editing transfer appears once, complete, as its `body' shorthand."
  (let (frames transfers)
    (dolist (text (plist-get consumer :texts) (nreverse frames))
      (let* ((frame (niwa-artifact-room-test--parse text))
             (type (alist-get 't frame)))
        (if (not (member type '("editing" "editing-event")))
            (push frame frames)
          (let* ((req-id (or (alist-get 'reqId frame) type))
                 (parts (concat (cdr (assoc req-id transfers))
                                (alist-get 'data frame))))
            (setf (alist-get req-id transfers nil nil #'equal) parts)
            (when (= (length parts) (alist-get 'total frame))
              (setq transfers (assoc-delete-all req-id transfers))
              (push `((t . ,type) (reqId . ,(alist-get 'reqId frame))
                      (body . ,(niwa-artifact-room-test--parse
                                (decode-coding-string (base64-decode-string parts)
                                                      'utf-8))))
                    frames))))))))

(defun niwa-artifact-room-test--await (consumer count)
  "Wait until CONSUMER received COUNT frames; return them all."
  (niwa-relay-test--pump
   (lambda () (>= (length (niwa-artifact-room-test--frames consumer)) count)))
  (niwa-artifact-room-test--frames consumer))

(defun niwa-artifact-room-test--close (consumer)
  "Close CONSUMER's socket."
  (websocket-close (plist-get consumer :ws)))

(defun niwa-artifact-room-test--host-joined (state)
  "Wait until the origin's room has dialed the stub relay with STATE."
  (should (niwa-relay-test--pump (lambda () (plist-get (car state) :host)))))


;;
;;; Sharing

(niwa-deftest niwa-collaboration-artifact-room--choose
  (:doc "filters real catalog sharing observations without scalar probes")
  (let* ((root (make-temp-file "niwa-artifact-choose-" t))
         (workspace (niwa-workspace--create :type 'file :id root :root root))
         (niwa-artifact-store-changed-functions nil)
         (expected '("shared")))
    (unwind-protect
        (progn
          (dolist (id '("shared" "private"))
            (let ((directory (niwa-artifact-store-artifact-directory workspace id)))
              (make-directory directory t)
              (write-region "page" nil (file-name-concat directory "index.html") nil 'silent)
              (niwa-artifact-store-create-meta workspace id "index.html")))
          (let ((link (niwa-artifact-store-link-path workspace "shared")))
            (write-region "private" nil link nil 'silent)
            (cl-letf (((symbol-function 'niwa-workspace-current) (lambda () workspace))
                      ((symbol-function 'niwa-artifact-store-shared-p)
                       (lambda (&rest _) (ert-fail "Unexpected scalar sharing probe")))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (should (equal expected (sort (mapcar #'cdr choices) #'string<)))
                         (caar choices))))
              (should (equal "shared" (niwa-collaboration-artifact-room--choose t)))
              (setq expected '("private" "shared"))
              (should (member (niwa-collaboration-artifact-room--choose) expected))
              (delete-file link)
              (should-error (niwa-collaboration-artifact-room--choose t)
                            :type 'user-error))))
      (delete-directory root t))))

(niwa-deftest niwa-collaboration-artifact-room-share
  ()
  ,test
  (test)
  :doc "returns persistent view and edit links that carry no artifact id"
  (niwa-artifact-room-test--with-origin (root workspace _state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (view (plist-get links :link-view))
           (edit (plist-get links :link-edit))
           (credentials (file-name-concat
                         (niwa-artifact-store-bookkeeping-directory workspace id)
                         "link")))
      ;; WEB-ORIGIN/#ROOM-ID.SECRET, 32 bytes to view and 48 to edit.
      (should (string-prefix-p (format "http://127.0.0.1:%d/#" port) view))
      (should (string-prefix-p (format "http://127.0.0.1:%d/#" port) edit))
      (should (= 32 (length (cdr (niwa-artifact-room-test--secret view)))))
      (should (= 48 (length (cdr (niwa-artifact-room-test--secret edit)))))
      (should (equal (car (niwa-artifact-room-test--secret view))
                     (car (niwa-artifact-room-test--secret edit))))
      (should (string-prefix-p (cdr (niwa-artifact-room-test--secret view))
                               (cdr (niwa-artifact-room-test--secret edit))))
      (should-not (string-search id view))
      (should-not (string-search id edit))
      ;; Credentials live privately in the artifact's bookkeeping.
      (should (file-exists-p credentials))
      (should (= #o600 (file-modes credentials)))
      ;; Sharing again returns the same links, also once the room stopped.
      (should (equal links (niwa-collaboration-artifact-room-share root id)))
      (niwa-collaboration-artifact-room-stop-all 'test)
      (should (equal links (niwa-collaboration-artifact-room-share root id)))))

  :doc "refuses an artifact that does not exist"
  (niwa-artifact-room-test--with-origin (root _workspace _state _port)
    (should-error (niwa-collaboration-artifact-room-share root "missing")
                  :type 'user-error)))

(niwa-deftest niwa-collaboration-share-artifact
  ()
  ,test
  (test)
  :doc "shares the chosen artifact, copies its edit link and shows both links"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (kill-ring nil)
           (default-directory root)
           candidates shown)
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (setq candidates (all-completions "" collection))
                       (car candidates)))
                    ((symbol-function 'yes-or-no-p) #'always))
            (niwa-test--with-captured-messages nil
              (call-interactively #'niwa-collaboration-share-artifact))
            (let ((links (niwa-collaboration-artifact-room-share root id)))
              (should (equal 1 (length candidates)))
              (should (string-match-p "Pipeline" (car candidates)))
              (should (equal (plist-get links :link-edit) (car kill-ring)))
              (setq shown (with-current-buffer
                              niwa-collaboration-artifact-room--links-buffer
                            (buffer-string)))
              (should (string-search (plist-get links :link-view) shown))
              (should (string-search (plist-get links :link-edit) shown))))
        (when-let* ((buffer (get-buffer niwa-collaboration-artifact-room--links-buffer)))
          (dolist (window (get-buffer-window-list buffer nil t))
            (quit-window t window))
          (when (buffer-live-p buffer) (kill-buffer buffer))))))

  :doc "shares nothing when the host declines"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let ((id (niwa-artifact-room-test--board workspace "Pipeline"))
          (default-directory root))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (car (all-completions "" collection))))
                ((symbol-function 'yes-or-no-p) #'ignore))
        (should-error (call-interactively #'niwa-collaboration-share-artifact)
                      :type 'user-error))
      (should-not (file-exists-p
                   (file-name-concat
                    (niwa-artifact-store-bookkeeping-directory workspace id)
                    "link"))))))

(defun niwa-artifact-room-test--kill-links-buffer ()
  "Close the artifact links buffer and its windows."
  (when-let* ((buffer (get-buffer niwa-collaboration-artifact-room--links-buffer)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (quit-window t window))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(niwa-deftest niwa-collaboration-revoke-artifact
  ()
  ,test
  (test)
  :doc "offers only shared artifacts and revokes the chosen one once confirmed"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (_other (niwa-artifact-room-test--board workspace "Private"))
           (default-directory root)
           candidates confirm)
      (niwa-collaboration-artifact-room-share root id)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq candidates (all-completions "" collection))
                   (car candidates)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) confirm)))
        (should-error (call-interactively #'niwa-collaboration-revoke-artifact)
                      :type 'user-error)
        (should (niwa-artifact-store-shared-p workspace id))
        (setq confirm t)
        (niwa-test--with-captured-messages nil
          (call-interactively #'niwa-collaboration-revoke-artifact)))
      (should (equal 1 (length candidates)))
      (should (string-match-p "Pipeline" (car candidates)))
      (should-not (niwa-artifact-store-shared-p workspace id))
      ;; Nothing is left to revoke.
      (should-error (call-interactively #'niwa-collaboration-revoke-artifact)
                    :type 'user-error))))

(niwa-deftest niwa-collaboration-rotate-artifact
  ()
  ,test
  (test)
  :doc "replaces the chosen artifact's links and shows the new ones"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (old (niwa-collaboration-artifact-room-share root id))
           (kill-ring nil)
           (default-directory root))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (car (all-completions "" collection))))
                    ((symbol-function 'yes-or-no-p) #'always))
            (niwa-test--with-captured-messages nil
              (call-interactively #'niwa-collaboration-rotate-artifact))
            (let ((links (niwa-collaboration-artifact-room-share root id))
                  (shown (with-current-buffer
                             niwa-collaboration-artifact-room--links-buffer
                           (buffer-string))))
              (should-not (equal old links))
              (should (equal (plist-get links :link-edit) (car kill-ring)))
              (should (string-search (plist-get links :link-view) shown))
              (should-not (string-search (plist-get old :link-view) shown))))
        (niwa-artifact-room-test--kill-links-buffer)))))


;;
;;; Joining

(niwa-deftest niwa-collaboration-artifact-room-origin-privacy
  ()
  ,test
  (test)
  :doc "default welcome omits origin rather than disclosing workspace or host"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((niwa-collaboration-artifact-room-origin-name nil)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (_ (niwa-artifact-room-test--host-joined state))
           (consumer (niwa-artifact-room-test--consumer
                      port (plist-get links :link-view))))
      (unwind-protect
          (progn
            (niwa-artifact-room-test--send
             consumer `((t . "hello") (proto . ,niwa-collaboration--protocol-version) (name . "viewer")
                        (guestId . "privacy-consumer")))
            (should
             (equal (mapcar #'cdr (niwa-artifact-room-test--fixture
                                  "welcome-private" `(("$ID" . ,id))))
                    (niwa-artifact-room-test--await consumer 1))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--on-frame
  ()
  ,test
  (test)
  :doc "answers an edit link's hello with the artifact welcome only"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((niwa-collaboration-artifact-room-origin-name "Shared studio")
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (_ (niwa-artifact-room-test--host-joined state))
           (consumer (niwa-artifact-room-test--consumer
                      port (plist-get links :link-edit)))
           (bindings `(("$ID" . ,id) ("$TITLE" . "Pipeline")
                       ("$ORIGIN" . "Shared studio")
                       ("$WRITE_TOKEN"
                        . ,(niwa-artifact-room-test--write-token consumer)))))
      (unwind-protect
          (progn
            (dolist (frame (niwa-artifact-room-test--fixture "hello" bindings))
              (niwa-artifact-room-test--send consumer (cdr frame)))
            (niwa-artifact-room-test--await consumer 1)
            ;; Anything more would arrive with the welcome.
            (accept-process-output nil 0.2)
            (should (equal (mapcar #'niwa-artifact-room-test--normal
                                   (mapcar #'cdr (niwa-artifact-room-test--fixture
                                                  "welcome" bindings)))
                           (mapcar #'niwa-artifact-room-test--normal
                                   (niwa-artifact-room-test--frames consumer)))))
        (niwa-artifact-room-test--close consumer))))

  :doc "welcomes a view link read-only and an edit link writable"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (_ (niwa-artifact-room-test--host-joined state))
           (view (niwa-artifact-room-test--consumer
                  port (plist-get links :link-view)))
           (edit (niwa-artifact-room-test--consumer
                  port (plist-get links :link-edit))))
      (unwind-protect
          (progn
            (niwa-artifact-room-test--send
             view `((t . "hello") (proto . ,niwa-collaboration--protocol-version) (name . "viewer")
                    (guestId . "consumer-view")))
            (niwa-artifact-room-test--send
             edit `((t . "hello") (proto . ,niwa-collaboration--protocol-version) (name . "owner?")
                    (guestId . "consumer-edit")
                    (writeToken . ,(niwa-artifact-room-test--write-token edit))))
            (should (eq t (alist-get 'readOnly
                                     (car (niwa-artifact-room-test--await view 1)))))
            (should (eq :false (alist-get 'readOnly
                                          (car (niwa-artifact-room-test--await edit 1))))))
        (niwa-artifact-room-test--close view)
        (niwa-artifact-room-test--close edit)))))


;;
;;; Reading

(defun niwa-artifact-room-test--joined (state port links tier)
  "Return a consumer admitted to the room of LINKS at TIER (`view' or `edit').
STATE and PORT are the stub relay's."
  (niwa-artifact-room-test--host-joined state)
  (let ((consumer (niwa-artifact-room-test--consumer
                   port (plist-get links (if (eq tier 'edit) :link-edit :link-view)))))
    (niwa-artifact-room-test--send
     consumer `((t . "hello") (proto . ,niwa-collaboration--protocol-version) (name . "remote agent")
                (guestId . "consumer-0001")
                ,@(when (eq tier 'edit)
                    `((writeToken . ,(niwa-artifact-room-test--write-token
                                      consumer))))))
    (niwa-artifact-room-test--await consumer 1)
    (plist-put consumer :texts nil)
    consumer))

(defun niwa-artifact-room-test--request (consumer req-id body &optional timeout)
  "Send editing BODY as request REQ-ID from CONSUMER; return the reply body.
Wait at most TIMEOUT seconds, 5 by default, for the reply's last chunk;
only the newest frame is decoded while waiting, so a large reply costs
no more than its size."
  (niwa-artifact-room-test--send
   consumer `((t . "editing") (reqId . ,req-id) (body . ,body)))
  (niwa-relay-test--pump
   (lambda ()
     (when-let* ((text (car (last (plist-get consumer :texts))))
                 (frame (niwa-artifact-room-test--parse text)))
       (and (equal req-id (alist-get 'reqId frame))
            (= (+ (alist-get 'offset frame) (length (alist-get 'data frame)))
               (alist-get 'total frame)))))
   timeout)
  (alist-get 'body (cl-find req-id (niwa-artifact-room-test--frames consumer)
                            :key (lambda (frame) (alist-get 'reqId frame)))))

(defun niwa-artifact-room-test--matches (expected actual)
  "Return non-nil when frame ACTUAL has fixture frame EXPECTED's shape.
A placeholder string in EXPECTED matches any string; everything else,
object keys included, must be equal."
  (cond
   ((and (stringp expected) (string-prefix-p "$" expected)) (stringp actual))
   ((vectorp expected)
    (and (vectorp actual) (= (length expected) (length actual))
         (cl-every #'niwa-artifact-room-test--matches expected actual)))
   ((and (consp expected) (consp (car expected)))
    (and (consp actual) (= (length expected) (length actual))
         (cl-every (lambda (entry)
                     (and (assq (car entry) actual)
                          (niwa-artifact-room-test--matches
                           (cdr entry) (alist-get (car entry) actual))))
                   expected)))
   (t (equal expected actual))))

(defun niwa-artifact-room-test--replies (consumer)
  "Return the request replies CONSUMER received, without change events."
  (cl-remove "editing-event" (niwa-artifact-room-test--frames consumer)
             :key (lambda (frame) (alist-get 't frame)) :test #'equal))

(defun niwa-artifact-room-test--exchange (consumer name bindings)
  "Play fixture NAME's consumer frames from CONSUMER with BINDINGS.
Return (EXPECTED . ACTUAL): the fixture's origin frames, placeholders
left unbound, and the replies the origin sent."
  (let* ((frames (niwa-artifact-room-test--fixture name bindings t))
         (expected (mapcar #'cdr (cl-remove "consumer" frames :key #'car :test #'equal))))
    (dolist (frame frames)
      (when (equal "consumer" (car frame))
        (niwa-artifact-room-test--send consumer (cdr frame))))
    (niwa-relay-test--pump
     (lambda () (>= (length (niwa-artifact-room-test--replies consumer))
                    (length expected))))
    (cons expected (niwa-artifact-room-test--replies consumer))))

(defun niwa-artifact-room-test--view (workspace id &optional part)
  "Return the text of WORKSPACE's item ID as the origin shows PART of it.
PART defaults to \"overview\", the lines of `shared://ID'."
  (plist-get (plist-get (niwa-artifact-room-test--call
                         workspace (list :action "view" :id id
                                         :part (or part "overview")))
                        :result)
             :text))

(niwa-deftest niwa-collaboration-artifact-room--read
  ()
  ,test
  (test)
  :doc "returns the board's lines exactly as shared://ID shows them at the origin"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (text (plist-get
                  (plist-get (niwa-artifact-room-test--call
                              workspace (list :action "view" :id id :part "overview"))
                             :result)
                  :text))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'view))
           (frames (niwa-artifact-room-test--fixture
                    "read" `(("$ID" . ,id) ("$TEXT" . ,text)))))
      (unwind-protect
          (progn
            (should (string-match-p "\"id\":\"box\"" text))
            (niwa-artifact-room-test--send consumer (cdr (assoc "consumer" frames)))
            (niwa-artifact-room-test--await consumer 1)
            (should (equal (list (niwa-artifact-room-test--normal
                                  (cdr (assoc "origin" frames))))
                           (mapcar #'niwa-artifact-room-test--normal
                                   (niwa-artifact-room-test--frames consumer)))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--read-history
  ()
  ,test
  (test)
  :doc "returns the recent contributions as shared://ID/history shows them, to any link"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (text (niwa-artifact-room-test--view workspace id "history"))
           (links (niwa-collaboration-artifact-room-share root id))
           (viewer (niwa-artifact-room-test--joined state port links 'view))
           (editor (niwa-artifact-room-test--joined state port links 'edit))
           (frames (niwa-artifact-room-test--fixture
                    "history" `(("$ID" . ,id) ("$TEXT" . ,text)))))
      (unwind-protect
          (progn
            (should (string-search "fixture-box · revision 2 · Host" text))
            (dolist (consumer (list viewer editor))
              (niwa-artifact-room-test--send consumer (cdr (assoc "consumer" frames)))
              (niwa-artifact-room-test--await consumer 1)
              (should (equal (list (niwa-artifact-room-test--normal
                                    (cdr (assoc "origin" frames))))
                             (mapcar #'niwa-artifact-room-test--normal
                                     (niwa-artifact-room-test--frames consumer))))))
        (niwa-artifact-room-test--close viewer)
        (niwa-artifact-room-test--close editor)))))


;;
;;; Editing

(niwa-deftest niwa-collaboration-artifact-room--edit
  ()
  ,test
  (test)
  :doc "patches through an edit link, answering with the changed lines and picture"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           attached)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-artifact-store-attach)
                     (lambda (&rest args) (push args attached))))
            (pcase-let* ((`(,expected . ,actual)
                          (niwa-artifact-room-test--exchange
                           consumer "patch" `(("$ID" . ,id))))
                         (result (alist-get 'result (alist-get 'body (car actual)))))
              (should (= 1 (length actual)))
              (should (niwa-artifact-room-test--matches (car expected) (car actual)))
              ;; The changed element's stored line, and the board as drawn.
              (should (string-match-p "\\\\\"id\\\\\":\\\\\"next\\\\\"" (alist-get 'text result)))
              (should-not (string-match-p "\\\\\"id\\\\\":\\\\\"box\\\\\"" (alist-get 'text result)))
              (should (string-prefix-p "\x89PNG" (base64-decode-string (alist-get 'png result)))))
            ;; The origin holds the change, attributed to the remote agent by
            ;; its hello name, and attached the board to no session.
            (should (string-match-p "\"id\":\"next\"" (niwa-artifact-room-test--view workspace id)))
            (should (string-search "Agent: remote agent (remote)"
                                   (niwa-artifact-room-test--view workspace id "history")))
            (should-not attached))
        (niwa-artifact-room-test--close consumer))))

  :doc "rejects a stale patch whole, returning the targets' current lines"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (before (niwa-artifact-room-test--view workspace id))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (pcase-let* ((`(,expected . ,actual)
                        (niwa-artifact-room-test--exchange
                         consumer "stale-patch" `(("$ID" . ,id)
                                                  ("$STALE_HASH" . "000000000000"))))
                       (body (alist-get 'body (car actual))))
            (should (niwa-artifact-room-test--matches (car expected) (car actual)))
            ;; The current line is the one shared://ID shows for the box.
            (should (string-search
                     (concat "\n" (alist-get 'line (aref (alist-get 'targets body) 0)) "\n")
                     (concat "\n" before "\n")))
            (should (equal before (niwa-artifact-room-test--view workspace id))))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses a view link's edit as view-only and changes nothing"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (before (niwa-artifact-room-test--view workspace id))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (pcase-let ((`(,expected . ,actual)
                       (niwa-artifact-room-test--exchange
                        consumer "view-only" `(("$ID" . ,id)))))
            (should (equal (mapcar #'niwa-artifact-room-test--normal expected)
                           (mapcar #'niwa-artifact-room-test--normal actual)))
            (should (equal before (niwa-artifact-room-test--view workspace id))))
        (niwa-artifact-room-test--close consumer)))))

(defconst niwa-artifact-room-test--png
  (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQ"
          "DwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
  "A 1x1 PNG, base64.")

(defun niwa-artifact-room-test--contribution (reply)
  "Return the contribution id an edit REPLY body names."
  (let ((text (alist-get 'text (alist-get 'result reply))))
    (should (stringp text))
    (alist-get 'contribution (json-parse-string text :object-type 'alist))))

(defconst niwa-artifact-room-test--next
  '((action . "patch")
    (changes . [((id . "next")
                 (after . ((type . "rectangle") (x . 200) (y . 0)
                           (width . 120) (height . 60))))]))
  "A consumer's patch adding the rectangle \"next\".")

(niwa-deftest niwa-collaboration-artifact-room--revert
  ()
  ,test
  (test)
  :doc "reverts a listed contribution through an edit link, as SharedEdit does"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (let ((contribution (niwa-artifact-room-test--contribution
                               (niwa-artifact-room-test--request
                                consumer 7 `((id . ,id) ,@niwa-artifact-room-test--next)))))
            ;; The contribution read_artifact's history lists is the one reverted.
            (should (string-search contribution
                                   (niwa-artifact-room-test--view workspace id "history")))
            (plist-put consumer :texts nil)
            (pcase-let ((`(,expected . ,actual)
                         (niwa-artifact-room-test--exchange
                          consumer "revert" `(("$ID" . ,id)
                                              ("$TRANSACTION" . ,contribution)))))
              (should (= 1 (length actual)))
              (should (niwa-artifact-room-test--matches (car expected) (car actual))))
            (should-not (string-match-p "\"id\":\"next\""
                                        (niwa-artifact-room-test--view workspace id)))
            (should (string-match-p "\"id\":\"box\""
                                    (niwa-artifact-room-test--view workspace id))))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses a revert whose targets changed since, changing nothing"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (contribution (niwa-artifact-room-test--contribution
                          (niwa-artifact-room-test--request
                           consumer 1 `((id . ,id) ,@niwa-artifact-room-test--next)))))
      (unwind-protect
          (progn
            ;; The origin moves the consumer's rectangle afterwards.
            (let* ((lines (niwa-artifact-room-test--view workspace id))
                   (hash (and (string-match "^\\([0-9a-f]+\\) {\"id\":\"next\"" lines)
                              (match-string 1 lines))))
              (should hash)
              (should-not (plist-get (niwa-artifact-room-test--call
                                      workspace (list :action "patch" :id id :actor "Host"
                                                      :opId "move-next"
                                                      :changes (vector (list :id "next" :hash hash
                                                                             :set (list :x 400)))))
                                     :error)))
            (let ((before (niwa-artifact-room-test--view workspace id))
                  (reply (niwa-artifact-room-test--request
                          consumer 2 `((action . "revert") (id . ,id)
                                       (transaction . ,contribution)))))
              (should (stringp (alist-get 'error reply)))
              (should-not (assq 'result reply))
              (should (equal before (niwa-artifact-room-test--view workspace id)))))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses a view link's revert as view-only"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (viewer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (should (equal "This artifact link is view-only"
                         (alist-get 'error (niwa-artifact-room-test--request
                                            viewer 1 `((action . "revert") (id . ,id)
                                                       (transaction . "fixture-box"))))))
        (niwa-artifact-room-test--close viewer))))

  :doc "lets the origin revert a consumer's contribution, as its Contributions offers"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (contribution (niwa-artifact-room-test--contribution
                          (niwa-artifact-room-test--request
                           consumer 1 `((id . ,id) ,@niwa-artifact-room-test--next)))))
      (unwind-protect
          (progn
            ;; The browser offers Revert for contributions by an "Agent:" actor.
            (should (string-search (concat contribution " · revision 3 · Agent: ")
                                   (niwa-artifact-room-test--view workspace id "history")))
            ;; Its Revert button sends this request.
            (should-not (plist-get (niwa-artifact-room-test--call
                                    workspace (list :action "revert" :id id
                                                    :transaction contribution
                                                    :actor "Guest: Host" :opId "undo-remote"))
                                   :error))
            (should-not (string-match-p "\"id\":\"next\""
                                        (niwa-artifact-room-test--view workspace id))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--edit-actions
  ()
  ,test
  (test)
  :doc "renames and recolours the artifact with SharedEdit's arguments"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            (should (assq 'result (niwa-artifact-room-test--request
                                   consumer 1 `((action . "rename") (id . ,id)
                                                (title . "Pipeline v2")))))
            (should (assq 'result (niwa-artifact-room-test--request
                                   consumer 2 `((action . "background") (id . ,id)
                                                (background . "#ffeedd")))))
            (should (equal "Pipeline v2"
                           (plist-get (niwa-artifact-store-meta workspace id) :title)))
            (should (string-search "#ffeedd" (niwa-artifact-room-test--view workspace id))))
        (niwa-artifact-room-test--close consumer))))

  :doc "uploads an image, then shows it and the board as pictures to any link"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (viewer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (let* ((added (niwa-artifact-room-test--request
                         consumer 1 `((action . "image") (id . ,id)
                                      (data . ,niwa-artifact-room-test--png)
                                      (x . 0) (y . 200) (width . 40))))
                 (text (alist-get 'text (alist-get 'result added)))
                 (file-id (and (string-match "\\\\\"fileId\\\\\":\\\\\"\\([^\\\"]+\\)" text)
                               (match-string 1 text))))
            (should file-id)
            (should (alist-get 'png (alist-get 'result added)))
            (let ((image (alist-get 'result (niwa-artifact-room-test--request
                                             viewer 1 `((action . "view") (id . ,id)
                                                        (image . ,file-id)))))
                  (board (alist-get 'result (niwa-artifact-room-test--request
                                             viewer 2 `((action . "view") (id . ,id))))))
              (should (equal "image/png" (alist-get 'mime image)))
              (should (equal (base64-decode-string niwa-artifact-room-test--png)
                             (base64-decode-string (alist-get 'png image))))
              (should (string-prefix-p "\x89PNG" (base64-decode-string (alist-get 'png board))))))
        (niwa-artifact-room-test--close consumer)
        (niwa-artifact-room-test--close viewer))))

  :doc "refuses an image above SharedEdit's 4.7 MB limit"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    ;; Bound down to 100 kB so the refusal needs a small payload; the real
    ;; limit is checked as the constant.
    (should (= 4700000 niwa-collaboration-artifact-room--image-max-bytes))
    (let* ((niwa-collaboration-artifact-room--image-max-bytes 100000)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (should (string-match-p
                   "limited to 100000 bytes"
                   (alist-get 'error (niwa-artifact-room-test--request
                                      consumer 1 `((action . "image") (id . ,id)
                                                   (data . ,(make-string 140000 ?A)))))))
        (niwa-artifact-room-test--close consumer)))))

(defun niwa-artifact-room-test--withdraw-at (function withdraw)
  "Return a stand-in for FUNCTION that runs WITHDRAW once, before it.
FUNCTION is a step of the editing queue: admission or commit."
  (let ((original (symbol-function function)) (done nil))
    (lambda (&rest args)
      (unless done
        (setq done t)
        (funcall withdraw))
      (apply original args))))

(niwa-deftest niwa-collaboration-artifact-room--authorize
  ()
  ,test
  (test)
  :doc "drops an edit whose link lost write access after dispatch, before commit"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (before (niwa-artifact-room-test--view workspace id))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (room (niwa-collaboration-artifact-room--find root id)))
      (unwind-protect
          (progn
            ;; The helper computed the edit; the link is narrowed before
            ;; the queue commits it.
            (cl-letf (((symbol-function 'niwa-shared-editing--accept)
                       (niwa-artifact-room-test--withdraw-at
                        'niwa-shared-editing--accept
                        (lambda ()
                          (maphash (lambda (_peer guest) (plist-put guest :writable nil))
                                   (plist-get room :guests))))))
              (should (equal "Editing authority ended"
                             (alist-get 'error
                                        (niwa-artifact-room-test--request
                                         consumer 1
                                         `((action . "patch") (id . ,id)
                                           (changes . [((id . "next")
                                                        (after . ((type . "rectangle")
                                                                  (x . 200) (y . 0)
                                                                  (width . 10)
                                                                  (height . 10))))])))))))
            (should (equal before (niwa-artifact-room-test--view workspace id))))
        (niwa-artifact-room-test--close consumer))))

  :doc "drops a queued edit whose room stopped before the queue admitted it"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (before (niwa-artifact-room-test--view workspace id))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (room (niwa-collaboration-artifact-room--find root id))
           (dispatched nil))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-shared-editing--drain)
                     (niwa-artifact-room-test--withdraw-at
                      'niwa-shared-editing--drain
                      (lambda ()
                        (setq dispatched t)
                        (niwa-collaboration-artifact-room--stop room 'test)))))
            (niwa-artifact-room-test--send
             consumer `((t . "editing") (reqId . 1)
                        (body . ((action . "rename") (id . ,id) (title . "Taken")))))
            (should (niwa-relay-test--pump (lambda () dispatched)))
            ;; The queue is serial: once a later call settles, the edit has.
            (should (equal before (niwa-artifact-room-test--view workspace id)))
            (should (equal "Pipeline"
                           (plist-get (niwa-artifact-store-meta workspace id) :title))))
        (niwa-artifact-room-test--close consumer)))))

;;
;;; Versions

(defun niwa-artifact-room-test--versions (workspace id)
  "Return how many versions WORKSPACE's artifact ID has after queued saves."
  ;; A whiteboard's version is saved through its editing queue, which is
  ;; serial: once a later call settles, an earlier save has too.
  (niwa-artifact-room-test--view workspace id)
  (length (niwa-artifact-store-versions workspace id)))

(defun niwa-artifact-room-test--rename (consumer req-id id title)
  "Rename artifact ID to TITLE from CONSUMER as request REQ-ID."
  (should (assq 'result (niwa-artifact-room-test--request
                         consumer req-id `((action . "rename") (id . ,id)
                                           (title . ,title))))))

(niwa-deftest niwa-collaboration-artifact-room--versions
  ()
  ,test
  (test)
  :doc "records one version once the room's writes pause, each write restarting the wait"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    ;; The idle timer is the real one, left at a long idle and fired by
    ;; its own handler, so no wall-clock margin decides the outcome.
    (let* ((niwa-collaboration-artifact-room-version-idle 300)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (before (niwa-artifact-room-test--versions workspace id))
           (timer (lambda ()
                    (plist-get (niwa-collaboration-artifact-room--find root id)
                               :version-timer))))
      (unwind-protect
          (let (first second)
            (niwa-artifact-room-test--rename consumer 1 id "Pipeline v2")
            (setq first (funcall timer))
            (should (memq first timer-list))
            (niwa-artifact-room-test--rename consumer 2 id "Pipeline v3")
            (setq second (funcall timer))
            ;; Each write restarts the wait: the first timer is gone and
            ;; the new one ends later, so no version is due yet.
            (should (memq second timer-list))
            (should-not (eq first second))
            (should-not (memq first timer-list))
            (should (time-less-p (timer--time first) (timer--time second)))
            (should (= before (niwa-artifact-room-test--versions workspace id)))
            ;; The wait ends: the timer's own handler records the stretch.
            (timer-event-handler second)
            (should (niwa-relay-test--pump
                     (lambda () (> (niwa-artifact-room-test--versions workspace id)
                                   before))
                     5))
            (should-not (funcall timer))
            ;; One version for the whole stretch, holding its last write.
            (should (= (1+ before) (niwa-artifact-room-test--versions workspace id)))
            (should (string-search
                     "Pipeline v3"
                     (with-temp-buffer
                       (insert-file-contents
                        (niwa-artifact-store-version-path workspace id (1+ before)))
                       (buffer-string)))))
        (niwa-artifact-room-test--close consumer))))

  :doc "records the stretch's version when its consumer disconnects"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (before (niwa-artifact-room-test--versions workspace id)))
      (niwa-artifact-room-test--rename consumer 1 id "Pipeline v2")
      (should (= before (niwa-artifact-room-test--versions workspace id)))
      (niwa-artifact-room-test--close consumer)
      (should (niwa-relay-test--pump
               (lambda () (= (1+ before)
                             (niwa-artifact-room-test--versions workspace id)))
               5))))

  :doc "records no version for a stretch of reads"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((niwa-collaboration-artifact-room-version-idle 300)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (before (niwa-artifact-room-test--versions workspace id)))
      (should (niwa-artifact-room-test--request
               consumer 1 `((action . "read") (id . ,id))))
      ;; A read starts no stretch: no idle timer is waiting to record one.
      (should-not (plist-get (niwa-collaboration-artifact-room--find root id)
                             :version-timer))
      (let ((room (niwa-collaboration-artifact-room--find root id)))
        (should (> (hash-table-count (plist-get room :guests)) 0))
        (niwa-artifact-room-test--close consumer)
        ;; The origin has settled the departure once the guest is gone.
        (should (niwa-relay-test--pump
                 (lambda () (zerop (hash-table-count (plist-get room :guests)))))))
      (should (= before (niwa-artifact-room-test--versions workspace id))))))

(niwa-deftest niwa-collaboration-artifact-room--changed
  ()
  ,test
  (test)
  :doc "tells a room's consumers of its artifact's changes and of no other's"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (other (niwa-artifact-room-test--board workspace "Private"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'view))
           (events (lambda ()
                     (mapcar (lambda (frame) (alist-get 'body frame))
                             (cl-remove-if-not
                              (lambda (frame) (equal "editing-event" (alist-get 't frame)))
                              (niwa-artifact-room-test--frames consumer))))))
      (unwind-protect
          (progn
            ;; Edits at the origin, as a local agent or browser makes them.
            (should-not (plist-get (niwa-artifact-room-test--call
                                    workspace (list :action "rename" :id other
                                                    :title "Still private" :actor "Host" :opId "rename-other"))
                                   :error))
            (should-not (plist-get (niwa-artifact-room-test--call
                                    workspace (list :action "rename" :id id
                                                    :title "Pipeline v2" :actor "Host" :opId "rename-own"))
                                   :error))
            (should (niwa-relay-test--pump (lambda () (funcall events))))
            (accept-process-output nil 0.2)
            ;; One event: the other board's change never reached this room.
            (should (equal '(((event . "changed") (id . "ID") (kind . "whiteboard")
                              (revision . 3) (title . "Pipeline v2")))
                           (mapcar (lambda (event)
                                     (niwa-artifact-room-test--normal
                                      (cons '(id . "ID")
                                            (assq-delete-all 'id (copy-sequence event)))))
                                   (funcall events))))
            (should (equal id (alist-get 'id (car (funcall events))))))
        (niwa-artifact-room-test--close consumer))))

  :doc "tells an HTML room's consumers when its files change, and of no other artifact's"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (niwa-artifact-room-test--site workspace "landing")
    (niwa-artifact-room-test--site workspace "other")
    (let* ((links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'view))
           (events (lambda ()
                     (mapcar (lambda (frame)
                               (niwa-artifact-room-test--normal (alist-get 'body frame)))
                             (cl-remove-if-not
                              (lambda (frame) (equal "editing-event" (alist-get 't frame)))
                              (niwa-artifact-room-test--frames consumer))))))
      (unwind-protect
          (progn
            ;; A store change that leaves this artifact's files alone says nothing.
            (niwa-artifact-store-write-file workspace "other" "index.html" "<p>other</p>"
                                               niwa-artifact-room-test--page-hash)
            (accept-process-output nil 0.2)
            (should-not (funcall events))
            ;; A write at the origin, as its own ApplyPatch makes, reaches it.
            (niwa-artifact-store-write-file workspace "landing" "site.css" "h1 {}" nil)
            (should (niwa-relay-test--pump (lambda () (funcall events))))
            (accept-process-output nil 0.2)
            (should (= 1 (length (funcall events))))
            ;; Its revision names the files' state, so consumers can tell it changed.
            (let ((event (car (funcall events))))
              (should (stringp (alist-get 'revision event)))
              (should (equal '((event . "changed") (id . "landing") (kind . "html")
                               (title . "Landing"))
                             (assq-delete-all 'revision (copy-sequence event))))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--roster
  ()
  ,test
  (test)
  :doc "lists a connected consumer by name on its artifact for origin browsers"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (session (niwa-session--create :name "s1" :workspace workspace))
           ;; An origin browser has the board open in a session room.
           (browser (list :session session :transport 'browser
                          :guests (let ((guests (make-hash-table :test #'eql)))
                                    (puthash 1 (list :name "Host" :guest-id "host-browser"
                                                     :viewing id :active t)
                                             guests)
                                    guests)))
           (niwa-collaboration--rooms (niwa-test-room-registry browser))
           (send (symbol-function 'niwa-collaboration--transport-send))
           (roster nil)
           (names (lambda ()
                    (mapcar (lambda (person) (plist-get person :name))
                            (plist-get (car roster) :people)))))
      (setf (niwa-session-session-id session) "s1")
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (transport peer frame)
                   (if (eq transport 'browser)
                       (when (equal "presence" (plist-get frame :t))
                         (push frame roster))
                     (funcall send transport peer frame)))))
        (let ((consumer (niwa-artifact-room-test--joined state port links 'edit)))
          (unwind-protect
              (progn
                (should (equal '("remote agent (remote)") (funcall names)))
                ;; Consumers report no page and hear no presence.
                (should (niwa-artifact-room-test--request
                         consumer 1 `((action . "read") (id . ,id))))
                (should (equal '(1) (mapcar (lambda (frame) (alist-get 'reqId frame))
                                            (niwa-artifact-room-test--frames consumer)))))
            (niwa-artifact-room-test--close consumer))
          (should (niwa-relay-test--pump (lambda () (null (funcall names))) 5))))))

  :doc "takes its consumers off the roster when the room stops"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (session (niwa-session--create :name "s1" :workspace workspace))
           (browser (list :session session :transport 'browser
                          :guests (let ((guests (make-hash-table :test #'eql)))
                                    (puthash 1 (list :name "Host" :guest-id "host-browser"
                                                     :viewing id :active t)
                                             guests)
                                    guests)))
           (niwa-collaboration--rooms (niwa-test-room-registry browser))
           (send (symbol-function 'niwa-collaboration--transport-send))
           (roster nil))
      (setf (niwa-session-session-id session) "s1")
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (transport peer frame)
                   (if (eq transport 'browser)
                       (when (equal "presence" (plist-get frame :t))
                         (push frame roster))
                     (funcall send transport peer frame)))))
        (let ((consumer (niwa-artifact-room-test--joined state port links 'view)))
          (unwind-protect
              (progn
                (should (= 1 (length (plist-get (car roster) :people))))
                (niwa-collaboration-artifact-room--stop
                 (niwa-collaboration-artifact-room--find root id) 'test)
                (should (equal [] (plist-get (car roster) :people))))
            (niwa-artifact-room-test--close consumer)))))))


;;
;;; Libraries

(defconst niwa-artifact-room-test--library
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

(defmacro niwa-artifact-room-test--with-library (&rest body)
  "Run BODY with a private library directory holding the user's library."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "niwa-artifact-library-" t))
          (niwa-shared-library-directory directory))
     (unwind-protect
         (progn
           (write-region niwa-artifact-room-test--library nil
                         (expand-file-name "My library.excalidrawlib" directory)
                         nil 'silent)
           ,@body)
       (delete-directory directory t))))

(defun niwa-artifact-room-test--document (workspace title)
  "Create a document TITLE in WORKSPACE; return its id."
  (let (reply)
    (niwa-shared-editing-create workspace "document" title "Host"
                                   (lambda (value) (setq reply value)))
    (niwa-test--await 10 "creation settles" reply)
    (should-not (plist-get reply :error))
    (plist-get reply :id)))

(niwa-deftest niwa-collaboration-artifact-room--library
  ()
  ,test
  (test)
  :doc "lists the origin user's libraries and previews them to a view link"
  (niwa-artifact-room-test--with-library
    (niwa-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
             (links (niwa-collaboration-artifact-room-share root id))
             (viewer (niwa-artifact-room-test--joined state port links 'view)))
        (unwind-protect
            (let ((listing (alist-get 'result
                                      (niwa-artifact-room-test--request
                                       viewer 1 '((action . "library")))))
                  (sheet (alist-get 'result
                                    (niwa-artifact-room-test--request
                                     viewer 2 '((action . "library") (part . "sheet")
                                                (library . "My library")))))
                  (item (alist-get 'result
                                   (niwa-artifact-room-test--request
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
                                  (niwa-artifact-room-test--request
                                   viewer 4 '((action . "library") (part . "item")
                                              (item . "My library/sphere")))))))
          (niwa-artifact-room-test--close viewer)))))

  :doc "reports an unknown library as an error"
  (niwa-artifact-room-test--with-library
    (niwa-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
             (links (niwa-collaboration-artifact-room-share root id))
             (viewer (niwa-artifact-room-test--joined state port links 'view)))
        (unwind-protect
            (should (string-match-p
                     "No element library"
                     (alist-get 'error
                                (niwa-artifact-room-test--request
                                 viewer 1 '((action . "library") (library . "Nope"))))))
          (niwa-artifact-room-test--close viewer)))))

  :doc "inserts a library item through an edit link and refuses a view link"
  (niwa-artifact-room-test--with-library
    (niwa-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
             (links (niwa-collaboration-artifact-room-share root id))
             (editor (niwa-artifact-room-test--joined state port links 'edit))
             (viewer (niwa-artifact-room-test--joined state port links 'view)))
        (unwind-protect
            (let ((before (niwa-artifact-room-test--view workspace id)))
              (should (equal "This artifact link is view-only"
                             (alist-get 'error
                                        (niwa-artifact-room-test--request
                                         viewer 1 `((action . "insert") (id . ,id)
                                                    (item . "My library/cube")
                                                    (x . 300) (y . 0))))))
              (should (equal before (niwa-artifact-room-test--view workspace id)))
              (let ((reply (niwa-artifact-room-test--request
                            editor 1 `((action . "insert") (id . ,id)
                                       (item . "My library/cube") (x . 300) (y . 0)))))
                (should (assq 'result reply))
                (should (alist-get 'png (alist-get 'result reply))))
              (should (string-search "\"type\":\"rectangle\""
                                     (niwa-artifact-room-test--view workspace id)))
              (should (string-search "Agent: remote agent (remote)"
                                     (niwa-artifact-room-test--view workspace id "history")))
              (should (string-match-p
                       "No element library"
                       (alist-get 'error
                                  (niwa-artifact-room-test--request
                                   editor 2 `((action . "insert") (id . ,id)
                                              (item . "Nope/cube") (x . 0) (y . 0)))))))
          (niwa-artifact-room-test--close editor)
          (niwa-artifact-room-test--close viewer)))))

  :doc "gives a document room no library access, even to an edit link"
  (niwa-artifact-room-test--with-library
    (niwa-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (niwa-artifact-room-test--document workspace "Notes"))
             (links (niwa-collaboration-artifact-room-share root id))
             (editor (niwa-artifact-room-test--joined state port links 'edit))
             (req-id 0))
        (unwind-protect
            (dolist (body `(((action . "library"))
                            ((action . "library") (part . "sheet"))
                            ((action . "insert") (id . ,id) (item . "My library/cube"))))
              (let ((reply (niwa-artifact-room-test--request
                            editor (cl-incf req-id) body)))
                (should (stringp (alist-get 'error reply)))
                (should-not (assq 'result reply))))
          (niwa-artifact-room-test--close editor)))))

  :doc "never lets a link change the origin user's libraries"
  (niwa-artifact-room-test--with-library
    (niwa-artifact-room-test--with-origin (root workspace state port)
      (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
             (links (niwa-collaboration-artifact-room-share root id))
             (editor (niwa-artifact-room-test--joined state port links 'edit))
             (file (expand-file-name "My library.excalidrawlib"
                                     niwa-shared-library-directory))
             (req-id 0))
        (unwind-protect
            (progn
              (dolist (body `(((action . "library-add")
                               (text . ,niwa-artifact-room-test--library))
                              ((action . "library-remove") (ids . ["cube"]))
                              ((action . "library-uninstall") (name . "My library"))))
                (should (stringp (alist-get 'error
                                            (niwa-artifact-room-test--request
                                             editor (cl-incf req-id) body)))))
              (should (equal niwa-artifact-room-test--library
                             (with-temp-buffer (insert-file-contents file)
                                               (buffer-string)))))
          (niwa-artifact-room-test--close editor))))))


;;
;;; Authority

(niwa-deftest niwa-collaboration-artifact-room--dispatch
  ()
  ,test
  (test)
  :doc "drops every frame type outside the allowlist unanswered"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            (dolist (type '("prompt" "abort" "ui-response" "new-session" "fork"
                            "files" "file-get" "file-upload" "discussion"
                            "store-list" "store-action" "artifact-get"
                            "artifact-delete" "recovery" "lobby-refresh"
                            "open-session" "viewing" "editing-presence"))
              (niwa-artifact-room-test--send
               consumer `((t . ,type) (reqId . 9) (id . ,id) (text . "hi"))))
            ;; A request after them is answered, so they were all handled.
            (should (niwa-artifact-room-test--request
                     consumer 1 `((action . "read") (id . ,id))))
            (should (equal '(1) (mapcar (lambda (frame) (alist-get 'reqId frame))
                                        (niwa-artifact-room-test--frames consumer)))))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses editing actions outside the allowlist and any other artifact"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (other (niwa-artifact-room-test--board workspace "Private"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
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
              (let ((reply (niwa-artifact-room-test--request
                            consumer (cl-incf req-id) body)))
                (should (stringp (alist-get 'error reply)))
                (should-not (assq 'result reply))))
            ;; Nothing changed: both items are still there, unrenamed.
            (should (equal '("Pipeline" "Private")
                           (mapcar (lambda (item)
                                     (plist-get (niwa-artifact-store-meta
                                                 workspace item)
                                                :title))
                                   (list id other)))))
        (niwa-artifact-room-test--close consumer))))

  :doc "answers no request from a peer that never said hello"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (_ (niwa-artifact-room-test--host-joined state))
           (stranger (niwa-artifact-room-test--consumer
                      port (plist-get links :link-edit))))
      (unwind-protect
          (progn
            (niwa-artifact-room-test--send
             stranger `((t . "editing") (reqId . 1)
                        (body . ((action . "read") (id . ,id)))))
            (accept-process-output nil 0.5)
            (should-not (niwa-artifact-room-test--frames stranger)))
        (niwa-artifact-room-test--close stranger)))))


;;
;;; HTML files

(defconst niwa-artifact-room-test--page "<!doctype html>\n<h1>Hello</h1>\n"
  "The primary file of the HTML artifact the fixtures use.")

(defconst niwa-artifact-room-test--page-hash
  "28c3689be4e0c696a6a45b9d7df5a41c8328b1c9939e240f27f3c6ac9a635099"
  "SHA-256 of `niwa-artifact-room-test--page', computed by sha256sum.")

(defun niwa-artifact-room-test--site (workspace id)
  "Create HTML artifact ID in WORKSPACE with a page and a binary asset.
Return the artifact's directory."
  (niwa-artifact-store-create-meta workspace id "index.html" nil "Landing")
  (let ((directory (niwa-artifact-store-artifact-directory workspace id)))
    (make-directory (file-name-concat directory "assets") t)
    (let ((coding-system-for-write 'no-conversion))
      (write-region niwa-artifact-room-test--page nil
                    (file-name-concat directory "index.html") nil 'silent)
      (write-region (unibyte-string #x89 ?P ?N ?G 0 #xff 1 2) nil
                    (file-name-concat directory "assets" "logo.png") nil 'silent))
    directory))

(defun niwa-artifact-room-test--file (directory file)
  "Return the bytes of FILE in DIRECTORY, or nil when it is absent."
  (let ((path (file-name-concat directory file)))
    (when (file-exists-p path)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (buffer-string)))))

(niwa-deftest niwa-collaboration-artifact-room--html-read
  ()
  ,test
  (test)
  :doc "shares an HTML artifact and reads its files through a view link"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (niwa-artifact-room-test--site workspace "landing")
    (let* ((links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (pcase-let ((`(,expected . ,actual)
                         (niwa-artifact-room-test--exchange
                          consumer "read-file" '(("$ID" . "landing")))))
              (should (equal (mapcar #'niwa-artifact-room-test--normal expected)
                             (mapcar #'niwa-artifact-room-test--normal actual))))
            ;; Without a file, the primary file.
            (let ((reply (alist-get 'result (niwa-artifact-room-test--request
                                             consumer 2 '((action . "read"))))))
              (should (equal "index.html" (alist-get 'file reply)))
              (should (equal niwa-artifact-room-test--page (alist-get 'text reply))))
            ;; Bytes that are not text arrive as base64 data.
            (let ((reply (alist-get 'result (niwa-artifact-room-test--request
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
                                       (niwa-artifact-room-test--normal
                                        (alist-get 'result (niwa-artifact-room-test--request
                                                            consumer 4 '((action . "files"))))))
                            nil))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--html-write
  ()
  ,test
  (test)
  :doc "replaces a file whose hash still matches and tells the store's observers"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (changed nil)
           (niwa-artifact-store-changed-functions
            (list (lambda (changed-workspace) (push changed-workspace changed)))))
      (unwind-protect
          (pcase-let ((`(,expected . ,actual)
                       (niwa-artifact-room-test--exchange
                        consumer "write-file" '(("$ID" . "landing")))))
            (should (equal (mapcar #'niwa-artifact-room-test--normal expected)
                           (mapcar #'niwa-artifact-room-test--normal actual)))
            (should (equal "<!doctype html>\n<h1>Hello, world</h1>\n"
                           (niwa-artifact-room-test--file directory "index.html")))
            ;; Origin rooms and viewers follow the store as after a local write.
            (should (memq workspace changed)))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses a stale write, returning the current hash and content"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            ;; Someone at the origin changed the page after the consumer read it.
            (let ((coding-system-for-write 'no-conversion))
              (write-region "<!doctype html>\n<h1>Hello, world</h1>\n" nil
                            (file-name-concat directory "index.html") nil 'silent))
            (pcase-let ((`(,expected . ,actual)
                         (niwa-artifact-room-test--exchange
                          consumer "write-conflict" '(("$ID" . "landing")))))
              (should (equal (mapcar #'niwa-artifact-room-test--normal expected)
                             (mapcar #'niwa-artifact-room-test--normal actual))))
            (should (equal "<!doctype html>\n<h1>Hello, world</h1>\n"
                           (niwa-artifact-room-test--file directory "index.html"))))
        (niwa-artifact-room-test--close consumer))))

  :doc "keeps the refused hash and content when a writer commits before the room replies"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (write (symbol-function 'niwa-artifact-store-write-files))
           injected reply)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'niwa-artifact-store-write-files)
                       (lambda (workspace id changes)
                         (let ((outcome (funcall write workspace id changes)))
                           (when (and (plist-get outcome :stale) (not injected))
                             (setq injected t)
                             (should (plist-get
                                      (niwa-artifact-store-write-file
                                       workspace id "index.html" "second writer"
                                       niwa-artifact-room-test--page-hash)
                                      :hash)))
                           outcome))))
              (setq reply (niwa-artifact-room-test--request
                           consumer 1 `((action . "write-file") (file . "index.html")
                                        (hash . ,(make-string 64 ?0))
                                        (text . "stale writer")))))
            (should injected)
            (should (equal "second writer"
                           (niwa-artifact-room-test--file directory "index.html")))
            (should (equal "stale" (alist-get 'code reply)))
            (should (equal niwa-artifact-room-test--page-hash (alist-get 'hash reply)))
            (should (equal niwa-artifact-room-test--page (alist-get 'text reply))))
        (niwa-artifact-room-test--close consumer))))

  :doc "preserves locked evidence for empty, binary, large, missing and create-conflict files"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (write (symbol-function 'niwa-artifact-store-write-files))
           (file "assets/conflict.bin")
           (path (file-name-concat directory file))
           (request-id 0))
      (unwind-protect
          (dolist (spec (list (list "write-file" "" nil)
                             (list "delete-file" (unibyte-string 0 #xff 1) nil)
                             (list "write-files" (make-string (* 256 1024) ?x) nil)
                             (list "write-file" nil nil)
                             (list "write-file" "already exists" t)))
            (pcase-let* ((`(,action ,bytes ,hashless) spec)
                         (hash (and bytes (secure-hash 'sha256 bytes)))
                         (expected (unless hashless (make-string 64 ?0)))
                         (change `((file . ,file) (hash . ,(or expected :null))
                                   ,@(if (equal action "delete-file")
                                         '((delete . t)) '((text . "stale writer")))))
                         (body (if (equal action "write-files")
                                   `((action . ,action) (changes . ,(vector change)))
                                 `((action . ,action) ,@change)))
                         (injected nil)
                         (reply nil))
              (when (file-exists-p path) (delete-file path))
              (when bytes
                (let ((coding-system-for-write 'no-conversion))
                  (write-region bytes nil path nil 'silent)))
              (cl-letf (((symbol-function 'niwa-artifact-store-write-files)
                         (lambda (workspace id changes)
                           (let ((outcome (funcall write workspace id changes)))
                             (when (and (plist-get outcome :stale) (not injected))
                               (setq injected t)
                               (should (plist-get
                                        (niwa-artifact-store-write-file
                                         workspace id file "second writer" hash)
                                        :hash)))
                             outcome))))
                (setq reply (niwa-artifact-room-test--request
                             consumer (cl-incf request-id) body)))
              (should injected)
              (should (equal "second writer" (niwa-artifact-room-test--file directory file)))
              (should (equal "stale" (alist-get 'code reply)))
              (should (equal (or hash :null) (alist-get 'hash reply)))
              (cond
               ((null bytes)
                (should-not (assq 'bytes reply))
                (should (string-search "does not exist" (alist-get 'error reply))))
               ((= (length bytes) (* 256 1024))
                (should (= (length bytes) (alist-get 'bytes reply)))
                (should-not (assq 'text reply))
                (should-not (assq 'data reply)))
               ((equal action "delete-file")
                (should (equal bytes (base64-decode-string (alist-get 'data reply)))))
               (t (should (equal bytes (alist-get 'text reply)))))))
        (niwa-artifact-room-test--close consumer))))

  :doc "creates a file, and its directories, only where none exists"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (css "h1 { color: teal; }\n"))
      (unwind-protect
          (let ((created (niwa-artifact-room-test--request
                          consumer 1 `((action . "write-file") (file . "styles/site.css")
                                       (hash . :null) (text . ,css))))
                (again (niwa-artifact-room-test--request
                        consumer 2 '((action . "write-file") (file . "index.html")
                                     (text . "<p>replaced</p>"))))
                (binary (niwa-artifact-room-test--request
                         consumer 3 `((action . "write-file") (file . "assets/dot.png")
                                      (data . ,(base64-encode-string
                                                (unibyte-string #x89 ?P 0 #xfe)))))))
            (should (eq t (alist-get 'created (alist-get 'result created))))
            (should (equal css (niwa-artifact-room-test--file directory "styles/site.css")))
            ;; An existing file is not created over: the consumer gets it instead.
            (should (equal "stale" (alist-get 'code again)))
            (should (equal niwa-artifact-room-test--page-hash (alist-get 'hash again)))
            (should (equal niwa-artifact-room-test--page (alist-get 'text again)))
            (should (equal niwa-artifact-room-test--page
                           (niwa-artifact-room-test--file directory "index.html")))
            (should (assq 'result binary))
            (should (equal (unibyte-string #x89 ?P 0 #xfe)
                           (niwa-artifact-room-test--file directory "assets/dot.png"))))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses a view link's write"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (let ((reply (niwa-artifact-room-test--request
                        consumer 1 `((action . "write-file") (file . "index.html")
                                     (hash . ,niwa-artifact-room-test--page-hash)
                                     (text . "<p>defaced</p>")))))
            (should (equal "This artifact link is view-only" (alist-get 'error reply)))
            ;; A batch is a write too.
            (should (equal "This artifact link is view-only"
                           (alist-get 'error (niwa-artifact-room-test--request
                                              consumer 2 `((action . "write-files")
                                                           (changes . [((file . "new.css")
                                                                        (text . "p {}"))]))))))
            (should-not (file-exists-p (file-name-concat directory "new.css")))
            (should (equal niwa-artifact-room-test--page
                           (niwa-artifact-room-test--file directory "index.html"))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--html-paths
  ()
  ,test
  (test)
  :doc "carries the store's refusal of a path, for each action"
  ;; The store proves its path rules itself; the room only relays the refusal.
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((_ (niwa-artifact-room-test--site workspace "landing"))
           (bookkeeping (niwa-artifact-store-bookkeeping-directory workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (meta (niwa-artifact-room-test--file bookkeeping "meta.el"))
           (req-id 0))
      ;; With the store's real limit, the budget is the 30 MiB it always was.
      (should (= (* 30 1024 1024)
                 (plist-get (niwa-collaboration-artifact-room--find root "landing")
                            :editing-max-bytes)))
      (unwind-protect
          (progn
            (dolist (body `(((action . "read") (file . "../.state/landing/meta.el"))
                            ((action . "write-file") (file . "../.state/landing/meta.el")
                             (text . "x"))
                            ((action . "delete-file") (file . "../.state/landing/meta.el")
                             (hash . ,niwa-artifact-room-test--page-hash))
                            ((action . "write-files")
                             (changes . [((file . "../.state/landing/meta.el") (text . "x"))]))))
              (let ((reply (niwa-artifact-room-test--request
                            consumer (cl-incf req-id) body)))
                (should (string-prefix-p "Refused path" (or (alist-get 'error reply) "")))
                (should-not (assq 'result reply))))
            (should (equal meta (niwa-artifact-room-test--file bookkeeping "meta.el"))))
        (niwa-artifact-room-test--close consumer))))

  :doc "carries a file of the store's largest size as base64 data, both ways"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    ;; The limit is bound down to 1.5 MiB, still more than one 1 MiB frame of
    ;; base64, so the real transfer runs on a much smaller file.  The room's
    ;; transfer budget derives from the limit at its creation.
    (should (= (* 16 1024 1024) niwa-artifact-store-file-max-bytes))
    (let* ((niwa-artifact-store-file-max-bytes (* 3 512 1024))
           (directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (largest (string-to-unibyte
                     (make-string niwa-artifact-store-file-max-bytes
                                  (unibyte-char-to-multibyte #xff)))))
      (should (= (ceiling (* 15 niwa-artifact-store-file-max-bytes) 8)
                 (plist-get (niwa-collaboration-artifact-room--find root "landing")
                            :editing-max-bytes)))
      (unwind-protect
          (progn
            (should (assq 'result (niwa-artifact-room-test--request
                                   consumer 1 `((action . "write-file")
                                                (file . "assets/large.bin")
                                                (data . ,(base64-encode-string largest t)))
                                   60)))
            (should (equal largest (niwa-artifact-room-test--file directory
                                                                     "assets/large.bin")))
            (let ((read (alist-get 'result (niwa-artifact-room-test--request
                                            consumer 2 '((action . "read")
                                                         (file . "assets/large.bin"))
                                            60))))
              (should (equal niwa-artifact-store-file-max-bytes (alist-get 'bytes read)))
              (should (equal largest (base64-decode-string (alist-get 'data read))))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--html-delete
  ()
  ,test
  (test)
  :doc "deletes a file whose hash still matches, never the primary file or the artifact"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (logo (alist-get 'hash (alist-get 'result (niwa-artifact-room-test--request
                                                      consumer 1 '((action . "read")
                                                                   (file . "assets/logo.png"))))))
           (stale (niwa-artifact-room-test--request
                   consumer 2 `((action . "delete-file") (file . "assets/logo.png")
                                (hash . ,niwa-artifact-room-test--page-hash))))
           (deleted (niwa-artifact-room-test--request
                     consumer 3 `((action . "delete-file") (file . "assets/logo.png")
                                  (hash . ,logo))))
           (primary (niwa-artifact-room-test--request
                     consumer 4 `((action . "delete-file") (file . "index.html")
                                  (hash . ,niwa-artifact-room-test--page-hash))))
           (artifact (niwa-artifact-room-test--request
                      consumer 5 '((action . "delete") (id . "landing")))))
      (unwind-protect
          (progn
            (should (equal "stale" (alist-get 'code stale)))
            (should (equal logo (alist-get 'hash stale)))
            (should (assq 'result deleted))
            (should-not (file-exists-p (file-name-concat directory "assets/logo.png")))
            ;; The store's refusal, as it words it.
            (should (string-search "primary file; delete the artifact instead"
                                   (alist-get 'error primary)))
            (should (stringp (alist-get 'error artifact)))
            (should (equal niwa-artifact-room-test--page
                           (niwa-artifact-room-test--file directory "index.html")))
            (should (niwa-artifact-store-meta workspace "landing")))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses a view link's deletion"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (should (equal "This artifact link is view-only"
                           (alist-get 'error (niwa-artifact-room-test--request
                                              consumer 1 '((action . "delete-file")
                                                           (file . "assets/logo.png")
                                                           (hash . "0"))))))
            (should (file-exists-p (file-name-concat directory "assets/logo.png"))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--html-write-files
  ()
  ,test
  (test)
  :doc "changes several files at once when every hash still matches"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (pcase-let ((`(,expected . ,actual)
                       (niwa-artifact-room-test--exchange
                        consumer "write-files" '(("$ID" . "landing")))))
            (should (equal (mapcar #'niwa-artifact-room-test--normal expected)
                           (mapcar #'niwa-artifact-room-test--normal actual)))
            (should (equal "<!doctype html>\n<h1>Hello, world</h1>\n"
                           (niwa-artifact-room-test--file directory "index.html")))
            (should (equal "h1 { color: teal; }\n"
                           (niwa-artifact-room-test--file directory "styles/site.css")))
            (should-not (file-exists-p (file-name-concat directory "assets/logo.png"))))
        (niwa-artifact-room-test--close consumer))))

  :doc "changes nothing when one hash is stale, and names that file"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((directory (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (reply (niwa-artifact-room-test--request
                   consumer 1 `((action . "write-files")
                                (changes . [((file . "index.html")
                                             (hash . ,niwa-artifact-room-test--page-hash)
                                             (text . "<p>mine</p>"))
                                            ((file . "new.css") (text . "p {}"))
                                            ((file . "assets/logo.png")
                                             (hash . ,niwa-artifact-room-test--page-hash)
                                             (delete . t))])))))
      (unwind-protect
          (progn
            (should (equal "stale" (alist-get 'code reply)))
            (should (equal "assets/logo.png" (alist-get 'file reply)))
            (should (equal niwa-artifact-room-test--page
                           (niwa-artifact-room-test--file directory "index.html")))
            (should-not (file-exists-p (file-name-concat directory "new.css")))
            (should (file-exists-p (file-name-concat directory "assets/logo.png")))
            ;; A refused batch is announced to no one and starts no version.
            (accept-process-output nil 0.1)
            (should-not (cl-find "event" (niwa-artifact-room-test--frames consumer)
                                 :key (lambda (frame) (alist-get 'reqId frame))
                                 :test #'equal))
            (niwa-artifact-room-test--close consumer)
            (should-not (niwa-artifact-store-versions workspace "landing")))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--html-versions
  ()
  ,test
  (test)
  :doc "records one version for a stretch that wrote the primary file, none for assets alone"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    ;; Long idle; each stretch is ended by firing the real idle timer.
    (let* ((niwa-collaboration-artifact-room-version-idle 300)
           (end-stretch (lambda ()
                          (when-let* ((timer (plist-get
                                              (niwa-collaboration-artifact-room--find
                                               root "landing")
                                              :version-timer)))
                            (timer-event-handler timer))))
           (_ (niwa-artifact-room-test--site workspace "landing"))
           (links (niwa-collaboration-artifact-room-share root "landing"))
           (consumer (niwa-artifact-room-test--joined state port links 'edit)))
      (unwind-protect
          (progn
            (should (assq 'result (niwa-artifact-room-test--request
                                   consumer 1 '((action . "write-file")
                                                (file . "assets/extra.css")
                                                (text . "p {}")))))
            (funcall end-stretch)
            (accept-process-output nil 0.1)
            (should-not (niwa-artifact-store-versions workspace "landing"))
            (should (assq 'result (niwa-artifact-room-test--request
                                   consumer 2 `((action . "write-file") (file . "index.html")
                                                (hash . ,niwa-artifact-room-test--page-hash)
                                                (text . "<p>v2</p>")))))
            (funcall end-stretch)
            (should (niwa-relay-test--pump
                     (lambda () (niwa-artifact-store-versions workspace "landing"))
                     5))
            (should (= 1 (length (niwa-artifact-store-versions workspace "landing"))))
            (should (equal "<p>v2</p>"
                           (niwa-artifact-room-test--file
                            (file-name-directory
                             (niwa-artifact-store-version-path workspace "landing" 1))
                            (file-name-nondirectory
                             (niwa-artifact-store-version-path workspace "landing" 1))))))
        (niwa-artifact-room-test--close consumer)))))


;;
;;; Revoking, rotating, deleting

(defun niwa-artifact-room-test--goodbye (consumer reason)
  "Check that CONSUMER was told goodbye for REASON, as the fixture shows."
  (niwa-artifact-room-test--await consumer 1)
  (should (equal (mapcar #'niwa-artifact-room-test--normal
                         (mapcar #'cdr (niwa-artifact-room-test--fixture
                                        "goodbye" `(("$REASON" . ,reason)))))
                 (mapcar #'niwa-artifact-room-test--normal
                         (niwa-artifact-room-test--frames consumer)))))

(niwa-deftest niwa-collaboration-artifact-room-revoke
  ()
  ,test
  (test)
  :doc "disconnects connected consumers with revoked and ends the links for good"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (should-not (niwa-collaboration-artifact-room-revoke root id))
            (niwa-artifact-room-test--goodbye consumer "revoked")
            (should-not (niwa-artifact-store-shared-p workspace id))
            ;; Sharing again mints new links; the old ones stay dead.
            (let ((again (niwa-collaboration-artifact-room-share root id)))
              (should-not (equal (plist-get links :link-view) (plist-get again :link-view)))
              (should-not (equal (plist-get links :link-edit) (plist-get again :link-edit)))))
        (niwa-artifact-room-test--close consumer))))

  :doc "leaves an artifact that is not shared unshared"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let ((id (niwa-artifact-room-test--board workspace "Pipeline")))
      (should-not (niwa-collaboration-artifact-room-revoke root id))
      (should-not (niwa-artifact-store-shared-p workspace id)))))

(niwa-deftest niwa-collaboration-artifact-room-rotate
  ()
  ,test
  (test)
  :doc "replaces the links, telling connected consumers revoked"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'edit))
           (rotated (niwa-collaboration-artifact-room-rotate root id)))
      (unwind-protect
          (progn
            (niwa-artifact-room-test--goodbye consumer "revoked")
            (should-not (equal (car (niwa-artifact-room-test--secret
                                     (plist-get links :link-view)))
                               (car (niwa-artifact-room-test--secret
                                     (plist-get rotated :link-view)))))
            (should (niwa-artifact-store-shared-p workspace id))
            ;; The new links work and are what sharing returns from now on.
            (should (equal rotated (niwa-collaboration-artifact-room-share root id)))
            (let ((fresh (niwa-artifact-room-test--joined state port rotated 'view)))
              (unwind-protect
                  (should (alist-get 'result
                                     (niwa-artifact-room-test--request
                                      fresh 1 `((action . "read") (id . ,id)))))
                (niwa-artifact-room-test--close fresh))))
        (niwa-artifact-room-test--close consumer))))

  :doc "refuses an artifact that does not exist"
  (niwa-artifact-room-test--with-origin (root _workspace _state _port)
    (should-error (niwa-collaboration-artifact-room-rotate root "missing")
                  :type 'user-error)))

(niwa-deftest niwa-collaboration-artifact-room-sharing
  ()
  ,test
  (test)
  :doc "is one host operation for sharing, revoking and rotating"
  (niwa-artifact-room-test--with-origin (_root workspace _state _port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-sharing workspace id 'share)))
      (should (equal links (niwa-collaboration-artifact-room-sharing workspace id 'share)))
      (let ((rotated (niwa-collaboration-artifact-room-sharing workspace id 'rotate)))
        (should (plist-get rotated :link-edit))
        (should-not (equal links rotated)))
      (should-not (niwa-collaboration-artifact-room-sharing workspace id 'revoke))
      (should-not (niwa-artifact-store-shared-p workspace id))
      (should-error (niwa-collaboration-artifact-room-sharing workspace id 'publish)))))

(niwa-deftest niwa-collaboration--store-sharing
  ()
  ,test
  (test)
  :doc "shares, rotates and revokes from the browser for owner links only"
  (niwa-artifact-room-test--with-origin (_root workspace _state _port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (lobby (list :workspace workspace :transport 'lobby
                        :guests (make-hash-table :test #'eql)))
           (owner (list :name "owner" :writable t :owner t :ready t))
           (full (list :name "full" :writable t :ready t))
           (view (list :name "view" :writable nil :ready t))
           (changes 0)
           (niwa-artifact-store-changed-functions
            (list (lambda (_workspace) (cl-incf changes)))))
      (cl-flet ((sharing (guest operation)
                  (niwa-collaboration--store-action
                   lobby guest (list :action "sharing" :id id :operation operation)))
                (listed ()
                  (plist-get (cl-find id (niwa-collaboration--store-rows lobby)
                                      :key (lambda (row) (plist-get row :id))
                                      :test #'equal)
                             :shared)))
        ;; Full and view links cannot share, rotate or revoke.
        (dolist (guest (list full view))
          (dolist (operation '("share" "rotate" "revoke"))
            (should (equal "Only the host or an owner link can share artifacts"
                           (cadr (should-error (sharing guest operation)))))))
        (should-not (niwa-artifact-store-shared-p workspace id))
        (should (eq :json-false (listed)))
        (should (zerop changes))
        ;; Sharing returns the same links the Emacs commands show.
        (let ((links (sharing owner "share")))
          (should (eq t (plist-get links :shared)))
          (should (equal (niwa-collaboration-artifact-room-sharing workspace id 'share)
                         (list :link-view (plist-get links :linkView)
                               :link-edit (plist-get links :linkEdit))))
          (should (eq t (listed)))
          (should (> changes 0))
          (should-error (sharing full "revoke"))
          (should (niwa-artifact-store-shared-p workspace id))
          (let ((rotated (sharing owner "rotate")))
            (should (eq t (plist-get rotated :shared)))
            (should (stringp (plist-get rotated :linkEdit)))
            (should-not (equal (plist-get links :linkEdit) (plist-get rotated :linkEdit)))))
        (setq changes 0)
        (should (equal (list :id id :shared :json-false) (sharing owner "revoke")))
        (should-not (niwa-artifact-store-shared-p workspace id))
        (should (eq :json-false (listed)))
        (should (> changes 0))
        (should (equal "Unknown sharing operation"
                       (cadr (should-error (sharing owner "publish")))))))))

(niwa-deftest niwa-collaboration-artifact-room--deleted
  ()
  ,test
  (test)
  :doc "deleting a shared artifact disconnects its consumers with artifact-deleted"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'view))
           (deleted 'pending))
      (unwind-protect
          (progn
            (niwa-artifact-store-delete workspace id "Host"
                                           (lambda (failure) (setq deleted failure)))
            (niwa-test--await 10 "deletion" (not (eq deleted 'pending)))
            (should-not deleted)
            (niwa-artifact-room-test--goodbye consumer "artifact-deleted")
            (should-not (niwa-collaboration-artifact-room--find root id))
            (should-not (niwa-artifact-store-meta workspace id)))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room--duplicate
  ()
  ,test
  (test)
  :doc "a duplicate of a shared artifact starts unshared"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id)))
      (niwa-artifact-store-duplicate workspace id "pipeline-copy")
      (should (niwa-artifact-store-meta workspace "pipeline-copy"))
      (should-not (niwa-artifact-store-shared-p workspace "pipeline-copy"))
      (should-not (niwa-collaboration-artifact-room--find root "pipeline-copy"))
      (should (equal links (niwa-collaboration-artifact-room-share root id))))))


;;
;;; Origin restarts

(niwa-deftest niwa-collaboration-artifact-room--stop-for-emacs
  ()
  ,test
  (test)
  :doc "an exiting origin tells consumers origin-stopped and stays shared"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((kill-emacs-hook nil)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'view)))
      (unwind-protect
          (progn
            (should (memq #'niwa-collaboration-artifact-room--stop-for-emacs
                          kill-emacs-hook))
            (run-hooks 'kill-emacs-hook)
            (niwa-artifact-room-test--goodbye consumer "origin-stopped")
            (should-not (niwa-collaboration-artifact-room--find root id))
            (should (niwa-artifact-store-shared-p workspace id))
            (should (equal (list root) (niwa-collaboration-artifact-room--recorded))))
        (niwa-artifact-room-test--close consumer)))))

(niwa-deftest niwa-collaboration-artifact-room-restore
  ()
  ,test
  (test)
  :doc "redials each recorded root's shared artifacts under their stored room ids"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (let* ((kill-emacs-hook nil)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id)))
      (niwa-artifact-room-test--host-joined state)
      (run-hooks 'kill-emacs-hook)
      (should (niwa-relay-test--pump (lambda () (not (plist-get (car state) :host)))))
      ;; The per-user record is a private data file.
      (let ((record (file-name-concat niwa-user-dir "artifact-rooms.eld")))
        (should (file-exists-p record))
        (should (= #o600 (file-modes record))))
      ;; The next Emacs knows nothing but the record and the bookkeeping.
      (clrhash niwa-collaboration-artifact-room--rooms)
      (niwa-collaboration-artifact-room-restore)
      (let ((room (niwa-collaboration-artifact-room--find root id)))
        (should room)
        (should (equal (car (niwa-artifact-room-test--secret (plist-get links :link-view)))
                       (plist-get room :room-id))))
      ;; The links handed out before the restart work again.
      (let ((consumer (niwa-artifact-room-test--joined state port links 'edit)))
        (unwind-protect
            (should (alist-get 'result
                               (niwa-artifact-room-test--request
                                consumer 1 `((action . "read") (id . ,id)))))
          (niwa-artifact-room-test--close consumer)))))

  :doc "drops roots that have no shared artifacts left or no longer exist"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((kill-emacs-hook nil)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (gone (file-name-as-directory (file-name-concat root "gone"))))
      (niwa-collaboration-artifact-room-share root id)
      (should (equal (list root) (niwa-collaboration-artifact-room--recorded)))
      ;; Revoking the last shared artifact drops the root at once.
      (niwa-collaboration-artifact-room-revoke root id)
      (should-not (niwa-collaboration-artifact-room--recorded))
      ;; A record that outlived its shares or its directory is cleaned up.
      (niwa-collaboration-artifact-room--set-recorded root t)
      (niwa-collaboration-artifact-room--set-recorded gone t)
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (niwa-collaboration-artifact-room-restore))
      (should-not (niwa-collaboration-artifact-room--recorded))
      (should (zerop (hash-table-count niwa-collaboration-artifact-room--rooms)))))

  :doc "keeps a root recorded when its rooms cannot start yet"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((kill-emacs-hook nil)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           warnings)
      (niwa-collaboration-artifact-room-share root id)
      (run-hooks 'kill-emacs-hook)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-open)
                 (lambda (&rest _) (error "Relay unavailable")))
                ((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warnings))))
        (niwa-collaboration-artifact-room-restore))
      (should warnings)
      (should (equal (list root) (niwa-collaboration-artifact-room--recorded)))
      (should (niwa-artifact-store-shared-p workspace id))))

  :doc "deleting the last shared artifact drops its root"
  (niwa-artifact-room-test--with-origin (root workspace _state _port)
    (let* ((kill-emacs-hook nil)
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (deleted 'pending))
      (niwa-collaboration-artifact-room-share root id)
      (niwa-artifact-store-delete workspace id "Host"
                                     (lambda (failure) (setq deleted failure)))
      (niwa-test--await 10 "deletion" (not (eq deleted 'pending)))
      (should-not (niwa-collaboration-artifact-room--recorded)))))


;;
;;; Independence from the lobby

(niwa-deftest niwa-collaboration-artifact-room--lobby
  ()
  ,test
  (test)
  :doc "starting, rotating and stopping the lobby leaves artifact rooms alone"
  (niwa-artifact-room-test--with-origin (root workspace state port)
    (require 'niwa-collaboration-lobby)
    (let* ((kill-emacs-hook nil)
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (id (niwa-artifact-room-test--board workspace "Pipeline"))
           (links (niwa-collaboration-artifact-room-share root id))
           (consumer (niwa-artifact-room-test--joined state port links 'view))
           (room (niwa-collaboration-artifact-room--find root id))
           (open (symbol-function 'niwa-collaboration--transport-open))
           (stop (symbol-function 'niwa-collaboration--transport-stop))
           (default-directory root))
      (unwind-protect
          ;; The stub relay hosts one room, the artifact's; the lobby
          ;; gets a transport of its own that goes nowhere.
          (cl-letf (((symbol-function 'niwa-collaboration--transport-open)
                     (lambda (url &rest args)
                       (if (string-search (plist-get room :room-id) url)
                           (apply open url args)
                         (list :fake-lobby t))))
                    ((symbol-function 'niwa-collaboration--transport-stop)
                     (lambda (transport)
                       (unless (plist-get transport :fake-lobby)
                         (funcall stop transport))))
                    ((symbol-function 'niwa-collaboration-share-present) #'ignore)
                    ((symbol-function 'yes-or-no-p) #'always))
            (niwa-test--with-captured-messages nil
              (niwa-collaboration-lobby)
              (should (niwa-collaboration-lobby--find workspace))
              (niwa-collaboration-lobby-rotate)
              (should (niwa-collaboration-lobby--find workspace))
              (niwa-collaboration-lobby-stop))
            (should-not (niwa-collaboration-lobby--find workspace))
            (should (eq room (niwa-collaboration-artifact-room--find root id)))
            (should (equal links (niwa-collaboration-artifact-room-share root id)))
            (should (alist-get 'result
                               (niwa-artifact-room-test--request
                                consumer 1 `((action . "read") (id . ,id)))))
            (should-not (cl-some (lambda (frame) (equal "bye" (alist-get 't frame)))
                                 (niwa-artifact-room-test--frames consumer))))
        (niwa-artifact-room-test--close consumer)))))

(provide 'test-niwa-collaboration-artifact-room)
;;; test-niwa-collaboration-artifact-room.el ends here
