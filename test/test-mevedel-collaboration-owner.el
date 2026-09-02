;;; test-mevedel-collaboration-owner.el --- Owner collaboration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests owner collaboration protocol helpers and rollback cleanup.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'mevedel-collaboration-agent)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-owner)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-task)
(require 'mevedel-collaboration-transport)
(require 'mevedel-session-artifacts)
(require 'mevedel-structs)

(mevedel-deftest mevedel-collaboration--new-session-reply
  (:doc "targets one peer with the supplied new-session outcome")
  (let (sent)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (transport peer frame)
                 (setq sent (list transport peer frame)))))
      (mevedel-collaboration--new-session-reply
       '(:transport relay) 7 42 "flow" :ok t :link "link")
      (should (equal '(relay 7 (:t "new-session" :reqId 42 :name "flow"
                                  :ok t :link "link"))
                     sent)))))

(mevedel-deftest mevedel-collaboration--discard-created-session
  (:doc "stops the partial room and kills its buffer without a query")
  (let ((buffer (generate-new-buffer " *partial-guest-session*"))
        stopped)
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions (list (lambda () nil)))
      (insert "modified"))
    (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
               (lambda (room reason) (setq stopped (cons room reason)))))
      (mevedel-collaboration--discard-created-session 'room buffer))
    (should (equal '(room . start-failed) stopped))
    (should-not (buffer-live-p buffer)))
  ;; Cleanup is best-effort and must not escape into the parent room,
  ;; even when both the room stop and a buffer hook fail.
  (let* ((buffer (generate-new-buffer " *failing-guest-session*"))
         (view (generate-new-buffer " *failing-guest-view*"))
         (hook (lambda () (error "Broken kill hook")))
         escaped)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--view-buffer view)
            (add-hook 'kill-buffer-hook hook nil t))
          (with-current-buffer view
            (add-hook 'kill-buffer-hook hook nil t))
          (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
                     (lambda (&rest _) (error "Broken room stop"))))
            (condition-case nil
                (mevedel-collaboration--discard-created-session 'room buffer)
              (error (setq escaped t))))
          (should-not escaped)
          (should-not (buffer-live-p buffer))
          (should-not (buffer-live-p view)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (remove-hook 'kill-buffer-hook hook t)
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (buffer-live-p view)
        (with-current-buffer view
          (remove-hook 'kill-buffer-hook hook t)
          (set-buffer-modified-p nil))
        (kill-buffer view)))))

(mevedel-deftest mevedel-collaboration--owner
  (:doc "grants owner authority only to a peer holding both tokens")
  (let* ((guests (make-hash-table :test #'eql))
         (token (mevedel-collaboration--random-bytes 16))
         (owner-token (mevedel-collaboration--random-bytes 16))
         (room (list :transport 'transport :guests guests
                     :write-token token :owner-token owner-token
                     :records nil
                     :ui-requests (make-hash-table :test #'eql))))
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (&rest _) t)))
      (let ((hello (lambda (peer write owner)
                     (mevedel-collaboration--handle-hello
                      room peer
                      (append
                       (list :proto mevedel-collaboration--protocol-version
                             :name (format "guest-%s" peer))
                       (when write
                         (list :writeToken
                               (mevedel-collaboration--base64url write)))
                       (when owner
                         (list :ownerToken
                               (mevedel-collaboration--base64url owner))))))))
        (funcall hello 1 token owner-token)
        (funcall hello 2 token nil)
        ;; The owner token alone is a forgery: the owner link always
        ;; carries the write token too.
        (funcall hello 3 nil owner-token)
        (funcall hello 4 token (mevedel-collaboration--random-bytes 16)))
      (should (mevedel-collaboration--owner room 1))
      (should-not (mevedel-collaboration--owner room 2))
      (should-not (mevedel-collaboration--owner room 3))
      (should-not (mevedel-collaboration--owner room 4))
      ;; An owner is a writer as well, never instead.
      (should (plist-get (gethash 1 guests) :writable)))))

(mevedel-deftest mevedel-collaboration--handle-set-mode
  (:doc "changes permission mode for an owner and refuses everyone else")
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :guests guests
                     :data-buffer (current-buffer)))
         transitions published)
    (puthash 1 (list :name "Owner" :writable t :owner t :ready t) guests)
    (puthash 2 (list :name "Writer" :writable t :ready t) guests)
    (cl-letf (((symbol-function 'mevedel-permission-mode-transition)
               (lambda (mode) (push mode transitions) mode))
              ((symbol-function 'mevedel-collaboration--publish-status)
               (lambda (_room) (setq published (1+ (or published 0)))))
              ((symbol-function 'mevedel-collaboration--room-data-buffer)
               (lambda (_room) (current-buffer))))
      (mevedel-test--with-captured-messages nil
        ;; A writable guest without the owner token changes nothing.
        (mevedel-collaboration--handle-set-mode room 2 '(:mode "full-auto"))
        (should-not transitions)
        ;; So does an owner naming a mode that does not exist.
        (mevedel-collaboration--handle-set-mode room 1 '(:mode "yolo"))
        (should-not transitions)
        (mevedel-collaboration--handle-set-mode room 1 '(:mode "edits"))
        (mevedel-collaboration--handle-set-mode room 1 '(:mode "full-auto"))
        (should (equal '(full-auto edits) transitions))
        ;; Every accepted change reaches the whole room's status strip.
        (should (= 2 published))))))

(mevedel-deftest mevedel-collaboration--handle-new-session
  (:doc "grants owners, asks for writers, and deduplicates exact requests")
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :guests guests))
         created asked)
    (puthash 1 (list :name "Owner" :writable t :owner t :ready t) guests)
    (puthash 2 (list :name "Writer" :writable t :ready t) guests)
    (puthash 3 (list :name "Reader" :ready t) guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--create-guest-session)
               (lambda (_room _peer _guest _request-id name prompt)
                 (push (cons name prompt) created)))
              ((symbol-function 'mevedel-collaboration--ask-host-new-session)
               (lambda (_room _peer _guest _request-id name prompt)
                 (push (cons name prompt) asked))))
      (mevedel-collaboration--handle-new-session
       room 1 '(:reqId 1 :name "onboarding" :prompt "Design the flow"))
      (should (equal '(("onboarding" . "Design the flow")) created))
      (mevedel-collaboration--handle-new-session
       room 2 '(:reqId 2 :name "auth work" :prompt ""))
      ;; The name is sanitized the way a session name typed in Emacs is,
      ;; because it becomes a directory; an empty prompt is no prompt.
      (should (equal '(("auth_work" . nil)) asked))
      ;; A read-only guest cannot ask at all.
      (mevedel-collaboration--handle-new-session
       room 3 '(:reqId 3 :name "sneaky"))
      (should (= 1 (length asked)))
      ;; Creating a session is not idempotent, so a double-fired submit
      ;; must not make two of them.
      (mevedel-collaboration--handle-new-session
       room 1 '(:reqId 4 :name "again"))
      (should (= 2 (length created)))
      (mevedel-collaboration--handle-new-session
       room 1 '(:reqId 4 :name "again"))
      (should (= 2 (length created)))
      ;; Neither a missing name nor one that sanitizes to punctuation is
      ;; a session name.
      (remhash 1 guests)
      (puthash 1 (list :name "Owner" :writable t :owner t :ready t) guests)
      (mevedel-collaboration--handle-new-session
       room 1 '(:reqId 5 :name "   "))
      (mevedel-collaboration--handle-new-session
       room 1 '(:reqId 6 :name "///"))
      (mevedel-collaboration--handle-new-session
       room 1 '(:reqId 7 :prompt "no name"))
      (mevedel-collaboration--handle-new-session
       room 1 '(:reqId "8" :name "invalid request id"))
      (should (= 2 (length created)))
      ;; A request already waiting on a person blocks the next one, whose
      ;; approval would only fail on the name the first one took.
      (let (sent)
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport _peer frame) (push frame sent))))
          (plist-put (gethash 2 guests) :pending-new-session
                     '(8 . "auth_work"))
          (mevedel-collaboration--handle-new-session
           room 2 '(:reqId 9 :name "other work"))
          (should (= 1 (length asked)))
          (should (eq :json-false (plist-get (car sent) :ok)))
          (should (equal "other_work" (plist-get (car sent) :name)))
          (should (= 9 (plist-get (car sent) :reqId)))
          (should (string-match-p "auth_work"
                                  (plist-get (car sent) :message)))
          ;; It unblocks once that one is answered.
          (plist-put (gethash 2 guests) :pending-new-session nil)
          (mevedel-collaboration--handle-new-session
           room 2 '(:reqId 10 :name "other work"))
          (should (= 2 (length asked))))))))

(mevedel-deftest mevedel-collaboration--ask-host-new-session
  (:doc "asks the host and owners, never the guest the request is about")
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :data-buffer (current-buffer)
                     :guests guests))
         (guest (list :name "Writer" :writable t :guest-id "writer-id"))
         (current-room room)
         captured sent)
    (puthash 7 guest guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--room-data-buffer)
               (lambda (_room) (current-buffer)))
              ((symbol-function 'mevedel-collaboration--room-for-buffer)
               (lambda (_buffer) current-room))
              ((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) sent)))
              ((symbol-function 'mevedel-collaboration--create-guest-session)
               (lambda (&rest args) (push (cons 'created args) sent)))
              ((symbol-function 'mevedel--prompt-user-with-overlay)
               (lambda (_title _content _question _echo callback &optional
                               host-only audience)
                 (setq captured (list :callback callback
                                      :host-only host-only
                                      :audience audience))
                 'overlay)))
      (mevedel-collaboration--ask-host-new-session
       room 7 guest 42 "flow" "go")
      ;; An owner may create a session outright, so approving someone
      ;; else's request is no new authority.  Only a non-owner ever gets
      ;; here, so restricting to owners already excludes the requester --
      ;; including when it is another tab of the owner's own browser,
      ;; which shares its stable guest identity.
      (should-not (plist-get captured :host-only))
      (should (eq 'owner (plist-get captured :audience)))
      ;; Declining tells the guest, and creates nothing.
      (funcall (plist-get captured :callback) 'deny)
      (should (equal "new-session" (plist-get (cdr (car sent)) :t)))
      (should (= 42 (plist-get (cdr (car sent)) :reqId)))
      (should (eq :json-false (plist-get (cdr (car sent)) :ok)))
      ;; Feedback becomes the reason the guest is shown.
      (funcall (plist-get captured :callback) '(feedback . "not now"))
      (should (equal "not now" (plist-get (cdr (car sent)) :message)))
      (setq sent nil)
      ;; A delayed answer cannot act on a restarted room or a peer that
      ;; authenticated again as a different guest.
      (setq current-room nil)
      (funcall (plist-get captured :callback) 'approve)
      (should-not sent)
      (setq current-room room)
      (puthash 7 (list :name "Replacement" :writable t) guests)
      (funcall (plist-get captured :callback) 'approve)
      (should-not sent)
      (puthash 7 guest guests)
      (funcall (plist-get captured :callback) 'approve)
      (should (eq 'created (car (car sent))))
      (should (equal '("flow" "go") (last (cdr (car sent)) 2))))))

(mevedel-deftest mevedel-collaboration--offer-room-to-owners
  (:doc "offers a created room to the other owners, never to the requester")
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :guests guests))
         sent)
    (puthash 1 (list :name "Requester" :writable t :owner t) guests)
    (puthash 2 (list :name "Other owner" :writable t :owner t) guests)
    (puthash 3 (list :name "Writer" :writable t) guests)
    (puthash 4 (list :name "Reader") guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) sent))))
      (mevedel-collaboration--offer-room-to-owners
       room 1 "flow" "owner-link"))
    ;; Only the other owner: the requester's own reply carried a link,
    ;; and a writer has no business being handed an owner link.
    (should (equal '(2) (mapcar #'car sent)))
    (let ((frame (cdr (car sent))))
      ;; Its own frame, because it answers no request the receiver made.
      (should (equal "room" (plist-get frame :t)))
      (should (equal "flow" (plist-get frame :name)))
      (should (equal "owner-link" (plist-get frame :link))))))

(mevedel-deftest mevedel-collaboration--create-guest-session
  (:doc "creates atomically, rejects collisions, and preserves guest tier")
  (let* ((session (mevedel-session--create :name "parent"))
         (room (list :transport 'transport :session session))
         (new-room (list :link-full "full-link" :link-owner "owner-link"))
         existing sent enqueued stopped)
    (setf (mevedel-session-workspace session) 'workspace)
    (setf (mevedel-session-working-directory session) "/tmp/ws/")
    (cl-letf (((symbol-function 'mevedel--workspace-sessions)
               (lambda (_workspace) existing))
              ((symbol-function 'mevedel--chat-buffer)
               (lambda (&rest _) (current-buffer)))
              ((symbol-function 'mevedel--display-chat-buffer) #'ignore)
              ((symbol-function 'mevedel-collaboration--start)
               (lambda (&rest _) new-room))
              ((symbol-function 'mevedel-collaboration--stop-internal)
               (lambda (stopped-room _reason)
                 (push stopped-room stopped)))
              ((symbol-function 'mevedel-view-enqueue-external-follow-up)
               (lambda (_buffer text &rest _) (push text enqueued)))
              ((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport _peer frame) (push frame sent))))
      (mevedel-test--with-captured-messages nil
        (setq existing '(("taken" . nil)))
        (mevedel-collaboration--create-guest-session
         room 1 '(:name "Writer" :writable t) 1 "taken" "go")
        (should (eq :json-false (plist-get (car sent) :ok)))
        (should-not enqueued)
        ;; A full-control requester gets a full-control link back:
        ;; asking for a session is never a way to gain authority.
        (setq existing nil sent nil)
        (mevedel-collaboration--create-guest-session
         room 1 '(:name "Writer" :writable t) 2 "fresh" "go")
        (should (equal "full-link" (plist-get (car sent) :link)))
        ;; The approved prompt goes straight into the pending queue,
        ;; which drains on idle -- nothing further to press.
        (should (equal '("go") enqueued))
        (setq sent nil enqueued nil)
        (mevedel-collaboration--create-guest-session
         room 1 '(:name "Owner" :writable t :owner t) 3 "fresh" nil)
        (should (equal "owner-link" (plist-get (car sent) :link)))
        (should (eq t (plist-get (car sent) :ok)))
        (should-not enqueued)
        ;; Host presentation is incidental: it cannot revoke a room or
        ;; suppress the successful protocol reply.
        (setq sent nil stopped nil)
        (cl-letf (((symbol-function 'mevedel--display-chat-buffer)
                   (lambda (&rest _) (error "Broken display"))))
          (mevedel-collaboration--create-guest-session
           room 1 '(:name "Owner" :writable t :owner t) 4 "display" nil))
        (should (eq t (plist-get (car sent) :ok)))
        (should-not stopped)
        ;; A room with no delivered bearer link is not a successful
        ;; creation and must not reserve the requested name.
        (let ((created-buffer (generate-new-buffer " *guest-session-send*")))
          (setq sent nil stopped nil)
          (cl-letf (((symbol-function 'mevedel--chat-buffer)
                     (lambda (&rest _) created-buffer))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (&rest _) nil)))
            (mevedel-collaboration--create-guest-session
             room 1 '(:name "Writer" :writable t) 5 "disconnected" nil))
          (should-not (buffer-live-p created-buffer))
          (should (equal (list new-room) stopped)))
        ;; Failure to queue an approved prompt rolls back both the room
        ;; and its new data buffer, without signaling into the parent room.
        (let ((created-buffer (generate-new-buffer " *guest-session-failure*")))
          (setq sent nil enqueued nil stopped nil)
          (cl-letf (((symbol-function 'mevedel--chat-buffer)
                     (lambda (&rest _) created-buffer))
                    ((symbol-function 'mevedel-view-enqueue-external-follow-up)
                     (lambda (&rest _) nil)))
            (mevedel-collaboration--create-guest-session
             room 1 '(:name "Writer" :writable t) 6 "broken" "go"))
          (should-not (buffer-live-p created-buffer))
          (should (equal (list new-room) stopped))
          (should (eq :json-false (plist-get (car sent) :ok)))
          (should (string-match-p "Could not queue"
                                  (plist-get (car sent) :message))))))))

(provide 'test-mevedel-collaboration-owner)
;;; test-mevedel-collaboration-owner.el ends here
