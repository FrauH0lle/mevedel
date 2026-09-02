;;; test-mevedel-collaboration-guest.el --- Guest collaboration protocol tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests guest snapshot delivery, remote interactions, untrusted input, and
;; relay callback handling.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-agent-control)
(require 'mevedel-collaboration-agent)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-task)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-owner)
(require 'mevedel-pending-inputs)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-chat)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-input-files)
(require 'mevedel-view-render)
(require 'mevedel-workspace)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-ui)


;;
;;; Snapshot delivery

(defun test-mevedel-collaboration-guest--chunk-frame-bytes (chunk)
  "Return the encoded size of the snapshot frame carrying CHUNK."
  (string-bytes
   (mevedel-collaboration--json-string
    (list :t "snapshot-chunk" :records (vconcat chunk) :final t))))

(mevedel-deftest mevedel-collaboration--snapshot-chunks ()
  ,test
  (test)
  :doc "splits records into chunks each under the wire bound"
  (progn
    (should-not (mevedel-collaboration--snapshot-chunks nil))
    (let* ((record (list :id "assistant-x" :kind "assistant" :revision 0
                         :text (make-string 100000 937)))
           (chunks (mevedel-collaboration--snapshot-chunks
                    (make-list 12 record))))
      (should (> (length chunks) 1))
      (should (= 12 (apply #'+ (mapcar #'length chunks))))
      (dolist (chunk chunks)
        (should (<= (test-mevedel-collaboration-guest--chunk-frame-bytes chunk)
                    mevedel-collaboration--max-message-bytes)))))

  :doc "bounds the frame it sends, separators included"
  ;; Enough tiny records that the separators alone carry a chunk over the
  ;; bound: summing record sizes admits about 21,400 of these, whose frame
  ;; is some 20 KiB larger than the sum says.
  (let* ((record (list :id "t" :kind "user" :revision 0 :text "hi"))
         (chunks (mevedel-collaboration--snapshot-chunks
                  (make-list 30000 record))))
    (should (= 30000 (apply #'+ (mapcar #'length chunks))))
    (dolist (chunk chunks)
      (should (<= (test-mevedel-collaboration-guest--chunk-frame-bytes chunk)
                  mevedel-collaboration--max-message-bytes))))

  :doc "keeps a worst-case escaped record inside one frame"
  ;; The raw-text bound reserves room for escaping plus the record's own
  ;; keys, so a maximal record of six-fold expanding characters must still
  ;; travel rather than be dropped.
  (let* ((record (list :id "esc" :kind "assistant" :revision 0
                       :text (make-string
                              mevedel-collaboration--max-record-text-bytes
                              ?\C-a)))
         (chunks (mevedel-collaboration--snapshot-chunks (list record))))
    (should (= 1 (apply #'+ (mapcar #'length chunks))))
    (should (<= (test-mevedel-collaboration-guest--chunk-frame-bytes (car chunks))
                mevedel-collaboration--max-message-bytes)))

  :doc "drops a record too large to travel in a frame of its own"
  (let* ((oversized (list :id "huge" :kind "assistant" :revision 0
                          :text (make-string
                                 mevedel-collaboration--max-message-bytes
                                 ?x)))
         (small (list :id "small" :kind "user" :revision 0 :text "hi"))
         (chunks (mevedel-collaboration--snapshot-chunks
                  (list small oversized small))))
    (should (= 2 (apply #'+ (mapcar #'length chunks))))
    (dolist (chunk chunks)
      (should (<= (test-mevedel-collaboration-guest--chunk-frame-bytes chunk)
                  mevedel-collaboration--max-message-bytes)))))

(mevedel-deftest mevedel-collaboration--send-snapshot
  (:doc "sends a targeted welcome then final-flagged snapshot chunks")
  (let* ((guests (make-hash-table :test #'eql))
         (records (list (list :id "u" :kind "user" :revision 0 :text "hi")))
         (room (list :transport 'transport :guests guests :records records))
         sent)
    (puthash 7 (list :name "g" :writable t :ready t) guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (mevedel-collaboration--send-snapshot room 7))
    (setq sent (nreverse sent))
    (should (equal '(7 7) (mapcar #'car sent)))
    (let ((welcome (cdr (nth 0 sent)))
          (chunk (cdr (nth 1 sent))))
      (should (equal "welcome" (plist-get welcome :t)))
      (should (eq :json-false (plist-get welcome :readOnly)))
      (should (= 1 (plist-get welcome :recordCount)))
      (should (equal "snapshot-chunk" (plist-get chunk :t)))
      (should (eq t (plist-get chunk :final)))
      (should (= 1 (length (plist-get chunk :records)))))
    ;; The invocation roster rides the welcome for writable guests only.
    (let ((mevedel-collaboration-guest-skills '("plan")))
      (setq sent nil)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent)
                   t))
                ((symbol-function 'mevedel-collaboration--guest-roster)
                 (lambda (_room) '((("name" . "plan") ("kind" . "command"))))))
        (mevedel-collaboration--send-snapshot room 7)
        (should (equal "plan"
                       (cdr (assoc "name"
                                   (aref (plist-get (cdr (car (last sent)))
                                                    :commands)
                                         0)))))
        (puthash 8 (list :name "viewer" :writable nil :ready t) guests)
        (setq sent nil)
        (mevedel-collaboration--send-snapshot room 8)
        (should-not (plist-member (cdr (car (last sent))) :commands))))))


;;
;;; Inbound guest frames

(mevedel-deftest mevedel-collaboration--handle-hello
  (:doc "rejects a protocol mismatch and classifies write-token possession")
  (let* ((guests (make-hash-table :test #'eql))
         (token (mevedel-collaboration--random-bytes 16))
         (owner-token (mevedel-collaboration--random-bytes 16))
         (room (list :transport 'transport :guests guests
                     :write-token token :owner-token owner-token
                     :records nil
                     :ui-requests (make-hash-table :test #'eql)))
         sent)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (mevedel-collaboration--handle-hello room 3 (list :proto 999 :name "x"))
      (should (equal "error" (plist-get (cdr (car sent)) :t)))
      (should (= 0 (hash-table-count guests)))
      (setq sent nil)
      (mevedel-collaboration--handle-hello
       room 1 (list :proto mevedel-collaboration--protocol-version
                    :name "Phone"
                    :guestId "phone-guest-id"
                    :writeToken (mevedel-collaboration--base64url token)))
      (mevedel-collaboration--handle-hello
       room 2 (list :proto mevedel-collaboration--protocol-version
                    :name "Laptop\n<evil>"
                    :guestId "in valid!"
                    :writeToken (mevedel-collaboration--base64url
                                 (mevedel-collaboration--random-bytes 16))))
      (let ((writer (gethash 1 guests))
            (viewer (gethash 2 guests)))
        (should (plist-get writer :writable))
        (should (equal "Phone" (plist-get writer :name)))
        ;; The stable guest identity survives from hello; a malformed
        ;; one is dropped rather than stored.
        (should (equal "phone-guest-id" (plist-get writer :guest-id)))
        (should-not (plist-get viewer :guest-id))
        (should-not (plist-get viewer :writable))
        (should-not (plist-get writer :owner))
        (should (equal "Laptop <evil>" (plist-get viewer :name))))
      (let ((welcomes (cl-remove-if-not
                       (lambda (entry)
                         (equal "welcome" (plist-get (cdr entry) :t)))
                       sent)))
        (should (= 2 (length welcomes))))
      ;; The roster broadcast is latched on change, so both joining
      ;; guests -- the view link included -- are told the current one
      ;; directly, even when it is empty.
      (let ((agents (cl-remove-if-not
                     (lambda (entry)
                       (equal "agents" (plist-get (cdr entry) :t)))
                     sent)))
        (should (equal '(1 2) (sort (mapcar #'car agents) #'<)))
        (should (equal [] (plist-get (cdr (car agents)) :agents))))
      ;; The task list is latched the same way, so both guests are told
      ;; the current one directly.
      (let ((tasks (cl-remove-if-not
                    (lambda (entry)
                      (equal "tasks" (plist-get (cdr entry) :t)))
                    sent)))
        (should (equal '(1 2) (sort (mapcar #'car tasks) #'<)))
        (should (equal [] (plist-get (cdr (car tasks)) :tasks)))))))


(mevedel-deftest mevedel-collaboration--handle-push-subscription
  (:doc "forwards valid subscriptions and removal only for an authenticated guest")
  (let* ((guests (make-hash-table :test #'eql))
         (push-guests (make-hash-table :test #'equal))
         (room (list :transport 'transport :guests guests
                     :push-guests push-guests))
         controls)
    (puthash 1 (list :name "Phone" :ready t :writable t
                     :guest-id "phone-guest-id")
             guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-control)
               (lambda (_transport control)
                 (push control controls)
                 t)))
      (mevedel-collaboration--handle-push-subscription
       room 1 (list :t "push-subscribe"
                    :endpoint "https://push.example/subscription"
                    :active t))
      (should (equal '(:t "push-subscribe" :peer 1
                       :guestId "phone-guest-id"
                       :endpoint "https://push.example/subscription"
                       :active t)
                     (car controls)))
      (should
       (equal '(:endpoint "https://push.example/subscription" :writable t)
              (gethash "phone-guest-id" push-guests)))
      (setq controls nil)
      (mevedel-collaboration--handle-push-subscription
       room 1 (list :t "push-state" :active :json-false))
      (should (equal '(:t "push-state" :peer 1
                       :guestId "phone-guest-id" :active :json-false)
                     (car controls)))
      (setq controls nil)
      (mevedel-collaboration--handle-push-subscription
       room 1 (list :t "push-subscribe"
                    :endpoint "http://internal.example/subscription"))
      (mevedel-collaboration--handle-push-subscription
       room 9 (list :t "push-subscribe"
                    :endpoint "https://push.example/subscription"))
      (should-not controls)
      (mevedel-collaboration--handle-push-subscription
       room 1 (list :t "push-unsubscribe"))
      (should (equal '(:t "push-unsubscribe" :guestId "phone-guest-id")
                     (car controls)))
      (should-not (gethash "phone-guest-id" push-guests)))))

(mevedel-deftest mevedel-collaboration--push-endpoint-p
  (:doc "accepts only bounded HTTPS endpoint strings")
  (let ()
    (should (mevedel-collaboration--push-endpoint-p
             "https://push.example/subscription"))
    (should-not (mevedel-collaboration--push-endpoint-p
                 "http://push.example/subscription"))
    (should-not (mevedel-collaboration--push-endpoint-p
                 (concat "https://push.example/" (make-string 2048 ?x))))
    (should-not (mevedel-collaboration--push-endpoint-p 42))))

(mevedel-deftest mevedel-collaboration--push-guests
  (:doc "sends one targeted relay wake control for nonempty guest ids")
  (let (controls)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-control)
               (lambda (_transport control) (push control controls))))
      (mevedel-collaboration--push-guests '(:transport transport) nil)
      (should-not controls)
      (mevedel-collaboration--push-guests
       '(:transport transport) '("phone-guest-id")))
    (should (equal '(:t "push" :guestIds ["phone-guest-id"])
                   (car controls)))))

(mevedel-deftest mevedel-collaboration--push-writable-guests
  (:doc "targets subscribed writable guests including disconnected ones")
  (let ((subscriptions (make-hash-table :test #'equal))
        targeted)
    (puthash "phone-guest-id" '(:writable t) subscriptions)
    (puthash "viewer-guest-id" '(:writable nil) subscriptions)
    (cl-letf (((symbol-function 'mevedel-collaboration--push-guests)
               (lambda (_room guest-ids) (setq targeted guest-ids))))
      (mevedel-collaboration--push-writable-guests
       (list :push-guests subscriptions)))
    (should (equal '("phone-guest-id") targeted))))

(mevedel-deftest mevedel-collaboration--restore-push-subscriptions
  (:doc "replays remembered endpoints as inactive after relay reconnect")
  (let ((subscriptions (make-hash-table :test #'equal))
        controls)
    (puthash "phone-guest-id"
             '(:endpoint "https://push.example/subscription" :writable t)
             subscriptions)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-control)
               (lambda (_transport control) (push control controls))))
      (mevedel-collaboration--restore-push-subscriptions
       (list :transport 'transport :push-guests subscriptions)))
    (should (equal '(:t "push-subscribe" :peer 0
                     :guestId "phone-guest-id"
                     :endpoint "https://push.example/subscription"
                     :active :json-false)
                   (car controls)))))

(mevedel-deftest mevedel-collaboration--handle-prompt--media-cleanup
  (:doc "leaves no saved image behind when the prompt is not queued")
  (let* ((guests (make-hash-table :test #'eql))
         (root (file-name-as-directory
                (make-temp-file "mevedel-guest-media-" t)))
         (data-buffer (generate-new-buffer " *collab-media-data*"))
         (view-buffer (generate-new-buffer " *collab-media-view*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport))
         (image (list :mime "image/png"
                      :data (base64-encode-string "\211PNG\r\n" t))))
    (unwind-protect
        (progn
          (puthash 1 (list :name "Phone" :writable t :ready t) guests)
          (with-current-buffer data-buffer
            ;; A live view but no session: the real enqueue refuses, which
            ;; is the path that used to leave the media behind.
            (setq-local mevedel--view-buffer view-buffer))
          (cl-letf (((symbol-function 'mevedel-view--media-dir)
                     (lambda () root))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (&rest _) t)))
            (mevedel-collaboration--handle-prompt
             room 1 (list :text "look at this" :images (list image))))
          (should-not (directory-files root nil "\\`guest-"))
          ;; The duplicate latch must not have swallowed the retry either,
          ;; since nothing was actually queued.
          (should-not (plist-get (gethash 1 guests) :last-prompt)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--handle-prompt--media-failure
  (:doc "keeps the room when a guest image cannot be written")
  (let* ((guests (make-hash-table :test #'eql))
         (root (file-name-as-directory
                (make-temp-file "mevedel-guest-media-fail-" t)))
         (data-buffer (generate-new-buffer " *collab-fail-data*"))
         (view-buffer (generate-new-buffer " *collab-fail-view*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport))
         (image (list :mime "image/png"
                      :data (base64-encode-string "PNGDATA" t)))
         (writes 0)
         diagnostics)
    (unwind-protect
        (progn
          (puthash 1 (list :name "Phone" :writable t :ready t) guests)
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer)
            (setq-local mevedel--session
                        (mevedel-session--create :name "share")))
          ;; The suite preloads the handler's owners before `write-region'
          ;; is stubbed.
          (mevedel-test--with-captured-diagnostics diagnostics
            (cl-letf* (((symbol-function 'mevedel-view--media-dir)
                        (lambda () root))
                       (real (symbol-function 'write-region))
                       ((symbol-function 'write-region)
                        (lambda (&rest args)
                          ;; The set is attached whole or not at all, so the
                          ;; first image must not survive the second's
                          ;; failure.
                          (if (= (cl-incf writes) 1)
                              (apply real args)
                            (error "Disk full"))))
                       ((symbol-function
                         'mevedel-collaboration--transport-send)
                        (lambda (&rest _) t)))
              ;; A failed write must not reach the frame handler, which
              ;; treats an error as an observer failure and stops the room.
              (mevedel-collaboration--handle-prompt
               room 1 (list :text "look" :images (list image image)))))
          (should (string-match-p "could not be saved" diagnostics))
          (should (= 2 writes))
          (should-not (directory-files root nil "\\`guest-"))
          (should-not (plist-get (gethash 1 guests) :last-prompt)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--handle-prompt
  (:doc "queues a writable guest prompt and ignores unauthorized or invalid ones")
  (let* ((guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-prompt-data*"))
         (view-buffer (generate-new-buffer " *collab-prompt-view*"))
         (room (list :data-buffer data-buffer :guests guests
                     :transport 'transport))
         enqueued rebuilt drained sent granted)
    (unwind-protect
        (progn
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "phone-guest-id")
                   guests)
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer)
            ;; A real struct: the turn-count accessor is a defsubst that
            ;; load order may have inlined beyond a stub's reach.
            (setq-local mevedel--session
                        (mevedel-session--create :name "share")))
          (cl-letf (((symbol-function 'mevedel-session-enqueue-pending-input)
                     (lambda (_session category entry)
                       (setq enqueued (cons category entry))
                       (plist-put (copy-sequence entry) :id 7)))
                    ((symbol-function 'mevedel--normalize-message-text)
                     (lambda (text) (string-trim text)))
                    ((symbol-function 'mevedel-view--interaction-rebuild)
                     (lambda () (setq rebuilt t)))
                    ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
                     (lambda () (setq drained t)))
                    ((symbol-function 'mevedel-view--pop-dropped-file-grants-for-input)
                     (lambda (_input _session) '(grant)))
                    ((symbol-function 'mevedel-session-add-dropped-file-grant)
                     (lambda (_session path) (push path granted)))
                    ((symbol-function 'mevedel-collaboration--save-guest-attachments)
                     (lambda (images)
                       (when images '("/tmp/media/guest-1.jpg"))))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent)
                       t)))
            (mevedel-collaboration--handle-prompt
             room 1 (list :t "prompt" :text " check the tests \n"
                          :name "DonHugo\n!"
                          :images (list (list :mime "image/jpeg"
                                              :data "ignored")))))
          (should (equal 'follow-up (car enqueued)))
          ;; Attached images become @file mentions with read grants.
          (should (equal "check the tests @file:/tmp/media/guest-1.jpg"
                         (plist-get (cdr enqueued) :input)))
          (should (equal '("/tmp/media/guest-1.jpg") granted))
          (should (equal '(grant)
                         (plist-get (cdr enqueued) :dropped-file-grants)))
          ;; The prompt frame's name refreshes the hello-time default.
          (should (equal "DonHugo !" (plist-get (cdr enqueued) :guest-name)))
          (should (equal "DonHugo !" (plist-get (gethash 1 guests) :name)))
          (should (numberp (plist-get (cdr enqueued) :queued-at-turn)))
          ;; The entry remembers who queued it and which files it owns,
          ;; which is what the retract seam checks against.
          (should (equal "phone-guest-id"
                         (plist-get (cdr enqueued) :guest-id)))
          (should (equal '("/tmp/media/guest-1.jpg")
                         (plist-get (cdr enqueued) :guest-paths)))
          ;; The guest gets a targeted queued acknowledgement carrying
          ;; the entry id its retract control needs.
          (should (equal '(1 . (:t "queued" :id 7)) (car sent)))
          (should rebuilt)
          (should drained)
          ;; A byte-identical repeat within the duplicate window is a
          ;; double-fired client submit and is dropped.
          (setq enqueued nil sent nil)
          (cl-letf (((symbol-function 'mevedel-session-enqueue-pending-input)
                     (lambda (_session category entry)
                       (setq enqueued (cons category entry))))
                    ((symbol-function 'mevedel--normalize-message-text)
                     (lambda (text) (string-trim text)))
                    ((symbol-function 'mevedel-view--interaction-rebuild)
                     (lambda () nil))
                    ((symbol-function 'mevedel-view--schedule-late-follow-up-drain)
                     (lambda () nil))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (&rest _) t)))
            (mevedel-collaboration--handle-prompt
             room 1 (list :t "prompt" :text " check the tests \n"))
            (should-not enqueued)
            ;; Different text inside the window still goes through.
            (mevedel-collaboration--handle-prompt
             room 1 (list :t "prompt" :text "another question"))
            (should (equal "another question"
                           (plist-get (cdr enqueued) :input))))
          ;; Unauthorized and invalid prompts never reach the queue.
          (let ((rejects nil))
            (puthash 8 (list :name "Viewer" :writable nil :ready t) guests)
            (cl-letf (((symbol-function 'mevedel-session-enqueue-pending-input)
                       (lambda (&rest args) (setq rejects args))))
              ;; Read-only guest.
              (mevedel-collaboration--handle-prompt
               room 8 (list :t "prompt" :text "denied"))
              ;; Unregistered peer.
              (mevedel-collaboration--handle-prompt
               room 9 (list :t "prompt" :text "denied"))
              ;; Oversized, blank, and non-string prompts from a writable
              ;; guest.
              (mevedel-collaboration--handle-prompt
               room 1 (list :t "prompt"
                            :text (make-string
                                   (1+ mevedel-collaboration--max-prompt-bytes)
                                   ?x)))
              (mevedel-collaboration--handle-prompt
               room 1 (list :t "prompt" :text "   "))
              (mevedel-collaboration--handle-prompt
               room 1 (list :t "prompt" :text 42)))
            (should-not rejects)))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer))))

(mevedel-deftest mevedel-collaboration--guest-directive-id
  (:doc "accepts an id the workspace still has and drops anything else")
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "/tmp/collab-dir/" "/tmp/collab-dir/" "collab"))
         (session (mevedel-session-create "main" workspace))
         (room (list :session session)))
    (unwind-protect
        (progn
          (setf (mevedel-workspace-directives workspace)
                (list (mevedel-directive--create :id "dir-1")))
          (should (equal "dir-1"
                         (mevedel-collaboration--guest-directive-id
                          room (list :directive "dir-1"))))
          ;; A stale filter, a non-string, and no filter at all all send
          ;; to main chat rather than failing the prompt.
          (should-not (mevedel-collaboration--guest-directive-id
                       room (list :directive "dir-gone")))
          (should-not (mevedel-collaboration--guest-directive-id
                       room (list :directive 42)))
          (should-not (mevedel-collaboration--guest-directive-id
                       room (list :text "no filter")))
          ;; A room with no session cannot resolve anything.
          (should-not (mevedel-collaboration--guest-directive-id
                       (list :session nil) (list :directive "dir-1"))))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-collaboration--handle-retract
  (:doc "removes only the sender's own pending entry with its attachments")
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "/tmp/collab-retract/" "/tmp/collab-retract/"
                     "cr"))
         (session (mevedel-session-create "main" workspace))
         (guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-retract-data*"))
         (view-buffer (generate-new-buffer " *collab-retract-view*"))
         (media (make-temp-file "mevedel-retract-media-" nil ".jpg" "img"))
         (room (list :session session :guests guests
                     :data-buffer data-buffer :transport 'transport))
         rebuilt)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--view-buffer view-buffer))
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "phone-guest-id")
                   guests)
          (puthash 2 (list :name "Tablet" :writable t :ready t
                           :guest-id "tablet-guest-id")
                   guests)
          (let ((mine (mevedel-session-enqueue-pending-input
                       session 'follow-up
                       (list :input "mine" :guest-id "phone-guest-id"
                             :guest-paths (list media))))
                (theirs (mevedel-session-enqueue-pending-input
                         session 'follow-up
                         (list :input "theirs"
                               :guest-id "tablet-guest-id"))))
            (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                       (lambda () (setq rebuilt t)))
                      ((symbol-function
                        'mevedel-collaboration--transport-send)
                       (lambda (&rest _) t)))
              ;; Another guest cannot retract it, nor can a bogus id.
              (mevedel-collaboration--handle-retract
               room 2 (list :id (plist-get mine :id)))
              (mevedel-collaboration--handle-retract
               room 1 (list :id 999))
              (should (= 2 (length
                            (mevedel-session-pending-follow-ups session))))
              (should (file-exists-p media))
              ;; An entry the drain is delivering right now is no longer
              ;; retractable: its files are about to be read mid-turn.
              (plist-put mine :delivering t)
              (mevedel-collaboration--handle-retract
               room 1 (list :id (plist-get mine :id)))
              (should (= 2 (length
                            (mevedel-session-pending-follow-ups session))))
              (should (file-exists-p media))
              (plist-put mine :delivering nil)
              ;; The owner can, and the attachment leaves with it.
              (mevedel-collaboration--handle-retract
               room 1 (list :id (plist-get mine :id)))
              (should (equal (list theirs)
                             (mevedel-session-pending-follow-ups session)))
              (should-not (file-exists-p media))
              (should rebuilt))))
      (when (file-exists-p media) (delete-file media))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-collaboration--save-guest-attachments
  (:doc "saves accepted types within budget and drops invalid sets whole")
  (let ((dir (make-temp-file "mevedel-guest-images" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--media-dir)
                   (lambda () dir)))
          (let ((paths (mevedel-collaboration--save-guest-attachments
                        (list (list :mime "image/jpeg"
                                    :data (base64-encode-string "jpegbytes"))
                              (list :mime "image/png"
                                    :data (base64-encode-string "pngbytes"))))))
            (should (= 2 (length paths)))
            (should (string-suffix-p ".jpg" (nth 0 paths)))
            (should (string-suffix-p ".png" (nth 1 paths)))
            (should (equal "jpegbytes"
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally (nth 0 paths))
                             (buffer-string)))))
          ;; A text attachment rides the same path; Read decides text
          ;; or media from the extension downstream.
          (let ((paths (mevedel-collaboration--save-guest-attachments
                        (list (list :mime "text/x-patch"
                                    :data (base64-encode-string "--- a\n"))))))
            (should (= 1 (length paths)))
            (should (string-suffix-p ".patch" (car paths))))
          ;; Unknown type, malformed data, over-budget, and too many
          ;; attachments each drop the whole set.
          (should-not (mevedel-collaboration--save-guest-attachments
                       (list (list :mime "image/svg+xml"
                                   :data (base64-encode-string "x")))))
          (should-not (mevedel-collaboration--save-guest-attachments
                       (list (list :mime "image/png" :data "not base64!"))))
          (should-not (mevedel-collaboration--save-guest-attachments
                       (list (list :mime "image/png"
                                   :data (base64-encode-string
                                          (make-string
                                           (1+ mevedel-collaboration--max-attachment-bytes)
                                           ?x))))))
          (should-not (mevedel-collaboration--save-guest-attachments
                       (make-list
                        (1+ mevedel-collaboration--max-prompt-attachments)
                        (list :mime "image/png"
                              :data (base64-encode-string "x")))))
          (should-not (mevedel-collaboration--save-guest-attachments nil)))
      (delete-directory dir t))))

(mevedel-deftest mevedel-collaboration--guest-invocable-p
  (:doc "accepts allowlisted names and refuses unsafe ones outright")
  (let ((mevedel-collaboration-guest-skills '("plan" "review" "mode")))
    (should (mevedel-collaboration--guest-invocable-p "plan"))
    (should (mevedel-collaboration--guest-invocable-p "review"))
    ;; Listed but unsafe: `mode' reaches full-auto, so the deny-set wins
    ;; over a mistaken allowlist entry.
    (should-not (mevedel-collaboration--guest-invocable-p "mode"))
    (should-not (mevedel-collaboration--guest-invocable-p "compact"))
    (should-not (mevedel-collaboration--guest-invocable-p nil))))

(mevedel-deftest mevedel-collaboration--handle-prompt--invoke
  (:doc "queues an allowlisted invocation by name and refuses anything else")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests
                       :transport 'transport :session 'session))
           (mevedel-collaboration-guest-skills '("plan" "mode"))
           enqueued sent)
      (puthash 1 (list :name "Phone" :writable t :ready t
                       :guest-id "phone-guest-id")
               guests)
      (puthash 2 (list :name "Viewer" :writable nil :ready t) guests)
      (setq-local mevedel--view-buffer (current-buffer))
      (cl-letf (((symbol-function 'mevedel-view-enqueue-external-follow-up)
                 (lambda (_data-buffer text &rest keys)
                   (setq enqueued (cons text keys))
                   (list :id 9 :input text)))
                ((symbol-function 'mevedel-collaboration--queue-position)
                 (lambda (&rest _) 1))
                ((symbol-function 'mevedel-collaboration--publish-queue)
                 (lambda (_room) nil))
                ((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent)
                   t)))
        ;; Unlisted, unsafe-listed, and read-only guests are all refused;
        ;; free text is never scanned for a sigil.
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "compact" :text ""))
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "mode" :text "full-auto"))
        (mevedel-collaboration--handle-prompt
         room 2 (list :invoke "plan" :text ""))
        (should-not enqueued)
        ;; An allowlisted invocation queues by name, with the text as its
        ;; arguments and skill planning left inert.
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text "add a retry cap"))
        (should (equal "add a retry cap" (car enqueued)))
        (should (equal "plan" (plist-get (cdr enqueued) :invoke)))
        (should (equal "phone-guest-id"
                       (plist-get (cdr enqueued) :guest-id)))
        (should (equal '(1 . (:t "queued" :id 9 :position 1)) (car sent)))
        ;; A bare invocation carries no arguments at all.
        (setq enqueued nil)
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text ""))
        (should (equal "" (car enqueued)))
        ;; A double-fired tap inside the duplicate window is one send.
        (setq enqueued nil)
        (mevedel-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text ""))
        (should-not enqueued)))))

(mevedel-deftest mevedel-collaboration--guest-roster
  (:doc "describes each allowlisted name with its namespace and hint")
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create :name "roster"))
           (room (list :data-buffer data-buf :session session))
           ;; `remember' is a skill and only a skill; `review' is
           ;; installed into the command alist at runtime by its own
           ;; module, so it would resolve as a command here.
           (mevedel-collaboration-guest-skills
            '("plan" "remember" "mode" "nonexistent")))
      (with-current-buffer data-buf
        (setq-local mevedel--view-buffer view-buf))
      (cl-letf (((symbol-function 'mevedel-skills-user-visible-skills)
                 (lambda (&rest _)
                   (list (mevedel-skill--create
                          :name "remember" :argument-hint "[focus]")))))
        (let ((roster (mevedel-collaboration--guest-roster room)))
          ;; A local command and a skill are both offered, each tagged
          ;; with the namespace the viewer needs to render its sigil.
          (should (equal '("plan" "remember")
                         (mapcar (lambda (e) (cdr (assoc "name" e))) roster)))
          (should (equal '("command" "skill")
                         (mapcar (lambda (e) (cdr (assoc "kind" e))) roster)))
          (should (equal "[prompt]" (cdr (assoc "hint" (nth 0 roster)))))
          (should (equal "[focus]" (cdr (assoc "hint" (nth 1 roster)))))
          ;; Unsafe and unresolvable names never become buttons.
          (should-not (assoc "mode" roster))
          (should-not (assoc "nonexistent" roster)))))))

(mevedel-deftest mevedel-collaboration--handle-abort ()
  ,test
  (test)
  :doc "aborts for a writable guest and ignores read-only guests"
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests))
           aborted)
      (puthash 1 (list :name "Viewer" :writable nil :ready t) guests)
      (puthash 2 (list :name "Writer" :writable t :ready t) guests)
      (cl-letf (((symbol-function 'mevedel-view--abort-data-buffer)
                 (lambda (buffer) (setq aborted buffer))))
        (mevedel-collaboration--handle-abort room 1)
        (should-not aborted)
        (mevedel-collaboration--handle-abort room 2)
        (should (eq (current-buffer) aborted)))))

  :doc "guest abort cancels a slow prompt hook before request dispatch"
  (mevedel-view-test--with-buffers
    (let* ((guests (make-hash-table :test #'eql))
           (session (mevedel-session--create :name "share"))
           (room (list :data-buffer data-buf :guests guests))
           late-callback submission send-called)
      (puthash 1 (list :name "Writer" :writable t :ready t) guests)
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    mevedel--workspace nil
                    mevedel--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (setq late-callback callback)))
                ((symbol-function 'mevedel-view--abort-data-buffer) #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (mevedel-view--run-prompt-submit-hook
           "guest prompt" "guest prompt"
           (lambda (_accepted) (gptel-send)))
          (setq submission mevedel-view--prompt-hook-pending))
        (mevedel-collaboration--handle-abort room 1)
        (funcall late-callback nil))
      (should (eq 'cancelled
                  (mevedel-prompt-submission-state submission)))
      (should-not send-called)
      (with-current-buffer data-buf
        (should (string-empty-p (buffer-string)))))))


;;
;;; Relay callbacks

(mevedel-deftest mevedel-collaboration--on-frame
  (:doc "dispatches known frames and stops the room on handler failure")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           handled stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--handle-hello)
                 (lambda (_room peer _frame) (setq handled peer))))
        (mevedel-collaboration--on-frame (current-buffer) 5
                                         (list :t "hello" :proto 2)))
      (should (= 5 handled))
      ;; Unknown frame types are tolerated.
      (mevedel-collaboration--on-frame (current-buffer) 5
                                       (list :t "future-frame"))
      (cl-letf (((symbol-function 'mevedel-collaboration--handle-prompt)
                 (lambda (&rest _) (error "Handler fault")))
                ((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (_room reason) (setq stopped reason)))
                ((symbol-function 'display-warning) (lambda (&rest _) nil)))
        (mevedel-collaboration--on-frame (current-buffer) 5
                                         (list :t "prompt" :text "x"))
        (should (eq 'observer-failure stopped))))))

(mevedel-deftest mevedel-collaboration--on-control
  (:doc "drops a guest on peer-left and waits for hello on peer-joined")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room)))
      (mevedel-collaboration--on-control (current-buffer) 'peer-joined 1)
      (should (= 0 (hash-table-count guests)))
      (puthash 1 (list :name "g") guests)
      (mevedel-collaboration--on-control (current-buffer) 'peer-left 1)
      (should (= 0 (hash-table-count guests))))))

(mevedel-deftest mevedel-collaboration--on-state ()
  ,test
  (test)
  :doc "clears guests and restores push endpoints on reconnect"
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (push-guests (make-hash-table :test #'equal))
           (room (list :data-buffer (current-buffer) :guests guests
                       :push-guests push-guests :transport 'transport))
           (mevedel-collaboration--rooms (mevedel-test-room-registry room))
           controls)
      (puthash 1 (list :name "g") guests)
      (puthash "phone-guest-id"
               '(:endpoint "https://push.example/subscription" :writable t)
               push-guests)
      (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) nil)))
        (mevedel-collaboration--on-state (current-buffer) 'down))
      (should (= 0 (hash-table-count guests)))
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-control)
                 (lambda (_transport control) (push control controls))))
        (mevedel-collaboration--on-state (current-buffer) 'open))
      (should (equal '(:t "push-subscribe" :peer 0
                       :guestId "phone-guest-id"
                       :endpoint "https://push.example/subscription"
                       :active :json-false)
                     (car controls)))
      (mevedel-collaboration--on-state (current-buffer) 'stopped)))

  :doc "warns once when a relay dial never opens, but not after it has"
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)))
           (mevedel-collaboration--rooms
            (mevedel-test-room-registry room))
           warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type text &rest _) (push text warnings))))
        ;; A share whose dial never succeeded is not live; say so once.
        (mevedel-collaboration--on-state (current-buffer) 'down)
        (mevedel-collaboration--on-state (current-buffer) 'down)
        (should (= 1 (length warnings)))
        (should (string-match-p "not live" (car warnings)))
        ;; An ordinary drop after a working connection stays quiet.
        (setq warnings nil)
        (mevedel-collaboration--on-state (current-buffer) 'open)
        (mevedel-collaboration--on-state (current-buffer) 'down)
        (should-not warnings)))))

(provide 'test-mevedel-collaboration-guest)
;;; test-mevedel-collaboration-guest.el ends here
