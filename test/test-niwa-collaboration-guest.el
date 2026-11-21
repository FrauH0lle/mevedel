;;; test-niwa-collaboration-guest.el --- Guest collaboration protocol tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests guest snapshot delivery, remote interactions, untrusted input, and
;; relay callback handling.

;;; Code:

(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'niwa-collaboration-relay-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "niwa-collaboration-relay-test-support"))
(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'cl-lib)
(require 'gptel)
(require 'niwa-agent-control)
(require 'niwa-collaboration-agent)
(require 'niwa-collaboration-artifact)
(require 'niwa-collaboration-discussions)
(require 'niwa-collaboration-files)
(require 'niwa-collaboration-history)
(require 'niwa-collaboration-presence)
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-task)
(require 'niwa-collaboration-transport)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-lobby)
(require 'niwa-collaboration-owner)
(require 'niwa-pending-inputs)
(require 'niwa-permission-mode)
(require 'niwa-prompt-submission)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-sleep)
(require 'niwa-structs)
(require 'niwa-transcript)
(require 'niwa-transcript-audit)
(require 'niwa-chat)
(require 'niwa-view)
(require 'websocket)
(require 'niwa-view-composer)
(require 'niwa-view-input-files)
(require 'niwa-view-render)
(require 'niwa-workspace)
(require 'niwa-skills-invoke)
(require 'niwa-skills-ui)


;;
;;; Snapshot delivery

(defun test-niwa-collaboration-guest--chunk-frame-bytes (chunk)
  "Return the encoded size of the snapshot frame carrying CHUNK."
  (string-bytes
   (json-encode (niwa-collaboration--records-frame (list :t "snapshot-chunk") chunk nil))))

(niwa-deftest niwa-collaboration--snapshot-chunks ()
  ,test
  (test)
  :doc "splits records into chunks each under the wire bound"
  (progn
    (should (equal '(nil) (niwa-collaboration--snapshot-chunks nil)))
    (let* ((record (list :id "assistant-x" :kind "assistant" :revision 0
                         :text (make-string 100000 937)))
           (chunks (niwa-collaboration--snapshot-chunks
                    (make-list 12 record))))
      (should (> (length chunks) 1))
      (should (= 12 (apply #'+ (mapcar #'length chunks))))
      (dolist (chunk chunks)
        (should (<= (test-niwa-collaboration-guest--chunk-frame-bytes chunk)
                    niwa-collaboration--max-message-bytes)))))

  :doc "bounds the frame it sends, separators included"
  ;; Enough tiny records that the separators alone carry a chunk over the
  ;; bound: summing record sizes admits about 21,400 of these, whose frame
  ;; is some 20 KiB larger than the sum says.
  (let* ((record (list :id "t" :kind "user" :revision 0 :text "hi"))
         (chunks (niwa-collaboration--snapshot-chunks
                  (make-list 30000 record))))
    (should (= 30000 (apply #'+ (mapcar #'length chunks))))
    (dolist (chunk chunks)
      (should (<= (test-niwa-collaboration-guest--chunk-frame-bytes chunk)
                  niwa-collaboration--max-message-bytes))))

  :doc "keeps a worst-case escaped record inside one frame"
  ;; The raw-text bound reserves room for escaping plus the record's own
  ;; keys, so a maximal record of six-fold expanding characters must still
  ;; travel rather than be dropped.
  (let* ((record (list :id "esc" :kind "assistant" :revision 0
                       :text (make-string
                              niwa-collaboration--max-record-text-bytes
                              ?\C-a)))
         (chunks (niwa-collaboration--snapshot-chunks (list record))))
    (should (= 1 (apply #'+ (mapcar #'length chunks))))
    (should (<= (test-niwa-collaboration-guest--chunk-frame-bytes (car chunks))
                niwa-collaboration--max-message-bytes)))

  :doc "drops a record too large to travel in a frame of its own"
  (let* ((oversized (list :id "huge" :kind "assistant" :revision 0
                          :text (make-string
                                 niwa-collaboration--max-message-bytes
                                 ?x)))
         (small (list :id "small" :kind "user" :revision 0 :text "hi"))
         (chunks (niwa-collaboration--snapshot-chunks
                  (list small oversized small))))
    (should (= 2 (apply #'+ (mapcar #'length chunks))))
    (dolist (chunk chunks)
      (should (<= (test-niwa-collaboration-guest--chunk-frame-bytes chunk)
                  niwa-collaboration--max-message-bytes)))))

(niwa-deftest niwa-collaboration--workspace-key
  (:doc "names a room's workspace opaquely, alike for its sessions and lobby")
  (let* ((make (lambda (root)
                 (niwa-workspace--create :type 'project :id root :root root)))
         (here (funcall make "/home/u/proj/"))
         (session (niwa-session--create :workspace here))
         (key (niwa-collaboration--workspace-key (list :session session))))
    (should (stringp key))
    (should-not (string-match-p "proj" key))
    (should (equal key (niwa-collaboration--workspace-key
                        (list :workspace here))))
    (should-not (equal key (niwa-collaboration--workspace-key
                            (list :workspace (funcall make "/home/u/other/")))))
    (should-not (niwa-collaboration--workspace-key nil))))

(niwa-deftest niwa-collaboration--send-snapshot--shared ()
  ,test
  (test)
  :doc "guests joining unchanged records share one encoding; a publish renews it"
  (let* ((guests (make-hash-table :test #'eql))
         (records (list (list :id "u" :kind "user" :revision 0 :text "hi")))
         (room (list :transport 'transport :guests guests :records records))
         (encodings 0)
         (chunk (symbol-function 'niwa-collaboration--snapshot-chunks))
         sent)
    (puthash 7 (list :name "a" :ready t) guests)
    (puthash 8 (list :name "b" :ready t) guests)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport _peer frame) (push frame sent) t))
              ((symbol-function 'niwa-collaboration--snapshot-chunks)
               (lambda (&rest args) (cl-incf encodings) (apply chunk args))))
      (niwa-collaboration--send-snapshot room 7)
      (niwa-collaboration--send-snapshot room 8)
      (should (= 1 encodings))
      (plist-put room :records (list (list :id "u" :kind "user" :revision 1 :text "ho")))
      (setq sent nil)
      (niwa-collaboration--send-snapshot room 7)
      (should (= 2 encodings))
      (should (equal "ho" (plist-get (aref (plist-get (niwa-test--frame (car sent)) :records) 0)
                                     :text)))))
  :doc "an empty room sends a final chunk on its first and subsequent joins"
  (let* ((room (list :transport 'transport :guests (make-hash-table :test #'eql)))
         sent)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport _peer frame) (push (niwa-test--frame frame) sent) t)))
      (dotimes (_ 2)
        (setq sent nil)
        (niwa-collaboration--send-snapshot room 7)
        (should (= 2 (length sent)))
        (should (= 0 (plist-get (cadr sent) :recordCount)))
        (should (equal "snapshot-chunk" (plist-get (car sent) :t)))
        (should (eq t (plist-get (car sent) :final)))
        (should (equal [] (plist-get (car sent) :records)))))))

(niwa-deftest niwa-collaboration--send-snapshot
  (:doc "sends a targeted welcome then final-flagged snapshot chunks")
  (let* ((guests (make-hash-table :test #'eql))
         (records (list (list :id "u" :kind "user" :revision 0 :text "hi")))
         (room (list :transport 'transport :guests guests :records records))
         sent)
    (puthash 7 (list :name "g" :writable t :ready t) guests)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t))
              ((symbol-function 'niwa-model-candidates)
               (lambda () '(("Codex:gpt-6-luna" . provider)))))
      (niwa-collaboration--send-snapshot room 7))
    (setq sent (nreverse sent))
    (should (equal '(7 7) (mapcar #'car sent)))
    (let ((welcome (cdr (nth 0 sent)))
          (chunk (niwa-test--frame (cdr (nth 1 sent)))))
      (should (equal "welcome" (plist-get welcome :t)))
      (should (eq :json-false (plist-get welcome :readOnly)))
      (should (= 1 (plist-get welcome :recordCount)))
      ;; A writable guest may ask for a session, so it learns the models.
      (should (equal ["Codex:gpt-6-luna"] (plist-get welcome :models)))
      (should (equal "snapshot-chunk" (plist-get chunk :t)))
      (should (eq t (plist-get chunk :final)))
      (should (= 1 (length (plist-get chunk :records)))))
    ;; The invocation roster rides the welcome for writable guests only.
    (let ((niwa-collaboration-guest-skills '((full "plan"))))
      (setq sent nil)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent)
                   t))
                ((symbol-function 'niwa-collaboration--guest-roster)
                 (lambda (_room _guest)
                   '((("name" . "plan") ("kind" . "command"))))))
        (niwa-collaboration--send-snapshot room 7)
        (should (equal "plan"
                       (cdr (assoc "name"
                                   (aref (plist-get (cdr (car (last sent)))
                                                    :commands)
                                         0)))))
        (puthash 8 (list :name "viewer" :writable nil :ready t) guests)
        (setq sent nil)
        (niwa-collaboration--send-snapshot room 8)
        (should-not (plist-member (cdr (car (last sent))) :commands))
        (should-not (plist-member (cdr (car (last sent))) :models))))
    ;; An empty room still ends each snapshot, or its guests keep loading.
    (let ((room (list :transport 'transport :guests guests :records nil)))
      (dotimes (_ 2)
        (setq sent nil)
        (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame) (push (cons peer frame) sent) t)))
          (niwa-collaboration--send-snapshot room 8))
        (should (= 2 (length sent)))
        (let ((chunk (niwa-test--frame (cdr (car sent)))))
          (should (eq t (plist-get chunk :final)))
          (should (equal [] (plist-get chunk :records))))))))

(defun test-niwa-collaboration-guest--workspace (root &optional name)
  "Return a project workspace at ROOT, displayed as NAME when given."
  (niwa-workspace--create :type 'project :id root :root root :name name))

(niwa-deftest niwa-collaboration--project-name
  (:doc "names a project by its workspace name, else its directory")
  (progn
    (should (equal "Niwa" (niwa-collaboration--project-name
                              (test-niwa-collaboration-guest--workspace
                               "/home/u/proj/" "Niwa"))))
    (should (equal "proj" (niwa-collaboration--project-name
                           (test-niwa-collaboration-guest--workspace
                            "/home/u/proj/"))))))

(niwa-deftest niwa-collaboration--lobby-field
  (:doc "an owner learns the running lobby's link, others only that it runs")
  (let* ((workspace (test-niwa-collaboration-guest--workspace "/home/u/proj/"))
         (room (list :session (niwa-session--create :workspace workspace)))
         (lobby (list :workspace workspace :link-owner "owner-link"))
         (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal)))
    ;; Nothing runs, and nothing is started to answer.
    (should (eq :json-false (niwa-collaboration--lobby-field
                             room (list :owner t :writable t))))
    (should (zerop (hash-table-count niwa-collaboration-lobby--lobbies)))
    (puthash "/home/u/proj/" lobby niwa-collaboration-lobby--lobbies)
    (should (equal "owner-link" (niwa-collaboration--lobby-field
                                 room (list :owner t :writable t))))
    (should (eq t (niwa-collaboration--lobby-field room (list :writable t))))
    (should (eq t (niwa-collaboration--lobby-field room nil)))
    ;; Another project's room learns nothing of this lobby.
    (should (eq :json-false
                (niwa-collaboration--lobby-field
                 (list :session (niwa-session--create
                                 :workspace (test-niwa-collaboration-guest--workspace
                                             "/home/u/other/")))
                 (list :owner t))))
    ;; The lobby itself needs no way back to itself.
    (should (eq :json-false (niwa-collaboration--lobby-field
                             lobby (list :owner t))))))

(niwa-deftest niwa-collaboration--announce-lobby
  (:doc "tells each guest of the workspace's rooms what its tier may know")
  (let* ((workspace (test-niwa-collaboration-guest--workspace "/home/u/proj/"))
         (here (list :transport 'here :guests (make-hash-table :test #'eql)
                     :session (niwa-session--create :workspace workspace)))
         (elsewhere (list :transport 'elsewhere :guests (make-hash-table :test #'eql)
                          :session (niwa-session--create
                                    :workspace (test-niwa-collaboration-guest--workspace
                                                "/home/u/other/"))))
         (niwa-collaboration--rooms (make-hash-table :test #'eq))
         (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         sent)
    (puthash 1 (list :owner t :writable t) (plist-get here :guests))
    (puthash 2 (list :writable t) (plist-get here :guests))
    (puthash 3 (list :owner t :writable t) (plist-get elsewhere :guests))
    (puthash 'here-buffer here niwa-collaboration--rooms)
    (puthash 'elsewhere-buffer elsewhere niwa-collaboration--rooms)
    ;; The lobby itself is told nothing.
    (let ((lobby-room (list :transport 'lobby :workspace workspace
                            :guests (make-hash-table :test #'eql))))
      (puthash 4 (list :owner t) (plist-get lobby-room :guests))
      (puthash 'lobby-buffer lobby-room niwa-collaboration--rooms))
    (puthash "/home/u/proj/" (list :workspace workspace :link-owner "owner-link")
             niwa-collaboration-lobby--lobbies)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (transport peer frame) (push (list transport peer frame) sent) t)))
      (niwa-collaboration--announce-lobby workspace)
      (should (equal '((here 1 (:t "lobby-state" :lobby "owner-link"))
                       (here 2 (:t "lobby-state" :lobby t)))
                     (sort (copy-sequence sent)
                           (lambda (a b) (< (cadr a) (cadr b))))))
      ;; A stopped lobby is announced as gone.
      (setq sent nil)
      (clrhash niwa-collaboration-lobby--lobbies)
      (niwa-collaboration--announce-lobby workspace)
      (should (equal '((:t "lobby-state" :lobby :json-false)
                       (:t "lobby-state" :lobby :json-false))
                     (mapcar #'caddr sent))))
    ;; A failed send still lets the next guest hear it.
    (setq sent nil)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (transport peer frame)
                 (when (eql peer 1) (error "Socket gone"))
                 (push (list transport peer frame) sent) t)))
      (niwa-collaboration--announce-lobby workspace)
      (should (equal '(2) (mapcar #'cadr sent))))))

(niwa-deftest niwa-collaboration--send-snapshot--project
  (:doc "the welcome names the project and what the guest may know of its lobby")
  (let* ((workspace (test-niwa-collaboration-guest--workspace "/home/u/proj/" "Niwa"))
         (guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :guests guests :records nil
                     :session (niwa-session--create :workspace workspace)))
         (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         sent)
    (puthash 7 (list :name "o" :owner t :ready t) guests)
    (puthash "/home/u/proj/" (list :workspace workspace :link-owner "owner-link")
             niwa-collaboration-lobby--lobbies)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport _peer frame) (push frame sent) t)))
      (niwa-collaboration--send-snapshot room 7))
    (let ((welcome (car (last sent))))
      (should (equal "welcome" (plist-get welcome :t)))
      (should (equal "Niwa" (plist-get welcome :project)))
      (should (equal "owner-link" (plist-get welcome :lobby))))
    ;; A room without a workspace names no project.
    (setq sent nil)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport _peer frame) (push frame sent) t)))
      (niwa-collaboration--send-snapshot
       (list :transport 'transport :guests guests :records nil) 7))
    (let ((welcome (car (last sent))))
      (should-not (plist-member welcome :project))
      (should (eq :json-false (plist-get welcome :lobby))))))


;;
;;; Inbound guest frames

(niwa-deftest niwa-collaboration--admit-hello
  (:doc "registers a guest at the tier its tokens prove, or refuses it")
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :guests guests
                     :write-token (make-string 16 ?w)
                     :owner-token (make-string 16 ?o)))
         (sleep-updates 0)
         sent)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) sent) t))
              ((symbol-function 'niwa-sleep-update)
               (lambda () (cl-incf sleep-updates))))
      (should-not (niwa-collaboration--admit-hello
                   room 1 '(:proto 4 :name "old")))
      (should (= 0 sleep-updates))
      (should (equal "error" (plist-get (cdar sent) :t)))
      (should (equal "protocol-mismatch" (plist-get (cdar sent) :code)))
      (should (= 5 (plist-get (cdar sent) :proto)))
      (should (string-match-p "Reload the viewer.*update niwa"
                              (plist-get (cdar sent) :message)))
      (should (= 0 (hash-table-count guests)))
      (let ((guest (niwa-collaboration--admit-hello
                    room 2
                    (list :proto niwa-collaboration--protocol-version
                          :name "Phone" :guestId "browser-1"
                          :writeToken (niwa-collaboration--base64url
                                       (make-string 16 ?w))
                          :ownerToken (niwa-collaboration--base64url
                                       (make-string 16 ?o))))))
        (should (eq guest (gethash 2 guests)))
        (should (equal "Phone" (plist-get guest :name)))
        (should (plist-get guest :writable))
        (should (plist-get guest :owner))
        ;; Without a page the guest is on the room itself.
        (should-not (plist-get guest :viewing)))
      ;; The hello carries the guest's page; a malformed one is the room.
      (let ((hello (list :proto niwa-collaboration--protocol-version
                         :name "Tab" :page "board" :active t)))
        (should (equal "board" (plist-get (niwa-collaboration--admit-hello
                                           room 3 hello)
                                          :viewing)))
        (should (eq t (plist-get (gethash 3 guests) :active)))
        (should-not (plist-get (niwa-collaboration--admit-hello
                                room 4 (plist-put (copy-sequence hello) :page "../x"))
                               :viewing)))
      ;; Admission alone sends nothing: what a guest is told depends on
      ;; whether it joined a room or a lobby.
      (should (= 3 sleep-updates))
      (should (= 1 (length sent))))))

(niwa-deftest niwa-collaboration--handle-hello
  (:doc "rejects a protocol mismatch and classifies write-token possession")
  (let* ((guests (make-hash-table :test #'eql))
         (token (niwa-collaboration--random-bytes 16))
         (owner-token (niwa-collaboration--random-bytes 16))
         (room (list :transport 'transport :guests guests
                     :write-token token :owner-token owner-token
                     :records nil
                     :ui-requests (make-hash-table :test #'eql)))
         sent)
    ;; This admission fixture has no session or workspace.  Store projection
    ;; is exercised with real workspaces in the artifact-store tests.
    (cl-letf (((symbol-function 'niwa-collaboration--store-frame)
               (lambda (_room) '(:t "store-artifacts" :artifacts [])))
              ((symbol-function 'niwa-collaboration--transport-send)
               (lambda (_transport peer frame)
                 (push (cons peer frame) sent)
                 t)))
      (niwa-collaboration--handle-hello room 3 (list :proto 999 :name "x"))
      (should (equal "error" (plist-get (cdr (car sent)) :t)))
      (should (= 0 (hash-table-count guests)))
      (setq sent nil)
      (niwa-collaboration--handle-hello
       room 1 (list :proto niwa-collaboration--protocol-version
                    :name "Phone"
                    :guestId "phone-guest-id"
                    :writeToken (niwa-collaboration--base64url token)))
      (niwa-collaboration--handle-hello
       room 2 (list :proto niwa-collaboration--protocol-version
                    :name "Laptop\n<evil>"
                    :guestId "in valid!"
                    :writeToken (niwa-collaboration--base64url
                                 (niwa-collaboration--random-bytes 16))))
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
      (let ((goals (cl-remove-if-not
                    (lambda (entry)
                      (equal "goal" (plist-get (cdr entry) :t)))
                    sent)))
        (should (equal '(1 2) (sort (mapcar #'car goals) #'<)))
        (should-not (alist-get 'goal (json-read-from-string (json-encode (cdar goals))))))
      (let ((tasks (cl-remove-if-not
                    (lambda (entry)
                      (equal "tasks" (plist-get (cdr entry) :t)))
                    sent)))
        (should (equal '(1 2) (sort (mapcar #'car tasks) #'<)))
        (should (equal [] (plist-get (cdr (car tasks)) :tasks)))))))


(niwa-deftest niwa-collaboration--audience-peer-p
  (:doc "narrows a mirrored interaction to owner-link guests")
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :guests guests)))
    (puthash 1 (list :name "Owner" :writable t :owner t
                     :guest-id "owner-id") guests)
    (puthash 2 (list :name "Writer" :writable t :guest-id "writer-id") guests)
    ;; No audience is every writable guest, the ordinary case.
    (should (niwa-collaboration--audience-peer-p room 2 nil))
    (should (niwa-collaboration--audience-peer-p room 1 nil))
    (let ((audience 'owner))
      (should (niwa-collaboration--audience-peer-p room 1 audience))
      (should-not (niwa-collaboration--audience-peer-p room 2 audience))
      ;; Two tabs of one browser share a guest id, so a narrowed
      ;; interaction must turn on the link a peer holds, not on who it
      ;; is: the owner tab sees it while the writer tab does not.
      (puthash 3 (list :name "Owner tab" :writable t :owner t
                       :guest-id "writer-id") guests)
      (should (niwa-collaboration--audience-peer-p room 3 audience))
      ;; An unknown peer is nobody.
      (should-not (niwa-collaboration--audience-peer-p room 42 audience)))
    ;; Unknown audience values fail closed.
    (should-not (niwa-collaboration--audience-peer-p room 1 'writer))))

(niwa-deftest niwa-collaboration--audience-peers
  (:doc "filters the writable peers by the overlay's declared audience")
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :transport 'transport :guests guests)))
    (puthash 1 (list :name "Owner" :writable t :owner t
                     :guest-id "owner-id") guests)
    (puthash 2 (list :name "Writer" :writable t :guest-id "writer-id") guests)
    (puthash 3 (list :name "Reader" :guest-id "reader-id") guests)
    (with-temp-buffer
      (let ((overlay (make-overlay (point-min) (point-min))))
        ;; A plain interaction reaches every writable guest.
        (overlay-put overlay 'niwa--remote '(:body "x"))
        (should (equal '(1 2)
                       (sort (niwa-collaboration--audience-peers
                              room overlay)
                             #'<)))
        (overlay-put overlay 'niwa--remote
                     '(:body "x" :audience owner))
        (should (equal '(1) (niwa-collaboration--audience-peers
                             room overlay)))
        ;; Nobody left is a decision only Emacs can take, not an error.
        (remhash 1 guests)
        (should-not (niwa-collaboration--audience-peers room overlay))))))

(niwa-deftest niwa-collaboration--handle-push-subscription
  (:doc "forwards valid subscriptions and removal only for an authenticated guest")
  (let* ((guests (make-hash-table :test #'eql))
         (push-guests (make-hash-table :test #'equal))
         (room (list :transport 'transport :guests guests
                     :push-guests push-guests))
         controls)
    (puthash 1 (list :name "Phone" :ready t :writable t
                     :guest-id "phone-guest-id")
             guests)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
               (lambda (_transport control)
                 (push control controls)
                 t)))
      (niwa-collaboration--handle-push-subscription
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
      (niwa-collaboration--handle-push-subscription
       room 1 (list :t "push-state" :active :json-false))
      (should (equal '(:t "push-state" :peer 1
                       :guestId "phone-guest-id" :active :json-false)
                     (car controls)))
      (setq controls nil)
      (niwa-collaboration--handle-push-subscription
       room 1 (list :t "push-subscribe"
                    :endpoint "http://internal.example/subscription"))
      (niwa-collaboration--handle-push-subscription
       room 9 (list :t "push-subscribe"
                    :endpoint "https://push.example/subscription"))
      (should-not controls)
      (niwa-collaboration--handle-push-subscription
       room 1 (list :t "push-unsubscribe"))
      (should (equal '(:t "push-unsubscribe" :guestId "phone-guest-id")
                     (car controls)))
      (should-not (gethash "phone-guest-id" push-guests)))))

(niwa-deftest niwa-collaboration--push-endpoint-p
  (:doc "accepts only bounded HTTPS endpoint strings")
  (let ()
    (should (niwa-collaboration--push-endpoint-p
             "https://push.example/subscription"))
    (should-not (niwa-collaboration--push-endpoint-p
                 "http://push.example/subscription"))
    (should-not (niwa-collaboration--push-endpoint-p
                 (concat "https://push.example/" (make-string 2048 ?x))))
    (should-not (niwa-collaboration--push-endpoint-p 42))))

(niwa-deftest niwa-collaboration--push-guests
  (:doc "sends one targeted relay wake control for nonempty guest ids")
  (let (controls)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
               (lambda (_transport control) (push control controls))))
      (niwa-collaboration--push-guests '(:transport transport) nil)
      (should-not controls)
      (niwa-collaboration--push-guests
       '(:transport transport) '("phone-guest-id")))
    (should (equal '(:t "push" :guestIds ["phone-guest-id"])
                   (car controls)))))

(niwa-deftest niwa-collaboration--push-writable-guests
  (:doc "targets subscribed writable guests including disconnected ones")
  (let ((subscriptions (make-hash-table :test #'equal))
        targeted)
    (puthash "phone-guest-id" '(:writable t) subscriptions)
    (puthash "viewer-guest-id" '(:writable nil) subscriptions)
    (cl-letf (((symbol-function 'niwa-collaboration--push-guests)
               (lambda (_room guest-ids) (setq targeted guest-ids))))
      (niwa-collaboration--push-writable-guests
       (list :push-guests subscriptions)))
    (should (equal '("phone-guest-id") targeted))))

(niwa-deftest niwa-collaboration--restore-push-subscriptions
  (:doc "replays remembered endpoints as inactive after relay reconnect")
  (let ((subscriptions (make-hash-table :test #'equal))
        controls)
    (puthash "phone-guest-id"
             '(:endpoint "https://push.example/subscription" :writable t)
             subscriptions)
    (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
               (lambda (_transport control) (push control controls))))
      (niwa-collaboration--restore-push-subscriptions
       (list :transport 'transport :push-guests subscriptions)))
    (should (equal '(:t "push-subscribe" :peer 0
                     :guestId "phone-guest-id"
                     :endpoint "https://push.example/subscription"
                     :active :json-false)
                   (car controls)))))

(niwa-deftest niwa-collaboration--handle-prompt--media-cleanup
  (:doc "leaves no saved image behind when the prompt is not queued")
  (let* ((guests (make-hash-table :test #'eql))
         (root (file-name-as-directory
                (make-temp-file "niwa-guest-media-" t)))
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
            (setq-local niwa--view-buffer view-buffer))
          (cl-letf (((symbol-function 'niwa-view--media-dir)
                     (lambda () root))
                    ((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (&rest _) t)))
            (niwa-collaboration--handle-prompt
             room 1 (list :text "look at this" :images (list image))))
          (should-not (directory-files root nil "\\`guest-"))
          ;; The duplicate latch must not have swallowed the retry either,
          ;; since nothing was actually queued.
          (should-not (plist-get (gethash 1 guests) :last-prompt)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory root t))))

(niwa-deftest niwa-collaboration--handle-prompt--media-failure
  (:doc "keeps the room when a guest image cannot be written")
  (let* ((guests (make-hash-table :test #'eql))
         (root (file-name-as-directory
                (make-temp-file "niwa-guest-media-fail-" t)))
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
            (setq-local niwa--view-buffer view-buffer)
            (setq-local niwa--session
                        (niwa-session--create :name "share")))
          ;; The suite preloads the handler's owners before `write-region'
          ;; is stubbed.
          (niwa-test--with-captured-diagnostics diagnostics
            (cl-letf* (((symbol-function 'niwa-view--media-dir)
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
                         'niwa-collaboration--transport-send)
                        (lambda (&rest _) t)))
              ;; A failed write must not reach the frame handler, which
              ;; treats an error as an observer failure and stops the room.
              (niwa-collaboration--handle-prompt
               room 1 (list :text "look" :images (list image image)))))
          (should (string-match-p "could not be saved" diagnostics))
          (should (= 2 writes))
          (should-not (directory-files root nil "\\`guest-"))
          (should-not (plist-get (gethash 1 guests) :last-prompt)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory root t))))

(niwa-deftest niwa-collaboration--handle-prompt
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
            (setq-local niwa--view-buffer view-buffer)
            ;; A real struct: the turn-count accessor is a defsubst that
            ;; load order may have inlined beyond a stub's reach.
            (setq-local niwa--session
                        (niwa-session--create :name "share")))
          (cl-letf (((symbol-function 'niwa-session-enqueue-pending-input)
                     (lambda (_session category entry)
                       (setq enqueued (cons category entry))
                       (plist-put (copy-sequence entry) :id 7)))
                    ((symbol-function 'niwa--normalize-message-text)
                     (lambda (text) (string-trim text)))
                    ((symbol-function 'niwa-view--interaction-rebuild)
                     (lambda () (setq rebuilt t)))
                    ((symbol-function 'niwa-pending-inputs-schedule-held-input-delivery)
                     (lambda () (setq drained t)))
                    ((symbol-function 'niwa-view--pop-dropped-file-grants-for-input)
                     (lambda (_input _session) '(grant)))
                    ((symbol-function 'niwa-session-add-dropped-file-grant)
                     (lambda (_session path) (push path granted)))
                    ((symbol-function 'niwa-collaboration--save-guest-attachments)
                     (lambda (images)
                       (when images '("/tmp/media/guest-1.jpg"))))
                    ((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent)
                       t)))
            (niwa-collaboration--handle-prompt
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
          (cl-letf (((symbol-function 'niwa-session-enqueue-pending-input)
                     (lambda (_session category entry)
                       (setq enqueued (cons category entry))))
                    ((symbol-function 'niwa--normalize-message-text)
                     (lambda (text) (string-trim text)))
                    ((symbol-function 'niwa-view--interaction-rebuild)
                     (lambda () nil))
                    ((symbol-function 'niwa-pending-inputs-schedule-held-input-delivery)
                     (lambda () nil))
                    ((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (&rest _) t)))
            (niwa-collaboration--handle-prompt
             room 1 (list :t "prompt" :text " check the tests \n"))
            (should-not enqueued)
            ;; Different text inside the window still goes through.
            (niwa-collaboration--handle-prompt
             room 1 (list :t "prompt" :text "another question"))
            (should (equal "another question"
                           (plist-get (cdr enqueued) :input))))
          ;; Unauthorized and invalid prompts never reach the queue.
          (let ((rejects nil))
            (puthash 8 (list :name "Viewer" :writable nil :ready t) guests)
            (cl-letf (((symbol-function 'niwa-session-enqueue-pending-input)
                       (lambda (&rest args) (setq rejects args))))
              ;; Read-only guest.
              (niwa-collaboration--handle-prompt
               room 8 (list :t "prompt" :text "denied"))
              ;; Unregistered peer.
              (niwa-collaboration--handle-prompt
               room 9 (list :t "prompt" :text "denied"))
              ;; Oversized, blank, and non-string prompts from a writable
              ;; guest.
              (niwa-collaboration--handle-prompt
               room 1 (list :t "prompt"
                            :text (make-string
                                   (1+ niwa-collaboration--max-prompt-bytes)
                                   ?x)))
              (niwa-collaboration--handle-prompt
               room 1 (list :t "prompt" :text "   "))
              (niwa-collaboration--handle-prompt
               room 1 (list :t "prompt" :text 42)))
            (should-not rejects)))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer))))

(niwa-deftest niwa-collaboration--handle-retract
  (:doc "removes only the sender's own pending entry with its attachments")
  (let* ((workspace (niwa-workspace-get-or-create
                     'project "/tmp/collab-retract/" "/tmp/collab-retract/"
                     "cr"))
         (session (niwa-session-create "main" workspace))
         (guests (make-hash-table :test #'eql))
         (data-buffer (generate-new-buffer " *collab-retract-data*"))
         (view-buffer (generate-new-buffer " *collab-retract-view*"))
         (media (make-temp-file "niwa-retract-media-" nil ".jpg" "img"))
         (room (list :session session :guests guests
                     :data-buffer data-buffer :transport 'transport))
         rebuilt)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local niwa--view-buffer view-buffer))
          (puthash 1 (list :name "Phone" :writable t :ready t
                           :guest-id "phone-guest-id")
                   guests)
          (puthash 2 (list :name "Tablet" :writable t :ready t
                           :guest-id "tablet-guest-id")
                   guests)
          (let ((mine (niwa-session-enqueue-pending-input
                       session 'follow-up
                       (list :input "mine" :guest-id "phone-guest-id"
                             :guest-paths (list media))))
                (theirs (niwa-session-enqueue-pending-input
                         session 'follow-up
                         (list :input "theirs"
                               :guest-id "tablet-guest-id"))))
            (cl-letf (((symbol-function 'niwa-view--interaction-rebuild)
                       (lambda () (setq rebuilt t)))
                      ((symbol-function
                        'niwa-collaboration--transport-send)
                       (lambda (&rest _) t)))
              ;; Another guest cannot retract it, nor can a bogus id.
              (niwa-collaboration--handle-retract
               room 2 (list :id (plist-get mine :id)))
              (niwa-collaboration--handle-retract
               room 1 (list :id 999))
              (should (= 2 (length
                            (niwa-session-pending-follow-ups session))))
              (should (file-exists-p media))
              ;; An entry the drain is delivering right now is no longer
              ;; retractable: its files are about to be read mid-turn.
              (plist-put mine :delivering t)
              (niwa-collaboration--handle-retract
               room 1 (list :id (plist-get mine :id)))
              (should (= 2 (length
                            (niwa-session-pending-follow-ups session))))
              (should (file-exists-p media))
              (plist-put mine :delivering nil)
              ;; The owner can, and the attachment leaves with it.
              (niwa-collaboration--handle-retract
               room 1 (list :id (plist-get mine :id)))
              (should (equal (list theirs)
                             (niwa-session-pending-follow-ups session)))
              (should-not (file-exists-p media))
              (should rebuilt))))
      (when (file-exists-p media) (delete-file media))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-collaboration--save-guest-attachments
  (:doc "saves accepted types within budget and drops invalid sets whole")
  (let ((dir (make-temp-file "niwa-guest-images" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-view--media-dir)
                   (lambda () dir)))
          (let ((paths (niwa-collaboration--save-guest-attachments
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
          (let ((paths (niwa-collaboration--save-guest-attachments
                        (list (list :mime "text/x-patch"
                                    :data (base64-encode-string "--- a\n"))))))
            (should (= 1 (length paths)))
            (should (string-suffix-p ".patch" (car paths))))
          ;; Other UTF-8 text keeps its own extension, unless Read would
          ;; take that extension for binary; binary bytes are refused.
          (let ((paths (niwa-collaboration--save-guest-attachments
                        (list (list :mime "text/plain" :extension "py"
                                    :data (base64-encode-string
                                           (encode-coding-string "print(\"λ\")\n" 'utf-8)))
                              (list :mime "text/plain" :extension "png"
                                    :data (base64-encode-string "<svg/>"))))))
            (should (equal '("py" "txt") (mapcar #'file-name-extension paths))))
          (should-not (niwa-collaboration--save-guest-attachments
                       (list (list :mime "text/plain" :extension "exe"
                                   :data (base64-encode-string "MZ\0\220")))))
          (should-not (niwa-collaboration--save-guest-attachments
                       (list (list :mime "text/plain" :extension "../x"
                                   :data (base64-encode-string "\377\376")))))
          (let ((path (car (niwa-collaboration--save-guest-attachments
                            (list (list :mime "text/plain" :extension "../x"
                                        :data (base64-encode-string "ok")))))))
            (should (equal dir (directory-file-name (file-name-directory path))))
            (should (string-suffix-p ".txt" path)))
          ;; Unknown type, malformed data, over-budget, and too many
          ;; attachments each drop the whole set.
          (should-not (niwa-collaboration--save-guest-attachments
                       (list (list :mime "image/svg+xml"
                                   :data (base64-encode-string "x")))))
          (should-not (niwa-collaboration--save-guest-attachments
                       (list (list :mime "image/png" :data "not base64!"))))
          (should-not (niwa-collaboration--save-guest-attachments
                       (list (list :mime "image/png"
                                   :data (base64-encode-string
                                          (make-string
                                           (1+ niwa-collaboration--max-attachment-bytes)
                                           ?x))))))
          (should-not (niwa-collaboration--save-guest-attachments
                       (make-list
                        (1+ niwa-collaboration--max-prompt-attachments)
                        (list :mime "image/png"
                              :data (base64-encode-string "x")))))
          (should-not (niwa-collaboration--save-guest-attachments nil)))
      (delete-directory dir t))))

(niwa-deftest niwa-collaboration--policy-admits-p
  (:doc "reads a mode-list policy left to right with an implicit nil")
  (progn
  (should (niwa-collaboration--policy-admits-p t "plan"))
  (should-not (niwa-collaboration--policy-admits-p nil "plan"))
  (should (niwa-collaboration--policy-admits-p '("plan" "goal") "goal"))
  (should-not (niwa-collaboration--policy-admits-p '("plan" "goal") "stop"))
  ;; `(not ...)' refuses before a later t admits the rest.
  (let ((policy '((not "learn" "remember") t)))
    (should (niwa-collaboration--policy-admits-p policy "plan"))
    (should-not (niwa-collaboration--policy-admits-p policy "learn")))
  ;; The first deciding element wins, whichever way it decides.
  (should (niwa-collaboration--policy-admits-p '("learn" (not "learn")) "learn"))
  (should-not (niwa-collaboration--policy-admits-p '(nil "plan") "plan"))
  (should (niwa-collaboration--policy-admits-p '(t) "anything"))))

(niwa-deftest niwa-collaboration--guest-invocable-p
  (:doc "admits by link tier and refuses unsafe names outright")
  (progn
  (let ((niwa-collaboration-guest-skills
         '((full "plan" "review" "mode") (owner . t))))
    (should (niwa-collaboration--guest-invocable-p "plan" 'full))
    (should (niwa-collaboration--guest-invocable-p "review" 'full))
    ;; Listed but unsafe: `mode' reaches full-auto, so the deny-set wins
    ;; over a mistaken allowlist entry, for the owner too.
    (should-not (niwa-collaboration--guest-invocable-p "mode" 'full))
    (should-not (niwa-collaboration--guest-invocable-p "mode" 'owner))
    (should-not (niwa-collaboration--guest-invocable-p "compact" 'full))
    (should (niwa-collaboration--guest-invocable-p "compact" 'owner))
    ;; A viewer has no tier and is admitted to nothing.
    (should-not (niwa-collaboration--guest-invocable-p "plan" nil))
    (should-not (niwa-collaboration--guest-invocable-p nil 'owner)))
  ;; An owner without an entry inherits the full-link policy.
  (let ((niwa-collaboration-guest-skills '((full "plan"))))
    (should (niwa-collaboration--guest-invocable-p "plan" 'owner))
    (should-not (niwa-collaboration--guest-invocable-p "goal" 'owner)))
  ;; Nothing configured at all admits nobody.
  (let ((niwa-collaboration-guest-skills nil))
    (should-not (niwa-collaboration--guest-invocable-p "plan" 'owner)))))

(niwa-deftest niwa-collaboration--handle-prompt--invoke
  (:doc "queues an allowlisted invocation by name and refuses anything else")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests
                       :transport 'transport :session 'session))
           (niwa-collaboration-guest-skills
            '((full "plan" "mode") (owner "plan" "review" "compact")))
           enqueued sent)
      (puthash 1 (list :name "Phone" :writable t :ready t
                       :guest-id "phone-guest-id")
               guests)
      (puthash 2 (list :name "Viewer" :writable nil :ready t) guests)
      (puthash 3 (list :name "Boss" :writable t :owner t :ready t
                       :guest-id "owner-guest-id")
               guests)
      (setq-local niwa--view-buffer (current-buffer))
      (cl-letf (((symbol-function 'niwa-view-enqueue-external-follow-up)
                 (lambda (_data-buffer text &rest keys)
                   (setq enqueued (cons text keys))
                   (list :id 9 :input text)))
                ((symbol-function 'niwa-collaboration--queue-position)
                 (lambda (&rest _) 1))
                ((symbol-function 'niwa-collaboration--publish-queue)
                 (lambda (_room) nil))
                ((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent)
                   t)))
        ;; Unlisted, unsafe-listed, and read-only guests are all refused;
        ;; free text is never scanned for a sigil.
        (niwa-collaboration--handle-prompt
         room 1 (list :invoke "compact" :text ""))
        (niwa-collaboration--handle-prompt
         room 1 (list :invoke "mode" :text "full-auto"))
        (niwa-collaboration--handle-prompt
         room 2 (list :invoke "plan" :text ""))
        (should-not enqueued)
        ;; An allowlisted invocation queues by name, with the text as its
        ;; arguments and skill planning left inert.
        (niwa-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text "add a retry cap"))
        (should (equal "add a retry cap" (car enqueued)))
        (should (equal "plan" (plist-get (cdr enqueued) :invoke)))
        (should (eq 'full (plist-get (cdr enqueued) :guest-role)))
        (should (equal "phone-guest-id"
                       (plist-get (cdr enqueued) :guest-id)))
        (should (equal '(1 . (:t "queued" :id 9 :position 1)) (car sent)))
        ;; The owner tier admits what the full tier does not, and the
        ;; entry remembers the tier it was queued under.
        (setq enqueued nil)
        (niwa-collaboration--handle-prompt
         room 3 (list :invoke "compact" :text ""))
        (should (equal "compact" (plist-get (cdr enqueued) :invoke)))
        (should (eq 'owner (plist-get (cdr enqueued) :guest-role)))
        ;; A bare `/review' would open the host's target picker, so it is
        ;; queued with the default target instead.
        (setq enqueued nil)
        (niwa-collaboration--handle-prompt
         room 3 (list :invoke "review" :text ""))
        (should (equal "uncommitted" (car enqueued)))
        (setq enqueued nil)
        (niwa-collaboration--handle-prompt
         room 3 (list :invoke "review" :text "branch:main"))
        (should (equal "branch:main" (car enqueued)))
        ;; A bare invocation carries no arguments at all.
        (setq enqueued nil)
        (niwa-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text ""))
        (should (equal "" (car enqueued)))
        ;; A double-fired tap inside the duplicate window is one send.
        (setq enqueued nil)
        (niwa-collaboration--handle-prompt
         room 1 (list :invoke "plan" :text ""))
        (should-not enqueued)))))

(niwa-deftest niwa-collaboration--guest-skills-admitted-p
  (:doc "admits only a bounded unique set of currently visible skills")
  (let* ((root (make-temp-file "guest-skills-" t))
         (source (niwa-skills-test--write-skill
                  root "alpha" "name: alpha\ndescription: Alpha\n" "Alpha"))
         (skill (niwa-skill--create
                 :name "alpha" :source-file source
                 :source-dir (file-name-directory source)
                 :source 'project :active-p t :user-invocable-p t))
         (session (niwa-session--create :name "skills" :skills (list skill)))
         (niwa-collaboration-guest-skills '((full "alpha") (owner . t))))
    (unwind-protect
        (progn
          (should (niwa-collaboration--guest-skills-admitted-p
                   '("alpha") 'full session))
          (dolist (names '(nil "alpha" ["alpha"] (42) ("missing")
                          ("alpha" "alpha") ("plan") ("mode")
                          ("a" "b" "c" "d" "e" "f" "g")))
            (should-not (niwa-collaboration--guest-skills-admitted-p
                         names 'owner session)))
          (let ((niwa-collaboration-guest-skills nil))
            (should-not (niwa-collaboration--guest-skills-admitted-p
                         '("alpha") 'full session))))
      (delete-directory root t))))

(niwa-deftest niwa-collaboration--handle-prompt--selected-skills
  (:doc "queues selected skills once and rejects mixed or unauthorized selections")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests
                       :transport 'transport :session 'session))
           (guest (list :name "Phone" :writable t :ready t))
           queued)
      (puthash 1 guest guests)
      (puthash 2 (list :writable nil) guests)
      (setq-local niwa--view-buffer (current-buffer))
      (cl-letf (((symbol-function 'niwa-collaboration--guest-skills-admitted-p)
                 (lambda (names _role _session) (equal names '("alpha" "beta"))))
                ((symbol-function 'niwa-view-enqueue-external-follow-up)
                 (lambda (_data text &rest keys)
                   (push (cons text keys) queued) (list :id 10)))
                ((symbol-function 'niwa-collaboration--queue-position) (lambda (&rest _) 1))
                ((symbol-function 'niwa-collaboration--publish-queue) #'ignore)
                ((symbol-function 'niwa-collaboration--transport-send) #'ignore))
        (dolist (frame '((:skills ("missing") :text "work")
                         (:skills ("alpha" "beta") :invoke "plan" :text "work")))
          (niwa-collaboration--handle-prompt room 1 frame))
        (niwa-collaboration--handle-prompt
         room 2 '(:skills ("alpha" "beta") :text "work"))
        (should-not queued)
        (dotimes (_ 2)
          (niwa-collaboration--handle-prompt
           room 1 '(:skills ("alpha" "beta") :text "$unselected stays literal")))
        (should (= 1 (length queued)))
        (should (equal "$unselected stays literal" (caar queued)))
        (should (equal '("alpha" "beta") (plist-get (cdar queued) :skills)))
        (should (eq 'full (plist-get (cdar queued) :guest-role)))))))

(niwa-deftest niwa-collaboration--guest-roster
  (:doc "describes each admitted name with its namespace and hint")
  (niwa-view-test--with-buffers
    (let* ((session (niwa-session--create :name "roster"))
           (room (list :data-buffer data-buf :session session))
           (full (list :name "Phone" :writable t))
           (owner (list :name "Boss" :writable t :owner t))
           (niwa-slash-commands
            '(("plan" . ignore) ("mode" . ignore) ("compact" . ignore)))
           (niwa-collaboration-guest-skills
            '((full "plan" "remember" "mode" "nonexistent")
              (owner (not "compact") t))))
      (with-current-buffer data-buf
        (setq-local niwa--view-buffer view-buf))
      (cl-letf (((symbol-function 'niwa-skills-user-visible-skills)
                 (lambda (&rest _)
                   (list (niwa-skill--create
                          :name "remember" :argument-hint "[focus]")))))
        (let ((roster (niwa-collaboration--guest-roster room full)))
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
          (should-not (assoc "nonexistent" roster)))
        ;; A `(not ...) t' policy offers the whole universe minus the
        ;; excluded and unsafe names, commands before skills.
        (should (equal '("plan" "remember")
                       (mapcar (lambda (e) (cdr (assoc "name" e)))
                               (niwa-collaboration--guest-roster
                                room owner))))
        ;; A viewer is offered nothing.
        (should-not (niwa-collaboration--guest-roster
                     room (list :name "Viewer" :writable nil)))))))

(niwa-deftest niwa-collaboration--handle-abort ()
  ,test
  (test)
  :doc "aborts for a writable guest and ignores read-only guests"
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests))
           aborted)
      (puthash 1 (list :name "Viewer" :writable nil :ready t) guests)
      (puthash 2 (list :name "Writer" :writable t :ready t) guests)
      (cl-letf (((symbol-function 'niwa-view--abort-data-buffer)
                 (lambda (buffer) (setq aborted buffer))))
        (niwa-collaboration--handle-abort room 1)
        (should-not aborted)
        (niwa-collaboration--handle-abort room 2)
        (should (eq (current-buffer) aborted)))))

  :doc "guest abort cancels a slow prompt hook before request dispatch"
  (niwa-view-test--with-buffers
    (let* ((guests (make-hash-table :test #'eql))
           (session (niwa-session--create :name "share"))
           (room (list :data-buffer data-buf :guests guests))
           late-callback submission send-called)
      (puthash 1 (list :name "Writer" :writable t :ready t) guests)
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    niwa--workspace nil
                    niwa--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local niwa--session session))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (setq late-callback callback)))
                ((symbol-function 'niwa-view--abort-data-buffer) #'ignore)
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (niwa-view--run-prompt-submit-hook
           "guest prompt" "guest prompt"
           (lambda (_accepted) (gptel-send)))
          (setq submission niwa-view--prompt-hook-pending))
        (niwa-collaboration--handle-abort room 1)
        (funcall late-callback nil))
      (should (eq 'cancelled
                  (niwa-prompt-submission-state submission)))
      (should-not send-called)
      (with-current-buffer data-buf
        (should (string-empty-p (buffer-string)))))))


;;
;;; Relay callbacks

(niwa-deftest niwa-collaboration--on-frame
  (:quiet t :doc "dispatches known frames and preserves the room on handler failure")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)
                       :session (niwa-session--create
                                 :name "s"
                                 :workspace (niwa-workspace--create
                                             :root "/proj/"))))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           handled stopped)
      (cl-letf (((symbol-function 'niwa-collaboration--handle-hello)
                 (lambda (_room peer _frame) (setq handled peer))))
        (niwa-collaboration--on-frame (current-buffer) 5
                                         (list :t "hello" :proto 3)))
      (should (= 5 handled))
      ;; Renaming changes only an existing guest's display attribution.
      (let ((guest (list :name "Before" :ready t :writable nil
                         :guest-id "stable-id")))
        (puthash 5 guest (plist-get room :guests))
        (niwa-collaboration--on-frame
         (current-buffer) 5 (list :t "set-name" :name "  Calm\nCat  "
                                 :writable t :owner t))
        (should (equal (plist-get guest :name) "Calm Cat"))
        (should-not (plist-get guest :writable))
        (should-not (plist-get guest :owner))
        (should (equal (plist-get guest :guest-id) "stable-id"))
        (niwa-collaboration--on-frame
         (current-buffer) 5 (list :t "set-name" :name 42))
        (should (equal (plist-get guest :name) "Calm Cat"))
        (niwa-collaboration--on-frame
         (current-buffer) 9 (list :t "set-name" :name "Unknown"))
        (should-not (gethash 9 (plist-get room :guests))))
      ;; Unknown frame types are tolerated.
      (niwa-collaboration--on-frame (current-buffer) 5
                                       (list :t "future-frame"))
      ;; A room takes uploads into its session's project root, and no
      ;; other project file frame.
      (let (uploaded)
        (cl-letf (((symbol-function 'niwa-collaboration-files-handle-upload)
                   (lambda (_room peer _frame root)
                     (setq uploaded (list peer root))))
                  ((symbol-function 'niwa-collaboration-files-handle-list)
                   (lambda (&rest _) (error "Not routed"))))
          (niwa-collaboration--on-frame (current-buffer) 5
                                           (list :t "file-upload"))
          (niwa-collaboration--on-frame (current-buffer) 5
                                           (list :t "files"))
          (should (equal '(5 "/proj/") uploaded))))
      ;; A handler fault warns the host with the error and tells only its
      ;; sender; other guests learn nothing about it.
      (let (sent broadcast warned)
        (cl-letf (((symbol-function 'niwa-collaboration--handle-prompt)
                   (lambda (&rest _) (error "Handler fault")))
                  ((symbol-function 'niwa-collaboration--stop-internal)
                   (lambda (_room reason) (setq stopped reason)))
                  ((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame) (push (cons peer frame) sent)))
                  ((symbol-function 'niwa-collaboration--broadcast)
                   (lambda (&rest args) (push args broadcast)))
                  ((symbol-function 'display-warning)
                   (lambda (_type message &rest _) (setq warned message))))
          (niwa-collaboration--on-frame (current-buffer) 5
                                           (list :t "prompt" :text "x")))
        (should-not stopped)
        (should (equal '(5) (mapcar #'car sent)))
        (should-not broadcast)
        (should (string-search "Handler fault" warned))
        (should-not (string-search "Handler fault" (plist-get (cdar sent) :message))))
      ;; A handler that would ask in Emacs is refused to its sender, and
      ;; the room stays up.
      (let (sent (stopped nil))
        (cl-letf (((symbol-function 'niwa-collaboration--handle-set-mode)
                   ;; `yes-or-no-p' would take the dialog path after an
                   ;; earlier test's mouse event, which ignores the guard.
                   (lambda (&rest _) (y-or-n-p "Take over? ")))
                  ((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame) (push (cons peer frame) sent)))
                  ((symbol-function 'niwa-collaboration--stop-internal)
                   (lambda (_room reason) (setq stopped reason))))
          (niwa-collaboration--on-frame (current-buffer) 5
                                           (list :t "set-mode" :mode "ask")))
        (should-not stopped)
        (should (equal (list (cons 5 (list :t "notice"
                                           :message niwa-collaboration-needs-host-message)))
                       sent))))))

(defvar test-niwa-collaboration-guest--sockets nil
  "Guest sockets the current relay-backed surface opened, to close after it.")

(defconst test-niwa-collaboration-guest--shared-handlers
  '(("viewing" . niwa-collaboration-presence-handle)
    ("new-session" . niwa-collaboration--handle-new-session)
    ("session-models" . niwa-collaboration--handle-session-models)
    ("file-upload" . niwa-collaboration-files-handle-upload)
    ("artifact-get" . niwa-collaboration--handle-artifact-get)
    ("artifact-delete" . niwa-collaboration--handle-artifact-delete)
    ("discussion" . niwa-collaboration-discussions-handle)
    ("store-list" . niwa-collaboration--handle-store-list)
    ("store-action" . niwa-collaboration--handle-store-action))
  "The frame types rooms and lobbies share, with their handlers.")

(niwa-deftest niwa-collaboration--dispatch-shared-frame
  (:doc "routes every shared type from a room and a lobby, after their own types")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)
                       :session (niwa-session--create
                                 :name "s"
                                 :workspace (niwa-workspace--create
                                             :root "/proj/"))))
           (lobby (list :guests (make-hash-table :test #'eql)))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (saved (mapcar (lambda (entry)
                            (cons (cdr entry) (symbol-function (cdr entry))))
                          test-niwa-collaboration-guest--shared-handlers))
           routed)
      (puthash "/lobby/" lobby niwa-collaboration-lobby--lobbies)
      (unwind-protect
          (progn
            ;; Each handler records its call and returns nil.
            (pcase-dolist (`(,type . ,handler)
                           test-niwa-collaboration-guest--shared-handlers)
              (fset handler
                    (if (equal type "file-upload")
                        (lambda (owner _peer _frame root)
                          (push (list type owner root) routed)
                          nil)
                      (lambda (owner &rest _)
                        (push (list type owner nil) routed)
                        nil))))
              ;; Every shared type reaches its handler from both kinds, a
              ;; room's uploads landing in its session's root and a lobby's
              ;; in its own.
              (pcase-dolist (`(,type . ,_) test-niwa-collaboration-guest--shared-handlers)
                (setq routed nil)
                (niwa-collaboration--on-frame (current-buffer) 5 (list :t type))
                (niwa-collaboration-lobby--on-frame "/lobby/" 5 (list :t type))
                (should (equal (list (list type lobby (and (equal type "file-upload")
                                                           "/lobby/"))
                                     (list type room (and (equal type "file-upload")
                                                          "/proj/")))
                               routed))
                ;; Handled is reported even though each handler returned nil.
                (should (eq 'handled (niwa-collaboration--dispatch-shared-frame
                                      room 5 (list :t type) "/proj/")))
                (should (eq 'handled (niwa-collaboration--dispatch-frame
                                      room 5 (list :t type))))
                (should (eq 'handled (niwa-collaboration-lobby--dispatch-frame
                                      lobby 5 (list :t type) "/lobby/"))))
              ;; Unknown frames fall through unhandled and touch nothing.
              (setq routed nil)
              (dolist (type '("future-frame" "files" "prompt"))
                (should (eq 'unhandled (niwa-collaboration--dispatch-shared-frame
                                        room 5 (list :t type) "/proj/"))))
              (should (eq 'unhandled (niwa-collaboration--dispatch-frame
                                      room 5 (list :t "future-frame"))))
              (should (eq 'unhandled (niwa-collaboration-lobby--dispatch-frame
                                      lobby 5 (list :t "future-frame") "/lobby/")))
              (should-not routed))
        (pcase-dolist (`(,handler . ,definition) saved)
          (fset handler definition)))
      ;; Each kind's own types are dispatched first and never fall through.
      (let (fell)
        (cl-letf (((symbol-function 'niwa-collaboration--dispatch-shared-frame)
                   (lambda (_room _peer frame _root) (push (plist-get frame :t) fell)))
                  ((symbol-function 'niwa-collaboration--handle-history-get) #'ignore)
                  ((symbol-function 'niwa-collaboration-lobby--handle-open) #'ignore)
                  ((symbol-function 'niwa-collaboration-files-handle-list) #'ignore))
          (niwa-collaboration--on-frame (current-buffer) 5 (list :t "history-get"))
          (niwa-collaboration--on-frame (current-buffer) 5 (list :t "set-name"))
          (niwa-collaboration-lobby--on-frame "/lobby/" 5 (list :t "open-session"))
          (niwa-collaboration-lobby--on-frame "/lobby/" 5 (list :t "files"))
          (should-not fell)
          (niwa-collaboration--on-frame (current-buffer) 5 (list :t "files"))
          (niwa-collaboration-lobby--on-frame "/lobby/" 5 (list :t "prompt"))
          (should (equal '("prompt" "files") fell)))))))

(defun test-niwa-collaboration-guest--with-surface (kind body)
  "Call BODY with a relay-backed guest surface of KIND, `room' or `lobby'.
A lobby is started for real; a room is a session room whose sealed
transport dials the same in-process stub relay.  BODY receives the room
or lobby plist and the stub relay's port."
  (niwa-relay-test--with-stub-relay (state port _server)
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-guest-frame-" t)))
           (niwa-user-dir (file-name-as-directory
                              (file-name-concat root "user")))
           (niwa-collaboration-relay-url (format "ws://127.0.0.1:%d" port))
           (niwa-collaboration-relay-host-token nil)
           (niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
           (niwa-collaboration--rooms (make-hash-table :test #'eq))
           (kill-emacs-hook nil)
           (workspace (niwa-workspace--create
                       :type 'project :id root :root root :name "proj"
                       :file-cache (niwa-file-cache--create
                                    :table (make-hash-table :test #'equal)
                                    :order nil :total-bytes 0)))
           (data-buffer (and (eq kind 'room)
                             (generate-new-buffer " *guest-frame-room*")))
           (test-niwa-collaboration-guest--sockets nil)
           room)
      (cl-letf (((symbol-function 'niwa-sleep-update) #'ignore)
                ((symbol-function 'niwa-collaboration--announce-lobby) #'ignore)
                ((symbol-function 'niwa-collaboration-lobby--workspace)
                 (lambda (_directory) workspace))
                ;; A room's hello also sends the transcript snapshot, which
                ;; this bare room has none of; admission is what matters.
                ((symbol-function 'niwa-collaboration--handle-hello)
                 (lambda (room peer frame)
                   (niwa-collaboration--admit-hello room peer frame))))
        (unwind-protect
            (progn
              (if (eq kind 'lobby)
                  (setq room (niwa-collaboration-lobby-start root))
                (setq room (list :data-buffer data-buffer
                                 :guests (make-hash-table :test #'eql)
                                 :session (niwa-session--create
                                           :name "s" :workspace workspace)
                                 :room-id "roomroomroomroom"
                                 :key (make-string 32 5)
                                 :write-token (make-string 16 ?w)
                                 :owner-token (make-string 16 ?o)))
                (puthash data-buffer room niwa-collaboration--rooms)
                (plist-put
                 room :transport
                 (niwa-collaboration--transport-open
                  (format "ws://127.0.0.1:%d/r/%s?role=host" port
                          (plist-get room :room-id))
                  (plist-get room :key)
                  :on-frame (lambda (peer frame)
                              (niwa-collaboration--on-frame
                               data-buffer peer frame)))))
              (should (niwa-relay-test--pump
                       (lambda () (plist-get (car state) :host))))
              (funcall body room port))
          (dolist (ws test-niwa-collaboration-guest--sockets)
            (let ((process (websocket-conn ws)))
              (set-process-sentinel process #'ignore)
              (delete-process process)))
          (if (eq kind 'lobby)
              (when room (niwa-collaboration-lobby--stop room 'emacs-exit))
            (when-let* ((transport (plist-get room :transport)))
              (niwa-collaboration--transport-stop transport)))
          (when data-buffer (kill-buffer data-buffer))
          (delete-directory root t))))))

(defun test-niwa-collaboration-guest--connect (room port &optional tokens)
  "Connect a guest to ROOM through the stub relay at PORT.
TOKENS is nil for no hello, or a list of `write' and `owner' (or
`forged-owner') naming the tokens its hello proves.  Return a plist
whose `:frames' are the decoded frames it received."
  (let* ((key (plist-get room :key))
         (guest (list :key key :frames nil :ws nil)))
    (plist-put
     guest :ws
     (websocket-open
      (format "ws://127.0.0.1:%d/r/%s?role=guest" port (plist-get room :room-id))
      :on-message
      (lambda (_ws frame)
        (when (eq 'binary (websocket-frame-opcode frame))
          (when-let* ((text (niwa-collaboration--unseal
                             key (cdr (niwa-collaboration--envelope-unpack
                                       (websocket-frame-payload frame))))))
            ;; As a browser does: drop a frame for another connection and
            ;; strip this one's nonce, which every helper frame carries.
            (let ((decoded (niwa-collaboration--frame-decode text)))
              (when (member (plist-get decoded :nonce) '(nil "guest-nonce"))
                (cl-remf decoded :nonce)
                (plist-put guest :frames
                           (append (plist-get guest :frames) (list decoded))))))))))
    (push (plist-get guest :ws) test-niwa-collaboration-guest--sockets)
    (should (niwa-relay-test--pump
             (lambda () (websocket-openp (plist-get guest :ws)))))
    (when tokens
      (let ((guests (plist-get room :guests))
            (before (hash-table-count (plist-get room :guests))))
        (should (niwa-relay-test--pump
                 (lambda () (test-niwa-collaboration-guest--challenge guest))))
        (test-niwa-collaboration-guest--send
         guest
         (append (list :t "hello" :proto niwa-collaboration--protocol-version
                       :name "Guest")
                 (when (memq 'write tokens)
                   (list :writeToken (niwa-collaboration--base64url
                                      (plist-get room :write-token))))
                 (cond
                  ((memq 'owner tokens)
                   (list :ownerToken (niwa-collaboration--base64url
                                      (plist-get room :owner-token))))
                  ((memq 'forged-owner tokens)
                   (list :ownerToken (niwa-collaboration--base64url
                                      (make-string 16 ?x)))))))
        (should (niwa-relay-test--pump
                 (lambda () (> (hash-table-count guests) before))))))
    guest))

(defun test-niwa-collaboration-guest--challenge (guest)
  "Return the challenge the host bound GUEST's connection with, or nil."
  (plist-get (cl-find "challenge" (plist-get guest :frames)
                      :key (lambda (frame) (plist-get frame :t)) :test #'equal)
             :challenge))

(defun test-niwa-collaboration-guest--send (guest frame)
  "Send FRAME from GUEST, sealed and bound to its connection, as a browser does."
  (plist-put guest :seq (1+ (or (plist-get guest :seq) 0)))
  (websocket-send
   (plist-get guest :ws)
   (make-websocket-frame
    :opcode 'binary :completep t
    :payload (niwa-collaboration--envelope-pack
              0 (niwa-collaboration--seal
                 (plist-get guest :key)
                 (json-serialize
                  (append frame
                          (list :challenge (test-niwa-collaboration-guest--challenge guest)
                                :seq (plist-get guest :seq)
                                :nonce "guest-nonce"))))))))

(defun test-niwa-collaboration-guest--exchange (guest frame)
  "Send FRAME then a store listing request from GUEST.
Return the notices GUEST received before the listing: frames on one
connection arrive in order, so the listing reply proves that FRAME's own
replies have all arrived and that the room still works.  Broadcasts such
as presence may interleave, so only notices are returned."
  (let ((start (length (plist-get guest :frames))))
    (test-niwa-collaboration-guest--send guest frame)
    (test-niwa-collaboration-guest--send guest (list :t "store-list"))
    (should (niwa-relay-test--pump
             (lambda ()
               (cl-find "store-artifacts"
                        (nthcdr start (plist-get guest :frames))
                        :key (lambda (frame) (plist-get frame :t))
                        :test #'equal))))
    (test-niwa-collaboration-guest--notices
     (cl-subseq (plist-get guest :frames) start
                (cl-position "store-artifacts" (plist-get guest :frames)
                             :key (lambda (frame) (plist-get frame :t))
                             :test #'equal :start start)))))

(defun test-niwa-collaboration-guest--notices (frames)
  "Return the notice frames among FRAMES."
  (cl-remove-if-not (lambda (frame) (equal "notice" (plist-get frame :t)))
                    frames))

(niwa-deftest niwa-collaboration--guard-guest-frame
  ()
  ,test
  (test)
  :doc "a failing shared frame answers only its sender, in a room and a lobby alike"
  (dolist (kind '(room lobby))
    (dolist (case
             (list (list 'owner-user-error '(write owner)
                         (lambda () (user-error "Artifact /secret/path is locked"))
                         "Artifact /secret/path is locked" nil)
                   (list 'writer-user-error '(write)
                         (lambda () (user-error "Artifact /secret/path is locked"))
                         "The host could not complete this action; the owner can review session status"
                         nil)
                   ;; A real question under `inhibit-interaction' signals
                   ;; instead of prompting, without naming itself ...
                   (list 'inhibited '(write owner)
                         (lambda () (read-string "Overwrite /secret/path? "))
                         niwa-collaboration-needs-host-message
                         "needed a decision in Emacs and was refused")
                   ;; ... while the daemon's unseen-question refusal names
                   ;; it, for the host's warning only.
                   (list 'inhibited-question '(write owner)
                         (lambda () (signal 'inhibited-interaction
                                            (list "Overwrite /secret/path?")))
                         niwa-collaboration-needs-host-message
                         "refused: Overwrite /secret/path?")
                   (list 'fault '(write owner)
                         (lambda () (error "Disk fault at /secret/path"))
                         "The host could not handle this action"
                         "Disk fault at /secret/path")))
      (pcase-let ((`(,name ,tokens ,producer ,notice ,warning) case))
        (ert-info ((format "%s in a %s" name kind))
          (test-niwa-collaboration-guest--with-surface
           kind
           (lambda (room port)
             (let ((guest (test-niwa-collaboration-guest--connect
                           room port tokens))
                   captured replies)
               (cl-letf (((symbol-function
                           'niwa-collaboration--handle-artifact-delete)
                          (lambda (&rest _) (funcall producer))))
                 (niwa-test--with-captured-diagnostics captured
                   (setq replies (test-niwa-collaboration-guest--exchange
                                  guest (list :t "artifact-delete" :reqId 1)))))
               ;; Exactly one notice, and the room answers the next frame.
               (should (equal (list (list :t "notice" :message notice))
                              replies))
               (if warning
                   (progn
                     (should (string-search warning captured))
                     (should (string-search
                              (format "niwa [%s]: "
                                      (niwa-workspace-root
                                       (niwa-collaboration--room-workspace room)))
                              captured)))
                 (should (string-empty-p captured)))
               (should (eq room (if (eq kind 'lobby)
                                    (gethash (plist-get room :directory)
                                             niwa-collaboration-lobby--lobbies)
                                  (niwa-collaboration--room-for-buffer
                                   (plist-get room :data-buffer))))))))))))

  :doc "pre-hello and forged-owner peers get only the generic sentence"
  (dolist (kind '(room lobby))
    (ert-info ((format "%s" kind))
      (test-niwa-collaboration-guest--with-surface
       kind
       (lambda (room port)
         (let ((stranger (test-niwa-collaboration-guest--connect room port))
               (forger (test-niwa-collaboration-guest--connect
                        room port '(write forged-owner)))
               (no-write (test-niwa-collaboration-guest--connect
                          room port '(owner)))
               (generic "The host could not complete this action; the owner can review session status"))
           (cl-letf (((symbol-function 'niwa-collaboration--handle-artifact-delete)
                      (lambda (&rest _) (user-error "Artifact /secret/path is locked"))))
             ;; The forger and the owner token without a write token are
             ;; admitted but never as owners.
             (should-not (niwa-collaboration--owner room 2))
             (should-not (niwa-collaboration--owner room 3))
             (dolist (guest (list forger no-write))
               (should (equal (list (list :t "notice" :message generic))
                              (test-niwa-collaboration-guest--exchange
                               guest (list :t "artifact-delete" :reqId 1)))))
             ;; A peer that never said hello is not a guest: its frame
             ;; meets the same generic sentence and nothing else.
             (test-niwa-collaboration-guest--send
              stranger (list :t "artifact-delete" :reqId 1))
             (should (niwa-relay-test--pump
                      (lambda ()
                        (test-niwa-collaboration-guest--notices
                         (plist-get stranger :frames)))))
             (should (equal (list (list :t "notice" :message generic))
                            (test-niwa-collaboration-guest--notices
                             (plist-get stranger :frames)))))))))))

(niwa-deftest niwa-collaboration--on-control
  (:doc "drops a guest on peer-left and waits for hello on peer-joined")
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :data-buffer (current-buffer) :guests guests))
           (niwa-collaboration--rooms (niwa-test-room-registry room)))
      (niwa-collaboration--on-control (current-buffer) 'peer-joined 1)
      (should (= 0 (hash-table-count guests)))
      (puthash 1 (list :name "g") guests)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1234)))
        (niwa-collaboration--on-control (current-buffer) 'peer-left 1))
      (should (= 1234 (plist-get room :last-guest-departure)))
      (niwa-collaboration--on-control (current-buffer) 'peer-left 99)
      (should (= 1234 (plist-get room :last-guest-departure)))
      (should (= 0 (hash-table-count guests))))))

(niwa-deftest niwa-collaboration--on-state ()
  ,test
  (test)
  :doc "clears guests and restores push endpoints on reconnect"
  (with-temp-buffer
    (let* ((guests (make-hash-table :test #'eql))
           (push-guests (make-hash-table :test #'equal))
           (room (list :data-buffer (current-buffer) :guests guests
                       :push-guests push-guests :transport 'transport))
           (niwa-collaboration--rooms (niwa-test-room-registry room))
           controls)
      (puthash 1 (list :name "g") guests)
      (puthash "phone-guest-id"
               '(:endpoint "https://push.example/subscription" :writable t)
               push-guests)
      (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) nil))
                ((symbol-function 'float-time) (lambda (&optional _) 1234)))
        (niwa-collaboration--on-state (current-buffer) 'down))
      (should (= 0 (hash-table-count guests)))
      (should (= 1234 (plist-get room :last-guest-departure)))
      (niwa-collaboration--on-state (current-buffer) 'down)
      (should (= 1234 (plist-get room :last-guest-departure)))
      (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
                 (lambda (_transport control) (push control controls))))
        (niwa-collaboration--on-state (current-buffer) 'open))
      (should (equal '(:t "push-subscribe" :peer 0
                       :guestId "phone-guest-id"
                       :endpoint "https://push.example/subscription"
                       :active :json-false)
                     (car controls)))
      (niwa-collaboration--on-state (current-buffer) 'stopped)))

  :doc "warns once when a relay dial never opens, but not after it has"
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :guests (make-hash-table :test #'eql)))
           (niwa-collaboration--rooms
            (niwa-test-room-registry room))
           warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type text &rest _) (push text warnings))))
        ;; A share whose dial never succeeded is not live; say so once.
        (niwa-collaboration--on-state (current-buffer) 'down)
        (niwa-collaboration--on-state (current-buffer) 'down)
        (should (= 1 (length warnings)))
        (should (string-match-p "not live" (car warnings)))
        ;; An ordinary drop after a working connection stays quiet.
        (setq warnings nil)
        (niwa-collaboration--on-state (current-buffer) 'open)
        (niwa-collaboration--on-state (current-buffer) 'down)
        (should-not warnings)))))

(niwa-deftest niwa-collaboration--admit-hello/relay-replay
  (:quiet t
   :doc "refuses an owner's recorded sealed frames a relay replays on the same or a new connection")
  (dolist (kind '(lobby room))
    (ert-info ((format "%s" kind))
      (test-niwa-collaboration-guest--with-surface
       kind
       (lambda (room port)
         (let* ((key (plist-get room :key))
                (frames
                 (cons (list :t "hello" :proto niwa-collaboration--protocol-version
                             :name "Owner"
                             :writeToken (niwa-collaboration--base64url
                                          (plist-get room :write-token))
                             :ownerToken (niwa-collaboration--base64url
                                          (plist-get room :owner-token)))
                       (if (eq kind 'lobby)
                           (list '(:t "new-session" :reqId 1 :prompt "Delete the cache"))
                         (list '(:t "set-mode" :mode "full-auto")
                               '(:t "prompt" :text "Deploy now")))))
                (expected (if (eq kind 'lobby)
                              '((new-session "Delete the cache" t))
                            '((set-mode full-auto) (prompt "Deploy now"))))
                (view (generate-new-buffer " *guest-frame-view*"))
                acted refused)
          (cl-labels
              ((send-all (guest recorded)
                 (dolist (sealed recorded)
                   (websocket-send
                    (plist-get guest :ws)
                    (make-websocket-frame
                     :opcode 'binary :completep t
                     :payload (niwa-collaboration--envelope-pack 0 sealed)))))
               (refusals (guest)
                 (cl-count "The host ignored a frame the relay repeated or reordered."
                           (plist-get guest :frames)
                           :key (lambda (frame) (plist-get frame :message))
                           :test #'equal)))
           (unwind-protect
               (cl-letf (((symbol-function 'niwa-collaboration--create-guest-session)
                          (lambda (_room _peer guest _id _name prompt &rest _)
                            (push (list 'new-session prompt (plist-get guest :owner))
                                  acted)))
                         ((symbol-function 'niwa-permission-mode-transition)
                          (lambda (mode) (push (list 'set-mode mode) acted)))
                         ((symbol-function 'niwa-collaboration--publish-status) #'ignore)
                         ((symbol-function 'niwa-view-enqueue-external-follow-up)
                          (lambda (_buffer text &rest _)
                            (push (list 'prompt text) acted)
                            nil)))
                 (when-let* ((data-buffer (plist-get room :data-buffer)))
                   (with-current-buffer data-buffer
                     (setq-local niwa--view-buffer view)))
                 (let* ((original (test-niwa-collaboration-guest--connect room port))
                        ;; The original connection's challenge; a host
                        ;; that issues none (before the fix) gets none.
                        (challenge
                         (progn (niwa-relay-test--pump
                                 (lambda () (test-niwa-collaboration-guest--challenge
                                             original))
                                 1)
                                (test-niwa-collaboration-guest--challenge original)))
                        (seq 0)
                        ;; What a recording relay keeps: each frame's sealed bytes.
                        (recorded
                         (mapcar (lambda (frame)
                                   (niwa-collaboration--seal
                                    key (json-serialize
                                         (if challenge
                                             (append frame (list :challenge challenge
                                                                 :seq (cl-incf seq)
                                                                 :nonce "guest-nonce"))
                                           frame))))
                                 frames)))
                   (send-all original recorded)
                   (should (niwa-relay-test--pump
                            (lambda () (= (length acted) (length expected)))))
                   ;; Replayed on the original connection ...
                   (send-all original recorded)
                   (niwa-relay-test--pump
                    (lambda () (= (refusals original) 1)) 2)
                   (push (refusals original) refused)
                   (websocket-close (plist-get original :ws))
                   (should (niwa-relay-test--pump
                            (lambda () (not (websocket-openp (plist-get original :ws))))))
                   ;; ... and on a new one, after the original has gone.
                   (let ((replay (test-niwa-collaboration-guest--connect room port)))
                     (send-all replay recorded)
                     (niwa-relay-test--pump
                      (lambda () (= (refusals replay) 1)) 2)
                     (push (refusals replay) refused))))
             (kill-buffer view))
          ;; The host acted once, for the original frames only ...
          (should (equal expected (reverse acted)))
          ;; ... and told each replaying connection why, once.
          (should (equal (list 1 1) refused)))))))))

(defun test-niwa-collaboration-guest--capturing-transport (body)
  "Call BODY with a live-looking transport and a function returning sent frames.
Peer 1 is bound with the longest nonce a guest may have, so every frame
for it carries the most the transport adds.  Frames come back decoded."
  (let* ((key (make-string 32 ?k))
         (peers (make-hash-table :test #'eql))
         (transport (list :state 'open :key key :peers peers
                          ;; A real struct: compiled callers inline its accessors.
                          :ws (websocket-inner-create :url "ws://relay" :conn 'conn)))
         sent)
    (puthash 1 (niwa-collaboration--make-binding
                :challenge "c" :last-seq 1
                :nonce (make-string niwa-collaboration--max-guest-nonce-chars ?n))
             peers)
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t))
              ((symbol-function 'process-send-string)
               (lambda (_process bytes) (push bytes sent))))
      (funcall body transport
               (lambda ()
                 (mapcar (lambda (bytes)
                           (niwa-collaboration--frame-decode
                            (niwa-collaboration--unseal
                             key (cdr (niwa-collaboration--envelope-unpack
                                       (websocket-frame-payload
                                        (websocket-read-frame bytes)))))))
                         (reverse sent)))))))

(niwa-deftest niwa-collaboration--send-chunked/guest-nonce
  (:doc "sends a file larger than one frame complete to a guest whose frames carry its nonce")
  (test-niwa-collaboration-guest--capturing-transport
   (lambda (transport frames)
     (let ((content (apply #'unibyte-string
                           (cl-loop for index below (* 900 1024)
                                    collect (% (* index 7) 256)))))
       (should (niwa-collaboration--send-chunked
                transport 1 (list :t "file" :reqId 7 :path "big.bin") content))
       (let ((received (funcall frames)))
         (should (> (length received) 1))
         (should (equal (make-string niwa-collaboration--max-guest-nonce-chars ?n)
                        (plist-get (car received) :nonce)))
         (should (eq t (plist-get (car (last received)) :final)))
         (should (equal content
                        (base64-decode-string
                         (mapconcat (lambda (frame) (plist-get frame :data))
                                    received))))
         ;; Should a budget ever undercount again, the sender fails loudly,
         ;; which the file and artifact handlers turn into a refusal.
         (let ((niwa-collaboration--max-frame-json-bytes
                (+ niwa-collaboration--max-wire-json-bytes 1000)))
           (should-error (niwa-collaboration--send-chunked
                          transport 1 (list :t "file" :reqId 8 :path "big.bin") content)
                         :type 'niwa-collaboration-frame-too-large)))))))

(niwa-deftest niwa-collaboration--snapshot-chunks/guest-nonce
  (:doc "a snapshot chunk filled to the bound still reaches a guest whose frames carry its nonce")
  (test-niwa-collaboration-guest--capturing-transport
   (lambda (transport frames)
     (let* ((overhead (niwa-collaboration--snapshot-frame-overhead))
            (record (list :id "assistant-x" :kind "assistant" :revision 0 :text ""))
            (base (string-bytes (json-encode (niwa-collaboration--json-record record)))))
       ;; One record that fills its chunk's frame to the caller budget exactly.
       (setq record (plist-put record :text
                               (make-string (- niwa-collaboration--max-frame-json-bytes
                                               overhead base)
                                            ?x)))
       (let* ((chunks (niwa-collaboration--snapshot-chunks (list record)))
              (frame (niwa-collaboration--records-frame
                      (list :t "snapshot-chunk") (car chunks) nil)))
         (should (= 1 (length (car chunks))))
         (should (= (string-bytes (json-encode frame)) niwa-collaboration--max-frame-json-bytes))
         (should (niwa-collaboration--transport-send transport 1 frame))
         (should (= 1 (length (plist-get (car (funcall frames)) :records)))))))))

(provide 'test-niwa-collaboration-guest)
;;; test-niwa-collaboration-guest.el ends here
