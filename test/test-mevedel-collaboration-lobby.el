;;; test-mevedel-collaboration-lobby.el --- Collaboration lobby tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the per-workspace lobby: persisted credentials, the session
;; listing, opening, creating and deleting sessions from guest frames,
;; the lobby lifecycle, and restarting lobbies with Emacs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'mevedel-collaboration-lobby)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-projection)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)

(defun mevedel-collaboration-lobby-test--workspace (root)
  "Return a workspace struct rooted at ROOT, outside the registry."
  (mevedel-workspace--create
   :type 'project :id root :root root :name "proj"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defmacro mevedel-collaboration-lobby-test--with-user-dir (&rest body)
  "Run BODY with `mevedel-user-dir' in a fresh temporary directory."
  (declare (indent 0))
  `(let ((mevedel-user-dir (file-name-as-directory
                            (make-temp-file "mevedel-lobby-user-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory mevedel-user-dir t))))

(defmacro mevedel-collaboration-lobby-test--with-root (root &rest body)
  "Bind ROOT to a fresh temporary directory around BODY.
`mevedel-user-dir' is a fresh temporary directory too, so recording a
running lobby touches no real state."
  (declare (indent 1))
  `(mevedel-collaboration-lobby-test--with-user-dir
     (let ((,root (file-name-as-directory
                   (make-temp-file "mevedel-lobby-" t))))
       (unwind-protect (progn ,@body)
         (delete-directory ,root t)))))

(defun mevedel-collaboration-lobby-test--lobby (workspace &rest keys)
  "Return a lobby plist for WORKSPACE with KEYS added."
  (append keys
          (list :transport 'transport :workspace workspace
                :directory (mevedel-workspace-root workspace)
                :project "proj"
                :write-token (make-string 16 ?w)
                :owner-token (make-string 16 ?o)
                :link-view "view-link" :link-full "full-link"
                :link-owner "owner-link"
                :guests (make-hash-table :test #'eql))))

(defun mevedel-collaboration-lobby-test--session (id)
  "Return a session struct with session ID."
  (let ((session (mevedel-session--create :name id)))
    (setf (mevedel-session-session-id session) id)
    session))

(mevedel-deftest mevedel-collaboration-lobby--read-credentials
  (:doc "accepts only complete, well-formed stored credentials")
  (mevedel-collaboration-lobby-test--with-root root
    (let ((path (file-name-concat root "lobby"))
          (b64 #'mevedel-collaboration--base64url))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      (with-temp-file path (insert "(unbalanced"))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      ;; A truncated key is not a weaker lobby, it is no lobby.
      (with-temp-file path
        (prin1 (list :room-id "abcdefghijkl"
                     :key (funcall b64 (make-string 31 ?k))
                     :write-token (funcall b64 (make-string 16 ?w))
                     :owner-token (funcall b64 (make-string 16 ?o)))
               (current-buffer)))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      (with-temp-file path
        (prin1 (list :room-id "bad/id"
                     :key (funcall b64 (make-string 32 ?k))
                     :write-token (funcall b64 (make-string 16 ?w))
                     :owner-token (funcall b64 (make-string 16 ?o)))
               (current-buffer)))
      (should-not (mevedel-collaboration-lobby--read-credentials path))
      (with-temp-file path
        (prin1 (list :room-id "abcdefghijkl"
                     :key (funcall b64 (make-string 32 ?k))
                     :write-token (funcall b64 (make-string 16 ?w))
                     :owner-token (funcall b64 (make-string 16 ?o)))
               (current-buffer)))
      (should (equal (list :room-id "abcdefghijkl"
                           :key (make-string 32 ?k)
                           :write-token (make-string 16 ?w)
                           :owner-token (make-string 16 ?o))
                     (mevedel-collaboration-lobby--read-credentials path))))))

(mevedel-deftest mevedel-collaboration-lobby--credentials
  (:doc "creates private credentials once, reuses them, and rotates them")
  (mevedel-collaboration-lobby-test--with-root root
    (let* ((workspace (mevedel-collaboration-lobby-test--workspace root))
           (path (mevedel-collaboration-lobby--credentials-path workspace))
           (first (mevedel-collaboration-lobby--credentials workspace)))
      (should (equal (file-name-concat root ".mevedel/" "lobby") path))
      (should (= #o600 (file-modes path)))
      (should (= 32 (length (plist-get first :key))))
      ;; A restart reads the same link back.
      (should (equal first (mevedel-collaboration-lobby--credentials
                            workspace)))
      (let ((rotated (mevedel-collaboration-lobby--credentials workspace t)))
        (should-not (equal (plist-get first :room-id)
                           (plist-get rotated :room-id)))
        (should-not (equal (plist-get first :key) (plist-get rotated :key)))
        (should (equal rotated (mevedel-collaboration-lobby--credentials
                                workspace)))
        (should (= #o600 (file-modes path))))
      ;; Unreadable credentials are replaced rather than trusted.
      (with-temp-file path (insert "garbage"))
      (let ((fresh (mevedel-collaboration-lobby--credentials workspace)))
        (should (= 16 (length (plist-get fresh :owner-token))))
        (should (equal fresh (mevedel-collaboration-lobby--credentials
                              workspace)))))))

(mevedel-deftest mevedel-collaboration-lobby--preview
  (:doc "prefers the latest prompt, flattens whitespace, and bounds length")
  (progn
    (should-not (mevedel-collaboration-lobby--preview nil))
    (should (equal "first"
                   (mevedel-collaboration-lobby--preview
                    '(:first-user-message "first"))))
    (should (equal "latest words"
                   (mevedel-collaboration-lobby--preview
                    '(:first-user-message "first"
                      :latest-user-message "  latest\n\n words "))))
    (should (= mevedel-collaboration-lobby--max-preview-chars
               (string-width
                (mevedel-collaboration-lobby--preview
                 (list :latest-user-message (make-string 500 ?x))))))))

(mevedel-deftest mevedel-collaboration-lobby--updated
  (:doc "converts a sidecar timestamp to epoch seconds")
  (progn
    (should-not (mevedel-collaboration-lobby--updated nil))
    (should-not (mevedel-collaboration-lobby--updated '(:updated-at "never")))
    (should (= (truncate (float-time (encode-time
                                      (list 14 49 0 15 9 2026 nil -1 nil))))
               (mevedel-collaboration-lobby--updated
                '(:updated-at "2026-09-15T00-49-14"))))))

(mevedel-deftest mevedel-collaboration-lobby--live-sessions
  (:doc "keys live root sessions by session id")
  (let ((buffer (generate-new-buffer " *lobby-live*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "s1")))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace) `(("design" . ,buffer)))))
            (should (equal `(("s1" "design" . ,buffer))
                           (mevedel-collaboration-lobby--live-sessions
                            'workspace)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-collaboration-lobby--rows
  (:doc "lists unsaved live sessions first and marks open saved ones")
  (let ((live-buffer (generate-new-buffer " *lobby-live*"))
        (shared-buffer (generate-new-buffer " *lobby-shared*"))
        (dedicated nil))
    (unwind-protect
        (let ((mevedel-collaboration--rooms
               (mevedel-test-room-registry
                (list :data-buffer shared-buffer))))
          (with-current-buffer live-buffer
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "fresh")))
          (with-current-buffer shared-buffer
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "s2")))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace)
                       `(("new" . ,live-buffer) ("renamed" . ,shared-buffer))))
                    ((symbol-function 'mevedel-artifact-store-dedicated-ids)
                     (lambda (_workspace) dedicated))
                    ((symbol-function
                      'mevedel-session-persistence-list-sessions)
                     (lambda (_workspace &optional _cached)
                       '((:save-path "/s2/"
                          :summary (:session-id "s2" :session-name "old"
                                    :updated-at "2026-09-15T00-49-14"
                                    :latest-user-message "hello"))
                         (:save-path "/s3/"
                          :summary (:session-id "s3"))))))
            (let ((rows (mevedel-collaboration-lobby--rows 'workspace)))
              (should (equal '("fresh" "s2" "s3")
                             (mapcar (lambda (row) (plist-get row :id)) rows)))
              (should (equal '(:id "fresh" :name "new" :updated nil
                               :preview nil :live t :shared :json-false)
                             (nth 0 rows)))
              ;; The live name wins over the saved one, and an open
              ;; shared session says so.
              (should (equal "renamed" (plist-get (nth 1 rows) :name)))
              (should (eq t (plist-get (nth 1 rows) :shared)))
              (should (equal "hello" (plist-get (nth 1 rows) :preview)))
              (should (integerp (plist-get (nth 1 rows) :updated)))
              (should (equal '(:id "s3" :name "Untitled" :updated nil
                               :preview nil :live :json-false
                               :shared :json-false)
                             (nth 2 rows))))
            ;; An artifact's conversation is reached from its artifact.
            (setq dedicated '("fresh" "s3"))
            (should (equal '("s2")
                           (mapcar (lambda (row) (plist-get row :id))
                                   (mevedel-collaboration-lobby--rows 'workspace))))))
      (kill-buffer live-buffer)
      (kill-buffer shared-buffer))))

(mevedel-deftest mevedel-collaboration-lobby--frame
  (:doc "caps the listing and reports how many rows it left out")
  (let ((mevedel-collaboration-lobby--max-sessions 2))
    (cl-letf (((symbol-function 'mevedel-collaboration-lobby--rows)
               (lambda (_workspace) '((:id "a") (:id "b") (:id "c"))))
              ((symbol-function 'mevedel-collaboration--workspace-key)
               (lambda (lobby) (format "key-%s" (plist-get lobby :workspace)))))
      (let ((frame (mevedel-collaboration-lobby--frame
                    '(:workspace w :project "proj"))))
        (should (equal "lobby" (plist-get frame :t)))
        (should (equal "proj" (plist-get frame :project)))
        (should (equal "key-w" (plist-get frame :workspace)))
        (should (equal [(:id "a") (:id "b")] (plist-get frame :sessions)))
        (should (= 1 (plist-get frame :omitted)))
        ;; Rows travel as a JSON array of objects.
        (should (string-prefix-p "{\"t\":\"lobby\",\"project\":\"proj\",\"workspace\":\"key-w\",\"sessions\":[{"
                                 (json-encode frame)))))))

(mevedel-deftest mevedel-collaboration-lobby--session-buffer
  (:doc "uses a live session, restores a listed one, and refuses others")
  (let ((live (generate-new-buffer " *lobby-live*"))
        restored)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "s1")))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace) `(("one" . ,live))))
                    ((symbol-function
                      'mevedel-session-persistence-list-sessions)
                     (lambda (_workspace &optional _cached)
                       '((:save-path "/sessions/s2/"
                          :summary (:session-id "s2")))))
                    ((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (dir _source _override workspace)
                       (setq restored (list dir workspace))
                       'restored-buffer)))
            (let ((lobby '(:workspace ws)))
              (should (eq live (mevedel-collaboration-lobby--session-buffer
                                lobby "s1")))
              (should-not restored)
              (should (eq 'restored-buffer
                          (mevedel-collaboration-lobby--session-buffer
                           lobby "s2")))
              (should (equal '("/sessions/s2/" ws) restored))
              ;; An id is a selection among listed sessions, never a path.
              (should-error (mevedel-collaboration-lobby--session-buffer
                             lobby "../../etc")))))
      (kill-buffer live))))

(mevedel-deftest mevedel-collaboration-lobby--handle-open
  (:doc "hands writable guests the session room at their own tier")
  (let* ((guests (make-hash-table :test #'eql))
         (lobby (list :transport 'transport :guests guests :workspace 'ws))
         (session (mevedel-collaboration-lobby-test--session "s1"))
         (buffer (generate-new-buffer " *lobby-open*"))
         (room '(:link-view "v" :link-full "f" :link-owner "o"))
         sent started)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (puthash 1 '(:name "Viewer") guests)
          (puthash 2 '(:name "Writer" :writable t) guests)
          (puthash 3 '(:name "Owner" :writable t :owner t) guests)
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent) t))
                    ((symbol-function 'mevedel-collaboration-lobby--session-buffer)
                     (lambda (_lobby id)
                       (pcase id
                         ("s1" buffer)
                         ("asks" (signal 'inhibited-interaction nil))
                         (_ (error "No such session")))))
                    ((symbol-function 'mevedel-collaboration--start)
                     (lambda (started-session data-buffer)
                       (setq started (list started-session data-buffer))
                       room)))
            (let ((open (lambda (peer id)
                          (setq sent nil)
                          (mevedel-collaboration-lobby--handle-open
                           lobby peer (list :reqId 7 :id id))
                          (cdar sent))))
              (should (equal '(:t "open-session" :reqId 7 :ok :json-false
                               :message "A view link cannot open sessions")
                             (funcall open 1 "s1")))
              (should-not started)
              (should (equal '(:t "open-session" :reqId 7 :ok t :link "f")
                             (funcall open 2 "s1")))
              (should (equal (list session buffer) started))
              (should (equal "o" (plist-get (funcall open 3 "s1") :link)))
              ;; Nobody may be at the keyboard to answer a question.
              (should (equal "This session needs a decision in Emacs first"
                             (plist-get (funcall open 3 "asks") :message)))
              (should (string-match-p
                       "could not be opened: No such session"
                       (plist-get (funcall open 3 "gone") :message)))
              (should (eq :json-false (plist-get (funcall open 3 "") :ok)))
              ;; Neither an unknown peer nor a malformed request id is
              ;; answered.
              (should-not (funcall open 9 "s1"))
              (setq sent nil)
              (mevedel-collaboration-lobby--handle-open
               lobby 3 '(:reqId "x" :id "s1"))
              (should-not sent))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-collaboration-lobby--handle-delete
  (:doc "deletes an owner's saved session and relists it for every guest")
  (let* ((guests (make-hash-table :test #'eql))
         (lobby (list :transport 'transport :guests guests :workspace 'ws))
         (live (generate-new-buffer " *lobby-delete-live*"))
         (in-use nil)
         sent deleted)
    (unwind-protect
        (progn
          (with-current-buffer live
            (setq-local mevedel--session
                        (mevedel-collaboration-lobby-test--session "live")))
          (puthash 2 '(:name "Writer" :writable t) guests)
          (puthash 3 '(:name "Owner" :writable t :owner t) guests)
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame)
                       (push (cons peer frame) sent) t))
                    ((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_workspace) `(("draw" . ,live))))
                    ((symbol-function
                      'mevedel-session-persistence-list-sessions)
                     (lambda (_workspace &optional _cached)
                       '((:save-path "/sessions/old/"
                          :summary (:session-id "old")))))
                    ((symbol-function 'mevedel-session-persistence-delete)
                     (lambda (workspace path)
                       (should (eq 'ws workspace))
                       (when (eq in-use 'error) (error "Disk gone"))
                       (unless in-use (push path deleted))))
                    ((symbol-function 'mevedel-collaboration-lobby--frame)
                     (lambda (_lobby) '(:t "lobby"))))
            (let ((delete (lambda (peer id)
                            (setq sent nil)
                            (mevedel-collaboration-lobby--handle-delete
                             lobby peer (list :reqId 4 :id id))
                            (plist-get (cdar (last sent)) :message))))
              (should (equal "Only an owner link can delete sessions"
                             (funcall delete 2 "old")))
              ;; Its buffer would save a live session straight back.
              (should (equal "Close draw in Emacs first"
                             (funcall delete 3 "live")))
              (should (equal "No such session" (funcall delete 3 "../x")))
              (should (equal "No session named" (funcall delete 3 nil)))
              (setq in-use t)
              (should (equal "The session is still in use elsewhere"
                             (funcall delete 3 "old")))
              (setq in-use 'error)
              (should (equal "Session could not be deleted: Disk gone"
                             (funcall delete 3 "old")))
              (should-not deleted)
              (setq in-use nil)
              (funcall delete 3 "old")
              (should (equal '("/sessions/old/") deleted))
              (should (equal '((3 :t "delete-session" :reqId 4 :ok t))
                             (last sent)))
              ;; Every guest's list drops the deleted row.
              (should (equal '(2 3) (sort (mapcar #'car (butlast sent)) #'<)))
              (should (cl-every (lambda (entry)
                                  (equal "lobby" (plist-get (cdr entry) :t)))
                                (butlast sent))))))
      (kill-buffer live))))

(mevedel-deftest mevedel-collaboration-lobby--on-frame
  (:doc "admits guests, routes lobby frames, and contains failures")
  (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (lobby (list :transport 'transport
                      :guests (make-hash-table :test #'eql)
                      :write-token (make-string 16 ?w)
                      :owner-token (make-string 16 ?o)))
         sent routed interaction)
    (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) sent) t))
              ((symbol-function 'mevedel-collaboration-lobby--frame)
               (lambda (_lobby) '(:t "lobby")))
              ((symbol-function 'mevedel-model-candidates)
               (lambda () '(("Codex:gpt-6-luna" . provider))))
              ((symbol-function 'mevedel-collaboration-lobby--handle-open)
               (lambda (_lobby peer _frame)
                 (setq interaction inhibit-interaction)
                 (push (cons 'open peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-new-session)
               (lambda (room peer _frame)
                 (should (eq lobby room))
                 (push (cons 'new peer) routed)))
              ((symbol-function 'mevedel-collaboration-lobby--handle-delete)
               (lambda (_lobby peer _frame)
                 (push (cons 'delete peer) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-list)
               (lambda (_lobby peer _frame root)
                 (push (list 'files peer root) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-get)
               (lambda (_lobby peer _frame root)
                 (push (list 'file-get peer root) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-upload)
               (lambda (_lobby peer _frame root)
                 (push (list 'file-upload peer root) routed)))
              ((symbol-function 'mevedel-collaboration-files-handle-remove)
               (lambda (_lobby peer _frame root)
                 (push (list 'file-remove peer root) routed)))
              ((symbol-function 'mevedel-collaboration--handle-artifact-get)
               (lambda (_lobby peer _frame) (push (list 'artifact-get peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-artifact-delete)
               (lambda (_lobby peer _frame) (push (list 'artifact-delete peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-artifact-comment)
               (lambda (_lobby peer _frame) (push (list 'artifact-comment peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-store-list)
               (lambda (_lobby peer _frame) (push (list 'store-list peer) routed)))
              ((symbol-function 'mevedel-collaboration--handle-store-action)
               (lambda (_lobby peer _frame) (push (list 'store-action peer) routed))))
      ;; A refresh from a peer that never said hello gets nothing.
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "lobby-refresh"))
      (should-not sent)
      (mevedel-collaboration-lobby--on-frame
       "/root/" 1
       (list :t "hello" :proto mevedel-collaboration--protocol-version
             :name "Phone"
             :writeToken (mevedel-collaboration--base64url
                          (make-string 16 ?w))))
      (should (equal '((1 :t "lobby")) sent))
      (should (plist-get (mevedel-collaboration--guest lobby 1) :writable))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "lobby-refresh"))
      (should (= 2 (length sent)))
      ;; Only an owner can create a session here, so only it learns the
      ;; models to create one on.
      (mevedel-collaboration-lobby--on-frame
       "/root/" 2
       (list :t "hello" :proto mevedel-collaboration--protocol-version
             :name "Owner"
             :writeToken (mevedel-collaboration--base64url
                          (make-string 16 ?w))
             :ownerToken (mevedel-collaboration--base64url
                          (make-string 16 ?o))))
      (should (equal '(2 :t "lobby" :models ["Codex:gpt-6-luna"]) (car sent)))
      (setq sent (cdr sent))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "open-session"))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "new-session"))
      (mevedel-collaboration-lobby--on-frame "/root/" 1 '(:t "delete-session"))
      (should (equal '((delete . 1) (new . 1) (open . 1)) routed))
      (should (eq t interaction))
      ;; Project file frames reach the files module with the lobby's root.
      (setq routed nil)
      (dolist (type '("files" "file-get" "file-upload" "file-remove"))
        (mevedel-collaboration-lobby--on-frame "/root/" 1 (list :t type)))
      (should (equal '((file-remove 1 "/root/") (file-upload 1 "/root/")
                       (file-get 1 "/root/") (files 1 "/root/"))
                     routed))
      ;; The workspace's artifacts are reached by store identity.
      (setq routed nil)
      (dolist (type '("artifact-get" "artifact-delete" "artifact-comment"
                      "store-list" "store-action"))
        (mevedel-collaboration-lobby--on-frame "/root/" 1 (list :t type)))
      (should (equal '((store-action 1) (store-list 1) (artifact-comment 1)
                       (artifact-delete 1) (artifact-get 1))
                     routed))
      (setq routed '((new . 1) (open . 1)))
      ;; Another workspace's frames never reach this lobby.
      (mevedel-collaboration-lobby--on-frame "/other/" 1 '(:t "open-session"))
      (should (= 2 (length routed)))
      ;; A failing handler is reported, and the lobby keeps working.
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby--handle-open)
                 (lambda (&rest _) (error "Boom"))))
        (let (captured)
          (mevedel-test--with-captured-messages captured
            (mevedel-collaboration-lobby--on-frame
             "/root/" 1 '(:t "open-session")))
          (should (string-match-p "lobby frame failed: Boom" captured))))
      (should (eq lobby (gethash "/root/"
                                 mevedel-collaboration-lobby--lobbies))))))

(mevedel-deftest mevedel-collaboration-lobby--on-control
  (:doc "forgets a guest that left")
  (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (guests (make-hash-table :test #'eql)))
    (puthash "/root/" (list :guests guests)
             mevedel-collaboration-lobby--lobbies)
    (puthash 1 '(:name "a") guests)
    (puthash 2 '(:name "b") guests)
    (mevedel-collaboration-lobby--on-control "/root/" 'peer-joined 3)
    (mevedel-collaboration-lobby--on-control "/root/" 'peer-left 1)
    (should (equal '(2) (hash-table-keys guests)))))

(mevedel-deftest mevedel-collaboration-lobby--on-state
  (:doc "drops every guest when the relay connection goes down")
  (let* ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (guests (make-hash-table :test #'eql)))
    (puthash "/root/" (list :guests guests)
             mevedel-collaboration-lobby--lobbies)
    (puthash 1 '(:name "a") guests)
    (mevedel-collaboration-lobby--on-state "/root/" 'open)
    (should (= 1 (hash-table-count guests)))
    (mevedel-collaboration-lobby--on-state "/root/" 'down)
    (should (= 0 (hash-table-count guests)))))

(mevedel-deftest mevedel-collaboration-lobby--workspace
  (:doc "refuses a directory that does not exist")
  (should-error (mevedel-collaboration-lobby--workspace
                 "/nonexistent/mevedel-lobby/")
                :type 'user-error))

(mevedel-deftest mevedel-collaboration-lobby-start
  (:doc "starts one lobby per workspace whose links survive a restart")
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (mevedel-collaboration-relay-url "wss://relay.example")
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace root))
          opened stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby--workspace)
                 (lambda (_directory) workspace))
                ((symbol-function 'mevedel-collaboration--transport-open)
                 (lambda (url _key &rest _callbacks)
                   (push url opened)
                   (list :url url)))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 (lambda (transport) (push transport stopped)))
                ((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (&rest _) t)))
        (let ((lobby (mevedel-collaboration-lobby-start root)))
          (should (eq lobby (mevedel-collaboration-lobby--find workspace)))
          (should (equal "Lobby: proj" (plist-get lobby :session-label)))
          (should (equal root (plist-get lobby :directory)))
          (should (string-match-p
                   "\\`https://relay.example/#[A-Za-z0-9_-]+\\."
                   (plist-get lobby :link-owner)))
          (should (equal (list (format "wss://relay.example/r/%s?role=host"
                                       (plist-get lobby :room-id)))
                         opened))
          (should (memq #'mevedel-collaboration-lobby--stop-all
                        kill-emacs-hook))
          ;; It runs until stopped, so the next Emacs restarts it.
          (should (equal (list root)
                         (mevedel-collaboration-lobby--intended)))
          ;; A second start is the same lobby, not a second room.
          (should (eq lobby (mevedel-collaboration-lobby-start root)))
          (should (= 1 (length opened)))
          (mevedel-collaboration-lobby--stop lobby 'user-stop)
          (should-not (mevedel-collaboration-lobby--find workspace))
          (should-not (mevedel-collaboration-lobby--intended))
          (should-not kill-emacs-hook)
          ;; Restarting revives the very same links.
          (let ((again (mevedel-collaboration-lobby-start root)))
            (should-not (eq lobby again))
            (should (equal (plist-get lobby :link-owner)
                           (plist-get again :link-owner)))
            (mevedel-collaboration-lobby--stop again 'user-stop))
          (should (= 2 (length stopped))))))))

(mevedel-deftest mevedel-collaboration-lobby--stop
  ()
  ,test
  (test)
  :doc "says goodbye to connected guests except when Emacs exits"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (kill-emacs-hook (list #'mevedel-collaboration-lobby--stop-all))
          (workspace (mevedel-collaboration-lobby-test--workspace "/root/"))
          sent stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer frame) sent) t))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 (lambda (transport) (push transport stopped))))
        (let ((lobby (mevedel-collaboration-lobby-test--lobby workspace)))
          (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
          (mevedel-collaboration-lobby--stop lobby 'user-stop)
          ;; Nobody connected, nobody told.
          (should-not sent)
          (should (equal '(transport) stopped))
          (should-not kill-emacs-hook))
        (let ((lobby (mevedel-collaboration-lobby-test--lobby workspace)))
          (puthash 1 '(:name "a") (plist-get lobby :guests))
          (mevedel-collaboration-lobby--stop lobby 'rotated)
          (should (equal '((0 :t "bye" :reason "rotated")) sent))
          (setq sent nil)
          (mevedel-collaboration-lobby--stop lobby 'emacs-exit)
          (should-not sent)))))

  :doc "only an Emacs exit leaves the lobby recorded to restart"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace "/root/")))
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-stop)
                 #'ignore))
        (mevedel-collaboration-lobby--set-intended "/root/" t)
        (mevedel-collaboration-lobby--stop
         (mevedel-collaboration-lobby-test--lobby workspace) 'emacs-exit)
        (should (equal '("/root/") (mevedel-collaboration-lobby--intended)))
        (mevedel-collaboration-lobby--stop
         (mevedel-collaboration-lobby-test--lobby workspace) 'user-stop)
        (should-not (mevedel-collaboration-lobby--intended))))))

(mevedel-deftest mevedel-collaboration-lobby--stop-all
  (:doc "stops every live lobby and keeps them recorded to restart")
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (kill-emacs-hook nil)
          stopped)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-stop)
                 (lambda (transport) (push transport stopped))))
        (dolist (root '("/a/" "/b/"))
          (mevedel-collaboration-lobby--set-intended root t)
          (puthash root
                   (mevedel-collaboration-lobby-test--lobby
                    (mevedel-collaboration-lobby-test--workspace root)
                    :transport root)
                   mevedel-collaboration-lobby--lobbies))
        (mevedel-collaboration-lobby--stop-all)
        (should (= 0 (hash-table-count mevedel-collaboration-lobby--lobbies)))
        (should (equal '("/a/" "/b/") (sort stopped #'string<)))
        (should (equal '("/a/" "/b/")
                       (mevedel-collaboration-lobby--intended)))))))

(mevedel-deftest mevedel-collaboration-lobby--status
  (:doc "reports each lobby without its secrets")
  (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal)))
    (should-not (mevedel-collaboration-lobby--status))
    (let ((lobby (mevedel-collaboration-lobby-test--lobby
                  (mevedel-collaboration-lobby-test--workspace "/root/")
                  :session-label "Lobby: proj")))
      (puthash 1 '(:name "a") (plist-get lobby :guests))
      (puthash "/root/" lobby mevedel-collaboration-lobby--lobbies)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-open-p)
                 (lambda (_transport) t)))
        (should (equal "Lobby: proj: relay connected; 1 guest"
                       (mevedel-collaboration-lobby--status)))))))

(mevedel-deftest mevedel-collaboration-lobby--intent-path
  (:doc "keeps the running lobbies in the user directory")
  (let ((mevedel-user-dir "/home/user/.mevedel/"))
    (should (equal "/home/user/.mevedel/lobbies.el"
                   (mevedel-collaboration-lobby--intent-path)))))

(mevedel-deftest mevedel-collaboration-lobby--intended
  (:doc "reads the recorded roots and ignores a damaged record")
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((path (mevedel-collaboration-lobby--intent-path)))
      (should-not (mevedel-collaboration-lobby--intended))
      (with-temp-file path (insert "(\"/a/\" 7 \"/b/\")"))
      (should (equal '("/a/" "/b/") (mevedel-collaboration-lobby--intended)))
      (with-temp-file path (insert "(\"/a/\""))
      (should-not (mevedel-collaboration-lobby--intended))
      (with-temp-file path (insert "(\"/a/\" . \"/b/\")"))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby--set-intended
  ()
  ,test
  (test)
  :doc "adds each root once and removes the record with the last one"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((path (mevedel-collaboration-lobby--intent-path)))
      (mevedel-collaboration-lobby--set-intended "/a/" t)
      (mevedel-collaboration-lobby--set-intended "/b/" t)
      (mevedel-collaboration-lobby--set-intended "/a/" t)
      (should (equal '("/a/" "/b/") (mevedel-collaboration-lobby--intended)))
      (mevedel-collaboration-lobby--set-intended "/a/" nil)
      (should (equal '("/b/") (mevedel-collaboration-lobby--intended)))
      (mevedel-collaboration-lobby--set-intended "/b/" nil)
      (should-not (file-exists-p path))
      ;; Forgetting what was never recorded writes nothing.
      (mevedel-collaboration-lobby--set-intended "/c/" nil)
      (should-not (file-exists-p path))))

  :doc "reports a record it cannot write instead of failing the lobby"
  (mevedel-collaboration-lobby-test--with-user-dir
    (let ((mevedel-user-dir (file-name-concat mevedel-user-dir "blocked/"))
          captured)
      ;; A file where the directory should be makes the write fail.
      (with-temp-file (directory-file-name mevedel-user-dir))
      (mevedel-test--with-captured-diagnostics captured
        (mevedel-collaboration-lobby--set-intended "/a/" t))
      (should (string-match-p "Could not record the lobby of /a/" captured))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby-restore
  ()
  ,test
  (test)
  :doc "restarts each recorded lobby and reports one that fails"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((broken (file-name-as-directory
                   (file-name-concat root "broken")))
          started captured)
      (make-directory broken)
      (mevedel-collaboration-lobby--set-intended broken t)
      (mevedel-collaboration-lobby--set-intended root t)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby-start)
                 (lambda (directory)
                   (push directory started)
                   (when (equal directory broken)
                     (user-error "No mevedel workspace at %s" directory)))))
        (mevedel-test--with-captured-diagnostics captured
          (mevedel-collaboration-lobby-restore)))
      (should (equal (list root broken) started))
      (should (string-match-p
               (regexp-quote (format "Lobby of %s not restarted" broken))
               captured))
      ;; A failed restart is retried with the next Emacs.
      (should (equal (list broken root)
                     (mevedel-collaboration-lobby--intended)))))

  :doc "an unavailable remote root cannot prompt or prevent later lobbies from restarting"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((remote "/ssh:unavailable:/project/")
          started captured
          (directory-p (symbol-function 'file-directory-p)))
      (mevedel-collaboration-lobby--set-intended remote t)
      (mevedel-collaboration-lobby--set-intended root t)
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (directory)
                   (if (equal directory remote)
                       (progn
                         (should inhibit-interaction)
                         (signal 'inhibited-interaction '("Remote authentication needed")))
                     (funcall directory-p directory))))
                ((symbol-function 'mevedel-collaboration-lobby-start)
                 (lambda (directory) (push directory started))))
        (mevedel-test--with-captured-diagnostics captured
          (mevedel-collaboration-lobby-restore)))
      (should (equal (list root) started))
      (should (string-match-p "not restarted" captured))
      (should (equal (list remote root) (mevedel-collaboration-lobby--intended)))))

  :doc "forgets a lobby whose directory is gone"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((gone (file-name-as-directory (file-name-concat root "gone")))
          started captured)
      (mevedel-collaboration-lobby--set-intended gone t)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby-start)
                 (lambda (directory) (push directory started))))
        (mevedel-test--with-captured-diagnostics captured
          (mevedel-collaboration-lobby-restore)))
      (should-not started)
      (should (string-match-p "forgotten" captured))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby-stop
  ()
  ,test
  (test)
  :doc "stops the running lobby so it does not restart with Emacs"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (mevedel-collaboration-relay-url "wss://relay.example")
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace root)))
      (cl-letf (((symbol-function 'mevedel-workspace)
                 (lambda (&optional _buffer) workspace))
                ((symbol-function 'mevedel-collaboration-lobby--workspace)
                 (lambda (_directory) workspace))
                ((symbol-function 'mevedel-collaboration--transport-open)
                 (lambda (url &rest _) (list :url url)))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 #'ignore))
        (mevedel-collaboration-lobby-start root)
        (mevedel-test--with-captured-messages nil
          (mevedel-collaboration-lobby-stop))
        (should-not (mevedel-collaboration-lobby--find workspace))
        (should-not (mevedel-collaboration-lobby--intended)))))

  :doc "ends the retries of a lobby that failed to restart"
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (workspace (mevedel-collaboration-lobby-test--workspace root))
          captured)
      (mevedel-collaboration-lobby--set-intended root t)
      (cl-letf (((symbol-function 'mevedel-workspace)
                 (lambda (&optional _buffer) workspace)))
        (mevedel-test--with-captured-messages captured
          (mevedel-collaboration-lobby-stop)))
      (should (string-match-p "no lobby is running" captured))
      (should-not (mevedel-collaboration-lobby--intended)))))

(mevedel-deftest mevedel-collaboration-lobby-rotate
  (:doc "replaces stored credentials and restarts a running lobby")
  (mevedel-collaboration-lobby-test--with-root root
    (let ((mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (mevedel-collaboration-relay-url "wss://relay.example")
          (kill-emacs-hook nil)
          (workspace (mevedel-collaboration-lobby-test--workspace root))
          presented)
      (cl-letf (((symbol-function 'mevedel-workspace)
                 (lambda (&optional _buffer) workspace))
                ((symbol-function 'mevedel-collaboration-lobby--workspace)
                 (lambda (_directory) workspace))
                ((symbol-function 'mevedel-collaboration--transport-open)
                 (lambda (url &rest _) (list :url url)))
                ((symbol-function 'mevedel-collaboration--transport-stop)
                 #'ignore)
                ((symbol-function 'mevedel-collaboration-share-present)
                 (lambda (lobby) (push lobby presented))))
        (let ((before (mevedel-collaboration-lobby--credentials workspace)))
          (mevedel-test--with-captured-messages nil
            (mevedel-collaboration-lobby-rotate))
          (should-not presented)
          (should-not (equal before (mevedel-collaboration-lobby--credentials
                                     workspace))))
        (let* ((running (mevedel-collaboration-lobby-start root))
               (old-link (plist-get running :link-owner)))
          (mevedel-collaboration-lobby-rotate)
          (should (= 1 (length presented)))
          (should-not (eq running (car presented)))
          (should-not (equal old-link (plist-get (car presented) :link-owner)))
          (mevedel-collaboration-lobby--stop (car presented) 'user-stop))))))

(provide 'test-mevedel-collaboration-lobby)
;;; test-mevedel-collaboration-lobby.el ends here
