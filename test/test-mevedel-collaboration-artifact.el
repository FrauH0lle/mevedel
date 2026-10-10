;;; test-mevedel-collaboration-artifact.el --- focused collaboration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for the extracted collaboration feature module.

;;; Code:

(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-agent-control)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-pending-inputs)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-view-agent)
(require 'mevedel-view-render)
(require 'mevedel-workspace)

(require 'mevedel-artifact-store)
(require 'mevedel-collaboration-artifact-projection)
(require 'mevedel-collaboration-artifact)

(mevedel-deftest mevedel-collaboration--artifact-fields
  (:doc "projects selected ApplyPatch render data inside the artifact store")
  (let* ((save-path (make-temp-file "mevedel-collab-artifacts-" t))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root save-path :name "w"))
         (dir (mevedel-artifact-store-directory workspace))
         (session (mevedel-session--create :name "s" :workspace workspace))
         (path (file-name-concat dir "mockup/index.html")))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (make-directory (file-name-directory path) t)
          (write-region "<h1>hi</h1>" nil path nil 'silent)
          (let ((fields (car (mevedel-collaboration--artifact-fields
                              `(:kind patch
                                :files ((:kind add :path ,path)))))))
            (should (equal "mockup/index.html" (plist-get fields :artifact)))
            (should (= 11 (plist-get fields :size)))
            (should (equal (expand-file-name path)
                           (plist-get fields :artifact-path)))
            (should-not (plist-member fields :missing)))
          (should-not
           (mevedel-collaboration--artifact-fields
            `(:kind patch :files
              ((:kind add :path ,(file-name-concat save-path "notes.html"))
               (:kind add :path ,(file-name-concat dir "mockup/meta.el"))
               (:kind add :path ,(file-name-concat dir "mockup/versions/000001.html"))
               (:kind delete :path ,path)))))
          (should-not (mevedel-collaboration--artifact-fields
                       `(:kind media :files ((:kind add :path ,path)))))
          ;; The stat is memoized against per-tick target round trips,
          ;; and a settling ApplyPatch drops the entry so the card follows.
          (write-region "<h1>hello again</h1>" nil path nil 'silent)
          (should (= 11 (plist-get
                         (car (mevedel-collaboration--artifact-fields
                               `(:kind patch
                                 :files ((:kind update :path ,path)))))
                         :size)))
          (mevedel-collaboration--artifact-stat-invalidate)
          (should (= 20 (plist-get
                         (car (mevedel-collaboration--artifact-fields
                               `(:kind patch
                                 :files ((:kind update :path ,path)))))
                         :size)))
          ;; A deleted artifact still projects, marked missing, so it
          ;; reads as deleted rather than as a gap in the log.
          (delete-file path)
          (mevedel-collaboration--artifact-stat-invalidate)
          (let ((fields (car (mevedel-collaboration--artifact-fields
                              `(:kind patch
                                :files ((:kind update :path ,path)))))))
            (should (eq t (plist-get fields :missing)))
            (should-not (plist-member fields :size)))
          ;; Without a session there is no artifact store at all.
          (with-temp-buffer
            (should-not (mevedel-collaboration--artifact-fields
                         `(:kind patch
                           :files ((:kind add :path ,path))))))
          ;; A remote session's TRAMP directory accepts the model's
          ;; target-native path and maps host I/O back to the TRAMP form.
          (with-temp-buffer
            (setq-local mevedel--session
                        (mevedel-session--create
                         :name "r"
                         :workspace (mevedel-workspace--create
                                     :type 'project :id "r"
                                     :root "/ssh:example:/base/" :name "r")))
            (cl-letf (((symbol-function
                        'mevedel-collaboration--artifact-stat)
                       (lambda (seen)
                         (should (equal "/ssh:example:/base/.mevedel/artifacts/m/m.html"
                                        seen))
                         (cons 5 nil))))
              (let ((fields (car (mevedel-collaboration--artifact-fields
                                  '(:kind patch
                                    :files
                                    ((:kind add
                                      :path "/base/.mevedel/artifacts/m/m.html")))))))
                (should (equal "m/m.html" (plist-get fields :artifact)))
                (should (equal "/ssh:example:/base/.mevedel/artifacts/m/m.html"
                               (plist-get fields :artifact-path)))))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory save-path t))))


(mevedel-deftest mevedel-collaboration--tool-segment-records
  (:doc "expands selected ApplyPatch files into stable artifact cards")
  (let* ((save-path (make-temp-file "mevedel-collab-patch-artifacts-" t))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root save-path :name "w"))
         (dir (mevedel-artifact-store-directory workspace))
         (one (file-name-concat dir "a/one.html"))
         (two (file-name-concat dir "a/two.md"))
         (code (file-name-concat save-path "code.el"))
         parsed)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session
                      (mevedel-session--create :name "s" :workspace workspace))
          (insert "tool")
          (make-directory (file-name-concat dir "a") t)
          (write-region "one" nil one nil 'silent)
          (write-region "two" nil two nil 'silent)
          (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
                     (lambda (_buffer _start _end) parsed)))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Applied patch: 2 changes"
                    :render-data
                    (:kind patch :files
                     ((:kind add :added 1 :deleted 0 :diff "" :path ,one)
                      (:kind add :added 1 :deleted 0 :diff "" :path ,two)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (equal '("a/one.html" "a/two.md")
                             (mapcar (lambda (record)
                                       (plist-get record :artifact))
                                     records)))
              (should (= 2 (length (delete-dups
                                    (mapcar (lambda (record)
                                              (plist-get record :id))
                                            records)))))
              (should (= 1 (length
                            (mevedel-collaboration--tool-records records)))))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Applied patch: 2 changes"
                    :render-data
                    (:kind patch :files
                     ((:kind update :added 1 :deleted 0 :diff "" :path ,code)
                      (:kind move :added 1 :deleted 0 :diff "" :path ,code :move-path ,one)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (= 2 (length records)))
              (should-not (plist-get (car records) :artifact))
              (should (equal "a/one.html"
                             (plist-get (cadr records) :artifact))))
            (setq parsed
                  `(:name "ApplyPatch" :args (:patch "patch")
                    :result "Error: patch failed"
                    :render-data
                    (:kind patch :files ((:kind add :added 1 :deleted 0 :diff "" :path ,one)))))
            (let ((records (mevedel-collaboration--tool-segment-records
                            (current-buffer) '(tool 1 5))))
              (should (= 1 (length records)))
              (should-not (plist-get (car records) :artifact)))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory save-path t))))


(mevedel-deftest mevedel-collaboration-notify-artifacts-changed
  (:doc "drops cached artifact stats at once and tells the workspace's rooms once")
  (let* ((data-buffer (generate-new-buffer " *collab-artifacts-data*"))
         (workspace (mevedel-workspace--create :type 'project :id "w"))
         (session (mevedel-session--create :name "artifacts" :workspace workspace))
         (room (list :session session :data-buffer data-buffer
                     :guests (make-hash-table :test #'eql)
                     :transport 'transport))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         (path (make-temp-file "mevedel-collab-stat-"))
         (mevedel-collaboration--store-notifications nil)
         published)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--schedule-publish)
                   (lambda (target) (push target published))))
          ;; Prime the memo, then delete behind it: the stale size
          ;; survives until this seam drops the cache.
          (should (= 0 (car (mevedel-collaboration--artifact-stat
                             (expand-file-name path)))))
          (delete-file path)
          (mevedel-collaboration-notify-artifacts-changed workspace)
          (mevedel-collaboration-notify-artifacts-changed workspace)
          (should-not published)
          (should (= 1 (length mevedel-collaboration--store-notifications)))
          (mevedel-collaboration--notify-store workspace)
          (should (equal (list room) published))
          (should-not mevedel-collaboration--store-notifications)
          (should (cdr (mevedel-collaboration--artifact-stat
                        (expand-file-name path))))
          ;; A workspace without rooms still drops the cache, publishes
          ;; nothing, and does not error.
          (let ((other (mevedel-workspace--create :type 'project :id "other")))
            (mevedel-collaboration-notify-artifacts-changed other)
            (mevedel-collaboration--notify-store other))
          (should (= 1 (length published))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (when (file-exists-p path) (delete-file path))
      (kill-buffer data-buffer))))


(defmacro mevedel-collaboration-artifact-test--with-store (&rest body)
  "Run BODY with WORKSPACE, its STORE, a SESSION, a ROOM and a LOBBY.
Artifact page is attached to SESSION, draft is not; SENT collects frames
as (PEER . FRAME), guest 1 reads and guest 2 writes in both rooms."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "mevedel-store-room-" t)))
          (workspace (mevedel-workspace--create :type 'file :id "w" :root root :name "w"))
          (store (mevedel-artifact-store-directory workspace))
          (data-buffer (generate-new-buffer " *store-room-data*"))
          (session (mevedel-session--create :name "s" :session-id "s1"
                                            :workspace workspace
                                            :authority-mode 'pid-lock))
          (room (list :session session :data-buffer data-buffer :transport 'room
                      :guests (make-hash-table :test #'eql)))
          (lobby (list :workspace workspace :transport 'lobby
                       :guests (make-hash-table :test #'eql)))
          (mevedel-collaboration--rooms (mevedel-test-room-registry room))
          (mevedel-collaboration--store-notifications nil)
          sent)
     (dolist (owner (list room lobby))
       (puthash 1 (list :name "viewer" :writable nil :ready t) (plist-get owner :guests))
       (puthash 2 (list :name "writer" :writable t :ready t) (plist-get owner :guests)))
     (unwind-protect
         (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                    (lambda (transport peer frame)
                      (push (list transport peer frame) sent) t))
                   ((symbol-function 'mevedel-collaboration--publish) #'ignore)
                   ((symbol-function 'mevedel-collaboration--schedule-publish) #'ignore)
                   ((symbol-function 'mevedel-collaboration-lobby--find)
                    (lambda (seen) (and (eq seen workspace) lobby)))
                   ((symbol-function 'mevedel-session-persistence-write-sidecar-now)
                    #'ignore))
           (dolist (entry '(("page" "index.html" "<p>page</p>") ("draft" "notes.md" "n")))
             (let ((path (file-name-concat store (car entry) (cadr entry))))
               (make-directory (file-name-directory path) t)
               (write-region (nth 2 entry) nil path nil 'silent)
               (mevedel-artifact-store-note-writes
                (if (equal (car entry) "page") session
                  (mevedel-session--create :workspace workspace))
                (list (list :action 'write :path path)))))
           ,@body)
       (dolist (pending mevedel-collaboration--store-notifications)
         (cancel-timer (cdr pending)))
       (kill-buffer data-buffer)
       (mevedel-collaboration--artifact-stat-invalidate)
       (delete-directory root t))))

(defun mevedel-collaboration-artifact-test--reply (sent transport)
  "Return the newest frame in SENT that went through TRANSPORT."
  (nth 2 (cl-find transport sent :key #'car)))

(mevedel-deftest mevedel-collaboration--artifact-target ()
  ,test
  (test)
  :doc "resolves store ids anywhere and cards by record, carrying their artifact"
  (mevedel-collaboration-artifact-test--with-store
    (let ((page (mevedel-collaboration--artifact-target lobby nil "artifact:page")))
      (should (equal "page" (plist-get page :store)))
      (should (equal "page/index.html" (plist-get page :artifact)))
      (should (equal (file-name-concat store "page" "index.html")
                     (plist-get page :artifact-path)))
      (should-not (plist-get page :missing)))
    (should-not (mevedel-collaboration--artifact-target lobby nil "artifact:nope"))
    (should-not (mevedel-collaboration--artifact-target lobby nil "artifact:../page"))
    (should-not (mevedel-collaboration--artifact-target lobby nil "tool-1"))
    (plist-put room :records (list (list :id "tool-1" :artifact "page/asset.html"
                                         :artifact-path "/x")))
    (should (equal "page" (plist-get (mevedel-collaboration--artifact-target
                                      room nil "tool-1")
                                     :store)))
    (delete-file (file-name-concat store "page" "index.html"))
    (should (plist-get (mevedel-collaboration--artifact-target room nil "artifact:page")
                       :missing))))

(mevedel-deftest mevedel-collaboration--store-rows ()
  ,test
  (test)
  :doc "lists the workspace store with attachment relative to the room"
  (mevedel-collaboration-artifact-test--with-store
    (let ((rows (append (plist-get (mevedel-collaboration--store-frame room) :artifacts) nil)))
      (should (equal '("draft" "page") (sort (mapcar (lambda (row) (plist-get row :id)) rows)
                                             #'string<)))
      (let ((page (cl-find "page" rows :key (lambda (row) (plist-get row :id)) :test #'equal)))
        (should (eq t (plist-get page :attached)))
        (should (equal "html" (plist-get page :kind)))
        (should (equal "page/index.html" (plist-get page :artifact)))
        (should (eq :json-false (plist-get page :item)))
        (should (= 1 (plist-get page :versions)))
        (should (integerp (plist-get page :modified)))))
    (should (cl-every (lambda (row) (eq :json-false (plist-get row :attached)))
                      (plist-get (mevedel-collaboration--store-frame lobby) :artifacts)))
    ;; Unreadable metadata leaves its artifact out, not the listing.
    (write-region "(:kind" nil (file-name-concat store "draft" "meta.el") nil 'silent)
    (should (equal '("page") (mapcar (lambda (row) (plist-get row :id))
                                     (mevedel-collaboration--store-rows workspace))))))

(mevedel-deftest mevedel-collaboration--handle-store-action ()
  ,test
  (test)
  :doc "lists versions for any link and changes the store for writable links"
  (mevedel-collaboration-artifact-test--with-store
    (cl-labels ((act (owner peer &rest frame)
                  (setq sent nil)
                  (mevedel-collaboration--handle-store-action
                   owner peer (append (list :reqId 5) frame))
                  (mevedel-collaboration-artifact-test--reply
                   sent (plist-get owner :transport))))
      (let ((versions (act lobby 1 :action "versions" :id "page")))
        (should (eq t (plist-get versions :ok)))
        (should (= 1 (plist-get (aref (plist-get versions :versions) 0) :n))))
      (should (string-match-p "not change" (plist-get (act lobby 1 :action "restore"
                                                           :id "page" :n 1)
                                                      :error)))
      (should (plist-get (act lobby 2 :action "versions" :id "../page") :error))
      ;; Restoring records a new version and tells every room, once.
      (should (= 2 (plist-get (act room 2 :action "restore" :id "page" :n 1) :n)))
      (mevedel-collaboration--notify-store workspace)
      (should (cl-find-if (lambda (entry)
                            (equal "store-artifacts" (plist-get (nth 2 entry) :t)))
                          sent))
      ;; Attaching needs a session; the lobby has none.
      (should (string-match-p "Open a session"
                              (plist-get (act lobby 2 :action "attach" :id "draft") :error)))
      (should (eq t (plist-get (act room 2 :action "attach" :id "draft") :ok)))
      (should (member "draft" (mevedel-session-attached-artifacts session)))
      ;; A duplicate is attached to the room's session.
      (should (equal "page-2" (plist-get (act room 2 :action "duplicate" :id "page"
                                              :newId "page-2")
                                         :id)))
      (should (member "page-2" (mevedel-session-attached-artifacts session)))
      (should (plist-get (act room 2 :action "duplicate" :id "page" :newId "../x") :error))
      ;; The conversation link comes from the dedicated session's own room.
      (cl-letf (((symbol-function 'mevedel-collaboration--store-conversation-link)
                 (lambda (_guest _workspace id) (concat "link-" id))))
        (should (equal "link-page"
                       (plist-get (act lobby 2 :action "conversation" :id "page") :link))))
      (should (plist-get (act lobby 2 :action "evil" :id "page") :error))
      ;; Whiteboards and documents keep manual versions and restore as edits.
      (should (string-match-p "File artifacts"
                              (plist-get (act room 2 :action "save-version" :id "page") :error)))
      (make-directory (file-name-concat store "board") t)
      (write-region "{}" nil (file-name-concat store "board" "state.json") nil 'silent)
      (mevedel-artifact-store-create-meta workspace "board" "state.json" 'whiteboard "Plan")
      (let (restored)
        (cl-letf (((symbol-function 'mevedel-shared-editing-save-version)
                   (lambda (_workspace _id session-id)
                     (should (equal "s1" session-id))
                     4))
                  ((symbol-function 'mevedel-shared-editing-restore)
                   (lambda (_workspace id n actor callback)
                     (should (equal (list "board" 1 "Guest: writer") (list id n actor)))
                     (setq restored callback))))
          (should (= 4 (plist-get (act room 2 :action "save-version" :id "board") :n)))
          ;; The guest learns of the restore once it is saved, or why not.
          (should-not (act room 2 :action "restore" :id "board" :n 1))
          (funcall restored '(:result (:revision 2)))
          (should (eq t (plist-get (mevedel-collaboration-artifact-test--reply sent 'room)
                                   :ok)))
          (should-not (act room 2 :action "restore" :id "board" :n 1))
          (funcall restored '(:error "Busy"))
          (should (equal "Busy" (plist-get (mevedel-collaboration-artifact-test--reply
                                            sent 'room)
                                           :error)))))
      ;; Its state never travels as a file.
      (setq sent nil)
      (mevedel-collaboration--handle-artifact-get lobby 1 '(:reqId 3 :id "artifact:board"))
      (should (stringp (plist-get (mevedel-collaboration-artifact-test--reply sent 'lobby)
                                  :error)))
      ;; Deleting: refused to view links, at once for a file, and once its
      ;; editing queue answers for a whiteboard.
      (should (string-match-p "not change" (plist-get (act lobby 1 :action "delete" :id "page")
                                                      :error)))
      (should (eq t (plist-get (act lobby 2 :action "delete" :id "page") :ok)))
      (should-not (file-exists-p (file-name-concat store "page")))
      (let (queued)
        (cl-letf (((symbol-function 'mevedel-shared-editing-call)
                   (lambda (_workspace request callback &rest _)
                     (should (equal '("delete" "board")
                                    (list (plist-get request :action) (plist-get request :id))))
                     (setq queued callback))))
          (should-not (act lobby 2 :action "delete" :id "board"))
          (funcall queued '(:error "Busy"))
          (should (equal "Busy" (plist-get (mevedel-collaboration-artifact-test--reply
                                            sent 'lobby)
                                           :error))))))))

(mevedel-deftest mevedel-collaboration--handle-store-list ()
  ,test
  (test)
  :doc "answers the sender with the store listing"
  (mevedel-collaboration-artifact-test--with-store
    (mevedel-collaboration--handle-store-list lobby 1 '(:t "store-list"))
    (let ((frame (mevedel-collaboration-artifact-test--reply sent 'lobby)))
      (should (equal "store-artifacts" (plist-get frame :t)))
      (should (= 2 (length (plist-get frame :artifacts))))))

  :doc "spends a guest's read budget before the host does more reading"
  (mevedel-collaboration-artifact-test--with-store
    (dotimes (_ 20)
      (mevedel-collaboration--handle-store-list lobby 1 '(:t "store-list")))
    (should (= (car mevedel-collaboration--store-read-budget) (length sent)))
    ;; A request that needs an answer says why it got none.
    (setq sent nil)
    (mevedel-collaboration--handle-store-action
     lobby 1 '(:reqId 4 :action "versions" :id "page"))
    (should (string-match-p "Too many requests"
                            (plist-get (mevedel-collaboration-artifact-test--reply
                                        sent 'lobby)
                                       :error)))
    ;; Another guest has its own budget.
    (setq sent nil)
    (mevedel-collaboration--handle-store-list lobby 2 '(:t "store-list"))
    (should (= 1 (length sent)))))

(mevedel-deftest mevedel-collaboration--artifact-mime
  (:doc "maps artifact extensions case-insensitively and defaults to octet-stream")
  (progn
    (should (equal "text/html"
                   (mevedel-collaboration--artifact-mime "Mockup.HTML")))
    (should (equal "text/markdown"
                   (mevedel-collaboration--artifact-mime "notes.md")))
    (should (equal "image/png"
                   (mevedel-collaboration--artifact-mime "shot.png")))
    (should (equal "application/octet-stream"
                   (mevedel-collaboration--artifact-mime "data.bin")))
    (should (equal "application/octet-stream"
                   (mevedel-collaboration--artifact-mime "noext")))))

(mevedel-deftest mevedel-collaboration--handle-artifact-delete
  (:doc "deletes a whole store artifact by card or store id for writable links only")
  (let* ((root (make-temp-file "mevedel-guest-artifact-delete-" t))
         ;; A file workspace, so the pid-lock session matches its authority.
         (workspace (mevedel-workspace--create :type 'file :id "w"
                                               :root root :name "w"))
         (dir (expand-file-name (mevedel-artifact-store-directory workspace)))
         (path (file-name-concat dir "mockup" "index.html"))
         (comments (file-name-concat dir "mockup" "comments.json"))
         (other (file-name-concat dir "other" "page.html"))
         (session (mevedel-session--create :name "s" :workspace workspace
                                           :authority-mode 'pid-lock))
         (guests (make-hash-table :test #'eql))
         (logo (file-name-concat dir "mockup" "logo.png"))
         (room (list :session session :guests guests :transport 'transport
                     :records
                     (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                                 :artifact "mockup/index.html" :artifact-path path)
                           (list :id "tool-2" :kind "tool" :name "ApplyPatch"
                                 :artifact "mockup/logo.png" :artifact-path logo))))
         sent)
    (puthash 1 (list :name "viewer" :writable nil :ready t) guests)
    (puthash 2 (list :name "writer" :writable t :ready t) guests)
    (unwind-protect
        (progn
          (dolist (file (list path comments other logo))
            (make-directory (file-name-directory file) t)
            (with-temp-file file (insert "x")))
          (mevedel-artifact-store-create-meta workspace "mockup" "index.html")
          (mevedel-artifact-store-create-meta workspace "other" "page.html")
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport peer frame) (push (cons peer frame) sent) t))
                    ((symbol-function 'mevedel-collaboration--publish) #'ignore))
            (cl-labels ((reply (peer id)
                          (setq sent nil)
                          (mevedel-collaboration--handle-artifact-delete
                           room peer (list :reqId 3 :id id))
                          (cdr (car sent))))
              (should (string-match-p "not delete" (plist-get (reply 1 "tool-1") :error)))
              (should (file-exists-p path))
              ;; Deleting goes by artifact, so a file beside its main one
              ;; does not take the whole artifact.
              (should (string-match-p "belongs to artifact mockup"
                                      (plist-get (reply 2 "tool-2") :error)))
              (should (file-exists-p logo))
              (should (eq t (plist-get (reply 2 "tool-1") :ok)))
              ;; The whole artifact goes, not only the carded file.
              (should-not (file-exists-p (file-name-concat dir "mockup")))
              (should-not (file-exists-p comments))
              (should (equal "artifact-delete" (plist-get (reply 2 "nope") :t)))
              (should (plist-get (reply 2 "nope") :error))
              ;; Any link may reach a store artifact by its id.
              (should (eq t (plist-get (reply 2 "artifact:other") :ok)))
              (should-not (file-exists-p (file-name-concat dir "other")))
              (should (plist-get (reply 2 "artifact:../x") :error)))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory root t))))

(mevedel-deftest mevedel-collaboration--handle-artifact-get
  (:doc "answers published artifacts in bounded chunks and refuses everything else")
  (let* ((save-path (make-temp-file "mevedel-guest-artifact-" t))
         (workspace (mevedel-workspace--create :type 'project :id "w"
                                               :root save-path :name "w"))
         (dir (expand-file-name (mevedel-artifact-store-directory workspace)))
         (path (file-name-concat dir "mockup.html"))
         (outside (file-name-concat save-path "outside.txt"))
         (escape (file-name-concat dir "escape.txt"))
         (session (mevedel-session--create :name "s" :workspace workspace))
         (guests (make-hash-table :test #'eql))
         (content (make-string 1000 ?x))
         (room (list :session session :guests guests :transport 'transport
                     :records
                     (list (list :id "tool-1" :kind "tool" :name "ApplyPatch"
                                 :artifact "mockup.html"
                                 :artifact-path path)
                           (list :id "tool-2" :kind "tool" :name "ApplyPatch"
                                 :artifact "escape"
                                 :artifact-path "/etc/passwd")
                           (list :id "tool-3" :kind "tool" :name "Bash"))))
         (now 1000.0)
         sent)
    ;; A read-only guest may fetch: the card is read state.
    (puthash 1 (list :name "viewer" :writable nil :ready t) guests)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport peer frame)
                     (push (cons peer frame) sent)
                     t))
                  ((symbol-function 'float-time)
                   (lambda (&optional _) now)))
          (make-directory dir t)
          (let ((coding-system-for-write 'binary))
            (write-region content nil path nil 'silent)
            (write-region "private" nil outside nil 'silent))
          (make-symbolic-link outside escape)
          (setf (plist-get room :records)
                (append (plist-get room :records)
                        (list (list :id "tool-4" :kind "tool" :name "ApplyPatch"
                                    :artifact "escape.txt"
                                    :artifact-path escape))))
          ;; An id outside the published set, a tool record without an
          ;; artifact, and a record whose path escaped the directory all
          ;; earn the same bounded refusal, touching no file.
          (dolist (id '("nope" "tool-3" "tool-2" "tool-4"))
            (setq now (+ now 2.0) sent nil)
            (mevedel-collaboration--handle-artifact-get
             room 1 (list :reqId 1 :id id))
            (should (= 1 (length sent)))
            (should (equal "artifact" (plist-get (cdr (car sent)) :t)))
            (should (stringp (plist-get (cdr (car sent)) :error))))
          ;; A published artifact answers final-flagged base64 chunks
          ;; carrying name, type, and decoded size, each under the wire
          ;; bound.
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-frame-json-bytes 600))
            (mevedel-collaboration--handle-artifact-get
             room 1 (list :reqId 7 :id "tool-1")))
          (setq sent (nreverse sent))
          (should (> (length sent) 1))
          (let ((first-frame (cdr (car sent))))
            (should (equal "artifact" (plist-get first-frame :t)))
            (should (= 7 (plist-get first-frame :reqId)))
            (should (equal "mockup.html" (plist-get first-frame :name)))
            (should (equal "text/html" (plist-get first-frame :mime)))
            (should (= 1000 (plist-get first-frame :size))))
          (should (eq :json-false
                      (plist-get (cdr (car sent)) :final)))
          (should (eq t (plist-get (cdr (car (last sent))) :final)))
          (dolist (entry sent)
            (should (<= (string-bytes (json-encode
                                       (cdr entry)))
                        600)))
          (should (equal content
                         (base64-decode-string
                          (mapconcat (lambda (entry)
                                       (plist-get (cdr entry) :data))
                                     sent))))
          ;; A repeat inside the throttle window is dropped silently.
          (setq now (+ now 0.5) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 8 :id "tool-1"))
          (should-not sent)
          ;; Empty files still carry metadata and one final empty chunk.
          (write-region "" nil path nil 'silent)
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 12 :id "tool-1"))
          (should (equal '((1 :t "artifact" :reqId 12 :id "tool-1"
                             :name "mockup.html" :mime "text/html"
                             :size 0 :data "" :final t))
                         sent))
          ;; A failed write stops a multi-chunk transfer immediately.
          (write-region content nil path nil 'silent)
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-frame-json-bytes 600))
            (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                       (lambda (_transport peer frame)
                         (push (cons peer frame) sent)
                         nil)))
              (mevedel-collaboration--handle-artifact-get
               room 1 (list :reqId 13 :id "tool-1"))))
          (should (= 1 (length sent)))
          (should (eq :json-false (plist-get (cdar sent) :final)))
          (should (= 600 (string-bytes (json-encode (cdar sent)))))
          ;; The read itself is capped at max+1, which is the authoritative
          ;; overflow check even if the file changes after containment.
          (setq now (+ now 2.0) sent nil)
          (let ((mevedel-collaboration--max-artifact-bytes 10)
                (read-function
                 (symbol-function 'insert-file-contents-literally))
                read-end)
            (cl-letf
                (((symbol-function 'insert-file-contents-literally)
                  (lambda (filename &optional visit beg end replace)
                    (setq read-end end)
                    (funcall read-function filename visit beg end replace))))
              (mevedel-collaboration--handle-artifact-get
               room 1 (list :reqId 9 :id "tool-1")))
            (should (= 11 read-end)))
          (should (string-match-p "too large"
                                  (plist-get (cdr (car sent)) :error)))
          ;; A deleted file is a targeted refusal, not a broken stream.
          (delete-file path)
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId 10 :id "tool-1"))
          (should (string-match-p "deleted"
                                  (plist-get (cdr (car sent)) :error)))
          ;; An unregistered peer and a malformed request id get nothing.
          (setq now (+ now 2.0) sent nil)
          (mevedel-collaboration--handle-artifact-get
           room 9 (list :reqId 11 :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId "11" :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId -1 :id "tool-1"))
          (mevedel-collaboration--handle-artifact-get
           room 1 (list :reqId #x20000000000000 :id "tool-1"))
          (should-not sent))
      (delete-directory save-path t))))


(provide 'test-mevedel-collaboration-artifact)
;;; test-mevedel-collaboration-artifact.el ends here
