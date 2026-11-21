;;; niwa-collaboration-artifact.el --- the artifact store in the browser -*- lexical-binding: t; -*-

;;; Commentary:

;; The workspace artifact store as browser guests see it.  Resolves
;; published cards and `artifact:ID' ids and sends artifact bytes on demand;
;; filesystem paths never cross the wire, and each read is re-authorized
;; against the store.  Lists the store to every room and the lobby, once per
;; change, and performs guests' store actions -- versions, restore, attach,
;; duplicate, conversation and delete -- within their link's tier.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)

;; `niwa-artifact-backend'
(declare-function niwa-artifact-backend-call "niwa-artifact-backend"
                  (workspace args callback &optional authorize commit))
(declare-function niwa-artifact-backend-catalog "niwa-artifact-backend"
                  (workspace &optional detail))
(declare-function niwa-artifact-backend-registered-p "niwa-artifact-backend"
                  (workspace id))
(autoload 'niwa-artifact-backend-call "niwa-artifact-backend")
(autoload 'niwa-artifact-backend-catalog "niwa-artifact-backend")
(autoload 'niwa-artifact-backend-registered-p "niwa-artifact-backend")

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-artifact-directory
                  "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-attach
                  "niwa-artifact-store" (session id &optional buffer))
(declare-function niwa-artifact-store-conversation
                  "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-delete
                  "niwa-artifact-store" (workspace id &optional actor callback))
(declare-function niwa-artifact-store-directory
                  "niwa-artifact-store" (workspace))
(declare-function niwa-artifact-store-duplicate
                  "niwa-artifact-store" (workspace id new-id))
(declare-function niwa-artifact-store-id-p "niwa-artifact-store" (id))
(declare-function niwa-artifact-store-item-p "niwa-artifact-store" (meta))
(declare-function niwa-artifact-store-meta "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-primary-path
                  "niwa-artifact-store" (workspace id &optional meta))
(declare-function niwa-artifact-store-restore-version
                  "niwa-artifact-store" (workspace id n &optional session-id actor callback))
(declare-function niwa-artifact-store-version-path
                  "niwa-artifact-store" (workspace id n))
(declare-function niwa-artifact-store-versions
                  "niwa-artifact-store" (workspace id))
(defvar niwa-artifact-store-changed-functions)
(defvar niwa-artifact-store-item-kinds)
(autoload 'niwa-artifact-store-artifact-directory "niwa-artifact-store")
(autoload 'niwa-artifact-store-attach "niwa-artifact-store")
(autoload 'niwa-artifact-store-conversation "niwa-artifact-store")
(autoload 'niwa-artifact-store-delete "niwa-artifact-store")
(autoload 'niwa-artifact-store-directory "niwa-artifact-store")
(autoload 'niwa-artifact-store-duplicate "niwa-artifact-store")
(autoload 'niwa-artifact-store-id-p "niwa-artifact-store")
(autoload 'niwa-artifact-store-item-p "niwa-artifact-store")
(autoload 'niwa-artifact-store-meta "niwa-artifact-store")
(autoload 'niwa-artifact-store-primary-path "niwa-artifact-store")
(autoload 'niwa-artifact-store-restore-version "niwa-artifact-store")
(autoload 'niwa-artifact-store-version-path "niwa-artifact-store")
(autoload 'niwa-artifact-store-versions "niwa-artifact-store")

;; `niwa-chat'
(declare-function niwa--workspace-sessions "niwa-chat" (workspace))
(autoload 'niwa--workspace-sessions "niwa-chat")

;; `niwa-collaboration'
(declare-function niwa-collaboration--broadcast
                  "niwa-collaboration" (room frame))
(declare-function niwa-collaboration--guest
                  "niwa-collaboration" (room peer))
(declare-function niwa-collaboration--guest-link
                  "niwa-collaboration" (room guest))
(declare-function niwa-collaboration--observer-failure
                  "niwa-collaboration" (room &optional err))
(declare-function niwa-collaboration--publish
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--room-data-buffer
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--room-list "niwa-collaboration" ())
(declare-function niwa-collaboration--start
                  "niwa-collaboration" (session data-buffer))
(defvar niwa-collaboration-needs-host-message)

;; `niwa-collaboration-artifact-projection'
(declare-function niwa-collaboration--artifact-revision
                  "niwa-collaboration-artifact-projection" (size modified))
(declare-function niwa-collaboration--artifact-stat-invalidate
                  "niwa-collaboration-artifact-projection" ())
(declare-function niwa-collaboration--artifacts-dir
                  "niwa-collaboration-artifact-projection" (session))
(autoload 'niwa-collaboration--artifact-revision
  "niwa-collaboration-artifact-projection")

;; `niwa-collaboration-artifact-room'
(declare-function niwa-collaboration-artifact-room-sharing
                  "niwa-collaboration-artifact-room" (workspace id operation))
(autoload 'niwa-collaboration-artifact-room-sharing
  "niwa-collaboration-artifact-room")

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--request-id-p
                  "niwa-collaboration-guest" (value))
(declare-function niwa-collaboration--room-workspace
                  "niwa-collaboration-guest" (room))

;; `niwa-collaboration-history'
(declare-function niwa-collaboration--history-artifacts
                  "niwa-collaboration-history" (room))
(autoload 'niwa-collaboration--history-artifacts
  "niwa-collaboration-history")

;; `niwa-collaboration-lobby'
(declare-function niwa-collaboration-lobby--find
                  "niwa-collaboration-lobby" (workspace))

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-send
                  "niwa-collaboration-transport" (transport peer frame))
(defvar niwa-collaboration--max-frame-json-bytes)

;; `niwa-resource'
(declare-function niwa-resource-within-root-p
                  "niwa-resource" (path root))
(autoload 'niwa-resource-within-root-p "niwa-resource")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-list-sessions
                  "niwa-session-persistence" (workspace &optional cached))
(autoload 'niwa-session-persistence-list-sessions "niwa-session-persistence")

;; `niwa-shared-editing'
(declare-function niwa-shared-editing-check-new "niwa-shared-editing" (kind title))
(declare-function niwa-shared-editing-create "niwa-shared-editing"
                  (workspace kind title actor callback &rest keys))
(declare-function niwa-shared-editing-save-version
                  "niwa-shared-editing" (workspace id &optional session-id))
(autoload 'niwa-shared-editing-check-new "niwa-shared-editing")
(autoload 'niwa-shared-editing-create "niwa-shared-editing")
(autoload 'niwa-shared-editing-save-version "niwa-shared-editing")

;; `niwa-structs'
(declare-function niwa-session-attached-artifacts "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--session)

;; `niwa-transport'
(declare-function niwa-transport-schedule-idle
                  "niwa-transport" (table key tag path thunk))
(autoload 'niwa-transport-schedule-idle "niwa-transport")

(defvar niwa-collaboration--artifact-notifications (make-hash-table :test #'eq)
  "Pending store notification timers keyed by workspace.")

(defconst niwa-collaboration--artifact-fetch-window 1.0
  "Seconds within which repeated artifact fetches from one guest drop.")

(defconst niwa-collaboration--store-read-budget '(8 . 2.0)
  "At most CAR store reads per guest within CDR seconds.
Listing the store, or an artifact's versions or discussions, reads files
for each one; a link that may only view must not keep the host busy.")

(defun niwa-collaboration--store-read-allowed-p (guest)
  "Count one store read for GUEST; return nil once its budget is spent."
  (let ((now (float-time))
        (window (plist-get guest :store-reads)))
    (unless (and window (< (- now (car window))
                           (cdr niwa-collaboration--store-read-budget)))
      (setq window (cons now 0))
      (plist-put guest :store-reads window))
    (<= (setcdr window (1+ (cdr window)))
        (car niwa-collaboration--store-read-budget))))

(defun niwa-collaboration--store-read (guest)
  "Count one store read for GUEST, or signal when its budget is spent."
  (unless (niwa-collaboration--store-read-allowed-p guest)
    (error "Too many requests; try again in a moment")))

(defconst niwa-collaboration--max-artifact-bytes (* 16 1024 1024)
  "Largest artifact file the host will send to a guest.")

(defconst niwa-collaboration--artifact-mime-types
  '(("html" . "text/html") ("htm" . "text/html")
    ("css" . "text/css") ("js" . "text/javascript")
    ("md" . "text/markdown") ("markdown" . "text/markdown")
    ("txt" . "text/plain") ("csv" . "text/csv")
    ("json" . "application/json")
    ("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("webp" . "image/webp") ("gif" . "image/gif")
    ("svg" . "image/svg+xml") ("pdf" . "application/pdf"))
  "Artifact file extensions and viewer transfer MIME types.")

(defun niwa-collaboration--artifact-mime (name)
  "Return the transfer MIME type for artifact file NAME."
  (or (cdr (assoc (downcase (or (file-name-extension name) ""))
                  niwa-collaboration--artifact-mime-types))
      "application/octet-stream"))

(defun niwa-collaboration--artifact-record (room guest id)
  "Return artifact record ID currently published to GUEST in ROOM."
  (when (stringp id)
    (cl-find-if
     (lambda (record)
       (and (equal id (plist-get record :id))
            (plist-get record :artifact)))
     (append (plist-get room :records)
             (niwa-collaboration--history-artifacts room)
             (plist-get guest :agent-artifacts)))))

(defun niwa-collaboration--store-target (workspace id)
  "Return store artifact ID of WORKSPACE as an artifact record, or nil.
The record names the artifact's primary file; it is marked missing when
that file is gone.  A remote artifact's record names no file: its origin
serves the primary one."
  (when (niwa-artifact-store-id-p id)
    (if-let* ((meta (niwa-artifact-store-meta workspace id))
              (file (plist-get meta :file)))
        (let ((path (niwa-artifact-store-primary-path workspace id meta)))
          (append (list :id (concat "artifact:" id) :store id
                        :artifact (concat id "/" file) :artifact-path path)
                  (when (niwa-artifact-store-item-p meta) (list :item t))
                  (unless (file-exists-p path) (list :missing t))))
      (when (niwa-artifact-backend-registered-p workspace id)
        (list :id (concat "artifact:" id) :store id :artifact (concat id "/"))))))

(defun niwa-collaboration--remote-artifact-get (room peer req-id record)
  "Answer PEER's REQ-ID in ROOM with remote RECORD's file, from its origin.
RECORD names the remote artifact as `:store' and the file after it in
`:artifact'; without one the origin's primary file is sent."
  (let* ((guest (niwa-collaboration--guest room peer))
         (id (plist-get record :store))
         (file (string-remove-prefix (concat id "/") (plist-get record :artifact))))
    (niwa-artifact-backend-call
     (niwa-collaboration--room-workspace room)
     (append (list :action "file" :id id)
             (unless (string-empty-p file) (list :file file)))
     (lambda (reply)
       (when (eq guest (niwa-collaboration--guest room peer))
         (if-let* ((failure (plist-get reply :error)))
             (niwa-collaboration--artifact-refuse room peer req-id failure)
           (let* ((result (plist-get reply :result))
                  (name (concat id "/" (plist-get result :file))))
             (niwa-collaboration--artifact-send-content
              room peer req-id (plist-put (copy-sequence record) :artifact name)
              (niwa-collaboration--artifact-mime name)
              (if (stringp (plist-get result :text))
                  (encode-coding-string (plist-get result :text) 'utf-8-unix)
                (base64-decode-string (or (plist-get result :data) "")))))))))))

(defun niwa-collaboration--artifact-target (room guest id)
  "Return the artifact a frame's ID names for GUEST in ROOM, or nil.
\"artifact:STORE-ID\" names a store artifact, which any link to the
workspace may reach; any other id names a card published to GUEST.  A
card in a store artifact's directory carries that artifact as `:store'."
  (if (and (stringp id) (string-prefix-p "artifact:" id))
      (niwa-collaboration--store-target
       (niwa-collaboration--room-workspace room) (substring id 9))
    (when-let* ((record (niwa-collaboration--artifact-record room guest id)))
      (let ((slash (string-search "/" (plist-get record :artifact))))
        (if slash
            (append (list :store (substring (plist-get record :artifact) 0 slash))
                    record)
          record)))))

(defun niwa-collaboration--artifact-refuse (room peer req-id message)
  "Send guest PEER a bounded artifact refusal for REQ-ID in ROOM."
  (niwa-collaboration--transport-send
   (plist-get room :transport) peer
   (list :t "artifact" :reqId req-id :error message)))

(defun niwa-collaboration--artifact-send-content (room peer req-id record mime content)
  "Send bounded CONTENT with MIME for RECORD to PEER's REQ-ID in ROOM."
  (let ((size (string-bytes content)))
    (if (> size niwa-collaboration--max-artifact-bytes)
        (niwa-collaboration--artifact-refuse
         room peer req-id
         (format "Artifact too large to send (%d MB); open it on the host"
                 (/ size 1024 1024)))
      (niwa-collaboration--send-chunked
       (plist-get room :transport) peer
       (list :t "artifact" :reqId req-id :id (plist-get record :id)
             :name (plist-get record :artifact) :mime mime :size size)
       content))))

(defun niwa-collaboration--handle-artifact-get (room peer frame)
  "Answer guest PEER's artifact fetch FRAME for ROOM.
An optional version number previews a historical file or editor export
without changing the live artifact."
  (let ((guest (niwa-collaboration--guest room peer))
        (req-id (plist-get frame :reqId))
        (now (float-time)))
    (when (and guest (niwa-collaboration--request-id-p req-id))
      (let ((last (plist-get guest :last-artifact-fetch)))
        (if (and last (< (- now last)
                         niwa-collaboration--artifact-fetch-window))
            (niwa-collaboration--artifact-refuse
             room peer req-id "Please wait a moment, then open the artifact again")
          (plist-put guest :last-artifact-fetch now)
          (condition-case err
              (let* ((record (niwa-collaboration--artifact-target
                              room guest (plist-get frame :id)))
                     (workspace (niwa-collaboration--room-workspace room))
                     (version (plist-get frame :version))
                     (id (plist-get record :store))
                     (remote (and id workspace
                                  (niwa-artifact-backend-registered-p workspace id)))
                     (path (if (plist-member frame :version)
                               (progn
                                 (unless (and id (natnump version))
                                   (error "No such version"))
                                 (when remote
                                   (error "Versions of a remote artifact stay at its origin"))
                                 (niwa-artifact-store-version-path workspace id version))
                             (plist-get record :artifact-path)))
                     (dir (and workspace (niwa-artifact-store-directory workspace))))
                (unless (or remote
                            (and dir path (niwa-resource-within-root-p path dir)
                                 (or version (not (plist-get record :item)))))
                  (error "This artifact is not published"))
                (cond
                 (remote (niwa-collaboration--remote-artifact-get
                          room peer req-id record))
                 ((and version (plist-get record :item))
                  (niwa-artifact-backend-call
                   workspace
                   (list :action "export" :id id :version version
                         :format (if (eq (plist-get (niwa-artifact-store-meta
                                                     workspace id) :kind) 'whiteboard)
                                     "svg" "html"))
                   (lambda (reply)
                     (when (eq guest (niwa-collaboration--guest room peer))
                       (if-let* ((failure (plist-get reply :error)))
                           (niwa-collaboration--artifact-refuse room peer req-id failure)
                         (let ((result (plist-get reply :result)))
                           (niwa-collaboration--artifact-send-content
                            room peer req-id record (plist-get result :mime)
                            (encode-coding-string (plist-get result :text) 'utf-8-unix))))))))
                 (t
                  (let ((content (condition-case nil
                                     (with-temp-buffer
                                       (set-buffer-multibyte nil)
                                       (insert-file-contents-literally
                                        path nil 0 (1+ niwa-collaboration--max-artifact-bytes))
                                       (buffer-string))
                                   (file-error (error "This artifact was deleted on the host")))))
                    (niwa-collaboration--artifact-send-content
                     room peer req-id record
                     (niwa-collaboration--artifact-mime (plist-get record :artifact))
                     content)))))
            (error (niwa-collaboration--artifact-refuse
                    room peer req-id (error-message-string err)))))))))

(defun niwa-collaboration--send-chunked (transport peer meta content)
  "Send unibyte CONTENT to PEER through TRANSPORT as base64 chunk frames.
Every frame carries the plist META, a `:data' slice, and `:final', true
on the last one; each stays under the wire bound.  Return non-nil when
every frame was written."
  (let* ((data (base64-encode-string content t))
         ;; Budget the metadata actually sent, with the longer non-final
         ;; marker and an empty data string.
         (overhead (string-bytes
                    (json-encode
                     (append meta '(:data "" :final :json-false)))))
         (chunk (max 1 (- niwa-collaboration--max-frame-json-bytes
                          overhead)))
         (total (length data))
         (start 0)
         (sent t)
         done)
    (while (and sent (not done))
      (let ((end (min total (+ start chunk))))
        (setq done (= end total)
              sent (niwa-collaboration--transport-send
                    transport peer
                    (append meta
                            (list :data (substring data start end)
                                  :final (if done t :json-false))))
              start end)))
    sent))

(defun niwa-collaboration--guest-actor (guest)
  "Return how edits by GUEST are attributed."
  (concat "Guest: " (plist-get guest :name)))

(defun niwa-collaboration--handle-artifact-delete (room peer frame)
  "Delete the published artifact FRAME names for writable guest PEER in ROOM.
The file comes from the host's own record, never from the frame.  A card
for a file beside an artifact's main one is refused, since deleting goes
by artifact.  The sender is answered once it is gone or was refused."
  (let ((guest (niwa-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (niwa-collaboration--request-id-p req-id))
      (let ((reply (lambda (fields)
                     (niwa-collaboration--transport-send
                      (plist-get room :transport) peer
                      (append (list :t "artifact-delete" :reqId req-id) fields)))))
        (condition-case err
            (let* ((record (niwa-collaboration--artifact-target
                            room guest (plist-get frame :id)))
                   (workspace (niwa-collaboration--room-workspace room))
                   (id (plist-get record :store))
                   (name (plist-get record :artifact))
                   (done (lambda (failure)
                           (funcall reply (if failure (list :error failure)
                                            (list :ok t :artifact name))))))
              (unless (plist-get guest :writable)
                (error "This link can view artifacts but not delete them"))
              (unless (and record (or id (not (plist-get record :missing))))
                (error "This artifact is no longer on the host"))
              (cond
               ;; A file written straight into the store is no artifact.
               ((null id)
                (delete-file (expand-file-name name (niwa-artifact-store-directory workspace)))
                (niwa-collaboration-notify-artifacts-changed workspace)
                (funcall done nil))
               ((not (equal name (concat id "/" (plist-get (niwa-artifact-store-meta workspace id)
                                                           :file))))
                (error "%s belongs to artifact %s; delete the artifact from its main file"
                       (file-name-nondirectory name) id))
               (t (niwa-artifact-store-delete
                   workspace id (niwa-collaboration--guest-actor guest) done))))
          (error (funcall reply (list :error (error-message-string err)))))))))

(defun niwa-collaboration--workspace-rooms (workspace)
  "Return WORKSPACE's live session rooms followed by its lobby, if any."
  (append
   (cl-remove-if-not
    (lambda (room)
      (when-let* ((session (plist-get room :session)))
        (eq workspace (niwa-session-workspace session))))
    (niwa-collaboration--room-list))
   (when-let* (((featurep 'niwa-collaboration-lobby))
               (lobby (niwa-collaboration-lobby--find workspace)))
     (list lobby))))

(defun niwa-collaboration-notify-artifacts-changed (workspace)
  "Invalidate artifact stats and schedule WORKSPACE's latest store listing.
Coalesce changes until the next transport-idle opportunity, after the current
mutation can acknowledge.  Resolve rooms and read the store when publishing."
  ;; Only a loaded collaboration can have a room to tell.
  (when (featurep 'niwa-collaboration)
    (niwa-collaboration--artifact-stat-invalidate)
    (when (niwa-collaboration--workspace-rooms workspace)
      (niwa-transport-schedule-idle
       niwa-collaboration--artifact-notifications workspace 'artifact-notifications
       (niwa-workspace-root workspace)
       (lambda ()
         (when-let* ((rooms (niwa-collaboration--workspace-rooms workspace)))
           (condition-case err
               (let (snapshot)
                 (dolist (room rooms)
                   (condition-case room-error
                       (progn
                         (when (plist-get room :session)
                           (niwa-collaboration--publish room))
                         ;; The listing is read once, and only for a room a
                         ;; guest is in.
                         (when (> (hash-table-count (plist-get room :guests)) 0)
                           (niwa-collaboration--broadcast
                            room (niwa-collaboration--store-frame
                                  room
                                  (or snapshot
                                      (setq snapshot
                                            (niwa-collaboration--store-snapshot
                                             workspace
                                             (cl-some (lambda (room) (not (plist-get room :session)))
                                                      rooms)
                                             t)))))))
                     (error (niwa-collaboration--observer-failure room room-error)))))
             (error
              (dolist (room rooms)
                (niwa-collaboration--observer-failure room err))))))))))


;;
;;; Store listing and actions

(defun niwa-collaboration--store-snapshot (workspace &optional count-attachments cached)
  "Read WORKSPACE's store once for a listing or notification fanout.
COUNT-ATTACHMENTS includes lobby attachment counts.  CACHED uses the last
session discovery for this decoration; live buffers always override it.
The catalog's sharing observations also record which artifacts have links."
  (let ((artifacts (niwa-artifact-backend-catalog workspace))
        (counts (make-hash-table :test #'equal))
        shared)
    (dolist (row artifacts)
      (when (plist-get row :shared)
        (push (plist-get row :id) shared)))
    (when (and artifacts count-attachments)
      (let ((sessions (make-hash-table :test #'equal)))
        (dolist (entry (niwa-session-persistence-list-sessions workspace cached))
          (let ((summary (plist-get entry :summary)))
            (puthash (plist-get summary :session-id)
                     (plist-get summary :attached-artifacts) sessions)))
        ;; A live session may have attachments not saved yet.  Replace its
        ;; durable entry instead of counting it twice.
        (dolist (entry (niwa--workspace-sessions workspace))
          (let ((session (buffer-local-value 'niwa--session (cdr entry))))
            (puthash (niwa-session-session-id session)
                     (niwa-session-attached-artifacts session) sessions)))
        (maphash (lambda (_session ids)
                   (dolist (id (delete-dups (copy-sequence ids)))
                     (puthash id (1+ (gethash id counts 0)) counts)))
                 sessions)))
    (list :artifacts artifacts :attached-counts counts :shared shared)))

(defun niwa-collaboration--store-rows (room &optional snapshot)
  "Return ROOM's workspace artifacts as guests receive them.
Attachment is relative to ROOM's session; a lobby has none.
SNAPSHOT, when given, supplies the store observation shared across rooms.
A row's revision changes with its artifact: for a local one with its
primary file's size and time, read with the listing; for a remote one with
the origin's last change event."
  (let* ((session (plist-get room :session))
         (attached (and session (niwa-session-attached-artifacts session)))
         (workspace (niwa-collaboration--room-workspace room))
         (snapshot (or snapshot (niwa-collaboration--store-snapshot
                                 workspace (not session)))))
    (mapcar (lambda (row)
              (let ((id (plist-get row :id)))
                (append
                 (list :id id
                       :title (plist-get row :title)
                       :kind (symbol-name (plist-get row :kind))
                       :artifact (concat id "/" (plist-get row :file))
                       :size (plist-get row :size)
                       :modified (if (plist-get row :missing) nil
                                   (truncate (float-time (plist-get row :modified))))
                       :revision (cond ((plist-get row :remote) (plist-get row :revision))
                                       ((plist-get row :missing) nil)
                                       (t (niwa-collaboration--artifact-revision
                                           (plist-get row :size) (plist-get row :modified))))
                       :versions (plist-get row :versions)
                       :item (if (memq (plist-get row :kind)
                                       niwa-artifact-store-item-kinds)
                                 t :json-false)
                       :missing (if (plist-get row :missing) t :json-false)
                       :attached (if (member id attached) t :json-false)
                       :shared (if (member id (plist-get snapshot :shared))
                                   t :json-false)
                       :conversation (if (plist-get row :dedicated-session)
                                         t :json-false))
                 (unless session
                   (list :attachedSessions
                         (gethash id (plist-get snapshot :attached-counts) 0))))))
            (plist-get snapshot :artifacts))))

(defun niwa-collaboration--store-frame (room &optional snapshot)
  "Return the store listing frame for ROOM's guests, using SNAPSHOT if given."
  (list :t "store-artifacts"
        :artifacts (vconcat (niwa-collaboration--store-rows room snapshot))))

(defun niwa-collaboration--handle-store-list (room peer _frame)
  "Send guest PEER in ROOM the workspace's artifact listing.
A request beyond the guest's read budget is dropped: every store change
sends the listing anyway."
  (when-let* ((guest (niwa-collaboration--guest room peer))
              ((niwa-collaboration--store-read-allowed-p guest)))
    (niwa-collaboration--transport-send
     (plist-get room :transport) peer
     (niwa-collaboration--store-frame room))))

(defun niwa-collaboration-item-link (link id &optional route)
  "Return room LINK opening shared item ID directly in its editor tab.
ROUTE \"artifact\" opens store artifact ID in the viewer's panel instead.
The viewer reads an editor tab's item from the query and a panel's from
the fragment, after the room's key."
  (if (equal route "artifact")
      (concat link "?artifact=" (url-hexify-string id))
    (let ((hash (string-search "#" link)))
      (concat (substring link 0 hash) "?shared=" (url-hexify-string id)
              (substring link hash)))))

(defun niwa-collaboration--store-conversation-link (guest workspace id)
  "Share artifact ID's dedicated session and return GUEST's link to it.
A whiteboard or document opens straight in its editor there."
  (let* ((buffer (niwa-artifact-store-conversation workspace id))
         (shared (niwa-collaboration--start
                  (buffer-local-value 'niwa--session buffer) buffer))
         (link (niwa-collaboration--guest-link shared guest)))
    (if (niwa-artifact-store-item-p (niwa-artifact-store-meta workspace id))
        (niwa-collaboration-item-link link id)
      link)))

(defconst niwa-collaboration--sharing-operations
  '(("share" . share) ("rotate" . rotate) ("revoke" . revoke))
  "Browser names of the sharing operations an owner link may request.")

(defun niwa-collaboration--store-sharing (workspace id operation)
  "Apply the browser's sharing OPERATION to WORKSPACE's artifact ID.
Run the same host operation as the Emacs commands and return the reply
fields: whether ID is now shared, and its view and edit links if so."
  (let* ((operation (or (cdr (assoc operation niwa-collaboration--sharing-operations))
                        (error "Unknown sharing operation")))
         (links (niwa-collaboration-artifact-room-sharing workspace id operation)))
    (append (list :id id :shared (if links t :json-false))
            (when links
              (list :linkView (plist-get links :link-view)
                    :linkEdit (plist-get links :link-edit))))))

(defun niwa-collaboration--store-action (room guest frame)
  "Perform GUEST's store FRAME in ROOM and return the reply fields.
An action that finishes later returns a function instead, to be called
with a function receiving reply fields, including `:error' on failure.
Signal an error for the guest when the action is refused.  Sharing is
for the host and owner links only."
  (let* ((action (plist-get frame :action))
         (workspace (niwa-collaboration--room-workspace room))
         (session (plist-get room :session))
         (id (plist-get frame :id)))
    (unless (or (equal action "create")
                (and (niwa-artifact-store-id-p id)
                     (niwa-artifact-store-meta workspace id)))
      (error "No such artifact"))
    (when (and (equal action "sharing") (not (plist-get guest :owner)))
      (error "Only the host or an owner link can share artifacts"))
    (unless (or (equal action "versions")
                ;; Opening an editor from the lobby needs its dedicated room.
                ;; The returned bearer remains capped to the guest's tier.
                (and (equal action "conversation")
                     (niwa-artifact-store-item-p
                      (niwa-artifact-store-meta workspace id)))
                (plist-get guest :writable))
      (error "This link can view artifacts but not change them"))
    (pcase action
      ("create"
       (let ((kind (plist-get frame :kind))
             (title (plist-get frame :title)))
         ;; Refused input answers the guest now, before any work is queued.
         (niwa-shared-editing-check-new kind title)
         (lambda (done)
           (niwa-shared-editing-create
            workspace kind title (or (plist-get guest :name) "Guest")
            (lambda (reply)
              (funcall done (if (plist-get reply :error)
                                (list :error (plist-get reply :error))
                              (list :id (plist-get reply :id)))))
            :session session
            :buffer (and session (niwa-collaboration--room-data-buffer room))
            :authorize
            (lambda ()
              (and (plist-get guest :writable)
                   (memq guest (hash-table-values (plist-get room :guests)))
                   (memq room (niwa-collaboration--workspace-rooms workspace))))))))
      ("versions"
       (niwa-collaboration--store-read guest)
       (list :id id
             :versions
             (vconcat
              (mapcar (lambda (row)
                        (list :n (plist-get row :n) :time (plist-get row :time)
                              :bytes (plist-get row :bytes)))
                      (reverse (niwa-artifact-store-versions workspace id))))))
      ("attach"
       (unless session (error "Open a session to attach artifacts to it"))
       (niwa-artifact-store-attach
        session id (niwa-collaboration--room-data-buffer room))
       (list :id id))
      ("restore"
       (let ((n (plist-get frame :n))
             (session-id (and session (niwa-session-session-id session))))
         (unless (natnump n) (error "No such version"))
         (lambda (done)
           (niwa-artifact-store-restore-version
            workspace id n session-id (niwa-collaboration--guest-actor guest)
            (lambda (reply) (funcall done (append (list :id id) reply)))))))
      ("save-version"
       (unless (niwa-artifact-store-item-p (niwa-artifact-store-meta workspace id))
         (error "File artifacts keep a version of every saved change"))
       (let ((n (niwa-shared-editing-save-version
                 workspace id (and session (niwa-session-session-id session)))))
         (list :id id :n n)))
      ("duplicate"
       (let ((new-id (plist-get frame :newId)))
         (unless (niwa-artifact-store-id-p new-id)
           (error "Name the copy with letters, digits, dashes or underscores"))
         (niwa-artifact-store-duplicate workspace id new-id)
         (when session
           (niwa-artifact-store-attach
            session new-id (niwa-collaboration--room-data-buffer room)))
         (list :id new-id)))
      ;; Deleting may wait for the item's editing queue, so it answers later too.
      ("delete"
       (lambda (done)
         (niwa-artifact-store-delete
          workspace id (niwa-collaboration--guest-actor guest)
          (lambda (failure)
            (funcall done (if failure (list :error failure) (list :id id)))))))
      ("conversation"
       (condition-case nil
           (list :id id :link (niwa-collaboration--store-conversation-link
                               guest workspace id))
         (inhibited-interaction (error "%s" niwa-collaboration-needs-host-message))))
      ("sharing"
       (niwa-collaboration--store-sharing workspace id (plist-get frame :operation)))
      (_ (error "Unknown artifact action")))))

(defun niwa-collaboration--handle-store-action (room peer frame)
  "Perform guest PEER's store action FRAME in ROOM and answer the sender."
  (let ((guest (niwa-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (niwa-collaboration--request-id-p req-id))
      (let ((reply (lambda (fields)
                     (niwa-collaboration--transport-send
                      (plist-get room :transport) peer
                      (append (list :t "store-action" :reqId req-id
                                    :action (plist-get frame :action))
                              fields))))
            (refused (lambda (message) (list :ok :json-false :error message))))
        (condition-case err
            (let ((result (niwa-collaboration--store-action room guest frame)))
              (if (functionp result)
                  (funcall result
                           (lambda (fields)
                             (funcall reply
                                      (append (list :ok (if (plist-get fields :error)
                                                           :json-false t))
                                              fields))))
                (funcall reply (append (list :ok t) result))))
          (error (funcall reply (funcall refused (error-message-string err)))))))))

(add-hook 'niwa-artifact-store-changed-functions
          #'niwa-collaboration-notify-artifacts-changed)

(provide 'niwa-collaboration-artifact)
;;; niwa-collaboration-artifact.el ends here
