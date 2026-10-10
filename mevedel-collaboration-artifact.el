;;; mevedel-collaboration-artifact.el --- the artifact store in the browser -*- lexical-binding: t; -*-

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

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-artifact-directory
                  "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-attach
                  "mevedel-artifact-store" (session id &optional buffer))
(declare-function mevedel-artifact-store-conversation
                  "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-delete
                  "mevedel-artifact-store" (workspace id &optional actor callback))
(declare-function mevedel-artifact-store-directory
                  "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-duplicate
                  "mevedel-artifact-store" (workspace id new-id))
(declare-function mevedel-artifact-store-id-p "mevedel-artifact-store" (id))
(declare-function mevedel-artifact-store-item-p "mevedel-artifact-store" (meta))
(declare-function mevedel-artifact-store-list "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-meta "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-primary-path
                  "mevedel-artifact-store" (workspace id &optional meta))
(declare-function mevedel-artifact-store-restore-version
                  "mevedel-artifact-store" (workspace id n &optional session-id actor callback))
(declare-function mevedel-artifact-store-version-path
                  "mevedel-artifact-store" (workspace id n))
(declare-function mevedel-artifact-store-versions
                  "mevedel-artifact-store" (workspace id))
(defvar mevedel-artifact-store-changed-functions)
(defvar mevedel-artifact-store-item-kinds)
(autoload 'mevedel-artifact-store-artifact-directory "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-attach "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-conversation "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-delete "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-directory "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-duplicate "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-id-p "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-item-p "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-list "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-meta "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-primary-path "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-restore-version "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-version-path "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-versions "mevedel-artifact-store")

;; `mevedel-chat'
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(autoload 'mevedel--workspace-sessions "mevedel-chat")

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast
                  "mevedel-collaboration" (room frame))
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--guest-link
                  "mevedel-collaboration" (room guest))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room &optional err))
(declare-function mevedel-collaboration--publish
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-data-buffer
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-list "mevedel-collaboration" ())
(declare-function mevedel-collaboration--start
                  "mevedel-collaboration" (session data-buffer))
(defvar mevedel-collaboration-needs-host-message)

;; `mevedel-collaboration-artifact-projection'
(declare-function mevedel-collaboration--artifact-stat-invalidate
                  "mevedel-collaboration-artifact-projection" ())
(declare-function mevedel-collaboration--artifacts-dir
                  "mevedel-collaboration-artifact-projection" (session))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))
(declare-function mevedel-collaboration--room-workspace
                  "mevedel-collaboration-guest" (room))

;; `mevedel-collaboration-history'
(declare-function mevedel-collaboration--history-artifacts
                  "mevedel-collaboration-history" (room))
(autoload 'mevedel-collaboration--history-artifacts
  "mevedel-collaboration-history")

;; `mevedel-collaboration-lobby'
(declare-function mevedel-collaboration-lobby--find
                  "mevedel-collaboration-lobby" (workspace))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(defvar mevedel-collaboration--max-frame-json-bytes)

;; `mevedel-resource'
(declare-function mevedel-resource-within-root-p
                  "mevedel-resource" (path root))
(autoload 'mevedel-resource-within-root-p "mevedel-resource")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-list-sessions
                  "mevedel-session-persistence" (workspace &optional cached))
(autoload 'mevedel-session-persistence-list-sessions "mevedel-session-persistence")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function mevedel-shared-editing-save-version
                  "mevedel-shared-editing" (workspace id &optional session-id))
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-save-version "mevedel-shared-editing")

;; `mevedel-structs'
(declare-function mevedel-session-attached-artifacts "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-transport'
(declare-function mevedel-transport-schedule-idle
                  "mevedel-transport" (table key tag path thunk))
(autoload 'mevedel-transport-schedule-idle "mevedel-transport")

(defvar mevedel-collaboration--artifact-notifications (make-hash-table :test #'eq)
  "Pending store notification timers keyed by workspace.")

(defconst mevedel-collaboration--artifact-fetch-window 1.0
  "Seconds within which repeated artifact fetches from one guest drop.")

(defconst mevedel-collaboration--store-read-budget '(8 . 2.0)
  "At most CAR store reads per guest within CDR seconds.
Listing the store, or an artifact's versions or comments, reads files
for each one; a link that may only view must not keep the host busy.")

(defun mevedel-collaboration--store-read-allowed-p (guest)
  "Count one store read for GUEST; return nil once its budget is spent."
  (let ((now (float-time))
        (window (plist-get guest :store-reads)))
    (unless (and window (< (- now (car window))
                           (cdr mevedel-collaboration--store-read-budget)))
      (setq window (cons now 0))
      (plist-put guest :store-reads window))
    (<= (setcdr window (1+ (cdr window)))
        (car mevedel-collaboration--store-read-budget))))

(defun mevedel-collaboration--store-read (guest)
  "Count one store read for GUEST, or signal when its budget is spent."
  (unless (mevedel-collaboration--store-read-allowed-p guest)
    (error "Too many requests; try again in a moment")))

(defconst mevedel-collaboration--max-artifact-bytes (* 16 1024 1024)
  "Largest artifact file the host will send to a guest.")

(defconst mevedel-collaboration--artifact-mime-types
  '(("html" . "text/html") ("htm" . "text/html")
    ("md" . "text/markdown") ("markdown" . "text/markdown")
    ("txt" . "text/plain") ("csv" . "text/csv")
    ("json" . "application/json")
    ("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("webp" . "image/webp") ("gif" . "image/gif")
    ("svg" . "image/svg+xml") ("pdf" . "application/pdf"))
  "Artifact file extensions and viewer transfer MIME types.")

(defun mevedel-collaboration--artifact-mime (name)
  "Return the transfer MIME type for artifact file NAME."
  (or (cdr (assoc (downcase (or (file-name-extension name) ""))
                  mevedel-collaboration--artifact-mime-types))
      "application/octet-stream"))

(defun mevedel-collaboration--artifact-record (room guest id)
  "Return artifact record ID currently published to GUEST in ROOM."
  (when (stringp id)
    (cl-find-if
     (lambda (record)
       (and (equal id (plist-get record :id))
            (plist-get record :artifact)))
     (append (plist-get room :records)
             (mevedel-collaboration--history-artifacts room)
             (plist-get guest :agent-artifacts)))))

(defun mevedel-collaboration--store-target (workspace id)
  "Return store artifact ID of WORKSPACE as an artifact record, or nil.
The record names the artifact's primary file; it is marked missing when
that file is gone."
  (when-let* (((mevedel-artifact-store-id-p id))
              (meta (mevedel-artifact-store-meta workspace id))
              (file (plist-get meta :file)))
    (let ((path (mevedel-artifact-store-primary-path workspace id meta)))
      (append (list :id (concat "artifact:" id) :store id
                    :artifact (concat id "/" file) :artifact-path path)
              (when (mevedel-artifact-store-item-p meta) (list :item t))
              (unless (file-exists-p path) (list :missing t))))))

(defun mevedel-collaboration--artifact-target (room guest id)
  "Return the artifact a frame's ID names for GUEST in ROOM, or nil.
\"artifact:STORE-ID\" names a store artifact, which any link to the
workspace may reach; any other id names a card published to GUEST.  A
card in a store artifact's directory carries that artifact as `:store'."
  (if (and (stringp id) (string-prefix-p "artifact:" id))
      (mevedel-collaboration--store-target
       (mevedel-collaboration--room-workspace room) (substring id 9))
    (when-let* ((record (mevedel-collaboration--artifact-record room guest id)))
      (let ((slash (string-search "/" (plist-get record :artifact))))
        (if slash
            (append (list :store (substring (plist-get record :artifact) 0 slash))
                    record)
          record)))))

(defun mevedel-collaboration--artifact-refuse (room peer req-id message)
  "Send guest PEER a bounded artifact refusal for REQ-ID in ROOM."
  (mevedel-collaboration--transport-send
   (plist-get room :transport) peer
   (list :t "artifact" :reqId req-id :error message)))

(defun mevedel-collaboration--artifact-send-content (room peer req-id record mime content)
  "Send bounded CONTENT with MIME for RECORD to PEER's REQ-ID in ROOM."
  (let ((size (string-bytes content)))
    (if (> size mevedel-collaboration--max-artifact-bytes)
        (mevedel-collaboration--artifact-refuse
         room peer req-id
         (format "Artifact too large to send (%d MB); open it on the host"
                 (/ size 1024 1024)))
      (mevedel-collaboration--send-chunked
       (plist-get room :transport) peer
       (list :t "artifact" :reqId req-id :id (plist-get record :id)
             :name (plist-get record :artifact) :mime mime :size size)
       content))))

(defun mevedel-collaboration--handle-artifact-get (room peer frame)
  "Answer guest PEER's artifact fetch FRAME for ROOM.
An optional version number previews a historical file or editor export
without changing the live artifact."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId))
        (now (float-time)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (let ((last (plist-get guest :last-artifact-fetch)))
        (if (and last (< (- now last)
                         mevedel-collaboration--artifact-fetch-window))
            (mevedel-collaboration--artifact-refuse
             room peer req-id "Please wait a moment, then open the artifact again")
          (plist-put guest :last-artifact-fetch now)
          (condition-case err
              (let* ((record (mevedel-collaboration--artifact-target
                              room guest (plist-get frame :id)))
                     (workspace (mevedel-collaboration--room-workspace room))
                     (version (plist-get frame :version))
                     (id (plist-get record :store))
                     (path (if (plist-member frame :version)
                               (progn
                                 (unless (and id (natnump version))
                                   (error "No such version"))
                                 (mevedel-artifact-store-version-path workspace id version))
                             (plist-get record :artifact-path)))
                     (dir (and workspace (mevedel-artifact-store-directory workspace))))
                (unless (and dir path (mevedel-resource-within-root-p path dir)
                             (or version (not (plist-get record :item))))
                  (error "This artifact is not published"))
                (if (and version (plist-get record :item))
                    (mevedel-shared-editing-call
                     workspace
                     (list :action "export" :id id :version version
                           :format (if (eq (plist-get (mevedel-artifact-store-meta
                                                       workspace id) :kind) 'whiteboard)
                                       "svg" "html"))
                     (lambda (reply)
                       (when (eq guest (mevedel-collaboration--guest room peer))
                         (if-let* ((failure (plist-get reply :error)))
                             (mevedel-collaboration--artifact-refuse room peer req-id failure)
                           (let ((result (plist-get reply :result)))
                             (mevedel-collaboration--artifact-send-content
                              room peer req-id record (plist-get result :mime)
                              (encode-coding-string (plist-get result :text) 'utf-8-unix)))))))
                  (let ((content (condition-case nil
                                     (with-temp-buffer
                                       (set-buffer-multibyte nil)
                                       (insert-file-contents-literally
                                        path nil 0 (1+ mevedel-collaboration--max-artifact-bytes))
                                       (buffer-string))
                                   (file-error (error "This artifact was deleted on the host")))))
                    (mevedel-collaboration--artifact-send-content
                     room peer req-id record
                     (mevedel-collaboration--artifact-mime (plist-get record :artifact))
                     content))))
            (error (mevedel-collaboration--artifact-refuse
                    room peer req-id (error-message-string err)))))))))

(defun mevedel-collaboration--send-chunked (transport peer meta content)
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
         (chunk (max 1 (- mevedel-collaboration--max-frame-json-bytes
                          overhead)))
         (total (length data))
         (start 0)
         (sent t)
         done)
    (while (and sent (not done))
      (let ((end (min total (+ start chunk))))
        (setq done (= end total)
              sent (mevedel-collaboration--transport-send
                    transport peer
                    (append meta
                            (list :data (substring data start end)
                                  :final (if done t :json-false))))
              start end)))
    sent))

(defun mevedel-collaboration--guest-actor (guest)
  "Return how edits by GUEST are attributed."
  (concat "Guest: " (plist-get guest :name)))

(defun mevedel-collaboration--handle-artifact-delete (room peer frame)
  "Delete the published artifact FRAME names for writable guest PEER in ROOM.
The file comes from the host's own record, never from the frame.  A card
for a file beside an artifact's main one is refused, since deleting goes
by artifact.  The sender is answered once it is gone or was refused."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (let ((reply (lambda (fields)
                     (mevedel-collaboration--transport-send
                      (plist-get room :transport) peer
                      (append (list :t "artifact-delete" :reqId req-id) fields)))))
        (condition-case err
            (let* ((record (mevedel-collaboration--artifact-target
                            room guest (plist-get frame :id)))
                   (workspace (mevedel-collaboration--room-workspace room))
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
                (delete-file (expand-file-name name (mevedel-artifact-store-directory workspace)))
                (mevedel-collaboration-notify-artifacts-changed workspace)
                (funcall done nil))
               ((not (equal name (concat id "/" (plist-get (mevedel-artifact-store-meta workspace id)
                                                           :file))))
                (error "%s belongs to artifact %s; delete the artifact from its main file"
                       (file-name-nondirectory name) id))
               (t (mevedel-artifact-store-delete
                   workspace id (mevedel-collaboration--guest-actor guest) done))))
          (error (funcall reply (list :error (error-message-string err)))))))))

(defun mevedel-collaboration--workspace-rooms (workspace)
  "Return WORKSPACE's live session rooms followed by its lobby, if any."
  (append
   (cl-remove-if-not
    (lambda (room)
      (when-let* ((session (plist-get room :session)))
        (eq workspace (mevedel-session-workspace session))))
    (mevedel-collaboration--room-list))
   (when-let* (((featurep 'mevedel-collaboration-lobby))
               (lobby (mevedel-collaboration-lobby--find workspace)))
     (list lobby))))

(defun mevedel-collaboration-notify-artifacts-changed (workspace)
  "Invalidate artifact stats and schedule WORKSPACE's latest store listing.
Coalesce changes until the next transport-idle opportunity, after the current
mutation can acknowledge.  Resolve rooms and read the store when publishing."
  ;; Only a loaded collaboration can have a room to tell.
  (when (featurep 'mevedel-collaboration)
    (mevedel-collaboration--artifact-stat-invalidate)
    (when (mevedel-collaboration--workspace-rooms workspace)
      (mevedel-transport-schedule-idle
       mevedel-collaboration--artifact-notifications workspace 'artifact-notifications
       (mevedel-workspace-root workspace)
       (lambda ()
         (when-let* ((rooms (mevedel-collaboration--workspace-rooms workspace)))
           (condition-case err
               (let (snapshot)
                 (dolist (room rooms)
                   (condition-case room-error
                       (progn
                         (when (plist-get room :session)
                           (mevedel-collaboration--publish room))
                         ;; The listing is read once, and only for a room a
                         ;; guest is in.
                         (when (> (hash-table-count (plist-get room :guests)) 0)
                           (mevedel-collaboration--broadcast
                            room (mevedel-collaboration--store-frame
                                  room
                                  (or snapshot
                                      (setq snapshot
                                            (mevedel-collaboration--store-snapshot
                                             workspace
                                             (cl-some (lambda (room) (not (plist-get room :session)))
                                                      rooms)
                                             t)))))))
                     (error (mevedel-collaboration--observer-failure room room-error)))))
             (error
              (dolist (room rooms)
                (mevedel-collaboration--observer-failure room err))))))))))


;;
;;; Store listing and actions

(defun mevedel-collaboration--store-snapshot (workspace &optional count-attachments cached)
  "Read WORKSPACE's store once for a listing or notification fanout.
COUNT-ATTACHMENTS includes lobby attachment counts.  CACHED uses the last
session discovery for this decoration; live buffers always override it."
  (let ((artifacts (mevedel-artifact-store-list workspace))
        (counts (make-hash-table :test #'equal)))
    (when (and artifacts count-attachments)
      (let ((sessions (make-hash-table :test #'equal)))
        (dolist (entry (mevedel-session-persistence-list-sessions workspace cached))
          (let ((summary (plist-get entry :summary)))
            (puthash (plist-get summary :session-id)
                     (plist-get summary :attached-artifacts) sessions)))
        ;; A live session may have attachments not saved yet.  Replace its
        ;; durable entry instead of counting it twice.
        (dolist (entry (mevedel--workspace-sessions workspace))
          (let ((session (buffer-local-value 'mevedel--session (cdr entry))))
            (puthash (mevedel-session-session-id session)
                     (mevedel-session-attached-artifacts session) sessions)))
        (maphash (lambda (_session ids)
                   (dolist (id (delete-dups (copy-sequence ids)))
                     (puthash id (1+ (gethash id counts 0)) counts)))
                 sessions)))
    (list :artifacts artifacts :attached-counts counts)))

(defun mevedel-collaboration--store-rows (room &optional snapshot)
  "Return ROOM's workspace artifacts as guests receive them.
Attachment is relative to ROOM's session; a lobby has none.
SNAPSHOT, when given, supplies the store observation shared across rooms."
  (let* ((session (plist-get room :session))
         (attached (and session (mevedel-session-attached-artifacts session)))
         (workspace (mevedel-collaboration--room-workspace room))
         (snapshot (or snapshot (mevedel-collaboration--store-snapshot
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
                       :versions (plist-get row :versions)
                       :item (if (memq (plist-get row :kind)
                                       mevedel-artifact-store-item-kinds)
                                 t :json-false)
                       :missing (if (plist-get row :missing) t :json-false)
                       :attached (if (member id attached) t :json-false)
                       :conversation (if (plist-get row :dedicated-session)
                                         t :json-false))
                 (unless session
                   (list :attachedSessions
                         (gethash id (plist-get snapshot :attached-counts) 0))))))
            (plist-get snapshot :artifacts))))

(defun mevedel-collaboration--store-frame (room &optional snapshot)
  "Return the store listing frame for ROOM's guests, using SNAPSHOT if given."
  (list :t "store-artifacts"
        :artifacts (vconcat (mevedel-collaboration--store-rows room snapshot))))

(defun mevedel-collaboration--handle-store-list (room peer _frame)
  "Send guest PEER in ROOM the workspace's artifact listing.
A request beyond the guest's read budget is dropped: every store change
sends the listing anyway."
  (when-let* ((guest (mevedel-collaboration--guest room peer))
              ((mevedel-collaboration--store-read-allowed-p guest)))
    (mevedel-collaboration--transport-send
     (plist-get room :transport) peer
     (mevedel-collaboration--store-frame room))))

(defun mevedel-collaboration-item-link (link id)
  "Return room LINK opening shared item ID directly in its editor tab."
  (let ((hash (string-search "#" link)))
    (concat (substring link 0 hash) "?shared=" (url-hexify-string id)
            (substring link hash))))

(defun mevedel-collaboration--store-conversation-link (guest workspace id)
  "Share artifact ID's dedicated session and return GUEST's link to it.
A whiteboard or document opens straight in its editor there."
  (let* ((buffer (mevedel-artifact-store-conversation workspace id))
         (shared (mevedel-collaboration--start
                  (buffer-local-value 'mevedel--session buffer) buffer))
         (link (mevedel-collaboration--guest-link shared guest)))
    (if (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
        (mevedel-collaboration-item-link link id)
      link)))

(defun mevedel-collaboration--store-action (room guest frame)
  "Perform GUEST's store FRAME in ROOM and return the reply fields.
An action that finishes later returns a function instead, to be called
with a function receiving reply fields, including `:error' on failure.
Signal an error for the guest when the action is refused."
  (let* ((action (plist-get frame :action))
         (workspace (mevedel-collaboration--room-workspace room))
         (session (plist-get room :session))
         (id (plist-get frame :id)))
    (unless (or (equal action "create")
                (and (mevedel-artifact-store-id-p id)
                     (mevedel-artifact-store-meta workspace id)))
      (error "No such artifact"))
    (unless (or (equal action "versions")
                ;; Opening an editor from the lobby needs its dedicated room.
                ;; The returned bearer remains capped to the guest's tier.
                (and (equal action "conversation")
                     (mevedel-artifact-store-item-p
                      (mevedel-artifact-store-meta workspace id)))
                (plist-get guest :writable))
      (error "This link can view artifacts but not change them"))
    (pcase action
      ("create"
       (let ((kind (plist-get frame :kind))
             (title (plist-get frame :title))
             (id (secure-hash 'sha256 (format "%s%s" (current-time) (random t)))))
         (unless (member kind '("whiteboard" "document"))
           (error "Choose a whiteboard or document"))
         (unless (and (stringp title) (> (length title) 0) (<= (length title) 200))
           (error "An artifact title must contain 1 to 200 characters"))
         (lambda (done)
           (mevedel-shared-editing-call
            workspace (list :action "create" :id id :opId id :kind kind
                            :title title :actor (or (plist-get guest :name) "Guest"))
            (lambda (reply)
              (if (plist-get reply :error)
                  (funcall done (list :error (plist-get reply :error)))
                (when session
                  (mevedel-artifact-store-attach
                   session id (mevedel-collaboration--room-data-buffer room)))
                (funcall done (list :id id))))
            (lambda ()
              (and (plist-get guest :writable)
                   (memq guest (hash-table-values (plist-get room :guests)))
                   (memq room (mevedel-collaboration--workspace-rooms workspace))))))))
      ("versions"
       (mevedel-collaboration--store-read guest)
       (list :id id
             :versions
             (vconcat
              (mapcar (lambda (row)
                        (list :n (plist-get row :n) :time (plist-get row :time)
                              :bytes (plist-get row :bytes)))
                      (reverse (mevedel-artifact-store-versions workspace id))))))
      ("attach"
       (unless session (error "Open a session to attach artifacts to it"))
       (mevedel-artifact-store-attach
        session id (mevedel-collaboration--room-data-buffer room))
       (list :id id))
      ("restore"
       (let ((n (plist-get frame :n))
             (session-id (and session (mevedel-session-session-id session))))
         (unless (natnump n) (error "No such version"))
         (lambda (done)
           (mevedel-artifact-store-restore-version
            workspace id n session-id (mevedel-collaboration--guest-actor guest)
            (lambda (reply) (funcall done (append (list :id id) reply)))))))
      ("save-version"
       (unless (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
         (error "File artifacts keep a version of every saved change"))
       (let ((n (mevedel-shared-editing-save-version
                 workspace id (and session (mevedel-session-session-id session)))))
         (list :id id :n n)))
      ("duplicate"
       (let ((new-id (plist-get frame :newId)))
         (unless (mevedel-artifact-store-id-p new-id)
           (error "Name the copy with letters, digits, dashes or underscores"))
         (mevedel-artifact-store-duplicate workspace id new-id)
         (when session
           (mevedel-artifact-store-attach
            session new-id (mevedel-collaboration--room-data-buffer room)))
         (list :id new-id)))
      ;; Deleting may wait for the item's editing queue, so it answers later too.
      ("delete"
       (lambda (done)
         (mevedel-artifact-store-delete
          workspace id (mevedel-collaboration--guest-actor guest)
          (lambda (failure)
            (funcall done (if failure (list :error failure) (list :id id)))))))
      ("conversation"
       (condition-case nil
           (list :id id :link (mevedel-collaboration--store-conversation-link
                               guest workspace id))
         (inhibited-interaction (error "%s" mevedel-collaboration-needs-host-message))))
      (_ (error "Unknown artifact action")))))

(defun mevedel-collaboration--handle-store-action (room peer frame)
  "Perform guest PEER's store action FRAME in ROOM and answer the sender."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (let ((reply (lambda (fields)
                     (mevedel-collaboration--transport-send
                      (plist-get room :transport) peer
                      (append (list :t "store-action" :reqId req-id
                                    :action (plist-get frame :action))
                              fields))))
            (refused (lambda (message) (list :ok :json-false :error message))))
        (condition-case err
            (let ((result (mevedel-collaboration--store-action room guest frame)))
              (if (functionp result)
                  (funcall result
                           (lambda (fields)
                             (funcall reply
                                      (append (list :ok (if (plist-get fields :error)
                                                           :json-false t))
                                              fields))))
                (funcall reply (append (list :ok t) result))))
          (error (funcall reply (funcall refused (error-message-string err)))))))))

(add-hook 'mevedel-artifact-store-changed-functions
          #'mevedel-collaboration-notify-artifacts-changed)

(provide 'mevedel-collaboration-artifact)
;;; mevedel-collaboration-artifact.el ends here
