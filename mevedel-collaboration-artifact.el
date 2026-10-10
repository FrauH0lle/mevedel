;;; mevedel-collaboration-artifact.el --- browser artifact transfer -*- lexical-binding: t; -*-

;;; Commentary:

;; Resolves published artifact record ids and sends their bytes to browser
;; guests on demand.  Filesystem paths never cross the wire, and the path is
;; re-authorized against the canonical artifact root before each read.

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
                  "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-directory
                  "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-duplicate
                  "mevedel-artifact-store" (workspace id new-id))
(declare-function mevedel-artifact-store-id-p "mevedel-artifact-store" (id))
(declare-function mevedel-artifact-store-item-p "mevedel-artifact-store" (meta))
(defvar mevedel-artifact-store-item-kinds)
(declare-function mevedel-artifact-store-list "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-meta "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-restore-version
                  "mevedel-artifact-store" (workspace id n &optional session-id))
(declare-function mevedel-artifact-store-versions
                  "mevedel-artifact-store" (workspace id))
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
(autoload 'mevedel-artifact-store-restore-version "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-versions "mevedel-artifact-store")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function mevedel-shared-editing-save-version
                  "mevedel-shared-editing" (workspace id &optional session-id))
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-save-version "mevedel-shared-editing")

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room &optional err))
(declare-function mevedel-collaboration--broadcast
                  "mevedel-collaboration" (room frame))
(declare-function mevedel-collaboration--guest-link
                  "mevedel-collaboration" (room guest))
(declare-function mevedel-collaboration--publish
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-data-buffer
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-list "mevedel-collaboration" ())
(declare-function mevedel-collaboration--start
                  "mevedel-collaboration" (session data-buffer))
(defvar mevedel-collaboration-needs-host-message)

;; `mevedel-collaboration-artifact-projection'
(declare-function mevedel-collaboration--artifacts-dir
                  "mevedel-collaboration-artifact-projection" (session))
(declare-function mevedel-collaboration--artifact-stat-invalidate
                  "mevedel-collaboration-artifact-projection" ())

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))
(declare-function mevedel-collaboration--room-workspace
                  "mevedel-collaboration-guest" (room))

;; `mevedel-collaboration-lobby'
(declare-function mevedel-collaboration-lobby--find
                  "mevedel-collaboration-lobby" (workspace))

;; `mevedel-collaboration-history'
(declare-function mevedel-collaboration--history-artifacts
                  "mevedel-collaboration-history" (room))
(autoload 'mevedel-collaboration--history-artifacts
  "mevedel-collaboration-history")

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(defvar mevedel-collaboration--max-frame-json-bytes)

;; `mevedel-resource'
(declare-function mevedel-resource-within-root-p
                  "mevedel-resource" (path root))
(autoload 'mevedel-resource-within-root-p "mevedel-resource")

;; `mevedel-structs'
(declare-function mevedel-session-attached-artifacts "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)


(defconst mevedel-collaboration--artifact-fetch-window 1.0
  "Seconds within which repeated artifact fetches from one guest drop.")

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
    (let ((path (file-name-concat
                 (mevedel-artifact-store-artifact-directory workspace id) file)))
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

(defun mevedel-collaboration--handle-artifact-get (room peer frame)
  "Answer guest PEER's artifact fetch FRAME for ROOM."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId))
        (transport (plist-get room :transport))
        (now (float-time)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      ;; ponytail: one budget per guest; split by record only if the viewer
      ;; gains concurrent artifact prefetch.
      (let ((last (plist-get guest :last-artifact-fetch)))
        (unless (and last (< (- now last)
                             mevedel-collaboration--artifact-fetch-window))
          (plist-put guest :last-artifact-fetch now)
          (let* ((record (mevedel-collaboration--artifact-target
                          room guest (plist-get frame :id)))
                 (path (plist-get record :artifact-path))
                 (dir (when-let* ((workspace (mevedel-collaboration--room-workspace room)))
                        (mevedel-artifact-store-directory workspace)))
                 ;; A whiteboard's or document's state opens in its editor.
                 (contained (and dir path (not (plist-get record :item))
                                 (mevedel-resource-within-root-p path dir)))
                 (read
                  (and contained
                       (condition-case nil
                           (list
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally
                               path nil 0
                               (1+ mevedel-collaboration--max-artifact-bytes))
                              (buffer-string)))
                         (file-error nil)))))
            (cond
             ((not contained)
              (mevedel-collaboration--artifact-refuse
               room peer req-id "This artifact is not published"))
             ((null read)
              (mevedel-collaboration--artifact-refuse
               room peer req-id "This artifact was deleted on the host"))
             (t
              (let* ((content (car read))
                     (size (length content)))
                (if (> size mevedel-collaboration--max-artifact-bytes)
                    (mevedel-collaboration--artifact-refuse
                     room peer req-id
                     (format
                      "Artifact too large to send (%d MB); open it on the host"
                      (/ size 1024 1024)))
                  (mevedel-collaboration--send-chunked
                   transport peer
                   (list :t "artifact" :reqId req-id
                         :id (plist-get record :id)
                         :name (plist-get record :artifact)
                         :mime (mevedel-collaboration--artifact-mime
                                (plist-get record :artifact))
                         :size size)
                   content)))))))))))

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

(defun mevedel-collaboration-delete-artifact (workspace name)
  "Delete WORKSPACE's store artifact NAME and update the workspace's rooms.
NAME is the path relative to the store, as cards show it; the whole
artifact directory it lies in goes, with metadata, versions, comments and
its dedicated session.  A whiteboard or document is deleted through its
editing queue, after any save in progress, so its editors learn of it."
  (let* ((slash (string-search "/" name))
         (id (and slash (substring name 0 slash))))
    (cond
     ((null id)
      (delete-file (expand-file-name
                    name (mevedel-artifact-store-directory workspace)))
      (mevedel-collaboration-notify-artifacts-changed workspace))
     ((mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
      (mevedel-shared-editing-call
       workspace (list :action "delete" :id id :actor "Host")
       (lambda (reply)
         (if (plist-get reply :error)
             (message "mevedel: %s was not deleted: %s" id (plist-get reply :error))
           (mevedel-collaboration-notify-artifacts-changed workspace)))))
     (t
      (mevedel-artifact-store-delete workspace id)
      (mevedel-collaboration-notify-artifacts-changed workspace)))))

(defun mevedel-collaboration--handle-artifact-delete (room peer frame)
  "Delete the published artifact FRAME names for writable guest PEER in ROOM.
The file comes from the host's own record, never from the frame.  Every
refusal is answered to the sender."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (append
        (list :t "artifact-delete" :reqId req-id)
        (condition-case err
            (let ((record (mevedel-collaboration--artifact-target
                           room guest (plist-get frame :id))))
              (unless (plist-get guest :writable)
                (error "This link can view artifacts but not delete them"))
              (unless (and record (or (plist-get record :store)
                                      (not (plist-get record :missing))))
                (error "This artifact is no longer on the host"))
              (mevedel-collaboration-delete-artifact
               (mevedel-collaboration--room-workspace room)
               (plist-get record :artifact))
              (list :ok t :artifact (plist-get record :artifact)))
          (error (list :error (error-message-string err)))))))))

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
  "Re-publish every room of WORKSPACE after its artifact store changed.
Rooms and the lobby also receive the store's new listing."
  ;; Only a loaded collaboration can have a room to tell.
  (when (featurep 'mevedel-collaboration)
    (mevedel-collaboration--artifact-stat-invalidate)
    (dolist (room (mevedel-collaboration--workspace-rooms workspace))
      (condition-case err
          (progn
            (when (plist-get room :session)
              (mevedel-collaboration--publish room))
            (mevedel-collaboration--broadcast
             room (mevedel-collaboration--store-frame room)))
        (error (mevedel-collaboration--observer-failure room err))))))


;;
;;; Store listing and actions

(defun mevedel-collaboration--store-rows (room)
  "Return ROOM's workspace artifacts as guests receive them.
Attachment is relative to ROOM's session; a lobby has none."
  (let* ((session (plist-get room :session))
         (attached (and session (mevedel-session-attached-artifacts session)))
         (workspace (mevedel-collaboration--room-workspace room)))
    (mapcar (lambda (row)
              (let ((id (plist-get row :id)))
                (list :id id
                      :title (plist-get row :title)
                      :kind (symbol-name (plist-get row :kind))
                      :artifact (concat id "/" (file-name-nondirectory
                                                (plist-get row :path)))
                      :size (plist-get row :size)
                      :modified (if (plist-get row :missing) nil
                                  (truncate (float-time (plist-get row :modified))))
                      :versions (plist-get row :versions)
                      :item (if (memq (plist-get row :kind)
                                      mevedel-artifact-store-item-kinds)
                                t :json-false)
                      :missing (if (plist-get row :missing) t :json-false)
                      :attached (if (member id attached) t :json-false)
                      :conversation
                      (if (plist-get (mevedel-artifact-store-meta workspace id)
                                     :dedicated-session)
                          t :json-false))))
            (mevedel-artifact-store-list workspace))))

(defun mevedel-collaboration--store-frame (room)
  "Return the store listing frame for ROOM's guests."
  (list :t "store-artifacts"
        :artifacts (vconcat (mevedel-collaboration--store-rows room))))

(defun mevedel-collaboration--handle-store-list (room peer _frame)
  "Send guest PEER in ROOM the workspace's artifact listing."
  (when (mevedel-collaboration--guest room peer)
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
Signal an error with a message for the guest when the action is refused."
  (let* ((action (plist-get frame :action))
         (workspace (mevedel-collaboration--room-workspace room))
         (session (plist-get room :session))
         (id (plist-get frame :id)))
    (unless (and (mevedel-artifact-store-id-p id)
                 (mevedel-artifact-store-meta workspace id))
      (error "No such artifact"))
    (unless (or (equal action "versions") (plist-get guest :writable))
      (error "This link can view artifacts but not change them"))
    (pcase action
      ("versions"
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
       (mevedel-collaboration-notify-artifacts-changed workspace)
       (list :id id))
      ("restore"
       (let ((n (plist-get frame :n)))
         (unless (natnump n) (error "No such version"))
         (let ((new (mevedel-artifact-store-restore-version
                     workspace id n
                     (and session (mevedel-session-session-id session)))))
           ;; A whiteboard or document restores as a queued edit.
           (when new (mevedel-collaboration-notify-artifacts-changed workspace))
           (list :id id :n (or new :json-false)))))
      ("save-version"
       (unless (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
         (error "File artifacts keep a version of every saved change"))
       (let ((n (mevedel-shared-editing-save-version
                 workspace id (and session (mevedel-session-session-id session)))))
         (mevedel-collaboration-notify-artifacts-changed workspace)
         (list :id id :n n)))
      ("duplicate"
       (let ((new-id (plist-get frame :newId)))
         (unless (and (stringp new-id)
                      (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9._-]\\{0,79\\}\\'" new-id))
           (error "Name the copy with letters, digits, dots, dashes or underscores"))
         (mevedel-artifact-store-duplicate workspace id new-id)
         (when session
           (mevedel-artifact-store-attach
            session new-id (mevedel-collaboration--room-data-buffer room)))
         (mevedel-collaboration-notify-artifacts-changed workspace)
         (list :id new-id)))
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
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (append (list :t "store-action" :reqId req-id
                     :action (plist-get frame :action))
               (condition-case err
                   (append (list :ok t)
                           (mevedel-collaboration--store-action room guest frame))
                 (error (list :ok :json-false
                              :error (error-message-string err)))))))))

(provide 'mevedel-collaboration-artifact)
;;; mevedel-collaboration-artifact.el ends here
