;;; mevedel-collaboration-lobby.el --- Collaboration lobby -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the per-workspace lobby: one bookmarkable browser room that lists
;; the workspace's live and saved sessions, opens one into its own shared
;; room, creates new ones, and deletes saved ones.
;;
;; A lobby is a room bound to a workspace rather than to a session.  Its
;; credentials persist in the workspace state directory, so its links
;; survive Emacs restarts; every session room it hands out keeps the
;; ordinary share lifetime.  Link tiers keep their meaning: a view link
;; lists sessions, a full link also opens them and works with the project
;; files (`mevedel-collaboration-files'), and an owner link also creates
;; and deletes sessions.  A guest is always handed a session room at its
;; own tier.
;;
;; Guest frames arrive while nobody may be at the keyboard, so lobby work
;; runs with `inhibit-interaction': a step that would ask in Emacs is
;; refused to the guest instead of waiting for an answer.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-collaboration)
(require 'mevedel-collaboration-files)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-owner)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)

;; `mevedel-chat'
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(autoload 'mevedel--workspace-sessions "mevedel-chat")

;; `mevedel-collaboration-share'
(declare-function mevedel-collaboration-share-dismiss
                  "mevedel-collaboration-share" (room))
(declare-function mevedel-collaboration-share-present
                  "mevedel-collaboration-share" (room))
(autoload 'mevedel-collaboration-share-dismiss "mevedel-collaboration-share")
(autoload 'mevedel-collaboration-share-present "mevedel-collaboration-share")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-delete
                  "mevedel-session-persistence" (workspace save-path))
(declare-function mevedel-session-persistence-list-sessions
                  "mevedel-session-persistence" (workspace &optional cached))
(declare-function mevedel-session-persistence-parse-iso-time
                  "mevedel-session-persistence" (str))
(declare-function mevedel-session-persistence-restore
                  "mevedel-session-persistence"
                  (session-dir &optional lifecycle-source session-override
                               workspace))
(autoload 'mevedel-session-persistence-delete "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-list-sessions
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-restore "mevedel-session-persistence")

;; `mevedel-structs'
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace-name "mevedel-workspace" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-workspace" (cl-x) t)
(declare-function mevedel-workspace-state-dir "mevedel-workspace" (workspace))


;;
;;; Credentials

(defconst mevedel-collaboration-lobby--max-sessions 200
  "Most sessions one lobby frame lists, newest first.")

(defconst mevedel-collaboration-lobby--max-preview-chars 160
  "Longest prompt preview one lobby row carries.")

(defvar mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal)
  "Live lobbies, keyed by their workspace root.")

(defun mevedel-collaboration-lobby--credentials-path (workspace)
  "Return the file holding WORKSPACE's lobby credentials."
  (file-name-concat (mevedel-workspace-state-dir workspace) "lobby"))

(defun mevedel-collaboration-lobby--read-credentials (path)
  "Return the valid lobby credentials stored at PATH, or nil.
The result is a plist of `:room-id' and the unibyte `:key',
`:write-token' and `:owner-token'."
  (when-let* (((file-readable-p path))
              (stored (condition-case nil
                          (with-temp-buffer
                            (insert-file-contents path)
                            (read (current-buffer)))
                        (error nil)))
              ((proper-list-p stored))
              (room-id (plist-get stored :room-id))
              ((stringp room-id))
              ((string-match-p "\\`[A-Za-z0-9_-]\\{10,64\\}\\'" room-id))
              (key (mevedel-collaboration--base64url-decode
                    (plist-get stored :key)))
              (write-token (mevedel-collaboration--base64url-decode
                            (plist-get stored :write-token)))
              (owner-token (mevedel-collaboration--base64url-decode
                            (plist-get stored :owner-token)))
              ((= 32 (length key)))
              ((= 16 (length write-token)))
              ((= 16 (length owner-token))))
    (list :room-id room-id :key key
          :write-token write-token :owner-token owner-token)))

(defun mevedel-collaboration-lobby--credentials (workspace &optional rotate)
  "Return WORKSPACE's lobby credentials, creating them when needed.
Missing or unreadable credentials are generated and stored, readable
only by this user.  ROTATE replaces stored ones, which revokes every
link made from them."
  (let ((path (mevedel-collaboration-lobby--credentials-path workspace)))
    (or (and (not rotate)
             (mevedel-collaboration-lobby--read-credentials path))
        (let ((credentials
               (list :room-id (mevedel-collaboration--base64url
                               (mevedel-collaboration--random-bytes 16))
                     :key (mevedel-collaboration--random-bytes 32)
                     :write-token (mevedel-collaboration--random-bytes 16)
                     :owner-token (mevedel-collaboration--random-bytes 16))))
          (make-directory (file-name-directory path) t)
          ;; The modes apply on creation only, so an old file goes first.
          (when (file-exists-p path)
            (delete-file path))
          (with-file-modes #o600
            (with-temp-file path
              (prin1 (list :room-id (plist-get credentials :room-id)
                           :key (mevedel-collaboration--base64url
                                 (plist-get credentials :key))
                           :write-token (mevedel-collaboration--base64url
                                         (plist-get credentials :write-token))
                           :owner-token (mevedel-collaboration--base64url
                                         (plist-get credentials :owner-token)))
                     (current-buffer))))
          credentials))))


;;
;;; Listing

(defun mevedel-collaboration-lobby--preview (summary)
  "Return SUMMARY's bounded prompt preview, or nil."
  (when-let* ((text (or (plist-get summary :latest-user-message)
                        (plist-get summary :first-user-message)))
              ((stringp text)))
    (truncate-string-to-width
     (string-trim (replace-regexp-in-string "[[:space:]]+" " " text))
     mevedel-collaboration-lobby--max-preview-chars nil nil "…")))

(defun mevedel-collaboration-lobby--updated (summary)
  "Return SUMMARY's last update in seconds since the epoch, or nil."
  (when-let* ((time (mevedel-session-persistence-parse-iso-time
                     (plist-get summary :updated-at))))
    (truncate (float-time time))))

(defun mevedel-collaboration-lobby--live-sessions (workspace)
  "Return WORKSPACE's live root sessions as (ID NAME . BUFFER) entries."
  (let (live)
    (pcase-dolist (`(,name . ,buffer) (mevedel--workspace-sessions workspace))
      (when-let* ((session (buffer-local-value 'mevedel--session buffer))
                  (id (mevedel-session-session-id session)))
        (push (cons id (cons name buffer)) live)))
    (nreverse live)))

(defun mevedel-collaboration-lobby--rows (workspace)
  "Return WORKSPACE's session rows for the lobby, newest first.
Live sessions that were never saved lead; saved ones follow in their
own order, marked live or shared when they are open here.  A row's
`:updated' is in seconds since the epoch."
  (let* ((live (mevedel-collaboration-lobby--live-sessions workspace))
         (saved (mevedel-session-persistence-list-sessions workspace))
         (saved-ids (mapcar (lambda (entry)
                              (plist-get (plist-get entry :summary)
                                         :session-id))
                            saved))
         (row (lambda (id name updated preview)
                (let ((buffer (cddr (assoc id live))))
                  (list :id id :name (or name "Untitled")
                        :updated updated :preview preview
                        :live (if buffer t :json-false)
                        :shared (if (mevedel-collaboration--room-for-buffer
                                     buffer)
                                    t :json-false))))))
    (append
     (cl-loop for (id name . _buffer) in live
              unless (member id saved-ids)
              collect (funcall row id name nil nil))
     (cl-loop for entry in saved
              for summary = (plist-get entry :summary)
              for id = (plist-get summary :session-id)
              when (stringp id)
              collect (funcall row id
                               (or (cadr (assoc id live))
                                   (plist-get summary :session-name))
                               (mevedel-collaboration-lobby--updated summary)
                               (mevedel-collaboration-lobby--preview
                                summary))))))

(defun mevedel-collaboration-lobby--frame (lobby)
  "Return LOBBY's session listing frame."
  (let ((rows (mevedel-collaboration-lobby--rows
               (plist-get lobby :workspace))))
    (list :t "lobby"
          :project (plist-get lobby :project)
          :workspace (mevedel-collaboration--workspace-key lobby)
          :sessions (vconcat (seq-take
                              rows mevedel-collaboration-lobby--max-sessions))
          :omitted (max 0 (- (length rows)
                             mevedel-collaboration-lobby--max-sessions)))))

(defun mevedel-collaboration-lobby--send-listing (lobby peer)
  "Send LOBBY's session listing to PEER.
An owner also receives the models it may create a session on."
  (mevedel-collaboration--transport-send
   (plist-get lobby :transport) peer
   (append (mevedel-collaboration-lobby--frame lobby)
           (when (plist-get (mevedel-collaboration--guest lobby peer) :owner)
             (list :models (mevedel-collaboration--model-labels))))))


;;
;;; Opening a session

(defun mevedel-collaboration-lobby--saved-entry (workspace id)
  "Return WORKSPACE's saved session entry for session ID, or nil.
The id only selects among sessions the listing would show; it never
becomes a path of its own."
  (cl-find id (mevedel-session-persistence-list-sessions workspace)
           :key (lambda (entry)
                  (plist-get (plist-get entry :summary) :session-id))
           :test #'equal))

(defun mevedel-collaboration-lobby--session-buffer (lobby id)
  "Return the data buffer for LOBBY's session ID, restoring it if saved.
Signal an error when ID names no session of LOBBY's workspace."
  (let ((workspace (plist-get lobby :workspace)))
    (or (cddr (assoc id (mevedel-collaboration-lobby--live-sessions
                         workspace)))
        (if-let* ((entry (mevedel-collaboration-lobby--saved-entry
                          workspace id)))
            (mevedel-session-persistence-restore
             (plist-get entry :save-path) nil nil workspace)
          (error "No such session")))))

(defun mevedel-collaboration-lobby--open-reply (lobby peer request-id &rest keys)
  "Send PEER in LOBBY the outcome KEYS of open REQUEST-ID."
  (mevedel-collaboration--transport-send
   (plist-get lobby :transport) peer
   (append (list :t "open-session" :reqId request-id) keys)))

(defun mevedel-collaboration-lobby--handle-open (lobby peer frame)
  "Share the session FRAME names and hand writable PEER its link.
The session is resumed when it is not live and shared when it is not
already; PEER receives the room at its own tier."
  (when-let* ((guest (mevedel-collaboration--guest lobby peer))
              (request-id (plist-get frame :reqId))
              ((mevedel-collaboration--request-id-p request-id)))
    (let ((id (plist-get frame :id))
          link failure)
      (cond
       ((not (plist-get guest :writable))
        (setq failure "A view link cannot open sessions"))
       ((not (and (stringp id) (not (string-empty-p id))))
        (setq failure "No session named"))
       (t
        (condition-case err
            (let* ((buffer (mevedel-collaboration-lobby--session-buffer
                            lobby id))
                   (room (mevedel-collaboration--start
                          (buffer-local-value 'mevedel--session buffer)
                          buffer)))
              (setq link (mevedel-collaboration--guest-link room guest)))
          (inhibited-interaction
           (setq failure "This session needs a decision in Emacs first"))
          (error
           (setq failure (format "Session could not be opened: %s"
                                 (error-message-string err)))))))
      (if link
          (mevedel-collaboration-lobby--open-reply
           lobby peer request-id :ok t :link link)
        (mevedel-collaboration-lobby--open-reply
         lobby peer request-id :ok :json-false :message failure)))))


;;
;;; Deleting a session

(defun mevedel-collaboration-lobby--delete-failure (lobby guest id)
  "Delete LOBBY's saved session ID for GUEST; return why not, or nil.
A session live in Emacs is refused, since its buffer would save it
straight back, and so is one another client may still hold."
  (let* ((workspace (plist-get lobby :workspace))
         (live (assoc id (mevedel-collaboration-lobby--live-sessions
                          workspace)))
         entry)
    (cond
     ((not (plist-get guest :owner))
      "Only an owner link can delete sessions")
     ((not (and (stringp id) (not (string-empty-p id))))
      "No session named")
     (live (format "Close %s in Emacs first" (cadr live)))
     ((not (setq entry (mevedel-collaboration-lobby--saved-entry
                        workspace id)))
      "No such session")
     ((not (mevedel-session-persistence-delete
            workspace (plist-get entry :save-path)))
      "The session is still in use elsewhere"))))

(defun mevedel-collaboration-lobby--handle-delete (lobby peer frame)
  "Delete the saved session FRAME names for owner PEER.
Every guest of LOBBY receives the new listing after a deletion."
  (when-let* ((guest (mevedel-collaboration--guest lobby peer))
              (request-id (plist-get frame :reqId))
              ((mevedel-collaboration--request-id-p request-id)))
    (let ((failure (condition-case err
                       (mevedel-collaboration-lobby--delete-failure
                        lobby guest (plist-get frame :id))
                     (error (format "Session could not be deleted: %s"
                                    (error-message-string err))))))
      (mevedel-collaboration--transport-send
       (plist-get lobby :transport) peer
       (append (list :t "delete-session" :reqId request-id)
               (if failure (list :ok :json-false :message failure)
                 (list :ok t))))
      (unless failure
        (maphash (lambda (other _guest)
                   (mevedel-collaboration-lobby--send-listing lobby other))
                 (plist-get lobby :guests))))))


;;
;;; Guest frames

(defun mevedel-collaboration-lobby--on-frame (root peer frame)
  "Dispatch guest FRAME from PEER for the lobby of workspace ROOT.
A fault in one frame is reported and dropped; it never ends the lobby,
whose link is meant to keep working."
  (when-let* ((lobby (gethash root mevedel-collaboration-lobby--lobbies)))
    (condition-case err
        (let ((inhibit-interaction t))
          (pcase (plist-get frame :t)
            ("hello"
             (when (mevedel-collaboration--admit-hello lobby peer frame)
               (mevedel-collaboration-lobby--send-listing lobby peer)))
            ("lobby-refresh"
             (when (mevedel-collaboration--guest lobby peer)
               (mevedel-collaboration-lobby--send-listing lobby peer)))
            ("open-session"
             (mevedel-collaboration-lobby--handle-open lobby peer frame))
            ("new-session"
             (mevedel-collaboration--handle-new-session lobby peer frame))
            ("delete-session"
             (mevedel-collaboration-lobby--handle-delete lobby peer frame))
            ("files"
             (mevedel-collaboration-files-handle-list lobby peer frame root))
            ("file-get"
             (mevedel-collaboration-files-handle-get lobby peer frame root))
            ("file-upload"
             (mevedel-collaboration-files-handle-upload
              lobby peer frame root))
            ("file-remove"
             (mevedel-collaboration-files-handle-remove
              lobby peer frame root))))
      (error
       (message "mevedel: lobby frame failed: %s"
                (error-message-string err))))))

(defun mevedel-collaboration-lobby--on-control (root event peer)
  "Forget PEER in ROOT's lobby when relay control EVENT says it left."
  (when-let* ((lobby (gethash root mevedel-collaboration-lobby--lobbies))
              ((eq event 'peer-left)))
    (remhash peer (plist-get lobby :guests))))

(defun mevedel-collaboration-lobby--on-state (root state)
  "Track relay transport STATE for ROOT's lobby.
The relay collects the room with the host connection, so every guest
rejoins with a fresh hello after a drop."
  (when-let* ((lobby (gethash root mevedel-collaboration-lobby--lobbies))
              ((eq state 'down)))
    (clrhash (plist-get lobby :guests))))


;;
;;; Lifecycle

(defun mevedel-collaboration-lobby--find (workspace)
  "Return WORKSPACE's live lobby, or nil."
  (gethash (mevedel-workspace-root workspace)
           mevedel-collaboration-lobby--lobbies))

(defun mevedel-collaboration-lobby--workspace (directory)
  "Return the workspace DIRECTORY belongs to, or signal `user-error'."
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (unless (file-directory-p dir)
      (user-error "%s is not a directory" dir))
    (or (with-temp-buffer
          (setq default-directory dir)
          (mevedel-workspace))
        (user-error "No mevedel workspace at %s" dir))))

(defun mevedel-collaboration-lobby-start (directory)
  "Start the lobby for DIRECTORY's workspace and return it.
Return the live lobby when one is already running.  The returned plist
carries the bearer links under `:link-view', `:link-full' and
`:link-owner'; they stay valid across restarts until the lobby is
rotated."
  (require 'mevedel-collaboration-transport)
  (unless (require 'websocket nil t)
    (user-error "Collaboration requires the 'websocket' package; install it first"))
  (let* ((workspace (mevedel-collaboration-lobby--workspace directory))
         (root (mevedel-workspace-root workspace)))
    (or (mevedel-collaboration-lobby--find workspace)
        (let* ((origins (mevedel-collaboration--relay-origins))
               (credentials
                (mevedel-collaboration-lobby--credentials workspace))
               (room-id (plist-get credentials :room-id))
               (key (plist-get credentials :key))
               (write-token (plist-get credentials :write-token))
               (owner-token (plist-get credentials :owner-token))
               (project (or (mevedel-workspace-name workspace)
                            (file-name-nondirectory
                             (directory-file-name root))))
               (lobby
                (append
                 (list :workspace workspace
                       :directory root
                       :project project
                       :session-label (format "Lobby: %s" project)
                       :room-id room-id
                       :key key
                       :write-token write-token
                       :owner-token owner-token
                       :guests (make-hash-table :test #'eql))
                 (mevedel-collaboration--links
                  (cdr origins) room-id key write-token owner-token))))
          (puthash root lobby mevedel-collaboration-lobby--lobbies)
          (add-hook 'kill-emacs-hook #'mevedel-collaboration-lobby--stop-all)
          (condition-case error-data
              (plist-put
               lobby :transport
               (mevedel-collaboration--transport-open
                (format "%s/r/%s?role=host" (car origins) room-id)
                key
                :headers (mevedel-collaboration--host-headers)
                :on-frame (lambda (peer frame)
                            (mevedel-collaboration-lobby--on-frame
                             root peer frame))
                :on-control (lambda (event peer)
                              (mevedel-collaboration-lobby--on-control
                               root event peer))
                :on-state (lambda (state)
                            (mevedel-collaboration-lobby--on-state
                             root state))))
            (error
             (remhash root mevedel-collaboration-lobby--lobbies)
             (signal (car error-data) (cdr error-data))))
          lobby))))

(defun mevedel-collaboration-lobby--stop (lobby reason)
  "Stop LOBBY for REASON, telling its guests unless Emacs is exiting.
Its credentials stay stored, so starting it again revives its links."
  (remhash (mevedel-workspace-root (plist-get lobby :workspace))
           mevedel-collaboration-lobby--lobbies)
  (when (zerop (hash-table-count mevedel-collaboration-lobby--lobbies))
    (remove-hook 'kill-emacs-hook #'mevedel-collaboration-lobby--stop-all))
  (ignore-errors (mevedel-collaboration-share-dismiss lobby))
  (when-let* ((transport (plist-get lobby :transport)))
    (unless (eq reason 'emacs-exit)
      (ignore-errors
        (when (> (hash-table-count (plist-get lobby :guests)) 0)
          (mevedel-collaboration--transport-send
           transport 0 (list :t "bye" :reason (format "%s" reason))))))
    (ignore-errors (mevedel-collaboration--transport-stop transport))))

(defun mevedel-collaboration-lobby--stop-all ()
  "Stop every lobby before Emacs exits."
  (let (lobbies)
    (maphash (lambda (_root lobby) (push lobby lobbies))
             mevedel-collaboration-lobby--lobbies)
    (dolist (lobby lobbies)
      (mevedel-collaboration-lobby--stop lobby 'emacs-exit))))

(defun mevedel-collaboration-lobby--status ()
  "Return a status line for every live lobby, or nil."
  (let (lines)
    (maphash
     (lambda (_root lobby)
       (push (format "%s: relay %s; %d guest%s"
                     (plist-get lobby :session-label)
                     (if (mevedel-collaboration--transport-open-p
                          (plist-get lobby :transport))
                         "connected" "reconnecting")
                     (hash-table-count (plist-get lobby :guests))
                     (if (= 1 (hash-table-count (plist-get lobby :guests)))
                         "" "s"))
             lines))
     mevedel-collaboration-lobby--lobbies)
    (and lines (mapconcat #'identity (nreverse lines) "; "))))


;;
;;; Commands

(defun mevedel-collaboration-lobby ()
  "Start the current workspace's lobby, or show its links."
  (interactive)
  (let* ((workspace (or (mevedel-workspace)
                        (user-error "No mevedel workspace here")))
         (root (mevedel-workspace-root workspace)))
    (unless (or (mevedel-collaboration-lobby--find workspace)
                (yes-or-no-p
                 (concat
                  "Share this project's session list through the relay? "
                  "Its links stay valid across restarts until rotated, and "
                  "a full or owner link can open sessions and read, upload "
                  "and remove project files; an owner link can also create "
                  "sessions. ")))
      (user-error "Lobby not started"))
    (mevedel-collaboration-share-present
     (mevedel-collaboration-lobby-start root))))

(defun mevedel-collaboration-lobby-stop ()
  "Stop the current workspace's lobby; its links work again on restart."
  (interactive)
  (if-let* ((workspace (mevedel-workspace))
            (lobby (mevedel-collaboration-lobby--find workspace)))
      (progn
        (mevedel-collaboration-lobby--stop lobby 'user-stop)
        (message "mevedel: lobby stopped for %s" (plist-get lobby :project)))
    (message "mevedel: no lobby is running for this workspace")))

(defun mevedel-collaboration-lobby-rotate ()
  "Replace the current workspace's lobby credentials.
Every earlier lobby link stops working; the lobby restarts when it was
running."
  (interactive)
  (let* ((workspace (or (mevedel-workspace)
                        (user-error "No mevedel workspace here")))
         (lobby (mevedel-collaboration-lobby--find workspace)))
    (when lobby
      (mevedel-collaboration-lobby--stop lobby 'rotated))
    (mevedel-collaboration-lobby--credentials workspace t)
    (if lobby
        (mevedel-collaboration-share-present
         (mevedel-collaboration-lobby-start
          (mevedel-workspace-root workspace)))
      (message "mevedel: lobby credentials rotated"))))

(provide 'mevedel-collaboration-lobby)
;;; mevedel-collaboration-lobby.el ends here
