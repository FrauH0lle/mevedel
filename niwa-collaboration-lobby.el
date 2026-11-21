;;; niwa-collaboration-lobby.el --- Collaboration lobby -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the per-workspace lobby: one bookmarkable browser room that lists
;; the workspace's live and saved sessions, opens one into its own shared
;; room, creates new ones, and deletes saved ones.
;;
;; A lobby is a room bound to a workspace rather than to a session.  Its
;; credentials persist in the workspace state directory, so its links
;; survive Emacs restarts; every session room it hands out keeps the
;; ordinary share lifetime.  A lobby also runs until it is stopped: the
;; roots of running lobbies are recorded in `niwa-user-dir', and
;; `niwa-collaboration-lobby-restore', which `niwa-install' runs
;; once startup is done, restarts them in the next Emacs.
;;
;; Link tiers keep their meaning: a view link lists sessions, a full link
;; also opens them and works with the project files
;; (`niwa-collaboration-files'), and an owner link also creates and
;; deletes sessions.  A guest is always handed a session room at its
;; own tier.
;;
;; Guest frames arrive while nobody may be at the keyboard, so lobby work
;; runs with `inhibit-interaction': a step that would ask in Emacs is
;; refused to the guest instead of waiting for an answer.

;;; Code:

(require 'cl-lib)
(require 'niwa-collaboration)
(require 'niwa-workspace)
(require 'niwa-utilities)

;; `niwa-chat'
(declare-function niwa--workspace-sessions "niwa-chat" (workspace))
(declare-function niwa-session-dedicated-artifact "niwa-structs" (cl-x))
(autoload 'niwa--workspace-sessions "niwa-chat")

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--admit-hello
                  "niwa-collaboration-guest" (room peer frame))
(declare-function niwa-collaboration--announce-lobby
                  "niwa-collaboration-guest" (workspace))
(declare-function niwa-collaboration--dispatch-shared-frame
                  "niwa-collaboration-guest" (room peer frame root))
(declare-function niwa-collaboration--guard-guest-frame
                  "niwa-collaboration-guest" (room peer frame dispatch))
(declare-function niwa-collaboration--handle-push-subscription
                  "niwa-collaboration-guest" (room peer frame &optional persist))
(declare-function niwa-collaboration--push-endpoint-p
                  "niwa-collaboration-guest" (value))
(declare-function niwa-collaboration--restore-push-subscriptions
                  "niwa-collaboration-guest" (room))
(declare-function niwa-collaboration--model-labels
                  "niwa-collaboration-guest" ())
(declare-function niwa-collaboration--project-name
                  "niwa-collaboration-guest" (workspace))
(declare-function niwa-collaboration--request-id-p
                  "niwa-collaboration-guest" (value))
(declare-function niwa-collaboration--workspace-key
                  "niwa-collaboration-guest" (room))
(autoload 'niwa-collaboration--admit-hello "niwa-collaboration-guest")
;; Loading it loads the shared frame handlers too, so a frame from a peer
;; the lobby never admitted still finds them.
(autoload 'niwa-collaboration--dispatch-shared-frame
  "niwa-collaboration-guest")
(autoload 'niwa-collaboration--guard-guest-frame
  "niwa-collaboration-guest")
(autoload 'niwa-collaboration--model-labels "niwa-collaboration-guest")
(autoload 'niwa-collaboration--request-id-p "niwa-collaboration-guest")
(autoload 'niwa-collaboration--workspace-key "niwa-collaboration-guest")

;; `niwa-collaboration-presence'
(declare-function niwa-collaboration-presence-publish
                  "niwa-collaboration-presence" (room))

;; `niwa-collaboration-share'
(declare-function niwa-collaboration-share-dismiss
                  "niwa-collaboration-share" (room))
(declare-function niwa-collaboration-share-present
                  "niwa-collaboration-share" (room))
(autoload 'niwa-collaboration-share-dismiss "niwa-collaboration-share")
(autoload 'niwa-collaboration-share-present "niwa-collaboration-share")

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-open
                  "niwa-collaboration-transport" (url key &rest callbacks))
(declare-function niwa-collaboration--transport-open-p
                  "niwa-collaboration-transport" (transport))
(declare-function niwa-collaboration--transport-send
                  "niwa-collaboration-transport" (transport peer frame))
(declare-function niwa-collaboration--transport-stop
                  "niwa-collaboration-transport" (transport))
(autoload 'niwa-collaboration--transport-open "niwa-collaboration-transport")
(autoload 'niwa-collaboration--transport-open-p "niwa-collaboration-transport")
(autoload 'niwa-collaboration--transport-send "niwa-collaboration-transport")
(autoload 'niwa-collaboration--transport-stop "niwa-collaboration-transport")

;; `niwa-collaboration-files'
(declare-function niwa-collaboration-files-handle-get
                  "niwa-collaboration-files" (owner peer frame root))
(declare-function niwa-collaboration-files-handle-list
                  "niwa-collaboration-files" (owner peer frame root))
(declare-function niwa-collaboration-files-handle-remove
                  "niwa-collaboration-files" (owner peer frame root))

;; `niwa-schedule'
(declare-function niwa-schedule-format-time "niwa-schedule" (seconds))
(declare-function niwa-schedule-timing-label "niwa-schedule" (timing))
(autoload 'niwa-schedule-format-time "niwa-schedule")
(autoload 'niwa-schedule-timing-label "niwa-schedule")

;; `niwa-session-naming'
(declare-function niwa-session-naming-rename "niwa-session-naming" (session buffer name &optional current-p))
(defvar niwa-session-naming-renamed-hook)
(autoload 'niwa-session-naming-rename "niwa-session-naming")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-delete
                  "niwa-session-persistence" (workspace save-path))
(declare-function niwa-session-persistence-list-sessions
                  "niwa-session-persistence" (workspace &optional cached))
(declare-function niwa-session-persistence-parse-iso-time
                  "niwa-session-persistence" (str))
(declare-function niwa-session-persistence-restore
                  "niwa-session-persistence"
                  (session-dir &optional lifecycle-source session-override
                               workspace))
(declare-function niwa-session-persistence-without-dedicated
                  "niwa-session-persistence" (workspace entries))

;; `niwa-turn'
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-turn-busy-p "niwa-turn")
(autoload 'niwa-session-persistence-delete "niwa-session-persistence")
(autoload 'niwa-session-persistence-without-dedicated
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-list-sessions
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-restore "niwa-session-persistence")
(autoload 'niwa-session-persistence-parse-iso-time "niwa-session-persistence")

;; `niwa-sleep'
(declare-function niwa-sleep-update "niwa-sleep" (&rest _))
(autoload 'niwa-sleep-update "niwa-sleep")

;; `niwa-structs'
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa-user-dir)

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(declare-function niwa-workspace-name "niwa-workspace" (cl-x) t)
(declare-function niwa-workspace-root "niwa-workspace" (cl-x) t)
(declare-function niwa-workspace-state-dir "niwa-workspace" (workspace))

;; `niwa-workspace-schedule'
(declare-function niwa-workspace-schedule-list "niwa-workspace-schedule" (workspace))
(defvar niwa-workspace-schedule-changed-functions)
(autoload 'niwa-workspace-schedule-list "niwa-workspace-schedule")

;; `niwa-workspace-schedule-runs'
(declare-function niwa-workspace-schedule-set-paused "niwa-workspace-schedule-runs"
                  (workspace id paused))
(autoload 'niwa-workspace-schedule-set-paused "niwa-workspace-schedule-runs")


;;
;;; Credentials

(defconst niwa-collaboration-lobby--max-sessions 200
  "Most sessions one lobby frame lists, newest first.")

(defconst niwa-collaboration-lobby--max-preview-chars 160
  "Longest prompt preview one lobby row carries.")

(defvar niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal)
  "Live lobbies, keyed by their workspace root.")

(defun niwa-collaboration-lobby--credentials-path (workspace)
  "Return the file holding WORKSPACE's lobby credentials."
  (file-name-concat (niwa-workspace-state-dir workspace) "lobby"))

(defun niwa-collaboration-lobby--read-credentials (path)
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
              (key (niwa-collaboration--base64url-decode
                    (plist-get stored :key)))
              (write-token (niwa-collaboration--base64url-decode
                            (plist-get stored :write-token)))
              (owner-token (niwa-collaboration--base64url-decode
                            (plist-get stored :owner-token)))
              ((= 32 (length key)))
              ((= 16 (length write-token)))
              ((= 16 (length owner-token))))
    (list :room-id room-id :key key
          :write-token write-token :owner-token owner-token)))

(defun niwa-collaboration-lobby--credentials (workspace &optional rotate)
  "Return WORKSPACE's lobby credentials, creating them when needed.
Missing or unreadable credentials are generated and stored, readable
only by this user.  ROTATE replaces stored ones, which revokes every
link made from them."
  (let ((path (niwa-collaboration-lobby--credentials-path workspace)))
    (or (and (not rotate)
             (niwa-collaboration-lobby--read-credentials path))
        (let ((credentials
               (list :room-id (niwa-collaboration--base64url
                               (niwa-collaboration--random-bytes 16))
                     :key (niwa-collaboration--random-bytes 32)
                     :write-token (niwa-collaboration--random-bytes 16)
                     :owner-token (niwa-collaboration--random-bytes 16))))
          (make-directory (file-name-directory path) t)
          ;; The modes apply on creation only, so an old file goes first.
          (when (file-exists-p path)
            (delete-file path))
          (with-file-modes #o600
            (with-temp-file path
              (prin1 (list :room-id (plist-get credentials :room-id)
                           :key (niwa-collaboration--base64url
                                 (plist-get credentials :key))
                           :write-token (niwa-collaboration--base64url
                                         (plist-get credentials :write-token))
                           :owner-token (niwa-collaboration--base64url
                                         (plist-get credentials :owner-token)))
                     (current-buffer))))
          credentials))))


;;
;;; Running across restarts

(defconst niwa-collaboration-lobby--push-limit 64
  "Maximum subscriptions per room, as in the relay's maxPushSubscriptions.")

(define-error 'niwa-collaboration-lobby-push-refused "Lobby push refused")

(defun niwa-collaboration-lobby--push-path (workspace room-id)
  "Return WORKSPACE's private endpoint store for the credential generation ROOM-ID."
  (file-name-concat (niwa-workspace-state-dir workspace)
                    (concat "lobby-push-" room-id ".json")))

(defun niwa-collaboration-lobby--load-push (path)
  "Read validated subscriptions from PATH; never silently replace damaged state."
  (let ((subscriptions (make-hash-table :test #'equal)))
    (when-let* ((attrs (file-attributes path)))
      (unless (and (file-regular-p path) (not (file-attribute-type attrs))
                   (= 1 (file-attribute-link-number attrs)))
        (error "Unsafe lobby subscription file"))
      (with-temp-buffer
        ;; 64 IDs and double-escaped 2 KiB endpoints fit in 300 KiB.
        (unless (<= (file-attribute-size attrs) (* 300 1024))
          (error "Oversized lobby subscription file"))
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents path nil 0 (* 300 1024)))
        (let ((endpoints (json-parse-buffer)))
          (unless (and (hash-table-p endpoints)
                       (<= (hash-table-count endpoints) niwa-collaboration-lobby--push-limit)
                       (progn (skip-chars-forward " \t\r\n") (eobp)))
            (error "Invalid lobby subscription file"))
          (maphash (lambda (id endpoint)
                     (unless (and (niwa-collaboration--sanitize-guest-id id)
                                  (niwa-collaboration--push-endpoint-p endpoint))
                       (error "Invalid stored lobby subscription"))
                     (puthash id (list :endpoint endpoint :writable t) subscriptions))
                   endpoints))))
    subscriptions))

(defun niwa-collaboration-lobby--restore-push (workspace room-id)
  "Load ROOM-ID's subscriptions without making WORKSPACE's lobby depend on them.
If its damaged file cannot be quarantined privately, nil disables mutations."
  (let ((path (niwa-collaboration-lobby--push-path workspace room-id)))
    (condition-case nil
        (niwa-collaboration-lobby--load-push path)
      (error
       (let ((saved
              (ignore-errors
                (when (and (file-regular-p path) (not (file-symlink-p path))
                           (= 1 (file-attribute-link-number (file-attributes path))))
                  (let ((backup (make-temp-name (concat path ".damaged-"))))
                    (set-file-modes path #o600)
                    (rename-file path backup nil)
                    backup)))))
         (niwa-diagnostic-warning
          (niwa-workspace-root workspace) 'niwa
          (if saved
              (format "Lobby subscriptions quarantined at %s; devices can re-enroll" saved)
            (format "Cannot safely quarantine %s; lobby subscriptions disabled until repaired" path)))
         (and saved (make-hash-table :test #'equal)))))))

(defun niwa-collaboration-lobby--persist-push (lobby id endpoint)
  "Store ID's new ENDPOINT (nil to unsubscribe) before changing live LOBBY."
  (let* ((subscriptions (or (plist-get lobby :push-guests)
                            (signal 'niwa-collaboration-lobby-push-refused
                                    '("Lobby notifications are unavailable; ask the host to repair them."))))
         (current (gethash id subscriptions)))
    (unless (equal endpoint (plist-get current :endpoint))
      (when (and endpoint (not current)
                 (>= (hash-table-count subscriptions) niwa-collaboration-lobby--push-limit))
        (signal 'niwa-collaboration-lobby-push-refused
                '("Lobby notifications are full; unsubscribe another device and try again.")))
      (let ((endpoints (make-hash-table :test #'equal)))
        (maphash (lambda (key value) (puthash key (plist-get value :endpoint) endpoints))
                 subscriptions)
        (if endpoint (puthash id endpoint endpoints) (remhash id endpoints))
        (niwa--write-file-atomically
         (niwa-collaboration-lobby--push-path (plist-get lobby :workspace)
                                                 (plist-get lobby :room-id))
         (json-serialize endpoints) 'utf-8-unix #o600)))))

(defun niwa-collaboration-lobby--intent-path ()
  "Return the file listing the roots whose lobbies run until stopped."
  (file-name-concat niwa-user-dir "lobbies.el"))

(defun niwa-collaboration-lobby--intended ()
  "Return the workspace roots whose lobbies should be running.
An unreadable or malformed record yields nil."
  (let ((path (niwa-collaboration-lobby--intent-path)))
    (when-let* (((file-readable-p path))
                (stored (condition-case nil
                            (with-temp-buffer
                              (insert-file-contents path)
                              (read (current-buffer)))
                          (error nil)))
                ((proper-list-p stored)))
      (seq-filter #'stringp stored))))

(defun niwa-collaboration-lobby--set-intended (root running)
  "Record whether the lobby of workspace ROOT is RUNNING.
A recorded lobby is restarted when Emacs next starts.  A record that
cannot be written is reported, since the lobby itself is unaffected."
  (let* ((roots (niwa-collaboration-lobby--intended))
         (updated (if running
                      (if (member root roots) roots (append roots (list root)))
                    (remove root roots)))
         (path (niwa-collaboration-lobby--intent-path)))
    (unless (equal updated roots)
      (condition-case err
          (if (null updated)
              (delete-file path)
            (make-directory (file-name-directory path) t)
            (with-temp-file path
              (prin1 updated (current-buffer))))
        (error
         (niwa-diagnostic-warning
          root 'niwa
          (format "Could not record the lobby of %s for the next start: %s"
                  root (error-message-string err))))))))

(defun niwa-collaboration-lobby-restore ()
  "Restart the lobbies that were running when Emacs last exited.
Stopping a lobby is what ends it, so a lobby still running at exit
comes back with its links.  A lobby that cannot start is reported as a
warning and stays recorded for the next start, unless its directory is
gone, in which case it is forgotten."
  (dolist (root (niwa-collaboration-lobby--intended))
    (condition-case err
        (let ((inhibit-interaction t))
          (if (not (file-directory-p root))
              (progn
                (niwa-collaboration-lobby--set-intended root nil)
                (niwa-diagnostic-warning
                 root 'niwa (format "Lobby of %s forgotten: the directory is gone"
                                  root)))
            (niwa-collaboration-lobby-start root)))
      (error
       (niwa-diagnostic-warning
        root 'niwa
        (format (concat "Lobby of %s not restarted: %s "
                        "(/collab lobby stop there ends the retries)")
                root (error-message-string err)))))))


;;
;;; Listing

(defun niwa-collaboration-lobby--preview (summary)
  "Return SUMMARY's bounded prompt preview, or nil."
  (when-let* ((text (or (plist-get summary :latest-user-message)
                        (plist-get summary :first-user-message)))
              ((stringp text)))
    (truncate-string-to-width
     (string-trim (replace-regexp-in-string "[[:space:]]+" " " text))
     niwa-collaboration-lobby--max-preview-chars nil nil "…")))

(defun niwa-collaboration-lobby--updated (summary)
  "Return SUMMARY's last update in seconds since the epoch, or nil."
  (when-let* ((time (niwa-session-persistence-parse-iso-time
                     (plist-get summary :updated-at))))
    (truncate (float-time time))))

(defun niwa-collaboration-lobby--live-sessions (workspace)
  "Return WORKSPACE's listed live root sessions as (ID NAME . BUFFER) entries.
Dedicated artifact conversations are reached only through their artifact."
  (let (live)
    (pcase-dolist (`(,name . ,buffer) (niwa--workspace-sessions workspace))
      (when-let* ((session (buffer-local-value 'niwa--session buffer))
                  ((not (niwa-session-dedicated-artifact session)))
                  (id (niwa-session-session-id session)))
        (push (cons id (cons name buffer)) live)))
    (nreverse live)))

(defun niwa-collaboration-lobby--rows (workspace)
  "Return WORKSPACE's session rows for the lobby, newest first.
Live sessions that were never saved lead; saved ones follow in their
own order, marked live or shared when they are open here.  A row's
`:updated' is in seconds since the epoch."
  (let* ((live (niwa-collaboration-lobby--live-sessions workspace))
         (saved (niwa-session-persistence-without-dedicated
                 workspace (niwa-session-persistence-list-sessions workspace)))
         (saved-ids (mapcar (lambda (entry)
                              (plist-get (plist-get entry :summary)
                                         :session-id))
                            saved))
         (row (lambda (id name updated preview)
                (let ((buffer (cddr (assoc id live))))
                  (list :id id :name (or name "Untitled")
                        :updated updated :preview preview
                        :live (if buffer t :json-false)
                        :shared (if (niwa-collaboration--room-for-buffer
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
                               (niwa-collaboration-lobby--updated summary)
                               (niwa-collaboration-lobby--preview
                                summary))))))

(defun niwa-collaboration-lobby--frame (lobby)
  "Return LOBBY's session listing frame."
  (let ((rows (niwa-collaboration-lobby--rows
               (plist-get lobby :workspace))))
    (list :t "lobby"
          :project (plist-get lobby :project)
          :workspace (niwa-collaboration--workspace-key lobby)
          :sessions (vconcat (seq-take
                              rows niwa-collaboration-lobby--max-sessions))
          :omitted (max 0 (- (length rows)
                             niwa-collaboration-lobby--max-sessions)))))

(defun niwa-collaboration-lobby--listings (lobby)
  "Return LOBBY's (VIEW . WRITABLE) listings from one store snapshot."
  (let ((frame (niwa-collaboration-lobby--frame lobby)) view writable)
    (condition-case err
        (progn
          (dolist (job (niwa-workspace-schedule-list (plist-get lobby :workspace)))
            (let* ((outcome (plist-get job :last-outcome))
                   (due (plist-get job :due))
                   (row (append (list :id (plist-get job :id)
                                      :timing (niwa-schedule-timing-label (plist-get job :timing))
                                      :nextRun (and due (niwa-schedule-format-time due))
                                      :status (symbol-name (plist-get job :status)))
                                (when (memq outcome '(done skipped expired failed))
                                  (list :lastOutcome (symbol-name outcome))))))
              (push row view)
              (push (append row (list :name (plist-get job :name))) writable)))
          (cons (append frame (list :schedules (vconcat (nreverse view))))
                (append frame (list :schedules (vconcat (nreverse writable))))))
      (error
       (niwa--warn-once (plist-get lobby :directory) 'lobby-schedule-listing
                           "Could not list lobby schedules: %s" (error-message-string err))
       (cons frame frame)))))

(defun niwa-collaboration-lobby--schedule-action (lobby peer frame)
  "Apply FRAME's pause or resume request for an admitted owner PEER of LOBBY."
  (when-let* ((guest (niwa-collaboration--guest lobby peer))
              (req (plist-get frame :reqId))
              ((niwa-collaboration--request-id-p req)))
    (let* ((ws (plist-get lobby :workspace))
           (id (plist-get frame :id))
           (action (plist-get frame :action))
           (case-fold-search nil)
           (failure
            (cond
             ((not (plist-get guest :owner)) "Only an owner link can change schedules")
             ((not (and (member action '("pause" "resume")) (stringp id) (= 8 (length id))
                        (string-match-p "\\`s-[0-9a-f]\\{6\\}\\'" id))) "Invalid schedule action")
             (t
              (condition-case err
                  (if (seq-find (lambda (job) (equal id (plist-get job :id)))
                                (niwa-workspace-schedule-list ws))
                      (progn (niwa-workspace-schedule-set-paused ws id (equal action "pause")) nil)
                    "No such schedule")
                (error
                 (niwa-diagnostic-warning
                  (plist-get lobby :directory) 'niwa
                  (format "Could not %s schedule %s: %s" action id (error-message-string err)))
                 "Schedule could not be changed"))))))
      (niwa-collaboration--transport-send
       (plist-get lobby :transport) peer
       (append (list :t "schedule-action" :reqId req)
               (if failure (list :ok :json-false :message failure) (list :ok t)))))))

(defun niwa-collaboration-lobby--send-listing (lobby peer &optional listings)
  "Send LOBBY's listing to PEER, reusing LISTINGS when broadcasting.
An owner also receives the models it may create a session on."
  (let ((guest (niwa-collaboration--guest lobby peer))
        (listings (or listings (niwa-collaboration-lobby--listings lobby))))
    (niwa-collaboration--transport-send
     (plist-get lobby :transport) peer
     (append (if (plist-get guest :writable) (cdr listings) (car listings))
             (when (plist-get guest :owner)
               (list :models (niwa-collaboration--model-labels)))))))


;;
;;; Opening a session

(defun niwa-collaboration-lobby--saved-entry (workspace id)
  "Return WORKSPACE's saved session entry for session ID, or nil.
The id only selects among sessions the listing would show; it never
becomes a path of its own.  An artifact's dedicated session is reached
through its artifact."
  (cl-find id (niwa-session-persistence-without-dedicated
               workspace (niwa-session-persistence-list-sessions workspace))
           :key (lambda (entry)
                  (plist-get (plist-get entry :summary) :session-id))
           :test #'equal))

(defun niwa-collaboration-lobby--session-buffer (lobby id)
  "Return the data buffer for LOBBY's session ID, restoring it if saved.
Signal an error when ID names no session of LOBBY's workspace."
  (let ((workspace (plist-get lobby :workspace)))
    (or (cddr (assoc id (niwa-collaboration-lobby--live-sessions
                         workspace)))
        (if-let* ((entry (niwa-collaboration-lobby--saved-entry
                          workspace id)))
            (niwa-session-persistence-restore
             (plist-get entry :save-path) nil nil workspace)
          (error "No such session")))))

(defun niwa-collaboration-lobby--open-reply (lobby peer request-id &rest keys)
  "Send PEER in LOBBY the outcome KEYS of open REQUEST-ID."
  (niwa-collaboration--transport-send
   (plist-get lobby :transport) peer
   (append (list :t "open-session" :reqId request-id) keys)))

(defun niwa-collaboration-lobby--handle-open (lobby peer frame)
  "Share the session FRAME names and hand writable PEER its link.
The session is resumed when it is not live and shared when it is not
already; PEER receives the room at its own tier."
  (when-let* ((guest (niwa-collaboration--guest lobby peer))
              (request-id (plist-get frame :reqId))
              ((niwa-collaboration--request-id-p request-id)))
    (let ((id (plist-get frame :id))
          link failure)
      (cond
       ((not (plist-get guest :writable))
        (setq failure "A view link cannot open sessions"))
       ((not (and (stringp id) (not (string-empty-p id))))
        (setq failure "No session named"))
       (t
        (condition-case err
            (let* ((buffer (niwa-collaboration-lobby--session-buffer
                            lobby id))
                   (room (niwa-collaboration--start
                          (buffer-local-value 'niwa--session buffer)
                          buffer)))
              (setq link (niwa-collaboration--guest-link room guest)))
          (inhibited-interaction
           (setq failure "This session needs a decision in Emacs first"))
          (error
           (setq failure (format "Session could not be opened: %s"
                                 (error-message-string err)))))))
      (if link
          (niwa-collaboration-lobby--open-reply
           lobby peer request-id :ok t :link link)
        (niwa-collaboration-lobby--open-reply
         lobby peer request-id :ok :json-false :message failure)))))


;;
;;; Renaming a session

(defun niwa-collaboration-lobby--broadcast-listing (lobby)
  "Refresh the admitted guests of LOBBY from a single snapshot."
  (let ((guests (plist-get lobby :guests))
        (report (lambda (err)
                  (niwa-diagnostic-message
                   (niwa-diagnostic-scope (plist-get lobby :workspace))
                   "Lobby refresh failed: %s" (error-message-string err)))))
    (when (> (hash-table-count guests) 0)
      (condition-case err
          (let ((listings (niwa-collaboration-lobby--listings lobby)))
            (maphash (lambda (peer _guest)
                       (condition-case err
                           (niwa-collaboration-lobby--send-listing lobby peer listings)
                         (error (funcall report err)))) guests))
        (error (funcall report err))))))

(defun niwa-collaboration-lobby--workspace-changed (workspace)
  "Refresh the lobby for WORKSPACE after a committed change."
  (when-let* ((workspace) (lobby (niwa-collaboration-lobby--find workspace)))
    (niwa-collaboration-lobby--broadcast-listing lobby)))

(defun niwa-collaboration-lobby--session-renamed (session)
  "Refresh SESSION's lobby after its name commits."
  (niwa-collaboration-lobby--workspace-changed (niwa-session-workspace session)))

(add-hook 'niwa-session-naming-renamed-hook
          #'niwa-collaboration-lobby--session-renamed)
(add-hook 'niwa-workspace-schedule-changed-functions
          #'niwa-collaboration-lobby--workspace-changed)

(defun niwa-collaboration-lobby--handle-rename (lobby peer frame)
  "Rename the listed session FRAME names for owner PEER in LOBBY."
  (when-let* ((guest (niwa-collaboration--guest lobby peer))
              (request-id (plist-get frame :reqId))
              ((niwa-collaboration--request-id-p request-id)))
    (let ((failure
           (condition-case err
               (progn
                 (unless (plist-get guest :owner)
                   (user-error "Only an owner link can rename sessions"))
                 (let ((id (plist-get frame :id))
                       (name (niwa-collaboration--guest-text (plist-get frame :name))))
                   (unless (and (stringp id) (not (string-empty-p id)) name)
                     (user-error "A session and nonempty name are required"))
                   (let ((buffer (niwa-collaboration-lobby--session-buffer lobby id)))
                     (niwa-session-naming-rename
                      (buffer-local-value 'niwa--session buffer) buffer name)))
                 nil)
             (inhibited-interaction "This session needs a decision in Emacs first")
             (error (format "Session could not be renamed: %s" (error-message-string err))))))
      (niwa-collaboration--transport-send
       (plist-get lobby :transport) peer
       (append (list :t "rename-session" :reqId request-id)
               (if failure (list :ok :json-false :message failure) (list :ok t)))))))


;;
;;; Deleting a session

(defun niwa-collaboration-lobby--delete-failure (lobby guest id)
  "Delete LOBBY's session ID for GUEST; return why not, or nil.
A session live in Emacs is closed first, ending its room, unless a turn
is still running there; its buffer would otherwise save it straight
back.  One another client may still hold is refused."
  (let* ((workspace (plist-get lobby :workspace))
         (live (assoc id (niwa-collaboration-lobby--live-sessions
                          workspace)))
         entry)
    (cond
     ((not (plist-get guest :owner))
      "Only an owner link can delete sessions")
     ((not (and (stringp id) (not (string-empty-p id))))
      "No session named")
     ((and live (niwa-turn-busy-p (cddr live)))
      (format "%s is still working; stop it first" (cadr live)))
     ((progn
        (when live
          (let ((kill-buffer-query-functions nil)
                (niwa-collaboration-stop-reason 'session-deleted))
            (kill-buffer (cddr live))))
        (not (setq entry (niwa-collaboration-lobby--saved-entry
                          workspace id))))
      ;; A live session that was never saved is gone with its buffer.
      (unless live "No such session"))
     ((not (niwa-session-persistence-delete
            workspace (plist-get entry :save-path)))
      "The session is still in use elsewhere"))))

(defun niwa-collaboration-lobby--handle-delete (lobby peer frame)
  "Delete the saved session FRAME names for owner PEER.
Every guest of LOBBY receives the new listing after a deletion."
  (when-let* ((guest (niwa-collaboration--guest lobby peer))
              (request-id (plist-get frame :reqId))
              ((niwa-collaboration--request-id-p request-id)))
    (let ((failure (condition-case err
                       (niwa-collaboration-lobby--delete-failure
                        lobby guest (plist-get frame :id))
                     (error (format "Session could not be deleted: %s"
                                    (error-message-string err))))))
      (niwa-collaboration--transport-send
       (plist-get lobby :transport) peer
       (append (list :t "delete-session" :reqId request-id)
               (if failure (list :ok :json-false :message failure)
                 (list :ok t))))
      (unless failure
        (niwa-collaboration-lobby--broadcast-listing lobby)))))


;;
;;; Guest frames

(defun niwa-collaboration-lobby--on-frame (root peer frame)
  "Dispatch guest FRAME from PEER for the lobby of workspace ROOT.
A failing frame is answered to its sender as in a session room; it never
ends the lobby, whose link is meant to keep working."
  (when-let* ((lobby (gethash root niwa-collaboration-lobby--lobbies)))
    (niwa-collaboration--guard-guest-frame
     lobby peer frame
     (lambda (lobby peer frame)
       (niwa-collaboration-lobby--dispatch-frame lobby peer frame root)))))

(defun niwa-collaboration-lobby--dispatch-frame (lobby peer frame root)
  "Handle guest FRAME from PEER in LOBBY of workspace ROOT.
Types a session room shares fall through to
`niwa-collaboration--dispatch-shared-frame', whose status this
returns; for the lobby's own types the value is unspecified."
  (pcase (plist-get frame :t)
    ("hello"
     (when (niwa-collaboration--admit-hello lobby peer frame)
       (niwa-collaboration-lobby--send-listing lobby peer)
       (niwa-collaboration-presence-publish lobby)))
    ("lobby-refresh"
     (when (niwa-collaboration--guest lobby peer)
       (niwa-collaboration-lobby--send-listing lobby peer)))
    ((or "push-subscribe" "push-unsubscribe" "push-state")
     (when (plist-get (niwa-collaboration--guest lobby peer) :writable)
       (condition-case refusal
           (niwa-collaboration--handle-push-subscription
            lobby peer frame (apply-partially #'niwa-collaboration-lobby--persist-push lobby))
         (niwa-collaboration-lobby-push-refused
          (niwa-collaboration--transport-send
           (plist-get lobby :transport) peer
           (list :t "notice" :kind "push-refused" :message (cadr refusal)))))))
    ("open-session"
     (niwa-collaboration-lobby--handle-open lobby peer frame))
    ("delete-session"
     (niwa-collaboration-lobby--handle-delete lobby peer frame))
    ("schedule-action"
     (niwa-collaboration-lobby--schedule-action lobby peer frame))
    ("rename-session"
     (niwa-collaboration-lobby--handle-rename lobby peer frame))
    ("files"
     (niwa-collaboration-files-handle-list lobby peer frame root))
    ("file-get"
     (niwa-collaboration-files-handle-get lobby peer frame root))
    ("file-remove"
     (niwa-collaboration-files-handle-remove lobby peer frame root))
    (_ (niwa-collaboration--dispatch-shared-frame lobby peer frame root))))

(defun niwa-collaboration-lobby--on-control (root event peer)
  "Forget PEER in ROOT's lobby when relay control EVENT says it left."
  (when-let* ((lobby (gethash root niwa-collaboration-lobby--lobbies))
              ((eq event 'peer-left)))
    (when (gethash peer (plist-get lobby :guests))
      (plist-put lobby :last-guest-departure (float-time)))
    (remhash peer (plist-get lobby :guests))
    (niwa-collaboration-presence-publish lobby)))

(defun niwa-collaboration-lobby--on-state (root state)
  "Track relay transport STATE for ROOT's lobby.
The relay collects the room with the host connection, so every guest
rejoins with a fresh hello after a drop."
  (when-let* ((lobby (gethash root niwa-collaboration-lobby--lobbies)))
    (pcase state
      ('open (niwa-collaboration--restore-push-subscriptions lobby))
      ('down
       (when (> (hash-table-count (plist-get lobby :guests)) 0)
         (plist-put lobby :last-guest-departure (float-time)))
       (clrhash (plist-get lobby :guests))
       (niwa-collaboration-presence-publish lobby)))))


;;
;;; Lifecycle

(defun niwa-collaboration-lobby--find (workspace)
  "Return WORKSPACE's live lobby, or nil."
  (gethash (niwa-workspace-root workspace)
           niwa-collaboration-lobby--lobbies))

(defun niwa-collaboration-lobby-live-p (workspace)
  "Return non-nil while WORKSPACE's lobby is shared.
It is read from memory, so status displays may call it freely."
  (and workspace (niwa-collaboration-lobby--find workspace) t))

(defun niwa-collaboration-lobby--workspace (directory)
  "Return the workspace DIRECTORY belongs to, or signal `user-error'."
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (unless (file-directory-p dir)
      (user-error "%s is not a directory" dir))
    (or (with-temp-buffer
          (setq default-directory dir)
          (niwa-workspace))
        (user-error "No niwa workspace at %s" dir))))

(defun niwa-collaboration-lobby-start (directory)
  "Start the lobby for DIRECTORY's workspace and return it.
Return the live lobby when one is already running.  The returned plist
carries the bearer links under `:link-view', `:link-full' and
`:link-owner'; they stay valid across restarts until the lobby is
rotated.  The lobby is recorded as running, so
`niwa-collaboration-lobby-restore' restarts it in the next Emacs
until it is stopped."
  (require 'niwa-collaboration-files)
  (require 'niwa-collaboration-guest)
  (require 'niwa-collaboration-owner)
  (require 'niwa-collaboration-presence)
  (require 'niwa-collaboration-projection)
  (require 'niwa-collaboration-transport)
  (unless (require 'websocket nil t)
    (user-error "Collaboration requires the 'websocket' package; install it first"))
  (let* ((workspace (niwa-collaboration-lobby--workspace directory))
         (root (niwa-workspace-root workspace)))
    (or (niwa-collaboration-lobby--find workspace)
        (let* ((origins (niwa-collaboration--relay-origins))
               (credentials
                (niwa-collaboration-lobby--credentials workspace))
               (room-id (plist-get credentials :room-id))
               (key (plist-get credentials :key))
               (write-token (plist-get credentials :write-token))
               (owner-token (plist-get credentials :owner-token))
               (project (niwa-collaboration--project-name workspace))
               (lobby
                (append
                 (list :workspace workspace
                       :started-at (float-time)
                       :last-guest-departure nil
                       :directory root
                       :project project
                       :session-label (format "Lobby: %s" project)
                       :room-id room-id
                       :key key
                       :write-token write-token
                       :owner-token owner-token
                       :push-guests (niwa-collaboration-lobby--restore-push workspace room-id)
                       :guests (make-hash-table :test #'eql))
                 (niwa-collaboration--links
                  (cdr origins) room-id key write-token owner-token))))
          (puthash root lobby niwa-collaboration-lobby--lobbies)
          (add-hook 'kill-emacs-hook #'niwa-collaboration-lobby--stop-all)
          (condition-case error-data
              (plist-put
               lobby :transport
               (niwa-collaboration--transport-open
                (format "%s/r/%s?role=host" (car origins) room-id)
                key
                :headers (niwa-collaboration--host-headers)
                :on-frame (lambda (peer frame)
                            (niwa-collaboration-lobby--on-frame
                             root peer frame))
                :on-control (lambda (event peer)
                              (niwa-collaboration-lobby--on-control
                               root event peer))
                :on-state (lambda (state)
                            (niwa-collaboration-lobby--on-state
                             root state))))
            (error
             (remhash root niwa-collaboration-lobby--lobbies)
             (signal (car error-data) (cdr error-data))))
          (niwa-collaboration-lobby--set-intended root t)
          (niwa-sleep-update)
          ;; Owners in the workspace's rooms get their way to it.
          (ignore-errors (niwa-collaboration--announce-lobby workspace))
          lobby))))

(defun niwa-collaboration-lobby--stop (lobby reason)
  "Stop LOBBY for REASON, telling its guests unless Emacs is exiting.
Its credentials stay stored, so starting it again revives its links.
Only an exit leaves it recorded as running, to restart with Emacs."
  (let* ((workspace (plist-get lobby :workspace))
         (root (niwa-workspace-root workspace)))
    (remhash root niwa-collaboration-lobby--lobbies)
    (unless (eq reason 'emacs-exit)
      (niwa-collaboration-lobby--set-intended root nil)
      (ignore-errors (niwa-collaboration--announce-lobby workspace))))
  (when (zerop (hash-table-count niwa-collaboration-lobby--lobbies))
    (remove-hook 'kill-emacs-hook #'niwa-collaboration-lobby--stop-all))
  (ignore-errors (niwa-collaboration-share-dismiss lobby))
  (when-let* ((transport (plist-get lobby :transport)))
    (unless (eq reason 'emacs-exit)
      (ignore-errors
        (when (> (hash-table-count (plist-get lobby :guests)) 0)
          (niwa-collaboration--transport-send
           transport 0 (list :t "bye" :reason (format "%s" reason))))))
    (ignore-errors (niwa-collaboration--transport-stop transport)))
  (unless (eq reason 'emacs-exit)
    (ignore-errors (niwa-collaboration-presence-publish lobby))))

(defun niwa-collaboration-lobby--stop-all ()
  "Stop every lobby before Emacs exits."
  (let (lobbies)
    (maphash (lambda (_root lobby) (push lobby lobbies))
             niwa-collaboration-lobby--lobbies)
    (dolist (lobby lobbies)
      (niwa-collaboration-lobby--stop lobby 'emacs-exit))))

(defun niwa-collaboration-lobby--status ()
  "Return a status line for every live lobby, or nil."
  (let (lines)
    (maphash
     (lambda (_root lobby)
       (push (format "%s: relay %s; %d guest%s; %s"
                     (plist-get lobby :session-label)
                     (if (niwa-collaboration--transport-open-p
                          (plist-get lobby :transport))
                         "connected" "reconnecting")
                     (hash-table-count (plist-get lobby :guests))
                     (if (= 1 (hash-table-count (plist-get lobby :guests)))
                         "" "s")
                     (niwa-collaboration--keep-awake-status
                      lobby (float-time)))
             lines))
     niwa-collaboration-lobby--lobbies)
    (and lines (mapconcat #'identity (nreverse lines) "; "))))

(defun niwa-collaboration-lobby-shares ()
  "Return share entries for every running and recorded lobby.
See `niwa-collaboration-shares' for the entry shape.  A recorded lobby
that is not running, usually because it failed to restart, is
`recorded-lobby'."
  (let (live)
    (maphash
     (lambda (root lobby)
       (push (list :kind 'lobby
                   :label (plist-get lobby :project)
                   :guests (hash-table-count (plist-get lobby :guests))
                   :connected (and (plist-get lobby :transport)
                                   (niwa-collaboration--transport-open-p
                                    (plist-get lobby :transport))
                                   t)
                   :directory root)
             live))
     niwa-collaboration-lobby--lobbies)
    (append
     (nreverse live)
     (mapcar (lambda (root)
               (list :kind 'recorded-lobby
                     :label (file-name-nondirectory (directory-file-name root))
                     :guests 0
                     :connected nil
                     :directory root))
             (seq-remove (lambda (root)
                           (gethash root niwa-collaboration-lobby--lobbies))
                         (niwa-collaboration-lobby--intended))))))

(defun niwa-collaboration-lobby-show-directory (root &optional restart)
  "Show the links of workspace ROOT's running lobby.
With RESTART, start a lobby that is recorded to run but is not running,
as the next Emacs start would; the recording is its earlier consent.
Otherwise a lobby that stopped meanwhile is refused."
  (if-let* ((lobby (gethash root niwa-collaboration-lobby--lobbies)))
      (niwa-collaboration-share-present lobby)
    (unless (and restart
                 (member root (niwa-collaboration-lobby--intended)))
      (niwa-diagnostic-user-error root "The lobby of %s is no longer running" root))
    (niwa-collaboration-share-present
     (niwa-collaboration-lobby-start root))))

(defun niwa-collaboration-lobby-stop-directory (root)
  "Stop workspace ROOT's lobby, or forget it when it is only recorded.
Either way it no longer restarts with Emacs."
  (if-let* ((lobby (gethash root niwa-collaboration-lobby--lobbies)))
      (progn
        (niwa-collaboration-lobby--stop lobby 'user-stop)
        (message "niwa: lobby stopped for %s" (plist-get lobby :project)))
    (niwa-collaboration-lobby--set-intended root nil)
    (message "niwa: lobby of %s no longer restarts" root)))

(defun niwa-collaboration-lobby-stop-every ()
  "Stop every running lobby and forget recorded ones; return how many.
Credentials stay stored, so starting a lobby again revives its links."
  (let ((recorded (niwa-collaboration-lobby--intended))
        lobbies)
    (maphash (lambda (_root lobby) (push lobby lobbies))
             niwa-collaboration-lobby--lobbies)
    (dolist (lobby lobbies)
      (niwa-collaboration-lobby--stop lobby 'user-stop))
    (let ((forgotten (seq-remove
                      (lambda (root)
                        (seq-some (lambda (lobby)
                                    (equal root (plist-get lobby :directory)))
                                  lobbies))
                      recorded)))
      (dolist (root forgotten)
        (niwa-collaboration-lobby--set-intended root nil))
      (+ (length lobbies) (length forgotten)))))


;;
;;; Commands

(defun niwa-collaboration-lobby ()
  "Start the current workspace's lobby, or show its links."
  (interactive)
  (let* ((workspace (niwa-workspace-current))
         (root (niwa-workspace-root workspace)))
    (unless (or (niwa-collaboration-lobby--find workspace)
                (yes-or-no-p
                 (concat
                  "Share this project's session list through the relay? "
                  "Its links stay valid across restarts until rotated, and "
                  "a full or owner link can open sessions and read, upload "
                  "and remove project files; an owner link can also create "
                  "sessions. ")))
      (niwa-diagnostic-user-error root "Lobby not started"))
    (niwa-collaboration-share-present
     (niwa-collaboration-lobby-start root))))

(defun niwa-collaboration-lobby-stop ()
  "Stop the current workspace's lobby until it is started again.
Its links work again once it is."
  (interactive)
  (let ((workspace (niwa-workspace)))
    (if-let* ((lobby (and workspace
                          (niwa-collaboration-lobby--find workspace))))
        (progn
          (niwa-collaboration-lobby--stop lobby 'user-stop)
          (message "niwa: lobby stopped for %s" (plist-get lobby :project)))
      ;; A lobby that failed to restart is still recorded to run.
      (when workspace
        (niwa-collaboration-lobby--set-intended
         (niwa-workspace-root workspace) nil))
      (message "niwa: no lobby is running for this workspace"))))

(defun niwa-collaboration-lobby-rotate ()
  "Replace the current workspace's lobby credentials.
Every earlier lobby link stops working; the lobby restarts when it was
running."
  (interactive)
  (let* ((workspace (niwa-workspace-current))
         (lobby (niwa-collaboration-lobby--find workspace))
         (old (or lobby (niwa-collaboration-lobby--read-credentials
                         (niwa-collaboration-lobby--credentials-path workspace)))))
    (when lobby
      (niwa-collaboration-lobby--stop lobby 'rotated))
    (niwa-collaboration-lobby--credentials workspace t)
    (when-let* ((room-id (plist-get old :room-id))
                (store (niwa-collaboration-lobby--push-path workspace room-id))
                ((file-exists-p store)))
      (delete-file store))
    (if lobby
        (niwa-collaboration-share-present
         (niwa-collaboration-lobby-start
          (niwa-workspace-root workspace)))
      (message "niwa: lobby credentials rotated"))))

(provide 'niwa-collaboration-lobby)
;;; niwa-collaboration-lobby.el ends here
