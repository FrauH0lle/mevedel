;;; mevedel-collaboration-artifact-room.el --- Artifact rooms for remote artifacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns artifact rooms: the origin side of sharing one artifact.  Sharing
;; an artifact gives it its own persistent relay room and two artifact
;; links, a view link and an edit link, that reach that artifact and
;; nothing else of the workspace.  A consumer elsewhere -- the
;; remote-artifact package for Claude Code or Codex, or another mevedel --
;; attaches a link and works on the artifact as a remote artifact; this
;; Emacs stays authoritative and carries out every read and edit itself.
;;
;; An artifact room is the third room kind beside session rooms and the
;; lobby, and runs independently of the lobby.  Its credentials (room id,
;; room key, write token; no owner token) live in the artifact's private
;; bookkeeping, so a duplicate starts unshared and deleting the artifact
;; removes them.  Their existence is what makes the artifact shared.
;;
;; The room answers a hello with its own small welcome naming the room
;; kind and the artifact, never a session snapshot or store listing.
;; Every other frame type is dropped.  Editing requests go through
;; `mevedel-collaboration-artifact-room--actions', the allowlist of what
;; an artifact link may do, each pinned to the room's own artifact: read
;; and view it and its recent contributions, and with an edit link change
;; it with `SharedEdit''s actions, revert included, attributed to the
;; consumer as a remote agent.  Committed changes of the artifact, and of
;; no other, reach its room as change events.
;;
;; An HTML artifact is shared as its directory of files instead.  Any link
;; lists and reads them; an edit link writes, creates and deletes them,
;; each change checked against the SHA-256 the consumer read, so the
;; origin needs no patch engine.  File changes notify the store as a local
;; write does.
;;
;; Consumers never touch versions.  The room groups its writes into
;; working stretches instead and records one version when a stretch
;; ends.  Connected consumers are listed by name on the artifact's page
;; for origin browsers, and are told no presence themselves.
;;
;; `mevedel-collaboration-artifact-room-sharing' is the one host
;; operation to share, revoke or rotate.  Revoking deletes the
;; credentials and tells consumers `revoked'; deleting the artifact tells
;; them `artifact-deleted', and an exiting Emacs `origin-stopped'.  The
;; roots with shared artifacts are recorded per user, and
;; `mevedel-collaboration-artifact-room-restore' shares them again at
;; startup under their stored room ids.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-artifact-backend)
(require 'mevedel-artifact-store)
(require 'mevedel-collaboration)
(require 'mevedel-resource)
(require 'mevedel-utilities)
(require 'mevedel-workspace)

;; `mevedel-collaboration-editing'
(declare-function mevedel-collaboration-editing--send
                  "mevedel-collaboration-editing" (room peer req-id value &optional raw))
(declare-function mevedel-collaboration-editing-handle
                  "mevedel-collaboration-editing" (room peer frame &optional dispatch))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--admit-hello
                  "mevedel-collaboration-guest" (room peer frame))

;; `mevedel-collaboration-presence'
(declare-function mevedel-collaboration-presence-publish
                  "mevedel-collaboration-presence" (room))

;; `mevedel-collaboration-projection'
(defvar mevedel-collaboration--protocol-version)

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-open
                  "mevedel-collaboration-transport" (url key &rest callbacks))
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(declare-function mevedel-collaboration--transport-stop
                  "mevedel-collaboration-transport" (transport))

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing--json "mevedel-shared-editing" (value))
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function mevedel-shared-editing-op-id "mevedel-shared-editing" ())

;; `mevedel-shared-library'
;; Restoring shared artifacts at startup loads this file; the libraries
;; load only when a consumer asks for them.
(declare-function mevedel-shared-library-libraries "mevedel-shared-library" ())
(autoload 'mevedel-shared-library-libraries "mevedel-shared-library")

;; `mevedel-sleep'
(declare-function mevedel-sleep-update "mevedel-sleep" (&rest _))
(autoload 'mevedel-sleep-update "mevedel-sleep")


;;
;;; Credentials

(defun mevedel-collaboration-artifact-room--read-credentials (path)
  "Return the valid artifact room credentials stored at PATH, or nil.
The result is a plist of `:room-id' and the unibyte `:key' and
`:write-token'."
  (when-let* ((stored (mevedel--read-data-file path))
              ((proper-list-p stored))
              (room-id (plist-get stored :room-id))
              ((stringp room-id))
              ((string-match-p "\\`[A-Za-z0-9_-]\\{10,64\\}\\'" room-id))
              (key (mevedel-collaboration--base64url-decode
                    (plist-get stored :key)))
              (write-token (mevedel-collaboration--base64url-decode
                            (plist-get stored :write-token)))
              ((= 32 (length key)))
              ((= 16 (length write-token))))
    (list :room-id room-id :key key :write-token write-token)))

(defun mevedel-collaboration-artifact-room--credentials (workspace id)
  "Return the credentials of WORKSPACE's artifact ID, minting them if needed.
Missing or unreadable credentials are generated and stored readable only
by this user; existing ones are returned unchanged, so sharing again
keeps every link working."
  (let ((path (mevedel-artifact-store-link-path workspace id)))
    (or (mevedel-collaboration-artifact-room--read-credentials path)
        (let ((credentials
               (list :room-id (mevedel-collaboration--base64url
                               (mevedel-collaboration--random-bytes 16))
                     :key (mevedel-collaboration--random-bytes 32)
                     :write-token (mevedel-collaboration--random-bytes 16))))
          (mevedel--write-private-data-file
           path (list :room-id (plist-get credentials :room-id)
                      :key (mevedel-collaboration--base64url (plist-get credentials :key))
                      :write-token (mevedel-collaboration--base64url
                                    (plist-get credentials :write-token))))
          credentials))))

(defun mevedel-collaboration-artifact-room--links (web-origin credentials)
  "Return the view and edit links of CREDENTIALS at WEB-ORIGIN.
The result is a plist of `:link-view' and `:link-edit'.  The edit
link's secret is the view link's followed by the write token."
  (let ((link (lambda (secret)
                (format "%s/#%s.%s" web-origin (plist-get credentials :room-id)
                        (mevedel-collaboration--base64url secret)))))
    (list :link-view (funcall link (plist-get credentials :key))
          :link-edit (funcall link (concat (plist-get credentials :key)
                                           (plist-get credentials :write-token))))))


;;
;;; Running across restarts

(defun mevedel-collaboration-artifact-room--record-path ()
  "Return the per-user file listing the roots with shared artifacts."
  (file-name-concat mevedel-user-dir "artifact-rooms.eld"))

(defun mevedel-collaboration-artifact-room--recorded ()
  "Return the workspace roots recorded to have shared artifacts.
An unreadable or malformed record yields nil."
  (let ((stored (mevedel--read-data-file
                 (mevedel-collaboration-artifact-room--record-path))))
    (when (proper-list-p stored)
      (seq-filter #'stringp stored))))

(defun mevedel-collaboration-artifact-room--set-recorded (root shared)
  "Record whether workspace ROOT has SHARED artifacts.
A recorded root's shared artifacts are shared again when mevedel next
starts.  A record that cannot be written is reported, since sharing
itself is unaffected."
  (let* ((roots (mevedel-collaboration-artifact-room--recorded))
         (updated (if shared
                      (if (member root roots) roots (append roots (list root)))
                    (remove root roots)))
         (path (mevedel-collaboration-artifact-room--record-path)))
    (unless (equal updated roots)
      (condition-case err
          (if (null updated)
              (delete-file path)
            (mevedel--write-private-data-file path updated))
        (error
         (display-warning
          'mevedel
          (format "Could not record the shared artifacts of %s for the next start: %s"
                  root (error-message-string err))))))))

(defun mevedel-collaboration-artifact-room--shared-ids (workspace)
  "Return the ids of WORKSPACE's artifacts that hold valid credentials."
  (let ((state (file-name-concat (mevedel-artifact-store-directory workspace) ".state")))
    (when (file-directory-p state)
      (seq-filter
       (lambda (id)
         (and (mevedel-artifact-store-id-p id)
              (mevedel-artifact-store-meta workspace id)
              (mevedel-collaboration-artifact-room--read-credentials
               (mevedel-artifact-store-link-path workspace id))))
       (directory-files state nil "\\`[A-Za-z0-9_-]+\\'")))))

(defun mevedel-collaboration-artifact-room--update-record (workspace)
  "Record WORKSPACE's root while it has shared artifacts, and drop it after."
  (mevedel-collaboration-artifact-room--set-recorded
   (mevedel-workspace-root workspace)
   (and (mevedel-collaboration-artifact-room--shared-ids workspace) t)))

(defun mevedel-collaboration-artifact-room-restore ()
  "Share again the artifacts that were shared when Emacs last exited.
Each recorded workspace root's shared artifacts get their rooms back
under their stored room ids, so every link handed out keeps working.  A
root that has no shared artifacts left, or whose directory is gone, is
dropped.  A room that cannot start is reported as a warning and its root
stays recorded for the next start."
  (dolist (root (mevedel-collaboration-artifact-room--recorded))
    (condition-case err
        (let ((inhibit-interaction t))
          (if (not (file-directory-p root))
              (progn
                (mevedel-collaboration-artifact-room--set-recorded root nil)
                (display-warning
                 'mevedel (format "Shared artifacts of %s forgotten: the directory is gone"
                                  root)))
            (let* ((workspace (mevedel-collaboration-artifact-room--workspace root))
                   (ids (mevedel-collaboration-artifact-room--shared-ids workspace)))
              (unless ids
                (mevedel-collaboration-artifact-room--set-recorded root nil))
              (dolist (id ids)
                (condition-case err
                    (mevedel-collaboration-artifact-room--start workspace id)
                  (error
                   (display-warning
                    'mevedel
                    (format (concat "Shared artifact %s of %s not restarted: %s "
                                    "(revoking it ends the retries)")
                            id root (error-message-string err)))))))))
      (error
       (display-warning
        'mevedel
        (format "Shared artifacts of %s not restarted: %s"
                root (error-message-string err)))))))


;;
;;; Working stretches

(defvar mevedel-collaboration-artifact-room-version-idle 300
  "Seconds without writes that end an artifact room's working stretch.
A consumer has no turn the origin sees, so its writes are grouped into
stretches instead: one version records each stretch's end.")

(defun mevedel-collaboration-artifact-room-wrote (room)
  "Note that a consumer of artifact ROOM changed its artifact.
Call it after every committed write from the room, of any kind: a
whiteboard or document edit, or an HTML file write.  The room's working
stretch then lasts until it has written nothing for
`mevedel-collaboration-artifact-room-version-idle' seconds, or its last
consumer leaves, and records one version when it ends."
  (when-let* ((timer (plist-get room :version-timer)))
    (cancel-timer timer))
  (plist-put room :version-timer
             (run-at-time mevedel-collaboration-artifact-room-version-idle nil
                          #'mevedel-collaboration-artifact-room--settle room)))

(defun mevedel-collaboration-artifact-room--settle (room)
  "End ROOM's working stretch, recording one version if it wrote.
The version is the store's own: a whiteboard or document records its
state, an HTML artifact its primary file, as after a local turn.  An
artifact deleted meanwhile records nothing."
  (when-let* ((timer (plist-get room :version-timer)))
    (cancel-timer timer)
    (plist-put room :version-timer nil)
    (let ((workspace (plist-get room :workspace))
          (id (plist-get room :artifact)))
      (when (mevedel-artifact-store-meta workspace id)
        (condition-case err
            (mevedel-artifact-store-save-version workspace id)
          (error (message "mevedel: no version of %s was saved: %s"
                          id (error-message-string err))))))))


;;
;;; Consumer frames

(defun mevedel-collaboration-artifact-room--find (root id)
  "Return the live room of artifact ID in the workspace at ROOT, or nil."
  (gethash (cons root id) mevedel-collaboration-artifact-room--rooms))

(defcustom mevedel-collaboration-artifact-room-origin-name nil
  "Name the consumers of this Emacs's artifact links know it by.
nil omits the origin name entirely; consumers use the relay host instead.
Only an explicitly configured name is disclosed, never a generated
workspace directory or machine name."
  :type '(choice (const :tag "Do not disclose an origin name" nil) string)
  :group 'mevedel)

(defun mevedel-collaboration-artifact-room--welcome (room guest)
  "Return ROOM's artifact welcome for GUEST.
It names the room kind and the artifact as it is now, and includes an
origin name only when explicitly configured."
  (let ((meta (mevedel-artifact-store-meta (plist-get room :workspace)
                                         (plist-get room :artifact))))
    (append (list :t "welcome"
                  :proto mevedel-collaboration--protocol-version
                  :room "artifact"
                  :id (plist-get room :artifact)
                  :kind (symbol-name (plist-get room :artifact-kind))
                  :title (plist-get meta :title))
            (when mevedel-collaboration-artifact-room-origin-name
              (list :origin mevedel-collaboration-artifact-room-origin-name))
            (list :readOnly (if (plist-get guest :writable) :json-false t)))))

(defun mevedel-collaboration-artifact-room--hello (room peer frame)
  "Admit PEER to ROOM from its hello FRAME and send it the welcome.
The consumer is on the artifact's page from then on, so origin browsers
showing the artifact list it; whatever page FRAME reports is ignored.
The first consumer of an HTML room starts from its files as they are, so
only later changes are announced."
  (let ((first (zerop (hash-table-count (plist-get room :guests)))))
    (when-let* ((guest (mevedel-collaboration--admit-hello room peer frame)))
      (when (and first (eq 'html (plist-get room :artifact-kind)))
        (plist-put room :files-fingerprint
                   (mevedel-collaboration-artifact-room--files-fingerprint room)))
      (plist-put guest :viewing (plist-get room :artifact))
      (plist-put guest :active t)
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (mevedel-collaboration-artifact-room--welcome room guest))
      (mevedel-collaboration-presence-publish room))))

(defun mevedel-collaboration-artifact-room--read (room _guest args authorize reply)
  "Read ROOM's whiteboard or document as `shared://ID' shows it.
With a true `:history' in ARGS, read its recent contributions instead,
as `shared://ID/history' shows them, each with the id a revert names.
An HTML artifact's file is read instead, as
`mevedel-collaboration-artifact-room--read-file' does.  AUTHORIZE must
still permit the read; REPLY receives (:result (:text TEXT)) or (:error
MESSAGE)."
  (cond
   ((eq (plist-get room :artifact-kind) 'html)
    (mevedel-collaboration-artifact-room--read-file room args reply))
   ((plist-get args :file)
    (error "Only HTML artifacts have files to read"))
   (t
    (mevedel-shared-editing-call
     (plist-get room :workspace)
     (list :action "view" :id (plist-get room :artifact)
           :part (if (eq t (plist-get args :history)) "history" "overview"))
     (lambda (value)
       (funcall reply
                (if-let* ((failure (plist-get value :error)))
                    (list :error failure)
                  (list :result (list :text (plist-get (plist-get value :result)
                                                       :text))))))
     authorize))))


;;
;;; HTML files

(defconst mevedel-collaboration-artifact-room--stale-content-bytes (* 256 1024)
  "Largest current file a refused write sends back with its hash.")

(defun mevedel-collaboration-artifact-room--file-fields (file bytes)
  "Return the reply fields of FILE holding the unibyte string BYTES.
They name the file and give its SHA-256 `:hash' and size `:bytes', and
its content as `:text' when BYTES are UTF-8 text, else as base64 `:data'."
  (let ((text (decode-coding-string bytes 'utf-8-unix)))
    (append (list :file file :hash (secure-hash 'sha256 bytes) :bytes (length bytes))
            (if (string-match-p (rx (any 0 (#x3fff80 . #x3fffff))) text)
                (list :data (base64-encode-string bytes t))
              (list :text text)))))

(defun mevedel-collaboration-artifact-room--file (room args)
  "Return the file ARGS name in ROOM's HTML artifact, its primary file by default."
  (let ((file (plist-get args :file)))
    (cond
     ((null file)
      (plist-get (mevedel-artifact-store-meta (plist-get room :workspace)
                                              (plist-get room :artifact))
                 :file))
     ((stringp file) file)
     (t (error "A file is named by its path in the artifact")))))

(defun mevedel-collaboration-artifact-room--read-file (room args reply)
  "Answer REPLY with the file ARGS name in ROOM's HTML artifact.
Any file in the artifact's directory may be read; without `:file' the
artifact's primary file is.  The result carries the file's hash, which
a write names to replace it."
  (let* ((file (mevedel-collaboration-artifact-room--file room args))
         (bytes (mevedel-artifact-store-read-file
                 (plist-get room :workspace) (plist-get room :artifact) file)))
    (funcall reply
             (if bytes
                 (list :result (mevedel-collaboration-artifact-room--file-fields file bytes))
               (list :error (format "No file %s in this artifact" file))))))

(defun mevedel-collaboration-artifact-room--files (room _guest _args _authorize reply)
  "Answer REPLY with every file of ROOM's HTML artifact and its size.
The result names the `:primary' file and lists `:files' as (:file NAME
:bytes SIZE)."
  (let ((workspace (plist-get room :workspace))
        (id (plist-get room :artifact)))
    (funcall reply
             (list :result
                   (list :primary (plist-get (mevedel-artifact-store-meta workspace id) :file)
                         :files (vconcat (mevedel-artifact-store-files workspace id)))))))

(defun mevedel-collaboration-artifact-room--stale (room file current expected)
  "Return the refusal of a change to FILE of ROOM's artifact.
CURRENT is the file's hash now and EXPECTED the one the change named,
each nil for an absent file.  A file under
`mevedel-collaboration-artifact-room--stale-content-bytes' comes with
its content, so the consumer can redo its change without reading again."
  (let ((bytes (and current
                    (mevedel-artifact-store-read-file
                     (plist-get room :workspace) (plist-get room :artifact) file))))
    (append
     (list :error (cond
                   ((null bytes)
                    (format "%s does not exist; write it without a hash to create it" file))
                   ((null expected)
                    (format "%s already exists; redo the change on its current content" file))
                   (t (format "%s changed since it was read; redo the change on its current content"
                              file)))
           :code "stale")
     (cond
      ((null bytes) (list :file file :hash nil))
      ((< (length bytes) mevedel-collaboration-artifact-room--stale-content-bytes)
       (mevedel-collaboration-artifact-room--file-fields file bytes))
      (t (list :file file :hash (secure-hash 'sha256 bytes) :bytes (length bytes)))))))

(defun mevedel-collaboration-artifact-room--changed-file (room file)
  "Note a consumer's committed change to FILE of ROOM's artifact.
A change of the primary file is part of the room's working stretch, as
only the primary file is versioned."
  (when (equal file (plist-get (mevedel-artifact-store-meta (plist-get room :workspace)
                                                           (plist-get room :artifact))
                               :file))
    (mevedel-collaboration-artifact-room-wrote room)))

(defun mevedel-collaboration-artifact-room--content (args)
  "Return the bytes ARGS give a file as `:text' or base64 `:data'."
  (let ((text (plist-get args :text))
        (data (plist-get args :data)))
    (cond
     ((and (stringp text) (null data)) (encode-coding-string text 'utf-8-unix))
     ((and (stringp data) (null text))
      (condition-case nil (base64-decode-string data)
        (error (error "The data must be the file's bytes as base64"))))
     (t (error "Give the file's content as text or as base64 data")))))

(defun mevedel-collaboration-artifact-room--write-file (room _guest args _authorize reply)
  "Write the file ARGS name in ROOM's HTML artifact, if its hash still matches.
ARGS carry the relative `:file', its content as `:text' or base64
`:data', and the `:hash' it was read with, nil to create it.  REPLY
receives (:result (:file :hash :bytes :created)) or a stale refusal
with the current hash and, for a small file, its content.  The store's
observers hear of the change as of a local write."
  (let* ((file (plist-get args :file))
         (content (mevedel-collaboration-artifact-room--content args)))
    (unless (stringp file)
      (error "A file is named by its path in the artifact"))
    (let ((outcome (mevedel-artifact-store-write-file
                    (plist-get room :workspace) (plist-get room :artifact)
                    file content (plist-get args :hash))))
      (if (plist-get outcome :stale)
          (funcall reply (mevedel-collaboration-artifact-room--stale
                          room file (plist-get outcome :hash) (plist-get args :hash)))
        (mevedel-collaboration-artifact-room--changed-file room file)
        (funcall reply
                 (list :result (list :file file :hash (plist-get outcome :hash)
                                     :bytes (length content)
                                     :created (if (plist-get outcome :created)
                                                  t :json-false))))))))

(defun mevedel-collaboration-artifact-room--delete-file (room _guest args _authorize reply)
  "Delete the file ARGS name in ROOM's HTML artifact, if its hash still matches.
ARGS carry the relative `:file' and the `:hash' it was read with.  The
primary file is refused: without it the artifact is gone, and a link
never deletes its artifact.  REPLY receives (:result (:file :deleted t))
or a stale refusal as a write gets."
  (let* ((workspace (plist-get room :workspace))
         (id (plist-get room :artifact))
         (file (plist-get args :file)))
    (unless (stringp file)
      (error "A file is named by its path in the artifact"))
    ;; The guard first, so a refused path is reported as such.
    (mevedel-artifact-store-file-path workspace id file)
    (when (equal file (plist-get (mevedel-artifact-store-meta workspace id) :file))
      (error "%s is the artifact's primary file; an artifact link cannot delete it" file))
    (let ((outcome (mevedel-artifact-store-delete-file
                    workspace id file (plist-get args :hash))))
      (funcall reply
               (if (plist-get outcome :stale)
                   (mevedel-collaboration-artifact-room--stale
                    room file (plist-get outcome :hash) (plist-get args :hash))
                 (list :result (list :file file :deleted t)))))))

(defun mevedel-collaboration-artifact-room--write-files (room _guest args _authorize reply)
  "Change several files of ROOM's HTML artifact if every hash still matches.
ARGS carry `:changes', each naming a relative `:file' and the `:hash' it
was read with, nil to create it, and either its content as `:text' or
base64 `:data', or `:delete' t.  Every hash is checked before any file
changes, under the store's lock, so all changes are made or none: a
patch of several files lands whole.  The primary file is never deleted.
REPLY receives (:result (:files RESULTS)), RESULTS in the order of the
changes as `write-file' and `delete-file' answer them, or the stale
refusal of the first file that changed since it was read."
  (let* ((workspace (plist-get room :workspace))
         (id (plist-get room :artifact))
         (primary (plist-get (mevedel-artifact-store-meta workspace id) :file))
         (changes (plist-get args :changes)))
    (unless (and (vectorp changes) (> (length changes) 0))
      (error "'write-files' needs changes: the files to write or delete"))
    (let* ((requested
            (mapcar (lambda (change)
                      (let ((file (plist-get change :file))
                            (hash (plist-get change :hash)))
                        (unless (stringp file)
                          (error "A file is named by its path in the artifact"))
                        ;; The guard first, so a refused path is reported as such.
                        (mevedel-artifact-store-file-path workspace id file)
                        (if (eq t (plist-get change :delete))
                            (progn
                              (when (equal file primary)
                                (error "%s is the artifact's primary file; an artifact link cannot delete it"
                                       file))
                              (list :file file :hash hash :delete t))
                          (list :file file :hash hash
                                :content (mevedel-collaboration-artifact-room--content change)))))
                    changes))
           (outcome (mevedel-artifact-store-write-files workspace id requested)))
      (if-let* ((stale (and (plist-get outcome :stale) (plist-get outcome :file))))
          (funcall reply (mevedel-collaboration-artifact-room--stale
                          room stale (plist-get outcome :hash)
                          (plist-get (cl-find stale requested
                                              :key (lambda (change) (plist-get change :file))
                                              :test #'equal)
                                     :hash)))
        (dolist (change requested)
          (mevedel-collaboration-artifact-room--changed-file room (plist-get change :file)))
        (funcall reply
                 (list :result
                       (list :files
                             (vconcat
                              (cl-mapcar
                               (lambda (change result)
                                 (if (plist-get change :delete)
                                     (list :file (plist-get change :file) :deleted t)
                                   (list :file (plist-get change :file)
                                         :hash (plist-get result :hash)
                                         :bytes (length (plist-get change :content))
                                         :created (if (plist-get result :created) t :json-false))))
                               requested (plist-get outcome :files))))))))))

(defconst mevedel-collaboration-artifact-room--image-max-bytes 4700000
  "Largest image an artifact link may upload, as SharedEdit's limit.")

(defun mevedel-collaboration-artifact-room--edit-reply (value)
  "Return the consumer's reply to the editing helper's VALUE.
It is what SharedEdit tells the model: the stored lines of what changed
as JSON `:text', and for a whiteboard the picture as base64 `:png'.  A
stale edit returns the stale targets with their current lines."
  (if-let* ((failure (plist-get value :error)))
      (if (equal (plist-get value :code) "stale")
          (list :error failure :code "stale" :targets (plist-get value :targets)
                :revision (plist-get value :revision))
        (list :error failure))
    (let ((result (plist-get value :result)))
      (list :result
            (append (list :text (mevedel-shared-editing--json (plist-get result :model)))
                    (when-let* ((png (plist-get result :png)))
                      (list :png png)))))))

(defun mevedel-collaboration-artifact-room--library-item (item)
  "Return the `:library' and `:item' keys for library item reference ITEM.
ITEM is LIBRARY/ITEM-ID, as `library' lists it; the library text is the
origin user's, resolved here since the consumer cannot read it."
  (let ((slash (and (stringp item) (string-search "/" item))))
    (unless slash
      (error "insert needs item as LIBRARY/ITEM-ID from the library listing"))
    (let ((library (cl-find (substring item 0 slash) (mevedel-shared-library-libraries)
                            :key (lambda (l) (plist-get l :name)) :test #'equal)))
      (unless library
        (error "No element library %s; list the libraries for installed ones"
               (substring item 0 slash)))
      (list :library (plist-get library :text) :item (substring item (1+ slash))))))

(defun mevedel-collaboration-artifact-room--edit (room guest args authorize reply)
  "Run GUEST's SharedEdit action ARGS on ROOM's artifact.
Only the keys `mevedel-artifact-backend-edit-keys' names
for the action pass, with the room's artifact id and the actor
\"Agent: NAME (remote)\", NAME being the one GUEST declared in its
hello.  AUTHORIZE and REPLY are as in
`mevedel-collaboration-artifact-room--actions'.  The item is attached
to no session."
  (let* ((action (plist-get args :action))
         (request (list :action action :id (plist-get room :artifact) :image t
                        :actor (format "Agent: %s (remote)" (plist-get guest :name))
                        :opId (mevedel-shared-editing-op-id))))
    (dolist (key (cdr (assoc action mevedel-artifact-backend-edit-keys)))
      (when (plist-member args key)
        (setq request (plist-put request key (plist-get args key)))))
    (when (equal action "insert")
      (let ((resolved (mevedel-collaboration-artifact-room--library-item
                       (plist-get args :item))))
        (setq request (plist-put request :library (plist-get resolved :library))
              request (plist-put request :item (plist-get resolved :item)))))
    (when (equal action "image")
      (let ((data (plist-get args :data)))
        (unless (and (stringp data) (not (string-empty-p data)))
          (error "The image action needs data: the image file as base64"))
        (when (> (length data)
                 (* 4 (ceiling mevedel-collaboration-artifact-room--image-max-bytes 3)))
          (error "Shared images are limited to %d bytes"
                 mevedel-collaboration-artifact-room--image-max-bytes))))
    (mevedel-shared-editing-call
     (plist-get room :workspace) request
     (lambda (value)
       (unless (plist-get value :error)
         (mevedel-collaboration-artifact-room-wrote room))
       (funcall reply (mevedel-collaboration-artifact-room--edit-reply value)))
     authorize)))

(defun mevedel-collaboration-artifact-room--view (room _guest args authorize reply)
  "Show ROOM's artifact as a picture, as `shared://ID/view.png' does.
With `:image' in ARGS, show that embedded image instead, as
`shared://ID/images/KEY' does.  AUTHORIZE and REPLY are as in
`mevedel-collaboration-artifact-room--actions'; REPLY receives
\(:result (:png BASE64 :mime MIME)) or (:error MESSAGE)."
  (let ((image (plist-get args :image)))
    (unless (or (null image) (stringp image))
      (error "The image to view is named by its key"))
    (mevedel-shared-editing-call
     (plist-get room :workspace)
     (append (list :action "view" :id (plist-get room :artifact)
                   :part (if image "image" "png"))
             (when image (list :image image)))
     (lambda (value)
       (funcall reply
                (if-let* ((failure (plist-get value :error)))
                    (list :error failure)
                  (let ((result (plist-get value :result)))
                    (list :result (list :png (plist-get result :png)
                                        :mime (or (plist-get result :mime) "image/png")))))))
     authorize)))

(defun mevedel-collaboration-artifact-room--library (room _guest args authorize reply)
  "Show the origin user's whiteboard libraries to ROOM's guest.
They are shown as `shared://library' shows them.  ARGS may name one
`:library'; with `:part' \"sheet\" the reply is the picture of the
library (or of all of them) as `:png' and `:mime', with \"item\" the
picture of the one `:item' LIBRARY/ITEM-ID names, else the listing as
`:text'.  Only reading is offered: the libraries are never changed.
AUTHORIZE and REPLY are as in
`mevedel-collaboration-artifact-room--actions'."
  (let* ((part (plist-get args :part))
         (item (plist-get args :item))
         (name (if (equal part "item")
                   (if-let* (((stringp item))
                             (slash (string-search "/" item)))
                       (substring item 0 slash)
                     (error "An item preview needs item as LIBRARY/ITEM-ID from the library listing"))
                 (plist-get args :library)))
         (libraries (cl-remove-if-not
                     (lambda (library) (or (null name) (equal name (plist-get library :name))))
                     (mevedel-shared-library-libraries))))
    (unless (or (null name) (stringp name))
      (error "A library is named by a string"))
    (unless (member part '(nil "list" "sheet" "item"))
      (error "The library part is list, sheet or item"))
    (when (and name (null libraries))
      (error "No element library %s; list the libraries for installed ones" name))
    (mevedel-shared-editing-call
     (plist-get room :workspace)
     (append
      (list :action "library-view" :part (if (member part '("sheet" "item")) part "list")
            :at (if name
                    (concat "shared://library/" (mevedel-resource-encode-component name))
                  "shared://library")
            :libraries (vconcat (mapcar (lambda (library)
                                          (list :name (plist-get library :name)
                                                :text (plist-get library :text)))
                                        libraries)))
      (when (equal part "item") (list :item item)))
     (lambda (value)
       (funcall reply
                (if-let* ((failure (plist-get value :error)))
                    (list :error failure)
                  (let ((result (plist-get value :result)))
                    (list :result
                          (if (plist-get result :text)
                              (list :text (plist-get result :text))
                            (list :png (plist-get result :png)
                                  :mime (or (plist-get result :mime) "image/png"))))))))
     authorize)))

(defconst mevedel-collaboration-artifact-room--actions
  '(("read" :kinds (whiteboard document html)
     :run mevedel-collaboration-artifact-room--read)
    ("files" :kinds (html)
     :run mevedel-collaboration-artifact-room--files)
    ("write-file" :kinds (html) :write t
     :run mevedel-collaboration-artifact-room--write-file)
    ("delete-file" :kinds (html) :write t
     :run mevedel-collaboration-artifact-room--delete-file)
    ("write-files" :kinds (html) :write t
     :run mevedel-collaboration-artifact-room--write-files)
    ("view" :kinds (whiteboard document)
     :run mevedel-collaboration-artifact-room--view)
    ("patch" :kinds (whiteboard document) :write t
     :run mevedel-collaboration-artifact-room--edit)
    ("rename" :kinds (whiteboard document) :write t
     :run mevedel-collaboration-artifact-room--edit)
    ("background" :kinds (whiteboard document) :write t
     :run mevedel-collaboration-artifact-room--edit)
    ("image" :kinds (whiteboard document) :write t
     :run mevedel-collaboration-artifact-room--edit)
    ("library" :kinds (whiteboard)
     :run mevedel-collaboration-artifact-room--library)
    ("insert" :kinds (whiteboard) :write t
     :run mevedel-collaboration-artifact-room--edit)
    ("revert" :kinds (whiteboard document) :write t
     :run mevedel-collaboration-artifact-room--edit))
  "Editing actions an artifact link may request, by action name.
Each entry is (ACTION . PROPERTIES):

  :kinds  artifact kinds the action applies to.
  :write  non-nil when the action needs an edit link.
  :run    called with the room, the guest, the request args (`:id'
          pinned to the room's artifact), an AUTHORIZE function and a
          REPLY function.  AUTHORIZE takes no arguments and says whether
          the request may still proceed; pass it on to the editing queue
          so it also holds at admission and commit.  REPLY takes
          \(:result VALUE) or (:error MESSAGE) and answers the consumer.

Every action not listed is refused, as is any other artifact id.")

(defun mevedel-collaboration-artifact-room--dispatch (room peer guest req-id args)
  "Run the allowlisted editing ARGS of GUEST's request REQ-ID in ROOM.
PEER is GUEST's relay peer.  The request is refused unless its action
is in `mevedel-collaboration-artifact-room--actions' for the room's
artifact kind, it names no artifact but the room's own, and GUEST may
still act: admitted, holding an edit link for a writing action, in a
room that is still running.  The id is checked once: every action runs
on the room's own artifact, pinned below, so it cannot change later."
  (let* ((action (plist-get args :action))
         (entry (cdr (assoc action mevedel-collaboration-artifact-room--actions)))
         (id (plist-get args :id))
         (key (cons (plist-get room :directory) (plist-get room :artifact)))
         (authorize
          (lambda ()
            (and (eq guest (mevedel-collaboration--guest room peer))
                 (or (not (plist-get entry :write)) (plist-get guest :writable))
                 (eq room (gethash key mevedel-collaboration-artifact-room--rooms)))))
         (reply (lambda (value)
                  (when (eq guest (mevedel-collaboration--guest room peer))
                    (mevedel-collaboration-editing--send room peer req-id value t)))))
    (cond
     ((not (and entry (memq (plist-get room :artifact-kind) (plist-get entry :kinds))))
      (error "An artifact link does not permit %s" (if (stringp action) action "that")))
     ((and id (not (equal id (plist-get room :artifact))))
      (error "This link reaches only its own artifact"))
     ((and (plist-get entry :write) (not (plist-get guest :writable)))
      (error "This artifact link is view-only"))
     ((not (funcall authorize))
      (error "This artifact link no longer permits that"))
     (t
      (funcall (plist-get entry :run) room guest
               (plist-put (copy-sequence args) :id (plist-get room :artifact))
               authorize reply)))))

(defun mevedel-collaboration-artifact-room--editing (room peer frame)
  "Assemble PEER's bounded editing FRAME in ROOM for the allowlist.
A refused request is answered with its error."
  (mevedel-collaboration-editing-handle
   room peer frame #'mevedel-collaboration-artifact-room--dispatch))

(defconst mevedel-collaboration-artifact-room--frames
  '(("hello" . mevedel-collaboration-artifact-room--hello)
    ("editing" . mevedel-collaboration-artifact-room--editing))
  "Frame types an artifact room accepts, with their handlers.
Each handler is called with the room, the sending peer and the frame.
Every other frame type -- prompts, aborts, session and store actions,
files, discussions, recovery -- is dropped unanswered.")

(defun mevedel-collaboration-artifact-room--on-frame (root id peer frame)
  "Dispatch consumer FRAME from PEER in the room of ROOT's artifact ID.
A fault in one frame is reported and dropped; it never ends the room,
whose links are meant to keep working."
  (when-let* ((room (mevedel-collaboration-artifact-room--find root id))
              (handler (cdr (assoc (plist-get frame :t)
                                   mevedel-collaboration-artifact-room--frames))))
    (condition-case err
        (let ((inhibit-interaction t))
          (funcall handler room peer frame))
      (error
       (message "mevedel: artifact room frame failed: %s"
                (error-message-string err))))))

(defun mevedel-collaboration-artifact-room--on-control (root id event peer)
  "Forget PEER in the room of ROOT's artifact ID when EVENT says it left.
The last consumer leaving ends the room's working stretch."
  (when-let* ((room (mevedel-collaboration-artifact-room--find root id))
              ((eq event 'peer-left))
              ((gethash peer (plist-get room :guests))))
    (remhash peer (plist-get room :guests))
    (plist-put room :last-guest-departure (float-time))
    (when (zerop (hash-table-count (plist-get room :guests)))
      (mevedel-collaboration-artifact-room--settle room))
    (mevedel-collaboration-presence-publish room)
    (mevedel-sleep-update)))

(defun mevedel-collaboration-artifact-room--changed (workspace state _result)
  "Tell the consumers of WORKSPACE's artifact room for STATE's item it changed.
Only the room of that very artifact hears of it, as a `changed' event
naming its id, kind, title and revision; consumers read the content
again when they need it.  Rooms of other artifacts hear nothing, and
a deletion is left to the room's own lifecycle."
  (when-let* (((not (plist-get state :deleted)))
              (room (mevedel-collaboration-artifact-room--find
                     (mevedel-workspace-root workspace) (plist-get state :id)))
              ((> (hash-table-count (plist-get room :guests)) 0)))
    (mevedel-collaboration-editing--send
     room 0 "event"
     (list :event "changed" :id (plist-get state :id) :kind (plist-get state :kind)
           :title (plist-get state :title) :revision (plist-get state :revision))
     t)))

(add-hook 'mevedel-artifact-backend-change-hook
          #'mevedel-collaboration-artifact-room--changed)

(defun mevedel-collaboration-artifact-room--files-fingerprint (room)
  "Return a fingerprint of the files of ROOM's HTML artifact.
It changes when a file is added, removed, resized or modified, and is
nil while the artifact's directory is gone."
  (let ((directory (mevedel-artifact-store-artifact-directory
                    (plist-get room :workspace) (plist-get room :artifact))))
    (when (file-directory-p directory)
      (secure-hash
       'sha1
       (prin1-to-string
        (mapcar (lambda (path)
                  (let ((attributes (file-attributes path)))
                    (list path (file-attribute-size attributes)
                          (file-attribute-modification-time attributes))))
                (directory-files-recursively directory "")))))))

(defun mevedel-collaboration-artifact-room--files-changed (workspace)
  "Tell the consumers of WORKSPACE's HTML artifact rooms whose files changed.
The store announces a change of WORKSPACE, not which artifact changed,
so each HTML room with connected consumers compares its files with what
it saw last and sends a `changed' event naming its artifact when they
differ, with that fingerprint as its revision.  Rooms of other artifacts
hear nothing."
  (let ((root (mevedel-workspace-root workspace)))
    (maphash
     (lambda (key room)
       (when-let* (((equal root (car key)))
                   ((eq 'html (plist-get room :artifact-kind)))
                   ((> (hash-table-count (plist-get room :guests)) 0))
                   (fingerprint (mevedel-collaboration-artifact-room--files-fingerprint room))
                   ((not (equal fingerprint (plist-get room :files-fingerprint)))))
         (plist-put room :files-fingerprint fingerprint)
         (mevedel-collaboration-editing--send
          room 0 "event"
          (list :event "changed" :id (plist-get room :artifact) :kind "html"
                :title (plist-get (mevedel-artifact-store-meta workspace (plist-get room :artifact))
                                  :title)
                :revision fingerprint)
          t)))
     mevedel-collaboration-artifact-room--rooms)))

(add-hook 'mevedel-artifact-store-changed-functions
          #'mevedel-collaboration-artifact-room--files-changed)

(defun mevedel-collaboration-artifact-room--on-state (root id state)
  "Track relay transport STATE for the room of ROOT's artifact ID.
The relay collects the room with the host connection, so every
consumer rejoins with a fresh hello after a drop; the drop ends the
room's working stretch."
  (when-let* ((room (mevedel-collaboration-artifact-room--find root id))
              ((eq state 'down))
              ((> (hash-table-count (plist-get room :guests)) 0)))
    (clrhash (plist-get room :guests))
    (plist-put room :last-guest-departure (float-time))
    (mevedel-collaboration-artifact-room--settle room)
    (mevedel-collaboration-presence-publish room)
    (mevedel-sleep-update)))


;;
;;; Lifecycle

(defun mevedel-collaboration-artifact-room--workspace (directory)
  "Return the workspace DIRECTORY belongs to, or signal `user-error'."
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (unless (file-directory-p dir)
      (user-error "%s is not a directory" dir))
    (or (with-temp-buffer
          (setq default-directory dir)
          (mevedel-workspace))
        (user-error "No mevedel workspace at %s" dir))))

(defconst mevedel-collaboration-artifact-room--kinds '(whiteboard document html)
  "Kinds of artifact that can be shared through an artifact link.")

(defun mevedel-collaboration-artifact-room--meta (workspace id)
  "Return the metadata of WORKSPACE's shareable artifact ID, or signal.
Whiteboards, documents and HTML artifacts can be shared."
  (let ((meta (and (mevedel-artifact-store-id-p id)
                   (mevedel-artifact-store-meta workspace id))))
    (unless meta
      (user-error "No artifact %s in this workspace" id))
    (unless (memq (plist-get meta :kind) mevedel-collaboration-artifact-room--kinds)
      (user-error "Only whiteboards, documents and HTML artifacts can be shared through an artifact link"))
    meta))

(defun mevedel-collaboration-artifact-room--start (workspace id)
  "Start the room of WORKSPACE's artifact ID and return it.
Return the live room when one is already running."
  (require 'mevedel-collaboration-guest)
  (require 'mevedel-collaboration-editing)
  (require 'mevedel-collaboration-presence)
  (require 'mevedel-collaboration-projection)
  (require 'mevedel-collaboration-transport)
  (unless (require 'websocket nil t)
    (user-error "Collaboration requires the 'websocket' package; install it first"))
  (let* ((root (mevedel-workspace-root workspace))
         (key (cons root id)))
    (or (mevedel-collaboration-artifact-room--find root id)
        (let* ((meta (mevedel-collaboration-artifact-room--meta workspace id))
               (origins (mevedel-collaboration--relay-origins))
               (credentials (mevedel-collaboration-artifact-room--credentials
                             workspace id))
               (room
                (append
                 (list :room-kind 'artifact
                       :workspace workspace
                       :artifact id
                       :artifact-kind (plist-get meta :kind)
                       :directory root
                       :session-label (format "Artifact: %s" (plist-get meta :title))
                       :started-at (float-time)
                       :last-guest-departure nil
                       :room-id (plist-get credentials :room-id)
                       :key (plist-get credentials :key)
                       :write-token (plist-get credentials :write-token)
                       ;; A largest HTML file, base64 in the JSON request,
                       ;; then the transfer's own base64.
                       :editing-max-bytes (* 30 1024 1024)
                       :guests (make-hash-table :test #'eql))
                 (mevedel-collaboration-artifact-room--links
                  (cdr origins) credentials))))
          (puthash key room mevedel-collaboration-artifact-room--rooms)
          (add-hook 'mevedel-artifact-store-deleted-functions
                    #'mevedel-collaboration-artifact-room--deleted)
          (add-hook 'kill-emacs-hook #'mevedel-collaboration-artifact-room--stop-for-emacs)
          (condition-case error-data
              (plist-put
               room :transport
               (mevedel-collaboration--transport-open
                (format "%s/r/%s?role=host" (car origins)
                        (plist-get credentials :room-id))
                (plist-get credentials :key)
                :headers (mevedel-collaboration--host-headers)
                :on-frame (lambda (peer frame)
                            (mevedel-collaboration-artifact-room--on-frame
                             root id peer frame))
                :on-control (lambda (event peer)
                              (mevedel-collaboration-artifact-room--on-control
                               root id event peer))
                :on-state (lambda (state)
                            (mevedel-collaboration-artifact-room--on-state
                             root id state))))
            (error
             (remhash key mevedel-collaboration-artifact-room--rooms)
             (signal (car error-data) (cdr error-data))))
          (mevedel-collaboration-artifact-room--set-recorded root t)
          (mevedel-sleep-update)
          room))))

(defun mevedel-collaboration-artifact-room--stop (room reason)
  "Stop artifact ROOM for REASON, telling its connected consumers.
Its credentials stay, so starting it again revives its links.  Its
working stretch ends with the connections."
  (remhash (cons (plist-get room :directory) (plist-get room :artifact))
           mevedel-collaboration-artifact-room--rooms)
  (mevedel-collaboration-artifact-room--settle room)
  (when (zerop (hash-table-count mevedel-collaboration-artifact-room--rooms))
    (remove-hook 'kill-emacs-hook #'mevedel-collaboration-artifact-room--stop-for-emacs))
  (when-let* ((transport (plist-get room :transport)))
    (ignore-errors
      (when (> (hash-table-count (plist-get room :guests)) 0)
        (mevedel-collaboration--transport-send
         transport 0 (list :t "bye" :reason (format "%s" reason)))))
    (ignore-errors (mevedel-collaboration--transport-stop transport)))
  ;; Out of the registry, its consumers leave the artifact's roster.
  (ignore-errors (mevedel-collaboration-presence-publish room))
  (mevedel-sleep-update))

(defun mevedel-collaboration-artifact-room-stop-all (reason)
  "Stop every running artifact room for REASON."
  (let (rooms)
    (maphash (lambda (_key room) (push room rooms))
             mevedel-collaboration-artifact-room--rooms)
    (dolist (room rooms)
      (mevedel-collaboration-artifact-room--stop room reason))))

(defun mevedel-collaboration-artifact-room--stop-for-emacs ()
  "Stop every artifact room before Emacs exits, telling consumers why.
Credentials and the record stay, so the next start shares them again."
  (mevedel-collaboration-artifact-room-stop-all 'origin-stopped))

(defun mevedel-collaboration-artifact-room--deleted (workspace id)
  "End the sharing of WORKSPACE's artifact ID, which was just deleted.
Its connected consumers are told `artifact-deleted'.  Its credentials
went with its bookkeeping."
  (when-let* ((room (mevedel-collaboration-artifact-room--find
                     (mevedel-workspace-root workspace) id)))
    (mevedel-collaboration-artifact-room--stop room 'artifact-deleted))
  (mevedel-collaboration-artifact-room--update-record workspace))


;;
;;; Sharing

(defun mevedel-collaboration-artifact-room-sharing (workspace id operation)
  "Apply sharing OPERATION to WORKSPACE's artifact ID.
This is the one host operation behind every way of sharing: Emacs
commands, daemon functions and the browser.  OPERATION is one of:

  `share'   start sharing ID, or keep its existing links; return them.
  `revoke'  stop sharing ID: every connected consumer is told `revoked'
            and disconnected, and its links stop working for good.
            Return nil.
  `rotate'  revoke, then share with new links; return them.

Links are a plist of `:link-view' and `:link-edit'.  Starting or ending
the sharing notifies store observers, so listings show the change."
  (pcase operation
    ('share
     (let* ((shared (mevedel-artifact-store-shared-p workspace id))
            (room (mevedel-collaboration-artifact-room--start workspace id)))
       (unless shared
         (mevedel-artifact-store-changed workspace))
       (list :link-view (plist-get room :link-view)
             :link-edit (plist-get room :link-edit))))
    ('revoke
     (when-let* ((room (mevedel-collaboration-artifact-room--find
                        (mevedel-workspace-root workspace) id)))
       (mevedel-collaboration-artifact-room--stop room 'revoked))
     (let ((path (mevedel-artifact-store-link-path workspace id)))
       (when (file-exists-p path)
         (delete-file path)
         (mevedel-artifact-store-changed workspace)))
     (mevedel-collaboration-artifact-room--update-record workspace)
     nil)
    ('rotate
     (mevedel-collaboration-artifact-room--meta workspace id)
     (mevedel-collaboration-artifact-room-sharing workspace id 'revoke)
     (mevedel-collaboration-artifact-room-sharing workspace id 'share))
    (_ (error "Unknown sharing operation: %S" operation))))

;;;###autoload
(defun mevedel-collaboration-artifact-room-share (directory id)
  "Share artifact ID of DIRECTORY's workspace and return its links.
The result is a plist of `:link-view' and `:link-edit', bearer links of
the form WEB-ORIGIN/#ROOM-ID.SECRET that reach only this artifact.
Sharing an artifact that is already shared returns its existing links.
An Emacs daemon shares this way, for example through emacsclient."
  (mevedel-collaboration-artifact-room-sharing
   (mevedel-collaboration-artifact-room--workspace directory) id 'share))

;;;###autoload
(defun mevedel-collaboration-artifact-room-revoke (directory id)
  "Stop sharing artifact ID of DIRECTORY's workspace; return nil.
Connected consumers are disconnected with the reason `revoked', and the
artifact's links stop working for good.  Sharing it again mints new
ones.  An Emacs daemon revokes this way, for example through
emacsclient."
  (mevedel-collaboration-artifact-room-sharing
   (mevedel-collaboration-artifact-room--workspace directory) id 'revoke))

;;;###autoload
(defun mevedel-collaboration-artifact-room-rotate (directory id)
  "Replace the links of artifact ID of DIRECTORY's workspace; return them.
This revokes the old links, as `mevedel-collaboration-artifact-room-revoke'
does, and shares the artifact again.  The result is a plist of
`:link-view' and `:link-edit'."
  (mevedel-collaboration-artifact-room-sharing
   (mevedel-collaboration-artifact-room--workspace directory) id 'rotate))

(defconst mevedel-collaboration-artifact-room--links-buffer
  "*mevedel artifact links*"
  "Buffer showing an artifact's links, kept out of *Messages*.")

(defun mevedel-collaboration-artifact-room--choose (&optional shared)
  "Read one of the current workspace's shareable artifacts; return its id.
With SHARED, offer only the shared ones."
  (let* ((workspace (mevedel-workspace-current))
         (choices (cl-loop for row in (mevedel-artifact-store-list workspace)
                           for id = (plist-get row :id)
                           when (and (memq (plist-get row :kind)
                                           mevedel-collaboration-artifact-room--kinds)
                                     (or (not shared)
                                         (mevedel-artifact-store-shared-p workspace id)))
                           collect (cons (format "%s (%s %s)" (plist-get row :title)
                                                 (plist-get row :kind) id)
                                         id))))
    (unless choices
      (user-error (if shared "No artifact of this workspace is shared"
                    "This workspace has no whiteboard, document or HTML artifact to share")))
    (cdr (assoc (completing-read (if shared "Shared artifact: " "Share artifact: ")
                                 choices nil t)
                choices))))

(defun mevedel-collaboration-artifact-room--present (meta links)
  "Show the artifact LINKS of the artifact described by META.
The edit link goes to the kill ring; neither goes to *Messages*."
  (kill-new (plist-get links :link-edit))
  (with-help-window mevedel-collaboration-artifact-room--links-buffer
    (princ (format "%s (%s) is shared.\n\n" (plist-get meta :title)
                   (plist-get meta :kind)))
    (princ (format "View link, to read it:\n  %s\n\n" (plist-get links :link-view)))
    (princ (format "Edit link, to read and change it:\n  %s\n\n"
                   (plist-get links :link-edit)))
    (princ (concat "The edit link is in the kill ring.  Attach a link where "
                   "you work, never in a chat:\n"
                   "  npx mevedel-remote-artifact attach LINK\n")))
  (message "mevedel: edit link copied to kill ring"))

;;;###autoload
(defun mevedel-collaboration-share-artifact (id)
  "Share the current workspace's artifact ID and show its links.
ID names a whiteboard, document or HTML artifact.
The edit link is copied to the kill ring.  An artifact that is shared
already keeps its links."
  (interactive (list (mevedel-collaboration-artifact-room--choose)))
  (let* ((workspace (mevedel-workspace-current))
         (meta (mevedel-collaboration-artifact-room--meta workspace id)))
    (unless (or (mevedel-artifact-store-shared-p workspace id)
                (yes-or-no-p
                 (format (concat "Share \"%s\" through the relay? Its view link "
                                 "lets whoever holds it read this %s, its edit "
                                 "link change it, until you revoke them. ")
                         (plist-get meta :title) (plist-get meta :kind))))
      (user-error "Artifact not shared"))
    (mevedel-collaboration-artifact-room--present
     meta (mevedel-collaboration-artifact-room-sharing workspace id 'share))))

;;;###autoload
(defun mevedel-collaboration-revoke-artifact (id)
  "Stop sharing the current workspace's artifact ID.
Every connected consumer is disconnected and its links stop working
for good."
  (interactive (list (mevedel-collaboration-artifact-room--choose t)))
  (let* ((workspace (mevedel-workspace-current))
         (title (or (plist-get (mevedel-artifact-store-meta workspace id) :title) id)))
    (unless (yes-or-no-p
             (format "Whoever holds its links loses access.  Revoke the links of \"%s\"? "
                     title))
      (user-error "Artifact still shared"))
    (mevedel-collaboration-artifact-room-sharing workspace id 'revoke)
    (message "mevedel: %s is no longer shared" title)))

;;;###autoload
(defun mevedel-collaboration-rotate-artifact (id)
  "Replace the links of the current workspace's shared artifact ID.
The old links stop working; the new ones are shown, the edit link
copied to the kill ring."
  (interactive (list (mevedel-collaboration-artifact-room--choose t)))
  (let* ((workspace (mevedel-workspace-current))
         (meta (mevedel-collaboration-artifact-room--meta workspace id)))
    (unless (yes-or-no-p
             (format "The old links stop working.  Replace the links of \"%s\"? "
                     (plist-get meta :title)))
      (user-error "Links unchanged"))
    (mevedel-collaboration-artifact-room--present
     meta (mevedel-collaboration-artifact-room-sharing workspace id 'rotate))))

(provide 'mevedel-collaboration-artifact-room)
;;; mevedel-collaboration-artifact-room.el ends here
