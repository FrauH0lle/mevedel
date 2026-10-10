;;; mevedel-artifact-store.el --- Workspace artifact store -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace artifact store at `<workspace>/.mevedel/artifacts/'.  Each
;; artifact is one directory, its name the stable id.  The directory holds the
;; artifact file the model writes with ApplyPatch, plus host bookkeeping:
;; `meta.el' (kind, title, primary file, dedicated session), `versions/'
;; (numbered copies and `index.el') and `comments.json'.  Sessions do not own
;; artifacts; they attach to them by id.  An artifact may have one dedicated
;; session, created on demand for conversations started outside any chat; it
;; is hidden from session lists, kept from expiry, and deleted with the
;; artifact.
;;
;; A settled ApplyPatch reports its writes here.  A write into a new id
;; directory creates the artifact; every write of its primary file records a
;; version.  Versions are capped by count and bytes, the latest always kept.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'mevedel-workspace)

;; `mevedel-chat'
(declare-function mevedel--chat-buffer "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--ensure-chat-preset "mevedel-chat" (chat-buffer))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(autoload 'mevedel--chat-buffer "mevedel-chat")
(autoload 'mevedel--ensure-chat-preset "mevedel-chat")
(autoload 'mevedel--workspace-sessions "mevedel-chat")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (workspace args callback &optional authorize commit))
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(declare-function mevedel-shared-editing-duplicate
                  "mevedel-shared-editing" (workspace id new-id))
(declare-function mevedel-shared-editing-restore
                  "mevedel-shared-editing" (workspace id n actor callback))
(declare-function mevedel-shared-editing-save-version
                  "mevedel-shared-editing" (workspace id &optional session-id))
(autoload 'mevedel-shared-editing-duplicate "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-restore "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-save-version "mevedel-shared-editing")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save "mevedel-session-artifacts"
                  (session buffer &optional settled force))
(declare-function mevedel-session-artifacts-sessions-dir
                  "mevedel-session-artifacts" (workspace))
(autoload 'mevedel-session-artifacts-save "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts")

;; `mevedel-session-naming'
(declare-function mevedel-session-naming-rename "mevedel-session-naming"
                  (session buffer name &optional current-p))
(autoload 'mevedel-session-naming-rename "mevedel-session-naming")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-delete
                  "mevedel-session-persistence" (workspace save-path))
(declare-function mevedel-session-persistence-resume-id
                  "mevedel-session-persistence" (workspace session-id))
(declare-function mevedel-session-persistence-write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(autoload 'mevedel-session-persistence-delete "mevedel-session-persistence")
(declare-function mevedel-session-persistence-list-sessions
                  "mevedel-session-persistence" (workspace &optional cached))
(autoload 'mevedel-session-persistence-list-sessions "mevedel-session-persistence")
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))
(autoload 'mevedel-turn-busy-p "mevedel-turn")
(defvar mevedel-collaboration-stop-reason)
(autoload 'mevedel-session-persistence-resume-id "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-write-sidecar-now
  "mevedel-session-persistence")

(defcustom mevedel-artifact-store-max-versions 20
  "Most versions kept per artifact; the oldest are dropped first."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-artifact-store-max-version-bytes (* 64 1024 1024)
  "Most bytes of versions kept per artifact.
The oldest versions are dropped first; the latest version always stays,
even when it alone exceeds this size."
  :type 'natnum
  :group 'mevedel)

(defvar mevedel-artifact-store-changed-functions nil
  "Functions called with a WORKSPACE after its artifact store changed.
Live collaboration rooms follow the store through it.")

(defun mevedel-artifact-store--changed (workspace)
  "Tell `mevedel-artifact-store-changed-functions' that WORKSPACE's store changed."
  (run-hook-with-args 'mevedel-artifact-store-changed-functions workspace))

(defconst mevedel-artifact-store--bookkeeping
  '("meta.el" "versions" "comments.json" "state.json")
  "Top-level names in an artifact directory that belong to the host.")

(defun mevedel-artifact-store-directory (workspace)
  "Return WORKSPACE's artifact store directory, with trailing slash."
  (file-name-as-directory
   (file-name-concat (mevedel-workspace-state-dir workspace) "artifacts")))

(defun mevedel-artifact-store-id-p (id)
  "Return non-nil when ID is a valid artifact id.
Letters, digits, `-' and `_', at most 80 of them: the rule shared items,
browser names and the migration all follow."
  (and (stringp id)
       (string-match-p "\\`[A-Za-z0-9_-]\\{1,80\\}\\'" id)))

(defun mevedel-artifact-store-artifact-directory (workspace id)
  "Return the directory of artifact ID in WORKSPACE's store."
  (unless (mevedel-artifact-store-id-p id)
    (error "Invalid artifact id: %S" id))
  (file-name-concat (mevedel-artifact-store-directory workspace) id))

(defun mevedel-artifact-store-ids (workspace)
  "Return the artifact ids in WORKSPACE's store, sorted.
Reads directory names only, so it stays cheap enough for a menu redraw."
  (let ((dir (mevedel-artifact-store-directory workspace)))
    (when (file-directory-p dir)
      (sort (cl-loop for (name . attributes)
                     in (directory-files-and-attributes dir nil "\\`[^.]" t)
                     when (and (eq t (file-attribute-type attributes))
                               (mevedel-artifact-store-id-p name))
                     collect name)
            #'string<))))

(defun mevedel-artifact-store--read (file)
  "Return the Lisp datum in FILE, or nil when FILE does not exist.
A file that exists but cannot be read signals: a passing failure, such
as a dropped remote connection, must not read as absent metadata or an
empty version index, which the next write would then replace."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer)))
    (file-missing nil)))

(defun mevedel-artifact-store--write (file datum)
  "Atomically replace FILE with the printed DATUM."
  (mevedel--write-file-atomically
   file (let ((print-length nil) (print-level nil))
          (prin1-to-string datum))))

(defun mevedel-artifact-store-meta (workspace id)
  "Return the metadata plist of artifact ID, or nil when it has none.
Metadata naming a primary file outside the artifact's directory signals:
the store may come from version control, and restoring a version writes
that file."
  (let ((meta (mevedel-artifact-store--read
               (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                                 "meta.el"))))
    (when meta
      (let ((file (plist-get meta :file)))
        (unless (and (stringp file) (not (string-empty-p file))
                     (not (file-name-absolute-p file))
                     (not (member ".." (split-string file "/"))))
          (error "Artifact %s names an invalid file: %S" id file))))
    meta))

(defun mevedel-artifact-store--kind (file)
  "Return the artifact kind for FILE's extension."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "html" "htm") 'html)
    ((or "md" "markdown") 'markdown)
    ((or "png" "jpg" "jpeg" "gif" "svg" "webp") 'image)
    (_ 'file)))

(defconst mevedel-artifact-store-item-kinds '(whiteboard document)
  "Kinds of artifact edited live through shared editing, not by file.")

(defun mevedel-artifact-store-item-p (meta)
  "Return non-nil when META describes a whiteboard or document."
  (memq (plist-get meta :kind) mevedel-artifact-store-item-kinds))

(defun mevedel-artifact-store-create-meta (workspace id file &optional kind title)
  "Write the metadata of new artifact ID whose primary file is FILE.
KIND defaults to what FILE's name says and TITLE to ID, which names what
the artifact shows where its file is often just index.html."
  (let ((meta (list :kind (or kind (mevedel-artifact-store--kind file))
                    :title (or title id)
                    :file file
                    :created (format-time-string "%FT%T%z"))))
    (mevedel-artifact-store--write
     (file-name-concat (mevedel-artifact-store-artifact-directory workspace id) "meta.el")
     meta)
    (mevedel-artifact-store--changed workspace)
    meta))

(defun mevedel-artifact-store--versions-dir (workspace id)
  "Return the versions directory of artifact ID."
  (file-name-concat (mevedel-artifact-store-artifact-directory workspace id) "versions"))

(defun mevedel-artifact-store-versions (workspace id)
  "Return artifact ID's version plists, oldest first.
Each has :n, :file (relative to the versions directory), :time (seconds
since the epoch), :session and :bytes."
  (mevedel-artifact-store--read
   (file-name-concat (mevedel-artifact-store--versions-dir workspace id)
                     "index.el")))

(defun mevedel-artifact-store-version-path (workspace id n)
  "Return the file holding version N of artifact ID."
  (if-let* ((entry (cl-find n (mevedel-artifact-store-versions workspace id)
                            :key (lambda (row) (plist-get row :n)))))
      (file-name-concat (mevedel-artifact-store--versions-dir workspace id)
                        (plist-get entry :file))
    (error "Artifact %s has no version %s" id n)))

(defun mevedel-artifact-store--prune (versions)
  "Return VERSIONS within the count and byte caps, oldest dropped first."
  (let ((versions (last versions (max 1 mevedel-artifact-store-max-versions))))
    (while (and (cdr versions)
                (> (apply #'+ (mapcar (lambda (row) (plist-get row :bytes))
                                      versions))
                   mevedel-artifact-store-max-version-bytes))
      (setq versions (cdr versions)))
    versions))

(defun mevedel-artifact-store-record-version (workspace id &optional session-id content)
  "Record the current primary file of artifact ID as a new version.
SESSION-ID names the session whose write this is.  CONTENT, a string,
is recorded instead of the file, for an artifact whose version is a
reduced copy of its state.  Return the version number."
  (let* ((meta (or (mevedel-artifact-store-meta workspace id)
                   (error "Artifact %s has no metadata" id)))
         (source (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                                   (plist-get meta :file)))
         (directory (mevedel-artifact-store--versions-dir workspace id))
         (versions (mevedel-artifact-store-versions workspace id))
         (n (1+ (or (plist-get (car (last versions)) :n) 0)))
         (name (format "%06d%s" n (or (file-name-extension source t) "")))
         (kept nil))
    (make-directory directory t)
    (if content
        (mevedel--write-file-atomically (file-name-concat directory name) content)
      (copy-file source (file-name-concat directory name) t))
    (setq versions
          (append versions
                  (list (list :n n :file name
                              :time (truncate (float-time))
                              :session session-id
                              :bytes (file-attribute-size
                                      (file-attributes
                                       (file-name-concat directory name)))))))
    (setq kept (mevedel-artifact-store--prune versions))
    (mevedel-artifact-store--write (file-name-concat directory "index.el") kept)
    ;; Index first: a crash leaves an orphan copy, never a dangling entry.
    (dolist (row versions)
      (unless (memq row kept)
        (ignore-errors
          (delete-file (file-name-concat directory (plist-get row :file))))))
    (mevedel-artifact-store--changed workspace)
    n))

(defun mevedel-artifact-store-save-version (workspace id &optional session-id)
  "Record artifact ID's current state as a new version and return its number.
A whiteboard or document records its reduced state; a file artifact, its
primary file.  SESSION-ID names the session whose work it is."
  (if (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
      (mevedel-shared-editing-save-version workspace id session-id)
    (mevedel-artifact-store-record-version workspace id session-id)))

(defun mevedel-artifact-store-restore-version (workspace id n &optional session-id)
  "Restore version N of artifact ID as its newest version.
SESSION-ID is recorded as the restoring session.  Return the new version
number.  A whiteboard or document restores through its editing queue as one
revertible edit; that returns nil, and the version follows once it is
saved."
  (let ((meta (mevedel-artifact-store-meta workspace id)))
    (if (mevedel-artifact-store-item-p meta)
        (progn
          (mevedel-shared-editing-restore
           workspace id n "Host"
           (lambda (reply)
             (if (plist-get reply :error)
                 (message "mevedel: %s was not restored: %s" id (plist-get reply :error))
               (mevedel-shared-editing-save-version workspace id session-id))))
          nil)
      (mevedel-artifact-store--restore-file workspace id n meta session-id))))

(defun mevedel-artifact-store--restore-file (workspace id n meta session-id)
  "Copy version N of file artifact ID, described by META, back as newest.
SESSION-ID is recorded as the restoring session."
  (let ((target (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                                  (plist-get meta :file))))
    ;; Copying onto a link would write wherever it points.
    (when (file-symlink-p target)
      (error "Artifact %s's file is a symbolic link" id))
    (copy-file (mevedel-artifact-store-version-path workspace id n) target t))
  (mevedel-artifact-store-record-version workspace id session-id))

(defun mevedel-artifact-store-duplicate (workspace id new-id)
  "Copy artifact ID to the new, independent artifact NEW-ID.
The copy starts with one version and its own metadata."
  (if (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
      (mevedel-shared-editing-duplicate workspace id new-id)
    (mevedel-artifact-store--duplicate-files workspace id new-id)))

(defun mevedel-artifact-store--duplicate-files (workspace id new-id)
  "Copy file artifact ID's directory to the new artifact NEW-ID."
  (let ((source (mevedel-artifact-store-artifact-directory workspace id))
        (target (mevedel-artifact-store-artifact-directory workspace new-id))
        (meta (or (mevedel-artifact-store-meta workspace id)
                  (error "Artifact %s has no metadata" id))))
    (when (file-exists-p target)
      (error "Artifact %s already exists" new-id))
    (make-directory target t)
    ;; The files only: versions, comments and metadata belong to the original.
    (dolist (name (directory-files source nil directory-files-no-dot-files-regexp t))
      (unless (member name mevedel-artifact-store--bookkeeping)
        (let ((from (file-name-concat source name)))
          (if (file-directory-p from)
              (copy-directory from (file-name-concat target name) t t)
            (copy-file from (file-name-concat target name) nil t t)))))
    (mevedel-artifact-store-create-meta workspace new-id (plist-get meta :file)
                                        (plist-get meta :kind) (plist-get meta :title))
    (mevedel-artifact-store-record-version workspace new-id)
    new-id))

(defun mevedel-artifact-store-delete (workspace id &optional actor callback)
  "Delete artifact ID with its metadata, versions and dedicated session.
A whiteboard or document is deleted through its editing queue, after any
save in progress, so its editors learn of it from ACTOR (default
\"Host\").  CALLBACK receives nil once the artifact is gone or the
reason it was not deleted; without one, a refusal is a message."
  (let ((callback (or callback
                      (lambda (failure)
                        (when failure
                          (message "mevedel: %s was not deleted: %s" id failure))))))
    (if (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
        (mevedel-shared-editing-call
         workspace (list :action "delete" :id id :actor (or actor "Host"))
         (lambda (reply) (funcall callback (plist-get reply :error))))
      (funcall callback (condition-case err
                            (progn (mevedel-artifact-store--delete workspace id) nil)
                          (error (error-message-string err)))))))

(defun mevedel-artifact-store--delete (workspace id)
  "Delete artifact ID's directory and dedicated session now.
A dedicated session open in this Emacs is closed first, and its room
ended, unless a turn is still running there.  One that cannot be deleted
yet -- held by another client, or pinned by pending journal capture --
stays as an ordinary session and expires like one."
  (when-let* ((session-id (plist-get (mevedel-artifact-store-meta workspace id)
                                     :dedicated-session))
              ((mevedel-artifact-store--conversation-agrees-p workspace session-id id)))
    (when-let* ((buffer (mevedel-artifact-store--live-buffer workspace session-id)))
      (when (mevedel-turn-busy-p buffer)
        (error "The conversation of %s is still working; stop it first" id))
      ;; Opening an item from the lobby opens this session for its room, so
      ;; its buffer is no reason to keep the artifact.
      (let ((kill-buffer-query-functions nil)
            (mevedel-collaboration-stop-reason 'artifact-deleted))
        (kill-buffer buffer)))
    (let ((save-path (file-name-concat
                      (mevedel-session-artifacts-sessions-dir workspace) session-id)))
      (when (and (file-directory-p save-path)
                 (not (mevedel-session-persistence-delete workspace save-path)))
        (message "mevedel: the conversation of %s is still in use; it stays as an ordinary session"
                 id))))
  (delete-directory (mevedel-artifact-store-artifact-directory workspace id) t)
  (mevedel-artifact-store--changed workspace))

(defun mevedel-artifact-store-list (workspace)
  "Return WORKSPACE's artifacts as plists, newest modification first.
Each has :id, :kind, :title, :file, :dedicated-session, :path, :size,
:modified and :versions; a missing primary file has :missing t.  An
artifact whose metadata cannot be read is left out rather than hiding
every other one."
  (let (rows)
    (dolist (id (mevedel-artifact-store-ids workspace))
      (when-let* ((meta (ignore-errors (mevedel-artifact-store-meta workspace id))))
        (let* ((path (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                                       (plist-get meta :file)))
               (attributes (file-attributes path)))
          (push (append
                 (list :id id :kind (plist-get meta :kind)
                       :title (plist-get meta :title) :file (plist-get meta :file)
                       :dedicated-session (plist-get meta :dedicated-session)
                       :path path
                       :versions (length (mevedel-artifact-store-versions
                                          workspace id)))
                 (if attributes
                     (list :size (file-attribute-size attributes)
                           :modified (file-attribute-modification-time
                                      attributes))
                   (list :missing t :size 0 :modified 0)))
                rows))))
    (sort rows (lambda (left right)
                 (time-less-p (plist-get right :modified)
                              (plist-get left :modified))))))

(defun mevedel-artifact-store-attach (session id &optional buffer)
  "Attach artifact ID to SESSION.
With BUFFER, the session's data buffer, the sidecar is written at once;
otherwise the next session save carries the attachment."
  (unless (member id (mevedel-session-attached-artifacts session))
    (setf (mevedel-session-attached-artifacts session)
          (append (mevedel-session-attached-artifacts session) (list id)))
    (when buffer
      (mevedel-session-persistence-write-sidecar-now session buffer))
    (mevedel-artifact-store--changed (mevedel-session-workspace session))))

(defun mevedel-artifact-store-update-meta (workspace id &rest properties)
  "Set PROPERTIES in the metadata of artifact ID and return it."
  (let ((meta (copy-sequence (or (mevedel-artifact-store-meta workspace id)
                                 (error "Artifact %s has no metadata" id)))))
    (while properties
      (setq meta (plist-put meta (pop properties) (pop properties))))
    (mevedel-artifact-store--write
     (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                       "meta.el")
     meta)
    meta))

(defun mevedel-artifact-store-dedicated-p (workspace session-id id)
  "Return non-nil when SESSION-ID is the conversation of WORKSPACE's artifact ID.
ID is what the session records as its artifact; the artifact must name the
session back, so a copied `meta.el' or a deleted artifact does not make
another session its conversation."
  (and (mevedel-artifact-store-id-p id)
       (equal session-id
              (plist-get (ignore-errors (mevedel-artifact-store-meta workspace id))
                         :dedicated-session))))

(defun mevedel-artifact-store--conversation-agrees-p (workspace session-id id)
  "Return non-nil when session SESSION-ID records WORKSPACE's artifact ID.
A session that does not, such as one a copied `meta.el' names, is never
deleted with ID."
  (if-let* ((buffer (mevedel-artifact-store--live-buffer workspace session-id)))
      (equal id (mevedel-session-dedicated-artifact
                 (buffer-local-value 'mevedel--session buffer)))
    (cl-some (lambda (entry)
               (let ((summary (plist-get entry :summary)))
                 (and (equal session-id (plist-get summary :session-id))
                      (equal id (plist-get summary :dedicated-artifact)))))
             (mevedel-session-persistence-list-sessions workspace))))

(defun mevedel-artifact-store--live-buffer (workspace session-id)
  "Return the live root data buffer of WORKSPACE's SESSION-ID, or nil."
  (cdr (cl-find-if (lambda (entry)
                     (equal session-id
                            (mevedel-session-session-id
                             (buffer-local-value 'mevedel--session (cdr entry)))))
                   (mevedel--workspace-sessions workspace))))

(defun mevedel-artifact-store-session-buffer (workspace session-id)
  "Return the data buffer of WORKSPACE's SESSION-ID, resuming it when saved.
Return nil when the session no longer exists."
  (or (mevedel-artifact-store--live-buffer workspace session-id)
      (mevedel-session-persistence-resume-id workspace session-id)))

(defun mevedel-artifact-store-conversation (workspace id)
  "Return the data buffer of artifact ID's dedicated session.
The session is created, attached and saved on first use, so it survives
before anyone has written in it."
  (let ((session-id (plist-get (or (mevedel-artifact-store-meta workspace id)
                                   (error "No artifact %s" id))
                               :dedicated-session)))
    (or (and session-id
             (mevedel-artifact-store-session-buffer workspace session-id))
        (let* ((buffer (mevedel--chat-buffer nil t workspace))
               (session (buffer-local-value 'mevedel--session buffer)))
          (mevedel--ensure-chat-preset buffer)
          (setf (mevedel-session-dedicated-artifact session) id)
          (mevedel-artifact-store-attach session id)
          (mevedel-session-artifacts-save session buffer nil t)
          (mevedel-session-naming-rename session buffer (format "Artifact %s" id))
          (mevedel-artifact-store-update-meta
           workspace id :dedicated-session (mevedel-session-session-id session))
          buffer))))

(defun mevedel-artifact-store-note-writes (session changes &optional request)
  "Record settled ApplyPatch CHANGES of SESSION that land in the store.
A write into a new id directory creates the artifact, and any write
attaches SESSION.  A write of an artifact's primary file is versioned once
REQUEST settles, so a turn's many small patches leave one version and the
one before the turn survives; without a request it is versioned now."
  (let ((workspace (mevedel-session-workspace session)))
    (when-let* ((store (and workspace
                            (expand-file-name
                             (mevedel-artifact-store-directory workspace)))))
      (dolist (change changes)
        (let* ((path (expand-file-name (plist-get change :path)))
               (relative (and (eq (plist-get change :action) 'write)
                              (string-prefix-p store path)
                              (substring path (length store))))
               (slash (and relative (string-search "/" relative)))
               (id (and slash (substring relative 0 slash)))
               (file (and slash (substring relative (1+ slash)))))
          ;; ponytail: only the primary file is versioned; secondary files
          ;; (assets) change without versions.
          (when (and (mevedel-artifact-store-id-p id)
                     (not (member (car (split-string file "/"))
                                  mevedel-artifact-store--bookkeeping)))
            (let ((meta (or (mevedel-artifact-store-meta workspace id)
                            (mevedel-artifact-store-create-meta
                             workspace id file))))
              (when (equal file (plist-get meta :file))
                (if request
                    (cl-pushnew (cons workspace id)
                                (mevedel-request-edited-artifacts request)
                                :test #'equal)
                  (mevedel-artifact-store-record-version
                   workspace id (mevedel-session-session-id session))))
              (mevedel-artifact-store-attach session id))))))))

(provide 'mevedel-artifact-store)
;;; mevedel-artifact-store.el ends here
