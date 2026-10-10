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

(defconst mevedel-artifact-store--bookkeeping
  '("meta.el" "versions" "comments.json" "state.json")
  "Top-level names in an artifact directory that belong to the host.")

(defun mevedel-artifact-store-directory (workspace)
  "Return WORKSPACE's artifact store directory, with trailing slash."
  (file-name-as-directory
   (file-name-concat (mevedel-workspace-state-dir workspace) "artifacts")))

(defun mevedel-artifact-store-id-p (id)
  "Return non-nil when ID names one visible store directory."
  (and (stringp id)
       (not (string-empty-p id))
       (not (string-prefix-p "." id))
       (not (string-match-p "/" id))))

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
                     when (eq t (file-attribute-type attributes))
                     collect name)
            #'string<))))

(defun mevedel-artifact-store--read (file)
  "Return the Lisp datum in FILE, or nil when it is missing or unreadable."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file)
      (read (current-buffer)))))

(defun mevedel-artifact-store--write (file datum)
  "Atomically replace FILE with the printed DATUM."
  (mevedel--write-file-atomically
   file (let ((print-length nil) (print-level nil))
          (prin1-to-string datum))))

(defun mevedel-artifact-store-meta (workspace id)
  "Return the metadata plist of artifact ID, or nil."
  (mevedel-artifact-store--read
   (file-name-concat (mevedel-artifact-store-artifact-directory workspace id) "meta.el")))

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
KIND and TITLE default to what FILE's name says."
  (let ((meta (list :kind (or kind (mevedel-artifact-store--kind file))
                    :title (or title (file-name-nondirectory file))
                    :file file
                    :created (format-time-string "%FT%T%z"))))
    (mevedel-artifact-store--write
     (file-name-concat (mevedel-artifact-store-artifact-directory workspace id) "meta.el")
     meta)
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
    n))

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
  (copy-file (mevedel-artifact-store-version-path workspace id n)
             (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                               (plist-get meta :file))
             t)
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
    (copy-directory source target nil t t)
    (delete-directory (mevedel-artifact-store--versions-dir workspace new-id) t)
    (mevedel-artifact-store-create-meta workspace new-id (plist-get meta :file)
                                        (plist-get meta :kind) (plist-get meta :title))
    (mevedel-artifact-store-record-version workspace new-id)
    new-id))

(defun mevedel-artifact-store-delete (workspace id)
  "Delete artifact ID with its metadata, versions and dedicated session.
Refuse while the dedicated session is open in Emacs, whose buffer would
save it straight back, or held by another client."
  (when-let* ((session-id (plist-get (mevedel-artifact-store-meta workspace id)
                                     :dedicated-session)))
    (when (mevedel-artifact-store--live-buffer workspace session-id)
      (error "Close the conversation of %s in Emacs first" id))
    (let ((save-path (file-name-concat
                      (mevedel-session-artifacts-sessions-dir workspace) session-id)))
      (when (and (file-directory-p save-path)
                 (not (mevedel-session-persistence-delete workspace save-path)))
        (error "The conversation of %s is still in use elsewhere" id))))
  (delete-directory (mevedel-artifact-store-artifact-directory workspace id) t))

(defun mevedel-artifact-store-list (workspace)
  "Return WORKSPACE's artifacts as plists, newest modification first.
Each has :id, :kind, :title, :path, :size, :modified and :versions; a
missing primary file has :missing t."
  (let (rows)
    (dolist (id (mevedel-artifact-store-ids workspace))
      (when-let* ((meta (mevedel-artifact-store-meta workspace id)))
        (let* ((path (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                                       (plist-get meta :file)))
               (attributes (file-attributes path)))
          (push (append
                 (list :id id :kind (plist-get meta :kind)
                       :title (plist-get meta :title) :path path
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
      (mevedel-session-persistence-write-sidecar-now session buffer))))

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

(defun mevedel-artifact-store-dedicated-ids (workspace)
  "Return the session ids dedicated to WORKSPACE's artifacts."
  (delq nil (mapcar (lambda (id)
                      (plist-get (mevedel-artifact-store-meta workspace id)
                                 :dedicated-session))
                    (mevedel-artifact-store-ids workspace))))

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
          (mevedel-artifact-store-attach session id)
          (mevedel-session-artifacts-save session buffer nil t)
          (mevedel-session-naming-rename session buffer (format "Artifact %s" id))
          (mevedel-artifact-store-update-meta
           workspace id :dedicated-session (mevedel-session-session-id session))
          buffer))))

(defun mevedel-artifact-store-note-writes (session changes)
  "Record settled ApplyPatch CHANGES of SESSION that land in the store.
A write into a new id directory creates the artifact; a write of an
artifact's primary file records a version.  Either attaches SESSION."
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
                (mevedel-artifact-store-record-version
                 workspace id (mevedel-session-session-id session)))
              (mevedel-artifact-store-attach session id))))))))

(provide 'mevedel-artifact-store)
;;; mevedel-artifact-store.el ends here
