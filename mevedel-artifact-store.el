;;; mevedel-artifact-store.el --- Workspace artifact store -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace artifact store at `<workspace>/.mevedel/artifacts/'.  Each
;; artifact is one directory, its name the stable id.  The directory holds the
;; artifact file the model writes with ApplyPatch, plus host bookkeeping:
;; `meta.el' (kind, title, primary file) and `versions/' (numbered copies and
;; `index.el').  Sessions do not own artifacts; they attach to them by id.
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

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
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

(defconst mevedel-artifact-store--bookkeeping '("meta.el" "versions")
  "Top-level names in an artifact directory that belong to the host.")

(defun mevedel-artifact-store-directory (workspace)
  "Return WORKSPACE's artifact store directory, with trailing slash."
  (file-name-as-directory
   (file-name-concat (mevedel-workspace-state-dir workspace) "artifacts")))

(defun mevedel-artifact-store--id-p (id)
  "Return non-nil when ID names one visible store directory."
  (and (stringp id)
       (not (string-empty-p id))
       (not (string-prefix-p "." id))
       (not (string-match-p "/" id))))

(defun mevedel-artifact-store--dir (workspace id)
  "Return the directory of artifact ID in WORKSPACE's store."
  (unless (mevedel-artifact-store--id-p id)
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
   (file-name-concat (mevedel-artifact-store--dir workspace id) "meta.el")))

(defun mevedel-artifact-store--kind (file)
  "Return the artifact kind for FILE's extension."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "html" "htm") 'html)
    ((or "md" "markdown") 'markdown)
    ((or "png" "jpg" "jpeg" "gif" "svg" "webp") 'image)
    (_ 'file)))

(defun mevedel-artifact-store--create-meta (workspace id file)
  "Write the metadata of new artifact ID whose primary file is FILE."
  (let ((meta (list :kind (mevedel-artifact-store--kind file)
                    :title (file-name-nondirectory file)
                    :file file
                    :created (format-time-string "%FT%T%z"))))
    (mevedel-artifact-store--write
     (file-name-concat (mevedel-artifact-store--dir workspace id) "meta.el")
     meta)
    meta))

(defun mevedel-artifact-store--versions-dir (workspace id)
  "Return the versions directory of artifact ID."
  (file-name-concat (mevedel-artifact-store--dir workspace id) "versions"))

(defun mevedel-artifact-store-versions (workspace id)
  "Return artifact ID's version plists, oldest first.
Each has :n, :file (relative to the versions directory), :time, :session
and :bytes."
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

(defun mevedel-artifact-store-record-version (workspace id &optional session-id)
  "Record the current primary file of artifact ID as a new version.
SESSION-ID names the session whose write this is.  Return the version
number."
  (let* ((meta (or (mevedel-artifact-store-meta workspace id)
                   (error "Artifact %s has no metadata" id)))
         (source (file-name-concat (mevedel-artifact-store--dir workspace id)
                                   (plist-get meta :file)))
         (directory (mevedel-artifact-store--versions-dir workspace id))
         (versions (mevedel-artifact-store-versions workspace id))
         (n (1+ (or (plist-get (car (last versions)) :n) 0)))
         (name (format "%06d%s" n (or (file-name-extension source t) "")))
         (kept nil))
    (make-directory directory t)
    (copy-file source (file-name-concat directory name) t)
    (setq versions
          (append versions
                  (list (list :n n :file name
                              :time (format-time-string "%FT%T%z")
                              :session session-id
                              :bytes (file-attribute-size
                                      (file-attributes source))))))
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
number."
  (let ((meta (mevedel-artifact-store-meta workspace id)))
    (copy-file (mevedel-artifact-store-version-path workspace id n)
               (file-name-concat (mevedel-artifact-store--dir workspace id)
                                 (plist-get meta :file))
               t)
    (mevedel-artifact-store-record-version workspace id session-id)))

(defun mevedel-artifact-store-duplicate (workspace id new-id)
  "Copy artifact ID to the new, independent artifact NEW-ID.
The copy starts with one version and its own metadata."
  (let ((source (mevedel-artifact-store--dir workspace id))
        (target (mevedel-artifact-store--dir workspace new-id))
        (meta (or (mevedel-artifact-store-meta workspace id)
                  (error "Artifact %s has no metadata" id))))
    (when (file-exists-p target)
      (error "Artifact %s already exists" new-id))
    (copy-directory source target nil t t)
    (delete-directory (mevedel-artifact-store--versions-dir workspace new-id) t)
    (mevedel-artifact-store--create-meta workspace new-id (plist-get meta :file))
    (mevedel-artifact-store-record-version workspace new-id)
    new-id))

(defun mevedel-artifact-store-delete (workspace id)
  "Delete artifact ID with its metadata and versions."
  (delete-directory (mevedel-artifact-store--dir workspace id) t))

(defun mevedel-artifact-store-list (workspace)
  "Return WORKSPACE's artifacts as plists, newest modification first.
Each has :id, :kind, :title, :path, :size, :modified and :versions; a
missing primary file has :missing t."
  (let (rows)
    (dolist (id (mevedel-artifact-store-ids workspace))
      (when-let* ((meta (mevedel-artifact-store-meta workspace id)))
        (let* ((path (file-name-concat (mevedel-artifact-store--dir workspace id)
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
          (when (and (mevedel-artifact-store--id-p id)
                     (not (member (car (split-string file "/"))
                                  mevedel-artifact-store--bookkeeping)))
            (let ((meta (or (mevedel-artifact-store-meta workspace id)
                            (mevedel-artifact-store--create-meta
                             workspace id file))))
              (when (equal file (plist-get meta :file))
                (mevedel-artifact-store-record-version
                 workspace id (mevedel-session-session-id session)))
              (mevedel-artifact-store-attach session id))))))))

(provide 'mevedel-artifact-store)
;;; mevedel-artifact-store.el ends here
