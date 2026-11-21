;;; niwa-collaboration-artifact-projection.el --- artifact projection -*- lexical-binding: t; -*-

;;; Commentary:

;; Maps settled ApplyPatch render data onto artifact cards for writes into
;; the workspace artifact store.  This is
;; the path-domain and file-stat leaf used by collaboration projection; file
;; bytes remain host-only until an authenticated guest requests a published
;; record id.  A write to a remote artifact's file, shared://ALIAS/files/PATH,
;; projects a card for that artifact too; its origin serves the bytes.

;;; Code:

(require 'subr-x)

;; `niwa-artifact-backend'
(declare-function niwa-artifact-backend-catalog "niwa-artifact-backend"
                  (workspace &optional detail))
(declare-function niwa-artifact-backend-registered-p "niwa-artifact-backend"
                  (workspace id))
(autoload 'niwa-artifact-backend-catalog "niwa-artifact-backend")
(autoload 'niwa-artifact-backend-registered-p "niwa-artifact-backend")

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-directory
                  "niwa-artifact-store" (workspace))
(declare-function niwa-artifact-store-meta
                  "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-relative
                  "niwa-artifact-store" (workspace path))
(autoload 'niwa-artifact-store-directory "niwa-artifact-store")
(autoload 'niwa-artifact-store-meta "niwa-artifact-store")
(autoload 'niwa-artifact-store-relative "niwa-artifact-store")

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")

;; `niwa-resource'
(declare-function niwa-resource-remote-file "niwa-resource" (address))
(autoload 'niwa-resource-remote-file "niwa-resource")

;; `niwa-structs'
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--session)

(defun niwa-collaboration--artifacts-dir (session)
  "Return SESSION's expanded artifact store directory, or nil."
  (when-let* ((workspace (and session (niwa-session-workspace session))))
    (expand-file-name (niwa-artifact-store-directory workspace))))

(defun niwa-collaboration--artifact-revision (size modified)
  "Return the revision of a file of SIZE bytes last MODIFIED at that time.
It changes when the file does; viewers only compare it for equality, to
offer reloading an open artifact."
  (format "%d-%s" size (format-time-string "%s.%N" modified t)))

(defvar niwa-collaboration--artifact-stats (make-hash-table :test #'equal)
  "Cached (SIZE . REVISION) per published artifact's qualified path.
SIZE is nil for a missing file; REVISION is its
`niwa-collaboration--artifact-revision'.
Projection runs on every coalesced publish tick, so a remote session
would otherwise pay one target round trip per artifact per tick.  The
small cache is cleared when ApplyPatch settles and whenever the artifact
store changes.")

(defvar niwa-collaboration--artifact-metadata (make-hash-table :test #'equal)
  "Cached metadata per qualified store item directory, or `absent'.
Titles change on store notifications, not on streaming projection ticks.")

(defun niwa-collaboration--artifact-stat (path)
  "Return cached (SIZE . REVISION) for the artifact at qualified PATH.
Both are nil when it is missing."
  (or (gethash path niwa-collaboration--artifact-stats)
      (puthash path
               (if-let* ((attributes (file-attributes path)))
                   (cons (file-attribute-size attributes)
                         (niwa-collaboration--artifact-revision
                          (file-attribute-size attributes)
                          (file-attribute-modification-time attributes)))
                 (cons nil nil))
               niwa-collaboration--artifact-stats)))

(defun niwa-collaboration--artifact-stat-invalidate ()
  "Drop every cached artifact stat and display metadata record."
  (clrhash niwa-collaboration--artifact-stats)
  (clrhash niwa-collaboration--artifact-metadata))

(defun niwa-collaboration--artifact-title (session relative)
  "Return the current display title of SESSION's RELATIVE artifact file.
A secondary file adds its relative filename.  Cache metadata, including
absence, until the store changes, so live and archived cards share titles
without target I/O on streaming projection ticks."
  (when-let* ((slash (string-search "/" relative))
              (store (substring relative 0 slash))
              (dir (niwa-collaboration--artifacts-dir session))
              (key (file-name-concat dir store)))
    (let* ((meta (or (gethash key niwa-collaboration--artifact-metadata)
                     (puthash key
                              (or (niwa-artifact-store-meta
                                   (niwa-session-workspace session) store)
                                  'absent)
                              niwa-collaboration--artifact-metadata)))
           (title (and (listp meta) (plist-get meta :title)))
           (file (substring relative (1+ slash))))
      (when (and title (not (string-blank-p title)))
        (if (equal file (plist-get meta :file)) title
          (format "%s · %s" title file))))))

(defun niwa-collaboration--remote-artifact-field (session file)
  "Return the card of settled ApplyPatch FILE in a remote artifact, or nil.
FILE's path is shared://ALIAS/files/PATH of an artifact SESSION's
workspace has attached.  Its size is that of the bytes ApplyPatch wrote,
which FILE records."
  (pcase-let* ((path (or (plist-get file :move-path) (plist-get file :path)))
               (`(,alias . ,relative) (niwa-resource-remote-file path))
               (workspace (niwa-session-workspace session)))
    (when (and alias (not (eq (plist-get file :kind) 'delete))
               (niwa-artifact-backend-registered-p workspace alias))
      (let* ((row (cl-find alias (niwa-artifact-backend-catalog workspace 'items)
                           :key (lambda (row) (plist-get row :id)) :test #'equal))
             (title (plist-get row :title)))
        (append (list :artifact (concat alias "/" relative) :store alias
                      ;; Null until the origin announces a change.
                      :artifactRevision (plist-get row :revision))
                (when-let* ((size (plist-get file :size))) (list :size size))
                (when (and title (not (string-blank-p title)))
                  (list :title (format "%s · %s" title relative))))))))

(defun niwa-collaboration--artifact-field (session file base-directory)
  "Return artifact fields for settled ApplyPatch FILE, or nil.
SESSION supplies the artifact root and execution target.  BASE-DIRECTORY is
the directory against which ApplyPatch resolved relative paths.  Move records
publish their destination; deletes publish no new card, while an older card
for the deleted path will re-project as missing."
  (if (string-prefix-p "shared://" (or (plist-get file :move-path) (plist-get file :path) ""))
      (niwa-collaboration--remote-artifact-field session file)
    (niwa-collaboration--local-artifact-field session file base-directory)))

(defun niwa-collaboration--local-artifact-field (session file base-directory)
  "Return artifact fields for settled ApplyPatch FILE in the local store.
See `niwa-collaboration--artifact-field' for SESSION and BASE-DIRECTORY."
  (let ((session-target (niwa-session-execution-target session)))
    (when-let* (((not (eq (plist-get file :kind) 'delete)))
                (path (or (plist-get file :move-path)
                          (plist-get file :path)))
                ((stringp path))
                (dir (niwa-collaboration--artifacts-dir session))
                (target (or session-target
                            (niwa-execution-target-create dir)))
                (full (ignore-errors
                        (niwa-execution-target-expand-path
                         target path (if session-target base-directory dir))))
                (relative (and full (niwa-artifact-store-relative
                                     (niwa-session-workspace session) full)))
                ((and (not (string-empty-p relative))
                      ;; Host bookkeeping is not an artifact.
                      (not (string-prefix-p ".state/" relative)))))
      (let* ((stat (niwa-collaboration--artifact-stat full))
             (slash (string-search "/" relative))
             (store (and slash (substring relative 0 slash)))
             (title (niwa-collaboration--artifact-title session relative)))
        (append (list :artifact relative :artifact-path full)
                ;; The store artifact the card belongs to.
                (when store (list :store store))
                (when title (list :title title))
                (if (null (car stat))
                    (list :missing t)
                  (list :size (car stat) :artifactRevision (cdr stat))))))))

(defun niwa-collaboration--artifact-fields (render-data)
  "Return artifact field plists from settled ApplyPatch RENDER-DATA.
Runs in the projected transcript buffer so `default-directory' matches the
path base used by the tool.  Persisted render data contains selected applied
files only, making it authoritative over the authored patch arguments."
  (when-let* ((session (bound-and-true-p niwa--session))
              ((eq (plist-get render-data :kind) 'patch)))
    (delq nil
          (mapcar (lambda (file)
                    (niwa-collaboration--artifact-field
                     session file default-directory))
                  (plist-get render-data :files)))))

(provide 'niwa-collaboration-artifact-projection)
;;; niwa-collaboration-artifact-projection.el ends here
