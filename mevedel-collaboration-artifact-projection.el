;;; mevedel-collaboration-artifact-projection.el --- artifact projection -*- lexical-binding: t; -*-

;;; Commentary:

;; Maps settled ApplyPatch render data onto artifact cards for writes into
;; the workspace artifact store.  This is
;; the path-domain and file-stat leaf used by collaboration projection; file
;; bytes remain host-only until an authenticated guest requests a published
;; record id.  A write to a remote artifact's file, shared://ALIAS/files/PATH,
;; projects a card for that artifact too; its origin serves the bytes.

;;; Code:

(require 'subr-x)

;; `mevedel-artifact-backend'
(declare-function mevedel-artifact-backend-catalog "mevedel-artifact-backend"
                  (workspace &optional detail))
(declare-function mevedel-artifact-backend-registered-p "mevedel-artifact-backend"
                  (workspace id))
(autoload 'mevedel-artifact-backend-catalog "mevedel-artifact-backend")
(autoload 'mevedel-artifact-backend-registered-p "mevedel-artifact-backend")

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-directory
                  "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-meta
                  "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-relative
                  "mevedel-artifact-store" (workspace path))
(autoload 'mevedel-artifact-store-directory "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-meta "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-relative "mevedel-artifact-store")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")

;; `mevedel-resource'
(declare-function mevedel-resource-remote-file "mevedel-resource" (address))
(autoload 'mevedel-resource-remote-file "mevedel-resource")

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

(defun mevedel-collaboration--artifacts-dir (session)
  "Return SESSION's expanded artifact store directory, or nil."
  (when-let* ((workspace (and session (mevedel-session-workspace session))))
    (expand-file-name (mevedel-artifact-store-directory workspace))))

(defvar mevedel-collaboration--artifact-stats (make-hash-table :test #'equal)
  "Cached (SIZE . MISSING-P) per published artifact's qualified path.
Projection runs on every coalesced publish tick, so a remote session
would otherwise pay one target round trip per artifact per tick.  The
small cache is cleared when ApplyPatch settles and whenever the artifact
store changes.")

(defvar mevedel-collaboration--artifact-metadata (make-hash-table :test #'equal)
  "Cached metadata per qualified store item directory, or `absent'.
Titles change on store notifications, not on streaming projection ticks.")

(defun mevedel-collaboration--artifact-stat (path)
  "Return cached (SIZE . MISSING-P) for the artifact at qualified PATH."
  (or (gethash path mevedel-collaboration--artifact-stats)
      (puthash path
               (if-let* ((attributes (file-attributes path)))
                   (cons (file-attribute-size attributes) nil)
                 (cons nil t))
               mevedel-collaboration--artifact-stats)))

(defun mevedel-collaboration--artifact-stat-invalidate ()
  "Drop every cached artifact stat and display metadata record."
  (clrhash mevedel-collaboration--artifact-stats)
  (clrhash mevedel-collaboration--artifact-metadata))

(defun mevedel-collaboration--artifact-title (session relative)
  "Return the current display title of SESSION's RELATIVE artifact file.
A secondary file adds its relative filename.  Cache metadata, including
absence, until the store changes, so live and archived cards share titles
without target I/O on streaming projection ticks."
  (when-let* ((slash (string-search "/" relative))
              (store (substring relative 0 slash))
              (dir (mevedel-collaboration--artifacts-dir session))
              (key (file-name-concat dir store)))
    (let* ((meta (or (gethash key mevedel-collaboration--artifact-metadata)
                     (puthash key
                              (or (mevedel-artifact-store-meta
                                   (mevedel-session-workspace session) store)
                                  'absent)
                              mevedel-collaboration--artifact-metadata)))
           (title (and (listp meta) (plist-get meta :title)))
           (file (substring relative (1+ slash))))
      (when (and title (not (string-blank-p title)))
        (if (equal file (plist-get meta :file)) title
          (format "%s · %s" title file))))))

(defun mevedel-collaboration--remote-artifact-field (session file)
  "Return the card of settled ApplyPatch FILE in a remote artifact, or nil.
FILE's path is shared://ALIAS/files/PATH of an artifact SESSION's
workspace has attached; the card carries no size, which only its origin
knows."
  (pcase-let* ((path (or (plist-get file :move-path) (plist-get file :path)))
               (`(,alias . ,relative) (mevedel-resource-remote-file path))
               (workspace (mevedel-session-workspace session)))
    (when (and alias (not (eq (plist-get file :kind) 'delete))
               (mevedel-artifact-backend-registered-p workspace alias))
      (let ((title (plist-get (cl-find alias (mevedel-artifact-backend-catalog workspace 'items)
                                       :key (lambda (row) (plist-get row :id)) :test #'equal)
                              :title)))
        (append (list :artifact (concat alias "/" relative) :store alias)
                (when (and title (not (string-blank-p title)))
                  (list :title (format "%s · %s" title relative))))))))

(defun mevedel-collaboration--artifact-field (session file base-directory)
  "Return artifact fields for settled ApplyPatch FILE, or nil.
SESSION supplies the artifact root and execution target.  BASE-DIRECTORY is
the directory against which ApplyPatch resolved relative paths.  Move records
publish their destination; deletes publish no new card, while an older card
for the deleted path will re-project as missing."
  (if (string-prefix-p "shared://" (or (plist-get file :move-path) (plist-get file :path) ""))
      (mevedel-collaboration--remote-artifact-field session file)
    (mevedel-collaboration--local-artifact-field session file base-directory)))

(defun mevedel-collaboration--local-artifact-field (session file base-directory)
  "Return artifact fields for settled ApplyPatch FILE in the local store.
See `mevedel-collaboration--artifact-field' for SESSION and BASE-DIRECTORY."
  (let ((session-target (mevedel-session-execution-target session)))
    (when-let* (((not (eq (plist-get file :kind) 'delete)))
                (path (or (plist-get file :move-path)
                          (plist-get file :path)))
                ((stringp path))
                (dir (mevedel-collaboration--artifacts-dir session))
                (target (or session-target
                            (mevedel-execution-target-create dir)))
                (full (ignore-errors
                        (mevedel-execution-target-expand-path
                         target path (if session-target base-directory dir))))
                (relative (and full (mevedel-artifact-store-relative
                                     (mevedel-session-workspace session) full)))
                ((and (not (string-empty-p relative))
                      ;; Host bookkeeping is not an artifact.
                      (not (string-prefix-p ".state/" relative)))))
      (let* ((stat (mevedel-collaboration--artifact-stat full))
             (slash (string-search "/" relative))
             (store (and slash (substring relative 0 slash)))
             (title (mevedel-collaboration--artifact-title session relative)))
        (append (list :artifact relative :artifact-path full)
                ;; The store artifact the card belongs to.
                (when store (list :store store))
                (when title (list :title title))
                (if (cdr stat)
                    (list :missing t)
                  (list :size (car stat))))))))

(defun mevedel-collaboration--artifact-fields (render-data)
  "Return artifact field plists from settled ApplyPatch RENDER-DATA.
Runs in the projected transcript buffer so `default-directory' matches the
path base used by the tool.  Persisted render data contains selected applied
files only, making it authoritative over the authored patch arguments."
  (when-let* ((session (bound-and-true-p mevedel--session))
              ((eq (plist-get render-data :kind) 'patch)))
    (delq nil
          (mapcar (lambda (file)
                    (mevedel-collaboration--artifact-field
                     session file default-directory))
                  (plist-get render-data :files)))))

(provide 'mevedel-collaboration-artifact-projection)
;;; mevedel-collaboration-artifact-projection.el ends here
