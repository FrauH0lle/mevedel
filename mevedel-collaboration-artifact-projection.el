;;; mevedel-collaboration-artifact-projection.el --- artifact projection -*- lexical-binding: t; -*-

;;; Commentary:

;; Maps settled ApplyPatch render data onto session artifact cards.  This is
;; the path-domain and file-stat leaf used by collaboration projection; file
;; bytes remain host-only until an authenticated guest requests a published
;; record id.

;;; Code:

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifacts-dir
                  "mevedel-session-artifacts" (save-path))

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

(defun mevedel-collaboration--artifacts-dir (session)
  "Return SESSION's expanded artifacts directory with trailing slash, or nil."
  (when-let* ((save-path (and session (mevedel-session-save-path session))))
    (file-name-as-directory
     (expand-file-name
      (mevedel-session-artifacts-artifacts-dir save-path)))))

(defvar mevedel-collaboration--artifact-stats (make-hash-table :test #'equal)
  "Cached (SIZE . MISSING-P) per published artifact's qualified path.
Projection runs on every coalesced publish tick, so a remote session
would otherwise pay one target round trip per artifact per tick.  The
small cache is cleared whenever ApplyPatch settles or the artifact
cockpit changes the folder.")

(defun mevedel-collaboration--artifact-stat (path)
  "Return cached (SIZE . MISSING-P) for the artifact at qualified PATH."
  (or (gethash path mevedel-collaboration--artifact-stats)
      (puthash path
               (if-let* ((attributes (file-attributes path)))
                   (cons (file-attribute-size attributes) nil)
                 (cons nil t))
               mevedel-collaboration--artifact-stats)))

(defun mevedel-collaboration--artifact-stat-invalidate ()
  "Drop every cached artifact stat."
  (clrhash mevedel-collaboration--artifact-stats))

(defun mevedel-collaboration--artifact-field (session file base-directory)
  "Return artifact fields for settled ApplyPatch FILE, or nil.
SESSION supplies the artifact root and execution target.  BASE-DIRECTORY is
the directory against which ApplyPatch resolved relative paths.  Move records
publish their destination; deletes publish no new card, while an older card
for the deleted path will re-project as missing."
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
                (relative (and full (file-relative-name full dir)))
                ((and relative
                      (not (equal relative "."))
                      (not (equal relative ".."))
                      (not (string-prefix-p
                            (file-name-as-directory "..") relative)))))
      (let ((stat (mevedel-collaboration--artifact-stat full)))
        (append (list :artifact relative :artifact-path full)
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
