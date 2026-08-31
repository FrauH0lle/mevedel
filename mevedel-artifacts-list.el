;;; mevedel-artifacts-list.el --- Session artifacts cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated browser for files under `<save-path>/artifacts/'.  The cockpit
;; lists, opens, and deletes those files and tells a live collaboration room
;; when an existing conversation card has changed on disk.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context "mevedel-cockpit" ())
(declare-function mevedel-cockpit-format-header
                  "mevedel-cockpit" (name scope state))
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))
(autoload 'mevedel-cockpit-context-session "mevedel-cockpit")
(autoload 'mevedel-cockpit-current-context "mevedel-cockpit")
(autoload 'mevedel-cockpit-format-header "mevedel-cockpit")
(autoload 'mevedel-cockpit-open-surface "mevedel-cockpit")
(autoload 'mevedel-cockpit-quit "mevedel-cockpit")
(autoload 'mevedel-cockpit-setup-tabulated-surface "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-context "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-refresh "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-selected "mevedel-cockpit")

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration-notify-artifacts-changed
                  "mevedel-collaboration-artifact" (session))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifacts-dir
                  "mevedel-session-artifacts" (save-path))

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())

(defconst mevedel-artifacts-list-buffer-name "*mevedel artifacts*"
  "Name of the session artifacts cockpit buffer.")

(defun mevedel-artifacts-list--session (&optional context)
  "Return the artifacts cockpit session for CONTEXT."
  (or (mevedel-cockpit-context-session
       (or context (mevedel-cockpit-surface-context)))
      (user-error "No mevedel session in this buffer")))

(defun mevedel-artifacts-list--directory (session)
  "Return SESSION's artifacts directory, or nil without a save path."
  (when-let* ((save-path (and session (mevedel-session-save-path session))))
    (mevedel-session-artifacts-artifacts-dir save-path)))

(defun mevedel-artifacts-list--files (session)
  "Return SESSION's artifact file plists, newest modification first."
  (when-let* ((dir (mevedel-artifacts-list--directory session))
              ((file-directory-p dir)))
    (let (items)
      (dolist (path (directory-files-recursively dir ".*"))
        (when-let* ((attributes (file-attributes path)))
          (push (list :name (file-relative-name path dir)
                      :path path
                      :size (file-attribute-size attributes)
                      :modified (file-attribute-modification-time
                                 attributes))
                items)))
      (sort items (lambda (left right)
                    (time-less-p (plist-get right :modified)
                                 (plist-get left :modified)))))))

(defun mevedel-artifacts-list-count (session)
  "Return how many artifact files SESSION has, best effort."
  (length (ignore-errors (mevedel-artifacts-list--files session))))

(defun mevedel-artifacts-list--collect (context)
  "Collect artifact rows for CONTEXT."
  (mevedel-artifacts-list--files
   (mevedel-artifacts-list--session context)))

(defun mevedel-artifacts-list--entry (item _context)
  "Return tabulated row for artifact ITEM."
  (list
   (plist-get item :path)
   (vector
    (plist-get item :name)
    (file-size-human-readable (or (plist-get item :size) 0))
    (format-time-string "%Y-%m-%d %H:%M" (plist-get item :modified)))))

(defun mevedel-artifacts-list--header (items context)
  "Return cockpit header for artifact ITEMS and CONTEXT."
  (let ((session (mevedel-cockpit-context-session context)))
    (mevedel-cockpit-format-header
     "artifacts"
     (if session (mevedel-session-name session) "")
     (format "%d file%s" (length items)
             (if (= 1 (length items)) "" "s")))))

(defun mevedel-artifacts-list--details (item _context)
  "Return detail text for artifact ITEM."
  (format "Artifact: %s\nPath: %s\nSize: %s\nModified: %s\n"
          (plist-get item :name)
          (plist-get item :path)
          (file-size-human-readable (or (plist-get item :size) 0))
          (format-time-string "%Y-%m-%d %H:%M:%S"
                              (plist-get item :modified))))

(defun mevedel-artifacts-list--selected-path ()
  "Return the selected artifact's still-existing path."
  (let ((path (plist-get (mevedel-cockpit-surface-selected) :path)))
    (unless (and path (file-exists-p path))
      (mevedel-cockpit-surface-refresh)
      (user-error "Artifact file no longer exists"))
    path))

(defun mevedel-artifacts-list-open-browser ()
  "Open the selected artifact in a web browser.
A remote artifact file is visited in Emacs instead: the local browser
cannot read the target's filesystem."
  (interactive)
  (let ((path (mevedel-artifacts-list--selected-path)))
    (if (file-remote-p path)
        (find-file path)
      (browse-url-of-file path))))

(defun mevedel-artifacts-list-visit ()
  "Visit the selected artifact file in Emacs."
  (interactive)
  (find-file (mevedel-artifacts-list--selected-path)))

(defun mevedel-artifacts-list-delete ()
  "Delete the selected artifact, which also unpublishes it."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-artifacts-list--session context))
         (item (mevedel-cockpit-surface-selected))
         (path (mevedel-artifacts-list--selected-path)))
    (when (yes-or-no-p (format "Delete artifact %s? "
                               (plist-get item :name)))
      (delete-file path)
      (when (fboundp 'mevedel-collaboration-notify-artifacts-changed)
        (mevedel-collaboration-notify-artifacts-changed session))
      (mevedel-cockpit-surface-refresh)
      (message "mevedel: artifact %s deleted" (plist-get item :name)))))

(defun mevedel-artifacts-list-quit ()
  "Quit the artifacts cockpit and return to the session cockpit."
  (interactive)
  (mevedel-cockpit-quit "artifacts cockpit"))

(defconst mevedel-artifacts-list--surface
  `(:buffer-name ,mevedel-artifacts-list-buffer-name
    :label "artifacts cockpit"
    :row-label "artifact"
    :mode mevedel-artifacts-list-mode
    :format [("Artifact" 40 t)
             ("Size" 9 t)
             ("Modified" 0 t)]
    :sort-key ("Modified" . t)
    :require-session t
    :collect mevedel-artifacts-list--collect
    :entry mevedel-artifacts-list--entry
    :header mevedel-artifacts-list--header
    :details mevedel-artifacts-list--details
    :details-buffer "*mevedel artifact details*"
    :keys (("o" "Open the selected artifact in a browser"
            mevedel-artifacts-list-open-browser)
           ("e" "Visit the selected artifact file in Emacs"
            mevedel-artifacts-list-visit)
           ("d" "Delete (and unpublish) the selected artifact"
            mevedel-artifacts-list-delete)))
  "Cockpit surface spec for session artifacts.")

(define-derived-mode mevedel-artifacts-list-mode tabulated-list-mode
  "mevedel-artifacts"
  "Major mode for browsing and deleting session artifacts."
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-artifacts-list--surface))

(defun mevedel-artifacts-list-open (&optional context)
  "Open the session artifacts cockpit for CONTEXT."
  (interactive)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (mevedel-artifacts-list--session context)
    (mevedel-cockpit-open-surface mevedel-artifacts-list--surface context)))

(provide 'mevedel-artifacts-list)
;;; mevedel-artifacts-list.el ends here
