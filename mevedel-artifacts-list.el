;;; mevedel-artifacts-list.el --- Session artifacts cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated browser for a session's artifacts: the files under
;; `<save-path>/artifacts/' and the shared whiteboards and documents.  The
;; cockpit lists, opens, and deletes them; a live collaboration room learns of
;; each deletion.  Items keep their state below `shared-editing/', so that
;; subtree lists as items, never as files.

;;; Code:

(require 'mevedel-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-data-buffer "mevedel-cockpit" (&optional context))
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
(autoload 'mevedel-cockpit-quit "mevedel-cockpit" nil t)
(autoload 'mevedel-cockpit-setup-tabulated-surface "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-context "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-refresh "mevedel-cockpit" nil t)
(autoload 'mevedel-cockpit-surface-selected "mevedel-cockpit")

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration-delete-artifact
                  "mevedel-collaboration-artifact" (session name))
(autoload 'mevedel-collaboration-delete-artifact "mevedel-collaboration-artifact")
;; `mevedel-collaboration'
(declare-function mevedel-collaboration-open-shared-item
                  "mevedel-collaboration" (data-buffer id))
(autoload 'mevedel-collaboration-open-shared-item "mevedel-collaboration")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (session args callback &optional authorize commit))
(declare-function mevedel-shared-editing-ids "mevedel-shared-editing" (session))
(declare-function mevedel-shared-editing-list "mevedel-shared-editing" (session))
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-ids "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-list "mevedel-shared-editing")

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

(defun mevedel-artifacts-list--file-paths (dir)
  "Return the artifact files under DIR, without the shared items' state."
  (cl-remove-if (lambda (path)
                  (string-prefix-p "shared-editing/" (file-relative-name path dir)))
                (directory-files-recursively dir ".*")))

(defun mevedel-artifacts-list--files (session)
  "Return SESSION's artifact file plists, newest modification first."
  (when-let* ((dir (mevedel-artifacts-list--directory session))
              ((file-directory-p dir)))
    (let (items)
      (dolist (path (mevedel-artifacts-list--file-paths dir))
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

(defun mevedel-artifacts-list--items (session)
  "Return SESSION's shared whiteboards and documents as artifact rows."
  (when (mevedel-session-save-path session)
    (mapcar (lambda (item)
              (list :name (format "%s · %s"
                                  (if (equal (plist-get item :kind) "whiteboard")
                                      "Whiteboard" "Document")
                                  (plist-get item :title))
                    :item (plist-get item :id)
                    :path (concat "item:" (plist-get item :id))
                    :kind (plist-get item :kind)
                    :title (plist-get item :title)))
            (mevedel-shared-editing-list session))))

(defun mevedel-artifacts-list-count (session)
  "Return how many artifacts SESSION has, best effort."
  (or (ignore-errors
        (when-let* ((directory (mevedel-artifacts-list--directory session))
                    ((file-directory-p directory)))
          ;; Item ids only: reading every item's state is not a count.
          (+ (length (mevedel-artifacts-list--file-paths directory))
             (length (mevedel-shared-editing-ids session)))))
      0))

(defun mevedel-artifacts-list--collect (context)
  "Collect artifact rows for CONTEXT: shared items, then files."
  (let ((session (mevedel-artifacts-list--session context)))
    (append (mevedel-artifacts-list--items session)
            (mevedel-artifacts-list--files session))))

(defun mevedel-artifacts-list--entry (item _context)
  "Return tabulated row for artifact ITEM."
  (list
   (plist-get item :path)
   (vector
    (plist-get item :name)
    (if (plist-get item :item) ""
      (file-size-human-readable (or (plist-get item :size) 0)))
    (if (plist-get item :item) ""
      (format-time-string "%Y-%m-%d %H:%M" (plist-get item :modified))))))

(defun mevedel-artifacts-list--header (items context)
  "Return cockpit header for artifact ITEMS and CONTEXT."
  (let ((session (mevedel-cockpit-context-session context)))
    (mevedel-cockpit-format-header
     "artifacts"
     (if session (mevedel-session-name session) "")
     (format "%d file%s" (length items)
             (if (= 1 (length items)) "" "s")))))

(defun mevedel-artifacts-list--details (item _context)
  "Return the information report for artifact ITEM."
  (if (plist-get item :item)
      (list :title (if (equal (plist-get item :kind) "whiteboard") "Whiteboard" "Document")
            :subtitle (plist-get item :title)
            :identity (plist-get item :item)
            :sections
            (list (list :id 'item :title "Shared item"
                        :body (mevedel-report-fields
                               (list "Title" (plist-get item :title))
                               (list "Id" (plist-get item :item))))))
    (list :title "Artifact" :subtitle (plist-get item :name)
          :identity (plist-get item :path)
          :sections
          (list
           (list :id 'artifact :title "Artifact"
                 :body (mevedel-report-fields
                        (list "Name" (plist-get item :name))
                        (list "Size" (file-size-human-readable (or (plist-get item :size) 0)))
                        (list "Modified" (format-time-string "%Y-%m-%d %H:%M:%S" (plist-get item :modified)))))
           (list :id 'location :title "Location"
                 :body (mevedel-report-fields (list "Path" (plist-get item :path))))))))

(defun mevedel-artifacts-list--selected-path ()
  "Return the selected artifact's still-existing path."
  (let* ((item (mevedel-cockpit-surface-selected))
         (path (plist-get item :path)))
    (when (plist-get item :item)
      (user-error "Whiteboards and documents have no file; open them with o"))
    (unless (and path (file-exists-p path))
      (mevedel-cockpit-surface-refresh)
      (user-error "Artifact file no longer exists"))
    path))

(defun mevedel-artifacts-list-open-browser ()
  "Open the selected artifact in a web browser.
A whiteboard or document opens in the session's room, which is started
after confirmation when the session is not shared.  A remote artifact
file is visited in Emacs instead: the local browser cannot read the
target's filesystem."
  (interactive)
  (if-let* ((id (plist-get (mevedel-cockpit-surface-selected) :item)))
      (mevedel-collaboration-open-shared-item
       (mevedel-cockpit-context-data-buffer (mevedel-cockpit-surface-context))
       id)
    (let ((path (mevedel-artifacts-list--selected-path)))
      (if (file-remote-p path)
          (find-file path)
        (browse-url-of-file path)))))

(defun mevedel-artifacts-list-visit ()
  "Visit the selected artifact file in Emacs."
  (interactive)
  (find-file (mevedel-artifacts-list--selected-path)))

(defun mevedel-artifacts-list-delete ()
  "Delete the selected artifact, which also unpublishes it.
A whiteboard or document is deleted as a shared item, after any save in
progress, together with its comments and history."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-artifacts-list--session context))
         (item (mevedel-cockpit-surface-selected))
         (id (plist-get item :item)))
    (if id
        (when (yes-or-no-p (format "Delete %s with its comments and history? "
                                   (plist-get item :name)))
          (let ((buffer (current-buffer)))
            (mevedel-shared-editing-call
             session (list :action "delete" :id id :actor "Host")
             (lambda (reply)
               (if-let* ((failure (plist-get reply :error)))
                   (message "mevedel: %s was not deleted: %s" (plist-get item :name) failure)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer (mevedel-cockpit-surface-refresh)))
                 (message "mevedel: %s deleted" (plist-get item :name)))))))
      (let ((path (mevedel-artifacts-list--selected-path)))
        (when (yes-or-no-p (format "Delete artifact %s? " (plist-get item :name)))
          (mevedel-collaboration-delete-artifact
           session (file-relative-name path (mevedel-artifacts-list--directory session)))
          (mevedel-cockpit-surface-refresh)
          (message "mevedel: artifact %s deleted" (plist-get item :name)))))))

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
    :keys (("o" "Open the selected artifact or shared item in a browser"
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
