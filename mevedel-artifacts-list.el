;;; mevedel-artifacts-list.el --- Artifacts cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated browser for the workspace artifact store, usable from a session
;; or from any project buffer (`mevedel-artifacts').  It lists, opens,
;; versions, duplicates and deletes store artifacts -- files, whiteboards and
;; documents -- and attaches them to the current session; live collaboration
;; rooms of the workspace learn of each change.

;;; Code:

(require 'mevedel-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-attach
                  "mevedel-artifact-store" (session id &optional buffer))
(declare-function mevedel-artifact-store-conversation
                  "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-directory
                  "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-duplicate
                  "mevedel-artifact-store" (workspace id new-id))
(declare-function mevedel-artifact-store-ids
                  "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-list
                  "mevedel-artifact-store" (workspace))
(declare-function mevedel-artifact-store-restore-version
                  "mevedel-artifact-store" (workspace id n &optional session-id callback))
(declare-function mevedel-artifact-store-version-path
                  "mevedel-artifact-store" (workspace id n))
(declare-function mevedel-artifact-store-versions
                  "mevedel-artifact-store" (workspace id))
(autoload 'mevedel-artifact-store-attach "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-conversation "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-directory "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-duplicate "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-ids "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-list "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-restore-version "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-version-path "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-versions "mevedel-artifact-store")
(defvar mevedel-artifact-store-item-kinds)

;; `mevedel-chat'
(declare-function mevedel--display-chat-buffer "mevedel-chat" (chat-buffer))
(autoload 'mevedel--display-chat-buffer "mevedel-chat")

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-for-buffer "mevedel-cockpit" (buffer))
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-workspace
                  "mevedel-cockpit" (&optional context))
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
(declare-function mevedel-cockpit-workspace-context
                  "mevedel-cockpit" (workspace &optional origin-buffer))
(autoload 'mevedel-cockpit-context-for-buffer "mevedel-cockpit")
(autoload 'mevedel-cockpit-context-session "mevedel-cockpit")
(autoload 'mevedel-cockpit-context-workspace "mevedel-cockpit")
(autoload 'mevedel-cockpit-format-header "mevedel-cockpit")
(autoload 'mevedel-cockpit-open-surface "mevedel-cockpit")
(autoload 'mevedel-cockpit-quit "mevedel-cockpit" nil t)
(autoload 'mevedel-cockpit-setup-tabulated-surface "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-context "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-refresh "mevedel-cockpit" nil t)
(autoload 'mevedel-cockpit-surface-selected "mevedel-cockpit")
(autoload 'mevedel-cockpit-workspace-context "mevedel-cockpit")

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration-delete-artifact
                  "mevedel-collaboration-artifact" (workspace name &optional callback))
(declare-function mevedel-collaboration-notify-artifacts-changed
                  "mevedel-collaboration-artifact" (workspace))
(autoload 'mevedel-collaboration-delete-artifact "mevedel-collaboration-artifact")
(autoload 'mevedel-collaboration-notify-artifacts-changed
  "mevedel-collaboration-artifact")
;; `mevedel-collaboration'
(declare-function mevedel-collaboration-open-shared-item
                  "mevedel-collaboration" (data-buffer id))
(autoload 'mevedel-collaboration-open-shared-item "mevedel-collaboration")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function mevedel-shared-editing-save-version
                  "mevedel-shared-editing" (workspace id &optional session-id))
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-save-version "mevedel-shared-editing")

;; `mevedel-structs'
(declare-function mevedel-session-attached-artifacts "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(autoload 'mevedel-workspace "mevedel-workspace")

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())

(defconst mevedel-artifacts-list-buffer-name "*mevedel artifacts*"
  "Name of the artifacts cockpit buffer.")

(defvar-local mevedel-artifacts-list--attached-only nil
  "Non-nil when the cockpit lists only the session's attached artifacts.")

(defun mevedel-artifacts-list--workspace (&optional context)
  "Return the artifacts cockpit workspace for CONTEXT."
  (or (mevedel-cockpit-context-workspace
       (or context (mevedel-cockpit-surface-context)))
      (user-error "No mevedel workspace here")))

(defun mevedel-artifacts-list--store-rows (workspace session)
  "Return WORKSPACE's store artifacts as rows, marking SESSION's attached."
  (let ((attached (and session (mevedel-session-attached-artifacts session))))
    (mapcar (lambda (row)
              (append (list :name (plist-get row :id)
                            :row (plist-get row :id)
                            :attached (and (member (plist-get row :id) attached) t))
                      ;; A whiteboard or document is edited live, not as a file.
                      (when (memq (plist-get row :kind) mevedel-artifact-store-item-kinds)
                        (list :item (plist-get row :id)))
                      row))
            (mevedel-artifact-store-list workspace))))

(defun mevedel-artifacts-list-count (session)
  "Return (ATTACHED . IN-PROJECT) artifact counts for SESSION, best effort."
  (or (ignore-errors
        ;; Ids only: reading every artifact's state is not a count.
        (let ((ids (mevedel-artifact-store-ids (mevedel-session-workspace session))))
          (cons (cl-count-if (lambda (id) (member id ids))
                             (mevedel-session-attached-artifacts session))
                (length ids))))
      (cons 0 0)))

(defun mevedel-artifacts-list--collect (context)
  "Collect artifact rows for CONTEXT's workspace store."
  (let ((session (mevedel-cockpit-context-session context)))
    (cl-remove-if-not
     (lambda (row) (or (not mevedel-artifacts-list--attached-only)
                       (plist-get row :attached)))
     (mevedel-artifacts-list--store-rows
      (mevedel-artifacts-list--workspace context) session))))

(defun mevedel-artifacts-list--entry (item _context)
  "Return tabulated row for artifact ITEM."
  (list
   (plist-get item :row)
   (vector
    (plist-get item :name)
    (format "%s" (or (plist-get item :kind) ""))
    (if (plist-get item :attached) "✓" "")
    (number-to-string (plist-get item :versions))
    (if (plist-get item :missing) "missing"
      (format-time-string "%Y-%m-%d %H:%M" (plist-get item :modified)))
    (file-size-human-readable (or (plist-get item :size) 0)))))

(defun mevedel-artifacts-list--header (items context)
  "Return cockpit header for artifact ITEMS and CONTEXT."
  (let ((session (mevedel-cockpit-context-session context)))
    (mevedel-cockpit-format-header
     "artifacts"
     (if session
         (mevedel-session-name session)
       (mevedel-workspace-name (mevedel-artifacts-list--workspace context)))
     (format "%d artifact%s%s" (length items)
             (if (= 1 (length items)) "" "s")
             (if mevedel-artifacts-list--attached-only " (attached)" "")))))

(defun mevedel-artifacts-list--details (item _context)
  "Return the information report for artifact ITEM."
  (list :title "Artifact" :subtitle (plist-get item :id)
        :identity (plist-get item :id)
        :sections
        (list
         (list :id 'artifact :title "Artifact"
               :body (mevedel-report-fields
                      (list "Id" (plist-get item :id))
                      (list "Title" (plist-get item :title))
                      (list "Kind" (format "%s" (plist-get item :kind)))
                      (list "Attached" (if (plist-get item :attached) "yes" "no"))
                      (list "Versions" (number-to-string (plist-get item :versions)))
                      (list "Size" (file-size-human-readable (or (plist-get item :size) 0)))
                      (list "Modified"
                            (if (plist-get item :missing)
                                "file missing"
                              (format-time-string "%Y-%m-%d %H:%M:%S"
                                                  (plist-get item :modified))))))
         (list :id 'location :title "Location"
               :body (mevedel-report-fields (list "Path" (plist-get item :path)))))))

(defun mevedel-artifacts-list--selected-artifact ()
  "Return the selected store artifact row."
  (mevedel-cockpit-surface-selected))

(defun mevedel-artifacts-list--selected-path ()
  "Return the selected artifact's still-existing path."
  (let ((item (mevedel-artifacts-list--selected-artifact)))
    (when (plist-get item :item)
      (user-error "Whiteboards and documents open in their editor; use o"))
    (unless (and (plist-get item :path) (file-exists-p (plist-get item :path)))
      (mevedel-cockpit-surface-refresh)
      (user-error "Artifact file no longer exists"))
    (plist-get item :path)))

(defun mevedel-artifacts-list--browse (path)
  "Show artifact file PATH: in a browser, or in Emacs when it is remote.
The local browser cannot read a remote target's filesystem."
  (if (file-remote-p path)
      (find-file path)
    (browse-url-of-file path)))

(defun mevedel-artifacts-list--changed (workspace &optional selected)
  "Refresh after a change to WORKSPACE's store, keeping SELECTED.
The store itself tells the workspace's rooms."
  (ignore workspace)
  (mevedel-cockpit-surface-refresh selected))

(defun mevedel-artifacts-list-open-browser ()
  "Open the selected artifact in a web browser.
A whiteboard or document opens in its editor in the session's room, which
is started after confirmation when the session is not shared.  Without a
session it opens in the room of the item's own conversation."
  (interactive)
  (let ((context (mevedel-cockpit-surface-context)))
    (if-let* ((id (plist-get (mevedel-cockpit-surface-selected) :item)))
        (mevedel-collaboration-open-shared-item
         (or (mevedel-cockpit-context-data-buffer context)
             (mevedel-artifact-store-conversation
              (mevedel-artifacts-list--workspace context) id))
         id)
      (mevedel-artifacts-list--browse (mevedel-artifacts-list--selected-path)))))

(defun mevedel-artifacts-list-visit ()
  "Visit the selected artifact file in Emacs."
  (interactive)
  (find-file (mevedel-artifacts-list--selected-path)))

(defun mevedel-artifacts-list-attach ()
  "Attach the selected artifact to the current session."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (or (mevedel-cockpit-context-session context)
                      (user-error "Attaching needs a session; open the cockpit from one")))
         (id (plist-get (mevedel-artifacts-list--selected-artifact) :id)))
    (mevedel-artifact-store-attach
     session id (mevedel-cockpit-context-data-buffer context))
    (mevedel-cockpit-surface-refresh id)
    (message "mevedel: %s attached to %s" id (mevedel-session-name session))))

(defun mevedel-artifacts-list-toggle-attached ()
  "Toggle between all artifacts and the session's attached ones."
  (interactive)
  (setq mevedel-artifacts-list--attached-only
        (not mevedel-artifacts-list--attached-only))
  (mevedel-cockpit-surface-refresh))

(defun mevedel-artifacts-list-versions ()
  "View or restore a version of the selected artifact.
Restoring copies the version over the artifact as a new version."
  (interactive)
  (let* ((buffer (current-buffer))
         (context (mevedel-cockpit-surface-context))
         (workspace (mevedel-artifacts-list--workspace context))
         (session (mevedel-cockpit-context-session context))
         (id (plist-get (mevedel-artifacts-list--selected-artifact) :id))
         (choices
          (mapcar (lambda (row)
                    (cons (format "%d  %s  %s" (plist-get row :n)
                                  (format-time-string "%Y-%m-%d %H:%M"
                                                      (plist-get row :time))
                                  (file-size-human-readable (plist-get row :bytes)))
                          (plist-get row :n)))
                  (reverse (mevedel-artifact-store-versions workspace id))))
         (n (cdr (assoc (completing-read (format "Version of %s: " id)
                                         choices nil t)
                        choices))))
    (pcase (if (plist-get (mevedel-artifacts-list--selected-artifact) :item)
               ;; A whiteboard's or document's version opens only by restoring.
               (and (y-or-n-p (format "Restore version %d of %s? " n id)) ?r)
             (car (read-multiple-choice
                   (format "Version %d" n)
                   '((?v "view" "Open this version")
                     (?r "restore" "Restore it as the newest version")))))
      (?v (mevedel-artifacts-list--browse
           (mevedel-artifact-store-version-path workspace id n)))
      (?r (mevedel-artifact-store-restore-version
           workspace id n
           (and session (mevedel-session-session-id session))
           (lambda (reply)
             (if (plist-get reply :error)
                 (message "mevedel: %s was not restored: %s"
                          id (plist-get reply :error))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (mevedel-artifacts-list--changed workspace id)))
               (message "mevedel: %s restored from version %d as version %d"
                        id n (plist-get reply :n)))))))))

(defun mevedel-artifacts-list-save-version ()
  "Save the selected whiteboard or document as a new version."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (workspace (mevedel-artifacts-list--workspace context))
         (session (mevedel-cockpit-context-session context))
         (id (or (plist-get (mevedel-artifacts-list--selected-artifact) :item)
                 (user-error "File artifacts keep a version of every saved change")))
         (n (mevedel-shared-editing-save-version
             workspace id (and session (mevedel-session-session-id session)))))
    (mevedel-artifacts-list--changed workspace id)
    (message "mevedel: saved %s as version %d" id n)))

(defun mevedel-artifacts-list-duplicate ()
  "Copy the selected artifact into a new, independent artifact."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (workspace (mevedel-artifacts-list--workspace context))
         (session (mevedel-cockpit-context-session context))
         (id (plist-get (mevedel-artifacts-list--selected-artifact) :id))
         (new-id (read-string "New artifact id: " (concat id "-copy"))))
    (mevedel-artifact-store-duplicate workspace id new-id)
    (when session
      (mevedel-artifact-store-attach
       session new-id (mevedel-cockpit-context-data-buffer context)))
    (mevedel-artifacts-list--changed workspace new-id)
    (message "mevedel: %s duplicated as %s" id new-id)))

(defun mevedel-artifacts-list-conversation ()
  "Open the selected artifact's own conversation, creating it on first use."
  (interactive)
  (let ((workspace (mevedel-artifacts-list--workspace))
        (id (plist-get (mevedel-artifacts-list--selected-artifact) :id)))
    (mevedel--display-chat-buffer
     (mevedel-artifact-store-conversation workspace id))))

(defun mevedel-artifacts-list-delete ()
  "Delete the selected artifact with its versions, which also unpublishes it.
A whiteboard or document is deleted as a shared item, after any save in
progress, together with its comments and history."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (workspace (mevedel-artifacts-list--workspace context))
         (item (mevedel-cockpit-surface-selected))
         (name (or (plist-get item :title) (plist-get item :id)))
         (buffer (current-buffer)))
    (when (yes-or-no-p (format "Delete %s with its versions, comments and conversation? "
                               name))
      (mevedel-collaboration-delete-artifact
       workspace
       (file-name-concat (plist-get item :id) (plist-get item :file))
       (lambda (failure)
         (if failure
             (message "mevedel: %s was not deleted: %s" name failure)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (mevedel-cockpit-surface-refresh)))
           (message "mevedel: %s deleted" name)))))))

(defun mevedel-artifacts-list-quit ()
  "Quit the artifacts cockpit."
  (interactive)
  (mevedel-cockpit-quit "artifacts cockpit"))

(defconst mevedel-artifacts-list--surface
  `(:buffer-name ,mevedel-artifacts-list-buffer-name
    :label "artifacts cockpit"
    :row-label "artifact"
    :mode mevedel-artifacts-list-mode
    :format [("Artifact" 32 t)
             ("Kind" 10 t)
             ("Att" 4 t)
             ("Ver" 4 t)
             ("Modified" 17 t)
             ("Size" 0 t)]
    :sort-key ("Modified" . t)
    :collect mevedel-artifacts-list--collect
    :entry mevedel-artifacts-list--entry
    :header mevedel-artifacts-list--header
    :details mevedel-artifacts-list--details
    :details-buffer "*mevedel artifact details*"
    :keys (("o" "Open the selected artifact or shared item in a browser"
            mevedel-artifacts-list-open-browser)
           ("e" "Visit the selected artifact file in Emacs"
            mevedel-artifacts-list-visit)
           ("a" "Attach the selected artifact to this session"
            mevedel-artifacts-list-attach)
           ("t" "Toggle between all and attached artifacts"
            mevedel-artifacts-list-toggle-attached)
           ("v" "View or restore a version of the selected artifact"
            mevedel-artifacts-list-versions)
           ("D" "Duplicate the selected artifact"
            mevedel-artifacts-list-duplicate)
           ("c" "Open the selected artifact's own conversation"
            mevedel-artifacts-list-conversation)
           ("s" "Save the selected whiteboard or document as a version"
            mevedel-artifacts-list-save-version)
           ("d" "Delete (and unpublish) the selected artifact"
            mevedel-artifacts-list-delete)))
  "Cockpit surface spec for the artifact store.")

(define-derived-mode mevedel-artifacts-list-mode tabulated-list-mode
  "mevedel-artifacts"
  "Major mode for browsing the workspace artifact store."
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-artifacts-list--surface))

(defun mevedel-artifacts-list-open (&optional context)
  "Open the artifacts cockpit for CONTEXT.
Without CONTEXT, use the current session, or else the current buffer's
workspace."
  (let ((context
         (or context
             (mevedel-cockpit-context-for-buffer (current-buffer))
             (mevedel-cockpit-workspace-context
              (or (mevedel-workspace) (user-error "No mevedel workspace here"))
              (current-buffer)))))
    (mevedel-cockpit-open-surface mevedel-artifacts-list--surface context)))

;;;###autoload
(defun mevedel-artifacts ()
  "Browse the current workspace's artifact store."
  (interactive)
  (mevedel-artifacts-list-open))

(provide 'mevedel-artifacts-list)
;;; mevedel-artifacts-list.el ends here
