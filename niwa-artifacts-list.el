;;; niwa-artifacts-list.el --- Artifacts cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated browser for the workspace artifact store, usable from a session
;; or from any project buffer (`niwa-artifacts').  It lists, opens,
;; versions, duplicates and deletes store artifacts -- files, whiteboards and
;; documents -- and attaches them to the current session; live collaboration
;; rooms of the workspace learn of each change.  It also creates whiteboards
;; and documents (`niwa-artifacts-new-shared-item'), which open in their
;; browser editor.
;;
;; Remote artifacts attached to the workspace are listed too, marked with
;; their origin and whether it was reachable; they attach to sessions like
;; local ones, deleting one detaches it, and what only a local artifact has
;; (files, versions, a conversation, copies) is refused.

;;; Code:

(require 'niwa-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `niwa-artifact-backend'
(declare-function niwa-artifact-backend-catalog "niwa-artifact-backend"
                  (workspace &optional detail))
(autoload 'niwa-artifact-backend-catalog "niwa-artifact-backend")

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-attach
                  "niwa-artifact-store" (session id &optional buffer))
(declare-function niwa-artifact-store-conversation
                  "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-delete
                  "niwa-artifact-store" (workspace id &optional actor callback))
(declare-function niwa-artifact-store-duplicate
                  "niwa-artifact-store" (workspace id new-id))
(declare-function niwa-artifact-store-restore-version
                  "niwa-artifact-store" (workspace id n &optional session-id actor callback))
(declare-function niwa-artifact-store-version-path
                  "niwa-artifact-store" (workspace id n))
(declare-function niwa-artifact-store-versions
                  "niwa-artifact-store" (workspace id))
(autoload 'niwa-artifact-store-attach "niwa-artifact-store")
(autoload 'niwa-artifact-store-conversation "niwa-artifact-store")
(autoload 'niwa-artifact-store-delete "niwa-artifact-store")
(autoload 'niwa-artifact-store-duplicate "niwa-artifact-store")
(autoload 'niwa-artifact-store-restore-version "niwa-artifact-store")
(autoload 'niwa-artifact-store-version-path "niwa-artifact-store")
(autoload 'niwa-artifact-store-versions "niwa-artifact-store")
(defvar niwa-artifact-store-item-kinds)

;; `niwa-chat'
(declare-function niwa--display-chat-buffer "niwa-chat" (chat-buffer))
(autoload 'niwa--display-chat-buffer "niwa-chat")

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-data-buffer "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-for-buffer "niwa-cockpit" (buffer))
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-workspace
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context "niwa-cockpit" ())
(declare-function niwa-cockpit-format-header
                  "niwa-cockpit" (name scope state))
(declare-function niwa-cockpit-number-cell "niwa-cockpit" (text number))
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-require-owner
                  "niwa-cockpit" (&optional label context))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(declare-function niwa-cockpit-sort-by-number "niwa-cockpit" (column))
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))
(declare-function niwa-cockpit-surface-selected
                  "niwa-cockpit" (&optional no-error))
(declare-function niwa-cockpit-workspace-context
                  "niwa-cockpit" (workspace &optional origin-buffer))
(autoload 'niwa-cockpit-context-for-buffer "niwa-cockpit")
(autoload 'niwa-cockpit-context-session "niwa-cockpit")
(autoload 'niwa-cockpit-context-workspace "niwa-cockpit")
(autoload 'niwa-cockpit-current-context "niwa-cockpit")
(autoload 'niwa-cockpit-format-header "niwa-cockpit")
(autoload 'niwa-cockpit-number-cell "niwa-cockpit")
(autoload 'niwa-cockpit-open-surface "niwa-cockpit")
(autoload 'niwa-cockpit-quit "niwa-cockpit" nil t)
(autoload 'niwa-cockpit-require-owner "niwa-cockpit")
(autoload 'niwa-cockpit-setup-tabulated-surface "niwa-cockpit")
(autoload 'niwa-cockpit-sort-by-number "niwa-cockpit")
(autoload 'niwa-cockpit-surface-context "niwa-cockpit")
(autoload 'niwa-cockpit-surface-refresh "niwa-cockpit" nil t)
(autoload 'niwa-cockpit-surface-selected "niwa-cockpit")
(autoload 'niwa-cockpit-workspace-context "niwa-cockpit")

;; `niwa-collaboration'
(declare-function niwa-collaboration-item-opener
                  "niwa-collaboration" (&optional data-buffer))
(declare-function niwa-collaboration-open-shared-item
                  "niwa-collaboration" (data-buffer id))
(autoload 'niwa-collaboration-item-opener "niwa-collaboration")
(autoload 'niwa-collaboration-open-shared-item "niwa-collaboration")

;; `niwa-collaboration-artifact'
(declare-function niwa-collaboration-item-link
                  "niwa-collaboration-artifact" (link id &optional route))
(autoload 'niwa-collaboration-item-link "niwa-collaboration-artifact")

;; `niwa-collaboration-artifact-room'
(declare-function niwa-collaboration-revoke-artifact
                  "niwa-collaboration-artifact-room" (id &optional workspace))
(declare-function niwa-collaboration-rotate-artifact
                  "niwa-collaboration-artifact-room" (id &optional workspace))
(declare-function niwa-collaboration-share-artifact
                  "niwa-collaboration-artifact-room" (id &optional workspace))
(autoload 'niwa-collaboration-revoke-artifact "niwa-collaboration-artifact-room" nil t)
(autoload 'niwa-collaboration-rotate-artifact "niwa-collaboration-artifact-room" nil t)
(autoload 'niwa-collaboration-share-artifact "niwa-collaboration-artifact-room" nil t)

;; `niwa-collaboration-lobby'
(declare-function niwa-collaboration-lobby--find
                  "niwa-collaboration-lobby" (workspace))

;; `niwa-remote-artifact'
(declare-function niwa-remote-artifact-attach "niwa-remote-artifact"
                  (workspace link &optional name))
(declare-function niwa-remote-artifact-detach "niwa-remote-artifact" (workspace alias))
(autoload 'niwa-remote-artifact-attach "niwa-remote-artifact" nil t)
(autoload 'niwa-remote-artifact-detach "niwa-remote-artifact" nil t)

;; `niwa-shared-editing'
(declare-function niwa-shared-editing-check-new "niwa-shared-editing" (kind title))
(declare-function niwa-shared-editing-create "niwa-shared-editing"
                  (workspace kind title actor callback &rest keys))
(declare-function niwa-shared-editing-save-version
                  "niwa-shared-editing" (workspace id &optional session-id))
(declare-function niwa-shared-editing-session-writable-p
                  "niwa-shared-editing" (buffer))
(autoload 'niwa-shared-editing-check-new "niwa-shared-editing")
(autoload 'niwa-shared-editing-create "niwa-shared-editing")
(autoload 'niwa-shared-editing-save-version "niwa-shared-editing")
(autoload 'niwa-shared-editing-session-writable-p "niwa-shared-editing")

;; `niwa-structs'
(declare-function niwa-session-attached-artifacts "niwa-structs" (cl-x) t)
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-name "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(autoload 'niwa-workspace "niwa-workspace")

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())

(defconst niwa-artifacts-list-buffer-name "*niwa artifacts*"
  "Name of the artifacts cockpit buffer.")

(defvar-local niwa-artifacts-list--attached-only nil
  "Non-nil when the cockpit lists only the session's attached artifacts.")

(defun niwa-artifacts-list--workspace (&optional context)
  "Return the artifacts cockpit workspace for CONTEXT."
  (or (niwa-cockpit-context-workspace
       (or context (niwa-cockpit-surface-context)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "No niwa workspace here")))

(defun niwa-artifacts-list--store-rows (workspace session)
  "Return WORKSPACE's store artifacts as rows, marking SESSION's attached.
Artifacts shared through artifact links are marked `:shared'."
  (let ((attached (and session (niwa-session-attached-artifacts session))))
    (mapcar (lambda (row)
              (append (list :name (replace-regexp-in-string
                                   "[\n\r\t]+" " " (plist-get row :title))
                            :row (plist-get row :id)
                            :attached (and (member (plist-get row :id) attached) t))
                      ;; A whiteboard or document is edited live, not as a file.
                      (when (memq (plist-get row :kind) niwa-artifact-store-item-kinds)
                        (list :item (plist-get row :id)))
                      row))
            (niwa-artifact-backend-catalog workspace))))

(defun niwa-artifacts-list-count (session)
  "Return (ATTACHED . IN-PROJECT) artifact counts for SESSION, best effort."
  (or (ignore-errors
        ;; Ids only: reading every artifact's state is not a count.
        (let ((ids (mapcar (lambda (row) (plist-get row :id))
                           (niwa-artifact-backend-catalog
                            (niwa-session-workspace session) 'ids))))
          (cons (cl-count-if (lambda (id) (member id ids))
                             (niwa-session-attached-artifacts session))
                (length ids))))
      (cons 0 0)))

(defun niwa-artifacts-list--collect (context)
  "Collect artifact rows for CONTEXT's workspace store."
  (let ((session (niwa-cockpit-context-session context)))
    (cl-remove-if-not
     (lambda (row) (or (not niwa-artifacts-list--attached-only)
                       (plist-get row :attached)))
     (niwa-artifacts-list--store-rows
      (niwa-artifacts-list--workspace context) session))))

(defun niwa-artifacts-list--entry (item _context)
  "Return tabulated row for artifact ITEM."
  (list
   (plist-get item :row)
   (vector
    (plist-get item :name)
    (format "%s" (or (plist-get item :kind) ""))
    (if (plist-get item :attached) "✓" "")
    (niwa-cockpit-number-cell (number-to-string (plist-get item :versions))
                                 (plist-get item :versions))
    (if (plist-get item :missing) "missing"
      (format-time-string "%Y-%m-%d %H:%M" (plist-get item :modified)))
    (let ((size (or (plist-get item :size) 0)))
      (niwa-cockpit-number-cell (file-size-human-readable size) size))
    (niwa-artifacts-list--link-cell item))))

(defun niwa-artifacts-list--reachability (item)
  "Return what is known of remote artifact ITEM's origin, as text."
  (pcase (plist-get item :reachable)
    ('yes "reachable")
    ('no "unavailable")
    (_ "not checked yet")))

(defun niwa-artifacts-list--link-cell (item)
  "Return the Link cell of artifact ITEM: shared, or a remote artifact's origin."
  (cond
   ((plist-get item :remote)
    (format "remote %s, %s" (plist-get item :origin)
            (niwa-artifacts-list--reachability item)))
   ((plist-get item :shared) "shared")
   (t "")))

(defun niwa-artifacts-list--header (items context)
  "Return cockpit header for artifact ITEMS and CONTEXT."
  (let ((session (niwa-cockpit-context-session context)))
    (niwa-cockpit-format-header
     "artifacts"
     (if session
         (niwa-session-name session)
       (niwa-workspace-name (niwa-artifacts-list--workspace context)))
     (format "%d artifact%s%s" (length items)
             (if (= 1 (length items)) "" "s")
             (if niwa-artifacts-list--attached-only " (attached)" "")))))

(defun niwa-artifacts-list--details (item _context)
  "Return the information report for artifact ITEM."
  (list :title "Artifact" :subtitle (plist-get item :id)
        :identity (plist-get item :id)
        :sections
        (delq
         nil
         (list
          (list :id 'artifact :title "Artifact"
                :body (niwa-report-fields
                       (list "Id" (plist-get item :id))
                       (list "Title" (plist-get item :title))
                       (list "Kind" (format "%s" (plist-get item :kind)))
                       (list "Attached" (if (plist-get item :attached) "yes" "no"))
                       (unless (plist-get item :remote)
                         (list "Shared" (if (plist-get item :shared) "yes" "no")))
                       (list "Versions" (number-to-string (plist-get item :versions)))
                       (list "Size" (file-size-human-readable (or (plist-get item :size) 0)))
                       (list "Modified"
                             (if (plist-get item :missing)
                                 "file missing"
                               (format-time-string "%Y-%m-%d %H:%M:%S"
                                                   (plist-get item :modified))))))
          (if (plist-get item :remote)
              (list :id 'origin :title "Origin"
                    :body (niwa-report-fields
                           (list "Origin" (plist-get item :origin))
                           (list "Relay" (plist-get item :relay))
                           (list "Origin id" (plist-get item :origin-id))
                           (list "Link" (plist-get item :tier))
                           (list "Reachable" (niwa-artifacts-list--reachability item))
                           (when-let* ((failure (plist-get item :error)))
                             (list "Last error" failure))))
            (list :id 'location :title "Location"
                  :body (niwa-report-fields (list "Path" (plist-get item :path)))))))))

(defun niwa-artifacts-list--local-selected ()
  "Return the selected artifact, refusing a remote one."
  (let ((item (niwa-cockpit-surface-selected)))
    (when (plist-get item :remote)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                  "%s lives at its origin; agents reach it as shared://%s"
                  (plist-get item :title) (plist-get item :id)))
    item))

(defun niwa-artifacts-list--selected-path ()
  "Return the selected artifact's still-existing path."
  (let ((item (niwa-artifacts-list--local-selected)))
    (when (plist-get item :item)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                   "Whiteboards and documents open in their editor; use o"))
    (unless (and (plist-get item :path) (file-exists-p (plist-get item :path)))
      (niwa-cockpit-surface-refresh)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                   "Artifact file no longer exists"))
    (plist-get item :path)))

(defun niwa-artifacts-list--browse (path)
  "Show artifact file PATH: in a browser, or in Emacs when it is remote.
The local browser cannot read a remote target's filesystem."
  (if (file-remote-p path)
      (find-file path)
    (browse-url-of-file path)))

(defun niwa-artifacts-list--open-remote-html (context item)
  "Open remote HTML artifact ITEM in a browser viewer for CONTEXT.
The session's room serves it; without a session, the workspace's running
lobby does, since a room resolves store artifacts by its workspace."
  (if-let* ((data (niwa-cockpit-context-data-buffer context)))
      (funcall (niwa-collaboration-item-opener data) data (plist-get item :id) "artifact")
    (let ((lobby (and (featurep 'niwa-collaboration-lobby)
                      (niwa-collaboration-lobby--find
                       (niwa-artifacts-list--workspace context)))))
      (unless lobby
        (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                    "To view %s, start the project lobby (/collab lobby) or open Project artifacts from a session"
                    (plist-get item :title)))
      (browse-url (niwa-collaboration-item-link
                   (plist-get lobby :link-full) (plist-get item :id) "artifact")))))

(defun niwa-artifacts-list-open-browser ()
  "Open the selected artifact in a web browser.
A whiteboard or document opens in its editor in the session's room, which
is started after confirmation when the session is not shared.  Without a
session it opens in the room of the item's own conversation.  A remote
HTML artifact opens in the viewer of the session's room, or without a
session the project lobby's; that room reads it from its origin."
  (interactive)
  (let ((context (niwa-cockpit-surface-context))
        (selected (niwa-cockpit-surface-selected)))
    (cond
     ((and (plist-get selected :remote) (eq (plist-get selected :kind) 'html))
      (niwa-artifacts-list--open-remote-html context selected))
     ((plist-get (niwa-artifacts-list--local-selected) :item)
      (let ((id (plist-get selected :item)))
        (niwa-collaboration-open-shared-item
         (or (niwa-cockpit-context-data-buffer context)
             (niwa-artifact-store-conversation
              (niwa-artifacts-list--workspace context) id))
         id)))
     (t (niwa-artifacts-list--browse (niwa-artifacts-list--selected-path))))))

(defun niwa-artifacts-list-visit ()
  "Visit the selected artifact file in Emacs."
  (interactive)
  (find-file (niwa-artifacts-list--selected-path)))

(defun niwa-artifacts-list--shared-selected ()
  "Return the selected artifact, refusing one that is not shared."
  (let ((item (niwa-artifacts-list--local-selected)))
    (unless (plist-get item :shared)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                   "%s is not shared; S shares it" (plist-get item :title)))
    item))

(defun niwa-artifacts-list--after-sharing (function item)
  "Call FUNCTION with ITEM's id and the cockpit workspace, then refresh."
  (let ((id (plist-get item :id)))
    (funcall function id (niwa-artifacts-list--workspace))
    (niwa-cockpit-surface-refresh id)))

(defun niwa-artifacts-list-share ()
  "Share the selected artifact through artifact links and show them.
A shared artifact keeps its links; they are shown again."
  (interactive)
  (niwa-artifacts-list--after-sharing
   #'niwa-collaboration-share-artifact (niwa-artifacts-list--local-selected)))

(defun niwa-artifacts-list-rotate ()
  "Replace the selected shared artifact's links and show the new ones."
  (interactive)
  (niwa-artifacts-list--after-sharing
   #'niwa-collaboration-rotate-artifact (niwa-artifacts-list--shared-selected)))

(defun niwa-artifacts-list-revoke ()
  "Stop sharing the selected artifact; its links stop working."
  (interactive)
  (niwa-artifacts-list--after-sharing
   #'niwa-collaboration-revoke-artifact (niwa-artifacts-list--shared-selected)))

(defun niwa-artifacts-list-attach-remote (link &optional name)
  "Attach the remote artifact LINK shares to this workspace.
The link is read without echo; a prefix argument asks for NAME."
  (interactive
   (list (read-passwd "Artifact link: ")
         (when current-prefix-arg (read-string "Attach as: "))))
  (let ((alias (niwa-remote-artifact-attach
                (niwa-artifacts-list--workspace) link name)))
    (niwa-cockpit-surface-refresh alias)))

(defun niwa-artifacts-list-attach ()
  "Attach the selected artifact to the current session."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (or (niwa-cockpit-context-session context)
                      (niwa-diagnostic-user-error
                       (niwa-cockpit-diagnostic-scope context)
                       "Attaching needs a session; open the cockpit from one")))
         (id (plist-get (niwa-cockpit-surface-selected) :id)))
    (niwa-artifact-store-attach
     session id (niwa-cockpit-context-data-buffer context))
    (niwa-cockpit-surface-refresh id)
    (message "niwa: %s attached to %s" id (niwa-session-name session))))

(defun niwa-artifacts-list-toggle-attached ()
  "Toggle between all artifacts and the session's attached ones."
  (interactive)
  (setq niwa-artifacts-list--attached-only
        (not niwa-artifacts-list--attached-only))
  (niwa-cockpit-surface-refresh))

(defun niwa-artifacts-list-versions ()
  "View or restore a version of the selected artifact.
Restoring copies the version over the artifact as a new version."
  (interactive)
  (let* ((buffer (current-buffer))
         (context (niwa-cockpit-surface-context))
         (workspace (niwa-artifacts-list--workspace context))
         (session (niwa-cockpit-context-session context))
         (id (plist-get (niwa-artifacts-list--local-selected) :id))
         (scope (niwa-diagnostic-scope workspace))
         (choices
          (mapcar (lambda (row)
                    (cons (format "%d  %s  %s" (plist-get row :n)
                                  (format-time-string "%Y-%m-%d %H:%M"
                                                      (plist-get row :time))
                                  (file-size-human-readable (plist-get row :bytes)))
                          (plist-get row :n)))
                  (reverse (niwa-artifact-store-versions workspace id))))
         (n (if choices
                (cdr (assoc (completing-read (format "Version of %s: " id)
                                             choices nil t)
                            choices))
              (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace)
                                           "%s has no versions yet" id))))
    (pcase (if (plist-get (niwa-cockpit-surface-selected) :item)
               ;; A whiteboard's or document's version opens only by restoring.
               (and (y-or-n-p (format "Restore version %d of %s? " n id)) ?r)
             (car (read-multiple-choice
                   (format "Version %d" n)
                   '((?v "view" "Open this version")
                     (?r "restore" "Restore it as the newest version")))))
      (?v (niwa-artifacts-list--browse
           (niwa-artifact-store-version-path workspace id n)))
      (?r (niwa-artifact-store-restore-version
           workspace id n
           (and session (niwa-session-session-id session))
           nil
           (lambda (reply)
             (if (plist-get reply :error)
                 (niwa-diagnostic-message scope
                                             "%s; restoring %s" (plist-get reply :error) id)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (niwa-cockpit-surface-refresh id)))
               (message "niwa: %s restored from version %d as version %d"
                        id n (plist-get reply :n)))))))))

(defun niwa-artifacts-list-save-version ()
  "Save the selected whiteboard or document as a new version."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (workspace (niwa-artifacts-list--workspace context))
         (session (niwa-cockpit-context-session context))
         (id (or (plist-get (niwa-artifacts-list--local-selected) :item)
                 (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace)
                                              "File artifacts keep a version of every saved change")))
         (n (niwa-shared-editing-save-version
             workspace id (and session (niwa-session-session-id session)))))
    (niwa-cockpit-surface-refresh id)
    (message "niwa: saved %s as version %d" id n)))

(defun niwa-artifacts-list-duplicate ()
  "Copy the selected artifact into a new, independent artifact."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (workspace (niwa-artifacts-list--workspace context))
         (session (niwa-cockpit-context-session context))
         (id (plist-get (niwa-artifacts-list--local-selected) :id))
         (new-id (read-string "New artifact id: " (concat id "-copy"))))
    (niwa-artifact-store-duplicate workspace id new-id)
    (when session
      (niwa-artifact-store-attach
       session new-id (niwa-cockpit-context-data-buffer context)))
    (niwa-cockpit-surface-refresh new-id)
    (message "niwa: %s duplicated as %s" id new-id)))

(defun niwa-artifacts-list-conversation ()
  "Open the selected artifact's own conversation, creating it on first use."
  (interactive)
  (let ((workspace (niwa-artifacts-list--workspace))
        (id (plist-get (niwa-artifacts-list--local-selected) :id)))
    (niwa--display-chat-buffer
     (niwa-artifact-store-conversation workspace id))))

(defun niwa-artifacts-list-delete ()
  "Delete the selected artifact with its versions, discussions and conversation.
A whiteboard or document is deleted as a shared item, after any save in
progress, together with its discussions and history.  A remote artifact
is detached instead: its origin keeps it."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (workspace (niwa-artifacts-list--workspace context))
         (item (niwa-cockpit-surface-selected))
         (name (or (plist-get item :title) (plist-get item :id)))
         (scope (niwa-diagnostic-scope workspace))
         (buffer (current-buffer)))
    (cond
     ((plist-get item :remote)
      (when (yes-or-no-p (format "Detach %s (its origin keeps it)? " name))
        (niwa-remote-artifact-detach workspace (plist-get item :id))
        (niwa-cockpit-surface-refresh)))
     ((yes-or-no-p (format "Delete %s with its versions, discussions and conversation? "
                           name))
      (niwa-artifact-store-delete
       workspace (plist-get item :id) nil
       (lambda (failure)
         (if failure
             (niwa-diagnostic-message scope
                                         "%s; %s was not deleted" failure name)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (niwa-cockpit-surface-refresh)))
           (message "niwa: %s deleted" name))))))))

(defun niwa-artifacts-list-quit ()
  "Quit the artifacts cockpit."
  (interactive)
  (niwa-cockpit-quit "artifacts cockpit"))


;;
;;; Creating whiteboards and documents

(defun niwa-artifacts-list--creation-owner ()
  "Return (CONTEXT WORKSPACE SESSION DATA-BUFFER) for creating an item here.
A session cockpit, the session menu or a session buffer creates for that
session; anywhere else in a project, for the workspace alone.  A session
that is read-only here refuses."
  (let* ((context (or (condition-case nil (niwa-cockpit-current-context)
                        (user-error nil))
                      (niwa-cockpit-workspace-context
                       (or (niwa-workspace)
                           (niwa-diagnostic-user-error
                            (niwa-diagnostic-scope (current-buffer))
                            "No niwa workspace here"))
                       (current-buffer))))
         (session (niwa-cockpit-context-session context))
         (data (and session (niwa-cockpit-context-data-buffer context))))
    (niwa-cockpit-require-owner "item creation" context)
    (unless (niwa-shared-editing-session-writable-p data)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "This session is read-only here; take control to create items"))
    (list context (niwa-artifacts-list--workspace context) session data)))

(defun niwa-artifacts-list--created (reply kind title workspace data opener)
  "Report creating a KIND item titled TITLE from REPLY, then open it.
WORKSPACE holds it; DATA is the session's data buffer, or nil to open it
in the item's own conversation.  OPENER is from
`niwa-collaboration-item-opener'.  A created item is never discarded:
a later failure is reported with its address."
  (if-let* ((failure (plist-get reply :error)))
      (niwa-diagnostic-message (niwa-diagnostic-scope workspace)
                                  "%s; %s \"%s\" was not created" failure kind title)
    (let ((id (plist-get reply :id)))
      (message "niwa: created %s \"%s\" (shared://%s)" kind title id)
      (when-let* ((failure (plist-get reply :attach-error)))
        (niwa-diagnostic-message (niwa-diagnostic-scope workspace)
                                    "%s; created shared://%s but it was not attached to this session"
                                    failure id))
      ;; Only a cockpit of the same store lists the item; selecting an id
      ;; another store lacks would move its selection.
      (when-let* ((cockpit (get-buffer niwa-artifacts-list-buffer-name)))
        (with-current-buffer cockpit
          (ignore-errors
            (when (equal (niwa-workspace-root workspace)
                         (niwa-workspace-root (niwa-artifacts-list--workspace)))
              (niwa-cockpit-surface-refresh id)))))
      (condition-case err
          (funcall opener (or data (niwa-artifact-store-conversation workspace id)) id)
        (error
         (niwa-diagnostic-message (niwa-diagnostic-scope workspace)
                                     "%s; created shared://%s \"%s\", but could not open it"
                                     (error-message-string err) id title))))))

;;;###autoload
(defun niwa-artifacts-new-shared-item (kind title)
  "Create a KIND whiteboard or document titled TITLE and open its editor.
From a session, its cockpits or its menu, the item is attached to that
session and opens in its room; elsewhere in a project it opens in the
item's own conversation.  The editor exists only in a room, so when that
session is not shared this first asks, with `/collab''s disclosure, to
share it.  Nothing is created when that is declined."
  (interactive
   (progn
     (niwa-artifacts-list--creation-owner)
     (let ((kind (downcase (completing-read "New shared item: "
                                            '("Document" "Whiteboard") nil t))))
       (list kind (read-string (format "%s title: " (capitalize kind)))))))
  (pcase-let* ((`(,_context ,workspace ,session ,data)
                (niwa-artifacts-list--creation-owner))
               (title (niwa-shared-editing-check-new kind title))
               (opener (niwa-collaboration-item-opener data)))
    (niwa-shared-editing-create
     workspace kind title "Host"
     (lambda (reply)
       (niwa-artifacts-list--created reply kind title workspace data opener))
     :session session :buffer data)))

(defconst niwa-artifacts-list--surface
  `(:buffer-name ,niwa-artifacts-list-buffer-name
    :label "artifacts cockpit"
    :row-label "artifact"
    :mode niwa-artifacts-list-mode
    :format [("Artifact" 32 t)
             ("Kind" 10 t)
             ("Att" 4 t)
             ("Ver" 4 ,(niwa-cockpit-sort-by-number 3))
             ("Modified" 17 t)
             ("Size" 8 ,(niwa-cockpit-sort-by-number 5))
             ("Link" 0 t)]
    :sort-key ("Modified" . t)
    :collect niwa-artifacts-list--collect
    :entry niwa-artifacts-list--entry
    :header niwa-artifacts-list--header
    :details niwa-artifacts-list--details
    :details-buffer "*niwa artifact details*"
    :keys (("o" "Open the selected artifact or shared item in a browser"
            niwa-artifacts-list-open-browser)
           ("n" "Create a whiteboard or document and open it"
            niwa-artifacts-new-shared-item)
           ("e" "Visit the selected artifact file in Emacs"
            niwa-artifacts-list-visit)
           ("a" "Attach the selected artifact to this session"
            niwa-artifacts-list-attach)
           ("t" "Toggle between all and attached artifacts"
            niwa-artifacts-list-toggle-attached)
           ("v" "View or restore a version of the selected artifact"
            niwa-artifacts-list-versions)
           ("D" "Duplicate the selected artifact"
            niwa-artifacts-list-duplicate)
           ("c" "Open the selected artifact's own conversation"
            niwa-artifacts-list-conversation)
           ("s" "Save the selected whiteboard or document as a version"
            niwa-artifacts-list-save-version)
           ("S" "Share the selected artifact, or show its links"
            niwa-artifacts-list-share)
           ("Z" "Replace the selected artifact's links"
            niwa-artifacts-list-rotate)
           ("X" "Stop sharing the selected artifact"
            niwa-artifacts-list-revoke)
           ("R" "Attach a remote artifact from its link"
            niwa-artifacts-list-attach-remote)
           ("d" "Delete the selected artifact, or detach a remote one"
            niwa-artifacts-list-delete)))
  "Cockpit surface spec for the artifact store.")

(define-derived-mode niwa-artifacts-list-mode tabulated-list-mode
  "niwa-artifacts"
  "Major mode for browsing the workspace artifact store."
  (niwa-cockpit-setup-tabulated-surface
   niwa-artifacts-list--surface))

(defun niwa-artifacts-list-open (&optional context)
  "Open the artifacts cockpit for CONTEXT.
Without CONTEXT, use the current session, or else the current buffer's
workspace."
  (let ((context
         (or context
             (niwa-cockpit-context-for-buffer (current-buffer))
             (niwa-cockpit-workspace-context
              (or (niwa-workspace)
                  (niwa-diagnostic-user-error
                   (niwa-diagnostic-scope (current-buffer))
                   "No niwa workspace here"))
              (current-buffer)))))
    (niwa-cockpit-open-surface niwa-artifacts-list--surface context)))

;;;###autoload
(defun niwa-artifacts ()
  "Browse the current workspace's artifact store."
  (interactive)
  (niwa-artifacts-list-open))

(provide 'niwa-artifacts-list)
;;; niwa-artifacts-list.el ends here
