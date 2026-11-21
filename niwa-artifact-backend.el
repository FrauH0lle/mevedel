;;; niwa-artifact-backend.el --- Where an artifact's content is served -*- lexical-binding: t -*-

;;; Commentary:

;; Whiteboards and documents are read, edited, versioned and deleted
;; through one artifact-backend interface, and every listing of the
;; project's artifacts comes from its catalog.  An adapter serves the
;; interface for some artifacts: the local adapter is the workspace
;; artifact store with its editing queue, item leases and Node helper.
;; Artifacts served from elsewhere are registered by id with their
;; adapter; every other id is local.
;;
;; An adapter is a plist:
;;
;;   :call     (WORKSPACE ARGS CALLBACK AUTHORIZE COMMIT) runs one editor
;;             request and returns a cancellation function, as
;;             `niwa-shared-editing-call' does.
;;   :catalog  (WORKSPACE DETAIL) returns the rows of the artifacts it
;;             serves, at the DETAIL `niwa-artifact-backend-catalog'
;;             describes.
;;
;; Adapters announce changes through the existing events: a committed
;; edit through `niwa-artifact-backend-notify', a changed listing
;; through `niwa-artifact-store-changed-functions'.  Registering or
;; unregistering an artifact announces a listing change.
;;
;; A registered artifact keeps its versions where it is served, so a turn
;; that edited one records none here.
;;
;; Registrations outlive Emacs: the first lookup of a workspace restores
;; the remote artifacts attached there (`niwa-remote-artifact').

;;; Code:

(require 'cl-lib)
(require 'niwa-artifact-store)
(require 'niwa-structs)

;; `niwa-remote-artifact'
(declare-function niwa-remote-artifact-registrations "niwa-remote-artifact" (workspace))
(autoload 'niwa-remote-artifact-registrations "niwa-remote-artifact")

;; `niwa-shared-editing'
(declare-function niwa-shared-editing-call "niwa-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function niwa-shared-editing-list "niwa-shared-editing" (workspace))
(declare-function niwa-shared-editing-present-p "niwa-shared-editing" (workspace id))
(autoload 'niwa-shared-editing-call "niwa-shared-editing")
(autoload 'niwa-shared-editing-list "niwa-shared-editing")
(autoload 'niwa-shared-editing-present-p "niwa-shared-editing")

(defvar niwa-artifact-backend-change-hook nil
  "Functions called with WORKSPACE, STATE, RESULT after an editing commit.
A deleted item arrives as a STATE with `:deleted' and `:actor' and a nil
RESULT.  Observers cannot change whether the preceding commit succeeded.")

(defvar niwa-artifact-backend--registry (make-hash-table :test #'equal)
  "Workspace root -> alist of (ID . ADAPTER) for artifacts served elsewhere.")

(defconst niwa-artifact-backend-edit-keys
  '(("patch" :changes)
    ("rename" :title)
    ("background" :background)
    ("image" :data :x :y :width :alt :afterId)
    ("insert" :item :x :y)
    ("revert" :transaction))
  "`SharedEdit' actions an artifact link carries, with the keys each passes on.
The origin's artifact room accepts these and the remote adapter sends
them; anything else a request carries is dropped.  An image travels as
`:data', its base64 content, since the origin cannot read the consumer's
files; an insert names its library item as LIBRARY/ITEM-ID, which the
origin resolves in its own libraries; a revert names the contribution,
from the history a read returns, as its `:transaction'.")

(defvar niwa-artifact-backend--restored (make-hash-table :test #'equal)
  "Workspace roots whose persisted registrations were restored.")

(defun niwa-artifact-backend-notify (workspace state result)
  "Tell `niwa-artifact-backend-change-hook' about WORKSPACE's STATE and RESULT.
A failing observer does not keep the others from hearing it."
  (dolist (observer niwa-artifact-backend-change-hook)
    (condition-case nil (funcall observer workspace state result)
      (error nil))))

(defun niwa-artifact-backend--registered (workspace)
  "Return WORKSPACE's alist of registered (ID . ADAPTER).
The first lookup of WORKSPACE restores its attached remote artifacts.  A
restored id that is invalid, names a local artifact or is registered
already is skipped."
  (let ((root (niwa-workspace-root workspace)))
    (unless (gethash root niwa-artifact-backend--restored)
      (puthash root t niwa-artifact-backend--restored)
      (when-let* ((restored (niwa-remote-artifact-registrations workspace)))
        (let ((local (niwa-artifact-store-ids workspace))
              (registered (gethash root niwa-artifact-backend--registry)))
          (dolist (pair restored)
            (unless (or (not (niwa-artifact-store-id-p (car pair)))
                        (member (car pair) local)
                        (assoc (car pair) registered))
              (push pair registered)))
          (puthash root registered niwa-artifact-backend--registry))))
    (gethash root niwa-artifact-backend--registry)))

;;;###autoload
(defun niwa-artifact-backend-registered-p (workspace id)
  "Return non-nil when WORKSPACE's artifact ID is served by a registered adapter."
  (and (assoc id (niwa-artifact-backend--registered workspace)) t))

(defun niwa-artifact-backend-register (workspace id adapter)
  "Serve WORKSPACE's artifact ID through ADAPTER instead of the local store.
ID follows the store's id rule and must not name a local artifact."
  (unless (niwa-artifact-store-id-p id)
    (error "Invalid artifact id: %S" id))
  (when (member id (niwa-artifact-store-ids workspace))
    (error "Artifact %s already exists in this project" id))
  (let ((registered (niwa-artifact-backend--registered workspace)))
    (puthash (niwa-workspace-root workspace)
             (cons (cons id adapter) (assoc-delete-all id (copy-sequence registered)))
             niwa-artifact-backend--registry))
  (niwa-artifact-store-changed workspace))

(defun niwa-artifact-backend-unregister (workspace id)
  "Stop serving WORKSPACE's artifact ID through a registered adapter."
  (let* ((root (niwa-workspace-root workspace))
         (remaining (assoc-delete-all
                     id (copy-sequence (niwa-artifact-backend--registered workspace)))))
    (if remaining
        (puthash root remaining niwa-artifact-backend--registry)
      (remhash root niwa-artifact-backend--registry)))
  (niwa-artifact-store-changed workspace))

;;;###autoload
(defun niwa-artifact-backend-call (workspace args callback &optional authorize commit)
  "Run editor ARGS for WORKSPACE's artifact `:id'; deliver one reply to CALLBACK.
The artifact's adapter serves it; see `niwa-shared-editing-call' for
AUTHORIZE, COMMIT, the reply and the returned cancellation function."
  (funcall (if-let* ((adapter (alist-get (plist-get args :id)
                                         (niwa-artifact-backend--registered workspace)
                                         nil nil #'equal)))
               (plist-get adapter :call)
             #'niwa-shared-editing-call)
           workspace args callback authorize commit))

;;;###autoload
(defun niwa-artifact-backend-present-p (workspace id)
  "Return non-nil when WORKSPACE has the whiteboard or document ID."
  (or (niwa-artifact-backend-registered-p workspace id)
      (niwa-shared-editing-present-p workspace id)))

;;;###autoload
(defun niwa-artifact-backend-save-version (workspace id &optional session-id)
  "Record WORKSPACE's artifact ID as a version after a turn edited it.
A local artifact is versioned by the store for the session SESSION-ID,
as `niwa-artifact-store-save-version' describes, and its result is
returned.  A registered artifact records nothing here and returns nil:
where it is served keeps its versions, an origin one per working stretch."
  (unless (niwa-artifact-backend-registered-p workspace id)
    (niwa-artifact-store-save-version workspace id session-id)))

(defun niwa-artifact-backend--local-catalog (workspace detail)
  "Return the rows of WORKSPACE's local store at DETAIL."
  (pcase detail
    ('ids (mapcar (lambda (id) (list :id id)) (niwa-artifact-store-ids workspace)))
    ('items (niwa-shared-editing-list workspace))
    (_ (niwa-artifact-store-list workspace))))

;;;###autoload
(defun niwa-artifact-backend-catalog (workspace &optional detail)
  "Return WORKSPACE's artifacts, local and registered, as plists.
DETAIL chooses what each row costs and carries:

  `ids'    every artifact's :id, from directory names and the registry.
  `items'  whiteboards and documents with :id, :kind (a string), :title
           and :revision, from their metadata only.
  nil      every artifact as `niwa-artifact-store-list' describes
           it, newest modification first.

A local row whose id is registered elsewhere is left out: the
registered adapter serves that id."
  (let* ((registered (niwa-artifact-backend--registered workspace))
         (rows (cl-remove-if (lambda (row) (assoc (plist-get row :id) registered))
                             (niwa-artifact-backend--local-catalog workspace detail))))
    (dolist (adapter (cl-remove-duplicates (mapcar #'cdr (reverse registered))
                                           :test #'eq :from-end t))
      (setq rows (append rows (funcall (plist-get adapter :catalog) workspace detail))))
    (if (or detail (null registered))
        rows
      (sort rows :key (lambda (row) (float-time (or (plist-get row :modified) 0)))
            :reverse t))))

(provide 'niwa-artifact-backend)
;;; niwa-artifact-backend.el ends here
