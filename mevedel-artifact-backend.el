;;; mevedel-artifact-backend.el --- Where an artifact's content is served -*- lexical-binding: t -*-

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
;;             `mevedel-shared-editing-call' does.
;;   :catalog  (WORKSPACE DETAIL) returns the rows of the artifacts it
;;             serves, at the DETAIL `mevedel-artifact-backend-catalog'
;;             describes.
;;
;; Adapters announce changes through the existing events: a committed
;; edit through `mevedel-artifact-backend-notify', a changed listing
;; through `mevedel-artifact-store-changed-functions'.  Registering or
;; unregistering an artifact announces a listing change.
;;
;; Registrations outlive Emacs where their owner persists them: the first
;; lookup of a workspace asks `mevedel-artifact-backend-restore-function'
;; for that workspace's registrations, which today are the remote
;; artifacts attached there (`mevedel-remote-artifact').

;;; Code:

(require 'cl-lib)
(require 'mevedel-artifact-store)
(require 'mevedel-structs)

;; `mevedel-remote-artifact'
(declare-function mevedel-remote-artifact-registrations "mevedel-remote-artifact" (workspace))
(autoload 'mevedel-remote-artifact-registrations "mevedel-remote-artifact")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function mevedel-shared-editing-list "mevedel-shared-editing" (workspace))
(declare-function mevedel-shared-editing-present-p "mevedel-shared-editing" (workspace id))
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-list "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-present-p "mevedel-shared-editing")

(defvar mevedel-artifact-backend-change-hook nil
  "Functions called with WORKSPACE, STATE, RESULT after an editing commit.
A deleted item arrives as a STATE with `:deleted' and `:actor' and a nil
RESULT.  Observers cannot change whether the preceding commit succeeded.")

(defvar mevedel-artifact-backend--registry (make-hash-table :test #'equal)
  "Workspace root -> alist of (ID . ADAPTER) for artifacts served elsewhere.")

(defvar mevedel-artifact-backend-restore-function #'mevedel-remote-artifact-registrations
  "Function returning a WORKSPACE's persisted registrations as (ID . ADAPTER).
It is called once per workspace root, on the first lookup.  A returned id
that is invalid, names a local artifact or is registered already is
skipped.")

(defvar mevedel-artifact-backend--restored (make-hash-table :test #'equal)
  "Workspace roots whose persisted registrations were restored.")

(defun mevedel-artifact-backend-notify (workspace state result)
  "Tell `mevedel-artifact-backend-change-hook' about WORKSPACE's STATE and RESULT.
A failing observer does not keep the others from hearing it."
  (dolist (observer mevedel-artifact-backend-change-hook)
    (condition-case nil (funcall observer workspace state result)
      (error nil))))

(defun mevedel-artifact-backend--registered (workspace)
  "Return WORKSPACE's alist of registered (ID . ADAPTER).
The first lookup of WORKSPACE restores its persisted registrations."
  (let ((root (mevedel-workspace-root workspace)))
    (unless (gethash root mevedel-artifact-backend--restored)
      (puthash root t mevedel-artifact-backend--restored)
      (when-let* ((restored (funcall mevedel-artifact-backend-restore-function workspace)))
        (let ((local (mevedel-artifact-store-ids workspace))
              (registered (gethash root mevedel-artifact-backend--registry)))
          (dolist (pair restored)
            (unless (or (not (mevedel-artifact-store-id-p (car pair)))
                        (member (car pair) local)
                        (assoc (car pair) registered))
              (push pair registered)))
          (puthash root registered mevedel-artifact-backend--registry))))
    (gethash root mevedel-artifact-backend--registry)))

;;;###autoload
(defun mevedel-artifact-backend-registered-p (workspace id)
  "Return non-nil when WORKSPACE's artifact ID is served by a registered adapter."
  (and (assoc id (mevedel-artifact-backend--registered workspace)) t))

(defun mevedel-artifact-backend-register (workspace id adapter)
  "Serve WORKSPACE's artifact ID through ADAPTER instead of the local store.
ID follows the store's id rule and must not name a local artifact."
  (unless (mevedel-artifact-store-id-p id)
    (error "Invalid artifact id: %S" id))
  (when (member id (mevedel-artifact-store-ids workspace))
    (error "Artifact %s already exists in this project" id))
  (let ((registered (mevedel-artifact-backend--registered workspace)))
    (puthash (mevedel-workspace-root workspace)
             (cons (cons id adapter) (assoc-delete-all id (copy-sequence registered)))
             mevedel-artifact-backend--registry))
  (mevedel-artifact-store--changed workspace))

(defun mevedel-artifact-backend-unregister (workspace id)
  "Stop serving WORKSPACE's artifact ID through a registered adapter."
  (let* ((root (mevedel-workspace-root workspace))
         (remaining (assoc-delete-all
                     id (copy-sequence (mevedel-artifact-backend--registered workspace)))))
    (if remaining
        (puthash root remaining mevedel-artifact-backend--registry)
      (remhash root mevedel-artifact-backend--registry)))
  (mevedel-artifact-store--changed workspace))

;;;###autoload
(defun mevedel-artifact-backend-call (workspace args callback &optional authorize commit)
  "Run editor ARGS for WORKSPACE's artifact `:id'; deliver one reply to CALLBACK.
The artifact's adapter serves it; see `mevedel-shared-editing-call' for
AUTHORIZE, COMMIT, the reply and the returned cancellation function."
  (funcall (if-let* ((adapter (alist-get (plist-get args :id)
                                         (mevedel-artifact-backend--registered workspace)
                                         nil nil #'equal)))
               (plist-get adapter :call)
             #'mevedel-shared-editing-call)
           workspace args callback authorize commit))

;;;###autoload
(defun mevedel-artifact-backend-present-p (workspace id)
  "Return non-nil when WORKSPACE has the whiteboard or document ID."
  (or (mevedel-artifact-backend-registered-p workspace id)
      (mevedel-shared-editing-present-p workspace id)))

(defun mevedel-artifact-backend--local-catalog (workspace detail)
  "Return the rows of WORKSPACE's local store at DETAIL."
  (pcase detail
    ('ids (mapcar (lambda (id) (list :id id)) (mevedel-artifact-store-ids workspace)))
    ('items (mevedel-shared-editing-list workspace))
    (_ (mevedel-artifact-store-list workspace))))

;;;###autoload
(defun mevedel-artifact-backend-catalog (workspace &optional detail)
  "Return WORKSPACE's artifacts, local and registered, as plists.
DETAIL chooses what each row costs and carries:

  `ids'    every artifact's :id, from directory names and the registry.
  `items'  whiteboards and documents with :id, :kind (a string), :title
           and :revision, from their metadata only.
  nil      every artifact as `mevedel-artifact-store-list' describes
           it, newest modification first.

A local row whose id is registered elsewhere is left out: the
registered adapter serves that id."
  (let* ((registered (mevedel-artifact-backend--registered workspace))
         (rows (cl-remove-if (lambda (row) (assoc (plist-get row :id) registered))
                             (mevedel-artifact-backend--local-catalog workspace detail))))
    (dolist (adapter (cl-remove-duplicates (mapcar #'cdr (reverse registered))
                                           :test #'eq :from-end t))
      (setq rows (append rows (funcall (plist-get adapter :catalog) workspace detail))))
    (if (or detail (null registered))
        rows
      (sort rows :key (lambda (row) (float-time (or (plist-get row :modified) 0)))
            :reverse t))))

(provide 'mevedel-artifact-backend)
;;; mevedel-artifact-backend.el ends here
