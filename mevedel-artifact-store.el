;;; mevedel-artifact-store.el --- Workspace artifact store -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace artifact store at `<workspace>/.mevedel/artifacts/'.  Each
;; artifact has a directory named by its stable id for authored files.
;; Protected `.state/ID/' directories hold host bookkeeping: `meta.el',
;; numbered versions with `index.el', `comments.json' and shared editor
;; `state.json'.  Sessions attach to artifacts by id.  An artifact may have
;; one dedicated session, created on demand for conversations outside any chat.
;; The two name each other; while they agree the session is hidden from session
;; lists, kept from expiry, and deleted with the artifact.
;;
;; A settled ApplyPatch reports its writes here.  A write into a new id
;; directory creates the artifact; a turn that wrote its primary file records
;; one version when it settles.  Versions are capped by count and bytes, the
;; latest always kept.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'mevedel-workspace)
(require 'mevedel-session-control-fs)

;; `mevedel-artifact-lease'
(declare-function mevedel-artifact-lease--forget "mevedel-artifact-lease" (directory))
(declare-function mevedel-artifact-lease-directory "mevedel-artifact-lease" (workspace id))
(declare-function mevedel-artifact-lease-run "mevedel-artifact-lease" (workspace id operations))
(autoload 'mevedel-artifact-lease--forget "mevedel-artifact-lease")
(autoload 'mevedel-artifact-lease-directory "mevedel-artifact-lease")
(autoload 'mevedel-artifact-lease-run "mevedel-artifact-lease")

;; `mevedel-chat'
(declare-function mevedel--chat-buffer "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--ensure-chat-preset "mevedel-chat" (chat-buffer))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(autoload 'mevedel--chat-buffer "mevedel-chat")
(autoload 'mevedel--ensure-chat-preset "mevedel-chat")
(autoload 'mevedel--workspace-sessions "mevedel-chat")

;; `mevedel-collaboration'
(defvar mevedel-collaboration-stop-reason)

;; `mevedel-resource'
(declare-function mevedel-resource-within-root-p "mevedel-resource" (path root))
(autoload 'mevedel-resource-within-root-p "mevedel-resource")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save "mevedel-session-artifacts"
                  (session buffer &optional settled force))
(declare-function mevedel-session-artifacts-sessions-dir
                  "mevedel-session-artifacts" (workspace))
(autoload 'mevedel-session-artifacts-save "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts")

;; `mevedel-session-naming'
(declare-function mevedel-session-naming-rename "mevedel-session-naming"
                  (session buffer name &optional current-p))
(autoload 'mevedel-session-naming-rename "mevedel-session-naming")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-delete
                  "mevedel-session-persistence" (workspace save-path))
(declare-function mevedel-session-persistence-resume-id
                  "mevedel-session-persistence" (workspace session-id))
(declare-function mevedel-session-persistence-write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(autoload 'mevedel-session-persistence-delete "mevedel-session-persistence")
(declare-function mevedel-session-persistence-list-sessions
                  "mevedel-session-persistence" (workspace &optional cached))
(autoload 'mevedel-session-persistence-list-sessions "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-resume-id "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-write-sidecar-now
  "mevedel-session-persistence")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-duplicate
                  "mevedel-shared-editing" (workspace id new-id))
(declare-function mevedel-shared-editing-restore
                  "mevedel-shared-editing" (workspace id n actor callback))
(declare-function mevedel-shared-editing-save-version
                  "mevedel-shared-editing" (workspace id &optional session-id))
(declare-function mevedel-shared-editing-call "mevedel-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function mevedel-shared-editing-save-version-later
                  "mevedel-shared-editing" (workspace id &optional session-id))
(autoload 'mevedel-shared-editing-call "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-save-version-later "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-duplicate "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-restore "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-save-version "mevedel-shared-editing")

;; `mevedel-turn'
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))
(autoload 'mevedel-turn-busy-p "mevedel-turn")

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

(defvar mevedel-artifact-store-changed-functions nil
  "Functions called with a WORKSPACE after its artifact store changed.
Live collaboration rooms follow the store through it.")

(defvar mevedel-artifact-store--pending-changes nil
  "Workspace changes collected by the current store mutation, or nil.")

(defmacro mevedel-artifact-store--with-changes (&rest body)
  "Run BODY, notifying each changed workspace once after its mutation."
  (declare (indent 0) (debug t))
  `(if mevedel-artifact-store--pending-changes
       (progn ,@body)
     (let ((mevedel-artifact-store--pending-changes (list nil)))
       (unwind-protect (progn ,@body)
         (let ((workspaces (cdr mevedel-artifact-store--pending-changes))
               (mevedel-artifact-store--pending-changes nil))
           (dolist (workspace workspaces)
             (mevedel-artifact-store--changed workspace)))))))

(defun mevedel-artifact-store--changed (workspace)
  "Notify observers of WORKSPACE, after the current mutation if collecting."
  (if mevedel-artifact-store--pending-changes
      (cl-pushnew workspace (cdr mevedel-artifact-store--pending-changes))
    (run-hook-with-args 'mevedel-artifact-store-changed-functions workspace)))

(defun mevedel-artifact-store-directory (workspace)
  "Return WORKSPACE's artifact store directory, with trailing slash."
  (file-name-as-directory
   (file-name-concat (mevedel-workspace-state-dir workspace) "artifacts")))

(defun mevedel-artifact-store-id-p (id)
  "Return non-nil when ID is a valid artifact id.
Letters, digits, `-' and `_', at most 80 of them: the rule shared items,
browser names and the migration all follow."
  (and (stringp id)
       (string-match-p "\\`[A-Za-z0-9_-]\\{1,80\\}\\'" id)))

(defun mevedel-artifact-store-artifact-directory (workspace id)
  "Return the directory of artifact ID, refusing symbolic links in the store."
  (unless (mevedel-artifact-store-id-p id)
    (error "Invalid artifact id: %S" id))
  (let ((path (file-name-concat (mevedel-artifact-store-directory workspace) id)))
    (unless (mevedel-resource-within-root-p path (mevedel-workspace-root workspace))
      (error "Artifact directory escapes the workspace: %s" path))
    path))

(defun mevedel-artifact-store-bookkeeping-directory (workspace id)
  "Return artifact ID's protected bookkeeping directory in WORKSPACE."
  (mevedel-artifact-store-artifact-directory workspace id)
  (mevedel-artifact-store--path (mevedel-artifact-store-directory workspace)
                               (file-name-concat ".state" id)))

(defun mevedel-artifact-store-primary-path (workspace id &optional meta)
  "Return artifact ID's primary file, using META when already read."
  (let ((meta (or meta (mevedel-artifact-store-meta workspace id)
                  (error "Artifact %s has no metadata" id))))
    (mevedel-artifact-store--path
     (if (mevedel-artifact-store-item-p meta)
         (mevedel-artifact-store-bookkeeping-directory workspace id)
       (mevedel-artifact-store-artifact-directory workspace id))
     (plist-get meta :file))))

(defun mevedel-artifact-store-relative (workspace path)
  "Return PATH relative to WORKSPACE's artifact store, or nil outside it."
  (let ((store (expand-file-name (mevedel-artifact-store-directory workspace)))
        (path (expand-file-name path)))
    (when (string-prefix-p store path)
      (substring path (length store)))))

(defun mevedel-artifact-store--relative-name-p (name)
  "Return non-nil when NAME is a normalized relative filename."
  (and (stringp name) (not (string-empty-p name))
       (not (file-name-absolute-p name))
       (not (string-match-p "[\\\\\0]" name))
       (not (cl-intersection (split-string name "/") '("" "." "..") :test #'equal))))

(defun mevedel-artifact-store--path (directory relative)
  "Return RELATIVE inside DIRECTORY, refusing traversal and symbolic links."
  (unless (mevedel-artifact-store--relative-name-p relative)
    (error "Invalid artifact filename: %S" relative))
  (let ((path (file-name-concat directory relative)))
    (unless (if (file-remote-p path)
                (mevedel-resource-within-root-p path directory)
              ;; RELATIVE already proves lexical containment.  Refusing every
              ;; local component proves physical containment too, including
              ;; ancestors of DIRECTORY; repeated truename walks add no proof.
              (let ((cursor "/"))
                (cl-every (lambda (component)
                            (setq cursor (file-name-concat cursor component))
                            (not (file-symlink-p cursor)))
                          (split-string (expand-file-name path) "/" t))))
      (error "Artifact filename escapes its directory: %s" relative))
    path))

(defun mevedel-artifact-store-ids (workspace)
  "Return the artifact ids in WORKSPACE's store, sorted.
Reads directory names only.  Bookkeeping ids also count because Git does
not preserve an item's empty authored directory."
  (let* ((store (mevedel-artifact-store-directory workspace))
         (bookkeeping (file-name-concat store ".state"))
         ids)
    (if (file-remote-p (mevedel-workspace-root workspace))
        (let ((result (car (mevedel-session-control-fs-run-program
                            (list (list :op 'path-exists-p :path bookkeeping
                                        :optional t))))))
          (unless (eq (plist-get result :status) 'absent)
            (mevedel-session-control-fs-program-value result)))
      (mevedel-artifact-store--path store ".state"))
    (dolist (directory (list store bookkeeping))
      (when (file-directory-p directory)
        (dolist (entry (directory-files-and-attributes directory nil "\\`[^.]" t))
          (when (and (eq t (file-attribute-type (cdr entry)))
                     (mevedel-artifact-store-id-p (car entry)))
            (push (car entry) ids)))))
    (sort (delete-dups ids) #'string<)))

(defun mevedel-artifact-store--read-bytes (file)
  "Return FILE's exact bytes, or nil if absent; signal other read failures."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (buffer-string))
    (file-missing nil)))

(defun mevedel-artifact-store--lock-directory (workspace)
  "Return WORKSPACE's stable artifact mutation lock directory, creating it."
  (let ((directory (file-name-concat (mevedel-workspace-state-dir workspace)
                                    "leases" "artifacts")))
    (unless (mevedel-resource-within-root-p directory (mevedel-workspace-root workspace))
      (error "Artifact lock directory escapes the workspace"))
    (make-directory directory t)
    directory))

(defun mevedel-artifact-store--compare-and-write (workspace file expected operations)
  "Run OPERATIONS when FILE still holds EXPECTED bytes in WORKSPACE.
Return nil on a stale expectation; signal any other failure.  All writers
share one target lock, including shared item lease changes."
  (let* ((results
          (mevedel-session-control-fs-run-program
           (cons (if expected (list :op 'verify :path file :content expected)
                   (list :op 'absent :path file))
                 operations)
           (mevedel-artifact-store--lock-directory workspace)))
         (status (plist-get (car results) :status)))
    (unless (memq status '(mismatch absent))
      (mapc #'mevedel-session-control-fs-program-value results)
      t)))

(defun mevedel-artifact-store--validate-meta (meta directory)
  "Return META after validating its fields and paths beneath DIRECTORY."
  (unless (and meta (proper-list-p meta) (zerop (% (length meta) 2))
               (memq (plist-get meta :kind) '(html markdown image file whiteboard document))
               (stringp (plist-get meta :title))
               (mevedel-artifact-store--relative-name-p (plist-get meta :file))
               (or (null (plist-get meta :dedicated-session))
                   (mevedel-artifact-store-id-p (plist-get meta :dedicated-session))))
    (error "Invalid artifact metadata: %s" directory))
  meta)

(defun mevedel-artifact-store-meta (workspace id)
  "Return validated metadata of artifact ID, or nil when absent."
  (let* ((directory (mevedel-artifact-store-artifact-directory workspace id))
         (bytes (mevedel-artifact-store--read-bytes
                 (mevedel-artifact-store--path
                  (mevedel-artifact-store-bookkeeping-directory workspace id) "meta.el"))))
    (when bytes
      (mevedel-artifact-store--validate-meta
       (read (decode-coding-string bytes 'utf-8-unix)) directory))))

(defun mevedel-artifact-store--kind (file)
  "Return the artifact kind for FILE's extension."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "html" "htm") 'html)
    ((or "md" "markdown") 'markdown)
    ((or "png" "jpg" "jpeg" "gif" "svg" "webp") 'image)
    (_ 'file)))

(defconst mevedel-artifact-store-item-kinds '(whiteboard document)
  "Kinds of artifact edited live through shared editing, not by file.")

(defun mevedel-artifact-store-item-p (meta)
  "Return non-nil when META describes a whiteboard or document."
  (memq (plist-get meta :kind) mevedel-artifact-store-item-kinds))

(defun mevedel-artifact-store-create-meta (workspace id file &optional kind title)
  "Create artifact ID's metadata, preserving another creator's winning record.
FILE names the primary file.  KIND defaults to what its name says and
TITLE to ID, which names what the artifact shows where its file is often
just index.html."
  (let* ((directory (mevedel-artifact-store-artifact-directory workspace id))
         (path (mevedel-artifact-store--path
                (mevedel-artifact-store-bookkeeping-directory workspace id) "meta.el"))
         (meta (mevedel-artifact-store--validate-meta
                (list :kind (or kind (mevedel-artifact-store--kind file))
                      :title (or title id)
                      :file file :created (format-time-string "%FT%T%z")) directory)))
    (make-directory directory t)
    (make-directory (file-name-directory path) t)
    (if (mevedel-artifact-store--compare-and-write
         workspace path nil (list (list :op 'write :path path :content (let ((print-length nil) (print-level nil)) (prin1-to-string meta)))))
        (progn (mevedel-artifact-store--changed workspace) meta)
      (or (mevedel-artifact-store-meta workspace id)
          (error "Artifact %s disappeared during creation" id)))))

(defun mevedel-artifact-store--versions-dir (workspace id)
  "Return the versions directory of artifact ID."
  (mevedel-artifact-store--path
   (mevedel-artifact-store-bookkeeping-directory workspace id) "versions"))

(defun mevedel-artifact-store--version-index (bytes)
  "Decode and validate version index BYTES without probing its files."
  (let ((rows (and bytes (read (decode-coding-string bytes 'utf-8-unix))))
        (last-number 0))
    (unless (proper-list-p rows) (error "Invalid artifact version index"))
    (dolist (row rows)
      (unless (and (proper-list-p row) (zerop (% (length row) 2))
                   (integerp (plist-get row :n)) (> (plist-get row :n) last-number)
                   (natnump (plist-get row :bytes))
                   (numberp (plist-get row :time))
                   (stringp (plist-get row :file))
                   (equal (file-name-nondirectory (plist-get row :file))
                          (plist-get row :file))
                   (string-match-p
                    (concat "\\`" (format "%06d" (plist-get row :n)) "\\(?:\\..+\\)?\\'")
                    (plist-get row :file)))
        (error "Invalid artifact version index"))
      (setq last-number (plist-get row :n)))
    rows))

(defun mevedel-artifact-store-versions (workspace id)
  "Return validated version plists for artifact ID, oldest first."
  (let ((directory (mevedel-artifact-store--versions-dir workspace id)))
    (mevedel-artifact-store--version-index
     (mevedel-artifact-store--read-bytes
      (mevedel-artifact-store--path directory "index.el")))))

(defun mevedel-artifact-store-version-path (workspace id n)
  "Return the file holding version N of artifact ID."
  (if-let* ((entry (cl-find n (mevedel-artifact-store-versions workspace id)
                            :key (lambda (row) (plist-get row :n)))))
      (mevedel-artifact-store--path (mevedel-artifact-store--versions-dir workspace id)
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

(defun mevedel-artifact-store-record-version (workspace id &optional session-id content restore)
  "Record artifact ID's primary file, or CONTENT, as a new version.
SESSION-ID identifies its writer.  Concurrent writers retry index publication
under the shared target lock, preserving each version.  When RESTORE is non-nil,
write CONTENT to the primary file in the same program.  Return its number."
  (let* ((meta (or (mevedel-artifact-store-meta workspace id)
                   (error "Artifact %s has no metadata" id)))
         (source (mevedel-artifact-store-primary-path workspace id meta))
         (directory (mevedel-artifact-store--versions-dir workspace id))
         (index (mevedel-artifact-store--path directory "index.el"))
         (content (or content (mevedel-session-control-fs-read-file source 'no-conversion)))
         (content (if (multibyte-string-p content)
                      (encode-coding-string content 'utf-8-unix) content))
         (bytes (string-bytes content))
         (attempts 0)
         number)
    (make-directory directory t)
    (while (and (null number) (< (cl-incf attempts) 9))
      (let* ((before (mevedel-artifact-store--read-bytes index))
             (versions (mevedel-artifact-store--version-index before))
             (n (1+ (or (plist-get (car (last versions)) :n) 0)))
             (name (format "%06d%s" n (or (file-name-extension source t) "")))
             (versions (append versions
                               (list (list :n n :file name :time (truncate (float-time))
                                           :session session-id :bytes bytes))))
             (kept (mevedel-artifact-store--prune versions)))
        (when (mevedel-artifact-store--compare-and-write
               workspace index before
               (append
                (list (list :op 'path-exists-p
                            :path (mevedel-artifact-store--path
                                   (mevedel-artifact-store-bookkeeping-directory workspace id) "meta.el"))
                      (list :op 'write :path (mevedel-artifact-store--path directory name) :content content))
                ;; Authored destinations need the pinned inline writer;
                ;; staging a payload there would follow mutable parent names.
                (when restore
                  (list (list :op 'write-mode :path source
                              :content (concat (format "%o\n" (or (file-modes source) #o600)) content))))
                (list (list :op 'write :path index :content (let ((print-length nil) (print-level nil)) (prin1-to-string kept))))
                ;; Index first: a crash leaves an orphan copy, never a dangling entry.
                (cl-loop for row in versions unless (memq row kept)
                         collect (list :op 'delete-file
                                       :path (mevedel-artifact-store--path directory (plist-get row :file))))))
          (setq number n))))
    (unless number (error "Artifact %s version index is busy; retry" id))
    (mevedel-artifact-store--changed workspace)
    number))

(defun mevedel-artifact-store-save-version (workspace id &optional session-id)
  "Record artifact ID's current state as a new version and return its number.
A file artifact records its primary file.  A whiteboard or document
records its reduced state through its editing queue, after any save in
progress; that returns nil.  SESSION-ID names the session whose work it is."
  (if (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
      (progn (mevedel-shared-editing-save-version-later workspace id session-id)
             nil)
    (mevedel-artifact-store-record-version workspace id session-id)))

(defun mevedel-artifact-store-restore-version
    (workspace id n &optional session-id actor callback)
  "Restore version N of artifact ID and record its newest version.
SESSION-ID identifies the restoring session.  A whiteboard or document
restores through its editing queue as one revertible edit by ACTOR
\(default \"Host\").  CALLBACK receives a plist with :n after saving, or
:error on failure, including a restore saved without its version.
Without CALLBACK, synchronous errors signal and asynchronous errors are
reported to the echo area.  Return the new version number for files,
nil for queued shared items."
  (let ((meta (mevedel-artifact-store-meta workspace id)))
    (if (mevedel-artifact-store-item-p meta)
        (progn
          (mevedel-shared-editing-restore
           workspace id n (or actor "Host")
           (lambda (reply)
             (let ((result
                    (if (plist-get reply :error) reply
                      (condition-case err
                          (list :n (mevedel-shared-editing-save-version
                                    workspace id session-id))
                        (error (list :error
                                     (format "Restored; no version saved: %s"
                                             (error-message-string err))))))))
               (if callback (funcall callback result)
                 (when (plist-get result :error)
                   (message "mevedel: restoring %s: %s"
                            id (plist-get result :error)))))))
          nil)
      (let ((result
             (condition-case err
                 (list :n (mevedel-artifact-store--restore-file
                           workspace id n meta session-id))
               (error (if callback (list :error (error-message-string err))
                        (signal (car err) (cdr err)))))))
        (when callback (funcall callback result))
        (plist-get result :n)))))

(defun mevedel-artifact-store--restore-file (workspace id n meta session-id)
  "Copy version N of file artifact ID, described by META, back as newest.
SESSION-ID is recorded as the restoring session."
  (unless meta (error "Artifact %s has no metadata" id))
  (mevedel-artifact-store-record-version
   workspace id session-id
   (mevedel-session-control-fs-read-file
    (mevedel-artifact-store-version-path workspace id n) 'no-conversion) t))

(defun mevedel-artifact-store-duplicate (workspace id new-id)
  "Copy artifact ID to the new, independent artifact NEW-ID.
The copy starts with one version and its own metadata."
  (unless (mevedel-artifact-store-id-p new-id)
    (error "Name the copy with letters, digits, dashes or underscores"))
  (if (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
      (mevedel-shared-editing-duplicate workspace id new-id)
    (mevedel-artifact-store--duplicate-files workspace id new-id)))

(defun mevedel-artifact-store--duplicate-files (workspace id new-id)
  "Copy file artifact ID's directory to NEW-ID, cleaning up partial copies."
  (mevedel-artifact-store--with-changes
    (let* ((source (mevedel-artifact-store-artifact-directory workspace id))
           (target (mevedel-artifact-store-artifact-directory workspace new-id))
           (bookkeeping (mevedel-artifact-store-bookkeeping-directory workspace new-id))
           (meta (or (mevedel-artifact-store-meta workspace id)
                     (error "Artifact %s has no metadata" id)))
           (created nil)
           (bookkeeping-created nil)
           directories
           (finished nil))
      (when (file-exists-p bookkeeping)
        (error "Artifact %s already has bookkeeping" new-id))
      ;; Reserve the target before copying; never remove another creator's directory.
      (unless (mevedel-session-control-fs-make-directory target)
        (error "Artifact %s already exists" new-id))
      (setq created t)
      (unwind-protect
          (progn
            (make-directory bookkeeping)
            (setq bookkeeping-created t)
            (let (files modes)
              (dolist (file (directory-files-recursively source "." t))
                (let ((relative (file-relative-name file source)))
                  (mevedel-artifact-store--path source relative)
                  (if (file-directory-p file)
                      (push (cons relative (file-modes file)) directories)
                    (push file files)
                    (push (file-modes file) modes))))
              ;; Capture source bytes through pinned parents too: an authored
              ;; source may change into a link after the inventory was read.
              (let* ((contents
                      (mapcar #'mevedel-session-control-fs-program-value
                              (mevedel-session-control-fs-run-program
                               (mapcar (lambda (file)
                                         (list :op 'read :path file :coding 'no-conversion))
                                       files))))
                     (operations
                      (append
                       (mapcar (lambda (entry)
                                 (list :op 'make-directory :path (file-name-concat target (car entry))))
                               (setq directories (sort directories :key #'car :lessp #'string<)))
                       (cl-mapcar
                        (lambda (file content mode)
                          (list :op 'write-mode
                                :path (file-name-concat target (file-relative-name file source))
                                :content (concat (format "%o\n" mode) content)))
                        files contents modes)
                       ;; Restrictive directory modes go last so descendants
                       ;; can be copied, deepest first so parents stay traversable.
                       (mapcar (lambda (entry)
                                 (list :op 'directory-mode :path (file-name-concat target (car entry))
                                       :content (format "%o" (cdr entry))))
                               (reverse directories)))))
                (mapc #'mevedel-session-control-fs-program-value
                      (mevedel-session-control-fs-run-program operations
                                                              (mevedel-artifact-store--lock-directory workspace)))))
            (mevedel-artifact-store-create-meta workspace new-id (plist-get meta :file)
                                                (plist-get meta :kind) (plist-get meta :title))
            (mevedel-artifact-store-record-version workspace new-id)
            (setq finished t)
            new-id)
        (when (and created (not finished))
          (unwind-protect
              ;; Copies may already have readonly source directory modes.
              ;; Restore owner access through pinned descriptors before removal.
              (mevedel-session-control-fs-program-value
               (car (last (mevedel-session-control-fs-run-program
                           (append
                            (mapcar (lambda (entry)
                                      (list :op 'directory-mode :path (file-name-concat target (car entry))
                                            :content "700" :optional t))
                                    (sort directories :key #'car :lessp #'string<))
                            (list (list :op 'delete-directory :path target)))))))
            (when bookkeeping-created (delete-directory bookkeeping t))))))))

(defun mevedel-artifact-store-delete (workspace id &optional actor callback)
  "Delete artifact ID with its metadata, versions and dedicated session.
A whiteboard or document is deleted through its editing queue, after any
save in progress, so its editors learn of it from ACTOR (default
\"Host\").  CALLBACK receives nil once the artifact is gone or the
reason it was not deleted; without one, a refusal is a message."
  (let ((callback (or callback
                      (lambda (failure)
                        (when failure
                          (message "mevedel: %s was not deleted: %s" id failure))))))
    (if (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id))
        (mevedel-shared-editing-call
         workspace (list :action "delete" :id id :actor (or actor "Host"))
         (lambda (reply) (funcall callback (plist-get reply :error))))
      (funcall callback (condition-case err
                            (progn (mevedel-artifact-store--delete workspace id) nil)
                          (error (error-message-string err)))))))

(defun mevedel-artifact-store--delete (workspace id)
  "Delete artifact ID's directories and dedicated session now.
A dedicated session open in this Emacs is closed first, and its room
ended, unless a turn is still running there.  One that cannot be deleted
yet -- held by another client, or pinned by pending journal capture --
stays as an ordinary session and expires like one.  A session that does
not record ID as its own, such as one a copied `meta.el' names, is kept."
  (when-let* ((session-id (plist-get (mevedel-artifact-store-meta workspace id)
                                     :dedicated-session))
              ((mevedel-artifact-store--conversation-agrees-p workspace session-id id)))
    (when-let* ((buffer (mevedel-artifact-store--live-buffer workspace session-id)))
      (when (mevedel-turn-busy-p buffer)
        (error "The conversation of %s is still working; stop it first" id))
      ;; Opening an item from the lobby opens this session for its room, so
      ;; its buffer is no reason to keep the artifact.
      (let ((kill-buffer-query-functions nil)
            (mevedel-collaboration-stop-reason 'artifact-deleted))
        (kill-buffer buffer)))
    (let ((save-path (file-name-concat
                      (mevedel-session-artifacts-sessions-dir workspace) session-id)))
      (when (and (file-directory-p save-path)
                 (not (mevedel-session-persistence-delete workspace save-path)))
        (message "mevedel: the conversation of %s is still in use; it stays as an ordinary session"
                 id))))
  (let* ((item (mevedel-artifact-store-item-p (mevedel-artifact-store-meta workspace id)))
         (operations
          (append
           (list (list :op 'delete-directory
                       :path (mevedel-artifact-store-artifact-directory workspace id))
                 (list :op 'delete-directory :optional t
                       :path (mevedel-artifact-store-bookkeeping-directory workspace id)))
           (when item
             (list (list :op 'delete-directory
                         :path (mevedel-artifact-lease-directory workspace id))))))
         (results
          (if item (mevedel-artifact-lease-run workspace id operations)
            (mevedel-session-control-fs-run-program
             operations (mevedel-artifact-store--lock-directory workspace)))))
    (dolist (result results)
      (unless (eq (plist-get result :status) 'absent)
        (mevedel-session-control-fs-program-value result)))
    (when item
      (mevedel-artifact-lease--forget (mevedel-artifact-lease-directory workspace id))))
  (mevedel-artifact-store--changed workspace))

(defun mevedel-artifact-store-list (workspace)
  "Return WORKSPACE's artifacts as plists, newest modification first.
Each has :id, :kind, :title, :file, :dedicated-session, :path, :size,
:modified and :versions; a missing primary file has :missing t.  An
artifact whose metadata cannot be read is left out."
  (let* ((ids (mevedel-artifact-store-ids workspace))
         (store (mevedel-artifact-store-directory workspace))
         (root (mevedel-workspace-root workspace))
         (remote (file-remote-p root))
         (operations
          (cl-loop for id in ids
                   unless (mevedel-artifact-store-id-p id)
                   do (error "Invalid artifact id: %S" id)
                   append
                   (let ((directory (file-name-concat store ".state" id)))
                     (list (list :op 'read :optional t :coding 'no-conversion
                                 :path (file-name-concat directory "meta.el"))
                           (list :op 'read :optional t :coding 'no-conversion
                                 :path (file-name-concat directory "versions" "index.el"))))))
         batches results rows)
    ;; The target proves each opened parent.  Repeating model-side symlink
    ;; walks before these same proofs costs hundreds of remote round trips.
    ;; Keep batches within the control reader's archive-transfer bound.
    (while operations
      (push (mevedel-session-control-fs-run-program
             (cl-loop repeat 32 while operations collect (pop operations)))
            batches))
    (setq results (apply #'append (nreverse batches)))
    (dolist (id ids)
      (let* ((metadata-result (pop results))
             (index-result (pop results))
             (metadata (unless (eq (plist-get metadata-result :status) 'absent)
                         (ignore-errors
                           (mevedel-session-control-fs-program-value metadata-result))))
             (index (unless (eq (plist-get index-result :status) 'absent)
                      (ignore-errors
                        (mevedel-session-control-fs-program-value index-result)))))
        (when-let* ((directory (file-name-concat store ".state" id))
                    ;; Unreadable metadata leaves its artifact out rather
                    ;; than hiding every other one.
                    (meta (and metadata
                               (ignore-errors
                                 (mevedel-artifact-store--validate-meta
                                  (read (decode-coding-string metadata 'utf-8-unix))
                                  directory)))))
          (let ((path (file-name-concat
                       (if (mevedel-artifact-store-item-p meta)
                           directory (file-name-concat store id))
                       (plist-get meta :file))))
            (when (or remote
                      ;; A linked primary file leaves its artifact out too.
                      (ignore-errors
                        (mevedel-artifact-store--path root (file-relative-name path root))))
              (push (list :id id :kind (plist-get meta :kind)
                        :title (plist-get meta :title) :path path
                        :file (plist-get meta :file)
                        :dedicated-session (plist-get meta :dedicated-session)
                          :versions (length (ignore-errors
                                              (mevedel-artifact-store--version-index index))))
                    rows))))))
    ;; One target proof replaces every remote component/truename walk.  The
    ;; following stat is observational, just as with the local path proof.
    (when remote
      (cl-mapc
       (lambda (row result)
         (if (eq (plist-get result :status) 'absent)
             (nconc row (list :missing t :size 0 :modified 0))
           (mevedel-session-control-fs-program-value result)))
       rows
       (mevedel-session-control-fs-run-program
        (mapcar (lambda (row)
                  (list :op 'path-exists-p :path (plist-get row :path) :optional t))
                rows))))
    (dolist (row rows)
      (unless (plist-get row :missing)
        (let ((attributes (file-attributes (plist-get row :path))))
          (nconc row (if attributes
                         (list :size (file-attribute-size attributes)
                               :modified (file-attribute-modification-time attributes))
                       (list :missing t :size 0 :modified 0))))))
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
      (mevedel-session-persistence-write-sidecar-now session buffer))
    (mevedel-artifact-store--changed (mevedel-session-workspace session))))

(defun mevedel-artifact-store-update-meta (workspace id &rest properties)
  "Set PROPERTIES in artifact ID's metadata, retaining concurrent updates.
Return the new metadata."
  (let ((file (mevedel-artifact-store--path
               (mevedel-artifact-store-bookkeeping-directory workspace id) "meta.el"))
        (attempts 0)
        updated)
    (while (and (null updated) (< (cl-incf attempts) 9))
      (let* ((before (mevedel-artifact-store--read-bytes file))
             (meta (or (and before (read (decode-coding-string before 'utf-8-unix)))
                       (error "Artifact %s has no metadata" id)))
             (remaining properties))
        (while remaining
          (setq meta (plist-put meta (pop remaining) (pop remaining))))
        (mevedel-artifact-store--validate-meta
         meta (mevedel-artifact-store-artifact-directory workspace id))
        (when (mevedel-artifact-store--compare-and-write
               workspace file before (list (list :op 'write :path file :content (let ((print-length nil) (print-level nil)) (prin1-to-string meta)))))
          (setq updated meta))))
    (unless updated (error "Artifact %s metadata is busy; retry" id))
    ;; A new title or conversation shows in every listing.
    (mevedel-artifact-store--changed workspace)
    updated))

(defun mevedel-artifact-store-dedicated-p (workspace session-id id)
  "Return non-nil when SESSION-ID is the conversation of WORKSPACE's artifact ID.
ID is what the session records as its artifact; the artifact must name the
session back, so a copied `meta.el' or a deleted artifact does not make
another session its conversation."
  (and (mevedel-artifact-store-id-p id)
       (equal session-id
              (plist-get (ignore-errors (mevedel-artifact-store-meta workspace id))
                         :dedicated-session))))

(defun mevedel-artifact-store--conversation-agrees-p (workspace session-id id)
  "Return non-nil when session SESSION-ID records WORKSPACE's artifact ID.
A session that does not, such as one a copied `meta.el' names, is never
deleted with ID."
  (if-let* ((buffer (mevedel-artifact-store--live-buffer workspace session-id)))
      (equal id (mevedel-session-dedicated-artifact
                 (buffer-local-value 'mevedel--session buffer)))
    (cl-some (lambda (entry)
               (let ((summary (plist-get entry :summary)))
                 (and (equal session-id (plist-get summary :session-id))
                      (equal id (plist-get summary :dedicated-artifact)))))
             (mevedel-session-persistence-list-sessions workspace))))

(defun mevedel-artifact-store--live-buffer (workspace session-id)
  "Return the live root data buffer of WORKSPACE's SESSION-ID, or nil."
  (cdr (cl-find-if (lambda (entry)
                     (equal session-id
                            (mevedel-session-session-id
                             (buffer-local-value 'mevedel--session (cdr entry)))))
                   (mevedel--workspace-sessions workspace))))

(defun mevedel-artifact-store-session-buffer (workspace session-id)
  "Return the data buffer of WORKSPACE's SESSION-ID, resuming it when saved.
Return nil when the session no longer exists."
  (or (mevedel-artifact-store--live-buffer workspace session-id)
      (mevedel-session-persistence-resume-id workspace session-id)))

(defun mevedel-artifact-store--claim-conversation (workspace id previous candidate)
  "Claim ID's conversation for CANDIDATE if it still names PREVIOUS.
Return the winning session id, preserving concurrent metadata updates."
  (let ((file (mevedel-artifact-store--path
               (mevedel-artifact-store-bookkeeping-directory workspace id) "meta.el"))
        (attempts 0)
        winner)
    (while (and (not winner) (< (cl-incf attempts) 9))
      (let* ((before (or (mevedel-artifact-store--read-bytes file)
                         (error "No artifact %s" id)))
             (meta (mevedel-artifact-store--validate-meta
                    (read (decode-coding-string before 'utf-8-unix))
                    (mevedel-artifact-store-artifact-directory workspace id)))
             (current (plist-get meta :dedicated-session)))
        (if (not (equal previous current))
            (setq winner (or current (error "Artifact conversation changed; retry")))
          (setq meta (plist-put meta :dedicated-session candidate))
          (when (mevedel-artifact-store--compare-and-write
                 workspace file before
                 (list (list :op 'write :path file
                             :content (let ((print-length nil) (print-level nil))
                                        (prin1-to-string meta)))))
            (setq winner candidate)))))
    (or winner (error "Artifact %s metadata is busy; retry" id))))

(defun mevedel-artifact-store-conversation (workspace id)
  "Return the data buffer of artifact ID's dedicated session.
Save a new conversation before publishing its id.  If another Emacs wins
publication, discard this unused candidate and open the winner."
  (mevedel-artifact-store--with-changes
    (let ((previous (plist-get (or (mevedel-artifact-store-meta workspace id)
                                   (error "No artifact %s" id))
                              :dedicated-session)))
      (or (and previous (mevedel-artifact-store-session-buffer workspace previous))
          (let* ((buffer (mevedel--chat-buffer nil t workspace))
                 (session (buffer-local-value 'mevedel--session buffer))
                 published winner)
            (unwind-protect
                (progn
                  (mevedel--ensure-chat-preset buffer)
                  (setf (mevedel-session-dedicated-artifact session) id)
                  (mevedel-artifact-store-attach session id)
                  (mevedel-session-artifacts-save session buffer nil t)
                  (mevedel-session-naming-rename session buffer (format "Artifact %s" id))
                  (setq winner (mevedel-artifact-store--claim-conversation
                                workspace id previous (mevedel-session-session-id session))
                        published (equal winner (mevedel-session-session-id session))))
              (unless published
                (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))
                (when-let* ((path (mevedel-session-save-path session)))
                  (mevedel-session-persistence-delete workspace path))))
            (if published buffer
              (or (mevedel-artifact-store-session-buffer workspace winner)
                  (error "Artifact conversation disappeared; retry"))))))))

(defun mevedel-artifact-store-note-writes (session changes &optional request)
  "Record settled ApplyPatch CHANGES of SESSION that land in the store.
A write into a new id directory creates the artifact, and any write
attaches SESSION.  A write of an artifact's primary file is versioned once
REQUEST settles, so a turn's many small patches leave one version and the
one before the turn survives; without a request the written bytes are
versioned now."
  (mevedel-artifact-store--with-changes
    (when-let* ((workspace (mevedel-session-workspace session)))
      (dolist (change changes)
        (let* ((relative (and (eq (plist-get change :action) 'write)
                              (mevedel-artifact-store-relative
                               workspace (plist-get change :path))))
               (slash (and relative (string-search "/" relative)))
               (id (and slash (substring relative 0 slash)))
               (file (and slash (substring relative (1+ slash)))))
          ;; ponytail: only the primary file is versioned; secondary files
          ;; (assets) change without versions.
          (when (mevedel-artifact-store-id-p id)
            (let ((meta (or (mevedel-artifact-store-meta workspace id)
                            (mevedel-artifact-store-create-meta
                             workspace id file))))
              (when (equal file (plist-get meta :file))
                (if request
                    (cl-pushnew (cons workspace id)
                                (mevedel-request-edited-artifacts request)
                                :test #'equal)
                  (mevedel-artifact-store-record-version
                   workspace id (mevedel-session-session-id session)
                   (or (plist-get change :bytes) (plist-get change :content)))))
              (mevedel-artifact-store-attach session id))))))))

(provide 'mevedel-artifact-store)
;;; mevedel-artifact-store.el ends here
