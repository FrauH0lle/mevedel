;;; niwa-artifact-store.el --- Workspace artifact store -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace artifact store at `<workspace>/.niwa/artifacts/'.  Each
;; artifact has a directory named by its stable id for authored files.
;; Protected `.state/ID/' directories hold host bookkeeping: `meta.el',
;; numbered versions with `index.el', `discussions.json' and shared editor
;; `state.json'.  Sessions attach to artifacts by id.  An artifact may have
;; one dedicated session, created on demand for conversations outside any chat.
;; The two name each other; while they agree the session is hidden from session
;; lists, kept from expiry, and deleted with the artifact.
;;
;; A settled ApplyPatch reports its writes here.  A write into a new id
;; directory creates the artifact; a turn that wrote its primary file records
;; one version when it settles.  Versions are capped by count and bytes, the
;; latest always kept.
;;
;; Artifact links read and change a file artifact's files through the
;; guarded file operations at the end, each change checked against the
;; SHA-256 its writer last saw.

;;; Code:

(require 'niwa-utilities)

(require 'cl-lib)
(require 'subr-x)
(require 'niwa-structs)
(require 'niwa-utilities)
(require 'niwa-workspace)
(require 'niwa-session-control-fs)

;; `niwa-artifact-lease'
(declare-function niwa-artifact-lease--forget "niwa-artifact-lease" (directory))
(declare-function niwa-artifact-lease-directory "niwa-artifact-lease" (workspace id))
(declare-function niwa-artifact-lease-run "niwa-artifact-lease" (workspace id operations))
(autoload 'niwa-artifact-lease--forget "niwa-artifact-lease")
(autoload 'niwa-artifact-lease-directory "niwa-artifact-lease")
(autoload 'niwa-artifact-lease-run "niwa-artifact-lease")

;; `niwa-chat'
(declare-function niwa--chat-buffer "niwa-chat"
                  (session-name &optional create workspace working-directory))
(declare-function niwa--ensure-chat-preset "niwa-chat" (chat-buffer))
(declare-function niwa--workspace-sessions "niwa-chat" (workspace))
(autoload 'niwa--chat-buffer "niwa-chat")
(autoload 'niwa--ensure-chat-preset "niwa-chat")
(autoload 'niwa--workspace-sessions "niwa-chat")

;; `niwa-collaboration'
(defvar niwa-collaboration-stop-reason)

;; `niwa-resource'
(declare-function niwa-resource-within-root-p "niwa-resource" (path root))
(autoload 'niwa-resource-within-root-p "niwa-resource")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-save "niwa-session-artifacts"
                  (session buffer &optional settled force))
(declare-function niwa-session-artifacts-sessions-dir
                  "niwa-session-artifacts" (workspace))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-sessions-dir "niwa-session-artifacts")

;; `niwa-session-naming'
(declare-function niwa-session-naming-rename "niwa-session-naming"
                  (session buffer name &optional current-p))
(autoload 'niwa-session-naming-rename "niwa-session-naming")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-delete
                  "niwa-session-persistence" (workspace save-path))
(declare-function niwa-session-persistence-resume-id
                  "niwa-session-persistence" (workspace session-id))
(declare-function niwa-session-persistence-write-sidecar-now
                  "niwa-session-persistence" (session buffer))
(autoload 'niwa-session-persistence-delete "niwa-session-persistence")
(declare-function niwa-session-persistence-list-sessions
                  "niwa-session-persistence" (workspace &optional cached))
(autoload 'niwa-session-persistence-list-sessions "niwa-session-persistence")
(autoload 'niwa-session-persistence-resume-id "niwa-session-persistence")
(autoload 'niwa-session-persistence-write-sidecar-now
  "niwa-session-persistence")

;; `niwa-shared-editing'
(declare-function niwa-shared-editing-duplicate
                  "niwa-shared-editing" (workspace id new-id))
(declare-function niwa-shared-editing-restore
                  "niwa-shared-editing" (workspace id n actor callback))
(declare-function niwa-shared-editing-save-version
                  "niwa-shared-editing" (workspace id &optional session-id))
(declare-function niwa-shared-editing-call "niwa-shared-editing"
                  (workspace args callback &optional authorize commit))
(declare-function niwa-shared-editing-save-version-later
                  "niwa-shared-editing" (workspace id &optional session-id))
(autoload 'niwa-shared-editing-call "niwa-shared-editing")
(autoload 'niwa-shared-editing-save-version-later "niwa-shared-editing")
(autoload 'niwa-shared-editing-duplicate "niwa-shared-editing")
(autoload 'niwa-shared-editing-restore "niwa-shared-editing")
(autoload 'niwa-shared-editing-save-version "niwa-shared-editing")

;; `niwa-turn'
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-turn-busy-p "niwa-turn")

(defcustom niwa-artifact-store-max-versions 20
  "Most versions kept per artifact; the oldest are dropped first."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-artifact-store-max-version-bytes (* 64 1024 1024)
  "Most bytes of versions kept per artifact.
The oldest versions are dropped first; the latest version always stays,
even when it alone exceeds this size."
  :type 'natnum
  :group 'niwa)

(defvar niwa-artifact-store-changed-functions nil
  "Functions called with a WORKSPACE after its artifact store changed.
Live collaboration rooms follow the store through it.")

(defvar niwa-artifact-store-deleted-functions nil
  "Functions called with WORKSPACE and ID after artifact ID was deleted.
Artifact rooms end the deleted artifact's sharing through it.")

(defvar niwa-artifact-store--pending-changes nil
  "Workspace changes collected by the current store mutation, or nil.")

(defmacro niwa-artifact-store--with-changes (&rest body)
  "Run BODY, notifying each changed workspace once after its mutation."
  (declare (indent 0) (debug t))
  `(if niwa-artifact-store--pending-changes
       (progn ,@body)
     (let ((niwa-artifact-store--pending-changes (list nil)))
       (unwind-protect (progn ,@body)
         (let ((workspaces (cdr niwa-artifact-store--pending-changes))
               (niwa-artifact-store--pending-changes nil))
           (dolist (workspace workspaces)
             (niwa-artifact-store-changed workspace)))))))

(defconst niwa-artifact-store--content-change-delay 2
  "Seconds content-only changes of a workspace wait to be announced together.
An edit reaches the item's viewers on its own; listings and artifact
cards show only its size and time.  Announcing every save made each
room reread every artifact's metadata and republish after each one.")

(defvar niwa-artifact-store--content-changes (make-hash-table :test #'equal)
  "Pending content-change notification timers, by workspace root.")

(defun niwa-artifact-store-changed (workspace)
  "Notify observers of WORKSPACE, after the current mutation if collecting.
The notice also carries WORKSPACE's content changes awaiting theirs."
  (if niwa-artifact-store--pending-changes
      (cl-pushnew workspace (cdr niwa-artifact-store--pending-changes))
    (let ((root (niwa-workspace-root workspace)))
      (when-let* ((timer (gethash root niwa-artifact-store--content-changes)))
        (cancel-timer timer)
        (remhash root niwa-artifact-store--content-changes)))
    (run-hook-with-args 'niwa-artifact-store-changed-functions workspace)))

(defun niwa-artifact-store--content-changed (workspace)
  "Notify observers of a content-only change to WORKSPACE shortly.
Further content changes until then share that notification."
  (let ((root (niwa-workspace-root workspace)))
    (unless (gethash root niwa-artifact-store--content-changes)
      (puthash root
               (niwa-transport-run-at-time
                niwa-artifact-store--content-change-delay
                #'niwa-artifact-store--announce-content workspace)
               niwa-artifact-store--content-changes))))

(defun niwa-artifact-store--announce-content (workspace)
  "Announce WORKSPACE's content changes, unless a notice carried them already."
  (when (gethash (niwa-workspace-root workspace)
                 niwa-artifact-store--content-changes)
    (niwa-artifact-store-changed workspace)))

(defun niwa-artifact-store-directory (workspace)
  "Return WORKSPACE's artifact store directory, with trailing slash."
  (file-name-as-directory
   (file-name-concat (niwa-workspace-state-dir workspace) "artifacts")))

(defun niwa-artifact-store-id-p (id)
  "Return non-nil when ID is a valid artifact id.
Letters, digits, `-' and `_', at most 80 of them: the rule shared items,
browser names and the migration all follow."
  (and (stringp id)
       (string-match-p "\\`[A-Za-z0-9_-]\\{1,80\\}\\'" id)))

(defun niwa-artifact-store-artifact-directory (workspace id)
  "Return the directory of artifact ID, refusing symbolic links in the store."
  (unless (niwa-artifact-store-id-p id)
    (error "Invalid artifact id: %S" id))
  (let ((path (file-name-concat (niwa-artifact-store-directory workspace) id)))
    (unless (niwa-resource-within-root-p path (niwa-workspace-root workspace))
      (error "Artifact directory escapes the workspace: %s" path))
    path))

(defun niwa-artifact-store-bookkeeping-directory (workspace id)
  "Return artifact ID's protected bookkeeping directory in WORKSPACE."
  (niwa-artifact-store-artifact-directory workspace id)
  (niwa-artifact-store--path (niwa-artifact-store-directory workspace)
                               (file-name-concat ".state" id)))

(defun niwa-artifact-store-link-path (workspace id)
  "Return the file holding WORKSPACE's artifact-link credentials for ID.
It lives in the protected bookkeeping, so deleting the artifact removes
it and a duplicate starts without one."
  (file-name-concat (niwa-artifact-store-bookkeeping-directory workspace id)
                    "link"))

(defun niwa-artifact-store-shared-p (workspace id)
  "Return non-nil when WORKSPACE's artifact ID is shared through artifact links."
  (file-exists-p (niwa-artifact-store-link-path workspace id)))

(defun niwa-artifact-store-primary-path (workspace id &optional meta)
  "Return artifact ID's primary file, using META when already read."
  (let ((meta (or meta (niwa-artifact-store-meta workspace id)
                  (error "Artifact %s has no metadata" id))))
    (niwa-artifact-store--path
     (if (niwa-artifact-store-item-p meta)
         (niwa-artifact-store-bookkeeping-directory workspace id)
       (niwa-artifact-store-artifact-directory workspace id))
     (plist-get meta :file))))

(defun niwa-artifact-store-relative (workspace path)
  "Return PATH relative to WORKSPACE's artifact store, or nil outside it."
  (let ((store (expand-file-name (niwa-artifact-store-directory workspace)))
        (path (expand-file-name path)))
    (when (string-prefix-p store path)
      (substring path (length store)))))

(defun niwa-artifact-store--relative-name-p (name)
  "Return non-nil when NAME is a normalized relative filename."
  (and (stringp name) (not (string-empty-p name))
       (not (file-name-absolute-p name))
       (not (string-match-p "[\\\\\0]" name))
       (not (cl-intersection (split-string name "/") '("" "." "..") :test #'equal))))

(defun niwa-artifact-store--path (directory relative)
  "Return RELATIVE inside DIRECTORY, refusing traversal and symbolic links."
  (unless (niwa-artifact-store--relative-name-p relative)
    (error "Invalid artifact filename: %S" relative))
  (let ((path (file-name-concat directory relative)))
    (unless (if (file-remote-p path)
                (niwa-resource-within-root-p path directory)
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

(defun niwa-artifact-store-ids (workspace)
  "Return the artifact ids in WORKSPACE's store, sorted.
Reads directory names only.  Bookkeeping ids also count because Git does
not preserve an item's empty authored directory."
  (let* ((store (niwa-artifact-store-directory workspace))
         (bookkeeping (file-name-concat store ".state"))
         ids)
    (if (file-remote-p (niwa-workspace-root workspace))
        (let ((result (car (niwa-session-control-fs-run-program
                            (list (list :op 'path-exists-p :path bookkeeping
                                        :optional t))))))
          (unless (eq (plist-get result :status) 'absent)
            (niwa-session-control-fs-program-value result)))
      (niwa-artifact-store--path store ".state"))
    (dolist (directory (list store bookkeeping))
      (when (file-directory-p directory)
        (dolist (entry (directory-files-and-attributes directory nil "\\`[^.]" t))
          (when (and (eq t (file-attribute-type (cdr entry)))
                     (niwa-artifact-store-id-p (car entry)))
            (push (car entry) ids)))))
    (sort (delete-dups ids) #'string<)))

(defun niwa-artifact-store--read-bytes (file)
  "Return FILE's exact bytes, or nil if absent; signal other read failures."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (buffer-string))
    (file-missing nil)))

(defun niwa-artifact-store--lock-directory (workspace)
  "Return WORKSPACE's stable artifact mutation lock directory, creating it."
  (let ((directory (file-name-concat (niwa-workspace-state-dir workspace)
                                    "leases" "artifacts")))
    (unless (niwa-resource-within-root-p directory (niwa-workspace-root workspace))
      (error "Artifact lock directory escapes the workspace"))
    (make-directory directory t)
    directory))

(defun niwa-artifact-store--compare-and-write (workspace file expected operations)
  "Run OPERATIONS when FILE still holds EXPECTED bytes in WORKSPACE.
Return nil on a stale expectation; signal any other failure.  All writers
share one target lock, including shared item lease changes."
  (let* ((results
          (niwa-session-control-fs-run-program
           (cons (if expected (list :op 'verify :path file :content expected)
                   (list :op 'absent :path file))
                 operations)
           (niwa-artifact-store--lock-directory workspace)))
         (status (plist-get (car results) :status)))
    (unless (memq status '(mismatch absent))
      (mapc #'niwa-session-control-fs-program-value results)
      t)))

(defun niwa-artifact-store--validate-meta (meta directory)
  "Return META after validating its fields and paths beneath DIRECTORY."
  (unless (and meta (proper-list-p meta) (zerop (% (length meta) 2))
               (memq (plist-get meta :kind) '(html markdown image file whiteboard document))
               (stringp (plist-get meta :title))
               (niwa-artifact-store--relative-name-p (plist-get meta :file))
               (or (null (plist-get meta :dedicated-session))
                   (niwa-artifact-store-id-p (plist-get meta :dedicated-session))))
    (error "Invalid artifact metadata: %s" directory))
  meta)

(defun niwa-artifact-store-meta (workspace id)
  "Return validated metadata of artifact ID, or nil when absent."
  (let* ((directory (niwa-artifact-store-artifact-directory workspace id))
         (bytes (niwa-artifact-store--read-bytes
                 (niwa-artifact-store--path
                  (niwa-artifact-store-bookkeeping-directory workspace id) "meta.el"))))
    (when bytes
      (niwa-artifact-store--validate-meta
       (read (decode-coding-string bytes 'utf-8-unix)) directory))))

(defun niwa-artifact-store--kind (file)
  "Return the artifact kind for FILE's extension."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "html" "htm") 'html)
    ((or "md" "markdown") 'markdown)
    ((or "png" "jpg" "jpeg" "gif" "svg" "webp") 'image)
    (_ 'file)))

(defconst niwa-artifact-store-item-kinds '(whiteboard document)
  "Kinds of artifact edited live through shared editing, not by file.")

(defun niwa-artifact-store-item-p (meta)
  "Return non-nil when META describes a whiteboard or document."
  (memq (plist-get meta :kind) niwa-artifact-store-item-kinds))

(defun niwa-artifact-store-create-meta (workspace id file &optional kind title)
  "Create artifact ID's metadata, preserving another creator's winning record.
FILE names the primary file.  KIND defaults to what its name says and
TITLE to ID, which names what the artifact shows where its file is often
just index.html."
  (let* ((directory (niwa-artifact-store-artifact-directory workspace id))
         (path (niwa-artifact-store--path
                (niwa-artifact-store-bookkeeping-directory workspace id) "meta.el"))
         (meta (niwa-artifact-store--validate-meta
                (list :kind (or kind (niwa-artifact-store--kind file))
                      :title (or title id)
                      :file file :created (format-time-string "%FT%T%z")) directory)))
    (make-directory directory t)
    (make-directory (file-name-directory path) t)
    (if (niwa-artifact-store--compare-and-write
         workspace path nil (list (list :op 'write :path path :content (let ((print-length nil) (print-level nil)) (prin1-to-string meta)))))
        (progn (niwa-artifact-store-changed workspace) meta)
      (or (niwa-artifact-store-meta workspace id)
          (error "Artifact %s disappeared during creation" id)))))

(defun niwa-artifact-store--versions-dir (workspace id)
  "Return the versions directory of artifact ID."
  (niwa-artifact-store--path
   (niwa-artifact-store-bookkeeping-directory workspace id) "versions"))

(defun niwa-artifact-store--version-index (bytes)
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

(defun niwa-artifact-store-versions (workspace id)
  "Return validated version plists for artifact ID, oldest first."
  (let ((directory (niwa-artifact-store--versions-dir workspace id)))
    (niwa-artifact-store--version-index
     (niwa-artifact-store--read-bytes
      (niwa-artifact-store--path directory "index.el")))))

(defun niwa-artifact-store-version-path (workspace id n)
  "Return the file holding version N of artifact ID."
  (if-let* ((entry (cl-find n (niwa-artifact-store-versions workspace id)
                            :key (lambda (row) (plist-get row :n)))))
      (niwa-artifact-store--path (niwa-artifact-store--versions-dir workspace id)
                                   (plist-get entry :file))
    (error "Artifact %s has no version %s" id n)))

(defun niwa-artifact-store--prune (versions)
  "Return VERSIONS within the count and byte caps, oldest dropped first."
  (let ((versions (last versions (max 1 niwa-artifact-store-max-versions))))
    (while (and (cdr versions)
                (> (apply #'+ (mapcar (lambda (row) (plist-get row :bytes))
                                      versions))
                   niwa-artifact-store-max-version-bytes))
      (setq versions (cdr versions)))
    versions))

(defun niwa-artifact-store-record-version (workspace id &optional session-id content restore)
  "Record artifact ID's primary file, or CONTENT, as a new version.
SESSION-ID identifies its writer.  Concurrent writers retry index publication
under the shared target lock, preserving each version.  When RESTORE is non-nil,
write CONTENT to the primary file in the same program.  Return its number."
  (let* ((meta (or (niwa-artifact-store-meta workspace id)
                   (error "Artifact %s has no metadata" id)))
         (source (niwa-artifact-store-primary-path workspace id meta))
         (directory (niwa-artifact-store--versions-dir workspace id))
         (index (niwa-artifact-store--path directory "index.el"))
         (content (or content (niwa-session-control-fs-read-file source 'no-conversion)))
         (content (if (multibyte-string-p content)
                      (encode-coding-string content 'utf-8-unix) content))
         (bytes (string-bytes content))
         (attempts 0)
         number)
    (make-directory directory t)
    (while (and (null number) (< (cl-incf attempts) 9))
      (let* ((before (niwa-artifact-store--read-bytes index))
             (versions (niwa-artifact-store--version-index before))
             (n (1+ (or (plist-get (car (last versions)) :n) 0)))
             (name (format "%06d%s" n (or (file-name-extension source t) "")))
             (versions (append versions
                               (list (list :n n :file name :time (truncate (float-time))
                                           :session session-id :bytes bytes))))
             (kept (niwa-artifact-store--prune versions)))
        (when (niwa-artifact-store--compare-and-write
               workspace index before
               (append
                (list (list :op 'path-exists-p
                            :path (niwa-artifact-store--path
                                   (niwa-artifact-store-bookkeeping-directory workspace id) "meta.el"))
                      (list :op 'write :path (niwa-artifact-store--path directory name) :content content))
                ;; Authored destinations need the pinned inline writer;
                ;; staging a payload there would follow mutable parent names.
                (when restore
                  (list (list :op 'write-mode :path source
                              :content (concat (format "%o\n" (or (file-modes source) #o600)) content))))
                (list (list :op 'write :path index :content (let ((print-length nil) (print-level nil)) (prin1-to-string kept))))
                ;; Index first: a crash leaves an orphan copy, never a dangling entry.
                (cl-loop for row in versions unless (memq row kept)
                         collect (list :op 'delete-file
                                       :path (niwa-artifact-store--path directory (plist-get row :file))))))
          (setq number n))))
    (unless number (error "Artifact %s version index is busy; retry" id))
    (niwa-artifact-store-changed workspace)
    number))

(defun niwa-artifact-store-save-version (workspace id &optional session-id)
  "Record artifact ID's current state as a new version and return its number.
A file artifact records its primary file.  A whiteboard or document
records its reduced state through its editing queue, after any save in
progress; that returns nil.  SESSION-ID names the session whose work it is."
  (if (niwa-artifact-store-item-p (niwa-artifact-store-meta workspace id))
      (progn (niwa-shared-editing-save-version-later workspace id session-id)
             nil)
    (niwa-artifact-store-record-version workspace id session-id)))

(defun niwa-artifact-store-restore-version
    (workspace id n &optional session-id actor callback)
  "Restore version N of artifact ID and record its newest version.
SESSION-ID identifies the restoring session.  A whiteboard or document
restores through its editing queue as one revertible edit by ACTOR
\(default \"Host\").  CALLBACK receives a plist with :n after saving, or
:error on failure, including a restore saved without its version.
Without CALLBACK, synchronous errors signal and asynchronous errors are
reported to the echo area.  Return the new version number for files,
nil for queued shared items."
  (let ((meta (niwa-artifact-store-meta workspace id))
        (scope (niwa-diagnostic-scope workspace)))
    (if (niwa-artifact-store-item-p meta)
        (progn
          (niwa-shared-editing-restore
           workspace id n (or actor "Host")
           (lambda (reply)
             (let ((result
                    (if (plist-get reply :error) reply
                      (condition-case err
                          (list :n (niwa-shared-editing-save-version
                                    workspace id session-id))
                        (error (list :error
                                     (format "Restored; no version saved: %s"
                                             (error-message-string err))))))))
               (if callback (funcall callback result)
                 (when (plist-get result :error)
                   (niwa-diagnostic-message scope "Restoring %s: %s"
                            id (plist-get result :error)))))))
          nil)
      (let ((result
             (condition-case err
                 (list :n (niwa-artifact-store--restore-file
                           workspace id n meta session-id))
               (error (if callback (list :error (error-message-string err))
                        (signal (car err) (cdr err)))))))
        (when callback (funcall callback result))
        (plist-get result :n)))))

(defun niwa-artifact-store--restore-file (workspace id n meta session-id)
  "Copy version N of file artifact ID, described by META, back as newest.
SESSION-ID is recorded as the restoring session."
  (unless meta (error "Artifact %s has no metadata" id))
  (niwa-artifact-store-record-version
   workspace id session-id
   (niwa-session-control-fs-read-file
    (niwa-artifact-store-version-path workspace id n) 'no-conversion) t))

(defun niwa-artifact-store-duplicate (workspace id new-id)
  "Copy artifact ID to the new, independent artifact NEW-ID.
The copy starts with one version and its own metadata."
  (unless (niwa-artifact-store-id-p new-id)
    (error "Name the copy with letters, digits, dashes or underscores"))
  (if (niwa-artifact-store-item-p (niwa-artifact-store-meta workspace id))
      (niwa-shared-editing-duplicate workspace id new-id)
    (niwa-artifact-store--duplicate-files workspace id new-id)))

(defun niwa-artifact-store--duplicate-files (workspace id new-id)
  "Copy file artifact ID's directory to NEW-ID, cleaning up partial copies."
  (niwa-artifact-store--with-changes
    (let* ((source (niwa-artifact-store-artifact-directory workspace id))
           (target (niwa-artifact-store-artifact-directory workspace new-id))
           (bookkeeping (niwa-artifact-store-bookkeeping-directory workspace new-id))
           (meta (or (niwa-artifact-store-meta workspace id)
                     (error "Artifact %s has no metadata" id)))
           (created nil)
           (bookkeeping-created nil)
           directories
           (finished nil))
      (when (file-exists-p bookkeeping)
        (error "Artifact %s already has bookkeeping" new-id))
      ;; Reserve the target before copying; never remove another creator's directory.
      (unless (niwa-session-control-fs-make-directory target)
        (error "Artifact %s already exists" new-id))
      (setq created t)
      (unwind-protect
          (progn
            (make-directory bookkeeping)
            (setq bookkeeping-created t)
            (let (files modes)
              (dolist (file (directory-files-recursively source "." t))
                (let ((relative (file-relative-name file source)))
                  (niwa-artifact-store--path source relative)
                  (if (file-directory-p file)
                      (push (cons relative (file-modes file)) directories)
                    (push file files)
                    (push (file-modes file) modes))))
              ;; Capture source bytes through pinned parents too: an authored
              ;; source may change into a link after the inventory was read.
              (let* ((contents
                      (mapcar #'niwa-session-control-fs-program-value
                              (niwa-session-control-fs-run-program
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
                (mapc #'niwa-session-control-fs-program-value
                      (niwa-session-control-fs-run-program operations
                                                              (niwa-artifact-store--lock-directory workspace)))))
            (niwa-artifact-store-create-meta workspace new-id (plist-get meta :file)
                                                (plist-get meta :kind) (plist-get meta :title))
            (niwa-artifact-store-record-version workspace new-id)
            (setq finished t)
            new-id)
        (when (and created (not finished))
          (unwind-protect
              ;; Copies may already have readonly source directory modes.
              ;; Restore owner access through pinned descriptors before removal.
              (niwa-session-control-fs-program-value
               (car (last (niwa-session-control-fs-run-program
                           (append
                            (mapcar (lambda (entry)
                                      (list :op 'directory-mode :path (file-name-concat target (car entry))
                                            :content "700" :optional t))
                                    (sort directories :key #'car :lessp #'string<))
                            (list (list :op 'delete-directory :path target)))))))
            (when bookkeeping-created (delete-directory bookkeeping t))))))))

(defun niwa-artifact-store-delete (workspace id &optional actor callback)
  "Delete artifact ID with its metadata, versions and dedicated session.
A whiteboard or document is deleted through its editing queue, after any
save in progress, so its editors learn of it from ACTOR (default
\"Host\").  CALLBACK receives nil once the artifact is gone or the
reason it was not deleted; without one, a refusal is a message."
  (let* ((scope (niwa-diagnostic-scope workspace))
         (callback (or callback
                      (lambda (failure)
                        (when failure
                          (niwa-diagnostic-message scope "%s was not deleted: %s" id failure))))))
    (if (niwa-artifact-store-item-p (niwa-artifact-store-meta workspace id))
        (niwa-shared-editing-call
         workspace (list :action "delete" :id id :actor (or actor "Host"))
         (lambda (reply) (funcall callback (plist-get reply :error))))
      (funcall callback (condition-case err
                            (progn (niwa-artifact-store--delete workspace id) nil)
                          (error (error-message-string err)))))))

(defun niwa-artifact-store--delete (workspace id)
  "Delete artifact ID's directories and dedicated session now.
A dedicated session open in this Emacs is closed first, and its room
ended, unless a turn is still running there.  One that cannot be deleted
yet -- held by another client, or pinned by pending journal capture --
stays as an ordinary session and expires like one.  A session that does
not record ID as its own, such as one a copied `meta.el' names, is kept."
  (when-let* ((session-id (plist-get (niwa-artifact-store-meta workspace id)
                                     :dedicated-session))
              ((niwa-artifact-store--conversation-agrees-p workspace session-id id)))
    (when-let* ((buffer (niwa-artifact-store--live-buffer workspace session-id)))
      (when (niwa-turn-busy-p buffer)
        (error "The conversation of %s is still working; stop it first" id))
      ;; Opening an item from the lobby opens this session for its room, so
      ;; its buffer is no reason to keep the artifact.
      (let ((kill-buffer-query-functions nil)
            (niwa-collaboration-stop-reason 'artifact-deleted))
        (kill-buffer buffer)))
    (let ((save-path (file-name-concat
                      (niwa-session-artifacts-sessions-dir workspace) session-id)))
      (when (and (file-directory-p save-path)
                 (not (niwa-session-persistence-delete workspace save-path)))
        (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "The conversation of %s is still in use; it stays as an ordinary session"
                 id))))
  (let* ((item (niwa-artifact-store-item-p (niwa-artifact-store-meta workspace id)))
         (operations
          (append
           (list (list :op 'delete-directory
                       :path (niwa-artifact-store-artifact-directory workspace id))
                 (list :op 'delete-directory :optional t
                       :path (niwa-artifact-store-bookkeeping-directory workspace id)))
           (when item
             (list (list :op 'delete-directory
                         :path (niwa-artifact-lease-directory workspace id))))))
         (results
          (if item (niwa-artifact-lease-run workspace id operations)
            (niwa-session-control-fs-run-program
             operations (niwa-artifact-store--lock-directory workspace)))))
    (dolist (result results)
      (unless (eq (plist-get result :status) 'absent)
        (niwa-session-control-fs-program-value result)))
    (when item
      (niwa-artifact-lease--forget (niwa-artifact-lease-directory workspace id))))
  (run-hook-with-args 'niwa-artifact-store-deleted-functions workspace id)
  (niwa-artifact-store-changed workspace))

(defun niwa-artifact-store-list (workspace)
  "Return WORKSPACE's artifacts as plists, newest modification first.
Each has :id, :kind, :title, :file, :dedicated-session, :path, :size,
:modified, :versions and boolean :shared; a missing primary file has
:missing t.  Sharing observes credential entry existence, not authority.
An artifact whose metadata cannot be read is left out.  Refused sharing
paths signal an error, including symbolic links."
  (let* ((ids (niwa-artifact-store-ids workspace))
         (store (niwa-artifact-store-directory workspace))
         (root (niwa-workspace-root workspace))
         (remote (file-remote-p root))
         (operations
          (cl-loop for id in ids
                   unless (niwa-artifact-store-id-p id)
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
      (push (niwa-session-control-fs-run-program
             (cl-loop repeat 32 while operations collect (pop operations)))
            batches))
    (setq results (apply #'append (nreverse batches)))
    (dolist (id ids)
      (let* ((metadata-result (pop results))
             (index-result (pop results))
             (metadata (unless (eq (plist-get metadata-result :status) 'absent)
                         (ignore-errors
                           (niwa-session-control-fs-program-value metadata-result))))
             (index (unless (eq (plist-get index-result :status) 'absent)
                      (ignore-errors
                        (niwa-session-control-fs-program-value index-result)))))
        (when-let* ((directory (file-name-concat store ".state" id))
                    ;; Unreadable metadata leaves its artifact out rather
                    ;; than hiding every other one.
                    (meta (and metadata
                               (ignore-errors
                                 (niwa-artifact-store--validate-meta
                                  (read (decode-coding-string metadata 'utf-8-unix))
                                  directory)))))
          (let ((path (file-name-concat
                       (if (niwa-artifact-store-item-p meta)
                           directory (file-name-concat store id))
                       (plist-get meta :file))))
            (when (or remote
                      ;; A linked primary file leaves its artifact out too.
                      (ignore-errors
                        (niwa-artifact-store--path root (file-relative-name path root))))
              (push (list :id id :kind (plist-get meta :kind)
                        :title (plist-get meta :title) :path path
                        :file (plist-get meta :file)
                        :dedicated-session (plist-get meta :dedicated-session)
                          :versions (length (ignore-errors
                                              (niwa-artifact-store--version-index index))))
                    rows))))))
    ;; One target proof replaces every remote component/truename walk.  The
    ;; following stat is observational, just as with the local path proof.
    (when remote
      (cl-mapc
       (lambda (row result)
         (if (eq (plist-get result :status) 'absent)
             (nconc row (list :missing t :size 0 :modified 0))
           (niwa-session-control-fs-program-value result)))
       rows
       (niwa-session-control-fs-run-program
        (mapcar (lambda (row)
                  (list :op 'path-exists-p :path (plist-get row :path) :optional t))
                rows))))
    ;; Keep probes separate so metadata reads retain their archive transfer.
    (let ((remaining rows))
      (while remaining
        (let ((batch (cl-loop repeat 32 while remaining collect (pop remaining))))
          (cl-mapc
           (lambda (row shared) (nconc row (list :shared shared)))
           batch
           (niwa-session-control-fs-paths-exist
            (mapcar (lambda (row)
                      (file-name-concat store ".state" (plist-get row :id) "link"))
                    batch))))))
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

(defun niwa-artifact-store-attach (session id &optional buffer)
  "Attach artifact ID to SESSION.
With BUFFER, the session's data buffer, the sidecar is written at once;
otherwise the next session save carries the attachment."
  (unless (member id (niwa-session-attached-artifacts session))
    (setf (niwa-session-attached-artifacts session)
          (append (niwa-session-attached-artifacts session) (list id)))
    (when buffer
      (niwa-session-persistence-write-sidecar-now session buffer))
    (niwa-artifact-store-changed (niwa-session-workspace session))))

(defun niwa-artifact-store-update-meta (workspace id &rest properties)
  "Set PROPERTIES in artifact ID's metadata, retaining concurrent updates.
Return the new metadata."
  (let ((file (niwa-artifact-store--path
               (niwa-artifact-store-bookkeeping-directory workspace id) "meta.el"))
        (attempts 0)
        updated)
    (while (and (null updated) (< (cl-incf attempts) 9))
      (let* ((before (niwa-artifact-store--read-bytes file))
             (meta (or (and before (read (decode-coding-string before 'utf-8-unix)))
                       (error "Artifact %s has no metadata" id)))
             (remaining properties))
        (while remaining
          (setq meta (plist-put meta (pop remaining) (pop remaining))))
        (niwa-artifact-store--validate-meta
         meta (niwa-artifact-store-artifact-directory workspace id))
        (when (niwa-artifact-store--compare-and-write
               workspace file before (list (list :op 'write :path file :content (let ((print-length nil) (print-level nil)) (prin1-to-string meta)))))
          (setq updated meta))))
    (unless updated (error "Artifact %s metadata is busy; retry" id))
    ;; A new title or conversation shows in every listing.
    (niwa-artifact-store-changed workspace)
    updated))

(defun niwa-artifact-store-dedicated-p (workspace session-id id)
  "Return non-nil when SESSION-ID is the conversation of WORKSPACE's artifact ID.
ID is what the session records as its artifact; the artifact must name the
session back, so a copied `meta.el' or a deleted artifact does not make
another session its conversation."
  (and (niwa-artifact-store-id-p id)
       (equal session-id
              (plist-get (ignore-errors (niwa-artifact-store-meta workspace id))
                         :dedicated-session))))

(defun niwa-artifact-store--conversation-agrees-p (workspace session-id id)
  "Return non-nil when session SESSION-ID records WORKSPACE's artifact ID.
A session that does not, such as one a copied `meta.el' names, is never
deleted with ID."
  (if-let* ((buffer (niwa-artifact-store--live-buffer workspace session-id)))
      (equal id (niwa-session-dedicated-artifact
                 (buffer-local-value 'niwa--session buffer)))
    (cl-some (lambda (entry)
               (let ((summary (plist-get entry :summary)))
                 (and (equal session-id (plist-get summary :session-id))
                      (equal id (plist-get summary :dedicated-artifact)))))
             (niwa-session-persistence-list-sessions workspace))))

(defun niwa-artifact-store--live-buffer (workspace session-id)
  "Return the live root data buffer of WORKSPACE's SESSION-ID, or nil."
  (cdr (cl-find-if (lambda (entry)
                     (equal session-id
                            (niwa-session-session-id
                             (buffer-local-value 'niwa--session (cdr entry)))))
                   (niwa--workspace-sessions workspace))))

(defun niwa-artifact-store-session-buffer (workspace session-id)
  "Return the data buffer of WORKSPACE's SESSION-ID, resuming it when saved.
Return nil when the session no longer exists."
  (or (niwa-artifact-store--live-buffer workspace session-id)
      (niwa-session-persistence-resume-id workspace session-id)))

(defun niwa-artifact-store--claim-conversation (workspace id previous candidate)
  "Claim ID's conversation for CANDIDATE if it still names PREVIOUS.
Return the winning session id, preserving concurrent metadata updates."
  (let ((file (niwa-artifact-store--path
               (niwa-artifact-store-bookkeeping-directory workspace id) "meta.el"))
        (attempts 0)
        winner)
    (while (and (not winner) (< (cl-incf attempts) 9))
      (let* ((before (or (niwa-artifact-store--read-bytes file)
                         (error "No artifact %s" id)))
             (meta (niwa-artifact-store--validate-meta
                    (read (decode-coding-string before 'utf-8-unix))
                    (niwa-artifact-store-artifact-directory workspace id)))
             (current (plist-get meta :dedicated-session)))
        (if (not (equal previous current))
            (setq winner (or current (error "Artifact conversation changed; retry")))
          (setq meta (plist-put meta :dedicated-session candidate))
          (when (niwa-artifact-store--compare-and-write
                 workspace file before
                 (list (list :op 'write :path file
                             :content (let ((print-length nil) (print-level nil))
                                        (prin1-to-string meta)))))
            (setq winner candidate)))))
    (or winner (error "Artifact %s metadata is busy; retry" id))))

(defun niwa-artifact-store-conversation (workspace id)
  "Return the data buffer of artifact ID's dedicated session.
Save a new conversation before publishing its id.  If another Emacs wins
publication, discard this unused candidate and open the winner.  The
session is named by the artifact's title."
  (niwa-artifact-store--with-changes
    (let* ((meta (or (niwa-artifact-store-meta workspace id)
                     (error "No artifact %s" id)))
           (previous (plist-get meta :dedicated-session)))
      (or (when-let* ((buffer (and previous (niwa-artifact-store-session-buffer
                                             workspace previous))))
            ;; A conversation from before a rename takes the current title.
            (niwa-artifact-store-name-conversation workspace id)
            buffer)
          (let* ((buffer (niwa--chat-buffer nil t workspace))
                 (session (buffer-local-value 'niwa--session buffer))
                 published winner)
            (unwind-protect
                (progn
                  (niwa--ensure-chat-preset buffer)
                  (setf (niwa-session-dedicated-artifact session) id)
                  (niwa-artifact-store-attach session id)
                  (niwa-session-artifacts-save session buffer nil t)
                  (niwa-session-naming-rename
                   session buffer (niwa-artifact-store--conversation-name meta id))
                  (setq winner (niwa-artifact-store--claim-conversation
                                workspace id previous (niwa-session-session-id session))
                        published (equal winner (niwa-session-session-id session))))
              (unless published
                (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))
                (when-let* ((path (niwa-session-save-path session)))
                  (niwa-session-persistence-delete workspace path))))
            (if published buffer
              (or (niwa-artifact-store-session-buffer workspace winner)
                  (error "Artifact conversation disappeared; retry"))))))))

(defun niwa-artifact-store--conversation-name (meta id)
  "Return the session name for the conversation of artifact ID with META.
It is the artifact's title, which people recognize, and ID only when the
title is blank."
  (let ((title (string-join (split-string (or (plist-get meta :title) "")) " ")))
    (if (string-empty-p title) id title)))

(defun niwa-artifact-store-name-conversation (workspace id)
  "Give artifact ID's live conversation the artifact's current title.
Do nothing while it is not open in this Emacs: opening it renames it.  A
failed rename, such as from a session another client holds, is reported
and leaves the old name."
  (when-let* ((meta (niwa-artifact-store-meta workspace id))
              (session-id (plist-get meta :dedicated-session))
              (buffer (niwa-artifact-store--live-buffer workspace session-id))
              (session (buffer-local-value 'niwa--session buffer))
              (name (niwa-artifact-store--conversation-name meta id))
              ((not (equal name (niwa-session-name session)))))
    (condition-case-unless-debug err (niwa-session-naming-rename session buffer name)
      (error (niwa-diagnostic-message (niwa-diagnostic-scope workspace)
                                        "Could not rename an artifact conversation: %S" err)))))

(defun niwa-artifact-store-note-writes (session changes &optional request)
  "Record settled ApplyPatch CHANGES of SESSION that land in the store.
A write into a new id directory creates the artifact, and any write
attaches SESSION.  A write of an artifact's primary file is versioned once
REQUEST settles, so a turn's many small patches leave one version and the
one before the turn survives; without a request the written bytes are
versioned now."
  (niwa-artifact-store--with-changes
    (when-let* ((workspace (niwa-session-workspace session)))
      (dolist (change changes)
        (let* ((relative (and (eq (plist-get change :action) 'write)
                              (niwa-artifact-store-relative
                               workspace (plist-get change :path))))
               (slash (and relative (string-search "/" relative)))
               (id (and slash (substring relative 0 slash)))
               (file (and slash (substring relative (1+ slash)))))
          ;; ponytail: only the primary file is versioned; secondary files
          ;; (assets) change without versions.
          (when (niwa-artifact-store-id-p id)
            (let ((meta (or (niwa-artifact-store-meta workspace id)
                            (niwa-artifact-store-create-meta
                             workspace id file))))
              (when (equal file (plist-get meta :file))
                (if request
                    (cl-pushnew (cons workspace id)
                                (niwa-request-edited-artifacts request)
                                :test #'equal)
                  (niwa-artifact-store-record-version
                   workspace id (niwa-session-session-id session)
                   (or (plist-get change :bytes) (plist-get change :content)))))
              (niwa-artifact-store-attach session id))))))))


;;
;;; Files of an artifact

(defconst niwa-artifact-store-file-max-bytes (* 16 1024 1024)
  "Largest file read or written through the file operations below.")

(defconst niwa-artifact-store-stale-content-bytes (* 256 1024)
  "Files smaller than this accompany a stale refusal with their bytes.")

(defun niwa-artifact-store-file-path (workspace id file)
  "Return FILE of WORKSPACE's artifact ID as a path, or signal why not.
FILE is relative to the artifact's directory, without `.' or `..'
components, and no component on the way may be a symbolic link.  The
artifact's bookkeeping lies beside that directory, in `.state/ID', so no
such name reaches it."
  (condition-case nil
      (niwa-artifact-store--path
       (niwa-artifact-store-artifact-directory workspace id) file)
    (error
     (error "Refused path %S: name a file relative to the artifact, without `..' or symbolic links"
            file))))

(defun niwa-artifact-store-files (workspace id)
  "Return the files of WORKSPACE's artifact ID, sorted by name.
Each is (:file NAME :bytes SIZE), NAME relative to the artifact's
directory.  Symbolic links, and what lies behind them, are left out."
  (let ((directory (file-name-as-directory
                    (niwa-artifact-store-artifact-directory workspace id))))
    (sort (cl-loop for path in (when (file-directory-p directory)
                                 (directory-files-recursively directory ""))
                   unless (file-symlink-p path)
                   collect (list :file (file-relative-name path directory)
                                 :bytes (file-attribute-size (file-attributes path))))
          (lambda (a b) (string< (plist-get a :file) (plist-get b :file))))))

(defun niwa-artifact-store-read-file (workspace id file)
  "Return the bytes of FILE of WORKSPACE's artifact ID, or nil when absent.
Signal when FILE is refused, as `niwa-artifact-store-file-path'
explains, or larger than `niwa-artifact-store-file-max-bytes'."
  (let* ((path (niwa-artifact-store-file-path workspace id file))
         (result (car (niwa-session-control-fs-run-program
                       (list (list :op 'read :path path :optional t :coding 'no-conversion
                                   :max-bytes (1+ niwa-artifact-store-file-max-bytes)))))))
    (unless (eq (plist-get result :status) 'absent)
      (let ((bytes (encode-coding-string
                    (niwa-session-control-fs-program-value result) 'no-conversion)))
        (when (> (length bytes) niwa-artifact-store-file-max-bytes)
          (error "%s is larger than %d bytes" file niwa-artifact-store-file-max-bytes))
        bytes))))

(defun niwa-artifact-store-write-files (workspace id changes)
  "Make CHANGES to the files of WORKSPACE's artifact ID if each still matches.
Each change is a plist naming a `:file' relative to the artifact's
directory and the `:hash' its writer last saw, the SHA-256 of its bytes
as hex, or nil for a file that must not exist yet.  With `:delete' t
the file is deleted, which needs its hash; otherwise it is replaced by,
or created with its missing directories as, `:content', a unibyte
string of at most `niwa-artifact-store-file-max-bytes'.  Every check
and then every change run as one program under the store's lock, so
either all changes are made or none.  The artifact's primary file is
never deleted, only replaced: without it the artifact is gone.

Return (:files RESULTS), a result per change in order: (:file FILE :hash
NEW :created CREATED) or (:file FILE :deleted t).  When a file no longer
matches, change nothing and return (:stale t :file FILE :hash CURRENT)
for the first such FILE, CURRENT being nil for an absent file.  The hash
and, for an existing file, `:bytes' size are observed under the lock.
Files smaller than `niwa-artifact-store-stale-content-bytes' also
carry their unibyte `:content' from that observation.  Signal
when a change is refused.  A change notifies the store's observers."
  (let ((primary (plist-get (niwa-artifact-store-meta workspace id) :file))
        files prepare checks operations)
    (dolist (change changes)
      (let* ((file (plist-get change :file))
             (hash (plist-get change :hash))
             (content (plist-get change :content))
             (path (niwa-artifact-store-file-path workspace id file)))
        (when (member file files)
          (error "%s is changed twice; give each file one change" file))
        (push file files)
        (unless (or (null hash)
                    (and (stringp hash)
                         (let ((case-fold-search nil))
                           (string-match-p "\\`[0-9a-f]\\{64\\}\\'" hash))))
          (error "A file's hash is the SHA-256 of its bytes, as 64 lowercase hex digits"))
        (push (list :op 'verify-hash :path path :content hash
                    :snapshot-bytes niwa-artifact-store-stale-content-bytes)
              checks)
        (if (plist-get change :delete)
            (progn
              (when (equal file primary)
                (error "%s is the artifact's primary file; delete the artifact instead" file))
              (unless hash
                (error "Deleting %s needs the hash it was read with" file))
              (push (list :op 'delete-file :path path) operations))
          (unless (and (stringp content) (not (multibyte-string-p content)))
            (error "File content must be bytes"))
          (when (> (length content) niwa-artifact-store-file-max-bytes)
            (error "%s would be larger than %d bytes" file niwa-artifact-store-file-max-bytes))
          ;; Only a new file can lack its directories.
          (unless hash
            (let ((parents (butlast (split-string file "/"))))
              (dotimes (depth (length parents))
                (push (list :op 'make-directory :optional t
                            :path (niwa-artifact-store-file-path
                                   workspace id (string-join (take (1+ depth) parents) "/")))
                      prepare))))
          (push (list :op 'write-mode :path path
                      :content (concat (format "%o\n" (or (file-modes path)
                                                          (logand #o666 (default-file-modes))))
                                       content))
                operations))))
    (let* ((prepare (nreverse prepare))
           (boundary (niwa-artifact-store-artifact-directory workspace id))
           (program (append (list (list :op 'begin-files :path boundary))
                            prepare (nreverse checks) (nreverse operations)
                            (list (list :op 'commit-files :path boundary))))
           (results (niwa-session-control-fs-run-program
                     program (niwa-artifact-store--lock-directory workspace)))
           (stale (cl-loop for change in changes
                           for result in (nthcdr (1+ (length prepare)) results)
                           when (memq (plist-get result :status) '(mismatch absent))
                           return (append (list :stale t :file (plist-get change :file))
                                          (or (plist-get result :observed)
                                              (list :hash nil))))))
      (if stale
          stale
        ;; An optional step, such as a directory that exists already, may fail.
        (cl-mapc (lambda (operation result)
                   (unless (plist-get operation :optional)
                     (niwa-session-control-fs-program-value result)))
                 program results)
        (niwa-artifact-store-changed workspace)
        (list :files
              (mapcar (lambda (change)
                        (if (plist-get change :delete)
                            (list :file (plist-get change :file) :deleted t)
                          (list :file (plist-get change :file)
                                :hash (secure-hash 'sha256 (plist-get change :content))
                                :created (null (plist-get change :hash)))))
                      changes))))))

(defun niwa-artifact-store-write-file (workspace id file content hash)
  "Replace FILE of WORKSPACE's artifact ID with CONTENT if it still has HASH.
CONTENT is a unibyte string of at most
`niwa-artifact-store-file-max-bytes'.  HASH is the SHA-256 of the
bytes the writer last saw, as hex, or nil to create FILE, which must not
exist yet; its missing directories are created.  The check and the write
are one step under the store's lock.  Return (:hash NEW :created
CREATED), or a stale outcome as `niwa-artifact-store-write-files'
returns, without writing.  Signal when FILE is refused."
  (let ((outcome (niwa-artifact-store-write-files
                  workspace id (list (list :file file :hash hash :content content)))))
    (if (plist-get outcome :stale)
        outcome
      (let ((result (car (plist-get outcome :files))))
        (list :hash (plist-get result :hash) :created (plist-get result :created))))))

(defun niwa-artifact-store-delete-file (workspace id file hash)
  "Delete FILE of WORKSPACE's artifact ID if it still has HASH.
HASH is the SHA-256 of the bytes the deleter last saw, as hex.  Return
t, or a stale outcome without deleting, as
`niwa-artifact-store-write-file' does.  Signal when FILE is refused,
as for `niwa-artifact-store-write-files', which names the primary
file's deletion among them."
  (let ((outcome (niwa-artifact-store-write-files
                  workspace id (list (list :file file :hash hash :delete t)))))
    (if (plist-get outcome :stale)
        outcome
      t)))

(provide 'niwa-artifact-store)
;;; niwa-artifact-store.el ends here
