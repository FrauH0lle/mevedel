;;; mevedel-persistence.el -- Save/load instructions -*- lexical-binding: t -*-

;;; Commentary:

;; Saves and restores source-bound references and workspace-owned directives
;; across Emacs sessions.  The workspace instruction store at the end of this
;; file is their durable record; session snapshots and exported save files
;; share its shape.  The format records directive identity and authored state
;; separately from file-specific overlay presentations.  When instructions
;; are restored against a buffer whose contents have changed, native buffer
;; replacement reconciles overlay positions.  Exported save files must match
;; the current mevedel version; the store carries its own format version.
;;
;; Activating a workspace observes its store: registering a struct in
;; `mevedel-workspace-get-or-create' calls
;; `mevedel-instruction-store-activate'.  That is bounded I/O, a
;; state-directory check plus at most one read or target-side hash, so
;; callers avoid creating workspaces on hot paths.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mevedel-directive-source)
(require 'mevedel-instruction-registry)
(require 'mevedel-overlays)
(require 'mevedel-utilities)
(require 'mevedel-notes-activation)
(require 'mevedel-workspace)

;; `mevedel-directive'
(declare-function mevedel-workspace-set-directives "mevedel-directive"
                  (workspace directives))
(declare-function mevedel-directive-in-flight-p "mevedel-directive" (directive))
(autoload 'mevedel-directive-in-flight-p "mevedel-directive")
(declare-function mevedel-workspace-rewind-directives "mevedel-directive"
                  (workspace session-id target-turn))
(autoload 'mevedel-workspace-rewind-directives "mevedel-directive")

;; `mevedel-directive-persistence'
(declare-function mevedel--deserialize-directives
                  "mevedel-directive-persistence"
                  (serialized base-directory))
(declare-function mevedel--serialize-directives
                  "mevedel-directive-persistence"
                  (workspace base-directory))
(autoload 'mevedel--deserialize-directives "mevedel-directive-persistence")
(autoload 'mevedel--serialize-directives "mevedel-directive-persistence")

;; `mevedel-directive-source'
(declare-function mevedel--mark-buffer-source-missing
                  "mevedel-directive-source" (buffer))
(declare-function mevedel--reconcile-directive-sources
                  "mevedel-directive-source" (workspace))
(declare-function mevedel--restore-source-missing-directives
                  "mevedel-directive-source" (buffer &optional lexical))

;; `mevedel-instruction-edit'
(autoload 'mevedel-instruction-replace-buffer-contents "mevedel-instruction-edit")

;; `mevedel-notes-migration'
(declare-function mevedel-notes-migrate-store "mevedel-notes-migration"
                  (workspace bytes data))

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))
(autoload 'mevedel--update-instruction-overlay "mevedel-overlay-ui")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-sessions-dir
                  "mevedel-session-artifacts" (workspace))
(autoload 'mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts")
(defvar mevedel-sessions-directory)

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-file-hash
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-program-value
                  "mevedel-session-control-fs" (result))
(declare-function mevedel-session-control-fs-run-program
                  "mevedel-session-control-fs" (operations &optional lock-directory))
(declare-function mevedel-session-control-fs-run-program-async
                  "mevedel-session-control-fs" (operations callback &optional scope connected-only))
(autoload 'mevedel-session-control-fs-program-value "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-run-program "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-run-program-async "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-file-hash "mevedel-session-control-fs")

;; `mevedel-structs'
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-checkpoint "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace--create "mevedel-structs" (&rest slots))
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--file-relative-name-or-absolute
                  "mevedel-utilities" (file directory))
(declare-function mevedel-diagnostic-message "mevedel-utilities" (scope format &rest args))
(declare-function mevedel-diagnostic-scope "mevedel-utilities" (owner))
(declare-function mevedel-diagnostic-warning "mevedel-utilities" (scope type message &optional level buffer-name))
(declare-function mevedel-version "mevedel-utilities" (&optional here message))
(autoload 'mevedel-diagnostic-message "mevedel-utilities")
(autoload 'mevedel-diagnostic-scope "mevedel-utilities")
(autoload 'mevedel-diagnostic-warning "mevedel-utilities")

;; `mevedel-workspace'
(declare-function mevedel-workspace-state-dir "mevedel-workspace" (workspace))
(autoload 'mevedel-workspace-state-dir "mevedel-workspace")
(defvar mevedel--workspace)

;; `mevedel-workspace-identity'
(declare-function mevedel-workspace-identity-read "mevedel-workspace-identity" (root))

(defcustom mevedel-patch-outdated-instructions t
  "Automatically patch instructions when the save file is outdated if non-nil."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel--inhibit-file-patching nil
  "If t, `mevedel--restore-file-instructions' becomes inert.
This is sometimes necessary to prevent various hooks from interfering with the
instruction restoration process.")

(defvar mevedel--inhibit-source-missing-restore nil
  "Non-nil while a historical instruction snapshot is being installed.")

(defconst mevedel-instruction-store-format-version 2
  "Format of the workspace instruction store and instruction exports.
Independent of the package version, so two installations of different
versions can share one workspace and import each other's exports.")

(defcustom mevedel-notes-closed-max-count 100
  "Maximum number of ordinary notes retained in recently closed.
Nil disables the count limit.  Zero expires notes immediately on closure.
Migrated archives marked :expiry-exempt neither expire nor count here."
  :type '(choice (const :tag "No count limit" nil) natnum)
  :group 'mevedel)

(defcustom mevedel-notes-closed-max-age-days 30
  "Maximum age in elapsed days of ordinary recently closed notes.
Nil disables the age limit.  Expiry is checked on access and store writes;
it never removes open notes or undoes file changes."
  :type '(choice (const :tag "No age limit" nil) natnum)
  :group 'mevedel)

(defun mevedel-instruction-store--retained-closed (instructions)
  "Return a UUID table of retained closed INSTRUCTIONS, newest first."
  (let ((keep (make-hash-table :test #'equal))
        (now (float-time))
        (count 0))
    (dolist (instruction (sort (copy-sequence instructions)
                               (lambda (a b)
                                 (> (plist-get a :closed-at)
                                    (plist-get b :closed-at)))))
      (when (or (plist-get instruction :expiry-exempt)
                (and (or (null mevedel-notes-closed-max-count)
                         (< count mevedel-notes-closed-max-count))
                     (or (null mevedel-notes-closed-max-age-days)
                         (< (- now (plist-get instruction :closed-at))
                            (* 86400 mevedel-notes-closed-max-age-days)))))
        (unless (plist-get instruction :expiry-exempt) (cl-incf count))
        (puthash (plist-get (plist-get instruction :properties) 'mevedel-uuid)
                 t keep)))
    keep))

(defun mevedel-instruction-store-prune-closed (workspace)
  "Enforce WORKSPACE's recently-closed retention limits in memory.
The next ordinary store write persists expiry; no timer or remote read runs."
  (let* ((mevedel--instruction-state-key-override
          (mevedel--instruction-workspace-key workspace))
         (state (mevedel--instruction-state
                 (mevedel--instruction-operation-state-key)))
         (entries (plist-get state :closed-notes))
         (keep (mevedel-instruction-store--retained-closed
                (mapcar (lambda (entry) (plist-get entry :instruction)) entries)))
         expired)
    (setf (plist-get state :closed-notes)
          (cl-remove-if
           (lambda (entry)
             (let ((props (plist-get (plist-get entry :instruction) :properties)))
               (unless (gethash (plist-get props 'mevedel-uuid) keep)
                 ;; Do not recycle expired numbers: surviving mentions/links
                 ;; must never silently point to a different note.
                 (remhash (plist-get props 'mevedel-id)
                          (mevedel--instruction-id-usage-map))
                 (setq expired t))))
           entries))
    (when expired (mevedel-instruction-store-touch workspace))))

;;;###autoload
(defun mevedel-save-instructions (path)
  "Save instruction overlays to PATH.

Instructions are only saved if they are associated with a buffer that has an
associated file on disk.  In other words, instructions in ethereal buffers are
not saved."
  (interactive (list (read-file-name "Save instruction list to file: ")))
  (funcall #'mevedel--write-instructions-file path (file-name-directory path)
           nil nil t))

(defun mevedel--instruction-id-state-plist ()
  "Return the current instruction ID state as a serializable plist."
  (list :id-counter (mevedel--instruction-id-counter)
        :used-ids (hash-table-keys (mevedel--instruction-id-usage-map))
        :retired-ids (mevedel--instruction-retired-ids)))

(defun mevedel--instructions-saved-count (file-alist)
  "Return the number of serialized instructions in FILE-ALIST."
  (cl-loop for (_ . plist) in file-alist
           sum (length (plist-get plist :instructions))))

(defun mevedel--instruction-file-metadata (content)
  "Return lightweight file metadata for instruction snapshot CONTENT."
  (list :anchor-schema 1
        :content-hash (secure-hash 'sha256 content)
        :char-count (length content)))

(defun mevedel--instruction-entry-with-metadata (entry content)
  "Return ENTRY enriched with metadata computed from CONTENT."
  (append (mevedel--instruction-file-metadata content)
          entry))


(defun mevedel--serialize-instructions
    (&optional base-directory include-original-content)
  "Return a plist snapshot of the current workspace instructions.

BASE-DIRECTORY controls how file names are stored.  When non-nil,
file names are serialized relative to it; otherwise absolute names are
used.  When INCLUDE-ORIGINAL-CONTENT is non-nil, include full buffer
contents for position patching if the file changes before restore."
  (mevedel--instruction-activate-buffer)
  (mevedel--reconcile-directive-sources
   (mevedel--instruction-buffer-workspace (current-buffer)))
  (mevedel-instruction-store-prune-closed
   (mevedel--instruction-buffer-workspace (current-buffer)))
  (let ((file-alist ())
        (workspace (mevedel--instruction-buffer-workspace (current-buffer)))
        (base-directory (and base-directory
                             (file-name-as-directory
                              (expand-file-name base-directory)))))
    (cl-loop for cons in (mevedel--instruction-alist)
             if (bufferp (car cons))
             do (let ((buffer (car cons)))
                  (when-let* (((buffer-live-p buffer))
                              (buffer-file-name (buffer-file-name buffer)))
                    (let ((file (if base-directory
                                    (mevedel--file-relative-name-or-absolute
                                     buffer-file-name base-directory)
                                  (expand-file-name buffer-file-name))))
                      (when-let* ((instrs (mevedel--stashed-buffer-instructions
                                           buffer)))
                        (let* ((content
                                (with-current-buffer buffer
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
                               (entry
                                (mevedel--instruction-entry-with-metadata
                                 (list :instructions instrs
                                       :source-missing
                                       (buffer-local-value
                                        'mevedel--buffer-source-missing buffer))
                                 content)))
                          (when include-original-content
                            (setq entry
                                  (plist-put
                                   entry :original-content
                                   content)))
                          (push (cons file entry) file-alist))))))
             else do
             (let ((entry (copy-sequence (cdr cons))))
               (when-let* ((content
                            (or (plist-get entry :original-content)
                                (and (stringp (car cons))
                                     (file-exists-p (car cons))
                                     (with-temp-buffer
                                       (insert-file-contents (car cons))
                                       (buffer-substring-no-properties
                                        (point-min) (point-max)))))))
                 (setq entry
                       (mevedel--instruction-entry-with-metadata
                        entry content)))
               (unless (or include-original-content
                           (not (listp (cdr cons))))
                 (cl-remf entry :original-content))
               (push (cons (if base-directory
                               (mevedel--file-relative-name-or-absolute
                                (car cons) base-directory)
                             (expand-file-name (car cons)))
                           entry)
                     file-alist)))
    (dolist (closed (plist-get (mevedel--instruction-state
                               (mevedel--instruction-operation-state-key))
                              :closed-notes))
      (let* ((path (plist-get closed :file))
             (file (if base-directory
                       (mevedel--file-relative-name-or-absolute path base-directory)
                     path))
             (cell (assoc file file-alist)))
        (unless cell
          (setq cell (cons file (list :instructions nil)))
          (push cell file-alist))
        (setcdr cell (plist-put (cdr cell) :instructions
                               (append (plist-get (cdr cell) :instructions)
                                       (list (plist-get closed :instruction)))))))
    (list :format mevedel-instruction-store-format-version
          :version (mevedel-version)
          :ids (mevedel--instruction-id-state-plist)
          :directives (and workspace
                           (mevedel--serialize-directives
                            workspace base-directory))
          :files file-alist)))


(defun mevedel-persistence--write-save-file (path save-file)
  "Atomically replace PATH with the printed SAVE-FILE form.
The file is the durable record of every workspace instruction, and the
reader rejects a truncated form, so an in-place write that died midway
would lose all of them at once."
  (mevedel--write-file-atomically
   path
   (let ((print-length nil)
         (print-level nil)
         (print-circle nil))
     (prin1-to-string save-file))))

(defun mevedel--write-instructions-file
    (path &optional base-directory write-empty quiet include-original-content)
  "Write current workspace instruction snapshot to PATH.

BASE-DIRECTORY is passed to `mevedel--serialize-instructions'.
When WRITE-EMPTY is non-nil, write an empty snapshot instead of
skipping the file.  When QUIET is non-nil, suppress user messages.
When INCLUDE-ORIGINAL-CONTENT is non-nil, include full buffer
contents for position patching if the file changes before restore.
Returns the number of saved instructions."
  (let* ((save-file (mevedel--serialize-instructions
                     base-directory include-original-content))
         (file-alist (plist-get save-file :files))
         (saved-instruction-count
          (mevedel--instructions-saved-count file-alist)))
    (if (not (zerop saved-instruction-count))
        (progn
          (mevedel-persistence--write-save-file path save-file)
          (unless quiet
            (let ((file-count (length file-alist)))
              (message "Wrote %d mevedel instruction%s from %d file%s to %s"
                       saved-instruction-count
                       (if (= 1 saved-instruction-count) "" "s")
                       file-count
                       (if (= 1 file-count) "" "s")
                       path))))
      (when write-empty
        (mevedel-persistence--write-save-file path save-file))
      (when (and (not quiet) (called-interactively-p 'any))
        (message "No mevedel instructions to save")))
    saved-instruction-count))

;;;###autoload
(defun mevedel-load-instructions (path)
  "Load instruction overlays from a file specified by PATH.
The loaded instructions replace the workspace instruction store, after
the confirmation the load asks for.  This also gives a workspace waiting
for its store, such as one whose migration staged candidates, its store."
  (interactive (list (read-file-name "Instruction list file: ")))
  (let ((confirm (called-interactively-p 'any))
        (workspace (mevedel--instruction-buffer-workspace (current-buffer))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (mevedel-instruction-store-replacing
       workspace
       (lambda ()
         (mevedel--load-instructions-file path (file-name-parent-directory path)
                                          confirm nil workspace))))))

(defun mevedel--read-instructions-file (path)
  "Read the instruction snapshot form from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (read (current-buffer))))

(defun mevedel--reset-instructions-preserving-directives
    (workspace directives)
  "Clear WORKSPACE presentations while retaining authored DIRECTIVES."
  (let ((mevedel--instruction-state-key-override
         (mevedel--instruction-workspace-key workspace)))
    (dolist (entry (copy-sequence (mevedel--instruction-alist)))
      (when (and (bufferp (car entry))
                 (buffer-live-p (car entry)))
        (mevedel--mark-buffer-source-missing (car entry))))
    (mevedel--clear-instruction-state workspace)
    (mevedel-workspace-set-directives workspace directives)))

(defun mevedel--restore-preserved-directives (workspace)
  "Safely restore Source missing directive presentations in WORKSPACE."
  (dolist (file
           (delete-dups
            (cl-loop
             for directive in (mevedel-workspace-directives workspace)
             for anchor = (mevedel-directive-anchor directive)
             append
             (append
              (when-let* (((eq 'source-missing (plist-get anchor :state)))
                          (file (plist-get anchor :file))
                          ((stringp file))
                          ((file-exists-p file)))
                (list file))
              (cl-loop
               for subdirective in
               (mevedel-directive-subdirectives directive)
               for file = (plist-get
                           (mevedel-subdirective-anchor subdirective) :file)
               when (and (stringp file) (file-exists-p file))
               collect file)))))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (setq-local mevedel--workspace workspace))
      (mevedel--restore-source-missing-directives buffer))))

(defun mevedel--load-instructions-file
    (path &optional base-directory confirm quiet workspace directive-records
          preserve-directives-p)
  "Load instruction overlays from PATH into WORKSPACE.

BASE-DIRECTORY resolves relative file names in PATH.  CONFIRM prompts
before replacing existing instructions.  QUIET suppresses messages.
DIRECTIVE-RECORDS retains current authored records; PRESERVE-DIRECTIVES-P
enables that mode for an empty record list."
  (let ((save-file (mevedel--read-instructions-file path)))
    ;; The shared format version lets another installation's export, such
    ;; as a candidate staged on the other machine, load here.  Files written
    ;; before they carried it load only on the version that wrote them.
    (unless (or (eql (plist-get save-file :format)
                     mevedel-instruction-store-format-version)
                (and (not (plist-member save-file :format))
                     (equal (plist-get save-file :version) (mevedel-version))))
      (user-error "Unsupported instruction file version: %s"
                  (or (plist-get save-file :format)
                      (plist-get save-file :version) "missing")))
    (mevedel--install-instructions
     save-file (or base-directory (file-name-parent-directory path))
     confirm quiet workspace directive-records preserve-directives-p
     (expand-file-name path))))

(defun mevedel--install-instructions
    (save-file base-directory &optional confirm quiet workspace
               directive-records preserve-directives-p source lazy)
  "Install instruction snapshot SAVE-FILE into WORKSPACE.

SAVE-FILE is an already read snapshot plist; its version or format is the
caller's to check.  BASE-DIRECTORY resolves relative file names.  CONFIRM,
QUIET, DIRECTIVE-RECORDS and PRESERVE-DIRECTIVES-P are as for
`mevedel--load-instructions-file'.  SOURCE names the snapshot in messages.
LAZY restores presentations only in files already visited; the others
restore when their file is visited."
  (setq workspace (or workspace
                      (mevedel--instruction-buffer-workspace
                       (current-buffer))))
  (let ((mevedel--instruction-state-key-override
         (mevedel--instruction-workspace-key workspace)))
    (mevedel--instruction-activate-workspace workspace)
    (when (and (mevedel--all-instructions)
             confirm)
    (unless (y-or-n-p "Discard existing mevedel instructions? ")
      (user-error "Aborted")))
  (let* ((file-alist (copy-tree (plist-get save-file :files)))
         (serialized-directives (plist-get save-file :directives))
         (id-counter-plist (plist-get save-file :ids))
         directives)
    (unless (listp file-alist)
      (user-error "Malformed mevedel instruction list"))
    (unless (and (plist-member save-file :directives)
                 (listp serialized-directives))
      (user-error "Malformed mevedel directive list"))
    ;; Decode before clearing: a malformed snapshot must leave live state.
    (unless (or preserve-directives-p directive-records)
      (setq directives (mevedel--deserialize-directives
                        serialized-directives base-directory)))
    (if (or preserve-directives-p directive-records)
        (mevedel--reset-instructions-preserving-directives
         workspace directive-records)
      (mevedel--clear-instruction-state workspace)
      (mevedel-workspace-set-directives workspace directives))
    (cl-destructuring-bind (&key id-counter used-ids retired-ids) id-counter-plist
      (let ((hm (make-hash-table)))
        (cl-loop for used-id in used-ids
                 do (puthash used-id t hm))
        (setf (mevedel--instruction-id-counter) (or id-counter 0)
              (mevedel--instruction-id-usage-map) hm
              (mevedel--instruction-retired-ids) retired-ids)))
    (let (closed)
      (dolist (entry file-alist)
        (setcdr entry
                (plist-put
                 (cdr entry) :instructions
                 (cl-remove-if
                  (lambda (instruction)
                    (when (eq (plist-get (plist-get instruction :properties)
                                         'mevedel-note-state)
                              'closed)
                      (push (list :file (expand-file-name (car entry) base-directory)
                                  :instruction instruction)
                            closed)
                      t))
                  (plist-get (cdr entry) :instructions)))))
      (setf (plist-get (mevedel--instruction-state
                        (mevedel--instruction-operation-state-key))
                       :closed-notes)
            (nreverse closed)))
    (setf (mevedel--instruction-alist)
          (cl-remove-if-not (lambda (entry) (plist-get (cdr entry) :instructions))
                            file-alist))
    (cl-loop for cons in (mevedel--instruction-alist)
             do (when (stringp (car cons))
                  (setf (car cons)
                        ;; We want to turn the relative paths of the save file to be absolute paths
                        ;; that we will be able to handle.
                        (expand-file-name (car cons) base-directory)))))
    (let ((total-restored 0)
          (total-kia 0)
          (total (mevedel--instructions-saved-count
                  (mevedel--instruction-alist))))
      (let ((mevedel--inhibit-source-missing-restore t))
        (cl-loop for (file . _) in (mevedel--instruction-alist)
                 for visited = (and lazy (get-file-buffer file))
                 when (or (not lazy) visited)
                 do (cl-multiple-value-bind (_ restored kia)
                        (mevedel--restore-file-instructions file t workspace visited)
                      (cl-incf total-restored restored)
                      (cl-incf total-kia kia))))
      (when (or preserve-directives-p directive-records)
        (mevedel--restore-preserved-directives workspace))
      (when (and (not quiet) confirm)
        (message "Restored %d out of %d instructions from %s%s"
                 total-restored
                 total
                 (or source "snapshot")
                 (if (not (zerop total-kia))
                     (format ", with %d lost to patching" total-kia)
                   "")))
      (list :restored total-restored :lost total-kia :total total))))

(defun mevedel--file-outdated-p (file)
  "Determine whether or not FILE needs patching.

A file being outdated refers to the file in the instructions alist not being
up-to-date, not the actual file on the disk being outdated."
  (when-let* ((buffer (find-buffer-visiting file)))
    (mevedel--instruction-activate-buffer buffer))
  (when (file-exists-p file)
    (when-let* ((file-plist (cdr (assoc file (mevedel--instruction-alist)))))
      (let ((mevedel--inhibit-file-patching t))
        (let ((original-content (plist-get file-plist :original-content))
              (buffer (find-file-noselect file)))
          (and original-content
               (with-current-buffer buffer
                 (not (string= original-content
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))))))))

(defvar-local mevedel--buffer-hooks-setup nil
  "Non-nil once `mevedel--setup-buffer-hooks' has run in this buffer.")

(defvar-local mevedel--buffer-instructions-reverted nil
  "Non-nil between a revert stashing instructions and their restoration.")

(defun mevedel--stash-instructions-on-kill ()
  "Stash the killed buffer's instructions, keyed by its file.

Runs on the global `kill-buffer-hook' for the buffer being killed.
It must be one named function registered once: a per-buffer closure
per instruction buffer grows the global hook without bound and runs
every closure on every kill in Emacs.

A buffer with no file is left alone before anything else happens.  This
hook sees every buffer Emacs kills, including internal ones such as
` *Compiler Input*', and instructions are stashed by file: resolving a
workspace for a buffer that has none can only cost time or fail."
  (let ((buffer (current-buffer)))
    (when-let* ((file (buffer-file-name buffer)))
      (mevedel--instruction-activate-buffer buffer)
      (when (mevedel--buffer-has-instructions-p buffer)
        (if (file-exists-p file)
            (let ((file-contents
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-substring-no-properties (point-min) (point-max)))))
              (mevedel--stash-buffer buffer file-contents))
          (mevedel--mark-buffer-source-missing buffer)
          (save-restriction
            (widen)
            (mevedel--stash-buffer buffer))
          (when-let* ((entry (assoc file (mevedel--instruction-alist))))
            (setcdr entry (plist-put (cdr entry) :source-missing t))))))))

(defun mevedel--clear-buffer-source-missing ()
  "Clear the cached missing-source observation after a successful save/restore."
  (when mevedel--buffer-source-missing
    (setq mevedel--buffer-source-missing nil)
    (mevedel-instruction-store-touch
     (mevedel--instruction-buffer-workspace (current-buffer)))))

(defun mevedel--setup-buffer-hooks (buffer)
  "Set up buffer hooks for instruction restoration on kill/revert.

Sets up hooks to preserve mevedel instructions when BUFFER is killed or
reverted, and restores them afterward."
  (add-hook 'kill-buffer-hook #'mevedel--stash-instructions-on-kill)
  (with-current-buffer buffer
    (unless mevedel--buffer-hooks-setup
      (add-hook 'after-save-hook #'mevedel--clear-buffer-source-missing nil t)
      (add-hook 'post-command-hook
                (lambda ()
                  ;; Remote files are skipped: this runs after every command,
                  ;; and `file-exists-p' on a target path is a synchronous
                  ;; round trip that would also nest inside whatever remote
                  ;; operation the command left in flight.  A vanished remote
                  ;; source is still caught by the revert and save hooks.
                  (when-let* ((file (buffer-file-name buffer))
                              ((not (file-remote-p file)))
                              ((or mevedel--buffer-source-missing
                                   (mevedel--buffer-has-instructions-p buffer))))
                    (if (file-exists-p file)
                        (mevedel--clear-buffer-source-missing)
                      (mevedel--mark-buffer-source-missing buffer))))
                nil t)
      (add-hook 'before-revert-hook
                (lambda ()
                  (mevedel--instruction-activate-buffer buffer)
                  (when (mevedel--buffer-has-instructions-p buffer)
                    (mevedel--stash-buffer buffer)
                    (setq-local mevedel--buffer-instructions-reverted t)))
                nil t)
      (add-hook 'after-revert-hook
                (lambda ()
                  (mevedel--instruction-activate-buffer buffer)
                  (when mevedel--buffer-instructions-reverted
                    (mevedel--restore-file-instructions (buffer-file-name buffer) t)
                    (setq-local mevedel--buffer-instructions-reverted nil)))
                nil t)
      (setq-local mevedel--buffer-hooks-setup t))))

(defun mevedel--instruction-current-file-hash ()
  "Return a sha256 hash of the current buffer contents."
  (secure-hash
   'sha256
   (buffer-substring-no-properties (point-min) (point-max))))

(defun mevedel--instruction-bounds-valid-p (start end &optional range)
  "Return non-nil if START and END are valid bounds in current buffer.

When RANGE is non-nil, it is a cons cell limiting valid bounds."
  (and (integerp start)
       (integerp end)
       (<= start end)
       (<= (point-min) start)
       (<= end (point-max))
       (or (null range)
           (and (<= (car range) start)
                (<= end (cdr range))))))

(defun mevedel--instruction-anchor-context-match-p (start end anchor)
  "Return non-nil if START/END has ANCHOR prefix and suffix context."
  (let ((prefix (or (plist-get anchor :prefix) ""))
        (suffix (or (plist-get anchor :suffix) "")))
    (and (or (not (string-empty-p prefix))
             (not (string-empty-p suffix)))
         (>= start (+ (point-min) (length prefix)))
         (<= (+ end (length suffix)) (point-max))
         (string= prefix
                  (buffer-substring-no-properties
                   (- start (length prefix)) start))
         (string= suffix
                  (buffer-substring-no-properties
                   end (+ end (length suffix)))))))

(defun mevedel--instruction-anchor-raw-match-p
    (start end anchor &optional range)
  "Return non-nil if raw START and END still match ANCHOR in RANGE."
  (when (mevedel--instruction-bounds-valid-p start end range)
    (if (plist-get anchor :bodyless)
        (mevedel--instruction-anchor-context-match-p start end anchor)
      (let ((text-hash (plist-get anchor :text-hash)))
        (or (and text-hash
                 (string=
                  text-hash
                  (secure-hash
                   'sha256
                   (buffer-substring-no-properties start end))))
            (mevedel--instruction-anchor-context-match-p
             start end anchor))))))

(defun mevedel--instruction-anchor-unique-candidate (candidates)
  "Return the single candidate in CANDIDATES, or nil when ambiguous."
  (let ((deduped (delete-dups (copy-sequence candidates))))
    (and (= (length deduped) 1)
         (car deduped))))

(defun mevedel--instruction-anchor-resolve-bodyless
    (overlay-start overlay-end anchor range)
  "Resolve bodyless ANCHOR in RANGE from OVERLAY-START to OVERLAY-END."
  (or (and (mevedel--instruction-anchor-raw-match-p
            overlay-start overlay-end anchor range)
           (cons overlay-start overlay-end))
      (let* ((prefix (or (plist-get anchor :prefix) ""))
             (suffix (or (plist-get anchor :suffix) ""))
             (needle (concat prefix suffix))
             candidates)
        (when (and (not (string-empty-p needle))
                   (or (not (string-empty-p prefix))
                       (not (string-empty-p suffix))))
          (save-excursion
            (goto-char (car range))
            (while (search-forward needle (cdr range) t)
              (let ((pos (- (point) (length suffix))))
                (when (mevedel--instruction-bounds-valid-p pos pos range)
                  (push (cons pos pos) candidates))))))
        (mevedel--instruction-anchor-unique-candidate candidates))))

(defun mevedel--instruction-anchor-resolve-text
    (overlay-start overlay-end anchor range)
  "Resolve text ANCHOR in RANGE from OVERLAY-START to OVERLAY-END."
  (or (and (mevedel--instruction-anchor-raw-match-p
            overlay-start overlay-end anchor range)
           (cons overlay-start overlay-end))
      (when-let* ((text (plist-get anchor :text))
                  ((not (string-empty-p text))))
        (let (context-candidates candidates)
          (save-excursion
            (goto-char (car range))
            (while (search-forward text (cdr range) t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (when (mevedel--instruction-bounds-valid-p
                       start end range)
                  (push (cons start end) candidates)
                  (when (mevedel--instruction-anchor-context-match-p
                         start end anchor)
                    (push (cons start end) context-candidates))))))
          (or (mevedel--instruction-anchor-unique-candidate
               context-candidates)
              (mevedel--instruction-anchor-unique-candidate
               candidates))))))

(defun mevedel-persistence-resolve-instruction-anchor
    (overlay-start overlay-end anchor parent-range)
  "Resolve ANCHOR near OVERLAY-START and OVERLAY-END within PARENT-RANGE."
  (let ((range (or parent-range (cons (point-min) (point-max)))))
    (cond
     ((null anchor) (cons overlay-start overlay-end))
     ((plist-get anchor :bodyless)
      (mevedel--instruction-anchor-resolve-bodyless
       overlay-start overlay-end anchor range))
     (t
      (mevedel--instruction-anchor-resolve-text
       overlay-start overlay-end anchor range)))))


(defun mevedel--instruction-restore-order (instructions)
  "Return INSTRUCTIONS ordered so parents are restored before children."
  (sort (copy-sequence instructions)
        (lambda (a b)
          (> (- (or (plist-get a :overlay-end) 0)
                (or (plist-get a :overlay-start) 0))
	             (- (or (plist-get b :overlay-end) 0)
	                (or (plist-get b :overlay-start) 0))))))

(defun mevedel--restore-instruction-plist
    (buffer instr raw-position-ok parent-ranges)
  "Restore one serialized INSTR into BUFFER.

RAW-POSITION-OK means file-level metadata matched, so saved positions
are authoritative.  PARENT-RANGES maps parent UUIDs to resolved ranges.
Return the restored overlay, or nil when unresolved."
  (cl-destructuring-bind
      (&key overlay-start overlay-end anchor properties &allow-other-keys)
      instr
    (with-current-buffer buffer
      (let* ((parent-uuid (plist-get anchor :parent-uuid))
             (parent-range (and parent-uuid
                                (gethash parent-uuid parent-ranges)))
             (bounds (cond
                      (raw-position-ok
                       (and (mevedel--instruction-bounds-valid-p
                             overlay-start overlay-end parent-range)
                            (cons overlay-start overlay-end)))
                      ((and parent-uuid (null parent-range))
                       nil)
                      (t
                       (mevedel-persistence-resolve-instruction-anchor
                        overlay-start overlay-end anchor parent-range)))))
        (when bounds
          (let ((ov (mevedel--restore-overlay buffer
                                              (car bounds)
                                              (cdr bounds)
                                              properties)))
            (when-let* ((uuid (plist-get anchor :uuid)))
              (puthash uuid bounds parent-ranges))
            ov))))))

(defvar mevedel-instructions-restored-hook nil
  "Functions called with the source buffer after its instructions are restored.")

(cl-defun mevedel--restore-file-instructions (file &optional message workspace visited)
  "Restore FILE and its INSTRUCTIONS.

Returns tree values: restored buffer, the amount of instructions restored, and
the amount of instructions lost to the patching process, if any.

If MESSAGE is non-nil, message the intent of patching outdated files.
When WORKSPACE is non-nil, associate the restored buffer with it before
restoring overlays.  VISITED is an already-read source buffer: restore
from it without checking or reading FILE on disk."
  (let ((mevedel--inhibit-file-patching t))
    (when-let* (((not mevedel--inhibit-source-missing-restore))
                (buffer (or visited (find-buffer-visiting file))))
      (mevedel--instruction-activate-buffer buffer)
      (mevedel--restore-source-missing-directives buffer visited))
    (unless (and (assoc file (mevedel--instruction-alist))
                 (or visited (file-exists-p file)))
      (cl-return-from mevedel--restore-file-instructions (cl-values nil 0 0)))
    (cl-destructuring-bind
        (&key original-content instructions content-hash &allow-other-keys)
        (alist-get file (mevedel--instruction-alist) nil nil #'equal)
      (when (null instructions)
        (error "Malformed file given for restoration"))
      (let ((buffer (or visited (find-file-noselect file)))
            (restored 0)
            (kia 0))
        (with-current-buffer buffer
          (when workspace
            (setq-local mevedel--workspace workspace))
          (mevedel--clear-buffer-source-missing)
          (mevedel--setup-buffer-hooks buffer)
          (cl-labels ((restore-overlays
                       (dstbuf instr-maybe-plists &optional raw-position-ok)
                        (let ((ovs ())
                              (parent-ranges (make-hash-table :test #'equal)))
                          (dolist (instr (if (cl-every #'listp
                                                       instr-maybe-plists)
                                             (mevedel--instruction-restore-order
                                              instr-maybe-plists)
                                           instr-maybe-plists))
                            (if (and (listp instr)
                                     (plist-get instr :overlay-start))
                                (when-let* ((ov
                                             (mevedel--restore-instruction-plist
                                              dstbuf instr raw-position-ok
                                              parent-ranges)))
                                  (push ov ovs))
                              (push (mevedel--restore-overlay dstbuf
                                                              (overlay-start instr)
                                                              (overlay-end instr)
                                                              (overlay-properties instr))
                                    ovs)))
                          ovs)))
            (if (and original-content
                     mevedel-patch-outdated-instructions
                     (if visited
                         (not (string= original-content
                                       (buffer-substring-no-properties
                                        (point-min) (point-max))))
                       (mevedel--file-outdated-p file)))
                (progn
                  (when message
                    (message "Patching outdated instructions in buffer '%s'..."
                             (buffer-name buffer)))
                  (with-temp-buffer
                    (let ((new-buffer (current-buffer)))
                      (insert-buffer-substring-no-properties buffer)
                      (with-temp-buffer
                        (insert original-content)
                        (restore-overlays (current-buffer) instructions t)
                        (mevedel-instruction-replace-buffer-contents new-buffer)
                        (restore-overlays
                         buffer
                         (mevedel--instructions-in (point-min) (point-max))
                         t)))))
              (restore-overlays
               buffer instructions
               (and content-hash
                    (string= content-hash
                             (mevedel--instruction-current-file-hash))))))
          (let ((restored-instrs (mevedel--instructions-in (point-min) (point-max))))
            (dolist (instr restored-instrs)
              (mevedel--update-instruction-overlay instr t))
            (setq restored (length restored-instrs)
                  kia (- (length instructions) restored))
            (setf (alist-get file (mevedel--instruction-alist) nil nil #'equal) restored-instrs)
            (when (and message (> kia 0))
              (mevedel-diagnostic-warning
               (mevedel-diagnostic-scope workspace) 'mevedel
               (format "Could not restore %d mevedel instruction%s in %s"
                       kia (if (= kia 1) "" "s") file)
               :warning))))
        (setf (car (assoc file (mevedel--instruction-alist))) buffer)
        (run-hook-with-args 'mevedel-instructions-restored-hook buffer)
        (cl-values buffer restored kia)))))


;;
;;; Workspace instruction store

;; The workspace owns its current instructions; sessions keep only turn
;; snapshots.  The store is one whole-set record under the workspace's
;; `.mevedel/', read and written through the pinned control-file programs.
;; Every write names the hash it was based on.  A stale write merges this
;; client's changes per instruction UUID into the fresh record: changes to
;; instructions nobody else changed apply, a creation or an activity append
;; is retried once, and an edit to an instruction another client changed is
;; refused, its request text going to the kill ring.

(defvar mevedel-instruction-store--inhibit nil
  "Workspace whose instruction changes must not reach its store yet, or t.
Rewind binds its own workspace until its commit point, when the store is
updated explicitly; other workspaces keep writing meanwhile.  The store
binds t while it installs its own records, so they are not echoed back.")

(define-error 'mevedel-instruction-store-unreadable
  "Instruction store is unreadable" 'user-error)

(defun mevedel-instruction-store--inhibited-p (workspace)
  "Return non-nil when WORKSPACE's store must not be written now."
  (or (eq mevedel-instruction-store--inhibit t)
      (and workspace (eq mevedel-instruction-store--inhibit workspace))))

(defvar mevedel-instruction-store--dirty nil
  "Workspaces whose instruction changes the next flush writes.")

(defvar mevedel-instruction-store--warned nil
  "Workspace keys already warned about a pending or unreadable store.")

(defconst mevedel-instruction-store--retry-seconds 30
  "Retry delay for a failed flush before session persistence is loaded.")

(defvar mevedel-instruction-store--failed nil
  "Alist of (WORKSPACE . TIME) for workspaces whose last flush failed.
A non-forced flush skips a workspace until the checkpoint interval has
passed since TIME.")

(defconst mevedel-instruction-store--presentation-keys
  '(:overlay-start :overlay-end :start :end :position :source-order
                   :prefix :suffix :text-hash :text)
  "Plist keys that locate an instruction rather than author it.
Concurrent clients editing the same file move positions without
changing what was authored, so these never decide a conflict.")

(defconst mevedel-instruction-store--activity-keys
  '(:attempts :discussion :planning)
  "Directive record keys holding append-only model activity.")

(defun mevedel-instruction-store-path (workspace)
  "Return WORKSPACE's instruction store file."
  (file-name-concat (mevedel-workspace-state-dir workspace) "instructions.el"))

(defun mevedel-instruction-store-migration-directory (workspace)
  "Return the directory where migration stages WORKSPACE's instructions."
  (file-name-concat (mevedel-workspace-state-dir workspace)
                    "instruction-migration"))

(defun mevedel-instruction-store--applicable-p (workspace)
  "Return non-nil when WORKSPACE keeps its instructions in a store.
A workspace keeps its store under its root, beside its sessions."
  (and workspace
       (stringp (mevedel-workspace-root workspace))))

(defun mevedel-instruction-store--root (workspace)
  "Return WORKSPACE's root as an absolute directory name."
  (file-name-as-directory (expand-file-name (mevedel-workspace-root workspace))))

(defun mevedel-instruction-store--lock-directory (workspace)
  "Return WORKSPACE's stable store lock directory, creating it."
  (let ((directory (file-name-concat (mevedel-workspace-state-dir workspace)
                                     "leases" "instructions")))
    (make-directory directory t)
    directory))


;;;; Record codec

(defun mevedel-instruction-store--validate (data workspace)
  "Signal unless DATA is a well-formed store record for WORKSPACE."
  (let ((ids (plist-get data :ids))
        (files (plist-get data :files)))
    (unless (and (proper-list-p ids) (cl-evenp (length ids))
                 (proper-list-p files)
                 (cl-every (lambda (entry)
                             (and (consp entry) (stringp (car entry))
                                  (proper-list-p
                                   (plist-get (cdr entry) :instructions))))
                           files)
                 (proper-list-p (plist-get data :rewinds))
                 (cl-every (lambda (intent)
                             (and (stringp (plist-get intent :session-id))
                                  (natnump (plist-get intent :from))
                                  (natnump (plist-get intent :to))))
                           (plist-get data :rewinds)))
      (error "Malformed instruction store record"))
    ;; The directive codec is the schema; decoding proves the records.
    (mevedel--deserialize-directives (plist-get data :directives)
                                     (mevedel-instruction-store--root workspace))
    data))

(defun mevedel-instruction-store--decode (bytes workspace)
  "Return (:status STATUS :data DATA :reason TEXT) for WORKSPACE's store BYTES.
STATUS is `ok', `unsupported' for a format this version cannot read,
or `corrupt'."
  (condition-case err
      (let ((form (with-temp-buffer
                    (insert (decode-coding-string bytes 'utf-8-unix))
                    (goto-char (point-min))
                    (prog1 (read (current-buffer))
                      (skip-chars-forward " \t\n")
                      (unless (eobp) (error "Trailing data"))))))
        (cond
         ((not (and (proper-list-p form) (cl-evenp (length form))
                    (plist-member form :format)))
          (list :status 'corrupt :reason "Not an instruction store record"))
         ((not (eql (plist-get form :format)
                    mevedel-instruction-store-format-version))
          (list :status 'unsupported :data form
                :reason (format "Unsupported store format %S"
                                (plist-get form :format))))
         (t
          (mevedel-instruction-store--validate form workspace)
          (list :status 'ok :data form))))
    (error (list :status 'corrupt :reason (error-message-string err)))))

(defun mevedel-instruction-store--print (data)
  "Return DATA printed as store bytes."
  (encode-coding-string
   (let ((print-length nil) (print-level nil) (print-circle nil))
     (concat (prin1-to-string data) "\n"))
   'utf-8-unix))

(defun mevedel-instruction-store--read (workspace)
  "Observe WORKSPACE's store.
Return (:status STATUS :hash HASH :data DATA :reason TEXT).  STATUS is
`absent', `ok', `unsupported' or `corrupt'; HASH is the SHA-256 of the
bytes read, the precondition a later write names."
  (if (not (file-directory-p (mevedel-workspace-state-dir workspace)))
      (list :status 'absent)
    (let ((result (car (mevedel-session-control-fs-run-program
                        (list (list :op 'read
                                    :path (mevedel-instruction-store-path workspace)
                                    :coding 'no-conversion :optional t))))))
      (if (eq (plist-get result :status) 'absent)
          (list :status 'absent)
        (let ((bytes (mevedel-session-control-fs-program-value result)))
          (let ((decoded (mevedel-instruction-store--decode bytes workspace)))
            (if (and (eq (plist-get decoded :status) 'unsupported)
                     (eql (plist-get (plist-get decoded :data) :format) 1))
                (progn
                  (require 'mevedel-notes-migration)
                  (mevedel-notes-migrate-store
                   workspace bytes (plist-get decoded :data)))
              (append (list :hash (secure-hash 'sha256 bytes)) decoded))))))))

(defun mevedel-instruction-store--write (workspace expected data)
  "Replace WORKSPACE's store with DATA while it still has hash EXPECTED.
EXPECTED nil requires the store to be absent.  Return the new hash, or
nil when the store changed since EXPECTED was observed."
  (let* ((path (mevedel-instruction-store-path workspace))
         (bytes (mevedel-instruction-store--print data))
         (results (mevedel-session-control-fs-run-program
                   (list (if expected
                             (list :op 'verify-hash :path path :content expected)
                           (list :op 'absent :path path))
                         (list :op 'write :path path :content bytes
                               :coding 'no-conversion))
                   (mevedel-instruction-store--lock-directory workspace))))
    (unless (memq (plist-get (car results) :status) '(mismatch absent conflict))
      (mapc #'mevedel-session-control-fs-program-value results)
      (secure-hash 'sha256 bytes))))

(defun mevedel-instruction-store--serialize (workspace)
  "Return WORKSPACE's in-memory instructions as a store record.
File names are relative to the workspace root, so the record names the
same files from every client of the workspace."
  (let* ((root (mevedel-instruction-store--root workspace))
         (snapshot (with-temp-buffer
                     (setq-local mevedel--workspace workspace)
                     (mevedel--serialize-instructions root t)))
         (ids (copy-sequence (plist-get snapshot :ids))))
    (setq ids (plist-put ids :used-ids
                         (sort (copy-sequence (plist-get ids :used-ids)) #'<)))
    (list :format mevedel-instruction-store-format-version
          :ids ids
          :directives (plist-get snapshot :directives)
          :files (mapcar (lambda (entry)
                           (cons (if (file-name-absolute-p (car entry))
                                     (mevedel--file-relative-name-or-absolute
                                      (car entry) root)
                                   (car entry))
                                 (cdr entry)))
                         (plist-get snapshot :files))
          :rewinds nil)))

(defun mevedel-instruction-store--install (workspace data &optional lazy)
  "Make store record DATA WORKSPACE's in-memory instructions.
LAZY restores presentations only in files already visited.  Directive
records keep their identity: a record already live under the same UUID
takes the installed fields in place, so a running request, an open
listing or Rewind holding it sees the merged state."
  (let ((mevedel-instruction-store--inhibit t)
        (live (mevedel-workspace-directives workspace)))
    (with-temp-buffer
      (setq-local mevedel--workspace workspace)
      (mevedel--install-instructions
       data (mevedel-instruction-store--root workspace) nil t workspace
       nil nil nil lazy))
    ;; A record absent from DATA is dropped from the list, as removal does.
    (mevedel-workspace-set-directives
     workspace
     (mapcar (lambda (fresh)
               (if-let* ((old (cl-find (mevedel-directive-id fresh) live
                                       :key #'mevedel-directive-id :test #'equal)))
                   (mevedel-instruction-store--adopt old fresh)
                 fresh))
             (mevedel-workspace-directives workspace)))))

(defun mevedel-instruction-store--adopt (live fresh)
  "Copy directive record FRESH's fields into LIVE and return LIVE.
Subdirectives keep their identity the same way.  While LIVE has a
request in flight, its transient state and Plan stay: the store holds
neither the running status nor the Plan's live chat buffer."
  (let ((running (mevedel-directive-in-flight-p live))
        (state (mevedel-directive-state live))
        (plan (mevedel-directive-plan live)))
    (setf (mevedel-directive-subdirectives fresh)
          (mapcar (lambda (sub)
                    (if-let* ((old (cl-find (mevedel-subdirective-id sub)
                                            (mevedel-directive-subdirectives live)
                                            :key #'mevedel-subdirective-id
                                            :test #'equal)))
                        (mevedel-instruction-store--copy-slots
                         'mevedel-subdirective old sub)
                      sub))
                  (mevedel-directive-subdirectives fresh)))
    (mevedel-instruction-store--copy-slots 'mevedel-directive live fresh)
    (when running
      (setf (mevedel-directive-state live) state
            (mevedel-directive-plan live) plan))
    live))

(defun mevedel-instruction-store--copy-slots (type live fresh)
  "Copy every slot of struct TYPE from FRESH into LIVE and return LIVE."
  (dolist (slot (cl-struct-slot-info type))
    (unless (eq (car slot) 'cl-tag-slot)
      (setf (cl-struct-slot-value type (car slot) live)
            (cl-struct-slot-value type (car slot) fresh))))
  live)


;;;; Merging a stale write

(defun mevedel-instruction-store--authored (value)
  "Return VALUE without its presentation keys, for conflict comparison."
  (cond
   ((and (proper-list-p value) (keywordp (car value)) (cl-evenp (length value)))
    (cl-loop for (key item) on value by #'cddr
             unless (memq key mevedel-instruction-store--presentation-keys)
             append (list key (mevedel-instruction-store--authored item))))
   ((proper-list-p value) (mapcar #'mevedel-instruction-store--authored value))
   ((consp value) (cons (mevedel-instruction-store--authored (car value))
                        (mevedel-instruction-store--authored (cdr value))))
   (t value)))

(defun mevedel-instruction-store--instruction-uuid (instruction)
  "Return serialized INSTRUCTION's UUID."
  (or (plist-get (plist-get instruction :anchor) :uuid)
      (plist-get (plist-get instruction :properties) 'mevedel-uuid)))

(defun mevedel-instruction-store--units (data)
  "Return (TABLE . ORDER) for store record DATA.
TABLE maps each instruction UUID to (:record RECORD :file FILE
:instr INSTRUCTION): a directive's record and its source presentation
share one UUID.  ORDER lists the UUIDs in record order."
  (let ((table (make-hash-table :test #'equal))
        order)
    (dolist (record (plist-get data :directives))
      (let ((id (plist-get record :id)))
        (unless (gethash id table) (push id order))
        (puthash id (plist-put (gethash id table) :record record) table)))
    (dolist (entry (plist-get data :files))
      (dolist (instruction (plist-get (cdr entry) :instructions))
        (when-let* ((id (mevedel-instruction-store--instruction-uuid
                         instruction)))
          (unless (gethash id table) (push id order))
          (puthash id (plist-put (plist-put (gethash id table)
                                            :file (car entry))
                                 :instr instruction)
                   table))))
    (cons table (nreverse order))))

(defun mevedel-instruction-store--without (plist keys)
  "Return PLIST without KEYS."
  (cl-loop for (key value) on plist by #'cddr
           unless (memq key keys) append (list key value)))

(defun mevedel-instruction-store--append-activity (base mine theirs)
  "Return THEIRS with the activity MINE appended since BASE, or nil.
Apply only when MINE changed nothing but activity and the directive is
still bound to the same execution session.  An identical entry already
in THEIRS counts as appended; the same sequence with different contents
is a conflict."
  (let ((rb (plist-get base :record))
        (rm (plist-get mine :record))
        (rt (plist-get theirs :record))
        (skip (cons :plan mevedel-instruction-store--activity-keys)))
    (when (and rb rm rt
               (equal (mevedel-instruction-store--authored
                       (mevedel-instruction-store--without rb skip))
                      (mevedel-instruction-store--authored
                       (mevedel-instruction-store--without rm skip)))
               (equal (plist-get rt :session-id) (plist-get rm :session-id)))
      (catch 'conflict
        (let ((record (copy-sequence rt))
              sequences)
          (dolist (key mevedel-instruction-store--activity-keys)
            (let ((entries (copy-sequence (plist-get rt key))))
              (dolist (entry (plist-get rm key))
                (unless (member entry (plist-get rb key))
                  (let ((existing (cl-find (plist-get entry :sequence) entries
                                           :key (lambda (item)
                                                  (plist-get item :sequence)))))
                    (cond ((null existing)
                           (setq entries (append entries (list entry))))
                          ((not (equal existing entry))
                           (throw 'conflict nil))))))
              (dolist (entry entries)
                (when (memql (plist-get entry :sequence) sequences)
                  (throw 'conflict nil))
                (push (plist-get entry :sequence) sequences))
              (setq record
                    (plist-put record key
                               (sort entries
                                     (lambda (a b)
                                       (< (plist-get a :sequence)
                                          (plist-get b :sequence))))))))
          (when (equal (plist-get rt :plan) (plist-get rb :plan))
            (setq record (plist-put record :plan (plist-get rm :plan))))
          (plist-put (copy-sequence theirs) :record record))))))

(defun mevedel-instruction-store--request-text (unit)
  "Return the authored request text UNIT carries, or nil."
  (plist-get (plist-get unit :record) :request))

(defun mevedel-instruction-store--file-metadata (file instructions data)
  "Return DATA's metadata for FILE when it holds every one of INSTRUCTIONS.
Raw positions are only valid against the content they were saved with."
  (when-let* ((entry (cdr (assoc file (plist-get data :files))))
              ((cl-every (lambda (instruction)
                           (member instruction (plist-get entry :instructions)))
                         instructions)))
    (mevedel-instruction-store--without entry '(:instructions))))

(defun mevedel-instruction-store--assign-ids (units origins mine theirs)
  "Return merged id state for UNITS, renumbering colliding new ids.
UNITS is a list of (UUID . UNIT); ORIGINS maps UUIDs to `mine' or
`theirs'.  MINE and THEIRS are the two records' id state.  A display id
two instructions share is reassigned on the one this client contributed,
above every used and retired id."
  (let* ((mine-ids (plist-get mine :ids))
         (theirs-ids (plist-get theirs :ids))
         (retired (delete-dups (append (plist-get mine-ids :retired-ids)
                                       (plist-get theirs-ids :retired-ids))))
         (owners (make-hash-table))
         (counter (max (or (plist-get mine-ids :id-counter) 0)
                       (or (plist-get theirs-ids :id-counter) 0)
                       (apply #'max 0 retired))))
    (dolist (unit units)
      (when-let* ((id (plist-get (plist-get (plist-get (cdr unit) :instr)
                                            :properties)
                                 'mevedel-id)))
        (setq counter (max counter id))))
    ;; Theirs first, so a collision renumbers this client's instruction.
    (dolist (unit (append (cl-remove-if-not
                           (lambda (unit) (eq 'theirs (gethash (car unit) origins)))
                           units)
                          (cl-remove-if
                           (lambda (unit) (eq 'theirs (gethash (car unit) origins)))
                           units)))
      (when-let* ((instruction (plist-get (cdr unit) :instr))
                  (id (plist-get (plist-get instruction :properties) 'mevedel-id)))
        (if (and (gethash id owners)
                 (not (equal (gethash id owners) (car unit))))
            (let ((fresh (cl-incf counter)))
              (setcdr unit
                      (plist-put (copy-sequence (cdr unit)) :instr
                                 (plist-put (copy-sequence instruction) :properties
                                            (plist-put (copy-sequence
                                                        (plist-get instruction :properties))
                                                       'mevedel-id fresh))))
              (puthash fresh (car unit) owners))
          (puthash id (car unit) owners))))
    (let ((used (sort (hash-table-keys owners) #'<)))
      (list :id-counter counter
            :used-ids used
            :retired-ids (sort (cl-set-difference retired used) #'<)))))

(defun mevedel-instruction-store--merge (base mine theirs)
  "Merge this client's changes from BASE to MINE into THEIRS.
BASE is the record this client last observed, MINE its in-memory state,
THEIRS the record now in the store.  Return (:data DATA :refused LIST),
LIST holding (UUID . REQUEST-TEXT) for each change of MINE refused."
  (pcase-let* ((`(,bt . ,_) (mevedel-instruction-store--units base))
               (`(,mt . ,morder) (mevedel-instruction-store--units mine))
               (`(,tt . ,torder) (mevedel-instruction-store--units theirs))
               (origins (make-hash-table :test #'equal))
               (author #'mevedel-instruction-store--authored)
               (units nil)
               (refused nil))
    (dolist (id (append torder (cl-remove-if (lambda (id) (gethash id tt)) morder)))
      (let* ((b (gethash id bt)) (m (gethash id mt)) (th (gethash id tt))
             (ab (funcall author b)) (am (funcall author m))
             (at (funcall author th))
             (choice
              (cond
               ((equal am ab)
                ;; Unchanged here; keep this client's positions if only
                ;; they moved and nobody else changed the instruction.
                (if (and m (equal at ab) (not (equal m b)))
                    (cons m 'mine)
                  (cons th 'theirs)))
               ((equal at ab) (cons m 'mine))
               ((equal am at) (cons m 'mine))
               ((when-let* ((appended (mevedel-instruction-store--append-activity
                                       b m th)))
                  (cons appended 'theirs)))
               (t (push (cons id (mevedel-instruction-store--request-text m))
                        refused)
                  (cons th 'theirs)))))
        (when (car choice)
          (puthash id (cdr choice) origins)
          (push (cons id (car choice)) units))))
    (setq units (nreverse units))
    ;; A creation needs its parent to exist in the merged record.
    (setq units
          (cl-remove-if
           (lambda (unit)
             (let ((parent (plist-get (plist-get (plist-get (cdr unit) :instr)
                                                 :anchor)
                                      :parent-uuid)))
               (when (and parent
                          (eq 'mine (gethash (car unit) origins))
                          (not (gethash (car unit) bt))
                          (not (assoc parent units)))
                 (push (cons (car unit)
                             (mevedel-instruction-store--request-text (cdr unit)))
                       refused))))
           units))
    (let ((keep (mevedel-instruction-store--retained-closed
                 (cl-loop for unit in units
                          for instruction = (plist-get (cdr unit) :instr)
                          when (eq (plist-get (plist-get instruction :properties)
                                               'mevedel-note-state) 'closed)
                          collect instruction))))
      (setq units (cl-remove-if
                   (lambda (unit)
                     (let* ((instruction (plist-get (cdr unit) :instr))
                            (props (plist-get instruction :properties)))
                       (and (eq (plist-get props 'mevedel-note-state) 'closed)
                            (not (gethash (car unit) keep)))))
                   units)))
    (let ((ids (mevedel-instruction-store--assign-ids units origins mine theirs))
          files)
      (dolist (unit units)
        (when-let* ((instruction (plist-get (cdr unit) :instr)))
          (let ((cell (assoc (plist-get (cdr unit) :file) files)))
            (if cell
                (push instruction (cdr cell))
              (push (list (plist-get (cdr unit) :file) instruction) files)))))
      (list
       :data
       (list :format mevedel-instruction-store-format-version
             :ids ids
             :directives (delq nil (mapcar (lambda (unit)
                                             (plist-get (cdr unit) :record))
                                           units))
             :files
             (mapcar (lambda (cell)
                       (let ((instructions (reverse (cdr cell))))
                         (cons (car cell)
                               (append
                                (or (mevedel-instruction-store--file-metadata
                                     (car cell) instructions mine)
                                    (mevedel-instruction-store--file-metadata
                                     (car cell) instructions theirs)
                                    (list :anchor-schema 1))
                                (list :instructions instructions)))))
                     (nreverse files))
             :rewinds (plist-get theirs :rewinds))
       :refused (nreverse refused)))))

(defun mevedel-instruction-store--report-refused (workspace refused)
  "Tell the user about WORKSPACE's REFUSED changes and keep request texts."
  (when refused
    (let ((texts (delq nil (mapcar #'cdr refused))))
      (dolist (text texts)
        (kill-new text))
      (mevedel-diagnostic-warning
       (mevedel-diagnostic-scope workspace) 'mevedel
       (format (concat "%d instruction change%s refused: another client changed "
                       "the same instruction first.  %s")
               (length refused) (if (= 1 (length refused)) " was" "s were")
               (if texts
                   (format "%d request text%s on the kill ring; reapply it explicitly."
                           (length texts) (if (= 1 (length texts)) " is" "s are"))
                 "Changes other than request text cannot be recovered."))
       :warning))))


;;;; Workspace state

(defun mevedel-instruction-store--warn (workspace kind text)
  "Warn about WORKSPACE's store once per KIND with TEXT."
  (let ((key (cons kind (mevedel--instruction-workspace-key workspace))))
    (unless (member key mevedel-instruction-store--warned)
      (push key mevedel-instruction-store--warned)
      (mevedel-diagnostic-warning (mevedel-diagnostic-scope workspace) 'mevedel text :warning))))

(defun mevedel-instruction-store--pending-p (workspace observed own-save-path)
  "Return non-nil when WORKSPACE, whose store is absent, awaits migration.
A staged migration's report, or any session other than OWN-SAVE-PATH,
means instructions may exist only in session snapshots, so creating an
empty store would hide them.  An absolute sessions directory holds every
workspace's sessions, so only those whose sidecar names WORKSPACE count.
OBSERVED with a hash means this client saw a store that has since been
removed; it writes a new one."
  (and (not (plist-get observed :hash))
       (or (file-exists-p
            (file-name-concat (mevedel-instruction-store-migration-directory
                               workspace)
                              "report.el"))
           (let ((sessions (mevedel-session-artifacts-sessions-dir workspace))
                 (own (and own-save-path
                           (directory-file-name (expand-file-name own-save-path)))))
             (if (file-name-absolute-p mevedel-sessions-directory)
                 (mevedel-instruction-store--shared-session-p workspace own)
               (and (file-directory-p sessions)
                    (cl-some (lambda (directory)
                               (and (file-directory-p directory)
                                    (not (equal (directory-file-name directory)
                                                own))))
                             (directory-files sessions t "\\`[^.]"))))))))

(declare-function mevedel-session-persistence--enumerate-sessions
                  "mevedel-session-persistence" (workspace &optional cached))

(defun mevedel-instruction-store--shared-session-p (workspace own)
  "Return non-nil when a session other than OWN may belong to WORKSPACE.
An absolute sessions directory holds every workspace's sessions, so a
compatible session counts only when its authoritative sidecar, as session
discovery reads it for the workspace's authority mode, names WORKSPACE's
identity.  An incompatible (older-format or unreadable) session cannot be
attributed and may hold this workspace's instructions, so it counts too."
  (require 'mevedel-session-persistence)
  (when-let* ((identity (mevedel-workspace-identity-read
                         (mevedel-workspace-root workspace))))
    (let ((found (mevedel-session-persistence--enumerate-sessions workspace)))
      (or (cl-some (lambda (session)
                     (and (equal identity
                                 (plist-get (plist-get (plist-get session :summary)
                                                       :workspace)
                                            :workspace-id))
                          (not (equal (directory-file-name
                                       (plist-get session :save-path))
                                      own))))
                   (plist-get found :sessions))
          (cl-some (lambda (session)
                     (not (equal (directory-file-name (plist-get session :save-path))
                                 own)))
                   (plist-get found :incompatible))))))

(defun mevedel-instruction-store--export-unsynced (workspace)
  "Export WORKSPACE's in-memory instructions before the store replaces them.
Return the export path."
  (let ((path (file-name-concat
               (mevedel-instruction-store-migration-directory workspace)
               (format "unsynced-%s-%s-%d.el"
                       (format-time-string "%Y%m%dT%H%M%S")
                       (system-name) (emacs-pid)))))
    (make-directory (file-name-directory path) t)
    (with-temp-buffer
      (setq-local mevedel--workspace workspace)
      (mevedel--write-instructions-file path (file-name-directory path) t t t))
    path))

(defun mevedel-instruction-store--unreadable (workspace observation)
  "Signal that WORKSPACE's store in OBSERVATION cannot be read.
Nothing is cleared or written: the store must be repaired or replaced
with `mevedel-load-instructions'."
  (signal 'mevedel-instruction-store-unreadable
          (list (format "Instruction store %s is unreadable (%s); fix it or replace it with M-x mevedel-load-instructions"
                        (mevedel-instruction-store-path workspace)
                        (plist-get observation :reason)))))

(defun mevedel-instruction-store--observe (workspace hash data)
  "Record that WORKSPACE's store holds DATA with HASH."
  (mevedel-notes-remember-remote-root
   (mevedel-workspace-root workspace)
   (cl-some (lambda (entry)
              (cl-some (lambda (instruction)
                         (eq (plist-get (plist-get instruction :properties)
                                        'mevedel-instruction-type)
                             'note))
                       (plist-get (cdr entry) :instructions)))
            (plist-get data :files)))
  (let ((key (mevedel--instruction-workspace-key workspace)))
    (setf (plist-get (mevedel--instruction-state key) :store-pending) nil
          (plist-get (mevedel--instruction-state key) :store-observation)
          (list :hash hash :data data))))

(defun mevedel-instruction-store--observation (workspace)
  "Return WORKSPACE's last store observation.
The plist holds :hash and :data of the record this client last read or
wrote, and :pending while the workspace awaits its store.  Clearing
instruction state drops the record, so a client whose state no longer
matches it merges its next write instead of overwriting.  The pending
mark survives that: a pending workspace's instructions come from a
session snapshot load that clears state, yet the store appearing later
must still be noticed."
  (let ((state (mevedel--instruction-state
                (mevedel--instruction-workspace-key workspace))))
    (append (plist-get state :store-observation)
            (and (plist-get state :store-pending) (list :pending t)))))


;;;; Public interface

(defun mevedel-instruction-store-sync (workspace &optional own-save-path)
  "Write WORKSPACE's in-memory instructions to its store; return the outcome.
OWN-SAVE-PATH is a session that may be WORKSPACE's first and so does not
make the workspace pending.  The outcome is nil when WORKSPACE keeps no
store or writes are inhibited, `pending' when it awaits migration,
`unchanged', `written', `merged', `replaced' after the store appeared
while this client was pending, `none' when WORKSPACE has no `.mevedel/'
state directory yet, or (refused . LIST) when some changes were refused.
An unreadable store signals without writing.

A store is created only where mevedel already keeps workspace state:
creating `.mevedel/' merely because an instruction was added to a file in
some directory would make that directory, and every directory below it,
resolve as a workspace.  The first session saved there creates both."
  ;; Held back by a Rewind of this workspace: write after it instead.
  (when (and (not (eq mevedel-instruction-store--inhibit t))
             (mevedel-instruction-store--inhibited-p workspace))
    (mevedel-instruction-store-touch workspace))
  (when (and (not (mevedel-instruction-store--inhibited-p workspace))
             (mevedel-instruction-store--applicable-p workspace))
    (setq mevedel-instruction-store--dirty
          (delq workspace mevedel-instruction-store--dirty))
    (if (not (file-directory-p (mevedel-workspace-state-dir workspace)))
        ;; No store can exist here, so there is nothing to serialize or read.
        'none
      (let* ((key (mevedel--instruction-workspace-key workspace))
             (observed (mevedel-instruction-store--observation workspace))
             (mine (mevedel-instruction-store--serialize workspace))
             (tries 0)
             outcome)
        (while (null outcome)
          (let* ((theirs (mevedel-instruction-store--read workspace))
                 (data (plist-get theirs :data))
                 (stored nil)
                 (refused nil))
            (pcase (plist-get theirs :status)
              ((or 'corrupt 'unsupported)
               (mevedel-instruction-store--unreadable workspace theirs))
              ('absent
               (cond
                ((not (file-directory-p (mevedel-workspace-state-dir workspace)))
                 (setq outcome 'none))
                ((mevedel-instruction-store--pending-p workspace observed own-save-path)
                 (setf (plist-get (mevedel--instruction-state key) :store-pending) t)
                 (mevedel-instruction-store--warn
                  workspace 'pending
                  (format (concat "Workspace %s has no instruction store yet; "
                                  "instructions stay in session snapshots.  "
                                  "Run scripts/migrate-sessions.el, or resolve its "
                                  "staged candidates in %s with M-x mevedel-load-instructions.")
                          (mevedel-workspace-root workspace)
                          (mevedel-instruction-store-migration-directory workspace)))
                 (setq outcome 'pending))
                (t
                 (when-let* ((hash (mevedel-instruction-store--write workspace nil mine)))
                   (setq stored (cons hash mine)
                         outcome 'written)))))
              ('ok
               (cond
                ((plist-get observed :pending)
                 ;; Another client created the store while this one was
                 ;; pending.  Keep this client's state as an export.
                 (let ((path (mevedel-instruction-store--export-unsynced workspace)))
                   (mevedel-instruction-store--install workspace data)
                   (mevedel-instruction-store--observe
                    workspace (plist-get theirs :hash) data)
                   (mevedel-diagnostic-warning
                    (mevedel-diagnostic-scope workspace) 'mevedel
                    (format (concat "The instruction store for %s now exists; it replaced "
                                    "this Emacs's instructions, which were exported to %s")
                            (mevedel-workspace-root workspace) path)
                    :warning)
                   (setq outcome 'replaced)))
                ((equal (plist-get theirs :hash) (plist-get observed :hash))
                 (let ((record (plist-put (copy-sequence mine)
                                          :rewinds (plist-get data :rewinds))))
                   (if (equal record data)
                       (setq outcome 'unchanged)
                     (when-let* ((hash (mevedel-instruction-store--write
                                        workspace (plist-get theirs :hash) record)))
                       (setq stored (cons hash record)
                             outcome 'written)))))
                (t
                 (let* ((merge (mevedel-instruction-store--merge
                                (plist-get observed :data) mine data))
                        (record (plist-get merge :data))
                        (hash (if (equal record data)
                                  (plist-get theirs :hash)
                                (mevedel-instruction-store--write
                                 workspace (plist-get theirs :hash) record))))
                   (when hash
                     (mevedel-instruction-store--install workspace record)
                     (setq stored (cons hash record)
                           refused (plist-get merge :refused)
                           outcome (if refused (cons 'refused refused) 'merged))))))))
            (when stored
              (mevedel-instruction-store--observe workspace (car stored) (cdr stored))
              (mevedel-instruction-store--report-refused workspace refused))
            (when (and (null outcome) (>= (cl-incf tries) 2))
              ;; Retried once and still stale: keep the store's record and
              ;; refuse this client's changes, recovering request texts.
              (let ((fresh (mevedel-instruction-store--read workspace)))
                (when (eq 'ok (plist-get fresh :status))
                  (pcase-let ((`(,bt . ,_) (mevedel-instruction-store--units
                                            (plist-get observed :data)))
                              (`(,mt . ,morder) (mevedel-instruction-store--units mine)))
                    (setq refused
                          (delq nil
                                (mapcar (lambda (id)
                                          (unless (equal (gethash id mt) (gethash id bt))
                                            (cons id (mevedel-instruction-store--request-text
                                                      (gethash id mt)))))
                                        morder))))
                  (mevedel-instruction-store--install workspace (plist-get fresh :data))
                  (mevedel-instruction-store--observe
                   workspace (plist-get fresh :hash) (plist-get fresh :data))
                  (mevedel-instruction-store--report-refused workspace refused))
                ;; Without a readable record nothing was refused or written:
                ;; keep the changes for the next write.
                (unless (eq 'ok (plist-get fresh :status))
                  (cl-pushnew workspace mevedel-instruction-store--dirty))
                (setq outcome 'stale)))))
        outcome))))

(defun mevedel-instruction-store-touch (&optional workspace)
  "Mark WORKSPACE's instructions changed; the next flush writes them.
WORKSPACE defaults to the current buffer's.  A change is recorded even
while a Rewind holds back its workspace's writes; only the store's own
installs are not recorded."
  (unless (eq mevedel-instruction-store--inhibit t)
    (when-let* ((workspace (or workspace
                               (mevedel--instruction-buffer-workspace
                                (current-buffer))))
                ((mevedel-instruction-store--applicable-p workspace)))
      (cl-pushnew workspace mevedel-instruction-store--dirty)
      (add-hook 'post-command-hook #'mevedel-instruction-store-flush))))

(defun mevedel-instruction-store-flush (&optional force)
  "Write every workspace with unwritten instruction changes to its store.
Runs after each command while changes are pending, and at exit.  While a
minibuffer is active it waits: editing a directive request in the
minibuffer changes it on every keystroke, and the command that read it
writes once when it returns.  FORCE writes even then, as at exit.  A
failure is reported and the workspace
stays pending, but is not retried by a non-forced flush until
`mevedel-session-checkpoint-interval' seconds have passed, so a lasting
failure does not repeat after every command.  An unreadable store is
reported once and left to the next observation point, so it is not re-read
after every command."
  (unless (and (> (minibuffer-depth) 0) (not force))
    (dolist (workspace mevedel-instruction-store--dirty)
      (let ((failed (alist-get workspace mevedel-instruction-store--failed)))
        (unless (and failed (not force)
                     (< (float-time (time-since failed))
                        (or (and (boundp 'mevedel-session-checkpoint-interval)
                                 (symbol-value 'mevedel-session-checkpoint-interval))
                            mevedel-instruction-store--retry-seconds)))
          (condition-case err
              (progn
                (mevedel-instruction-store-sync workspace)
                (setf (alist-get workspace mevedel-instruction-store--failed
                                 nil t)
                      nil))
            (mevedel-instruction-store-unreadable
             (setq mevedel-instruction-store--dirty
                   (delq workspace mevedel-instruction-store--dirty))
             (mevedel-instruction-store--warn workspace 'unreadable
                                              (error-message-string err)))
            (error
             (cl-pushnew workspace mevedel-instruction-store--dirty)
             (setf (alist-get workspace mevedel-instruction-store--failed)
                   (current-time))
             (mevedel-diagnostic-message (mevedel-diagnostic-scope workspace) "instructions not written to the store: %s"
                      (error-message-string err)))))))
    (unless mevedel-instruction-store--dirty
      (remove-hook 'post-command-hook #'mevedel-instruction-store-flush))))

(defun mevedel-instruction-store--flush-at-exit ()
  "Write every pending workspace before Emacs exits."
  (mevedel-instruction-store-flush t))

(add-hook 'kill-emacs-hook #'mevedel-instruction-store--flush-at-exit)

(defun mevedel-instruction-store--settle (workspace session-id turn)
  "Resolve SESSION-ID's Rewind intents in WORKSPACE from committed TURN.
An intent records a Rewind from turn :from to :to surviving turns, which
discards the session's activity from turn :prune-from on.  TURN equal to
:to means the Rewind committed: prune that activity, unless that already
happened.  TURN equal to :from means it did not.  Return non-nil when an
intent was resolved.  Signal when one matches neither, or when the store
cannot be read: an intent it may hold must not be skipped, or the
session's turn count would move past it."
  (let ((tries 0) result)
    (while (and (null result) (< tries 2))
      (cl-incf tries)
      (let* ((theirs (mevedel-instruction-store--read workspace))
             (data (plist-get theirs :data))
             (intents (cl-remove-if-not
                       (lambda (intent)
                         (equal (plist-get intent :session-id) session-id))
                       (plist-get data :rewinds))))
        (unless (memq (plist-get theirs :status) '(ok absent))
          (mevedel-instruction-store--unreadable workspace theirs))
        (if (null intents)
            (progn (mevedel-instruction-store--owe workspace session-id nil)
                   (setq result 'none))
          (let ((record data))
            (dolist (intent intents)
              (cond
               ((eql turn (plist-get intent :to))
                (setq record (mevedel-instruction-store--prune
                              workspace record session-id
                              (plist-get intent :prune-from))))
               ((eql turn (plist-get intent :from)))
               (t (user-error "Rewind of session %s from turn %d to %d does not match its committed turn %s"
                              session-id (plist-get intent :from)
                              (plist-get intent :to) turn))))
            (setq record (plist-put (copy-sequence record) :rewinds
                                    (cl-set-difference (plist-get data :rewinds)
                                                       intents :test #'equal)))
            (when-let* ((hash (mevedel-instruction-store--write
                               workspace (plist-get theirs :hash) record)))
              (mevedel-instruction-store--settled workspace theirs hash record)
              (mevedel-instruction-store--owe workspace session-id nil)
              (setq result 'resolved))))))
    (unless result
      (error "Instruction store kept changing while resolving a Rewind"))
    (eq result 'resolved)))

(defun mevedel-instruction-store--settled (workspace before hash record)
  "Adopt RECORD with HASH, written over BEFORE, as WORKSPACE's observation.
A client in step with BEFORE pruned its in-memory records the same way
when it rewound, so RECORD becomes its merge base.  The base carries no
hash: memory is not exactly RECORD, so the next write must merge rather
than overwrite.  Installing RECORD instead would replace the directive
records live code still holds."
  (ignore hash)
  (let ((observed (mevedel-instruction-store--observation workspace)))
    (when (equal (plist-get observed :hash) (plist-get before :hash))
      (mevedel-instruction-store--observe workspace nil record))))

(defun mevedel-instruction-store--prune (workspace data session-id target-turn)
  "Return WORKSPACE's DATA without SESSION-ID's activity from TARGET-TURN on.
Only directives holding such activity go through the Rewind prune, which
also returns consumed subdirectives and re-derives the Plan; applying it
to records without that activity could change their Plan."
  (let* ((root (mevedel-instruction-store--root workspace))
         (records (mevedel--deserialize-directives (plist-get data :directives) root))
         (affected
          (cl-remove-if-not
           (lambda (record)
             (cl-some (lambda (checkpoint)
                        (and (equal session-id (plist-get checkpoint :session-id))
                             (>= (or (plist-get checkpoint :turn) 0) target-turn)))
                      (append
                       (mapcar #'mevedel-directive-attempt-checkpoint
                               (mevedel-directive-attempts record))
                       (mapcar #'mevedel-directive-discussion-turn-checkpoint
                               (mevedel-directive-discussion record))
                       (mapcar (lambda (turn) (plist-get turn :checkpoint))
                               (mevedel-directive-planning record)))))
           records)))
    (if (null affected)
        data
      (let ((scratch (mevedel-workspace--create
                      :type 'project :id "instruction-store-prune" :root root)))
        (mevedel-workspace-set-directives scratch affected)
        (mevedel-workspace-rewind-directives scratch session-id target-turn)
        (let ((pruned (cl-mapcar #'cons
                                 (mapcar (lambda (record) (plist-get record :id))
                                         (mevedel--serialize-directives scratch root))
                                 (mevedel--serialize-directives scratch root))))
          (plist-put (copy-sequence data) :directives
                     (mapcar (lambda (record)
                               (or (cdr (assoc (plist-get record :id) pruned))
                                   record))
                             (plist-get data :directives))))))))

(defun mevedel-instruction-store-settle-session (session)
  "Resolve SESSION's pending Rewind intents from its committed turn count.
Run before a session acts on workspace instructions: at resume and at
request admission.  Return non-nil when an intent was resolved."
  (let ((workspace (mevedel-session-workspace session)))
    (when (mevedel-instruction-store--applicable-p workspace)
      (mevedel-instruction-store--settle
       workspace (mevedel-session-session-id session)
       (or (mevedel-session-turn-count session) 0)))))

(defun mevedel-instruction-store-owes-p (session)
  "Return non-nil when SESSION may still owe the store a Rewind resolution.
True when this client wrote an intent for SESSION, saw one, or resumed
SESSION without settling: after a restart nothing in memory remembers an
intent a crash left behind, so resume marks the session first and only a
successful settle clears the mark.  A session created in this Emacs has
no intent to owe."
  (let* ((workspace (mevedel-session-workspace session))
         (sid (mevedel-session-session-id session))
         (state (and (mevedel-instruction-store--applicable-p workspace)
                     (mevedel--instruction-state
                      (mevedel--instruction-workspace-key workspace))))
         (observation (and state (mevedel-instruction-store--observation workspace))))
    (and (mevedel-instruction-store--applicable-p workspace)
         (or (member sid (plist-get state :store-owed))
             (cl-some (lambda (intent)
                        (equal (plist-get intent :session-id) sid))
                      (plist-get (plist-get observation :data) :rewinds))))))

(defun mevedel-instruction-store--owe (workspace session-id owed)
  "Record whether SESSION-ID owes WORKSPACE's store a Rewind resolution.
OWED non-nil adds the record, nil removes it."
  (let ((state (mevedel--instruction-state
                (mevedel--instruction-workspace-key workspace))))
    (setf (plist-get state :store-owed)
          (if owed
              (cl-adjoin session-id (plist-get state :store-owed) :test #'equal)
            (remove session-id (plist-get state :store-owed))))
    (puthash (mevedel--instruction-workspace-key workspace) state
             mevedel--instruction-states)))

(defun mevedel-instruction-store-restore (session)
  "Load SESSION's workspace instructions from its store at resume.
Return `pending' when the workspace has no store yet, so the caller
restores the session snapshot as before; otherwise the install result.
An unreadable store signals without clearing or writing."
  (let ((workspace (mevedel-session-workspace session)))
    (when (mevedel-instruction-store--applicable-p workspace)
      ;; Until settling succeeds, admission must settle again.
      (mevedel-instruction-store--owe
       workspace (mevedel-session-session-id session) t)
      (mevedel-instruction-store-settle-session session)
      (let ((outcome (mevedel-instruction-store-sync workspace)))
        ;; Without a store, the session snapshot is still where its
        ;; instructions live.
        (if (memq outcome '(pending none))
            'pending
          (list :store outcome))))))

(defvar mevedel-instruction-store--activating nil
  "Non-nil while a workspace's activation observes its store.")

(defun mevedel-instruction-store--current-hash (workspace)
  "Return the hash of WORKSPACE's store bytes, or nil when it is absent.
The bytes are hashed on the execution target and never transferred."
  (condition-case nil
      (plist-get (mevedel-session-control-fs-file-hash
                  (mevedel-instruction-store-path workspace))
                 :sha256)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-instruction-store--memory-empty-p (workspace)
  "Return non-nil when WORKSPACE holds no instructions in memory."
  (and (null (mevedel-workspace-directives workspace))
       (null (plist-get (mevedel--instruction-state
                         (mevedel--instruction-workspace-key workspace))
                        :instructions))))

(defun mevedel-instruction-store-observe (workspace)
  "Bring WORKSPACE's in-memory instructions up to date with its store.
An observation point: workspace activation, opening a listing.  When the
store still holds the record last observed, one target-side hash is the
whole cost.  A workspace with nothing in memory and nothing observed
loads the store, restoring presentations only in visited files.  Any
other difference merges as a write would.  Return `current', `loaded',
a `mevedel-instruction-store-sync' outcome, or nil without a store.  An
unreadable store signals."
  (when (and (mevedel-instruction-store--applicable-p workspace)
             (not (mevedel-instruction-store--inhibited-p workspace))
             (file-directory-p (mevedel-workspace-state-dir workspace)))
    (let ((observed (mevedel-instruction-store--observation workspace)))
      (cond
       ((and (plist-get observed :hash)
             (equal (plist-get observed :hash)
                    (mevedel-instruction-store--current-hash workspace)))
        'current)
       ((and (null (plist-get observed :data))
             (mevedel-instruction-store--memory-empty-p workspace))
        (let ((observation (mevedel-instruction-store--read workspace)))
          (pcase (plist-get observation :status)
            ('absent nil)
            ('ok
             (mevedel-instruction-store--install
              workspace (plist-get observation :data) t)
             (mevedel-instruction-store--observe
              workspace (plist-get observation :hash) (plist-get observation :data))
             'loaded)
            (_ (mevedel-instruction-store--unreadable workspace observation)))))
       (t (mevedel-instruction-store-sync workspace))))))

(defun mevedel-instruction-store-activate (workspace)
  "Observe WORKSPACE's store when the workspace is first activated.
After a restart this makes the workspace's instructions available before
any session resumes.  A failure is reported, never signalled: activation
happens wherever a buffer first resolves its workspace.  A remote root
without a live connection is left to the next observation point (resume,
a listing): resolving a workspace must not open a connection."
  (unless (or mevedel-instruction-store--activating
              (let ((root (mevedel-workspace-root workspace)))
                (and (stringp root) (file-remote-p root)
                     (not (file-remote-p root nil t)))))
    (let ((mevedel-instruction-store--activating t))
      (condition-case err
          (mevedel-instruction-store-observe workspace)
        (error
         (mevedel-instruction-store--warn workspace 'activation
                                          (error-message-string err))
         nil)))))

(defun mevedel-instruction-store-rewind-begin
    (session surviving-turn prune-from)
  "Durably record that SESSION is rewinding to SURVIVING-TURN turns.
The Rewind discards SESSION's activity from turn PRUNE-FROM on.  Call
before Rewind's first irreversible step.  Return non-nil when an intent
was recorded, nil when the workspace keeps no active store.  Signal when
the intent cannot be recorded, which aborts the Rewind."
  (let ((workspace (mevedel-session-workspace session))
        (tries 0) result)
    (when (mevedel-instruction-store--applicable-p workspace)
      (while (and (null result) (< tries 2))
        (cl-incf tries)
        (let ((theirs (mevedel-instruction-store--read workspace)))
          (pcase (plist-get theirs :status)
            ('absent (setq result 'none))
            ((or 'corrupt 'unsupported)
             (mevedel-instruction-store--unreadable workspace theirs))
            ('ok
             (let* ((data (plist-get theirs :data))
                    (sid (mevedel-session-session-id session))
                    (record (plist-put
                             (copy-sequence data) :rewinds
                             (append (plist-get data :rewinds)
                                     (list (list :session-id sid
                                                 :from (or (mevedel-session-turn-count session) 0)
                                                 :to surviving-turn
                                                 :prune-from prune-from))))))
               (when (cl-find sid (plist-get data :rewinds)
                              :key (lambda (intent) (plist-get intent :session-id))
                              :test #'equal)
                 (user-error "Session %s has an unresolved Rewind in the instruction store"
                             sid))
               (when-let* ((hash (mevedel-instruction-store--write
                                  workspace (plist-get theirs :hash) record)))
                 (let ((observed (mevedel-instruction-store--observation workspace)))
                   (when (equal (plist-get observed :hash) (plist-get theirs :hash))
                     (mevedel-instruction-store--observe workspace hash record)))
                 (mevedel-instruction-store--owe workspace sid t)
                 (setq result 'recorded)))))))
      (unless result
        (error "Instruction store kept changing while recording a Rewind"))
      (eq result 'recorded))))

(defun mevedel-instruction-store-rewind-end (session committed)
  "Resolve SESSION's Rewind intent after its outcome is known.
COMMITTED non-nil means the Rewind passed its commit point: prune the
session's discarded activity from the store, then reinstall source
presentations from the store, keeping the directive records Rewind
already pruned in memory.  The session's historical snapshot is not the
workspace's current set.  Otherwise drop the intent."
  (let ((workspace (mevedel-session-workspace session)))
    (when (mevedel-instruction-store--applicable-p workspace)
      (if committed
          (let (settled)
            (unwind-protect
                (progn
                  (mevedel-instruction-store-settle-session session)
                  (let ((current (mevedel-instruction-store--read workspace))
                        (base (plist-get (mevedel-instruction-store--observation
                                          workspace)
                                         :data)))
                    (when (eq 'ok (plist-get current :status))
                      (let ((mevedel-instruction-store--inhibit t))
                        (with-temp-buffer
                          (setq-local mevedel--workspace workspace)
                          (mevedel--install-instructions
                           (plist-get current :data)
                           (mevedel-instruction-store--root workspace)
                           nil t workspace
                           (mevedel-workspace-directives workspace) t)))
                      ;; Presentations now match the store; directive records
                      ;; match the base memory was in step with.
                      (mevedel-instruction-store--observe
                       workspace nil
                       (plist-put (copy-sequence (plist-get current :data))
                                  :directives (plist-get base :directives)))
                      (setq settled t))))
              (unless settled
                ;; Memory may hold the historical snapshot's presentations.
                ;; Without a base, the next write cannot delete anything.
                (mevedel-instruction-store--forget workspace))))
        (mevedel-instruction-store--settle
         workspace (mevedel-session-session-id session)
         (or (mevedel-session-turn-count session) 0))))))

(defun mevedel-instruction-store--forget (workspace)
  "Drop WORKSPACE's store observation.
Use when memory may not correspond to any observed record: with no base,
a later write keeps every instruction the store holds.  The price is a
broad refusal: without a base, an edit this client made to an
instruction the store also holds, differently, is indistinguishable from
a conflict and is refused.  Using the store's current record as the base
instead would treat the historical presentations memory may hold as this
client's edits and delete other sessions' instructions."
  (setf (plist-get (mevedel--instruction-state
                    (mevedel--instruction-workspace-key workspace))
                   :store-observation)
        nil))

(defun mevedel-instruction-store-replacing (workspace load)
  "Call LOAD to replace WORKSPACE's instructions, then replace its store.
Used by `mevedel-load-instructions': the explicit import that also gives
a pending workspace its store.  Where no `.mevedel/' exists, only LOAD
runs, as for a store created automatically.  Before LOAD changes
anything, refuse unless this client last observed exactly the store's
current record, so an import cannot discard instructions it never saw;
an absent store, or an unreadable one being repaired, has nothing to
lose.  The write then requires the store to be unchanged since that
check.  Return LOAD's value."
  (if (not (and (mevedel-instruction-store--applicable-p workspace)
                (file-directory-p (mevedel-workspace-state-dir workspace))))
      (funcall load)
    (let* ((observed (mevedel-instruction-store--observation workspace))
           (theirs (mevedel-instruction-store--read workspace))
           (expected (plist-get theirs :hash)))
      (when (and (eq 'ok (plist-get theirs :status))
                 (not (equal expected (plist-get observed :hash))))
        (user-error "The instruction store holds instructions this Emacs has not loaded; list the directives or resume a session to load them, then import again"))
      (prog1 (funcall load)
        (let ((record (plist-put (mevedel-instruction-store--serialize workspace)
                                 :rewinds (plist-get (plist-get theirs :data)
                                                     :rewinds))))
          (if-let* ((hash (mevedel-instruction-store--write workspace expected record)))
              (mevedel-instruction-store--observe workspace hash record)
            (user-error "The instruction store changed while importing; load the file again")))))))

(defun mevedel-instruction-store-read-connected (root)
  "Read indexed remote ROOT once without waiting for target I/O.
Only a live SSH master may carry the read.  A failed carrier leaves state
untouched until an explicit workspace operation observes it normally."
  (let* ((mevedel-instruction-store--activating t)
         (workspace (mevedel-workspace-get-or-create 'project root root root))
         timer)
    (cl-labels
        ((failed (reason)
           (mevedel-diagnostic-warning
            root 'mevedel (format "Background note restore skipped: %s" reason) :debug))
         (fresh-p ()
           (and (null (plist-get (mevedel-instruction-store--observation workspace) :data))
                (mevedel-instruction-store--memory-empty-p workspace))))
      (when (fresh-p)
        (condition-case err
            (let ((process
                   (mevedel-session-control-fs-run-program-async
                    (list (list :op 'read :path (mevedel-instruction-store-path workspace)
                                :coding 'no-conversion :optional t))
                    (lambda (results failure)
                      (when timer (cancel-timer timer))
                      (condition-case err
                          (cond
                           (failure (failed (error-message-string failure)))
                           ;; An explicit operation may have changed notes while
                           ;; the read was in flight.  Its newer state wins.
                           ((not (fresh-p)))
                           ((eq (plist-get (car results) :status) 'absent)
                            (mevedel-notes-remember-remote-root root nil))
                           (t
                            (let* ((bytes (mevedel-session-control-fs-program-value (car results)))
                                   (decoded (mevedel-instruction-store--decode bytes workspace)))
                              (if (eq (plist-get decoded :status) 'ok)
                                  (progn
                                    (mevedel-instruction-store--install
                                     workspace (plist-get decoded :data) t)
                                    (mevedel-instruction-store--observe
                                     workspace (secure-hash 'sha256 bytes) (plist-get decoded :data)))
                                (failed (plist-get decoded :reason))))))
                        (error (failed (error-message-string err)))))
                    root t)))
              (when (process-live-p process)
                (setq timer (run-at-time 15 nil #'delete-process process))))
          (error (failed (error-message-string err))))))))

(defun mevedel-instruction-store-visit (file &optional key root)
  "Restore visited FILE from cached KEY or the local store at ROOT.
Only the local discovery path may register and observe a workspace."
  (let* ((workspace
          (if root
              (mevedel-workspace-get-or-create
               'project root root (mevedel-workspace--project-name root))
            (gethash key mevedel-workspace--registry)))
         (mevedel--instruction-state-key-override
          (or key (mevedel--instruction-workspace-key workspace))))
    ;; Discovering an ancestor's store does not give it ownership of an
    ;; unnoted file in a nested project.  Cache ownership only for a match.
    (when (or (assoc file (mevedel--instruction-alist))
              (and workspace
                   (cl-some
                    (lambda (directive)
                      (equal file (plist-get (mevedel-directive-anchor directive) :file)))
                    (mevedel-workspace-directives workspace))))
      (when workspace
        (setq-local mevedel--workspace workspace))
      (mevedel--restore-file-instructions file nil workspace (current-buffer)))))

(provide 'mevedel-persistence)

;;; mevedel-persistence.el ends here
