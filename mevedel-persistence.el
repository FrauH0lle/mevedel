;;; mevedel-persistence.el -- Save/load instructions -*- lexical-binding: t -*-

;;; Commentary:

;; Saves and restores source-bound references and workspace-owned directives
;; across Emacs sessions.  The save format records directive identity and
;; authored state separately from file-specific overlay presentations.  When a
;; save file is loaded against a buffer whose contents have changed, native
;; buffer replacement reconciles overlay positions.  Save files must match the
;; current mevedel version and directive schema.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mevedel-directive-source)
(require 'mevedel-instruction-registry)
(require 'mevedel-overlays)
(require 'mevedel-utilities)

;; `mevedel-directive'
(declare-function mevedel-workspace-set-directives "mevedel-directive"
                  (workspace directives))

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
(declare-function mevedel--reattach-directive
                  "mevedel-directive-source" (record workspace buffer start end))
(declare-function mevedel--reattach-subdirective
                  "mevedel-directive-source"
                  (record owner workspace buffer start end))
(declare-function mevedel--reconcile-directive-sources
                  "mevedel-directive-source" (workspace))
(declare-function mevedel--refresh-directive-anchor
                  "mevedel-directive-source" (directive))
(declare-function mevedel--restore-source-missing-directives
                  "mevedel-directive-source" (buffer))

;; `mevedel-overlays'
(declare-function mevedel--subinstruction-of-p
                  "mevedel-overlays" (sub parent))

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))
(autoload 'mevedel--update-instruction-overlay "mevedel-overlay-ui")

;; `mevedel-structs'
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel-version "mevedel-utilities" (&optional here message))

;; `mevedel-workspace'
(defvar mevedel--workspace)

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
                                 (list :instructions instrs)
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
               (push (cons (car cons) entry)
                     file-alist)))
    (list :version (mevedel-version)
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
  "Load instruction overlays from a file specified by PATH."
  (interactive (list (read-file-name "Instruction list file: ")))
  (mevedel--load-instructions-file path (file-name-parent-directory path)
                                   (called-interactively-p 'any)
                                   nil))

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
  (let* ((save-file (mevedel--read-instructions-file path))
         (file-alist (plist-get save-file :files))
         (serialized-directives (plist-get save-file :directives))
         (id-counter-plist (plist-get save-file :ids)))
    (unless (equal (plist-get save-file :version) (mevedel-version))
      (user-error "Unsupported instruction file version: %s"
                  (or (plist-get save-file :version) "missing")))
    (unless (listp file-alist)
      (user-error "Malformed mevedel instruction list"))
    (unless (and (plist-member save-file :directives)
                 (listp serialized-directives))
      (user-error "Malformed mevedel directive list"))
    (if (or preserve-directives-p directive-records)
        (mevedel--reset-instructions-preserving-directives
         workspace directive-records)
      (mevedel--clear-instruction-state workspace)
      (mevedel-workspace-set-directives
       workspace
       (mevedel--deserialize-directives
        serialized-directives
        (or base-directory (file-name-parent-directory path)))))
    (cl-destructuring-bind (&key id-counter used-ids retired-ids) id-counter-plist
      (let ((hm (make-hash-table)))
        (cl-loop for used-id in used-ids
                 do (puthash used-id t hm))
        (setf (mevedel--instruction-id-counter) (or id-counter 0)
              (mevedel--instruction-id-usage-map) hm
              (mevedel--instruction-retired-ids) retired-ids)))
    (setf (mevedel--instruction-alist) file-alist)
    (cl-loop for cons in (mevedel--instruction-alist)
             do (when (stringp (car cons))
                  (setf (car cons)
                        ;; We want to turn the relative paths of the save file to be absolute paths
                        ;; that we will be able to handle.
                        (expand-file-name
                         (car cons)
                         (or base-directory
                             (file-name-parent-directory path)))))))
    (let ((total-restored 0)
          (total-kia 0)
          (total (cl-reduce #'+
                            (mapcar #'length
                                    (mapcar (lambda (plist)
                                              (plist-get plist :instructions))
                                            (mapcar #'cdr (mevedel--instruction-alist))))
                            :initial-value 0)))
      (let ((mevedel--inhibit-source-missing-restore t))
        (cl-loop for (file . _) in (mevedel--instruction-alist)
                 do (cl-multiple-value-bind (_ restored kia)
                        (mevedel--restore-file-instructions file t workspace)
                      (cl-incf total-restored restored)
                      (cl-incf total-kia kia))))
      (when (or preserve-directives-p directive-records)
        (mevedel--restore-preserved-directives workspace))
      (when (and (not quiet) confirm)
        (message "Restored %d out of %d instructions from %s%s"
                 total-restored
                 total
                 (expand-file-name path)
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
          (mevedel--mark-buffer-source-missing buffer))))))

(defun mevedel--setup-buffer-hooks (buffer)
  "Set up buffer hooks for instruction restoration on kill/revert.

Sets up hooks to preserve mevedel instructions when BUFFER is killed or
reverted, and restores them afterward."
  (add-hook 'kill-buffer-hook #'mevedel--stash-instructions-on-kill)
  (with-current-buffer buffer
    (unless mevedel--buffer-hooks-setup
      (add-hook 'post-command-hook
                (lambda ()
                  ;; Remote files are skipped: this runs after every command,
                  ;; and `file-exists-p' on a target path is a synchronous
                  ;; round trip that would also nest inside whatever remote
                  ;; operation the command left in flight.  A vanished remote
                  ;; source is still caught by the revert and save hooks.
                  (when-let* ((file (buffer-file-name buffer))
                              ((not (file-remote-p file)))
                              ((not (file-exists-p file)))
                              ((mevedel--buffer-has-instructions-p buffer)))
                    (mevedel--mark-buffer-source-missing buffer)))
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

(cl-defun mevedel--restore-file-instructions (file &optional message workspace)
  "Restore FILE and its INSTRUCTIONS.

Returns tree values: restored buffer, the amount of instructions restored, and
the amount of instructions lost to the patching process, if any.

If MESSAGE is non-nil, message the intent of patching outdated files.
When WORKSPACE is non-nil, associate the restored buffer with it before
restoring overlays."
  (let ((mevedel--inhibit-file-patching t))
    (when-let* (((not mevedel--inhibit-source-missing-restore))
                (buffer (find-buffer-visiting file)))
      (mevedel--instruction-activate-buffer buffer)
      (mevedel--restore-source-missing-directives buffer))
    (unless (and (file-exists-p file)
                 (assoc file (mevedel--instruction-alist)))
      (cl-return-from mevedel--restore-file-instructions (cl-values nil 0 0)))
    (cl-destructuring-bind
        (&key original-content instructions content-hash &allow-other-keys)
        (alist-get file (mevedel--instruction-alist) nil nil #'equal)
      (when (null instructions)
        (error "Malformed file given for restoration"))
      (let ((buffer (find-file-noselect file))
            (restored 0)
            (kia 0))
        (with-current-buffer buffer
          (when workspace
            (setq-local mevedel--workspace workspace))
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
                     (mevedel--file-outdated-p file))
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
                        (replace-region-contents (point-min) (point-max) new-buffer)
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
              (display-warning
               'mevedel
               (format "Could not restore %d mevedel instruction%s in %s"
                       kia (if (= kia 1) "" "s") file)
               :warning))))
        (setf (car (assoc file (mevedel--instruction-alist))) buffer)
        (cl-values buffer restored kia)))))

(add-hook 'find-file-hook
          (lambda ()
            (unless mevedel--inhibit-file-patching
              (mevedel--instruction-activate-buffer (current-buffer))
              (mevedel--restore-file-instructions (buffer-file-name (current-buffer))))))

(provide 'mevedel-persistence)

;;; mevedel-persistence.el ends here
