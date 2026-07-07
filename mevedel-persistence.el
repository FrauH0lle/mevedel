;;; mevedel-persistence.el -- Save/load instructions -*- lexical-binding: t -*-

;;; Commentary:

;; Saves and restores instruction overlays across Emacs sessions.
;; Instructions are file-specific; the save format records buffer
;; associations, positions, and overlay properties.  When a save file
;; is loaded against a buffer whose contents have changed, ediff is
;; used to reconcile overlay positions.  Version stamping on the save
;; format lets older files be auto-patched when loaded.

;;; Code:

(require 'cl-lib)
(require 'ediff)
(require 'subr-x)

(require 'mevedel-overlays)
(require 'mevedel-utilities)

;; `mevedel'
(declare-function mevedel-version "mevedel" (&optional here message))

(defcustom mevedel-patch-outdated-instructions t
  "Automatically patch instructions when the save file is outdated if non-nil."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel--inhibit-file-patching nil
  "If t, `mevedel--restore-file-instructions' becomes inert.
This is sometimes necessary to prevent various hooks from interfering with the
instruction restoration process.")

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
  (list :id-counter mevedel--id-counter
        :used-ids (hash-table-keys mevedel--id-usage-map)
        :retired-ids mevedel--retired-ids))

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

(defun mevedel--instruction-relative-file-name (file base-directory)
  "Return FILE relative to BASE-DIRECTORY, tolerating alias spellings."
  (let ((file (expand-file-name file))
        (base (file-name-as-directory (expand-file-name base-directory))))
    (cond
     ((file-in-directory-p file base)
      (file-relative-name file base))
     ((let ((true-file (ignore-errors (file-truename file)))
            (true-base (ignore-errors
                         (file-name-as-directory (file-truename base)))))
        (when (and true-file true-base
                   (file-in-directory-p true-file true-base))
          (file-relative-name true-file true-base))))
     (t file))))

(defun mevedel--serialize-instructions
    (&optional base-directory include-original-content)
  "Return a plist snapshot of the current workspace instructions.

BASE-DIRECTORY controls how file names are stored.  When non-nil,
file names are serialized relative to it; otherwise absolute names are
used.  When INCLUDE-ORIGINAL-CONTENT is non-nil, include full buffer
contents for position patching if the file changes before restore."
  (mevedel--instruction-activate-buffer)
  (let ((file-alist ())
        (base-directory (and base-directory
                             (file-name-as-directory
                              (expand-file-name base-directory)))))
    (cl-loop for cons in mevedel--instructions
             if (bufferp (car cons))
             do (let ((buffer (car cons)))
                  (when-let* (((buffer-live-p buffer))
                              (buffer-file-name (buffer-file-name buffer)))
                    (let ((file (if base-directory
                                    (mevedel--instruction-relative-file-name
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
          :files file-alist)))

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
        (with-temp-file path
          (prin1 save-file (current-buffer))
          (unless quiet
            (let ((file-count (length file-alist)))
              (message "Wrote %d mevedel instruction%s from %d file%s to %s"
                       saved-instruction-count
                       (if (= 1 saved-instruction-count) "" "s")
                       file-count
                       (if (= 1 file-count) "" "s")
                       path))))
      (when write-empty
        (with-temp-file path
          (prin1 save-file (current-buffer))))
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

(defun mevedel--skip-printed-string (text pos)
  "Return the position after the string in TEXT at POS."
  (let ((len (length text))
        (pos (1+ pos)))
    (catch 'done
      (while (< pos len)
        (pcase (aref text pos)
          (?\\ (setq pos (+ pos 2)))
          (?\" (throw 'done (1+ pos)))
          (_ (setq pos (1+ pos)))))
      (error "Unterminated string in instruction snapshot"))))

(defun mevedel--skip-printed-list-tail (text pos)
  "Return the position after the list tail in TEXT starting at POS."
  (let ((len (length text))
        (depth 1))
    (while (and (< pos len) (> depth 0))
      (pcase (aref text pos)
        (?\" (setq pos (mevedel--skip-printed-string text pos)))
        ((or ?\( ?\[)
         (setq depth (1+ depth)
               pos (1+ pos)))
        ((or ?\) ?\])
         (setq depth (1- depth)
               pos (1+ pos)))
        (_ (setq pos (1+ pos)))))
    (unless (zerop depth)
      (error "Unterminated list in instruction snapshot"))
    pos))

(defun mevedel--strip-printed-text-properties (text)
  "Return TEXT with printed propertized strings collapsed to strings."
  (let ((start 0)
        chunks)
    (while (string-match "#(\"" text start)
      (let ((match-start (match-beginning 0))
            (string-start (+ (match-beginning 0) 2)))
        (condition-case nil
            (let* ((read-result (read-from-string text string-start))
                   (plain (substring-no-properties (car read-result)))
                   (after-vector
                    (mevedel--skip-printed-list-tail text (cdr read-result))))
              (push (substring text start match-start) chunks)
              (push (prin1-to-string plain) chunks)
              (setq start after-vector))
          (error
           (push (substring text start (match-end 0)) chunks)
           (setq start (match-end 0))))))
    (push (substring text start) chunks)
    (apply #'concat (nreverse chunks))))

(defun mevedel--read-instructions-file (path)
  "Read the instruction snapshot form from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (condition-case original-err
        (read (current-buffer))
      (error
       (let ((stripped (mevedel--strip-printed-text-properties
                        (buffer-string))))
         (if (string= stripped (buffer-string))
             (signal (car original-err) (cdr original-err))
           (with-temp-buffer
             (insert stripped)
             (goto-char (point-min))
             (read (current-buffer)))))))))

(defun mevedel--load-instructions-file
    (path &optional base-directory confirm quiet workspace)
  "Load instruction overlays from PATH into WORKSPACE.

BASE-DIRECTORY resolves relative file names in PATH.  CONFIRM prompts
before replacing existing instructions.  QUIET suppresses messages."
  (setq workspace (or workspace
                      (mevedel--instruction-buffer-workspace
                       (current-buffer))))
  (mevedel--instruction-activate-workspace workspace)
  (when (and (mevedel--instructions)
             confirm)
    (unless (y-or-n-p "Discard existing mevedel instructions? ")
      (user-error "Aborted")))
  (let* ((save-file (mevedel--patch-save-file
                     (mevedel--read-instructions-file path)))
         (file-alist (plist-get save-file :files))
         (id-counter-plist (plist-get save-file :ids)))
    (unless (listp file-alist)
      (user-error "Malformed mevedel instruction list"))
    (mevedel--clear-instruction-state workspace)
    (cl-destructuring-bind (&key id-counter used-ids retired-ids) id-counter-plist
      (let ((hm (make-hash-table)))
        (cl-loop for used-id in used-ids
                 do (puthash used-id t hm))
        (setq mevedel--id-counter (or id-counter 0)
              mevedel--id-usage-map hm
              mevedel--retired-ids retired-ids)))
    (setq mevedel--instructions file-alist)
    (cl-loop for cons in mevedel--instructions
             do (when (stringp (car cons))
                  (setf (car cons)
                        ;; We want to turn the relative paths of the save file to be absolute paths
                        ;; that we will be able to handle.
                        (expand-file-name
                         (car cons)
                         (or base-directory
                             (file-name-parent-directory path)))))))
    (mevedel--instruction-save-current-state)
    (let ((total-restored 0)
          (total-kia 0)
          (total (cl-reduce #'+
                            (mapcar #'length
                                    (mapcar (lambda (plist)
                                              (plist-get plist :instructions))
                                            (mapcar #'cdr mevedel--instructions)))
                            :initial-value 0)))
      (cl-loop for (file . _) in mevedel--instructions
               do (progn
                    (cl-multiple-value-bind (_ restored kia) (mevedel--restore-file-instructions file t)
                      (cl-incf total-restored restored)
                      (cl-incf total-kia kia))))
      (when (and (not quiet) confirm)
        (message "Restored %d out of %d instructions from %s%s"
                 total-restored
                 total
                 (expand-file-name path)
                 (if (not (zerop total-kia))
                     (format ", with %d lost to patching" total-kia)
                   "")))
      (list :restored total-restored :lost total-kia :total total)))

(defun mevedel--file-outdated-p (file)
  "Determine whether or not FILE needs patching.

A file being outdated refers to the file in the instructions alist not being
up-to-date, not the actual file on the disk being outdated."
  (when-let* ((buffer (find-buffer-visiting file)))
    (mevedel--instruction-activate-buffer buffer))
  (when (file-exists-p file)
    (when-let* ((file-plist (cdr (assoc file mevedel--instructions))))
      (let ((mevedel--inhibit-file-patching t))
        (let ((original-content (plist-get file-plist :original-content))
              (buffer (find-file-noselect file)))
          (and original-content
               (with-current-buffer buffer
                 (not (string= original-content
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))))))))

(defun mevedel--setup-buffer-hooks (buffer)
  "Set up buffer hooks for instruction restoration on kill/revert.

Sets up hooks to preserve mevedel instructions when BUFFER is killed or
reverted, and restores them afterward."
  (with-current-buffer buffer
    (unless (bound-and-true-p mevedel--buffer-hooks-setup)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (mevedel--instruction-activate-buffer (current-buffer))
                  (when (mevedel--buffer-has-instructions-p (current-buffer))
                    (when-let* ((file (buffer-file-name buffer)))
                      (if (file-exists-p file)
                          (let ((file-contents
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-substring-no-properties (point-min) (point-max)))))
                            (mevedel--stash-buffer buffer file-contents))
                        (setq mevedel--instructions (assq-delete-all buffer mevedel--instructions))))))
                  (mevedel--instruction-save-current-state))
                nil t))
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
                (when (bound-and-true-p mevedel--buffer-instructions-reverted)
                  (mevedel--restore-file-instructions (buffer-file-name buffer) t)
                  (setq-local mevedel--buffer-instructions-reverted nil)))
              nil t)
    (setq-local mevedel--buffer-hooks-setup t))

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

(defun mevedel--instruction-anchor-resolve
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
                       (mevedel--instruction-anchor-resolve
                        overlay-start overlay-end anchor parent-range)))))
        (when bounds
          (let ((ov (mevedel--restore-overlay buffer
                                              (car bounds)
                                              (cdr bounds)
                                              properties)))
            (when-let* ((uuid (plist-get anchor :uuid)))
              (puthash uuid bounds parent-ranges))
            ov))))))

(cl-defun mevedel--restore-file-instructions (file &optional message)
  "Restore FILE and its INSTRUCTIONS.

Returns tree values: restored buffer, the amount of instructions restored, and
the amount of instructions lost to the patching process, if any.

If MESSAGE is non-nil, message the intent of patching outdated files."
  (let ((mevedel--inhibit-file-patching t))
    (when-let* ((buffer (find-buffer-visiting file)))
      (mevedel--instruction-activate-buffer buffer))
    (unless (and (file-exists-p file)
                 (assoc file mevedel--instructions))
      (cl-return-from mevedel--restore-file-instructions (cl-values nil 0 0)))
    (cl-destructuring-bind
        (&key original-content instructions content-hash &allow-other-keys)
        (alist-get file mevedel--instructions nil nil #'equal)
      (when (null instructions)
        (error "Malformed file given for restoration"))
      (let ((buffer (find-file-noselect file))
            (restored 0)
            (kia 0))
        (with-current-buffer buffer
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
                (if (not (executable-find "diff"))
                    (progn
                      (warn "Patching outdated instructions requires 'diff' to be installed.")
                      (setq mevedel-patch-outdated-instructions nil)
                      (restore-overlays buffer instructions t))
                  (when message
                    (message "Patching outdated instructions in buffer '%s'..."
                             (buffer-name buffer)))
                  (with-temp-buffer
                    (let ((new-buffer (current-buffer)))
                      (insert-buffer-substring-no-properties buffer)
                      (with-temp-buffer
                        (insert original-content)
                        (restore-overlays (current-buffer) instructions t)
                        (mevedel--wordwise-diff-patch-buffers (current-buffer) new-buffer)
                        (restore-overlays buffer (mevedel--instructions-in (point-min) (point-max)) t)))))
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
            (setf (alist-get file mevedel--instructions nil nil #'equal) restored-instrs)
            (when (and message (> kia 0))
              (display-warning
               'mevedel
               (format "Could not restore %d mevedel instruction%s in %s"
                       kia (if (= kia 1) "" "s") file)
               :warning))))
        (setf (car (assoc file mevedel--instructions)) buffer)
        (mevedel--instruction-save-current-state)
        (cl-values buffer restored kia)))))

(defun mevedel--wordwise-diff-patch-buffers (old new)
  "Wordwise patch buffer OLD to be equivalent to buffer NEW via `ediff-buffers'.

This is mostly a brittle hack meant to make Ediff be used noninteractively."
  (cl-labels ((apply-all-diffs ()
                (ediff-next-difference)
                (while (ediff-valid-difference-p)
                  (ediff-copy-B-to-A nil)
                  (ediff-next-difference))))
    (let ((orig-window-config (current-window-configuration)))
      (unwind-protect
          (progn
            (let ((old-region (with-current-buffer old
                                (cons (point-min) (point-max))))
                  (new-region (with-current-buffer new
                                (cons (point-min) (point-max)))))
              ;; The following two bindings prevent Ediff from creating a new window.
              (let ((ediff-window-setup-function 'ediff-setup-windows-plain)
                    (ediff-split-window-function 'split-window-horizontally))
                (let ((inhibit-message t))
                  ;; Prevent Ediff from polluting the messages buffer.
                  (cl-letf (((symbol-function 'message) (lambda (&rest _)) t))
                    ;; Run wordwise diff first to replace with higher granularity.
                    (ediff-regions-internal old
                                            (car old-region)
                                            (cdr old-region)
                                            new
                                            (car new-region)
                                            (cdr new-region)
                                            nil
                                            (gensym "mevedel-ediff-")
                                            t
                                            nil)
                    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                      (let ((ediff-control-buffer-name "*Ediff Control Panel*"))
                        ;; This is very brittle.
                        (with-current-buffer (get-buffer ediff-control-buffer-name)
                          (apply-all-diffs)
                          (ediff-quit t))
                        ;; Run regular diff to also replace empty newlines.
                        (ediff-buffers old new)
                        (with-current-buffer (get-buffer ediff-control-buffer-name)
                          (apply-all-diffs)
                          (ediff-quit t)))))))))
        (set-window-configuration orig-window-config)))))

(cl-defun mevedel--patch-save-file (save-file)
  "Return a patched SAVE-FILE that matches the current version."
  (let ((save-file-version (plist-get save-file :version))
        (new-save-file ()))
    (when (string= save-file-version (mevedel-version))
      (cl-return-from mevedel--patch-save-file save-file))
    (cl-labels ((recreate-instr-ids (files-alist)
                  (let ((mevedel--id-counter 0)
                        (mevedel--id-usage-map (make-hash-table))
                        (mevedel--retired-ids ()))
                    (cl-loop for (_ . file-plist) in files-alist
                             do (let ((instr-plists (plist-get file-plist :instructions)))
                                  (cl-loop for instr-plist in instr-plists
                                           do (let ((ov-props (plist-get instr-plist :properties)))
                                                (with-temp-buffer
                                                  (let ((ov (make-overlay 1 1)))
                                                    (mapc (lambda (prop)
                                                            (overlay-put ov
                                                                         prop
                                                                         (plist-get ov-props prop)))
                                                          ov-props)
                                                    (overlay-put ov 'mevedel-id (mevedel--create-id))
                                                    (plist-put instr-plist
                                                               :properties
                                                               (overlay-properties ov))))))))
                    (cl-values files-alist mevedel--id-counter mevedel--id-usage-map mevedel--retired-ids)))
                (recreate-id-counter (files-alist)
                  (cl-multiple-value-bind (files-alist id-counter id-usage-map retired-ids)
                      (recreate-instr-ids files-alist)
                    (cl-values
                     (list :id-counter id-counter
                           :used-ids (hash-table-keys id-usage-map)
                           :retired-ids retired-ids)
                     files-alist))))
      ;; NOTE: Here is the place to introduce backwards compatibility logic for
      ;;   different save file version.
      (pcase save-file-version
        ;; Example
        ;; ("v0.5.0"
        ;;  ;; Compatibility logic ...
        ;;  )
        ;; Save file is a newer version, but needs no patching. We would still
        ;; like to display a message indicating that the file underwent a
        ;; patching procedure.
        (_ (setq new-save-file save-file)))
      (if new-save-file
          (progn
            (message "Patched loaded save file to version %s" (mevedel-version))
            (setq new-save-file (plist-put new-save-file :version (mevedel-version)))
            new-save-file)
        save-file))))

(add-hook 'find-file-hook
          (lambda ()
            (unless mevedel--inhibit-file-patching
              (mevedel--instruction-activate-buffer (current-buffer))
              (mevedel--restore-file-instructions (buffer-file-name (current-buffer))))))

(provide 'mevedel-persistence)

;;; mevedel-persistence.el ends here
