;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ediff)

(require 'mevedel-instructions)
(require 'mevedel-utilities)

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
  "Save instructions overlays to a file PATH specified by the user.

Instructions are only saved if they are associated with a buffer that has an
associated file on disk.  In other words, instructions in ethereal buffers are
not saved."
  (interactive (list (read-file-name "Save instruction list to file: ")))
  (let ((file-alist ())
        (saved-instruction-count 0))
    (cl-loop for cons in mevedel--instructions
             if (bufferp (car cons))
             do (let ((buffer (car cons)))
                  (when-let ((buffer-file-name (buffer-file-name buffer)))
                    (let ((file (file-relative-name buffer-file-name
                                                    (file-name-directory path))))
                      (when-let ((instrs (mevedel--stashed-buffer-instructions buffer)))
                        (let ((original-content
                               (with-current-buffer buffer
                                 (buffer-substring-no-properties (point-min) (point-max)))))
                          (push (cons file
                                      (list :original-content original-content
                                            :instructions instrs))
                                file-alist))
                        (cl-incf saved-instruction-count (length instrs))))))
             else do
             (push cons file-alist)
             (cl-incf saved-instruction-count (length (plist-get (cdr cons) :instructions))))
    (if (not (zerop saved-instruction-count))
        (with-temp-file path
          (let ((save-file ()))
            (setf save-file (plist-put save-file :version (mevedel-version)))
            (setf save-file
                  (plist-put save-file :ids (list :id-counter mevedel--id-counter
                                                  :used-ids (hash-table-keys mevedel--id-usage-map)
                                                  :retired-ids mevedel--retired-ids)))
            (setf save-file (plist-put save-file :files file-alist))
            (prin1 save-file (current-buffer)))
          (let ((file-count (length file-alist)))
            (message "Wrote %d mevedel instruction%s from %d file%s to %s"
                     saved-instruction-count
                     (if (= 1 saved-instruction-count) "" "s")
                     file-count
                     (if (= 1 file-count) "" "s")
                     path)))
      (when (called-interactively-p 'any)
        (message "No mevedel instructions to save")))))

;;;###autoload
(defun mevedel-load-instructions (path)
  "Load instruction overlays from a file specified by PATH."
  (interactive (list (read-file-name "Instruction list file: ")))
  (when (and (mevedel--instructions)
             (called-interactively-p 'any))
    (unless (y-or-n-p "Discard existing mevedel instructions? ")
      (user-error "Aborted")))
  (let* ((save-file (mevedel--patch-save-file (with-temp-buffer
                                                (insert-file-contents path)
                                                (read (current-buffer)))))
         (file-alist (plist-get save-file :files))
         (id-counter-plist (plist-get save-file :ids)))
    (unless (listp file-alist)
      (user-error "Malformed mevedel instruction list"))
    (mevedel-delete-all-instructions)
    (cl-destructuring-bind (&key id-counter used-ids retired-ids) id-counter-plist
      (let ((hm (make-hash-table)))
        (cl-loop for used-id in used-ids
                 do (puthash used-id t hm))
        (setq mevedel--id-counter id-counter
              mevedel--id-usage-map hm
              mevedel--retired-ids retired-ids)))
    (setq mevedel--instructions file-alist)
    (cl-loop for cons in mevedel--instructions
             do (when (stringp (car cons))
                  (setf (car cons)
                        ;; We want to turn the relative paths of the save file to be absolute paths
                        ;; that we will be able to handle.
                        (expand-file-name (car cons) (file-name-parent-directory path)))))
    (let ((total-restored 0)
          (total-kia 0)
          (total (cl-reduce #'+
                            (mapcar #'length
                                    (mapcar (lambda (plist)
                                              (plist-get plist :instructions))
                                            (mapcar #'cdr mevedel--instructions))))))
      (cl-loop for (file . _) in mevedel--instructions
               do (progn
                    (cl-multiple-value-bind (_ restored kia) (mevedel--restore-file-instructions file t)
                      (cl-incf total-restored restored)
                      (cl-incf total-kia kia))))
      (when (called-interactively-p 'any)
        (message "Restored %d out of %d instructions from %s%s"
                 total-restored
                 total
                 (expand-file-name path)
                 (if (not (zerop total-kia))
                     (format ", with %d lost to patching" total-kia)
                   ""))))))

(defun mevedel--file-outdated-p (file)
  "Determine whether or not FILE needs patching.

A file being outdated refers to the file in the instructions alist not being
up-to-date, not the actual file on the disk being outdated."
  (when (file-exists-p file)
    (when-let ((file-plist (cdr (assoc file mevedel--instructions))))
      (let ((mevedel--inhibit-file-patching t))
        (let ((original-content (plist-get file-plist :original-content))
              (buffer (find-file-noselect file)))
          (with-current-buffer buffer
            (not (string= original-content
                          (buffer-substring-no-properties (point-min) (point-max))))))))))

(defun mevedel--setup-buffer-hooks (buffer)
  "Set up buffer hooks for instruction restoration on kill/revert.

Sets up hooks to preserve mevedel instructions when BUFFER is killed or
reverted, and restores them afterward."
  (with-current-buffer buffer
    (unless (bound-and-true-p mevedel--buffer-hooks-setup)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (mevedel--buffer-has-instructions-p (current-buffer))
                    (when-let ((file (buffer-file-name buffer)))
                      (if (file-exists-p file)
                          (let ((file-contents
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-substring-no-properties (point-min) (point-max)))))
                            (mevedel--stash-buffer buffer file-contents))
                        (setq mevedel--instructions (assq-delete-all buffer mevedel--instructions))))))
                nil t))
    (add-hook 'before-revert-hook
              (lambda ()
                (when (mevedel--buffer-has-instructions-p buffer)
                  (mevedel--stash-buffer buffer)
                  (setq-local mevedel--buffer-instructions-reverted t)))
              nil t)
    (add-hook 'after-revert-hook
              (lambda ()
                (when (bound-and-true-p mevedel--buffer-instructions-reverted)
                  (mevedel--restore-file-instructions (buffer-file-name buffer) t)
                  (setq-local mevedel--buffer-instructions-reverted nil)))
              nil t)
    (setq-local mevedel--buffer-hooks-setup t)))

(cl-defun mevedel--restore-file-instructions (file &optional message)
  "Restore FILE and its INSTRUCTIONS.

Returns tree values: restored buffer, the amount of instructions restored, and
the amount of instructions lost to the patching process, if any.

If MESSAGE is non-nil, message the intent of patching outdated files."
  (let ((mevedel--inhibit-file-patching t))
    (unless (and (file-exists-p file)
                 (assoc file mevedel--instructions))
      (cl-return-from mevedel--restore-file-instructions (cl-values nil 0 0)))
    (cl-destructuring-bind (&key original-content instructions)
        (alist-get file mevedel--instructions nil nil #'equal)
      (when (or (null original-content)
                (null instructions))
        (error "Malformed file given for restoration"))
      (let ((buffer (find-file-noselect file))
            (restored 0)
            (kia 0))
        (with-current-buffer buffer
          (mevedel--setup-buffer-hooks buffer)
          (cl-labels ((restore-overlays (dstbuf instr-maybe-plists)
                        (let ((ovs ()))
                          (dolist (instr instr-maybe-plists)
                            (if (plist-get instr :overlay-start)
                                (cl-destructuring-bind (&key overlay-start overlay-end properties)
                                    instr
                                  (push (mevedel--restore-overlay dstbuf
                                                                  overlay-start
                                                                  overlay-end
                                                                  properties)
                                        ovs))
                              (push (mevedel--restore-overlay dstbuf
                                                              (overlay-start instr)
                                                              (overlay-end instr)
                                                              (overlay-properties instr))
                                    ovs)))
                          ovs)))
            (if (and mevedel-patch-outdated-instructions
                     (mevedel--file-outdated-p file))
                (if (not (executable-find "diff"))
                    (progn
                      (warn "Patching outdated instructions requires 'diff' to be installed.")
                      (setq mevedel-patch-outdated-instructions nil)
                      (restore-overlays buffer instructions))
                  (when message
                    (message "Patching outdated instructions in buffer '%s'..."
                             (buffer-name buffer)))
                  (with-temp-buffer
                    (let ((new-buffer (current-buffer)))
                      (insert-buffer-substring-no-properties buffer)
                      (with-temp-buffer
                        (insert original-content)
                        (restore-overlays (current-buffer) instructions)
                        (mevedel--wordwise-diff-patch-buffers (current-buffer) new-buffer)
                        (restore-overlays buffer (mevedel--instructions-in (point-min) (point-max)))))))
              (restore-overlays buffer instructions)))
          (let ((restored-instrs (mevedel--instructions-in (point-min) (point-max))))
            (setq restored (length restored-instrs)
                  kia (- (length instructions) restored))
            (setf (alist-get file mevedel--instructions nil nil #'equal) restored-instrs)))
        (setf (car (assoc file mevedel--instructions)) buffer)
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
      ;; There is no save file version available.  This means we are using a save file whose version
      ;; is v0.4.7 or older.  Only v0.4.7 is newer support backward save compatibility.
      ;;
      ;; This branch updates version v0.4.7 to the latest version by adding ids to the existing
      ;; instructions, and changing the save file to include the latest version number and the id
      ;; counter.
      (if (null save-file-version)
          (condition-case err
              (cl-multiple-value-bind (ids-plist files-alist) (recreate-id-counter save-file)
                (setq new-save-file (plist-put new-save-file :ids ids-plist))
                (setq new-save-file (plist-put new-save-file :files files-alist)))
            (error
             (error "Error patching a versionless save file.

Save file backward compatibility was added in v0.4.7.  If the save file is older than that, then \
unfortunately it is no longer supported.  If the save file is from v0.4.7 or newer, then this is a \
bug that you should report.

The error: %s" err)))
        (pcase save-file-version
          ("v0.4.9"
           ;; v0.4.9 had a problem where overlays which were deleted extrajudicially did not retire
           ;; their id number, causing the id to be used perpetually.  This patch cleans the used id
           ;; list.
           (cl-multiple-value-bind (ids-plist files-alist)
               (recreate-id-counter (plist-get save-file :files))
             (setq new-save-file (plist-put new-save-file :ids ids-plist))
             (setq new-save-file (plist-put new-save-file :files files-alist))))
          ;; Save file is a newer version, but needs no patching.  We would still like to display
          ;; a message indicating that the file underwent a patching procedure.
          (_ (setq new-save-file save-file))))
      (if new-save-file
          (progn
            (message "Patched loaded save file to version %s" (mevedel-version))
            (setq new-save-file (plist-put new-save-file :version (mevedel-version)))
            new-save-file)
        save-file))))

(add-hook 'find-file-hook
          (lambda ()
            (unless mevedel--inhibit-file-patching
              (mevedel--restore-file-instructions (buffer-file-name (current-buffer))))))

(provide 'mevedel-restorer)


;;; mevedel-restorer.el ends here.
