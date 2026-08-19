;;; mevedel-diff-apply.el --- Apply diffs through one file transaction -*- lexical-binding: t; -*-

;;; Commentary:

;; `mevedel-diff-apply-buffer' resolves unified diff hunks with Emacs, stages
;; each file's final text, and commits every touched path through ApplyPatch's
;; shared rollback transaction.  Visited buffers are synchronized with
;; `replace-buffer-contents', so normal instruction edit hooks own overlay and
;; directive lifecycle updates.

;;; Code:

(require 'diff-mode)

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-tool-patch'
(declare-function mevedel-tool-patch--commit "mevedel-tool-patch" (changes))
(declare-function mevedel-tool-patch--missing-parent-directories
                  "mevedel-tool-patch" (path))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))


(defun mevedel--path-has-suffix-p (path suffix)
  "Check if PATH ends with directory suffix SUFFIX."
  (let* ((path-parts (seq-filter (lambda (s) (not (string-empty-p s)))
                                 (file-name-split path)))
         (suffix-parts (seq-filter (lambda (s) (not (string-empty-p s)))
                                   (file-name-split suffix))))
    (and (>= (length path-parts) (length suffix-parts))
         (equal suffix-parts
                (last path-parts (length suffix-parts))))))

(defun mevedel--diff-find-file-operations ()
  "Determine if diff application requires the creation/deletion of files."
  (let ((ws-root (mevedel-workspace-root (mevedel-workspace)))
        files-to-create
        files-to-remove)
    (goto-char (point-min))
    (diff-beginning-of-hunk t)
    (while (pcase-let* ((`(,new ,old) (diff-hunk-file-names))
                        (new (expand-file-name (diff-filename-drop-dir new)
                                               ws-root))
                        (old (expand-file-name (diff-filename-drop-dir old)
                                               ws-root))
                        (create-p (mevedel--path-has-suffix-p old "dev/null"))
                        (delete-p (mevedel--path-has-suffix-p new "dev/null")))
             (cond (create-p
                    (push new files-to-create))
                   (delete-p
                    (push old files-to-remove)))
             (and (not (eq (prog1 (point) (ignore-errors (diff-hunk-next)))
                           (point)))
                  (looking-at-p diff-hunk-header-re))))
    (list files-to-create files-to-remove)))

(defun mevedel-diff-apply--stage-buffer (buffer edits)
  "Return BUFFER text after applying resolved EDITS from last to first."
  (let ((content
         (with-current-buffer buffer
           (save-restriction
             (widen)
             (buffer-substring-no-properties (point-min) (point-max))))))
    (with-temp-buffer
      (insert content)
      (dolist (edit edits)
        (pcase-let ((`(,start . ,end) (plist-get edit :pos)))
          (goto-char start)
          (delete-region start end)
          (insert (car (plist-get edit :dst)))))
      (buffer-string))))

(defun mevedel-diff-apply-buffer (&optional no-prompt)
  "Apply every diff hunk in one rollback transaction.

When NO-PROMPT is non-nil, reject hunks that Emacs would offer to
repair heuristically instead of prompting or modifying the diff."
  (interactive)
  (require 'mevedel-tool-patch)
  (when no-prompt
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward diff-hunk-header-re nil t)
        (error "No diff hunks found"))
      (let ((hunk-start (match-beginning 0)))
        (while hunk-start
          (goto-char hunk-start)
          (let* ((start (line-beginning-position))
                 (end (min (point-max) (+ start 500)))
                 (diagnostic
                  (concat (buffer-substring-no-properties start end)
                          (when (< end (point-max)) "..."))))
            (condition-case err
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (prompt)
                             (error "Heuristic repair required: %s" prompt))))
                  (diff-sanity-check-hunk))
              (error
               (error "Rejected ambiguous diff hunk: %s\n%s"
                      (error-message-string err)
                      diagnostic))))
          (forward-line)
          (setq hunk-start
                (and (re-search-forward diff-hunk-header-re nil t)
                     (match-beginning 0)))))))
  (pcase-let ((buffer-edits nil)
              (failures 0)
              (created-files nil)
              (created-directories nil)
              (preexisting-buffers nil)
              (preexisting-buffer-modtimes nil)
              (applied nil)
              (diff-refine nil)
              (workspace-root
               (mevedel-workspace-root (mevedel-workspace)))
              (`(,files-to-create ,files-to-remove)
               (mevedel--diff-find-file-operations)))
    (unwind-protect
        (progn
          (dolist (file files-to-create)
            (unless (file-exists-p file)
              (setq created-directories
                    (nconc
                     (mevedel-tool-patch--missing-parent-directories file)
                     created-directories))
              (when-let* ((buffer (find-buffer-visiting file)))
                (push buffer preexisting-buffers)
                (push (cons buffer
                            (with-current-buffer buffer
                              (visited-file-modtime)))
                      preexisting-buffer-modtimes))
              (make-empty-file file 'parents)
              (when-let* ((buffer (find-buffer-visiting file)))
                (with-current-buffer buffer
                  (set-visited-file-modtime)))
              (push file created-files)))
          (save-excursion
            (goto-char (point-min))
            (diff-beginning-of-hunk t)
            (while
                (pcase-let* ((`(,new ,old) (diff-hunk-file-names))
                             (source-name
                              (if (mevedel--path-has-suffix-p old "dev/null")
                                  new
                                old))
                             (source-path
                              (expand-file-name
                               (diff-filename-drop-dir source-name)
                               workspace-root))
                             (source-buffer
                              (find-buffer-visiting source-path))
                             (diff-buffer (current-buffer))
                             (location
                              (if source-buffer
                                  (with-current-buffer source-buffer
                                    (save-restriction
                                      (widen)
                                      (with-current-buffer diff-buffer
                                        (diff-find-source-location
                                         nil nil no-prompt))))
                                (diff-find-source-location
                                 nil nil no-prompt)))
                             (`(,buf ,line-offset ,pos ,_src ,dst ,switched)
                              location))
                  (if (and line-offset (not switched))
                      (push (list :buf buf :pos pos :dst dst)
                            buffer-edits)
                    (setq failures (1+ failures)))
                  (and
                   (not
                    (eq (prog1 (point) (ignore-errors (diff-hunk-next)))
                        (point)))
                   (looking-at-p diff-hunk-header-re)))))
          (if (not (zerop failures))
              (message "%d hunks failed; no buffers changed" failures)
            (let ((edits-by-buffer (make-hash-table :test 'eq))
                  buffer-order
                  changes)
              (dolist (edit (reverse buffer-edits))
                (let ((buffer (plist-get edit :buf)))
                  (unless (gethash buffer edits-by-buffer)
                    (push buffer buffer-order))
                  (push edit (gethash buffer edits-by-buffer))))
              (setq buffer-order (nreverse buffer-order))
              (dolist (buffer buffer-order)
                (let ((path (buffer-file-name buffer)))
                  (push (if (member path files-to-remove)
                            (list :path path :action 'delete)
                          (let* ((content
                                  (mevedel-diff-apply--stage-buffer
                                   buffer (gethash buffer edits-by-buffer)))
                                 (coding
                                  (buffer-local-value
                                   'buffer-file-coding-system buffer)))
                            (list :path path :action 'write
                                  :content content
                                  :bytes (encode-coding-string content coding))))
                        changes)))
              (dolist (file files-to-remove)
                (unless (seq-some (lambda (change)
                                    (equal file (plist-get change :path)))
                                  changes)
                  (push (list :path file :action 'delete) changes)))
              (mevedel-tool-patch--commit (nreverse changes))
              (setq applied t)
              (message "Saved %d buffers" (length buffer-order)))))
      (unless applied
        (dolist (file created-files)
          (when-let* ((buffer (find-buffer-visiting file))
                      ((not (memq buffer preexisting-buffers))))
            (kill-buffer buffer))
          (when (file-exists-p file)
            (delete-file file)))
        (dolist (entry preexisting-buffer-modtimes)
          (when (buffer-live-p (car entry))
            (with-current-buffer (car entry)
              (set-visited-file-modtime (cdr entry)))))
        (dolist (directory
                 (sort (delete-dups created-directories)
                       (lambda (a b) (> (length a) (length b)))))
          (when (and (file-directory-p directory)
                     (null (directory-files
                            directory nil
                            directory-files-no-dot-files-regexp)))
            (delete-directory directory)))))))

(provide 'mevedel-diff-apply)

;;; mevedel-diff-apply.el ends here
