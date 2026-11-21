;;; niwa-note-merge.el -- Checked three-way note merges -*- lexical-binding: t -*-

;;; Commentary:

;; Conflict markers live only in this separate editor.  Confirmation checks
;; both visited text and disk snapshots before the normal patch transaction.

;;; Code:

(require 'smerge-mode)
(require 'niwa-note-change)

(defvar-local niwa-note-merge--note nil "Note whose change is being merged.")
(defvar-local niwa-note-merge--record nil "Prepared change captured on opening.")
(defvar-local niwa-note-merge--sections nil "Source snapshots and editable regions.")
(defvar-local niwa-note-merge--origin nil "Window state to restore on close.")
(defvar-local niwa-note-merge--commit nil "Optional owner callback for resolved changes.")

(defun niwa-note-merge-open (name sections commit)
  "Open NAME with editable three-way SECTIONS and confirmation callback COMMIT.
Each section supplies :path, :disk, :coding, :yours, :seen and :proposed.
Optional :source-yours checks source text separately from intermediate :yours.
Optional :data is caller-owned and returned untouched.
COMMIT receives a list of (CHANGE . DATA), one per section in order, and owns
closing the editor on success."
  (let ((default-directory temporary-file-directory))
    (unless (executable-find "git")
      (user-error "Three-way merge requires a local git executable")))
  (dolist (section sections)
    (when (cl-some (lambda (key) (string-search "\0" (plist-get section key)))
                  '(:yours :seen :proposed))
      (user-error "Cannot merge binary source")))
  (let ((merge (generate-new-buffer name))
        (origin (list (selected-window) (current-buffer) (copy-marker (point))
                      (window-start) (window-dedicated-p (selected-window)))))
    (with-current-buffer merge
      (niwa-note-merge-mode)
      (setq-local niwa-note-merge--origin origin
                  niwa-note-merge--sections sections
                  niwa-note-merge--commit commit)
      (dolist (section sections)
        (insert (propertize (format "File: %s\n" (plist-get section :path))
                            'read-only t 'rear-nonsticky t))
        (plist-put section :start (copy-marker (point)))
        (niwa-note-merge--insert-three-way section)
        (plist-put section :end (copy-marker (point))))
      (dolist (section sections)
        (set-marker-insertion-type (plist-get section :end) t))
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer merge)
    merge))

(defvar-keymap niwa-note-merge-mode-map
  "C-c C-c" #'niwa-note-merge-confirm
  "C-c C-k" #'niwa-note-merge-cancel)

(define-derived-mode niwa-note-merge-mode text-mode "Note Merge"
  "Resolve note changes, then run \\[niwa-note-merge-confirm]."
  (setq-local header-line-format
              " Resolve with smerge (C-c ^ u yours · C-c ^ l AI) · C-c C-c confirm · C-c C-k cancel")
  (smerge-mode 1))

(defun niwa-note-merge-current-text (path coding)
  "Return current text of PATH using CODING, including unsaved source edits."
  (if-let* ((buffer (find-buffer-visiting path)))
      (with-current-buffer buffer
        (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
    (decode-coding-string (or (plist-get (niwa-tool-patch--snapshot path) :bytes) "") coding)))

(defun niwa-note-merge--insert-three-way (section)
  "Insert SECTION's three-way merge with only overlapping lines in conflict."
  (let* ((directory (make-temp-file "niwa-note-merge-" t))
         (files (mapcar (lambda (name) (file-name-concat directory name)) '("yours" "base" "ai")))
         (texts (mapcar (lambda (key) (plist-get section key)) '(:yours :seen :proposed)))
         (start (point))
         (default-directory temporary-file-directory)
         (coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (progn
          (cl-mapc (lambda (text file) (write-region text nil file nil 'silent)) texts files)
          (let ((status (apply #'call-process "git" nil t nil "merge-file" "--stdout" "--diff3"
                               "-L" "yours now" "-L" "what the AI saw" "-L" "AI" "--" files)))
            (unless (and (integerp status) (<= 0 status) (<= status 127))
              (error "Three-way merge failed: %s" status)))
          (save-excursion
            (goto-char start)
            (while (re-search-forward "^<<<<<<< yours now\n" nil t)
              (put-text-property (match-beginning 0) (match-end 0) 'niwa-note-merge-marker t)
              (dolist (marker '("^||||||| what the AI saw\n" "^=======\n" "^>>>>>>> AI\n"))
                (unless (re-search-forward marker nil t) (error "Incomplete three-way merge markers"))
                (put-text-property (match-beginning 0) (match-end 0) 'niwa-note-merge-marker t))))
          ;; Git adds separators at EOF inside a conflict, not to resolved text.
          (when (and (>= (- (point) start) (length ">>>>>>> AI\n"))
                     (equal (buffer-substring-no-properties (- (point) (length ">>>>>>> AI\n")) (point))
                            ">>>>>>> AI\n"))
            (save-excursion
              (cl-mapc
               (lambda (text marker)
                 (when (and (not (string-empty-p text)) (not (string-suffix-p "\n" text)))
                   (goto-char (point-max))
                   (when (re-search-backward marker start t)
                     (put-text-property (1- (point)) (point) 'niwa-note-merge-padding t))))
               texts '("^||||||| what the AI saw$" "^=======$" "^>>>>>>> AI$")))))
      (delete-directory directory t))))

;;;###autoload
(defun niwa-note-merge-change (&optional note)
  "Open NOTE's three versions in a separate smerge buffer, without writes."
  (interactive)
  (setq note (niwa-note-change--note note))
  (let* ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note)))
         (record (niwa-note-change--record note))
         sections)
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (let ((default-directory temporary-file-directory))
        (unless (executable-find "git")
          (user-error "Three-way note merge requires a local git executable")))
      (dolist (change (plist-get record :changes))
        (let* ((path (plist-get change :path))
               (base (cl-find path (plist-get record :before)
                              :key (lambda (snapshot) (plist-get snapshot :path)) :test #'equal))
               (disk (niwa-tool-patch--snapshot path))
               (buffer (find-buffer-visiting path))
               (coding (if buffer (buffer-local-value 'buffer-file-coding-system buffer)
                         (if (plist-get disk :exists)
                             (cdr (niwa-tool-patch--read-file path t)) 'utf-8-unix)))
               (yours (niwa-note-merge-current-text path coding))
               (seen (decode-coding-string (or (plist-get base :bytes) "") coding))
               (proposed (or (plist-get change :content)
                             (and (plist-get change :bytes)
                                  (decode-coding-string (plist-get change :bytes) 'undecided))
                             "")))
          (when (cl-some (lambda (text) (string-search "\0" text)) (list yours seen proposed))
            (user-error "Cannot merge binary source; ask in the conversation"))
          (push (list :path path :disk disk :coding coding :yours yours :seen seen
                      :proposed proposed :change change) sections)))
      (let ((merge (niwa-note-merge-open
                    (format "*Note %s merge*" (overlay-get note 'niwa-id))
                    (nreverse sections) nil)))
        (with-current-buffer merge
          (setq-local niwa-note-merge--note note
                      niwa-note-merge--record record))
        merge))))

(defun niwa-note-merge-cancel ()
  "Close this merge editor without changing source, restoring its origin."
  (interactive)
  (pcase-let ((`(,window ,buffer ,point ,start ,dedicated) niwa-note-merge--origin)
              (merge (current-buffer)))
    (when (and (window-live-p window) (buffer-live-p buffer))
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer)
      (set-window-point window point)
      (set-window-start window start t)
      (set-window-dedicated-p window dedicated))
    (when (markerp point) (set-marker point nil))
    (set-buffer-modified-p nil)
    (kill-buffer merge)))

(defun niwa-note-merge-confirm ()
  "Confirm resolved text only if this editor's source and waiting change match."
  (interactive)
  (let* ((note niwa-note-merge--note)
         (record niwa-note-merge--record)
         (sections niwa-note-merge--sections)
         (workspace (and (overlayp note) (overlay-buffer note)
                         (niwa--instruction-buffer-workspace (overlay-buffer note))))
         changes)
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (unless (or niwa-note-merge--commit
                  (and workspace
                       (equal (plist-get record :id)
                              (plist-get (overlay-get note 'niwa-note-prepared-change) :id))))
        (user-error "The note or waiting change changed; reopen the merge"))
      (when (text-property-any (point-min) (point-max) 'niwa-note-merge-marker t)
        (user-error "Resolve all conflict markers before confirming"))
      (dolist (section sections)
        (let* ((path (plist-get section :path))
               (coding (plist-get section :coding))
               (text (buffer-substring (plist-get section :start) (plist-get section :end))))
          (unless (and (equal (or (plist-get section :source-yours) (plist-get section :yours))
                              (niwa-note-merge-current-text path coding))
                       (niwa-change-same-p (plist-get section :disk)
                                              (niwa-tool-patch--snapshot path)))
            (user-error "Source changed again: %s; reopen the merge" path))
          (when (and (> (length text) 0)
                     (get-text-property (1- (length text)) 'niwa-note-merge-padding text))
            (setq text (substring text 0 -1)))
          (setq text (substring-no-properties text))
          (push (list :path path :before (plist-get section :disk)
                      :history-before (list :bytes (when (or (plist-get (plist-get section :disk) :exists)
                                                            (find-buffer-visiting path))
                                                    (encode-coding-string (plist-get section :yours) coding)))
                      :action (if (and (eq (plist-get (plist-get section :change) :action) 'delete)
                                       (string-empty-p text)) 'delete 'write)
                      :content text :bytes (encode-coding-string text coding)
                      :mode (or (plist-get (plist-get section :disk) :mode)
                                (plist-get (plist-get section :change) :mode))) changes)))
      (setq changes (nreverse changes))
      (if niwa-note-merge--commit
          (funcall niwa-note-merge--commit
                   (cl-mapcar (lambda (change section) (cons change (plist-get section :data)))
                              changes sections))
        (niwa-note-change--commit note record changes)
        (niwa-note-merge-cancel)))))

(provide 'niwa-note-merge)
;;; niwa-note-merge.el ends here
