;;; mevedel-notes-list.el -- Workspace-wide notes list -*- lexical-binding: t -*-

;;; Commentary:

;; Explicitly observe the workspace store when opening or refreshing the list.
;; Rows use live or stashed metadata, never opening source files just to list
;; them.  Actions resolve the selected UUID rather than a reusable display ID.

;;; Code:

(require 'tabulated-list)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-window)

(defvar-local mevedel-notes-list--workspace nil
  "Workspace whose notes this list displays.")

(defvar mevedel-notes-list-extra-entries-functions nil
  "Functions appending additional groups to the notes list.
Each receives the workspace and returns tabulated-list entries.")

(defvar mevedel-notes-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "v") #'mevedel-notes-list-visit)
    (define-key map (kbd "RET") #'mevedel-notes-list-visit)
    (define-key map (kbd "d") #'mevedel-notes-list-delete)
    (define-key map (kbd "t") #'mevedel-notes-list-tags)
    (define-key map (kbd "e") #'mevedel-notes-list-edit)
    (define-key map (kbd "g") #'mevedel-notes-list-refresh)
    (define-key map (kbd "q") #'mevedel-window-quit)
    map)
  "Single-letter actions on the notes list, never on source text.")

(define-derived-mode mevedel-notes-list-mode tabulated-list-mode "Notes"
  "Workspace notes grouped by the attention they need.
Use v or RET to visit, e to edit text, t to edit tags, d to delete,
g to refresh and q to return to the invoking window."
  (setq tabulated-list-format
        [("#" 5 nil) ("Where" 26 nil) ("By" 14 nil)
         ("Text" 40 nil) ("State" 12 nil) ("Tags" 0 nil)])
  (setq-local revert-buffer-function (lambda (&rest _) (mevedel-notes-list-refresh)))
  (tabulated-list-init-header))

(defun mevedel-notes-list--entry (note)
  "Return the tabulated-list row for NOTE's snapshot."
  (let ((props (plist-get note :properties)))
    (list (plist-get note :uuid)
          (vector
           (number-to-string (plist-get props 'mevedel-id))
           (format "%s:%s" (file-relative-name
                             (plist-get note :file)
                             (mevedel-workspace-root mevedel-notes-list--workspace))
                   (plist-get note :line))
           (format "%s" (plist-get props 'mevedel-note-author))
           (replace-regexp-in-string "[\n\r\t]+" " "
                                     (or (plist-get props 'mevedel-note-text) ""))
           (symbol-name (mevedel-notes-attention-state props))
           (mapconcat #'symbol-name (plist-get props 'mevedel-note-tags) ", ")))))

(defun mevedel-notes-list-refresh ()
  "Observe the workspace store and refresh this notes list."
  (interactive)
  (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope mevedel-notes-list--workspace)
    (mevedel-instruction-store-observe mevedel-notes-list--workspace)
    (let ((notes (mevedel-notes--entries mevedel-notes-list--workspace))
          entries)
      (dolist (group '(ready stale working sent taken unsent))
        (when-let* ((members
                     (cl-remove-if-not
                      (lambda (note)
                        (eq group (mevedel-notes-attention-state
                                   (plist-get note :properties))))
                      notes)))
          (setq entries
                (append entries
                        (list (list nil (vector "" (capitalize (symbol-name group))
                                                "" "" "" "")))
                        (mapcar #'mevedel-notes-list--entry members)))))
      (dolist (function mevedel-notes-list-extra-entries-functions)
        (setq entries (append entries (funcall function mevedel-notes-list--workspace))))
      (setq tabulated-list-entries entries)
      (tabulated-list-print t))))

(defun mevedel-notes-list--selected-note ()
  "Resolve the selected row's exact live note, or refuse."
  (or (when-let* ((uuid (tabulated-list-get-id))
                  (note (mevedel-notes--resolve
                         uuid mevedel-notes-list--workspace)))
        (and (eq (overlay-get note 'mevedel-instruction-type) 'note) note))
      (mevedel-diagnostic-user-error
       (mevedel-diagnostic-scope mevedel-notes-list--workspace)
       "No open note on this row")))

(defun mevedel-notes-list-visit ()
  "Visit the selected note's code."
  (interactive)
  (let ((note (mevedel-notes-list--selected-note)))
    (pop-to-buffer (overlay-buffer note))
    (goto-char (overlay-start note))))

(defun mevedel-notes-list-delete ()
  "Delete the selected note without changing its code."
  (interactive)
  (mevedel-note-delete (mevedel-notes-list--selected-note))
  (mevedel-notes-list-refresh))

(defun mevedel-notes-list-tags ()
  "Edit the selected note's complete tag set."
  (interactive)
  (let* ((note (mevedel-notes-list--selected-note))
         (tags (overlay-get note 'mevedel-note-tags)))
    (mevedel-note-set-tags
     (mapcar #'intern (completing-read-multiple
                       "Tags (empty removes all): " (mapcar #'symbol-name tags)
                       nil nil (mapconcat #'symbol-name tags ",")))
     note))
  (mevedel-notes-list-refresh))

(defun mevedel-notes-list-edit ()
  "Edit the selected note's text."
  (interactive)
  (let ((note (mevedel-notes-list--selected-note)))
    (mevedel-note-edit (read-string "Note: " (overlay-get note 'mevedel-note-text)) note))
  (mevedel-notes-list-refresh))

;;;###autoload
(defun mevedel-notes-list (&optional workspace)
  "Open the workspace-wide notes list for WORKSPACE and return its buffer."
  (interactive)
  (setq workspace (or workspace (mevedel--instruction-buffer-workspace (current-buffer))))
  (unless workspace
    (mevedel-diagnostic-user-error 'unavailable "No workspace for notes"))
  (let ((buffer (get-buffer-create
                 (format "*mevedel notes: %s*" (mevedel-workspace-root workspace)))))
    (with-current-buffer buffer
      (mevedel-notes-list-mode)
      (setq mevedel-notes-list--workspace workspace)
      (setq-local mevedel--workspace workspace)
      (mevedel-notes-list-refresh))
    (mevedel-window-display-buffer buffer)
    buffer))

(provide 'mevedel-notes-list)
;;; mevedel-notes-list.el ends here
