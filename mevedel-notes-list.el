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

(defvar-local mevedel-notes-list--open-entries nil
  "Cached open rows, collected at the last explicit refresh.")

(defvar-local mevedel-notes-list--closed-rows nil
  "Cached closed rows, collected at the last explicit refresh.")

(defvar-local mevedel-notes-list--closed-expanded nil
  "Whether the recently closed group is expanded.")

(defvar mevedel-notes-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "v") #'mevedel-notes-list-visit)
    (define-key map (kbd "RET") #'mevedel-notes-list-visit)
    (define-key map (kbd "d") #'mevedel-notes-list-delete)
    (define-key map (kbd "x") #'mevedel-notes-list-done)
    (define-key map (kbd "r") #'mevedel-notes-list-restore)
    (define-key map (kbd "TAB") #'mevedel-notes-list-toggle-closed)
    (define-key map (kbd "t") #'mevedel-notes-list-tags)
    (define-key map (kbd "e") #'mevedel-notes-list-edit)
    (define-key map (kbd "s") #'mevedel-notes-list-send)
    (define-key map (kbd "a") #'mevedel-notes-list-apply)
    (define-key map (kbd "T") #'mevedel-notes-list-take)
    (define-key map (kbd "g") #'mevedel-notes-list-refresh)
    (define-key map (kbd "q") #'mevedel-window-quit)
    map)
  "Single-letter actions on the notes list, never on source text.")

(define-derived-mode mevedel-notes-list-mode tabulated-list-mode "Notes"
  "Workspace notes grouped by the attention they need.
Use v or RET to visit, s to send, a to apply, T to take/drop, e to edit,
t for tags, d to delete, x for done, r to restore, TAB to fold, g to refresh
and q to return to the invoking window."
  (setq tabulated-list-format
        [("#" 5 nil) ("Where" 26 nil) ("By" 14 nil)
         ("Text" 40 nil) ("State" 24 nil) ("Tags" 0 nil)])
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
           (format "%s%s%s" (or (plist-get note :closed-reason)
                               (if (eq 'taken (mevedel-notes-attention-state props))
                                   (if (mevedel-note-watching-p props mevedel-notes-list--workspace)
                                       "taken; Buddy watches"
                                     "taken")
                                 (mevedel-notes-attention-state props)))
                   (if (and (eq 'taken (plist-get props 'mevedel-note-state))
                            (not (mevedel-note-watching-p props mevedel-notes-list--workspace)))
                       "; paused" "")
                   (if (plist-get note :source-missing) "; source missing" ""))
           (mapconcat #'symbol-name (plist-get props 'mevedel-note-tags) ", ")))))

(defun mevedel-notes-list--closed-entries (_workspace)
  "Return the cached recently closed group for _WORKSPACE."
  (when mevedel-notes-list--closed-rows
    (append (list (list nil (vector "" (format "%s Recently closed (%d)"
                                             (if mevedel-notes-list--closed-expanded "[-]" "[+]")
                                             (length mevedel-notes-list--closed-rows))
                                   "" "" "" "")))
            (and mevedel-notes-list--closed-expanded mevedel-notes-list--closed-rows))))

(defun mevedel-notes-list--print ()
  "Print cached rows without observing the store or visiting source files."
  (setq tabulated-list-entries
        (append mevedel-notes-list--open-entries
                (mevedel-notes-list--closed-entries mevedel-notes-list--workspace)))
  (tabulated-list-print t))

(defun mevedel-notes-list-toggle-closed ()
  "Expand or fold cached recently closed rows without store observation."
  (interactive)
  (setq mevedel-notes-list--closed-expanded (not mevedel-notes-list--closed-expanded))
  (mevedel-notes-list--print))

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
      (setq mevedel-notes-list--open-entries entries
            mevedel-notes-list--closed-rows
            (mapcar (lambda (entry)
                      (let ((instruction (plist-get entry :instruction)))
                        (mevedel-notes-list--entry
                         (list :file (plist-get entry :file)
                               :line (car (plist-get instruction :closed-position))
                               :uuid (plist-get (plist-get instruction :properties) 'mevedel-uuid)
                               :properties (plist-get instruction :properties)
                               :closed-reason (plist-get instruction :closed-reason)))))
                    (mevedel-notes-closed mevedel-notes-list--workspace t)))
      (mevedel-notes-list--print))))

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

(defun mevedel-notes-list-send ()
  "Send or reopen the selected note's conversation and return its view."
  (interactive)
  (let ((list-buffer (current-buffer))
        (view (mevedel-note-send (mevedel-notes-list--selected-note))))
    (with-current-buffer list-buffer (mevedel-notes-list-refresh))
    view))

(defun mevedel-notes-list-apply ()
  "Apply the selected note's prepared change."
  (interactive)
  (mevedel-note-apply-change (mevedel-notes-list--selected-note))
  (mevedel-notes-list-refresh))

(defun mevedel-notes-list-done ()
  "Mark the selected open note done without changing its code."
  (interactive)
  (mevedel-note-done (mevedel-notes-list--selected-note))
  (mevedel-notes-list-refresh))

(defun mevedel-notes-list-take ()
  "Take the selected note, or drop it if it is already taken."
  (interactive)
  (let ((note (mevedel-notes-list--selected-note)))
    (if (mevedel-note-watching-p note)
        (mevedel-note-drop note)
      (mevedel-note-take note)))
  (mevedel-notes-list-refresh))

(defun mevedel-notes-list-restore ()
  "Restore the selected closed note by its exact UUID."
  (interactive)
  (mevedel-note-restore (tabulated-list-get-id) mevedel-notes-list--workspace)
  (mevedel-notes-list-refresh))

(defun mevedel-notes-list-tags ()
  "Edit the selected note's complete tag set."
  (interactive)
  (let ((note (mevedel-notes-list--selected-note)))
    (mevedel-note-set-tags (mevedel-notes--read-tags note) note))
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
