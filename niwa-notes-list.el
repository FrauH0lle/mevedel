;;; niwa-notes-list.el -- Workspace-wide notes list -*- lexical-binding: t -*-

;;; Commentary:

;; Explicitly observe the workspace store when opening or refreshing the list.
;; Rows use live or stashed metadata, never opening source files just to list
;; them.  Actions resolve the selected UUID rather than a reusable display ID.

;;; Code:

(require 'tabulated-list)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-window)

(defvar-local niwa-notes-list--workspace nil
  "Workspace whose notes this list displays.")

(defvar-local niwa-notes-list--open-entries nil
  "Cached open rows, collected at the last explicit refresh.")

(defvar-local niwa-notes-list--closed-rows nil
  "Cached closed rows, collected at the last explicit refresh.")

(defvar-local niwa-notes-list--closed-expanded nil
  "Whether the recently closed group is expanded.")

(defvar niwa-notes-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "v") #'niwa-notes-list-visit)
    (define-key map (kbd "RET") #'niwa-notes-list-visit)
    (define-key map (kbd "d") #'niwa-notes-list-delete)
    (define-key map (kbd "x") #'niwa-notes-list-done)
    (define-key map (kbd "r") #'niwa-notes-list-restore)
    (define-key map (kbd "TAB") #'niwa-notes-list-toggle-closed)
    (define-key map (kbd "t") #'niwa-notes-list-tags)
    (define-key map (kbd "e") #'niwa-notes-list-edit)
    (define-key map (kbd "s") #'niwa-notes-list-send)
    (define-key map (kbd "a") #'niwa-notes-list-apply)
    (define-key map (kbd "T") #'niwa-notes-list-take)
    (define-key map (kbd "g") #'niwa-notes-list-refresh)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "q") #'niwa-window-quit)
    map)
  "Single-letter actions on the notes list, never on source text.")

(define-derived-mode niwa-notes-list-mode tabulated-list-mode "Notes"
  "Workspace notes grouped by the attention they need.
Use v or RET to visit, s to send, a to apply, T to take/drop, e to edit,
t for tags, d to delete, x for done, r to restore, TAB to fold, g to refresh
\(gr in Evil normal state), ? for this help and q to return to the invoking
window."
  (setq tabulated-list-format
        [("#" 5 nil) ("Where" 26 nil) ("By" 14 nil)
         ("Text" 40 nil) ("State" 24 nil) ("Tags" 0 nil)])
  (setq-local revert-buffer-function (lambda (&rest _) (niwa-notes-list-refresh)))
  (tabulated-list-init-header))

(defun niwa-notes-list--entry (note)
  "Return the tabulated-list row for NOTE's snapshot."
  (let ((props (plist-get note :properties)))
    (list (plist-get note :uuid)
          (vector
           (number-to-string (plist-get props 'niwa-id))
           (format "%s:%s" (file-relative-name
                             (plist-get note :file)
                             (niwa-workspace-root niwa-notes-list--workspace))
                   (plist-get note :line))
           (niwa-notes--author-name (plist-get props 'niwa-note-author))
           (replace-regexp-in-string "[\n\r\t]+" " "
                                     (or (plist-get props 'niwa-note-text) ""))
           (format "%s%s%s" (or (plist-get note :closed-reason)
                               (if (eq 'taken (niwa-notes-attention-state props))
                                   (if (niwa-note-watching-p props niwa-notes-list--workspace)
                                       "taken; Buddy watches"
                                     "taken")
                                 (niwa-notes-attention-state props)))
                   (if (and (eq 'taken (plist-get props 'niwa-note-state))
                            (not (niwa-note-watching-p props niwa-notes-list--workspace)))
                       "; paused" "")
                   (if (plist-get note :source-missing) "; source missing" ""))
           (mapconcat #'symbol-name (plist-get props 'niwa-note-tags) ", ")))))

(defun niwa-notes-list--closed-entries (_workspace)
  "Return the cached recently closed group for _WORKSPACE."
  (when niwa-notes-list--closed-rows
    (append (list (list 'closed (vector "" (format "%s Recently closed (%d)"
                                             (if niwa-notes-list--closed-expanded "[-]" "[+]")
                                             (length niwa-notes-list--closed-rows))
                                   "" "" "" "")))
            (and niwa-notes-list--closed-expanded niwa-notes-list--closed-rows))))

(defun niwa-notes-list--print ()
  "Print cached rows without observing the store or visiting source files."
  (let ((id (tabulated-list-get-id))
        (line (line-number-at-pos))
        (column (current-column)))
    (setq tabulated-list-entries
          (append niwa-notes-list--open-entries
                  (niwa-notes-list--closed-entries niwa-notes-list--workspace)))
    (tabulated-list-print t)
    (unless (equal id (tabulated-list-get-id))
      (goto-char (point-min))
      (forward-line (1- line))
      (when (and (eobp) (not (bobp))) (forward-line -1))
      (move-to-column column))))

(defun niwa-notes-list-toggle-closed ()
  "Expand or fold cached recently closed rows without store observation."
  (interactive)
  (setq niwa-notes-list--closed-expanded (not niwa-notes-list--closed-expanded))
  (niwa-notes-list--print))

(defun niwa-notes-list-refresh ()
  "Observe the workspace store and refresh this notes list."
  (interactive)
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope niwa-notes-list--workspace)
    (niwa-instruction-store-observe niwa-notes-list--workspace)
    (let ((notes (niwa-notes-open niwa-notes-list--workspace))
          entries)
      (dolist (group '(ready stale working sent taken unsent))
        (when-let* ((members
                     (cl-remove-if-not
                      (lambda (note)
                        (eq group (niwa-notes-attention-state
                                   (plist-get note :properties))))
                      notes)))
          (setq entries
                (append entries
                        (list (list group (vector "" (capitalize (symbol-name group))
                                                "" "" "" "")))
                        (mapcar #'niwa-notes-list--entry members)))))
      (setq niwa-notes-list--open-entries entries
            niwa-notes-list--closed-rows
            (mapcar (lambda (entry)
                      (let ((instruction (plist-get entry :instruction)))
                        (niwa-notes-list--entry
                         (list :file (plist-get entry :file)
                               :line (or (car (plist-get instruction :closed-position)) 1)
                               :uuid (plist-get (plist-get instruction :properties) 'niwa-uuid)
                               :properties (plist-get instruction :properties)
                               :closed-reason (plist-get instruction :closed-reason)))))
                    (niwa-notes-closed niwa-notes-list--workspace t)))
      (niwa-notes-list--print))))

(defun niwa-notes-list--selected-note ()
  "Resolve the selected row's exact live note, or refuse."
  (or (when-let* ((uuid (tabulated-list-get-id))
                  (note (niwa-note-resolve
                         uuid niwa-notes-list--workspace)))
        (and (eq (overlay-get note 'niwa-instruction-type) 'note) note))
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope niwa-notes-list--workspace)
       "No open note on this row")))

(defun niwa-notes-list-visit ()
  "Visit the selected note's code."
  (interactive)
  (let ((note (niwa-notes-list--selected-note)))
    (pop-to-buffer (overlay-buffer note))
    (goto-char (overlay-start note))))

(defun niwa-notes-list-delete ()
  "Delete the selected note without changing its code."
  (interactive)
  (niwa-note-delete (niwa-notes-list--selected-note))
  (niwa-notes-list-refresh))

(defun niwa-notes-list-send ()
  "Send or reopen the selected note's conversation and return its view."
  (interactive)
  (let ((list-buffer (current-buffer))
        (view (niwa-note-send (niwa-notes-list--selected-note))))
    (with-current-buffer list-buffer (niwa-notes-list-refresh))
    view))

(defun niwa-notes-list-apply ()
  "Apply the selected note's prepared change."
  (interactive)
  (niwa-note-apply-change (niwa-notes-list--selected-note))
  (niwa-notes-list-refresh))

(defun niwa-notes-list-done ()
  "Mark the selected open note done without changing its code."
  (interactive)
  (niwa-note-done (niwa-notes-list--selected-note))
  (niwa-notes-list-refresh))

(defun niwa-notes-list-take ()
  "Take the selected note, or drop it if it is already taken."
  (interactive)
  (let ((note (niwa-notes-list--selected-note)))
    (if (niwa-note-watching-p note)
        (niwa-note-drop note)
      (niwa-note-take note)))
  (niwa-notes-list-refresh))

(defun niwa-notes-list-restore ()
  "Restore the selected closed note by its exact UUID."
  (interactive)
  (niwa-note-restore (tabulated-list-get-id) niwa-notes-list--workspace)
  (niwa-notes-list-refresh))

(defun niwa-notes-list-tags ()
  "Edit the selected note's complete tag set."
  (interactive)
  (let ((note (niwa-notes-list--selected-note)))
    (niwa-note-set-tags (niwa-notes--read-tags note) note))
  (niwa-notes-list-refresh))

(defun niwa-notes-list-edit ()
  "Edit the selected note's text."
  (interactive)
  (let ((note (niwa-notes-list--selected-note)))
    (niwa-note-edit (read-string "Note: " (overlay-get note 'niwa-note-text)) note))
  (niwa-notes-list-refresh))

;;;###autoload
(defun niwa-notes-list (&optional workspace)
  "Open the workspace-wide notes list for WORKSPACE and return its buffer."
  (interactive)
  (setq workspace (or workspace (niwa--instruction-buffer-workspace (current-buffer))))
  (unless workspace
    (niwa-diagnostic-user-error 'unavailable "No workspace for notes"))
  (let ((buffer (get-buffer-create
                 (format "*niwa notes: %s*" (niwa-workspace-root workspace)))))
    (with-current-buffer buffer
      (niwa-notes-list-mode)
      (setq niwa-notes-list--workspace workspace)
      (setq-local niwa--workspace workspace)
      (niwa-notes-list-refresh))
    (niwa-window-display-buffer buffer)
    buffer))

(provide 'niwa-notes-list)
;;; niwa-notes-list.el ends here
