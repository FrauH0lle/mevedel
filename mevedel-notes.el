;;; mevedel-notes.el -- Workspace notes in source buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Notes mark code with optional text.  They share instruction identities,
;; source edit tracking and the workspace store with references and directives.
;; Presentation and commands live here; a note never submits a request merely
;; by being created or edited.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-directive-source)
(require 'mevedel-instruction-registry)
(require 'mevedel-overlay-ui)
(require 'mevedel-overlays)
(require 'mevedel-utilities)

;; `mevedel-persistence'
(declare-function mevedel-instruction-store-touch "mevedel-persistence"
                  (&optional workspace))
(autoload 'mevedel-instruction-store-touch "mevedel-persistence")

(defvar-local mevedel-notes--last-hinted nil
  "Note most recently announced by the local entry hint.")

(defun mevedel-notes--display-text (note)
  "Return NOTE's bounded, single-line display text, keeping authored text intact."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\r\n]+" " "
                             (or (overlay-get note 'mevedel-note-text) ""))
   80 nil nil "…"))

(defun mevedel-notes--actions-hint ()
  "Return the current modified-key menu hint."
  (if-let* ((key (where-is-internal #'mevedel--ov-actions-dispatch
                                   mevedel-note-actions-map t)))
      (format "%s: edit, delete" (key-description key))
    "M-x mevedel-note-menu: edit, delete"))

(defun mevedel-notes--entry-hint ()
  "Announce the note and its next actions when point enters it."
  (let ((note (mevedel--highest-priority-instruction
               (mevedel--instructions-at (point) 'note))))
    (unless (eq note mevedel-notes--last-hinted)
      (setq mevedel-notes--last-hinted note)
      (when note
        (message "Note %s · %s · %s"
                 (overlay-get note 'mevedel-id)
                 (mevedel-notes--display-text note)
                 (mevedel-notes--actions-hint))))))

(defun mevedel-notes--render (note)
  "Render NOTE's label below its code."
  (with-current-buffer (overlay-buffer note)
    (save-excursion
      (goto-char (overlay-end note))
      (when (and (> (point) (overlay-start note)) (bolp))
        (backward-char))
      (let* ((end (line-end-position))
             (label (or (overlay-get note 'mevedel-label-overlay)
                        (let ((overlay (make-overlay
                                        (overlay-start note) end nil t t)))
                          (overlay-put overlay 'mevedel-note-owner note)
                          (overlay-put note 'mevedel-label-overlay overlay)
                          overlay)))
             (text (format "\n  Note %s · you · %s · not sent · %s"
                           (overlay-get note 'mevedel-id)
                           (mevedel-notes--display-text note)
                           (mevedel-notes--actions-hint))))
        (overlay-put note 'keymap mevedel-note-actions-map)
        (move-overlay label (overlay-start note) end)
        (unless (equal text (overlay-get label 'after-string))
          (overlay-put label 'after-string (propertize text 'face 'shadow)))))
    (add-hook 'after-change-functions #'mevedel-notes--after-change nil t)
    (add-hook 'post-command-hook #'mevedel-notes--entry-hint nil t)))

(defun mevedel-notes--after-change (beg end _old-length)
  "Reposition labels touched by BEG..END, including edits after noted code."
  (dolist (overlay (overlays-in (max (point-min) (1- beg))
                               (min (point-max) (1+ end))))
    (when-let* ((note (overlay-get overlay 'mevedel-note-owner)))
      (if (overlay-buffer note)
          (mevedel-notes--render note)
        (delete-overlay overlay)))))

(defun mevedel-notes--at-point ()
  "Return the note at point, or refuse when there is none."
  (or (mevedel--highest-priority-instruction
       (mevedel--instructions-at (point) 'note))
      (user-error "No note at point")))

;;;###autoload
(defun mevedel-note-edit (text &optional note)
  "Replace NOTE's text with TEXT; default to the note at point."
  (interactive (let ((note (mevedel-notes--at-point)))
                 (list (read-string "Note: " (overlay-get note 'mevedel-note-text))
                       note)))
  (let ((workspace (mevedel--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (setq note (or note (mevedel-notes--at-point)))
      (overlay-put note 'mevedel-note-text text)
      (mevedel-notes--render note)
      (mevedel-instruction-store-touch workspace))))

;;;###autoload
(defun mevedel-note-delete (&optional note)
  "Delete NOTE, defaulting to the note at point, without changing code."
  (interactive)
  (let ((workspace (mevedel--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (mevedel--delete-instruction (or note (mevedel-notes--at-point))))))

;;;###autoload
(defun mevedel-note-menu (&optional note)
  "Show the actions menu for NOTE, defaulting to the note at point."
  (interactive)
  (let ((workspace (mevedel--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (setq note (or note (mevedel-notes--at-point)))
      (pcase (car (read-multiple-choice
                   (format "Note %s: " (overlay-get note 'mevedel-id))
                   '((?e "edit" "Edit note text")
                     (?d "delete" "Delete note without changing code"))))
        (?e (mevedel-note-edit
             (read-string "Note: " (overlay-get note 'mevedel-note-text)) note))
        (?d (mevedel-note-delete note))))))

;;;###autoload
(defun mevedel-note-create (&optional text)
  "Create a note with optional TEXT on the region or current line.
An empty string creates a code-only note.  Return the note overlay."
  (interactive (list (read-string "Note (optional): ")))
  (let ((workspace (mevedel--instruction-buffer-workspace (current-buffer))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless buffer-file-name
        (user-error "Notes need a source file"))
      (let* ((region (use-region-p))
             (note (mevedel--create-instruction-overlay-in
                    (current-buffer)
                    (if region (region-beginning) (line-beginning-position))
                    (if region (region-end) (line-end-position))
                    (unless region 'line))))
        (overlay-put note 'mevedel-instruction-type 'note)
        (overlay-put note 'mevedel-note-text (or text ""))
        (overlay-put note 'mevedel-note-author 'user)
        (overlay-put note 'mevedel-note-state 'unsent)
        (mevedel--update-instruction-overlay note t)
        (mevedel--instruction-store-write (current-buffer))
        (deactivate-mark)
        note))))

(provide 'mevedel-notes)
;;; mevedel-notes.el ends here
