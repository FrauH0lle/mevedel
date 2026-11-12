;;; mevedel-notes.el -- Workspace notes in source buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Notes mark code with optional text.  They share instruction identities,
;; source edit tracking and the workspace store with references and directives.
;; Presentation and commands live here; a note never submits a request merely
;; by being created or edited.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
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

(defvar mevedel-notes--unsaved-noticed nil
  "Nonproject roots whose in-memory-only note status was announced.")

(defun mevedel-notes--state-ignored-p (root)
  "Return non-nil when ROOT's Git ignore policy covers `.mevedel/'."
  (with-temp-buffer
    (let ((default-directory root))
      (and (condition-case nil
               (zerop (process-file
                       "git" nil (list t nil) nil
                       "-c" "core.excludesFile=/dev/null"
                       "check-ignore" "--no-index" "-v" ".mevedel/"))
             (file-error nil))
           (string-match-p
            "\\(?:\\`\\|/\\)\\.gitignore:[0-9]+:[^!\n]"
            (buffer-string))))))

(defvar mevedel-notes-attention-state-functions nil
  "Functions deriving attention state from a note's properties plist.
The first non-nil result wins.  Called from redisplay: use only in-memory
state, never load features, perform I/O, or mutate the supplied properties.")

(defun mevedel-notes-attention-state (properties)
  "Return a note's attention state from PROPERTIES, a plist or live overlay.
Transient workflow attention takes precedence over persisted lifecycle."
  (or (if (overlayp properties)
          (overlay-get properties 'mevedel-note-attention-state)
        (plist-get properties 'mevedel-note-attention-state))
      (and mevedel-notes-attention-state-functions
           (run-hook-with-args-until-success
            'mevedel-notes-attention-state-functions
            (if (overlayp properties) (overlay-properties properties) properties)))
      (if (overlayp properties)
          (overlay-get properties 'mevedel-note-state)
        (plist-get properties 'mevedel-note-state))
      'unsent))

(defun mevedel-notes-mode-line ()
  "Return this buffer's cached-workspace note attention summary.
Only inspect in-memory records.  Never discover a workspace, visit a file,
or observe a store from redisplay."
  (when-let* ((key (or (when-let* ((workspace
                                   (or (bound-and-true-p mevedel--workspace)
                                       (and (bound-and-true-p mevedel--session)
                                            (mevedel-session-workspace mevedel--session)))))
                         (mevedel--instruction-workspace-key workspace))
                       mevedel--instruction-current-state-key))
              (state (gethash key mevedel--instruction-states)))
    (let ((working 0) (ready 0) (stale 0))
      (dolist (entry (plist-get state :instructions))
        (dolist (note (if (bufferp (car entry)) (cdr entry)
                       (plist-get (cdr entry) :instructions)))
          (let ((properties (if (overlayp note) note (plist-get note :properties))))
            (when (eq (if (overlayp note)
                          (overlay-get note 'mevedel-instruction-type)
                        (plist-get properties 'mevedel-instruction-type)) 'note)
              (pcase (mevedel-notes-attention-state properties)
                ('working (cl-incf working))
                ('ready (cl-incf ready))
                ('stale (cl-incf stale)))))))
      (when (> (+ working ready stale) 0)
        (concat " Notes: "
                (string-join
                 (delq nil (list (and (> working 0) (format "%d working" working))
                                 (and (> ready 0) (format "%d ready" ready))
                                 (and (> stale 0) (format "%d stale" stale))))
                 ", "))))))

(unless (listp global-mode-string)
  (setq global-mode-string (list global-mode-string)))
(add-to-list 'global-mode-string '(:eval (mevedel-notes-mode-line)) t)

(defun mevedel-notes--entries (workspace)
  "Return lightweight open note snapshots in WORKSPACE without visiting files.
Each snapshot holds :file, :line, :properties and :uuid.  Stashed positions
refer to the last observed source; visiting the note re-anchors it."
  (let (notes)
    (dolist (entry (plist-get (mevedel--instruction-state
                             (mevedel--instruction-workspace-key workspace))
                            :instructions))
      (if (bufferp (car entry))
          (when (buffer-live-p (car entry))
            (with-current-buffer (car entry)
              (save-restriction
                (widen)
                (dolist (note (cdr entry))
                  (when (and (overlay-buffer note)
                             (eq (overlay-get note 'mevedel-instruction-type) 'note))
                    (push (list :file buffer-file-name
                                :line (line-number-at-pos (overlay-start note))
                                :uuid (overlay-get note 'mevedel-uuid)
                                :properties
                                (plist-put (mevedel--instruction-persisted-properties note)
                                           'mevedel-note-attention-state
                                           (overlay-get note 'mevedel-note-attention-state)))
                          notes))))))
        (dolist (note (plist-get (cdr entry) :instructions))
          (let ((properties (plist-get note :properties))
                (content (plist-get (cdr entry) :original-content)))
            (when (eq (plist-get properties 'mevedel-instruction-type) 'note)
              (push (list :file (car entry)
                          :line (if content
                                    (1+ (cl-count ?\n content :end
                                                  (min (length content)
                                                       (max 0 (1- (plist-get note :overlay-start))))))
                                  1)
                          :uuid (plist-get properties 'mevedel-uuid)
                          :properties (copy-tree properties))
                    notes))))))
    (sort notes (lambda (a b)
                  (< (plist-get (plist-get a :properties) 'mevedel-id)
                     (plist-get (plist-get b :properties) 'mevedel-id))))))

(defun mevedel-notes--resolve (identity workspace)
  "Return the live note with numeric ID or UUID IDENTITY in WORKSPACE.
Restore only the selected note's file, not every stashed source file."
  (when-let* ((entry (cl-find-if
                     (lambda (note)
                       (equal identity (if (integerp identity)
                                           (plist-get (plist-get note :properties) 'mevedel-id)
                                         (plist-get note :uuid))))
                     (mevedel-notes--entries workspace))))
    (let ((file (plist-get entry :file))
          (uuid (plist-get entry :uuid))
          (mevedel--instruction-state-key-override
           (mevedel--instruction-workspace-key workspace)))
      (mevedel--restore-file-instructions file nil workspace)
      (when-let* ((buffer (find-buffer-visiting file)))
        (cl-find uuid (cdr (assq buffer (mevedel--instruction-alist)))
                 :key (lambda (note) (overlay-get note 'mevedel-uuid))
                 :test #'equal)))))

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
(defun mevedel-note-set-tags (tags &optional note)
  "Replace NOTE's tags with TAGS, a list of symbols.
Default to the note at point.  An empty list removes every tag."
  (interactive
   (let* ((note (mevedel-notes--at-point))
          (tags (overlay-get note 'mevedel-note-tags)))
     (list (mapcar #'intern
                   (completing-read-multiple
                    "Tags (empty removes all): " (mapcar #'symbol-name tags)
                    nil nil (mapconcat #'symbol-name tags ",")))
           note)))
  (setq note (or note (mevedel-notes--at-point)))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless (and (proper-list-p tags) (cl-every #'symbolp tags))
        (user-error "Tags must be a list of symbols"))
      (overlay-put note 'mevedel-note-tags (delete-dups (copy-sequence tags)))
      (mevedel-notes--render note)
      (mevedel-instruction-store-touch workspace))))

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
      (unless (file-directory-p (mevedel-workspace-state-dir workspace))
        (if-let* ((project (mevedel-workspace--project-current
                           (file-name-directory buffer-file-name))))
            (progn
              (setq workspace (mevedel-workspace-get-or-create
                               'project (project-root project) (project-root project)
                               (project-name project)))
              (setq-local mevedel--workspace workspace)
              (make-directory (mevedel-workspace-state-dir workspace) t)
              (unless (mevedel-notes--state-ignored-p
                       (mevedel-workspace-root workspace))
                (mevedel-diagnostic-message
                 (mevedel-diagnostic-scope workspace)
                 "Created .mevedel/; it is not covered by the project's .gitignore")))
          (let ((root (mevedel-workspace-root workspace)))
            (unless (member root mevedel-notes--unsaved-noticed)
              (push root mevedel-notes--unsaved-noticed)
              (mevedel-diagnostic-message
               (mevedel-diagnostic-scope workspace)
               "Notes outside a project are not saved")))))
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
