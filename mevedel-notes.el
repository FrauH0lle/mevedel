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
(declare-function mevedel-instruction-store-observe "mevedel-persistence"
                  (workspace))
(declare-function mevedel-instruction-store-prune-closed "mevedel-persistence"
                  (workspace))
(declare-function mevedel-instruction-store-touch "mevedel-persistence"
                  (&optional workspace))
(autoload 'mevedel-instruction-store-observe "mevedel-persistence")
(autoload 'mevedel-instruction-store-prune-closed "mevedel-persistence")
(autoload 'mevedel-instruction-store-touch "mevedel-persistence")

;; `mevedel-buddy-note'
(autoload 'mevedel-buddy-dismiss-note "mevedel-buddy-note" nil t)
(autoload 'mevedel-buddy-keep-note "mevedel-buddy-note" nil t)

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

(defun mevedel-notes--display-text (note)
  "Return NOTE's bounded, single-line display text, keeping authored text intact."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\r\n]+" " "
                             (or (overlay-get note 'mevedel-note-text) ""))
   80 nil nil "…"))

(defun mevedel-notes--actions-hint (&optional note)
  "Return the current modified-key menu hint for NOTE."
  (format "%s: edit, done, %s"
          (if-let* ((key (where-is-internal #'mevedel--ov-actions-dispatch
                                           mevedel-note-actions-map t)))
              (key-description key)
            "M-x mevedel-note-menu")
          (if (and note (eq 'buddy (overlay-get note 'mevedel-note-author)))
              "dismiss, keep" "delete")))

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
                 (mevedel-notes--actions-hint note))))))

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
             (text (format "\n  Note %s · %s · %s · not sent · %s"
                           (overlay-get note 'mevedel-id)
                           (pcase (overlay-get note 'mevedel-note-author)
                             ('buddy "Buddy") ('agent "AI") (_ "you"))
                           (mevedel-notes--display-text note)
                           (mevedel-notes--actions-hint note))))
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
  "Move NOTE to recently closed as deleted, without changing code.
NOTE defaults to the note at point.  Restore it with `mevedel-note-restore'."
  (interactive)
  (mevedel-note-close (or note (mevedel-notes--at-point)) 'deleted))

(defun mevedel-note-close (note reason)
  "Move live NOTE to recently closed with REASON, retaining its attachments.
REASON is `done', `deleted' or `dismissed'.  Closing never changes source code."
  (let* ((buffer (overlay-buffer note))
         (workspace (mevedel--instruction-buffer-workspace buffer))
         (mevedel--instruction-state-key-override
          (mevedel--instruction-workspace-key workspace)))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless (memq reason '(done deleted dismissed))
        (user-error "Unknown close reason: %s" reason))
      (unless (and buffer (eq (overlay-get note 'mevedel-instruction-type) 'note))
        (user-error "Not a live note"))
      (with-current-buffer buffer
        (let* ((properties (mevedel--instruction-persisted-properties note))
               (instruction
                (list :overlay-start (overlay-start note)
                      :overlay-end (overlay-end note)
                      :anchor (mevedel--instruction-anchor-for-instruction note)
                      :closed-position
                      (save-excursion
                        (save-restriction
                          (widen)
                          (let ((start (overlay-start note))
                                (end (overlay-end note)))
                            (list (line-number-at-pos start)
                                  (progn (goto-char start) (- start (line-beginning-position)))
                                  (line-number-at-pos end)
                                  (progn (goto-char end) (- end (line-beginning-position)))))))
                      :open-state (overlay-get note 'mevedel-note-state)
                      :closed-reason reason :closed-at (float-time)
                      :properties (plist-put properties 'mevedel-note-state 'closed))))
          (push (list :file buffer-file-name :instruction instruction)
                (plist-get (mevedel--instruction-state
                            (mevedel--instruction-operation-state-key))
                           :closed-notes))
          ;; Explicit closure wins over older source geometry undo.  Reserve the
          ;; display number until expiry, rather than retiring it on close.
          (overlay-put note 'mevedel-marked-for-deletion t)
          (mevedel--delete-instruction-overlay note)
          (setf (alist-get buffer (mevedel--instruction-alist))
                (delq note (alist-get buffer (mevedel--instruction-alist))))
          (mevedel-instruction-store-prune-closed workspace)
          (mevedel-instruction-store-touch workspace))))))

;;;###autoload
(defun mevedel-note-done (&optional note)
  "Explicitly mark NOTE done, defaulting to the note at point."
  (interactive)
  (mevedel-note-close (or note (mevedel-notes--at-point)) 'done))

(defun mevedel-notes-closed (&optional workspace loaded-only)
  "Return snapshots of recently closed notes in WORKSPACE.
Each entry has :file and :instruction, the serialized note with :closed-reason,
:closed-at and :open-state.  Callers may change these snapshots freely.
When LOADED-ONLY is non-nil, inspect only loaded state without observing the
store.  Retention pruning and snapshot copying still apply."
  (setq workspace (or workspace (mevedel--instruction-buffer-workspace
                                (current-buffer))))
  (unless loaded-only (mevedel-notes--load-closed workspace))
  (mevedel-instruction-store-prune-closed workspace)
  (mevedel--instruction-serializable-value
   (plist-get (mevedel--instruction-state
               (mevedel--instruction-workspace-key workspace))
              :closed-notes)))

(defun mevedel-notes--load-closed (workspace)
  "Load WORKSPACE's store on cold first use of closed-note commands."
  (unless (plist-get (mevedel--instruction-state
                      (mevedel--instruction-workspace-key workspace))
                     :store-observation)
    (mevedel-instruction-store-observe workspace)))

;;;###autoload
(defun mevedel-note-restore (id &optional workspace)
  "Restore the recently closed note with display number or UUID ID in WORKSPACE.
Return its live source overlay, retaining its text, tags and attachments."
  (interactive
   (let* ((workspace (mevedel--instruction-buffer-workspace (current-buffer)))
          (choices (mapcar
                    (lambda (entry)
                      (let* ((instruction (plist-get entry :instruction))
                             (props (plist-get instruction :properties)))
                        (cons (format "%s: %s (%s)"
                                      (plist-get props 'mevedel-id)
                                      (plist-get props 'mevedel-note-text)
                                      (plist-get instruction :closed-reason))
                              (plist-get props 'mevedel-uuid))))
                    (mevedel-notes-closed workspace))))
     (list (cdr (assoc (completing-read "Restore note: " choices nil t) choices))
           workspace)))
  (setq workspace (or workspace (mevedel--instruction-buffer-workspace
                                (current-buffer))))
  (mevedel-notes--load-closed workspace)
  (mevedel-instruction-store-prune-closed workspace)
  (let* ((mevedel--instruction-state-key-override
          (mevedel--instruction-workspace-key workspace))
         (state (mevedel--instruction-state
                 (mevedel--instruction-operation-state-key)))
         (entry (cl-find id (plist-get state :closed-notes)
                         :key (lambda (entry)
                                (plist-get
                                 (plist-get (plist-get entry :instruction) :properties)
                                 (if (numberp id) 'mevedel-id 'mevedel-uuid)))
                         :test #'equal)))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless entry (user-error "No recently closed note %s" id))
      (let* ((instruction (plist-get entry :instruction))
             (buffer (find-file-noselect (plist-get entry :file)))
             (properties (copy-tree (plist-get instruction :properties))))
        (with-current-buffer buffer
          (setq-local mevedel--workspace workspace)
          (plist-put properties 'mevedel-note-state (plist-get instruction :open-state))
          (let ((note
                 (save-excursion
                   (save-restriction
                     (widen)
                     (let ((bounds (mevedel-notes--restore-bounds instruction)))
                       (mevedel--restore-overlay buffer (car bounds) (cdr bounds)
                                                 properties))))))
            (setf (plist-get state :closed-notes)
                  (delq entry (plist-get state :closed-notes)))
            (push note (alist-get buffer (mevedel--instruction-alist)))
            (mevedel--setup-buffer-hooks buffer)
            (save-restriction
              (widen)
              (mevedel--update-instruction-overlay note t))
            (mevedel-instruction-store-touch workspace)
            note))))))

(defun mevedel-notes--restore-bounds (instruction)
  "Resolve closed INSTRUCTION in the widened source, falling back to its line."
  (or (mevedel-persistence-resolve-instruction-anchor
       (plist-get instruction :overlay-start)
       (plist-get instruction :overlay-end)
       (plist-get instruction :anchor) nil)
      (pcase-let ((`(,line ,column ,end-line ,end-column)
                   (plist-get instruction :closed-position)))
        (goto-char (point-min))
        (forward-line (1- line))
        (let ((start (min (line-end-position) (+ (point) column))))
          (goto-char (point-min))
          (forward-line (1- end-line))
          (cons start (max start (min (line-end-position)
                                     (+ (point) end-column))))))))

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
                   (append '((?e "edit" "Edit note text")
                             (?x "done" "Mark note done"))
                           (if (eq 'buddy (overlay-get note 'mevedel-note-author))
                               '((?d "dismiss" "Dismiss this remark")
                                 (?k "keep" "Keep as your own note"))
                             '((?d "delete" "Delete note without changing code"))))))
        (?e (mevedel-note-edit
             (read-string "Note: " (overlay-get note 'mevedel-note-text)) note))
        (?x (mevedel-note-done note))
        (?d (if (eq 'buddy (overlay-get note 'mevedel-note-author))
                (mevedel-buddy-dismiss-note note)
              (mevedel-note-delete note)))
        (?k (mevedel-buddy-keep-note note))))))

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
