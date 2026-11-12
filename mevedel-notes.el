;;; mevedel-notes.el -- Workspace notes in source buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Notes mark code with optional text.  They share instruction identities,
;; source edit tracking and the workspace store with directives.
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

;; `mevedel-note-conversation'
(autoload 'mevedel-note-send "mevedel-note-conversation" nil t)
(autoload 'mevedel-note-send-batch "mevedel-note-conversation" nil t)
(autoload 'mevedel-note-reask "mevedel-note-conversation" nil t)
(autoload 'mevedel-note-apply-change "mevedel-note-change" nil t)
(autoload 'mevedel-note-merge-change "mevedel-note-merge" nil t)

;; `mevedel-directive-activity'
(declare-function mevedel-open-directive-activity "mevedel-directive-activity"
                  (&optional directive workspace frozen))
(autoload 'mevedel-open-directive-activity "mevedel-directive-activity")

;; `mevedel-directive-persistence'
(declare-function mevedel--deserialize-directives "mevedel-directive-persistence"
                  (serialized base-directory))

;; `mevedel-persistence'
(defvar mevedel-instructions-restored-hook)
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

;; `mevedel-buddy'
(autoload 'mevedel-buddy-mode "mevedel-buddy" nil t)

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
Each snapshot holds :file, :line, :properties, :uuid and cached :source-missing.
The internal :source is a live overlay or a stashed instruction record.
Stashed positions refer to the last observed source; visiting re-anchors it."
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
                                :source note
                                :source-missing mevedel--buffer-source-missing
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
                          :source note
                          :source-missing (plist-get (cdr entry) :source-missing)
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

(defun mevedel-notes--actions-hint (&optional note)
  "Return the current modified-key menu hint for NOTE."
  (format "%s: send, %s, edit, done, %s"
          (if-let* ((key (where-is-internal #'mevedel--ov-actions-dispatch
                                           mevedel-note-actions-map t)))
              (key-description key)
            "M-x mevedel-note-menu")
          (if (and note (eq 'taken (overlay-get note 'mevedel-note-state))) "drop" "take")
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
             (text (format "\n  Note %s · %s · %s · %s · %s"
                           (overlay-get note 'mevedel-id)
                           (pcase (overlay-get note 'mevedel-note-author)
                             ('buddy "Buddy") ('agent "AI") (_ "you"))
                           (mevedel-notes--display-text note)
                           (pcase (mevedel-notes-attention-state note)
                             ('ready "change ready") ('stale "change stale")
                             (_ (if (eq 'taken (overlay-get note 'mevedel-note-state))
                                    (if (overlay-get note 'mevedel-note-buddy-looks-done)
                                        "looks done · done? · Buddy watches"
                                      "you're on it · Buddy watches")
                                  (if (overlay-get note 'mevedel-note-session-id) "sent" "not sent"))))
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
          (progn
            (when (and (overlay-get note 'mevedel-note-prepared-change)
                       (<= beg (overlay-end note))
                       (>= end (overlay-start note)))
              (overlay-put note 'mevedel-note-attention-state 'stale))
            (overlay-put note 'mevedel-note-buddy-looks-done nil)
            (mevedel-notes--render note))
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
      (overlay-put note 'mevedel-note-buddy-looks-done nil)
      (mevedel-notes--render note)
      (mevedel-instruction-store-touch workspace))))

;;;###autoload
(defun mevedel-note-take (&optional note)
  "Take NOTE to implement it yourself, with Buddy watching its code.
Default to the note at point.  Taking Buddy's remark first keeps it as yours."
  (interactive)
  (setq note (or note (mevedel-notes--at-point)))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (when (eq 'buddy (overlay-get note 'mevedel-note-author))
        (mevedel-buddy-keep-note note))
      (overlay-put note 'mevedel-note-state 'taken)
      (overlay-put note 'mevedel-note-buddy-looks-done nil)
      (with-current-buffer (overlay-buffer note) (mevedel-buddy-mode 1))
      (mevedel-notes--render note)
      (mevedel-instruction-store-touch workspace)))
  note)

;;;###autoload
(defun mevedel-note-drop (&optional note)
  "Stop watching taken NOTE and return it to unsent, keeping its conversation."
  (interactive)
  (setq note (or note (mevedel-notes--at-point)))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless (eq 'taken (overlay-get note 'mevedel-note-state))
        (user-error "Note is not taken"))
      (overlay-put note 'mevedel-note-state 'unsent)
      (overlay-put note 'mevedel-note-buddy-looks-done nil)
      (mevedel-notes--render note)
      (mevedel-instruction-store-touch workspace)))
  note)

(defun mevedel-notes--activate-taken (buffer)
  "Resume watching taken notes restored in BUFFER, without loading its dial."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (when (seq-some (lambda (note)
                        (and (eq 'note (overlay-get note 'mevedel-instruction-type))
                             (eq 'taken (overlay-get note 'mevedel-note-state))))
                      (overlays-in (point-min) (point-max)))
        (mevedel-buddy-mode 1)))))

(add-hook 'mevedel-instructions-restored-hook #'mevedel-notes--activate-taken)

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
(defun mevedel-note-history (&optional note)
  "Open NOTE's frozen pre-migration history in the directive inspector."
  (interactive)
  (setq note (or note (mevedel-notes--at-point)))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless (overlay-get note 'mevedel-note-legacy-history)
        (user-error "Note has no pre-migration history"))
      (require 'mevedel-directive-persistence)
      (mevedel-open-directive-activity
       (car (mevedel--deserialize-directives
             (list (copy-tree (overlay-get note 'mevedel-note-legacy-history)))
             (mevedel-workspace-root workspace)))
       workspace t))))

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
                   (append '((?s "send" "Send or reopen the note conversation")
                             (?e "edit" "Edit note text")
                             (?x "done" "Mark note done"))
                           (if (eq 'taken (overlay-get note 'mevedel-note-state))
                               '((?t "drop" "Stop watching this note"))
                             '((?t "take" "Implement yourself with Buddy watching")))
                           (if (eq 'buddy (overlay-get note 'mevedel-note-author))
                               '((?d "dismiss" "Dismiss this remark")
                                 (?k "keep" "Keep as your own note"))
                             '((?d "delete" "Delete note without changing code")))
                           (when (overlay-get note 'mevedel-note-legacy-history)
                             '((?h "history" "Inspect read-only pre-migration history")))
                           (when (overlay-get note 'mevedel-note-prepared-change)
                             '((?a "apply" "Apply prepared change")
                               (?r "re-ask" "Ask again on current code")
                               (?m "merge" "Resolve in a three-way view"))))))
        (?s (mevedel-note-send note))
        (?a (mevedel-note-apply-change note))
        (?r (mevedel-note-reask note))
        (?m (mevedel-note-merge-change note))
        (?t (if (eq 'taken (overlay-get note 'mevedel-note-state))
                (mevedel-note-drop note)
              (mevedel-note-take note)))
        (?e (mevedel-note-edit
             (read-string "Note: " (overlay-get note 'mevedel-note-text)) note))
        (?x (mevedel-note-done note))
        (?d (if (eq 'buddy (overlay-get note 'mevedel-note-author))
                (mevedel-buddy-dismiss-note note)
              (mevedel-note-delete note)))
        (?k (mevedel-buddy-keep-note note))
        (?h (mevedel-note-history note))))))

(defun mevedel-notes--ensure-workspace (workspace)
  "Prepare the current file's WORKSPACE for note storage, without syncing.
Create `.mevedel/' only in a recognized project.  Return the actual workspace."
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
  workspace)

;;;###autoload
(defun mevedel-note-create (&optional text)
  "Create a note with optional TEXT on the region or current line.
An empty string creates a code-only note.  Return the note overlay."
  (interactive (list (read-string "Note (optional): ")))
  (let ((workspace (mevedel--instruction-buffer-workspace (current-buffer))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless buffer-file-name
        (user-error "Notes need a source file"))
      (setq workspace (mevedel-notes--ensure-workspace workspace))
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
