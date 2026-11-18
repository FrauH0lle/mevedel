;;; mevedel-notes.el -- Workspace notes in source buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Notes mark code with optional text.  They share instruction identities,
;; source edit tracking and the workspace store through instruction owners.
;; Presentation and commands live here; a note never submits a request merely
;; by being created or edited.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'project)
(require 'mevedel-note-source)
(require 'mevedel-instruction-registry)
(require 'mevedel-overlay-ui)
(require 'mevedel-overlays)
(require 'mevedel-utilities)

;; `mevedel-note-conversation'
(autoload 'mevedel-note-send "mevedel-note-conversation" nil t)
(autoload 'mevedel-note-preview-prompt "mevedel-note-conversation" nil t)
(autoload 'mevedel-note-send-batch "mevedel-note-conversation" nil t)
(declare-function mevedel-note-conversation-refresh-hint
                  "mevedel-note-conversation" (&optional note))
(autoload 'mevedel-note-conversation-refresh-hint "mevedel-note-conversation")
(autoload 'mevedel-note-apply-change "mevedel-note-change" nil t)
(autoload 'mevedel-note-revert-change "mevedel-note-change" nil t)
(autoload 'mevedel-note-merge-change "mevedel-note-merge" nil t)

;; `mevedel-note-settings'
(autoload 'mevedel-note-settings "mevedel-note-settings" nil t)

;; `mevedel-session-control-transfer'
(autoload 'mevedel-session-control-transfer-root-buffer-for-id
  "mevedel-session-control-transfer")

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
(autoload 'mevedel-instruction-store-sync "mevedel-persistence")

;; `mevedel-buddy-note'
(autoload 'mevedel-buddy-dismiss-note "mevedel-buddy-note" nil t)
(autoload 'mevedel-buddy-keep-note "mevedel-buddy-note" nil t)

;; `mevedel-buddy'
(autoload 'mevedel-buddy-mode "mevedel-buddy" nil t)

;; `mevedel-view-composer'
(declare-function mevedel-view-call-preserving-window-configuration
                  "mevedel-view-composer" (thunk))

;; `rmc'
(declare-function rmc--add-key-description "rmc" (elem))

(defcustom mevedel-note-labels 'collapsed
  "Initial display of note labels in source buffers.
Individual label toggles last only while the note's overlay is live."
  :type '(choice (const collapsed) (const expanded))
  :group 'mevedel)

(defface mevedel-note-you
  '((t :extend t))
  "Code covered by a note you wrote."
  :group 'mevedel)

(defface mevedel-note-agent
  '((t :extend t))
  "Code covered by a note an agent wrote."
  :group 'mevedel)

(defface mevedel-note-buddy
  '((t :extend t))
  "Code covered by a Buddy remark."
  :group 'mevedel)

(defface mevedel-note-label-you
  '((t :inherit font-lock-keyword-face :weight bold))
  "Label of a note you wrote."
  :group 'mevedel)

(defface mevedel-note-label-agent
  '((t :inherit font-lock-function-name-face :weight bold))
  "Label of a note an agent wrote."
  :group 'mevedel)

(defface mevedel-note-label-buddy
  '((t :inherit font-lock-warning-face :weight bold))
  "Label of a Buddy remark."
  :group 'mevedel)

(defun mevedel-notes--theme-colors (&rest _)
  "Derive note tints from the current theme, preserving user backgrounds."
  (dolist (pair '((mevedel-note-you . mevedel-note-label-you)
                  (mevedel-note-agent . mevedel-note-label-agent)
                  (mevedel-note-buddy . mevedel-note-label-buddy)))
    (let* ((face (car pair))
           (background (face-attribute face :background nil t))
           (specs (list (get face 'customized-face) (get face 'saved-face)
                        (get face 'face-override-spec)
                        (cadr (assq 'user (get face 'theme-face))))))
      (unless (or (cl-some (lambda (spec)
                            (let ((value (plist-get (face-spec-choose spec) :background)))
                              (and value (not (eq value 'unspecified)))))
                          specs)
                  (and (not (memq background '(nil unspecified)))
                       (not (equal background (get face 'mevedel-note-theme-background)))))
        (let ((color (mevedel--tint (face-background 'default nil t)
                                    (or (face-foreground (cdr pair) nil t)
                                        (face-foreground 'default nil t)) 0.10)))
          (set-face-background face color)
          (put face 'mevedel-note-theme-background color))))))

(mevedel-notes--theme-colors)
(add-hook 'enable-theme-functions #'mevedel-notes--theme-colors)
(add-hook 'disable-theme-functions #'mevedel-notes--theme-colors)

(defvar-local mevedel-notes--last-hinted nil
  "Note most recently announced by the local entry hint.")

(defvar mevedel-notes--unsaved-noticed nil
  "Nonproject roots whose in-memory-only note status was announced.")

(defvar mevedel-notes--taken-watches (make-hash-table :test #'equal)
  "Note UUIDs and workspace keys explicitly resumed in this Emacs session.")

(defun mevedel-note-watching-p (note &optional workspace)
  "Return non-nil if taken NOTE is resumed in this Emacs session.
NOTE is an overlay or properties plist.  WORKSPACE defaults to its cached owner."
  (let ((properties (if (overlayp note) (overlay-properties note) note))
        (workspace (or workspace
                       (and (overlayp note) (overlay-buffer note)
                            (buffer-local-value 'mevedel--workspace (overlay-buffer note))))))
    (and (eq 'taken (plist-get properties 'mevedel-note-state))
         (or (gethash (plist-get properties 'mevedel-uuid) mevedel-notes--taken-watches)
             (gethash (mevedel--instruction-workspace-key workspace)
                      mevedel-notes--taken-watches)))))

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
  "Functions deriving attention state from a live overlay or properties plist.
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
            properties))
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

(defun mevedel-notes-open (workspace)
  "Return lightweight open note snapshots in WORKSPACE without visiting files.
Each snapshot holds :file, :line, :properties, :uuid and cached :source-missing.
The :source is a live overlay or a stashed instruction record; treat it as
opaque.  :properties is a fresh plist; stashed entries copy theirs, so changing
a snapshot never changes the canonical note.  Stashed positions refer to the
last observed source; visiting re-anchors it."
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

(defun mevedel-note-resolve (identity workspace)
  "Return the live note with numeric ID or UUID IDENTITY in WORKSPACE.
Restore only the selected note's file, not every stashed source file."
  (when-let* ((entry (cl-find-if
                     (lambda (note)
                       (equal identity (if (integerp identity)
                                           (plist-get (plist-get note :properties) 'mevedel-id)
                                         (plist-get note :uuid))))
                     (mevedel-notes-open workspace))))
    (let ((file (plist-get entry :file))
          (uuid (plist-get entry :uuid))
          (mevedel--instruction-state-key-override
           (mevedel--instruction-workspace-key workspace)))
      (mevedel--restore-file-instructions file nil workspace)
      (when-let* ((buffer (find-buffer-visiting file)))
        (cl-find uuid (cdr (assq buffer (mevedel--instruction-alist)))
                 :key (lambda (note) (overlay-get note 'mevedel-uuid))
                 :test #'equal)))))

(defun mevedel-notes-resolve-entries (entries workspace)
  "Resolve note ENTRIES from `mevedel-notes-open' in WORKSPACE.
Restore each source at most once and return the live notes in ENTRIES' order.
Refuse unavailable sources before a caller starts mutating the notes."
  (let ((mevedel--instruction-state-key-override
         (mevedel--instruction-workspace-key workspace))
        (notes (make-hash-table :test #'equal)))
    (dolist (file (delete-dups (mapcar (lambda (entry) (plist-get entry :file)) entries)))
      (mevedel--restore-file-instructions file nil workspace)
      (when-let* ((buffer (find-buffer-visiting file)))
        (dolist (note (cdr (assq buffer (mevedel--instruction-alist))))
          (when (overlay-buffer note)
            (puthash (overlay-get note 'mevedel-uuid) note notes)))))
    (mapcar (lambda (entry)
              (let ((uuid (plist-get entry :uuid)))
                (or (gethash uuid notes)
                    (user-error "Source unavailable for note %s; no notes changed" uuid))))
            entries)))

(defun mevedel-notes--display-text (note &optional width)
  "Return NOTE's single-line text bounded by WIDTH and 80 columns."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\r\n]+" " "
                             (or (overlay-get note 'mevedel-note-text) ""))
   (min 80 (or width 80)) nil nil "…"))

(defun mevedel-notes--actions-hint (&optional note)
  "Return the current modified-key menu hint for NOTE."
  (format "%s: %s"
          (if-let* ((key (where-is-internal #'mevedel--ov-actions-dispatch
                                           mevedel-note-actions-map t)))
              (key-description key)
            "M-x mevedel-note-menu")
          (format "send, %s, edit, done, %s"
                  (if (and note (mevedel-note-watching-p note)) "drop" "take")
                  (pcase (and note (overlay-get note 'mevedel-note-author))
                    ('buddy "dismiss, keep") ('agent "dismiss") (_ "delete")))))

(defun mevedel-notes--author-name (author)
  "Return AUTHOR's display name without changing its stored symbol."
  (pcase author ('buddy "Buddy") ('agent "AI") (_ "You")))

(defun mevedel-notes--entry-hint ()
  "Announce the note and its next actions when point enters it."
  (unless (or (bound-and-true-p isearch-mode) executing-kbd-macro
              (active-minibuffer-window))
    (let ((notes (sort (mevedel--instructions-at (point) 'note)
                       (lambda (a b) (< (overlay-get a 'mevedel-id)
                                        (overlay-get b 'mevedel-id))))))
      (unless (equal notes mevedel-notes--last-hinted)
        (setq mevedel-notes--last-hinted notes)
        (when notes
          (message "%s · %s"
                   (mapconcat (lambda (note)
                                (format "Note %s · %s  %s"
                                        (overlay-get note 'mevedel-id)
                                        (mevedel-notes--author-name
                                         (overlay-get note 'mevedel-note-author))
                                        (mevedel-notes--display-text note)))
                              notes " | ")
                   (if (cdr notes) "M-m: choose note"
                     (mevedel-notes--actions-hint (car notes)))))))))

(defvar-local mevedel-notes--window-width nil
  "Width last fitted after a window resize.")

(defun mevedel-notes--label-width ()
  "Return the narrowest visible width, since labels are shared by windows."
  (max 4 (if-let* ((windows (get-buffer-window-list nil nil 'visible)))
             (apply #'min (mapcar #'window-body-width windows))
           (window-body-width))))

(defun mevedel-notes--refit-labels (_window)
  "Refit the buffer's existing labels when their available width changes."
  (let ((width (mevedel-notes--label-width)))
    (unless (eql width mevedel-notes--window-width)
      (setq mevedel-notes--window-width width)
      (save-restriction
        (widen)
        (dolist (note (cdr (assq (current-buffer) (mevedel--instruction-alist))))
          (when (and (overlay-buffer note) (overlay-get note 'mevedel-label-overlay))
            (mevedel-notes--render note)))))))

(defun mevedel-notes--label-string (header text bar &optional collapsed)
  "Fill HEADER and TEXT with BAR at indentation, or truncate if COLLAPSED."
  (let* ((window-width (mevedel-notes--label-width))
         (width (if collapsed window-width (max 4 (min fill-column window-width))))
         (indent (min (current-indentation) (- width 3)))
         (prefix (concat (make-string indent ?\s) bar " ")))
    (if collapsed
        (concat "\n" prefix
                (truncate-string-to-width
                 (concat header (unless (string-empty-p text) "  ")
                         (propertize (replace-regexp-in-string "[ \t\r\n]+" " " text)
                                     'face 'default))
                 (min 80 (- width indent 2)) nil nil "…"))
      (with-temp-buffer
      (setq-local fill-column (- width indent 2))
      (insert header (if (string-empty-p text) "" "  ")
              (propertize text 'face 'default))
      (fill-region (point-min) (point-max) nil t)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        ;; Filling leaves unbroken tokens wider than the column intact.
        (move-to-column fill-column)
        (when (or (not (eolp)) (> (current-column) fill-column))
          (when (> (current-column) fill-column)
            (backward-char)
            (when (bolp)
              ;; Escape a control wider than the column so wrapping advances.
              (let ((character (char-after)))
                (delete-char 1)
                (insert (format "\\%03o" character)))
              (beginning-of-line)
              (move-to-column fill-column)))
          (insert "\n")
          (forward-line -1))
        (beginning-of-line)
        (insert prefix)
        (forward-line 1))
        (concat "\n" (buffer-string))))))

(defun mevedel-notes--refresh-label-stack (position)
  "Tint only the leading newline of the first note label at POSITION."
  (save-restriction
    (widen)
    (let* ((labels (cl-remove-if-not
                    (lambda (label)
                      (and (= (overlay-end label) position)
                           (overlay-get label 'mevedel-note-owner)
                           (overlay-get label 'after-string)))
                    (overlays-in (max (point-min) (1- position))
                                 (min (point-max) (1+ position)))))
           (priority (lambda (label)
                       (or (overlay-get (overlay-get label 'mevedel-note-owner)
                                        'priority) 0)))
           (first t)
           previous)
      ;; Redisplay orders after-strings by decreasing integer priority.
      (setq labels
            (sort labels :in-place t
                  :lessp (lambda (a b)
                           (let ((ap (funcall priority a)) (bp (funcall priority b)))
                             (if (= ap bp)
                                 (< (overlay-get (overlay-get a 'mevedel-note-owner) 'mevedel-id)
                                    (overlay-get (overlay-get b 'mevedel-note-owner) 'mevedel-id))
                               (> ap bp))))))
      (dolist (label labels)
        (let* ((note (overlay-get label 'mevedel-note-owner))
               (text (overlay-get label 'after-string))
               (rank (if previous (min (1- previous) (funcall priority label))
                       (funcall priority label)))
               (face (if first (and (eq 'line (overlay-get note 'mevedel-span))
                                    (overlay-get note 'face))
                       'default)))
          (overlay-put label 'priority rank)
          (unless (eq face (get-text-property 0 'face text))
            (if face (put-text-property 0 1 'face face text)
              (remove-list-of-text-properties 0 1 '(face) text))
            (overlay-put label 'after-string nil)
            (overlay-put label 'after-string text))
          (setq first nil previous rank))))))

(defun mevedel-notes--render (note)
  "Render NOTE's label below its code."
  (with-current-buffer (overlay-buffer note)
    (unless (overlay-get note 'mevedel-note-labels)
      (overlay-put note 'mevedel-note-labels mevedel-note-labels))
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
             (old-end (overlay-end label))
             (old-text (overlay-get label 'after-string))
             (author (overlay-get note 'mevedel-note-author))
             (code-face (pcase author
                          ('buddy 'mevedel-note-buddy) ('agent 'mevedel-note-agent)
                          (_ 'mevedel-note-you)))
             (tint (overlay-get note 'mevedel-note-tint-overlay))
             (face (cond ((eq note mevedel--highlighted-instruction) 'highlight)
                         ((eq author 'buddy) 'mevedel-note-label-buddy)
                         ((eq author 'agent) 'mevedel-note-label-agent)
                         (t 'mevedel-note-label-you)))
             (text (mevedel-notes--label-string
                      (concat
                       (propertize
                        (format "Note %s · %s"
                                (overlay-get note 'mevedel-id)
                                (mevedel-notes--author-name author))
                        'face face)
                       (let* ((paused (and (eq 'taken (overlay-get note 'mevedel-note-state))
                                           (not (mevedel-note-watching-p note))))
                              (state (pcase (mevedel-notes-attention-state note)
                                       ('ready "change ready") ('stale "change stale")
                                       ('working "working…")
                                       ('taken
                                        (unless paused
                                          (if (overlay-get note 'mevedel-note-buddy-looks-done)
                                              "looks done"
                                            "Buddy watches"))))))
                         (when (or state paused)
                           (propertize (concat (and state (concat " · " state))
                                               (and paused " · paused"))
                                       'face 'shadow))))
                      (or (overlay-get note 'mevedel-note-text) "")
                      (propertize (if (eq author 'buddy) "┆" "▍") 'face face)
                      (eq 'collapsed (overlay-get note 'mevedel-note-labels)))))
        (overlay-put note 'keymap mevedel-note-actions-map)
        (unless (eq code-face (overlay-get note 'face))
          (overlay-put note 'face code-face))
        ;; The label tracks edits; this sibling only gives the newline its face.
        (if (and (eq 'line (overlay-get note 'mevedel-span))
                 (or text (= end (overlay-end note)))
                 (eq (char-after end) ?\n))
            (progn
              (unless tint
                (setq tint (make-overlay end (1+ end) nil t nil))
                (overlay-put note 'mevedel-note-tint-overlay tint))
              (move-overlay tint end (1+ end))
              (overlay-put tint 'priority (1+ (or (overlay-get note 'priority) 0)))
              (let ((tint-face (if text 'default code-face)))
                (unless (eq tint-face (overlay-get tint 'face))
                  (overlay-put tint 'face tint-face))))
          (when tint
            (delete-overlay tint)
            (overlay-put note 'mevedel-note-tint-overlay nil)))
        (move-overlay label (overlay-start note) end)
        (when-let* ((leading-face (and old-text (get-text-property 0 'face old-text))))
          (put-text-property 0 1 'face leading-face text))
        (unless (equal-including-properties text old-text)
          (overlay-put label 'after-string text))
        (when (and old-end (/= end old-end))
          (mevedel-notes--refresh-label-stack old-end))
        (mevedel-notes--refresh-label-stack end)))
    (add-hook 'after-change-functions #'mevedel-notes--after-change nil t)
    (add-hook 'post-command-hook #'mevedel-notes--entry-hint nil t)
    (add-hook 'window-size-change-functions #'mevedel-notes--refit-labels nil t)))

;;;###autoload
(defun mevedel-notes-toggle-labels ()
  "Expand every note label in this buffer, or collapse if all are expanded."
  (interactive)
  (let* ((notes (cl-remove-if-not
                 (lambda (note)
                   (and (overlay-buffer note)
                        (eq 'note (overlay-get note 'mevedel-instruction-type))))
                 (cdr (assq (current-buffer) (mevedel--instruction-alist)))))
         (display (if (cl-every (lambda (note)
                                 (eq 'expanded (overlay-get note 'mevedel-note-labels)))
                               notes)
                      'collapsed 'expanded)))
    (save-restriction
      (widen)
      (dolist (note notes)
        (overlay-put note 'mevedel-note-labels display)
        (mevedel-notes--render note)))))

(defun mevedel-notes--after-change (beg end old-length)
  "Reposition labels touched by BEG..END after replacing OLD-LENGTH characters."
  (dolist (overlay (overlays-in (max (point-min) (1- beg))
                               (min (point-max) (1+ end))))
    (when-let* ((note (overlay-get overlay 'mevedel-note-owner)))
      (if (overlay-buffer note)
          (progn
            (overlay-put note 'mevedel-note-buddy-looks-done nil)
            (if (and (overlay-get note 'mevedel-note-prepared-change)
                     (if (zerop old-length)
                         (and (< beg (overlay-end note))
                              (> end (overlay-start note)))
                       (and (<= beg (overlay-end note))
                            (>= end (overlay-start note)))))
                (mevedel-note-mark-stale note)
              (mevedel-notes--render note)))
        (mevedel--delete-instruction-overlay note)))))

(defun mevedel-notes--at-point ()
  "Return the note at point, choosing when notes overlap; refuse if absent."
  (let ((notes (mevedel--instructions-at (point) 'note)))
    (unless notes (user-error "No note at point"))
    (if (not (cdr notes)) (car notes)
      (let ((choices (mapcar
                      (lambda (note)
                        (cons (format "Note %s · %s  %s"
                                      (overlay-get note 'mevedel-id)
                                      (mevedel-notes--author-name
                                       (overlay-get note 'mevedel-note-author))
                                      (mevedel-notes--display-text note)) note))
                      (sort notes (lambda (a b) (< (overlay-get a 'mevedel-id)
                                                  (overlay-get b 'mevedel-id)))))))
        (cdr (assoc (completing-read "Choose note: " (mapcar #'car choices) nil t)
                    choices))))))

(defun mevedel-notes--read-tags (note)
  "Read the complete tag set for NOTE."
  (let ((tags (overlay-get note 'mevedel-note-tags)))
    (mapcar #'intern
            (completing-read-multiple
             "Tags (empty removes all): " (mapcar #'symbol-name tags)
             nil nil (mapconcat #'symbol-name tags ",")))))

(defun mevedel-notes--validate-tags (tags)
  "Validate queryable TAGS and return the unchanged list of symbols."
  (unless (and (proper-list-p tags) (cl-every #'symbolp tags))
    (user-error "Tags must be a list of symbols"))
  (dolist (tag tags)
    (unless (and tag (not (memq tag '(and or not)))
                 (condition-case nil
                     (eq tag (read (symbol-name tag)))
                   (error nil)))
      (user-error "Tag %S cannot be used in a note query" tag)))
  tags)

;;;###autoload
(defun mevedel-note-set-tags (tags &optional note)
  "Replace NOTE's tags with TAGS, a list of symbols.
Default to the note at point.  An empty list removes every tag."
  (interactive
   (let ((note (mevedel-notes--at-point)))
     (list (mevedel-notes--read-tags note) note)))
  (setq note (or note (mevedel-notes--at-point)))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (mevedel-notes--validate-tags tags)
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
        (mevedel-note-keep note))
      (overlay-put note 'mevedel-note-state 'taken)
      (overlay-put note 'mevedel-note-buddy-looks-done nil)
      (overlay-put note 'mevedel-note-buddy-reviewed-hash nil)
      (puthash (overlay-get note 'mevedel-uuid) t mevedel-notes--taken-watches)
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


;;
;;; Lifecycle transitions

;; Each verb owns the properties that change together and only the render,
;; conversation-hint and store-touch effects its callers need.  None flushes:
;; a caller that needs durability flushes after the verb.

(defun mevedel-notes--workspace (notes)
  "Return the workspace owning the first live note in NOTES."
  (mevedel--instruction-buffer-workspace (overlay-buffer (car notes))))

(defun mevedel-note-mark-sent (notes session-id)
  "Mark live NOTES sent in conversation SESSION-ID, then touch the store once.
A note keeps its existing session id."
  (dolist (note notes)
    (unless (overlay-get note 'mevedel-note-session-id)
      (overlay-put note 'mevedel-note-session-id session-id))
    (overlay-put note 'mevedel-note-state 'sent)
    (mevedel-notes--render note)
    (mevedel-note-conversation-refresh-hint note))
  (when notes (mevedel-instruction-store-touch (mevedel-notes--workspace notes))))

(defun mevedel-note-hold-change (notes record)
  "Hold prepared change RECORD on live NOTES as ready, then touch the store once.
The caller validates RECORD and computes its :note-extents."
  (dolist (note notes)
    (overlay-put note 'mevedel-note-prepared-change record)
    (overlay-put note 'mevedel-note-attention-state 'ready)
    (mevedel-notes--render note)
    ;; An unlinked suggestion has no conversation; without an id the hint
    ;; would fall back to the current one and advertise changes it cannot apply.
    (when (overlay-get note 'mevedel-note-session-id)
      (mevedel-note-conversation-refresh-hint note)))
  (when notes (mevedel-instruction-store-touch (mevedel-notes--workspace notes))))

(defun mevedel-note-record-applied (record files workspace)
  "Record applied change RECORD on every open note in WORKSPACE that holds it.
Each such note gains the applied pair, with FILES only when RECORD belongs to
another session's suggestion, and loses its prepared change and attention.
Touch the store once."
  (dolist (entry (mevedel-notes-open workspace))
    (let ((props (plist-get entry :properties)))
      (when (equal (plist-get record :id)
                   (plist-get (plist-get props 'mevedel-note-prepared-change) :id))
        (let* ((source (plist-get entry :source))
               (id (plist-get record :session-id))
               (pair (append (list :session-id id :turn (plist-get record :turn))
                             (unless (equal id (plist-get props 'mevedel-note-session-id))
                               (list :files files))))
               (pairs (cl-adjoin pair (plist-get props 'mevedel-note-applied-changes)
                                 :test #'equal)))
          (if (overlayp source)
              (progn
                (overlay-put source 'mevedel-note-applied-changes pairs)
                (overlay-put source 'mevedel-note-prepared-change nil)
                (overlay-put source 'mevedel-note-attention-state nil)
                (mevedel-notes--render source)
                (mevedel-note-conversation-refresh-hint source))
            (let ((stored (plist-get source :properties)))
              (plist-put stored 'mevedel-note-applied-changes pairs)
              (plist-put stored 'mevedel-note-prepared-change nil)
              (plist-put stored 'mevedel-note-attention-state nil)))))))
  (mevedel-instruction-store-touch workspace))

(defun mevedel-note-mark-stale (note)
  "Mark NOTE's prepared change stale and render it."
  (overlay-put note 'mevedel-note-attention-state 'stale)
  (mevedel-notes--render note))

(defun mevedel-note-set-settings (note settings)
  "Merge the plist SETTINGS into NOTE's conversation settings and touch the store."
  (let ((merged (copy-sequence (overlay-get note 'mevedel-note-settings))))
    (while settings
      (setq merged (plist-put merged (pop settings) (pop settings))))
    (overlay-put note 'mevedel-note-settings merged)
    (mevedel-instruction-store-touch (mevedel-notes--workspace (list note)))))

(defun mevedel-note-keep (note)
  "Keep Buddy's NOTE as a user note, touching the store for file-backed buffers."
  (unless (eq 'buddy (overlay-get note 'mevedel-note-author))
    (user-error "Not a Buddy remark"))
  (overlay-put note 'mevedel-note-author 'user)
  (overlay-put note 'mevedel-note-kept-from-buddy t)
  (mevedel--update-instruction-overlay note t)
  (when (buffer-local-value 'buffer-file-name (overlay-buffer note))
    (mevedel-instruction-store-touch (mevedel-notes--workspace (list note))))
  note)

(defun mevedel-notes-rewind-instruction (instruction session-id surviving-turn)
  "Rewind INSTRUCTION's note activity owned by SESSION-ID past SURVIVING-TURN.
INSTRUCTION is a persisted record with :properties.  Mutate only lifecycle
properties; preserve its text, anchor and session link."
  (let* ((props (plist-get instruction :properties))
         (change (plist-get props 'mevedel-note-prepared-change)))
    (when (equal session-id (plist-get props 'mevedel-note-session-id))
      (when (zerop surviving-turn)
        (when (eq 'sent (plist-get props 'mevedel-note-state))
          (plist-put props 'mevedel-note-state 'unsent))
        (when (eq 'sent (plist-get instruction :open-state))
          (plist-put instruction :open-state 'unsent))))
    (when (and (equal session-id (plist-get change :session-id))
               (> (or (plist-get change :turn) 0) surviving-turn))
      (plist-put props 'mevedel-note-prepared-change nil)
      (plist-put props 'mevedel-note-attention-state nil))
    (when-let* ((pairs (plist-get props 'mevedel-note-applied-changes)))
      (plist-put props 'mevedel-note-applied-changes
                 (cl-remove-if (lambda (pair)
                                 (and (equal session-id (plist-get pair :session-id))
                                      (> (plist-get pair :turn) surviving-turn))) pairs))))
  instruction)

(defun mevedel-notes-rewind-live (workspace session-id turn)
  "Apply committed SESSION-ID Rewind to TURN to WORKSPACE's current notes.
Keep memory in step with the new merge base, including crash recovery."
  (let ((state (mevedel--instruction-state (mevedel--instruction-workspace-key workspace))))
    (dolist (file (plist-get state :instructions))
      (if (bufferp (car file))
          (dolist (note (cdr file))
            (let* ((before (overlay-properties note))
                   (instruction (list :properties (copy-sequence before)))
                   (after (plist-get (mevedel-notes-rewind-instruction
                                      instruction session-id turn) :properties)))
              (unless (equal before after)
                (dolist (key '(mevedel-note-state mevedel-note-prepared-change
                              mevedel-note-applied-changes mevedel-note-attention-state))
                  (overlay-put note key (plist-get after key)))
                (mevedel--update-instruction-overlay note t)
                (mevedel-note-conversation-refresh-hint note))))
        (dolist (instruction (plist-get (cdr file) :instructions))
          (mevedel-notes-rewind-instruction instruction session-id turn))))
    (dolist (closed (plist-get state :closed-notes))
      (mevedel-notes-rewind-instruction (plist-get closed :instruction) session-id turn))))

(defun mevedel-notes--restore-taken (buffer)
  "Label restored takes in BUFFER; watch only explicitly resumed notes."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (dolist (note (cdr (assq buffer (mevedel--instruction-alist))))
        (when (eq 'taken (overlay-get note 'mevedel-note-state))
          (mevedel-notes--render note)
          (when (mevedel-note-watching-p note)
            (unless (bound-and-true-p mevedel-buddy-mode) (mevedel-buddy-mode 1))))))))

(add-hook 'mevedel-instructions-restored-hook #'mevedel-notes--restore-taken)

(defun mevedel-notes-resume-taken (workspace)
  "Resume WORKSPACE's taken notes for the rest of this Emacs session."
  (puthash (mevedel--instruction-workspace-key workspace) t mevedel-notes--taken-watches)
  (dolist (entry (plist-get (mevedel--instruction-state
                           (mevedel--instruction-workspace-key workspace)) :instructions))
    (when (buffer-live-p (car entry))
      (mevedel-notes--restore-taken (car entry)))))

;;;###autoload
(defun mevedel-note-delete (&optional note)
  "Move NOTE to recently closed as deleted, without changing code.
NOTE defaults to the note at point.  Restore it with `mevedel-note-restore'."
  (interactive)
  (mevedel-note-close (or note (mevedel-notes--at-point)) 'deleted))

;;;###autoload
(defun mevedel-note-dismiss (&optional note)
  "Dismiss agent-authored NOTE, defaulting to the note at point."
  (interactive)
  (setq note (or note (mevedel-notes--at-point)))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless (eq 'agent (overlay-get note 'mevedel-note-author))
        (user-error "Not an agent-authored note"))
      (mevedel-note-close note 'dismissed))))

;;;###autoload
(defun mevedel-note-dismiss-agent-notes (&optional workspace)
  "Dismiss all agent-authored notes in WORKSPACE, syncing once.
Return the number dismissed.  User and Buddy notes are left alone."
  (interactive)
  (setq workspace (or workspace (mevedel--instruction-buffer-workspace (current-buffer))))
  (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
    (mevedel-instruction-store-observe workspace)
    (let ((count 0)
          (notes (mevedel-notes-resolve-entries
                  (cl-remove-if-not
                   (lambda (entry)
                     (eq 'agent (plist-get (plist-get entry :properties) 'mevedel-note-author)))
                   (mevedel-notes-open workspace))
                  workspace)))
      (unwind-protect
          (dolist (note notes)
            (mevedel-note-dismiss note)
            (cl-incf count))
        (when (> count 0) (mevedel-instruction-store-sync workspace)))
      count)))

(defun mevedel-notes--store-closed (file instruction reason)
  "Record FILE's serialized INSTRUCTION as closed with REASON."
  (let ((properties (plist-get instruction :properties)))
    (plist-put instruction :open-state (plist-get properties 'mevedel-note-state))
    (plist-put instruction :closed-reason reason)
    (plist-put instruction :closed-at (float-time))
    (plist-put properties 'mevedel-note-state 'closed)
    (push (list :file file :instruction instruction)
          (plist-get (mevedel--instruction-state (mevedel--instruction-operation-state-key))
                     :closed-notes))))

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
                      :properties properties)))
          (mevedel-notes--store-closed buffer-file-name instruction reason)
          ;; Explicit closure wins over older source geometry undo.  Reserve the
          ;; display number until expiry, rather than retiring it on close.
          (overlay-put note 'mevedel-marked-for-deletion t)
          (mevedel--delete-instruction-overlay note)
          (setf (alist-get buffer (mevedel--instruction-alist))
                (delq note (alist-get buffer (mevedel--instruction-alist))))
          (mevedel-instruction-store-prune-closed workspace)
          (mevedel-instruction-store-touch workspace))))))

(defun mevedel-notes-close-applied (note session-id workspace)
  "Close NOTE as done, including notes it shares with SESSION-ID in WORKSPACE."
  (let ((mevedel--instruction-state-key-override
         (mevedel--instruction-workspace-key workspace))
        (uuid (overlay-get note 'mevedel-uuid))
        (shared (equal session-id (overlay-get note 'mevedel-note-session-id))))
    (dolist (entry (mevedel-notes-open workspace))
      (when (or (equal uuid (plist-get entry :uuid))
                (and shared (equal session-id
                                   (plist-get (plist-get entry :properties) 'mevedel-note-session-id))))
        (let ((source (plist-get entry :source)))
          (if (overlayp source)
              (mevedel-note-close source 'done)
            (let* ((file (plist-get entry :file))
                   (stash (assoc file (mevedel--instruction-alist))))
              (mevedel-notes--store-closed file source 'done)
              (setf (plist-get (cdr stash) :instructions)
                    (delq source (plist-get (cdr stash) :instructions)))
              (unless (plist-get (cdr stash) :instructions)
                (setf (mevedel--instruction-alist) (delq stash (mevedel--instruction-alist)))))))))
    (mevedel-instruction-store-prune-closed workspace)
    (mevedel-instruction-store-touch workspace)))

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
  "Resolve closed INSTRUCTION, falling back to its line or absolute positions."
  (or (mevedel-persistence-resolve-instruction-anchor
       (plist-get instruction :overlay-start)
       (plist-get instruction :overlay-end)
       (plist-get instruction :anchor) nil)
      (when-let* ((position (plist-get instruction :closed-position)))
        (pcase-let ((`(,line ,column ,end-line ,end-column) position))
          (goto-char (point-min))
          (forward-line (1- line))
          (let ((start (min (line-end-position) (+ (point) column))))
            (goto-char (point-min))
            (forward-line (1- end-line))
            (cons start (max start (min (line-end-position)
                                       (+ (point) end-column)))))))
      (let ((start (max (point-min) (min (point-max) (plist-get instruction :overlay-start)))))
        (cons start (max start (min (point-max) (plist-get instruction :overlay-end)))))))

;;;###autoload
(defun mevedel-note-menu (&optional note)
  "Show the actions menu for NOTE, defaulting to the note at point."
  (interactive)
  (require 'rmc)
  (let ((workspace (mevedel--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (setq note (or note (mevedel-notes--at-point)))
      (let* ((choices
              (append '((?s "send" "Send or reopen the note conversation")
                        (?p "preview" "Preview the exact note prompt without sending")
                        (?i "edit" "Edit note text")
                        (?w "copy text" "Copy full note text to the kill ring")
                        (?x "done" "Mark note done"))
                      (if (eq 'expanded (overlay-get note 'mevedel-note-labels))
                          '((?e "collapse" "Show a one-line note label"))
                        '((?e "expand" "Show the full note label")))
                      (if (mevedel-note-watching-p note)
                          '((?t "drop" "Stop watching this note"))
                        '((?t "take" "Implement yourself with Buddy watching")))
                      (if (eq 'buddy (overlay-get note 'mevedel-note-author))
                          '((?k "dismiss" "Dismiss this remark")
                            (?b "keep" "Keep as your own note"))
                        (if (eq 'agent (overlay-get note 'mevedel-note-author))
                            '((?k "dismiss" "Dismiss this agent finding"))
                          '((?k "delete" "Delete note without changing code"))))
                      (when (or (overlay-get note 'mevedel-note-applied-changes)
                                (when-let* ((id (overlay-get note 'mevedel-note-session-id))
                                            (data (mevedel-session-control-transfer-root-buffer-for-id id))
                                            (session (buffer-local-value 'mevedel--session data)))
                                  (cl-some #'cdr (mevedel-session-file-snapshots session))))
                        '((?r "revert" "Revert an applied change, preserving later edits")))
                      (when (overlay-get note 'mevedel-note-prepared-change)
                        '((?a "apply" "Apply prepared change")
                          (?m "merge" "Resolve in a three-way view")))
                      '((?g "tags" "Edit note tags")
                        (?S "settings" "Configure this note conversation"))))
             (label (overlay-get note 'mevedel-label-overlay))
             (original (overlay-get label 'after-string))
             (menu (concat original "\n  "
                           (propertize "Actions: " 'face 'shadow)
                           (mapconcat (lambda (entry)
                                        (cdr (rmc--add-key-description entry)))
                                      choices ", ")))
             choice)
        (unwind-protect
            (progn
              (overlay-put label 'after-string menu)
              (let ((key (read-key)))
                (setq choice (and (assq key choices) key))))
          (when (eq menu (overlay-get label 'after-string))
            (overlay-put label 'after-string original)
            (when (overlay-buffer label)
              (with-current-buffer (overlay-buffer label)
                (mevedel-notes--refresh-label-stack (overlay-end label))))))
        (pcase choice
          (?s (mevedel-note-send note))
          (?p (mevedel-note-preview-prompt note))
          (?a (mevedel-note-apply-change note))
          (?r (mevedel-note-revert-change note))
          (?m (mevedel-note-merge-change note))
          (?t (if (mevedel-note-watching-p note)
                  (mevedel-note-drop note)
                (mevedel-note-take note)))
          (?e (overlay-put note 'mevedel-note-labels
                           (if (eq 'expanded (overlay-get note 'mevedel-note-labels))
                               'collapsed 'expanded))
              (mevedel-notes--render note))
          (?i (mevedel-note-edit
               (read-string "Note: " (overlay-get note 'mevedel-note-text)) note))
          (?w (kill-new (or (overlay-get note 'mevedel-note-text) ""))
              (message "mevedel: note text copied"))
          (?x (mevedel-note-done note))
          (?g (mevedel-note-set-tags (mevedel-notes--read-tags note) note))
          (?S (mevedel-note-settings note))
          (?k (pcase (overlay-get note 'mevedel-note-author)
                ('buddy (mevedel-buddy-dismiss-note note))
                ('agent (mevedel-note-dismiss note))
                (_ (mevedel-note-delete note))))
          (?b (mevedel-buddy-keep-note note)))))))

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

(cl-defun mevedel-note-make (buffer beg end &key text author span)
  "Create and render a note in BUFFER over BEG..END, and return its overlay.
AUTHOR is `user' (default), `agent' or `buddy'; SPAN is `line' or nil for the
exact range.  The note starts unsent with TEXT (default empty).  Work in
BUFFER without a file, never read or change the region, point or selected
window, and neither write nor touch the store: callers prepare the workspace
and decide durability."
  (let ((note (mevedel--create-instruction-overlay-in buffer beg end span)))
    (overlay-put note 'mevedel-instruction-type 'note)
    (overlay-put note 'mevedel-note-text (or text ""))
    (overlay-put note 'mevedel-note-author (or author 'user))
    (overlay-put note 'mevedel-note-state 'unsent)
    (mevedel--update-instruction-overlay note t)
    note))

;;;###autoload
(cl-defun mevedel-note-create (&optional text &key defer-write)
  "Create a note with optional TEXT on the region or current line.
An empty string creates a code-only note.  Return the note overlay.
DEFER-WRITE leaves durability to the caller's batch store sync."
  (interactive
   (list
    (if (featurep 'mevedel-view-composer)
        (mevedel-view-call-preserving-window-configuration
         (lambda () (read-string "Note (optional): ")))
      (read-string "Note (optional): "))))
  (let ((workspace (mevedel--instruction-buffer-workspace (current-buffer))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (unless buffer-file-name
        (user-error "Notes need a source file"))
      (setq workspace (mevedel-notes--ensure-workspace workspace))
      (let* ((region (use-region-p))
             (note (mevedel-note-make
                    (current-buffer)
                    (if region (region-beginning) (line-beginning-position))
                    (if region (region-end) (line-end-position))
                    :text text :span (unless region 'line))))
        (if defer-write
            (mevedel-instruction-store-touch workspace)
          (mevedel--instruction-store-write (current-buffer)))
        (deactivate-mark)
        note))))

(provide 'mevedel-notes)
;;; mevedel-notes.el ends here
