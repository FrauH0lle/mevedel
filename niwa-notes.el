;;; niwa-notes.el -- Workspace notes in source buffers -*- lexical-binding: t -*-

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
(require 'niwa-note-source)
(require 'niwa-instruction-registry)
(require 'niwa-overlay-ui)
(require 'niwa-overlays)
(require 'niwa-utilities)

;; `niwa-note-conversation'
(autoload 'niwa-note-send "niwa-note-conversation" nil t)
(autoload 'niwa-note-preview-prompt "niwa-note-conversation" nil t)
(autoload 'niwa-note-send-batch "niwa-note-conversation" nil t)
(declare-function niwa-note-conversation-refresh-hint
                  "niwa-note-conversation" (&optional note))
(autoload 'niwa-note-conversation-refresh-hint "niwa-note-conversation")
(autoload 'niwa-note-apply-change "niwa-note-change" nil t)
(autoload 'niwa-note-revert-change "niwa-note-change" nil t)
(autoload 'niwa-note-merge-change "niwa-note-merge" nil t)

;; `niwa-note-settings'
(autoload 'niwa-note-settings "niwa-note-settings" nil t)

;; `niwa-session-control-transfer'
(autoload 'niwa-session-control-transfer-root-buffer-for-id
  "niwa-session-control-transfer")

;; `niwa-persistence'
(defvar niwa-instructions-restored-hook)
(declare-function niwa-instruction-store-observe "niwa-persistence"
                  (workspace))
(declare-function niwa-instruction-store-prune-closed "niwa-persistence"
                  (workspace))
(declare-function niwa-instruction-store-touch "niwa-persistence"
                  (&optional workspace))
(autoload 'niwa-instruction-store-observe "niwa-persistence")
(autoload 'niwa-instruction-store-prune-closed "niwa-persistence")
(autoload 'niwa-instruction-store-touch "niwa-persistence")
(autoload 'niwa-instruction-store-sync "niwa-persistence")

;; `niwa-buddy-note'
(autoload 'niwa-buddy-dismiss-note "niwa-buddy-note" nil t)
(autoload 'niwa-buddy-keep-note "niwa-buddy-note" nil t)

;; `niwa-buddy'
(autoload 'niwa-buddy-mode "niwa-buddy" nil t)

;; `niwa-view-composer'
(declare-function niwa-view-call-preserving-window-configuration
                  "niwa-view-composer" (thunk))

;; `rmc'
(declare-function rmc--add-key-description "rmc" (elem))

(defcustom niwa-note-labels 'collapsed
  "Initial display of note labels in source buffers.
Individual label toggles last only while the note's overlay is live."
  :type '(choice (const collapsed) (const expanded))
  :group 'niwa)

(defface niwa-note-you
  '((t :extend t))
  "Code covered by a note you wrote."
  :group 'niwa)

(defface niwa-note-agent
  '((t :extend t))
  "Code covered by a note an agent wrote."
  :group 'niwa)

(defface niwa-note-buddy
  '((t :extend t))
  "Code covered by a Buddy remark."
  :group 'niwa)

(defface niwa-note-label-you
  '((t :inherit font-lock-keyword-face :weight bold))
  "Label of a note you wrote."
  :group 'niwa)

(defface niwa-note-label-agent
  '((t :inherit font-lock-function-name-face :weight bold))
  "Label of a note an agent wrote."
  :group 'niwa)

(defface niwa-note-label-buddy
  '((t :inherit font-lock-warning-face :weight bold))
  "Label of a Buddy remark."
  :group 'niwa)

(defun niwa-notes--theme-colors (&rest _)
  "Derive note tints from the current theme, preserving user backgrounds."
  (dolist (pair '((niwa-note-you . niwa-note-label-you)
                  (niwa-note-agent . niwa-note-label-agent)
                  (niwa-note-buddy . niwa-note-label-buddy)))
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
                       (not (equal background (get face 'niwa-note-theme-background)))))
        (let ((color (niwa--tint (face-background 'default nil t)
                                    (or (face-foreground (cdr pair) nil t)
                                        (face-foreground 'default nil t)) 0.10)))
          (set-face-background face color)
          (put face 'niwa-note-theme-background color))))))

(niwa-notes--theme-colors)
(add-hook 'enable-theme-functions #'niwa-notes--theme-colors)
(add-hook 'disable-theme-functions #'niwa-notes--theme-colors)

(defvar-local niwa-notes--last-hinted nil
  "Note most recently announced by the local entry hint.")

(defvar niwa-notes--unsaved-noticed nil
  "Nonproject roots whose in-memory-only note status was announced.")

(defvar niwa-notes--taken-watches (make-hash-table :test #'equal)
  "Note UUIDs and workspace keys explicitly resumed in this Emacs session.")

(defun niwa-note-watching-p (note &optional workspace)
  "Return non-nil if taken NOTE is resumed in this Emacs session.
NOTE is an overlay or properties plist.  WORKSPACE defaults to its cached owner."
  (let ((properties (if (overlayp note) (overlay-properties note) note))
        (workspace (or workspace
                       (and (overlayp note) (overlay-buffer note)
                            (buffer-local-value 'niwa--workspace (overlay-buffer note))))))
    (and (eq 'taken (plist-get properties 'niwa-note-state))
         (or (gethash (plist-get properties 'niwa-uuid) niwa-notes--taken-watches)
             (gethash (niwa--instruction-workspace-key workspace)
                      niwa-notes--taken-watches)))))

(defun niwa-notes--state-ignored-p (root)
  "Return non-nil when ROOT's Git ignore policy covers `.niwa/'."
  (with-temp-buffer
    (let ((default-directory root))
      (and (condition-case nil
               (zerop (process-file
                       "git" nil (list t nil) nil
                       "-c" "core.excludesFile=/dev/null"
                       "check-ignore" "--no-index" "-v" ".niwa/"))
             (file-error nil))
           (string-match-p
            "\\(?:\\`\\|/\\)\\.gitignore:[0-9]+:[^!\n]"
            (buffer-string))))))

(defvar niwa-notes-attention-state-functions nil
  "Functions deriving attention state from a live overlay or properties plist.
The first non-nil result wins.  Called from redisplay: use only in-memory
state, never load features, perform I/O, or mutate the supplied properties.")

(defun niwa-notes-attention-state (properties)
  "Return a note's attention state from PROPERTIES, a plist or live overlay.
Transient workflow attention takes precedence over persisted lifecycle."
  (or (if (overlayp properties)
          (overlay-get properties 'niwa-note-attention-state)
        (plist-get properties 'niwa-note-attention-state))
      (and niwa-notes-attention-state-functions
           (run-hook-with-args-until-success
            'niwa-notes-attention-state-functions
            properties))
      (if (overlayp properties)
          (overlay-get properties 'niwa-note-state)
        (plist-get properties 'niwa-note-state))
      'unsent))

(defun niwa-notes-mode-line ()
  "Return this buffer's cached-workspace note attention summary.
Only inspect in-memory records.  Never discover a workspace, visit a file,
or observe a store from redisplay."
  (when-let* ((key (or (when-let* ((workspace
                                   (or (bound-and-true-p niwa--workspace)
                                       (and (bound-and-true-p niwa--session)
                                            (niwa-session-workspace niwa--session)))))
                         (niwa--instruction-workspace-key workspace))
                       niwa--instruction-current-state-key))
              (state (gethash key niwa--instruction-states)))
    (let ((working 0) (ready 0) (stale 0))
      (dolist (entry (plist-get state :instructions))
        (dolist (note (if (bufferp (car entry)) (cdr entry)
                       (plist-get (cdr entry) :instructions)))
          (let ((properties (if (overlayp note) note (plist-get note :properties))))
            (when (eq (if (overlayp note)
                          (overlay-get note 'niwa-instruction-type)
                        (plist-get properties 'niwa-instruction-type)) 'note)
              (pcase (niwa-notes-attention-state properties)
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
(add-to-list 'global-mode-string '(:eval (niwa-notes-mode-line)) t)

(defun niwa-notes-open (workspace)
  "Return lightweight open note snapshots in WORKSPACE without visiting files.
Each snapshot holds :file, :line, :properties, :uuid and cached :source-missing.
The :source is a live overlay or a stashed instruction record; treat it as
opaque.  :properties is a fresh plist; stashed entries copy theirs, so changing
a snapshot never changes the canonical note.  Stashed positions refer to the
last observed source; visiting re-anchors it."
  (let (notes)
    (dolist (entry (plist-get (niwa--instruction-state
                             (niwa--instruction-workspace-key workspace))
                            :instructions))
      (if (bufferp (car entry))
          (when (buffer-live-p (car entry))
            (with-current-buffer (car entry)
              (save-restriction
                (widen)
                (dolist (note (cdr entry))
                  (when (and (overlay-buffer note)
                             (eq (overlay-get note 'niwa-instruction-type) 'note))
                    (push (list :file buffer-file-name
                                :source note
                                :source-missing niwa--buffer-source-missing
                                :line (line-number-at-pos (overlay-start note))
                                :uuid (overlay-get note 'niwa-uuid)
                                :properties
                                (plist-put (niwa--instruction-persisted-properties note)
                                           'niwa-note-attention-state
                                           (overlay-get note 'niwa-note-attention-state)))
                          notes))))))
        (dolist (note (plist-get (cdr entry) :instructions))
          (let ((properties (plist-get note :properties))
                (content (plist-get (cdr entry) :original-content)))
            (when (eq (plist-get properties 'niwa-instruction-type) 'note)
              (push (list :file (car entry)
                          :source note
                          :source-missing (plist-get (cdr entry) :source-missing)
                          :line (if content
                                    (1+ (cl-count ?\n content :end
                                                  (min (length content)
                                                       (max 0 (1- (plist-get note :overlay-start))))))
                                  1)
                          :uuid (plist-get properties 'niwa-uuid)
                          :properties (copy-tree properties))
                    notes))))))
    (sort notes (lambda (a b)
                  (< (plist-get (plist-get a :properties) 'niwa-id)
                     (plist-get (plist-get b :properties) 'niwa-id))))))

(defun niwa-note-resolve (identity workspace)
  "Return the live note with numeric ID or UUID IDENTITY in WORKSPACE.
Restore only the selected note's file, not every stashed source file."
  (when-let* ((entry (cl-find-if
                     (lambda (note)
                       (equal identity (if (integerp identity)
                                           (plist-get (plist-get note :properties) 'niwa-id)
                                         (plist-get note :uuid))))
                     (niwa-notes-open workspace))))
    (let ((file (plist-get entry :file))
          (uuid (plist-get entry :uuid))
          (niwa--instruction-state-key-override
           (niwa--instruction-workspace-key workspace)))
      (niwa--restore-file-instructions file nil workspace)
      (when-let* ((buffer (find-buffer-visiting file)))
        (cl-find uuid (cdr (assq buffer (niwa--instruction-alist)))
                 :key (lambda (note) (overlay-get note 'niwa-uuid))
                 :test #'equal)))))

(defun niwa-notes-resolve-entries (entries workspace)
  "Resolve note ENTRIES from `niwa-notes-open' in WORKSPACE.
Restore each source at most once and return the live notes in ENTRIES' order.
Refuse unavailable sources before a caller starts mutating the notes."
  (let ((niwa--instruction-state-key-override
         (niwa--instruction-workspace-key workspace))
        (notes (make-hash-table :test #'equal)))
    (dolist (file (delete-dups (mapcar (lambda (entry) (plist-get entry :file)) entries)))
      (niwa--restore-file-instructions file nil workspace)
      (when-let* ((buffer (find-buffer-visiting file)))
        (dolist (note (cdr (assq buffer (niwa--instruction-alist))))
          (when (overlay-buffer note)
            (puthash (overlay-get note 'niwa-uuid) note notes)))))
    (mapcar (lambda (entry)
              (let ((uuid (plist-get entry :uuid)))
                (or (gethash uuid notes)
                    (user-error "Source unavailable for note %s; no notes changed" uuid))))
            entries)))

(defun niwa-notes--display-text (note &optional width)
  "Return NOTE's single-line text bounded by WIDTH and 80 columns."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\r\n]+" " "
                             (or (overlay-get note 'niwa-note-text) ""))
   (min 80 (or width 80)) nil nil "…"))

(defun niwa-notes--actions-hint (&optional note)
  "Return the current modified-key menu hint for NOTE."
  (format "%s: %s"
          (if-let* ((key (where-is-internal #'niwa--ov-actions-dispatch
                                           niwa-note-actions-map t)))
              (key-description key)
            "M-x niwa-note-menu")
          (format "send, %s, edit, done, %s"
                  (if (and note (niwa-note-watching-p note)) "drop" "take")
                  (pcase (and note (overlay-get note 'niwa-note-author))
                    ('buddy "dismiss, keep") ('agent "dismiss") (_ "delete")))))

(defun niwa-notes--author-name (author)
  "Return AUTHOR's display name without changing its stored symbol."
  (pcase author ('buddy "Buddy") ('agent "AI") (_ "You")))

(defun niwa-notes--entry-hint ()
  "Announce the note and its next actions when point enters it."
  (unless (or (bound-and-true-p isearch-mode) executing-kbd-macro
              (active-minibuffer-window))
    (let ((notes (sort (niwa--instructions-at (point) 'note)
                       (lambda (a b) (< (overlay-get a 'niwa-id)
                                        (overlay-get b 'niwa-id))))))
      (unless (equal notes niwa-notes--last-hinted)
        (setq niwa-notes--last-hinted notes)
        (when notes
          (message "%s · %s"
                   (mapconcat (lambda (note)
                                (format "Note %s · %s  %s"
                                        (overlay-get note 'niwa-id)
                                        (niwa-notes--author-name
                                         (overlay-get note 'niwa-note-author))
                                        (niwa-notes--display-text note)))
                              notes " | ")
                   (if (cdr notes) "M-m: choose note"
                     (niwa-notes--actions-hint (car notes)))))))))

(defvar-local niwa-notes--window-width nil
  "Width last fitted after a window resize.")

(defun niwa-notes--label-width ()
  "Return the narrowest visible width, since labels are shared by windows."
  (max 4 (if-let* ((windows (get-buffer-window-list nil nil 'visible)))
             (apply #'min (mapcar #'window-body-width windows))
           (window-body-width))))

(defun niwa-notes--refit-labels (_window)
  "Refit the buffer's existing labels when their available width changes."
  (let ((width (niwa-notes--label-width)))
    (unless (eql width niwa-notes--window-width)
      (setq niwa-notes--window-width width)
      (save-restriction
        (widen)
        (dolist (note (cdr (assq (current-buffer) (niwa--instruction-alist))))
          (when (and (overlay-buffer note) (overlay-get note 'niwa-label-overlay))
            (niwa-notes--render note)))))))

(defun niwa-notes--label-string (header text bar &optional collapsed)
  "Fill HEADER and TEXT with BAR at indentation, or truncate if COLLAPSED."
  (let* ((window-width (niwa-notes--label-width))
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

(defun niwa-notes--refresh-label-stack (position)
  "Keep the source row's tint separate from the label stack at POSITION."
  (save-restriction
    (widen)
    (let* ((overlays (overlays-in (max (point-min) (1- position))
                                 (min (point-max) (1+ position))))
           (labels (cl-remove-if-not
                    (lambda (label)
                      (and (= (overlay-end label) position)
                           (overlay-get label 'niwa-note-owner)
                           (overlay-get label 'after-string)))
                    overlays))
           (priority (lambda (note) (or (overlay-get note 'priority) 0)))
           (precedes (lambda (a b)
                       (let ((ap (funcall priority a)) (bp (funcall priority b)))
                         (if (= ap bp)
                             (< (overlay-get a 'niwa-id) (overlay-get b 'niwa-id))
                           (> ap bp)))))
           line real-priority previous)
      (when labels
        (dolist (note overlays)
          (when (and (eq 'note (overlay-get note 'niwa-instruction-type))
                     (<= (overlay-start note) position (overlay-end note)))
            (when (< position (overlay-end note))
              (setq real-priority (max (or real-priority 0) (funcall priority note))))
            (when (and (eq 'line (overlay-get note 'niwa-span))
                       (or (not line) (funcall precedes note line)))
              (setq line note)))))
      ;; Redisplay orders after-strings by decreasing integer priority.
      (setq labels (sort labels :in-place t :key (lambda (label) (overlay-get label 'niwa-note-owner))
                         :lessp precedes))
      (dolist (label labels)
        (let* ((note (overlay-get label 'niwa-note-owner))
               (text (overlay-get label 'after-string))
               (rank (funcall priority note))
               (face (if previous 'default (and line (overlay-get line 'face))))
               (tint (overlay-get note 'niwa-note-tint-overlay)))
          (when previous (setq rank (min (1- previous) rank)))
          ;; The real newline ends the final label row, not the source row.
          (if (and (not previous) (or line real-priority) (eq (char-after position) ?\n))
              (progn
                (unless tint
                  (setq tint (make-overlay position (1+ position) nil t nil))
                  (overlay-put note 'niwa-note-tint-overlay tint))
                (unless (and (eql (overlay-start tint) position)
                             (eql (overlay-end tint) (1+ position)))
                  (move-overlay tint position (1+ position)))
                (overlay-put tint 'priority (1+ (max rank (or real-priority 0))))
                (overlay-put tint 'face (if real-priority '(:inherit default :extend t) 'default)))
            (when tint
              (delete-overlay tint)
              (overlay-put note 'niwa-note-tint-overlay nil)))
          (overlay-put label 'priority rank)
          (unless (eq face (get-text-property 0 'face text))
            (if face (put-text-property 0 1 'face face text)
              (remove-list-of-text-properties 0 1 '(face) text))
            (overlay-put label 'after-string nil)
            (overlay-put label 'after-string text))
          (setq previous rank))))))

(defun niwa-notes--refresh-label-stacks-in (start end)
  "Refresh label stacks on source rows between START and END, inclusive."
  (save-restriction
    (widen)
    (let (positions)
      (dolist (label (overlays-in (max (point-min) (1- start))
                                  (min (point-max) (1+ end))))
        (when (and (overlay-get label 'niwa-note-owner)
                   (<= start (overlay-end label) end))
          (cl-pushnew (overlay-end label) positions)))
      (dolist (position positions)
        (niwa-notes--refresh-label-stack position)))))

(defun niwa-notes--render (note)
  "Render NOTE's label below its code."
  (with-current-buffer (overlay-buffer note)
    (unless (overlay-get note 'niwa-note-labels)
      (overlay-put note 'niwa-note-labels niwa-note-labels))
    (save-excursion
      (goto-char (overlay-end note))
      (when (and (> (point) (overlay-start note)) (bolp))
        (backward-char))
      (let* ((end (line-end-position))
             (label (or (overlay-get note 'niwa-label-overlay)
                        (let ((overlay (make-overlay
                                        (overlay-start note) end nil t t)))
                          (overlay-put overlay 'niwa-note-owner note)
                          (overlay-put note 'niwa-label-overlay overlay)
                          overlay)))
             (old-end (overlay-end label))
             (old-text (overlay-get label 'after-string))
             (author (overlay-get note 'niwa-note-author))
             (code-face (pcase author
                          ('buddy 'niwa-note-buddy) ('agent 'niwa-note-agent)
                          (_ 'niwa-note-you)))
             (face-changed (not (eq code-face (overlay-get note 'face))))
             (face (cond ((eq note niwa--highlighted-instruction) 'highlight)
                         ((eq author 'buddy) 'niwa-note-label-buddy)
                         ((eq author 'agent) 'niwa-note-label-agent)
                         (t 'niwa-note-label-you)))
             (text (niwa-notes--label-string
                      (concat
                       (propertize
                        (format "Note %s · %s"
                                (overlay-get note 'niwa-id)
                                (niwa-notes--author-name author))
                        'face face)
                       (let* ((paused (and (eq 'taken (overlay-get note 'niwa-note-state))
                                           (not (niwa-note-watching-p note))))
                              (state (pcase (niwa-notes-attention-state note)
                                       ('ready "change ready") ('stale "change stale")
                                       ('working "working…")
                                       ('taken
                                        (unless paused
                                          (if (overlay-get note 'niwa-note-buddy-looks-done)
                                              "looks done"
                                            "Buddy watches"))))))
                         (when (or state paused)
                           (propertize (concat (and state (concat " · " state))
                                               (and paused " · paused"))
                                       'face 'shadow))))
                      (or (overlay-get note 'niwa-note-text) "")
                      (propertize (if (eq author 'buddy) "┆" "▍") 'face face)
                      (eq 'collapsed (overlay-get note 'niwa-note-labels)))))
        (overlay-put note 'keymap niwa-note-actions-map)
        (when face-changed
          (overlay-put note 'face code-face))
        (move-overlay label (overlay-start note) end)
        (when-let* ((leading-face (and old-text (get-text-property 0 'face old-text))))
          (put-text-property 0 1 'face leading-face text))
        (unless (equal-including-properties text old-text)
          (overlay-put label 'after-string text))
        (when (and old-end (/= end old-end))
          (niwa-notes--refresh-label-stack old-end))
        (if (and face-changed (eq 'line (overlay-get note 'niwa-span)))
            (niwa-notes--refresh-label-stacks-in (overlay-start note) end)
          (niwa-notes--refresh-label-stack end))))
    (add-hook 'after-change-functions #'niwa-notes--after-change nil t)
    (add-hook 'post-command-hook #'niwa-notes--entry-hint nil t)
    (add-hook 'window-size-change-functions #'niwa-notes--refit-labels nil t)))

;;;###autoload
(defun niwa-notes-toggle-labels ()
  "Expand every note label in this buffer, or collapse if all are expanded."
  (interactive)
  (let* ((notes (cl-remove-if-not
                 (lambda (note)
                   (and (overlay-buffer note)
                        (eq 'note (overlay-get note 'niwa-instruction-type))))
                 (cdr (assq (current-buffer) (niwa--instruction-alist)))))
         (display (if (cl-every (lambda (note)
                                 (eq 'expanded (overlay-get note 'niwa-note-labels)))
                               notes)
                      'collapsed 'expanded)))
    (save-restriction
      (widen)
      (dolist (note notes)
        (overlay-put note 'niwa-note-labels display)
        (niwa-notes--render note)))))

(defun niwa-notes--after-change (beg end old-length)
  "Reposition labels touched by BEG..END after replacing OLD-LENGTH characters."
  (dolist (overlay (overlays-in (max (point-min) (1- beg))
                               (min (point-max) (1+ end))))
    (when-let* ((note (overlay-get overlay 'niwa-note-owner)))
      (if (overlay-buffer note)
          (progn
            (overlay-put note 'niwa-note-buddy-looks-done nil)
            (if (and (overlay-get note 'niwa-note-prepared-change)
                     (if (zerop old-length)
                         (and (< beg (overlay-end note))
                              (> end (overlay-start note)))
                       (and (<= beg (overlay-end note))
                            (>= end (overlay-start note)))))
                (niwa-note-mark-stale note)
              (niwa-notes--render note)))
        (niwa--delete-instruction-overlay note)))))

(defun niwa-notes--at-point ()
  "Return the note at point, choosing when notes overlap; refuse if absent."
  (let ((notes (niwa--instructions-at (point) 'note)))
    (unless notes (user-error "No note at point"))
    (if (not (cdr notes)) (car notes)
      (let ((choices (mapcar
                      (lambda (note)
                        (cons (format "Note %s · %s  %s"
                                      (overlay-get note 'niwa-id)
                                      (niwa-notes--author-name
                                       (overlay-get note 'niwa-note-author))
                                      (niwa-notes--display-text note)) note))
                      (sort notes (lambda (a b) (< (overlay-get a 'niwa-id)
                                                  (overlay-get b 'niwa-id)))))))
        (cdr (assoc (completing-read "Choose note: " (mapcar #'car choices) nil t)
                    choices))))))

(defun niwa-notes--read-tags (note)
  "Read the complete tag set for NOTE."
  (let ((tags (overlay-get note 'niwa-note-tags)))
    (mapcar #'intern
            (completing-read-multiple
             "Tags (empty removes all): " (mapcar #'symbol-name tags)
             nil nil (mapconcat #'symbol-name tags ",")))))

(defun niwa-notes--validate-tags (tags)
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
(defun niwa-note-set-tags (tags &optional note)
  "Replace NOTE's tags with TAGS, a list of symbols.
Default to the note at point.  An empty list removes every tag."
  (interactive
   (let ((note (niwa-notes--at-point)))
     (list (niwa-notes--read-tags note) note)))
  (setq note (or note (niwa-notes--at-point)))
  (let ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (niwa-notes--validate-tags tags)
      (overlay-put note 'niwa-note-tags (delete-dups (copy-sequence tags)))
      (niwa-notes--render note)
      (niwa-instruction-store-touch workspace))))

;;;###autoload
(defun niwa-note-edit (text &optional note)
  "Replace NOTE's text with TEXT; default to the note at point."
  (interactive (let ((note (niwa-notes--at-point)))
                 (list (read-string "Note: " (overlay-get note 'niwa-note-text))
                       note)))
  (let ((workspace (niwa--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (setq note (or note (niwa-notes--at-point)))
      (overlay-put note 'niwa-note-text text)
      (overlay-put note 'niwa-note-buddy-looks-done nil)
      (niwa-notes--render note)
      (niwa-instruction-store-touch workspace))))

;;;###autoload
(defun niwa-note-take (&optional note)
  "Take NOTE to implement it yourself, with Buddy watching its code.
Default to the note at point.  Taking Buddy's remark first keeps it as yours."
  (interactive)
  (setq note (or note (niwa-notes--at-point)))
  (let ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (when (eq 'buddy (overlay-get note 'niwa-note-author))
        (niwa-note-keep note))
      (overlay-put note 'niwa-note-state 'taken)
      (overlay-put note 'niwa-note-buddy-looks-done nil)
      (overlay-put note 'niwa-note-buddy-reviewed-hash nil)
      (puthash (overlay-get note 'niwa-uuid) t niwa-notes--taken-watches)
      (with-current-buffer (overlay-buffer note) (niwa-buddy-mode 1))
      (niwa-notes--render note)
      (niwa-instruction-store-touch workspace)))
  note)

;;;###autoload
(defun niwa-note-drop (&optional note)
  "Stop watching taken NOTE and return it to unsent, keeping its conversation."
  (interactive)
  (setq note (or note (niwa-notes--at-point)))
  (let ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (unless (eq 'taken (overlay-get note 'niwa-note-state))
        (user-error "Note is not taken"))
      (overlay-put note 'niwa-note-state 'unsent)
      (overlay-put note 'niwa-note-buddy-looks-done nil)
      (niwa-notes--render note)
      (niwa-instruction-store-touch workspace)))
  note)


;;
;;; Lifecycle transitions

;; Each verb owns the properties that change together and only the render,
;; conversation-hint and store-touch effects its callers need.  None flushes:
;; a caller that needs durability flushes after the verb.

(defun niwa-notes--workspace (notes)
  "Return the workspace owning the first live note in NOTES."
  (niwa--instruction-buffer-workspace (overlay-buffer (car notes))))

(defun niwa-note-mark-sent (notes session-id)
  "Mark live NOTES sent in conversation SESSION-ID, then touch the store once.
A note keeps its existing session id."
  (dolist (note notes)
    (unless (overlay-get note 'niwa-note-session-id)
      (overlay-put note 'niwa-note-session-id session-id))
    (overlay-put note 'niwa-note-state 'sent)
    (niwa-notes--render note)
    (niwa-note-conversation-refresh-hint note))
  (when notes (niwa-instruction-store-touch (niwa-notes--workspace notes))))

(defun niwa-note-hold-change (notes record)
  "Hold prepared change RECORD on live NOTES as ready, then touch the store once.
The caller validates RECORD and computes its :note-extents."
  (dolist (note notes)
    (overlay-put note 'niwa-note-prepared-change record)
    (overlay-put note 'niwa-note-attention-state 'ready)
    (niwa-notes--render note)
    ;; An unlinked suggestion has no conversation; without an id the hint
    ;; would fall back to the current one and advertise changes it cannot apply.
    (when (overlay-get note 'niwa-note-session-id)
      (niwa-note-conversation-refresh-hint note)))
  (when notes (niwa-instruction-store-touch (niwa-notes--workspace notes))))

(defun niwa-note-record-applied (record files workspace)
  "Record applied change RECORD on every open note in WORKSPACE that holds it.
Each such note gains the applied pair, with FILES only when RECORD belongs to
another session's suggestion, and loses its prepared change and attention.
Touch the store once."
  (dolist (entry (niwa-notes-open workspace))
    (let ((props (plist-get entry :properties)))
      (when (equal (plist-get record :id)
                   (plist-get (plist-get props 'niwa-note-prepared-change) :id))
        (let* ((source (plist-get entry :source))
               (id (plist-get record :session-id))
               (pair (append (list :session-id id :turn (plist-get record :turn))
                             (unless (equal id (plist-get props 'niwa-note-session-id))
                               (list :files files))))
               (pairs (cl-adjoin pair (plist-get props 'niwa-note-applied-changes)
                                 :test #'equal)))
          (if (overlayp source)
              (progn
                (overlay-put source 'niwa-note-applied-changes pairs)
                (overlay-put source 'niwa-note-prepared-change nil)
                (overlay-put source 'niwa-note-attention-state nil)
                (niwa-notes--render source)
                (niwa-note-conversation-refresh-hint source))
            (let ((stored (plist-get source :properties)))
              (plist-put stored 'niwa-note-applied-changes pairs)
              (plist-put stored 'niwa-note-prepared-change nil)
              (plist-put stored 'niwa-note-attention-state nil)))))))
  (niwa-instruction-store-touch workspace))

(defun niwa-note-mark-stale (note)
  "Mark NOTE's prepared change stale and render it."
  (overlay-put note 'niwa-note-attention-state 'stale)
  (niwa-notes--render note))

(defun niwa-note-set-settings (note settings)
  "Merge the plist SETTINGS into NOTE's conversation settings and touch the store."
  (let ((merged (copy-sequence (overlay-get note 'niwa-note-settings))))
    (while settings
      (setq merged (plist-put merged (pop settings) (pop settings))))
    (overlay-put note 'niwa-note-settings merged)
    (niwa-instruction-store-touch (niwa-notes--workspace (list note)))))

(defun niwa-note-keep (note)
  "Keep Buddy's NOTE as a user note, touching the store for file-backed buffers."
  (unless (eq 'buddy (overlay-get note 'niwa-note-author))
    (user-error "Not a Buddy remark"))
  (overlay-put note 'niwa-note-author 'user)
  (overlay-put note 'niwa-note-kept-from-buddy t)
  (niwa--update-instruction-overlay note t)
  (when (buffer-local-value 'buffer-file-name (overlay-buffer note))
    (niwa-instruction-store-touch (niwa-notes--workspace (list note))))
  note)

(defun niwa-notes-rewind-instruction (instruction session-id surviving-turn)
  "Rewind INSTRUCTION's note activity owned by SESSION-ID past SURVIVING-TURN.
INSTRUCTION is a persisted record with :properties.  Mutate only lifecycle
properties; preserve its text, anchor and session link."
  (let* ((props (plist-get instruction :properties))
         (change (plist-get props 'niwa-note-prepared-change)))
    (when (equal session-id (plist-get props 'niwa-note-session-id))
      (when (zerop surviving-turn)
        (when (eq 'sent (plist-get props 'niwa-note-state))
          (plist-put props 'niwa-note-state 'unsent))
        (when (eq 'sent (plist-get instruction :open-state))
          (plist-put instruction :open-state 'unsent))))
    (when (and (equal session-id (plist-get change :session-id))
               (> (or (plist-get change :turn) 0) surviving-turn))
      (plist-put props 'niwa-note-prepared-change nil)
      (plist-put props 'niwa-note-attention-state nil))
    (when-let* ((pairs (plist-get props 'niwa-note-applied-changes)))
      (plist-put props 'niwa-note-applied-changes
                 (cl-remove-if (lambda (pair)
                                 (and (equal session-id (plist-get pair :session-id))
                                      (> (plist-get pair :turn) surviving-turn))) pairs))))
  instruction)

(defun niwa-notes-rewind-live (workspace session-id turn)
  "Apply committed SESSION-ID Rewind to TURN to WORKSPACE's current notes.
Keep memory in step with the new merge base, including crash recovery."
  (let ((state (niwa--instruction-state (niwa--instruction-workspace-key workspace))))
    (dolist (file (plist-get state :instructions))
      (if (bufferp (car file))
          (dolist (note (cdr file))
            (let* ((before (overlay-properties note))
                   (instruction (list :properties (copy-sequence before)))
                   (after (plist-get (niwa-notes-rewind-instruction
                                      instruction session-id turn) :properties)))
              (unless (equal before after)
                (dolist (key '(niwa-note-state niwa-note-prepared-change
                              niwa-note-applied-changes niwa-note-attention-state))
                  (overlay-put note key (plist-get after key)))
                (niwa--update-instruction-overlay note t)
                (niwa-note-conversation-refresh-hint note))))
        (dolist (instruction (plist-get (cdr file) :instructions))
          (niwa-notes-rewind-instruction instruction session-id turn))))
    (dolist (closed (plist-get state :closed-notes))
      (niwa-notes-rewind-instruction (plist-get closed :instruction) session-id turn))))

(defun niwa-notes--restore-taken (buffer)
  "Label restored takes in BUFFER; watch only explicitly resumed notes."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (dolist (note (cdr (assq buffer (niwa--instruction-alist))))
        (when (eq 'taken (overlay-get note 'niwa-note-state))
          (niwa-notes--render note)
          (when (niwa-note-watching-p note)
            (unless (bound-and-true-p niwa-buddy-mode) (niwa-buddy-mode 1))))))))

(add-hook 'niwa-instructions-restored-hook #'niwa-notes--restore-taken)

(defun niwa-notes-resume-taken (workspace)
  "Resume WORKSPACE's taken notes for the rest of this Emacs session."
  (puthash (niwa--instruction-workspace-key workspace) t niwa-notes--taken-watches)
  (dolist (entry (plist-get (niwa--instruction-state
                           (niwa--instruction-workspace-key workspace)) :instructions))
    (when (buffer-live-p (car entry))
      (niwa-notes--restore-taken (car entry)))))

;;;###autoload
(defun niwa-note-delete (&optional note)
  "Move NOTE to recently closed as deleted, without changing code.
NOTE defaults to the note at point.  Restore it with `niwa-note-restore'."
  (interactive)
  (niwa-note-close (or note (niwa-notes--at-point)) 'deleted))

;;;###autoload
(defun niwa-note-dismiss (&optional note)
  "Dismiss agent-authored NOTE, defaulting to the note at point."
  (interactive)
  (setq note (or note (niwa-notes--at-point)))
  (let ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (unless (eq 'agent (overlay-get note 'niwa-note-author))
        (user-error "Not an agent-authored note"))
      (niwa-note-close note 'dismissed))))

;;;###autoload
(defun niwa-note-dismiss-agent-notes (&optional workspace)
  "Dismiss all agent-authored notes in WORKSPACE, syncing once.
Return the number dismissed.  User and Buddy notes are left alone."
  (interactive)
  (setq workspace (or workspace (niwa--instruction-buffer-workspace (current-buffer))))
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
    (niwa-instruction-store-observe workspace)
    (let ((count 0)
          (notes (niwa-notes-resolve-entries
                  (cl-remove-if-not
                   (lambda (entry)
                     (eq 'agent (plist-get (plist-get entry :properties) 'niwa-note-author)))
                   (niwa-notes-open workspace))
                  workspace)))
      (unwind-protect
          (dolist (note notes)
            (niwa-note-dismiss note)
            (cl-incf count))
        (when (> count 0) (niwa-instruction-store-sync workspace)))
      count)))

(defun niwa-notes--store-closed (file instruction reason)
  "Record FILE's serialized INSTRUCTION as closed with REASON."
  (let ((properties (plist-get instruction :properties)))
    (plist-put instruction :open-state (plist-get properties 'niwa-note-state))
    (plist-put instruction :closed-reason reason)
    (plist-put instruction :closed-at (float-time))
    (plist-put properties 'niwa-note-state 'closed)
    (push (list :file file :instruction instruction)
          (plist-get (niwa--instruction-state (niwa--instruction-operation-state-key))
                     :closed-notes))))

(defun niwa-note-close (note reason)
  "Move live NOTE to recently closed with REASON, retaining its attachments.
REASON is `done', `deleted' or `dismissed'.  Closing never changes source code."
  (let* ((buffer (overlay-buffer note))
         (workspace (niwa--instruction-buffer-workspace buffer))
         (niwa--instruction-state-key-override
          (niwa--instruction-workspace-key workspace)))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (unless (memq reason '(done deleted dismissed))
        (user-error "Unknown close reason: %s" reason))
      (unless (and buffer (eq (overlay-get note 'niwa-instruction-type) 'note))
        (user-error "Not a live note"))
      (with-current-buffer buffer
        (let* ((properties (niwa--instruction-persisted-properties note))
               (instruction
                (list :overlay-start (overlay-start note)
                      :overlay-end (overlay-end note)
                      :anchor (niwa--instruction-anchor-for-instruction note)
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
          (niwa-notes--store-closed buffer-file-name instruction reason)
          ;; Explicit closure wins over older source geometry undo.  Reserve the
          ;; display number until expiry, rather than retiring it on close.
          (overlay-put note 'niwa-marked-for-deletion t)
          (niwa--delete-instruction-overlay note)
          (setf (alist-get buffer (niwa--instruction-alist))
                (delq note (alist-get buffer (niwa--instruction-alist))))
          (niwa-instruction-store-prune-closed workspace)
          (niwa-instruction-store-touch workspace))))))

(defun niwa-notes-close-applied (note session-id workspace)
  "Close NOTE as done, including notes it shares with SESSION-ID in WORKSPACE."
  (let ((niwa--instruction-state-key-override
         (niwa--instruction-workspace-key workspace))
        (uuid (overlay-get note 'niwa-uuid))
        (shared (equal session-id (overlay-get note 'niwa-note-session-id))))
    (dolist (entry (niwa-notes-open workspace))
      (when (or (equal uuid (plist-get entry :uuid))
                (and shared (equal session-id
                                   (plist-get (plist-get entry :properties) 'niwa-note-session-id))))
        (let ((source (plist-get entry :source)))
          (if (overlayp source)
              (niwa-note-close source 'done)
            (let* ((file (plist-get entry :file))
                   (stash (assoc file (niwa--instruction-alist))))
              (niwa-notes--store-closed file source 'done)
              (setf (plist-get (cdr stash) :instructions)
                    (delq source (plist-get (cdr stash) :instructions)))
              (unless (plist-get (cdr stash) :instructions)
                (setf (niwa--instruction-alist) (delq stash (niwa--instruction-alist)))))))))
    (niwa-instruction-store-prune-closed workspace)
    (niwa-instruction-store-touch workspace)))

;;;###autoload
(defun niwa-note-done (&optional note)
  "Explicitly mark NOTE done, defaulting to the note at point."
  (interactive)
  (niwa-note-close (or note (niwa-notes--at-point)) 'done))

(defun niwa-notes-closed (&optional workspace loaded-only)
  "Return snapshots of recently closed notes in WORKSPACE.
Each entry has :file and :instruction, the serialized note with :closed-reason,
:closed-at and :open-state.  Callers may change these snapshots freely.
When LOADED-ONLY is non-nil, inspect only loaded state without observing the
store.  Retention pruning and snapshot copying still apply."
  (setq workspace (or workspace (niwa--instruction-buffer-workspace
                                (current-buffer))))
  (unless loaded-only (niwa-notes--load-closed workspace))
  (niwa-instruction-store-prune-closed workspace)
  (niwa--instruction-serializable-value
   (plist-get (niwa--instruction-state
               (niwa--instruction-workspace-key workspace))
              :closed-notes)))

(defun niwa-notes--load-closed (workspace)
  "Load WORKSPACE's store on cold first use of closed-note commands."
  (unless (plist-get (niwa--instruction-state
                      (niwa--instruction-workspace-key workspace))
                     :store-observation)
    (niwa-instruction-store-observe workspace)))

;;;###autoload
(defun niwa-note-restore (id &optional workspace)
  "Restore the recently closed note with display number or UUID ID in WORKSPACE.
Return its live source overlay, retaining its text, tags and attachments."
  (interactive
   (let* ((workspace (niwa--instruction-buffer-workspace (current-buffer)))
          (choices (mapcar
                    (lambda (entry)
                      (let* ((instruction (plist-get entry :instruction))
                             (props (plist-get instruction :properties)))
                        (cons (format "%s: %s (%s)"
                                      (plist-get props 'niwa-id)
                                      (plist-get props 'niwa-note-text)
                                      (plist-get instruction :closed-reason))
                              (plist-get props 'niwa-uuid))))
                    (niwa-notes-closed workspace))))
     (list (cdr (assoc (completing-read "Restore note: " choices nil t) choices))
           workspace)))
  (setq workspace (or workspace (niwa--instruction-buffer-workspace
                                (current-buffer))))
  (niwa-notes--load-closed workspace)
  (niwa-instruction-store-prune-closed workspace)
  (let* ((niwa--instruction-state-key-override
          (niwa--instruction-workspace-key workspace))
         (state (niwa--instruction-state
                 (niwa--instruction-operation-state-key)))
         (entry (cl-find id (plist-get state :closed-notes)
                         :key (lambda (entry)
                                (plist-get
                                 (plist-get (plist-get entry :instruction) :properties)
                                 (if (numberp id) 'niwa-id 'niwa-uuid)))
                         :test #'equal)))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (unless entry (user-error "No recently closed note %s" id))
      (let* ((instruction (plist-get entry :instruction))
             (buffer (find-file-noselect (plist-get entry :file)))
             (properties (copy-tree (plist-get instruction :properties))))
        (with-current-buffer buffer
          (setq-local niwa--workspace workspace)
          (plist-put properties 'niwa-note-state (plist-get instruction :open-state))
          (let ((note
                 (save-excursion
                   (save-restriction
                     (widen)
                     (let ((bounds (niwa-notes--restore-bounds instruction)))
                       (niwa--restore-overlay buffer (car bounds) (cdr bounds)
                                                 properties))))))
            (setf (plist-get state :closed-notes)
                  (delq entry (plist-get state :closed-notes)))
            (push note (alist-get buffer (niwa--instruction-alist)))
            (niwa--setup-buffer-hooks buffer)
            (save-restriction
              (widen)
              (niwa--update-instruction-overlay note t))
            (niwa-instruction-store-touch workspace)
            note))))))

(defun niwa-notes--restore-bounds (instruction)
  "Resolve closed INSTRUCTION, falling back to its line or absolute positions."
  (or (niwa-persistence-resolve-instruction-anchor
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
(defun niwa-note-menu (&optional note)
  "Show the actions menu for NOTE, defaulting to the note at point."
  (interactive)
  (require 'rmc)
  (let ((workspace (niwa--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (setq note (or note (niwa-notes--at-point)))
      (let* ((choices
              (append '((?s "send" "Send or reopen the note conversation")
                        (?p "preview" "Preview the exact note prompt without sending")
                        (?i "edit" "Edit note text")
                        (?w "copy text" "Copy full note text to the kill ring")
                        (?x "done" "Mark note done"))
                      (if (eq 'expanded (overlay-get note 'niwa-note-labels))
                          '((?e "collapse" "Show a one-line note label"))
                        '((?e "expand" "Show the full note label")))
                      (if (niwa-note-watching-p note)
                          '((?t "drop" "Stop watching this note"))
                        '((?t "take" "Implement yourself with Buddy watching")))
                      (if (eq 'buddy (overlay-get note 'niwa-note-author))
                          '((?k "dismiss" "Dismiss this remark")
                            (?b "keep" "Keep as your own note"))
                        (if (eq 'agent (overlay-get note 'niwa-note-author))
                            '((?k "dismiss" "Dismiss this agent finding"))
                          '((?k "delete" "Delete note without changing code"))))
                      (when (or (overlay-get note 'niwa-note-applied-changes)
                                (when-let* ((id (overlay-get note 'niwa-note-session-id))
                                            (data (niwa-session-control-transfer-root-buffer-for-id id))
                                            (session (buffer-local-value 'niwa--session data)))
                                  (cl-some #'cdr (niwa-session-file-snapshots session))))
                        '((?r "revert" "Revert an applied change, preserving later edits")))
                      (when (overlay-get note 'niwa-note-prepared-change)
                        '((?a "apply" "Apply prepared change")
                          (?m "merge" "Resolve in a three-way view")))
                      '((?g "tags" "Edit note tags")
                        (?S "settings" "Configure this note conversation"))))
             (label (overlay-get note 'niwa-label-overlay))
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
                (niwa-notes--refresh-label-stack (overlay-end label))))))
        (pcase choice
          (?s (niwa-note-send note))
          (?p (niwa-note-preview-prompt note))
          (?a (niwa-note-apply-change note))
          (?r (niwa-note-revert-change note))
          (?m (niwa-note-merge-change note))
          (?t (if (niwa-note-watching-p note)
                  (niwa-note-drop note)
                (niwa-note-take note)))
          (?e (overlay-put note 'niwa-note-labels
                           (if (eq 'expanded (overlay-get note 'niwa-note-labels))
                               'collapsed 'expanded))
              (niwa-notes--render note))
          (?i (niwa-note-edit
               (read-string "Note: " (overlay-get note 'niwa-note-text)) note))
          (?w (kill-new (or (overlay-get note 'niwa-note-text) ""))
              (message "niwa: note text copied"))
          (?x (niwa-note-done note))
          (?g (niwa-note-set-tags (niwa-notes--read-tags note) note))
          (?S (niwa-note-settings note))
          (?k (pcase (overlay-get note 'niwa-note-author)
                ('buddy (niwa-buddy-dismiss-note note))
                ('agent (niwa-note-dismiss note))
                (_ (niwa-note-delete note))))
          (?b (niwa-buddy-keep-note note)))))))

(defun niwa-notes--ensure-workspace (workspace)
  "Prepare the current file's WORKSPACE for note storage, without syncing.
Create `.niwa/' only in a recognized project.  Return the actual workspace."
  (unless (file-directory-p (niwa-workspace-state-dir workspace))
    (if-let* ((project (niwa-workspace--project-current
                        (file-name-directory buffer-file-name))))
        (progn
          (setq workspace (niwa-workspace-get-or-create
                           'project (project-root project) (project-root project)
                           (project-name project)))
          (setq-local niwa--workspace workspace)
          (make-directory (niwa-workspace-state-dir workspace) t)
          (unless (niwa-notes--state-ignored-p
                   (niwa-workspace-root workspace))
            (niwa-diagnostic-message
             (niwa-diagnostic-scope workspace)
             "Created .niwa/; it is not covered by the project's .gitignore")))
      (let ((root (niwa-workspace-root workspace)))
        (unless (member root niwa-notes--unsaved-noticed)
          (push root niwa-notes--unsaved-noticed)
          (niwa-diagnostic-message
           (niwa-diagnostic-scope workspace)
           "Notes outside a project are not saved")))))
  workspace)

(cl-defun niwa-note-make (buffer beg end &key text author span)
  "Create and render a note in BUFFER over BEG..END, and return its overlay.
AUTHOR is `user' (default), `agent' or `buddy'.  SPAN is `line' for whole
lines, `char' for the exact range, or nil to infer the shape.
The note starts unsent with TEXT (default empty).  Work in
BUFFER without a file, never read or change the region, point or selected
window, and neither write nor touch the store: callers prepare the workspace
and decide durability."
  (let ((note (niwa--create-instruction-overlay-in buffer beg end span)))
    (overlay-put note 'niwa-instruction-type 'note)
    (overlay-put note 'niwa-note-text (or text ""))
    (overlay-put note 'niwa-note-author (or author 'user))
    (overlay-put note 'niwa-note-state 'unsent)
    (niwa--update-instruction-overlay note t)
    note))

;;;###autoload
(cl-defun niwa-note-create (&optional text &key defer-write)
  "Create a note with optional TEXT on the region or current line.
An empty string creates a code-only note.  Return the note overlay.
DEFER-WRITE leaves durability to the caller's batch store sync."
  (interactive
   (list
    (if (featurep 'niwa-view-composer)
        (niwa-view-call-preserving-window-configuration
         (lambda () (read-string "Note (optional): ")))
      (read-string "Note (optional): "))))
  (let ((workspace (niwa--instruction-buffer-workspace (current-buffer))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (unless buffer-file-name
        (user-error "Notes need a source file"))
      (setq workspace (niwa-notes--ensure-workspace workspace))
      (let* ((region (use-region-p))
             (note (niwa-note-make
                    (current-buffer)
                    (if region (region-beginning) (line-beginning-position))
                    (if region (region-end) (line-end-position))
                    :text text :span (unless region 'line))))
        (if defer-write
            (niwa-instruction-store-touch workspace)
          (niwa--instruction-store-write (current-buffer)))
        (deactivate-mark)
        note))))

(provide 'niwa-notes)
;;; niwa-notes.el ends here
