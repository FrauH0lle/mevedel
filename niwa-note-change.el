;;; niwa-note-change.el -- Prepared note changes -*- lexical-binding: t -*-

;;; Commentary:

;; Note-session source patches settle immediately as durable proposals.  User
;; commands later apply them through the normal patch transaction and review.
;; Recorded application pairs support checked reversal without Rewind.

;;; Code:

(require 'cl-lib)
(require 'niwa-change)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-tool-patch)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-transfer)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)

(declare-function niwa-patch-review-start "niwa-patch-review" (proposal callback data-buffer))

;; `niwa-session-rewind'
(declare-function niwa-session-artifacts-read-backup "niwa-session-artifacts" (session backup-name))
(autoload 'niwa-session-artifacts-read-backup "niwa-session-artifacts")

(defun niwa-note-change-record (proposal changes data-buffer)
  "Capture prepared PROPOSAL and CHANGES owned by DATA-BUFFER's current turn."
  (with-current-buffer data-buffer
    (let ((session niwa--session)
          (proposal (copy-tree proposal))
          (changes (copy-tree changes)))
      (cl-remf proposal :baseline)
      (dolist (operation (plist-get proposal :operations))
        (cl-remf operation :baseline-content))
      (dolist (change changes)
        (when (plist-get change :bytes)
          (cl-remf change :content)))
      (list :id (niwa--create-uuid)
            :session-id (niwa-session-session-id session)
            :turn (if niwa--current-request
                      (niwa-current-turn session)
                    (or (niwa-session-turn-count session) 0))
            :proposal proposal :changes changes
            :before (mapcar (lambda (change)
                              (niwa-tool-patch--snapshot (plist-get change :path)))
                            changes)))))

(defun niwa-note-change--base-extent (note record)
  "Return NOTE's zero-based line extent in RECORD's base coordinates."
  (with-current-buffer (overlay-buffer note)
    (save-restriction
      (widen)
      (when-let* ((before (cl-find buffer-file-name (plist-get record :before)
                                  :key (lambda (item) (plist-get item :path)) :test #'equal)))
        (let* ((start (1- (line-number-at-pos (overlay-start note))))
               (end (line-number-at-pos (max (overlay-start note) (1- (overlay-end note)))))
               (bytes (encode-coding-string
                       (buffer-substring-no-properties (point-min) (point-max))
                       buffer-file-coding-system))
               (base (or (plist-get before :bytes) ""))
               (start-offset 0) (end-offset 0) touched)
          (unless (equal bytes base)
            (dolist (run (niwa-change--runs bytes base))
              (pcase-let ((`(,a ,b ,c ,d) run))
                (when (<= b start)
                  (cl-incf start-offset (- (- d c) (- b a))))
                (when (<= b end)
                  (cl-incf end-offset (- (- d c) (- b a))))
                (when (niwa-change--overlap-p start end a b)
                  (push run touched)))))
          (list :path buffer-file-name
                :start-line (apply #'min (+ start start-offset)
                                   (mapcar (lambda (run) (nth 2 run)) touched))
                :end-line (apply #'max (+ end end-offset)
                                 (mapcar (lambda (run) (nth 3 run)) touched))))))))

(defun niwa-note-change--touches-p (note record)
  "Return non-nil when RECORD changes NOTE's marked source lines."
  (with-current-buffer (overlay-buffer note)
    (save-restriction
      (widen)
      (when-let* ((change (cl-find buffer-file-name (plist-get record :changes)
                                  :key (lambda (item) (plist-get item :path)) :test #'equal)))
        (or (eq 'delete (plist-get change :action))
            (let* ((before (cl-find buffer-file-name (plist-get record :before)
                                    :key (lambda (item) (plist-get item :path)) :test #'equal))
                   (current (niwa-note-change--source-snapshot before))
                   (after (plist-put
                           (copy-sequence before) :bytes
                           (or (plist-get change :bytes)
                               (encode-coding-string (plist-get change :content) 'utf-8-unix))))
                   (prepared (niwa-change-prepare before after current))
                   (runs (or (plist-get prepared :ranges)
                             (if (plist-get prepared :conflicts)
                                 (list (list 0 (line-number-at-pos (point-max))))
                               (niwa-change--runs
                                (or (plist-get current :bytes) "")
                                (or (plist-get (plist-get prepared :after) :bytes) "")))))
                   (start (1- (line-number-at-pos (overlay-start note))))
                   (end (line-number-at-pos (max (overlay-start note) (1- (overlay-end note))))))
              (cl-some (lambda (run)
                         (niwa-change--overlap-p start end (nth 0 run) (nth 1 run)))
                       runs)))))))

(defun niwa-note-prepare-change (proposal changes data-buffer)
  "Hold PROPOSAL and CHANGES for DATA-BUFFER's notes without writing source.
Return an ordinary tool result immediately; no tool callback is retained."
  (let* ((session (buffer-local-value 'niwa--session data-buffer))
         (workspace (niwa-session-workspace session))
         (id (niwa-session-session-id session))
         (notes (cl-remove-if-not
                 (lambda (entry)
                   (equal id (plist-get (plist-get entry :properties)
                                        'niwa-note-session-id)))
                 (niwa-notes-open workspace)))
         (record (niwa-note-change-record proposal changes data-buffer)))
    (unless notes (error "No open note owns this conversation"))
    (setq notes (niwa-notes-resolve-entries notes workspace))
    ;; New-file-only work still belongs to the requesting conversation.
    (setq notes (or (cl-remove-if-not
                     (lambda (note) (niwa-note-change--touches-p note record)) notes)
                    notes))
    (setq record (plist-put record :note-extents
                            (delq nil (mapcar (lambda (note)
                                                (niwa-note-change--base-extent note record))
                                              notes))))
    (with-current-buffer data-buffer
      (when niwa--current-request
        (let ((snapshots (niwa-request-file-snapshots niwa--current-request)))
          (dolist (before (plist-get record :before))
            (let ((path (plist-get before :path)))
              (when (equal (gethash path snapshots 'missing) (plist-get before :bytes))
                (remhash path snapshots)))))))
    (niwa-note-hold-change notes record)
    (niwa-instruction-store-flush t)
    (let* ((result (niwa-tool-patch-result proposal changes))
           (text (plist-get result :result)))
      (plist-put result :result
                 (concat "Prepared change ready; source files are unchanged. "
                         "The user can apply, review, or merge later.\n\n" text))
      result)))

(defun niwa-note-change--note (&optional note)
  "Return live NOTE, the source note at point, or this conversation's note."
  (let* ((data (if (buffer-live-p niwa--data-buffer) niwa--data-buffer (current-buffer)))
         (session (buffer-local-value 'niwa--session data))
         (workspace (and session (niwa-session-workspace session))))
    (setq note
          (or note
              (niwa--highest-priority-instruction (niwa--instructions-at (point) 'note))
              (when session
                (when-let* ((entries (cl-remove-if-not
                                     (lambda (entry)
                                       (equal (niwa-session-session-id session)
                                              (plist-get (plist-get entry :properties)
                                                         'niwa-note-session-id)))
                                     (niwa-notes-open workspace)))
                            (entry (or (cl-find-if
                                        (lambda (entry)
                                          (plist-get (plist-get entry :properties)
                                                     'niwa-note-prepared-change)) entries)
                                       (car entries))))
                  (niwa-note-resolve (plist-get entry :uuid) workspace)))))
    (unless (and (overlayp note) (overlay-buffer note)
                 (eq (overlay-get note 'niwa-instruction-type) 'note))
      (user-error "No live note here"))
    note))

(defun niwa-note-change--record (note)
  "Return NOTE's waiting change, or refuse."
  (or (overlay-get note 'niwa-note-prepared-change)
      (user-error "No prepared change for this note")))

(defun niwa-note-change--source-snapshot (disk)
  "Return DISK's snapshot with visiting source text, including unsaved edits."
  (let ((snapshot (copy-sequence disk)))
    (when-let* ((buffer (find-buffer-visiting (plist-get disk :path)))
                ((buffer-modified-p buffer)))
      (with-current-buffer buffer
        (unless (verify-visited-file-modtime buffer)
          (user-error "File changed on disk; reconcile unsaved edits before applying or reverting %s"
                      (plist-get disk :path)))
        (save-restriction
          (widen)
          (when (unencodable-char-position (point-min) (point-max) buffer-file-coding-system)
            (user-error "Save %s first: its coding cannot write some characters"
                        (plist-get disk :path)))
          (setq snapshot (plist-put snapshot :exists t))
          (setq snapshot
                (plist-put snapshot :bytes
                           (encode-coding-string
                            (buffer-substring-no-properties (point-min) (point-max))
                            buffer-file-coding-system))))))
    snapshot))

(defun niwa-note-change--plan (note &optional selected)
  "Prepare NOTE's waiting changes against current source, without writes.
When SELECTED is non-nil, prepare only those reviewed changes."
  (let ((record (niwa-note-change--record note))
        changes conflicts)
    (when (or (eq 'stale (niwa-notes-attention-state note))
              (cl-some
               (lambda (entry)
                 (let ((props (plist-get entry :properties)))
                   (and (equal (plist-get record :id)
                               (plist-get (plist-get props 'niwa-note-prepared-change) :id))
                        (eq 'stale (plist-get props 'niwa-note-attention-state)))))
               (niwa-notes-open
                (niwa--instruction-buffer-workspace (overlay-buffer note)))))
      (user-error "Change stale; ask in the conversation or merge"))
    (dolist (before (plist-get record :before))
      (let* ((path (plist-get before :path))
             (extents (cl-remove-if-not
                       (lambda (extent) (equal path (plist-get extent :path)))
                       (plist-get record :note-extents))))
        (when extents
          (let ((current (niwa-note-change--source-snapshot
                          (niwa-tool-patch--snapshot path))))
            (cond
             ((not (eq (plist-get before :exists) (plist-get current :exists)))
              (push (list :path path :start-line 1 :end-line 1) conflicts))
             ((not (equal (plist-get before :bytes) (plist-get current :bytes)))
              (dolist (run (niwa-change--runs (or (plist-get before :bytes) "")
                                               (or (plist-get current :bytes) "")))
                (when (cl-some (lambda (extent)
                                 (niwa-change--overlap-p
                                  (plist-get extent :start-line) (plist-get extent :end-line)
                                  (nth 0 run) (nth 1 run))) extents)
                  (push (list :path path :start-line (1+ (nth 2 run))
                              :end-line (max (1+ (nth 2 run)) (nth 3 run))) conflicts)))))))))
    (dolist (change (or selected (plist-get record :changes)))
      (let* ((path (plist-get change :path))
             (before (cl-find path (plist-get record :before)
                              :key (lambda (snapshot) (plist-get snapshot :path)) :test #'equal))
             (after (list :path path :exists (eq (plist-get change :action) 'write)
                          :bytes (when (eq (plist-get change :action) 'write)
                                   (or (plist-get change :bytes)
                                       (encode-coding-string (plist-get change :content) 'utf-8-unix)))
                          :mode (when (eq (plist-get change :action) 'write)
                                  (or (plist-get change :mode) (plist-get before :mode)
                                      (default-file-modes)))))
             (disk (niwa-tool-patch--snapshot path))
             (current (niwa-note-change--source-snapshot disk))
             (prepared (niwa-change-prepare before after current)))
        (setq conflicts (append conflicts (plist-get prepared :conflicts)))
        (unless (plist-get prepared :conflicts)
          (let* ((result (plist-get prepared :after))
                 (buffer (find-buffer-visiting path))
                 (coding (if buffer (buffer-local-value 'buffer-file-coding-system buffer)
                           (if (plist-get current :exists)
                               (cdr (niwa-tool-patch--read-file path t)) 'utf-8-unix))))
            (push (list :path path :before disk :history-before current
                        :action (if (plist-get result :exists) 'write 'delete)
                        :bytes (plist-get result :bytes)
                        :content (and (plist-get result :exists)
                                      (decode-coding-string (plist-get result :bytes) coding))
                        :mode (plist-get result :mode)) changes)))))
    (when conflicts
      (niwa-note-mark-stale note)
      (user-error "Change stale at %s; ask in the conversation or merge"
                  (mapconcat (lambda (conflict)
                               (format "%s:%d-%d" (plist-get conflict :path)
                                       (plist-get conflict :start-line)
                                       (plist-get conflict :end-line))) conflicts ", ")))
    (nreverse changes)))

(defun niwa-note-change--commit (note record changes)
  "Commit NOTE's CHANGES and retain their actual pair under RECORD's turn."
  (let* ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note)))
         (id (plist-get record :session-id))
         (data (or (niwa-session-control-transfer-root-buffer-for-id id)
                   (niwa-session-persistence-resume-id workspace id)
                   (user-error "Note conversation is unavailable: %s" id)))
         (session (buffer-local-value 'niwa--session data))
         (turn (plist-get record :turn))
         (foreign (not (equal id (overlay-get note 'niwa-note-session-id)))))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (dolist (change changes)
      (when-let* ((buffer (find-buffer-visiting (plist-get change :path))))
        (with-current-buffer buffer (undo-boundary))))
    (niwa-tool-patch-commit changes)
    (dolist (change changes)
      (when-let* ((buffer (find-buffer-visiting (plist-get change :path))))
        (with-current-buffer buffer (undo-boundary))))
    (niwa-session-artifacts-record-applied
     session turn changes :application-backups foreign
     :then (lambda (files) (niwa-note-change--finish note record files workspace)))))

(defun niwa-note-change--finish (note record files &optional workspace)
  "Record applied RECORD and FILES, then close its conversation's notes.
NOTE identifies the applying suggestion; WORKSPACE survives source deletion."
  (setq workspace (or workspace (niwa--instruction-buffer-workspace (overlay-buffer note))))
  (niwa-note-record-applied record files workspace)
  (niwa-notes-close-applied note (plist-get record :session-id) workspace)
  (niwa-instruction-store-flush t))

(defun niwa-note-change--review (note record)
  "Open the existing detailed review for NOTE's waiting RECORD."
  (require 'niwa-patch-review)
  (let* ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note)))
         (id (plist-get record :session-id))
         (data (or (niwa-session-control-transfer-root-buffer-for-id id)
                   (niwa-session-persistence-resume-id workspace id)
                   (user-error "Note conversation is unavailable: %s" id)))
         (proposal (copy-tree (plist-get record :proposal))))
    (plist-put proposal :prepared-snapshots (copy-tree (plist-get record :before)))
    (dolist (operation (plist-get proposal :operations))
      (when-let* ((before (cl-find (niwa-tool-patch-physical-path operation)
                                  (plist-get proposal :prepared-snapshots) :test #'equal
                                  :key (lambda (snapshot) (plist-get snapshot :path))))
                  ((plist-get before :exists)))
        (plist-put operation :baseline-content
                   (decode-coding-string (plist-get before :bytes) 'undecided))))
    (plist-put proposal :commit
               (lambda (_data selected done _failed)
                 (unless (equal (plist-get record :id)
                                (plist-get (overlay-get note 'niwa-note-prepared-change) :id))
                   (user-error "The waiting change changed; reopen its review"))
                 (niwa-note-change--commit
                  note record (niwa-note-change--plan note selected))
                 (funcall done)))
    (niwa-patch-review-start proposal #'ignore data)
    (if (overlay-get note 'niwa-note-session-id)
        (niwa-note-send note)
      (pop-to-buffer (buffer-local-value 'niwa--view-buffer data)))))

;;;###autoload
(defun niwa-note-apply-change (&optional note review)
  "Apply NOTE's prepared change as one undo step, refusing newer source edits.
With prefix argument REVIEW, open the existing detailed ApplyPatch review.
Confirming a partial selection drops the rest of this waiting change."
  (interactive (list nil current-prefix-arg))
  (setq note (niwa-note-change--note note))
  (let ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note)))
        (uuid (overlay-get note 'niwa-uuid)))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (niwa-instruction-store-observe workspace)
      (setq note (or (niwa-note-resolve uuid workspace)
                     (user-error "Note no longer available: %s" uuid)))
      (let ((record (niwa-note-change--record note))
            (changes (niwa-note-change--plan note)))
        (if review
            (niwa-note-change--review note record)
          (niwa-note-change--commit note record changes)
          (message "niwa: change applied; conversation notes closed"))))))

(defun niwa-note-change--revert-plan (session turn entries)
  "Prepare SESSION's applied TURN ENTRIES for reversal, refusing all conflicts."
  (let (changes conflicts)
    (unless entries (user-error "No applied change at turn %s" turn))
    (dolist (entry entries)
      (let* ((path (car entry))
             (record (cdr entry))
             (disk (niwa-tool-patch--snapshot path))
             (current (niwa-note-change--source-snapshot disk))
             (buffer (find-buffer-visiting path)))
        (when (plist-get record :gap)
          (user-error "Cannot revert uncaptured change at %s: %s"
                      path (plist-get record :gap)))
        (cl-labels ((snapshot (name)
                      (list :path path :exists (and name t)
                            :bytes (and name (niwa-session-artifacts-read-backup session name))
                            ;; File history records bytes, not modes; retain today's mode.
                            :mode (and name (plist-get current :mode)))))
          (let* ((before (snapshot (plist-get record :pre-backup-name)))
                 (after (snapshot (plist-get record :backup-name)))
                 (prepared (if (niwa-change-same-p before current)
                               (list :after current)
                             (niwa-change-prepare before after current t)))
                 (result (plist-get prepared :after)))
            (setq conflicts (append conflicts (plist-get prepared :conflicts)))
            (when (and result (not (niwa-change-same-p result current)))
              (let ((coding (if buffer (buffer-local-value 'buffer-file-coding-system buffer)
                              (if (plist-get current :exists)
                                  (cdr (niwa-tool-patch--read-file path t)) 'utf-8-unix))))
                (push (list :path path :before disk
                            :action (if (plist-get result :exists) 'write 'delete)
                            :bytes (plist-get result :bytes)
                            :content (and (plist-get result :exists)
                                          (decode-coding-string (plist-get result :bytes) coding))
                            :mode (plist-get result :mode)) changes)))))))
    (when conflicts
      (user-error "Cannot revert; changed lines at %s"
                  (mapconcat (lambda (conflict)
                               (format "%s:%d-%d" (plist-get conflict :path)
                                       (plist-get conflict :start-line)
                                       (plist-get conflict :end-line))) conflicts ", ")))
    (nreverse changes)))

(defun niwa-note-change--revert-target (note workspace)
  "Resolve live or closed NOTE in WORKSPACE, prompting without a context note."
  (niwa-notes--load-closed workspace)
  (let* ((entries (append (niwa-notes-open workspace)
                          (mapcar (lambda (entry)
                                    (let* ((instruction (plist-get entry :instruction))
                                           (props (plist-get instruction :properties)))
                                      (list :properties props
                                            :uuid (plist-get props 'niwa-uuid)
                                            :closed-reason (plist-get instruction :closed-reason))))
                                  (niwa-notes-closed workspace t))))
         (at-point (niwa--highest-priority-instruction
                    (niwa--instructions-at (point) 'note)))
         (data (if (buffer-live-p niwa--data-buffer) niwa--data-buffer (current-buffer)))
         (session (buffer-local-value 'niwa--session data))
         (id (or (if (overlayp note) (overlay-get note 'niwa-uuid) note)
                 (and at-point (overlay-get at-point 'niwa-uuid))
                 (when session
                   (plist-get
                    (cl-find (niwa-session-session-id session) entries
                             :key (lambda (entry)
                                    (plist-get (plist-get entry :properties) 'niwa-note-session-id))
                             :test #'equal) :uuid))
                 (let ((choices (mapcar
                                 (lambda (entry)
                                   (let ((props (plist-get entry :properties)))
                                     (cons (format "%s: %s%s" (plist-get props 'niwa-id)
                                                   (plist-get props 'niwa-note-text)
                                                   (if (plist-get entry :closed-reason)
                                                       " (closed)" ""))
                                           (plist-get entry :uuid)))) entries)))
                   (unless choices (user-error "No notes here"))
                   (cdr (assoc (completing-read "Revert change of note: " choices nil t) choices))))))
    (or (cl-find id entries :test #'equal
                 :key (lambda (entry)
                        (if (numberp id) (plist-get (plist-get entry :properties) 'niwa-id)
                          (plist-get entry :uuid))))
        (user-error "No note %s" id))))

;;;###autoload
(defun niwa-note-revert-change (&optional note turn workspace)
  "Revert one applied TURN of NOTE without rewinding its conversation.
NOTE is an overlay, display number or UUID in WORKSPACE; default to the source
or conversation note, or prompt.  When TURN is omitted, select from
the recorded applied turns.  Refuse if affected lines changed.  Reopen a done
note only after success; do not change the conversation or its history."
  (interactive)
  (setq workspace
        (or workspace
            (when (and (overlayp note) (not (overlay-buffer note)))
              (when-let* ((id (or (overlay-get note 'niwa-note-session-id)
                                  (plist-get (car (overlay-get note 'niwa-note-applied-changes))
                                             :session-id)))
                          (data (niwa-session-control-transfer-root-buffer-for-id id))
                          (session (buffer-local-value 'niwa--session data)))
                (niwa-session-workspace session)))
            (niwa--instruction-buffer-workspace
             (if (and (overlayp note) (overlay-buffer note))
                 (overlay-buffer note) (current-buffer)))))
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
    (setq note (or note (niwa--highest-priority-instruction
                        (niwa--instructions-at (point) 'note))))
    (when (overlayp note)
      (setq note (overlay-get note 'niwa-uuid)))
    (niwa-instruction-store-observe workspace)
    (let* ((target (niwa-note-change--revert-target note workspace))
           (props (plist-get target :properties))
           (own-id (plist-get props 'niwa-note-session-id))
           (own-data (and own-id
                          (or (niwa-session-control-transfer-root-buffer-for-id own-id)
                              (niwa-session-persistence-resume-id workspace own-id))))
           (pairs (copy-sequence (plist-get props 'niwa-note-applied-changes))))
      (when own-data
        (dolist (entry (niwa-session-file-snapshots
                        (buffer-local-value 'niwa--session own-data)))
          (when (cdr entry)
            (cl-pushnew (list :session-id own-id :turn (car entry)) pairs :test #'equal))))
      (when turn
        (setq pairs (cl-remove-if-not (lambda (pair) (= turn (plist-get pair :turn))) pairs)))
      (unless pairs (user-error "Note has no applied changes"))
      (let* ((choices (mapcar (lambda (pair)
                               (cons (format "%s: turn %d" (plist-get pair :session-id)
                                             (plist-get pair :turn)) pair))
                             (sort pairs (lambda (a b) (> (plist-get a :turn)
                                                          (plist-get b :turn))))))
             (pair (if (null (cdr choices)) (cdar choices)
                     (cdr (assoc (completing-read "Revert applied turn: " choices nil t) choices))))
             (id (plist-get pair :session-id))
             (data (or (niwa-session-control-transfer-root-buffer-for-id id)
                       (niwa-session-persistence-resume-id workspace id)
                       (user-error "The conversation that made this change no longer exists: %s" id)))
             (session (buffer-local-value 'niwa--session data))
             (turn (plist-get pair :turn)))
        (niwa-session-artifacts-assert-new-mutation-authority session)
        (niwa-tool-patch-commit
         (niwa-note-change--revert-plan
          session turn (if (equal own-id id)
                           (cdr (assq turn (niwa-session-file-snapshots session)))
                         (or (plist-get pair :files)
                             (user-error "This note has no captured application files")))))
        (when (eq 'done (plist-get target :closed-reason))
          (niwa-note-restore (plist-get target :uuid) workspace)
          (niwa-instruction-store-flush t))
        (message "niwa: note change reverted")))))

(provide 'niwa-note-change)
;;; niwa-note-change.el ends here
