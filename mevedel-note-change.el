;;; mevedel-note-change.el -- Prepared note changes -*- lexical-binding: t -*-

;;; Commentary:

;; Note-session source patches settle immediately as durable proposals.  User
;; commands later apply them through the normal patch transaction and review.
;; Recorded application pairs support checked reversal without Rewind.

;;; Code:

(require 'cl-lib)
(require 'mevedel-change)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-tool-patch)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-persistence)
(require 'mevedel-session-publication)

(declare-function mevedel-patch-review-start "mevedel-patch-review" (proposal callback data-buffer))

(defun mevedel-note-change-record (proposal changes data-buffer)
  "Capture prepared PROPOSAL and CHANGES owned by DATA-BUFFER's current turn."
  (with-current-buffer data-buffer
    (let ((session mevedel--session)
          (proposal (copy-tree proposal))
          (changes (copy-tree changes)))
      (cl-remf proposal :baseline)
      (dolist (operation (plist-get proposal :operations))
        (cl-remf operation :baseline-content))
      (dolist (change changes)
        (when (plist-get change :bytes)
          (cl-remf change :content)))
      (list :id (mevedel--create-uuid)
            :session-id (mevedel-session-session-id session)
            :turn (if mevedel--current-request
                      (mevedel-current-turn session)
                    (or (mevedel-session-turn-count session) 0))
            :proposal proposal :changes changes
            :before (mapcar (lambda (change)
                              (mevedel-tool-patch--snapshot (plist-get change :path)))
                            changes)))))

(defun mevedel-note-change--base-extent (note record)
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
            (dolist (run (mevedel-change--runs bytes base))
              (pcase-let ((`(,a ,b ,c ,d) run))
                (when (<= b start)
                  (cl-incf start-offset (- (- d c) (- b a))))
                (when (<= b end)
                  (cl-incf end-offset (- (- d c) (- b a))))
                (when (mevedel-change--overlap-p start end a b)
                  (push run touched)))))
          (list :path buffer-file-name
                :start-line (apply #'min (+ start start-offset)
                                   (mapcar (lambda (run) (nth 2 run)) touched))
                :end-line (apply #'max (+ end end-offset)
                                 (mapcar (lambda (run) (nth 3 run)) touched))))))))

(defun mevedel-note-change--touches-p (note record)
  "Return non-nil when RECORD changes NOTE's marked source lines."
  (with-current-buffer (overlay-buffer note)
    (save-restriction
      (widen)
      (when-let* ((change (cl-find buffer-file-name (plist-get record :changes)
                                  :key (lambda (item) (plist-get item :path)) :test #'equal)))
        (or (eq 'delete (plist-get change :action))
            (let* ((before (cl-find buffer-file-name (plist-get record :before)
                                    :key (lambda (item) (plist-get item :path)) :test #'equal))
                   (current (mevedel-note-change--source-snapshot before))
                   (after (plist-put
                           (copy-sequence before) :bytes
                           (or (plist-get change :bytes)
                               (encode-coding-string (plist-get change :content) 'utf-8-unix))))
                   (prepared (mevedel-change-prepare before after current))
                   (runs (or (plist-get prepared :ranges)
                             (if (plist-get prepared :conflicts)
                                 (list (list 0 (line-number-at-pos (point-max))))
                               (mevedel-change--runs
                                (or (plist-get current :bytes) "")
                                (or (plist-get (plist-get prepared :after) :bytes) "")))))
                   (start (1- (line-number-at-pos (overlay-start note))))
                   (end (line-number-at-pos (max (overlay-start note) (1- (overlay-end note))))))
              (cl-some (lambda (run)
                         (mevedel-change--overlap-p start end (nth 0 run) (nth 1 run)))
                       runs)))))))

(defun mevedel-note-prepare-change (proposal changes data-buffer)
  "Hold PROPOSAL and CHANGES for DATA-BUFFER's notes without writing source.
Return an ordinary tool result immediately; no tool callback is retained."
  (let* ((session (buffer-local-value 'mevedel--session data-buffer))
         (workspace (mevedel-session-workspace session))
         (id (mevedel-session-session-id session))
         (notes (cl-remove-if-not
                 (lambda (entry)
                   (equal id (plist-get (plist-get entry :properties)
                                        'mevedel-note-session-id)))
                 (mevedel-notes--entries workspace)))
         (record (mevedel-note-change-record proposal changes data-buffer)))
    (unless notes (error "No open note owns this conversation"))
    (setq notes (mevedel-notes--resolve-entries notes workspace))
    ;; New-file-only work still belongs to the requesting conversation.
    (setq notes (or (cl-remove-if-not
                     (lambda (note) (mevedel-note-change--touches-p note record)) notes)
                    notes))
    (setq record (plist-put record :note-extents
                            (delq nil (mapcar (lambda (note)
                                                (mevedel-note-change--base-extent note record))
                                              notes))))
    (with-current-buffer data-buffer
      (when mevedel--current-request
        (let ((snapshots (mevedel-request-file-snapshots mevedel--current-request)))
          (dolist (before (plist-get record :before))
            (let ((path (plist-get before :path)))
              (when (equal (gethash path snapshots 'missing) (plist-get before :bytes))
                (remhash path snapshots)))))))
    (dolist (note notes)
      (overlay-put note 'mevedel-note-prepared-change record)
      (overlay-put note 'mevedel-note-attention-state 'ready)
      (mevedel-notes--render note))
    (mevedel-instruction-store-touch workspace)
    (mevedel-instruction-store-flush t)
    (let* ((result (mevedel-tool-patch-result proposal changes))
           (text (plist-get result :result)))
      (plist-put result :result
                 (concat "Prepared change ready; source files are unchanged. "
                         "The user can apply, review, or re-ask later.\n\n" text))
      result)))

(defun mevedel-note-change--note (&optional note)
  "Return live NOTE, the source note at point, or this conversation's note."
  (let* ((data (if (buffer-live-p mevedel--data-buffer) mevedel--data-buffer (current-buffer)))
         (session (buffer-local-value 'mevedel--session data))
         (workspace (and session (mevedel-session-workspace session))))
    (setq note
          (or note
              (mevedel--highest-priority-instruction (mevedel--instructions-at (point) 'note))
              (when session
                (when-let* ((entries (cl-remove-if-not
                                     (lambda (entry)
                                       (equal (mevedel-session-session-id session)
                                              (plist-get (plist-get entry :properties)
                                                         'mevedel-note-session-id)))
                                     (mevedel-notes--entries workspace)))
                            (entry (or (cl-find-if
                                        (lambda (entry)
                                          (plist-get (plist-get entry :properties)
                                                     'mevedel-note-prepared-change)) entries)
                                       (car entries))))
                  (mevedel-notes--resolve (plist-get entry :uuid) workspace)))))
    (unless (and (overlayp note) (overlay-buffer note)
                 (eq (overlay-get note 'mevedel-instruction-type) 'note))
      (user-error "No live note here"))
    note))

(defun mevedel-note-change--record (note)
  "Return NOTE's waiting change, or refuse."
  (or (overlay-get note 'mevedel-note-prepared-change)
      (user-error "No prepared change for this note")))

(defun mevedel-note-change--source-snapshot (disk)
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
          (setq snapshot (plist-put snapshot :exists t))
          (setq snapshot
                (plist-put snapshot :bytes
                           (encode-coding-string
                            (buffer-substring-no-properties (point-min) (point-max))
                            buffer-file-coding-system))))))
    snapshot))

(defun mevedel-note-change--plan (note &optional selected)
  "Prepare NOTE's waiting changes against current source, without writes.
When SELECTED is non-nil, prepare only those reviewed changes."
  (let ((record (mevedel-note-change--record note))
        changes conflicts)
    (when (or (eq 'stale (mevedel-notes-attention-state note))
              (cl-some
               (lambda (entry)
                 (let ((props (plist-get entry :properties)))
                   (and (equal (plist-get record :id)
                               (plist-get (plist-get props 'mevedel-note-prepared-change) :id))
                        (eq 'stale (plist-get props 'mevedel-note-attention-state)))))
               (mevedel-notes--entries
                (mevedel--instruction-buffer-workspace (overlay-buffer note)))))
      (user-error "Change stale; re-ask on current code or merge"))
    (dolist (before (plist-get record :before))
      (let* ((path (plist-get before :path))
             (extents (cl-remove-if-not
                       (lambda (extent) (equal path (plist-get extent :path)))
                       (plist-get record :note-extents))))
        (when extents
          (let ((current (mevedel-note-change--source-snapshot
                          (mevedel-tool-patch--snapshot path))))
            (cond
             ((not (eq (plist-get before :exists) (plist-get current :exists)))
              (push (list :path path :start-line 1 :end-line 1) conflicts))
             ((not (equal (plist-get before :bytes) (plist-get current :bytes)))
              (dolist (run (mevedel-change--runs (or (plist-get before :bytes) "")
                                               (or (plist-get current :bytes) "")))
                (when (cl-some (lambda (extent)
                                 (mevedel-change--overlap-p
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
             (disk (mevedel-tool-patch--snapshot path))
             (current (mevedel-note-change--source-snapshot disk))
             (prepared (mevedel-change-prepare before after current)))
        (setq conflicts (append conflicts (plist-get prepared :conflicts)))
        (unless (plist-get prepared :conflicts)
          (let* ((result (plist-get prepared :after))
                 (buffer (find-buffer-visiting path))
                 (coding (if buffer (buffer-local-value 'buffer-file-coding-system buffer)
                           (if (plist-get current :exists)
                               (cdr (mevedel-tool-patch--read-file path t)) 'utf-8-unix))))
            (push (list :path path :before disk :history-before current
                        :action (if (plist-get result :exists) 'write 'delete)
                        :bytes (plist-get result :bytes)
                        :content (and (plist-get result :exists)
                                      (decode-coding-string (plist-get result :bytes) coding))
                        :mode (plist-get result :mode)) changes)))))
    (when conflicts
      (overlay-put note 'mevedel-note-attention-state 'stale)
      (mevedel-notes--render note)
      (user-error "Change stale at %s; re-ask on current code or merge"
                  (mapconcat (lambda (conflict)
                               (format "%s:%d-%d" (plist-get conflict :path)
                                       (plist-get conflict :start-line)
                                       (plist-get conflict :end-line))) conflicts ", ")))
    (nreverse changes)))

(defun mevedel-note-change--commit (note record changes)
  "Commit NOTE's CHANGES and retain their actual pair under RECORD's turn."
  (let* ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note)))
         (id (plist-get record :session-id))
         (data (or (mevedel-session-control-transfer-root-buffer-for-id id)
                   (mevedel-session-persistence-resume-id workspace id)
                   (user-error "Note conversation is unavailable: %s" id)))
         (session (buffer-local-value 'mevedel--session data))
         (turn (plist-get record :turn))
         (previous (cdr (assq turn (mevedel-session-file-snapshots session))))
         (pre (make-hash-table :test #'equal)))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (dolist (change changes)
      (let* ((path (plist-get change :path))
             (old (cdr (assoc path previous)))
             (name (plist-get old :pre-backup-name)))
        (puthash path (cond ((plist-get old :gap) (list :gap (plist-get old :gap)))
                            (old (and name (mevedel-session-artifacts-read-file-raw
                                            (mevedel-session-artifacts-backup-path
                                             (mevedel-session-save-path session) name))))
                            (t (plist-get (or (plist-get change :history-before)
                                             (plist-get change :before)) :bytes))) pre))
      (when-let* ((buffer (find-buffer-visiting (plist-get change :path))))
        (with-current-buffer buffer (undo-boundary))))
    (mevedel-tool-patch-commit changes)
    (dolist (change changes)
      (when-let* ((buffer (find-buffer-visiting (plist-get change :path))))
        (with-current-buffer buffer (undo-boundary))))
    (let ((mevedel-session-artifacts--collecting-critical-artifacts-p
           (mevedel-session-codec-portable-authority-p session))
          (mevedel-session-artifacts--critical-artifacts nil))
      (mevedel-session-artifacts-snapshot-modified session turn pre)
      (when mevedel-session-artifacts--critical-artifacts
        (mevedel-session-publication-publish
         session (nreverse mevedel-session-artifacts--critical-artifacts) t)))
    (let ((cell (assq turn (mevedel-session-file-snapshots session))))
      (setcdr cell (sort (append (cdr cell)
                                (cl-remove-if (lambda (entry)
                                                (not (eq 'absent (gethash (car entry) pre 'absent))))
                                              previous))
                        (lambda (a b) (string< (car a) (car b))))))
    (with-current-buffer data
      (when (and mevedel--current-request (= turn (mevedel-request-turn mevedel--current-request)))
        (maphash (lambda (path bytes)
                   (puthash path bytes (mevedel-request-file-snapshots mevedel--current-request))) pre))
      (let ((mevedel--current-request nil))
        (mevedel-session-artifacts-save session data nil t)))
    (mevedel-note-change--finish note record)))

(defun mevedel-note-change--finish (note record)
  "Retain applied RECORD ownership on NOTE and its batch siblings."
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (dolist (entry (mevedel-notes--entries workspace))
      (when (equal (plist-get record :id)
                   (plist-get (plist-get (plist-get entry :properties)
                                        'mevedel-note-prepared-change) :id))
        (let ((item (mevedel-notes--resolve (plist-get entry :uuid) workspace)))
          (overlay-put item 'mevedel-note-applied-changes
                       (cl-adjoin (list :session-id (plist-get record :session-id)
                                        :turn (plist-get record :turn))
                                  (overlay-get item 'mevedel-note-applied-changes)
                                  :test #'equal))
          (overlay-put item 'mevedel-note-prepared-change nil)
          (overlay-put item 'mevedel-note-attention-state nil)
          (mevedel-notes--render item))))
    (mevedel-instruction-store-touch workspace)
    (mevedel-instruction-store-flush t)))

(defun mevedel-note-change--review (note record)
  "Open the existing detailed review for NOTE's waiting RECORD."
  (require 'mevedel-patch-review)
  (let* ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note)))
         (id (plist-get record :session-id))
         (data (or (mevedel-session-control-transfer-root-buffer-for-id id)
                   (mevedel-session-persistence-resume-id workspace id)
                   (user-error "Note conversation is unavailable: %s" id)))
         (proposal (copy-tree (plist-get record :proposal))))
    (plist-put proposal :prepared-snapshots (copy-tree (plist-get record :before)))
    (dolist (operation (plist-get proposal :operations))
      (when-let* ((before (cl-find (mevedel-tool-patch-physical-path operation)
                                  (plist-get proposal :prepared-snapshots) :test #'equal
                                  :key (lambda (snapshot) (plist-get snapshot :path))))
                  ((plist-get before :exists)))
        (plist-put operation :baseline-content
                   (decode-coding-string (plist-get before :bytes) 'undecided))))
    (plist-put proposal :commit
               (lambda (_data selected done _failed)
                 (unless (equal (plist-get record :id)
                                (plist-get (overlay-get note 'mevedel-note-prepared-change) :id))
                   (user-error "The waiting change changed; reopen its review"))
                 (mevedel-note-change--commit
                  note record (mevedel-note-change--plan note selected))
                 (funcall done)))
    (mevedel-patch-review-start proposal #'ignore data)
    (if (overlay-get note 'mevedel-note-session-id)
        (mevedel-note-send note)
      (pop-to-buffer (buffer-local-value 'mevedel--view-buffer data)))))

;;;###autoload
(defun mevedel-note-apply-change (&optional note review)
  "Apply NOTE's prepared change as one undo step, refusing newer source edits.
With prefix argument REVIEW, open the existing detailed ApplyPatch review.
Confirming a partial selection drops the rest of this waiting change."
  (interactive (list nil current-prefix-arg))
  (setq note (mevedel-note-change--note note))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (let ((record (mevedel-note-change--record note))
            (changes (mevedel-note-change--plan note)))
        (if review
            (mevedel-note-change--review note record)
          (mevedel-note-change--commit note record changes)
          (message "mevedel: change applied; mark the note done when ready"))))))

(defun mevedel-note-change--revert-plan (session turn)
  "Prepare SESSION's applied TURN for reversal, refusing all conflicts."
  (let ((entries (cdr (assq turn (mevedel-session-file-snapshots session))))
        changes conflicts)
    (unless entries (user-error "No applied change at turn %s" turn))
    (dolist (entry entries)
      (let* ((path (car entry))
             (record (cdr entry))
             (disk (mevedel-tool-patch--snapshot path))
             (current (mevedel-note-change--source-snapshot disk))
             (buffer (find-buffer-visiting path)))
        (when (plist-get record :gap)
          (user-error "Cannot revert uncaptured change at %s: %s"
                      path (plist-get record :gap)))
        (cl-labels ((snapshot (name)
                      (list :path path :exists (and name t)
                            :bytes (and name (mevedel-session-artifacts-read-file-raw
                                              (mevedel-session-artifacts-backup-path
                                               (mevedel-session-save-path session) name)))
                            ;; File history records bytes, not modes; retain today's mode.
                            :mode (and name (plist-get current :mode)))))
          (let* ((before (snapshot (plist-get record :pre-backup-name)))
                 (after (snapshot (plist-get record :backup-name)))
                 (prepared (if (mevedel-change-same-p before current)
                               (list :after current)
                             (mevedel-change-prepare before after current t)))
                 (result (plist-get prepared :after)))
            (setq conflicts (append conflicts (plist-get prepared :conflicts)))
            (when (and result (not (mevedel-change-same-p result current)))
              (let ((coding (if buffer (buffer-local-value 'buffer-file-coding-system buffer)
                              (if (plist-get current :exists)
                                  (cdr (mevedel-tool-patch--read-file path t)) 'utf-8-unix))))
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

(defun mevedel-note-change--revert-target (note workspace)
  "Resolve live or closed NOTE in WORKSPACE, prompting without a context note."
  (mevedel-notes--load-closed workspace)
  (let* ((entries (append (mevedel-notes--entries workspace)
                          (mapcar (lambda (entry)
                                    (let* ((instruction (plist-get entry :instruction))
                                           (props (plist-get instruction :properties)))
                                      (list :properties props
                                            :uuid (plist-get props 'mevedel-uuid)
                                            :closed-reason (plist-get instruction :closed-reason))))
                                  (mevedel-notes-closed workspace t))))
         (at-point (mevedel--highest-priority-instruction
                    (mevedel--instructions-at (point) 'note)))
         (data (if (buffer-live-p mevedel--data-buffer) mevedel--data-buffer (current-buffer)))
         (session (buffer-local-value 'mevedel--session data))
         (id (or (if (overlayp note) (overlay-get note 'mevedel-uuid) note)
                 (and at-point (overlay-get at-point 'mevedel-uuid))
                 (when session
                   (plist-get
                    (cl-find (mevedel-session-session-id session) entries
                             :key (lambda (entry)
                                    (plist-get (plist-get entry :properties) 'mevedel-note-session-id))
                             :test #'equal) :uuid))
                 (let ((choices (mapcar
                                 (lambda (entry)
                                   (let ((props (plist-get entry :properties)))
                                     (cons (format "%s: %s%s" (plist-get props 'mevedel-id)
                                                   (plist-get props 'mevedel-note-text)
                                                   (if (plist-get entry :closed-reason)
                                                       " (closed)" ""))
                                           (plist-get entry :uuid)))) entries)))
                   (unless choices (user-error "No notes here"))
                   (cdr (assoc (completing-read "Revert change of note: " choices nil t) choices))))))
    (or (cl-find id entries :test #'equal
                 :key (lambda (entry)
                        (if (numberp id) (plist-get (plist-get entry :properties) 'mevedel-id)
                          (plist-get entry :uuid))))
        (user-error "No note %s" id))))

;;;###autoload
(defun mevedel-note-revert-change (&optional note turn workspace)
  "Revert one applied TURN of NOTE without rewinding its conversation.
NOTE is a live overlay, display number or UUID in WORKSPACE; default to the
source or conversation note, or prompt.  When TURN is omitted, select from
the recorded applied turns.  Refuse if affected lines changed.  Reopen a done
note only after success; do not change the conversation or its history."
  (interactive)
  (setq workspace (or workspace (mevedel--instruction-buffer-workspace
                                 (if (and (overlayp note) (overlay-buffer note))
                                     (overlay-buffer note) (current-buffer)))))
  (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
    (let* ((target (mevedel-note-change--revert-target note workspace))
           (props (plist-get target :properties))
           (own-id (plist-get props 'mevedel-note-session-id))
           (own-data (and own-id
                          (or (mevedel-session-control-transfer-root-buffer-for-id own-id)
                              (mevedel-session-persistence-resume-id workspace own-id))))
           (pairs (copy-sequence (plist-get props 'mevedel-note-applied-changes))))
      (when own-data
        (dolist (entry (mevedel-session-file-snapshots
                        (buffer-local-value 'mevedel--session own-data)))
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
             (data (or (mevedel-session-control-transfer-root-buffer-for-id id)
                       (mevedel-session-persistence-resume-id workspace id)
                       (user-error "The conversation that made this change no longer exists: %s" id)))
             (session (buffer-local-value 'mevedel--session data))
             (turn (plist-get pair :turn)))
        (mevedel-session-artifacts-assert-new-mutation-authority session)
        (mevedel-tool-patch-commit (mevedel-note-change--revert-plan session turn))
        (when (eq 'done (plist-get target :closed-reason))
          (mevedel-note-restore (plist-get target :uuid) workspace)
          (mevedel-instruction-store-flush t))
        (message "mevedel: note change reverted")))))

(provide 'mevedel-note-change)
;;; mevedel-note-change.el ends here
