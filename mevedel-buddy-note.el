;;; mevedel-buddy-note.el -- Buddy-authored workspace notes -*- lexical-binding: t -*-

;;; Commentary:

;; Buddy remarks are ordinary workspace notes with Buddy as author.  The
;; workspace instruction store owns their durable identity and source anchor;
;; the common note renderer owns their labels.  This module maintains a
;; transient review index, validates captured-buffer tools, and syncs changes
;; once at the review boundary.  Buddy never edits source or curated memory.

;;; Code:

(require 'mevedel-utilities)
(require 'mevedel-notes)
(require 'mevedel-persistence)

(eval-when-compile
  (require 'cl-lib))
(require 'seq)

;; `gptel'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-buddy'
(declare-function mevedel-buddy--changes-for-scope "mevedel-buddy" (scope-key))
(declare-function mevedel-buddy--guide-payload "mevedel-buddy" (beg end))
(declare-function mevedel-buddy--payload-lines "mevedel-buddy" (payload))
(declare-function mevedel-buddy--scope-key "mevedel-buddy" ())

;; `mevedel-notes'
(declare-function mevedel-note-close "mevedel-notes" (note reason))
(declare-function mevedel-notes-closed "mevedel-notes" (&optional workspace loaded-only))

;;
;;; Customization

(defcustom mevedel-buddy-note-serialize-limit 40
  "How many notes at most are described to the model in one request.

An unbounded set would make background prompt cost grow with the entire
workspace.  This bounds model context, not the number of open remarks."
  :type 'natnum
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-note-read-limit 200
  "Positive maximum lines in a taken section or one `read_buffer' call.

Both bounds of a read are required, so this only caps how much one call
may ask for.  A background feature that fires on an idle timer must not
have its per-request cost scale with file size rather than edit size."
  :type '(restricted-sexp
          :tag "Positive integer"
          :match-alternatives
          ((lambda (value) (and (integerp value) (> value 0)))))
  :group 'mevedel-buddy)

(defvar mevedel-buddy-note--notes nil
  "Transient review index rebuilt from the workspace's canonical notes.

Each note is a plist with `:id', `:buffer', `:line', `:note',
`:severity', `:status', and `:overlay'.")

(defvar mevedel-buddy-note--changed-workspaces nil
  "Workspaces changed by the current review, synced once at settlement.")

(defvar mevedel-buddy-note--prepared-buffers nil
  "Buffers whose first-remark workspace setup ran during the current review.")

(defvar mevedel-buddy-note--scope-buffers nil
  "Alist of names and exact buffers the running review may touch.
Nil denies every tool call, including calls after abandonment.")

(defvar mevedel-buddy-note--taken nil
  "Captured taken-note goals and code owned by the current review.")

(defvar mevedel-buddy-note--goals nil
  "Captured authored goals near the current review's shown code.")

(defvar mevedel-buddy-note--level nil
  "Captured automatic dial level; nil means an explicitly requested review.")

(defun mevedel-buddy-note--taken-payload (note shown)
  "Return bounded NOTE code near SHOWN lines or the source cursor."
  (let* ((first (line-number-at-pos (overlay-start note)))
         (last (line-number-at-pos (max (overlay-start note) (1- (overlay-end note)))))
         (limit (max 1 mevedel-buddy-note-read-limit))
         (edit (seq-find
                (lambda (change)
                  (and (equal (buffer-name) (plist-get change :buffer))
                       (< (plist-get change :beg) (overlay-end note))
                       (> (plist-get change :end) (overlay-start note))))
                (mevedel-buddy--changes-for-scope (mevedel-buddy--scope-key))))
         (focus (or (when edit
                      (line-number-at-pos (min (point-max) (plist-get edit :beg))))
                    (seq-find (lambda (line) (<= first line last)) shown)
                    (line-number-at-pos (point))))
         (from (max first (min (1+ (- last limit)) (- focus (/ limit 2)))))
         (to (min last (+ from limit -1))))
    (save-excursion
      (goto-char (point-min)) (forward-line (1- from))
      (let ((beg (max (point) (overlay-start note))))
        (forward-line (- to from))
        (concat
         (when (or (> from first) (< to last))
           (format "Code spans lines %s-%s; use read_buffer for the rest.\n" first last))
         (mevedel-buddy--guide-payload
          beg (min (line-end-position) (overlay-end note))))))))

(defun mevedel-buddy-note-prepare-context (payload &optional level)
  "Add invited goal context to PAYLOAD, capturing automatic dial LEVEL.
Taken notes supply their code and brief.  Active also receives authored goals
within six lines of the edited region; other notes remain out of the prompt."
  (setq mevedel-buddy-note--taken nil
        mevedel-buddy-note--goals nil
        mevedel-buddy-note--level level)
  (let ((shown (mevedel-buddy--payload-lines payload)) sections)
    (dolist (entry mevedel-buddy-note--scope-buffers)
      (with-current-buffer (cdr entry)
        (save-restriction
          (widen)
          (dolist (note (overlays-in (point-min) (point-max)))
            (when (and (eq 'note (overlay-get note 'mevedel-instruction-type))
                       (not (eq 'buddy (overlay-get note 'mevedel-note-author)))
                       (not (string-blank-p (or (overlay-get note 'mevedel-note-text) ""))))
              (let* ((first (line-number-at-pos (overlay-start note)))
                     (last (line-number-at-pos (max (overlay-start note)
                                                    (1- (overlay-end note)))))
                     (nearby (and (eq level 'active)
                                  (seq-some (lambda (line) (<= (- first 6) line (+ last 6)))
                                            (cdr (assoc (car entry) shown))))))
                (when (or (mevedel-note-watching-p note)
                          nearby)
                  (push (list :overlay note :text (overlay-get note 'mevedel-note-text)
                              :taken (not nearby))
                        mevedel-buddy-note--goals)
                  (unless (mevedel-note-watching-p note)
                    (push (format "\nNearby goal, note %s at %s:%s-%s: %s\n"
                                  (overlay-get note 'mevedel-id) (car entry) first last
                                  (replace-regexp-in-string
                                   "[ \t\r\n]+" " " (overlay-get note 'mevedel-note-text))) sections)))))
            (when (mevedel-note-watching-p note)
              (push (list :overlay note
                          :text (overlay-get note 'mevedel-note-text)
                          :code (buffer-substring-no-properties
                                 (overlay-start note) (overlay-end note)))
                    mevedel-buddy-note--taken)
              (push (format "\nTaken note %s. User's brief: %s\n%s"
                            (overlay-get note 'mevedel-id)
                            (replace-regexp-in-string
                             "[ \t\r\n]+" " " (or (overlay-get note 'mevedel-note-text) ""))
                            (mevedel-buddy-note--taken-payload
                             note (cdr (assoc (car entry) shown))))
                    sections))))))
    (concat payload (mapconcat #'identity (nreverse sections) "\n")
            (when level
              (format (concat "\nWorkspace dial: %s. Quiet: only taken-note code. "
                              "Watching: likely mistakes only. Active: ideas only near "
                              "the supplied goals; elsewhere behave like Watching.\n") level)))))

(defun mevedel-buddy-note-review-hash (context)
  "Hash captured goals and taken code together with request CONTEXT."
  (cl-labels ((records (entries)
                (sort
                 (mapcar (lambda (record)
                           (let ((note (plist-get record :overlay)))
                             (list (overlay-get note 'mevedel-uuid)
                                   (overlay-get note 'mevedel-id)
                                   (plist-get record :text)
                                   (plist-get record :code)
                                   (plist-get record :taken))))
                         entries)
                 (lambda (a b) (string< (car a) (car b))))))
    (let ((print-length nil) (print-level nil))
      (secure-hash 'sha256
                   (prin1-to-string
                    (list context (records mevedel-buddy-note--taken)
                          (records mevedel-buddy-note--goals)))))))

(defun mevedel-buddy-note--near-goal-p (buffer position records distance &optional taken)
  "Whether BUFFER's POSITION is within DISTANCE lines of captured RECORDS.
The authored goal must still match.  TAKEN requires the note still be taken."
  (seq-some
   (lambda (record)
     (let ((note (plist-get record :overlay)))
       (and (eq buffer (overlay-buffer note))
            (or (not (or taken (plist-get record :taken)))
                (mevedel-note-watching-p note))
            (equal (plist-get record :text) (overlay-get note 'mevedel-note-text))
            (with-current-buffer buffer
              (save-restriction
                (widen)
                (<= (- (line-number-at-pos (overlay-start note)) distance)
                    (line-number-at-pos position)
                    (+ (line-number-at-pos (max (overlay-start note)
                                               (1- (overlay-end note)))) distance)))))))
   records))

(defun mevedel-buddy-note--admission (buffer position kind)
  "Return a refusal for KIND at BUFFER's POSITION, or nil when invited."
  (cond
   ((not (member kind '(nil "mistake" "idea"))) "Unknown remark kind")
   ((and (eq 'quiet mevedel-buddy-note--level)
         (not (mevedel-buddy-note--near-goal-p
               buffer position mevedel-buddy-note--taken 0 t)))
    "Quiet only watches taken-note code")
   ((and (equal kind "idea") mevedel-buddy-note--level
         (or (not (eq 'active mevedel-buddy-note--level))
             (not (mevedel-buddy-note--near-goal-p
                   buffer position mevedel-buddy-note--goals 6))))
    "Ideas need Active and a nearby known goal")))

(defun mevedel-buddy-note-looks-done (id)
  "Say taken note ID looks done if its captured goal and code are still current.
This is feedback only: the user must explicitly close the note."
  (if-let* ((record (seq-find
                    (lambda (record)
                      (equal (mevedel-buddy-note--integer id)
                             (overlay-get (plist-get record :overlay) 'mevedel-id)))
                    mevedel-buddy-note--taken))
            (note (plist-get record :overlay))
            (buffer (overlay-buffer note))
            ((eq buffer (mevedel-buddy-note--scope-buffer (buffer-name buffer))))
            ((mevedel-note-watching-p note))
            ((equal (plist-get record :text) (overlay-get note 'mevedel-note-text)))
            ((not (string-blank-p (or (plist-get record :text) ""))))
            ((with-current-buffer buffer
               (equal (plist-get record :code)
                      (buffer-substring-no-properties (overlay-start note) (overlay-end note))))))
      (progn
        (overlay-put note 'mevedel-note-buddy-looks-done t)
        (mevedel-notes--render note)
        (format "Note %s looks done; waiting for the user to mark done" id))
    "Taken note or its goal/code changed; review it again"))

(defun mevedel-buddy-note-finish-review ()
  "Persist the current review's whole change set through the checked store."
  (setq mevedel-buddy-note--prepared-buffers nil
        mevedel-buddy-note--taken nil
        mevedel-buddy-note--goals nil
        mevedel-buddy-note--level nil)
  (dolist (record mevedel-buddy-note--notes)
    (unless (plist-get record :overlay)
      (plist-put record :line (mevedel-buddy-note--line record))))
  (let ((workspaces mevedel-buddy-note--changed-workspaces))
    (setq mevedel-buddy-note--changed-workspaces nil)
    (dolist (workspace workspaces)
      (mevedel-instruction-store-touch workspace))
    (when workspaces
      (mevedel-instruction-store-flush))))

(defun mevedel-buddy-note--changed (overlay)
  "Record OVERLAY's workspace for the review's single final store sync."
  (when-let* ((buffer (overlay-buffer overlay))
              ((buffer-local-value 'buffer-file-name buffer))
              (workspace (mevedel--instruction-buffer-workspace buffer)))
    (cl-pushnew workspace mevedel-buddy-note--changed-workspaces)))

(defun mevedel-buddy-note-restore ()
  "Rebuild the review index from canonical notes in its captured buffers."
  (setq mevedel-buddy-note--prepared-buffers nil)
  (let (workspaces)
    (dolist (entry mevedel-buddy-note--scope-buffers)
      (when-let* ((workspace (mevedel--instruction-buffer-workspace (cdr entry))))
        (cl-pushnew workspace workspaces)))
    (setq mevedel-buddy-note--notes
          (seq-filter
           (lambda (record)
             (when-let* ((buffer (mevedel-buddy-note--scope-buffer
                                 (plist-get record :buffer))))
               (and (eq 'dismissed (plist-get record :status))
                    (not (buffer-local-value 'buffer-file-name buffer)))))
           mevedel-buddy-note--notes))
    (dolist (workspace workspaces)
      ;; Workspace activation already hydrated the store.  Indexing must not
      ;; hash a remote store or merge pending changes on each idle review.
      (dolist (entry (mevedel-notes-closed workspace t))
        (let* ((instruction (plist-get entry :instruction))
               (props (plist-get instruction :properties))
               (file (plist-get entry :file))
               (buffer
                (and (eq 'buddy (plist-get props 'mevedel-note-author))
                     (eq 'dismissed (plist-get instruction :closed-reason))
                     (or (cdr (seq-find
                               (lambda (scope)
				 (let ((source (cdr scope)))
				   (or (equal file (buffer-local-value 'buffer-file-name source))
                                       (equal file (buffer-local-value 'buffer-file-truename source)))))
                               mevedel-buddy-note--scope-buffers))
			 ;; Local aliases may need filesystem identity, but a review
			 ;; must not resolve a remote truename or stat its source.
			 (and (not (file-remote-p file))
                              (find-buffer-visiting
                               file (lambda (source)
                                      (rassq source mevedel-buddy-note--scope-buffers))))))))
          (when buffer
            (push (list :id (plist-get props 'mevedel-id)
                        :buffer (buffer-name buffer)
                        :line (car (plist-get instruction :closed-position))
                        :note (plist-get props 'mevedel-note-text)
                        :severity (plist-get props 'mevedel-note-buddy-severity)
                        :status 'dismissed :overlay nil)
                  mevedel-buddy-note--notes))))))
  (dolist (entry mevedel-buddy-note--scope-buffers)
    (with-current-buffer (cdr entry)
      (add-hook 'kill-buffer-hook #'mevedel-buddy-note--on-kill-buffer nil t)
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (or (eq 'buddy (overlay-get overlay 'mevedel-note-author))
                  (overlay-get overlay 'mevedel-note-kept-from-buddy))
          (push (list :id (overlay-get overlay 'mevedel-id)
                      :buffer (buffer-name)
                      :line (line-number-at-pos (overlay-start overlay))
                      :note (overlay-get overlay 'mevedel-note-text)
                      :severity (overlay-get overlay 'mevedel-note-buddy-severity)
                      :status (if (eq 'buddy (overlay-get overlay 'mevedel-note-author))
                                  'active 'kept)
                      :overlay overlay)
                mevedel-buddy-note--notes))))))

(defvar mevedel-buddy-note--markers nil
  "Alist of captured line boundaries for the running review.

Each buffer entry maps a shown line number to advancing start and
non-advancing end markers.  The boundaries distinguish insertion and
replacement from deletion.")

(defconst mevedel-buddy-note--marker-ceiling 2000
  "Marker count above which reads add no new annotatable lines.

Emacs walks a buffer marker list on every insertion, and the user is
typing throughout.  Initial diff and guidance markers are not
truncated, but a review may issue several `read_buffer' calls per round
and several rounds.  Without a ceiling the marker set grows with what
the model asks for rather than with what the user edited.")

(defun mevedel-buddy-note--severities ()
  "Return the accepted severity names, least to most severe."
  '("trivial" "significant" "critical"))

(defun mevedel-buddy-note--integer (value)
  "Return VALUE as an integer, or nil when it does not name one.

A line number arrives as a tool argument, and the model behind Buddy is
often a local one, which sends a JSON number as a string often enough
that refusing the string would refuse real notes."
  (cond ((integerp value) value)
        ((and (stringp value)
              (string-match-p "\\`-?[0-9]+\\'" (string-trim value)))
         (string-to-number (string-trim value)))))

(defun mevedel-buddy-note--find (id)
  "Return the note with ID, or nil."
  (let ((id (if (stringp id) (string-to-number id) id)))
    (seq-find (lambda (note) (equal (plist-get note :id) id))
              mevedel-buddy-note--notes)))

(defun mevedel-buddy-note--line (note)
  "Return NOTE's current line number, following its overlay when live."
  (let ((overlay (plist-get note :overlay)))
    (if (and (overlayp overlay) (overlay-buffer overlay))
        (with-current-buffer (overlay-buffer overlay)
          (save-restriction
            (widen)
            (line-number-at-pos (overlay-start overlay))))
      (or (when-let* ((buffer (mevedel-buddy-note--scope-buffer
                              (plist-get note :buffer)))
                      (position (mevedel-buddy-note--position
                                 buffer (plist-get note :line))))
            (with-current-buffer buffer
              (save-restriction (widen) (line-number-at-pos position))))
          (plist-get note :line)))))


;;
;;; Marker capture

(defun mevedel-buddy-note-capture-markers (shown-lines)
  "Capture markers for SHOWN-LINES, an alist of (BUFFER-NAME . LINES).

A model answers with the line numbers it was shown, but the user keeps
typing while the request is in flight.  Resolving those numbers through
markers taken now means a note lands on the text it describes rather
than wherever that number points by the time the answer arrives.

Only lines the model was shown are marked.  Emacs walks a buffer marker
list on every insertion, so marking every line of a large file would
make typing lag for as long as the request runs."
  (mevedel-buddy-note-release-markers)
  (setq mevedel-buddy-note--markers
        (delq nil
              (mapcar
               (lambda (entry)
                 (when-let* ((buffer (get-buffer (car entry)))
                             ((buffer-live-p buffer)))
                   (cons (car entry)
                         (with-current-buffer buffer
                           (save-excursion
                             (save-restriction
                               (widen)
                               (mapcar
                                (lambda (line)
                                  (goto-char (point-min))
                                  (forward-line (1- (max 1 line)))
                                  (let ((start (point))
                                        (end (line-beginning-position 2)))
                                    (list line
                                          (copy-marker start t)
                                          (copy-marker end))))
                                (cdr entry))))))))
               shown-lines))))

(defun mevedel-buddy-note-release-markers ()
  "Drop the captured line markers so they stop tracking edits."
  (dolist (entry mevedel-buddy-note--markers)
    (dolist (pair (cdr entry))
      (set-marker (nth 1 pair) nil)
      (set-marker (nth 2 pair) nil)))
  (setq mevedel-buddy-note--markers nil))

(defun mevedel-buddy-note--mark-range (buffer from to)
  "Capture markers for lines FROM through TO of BUFFER.

Lines a read exposed can carry notes, so they need the boundaries the
diff's lines were given.  A line already held keeps its original
markers: recapturing one would move an existing note's anchor to
wherever that number points now, which is the failure markers exist to
prevent.

Stop before a line whose numeric key already names different text or
whose marker would exceed `mevedel-buddy-note--marker-ceiling'.  Return
a cons of the last safely marked line and the stop reason, either nil,
`changed', or `ceiling'.  Walk forward once instead of counting from
`point-min' for every line."
  (let* ((name (buffer-name buffer))
         (entry (assoc name mevedel-buddy-note--markers))
         (room (- mevedel-buddy-note--marker-ceiling
                  (apply #'+ (mapcar (lambda (each) (length (cdr each)))
                                     mevedel-buddy-note--markers))))
         (last (1- from))
         (reason nil)
         (new nil))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- (max 1 from)))
          (cl-loop for line from (max 1 from) to to
                   until (eobp)
                   for held = (assq line (cdr entry))
                   for held-position = (and held
                                            (mevedel-buddy-note--position
                                             buffer line))
                   do (cond
                       ((and held
                             (or (not held-position)
                                 (/= line
                                     (line-number-at-pos held-position))))
                        (setq reason 'changed)
                        (cl-return))
                       (held
                        (setq last line))
                       ((<= room 0)
                        (setq reason 'ceiling)
                        (cl-return))
                       (t
                        (push (list line
                                    (copy-marker (point) t)
                                    (copy-marker (line-beginning-position 2)))
                              new)
                        (setq room (1- room)
                              last line)))
                   (forward-line 1)))))
    (when new
      (setq new (nreverse new))
      (if entry
          (setcdr entry (append (cdr entry) new))
        (push (cons name new) mevedel-buddy-note--markers)))
    (cons last reason)))

(defun mevedel-buddy-note--position (buffer line)
  "Return the position in BUFFER for LINE as the model saw it.

Return nil unless a live marker captured for the running review covers
LINE.  Raw line counting is not annotation authority."
  (when-let* ((markers (cdr (assoc (buffer-name buffer)
                                   mevedel-buddy-note--markers)))
              (entry (and (integerp line) (assq line markers)))
              (start (nth 1 entry))
              (end (nth 2 entry))
              ((eq (marker-buffer start) buffer))
              ((eq (marker-buffer end) buffer))
              (start-pos (marker-position start))
              (end-pos (marker-position end))
              ((/= start-pos end-pos)))
    (min start-pos end-pos)))


;;
;;; Overlays

(defun mevedel-buddy-note--code (overlay)
  "Return the current source text marked by OVERLAY, without properties."
  (with-current-buffer (overlay-buffer overlay)
    (save-restriction
      (widen)
      (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))))

(defun mevedel-buddy-note--make-overlay (buffer line note severity)
  "Return a note overlay on LINE of BUFFER showing NOTE at SEVERITY."
  (with-current-buffer buffer
    (when (and buffer-file-name
               (not (memq buffer mevedel-buddy-note--prepared-buffers)))
      (mevedel-notes--ensure-workspace
       (mevedel--instruction-buffer-workspace buffer))
      (push buffer mevedel-buddy-note--prepared-buffers))
    (let* ((position (mevedel-buddy-note--position buffer line))
           (overlay (save-excursion
                      (goto-char position)
                      (mevedel--create-instruction-overlay-in
                       buffer (line-beginning-position) (line-end-position) 'line))))
      (overlay-put overlay 'mevedel-instruction-type 'note)
      (overlay-put overlay 'mevedel-note-author 'buddy)
      (overlay-put overlay 'mevedel-note-state 'unsent)
      (overlay-put overlay 'mevedel-note-text note)
      (overlay-put overlay 'mevedel-note-buddy-severity severity)
      (overlay-put overlay 'mevedel-note-buddy-code (mevedel-buddy-note--code overlay))
      (overlay-put overlay 'evaporate nil)
      (mevedel--update-instruction-overlay overlay t)
      (mevedel-buddy-note--changed overlay)
      (add-hook 'kill-buffer-hook #'mevedel-buddy-note--on-kill-buffer nil t)
      overlay)))

(defun mevedel-buddy-note--on-kill-buffer ()
  "Drop the current buffer's transient review entries as it is killed."
  (mevedel-buddy-note-forget-buffer (buffer-name)))


;;
;;; Tool operations

(defun mevedel-buddy-note--scope-buffer (buffer-name)
  "Return the exact review buffer recorded for BUFFER-NAME."
  (cdr (assoc buffer-name mevedel-buddy-note--scope-buffers)))

(defun mevedel-buddy-note--in-scope-p (buffer-name)
  "Return non-nil when BUFFER-NAME may be touched by the running review."
  (when-let* ((review-buffer (mevedel-buddy-note--scope-buffer buffer-name)))
    (eq review-buffer (get-buffer buffer-name))))

(defun mevedel-buddy-note-add (buffer-name line note severity &optional kind)
  "Attach NOTE to LINE of BUFFER-NAME at SEVERITY.

KIND is the string mistake or idea, defaulting to mistake.
Return the note id, reusing an exact repeat, or an explanatory string."
  (let ((buffer (get-buffer buffer-name))
        (number (mevedel-buddy-note--integer line)))
    (cond
     ((not (mevedel-buddy-note--in-scope-p buffer-name))
      (format "Buffer %s is not in the review scope" buffer-name))
     ((not (buffer-live-p buffer))
      (format "Unknown buffer: %s" buffer-name))
     ((not (mevedel-buddy-note--position buffer number))
      (format "Line %s was not shown in this review" line))
     ((not (member severity (mevedel-buddy-note--severities)))
      (format "Unknown severity: %s" severity))
     ((mevedel-buddy-note--admission
       buffer (mevedel-buddy-note--position buffer number) kind))
     (t
      (let* ((actual-line
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (line-number-at-pos (mevedel-buddy-note--position buffer number)))))
             (existing
              (seq-find (lambda (record)
                          (and (equal buffer-name (plist-get record :buffer))
                               (or (eq 'dismissed (plist-get record :status))
                                   (= actual-line (mevedel-buddy-note--line record)))
                               (equal note (plist-get record :note))))
                        mevedel-buddy-note--notes))
             (overlay (unless existing
                        (mevedel-buddy-note--make-overlay
                         buffer number note severity)))
             (id (if existing (plist-get existing :id)
                   (overlay-get overlay 'mevedel-id))))
        (unless existing
          (push (list :id id
                      :buffer buffer-name
                      :line number
                      :note note
                      :severity severity
                      :status 'active
                      :overlay overlay)
                mevedel-buddy-note--notes))
        id)))))

(defun mevedel-buddy-note-update (id note &optional kind)
  "Replace the text of note ID with NOTE, leaving it where it is.
KIND is the string mistake or idea, defaulting to mistake."
  (if-let* ((record (mevedel-buddy-note--find id)))
      (cond
       ((not (mevedel-buddy-note--in-scope-p (plist-get record :buffer)))
        (format "Note %s is not in the review scope" (plist-get record :id)))
       ((not (mevedel-buddy-note--owned-p record))
        (format "Note %s is no longer owned by this review" id))
       ((let ((overlay (plist-get record :overlay)))
          (mevedel-buddy-note--admission
           (overlay-buffer overlay) (overlay-start overlay) kind)))
       (t
        (let ((overlay (plist-get record :overlay)))
          (plist-put record :note note)
          (when (and (overlayp overlay) (overlay-buffer overlay))
            (overlay-put overlay 'mevedel-note-text note)
            (overlay-put overlay 'mevedel-note-buddy-code
                         (mevedel-buddy-note--code overlay))
            (mevedel--update-instruction-overlay overlay t)
            (mevedel-buddy-note--changed overlay))
          (format "Updated note %s" (plist-get record :id)))))
    (format "Unknown note: %s" id)))

(defun mevedel-buddy-note-remove (id)
  "Retract note ID."
  (if-let* ((record (mevedel-buddy-note--find id)))
      (cond
       ((not (mevedel-buddy-note--in-scope-p (plist-get record :buffer)))
        (format "Note %s is not in the review scope" (plist-get record :id)))
       ((not (mevedel-buddy-note--owned-p record))
        (format "Note %s is no longer owned by this review" id))
       (t
        (mevedel-buddy-note--delete record)
        (format "Removed note %s" (plist-get record :id))))
    (format "Unknown note: %s" id)))

(defun mevedel-buddy-note--owned-p (record)
  "Return non-nil if RECORD still describes an unchanged Buddy-owned note."
  (when-let* ((overlay (plist-get record :overlay))
              ((overlay-buffer overlay)))
    (and (eq 'buddy (overlay-get overlay 'mevedel-note-author))
         (equal (plist-get record :note)
                (overlay-get overlay 'mevedel-note-text)))))

(defun mevedel-buddy-note--delete (record)
  "Delete RECORD's overlay and drop it from the note set."
  (let ((overlay (plist-get record :overlay)))
    (when (and (overlayp overlay) (overlay-buffer overlay))
      (mevedel-buddy-note--changed overlay)
      (let ((mevedel-instruction-store--inhibit t))
        (mevedel--delete-instruction overlay nil t)))
    (setq mevedel-buddy-note--notes (delq record mevedel-buddy-note--notes))))

(defun mevedel-buddy-note--dismiss-overlay (note &optional reason)
  "Close Buddy NOTE as dismissed, retaining REASON in the transient index."
  (unless (eq 'buddy (overlay-get note 'mevedel-note-author))
    (user-error "Not a Buddy remark"))
  (when-let* ((record (seq-find (lambda (record)
                                (eq note (plist-get record :overlay)))
                              mevedel-buddy-note--notes)))
    ;; Keep :line in review coordinates; its captured marker follows edits.
    (plist-put record :overlay nil)
    (plist-put record :status 'dismissed)
    (plist-put record :dismissed-reason (or reason "user")))
  (if (buffer-local-value 'buffer-file-name (overlay-buffer note))
      (mevedel-note-close note 'dismissed)
    ;; An unsaved buffer has no file anchor for recently-closed persistence.
    ;; Its rejected advice remains only in the existing transient index.
    (let ((mevedel-instruction-store--inhibit t))
      (mevedel--delete-instruction note nil t))))

(defun mevedel-buddy-note-forget-buffer (buffer-name)
  "Drop transient review entries belonging to BUFFER-NAME, not stored notes."
  (setq mevedel-buddy-note--notes
        (seq-remove (lambda (record)
                      (equal (plist-get record :buffer) buffer-name))
                    mevedel-buddy-note--notes)))


;;
;;; Serialization for the prompt

(defun mevedel-buddy-note-serialize ()
  "Return the note set described for the model, or an empty string.

Only notes belonging to buffers in the running review scope are
described: a review of one project has no business seeing another
project buffer names, line numbers, and note text, and could otherwise
be told to maintain notes it cannot see.

Active and dismissed notes are both described.  Dismissed ones are
labelled so the model can see what the user already rejected instead of
raising it again."
  (let* ((scoped (seq-filter
                  (lambda (record)
                    (mevedel-buddy-note--in-scope-p (plist-get record :buffer)))
                  mevedel-buddy-note--notes))
         (dismissed (lambda (record) (eq 'dismissed (plist-get record :status))))
         (notes (seq-take (append (seq-filter dismissed scoped)
                                  (seq-remove dismissed scoped))
                          mevedel-buddy-note-serialize-limit)))
    (if (null notes)
        ""
      (concat
       "\n\nNotes you have already left.  Active notes are "
       "visible to the user; dismissed notes are not, but you should still "
       "avoid repeating them. Kept notes now belong to the user; do not "
       "update or retract them.\n"
       (mapconcat
        (lambda (record)
          (format "- note_id %s [%s] %s:%s: %s%s"
                  (plist-get record :id)
                  (plist-get record :status)
                  (plist-get record :buffer)
                  (mevedel-buddy-note--line record)
                  (plist-get record :note)
                  (cond
                   ((eq (plist-get record :status) 'dismissed)
                    (format " (dismissed: %s)"
                            (or (plist-get record :dismissed-reason)
                                "unknown")))
                   ((when-let* ((overlay (plist-get record :overlay))
                                ((overlay-buffer overlay)))
                      (not (equal (overlay-get overlay 'mevedel-note-buddy-code)
                                  (mevedel-buddy-note--code overlay))))
                    (concat " (the annotated line changed since this note was"
                            " written; decide whether to update or remove it)"))
                   (t ""))))
        notes
        "\n")))))


;;
;;; Buffer reading

(defun mevedel-buddy-note-read-buffer (buffer-name begin end)
  "Return lines BEGIN through END of BUFFER-NAME, numbered.

Both bounds are required.  An omitted bound that meant the whole buffer
would send a whole file to the provider on an idle timer, for an edit of
one line, through a tool that takes no permission step.

A span longer than `mevedel-buddy-note-read-limit' is truncated rather
than refused: an over-long request then costs the model a shorter answer
instead of a wasted tool round.

The returned lines may carry notes.  Markers are captured for them the
same way the diff's lines were, so a note placed on one still lands on
the text it was written about.  Stop before returning a line whose
marker cannot be retained or whose number already names older review
text."
  (let ((buffer (get-buffer buffer-name))
        (review-buffer (mevedel-buddy-note--scope-buffer buffer-name))
        (from (mevedel-buddy-note--integer begin))
        (to (mevedel-buddy-note--integer end)))
    (cond
     ((not review-buffer)
      (format "Buffer %s is not in the review scope" buffer-name))
     ((not (buffer-live-p buffer))
      (format "Unknown buffer: %s" buffer-name))
     ((not (eq buffer review-buffer))
      (format "Buffer %s is not in the review scope" buffer-name))
     ((not (and from to))
      "Both begin and end are required, as line numbers")
     ((<= mevedel-buddy-note-read-limit 0)
      "The read line limit must be positive")
     ((> from to)
      (format "Line %s comes after line %s" begin end))
     (t
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (let ((last (line-number-at-pos (point-max))))
              (if (> (max 1 from) last)
                  (format "Buffer %s has only %d lines" buffer-name last)
                (let* ((first (max 1 from))
                       (wanted (min to last))
                       (limited (min wanted
                                     (+ first mevedel-buddy-note-read-limit -1)))
                       (marked (mevedel-buddy-note--mark-range
                                buffer first limited))
                       (final (car marked))
                       (reason (cdr marked))
                       (lines nil))
                  (if (< final first)
                      (pcase reason
                        ('changed
                         (format (concat "Line %d no longer names the text "
                                         "shown earlier in this review")
                                 first))
                        (_ "This review cannot retain more annotatable lines"))
                    (goto-char (point-min))
                    (forward-line (1- first))
                    (cl-loop for line from first to final
                             do (push (format "%6d  %s" line
                                              (buffer-substring-no-properties
                                               (line-beginning-position)
                                               (line-end-position)))
                                      lines)
                             (forward-line 1))
                    (concat
                     (string-join (nreverse lines) "\n")
                     (pcase reason
                       ('changed
                        (format (concat "\n\n[Stopped before line %d because "
                                        "its number names older review text.]")
                                (1+ final)))
                       ('ceiling
                        "\n\n[Stopped at the review's annotatable-line limit.]")
                       (_
                        (when (< limited wanted)
                          (format (concat "\n\n[Stopped at the %d line limit."
                                          "  Read again from line %d for more.]")
                                  mevedel-buddy-note-read-limit
                                  (1+ final)))))))))))))))))


;;
;;; Model-facing tools

(defun mevedel-buddy-note--live-tool (currentp function)
  "Wrap FUNCTION so it refuses once CURRENTP reports its review is over.

A request outlives the review that started it, and the buffer allowlist
is repopulated by whichever review runs next, so a straggler's calls
would otherwise pass the scope check and land in that review -- with
`remove_note' deleting its notes, since ids come from one counter."
  (lambda (&rest args)
    (if (funcall currentp)
        (apply function args)
      "This review has ended; its notes are no longer yours to change")))

(defun mevedel-buddy-note-tools (currentp)
  "Return the gptel tools a Buddy review is given.

CURRENTP is called with no arguments and reports whether the review that
owns these tools is still the one in flight.

These are plain gptel tools rather than registry tools: they change only
advisory notes, never source.  Store synchronization happens once after
the review, not once per tool call."
  (require 'gptel)
  (list
   (gptel-make-tool
    :name "read_buffer"
    :function (mevedel-buddy-note--live-tool
               currentp #'mevedel-buddy-note-read-buffer)
    :description
    (concat "Read numbered lines from a buffer in the current review. "
            "Use it when the supplied region leaves a relevant question "
            "unanswered. Both bounds are required. The lines "
            "you read back can carry notes, like the ones in the diff.")
    :args '((:name "buffer_name" :type string
             :description "Name of the buffer to read.")
            (:name "begin" :type integer
             :description "First line to read.")
            (:name "end" :type integer
             :description "Last line to read."))
    :category "buddy")
   (gptel-make-tool
    :name "add_note"
    :function (mevedel-buddy-note--live-tool currentp #'mevedel-buddy-note-add)
    :description
    (concat "Attach one short remark to one line of a buffer, shown to the "
            "user as an overlay. Returns the note_id you need to update or "
            "remove it later.")
    :args '((:name "buffer" :type string
             :description "Name of the buffer to annotate.")
            (:name "line_number" :type integer
             :description "Line to annotate, from the diff or from a read.")
            (:name "note" :type string
             :description "The concise remark.")
            (:name "severity" :type string
             :enum ["trivial" "significant" "critical"]
             :description "How much this matters.")
            (:name "kind" :type string :enum ["mistake" "idea"]
             :description "A likely mistake, or a goal-directed idea (automatic Active only)."))
    :category "buddy")
   (gptel-make-tool
    :name "update_note"
    :function (mevedel-buddy-note--live-tool currentp #'mevedel-buddy-note-update)
    :description
    (concat "Replace the text of a note you left earlier, when it is still "
            "worth making but no longer worded correctly.")
    :args '((:name "note_id" :type integer
             :description "The note_id from the note list.")
            (:name "note" :type string
             :description "Replacement remark text.")
            (:name "kind" :type string :enum ["mistake" "idea"]
             :description "A likely mistake, or a goal-directed idea (automatic Active only)."))
    :category "buddy")
   (gptel-make-tool
    :name "remove_note"
    :function (mevedel-buddy-note--live-tool currentp #'mevedel-buddy-note-remove)
    :description
    (concat "Retract a note you left earlier, because the user addressed it "
            "or it no longer applies.")
    :args '((:name "note_id" :type integer
             :description "The note_id from the note list."))
    :category "buddy")
   (gptel-make-tool
    :name "note_looks_done"
    :function (mevedel-buddy-note--live-tool currentp #'mevedel-buddy-note-looks-done)
    :description "Say a taken note's stated goal looks met. Never closes the note."
    :args '((:name "note_id" :type integer :description "The taken note's number."))
    :category "buddy")))


;;
;;; Commands

;;;###autoload
(defun mevedel-buddy-keep-note (&optional note)
  "Keep Buddy's NOTE as a user note; default to the remark at point."
  (interactive)
  (let ((workspace (mevedel--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (setq note (or note (mevedel-buddy-note--at-point)
                     (user-error "No Buddy remark at point")))
      (unless (eq 'buddy (overlay-get note 'mevedel-note-author))
        (user-error "Not a Buddy remark"))
      (overlay-put note 'mevedel-note-author 'user)
      (overlay-put note 'mevedel-note-kept-from-buddy t)
      (mevedel--update-instruction-overlay note t)
      (when (buffer-local-value 'buffer-file-name (overlay-buffer note))
        (mevedel-instruction-store-touch workspace))
      note)))

(defun mevedel-buddy-note--at-point ()
  "Return a canonical Buddy note at point, independent of the review index."
  (seq-find (lambda (note) (eq 'buddy (overlay-get note 'mevedel-note-author)))
            (mevedel--instructions-at (point) 'note)))

;;;###autoload
(defun mevedel-buddy-dismiss-note (&optional note)
  "Dismiss Buddy NOTE, defaulting to the remark at point.

The note stops being shown, but the model is still told it was
dismissed so it does not raise the same point again."
  (interactive)
  (let ((workspace (mevedel--instruction-buffer-workspace
                    (if note (overlay-buffer note) (current-buffer)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (mevedel-buddy-note--dismiss-overlay
       (or note (mevedel-buddy-note--at-point)
           (user-error "No Buddy remark at point")))
      (message "mevedel: note dismissed"))))

;;;###autoload
(defun mevedel-buddy-dismiss-notes (&optional workspace)
  "Dismiss every Buddy note in WORKSPACE, syncing once.
Default to the current workspace and return the number dismissed."
  (interactive)
  (setq workspace (or workspace (mevedel--instruction-buffer-workspace (current-buffer))))
  (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
    (mevedel-instruction-store-observe workspace)
    (let ((dismissed 0)
          (notes (mevedel-notes--resolve-entries
                  (cl-remove-if-not
                   (lambda (entry)
                     (eq 'buddy (plist-get (plist-get entry :properties) 'mevedel-note-author)))
                   (mevedel-notes--entries workspace))
                  workspace)))
      (unwind-protect
          (dolist (note notes)
            (mevedel-buddy-note--dismiss-overlay note)
            (cl-incf dismissed))
        (when (> dismissed 0) (mevedel-instruction-store-sync workspace)))
      (message "mevedel: %d note%s dismissed"
               dismissed (if (= dismissed 1) "" "s"))
      dismissed)))


(provide 'mevedel-buddy-note)
;;; mevedel-buddy-note.el ends here
