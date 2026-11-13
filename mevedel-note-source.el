;;; mevedel-note-source.el -- Durable note source presentations -*- lexical-binding: t -*-

;;; Commentary:

;; Owns note anchors, source presentations and command-scoped replacement.

;;; Code:

(require 'mevedel-utilities)

;; `cl-lib' is a runtime dependency, not a compile-time one: these owners
;; call `cl-lib' functions -- `cl-remove', `cl-some', `cl-find' -- and
;; `cl-lib' autoloads none of them.  Loading this module compiled and
;; without the umbrella, as the split-owner contract allows, then hit a
;; void-function; loading it as source only worked because
;; `eval-when-compile' degrades to `progn' in interpreted code.
(require 'cl-lib)

(require 'mevedel-instruction-registry)
(require 'mevedel-instruction-undo)
(require 'mevedel-structs)

;; `mevedel-instruction-registry'
(declare-function mevedel--create-id "mevedel-instruction-registry" ())
(declare-function mevedel--instruction-activate-buffer
                  "mevedel-instruction-registry" (&optional buffer))
(declare-function mevedel--instruction-activate-workspace
                  "mevedel-instruction-registry" (&optional workspace))
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-instruction-registry" (buffer))
(declare-function mevedel--instruction-id
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-instruction-registry"
                  (uuid &optional workspace))
(declare-function mevedel--instruction-workspace-key
                  "mevedel-instruction-registry" (&optional workspace))
(declare-function mevedel--retire-id "mevedel-instruction-registry" (id))
(defvar mevedel--instruction-state-key-override)

;; `mevedel-notes'
(declare-function mevedel-note-delete "mevedel-notes" (&optional note))
(autoload 'mevedel-note-delete "mevedel-notes")

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))
(autoload 'mevedel--update-instruction-overlay "mevedel-overlay-ui")

;; `mevedel-overlays'
(declare-function mevedel--child-instructions
                  "mevedel-overlays" (instruction))
(declare-function mevedel--instructions-at
                  "mevedel-overlays" (point &optional type))
(declare-function mevedel--instructions-in
                  "mevedel-overlays" (start end &optional type))
(declare-function mevedel--parent-instruction
                  "mevedel-overlays" (instruction &optional of-type))
(declare-function mevedel--subinstruction-of-p
                  "mevedel-overlays" (sub parent))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))
(autoload 'mevedel--child-instructions "mevedel-overlays")
(autoload 'mevedel--instructions-at "mevedel-overlays")
(autoload 'mevedel--instructions-in "mevedel-overlays")
(autoload 'mevedel--parent-instruction "mevedel-overlays")
(autoload 'mevedel--subinstruction-of-p "mevedel-overlays")
(autoload 'mevedel--topmost-instruction "mevedel-overlays")
(defvar mevedel--default-instruction-priority)

;; `mevedel-persistence'
(declare-function mevedel--setup-buffer-hooks
                  "mevedel-persistence" (buffer))
(declare-function mevedel-instruction-store-sync
                  "mevedel-persistence" (workspace))
(declare-function mevedel-instruction-store-touch
                  "mevedel-persistence" (&optional workspace))
(declare-function mevedel-persistence-resolve-instruction-anchor
                  "mevedel-persistence"
                  (overlay-start overlay-end anchor parent-range))
(autoload 'mevedel--setup-buffer-hooks "mevedel-persistence")
(autoload 'mevedel-instruction-store-sync "mevedel-persistence")
(autoload 'mevedel-instruction-store-touch "mevedel-persistence")
(autoload 'mevedel-persistence-resolve-instruction-anchor
  "mevedel-persistence")

;; `mevedel-structs'
(defcustom mevedel-instruction-anchor-context-chars 160
  "Number of surrounding characters stored in instruction anchors."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-instruction-anchor-text-max-chars 8192
  "Maximum selected text size stored directly in instruction anchors.

Selections larger than this are represented by hashes and boundary
context only."
  :type 'integer
  :group 'mevedel)

(defvar-local mevedel--buffer-source-missing nil
  "Non-nil when the last source observation found this buffer's file missing.")

(defvar-local mevedel--pending-instruction-edit nil
  "Instruction geometry and old text captured before a buffer edit.")

(defvar-local mevedel--pending-instruction-undo nil
  "Presentation undo token captured before the current edit.")

(defvar mevedel--instruction-line-mapping nil
  "Explicit replacement bounds for line instructions during source sync.")

(defvar mevedel--instruction-command-active nil
  "Outermost active command's recursion depth, or nil outside commands.")

(defvar mevedel--instruction-replacement-buffers nil
  "Buffers with replacement entries awaiting scope completion.")

(defvar-local mevedel--instruction-replacement-held nil
  "Non-nil while an editing integration holds the replacement scope open.")

(defvar-local mevedel--instruction-replacements nil
  "Full-deletion and partial-edge intents in the current replacement scope.")

(defun mevedel-instruction-replacement-hold (&optional buffer)
  "Hold BUFFER's deletion replacement scope across commands.
BUFFER defaults to the current buffer.  Calls are idempotent, not nested.
The caller must end the scope on completion, cancellation, and teardown."
  (with-current-buffer (or buffer (current-buffer))
    (setq mevedel--instruction-replacement-held t)))

(defun mevedel--instruction-replacement-restore (overlay entry)
  "Restore OVERLAY's pending replacement ENTRY during source undo."
  (overlay-put overlay 'mevedel-replacement-pending entry)
  (cl-pushnew entry mevedel--instruction-replacements)
  (cl-pushnew (current-buffer) mevedel--instruction-replacement-buffers))

(defun mevedel--instruction-replacement-forget (overlay)
  "Remove OVERLAY's pending presentation during source undo.
Keep its provenance in the scope ledger until end, even when undo removes
the last pending presentation: redo must never reopen a finished scope."
  (overlay-put overlay 'mevedel-replacement-pending nil)
  (overlay-put overlay 'mevedel-replacement-growing nil))

(defun mevedel--instruction-replacement-finish (entry)
  "Finalize the unrecovered full deletion described by ENTRY."
  (let ((overlay (plist-get entry :overlay)))
    (when (eq entry (overlay-get overlay 'mevedel-replacement-growing))
      (overlay-put overlay 'mevedel-replacement-growing nil))
    (when (eq entry (overlay-get overlay 'mevedel-replacement-pending))
      (overlay-put overlay 'mevedel-replacement-pending nil)
      (when (overlay-buffer overlay)
        (if (or (< (overlay-start overlay) (overlay-end overlay))
                (eq (overlay-get overlay 'mevedel-instruction-type) 'note))
            ;; A later hook may have rejected the deletion.  Notes also stay
            ;; open at an empty anchor until explicitly closed by the user.
            (progn
              (overlay-put overlay 'evaporate (plist-get entry :evaporate))
              overlay)
          (mevedel--delete-instruction overlay))))))

(defun mevedel--instruction-replacement-settle-undo ()
  "Finalize pending undo states whose original replacement scope has ended."
  (let (redraw)
    (dolist (entry (copy-sequence mevedel--instruction-replacements))
      (when (plist-get entry :closed)
        (push (mevedel--instruction-replacement-finish entry) redraw)
        (setq mevedel--instruction-replacements
              (delq entry mevedel--instruction-replacements))))
    (dolist (overlay redraw)
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (mevedel--update-instruction-overlay overlay)))))

(defun mevedel--instruction-replacement-close (&optional depth)
  "Finalize current buffer's replacement scopes at DEPTH or deeper.
When DEPTH is nil, close every scope, including explicitly held entries."
  (let (entries retained redraw)
    (dolist (entry mevedel--instruction-replacements)
      (if (or (null depth) (>= (plist-get entry :depth) depth))
          (push entry entries)
        (push entry retained)))
    (setq entries (nreverse entries)
          mevedel--instruction-replacements (nreverse retained))
    ;; Close recovered entries too: undo must not reopen a finished scope.
    (dolist (entry entries)
      (setf (plist-get entry :closed) t))
    (dolist (entry entries)
      (push (mevedel--instruction-replacement-finish entry) redraw))
    ;; Settle the whole tree before any redraw can inspect its neighbors.
    (dolist (overlay redraw)
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (mevedel--update-instruction-overlay overlay))))
  (unless mevedel--instruction-replacements
    (setq mevedel--instruction-replacement-buffers
          (delq (current-buffer) mevedel--instruction-replacement-buffers))))

(defun mevedel-instruction-replacement-end (&optional buffer)
  "End BUFFER's replacement scope, finalizing any still-empty instructions.
BUFFER defaults to the current buffer.  Recovered spans retain their identity,
shape and links.  Calls are idempotent."
  (with-current-buffer (or buffer (current-buffer))
    (setq mevedel--instruction-replacement-held nil)
    (mevedel--instruction-replacement-close)))

(defun mevedel--instruction-command-begin ()
  "Open replacement inference for the command about to execute."
  (let ((depth (recursion-depth)))
    (when (or (null mevedel--instruction-command-active)
              (< depth mevedel--instruction-command-active))
      (setq mevedel--instruction-command-active depth))))

(defun mevedel--instruction-command-end ()
  "Finish unheld replacement scopes, including commands that switched buffers."
  (let ((depth (recursion-depth)))
    (when (and mevedel--instruction-command-active
               (<= depth mevedel--instruction-command-active))
      (setq mevedel--instruction-command-active nil))
    (dolist (buffer mevedel--instruction-replacement-buffers)
      (if (buffer-live-p buffer)
          (unless (buffer-local-value 'mevedel--instruction-replacement-held buffer)
            (with-current-buffer buffer
              (mevedel--instruction-replacement-close depth)))
        (setq mevedel--instruction-replacement-buffers
              (delq buffer mevedel--instruction-replacement-buffers))))))

(defconst mevedel--persisted-instruction-properties
  '(mevedel-instruction
    mevedel-id
    mevedel-uuid
    mevedel-instruction-type
    mevedel-note-text
    mevedel-note-tags
    mevedel-note-author
    mevedel-note-state
    mevedel-note-session-id
    mevedel-note-prepared-change
    mevedel-note-applied-changes
    mevedel-note-attention-state
    mevedel-note-buddy-severity
    mevedel-note-buddy-code
    mevedel-note-kept-from-buddy
    mevedel-span
    mevedel-instruction-collapse-p
    mevedel-links
    evaporate)
  "Overlay properties that are part of the instruction data model.

Visual and runtime properties such as faces, keymaps, display strings,
markers, and buffers are rebuilt from these values when an instruction
overlay is restored.")

(defun mevedel--instruction-serializable-value (value)
  "Return VALUE with transient text properties stripped from strings."
  (cond
   ((stringp value)
    (substring-no-properties value))
   ((consp value)
    (cons (mevedel--instruction-serializable-value (car value))
          (mevedel--instruction-serializable-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'mevedel--instruction-serializable-value value)))
   (t value)))

(defun mevedel--instruction-persisted-properties (instruction &optional source-only)
  "Return serializable persisted properties for INSTRUCTION.
SOURCE-ONLY omits note text, history and lifecycle before copying values;
source geometry undo must leave these authored fields on the live note."
  (let ((raw-properties (overlay-properties instruction))
        properties)
    (dolist (prop mevedel--persisted-instruction-properties)
      (when (and (memq prop raw-properties)
                 (not (and source-only
                           (string-prefix-p "mevedel-note-" (symbol-name prop)))))
        (setq properties
              (plist-put properties prop
                         (mevedel--instruction-serializable-value
                          (overlay-get instruction prop))))))
    properties))

(defun mevedel--instruction-anchor-substring (start end)
  "Return buffer substring between START and END, without properties."
  (buffer-substring-no-properties
   (max (point-min) start)
   (min (point-max) end)))

(defun mevedel--instruction-anchor-for-instruction (instruction)
  "Return a lightweight restore anchor for INSTRUCTION."
  (when-let* ((buffer (overlay-buffer instruction)))
    (with-current-buffer buffer
      (let* ((start (overlay-start instruction))
             (end (overlay-end instruction))
             (bodyless (= start end))
             (length (- end start))
             (parent (mevedel--parent-instruction instruction))
             (text (unless bodyless
                     (mevedel--instruction-anchor-substring start end)))
             (stored-text (and text
                               (<= (length text)
                                   mevedel-instruction-anchor-text-max-chars)
                               text))
             (context mevedel-instruction-anchor-context-chars))
        (list :schema 1
              :uuid (overlay-get instruction 'mevedel-uuid)
              :parent-uuid (and parent
                                (overlay-get parent 'mevedel-uuid))
              :bodyless bodyless
              :text-hash (and text (secure-hash 'sha256 text))
              :text stored-text
              :prefix (mevedel--instruction-anchor-substring
                       (- start context) start)
              :suffix (mevedel--instruction-anchor-substring
                       end (+ end context))
              :length length)))))

(defun mevedel--mark-buffer-source-missing (buffer)
  "Retain BUFFER's open notes when its file is missing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (mevedel--instruction-activate-buffer buffer)
      (let ((instructions
             (copy-sequence
              (alist-get buffer (mevedel--instruction-alist)))))
        (dolist (instruction instructions)
          (cond
           ((eq (overlay-get instruction 'mevedel-instruction-type) 'note)
            ;; The buffer is still editable.  Keep notes tracking its code;
            ;; stashing repeatedly here would duplicate the file entry.
            nil)
           (t (mevedel--delete-instruction instruction buffer))))
        (if (alist-get buffer (mevedel--instruction-alist))
            (unless mevedel--buffer-source-missing
              (setq mevedel--buffer-source-missing t)
              (mevedel-instruction-store-touch
               (mevedel--instruction-buffer-workspace buffer)))
          (setf (mevedel--instruction-alist)
                (assq-delete-all buffer (mevedel--instruction-alist)))))))
  buffer)

(defun mevedel--reconcile-note-sources (workspace)
  "Mark live note buffers in WORKSPACE Source missing when files vanish."
  (when workspace
    (mevedel--instruction-activate-workspace workspace)
    (dolist (entry (copy-sequence (mevedel--instruction-alist)))
      (when-let* (((bufferp (car entry)))
                  (buffer (car entry))
                  ((buffer-live-p buffer))
                  (file (buffer-file-name buffer))
                  ((not (file-exists-p file))))
        (mevedel--mark-buffer-source-missing buffer))))
  workspace)

(defun mevedel--instruction-before-change (beg end)
  "Capture local instruction geometry affected by BEG..END."
  (let ((instructions
         (cl-remove-if-not
          (lambda (overlay)
            (and (<= beg (overlay-end overlay))
                 (<= (overlay-start overlay) end)))
          (mevedel--instructions-in (max (point-min) (1- beg))
                                    (min (point-max) (1+ end))))))
    (setq mevedel--pending-instruction-undo
          (mevedel-instruction-undo-capture beg end instructions))
    (setq mevedel--pending-instruction-edit
          (when instructions
            (list beg (buffer-substring-no-properties beg end)
                  (mapcar (lambda (overlay)
                            (list overlay (overlay-start overlay)
                                  (overlay-end overlay)))
                          instructions)))))
  (when (and (< beg end) (not undo-in-progress)
             (or mevedel--instruction-command-active
                 mevedel--instruction-replacement-held))
    (dolist (geometry (nth 2 mevedel--pending-instruction-edit))
      (pcase-let ((`(,overlay ,start ,finish) geometry))
        (when (and (< start finish) (<= beg start) (<= finish end))
          (let ((entry (list :overlay overlay
                             :evaporate (overlay-get overlay 'evaporate)
                             :depth (recursion-depth) :closed nil
                              :covered-by
                              (cl-some
                               (lambda (other)
                                 (and (not (eq overlay (car other)))
                                      (<= beg (nth 1 other) start)
                                      (<= finish (nth 2 other) end)
                                      (or (< (nth 1 other) start)
                                          (< finish (nth 2 other)))))
                               (nth 2 mevedel--pending-instruction-edit)))))
            (overlay-put overlay 'evaporate nil)
            (mevedel--instruction-replacement-restore overlay entry)))))))

(defun mevedel--instruction-move-after-edit (overlay start end &optional entry)
  "Move OVERLAY to START and END only when native shifts did not do so.
ENTRY holds its pre-edit geometry for lazily promoting presentation undo."
  (unless (and (overlay-buffer overlay)
               (= start (overlay-start overlay)) (= end (overlay-end overlay)))
    (when entry
      (mevedel-instruction-undo-extend mevedel--pending-instruction-undo
                                       overlay (nth 1 entry) (nth 2 entry)))
    (move-overlay overlay start end (current-buffer))))

(defun mevedel--instruction-normalize-lines (overlay &optional entry)
  "Expand live nonempty line OVERLAY to whole lines.
ENTRY supplies pre-edit geometry when extra movement needs undo restoration."
  (when (and (overlay-buffer overlay)
             (eq (overlay-get overlay 'mevedel-span) 'line)
             (< (overlay-start overlay) (overlay-end overlay)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (overlay-start overlay))
        (let ((start (line-beginning-position)))
          (goto-char (overlay-end overlay))
          (mevedel--instruction-move-after-edit
           overlay start (if (bolp) (point) (line-end-position)) entry))))))

(defun mevedel--instruction-adjust-edit (beg end)
  "Grow intersected instructions and preserve line shape around BEG and END."
  (pcase-let* ((`(,old-beg ,old-text ,entries)
                (prog1 mevedel--pending-instruction-edit
                  (setq mevedel--pending-instruction-edit nil)))
               (new-text (buffer-substring-no-properties beg end))
               (prefix 0)
               (old-length (length old-text))
               (new-length (length new-text))
               (suffix 0))
    ;; Some primitives announce a larger region than they actually change.
    ;; Ignore unchanged boundary context, rather than absorbing that context.
    (while (and (< prefix (min old-length new-length))
                (= (aref old-text prefix) (aref new-text prefix)))
      (setq prefix (1+ prefix)))
    (while (and (< suffix (- (min old-length new-length) prefix))
                (= (aref old-text (- old-length suffix 1))
                   (aref new-text (- new-length suffix 1))))
      (setq suffix (1+ suffix)))
    (let ((from (+ old-beg prefix))
          (old-end (+ old-beg old-length (- suffix)))
          (new-end (+ beg new-length (- suffix))))
      (dolist (entry entries)
        (pcase-let ((`(,overlay ,start ,finish) entry))
          (when (and (< from old-end) (= from new-end)
                     (not undo-in-progress)
                     (or mevedel--instruction-command-active
                         mevedel--instruction-replacement-held)
                     (or (and (<= from start) (< start old-end finish))
                         (and (< start from finish) (<= finish old-end))))
            (let* ((edge (if (<= from start) 'front 'rear))
                   (old (overlay-get overlay 'mevedel-replacement-growing))
                   (intent (list :overlay overlay :depth (recursion-depth)
                                 :edge (if (and old (not (plist-get old :closed))
                                                (not (eq edge (plist-get old :edge))))
                                           'both edge)
                                 :closed nil)))
              ;; Only a successful effective edge deletion arms replacement;
              ;; rejected edits and unchanged announced context cannot do so.
              (overlay-put overlay 'mevedel-replacement-growing intent)
              (push intent mevedel--instruction-replacements)
              (cl-pushnew (current-buffer)
                          mevedel--instruction-replacement-buffers)))
          (when-let* ((pending (overlay-get overlay 'mevedel-replacement-pending))
                      ((not (plist-get pending :covered-by)))
                      ((> new-end from))
                      ((or (and (= start finish from old-end))
                           (and (<= from start) (<= finish old-end)))))
            (mevedel--instruction-move-after-edit overlay from new-end entry)
            (overlay-put overlay 'mevedel-replacement-pending nil)
            (overlay-put overlay 'evaporate (plist-get pending :evaporate))
            (when (or mevedel--instruction-command-active
                      mevedel--instruction-replacement-held)
              (overlay-put overlay 'mevedel-replacement-growing pending)))
          (when-let* (((or mevedel--instruction-command-active
                           mevedel--instruction-replacement-held))
                      (growing (overlay-get overlay 'mevedel-replacement-growing))
                      ((= from old-end))
                      ((> new-end from))
                      ((pcase (plist-get growing :edge)
                         ('front (= from start))
                         ('rear (= from finish))
                         (_ (or (= from start) (= from finish))))))
            (mevedel--instruction-move-after-edit
             overlay start (max new-end (overlay-end overlay)) entry))
          (when (and (< start finish)
                     (if (= from old-end)
                         (< start from finish)
                       (and (< from finish) (< start old-end)))
                     ;; A pure full deletion still evaporates or detaches.
                     (or (> new-end from)
                         (< start from) (> finish old-end)))
            (let ((live (overlay-buffer overlay)))
              (mevedel--instruction-move-after-edit
               overlay
               (min from (if live (overlay-start overlay) from))
               (max new-end (if live (overlay-end overlay) new-end)) entry))))
        ;; Opening a line just before a selected trailing newline splits its
        ;; boundary, rather than adding that new line to the instruction.
        (pcase-let ((`(,overlay ,start ,finish) entry))
          (when (and (eq (overlay-get overlay 'mevedel-span) 'line)
                     (= from old-end (1- finish))
                     (< start finish) (> new-end from)
                     (eq (char-after from) ?\n)
                     (eq (char-after (1- (+ finish (- new-length old-length)))) ?\n))
            (mevedel--instruction-move-after-edit overlay start (1+ from) entry)))
        (mevedel--instruction-normalize-lines (car entry) entry)))))

(defun mevedel--instruction-after-change (beg end old-length)
  "Redraw instructions affected by BEG and END.
OLD-LENGTH is the actual replaced character count reported by Emacs."
  (when (and mevedel--pending-instruction-edit
             (not (and (= beg (car mevedel--pending-instruction-edit))
                       (= old-length (length (nth 1 mevedel--pending-instruction-edit))))))
    ;; A rejected before-change hook can suppress the next before notification.
    ;; Its old capture must not turn an unrelated insertion into replacement.
    (setq mevedel--pending-instruction-edit nil
          mevedel--pending-instruction-undo nil))
  (let ((entries (nth 2 mevedel--pending-instruction-edit)))
    (when mevedel--pending-instruction-edit
      (mevedel--instruction-adjust-edit beg end))
    (dolist (mapping mevedel--instruction-line-mapping)
      (mevedel--instruction-move-after-edit
       (nth 0 mapping) (nth 1 mapping) (nth 2 mapping)
       (assq (nth 0 mapping) entries))))
  (let ((instructions (mevedel--instructions-in
                       (max (point-min) (1- beg))
                       (min (point-max) (1+ end)))))
    (dolist (instruction instructions)
      (unless (overlay-get instruction 'mevedel-replacement-pending)
        (mevedel--update-instruction-overlay instruction)))
    (mevedel-instruction-undo-record
     (prog1 mevedel--pending-instruction-undo
       (setq mevedel--pending-instruction-undo nil)) instructions)))

(defun mevedel--instruction-initialize-overlay (overlay)
  "Initialize edit tracking and inferred shape for restored OVERLAY."
  (with-current-buffer (overlay-buffer overlay)
    (unless (overlay-get overlay 'mevedel-span)
      (overlay-put
       overlay 'mevedel-span
       (save-excursion
         (goto-char (overlay-start overlay))
         (if (and (< (overlay-start overlay) (overlay-end overlay))
                  (bolp)
                  (progn (goto-char (overlay-end overlay)) (or (bolp) (eolp))))
             'line 'char))))
    (mevedel--instruction-normalize-lines overlay)
    (unless (bound-and-true-p mevedel--after-change-functions-hooked)
      (setq-local mevedel--after-change-functions-hooked t)
      ;; Run before delete-selection's pre-command deletion.  The global end
      ;; hook also settles source edits made by a command in another buffer.
      (add-hook 'pre-command-hook #'mevedel--instruction-command-begin -90)
      (add-hook 'post-command-hook #'mevedel--instruction-command-end 90)
      (add-hook 'change-major-mode-hook #'mevedel-instruction-replacement-end -90 t)
      (add-hook 'kill-buffer-hook #'mevedel-instruction-replacement-end -90 t)
      (add-hook 'before-change-functions #'mevedel--instruction-before-change nil t)
      (add-hook 'after-change-functions #'mevedel--instruction-after-change nil t))
    overlay))

(defun mevedel--create-instruction-overlay-in (buffer start end &optional span)
  "Create an instruction in BUFFER from START to END.
SPAN is `line' or `char'; when nil, infer it from the selection bounds."
  (make-local-variable 'mevedel--after-change-functions-hooked)
  (mevedel--instruction-activate-buffer buffer)
  (with-current-buffer buffer
    (let ((overlay (make-overlay start end (current-buffer) t nil)))
      (overlay-put overlay 'mevedel-span span)
      (mevedel--instruction-initialize-overlay overlay)
      (overlay-put overlay 'mevedel-instruction t)
      (overlay-put overlay 'mevedel-id (mevedel--create-id))
      (unless (overlay-get overlay 'mevedel-uuid)
        (overlay-put overlay 'mevedel-uuid (mevedel--create-uuid)))
      (push overlay (alist-get buffer (mevedel--instruction-alist)))
      (mevedel--setup-buffer-hooks buffer)
      overlay)))

(defun mevedel--create-uuid ()
  "Generate a random UUID."
  (let ((s (md5 (format "%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        (current-time)
                        (emacs-uptime)
                        (random)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring s 17 20)
            (substring s 20 32))))

(defun mevedel--instruction-store-write (buffer)
  "Write BUFFER's workspace instructions to its store after a creation.
A creation is written at once, so it survives an Emacs exit before any
session saves.  A failure is reported and retried by the next flush."
  (when-let* ((workspace (mevedel--instruction-buffer-workspace buffer)))
    (condition-case err
        (mevedel-instruction-store-sync workspace)
      (error
       (mevedel-instruction-store-touch workspace)
       (mevedel-diagnostic-message (mevedel-diagnostic-scope workspace)
                "%s (instruction not yet written to the store)"
                (error-message-string err))))))

(defun mevedel--retire-instruction-identity (id workspace)
  "Retire ID and remove its live and durable links in WORKSPACE."
  (when id
    (let ((mevedel--instruction-state-key-override
           (mevedel--instruction-workspace-key workspace)))
      (cl-labels ((prune (links)
                    (when (or (memq id (plist-get links :from))
                              (memq id (plist-get links :to)))
                      (list :from (remq id (plist-get links :from))
                            :to (remq id (plist-get links :to))))))
        (mevedel--retire-id id)
        (mevedel--foreach-instruction instruction
          when (overlay-buffer instruction)
          do (when-let* ((links (prune (overlay-get instruction 'mevedel-links))))
               (overlay-put instruction 'mevedel-links links)
               (mevedel--update-instruction-overlay instruction)))))))

(cl-defun mevedel--delete-instruction (instruction &optional buffer forget-note)
  "Delete the INSTRUCTION overlay and return it.

If the overlay is already dead, just perform the cleanup.
BUFFER is required in order to perform cleanup on a dead instruction.
Live notes move to recently closed unless FORGET-NOTE is non-nil.  That
exception is for automatic retraction, not explicit user deletion."
  ;; We want to handle this function in two different ways. The first way
  ;; handles regular deletion, i.e. when the function was invoked on an existing
  ;; instruction. The second way is for when the instruction was deleted
  ;; uncanonically through text manipulation. In the latter case, the function
  ;; will be called during a cleanup routine and the instruction will not be
  ;; alive.
  (when (overlay-get instruction 'mevedel-marked-for-deletion)
    (error "Instruction %s already marked for deletion" instruction))
  (when (and (not forget-note)
             (buffer-live-p (overlay-buffer instruction))
             (eq (overlay-get instruction 'mevedel-instruction-type) 'note))
    (mevedel-note-delete instruction)
    (cl-return-from mevedel--delete-instruction instruction))
  (let* ((instruction-buffer
          (or (overlay-buffer instruction) buffer (current-buffer)))
         (workspace
          (mevedel--instruction-buffer-workspace instruction-buffer)))
    (mevedel--instruction-activate-workspace workspace)
    (overlay-put instruction 'mevedel-marked-for-deletion t)
    (let ((ov-buffer (overlay-buffer instruction)))
      (when (buffer-live-p ov-buffer)
        (let ((children (mevedel--child-instructions instruction)))
          (mevedel--delete-instruction-overlay instruction)
          (dolist (child children)
            (mevedel--update-instruction-overlay child t)))))
    (let ((id (mevedel--instruction-id instruction)))
      (with-current-buffer instruction-buffer
        (mevedel--retire-instruction-identity id workspace)
        (when (eq (overlay-get instruction 'mevedel-instruction-type) 'note)
          ;; Old mentions must not redirect to another note after retraction.
          (setf (mevedel--instruction-retired-ids)
                (delq id (mevedel--instruction-retired-ids))))))
    (mevedel-instruction-store-touch workspace)
    (setf (cdr (assoc instruction-buffer (mevedel--instruction-alist)))
          (delq instruction
                (cdr (assoc instruction-buffer
                            (mevedel--instruction-alist))))))
  instruction)

(provide 'mevedel-note-source)
;;; mevedel-note-source.el ends here
