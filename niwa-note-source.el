;;; niwa-note-source.el -- Durable note source presentations -*- lexical-binding: t -*-

;;; Commentary:

;; Owns note anchors, source presentations and command-scoped replacement.

;;; Code:

(require 'niwa-utilities)

;; `cl-lib' is a runtime dependency, not a compile-time one: these owners
;; call `cl-lib' functions -- `cl-remove', `cl-some', `cl-find' -- and
;; `cl-lib' autoloads none of them.  Loading this module compiled and
;; without the umbrella, as the split-owner contract allows, then hit a
;; void-function; loading it as source only worked because
;; `eval-when-compile' degrades to `progn' in interpreted code.
(require 'cl-lib)

(require 'niwa-instruction-registry)
(require 'niwa-instruction-undo)
(require 'niwa-structs)

;; `niwa-instruction-registry'
(declare-function niwa--create-id "niwa-instruction-registry" ())
(declare-function niwa--instruction-activate-buffer
                  "niwa-instruction-registry" (&optional buffer))
(declare-function niwa--instruction-activate-workspace
                  "niwa-instruction-registry" (&optional workspace))
(declare-function niwa--instruction-buffer-workspace
                  "niwa-instruction-registry" (buffer))
(declare-function niwa--instruction-id
                  "niwa-instruction-registry" (instruction))
(declare-function niwa--instruction-with-uuid
                  "niwa-instruction-registry"
                  (uuid &optional workspace))
(declare-function niwa--instruction-workspace-key
                  "niwa-instruction-registry" (&optional workspace))
(declare-function niwa--retire-id "niwa-instruction-registry" (id))
(defvar niwa--instruction-state-key-override)

;; `niwa-notes'
(declare-function niwa-note-delete "niwa-notes" (&optional note))
(autoload 'niwa-note-delete "niwa-notes")

;; `niwa-overlay-ui'
(declare-function niwa--update-instruction-overlay
                  "niwa-overlay-ui"
                  (instruction &optional update-children))
(autoload 'niwa--update-instruction-overlay "niwa-overlay-ui")

;; `niwa-overlays'
(declare-function niwa--child-instructions
                  "niwa-overlays" (instruction))
(declare-function niwa--instructions-at
                  "niwa-overlays" (point &optional type))
(declare-function niwa--instructions-in
                  "niwa-overlays" (start end &optional type))
(declare-function niwa--parent-instruction
                  "niwa-overlays" (instruction &optional of-type))
(declare-function niwa--topmost-instruction
                  "niwa-overlays" (instruction &optional of-type pred))
(autoload 'niwa--child-instructions "niwa-overlays")
(autoload 'niwa--instructions-at "niwa-overlays")
(autoload 'niwa--instructions-in "niwa-overlays")
(autoload 'niwa--parent-instruction "niwa-overlays")
(autoload 'niwa--topmost-instruction "niwa-overlays")
(defvar niwa--default-instruction-priority)

;; `niwa-persistence'
(declare-function niwa--setup-buffer-hooks
                  "niwa-persistence" (buffer))
(declare-function niwa-instruction-store-sync
                  "niwa-persistence" (workspace))
(declare-function niwa-instruction-store-touch
                  "niwa-persistence" (&optional workspace))
(declare-function niwa-persistence-resolve-instruction-anchor
                  "niwa-persistence"
                  (overlay-start overlay-end anchor parent-range))
(autoload 'niwa--setup-buffer-hooks "niwa-persistence")
(autoload 'niwa-instruction-store-sync "niwa-persistence")
(autoload 'niwa-instruction-store-touch "niwa-persistence")
(autoload 'niwa-persistence-resolve-instruction-anchor
  "niwa-persistence")

;; `niwa-structs'
(defcustom niwa-instruction-anchor-context-chars 160
  "Number of surrounding characters stored in instruction anchors."
  :type 'integer
  :group 'niwa)

(defcustom niwa-instruction-anchor-text-max-chars 8192
  "Maximum selected text size stored directly in instruction anchors.

Selections larger than this are represented by hashes and boundary
context only."
  :type 'integer
  :group 'niwa)

(defvar-local niwa--buffer-source-missing nil
  "Non-nil when the last source observation found this buffer's file missing.")

(defvar-local niwa--pending-instruction-edit nil
  "Instruction geometry and old text captured before a buffer edit.")

(defvar-local niwa--pending-instruction-undo nil
  "Presentation undo token captured before the current edit.")

(defvar niwa--instruction-line-mapping nil
  "Explicit replacement bounds for line instructions during source sync.")

(defvar niwa--instruction-command-active nil
  "Outermost active command's recursion depth, or nil outside commands.")

(defvar niwa--instruction-replacement-buffers nil
  "Buffers with replacement entries awaiting scope completion.")

(defvar-local niwa--instruction-replacement-held nil
  "Non-nil while an editing integration holds the replacement scope open.")

(defvar-local niwa--instruction-replacements nil
  "Full-deletion and partial-edge intents in the current replacement scope.")

(defun niwa-instruction-replacement-hold (&optional buffer)
  "Hold BUFFER's deletion replacement scope across commands.
BUFFER defaults to the current buffer.  Calls are idempotent, not nested.
The caller must end the scope on completion, cancellation, and teardown."
  (with-current-buffer (or buffer (current-buffer))
    (setq niwa--instruction-replacement-held t)))

(defun niwa--instruction-replacement-restore (overlay entry)
  "Restore OVERLAY's pending replacement ENTRY during source undo."
  (overlay-put overlay 'niwa-replacement-pending entry)
  (cl-pushnew entry niwa--instruction-replacements)
  (cl-pushnew (current-buffer) niwa--instruction-replacement-buffers))

(defun niwa--instruction-replacement-forget (overlay)
  "Remove OVERLAY's pending presentation during source undo.
Keep its provenance in the scope ledger until end, even when undo removes
the last pending presentation: redo must never reopen a finished scope."
  (overlay-put overlay 'niwa-replacement-pending nil)
  (overlay-put overlay 'niwa-replacement-growing nil))

(defun niwa--instruction-replacement-finish (entry)
  "Finalize the unrecovered full deletion described by ENTRY."
  (let ((overlay (plist-get entry :overlay)))
    (when (eq entry (overlay-get overlay 'niwa-replacement-growing))
      (overlay-put overlay 'niwa-replacement-growing nil))
    (when (eq entry (overlay-get overlay 'niwa-replacement-pending))
      (overlay-put overlay 'niwa-replacement-pending nil)
      (when (overlay-buffer overlay)
        (if (or (< (overlay-start overlay) (overlay-end overlay))
                (eq (overlay-get overlay 'niwa-instruction-type) 'note))
            ;; A later hook may have rejected the deletion.  Notes also stay
            ;; open at an empty anchor until explicitly closed by the user.
            (progn
              (overlay-put overlay 'evaporate (plist-get entry :evaporate))
              overlay)
          (niwa--delete-instruction overlay))))))

(defun niwa--instruction-replacement-settle-undo ()
  "Finalize pending undo states whose original replacement scope has ended."
  (let (redraw)
    (dolist (entry (copy-sequence niwa--instruction-replacements))
      (when (plist-get entry :closed)
        (push (niwa--instruction-replacement-finish entry) redraw)
        (setq niwa--instruction-replacements
              (delq entry niwa--instruction-replacements))))
    (dolist (overlay redraw)
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (niwa--update-instruction-overlay overlay)))))

(defun niwa--instruction-replacement-close (&optional depth)
  "Finalize current buffer's replacement scopes at DEPTH or deeper.
When DEPTH is nil, close every scope, including explicitly held entries."
  (let (entries retained redraw)
    (dolist (entry niwa--instruction-replacements)
      (if (or (null depth) (>= (plist-get entry :depth) depth))
          (push entry entries)
        (push entry retained)))
    (setq entries (nreverse entries)
          niwa--instruction-replacements (nreverse retained))
    ;; Close recovered entries too: undo must not reopen a finished scope.
    (dolist (entry entries)
      (setf (plist-get entry :closed) t))
    (dolist (entry entries)
      (push (niwa--instruction-replacement-finish entry) redraw))
    ;; Settle the whole tree before any redraw can inspect its neighbors.
    (dolist (overlay redraw)
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (niwa--update-instruction-overlay overlay))))
  (unless niwa--instruction-replacements
    (setq niwa--instruction-replacement-buffers
          (delq (current-buffer) niwa--instruction-replacement-buffers))))

(defun niwa-instruction-replacement-end (&optional buffer)
  "End BUFFER's replacement scope, finalizing any still-empty instructions.
BUFFER defaults to the current buffer.  Recovered spans retain their identity,
shape and links.  Calls are idempotent."
  (with-current-buffer (or buffer (current-buffer))
    (setq niwa--instruction-replacement-held nil)
    (niwa--instruction-replacement-close)))

(defun niwa--instruction-command-begin ()
  "Open replacement inference for the command about to execute."
  (let ((depth (recursion-depth)))
    (when (or (null niwa--instruction-command-active)
              (< depth niwa--instruction-command-active))
      (setq niwa--instruction-command-active depth))))

(defun niwa--instruction-command-end ()
  "Finish unheld replacement scopes, including commands that switched buffers."
  (let ((depth (recursion-depth)))
    (when (and niwa--instruction-command-active
               (<= depth niwa--instruction-command-active))
      (setq niwa--instruction-command-active nil))
    (dolist (buffer niwa--instruction-replacement-buffers)
      (if (buffer-live-p buffer)
          (unless (buffer-local-value 'niwa--instruction-replacement-held buffer)
            (with-current-buffer buffer
              (niwa--instruction-replacement-close depth)))
        (setq niwa--instruction-replacement-buffers
              (delq buffer niwa--instruction-replacement-buffers))))))

(defconst niwa--persisted-instruction-properties
  '(niwa-instruction
    niwa-id
    niwa-uuid
    niwa-instruction-type
    niwa-note-text
    niwa-note-tags
    niwa-note-author
    niwa-note-state
    niwa-note-session-id
    niwa-note-settings
    niwa-note-prepared-change
    niwa-note-applied-changes
    niwa-note-attention-state
    niwa-note-buddy-severity
    niwa-note-buddy-code
    niwa-note-kept-from-buddy
    niwa-span
    niwa-links
    evaporate)
  "Overlay properties that are part of the instruction data model.

Visual and runtime properties such as faces, keymaps, display strings,
markers, and buffers are rebuilt from these values when an instruction
overlay is restored.")

(defun niwa--instruction-serializable-value (value)
  "Return VALUE with transient text properties stripped from strings."
  (cond
   ((stringp value)
    (substring-no-properties value))
   ((consp value)
    (cons (niwa--instruction-serializable-value (car value))
          (niwa--instruction-serializable-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'niwa--instruction-serializable-value value)))
   (t value)))

(defun niwa--instruction-persisted-properties (instruction &optional source-only)
  "Return serializable persisted properties for INSTRUCTION.
SOURCE-ONLY omits note text, history and lifecycle before copying values;
source geometry undo must leave these authored fields on the live note."
  (let ((raw-properties (overlay-properties instruction))
        properties)
    (dolist (prop niwa--persisted-instruction-properties)
      (when (and (memq prop raw-properties)
                 (not (and source-only
                           (string-prefix-p "niwa-note-" (symbol-name prop)))))
        (setq properties
              (plist-put properties prop
                         (niwa--instruction-serializable-value
                          (overlay-get instruction prop))))))
    properties))

(defun niwa--instruction-anchor-substring (start end)
  "Return buffer substring between START and END, without properties."
  (buffer-substring-no-properties
   (max (point-min) start)
   (min (point-max) end)))

(defun niwa--instruction-anchor-for-instruction (instruction)
  "Return a lightweight restore anchor for INSTRUCTION."
  (when-let* ((buffer (overlay-buffer instruction)))
    (with-current-buffer buffer
      (let* ((start (overlay-start instruction))
             (end (overlay-end instruction))
             (bodyless (= start end))
             (length (- end start))
             (parent (niwa--parent-instruction instruction))
             (text (unless bodyless
                     (niwa--instruction-anchor-substring start end)))
             (stored-text (and text
                               (<= (length text)
                                   niwa-instruction-anchor-text-max-chars)
                               text))
             (context niwa-instruction-anchor-context-chars))
        (list :schema 1
              :uuid (overlay-get instruction 'niwa-uuid)
              :parent-uuid (and parent
                                (overlay-get parent 'niwa-uuid))
              :bodyless bodyless
              :text-hash (and text (secure-hash 'sha256 text))
              :text stored-text
              :prefix (niwa--instruction-anchor-substring
                       (- start context) start)
              :suffix (niwa--instruction-anchor-substring
                       end (+ end context))
              :length length)))))

(defun niwa--mark-buffer-source-missing (buffer)
  "Retain BUFFER's open notes when its file is missing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (niwa--instruction-activate-buffer buffer)
      (let ((instructions
             (copy-sequence
              (alist-get buffer (niwa--instruction-alist)))))
        (dolist (instruction instructions)
          (cond
           ((eq (overlay-get instruction 'niwa-instruction-type) 'note)
            ;; The buffer is still editable.  Keep notes tracking its code;
            ;; stashing repeatedly here would duplicate the file entry.
            nil)
           (t (niwa--delete-instruction instruction buffer))))
        (if (alist-get buffer (niwa--instruction-alist))
            (unless niwa--buffer-source-missing
              (setq niwa--buffer-source-missing t)
              (niwa-instruction-store-touch
               (niwa--instruction-buffer-workspace buffer)))
          (setf (niwa--instruction-alist)
                (assq-delete-all buffer (niwa--instruction-alist)))))))
  buffer)

(defun niwa--reconcile-note-sources (workspace)
  "Mark live note buffers in WORKSPACE Source missing when files vanish."
  (when workspace
    (niwa--instruction-activate-workspace workspace)
    (dolist (entry (copy-sequence (niwa--instruction-alist)))
      (when-let* (((bufferp (car entry)))
                  (buffer (car entry))
                  ((buffer-live-p buffer))
                  (file (buffer-file-name buffer))
                  ((not (file-exists-p file))))
        (niwa--mark-buffer-source-missing buffer))))
  workspace)

(defun niwa--instruction-before-change (beg end)
  "Capture local instruction geometry affected by BEG..END."
  (let ((instructions
         (cl-remove-if-not
          (lambda (overlay)
            (and (<= beg (overlay-end overlay))
                 (<= (overlay-start overlay) end)))
          (niwa--instructions-in (max (point-min) (1- beg))
                                    (min (point-max) (1+ end))))))
    (setq niwa--pending-instruction-undo
          (niwa-instruction-undo-capture beg end instructions))
    (setq niwa--pending-instruction-edit
          (when instructions
            (list beg (buffer-substring-no-properties beg end)
                  (mapcar (lambda (overlay)
                            (list overlay (overlay-start overlay)
                                  (overlay-end overlay)))
                          instructions)))))
  (when (and (< beg end) (not undo-in-progress)
             (or niwa--instruction-command-active
                 niwa--instruction-replacement-held))
    (dolist (geometry (nth 2 niwa--pending-instruction-edit))
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
                               (nth 2 niwa--pending-instruction-edit)))))
            (overlay-put overlay 'evaporate nil)
            (niwa--instruction-replacement-restore overlay entry)))))))

(defun niwa--instruction-move-after-edit (overlay start end &optional entry)
  "Move OVERLAY to START and END only when native shifts did not do so.
ENTRY holds its pre-edit geometry for lazily promoting presentation undo."
  (unless (and (overlay-buffer overlay)
               (= start (overlay-start overlay)) (= end (overlay-end overlay)))
    (when entry
      (niwa-instruction-undo-extend niwa--pending-instruction-undo
                                       overlay (nth 1 entry) (nth 2 entry)))
    (move-overlay overlay start end (current-buffer))))

(defun niwa--instruction-normalize-lines (overlay &optional entry)
  "Expand live nonempty line OVERLAY to whole lines.
ENTRY supplies pre-edit geometry when extra movement needs undo restoration."
  (when (and (overlay-buffer overlay)
             (eq (overlay-get overlay 'niwa-span) 'line)
             (< (overlay-start overlay) (overlay-end overlay)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (overlay-start overlay))
        (let ((start (line-beginning-position)))
          (goto-char (overlay-end overlay))
          (niwa--instruction-move-after-edit
           overlay start (if (bolp) (point) (line-end-position)) entry))))))

(defun niwa--instruction-adjust-edit (beg end)
  "Grow intersected instructions and preserve line shape around BEG and END."
  (pcase-let* ((`(,old-beg ,old-text ,entries)
                (prog1 niwa--pending-instruction-edit
                  (setq niwa--pending-instruction-edit nil)))
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
                     (or niwa--instruction-command-active
                         niwa--instruction-replacement-held)
                     (or (and (<= from start) (< start old-end finish))
                         (and (< start from finish) (<= finish old-end))))
            (let* ((edge (if (<= from start) 'front 'rear))
                   (old (overlay-get overlay 'niwa-replacement-growing))
                   (intent (list :overlay overlay :depth (recursion-depth)
                                 :edge (if (and old (not (plist-get old :closed))
                                                (not (eq edge (plist-get old :edge))))
                                           'both edge)
                                 :closed nil)))
              ;; Only a successful effective edge deletion arms replacement;
              ;; rejected edits and unchanged announced context cannot do so.
              (overlay-put overlay 'niwa-replacement-growing intent)
              (push intent niwa--instruction-replacements)
              (cl-pushnew (current-buffer)
                          niwa--instruction-replacement-buffers)))
          (when-let* ((pending (overlay-get overlay 'niwa-replacement-pending))
                      ((not (plist-get pending :covered-by)))
                      ((> new-end from))
                      ((or (and (= start finish from old-end))
                           (and (<= from start) (<= finish old-end)))))
            (niwa--instruction-move-after-edit overlay from new-end entry)
            (overlay-put overlay 'niwa-replacement-pending nil)
            (overlay-put overlay 'evaporate (plist-get pending :evaporate))
            (when (or niwa--instruction-command-active
                      niwa--instruction-replacement-held)
              (overlay-put overlay 'niwa-replacement-growing pending)))
          (when-let* (((or niwa--instruction-command-active
                           niwa--instruction-replacement-held))
                      (growing (overlay-get overlay 'niwa-replacement-growing))
                      ((= from old-end))
                      ((> new-end from))
                      ((pcase (plist-get growing :edge)
                         ('front (= from start))
                         ('rear (= from finish))
                         (_ (or (= from start) (= from finish))))))
            (niwa--instruction-move-after-edit
             overlay start (max new-end (overlay-end overlay)) entry))
          (when (and (< start finish)
                     (if (= from old-end)
                         (< start from finish)
                       (and (< from finish) (< start old-end)))
                     ;; A pure full deletion still evaporates or detaches.
                     (or (> new-end from)
                         (< start from) (> finish old-end)))
            (let ((live (overlay-buffer overlay)))
              (niwa--instruction-move-after-edit
               overlay
               (min from (if live (overlay-start overlay) from))
               (max new-end (if live (overlay-end overlay) new-end)) entry))))
        ;; Opening a line just before a selected trailing newline splits its
        ;; boundary, rather than adding that new line to the instruction.
        (pcase-let ((`(,overlay ,start ,finish) entry))
          (when (and (eq (overlay-get overlay 'niwa-span) 'line)
                     (= from old-end (1- finish))
                     (< start finish) (> new-end from)
                     (eq (char-after from) ?\n)
                     (eq (char-after (1- (+ finish (- new-length old-length)))) ?\n))
            (niwa--instruction-move-after-edit overlay start (1+ from) entry)))
        (niwa--instruction-normalize-lines (car entry) entry)))))

(defun niwa--instruction-after-change (beg end old-length)
  "Redraw instructions affected by BEG and END.
OLD-LENGTH is the actual replaced character count reported by Emacs."
  (when (and niwa--pending-instruction-edit
             (not (and (= beg (car niwa--pending-instruction-edit))
                       (= old-length (length (nth 1 niwa--pending-instruction-edit))))))
    ;; A rejected before-change hook can suppress the next before notification.
    ;; Its old capture must not turn an unrelated insertion into replacement.
    (setq niwa--pending-instruction-edit nil
          niwa--pending-instruction-undo nil))
  (let ((entries (nth 2 niwa--pending-instruction-edit)))
    (when niwa--pending-instruction-edit
      (niwa--instruction-adjust-edit beg end))
    (dolist (mapping niwa--instruction-line-mapping)
      (niwa--instruction-move-after-edit
       (nth 0 mapping) (nth 1 mapping) (nth 2 mapping)
       (assq (nth 0 mapping) entries))))
  (let ((instructions (niwa--instructions-in
                       (max (point-min) (1- beg))
                       (min (point-max) (1+ end)))))
    (dolist (instruction instructions)
      (unless (overlay-get instruction 'niwa-replacement-pending)
        (niwa--update-instruction-overlay instruction)))
    (niwa-instruction-undo-record
     (prog1 niwa--pending-instruction-undo
       (setq niwa--pending-instruction-undo nil)) instructions)))

(defun niwa--instruction-initialize-overlay (overlay)
  "Initialize edit tracking and inferred shape for restored OVERLAY."
  (with-current-buffer (overlay-buffer overlay)
    (unless (overlay-get overlay 'niwa-span)
      (overlay-put
       overlay 'niwa-span
       (save-excursion
         (goto-char (overlay-start overlay))
         (if (and (< (overlay-start overlay) (overlay-end overlay))
                  (bolp)
                  (progn (goto-char (overlay-end overlay)) (or (bolp) (eolp))))
             'line 'char))))
    (niwa--instruction-normalize-lines overlay)
    (unless (bound-and-true-p niwa--after-change-functions-hooked)
      (setq-local niwa--after-change-functions-hooked t)
      ;; Run before delete-selection's pre-command deletion.  The global end
      ;; hook also settles source edits made by a command in another buffer.
      (add-hook 'pre-command-hook #'niwa--instruction-command-begin -90)
      (add-hook 'post-command-hook #'niwa--instruction-command-end 90)
      (add-hook 'change-major-mode-hook #'niwa-instruction-replacement-end -90 t)
      (add-hook 'kill-buffer-hook #'niwa-instruction-replacement-end -90 t)
      (add-hook 'before-change-functions #'niwa--instruction-before-change nil t)
      (add-hook 'after-change-functions #'niwa--instruction-after-change nil t))
    overlay))

(defun niwa--create-instruction-overlay-in (buffer start end &optional span)
  "Create an instruction in BUFFER from START to END.
SPAN is `line' or `char'; when nil, infer it from the selection bounds."
  (make-local-variable 'niwa--after-change-functions-hooked)
  (niwa--instruction-activate-buffer buffer)
  (with-current-buffer buffer
    (let ((overlay (make-overlay start end (current-buffer) t nil)))
      (overlay-put overlay 'niwa-span span)
      (niwa--instruction-initialize-overlay overlay)
      (overlay-put overlay 'niwa-instruction t)
      (overlay-put overlay 'niwa-id (niwa--create-id))
      (unless (overlay-get overlay 'niwa-uuid)
        (overlay-put overlay 'niwa-uuid (niwa--create-uuid)))
      (push overlay (alist-get buffer (niwa--instruction-alist)))
      (niwa--setup-buffer-hooks buffer)
      overlay)))

(defun niwa--create-uuid ()
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

(defun niwa--instruction-store-write (buffer)
  "Write BUFFER's workspace instructions to its store after a creation.
A creation is written at once, so it survives an Emacs exit before any
session saves.  A failure is reported and retried by the next flush."
  (when-let* ((workspace (niwa--instruction-buffer-workspace buffer)))
    (condition-case err
        (niwa-instruction-store-sync workspace)
      (error
       (niwa-instruction-store-touch workspace)
       (niwa-diagnostic-message (niwa-diagnostic-scope workspace)
                "%s (instruction not yet written to the store)"
                (error-message-string err))))))

(defun niwa--retire-instruction-identity (id workspace)
  "Retire ID and remove its live and durable links in WORKSPACE."
  (when id
    (let ((niwa--instruction-state-key-override
           (niwa--instruction-workspace-key workspace)))
      (cl-labels ((prune (links)
                    (when (or (memq id (plist-get links :from))
                              (memq id (plist-get links :to)))
                      (list :from (remq id (plist-get links :from))
                            :to (remq id (plist-get links :to))))))
        (niwa--retire-id id)
        (niwa--foreach-instruction instruction
          when (overlay-buffer instruction)
          do (when-let* ((links (prune (overlay-get instruction 'niwa-links))))
               (overlay-put instruction 'niwa-links links)
               (niwa--update-instruction-overlay instruction)))))))

(cl-defun niwa--delete-instruction (instruction &optional buffer forget-note)
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
  (when (overlay-get instruction 'niwa-marked-for-deletion)
    (error "Instruction %s already marked for deletion" instruction))
  (when (and (not forget-note)
             (buffer-live-p (overlay-buffer instruction))
             (eq (overlay-get instruction 'niwa-instruction-type) 'note))
    (niwa-note-delete instruction)
    (cl-return-from niwa--delete-instruction instruction))
  (let* ((instruction-buffer
          (or (overlay-buffer instruction) buffer (current-buffer)))
         (workspace
          (niwa--instruction-buffer-workspace instruction-buffer)))
    (niwa--instruction-activate-workspace workspace)
    (overlay-put instruction 'niwa-marked-for-deletion t)
    (let ((ov-buffer (overlay-buffer instruction)))
      (when (buffer-live-p ov-buffer)
        (let ((children (niwa--child-instructions instruction)))
          (niwa--delete-instruction-overlay instruction)
          (dolist (child children)
            (niwa--update-instruction-overlay child t)))))
    (let ((id (niwa--instruction-id instruction)))
      (with-current-buffer instruction-buffer
        (niwa--retire-instruction-identity id workspace)
        (when (eq (overlay-get instruction 'niwa-instruction-type) 'note)
          ;; Old mentions must not redirect to another note after retraction.
          (setf (niwa--instruction-retired-ids)
                (delq id (niwa--instruction-retired-ids))))))
    (niwa-instruction-store-touch workspace)
    (setf (cdr (assoc instruction-buffer (niwa--instruction-alist)))
          (delq instruction
                (cdr (assoc instruction-buffer
                            (niwa--instruction-alist))))))
  instruction)

(provide 'niwa-note-source)
;;; niwa-note-source.el ends here
