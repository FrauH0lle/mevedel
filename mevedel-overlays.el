;;; mevedel-overlays.el -- Instruction overlays, tags, IDs, navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Core overlay system for mevedel instructions.  Instructions come in
;; two flavours: references (provide context, tagged for query) and
;; directives (LLM prompts that may query references by tag).
;;
;; Responsibilities: overlay CRUD (create/modify/delete), unique ID
;; management for cross-instruction linking, tag storage and boolean
;; query evaluation (and/or/not operators), visual styling via colour
;; tinting, and buffer navigation between instructions.  The central
;; `mevedel--update-instruction-overlay' helper renders every visible
;; aspect of an overlay (colour, label, priority, linking) based on
;; its current state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mevedel-utilities)

;; `gptel'
(defvar gptel-display-buffer-action)
(defvar gptel--fsm-last)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (fsm))
(declare-function gptel--model-name "ext:gptel-request" (model))
(defvar gptel-model)

;; `mevedel-chat'
(declare-function mevedel--patch-buffer "mevedel-chat" (&optional create workspace))
(declare-function mevedel--active-chat-buffer "mevedel-chat" (&optional workspace))
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch-content))
(defvar mevedel--view-buffer)

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" ())
(defvar mevedel-view--input-marker)

;; `mevedel-persistence'
(declare-function mevedel--restore-file-instructions "mevedel-persistence" (file &optional message))
(declare-function mevedel--setup-buffer-hooks "mevedel-persistence" (buffer))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)

(defcustom mevedel-reference-color
  (face-attribute 'font-lock-constant-face :foreground nil 'default)
  "Color to be used as a tint for reference overlays."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-directive-color
  (face-attribute 'font-lock-keyword-face :foreground nil 'default)
  "Color to be used as a tint for directive overlays."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-directive-processing-color
  (face-attribute 'warning :foreground nil 'default)
  "Color to be used as a tint for directives being processed by the model."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-directive-success-color
  (face-attribute 'success :foreground nil 'default)
  "Color to be used as a tint for directives successfully processed by the model."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-directive-fail-color
  (face-attribute 'error :foreground nil 'default)
  "Color to be used as a tint for directives the model could not process."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-highlighted-instruction-color
  (face-attribute 'highlight :background nil 'default)
  "Color for currently highlighted instructions."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-instruction-bg-tint-intensity 0.15
  "Default intensity for background tinting of instructions."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-instruction-label-tint-intensity 0.25
  "Default intensity for label tinting of instructions."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-highlighted-instruction-tint-intensity 0.25
  "Default intensity for tinting of highlighted instructions."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-subinstruction-tint-coefficient 0.4
  "Coefficient multiplied by tint intensities.

Only applicable to subinstructions, allowing finer control over tinting.

Does not affect the label colors, just the backgrounds."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-empty-tag-query-matches-all t
  "Determine behavior of directives without a tag search query.

If set to t, directives without a specific tag search query will use all
available references.  If set to nil, directives without a search query
will not use any references."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-always-match-untagged-references t
  "Control inclusion of untagged references in directive prompts.

When set to t, untagged references are always incorporated into
directive references, ensuring comprehensive coverage.  When set to nil,
untagged references are ignored unless
`mevedel-empty-tag-query-matches-all' is set to t.

A reference is considered untagged when it has no direct tags.
References can inherit tags from ancestor references and still be
considered untagged."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-include-full-instructions t
  "Control whether instructions are fully included in the prompt.

When set to non-nil, the content of directives and references is
included in the prompt submitted to the LLM.  When nil, only file and
line numbers are included.

Setting this to nil makes the initial prompt shorter but relies on the
LLM to find and read the instructions.  Depending on the model, this
might yield better or worse results."
  :type 'boolean
  :group 'mevedel)

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

(defvar mevedel--default-instruction-priority -99)
(defvar mevedel--highlighted-instruction nil)

(defvar mevedel--instruction-states (make-hash-table :test #'equal)
  "Workspace-keyed instruction state table.

Each value is a plist with keys `:instructions', `:id-counter',
`:id-usage-map', and `:retired-ids'.")

(defvar-local mevedel--instruction-current-state-key nil
  "Workspace key selected for this buffer's instruction operations.")

(defvar mevedel--instruction-state-key-override nil
  "Dynamically bound instruction state key for explicit operations.")

(defun mevedel--instruction-operation-state-key ()
  "Return the explicit or buffer-derived instruction state key."
  (or mevedel--instruction-state-key-override
      (and mevedel--instruction-current-state-key
           (not (eq mevedel--instruction-current-state-key :global))
           mevedel--instruction-current-state-key)
      (when-let* ((workspace
                   (mevedel--instruction-buffer-workspace (current-buffer))))
        (mevedel--instruction-workspace-key workspace))
      mevedel--instruction-current-state-key
      :global))

(defmacro mevedel--instruction-alist ()
  "Return the active workspace's instruction alist as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :instructions))

(defmacro mevedel--instruction-id-counter ()
  "Return the active workspace's instruction ID counter as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :id-counter))

(defmacro mevedel--instruction-id-usage-map ()
  "Return the active workspace's used-ID table as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :id-usage-map))

(defmacro mevedel--instruction-retired-ids ()
  "Return the active workspace's retired IDs as a settable place."
  '(plist-get (mevedel--instruction-state
               (mevedel--instruction-operation-state-key))
              :retired-ids))

(defun mevedel--instruction-alist-value ()
  "Return the active workspace's instruction alist."
  (mevedel--instruction-alist))

(defun mevedel--set-instruction-alist-value (value)
  "Set the active workspace's instruction alist to VALUE."
  (setf (mevedel--instruction-alist) value))

(defconst mevedel--persisted-instruction-properties
  '(mevedel-instruction
    mevedel-id
    mevedel-uuid
    mevedel-instruction-type
    mevedel-instruction-collapse-p
    mevedel-links
    mevedel-reference-tags
    mevedel-commentary
    mevedel-commentary-truncated
    mevedel-directive
    mevedel-directive-truncated
    mevedel-directive-status
    mevedel-directive-fail-reason
    mevedel-directive-action
    mevedel-directive-patch
    mevedel-directive-prefix-tag-query
    mevedel-directive-infix-tag-query-string
    mevedel-subdirective-typename
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

(defun mevedel--instruction-persisted-properties (instruction)
  "Return serializable persisted properties for INSTRUCTION."
  (let ((raw-properties (overlay-properties instruction))
        properties)
    (dolist (prop mevedel--persisted-instruction-properties)
      (when (memq prop raw-properties)
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

(defun mevedel--instruction-workspace-key (&optional workspace)
  "Return the instruction-state key for WORKSPACE."
  (if workspace
      (cons (mevedel-workspace-type workspace)
            (mevedel-workspace-id workspace))
    :global))

(defun mevedel--instruction-buffer-workspace (buffer)
  "Return BUFFER's workspace, or nil when it cannot be resolved."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and (boundp 'mevedel--workspace)
               (bound-and-true-p mevedel--workspace))
          (ignore-errors (mevedel-workspace buffer))))))

(defun mevedel--instruction-state (&optional key)
  "Return instruction state plist for KEY, creating it if needed."
  (let ((key (or key :global)))
    (or (gethash key mevedel--instruction-states)
        (puthash key
                 (list :instructions nil
                       :id-counter 0
                       :id-usage-map (make-hash-table)
                       :retired-ids nil)
                 mevedel--instruction-states))))

(gv-define-setter mevedel--instruction-state (value &optional key)
  `(puthash (or ,key :global) ,value mevedel--instruction-states))

(defun mevedel--instruction-activate-workspace (&optional workspace)
  "Make WORKSPACE's instruction state current."
  (let ((key (mevedel--instruction-workspace-key workspace)))
    (mevedel--instruction-state key)
    (setq mevedel--instruction-current-state-key key))
  mevedel--instruction-current-state-key)

(defun mevedel--instruction-activate-buffer (&optional buffer)
  "Make BUFFER's workspace instruction state current."
  (mevedel--instruction-activate-workspace
   (mevedel--instruction-buffer-workspace (or buffer (current-buffer)))))

(defun mevedel--clear-instruction-state (&optional workspace)
  "Delete all visible instruction overlays in WORKSPACE and clear its state."
  (let ((mevedel--instruction-state-key-override
         (mevedel--instruction-workspace-key workspace)))
    (dolist (entry (mevedel--instruction-alist))
      (when (bufferp (car entry))
        (dolist (instr (cdr entry))
          (when (overlayp instr)
            (delete-overlay instr)))))
    (setf (mevedel--instruction-alist) nil)
    (setq mevedel--highlighted-instruction nil)
    (setf (mevedel--instruction-id-counter) 0)
    (setf (mevedel--instruction-id-usage-map) (make-hash-table))
    (setf (mevedel--instruction-retired-ids) nil)))

(defmacro mevedel--foreach-instruction (binding &rest body)
  "Iterate over `(mevedel--instruction-alist)' with BINDING as the binding.

Executes BODY inside an existing `cl-loop' form, which means that the
macro is expecting for BODY to be written in the `cl-loop' DSL.

BINDING can either be a symbol to bind the instruction to, or a list
where the `car' is the symbol binding and the `cadr' is a buffer.

If the buffer inside BINDING is non-nil, only iterate over the
instructions that are located inside that buffer.

The purpose of this macro is to be able to iterate over instructions
while also making sure that the iterated instructions are valid, i.e.
have an associated buffer to the overlay.

This macro is the preferred way to iterate over instructions, as it
handles all the internal bookkeeping and cleanup."
  (declare (indent 1))
  ;; "bof" stands for "buffer or file".
  (cl-with-gensyms (cons bof specific-buffer)
    (let ((instr (if (listp binding) (car binding) binding)))
      `(cl-labels ((trashp (instr)
                     (and (null (overlay-buffer instr))
                          (not (overlay-get instr 'mevedel-marked-for-deletion))))
                   (clean-alist-entry (cons)
                     (mapc (lambda (instr)
                             (mevedel--delete-instruction instr (car cons)))
                           (cl-remove-if-not #'trashp (cdr cons)))
                     (let ((instrs (cl-remove-if #'trashp (cdr cons))))
                       (setf (cdr cons) instrs))))
         (let ((,specific-buffer ,(if (listp binding) (cadr binding) nil)))
           (mevedel--instruction-activate-workspace
            (if (bufferp ,specific-buffer)
                (mevedel--instruction-buffer-workspace ,specific-buffer)
              (mevedel--instruction-buffer-workspace (current-buffer))))
           (if (null ,specific-buffer)
               (cl-loop for ,cons in (mevedel--instruction-alist)
                        do (let ((,bof (car ,cons)))
                             (if (stringp ,bof) ; bof is a file, restore it.
                                 (mevedel--restore-file-instructions ,bof)
                               (clean-alist-entry ,cons)))) ; bof is a buffer, clean it.
             (when (stringp ,specific-buffer)
               (cl-destructuring-bind (buffer _ _) (mevedel--restore-file-instructions ,specific-buffer)
                 (setq ,specific-buffer buffer)))
             (when-let* ((cons (assoc ,specific-buffer (mevedel--instruction-alist))))
               (clean-alist-entry cons)))
           ;; Remove empty cons cells from the alist.
           (setf (mevedel--instruction-alist) (cl-remove-if (lambda (cons)
                                                       (null (cdr cons)))
                                                     (mevedel--instruction-alist)))
           ;; The instructions alist should now be cleaned of deleted
           ;; instructions.
           (cl-loop for ,instr
                    in (if ,specific-buffer
                           (alist-get ,specific-buffer (mevedel--instruction-alist))
                         (flatten-tree
                          (cl-remove nil
                                     (mapcar (lambda (plist-or-instrs)
                                               (if (plist-get plist-or-instrs :instructions)
                                                   nil ; Plist
                                                 plist-or-instrs))
                                             (mapcar #'cdr (mevedel--instruction-alist))))))
                    ,@body))))))

(defun mevedel-link-instructions (from-list to-list)
  "Link instructions with ids in FROM-LIST to those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (progn
     (mevedel--instruction-activate-buffer)
     (let ((completion-table (mapcar #'number-to-string (hash-table-keys (mevedel--instruction-id-usage-map)))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link to: "
                                             completion-table nil t))))))
  (mevedel--instruction-activate-buffer)
  (cl-labels
      ((update-links (instr-id num-key update-id)
         (let* ((instr (mevedel--instruction-with-id instr-id))
                (links (overlay-get instr 'mevedel-links))
                (ids (plist-get links num-key)))
           (unless (member update-id ids)
             (setq ids (cons update-id ids))
             (overlay-put instr 'mevedel-links (plist-put links num-key ids))
             t))))
    (let ((new-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let* ((from-instr (mevedel--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when (/= from-id to-id)
              (when-let* ((to-instr (mevedel--instruction-with-id to-id)))
                (when (and (update-links from-id :to to-id)
                           (update-links to-id :from from-id))
                  (puthash from-instr t involved-instrs)
                  (puthash to-instr t involved-instrs)
                  (cl-incf new-link-count)))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (mevedel--update-instruction-overlay instr))
      (when (called-interactively-p 'interactive)
        (message "Created %d instruction link%s"
                 new-link-count
                 (if (= new-link-count 1) "" "s"))))))

(defun mevedel-unlink-instructions (from-list to-list)
  "Unlink instructions with ids in FROM-LIST from those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (progn
     (mevedel--instruction-activate-buffer)
     (let ((completion-table (mapcar #'number-to-string (hash-table-keys (mevedel--instruction-id-usage-map)))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink from: "
                                             completion-table nil t))))))
  (mevedel--instruction-activate-buffer)
  (cl-labels
      ((remove-links (instr-id num-key remove-id)
         (let* ((instr (mevedel--instruction-with-id instr-id))
                (links (overlay-get instr 'mevedel-links))
                (ids (plist-get links num-key)))
           (when (member remove-id ids)
             (setq ids (remove remove-id ids))
             (overlay-put instr 'mevedel-links (plist-put links num-key ids))
             t))))
    (let ((removed-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let* ((from-instr (mevedel--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when-let* ((to-instr (mevedel--instruction-with-id to-id)))
              (when (and (remove-links from-id :to to-id)
                         (remove-links to-id :from from-id))
                (puthash from-instr t involved-instrs)
                (puthash to-instr t involved-instrs)
                (cl-incf removed-link-count))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (when (buffer-live-p (overlay-buffer instr))
                    (mevedel--update-instruction-overlay instr)))
      (when (called-interactively-p 'interactive)
        (message "Removed %d instruction link%s"
                 removed-link-count
                 (if (= removed-link-count 1) "" "s"))))))

(defun mevedel-cycle-instructions-at-point (point)
  "Cycle through instructions at POINT, highlighting them.

This command allows for cycling through overlapping instructions at a
point in the buffer and allows one to have better accuracy when
instructions overlap to the point where no other reasonable option is
available."
  (interactive "d")
  (let ((instructions-at-point (mevedel--instructions-at point))
        (original-highlighted-instruction mevedel--highlighted-instruction))
    (cond
     ((null instructions-at-point)
      (setq mevedel--highlighted-instruction nil)
      (when (called-interactively-p 'any)
        (message "No instructions at point")))
     ((or (null mevedel--highlighted-instruction)
          (not (memq mevedel--highlighted-instruction instructions-at-point)))
      (setq mevedel--highlighted-instruction nil)
      (setq mevedel--highlighted-instruction (mevedel--highest-priority-instruction instructions-at-point)))
     (t
      (if-let* ((parent (mevedel--parent-instruction mevedel--highlighted-instruction)))
          (setq mevedel--highlighted-instruction parent)
        (setq mevedel--highlighted-instruction nil))))
    (when mevedel--highlighted-instruction
      (mevedel--update-instruction-overlay mevedel--highlighted-instruction))
    (when original-highlighted-instruction
      (mevedel--update-instruction-overlay original-highlighted-instruction))
    mevedel--highlighted-instruction))

(defun mevedel-modify-directive ()
  "Modify the directive under the point."
  (interactive)
  (when-let* ((directive (mevedel--highest-priority-instruction (mevedel--instructions-at (point) 'directive)
                                                                t)))
    (when (eq (overlay-get directive 'mevedel-directive-status) 'processing)
      (overlay-put directive 'mevedel-directive-status nil))
    (let ((topmost-directive (mevedel--topmost-instruction directive 'directive)))
      (when (eq (overlay-get topmost-directive 'mevedel-directive-status) 'failed)
        (setf (overlay-get topmost-directive 'mevedel-directive-status) nil)
        (mevedel--update-instruction-overlay topmost-directive t)))
    (mevedel--read-directive directive)))

(defun mevedel-modify-reference-commentary ()
  "Modify the reference commentary under the point."
  (interactive)
  (when-let* ((reference (mevedel--highest-priority-instruction (mevedel--instructions-at (point) 'reference)
                                                                t)))
    (mevedel--read-commentary reference)))

(defun mevedel-delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were
deleted.  Throw a user error if no instructions to delete were found."
  (interactive)
  (let ((deleted-count 0))
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (dolist (overlay (mevedel--wholly-contained-instructions (current-buffer) start end))
            (when (overlay-get overlay 'mevedel-instruction)
              (mevedel--delete-instruction overlay)
              (setq deleted-count (1+ deleted-count))))
          (when (> deleted-count 0)
            (deactivate-mark))
          (unless (> deleted-count 0)
            (user-error "No instructions to delete within the selected region")))
      (let ((overlay (mevedel--delete-instruction-at (point))))
        (when overlay
          (setq deleted-count 1))
        (unless overlay
          (user-error "No instruction to delete at point"))))
    (when (> deleted-count 0)
      (message "Deleted %d instruction%s" deleted-count (if (> deleted-count 1) "s" "")))))

(defun mevedel-delete-all-instructions ()
  "Delete all mevedel instructions across all buffers."
  (interactive)
  (mevedel--instruction-activate-buffer)
  (let ((instr-count (length (mevedel--all-instructions))))
    (when (and (called-interactively-p 'any)
               (zerop instr-count))
      (user-error "No instructions to delete"))
    (when (and (called-interactively-p 'any)
               instr-count
               (not (y-or-n-p "Are you sure you want to delete all instructions?")))
      (user-error "Aborted")))
  (let ((buffer-count 0)
        (deleted-instr-count 0))
    (mevedel--foreach-instruction instr
      with buffer-hash = (make-hash-table)
      unless (gethash (overlay-buffer instr) buffer-hash)
      do (progn
           (puthash (overlay-buffer instr) t buffer-hash)
           (cl-incf buffer-count))
      do (progn
           (mevedel--delete-instruction instr)
           (cl-incf deleted-instr-count)))
    (when (not (zerop deleted-instr-count))
      (message "Deleted %d mevedel instruction%s in %d buffer%s"
               deleted-instr-count
               (if (= 1 deleted-instr-count) "" "s")
               buffer-count
               (if (= 1 buffer-count) "" "s"))))
  (mevedel--clear-instruction-state
   (mevedel--instruction-buffer-workspace (current-buffer))))

(defun mevedel-convert-instructions ()
  "Convert instructions between reference and directive type.

If a region is selected, convert all instructions within the region.  If
no region is selected, convert only the highest priority instruction at
point.

Bodyless directives cannot be converted to references.  Attempting to do
so will throw a user error."
  (interactive)
  (let* ((instructions (if (use-region-p)
                           (mevedel--instructions-in (region-beginning)
                                                     (region-end))
                         (cl-remove-if #'null
                                       (list (mevedel--highest-priority-instruction
                                              (mevedel--instructions-at (point))
                                              t)))))
         (num-instructions (length instructions))
         (converted-directives-to-references 0)
         (converted-references-to-directives 0))
    (if (= num-instructions 0)
        (user-error "No instructions to convert")
      (dolist (instr instructions)
        (cond
         ((mevedel--directivep instr)
          (unless (mevedel--bodyless-instruction-p instr)
            (overlay-put instr 'mevedel-instruction-type 'reference)
            (setq converted-directives-to-references (1+ converted-directives-to-references))))
         ((mevedel--referencep instr)
          (overlay-put instr 'mevedel-instruction-type 'directive)
          (setq converted-references-to-directives (1+ converted-references-to-directives)))
         (t
          (user-error "Unknown instruction type")))
        (mevedel--update-instruction-overlay instr t))
      (let ((msg "Converted %d instruction%s")
            (conversion-msgs
             (delq nil
                   (list (when (> converted-directives-to-references 0)
                           (format "%d directive%s to reference%s"
                                   converted-directives-to-references
                                   (if (= converted-directives-to-references 1) "" "s")
                                   (if (= converted-directives-to-references 1) "" "s")))
                         (when (> converted-references-to-directives 0)
                           (format "%d reference%s to directive%s"
                                   converted-references-to-directives
                                   (if (= converted-references-to-directives 1) "" "s")
                                   (if (= converted-references-to-directives 1) "" "s")))))))
        (message (concat
                  msg (if conversion-msgs
                          (concat ": " (mapconcat #'identity conversion-msgs " and "))
                        ""))
                 num-instructions
                 (if (> num-instructions 1) "s" ""))
        (when (region-active-p)
          (deactivate-mark))))))

(defun mevedel-next-instruction ()
  "Cycle through instructions in the forward direction."
  (interactive)
  (unless (mevedel--cycle-instruction nil 'next)
    (mevedel--print-instruction-not-found 'next nil)))

(defun mevedel-previous-instruction ()
  "Cycle through instructions in the backward direction."
  (interactive)
  (unless (mevedel--cycle-instruction nil 'previous)
    (mevedel--print-instruction-not-found 'previous nil)))

(defun mevedel-next-reference ()
  "Cycle through references in the forward direction."
  (interactive)
  (unless (mevedel--cycle-instruction 'reference 'next)
    (mevedel--print-instruction-not-found 'next 'reference)))

(defun mevedel-previous-reference ()
  "Cycle through references in the backward direction."
  (interactive)
  (unless (mevedel--cycle-instruction 'reference 'previous)
    (mevedel--print-instruction-not-found 'previous 'reference)))

(defun mevedel-next-directive ()
  "Cycle through directives in the forward direction."
  (interactive)
  (unless (mevedel--cycle-instruction 'directive 'next)
    (mevedel--print-instruction-not-found 'next 'directive)))

(defun mevedel-previous-directive ()
  "Cycle through directives in the backward direction."
  (interactive)
  (unless (mevedel--cycle-instruction 'directive 'previous)
    (mevedel--print-instruction-not-found 'previous 'directive)))

(defun mevedel-preview-directive-prompt ()
  "Preview directive prompt at the current point.

This command is useful to see what is actually being sent to the model."
  (interactive)
  (let ((directive (mevedel--topmost-instruction (car (mevedel--instructions-at (point) 'directive))
                                                 'directive)))
    (let ((request-string (mevedel--directive-llm-prompt directive)))
      (let ((bufname "*mevedel-directive-preview*"))
        (with-temp-buffer-window bufname
            '((display-buffer-reuse-window
               display-buffer-same-window))
            nil
          (princ request-string)
          (with-current-buffer bufname
            (when (fboundp 'markdown-mode)
              (markdown-mode))
            (read-only-mode 1)
            (visual-line-mode 1)
            (display-line-numbers-mode 1)
            (let ((local-map (make-sparse-keymap)))
              (set-keymap-parent local-map (current-local-map))
              (define-key local-map (kbd "q") 'quit-window)
              (use-local-map local-map))))))))

(defun mevedel-modify-directive-tag-query ()
  "Prompt minibuffer to enter a tag search query for a directive.

The directive in question is the directive under the current point.

A tag query is an _infix_ expression, containing symbol atoms and the
operator symbols: `and', `or', `not'.  If no operator is present between
two expressions, then an implicit `and' operator is assumed.

Examples:
  (signature and function and doc)
  (not dog or not cat)
  (cat or dog or (sheep and black))
  ((cat and dog) or (dog and goose))"
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction
                        (mevedel--highest-priority-instruction (mevedel--instructions-at (point)) t)
                        'directive)))
      (let ((query (mevedel--read-tag-query (substring-no-properties
                                             (or
                                              (overlay-get directive
                                                           'mevedel-directive-infix-tag-query-string)
                                              "")))))
        (mevedel--set-directive-tag-query directive query))
    (user-error "No directive at point")))

(defun mevedel-add-tags (&optional reference)
  "Add tags to the reference under the point.

Adds specificly to REFERENCE if it is non-nil."
  (interactive)
  (let* ((instructions (mevedel--instructions-at (point) 'reference))
         (instr (or reference (mevedel--highest-priority-instruction instructions t))))
    (if instr
        (let* ((existing-tags (mevedel--available-tags))
               (input (completing-read-multiple "Add tags (or leave empty): "
                                                existing-tags nil nil))
               (new-tags (mapcar #'intern input)))
          (let ((added (mevedel--add-tags instr new-tags)))
            (message "%d tag%s added" added (if (= added 1) "" "s"))))
      (user-error "No reference at point"))))

(defun mevedel-remove-tags ()
  "Remove tags from the reference under the point."
  (interactive)
  (let* ((instructions (mevedel--instructions-at (point) 'reference))
         (instr (mevedel--highest-priority-instruction instructions t)))
    (if instr
        (let ((tags-list (mevedel--reference-tags instr)))
          (if (null tags-list)
              (user-error "Reference has no tags of its own to remove")
            ;; Prompt the user to remove tags.
            (let* ((input (completing-read-multiple "Remove tags: " tags-list nil t))
                   (tags-to-remove (mapcar #'intern input)))
              (let ((removed (mevedel--remove-tags instr tags-to-remove)))
                (message "%d tag%s removed" removed (if (= removed 1) "" "s"))))))
      (user-error "No reference at point"))))

(declare-function mevedel--instruction-with-id "mevedel" (target-id))
(let ((map (make-hash-table))
      (map-key nil))
  (cl-defun mevedel--instruction-with-id (target-id)
    "Return the instruction with the given integer TARGET-ID.

Returns nil if no instruction with the spcific id was found."
    (cl-labels ((entry-live-p (entry)
                  (and (bufferp (car entry))
                       (cl-some (lambda (instr)
                                  (and (overlayp instr)
                                       (buffer-live-p
                                        (overlay-buffer instr))))
                                (cdr entry))))
                (current-state-live-p ()
                  (cl-some #'entry-live-p (mevedel--instruction-alist)))
                (find-in-all-states ()
                  (let ((found nil)
                        (ambiguous nil))
                    (maphash
                     (lambda (_key state)
                       (dolist (entry (plist-get state :instructions))
                         (when (bufferp (car entry))
                           (dolist (instr (cdr entry))
                             (when (and (overlayp instr)
                                        (buffer-live-p (overlay-buffer instr))
                                        (= target-id
                                           (mevedel--instruction-id instr)))
                               (if found
                                   (setq ambiguous t)
                                 (setq found instr)))))))
                     mevedel--instruction-states)
                    (and (not ambiguous) found))))
      (let ((workspace (mevedel--instruction-buffer-workspace
                        (current-buffer))))
        (mevedel--instruction-activate-workspace workspace)
        (unless (equal map-key mevedel--instruction-current-state-key)
          (setq map (make-hash-table)
                map-key mevedel--instruction-current-state-key))
        (when-let* ((instr (gethash target-id map)))
          (when (buffer-live-p (overlay-buffer instr))
            (cl-return-from mevedel--instruction-with-id instr)))
        (setq map (make-hash-table))
        (mevedel--foreach-instruction instr
          do (puthash (mevedel--instruction-id instr) instr map))
        (or (gethash target-id map)
            ;; Prompt-copy and test buffers can have a workspace that is
            ;; unrelated to the source buffers holding references.  If the
            ;; current bucket is empty, fall back to a unique match anywhere.
            (and (not (current-state-live-p))
                 (find-in-all-states)))))))

(defun mevedel--instruction-id (instruction)
  "Return unique identifier for INSTRUCTION overlay."
  (overlay-get instruction 'mevedel-id))

(defun mevedel--stashed-buffer-instructions (buffer)
  "Return stashed instruction data for all instructions in BUFFER.

Each instruction is represented as a plist with :overlay-start,
:overlay-end, :anchor, and :properties keys, capturing the overlay's
position, lightweight re-anchoring context, and semantic properties for
later restoration."
  (mevedel--foreach-instruction (instr buffer)
    collect (list :overlay-start (overlay-start instr)
                  :overlay-end (overlay-end instr)
                  :anchor (mevedel--instruction-anchor-for-instruction
                           instr)
                  :properties (mevedel--instruction-persisted-properties
                               instr))))

(defun mevedel--stash-buffer (buffer &optional file-contents)
  "Stash BUFFER's instructions and original content.

Save the buffer's instructions and original content to
`(mevedel--instruction-alist)', then remove the instruction overlays from the
buffer.  The content is either the current buffer content or
FILE-CONTENTS."
  (mevedel--instruction-activate-buffer buffer)
  (let ((instrs (mevedel--stashed-buffer-instructions buffer)))
    (when instrs
      (with-current-buffer buffer
        (let ((original-content (or file-contents (buffer-substring-no-properties (point-min)
                                                                                  (point-max)))))
          (setf (alist-get buffer (mevedel--instruction-alist))
                (list :original-content original-content
                      :instructions instrs)
                (car (assoc buffer (mevedel--instruction-alist)))
                (buffer-file-name buffer))
          (mapc #'delete-overlay
                (mevedel--instructions-in (point-min) (point-max))))))))

(defun mevedel--reference-list-info (refs)
  "Return a plist with information regarding REFS list.

:buffer-count - Amount of buffers with references from REFS
:line-count   - Amount of total lines spanned by top-level references"
  (let ((bufhash (make-hash-table))
        (buffer-count 0)
        (line-count 0))
    (cl-loop
     for ref in refs
     do (let ((buffer (overlay-buffer ref))
              (start (overlay-start ref))
              (end (overlay-end ref)))
          (if-let* ((line-ranges (gethash buffer bufhash)))
              (cl-loop for range in line-ranges
                       do (cl-destructuring-bind (range-start . range-end) range
                            (when (<= start range-start range-end end)
                              (setf (car range) start
                                    (cdr range) end)
                              (cl-return)))
                       finally (puthash buffer (push (cons start end) line-ranges) bufhash))
            (puthash buffer `((,(overlay-start ref) . ,(overlay-end ref))) bufhash)))
     finally (setq buffer-count
                   (hash-table-count bufhash))
     (maphash (lambda (buffer ranges)
                (with-current-buffer buffer
                  (cl-loop for (beg . end) in ranges
                           do (let ((end-lineno (line-number-at-pos end))
                                    (beg-lineno (line-number-at-pos beg)))
                                (setq line-count
                                      (+ line-count (+ 1 (- end-lineno beg-lineno))))))))
              bufhash)
     (cl-return (list :buffer-count buffer-count :line-count line-count)))))

(defun mevedel--reference-list-info-string (refs)
  "Return a formatted statistics string for REFS.

REFS is a list of references to format.  The string includes hit count,
buffer count, and line count with proper pluralization."
  (cl-destructuring-bind (&key buffer-count line-count)
      (mevedel--reference-list-info refs)
    (let ((ref-count (length refs)))
      (format "%d hit%s in %d buffer%s, %d line%s"
              ref-count
              (if (= ref-count 1) "" "s")
              buffer-count
              (if (= buffer-count 1) "" "s")
              line-count
              (if (= line-count 1) "" "s")))))

(defun mevedel--read-tag-query (&optional default)
  "Prompt user via minibuffer for a tag query text.

DEFAULT is the default query to display in the minibuffer.
Returns the validated query string."
  (minibuffer-with-setup-hook
      (lambda ()
        (let ((timer nil)
              (minibuffer-message))
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (when timer
                        (cancel-timer timer)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (when timer
                        (cancel-timer timer))
                      (setq timer
                            (run-with-timer
                             0.5
                             nil
                             (lambda ()
                               (condition-case err
                                   (let* ((input (minibuffer-contents))
                                          (query (read (concat "(" input ")"))))
                                     (let ((refs (mevedel--filter-references
                                                  (mevedel--tag-query-prefix-from-infix query))))
                                       (setq minibuffer-message
                                             (mevedel--reference-list-info-string refs))))
                                 (error
                                  (let ((errmsg (error-message-string err)))
                                    (setq minibuffer-message errmsg))))
                               (when minibuffer-message
                                 (set-minibuffer-message minibuffer-message))))))
                    nil t)))
    (let ((default (or default "")))
      (let ((input (read-from-minibuffer "Tag query: " default)))
        (let ((query (read (format "(%s)" input))))
          (condition-case err
              (progn
                (mevedel--tag-query-prefix-from-infix query)
                (mapconcat (lambda (q) (format "%s" q)) query " "))
            (error
             (let ((errmsg (error-message-string err)))
               (user-error errmsg)))))))))

(defun mevedel--set-directive-tag-query (directive query)
  "Set the tag query for DIRECTIVE to QUERY string."
  (condition-case err
      (let ((parsed-prefix-tag-query
             (mevedel--tag-query-prefix-from-infix (read (concat "(" query ")")))))
        (overlay-put directive 'mevedel-directive-prefix-tag-query parsed-prefix-tag-query)
        (if (string-empty-p query)
            (overlay-put directive 'mevedel-directive-infix-tag-query-string nil)
          (overlay-put directive
                       'mevedel-directive-infix-tag-query-string
                       (mevedel--apply-face-to-match "\\b\\(?:(*not\\|or\\|and\\)\\b\\|(\\|)"
                                                     (mevedel--apply-face-to-match
                                                      "\\(:?.+\\)"
                                                      query
                                                      'font-lock-constant-face)
                                                     nil))
          (overlay-put directive 'mevedel-directive-status nil))
        (mevedel--update-instruction-overlay directive t))
    (error
     (message (error-message-string err)))))

(defun mevedel--print-instruction-not-found (direction type)
  "Print a not found message for the given DIRECTION and TYPE."
  (let ((type-string (pcase type
                       ('directive "directive")
                       ('reference "reference")
                       (_ "instruction"))))
    (message "No %s %s found"
             (if (eq direction 'next) "next" "previous")
             type-string)))

(cl-defun mevedel--reference-matches-query-p (reference query)
  "Return t only if REFERENCE matches the tag QUERY."
  (unless reference
    (cl-return-from mevedel--reference-matches-query-p nil))
  (let ((atoms (cl-remove-duplicates (cl-remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) mevedel-empty-tag-query-matches-all)
        t
      (let ((tags (mevedel--reference-tags reference t))
            (direct-tags (mevedel--reference-tags reference nil))
            (instr-id (lambda (tag) (let ((tagname (symbol-name tag)))
                                      (when (string-match "^id:\\([1-9][0-9]*\\)$" tagname)
                                        (string-to-number (match-string 1 tagname)))))))
        (if (and (null direct-tags) mevedel-always-match-untagged-references)
            t
          (let ((atom-bindings (mapcar (lambda (atom)
                                         (pcase atom
                                           ('is:bufferlevel
                                            (mevedel--instruction-bufferlevel-p reference))
                                           ('is:subreference
                                            (mevedel--parent-instruction reference 'reference))
                                           ('is:tagless
                                            (null tags))
                                           ('is:directly-tagless
                                            (null (mevedel--reference-tags reference nil)))
                                           ('is:with-commentary
                                            (not (string-empty-p (mevedel--commentary-text reference))))
                                           (_ (if-let* ((id (funcall instr-id atom)))
                                                  (= id (mevedel--instruction-id reference))
                                                (member atom tags)))))
                                       atoms)))
            (cl-progv atoms atom-bindings
              (eval query))))))))

(defun mevedel--filter-references (query)
  "Return a list of all references filtered by the tag QUERY.

See `mevedel--tag-query-prefix-from-infix' for QUERY format."
  (let ((atoms (cl-remove-duplicates (cl-remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) mevedel-empty-tag-query-matches-all)
        (mevedel--foreach-instruction instr when (mevedel--referencep instr) collect instr)
      (mevedel--foreach-instruction instr
        when (and (mevedel--referencep instr)
                  (mevedel--reference-matches-query-p instr query))
        collect instr))))

(defun mevedel--available-tags ()
  "Return a list of all the tags in the loaded references."
  (let ((tags-hash (make-hash-table)))
    (mevedel--foreach-instruction (ref)
      do (when (mevedel--referencep ref)
           (cl-loop for tag in (mevedel--reference-tags ref)
                    do (puthash tag t tags-hash))))
    (hash-table-keys tags-hash)))

(defun mevedel--cycle-instruction (type direction)
  "Get the next or previous instruction overlay of TYPE.
DIRECTION should be `next' or `previous' from the current point.

If no instruction found in the buffer, checks the next buffers in the
`(mevedel--instruction-alist)' alist.

Returns the found instruction, if any."
  ;; We want the buffers to be a cyclic list, based on the current buffer.
  (mevedel--instruction-activate-buffer)
  (let* ((buffers (let ((bufs (mapcar #'car (mevedel--instruction-alist))))
                    (if (eq direction 'next)
                        (mevedel--cycle-list-around (current-buffer) bufs)
                      (mevedel--cycle-list-around (current-buffer) (nreverse bufs)))))
         (original-buffer (current-buffer))
         (found-instr))
    (while (and buffers (null found-instr))
      (let* ((buffer (car buffers))
             (instrs (mevedel--foreach-instruction (instr buffer) collect instr)))
        (setq buffers (delq buffer buffers))
        (when type
          (setq instrs (cl-remove-if-not (lambda (instr)
                                           (eq (mevedel--instruction-type instr) type))
                                         instrs)))
        (let ((sorting-pred (pcase direction
                              ('next #'<)
                              ('previous #'>))))
          (when (eq buffer original-buffer)
            (setq instrs (cl-remove-if-not (lambda (instr)
                                             (funcall sorting-pred
                                                      (point)
                                                      (overlay-start instr)))
                                           instrs)))
          (setq instrs (sort instrs (lambda (instr1 instr2)
                                      (funcall sorting-pred
                                               (overlay-start instr1)
                                               (overlay-start instr2)))))
          (when-let* ((instruction (car instrs)))
            (let ((buffer (overlay-buffer instruction)))
              (unless (eq buffer original-buffer)
                (switch-to-buffer buffer)))
            (goto-char (overlay-start instruction))
            (setq found-instr instruction)))))
    found-instr))

(defun mevedel--add-tags (reference tags)
  "Add TAGS to REFERENCE.

TAGS should be a list of symbols.
Returns the number of new tags added."
  (let* ((tag-type 'mevedel-reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl-remove-if (lambda (tag) (member tag existing-tags)) tags)))
    (overlay-put reference tag-type (cl-union existing-tags new-tags :test 'eq))
    (let ((added (length new-tags)))
      (when (> added 0)
        (mevedel--update-instruction-overlay reference t))
      added)))

(defun mevedel--remove-tags (reference tags)
  "Remove TAGS from REFERENCE.

TAGS should be a list of symbols.
Returns the number of tags removed."
  (let* ((tag-type 'mevedel-reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl-set-difference existing-tags tags :test 'eq)))
    (overlay-put reference tag-type new-tags)
    (let ((removed (- (length existing-tags) (length new-tags))))
      (when (> removed 0)
        (mevedel--update-instruction-overlay reference t))
      removed)))

(defun mevedel--inherited-tags (reference)
  "Return the list of all tags that REFERENCE inherits from its parents."
  (when-let* ((parent (mevedel--parent-instruction reference 'reference)))
    (mevedel--reference-tags parent t)))

(defun mevedel--reference-tags (reference &optional include-parent-tags)
  "Return the list of tags for the given REFERENCE.

If INCLUDE-PARENT-TAGS is non-nil, gets te parent's tags as well."
  (if (not include-parent-tags)
      (overlay-get reference 'mevedel-reference-tags)
    (append (overlay-get reference 'mevedel-reference-tags)
            (when-let* ((parent (mevedel--parent-instruction reference 'reference)))
              (mevedel--reference-tags parent t)))))

(defun mevedel--delete-instruction-at (point)
  "Delete the instruction at POINT.

Returns the deleted instruction overlay."
  (let* ((instructions (mevedel--instructions-at point))
         (target (mevedel--highest-priority-instruction instructions t)))
    (when target
      (mevedel--delete-instruction target))))

(defun mevedel--directive-empty-p (directive)
  "Check if DIRECTIVE is empty.

A directive is empty if it does not have a body or secondary directives."
  (let ((subdirectives
         (cl-remove-if-not #'mevedel--directivep
                           (mevedel--wholly-contained-instructions (overlay-buffer directive)
                                                                   (overlay-start directive)
                                                                   (overlay-end directive)))))
    (not (cl-some (lambda (subdir)
                    (not (string-empty-p (mevedel--directive-text subdir))))
                  subdirectives))))

(defun mevedel--create-instruction (type)
  "Create or scale an instruction of the given TYPE within the region.

If a region is selected but partially covers an existing instruction,
then the function will resize it.  See either `mevedel-create-reference'
or `mevedel-create-directive' for details on how the resizing works."
  (if (use-region-p)
      (let ((intersecting-instructions
             (cl-remove-if (lambda (instr)
                             (xor (= (overlay-start instr) (region-beginning))
                                  (= (overlay-end instr) (region-end))))
                           (mevedel--partially-contained-instructions (current-buffer)
                                                                      (region-beginning)
                                                                      (region-end)))))
        (if-let* ((instructions
                   (cl-remove-if-not (lambda (instr)
                                       (eq (mevedel--instruction-type instr) type))
                                     intersecting-instructions)))
            (progn
              (dolist (instruction instructions)
                (if (< (overlay-start instruction) (point) (overlay-end instruction))
                    (if (< (mark) (point))
                        (setf (overlay-start instruction) (point))
                      (setf (overlay-end instruction) (point)))
                  (if (> (mark) (point))
                      (setf (overlay-start instruction) (point))
                    (setf (overlay-end instruction) (point))))
                (mevedel--update-instruction-overlay instruction))
              (when instructions
                (deactivate-mark)))
          ;; Else - there are no partially contained instructions of the same
          ;; type within the region...
          (when (or intersecting-instructions
                    (or (cl-some (lambda (instr)
                                   (and (= (overlay-start instr) (region-beginning))
                                        (= (overlay-end instr) (region-end))))
                                 (mevedel--instructions-in (region-beginning) (region-end)))))
            ;; ...but there are intersecting instructions of another type, or
            ;; another instruction existing precisely at the start of another.
            (user-error "Instruction intersects with existing instruction"))
          (let* ((buffer (current-buffer))
                 (instruction (if (eq type 'reference)
                                  (mevedel--create-reference-in buffer
                                                                (region-beginning)
                                                                (region-end))
                                (save-window-excursion
                                  (let ((pos (region-beginning)))
                                    (unless (<= (window-start) pos (window-end))
                                      (set-window-start (selected-window)
                                                        (max (point-min)
                                                             (- (region-beginning)
                                                                (- (window-end) (window-start))))))
                                    (mevedel--create-directive-in buffer
                                                                  (region-beginning)
                                                                  (region-end)))))))
            (with-current-buffer buffer
              (deactivate-mark)
              (when (eq type 'reference)
                (mevedel-add-tags instruction)))
            instruction)))
    (when (eq type 'directive)
      (prog1 (mevedel--create-directive-in (current-buffer) (point) (point) t)
        (deactivate-mark)))))

(defun mevedel--referencep (instruction)
  "Return non-nil if INSTRUCTION is a reference."
  (eq (mevedel--instruction-type instruction) 'reference))

(defun mevedel--directivep (instruction)
  "Return non-nil if INSTRUCTION is a directive."
  (eq (mevedel--instruction-type instruction) 'directive))

(cl-defun mevedel--highest-priority-instruction (instructions &optional return-highlighted)
  "Return the instruction with the highest priority from the INSTRUCTIONS list.

Priority here refers to the priority property used by overlays.

If RETURN-HIGHLIGHTED is non-nil and
`mevedel--highlighted-instruction' is non-nil, the function will
return `mevedel--highlighted-instruction' if it is also in the
INSTRUCTIONS list."
  (when (and return-highlighted
             mevedel--highlighted-instruction
             (member mevedel--highlighted-instruction instructions))
    (cl-return-from mevedel--highest-priority-instruction mevedel--highlighted-instruction))
  (cl-reduce (lambda (acc instruction)
               (if (or (not acc)
                       (> (or (overlay-get instruction 'priority)
                              mevedel--default-instruction-priority)
                          (or (overlay-get acc 'priority)
                              mevedel--default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun mevedel--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay.

Instruction type can either be `reference' or `directive'."
  (if-let* ((type (overlay-get instruction 'mevedel-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun mevedel--create-instruction-overlay-in (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (make-local-variable 'mevedel--after-change-functions-hooked)
  (mevedel--instruction-activate-buffer buffer)
  (with-current-buffer buffer
    (let ((is-bufferlevel
           ;; Check if the overlay spans the start and end of the buffer. If it
           ;; does, make it sticky so that additions to edges of the buffer will
           ;; cause it to expand there. This is useful for when we want to
           ;; append new text to the end of the buffer but don't want to
           ;; "invalidate" the buffer-level status of the instruction.
           (and (= start (point-min)) (= end (point-max)))))
      (let ((overlay (make-overlay start end (current-buffer) (not is-bufferlevel) is-bufferlevel)))
        (overlay-put overlay 'mevedel-instruction t)
        (overlay-put overlay 'mevedel-id (mevedel--create-id))
        (unless (overlay-get overlay 'mevedel-uuid)
          (overlay-put overlay 'mevedel-uuid (mevedel--create-uuid)))
        (push overlay (alist-get buffer (mevedel--instruction-alist)))
        (unless (bound-and-true-p mevedel--after-change-functions-hooked)
          (setq-local mevedel--after-change-functions-hooked t)
          (add-hook 'after-change-functions
                    (lambda (beg end _len)
                      (let ((beg (max (point-min) (1- beg)))
                            (end (min (point-max) (1+ end))))
                        (let ((affected-instructions (mevedel--instructions-in beg end)))
                          (dolist (instruction affected-instructions)
                            (mevedel--update-instruction-overlay instruction)))))
                    nil t))
        (mevedel--setup-buffer-hooks buffer)
        overlay))))

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

(defun mevedel--find-directive-by-uuid (uuid)
  "Find directive overlay with UUID.
Returns the overlay, or nil if not found."
  (when uuid
    (mevedel--foreach-instruction instr
      when (and (mevedel--directivep instr)
                (equal (overlay-get instr 'mevedel-uuid) uuid))
      return instr)))

(defun mevedel-get-directive-patch (directive)
  "Get the stored patch for DIRECTIVE, if any.
Returns the unified diff string, or nil if no patch is stored."
  (overlay-get directive 'mevedel-directive-patch))

(defun mevedel--parent-instruction (instruction &optional of-type)
  "Return the parent of the given INSTRUCTION overlay.

If OF-TYPE is non-nil, returns the parent with the given type."
  (with-current-buffer (overlay-buffer instruction)
    (let ((beg (overlay-start instruction))
          (end (overlay-end instruction)))
      (mevedel--highest-priority-instruction
       (cl-remove-if-not (lambda (instr)
                           (and (not (eq instr instruction))
                                (or (null of-type)
                                    (eq (mevedel--instruction-type instr)
                                        of-type))
                                (<= (overlay-start instr) beg
                                    end (overlay-end instr))))
                         (mevedel--instructions-in beg end))))))

(defun mevedel--bodyless-instruction-p (instr)
  "Return non-nil if the INSTR instruction has a body."
  (= (overlay-start instr) (overlay-end instr)))

(defun mevedel--subinstruction-of-p (sub parent)
  "Return t is instruction SUB is contained entirely within instruction PARENT.

In this case, an instruction is _not_ considered a subinstruction of
itself."
  (and (eq (overlay-buffer sub)
           (overlay-buffer parent))
       (<= (overlay-start parent) (overlay-start sub) (overlay-end sub) (overlay-end parent))
       (and (/= (overlay-start parent) (overlay-start sub))
            (/= (overlay-end parent) (overlay-end sub)))))

(cl-defun mevedel--child-instructions (instruction)
  "Return the direct child instructions of the given INSTRUCTION overlay."
  ;; Bodyless instructions cannot have any children.
  (when (mevedel--bodyless-instruction-p instruction)
    (cl-return-from mevedel--child-instructions nil))
  (let ((children (cl-remove-if (lambda (instr)
                                  (or (eq instr instruction)
                                      (and (= (overlay-start instr) (overlay-start instruction))
                                           (= (overlay-end instr) (overlay-end instruction)))))
                                (mevedel--wholly-contained-instructions (overlay-buffer instruction)
                                                                        (overlay-start instruction)
                                                                        (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl-set-difference children
                                        (mevedel--child-instructions child))))
    children))

(defun mevedel--create-reference-in (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (let ((ov (mevedel--create-instruction-overlay-in buffer start end)))
    (overlay-put ov 'mevedel-instruction-type 'reference)
    (overlay-put ov 'evaporate t)
    (mevedel--update-instruction-overlay ov t)
    ov))

(defun mevedel--create-directive-in (buffer start end &optional bodyless directive-text)
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution.  BODYLESS
controls special formatting if non-nil.

DIRECTIVE-TEXT is used as the default directive.  Having DIRECTIVE-TEXT
be non-nil prevents the opening of a prompt buffer."
  (let ((ov (mevedel--create-instruction-overlay-in buffer start end)))
    (unless bodyless
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'mevedel-instruction-type 'directive)
    (overlay-put ov 'mevedel-directive (or directive-text ""))
    (mevedel--update-instruction-overlay ov (not bodyless))
    (unless directive-text
      (deactivate-mark)
      (mevedel--read-directive ov))
    ov))

(defun mevedel--delete-instruction (instruction &optional buffer)
  "Delete the INSTRUCTION overlay and return it.

If the overlay is already dead, just perform the cleanup.
BUFFER is required in order to perform cleanup on a dead instruction."
  ;; We want to handle this function in two different ways. The first way
  ;; handles regular deletion, i.e. when the function was invoked on an existing
  ;; instruction. The second way is for when the instruction was deleted
  ;; uncanonically through text manipulation. In the latter case, the function
  ;; will be called during a cleanup routine and the instruction will not be
  ;; alive.
  (when (overlay-get instruction 'mevedel-marked-for-deletion)
    (error "Instruction %s already marked for deletion" instruction))
  (mevedel--instruction-activate-workspace
   (mevedel--instruction-buffer-workspace
    (or (overlay-buffer instruction) buffer (current-buffer))))
  (overlay-put instruction 'mevedel-marked-for-deletion t)
  (cl-labels ((cleanup (instr buffer)
                (let ((id (mevedel--instruction-id instr)))
                  (mevedel--retire-id id)
                  (with-current-buffer buffer
                    (mevedel-unlink-instructions
                     `(,id) (mevedel--instruction-outlinks instr))
                    (mevedel-unlink-instructions
                     (mevedel--instruction-inlinks instr) `(,id))))
                (setf (cdr (assoc buffer (mevedel--instruction-alist)))
                      (delq instr (cdr (assoc buffer (mevedel--instruction-alist)))))))
    (let ((ov-buffer (overlay-buffer instruction)))
      (when (buffer-live-p ov-buffer)
        (let ((children (mevedel--child-instructions instruction)))
          (delete-overlay instruction)
          (dolist (child children)
            (mevedel--update-instruction-overlay child t))))
      (cleanup instruction (or ov-buffer
                               buffer
                               (error "Cannot perform cleanup without a buffer")))))
  instruction)

(defun mevedel--instructions-congruent-p (a b)
  "Return t if instruction overlays A and B are congruent.
Two overlays are considered congruent if they are in the same buffer and
have identical start and end positions.

A, B: Two overlays to compare for congruence.

Returns: t if A and B are congruent, nil otherwise."
  (and (eq (overlay-buffer a) (overlay-buffer b))
       (= (overlay-start a) (overlay-start b))
       (= (overlay-end a) (overlay-end b))))

(defun mevedel--instruction-bufferlevel-p (instruction)
  "Return t if INSTRUCTION spans the entirety of its buffer."
  (let ((buffer (overlay-buffer instruction)))
    (when buffer
      (with-current-buffer buffer
        (without-restriction
          (and (= (overlay-start instruction) (point-min))
               (= (overlay-end instruction) (point-max))))))))

;; Overlay actions adapted from `gptel-rewrite'

(defun mevedel--ov-actions-getov ()
  "Return an instruction overlay at point for action dispatch.

If multiple instruction overlays exist at point, prompt the user to
select one via `completing-read'.  If only one overlay exists, return it
directly.  Return nil if no overlays exist at point."
  (let* ((ovs (mevedel--instructions-at (point)))
         (ov-strings (cl-loop for ov in ovs
                              collect (string-trim (overlay-get ov 'before-string))))
         (ov-map (cl-loop for i below (length ovs)
                          collect (cons (nth i ov-strings) (nth i ovs))))
         selection)
    (if (length> ovs 1)
        (setq selection (completing-read "Choose instruction overlay: " ov-strings))
      (setq selection (car ov-strings)))
    (alist-get selection ov-map nil nil #'equal)))

(defun mevedel--ov-actions-dispatch (&optional instruction ci)
  "Dispatch actions for a successful instruction overlay.

INSTRUCTION is the overlay to dispatch actions for, CI is true for
interactive calls."
  (interactive (list (mevedel--ov-actions-getov) t))
  (let ((choice)
        (instruction-type (mevedel--instruction-type instruction))
        (before-string (overlay-get instruction 'before-string)))
    (unwind-protect
        (pcase-let ((choices
                     (pcase instruction-type
                       (`reference `((?t "add-tags") (?r "remove-tags") (?l "link") (?u "unlink") (?c "commentary") (?k "clear")
                                     ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                          '(?e "expand") '(?e "collapse"))))
                       (`directive
                        (pcase (overlay-get instruction 'mevedel-directive-status)
                          ('processing `((?a "abort") (?k "clear")
                                         ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                              '(?e "expand") '(?e "collapse"))))
                          ('succeeded `((?v "view") (?w "show-answer") (?r "revise") (?p "preview") (?m "modify") (?k "clear")
                                        ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                             '(?e "expand") '(?e "collapse"))))
                          ('failed `((?i "implement") (?r "revise") (?m "modify") (?p "preview") (?k "clear")
                                     ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                          '(?e "expand") '(?e "collapse"))))
                          (_ `((?d "discuss") (?i "implement") (?r "revise") (?t "tags") (?m "modify")
                               (?p "preview") (?k "clear")
                               ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                    '(?e "expand") '(?e "collapse"))))))))
                    (hint-str (concat "[" (gptel--model-name gptel-model) "]\n")))
          (overlay-put
           instruction 'before-string
           (concat
            before-string
            (propertize "ACTIONS: " 'face 'success)
            (when (fboundp #'rmc--add-key-description)
              (mapconcat (lambda (e) (cdr e)) (mapcar #'rmc--add-key-description choices) ", "))
            (propertize
             " " 'display `(space :align-to (- right ,(1+ (length hint-str)))))
            (propertize hint-str 'face 'success)))
          (setq choice (read-multiple-choice "Action: " choices)))
      (overlay-put instruction 'before-string before-string))
    (let ((cmd (if (member (cadr choice) '("expand" "collapse"))
                   "cycle"
                 (cadr choice))))
      (if ci
          (funcall-interactively (intern (concat "mevedel--ov-actions-" cmd)) instruction)
        (funcall (intern (concat "mevedel--ov-actions-" cmd)) instruction)))))

;; Declare overlay action functions
(eval-and-compile
  (dolist (pair '((add-tags    . mevedel-add-tags)
                  (remove-tags . mevedel-remove-tags)
                  (link        . mevedel-link-instructions)
                  (unlink      . mevedel-unlink-instructions)
                  (commentary  . mevedel-modify-reference-commentary)
                  (abort       . mevedel-abort)
                  (modify      . mevedel-modify-directive)
                  (discuss     . mevedel-discuss-directive)
                  (implement   . mevedel-implement-directive)
                  (revise      . mevedel-revise-directive)
                  (tags        . mevedel-modify-directive-tag-query)
                  (preview     . mevedel-preview-directive-prompt)))
    (let ((name (car pair))
          (target (cdr pair)))
      (defalias (intern (format "mevedel--ov-actions-%s" name))
        (lambda (&optional _instructions)
          (interactive)
          (call-interactively target))
        (format "Wrapper around `%s' for overlay dispatch actions." target)))))

(defun mevedel--ov-actions-clear (&optional _instructions)
  "Clear instructions.
Deletes all instructions at point and removes the eldoc hook that
provides help for instruction actions if not other instructions are
active in the buffer."
  (interactive)
  (mevedel-delete-instructions)
  (with-current-buffer (current-buffer)
    (unless (alist-get (current-buffer) (mevedel--instruction-alist))
      (remove-hook 'eldoc-documentation-functions 'mevedel--ov-actions-help 'local))))

(defun mevedel--ov-actions-show-answer (&optional instructions)
  "Navigate to INSTRUCTIONS' directive answer in the view buffer.

Falls back to the authoritative chat buffer if the compact view is not live."
  (interactive (list (mevedel--ov-actions-getov)))
  (let* ((response-marker
          (and instructions
               (overlay-get instructions 'mevedel-directive-response-start)))
         (chat-buffer
          (or (and (markerp response-marker)
                   (marker-buffer response-marker))
              (and gptel--fsm-last
                   (plist-get (gptel-fsm-info gptel--fsm-last) :buffer))
              (mevedel--active-chat-buffer)))
         (response-start
          (or (and (markerp response-marker)
                   (marker-position response-marker))
              (and gptel--fsm-last
                   (let* ((info (gptel-fsm-info gptel--fsm-last))
                          (pos (plist-get info :position)))
                     (and (markerp pos) (marker-position pos))))
              (mevedel--ov-actions--find-directive-response-start
               instructions chat-buffer)))
         (view-buffer
          (mevedel--ov-actions--directive-view-buffer
           chat-buffer instructions)))
    (cond
     ((and response-start
           view-buffer
           (buffer-live-p view-buffer))
        (let ((window (display-buffer view-buffer gptel-display-buffer-action)))
          (when window
            (select-window window))
          (with-current-buffer view-buffer
            (unless (mevedel--ov-actions--goto-view-source response-start)
              (when (fboundp 'mevedel-view--full-rerender)
                (mevedel-view--full-rerender))
              (unless (mevedel--ov-actions--goto-view-source response-start)
                (user-error "Answer is not currently rendered in the view")))
            (when-let* ((display-window (get-buffer-window (current-buffer) t)))
              (with-selected-window display-window
                (recenter))))))
     ((and view-buffer
           (buffer-live-p view-buffer))
      (let ((window (display-buffer view-buffer gptel-display-buffer-action)))
        (when window
          (select-window window))
        (with-current-buffer view-buffer
          (unless (mevedel--ov-actions--goto-view-directive-answer
                   instructions)
            (user-error "No answer location recorded for this directive"))
          (when-let* ((display-window (get-buffer-window (current-buffer) t)))
            (with-selected-window display-window
              (recenter))))))
     ((and response-start chat-buffer (buffer-live-p chat-buffer))
      (let ((window (display-buffer chat-buffer gptel-display-buffer-action)))
        (when window
          (select-window window))
        (with-current-buffer chat-buffer
          (goto-char response-start)
          (when-let* ((display-window (get-buffer-window (current-buffer) t)))
            (with-selected-window display-window
              (recenter))))))
     (t
      (user-error "No answer location recorded for this directive")))))

(defun mevedel--ov-actions--directive-view-buffer
    (chat-buffer instructions)
  "Return the view buffer for CHAT-BUFFER or INSTRUCTIONS."
  (or (and chat-buffer
           (buffer-live-p chat-buffer)
           (buffer-local-value 'mevedel--view-buffer chat-buffer))
      (when-let* ((workspace
                   (or (and instructions
                            (buffer-live-p (overlay-buffer instructions))
                            (with-current-buffer (overlay-buffer instructions)
                              (mevedel-workspace)))
                       (mevedel-workspace))))
        (catch 'view
          (dolist (buf (buffer-list))
            (when (and (buffer-live-p buf)
                       (buffer-local-value 'mevedel--data-buffer buf))
              (let* ((data-buffer
                      (buffer-local-value 'mevedel--data-buffer buf))
                     (session
                      (and (buffer-live-p data-buffer)
                           (buffer-local-value 'mevedel--session
                                               data-buffer)))
                     (session-workspace
                      (and session
                           (mevedel-session-workspace session))))
                (when (and session-workspace
                           (eq (mevedel-workspace-type session-workspace)
                               (mevedel-workspace-type workspace))
                           (equal (mevedel-workspace-id session-workspace)
                                  (mevedel-workspace-id workspace)))
                  (throw 'view buf)))))))))

(defun mevedel--ov-actions--goto-view-directive-answer (instructions)
  "Move point to the rendered answer for INSTRUCTIONS in the current view."
  (when-let* ((display-text
               (mevedel--ov-actions--directive-display-text instructions)))
    (let ((limit (or (and (boundp 'mevedel-view--input-marker)
                          (markerp mevedel-view--input-marker)
                          (marker-position mevedel-view--input-marker))
                     (point-max)))
          (case-fold-search nil)
          answer-pos)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward display-text limit t)
          (while (and (not answer-pos) (< (point) limit))
            (let* ((pos (point))
                   (type (get-text-property pos 'mevedel-view-type))
                   (next (or (next-single-property-change
                              pos 'mevedel-view-type nil limit)
                             limit)))
              (when (eq type 'response)
                (setq answer-pos pos))
              (goto-char (max (1+ pos) next))))))
      (when answer-pos
        (goto-char answer-pos)
        t))))

(defun mevedel--ov-actions--directive-display-text (instructions)
  "Return the user-facing directive text for INSTRUCTIONS."
  (when instructions
    (let* ((action (or (overlay-get instructions 'mevedel-directive-action)
                       'implement))
           (action-label (or (alist-get action
                                        '((implement . "Implement")
                                          (revise . "Revise")
                                          (discuss . "Discuss")
                                          (tutor . "Tutor")))
                             (capitalize
                              (replace-regexp-in-string
                               "[-_]+" " " (symbol-name action)))))
           (directive-text (string-trim
                            (mevedel--directive-text instructions))))
      (if (string-empty-p directive-text)
          action-label
        (format "%s: %s" action-label directive-text)))))

(defun mevedel--ov-actions--goto-view-source (response-start)
  "Move point to rendered source for RESPONSE-START in the current view buffer."
  (let ((limit (or (and (boundp 'mevedel-view--input-marker)
                        (markerp mevedel-view--input-marker)
                        (marker-position mevedel-view--input-marker))
                   (point-max)))
        response-pos fallback-pos)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) limit)
        (let* ((pos (point))
               (source (get-text-property pos 'mevedel-view-source))
               (type (get-text-property pos 'mevedel-view-type))
               (next (or (next-single-property-change
                          pos 'mevedel-view-source nil limit)
                         limit)))
          (when (and (consp source)
                     (< response-start (cdr source)))
            (unless fallback-pos
              (setq fallback-pos pos))
            (when (and (not response-pos)
                       (eq type 'response))
              (setq response-pos pos)))
          (goto-char (max (1+ pos) next)))))
    (when-let* ((target (or response-pos fallback-pos)))
      (goto-char target)
      t)))

(defun mevedel--ov-actions--find-directive-response-start
    (instructions chat-buffer)
  "Find the response start for INSTRUCTIONS by scanning CHAT-BUFFER.

This is a compatibility fallback for directive turns created before the
overlay stored `mevedel-directive-response-start'."
  (when (and instructions chat-buffer (buffer-live-p chat-buffer))
    (let* ((action (overlay-get instructions 'mevedel-directive-action))
           (action-str (and action (symbol-name action)))
           (directive-text (string-trim (mevedel--directive-text instructions)))
           match)
      (with-current-buffer chat-buffer
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^:PROMPT:\n" nil t)
            (let* ((drawer-start (match-beginning 0))
                   (body-start (point))
                   (line (save-excursion
                           (goto-char drawer-start)
                           (forward-line -1)
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
              (when (re-search-forward "^:END:[ \t]*\n?" nil t)
                (let* ((drawer-end (point))
                       (body (buffer-substring-no-properties
                              body-start (match-beginning 0)))
                       (action-match-p
                        (or (null action-str)
                            (string-match-p
                             (regexp-quote (format ":%s:" action-str))
                             line)
                            (string-match-p
                             (regexp-quote (format "`%s`" action-str))
                             line)))
                       (directive-match-p
                        (or (string-empty-p directive-text)
                            (string-match-p
                             (regexp-quote directive-text)
                             line)
                            (string-match-p
                             (regexp-quote directive-text)
                             body))))
                  (when (and action-match-p directive-match-p)
                    (when-let* ((response-start
                                 (mevedel--ov-actions--next-response-position
                                  drawer-end)))
                      (setq match response-start))))))))
        match))))

(defun mevedel--ov-actions--next-response-position (start)
  "Return the first `gptel' response position at or after START."
  (save-excursion
    (goto-char start)
    (let (found)
      (while (and (not found) (< (point) (point-max)))
        (let ((next (or (next-single-property-change
                         (point) 'gptel nil (point-max))
                        (point-max))))
          (when (eq (get-text-property (point) 'gptel) 'response)
            (setq found (point)))
          (goto-char (if (= next (point)) (1+ (point)) next))))
      found)))

(defun mevedel--ov-actions-view (&optional instructions)
  "Display the patch buffer for INSTRUCTIONS."
  (interactive (list (mevedel--ov-actions-getov)))
  (when-let* ((patch (overlay-get instructions 'mevedel-directive-patch)))
    (mevedel--replace-patch-buffer patch)
    (let ((patch-buffer (mevedel--patch-buffer)))
      (if-let* ((patch-buffer-window (get-buffer-window patch-buffer)))
          (quit-window nil patch-buffer-window)
        (display-buffer patch-buffer)))))

(defun mevedel--ov-actions-cycle (&optional instructions)
  "Collapse or expand INSTRUCTIONS."
  (interactive (list (mevedel--ov-actions-getov)))
  (if (eq (overlay-get instructions 'mevedel-instruction-collapse-p)
          'collapse)
      (overlay-put instructions 'mevedel-instruction-collapse-p 'expand)
    (overlay-put instructions 'mevedel-instruction-collapse-p 'collapse))
  (mevedel--update-instruction-overlay instructions))

(defun mevedel--ov-actions-help (callback)
  "Eldoc documentation function for `mevedel' instruction actions.

CALLBACK is supplied by Eldoc, see `eldoc-documentation-functions'."
  (when-let* ((instruction-type (get-char-property (point) 'mevedel-instruction-type)))
    (funcall callback
             (format
              (pcase instruction-type
                (`reference (substitute-command-keys
                             "%s Options: show menu \\[mevedel--ov-actions-dispatch]"))
                (`directive
                 (pcase (get-char-property (point) 'mevedel-directive-status)
                   ('processing
                    (substitute-command-keys "%s Options: abort \\[mevedel--ov-actions-abort] or show menu \\[mevedel--ov-actions-dispatch]"))
                   (_
                    (substitute-command-keys
                     "%s Options: show menu \\[mevedel--ov-actions-dispatch]")))))
              (propertize (gptel--model-name gptel-model) 'face 'mode-line-emphasis)))))


(defvar-keymap mevedel-reference-actions-map
  :doc "Keymap for `mevedel' reference overlay actions at point."
  "M-m" #'mevedel--ov-actions-dispatch)

(defvar-keymap mevedel-directive-actions-map
  :doc "Keymap for `mevedel' directive overlay actions at point."
  "M-m" #'mevedel--ov-actions-dispatch)

(defvar-keymap mevedel-directive-processing-actions-map
  :doc "Keymap for `mevedel' processing directive overlay actions at point."
  "M-m" #'mevedel--ov-actions-dispatch
  "C-c C-k" #'mevedel--ov-actions-abort)

(defvar-keymap mevedel-directive-succeeded-actions-map
  :doc "Keymap for `mevedel' succeeded directive overlay actions at point."
  "M-m" #'mevedel--ov-actions-dispatch)

(defvar-keymap mevedel-directive-failed-actions-map
  :doc "Keymap for `mevedel' failed directive overlay actions at point."
  "M-m" #'mevedel--ov-actions-dispatch)

(defvar mevedel--actions-maps '(mevedel-reference-actions-map
                                mevedel-directive-actions-map
                                mevedel-directive-processing-actions-map
                                mevedel-directive-succeeded-actions-map
                                mevedel-directive-failed-actions-map))

(defun mevedel--update-instruction-overlay (instruction &optional update-children)
  "Update the appearance of the INSTRUCTION overlay.

This function updates the overlay label text, color of the label text,
and the background of the instruction overlay.  This function should be
called every time there is a hierarchy or status change in the
instruction overlay that we wish to reflect.

Also updates the child instructions of the INSTRUCTION, if
UPDATE-CHILDREN is non-nil."
  (cl-labels
      ((directive-color (directive)
         (cl-labels ((dircol ()
                       (pcase (overlay-get directive 'mevedel-directive-status)
                         ('processing mevedel-directive-processing-color)
                         ('succeeded  mevedel-directive-success-color)
                         ('failed     mevedel-directive-fail-color)
                         (_           mevedel-directive-color))))
           (if-let* ((parent-directive (mevedel--topmost-instruction directive 'directive)))
               (let ((parent-status (overlay-get parent-directive 'mevedel-directive-status)))
                 (if (eq parent-status 'processing)
                     mevedel-directive-processing-color
                   (if (eq parent-status 'failed)
                       mevedel-directive-fail-color
                     (dircol))))
             (dircol))))
       (aux (instruction &optional update-children priority (parent nil))
         (let* ((instruction-type (mevedel--instruction-type instruction))
                (padding (with-current-buffer (overlay-buffer instruction)
                           (save-excursion
                             (goto-char (overlay-start instruction))
                             (make-string (current-column) ? ))))
                (is-bufferlevel (mevedel--instruction-bufferlevel-p instruction))
                (parent-bufferlevel (and parent (mevedel--instruction-bufferlevel-p parent)))
                ;; This is to prevent the buffer-level instruction from having a
                ;; background color.
                (priority (if is-bufferlevel (1- priority) priority))
                (label "")
                color)
           (cl-labels
               ((action-setup ()
                  (add-hook 'eldoc-documentation-functions #'mevedel--ov-actions-help nil 'local)
                  (overlay-put instruction 'keymap (pcase instruction-type
                                                     (`reference mevedel-reference-actions-map)
                                                     (`directive
                                                      (pcase (overlay-get instruction 'mevedel-directive-status)
                                                        ('processing mevedel-directive-processing-actions-map)
                                                        ('succeeded mevedel-directive-succeeded-actions-map)
                                                        ('failed mevedel-directive-failed-actions-map)
                                                        (_ mevedel-directive-actions-map)))))
                  (overlay-put
                   instruction 'help-echo
                   (format (concat "%s \\[mevedel--ov-actions-dispatch] for options")
                           (pcase instruction-type
                             (`reference "Press")
                             (`directive
                              (pcase (overlay-get instruction 'mevedel-directive-status)
                                ('processing "Request in progress, press")
                                ('succeeded "Request succeeded, press")
                                ('failed "Request failed, press")
                                (_ "Press")))))))

                (append-to-label (content &optional prefix)
                  (setq label
                        (concat label
                                (if (string-empty-p label) "" (concat "\n" padding))
                                (mevedel--fill-label-string content
                                                            (or prefix "")
                                                            padding
                                                            (overlay-buffer instruction)))))
                (stylized-id-str (id)
                  (propertize (format "#%d" id) 'face 'font-lock-constant-face))
                (append-links-to-label ()
                  (cl-labels ((filter-ids (ids)
                                (cl-loop for id in ids
                                         unless
                                         (let ((instr (mevedel--instruction-with-id id)))
                                           (or (null instr)
                                               (not (eq (mevedel--instruction-type instr)
                                                        instruction-type))))
                                         collect id)))
                    (let ((outlinks (filter-ids (mevedel--instruction-outlinks instruction)))
                          (inlinks (filter-ids (mevedel--instruction-inlinks instruction))))
                      (when (or outlinks inlinks)
                        (let ((prefix (format "%s LINKS: "
                                              (if (eq instruction-type instruction)
                                                  "REFERENCE"
                                                "DIRECTIVE")))
                              (link-list-text
                               (concat
                                (when outlinks
                                  (format "TO: %s"
                                          (string-join (mapcar #'stylized-id-str
                                                               outlinks)
                                                       ", ")))
                                (when inlinks
                                  (format "%sFROM: %s"
                                          (if outlinks "\n" "")
                                          (string-join (mapcar #'stylized-id-str
                                                               inlinks)
                                                       ", "))))))
                          (append-to-label link-list-text
                                           prefix)))))))
             (pcase instruction-type
               ('reference ; REFERENCE
                (setq color mevedel-reference-color)
                (if (and parent
                         (and (eq (mevedel--instruction-type parent) 'reference)
                              (not parent-bufferlevel)))
                    (append-to-label (format "SUBREFERENCE %s"
                                             (stylized-id-str (mevedel--instruction-id instruction))))
                  (if is-bufferlevel
                      (append-to-label (format "BUFFER REFERENCE %s"
                                               (stylized-id-str (mevedel--instruction-id instruction))))
                    (append-to-label (format "REFERENCE %s"
                                             (stylized-id-str (mevedel--instruction-id instruction))))))
                (let* ((direct-tags (mevedel--reference-tags instruction))
                       (inherited-tags (mevedel--inherited-tags instruction))
                       (common-tags (cl-intersection inherited-tags direct-tags))
                       (unique-tags (cl-set-difference direct-tags common-tags)))
                  (cl-labels
                      ((propertized-string-from-tags (tags)
                         (string-join
                          (mapcar (lambda (tag)
                                    (propertize (symbol-name tag)
                                                'face
                                                (if (memq tag common-tags)
                                                    'font-lock-warning-face
                                                  'font-lock-constant-face)))
                                  tags)
                          " ")))
                    (when inherited-tags
                      (append-to-label (propertized-string-from-tags
                                        (sort (append inherited-tags) #'string-lessp))
                                       (if common-tags
                                           "INHERITED & COMMON TAGS: "
                                         "INHERITED TAGS: ")))
                    (when unique-tags
                      (append-to-label (propertized-string-from-tags
                                        (sort unique-tags #'string-lessp))
                                       (if inherited-tags
                                           (if common-tags
                                               "UNIQUE TAGS: "
                                             "DIRECT TAGS: ")
                                         "TAGS: ")))))
                (append-links-to-label)
                (let ((commentary (string-trim (or (if (eq (overlay-get instruction 'mevedel-instruction-collapse-p)
                                                           'collapse)
                                                       (mevedel--commentary-truncated-text instruction)
                                                     (mevedel--commentary-text instruction))
                                                   ""))))
                  (unless (string-empty-p commentary)
                    (append-to-label commentary "COMMENTARY: ")))
                (action-setup))
               ('directive ; DIRECTIVE
                (pcase (overlay-get instruction 'mevedel-directive-status)
                  ('processing (append-to-label "PROCESSING"))
                  ('succeeded (append-to-label "SUCCEEDED"))
                  ('failed (append-to-label (overlay-get instruction
                                                         'mevedel-directive-fail-reason)
                                            "FAILED: ")))
                (setq color (directive-color instruction))
                (let (sublabel
                      (directive-typename "DIRECTIVE"))
                  (if (and parent
                           (mevedel--directivep parent))
                      (progn
                        (pcase (overlay-get parent 'mevedel-directive-status)
                          ((or 'processing 'failed)
                           (if-let* ((existing-typename (overlay-get instruction
                                                                     'mevedel-subdirective-typename)))
                               (setq directive-typename existing-typename)
                             (setq directive-typename "HINT")))
                          ('succeeded (setq directive-typename "CORRECTION"))
                          (_ (setq directive-typename "HINT")))
                        (setf (overlay-get instruction 'mevedel-subdirective-typename)
                              directive-typename))
                    (setf (overlay-get instruction 'mevedel-subdirective-typename) nil))
                  (setq sublabel (concat
                                  sublabel
                                  (format "%s %s"
                                          directive-typename
                                          (stylized-id-str (mevedel--instruction-id instruction)))))
                  (let ((directive (string-trim (or (if (eq (overlay-get instruction 'mevedel-instruction-collapse-p)
                                                            'collapse)
                                                        (mevedel--directive-truncated-text instruction)
                                                      (mevedel--directive-text instruction))
                                                    ""))))
                    (if (string-empty-p directive)
                        (setq sublabel (concat "EMPTY " sublabel))
                      (setq sublabel (concat sublabel ": ")))
                    (setq label (concat
                                 label
                                 (unless (string-empty-p label)
                                   (concat "\n" padding))
                                 (mevedel--fill-label-string directive
                                                             sublabel
                                                             padding
                                                             (overlay-buffer instruction))))
                    (unless (mevedel--parent-instruction instruction 'directive)
                      (if-let* ((query-string (overlay-get instruction
                                                           'mevedel-directive-infix-tag-query-string)))
                          (append-to-label query-string "TAG QUERY: ")
                        (let (matchinfo)
                          (if mevedel-empty-tag-query-matches-all
                              (setq matchinfo "REFERENCES ALL")
                            (if mevedel-always-match-untagged-references
                                (setq matchinfo "REFERENCES UNTAGGED ONLY")
                              (setq matchinfo "REFERENCES NOTHING")))
                          (setq label (concat label "\n" padding matchinfo))))
                      (append-links-to-label))))
                (action-setup)))
             (let* ((default-fg (face-foreground 'default))
                    (default-bg (face-background 'default))
                    (bg-tint-intensity
                     (if (and parent (not parent-bufferlevel))
                         (* mevedel-subinstruction-tint-coefficient mevedel-instruction-bg-tint-intensity)
                       mevedel-instruction-bg-tint-intensity))
                    (label-color (if is-bufferlevel
                                     (mevedel--tint default-fg color mevedel-instruction-label-tint-intensity)
                                   (let ((tint (mevedel--tint default-fg
                                                              color
                                                              mevedel-instruction-label-tint-intensity)))
                                     (dotimes (_  (- priority
                                                     mevedel--default-instruction-priority))
                                       (setq tint (mevedel--tint tint
                                                                 color
                                                                 mevedel-instruction-label-tint-intensity)))
                                     tint)))
                    ;; We want to make sure that the buffer-level instructions don't superfluously
                    ;; tint the background.
                    (bg-color (if (and is-bufferlevel (eq instruction-type 'reference))
                                  default-bg
                                (let ((tint (mevedel--tint default-bg
                                                           color
                                                           mevedel-instruction-bg-tint-intensity)))
                                  (dotimes (_ (- priority
                                                 mevedel--default-instruction-priority))
                                    (setq tint (mevedel--tint tint color bg-tint-intensity)))
                                  tint))))
               (overlay-put instruction 'mevedel-bg-color bg-color)
               (overlay-put instruction 'mevedel-label-color label-color)
               (overlay-put instruction 'priority priority)
               (when (eq instruction
                         mevedel--highlighted-instruction)
                 (setq bg-color
                       (mevedel--tint default-bg
                                      mevedel-highlighted-instruction-color
                                      mevedel-highlighted-instruction-tint-intensity)))
               (let ((instruction-is-at-eol (with-current-buffer (overlay-buffer instruction)
                                              (save-excursion
                                                (goto-char (overlay-end instruction))
                                                (eolp)))))
                 ;; Propertize specific parts of the before-string of the label, to give
                 ;; the illusion that its a "sticker" in the buffer.
                 (cl-labels
                     ((colorize-region (beg end &optional fg bg)
                        (unless (= beg end)
                          (let ((fg (or fg label-color))
                                (bg (or bg bg-color)))
                            (add-face-text-property beg end
                                                    (list :inherit 'default
                                                          :extend t
                                                          :foreground fg
                                                          :background bg)
                                                    t))))
                      (colorize-region-as-parent (beg end)
                        (when-let* ((parent (mevedel--parent-instruction instruction)))
                          (colorize-region beg end
                                           (overlay-get parent 'mevedel-label-color)
                                           (overlay-get parent 'mevedel-bg-color)))))
                   (let ((before-string
                          (with-temp-buffer
                            (insert label)
                            (if (mevedel--bodyless-instruction-p instruction)
                                (unless instruction-is-at-eol
                                  (insert "\n"))
                              (insert "\n"))
                            (goto-char (point-min))
                            (end-of-line)
                            (unless (eobp)
                              (forward-char))
                            (colorize-region (point-min) (point))
                            (goto-char (point-min))
                            (forward-line)
                            (while (not (eobp))
                              (beginning-of-line)
                              (let ((mark (point)))
                                (forward-char (length padding))
                                (colorize-region-as-parent mark (point)))
                              (let ((went-to-next-line))
                                (let ((mark (point)))
                                  (end-of-line)
                                  (unless (eobp)
                                    (setq went-to-next-line t)
                                    (forward-char))
                                  (colorize-region mark (point)))
                                (unless went-to-next-line
                                  (forward-line))))
                            (unless (mevedel--bodyless-instruction-p instruction)
                              (let ((mark (point)))
                                (insert padding)
                                (colorize-region-as-parent mark (point))))
                            (buffer-string))))
                     (overlay-put instruction 'before-string before-string))))
               (overlay-put instruction 'face `(:extend t :background ,bg-color)))
             (when update-children
               (dolist (child (mevedel--child-instructions instruction))
                 (aux child update-children (1+ priority) instruction)))))))
    (let ((instructions-conflicting (cl-some (lambda (instr)
                                               (and (not (eq instr instruction))
                                                    (mevedel--instructions-congruent-p instruction
                                                                                       instr)))
                                             (mevedel--instructions-at (overlay-start instruction)))))
      (if instructions-conflicting
          ;; This instruction is causing conflicts, and therefore must be
          ;; deleted.
          (mevedel--delete-instruction instruction)
        (let ((parent (mevedel--parent-instruction instruction)))
          (let ((priority (if parent
                              (1+ (overlay-get parent 'priority))
                            mevedel--default-instruction-priority)))
            (aux instruction update-children priority parent)))))))

(defun mevedel--buffer-has-instructions-p (buffer)
  "Return non-nil if BUFFER has any mevedel instructions associated with it."
  (mevedel--instruction-activate-buffer buffer)
  (assoc buffer (mevedel--instruction-alist)))

(defun mevedel--wholly-contained-instructions (buffer start end)
  "Return mevedel overlays in BUFFER that are entirely within START and END."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'mevedel-instruction)
                             (>= (overlay-start ov) start)
                             (<= (overlay-end ov) end)))
                      (overlays-in start end))))

(defun mevedel--instructions-at (point &optional type)
  "Return a list of instructions at current POINT.

Optionally return only instructions of specific TYPE.  Also returns
bodyless overlays located right before the point."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'mevedel-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'mevedel-instruction-type)
                                        type))
                               (null type))))
                    (overlays-in point
                                 (min (point-max) (1+ point)))))

(defun mevedel--instructions-in (start end &optional type)
  "Return a list of instructions in region delimited by START and END.

Optionally return only instructions of specific TYPE."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'mevedel-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'mevedel-instruction-type)
                                        type))
                               (null type))))
                    (overlays-in start end)))

(defun mevedel--partially-contained-instructions (buffer start end)
  "Return instructions in BUFFER that overlap with START and END.

Does not return instructions that contain the region in its entirety."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'mevedel-instruction)
                             (or (<= (overlay-start ov) start)
                                 (>= (overlay-end ov) end))
                             (not (and (<= (overlay-start ov) start)
                                       (>= (overlay-end ov) end)))))
                      (overlays-in start end))))

(defun mevedel--all-instructions ()
  "Return a list of all currently loaded instructions."
  (mevedel--foreach-instruction inst collect inst))

(cl-defun mevedel--topmost-instruction (instruction &optional of-type pred)
  "Return the topmost instruction containing the INSTRUCTION, if any.

If OF-TYPE is non-nil, filter by the specified instruction OF-TYPE.  If
OF-TYPE is nil, the instruction returned is the top-level one.

If PRED is non-nil, then the best instruction must also satisfy it.  The
PRED must be a function which accepts an instruction."
  (unless instruction
    (cl-return-from mevedel--topmost-instruction nil))
  (with-current-buffer (overlay-buffer instruction)
    (let ((best-instruction instruction))
      (cl-labels ((parent-instr (instr)
                    (if-let* ((parent (mevedel--parent-instruction instr)))
                        (progn
                          (when (and (or (null of-type) (eq of-type (mevedel--instruction-type parent)))
                                     (or (null pred) (funcall pred parent)))
                            (setq best-instruction parent))
                          (parent-instr parent))
                      best-instruction)))
        (setq best-instruction (parent-instr instruction)))
      (if (and (or (null of-type) (eq of-type (mevedel--instruction-type best-instruction)))
               (or (null pred) (funcall pred best-instruction)))
          best-instruction
        nil))))

(defun mevedel--directive-text (directive)
  "Return the directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (or (overlay-get directive 'mevedel-directive) ""))

(defun mevedel--directive-truncated-text (directive)
  "Return the truncated directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (or (overlay-get directive 'mevedel-directive-truncated) ""))

(defun mevedel--commentary-text (reference)
  "Return the commentary text of the REFERENCE overlay.

Returns an empty string if there is no commentary."
  (or (overlay-get reference 'mevedel-commentary) ""))

(defun mevedel--commentary-truncated-text (reference)
  "Return the truncated commentary text of the REFERENCE overlay.

Returns an empty string if there is no commentary."
  (or (overlay-get reference 'mevedel-commentary-truncated) ""))

(defcustom mevedel-instructions-truncated-max 100
  "Maximum display length for truncated directive text.
Used by `mevedel-truncate-directive' to limit the length of directive
text shown in UI elements such as the minibuffer prompt."
  :type 'integer
  :group 'mevedel)

(defun mevedel-truncate-directive (text)
  "Truncate TEXT to `mevedel-instructions-truncated-max' characters.

Returns TEXT truncated if longer than the maximum, otherwise returns
TEXT unchanged.  Truncation uses ellipsis to indicate omitted content."
  (truncate-string-to-width
   text mevedel-instructions-truncated-max nil nil
   t))

(defun mevedel--read-directive (directive)
  "Prompt user to enter a directive text via minibuffer for DIRECTIVE."
  (let ((original-directive-text (mevedel--directive-text directive))
        (original-directive-status (overlay-get directive 'mevedel-directive-status))
        (set-directive-text (lambda (directive text)
                              (let ((text-truncated (mevedel-truncate-directive text)))
                                (overlay-put directive 'mevedel-directive text)
                                (overlay-put directive 'mevedel-directive-truncated text-truncated)
                                (unless (overlay-get directive 'mevedel-instruction-collapse-p)
                                  (overlay-put directive 'mevedel-instruction-collapse-p
                                               (if (> (length text)
                                                      mevedel-instructions-truncated-max)
                                                   'collapse
                                                 'expand)))))))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((directive-text (minibuffer-contents)))
                        (funcall set-directive-text directive directive-text)
                        (mevedel--update-instruction-overlay directive)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put directive 'mevedel-directive (minibuffer-contents))
                      (overlay-put directive 'mevedel-directive-status nil)
                      (mevedel--update-instruction-overlay directive))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Directive: " original-directive-text)
        (quit
         (if (string-empty-p original-directive-text)
             (mevedel--delete-instruction directive)
           (funcall set-directive-text directive original-directive-text)
           (overlay-put directive 'mevedel-directive-status original-directive-status)
           (mevedel--update-instruction-overlay directive nil))
         (signal 'quit nil))))))

(defun mevedel--read-commentary (reference)
  "Prompt user to enter a commentary text via minibuffer for REFERENCE."
  (let ((original-commentary-text (mevedel--commentary-text reference))
        (set-commentary-text (lambda (reference text)
                               (let ((text-truncated (mevedel-truncate-directive text)))
                                 (overlay-put reference 'mevedel-commentary text)
                                 (overlay-put reference 'mevedel-commentary-truncated text-truncated)
                                 (unless (overlay-get reference 'mevedel-instruction-collapse-p)
                                   (overlay-put reference 'mevedel-instruction-collapse-p
                                                (if (> (length text)
                                                       mevedel-instructions-truncated-max)
                                                    'collapse
                                                  'expand)))))))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((commentary-text (minibuffer-contents)))
                        (funcall set-commentary-text reference commentary-text)
                        (mevedel--update-instruction-overlay reference)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put reference 'mevedel-commentary (minibuffer-contents))
                      (mevedel--update-instruction-overlay reference))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Commentary: " original-commentary-text)
        (quit
         (funcall set-commentary-text reference original-commentary-text)
         (mevedel--update-instruction-overlay reference nil))
        (signal 'quit nil)))))

(cl-defun mevedel--ancestral-instructions (instruction &optional of-type)
  "Return a list of ancestors for the current INSTRUCTION.

Optionally filer the by OF-TYPE (either reference or directive)."
  (if-let* ((parent (mevedel--parent-instruction instruction)))
      (if (or (null of-type)
              (eq (mevedel--instruction-type parent) of-type))
          (cons parent (mevedel--ancestral-instructions parent of-type))
        (mevedel--ancestral-instructions parent of-type))
    nil))

(defun mevedel--context (&optional query directive)
  "Get context plist.

Returns plist with :summary and :references keys, optionally for
specified DIRECTIVE and tag QUERY."
  (let* ((pred
          (lambda (instr)
            (mevedel--reference-matches-query-p instr
                                                (or query
                                                    (when directive
                                                      (overlay-get directive
                                                                   'mevedel-directive-prefix-tag-query))))))
         (used-commentary-refs (make-hash-table))
         (toplevel-refs (mevedel--foreach-instruction instr
                          when (and (mevedel--referencep instr)
                                    (eq (mevedel--topmost-instruction instr 'reference pred)
                                        instr))
                          collect instr))
         (linked-refs (let ((visited-refs (make-hash-table))
                            (independent-refs ())
                            (child-refmap
                             (let ((ht (make-hash-table)))
                               (cl-loop for tlr in toplevel-refs
                                        do (cl-loop for instr in (mevedel--wholly-contained-instructions
                                                                  (overlay-buffer tlr)
                                                                  (overlay-start tlr)
                                                                  (overlay-end tlr))
                                                    when (and (not (eq instr tlr))
                                                              (mevedel--referencep instr))
                                                    do (puthash instr t ht)))
                               ht)))
                        (cl-labels ((collect-linked-references-recursively (ref)
                                      (puthash ref t visited-refs)
                                      (dolist (linked-id (mevedel--instruction-outlinks ref))
                                        (let ((linked-ref (mevedel--instruction-with-id linked-id)))
                                          (when (and linked-ref
                                                     (mevedel--referencep linked-ref)
                                                     (not (gethash linked-ref visited-refs)))
                                            (unless (gethash linked-ref child-refmap)
                                              (push linked-ref independent-refs))
                                            (collect-linked-references-recursively linked-ref))))))
                          (mapc #'collect-linked-references-recursively
                                (cl-remove-duplicates
                                 (append (when directive
                                           (mevedel--ancestral-instructions directive 'reference))
                                         toplevel-refs
                                         (flatten-tree
                                          (mapcar (lambda (instr)
                                                    (mevedel--ancestral-instructions instr 'reference))
                                                  toplevel-refs)))))
                          independent-refs)))
         (total-refs (cl-remove-if (lambda (ref)
                                     (and directive (mevedel--subinstruction-of-p ref directive)))
                                   (cl-union toplevel-refs linked-refs)))
         (reference-alist (cl-loop for reference in total-refs with alist = ()
                                   do (push reference (alist-get (overlay-buffer reference) alist))
                                   finally (progn
                                             (cl-loop for (_ . references) in alist
                                                      do (sort references
                                                               (lambda (x y)
                                                                 (< (overlay-start x)
                                                                    (overlay-start y)))))
                                             (cl-return alist)))))
    (with-temp-buffer
      (insert (format "### Reference%s"
                      (if (> (length total-refs) 1) "s" "")))
      (cl-loop for (buffer . references) in reference-alist
               do (dolist (ref references)
                    (cl-destructuring-bind (ref-info-string ref-string)
                        (mevedel--overlay-region-info ref)
                      (let ((markdown-delimiter
                             (mevedel--delimiting-markdown-backticks ref-string)))
                        (insert
                         (concat
                          "\n\n"
                          (format "#### Reference #%d" (mevedel--instruction-id ref))
                          "\n\n"
                          (format "%s"
                                  (let ((rel-path
                                         (with-current-buffer buffer
                                           (file-relative-name
                                            (buffer-file-name buffer)
                                            (mevedel-workspace-root
                                             (mevedel-workspace))))))
                                    (if (mevedel--instruction-bufferlevel-p ref)
                                        (format "File `%s`" rel-path)
                                      (format "In file `%s`, %s" rel-path ref-info-string))))
                          (if (or (mevedel--instruction-bufferlevel-p ref)
                                  (not mevedel-include-full-instructions))
                              "."
                            (concat
                             ":"
                             (format "\n\n%s\n%s\n%s"
                                     markdown-delimiter
                                     ref-string
                                     markdown-delimiter)))

                          (let ((commentary (mevedel--commentary-text ref)))
                            (unless (string-empty-p commentary)
                              (puthash ref t used-commentary-refs)
                              (format "\n\nCommentary:\n\n%s"
                                      (mevedel--markdown-enquote commentary))))))))))
      (list :summary (if reference-alist (buffer-string) "")
            :references reference-alist))))

(defun mevedel--directive-llm-prompt (directive)
  "Craft the prompt for the LLM model associated with the DIRECTIVE."
  (when (mevedel--directive-empty-p directive)
    (error "Directive %s is empty" directive))
  (let* ((context (mevedel--context nil directive))
         (reference-count (length (flatten-tree (mapcar #'cdr (plist-get context :references)))))
         (directive-toplevel-reference (mevedel--topmost-instruction directive 'reference))
         (directive-buffer (overlay-buffer directive))
         (directive-filename (buffer-file-name directive-buffer))
         (directive-filename-relpath
          (when directive-filename
            (with-current-buffer directive-buffer
              (file-relative-name
               directive-filename
               (mevedel-workspace-root (mevedel-workspace)))))))
    (cl-destructuring-bind (directive-region-info-string directive-region-string)
        (mevedel--overlay-region-info directive)
      (let ((expanded-directive-text
             (let ((secondary-directives
                    (cl-remove-if-not (lambda (inst)
                                        (and (eq (mevedel--instruction-type inst) 'directive)
                                             (not (eq inst directive))))
                                      (mevedel--wholly-contained-instructions
                                       (overlay-buffer directive)
                                       (overlay-start directive)
                                       (overlay-end directive))))
                   (sd-typename (if (not (eq (overlay-get directive 'mevedel-directive-status)
                                             'succeeded))
                                    "hint"
                                  "correction")))
               (concat
                (if (mevedel--instruction-bufferlevel-p directive)
                    ""
                  (concat
                   (format ", %s" directive-region-info-string)
                   (if (or (string-empty-p directive-region-string)
                           (not mevedel-include-full-instructions))
                       "."
                     (let ((markdown-delimiter
                            (mevedel--delimiting-markdown-backticks directive-region-string)))
                       (concat
                        (format ", which correspond%s to:"
                                (if (mevedel--multiline-string-p directive-region-string) "" "s"))
                        "\n\n"
                        (format "%s\n%s\n%s"
                                markdown-delimiter
                                directive-region-string
                                markdown-delimiter))))))
                "\n\n"
                (if (not (string-empty-p (mevedel--directive-text directive)))
                    (format "The directive is:\n\n%s"
                            (mevedel--markdown-enquote (overlay-get directive 'mevedel-directive)))
                  (format "The directive is composed entirely out of %ss, so you should \
treat them as subdirectives, instead."
                          sd-typename))
                (cl-loop for sd in secondary-directives
                         when (not (string-empty-p (mevedel--directive-text sd)))
                         concat (concat
                                 "\n\n"
                                 (cl-destructuring-bind (sd-region-info sd-region)
                                     (mevedel--overlay-region-info sd)
                                   (concat
                                    (format "For file `%s`, %s"
                                            directive-filename-relpath
                                            sd-region-info)
                                    (let ((sd-text (mevedel--markdown-enquote
                                                    (overlay-get sd 'mevedel-directive))))
                                      (if (mevedel--bodyless-instruction-p sd)
                                          (format ", you have a %s:\n\n%s"
                                                  sd-typename
                                                  sd-text)
                                        (let ((markdown-delimiter
                                               (mevedel--delimiting-markdown-backticks
                                                sd-region)))
                                          (concat
                                           (if mevedel-include-full-instructions
                                               (format ", which correspond%s to:\n\n%s"
                                                       (if (mevedel--multiline-string-p sd-region)
                                                           "" "s")
                                                       (format "%s\n%s\n%s"
                                                               markdown-delimiter
                                                               sd-region
                                                               markdown-delimiter))
                                             ".")
                                           (format "\n\nYou have the %s:\n\n%s"
                                                   sd-typename
                                                   sd-text)))))))))))))
        (with-temp-buffer
          (insert
           (concat
            "Listed below" (pcase reference-count
                             (0 " is a")
                             (1 " is a single reference and a")
                             (_ " are references and a"))
            " directive."
            (when directive-toplevel-reference
              (format " Note that the directive is embedded within %s reference."
                      (if (> reference-count 1) "the" "a")))
            (unless (zerop reference-count)
              (concat "\n\n"
                      (plist-get context :summary)))
            (format "\n\n### Directive\n\n")
            (format "For %s%s"
                    (if directive-filename
                        (format "file `%s`" directive-filename-relpath)
                      (format "buffer `%s`" (buffer-name directive-buffer)))
                    expanded-directive-text)
            (when directive-toplevel-reference
              (concat "\n\n"
                      "Recall that the directive is embedded within "
                      (format "reference #%d in %s."
                              (mevedel--instruction-id directive-toplevel-reference)
                              directive-region-info-string)))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun mevedel--create-id ()
  "Create a unique identifier for an instruction.

Retrieves an unused ID from retired IDs or generates a new one by
incrementing the ID counter.  Tracks ID usage via a hash table."
  (let ((id
         (if (mevedel--instruction-retired-ids)
             (prog1
                 (car (mevedel--instruction-retired-ids))
               (setf (mevedel--instruction-retired-ids) (cdr (mevedel--instruction-retired-ids))))
           (cl-incf (mevedel--instruction-id-counter)))))
    (puthash id t (mevedel--instruction-id-usage-map))
    id))

(defun mevedel--retire-id (id)
  "Retire an ID by removing it from `(mevedel--instruction-id-usage-map)'.
The id is added to `(mevedel--instruction-retired-ids)'"
  (when (gethash id (mevedel--instruction-id-usage-map))
    (remhash id (mevedel--instruction-id-usage-map))
    (push id (mevedel--instruction-retired-ids))))

(defun mevedel--instruction-outlinks (instruction)
  "Return the :to links of INSTRUCTION."
  (plist-get (overlay-get instruction 'mevedel-links) :to))

(defun mevedel--instruction-inlinks (instruction)
  "Return the :from links of INSTRUCTION."
  (plist-get (overlay-get instruction 'mevedel-links) :from))

(provide 'mevedel-overlays)
;;; mevedel-overlays.el ends here
