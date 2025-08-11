;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'macher-instruct-utilities)

;; `macher'
(declare-function macher-workspace "ext:macher" (&optional buffer))
(declare-function macher--workspace-root "ext:macher" (workspace))


(defcustom macher-instruct-reference-color "yellow"
  "Color to be used as a tint for reference overlays."
  :type 'string
  :group 'macher-instruct)

(defcustom macher-instruct-directive-color "orange"
  "Color to be used as a tint for directive overlays."
  :type 'string
  :group 'macher-instruct)

(defcustom macher-instruct-directive-processing-color "cyan"
  "Color to be used as a tint for directives being processed by the model."
  :type 'string
  :group 'macher-instruct)

(defcustom macher-instruct-directive-success-color "green"
  "Color to be used as a tint for directives successfully processed by the model."
  :type 'string
  :group 'macher-instruct)

(defcustom macher-instruct-directive-fail-color "red"
  "Color to be used as a tint for directives the model could not process."
  :type 'string
  :group 'macher-instruct)

(defcustom macher-instruct-highlighted-instruction-color "cyan"
  "Color for currently highlighted instructions."
  :type 'string
  :group 'macher-instruct)

(defcustom macher-instruct-instruction-bg-tint-intensity 0.1
  "Default intensity for background tinting of instructions."
  :type 'float
  :group 'macher-instruct)

(defcustom macher-instruct-instruction-label-tint-intensity 0.2
  "Default intensity for label tinting of instructions."
  :type 'float
  :group 'macher-instruct)

(defcustom macher-instruct-highlighted-instruction-tint-intensity 0.2
  "Default intensity for tinting of highlighted instructions."
  :type 'float
  :group 'macher-instruct)

(defcustom macher-instruct-subinstruction-tint-coefficient 0.4
  "Coeffecient multiplied by by tint intensities.

Only applicable to the subinstructions. Makes it possible to have more a
more finely-tuned control over how tinting looks.

Does not affect the label colors, just the backgrounds."
  :type 'float
  :group 'macher-instruct)

(defcustom macher-instruct-empty-tag-query-matches-all t
  "Determines behavior of directives without a tag search query.

If set to t, directives without a specific tag search query will use all
available references. Alternatively, if this is set to nil, directives
without a search query will not use any references."
  :type 'boolean
  :group 'macher-instruct)

(defcustom macher-instruct-always-match-untagged-references t
  "Controls inclusion of untagged references in directive prompts.

When set to t, untagged references are always incorporated into
directive references, ensuring comprehensive coverage. Conversely, when
set to nil, untagged references are ignored, unless
`macher-instruct-empty-tag-query-matches-all' is set to t.

A reference is considered untagged when it has no direct tags.
References can inherit tags from ancestor references and still be
considered untagged."
  :type 'boolean
  :group 'macher-instruct)

(defvar macher-instruct--instructions ()
  "Association list mapping buffers or files to lists of instruction overlays.")
(defvar macher-instruct--default-instruction-priority -99)
(defvar macher-instruct--highlighted-instruction nil)
(defvar macher-instruct--id-counter 0)
(defvar macher-instruct--id-usage-map (make-hash-table))
(defvar macher-instruct--retired-ids ())

(defmacro macher-instruct--foreach-instruction (binding &rest body)
  "Iterate over `macher-instruct--instructions' with BINDING as the binding.

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
                          (not (overlay-get instr 'macher-instruct-marked-for-deletion))))
                   (clean-alist-entry (cons)
                     (mapc (lambda (instr)
                             (macher-instruct--delete-instruction instr (car cons)))
                           (cl-remove-if-not #'trashp (cdr cons)))
                     (let ((instrs (cl-remove-if #'trashp (cdr cons))))
                       (setf (cdr cons) instrs))))
         (let ((,specific-buffer ,(if (listp binding) (cadr binding) nil)))
           (if (null ,specific-buffer)
               (cl-loop for ,cons in macher-instruct--instructions
                        do (let ((,bof (car ,cons)))
                             (if (stringp ,bof) ; bof is a file, restore it.
                                 (macher-instruct--restore-file-instructions ,bof)
                               (clean-alist-entry ,cons)))) ; bof is a buffer, clean it.
             (when (stringp ,specific-buffer)
               (cl-destructuring-bind (buffer _ _) (macher-instruct--restore-file-instructions ,specific-buffer)
                 (setq ,specific-buffer buffer)))
             (when-let ((cons (assoc ,specific-buffer macher-instruct--instructions)))
               (clean-alist-entry cons)))
           ;; Remove empty cons cells from the alist.
           (setq macher-instruct--instructions (cl-remove-if (lambda (cons)
                                                               (null (cdr cons)))
                                                             macher-instruct--instructions))
           ;; The instructions alist should now be cleaned of deleted
           ;; instructions.
           (cl-loop for ,instr
                    in (if ,specific-buffer
                           (alist-get ,specific-buffer macher-instruct--instructions)
                         (flatten-tree
                          (cl-remove nil
                                     (mapcar (lambda (plist-or-instrs)
                                               (if (plist-get plist-or-instrs :instructions)
                                                   nil ; Plist
                                                 plist-or-instrs))
                                             (mapcar #'cdr macher-instruct--instructions)))))
                    ,@body))))))

(defun macher-instruct-link-instructions (from-list to-list)
  "Link instructions with ids in FROM-LIST to those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (let ((completion-table (mapcar #'number-to-string (hash-table-keys macher-instruct--id-usage-map))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link to: "
                                             completion-table nil t)))))
  (cl-labels
      ((update-links (instr-id num-key update-id)
         (let* ((instr (macher-instruct--instruction-with-id instr-id))
                (links (overlay-get instr 'macher-instruct-links))
                (ids (plist-get links num-key)))
           (unless (member update-id ids)
             (setq ids (cons update-id ids))
             (overlay-put instr 'macher-instruct-links (plist-put links num-key ids))
             t))))
    (let ((new-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let ((from-instr (macher-instruct--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when (/= from-id to-id)
              (when-let ((to-instr (macher-instruct--instruction-with-id to-id)))
                (when (and (update-links from-id :to to-id)
                           (update-links to-id :from from-id))
                  (puthash from-instr t involved-instrs)
                  (puthash to-instr t involved-instrs)
                  (cl-incf new-link-count)))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (macher-instruct--update-instruction-overlay instr))
      (when (called-interactively-p 'interactive)
        (message "Created %d instruction link%s"
                 new-link-count
                 (if (= new-link-count 1) "" "s"))))))

(defun macher-instruct-unlink-instructions (from-list to-list)
  "Unlink instructions with ids in FROM-LIST from those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction
ids."
  (interactive
   (let ((completion-table (mapcar #'number-to-string (hash-table-keys macher-instruct--id-usage-map))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink from: "
                                             completion-table nil t)))))
  (cl-labels
      ((remove-links (instr-id num-key remove-id)
         (let* ((instr (macher-instruct--instruction-with-id instr-id))
                (links (overlay-get instr 'macher-instruct-links))
                (ids (plist-get links num-key)))
           (when (member remove-id ids)
             (setq ids (remove remove-id ids))
             (overlay-put instr 'macher-instruct-links (plist-put links num-key ids))
             t))))
    (let ((removed-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let ((from-instr (macher-instruct--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when-let ((to-instr (macher-instruct--instruction-with-id to-id)))
              (when (and (remove-links from-id :to to-id)
                         (remove-links to-id :from from-id))
                (puthash from-instr t involved-instrs)
                (puthash to-instr t involved-instrs)
                (cl-incf removed-link-count))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (when (buffer-live-p (overlay-buffer instr))
                    (macher-instruct--update-instruction-overlay instr)))
      (when (called-interactively-p 'interactive)
        (message "Removed %d instruction link%s"
                 removed-link-count
                 (if (= removed-link-count 1) "" "s"))))))

(defun macher-instruct-cycle-instructions-at-point (point)
  "Cycle through instructions at POINT, highlighting them.

This command allows for cycling through overlapping instructions at a
point in the buffer and allows one to have better accuracy when instructions
overlap to the point where no other reasonable option is available."
  (interactive "d")
  (let ((instructions-at-point (macher-instruct--instructions-at point))
        (original-highlighted-instruction macher-instruct--highlighted-instruction))
    (cond
     ((null instructions-at-point)
      (setq macher-instruct--highlighted-instruction nil)
      (when (called-interactively-p 'any)
        (message "No instructions at point")))
     ((or (null macher-instruct--highlighted-instruction)
          (not (memq macher-instruct--highlighted-instruction instructions-at-point)))
      (setq macher-instruct--highlighted-instruction nil)
      (setq macher-instruct--highlighted-instruction (macher-instruct--highest-priority-instruction instructions-at-point)))
     (t
      (if-let ((parent (macher-instruct--parent-instruction macher-instruct--highlighted-instruction)))
          (setq macher-instruct--highlighted-instruction parent)
        (setq macher-instruct--highlighted-instruction nil))))
    (when macher-instruct--highlighted-instruction
      (macher-instruct--update-instruction-overlay macher-instruct--highlighted-instruction))
    (when original-highlighted-instruction
      (macher-instruct--update-instruction-overlay original-highlighted-instruction))
    macher-instruct--highlighted-instruction))

(defun macher-instruct-modify-directive ()
  "Modify the directive under the point."
  (interactive)
  (when-let ((directive (macher-instruct--highest-priority-instruction (macher-instruct--instructions-at (point) 'directive)
                                                                       t)))
    (when (eq (overlay-get directive 'macher-instruct-directive-status) 'processing)
      (overlay-put directive 'macher-instruct-directive-status nil))
    (let ((topmost-directive (macher-instruct--topmost-instruction directive 'directive)))
      (when (eq (overlay-get topmost-directive 'macher-instruct-directive-status) 'failed)
        (setf (overlay-get topmost-directive 'macher-instruct-directive-status) nil)
        (macher-instruct--update-instruction-overlay topmost-directive t)))
    (macher-instruct--read-directive directive)))

(defun macher-instruct-modify-reference-commentary ()
  "Modify the reference commentary under the point."
  (interactive)
  (when-let ((reference (macher-instruct--highest-priority-instruction (macher-instruct--instructions-at (point) 'reference)
                                                                       t)))
    (macher-instruct--read-commentary reference)))
;; DEPRECATED 2025-08-06:
;; (cl-defgeneric macher-instruct--process-directive (directive callback))
;; DEPRECATED 2025-08-06:
(cl-defgeneric macher-instruct--llm-client-name ())

;; DEPRECATED 2025-08-06:
(defun macher-instruct-process-directives ()
  "Send directives to model .

If a region is selected, send all directives within the region.
If a region is not selected and there is a directive under the point, send it."
  (interactive)
  (let ((count 0))
    (cl-labels ((execute (directive)
                  (unless (macher-instruct--being-processed-p directive)
                    (if (macher-instruct--directive-empty-p directive)
                        ;; There is no point in sending empty directives.
                        (macher-instruct--process-directive-llm-response "The directive is empty!"
                                                                         directive
                                                                         'empty-directive)
                      (macher-instruct--process-directive directive #'macher-instruct--process-directive-llm-response)
                      (overlay-put directive 'macher-instruct-directive-status 'processing)
                      (macher-instruct--update-instruction-overlay directive t)
                      (setq count (1+ count))))))
      (if (region-active-p)
          (when-let ((toplevel-directives
                      (cl-remove-duplicates
                       (mapcar (lambda (instr)
                                 (macher-instruct--topmost-instruction instr 'directive))
                               (macher-instruct--instructions-in (region-beginning)
                                                                 (region-end)
                                                                 'directive)))))
            (dolist (directive toplevel-directives)
              (execute directive)))
        (if-let ((directive (macher-instruct--topmost-instruction (macher-instruct--highest-priority-instruction
                                                                   (macher-instruct--instructions-at (point) 'directive)
                                                                   t)
                                                                  'directive)))
            (execute directive)
          (when-let ((toplevel-directives (cl-remove-duplicates
                                           (mapcar (lambda (instr)
                                                     (macher-instruct--topmost-instruction instr 'directive)
                                                     (without-restriction
                                                       (macher-instruct--instructions-in (point-min)
                                                                                         (point-max)
                                                                                         'directive)))))))
            (dolist (dir toplevel-directives)
              (execute dir)))))
      (if (> count 0)
          (message "Sent %d directive%s to %s for processing"
                   count
                   (if (> count 1) "s" "")
                   (macher-instruct--llm-client-name))
        (message "No directives sent to %s" (macher-instruct--llm-client-name))))))

(defun macher-instruct-delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were deleted.
Throw a user error if no instructions to delete were found."
  (interactive)
  (let ((deleted-count 0))
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (dolist (overlay (macher-instruct--wholly-contained-instructions (current-buffer) start end))
            (when (overlay-get overlay 'macher-instruct-instruction)
              (macher-instruct--delete-instruction overlay)
              (setq deleted-count (1+ deleted-count))))
          (when (> deleted-count 0)
            (deactivate-mark))
          (unless (> deleted-count 0)
            (user-error "No instructions to delete within the selected region")))
      (let ((overlay (macher-instruct--delete-instruction-at (point))))
        (when overlay
          (setq deleted-count 1))
        (unless overlay
          (user-error "No instruction to delete at point"))))
    (when (> deleted-count 0)
      (message "Deleted %d instruction%s" deleted-count (if (> deleted-count 1) "s" "")))))

(defun macher-instruct-delete-all-instructions ()
  "Delete all macher instructions across all buffers."
  (interactive)
  (let ((instr-count (length (macher-instruct--instructions))))
    (when (and (called-interactively-p 'any)
               (zerop instr-count))
      (user-error "No instructions to delete"))
    (when (and (called-interactively-p 'any)
               instr-count
               (not (y-or-n-p "Are you sure you want to delete all instructions?")))
      (user-error "Aborted")))
  (let ((buffer-count 0)
        (deleted-instr-count 0))
    (macher-instruct--foreach-instruction instr
      with buffer-hash = (make-hash-table)
      unless (gethash (overlay-buffer instr) buffer-hash)
      do (progn
           (puthash (overlay-buffer instr) t buffer-hash)
           (cl-incf buffer-count))
      do (progn
           (macher-instruct--delete-instruction instr)
           (cl-incf deleted-instr-count)))
    (when (not (zerop deleted-instr-count))
      (message "Deleted %d macher instruction%s in %d buffer%s"
               deleted-instr-count
               (if (= 1 deleted-instr-count) "" "s")
               buffer-count
               (if (= 1 buffer-count) "" "s"))))
  (setq macher-instruct--instructions nil)
  (macher-instruct--reset-id-counter))

(defun macher-instruct-convert-instructions ()
  "Convert instructions between reference and directive type.

If a region is selected, convert all instructions within the region.  If no
region is selected, convert only the highest priority instruction at point.

Bodyless directives cannot be converted to references.  Attempting to do so
will throw a user error."
  (interactive)
  (let* ((instructions (if (use-region-p)
                           (macher-instruct--instructions-in (region-beginning)
                                                             (region-end))
                         (cl-remove-if #'null
                                       (list (macher-instruct--highest-priority-instruction
                                              (macher-instruct--instructions-at (point))
                                              t)))))
         (num-instructions (length instructions))
         (converted-directives-to-references 0)
         (converted-references-to-directives 0))
    (if (= num-instructions 0)
        (user-error "No instructions to convert")
      (dolist (instr instructions)
        (cond
         ((macher-instruct--directivep instr)
          (unless (macher-instruct--bodyless-instruction-p instr)
            (overlay-put instr 'macher-instruct-instruction-type 'reference)
            (setq converted-directives-to-references (1+ converted-directives-to-references))))
         ((macher-instruct--referencep instr)
          (overlay-put instr 'macher-instruct-instruction-type 'directive)
          (setq converted-references-to-directives (1+ converted-references-to-directives)))
         (t
          (user-error "Unknown instruction type")))
        (macher-instruct--update-instruction-overlay instr t))
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

(defun macher-instruct-next-instruction ()
  "Cycle through instructions in the forward direction."
  (interactive)
  (unless (macher-instruct--cycle-instruction nil 'next)
    (macher-instruct--print-instruction-not-found 'next nil)))

(defun macher-instruct-previous-instruction ()
  "Cycle through instructions in the backward direction."
  (interactive)
  (unless (macher-instruct--cycle-instruction nil 'previous)
    (macher-instruct--print-instruction-not-found 'previous nil)))

(defun macher-instruct-next-reference ()
  "Cycle through references in the forward direction."
  (interactive)
  (unless (macher-instruct--cycle-instruction 'reference 'next)
    (macher-instruct--print-instruction-not-found 'next 'reference)))

(defun macher-instruct-previous-reference ()
  "Cycle through references in the backward direction."
  (interactive)
  (unless (macher-instruct--cycle-instruction 'reference 'previous)
    (macher-instruct--print-instruction-not-found 'previous 'reference)))

(defun macher-instruct-next-directive ()
  "Cycle through directives in the forward direction."
  (interactive)
  (unless (macher-instruct--cycle-instruction 'directive 'next)
    (macher-instruct--print-instruction-not-found 'next 'directive)))

(defun macher-instruct-previous-directive ()
  "Cycle through directives in the backward direction."
  (interactive)
  (unless (macher-instruct--cycle-instruction 'directive 'previous)
    (macher-instruct--print-instruction-not-found 'previous 'directive)))

(defun macher-instruct-preview-directive-prompt ()
  "Preview directive prompt at the current point.

This command is useful to see what is actually being sent to the model."
  (interactive)
  (let ((directive (macher-instruct--topmost-instruction (car (macher-instruct--instructions-at (point) 'directive))
                                                         'directive)))
    (let ((request-string (macher-instruct--directive-llm-prompt directive)))
      (let ((bufname "*macher-directive-preview*"))
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

(defun macher-instruct-modify-directive-tag-query ()
  "Prompt minibuffer to enter a tag search query for a directive.

The directive in question is either the directive under the curent
point.

A tag query is an _infix_ expression, containing symbol atoms and the
operator symbols: `and', `or', `not'. If no operator is present between
two expressions, then an implicit `and' operator is assumed.

Examples:
  (signature and function and doc)
  (not dog or not cat)
  (cat or dog or (sheep and black))
  ((cat and dog) or (dog and goose))"
  (interactive)
  (if-let ((directive (macher-instruct--topmost-instruction
                       (macher-instruct--highest-priority-instruction (macher-instruct--instructions-at (point)) t)
                       'directive)))
      (let ((query (macher-instruct--read-tag-query (substring-no-properties
                                                     (or
                                                      (overlay-get directive
                                                                   'macher-instruct-directive-infix-tag-query-string)
                                                      "")))))
        (macher-instruct--set-directive-tag-query directive query))
    (user-error "No directive at point")))

(defun macher-instruct-directive-undo (&optional arg)
  "Undo the last change of the directive history at point.

If ARG is nonzero, traverse the directive history backwards; otherwise,
forwards."
  (interactive "P")
  (let ((directive (macher-instruct--highest-priority-instruction
                    (macher-instruct--instructions-at (point) 'directive))))
    (if directive
        (macher-instruct--directive-next-history directive (not (null arg)))
      (user-error "No directive found at point"))))

(defun macher-instruct-add-tags (&optional reference)
  "Add tags to the reference under the point.

Adds specificly to REFERENCE if it is non-nil."
  (interactive)
  (let* ((instructions (macher-instruct--instructions-at (point) 'reference))
         (instr (or reference (macher-instruct--highest-priority-instruction instructions t))))
    (if instr
        (let* ((existing-tags (macher-instruct--available-tags))
               (input (completing-read-multiple "Add tags (or leave empty): "
                                                existing-tags nil nil))
               (new-tags (mapcar #'intern input)))
          (let ((added (macher-instruct--add-tags instr new-tags)))
            (message "%d tag%s added" added (if (= added 1) "" "s"))))
      (user-error "No reference at point"))))

(defun macher-instruct-remove-tags ()
  "Remove tags from the reference under the point."
  (interactive)
  (let* ((instructions (macher-instruct--instructions-at (point) 'reference))
         (instr (macher-instruct--highest-priority-instruction instructions t)))
    (if instr
        (let ((tags-list (macher-instruct--reference-tags instr)))
          (if (null tags-list)
              (user-error "Reference has no tags of its own to remove")
            ;; Prompt the user to remove tags.
            (let* ((input (completing-read-multiple "Remove tags: " tags-list nil t))
                   (tags-to-remove (mapcar #'intern input)))
              (let ((removed (macher-instruct--remove-tags instr tags-to-remove)))
                (message "%d tag%s removed" removed (if (= removed 1) "" "s"))))))
      (user-error "No reference at point"))))

(declare-function macher-instruct--instruction-with-id "macher-instruct" (target-id))
(let ((map (make-hash-table)))
  (cl-defun macher-instruct--instruction-with-id (target-id)
    "Return the instruction with the given integer TARGET-ID.

Returns nil if no instruction with the spcific id was found."
    (when-let ((instr (gethash target-id map)))
      (when (buffer-live-p instr)
        (cl-return-from macher-instruct--instruction-with-id instr)))
    (setq map (make-hash-table))
    (macher-instruct--foreach-instruction instr
      do (puthash (macher-instruct--instruction-id instr) instr map))
    (gethash target-id map)))

(defun macher-instruct--instruction-id (instruction)
  "Return unique identifier for INSTRUCTION overlay."
  (overlay-get instruction 'macher-instruct-id))

(defun macher-instruct--stashed-buffer-instructions (buffer)
  (macher-instruct--foreach-instruction (instr buffer)
    collect (list :overlay-start (overlay-start instr)
                  :overlay-end (overlay-end instr)
                  :properties (overlay-properties instr))))

(defun macher-instruct--stash-buffer (buffer &optional file-contents)
  (let ((instrs (macher-instruct--stashed-buffer-instructions buffer)))
    (when instrs
      (with-current-buffer buffer
        (let ((original-content (or file-contents (buffer-substring-no-properties (point-min)
                                                                                  (point-max)))))
          (setf (alist-get buffer macher-instruct--instructions)
                (list :original-content original-content
                      :instructions instrs)
                (car (assoc buffer macher-instruct--instructions))
                (buffer-file-name buffer))
          (mapc #'delete-overlay (macher-instruct--instructions-in (point-min) (point-max))))))))

(defun macher-instruct--reference-list-info (refs)
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
          (if-let ((line-ranges (gethash buffer bufhash)))
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

(defun macher-instruct--reference-list-info-string (refs)
  (cl-destructuring-bind (&key buffer-count line-count)
      (macher-instruct--reference-list-info refs)
    (let ((ref-count (length refs)))
      (format "%d hit%s in %d buffer%s, %d line%s"
              ref-count
              (if (= ref-count 1) "" "s")
              buffer-count
              (if (= buffer-count 1) "" "s")
              line-count
              (if (= line-count 1) "" "s")))))

(defun macher-instruct--read-tag-query (&optional default)
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
                                     (let ((refs (macher-instruct--filter-references
                                                  (macher-instruct--tag-query-prefix-from-infix query))))
                                       (setq minibuffer-message
                                             (macher-instruct--reference-list-info-string refs))))
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
                (macher-instruct--tag-query-prefix-from-infix query)
                (mapconcat (lambda (q) (format "%s" q)) query " "))
            (error
             (let ((errmsg (error-message-string err)))
               (user-error errmsg)))))))))

(defun macher-instruct--set-directive-tag-query (directive query)
  "Set the tag query for DIRECTIVE to QUERY string."
  (condition-case err
      (let ((parsed-prefix-tag-query
             (macher-instruct--tag-query-prefix-from-infix (read (concat "(" query ")")))))
        (overlay-put directive 'macher-instruct-directive-prefix-tag-query parsed-prefix-tag-query)
        (if (string-empty-p query)
            (overlay-put directive 'macher-instruct-directive-infix-tag-query-string nil)
          (overlay-put directive
                       'macher-instruct-directive-infix-tag-query-string
                       (macher-instruct--apply-face-to-match "\\b\\(?:(*not\\|or\\|and\\)\\b\\|(\\|)"
                                                             (macher-instruct--apply-face-to-match
                                                              "\\(:?.+\\)"
                                                              query
                                                              'font-lock-constant-face)
                                                             nil))
          (overlay-put directive 'macher-instruct-directive-status nil))
        (macher-instruct--update-instruction-overlay directive t))
    (error
     (message (error-message-string err)))))

(defun macher-instruct--print-instruction-not-found (direction type)
  "Print a not found message for the given DIRECTION and TYPE."
  (let ((type-string (pcase type
                       ('directive "directive")
                       ('reference "reference")
                       (_ "instruction"))))
    (message "No %s %s found"
             (if (eq direction 'next) "next" "previous")
             type-string)))

(cl-defun macher-instruct--reference-matches-query-p (reference query)
  "Return t only if REFERENCE matches the tag QUERY."
  (unless reference
    (cl-return-from macher-instruct--reference-matches-query-p nil))
  (let ((atoms (cl-remove-duplicates (cl-remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) macher-instruct-empty-tag-query-matches-all)
        t
      (let ((tags (macher-instruct--reference-tags reference t))
            (direct-tags (macher-instruct--reference-tags reference nil))
            (instr-id (lambda (tag) (let ((tagname (symbol-name tag)))
                                      (when (string-match "^id:\\([1-9][0-9]*\\)$" tagname)
                                        (string-to-number (match-string 1 tagname)))))))
        (if (and (null direct-tags) macher-instruct-always-match-untagged-references)
            t
          (let ((atom-bindings (mapcar (lambda (atom)
                                         (pcase atom
                                           ('is:bufferlevel
                                            (macher-instruct--instruction-bufferlevel-p reference))
                                           ('is:subreference
                                            (macher-instruct--parent-instruction reference 'reference))
                                           ('is:tagless
                                            (null tags))
                                           ('is:directly-tagless
                                            (null (macher-instruct--reference-tags reference nil)))
                                           ('is:with-commentary
                                            (not (string-empty-p (macher-instruct--commentary-text reference))))
                                           (_ (if-let ((id (funcall instr-id atom)))
                                                  (= id (macher-instruct--instruction-id reference))
                                                (member atom tags)))))
                                       atoms)))
            (cl-progv atoms atom-bindings
              (eval query))))))))

(defun macher-instruct--filter-references (query)
  "Return a list of all references filtered by the tag QUERY.

See `macher-instruct--tag-query-prefix-from-infix' for QUERY format."
  (let ((atoms (cl-remove-duplicates (cl-remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) macher-instruct-empty-tag-query-matches-all)
        (macher-instruct--foreach-instruction instr when (macher-instruct--referencep instr) collect instr)
      (macher-instruct--foreach-instruction instr
        when (and (macher-instruct--referencep instr)
                  (macher-instruct--reference-matches-query-p instr query))
        collect instr))))

(defun macher-instruct--available-tags ()
  "Return a list of all the tags in the loaded references."
  (let ((tags-hash (make-hash-table)))
    (macher-instruct--foreach-instruction (ref)
      do (when (macher-instruct--referencep ref)
           (cl-loop for tag in (macher-instruct--reference-tags ref)
                    do (puthash tag t tags-hash))))
    (hash-table-keys tags-hash)))

(defun macher-instruct--cycle-instruction (type direction)
  "Get the next or previous instruction overlay of TYPE.
DIRECTION should be `next' or `previous' from the current point.

If no instruction found in the buffer, checks the next buffers in the
`macher-instruct--instructions' alist.

Returns the found instruction, if any."
  ;; We want the buffers to be a cyclic list, based on the current buffer.
  (let* ((buffers (let ((bufs (mapcar #'car macher-instruct--instructions)))
                    (if (eq direction 'next)
                        (macher-instruct--cycle-list-around (current-buffer) bufs)
                      (macher-instruct--cycle-list-around (current-buffer) (nreverse bufs)))))
         (original-buffer (current-buffer))
         (found-instr))
    (while (and buffers (null found-instr))
      (let* ((buffer (car buffers))
             (instrs (macher-instruct--foreach-instruction (instr buffer) collect instr)))
        (setq buffers (delq buffer buffers))
        (when type
          (setq instrs (cl-remove-if-not (lambda (instr)
                                           (eq (macher-instruct--instruction-type instr) type))
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
          (when-let ((instruction (car instrs)))
            (let ((buffer (overlay-buffer instruction)))
              (unless (eq buffer original-buffer)
                (switch-to-buffer buffer)))
            (goto-char (overlay-start instruction))
            (setq found-instr instruction)))))
    found-instr))

(defun macher-instruct--add-tags (reference tags)
  "Add TAGS to REFERENCE.

TAGS should be a list of symbols.
Returns the number of new tags added."
  (let* ((tag-type 'macher-instruct-reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl-remove-if (lambda (tag) (member tag existing-tags)) tags)))
    (overlay-put reference tag-type (cl-union existing-tags new-tags :test 'eq))
    (let ((added (length new-tags)))
      (when (> added 0)
        (macher-instruct--update-instruction-overlay reference t))
      added)))

(defun macher-instruct--remove-tags (reference tags)
  "Remove TAGS from REFERENCE.

TAGS should be a list of symbols.
Returns the number of tags removed."
  (let* ((tag-type 'macher-instruct-reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl-set-difference existing-tags tags :test 'eq)))
    (overlay-put reference tag-type new-tags)
    (let ((removed (- (length existing-tags) (length new-tags))))
      (when (> removed 0)
        (macher-instruct--update-instruction-overlay reference t))
      removed)))

(defun macher-instruct--inherited-tags (reference)
  "Return the list of all tags that REFERENCE inherits from its parents."
  (when-let ((parent (macher-instruct--parent-instruction reference 'reference)))
    (macher-instruct--reference-tags parent t)))

(defun macher-instruct--reference-tags (reference &optional include-parent-tags)
  "Return the list of tags for the given REFERENCE.

If INCLUDE-PARENT-TAGS is non-nil, gets te parent's tags as well."
  (if (not include-parent-tags)
      (overlay-get reference 'macher-instruct-reference-tags)
    (append (overlay-get reference 'macher-instruct-reference-tags)
            (when-let ((parent (macher-instruct--parent-instruction reference 'reference)))
              (macher-instruct--reference-tags parent t)))))

(defun macher-instruct--delete-instruction-at (point)
  "Delete the instruction at POINT.

Returns the deleted instruction overlay."
  (let* ((instructions (macher-instruct--instructions-at point))
         (target (macher-instruct--highest-priority-instruction instructions t)))
    (when target
      (macher-instruct--delete-instruction target))))

(defun macher-instruct--being-processed-p (instruction)
  "Return non-nil if the directive INSTRUCTION is being processed."
  (eq (overlay-get instruction 'macher-instruct-directive-status) 'processing))

(defun macher-instruct--directive-empty-p (directive)
  "Check if DIRECTIVE is empty.

A directive is empty if it does not have a body or secondary directives."
  (let ((subdirectives
         (cl-remove-if-not #'macher-instruct--directivep
                           (macher-instruct--wholly-contained-instructions (overlay-buffer directive)
                                                                           (overlay-start directive)
                                                                           (overlay-end directive)))))
    (not (cl-some (lambda (subdir)
                    (not (string-empty-p (macher-instruct--directive-text subdir))))
                  subdirectives))))

(defun macher-instruct--create-instruction (type)
  "Create or scale an instruction of the given TYPE within the region.

If a region is selected but partially covers an existing instruction,
then the function will resize it. See either
`macher-instruct-create-reference' or `macher-instruct-create-directive'
for details on how the resizing works."
  (if (use-region-p)
      (let ((intersecting-instructions
             (cl-remove-if (lambda (instr)
                             (xor (= (overlay-start instr) (region-beginning))
                                  (= (overlay-end instr) (region-end))))
                           (macher-instruct--partially-contained-instructions (current-buffer)
                                                                              (region-beginning)
                                                                              (region-end)))))
        (if-let ((instructions
                  (cl-remove-if-not (lambda (instr)
                                      (eq (macher-instruct--instruction-type instr) type))
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
                (macher-instruct--update-instruction-overlay instruction))
              (when instructions
                (deactivate-mark)))
          ;; Else - there are no partially contained instructions of the same
          ;; type within the region...
          (when (or intersecting-instructions
                    (or (cl-some (lambda (instr)
                                   (and (= (overlay-start instr) (region-beginning))
                                        (= (overlay-end instr) (region-end))))
                                 (macher-instruct--instructions-in (region-beginning) (region-end)))))
            ;; ...but there are intersecting instructions of another type, or
            ;; another instruction existing precisely at the start of another.
            (user-error "Instruction intersects with existing instruction"))
          (let* ((buffer (current-buffer))
                 (instruction (if (eq type 'reference)
                                  (macher-instruct--create-reference-in buffer
                                                                        (region-beginning)
                                                                        (region-end))
                                (save-window-excursion
                                  (let ((pos (region-beginning)))
                                    (unless (<= (window-start) pos (window-end))
                                      (set-window-start (selected-window)
                                                        (max (point-min)
                                                             (- (region-beginning)
                                                                (- (window-end) (window-start))))))
                                    (macher-instruct--create-directive-in buffer
                                                                          (region-beginning)
                                                                          (region-end)))))))
            (with-current-buffer buffer
              (deactivate-mark)
              (when (eq type 'reference)
                (macher-instruct-add-tags instruction)))
            instruction)))
    (when (eq type 'directive)
      (prog1 (macher-instruct--create-directive-in (current-buffer) (point) (point) t)
        (deactivate-mark)))))

;; DEPRECATED 2025-08-04:
(cl-defun macher-instruct--process-directive-llm-response (response directive status)
  "Process RESPONSE string for sent DIRECTIVE.

Removes any superfluous markup formatting and indents the response
according to the current buffer."
  (unless (overlay-buffer directive)
    ;; Directive is gone...
    (cl-return-from macher-instruct--process-directive-llm-response))
  (unless (eq (overlay-get directive 'macher-instruct-directive-status) 'processing)
    ;; The directive has been modified.  Do not continue.
    (cl-return-from macher-instruct--process-directive-llm-response))
  (cl-flet ((mark-failed (reason)
              (overlay-put directive 'macher-instruct-directive-status 'failed)
              (overlay-put directive 'macher-instruct-directive-fail-reason reason)))
    (cond
     ((null response)
      (mark-failed status))
     ((eq status 'aborted)
      (mark-failed "The request has been aborted."))
     (t
      (let* ((response-code-blocks (macher-instruct--markdown-code-blocks response))
             (parsed-response (car response-code-blocks)))
        (if (/= (length response-code-blocks) 1)
            (mark-failed response)
          (overlay-put directive 'macher-instruct-directive-status 'succeeded)
          (with-current-buffer (overlay-buffer directive)
            (let ((beg (overlay-start directive))
                  (end (overlay-end directive)))
              ;; Add current directive text to history.
              (let ((current-text (buffer-substring-no-properties beg end)))
                (let ((trimmed-text (if (string= " " current-text) " " (string-trim current-text))))
                  (unless (string-empty-p trimmed-text)
                    (push current-text (overlay-get directive 'macher-instruct-directive-history)))))
              ;; Delete any child directives of the top-level directive.
              (let ((child-directives (cl-remove-if-not #'macher-instruct--directivep
                                                        (macher-instruct--child-instructions directive))))
                (dolist (child-directive child-directives)
                  (macher-instruct--delete-instruction child-directive)))
              (save-excursion
                (goto-char beg)
                ;; Insert a dummy character so that the overlay won't be deleted
                ;; when we erase the entire region spanned by the overlay.
                (insert " ")
                (delete-region (1+ beg) (1+ end))
                (insert parsed-response)
                (let ((end (point)))
                  (setf (overlay-end directive) end)
                  (goto-char (1+ beg))
                  (backward-delete-char 1)
                  (unless (eq indent-line-function #'indent-relative)
                    (indent-region beg end)))
                (overlay-put directive 'evaporate t))))))))
    (macher-instruct--update-instruction-overlay directive t)))

;; DEPRECATED 2025-08-05:
(defun macher-instruct--directive-next-history (directive &optional backwards)
  "Cycle through the directive history.

DIRECTIVE is an instruction directive overlay. If BACKWARDS is non-nil,
traverse the history backward."
  (when-let ((history (overlay-get directive 'macher-instruct-directive-history)))
    (let ((current-text (buffer-substring-no-properties
                         (overlay-start directive)
                         (overlay-end directive))))
      (if backwards
          ;; Traverse backwards: Get the last element of the history.
          (let ((prev-text (car (last history))))
            ;; Replace current directive text with the previous directive.
            (macher-instruct--replace-text (overlay-start directive) (overlay-end directive) prev-text)
            ;; Append current text to the end of the history list.
            (setf (overlay-get directive 'macher-instruct-directive-history)
                  (append (butlast history) (list current-text))))
        ;; Traverse forward: Get the first element of the history.
        (save-excursion
          (let ((next-text (car history)))
            ;; Replace current directive text with the next directive.
            (macher-instruct--replace-text (overlay-start directive) (overlay-end directive) next-text)))
        ;; Move current text to the end of the history list.
        (setf (overlay-get directive 'macher-instruct-directive-history)
              (append (cdr history) (list current-text)))))))

(defun macher-instruct--referencep (instruction)
  "Return non-nil if INSTRUCTION is a reference."
  (eq (macher-instruct--instruction-type instruction) 'reference))

(defun macher-instruct--directivep (instruction)
  "Return non-nil if INSTRUCTION is a directive."
  (eq (macher-instruct--instruction-type instruction) 'directive))

(cl-defun macher-instruct--highest-priority-instruction (instructions &optional return-highlighted)
  "Return the instruction with the highest priority from the INSTRUCTIONS list.

Priority here refers to the priority property used by overlays.

If RETURN-HIGHLIGHTED is non-nil and
`macher-instruct--highlighted-instruction' is non-nil, the function will
return `macher-instruct--highlighted-instruction' if it is also in the
INSTRUCTIONS list."
  (when (and return-highlighted
             macher-instruct--highlighted-instruction
             (member macher-instruct--highlighted-instruction instructions))
    (cl-return-from macher-instruct--highest-priority-instruction macher-instruct--highlighted-instruction))
  (cl-reduce (lambda (acc instruction)
               (if (or (not acc)
                       (> (or (overlay-get instruction 'priority)
                              macher-instruct--default-instruction-priority)
                          (or (overlay-get acc 'priority)
                              macher-instruct--default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun macher-instruct--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay.

Instruction type can either be `reference' or `directive'."
  (if-let ((type (overlay-get instruction 'macher-instruct-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun macher-instruct--create-instruction-overlay-in (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (make-local-variable 'macher-instruct--after-change-functions-hooked)
  (with-current-buffer buffer
    (let ((is-bufferlevel
           ;; Check if the overlay spans the start and end of the buffer. If it
           ;; does, make it sticky so that additions to edges of the buffer will
           ;; cause it to expand there. This is useful for when we want to
           ;; append new text to the end of the buffer but don't want to
           ;; "invalidate" the buffer-level status of the instruction.
           (and (= start (point-min)) (= end (point-max)))))
      (let ((overlay (make-overlay start end (current-buffer) nil is-bufferlevel)))
        (overlay-put overlay 'macher-instruct-instruction t)
        (overlay-put overlay 'macher-instruct-id (macher-instruct--create-id))
        (push overlay (alist-get buffer macher-instruct--instructions))
        (unless (bound-and-true-p macher-instruct--after-change-functions-hooked)
          (setq-local macher-instruct--after-change-functions-hooked t)
          (add-hook 'after-change-functions
                    (lambda (beg end _len)
                      (let ((beg (max (point-min) (1- beg)))
                            (end (min (point-max) (1+ end))))
                        (let ((affected-instructions (macher-instruct--instructions-in beg end)))
                          (dolist (instruction affected-instructions)
                            (macher-instruct--update-instruction-overlay instruction)))))
                    nil t))
        (macher-instruct--setup-buffer-hooks buffer)
        overlay))))

(defun macher-instruct--instruction-p (overlay)
  "Return non-nil if OVERLAY is an instruction overlay."
  (overlay-get overlay 'macher-instruct-instruction))

(defun macher-instruct--parent-instruction (instruction &optional of-type)
  "Return the parent of the given INSTRUCTION overlay.

If OF-TYPE is non-nil, returns the parent with the given type."
  (with-current-buffer (overlay-buffer instruction)
    (let ((beg (overlay-start instruction))
          (end (overlay-end instruction)))
      (macher-instruct--highest-priority-instruction (cl-remove-if-not (lambda (instr)
                                                                         (and (not (eq instr instruction))
                                                                              (or (null of-type)
                                                                                  (eq (macher-instruct--instruction-type instr)
                                                                                      of-type))
                                                                              (<= (overlay-start instr) beg
                                                                                  end (overlay-end instr))))
                                                                       (macher-instruct--instructions-in beg end))))))

(defun macher-instruct--bodyless-instruction-p (instr)
  "Return non-nil if the INSTR instruction has a body."
  (= (overlay-start instr) (overlay-end instr)))

(defun macher-instruct--subinstruction-of-p (sub parent)
  "Return t is instruction SUB is contained entirely within instruction PARENT.

In this case, an instruction is _not_ considered a subinstruction of
itself."
  (and (eq (overlay-buffer sub)
           (overlay-buffer parent))
       (<= (overlay-start parent) (overlay-start sub) (overlay-end sub) (overlay-end parent))
       (and (/= (overlay-start parent) (overlay-start sub))
            (/= (overlay-end parent) (overlay-end sub)))))

(cl-defun macher-instruct--child-instructions (instruction)
  "Return the direct child instructions of the given INSTRUCTION overlay."
  ;; Bodyless instructions cannot have any children.
  (when (macher-instruct--bodyless-instruction-p instruction)
    (cl-return-from macher-instruct--child-instructions nil))
  (let ((children (cl-remove-if (lambda (instr)
                                  (or (eq instr instruction)
                                      (and (= (overlay-start instr) (overlay-start instruction))
                                           (= (overlay-end instr) (overlay-end instruction)))))
                                (macher-instruct--wholly-contained-instructions (overlay-buffer instruction)
                                                                                (overlay-start instruction)
                                                                                (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl-set-difference children
                                        (macher-instruct--child-instructions child))))
    children))

(defun macher-instruct--create-reference-in (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (let ((ov (macher-instruct--create-instruction-overlay-in buffer start end)))
    (overlay-put ov 'macher-instruct-instruction-type 'reference)
    (overlay-put ov 'evaporate t)
    (macher-instruct--update-instruction-overlay ov t)
    ov))

(defun macher-instruct--create-directive-in (buffer start end &optional bodyless directive-text)
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution. BODYLESS
controls special formatting if non-nil.

DIRECTIVE-TEXT is used as the default directive. Having DIRECTIVE-TEXT
be non-nil prevents the opening of a prompt buffer."
  (let ((ov (macher-instruct--create-instruction-overlay-in buffer start end)))
    (unless bodyless
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'macher-instruct-instruction-type 'directive)
    (overlay-put ov 'macher-instruct-directive (or directive-text ""))
    (macher-instruct--update-instruction-overlay ov (not bodyless))
    (unless directive-text
      (deactivate-mark)
      (macher-instruct--read-directive ov))
    ov))

(defun macher-instruct--delete-instruction (instruction &optional buffer)
  "Delete the INSTRUCTION overlay and return it.

If the overlay is already dead, just perform the cleanup.
BUFFER is required in order to perform cleanup on a dead instruction."
  ;; We want to handle this function in two different ways. The first way
  ;; handles regular deletion, i.e. when the function was invoked on an existing
  ;; instruction. The second way is for when the instruction was deleted
  ;; uncanonically through text manipulation. In the latter case, the function
  ;; will be called during a cleanup routine and the instruction will not be
  ;; alive.
  (when (overlay-get instruction 'macher-instruct-marked-for-deletion)
    (error "Instruction %s already marked for deletion" instruction))
  (overlay-put instruction 'macher-instruct-marked-for-deletion t)
  (cl-labels ((cleanup (instr buffer)
                (let ((id (macher-instruct--instruction-id instr)))
                  (macher-instruct--retire-id id)
                  (macher-instruct-unlink-instructions `(,id) (macher-instruct--instruction-outlinks instr))
                  (macher-instruct-unlink-instructions (macher-instruct--instruction-inlinks instr) `(,id)))
                (setf (cdr (assoc buffer macher-instruct--instructions))
                      (delq instr (cdr (assoc buffer macher-instruct--instructions))))))
    (let ((ov-buffer (overlay-buffer instruction)))
      (when (buffer-live-p ov-buffer)
        (let ((children (macher-instruct--child-instructions instruction)))
          (delete-overlay instruction)
          (dolist (child children)
            (macher-instruct--update-instruction-overlay child t))))
      (cleanup instruction (or ov-buffer
                               buffer
                               (error "Cannot perform cleanup without a buffer")))))
  instruction)

(defun macher-instruct--instructions-congruent-p (a b)
  "Return t only if instruction overlays A and B are congruent."
  (and (eq (overlay-buffer a) (overlay-buffer b))
       (= (overlay-start a) (overlay-start b))
       (= (overlay-end a) (overlay-end b))))

(defun macher-instruct--instruction-bufferlevel-p (instruction)
  "Return t if INSTRUCTION contains the entirety of its buffer."
  (let ((buffer (overlay-buffer instruction)))
    (when buffer
      (with-current-buffer buffer
        (without-restriction
          (and (= (overlay-start instruction) (point-min))
               (= (overlay-end instruction) (point-max))))))))

;; Overlay actions adapted from `gptel-rewrite'

(defvar-keymap macher-instruct-reference-actions-map
  :doc "Keymap for `macher-instruct' reference overlay actions at point."
  "RET" #'macher-instruct--ov-actions-dispatch
  "C-c C-m" #'macher-instruct--ov-actions-modify
  "C-c C-c" #'macher-instruct--ov-actions-commentary
  "C-c C-k" #'macher-instruct--ov-actions-clear
  "C-c C-l" #'macher-instruct--ov-actions-link
  "C-c C-u" #'macher-instruct--ov-actions-unlink
  "C-c C-t" #'macher-instruct--ov-actions-add-tags
  "C-c C-r" #'macher-instruct--ov-actions-remove-tags)

(defvar-keymap macher-instruct-directive-actions-map
  :doc "Keymap for `macher-instruct' directive overlay actions at point."
  "RET" #'macher-instruct--ov-actions-dispatch
  "C-c C-d" #'macher-instruct--ov-actions-discuss
  "C-c C-c" #'macher-instruct--ov-actions-implement
  "C-c C-r" #'macher-instruct--ov-actions-revise
  "C-c C-m" #'macher-instruct--ov-actions-modify
  "C-c C-t" #'macher-instruct--ov-actions-tags
  "C-c C-k" #'macher-instruct--ov-actions-clear)

(defvar-keymap macher-instruct-directive-processing-actions-map
  :doc "Keymap for `macher-instruct' processing directive overlay actions at point."
  "RET" #'macher-instruct--ov-actions-dispatch
  "C-c C-a" #'macher-instruct--ov-actions-abort
  "C-c C-k" #'macher-instruct--ov-actions-clear)

(defvar-keymap macher-instruct-directive-succeeded-actions-map
  :doc "Keymap for `macher-instruct' succeeded directive overlay actions at point."
  "RET" #'macher-instruct--ov-actions-dispatch
  "C-c C-a" #'macher-instruct--ov-actions-accept
  "C-c C-w" #'macher-instruct--ov-actions-show-answer
  "C-c C-k" #'macher-instruct--ov-actions-clear
  "C-c C-u" #'macher-instruct--ov-actions-undo
  "C-c C-r" #'macher-instruct--ov-actions-revise
  "C-c C-m" #'macher-instruct--ov-actions-modify
  "C-c C-v" #'macher-instruct--ov-actions-view)

(defvar-keymap macher-instruct-directive-failed-actions-map
  :doc "Keymap for `macher-instruct' failed directive overlay actions at point."
  "RET" #'macher-instruct--ov-actions-dispatch
  "C-c C-c" #'macher-instruct--ov-actions-implement
  "C-c C-r" #'macher-instruct--ov-actions-revise
  "C-c C-m" #'macher-instruct--ov-actions-modify
  "C-c C-k" #'macher-instruct--ov-actions-clear)

(defun macher-instruct--ov-actions-dispatch (&optional instruction ci)
  "Dispatch actions for a successful instruction overlay.

INSTRUCTION is the overlay to dispatch actions for, CI is true for
interactive calls."
  (interactive (list (or (macher-instruct--highest-priority-instruction
                          (macher-instruct--instructions-at (point) 'directive)
                          t)
                         (macher-instruct--highest-priority-instruction
                          (macher-instruct--instructions-at (point) 'reference)
                          t))
                     t))
  (let ((choice)
        (instruction-type (macher-instruct--instruction-type instruction))
        (before-string (overlay-get instruction 'before-string)))
    (unwind-protect
        (pcase-let ((choices
                     (pcase instruction-type
                       (`reference '((?m "modify") (?c "commentary") (?l "link") (?u "unlink") (?t "add-tags") (?r "remove-tags") (?k "clear")))
                       (`directive
                        (pcase (overlay-get instruction 'macher-instruct-directive-status)
                          ('processing '((?a "abort") (?k "clear")))
                          ('succeeded '((?v "view") (?a "accept") (?r "revise") (?m "modify") (?w "show-answer") (?u "undo") (?k "clear")))
                          ('failed '((?i "implement") (?r "revise") (?m "modify") (?k "clear")))
                          (_ '((?d "discuss") (?i "implement") (?r "revise") (?m "modify") (?t "tags") (?k "clear")))))))
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
    (if ci
        (call-interactively (intern (concat "macher-instruct--ov-actions-" (cadr choice))))
      (funcall (intern (concat "macher-instruct--ov-actions-" (cadr choice))) instruction))))

(defalias #'macher-instruct--ov-actions-modify #'macher-instruct-modify-directive)
(defalias #'macher-instruct--ov-actions-commentary #'macher-instruct-modify-reference-commentary)
(defalias #'macher-instruct--ov-actions-link #'macher-instruct-link-instructions)
(defalias #'macher-instruct--ov-actions-unlink #'macher-instruct-unlink-instructions)
(defalias #'macher-instruct--ov-actions-add-tags #'macher-instruct-add-tags)
(defalias #'macher-instruct--ov-actions-remove-tags #'macher-instruct-remove-tags)
(defalias #'macher-instruct--ov-actions-discuss #'macher-discuss-directive)
(defalias #'macher-instruct--ov-actions-implement #'macher-implement-directive)
(defalias #'macher-instruct--ov-actions-revise #'macher-revise-directive)
(defalias #'macher-instruct--ov-actions-tags #'macher-instruct-modify-directive-tag-query)
(defalias #'macher-instruct--ov-actions-abort #'macher-abort)

(defun macher-instruct--ov-actions-view ()
  "Display the patch buffer in the macher workspace."
  (interactive)
  (display-buffer
   (macher-patch-buffer (macher-workspace) t)))

(defun macher-instruct--ov-actions-accept ()
  "Accept patch for the highest priority directive at point."
  (interactive)
  (save-excursion
    (with-current-buffer (macher-patch-buffer (macher-workspace) t)
      (diff-apply-buffer-with-overlay-adjustment))))
(defun macher-instruct--ov-actions-show-answer ()
  "Show answer by navigating to the response prefix in action buffer."
  (interactive)
  (with-current-buffer (macher-action-buffer)
    (display-buffer (macher-action-buffer))
    (goto-char (point-max))
    (goto-char (line-beginning-position))
    (when (re-search-backward
           (regexp-quote (gptel-response-prefix-string)) nil t)
      (goto-char (line-beginning-position)))))

(defun macher-instruct--ov-actions-clear ()
  "Clear instructions.
Deletes all instructions at point and removes the eldoc hook that
provides help for instruction actions if not other instructions are
active in the buffer."
  (interactive)
  (macher-instruct-delete-instructions)
  (with-current-buffer (current-buffer)
    (unless (alist-get (current-buffer) macher-instruct--instructions)
      (remove-hook 'eldoc-documentation-functions 'macher-instruct--ov-actions-help 'local))))

(defun macher-instruct--ov-actions-help (callback)
  "Eldoc documentation function for `macher-instruct' instruction actions.

CALLBACK is supplied by Eldoc, see `eldoc-documentation-functions'."
  (when-let* ((instruction-type (get-char-property (point) 'macher-instruct-instruction-type)))
    (funcall callback
             (format
              (pcase instruction-type
                (`reference (substitute-command-keys
                             "%s Options: \
modify \\[macher-instruct--ov-actions-modify], \
commentary \\[macher-instruct--ov-actions-commentary], \
link \\[macher-instruct--ov-actions-link], \
unlink \\[macher-instruct--ov-actions-unlink], \
add tags \\[macher-instruct--ov-actions-add-tags], \
remove tags \\[macher-instruct--ov-actions-remove-tags] or clear \\[macher-instruct--ov-actions-clear]"))
                (`directive
                 (pcase (get-char-property (point) 'macher-instruct-directive-status)
                   ('processing
                    (substitute-command-keys "%s Options: abort \\[macher-instruct--ov-actions-abort] or clear \\[macher-instruct--ov-actions-clear]"))
                   ('succeeded
                    (substitute-command-keys
                     "%s Options: \
view \\[macher-instruct--ov-actions-view], \
accept \\[macher-instruct--ov-actions-accept], \
revise \\[macher-instruct--ov-actions-revise], \
modify \\[macher-instruct--ov-actions-modify], \
show answer \\[macher-instruct--ov-actions-show-answer], \
undo \\[macher-instruct--ov-actions-undo] or clear \\[macher-instruct--ov-actions-clear]"))
                   ('failed
                    (substitute-command-keys
                     "%s Options: \
implement \\[macher-instruct--ov-actions-implement], \
revise \\[macher-instruct--ov-actions-revise], \
modify \\[macher-instruct--ov-actions-modify] or clear \\[macher-instruct--ov-actions-clear]"))
                   (_
                    (substitute-command-keys
                     "%s Options: \
discuss \\[macher-instruct--ov-actions-discuss], \
implement \\[macher-instruct--ov-actions-implement], \
revise \\[macher-instruct--ov-actions-revise], \
modify \\[macher-instruct--ov-actions-modify], \
tags \\[macher-instruct--ov-actions-tags] or clear \\[macher-instruct--ov-actions-clear]")))))
              (propertize (gptel--model-name gptel-model) 'face 'mode-line-emphasis)))))

(defun macher-instruct--update-instruction-overlay (instruction &optional update-children)
  "Update the appearance of the INSTRUCTION overlay.

This function updates the overlay label text, color of the label text,
and the background of the instruction overlay. This function should be
called every time there is a hierarchy or status change in the
instruction overlay that we wish to reflect.

Also updates the child instructions of the INSTRUCTION, if
UPDATE-CHILDREN is non-nil."
  (cl-labels
      ((directive-color (directive)
         (cl-labels ((dircol ()
                       (pcase (overlay-get directive 'macher-instruct-directive-status)
                         ('processing macher-instruct-directive-processing-color)
                         ('succeeded  macher-instruct-directive-success-color)
                         ('failed     macher-instruct-directive-fail-color)
                         (_           macher-instruct-directive-color))))
           (if-let ((parent-directive (macher-instruct--topmost-instruction directive 'directive)))
               (let ((parent-status (overlay-get parent-directive 'macher-instruct-directive-status)))
                 (if (eq parent-status 'processing)
                     macher-instruct-directive-processing-color
                   (if (eq parent-status 'failed)
                       macher-instruct-directive-fail-color
                     (dircol))))
             (dircol))))
       (aux (instruction &optional update-children priority (parent nil))
         (let* ((instruction-type (macher-instruct--instruction-type instruction))
                (padding (with-current-buffer (overlay-buffer instruction)
                           (save-excursion
                             (goto-char (overlay-start instruction))
                             (make-string (current-column) ? ))))
                (is-bufferlevel (macher-instruct--instruction-bufferlevel-p instruction))
                (parent-bufferlevel (and parent (macher-instruct--instruction-bufferlevel-p parent)))
                ;; This is to prevent the buffer-level instruction from having a
                ;; background color.
                (priority (if is-bufferlevel (1- priority) priority))
                (label "")
                color)
           (cl-labels
               ((action-setup ()
                  (add-hook 'eldoc-documentation-functions #'macher-instruct--ov-actions-help nil 'local)
                  (overlay-put instruction 'keymap (pcase instruction-type
                                                     (`reference macher-instruct-reference-actions-map)
                                                     (`directive
                                                      (pcase (overlay-get instruction 'macher-instruct-directive-status)
                                                        ('processing macher-instruct-directive-processing-actions-map)
                                                        ('succeeded macher-instruct-directive-succeeded-actions-map)
                                                        ('failed macher-instruct-directive-failed-actions-map)
                                                        (_ macher-instruct-directive-actions-map)))))
                  (overlay-put
                   instruction 'help-echo
                   (format (concat "%s \\[macher-instruct--ov-actions-dispatch] for options")
                           (pcase instruction-type
                             (`reference "Press")
                             (`directive
                              (pcase (overlay-get instruction 'macher-instruct-directive-status)
                                ('processing "Request in progress, press")
                                ('succeeded "Patch ready, press")
                                ('failed "Request failed, press")
                                (_ "Press")))))))

                (append-to-label (content &optional prefix)
                  (setq label
                        (concat label
                                (if (string-empty-p label) "" (concat "\n" padding))
                                (macher-instruct--fill-label-string content
                                                                    (or prefix "")
                                                                    padding
                                                                    (overlay-buffer instruction)))))
                (stylized-id-str (id)
                  (propertize (format "#%d" id) 'face 'font-lock-constant-face))
                (append-links-to-label ()
                  (cl-labels ((filter-ids (ids)
                                (cl-loop for id in ids
                                         unless
                                         (let ((instr (macher-instruct--instruction-with-id id)))
                                           (or (null instr)
                                               (not (eq (macher-instruct--instruction-type instr)
                                                        instruction-type))))
                                         collect id)))
                    (let ((outlinks (filter-ids (macher-instruct--instruction-outlinks instruction)))
                          (inlinks (filter-ids (macher-instruct--instruction-inlinks instruction))))
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
                (setq color macher-instruct-reference-color)
                (if (and parent
                         (and (eq (macher-instruct--instruction-type parent) 'reference)
                              (not parent-bufferlevel)))
                    (append-to-label (format "SUBREFERENCE %s"
                                             (stylized-id-str (macher-instruct--instruction-id instruction))))
                  (if is-bufferlevel
                      (append-to-label (format "BUFFER REFERENCE %s"
                                               (stylized-id-str (macher-instruct--instruction-id instruction))))
                    (append-to-label (format "REFERENCE %s"
                                             (stylized-id-str (macher-instruct--instruction-id instruction))))))
                (let* ((direct-tags (macher-instruct--reference-tags instruction))
                       (inherited-tags (macher-instruct--inherited-tags instruction))
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
                (let ((commentary (string-trim (or (macher-instruct--commentary-text instruction)
                                                   ""))))
                  (unless (string-empty-p commentary)
                    (append-to-label commentary "COMMENTARY: ")))
                (action-setup))
               ('directive ; DIRECTIVE
                (pcase (overlay-get instruction 'macher-instruct-directive-status)
                  ('processing (append-to-label "PROCESSING"))
                  ('succeeded (append-to-label "SUCCEEDED"))
                  ('failed (append-to-label (overlay-get instruction
                                                         'macher-instruct-directive-fail-reason)
                                            "FAILED: ")))
                (setq color (directive-color instruction))
                (let (sublabel
                      (directive-typename "DIRECTIVE"))
                  (if (and parent
                           (macher-instruct--directivep parent))
                      (progn
                        (pcase (overlay-get parent 'macher-instruct-directive-status)
                          ((or 'processing 'failed)
                           (if-let ((existing-typename (overlay-get instruction
                                                                    'macher-instruct-subdirective-typename)))
                               (setq directive-typename existing-typename)
                             (setq directive-typename "HINT")))
                          ('succeeded (setq directive-typename "CORRECTION"))
                          (_ (setq directive-typename "HINT")))
                        (setf (overlay-get instruction 'macher-instruct-subdirective-typename)
                              directive-typename))
                    (setf (overlay-get instruction 'macher-instruct-subdirective-typename) nil))
                  (setq sublabel (concat
                                  sublabel
                                  (format "%s %s"
                                          directive-typename
                                          (stylized-id-str (macher-instruct--instruction-id instruction)))))
                  (let ((directive (string-trim (or (overlay-get instruction 'macher-instruct-directive)
                                                    ""))))
                    (if (string-empty-p directive)
                        (setq sublabel (concat "EMPTY " sublabel))
                      (setq sublabel (concat sublabel ": ")))
                    (setq label (concat
                                 label
                                 (unless (string-empty-p label)
                                   (concat "\n" padding))
                                 (macher-instruct--fill-label-string directive
                                                                     sublabel
                                                                     padding
                                                                     (overlay-buffer instruction))))
                    (unless (macher-instruct--parent-instruction instruction 'directive)
                      (if-let ((query-string (overlay-get instruction
                                                          'macher-instruct-directive-infix-tag-query-string)))
                          (append-to-label query-string "TAG QUERY: ")
                        (let (matchinfo)
                          (if macher-instruct-empty-tag-query-matches-all
                              (setq matchinfo "REFERENCES ALL")
                            (if macher-instruct-always-match-untagged-references
                                (setq matchinfo "REFERENCES UNTAGGED ONLY")
                              (setq matchinfo "REFERENCES NOTHING")))
                          (setq label (concat label "\n" padding matchinfo))))
                      (append-links-to-label))))
                (action-setup)))
             (let* ((default-fg (face-foreground 'default))
                    (default-bg (face-background 'default))
                    (bg-tint-intensity
                     (if (and parent (not parent-bufferlevel))
                         (* macher-instruct-subinstruction-tint-coefficient macher-instruct-instruction-bg-tint-intensity)
                       macher-instruct-instruction-bg-tint-intensity))
                    (label-color (if is-bufferlevel
                                     (macher-instruct--tint default-fg color macher-instruct-instruction-label-tint-intensity)
                                   (let ((tint (macher-instruct--tint default-fg
                                                                      color
                                                                      macher-instruct-instruction-label-tint-intensity)))
                                     (dotimes (_  (- priority
                                                     macher-instruct--default-instruction-priority))
                                       (setq tint (macher-instruct--tint tint
                                                                         color
                                                                         macher-instruct-instruction-label-tint-intensity)))
                                     tint)))
                    ;; We want to make sure that the buffer-level instructions don't superfluously
                    ;; tint the background.
                    (bg-color (if (and is-bufferlevel (eq instruction-type 'reference))
                                  default-bg
                                (let ((tint (macher-instruct--tint default-bg
                                                                   color
                                                                   macher-instruct-instruction-bg-tint-intensity)))
                                  (dotimes (_ (- priority
                                                 macher-instruct--default-instruction-priority))
                                    (setq tint (macher-instruct--tint tint color bg-tint-intensity)))
                                  tint))))
               (overlay-put instruction 'macher-instruct-bg-color bg-color)
               (overlay-put instruction 'macher-instruct-label-color label-color)
               (overlay-put instruction 'priority priority)
               (when (eq instruction
                         macher-instruct--highlighted-instruction)
                 (setq bg-color
                       (macher-instruct--tint default-bg
                                              macher-instruct-highlighted-instruction-color
                                              macher-instruct-highlighted-instruction-tint-intensity)))
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
                        (when-let ((parent (macher-instruct--parent-instruction instruction)))
                          (colorize-region beg end
                                           (overlay-get parent 'macher-instruct-label-color)
                                           (overlay-get parent 'macher-instruct-bg-color)))))
                   (let ((before-string
                          (with-temp-buffer
                            (insert label)
                            (if (macher-instruct--bodyless-instruction-p instruction)
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
                            (unless (macher-instruct--bodyless-instruction-p instruction)
                              (let ((mark (point)))
                                (insert padding)
                                (colorize-region-as-parent mark (point))))
                            (buffer-string))))
                     (overlay-put instruction 'before-string before-string))))
               (overlay-put instruction 'face `(:extend t :background ,bg-color)))
             (when update-children
               (dolist (child (macher-instruct--child-instructions instruction))
                 (aux child update-children (1+ priority) instruction)))))))
    (let ((instructions-conflicting (cl-some (lambda (instr)
                                               (and (not (eq instr instruction))
                                                    (macher-instruct--instructions-congruent-p instruction
                                                                                               instr)))
                                             (macher-instruct--instructions-at (overlay-start instruction)))))
      (if instructions-conflicting
          ;; This instruction is causing conflicts, and therefore must be
          ;; deleted.
          (macher-instruct--delete-instruction instruction)
        (let ((parent (macher-instruct--parent-instruction instruction)))
          (let ((priority (if parent
                              (1+ (overlay-get parent 'priority))
                            macher-instruct--default-instruction-priority)))
            (aux instruction update-children priority parent)))))
    (pulse-momentary-highlight-region (overlay-start instruction) (overlay-end instruction))))

(defun macher-instruct--buffer-has-instructions-p (buffer)
  "Return non-nil if BUFFER has any macher instructions associated with it."
  (assoc buffer macher-instruct--instructions))

(defun macher-instruct--wholly-contained-instructions (buffer start end)
  "Return macher overlays in BUFFER that are entirely within START and END."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'macher-instruct-instruction)
                             (>= (overlay-start ov) start)
                             (<= (overlay-end ov) end)))
                      (overlays-in start end))))

(defun macher-instruct--instructions-at (point &optional type)
  "Return a list of instructions at current POINT.

Optionally return only instructions of specific TYPE. Also returns
bodyless overlays located right before the point."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'macher-instruct-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'macher-instruct-instruction-type)
                                        type))
                               (null type))))
                    (overlays-in point
                                 (min (point-max) (1+ point)))))

(defun macher-instruct--instructions-in (start end &optional type)
  "Return a list of instructions in region delimited by START and END.

Optionally return only instructions of specific TYPE."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'macher-instruct-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'macher-instruct-instruction-type)
                                        type))
                               (null type))))
                    (overlays-in start end)))

(defun macher-instruct--partially-contained-instructions (buffer start end)
  "Return instructions in BUFFER that overlap with START and END.

Does not return instructions that contain the region in its entirety."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'macher-instruct-instruction)
                             (or (<= (overlay-start ov) start)
                                 (>= (overlay-end ov) end))
                             (not (and (<= (overlay-start ov) start)
                                       (>= (overlay-end ov) end)))))
                      (overlays-in start end))))

(defun macher-instruct--instructions ()
  "Return a list of all currently loaded instructions."
  (macher-instruct--foreach-instruction inst collect inst))

(cl-defun macher-instruct--topmost-instruction (instruction &optional of-type pred)
  "Return the topmost instruction containing the INSTRUCTION, if any.

If OF-TYPE is non-nil, filter by the specified instruction OF-TYPE. If
OF-TYPE is nil, the instruction returned is the top-level one.

If PRED is non-nil, then the best instruction must also satisfy it. The
PRED must be a function which accepts an instruction."
  (unless instruction
    (cl-return-from macher-instruct--topmost-instruction nil))
  (with-current-buffer (overlay-buffer instruction)
    (let ((best-instruction instruction))
      (cl-labels ((parent-instr (instr)
                    (if-let ((parent (macher-instruct--parent-instruction instr)))
                        (progn
                          (when (and (or (null of-type) (eq of-type (macher-instruct--instruction-type parent)))
                                     (or (null pred) (funcall pred parent)))
                            (setq best-instruction parent))
                          (parent-instr parent))
                      best-instruction)))
        (setq best-instruction (parent-instr instruction)))
      (if (and (or (null of-type) (eq of-type (macher-instruct--instruction-type best-instruction)))
               (or (null pred) (funcall pred best-instruction)))
          best-instruction
        nil))))

(defun macher-instruct--toplevel-instructions (&optional of-type)
  "Return the global top-level instructions across all buffers.

Returns only instructions of specific type if OF-TYPE is non-nil. If
OF-TYPE is non-nil, this function does _not_ return the \"next best\"
instruction of the matching type; i.e., the returned list consists only
of toplevel instructions that also match the specified type."
  (macher-instruct--foreach-instruction instr
    with toplevels = (make-hash-table)
    with inferiors = (make-hash-table)
    unless (or (gethash instr toplevels) (gethash instr inferiors))
    do (with-current-buffer (overlay-buffer instr)
         (let* ((instrs (macher-instruct--instructions-at (overlay-start instr)))
                (topmost (car (cl-remove-if #'macher-instruct--parent-instruction instrs)))
                (children (delq topmost instrs)))
           (puthash topmost t toplevels)
           (cl-loop for child in children do (puthash child t inferiors))))
    finally (cl-return (if of-type
                           (cl-remove-if-not (lambda (instr)
                                               (eq (macher-instruct--instruction-type instr)
                                                   of-type))
                                             (hash-table-keys toplevels))
                         (hash-table-keys toplevels)))))

(defun macher-instruct--directive-text (directive)
  "Return the directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (or (overlay-get directive 'macher-instruct-directive) ""))

(defun macher-instruct--commentary-text (reference)
  "Return the commentary text of the REFERENCE overlay.

Returns an empty string if there is no commentary."
  (or (overlay-get reference 'macher-instruct-commentary) ""))

(defun macher-instruct--read-directive (directive)
  "Prompt user to enter a directive text via minibuffer for DIRECTIVE."
  (let ((original-directive-text (macher-instruct--directive-text directive))
        (original-directive-status (overlay-get directive 'macher-instruct-directive-status)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((directive-text (minibuffer-contents)))
                        (overlay-put directive 'macher-instruct-directive directive-text)
                        (macher-instruct--update-instruction-overlay directive)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put directive 'macher-instruct-directive (minibuffer-contents))
                      (overlay-put directive 'macher-instruct-directive-status nil)
                      (macher-instruct--update-instruction-overlay directive))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Directive: " original-directive-text)
        (quit
         (if (string-empty-p original-directive-text)
             (macher-instruct--delete-instruction directive)
           (overlay-put directive 'macher-instruct-directive original-directive-text)
           (overlay-put directive 'macher-instruct-directive-status original-directive-status)
           (macher-instruct--update-instruction-overlay directive nil))
         (signal 'quit nil))))))

(defun macher-instruct--read-commentary (reference)
  "Prompt user to enter a commentary text via minibuffer for REFERENCE."
  (let ((original-commentary-text (macher-instruct--commentary-text reference)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((commentary-text (minibuffer-contents)))
                        (overlay-put reference 'macher-instruct-commentary commentary-text)
                        (macher-instruct--update-instruction-overlay reference)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put reference 'macher-instruct-commentary (minibuffer-contents))
                      (macher-instruct--update-instruction-overlay reference))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Commentary: " original-commentary-text)
        (quit
         (overlay-put reference 'macher-instruct-commentary original-commentary-text)
         (macher-instruct--update-instruction-overlay reference nil))
        (signal 'quit nil)))))

(defun macher-instruct--toplevel-references ()
  "Fetch all toplevel reference instructions.

A toplevel reference instruction is one that has no parents."
  (seq-filter (lambda (instr)
                (and (null (macher-instruct--parent-instruction instr))
                     (macher-instruct--referencep instr)))
              (macher-instruct--instructions)))

(cl-defun macher-instruct--ancestral-instructions (instruction &optional of-type)
  "Return a list of ancestors for the current INSTRUCTION."
  (if-let ((parent (macher-instruct--parent-instruction instruction)))
      (if (or (null of-type)
              (eq (macher-instruct--instruction-type parent) of-type))
          (cons parent (macher-instruct--ancestral-instructions parent of-type))
        (macher-instruct--ancestral-instructions parent of-type))
    nil))

(defun macher-instruct--context (&optional query directive)
  "Get context plist.

Returns plist with :summary and :references keys, optionally for
specified DIRECTIVE and tag QUERY."
  (let* ((pred
          (lambda (instr)
            (macher-instruct--reference-matches-query-p instr
                                                        (or query
                                                            (when directive
                                                              (overlay-get directive
                                                                           'macher-instruct-directive-prefix-tag-query))))))
         (used-commentary-refs (make-hash-table))
         (toplevel-refs (macher-instruct--foreach-instruction instr
                          when (and (macher-instruct--referencep instr)
                                    (eq (macher-instruct--topmost-instruction instr 'reference pred)
                                        instr))
                          collect instr))
         (linked-refs (let ((visited-refs (make-hash-table))
                            (independent-refs ())
                            (child-refmap
                             (let ((ht (make-hash-table)))
                               (cl-loop for tlr in toplevel-refs
                                        do (cl-loop for instr in (macher-instruct--wholly-contained-instructions
                                                                  (overlay-buffer tlr)
                                                                  (overlay-start tlr)
                                                                  (overlay-end tlr))
                                                    when (and (not (eq instr tlr))
                                                              (macher-instruct--referencep instr))
                                                    do (puthash instr t ht)))
                               ht)))
                        (cl-labels ((collect-linked-references-recursively (ref)
                                      (puthash ref t visited-refs)
                                      (dolist (linked-id (macher-instruct--instruction-outlinks ref))
                                        (let ((linked-ref (macher-instruct--instruction-with-id linked-id)))
                                          (when (and linked-ref
                                                     (macher-instruct--referencep linked-ref)
                                                     (not (gethash linked-ref visited-refs)))
                                            (unless (gethash linked-ref child-refmap)
                                              (push linked-ref independent-refs))
                                            (collect-linked-references-recursively linked-ref))))))
                          (mapc #'collect-linked-references-recursively
                                (cl-remove-duplicates
                                 (append (when directive
                                           (macher-instruct--ancestral-instructions directive 'reference))
                                         toplevel-refs
                                         (flatten-tree
                                          (mapcar (lambda (instr)
                                                    (macher-instruct--ancestral-instructions instr 'reference))
                                                  toplevel-refs)))))
                          independent-refs)))
         (total-refs (cl-remove-if (lambda (ref)
                                     (and directive (macher-instruct--subinstruction-of-p ref directive)))
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
                        (macher-instruct--overlay-region-info ref)
                      (let ((markdown-delimiter
                             (macher-instruct--delimiting-markdown-backticks ref-string)))
                        (insert
                         (concat
                          "\n\n"
                          (format "#### Reference #%d" (macher-instruct--instruction-id ref))
                          "\n\n"
                          (format "%s"
                                  (if (macher-instruct--instruction-bufferlevel-p ref)
                                      (format "File `%s`"
                                              (file-relative-name
                                               (buffer-file-name buffer)
                                               (macher--workspace-root (macher-workspace))))
                                    (format "In file `%s`, %s:"
                                            (file-relative-name
                                             (buffer-file-name buffer)
                                             (macher--workspace-root (macher-workspace)))
                                            ref-info-string)))
                          (unless (macher-instruct--instruction-bufferlevel-p ref)
                            (format "\n\n%s\n%s\n%s"
                                    markdown-delimiter
                                    ref-string
                                    markdown-delimiter))

                          (let ((commentary (macher-instruct--commentary-text ref)))
                            (unless (string-empty-p commentary)
                              (puthash ref t used-commentary-refs)
                              (format "\n\nCommentary:\n\n%s"
                                      (macher-instruct--markdown-enquote commentary))))))))))
      (list :summary (if reference-alist (buffer-string) "")
            :references reference-alist))))

(defun macher-instruct--directive-llm-prompt (directive)
  "Craft the prompt for the LLM model associated with the DIRECTIVE."
  (when (macher-instruct--directive-empty-p directive)
    (error "Directive %s is empty" directive))
  (let* ((context (macher-instruct--context nil directive))
         (reference-count (length (flatten-tree (mapcar #'cdr (plist-get context :references)))))
         (directive-toplevel-reference (macher-instruct--topmost-instruction directive 'reference))
         (directive-buffer (overlay-buffer directive))
         (directive-filename (buffer-file-name directive-buffer))
         (directive-workspace (macher-workspace))
         (directive-filename-relpath
          (when directive-filename
            (file-relative-name directive-filename (macher--workspace-root directive-workspace)))))
    (cl-destructuring-bind (directive-region-info-string directive-region-string)
        (macher-instruct--overlay-region-info directive)
      (let ((expanded-directive-text
             (let ((secondary-directives
                    (cl-remove-if-not (lambda (inst)
                                        (and (eq (macher-instruct--instruction-type inst) 'directive)
                                             (not (eq inst directive))))
                                      (macher-instruct--wholly-contained-instructions
                                       (overlay-buffer directive)
                                       (overlay-start directive)
                                       (overlay-end directive))))
                   (sd-typename (if (not (eq (overlay-get directive 'macher-instruct-directive-status)
                                             'succeeded))
                                    "hint"
                                  "correction")))
               (concat
                (if (macher-instruct--instruction-bufferlevel-p directive)
                    ""
                  (concat
                   (format ", %s" directive-region-info-string)
                   (if (string-empty-p directive-region-string)
                       "."
                     (let ((markdown-delimiter
                            (macher-instruct--delimiting-markdown-backticks directive-region-string)))
                       (concat
                        (format ", which correspond%s to:"
                                (if (macher-instruct--multiline-string-p directive-region-string) "" "s"))
                        "\n\n"
                        (format "%s\n%s\n%s"
                                markdown-delimiter
                                directive-region-string
                                markdown-delimiter))))))
                "\n\n"
                (if (not (string-empty-p (macher-instruct--directive-text directive)))
                    (format "The directive is:\n\n%s"
                            (macher-instruct--markdown-enquote (overlay-get directive 'macher-instruct-directive)))
                  (format "The directive is composed entirely out of %ss, so you should \
treat them as subdirectives, instead."
                          sd-typename))
                (cl-loop for sd in secondary-directives
                         when (not (string-empty-p (macher-instruct--directive-text sd)))
                         concat (concat
                                 "\n\n"
                                 (cl-destructuring-bind (sd-region-info sd-region)
                                     (macher-instruct--overlay-region-info sd)
                                   (concat
                                    (format "For file `%s`, %s"
                                            directive-filename-relpath
                                            sd-region-info)
                                    (let ((sd-text (macher-instruct--markdown-enquote
                                                    (overlay-get sd 'macher-instruct-directive))))
                                      (if (macher-instruct--bodyless-instruction-p sd)
                                          (format ", you have a %s:\n\n%s"
                                                  sd-typename
                                                  sd-text)
                                        (let ((markdown-delimiter
                                               (macher-instruct--delimiting-markdown-backticks
                                                sd-region)))
                                          (concat
                                           (format ", which correspond%s to:\n\n%s"
                                                   (if (macher-instruct--multiline-string-p sd-region)
                                                       "" "s")
                                                   (format "%s\n%s\n%s"
                                                           markdown-delimiter
                                                           sd-region
                                                           markdown-delimiter))
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
                              (macher-instruct--instruction-id directive-toplevel-reference)
                              directive-region-info-string)))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun macher-instruct--ancestral-commentators (instruction)
  "Return list of references which contain INSTRUCTION that have commentary.

The list is sorted with the topmost references first."
  (with-current-buffer (overlay-buffer instruction)
    (let* ((start (overlay-start instruction))
           (end (overlay-end instruction))
           (instructions (macher-instruct--instructions-in start end 'reference))
           (filtered (cl-remove-if-not (lambda (instr)
                                         (and (not (string-empty-p (macher-instruct--commentary-text instr)))
                                              (macher-instruct--subinstruction-of-p instruction instr)))
                                       instructions))
           (sorted (sort filtered (lambda (a b) (macher-instruct--subinstruction-of-p b a)))))
      sorted)))

(defun macher-instruct--create-id ()
  "Create a unique identifier for an instruction.

Retrieves an unused ID from retired IDs or generates a new one by
incrementing the ID counter. Tracks ID usage via a hash table."
  (let ((id
         (if macher-instruct--retired-ids
             (prog1
                 (car macher-instruct--retired-ids)
               (setq macher-instruct--retired-ids (cdr macher-instruct--retired-ids)))
           (cl-incf macher-instruct--id-counter))))
    (puthash id t macher-instruct--id-usage-map )
    id))

(defun macher-instruct--retire-id (id)
  "Retire an ID by removing it from `macher-instruct--id-usage-map'.
The id is added to `macher-instruct--retired-ids'"
  (when (gethash id macher-instruct--id-usage-map)
    (remhash id macher-instruct--id-usage-map)
    (push id macher-instruct--retired-ids)))

(defun macher-instruct--reset-id-counter ()
  "Reset all custom variables to their default values."
  (setq macher-instruct--id-counter 0)
  (setq macher-instruct--id-usage-map (make-hash-table))
  (setq macher-instruct--retired-ids ()))

(defun macher-instruct--instruction-outlinks (instruction)
  "Return the :to links of INSTRUCTION."
  (plist-get (overlay-get instruction 'macher-instruct-links) :to))

(defun macher-instruct--instruction-inlinks (instruction)
  "Return the :from links of INSTRUCTION."
  (plist-get (overlay-get instruction 'macher-instruct-links) :from))

(provide 'macher-instruct-instructions)

;;; macher-instruct-instructions.el ends here.
