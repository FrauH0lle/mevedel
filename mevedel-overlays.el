;;; mevedel-overlays.el -- Instruction geometry, tags, and context -*- lexical-binding: t -*-

;;; Commentary:

;; Core overlay system for mevedel notes with explicit source context.
;;
;; This core owns containment and tree queries, tag/context semantics,
;; navigation, and prompt assembly.  `mevedel-instruction-registry' owns
;; workspace buckets, IDs, and links; `mevedel-directive-source' owns durable
;; record/presentation mutation; `mevedel-overlay-ui' owns actions and
;; rendering.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'mevedel-directive-source)
(require 'mevedel-instruction-registry)
(require 'mevedel-utilities)

;; `gptel'
(defvar gptel--fsm-last)
(defvar gptel-display-buffer-action)

;; `gptel-request'
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `mevedel-chat'
(autoload 'mevedel--active-chat-buffer "mevedel-chat")
(autoload 'mevedel--chat-buffer "mevedel-chat")
(autoload 'mevedel--patch-buffer "mevedel-chat")
(autoload 'mevedel--replace-patch-buffer "mevedel-chat")
(defvar mevedel--view-buffer)

;; `mevedel-directive-source'
(declare-function mevedel--delete-instruction
                  "mevedel-directive-source"
                  (instruction &optional buffer))
(declare-function mevedel--instruction-anchor-for-instruction
                  "mevedel-directive-source" (instruction))
(declare-function mevedel--instruction-persisted-properties
                  "mevedel-directive-source" (instruction))
;; `mevedel-instruction-registry'
(declare-function mevedel--instruction-activate-buffer
                  "mevedel-instruction-registry" (&optional buffer))
(declare-function mevedel--instruction-activate-workspace
                  "mevedel-instruction-registry" (&optional workspace))
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-instruction-registry" (buffer))
(declare-function mevedel--instruction-id
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-inlinks
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-operation-state-key
                  "mevedel-instruction-registry" ())
(declare-function mevedel--instruction-outlinks
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-state
                  "mevedel-instruction-registry" (&optional key))
(declare-function mevedel-link-instructions
                  "mevedel-instruction-registry" (from-list to-list))
(declare-function mevedel--instruction-with-id
                  "mevedel-instruction-registry"
                  (target-id &optional workspace))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-instruction-registry"
                  (uuid &optional workspace))
(declare-function mevedel--instruction-workspace-key
                  "mevedel-instruction-registry" (&optional workspace))
(defvar mevedel--instruction-state-key-override)

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))
(autoload 'mevedel--update-instruction-overlay "mevedel-overlay-ui")

;; `mevedel-persistence'
(declare-function mevedel--restore-file-instructions
                  "mevedel-persistence" (file &optional message workspace))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-append-implementation-input
                  "mevedel-plan-handoff" (prompt selection))
(autoload 'mevedel-plan-handoff-append-implementation-input
  "mevedel-plan-handoff")

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-view-composer'
(autoload 'mevedel-view-call-preserving-window-configuration
  "mevedel-view-composer")
(defvar mevedel-view--input-marker)

;; `mevedel-view-fontify'
(declare-function mevedel-view--markdown-fontify-mode "mevedel-view-fontify" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(autoload 'mevedel-workspace "mevedel-workspace")


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


(defvar mevedel--default-instruction-priority -99)
(defvar mevedel--highlighted-instruction nil)


(defun mevedel-cycle-instructions-at-point (point)
  "Cycle through instructions at POINT, highlighting them.

This command allows for cycling through overlapping instructions at a
point in the buffer and allows one to have better accuracy when
instructions overlap to the point where no other reasonable option is
available."
  (interactive "d")
  (let ((instructions-at-point (mevedel--instructions-at point))
        (scope (mevedel-diagnostic-scope
                (mevedel--instruction-buffer-workspace (current-buffer))))
        (original-highlighted-instruction mevedel--highlighted-instruction))
    (cond
     ((null instructions-at-point)
      (setq mevedel--highlighted-instruction nil)
      (when (called-interactively-p 'any)
        (mevedel-diagnostic-message scope
                                    "No instructions at point")))
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

(defun mevedel-delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were
deleted.  Throw a user error if no instructions to delete were found."
  (interactive)
  (let ((deleted-count 0)
        (scope (mevedel-diagnostic-scope
                (mevedel--instruction-buffer-workspace (current-buffer)))))
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
            (mevedel-diagnostic-user-error scope
                                         "No instructions to delete within the selected region")))
      (let ((overlay (mevedel--delete-instruction-at (point))))
        (when overlay
          (setq deleted-count 1))
        (unless overlay
          (mevedel-diagnostic-user-error scope
                                       "No instruction to delete at point"))))
    (when (> deleted-count 0)
      (message "Deleted %d instruction%s" deleted-count (if (> deleted-count 1) "s" "")))))

(defun mevedel--delete-all-instructions-of-type (type interactivep)
  "Delete instructions of TYPE in the current workspace.
TYPE is `note' or nil.  INTERACTIVEP requests confirmation.
Return the deleted count."
  (let* ((workspace (mevedel--instruction-buffer-workspace (current-buffer)))
         (mevedel--instruction-state-key-override
          (mevedel--instruction-workspace-key workspace))
         (instructions
          (progn
            ;; Reopened external sources must keep the registry's owner,
            ;; rather than resolving a different workspace from their path.
            (dolist (entry (copy-sequence (mevedel--instruction-alist)))
              (when (stringp (car entry))
                (mevedel--restore-file-instructions (car entry) nil workspace)))
            (seq-filter
             (lambda (instruction)
               (or (null type)
                   (eq type (overlay-get instruction 'mevedel-instruction-type))))
             (mevedel--all-instructions))))
         (noun (symbol-name (or type 'instruction)))
         (buffer-count (length (delete-dups (mapcar #'overlay-buffer instructions)))))
    (let* ((count (length instructions))
           (summary (format "%d %s%s in %d buffer%s"
                            count noun (if (= count 1) "" "s")
                            buffer-count (if (= buffer-count 1) "" "s"))))
      (when interactivep
        (when (zerop count)
          (mevedel-diagnostic-user-error (mevedel-diagnostic-scope (or workspace 'global))
                                       "No %ss to delete" noun))
        (unless (y-or-n-p (format "Delete %s in %s? " summary
                                 (if workspace "the current workspace"
                                   "the shared global bucket")))
          (mevedel-diagnostic-user-error (mevedel-diagnostic-scope (or workspace 'global))
                                       "Aborted")))
      (dolist (instruction instructions)
        ;; Deleting a parent already deletes its nested notes.
        (unless (overlay-get instruction 'mevedel-marked-for-deletion)
          (mevedel--delete-instruction instruction)))
      (when (memq mevedel--highlighted-instruction instructions)
        (setq mevedel--highlighted-instruction nil))
      (unless (zerop count)
        (message "Deleted %s" summary))
      count)))

;;;###autoload
(defun mevedel-delete-all-instructions ()
  "Delete notes in the current workspace.
All buffers in that workspace are included.  Without a workspace, operate
on the shared global bucket of all buffers without a workspace instead.
Interactively, confirm the number of notes and buffers."
  (interactive)
  (mevedel--delete-all-instructions-of-type nil (called-interactively-p 'any)))

;;;###autoload
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
          (mapc #'mevedel--delete-instruction-overlay
                (mevedel--instructions-in (point-min) (point-max))))))))

(defun mevedel--print-instruction-not-found (direction type)
  "Print a not found message for the given DIRECTION and TYPE."
  (let ((type-string (pcase type
                       ('note "note")
                       (_ "instruction")))
        (scope (mevedel-diagnostic-scope
                (mevedel--instruction-buffer-workspace (current-buffer)))))
    (mevedel-diagnostic-message scope
             "No %s %s found"
             (if (eq direction 'next) "next" "previous")
             type-string)))


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


(defun mevedel--delete-instruction-at (point)
  "Delete the instruction at POINT.

Returns the deleted instruction overlay."
  (let* ((instructions (mevedel--instructions-at point))
         (target (mevedel--highest-priority-instruction instructions t)))
    (when target
      (mevedel--delete-instruction target))))

;;;###autoload
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

The instruction type is `note'."
  (if-let* ((type (overlay-get instruction 'mevedel-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))


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
                                    end (overlay-end instr))
                                ;; Equal ranges can result from replacement.
                                ;; Neither is the other's geometric parent.
                                (or (< (overlay-start instr) beg)
                                    (< end (overlay-end instr)))))
                         (mevedel--instructions-in beg end))))))

(defun mevedel--subinstruction-of-p (sub parent)
  "Return t is instruction SUB is contained entirely within instruction PARENT.

In this case, an instruction is _not_ considered a subinstruction of
itself."
  (and (eq (overlay-buffer sub)
           (overlay-buffer parent))
       (<= (overlay-start parent) (overlay-start sub) (overlay-end sub) (overlay-end parent))
       (and (/= (overlay-start parent) (overlay-start sub))
            (/= (overlay-end parent) (overlay-end sub)))))

(defun mevedel--child-instructions (instruction)
  "Return the direct child instructions of the given INSTRUCTION overlay."
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

(defun mevedel--instruction-bufferlevel-p (instruction)
  "Return t if INSTRUCTION spans the entirety of its buffer."
  (let ((buffer (overlay-buffer instruction)))
    (when buffer
      (with-current-buffer buffer
        (without-restriction
          (and (= (overlay-start instruction) (point-min))
               (= (overlay-end instruction) (point-max))))))))


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
  (mevedel--instructions-in point (min (point-max) (1+ point)) type))

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


(provide 'mevedel-overlays)
;;; mevedel-overlays.el ends here
