;;; niwa-overlays.el -- Instruction geometry, tags, and context -*- lexical-binding: t -*-

;;; Commentary:

;; Core overlay system for niwa notes with explicit source context.
;;
;; This core owns containment and tree queries, tag/context semantics,
;; navigation, and prompt assembly.  `niwa-instruction-registry' owns
;; workspace buckets, IDs, and links; `niwa-note-source' owns durable
;; record/presentation mutation; `niwa-overlay-ui' owns actions and
;; rendering.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'niwa-note-source)
(require 'niwa-instruction-registry)
(require 'niwa-utilities)

;; `gptel'
(defvar gptel--fsm-last)
(defvar gptel-display-buffer-action)

;; `gptel-request'
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `niwa-chat'
(autoload 'niwa--active-chat-buffer "niwa-chat")
(autoload 'niwa--chat-buffer "niwa-chat")
(autoload 'niwa--patch-buffer "niwa-chat")
(autoload 'niwa--replace-patch-buffer "niwa-chat")
(defvar niwa--view-buffer)

;; `niwa-instruction-registry'
(declare-function niwa--instruction-activate-buffer
                  "niwa-instruction-registry" (&optional buffer))
(declare-function niwa--instruction-activate-workspace
                  "niwa-instruction-registry" (&optional workspace))
(declare-function niwa--instruction-buffer-workspace
                  "niwa-instruction-registry" (buffer))
(declare-function niwa--instruction-id
                  "niwa-instruction-registry" (instruction))
(declare-function niwa--instruction-inlinks
                  "niwa-instruction-registry" (instruction))
(declare-function niwa--instruction-operation-state-key
                  "niwa-instruction-registry" ())
(declare-function niwa--instruction-outlinks
                  "niwa-instruction-registry" (instruction))
(declare-function niwa--instruction-state
                  "niwa-instruction-registry" (&optional key))
(declare-function niwa-link-instructions
                  "niwa-instruction-registry" (from-list to-list))
(declare-function niwa--instruction-with-id
                  "niwa-instruction-registry"
                  (target-id &optional workspace))
(declare-function niwa--instruction-with-uuid
                  "niwa-instruction-registry"
                  (uuid &optional workspace))
(declare-function niwa--instruction-workspace-key
                  "niwa-instruction-registry" (&optional workspace))
(defvar niwa--instruction-state-key-override)

;; `niwa-note-source'
(declare-function niwa--delete-instruction
                  "niwa-note-source"
                  (instruction &optional buffer))
(declare-function niwa--instruction-anchor-for-instruction
                  "niwa-note-source" (instruction))
(declare-function niwa--instruction-persisted-properties
                  "niwa-note-source" (instruction))

;; `niwa-overlay-ui'
(declare-function niwa--update-instruction-overlay
                  "niwa-overlay-ui"
                  (instruction &optional update-children))
(autoload 'niwa--update-instruction-overlay "niwa-overlay-ui")

;; `niwa-persistence'
(declare-function niwa--restore-file-instructions
                  "niwa-persistence" (file &optional message workspace))

;; `niwa-plan-handoff'
(declare-function niwa-plan-handoff-append-implementation-input
                  "niwa-plan-handoff" (prompt selection))
(autoload 'niwa-plan-handoff-append-implementation-input
  "niwa-plan-handoff")

;; `niwa-structs'
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)

;; `niwa-view-composer'
(autoload 'niwa-view-call-preserving-window-configuration
  "niwa-view-composer")

;; `niwa-view-fontify'
(declare-function niwa-view--markdown-fontify-mode "niwa-view-fontify" ())

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(autoload 'niwa-workspace "niwa-workspace")


(defvar niwa--default-instruction-priority 0)
(defvar niwa--highlighted-instruction nil)


(defun niwa-cycle-instructions-at-point (point)
  "Cycle through instructions at POINT, highlighting them.

This command allows for cycling through overlapping instructions at a
point in the buffer and allows one to have better accuracy when
instructions overlap to the point where no other reasonable option is
available."
  (interactive "d")
  (let ((instructions-at-point (niwa--instructions-at point))
        (scope (niwa-diagnostic-scope
                (niwa--instruction-buffer-workspace (current-buffer))))
        (original-highlighted-instruction niwa--highlighted-instruction))
    (cond
     ((null instructions-at-point)
      (setq niwa--highlighted-instruction nil)
      (when (called-interactively-p 'any)
        (niwa-diagnostic-message scope
                                    "No instructions at point")))
     ((or (null niwa--highlighted-instruction)
          (not (memq niwa--highlighted-instruction instructions-at-point)))
      (setq niwa--highlighted-instruction nil)
      (setq niwa--highlighted-instruction (niwa--highest-priority-instruction instructions-at-point)))
     (t
      (if-let* ((parent (niwa--parent-instruction niwa--highlighted-instruction)))
          (setq niwa--highlighted-instruction parent)
        (setq niwa--highlighted-instruction nil))))
    (when niwa--highlighted-instruction
      (niwa--update-instruction-overlay niwa--highlighted-instruction))
    (when original-highlighted-instruction
      (niwa--update-instruction-overlay original-highlighted-instruction))
    niwa--highlighted-instruction))

(defun niwa-delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were
deleted.  Throw a user error if no instructions to delete were found."
  (interactive)
  (let ((deleted-count 0)
        (scope (niwa-diagnostic-scope
                (niwa--instruction-buffer-workspace (current-buffer)))))
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (dolist (overlay (niwa--wholly-contained-instructions (current-buffer) start end))
            (when (overlay-get overlay 'niwa-instruction)
              (niwa--delete-instruction overlay)
              (setq deleted-count (1+ deleted-count))))
          (when (> deleted-count 0)
            (deactivate-mark))
          (unless (> deleted-count 0)
            (niwa-diagnostic-user-error scope
                                         "No instructions to delete within the selected region")))
      (let ((overlay (niwa--delete-instruction-at (point))))
        (when overlay
          (setq deleted-count 1))
        (unless overlay
          (niwa-diagnostic-user-error scope
                                       "No instruction to delete at point"))))
    (when (> deleted-count 0)
      (message "Deleted %d instruction%s" deleted-count (if (> deleted-count 1) "s" "")))))

(defun niwa--delete-all-instructions-of-type (type interactivep)
  "Delete instructions of TYPE in the current workspace.
TYPE is `note' or nil.  INTERACTIVEP requests confirmation.
Return the deleted count."
  (let* ((workspace (niwa--instruction-buffer-workspace (current-buffer)))
         (niwa--instruction-state-key-override
          (niwa--instruction-workspace-key workspace))
         (instructions
          (progn
            ;; Reopened external sources must keep the registry's owner,
            ;; rather than resolving a different workspace from their path.
            (dolist (entry (copy-sequence (niwa--instruction-alist)))
              (when (stringp (car entry))
                (niwa--restore-file-instructions (car entry) nil workspace)))
            (seq-filter
             (lambda (instruction)
               (or (null type)
                   (eq type (overlay-get instruction 'niwa-instruction-type))))
             (niwa--all-instructions))))
         (noun (symbol-name (or type 'instruction)))
         (buffer-count (length (delete-dups (mapcar #'overlay-buffer instructions)))))
    (let* ((count (length instructions))
           (summary (format "%d %s%s in %d buffer%s"
                            count noun (if (= count 1) "" "s")
                            buffer-count (if (= buffer-count 1) "" "s"))))
      (when interactivep
        (when (zerop count)
          (niwa-diagnostic-user-error (niwa-diagnostic-scope (or workspace 'global))
                                       "No %ss to delete" noun))
        (unless (y-or-n-p (format "Delete %s in %s? " summary
                                 (if workspace "the current workspace"
                                   "the shared global bucket")))
          (niwa-diagnostic-user-error (niwa-diagnostic-scope (or workspace 'global))
                                       "Aborted")))
      (dolist (instruction instructions)
        ;; Deleting a parent already deletes its nested notes.
        (unless (overlay-get instruction 'niwa-marked-for-deletion)
          (niwa--delete-instruction instruction)))
      (when (memq niwa--highlighted-instruction instructions)
        (setq niwa--highlighted-instruction nil))
      (unless (zerop count)
        (message "Deleted %s" summary))
      count)))

;;;###autoload
(defun niwa-delete-all-instructions ()
  "Delete notes in the current workspace.
All buffers in that workspace are included.  Without a workspace, operate
on the shared global bucket of all buffers without a workspace instead.
Interactively, confirm the number of notes and buffers."
  (interactive)
  (niwa--delete-all-instructions-of-type nil (called-interactively-p 'any)))

;;;###autoload
(defun niwa-next-instruction ()
  "Cycle through instructions in the forward direction."
  (interactive)
  (unless (niwa--cycle-instruction nil 'next)
    (niwa--print-instruction-not-found 'next nil)))

(defun niwa-previous-instruction ()
  "Cycle through instructions in the backward direction."
  (interactive)
  (unless (niwa--cycle-instruction nil 'previous)
    (niwa--print-instruction-not-found 'previous nil)))


(defun niwa--stashed-buffer-instructions (buffer)
  "Return stashed instruction data for all instructions in BUFFER.

Each instruction is represented as a plist with :overlay-start,
:overlay-end, :anchor, and :properties keys, capturing the overlay's
position, lightweight re-anchoring context, and semantic properties for
later restoration."
  (niwa--foreach-instruction (instr buffer)
    collect (list :overlay-start (overlay-start instr)
                  :overlay-end (overlay-end instr)
                  :anchor (niwa--instruction-anchor-for-instruction
                           instr)
                  :properties (niwa--instruction-persisted-properties
                               instr))))

(defun niwa--stash-buffer (buffer &optional file-contents)
  "Stash BUFFER's instructions and original content.

Save the buffer's instructions and original content to
`(niwa--instruction-alist)', then remove the instruction overlays from the
buffer.  The content is either the current buffer content or
FILE-CONTENTS."
  (niwa--instruction-activate-buffer buffer)
  (let ((instrs (niwa--stashed-buffer-instructions buffer)))
    (when instrs
      (with-current-buffer buffer
        (let ((original-content (or file-contents (buffer-substring-no-properties (point-min)
                                                                                  (point-max)))))
          (setf (alist-get buffer (niwa--instruction-alist))
                (list :original-content original-content
                      :instructions instrs)
                (car (assoc buffer (niwa--instruction-alist)))
                (buffer-file-name buffer))
          (mapc #'niwa--delete-instruction-overlay
                (niwa--instructions-in (point-min) (point-max))))))))

(defun niwa--print-instruction-not-found (direction type)
  "Print a not found message for the given DIRECTION and TYPE."
  (let ((type-string (pcase type
                       ('note "note")
                       (_ "instruction")))
        (scope (niwa-diagnostic-scope
                (niwa--instruction-buffer-workspace (current-buffer)))))
    (niwa-diagnostic-message scope
             "No %s %s found"
             (if (eq direction 'next) "next" "previous")
             type-string)))


(defun niwa--cycle-instruction (type direction)
  "Get the next or previous instruction overlay of TYPE.
DIRECTION should be `next' or `previous' from the current point.

If no instruction found in the buffer, checks the next buffers in the
`(niwa--instruction-alist)' alist.

Returns the found instruction, if any."
  ;; We want the buffers to be a cyclic list, based on the current buffer.
  (niwa--instruction-activate-buffer)
  (let* ((buffers (let ((bufs (mapcar #'car (niwa--instruction-alist))))
                    (if (eq direction 'next)
                        (niwa--cycle-list-around (current-buffer) bufs)
                      (niwa--cycle-list-around (current-buffer) (nreverse bufs)))))
         (original-buffer (current-buffer))
         (found-instr))
    (while (and buffers (null found-instr))
      (let* ((buffer (car buffers))
             (instrs (niwa--foreach-instruction (instr buffer) collect instr)))
        (setq buffers (delq buffer buffers))
        (when type
          (setq instrs (cl-remove-if-not (lambda (instr)
                                           (eq (niwa--instruction-type instr) type))
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


(defun niwa--delete-instruction-at (point)
  "Delete the instruction at POINT.

Returns the deleted instruction overlay."
  (let* ((instructions (niwa--instructions-at point))
         (target (niwa--highest-priority-instruction instructions t)))
    (when target
      (niwa--delete-instruction target))))

;;;###autoload
(cl-defun niwa--highest-priority-instruction (instructions &optional return-highlighted)
  "Return the instruction with the highest priority from the INSTRUCTIONS list.

Priority here refers to the priority property used by overlays.

If RETURN-HIGHLIGHTED is non-nil and
`niwa--highlighted-instruction' is non-nil, the function will
return `niwa--highlighted-instruction' if it is also in the
INSTRUCTIONS list."
  (when (and return-highlighted
             niwa--highlighted-instruction
             (member niwa--highlighted-instruction instructions))
    (cl-return-from niwa--highest-priority-instruction niwa--highlighted-instruction))
  (cl-reduce (lambda (acc instruction)
               (if (or (not acc)
                       (> (or (overlay-get instruction 'priority)
                              niwa--default-instruction-priority)
                          (or (overlay-get acc 'priority)
                              niwa--default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun niwa--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay.

The instruction type is `note'."
  (if-let* ((type (overlay-get instruction 'niwa-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))


(defun niwa--parent-instruction (instruction &optional of-type)
  "Return the parent of the given INSTRUCTION overlay.

If OF-TYPE is non-nil, returns the parent with the given type."
  (with-current-buffer (overlay-buffer instruction)
    (let ((beg (overlay-start instruction))
          (end (overlay-end instruction)))
      (niwa--highest-priority-instruction
       (cl-remove-if-not (lambda (instr)
                           (and (not (eq instr instruction))
                                (or (null of-type)
                                    (eq (niwa--instruction-type instr)
                                        of-type))
                                (<= (overlay-start instr) beg
                                    end (overlay-end instr))
                                ;; Equal ranges can result from replacement.
                                ;; Neither is the other's geometric parent.
                                (or (< (overlay-start instr) beg)
                                    (< end (overlay-end instr)))))
                         (niwa--instructions-in beg end))))))

(defun niwa--child-instructions (instruction)
  "Return the direct child instructions of the given INSTRUCTION overlay."
  (let ((children (cl-remove-if (lambda (instr)
                                  (or (eq instr instruction)
                                      (and (= (overlay-start instr) (overlay-start instruction))
                                           (= (overlay-end instr) (overlay-end instruction)))))
                                (niwa--wholly-contained-instructions (overlay-buffer instruction)
                                                                        (overlay-start instruction)
                                                                        (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl-set-difference children
                                        (niwa--child-instructions child))))
    children))

(defun niwa--buffer-has-instructions-p (buffer)
  "Return non-nil if BUFFER has any niwa instructions associated with it."
  (niwa--instruction-activate-buffer buffer)
  (assoc buffer (niwa--instruction-alist)))

(defun niwa--wholly-contained-instructions (buffer start end)
  "Return niwa overlays in BUFFER that are entirely within START and END."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'niwa-instruction)
                             (>= (overlay-start ov) start)
                             (<= (overlay-end ov) end)))
                      (overlays-in start end))))

(defun niwa--instructions-at (point &optional type)
  "Return a list of instructions at current POINT.

Optionally return only instructions of specific TYPE.  Also returns
bodyless overlays located right before the point."
  (niwa--instructions-in point (min (point-max) (1+ point)) type))

(defun niwa--instructions-in (start end &optional type)
  "Return a list of instructions in region delimited by START and END.

Optionally return only instructions of specific TYPE."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'niwa-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'niwa-instruction-type)
                                        type))
                               (null type))))
                    (overlays-in start end)))

(defun niwa--all-instructions ()
  "Return a list of all currently loaded instructions."
  (niwa--foreach-instruction inst collect inst))

(cl-defun niwa--topmost-instruction (instruction &optional of-type pred)
  "Return the topmost instruction containing the INSTRUCTION, if any.

If OF-TYPE is non-nil, filter by the specified instruction OF-TYPE.  If
OF-TYPE is nil, the instruction returned is the top-level one.

If PRED is non-nil, then the best instruction must also satisfy it.  The
PRED must be a function which accepts an instruction."
  (unless instruction
    (cl-return-from niwa--topmost-instruction nil))
  (with-current-buffer (overlay-buffer instruction)
    (let ((best-instruction instruction))
      (cl-labels ((parent-instr (instr)
                    (if-let* ((parent (niwa--parent-instruction instr)))
                        (progn
                          (when (and (or (null of-type) (eq of-type (niwa--instruction-type parent)))
                                     (or (null pred) (funcall pred parent)))
                            (setq best-instruction parent))
                          (parent-instr parent))
                      best-instruction)))
        (setq best-instruction (parent-instr instruction)))
      (if (and (or (null of-type) (eq of-type (niwa--instruction-type best-instruction)))
               (or (null pred) (funcall pred best-instruction)))
          best-instruction
        nil))))


(provide 'niwa-overlays)
;;; niwa-overlays.el ends here
