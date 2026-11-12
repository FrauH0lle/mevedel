;;; mevedel-overlays.el -- Instruction geometry, tags, and context -*- lexical-binding: t -*-

;;; Commentary:

;; Core overlay system for mevedel instructions.  Instructions come in
;; two flavours: notes (explicit context) and directives (LLM requests).
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

;; `mevedel-directive-frame'
(autoload 'mevedel-directive-frame-display "mevedel-directive-frame")

;; `mevedel-directive-plan'
(declare-function mevedel-directive-plan--planning-prompt
                  "mevedel-directive-plan"
                  (implementation-prompt &optional feedback proposal))
(autoload 'mevedel-directive-plan--planning-prompt "mevedel-directive-plan")

;; `mevedel-directive-request'
(declare-function mevedel--directive-implementation-prompt
                  "mevedel-directive-request" (content directive &optional feedback))
(declare-function mevedel--discuss-directive-prompt
                  "mevedel-directive-request"
                  (content &optional directive message attempt-index))
(autoload 'mevedel--directive-implementation-prompt "mevedel-directive-request")
(autoload 'mevedel--discuss-directive-prompt "mevedel-directive-request")

;; `mevedel-directive'
(declare-function mevedel-directive-has-activity-p
                  "mevedel-directive" (directive))
(autoload 'mevedel-directive-has-activity-p "mevedel-directive")

;; `mevedel-directive-source'
(declare-function mevedel--create-directive-in
                  "mevedel-directive-source"
                  (buffer start end &optional bodyless directive-text span))
(declare-function mevedel--delete-instruction
                  "mevedel-directive-source"
                  (instruction &optional buffer))
(declare-function mevedel--delete-source-missing-directive
                  "mevedel-directive-source" (record workspace))
(declare-function mevedel--detached-directive-p
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-record
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-status
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-text
                  "mevedel-directive-source" (directive))
(declare-function mevedel--instruction-anchor-for-instruction
                  "mevedel-directive-source" (instruction))
(declare-function mevedel--instruction-persisted-properties
                  "mevedel-directive-source" (instruction))
(declare-function mevedel--reattach-directive-overlay
                  "mevedel-directive-source"
                  (id anchor workspace buffer start end))
(declare-function mevedel--remove-directive-presentation
                  "mevedel-directive-source" (directive &optional buffer))
(declare-function mevedel--set-directive-request
                  "mevedel-directive-source" (directive request))
(declare-function mevedel--set-directive-status
                  "mevedel-directive-source" (directive status))
(declare-function mevedel--subdirective-owner
                  "mevedel-directive-source" (directive &optional workspace))

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
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning-enabled
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
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


(defcustom mevedel-include-full-instructions t
  "Control whether instructions are fully included in the prompt.

When set to non-nil, the content of directives is
included in the prompt submitted to the LLM.  When nil, only file and
line numbers are included.

Setting this to nil makes the initial prompt shorter but relies on the
LLM to find and read the instructions.  Depending on the model, this
might yield better or worse results."
  :type 'boolean
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

(defun mevedel-modify-directive ()
  "Modify the directive under the point."
  (interactive)
  (when-let* ((directive (mevedel--highest-priority-instruction (mevedel--instructions-at (point) 'directive)
                                                                t)))
    (when (memq (mevedel--directive-status directive)
                '(implementing discussing))
      (mevedel--set-directive-status directive nil))
    (let ((topmost-directive (mevedel--topmost-instruction directive 'directive)))
      (when (eq (mevedel--directive-status topmost-directive) 'failed)
        (mevedel--set-directive-status topmost-directive nil)
        (mevedel--update-instruction-overlay topmost-directive t)))
    (mevedel--read-directive directive)))


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
TYPE is `note', `directive', or nil for both.  INTERACTIVEP requests
confirmation and offers inclusion of Source-missing directive records.
Return the deleted count; Source-missing records count once each, without
contributing to the buffer count.  Archived records are never deleted."
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
         (missing
          (when (and workspace (or (null type) (eq type 'directive)))
            (seq-filter
             (lambda (record)
               (eq 'source-missing
                   (plist-get (mevedel-directive-anchor record) :state)))
             (mevedel-workspace-directives workspace))))
         (noun (symbol-name (or type 'instruction)))
         (buffer-count (length (delete-dups (mapcar #'overlay-buffer instructions)))))
    (when missing
      (setq missing
            (when (and interactivep
                       (let ((query-replace-map (copy-keymap query-replace-map)))
                         (define-key query-replace-map (kbd "RET") 'skip)
                         (define-key query-replace-map [return] 'skip)
                         (y-or-n-p
                          (format "Include %d Source-missing directive%s as well (default no)? "
                                  (length missing)
                                  (if (= 1 (length missing)) "" "s")))))
              missing)))
    (let* ((count (+ (length instructions) (length missing)))
           (summary (format "%d %s%s in %d buffer%s"
                            count noun (if (= count 1) "" "s")
                            buffer-count (if (= buffer-count 1) "" "s"))))
      ;; Match single deletion's activity guard before making any changes.
      (dolist (record (append missing
                              (delq nil (mapcar #'mevedel--directive-record
                                                (seq-filter #'mevedel--directivep
                                                            instructions)))))
        (when (mevedel-directive-has-activity-p record)
          (mevedel-diagnostic-user-error (mevedel-diagnostic-scope (or workspace 'global))
                                       "Archive directives with activity")))
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
        ;; Deleting a parent already deletes its nested directives.
        (unless (overlay-get instruction 'mevedel-marked-for-deletion)
          (mevedel--delete-instruction instruction)))
      (dolist (record missing)
        (mevedel--delete-source-missing-directive record workspace))
      (when (memq mevedel--highlighted-instruction instructions)
        (setq mevedel--highlighted-instruction nil))
      (unless (zerop count)
        (message "Deleted %s" summary))
      count)))

;;;###autoload
(defun mevedel-delete-all-instructions ()
  "Delete notes and visible directives in the current workspace.
All buffers in that workspace are included.  Without a workspace, operate
on the shared global bucket of all buffers without a workspace instead.
Interactively, confirm counts and offer Source-missing directives too,
defaulting to no.  Archived directives remain; activity requires archiving."
  (interactive)
  (mevedel--delete-all-instructions-of-type nil (called-interactively-p 'any)))

;;;###autoload
(defun mevedel-delete-all-directives ()
  "Delete visible directives and their subdirectives in the current workspace.
All buffers in that workspace are included.  Without a workspace, operate
on the shared global bucket of all buffers without a workspace instead.
Interactively, offer Source-missing records once (default no), then confirm
counts.  Archived directives remain; activity requires archiving.
Leave notes intact."
  (interactive)
  (mevedel--delete-all-instructions-of-type 'directive (called-interactively-p 'any)))

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
    (let* ((record (mevedel--directive-record directive))
           (state (mevedel-directive-state record))
           (action (intern (completing-read "Preview action: "
                                            '("implement" "discuss")
                                            nil t nil nil "implement")))
           ;; Feedback and guidance are composed in the multiline scoped
           ;; composer at submission; the preview shows their slot instead
           ;; of soliciting throwaway minibuffer text.
           (request-string
            (if (eq action 'discuss)
                (mevedel--discuss-directive-prompt
                 (mevedel--directive-llm-prompt directive) record)
              (mevedel--directive-implementation-prompt
               (mevedel--directive-llm-prompt directive) record
               (pcase state
                 ('implemented "[requested changes, composed at submission]")
                 ((or 'failed 'aborted)
                  "[optional retry guidance, composed at submission]"))))))
      ;; Direct implementation runs attach the record's skills at
      ;; dispatch; mirror the attachment (without session validation)
      ;; so the preview shows the complete isolated request.
      (when-let* (((eq action 'implement))
                  ((not (mevedel-directive-planning-enabled record)))
                  (skills (mevedel-directive-skills record)))
        (setq request-string
              (mevedel-plan-handoff-append-implementation-input
               request-string (list :skills skills))))
      (when (and (eq action 'implement)
                 (mevedel-directive-planning-enabled record))
        (setq request-string
              (mevedel-directive-plan--planning-prompt request-string)))
      (let ((bufname "*mevedel-directive-preview*"))
        (with-temp-buffer-window bufname
            '((display-buffer-reuse-window
               display-buffer-same-window))
            nil
          (princ request-string)
          (with-current-buffer bufname
            (when-let* ((mode (mevedel-view--markdown-fontify-mode)))
              (funcall mode))
            (read-only-mode 1)
            (visual-line-mode 1)
            (display-line-numbers-mode 1)
            (let ((local-map (make-sparse-keymap)))
              (set-keymap-parent local-map (current-local-map))
              (define-key local-map (kbd "q") 'quit-window)
              (use-local-map local-map))))))))


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
                       ('directive "directive")
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

(defun mevedel--directive-empty-p (directive)
  "Check if DIRECTIVE is empty.

A directive is empty if it does not have a body or secondary directives."
  (let* ((record (or (mevedel--directive-record directive)
                     (error "Directive record not found")))
         (requests
          (cons (mevedel-directive-request record)
                (mapcar #'mevedel-subdirective-request
                        (mevedel-directive-subdirectives record)))))
    (not (cl-some (lambda (request)
                    (not (string-empty-p request)))
                  requests))))

(defun mevedel--create-instruction (type &optional span)
  "Create or scale an instruction of the given TYPE within the region.

If a region is selected but partially covers an existing instruction,
then the function will resize it.  See `mevedel-create-directive' for
details on how the resizing works.
SPAN, when non-nil, explicitly selects `line' or `char' geometry."
  (unless (eq type 'directive)
    (error "Unsupported instruction creation type: %s" type))
  ;; A directive's durable record is anchored to a file: without one it
  ;; can never be reattached after a load, and archiving or detaching it
  ;; would write a record that no longer loads at all.
  (when (and (eq type 'directive) (not (buffer-file-name)))
    (user-error "Cannot create a directive in a buffer visiting no file"))
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
                (when span (overlay-put instruction 'mevedel-span span))
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
                 (instruction (mevedel-view-call-preserving-window-configuration
                               (lambda ()
                                 ;; Minibuffer input can yield to chat redraws.
                                 ;; Restore their anchors, not stale raw points.
                                 (let ((pos (region-beginning)))
                                   (unless (<= (window-start) pos (window-end))
                                     (set-window-start (selected-window)
                                                       (max (point-min)
                                                            (- (region-beginning)
                                                               (- (window-end) (window-start))))))
                                   (mevedel--create-directive-in buffer
                                                                 (region-beginning)
                                                                 (region-end)
                                                                 nil nil span))))))
            (with-current-buffer buffer
              (deactivate-mark))
            instruction)))
    (when (eq type 'directive)
      (prog1 (mevedel--create-directive-in (current-buffer) (point) (point) t)
        (deactivate-mark)))))

;;;###autoload
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

Instruction type can either be `note' or `directive'."
  (if-let* ((type (overlay-get instruction 'mevedel-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))


(defun mevedel-get-directive-patch (directive)
  "Get the stored patch for DIRECTIVE, if any.
Returns the unified diff string, or nil if no patch is stored."
  (when-let* ((record (mevedel--directive-record directive))
              (attempt (car (last (mevedel-directive-attempts record))))
              (patch (mevedel-directive-attempt-patch attempt))
              ((not (string-empty-p patch))))
    patch))

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
                                (not
                                 (and (mevedel--detached-directive-p instruction)
                                      (mevedel--detached-directive-p instr)))
                                (<= (overlay-start instr) beg
                                    end (overlay-end instr))
                                ;; Equal ranges can result from replacement.
                                ;; Neither is the other's geometric parent.
                                (or (< (overlay-start instr) beg)
                                    (< end (overlay-end instr)))))
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

(defun mevedel--nested-directives (directive)
  "Return every directive nested under DIRECTIVE in stable source order."
  (sort
   (cl-remove-if-not
    (lambda (instruction)
      (and (mevedel--directivep instruction)
           (not (eq instruction directive))))
    (mevedel--wholly-contained-instructions
     (overlay-buffer directive)
     (overlay-start directive)
     (overlay-end directive)))
   (lambda (a b)
     (or (< (overlay-start a) (overlay-start b))
         (and (= (overlay-start a) (overlay-start b))
              (or (> (overlay-end a) (overlay-end b))
                  (and (= (overlay-end a) (overlay-end b))
                       (string-lessp (overlay-get a 'mevedel-uuid)
                                     (overlay-get b 'mevedel-uuid)))))))))


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
  ;; Durable directive ownership outlives source geometry.  Replacements can
  ;; make parent and detail presentations coextensive without changing owner.
  (when (eq of-type 'directive)
    (when-let* ((owner (mevedel--subdirective-owner instruction))
                (parent
                 (with-current-buffer (overlay-buffer instruction)
                   ;; Registry lookup can hydrate this file recursively while
                   ;; its restored presentations are still being rendered.
                   (save-restriction
                     (widen)
                     (cl-find (mevedel-directive-id (car owner))
                              (overlays-in (point-min) (point-max))
                              :key (lambda (overlay)
                                     (overlay-get overlay 'mevedel-uuid))
                              :test #'equal)))))
      (cl-return-from mevedel--topmost-instruction
        (and (or (null pred) (funcall pred parent)) parent))))
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


(defun mevedel--directive-at-point ()
  "Return the top-level owner of the selected directive at point, or nil.
Prefer the highlighted directive when present, otherwise overlay priority."
  (mevedel--topmost-instruction
   (mevedel--highest-priority-instruction
    (mevedel--instructions-at (point) 'directive) t)
   'directive))

(defun mevedel--directive-truncated-text (directive)
  "Return the truncated directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (mevedel-truncate-directive (mevedel--directive-text directive)))


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
  (mevedel--truncate-display text mevedel-instructions-truncated-max t))

(defun mevedel--read-directive (directive)
  "Prompt user to enter a directive text via minibuffer for DIRECTIVE."
  (let ((original-directive-text (mevedel--directive-text directive))
        (original-directive-status (mevedel--directive-status directive))
        (set-directive-text (lambda (directive text)
                              (mevedel--set-directive-request directive text)
                              (unless (overlay-get directive 'mevedel-instruction-collapse-p)
                                (overlay-put directive 'mevedel-instruction-collapse-p
                                             (if (> (length text)
                                                    mevedel-instructions-truncated-max)
                                                 'collapse
                                               'expand))))))
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
                      (mevedel--set-directive-request
                       directive (minibuffer-contents))
                      (mevedel--update-instruction-overlay directive))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Directive: " original-directive-text)
        (quit
         (if (string-empty-p original-directive-text)
             (mevedel--delete-instruction directive)
           (funcall set-directive-text directive original-directive-text)
           (mevedel--set-directive-status directive original-directive-status)
           (mevedel--update-instruction-overlay directive nil))
         (signal 'quit nil))))))


(defun mevedel--directive-llm-prompt (directive)
  "Craft the prompt for the LLM model associated with the DIRECTIVE."
  (when (mevedel--directive-empty-p directive)
    (error "Directive %s is empty" directive))
  (let* ((directive-buffer (overlay-buffer directive))
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
             (let* ((detached-p (mevedel--detached-directive-p directive))
                    (secondary-directives
                     (if detached-p
                         (mevedel-directive-subdirectives
                          (mevedel--directive-record directive))
                       (mevedel--nested-directives directive)))
                    (sd-typename (if (not (eq (mevedel--directive-status directive)
                                              'implemented))
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
                            (mevedel--markdown-enquote
                             (mevedel--directive-text directive)))
                  (format "The directive is composed entirely out of %ss, so you should \
treat them as subdirectives, instead."
                          sd-typename))
                (cl-loop
                 for sd in secondary-directives
                 for sd-request =
                 (if detached-p
                     (mevedel-subdirective-request sd)
                   (mevedel--directive-text sd))
                 when (not (string-empty-p sd-request))
                 concat
                 (if detached-p
                     (format "\n\nYou have the %s:\n\n%s"
                             sd-typename
                             (mevedel--markdown-enquote sd-request))
                   (concat
                    "\n\n"
                    (cl-destructuring-bind (sd-region-info sd-region)
                        (mevedel--overlay-region-info sd)
                      (concat
                       (format "For file `%s`, %s"
                               directive-filename-relpath
                               sd-region-info)
                       (let ((sd-text (mevedel--markdown-enquote sd-request)))
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
                                          (if (mevedel--multiline-string-p
                                               sd-region)
                                              "" "s")
                                          (format "%s\n%s\n%s"
                                                  markdown-delimiter
                                                  sd-region
                                                  markdown-delimiter))
                                ".")
                              (format "\n\nYou have the %s:\n\n%s"
                                      sd-typename
                                      sd-text))))))))))))))
        (with-temp-buffer
          (insert
           (concat
            "Listed below is a directive."
            (format "\n\n### Directive\n\n")
            (format "For %s%s"
                    (if directive-filename
                        (format "file `%s`" directive-filename-relpath)
                      (format "buffer `%s`" (buffer-name directive-buffer)))
                    expanded-directive-text)))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun mevedel--directive-action-context (record workspace)
  "Return RECORD's live directive and validated prompt in WORKSPACE.

This is the shared individual and batch submission eligibility check."
  (let* ((anchor (mevedel-directive-anchor record))
         (evidence (plist-get anchor :evidence))
         (directive
          (or
           (mevedel--instruction-with-uuid
            (mevedel-directive-id record) workspace)
           (when (and (eq 'source-missing (plist-get anchor :state))
                      (plist-get evidence :bodyless)
                      (null (plist-get evidence :parent-uuid))
                      (null (mevedel-directive-subdirectives record)))
             (let ((buffer
                    (generate-new-buffer
                     (format " *mevedel-directive-context %s*"
                             (mevedel-directive-id record)))))
               (with-current-buffer buffer
                 (setq-local mevedel--workspace workspace
                             buffer-file-name (plist-get anchor :file)
                             default-directory
                             (mevedel-workspace-root workspace))
                 (let ((overlay
                        (mevedel--reattach-directive-overlay
                         (mevedel-directive-id record) anchor workspace
                         buffer (point-min) (point-min))))
                   (overlay-put overlay 'mevedel-transient-source-missing t)
                   overlay)))))))
    (unless directive
      (mevedel-diagnostic-user-error (mevedel-diagnostic-scope workspace)
                                   "Directive prompt context is unavailable; reattach its source first"))
    (condition-case err
        (list :directive directive
              :prompt (mevedel--directive-llm-prompt directive))
      (error
       (when (overlay-get directive 'mevedel-transient-source-missing)
         (let ((buffer (overlay-buffer directive)))
           (mevedel--remove-directive-presentation directive)
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))
       (signal (car err) (cdr err))))))


(provide 'mevedel-overlays)
;;; mevedel-overlays.el ends here
