;;; mevedel-overlays.el -- Instruction geometry, tags, and context -*- lexical-binding: t -*-

;;; Commentary:

;; Core overlay system for mevedel instructions.  Instructions come in
;; two flavours: references (provide context, tagged for query) and
;; directives (LLM prompts that may query references by tag).
;;
;; This core owns containment and tree queries, tag/context semantics,
;; navigation, and prompt assembly.  `mevedel-instruction-registry' owns
;; workspace buckets, IDs, and links; `mevedel-directive-source' owns durable
;; record/presentation mutation; `mevedel-overlay-ui' owns actions and
;; rendering.

;;; Code:

(eval-when-compile (require 'mevedel-instruction-registry))
(require 'cl-lib)
(require 'subr-x)

(require 'mevedel-utilities)

;; `gptel'
(defvar gptel-display-buffer-action)
(defvar gptel--fsm-last)

;; `gptel-request'
(declare-function gptel--model-name "ext:gptel-request" (model))
(declare-function gptel-fsm-info "ext:gptel-request" (fsm))
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer "mevedel-chat" (&optional workspace))
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--directive-bound-session-buffer
                  "mevedel-chat" (record workspace))
(declare-function mevedel--directive-implementation-prompt
                  "mevedel-chat" (content directive &optional feedback))
(declare-function mevedel--directive-session-buffer
                  "mevedel-chat" (directive workspace))
(declare-function mevedel--discuss-directive-prompt
                  "mevedel-chat"
                  (content &optional directive message attempt-index))
(declare-function mevedel--patch-buffer "mevedel-chat" (&optional create workspace))
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch-content))
(defvar mevedel--view-buffer)

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))
(declare-function mevedel-directive-add-subdirective
                  "mevedel-directive" (directive subdirective))
(declare-function mevedel-directive-has-activity-p
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-invalidate-plan
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-remove-subdirective
                  "mevedel-directive" (directive subdirective))
(declare-function mevedel-directive-request-changed-p
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-set-request "mevedel-directive"
                  (directive request))

;; `mevedel-directive-frame'
(declare-function mevedel-directive-frame-display
                  "mevedel-directive-frame"
                  (directive view-buffer &optional focus))

;; `mevedel-directive-plan'
(declare-function mevedel-directive-plan--planning-prompt
                  "mevedel-directive-plan"
                  (implementation-prompt &optional feedback proposal))

;; `mevedel-directive-source'
(declare-function mevedel--create-directive-in
                  "mevedel-directive-source"
                  (buffer start end &optional bodyless directive-text))
(declare-function mevedel--create-reference-in
                  "mevedel-directive-source" (buffer start end))
(declare-function mevedel--delete-instruction
                  "mevedel-directive-source"
                  (instruction &optional buffer))
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

;; `mevedel-instruction-registry'
(declare-function mevedel--clear-instruction-state
                  "mevedel-instruction-registry" (&optional workspace))
(declare-function mevedel--instruction-activate-buffer
                  "mevedel-instruction-registry" (&optional buffer))
(declare-function mevedel--instruction-activate-workspace
                  "mevedel-instruction-registry" (&optional workspace))
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-instruction-registry" (buffer))
(declare-function mevedel--instruction-id
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-operation-state-key
                  "mevedel-instruction-registry" ())
(declare-function mevedel--instruction-outlinks
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-state
                  "mevedel-instruction-registry" (&optional key))
(declare-function mevedel--instruction-with-id
                  "mevedel-instruction-registry"
                  (target-id &optional workspace))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-instruction-registry"
                  (uuid &optional workspace))
(declare-function mevedel--instruction-workspace-key
                  "mevedel-instruction-registry" (&optional workspace))
(defvar mevedel--instruction-state-key-override)

;; `mevedel-menu'
(declare-function mevedel-menu-open-model-selection
                  "mevedel-menu" (&rest options))

;; `mevedel-models'
(declare-function mevedel-model-current-provider-label
                  "mevedel-models" (&optional buffer))

;; `mevedel-overlay-ui'
(declare-function mevedel--update-instruction-overlay
                  "mevedel-overlay-ui"
                  (instruction &optional update-children))

;; `mevedel-persistence'
(declare-function mevedel--restore-file-instructions
                  "mevedel-persistence" (file &optional message workspace))
(declare-function mevedel--setup-buffer-hooks "mevedel-persistence" (buffer))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff--append-implementation-input
                  "mevedel-plan-handoff" (prompt selection))

;; `mevedel-skills-core'
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-source-file "mevedel-skills-core" (cl-x) t)

;; `mevedel-skills-ui'
(declare-function mevedel-skills--user-visible-skills
                  "mevedel-skills-ui" (session &optional inline-only))

;; `mevedel-structs'
(declare-function mevedel-directive--create "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-p "mevedel-structs" (cl-x))
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning-enabled
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-set-anchor "mevedel-structs"
                  (directive anchor))
(declare-function mevedel-directive-set-planning-enabled
                  "mevedel-structs" (directive enabled))
(declare-function mevedel-directive-set-skills "mevedel-structs"
                  (directive skills))
(declare-function mevedel-directive-set-state "mevedel-structs"
                  (directive state))
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-subdirective-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-copy "mevedel-structs" (subdirective))
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-set-anchor
                  "mevedel-structs" (subdirective anchor))
(declare-function mevedel-subdirective-set-request
                  "mevedel-structs" (subdirective request))
(declare-function mevedel-workspace-add-directive "mevedel-structs"
                  (workspace directive))
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-remove-directive "mevedel-structs"
                  (workspace directive))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-set-directives "mevedel-structs"
                  (workspace directives))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" ())

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view-enter-directive-scope
                  "mevedel-view-composer"
                  (directive action &optional attempt-index workspace))
(defvar mevedel-view--input-marker)

;; `mevedel-view-render'
(declare-function mevedel-view-toggle-section "mevedel-view-render" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

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


(defvar mevedel--default-instruction-priority -99)
(defvar mevedel--highlighted-instruction nil)


(defun mevedel-cycle-instructions-at-point (point)
  "Cycle through instructions at POINT, highlighting them.

This command allows for cycling through overlapping instructions at a
point in the buffer and allows one to have better accuracy when
instructions overlap to the point where no other reasonable option is
available."
  (interactive "d")
  (require 'mevedel-overlay-ui)
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
  (require 'mevedel-directive-source)
  (require 'mevedel-overlay-ui)
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

(defun mevedel-modify-reference-commentary ()
  "Modify the reference commentary under the point."
  (interactive)
  (require 'mevedel-overlay-ui)
  (when-let* ((reference (mevedel--highest-priority-instruction (mevedel--instructions-at (point) 'reference)
                                                                t)))
    (mevedel--read-commentary reference)))

(defun mevedel-delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were
deleted.  Throw a user error if no instructions to delete were found."
  (interactive)
  (require 'mevedel-directive-source)
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
  (require 'mevedel-directive-source)
  (require 'mevedel-instruction-registry)
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
  (require 'mevedel-directive-source)
  (let ((directive (mevedel--topmost-instruction (car (mevedel--instructions-at (point) 'directive))
                                                 'directive)))
    (require 'mevedel-chat)
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
        (require 'mevedel-plan-handoff)
        (setq request-string
              (mevedel-plan-handoff--append-implementation-input
               request-string (list :skills skills))))
      (when (and (eq action 'implement)
                 (mevedel-directive-planning-enabled record))
        (require 'mevedel-directive-plan)
        (setq request-string
              (mevedel-directive-plan--planning-prompt request-string)))
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
  (require 'mevedel-directive-source)
  (require 'mevedel-overlay-ui)
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
  (require 'mevedel-instruction-registry)
  (require 'mevedel-overlay-ui)
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
  (require 'mevedel-overlay-ui)
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


(defun mevedel--stashed-buffer-instructions (buffer)
  "Return stashed instruction data for all instructions in BUFFER.

Each instruction is represented as a plist with :overlay-start,
:overlay-end, :anchor, and :properties keys, capturing the overlay's
position, lightweight re-anchoring context, and semantic properties for
later restoration."
  (require 'mevedel-instruction-registry)
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
  (require 'mevedel-instruction-registry)
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
          (mevedel--set-directive-status directive nil))
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

(defun mevedel--filter-references (query &optional workspace)
  "Return references in WORKSPACE filtered by tag QUERY.

See `mevedel--tag-query-prefix-from-infix' for QUERY format."
  (require 'mevedel-instruction-registry)
  (let ((mevedel--instruction-state-key-override
         (or (and workspace (mevedel--instruction-workspace-key workspace))
             mevedel--instruction-state-key-override))
        (atoms (cl-remove-duplicates
                (cl-remove-if (lambda (elm)
                                (member elm '(not or and nil)))
                              (flatten-tree query)))))
    (if (and (null atoms) mevedel-empty-tag-query-matches-all)
        (mevedel--foreach-instruction instr
          when (mevedel--referencep instr)
          collect instr)
      (mevedel--foreach-instruction instr
        when (and (mevedel--referencep instr)
                  (mevedel--reference-matches-query-p instr query))
        collect instr))))

(defun mevedel--available-tags ()
  "Return a list of all the tags in the loaded references."
  (require 'mevedel-instruction-registry)
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
  (require 'mevedel-instruction-registry)
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
  (let* ((record (or (mevedel--directive-record directive)
                     (error "Directive record not found")))
         (requests
          (cons (mevedel-directive-request record)
                (mapcar #'mevedel-subdirective-request
                        (mevedel-directive-subdirectives record)))))
    (not (cl-some (lambda (request)
                    (not (string-empty-p request)))
                  requests))))

(defun mevedel--create-instruction (type)
  "Create or scale an instruction of the given TYPE within the region.

If a region is selected but partially covers an existing instruction,
then the function will resize it.  See either `mevedel-create-reference'
or `mevedel-create-directive' for details on how the resizing works."
  (require 'mevedel-directive-source)
  (require 'mevedel-overlay-ui)
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


(defun mevedel-get-directive-patch (directive)
  "Get the stored patch for DIRECTIVE, if any.
Returns the unified diff string, or nil if no patch is stored."
  (require 'mevedel-directive-source)
  (when-let* ((record (mevedel--directive-record directive))
              (attempt (car (last (mevedel-directive-attempts record))))
              (patch (mevedel-directive-attempt-patch attempt))
              ((not (string-empty-p patch))))
    patch))

(defun mevedel--parent-instruction (instruction &optional of-type)
  "Return the parent of the given INSTRUCTION overlay.

If OF-TYPE is non-nil, returns the parent with the given type."
  (require 'mevedel-directive-source)
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


(defun mevedel--instructions-congruent-p (a b)
  "Return t if instruction overlays A and B are congruent.
Two overlays are considered congruent if they are in the same buffer and
have identical start and end positions.

A, B: Two overlays to compare for congruence.

Returns: t if A and B are congruent, nil otherwise."
  (and (eq (overlay-buffer a) (overlay-buffer b))
       (= (overlay-start a) (overlay-start b))
       (= (overlay-end a) (overlay-end b))
       (not (and (mevedel--detached-directive-p a)
                 (mevedel--detached-directive-p b)))))

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
  (require 'mevedel-instruction-registry)
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
  (require 'mevedel-instruction-registry)
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


(defun mevedel--directive-truncated-text (directive)
  "Return the truncated directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (mevedel-truncate-directive (mevedel--directive-text directive)))

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
  (require 'mevedel-instruction-registry)
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
  (require 'mevedel-workspace)
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

(defun mevedel--directive-action-context (record workspace)
  "Return RECORD's live directive and validated prompt in WORKSPACE.

This is the shared individual and batch submission eligibility check."
  (require 'mevedel-directive-source)
  (require 'mevedel-instruction-registry)
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
      (user-error "Directive prompt context is unavailable; reattach its source first"))
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
