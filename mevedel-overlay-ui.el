;;; mevedel-overlay-ui.el -- Instruction overlay actions and rendering -*- lexical-binding: t -*-

;;; Commentary:

;; Owns instruction action dispatch, labels, keymaps, styling, and redraw.

;;; Code:

;; `cl-lib' is a runtime dependency, not a compile-time one: these owners
;; call `cl-lib' functions -- `cl-remove', `cl-some', `cl-find' -- and
;; `cl-lib' autoloads none of them.  Loading this module compiled and
;; without the umbrella, as the split-owner contract allows, then hit a
;; void-function; loading it as source only worked because
;; `eval-when-compile' degrades to `progn' in interpreted code.
(require 'cl-lib)

(require 'mevedel-instruction-registry)
(require 'mevedel-utilities)

;; `gptel-request'
(declare-function gptel--model-name "ext:gptel-request" (model))
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer
                  "mevedel-chat" (&optional workspace))
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--patch-buffer
                  "mevedel-chat" (&optional create workspace))
(declare-function mevedel--replace-patch-buffer
                  "mevedel-chat" (patch-content))
(autoload 'mevedel--active-chat-buffer "mevedel-chat")
(autoload 'mevedel--chat-buffer "mevedel-chat")
(autoload 'mevedel--patch-buffer "mevedel-chat")
(autoload 'mevedel--replace-patch-buffer "mevedel-chat")

;; `mevedel-directive'
;; `mevedel-directive-frame'
(declare-function mevedel-directive-frame-display
                  "mevedel-directive-frame"
                  (directive view-buffer &optional focus))
(autoload 'mevedel-directive-frame-display "mevedel-directive-frame")

;; `mevedel-directive-request'


;; `mevedel-directive-source'
(declare-function mevedel--delete-instruction
                  "mevedel-directive-source"
                  (instruction &optional buffer))
(autoload 'mevedel--delete-instruction "mevedel-directive-source")
;; `mevedel-instruction-registry'
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-instruction-registry" (buffer))
(declare-function mevedel--instruction-id
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-inlinks
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-outlinks
                  "mevedel-instruction-registry" (instruction))
(declare-function mevedel--instruction-with-id
                  "mevedel-instruction-registry"
                  (target-id &optional workspace))
(declare-function mevedel-link-instructions
                  "mevedel-instruction-registry" (from-list to-list))
(declare-function mevedel-unlink-instructions
                  "mevedel-instruction-registry" (from-list to-list))

;; `mevedel-menu'
(declare-function mevedel-menu-open-model-selection
                  "mevedel-menu" (&rest options))
(autoload 'mevedel-menu-open-model-selection "mevedel-menu")

;; `mevedel-models'
(declare-function mevedel-model-current-provider-label
                  "mevedel-models" (&optional buffer))
(autoload 'mevedel-model-current-provider-label "mevedel-models")

;; `mevedel-notes'
(declare-function mevedel-note-menu "mevedel-notes" (&optional note))
(declare-function mevedel-notes--render "mevedel-notes" (note))
(autoload 'mevedel-note-menu "mevedel-notes" nil t)
(autoload 'mevedel-notes--render "mevedel-notes")

;; `mevedel-overlay-ui'
(declare-function mevedel--ov-actions-abort
                  "mevedel-overlay-ui" (&optional instructions))

;; `mevedel-overlays'
(declare-function mevedel--bodyless-instruction-p
                  "mevedel-overlays" (instruction))
(declare-function mevedel--child-instructions
                  "mevedel-overlays" (instruction))
(declare-function mevedel--highest-priority-instruction
                  "mevedel-overlays"
                  (instructions &optional return-highlighted))
(declare-function mevedel--instruction-bufferlevel-p
                  "mevedel-overlays" (instruction))
(declare-function mevedel--instruction-type
                  "mevedel-overlays" (instruction))
(declare-function mevedel--instructions-at
                  "mevedel-overlays" (point &optional type))
(declare-function mevedel--parent-instruction
                  "mevedel-overlays" (instruction &optional of-type))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))
(declare-function mevedel-delete-instructions "mevedel-overlays" ())
(autoload 'mevedel--bodyless-instruction-p "mevedel-overlays")
(autoload 'mevedel--child-instructions "mevedel-overlays")
(autoload 'mevedel--highest-priority-instruction "mevedel-overlays")
(autoload 'mevedel--instruction-bufferlevel-p "mevedel-overlays")
(autoload 'mevedel--instruction-type "mevedel-overlays")
(autoload 'mevedel--instructions-at "mevedel-overlays")
(autoload 'mevedel--parent-instruction "mevedel-overlays")
(autoload 'mevedel--topmost-instruction "mevedel-overlays")
(autoload 'mevedel-delete-instructions "mevedel-overlays" nil t)
(defvar mevedel--default-instruction-priority)
(defvar mevedel--highlighted-instruction)
(defvar mevedel-highlighted-instruction-color)
(defvar mevedel-highlighted-instruction-tint-intensity)
(defvar mevedel-instruction-bg-tint-intensity)
(defvar mevedel-instruction-label-tint-intensity)
(defvar mevedel-subinstruction-tint-coefficient)

;; `mevedel-skills-core'
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-source-file "mevedel-skills-core" (cl-x) t)

;; `mevedel-skills-ui'
(declare-function mevedel-skills-user-visible-skills
                  "mevedel-skills-ui" (session &optional inline-only))
(autoload 'mevedel-skills-user-visible-skills "mevedel-skills-ui")

;; `mevedel-structs'
;; `mevedel-utilities'
(declare-function mevedel--fill-label-string
                  "mevedel-utilities"
                  (string &optional prefix-string padding buffer))
(declare-function mevedel--tint
                  "mevedel-utilities"
                  (source-color-name tint-color-name &optional intensity))

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())

(autoload 'mevedel-view--input-marker-position "mevedel-view-composer")


;; `mevedel-view-disclosure'
(declare-function mevedel-view-toggle-section "mevedel-view-disclosure" ())

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(autoload 'mevedel-view--full-rerender "mevedel-view-render")

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(autoload 'mevedel-workspace "mevedel-workspace")

(defconst mevedel--directive-action-labels
  '((implement . "Implement")
    (request-changes . "Request changes")
    (retry . "Retry")
    (plan . "Plan")
    (discuss . "Discuss"))
  "Plain display labels for directive actions.")

(defun mevedel-overlay-ui-directive-action-label (action)
  "Return the display label for directive ACTION."
  (or (alist-get (if (symbolp action) action (intern-soft action))
                 mevedel--directive-action-labels)
      (capitalize (replace-regexp-in-string
                   "[-_]+" " " (format "%s" action)))))


(defun mevedel--ov-actions-read-choice
    (instruction label prompt choices &optional hint-str)
  "Read one of CHOICES for INSTRUCTION, rendering them on its overlay.
LABEL heads the rendered overlay row and PROMPT is the minibuffer
prompt.  HINT-STR is an optional right-aligned annotation ending in a
newline.  The overlay row is restored afterwards; without a displayed
overlay the choices appear only in the minibuffer."
  (let ((before-string (and (overlay-buffer instruction)
                            (overlay-get instruction 'before-string))))
    (unwind-protect
        (progn
          (when (overlay-buffer instruction)
            (overlay-put
             instruction 'before-string
             (concat
              before-string
              (propertize label 'face 'success)
              (when (fboundp #'rmc--add-key-description)
                (mapconcat (lambda (e) (cdr e))
                           (mapcar #'rmc--add-key-description choices)
                           ", "))
              (if hint-str
                  (concat
                   (propertize
                    " " 'display
                    `(space :align-to (- right ,(1+ (length hint-str)))))
                   (propertize hint-str 'face 'success))
                "\n"))))
          (read-multiple-choice prompt choices))
      (when (overlay-buffer instruction)
        (overlay-put instruction 'before-string before-string)))))


(defun mevedel--ov-actions-dispatch (&optional instruction _ci)
  "Open the note menu for INSTRUCTION."
  (interactive (list (mevedel--ov-actions-getov) t))
  (mevedel-note-menu instruction))


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
  (when (eq (get-char-property (point) 'mevedel-instruction-type) 'note)
    (funcall callback
             (substitute-command-keys
              "Note menu: \\[mevedel--ov-actions-dispatch]"))))

(defvar-keymap mevedel-note-actions-map
  :doc "Modified-key menu for notes; plain letters always edit source."
  "C-c C-a" #'mevedel-note-apply-change
  "C-c C-b" #'mevedel-note-revert-change
  "M-m" #'mevedel--ov-actions-dispatch)

(defvar mevedel--actions-maps '(mevedel-note-actions-map))

(defun mevedel--update-instruction-overlay-tree
    (instruction update-children priority _parent)
  "Render INSTRUCTION and optionally UPDATE-CHILDREN at PRIORITY."
  (overlay-put instruction 'priority priority)
  (mevedel-notes--render instruction)
  (when update-children
    (dolist (child (mevedel--child-instructions instruction))
      (mevedel--update-instruction-overlay-tree
       child update-children (1+ priority) instruction))))

(defun mevedel--update-instruction-overlay
    (instruction &optional update-children)
  "Update INSTRUCTION's presentation and optionally UPDATE-CHILDREN."
  (let* ((parent (mevedel--parent-instruction instruction))
         (priority (if parent
                       (1+ (or (overlay-get parent 'priority)
                               mevedel--default-instruction-priority))
                     mevedel--default-instruction-priority)))
    ;; Presentation must not delete authored data when an edit makes two
    ;; ranges equal.  Creation validates conflicts at its own boundary.
    (mevedel--update-instruction-overlay-tree
     instruction update-children priority parent)))


;; Overlay actions adapted from `gptel-rewrite'

(defun mevedel--ov-actions-getov ()
  "Return an instruction overlay at point for action dispatch.

If multiple instruction overlays exist at point, prompt the user to
select one via `completing-read'.  If only one overlay exists, return it
directly.  Return nil if no overlays exist at point."
  (let* ((ovs (mevedel--instructions-at (point)))
         (ov-strings (cl-loop for ov in ovs
                              collect (string-trim
                                       (or (overlay-get ov 'before-string)
                                           (when-let* ((label (overlay-get
                                                              ov 'mevedel-label-overlay)))
                                             (overlay-get label 'after-string))
                                           ""))))
         (ov-map (cl-mapcar #'cons ov-strings ovs))
         selection)
    (if (length> ovs 1)
        (setq selection (completing-read "Choose instruction overlay: " ov-strings))
      (setq selection (car ov-strings)))
    (alist-get selection ov-map nil nil #'equal)))

;; Declare overlay action functions
(eval-and-compile
  (dolist (spec '((link mevedel-link-instructions mevedel-instruction-registry)
                  (unlink mevedel-unlink-instructions mevedel-instruction-registry)
                  (abort mevedel-abort mevedel-chat)
                  (activity mevedel-note-history mevedel-notes)))
    (let ((name (nth 0 spec))
          (target (nth 1 spec))
          (owner (nth 2 spec)))
      (autoload target (symbol-name owner))
      (defalias (intern (format "mevedel--ov-actions-%s" name))
        (lambda (&optional _instructions)
          (interactive)
          (call-interactively target))
        (format "Wrapper around `%s' for overlay dispatch actions." target)))))

(provide 'mevedel-overlay-ui)
;;; mevedel-overlay-ui.el ends here
