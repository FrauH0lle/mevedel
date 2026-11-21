;;; niwa-overlay-ui.el -- Instruction overlay actions and rendering -*- lexical-binding: t -*-

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

(require 'niwa-instruction-registry)
(require 'niwa-utilities)

;; `gptel-request'
(declare-function gptel--model-name "ext:gptel-request" (model))
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `niwa-chat'
(declare-function niwa--active-chat-buffer
                  "niwa-chat" (&optional workspace))
(declare-function niwa--chat-buffer
                  "niwa-chat"
                  (session-name &optional create workspace working-directory))
(declare-function niwa--patch-buffer
                  "niwa-chat" (&optional create workspace))
(declare-function niwa--replace-patch-buffer
                  "niwa-chat" (patch-content))
(autoload 'niwa--active-chat-buffer "niwa-chat")
(autoload 'niwa--chat-buffer "niwa-chat")
(autoload 'niwa--patch-buffer "niwa-chat")
(autoload 'niwa--replace-patch-buffer "niwa-chat")

;; `niwa-instruction-registry'
(declare-function niwa--instruction-buffer-workspace
                  "niwa-instruction-registry" (buffer))
(declare-function niwa--instruction-id
                  "niwa-instruction-registry" (instruction))
(declare-function niwa--instruction-inlinks
                  "niwa-instruction-registry" (instruction))
(declare-function niwa--instruction-outlinks
                  "niwa-instruction-registry" (instruction))
(declare-function niwa--instruction-with-id
                  "niwa-instruction-registry"
                  (target-id &optional workspace))
(declare-function niwa-link-instructions
                  "niwa-instruction-registry" (from-list to-list))
(declare-function niwa-unlink-instructions
                  "niwa-instruction-registry" (from-list to-list))

;; `niwa-menu'
(declare-function niwa-menu-open-model-selection
                  "niwa-menu" (&rest options))
(autoload 'niwa-menu-open-model-selection "niwa-menu")

;; `niwa-models'
(declare-function niwa-model-current-provider-label
                  "niwa-models" (&optional buffer))
(autoload 'niwa-model-current-provider-label "niwa-models")

;; `niwa-note-source'
(declare-function niwa--delete-instruction
                  "niwa-note-source"
                  (instruction &optional buffer))
(autoload 'niwa--delete-instruction "niwa-note-source")

;; `niwa-notes'
(declare-function niwa-note-menu "niwa-notes" (&optional note))
(declare-function niwa-notes--at-point "niwa-notes" ())
(declare-function niwa-notes--author-name "niwa-notes" (author))
(declare-function niwa-notes--render "niwa-notes" (note))
(autoload 'niwa-note-menu "niwa-notes" nil t)
(autoload 'niwa-notes--at-point "niwa-notes")
(autoload 'niwa-notes--author-name "niwa-notes")
(autoload 'niwa-notes--render "niwa-notes")

;; `niwa-overlays'
(declare-function niwa--child-instructions
                  "niwa-overlays" (instruction))
(declare-function niwa--highest-priority-instruction
                  "niwa-overlays"
                  (instructions &optional return-highlighted))
(declare-function niwa--instruction-type
                  "niwa-overlays" (instruction))
(declare-function niwa--instructions-at
                  "niwa-overlays" (point &optional type))
(declare-function niwa--parent-instruction
                  "niwa-overlays" (instruction &optional of-type))
(declare-function niwa--topmost-instruction
                  "niwa-overlays" (instruction &optional of-type pred))
(declare-function niwa-delete-instructions "niwa-overlays" ())
(autoload 'niwa--child-instructions "niwa-overlays")
(autoload 'niwa--highest-priority-instruction "niwa-overlays")
(autoload 'niwa--instruction-type "niwa-overlays")
(autoload 'niwa--instructions-at "niwa-overlays")
(autoload 'niwa--parent-instruction "niwa-overlays")
(autoload 'niwa--topmost-instruction "niwa-overlays")
(autoload 'niwa-delete-instructions "niwa-overlays" nil t)
(defvar niwa--default-instruction-priority)
(defvar niwa--highlighted-instruction)

;; `niwa-skills-core'
(declare-function niwa-skill-name "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-source-file "niwa-skills-core" (cl-x) t)

;; `niwa-skills-ui'
(declare-function niwa-skills-user-visible-skills
                  "niwa-skills-ui" (session &optional inline-only))
(autoload 'niwa-skills-user-visible-skills "niwa-skills-ui")

;; `niwa-structs'
;; `niwa-utilities'
(declare-function niwa--fill-label-string
                  "niwa-utilities"
                  (string &optional prefix-string padding buffer))
(declare-function niwa--tint
                  "niwa-utilities"
                  (source-color-name tint-color-name &optional intensity))

;; `niwa-view-disclosure'
(declare-function niwa-view-toggle-section "niwa-view-disclosure" ())

;; `niwa-view-render'
(declare-function niwa-view--full-rerender "niwa-view-render" ())
(autoload 'niwa-view--full-rerender "niwa-view-render")

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(autoload 'niwa-workspace "niwa-workspace")

(defun niwa--ov-actions-dispatch (&optional instruction _ci)
  "Open the note menu for INSTRUCTION."
  (interactive (list (niwa--ov-actions-getov) t))
  (niwa-note-menu instruction))

(defvar-keymap niwa-note-actions-map
  :doc "Modified-key menu for notes; plain letters always edit source."
  "C-c C-a" #'niwa-note-apply-change
  "C-c C-b" #'niwa-note-revert-change
  "M-m" #'niwa--ov-actions-dispatch)

(defvar niwa--actions-maps '(niwa-note-actions-map))

(defun niwa--update-instruction-overlay-tree
    (instruction update-children priority _parent)
  "Render INSTRUCTION and optionally UPDATE-CHILDREN at PRIORITY."
  (overlay-put instruction 'priority priority)
  (niwa-notes--render instruction)
  (when update-children
    (dolist (child (niwa--child-instructions instruction))
      (niwa--update-instruction-overlay-tree
       child update-children (1+ priority) instruction))))

(defun niwa--update-instruction-overlay
    (instruction &optional update-children)
  "Update INSTRUCTION's presentation and optionally UPDATE-CHILDREN."
  (let* ((parent (niwa--parent-instruction instruction))
         (priority (if parent
                       (1+ (or (overlay-get parent 'priority)
                               niwa--default-instruction-priority))
                     niwa--default-instruction-priority)))
    ;; Presentation must not delete authored data when an edit makes two
    ;; ranges equal.  Creation validates conflicts at its own boundary.
    (niwa--update-instruction-overlay-tree
     instruction update-children priority parent)))


;; Overlay actions adapted from `gptel-rewrite'

(defun niwa--ov-actions-getov ()
  "Return the note chosen at point for action dispatch."
  (niwa-notes--at-point))

;; Declare overlay action functions
(eval-and-compile
  (dolist (spec '((link niwa-link-instructions niwa-instruction-registry)
                  (unlink niwa-unlink-instructions niwa-instruction-registry)))
    (let ((name (nth 0 spec))
          (target (nth 1 spec))
          (owner (nth 2 spec)))
      (autoload target (symbol-name owner))
      (defalias (intern (format "niwa--ov-actions-%s" name))
        (lambda (&optional _instructions)
          (interactive)
          (call-interactively target))
        (format "Wrapper around `%s' for overlay dispatch actions." target)))))

(provide 'niwa-overlay-ui)
;;; niwa-overlay-ui.el ends here
