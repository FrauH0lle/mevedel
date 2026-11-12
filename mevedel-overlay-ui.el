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
(declare-function mevedel-directive-actions
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-has-activity-p
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-request-changed-p
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-set-planning-enabled
                  "mevedel-directive" (directive enabled))
(declare-function mevedel-directive-set-skills
                  "mevedel-directive" (directive skills))
(autoload 'mevedel-directive-actions "mevedel-directive")
(autoload 'mevedel-directive-has-activity-p "mevedel-directive")
(autoload 'mevedel-directive-request-changed-p "mevedel-directive")
(autoload 'mevedel-directive-set-planning-enabled "mevedel-directive")
(autoload 'mevedel-directive-set-skills "mevedel-directive")

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
(declare-function mevedel--detached-directive-p
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-record
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-status
                  "mevedel-directive-source" (directive))
(declare-function mevedel--directive-text
                  "mevedel-directive-source" (directive))
(declare-function mevedel--order-detached-directives-at
                  "mevedel-directive-source" (position))
(declare-function mevedel--refresh-directive-anchor
                  "mevedel-directive-source" (directive))
(declare-function mevedel-archive-directive
                  "mevedel-directive-source" (record workspace))
(autoload 'mevedel--delete-instruction "mevedel-directive-source")
(autoload 'mevedel--detached-directive-p "mevedel-directive-source")
(autoload 'mevedel--directive-record "mevedel-directive-source")
(autoload 'mevedel--directive-status "mevedel-directive-source")
(autoload 'mevedel--directive-text "mevedel-directive-source")
(autoload 'mevedel--order-detached-directives-at "mevedel-directive-source")
(autoload 'mevedel--refresh-directive-anchor "mevedel-directive-source")
(autoload 'mevedel-archive-directive "mevedel-directive-source")

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
(declare-function mevedel--directive-truncated-text
                  "mevedel-overlays" (directive))
(declare-function mevedel--directivep "mevedel-overlays" (instruction))
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
(declare-function mevedel-get-directive-patch
                  "mevedel-overlays" (directive))
(autoload 'mevedel--bodyless-instruction-p "mevedel-overlays")
(autoload 'mevedel--child-instructions "mevedel-overlays")
(autoload 'mevedel--directive-truncated-text "mevedel-overlays")
(autoload 'mevedel--directivep "mevedel-overlays")
(autoload 'mevedel--highest-priority-instruction "mevedel-overlays")
(autoload 'mevedel--instruction-bufferlevel-p "mevedel-overlays")
(autoload 'mevedel--instruction-type "mevedel-overlays")
(autoload 'mevedel--instructions-at "mevedel-overlays")
(autoload 'mevedel--parent-instruction "mevedel-overlays")
(autoload 'mevedel--topmost-instruction "mevedel-overlays")
(autoload 'mevedel-delete-instructions "mevedel-overlays" nil t)
(autoload 'mevedel-get-directive-patch "mevedel-overlays")
(defvar mevedel--default-instruction-priority)
(defvar mevedel--highlighted-instruction)
(defvar mevedel-directive-color)
(defvar mevedel-directive-fail-color)
(defvar mevedel-directive-processing-color)
(defvar mevedel-directive-success-color)
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
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning-enabled
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)

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


(defun mevedel--ov-actions-archive (&optional instruction)
  "Archive INSTRUCTION's workspace directive record."
  (interactive (list (mevedel--ov-actions-getov)))
  (let* ((owner (mevedel--topmost-instruction instruction 'directive))
         (workspace
          (mevedel--instruction-buffer-workspace (overlay-buffer owner)))
         (record (mevedel--directive-record owner)))
    (mevedel-archive-directive record workspace)))

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


(defun mevedel--ov-actions-view-changes (&optional instructions)
  "Toggle the latest patch buffer for INSTRUCTIONS."
  (interactive (list (mevedel--ov-actions-getov)))
  (when-let* ((patch (mevedel-get-directive-patch instructions)))
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
                (`directive
                 (pcase (when-let* ((directive
                                     (mevedel--highest-priority-instruction
                                      (mevedel--instructions-at
                                       (point) 'directive))))
                          (mevedel--directive-status directive))
                   ((or 'implementing 'discussing)
                    (substitute-command-keys "%s Options: abort \\[mevedel--ov-actions-abort] or show menu \\[mevedel--ov-actions-dispatch]"))
                   (_
                    (substitute-command-keys
                     "%s Options: show menu \\[mevedel--ov-actions-dispatch]")))))
              (propertize (gptel--model-name gptel-model) 'face 'mode-line-emphasis)))))

(defvar-keymap mevedel-note-actions-map
  :doc "Modified-key menu for notes; plain letters always edit source."
  "C-c C-a" #'mevedel-note-apply-change
  "C-c C-b" #'mevedel-note-revert-change
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

(defvar mevedel--actions-maps '(mevedel-note-actions-map
                                mevedel-directive-actions-map
                                mevedel-directive-processing-actions-map
                                mevedel-directive-succeeded-actions-map
                                mevedel-directive-failed-actions-map))

(defun mevedel--instruction-directive-color (instruction)
  "Return the status color for directive INSTRUCTION."
  (let ((own-color
         (pcase (mevedel--directive-status instruction)
           ((or 'implementing 'discussing)
            mevedel-directive-processing-color)
           ((or 'implemented 'discussed)
            mevedel-directive-success-color)
           ('aborted mevedel-directive-fail-color)
           ('failed mevedel-directive-fail-color)
           (_ mevedel-directive-color))))
    (if-let* ((parent
               (mevedel--topmost-instruction instruction 'directive)))
        (pcase (mevedel--directive-status parent)
          ((or 'implementing 'discussing)
           mevedel-directive-processing-color)
          ('failed mevedel-directive-fail-color)
          (_ own-color))
      own-color)))

(defun mevedel--instruction-action-setup (instruction instruction-type)
  "Install interaction properties for INSTRUCTION of INSTRUCTION-TYPE."
  (add-hook 'eldoc-documentation-functions
            #'mevedel--ov-actions-help nil 'local)
  (let ((status (and (eq instruction-type 'directive)
                     (mevedel--directive-status instruction))))
    (overlay-put
     instruction 'keymap
     (pcase status
       ((or 'implementing 'discussing)
        mevedel-directive-processing-actions-map)
       ((or 'implemented 'discussed)
        mevedel-directive-succeeded-actions-map)
       ('aborted mevedel-directive-failed-actions-map)
       ('failed mevedel-directive-failed-actions-map)
       (_ mevedel-directive-actions-map)))
    (overlay-put
     instruction 'help-echo
     (format
      "%s \\[mevedel--ov-actions-dispatch] for options"
      (pcase status
        ('implementing "Implementation in progress, press")
        ('discussing "Discussion in progress, press")
        ('implemented "Request implemented, press")
        ('discussed "Request discussed, press")
        ('aborted "Request aborted, press")
        ('failed "Request failed, press")
        (_ "Press"))))))

(defun mevedel--instruction-directive-typename (instruction parent)
  "Return the display type name for directive INSTRUCTION under PARENT."
  (if (and parent (mevedel--directivep parent))
      (pcase (mevedel--directive-status parent)
        ((or 'implementing 'discussing 'failed 'aborted)
         (or (overlay-get instruction 'mevedel-subdirective-typename)
             "HINT"))
        ('implemented "CORRECTION")
        (_ "HINT"))
    "DIRECTIVE"))

(defun mevedel--instruction-label (presentation)
  "Return PRESENTATION with its instruction label and color computed."
  (let* ((instruction (plist-get presentation :instruction))
         (instruction-type (plist-get presentation :type))
         (directive-typename (plist-get presentation :directive-typename))
         (padding (plist-get presentation :padding))
         (bufferlevel-p (plist-get presentation :bufferlevel-p))
         (parent (plist-get presentation :parent))
         (label "")
         color)
    (cl-labels
        ((append-label (content &optional prefix)
           (setq label
                 (concat
                  label
                  (if (string-empty-p label) "" (concat "\n" padding))
                  (mevedel--fill-label-string
                   content (or prefix "") padding
                   (overlay-buffer instruction)))))
         (stylized-id (id)
           (propertize (format "#%d" id)
                       'face 'font-lock-constant-face))
         (filter-link-ids (ids)
           (cl-loop
            for id in ids
            unless
            (let ((target (mevedel--instruction-with-id id)))
              (or (null target)
                  (not (eq (mevedel--instruction-type target)
                           instruction-type))))
            collect id))
         (append-links ()
           (let ((outlinks
                  (filter-link-ids
                   (mevedel--instruction-outlinks instruction)))
                 (inlinks
                  (filter-link-ids
                   (mevedel--instruction-inlinks instruction))))
             (when (or outlinks inlinks)
               (append-label
                (concat
                 (when outlinks
                   (format "TO: %s"
                           (string-join
                            (mapcar #'stylized-id outlinks) ", ")))
                 (when inlinks
                   (format "%sFROM: %s"
                           (if outlinks "\n" "")
                           (string-join
                            (mapcar #'stylized-id inlinks) ", "))))
                "DIRECTIVE LINKS: ")))))
      (pcase instruction-type
        ('directive
         (pcase (mevedel--directive-status instruction)
           ('planning (append-label "PLANNING"))
           ('plan-ready (append-label "PLAN READY"))
           ('plan-accepted (append-label "PLAN ACCEPTED"))
           ('implementing (append-label "IMPLEMENTING"))
           ('discussing (append-label "DISCUSSING"))
           ('implemented (append-label "IMPLEMENTED"))
           ('discussed (append-label "DISCUSSED"))
           ('aborted (append-label "ABORTED"))
           ('failed
            (append-label
             (or (overlay-get instruction 'mevedel-directive-fail-reason)
                 (and parent
                      (overlay-get parent 'mevedel-directive-fail-reason))
                 "request failed")
             "FAILED: "))
           (_
            (when-let* ((record (mevedel--directive-record instruction))
                        ((mevedel-directive-request-changed-p record)))
              (append-label "READY · REQUEST CHANGED"))))
         (setq color
               (mevedel--instruction-directive-color instruction))
         (let* ((directive
                 (string-trim
                  (or
                   (if (eq
                        (overlay-get
                         instruction 'mevedel-instruction-collapse-p)
                        'collapse)
                       (mevedel--directive-truncated-text instruction)
                     (mevedel--directive-text instruction))
                   "")))
                (display-typename
                 (if (and bufferlevel-p
                          (equal directive-typename "DIRECTIVE"))
                     "BUFFER DIRECTIVE"
                   directive-typename))
                (prefix
                 (format "%s %s"
                         display-typename
                         (stylized-id
                          (mevedel--instruction-id instruction)))))
           (append-label
            directive
            (if (string-empty-p directive)
                (concat "EMPTY " prefix)
              (concat prefix ": "))))
         (when-let* ((provider
                      (overlay-get
                       instruction
                       'mevedel-directive-model-provider)))
           (append-label
            (format
             "%s · effort %s"
             provider
             (or
              (overlay-get
               instruction
               'mevedel-directive-reasoning-effort)
              "default"))
            "MODEL: "))
         (when-let* ((owner (mevedel--topmost-instruction
                             instruction 'directive))
                     (record (mevedel--directive-record owner))
                     ((mevedel-directive-planning-enabled record)))
           (append-label "ON" "PLAN: "))
         (when-let* ((owner (mevedel--topmost-instruction
                             instruction 'directive))
                     (record (mevedel--directive-record owner))
                     (skills (mevedel-directive-skills record)))
           (append-label
            (mapconcat (lambda (skill) (plist-get skill :name))
                       skills ", ")
            "SKILLS: "))
         (unless (mevedel--parent-instruction instruction 'directive)
           (append-links))
         (when (mevedel--detached-directive-p instruction)
           (setq label "")
           (append-label
            (format
             "DETACHED · %s · DIRECTIVE %s: %s"
             (upcase
              (symbol-name
               (or (mevedel--directive-status instruction) 'ready)))
             (stylized-id (mevedel--instruction-id instruction))
             (string-trim
              (mevedel--directive-truncated-text instruction)))))))
      (append presentation (list :label label :color color)))))

(defun mevedel--instruction-style (presentation)
  "Apply the computed instruction PRESENTATION."
  (let* ((instruction (plist-get presentation :instruction))
         (label (plist-get presentation :label))
         (color (plist-get presentation :color))
         (padding (plist-get presentation :padding))
         (priority (plist-get presentation :priority))
         (parent (plist-get presentation :parent))
         (bufferlevel-p (plist-get presentation :bufferlevel-p))
         (parent-bufferlevel-p
          (plist-get presentation :parent-bufferlevel-p))
         (default-fg (face-foreground 'default))
         (default-bg (face-background 'default))
         (bg-tint-intensity
          (if (and parent (not parent-bufferlevel-p))
              (* mevedel-subinstruction-tint-coefficient
                 mevedel-instruction-bg-tint-intensity)
            mevedel-instruction-bg-tint-intensity))
         (label-color
          (if bufferlevel-p
              (mevedel--tint
               default-fg color
               mevedel-instruction-label-tint-intensity)
            (let ((tint
                   (mevedel--tint
                    default-fg color
                    mevedel-instruction-label-tint-intensity)))
              (dotimes
                  (_ (- priority
                        mevedel--default-instruction-priority))
                (setq tint
                      (mevedel--tint
                       tint color
                       mevedel-instruction-label-tint-intensity)))
              tint)))
         (bg-color
          (let ((tint
                 (mevedel--tint
                  default-bg color
                  mevedel-instruction-bg-tint-intensity)))
            (dotimes
                (_ (- priority
                      mevedel--default-instruction-priority))
              (setq tint
                    (mevedel--tint
                     tint color bg-tint-intensity)))
            tint)))
    (overlay-put instruction 'mevedel-bg-color bg-color)
    (overlay-put instruction 'mevedel-label-color label-color)
    (overlay-put instruction 'priority priority)
    (when (eq instruction mevedel--highlighted-instruction)
      (setq bg-color
            (mevedel--tint
             default-bg
             mevedel-highlighted-instruction-color
             mevedel-highlighted-instruction-tint-intensity)))
    (let ((instruction-at-eol-p
           (with-current-buffer (overlay-buffer instruction)
             (save-excursion
               (goto-char (overlay-end instruction))
               (eolp)))))
      (cl-labels
          ((colorize (beg end &optional fg bg)
             (unless (= beg end)
               (add-face-text-property
                beg end
                (list :inherit 'default
                      :extend t
                      :foreground (or fg label-color)
                      :background (or bg bg-color))
                t)))
           (colorize-as-parent (beg end)
             (when parent
               (colorize
                beg end
                (overlay-get parent 'mevedel-label-color)
                (overlay-get parent 'mevedel-bg-color)))))
        (overlay-put
         instruction 'before-string
         (with-temp-buffer
           (insert label)
           (if (mevedel--bodyless-instruction-p instruction)
               (unless instruction-at-eol-p
                 (insert "\n"))
             (insert "\n"))
           (goto-char (point-min))
           (end-of-line)
           (unless (eobp)
             (forward-char))
           (colorize (point-min) (point))
           (goto-char (point-min))
           (forward-line)
           (while (not (eobp))
             (beginning-of-line)
             (let ((mark (point)))
               (forward-char (length padding))
               (colorize-as-parent mark (point)))
             (let ((mark (point))
                   advanced)
               (end-of-line)
               (unless (eobp)
                 (setq advanced t)
                 (forward-char))
               (colorize mark (point))
               (unless advanced
                 (forward-line))))
           (unless (mevedel--bodyless-instruction-p instruction)
             (let ((mark (point)))
               (insert padding)
               (colorize-as-parent mark (point))))
           (buffer-string)))))
    (overlay-put
     instruction 'face `(:extend t :background ,bg-color))))

(cl-defun mevedel--update-instruction-overlay-tree
    (instruction update-children priority parent)
  "Render INSTRUCTION and optionally UPDATE-CHILDREN.
PRIORITY is the inherited priority and PARENT is the tree parent."
  (when (eq (overlay-get instruction 'mevedel-instruction-type) 'note)
    (overlay-put instruction 'priority priority)
    (mevedel-notes--render instruction)
    (when update-children
      (dolist (child (mevedel--child-instructions instruction))
        (mevedel--update-instruction-overlay-tree
         child update-children (1+ priority) instruction)))
    (cl-return-from mevedel--update-instruction-overlay-tree instruction))
  (let* ((instruction-type
          (mevedel--instruction-type instruction))
         (padding
          (with-current-buffer (overlay-buffer instruction)
            (save-excursion
              (goto-char (overlay-start instruction))
              (make-string (current-column) ? ))))
         (bufferlevel-p
          (mevedel--instruction-bufferlevel-p instruction))
         (parent-bufferlevel-p
          (and parent
               (mevedel--instruction-bufferlevel-p parent)))
         (priority
          (if bufferlevel-p (1- priority) priority))
         (directive-typename
          (and (eq instruction-type 'directive)
               (mevedel--instruction-directive-typename
                instruction parent)))
         (presentation
          (mevedel--instruction-label
           (list :instruction instruction
                 :type instruction-type
                 :directive-typename directive-typename
                 :padding padding
                 :bufferlevel-p bufferlevel-p
                 :parent parent
                 :parent-bufferlevel-p parent-bufferlevel-p
                 :priority priority))))
    (overlay-put instruction 'mevedel-subdirective-typename
                 (and parent
                      (mevedel--directivep parent)
                      directive-typename))
    (mevedel--instruction-action-setup
     instruction instruction-type)
    (mevedel--instruction-style presentation)
    (when update-children
      (dolist (child
               (mevedel--child-instructions instruction))
        (mevedel--update-instruction-overlay-tree
         child update-children (1+ priority) instruction)))))

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
    (when (eq (overlay-get instruction 'mevedel-instruction-type) 'directive)
      (mevedel--refresh-directive-anchor instruction))
    (mevedel--update-instruction-overlay-tree
     instruction update-children priority parent)
    (when (mevedel--detached-directive-p instruction)
      (mevedel--order-detached-directives-at (overlay-start instruction)))))


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
                  (activity mevedel-note-history mevedel-notes)
                  (modify mevedel-modify-directive mevedel-overlays)))
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
