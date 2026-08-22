;;; mevedel-overlay-ui.el -- Instruction overlay actions and rendering -*- lexical-binding: t -*-

;;; Commentary:

;; Owns instruction action dispatch, labels, keymaps, styling, and redraw.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-instruction-registry))

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

;; `mevedel-directive-frame'
(declare-function mevedel-directive-frame-display
                  "mevedel-directive-frame"
                  (directive view-buffer &optional focus))

;; `mevedel-directive-request'
(declare-function mevedel--directive-bound-session-buffer
                  "mevedel-directive-request" (record workspace))
(declare-function mevedel--directive-session-buffer
                  "mevedel-directive-request" (directive workspace))

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

;; `mevedel-models'
(declare-function mevedel-model-current-provider-label
                  "mevedel-models" (&optional buffer))

;; `mevedel-overlay-ui'
(declare-function mevedel--ov-actions-abort
                  "mevedel-overlay-ui" (&optional instructions))

;; `mevedel-overlays'
(declare-function mevedel--bodyless-instruction-p
                  "mevedel-overlays" (instruction))
(declare-function mevedel--child-instructions
                  "mevedel-overlays" (instruction))
(declare-function mevedel--commentary-text
                  "mevedel-overlays" (reference))
(declare-function mevedel--commentary-truncated-text
                  "mevedel-overlays" (reference))
(declare-function mevedel--directive-truncated-text
                  "mevedel-overlays" (directive))
(declare-function mevedel--directivep "mevedel-overlays" (instruction))
(declare-function mevedel--highest-priority-instruction
                  "mevedel-overlays"
                  (instructions &optional return-highlighted))
(declare-function mevedel--inherited-tags
                  "mevedel-overlays" (reference))
(declare-function mevedel--instruction-bufferlevel-p
                  "mevedel-overlays" (instruction))
(declare-function mevedel--instruction-type
                  "mevedel-overlays" (instruction))
(declare-function mevedel--instructions-at
                  "mevedel-overlays" (point &optional type))
(declare-function mevedel--instructions-congruent-p
                  "mevedel-overlays" (a b))
(declare-function mevedel--parent-instruction
                  "mevedel-overlays" (instruction &optional of-type))
(declare-function mevedel--reference-tags
                  "mevedel-overlays"
                  (reference &optional include-parent-tags))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))
(declare-function mevedel-delete-instructions "mevedel-overlays" ())
(declare-function mevedel-get-directive-patch
                  "mevedel-overlays" (directive))
(defvar mevedel--default-instruction-priority)
(defvar mevedel--highlighted-instruction)
(defvar mevedel-always-match-untagged-references)
(defvar mevedel-directive-color)
(defvar mevedel-directive-fail-color)
(defvar mevedel-directive-processing-color)
(defvar mevedel-directive-success-color)
(defvar mevedel-empty-tag-query-matches-all)
(defvar mevedel-highlighted-instruction-color)
(defvar mevedel-highlighted-instruction-tint-intensity)
(defvar mevedel-instruction-bg-tint-intensity)
(defvar mevedel-instruction-label-tint-intensity)
(defvar mevedel-reference-color)
(defvar mevedel-subinstruction-tint-coefficient)

;; `mevedel-skills-core'
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-source-file "mevedel-skills-core" (cl-x) t)

;; `mevedel-skills-ui'
(declare-function mevedel-skills-user-visible-skills
                  "mevedel-skills-ui" (session &optional inline-only))

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
(declare-function mevedel-view-enter-directive-scope
                  "mevedel-view-composer"
                  (directive action &optional attempt-index workspace))

;; `mevedel-view-disclosure'
(declare-function mevedel-view-toggle-section "mevedel-view-disclosure" ())

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

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

(defun mevedel--directive-model-values (directive)
  "Return DIRECTIVE's effective model provider, effort, and inheritance."
  (if-let* ((provider
             (overlay-get directive 'mevedel-directive-model-provider)))
      (list provider
            (overlay-get directive 'mevedel-directive-reasoning-effort)
            nil)
    (require 'mevedel-chat)
    (require 'mevedel-models)
    (require 'mevedel-workspace)
    (let ((buffer
           (mevedel--chat-buffer
            "main" t (mevedel-workspace (overlay-buffer directive)))))
      (list
       (mevedel-model-current-provider-label buffer)
       (with-current-buffer buffer
         (and (boundp 'gptel-reasoning-effort)
              gptel-reasoning-effort))
       t))))

(defun mevedel--ov-actions-model (&optional instruction)
  "Select a request-local model for owning directive INSTRUCTION."
  (interactive (list (mevedel--ov-actions-getov)))
  (let ((directive
         (mevedel--topmost-instruction instruction 'directive)))
    (when (memq (mevedel--directive-status directive)
                '(implementing discussing))
      (user-error "Cannot change the model while the directive is processing"))
    (pcase-let ((`(,provider ,effort ,inherited)
                 (mevedel--directive-model-values directive)))
      (require 'mevedel-menu)
      (mevedel-menu-open-model-selection
       :title "Directive model"
       :provider provider
       :effort effort
       :inherited inherited
       :update
       (lambda (new-provider new-effort)
         (overlay-put directive 'mevedel-directive-model-provider
                      new-provider)
         (overlay-put directive 'mevedel-directive-reasoning-effort
                      new-effort)
         (mevedel--update-instruction-overlay directive t))
       :reset
       (lambda ()
         (overlay-put directive 'mevedel-directive-model-provider nil)
         (overlay-put directive 'mevedel-directive-reasoning-effort nil)
         (mevedel--update-instruction-overlay directive t)
         (let ((values (mevedel--directive-model-values directive)))
           (list (car values) (cadr values))))))))

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

(defun mevedel--directive-skills-session (record workspace)
  "Return the session that enumerates skills for RECORD in WORKSPACE.
Prefers RECORD's live bound execution session, then the workspace's
active chat session."
  (require 'mevedel-chat)
  (let ((buffer (or (mevedel--directive-bound-session-buffer
                     record workspace)
                    (mevedel--active-chat-buffer workspace))))
    (or (and (buffer-live-p buffer)
             (buffer-local-value 'mevedel--session buffer))
        (user-error
         "Skill selection needs an open session for this workspace"))))

(defun mevedel--ov-actions-toggle-skill (directive)
  "Toggle one implementation skill on DIRECTIVE's directive record."
  (require 'mevedel-directive)
  (require 'mevedel-skills-ui)
  (let* ((record (mevedel--directive-record directive))
         (workspace
          (mevedel--instruction-buffer-workspace
           (overlay-buffer directive)))
         (session (mevedel--directive-skills-session record workspace))
         (candidates
          (mapcar (lambda (skill)
                    (cons (mevedel-skill-name skill) skill))
                  (mevedel-skills-user-visible-skills session))))
    (unless candidates
      (user-error "No user-invocable skills available"))
    (let* ((choice (completing-read "Toggle implementation skill: "
                                    candidates nil t))
           (skill (cdr (assoc choice candidates)))
           (source-file (mevedel-skill-source-file skill))
           (selected (mevedel-directive-skills record))
           (existing
            (cl-find source-file selected
                     :key (lambda (item) (plist-get item :source-file))
                     :test #'equal)))
      (mevedel-directive-set-skills
       record
       (if existing
           (delete existing selected)
         (append selected
                 (list (list :name (mevedel-skill-name skill)
                             :source-file source-file)))))
      (mevedel--update-instruction-overlay directive t))))

(defun mevedel--ov-actions-settings (&optional instruction)
  "Edit Plan-before-implementation, skills, and model settings for INSTRUCTION."
  (interactive (list (mevedel--ov-actions-getov)))
  (require 'mevedel-directive)
  (let* ((directive (mevedel--topmost-instruction instruction 'directive))
         (record (mevedel--directive-record directive))
         (planning-enabled (mevedel-directive-planning-enabled record))
         (skills-label
          (if-let* ((skills (mevedel-directive-skills record)))
              (mapconcat (lambda (skill) (plist-get skill :name))
                         skills ", ")
            "none"))
         (choice
          (mevedel--ov-actions-read-choice
           directive "SETTINGS: " "Directive settings: "
           `((?p ,(format "plan before implementation: %s"
                          (if planning-enabled "on" "off")))
             (?s ,(format "skills: %s" skills-label))
             (?m ,(if planning-enabled
                      "planning model/effort"
                    "model/effort"))
             (?b "back")))))
    (pcase (car choice)
      (?p
       (mevedel-directive-set-planning-enabled record (not planning-enabled))
       (mevedel--update-instruction-overlay directive t))
      (?s (mevedel--ov-actions-toggle-skill directive))
      (?m (mevedel--ov-actions-model directive))
      (?b (mevedel--ov-actions-dispatch directive t)))))

(defun mevedel--ov-actions-dispatch (&optional instruction ci)
  "Dispatch actions for a successful instruction overlay.

INSTRUCTION is the overlay to dispatch actions for, CI is true for
interactive calls."
  (interactive (list (mevedel--ov-actions-getov) t))
  (require 'mevedel-directive-source)
  (require 'mevedel-instruction-registry)
  (require 'mevedel-overlays)
  (let ((choice)
        (instruction-type (mevedel--instruction-type instruction)))
    (pcase-let* ((request-owner
                      (and (eq instruction-type 'directive)
                           (mevedel--topmost-instruction
                            instruction 'directive)))
                     (record
                      (and request-owner
                           (mevedel--directive-record request-owner)))
                     (actions (and record (mevedel-directive-actions record)))
                     (settings-choice
                      (and request-owner
                           (not (memq 'abort actions))
                           '(?s "settings")))
                     (has-activity-p
                      (and record (mevedel-directive-has-activity-p record)))
                     (activity-choice
                      (and has-activity-p '(?o "activity")))
                     (continue-plan-choice
                      (and record
                           (eq (plist-get (mevedel-directive-plan record)
                                          :status)
                               'draft)
                           (not (plist-get (mevedel-directive-plan record)
                                           :invalidated))
                           (not (plist-get (mevedel-directive-plan record)
                                           :cancelled))
                           '(?P "continue-plan")))
                     (view-changes-choice
                      (and request-owner
                           (mevedel-get-directive-patch request-owner)
                           '(?v "view-changes")))
                     (remove-choice
                      (if has-activity-p
                          '(?A "archive")
                        '(?k "clear")))
                     (choices
                     (pcase instruction-type
                       (`reference `((?t "add-tags") (?r "remove-tags") (?l "link") (?u "unlink") (?c "commentary") (?k "clear")
                                     ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                          '(?e "expand") '(?e "collapse"))))
                       (`directive
                        (cond
                          ;; In flight: always reachable activity, and no
                          ;; clear/archive that would conflict with the
                          ;; running submission.
                          ((memq 'abort actions)
                           `((?a "abort")
                             (?o "activity")
                             ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                  '(?e "expand") '(?e "collapse"))))
                          ((memq 'implement-this actions)
                           `(,@(and activity-choice (list activity-choice))
                             ,@(and continue-plan-choice
                                    (list continue-plan-choice))
                             (?d "continue-discussion")
                             (?i "implement-this") (?m "modify") ,remove-choice
                             ,@(and settings-choice (list settings-choice))
                             ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                  '(?e "expand") '(?e "collapse"))))
                          ((memq 'request-changes actions)
                           `(,@(and activity-choice (list activity-choice))
                             ,@(and continue-plan-choice
                                    (list continue-plan-choice))
                             (?d "discuss-result")
                             ,@(and view-changes-choice
                                    (list view-changes-choice))
                             (?w "show-answer") (?c "request-changes") (?p "preview") (?m "modify") ,remove-choice
                                        ,@(and settings-choice
                                               (list settings-choice))
                                        ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                             '(?e "expand") '(?e "collapse"))))
                          ((memq 'retry actions)
                           `(,@(and activity-choice (list activity-choice))
                             ,@(and continue-plan-choice
                                    (list continue-plan-choice))
                             (?d "discuss-result")
                             (?r "retry") (?m "modify") (?p "preview") ,remove-choice
                                     ,@(and settings-choice
                                            (list settings-choice))
                                     ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                          '(?e "expand") '(?e "collapse"))))
                          (t `(,@(and activity-choice (list activity-choice))
                               ,@(and continue-plan-choice
                                      (list continue-plan-choice))
                               (?d "discuss") (?i "implement") (?t "tags") (?m "modify")
                               (?p "preview") ,remove-choice
                               ,@(and settings-choice (list settings-choice))
                               ,(if (eq (overlay-get instruction 'mevedel-instruction-collapse-p) 'collapse)
                                    '(?e "expand") '(?e "collapse"))))))))
                    (model-values
                     (and request-owner
                          (mevedel--directive-model-values request-owner)))
                    (hint-str
                     (concat
                      "["
                      (if model-values
                          (format "%s · effort %s%s"
                                  (car model-values)
                                  (or (cadr model-values) "default")
                                  (if (caddr model-values) " · session" ""))
                        (gptel--model-name gptel-model))
                      "]\n")))
      (setq choice (mevedel--ov-actions-read-choice
                    instruction "ACTIONS: " "Action: " choices hint-str)))
    (let ((cmd (if (member (cadr choice) '("expand" "collapse"))
                   "cycle"
                 (cadr choice))))
      (if ci
          (funcall-interactively (intern (concat "mevedel--ov-actions-" cmd)) instruction)
        (funcall (intern (concat "mevedel--ov-actions-" cmd)) instruction)))))

(defun mevedel--ov-actions-continue-plan (&optional instruction)
  "Continue INSTRUCTION's retained directive planning conversation."
  (interactive (list (mevedel--ov-actions-getov)))
  (require 'mevedel-view-composer)
  (mevedel-view-enter-directive-scope
   (mevedel--topmost-instruction instruction 'directive) 'plan))

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

(defun mevedel--ov-actions-show-answer (&optional instructions)
  "Navigate to INSTRUCTIONS' latest rendered directive turn."
  (interactive (list (mevedel--ov-actions-getov)))
  (let* ((owner (mevedel--topmost-instruction instructions 'directive))
         (workspace
          (mevedel--instruction-buffer-workspace (overlay-buffer owner)))
         (record (mevedel--directive-record owner))
         (id (mevedel-directive-id record))
         (data-buffer
          (progn
            (require 'mevedel-chat)
            (car (mevedel--directive-session-buffer record workspace))))
         (view-buffer (buffer-local-value 'mevedel--view-buffer data-buffer))
         found)
    (with-current-buffer view-buffer
      (mevedel-view--full-rerender)
      (let ((pos (point-min))
            (limit (mevedel-view--input-marker-position)))
        (while (< pos limit)
          (when-let* ((directive
                       (get-text-property pos 'mevedel-view-directive)))
            (when (equal id (plist-get directive :directive-id))
              (setq found pos)))
          (setq pos (or (next-single-property-change
                         pos 'mevedel-view-directive nil limit)
                        limit))))
      (unless found
        (user-error "Directive answer is not in the live transcript"))
      (goto-char found)
      (when (get-text-property found 'mevedel-view-collapsed)
        (mevedel-view-toggle-section)))
    ;; Navigation only: point stays on the answer, so this does not enter
    ;; directive composer scope the way the follow-up actions do.
    (require 'mevedel-directive-frame)
    (mevedel-directive-frame-display owner view-buffer t)))

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
                (`reference (substitute-command-keys
                             "%s Options: show menu \\[mevedel--ov-actions-dispatch]"))
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
  (require 'mevedel-directive-source)
  (add-hook 'eldoc-documentation-functions
            #'mevedel--ov-actions-help nil 'local)
  (let ((status (and (eq instruction-type 'directive)
                     (mevedel--directive-status instruction))))
    (overlay-put
     instruction 'keymap
     (if (eq instruction-type 'reference)
         mevedel-reference-actions-map
       (pcase status
         ((or 'implementing 'discussing)
          mevedel-directive-processing-actions-map)
         ((or 'implemented 'discussed)
          mevedel-directive-succeeded-actions-map)
         ('aborted mevedel-directive-failed-actions-map)
         ('failed mevedel-directive-failed-actions-map)
         (_ mevedel-directive-actions-map))))
    (overlay-put
     instruction 'help-echo
     (format
      "%s \\[mevedel--ov-actions-dispatch] for options"
      (if (eq instruction-type 'reference)
          "Press"
        (pcase status
          ('implementing "Implementation in progress, press")
          ('discussing "Discussion in progress, press")
          ('implemented "Request implemented, press")
          ('discussed "Request discussed, press")
          ('aborted "Request aborted, press")
          ('failed "Request failed, press")
          (_ "Press")))))))

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
         (parent-bufferlevel-p
          (plist-get presentation :parent-bufferlevel-p))
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
                (format "%s LINKS: "
                        (if (eq instruction-type 'reference)
                            "REFERENCE"
                          "DIRECTIVE"))))))
         (tags-string (tags common-tags)
           (string-join
            (mapcar
             (lambda (tag)
               (propertize
                (symbol-name tag) 'face
                (if (memq tag common-tags)
                    'font-lock-warning-face
                  'font-lock-constant-face)))
             tags)
            " ")))
      (pcase instruction-type
        ('reference
         (setq color mevedel-reference-color)
         (cond
          ((and parent
                (eq (mevedel--instruction-type parent) 'reference)
                (not parent-bufferlevel-p))
           (append-label
            (format "SUBREFERENCE %s"
                    (stylized-id
                     (mevedel--instruction-id instruction)))))
          (bufferlevel-p
           (append-label
            (format "BUFFER REFERENCE %s"
                    (stylized-id
                     (mevedel--instruction-id instruction)))))
          (t
           (append-label
            (format "REFERENCE %s"
                    (stylized-id
                     (mevedel--instruction-id instruction))))))
         (let* ((direct-tags (mevedel--reference-tags instruction))
                (inherited-tags (mevedel--inherited-tags instruction))
                (common-tags
                 (cl-intersection inherited-tags direct-tags))
                (unique-tags
                 (cl-set-difference direct-tags common-tags)))
           (when inherited-tags
             (append-label
              (tags-string
               (sort (append inherited-tags) #'string-lessp)
               common-tags)
              (if common-tags
                  "INHERITED & COMMON TAGS: "
                "INHERITED TAGS: ")))
           (when unique-tags
             (append-label
              (tags-string
               (sort unique-tags #'string-lessp) common-tags)
              (if inherited-tags
                  (if common-tags "UNIQUE TAGS: " "DIRECT TAGS: ")
                "TAGS: "))))
         (append-links)
         (let ((commentary
                (string-trim
                 (or
                  (if (eq
                       (overlay-get
                        instruction 'mevedel-instruction-collapse-p)
                       'collapse)
                      (mevedel--commentary-truncated-text instruction)
                    (mevedel--commentary-text instruction))
                  ""))))
           (unless (string-empty-p commentary)
             (append-label commentary "COMMENTARY: "))))
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
                (prefix
                 (format "%s %s"
                         directive-typename
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
           (if-let* ((query
                      (overlay-get
                       instruction
                       'mevedel-directive-infix-tag-query-string)))
               (append-label query "TAG QUERY: ")
             (append-label
              (cond
               (mevedel-empty-tag-query-matches-all
                "REFERENCES ALL")
               (mevedel-always-match-untagged-references
                "REFERENCES UNTAGGED ONLY")
               (t "REFERENCES NOTHING"))))
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
         (instruction-type (plist-get presentation :type))
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
          (if (and bufferlevel-p
                   (eq instruction-type 'reference))
              default-bg
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
              tint))))
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

(defun mevedel--update-instruction-overlay-tree
    (instruction update-children priority parent)
  "Render INSTRUCTION and optionally UPDATE-CHILDREN.
PRIORITY is the inherited priority and PARENT is the tree parent."
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
  (require 'mevedel-directive-source)
  (require 'mevedel-overlays)
  (let ((conflicting
         (cl-some
          (lambda (other)
            (and
             (not (eq other instruction))
             (mevedel--instructions-congruent-p
              instruction other)))
          (mevedel--instructions-at
           (overlay-start instruction)))))
    (if conflicting
        (mevedel--delete-instruction instruction)
      (let* ((parent
              (mevedel--parent-instruction instruction))
             (priority
              (if parent
                  (1+ (overlay-get parent 'priority))
                mevedel--default-instruction-priority)))
        (when (eq (overlay-get instruction 'mevedel-instruction-type)
                  'directive)
          (mevedel--refresh-directive-anchor instruction))
        (mevedel--update-instruction-overlay-tree
         instruction update-children priority parent)
        (when (mevedel--detached-directive-p instruction)
          (mevedel--order-detached-directives-at
           (overlay-start instruction)))))))


;; Overlay actions adapted from `gptel-rewrite'

(defun mevedel--ov-actions-getov ()
  "Return an instruction overlay at point for action dispatch.

If multiple instruction overlays exist at point, prompt the user to
select one via `completing-read'.  If only one overlay exists, return it
directly.  Return nil if no overlays exist at point."
  (require 'mevedel-overlays)
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

;; Declare overlay action functions
(eval-and-compile
  (dolist (spec '((add-tags mevedel-add-tags mevedel-overlays)
                  (remove-tags mevedel-remove-tags mevedel-overlays)
                  (link mevedel-link-instructions mevedel-instruction-registry)
                  (unlink mevedel-unlink-instructions mevedel-instruction-registry)
                  (commentary mevedel-modify-reference-commentary mevedel-overlays)
                  (abort mevedel-abort mevedel-chat)
                  (activity mevedel-open-directive-activity mevedel-directive-activity)
                  (modify mevedel-modify-directive mevedel-overlays)
                  (discuss mevedel-discuss-directive mevedel)
                  (continue-discussion mevedel-discuss-directive mevedel)
                  (discuss-result mevedel-discuss-directive mevedel)
                  (implement mevedel-implement-directive mevedel)
                  (implement-this mevedel-implement-discussion-directive mevedel)
                  (request-changes mevedel-request-directive-changes mevedel)
                  (retry mevedel-retry-directive mevedel)
                  (tags mevedel-modify-directive-tag-query mevedel-overlays)
                  (preview mevedel-preview-directive-prompt mevedel-overlays)))
    (let ((name (nth 0 spec))
          (target (nth 1 spec))
          (owner (nth 2 spec)))
      (defalias (intern (format "mevedel--ov-actions-%s" name))
        (lambda (&optional _instructions)
          (interactive)
          (require owner)
          (call-interactively target))
        (format "Wrapper around `%s' for overlay dispatch actions." target)))))

(provide 'mevedel-overlay-ui)
;;; mevedel-overlay-ui.el ends here
