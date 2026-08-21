;;; mevedel-directive-activity.el -- Read-only directive inspector -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Projects one workspace-owned directive record into a read-only inspector.
;; All model-bound actions return to the directive's shared session view.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `gptel'
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-chat'
(declare-function mevedel--implement-discussion
                  "mevedel-chat" (directive &optional callback))
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch))
(declare-function mevedel--start-directive-discussion
                  "mevedel-chat" (directive &optional callback))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))
(declare-function mevedel-directive-request-changed-p
                  "mevedel-directive" (directive))

;; `mevedel-directive-source'
(declare-function mevedel--directive-record
                  "mevedel-directive-source" (directive))
(declare-function mevedel--reattach-directive
                  "mevedel-directive-source"
                  (record workspace buffer start end))
(declare-function mevedel--reconcile-directive-sources
                  "mevedel-directive-source" (workspace))
(declare-function mevedel-archive-directive
                  "mevedel-directive-source" (record workspace))

;; `mevedel-instruction-registry'
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-instruction-registry" (buffer))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-instruction-registry" (uuid &optional workspace))

;; `mevedel-overlay-ui'
(declare-function mevedel--ov-actions-getov "mevedel-overlay-ui" ())
(declare-function mevedel--ov-actions-settings
                  "mevedel-overlay-ui" (&optional instruction))

;; `mevedel-overlays'
(declare-function mevedel--directive-action-context
                  "mevedel-overlays" (record workspace))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-rewind-checkpoint
                  "mevedel-session-rewind"
                  (workspace checkpoint &optional buffer))

;; `mevedel-structs'
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-action
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-capture
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-captured-at
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-covered-files
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-gaps
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-untracked-effects
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-attempt-index
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-message
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-p "mevedel-structs" (cl-x))
(declare-function mevedel-directive-planning "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-planning-enabled
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-create
                  "mevedel-structs" (name workspace &optional directory))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (&optional start end))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))

;; `mevedel-view'
(declare-function mevedel-view-activate-at-point
                  "mevedel-view" (&optional event))
(defvar mevedel-view--display-map)

;; `mevedel-view-composer'
(declare-function mevedel-view-enter-directive-scope
                  "mevedel-view-composer"
                  (directive action &optional attempt-index workspace))
(defvar mevedel-view--input-marker)
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-marker)

;; `mevedel-view-render'
(declare-function mevedel-view--collapse-settled-directive-turns
                  "mevedel-view-render" (&optional collapse-newest))
(declare-function mevedel-view--group-transcript-turns
                  "mevedel-view-render" (segments data-buf))
(declare-function mevedel-view--render-turn
                  "mevedel-view-render"
                  (turn data-buf &optional decorate-variants variant-session))
(declare-function mevedel-view-render-initialize
                  "mevedel-view-render" ())

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-forget "mevedel-view-zone" (&optional zone))
(declare-function mevedel-view-zone-next "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-previous
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-reconcile
                  "mevedel-view-zone" (zone start end fragments))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `org-src'
(declare-function org-escape-code-in-string "org-src" (string))

(defvar-local mevedel-directive-activity--workspace nil
  "Workspace owning the inspected directive.")

(defvar-local mevedel-directive-activity--directive nil
  "Workspace directive rendered in the current inspector.")

(defvar-local mevedel-directive-activity--transcript-buffer nil
  "Synthetic record transcript rendered by the shared view machinery.")

(defvar-keymap mevedel-directive-activity-mode-map
  :doc "Keymap for `mevedel-directive-activity-mode'."
  :parent special-mode-map
  "g" #'mevedel-directive-activity-refresh
  "o" #'mevedel-directive-activity-goto-source
  "r" #'mevedel-directive-activity-reattach
  "R" #'mevedel-directive-activity-rewind
  "a" #'mevedel-directive-activity-archive
  "d" #'mevedel-directive-activity-discuss
  "c" #'mevedel-directive-activity-request-changes
  "x" #'mevedel-directive-activity-retry
  "i" #'mevedel-directive-activity-implement-this
  "v" #'mevedel-directive-activity-view-patch
  "s" #'mevedel-directive-activity-settings
  "n" #'mevedel-view-zone-next
  "p" #'mevedel-view-zone-previous
  "RET" #'mevedel-view-activate-at-point)

(define-derived-mode mevedel-directive-activity-mode special-mode
  "MevDirectiveInspector"
  "Read-only inspector for one durable workspace directive."
  (require 'mevedel-view)
  (require 'mevedel-view-composer)
  (require 'mevedel-view-render)
  (require 'mevedel-view-stream)
  (mevedel-view-render-initialize)
  (add-hook 'kill-buffer-hook
            #'mevedel-directive-activity--kill-transcript nil t))


;;
;;; Rendering

(defun mevedel-directive-activity--kill-transcript ()
  "Kill the synthetic transcript owned by the current inspector."
  (when (buffer-live-p mevedel-directive-activity--transcript-buffer)
    (kill-buffer mevedel-directive-activity--transcript-buffer))
  (setq mevedel-directive-activity--transcript-buffer nil))

(defun mevedel-directive-activity--entries (directive)
  "Return DIRECTIVE activity entries in settlement order."
  (sort
   (append
    (mapcar (lambda (attempt)
              (list :kind 'attempt
                    :sequence (mevedel-directive-attempt-sequence attempt)
                    :entry attempt))
            (mevedel-directive-attempts directive))
    (mapcar (lambda (turn)
              (list :kind 'discussion
                    :sequence
                    (mevedel-directive-discussion-turn-sequence turn)
                    :entry turn))
            (mevedel-directive-discussion directive))
    (mapcar (lambda (turn)
              (list :kind 'planning
                    :sequence (plist-get turn :sequence)
                    :entry turn))
            (mevedel-directive-planning directive)))
   (lambda (left right)
     (< (or (plist-get left :sequence) 0)
        (or (plist-get right :sequence) 0)))))

(defun mevedel-directive-activity--attempt-details (attempt)
  "Return durable capture and checkpoint details for ATTEMPT."
  (let* ((checkpoint (mevedel-directive-attempt-checkpoint attempt))
         (covered (mevedel-directive-attempt-covered-files attempt))
         (gaps (mevedel-directive-attempt-gaps attempt))
         (effects (mevedel-directive-attempt-untracked-effects attempt)))
    (string-join
     (delq
      nil
      (list
       (format "Capture: %s"
               (capitalize
                (symbol-name
                 (or (mevedel-directive-attempt-capture attempt)
                     'incomplete))))
       (format "Captured at: %s"
               (or (mevedel-directive-attempt-captured-at attempt)
                   "unknown"))
       (format "Checkpoint: %s turn %s"
               (or (plist-get checkpoint :session-id) "unavailable")
               (or (plist-get checkpoint :turn) "unavailable"))
       (when covered
         (format "Covered files: %s" (string-join covered ", ")))
       (when gaps
         (format "File gaps: %s"
                 (mapconcat
                  (lambda (gap) (format "%s (%s)" (car gap) (cdr gap)))
                  gaps ", ")))
       (when effects
         (format "Untracked effects: %s"
                 (mapconcat
                  (lambda (effect)
                    (format "%s (%s)" (car effect) (cdr effect)))
                  effects ", ")))))
     "\n")))

(defun mevedel-directive-activity--transcript (directive entries)
  "Build and return the shared-renderer transcript for DIRECTIVE ENTRIES."
  (mevedel-directive-activity--kill-transcript)
  (let ((buffer (generate-new-buffer " *mevedel-directive-inspector-data*"))
        (workspace mevedel-directive-activity--workspace)
        (id (mevedel-directive-id directive)))
    (with-current-buffer buffer
      (org-mode)
      (require 'org-src)
      (setq-local gptel-response-separator "\n\n"
                  gptel-prompt-prefix-alist '((org-mode . "*** "))
                  mevedel--session
                  (mevedel-session-create "directive-inspector" workspace))
      (dolist (item entries)
        (let* ((kind (plist-get item :kind))
               (entry (plist-get item :entry))
               (sequence (plist-get item :sequence))
               (attempt-p (eq kind 'attempt))
               (planning-p (eq kind 'planning))
               (checkpoint
                (cond (attempt-p
                       (mevedel-directive-attempt-checkpoint entry))
                      (planning-p (plist-get entry :checkpoint))
                      (t
                       (mevedel-directive-discussion-turn-checkpoint entry))))
               (turn (plist-get checkpoint :turn))
               (message
                (cond (attempt-p
                       (or (mevedel-directive-attempt-directive-request entry)
                           (mevedel-directive-request directive)))
                      (planning-p
                       (or (plist-get entry :message) "Plan directive"))
                      (t
                       (mevedel-directive-discussion-turn-message entry))))
               (request
                (cond (attempt-p (mevedel-directive-attempt-request entry))
                      (planning-p (plist-get entry :request))
                      (t (mevedel-directive-discussion-turn-request entry))))
               (result
                (cond
                 (attempt-p
                    (concat
                     (mevedel-directive-attempt-result entry)
                     "\n\n"
                     (mevedel-directive-activity--attempt-details entry)))
                 (planning-p (plist-get entry :result))
                 (t (mevedel-directive-discussion-turn-result entry))))
               (outcome
                (cond (attempt-p (mevedel-directive-attempt-outcome entry))
                      (planning-p (plist-get entry :outcome))
                      (t (mevedel-directive-discussion-turn-outcome entry))))
               (start
                (mevedel--format-hook-audit-record
                 (list :type 'directive-turn-boundary :edge 'start
                       :directive-id id
                       :action (cond
                                (attempt-p
                                 (mevedel-directive-attempt-action entry))
                                (planning-p 'plan)
                                (t 'discuss))
                       :turn turn)))
               (end
                (mevedel--format-hook-audit-record
                 (list :type 'directive-turn-boundary :edge 'end
                       :directive-id id
                       :action (cond
                                (attempt-p
                                 (mevedel-directive-attempt-action entry))
                                (planning-p 'plan)
                                (t 'discuss))
                       :turn turn :outcome outcome
                       :activity-kind kind :sequence sequence))))
          (insert start)
          (insert (format "*** %s :%s:\n:PROMPT:\n%s\n:END:\n"
                          (or message "Directive activity")
                          (cond
                           (attempt-p
                              (symbol-name
                               (mevedel-directive-attempt-action entry)))
                           (planning-p "plan")
                           (t "discuss"))
                          (org-escape-code-in-string (or request ""))))
          (let ((response-start (point)))
            (insert (or result ""))
            (unless (bolp) (insert "\n"))
            (put-text-property response-start (point) 'gptel 'response))
          (insert end))))
    (setq mevedel-directive-activity--transcript-buffer buffer)
    buffer))

(defun mevedel-directive-activity--render-entries (directive entries)
  "Render DIRECTIVE ENTRIES through the ordinary transcript renderer."
  (when entries
    (let ((data-buffer
           (mevedel-directive-activity--transcript directive entries))
          (inspector (current-buffer)))
      (setq-local mevedel--data-buffer data-buffer)
      (goto-char (point-max))
      (setq-local mevedel-view--status-marker (copy-marker (point) t)
                  mevedel-view--interaction-marker (copy-marker (point) t)
                  mevedel-view--input-marker (copy-marker (point) nil))
      (with-current-buffer data-buffer
        (require 'mevedel-transcript)
        (let* ((segments (mevedel-transcript-segments
                          (point-min) (point-max)))
               (turns (mevedel-view--group-transcript-turns
                       segments data-buffer)))
          (with-current-buffer inspector
            (dolist (turn turns)
              (mevedel-view--render-turn turn data-buffer))
            (mevedel-view--collapse-settled-directive-turns t))))
      (let ((pos (point-min))
            (limit (marker-position mevedel-view--input-marker)))
        (while (< pos limit)
          (when-let* ((metadata
                       (get-text-property pos 'mevedel-view-directive))
                      (item
                       ;; A sequence is unique across the three collections
                       ;; only as long as the record is sound, and the row
                       ;; already knows which kind it renders.
                       (cl-find-if
                        (lambda (candidate)
                          (and (eq (plist-get metadata :activity-kind)
                                   (plist-get candidate :kind))
                               (eql (plist-get metadata :sequence)
                                    (plist-get candidate :sequence))))
                        entries)))
            (put-text-property
             pos
             (or (next-single-property-change
                  pos 'mevedel-view-directive nil limit)
                 limit)
             'mevedel-view-zone-entry (plist-get item :entry)))
          (setq pos (or (next-single-property-change
                         pos 'mevedel-view-directive nil limit)
                        limit)))))))

(defun mevedel-directive-activity-refresh ()
  "Refresh the current read-only directive inspector."
  (interactive)
  (unless (and mevedel-directive-activity--workspace
               (mevedel-directive-p mevedel-directive-activity--directive))
    (user-error "No directive inspector is associated with this buffer"))
  (require 'mevedel-view-zone)
  (let ((inhibit-read-only t))
    (mevedel-directive-activity--kill-transcript)
    (dolist (symbol '(mevedel-view--status-marker
                      mevedel-view--interaction-marker
                      mevedel-view--input-marker))
      (when (and (boundp symbol) (markerp (symbol-value symbol)))
        (set-marker (symbol-value symbol) nil)))
    (setq mevedel-view--status-marker nil
          mevedel-view--interaction-marker nil
          mevedel-view--input-marker nil)
    (erase-buffer)
    (mevedel-view-zone-forget 'directive-inspector))
  (mevedel--reconcile-directive-sources
   mevedel-directive-activity--workspace)
  (let* ((inhibit-read-only t)
         (directive mevedel-directive-activity--directive)
         (entries (mevedel-directive-activity--entries directive))
         (state (or (mevedel-directive-state directive) 'ready))
         (anchor-state
          (or (plist-get (mevedel-directive-anchor directive) :state)
              'source-missing)))
    (mevedel-view-zone-reconcile
     'directive-inspector (point-min) (point-max)
     (append
      `((:namespace directive-inspector :id request
		    :label-left ,(propertize "REQUEST" 'face 'bold)
		    :body ,(mevedel-directive-request directive) :navigatable t)
        (:namespace directive-inspector :id state
		    :label-left ,(propertize "STATE" 'face 'bold)
		    :body ,(if (mevedel-directive-request-changed-p directive)
			       "Ready · request changed"
			     (capitalize
			      (replace-regexp-in-string "-" " "
							(symbol-name state))))
		    :navigatable t)
        (:namespace directive-inspector :id planning
		    :label-left ,(propertize "PLAN" 'face 'bold)
		    :body ,(if (mevedel-directive-planning-enabled directive)
                               "On · s: settings"
                             "Off · s: settings")
		    :activate mevedel-directive-activity-settings
		    :help-echo "RET: edit directive settings"
		    :navigatable t)
        (:namespace directive-inspector :id anchor
		    :label-left ,(propertize "ANCHOR" 'face 'bold)
		    :body ,(capitalize
			    (replace-regexp-in-string "-" " "
						      (symbol-name anchor-state)))
		    :navigatable t
		    ,@(when (eq anchor-state 'attached)
			'(:activate mevedel-directive-activity-goto-source
				    :help-echo "RET: visit source anchor"))))
      (unless entries
        '((:namespace directive-inspector :id activity
		      :label-left "ACTIVITY" :body "No activity yet."
		      :navigatable t)))))
    (mevedel-directive-activity--render-entries directive entries)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil))
  mevedel-directive-activity--directive)


;;
;;; Actions

(defun mevedel-directive-activity--source-directive ()
  "Return a validated source overlay for the inspected directive."
  (plist-get
   (mevedel--directive-action-context
    mevedel-directive-activity--directive
    mevedel-directive-activity--workspace)
   :directive))

(defun mevedel-directive-activity--attempt-at-point ()
  "Return the implementation attempt at point, or nil."
  (let ((entry (get-text-property (point) 'mevedel-view-zone-entry)))
    (and (memq entry
               (mevedel-directive-attempts
                mevedel-directive-activity--directive))
         entry)))

(defun mevedel-directive-activity--attempt-index-at-point ()
  "Return the one-based implementation-attempt index at point."
  (when-let* ((attempt (mevedel-directive-activity--attempt-at-point)))
    (1+ (cl-position
         attempt
         (mevedel-directive-attempts
          mevedel-directive-activity--directive)
         :test #'eq))))

(defun mevedel-directive-activity-discuss ()
  "Enter shared composer discussion scope for this directive."
  (interactive)
  (if (memq 'discuss
            (mevedel-directive-actions
             mevedel-directive-activity--directive))
      (let ((directive (mevedel-directive-activity--source-directive)))
        (mevedel-view-enter-directive-scope
         mevedel-directive-activity--directive 'discuss nil
         mevedel-directive-activity--workspace)
        (mevedel--start-directive-discussion directive))
    (mevedel-view-enter-directive-scope
     mevedel-directive-activity--directive 'discuss
     (mevedel-directive-activity--attempt-index-at-point)
     mevedel-directive-activity--workspace)))

(defun mevedel-directive-activity-request-changes ()
  "Enter shared composer Request changes scope."
  (interactive)
  (mevedel-view-enter-directive-scope
   mevedel-directive-activity--directive 'request-changes nil
   mevedel-directive-activity--workspace))

(defun mevedel-directive-activity-retry ()
  "Enter shared composer Retry scope."
  (interactive)
  (mevedel-view-enter-directive-scope
   mevedel-directive-activity--directive 'retry nil
   mevedel-directive-activity--workspace))

(defun mevedel-directive-activity-implement-this ()
  "Implement the inspected directive using its local discussion."
  (interactive)
  (mevedel--implement-discussion
   (mevedel-directive-activity--source-directive)))

(defun mevedel-directive-activity-settings ()
  "Edit settings for the inspected directive."
  (interactive)
  (mevedel--ov-actions-settings
   (mevedel-directive-activity--source-directive))
  (mevedel-directive-activity-refresh))

(defun mevedel-directive-activity-view-patch ()
  "Open the implementation attempt at point in the patch viewer."
  (interactive)
  (let* ((attempt (mevedel-directive-activity--attempt-at-point))
         (patch (and attempt (mevedel-directive-attempt-patch attempt))))
    (unless (and patch (not (string-empty-p patch)))
      (user-error "Attempt has no captured patch"))
    (mevedel--replace-patch-buffer patch)))

(defun mevedel-directive-activity-rewind ()
  "Rewind the shared execution session before the attempt at point."
  (interactive)
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-fork)
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-rewind)
  (let* ((attempt (mevedel-directive-activity--attempt-at-point))
         (checkpoint
          (and attempt (mevedel-directive-attempt-checkpoint attempt))))
    (unless checkpoint
      (user-error "Point is not on an implementation attempt"))
    (let* ((session-id (plist-get checkpoint :session-id))
           (buffer
            (cl-loop
             for (_ . candidate) in
             (mevedel--workspace-sessions
              mevedel-directive-activity--workspace)
             when (equal
                   session-id
                   (mevedel-session-session-id
                    (buffer-local-value 'mevedel--session candidate)))
             return candidate)))
      (mevedel-session-rewind-rewind-checkpoint
       mevedel-directive-activity--workspace checkpoint buffer))))

(defun mevedel-directive-activity-reattach (file start end)
  "Reattach the inspected directive to FILE from START to END."
  (interactive
   (let* ((anchor
           (mevedel-directive-anchor
            mevedel-directive-activity--directive))
          (file (read-file-name "Reattach to file: " nil
                                (plist-get anchor :file) t))
          (buffer (find-file-noselect file)))
     (list file
           (read-number "Start buffer position: "
                        (with-current-buffer buffer (point-min)))
           (read-number "End buffer position: "
                        (with-current-buffer buffer (point-max))))))
  (mevedel--reattach-directive
   mevedel-directive-activity--directive
   mevedel-directive-activity--workspace
   (find-file-noselect file) start end)
  (mevedel-directive-activity-refresh))

(defun mevedel-directive-activity-archive ()
  "Archive the inspected directive."
  (interactive)
  (mevedel-archive-directive
   mevedel-directive-activity--directive
   mevedel-directive-activity--workspace)
  (mevedel-directive-activity-refresh))

(defun mevedel-directive-activity-goto-source ()
  "Visit the inspected directive's attached source."
  (interactive)
  (let ((overlay
         (mevedel--instruction-with-uuid
          (mevedel-directive-id mevedel-directive-activity--directive)
          mevedel-directive-activity--workspace)))
    (unless (overlayp overlay)
      (user-error "Directive has no live attached source"))
    (pop-to-buffer (overlay-buffer overlay))
    (goto-char (overlay-start overlay))))


;;
;;; Opening and selection

(defun mevedel-open-directive-activity (&optional directive workspace)
  "Open DIRECTIVE's read-only inspector in WORKSPACE."
  (interactive (list (mevedel--ov-actions-getov)))
  (pcase-let* ((`(,workspace ,record)
                (cond
                 ((overlayp directive)
                  (let* ((owner
                          (mevedel--topmost-instruction directive 'directive))
                         (workspace
                          (mevedel--instruction-buffer-workspace
                           (overlay-buffer owner))))
                    (list workspace (mevedel--directive-record owner))))
                 ((and workspace (mevedel-directive-p directive)
                       (memq directive
                             (mevedel-workspace-directives workspace)))
                  (list workspace directive))
                 (t (user-error "No directive selected"))))
               (buffer
                (get-buffer-create
                 (format "*mevedel:directive:%s@%s*"
                         (mevedel-directive-id record)
                         (mevedel-workspace-id workspace)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'mevedel-directive-activity-mode)
        (mevedel-directive-activity-mode))
      (setq-local mevedel-directive-activity--workspace workspace
                  mevedel-directive-activity--directive record)
      (mevedel-directive-activity-refresh)
      (goto-char (point-min)))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun mevedel-directive-activity--choose-directive (workspace archived-p)
  "Choose a directive in WORKSPACE whose archive state is ARCHIVED-P."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (_ (unless archived-p
              (mevedel--reconcile-directive-sources workspace)))
         (directives
          (cl-remove-if-not
           (lambda (directive)
             (eq archived-p
                 (eq 'archived
                     (plist-get (mevedel-directive-anchor directive) :state))))
           (mevedel-workspace-directives workspace))))
    (unless directives
      (user-error (if archived-p
                      "Workspace has no archived directives"
                    "Workspace has no directives")))
    (let* ((choices
            (mapcar
             (lambda (directive)
               (cons (format "%s  %s"
                             (mevedel-directive-id directive)
                             (replace-regexp-in-string
                              "\n" " "
                              (mevedel-directive-request directive)))
                     directive))
             directives))
           (choice (completing-read "Directive: " choices nil t)))
      (mevedel-open-directive-activity
       (alist-get choice choices nil nil #'equal) workspace))))

(defun mevedel-list-directives (&optional workspace)
  "Choose and inspect an active directive in WORKSPACE."
  (interactive)
  (mevedel-directive-activity--choose-directive workspace nil))

(defun mevedel-list-archived-directives (&optional workspace)
  "Choose and inspect an archived directive in WORKSPACE."
  (interactive)
  (mevedel-directive-activity--choose-directive workspace t))

(provide 'mevedel-directive-activity)

;;; mevedel-directive-activity.el ends here
