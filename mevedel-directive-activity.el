;;; mevedel-directive-activity.el -- Read-only directive inspector -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Projects a migrated note's persisted history plist into a read-only inspector.
;; Historical activity never starts a new model request.

;;; Code:

(require 'mevedel-utilities)

(eval-when-compile (require 'cl-lib))

;; `gptel'
(defvar gptel-response-separator)

;; `mevedel-chat'
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch))

;; `mevedel-instruction-registry'
(declare-function mevedel--instruction-with-uuid
                  "mevedel-instruction-registry" (uuid &optional workspace))

;; `mevedel-structs'
(declare-function mevedel-session-create
                  "mevedel-structs" (name workspace &optional directory))
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (&optional start end))
(autoload 'mevedel-transcript-segments "mevedel-transcript")

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))

;; `mevedel-view'
(declare-function mevedel-view-activate-at-point
                  "mevedel-view" (&optional event))
(autoload 'mevedel-view-activate-at-point "mevedel-view" nil t)
(defvar mevedel-view--display-map)

;; `mevedel-view-composer'
(defvar mevedel-view--input-marker)
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-marker)

;; `mevedel-view-render'
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
(autoload 'mevedel-view-zone-forget "mevedel-view-zone")
(autoload 'mevedel-view-zone-next "mevedel-view-zone" nil t)
(autoload 'mevedel-view-zone-previous "mevedel-view-zone" nil t)
(autoload 'mevedel-view-zone-reconcile "mevedel-view-zone")

;; `org-src'
(declare-function org-escape-code-in-string "org-src" (string))
(autoload 'org-escape-code-in-string "org-src")

(defvar-local mevedel-directive-activity--workspace nil
  "Workspace owning the inspected directive.")

(defvar-local mevedel-directive-activity--directive nil
  "Frozen history plist rendered in the current inspector.")

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
  "v" #'mevedel-directive-activity-view-patch
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
                    :sequence (plist-get attempt :sequence)
                    :entry attempt))
            (plist-get directive :attempts))
    (mapcar (lambda (turn)
              (list :kind 'discussion
                    :sequence
                    (plist-get turn :sequence)
                    :entry turn))
            (plist-get directive :discussion))
    (mapcar (lambda (turn)
              (list :kind 'planning
                    :sequence (plist-get turn :sequence)
                    :entry turn))
            (plist-get directive :planning)))
   (lambda (left right)
     (< (or (plist-get left :sequence) 0)
        (or (plist-get right :sequence) 0)))))

(defun mevedel-directive-activity--attempt-details (attempt)
  "Return durable capture and checkpoint details for ATTEMPT."
  (let* ((checkpoint (plist-get attempt :checkpoint))
         (covered (plist-get attempt :covered-files))
         (gaps (plist-get attempt :gaps))
         (effects (plist-get attempt :untracked-effects)))
    (string-join
     (delq
      nil
      (list
       (format "Capture: %s"
               (capitalize
                (symbol-name
                 (or (plist-get attempt :capture)
                     'incomplete))))
       (format "Captured at: %s"
               (or (plist-get attempt :captured-at)
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
        (id (plist-get directive :id)))
    (with-current-buffer buffer
      (org-mode)
      (setq-local gptel-response-separator "\n\n"
                  mevedel--session
                  (mevedel-session-create "directive-inspector" workspace))
      (dolist (item entries)
        (let* ((kind (plist-get item :kind))
               (entry (plist-get item :entry))
               (sequence (plist-get item :sequence))
               (attempt-p (eq kind 'attempt))
               (planning-p (eq kind 'planning))
               (checkpoint (plist-get entry :checkpoint))
               (turn (plist-get checkpoint :turn))
               (message
                (cond (attempt-p
                       (or (plist-get entry :directive-request)
                           (plist-get directive :request)))
                      (planning-p
                       (or (plist-get entry :message) "Plan directive"))
                      (t
                       (plist-get entry :message))))
               (request (plist-get entry :request))
               (result
                (cond
                 (attempt-p
                    (concat
                     (plist-get entry :result)
                     "\n\n"
                     (mevedel-directive-activity--attempt-details entry)))
                 (t (plist-get entry :result))))
               (outcome (plist-get entry :outcome))
               (action (cond
                        (attempt-p (plist-get entry :action))
                        (planning-p 'plan)
                        (t 'discuss)))
               (start
                (mevedel--format-hook-audit-record
                 (list :type 'directive-turn-boundary :edge 'start
                       :directive-id id
                       :action action
                       :turn turn)))
               (end
                (mevedel--format-hook-audit-record
                 (list :type 'directive-turn-boundary :edge 'end
                       :directive-id id
                       :action action
                       :turn turn :outcome outcome
                       :activity-kind kind :sequence sequence))))
          (insert start)
          (insert (format "%s :%s:\n:PROMPT:\n%s\n:END:\n"
                          (or message "Directive activity")
                          (symbol-name action)
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
        (let* ((segments (mevedel-transcript-segments
                          (point-min) (point-max)))
               (turns (mevedel-view--group-transcript-turns
                       segments data-buffer)))
          (with-current-buffer inspector
            (dolist (turn turns)
              (mevedel-view--render-turn turn data-buffer)))))
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
             'mevedel-view-zone-entry (plist-get item :entry))
            (put-text-property
             pos (or (next-single-property-change
                      pos 'mevedel-view-directive nil limit) limit)
             'mevedel-view-zone-activate
             #'mevedel-directive-activity--require-writable))
          (setq pos (or (next-single-property-change
                         pos 'mevedel-view-directive nil limit)
                        limit)))))))

(defun mevedel-directive-activity-refresh ()
  "Refresh the current read-only directive inspector."
  (interactive)
  (unless (and mevedel-directive-activity--workspace
               (plist-get mevedel-directive-activity--directive :id))
    (mevedel-diagnostic-user-error (mevedel-diagnostic-scope mevedel-directive-activity--workspace) "No directive inspector is associated with this buffer"))
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
  (let* ((inhibit-read-only t)
         (directive mevedel-directive-activity--directive)
         (entries (mevedel-directive-activity--entries directive))
         (attempt (car (last (plist-get directive :attempts))))
         (state
          (cond
           ((and attempt (not (equal (plist-get directive :request)
                                     (plist-get attempt :directive-request))))
            "Ready · request changed")
           (attempt
            (pcase (plist-get attempt :outcome)
              ('success "Implemented") ('aborted "Aborted") (_ "Failed")))
           ((cl-some (lambda (turn)
                       (and (eq (plist-get turn :outcome) 'success)
                            (equal (plist-get directive :request)
                                   (plist-get turn :directive-request))))
                     (plist-get directive :discussion))
            "Discussed")
           (t "Ready")))
         (anchor-state
          (or (plist-get (plist-get directive :anchor) :state)
              'source-missing)))
    (mevedel-view-zone-reconcile
     'directive-inspector (point-min) (point-max)
     (append
      `((:namespace directive-inspector :id request
		    :label-left ,(propertize "** Request" 'face 'outline-2)
		    :body ,(plist-get directive :request) :navigatable t)
        (:namespace directive-inspector :id state
		    :label-left ,(propertize "** State" 'face 'outline-2)
		    :body ,state
		    :navigatable t)
        (:namespace directive-inspector :id planning
		    :label-left ,(propertize "** Planning" 'face 'outline-2)
		    :body "Read-only pre-migration history"
		    :navigatable t)
        (:namespace directive-inspector :id anchor
		    :label-left ,(propertize "** Anchor" 'face 'outline-2)
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

(defun mevedel-directive-activity--require-writable ()
  "Refuse actions that would change frozen pre-migration history."
  (mevedel-diagnostic-user-error
   (mevedel-diagnostic-scope mevedel-directive-activity--workspace)
   "Pre-migration history is read-only; send the note for a new conversation"))


(defun mevedel-directive-activity--attempt-at-point ()
  "Return the implementation attempt at point, or nil."
  (let ((entry (get-text-property (point) 'mevedel-view-zone-entry)))
    (and (memq entry
               (plist-get mevedel-directive-activity--directive :attempts))
         entry)))


(defun mevedel-directive-activity-view-patch ()
  "Open the implementation attempt at point in the patch viewer."
  (interactive)
  (let* ((attempt (mevedel-directive-activity--attempt-at-point))
         (patch (and attempt (plist-get attempt :patch))))
    (unless (and patch (not (string-empty-p patch)))
      (mevedel-diagnostic-user-error (mevedel-diagnostic-scope mevedel-directive-activity--workspace) "Attempt has no captured patch"))
    (mevedel--replace-patch-buffer patch)))

(defun mevedel-directive-activity-rewind ()
  "Refuse rewinding from read-only pre-migration history."
  (interactive)
  (mevedel-directive-activity--require-writable))

(defun mevedel-directive-activity-reattach (_file _start _end)
  "Refuse changing the historical anchor, ignoring _FILE, _START and _END."
  (interactive (list nil nil nil))
  (mevedel-directive-activity--require-writable))

(defun mevedel-directive-activity-archive ()
  "Refuse archiving read-only pre-migration history."
  (interactive)
  (mevedel-directive-activity--require-writable))

(defun mevedel-directive-activity-goto-source ()
  "Visit the inspected directive's attached source."
  (interactive)
  (let ((overlay
         (mevedel--instruction-with-uuid
          (plist-get mevedel-directive-activity--directive :id)
          mevedel-directive-activity--workspace)))
    (unless (overlayp overlay)
      (mevedel-diagnostic-user-error (mevedel-diagnostic-scope mevedel-directive-activity--workspace) "Directive has no live attached source"))
    (pop-to-buffer (overlay-buffer overlay))
    (goto-char (overlay-start overlay))))


;;
;;; Opening and selection

(defun mevedel-open-directive-activity (history workspace)
  "Open frozen HISTORY's read-only inspector in WORKSPACE.
HISTORY is the persisted plist on a migrated note, never a live record."
  (unless (and workspace (listp history) (stringp (plist-get history :id)))
    (mevedel-diagnostic-user-error
     (mevedel-diagnostic-scope workspace) "No historical directive selected"))
  (let ((buffer
         (get-buffer-create
          (format "*mevedel:directive:%s@%s*"
                  (plist-get history :id) (mevedel-workspace-id workspace)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'mevedel-directive-activity-mode)
        (mevedel-directive-activity-mode))
      (setq-local mevedel-directive-activity--workspace workspace
                  mevedel-directive-activity--directive (copy-tree history))
      (mevedel-directive-activity-refresh)
      (goto-char (point-min)))
    (pop-to-buffer-same-window buffer)
    buffer))

(provide 'mevedel-directive-activity)

;;; mevedel-directive-activity.el ends here
