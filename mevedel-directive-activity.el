;;; mevedel-directive-activity.el -- Directive activity views -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Renders workspace-owned directives in ephemeral managed view buffers.
;; Activity views own no chat transcript or request state; their composer
;; dispatches isolated directive requests through the chat lifecycle.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-chat'
(declare-function mevedel--discuss-directive-turn
                  "mevedel-chat"
                  (directive message &optional attempt-index callback))
(declare-function mevedel--implement-discussion
                  "mevedel-chat" (directive &optional callback))
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch))
(declare-function mevedel--request-directive-changes
                  "mevedel-chat" (directive feedback &optional callback))
(declare-function mevedel--retry-directive
                  "mevedel-chat" (directive guidance &optional callback))
(declare-function mevedel--start-directive-discussion
                  "mevedel-chat" (directive &optional callback))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))

;; `mevedel-overlays'
(declare-function mevedel--directive-action-context
                  "mevedel-overlays" (record workspace))
(declare-function mevedel--directive-record "mevedel-overlays" (directive))
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-overlays" (buffer))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-overlays" (uuid &optional workspace))
(declare-function mevedel--ov-actions-getov "mevedel-overlays" ())
(declare-function mevedel--reattach-directive
                  "mevedel-overlays" (record workspace buffer start end))
(declare-function mevedel--reconcile-directive-sources
                  "mevedel-overlays" (workspace))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction type))
(declare-function mevedel-archive-directive
                  "mevedel-overlays" (record workspace))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-rewind-checkpoint
                  "mevedel-session-persistence"
                  (workspace checkpoint &optional buffer))

;; `mevedel-structs'
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-capture
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-captured-at
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-consumed-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-covered-files
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
(declare-function mevedel-directive-discussion-turn-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-p "mevedel-structs" (cl-x))
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request-changed-p
                  "mevedel-structs" (directive))
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)

;; `mevedel-view'
(autoload 'mevedel-surface-mode "mevedel-view")
(declare-function mevedel-view-activate-at-point "mevedel-view"
                  (&optional event))

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-next "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-previous
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-reconcile
                  "mevedel-view-zone" (zone start end fragments))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

(defvar-local mevedel-directive-activity--workspace nil
  "Workspace whose directive is rendered in the current activity view.")

(defvar-local mevedel-directive-activity--directive nil
  "Workspace-owned directive rendered in the current activity view.")

(defvar-local mevedel-directive-activity--composer-marker nil
  "Marker separating rendered activity from the local composer.")

(defvar-local mevedel-directive-activity--input-marker nil
  "Marker at the first editable character of the local composer.")

(defvar-local mevedel-directive-activity--display-overlay nil
  "Keymap overlay covering the rendered read-only activity area.")

(defvar-local mevedel-directive-activity--selected-attempt-index nil
  "Implementation attempt attached to the next discussion turn.")

(defvar-local mevedel-directive-activity--composer-action 'discuss
  "Action submitted by the local directive activity composer.")

(defvar-keymap mevedel-directive-activity--display-map
  :doc "Keymap active in rendered directive activity."
  "g" #'mevedel-directive-activity-refresh
  "o" #'mevedel-directive-activity-goto-source
  "r" #'mevedel-directive-activity-reattach
  "R" #'mevedel-directive-activity-rewind
  "a" #'mevedel-directive-activity-archive
  "d" #'mevedel-directive-activity-discuss-result
  "n" #'mevedel-view-zone-next
  "p" #'mevedel-view-zone-previous
  "RET" #'mevedel-view-activate-at-point)

(defvar-keymap mevedel-directive-activity-mode-map
  :doc "Keymap for `mevedel-directive-activity-mode'."
  :parent text-mode-map
  "C-c RET" #'mevedel-directive-activity-submit
  "C-c C-i" #'mevedel-directive-activity-implement-this)

(defun mevedel-directive-activity--composer-prompt (action)
  "Return the local composer prompt for ACTION."
  (pcase action
    ('discuss
     (if (and mevedel-directive-activity--directive
              (eq 'discussed
                  (mevedel-directive-state
                   mevedel-directive-activity--directive)))
         "DISCUSSION  C-c RET: discuss · C-c C-i: implement this\n> "
       "DISCUSSION  C-c RET: discuss\n> "))
    ('request-changes
     "REQUEST CHANGES  C-c RET: implement feedback\n> ")
    ('retry
     "RETRY  C-c RET: retry (guidance optional)\n> ")
    (_ (error "Unknown directive activity action: %S" action))))

(defun mevedel-directive-activity--default-action (directive)
  "Return the composer action implied by DIRECTIVE state."
  (pcase (mevedel-directive-state directive)
    ('discussed 'discuss)
    ('implemented 'request-changes)
    ((or 'failed 'aborted) 'retry)
    (_ nil)))

(defun mevedel-directive-activity-set-action (action)
  "Set the local composer to ACTION while preserving its draft.
Remove the editable composer when ACTION is nil."
  (let* ((draft
          (if (and (markerp mevedel-directive-activity--input-marker)
                   (marker-buffer mevedel-directive-activity--input-marker))
              (buffer-substring-no-properties
               mevedel-directive-activity--input-marker (point-max))
            ""))
         (offset (and (markerp mevedel-directive-activity--input-marker)
                      (>= (point) mevedel-directive-activity--input-marker)
                      (- (point) mevedel-directive-activity--input-marker)))
         (start (marker-position mevedel-directive-activity--composer-marker))
         (inhibit-read-only t))
    (delete-region start (point-max))
    (goto-char start)
    (set-marker mevedel-directive-activity--composer-marker start)
    (when (markerp mevedel-directive-activity--input-marker)
      (set-marker mevedel-directive-activity--input-marker nil))
    (setq mevedel-directive-activity--input-marker nil
          mevedel-directive-activity--composer-action action)
    (when action
      (let ((prompt-start (point)))
        (insert (mevedel-directive-activity--composer-prompt action))
        (add-text-properties
         prompt-start (point)
         '(read-only t
           front-sticky (read-only)
           rear-nonsticky (read-only font-lock-face))))
      (setq mevedel-directive-activity--input-marker
            (copy-marker (point) nil))
      (insert draft)
      (when offset
        (goto-char (+ mevedel-directive-activity--input-marker
                      (min offset (length draft))))))))

(define-derived-mode mevedel-directive-activity-mode mevedel-surface-mode
  "MevDirective"
  "Major mode for a workspace directive's activity view."
  (erase-buffer)
  (setq mevedel-directive-activity--display-overlay
        (make-overlay (point-min) (point-min)))
  (overlay-put mevedel-directive-activity--display-overlay
               'keymap mevedel-directive-activity--display-map)
  (setq mevedel-directive-activity--composer-marker
        (copy-marker (point) t))
  (mevedel-directive-activity-set-action nil))


;;
;;; Rendering

(defun mevedel-directive-activity--attempt-fragments (directive)
  "Return chronological activity fragments for DIRECTIVE."
  (let ((index 0))
    (mapcar
      (lambda (attempt)
        (setq index (1+ index))
        (let* ((outcome (mevedel-directive-attempt-outcome attempt))
               (patch (mevedel-directive-attempt-patch attempt))
               (capture (mevedel-directive-attempt-capture attempt))
               (covered (mevedel-directive-attempt-covered-files attempt))
               (gaps (mevedel-directive-attempt-gaps attempt))
               (consumed
                (mevedel-directive-attempt-consumed-subdirectives attempt))
               (checkpoint (mevedel-directive-attempt-checkpoint attempt))
               (patch-p (not (string-empty-p patch)))
               (rewind-p
                (or (eq capture 'incomplete) covered gaps patch-p))
               (capture-line
                (cond
                 ((eq capture 'incomplete)
                  (format "Incomplete capture; %d covered; %d gaps"
                          (length covered) (length gaps)))
                 (patch-p "Complete capture; changes captured")
                 (t "Complete capture; no changes"))))
          `(:namespace directive-activity :id ,(list 'attempt index)
            :sequence ,(mevedel-directive-attempt-sequence attempt)
            :label-left
            ,(propertize
              (format "ATTEMPT %d · %s" index
                      (upcase (symbol-name outcome)))
              'face 'bold)
            :body
            ,(format "%s\nCaptured: %s\nCheckpoint: %s, turn %s%s%s\n\nRequest:\n%s\n\nResult:\n%s"
                     capture-line
                     (mevedel-directive-attempt-captured-at attempt)
                     (plist-get checkpoint :session-id)
                     (plist-get checkpoint :turn)
                     (if rewind-p
                         "\nRewind: R restores the complete session suffix"
                       "")
                     (if consumed
                         (format "\nConsumed details:\n%s"
                                 (mapconcat
                                  (lambda (subdirective)
                                    (concat "- "
                                            (mevedel-subdirective-request
                                             subdirective)))
                                  consumed "\n"))
                       "")
                     (mevedel-directive-attempt-request attempt)
                     (mevedel-directive-attempt-result attempt))
            :entry ,attempt
            :navigatable t
            ,@(when patch-p
                '(:activate mevedel-directive-activity-view-patch
                  :help-echo "RET: view captured patch")))))
      (mevedel-directive-attempts directive))))

(defun mevedel-directive-activity--discussion-fragments (directive)
  "Return chronological local discussion fragments for DIRECTIVE."
  (let ((index 0))
    (mapcar
     (lambda (turn)
       (setq index (1+ index))
       (let ((attempt-index
              (mevedel-directive-discussion-turn-attempt-index turn))
             (checkpoint
              (mevedel-directive-discussion-turn-checkpoint turn)))
         `(:namespace directive-activity :id ,(list 'discussion index)
           :sequence ,(mevedel-directive-discussion-turn-sequence turn)
           :label-left
           ,(propertize
             (format "DISCUSSION %d · %s" index
                     (upcase
                      (symbol-name
                       (mevedel-directive-discussion-turn-outcome turn))))
             'face 'bold)
           :body
           ,(format "%sCheckpoint: %s, turn %s\n\nYou:\n%s\n\nAssistant:\n%s"
                    (if attempt-index
                        (format "Attempt %d attached\n" attempt-index)
                      "")
                    (plist-get checkpoint :session-id)
                    (plist-get checkpoint :turn)
                    (mevedel-directive-discussion-turn-message turn)
                    (mevedel-directive-discussion-turn-result turn))
           :entry ,turn
           :navigatable t)))
     (mevedel-directive-discussion directive))))

(defun mevedel-directive-activity--activity-fragments (directive)
  "Return all DIRECTIVE activity fragments in settlement order."
  (or
   (sort
    (append
     (mevedel-directive-activity--attempt-fragments directive)
     (mevedel-directive-activity--discussion-fragments directive))
    (lambda (left right)
      (< (or (plist-get left :sequence) 0)
         (or (plist-get right :sequence) 0))))
   `((:namespace directive-activity :id activity
      :label-left ,(propertize "ACTIVITY" 'face 'bold)
      :body "No activity yet."
      :navigatable t))))

(defun mevedel-directive-activity--input-text ()
  "Return the exact editable directive discussion draft."
  (if (and (markerp mevedel-directive-activity--input-marker)
           (marker-buffer mevedel-directive-activity--input-marker))
      (buffer-substring-no-properties
       mevedel-directive-activity--input-marker (point-max))
    ""))

(defun mevedel-directive-activity-refresh ()
  "Refresh the current directive activity view from its workspace record."
  (interactive)
  (unless (and mevedel-directive-activity--workspace
               (mevedel-directive-p mevedel-directive-activity--directive))
    (user-error "No directive activity is associated with this buffer"))
  (require 'mevedel-view-zone)
  (mevedel--reconcile-directive-sources
   mevedel-directive-activity--workspace)
  (let* ((input-offset
          (and (markerp mevedel-directive-activity--input-marker)
               (>= (point) mevedel-directive-activity--input-marker)
               (- (point) mevedel-directive-activity--input-marker)))
         (directive mevedel-directive-activity--directive)
         (anchor-state
          (or (plist-get (mevedel-directive-anchor directive) :state)
              'source-missing)))
    (mevedel-view-zone-reconcile
     'directive-activity
     (point-min) mevedel-directive-activity--composer-marker
     (append
      `((:namespace directive-activity :id request
        :label-left ,(propertize "REQUEST" 'face 'bold)
        :body ,(mevedel-directive-request directive)
        :navigatable t)
       (:namespace directive-activity :id state
        :label-left ,(propertize "STATE" 'face 'bold)
        :body ,(if (mevedel-directive-request-changed-p directive)
                   "Ready · request changed"
                 (capitalize
                  (string-replace
                   "-" " "
                   (symbol-name
                    (or (mevedel-directive-state directive) 'ready)))))
        :navigatable t)
       (:namespace directive-activity :id anchor
        :label-left ,(propertize "ANCHOR" 'face 'bold)
        :body ,(capitalize
                (string-replace "-" " " (symbol-name anchor-state)))
        :navigatable t
        ,@(when (eq anchor-state 'attached)
            '(:activate mevedel-directive-activity-goto-source
              :help-echo "RET: visit source anchor")))
       ,@(when (null (mevedel-directive-state directive))
           '((:namespace directive-activity :id discuss-action
              :label-left "ACTION"
              :body "Discuss"
              :navigatable t
              :activate mevedel-directive-activity-start-discussion
              :help-echo "RET: discuss directive"))))
      (mevedel-directive-activity--activity-fragments directive)))
    (move-overlay mevedel-directive-activity--display-overlay
                  (point-min) mevedel-directive-activity--composer-marker)
    (when input-offset
      (goto-char
       (+ mevedel-directive-activity--input-marker
          (min input-offset
               (length (mevedel-directive-activity--input-text)))))))
  (set-buffer-modified-p nil)
  mevedel-directive-activity--directive)

(defun mevedel-directive-activity-view-patch ()
  "Project the implementation attempt at point into the patch viewer."
  (interactive)
  (let* ((attempt
          (or (get-text-property (point) 'mevedel-directive-attempt)
              (get-text-property (point) 'mevedel-view-zone-entry)))
         (patch (and attempt (mevedel-directive-attempt-patch attempt))))
    (unless (and patch (not (string-empty-p patch)))
      (user-error "Attempt has no captured patch"))
    (require 'mevedel-chat)
    (mevedel--replace-patch-buffer patch)))

(defun mevedel-directive-activity-rewind ()
  "Rewind the execution session to before the implementation at point."
  (interactive)
  (let* ((attempt (get-text-property (point) 'mevedel-view-zone-entry))
         (checkpoint
          (and attempt (mevedel-directive-attempt-checkpoint attempt))))
    (unless (and attempt
                 checkpoint
                 (or (eq 'incomplete
                         (mevedel-directive-attempt-capture attempt))
                     (mevedel-directive-attempt-covered-files attempt)
                     (mevedel-directive-attempt-gaps attempt)
                     (not (string-empty-p
                           (or (mevedel-directive-attempt-patch attempt)
                               "")))))
      (user-error "No effectful implementation attempt at point"))
    (require 'mevedel-chat)
    (require 'mevedel-session-persistence)
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
      (prog1
          (mevedel-session-persistence-rewind-checkpoint
           mevedel-directive-activity--workspace checkpoint buffer)
        (mevedel-directive-activity-refresh)))))

(defun mevedel-directive-activity--source-directive ()
  "Return the current activity directive after validating its prompt context."
  (plist-get
   (mevedel--directive-action-context
    mevedel-directive-activity--directive
    mevedel-directive-activity--workspace)
   :directive))

(defun mevedel-directive-activity-start-discussion ()
  "Submit the current Ready directive as its initial discussion turn."
  (interactive)
  (let ((buffer (current-buffer))
        (directive (mevedel-directive-activity--source-directive)))
    (mevedel-directive-activity-set-action nil)
    (condition-case err
        (prog1
            (mevedel--start-directive-discussion
             directive
             (lambda (_err _fsm)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (mevedel-directive-activity-set-action
                    (mevedel-directive-activity--default-action
                     mevedel-directive-activity--directive))
                   (mevedel-directive-activity-refresh)))))
          (mevedel-directive-activity-refresh))
      (error
       (mevedel-directive-activity-refresh)
       (signal (car err) (cdr err))))))

(defun mevedel-directive-activity-submit ()
  "Submit the local composer using its selected directive ACTION."
  (interactive)
  (let* ((buffer (current-buffer))
         (message (mevedel-directive-activity--input-text))
         (attempt-index
          mevedel-directive-activity--selected-attempt-index)
         (directive (mevedel-directive-activity--source-directive))
         (callback
          (lambda (_err _fsm)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (mevedel-directive-activity-refresh)
                (mevedel-directive-activity-set-action
                 (mevedel-directive-activity--default-action
                  mevedel-directive-activity--directive))))))
         (fsm
          (pcase mevedel-directive-activity--composer-action
            ('discuss
             (mevedel--discuss-directive-turn
              directive message attempt-index callback))
            ('request-changes
             (mevedel--request-directive-changes
              directive message callback))
            ('retry
             (mevedel--retry-directive directive message callback))
            (_
             (error "Unknown directive activity action: %S"
                    mevedel-directive-activity--composer-action)))))
    (when (and (markerp mevedel-directive-activity--input-marker)
               (marker-buffer mevedel-directive-activity--input-marker))
      (let ((inhibit-read-only t))
        (delete-region mevedel-directive-activity--input-marker (point-max))))
    (setq mevedel-directive-activity--selected-attempt-index nil)
    (mevedel-directive-activity-refresh)
    (when mevedel-directive-activity--input-marker
      (goto-char mevedel-directive-activity--input-marker))
    fsm))

(defun mevedel-directive-activity-discuss-result ()
  "Attach the implementation attempt at point to the next discussion turn."
  (interactive)
  (let* ((attempt (get-text-property (point) 'mevedel-view-zone-entry))
         (index
          (and attempt
               (cl-position
                attempt
                (mevedel-directive-attempts
                 mevedel-directive-activity--directive)
                :test #'eq))))
    (unless index
      (user-error "No implementation attempt at point"))
    (setq mevedel-directive-activity--selected-attempt-index (1+ index))
    (mevedel-directive-activity-set-action 'discuss)
    (goto-char mevedel-directive-activity--input-marker)
    (message "mevedel: next discussion turn includes attempt %d" (1+ index))))

(defun mevedel-directive-activity-implement-this ()
  "Implement the current directive using its complete local discussion."
  (interactive)
  (unless (eq mevedel-directive-activity--composer-action 'discuss)
    (user-error "Implement this is available from Discussion"))
  (unless
      (string-empty-p
       (string-trim (mevedel-directive-activity--input-text)))
    (user-error "Send or clear the discussion draft first"))
  (let ((buffer (current-buffer)))
    (mevedel--implement-discussion
     (mevedel-directive-activity--source-directive)
     (lambda (_err _fsm)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (mevedel-directive-activity-refresh)))))))


;;
;;; Opening and navigation

(defun mevedel-open-directive-activity (&optional directive workspace)
  "Open DIRECTIVE's activity in WORKSPACE.

Interactively, use the source directive at point.  Programmatic callers may
pass a workspace-owned directive record together with WORKSPACE."
  (interactive (list (mevedel--ov-actions-getov)))
  (require 'mevedel-overlays)
  (require 'mevedel-structs)
  (pcase-let* ((`(,workspace ,directive)
                (cond
                 ((overlayp directive)
                  (let* ((owner
                          (mevedel--topmost-instruction
                           directive 'directive))
                         (workspace
                          (mevedel--instruction-buffer-workspace
                           (overlay-buffer owner)))
                         (record (mevedel--directive-record owner)))
                    (unless (and workspace record)
                      (user-error "Directive has no workspace activity"))
                    (list workspace record)))
                 ((and workspace (mevedel-directive-p directive))
                  (unless (memq directive
                                (mevedel-workspace-directives workspace))
                    (user-error "Directive does not belong to this workspace"))
                  (list workspace directive))
                 (t
                  (user-error "No directive selected"))))
               (buffer
                (get-buffer-create
                 (format "*mevedel:directive:%s@%s*"
                         (mevedel-directive-id directive)
                         (mevedel-workspace-id workspace))))
               (new-p
                (not (with-current-buffer buffer
                       (derived-mode-p 'mevedel-directive-activity-mode)))))
    (with-current-buffer buffer
      (when new-p
        (mevedel-directive-activity-mode))
      (setq-local mevedel-directive-activity--workspace workspace
                  mevedel-directive-activity--directive directive)
      (when new-p
        (mevedel-directive-activity-set-action
         (mevedel-directive-activity--default-action directive)))
      (mevedel-directive-activity-refresh)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun mevedel-directive-activity--choose-directive (workspace archived-p)
  "Choose a directive in WORKSPACE whose archive state is ARCHIVED-P."
  (require 'mevedel-structs)
  (require 'mevedel-workspace)
  (let* ((workspace (or workspace (mevedel-workspace)))
         (_ (unless archived-p
              (mevedel--reconcile-directive-sources workspace)))
         (directives
          (and workspace
               (cl-remove-if-not
                (lambda (directive)
                  (eq archived-p
                      (eq 'archived
                          (plist-get (mevedel-directive-anchor directive)
                                     :state))))
                (mevedel-workspace-directives workspace)))))
    (unless directives
      (user-error (if archived-p
                      "Workspace has no archived directives"
                    "Workspace has no directives")))
    (let* ((choices
            (mapcar
             (lambda (directive)
               (cons (format "%s  %s"
                             (mevedel-directive-id directive)
                             (string-replace
                              "\n" " "
                              (mevedel-directive-request directive)))
                     directive))
             directives))
           (choice (completing-read
                    (if archived-p "Archived directive: " "Directive: ")
                    choices nil t)))
      (mevedel-open-directive-activity
       (alist-get choice choices nil nil #'equal) workspace))))

(defun mevedel-list-directives (&optional workspace)
  "Choose and open a directive belonging to WORKSPACE."
  (interactive)
  (mevedel-directive-activity--choose-directive workspace nil))

(defun mevedel-list-archived-directives (&optional workspace)
  "Choose and inspect an archived directive belonging to WORKSPACE."
  (interactive)
  (mevedel-directive-activity--choose-directive workspace t))

(defun mevedel-directive-activity-reattach (file start end)
  "Reattach the current Source missing directive to FILE from START to END."
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
  (let ((buffer (find-file-noselect file)))
    (mevedel--reattach-directive
     mevedel-directive-activity--directive
     mevedel-directive-activity--workspace buffer start end)
    (mevedel-directive-activity-refresh)))

(defun mevedel-directive-activity-archive ()
  "Archive the current directive and retain its activity."
  (interactive)
  (mevedel-archive-directive
   mevedel-directive-activity--directive
   mevedel-directive-activity--workspace)
  (mevedel-directive-activity-refresh))

(defun mevedel-directive-activity-goto-source ()
  "Visit the live attached source for the current directive."
  (interactive)
  (unless (and mevedel-directive-activity--workspace
               (mevedel-directive-p mevedel-directive-activity--directive))
    (user-error "No directive activity is associated with this buffer"))
  (require 'mevedel-overlays)
  (let ((anchor
         (mevedel--instruction-with-uuid
          (mevedel-directive-id mevedel-directive-activity--directive)
          mevedel-directive-activity--workspace)))
    (unless (and anchor
                 (eq (plist-get
                      (mevedel-directive-anchor
                       mevedel-directive-activity--directive)
                      :state)
                     'attached))
      (user-error "Directive has no live attached source"))
    (pop-to-buffer (overlay-buffer anchor))
    (goto-char (overlay-start anchor))
    anchor))

(provide 'mevedel-directive-activity)
;;; mevedel-directive-activity.el ends here
