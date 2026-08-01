;;; mevedel-directive-activity.el --- Directive activity views -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Renders workspace-owned directives in ephemeral managed view buffers.
;; Activity views read the workspace record directly and never participate in
;; a chat transcript or model request.

;;; Code:

;; `mevedel-overlays'
(declare-function mevedel--directive-record "mevedel-overlays" (directive))
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-overlays" (buffer))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-overlays" (uuid &optional workspace))
(declare-function mevedel--ov-actions-getov "mevedel-overlays" ())
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction type))

;; `mevedel-structs'
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-capture
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-checkpoint
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
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-p "mevedel-structs" (cl-x))
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch))

;; `mevedel-view'
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

(defvar-keymap mevedel-directive-activity-mode-map
  :doc "Keymap for `mevedel-directive-activity-mode'."
  :parent special-mode-map
  "g" #'mevedel-directive-activity-refresh
  "o" #'mevedel-directive-activity-goto-source
  "n" #'mevedel-view-zone-next
  "p" #'mevedel-view-zone-previous
  "RET" #'mevedel-view-activate-at-point)

(define-derived-mode mevedel-directive-activity-mode special-mode
  "MevDirective"
  "Major mode for a workspace directive's activity view."
  (require 'mevedel-view)
  (visual-line-mode +1))


;;
;;; Rendering

(defun mevedel-directive-activity--attempt-fragments (directive)
  "Return chronological activity fragments for DIRECTIVE."
  (let ((index 0))
    (or
     (mapcar
      (lambda (attempt)
        (setq index (1+ index))
        (let* ((outcome (mevedel-directive-attempt-outcome attempt))
               (patch (mevedel-directive-attempt-patch attempt))
               (capture (mevedel-directive-attempt-capture attempt))
               (covered (mevedel-directive-attempt-covered-files attempt))
               (gaps (mevedel-directive-attempt-gaps attempt))
               (checkpoint (mevedel-directive-attempt-checkpoint attempt))
               (patch-p (not (string-empty-p patch)))
               (capture-line
                (cond
                 ((eq capture 'incomplete)
                  (format "Incomplete capture; %d covered; %d gaps"
                          (length covered) (length gaps)))
                 (patch-p "Complete capture; changes captured")
                 (t "Complete capture; no changes"))))
          `(:namespace directive-activity :id ,(list 'attempt index)
            :label-left
            ,(propertize
              (format "ATTEMPT %d · %s" index
                      (upcase (symbol-name outcome)))
              'face 'bold)
            :body
            ,(format "%s\nCheckpoint: %s, turn %s\n\nRequest:\n%s\n\nResult:\n%s"
                     capture-line
                     (plist-get checkpoint :session-id)
                     (plist-get checkpoint :turn)
                     (mevedel-directive-attempt-request attempt)
                     (mevedel-directive-attempt-result attempt))
            :entry ,attempt
            :navigatable t
            ,@(when patch-p
                '(:activate mevedel-directive-activity-view-patch
                  :help-echo "RET: view captured patch")))))
      (mevedel-directive-attempts directive))
     `((:namespace directive-activity :id activity
        :label-left ,(propertize "ACTIVITY" 'face 'bold)
        :body "No activity yet."
        :navigatable t)))))

(defun mevedel-directive-activity-refresh ()
  "Refresh the current directive activity view from its workspace record."
  (interactive)
  (unless (and mevedel-directive-activity--workspace
               (mevedel-directive-p mevedel-directive-activity--directive))
    (user-error "No directive activity is associated with this buffer"))
  (require 'mevedel-view-zone)
  (let* ((directive mevedel-directive-activity--directive)
         (anchor-state
          (or (plist-get (mevedel-directive-anchor directive) :state)
              'source-missing)))
    (mevedel-view-zone-reconcile
     'directive-activity (point-min) (point-max)
     (append
      `((:namespace directive-activity :id request
        :label-left ,(propertize "REQUEST" 'face 'bold)
        :body ,(mevedel-directive-request directive)
        :navigatable t)
       (:namespace directive-activity :id state
        :label-left ,(propertize "STATE" 'face 'bold)
        :body ,(capitalize
                (string-replace
                 "-" " "
                 (symbol-name
                  (or (mevedel-directive-state directive) 'ready))))
        :navigatable t)
       (:namespace directive-activity :id anchor
        :label-left ,(propertize "ANCHOR" 'face 'bold)
        :body ,(capitalize
                (string-replace "-" " " (symbol-name anchor-state)))
        :navigatable t
        ,@(when (eq anchor-state 'attached)
            '(:activate mevedel-directive-activity-goto-source
              :help-echo "RET: visit source anchor"))))
      (mevedel-directive-activity--attempt-fragments directive))))
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
                         (mevedel-workspace-id workspace)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'mevedel-directive-activity-mode)
        (mevedel-directive-activity-mode))
      (setq-local mevedel-directive-activity--workspace workspace
                  mevedel-directive-activity--directive directive)
      (mevedel-directive-activity-refresh)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun mevedel-list-directives (&optional workspace)
  "Choose and open a directive belonging to WORKSPACE."
  (interactive)
  (require 'mevedel-structs)
  (require 'mevedel-workspace)
  (let* ((workspace (or workspace (mevedel-workspace)))
         (directives (and workspace
                          (mevedel-workspace-directives workspace))))
    (unless directives
      (user-error "Workspace has no directives"))
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
           (choice (completing-read "Directive: " choices nil t)))
      (mevedel-open-directive-activity
       (alist-get choice choices nil nil #'equal) workspace))))

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
