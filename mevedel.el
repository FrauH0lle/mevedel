;;; mevedel.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 daedsidog
;; Copyright (C) 2025- FrauH0lle

;; Author: FrauH0lle
;; Version: 0.5.0
;; Keywords: convenience, tools, llm, gptel, macher
;; Package-Requires: ((emacs "30.1") (macher "0.3.0"))
;; URL: https://github.com/FrauH0lle/mevedel

;; SPDX-License-Identifier: GPL-3.0-or-later
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'macher)

(require 'mevedel-instructions)
(require 'mevedel-restorer)

(defgroup mevedel nil
  "Customization group for Evedel."
  :group 'tools)

(defcustom mevedel-show-patch-buffer nil
  "Control if the `macher-patch-buffer' should be shown automactically.

If non-nil, the patch buffer will automatically be displayed after a
query completes."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-auto-apply-patches nil
  "Control if patches should be applied automactically.

If non-nil, patches will be applied automatically."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-show-action-buffer t
  "Control if the `macher-action-buffer' should be shown automactically.

If non-nil, the action buffer will automatically be displayed."
  :type 'boolean
  :group 'mevedel)

;;;###autoload
(defun mevedel-version (&optional here message)
  "Return the current version of mevedel.

Interactively, or when MESSAGE is non-nil, show it in echo area. With
prefix argument, or when HERE is non-nil, insert it at point."
  (interactive (list (or current-prefix-arg 'interactive)))
  (let ((version "v0.3.0"))
    (cond
     ((or message (called-interactively-p 'any)) (message "mevedel %s" version))
     (here (insert (format "mevedel %s" version)))
     (t version))))

(defun mevedel--action-from-directive (transform preset directive)
  ;; Prompt to save any unsaved buffers.
  (save-some-buffers nil (lambda () (and (buffer-file-name) (buffer-modified-p))))
  `(:prompt ,(funcall transform (mevedel--directive-llm-prompt directive))
    :preset ,preset
    :summary ,(overlay-get directive 'mevedel-directive)))

(defun mevedel--implement-directive-prompt (content)
  "Generate an implementation prompt for CONTENT in the current buffer."
  (let* ((workspace (macher-workspace))
         (filename (buffer-file-name))
         (relpath
          (when filename
            (file-relative-name filename (macher--workspace-root workspace))))
         (source-description
          (cond
           ;; No file associated with buffer.
           ((null filename)
            "")
           ;; Directory case.
           ((file-directory-p filename)
            (format "The request was sent from the workspace directory `%s`. " relpath))
           ;; Regular file case.
           (t
            (let* ((lang
                    (downcase
                     (if (stringp mode-name)
                         mode-name
                       (car mode-name)))))
              (format (concat
                       "The request was sent from the %s file `%s` in the workspace. "
                       "If the request text appears as a comment or placeholder in the file, "
                       "replace it with the actual implementation. ")
                      lang relpath))))))
    (format (concat
             "## TASK: Implement the following request using workspace tools.\n\n"
             "## INSTRUCTIONS:\n"
             "1. Read and understand the implementation request below\n"
             "2. Read and understand all provided references\n"
             "3. Use the reference to complete the request\n"
             "4. Use the workspace tools to edit files as needed\n"
             "5. Create working, complete code that fulfills the request\n\n"
             "%s"
             "\n\n##IMPLEMENTATION REQUEST:\n\n%s")
            source-description content)))

(defun mevedel--revise-directive-prompt (content &optional patch-buffer)
  "Generate a prompt for revising based on CONTENT (revision instructions).

The contents of the PATCH-BUFFER (defaulting to the current workspace's
patch buffer) are included in the generated prompt."
  (let* ((patch-buffer (or patch-buffer (macher-patch-buffer)))
         (patch-content
          (if patch-buffer
              (with-current-buffer patch-buffer
                (buffer-substring-no-properties (point-min) (point-max)))
            ;; Doesn't make sense to call this without a patch.
            (user-error "No patch buffer found for revision"))))
    (format (concat
             "## TASK: Revise your previous implementation based on new feedback.\n\n"
             "## WHAT YOU NEED TO DO:\n"
             "1. Read the revision instructions below (if any)\n"
             "2. Review your previous patch and its original prompt\n"
             "3. Read and understand all provided references\n"
             "4. Use the reference to complete the request\n"
             "5. Understand what needs to be changed or improved\n"
             "6. Create a NEW implementation that addresses the feedback\n"
             "7. Use the workspace editing tools to make the changes\n\n"
             "%s"
             "\n\n"
             "==================================\n"
             "YOUR PREVIOUS WORK (for reference)\n"
             "==================================\n\n"
             "%s")
            (if (and content (not (string-empty-p content)))
                (format "## REVISION INSTRUCTIONS:\n%s\n\n" content)
              "")
            patch-content)))

(defun mevedel--discuss-directive-prompt (content)
  "Generate a discussion prompt for CONTENT in the current buffer."
  (let* ((workspace (macher-workspace))
         (filename (buffer-file-name))
         (relpath
          (when filename
            (file-relative-name filename (macher--workspace-root workspace))))
         (source-description
          (cond
           ;; No file associated with buffer.
           ((null filename)
            "")
           ;; Directory case.
           ((file-directory-p filename)
            (format "The request was sent from the workspace directory `%s`. " relpath))
           ;; Regular file case.
           (t
            (let* ((lang
                    (downcase
                     (if (stringp mode-name)
                         mode-name
                       (car mode-name)))))
              (format "The request was sent from the %s file `%s` in the workspace."
                      lang relpath))))))
    (format (concat
             "## TASK: Answer the following request using workspace tools.\n\n"
             "## INSTRUCTIONS:\n"
             "1. Read and understand the implementation request below\n"
             "2. Read and understand all provided references\n"
             "3. Use the reference to complete the request\n"
             "4. Use the workspace tools to access files as needed\n"
             "%s"
             "\n\n## REQUEST:\n\n%s")
            source-description content)))


;;
;;; Commands

(declare-function macher-action "ext:macher" (action &optional user-input callback))
;;;###autoload
(defun macher-implement-directive (&optional callback)
  "Propose a patch to implement directive at point.

If CALLBACK is provided, it will be called when the implementation
process completes. The callback will receive three arguments: ERROR (nil
on success, a string error description on failure, or the symbol
\\='abort if the request was aborted), EXECUTION (the
macher-action-execution object for the action), and FSM (the gptel-fsm
object for the request)."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (overlay-put directive 'mevedel-directive-action 'implement)
        (mevedel--process-directive directive 'implementDirective callback))
    (user-error "No directive found at point")))

;;;###autoload
(defun macher-revise-directive (&optional callback)
  "Propose a revision to a patch based on the directive at point.

If CALLBACK is provided, it will be called when the implementation
process completes. The callback will receive three arguments: ERROR (nil
on success, a string error description on failure, or the symbol
\\='abort if the request was aborted), EXECUTION (the
macher-action-execution object for the action), and FSM (the gptel-fsm
object for the request)."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (overlay-put directive 'mevedel-directive-action 'revise)
        (mevedel--process-directive directive 'reviseDirective callback))
    (user-error "No directive found at point")))

;;;###autoload
(defun macher-discuss-directive (&optional callback)
  "Discuss the directive at point.

If CALLBACK is provided, it will be called when the implementation
process completes. The callback will receive three arguments: ERROR (nil
on success, a string error description on failure, or the symbol
\\='abort if the request was aborted), EXECUTION (the
macher-action-execution object for the action), and FSM (the gptel-fsm
object for the request)."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (progn
        (overlay-put directive 'mevedel-directive-action 'discuss)
        (mevedel--process-directive directive 'discussDirective callback))
    (user-error "No directive found at point")))

(defvar mevedel--current-directive-uuid nil
  "UUID of the directive currently being processed.")

(defun mevedel--process-directive (directive action callback)
  "Process DIRECTIVE with ACTION, calling CALLBACK when complete.
Updates directive status and overlay, handles success/failure states."
  (let ((callback-fn (lambda (err execution fsm)
                       (if err
                           (let ((reason err))
                             (overlay-put directive 'mevedel-directive-status 'failed)
                             (overlay-put directive 'mevedel-directive-fail-reason reason)
                             (mevedel--update-instruction-overlay directive t)
                             (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))
                             (setq mevedel--current-directive-uuid nil)
                             (user-error "Error: %s" err))
                         (overlay-put directive 'mevedel-directive-status 'succeeded)
                         (with-current-buffer (overlay-buffer directive)
                           ;; Delete any child directives of the top-level
                           ;; directive.
                           (let ((child-directives (cl-remove-if-not #'mevedel--directivep
                                                                     (mevedel--child-instructions directive))))
                             (dolist (child-directive child-directives)
                               (mevedel--delete-instruction child-directive)))
                           (save-excursion
                             (goto-char (overlay-start directive))
                             (overlay-put directive 'evaporate t)))
                         (mevedel--update-instruction-overlay directive t)
                         (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))
                         (prog1
                             (when callback
                               (funcall callback err execution fsm))
                           (setq mevedel--current-directive-uuid nil))))))
    (setq mevedel--current-directive-uuid (overlay-get directive 'mevedel-uuid))
    (macher-action action callback-fn directive)
    ;; Add current directive to history.
    (with-current-buffer (overlay-buffer directive)
      (let ((entry (mevedel--create-history-entry directive))
            (current-history-pos (overlay-get directive 'mevedel-directive-history-position)))
        (if (or (not current-history-pos) (= current-history-pos 0))
            ;; Add entry to the front
            (push entry (overlay-get directive 'mevedel-directive-history))
          ;; Cut off anything before current position and add current entry
          (let ((history (overlay-get directive 'mevedel-directive-history)))
            (setf (overlay-get directive 'mevedel-directive-history)
                  (cons entry (nthcdr (1+ current-history-pos) history)))))
        ;; Reset history position when new entry is added
        (overlay-put directive 'mevedel-directive-history-position 0)))
    (overlay-put directive 'mevedel-directive-status 'processing)
    (mevedel--update-instruction-overlay directive t)
    (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))))

;;;###autoload
(defun mevedel-instruction-count ()
  "Return the number of instructions currently loaded instructions.

If called interactively, it messages the number of instructions and
buffers."
  (interactive)
  (let ((count 0)
        (buffer-hash (make-hash-table :test 'eq)))
    (mevedel--foreach-instruction instr count instr into instr-count
                                  do (puthash (overlay-buffer instr) t buffer-hash)
                                  finally (setf count instr-count))
    (let ((buffers (hash-table-count buffer-hash)))
      (when (called-interactively-p 'interactive)
        (if (= count 0)
            (message "No macher instructions currently loaded")
          (message "macher is showing %d instruction%s from %d buffer%s"
                   count (if (/= count 1) "s" "")
                   buffers (if (/= buffers 1) "s" ""))))
      count)))

;;;###autoload
(defun mevedel-create-reference ()
  "Create a reference instruction within the selected region.

If a region is selected but partially covers an existing reference, then
the command will resize the reference in the following manner:

  - If the mark is located INSIDE the reference (i.e., the point is
    located OUTSIDE the reference) then the reference will be expanded
    to the point.
  - If the mark is located OUTSIDE the reference (i.e., the point is
    located INSIDE the reference) then the reference will be shrunk to
    the point."
  (interactive)
  (mevedel--create-instruction 'reference))

;;;###autoload
(defun mevedel-create-directive ()
  "Create a directive instruction within the selected region.

If a region is selected but partially covers an existing directive, then
the command will resize the directive in the following manner:

  - If the mark is located INSIDE the directive (i.e., the point is
    located OUTSIDE the directive) then the directive will be expanded
    to the point.
  - If the mark is located OUTSIDE the directive (i.e., the point is
    located INSIDE the directive) then the directive will be shrunk to
    the point."
  (interactive)
  (mevedel--create-instruction 'directive))


;;
;;; Installation

(defun mevedel--suppress-patch-buffer (fn &rest args)
  "Suppress display of patch buffer while executing FN with ARGS."
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (apply fn args)))

(defun mevedel--suppress-action-buffer (fn &rest args)
  "Suppress display of action buffer while executing FN with ARGS."
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (apply fn args)))

(defvar macher-actions-alist)
;;;###autoload
(defun mevedel-install ()
  "Register `mevedel' actions and advices."
  (interactive)
  ;; Add custom actions to `macher-actions-alist'
  (setf (alist-get 'implementDirective macher-actions-alist)
        (apply-partially #'mevedel--action-from-directive #'mevedel--implement-directive-prompt 'macher))
  (setf (alist-get 'reviseDirective macher-actions-alist)
        (apply-partially #'mevedel--action-from-directive #'mevedel--revise-directive-prompt 'macher))
  (setf (alist-get 'discussDirective macher-actions-alist)
        (apply-partially #'mevedel--action-from-directive #'mevedel--discuss-directive-prompt 'macher-ro))

  ;; Apply advices & hooks if required
  (if mevedel-show-patch-buffer
      (advice-remove 'macher--patch-ready #'mevedel--suppress-patch-buffer)
    (advice-add 'macher--patch-ready :around #'mevedel--suppress-patch-buffer))
  (if mevedel-show-action-buffer
      (advice-remove 'macher--before-action #'mevedel--suppress-action-buffer)
    (advice-add 'macher--before-action :around #'mevedel--suppress-action-buffer))
  (if mevedel-auto-apply-patches
      (add-hook 'macher-patch-ready-hook #'mevedel--diff-apply-buffer-with-ov-adjustment)
    (remove-hook 'macher-patch-ready-hook #'mevedel--diff-apply-buffer-with-ov-adjustment)))

;;;###autoload
(defun mevedel-uninstall ()
  "Remove `mevedel' actions and advices."
  (interactive)
  ;; Remove custom actions from `macher-actions-alist'
  (setf (alist-get 'implementDirective macher-actions-alist nil 'remove) nil)
  (setf (alist-get 'reviseDirective macher-actions-alist nil 'remove) nil)
  (setf (alist-get 'discussDirective macher-actions-alist nil 'remove) nil)

  ;; Remove advices & hooks
  (advice-remove 'macher--patch-ready #'mevedel--suppress-patch-buffer)
  (advice-remove 'macher--before-action #'mevedel--suppress-action-buffer)
  (remove-hook 'macher-patch-ready-hook #'mevedel--diff-apply-buffer-with-ov-adjustment))

(provide 'mevedel)

;;; mevedel.el ends here.
