;;; macher-instruct.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

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

(defgroup macher-instruct nil
  "Customization group for Evedel."
  :group 'tools)

;;;###autoload
(defun macher-instruct-version (&optional here message)
  "Return the current version of macher.

Interactively, or when MESSAGE is non-nil, show it in echo area. With
prefix argument, or when HERE is non-nil, insert it at point."
  (interactive (list (or current-prefix-arg 'interactive)))
  (let ((version "v0.3.0"))
    (cond
     ((or message (called-interactively-p 'any)) (message "macher %s" version))
     (here (insert (format "macher %s" version)))
     (t version))))

(defvar macher-actions-alist)
(with-eval-after-load 'macher
  (add-to-list 'macher-actions-alist
               '(implementDirective :preset macher :transform macher-instruct--implement-directive-prompt))
  (add-to-list 'macher-actions-alist
               '(reviseDirective :preset macher :transform macher-instruct--revise-directive-prompt))
  (add-to-list 'macher-actions-alist
               '(discussDirective :preset macher-ro :transform macher-instruct--discuss-directive-prompt))

  ;; TODO 2025-08-06: Move me
  ;; Don't open the patch buffer automatically
  (defun macher-instruct--patch-suppress-buffer (fn &rest args)
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (apply fn args)))
  (advice-add 'macher--patch-ready :around #'macher-instruct--patch-suppress-buffer))

(defun macher-instruct--implement-directive-prompt (content)
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

(defun macher-instruct--revise-directive-prompt (content &optional patch-buffer)
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

(defun macher-instruct--discuss-directive-prompt (content)
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

(declare-function macher-action "ext:macher" (action &optional user-input callback))
;;;###autoload
(defun macher-implement-directive (&optional callback)
  (interactive)
  (if-let* ((directive (macher-instruct--topmost-instruction (macher-instruct--highest-priority-instruction
                                                              (macher-instruct--instructions-at (point) 'directive)
                                                              t)
                                                             'directive)))
      (macher-instruct--process-directive directive 'implementDirective callback)
    (user-error "No directive found at point")))

;;;###autoload
(defun macher-revise-directive (&optional callback)
  (interactive)
  (if-let* ((directive (macher-instruct--topmost-instruction (macher-instruct--highest-priority-instruction
                                                              (macher-instruct--instructions-at (point) 'directive)
                                                              t)
                                                             'directive)))
      (macher-instruct--process-directive directive 'reviseDirective callback)
    (user-error "No directive found at point")))

;;;###autoload
(defun macher-discuss-directive (&optional callback)
  (interactive)
  (if-let* ((directive (macher-instruct--topmost-instruction (macher-instruct--highest-priority-instruction
                                                              (macher-instruct--instructions-at (point) 'directive)
                                                              t)
                                                             'directive)))
      (macher-instruct--process-directive directive 'discussDirective callback)
    (user-error "No directive found at point")))

(defun macher-instruct--process-directive (directive action callback)
  (let ((callback-fn (lambda (err execution fsm)
                       (if err
                           (let ((reason err))
                             (overlay-put directive 'macher-instruct-directive-status 'failed)
                             (overlay-put directive 'macher-instruct-directive-fail-reason reason)
                             (macher-instruct--update-instruction-overlay directive t)
                             (user-error "Error: %s" err))
                         (overlay-put directive 'macher-instruct-directive-status 'succeeded)
                         (with-current-buffer (overlay-buffer directive)
                           (let ((beg (overlay-start directive))
                                 (end (overlay-end directive)))
                             ;; Add current directive text to history.
                             (let ((current-text (buffer-substring-no-properties beg end)))
                               (let ((trimmed-text (if (string= " " current-text) " " (string-trim current-text))))
                                 (unless (string-empty-p trimmed-text)
                                   (push current-text (overlay-get directive 'macher-instruct-directive-history)))))
                             ;; Delete any child directives of the top-level
                             ;; directive.
                             (let ((child-directives (cl-remove-if-not #'macher-instruct--directivep
                                                                       (macher-instruct--child-instructions directive))))
                               (dolist (child-directive child-directives)
                                 (macher-instruct--delete-instruction child-directive)))
                             (save-excursion
                               (goto-char beg)
                               (overlay-put directive 'evaporate t))))
                         (macher-instruct--update-instruction-overlay directive t)
                         (when callback
                           (funcall callback err execution fsm))))))
    (overlay-put directive 'macher-instruct-directive-status 'processing)
    (macher-instruct--update-instruction-overlay directive t)
    (macher-action action (macher-instruct--directive-llm-prompt directive) callback-fn)))

(require 'macher-instruct-instructions)
(require 'macher-instruct-restorer)

(provide 'macher-instruct)

;;; macher-instruct.el ends here.
